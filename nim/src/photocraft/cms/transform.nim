## Colour transforms between profiles through the PCS, with rendering intents, black point
## compensation, soft-proofing chains, and fast lookup-table evaluation of pixel buffers.

import std/[options, tables, math, hashes]
import ./curve, ./clut, ./math as cmath, ./pipeline, ./profile, ./write, ./builtin

type
  SampleKind* = enum skU8, skU16, skF32

  TransformOptions* = object
    intent*: Intent
    bpc*: bool          ## Adobe-style black point compensation (ignored for absolute).
    preciseFloat*: bool ## float buffers go through the exact pipeline

  CoreKind = enum coreShaper, coreGrid, coreExact
  Core = object
    case kind: CoreKind
    of coreShaper:
      curves8: seq[array[256, float32]]
      curves: seq[seq[float32]]
      hasMatrix: bool
      mRows, mCols: int
      mat, moff: seq[float32]
    of coreGrid: grid: Clut
    of coreExact: discard

  Transform* = ref object
    ## A colour transform from one profile's device space to another's.
    inputs*, outputs*: int
    opts*: TransformOptions
    pipeline*: Pipeline
    core: Core
    post: Option[seq[seq[float32]]]
    grid8: seq[array[256, (int, float32)]]

const
  Grid3D* = 33
  Grid4D* = 17
  Table1D = 4096
  OutTable = 4096
  PerceptualBlack: Vec3 = [0.00336, 0.0034731, 0.00287]

proc defaultTransformOptions*(): TransformOptions =
  TransformOptions(intent: RelativeColorimetric, bpc: true, preciseFloat: true)

proc hash*(o: TransformOptions): Hash = hash((ord(o.intent), o.bpc, o.preciseFloat))

proc postEval(t: seq[float32]; x: float32): float32 {.inline.} =
  let u = sqrt(sqrt(clamp(x, 0.0'f32, 1.0'f32))) * float32(OutTable - 1)
  let i = min(int(u), OutTable - 2)
  let f = u - float32(i)
  t[i] + (t[i + 1] - t[i]) * f

proc shaperCore(cs: seq[Curve]; matrix: Option[(int, int, seq[float64], seq[float64])]): Core =
  result = Core(kind: coreShaper)
  for c in cs:
    var a: array[256, float32]
    for i in 0 ..< 256: a[i] = float32(c.eval64(float64(i) / 255.0))
    result.curves8.add a
    var t = newSeq[float32](Table1D + 1)
    for i in 0..Table1D: t[i] = float32(c.eval64(float64(i) / float64(Table1D)))
    result.curves.add t
  if matrix.isSome:
    let (r, c, m, o) = matrix.get
    result.hasMatrix = true
    result.mRows = r
    result.mCols = c
    for v in m: result.mat.add float32(v)
    for v in o: result.moff.add float32(v)

proc table1(t: seq[float32]; x: float32): float32 {.inline.} =
  let n = t.len - 1
  let p = x * float32(n)
  let i = min(int(max(floor(p), 0.0'f32)), n - 1)
  let f = p - float32(i)
  t[i] + (t[i + 1] - t[i]) * f

proc applyMatrix(c: Core; v: PipeVec; outp: var PipeVec; n: int) {.inline.} =
  if not c.hasMatrix:
    for i in 0 ..< n: outp[i] = v[i]
  else:
    for r in 0 ..< c.mRows:
      var s = c.moff[r]
      for k in 0 ..< c.mCols: s += c.mat[r * c.mCols + k] * v[k]
      outp[r] = s

proc fromPipeline*(pipeline0: Pipeline; opts: TransformOptions): Transform =
  ## Builds a transform from an arbitrary pipeline over normalised values.
  var pipeline = pipeline0
  pipeline.optimize()
  let inputs = pipeline.inputs
  let outputs = pipeline.outputs
  var head = pipeline.stages
  var post = none(seq[seq[float32]])
  if head.len > 0 and head[^1].kind == skInvCurves and head[^1].curves.len == outputs:
    var tables: seq[seq[float32]]
    for c in head[^1].curves:
      var t = newSeq[float32](OutTable)
      for i in 0 ..< OutTable:
        let u = float64(i) / float64(OutTable - 1)
        t[i] = float32(clamp(c.evalInverse64(u * u * u * u), 0.0, 1.0))
      tables.add t
    post = some(tables)
    discard head.pop()
    if head.len > 0 and head[^1].kind == skClamp01: discard head.pop()
  let headPipe = newPipeline(inputs, head)
  let mid = headPipe.outputs
  var core: Core
  var hasClut = false
  for s in head:
    if s.kind == skClut: hasClut = true
  if head.len == 0: core = Core(kind: coreShaper)
  elif head.len == 1 and head[0].kind == skCurves and head[0].curves.len == inputs:
    core = shaperCore(head[0].curves, none((int, int, seq[float64], seq[float64])))
  elif head.len == 1 and head[0].kind == skMatrix:
    core = shaperCore(@[], some((head[0].rows, head[0].cols, head[0].m, head[0].offset)))
  elif head.len == 2 and head[0].kind == skCurves and head[1].kind == skMatrix and head[0].curves.len == inputs:
    core = shaperCore(head[0].curves, some((head[1].rows, head[1].cols, head[1].m, head[1].offset)))
  elif not hasClut: core = Core(kind: coreExact)
  else:
    let f = proc(i: openArray[float32]; o: var openArray[float32]) = headPipe.eval(i, o)
    case inputs
    of 1: core = Core(kind: coreGrid, grid: sampleClut(@[Table1D], mid, f))
    of 3: core = Core(kind: coreGrid, grid: sampleClut(@[Grid3D, Grid3D, Grid3D], mid, f))
    of 4: core = Core(kind: coreGrid, grid: sampleClut(@[Grid4D, Grid4D, Grid4D, Grid4D], mid, f))
    of 2: core = Core(kind: coreGrid, grid: sampleClut(@[Grid3D, Grid3D], mid, f))
    else: core = Core(kind: coreExact)
  var grid8: seq[array[256, (int, float32)]]
  if core.kind == coreGrid and core.grid.inputs >= 2:
    let c = core.grid
    for d in 0 ..< c.inputs:
      let n = c.grid[d]
      var prod = 1
      for k in 0..d: prod *= c.grid[k]
      let stride = c.data.len div c.outputs div prod * c.outputs
      var a: array[256, (int, float32)]
      for v in 0 ..< 256:
        let p = float32(v) / 255.0'f32 * float32(n - 1)
        let i = min(int(p), n - 2)
        a[v] = (i * stride, p - float32(i))
      grid8.add a
  Transform(inputs: inputs, outputs: outputs, opts: opts, pipeline: pipeline, core: core, post: post, grid8: grid8)

proc deviceWhite(cs: ColorSpace): Option[seq[float32]] =
  case cs.kind
  of csRgb: some(@[1.0'f32, 1, 1])
  of csGray: some(@[1.0'f32])
  of csCmyk: some(@[0.0'f32, 0, 0, 0])
  of csCmy: some(@[0.0'f32, 0, 0])
  of csLab: some(@[1.0'f32, 0.5, 0.5])
  else: none(seq[float32])

proc fixWhite(t: Transform; src, dst: ColorSpace) =
  ## The device-link node at the source's device white gets exactly the destination's white.
  let sw = deviceWhite(src)
  var dw = deviceWhite(dst)
  if sw.isNone or dw.isNone: return
  if t.post.isSome:
    if dst.kind notin {csRgb, csGray}: return
    for v in dw.get.mitems: v = 1.0
  if t.core.kind != coreGrid: return
  let c = addr t.core.grid
  if c.inputs != sw.get.len or c.outputs != dw.get.len: return
  var off = 0
  var stride = c.outputs
  for d in countdown(c.inputs - 1, 0):
    let n = c.grid[d]
    let pos = sw.get[d] * float32(n - 1)
    if abs(pos - round(pos)) > 1e-6: return
    off += int(round(pos)) * stride
    stride *= n
  for k in 0 ..< c.outputs: c.data[off + k] = dw.get[k]

# ------------------------------------------------------------ linking

proc blackPoint*(p: Profile; intent: Intent; input: bool): float64

proc connection(src, dst: Profile; intent: Intent; bpc: bool): Option[(Vec3, Vec3)] =
  if intent == AbsoluteColorimetric:
    let ws = src.mediaWhite
    let wd = dst.mediaWhite
    proc usable(w: Vec3): bool =
      for v in w:
        if not (isFiniteF(v) and v > 0.0): return false
      true
    if not usable(ws) or not usable(wd):
      raise newCmsError(ceInvalid, "absolute colorimetric needs a positive media white point")
    let s = [ws[0] / wd[0], ws[1] / wd[1], ws[2] / wd[2]]
    if abs(s[0] - 1.0) < 1e-6 and abs(s[1] - 1.0) < 1e-6 and abs(s[2] - 1.0) < 1e-6: return none((Vec3, Vec3))
    return some((s, [0.0, 0.0, 0.0]))
  if not bpc: return none((Vec3, Vec3))
  let bs = blackPoint(src, intent, true)
  let bd = blackPoint(dst, intent, false)
  if abs(bs - bd) < 1e-6: return none((Vec3, Vec3))
  let w = D50
  var scale, offset: Vec3
  for i in 0..2:
    let s = bs * w[i]
    let d = bd * w[i]
    let a = (w[i] - d) / (w[i] - s)
    scale[i] = a
    offset[i] = w[i] * (1.0 - a)
  some((scale, offset))

proc linkStages*(src, dst: Profile; intent: Intent; bpc: bool): seq[Stage] =
  ## Stages from `src` device values to `dst` device values. Raises `CmsError`.
  for p in [src, dst]:
    var labToLab = p.colorSpace.kind == csLab
    for l in p.a2b:
      if l.isSome and l.get.outputs != 3: labToLab = false
    for l in p.b2a:
      if l.isSome and l.get.inputs != 3: labToLab = false
    if p.class.kind in {pcDeviceLink, pcNamedColor, pcAbstract} and not labToLab:
      raise newCmsError(ceUnsupported, $p.class & " profiles cannot be used as a source or destination")
  var (stages, sPcs) = src.deviceToPcs(intent)
  let (dStages, dPcs) = dst.pcsToDevice(intent)
  let adjust = connection(src, dst, intent, bpc)
  if adjust.isSome:
    let (scale, offset) = adjust.get
    if sPcs == pcsLab: stages.add simpleStage(skLabToXyz)
    stages.add matrixStage(3, 3, @[scale[0], 0.0, 0.0, 0.0, scale[1], 0.0, 0.0, 0.0, scale[2]], @[offset[0], offset[1], offset[2]])
    if dPcs == pcsLab: stages.add simpleStage(skXyzToLab)
  else:
    if sPcs == pcsLab and dPcs == pcsXyz: stages.add simpleStage(skLabToXyz)
    elif sPcs == pcsXyz and dPcs == pcsLab: stages.add simpleStage(skXyzToLab)
  stages.add dStages
  stages

proc blackPoint*(p: Profile; intent: Intent; input: bool): float64 =
  ## Luminance (Y, D50-relative) of the profile's black point for `intent` (Adobe BPC structure).
  if p.colorSpace.kind in {csLab, csXyz}: return 0.0
  var lutBased = false
  for l in p.a2b:
    if l.isSome: lutBased = true
  if lutBased and p.version[0] >= 4 and intent in {Perceptual, Saturation}: return PerceptualBlack[1]
  proc pcsY(st: seq[Stage]; pcs: Pcs; dev: openArray[float32]): float64 =
    let pipe = newPipeline(p.channels, st)
    var o: PipeVec
    pipe.eval(dev, o)
    let y = if pcs == pcsXyz: float64(o[1]) else: labToXyz([float64(o[0]), 0.0, 0.0], D50)[1]
    clamp(y, 0.0, 0.5)
  let (toPcs, pcs) = p.deviceToPcs(intent)
  let additive = p.colorSpace.kind in {csRgb, csGray}
  if additive or not lutBased:
    let zero = newSeq[float32](p.channels)
    return pcsY(toPcs, pcs, zero)
  let colorimetric = if intent == Saturation: Saturation else: RelativeColorimetric
  try:
    let (fromPcs, fpcs) = p.pcsToDevice(colorimetric)
    var st: seq[Stage]
    if fpcs == pcsXyz: st.add simpleStage(skLabToXyz)
    st.add fromPcs
    let pipe = newPipeline(3, st)
    var dev: PipeVec
    pipe.eval([0.0'f32, 0.0, 0.0], dev)
    pcsY(toPcs, pcs, dev.toOpenArray(0, p.channels - 1))
  except CmsError:
    var ones = newSeq[float32](p.channels)
    for v in ones.mitems: v = 1.0
    pcsY(toPcs, pcs, ones)

proc newTransform*(src, dst: Profile; opts: TransformOptions): Transform =
  ## Transform from `src` to `dst`. Raises `CmsError`.
  let stages = linkStages(src, dst, opts.intent, opts.bpc)
  result = fromPipeline(newPipeline(src.channels, stages), opts)
  if opts.intent != AbsoluteColorimetric: result.fixWhite(src.colorSpace, dst.colorSpace)

proc newTransform*(src, dst: Profile; intent: Intent; bpc: bool): Transform =
  newTransform(src, dst, TransformOptions(intent: intent, bpc: bpc, preciseFloat: true))

proc proofTransform*(src, proof, dst: Profile; intent: Intent; bpc, simulatePaper: bool): Transform =
  ## Soft-proof transform `src → proof → dst` (View › Proof Setup).
  var stages = linkStages(src, proof, intent, bpc)
  stages.add simpleStage(skClamp01)
  let (i2, b2) = if simulatePaper: (AbsoluteColorimetric, false) else: (RelativeColorimetric, true)
  stages.add linkStages(proof, dst, i2, b2)
  fromPipeline(newPipeline(src.channels, stages), TransformOptions(intent: intent, bpc: bpc, preciseFloat: true))

proc eval*(t: Transform; input: openArray[float32]; outp: var openArray[float32]) {.inline.} =
  ## Exact evaluation of one colour.
  t.pipeline.eval(input, outp)

proc finish(t: Transform; mid: PipeVec; outp: var openArray[float32]) {.inline.} =
  if t.post.isSome:
    for k in 0 ..< t.outputs: outp[k] = postEval(t.post.get[k], mid[k])
  else:
    for k in 0 ..< t.outputs: outp[k] = mid[k]

proc evalFast*(t: Transform; input: openArray[float32]; outp: var openArray[float32]) =
  ## Lookup-table evaluation of one colour (what integer buffers use).
  var mid: PipeVec
  case t.core.kind
  of coreExact:
    t.pipeline.eval(input, outp)
    return
  of coreGrid:
    var inp: PipeVec
    for d in 0 ..< t.inputs: inp[d] = clamp(input[d], 0.0'f32, 1.0'f32)
    t.core.grid.eval(inp.toOpenArray(0, t.inputs - 1), mid)
  of coreShaper:
    var v: PipeVec
    for k in 0 ..< t.inputs:
      v[k] = if t.core.curves.len == 0: input[k] else: table1(t.core.curves[k], input[k])
    t.core.applyMatrix(v, mid, t.inputs)
  t.finish(mid, outp)

proc checkStride(t: Transform; ss, ds: int) =
  if ss < t.inputs or ds < t.outputs:
    raise newCmsError(ceInvalid, "pixel stride smaller than the channel count")

proc extraChannels(t: Transform; ss, ds: int; copy: bool): int =
  if copy: min(ss - t.inputs, ds - t.outputs) else: 0

proc convertF32*(t: Transform; src: openArray[float32]; srcStride: int; dst: var openArray[float32]; dstStride: int; copyExtra: bool) =
  ## Converts float pixels; colour channels come first, trailing channels copied when asked.
  t.checkStride(srcStride, dstStride)
  let extra = t.extraChannels(srcStride, dstStride, copyExtra)
  let n = min(src.len div srcStride, dst.len div dstStride)
  let precise = t.opts.preciseFloat or t.core.kind == coreExact
  var o: PipeVec
  for px in 0 ..< n:
    let s = px * srcStride
    let d = px * dstStride
    if precise: t.pipeline.eval(src.toOpenArray(s, s + srcStride - 1), o)
    else: t.evalFast(src.toOpenArray(s, s + srcStride - 1), o)
    for k in 0 ..< t.outputs: dst[d + k] = o[k]
    for e in 0 ..< extra: dst[d + t.outputs + e] = src[s + t.inputs + e]

proc apply*(t: Transform; buf: var openArray[float32]; stride: int) =
  ## In-place float conversion; `stride ≥ max(inputs, outputs)`.
  if stride < max(t.inputs, t.outputs): raise newCmsError(ceInvalid, "stride too small")
  let precise = t.opts.preciseFloat or t.core.kind == coreExact
  var o: PipeVec
  for px in 0 ..< buf.len div stride:
    let s = px * stride
    if precise: t.pipeline.eval(buf.toOpenArray(s, s + stride - 1), o)
    else: t.evalFast(buf.toOpenArray(s, s + stride - 1), o)
    for k in 0 ..< t.outputs: buf[s + k] = o[k]

proc q8(v: float32): uint8 {.inline.} = uint8(clamp(v, 0.0'f32, 1.0'f32) * 255.0'f32 + 0.5'f32)

proc convertU8*(t: Transform; src: openArray[uint8]; ss: int; dst: var openArray[uint8]; ds: int; copyExtra: bool) =
  ## Converts 8-bit pixels (see `convertF32` for the layout).
  t.checkStride(ss, ds)
  let extra = t.extraChannels(ss, ds, copyExtra)
  let n = min(src.len div ss, dst.len div ds)
  var mid, outv: PipeVec
  if t.core.kind == coreGrid and t.core.grid.inputs == 3:
    let c = t.core.grid
    let s1 = c.outputs * c.grid[2]
    let s0 = s1 * c.grid[1]
    for px in 0 ..< n:
      let sp = px * ss
      let dp = px * ds
      let a = t.grid8[0][src[sp]]
      let b = t.grid8[1][src[sp + 1]]
      let cc = t.grid8[2][src[sp + 2]]
      c.tetra(a[0] + b[0] + cc[0], [s0, s1, c.outputs], [a[1], b[1], cc[1]], mid)
      t.finish(mid, outv)
      for k in 0 ..< t.outputs: dst[dp + k] = q8(outv[k])
      for e in 0 ..< extra: dst[dp + t.outputs + e] = src[sp + t.inputs + e]
  elif t.core.kind == coreGrid and t.core.grid.inputs == 4:
    let c = t.core.grid
    let s3 = c.outputs
    let s2 = s3 * c.grid[3]
    let s1 = s2 * c.grid[2]
    let s0 = s1 * c.grid[1]
    var hi: PipeVec
    for px in 0 ..< n:
      let sp = px * ss
      let dp = px * ds
      let a = t.grid8[0][src[sp]]
      let b = t.grid8[1][src[sp + 1]]
      let cc = t.grid8[2][src[sp + 2]]
      let k4 = t.grid8[3][src[sp + 3]]
      let base = a[0] + b[0] + cc[0] + k4[0]
      c.tetra(base, [s1, s2, s3], [b[1], cc[1], k4[1]], mid)
      if a[1] > 0.0:
        c.tetra(base + s0, [s1, s2, s3], [b[1], cc[1], k4[1]], hi)
        for k in 0 ..< c.outputs: mid[k] += (hi[k] - mid[k]) * a[1]
      t.finish(mid, outv)
      for k in 0 ..< t.outputs: dst[dp + k] = q8(outv[k])
      for e in 0 ..< extra: dst[dp + t.outputs + e] = src[sp + t.inputs + e]
  elif t.core.kind == coreShaper:
    var v: PipeVec
    for px in 0 ..< n:
      let sp = px * ss
      let dp = px * ds
      for k in 0 ..< t.inputs:
        v[k] = if t.core.curves8.len == 0: float32(src[sp + k]) / 255.0'f32 else: t.core.curves8[k][src[sp + k]]
      t.core.applyMatrix(v, mid, t.inputs)
      t.finish(mid, outv)
      for k in 0 ..< t.outputs: dst[dp + k] = q8(outv[k])
      for e in 0 ..< extra: dst[dp + t.outputs + e] = src[sp + t.inputs + e]
  else:
    var inp: PipeVec
    for px in 0 ..< n:
      let sp = px * ss
      let dp = px * ds
      for k in 0 ..< t.inputs: inp[k] = float32(src[sp + k]) / 255.0'f32
      t.evalFast(inp, outv)
      for k in 0 ..< t.outputs: dst[dp + k] = q8(outv[k])
      for e in 0 ..< extra: dst[dp + t.outputs + e] = src[sp + t.inputs + e]

proc convertU16*(t: Transform; src: openArray[uint16]; ss: int; dst: var openArray[uint16]; ds: int; copyExtra: bool) =
  ## Converts 16-bit pixels.
  t.checkStride(ss, ds)
  let extra = t.extraChannels(ss, ds, copyExtra)
  let n = min(src.len div ss, dst.len div ds)
  var inp, outv: PipeVec
  for px in 0 ..< n:
    let sp = px * ss
    let dp = px * ds
    for k in 0 ..< t.inputs: inp[k] = float32(src[sp + k]) / 65535.0'f32
    t.evalFast(inp, outv)
    for k in 0 ..< t.outputs: dst[dp + k] = uint16(clamp(outv[k], 0.0'f32, 1.0'f32) * 65535.0'f32 + 0.5'f32)
    for e in 0 ..< extra: dst[dp + t.outputs + e] = src[sp + t.inputs + e]

# ------------------------------------------------------------ cache

var transformCache: Table[(uint64, uint64, TransformOptions), Transform]

proc cached*(src, dst: Profile; opts: TransformOptions): Transform =
  ## Process-wide cache of transforms keyed by profile content and options. Raises `CmsError`.
  {.cast(gcsafe).}:
    let key = (src.contentHash, dst.contentHash, opts)
    if transformCache.hasKey(key): return transformCache[key]
    let t = newTransform(src, dst, opts)
    if transformCache.len > 64: transformCache.clear()
    transformCache[key] = t
    t

proc sameColors*(a, b: Profile): bool =
  ## Whether `a` and `b` describe the same colours (every sample of a device grid within
  ## `SameColorsMaxDeltaE` through both, relative colorimetric).
  if a.contentHash == b.contentHash: return true
  let n = a.channels
  if a.colorSpace != b.colorSpace or n == 0 or n > 4: return false
  let lab = bLabD50.profile
  var ta, tb: Transform
  try:
    ta = newTransform(a, lab, RelativeColorimetric, false)
    tb = newTransform(b, lab, RelativeColorimetric, false)
  except CmsError:
    return false
  let steps = if n <= 3: 17 else: 9
  proc decode(o: PipeVec): Vec3 = [float64(o[0]) * 100.0, float64(o[1]) * 255.0 - 128.0, float64(o[2]) * 255.0 - 128.0]
  var input: array[4, float32]
  var total = 1
  for _ in 0 ..< n: total *= steps
  for i in 0 ..< total:
    var k = i
    for c in 0 ..< n:
      input[c] = float32(k mod steps) / float32(steps - 1)
      k = k div steps
    var la, lb: PipeVec
    ta.eval(input.toOpenArray(0, n - 1), la)
    tb.eval(input.toOpenArray(0, n - 1), lb)
    if deltaE76(decode(la), decode(lb)) > SameColorsMaxDeltaE: return false
  true
