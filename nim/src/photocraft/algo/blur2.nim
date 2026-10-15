## Smart Blur, Shape Blur and Lens Blur (port of `blur2.rs` and `shape_blur.rs`).
##
## Shaped kernels are applied as horizontal spans over per-row prefix sums (f64, to keep 16-bit
## precision), so a kernel of radius r costs O(r) per pixel instead of O(r²).

import std/[math, options]
import ../geom
import params, image, fxutil, noise

let   # run-time: Nim folds float32 constant arithmetic in float64
  Pi32 = float32(PI)
  Tau32 = float32(TAU)

# ---------------------------------------------------------------- span kernels

type
  InsideProc* = proc(u, v: float32): bool {.closure.}
  Spans* = object
    ## A binary kernel as horizontal runs `(dy, dx0, dx1)` (inclusive).
    rows*: seq[(int, int, int)]
    count*: float64
  Prefix* = object
    ## Per-row prefix sums of an interleaved window: `(w + 1) × h × n`.
    w, n: int
    data: seq[float64]

proc newSpans*(r: float32; inside: InsideProc): Spans =
  ## Rasterizes `inside(u, v)` (u, v in −1…1) over a disc of radius `r`.
  let ri = asI32(float64(ceil(max(r, 0))))
  if r < 0.5: return Spans(rows: @[(0, 0, 0)], count: 1)
  for dy in -ri .. ri:
    var run = none(int)
    for dx in -ri .. ri + 1:
      let hit = dx <= ri and inside(float32(dx) / r, float32(dy) / r)
      if hit and run.isNone: run = some(dx)
      elif not hit and run.isSome:
        result.rows.add (dy, run.get, dx - 1)
        result.count += float64(dx - run.get)
        run = none(int)
  if result.rows.len == 0:
    result.rows.add (0, 0, 0)
    result.count = 1

proc newPrefix*(p: openArray[float32]; w, h, n: int): Prefix =
  result = Prefix(w: w, n: n, data: newSeq[float64]((w + 1) * h * n))
  for y in 0 ..< h:
    let s = y * w * n
    let d = y * (w + 1) * n
    for x in 0 ..< w:
      for c in 0 ..< n: result.data[d + (x + 1) * n + c] = result.data[d + x * n + c] + float64(p[s + x * n + c])

proc mean*(pre: Prefix; spans: Spans; x, y: int; acc: var openArray[float64]) =
  ## Kernel mean at window pixel `(x, y)` into `acc` (n values).
  let n = pre.n
  for c in 0 ..< n: acc[c] = 0
  let h = pre.data.len div ((pre.w + 1) * n)
  for (dy, a, b) in spans.rows:
    let yy = y + dy
    if yy < 0 or yy >= h: continue
    let x0 = clamp(x + a, 0, pre.w)
    let x1 = clamp(x + b + 1, 0, pre.w)
    let row = yy * (pre.w + 1) * n
    for c in 0 ..< n: acc[c] += pre.data[row + x1 * n + c] - pre.data[row + x0 * n + c]
  for c in 0 ..< n: acc[c] /= spans.count

proc inPoly(u, v: float32; pts: openArray[(float32, float32)]): bool =
  ## Point-in-polygon (even–odd).
  var j = pts.len - 1
  for i in 0 ..< pts.len:
    let (xi, yi) = pts[i]
    let (xj, yj) = pts[j]
    if (yi > v) != (yj > v) and u < (xj - xi) * (v - yi) / (yj - yi) + xi: result = not result
    j = i

proc remEuclidF(a, b: float32): float32 =
  let r = a mod b
  if r < 0: r + abs(b) else: r

proc polygonRadius(theta: float32; n: uint32; rot: float32): float32 =
  ## Radius of a regular `n`-gon (circumradius 1) in direction `theta`.
  let seg = Tau32 / float32(n)
  let a = remEuclidF(theta - rot, seg) - seg / 2
  cos(Pi32 / float32(n)) / cos(a)

proc insideRing(u, v: float32): bool =
  let d = u * u + v * v
  d >= 0.36'f32 and d <= 1

proc insideHeart(u, v: float32): bool =
  # (x² + y² − 1)³ − x²y³ ≤ 0, scaled into the unit square and flipped (y up).
  let (x, y) = (u * 1.2'f32, -v * 1.2'f32 + 0.15'f32)
  let q = x * x + y * y - 1
  q * q * q - x * x * y * y * y <= 0

proc starPoints(): seq[(float32, float32)] =
  for i in 0 ..< 10:
    let r = if i mod 2 == 0: 1'f32 else: 0.42'f32
    let a = -Pi32 / 2 + float32(i) * Pi32 / 5
    result.add (r * cos(a), r * sin(a))

proc shapeInside*(shape: BlurShape): InsideProc =
  case shape
  of bshCircle: result = proc(u, v: float32): bool = u * u + v * v <= 1
  of bshRing: result = proc(u, v: float32): bool = insideRing(u, v)
  of bshSquare: result = proc(u, v: float32): bool = true
  of bshDiamond: result = proc(u, v: float32): bool = abs(u) + abs(v) <= 1
  of bshTriangle: result = proc(u, v: float32): bool = v <= 0.75'f32 and abs(u) <= (v + 1) / 1.75'f32
  of bshHexagon: result = proc(u, v: float32): bool = sqrt(u * u + v * v) <= polygonRadius(arctan2(v, u), 6, 0)
  of bshStar:
    let pts = starPoints()
    result = proc(u, v: float32): bool = inPoly(u, v, pts)
  of bshHeart: result = proc(u, v: float32): bool = insideHeart(u, v)
  of bshCross: result = proc(u, v: float32): bool = abs(u) <= 0.3'f32 or abs(v) <= 0.3'f32

# ---------------------------------------------------------------- shape blur

proc shapeRadius*(r: float32): float32 =
  ## The command supports at most 1000 px; nonfinite input uses zero radius.
  if r.classify notin {fcNan, fcInf, fcNegInf}: clamp(r, 0'f32, 1000'f32) else: 0

type ShapePrepared* = object
  spans: Spans

proc newShapePrepared*(r: float32; shape: BlurShape): ShapePrepared =
  ShapePrepared(spans: newSpans(shapeRadius(r), shapeInside(shape)))

proc checkedRun*(p: ShapePrepared; src: Image; o: Rect; ctx: Ctx): Option[seq[float32]] =
  ## Shape Blur of `o`: row prefix sums of the premultiplied window, spans per output row. None
  ## for malformed storage or oversized scratch.
  let n = src.ch
  if n == 0 or n > MaxC or o.isEmpty: return some(newSeq[float32]())
  let w = src.rect.width
  let h = src.rect.height
  let inputRow = w * n
  if src.data.len != inputRow * h: return
  let stride = (w + 1) * n
  let prefixLen = stride * h
  let rowLen = o.width * n
  let outputLen = rowLen * o.height
  const MaxSamples = 512 * 1024 * 1024
  if prefixLen > MaxSamples or outputLen > MaxSamples: return
  var prefix = newSeq[float64](prefixLen)
  if inputRow != 0:
    for y in 0 ..< h:
      var sum: array[MaxC, float64]
      for x in 0 ..< w:
        let s = y * inputRow + x * n
        let alpha = if ctx.alpha: src.data[s + n - 1] else: 1'f32
        for c in 0 ..< n:
          # Multiplication stays f32, exactly as premulWindow.
          let v = if ctx.alpha and c + 1 < n: src.data[s + c] * alpha else: src.data[s + c]
          sum[c] += float64(v)
          prefix[y * stride + (x + 1) * n + c] = sum[c]
  var res = newSeqOfCap[float32](outputLen)
  var acc = newSeq[float64](rowLen)
  let x = o.x0 - src.rect.x0
  for y in o.y0 ..< o.y1:
    for v in acc.mitems: v = 0
    for (dy, a, b) in p.spans.rows:
      let yy = y - src.rect.y0 + dy
      if yy < 0 or yy >= h: continue
      let row = yy * stride
      let lo = x + a
      let hi = x + b + 1
      let e = o.width
      if lo >= 0 and hi + e <= w:
        for k in 0 ..< rowLen: acc[k] += prefix[row + hi * n + k] - prefix[row + lo * n + k]
      else:
        # Direct kernel callers may omit the halo or request pixels outside src: zero extension.
        for i in 0 ..< o.width:
          let low = clamp(lo + i, 0, w) * n
          let high = clamp(hi + i, 0, w) * n
          for c in 0 ..< n: acc[i * n + c] += prefix[row + high + c] - prefix[row + low + c]
    var sample: array[MaxC, float32]
    for i in 0 ..< o.width:
      for c in 0 ..< n: sample[c] = float32(acc[i * n + c] / p.spans.count)
      unpremulPx(sample.toOpenArray(0, n - 1), ctx.alpha)
      for c in 0 ..< n: res.add sample[c]
  some(res)

proc shapeBlur*(src: Image; o: Rect; ctx: Ctx; radius: float32; shape: BlurShape): seq[float32] =
  ## Shape Blur: mean over a built-in shape of the given radius.
  newShapePrepared(radius, shape).checkedRun(src, o, ctx).get(newSeq[float32]())

# ---------------------------------------------------------------- smart blur

proc smartBlur*(src: Image; o: Rect; ctx: Ctx; radius, threshold: float32; quality: BlurQuality;
                mode: SmartBlurMode): seq[float32] =
  ## Smart Blur: averages only neighbours within `threshold` levels of the centre pixel (so edges
  ## stay crisp); Edge Only / Overlay Edge draw the edges in white.
  let n = src.ch
  let cc = ncol(ctx, n)
  let r = clamp(radius, 0.1'f32, 100'f32)
  let t = clamp(threshold, 0.1'f32, 100'f32) / 255
  let ri = asI32(float64(ceil(r)))
  let target = case quality
    of bqLow: 4
    of bqMedium: 7
    of bqHigh: 12
  let stride = max((ri + target - 1) div target, 1)
  var offs: seq[(int, int)]
  var dy = -ri
  while dy <= ri:
    var dx = -ri
    while dx <= ri:
      if float32(dx * dx + dy * dy) <= r * r + 0.5'f32: offs.add (dx, dy)
      dx += stride
    dy += stride
  let white = native(ctx, [1'f32, 1, 1, 1])
  let black = native(ctx, [0'f32, 0, 0, 1])
  result = src.crop(o)
  var q: array[MaxC, float32]
  var acc: array[MaxC, float32]
  var i = 0
  while i * n + n <= result.len:
    let at = i * n
    let (x, y) = xy(o, i)
    var me: array[MaxC, float32]
    for c in 0 ..< n: me[c] = result[at + c]
    template differs(): bool =
      (block:
        var d = false
        for c in 0 ..< cc:
          if abs(q[c] - me[c]) > t:
            d = true
            break
        d)
    if mode != sbNormal:
      var edge = false
      for (ddx, ddy) in [(1, 0), (0, 1), (-1, 0), (0, -1)]:
        for c in 0 ..< n: q[c] = src.get(x + ddx, y + ddy, c)
        if differs():
          edge = true
          break
      let a = if ctx.alpha: result[at + n - 1] else: 1'f32
      if edge:
        for c in 0 ..< cc: result[at + c] = white[c]
      elif mode == sbEdgeOnly:
        for c in 0 ..< cc: result[at + c] = black[c]
      if ctx.alpha: result[at + n - 1] = a
      inc i
      continue
    for c in 0 ..< n: acc[c] = 0
    var wsum = 0'f32
    for (ddx, ddy) in offs:
      for c in 0 ..< n: q[c] = src.get(x + ddx, y + ddy, c)
      if differs(): continue
      let a = if ctx.alpha: q[n - 1] else: 1'f32
      for c in 0 ..< n: acc[c] += (if ctx.alpha and c < n - 1: q[c] * a else: q[c])
      wsum += 1
    if wsum > 0:
      for c in 0 ..< n: result[at + c] = acc[c] / wsum
      unpremulPx(result.toOpenArray(at, at + n - 1), ctx.alpha)
    inc i

# ---------------------------------------------------------------- lens blur

type LensSpec* = object
  radius*: float32
  blades*: uint32
  curvature*, rotation*: float32
  depth*: DepthSource
  focal*: float32
  invert*: bool
  brightness*, threshold*, noise*: float32
  distribution*: Distribution
  mono*: bool
  seed*: uint32
  depthMap*: ref Image

const LensLevels = 8  ## depth levels the per-pixel iris radius is interpolated between

proc lensBlur*(src: Image; o: Rect; ctx: Ctx; spec: LensSpec): seq[float32] =
  ## Lens Blur: an iris-shaped (polygon → circle by curvature) bokeh kernel, with highlights
  ## above the threshold brightened first so they bloom into iris shapes. With a depth map the
  ## iris radius grows with distance from the focal plane. Optional noise restores grain.
  let n = src.ch
  let cc = ncol(ctx, n)
  let win = src.rect
  let (ww, wh) = (win.width, win.height)
  let rmax = clamp(spec.radius, 0'f32, 100'f32)
  let blades = clamp(spec.blades, 3'u32, 8'u32)
  let curv = clamp(spec.curvature, 0'f32, 100'f32) / 100
  let rot = degToRad(spec.rotation)
  let iris: InsideProc = proc(u, v: float32): bool =
    let rho = sqrt(u * u + v * v)
    let pr = polygonRadius(arctan2(v, u), blades, rot - Pi32 / 2)
    rho <= pr + (1 - pr) * curv
  var levels: seq[Spans]
  for k in 0 .. LensLevels: levels.add newSpans(rmax * float32(k) / float32(LensLevels), iris)
  # Specular highlight boost before blurring.
  var p = premulWindow(src, win, ctx.alpha)
  let boost = clamp(spec.brightness, 0'f32, 100'f32) / 100 * 4
  if boost > 0:
    let th = clamp(spec.threshold, 0'f32, 255'f32) / 255
    let sub = subtractive(ctx)
    var i = 0
    while i + n <= p.len:
      let a = if ctx.alpha: p[i + n - 1] else: 1'f32
      if a > 0:
        var st: array[MaxC, float32]
        for c in 0 ..< n: st[c] = (if ctx.alpha and c < n - 1: p[i + c] / a else: p[i + c])
        let l = luma(ctx, st.toOpenArray(0, n - 1))
        let k = 1 + boost * smoothstep(th, th + 0.02'f32, l)
        for c in 0 ..< cc: p[i + c] = (if sub: a - (a - p[i + c]) / k else: p[i + c] * k)
      i += n
  let pre = newPrefix(p, ww, wh, n)
  let focal = clamp(spec.focal, 0'f32, 255'f32) / 255
  let amount = clamp(spec.noise, 0'f32, 100'f32) / 100 * 0.25'f32
  var acc, acc2: array[MaxC, float64]
  var px: array[MaxC, float32]
  result = newSeqOfCap[float32](o.width * o.height * n)
  for y in o.y0 ..< o.y1:
    for x in o.x0 ..< o.x1:
      let (wx, wy) = (x - win.x0, y - win.y0)
      var d = none(float32)
      case spec.depth
      of dsNone: discard
      of dsTransparency:
        if ctx.alpha: d = some(src.get(x, y, n - 1))
      of dsLayerMask:
        if not spec.depthMap.isNil: d = some(spec.depthMap[].get(x, y, 0))
      if d.isSome and spec.invert: d = some(1 - d.get)
      let level = if d.isNone: float32(LensLevels) else: min(abs(d.get - focal) * float32(LensLevels), float32(LensLevels))
      let lo = asUsize(float64(floor(level)))
      let f = level - float32(lo)
      pre.mean(levels[lo], wx, wy, acc)
      if f > 1e-4'f32 and lo < LensLevels:
        pre.mean(levels[lo + 1], wx, wy, acc2)
        for c in 0 ..< n: acc[c] += (acc2[c] - acc[c]) * float64(f)
      for c in 0 ..< n: px[c] = float32(acc[c])
      unpremulPx(px.toOpenArray(0, n - 1), ctx.alpha)
      if amount > 0 and (not ctx.alpha or px[n - 1] > 0):
        for c in 0 ..< cc:
          let ch = if spec.mono: 0'u32 else: uint32(c)
          px[c] += (case spec.distribution
            of diUniform: (hash01(x, y, ch + 200, spec.seed) - 0.5'f32) * 2 * amount
            of diGaussian:
              let u1 = max(hash01(x, y, ch * 2 + 301, spec.seed), 1e-7'f32)
              let u2 = hash01(x, y, ch * 2 + 302, spec.seed)
              sqrt(-2 * ln(u1)) * cos(Tau32 * u2) * amount * 0.5'f32)
      for c in 0 ..< n: result.add px[c]
