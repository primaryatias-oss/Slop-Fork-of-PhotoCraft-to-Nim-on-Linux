## Float evaluation pipelines: a list of stages mapping normalised device values to the
## profile connection space (or back). This is the exact reference path; `Transform` builds
## faster lookup tables from it.

import ./curve, ./clut, ./math

const MaxPipe* = 16 ## Largest channel count flowing through a pipeline.

type
  StageKind* = enum
    skCurves, skInvCurves, skMatrix, skClut, skLabToXyz, skXyzToLab, skDecodeLabV4, skEncodeLabV4,
    skDecodeLabV2, skEncodeLabV2, skDecodeXyz, skEncodeXyz, skClamp01
  Stage* = object
    case kind*: StageKind
    of skCurves, skInvCurves: curves*: seq[Curve]
    of skMatrix:
      rows*, cols*: int
      m*: seq[float64]       ## `rows × cols`, row-major
      offset*: seq[float64]
    of skClut: clut*: Clut
    else: discard

  Pipeline* = object
    inputs*, outputs*: int
    stages*: seq[Stage]

  PipeVec* = array[MaxPipe, float32]

proc `==`*(a, b: Stage): bool =
  if a.kind != b.kind: return false
  case a.kind
  of skCurves, skInvCurves: a.curves == b.curves
  of skMatrix: a.rows == b.rows and a.cols == b.cols and a.m == b.m and a.offset == b.offset
  of skClut: a.clut == b.clut
  else: true

proc curvesStage*(c: seq[Curve]): Stage = Stage(kind: skCurves, curves: c)
proc invCurvesStage*(c: seq[Curve]): Stage = Stage(kind: skInvCurves, curves: c)
proc clutStage*(c: Clut): Stage = Stage(kind: skClut, clut: c)
proc matrixStage*(rows, cols: int; m, offset: seq[float64]): Stage =
  Stage(kind: skMatrix, rows: rows, cols: cols, m: m, offset: offset)
proc matrix3*(m: Mat3): Stage =
  var v: seq[float64]
  for r in m:
    for x in r: v.add x
  matrixStage(3, 3, v, @[0.0, 0.0, 0.0])
proc simpleStage*(k: StageKind): Stage =
  case k
  of skLabToXyz: Stage(kind: skLabToXyz)
  of skXyzToLab: Stage(kind: skXyzToLab)
  of skDecodeLabV4: Stage(kind: skDecodeLabV4)
  of skEncodeLabV4: Stage(kind: skEncodeLabV4)
  of skDecodeLabV2: Stage(kind: skDecodeLabV2)
  of skEncodeLabV2: Stage(kind: skEncodeLabV2)
  of skDecodeXyz: Stage(kind: skDecodeXyz)
  of skEncodeXyz: Stage(kind: skEncodeXyz)
  of skClamp01: Stage(kind: skClamp01)
  else: raise newException(CmsDefect, "not a simple stage")

const
  XyzScale = float32(65535.0 / 32768.0)
  V2L = float32(100.0 * 65535.0 / 65280.0)
  V2AB = float32(255.0 * 65535.0 / 65280.0)

proc outputs*(s: Stage; n: int): int =
  case s.kind
  of skMatrix: s.rows
  of skClut: s.clut.outputs
  else: n

proc run(s: Stage; v: var PipeVec; n: int): int {.inline.} =
  case s.kind
  of skCurves:
    for i in 0 ..< min(s.curves.len, MaxPipe): v[i] = s.curves[i].eval(v[i])
    n
  of skInvCurves:
    for i in 0 ..< min(s.curves.len, MaxPipe): v[i] = s.curves[i].evalInverse(v[i])
    n
  of skMatrix:
    var o: PipeVec
    for r in 0 ..< s.rows:
      var acc = s.offset[r]
      for c in 0 ..< s.cols: acc += s.m[r * s.cols + c] * float64(v[c])
      o[r] = float32(acc)
    for r in 0 ..< s.rows: v[r] = o[r]
    s.rows
  of skClut:
    var o: PipeVec
    s.clut.eval(v.toOpenArray(0, s.clut.inputs - 1), o)
    for k in 0 ..< s.clut.outputs: v[k] = o[k]
    s.clut.outputs
  of skLabToXyz:
    let x = labToXyzF32([v[0], v[1], v[2]])
    v[0] = x[0]; v[1] = x[1]; v[2] = x[2]
    3
  of skXyzToLab:
    let l = xyzToLabF32([max(v[0], 0.0'f32), max(v[1], 0.0'f32), max(v[2], 0.0'f32)])
    v[0] = l[0]; v[1] = l[1]; v[2] = l[2]
    3
  of skDecodeLabV4:
    v[0] *= 100.0
    v[1] = v[1] * 255.0'f32 - 128.0'f32
    v[2] = v[2] * 255.0'f32 - 128.0'f32
    3
  of skEncodeLabV4:
    v[0] = clamp(v[0] / 100.0'f32, 0.0'f32, 1.0'f32)
    v[1] = clamp((v[1] + 128.0'f32) / 255.0'f32, 0.0'f32, 1.0'f32)
    v[2] = clamp((v[2] + 128.0'f32) / 255.0'f32, 0.0'f32, 1.0'f32)
    3
  of skDecodeLabV2:
    v[0] *= V2L
    v[1] = v[1] * V2AB - 128.0'f32
    v[2] = v[2] * V2AB - 128.0'f32
    3
  of skEncodeLabV2:
    v[0] = clamp(v[0] / V2L, 0.0'f32, 1.0'f32)
    v[1] = clamp((v[1] + 128.0'f32) / V2AB, 0.0'f32, 1.0'f32)
    v[2] = clamp((v[2] + 128.0'f32) / V2AB, 0.0'f32, 1.0'f32)
    3
  of skDecodeXyz:
    for i in 0..2: v[i] *= XyzScale
    3
  of skEncodeXyz:
    for i in 0..2: v[i] = clamp(v[i] / XyzScale, 0.0'f32, 1.0'f32)
    3
  of skClamp01:
    for i in 0 ..< n: v[i] = clamp(v[i], 0.0'f32, 1.0'f32)
    n

proc newPipeline*(inputs: int; stages: seq[Stage]): Pipeline =
  var outputs = inputs
  for s in stages: outputs = s.outputs(outputs)
  Pipeline(inputs: inputs, outputs: outputs, stages: stages)

proc extend*(p: var Pipeline; stages: seq[Stage]) =
  for s in stages:
    p.outputs = s.outputs(p.outputs)
    p.stages.add s

proc eval*(p: Pipeline; input: openArray[float32]; outp: var openArray[float32]) {.inline.} =
  ## Evaluates one pixel: `input` has `inputs` values, `outp` receives `outputs` values.
  var v: PipeVec
  for i in 0 ..< p.inputs: v[i] = input[i]
  var n = p.inputs
  for s in p.stages: n = s.run(v, n)
  for i in 0 ..< p.outputs: outp[i] = v[i]

proc isNoop(s: Stage): bool =
  case s.kind
  of skCurves, skInvCurves:
    for c in s.curves:
      if not c.isIdentity: return false
    true
  of skMatrix:
    if s.rows != 3 or s.cols != 3: return false
    for o in s.offset:
      if abs(o) >= 1e-12: return false
    for i, v in s.m:
      if abs(v - (if i mod 4 == 0: 1.0 else: 0.0)) >= 1e-12: return false
    true
  else: false

proc optimize*(p: var Pipeline) =
  ## Removes no-op stages and cancels adjacent inverse pairs (Lab↔XYZ, encode/decode).
  var outs: seq[Stage]
  for s in p.stages:
    if s.isNoop: continue
    if outs.len > 0:
      let last = outs[^1].kind
      let cancels = (last == skLabToXyz and s.kind == skXyzToLab) or (last == skXyzToLab and s.kind == skLabToXyz) or
        (last == skEncodeLabV4 and s.kind == skDecodeLabV4) or (last == skEncodeLabV2 and s.kind == skDecodeLabV2) or
        (last == skEncodeXyz and s.kind == skDecodeXyz) or
        (last == skDecodeLabV4 and s.kind == skEncodeLabV4) or (last == skDecodeLabV2 and s.kind == skEncodeLabV2) or
        (last == skDecodeXyz and s.kind == skEncodeXyz)
      if cancels:
        discard outs.pop()
        continue
      let a = outs[^1]
      if a.kind == skMatrix and a.rows == 3 and a.cols == 3 and s.kind == skMatrix and s.rows == 3 and s.cols == 3:
        var m = newSeq[float64](9)
        var off = newSeq[float64](3)
        for r in 0..2:
          for c in 0..2:
            var acc = 0.0
            for k in 0..2: acc += s.m[r * 3 + k] * a.m[k * 3 + c]
            m[r * 3 + c] = acc
          var o = 0.0
          for k in 0..2: o += s.m[r * 3 + k] * a.offset[k]
          off[r] = o + s.offset[r]
        discard outs.pop()
        outs.add matrixStage(3, 3, m, off)
        continue
    outs.add s
  p.stages = outs
