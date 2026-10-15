## Multi-dimensional colour lookup tables (ICC CLUTs) with tetrahedral interpolation.
##
## Grid layout follows ICC.1: the first input channel varies slowest, output values of one
## node are stored together. 3-input tables use tetrahedral interpolation (Sakamoto 1980 /
## Kasson et al. 1995); 4-input tables interpolate linearly along the first input between
## two tetrahedral evaluations; other input counts use multilinear interpolation.

import ../rmath

const MaxChannels* = 15 ## Maximum number of input or output channels of a CLUT.

type
  Clut* = object
    inputs*, outputs*: int
    grid*: seq[int]     ## Grid points per input dimension.
    data*: seq[float32] ## Normalised node values, `outputs` per node.
    strides: seq[int]

  CmsDefect* = object of ValueError ## Invalid table construction (caller bug or hostile data).

proc newClut*(grid: seq[int]; outputs: int; data: seq[float32]): Clut =
  ## Builds a table from node data. Raises `CmsDefect` on bad dimensions or data length.
  let inputs = grid.len
  if inputs < 1 or inputs > MaxChannels or outputs < 1 or outputs > MaxChannels:
    raise newException(CmsDefect, "bad CLUT dimensions")
  var strides = newSeq[int](inputs)
  var s = outputs
  for i in countdown(inputs - 1, 0):
    strides[i] = s
    s *= max(grid[i], 1)
  if data.len != s: raise newException(CmsDefect, "CLUT data length")
  Clut(inputs: inputs, outputs: outputs, grid: grid, data: data, strides: strides)

proc `==`*(a, b: Clut): bool = a.grid == b.grid and a.outputs == b.outputs and a.data == b.data

proc nodes*(c: Clut): int = c.data.len div c.outputs

proc sampleClut*(grid: seq[int]; outputs: int; f: proc(inp: openArray[float32]; outp: var openArray[float32])): Clut =
  ## Builds a table by sampling `f` at every node (inputs normalised to `[0, 1]`).
  let inputs = grid.len
  var total = 1
  for g in grid: total *= g
  var data = newSeq[float32](total * outputs)
  var idx = newSeq[int](inputs)
  var inp = newSeq[float32](inputs)
  var o = newSeq[float32](outputs)
  for node in 0 ..< total:
    var rem = node
    for d in countdown(inputs - 1, 0):
      idx[d] = rem mod grid[d]
      rem = rem div grid[d]
    for d in 0 ..< inputs:
      inp[d] = if grid[d] > 1: float32(idx[d]) / float32(grid[d] - 1) else: 0.0'f32
    f(inp, o)
    for k in 0 ..< outputs: data[node * outputs + k] = o[k]
  newClut(grid, outputs, data)

proc locate(c: Clut; d: int; v: float32): (int, float32, int) {.inline.} =
  let n = c.grid[d]
  if n < 2: return (0, 0.0'f32, 0)
  let p = clamp(v, 0.0'f32, 1.0'f32) * float32(n - 1)
  let i = min(asIdx(p), n - 2)
  (i * c.strides[d], p - float32(i), c.strides[d])

proc tetra*(c: Clut; base: int; s: array[3, int]; r: array[3, float32]; outp: var openArray[float32]) {.inline.} =
  ## Tetrahedral interpolation inside one cube of three dimensions (strides relative to `base`).
  let (rx, ry, rz) = (r[0], r[1], r[2])
  let (sx, sy, sz) = (s[0], s[1], s[2])
  var p1, p2, p3: int
  var w1, w2, w3: float32
  if rx >= ry:
    if ry >= rz: (p1, p2, p3, w1, w2, w3) = (sx, sx + sy, sx + sy + sz, rx, ry, rz)
    elif rx >= rz: (p1, p2, p3, w1, w2, w3) = (sx, sx + sz, sx + sy + sz, rx, rz, ry)
    else: (p1, p2, p3, w1, w2, w3) = (sz, sx + sz, sx + sy + sz, rz, rx, ry)
  elif rx >= rz: (p1, p2, p3, w1, w2, w3) = (sy, sx + sy, sx + sy + sz, ry, rx, rz)
  elif ry >= rz: (p1, p2, p3, w1, w2, w3) = (sy, sy + sz, sx + sy + sz, ry, rz, rx)
  else: (p1, p2, p3, w1, w2, w3) = (sz, sy + sz, sx + sy + sz, rz, ry, rx)
  let o = min(c.outputs, MaxChannels)
  for k in 0 ..< o:
    let v0 = c.data[base + k]
    let v1 = c.data[base + p1 + k]
    let v2 = c.data[base + p2 + k]
    let v3 = c.data[base + p3 + k]
    outp[k] = v0 + (v1 - v0) * w1 + (v2 - v1) * w2 + (v3 - v2) * w3

proc evalMultilinear*(c: Clut; input: openArray[float32]; outp: var openArray[float32]) =
  ## Multilinear interpolation over all 2^n cube corners.
  let n = c.inputs
  var base = 0
  var r: array[MaxChannels, float32]
  var s: array[MaxChannels, int]
  for d in 0 ..< n:
    let (b, rr, ss) = c.locate(d, input[d])
    base += b
    r[d] = rr
    s[d] = ss
  let o = c.outputs
  for k in 0 ..< o: outp[k] = 0.0
  for corner in 0 ..< (1 shl n):
    var w = 1.0'f32
    var off = base
    for d in 0 ..< n:
      if (corner and (1 shl d)) != 0:
        w *= r[d]
        off += s[d]
      else:
        w *= 1.0'f32 - r[d]
    if w == 0.0'f32: continue
    for k in 0 ..< o: outp[k] += w * c.data[off + k]

proc eval1(c: Clut; v: float32; outp: var openArray[float32]) =
  let (b, r, s) = c.locate(0, v)
  for k in 0 ..< c.outputs:
    let a = c.data[b + k]
    let cc = if s > 0: c.data[b + s + k] else: a
    outp[k] = a + (cc - a) * r

proc eval*(c: Clut; input: openArray[float32]; outp: var openArray[float32]) =
  ## Interpolates at `input` (`inputs` values) into `outp` (`outputs` values).
  case c.inputs
  of 1: c.eval1(input[0], outp)
  of 3:
    let (b0, r0, s0) = c.locate(0, input[0])
    let (b1, r1, s1) = c.locate(1, input[1])
    let (b2, r2, s2) = c.locate(2, input[2])
    c.tetra(b0 + b1 + b2, [s0, s1, s2], [r0, r1, r2], outp)
  of 4:
    let (b0, r0, s0) = c.locate(0, input[0])
    let (b1, r1, s1) = c.locate(1, input[1])
    let (b2, r2, s2) = c.locate(2, input[2])
    let (b3, r3, s3) = c.locate(3, input[3])
    let base = b0 + b1 + b2 + b3
    var lo: array[MaxChannels, float32]
    c.tetra(base, [s1, s2, s3], [r1, r2, r3], lo)
    if r0 > 0.0'f32 and s0 > 0:
      var hi: array[MaxChannels, float32]
      c.tetra(base + s0, [s1, s2, s3], [r1, r2, r3], hi)
      for k in 0 ..< c.outputs: outp[k] = lo[k] + (hi[k] - lo[k]) * r0
    else:
      for k in 0 ..< c.outputs: outp[k] = lo[k]
  else: c.evalMultilinear(input, outp)
