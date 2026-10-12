## VP8 key frames (the lossy WebP bitstream), from RFC 6386: the boolean entropy coder, the
## 4×4 DCT/WHT, intra predictors, a clean-room encoder (port of `crates/codecs/src/codecs/vp8`)
## and a decoder (segments, multiple token partitions, simple and normal loop filters).

import std/[math]
import ./vp8tables

const
  MaxDimension* = 16383
  DcPred* = 0'u8
  VPred* = 1'u8
  HPred* = 2'u8
  TmPred* = 3'u8
  BPred* = 4'u8
  BDcPred = 0'u8
  BTmPred = 1'u8
  BVePred = 2'u8
  BHePred = 3'u8
  BLdPred = 4'u8
  BRdPred = 5'u8
  BVrPred = 6'u8
  BVlPred = 7'u8
  BHdPred = 8'u8
  BHuPred = 9'u8
  Dct0 = 0'u8
  Dct1 = 1'u8
  Dct2 = 2'u8
  Dct3 = 3'u8
  Dct4 = 4'u8
  DctCat1 = 5'u8
  DctCat2 = 6'u8
  DctCat3 = 7'u8
  DctCat4 = 8'u8
  DctCat5 = 9'u8
  DctCat6 = 10'u8
  DctEob = 11'u8
  KfYmodeTree: array[8, int8] = [-4'i8, 2, 4, 6, -0, -1, -2, -3]
  KfYmodeProb: array[4, uint8] = [145'u8, 156, 163, 128]
  UvModeTree: array[6, int8] = [-0'i8, 2, -1, 4, -2, -3]
  KfUvModeProb: array[3, uint8] = [142'u8, 114, 183]
  BmodeTree: array[18, int8] = [-0'i8, 2, -1, 4, -2, 6, 8, 12, -3, 10, -5, -6, -4, 14, -7, 16, -8, -9]
  CoeffTree: array[22, int8] = [-11'i8, 2, -0, 4, -1, 6, 8, 12, -2, 10, -3, -4, 14, 16, -5, -6, 18, 20, -7, -8, -9, -10]
  Pcat: array[6, seq[uint8]] = [@[159'u8], @[165'u8, 145], @[173'u8, 148, 140], @[176'u8, 155, 140, 135],
                                @[180'u8, 157, 141, 134, 130], @[254'u8, 254, 243, 230, 196, 177, 153, 140, 133, 130, 129]]
  CatBase: array[6, int32] = [5'i32, 7, 11, 19, 35, 67]
  CoeffBands: array[17, int] = [0, 1, 2, 3, 6, 4, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 0]
  Zigzag: array[16, int] = [0, 1, 4, 8, 5, 2, 3, 6, 9, 12, 13, 10, 7, 11, 14, 15]
  MbSegmentTree: array[6, int8] = [2'i8, 4, -0, -1, -2, -3]

type
  Vp8Error* = object of CatchableError

proc vp8err(msg: string): ref Vp8Error = newException(Vp8Error, msg)

# ---------------------------------------------------------------- bool encoder

type BoolEncoder* = object
  output: seq[byte]
  range: uint32
  bottom: uint32
  bitCount: int32

proc newBoolEncoder*(): BoolEncoder = BoolEncoder(range: 255, bottom: 0, bitCount: 24)

proc addOne(e: var BoolEncoder) =
  for i in countdown(e.output.len - 1, 0):
    if e.output[i] == 255: e.output[i] = 0
    else:
      inc e.output[i]
      return

proc put*(e: var BoolEncoder; prob: uint8; bit: bool) =
  let split = 1'u32 + (((e.range - 1) * uint32(prob)) shr 8)
  if bit:
    e.bottom = e.bottom + split
    e.range -= split
  else:
    e.range = split
  while e.range < 128:
    e.range = e.range shl 1
    if (e.bottom and (1'u32 shl 31)) != 0: e.addOne
    e.bottom = e.bottom shl 1
    dec e.bitCount
    if e.bitCount == 0:
      e.output.add byte(e.bottom shr 24)
      e.bottom = e.bottom and ((1'u32 shl 24) - 1)
      e.bitCount = 8

proc putFlag*(e: var BoolEncoder; bit: bool) = e.put(128, bit)

proc putLiteral*(e: var BoolEncoder; n: int; value: uint32) =
  for i in countdown(n - 1, 0): e.putFlag(((value shr i) and 1) != 0)

proc findPath(tree: openArray[int8]; node: int; value: uint8; path: var seq[(int, bool)]): bool =
  for bit in [false, true]:
    let i = node + (if bit: 1 else: 0)
    if i >= tree.len: continue
    let entry = tree[i]
    path.add (node, bit)
    if entry <= 0:
      if uint8(-int(entry)) == value: return true
    elif findPath(tree, int(entry), value, path): return true
    discard path.pop
  false

proc putTree*(e: var BoolEncoder; tree: openArray[int8]; probs: openArray[uint8]; value: uint8; start = 0) =
  var path: seq[(int, bool)]
  if not findPath(tree, start, value, path): return
  for (node, bit) in path:
    let p = if (node shr 1) < probs.len: probs[node shr 1] else: 128'u8
    e.put(p, bit)

proc finish*(e: var BoolEncoder): seq[byte] =
  var c = e.bitCount
  var v = e.bottom
  if c <= 32 and (v and (1'u32 shl (32 - c))) != 0: e.addOne
  v = v shl uint32(c and 7)
  c = c shr 3
  while c > 0:
    v = v shl 8
    dec c
  for _ in 0..3:
    e.output.add byte(v shr 24)
    v = v shl 8
  e.output

# ---------------------------------------------------------------- bool decoder

type BoolDecoder = object
  data: ptr UncheckedArray[byte]
  len, pos: int
  value: uint32
  range: uint32
  bitCount: int

proc initBoolDecoder(data: ptr UncheckedArray[byte]; start, len: int): BoolDecoder =
  result = BoolDecoder(data: data, len: start + len, pos: start, range: 255)
  for _ in 0..1:
    result.value = (result.value shl 8) or (if result.pos < result.len: uint32(data[result.pos]) else: 0'u32)
    inc result.pos
  result.bitCount = 0

proc readBool(d: var BoolDecoder; prob: int): bool {.inline.} =
  let split = 1'u32 + (((d.range - 1) * uint32(prob)) shr 8)
  let bigSplit = split shl 8
  if d.value >= bigSplit:
    d.range -= split
    d.value -= bigSplit
    result = true
  else:
    d.range = split
    result = false
  while d.range < 128:
    d.value = d.value shl 1
    d.range = d.range shl 1
    inc d.bitCount
    if d.bitCount == 8:
      d.bitCount = 0
      d.value = d.value or (if d.pos < d.len: uint32(d.data[d.pos]) else: 0'u32)
      inc d.pos

proc readLiteral(d: var BoolDecoder; n: int): int =
  for _ in 0 ..< n: result = (result shl 1) or (if d.readBool(128): 1 else: 0)

proc readSigned(d: var BoolDecoder; n: int): int =
  ## A flagged, signed delta: flag, magnitude, sign.
  if not d.readBool(128): return 0
  let v = d.readLiteral(n)
  if d.readBool(128): -v else: v

proc readTree(d: var BoolDecoder; tree: openArray[int8]; probs: openArray[uint8]; start = 0): int =
  var i = start
  while true:
    i = int(tree[i + (if d.readBool(int(probs[i shr 1])): 1 else: 0)])
    if i <= 0: break
  -i

# ---------------------------------------------------------------- transforms

const
  C1 = 20091'i32
  S1 = 35468'i32

proc fdct*(input: array[16, int32]): array[16, int32] =
  var tmp: array[16, int32]
  for i in 0..3:
    let (x0, x1, x2, x3) = (input[i], input[4 + i], input[8 + i], input[12 + i])
    let a = x0 + x3
    let b = x1 + x2
    let c = x1 - x2
    let d = x0 - x3
    tmp[i] = a + b
    tmp[8 + i] = a - b
    tmp[4 + i] = clamp((d * (65536'i32 + C1) + c * S1 + 32768) shr 16, -32768'i32, 32767'i32)
    tmp[12 + i] = clamp((d * S1 - c * (65536'i32 + C1) + 32768) shr 16, -32768'i32, 32767'i32)
  for r in 0..3:
    let (x0, x1, x2, x3) = (tmp[r * 4], tmp[r * 4 + 1], tmp[r * 4 + 2], tmp[r * 4 + 3])
    let a = x0 + x3
    let b = x1 + x2
    let c = x1 - x2
    let d = x0 - x3
    result[r * 4] = (a + b + 1) shr 1
    result[r * 4 + 2] = (a - b + 1) shr 1
    result[r * 4 + 1] = clamp((d * (65536'i32 + C1) + c * S1 + 65536) shr 17, -32768'i32, 32767'i32)
    result[r * 4 + 3] = clamp((d * S1 - c * (65536'i32 + C1) + 65536) shr 17, -32768'i32, 32767'i32)

proc idct*(input: array[16, int32]): array[16, int32] =
  var tmp: array[16, int32]
  for i in 0..3:
    let (ip0, ip4, ip8, ip12) = (input[i], input[4 + i], input[8 + i], input[12 + i])
    let a1 = ip0 + ip8
    let b1 = ip0 - ip8
    var temp1 = (ip4 * S1) shr 16
    var temp2 = ip12 + ((ip12 * C1) shr 16)
    let c1 = temp1 - temp2
    temp1 = ip4 + ((ip4 * C1) shr 16)
    temp2 = (ip12 * S1) shr 16
    let d1 = temp1 + temp2
    tmp[i] = a1 + d1
    tmp[12 + i] = a1 - d1
    tmp[4 + i] = b1 + c1
    tmp[8 + i] = b1 - c1
  for r in 0..3:
    let (ip0, ip1, ip2, ip3) = (tmp[r * 4], tmp[r * 4 + 1], tmp[r * 4 + 2], tmp[r * 4 + 3])
    let a1 = ip0 + ip2
    let b1 = ip0 - ip2
    var temp1 = (ip1 * S1) shr 16
    var temp2 = ip3 + ((ip3 * C1) shr 16)
    let c1 = temp1 - temp2
    temp1 = ip1 + ((ip1 * C1) shr 16)
    temp2 = (ip3 * S1) shr 16
    let d1 = temp1 + temp2
    result[r * 4] = (a1 + d1 + 4) shr 3
    result[r * 4 + 3] = (a1 - d1 + 4) shr 3
    result[r * 4 + 1] = (b1 + c1 + 4) shr 3
    result[r * 4 + 2] = (b1 - c1 + 4) shr 3

proc fwht*(input: array[16, int32]): array[16, int32] =
  var tmp: array[16, int32]
  for i in 0..3:
    let (x0, x1, x2, x3) = (input[i], input[4 + i], input[8 + i], input[12 + i])
    let a1 = x0 + x3
    let b1 = x1 + x2
    let c1 = x1 - x2
    let d1 = x0 - x3
    tmp[i] = a1 + b1
    tmp[4 + i] = c1 + d1
    tmp[8 + i] = a1 - b1
    tmp[12 + i] = d1 - c1
  proc half(v: int32): int32 = (v + (if v > 0: 1'i32 elif v < 0: -1'i32 else: 0'i32)) div 2
  for r in 0..3:
    let (x0, x1, x2, x3) = (tmp[r * 4], tmp[r * 4 + 1], tmp[r * 4 + 2], tmp[r * 4 + 3])
    let a1 = x0 + x3
    let b1 = x1 + x2
    let c1 = x1 - x2
    let d1 = x0 - x3
    result[r * 4] = half(a1 + b1)
    result[r * 4 + 1] = half(c1 + d1)
    result[r * 4 + 2] = half(a1 - b1)
    result[r * 4 + 3] = half(d1 - c1)

proc iwht*(input: array[16, int32]): array[16, int32] =
  var tmp: array[16, int32]
  for i in 0..3:
    let a1 = input[i] + input[12 + i]
    let b1 = input[4 + i] + input[8 + i]
    let c1 = input[4 + i] - input[8 + i]
    let d1 = input[i] - input[12 + i]
    tmp[i] = a1 + b1
    tmp[4 + i] = c1 + d1
    tmp[8 + i] = a1 - b1
    tmp[12 + i] = d1 - c1
  for r in 0..3:
    let a1 = tmp[r * 4] + tmp[r * 4 + 3]
    let b1 = tmp[r * 4 + 1] + tmp[r * 4 + 2]
    let c1 = tmp[r * 4 + 1] - tmp[r * 4 + 2]
    let d1 = tmp[r * 4] - tmp[r * 4 + 3]
    result[r * 4] = (a1 + b1 + 3) shr 3
    result[r * 4 + 1] = (c1 + d1 + 3) shr 3
    result[r * 4 + 2] = (a1 - b1 + 3) shr 3
    result[r * 4 + 3] = (d1 - c1 + 3) shr 3

# ---------------------------------------------------------------- planes and prediction

type
  Plane* = object
    data*: seq[uint8]
    width*: int
  SubEdges = object
    above: array[8, uint8]
    left: array[4, uint8]
    p: uint8

proc newPlane*(width, height: int): Plane = Plane(data: newSeq[uint8](width * height), width: width)

proc at(p: Plane; x, y: int): uint8 {.inline.} =
  if y < 0: 127'u8
  elif x < 0: 129'u8
  else:
    let i = y * p.width + x
    if i < p.data.len: p.data[i] else: 127'u8

proc set(p: var Plane; x, y: int; v: uint8) {.inline.} =
  let i = y * p.width + x
  if i >= 0 and i < p.data.len: p.data[i] = v

proc clamp255(v: int32): uint8 {.inline.} = uint8(clamp(v, 0'i32, 255'i32))

proc predictBlock(plane: Plane; x, y, n: int; mode: uint8; outp: var openArray[uint8]) =
  var above, left: array[16, uint8]
  for i in 0 ..< n:
    above[i] = plane.at(x + i, y - 1)
    left[i] = plane.at(x - 1, y + i)
  case mode
  of VPred:
    for r in 0 ..< n:
      for c in 0 ..< n: outp[r * n + c] = above[c]
  of HPred:
    for r in 0 ..< n:
      for c in 0 ..< n: outp[r * n + c] = left[r]
  of TmPred:
    let p = int32(plane.at(x - 1, y - 1))
    for r in 0 ..< n:
      for c in 0 ..< n: outp[r * n + c] = clamp255(int32(left[r]) + int32(above[c]) - p)
  else:
    let shift = if n == 16: 4 else: 3
    var v: uint8
    if y == 0 and x == 0: v = 128
    elif y == 0:
      var s = 0'u32
      for i in 0 ..< n: s += uint32(left[i])
      v = uint8((s + (1'u32 shl (shift - 1))) shr shift)
    elif x == 0:
      var s = 0'u32
      for i in 0 ..< n: s += uint32(above[i])
      v = uint8((s + (1'u32 shl (shift - 1))) shr shift)
    else:
      var s = 0'u32
      for i in 0 ..< n: s += uint32(above[i]) + uint32(left[i])
      v = uint8((s + (1'u32 shl shift)) shr (shift + 1))
    for i in 0 ..< n * n: outp[i] = v

proc avg2(a, b: uint8): uint8 {.inline.} = uint8((uint16(a) + uint16(b) + 1) shr 1)
proc avg3(a, b, c: uint8): uint8 {.inline.} = uint8((uint16(a) + 2 * uint16(b) + uint16(c) + 2) shr 2)

proc gatherEdges(plane: Plane; mx, my, b, mbCols: int): SubEdges =
  let bx = mx + (b and 3) * 4
  let by = my + (b shr 2) * 4
  for i in 0..3:
    result.above[i] = plane.at(bx + i, by - 1)
    result.left[i] = plane.at(bx - 1, by + i)
  result.p = plane.at(bx - 1, by - 1)
  if (b and 3) == 3:
    let lastCol = mx div 16 + 1 == mbCols
    for i in 0..3:
      let ax = if lastCol: mx + 15 else: mx + 16 + i
      result.above[4 + i] = plane.at(ax, my - 1)
  else:
    for i in 0..3: result.above[4 + i] = plane.at(bx + 4 + i, by - 1)

proc pick8(a: array[8, uint8]; p: uint8; i: int): uint8 {.inline.} = (if i < 0: p else: a[i])
proc pick4(a: array[4, uint8]; p: uint8; i: int): uint8 {.inline.} = (if i < 0: p else: a[i])

proc predictSub(e: SubEdges; mode: uint8; outp: var array[16, uint8]) =
  let a = e.above
  let l = e.left
  let ed = [l[3], l[2], l[1], l[0], e.p, a[0], a[1], a[2], a[3]]
  template e3(i: int): uint8 = avg3(ed[i - 1], ed[i], ed[i + 1])
  template e2(i: int): uint8 = avg2(ed[i], ed[i + 1])
  template ap(i: int): uint8 = pick8(a, e.p, i)
  template a3(i: int): uint8 = avg3(ap(i - 1), ap(i), ap(i + 1))
  template a2(i: int): uint8 = avg2(ap(i), ap(i + 1))
  template lp(i: int): uint8 = pick4(l, e.p, i)
  template l3(i: int): uint8 = avg3(lp(i - 1), lp(i), lp(i + 1))
  template l2(i: int): uint8 = avg2(lp(i), lp(i + 1))
  var b: array[4, array[4, uint8]]
  case mode
  of BTmPred:
    for r in 0..3:
      for c in 0..3: b[r][c] = clamp255(int32(l[r]) + int32(a[c]) - int32(e.p))
  of BVePred:
    for c in 0..3:
      let v = a3(c)
      for r in 0..3: b[r][c] = v
  of BHePred:
    let rows = [l3(0), l3(1), l3(2), avg3(l[2], l[3], l[3])]
    for r in 0..3:
      for c in 0..3: b[r][c] = rows[r]
  of BLdPred:
    b[0][0] = a3(1); b[0][1] = a3(2); b[1][0] = b[0][1]; b[0][2] = a3(3); b[1][1] = b[0][2]; b[2][0] = b[0][2]
    b[0][3] = a3(4); b[1][2] = b[0][3]; b[2][1] = b[0][3]; b[3][0] = b[0][3]; b[1][3] = a3(5); b[2][2] = b[1][3]
    b[3][1] = b[1][3]; b[2][3] = a3(6); b[3][2] = b[2][3]; b[3][3] = avg3(a[6], a[7], a[7])
  of BRdPred:
    b[3][0] = e3(1); b[3][1] = e3(2); b[2][0] = b[3][1]; b[3][2] = e3(3); b[2][1] = b[3][2]; b[1][0] = b[3][2]
    b[3][3] = e3(4); b[2][2] = b[3][3]; b[1][1] = b[3][3]; b[0][0] = b[3][3]; b[2][3] = e3(5); b[1][2] = b[2][3]
    b[0][1] = b[2][3]; b[1][3] = e3(6); b[0][2] = b[1][3]; b[0][3] = e3(7)
  of BVrPred:
    b[3][0] = e3(2); b[2][0] = e3(3); b[3][1] = e3(4); b[1][0] = b[3][1]; b[2][1] = e2(4); b[0][0] = b[2][1]
    b[3][2] = e3(5); b[1][1] = b[3][2]; b[2][2] = e2(5); b[0][1] = b[2][2]; b[3][3] = e3(6); b[1][2] = b[3][3]
    b[2][3] = e2(6); b[0][2] = b[2][3]; b[1][3] = e3(7); b[0][3] = e2(7)
  of BVlPred:
    b[0][0] = a2(0); b[1][0] = a3(1); b[2][0] = a2(1); b[0][1] = b[2][0]; b[1][1] = a3(2); b[3][0] = b[1][1]
    b[2][1] = a2(2); b[0][2] = b[2][1]; b[3][1] = a3(3); b[1][2] = b[3][1]; b[2][2] = a2(3); b[0][3] = b[2][2]
    b[3][2] = a3(4); b[1][3] = b[3][2]; b[2][3] = a3(5); b[3][3] = a3(6)
  of BHdPred:
    b[3][0] = e2(0); b[3][1] = e3(1); b[2][0] = e2(1); b[3][2] = b[2][0]; b[2][1] = e3(2); b[3][3] = b[2][1]
    b[2][2] = e2(2); b[1][0] = b[2][2]; b[2][3] = e3(3); b[1][1] = b[2][3]; b[1][2] = e2(3); b[0][0] = b[1][2]
    b[1][3] = e3(4); b[0][1] = b[1][3]; b[0][2] = e3(5); b[0][3] = e3(6)
  of BHuPred:
    b[0][0] = l2(0); b[0][1] = l3(1); b[0][2] = l2(1); b[1][0] = b[0][2]; b[0][3] = l3(2); b[1][1] = b[0][3]
    b[1][2] = l2(2); b[2][0] = b[1][2]; b[1][3] = avg3(l[2], l[3], l[3]); b[2][1] = b[1][3]; b[2][2] = l[3]
    b[2][3] = l[3]
    for c in 0..3: b[3][c] = l[3]
  else:
    var s = 0'u32
    for i in 0..3: s += uint32(a[i]) + uint32(l[i])
    let v = uint8((s + 4) shr 3)
    for r in 0..3:
      for c in 0..3: b[r][c] = v
  for r in 0..3:
    for c in 0..3: outp[r * 4 + c] = b[r][c]

# ---------------------------------------------------------------- encoder

type
  Yuv* = object
    width*, height*: int
    y*, u*, v*: seq[uint8]

  Quant = object
    yDc, yAc, y2Dc, y2Ac, uvDc, uvAc: int32

  Block = object
    coeffs: array[16, int32]
    first, eob: int

  Macroblock = object
    yMode: uint8
    bModes: array[16, uint8]
    uvMode: uint8
    hasY2: bool
    y2: Block
    y, u, v: seq[Block]
    skip: bool

  Encoder = object
    src: ptr Yuv
    mbCols, mbRows: int
    q: Quant
    recY, recU, recV: Plane
    mbs: seq[Macroblock]

  NodeStats = seq[array[8, array[3, array[11, array[2, uint32]]]]]
  Probs = array[4, array[8, array[3, array[11, uint8]]]]

proc yuvFromRgb*(rgb: openArray[uint8]; width, height, stride: int): Yuv =
  ## Studio-range BT.601 Y'CbCr with chroma averaged over each 2 × 2 block.
  let w = width
  let h = height
  let cw = (w + 1) div 2
  let ch = (h + 1) div 2
  result = Yuv(width: w, height: h, y: newSeq[uint8](w * h), u: newSeq[uint8](cw * ch), v: newSeq[uint8](cw * ch))
  template px(x, yy: int): (int32, int32, int32) =
    (let i = (min(yy, h - 1) * w + min(x, w - 1)) * stride; (int32(rgb[i]), int32(rgb[i + 1]), int32(rgb[i + 2])))
  for yy in 0 ..< h:
    for x in 0 ..< w:
      let (r, g, b) = px(x, yy)
      result.y[yy * w + x] = uint8(((66 * r + 129 * g + 25 * b + 128) shr 8) + 16)
  for cy in 0 ..< ch:
    for cx in 0 ..< cw:
      var r, g, b: int32
      for (dx, dy) in [(0, 0), (1, 0), (0, 1), (1, 1)]:
        let (pr, pg, pb) = px(cx * 2 + dx, cy * 2 + dy)
        r += pr; g += pg; b += pb
      r = (r + 2) shr 2; g = (g + 2) shr 2; b = (b + 2) shr 2
      result.u[cy * cw + cx] = uint8(clamp(((-38 * r - 74 * g + 112 * b + 128) shr 8) + 128, 0'i32, 255'i32))
      result.v[cy * cw + cx] = uint8(clamp(((112 * r - 94 * g - 18 * b + 128) shr 8) + 128, 0'i32, 255'i32))

proc quantizerIndex*(quality: int): int =
  let q = float64(min(quality, 100))
  let t = (100.0 - q) / 100.0
  int(clamp(round(pow(t, 1.1) * 127.0), 0.0, 127.0))

proc newQuant(qi: int): Quant =
  let i = clamp(qi, 0, 127)
  Quant(yDc: DcQLookup[i], yAc: AcQLookup[i], y2Dc: DcQLookup[i] * 2, y2Ac: max(AcQLookup[i] * 155 div 100, 8'i32),
        uvDc: min(DcQLookup[i], 132'i32), uvAc: AcQLookup[i])

proc quantizeCoeff(coeff, factor: int32; dc: bool): int32 =
  let bias = if dc: factor div 2 else: factor * 7 div 16
  let q = min((abs(coeff) + bias) div factor, 2048'i32)
  if coeff < 0: -q else: q

const
  TypeYAfterY2 = 0
  TypeY2 = 1
  TypeUv = 2
  TypeYWithDc = 3

proc tokenOf(v: int32): (uint8, int, uint32) =
  ## (token, category index or -1, extra bits)
  let a = abs(v)
  case a
  of 0: (Dct0, -1, 0'u32)
  of 1: (Dct1, -1, 0'u32)
  of 2: (Dct2, -1, 0'u32)
  of 3: (Dct3, -1, 0'u32)
  of 4: (Dct4, -1, 0'u32)
  of 5..6: (DctCat1, 0, uint32(a - CatBase[0]))
  of 7..10: (DctCat2, 1, uint32(a - CatBase[1]))
  of 11..18: (DctCat3, 2, uint32(a - CatBase[2]))
  of 19..34: (DctCat4, 3, uint32(a - CatBase[3]))
  of 35..66: (DctCat5, 4, uint32(a - CatBase[4]))
  else: (DctCat6, 5, uint32(a - CatBase[5]))

proc blockFromRaster(raster: array[16, int32]; first: int): Block =
  for pos in 0..15: result.coeffs[pos] = raster[Zigzag[pos]]
  result.first = first
  result.eob = first
  for p in countdown(15, first):
    if result.coeffs[p] != 0:
      result.eob = p + 1
      break

proc hasCoeffs(b: Block): bool = b.eob > b.first

proc treePath(value: uint8; start: int): seq[(int, bool)] =
  var path: seq[(int, bool)]
  discard findPath(CoeffTree, start, value, path)
  for (node, bit) in path: result.add (node shr 1, bit)

type
  EventKind = enum evNode, evExtra, evSign
  Event = object
    case kind: EventKind
    of evNode:
      ty, band, ctx, node: int
      bit: bool
    of evExtra:
      cat: int
      bits: uint32
    of evSign: negative: bool

iterator tokenEvents(blk: Block; ty, ctx0: int): Event =
  var ctx = ctx0
  var prevZero = false
  var pos = blk.first
  while pos < blk.eob:
    let v = blk.coeffs[pos]
    let (token, cat, bits) = tokenOf(v)
    let band = CoeffBands[pos]
    for (node, bit) in treePath(token, if prevZero: 2 else: 0):
      yield Event(kind: evNode, ty: ty, band: band, ctx: ctx, node: node, bit: bit)
    if cat >= 0: yield Event(kind: evExtra, cat: cat, bits: bits)
    if v != 0: yield Event(kind: evSign, negative: v < 0)
    ctx = if v == 0: 0 elif abs(v) == 1: 1 else: 2
    prevZero = v == 0
    inc pos
  if blk.eob < 16:
    let band = CoeffBands[blk.eob]
    for (node, bit) in treePath(DctEob, 0):
      yield Event(kind: evNode, ty: ty, band: band, ctx: ctx, node: node, bit: bit)

proc srcY(e: Encoder; x, y: int): int32 {.inline.} =
  let w = e.src.width
  let h = e.src.height
  int32(e.src.y[min(y, h - 1) * w + min(x, w - 1)])

proc srcC(e: Encoder; plane: seq[uint8]; x, y: int): int32 {.inline.} =
  let w = (e.src.width + 1) div 2
  let h = (e.src.height + 1) div 2
  int32(plane[min(y, h - 1) * w + min(x, w - 1)])

proc codeBlock(residual: array[16, int32]; dcF, acF: int32; withDc: bool): (array[16, int32], array[16, int32]) =
  let coeffs = fdct(residual)
  for i in 0..15:
    let factor = if i == 0: dcF else: acF
    if i == 0 and not withDc: continue
    result[0][i] = quantizeCoeff(coeffs[i], factor, i == 0)
    result[1][i] = result[0][i] * factor

proc sse(pred: openArray[uint8]; src: openArray[int32]): uint64 =
  for i in 0 ..< pred.len:
    let d = int64(pred[i]) - int64(src[i])
    result += uint64(d * d)

proc encodeMacroblock(e: var Encoder; mbx, mby: int) =
  let x0 = mbx * 16
  let y0 = mby * 16
  var src: array[256, int32]
  for r in 0..15:
    for c in 0..15: src[r * 16 + c] = e.srcY(x0 + c, y0 + r)
  var pred16, bestPred: array[256, uint8]
  var bestMode = DcPred
  var bestSse = high(uint64)
  for mode in [DcPred, VPred, HPred, TmPred]:
    predictBlock(e.recY, x0, y0, 16, mode, pred16)
    let s = sse(pred16, src)
    if s < bestSse:
      bestSse = s
      bestMode = mode
      bestPred = pred16
  let lambda = uint64(uint32(e.q.yAc * e.q.yAc)) div 8
  let wholeCost = bestSse + 3 * lambda
  let savedY = e.recY.data
  var bModes: array[16, uint8]
  var bBlocks: seq[Block]
  var bSse = 0'u64
  for b in 0..15:
    let bx = x0 + (b and 3) * 4
    let by = y0 + (b shr 2) * 4
    let edges = gatherEdges(e.recY, x0, y0, b, e.mbCols)
    var subSrc: array[16, int32]
    for r in 0..3:
      for c in 0..3: subSrc[r * 4 + c] = src[((b shr 2) * 4 + r) * 16 + (b and 3) * 4 + c]
    var best = (high(uint64), BDcPred, default(array[16, uint8]))
    var pred: array[16, uint8]
    for mode in 0'u8 .. 9'u8:
      predictSub(edges, mode, pred)
      let s = sse(pred, subSrc)
      if s < best[0]: best = (s, mode, pred)
    bModes[b] = best[1]
    bSse += best[0]
    var residual: array[16, int32]
    for i in 0..15: residual[i] = subSrc[i] - int32(best[2][i])
    let (quantized, dequantized) = codeBlock(residual, e.q.yDc, e.q.yAc, true)
    let recon = idct(dequantized)
    for r in 0..3:
      for c in 0..3: e.recY.set(bx + c, by + r, clamp255(int32(best[2][r * 4 + c]) + recon[r * 4 + c]))
    bBlocks.add blockFromRaster(quantized, 0)
  let bCost = bSse + 50 * lambda
  var mb = Macroblock(bModes: bModes)
  if bCost < wholeCost:
    mb.yMode = BPred
    mb.y = bBlocks
  else:
    e.recY.data = savedY
    var dcs: array[16, int32]
    var acBlocks, deqBlocks: seq[array[16, int32]]
    for b in 0..15:
      var residual: array[16, int32]
      for r in 0..3:
        for c in 0..3:
          let i = ((b shr 2) * 4 + r) * 16 + (b and 3) * 4 + c
          residual[r * 4 + c] = src[i] - int32(bestPred[i])
      let coeffs = fdct(residual)
      dcs[b] = coeffs[0]
      var quantized, dequantized: array[16, int32]
      for i in 1..15:
        quantized[i] = quantizeCoeff(coeffs[i], e.q.yAc, false)
        dequantized[i] = quantized[i] * e.q.yAc
      acBlocks.add quantized
      deqBlocks.add dequantized
    let y2c = fwht(dcs)
    var y2q, y2deq: array[16, int32]
    for i in 0..15:
      let factor = if i == 0: e.q.y2Dc else: e.q.y2Ac
      y2q[i] = quantizeCoeff(y2c[i], factor, i == 0)
      y2deq[i] = y2q[i] * factor
    let dcRec = iwht(y2deq)
    for b in 0..15:
      deqBlocks[b][0] = dcRec[b]
      let recon = idct(deqBlocks[b])
      let bx = x0 + (b and 3) * 4
      let by = y0 + (b shr 2) * 4
      for r in 0..3:
        for c in 0..3:
          let i = ((b shr 2) * 4 + r) * 16 + (b and 3) * 4 + c
          e.recY.set(bx + c, by + r, clamp255(int32(bestPred[i]) + recon[r * 4 + c]))
    for q in acBlocks: mb.y.add blockFromRaster(q, 1)
    mb.yMode = bestMode
    mb.hasY2 = true
    mb.y2 = blockFromRaster(y2q, 0)
  # chroma
  let cx0 = mbx * 8
  let cy0 = mby * 8
  var srcU, srcV: array[64, int32]
  for r in 0..7:
    for c in 0..7:
      srcU[r * 8 + c] = e.srcC(e.src.u, cx0 + c, cy0 + r)
      srcV[r * 8 + c] = e.srcC(e.src.v, cx0 + c, cy0 + r)
  var uvMode = DcPred
  var uvBest = high(uint64)
  var predU, predV, pu, pv: array[64, uint8]
  for mode in [DcPred, VPred, HPred, TmPred]:
    predictBlock(e.recU, cx0, cy0, 8, mode, pu)
    predictBlock(e.recV, cx0, cy0, 8, mode, pv)
    let s = sse(pu, srcU) + sse(pv, srcV)
    if s < uvBest:
      uvBest = s
      uvMode = mode
      predU = pu
      predV = pv
  mb.uvMode = uvMode
  for isU in [true, false]:
    for b in 0..3:
      var residual: array[16, int32]
      for r in 0..3:
        for c in 0..3:
          let i = ((b shr 1) * 4 + r) * 8 + (b and 1) * 4 + c
          residual[r * 4 + c] = (if isU: srcU[i] - int32(predU[i]) else: srcV[i] - int32(predV[i]))
      let (quantized, dequantized) = codeBlock(residual, e.q.uvDc, e.q.uvAc, true)
      let recon = idct(dequantized)
      let bx = cx0 + (b and 1) * 4
      let by = cy0 + (b shr 1) * 4
      for r in 0..3:
        for c in 0..3:
          let i = ((b shr 1) * 4 + r) * 8 + (b and 1) * 4 + c
          if isU: e.recU.set(bx + c, by + r, clamp255(int32(predU[i]) + recon[r * 4 + c]))
          else: e.recV.set(bx + c, by + r, clamp255(int32(predV[i]) + recon[r * 4 + c]))
      if isU: mb.u.add blockFromRaster(quantized, 0) else: mb.v.add blockFromRaster(quantized, 0)
  var skip = not (mb.hasY2 and mb.y2.hasCoeffs)
  for b in mb.y:
    if b.hasCoeffs: skip = false
  for b in mb.u:
    if b.hasCoeffs: skip = false
  for b in mb.v:
    if b.hasCoeffs: skip = false
  mb.skip = skip
  e.mbs.add mb

iterator walkBlocks(e: Encoder): (ptr Block, int, int) =
  ## Every coded block with its type and non-zero context, in coding order.
  var above = newSeq[array[9, uint8]](e.mbCols)
  for mby in 0 ..< e.mbRows:
    var left: array[9, uint8]
    for mbx in 0 ..< e.mbCols:
      let mb = unsafeAddr e.mbs[mby * e.mbCols + mbx]
      if mb.skip:
        for i in 0..7:
          left[i] = 0
          above[mbx][i] = 0
        if mb.yMode != BPred:
          left[8] = 0
          above[mbx][8] = 0
        continue
      if mb.hasY2:
        let ctx = int(left[8] + above[mbx][8])
        yield (unsafeAddr mb.y2, TypeY2, ctx)
        let t = uint8(mb.y2.hasCoeffs)
        left[8] = t
        above[mbx][8] = t
      let ty = if mb.hasY2: TypeYAfterY2 else: TypeYWithDc
      for i in 0 ..< mb.y.len:
        let ai = i and 3
        let li = i shr 2
        yield (unsafeAddr mb.y[i], ty, int(left[li] + above[mbx][ai]))
        let t = uint8(mb.y[i].hasCoeffs)
        left[li] = t
        above[mbx][ai] = t
      for (base, isU) in [(4, true), (6, false)]:
        for i in 0..3:
          let ai = base + (i and 1)
          let li = base + (i shr 1)
          let bp = if isU: unsafeAddr mb.u[i] else: unsafeAddr mb.v[i]
          yield (bp, TypeUv, int(left[li] + above[mbx][ai]))
          let t = uint8(bp[].hasCoeffs)
          left[li] = t
          above[mbx][ai] = t

proc chooseProbs(stats: NodeStats): Probs =
  result = DefaultCoeffProbs
  for t in 0..3:
    for b in 0..7:
      for c in 0..2:
        for n in 0..10:
          let zeros = stats[t][b][c][n][0]
          let ones = stats[t][b][c][n][1]
          let total = zeros + ones
          if total < 24: continue
          let est = uint8(clamp((uint64(zeros) * 256 + uint64(total) div 2) div uint64(total), 1'u64, 255'u64))
          proc cost(pr: uint8): float64 =
            let p0 = float64(pr) / 256.0
            -(float64(zeros) * log2(p0) + float64(ones) * log2(1.0 - p0))
          if cost(result[t][b][c][n]) - cost(est) > 18.0: result[t][b][c][n] = est

proc subModeOf(mb: Macroblock; b: int): uint8 =
  case mb.yMode
  of BPred: mb.bModes[min(b, 15)]
  of VPred: BVePred
  of HPred: BHePred
  of TmPred: BTmPred
  else: BDcPred

proc writeFrame(e: Encoder; probs: Probs; lfLevel, qi: int): seq[byte] =
  var h = newBoolEncoder()
  h.putFlag(false); h.putFlag(false); h.putFlag(false); h.putFlag(false)
  h.putLiteral(6, uint32(min(lfLevel, 63)))
  h.putLiteral(3, 0)
  h.putFlag(false)
  h.putLiteral(2, 0)
  h.putLiteral(7, uint32(min(qi, 127)))
  for _ in 0..4: h.putFlag(false)
  h.putFlag(true)
  for t in 0..3:
    for b in 0..7:
      for c in 0..2:
        for n in 0..10:
          let changed = probs[t][b][c][n] != DefaultCoeffProbs[t][b][c][n]
          h.put(CoeffUpdateProbs[t][b][c][n], changed)
          if changed: h.putLiteral(8, uint32(probs[t][b][c][n]))
  h.putFlag(true)
  var skipped = 0'u64
  for m in e.mbs:
    if m.skip: inc skipped
  let total = uint64(max(e.mbs.len, 1))
  let probSkipFalse = uint8(clamp(((total - skipped) * 256 + total div 2) div total, 1'u64, 255'u64))
  h.putLiteral(8, uint32(probSkipFalse))
  for mby in 0 ..< e.mbRows:
    for mbx in 0 ..< e.mbCols:
      let mb = e.mbs[mby * e.mbCols + mbx]
      h.put(probSkipFalse, mb.skip)
      h.putTree(KfYmodeTree, KfYmodeProb, mb.yMode)
      if mb.yMode == BPred:
        for b in 0..15:
          let above = if b >= 4: mb.bModes[b - 4] elif mby == 0: BDcPred else: subModeOf(e.mbs[(mby - 1) * e.mbCols + mbx], b + 12)
          let left = if (b and 3) != 0: mb.bModes[b - 1] elif mbx == 0: BDcPred else: subModeOf(e.mbs[mby * e.mbCols + mbx - 1], b + 3)
          h.putTree(BmodeTree, KfBmodeProbs[above][left], mb.bModes[b])
      h.putTree(UvModeTree, KfUvModeProb, mb.uvMode)
  let first = h.finish
  var t = newBoolEncoder()
  for (bp, ty, ctx0) in e.walkBlocks:
    for ev in tokenEvents(bp[], ty, ctx0):
      case ev.kind
      of evNode: t.put(probs[ev.ty][ev.band][ev.ctx][ev.node], ev.bit)
      of evExtra:
        let p = Pcat[ev.cat]
        for i, prob in p:
          let shift = p.len - 1 - i
          t.put(prob, ((ev.bits shr shift) and 1) != 0)
      of evSign: t.putFlag(ev.negative)
  let tokens = t.finish
  let size = uint32(min(first.len, (1 shl 19) - 1))
  let tag = (1'u32 shl 4) or (size shl 5)
  result.add [byte(tag and 0xFF), byte((tag shr 8) and 0xFF), byte((tag shr 16) and 0xFF)]
  result.add [0x9d'u8, 0x01, 0x2a]
  result.add [byte(e.src.width and 0xFF), byte(e.src.width shr 8), byte(e.src.height and 0xFF), byte(e.src.height shr 8)]
  result.add first
  result.add tokens

proc encodeFrame*(yuv: Yuv; quality: int): seq[byte] =
  ## Encodes `yuv` as one VP8 key frame (the payload of a WebP `VP8 ` chunk).
  if yuv.width <= 0 or yuv.height <= 0 or yuv.width > MaxDimension or yuv.height > MaxDimension:
    raise vp8err("VP8 frames are 1 to " & $MaxDimension & " pixels per side")
  let w = yuv.width
  let h = yuv.height
  if yuv.y.len != w * h or yuv.u.len != ((w + 1) div 2) * ((h + 1) div 2) or yuv.v.len != yuv.u.len:
    raise vp8err("plane sizes do not match the dimensions")
  let qi = quantizerIndex(quality)
  var e = Encoder(src: unsafeAddr yuv, mbCols: (w + 15) div 16, mbRows: (h + 15) div 16, q: newQuant(qi))
  e.recY = newPlane(e.mbCols * 16, e.mbRows * 16)
  e.recU = newPlane(e.mbCols * 8, e.mbRows * 8)
  e.recV = newPlane(e.mbCols * 8, e.mbRows * 8)
  for mby in 0 ..< e.mbRows:
    for mbx in 0 ..< e.mbCols: e.encodeMacroblock(mbx, mby)
  var stats: NodeStats = newSeq[array[8, array[3, array[11, array[2, uint32]]]]](4)
  for (bp, ty, ctx0) in e.walkBlocks:
    for ev in tokenEvents(bp[], ty, ctx0):
      if ev.kind == evNode: inc stats[ev.ty][ev.band][ev.ctx][ev.node][if ev.bit: 1 else: 0]
  let probs = chooseProbs(stats)
  let lf = if qi == 0: 0 else: min(qi div 2 + 4, 63)
  e.writeFrame(probs, lf, qi)

# ---------------------------------------------------------------- decoder

type
  DecodedFrame* = object
    width*, height*: int
    y*, u*, v*: Plane   ## padded to whole macroblocks

  MbInfo = object
    yMode: uint8
    bModes: array[16, uint8]
    uvMode: uint8
    segment: int
    skip: bool
    hasCoeffs: bool

proc decodeFrame*(data: openArray[byte]): DecodedFrame =
  ## Decodes a VP8 key frame. Raises `Vp8Error`.
  if data.len < 10: raise vp8err("VP8 frame too short")
  let bp = cast[ptr UncheckedArray[byte]](unsafeAddr data[0])
  let tag = uint32(data[0]) or (uint32(data[1]) shl 8) or (uint32(data[2]) shl 16)
  if (tag and 1) != 0: raise vp8err("not a key frame")
  let firstSize = int(tag shr 5)
  if data[3] != 0x9d or data[4] != 0x01 or data[5] != 0x2a: raise vp8err("bad start code")
  let w = (int(data[6]) or (int(data[7]) shl 8)) and 0x3FFF
  let h = (int(data[8]) or (int(data[9]) shl 8)) and 0x3FFF
  if w == 0 or h == 0: raise vp8err("zero-sized frame")
  if 10 + firstSize > data.len: raise vp8err("first partition is truncated")
  var d = initBoolDecoder(bp, 10, firstSize)
  discard d.readBool(128) # colour space
  discard d.readBool(128) # clamping type
  let segEnabled = d.readBool(128)
  var updateMap = false
  var segAbs = false
  var segQ, segLf: array[4, int]
  var segProbs = [255'u8, 255, 255]
  if segEnabled:
    updateMap = d.readBool(128)
    let updateData = d.readBool(128)
    if updateData:
      segAbs = d.readBool(128)
      for i in 0..3: segQ[i] = d.readSigned(7)
      for i in 0..3: segLf[i] = d.readSigned(6)
    if updateMap:
      for i in 0..2: segProbs[i] = if d.readBool(128): uint8(d.readLiteral(8)) else: 255'u8
  let filterType = if d.readBool(128): 1 else: 0  # 1 = simple
  let filterLevel = d.readLiteral(6)
  let sharpness = d.readLiteral(3)
  var refDelta, modeDelta: array[4, int]
  let lfAdj = d.readBool(128)
  if lfAdj:
    if d.readBool(128):
      for i in 0..3: refDelta[i] = d.readSigned(6)
      for i in 0..3: modeDelta[i] = d.readSigned(6)
  let nParts = 1 shl d.readLiteral(2)
  let qBase = d.readLiteral(7)
  let dqYdc = d.readSigned(4)
  let dqY2dc = d.readSigned(4)
  let dqY2ac = d.readSigned(4)
  let dqUVdc = d.readSigned(4)
  let dqUVac = d.readSigned(4)
  discard d.readBool(128) # refresh entropy probs
  var probs: Probs = DefaultCoeffProbs
  for t in 0..3:
    for b in 0..7:
      for c in 0..2:
        for n in 0..10:
          if d.readBool(int(CoeffUpdateProbs[t][b][c][n])): probs[t][b][c][n] = uint8(d.readLiteral(8))
  let useSkip = d.readBool(128)
  let probSkip = if useSkip: d.readLiteral(8) else: 0
  # token partitions
  var partStart = 10 + firstSize
  let sizesLen = 3 * (nParts - 1)
  if partStart + sizesLen > data.len: raise vp8err("partition table is truncated")
  var parts: seq[BoolDecoder]
  var p = partStart + sizesLen
  for i in 0 ..< nParts:
    var size: int
    if i < nParts - 1:
      let o = partStart + 3 * i
      size = int(data[o]) or (int(data[o + 1]) shl 8) or (int(data[o + 2]) shl 16)
    else: size = data.len - p
    size = max(0, min(size, data.len - p))
    parts.add initBoolDecoder(bp, p, size)
    p += size
  # dequantization per segment
  var dq: array[4, (int32, int32, int32, int32, int32, int32)]
  for s in 0..3:
    var q = qBase
    if segEnabled: q = if segAbs: segQ[s] else: qBase + segQ[s]
    proc lk(tab: array[128, int32]; i: int): int32 = tab[clamp(i, 0, 127)]
    let y2ac = max(lk(AcQLookup, q + dqY2ac) * 155 div 100, 8'i32)
    dq[s] = (lk(DcQLookup, q + dqYdc), lk(AcQLookup, q), lk(DcQLookup, q + dqY2dc) * 2, y2ac,
             min(lk(DcQLookup, q + dqUVdc), 132'i32), lk(AcQLookup, q + dqUVac))
  let mbCols = (w + 15) div 16
  let mbRows = (h + 15) div 16
  var fr = DecodedFrame(width: w, height: h, y: newPlane(mbCols * 16, mbRows * 16),
                        u: newPlane(mbCols * 8, mbRows * 8), v: newPlane(mbCols * 8, mbRows * 8))
  var infos = newSeq[MbInfo](mbCols * mbRows)
  var aboveNz = newSeq[array[9, uint8]](mbCols)
  var aboveModes = newSeq[array[4, uint8]](mbCols)
  # coefficient reader
  proc readCoeffs(t: var BoolDecoder; ty, ctx, first: int; dcF, acF: int32; outp: var array[16, int32]): int =
    var n = first
    var pr = addr probs[ty][CoeffBands[n]][ctx]
    while n < 16:
      if not t.readBool(int(pr[][0])): return n
      while not t.readBool(int(pr[][1])):
        inc n
        if n == 16: return 16
        pr = addr probs[ty][CoeffBands[n]][0]
      var v: int
      var nextCtx: int
      if not t.readBool(int(pr[][2])):
        v = 1
        nextCtx = 1
      else:
        if not t.readBool(int(pr[][3])):
          if not t.readBool(int(pr[][4])): v = 2
          else: v = 3 + (if t.readBool(int(pr[][5])): 1 else: 0)
        else:
          if not t.readBool(int(pr[][6])):
            if not t.readBool(int(pr[][7])): v = 5 + (if t.readBool(159): 1 else: 0)
            else:
              v = 7 + 2 * (if t.readBool(165): 1 else: 0)
              v += (if t.readBool(145): 1 else: 0)
          else:
            let bit1 = if t.readBool(int(pr[][8])): 1 else: 0
            let bit0 = if t.readBool(int(pr[][9 + bit1])): 1 else: 0
            let cat = 2 * bit1 + bit0
            v = 0
            for prob in Pcat[2 + cat]: v = v + v + (if t.readBool(int(prob)): 1 else: 0)
            v += 3 + (8 shl cat)
        nextCtx = 2
      let signed = if t.readBool(128): -v else: v
      outp[Zigzag[n]] = int32(signed) * (if n > 0: acF else: dcF)
      inc n
      if n < 16: pr = addr probs[ty][CoeffBands[n]][nextCtx]
    16
  var predBuf: array[256, uint8]
  for mby in 0 ..< mbRows:
    var leftNz: array[9, uint8]
    var leftModes: array[4, uint8]
    var t = addr parts[mby mod nParts]
    for mbx in 0 ..< mbCols:
      var info: MbInfo
      if updateMap: info.segment = d.readTree(MbSegmentTree, segProbs)
      info.skip = if useSkip: d.readBool(probSkip) else: false
      info.yMode = uint8(d.readTree(KfYmodeTree, KfYmodeProb))
      if info.yMode == BPred:
        for b in 0..15:
          let above = if b >= 4: info.bModes[b - 4] else: aboveModes[mbx][b]
          let left = if (b and 3) != 0: info.bModes[b - 1] else: leftModes[b shr 2]
          info.bModes[b] = uint8(d.readTree(BmodeTree, KfBmodeProbs[above][left]))
      else:
        let m = case info.yMode
          of VPred: BVePred
          of HPred: BHePred
          of TmPred: BTmPred
          else: BDcPred
        for b in 0..15: info.bModes[b] = m
      for i in 0..3:
        aboveModes[mbx][i] = info.bModes[12 + i]
        leftModes[i] = info.bModes[i * 4 + 3]
      info.uvMode = uint8(d.readTree(UvModeTree, KfUvModeProb))
      let (yDc, yAc, y2Dc, y2Ac, uvDc, uvAc) = dq[info.segment]
      # residuals
      var coeffs: array[25, array[16, int32]]
      var nonZero = false
      if not info.skip:
        var first = 0
        var ty = TypeYWithDc
        if info.yMode != BPred:
          var y2: array[16, int32]
          let ctx = int(leftNz[8] + aboveNz[mbx][8])
          let n = readCoeffs(t[], TypeY2, ctx, 0, y2Dc, y2Ac, y2)
          let nz = uint8(n > 0)
          leftNz[8] = nz
          aboveNz[mbx][8] = nz
          if n > 0: nonZero = true
          let dc = iwht(y2)
          for b in 0..15: coeffs[b][0] = dc[b]
          first = 1
          ty = TypeYAfterY2
        for b in 0..15:
          let ai = b and 3
          let li = b shr 2
          let ctx = int(leftNz[li] + aboveNz[mbx][ai])
          let n = readCoeffs(t[], ty, ctx, first, yDc, yAc, coeffs[b])
          let nz = uint8(n > first)
          leftNz[li] = nz
          aboveNz[mbx][ai] = nz
          if n > first: nonZero = true
        for (base, off) in [(4, 16), (6, 20)]:
          for i in 0..3:
            let ai = base + (i and 1)
            let li = base + (i shr 1)
            let ctx = int(leftNz[li] + aboveNz[mbx][ai])
            let n = readCoeffs(t[], TypeUv, ctx, 0, uvDc, uvAc, coeffs[off + i])
            let nz = uint8(n > 0)
            leftNz[li] = nz
            aboveNz[mbx][ai] = nz
            if n > 0: nonZero = true
      else:
        for i in 0..7:
          leftNz[i] = 0
          aboveNz[mbx][i] = 0
        if info.yMode != BPred:
          leftNz[8] = 0
          aboveNz[mbx][8] = 0
      info.hasCoeffs = nonZero
      # reconstruction
      let x0 = mbx * 16
      let y0 = mby * 16
      if info.yMode == BPred:
        var pred: array[16, uint8]
        for b in 0..15:
          let edges = gatherEdges(fr.y, x0, y0, b, mbCols)
          predictSub(edges, info.bModes[b], pred)
          let recon = idct(coeffs[b])
          let bx = x0 + (b and 3) * 4
          let by = y0 + (b shr 2) * 4
          for r in 0..3:
            for c in 0..3: fr.y.set(bx + c, by + r, clamp255(int32(pred[r * 4 + c]) + recon[r * 4 + c]))
      else:
        predictBlock(fr.y, x0, y0, 16, info.yMode, predBuf)
        for b in 0..15:
          let recon = idct(coeffs[b])
          let bx = (b and 3) * 4
          let by = (b shr 2) * 4
          for r in 0..3:
            for c in 0..3:
              fr.y.set(x0 + bx + c, y0 + by + r, clamp255(int32(predBuf[(by + r) * 16 + bx + c]) + recon[r * 4 + c]))
      let cx0 = mbx * 8
      let cy0 = mby * 8
      for (plane, off) in [(addr fr.u, 16), (addr fr.v, 20)]:
        var cpred: array[64, uint8]
        predictBlock(plane[], cx0, cy0, 8, info.uvMode, cpred)
        for b in 0..3:
          let recon = idct(coeffs[off + b])
          let bx = (b and 1) * 4
          let by = (b shr 1) * 4
          for r in 0..3:
            for c in 0..3:
              plane[].set(cx0 + bx + c, cy0 + by + r, clamp255(int32(cpred[(by + r) * 8 + bx + c]) + recon[r * 4 + c]))
      infos[mby * mbCols + mbx] = info
  # loop filter
  if filterLevel > 0 or segEnabled:
    proc s2u(v: int): uint8 {.inline.} = uint8(clamp(v, -128, 127) + 128)
    proc u2s(v: uint8): int {.inline.} = int(v) - 128
    proc c8(v: int): int {.inline.} = clamp(v, -128, 127)
    proc commonAdjust(pl: var Plane; useOuter: bool; i0, stride: int): int =
      let p1 = u2s(pl.data[i0 - 2 * stride])
      let p0 = u2s(pl.data[i0 - stride])
      let q0 = u2s(pl.data[i0])
      let q1 = u2s(pl.data[i0 + stride])
      var a = c8((if useOuter: c8(p1 - q1) else: 0) + 3 * (q0 - p0))
      let b = c8(a + 3) shr 3
      a = c8(a + 4) shr 3
      pl.data[i0] = s2u(q0 - a)
      pl.data[i0 - stride] = s2u(p0 + b)
      a
    proc simpleOk(pl: Plane; i0, stride, edge: int): bool =
      let p1 = int(pl.data[i0 - 2 * stride])
      let p0 = int(pl.data[i0 - stride])
      let q0 = int(pl.data[i0])
      let q1 = int(pl.data[i0 + stride])
      abs(p0 - q0) * 2 + abs(p1 - q1) div 2 <= edge
    proc normalOk(pl: Plane; i0, stride, edge, interior: int): bool =
      if not simpleOk(pl, i0, stride, edge): return false
      let p3 = int(pl.data[i0 - 4 * stride]); let p2 = int(pl.data[i0 - 3 * stride])
      let p1 = int(pl.data[i0 - 2 * stride]); let p0 = int(pl.data[i0 - stride])
      let q0 = int(pl.data[i0]); let q1 = int(pl.data[i0 + stride])
      let q2 = int(pl.data[i0 + 2 * stride]); let q3 = int(pl.data[i0 + 3 * stride])
      abs(p3 - p2) <= interior and abs(p2 - p1) <= interior and abs(p1 - p0) <= interior and
        abs(q3 - q2) <= interior and abs(q2 - q1) <= interior and abs(q1 - q0) <= interior
    proc hev(pl: Plane; i0, stride, thresh: int): bool =
      let p1 = int(pl.data[i0 - 2 * stride]); let p0 = int(pl.data[i0 - stride])
      let q0 = int(pl.data[i0]); let q1 = int(pl.data[i0 + stride])
      abs(p1 - p0) > thresh or abs(q1 - q0) > thresh
    proc mbFilter(pl: var Plane; i0, stride: int) =
      let p2 = u2s(pl.data[i0 - 3 * stride]); let p1 = u2s(pl.data[i0 - 2 * stride])
      let p0 = u2s(pl.data[i0 - stride]); let q0 = u2s(pl.data[i0])
      let q1 = u2s(pl.data[i0 + stride]); let q2 = u2s(pl.data[i0 + 2 * stride])
      let w = c8(c8(p1 - q1) + 3 * (q0 - p0))
      var a = c8((27 * w + 63) shr 7)
      pl.data[i0] = s2u(q0 - a); pl.data[i0 - stride] = s2u(p0 + a)
      a = c8((18 * w + 63) shr 7)
      pl.data[i0 + stride] = s2u(q1 - a); pl.data[i0 - 2 * stride] = s2u(p1 + a)
      a = c8((9 * w + 63) shr 7)
      pl.data[i0 + 2 * stride] = s2u(q2 - a); pl.data[i0 - 3 * stride] = s2u(p2 + a)
    proc edgeFilter(pl: var Plane; i0, step, stride, count: int; mbEdge: bool; edge, interior, hevT: int; simple: bool) =
      ## `count` positions along the edge (`step` apart), filtering across it (`stride`).
      for k in 0 ..< count:
        let i = i0 + k * step
        if simple:
          if simpleOk(pl, i, stride, edge): discard commonAdjust(pl, true, i, stride)
        elif normalOk(pl, i, stride, edge, interior):
          if hev(pl, i, stride, hevT):
            discard commonAdjust(pl, true, i, stride)
          elif mbEdge:
            mbFilter(pl, i, stride)
          else:
            var a = commonAdjust(pl, false, i, stride)
            a = (a + 1) shr 1
            let q1 = u2s(pl.data[i + stride])
            let p1 = u2s(pl.data[i - 2 * stride])
            pl.data[i + stride] = s2u(q1 - a)
            pl.data[i - 2 * stride] = s2u(p1 + a)
    for mby in 0 ..< mbRows:
      for mbx in 0 ..< mbCols:
        let info = infos[mby * mbCols + mbx]
        var level = filterLevel
        if segEnabled: level = if segAbs: segLf[info.segment] else: filterLevel + segLf[info.segment]
        level = clamp(level, 0, 63)
        if lfAdj:
          level += refDelta[0]
          if info.yMode == BPred: level += modeDelta[0]
          level = clamp(level, 0, 63)
        if level == 0: continue
        var interior = level
        if sharpness > 0:
          interior = interior shr (if sharpness > 4: 2 else: 1)
          interior = min(interior, 9 - sharpness)
        interior = max(interior, 1)
        let hevT = if level >= 40: 2 elif level >= 15: 1 else: 0
        let mbEdgeLim = (level + 2) * 2 + interior
        let subEdgeLim = level * 2 + interior
        let inner = info.yMode == BPred or info.hasCoeffs
        let simple = filterType == 1
        let ys = fr.y.width
        let x0 = mbx * 16
        let y0 = mby * 16
        if mbx > 0: edgeFilter(fr.y, y0 * ys + x0, ys, 1, 16, true, mbEdgeLim, interior, hevT, simple)
        if inner:
          for xx in [4, 8, 12]: edgeFilter(fr.y, y0 * ys + x0 + xx, ys, 1, 16, false, subEdgeLim, interior, hevT, simple)
        if mby > 0: edgeFilter(fr.y, y0 * ys + x0, 1, ys, 16, true, mbEdgeLim, interior, hevT, simple)
        if inner:
          for yy in [4, 8, 12]: edgeFilter(fr.y, (y0 + yy) * ys + x0, 1, ys, 16, false, subEdgeLim, interior, hevT, simple)
        if not simple:
          let cs = fr.u.width
          let cx0 = mbx * 8
          let cy0 = mby * 8
          for pl in [addr fr.u, addr fr.v]:
            if mbx > 0: edgeFilter(pl[], cy0 * cs + cx0, cs, 1, 8, true, mbEdgeLim, interior, hevT, false)
            if inner: edgeFilter(pl[], cy0 * cs + cx0 + 4, cs, 1, 8, false, subEdgeLim, interior, hevT, false)
            if mby > 0: edgeFilter(pl[], cy0 * cs + cx0, 1, cs, 8, true, mbEdgeLim, interior, hevT, false)
            if inner: edgeFilter(pl[], (cy0 + 4) * cs + cx0, 1, cs, 8, false, subEdgeLim, interior, hevT, false)
  fr

proc yuvToRgb*(fr: DecodedFrame; outp0: var openArray[uint8]; stride: int) =
  ## Converts to interleaved RGB(A) with libwebp-style "fancy" chroma upsampling (alpha untouched).
  if outp0.len < fr.width * fr.height * stride: raise vp8err("output buffer too small")
  let op = cast[ptr UncheckedArray[uint8]](addr outp0[0])
  let olen = outp0.len
  template outp: untyped = toOpenArray(op, 0, olen - 1)
  let w = fr.width
  let h = fr.height
  proc mulHi(v, c: int): int {.inline.} = (v * c) shr 8
  proc clip8(v: int): uint8 {.inline.} =
    if (v and not 16383) == 0: uint8(v shr 6) elif v < 0: 0'u8 else: 255'u8
  proc put(x, y: int; yy, u, v: int) =
    let o = (y * w + x) * stride
    outp[o] = clip8(mulHi(yy, 19077) + mulHi(v, 26149) - 14234)
    outp[o + 1] = clip8(mulHi(yy, 19077) - mulHi(u, 6419) - mulHi(v, 13320) + 8708)
    outp[o + 2] = clip8(mulHi(yy, 19077) + mulHi(u, 33050) - 17685)
  let cw = fr.u.width
  proc uvAt(row, x: int): uint32 = uint32(fr.u.data[row * cw + x]) or (uint32(fr.v.data[row * cw + x]) shl 16)
  proc lineP(topY: int; botY: int; topRow, curRow: int) =
    let lastPair = (w - 1) shr 1
    var tl = uvAt(topRow, 0)
    var l = uvAt(curRow, 0)
    block:
      let uv0 = (3'u32 * tl + l + 0x00020002'u32) shr 2
      put(0, topY, int(fr.y.data[topY * fr.y.width]), int(uv0 and 0xff), int((uv0 shr 16) and 0xff))
    if botY >= 0:
      let uv0 = (3'u32 * l + tl + 0x00020002'u32) shr 2
      put(0, botY, int(fr.y.data[botY * fr.y.width]), int(uv0 and 0xff), int((uv0 shr 16) and 0xff))
    for x in 1 .. lastPair:
      let t = uvAt(topRow, x)
      let uv = uvAt(curRow, x)
      let avg = tl + t + l + uv + 0x00080008'u32
      let diag12 = (avg + 2'u32 * (t + l)) shr 3
      let diag03 = (avg + 2'u32 * (tl + uv)) shr 3
      block:
        let uv0 = (diag12 + tl) shr 1
        let uv1 = (diag03 + t) shr 1
        put(2 * x - 1, topY, int(fr.y.data[topY * fr.y.width + 2 * x - 1]), int(uv0 and 0xff), int((uv0 shr 16) and 0xff))
        put(2 * x, topY, int(fr.y.data[topY * fr.y.width + 2 * x]), int(uv1 and 0xff), int((uv1 shr 16) and 0xff))
      if botY >= 0:
        let uv0 = (diag03 + l) shr 1
        let uv1 = (diag12 + uv) shr 1
        put(2 * x - 1, botY, int(fr.y.data[botY * fr.y.width + 2 * x - 1]), int(uv0 and 0xff), int((uv0 shr 16) and 0xff))
        put(2 * x, botY, int(fr.y.data[botY * fr.y.width + 2 * x]), int(uv1 and 0xff), int((uv1 shr 16) and 0xff))
      tl = t
      l = uv
    if (w and 1) == 0:
      block:
        let uv0 = (3'u32 * tl + l + 0x00020002'u32) shr 2
        put(w - 1, topY, int(fr.y.data[topY * fr.y.width + w - 1]), int(uv0 and 0xff), int((uv0 shr 16) and 0xff))
      if botY >= 0:
        let uv0 = (3'u32 * l + tl + 0x00020002'u32) shr 2
        put(w - 1, botY, int(fr.y.data[botY * fr.y.width + w - 1]), int(uv0 and 0xff), int((uv0 shr 16) and 0xff))
  lineP(0, -1, 0, 0)
  var y = 1
  while y + 1 < h:
    lineP(y, y + 1, (y - 1) shr 1, (y + 1) shr 1)
    y += 2
  if (h and 1) == 0 and h > 1:
    let last = (h - 1) shr 1
    lineP(h - 1, -1, last, last)
