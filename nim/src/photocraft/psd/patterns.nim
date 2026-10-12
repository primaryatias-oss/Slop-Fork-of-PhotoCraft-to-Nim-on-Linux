## Patterns: the global `Patt`/`Pat2`/`Pat3` blocks and `.pat` files (Adobe spec "Patterns" and
## "Virtual Memory Array List"). Like ag-psd (MIT), the first written arrays are the colour
## channels and the next one is transparency.

import std/options
import ./io, ./compression

const
  PatMaxEdge* = 30_000
  MaxRleExpansion = 64'u64

type PsdPattern* = object
  mode*: uint32                ## PSD colour mode number
  width*, height*: int
  name*, id*: string
  palette*: Option[seq[byte]]  ## indexed: 768 bytes
  depth*: int
  channels*: seq[seq[byte]]    ## planar big-endian colour planes
  alpha*: Option[seq[byte]]

proc modeChannels*(mode: uint32): int =
  case mode
  of 3, 9: 3
  of 4: 4
  else: 1

proc readRect4(r: var Reader): array[4, int32] =
  for k in 0..3: result[k] = r.i32()

proc placePlane(plane: seq[byte]; src: PlaneLayout; x, y: int; dst: PlaneLayout): seq[byte] =
  if x + src.width > dst.width or y + src.height > dst.height:
    raise invalid("pattern channel rectangle falls outside pattern rectangle")
  if plane.len != src.decodedLen: raise invalid("pattern channel payload does not match its rectangle")
  let dlen = dst.decodedLen
  if x == 0 and y == 0 and src.width == dst.width and src.height == dst.height:
    result = plane
    if src.depth == 1 and src.width mod 8 != 0:
      let mask = byte(0xFF shl (8 - src.width mod 8))
      let rb = src.rowBytes
      var row = rb - 1
      while row < result.len:
        result[row] = result[row] and mask
        row += rb
    return
  result = newSeq[byte](dlen)
  if src.depth == 1:
    let (srb, drb) = (src.rowBytes, dst.rowBytes)
    for row in 0 ..< src.height:
      for col in 0 ..< src.width:
        let v = (plane[row * srb + col div 8] shr (7 - col mod 8)) and 1
        let dc = x + col
        result[(y + row) * drb + dc div 8] = result[(y + row) * drb + dc div 8] or (v shl (7 - dc mod 8))
  else:
    let bps = src.depth div 8
    let (srb, drb) = (src.rowBytes, dst.rowBytes)
    let rowBytes = src.width * bps
    for row in 0 ..< src.height:
      if rowBytes > 0:
        copyMem(addr result[(y + row) * drb + x * bps], unsafeAddr plane[row * srb], rowBytes)

proc readPattern(r: var Reader): PsdPattern =
  let version = r.u32()
  if version != 1: raise invalid("pattern version " & $version)
  result.mode = r.u32()
  discard r.u16()
  discard r.u16()
  let n = int(r.u32())
  if n > 65_536: raise limitExceeded("pattern name length")
  r.checkCount(uint64(n), 2)
  var units: seq[uint16]
  for _ in 0 ..< n: units.add r.u16()
  while units.len > 0 and units[^1] == 0: units.setLen(units.len - 1)
  result.name = fromUtf16(units)
  let idl = int(r.u8())
  let idb = r.bytes(idl)
  result.id = decodeLegacyName(idb)
  if result.mode == 2:
    result.palette = some(r.bytes(768))
    if r.remaining >= 4 and not (r.at(r.pos) == 0 and r.at(r.pos + 1) == 0 and r.at(r.pos + 2) == 0 and r.at(r.pos + 3) == 3):
      r.skip(4)
  let vver = r.u32()
  if vver != 3: raise invalid("virtual memory array list version " & $vver)
  let ln = r.u32()
  var v = r.sub(uint64(ln))
  let rect = readRect4(v)
  let (top, left, bottom, right) = (int64(rect[0]), int64(rect[1]), int64(rect[2]), int64(rect[3]))
  let w = right - left
  let h = bottom - top
  if w <= 0 or h <= 0 or w > PatMaxEdge or h > PatMaxEdge: raise limitExceeded("pattern size")
  result.width = int(w)
  result.height = int(h)
  let count = v.u32()
  if count > 64: raise limitExceeded("pattern channel count")
  type Pending = object
    data: seq[byte]
    comp: Compression
    src, dst: PlaneLayout
    x, y: int
  var pending: seq[Pending]
  var depth = 8
  var decodedBytes = 0'u64
  for _ in 0'u32 ..< count + 2:
    if v.isEmpty: break
    if v.u32() == 0: continue
    let alen = int(v.u32())
    if alen == 0: continue
    if alen < 23: raise invalid("short virtual memory array")
    let d32 = v.u32()
    let cr = readRect4(v)
    let d16 = int(v.u16())
    let comp = v.u8()
    let data = v.bytes(alen - 23)
    let d = if d16 in [1, 8, 16, 32]: d16 else: int(d32)
    if d notin [1, 8, 16, 32]: raise invalid("pattern depth " & $d)
    depth = d
    if cr[0] < rect[0] or cr[1] < rect[1] or cr[2] > rect[2] or cr[3] > rect[3] or cr[2] < cr[0] or cr[3] < cr[1]:
      raise invalid("pattern channel rectangle falls outside pattern rectangle")
    let src = PlaneLayout(planes: 1, width: int(cr[3] - cr[1]), height: int(cr[2] - cr[0]), depth: d, version: vPsd)
    let dst = PlaneLayout(planes: 1, width: result.width, height: result.height, depth: d, version: vPsd)
    decodedBytes += dst.totalBytes
    if decodedBytes > MaxDecodedBytes: raise limitExceeded("pattern data exceeds MAX_DECODED_BYTES")
    discard dst.decodedLen
    let c = Compression(code: uint16(comp))
    let maxExp = if comp == 0: 1'u64 elif comp == 1: MaxRleExpansion else: 0'u64
    if maxExp > 0 and src.totalBytes > uint64(data.len) * maxExp:
      raise limitExceeded("pattern channel exceeds its input-derived size limit")
    pending.add Pending(data: data, comp: c, src: src, dst: dst, x: int(cr[1] - rect[1]), y: int(cr[0] - rect[0]))
  result.depth = depth
  let nc = modeChannels(result.mode)
  if pending.len < nc: raise invalid("pattern has fewer channels than its colour mode")
  var planes: seq[seq[byte]]
  for p in pending:
    planes.add placePlane(decodePlanes(p.comp, p.data, p.src), p.src, p.x, p.y, p.dst)
  if planes.len > nc: result.alpha = some(planes[nc])
  planes.setLen(nc)
  result.channels = planes

proc writePattern(p: PsdPattern; o: var seq[byte]) =
  if p.width > PatMaxEdge or p.height > PatMaxEdge or p.width > 32767 or p.height > 32767:
    raise limitExceeded("pattern size")
  o.putU32(1)
  o.putU32(p.mode)
  o.putU16(uint16(p.height))
  o.putU16(uint16(p.width))
  let units = utf16Units(p.name) & @[0'u16]
  o.putU32(uint32(units.len))
  for u in units: o.putU16(u)
  let idl = min(p.id.len, 255)
  o.putU8(uint8(idl))
  for i in 0 ..< idl: o.add byte(p.id[i])
  if p.mode == 2:
    var pal = p.palette.get(@[])
    pal.setLen(768)
    o.put(pal)
  let rect = [0'i32, 0, int32(p.height), int32(p.width)]
  var body: seq[byte]
  for v in rect: body.putI32(v)
  const Slots = 24
  body.putU32(Slots)
  let layout = PlaneLayout(planes: 1, width: p.width, height: p.height, depth: p.depth, version: vPsd)
  template arr(plane: seq[byte]) =
    let comp = if p.depth == 8: 1'u8 else: 0'u8
    let data = encodePlanes(if p.depth == 8: cRle else: cRaw, plane, layout)
    body.putU32(1)
    body.putU32(uint32(23 + data.len))
    body.putU32(uint32(p.depth))
    for v in rect: body.putI32(v)
    body.putU16(uint16(p.depth))
    body.putU8(comp)
    body.put(data)
  let nc = min(p.channels.len, Slots)
  for i in 0 ..< nc: arr(p.channels[i])
  for _ in nc ..< Slots: body.putU32(0)
  if p.alpha.isSome: arr(p.alpha.get)
  else: body.putU32(0)
  body.putU32(0)
  o.putU32(3)
  o.putU32(uint32(body.len))
  o.put(body)

proc parsePatternBlock*(data: openArray[byte]): seq[PsdPattern] =
  var r = newReader(data)
  while r.remaining >= 4:
    let ln = int(r.u32())
    if ln == 0: break
    var body = r.sub(uint64(ln))
    result.add readPattern(body)
    r.pos = min(r.pos + (4 - ln mod 4) mod 4, r.size)

proc writePatternBlock*(patterns: openArray[PsdPattern]): seq[byte] =
  for p in patterns:
    var one: seq[byte]
    writePattern(p, one)
    result.putU32(uint32(one.len))
    result.put(one)
    for _ in 0 ..< (4 - one.len mod 4) mod 4: result.add 0

proc patternBlockKey*(depth: int): string =
  case depth
  of 16: "Pat2"
  of 32: "Pat3"
  else: "Patt"

proc parsePatFile*(data: openArray[byte]): seq[PsdPattern] =
  var r = newReader(data)
  let sig = r.fourcc()
  if sig != "8BPT": raise badSignature("8BPT", sig)
  discard r.u16()
  let count = r.u32()
  for _ in 0'u32 ..< min(count, 100_000'u32): result.add readPattern(r)

proc writePatFile*(patterns: openArray[PsdPattern]): seq[byte] =
  result.put("8BPT")
  result.putU16(1)
  result.putU32(uint32(patterns.len))
  for p in patterns: writePattern(p, result)
