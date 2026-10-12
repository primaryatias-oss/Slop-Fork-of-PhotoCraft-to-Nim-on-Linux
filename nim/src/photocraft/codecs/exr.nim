## OpenEXR, written from the OpenEXR file layout documentation: single- and multi-part files,
## scanline and tiled (level 0) parts, HALF/FLOAT/UINT channels, and the NONE, RLE, ZIPS, ZIP,
## PIZ and PXR24 compressions. Multi-part files open the part that looks most like a colour
## image (RGBA > RGB > YA > Y > single channel) with a warning naming the part count. Writing:
## scanline RGB(A)/Y(A), HALF or FLOAT, NONE/RLE/ZIPS/ZIP (PIZ is written as ZIP).

import std/[options, strutils, tables, algorithm]
import ./image, ../zlib, ../cms/lut3d

const Magic = [0x76'u8, 0x2F, 0x31, 0x01]

type
  ExrChannel* = object
    name*: string
    pixelType*: int   ## 0 UINT, 1 HALF, 2 FLOAT
    xs, ys: int
  ExrPart* = object
    index*: int
    name*, view*: Option[string]
    channels*: seq[ExrChannel]
    compression*: int
    dataWindow*: array[4, int]
    tiled*: bool
    deep*: bool
    tileW, tileH: int
    lineOrder: int
    chunkCount: int
    offsets: seq[int64]

proc err(msg: string): ref CodecError = malformed(fmtOpenExr, msg)

proc rdI32(b: openArray[byte]; p: int): int =
  if p < 0 or p + 4 > b.len: raise err("truncated data")
  int(cast[int32](uint32(b[p]) or (uint32(b[p + 1]) shl 8) or (uint32(b[p + 2]) shl 16) or (uint32(b[p + 3]) shl 24)))
proc rdU32(b: openArray[byte]; p: int): uint32 =
  if p < 0 or p + 4 > b.len: raise err("truncated data")
  uint32(b[p]) or (uint32(b[p + 1]) shl 8) or (uint32(b[p + 2]) shl 16) or (uint32(b[p + 3]) shl 24)
proc rdI64(b: openArray[byte]; p: int): int64 =
  if p < 0 or p + 8 > b.len: raise err("truncated data")
  var v: uint64
  for i in countdown(7, 0): v = (v shl 8) or uint64(b[p + i])
  cast[int64](v)

proc baseName(n: string): string =
  let d = n.rfind('.')
  if d >= 0: n[d + 1 .. ^1] else: n

proc colorRank*(p: ExrPart): int =
  proc has(n: string): bool =
    for c in p.channels:
      if baseName(c.name) == n: return true
    false
  if has("R") and has("G") and has("B"): return (if has("A"): 4 else: 3)
  if has("Y"): return (if has("A"): 2 else: 1)
  if p.channels.len == 1: 1 else: 0

proc readHeaders(b: openArray[byte]): seq[ExrPart] =
  if b.len < 8 or b[0] != Magic[0] or b[1] != Magic[1] or b[2] != Magic[2] or b[3] != Magic[3]: raise err("not an OpenEXR file")
  let flags = rdU32(b, 4)
  let tiledFlag = (flags and 0x200) != 0
  let multi = (flags and 0x1000) != 0
  let deepFlag = (flags and 0x800) != 0
  var p = 8
  template cstr(): string =
    var r = ""
    while p < b.len and b[p] != 0:
      r.add char(b[p])
      inc p
    if p >= b.len: raise err("truncated header")
    inc p
    r
  while true:
    var part = ExrPart(index: result.len, tiled: tiledFlag and not multi, deep: deepFlag and not multi, lineOrder: 0)
    var any = false
    while true:
      if p >= b.len: raise err("truncated header")
      if b[p] == 0:
        inc p
        break
      any = true
      let name = cstr()
      let ty = cstr()
      let size = rdI32(b, p)
      p += 4
      if size < 0 or p + size > b.len: raise err("attribute " & name & " is truncated")
      let s = p
      case name
      of "channels":
        var q = s
        while q < s + size and b[q] != 0:
          var cn = ""
          while q < s + size and b[q] != 0:
            cn.add char(b[q])
            inc q
          inc q
          let pt = rdI32(b, q)
          let xs = rdI32(b, q + 8)
          let ys = rdI32(b, q + 12)
          q += 16
          part.channels.add ExrChannel(name: cn, pixelType: pt, xs: xs, ys: ys)
      of "compression": part.compression = int(b[s])
      of "dataWindow":
        for k in 0..3: part.dataWindow[k] = rdI32(b, s + 4 * k)
      of "lineOrder": part.lineOrder = int(b[s])
      of "tiles":
        part.tileW = int(rdU32(b, s))
        part.tileH = int(rdU32(b, s + 4))
        part.tiled = true
      of "name":
        var n = ""
        for k in 0 ..< size: n.add char(b[s + k])
        part.name = some(n)
      of "view":
        var n = ""
        for k in 0 ..< size: n.add char(b[s + k])
        part.view = some(n)
      of "type":
        var n = ""
        for k in 0 ..< size: n.add char(b[s + k])
        part.tiled = n.startsWith("tiled")
        part.deep = n.startsWith("deep")
      of "chunkCount": part.chunkCount = rdI32(b, s)
      else: discard ty
      p += size
    if not any: break
    result.add part
    if not multi: break
    if p < b.len and b[p] == 0:
      inc p
      break
  if result.len == 0: raise err("no parts")
  # offset tables
  for part in result.mitems:
    let w = part.dataWindow[2] - part.dataWindow[0] + 1
    let h = part.dataWindow[3] - part.dataWindow[1] + 1
    if w <= 0 or h <= 0: raise err("empty data window")
    var n = part.chunkCount
    if n <= 0:
      if part.tiled:
        if part.tileW <= 0 or part.tileH <= 0: raise err("bad tile size")
        n = ((w + part.tileW - 1) div part.tileW) * ((h + part.tileH - 1) div part.tileH)
      else:
        let lines = case part.compression
          of 0, 1, 2: 1
          of 3, 5: 16
          of 4, 6, 7: 32
          of 8: 32
          of 9: 256
          else: 1
        n = (h + lines - 1) div lines
    if n > 10_000_000 or p + 8 * n > b.len: raise err("offset table is truncated")
    for k in 0 ..< n: part.offsets.add rdI64(b, p + 8 * k)
    p += 8 * n

proc exrInfo*(b: openArray[byte]): seq[ExrPart] = readHeaders(b)

# ---------------------------------------------------------------- decompression

proc unpredictInterleave(t: var seq[byte]): seq[byte] =
  for i in 1 ..< t.len: t[i] = byte((int(t[i - 1]) + int(t[i]) - 128) and 0xFF)
  result = newSeq[byte](t.len)
  let half = (t.len + 1) div 2
  var o = 0
  for i in 0 ..< half:
    result[o] = t[i]
    inc o
    if half + i < t.len:
      result[o] = t[half + i]
      inc o

proc rleDecode(src: openArray[byte]; expected: int): seq[byte] =
  var p = 0
  while p < src.len and result.len < expected:
    let n = int(cast[int8](src[p]))
    inc p
    if n < 0:
      for k in 0 ..< -n:
        if p + k < src.len: result.add src[p + k]
      p += -n
    else:
      if p < src.len:
        for _ in 0 .. n: result.add src[p]
      inc p

# PIZ: Huffman + Haar wavelet + bitmap LUT

const
  HufEncSize = 65537
  ShortZeroRun = 59
  LongZeroRun = 63
  ShortestLongRun = 2 + LongZeroRun - ShortZeroRun

proc hufUncompress(src: openArray[byte]; nRaw: int): seq[uint16] =
  if src.len < 20: raise err("PIZ Huffman data is truncated")
  let imin = int(rdU32(src, 0))
  let imax = int(rdU32(src, 4))
  let nBits = int(rdU32(src, 12))
  if imin < 0 or imin >= HufEncSize or imax < 0 or imax >= HufEncSize: raise err("bad PIZ Huffman range")
  var p = 20
  var c: uint64 = 0
  var lc = 0
  template getBits(n: int): int =
    while lc < n:
      if p >= src.len: raise err("PIZ Huffman table is truncated")
      c = (c shl 8) or uint64(src[p])
      inc p
      lc += 8
    lc -= n
    int((c shr lc) and ((1'u64 shl n) - 1))
  var hcode = newSeq[int64](HufEncSize)
  var i = imin
  while i <= imax:
    let l = getBits(6)
    hcode[i] = l
    if l == LongZeroRun:
      var zr = getBits(8) + ShortestLongRun
      if i + zr > imax + 1: raise err("bad PIZ zero run")
      while zr > 0:
        hcode[i] = 0
        inc i
        dec zr
      dec i
    elif l >= ShortZeroRun:
      var zr = l - ShortZeroRun + 2
      if i + zr > imax + 1: raise err("bad PIZ zero run")
      while zr > 0:
        hcode[i] = 0
        inc i
        dec zr
      dec i
    inc i
  # canonical codes
  var n: array[59, int64]
  for k in 0 ..< HufEncSize: inc n[min(hcode[k], 58)]
  var cc: int64 = 0
  for k in countdown(58, 1):
    let nc = (cc + n[k]) shr 1
    n[k] = cc
    cc = nc
  var lookup: array[59, Table[int64, int]]
  for k in 0 ..< HufEncSize:
    let l = int(hcode[k])
    if l > 0:
      lookup[l][n[l]] = k
      inc n[l]
  result = newSeqOfCap[uint16](nRaw)
  # decode exactly nBits bits, MSB first
  let dataStart = p
  var bitPos = 0
  var code: int64 = 0
  var clen = 0
  let rlc = imax
  template bit(): int =
    let byteI = dataStart + (bitPos shr 3)
    if byteI >= src.len: raise err("PIZ Huffman data is truncated")
    let bv = (int(src[byteI]) shr (7 - (bitPos and 7))) and 1
    inc bitPos
    bv
  while bitPos < nBits:
    code = (code shl 1) or int64(bit())
    inc clen
    if clen > 58: raise err("bad PIZ Huffman code")
    let hit = lookup[clen].getOrDefault(code, -1)
    if hit >= 0:
      if hit == rlc:
        if bitPos + 8 > nBits: raise err("PIZ run truncated")
        var cs = 0
        for _ in 0..7: cs = (cs shl 1) or bit()
        if result.len == 0: raise err("PIZ run without a value")
        let v = result[^1]
        for _ in 0 ..< cs:
          if result.len >= nRaw: break
          result.add v
      else:
        if result.len >= nRaw: break
        result.add uint16(hit)
      code = 0
      clen = 0
  if result.len != nRaw: raise err("PIZ data decodes to the wrong size")

proc wdec14(l, h: uint16): (uint16, uint16) {.inline.} =
  let ls = int(cast[int16](l))
  let hs = int(cast[int16](h))
  let ai = ls + (hs and 1) + (hs shr 1)
  (uint16(ai and 0xFFFF), uint16((ai - hs) and 0xFFFF))

proc wdec16(l, h: uint16): (uint16, uint16) {.inline.} =
  let m = int(l)
  let d = int(h)
  let bb = (m - (d shr 1)) and 0xFFFF
  let aa = (d + bb - 32768) and 0xFFFF
  (uint16(aa), uint16(bb))

proc wav2Decode(buf: var seq[uint16]; start, nx, ox, ny, oy: int; mx: int) =
  let w14 = mx < (1 shl 14)
  let n = min(nx, ny)
  var p = 1
  while p <= n: p = p shl 1
  p = p shr 1
  var p2 = p
  p = p shr 1
  template dec(a, b: uint16): (uint16, uint16) = (if w14: wdec14(a, b) else: wdec16(a, b))
  while p >= 1:
    var py = start
    let ey = start + oy * (ny - p2)
    let oy1 = oy * p
    let oy2 = oy * p2
    let ox1 = ox * p
    let ox2 = ox * p2
    while py <= ey:
      var px = py
      let ex = py + ox * (nx - p2)
      while px <= ex:
        let p01 = px + ox1
        let p10 = px + oy1
        let p11 = p10 + ox1
        let (i00, i10) = dec(buf[px], buf[p10])
        let (i01, i11) = dec(buf[p01], buf[p11])
        (buf[px], buf[p01]) = dec(i00, i01)
        (buf[p10], buf[p11]) = dec(i10, i11)
        px += ox2
      if (nx and p) != 0:
        let p10 = px + oy1
        let (i00, x10) = dec(buf[px], buf[p10])
        buf[p10] = x10
        buf[px] = i00
      py += oy2
    if (ny and p) != 0:
      var px = py
      let ex = py + ox * (nx - p2)
      while px <= ex:
        let p01 = px + ox1
        let (i00, x01) = dec(buf[px], buf[p01])
        buf[p01] = x01
        buf[px] = i00
        px += ox2
    p2 = p
    p = p shr 1

proc pizDecode(src: openArray[byte]; chans: seq[ExrChannel]; nx, ny: int): seq[byte] =
  ## Returns the uncompressed block (per line, per channel, per pixel; little-endian).
  if src.len < 4: raise err("PIZ block is truncated")
  let minNz = int(src[0]) or (int(src[1]) shl 8)
  let maxNz = int(src[2]) or (int(src[3]) shl 8)
  if maxNz >= 8192: raise err("bad PIZ bitmap range")
  var bitmap = newSeq[byte](8192)
  var p = 4
  if minNz <= maxNz:
    if p + maxNz - minNz + 1 > src.len: raise err("PIZ bitmap is truncated")
    for k in minNz .. maxNz: bitmap[k] = src[p + k - minNz]
    p += maxNz - minNz + 1
  var lut = newSeq[uint16](65536)
  var k = 0
  for i in 0 ..< 65536:
    if i == 0 or (bitmap[i shr 3] and byte(1 shl (i and 7))) != 0:
      lut[k] = uint16(i)
      inc k
  let maxValue = k - 1
  let length = rdI32(src, p)
  p += 4
  if length < 0 or p + length > src.len: raise err("PIZ data is truncated")
  var words = 0
  var sizes: seq[int]
  for c in chans:
    let size = if c.pixelType == 1: 1 else: 2
    sizes.add size
    words += nx * ny * size
  var tmp = hufUncompress(src.toOpenArray(p, p + length - 1), words)
  var start = 0
  for i, c in chans:
    for j in 0 ..< sizes[i]:
      wav2Decode(tmp, start + j, nx, sizes[i], ny, nx * sizes[i], maxValue)
    start += nx * ny * sizes[i]
  for v in tmp.mitems: v = lut[v]
  result = newSeqOfCap[byte](words * 2)
  var planeStart = newSeq[int](chans.len)
  var acc = 0
  for i in 0 ..< chans.len:
    planeStart[i] = acc
    acc += nx * ny * sizes[i]
  for y in 0 ..< ny:
    for i in 0 ..< chans.len:
      let n = nx * sizes[i]
      let s = planeStart[i] + y * n
      for q in 0 ..< n:
        let v = tmp[s + q]
        result.add byte(v and 0xFF)
        result.add byte(v shr 8)

proc pxr24Decode(src: openArray[byte]; chans: seq[ExrChannel]; nx, ny: int): seq[byte] =
  var raw: seq[byte]
  try: raw = zlibDecompress(src, maxOut = nx * ny * chans.len * 4 + 1024, checkAdler = false)
  except ZlibError as e: raise err("PXR24: " & e.msg)
  var p = 0
  for y in 0 ..< ny:
    for c in chans:
      case c.pixelType
      of 0:
        if p + 4 * nx > raw.len: raise err("PXR24 data is truncated")
        var pixel = 0'u32
        for j in 0 ..< nx:
          let diff = (uint32(raw[p + j]) shl 24) or (uint32(raw[p + nx + j]) shl 16) or (uint32(raw[p + 2 * nx + j]) shl 8) or uint32(raw[p + 3 * nx + j])
          pixel += diff
          for k in 0..3: result.add byte((pixel shr (8 * k)) and 0xFF)
        p += 4 * nx
      of 1:
        if p + 2 * nx > raw.len: raise err("PXR24 data is truncated")
        var pixel = 0'u32
        for j in 0 ..< nx:
          let diff = (uint32(raw[p + j]) shl 8) or uint32(raw[p + nx + j])
          pixel = (pixel + diff) and 0xFFFF
          result.add byte(pixel and 0xFF)
          result.add byte(pixel shr 8)
        p += 2 * nx
      else:
        if p + 3 * nx > raw.len: raise err("PXR24 data is truncated")
        var pixel = 0'u32
        for j in 0 ..< nx:
          let diff = (uint32(raw[p + j]) shl 24) or (uint32(raw[p + nx + j]) shl 16) or (uint32(raw[p + 2 * nx + j]) shl 8)
          pixel += diff
          for k in 0..3: result.add byte((pixel shr (8 * k)) and 0xFF)
        p += 3 * nx

proc decompressBlock(comp: int; src: openArray[byte]; chans: seq[ExrChannel]; nx, ny, expected: int): seq[byte] =
  # writers store a block raw when compressing would not make it smaller
  if src.len >= expected: return @(src.toOpenArray(0, expected - 1))
  case comp
  of 0: @src
  of 1:
    var t = rleDecode(src, expected)
    unpredictInterleave(t)
  of 2, 3:
    var t: seq[byte]
    try: t = zlibDecompress(src, maxOut = expected + 1024, checkAdler = false)
    except ZlibError as e: raise err("zip: " & e.msg)
    unpredictInterleave(t)
  of 4: pizDecode(src, chans, nx, ny)
  of 5: pxr24Decode(src, chans, nx, ny)
  else: raise unsupported(fmtOpenExr, "compression " & $comp & " (B44/DWA) is not supported")

# ---------------------------------------------------------------- decode

proc decodePartImpl(b: openArray[byte]; part: ExrPart; multi: bool; limits: Limits; names: seq[string]): Image =
  if part.deep: raise unsupported(fmtOpenExr, "deep data is not supported")
  let x0 = part.dataWindow[0]
  let y0 = part.dataWindow[1]
  let w = part.dataWindow[2] - x0 + 1
  let h = part.dataWindow[3] - y0 + 1
  limits.checkBytes(w, h, max(part.channels.len, 1) * 4)
  for c in part.channels:
    if c.xs != 1 or c.ys != 1: raise unsupported(fmtOpenExr, "subsampled channels")
  # pick channels
  proc find(n: string): int =
    for i, c in part.channels:
      if (if names.len > 0: c.name == n else: baseName(c.name) == n): return i
    -1
  var pick: seq[int]
  var layout: ChannelLayout
  if names.len > 0:
    var rIdx, gIdx, bIdx, aIdx, yIdx = -1
    for n in names:
      var i = -1
      for k, c in part.channels:
        if c.name == n:
          i = k
          break
      if i < 0: raise err("no channel " & n)
      case baseName(n)
      of "R": rIdx = i
      of "G": gIdx = i
      of "B": bIdx = i
      of "A": aIdx = i
      else: yIdx = i
    if rIdx >= 0 and gIdx >= 0 and bIdx >= 0:
      pick = @[rIdx, gIdx, bIdx]
      layout = clRgb
    elif yIdx >= 0:
      pick = @[yIdx]
      layout = clGray
    else: raise unsupported(fmtOpenExr, "no R/G/B or Y channels")
    if aIdx >= 0:
      pick.add aIdx
      layout = layout.withAlpha
  else:
    let (r, g, bb, a, y) = (find("R"), find("G"), find("B"), find("A"), find("Y"))
    if r >= 0 and g >= 0 and bb >= 0:
      pick = @[r, g, bb]
      layout = clRgb
    elif y >= 0 or part.channels.len == 1:
      pick = @[if y >= 0: y else: 0]
      layout = clGray
    else: raise unsupported(fmtOpenExr, "no R/G/B or Y channels")
    if a >= 0 and a notin pick:
      pick.add a
      layout = layout.withAlpha
  var allHalf = true
  for i in pick:
    if part.channels[i].pixelType != 1: allHalf = false
  let sample = if allHalf: isF16 else: isF32
  var img = newImage(w, h, layout, sample)
  let outCh = layout.channels
  var chanOut = newSeq[int](part.channels.len)
  for i in 0 ..< chanOut.len: chanOut[i] = -1
  for k, i in pick: chanOut[i] = k
  var bytesPerPixelLine = 0
  for c in part.channels: bytesPerPixelLine += (if c.pixelType == 1: 2 else: 4)
  proc storeLine(img: var Image; data: openArray[byte]; off: int; y, xStart, nx: int) =
    ## One line of a block: channels in file order, `nx` pixels each.
    var p = off
    for ci, c in part.channels:
      let sz = if c.pixelType == 1: 2 else: 4
      let k = chanOut[ci]
      if k >= 0 and y >= 0 and y < h:
        for x in 0 ..< nx:
          let xx = xStart + x
          if xx < 0 or xx >= w: continue
          let o = (y * w + xx) * outCh + k
          let at = p + x * sz
          if at + sz > data.len: break
          var f: float32
          case c.pixelType
          of 1:
            let hbits = uint16(data[at]) or (uint16(data[at + 1]) shl 8)
            if sample == isF16:
              img.data[2 * o] = data[at]
              img.data[2 * o + 1] = data[at + 1]
              continue
            f = f16ToF32(hbits)
          of 2: f = cast[float32](uint32(data[at]) or (uint32(data[at + 1]) shl 8) or (uint32(data[at + 2]) shl 16) or (uint32(data[at + 3]) shl 24))
          else: f = float32(uint32(data[at]) or (uint32(data[at + 1]) shl 8) or (uint32(data[at + 2]) shl 16) or (uint32(data[at + 3]) shl 24))
          cast[ptr float32](addr img.data[4 * o])[] = f
      p += nx * sz
  if part.tiled:
    let tw = part.tileW
    let th = part.tileH
    for off in part.offsets:
      if off <= 0 or off + 20 > b.len: continue
      var q = int(off)
      if multi: q += 4  # part number
      let tx = rdI32(b, q)
      let ty = rdI32(b, q + 4)
      let lx = rdI32(b, q + 8)
      let ly = rdI32(b, q + 12)
      let size = rdI32(b, q + 16)
      if lx != 0 or ly != 0: continue
      if size < 0 or q + 20 + size > b.len: raise err("tile is truncated")
      let bx = tx * tw
      let by = ty * th
      let nx = min(tw, w - bx)
      let ny = min(th, h - by)
      if nx <= 0 or ny <= 0: continue
      let expected = nx * ny * bytesPerPixelLine
      let data = decompressBlock(part.compression, b.toOpenArray(q + 20, q + 20 + size - 1), part.channels, nx, ny, expected)
      for r in 0 ..< ny: storeLine(img, data, r * nx * bytesPerPixelLine, by + r, bx, nx)
  else:
    for off in part.offsets:
      if off <= 0 or off + 8 > b.len: continue
      var q = int(off)
      if multi: q += 4
      let yStart = rdI32(b, q) - y0
      let size = rdI32(b, q + 4)
      if size < 0 or q + 8 + size > b.len: raise err("scanline block is truncated")
      let lines = case part.compression
        of 0, 1, 2: 1
        of 3, 5: 16
        else: 32
      let ny = min(lines, h - yStart)
      if ny <= 0: continue
      let expected = w * ny * bytesPerPixelLine
      let data = decompressBlock(part.compression, b.toOpenArray(q + 8, q + 8 + size - 1), part.channels, w, ny, expected)
      for r in 0 ..< ny: storeLine(img, data, r * w * bytesPerPixelLine, yStart + r, 0, w)
  img

proc pickPart(parts: seq[ExrPart]): int =
  var best = 0
  var bestRank = -1
  for i, p in parts:
    if p.deep: continue
    let r = p.colorRank
    if r > bestRank:
      bestRank = r
      best = i
  best

proc decodeExrPart*(b: openArray[byte]; part: int; limits: Limits): Image =
  let parts = readHeaders(b)
  if part < 0 or part >= parts.len: raise err("no part " & $part)
  decodePartImpl(b, parts[part], parts.len > 1, limits, @[])

proc decodeExrChannels*(b: openArray[byte]; part: int; names: seq[string]; limits: Limits): Image =
  let parts = readHeaders(b)
  if part < 0 or part >= parts.len: raise err("no part " & $part)
  decodePartImpl(b, parts[part], parts.len > 1, limits, names)

proc decodeExr*(b: openArray[byte]; limits: Limits): Image =
  ## Decodes the part that looks most like a colour image. Raises `CodecError`.
  let parts = readHeaders(b)
  result = decodePartImpl(b, parts[pickPart(parts)], parts.len > 1, limits, @[])
  if parts.len > 1: result.warnings.add DecodeWarning(kind: dwMoreParts, total: some(parts.len))

proc toSeq(r: HSlice[int, int]): seq[int] =
  for i in r: result.add i

# ---------------------------------------------------------------- encode

proc predictDeinterleave(src: openArray[byte]): seq[byte] =
  var t = newSeq[byte](src.len)
  let half = (src.len + 1) div 2
  var a = 0
  var b = half
  var i = 0
  while i < src.len:
    t[a] = src[i]
    inc a
    inc i
    if i < src.len:
      t[b] = src[i]
      inc b
      inc i
  var prev = int(t[0])
  for k in 1 ..< t.len:
    let d = int(t[k]) - prev + 128
    prev = int(t[k])
    t[k] = byte(d and 0xFF)
  t

proc rleEncode(src: openArray[byte]): seq[byte] =
  var i = 0
  while i < src.len:
    var run = 1
    while i + run < src.len and run < 128 and src[i + run] == src[i]: inc run
    if run >= 3:
      result.add byte(run - 1)
      result.add src[i]
      i += run
    else:
      var j = i
      while j < src.len and j - i < 127:
        if j + 2 < src.len and src[j] == src[j + 1] and src[j] == src[j + 2]: break
        inc j
      if j == i: j = i + 1
      result.add byte(cast[uint8](int8(-(j - i))))
      result.add src.toOpenArray(i, j - 1)
      i = j

proc encodeExr*(src: Image; layout: ChannelLayout; sample: ImgSample; opts: EncodeOptions): seq[byte] =
  ## Scanline OpenEXR: Y/YA/RGB/RGBA as HALF (F16) or FLOAT (F32).
  let lay = if layout.isGray: (if layout.hasAlpha: clGrayA else: clGray) else: (if layout.hasAlpha: clRgba else: clRgb)
  let s = if sample == isF16: isF16 else: isF32
  let img = src.convert(lay, s)
  let w = img.width
  let h = img.height
  let names = case lay
    of clGray: @["Y"]
    of clGrayA: @["Y", "A"]
    of clRgb: @["R", "G", "B"]
    else: @["R", "G", "B", "A"]
  var order = toSeq(0 ..< names.len)
  order.sort(proc(a, b: int): int = cmp(names[a], names[b]))
  let comp = case opts.exrCompression
    of ecNone: 0
    of ecRle: 1
    of ecZip1: 2
    of ecZip16, ecPiz: 3
  let lines = if comp == 3: 16 else: 1
  let ptype = if s == isF16: 1 else: 2
  let sz = if s == isF16: 2 else: 4
  var hdr: seq[byte] = @Magic
  hdr.add [2'u8, 0, 0, 0]
  proc attr(o: var seq[byte]; name, ty: string; data: openArray[byte]) =
    o.add name.toBytes; o.add 0
    o.add ty.toBytes; o.add 0
    let n = data.len
    o.add [byte(n and 0xFF), byte((n shr 8) and 0xFF), byte((n shr 16) and 0xFF), byte((n shr 24) and 0xFF)]
    o.add data
  proc i32s(vals: openArray[int]): seq[byte] =
    for v in vals:
      let u = cast[uint32](int32(v))
      result.add [byte(u and 0xFF), byte((u shr 8) and 0xFF), byte((u shr 16) and 0xFF), byte(u shr 24)]
  proc f32s(vals: openArray[float32]): seq[byte] =
    for v in vals:
      let u = cast[uint32](v)
      result.add [byte(u and 0xFF), byte((u shr 8) and 0xFF), byte((u shr 16) and 0xFF), byte(u shr 24)]
  var chl: seq[byte]
  for k in order:
    chl.add names[k].toBytes
    chl.add 0
    chl.add i32s([ptype, 0, 1, 1])
  chl.add 0
  hdr.attr("channels", "chlist", chl)
  hdr.attr("compression", "compression", [byte(comp)])
  hdr.attr("dataWindow", "box2i", i32s([0, 0, w - 1, h - 1]))
  hdr.attr("displayWindow", "box2i", i32s([0, 0, w - 1, h - 1]))
  hdr.attr("lineOrder", "lineOrder", [0'u8])
  hdr.attr("pixelAspectRatio", "float", f32s([1.0'f32]))
  hdr.attr("screenWindowCenter", "v2f", f32s([0.0'f32, 0.0]))
  hdr.attr("screenWindowWidth", "float", f32s([1.0'f32]))
  hdr.add 0
  let nBlocks = (h + lines - 1) div lines
  let tableAt = hdr.len
  result = hdr
  for _ in 0 ..< nBlocks * 8: result.add 0
  let ch = lay.channels
  for blk in 0 ..< nBlocks:
    let yStart = blk * lines
    let ny = min(lines, h - yStart)
    var raw: seq[byte]
    for y in yStart ..< yStart + ny:
      for k in order:
        for x in 0 ..< w:
          let o = ((y * w + x) * ch + k) * sz
          for q in 0 ..< sz: raw.add img.data[o + q]
    let data = case comp
      of 0: raw
      of 1: rleEncode(predictDeinterleave(raw))
      else: zlibCompress(predictDeinterleave(raw), 6)
    let payload = if data.len < raw.len: data else: raw
    let off = result.len
    for k in 0..7: result[tableAt + blk * 8 + k] = byte((off shr (8 * k)) and 0xFF)
    result.add i32s([yStart, payload.len])
    result.add payload

