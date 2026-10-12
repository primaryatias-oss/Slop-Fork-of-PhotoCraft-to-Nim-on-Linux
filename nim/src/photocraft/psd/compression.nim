## Channel image data codecs: Raw, RLE (PackBits), ZIP and ZIP with prediction.
##
## Decoded data is planar bytes in the file's big-endian sample order: `planes × height ×
## rowBytes` (depth 1 packs 8 pixels per byte, MSB first).

import ./io, ../zlib

const MaxDecodedBytes* = 8'u64 shl 30
  ## Backstop against decompression bombs (8 GiB, as the Rust crate on 64-bit targets).

type
  Version* = enum vPsd = 1, vPsb = 2
  Compression* = object
    ## 0 raw, 1 RLE, 2 ZIP, 3 ZIP with prediction; other values are preserved but undecodable.
    code*: uint16
  PlaneLayout* = object
    planes*, width*, height*: int
    depth*: int
    version*: Version

const
  cRaw* = Compression(code: 0)
  cRle* = Compression(code: 1)
  cZip* = Compression(code: 2)
  cZipPrediction* = Compression(code: 3)
  AllCompressions* = [cRaw, cRle, cZip, cZipPrediction]

proc isPsb*(v: Version): bool {.inline.} = v == vPsb
proc `==`*(a, b: Compression): bool = a.code == b.code
proc `$`*(c: Compression): string =
  case c.code
  of 0: "Raw"
  of 1: "Rle"
  of 2: "Zip"
  of 3: "ZipPrediction"
  else: "Unknown(" & $c.code & ")"

proc rowBytes*(width, depth: int): int =
  if depth == 1: (width + 7) div 8 else: width * max(depth div 8, 1)

proc rowBytes*(l: PlaneLayout): int = rowBytes(l.width, l.depth)

proc totalBytes*(l: PlaneLayout): uint64 =
  let rb = uint64(l.rowBytes)
  let p = uint64(max(l.planes, 0))
  let h = uint64(max(l.height, 0))
  if p != 0 and h > high(uint64) div p: raise limitExceeded("decoded size overflow")
  let ph = p * h
  if rb != 0 and ph > high(uint64) div rb: raise limitExceeded("decoded size overflow")
  ph * rb

proc decodedLen*(l: PlaneLayout): int =
  let t = l.totalBytes
  if t > MaxDecodedBytes: raise limitExceeded("decoded channel data exceeds MAX_DECODED_BYTES")
  int(t)

proc rows(l: PlaneLayout): int = l.planes * l.height
proc countSize(l: PlaneLayout): int = (if l.version.isPsb: 4 else: 2)

# ---------------------------------------------------------------- PackBits

proc packbitsEncode*(src: openArray[byte]; o: var seq[byte]) =
  ## Encodes one row. Never emits the -128 no-op.
  let n = src.len
  var i = 0
  while i < n:
    var run = 1
    while i + run < n and run < 128 and src[i + run] == src[i]: inc run
    if run >= 2:
      o.add cast[byte](int8(1 - run))
      o.add src[i]
      i += run
      continue
    let start = i
    var ln = 0
    while i < n and ln < 128:
      if i + 2 < n and src[i] == src[i + 1] and src[i] == src[i + 2]: break
      inc i
      inc ln
    o.add byte(ln - 1)
    for k in start ..< start + ln: o.add src[k]

proc packbitsEncode*(src: openArray[byte]): seq[byte] = packbitsEncode(src, result)

proc packbitsDecodeInto*(src: openArray[byte]; expected: int; o: var seq[byte]) =
  ## Decodes `src` into exactly `expected` bytes appended to `o`.
  let target = o.len + expected
  var i = 0
  while o.len < target:
    if i >= src.len: raise decompressErr("PackBits row ended early")
    let h = int(cast[int8](src[i]))
    inc i
    if h >= 0:
      let ln = h + 1
      if i + ln > src.len: raise decompressErr("PackBits literal truncated")
      if o.len + ln > target: raise decompressErr("PackBits literal overflows row")
      o.add src.toOpenArray(i, i + ln - 1)
      i += ln
    elif h != -128:
      let ln = 1 - h
      if i >= src.len: raise decompressErr("PackBits run truncated")
      let b = src[i]
      inc i
      if o.len + ln > target: raise decompressErr("PackBits run overflows row")
      for _ in 0 ..< ln: o.add b

proc packbitsDecode*(src: openArray[byte]; expected: int): seq[byte] =
  result = newSeqOfCap[byte](min(expected, src.len * 64))
  packbitsDecodeInto(src, expected, result)

# ---------------------------------------------------------------- zip

proc zipCompress*(data: openArray[byte]): seq[byte] = zlibCompress(data, 6)

proc inflateChecked(data: openArray[byte]; expected: uint64): seq[byte] =
  ## Inflates, requiring at least `expected` bytes; keeps exactly `expected`.
  let cap = if expected > uint64(high(int) - (1 shl 20)): high(int) else: int(expected) + (1 shl 20)
  try:
    result = zlibDecompress(data, maxOut = cap, checkAdler = true)
  except ZlibError as e:
    raise decompressErr(e.msg)
  if uint64(result.len) < expected:
    raise decompressErr("zlib stream produced " & $result.len & " of " & $expected & " bytes")
  result.setLen(int(expected))

proc zipDecompress*(data: openArray[byte]; expected: int): seq[byte] =
  inflateChecked(data, uint64(expected))

# ---------------------------------------------------------------- prediction

proc predict*(buf: var seq[byte]; l: PlaneLayout) =
  ## Horizontal delta prediction in place (encoder side). 32-bit rows are split into four byte
  ## planes (MSB first) and then byte-delta coded, as Photoshop does.
  let rb = l.rowBytes
  if rb == 0: return
  let w = l.width
  var scratch = newSeq[byte](rb)
  var row = 0
  while row + rb <= buf.len:
    case l.depth
    of 1, 8:
      for i in countdown(rb - 1, 1): buf[row + i] = buf[row + i] - buf[row + i - 1]
    of 16:
      for i in countdown(w - 1, 1):
        let a = beU16(buf, row + 2 * i)
        let b = beU16(buf, row + 2 * i - 2)
        let d = a - b
        buf[row + 2 * i] = byte(d shr 8)
        buf[row + 2 * i + 1] = byte(d and 0xFF)
    of 32:
      for i in 0 ..< w:
        for k in 0..3: scratch[k * w + i] = buf[row + 4 * i + k]
      for i in countdown(rb - 1, 1): scratch[i] = scratch[i] - scratch[i - 1]
      for i in 0 ..< rb: buf[row + i] = scratch[i]
    else: raise unsupported("prediction for depth " & $l.depth)
    row += rb

proc unpredict*(buf: var seq[byte]; l: PlaneLayout) =
  let rb = l.rowBytes
  if rb == 0: return
  let w = l.width
  var scratch = newSeq[byte](rb)
  var row = 0
  while row + rb <= buf.len:
    case l.depth
    of 1, 8:
      for i in 1 ..< rb: buf[row + i] = buf[row + i] + buf[row + i - 1]
    of 16:
      for i in 1 ..< w:
        let a = beU16(buf, row + 2 * i)
        let b = beU16(buf, row + 2 * i - 2)
        let s = a + b
        buf[row + 2 * i] = byte(s shr 8)
        buf[row + 2 * i + 1] = byte(s and 0xFF)
    of 32:
      for i in 1 ..< rb: buf[row + i] = buf[row + i] + buf[row + i - 1]
      for i in 0 ..< rb: scratch[i] = buf[row + i]
      for i in 0 ..< w:
        for k in 0..3: buf[row + 4 * i + k] = scratch[k * w + i]
    else: raise unsupported("prediction for depth " & $l.depth)
    row += rb

# ---------------------------------------------------------------- planes

proc readCount(r: var Reader; v: Version): int =
  if v.isPsb: int(r.u32()) else: int(r.u16())

proc decodeRle(data: openArray[byte]; l: PlaneLayout; total: int): seq[byte] =
  let rows = l.rows
  let rb = l.rowBytes
  var r = newReader(data)
  r.checkCount(uint64(rows), uint64(l.countSize))
  var counts = newSeq[int](rows)
  for i in 0 ..< rows: counts[i] = readCount(r, l.version)
  if uint64(total) > uint64(r.remaining) * 64: raise decompressErr("RLE data too short for declared size")
  result = newSeqOfCap[byte](total)
  for c in counts:
    if c > r.remaining: raise eof(r.pos, c - r.remaining)
    packbitsDecodeInto(data.toOpenArray(r.pos, r.pos + c - 1), rb, result)
    r.pos += c

proc encodeRle(decoded: openArray[byte]; l: PlaneLayout): seq[byte] =
  let rows = l.rows
  let rb = l.rowBytes
  let cs = l.countSize
  result = newSeq[byte](rows * cs)
  var enc: seq[byte]
  for row in 0 ..< rows:
    let start = enc.len
    if rb > 0: packbitsEncode(decoded.toOpenArray(row * rb, (row + 1) * rb - 1), enc)
    let n = enc.len - start
    if cs == 2:
      if n > 65535: raise limitExceeded("RLE row exceeds 65535 bytes (use PSB)")
      result[row * 2] = byte(n shr 8)
      result[row * 2 + 1] = byte(n and 0xFF)
    else:
      for k in 0..3: result[row * 4 + k] = byte((n shr (8 * (3 - k))) and 0xFF)
  result.add enc

proc decodePlanes*(c: Compression; data: openArray[byte]; l: PlaneLayout): seq[byte] =
  ## Decodes planar data (excluding the 2-byte compression field).
  let total = l.decodedLen
  case c.code
  of 0:
    if data.len < total: raise eof(data.len, total - data.len)
    result = newSeq[byte](total)
    if total > 0: copyMem(addr result[0], unsafeAddr data[0], total)
  of 1: result = decodeRle(data, l, total)
  of 2: result = inflateChecked(data, uint64(total))
  of 3:
    result = inflateChecked(data, uint64(total))
    unpredict(result, l)
  else: raise unsupported("compression method " & $c.code)

proc encodePlanes*(c: Compression; decoded: openArray[byte]; l: PlaneLayout): seq[byte] =
  let total = l.decodedLen
  if decoded.len != total:
    raise invalid("decoded buffer has " & $decoded.len & " bytes, layout requires " & $total)
  case c.code
  of 0: @decoded
  of 1: encodeRle(decoded, l)
  of 2: zipCompress(decoded)
  of 3:
    var v = @decoded
    predict(v, l)
    zipCompress(v)
  else: raise unsupported("compression method " & $c.code)

proc validatePlanes*(c: Compression; data: openArray[byte]; l: PlaneLayout) =
  ## Validates encoded sizes and, for ZIP, the complete stream. Oversized ZIP data is kept
  ## verbatim without inflating it (decoding reports the limit later).
  let total = l.totalBytes
  case c.code
  of 0:
    if uint64(data.len) < total: raise eof(data.len, int(min(total - uint64(data.len), uint64(high(int)))))
  of 1:
    let rows = l.rows
    var r = newReader(data)
    r.checkCount(uint64(rows), uint64(l.countSize))
    var sum = 0'u64
    for _ in 0 ..< rows: sum += uint64(readCount(r, l.version))
    if sum > uint64(r.remaining): raise eof(data.len, int(min(sum - uint64(r.remaining), uint64(high(int)))))
  of 2, 3:
    if total > MaxDecodedBytes: return
    discard inflateChecked(data, total)
  else: discard
