## Lossless JPEG decoder (port of `raw/src/ljpeg.rs`; ITU-T T.81 / ISO 10918-1, process 14
## "LJ92": Huffman-coded, predictive, sequential, 2–16 bit samples).
##
## Output is the sample sequence in raster order: `height` rows of `width × components`
## interleaved samples. Only one interleaved scan holding every component with sampling factors
## 1×1 is supported, which is how DNG, CR2 and the TIFF-based raws use it.

import std/options
import ./tiff

type
  LjFrame* = object
    ## Frame header (SOF3).
    precision*: int
    width*, height*, components*: int

  Huffman = object
    lookup: seq[uint16] ## `(length shl 8) or symbol` for codes up to `LookupBits`, 0 if longer
    fast: seq[int32]    ## `(difference shl 8) or bits` when code + extra bits fit, else 0
    maxcode: array[17, int32]
    mincode: array[17, int32]
    valptr: array[17, int]
    vals: seq[uint8]

  Bits = object
    ## MSB-first bit reader over entropy-coded data: removes `FF 00` stuffing, stops at markers
    ## (then feeds zero bits).
    data: ptr UncheckedArray[byte]
    len, pos: int
    acc: uint64
    n: int
    marker: bool
    fake: int ## zero bytes fed past the end of the data or a marker

const LookupBits = 13 ## bits resolved by one table lookup

proc samples*(f: LjFrame): int = f.width * f.height * f.components

proc newHuffman(counts: array[16, uint8]; vals: openArray[uint8]): Huffman =
  var total = 0
  for c in counts: total += int(c)
  if total == 0 or total > 256 or vals.len < total: raise malformed("bad Huffman table")
  result.lookup = newSeq[uint16](1 shl LookupBits)
  result.fast = newSeq[int32](1 shl LookupBits)
  for i in 0 .. 16: result.maxcode[i] = -1
  result.vals = @(vals.toOpenArray(0, total - 1))
  var code = 0'u32
  var k = 0
  for len in 1 .. 16:
    let n = int(counts[len - 1])
    if n > 0:
      result.valptr[len] = k
      result.mincode[len] = int32(code)
      for _ in 0 ..< n:
        if code >= (1'u32 shl len): raise malformed("Huffman code overflow")
        if len <= LookupBits:
          let shift = LookupBits - len
          let base = int(code shl shift)
          let entry = (uint16(len) shl 8) or uint16(result.vals[k])
          for slot in base ..< base + (1 shl shift): result.lookup[slot] = entry
        code += 1
        k += 1
      result.maxcode[len] = int32(code) - 1
    code = code shl 1
  for idx in 0 ..< result.fast.len:
    let e = result.lookup[idx]
    let (len, s) = (int(e shr 8), int(e and 0xFF))
    if len == 0: continue
    var d: int32
    if s == 0: d = 0
    elif s == 16: d = 32768
    elif s in 1 .. 15 and len + s <= LookupBits:
      let v = int32((uint32(idx) shr (LookupBits - len - s)) and ((1'u32 shl s) - 1))
      d = if v < (1'i32 shl (s - 1)): v - (1'i32 shl s) + 1 else: v
    else: continue
    result.fast[idx] = (d shl 8) or int32(len + (if s in 1 .. 15: s else: 0))

proc newBits(data: openArray[byte]): Bits =
  Bits(data: if data.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr data[0]) else: nil, len: data.len)

proc refill(b: var Bits) {.inline.} =
  # Fast path: the next 8 bytes hold no 0xFF (no stuffing, no marker).
  if not b.marker and b.pos + 8 <= b.len:
    var chunk = 0'u64
    for k in 0 .. 7: chunk = (chunk shl 8) or uint64(b.data[b.pos + k])
    let inv = not chunk
    if ((inv - 0x0101_0101_0101_0101'u64) and (not inv) and 0x8080_8080_8080_8080'u64) == 0:
      let k = (64 - b.n) div 8 # whole bytes that fit (1..=8)
      if k == 0: return
      if k == 8: b.acc = chunk
      else: b.acc = b.acc or ((chunk shr (64 - 8 * k)) shl (64 - b.n - 8 * k))
      b.n += 8 * k
      b.pos += k
      return
  while b.n <= 56:
    var byt: uint64
    if b.marker:
      inc b.fake
      byt = 0
    elif b.pos < b.len:
      if b.data[b.pos] == 0xFF:
        if b.pos + 1 < b.len and b.data[b.pos + 1] == 0:
          b.pos += 2
          byt = 0xFF
        else:
          b.marker = true
          inc b.fake
          byt = 0
      else:
        byt = uint64(b.data[b.pos])
        inc b.pos
    else:
      b.marker = true
      inc b.fake
      byt = 0
    b.acc = b.acc or (byt shl (56 - b.n))
    b.n += 8

proc peek(b: Bits; k: int): uint32 {.inline.} = uint32(b.acc shr (64 - k))

proc consume(b: var Bits; k: int) {.inline.} =
  b.acc = if k >= 64: 0'u64 else: b.acc shl k
  b.n -= k

proc get(b: var Bits; k: int): uint32 {.inline.} =
  ## Reads `k` (1..=16) bits.
  if b.n < k: b.refill()
  result = b.peek(k)
  b.consume(k)

proc restart(b: var Bits) =
  ## Skips to the next RSTn marker and past it; resets the bit buffer.
  b.acc = 0
  b.n = 0
  b.marker = false
  b.fake = 0
  # Tolerate fill bytes before the marker.
  while b.pos < b.len:
    if b.data[b.pos] == 0xFF:
      if b.pos + 1 < b.len and b.data[b.pos + 1] in 0xD0'u8 .. 0xD7'u8:
        b.pos += 2
        return
      elif b.pos + 1 < b.len and b.data[b.pos + 1] == 0xFF: b.pos += 1
      else: raise malformed("expected a restart marker")
    else: b.pos += 1
  raise malformed("missing restart marker")

proc decode(b: var Bits; h: Huffman): uint32 {.inline.} =
  if b.n < 32: b.refill()
  let e = h.lookup[int(b.peek(LookupBits))]
  if e != 0:
    b.consume(int(e shr 8))
    return uint32(e and 0xFF)
  for len in LookupBits + 1 .. 16:
    let code = int32(b.peek(len))
    if code <= h.maxcode[len]:
      let i = h.valptr[len] + int(code - h.mincode[len])
      if i < 0 or i >= h.vals.len: raise malformed("bad Huffman code")
      b.consume(len)
      return uint32(h.vals[i])
  raise malformed("bad Huffman code")

proc diff(b: var Bits; h: Huffman): int32 {.inline.} =
  ## A difference value: Huffman-coded category SSSS then SSSS extra bits (T.81 H.1.2.2).
  if b.n < 32: b.refill()
  let f = h.fast[int(b.peek(LookupBits))]
  if f != 0:
    b.consume(int(f and 0xFF))
    return ashr(f, 8)
  let s = int(b.decode(h))
  case s
  of 0: 0'i32
  of 1 .. 15:
    let v = int32(b.get(s))
    if v < (1'i32 shl (s - 1)): v - (1'i32 shl s) + 1 else: v
  of 16: 32768'i32
  else: raise malformed("bad difference category")

proc be16(d: openArray[byte]; at: int): int =
  if at < 0 or at + 2 > d.len: raise malformed("truncated lossless JPEG")
  (int(d[at]) shl 8) or int(d[at + 1])

type Parsed = object
  frame: LjFrame
  tables: seq[Huffman] ## per component of the scan (in frame order): its Huffman table
  predictor, pointTransform: int
  restart: int
  entropy: int         ## offset of the entropy-coded data

proc parse(data: openArray[byte]): Parsed =
  if data.len < 2 or data[0] != 0xFF or data[1] != 0xD8: raise malformed("lossless JPEG: missing SOI")
  var pos = 2
  var frame = none(LjFrame)
  var compIds: seq[uint8]
  var huff: array[4, Option[Huffman]]
  var restart = 0
  while true:
    # Skip fill bytes up to the marker.
    while pos + 1 < data.len and data[pos] == 0xFF and data[pos + 1] == 0xFF: inc pos
    if pos >= data.len or data[pos] != 0xFF: raise malformed("lossless JPEG: expected a marker")
    if pos + 1 >= data.len: raise malformed("truncated lossless JPEG")
    let marker = data[pos + 1]
    pos += 2
    if marker == 0xD9: raise malformed("lossless JPEG: no scan")
    if marker in 0xD0'u8 .. 0xD7'u8 or marker == 0x01: continue
    let len = be16(data, pos)
    if len < 2: raise malformed("lossless JPEG: bad segment length")
    if pos + len > data.len: raise malformed("truncated lossless JPEG segment")
    let segStart = pos + 2
    let segLen = len - 2
    template seg(i: int): uint8 = data[segStart + i]
    case marker
    of 0xC3:
      if segLen < 6: raise malformed("short SOF3")
      let precision = int(seg(0))
      let height = (int(seg(1)) shl 8) or int(seg(2))
      let width = (int(seg(3)) shl 8) or int(seg(4))
      let nf = int(seg(5))
      if precision notin 2 .. 16 or width == 0 or height == 0 or nf == 0 or nf > 4 or segLen < 6 + 3 * nf:
        raise malformed("unsupported SOF3 (" & $precision & "-bit, " & $width & "x" & $height & ", " & $nf & " components)")
      compIds.setLen(0)
      for c in 0 ..< nf:
        if seg(6 + 3 * c + 1) != 0x11: raise unsupported("lossless JPEG with subsampled components")
        compIds.add seg(6 + 3 * c)
      frame = some(LjFrame(precision: precision, width: width, height: height, components: nf))
    of 0xC0 .. 0xC2: raise unsupported("lossy (DCT) JPEG compressed raw data")
    of 0xC5 .. 0xC7, 0xC9 .. 0xCB, 0xCD .. 0xCF: raise unsupported("hierarchical or arithmetic-coded JPEG")
    of 0xC4:
      var p = 0
      while p < segLen:
        let id = int(seg(p) and 0x0F)
        if p + 17 > segLen: raise malformed("short DHT")
        var counts: array[16, uint8]
        var total = 0
        for k in 0 .. 15:
          counts[k] = seg(p + 1 + k)
          total += int(counts[k])
        if p + 17 + total > segLen: raise malformed("short DHT")
        if id < 4: huff[id] = some(newHuffman(counts, data.toOpenArray(segStart + p + 17, segStart + p + 17 + total - 1)))
        p += 17 + total
    of 0xDD:
      restart = be16(data.toOpenArray(segStart, segStart + segLen - 1), 0)
    of 0xDA:
      if frame.isNone: raise malformed("lossless JPEG: SOS before SOF3")
      let f = frame.get
      if segLen < 1: raise malformed("short SOS")
      let ns = int(seg(0))
      if ns != f.components or segLen < 1 + 2 * ns + 3: raise unsupported("lossless JPEG with non-interleaved scans")
      var tables: seq[Huffman]
      for c in 0 ..< ns:
        let id = seg(1 + 2 * c)
        if c >= compIds.len or compIds[c] != id: raise unsupported("lossless JPEG scan component order")
        let td = int(seg(2 + 2 * c) shr 4)
        if td >= 4 or huff[td].isNone: raise malformed("missing Huffman table")
        tables.add huff[td].get
      let predictor = int(seg(1 + 2 * ns))
      let pointTransform = int(seg(3 + 2 * ns) and 0x0F)
      if predictor notin 1 .. 7: raise malformed("lossless JPEG predictor " & $predictor)
      if pointTransform >= f.precision: raise malformed("lossless JPEG point transform")
      return Parsed(frame: f, tables: tables, predictor: predictor, pointTransform: pointTransform,
                    restart: restart, entropy: min(pos + len, data.len))
    else: discard # APPn, COM, DQT, DNL… ignored
    pos += len

proc ljFrame*(data: openArray[byte]): LjFrame =
  ## Reads the frame header without decoding.
  parse(data).frame

proc decodeLj*(data: openArray[byte]; maxSamples: int): (LjFrame, seq[uint16]) =
  ## Decodes a lossless JPEG stream. `maxSamples` bounds the output size and is checked before
  ## allocating.
  let p = parse(data)
  let f = p.frame
  let total = f.samples
  if total > maxSamples:
    raise rawError(reLimitExceeded, "lossless JPEG frame " & $f.width & "x" & $f.height & "x" & $f.components)
  # An entropy-coded sample needs at least one bit: a stream far shorter than the frame cannot
  # be valid (cheap decompression-bomb guard).
  let entropyLen = data.len - p.entropy
  if entropyLen * 8 < total div 2: raise malformed("lossless JPEG data is truncated")
  let rowLen = f.width * f.components
  if p.restart != 0 and p.restart mod f.width != 0:
    raise unsupported("lossless JPEG restart interval that is not whole rows")
  let rowsPerInterval = if p.restart == 0: high(int) else: p.restart div f.width
  var outp = newSeq[uint16](total)
  var bits = newBits(data.toOpenArray(p.entropy, data.len - 1))
  let nc = f.components
  let initial = 1'i32 shl (f.precision - p.pointTransform - 1)
  let pred = p.predictor
  var intervalRow = 0
  for y in 0 ..< f.height:
    if intervalRow == rowsPerInterval:
      bits.restart()
      intervalRow = 0
    let firstLine = intervalRow == 0
    let cur = y * rowLen
    let prev = (y - 1) * rowLen
    if pred == 1 or firstLine:
      # Predictor 1 (or the first line of an interval): Ra after the first column.
      for c in 0 ..< nc:
        let px = if firstLine: initial else: int32(outp[prev + c])
        outp[cur + c] = uint16((px + bits.diff(p.tables[c])) and 0xFFFF)
      if nc == 1:
        let t = p.tables[0]
        for i in 1 ..< rowLen:
          outp[cur + i] = uint16((int32(outp[cur + i - 1]) + bits.diff(t)) and 0xFFFF)
      else:
        var i = nc
        while i < rowLen:
          for c in 0 ..< nc:
            outp[cur + i + c] = uint16((int32(outp[cur + i + c - nc]) + bits.diff(p.tables[c])) and 0xFFFF)
          i += nc
      inc intervalRow
      if bits.fake > 64: raise malformed("lossless JPEG data is truncated")
      continue
    for x in 0 ..< f.width:
      for c in 0 ..< nc:
        let i = x * nc + c
        var px: int32
        if x == 0:
          px = if firstLine: initial else: int32(outp[prev + i])
        else:
          let ra = int32(outp[cur + i - nc])
          if firstLine: px = ra
          else:
            let rb = int32(outp[prev + i])
            let rc = int32(outp[prev + i - nc])
            px = case pred
              of 1: ra
              of 2: rb
              of 3: rc
              of 4: ra + rb - rc
              of 5: ra + ashr(rb - rc, 1)
              of 6: rb + ashr(ra - rc, 1)
              else: ashr(ra + rb, 1)
        let d = bits.diff(p.tables[c])
        outp[cur + i] = uint16((px + d) and 0xFFFF)
    inc intervalRow
    if bits.fake > 64: raise malformed("lossless JPEG data is truncated")
  if p.pointTransform > 0:
    for v in outp.mitems: v = v shl p.pointTransform
  (f, outp)
