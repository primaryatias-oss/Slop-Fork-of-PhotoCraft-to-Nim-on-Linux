## Nikon Huffman-compressed NEF sensor data (port of `raw/src/nefc.rs`; TIFF compression 34713):
## "lossless compressed" and "lossy compressed" (type 1 and type 2), 12 and 14 bits.
##
## Format facts (Laurent Clévy's NEF description, Bill Claff's "NEF Compression", ExifTool's tag
## names, T.81 Annex F/H) and the Huffman tables (recovered by black-box analysis of CC0 samples,
## LightCraft storytold/lightcraft#86):
##
## * One strip; an MSB-first bit stream without byte stuffing, continuous across rows. Per pixel a
##   Huffman code gives the category `n`, followed by `n` additional bits.
## * Each pixel is predicted from the same-colour pixel two to the left; the first two pixels of a
##   row from the first two pixels of the previous row of the same parity, starting at the seeds
##   of maker note `0x0096`.
## * Lossless (`0x0096` version `0x46`): the decoded values are the samples. Lossy (`0x44`): they
##   index a curve. Type 1 (`0x44 0x10`) stores the whole curve, type 2 (`0x44 0x20` / `0x44
##   0x40`) stores `n` points spaced `2^bits / (n - 1)` codes apart, linearly interpolated.
##
## Not decoded (reported as unsupported): "lossy after split" files, other `0x0096` versions and
## bit depths other than 12 / 14.

import std/[options, strutils]
import ./tiff, ./sensor

const
  NikonCompression* = 34713'u32       ## TIFF Compression of Nikon's Huffman-coded NEF data
  NikonLinearizationTable* = 0x0096'u16 ## maker note NEFLinearizationTable
  Unseen = 255'u8                     ## a code word that never occurred in any analysed sample
  MaxLen = 9                          ## longest code word in the tables

type
  HuffTable = object
    counts: seq[uint8]
    symbols: seq[uint8]

  NefHuffman = object
    lut: seq[(uint8, uint8)] ## `(category, code length)`; length 0 = no such code

  NefBits = object
    ## MSB-first bit reader without byte stuffing; reads past the end return zero bits.
    src: ptr UncheckedArray[byte]
    len, pos: int
    acc: uint64
    n: int
    consumed: int

  EncodingKind* = enum ekLossless, ekLossy

  DecodeTable* = object
    ## Maker note `0x0096`, parsed.
    encoding*: EncodingKind
    curve*: seq[uint16]               ## lossy: the decoded values index this curve
    seeds*: array[2, array[2, int32]] ## predictor seeds `[row parity][column]`

let
  Lossless12 = HuffTable(counts: @[0'u8, 1, 4, 2, 3, 1, 2], symbols: @[5'u8, 4, 6, 3, 7, 2, 8, 1, 9, 0, 10, 11, 12])
  Lossless14 = HuffTable(counts: @[0'u8, 1, 4, 2, 2, 3, 1, 2], symbols: @[7'u8, 6, 8, 5, 9, 4, 10, 3, 11, 12, 2, 0, 1, 13, 14])
  Lossy12 = HuffTable(counts: @[0'u8, 1, 5, 1, 1, 1, 1, 1, 1], symbols: @[5'u8, 4, 3, 6, 2, 7, 1, 0, 8, 9, Unseen, 10])
  Lossy14 = HuffTable(counts: @[0'u8, 1, 4, 3, 1, 1, 1, 1, 1], symbols: @[5'u8, 6, 4, 7, 8, 3, 9, 2, 1, 0, 10, 11, 12])

proc newNefHuffman(t: HuffTable): NefHuffman =
  result.lut = newSeq[(uint8, uint8)](1 shl MaxLen)
  var code = 0'u32
  var k = 0
  for i, n in t.counts:
    let len = i + 1
    if len > MaxLen and n > 0: raise malformed("NEF Huffman code longer than the lookup")
    for _ in 0'u8 ..< n:
      if k >= t.symbols.len: raise malformed("NEF Huffman table is short of symbols")
      let s = t.symbols[k]
      inc k
      let first = int(code shl (MaxLen - len))
      let span = 1 shl (MaxLen - len)
      if s != Unseen:
        if first + span > result.lut.len: raise malformed("NEF Huffman table overflows")
        for e in first ..< first + span: result.lut[e] = (s, uint8(len))
      code += 1
    code = code shl 1

proc newNefBits(src: openArray[byte]): NefBits =
  NefBits(src: if src.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr src[0]) else: nil, len: src.len)

proc fill(b: var NefBits) {.inline.} =
  while b.n <= 56:
    let byt = if b.pos < b.len: uint64(b.src[b.pos]) else: 0'u64
    inc b.pos
    b.acc = b.acc or (byt shl (56 - b.n))
    b.n += 8

proc peek(b: var NefBits; k: int): uint32 {.inline.} =
  b.fill()
  uint32(b.acc shr (64 - k))

proc skip(b: var NefBits; k: int) {.inline.} =
  b.acc = b.acc shl k
  b.n -= k
  b.consumed += k

proc take(b: var NefBits; k: int): uint32 {.inline.} =
  ## `k` bits (k ≤ 32) as a number.
  if k == 0: return 0
  result = b.peek(k)
  b.skip(k)

proc diff(b: var NefBits; h: NefHuffman): Option[int32] {.inline.} =
  ## One coded difference, or none for a code word not in the table.
  let idx = int(b.peek(MaxLen))
  if idx >= h.lut.len: return
  let (cat, len) = h.lut[idx]
  if len == 0: return
  b.skip(int(len))
  if cat > 16: return
  let c = int(cat)
  let v = int32(b.take(c))
  # T.81 F.1.2.1: a leading 0 bit means a negative difference.
  some(if c > 0 and v < (1'i32 shl (c - 1)): v - (1'i32 shl c) + 1 else: v)

proc hex2(v: uint8): string = "0x" & toHex(v, 2).toLowerAscii

proc tableU16(t: openArray[byte]; le: bool; o: int): Option[uint16] =
  if o < 0 or o + 2 > t.len: return
  let (a, b) = (uint16(t[o]), uint16(t[o + 1]))
  some(if le: a or (b shl 8) else: (a shl 8) or b)

proc parseTable*(t: openArray[byte]; le: bool; bits: int): DecodeTable =
  ## Parses maker note `0x0096` for a `bits`-bit image; `le` is the maker note's byte order.
  template u16At(o: int): Option[uint16] = tableU16(t, le, o)
  template short(): ref RawError = malformed("NEF linearization table too short (" & $t.len & " bytes)")
  if bits != 12 and bits != 14: raise unsupported("Nikon compressed NEF with " & $bits & "-bit samples")
  if t.len < 2: raise short()
  let (v0, v1) = (t[0], t[1])
  for i in 0 .. 3:
    let v = u16At(2 + 2 * i)
    if v.isNone: raise short()
    result.seeds[i div 2][i mod 2] = int32(v.get)
  if v0 == 0x46: result.encoding = ekLossless
  elif v0 == 0x44 and v1 in [0x10'u8, 0x20, 0x40]:
    let n0 = u16At(10)
    if n0.isNone: raise short()
    let n = int(n0.get)
    if n < 2: raise malformed("NEF linearization curve of " & $n & " points")
    var points = newSeq[uint16](n)
    for i in 0 ..< n:
      let v = u16At(12 + 2 * i)
      if v.isNone: raise short()
      points[i] = v.get
    result.encoding = ekLossy
    if v1 == 0x10: result.curve = points
    else:
      let split = u16At(562).get(0)
      if split != 0: raise unsupported("Nikon \"lossy after split\" compressed NEF (split at row " & $split & ")")
      let range = 1 shl bits
      if n - 1 > range or range mod (n - 1) != 0:
        raise unsupported("Nikon compressed NEF: " & $n & "-point curve for " & $bits & "-bit data")
      let step = uint32(range div (n - 1))
      var curve = newSeqOfCap[uint16](range + 1)
      for i in 0 ..< n - 1:
        let (a, b) = (uint32(points[i]), uint32(points[i + 1]))
        for k in 0'u32 ..< step: curve.add uint16((a * (step - k) + b * k) div step)
      curve.add points[^1]
      result.curve = curve
  else: raise unsupported("Nikon compressed NEF version " & hex2(v0) & " " & hex2(v1))

proc satAdd32(a, b: int32): int32 {.inline.} =
  let s = int64(a) + int64(b)
  int32(clamp(s, int64(low(int32)), int64(high(int32))))

proc decodeNef*(src: openArray[byte]; w, h, bits: int; table: DecodeTable): seq[uint16] =
  ## Decodes a `w × h` Nikon Huffman-compressed strip with `bits`-bit samples.
  if bits != 12 and bits != 14: raise unsupported("Nikon compressed NEF with " & $bits & "-bit samples")
  if w <= 0 or h <= 0 or w > high(int) div h: raise malformed("NEF image is empty")
  let n = w * h
  # Every code word is at least 2 bits: a strip that can't hold the image is truncated (this also
  # bounds the allocation by the file size).
  if n div 4 > src.len:
    raise malformed("NEF: " & $src.len & " bytes of compressed data for " & $w & "x" & $h & " pixels")
  let huff = newNefHuffman(
    if table.encoding == ekLossless: (if bits == 12: Lossless12 else: Lossless14)
    else: (if bits == 12: Lossy12 else: Lossy14))
  let lossy = table.encoding == ekLossy
  let mx = if lossy: int32(min(table.curve.len, int(high(int32)))) - 1 else: (1'i32 shl bits) - 1
  result = newSeq[uint16](n)
  var stream = newNefBits(src)
  var vpred = table.seeds
  var clipped = 0
  for y in 0 ..< h:
    var hpred: array[2, int32]
    for x in 0 ..< w:
      let d0 = stream.diff(huff)
      if d0.isNone: raise malformed("NEF: invalid Huffman code in row " & $y)
      let d = d0.get
      var p: int32
      if x < 2:
        vpred[y and 1][x] = satAdd32(vpred[y and 1][x], d)
        hpred[x] = vpred[y and 1][x]
        p = vpred[y and 1][x]
      else:
        hpred[x and 1] = satAdd32(hpred[x and 1], d)
        p = hpred[x and 1]
      let v = clamp(p, 0'i32, mx)
      if v != p: inc clipped
      result[y * w + x] =
        if not lossy: uint16(v)
        elif int(v) < table.curve.len: table.curve[int(v)]
        else: 0'u16
    # The camera pads the strip; reading well past its end means the data is truncated or corrupt.
    if stream.consumed > src.len * 8 + 64:
      raise malformed("NEF: compressed data ends in row " & $y & " of " & $h)
  # A wrong table or a corrupt stream drifts out of range quickly; real files stay inside.
  if clipped > n div 1000 + 16: raise malformed("NEF: " & $clipped & " decoded values out of range")

proc readCompressed*(t: Tiff; raw: Ifd; note: Option[(Tiff, Ifd)]; limits: RawLimits): Plane =
  ## Reads the Huffman-compressed CFA image of `raw`. `note` is the Nikon maker note (its own TIFF
  ## and IFD 0), which holds the decode table.
  let w0 = t.tagUint(raw, TagImageWidth)
  if w0.isNone: raise malformed("raw image has no width")
  let h0 = t.tagUint(raw, TagImageLength)
  if h0.isNone: raise malformed("raw image has no height")
  let (width, height) = (int(w0.get), int(h0.get))
  let samples = t.tagUint(raw, TagSamplesPerPixel).get(1)
  if samples != 1: raise unsupported("Nikon compressed NEF with " & $samples & " samples per pixel")
  let bits = int(t.tagUint(raw, TagBitsPerSample).get(0))
  var tableBytes: Option[(seq[byte], bool)]
  if note.isSome:
    let (nt, m) = note.get
    let e = m.get(NikonLinearizationTable)
    if e.isSome:
      let b = nt.rawBytes(e.get)
      if b.isSome: tableBytes = some((b.get, nt.le))
  if tableBytes.isNone:
    raise unsupported("Nikon compressed NEF without a linearization table (maker note 0x96)")
  let table = parseTable(tableBytes.get[0], tableBytes.get[1], bits)
  limits.check(uint64(width), uint64(height), 2)
  # One strip in every sample; several would be consecutive parts of the same bit stream.
  let offsets = t.tagUints(raw, TagStripOffsets)
  let counts = t.tagUints(raw, TagStripByteCounts)
  if offsets.len == 0 or counts.len == 0: raise malformed("NEF: no image data")
  let start = int(offsets[0])
  let stop = min(int(offsets[^1]) + int(counts[^1]), t.data.len)
  if start >= stop or start > t.data.len: raise malformed("raw data lies outside the file")
  let data = decodeNef(t.data.toOpenArray(start, stop - 1), width, height, bits, table)
  Plane(width: width, height: height, samples: 1, bits: bits, data: data)
