## Synthetic raw-file generators for tests and fuzz seeds (port of `raw/src/testgen.rs`): a
## lossless-JPEG (T.81 process 14) encoder, a small TIFF writer, and DNG / CR2 / TIFF-EP / NEF /
## cRAW / RW2 / ORF / RAF builders. The files are written from the public specifications only
## and are meant to exercise the decoder, not to be camera-accurate.
##
## Test-only code: it assumes well-formed arguments (like the Rust module, which allows panics).
## Two outputs differ from the Rust generator byte for byte: deflate strips (this zlib encoder is
## not miniz_oxide) and lossy-JPEG tiles (this JPEG encoder is not the `image` crate's); both
## decode to the same kind of data.

import std/[options, algorithm, math, bitops]
import ../zlib
import ../codecs/image as cimage
import ../codecs/jpeg as cjpeg

# ---------------------------------------------------------------- lossless JPEG

const Lengths: array[17, uint8] = [2'u8, 3, 3, 3, 4, 4, 5, 5, 6, 6, 7, 8, 9, 10, 11, 12, 13]
  ## Huffman code lengths assigned to categories in decreasing frequency order. Kraft sum < 1 and
  ## no code is all ones.

proc category(d: int32): uint32 =
  if d == 0: 0'u32
  elif d == 32768: 16'u32
  else: uint32(32 - countLeadingZeroBits(uint32(abs(int64(d)))))

type BitWriter = object
  outp: seq[byte]
  acc, n: uint32

proc put(w: var BitWriter; v, bits: uint32) =
  var i = int(bits) - 1
  while i >= 0:
    w.acc = (w.acc shl 1) or ((v shr uint32(i)) and 1)
    inc w.n
    if w.n == 8:
      let b = byte(w.acc and 0xFF)
      w.outp.add b
      if b == 0xFF: w.outp.add 0
      w.acc = 0
      w.n = 0
    dec i

proc flush(w: var BitWriter) =
  if w.n > 0:
    let pad = 8 - w.n
    w.put((1'u32 shl pad) - 1, pad)

proc be16(v: int): array[2, byte] = [byte((v shr 8) and 0xFF), byte(v and 0xFF)]

proc lj92Encode*(samples: openArray[uint16]; width, height, components: int; precision, predictor: uint8;
                 restartRows = none(int)): seq[byte] =
  ## Encodes `samples` (raster order, `components` interleaved, values below `2^precision`) as a
  ## lossless JPEG with the given predictor (1–7) and an optional restart interval in rows.
  doAssert samples.len == width * height * components
  let row = width * components
  let interval = restartRows.get(high(int))
  # Differences per sample, following T.81 H.1.2.1 prediction rules.
  var diffs = newSeqOfCap[int32](samples.len)
  let initial = 1'i32 shl (int(precision) - 1)
  for y in 0 ..< height:
    let firstLine = y mod interval == 0
    for x in 0 ..< width:
      for c in 0 ..< components:
        let i = y * row + x * components + c
        let px =
          if x == 0:
            (if firstLine: initial else: int32(samples[i - row]))
          elif firstLine: int32(samples[i - components])
          else:
            let ra = int32(samples[i - components])
            let rb = int32(samples[i - row])
            let rc = int32(samples[i - row - components])
            case predictor
            of 1: ra
            of 2: rb
            of 3: rc
            of 4: ra + rb - rc
            of 5: ra + ((rb - rc) shr 1)
            of 6: rb + ((ra - rc) shr 1)
            else: (ra + rb) shr 1
        var d = (int32(samples[i]) - px) and 0xFFFF
        if d > 32768: d -= 65536
        diffs.add d
  # Table: categories ordered by frequency get increasing code lengths.
  var freq: array[17, int]
  for d in diffs: inc freq[category(d)]
  var order = newSeq[int](17)
  for i in 0 ..< 17: order[i] = i
  order.sort(proc(a, b: int): int = cmp(freq[b], freq[a])) # stable, descending frequency
  var lenOf: array[17, uint8]
  for rank, c in order: lenOf[c] = Lengths[rank]
  var counts: array[16, uint8]
  var vals: seq[byte]
  var codeOf: array[17, (uint32, uint32)]
  var code = 0'u32
  for len in 1'u8 .. 16'u8:
    for c in 0 ..< 17:
      if lenOf[c] == len:
        inc counts[int(len) - 1]
        vals.add byte(c)
        codeOf[c] = (code, uint32(len))
        inc code
    code = code shl 1
  var o = @[0xFF'u8, 0xD8]
  # SOF3
  o.add [0xFF'u8, 0xC3]
  o.add be16(8 + 3 * components)
  o.add precision
  o.add be16(height)
  o.add be16(width)
  o.add byte(components)
  for c in 0 ..< components: o.add [byte(c + 1), 0x11'u8, 0]
  # DHT (one table, id 0)
  o.add [0xFF'u8, 0xC4]
  o.add be16(2 + 17 + vals.len)
  o.add 0x00'u8
  o.add counts
  o.add vals
  if restartRows.isSome:
    o.add [0xFF'u8, 0xDD, 0, 4]
    o.add be16(restartRows.get * width)
  # SOS
  o.add [0xFF'u8, 0xDA]
  o.add be16(6 + 2 * components)
  o.add byte(components)
  for c in 0 ..< components: o.add [byte(c + 1), 0x00'u8]
  o.add [predictor, 0'u8, 0]
  var w = BitWriter(outp: o)
  var rst = 0'u8
  let every = if interval > high(int) div max(row, 1): high(int) else: interval * row
  for i, d in diffs:
    if i > 0 and i mod every == 0:
      w.flush()
      w.outp.add [0xFF'u8, 0xD0'u8 + rst]
      rst = (rst + 1) mod 8
    let s = category(d)
    let (c, l) = codeOf[s]
    w.put(c, l)
    if s in 1'u32 ..< 16'u32:
      let extra = if d < 0: cast[uint32](d - 1) and ((1'u32 shl s) - 1) else: uint32(d)
      w.put(extra, s)
  w.flush()
  result = w.outp
  result.add [0xFF'u8, 0xD9]

# ---------------------------------------------------------------- TIFF writer

type
  ValKind* = enum
    vkByte, vkAscii, vkShort, vkLong, vkDouble, vkRational, vkSRational, vkUndefined, vkBlobs, vkIfds, vkIfdBytes

  Val* = object
    ## A TIFF entry value.
    case kind*: ValKind
    of vkByte, vkUndefined: bytes*: seq[byte]
    of vkAscii: str*: string
    of vkShort: shorts*: seq[uint16]
    of vkLong: longs*: seq[uint32]
    of vkDouble: doubles*: seq[float64]
    of vkRational: rats*: seq[(uint32, uint32)]
    of vkSRational: srats*: seq[(int32, int32)]
    of vkBlobs, vkIfds: refs*: seq[int] ## offsets of these blobs / IFDs (LONG)
    of vkIfdBytes: ifd*: int             ## an UNDEFINED value whose bytes are this IFD (a maker note)

  Entry* = (uint16, Val)

  TiffBuilder* = object
    ## A TIFF file under construction.
    bigEndian*: bool
    ifds*: seq[seq[Entry]]  ## entries per IFD (sorted by tag on write)
    chain*: seq[int]        ## the main IFD chain, by index into `ifds`
    blobs*: seq[seq[byte]]
    cr2RawIfd*: Option[int] ## write a CR2 header pointing at this IFD as the raw IFD

proc vByte*(b: openArray[byte]): Val = Val(kind: vkByte, bytes: @b)
proc vUndefined*(b: openArray[byte]): Val = Val(kind: vkUndefined, bytes: @b)
proc vAscii*(s: string): Val = Val(kind: vkAscii, str: s)
proc vShort*(s: openArray[uint16]): Val = Val(kind: vkShort, shorts: @s)
proc vLong*(l: openArray[uint32]): Val = Val(kind: vkLong, longs: @l)
proc vDouble*(d: openArray[float64]): Val = Val(kind: vkDouble, doubles: @d)
proc vRational*(r: openArray[(uint32, uint32)]): Val = Val(kind: vkRational, rats: @r)
proc vSRational*(r: openArray[(int32, int32)]): Val = Val(kind: vkSRational, srats: @r)
proc vBlobs*(b: openArray[int]): Val = Val(kind: vkBlobs, refs: @b)
proc vIfds*(i: openArray[int]): Val = Val(kind: vkIfds, refs: @i)
proc vIfdBytes*(i: int): Val = Val(kind: vkIfdBytes, ifd: i)

proc ifd*(t: var TiffBuilder; entries: seq[Entry]): int =
  t.ifds.add entries
  t.ifds.len - 1

proc blob*(t: var TiffBuilder; data: seq[byte]): int =
  t.blobs.add data
  t.blobs.len - 1

proc ifdSize(t: TiffBuilder; i: int): int = 2 + 12 * t.ifds[i].len + 4

proc valSize(t: TiffBuilder; v: Val): int =
  case v.kind
  of vkByte, vkUndefined: v.bytes.len
  of vkAscii: v.str.len + 1
  of vkShort: 2 * v.shorts.len
  of vkLong: 4 * v.longs.len
  of vkDouble: 8 * v.doubles.len
  of vkRational: 8 * v.rats.len
  of vkSRational: 8 * v.srats.len
  of vkBlobs, vkIfds: 4 * v.refs.len
  of vkIfdBytes: t.ifdSize(v.ifd)

proc sorted(t: TiffBuilder; i: int): seq[Entry] =
  result = t.ifds[i]
  result.sort(proc(a, b: Entry): int = cmp(a[0], b[0]))

proc b16(be: bool; v: uint16): array[2, byte] =
  if be: [byte(v shr 8), byte(v and 0xFF)] else: [byte(v and 0xFF), byte(v shr 8)]

proc b32(be: bool; v: uint32): array[4, byte] =
  if be: [byte(v shr 24), byte((v shr 16) and 0xFF), byte((v shr 8) and 0xFF), byte(v and 0xFF)]
  else: [byte(v and 0xFF), byte((v shr 8) and 0xFF), byte((v shr 16) and 0xFF), byte(v shr 24)]

proc b64(be: bool; v: uint64): array[8, byte] =
  for k in 0 .. 7:
    let sh = if be: 8 * (7 - k) else: 8 * k
    result[k] = byte((v shr uint64(sh)) and 0xFF)

proc encode(t: TiffBuilder; v: Val; blobPos, ifdPos: openArray[int]): (uint16, uint32, seq[byte]) =
  let be = t.bigEndian
  case v.kind
  of vkByte: (1'u16, uint32(v.bytes.len), v.bytes)
  of vkUndefined: (7'u16, uint32(v.bytes.len), v.bytes)
  of vkAscii:
    var b: seq[byte]
    for c in v.str: b.add byte(c)
    b.add 0
    (2'u16, uint32(b.len), b)
  of vkShort:
    var b: seq[byte]
    for x in v.shorts: b.add b16(be, x)
    (3'u16, uint32(v.shorts.len), b)
  of vkLong:
    var b: seq[byte]
    for x in v.longs: b.add b32(be, x)
    (4'u16, uint32(v.longs.len), b)
  of vkDouble:
    var b: seq[byte]
    for x in v.doubles: b.add b64(be, cast[uint64](x))
    (12'u16, uint32(v.doubles.len), b)
  of vkRational:
    var b: seq[byte]
    for (n, d) in v.rats:
      b.add b32(be, n)
      b.add b32(be, d)
    (5'u16, uint32(v.rats.len), b)
  of vkSRational:
    var b: seq[byte]
    for (n, d) in v.srats:
      b.add b32(be, cast[uint32](n))
      b.add b32(be, cast[uint32](d))
    (10'u16, uint32(v.srats.len), b)
  of vkBlobs:
    var b: seq[byte]
    for i in v.refs: b.add b32(be, uint32(blobPos[i]))
    (4'u16, uint32(v.refs.len), b)
  of vkIfds:
    var b: seq[byte]
    for k in v.refs: b.add b32(be, uint32(ifdPos[k]))
    (4'u16, uint32(v.refs.len), b)
  of vkIfdBytes: (7'u16, uint32(t.ifdSize(v.ifd)), newSeq[byte](8))

proc build*(t: TiffBuilder): seq[byte] =
  let header = if t.cr2RawIfd.isSome: 16 else: 8
  # Layout: each IFD, then its overflow values; then the blobs.
  var pos = header
  var ifdPos = newSeq[int](t.ifds.len)
  var valPos = newSeq[seq[int]](t.ifds.len)
  # Maker-note IFDs are written inside their owner's value area.
  var embedded: seq[int]
  for es in t.ifds:
    for (_, v) in es:
      if v.kind == vkIfdBytes: embedded.add v.ifd
  for i in 0 ..< t.ifds.len:
    if i in embedded: continue
    ifdPos[i] = pos
    pos += t.ifdSize(i)
    var vp: seq[int]
    for (_, v) in t.sorted(i):
      let size = t.valSize(v)
      if size > 4:
        vp.add pos
        if v.kind == vkIfdBytes:
          let e = v.ifd
          ifdPos[e] = pos
          # The embedded IFD's own values follow it.
          let inner = pos + t.ifdSize(e)
          var ip: seq[int]
          var q = inner
          for (_, iv) in t.sorted(e):
            let s = t.valSize(iv)
            if s > 4:
              ip.add q
              q += s + (s and 1)
            else: ip.add 0
          valPos[e] = ip
          pos = q
        else:
          pos += size + (size and 1)
      else: vp.add 0
    valPos[i] = vp
  var blobPos: seq[int]
  for b in t.blobs:
    blobPos.add pos
    pos += b.len + (b.len and 1)
  var o = newSeq[byte](pos)
  let be = t.bigEndian
  proc w16(o: var seq[byte]; at: int; v: uint16) =
    let b = b16(be, v)
    o[at] = b[0]
    o[at + 1] = b[1]
  proc w32(o: var seq[byte]; at: int; v: uint32) =
    let b = b32(be, v)
    for k in 0 .. 3: o[at + k] = b[k]
  let magic = if be: "MM\0*" else: "II*\0"
  for k in 0 .. 3: o[k] = byte(magic[k])
  w32(o, 4, uint32(ifdPos[t.chain[0]]))
  if t.cr2RawIfd.isSome:
    let m = "CR\x02\0"
    for k in 0 .. 3: o[8 + k] = byte(m[k])
    w32(o, 12, uint32(ifdPos[t.cr2RawIfd.get]))
  for i in 0 ..< t.ifds.len:
    let at = ifdPos[i]
    let entries = t.sorted(i)
    w16(o, at, uint16(entries.len))
    for k, (tag, v) in entries:
      let e = at + 2 + 12 * k
      let (typ, count, bytes) = t.encode(v, blobPos, ifdPos)
      w16(o, e, tag)
      w16(o, e + 2, typ)
      w32(o, e + 4, count)
      if bytes.len <= 4:
        for q in 0 ..< bytes.len: o[e + 8 + q] = bytes[q]
      else:
        let vp = valPos[i][k]
        w32(o, e + 8, uint32(vp))
        if v.kind != vkIfdBytes:
          for q in 0 ..< bytes.len: o[vp + q] = bytes[q]
    var next = 0
    let p = t.chain.find(i)
    if p >= 0 and p + 1 < t.chain.len: next = ifdPos[t.chain[p + 1]]
    w32(o, at + 2 + 12 * entries.len, uint32(next))
  for k, b in t.blobs:
    for q in 0 ..< b.len: o[blobPos[k] + q] = b[q]
  o

proc satI32(v: float64): int32 =
  if v != v: 0'i32
  elif v >= 2147483647.0: high(int32)
  elif v <= -2147483648.0: low(int32)
  else: int32(v)

proc satU32(v: float64): uint32 =
  if v != v or v <= 0: 0'u32
  elif v >= 4294967295.0: high(uint32)
  else: uint32(v)

proc srat(v: float64): (int32, int32) = (satI32(round(v * 10000.0)), 10000'i32)
proc urat(v: float64): (uint32, uint32) = (satU32(round(v * 10000.0)), 10000'u32)

proc le16s(v: openArray[uint16]): seq[byte] =
  for x in v: result.add b16(false, x)

# ---------------------------------------------------------------- DNG

type
  DngStorageKind* = enum
    dsStrips          ## one strip per `rows` rows, uncompressed at `bits` per sample
    dsLossyJpegTiles  ## compression 34892: baseline JPEG tiles, 8-bit at tile size (quality 60)
    dsDeflateStrips   ## compression 8: zlib streams with the TIFF predictor (1 or 2)
    dsLj92Tiles       ## lossless-JPEG tiles (2 components per JPEG row, as Adobe writes CFA tiles)
    dsLj92Strips      ## lossless-JPEG strips of `rows` rows, 1 component
    dsJxlStrips       ## JPEG XL strips (compression 52546) from `jxlSegments`
    dsJxlTiles        ## JPEG XL tiles from `jxlSegments`, row-major

  DngStorage* = object
    ## How the DNG stores its raw data.
    kind*: DngStorageKind
    rows*: int             ## strips
    width*, height*: int   ## tiles
    predictor*: uint16     ## deflate strips

  DngSpec* = object
    ## A synthetic DNG.
    width*, height*: int
    samples*: int        ## 1 (CFA, or monochrome LinearRaw with `monochrome`) or 3 (LinearRaw)
    monochrome*: bool
    data*: seq[uint16]   ## row-major samples (`width * height * samples`)
    bits*: uint16
    cfa*: array[4, uint8]
    storage*: DngStorage
    bigEndian*: bool
    black*: seq[uint32]
    blackRepeat*: (uint16, uint16)
    white*: uint32
    activeArea*: Option[array[4, uint32]] ## top, left, bottom, right
    defaultCrop*: Option[(array[2, uint32], array[2, uint32])] ## origin (x, y), size (w, h)
    defaultCropOriginDouble*: Option[array[2, float64]]
    defaultCropSizeOnly*: Option[array[2, uint32]]
    linearization*: Option[seq[uint16]]
    colorMatrix1*, colorMatrix2*: Option[(uint16, array[9, float64])] ## (illuminant, matrix)
    forwardMatrix1*, forwardMatrix2*: Option[array[9, float64]]
    asShotNeutral*: Option[array[3, float64]]
    baselineExposure*: Option[float64]
    opcodeList2*: Option[seq[byte]]
    jxlSegments*: seq[seq[byte]]
    profileGainTableMap*: Option[seq[byte]]
    orientation*: uint16
    make*, model*: string
    ifd0Extra*: seq[Entry] ## extra IFD0 entries, written after the generated ones

proc strips*(rows: int): DngStorage = DngStorage(kind: dsStrips, rows: rows)
proc lossyJpegTiles*(width, height: int): DngStorage = DngStorage(kind: dsLossyJpegTiles, width: width, height: height)
proc deflateStrips*(rows: int; predictor: uint16): DngStorage = DngStorage(kind: dsDeflateStrips, rows: rows, predictor: predictor)
proc lj92Tiles*(width, height: int): DngStorage = DngStorage(kind: dsLj92Tiles, width: width, height: height)
proc lj92Strips*(rows: int): DngStorage = DngStorage(kind: dsLj92Strips, rows: rows)
proc jxlStrips*(rows: int): DngStorage = DngStorage(kind: dsJxlStrips, rows: rows)
proc jxlTiles*(width, height: int): DngStorage = DngStorage(kind: dsJxlTiles, width: width, height: height)

proc dngCfa*(width, height: int; data: seq[uint16]): DngSpec =
  ## A CFA DNG with default metadata around `data` (RGGB, 16-bit, one strip).
  DngSpec(width: width, height: height, samples: 1, data: data, bits: 16, cfa: [0'u8, 1, 1, 2],
          storage: strips(height), black: @[0'u32], blackRepeat: (1'u16, 1'u16), white: 65535,
          orientation: 1, make: "Photocraft", model: "Synthetic")

proc dngMonochrome*(width, height: int; data: seq[uint16]): DngSpec =
  ## A monochrome (one-sample LinearRaw) DNG around `data`: no CFA tags and no colour matrices.
  result = dngCfa(width, height, data)
  result.monochrome = true

proc isCfa(d: DngSpec): bool = d.samples == 1 and not d.monochrome

proc pack(d: DngSpec; r0, r1: int): seq[byte] =
  let n = d.width * d.samples
  template vals: untyped = d.data.toOpenArray(r0 * n, r1 * n - 1)
  case d.bits
  of 8:
    for v in vals: result.add byte(v and 0xFF)
  of 16:
    for v in vals: result.add b16(d.bigEndian, v)
  else:
    # MSB-first bit packing, each row padded to a byte.
    let b = uint32(d.bits)
    for row in r0 ..< r1:
      var acc = 0'u64
      var have = 0'u32
      for i in 0 ..< n:
        acc = (acc shl b) or uint64(d.data[row * n + i])
        have += b
        while have >= 8:
          have -= 8
          result.add byte((acc shr have) and 0xFF)
      if have > 0: result.add byte((acc shl (8 - have)) and 0xFF)

proc build*(d: DngSpec): seq[byte] =
  var t = TiffBuilder(bigEndian: d.bigEndian)
  var bitsV = newSeq[uint16](d.samples)
  for v in bitsV.mitems: v = d.bits
  var whiteV = newSeq[uint32](d.samples)
  for v in whiteV.mitems: v = d.white
  var raw: seq[Entry] = @[
    (254'u16, vLong([0'u32])), (256'u16, vLong([uint32(d.width)])), (257'u16, vLong([uint32(d.height)])),
    (258'u16, vShort(bitsV)), (262'u16, vShort([if d.isCfa: 32803'u16 else: 34892'u16])),
    (277'u16, vShort([uint16(d.samples)])), (284'u16, vShort([1'u16])), (50717'u16, vLong(whiteV)),
    (50713'u16, vShort([d.blackRepeat[0], d.blackRepeat[1]])), (50714'u16, vLong(d.black))]
  if d.isCfa:
    raw.add (33421'u16, vShort([2'u16, 2]))
    raw.add (33422'u16, vByte(d.cfa))
    raw.add (50710'u16, vByte([0'u8, 1, 2]))
    raw.add (50711'u16, vShort([1'u16]))
  let st = d.storage
  case st.kind
  of dsDeflateStrips:
    var offs: seq[int]
    var lens: seq[uint32]
    var y = 0
    while y < d.height:
      let r = min(y + st.rows, d.height) - y
      let n = d.width * d.samples
      var v = d.data[y * n ..< (y + r) * n]
      if st.predictor == 2:
        # Horizontal differencing on the sample values, before byte packing.
        let plane = if d.samples mod 3 == 0: 3 else: 1
        for rr in 0 ..< r:
          for i in countdown(n - 1, plane):
            v[rr * n + i] = v[rr * n + i] - v[rr * n + i - plane]
      var bytes: seq[byte]
      case d.bits
      of 8:
        for x in v: bytes.add byte(x and 0xFF)
      of 16:
        for x in v: bytes.add b16(d.bigEndian, x)
      else:
        for x in v: bytes.add b16(false, x)
      let z = zlibCompress(bytes, 6)
      lens.add uint32(z.len)
      offs.add t.blob(z)
      y += st.rows
    raw.add (259'u16, vShort([8'u16]))
    raw.add (317'u16, vShort([st.predictor]))
    raw.add (278'u16, vLong([uint32(st.rows)]))
    raw.add (273'u16, vBlobs(offs))
    raw.add (279'u16, vLong(lens))
  of dsStrips:
    var offs: seq[int]
    var lens: seq[uint32]
    var y = 0
    while y < d.height:
      let b = d.pack(y, min(y + st.rows, d.height))
      lens.add uint32(b.len)
      offs.add t.blob(b)
      y += st.rows
    raw.add (259'u16, vShort([1'u16]))
    raw.add (278'u16, vLong([uint32(st.rows)]))
    raw.add (273'u16, vBlobs(offs))
    raw.add (279'u16, vLong(lens))
  of dsLossyJpegTiles:
    var offs: seq[int]
    var lens: seq[uint32]
    let (tw, th, s) = (st.width, st.height, d.samples)
    var ty = 0
    while ty < d.height:
      var tx = 0
      while tx < d.width:
        # Edge tiles are padded by repeating the last row / column. A 3-sample (LinearRaw) spec
        # writes RGB tiles, a CFA spec gray ones.
        var tile = newSeq[byte](tw * th * s)
        for y in 0 ..< th:
          for x in 0 ..< tw:
            let sy = min(ty + y, d.height - 1)
            let sx = min(tx + x, d.width - 1)
            for c in 0 ..< s: tile[(y * tw + x) * s + c] = byte(d.data[(sy * d.width + sx) * s + c] shr 8)
        let layout = if s == 3: clRgb else: clGray
        let img = imageFromRaw(tw, th, layout, isU8, tile)
        let j = encodeJpeg(img, layout, EncodeOptions(jpegQuality: 60, jpegChromaSubsampling: true))
        lens.add uint32(j.len)
        offs.add t.blob(j)
        tx += tw
      ty += th
    raw.add (259'u16, vShort([34892'u16]))
    raw.add (322'u16, vLong([uint32(tw)]))
    raw.add (323'u16, vLong([uint32(th)]))
    raw.add (324'u16, vBlobs(offs))
    raw.add (325'u16, vLong(lens))
  of dsLj92Strips:
    var offs: seq[int]
    var lens: seq[uint32]
    var y = 0
    while y < d.height:
      let r = min(y + st.rows, d.height) - y
      let n = d.width * d.samples
      let j = lj92Encode(d.data.toOpenArray(y * n, (y + r) * n - 1), d.width, r, d.samples, uint8(d.bits), 1)
      lens.add uint32(j.len)
      offs.add t.blob(j)
      y += st.rows
    raw.add (259'u16, vShort([7'u16]))
    raw.add (278'u16, vLong([uint32(st.rows)]))
    raw.add (273'u16, vBlobs(offs))
    raw.add (279'u16, vLong(lens))
  of dsLj92Tiles:
    var offs: seq[int]
    var lens: seq[uint32]
    let (tw, th, s) = (st.width, st.height, d.samples)
    var ty = 0
    while ty < d.height:
      var tx = 0
      while tx < d.width:
        # Edge tiles are padded by repeating the last row / column.
        var tile = newSeq[uint16](tw * th * s)
        for y in 0 ..< th:
          for x in 0 ..< tw:
            let sy = min(ty + y, d.height - 1)
            let sx = min(tx + x, d.width - 1)
            for c in 0 ..< s: tile[(y * tw + x) * s + c] = d.data[(sy * d.width + sx) * s + c]
        # CFA tiles as Adobe writes them: half width, 2 components.
        let j = if s == 1 and tw mod 2 == 0: lj92Encode(tile, tw div 2, th, 2, uint8(d.bits), 1)
                else: lj92Encode(tile, tw, th, s, uint8(d.bits), 1)
        lens.add uint32(j.len)
        offs.add t.blob(j)
        tx += tw
      ty += th
    raw.add (259'u16, vShort([7'u16]))
    raw.add (322'u16, vLong([uint32(tw)]))
    raw.add (323'u16, vLong([uint32(th)]))
    raw.add (324'u16, vBlobs(offs))
    raw.add (325'u16, vLong(lens))
  of dsJxlStrips, dsJxlTiles:
    var offs: seq[int]
    var lens: seq[uint32]
    for j in d.jxlSegments:
      lens.add uint32(j.len)
      offs.add t.blob(j)
    raw.add (259'u16, vShort([52546'u16]))
    if st.kind == dsJxlStrips:
      raw.add (278'u16, vLong([uint32(st.rows)]))
      raw.add (273'u16, vBlobs(offs))
      raw.add (279'u16, vLong(lens))
    else:
      raw.add (322'u16, vLong([uint32(st.width)]))
      raw.add (323'u16, vLong([uint32(st.height)]))
      raw.add (324'u16, vBlobs(offs))
      raw.add (325'u16, vLong(lens))
  if d.activeArea.isSome: raw.add (50829'u16, vLong(d.activeArea.get))
  if d.defaultCrop.isSome:
    let (o, s) = d.defaultCrop.get
    let origin = if d.defaultCropOriginDouble.isSome: vDouble(d.defaultCropOriginDouble.get) else: vLong(o)
    raw.add (50719'u16, origin)
    raw.add (50720'u16, vLong(s))
  elif d.defaultCropOriginDouble.isSome:
    raw.add (50719'u16, vDouble(d.defaultCropOriginDouble.get))
  elif d.defaultCropSizeOnly.isSome:
    raw.add (50720'u16, vLong(d.defaultCropSizeOnly.get))
  if d.opcodeList2.isSome: raw.add (51009'u16, vUndefined(d.opcodeList2.get))
  if d.linearization.isSome: raw.add (50712'u16, vShort(d.linearization.get))
  let rawIfd = t.ifd(raw)
  # IFD0: a tiny RGB thumbnail plus the DNG metadata.
  var thumbData = newSeq[byte](12)
  for v in thumbData.mitems: v = 128
  let thumb = t.blob(thumbData)
  var ifd0: seq[Entry] = @[
    (254'u16, vLong([1'u32])), (256'u16, vLong([2'u32])), (257'u16, vLong([2'u32])), (258'u16, vShort([8'u16, 8, 8])),
    (259'u16, vShort([1'u16])), (262'u16, vShort([2'u16])), (271'u16, vAscii(d.make)), (272'u16, vAscii(d.model)),
    (273'u16, vBlobs([thumb])), (274'u16, vShort([d.orientation])), (277'u16, vShort([3'u16])),
    (278'u16, vLong([2'u32])), (279'u16, vLong([12'u32])), (330'u16, vIfds([rawIfd])),
    (50706'u16, vByte([1'u8, 4, 0, 0])), (50708'u16, vAscii(d.make & " " & d.model))]
  proc m(v: array[9, float64]): Val =
    var r: seq[(int32, int32)]
    for x in v: r.add srat(x)
    vSRational(r)
  if d.colorMatrix1.isSome:
    ifd0.add (50721'u16, m(d.colorMatrix1.get[1]))
    ifd0.add (50778'u16, vShort([d.colorMatrix1.get[0]]))
  if d.colorMatrix2.isSome:
    ifd0.add (50722'u16, m(d.colorMatrix2.get[1]))
    ifd0.add (50779'u16, vShort([d.colorMatrix2.get[0]]))
  if d.forwardMatrix1.isSome: ifd0.add (50964'u16, m(d.forwardMatrix1.get))
  if d.forwardMatrix2.isSome: ifd0.add (50965'u16, m(d.forwardMatrix2.get))
  if d.asShotNeutral.isSome:
    let n = d.asShotNeutral.get
    ifd0.add (50728'u16, vRational([urat(n[0]), urat(n[1]), urat(n[2])]))
  if d.baselineExposure.isSome: ifd0.add (50730'u16, vSRational([srat(d.baselineExposure.get)]))
  if d.profileGainTableMap.isSome: ifd0.add (52525'u16, vUndefined(d.profileGainTableMap.get))
  for e in d.ifd0Extra: ifd0.add e
  let i0 = t.ifd(ifd0)
  t.chain = @[i0]
  t.build()

proc gainMapOpcodeList*(area: array[4, uint32]; pitch: uint32; gains: array[2, array[2, float32]]): seq[byte] =
  ## An OpcodeList2 with one GainMap over active-area rectangle `area` (top, left, bottom, right),
  ## every `pitch` rows and columns, from a 2×2 grid of gains spanning the image.
  var p: seq[byte]
  for v in [area[0], area[1], area[2], area[3], 0'u32, 1, pitch, pitch, 2, 2]: p.add b32(true, v)
  for v in [1.0, 1.0, 0.0, 0.0]: p.add b64(true, cast[uint64](v))
  p.add b32(true, 1)
  for r in gains:
    for g in r: p.add b32(true, cast[uint32](g))
  result = @(b32(true, 1))
  for v in [9'u32, 0x0103_0000'u32, 1, uint32(p.len)]: result.add b32(true, v)
  result.add p

# ---------------------------------------------------------------- CR2

type Cr2Spec* = object
  ## A synthetic CR2: one lossless JPEG with `components` components, cut into vertical slices,
  ## plus a Canon maker note with SensorInfo and ColorData.
  width*, height*: int
  data*: seq[uint16]        ## sensor samples, row-major
  precision*: uint8
  components*: int
  slices*: seq[int]         ## slice widths (all but the last equal); empty = no slice tag
  borders*: Option[array[4, uint16]] ## left, top, right, bottom (inclusive), as in SensorInfo
  wbRggb*: Option[array[4, uint16]]  ## as-shot RGGB levels at ColorData word offset 0x47
  orientation*: uint16
  modelId*: Option[uint32]  ## Canon model ID written to MakerNote tag 0x0010

proc build*(c: Cr2Spec): seq[byte] =
  let (w, h) = (c.width, c.height)
  # Slice the sensor data into the JPEG sample order.
  let widths = if c.slices.len == 0: @[w] else: c.slices
  var total = 0
  for sw in widths: total += sw
  doAssert total == w
  var sq = newSeqOfCap[uint16](w * h)
  var x0 = 0
  for sw in widths:
    for y in 0 ..< h:
      for k in 0 ..< sw: sq.add c.data[y * w + x0 + k]
    x0 += sw
  let comps = c.components
  doAssert w mod comps == 0
  let jpeg = lj92Encode(sq, w div comps, h, comps, c.precision, 1)
  var t = TiffBuilder()
  let jb = t.blob(jpeg)
  var raw: seq[Entry] = @[(259'u16, vShort([6'u16])), (273'u16, vBlobs([jb])), (279'u16, vLong([uint32(jpeg.len)]))]
  if c.slices.len > 0:
    let n = widths.len - 1
    raw.add (50752'u16, vShort([uint16(n), uint16(widths[0]), uint16(widths[n])]))
  let rawIfd = t.ifd(raw)
  var mn: seq[Entry]
  if c.modelId.isSome: mn.add (0x0010'u16, vLong([c.modelId.get]))
  if c.borders.isSome:
    let b = c.borders.get
    mn.add (0x00E0'u16, vShort([34'u16, uint16(w), uint16(h), 0, 0, b[0], b[1], b[2], b[3], 0, 0, 0, 0, 0, 0, 0, 0]))
  if c.wbRggb.isSome:
    # ColorData (0x4001) as UNDEFINED bytes that are 16-bit words; the PowerShot version's
    # WB_RGGBLevelsAsShot sits at word offset 0x47.
    var cd = newSeq[byte](2 * 1273)
    for i, v in c.wbRggb.get:
      let b = b16(false, v)
      cd[2 * (0x47 + i)] = b[0]
      cd[2 * (0x47 + i) + 1] = b[1]
    mn.add (0x4001'u16, vUndefined(cd))
  var exif: seq[Entry] = @[(33434'u16, vRational([(1'u32, 100'u32)]))]
  if mn.len > 0:
    let mi = t.ifd(mn)
    exif.add (37500'u16, vIfdBytes(mi))
  let exifIfd = t.ifd(exif)
  var thumbData = newSeq[byte](12)
  for v in thumbData.mitems: v = 128
  let thumb = t.blob(thumbData)
  let ifd0 = t.ifd(@[
    (256'u16, vLong([2'u32])), (257'u16, vLong([2'u32])), (259'u16, vShort([1'u16])), (262'u16, vShort([2'u16])),
    (271'u16, vAscii("Canon")), (272'u16, vAscii("Canon EOS Synthetic")), (273'u16, vBlobs([thumb])),
    (274'u16, vShort([c.orientation])), (277'u16, vShort([3'u16])), (279'u16, vLong([12'u32])),
    (34665'u16, vIfds([exifIfd]))])
  let ifd1 = t.ifd(@[(256'u16, vLong([1'u32]))])
  let ifd2 = t.ifd(@[(256'u16, vLong([1'u32]))])
  t.chain = @[ifd0, ifd1, ifd2, rawIfd]
  t.cr2RawIfd = some(rawIfd)
  t.build()

# ---------------------------------------------------------------- TIFF/EP

proc tiffEp*(make: string; width, height: int; data: openArray[uint16]; cfa: array[4, uint8]; bits: uint16;
             extraRawTags: seq[Entry] = @[]): seq[byte] =
  ## A synthetic uncompressed TIFF/EP raw (NEF / ARW-like): IFD0 with Make, the CFA image in a
  ## SubIFD with 16-bit samples.
  var t = TiffBuilder()
  let strip = t.blob(le16s(data))
  var raw: seq[Entry] = @[
    (254'u16, vLong([0'u32])), (256'u16, vLong([uint32(width)])), (257'u16, vLong([uint32(height)])),
    (258'u16, vShort([bits])), (259'u16, vShort([1'u16])), (262'u16, vShort([32803'u16])), (273'u16, vBlobs([strip])),
    (277'u16, vShort([1'u16])), (278'u16, vLong([uint32(height)])), (279'u16, vLong([uint32(data.len * 2)])),
    (33421'u16, vShort([2'u16, 2])), (33422'u16, vByte(cfa))]
  for e in extraRawTags: raw.add e
  let rawIfd = t.ifd(raw)
  var thumbData = newSeq[byte](12)
  for v in thumbData.mitems: v = 128
  let thumb = t.blob(thumbData)
  let ifd0 = t.ifd(@[
    (254'u16, vLong([1'u32])), (256'u16, vLong([2'u32])), (257'u16, vLong([2'u32])), (259'u16, vShort([1'u16])),
    (262'u16, vShort([2'u16])), (271'u16, vAscii(make)), (273'u16, vBlobs([thumb])), (277'u16, vShort([3'u16])),
    (279'u16, vLong([12'u32])), (330'u16, vIfds([rawIfd]))])
  t.chain = @[ifd0]
  t.build()

proc nefCompressed*(width, height: int; bits: uint16; strip: seq[byte]; cfa: array[4, uint8]; noteBigEndian: bool;
                    noteTags: seq[Entry]): seq[byte] =
  ## A synthetic Nikon NEF with a Huffman-compressed (34713) strip: IFD0 with Make and an EXIF IFD
  ## whose maker note (`Nikon\0`, version 2.10, then an embedded TIFF in the given byte order)
  ## holds `noteTags`.
  var note = TiffBuilder(bigEndian: noteBigEndian)
  let n = note.ifd(noteTags)
  note.chain = @[n]
  var maker: seq[byte]
  for ch in "Nikon\0\x02\x10\0\0": maker.add byte(ch)
  maker.add note.build()
  var t = TiffBuilder()
  let len = strip.len
  let s = t.blob(strip)
  let rawIfd = t.ifd(@[
    (254'u16, vLong([0'u32])), (256'u16, vLong([uint32(width)])), (257'u16, vLong([uint32(height)])),
    (258'u16, vShort([bits])), (259'u16, vShort([34713'u16])), (262'u16, vShort([32803'u16])), (273'u16, vBlobs([s])),
    (277'u16, vShort([1'u16])), (278'u16, vLong([uint32(height)])), (279'u16, vLong([uint32(len)])),
    (33421'u16, vShort([2'u16, 2])), (33422'u16, vByte(cfa))])
  let exif = t.ifd(@[(37500'u16, vUndefined(maker))])
  let ifd0 = t.ifd(@[(271'u16, vAscii("NIKON CORPORATION")), (330'u16, vIfds([rawIfd])), (34665'u16, vIfds([exif]))])
  t.chain = @[ifd0]
  t.build()

# ---------------------------------------------------------------- Sony cRAW

proc orBits(b: var array[16, byte]; at: int; v: uint64; width: int) =
  ## ORs the low `width` bits of `v` into a little-endian 128-bit value at bit `at`.
  for k in 0 ..< width:
    if ((v shr uint64(k)) and 1) != 0 and at + k < 128:
      b[(at + k) div 8] = b[(at + k) div 8] or byte(1 shl ((at + k) mod 8))

proc crawBlock*(codes: array[16, uint16]): array[16, byte] =
  ## Encodes 16 same-colour 11-bit codes as one cRAW block: maximum, minimum, their positions,
  ## then 7-bit deltas at the smallest power-of-two step that covers the range.
  let mx = max(codes)
  let mn = min(codes)
  let imax = codes.find(mx)
  var imin = codes.find(mn)
  if imin == imax: imin = (imax + 1) mod 16
  var sh = 0
  while (128'u32 shl sh) <= uint32(mx - mn): inc sh
  orBits(result, 0, uint64(mx), 64)
  orBits(result, 11, uint64(mn), 64)
  orBits(result, 22, uint64(imax), 64)
  orBits(result, 26, uint64(imin), 64)
  var at = 30
  for i, c in codes:
    if i != imax and i != imin:
      orBits(result, at, min(uint64((c - mn) shr sh), 127'u64), 7)
      at += 7

proc crawEncode*(codes: openArray[uint16]; width, height: int): seq[byte] =
  ## Encodes a mosaic of 11-bit codes (`width` a multiple of 32) as cRAW rows.
  doAssert width mod 32 == 0
  for y in 0 ..< min(height, codes.len div width):
    for g in 0 ..< width div 32:
      for parity in 0 .. 1:
        var sq: array[16, uint16]
        for i in 0 ..< 16: sq[i] = min(codes[y * width + g * 32 + 2 * i + parity], 2047'u16)
        result.add crawBlock(sq)

proc sonyCraw*(width, height: int; codes: openArray[uint16]; curve: array[4, uint16]): seq[byte] =
  ## A synthetic Sony compressed ARW: IFD0 with Make, the cRAW image in a SubIFD with
  ## SonyRawFileType 2, the tone curve (0x7010), black (0x7310), white (50717) and WB (0x7313).
  let bytes = crawEncode(codes, width, height)
  var t = TiffBuilder()
  let n = bytes.len
  let strip = t.blob(bytes)
  let rawIfd = t.ifd(@[
    (254'u16, vLong([0'u32])), (256'u16, vLong([uint32(width)])), (257'u16, vLong([uint32(height)])),
    (258'u16, vShort([12'u16])), (259'u16, vShort([32767'u16])), (262'u16, vShort([32803'u16])),
    (273'u16, vBlobs([strip])), (277'u16, vShort([1'u16])), (278'u16, vLong([uint32(height)])),
    (279'u16, vLong([uint32(n)])), (33421'u16, vShort([2'u16, 2])), (33422'u16, vByte([0'u8, 1, 1, 2])),
    (0x7000'u16, vShort([2'u16])), (0x7010'u16, vShort(curve)), (0x7310'u16, vShort([512'u16, 512, 512, 512])),
    (0x7313'u16, vShort([2048'u16, 1024, 1024, 1536])), (50717'u16, vShort([16383'u16]))])
  let ifd0 = t.ifd(@[(271'u16, vAscii("SONY")), (272'u16, vAscii("ILCE-Synthetic")), (330'u16, vIfds([rawIfd]))])
  t.chain = @[ifd0]
  t.build()

# ---------------------------------------------------------------- Panasonic RW2

const
  Rw2Page = 0x4000
  Rw2Split = 0x1FF8

proc rw2Pack*(data: openArray[uint16]; width: int; bits: int): seq[byte] =
  ## Packs `bits`-bit samples (12: 10 per block, 14: 9 per block) into the RW2 RawFormat 5 layout:
  ## 16-byte little-endian blocks, stored in 0x4000-byte pages split at 0x1FF8.
  let ppb = if bits == 14: 9 else: 10
  doAssert width mod ppb == 0
  var logical: seq[byte]
  var i = 0
  while i + ppb <= data.len:
    var blk: array[16, byte]
    for k in 0 ..< ppb: orBits(blk, bits * k, uint64(data[i + k] and uint16((1 shl bits) - 1)), bits)
    logical.add blk
    i += ppb
  logical.setLen((logical.len + Rw2Page - 1) div Rw2Page * Rw2Page)
  result = newSeq[byte](logical.len)
  for k, b in logical:
    let page = k - k mod Rw2Page
    result[page + (k mod Rw2Page + Rw2Split) mod Rw2Page] = b

proc rw2*(width, height: int; data: openArray[uint16]; bits: int): seq[byte] =
  ## A synthetic Panasonic RW2 (RawFormat 5) with an RGGB sensor, a 2-pixel border on every side,
  ## black levels 128 / 129 / 130 and WB levels 512 / 256 / 384.
  var t = TiffBuilder()
  let raw = t.blob(rw2Pack(data, width, bits))
  let white = uint16((1 shl bits) - 1)
  let ifd0 = t.ifd(@[
    (0x0002'u16, vShort([uint16(width)])), (0x0003'u16, vShort([uint16(height)])), (0x0004'u16, vShort([2'u16])),
    (0x0005'u16, vShort([2'u16])), (0x0006'u16, vShort([uint16(height - 2)])), (0x0007'u16, vShort([uint16(width - 2)])),
    (0x0009'u16, vShort([1'u16])), (0x000A'u16, vShort([uint16(bits)])), (0x000E'u16, vShort([white])),
    (0x000F'u16, vShort([white])), (0x0010'u16, vShort([white])), (0x001C'u16, vShort([128'u16])),
    (0x001D'u16, vShort([129'u16])), (0x001E'u16, vShort([130'u16])), (0x0024'u16, vShort([512'u16])),
    (0x0025'u16, vShort([256'u16])), (0x0026'u16, vShort([384'u16])), (0x002D'u16, vShort([5'u16])),
    (0x010F'u16, vAscii("Panasonic")), (0x0110'u16, vAscii("DC-Synthetic")), (0x0112'u16, vShort([1'u16])),
    (0x0118'u16, vBlobs([raw]))])
  t.chain = @[ifd0]
  result = t.build()
  result[2] = byte('U')
  result[3] = 0

# ---------------------------------------------------------------- Olympus ORF

proc relativeIfd(entries: openArray[(uint16, uint16, uint32, seq[byte])]; baseOffset: int): seq[byte] =
  ## A little-endian IFD whose value offsets are relative to `base` (the IFD itself sits at
  ## `baseOffset` from that base). Entries: (tag, type, count, value bytes).
  let head = 2 + 12 * entries.len + 4
  var extra: seq[byte]
  result.add b16(false, uint16(entries.len))
  for (tag, typ, count, bytes) in entries:
    result.add b16(false, tag)
    result.add b16(false, typ)
    result.add b32(false, count)
    if bytes.len <= 4:
      var v = bytes
      v.setLen(4)
      result.add v
    else:
      result.add b32(false, uint32(baseOffset + head + extra.len))
      extra.add bytes
  result.add b32(false, 0)
  result.add extra

proc le32(v: uint32): seq[byte] = @(b32(false, v))

proc orf*(width, height: int; data: openArray[uint16]): seq[byte] =
  ## A synthetic uncompressed Olympus ORF (`IIRO`): 12-bit `data` stored left-justified in 16-bit
  ## samples, a GRBG EXIF CFAPattern and a new-style maker note with ImageProcessing levels
  ## (black 64, WB 2.0 / 1.5, crop of a 2-pixel border) and CameraSettings pointing at a tiny
  ## preview JPEG.
  let ip = relativeIfd([
    (0x0100'u16, 3'u16, 2'u32, le16s([512'u16, 384])), (0x011F'u16, 3'u16, 1'u32, le16s([256'u16])),
    (0x0600'u16, 3'u16, 4'u32, le16s([64'u16, 64, 64, 64])), (0x0611'u16, 3'u16, 2'u32, le16s([12'u16, 0])),
    (0x0612'u16, 3'u16, 2'u32, le16s([2'u16, 0])), (0x0613'u16, 3'u16, 2'u32, le16s([2'u16, 0])),
    (0x0614'u16, 4'u16, 1'u32, le32(uint32(width - 4))), (0x0615'u16, 4'u16, 1'u32, le32(uint32(height - 4)))], 64)
  # CameraSettings with a preview JPEG (SOI, SOF0 16×8, EOI) right after it.
  let jpeg = @[0xFF'u8, 0xD8, 0xFF, 0xC0, 0x00, 0x0B, 8, 0x00, 0x08, 0x00, 0x10, 1, 1, 0x11, 0, 0xFF, 0xD9]
  let csAt = 64 + (ip.len + 3) div 4 * 4
  let csLen = 2 + 12 * 3 + 4
  let cs = relativeIfd([
    (0x0100'u16, 4'u16, 1'u32, le32(1)), (0x0101'u16, 4'u16, 1'u32, le32(uint32(csAt + csLen))),
    (0x0102'u16, 4'u16, 1'u32, le32(uint32(jpeg.len)))], csAt)
  var note: seq[byte]
  for ch in "OLYMPUS\0II\x03\0": note.add byte(ch)
  note.add relativeIfd([(0x2020'u16, 13'u16, 1'u32, le32(uint32(csAt))), (0x2040'u16, 13'u16, 1'u32, le32(64))], 12)
  note.setLen(64)
  note.add ip
  note.setLen(csAt)
  note.add cs
  note.add jpeg
  var t = TiffBuilder()
  var stripData: seq[byte]
  for v in data: stripData.add b16(false, v shl 4)
  let strip = t.blob(stripData)
  let exif = t.ifd(@[(37500'u16, vUndefined(note)), (41730'u16, vUndefined([2'u8, 0, 2, 0, 1, 0, 2, 1]))])
  let ifd0 = t.ifd(@[
    (256'u16, vLong([uint32(width)])), (257'u16, vLong([uint32(height)])), (258'u16, vShort([16'u16])),
    (259'u16, vShort([1'u16])), (262'u16, vShort([1'u16])), (271'u16, vAscii("OLYMPUS IMAGING CORP.")),
    (272'u16, vAscii("E-Synthetic")), (273'u16, vBlobs([strip])), (277'u16, vShort([1'u16])),
    (278'u16, vLong([uint32(height)])), (279'u16, vLong([uint32(data.len * 2)])), (34665'u16, vIfds([exif]))])
  t.chain = @[ifd0]
  result = t.build()
  result[2] = byte('R')
  result[3] = byte('O')

# ---------------------------------------------------------------- Fujifilm RAF

type
  RafPackingKind* = enum
    rpU16        ## 16-bit containers; little-endian (`II`) or big-endian (`MM`) CFA TIFF
    rpLsb12      ## 12-bit, little-endian bit stream, least significant bit first
    rpWords14    ## 14-bit, most significant bit first in little-endian 32-bit words
    rpCompressed ## a short `IS…` strip standing in for Fujifilm's compressed data

  RafPacking* = object
    kind*: RafPackingKind
    le*: bool ## `rpU16` only

  RafSpec* = object
    ## A synthetic Fujifilm RAF.
    width*, height*: int
    data*: seq[uint16]
    bits*: uint32
    packing*: RafPacking
    cfa*: seq[uint8]                    ## 36 (X-Trans, 6×6) or 4 (Bayer, 2×2) colours, row major
    crop*: Option[array[4, uint16]]     ## top, left, height, width
    black*: seq[uint32]
    wbGrb*: Option[array[3, uint32]]    ## G, R, B
    exposureBias*: Option[(int16, int16)]
    orientation*: uint16                ## EXIF orientation of the preview JPEG
    cfaTiff*: bool                      ## false: an early-FinePix-style file without the CFA TIFF

const Xtrans*: array[36, uint8] = [
  1'u8, 1, 0, 1, 1, 2,
  1, 1, 2, 1, 1, 0,
  2, 0, 1, 0, 2, 1,
  1, 1, 2, 1, 1, 0,
  1, 1, 0, 1, 1, 2,
  0, 2, 1, 2, 0, 1]
  ## The X-Trans repeat at the data origin, as observed (row major, 0 = R, 1 = G, 2 = B).

proc u16Packing*(le: bool): RafPacking = RafPacking(kind: rpU16, le: le)
proc lsb12*(): RafPacking = RafPacking(kind: rpLsb12)
proc words14*(): RafPacking = RafPacking(kind: rpWords14)
proc compressedPacking*(): RafPacking = RafPacking(kind: rpCompressed)

proc newRafSpec*(width, height: int; data: seq[uint16]; cfa: seq[uint8]): RafSpec =
  RafSpec(width: width, height: height, data: data, bits: 14, packing: u16Packing(true), cfa: cfa,
          black: @[1024'u32], wbGrb: some([302'u32, 604, 453]), orientation: 1, cfaTiff: true)

proc strip(r: RafSpec): seq[byte] =
  ## The samples as stored.
  case r.packing.kind
  of rpU16:
    for v in r.data: result.add b16(not r.packing.le, v)
  of rpLsb12:
    var acc = 0'u64
    var n = 0'u32
    for v in r.data:
      acc = acc or (uint64(v and 0xFFF) shl n)
      n += 12
      while n >= 8:
        result.add byte(acc and 0xFF)
        acc = acc shr 8
        n -= 8
    if n > 0: result.add byte(acc and 0xFF)
  of rpWords14:
    var logical: seq[byte]
    var acc = 0'u64
    var n = 0'u32
    for v in r.data:
      acc = (acc shl 14) or uint64(v and 0x3FFF)
      n += 14
      while n >= 8:
        logical.add byte((acc shr (n - 8)) and 0xFF)
        n -= 8
    if n > 0: logical.add byte((acc shl (8 - n)) and 0xFF)
    logical.setLen((logical.len + 3) div 4 * 4)
    var i = 0
    while i < logical.len:
      for k in countdown(3, 0): result.add logical[i + k]
      i += 4
  of rpCompressed:
    for ch in "IS\x01\x00": result.add byte(ch)
    let target = r.width * r.height div 2
    while result.len < target: result.add 0x5A
    result.setLen(target)

proc cfaRegion(r: RafSpec): seq[byte] =
  ## The CFA region: a TIFF whose IFD0 points (0xF000) at the Fuji raw IFD.
  let le = not (r.packing.kind == rpU16 and not r.packing.le)
  let be = not le
  let strip = r.strip
  let stripAt = 2048
  var entries: seq[(uint16, seq[uint32])] = @[
    (0xF001'u16, @[uint32(r.width)]), (0xF002'u16, @[uint32(r.height)]), (0xF003'u16, @[r.bits]),
    (0xF007'u16, @[uint32(stripAt)]), (0xF008'u16, @[uint32(strip.len)]), (0xF00A'u16, r.black)]
  if r.wbGrb.isSome: entries.add (0xF00E'u16, @(r.wbGrb.get))
  result = if le: @[byte('I'), byte('I'), 42, 0] else: @[byte('M'), byte('M'), 0, 42]
  result.add b32(be, 8)
  # IFD0: one entry, 0xF000 (type IFD) → the Fuji IFD at 26.
  result.add b16(be, 1)
  result.add b16(be, 0xF000)
  result.add b16(be, 13)
  result.add b32(be, 1)
  result.add b32(be, 26)
  result.add b32(be, 0)
  let fujiAt = result.len
  var extraAt = fujiAt + 2 + 12 * entries.len + 4
  var extra: seq[byte]
  result.add b16(be, uint16(entries.len))
  for (tag, vals) in entries:
    result.add b16(be, tag)
    result.add b16(be, 4)
    result.add b32(be, uint32(vals.len))
    if vals.len == 1: result.add b32(be, vals[0])
    else:
      result.add b32(be, uint32(extraAt))
      for v in vals: extra.add b32(be, v)
      extraAt += 4 * vals.len
  result.add b32(be, 0)
  result.add extra
  doAssert result.len <= stripAt
  result.setLen(stripAt)
  result.add strip

proc preview(r: RafSpec): seq[byte] =
  ## A tiny baseline-JPEG preview (APP1 EXIF with make, model and orientation; SOF0 32×24; EOI).
  var t = TiffBuilder()
  let ifd0 = t.ifd(@[(271'u16, vAscii("FUJIFILM")), (272'u16, vAscii("X-Synthetic")), (274'u16, vShort([r.orientation]))])
  t.chain = @[ifd0]
  var app1: seq[byte]
  for ch in "Exif\0\0": app1.add byte(ch)
  app1.add t.build()
  result = @[0xFF'u8, 0xD8, 0xFF, 0xE1]
  result.add be16(app1.len + 2)
  result.add app1
  result.add [0xFF'u8, 0xC0, 0x00, 0x0B, 8, 0x00, 24, 0x00, 32, 1, 1, 0x11, 0, 0xFF, 0xD9]

proc meta(r: RafSpec): seq[byte] =
  ## The metadata directory.
  var recs: seq[(uint16, seq[byte])]
  if r.crop.isSome:
    let c = r.crop.get
    recs.add (0x0110'u16, @(b16(true, c[0])) & @(b16(true, c[1])))
    recs.add (0x0111'u16, @(b16(true, c[2])) & @(b16(true, c[3])))
  if r.cfa.len == 36:
    recs.add (0x0130'u16, @[0x0C'u8, 0x0C, 0x0C, 0x0C])
    var rev: seq[byte]
    for k in countdown(r.cfa.high, 0): rev.add r.cfa[k]
    recs.add (0x0131'u16, rev)
  else:
    # Bayer: reversed, colour in the low two bits, the second green as 3.
    var greens = 0
    var codes: seq[byte]
    for c in r.cfa:
      if c == 1:
        inc greens
        codes.add(if greens == 1: 0x09'u8 else: 0x0B'u8)
      else: codes.add(0x08'u8 or c)
    var rev: seq[byte]
    for k in countdown(codes.high, 0): rev.add codes[k]
    recs.add (0x0130'u16, rev)
  if r.exposureBias.isSome:
    let (n, d) = r.exposureBias.get
    recs.add (0x9650'u16, @(b16(true, cast[uint16](n))) & @(b16(true, cast[uint16](d))))
  result = @(b32(true, uint32(recs.len)))
  for (tag, d) in recs:
    result.add b16(true, tag)
    result.add b16(true, uint16(d.len))
    result.add d

proc build*(r: RafSpec): seq[byte] =
  var b: seq[byte]
  for ch in "FUJIFILMCCD-RAW 0201FF000000": b.add byte(ch)
  for ch in "X-Synthetic": b.add byte(ch)
  b.setLen(60)
  for ch in "0100": b.add byte(ch)
  b.setLen(160)
  let pv = r.preview
  let mt = r.meta
  let region = if r.cfaTiff: r.cfaRegion else: r.strip
  let (pAt, mAt) = (b.len, b.len + pv.len)
  let cAt = mAt + mt.len
  for (at, v) in [(84, pAt), (88, pv.len), (92, mAt), (96, mt.len), (100, cAt), (104, region.len)]:
    let q = b32(true, uint32(v))
    for k in 0 .. 3: b[at + k] = q[k]
  b.add pv
  b.add mt
  b.add region
  b

# ---------------------------------------------------------------- scenes

proc satU16(v: float32): uint16 =
  if v != v or v <= 0: 0'u16 elif v >= 65535: 65535'u16 else: uint16(v)

proc mosaicCfa*(rgb: openArray[array[3, float32]]; width: int; cfa: openArray[uint8]; cfaW: int; black, white: uint16): seq[uint16] =
  ## Mosaics linear RGB through any CFA repeat (`cfa`: `cfaW` colours per row) anchored at (0, 0).
  let cfaH = cfa.len div cfaW
  result = newSeq[uint16](rgb.len)
  for i, p in rgb:
    let (x, y) = (i mod width, i div width)
    let c = int(cfa[(y mod cfaH) * cfaW + x mod cfaW])
    result[i] = satU16(round(float32(black) + clamp(p[c], 0'f32, 1'f32) * float32(white - black)))

proc scene*(width, height: int): seq[array[3, float32]] =
  ## A smooth, colourful synthetic scene as linear RGB in 0..1.
  result = newSeq[array[3, float32]](width * height)
  for i in 0 ..< width * height:
    let x = float32(i mod width) / float32(max(width, 1))
    let y = float32(i div width) / float32(max(height, 1))
    result[i] = [0.1'f32 + 0.7'f32 * x, 0.15'f32 + 0.5'f32 * (1'f32 - y) * x + 0.2'f32 * y,
                 0.6'f32 - 0.4'f32 * y + 0.2'f32 * x]

proc mosaic*(rgb: openArray[array[3, float32]]; width: int; cfa: array[4, uint8]; black, white: uint16): seq[uint16] =
  ## Mosaics linear RGB through a 2×2 CFA (`cfa` row-major colours) into sensor values
  ## `black + v * (white - black)`.
  result = newSeq[uint16](rgb.len)
  for i, p in rgb:
    let (x, y) = (i mod width, i div width)
    let c = int(cfa[(y and 1) * 2 + (x and 1)])
    let v = clamp(p[c], 0'f32, 1'f32)
    result[i] = satU16(round(float32(black) + v * float32(white - black)))
