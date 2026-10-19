## Vendor raw formats (`raw/tests/vendors.rs`, `vendor_codecs.rs`, `raf.rs`, the NEF tests of
## `nefc.rs` and the camera-look tests of `look.rs`): CR2 slices and phase measurement, TIFF/EP,
## Nikon Huffman NEF, Sony cRAW, Panasonic RW2, Olympus ORF, Fujifilm RAF, embedded previews and
## the colour fit to the camera JPEG.

import std/[unittest, options, math, strutils, sequtils]
import photocraft/raw
import photocraft/raw/[testgen, nefc, color, develop]

proc kindOf(b: openArray[byte]; limits = defaultRawLimits()): RawErrorKind =
  try:
    discard decode(b, limits)
    raise newException(AssertionDefect, "decoded")
  except RawError as e: e.kind

proc errOf(b: openArray[byte]): ref RawError =
  try:
    discard decode(b)
    raise newException(AssertionDefect, "decoded")
  except RawError as e: e

proc bytesOf(s: string): seq[byte] =
  for c in s: result.add byte(c)

proc le16At(b: openArray[byte]; at: int): int = int(b[at]) or (int(b[at + 1]) shl 8)
proc le32At(b: openArray[byte]; at: int): int = le16At(b, at) or (le16At(b, at + 2) shl 16)

# ---------------------------------------------------------------- CR2 and TIFF/EP

proc cr2Spec(w, h, comps: int; slices: seq[int]): Cr2Spec =
  ## Left / top borders of 16 / 4 masked pixels at black level 512.
  let (l, t) = (16, 4)
  let img = mosaic(scene(w - l, h - t), w - l, [0'u8, 1, 1, 2], 512, 13000)
  var data = newSeqWith(w * h, 512'u16)
  for y in t ..< h:
    for x in l ..< w: data[y * w + x] = img[(y - t) * (w - l) + (x - l)]
  Cr2Spec(width: w, height: h, data: data, precision: 14, components: comps, slices: slices,
          borders: some([uint16(l), uint16(t), uint16(w - 1), uint16(h - 1)]), wbRggb: some([2048'u16, 1024, 1024, 1536]),
          orientation: 1)

proc cr2PhaseSpec(cfa: array[4, uint8]; l, t: int; modelId: Option[uint32]): Cr2Spec =
  ## #193: left half a colourful scene, right half a neutral grey patch, recorded through WB gains.
  let (w, h) = (192, 96)
  let wb = [2'f32, 1, 1.5]
  let colourful = scene(w, h)
  var rgb: seq[array[3, float32]]
  for i in 0 ..< w * h:
    let p = if i mod w >= w div 2: [0.4'f32, 0.4, 0.4] else: colourful[i]
    rgb.add [p[0] / wb[0], p[1] / wb[1], p[2] / wb[2]]
  var data = mosaic(rgb, w, cfa, 512, 13000)
  for y in 0 ..< h:
    for x in 0 ..< w:
      if x < l or y < t: data[y * w + x] = 512
  Cr2Spec(width: w, height: h, data: data, precision: 14, components: 2, slices: @[96, 96],
          borders: some([uint16(l), uint16(t), uint16(w - 1), uint16(h - 1)]), wbRggb: some([2048'u16, 1024, 1024, 1536]),
          orientation: 1, modelId: modelId)

proc greyPatch(d: Developed; x0: int): array[3, float64] =
  let (w, h) = (int(d.width), int(d.height))
  var n = 0.0
  for y in 8 ..< h - 8:
    for x in x0 + 8 ..< w - 8:
      for c in 0 .. 2: result[c] += float64(d.rgb[(y * w + x) * 3 + c])
      n += 1
  for c in 0 .. 2: result[c] /= n

suite "raw: CR2 and TIFF/EP":
  test "CR2 slices decode exactly":
    for (comps, slices) in [(2, @[32, 32, 24]), (4, @[40, 48]), (2, newSeq[int]()), (4, @[88])]:
      let spec = cr2Spec(88, 30, comps, slices)
      let b = spec.build()
      check identify(b) == some(rfCr2)
      let s = decode(b)
      check (s.width, s.height) == (88, 30)
      check s.data == spec.data
      check s.active == rawRect(16, 4, 72, 26)
      check s.cameraWb == some([2.0, 1.0, 1.5])
      check s.black.values.allIt(abs(it - 512'f32) < 1e-3)
      check s.cfa.get.phase(16, 4) == [0'u8, 1, 1, 2]

  test "CR2 develops":
    let d = develop(cr2Spec(88, 30, 2, @[32, 32, 24]).build())
    check (d.width, d.height) == (72'u32, 26'u32)
    check d.info.format == rfCr2
    check d.info.wbMultipliers == [2.0, 1.0, 1.5]
    check d.warnings.anyIt("sRGB primaries" in it)

  test "CR2 sRAW is unsupported":
    var b = cr2Spec(32, 8, 2, @[]).build()
    var sof = -1
    for i in 0 ..< b.len - 1:
      if b[i] == 0xFF and b[i + 1] == 0xC3:
        sof = i
        break
    b[sof + 11] = 0x21
    let e = errOf(b)
    check e.kind == reUnsupported and "sRAW" in e.msg

  test "CR2 CFA phase is measured for every row phase and border parity":
    for cfa in [[0'u8, 1, 1, 2], [1'u8, 2, 0, 1]]:
      for (l, t) in [(16, 4), (17, 4), (16, 5), (17, 5)]:
        let s = decode(cr2PhaseSpec(cfa, l, t, none(uint32)).build())
        check s.cfa.get.phase(0, 0) == cfa
        check s.warnings.len == 0
        var o = defaultDevelopOptions()
        o.orient = false
        let g = greyPatch(developSensor(s, o), 192 div 2 - l)
        check abs(g[0] / g[1] - 1.0) < 0.02 and abs(g[2] / g[1] - 1.0) < 0.02

  test "CR2 CFA phase falls back to the model table":
    proc flat(modelId: Option[uint32]): Sensor =
      var spec = cr2PhaseSpec([0'u8, 1, 1, 2], 16, 4, modelId)
      for v in spec.data.mitems: v = 512
      decode(spec.build())
    var s = flat(some(0x8000_0250'u32))
    check s.cfa.get.phase(0, 0) == [1'u8, 2, 0, 1]
    check not s.warnings.anyIt("colour filter phase" in it)
    s = flat(some(0x8000_0001'u32))
    check s.cfa.get.phase(0, 0) == [0'u8, 1, 1, 2]
    check s.warnings.anyIt("colour filter phase" in it)
    check flat(none(uint32)).cfa.get.phase(0, 0) == [0'u8, 1, 1, 2]

  test "a NEF-like TIFF/EP raw":
    let (w, h) = (24, 16)
    let data = mosaic(scene(w, h), w, [2'u8, 1, 1, 0], 0, 4095)
    let b = tiffEp("NIKON CORPORATION", w, h, data, [2'u8, 1, 1, 0], 12)
    check identify(b) == some(rfNef)
    let s = decode(b)
    check s.data == data
    check s.cfa.get.phase(0, 0) == [2'u8, 1, 1, 0]
    check s.warnings.anyIt("black level" in it)
    discard developSensor(s, defaultDevelopOptions())

  test "an ARW-like raw reads Sony levels and crop":
    let (w, h) = (24, 16)
    let data = mosaic(scene(w, h), w, [0'u8, 1, 1, 2], 512, 16383)
    let extra = @[(0x7310'u16, vShort([512'u16, 512, 512, 512])), (0x7313'u16, vShort([2048'u16, 1024, 1024, 1536])),
                  (50717'u16, vShort([16383'u16])), (50719'u16, vLong([2'u32, 2])), (50720'u16, vLong([20'u32, 12]))]
    let b = tiffEp("SONY", w, h, data, [0'u8, 1, 1, 2], 14, extra)
    check identify(b) == some(rfArw)
    let s = decode(b)
    check s.black.values == @[512'f32, 512, 512, 512]
    check s.white == [16383'f32, 16383, 16383]
    check s.cameraWb == some([2.0, 1.0, 1.5])
    check s.crop == rawRect(2, 2, 20, 12)
    check s.warnings.len == 0
    let d = developSensor(s, defaultDevelopOptions())
    check (d.width, d.height) == (20'u32, 12'u32)

  test "other raw containers are recognised but unsupported":
    var cr3 = @[0'u8, 0, 0, 24] & bytesOf("ftypcrx ") & newSeq[byte](12)
    check identify(cr3) == some(rfCr3)
    check kindOf(cr3) == reUnsupported
    var rafb = bytesOf("FUJIFILMCCD-RAW 0201FF383501")
    rafb.setLen(200)
    check identify(rafb) == some(rfRaf)
    check kindOf(rafb) == reUnsupported
    check kindOf(bytesOf("not a raw file")) == reNotRaw

  test "the embedded preview is found":
    let jpeg = @[0xFF'u8, 0xD8, 0xFF, 0xC0, 0x00, 0x0B, 8, 0x00, 0x20, 0x00, 0x30, 1, 1, 0x11, 0, 0xFF, 0xD9]
    var rafb = bytesOf("FUJIFILMCCD-RAW 0201FF383501")
    rafb.setLen(100)
    rafb[84 .. 87] = [0'u8, 0, 0, 100]
    rafb[88 .. 91] = [0'u8, 0, 0, byte(jpeg.len)]
    rafb.add jpeg
    let p = embeddedPreview(rafb).get
    check (p.width, p.height) == (0x30'u32, 0x20'u32)
    check rafb[p.offset ..< p.offset + p.len] == jpeg
    check embeddedPreview(cr2Spec(32, 8, 2, @[]).build()).isNone

# ---------------------------------------------------------------- Nikon Huffman NEF

type Table = object
  counts, symbols: seq[uint8]

const Unseen = 255'u8
let
  TLossless12 = Table(counts: @[0'u8, 1, 4, 2, 3, 1, 2], symbols: @[5'u8, 4, 6, 3, 7, 2, 8, 1, 9, 0, 10, 11, 12])
  TLossless14 = Table(counts: @[0'u8, 1, 4, 2, 2, 3, 1, 2], symbols: @[7'u8, 6, 8, 5, 9, 4, 10, 3, 11, 12, 2, 0, 1, 13, 14])
  TLossy12 = Table(counts: @[0'u8, 1, 5, 1, 1, 1, 1, 1, 1], symbols: @[5'u8, 4, 3, 6, 2, 7, 1, 0, 8, 9, Unseen, 10])
  TLossy14 = Table(counts: @[0'u8, 1, 4, 3, 1, 1, 1, 1, 1], symbols: @[5'u8, 6, 4, 7, 8, 3, 9, 2, 1, 0, 10, 11, 12])

proc codes(t: Table): seq[(uint32, uint32)] =
  result = newSeq[(uint32, uint32)](17)
  var code = 0'u32
  var k = 0
  for i, n in t.counts:
    for _ in 0 ..< int(n):
      let s = t.symbols[k]
      inc k
      if s != Unseen: result[s] = (code, uint32(i + 1))
      inc code
    code = code shl 1

proc nefEncode(values: openArray[int32]; w: int; t: Table; seeds: array[2, array[2, int32]]): seq[byte] =
  ## Encodes values the way the camera does (MSB-first, no byte stuffing, 4 bytes of padding).
  let cs = codes(t)
  var acc = 0'u64
  var n = 0
  proc put(o: var seq[byte]; v, len: uint32) =
    var i = int(len) - 1
    while i >= 0:
      acc = (acc shl 1) or uint64((v shr uint32(i)) and 1)
      inc n
      if n == 8:
        o.add byte(acc and 0xFF)
        acc = 0
        n = 0
      dec i
  var vpred = seeds
  for y in 0 ..< values.len div w:
    for x in 0 ..< w:
      let v = values[y * w + x]
      let pred = if x < 2: vpred[y and 1][x] else: values[y * w + x - 2]
      if x < 2: vpred[y and 1][x] = v
      let d = v - pred
      var k = 0'u32
      while k < 32 and (uint32(abs(d)) shr k) != 0: inc k
      let (code, len) = cs[k]
      doAssert len > 0
      put(result, code, len)
      put(result, (if d < 0: uint32(d + (1'i32 shl k) - 1) else: uint32(d)), k)
  if n > 0: result.add byte((acc shl (8 - n)) and 0xFF)
  result.add [0'u8, 0, 0, 0]

proc nefImage(w, h: int; mx: int32; seed: uint32): seq[int32] =
  ## Smooth gradient + texture + hard edges, within `0..=max`.
  var s = seed
  for i in 0 ..< w * h:
    let (x, y) = (int32(i mod w), int32(i div w))
    s = s * 1_664_525'u32 + 1_013_904_223'u32
    let noise = int32(s shr 24) mod 9 - 4
    let base = (x * 37 + y * 11) mod (mx div 2) + (if (x div 7 + y div 5) mod 3 == 0: mx div 3 else: 0)
    result.add clamp(base + noise, 0'i32, mx)

proc lossyTable(bits: int; le: bool): seq[byte] =
  result = @[0x44'u8, 0x20]
  proc put(t: var seq[byte]; v: uint16) =
    if le: t.add [byte(v and 0xFF), byte(v shr 8)] else: t.add [byte(v shr 8), byte(v and 0xFF)]
  for _ in 0 ..< 4: put(result, 300)
  put(result, 257)
  let mx = float64((1'u32 shl bits) - 1)
  for i in 0'u32 ..< 257: put(result, uint16(mx * sqrt(min(float64(i) / 64.0, 1.0))))
  result.setLen(624)

proc losslessTable(seed: uint16; le: bool): seq[byte] =
  result = @[0x46'u8, 0x30]
  for _ in 0 ..< 4: result.add(if le: @[byte(seed and 0xFF), byte(seed shr 8)] else: @[byte(seed shr 8), byte(seed and 0xFF)])
  result.setLen(46)

proc toU16(v: seq[int32]): seq[uint16] =
  for x in v: result.add uint16(x)

suite "raw: Nikon Huffman NEF":
  test "lossless round trip, 12 and 14 bits":
    for (bits, t) in [(12, TLossless12), (14, TLossless14)]:
      let (w, h) = (64, 9)
      let mx = (1'i32 shl bits) - 1
      var img = nefImage(w, h, mx, uint32(bits))
      img[5] = 0
      img[7] = mx
      img[w + 9] = mx
      let s = 1'i32 shl (bits - 3)
      let table = DecodeTable(encoding: ekLossless, seeds: [[s, s], [s, s]])
      check decodeNef(nefEncode(img, w, t, table.seeds), w, h, bits, table) == toU16(img)

  test "lossy type 2 curve and round trip":
    for (bits, t) in [(12, TLossy12), (14, TLossy14)]:
      for le in [false, true]:
        let table = parseTable(lossyTable(bits, le), le, bits)
        check table.seeds == [[300'i32, 300], [300'i32, 300]]
        check table.encoding == ekLossy
        let curve = table.curve
        let step = (1 shl bits) div 256
        check curve.len == (1 shl bits) + 1
        check int(curve[64 * step]) == (1 shl bits) - 1
        for i in 1 ..< curve.len: check curve[i - 1] <= curve[i]
        let (w, h) = (48, 7)
        let img = nefImage(w, h, int32(64 * step - 1), 7)
        var want: seq[uint16]
        for v in img: want.add curve[v]
        check decodeNef(nefEncode(img, w, t, table.seeds), w, h, bits, table) == want

  test "lossy type 1 full curve":
    var t = @[0x44'u8, 0x10, 0, 10, 0, 10, 0, 10, 0, 10, 0, 5]
    for v in [0'u16, 100, 400, 900, 1600]: t.add [byte(v shr 8), byte(v and 0xFF)]
    let table = parseTable(t, false, 12)
    check table.encoding == ekLossy and table.curve == @[0'u16, 100, 400, 900, 1600]
    let img = @[0'i32, 4, 1, 3, 2, 2, 4, 0]
    check decodeNef(nefEncode(img, 4, TLossy12, table.seeds), 4, 2, 12, table) == @[0'u16, 1600, 100, 900, 400, 400, 1600, 0]

  test "unsupported and malformed tables":
    proc kind(t: seq[byte]; le: bool; bits: int): RawErrorKind =
      try:
        discard parseTable(t, le, bits)
        raise newException(AssertionDefect, "parsed")
      except RawError as e: e.kind
    var split = lossyTable(12, false)
    split[562] = byte(345 shr 8)
    split[563] = byte(345 and 0xFF)
    check kind(split, false, 12) == reUnsupported
    check kind(@[0x49'u8, 0x30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], false, 12) == reUnsupported
    check kind(lossyTable(12, false), false, 16) == reUnsupported
    check kind(@[], false, 12) == reMalformed
    check kind(@[0x46'u8, 0x30, 8], false, 14) == reMalformed
    check kind(lossyTable(12, false)[0 ..< 100], false, 12) == reMalformed
    var odd = lossyTable(12, false)
    odd[10] = byte(300 shr 8)
    odd[11] = byte(300 and 0xFF)
    expect RawError: discard parseTable(odd, false, 12)
    expect RawError: discard parseTable(@[0x44'u8, 0x20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0], false, 12)

  test "decodes through the NEF container":
    let (w, h) = (40, 6)
    var img = nefImage(w, h, 16383, 11)
    var src = nefEncode(img, w, TLossless14, [[2048'i32, 2048], [2048'i32, 2048]])
    var note = @[(0x0096'u16, vUndefined(losslessTable(2048, false))), (0x003d'u16, vShort([600'u16, 600, 600, 600]))]
    let b = nefCompressed(w, h, 14, src, [0'u8, 1, 1, 2], true, note)
    check identify(b) == some(rfNef)
    var s = decode(b)
    check s.data == toU16(img)
    check s.black.values == @[600'f32, 600, 600, 600]
    let table = lossyTable(12, true)
    let t = parseTable(table, true, 12)
    img = nefImage(w, h, 1023, 5)
    src = nefEncode(img, w, TLossy12, t.seeds)
    note = @[(0x0096'u16, vUndefined(table)), (0x003d'u16, vShort([600'u16, 600, 600, 600]))]
    s = decode(nefCompressed(w, h, 12, src, [2'u8, 1, 1, 0], false, note))
    var want: seq[uint16]
    for v in img: want.add t.curve[v]
    check s.data == want
    check s.black.values == @[150'f32, 150, 150, 150]
    discard developSensor(s, defaultDevelopOptions())
    var split = table
    split[562] = 3
    split[563] = 0
    check kindOf(nefCompressed(w, h, 12, src, [2'u8, 1, 1, 0], false, @[(0x0096'u16, vUndefined(split))])) == reUnsupported
    let e = errOf(nefCompressed(w, h, 12, src, [2'u8, 1, 1, 0], false, @[(0x0001'u16, vShort([0'u16]))]))
    check e.kind == reUnsupported and "Nikon compressed NEF" in e.msg

  test "a Nikon without a black-level tag is black-subtracted":
    let (w, h) = (32, 8)
    let src = nefEncode(nefImage(w, h, 16383, 4), w, TLossless14, [[2048'i32, 2048], [2048'i32, 2048]])
    let s = decode(nefCompressed(w, h, 14, src, [0'u8, 1, 1, 2], true, @[(0x0096'u16, vUndefined(losslessTable(2048, false)))]))
    check s.black.values == @[0'f32]
    check not s.warnings.anyIt("black level" in it)

  test "truncated containers and corrupt streams never panic":
    let (w, h) = (32, 8)
    let src = nefEncode(nefImage(w, h, 16383, 2), w, TLossless14, [[2048'i32, 2048], [2048'i32, 2048]])
    let b = nefCompressed(w, h, 14, src, [0'u8, 1, 1, 2], true, @[(0x0096'u16, vUndefined(losslessTable(2048, false)))])
    for n in 0 ..< b.len:
      try: discard decode(b[0 ..< n])
      except RawError: discard
    let (w2, h2) = (64, 16)
    let table = DecodeTable(encoding: ekLossless, seeds: [[2048'i32, 2048], [2048'i32, 2048]])
    let s2 = nefEncode(nefImage(w2, h2, 16383, 3), w2, TLossless14, table.seeds)
    for cut in [0, 1, 10, s2.len div 2, s2.len - 40]:
      expect RawError: discard decodeNef(s2[0 ..< cut], w2, h2, 14, table)
    var s = 12345'u32
    for i in 0 ..< 200:
      var bad = s2
      for _ in 0 ..< 1 + i mod 8:
        s = s * 1_103_515_245'u32 + 12345'u32
        let at = int(s shr 8) mod bad.len
        bad[at] = bad[at] xor byte(1 shl (s mod 8))
      try: discard decodeNef(bad, w2, h2, 14, table)
      except RawError: discard
      var noise: seq[byte]
      for k in 0 ..< s2.len: noise.add byte(((uint32(k) * 2_654_435_761'u32) shr (i mod 24)) and 0xFF)
      try: discard decodeNef(noise, w2, h2, 14, table)
      except RawError: discard
      try: discard decodeNef(noise, w2, h2, 12, DecodeTable(encoding: ekLossless, seeds: [[512'i32, 512], [512'i32, 512]]))
      except RawError: discard
    let lossy = parseTable(lossyTable(12, false), false, 12)
    expect RawError: discard decodeNef(newSeqWith(64, 0xFF'u8), 8, 8, 12, lossy)
    expect RawError: discard decodeNef(s2, high(int), 2, 14, table)
    expect RawError: discard decodeNef(s2, 0, 2, 14, table)

# ---------------------------------------------------------------- Sony, Panasonic, Olympus

const Curve = [8000'u16, 10400, 12900, 14100]

proc craTone(code: uint16): uint16 =
  ## The cRAW tone curve re-stated for the tests: step 1, 2, 4, 8, 16 per 1/8 code between knots, ÷ 4.
  let pts = [0'u32, 8000, 10400, 12900, 14100, 16384]
  let x = uint32(code) * 8
  var y = 0'u32
  for i in 0 ..< 5:
    if x > pts[i]: y += (min(x, pts[i + 1]) - pts[i]) shl i
  uint16(min(y div 4, 16383))

proc hideRawSubIfdTag(b: var seq[byte]; tag: uint16) =
  ## Renames a tag of the raw SubIFD (tag 330 of IFD0), hiding it from the decoder.
  let ifd = le32At(b, 4)
  for i in 0 ..< le16At(b, ifd):
    let e = ifd + 2 + 12 * i
    if le16At(b, e) == 330:
      let sub = le32At(b, e + 8)
      for j in 0 ..< le16At(b, sub):
        let se = sub + 2 + 12 * j
        if le16At(b, se) == int(tag):
          b[se] = 0xFE
          b[se + 1] = 0xFF
          return
  doAssert false, "tag not found"

proc patchIfd0Short(b: var seq[byte]; tag, v: uint16) =
  let ifd = le32At(b, 4)
  for i in 0 ..< le16At(b, ifd):
    let e = ifd + 2 + 12 * i
    if le16At(b, e) == int(tag):
      b[e + 8] = byte(v and 0xFF)
      b[e + 9] = byte(v shr 8)
      return
  doAssert false, "tag not found"

suite "raw: Sony, Panasonic, Olympus":
  test "cRAW round trip is exact on smooth data":
    let (w, h) = (640, 8)
    let codes = mosaic(scene(w, h), w, [0'u8, 1, 1, 2], 256, 1900)
    let b = sonyCraw(w, h, codes, Curve)
    check identify(b) == some(rfArw)
    let s = decode(b)
    check (s.width, s.height) == (w, h)
    check s.data == codes.mapIt(craTone(it))
    check s.black.values == @[512'f32, 512, 512, 512]
    check s.white == [16383'f32, 16383, 16383]
    check s.cameraWb == some([2.0, 1.0, 1.5])
    let d = developSensor(s, defaultDevelopOptions())
    check (int(d.width), int(d.height)) == (w, h)

  test "cRAW wide ranges lose only low bits":
    let (w, h) = (64, 8)
    var s = 12345'u32
    var codes: seq[uint16]
    for _ in 0 ..< w * h:
      s = s * 1103515245'u32 + 12345'u32
      codes.add uint16((s shr 16) mod 2000)
    let d = decode(sonyCraw(w, h, codes, Curve))
    for y in 0 ..< h:
      for g in 0 ..< w div 32:
        for parity in 0 .. 1:
          var run: seq[uint16]
          for i in 0 ..< 16: run.add codes[y * w + g * 32 + 2 * i + parity]
          let range = max(run) - min(run)
          var step = 1'u16
          while 128'u16 * step <= range: step = step * 2
          for i, c in run:
            let got = d.data[y * w + g * 32 + 2 * i + parity]
            let lo = craTone(if c >= step - 1: c - (step - 1) else: 0)
            check got <= craTone(c) and got >= lo

  test "unusual cRAW variants are unsupported or malformed":
    let codes32 = newSeqWith(32 * 4, 300'u16)
    let ok = sonyCraw(32, 4, codes32, Curve)
    discard decode(ok)
    check kindOf(sonyCraw(32, 4, codes32, [9000'u16, 8000, 12000, 13000])) == reUnsupported
    expect RawError: discard decode(ok[0 ..< ok.len - 20])

  test "cRAW without a black-level tag reads black from the tone curve":
    let (w, h) = (64, 4)
    let codes = newSeqWith(w * h, 1024'u16)
    var b = sonyCraw(w, h, codes, Curve)
    hideRawSubIfdTag(b, 0x7310)
    var s = decode(b)
    check s.black.values == @[512'f32]
    check not s.warnings.anyIt("black level" in it)
    s = decode(sonyCraw(w, h, codes, Curve))
    check s.black.values == @[512'f32, 512, 512, 512]

  test "RW2 round trip, 12 and 14 bits":
    for (bits, w, h) in [(12, 120, 300), (14, 117, 260)]:
      let mx = uint16((1 shl bits) - 1)
      let data = mosaic(scene(w, h), w, [0'u8, 1, 1, 2], 128, mx)
      let b = rw2(w, h, data, bits)
      check b.len > 3 * 0x4000
      check identify(b) == some(rfRw2)
      let s = decode(b)
      check (s.width, s.height) == (w, h)
      check s.data == data
      check s.cfa.get.phase(0, 0) == [0'u8, 1, 1, 2]
      check s.black.values == @[128'f32, 129, 129, 130]
      check s.white == [float32(mx), float32(mx), float32(mx)]
      check s.cameraWb == some([2.0, 1.0, 1.5])
      check s.crop == rawRect(2, 2, w - 4, h - 4)
      let d = developSensor(s, defaultDevelopOptions())
      check (int(d.width), int(d.height)) == (w - 4, h - 4)

  test "compressed RW2 formats fall back":
    let data = newSeqWith(20 * 4, 200'u16)
    var b = rw2(20, 4, data, 12)
    discard decode(b)
    patchIfd0Short(b, 0x002D, 4)
    let e = errOf(b)
    check e.kind == reUnsupported and "raw format 4" in e.msg
    b = rw2(20, 4, data, 12)
    patchIfd0Short(b, 0x0002, 19)
    check kindOf(b) == reUnsupported
    b = rw2(20, 4, data, 12)
    patchIfd0Short(b, 0x0003, 60000)
    var tight = defaultRawLimits()
    tight.maxPixels = 1 shl 16
    check kindOf(b, tight) == reLimitExceeded

  test "ORF uncompressed round trip":
    let (w, h) = (40, 24)
    let data = mosaic(scene(w, h), w, [1'u8, 0, 2, 1], 64, 4095)
    let b = orf(w, h, data)
    check identify(b) == some(rfOrf)
    let s = decode(b)
    check s.data == data
    check s.cfa.get.phase(0, 0) == [1'u8, 0, 2, 1]
    check s.black.values == @[64'f32, 64, 64, 64]
    check s.white == [4095'f32, 4095, 4095]
    check s.cameraWb == some([2.0, 1.0, 1.5])
    check s.crop == rawRect(2, 2, w - 4, h - 4)
    check s.warnings.len == 0
    discard developSensor(s, defaultDevelopOptions())
    let p = embeddedPreview(b).get
    check (p.width, p.height) == (16'u32, 8'u32)

  test "an ORF truncated in IFD0 is malformed, not the compressed variant":
    let (w, h) = (40, 24)
    let b = orf(w, h, mosaic(scene(w, h), w, [1'u8, 0, 2, 1], 64, 4095))
    let ifd = le32At(b, 4)
    let n = le16At(b, ifd)
    var entries: seq[int]
    for i in 0 ..< n: entries.add le16At(b, ifd + 2 + 12 * i)
    proc at(tag: int): int = ifd + 2 + 12 * entries.find(tag)
    let cut = at(279)
    check cut > at(259)
    check kindOf(b[0 ..< cut]) == reMalformed
    for c in 0 ..< b.len:
      try: discard decode(b[0 ..< c])
      except RawError as e:
        if e.kind == reUnsupported: check "compressed" notin e.msg

  test "a packed or compressed ORF falls back":
    let (w, h) = (40, 24)
    var b = orf(w, h, newSeqWith(w * h, 1000'u16))
    let ifd = le32At(b, 4)
    for i in 0 ..< le16At(b, ifd):
      let e = ifd + 2 + 12 * i
      if le16At(b, e) == 279:
        let v = uint32(w * h)
        b[e + 8 .. e + 11] = [byte(v and 0xFF), byte((v shr 8) and 0xFF), byte((v shr 16) and 0xFF), byte(v shr 24)]
    check kindOf(b) == reUnsupported

# ---------------------------------------------------------------- Fujifilm RAF

proc xtrans(packing: RafPacking; bits: uint32): RafSpec =
  ## A 30×18 X-Trans RAF with a 2-pixel masked border.
  let (w, h) = (30, 18)
  let white = uint16((1'u32 shl bits) - 1)
  result = newRafSpec(w, h, mosaicCfa(scene(w, h), w, Xtrans, 6, 256, white), @Xtrans)
  result.bits = bits
  result.packing = packing
  result.black = newSeqWith(36, 256'u32)
  result.crop = some([2'u16, 2, 14, 26])

suite "raw: Fujifilm RAF":
  test "every storage decodes exactly":
    for (packing, bits) in [(u16Packing(true), 14'u32), (u16Packing(false), 16'u32), (lsb12(), 12'u32), (words14(), 14'u32)]:
      let spec = xtrans(packing, bits)
      let b = spec.build()
      check identify(b) == some(rfRaf)
      let s = decode(b)
      check (s.width, s.height) == (30, 18)
      check s.data == spec.data

  test "X-Trans metadata":
    var spec = xtrans(u16Packing(true), 14)
    spec.exposureBias = some((-72'i16, 100'i16))
    spec.orientation = 6
    let s = decode(spec.build())
    let cfa = s.cfa.get
    check (cfa.width, cfa.height) == (6, 6)
    for i, c in Xtrans: check cfa.color(i mod 6, i div 6) == c
    check not cfa.isBayer
    check s.crop == rawRect(2, 2, 26, 14)
    check s.black == uniformBlack(256)
    check s.cameraWb == some([2.0, 1.0, 1.5])
    check abs(s.baselineExposure - 0.72) < 1e-9
    check s.orientation == 6
    check (s.make, s.model) == (some("FUJIFILM"), some("X-Synthetic"))
    check s.warnings.len == 0

  test "X-Trans develops":
    var spec = xtrans(u16Packing(true), 14)
    spec.orientation = 6
    let b = spec.build()
    for m in [dmBilinear, dmMhc, dmAhd]:
      var o = defaultDevelopOptions()
      o.demosaic = m
      let d = develop(b, o)
      check (d.width, d.height) == (14'u32, 26'u32)
      check d.info.format == rfRaf
      check d.info.cfa == some("GRBGBR/BGGRGG/RGGBGG/GBRGRB/RGGBGG/BGGRGG")
      check d.info.wbMultipliers == [2.0, 1.0, 1.5]

  test "X-Trans white balance is per site":
    let (w, h) = (36, 24)
    let rgb = newSeqWith(w * h, [0.4'f32 / 2'f32, 0.4, 0.4'f32 / 1.5'f32])
    var spec = newRafSpec(w, h, mosaicCfa(rgb, w, Xtrans, 6, 256, 16383), @Xtrans)
    spec.black = @[256'u32]
    let d = develop(spec.build())
    for p in 0 ..< int(d.width * d.height):
      check abs(int(d.rgb[p * 3]) - int(d.rgb[p * 3 + 1])) <= 2 and abs(int(d.rgb[p * 3 + 2]) - int(d.rgb[p * 3 + 1])) <= 2

  test "Bayer layouts":
    let (w, h) = (24, 16)
    for cfa in [[0'u8, 1, 1, 2], [2'u8, 1, 1, 0]]:
      for (packing, bits) in [(lsb12(), 12'u32), (words14(), 14'u32), (u16Packing(true), 14'u32)]:
        let white = uint16((1'u32 shl bits) - 1)
        var spec = newRafSpec(w, h, mosaicCfa(scene(w, h), w, cfa, 2, 64, white), @cfa)
        spec.bits = bits
        spec.packing = packing
        spec.black = @[64'u32, 64, 64, 64]
        let s = decode(spec.build())
        check s.data == spec.data
        check s.cfa.get.phase(0, 0) == cfa
        check developSensor(s, defaultDevelopOptions()).info.cfa == some(if cfa[0] == 0: "RGGB" else: "BGGR")

  test "unsupported variants report and keep the preview":
    var spec = xtrans(compressedPacking(), 14)
    let b = spec.build()
    var e = errOf(b)
    check e.kind == reUnsupported and "compressed" in e.msg
    let p = embeddedPreview(b).get
    check (p.width, p.height) == (32'u32, 24'u32)
    spec.packing = u16Packing(true)
    spec.cfaTiff = false
    e = errOf(spec.build())
    check e.kind == reUnsupported and "FinePix" in e.msg

  test "bad layouts and sizes fail cleanly":
    check kindOf(newRafSpec(8, 4, newSeqWith(32, 100'u16), @[0'u8, 0, 2, 2]).build()) == reUnsupported
    let spec = xtrans(u16Packing(true), 14)
    var limits = defaultRawLimits()
    limits.maxPixels = 100
    check kindOf(spec.build(), limits) == reLimitExceeded
    let b = spec.build()
    check kindOf(b[0 ..< b.len - 7]) == reMalformed

# ---------------------------------------------------------------- camera look

const Wb = [1.5, 1.0, 1.25]

proc patches(w, h: int): seq[array[3, float32]] =
  var seed = 12345'u32
  proc rnd(): float32 =
    seed = seed * 1_664_525'u32 + 1_013_904_223'u32
    float32(seed shr 8) / float32(1'u32 shl 24)
  let (pw, ph) = (12, 8)
  var colours: seq[array[3, float32]]
  for _ in 0 ..< pw * ph:
    let a = 0.15'f32 + 0.45'f32 * rnd()
    let b = 0.15'f32 + 0.45'f32 * rnd()
    let c = 0.15'f32 + 0.45'f32 * rnd()
    colours.add [a, b, c]
  for i in 0 ..< w * h:
    let (x, y) = (i mod w, i div w)
    let (px, py) = (x * pw div w, y * ph div h)
    let ramp = 0.15'f32 + 0.85'f32 * float32((x * pw) mod w) / float32(w)
    let c = colours[py * pw + px]
    result.add [c[0] * ramp, c[1] * ramp, c[2] * ramp]

proc trueMatrix(): Mat3 =
  mul(mul(xyzD50ToProphoto(), srgbToXyzD50()), [[1.35, -0.25, -0.1], [-0.15, 1.25, -0.1], [0.0, -0.3, 1.3]])

proc shot(w, h: int; m: Mat3): (Sensor, seq[byte]) =
  ## A NEF-like raw of balanced patches and its JPEG-like rendering `f(m · balanced)` in sRGB.
  var cam: seq[array[3, float32]]
  for p in patches(w, h): cam.add [p[0] / float32(Wb[0]), p[1] / float32(Wb[1]), p[2] / float32(Wb[2])]
  let data = mosaic(cam, w, [0'u8, 1, 1, 2], 0, 4095)
  let s = decode(tiffEp("NIKON CORPORATION", w, h, data, [0'u8, 1, 1, 2], 12))
  let proToSrgb = mul(invert(srgbToXyzD50()).get, rgbToXyz(Romm, D50Xy))
  proc f(x: float64): float64 = 1.6 * x / (1.0 + 0.6 * x)
  proc enc(v0: float64): byte =
    let v = clamp(v0, 0.0, 1.0)
    let e = if v <= 0.0031308: 12.92 * v else: 1.055 * pow(v, 1.0 / 2.4) - 0.055
    byte(round(e * 255.0))
  var rgb: seq[byte]
  for p in cam:
    let bal = [float64(p[0]) * Wb[0], float64(p[1]) * Wb[1], float64(p[2]) * Wb[2]]
    var pro = apply(m, bal)
    for v in pro.mitems: v = f(clamp(v, 0.0, 1.0))
    for v in apply(proToSrgb, pro): rgb.add enc(v)
  (s, rgb)

proc wbOpts(): DevelopOptions =
  result = defaultDevelopOptions()
  result.whiteBalance = multipliersWb(Wb)

suite "raw: camera look":
  test "recovers a known matrix and curve":
    let (w, h) = (480, 320)
    let m = trueMatrix()
    let (s, rgb) = shot(w, h, m)
    let look = fitLook(s, wbOpts(), Reference(width: uint32(w), height: uint32(h), rgb: rgb))
    let fitted = mul(xyzD50ToProphoto(), look.toXyz)
    for i in 0 .. 2:
      for j in 0 .. 2: check abs(fitted[i][j] - m[i][j]) < 0.06
    for x in [0.02, 0.1, 0.3, 0.6]: check abs(look.tone.get.eval(x) - 1.6 * x / (1.0 + 0.6 * x)) < 0.03
    check look.deltaE < 1.5 and look.deltaE < 0.2 * look.deltaEFallback
    var o = wbOpts()
    o.cameraLook = some(look)
    let d = developSensor(s, o)
    check d.warnings.anyIt("fitted to the camera's embedded JPEG" in it)
    check not d.warnings.anyIt("sRGB primaries" in it)
    let plain = developSensor(s, wbOpts())
    check plain.warnings.anyIt("sRGB primaries" in it)
    check d.rgb != plain.rgb

  test "follows the raw orientation":
    let (w, h) = (480, 320)
    var (s, rgb) = shot(w, h, trueMatrix())
    s.orientation = 6
    var up = newSeq[byte](rgb.len)
    for y2 in 0 ..< w:
      for x2 in 0 ..< h:
        let (x, y) = sourceXy(6, w, h, x2, y2)
        for k in 0 .. 2: up[(y2 * h + x2) * 3 + k] = rgb[(y * w + x) * 3 + k]
    check fitLook(s, wbOpts(), Reference(width: uint32(h), height: uint32(w), rgb: up)).deltaE < 1.5
    expect LookRejected: discard fitLook(s, wbOpts(), Reference(width: uint32(w), height: uint32(h), rgb: rgb))

  test "unusable references keep the fallback":
    let (w, h) = (480, 320)
    let (s, rgb) = shot(w, h, trueMatrix())
    let opts = wbOpts()
    proc rejected(s: Sensor; r: Reference): bool =
      try:
        discard fitLook(s, opts, r)
        false
      except LookRejected: true
    var seed = 7'u32
    var noise: seq[byte]
    for _ in 0 ..< rgb.len:
      seed = seed * 1_103_515_245'u32 + 12345'u32
      noise.add byte((seed shr 16) and 0xFF)
    check rejected(s, Reference(width: uint32(w), height: uint32(h), rgb: noise))
    check rejected(s, Reference(width: uint32(w), height: uint32(h), rgb: newSeqWith(rgb.len, 118'u8)))
    check rejected(s, Reference(width: 320, height: 320, rgb: rgb[0 ..< 320 * 320 * 3]))
    check rejected(s, Reference(width: uint32(w), height: uint32(h), rgb: rgb[0 ..< rgb.len - 1]))
    check rejected(s, Reference(width: 12, height: 8, rgb: rgb[0 ..< 12 * 8 * 3]))
    check rejected(s, Reference(width: 0, height: 0, rgb: @[]))
    check rejected(s, Reference(width: high(uint32), height: high(uint32), rgb: rgb))
    var bad = s
    bad.crop.width += 10
    check rejected(bad, Reference(width: uint32(w), height: uint32(h), rgb: rgb))
    var short = s
    short.data.setLen(100)
    check rejected(short, Reference(width: uint32(w), height: uint32(h), rgb: rgb))
