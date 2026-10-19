## Truncated and corrupted raw files fail cleanly, never crash and never allocate beyond the
## limits (`raw/tests/hostile.rs`). Every decode, develop and colour fit below may raise
## `RawError` / `LookRejected`; anything else (an IndexDefect, an overflow) fails the test.

import std/[unittest, options, sequtils]
import photocraft/raw
import photocraft/raw/testgen

proc samples(): seq[seq[byte]] =
  let (w, h) = (24, 12)
  let cfa = mosaic(scene(w, h), w, [0'u8, 1, 1, 2], 64, 4000)
  var d = dngCfa(w, h, cfa)
  d.bits = 12
  d.white = 4000
  d.black = @[64'u32]
  d.asShotNeutral = some([0.5, 1.0, 0.7])
  d.colorMatrix1 = some((21'u16, [3.2, -1.5, -0.5, -1.0, 1.9, 0.04, 0.06, -0.2, 1.06]))
  result.add d.build()
  d.storage = lj92Tiles(16, 8)
  d.activeArea = some([1'u32, 2, 11, 22])
  d.defaultCrop = some(([1'u32, 1], [16'u32, 8]))
  result.add d.build()
  d.storage = strips(5)
  d.bigEndian = true
  result.add d.build()
  # LinearRaw in one JPEG XL strip: unsupported here, which must stay an error too.
  var rgb: seq[uint16]
  for p in scene(w, h):
    for v in p: rgb.add uint16(v * 30000'f32)
  var j = dngCfa(w, h, rgb)
  j.samples = 3
  j.storage = jxlStrips(h)
  j.jxlSegments = @[@[0xFF'u8, 0x0A, 0xFA, 0x7F, 0x01, 0x90, 0x08, 0x06]]
  j.profileGainTableMap = some(newSeq[byte](32))
  result.add j.build()
  var grey: seq[uint16]
  for p in scene(w, h): grey.add 64'u16 + uint16(p[1] * 3900'f32)
  var m = dngMonochrome(w, h, grey)
  m.bits = 12
  m.white = 4000
  m.black = @[64'u32]
  m.storage = lj92Tiles(16, 8)
  result.add m.build()
  m.storage = deflateStrips(5, 2)
  result.add m.build()
  var data = newSeq[uint16](40 * 10)
  for i in 0 ..< data.len: data[i] = 600'u16 + uint16((i * 7) mod 3000)
  result.add Cr2Spec(width: 40, height: 10, data: data, precision: 14, components: 2, slices: @[16, 16, 8],
                     borders: some([8'u16, 2, 39, 9]), wbRggb: some([2000'u16, 1024, 1024, 1500]), orientation: 8).build()
  result.add tiffEp("NIKON", w, h, cfa, [0'u8, 1, 1, 2], 12)
  result.add sonyCraw(64, 6, mosaic(scene(64, 6), 64, [0'u8, 1, 1, 2], 256, 1900), [8000'u16, 10400, 12900, 14100])
  result.add rw2(30, 8, mosaic(scene(30, 8), 30, [0'u8, 1, 1, 2], 128, 4095), 12)
  result.add orf(w, h, cfa)
  let xt = mosaicCfa(scene(w, h), w, Xtrans, 6, 256, 4000)
  for packing in [u16Packing(true), lsb12()]:
    var r = newRafSpec(w, h, xt, @Xtrans)
    r.bits = 12
    r.packing = packing
    r.black = newSeqWith(36, 256'u32)
    r.crop = some([1'u16, 2, 10, 20])
    r.orientation = 8
    result.add r.build()
  var bayer = newRafSpec(w, h, cfa, @[0'u8, 1, 1, 2])
  bayer.packing = words14()
  bayer.exposureBias = some((-100'i16, 100'i16))
  result.add bayer.build()

proc exercise(b: openArray[byte]) =
  discard identify(b)
  discard embeddedPreview(b)
  discard embeddedPreviews(b)
  var opts = defaultDevelopOptions()
  opts.limits.maxAlloc = 64'u64 shl 20
  var s: Sensor
  try: s = decode(b, opts.limits)
  except RawError: return
  for m in [dmBilinear, dmAhd]:
    var o = opts
    o.demosaic = m
    try: discard developSensor(s, o)
    except RawError: discard
  # A colour fit against a gradient of the crop's (capped) shape, then a develop with it.
  let (w, h) = (min(max(s.crop.width, 0), 256), min(max(s.crop.height, 0), 256))
  var rgb = newSeq[byte](w * h * 3)
  for i in 0 ..< rgb.len: rgb[i] = byte((i * 7) mod 251)
  try:
    let look = fitLook(s, opts, Reference(width: uint32(w), height: uint32(h), rgb: rgb))
    var o = opts
    o.cameraLook = some(look)
    try: discard developSensor(s, o)
    except RawError: discard
  except LookRejected: discard

suite "raw: hostile input":
  test "the originals develop (the JPEG XL one is unsupported here)":
    for i, b in samples():
      if i == 3:
        expect RawError: discard develop(b)
      else: discard develop(b)

  test "every truncation fails cleanly":
    for b in samples():
      var n = 0
      while n < b.len:
        exercise(b.toOpenArray(0, n - 1))
        n += 3

  test "random corruption never crashes":
    var s = 0xDEAD_BEEF'u64
    proc next(): uint64 =
      s = s * 6364136223846793005'u64 + 1442695040888963407'u64
      s shr 33
    for b in samples():
      for _ in 0 ..< 400:
        var c = b
        for _ in 0'u64 ..< 1 + next() mod 6:
          let at = int(next() mod uint64(c.len))
          c[at] = case next() mod 4
            of 0: 0xFF'u8
            of 1: 0'u8
            else: byte(next() and 0xFF)
        exercise(c)

  test "absurd dimensions are rejected without allocating":
    var d = dngCfa(4, 4, newSeq[uint16](16))
    d.storage = strips(4)
    var b = d.build()
    var patched = 0
    for i in 0 ..< max(b.len - 12, 0):
      if b[i] == 0 and b[i + 1] == 1 and b[i + 2] == 4 and b[i + 3] == 0 and b[i + 8] == 4 and b[i + 9] == 0 and
          b[i + 10] == 0 and b[i + 11] == 0:
        for (at, v) in [(i + 8, 200_000'u32), (i + 20, 200_000'u32)]:
          for k in 0 .. 3: b[at + k] = byte((v shr (8 * k)) and 0xFF)
        inc patched
    check patched > 0
    try:
      discard decode(b)
      check false
    except RawError as e: check e.kind == reLimitExceeded

  proc oversizedLj92Tile(): seq[byte] =
    ## An 8×8 CFA image stored as one lossless-JPEG tile that the file declares 512×512 (#2925).
    var d = dngCfa(8, 8, mosaic(scene(8, 8), 8, [0'u8, 1, 1, 2], 64, 4000))
    d.bits = 12
    d.white = 4000
    d.black = @[64'u32]
    d.storage = lj92Tiles(512, 512)
    d.build()

  proc patchLong(b: var seq[byte]; tag: uint16; to: uint32): bool =
    var patched = 0
    for i in 0 ..< max(b.len - 12, 0):
      if b[i] == byte(tag and 0xFF) and b[i + 1] == byte(tag shr 8) and b[i + 2] == 4 and b[i + 3] == 0 and
          b[i + 4] == 1 and b[i + 5] == 0 and b[i + 6] == 0 and b[i + 7] == 0:
        for k in 0 .. 3: b[i + 8 + k] = byte((to shr (8 * k)) and 0xFF)
        inc patched
    patched == 1

  proc limited(maxAlloc: uint64): RawLimits =
    result = defaultRawLimits()
    result.maxAlloc = maxAlloc

  test "the lossless-JPEG intermediate is charged against maxAlloc (#2925)":
    let b = oversizedLj92Tile()
    discard decode(b)
    try:
      discard decode(b, limited(64'u64 shl 10))
      check false
    except RawError as e: check e.kind == reLimitExceeded
    discard decode(b, limited(4'u64 shl 20))
    var normal = dngCfa(8, 8, mosaic(scene(8, 8), 8, [0'u8, 1, 1, 2], 64, 4000))
    normal.bits = 12
    normal.white = 4000
    normal.black = @[64'u32]
    normal.storage = lj92Tiles(4, 4)
    discard decode(normal.build(), limited(4096))

  test "a lossless-JPEG frame larger than the budget is refused":
    var b = oversizedLj92Tile()
    check patchLong(b, 322, 256)
    check patchLong(b, 323, 256)
    try:
      discard decode(b, limited(256'u64 shl 10))
      check false
    except RawError as e: check e.kind == reLimitExceeded
