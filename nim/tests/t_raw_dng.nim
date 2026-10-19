## Synthetic DNG files (`raw/tests/dng.rs`): every storage layout decodes to the exact sensor
## samples, development reproduces known colours, and hostile or odd metadata fails cleanly.
## JPEG XL tiles (DNG 1.7) are not decoded by this port: they report "unsupported".

import std/[unittest, options, math, strutils, sequtils]
import photocraft/raw
import photocraft/raw/testgen
import photocraft/zlib
import photocraft/codecs/image as cimage
import photocraft/codecs/jpeg as cjpeg

proc sensor(b: openArray[byte]): Sensor = decode(b)

proc noise(n: int; bits: uint16): seq[uint16] =
  ## Deterministic sensor-like samples below `2^bits`.
  var s = 0x9E37_79B9_7F4A_7C15'u64
  let mx = (1'u32 shl bits) - 1
  for i in 0 ..< n:
    s = s * 6364136223846793005'u64 + 1442695040888963407'u64
    result.add uint16((uint32(i) * 13 + uint32(s shr 40) mod 200) mod (mx + 1))

proc ramp(w, h: int; lo, hi: uint16): seq[uint16] =
  ## A horizontal ramp from `lo` to `hi` (inclusive), the same in every row.
  for i in 0 ..< w * h: result.add lo + uint16((uint32(hi - lo) * uint32(i mod w)) div uint32(w - 1))

proc lin(v: uint16): float64 = pow(float64(v) / 65535.0, 1.8)

proc assertNeutralRamp(d: Developed; name: string): seq[uint16] =
  ## Every pixel has R = G = B, and the first row's grey level rises with the ramp.
  for p in 0 ..< int(d.width * d.height):
    check d.rgb[p * 3] == d.rgb[p * 3 + 1] and d.rgb[p * 3 + 1] == d.rgb[p * 3 + 2]
  for x in 0 ..< int(d.width): result.add d.rgb[x * 3]
  for i in 1 ..< result.len: check result[i] >= result[i - 1]
  check int(result[^1]) > int(result[0]) + 10_000

proc kindOf(b: openArray[byte]; limits = defaultRawLimits()): RawErrorKind =
  try:
    discard decode(b, limits)
    raise newException(AssertionDefect, "decoded")
  except RawError as e: e.kind

const SrgbToXyz = [[0.4124564, 0.3575761, 0.1804375], [0.2126729, 0.7151522, 0.0721750], [0.0193339, 0.1191920, 0.9503041]]

proc inv3(m: array[3, array[3, float64]]): array[3, array[3, float64]] =
  let (a, b, c, d, e, f, g, h, i) = (m[0][0], m[0][1], m[0][2], m[1][0], m[1][1], m[1][2], m[2][0], m[2][1], m[2][2])
  let det = a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
  [[(e * i - f * h) / det, (c * h - b * i) / det, (b * f - c * e) / det],
   [(f * g - d * i) / det, (a * i - c * g) / det, (c * d - a * f) / det],
   [(d * h - e * g) / det, (b * g - a * h) / det, (a * e - b * d) / det]]

proc ap(m: array[3, array[3, float64]]; v: array[3, float64]): array[3, float64] =
  for i in 0 .. 2: result[i] = m[i][0] * v[0] + m[i][1] * v[1] + m[i][2] * v[2]

proc flat(m: array[3, array[3, float64]]): array[9, float64] =
  [m[0][0], m[0][1], m[0][2], m[1][0], m[1][1], m[1][2], m[2][0], m[2][1], m[2][2]]

proc referenceProphoto(camera, neutral: array[3, float64]): array[3, float64] =
  ## The DNG model computed independently: camera XYZ under an illuminant whose camera neutral is
  ## `neutral`, Bradford-adapted from that white (Y = 1) to D50, then XYZ D50 → linear ProPhoto.
  const
    Bradford = [[0.8951, 0.2664, -0.1614], [-0.7502, 1.7135, 0.0367], [0.0389, -0.0685, 1.0296]]
    D50 = [0.9642, 1.0, 0.8249]
    XyzD50ToProphoto = [[1.3459433, -0.2556075, -0.0511118], [-0.5445989, 1.5081673, 0.0205351], [0.0, 0.0, 1.2118128]]
  var white = ap(SrgbToXyz, neutral)
  let wy = white[1]
  for v in white.mitems: v /= wy
  var xyz = ap(SrgbToXyz, camera)
  let ny = ap(SrgbToXyz, neutral)[1]
  for v in xyz.mitems: v /= ny
  let (s, d) = (ap(Bradford, white), ap(Bradford, D50))
  let cone = ap(Bradford, xyz)
  let adapted = ap(inv3(Bradford), [cone[0] * d[0] / s[0], cone[1] * d[1] / s[1], cone[2] * d[2] / s[2]])
  ap(XyzD50ToProphoto, adapted)

proc findBytes(hay, needle: openArray[byte]): int =
  for i in 0 .. hay.len - needle.len:
    var ok = true
    for k in 0 ..< needle.len:
      if hay[i + k] != needle[k]:
        ok = false
        break
    if ok: return i
  -1

suite "raw: DNG":
  test "identifies DNG":
    let b = dngCfa(4, 4, newSeq[uint16](16)).build()
    check identify(b) == some(rfDng)
    check isRaw(b)
    check not isRaw(@[byte('I'), byte('I'), 42, 0, 8, 0, 0, 0, 0, 0])
    check not isRaw(@[0x89'u8, byte('P'), byte('N'), byte('G'), 13, 10, 26, 10])

  test "storage layouts decode exactly":
    let (w, h) = (37, 23) # odd sizes exercise partial strips and edge tiles
    for (name, bits, storage, be) in [("16-bit strips", 16'u16, strips(5), false), ("16-bit big-endian", 16'u16, strips(23), true),
                                      ("12-bit packed", 12'u16, strips(4), false), ("10-bit packed BE", 10'u16, strips(7), true),
                                      ("8-bit", 8'u16, strips(9), false), ("LJ92 tiles", 14'u16, lj92Tiles(16, 8), false),
                                      ("LJ92 tiles BE", 16'u16, lj92Tiles(32, 32), true), ("LJ92 strips", 12'u16, lj92Strips(6), false)]:
      let data = noise(w * h, bits)
      var spec = dngCfa(w, h, data)
      spec.bits = bits
      spec.storage = storage
      spec.bigEndian = be
      spec.white = (1'u32 shl bits) - 1
      let s = sensor(spec.build())
      check (s.width, s.height, s.samples) == (w, h, 1)
      check s.data == data
      check s.format == rfDng

  test "LinearRaw decodes and develops":
    let (w, h) = (20, 10)
    let data = noise(w * h * 3, 16)
    var spec = dngCfa(w, h, data)
    spec.samples = 3
    spec.storage = lj92Tiles(16, 16)
    let s = sensor(spec.build())
    check s.samples == 3 and s.cfa.isNone
    check s.data == data
    let d = developSensor(s, defaultDevelopOptions())
    check (d.width, d.height, d.rgb.len) == (20'u32, 10'u32, 600)
    check d.info.cfa.isNone

  test "monochrome LinearRaw develops to neutral grey (#3497)":
    let (w, h) = (37, 23)
    for (name, bits, storage) in [("16-bit strips", 16'u16, strips(5)), ("12-bit packed", 12'u16, strips(4)),
                                  ("8-bit", 8'u16, strips(23)), ("LJ92 strips", 14'u16, lj92Strips(6)),
                                  ("LJ92 tiles", 16'u16, lj92Tiles(16, 8)), ("deflate predictor 2", 16'u16, deflateStrips(7, 2)),
                                  ("lossy JPEG tile", 16'u16, lossyJpegTiles(37, 23))]:
      let white = uint16((1'u32 shl bits) - 1)
      let data = ramp(w, h, 0, white)
      var spec = dngMonochrome(w, h, data)
      spec.bits = bits
      spec.white = uint32(white)
      spec.storage = storage
      let lossy = storage.kind == dsLossyJpegTiles
      let s = sensor(spec.build())
      check s.samples == 1 and s.cfa.isNone and s.isMonochrome
      if not lossy: check s.data == data
      let d = developSensor(s, defaultDevelopOptions())
      check (d.width, d.height, d.rgb.len) == (37'u32, 23'u32, 37 * 23 * 3)
      check d.info.cfa.isNone and d.info.wbMultipliers == [1.0, 1.0, 1.0]
      for wn in d.warnings: check "colour" notin wn and "white balance" notin wn
      let row = assertNeutralRamp(d, name)
      if not lossy: check (row[0], row[^1]) == (0'u16, 65535'u16)

  test "monochrome levels and white balance":
    let (w, h) = (32, 4)
    var spec = dngMonochrome(w, h, ramp(w, h, 1000, 60000))
    spec.black = @[1000'u32]
    spec.white = 60000
    let s = sensor(spec.build())
    for wbv in [asShotWb(), autoWb(), multipliersWb([2.0, 1.0, 1.5])]:
      var o = defaultDevelopOptions()
      o.whiteBalance = wbv
      let row = assertNeutralRamp(developSensor(s, o), $wbv.kind)
      check (row[0], row[w - 1]) == (0'u16, 65535'u16)
    var o = defaultDevelopOptions()
    o.exposure = -1.0
    let row = assertNeutralRamp(developSensor(s, o), "exposure -1")
    check row[w - 1] < 60000

  test "monochrome with odd colour metadata develops":
    let (w, h) = (16, 6)
    let base = dngMonochrome(w, h, ramp(w, h, 100, 4000))
    proc rat(v: openArray[float64]): Val =
      var r: seq[(int32, int32)]
      for x in v: r.add (int32(x * 10000.0), 10000'i32)
      vSRational(r)
    var variants: seq[DngSpec]
    var s = base
    s.ifd0Extra = @[(50721'u16, rat([0.3, 0.6, 0.1])), (50778'u16, vShort([21'u16]))]
    variants.add s
    s = base
    s.ifd0Extra = @[(50721'u16, rat([1.0]))]
    variants.add s
    s = base
    s.ifd0Extra = @[(50728'u16, vRational([(1'u32, 1'u32)]))]
    variants.add s
    for bl in [@[100'u32, 120], @[100'u32, 110, 120]]:
      s = base
      s.black = bl
      variants.add s
    s = base
    s.black = @[100'u32, 101, 102, 103]
    s.blackRepeat = (2'u16, 2'u16)
    variants.add s
    s = base
    s.black = @[100'u32]
    s.blackRepeat = (2'u16, 2'u16)
    variants.add s
    s = base
    s.colorMatrix1 = some((21'u16, [3.2, -1.5, -0.5, -1.0, 1.9, 0.04, 0.06, -0.2, 1.06]))
    variants.add s
    s = base
    s.asShotNeutral = some([0.5, 1.0, 0.7])
    variants.add s
    for i, v in variants: discard assertNeutralRamp(develop(v.build()), $i)

  test "broken monochrome DNGs error":
    let (w, h) = (16, 6)
    let spec = dngMonochrome(w, h, ramp(w, h, 100, 4000))
    let b = spec.build()
    for n in 0 ..< b.len:
      try:
        let s = decode(b[0 ..< n])
        try: discard developSensor(s, defaultDevelopOptions())
        except RawError: discard
      except RawError: discard
    var first: seq[byte]
    for v in spec.data[0 ..< 4]: first.add [byte(v and 0xFF), byte(v shr 8)]
    let strip = findBytes(b, first)
    check strip >= 0
    expect RawError: discard decode(b[0 ..< strip + w])
    var two = spec
    two.samples = 2
    two.data = ramp(2 * w, h, 100, 4000)
    try:
      discard decode(two.build())
      check false
    except RawError as e:
      check "2 samples per pixel" in e.msg
    var s = sensor(spec.build())
    s.data.setLen(10)
    expect RawError: discard developSensor(s, defaultDevelopOptions())

  test "an orientation past u16 is not truncated into range (#1817)":
    var spec = dngCfa(8, 6, newSeqWith(48, 500'u16))
    spec.orientation = 2
    proc withLong(spec: DngSpec; frm: uint16; to: uint32): seq[byte] =
      result = spec.build()
      let short = @[0x12'u8, 0x01, 3, 0, 1, 0, 0, 0, byte(frm and 0xFF), byte(frm shr 8), 0, 0]
      let at = findBytes(result, short)
      doAssert at >= 0
      let long = @[0x12'u8, 0x01, 4, 0, 1, 0, 0, 0, byte(to and 0xFF), byte((to shr 8) and 0xFF),
                   byte((to shr 16) and 0xFF), byte(to shr 24)]
      for k, v in long: result[at + k] = v
    check sensor(withLong(spec, 2, 65_538)).orientation == 1
    check sensor(withLong(spec, 2, 6)).orientation == 6

  test "metadata is read":
    let (w, h) = (40, 30)
    var spec = dngCfa(w, h, newSeqWith(w * h, 1000'u16))
    spec.bits = 14
    spec.white = 15000
    spec.black = @[100'u32, 101, 102, 103]
    spec.blackRepeat = (2'u16, 2'u16)
    spec.activeArea = some([2'u32, 4, 28, 36])
    spec.defaultCrop = some(([2'u32, 1], [20'u32, 16]))
    spec.cfa = [1'u8, 0, 2, 1]
    spec.asShotNeutral = some([0.5, 1.0, 0.8])
    spec.baselineExposure = some(0.5)
    spec.orientation = 6
    var l: seq[uint16]
    for v in 0'u32 ..< 4096: l.add uint16(min(v * 3, 65535))
    spec.linearization = some(l)
    let s = sensor(spec.build())
    check s.white == [15000'f32, 15000, 15000]
    check s.black.values == @[100'f32, 101, 102, 103]
    check s.active == rawRect(4, 2, 32, 26)
    check s.crop == rawRect(6, 3, 20, 16)
    check s.cfa.get.phase(4, 2) == [1'u8, 0, 2, 1]
    check s.color.asShotNeutral == some([0.5, 1.0, 0.8])
    check abs(s.baselineExposure - 0.5) < 1e-6
    check s.orientation == 6
    check s.linearization.get.len == 4096
    var d = developSensor(s, defaultDevelopOptions())
    check (d.width, d.height) == (16'u32, 20'u32)
    check d.info.wbMultipliers == [2.0, 1.0, 1.25]
    var o = defaultDevelopOptions()
    o.orient = false
    d = developSensor(s, o)
    check (d.width, d.height) == (20'u32, 16'u32)

  test "an enormous double crop origin is rejected":
    var spec = dngCfa(16, 12, newSeqWith(16 * 12, 1000'u16))
    spec.activeArea = some([2'u32, 3, 10, 14])
    spec.defaultCrop = some(([0'u32, 0], [11'u32, 8]))
    spec.defaultCropOriginDouble = some([1e300, 0.0])
    check kindOf(spec.build()) == reMalformed

  test "out-of-range default crops fall back to the active area with a warning":
    for which in 0 .. 1:
      var spec = dngCfa(16, 12, newSeqWith(16 * 12, 1000'u16))
      spec.activeArea = some([2'u32, 3, 10, 14])
      if which == 0: spec.defaultCrop = some(([10'u32, 0], [5'u32, 4]))
      else: spec.defaultCropOriginDouble = some([1.0, 1.0]) # the size defaults to the whole image
      let s = sensor(spec.build())
      check s.crop == s.active
      check s.warnings.anyIt("out-of-range" in it and "default crop" in it)

  test "a size-only default crop starts at the active area origin (#950)":
    var data: seq[uint16]
    for i in 0 ..< 64: data.add uint16(i * 37)
    var spec = dngCfa(8, 8, data)
    spec.activeArea = some([0'u32, 0, 8, 8])
    spec.defaultCropSizeOnly = some([4'u32, 4])
    let bytes = spec.build()
    let s = sensor(bytes)
    check s.crop == rawRect(0, 0, 4, 4)
    check not s.warnings.anyIt("default crop" in it)
    let d = develop(bytes)
    check (d.width, d.height) == (4'u32, 4'u32)
    var spec2 = dngCfa(16, 12, newSeqWith(16 * 12, 1000'u16))
    spec2.activeArea = some([2'u32, 3, 10, 14])
    spec2.defaultCropSizeOnly = some([6'u32, 4])
    check sensor(spec2.build()).crop == rawRect(3, 2, 6, 4)

  test "colour-matrix development matches the reference":
    let (w, h) = (48, 32)
    let gains = [0.5'f32, 1.0, 0.7]
    let patches = [[0.18'f32, 0.18, 0.18], [0.40'f32, 0.10, 0.05], [0.05'f32, 0.30, 0.10], [0.10'f32, 0.12, 0.45]]
    var rgb: seq[array[3, float32]]
    for i in 0 ..< w * h:
      let p = patches[((i mod w) div (w div 2)) + 2 * ((i div w) div (h div 2))]
      rgb.add [p[0] * gains[0], p[1] * gains[1], p[2] * gains[2]]
    var spec = dngCfa(w, h, mosaic(rgb, w, [0'u8, 1, 1, 2], 256, 16383))
    spec.bits = 14
    spec.black = @[256'u32]
    spec.white = 16383
    spec.colorMatrix1 = some((21'u16, flat(inv3(SrgbToXyz))))
    let g = [float64(gains[0]), float64(gains[1]), float64(gains[2])]
    spec.asShotNeutral = some(g)
    for storage in [strips(32), lj92Tiles(16, 16)]:
      spec.storage = storage
      for meth in [dmBilinear, dmMhc, dmAhd]:
        var o = defaultDevelopOptions()
        o.demosaic = meth
        let d = develop(spec.build(), o)
        for pi, p in patches:
          let (cx, cy) = ((pi mod 2) * (w div 2) + w div 4, (pi div 2) * (h div 2) + h div 4)
          let want = referenceProphoto([float64(p[0]) * g[0], float64(p[1]) * g[1], float64(p[2]) * g[2]], g)
          for c in 0 .. 2:
            check abs(lin(d.rgb[(cy * w + cx) * 3 + c]) - want[c]) < 0.006

  test "the forward-matrix path maps the neutral to white":
    let (w, h) = (16, 16)
    var spec = dngCfa(w, h, mosaic(newSeqWith(w * h, [0.3'f32, 0.6, 0.45]), w, [0'u8, 1, 1, 2], 0, 65535))
    let cm = flat(inv3(SrgbToXyz))
    spec.colorMatrix1 = some((17'u16, cm))
    spec.colorMatrix2 = some((21'u16, cm))
    let fm = [0.4360747, 0.3850649, 0.1430804, 0.2225045, 0.7168786, 0.0606169, 0.0139322, 0.0971045, 0.7141733]
    spec.forwardMatrix1 = some(fm)
    spec.forwardMatrix2 = some(fm)
    spec.asShotNeutral = some([0.5, 1.0, 0.75])
    let d = develop(spec.build())
    for c in 0 .. 2: check abs(lin(d.rgb[(8 * w + 8) * 3 + c]) - 0.6) < 0.004

  test "highlights clip to white and exposure applies":
    var spec = dngCfa(8, 8, newSeqWith(64, 65535'u16))
    spec.asShotNeutral = some([0.5, 1.0, 0.6])
    check develop(spec.build()).rgb.allIt(it >= 65500)
    spec = dngCfa(8, 8, newSeqWith(64, 16384'u16))
    spec.asShotNeutral = some([1.0, 1.0, 1.0])
    let b = spec.build()
    let base = develop(b).rgb[27]
    var o = defaultDevelopOptions()
    o.exposure = 1.0
    let up = develop(b, o).rgb[27]
    check abs(lin(up) / lin(base) - 2.0) < 0.02

  test "the gain-map opcode is applied":
    let (w, h) = (16, 12)
    var spec = dngCfa(w, h, newSeqWith(w * h, 16384'u16))
    spec.asShotNeutral = some([1.0, 1.0, 1.0])
    let base = develop(spec.build()).rgb[(6 * w + 8) * 3 + 1]
    spec.opcodeList2 = some(gainMapOpcodeList([0'u32, 0, uint32(h), uint32(w)], 1, [[2'f32, 2], [2'f32, 2]]))
    let d = develop(spec.build())
    check not d.warnings.anyIt("Opcode" in it)
    check abs(lin(d.rgb[(6 * w + 8) * 3 + 1]) / lin(base) - 2.0) < 0.02

  test "white-balance options":
    let (w, h) = (16, 16)
    let b = dngCfa(w, h, mosaic(newSeqWith(w * h, [0.2'f32, 0.4, 0.3]), w, [0'u8, 1, 1, 2], 0, 65535)).build()
    var d = develop(b)
    check d.warnings.anyIt("grey world" in it)
    let o = (8 * w + 8) * 3
    check abs(int(d.rgb[o]) - int(d.rgb[o + 1])) < 300 and abs(int(d.rgb[o + 2]) - int(d.rgb[o + 1])) < 300
    var op = defaultDevelopOptions()
    op.whiteBalance = multipliersWb([2.0, 1.0, 4.0 / 3.0])
    d = develop(b, op)
    check [round(d.info.wbMultipliers[0] * 1000), round(d.info.wbMultipliers[1] * 1000), round(d.info.wbMultipliers[2] * 1000)] ==
      [2000.0, 1000.0, 1333.0]
    op.whiteBalance = multipliersWb([0.0, 1.0, 1.0])
    expect RawError: discard develop(b, op)

  test "a smooth scene develops alike with every method":
    let (w, h) = (64, 48)
    var spec = dngCfa(w, h, mosaic(scene(w, h), w, [2'u8, 1, 1, 0], 0, 65535))
    spec.cfa = [2'u8, 1, 1, 0]
    spec.asShotNeutral = some([1.0, 1.0, 1.0])
    let b = spec.build()
    var outs: seq[seq[uint16]]
    for m in [dmBilinear, dmMhc, dmAhd]:
      var o = defaultDevelopOptions()
      o.demosaic = m
      outs.add develop(b, o).rgb
    for y in 4 ..< h - 4:
      for x in 4 ..< w - 4:
        for c in 0 .. 2:
          let i = (y * w + x) * 3 + c
          check abs(int(outs[0][i]) - int(outs[1][i])) < 800 and abs(int(outs[0][i]) - int(outs[2][i])) < 800

  test "unsupported variants say so":
    var spec = dngCfa(8, 8, newSeq[uint16](64))
    spec.cfa = [0'u8, 1, 2, 1]
    check kindOf(spec.build()) == reUnsupported

  test "limits are enforced before allocating":
    let b = dngCfa(64, 64, newSeq[uint16](64 * 64)).build()
    var tiny = defaultRawLimits()
    tiny.maxWidth = 32
    check kindOf(b, tiny) == reLimitExceeded
    tiny = defaultRawLimits()
    tiny.maxAlloc = 1000
    check kindOf(b, tiny) == reLimitExceeded

  test "deflate strips round trip":
    let (w, h) = (48, 40)
    for (name, bits, storage, be) in [("16-bit", 16'u16, deflateStrips(16, 1), false), ("16-bit p2", 16'u16, deflateStrips(16, 2), false),
                                      ("12-bit p2", 12'u16, deflateStrips(12, 2), false), ("16-bit p2 BE", 16'u16, deflateStrips(16, 2), true),
                                      ("LinearRaw p2", 16'u16, deflateStrips(40, 2), false)]:
      let samples = if "LinearRaw" in name: 3 else: 1
      let data = noise(w * h * samples, bits)
      var spec = dngCfa(w, h, data)
      spec.samples = samples
      spec.bits = bits
      spec.storage = storage
      spec.bigEndian = be
      spec.white = (1'u32 shl bits) - 1
      let s = sensor(spec.build())
      check (s.width, s.height, s.samples) == (w, h, samples)
      check s.data == data

  test "a cut deflate stream fails cleanly":
    var spec = dngCfa(32, 32, noise(32 * 32, 16))
    spec.storage = deflateStrips(32, 2)
    let bytes = spec.build()
    check sensor(bytes).data.len == 32 * 32
    let at = findBytes(bytes, [0x78'u8, 0x9c]) + 2
    expect RawError: discard decode(bytes[0 ..< at + (bytes.len - at) div 2])

  test "8-bit predictor wraps; packed 12-bit deflate decodes":
    let (w, h) = (16, 4)
    var data8: seq[uint16]
    for i in 0 ..< w * h: data8.add(if i mod w < w div 2: 250'u16 else: 10'u16)
    var spec = dngCfa(w, h, data8)
    spec.bits = 8
    spec.white = 255
    spec.storage = deflateStrips(h, 2)
    check sensor(spec.build()).data == data8
    var data12: seq[uint16]
    for i in 0 ..< w * h: data12.add uint16(i * 61) and 0xFFF
    spec = dngCfa(w, h, data12)
    spec.bits = 12
    spec.white = 4095
    spec.storage = deflateStrips(h, 1)
    var bytes = spec.build()
    var containers: seq[byte]
    for v in data12: containers.add [byte(v and 0xFF), byte(v shr 8)]
    let old = zlibCompress(containers)
    var packed: seq[byte]
    for i in countup(0, data12.len - 1, 2):
      let (a, b) = (data12[i], data12[i + 1])
      packed.add [byte(a shr 4), byte(((a and 0xF) shl 4) or (b shr 8)), byte(b and 0xFF)]
    var nw = zlibCompress(packed)
    let at = findBytes(bytes, old)
    check at >= 0 and nw.len <= old.len
    nw.setLen(old.len) # trailing bytes after the zlib stream are ignored
    for k, v in nw: bytes[at + k] = v
    check sensor(bytes).data == data12

  test "JPEG XL tiles are unsupported in this port":
    var spec = dngCfa(16, 8, noise(16 * 8 * 3, 16))
    spec.samples = 3
    spec.storage = jxlStrips(8)
    spec.jxlSegments = @[@[0xff'u8, 0x0a, 1, 2, 3, 4, 5]]
    try:
      discard decode(spec.build())
      check false
    except RawError as e:
      check e.kind == reUnsupported and "JPEG XL" in e.msg

  test "ProfileGainTableMap is reported":
    var spec = dngCfa(8, 8, newSeqWith(64, 500'u16))
    check not sensor(spec.build()).warnings.anyIt("ProfileGainTableMap" in it)
    spec.profileGainTableMap = some(newSeq[byte](64))
    check sensor(spec.build()).warnings.anyIt("ProfileGainTableMap" in it)

  test "lossy JPEG tiles decode within the quality band":
    let (w, h) = (32, 32)
    var data: seq[uint16]
    for i in 0 ..< w * h: data.add 4096'u16 + uint16(i div w) * 16
    var spec = dngCfa(w, h, data)
    spec.storage = lossyJpegTiles(16, 16)
    let s = sensor(spec.build())
    check (s.width, s.height, s.samples) == (w, h, 1)
    var worst = 0
    for i, got in s.data: worst = max(worst, abs(int(got) - int((data[i] shr 8) shl 8)))
    check worst <= 4 * 256

  test "a cut lossy JPEG tile fails cleanly":
    var spec = dngCfa(32, 32, newSeqWith(32 * 32, 8192'u16))
    spec.storage = lossyJpegTiles(32, 32)
    let bytes = spec.build()
    let at = findBytes(bytes, [0xFF'u8, 0xD8]) + 2
    expect RawError: discard decode(bytes[0 ..< at + (bytes.len - at) div 2])

  test "a lossy tile whose header lies about its size errors":
    var data: seq[uint16]
    for i in 0 ..< 32 * 32: data.add 4096'u16 + uint16(i div 32) * 16
    var spec = dngCfa(32, 32, data)
    spec.storage = lossyJpegTiles(32, 32)
    var bytes = spec.build()
    var px = newSeq[byte](16 * 16)
    for i in 0 ..< 256: px[i] = byte((i mod 16) * 16)
    let small = encodeJpeg(imageFromRaw(16, 16, clGray, isU8, px), clGray, EncodeOptions(jpegQuality: 60))
    let soi = findBytes(bytes, [0xFF'u8, 0xD8, 0xFF])
    let eoi = soi + findBytes(bytes[soi .. ^1], [0xFF'u8, 0xD9]) + 2
    bytes = bytes[0 ..< soi] & small & bytes[eoi .. ^1]
    try:
      discard decode(bytes)
      check false
    except RawError as e:
      check "16x16" in e.msg and "32x32" in e.msg

  test "lossy RGB LinearRaw tiles keep their channels apart":
    let (w, h) = (32, 32)
    var data: seq[uint16]
    for i in 0 ..< w * h * 3: data.add 4096'u16 + uint16(i div (w * 3)) * 16 + uint16(i mod 3) * 64 * 256
    var spec = dngCfa(w, h, data)
    spec.samples = 3
    spec.storage = lossyJpegTiles(16, 16)
    let s = sensor(spec.build())
    check (s.width, s.height, s.samples) == (w, h, 3)
    var worst = 0
    for i, got in s.data: worst = max(worst, abs(int(got) - int((data[i] shr 8) shl 8)))
    check worst <= 8 * 256
    for p in 0 ..< w * h: check abs(int(s.data[p * 3]) - int(s.data[p * 3 + 2])) >= 64 * 256 - 8 * 256
