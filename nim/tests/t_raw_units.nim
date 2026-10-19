## Unit tests of the raw decoder's parts (the `#[cfg(test)]` modules of `crates/raw/src`): TIFF
## reading, colour science, lossless JPEG, opcodes, packings, Sony / Panasonic / Fujifilm
## helpers, Nikon padding, camera profiles, CR2 phase measurement, demosaicing, the gamma table,
## orientation and the camera-look tone curve.

import std/[unittest, options, math, sequtils, strutils]
import photocraft/raw
import photocraft/raw/[tiff, color, ljpeg, opcodes, sensor, raf, rw2, sony, tiffep, cameras, cr2, demosaic, develop, tone, look, testgen]

proc bytesOf(s: string): seq[byte] =
  for c in s: result.add byte(c)

proc le16(v: uint16): seq[byte] = @[byte(v and 0xFF), byte(v shr 8)]
proc le32(v: uint32): seq[byte] = @[byte(v and 0xFF), byte((v shr 8) and 0xFF), byte((v shr 16) and 0xFF), byte(v shr 24)]
proc be32(v: uint32): seq[byte] = @[byte(v shr 24), byte((v shr 16) and 0xFF), byte((v shr 8) and 0xFF), byte(v and 0xFF)]

suite "raw: TIFF":
  test "orientation is range-checked before narrowing (#1817)":
    for (v, want) in [(none(uint32), 1'u16), (some(0'u32), 1'u16), (some(9'u32), 1'u16), (some(65_538'u32), 1'u16),
                      (some(65_537'u32), 1'u16), (some(high(uint32)), 1'u16)]:
      check orientation(v) == want
    for o in 1'u32 .. 8'u32: check orientation(some(o)) == uint16(o)

  test "header variants":
    check newTiff(bytesOf("II*\0\x08\0\0\0")).isSome
    check newTiff(bytesOf("MM\0*\0\0\0\x08")).isSome
    check newTiff(bytesOf("IIRO\x08\0\0\0")).isSome
    check newTiff(bytesOf("IIU\0\x08\0\0\0")).isSome
    check newTiff(bytesOf("II*\0")).isNone
    check newTiff(bytesOf("PNG\0\0\0\0\0")).isNone

  test "self-referencing IFDs terminate":
    var b = bytesOf("II*\0\x08\0\0\0")
    b.add le16(1) & le16(330) & le16(4) & le32(1) & le32(8) & le32(8)
    check newTiff(b).get.allIfds.len == 1

  test "a rational with a zero denominator is none":
    var b = bytesOf("II*\0\x08\0\0\0")
    b.add le16(1) & le16(50730) & le16(10) & le32(1) & le32(26) & le32(0) & le32(5) & le32(0)
    let t = newTiff(b).get
    let ifd = t.ifdAt(8, 0).get
    check t.float(ifd.get(50730).get, 0).isNone

suite "raw: colour":
  proc close(a, b, eps: float64): bool = abs(a - b) < eps

  test "inverse round trip":
    let m = srgbToXyzD50()
    let p = mul(m, invert(m).get)
    for r in 0 .. 2:
      for c in 0 .. 2: check close(p[r][c], (if r == c: 1.0 else: 0.0), 1e-9)
    var z: Mat3
    check invert(z).isNone

  test "white points map to D50":
    let w = apply(srgbToXyzD50(), [1.0, 1.0, 1.0])
    check close(w[0], 0.9642, 2e-3) and close(w[1], 1.0, 1e-9) and close(w[2], 0.8249, 2e-3)
    let p = apply(xyzD50ToProphoto(), xyToXyz(D50Xy))
    for v in p: check close(v, 1.0, 1e-9)

  test "McCamy reference points":
    check close(cct(D65Xy), 6504.0, 15.0)
    check close(cct([0.44757, 0.40745]), 2856.0, 15.0)

  test "DNG colour-matrix round trip":
    let cm = invert(rgbToXyz(Rec709, D65Xy)).get
    let info = ColorInfo(calibrations: @[Calibration(temperature: 6504.0, colorMatrix: cm, cameraCalibration: Identity)])
    let neutral = info.xyToNeutral(D65Xy).get
    for v in neutral: check close(v, 1.0, 1e-3)
    let xy = info.neutralToXy(neutral)
    check close(xy[0], D65Xy[0], 1e-4) and close(xy[1], D65Xy[1], 1e-4)
    let w = apply(info.balancedToXyzD50(neutral).get, [1.0, 1.0, 1.0])
    check close(w[0], 0.9642, 3e-3) and close(w[1], 1.0, 1e-9) and close(w[2], 0.8249, 3e-3)

suite "raw: lossless JPEG":
  proc samples(n: int; precision: uint8; seed: uint64): seq[uint16] =
    var s = seed
    let mx = (1'u32 shl precision) - 1
    for i in 0 ..< n:
      s = s * 6364136223846793005'u64 + 1442695040888963407'u64
      let noise = uint32(s shr 33) mod 64
      let base = (uint32(i) * 37) mod (mx + 1)
      result.add uint16((base + noise) mod (mx + 1))

  test "round trip: all predictors and precisions":
    for precision in [2'u8, 8, 12, 14, 16]:
      for predictor in 1'u8 .. 7'u8:
        for comps in 1 .. 4:
          let (w, h) = (13, 7)
          let v = samples(w * h * comps, precision, uint64(predictor) * 31 + uint64(comps))
          let (f, outp) = decodeLj(lj92Encode(v, w, h, comps, precision, predictor), high(int))
          check (f.precision, f.width, f.height, f.components) == (int(precision), w, h, comps)
          check outp == v

  test "round trip with restarts and extreme values":
    let (w, h) = (16, 9)
    var v: seq[uint16]
    for i in 0 ..< w * h * 2: v.add(if (i div 3) mod 2 == 0: 0'u16 else: 65535'u16)
    for rows in [1, 2, 4]:
      check decodeLj(lj92Encode(v, w, h, 2, 16, 1, some(rows)), high(int))[1] == v

  test "hostile streams error without panicking":
    let v = samples(32 * 8, 14, 7)
    let j = lj92Encode(v, 32, 8, 1, 14, 1, some(2))
    for n in 0 ..< j.len:
      try: discard decodeLj(j[0 ..< n], high(int))
      except RawError: discard
    var s = 0x1234_5678'u64
    for _ in 0 ..< 2000:
      var b = j
      for _ in 0 ..< 4:
        s = s * 6364136223846793005'u64 + 1
        b[int((s shr 33) mod uint64(b.len))] = byte((s shr 20) and 0xFF)
      try: discard decodeLj(b, 1 shl 20)
      except RawError: discard
    try:
      discard decodeLj(j, 10)
      check false
    except RawError as e: check e.kind == reLimitExceeded
    var big = j
    big[7] = 0xEA; big[8] = 0x60; big[9] = 0xEA; big[10] = 0x60
    expect RawError: discard decodeLj(big, high(int))
    try:
      discard decodeLj([0xFF'u8, 0xD8, 0xFF, 0xC0, 0x00, 0x0B, 8, 0, 1, 0, 1, 1, 1, 0x11, 0], high(int))
      check false
    except RawError as e: check e.kind == reUnsupported

suite "raw: opcodes":
  proc list(ops: openArray[(uint32, seq[byte])]): seq[byte] =
    result = be32(uint32(ops.len))
    for (id, p) in ops: result.add be32(id) & be32(0x0103_0000) & be32(1) & be32(uint32(p.len)) & p

  proc gainMapParams(area: array[4, uint32]; pitch: uint32; gains: array[2, array[2, float32]]): seq[byte] =
    for v in [area[0], area[1], area[2], area[3], 0'u32, 1, pitch, pitch, 2, 2]: result.add be32(v)
    for v in [1.0, 1.0, 0.0, 0.0]:
      let u = cast[uint64](v)
      result.add be32(uint32(u shr 32)) & be32(uint32(u and 0xFFFF_FFFF'u64))
    result.add be32(1)
    for r in gains:
      for g in r: result.add be32(cast[uint32](g))

  test "parses gain maps and names the others":
    let (maps, skipped) = parseOpcodes(list([(9'u32, gainMapParams([0'u32, 0, 10, 10], 1, [[1'f32, 2], [3'f32, 4]])),
                                             (1'u32, newSeq[byte](8))]))
    check maps.len == 1
    check skipped == @["WarpRectilinear"]
    let m = maps[0]
    check abs(m.at(m.row(0.5, 0), 0.5) - 2.5) < 1e-6
    check abs(m.at(m.row(0.0, 0), 0.0) - 1.0) < 1e-6
    check abs(m.at(m.row(1.0, 0), 1.0) - 4.0) < 1e-6
    check abs(m.at(m.row(-3.0, 0), 9.0) - 2.0) < 1e-6

  test "hostile lists do not panic":
    let b = list([(9'u32, gainMapParams([0'u32, 0, 10, 10], 2, [[1'f32, 2], [3'f32, 4]]))])
    for n in 0 ..< b.len: discard parseOpcodes(b[0 ..< n])
    var huge = b
    for k in 4 + 16 + 32 ..< 4 + 16 + 40: huge[k] = 0xFF
    check parseOpcodes(huge)[0].len == 0

suite "raw: packings":
  test "sensor: Bayer patterns and 12-bit unpacking":
    for p in ["RGGB", "BGGR", "GRBG", "GBRG"]: check bayer(p).get.isBayer
    for p in ["RGBG", "RRGB", "GGRB"]: check not bayer(p).get.isBayer
    var c = bayer("RGGB").get
    c.originX = 1
    check c.phase(0, 0) == [1'u8, 0, 2, 1]
    check unpack([0xAB'u8, 0xC1, 0x23], 2, 1, 1, 12, true) == @[0xABC'u16, 0x123]
    check unpack([0xBC'u8, 0x0A, 0x23, 0x01], 2, 1, 1, 12, true) == @[0xABC'u16, 0x123]
    expect RawError: discard unpack([0xAB'u8], 2, 1, 1, 12, true)

  test "RAF: 12-bit LSB first and 14-bit words":
    check unpackRaf([0xBC'u8, 0x3A, 0x12], 2, 1, 12, pkLsbFirst) == @[0xABC'u16, 0x123]
    let vals = [0x3FFF'u16, 0x0001, 0x2AAA, 0x1555]
    var acc = 0'u64
    for v in vals: acc = (acc shl 14) or uint64(v)
    var logical: seq[byte]
    for k in 0 ..< 7: logical.add byte((acc shr (8 * (6 - k))) and 0xFF)
    logical.add 0
    var phys: seq[byte]
    for i in countup(0, logical.len - 1, 4):
      for k in countdown(3, 0): phys.add logical[i + k]
    check unpackRaf(phys, 4, 1, 14, pkWords32MsbFirst) == @vals
    check unpackRaf(phys[0 ..< 3], 4, 1, 14, pkWords32MsbFirst).len == 4

  test "RAF: layouts are reversed":
    proc fuji(v: seq[byte]): Option[array[4, uint8]] =
      let c = layoutCfa(none(seq[byte]), some(v))
      if c.isSome: result = some(c.get.phase(0, 0))
    check fuji(@[0x0A'u8, 0x0B, 0x09, 0x08]) == some([0'u8, 1, 1, 2])
    check fuji(@[0x08'u8, 0x09, 0x0B, 0x0A]) == some([2'u8, 1, 1, 0])
    check fuji(@[0x0C'u8, 0x0C, 0x0C, 0x0C]).isNone
    var xt: seq[byte]
    for i in 0 ..< 36: xt.add byte(i mod 3)
    let c = layoutCfa(some(xt), some(@[0x0C'u8, 0x0C, 0x0C, 0x0C])).get
    check (c.width, c.height) == (6, 6)
    check c.color(0, 0) == xt[35] and c.color(5, 5) == xt[0]
    check layoutCfa(some(newSeqWith(36, 3'u8)), none(seq[byte])).isNone

  test "RW2: pages are split and blocks unpack LSB first":
    check physical(0) == 0x1FF8 and physical(7) == 0x1FFF and physical(8) == 0x2000
    check physical(0x2007) == 0x3FFF and physical(0x2008) == 0 and physical(0x3FFF) == 0x1FF7 and physical(0x4000) == 0x5FF8
    var blk = newSeq[byte](16)
    for i in 0 ..< 10:
      let v = 0x100 + i
      for k in 0 ..< 12:
        if ((v shr k) and 1) != 0: blk[(12 * i + k) div 8] = blk[(12 * i + k) div 8] or byte(1 shl ((12 * i + k) mod 8))
    var outp = newSeq[uint16](10)
    unpackBlock(blk, 12, outp)
    check outp == @[0x100'u16, 0x101, 0x102, 0x103, 0x104, 0x105, 0x106, 0x107, 0x108, 0x109]
    var o2 = newSeq[uint16](12)
    unpackBlock(newSeqWith(16, 0xFF'u8), 14, o2)
    check o2[0 ..< 9] == newSeqWith(9, 0x3FFF'u16) and o2[9 .. ^1] == @[0'u16, 0, 0]

  test "Sony: tone curve anchors and hostile blocks":
    let lut = toneLut([8000'u32, 10400, 12900, 14100]).get
    check lut[0] == 0 and lut[256] == 512 and lut[999] == 1998
    check lut[1001] - lut[1000] == 2 * (lut[1000] - lut[999])
    check lut[2021] >= 16300 and lut[2047] == 16383
    for i in 1 ..< lut.len: check lut[i - 1] <= lut[i]
    check toneLut([9000'u32, 8000, 12000, 13000]).isNone
    check toneLut([1'u32, 2, 3]).isNone
    check toneLut([0'u32, 0, 0, 20000]).isNone
    for b in [newSeqWith(16, 0xFF'u8), newSeq[byte](16), newSeqWith(16, 0x55'u8)]: discard decodeBlock(b)
    # Block layout: max 300 at 3, min 100 at 9, others 100 + 2k: range 200 → step 2.
    var codes: array[16, uint16]
    for i in 0 ..< 16: codes[i] = 100'u16 + 2 * uint16(i)
    codes[3] = 300
    codes[9] = 100
    codes[0] = 101
    let cb = crawBlock(codes)
    proc bitsAt(b: array[16, byte]; at, n: int): int =
      for k in 0 ..< n:
        if ((b[(at + k) div 8] shr ((at + k) mod 8)) and 1) != 0: result = result or (1 shl k)
    check bitsAt(cb, 0, 11) == 300 and bitsAt(cb, 11, 11) == 100 and bitsAt(cb, 22, 4) == 3 and bitsAt(cb, 26, 4) == 9
    check bitsAt(cb, 30, 7) == 0

  test "Nikon: trailing padding is trimmed":
    proc padded(w, h, pad: int; fill: uint16): seq[uint16] =
      for i in 0 ..< w * h:
        let (x, y) = (i mod w, i div w)
        result.add(if x >= w - pad: (if y mod 20 == 7: fill + 2 else: fill) else: uint16((x * 37 + y * 101) mod 3000))
    let (w, h) = (300, 40)
    check paddedWidth(padded(w, h, 46, 256), w, h, 12) == 254
    check paddedWidth(padded(w, h, 45, 256), w, h, 12) == 254
    check paddedWidth(padded(w, h, 0, 256), w, h, 12) == 300
    check paddedWidth(padded(w, h, 46, 4095), w, h, 12) == 300
    check paddedWidth(padded(w, h, 200, 256), w, h, 12) == 172
    check paddedWidth(newSeq[uint16](10), w, h, 12) == 300
    check paddedWidth(padded(100, 4, 20, 256), 100, 4, 12) == 100
    check paddedWidth(newSeq[uint16](), 0, 0, 12) == 0

suite "raw: camera profiles":
  proc sensorOf(make, model: string; width, height: int): Sensor =
    Sensor(format: rfNef, make: some(make), model: some(model), width: width, height: height, samples: 1,
           black: uniformBlack(0), white: [16383'f32, 16383, 16383], active: rawRect(0, 0, width, height),
           crop: rawRect(0, 0, width, height), cameraWb: some([2.0, 1.0, 1.4]), orientation: 1,
           warnings: @["NEF: black level not recorded in a documented tag; assumed 0"])
  proc nef(model: string; w, h: int): Sensor = sensorOf("NIKON CORPORATION", model, w, h)

  test "a D4 NEF gets its crop, levels, matrix and curve":
    var s = nef("NIKON D4", 4992, 3292)
    applyCameraProfile(s)
    check s.crop == rawRect(8, 6, 4928, 3280)
    check s.white == [15520'f32, 15520, 15520]
    check s.color.calibrations.len == 1
    check s.color.calibrations[0].forwardMatrix == some(D4Forward)
    check s.toneCurve.len == D4Tone.len
    check s.warnings.len == 0
    check s.color.balancedToXyzD50([0.5, 1.0, 0.7]).isSome

  test "D5100 image area respects explicit crops and the exact sensor size":
    var s = nef("NIKON D5100", 4992, 3280)
    s.active = rawRect(0, 0, 4984, 3280)
    s.crop = s.active
    let wb = s.cameraWb
    applyCameraProfile(s)
    check s.crop == rawRect(8, 8, 4928, 3264)
    check s.active == rawRect(0, 0, 4984, 3280)
    check s.cameraWb == wb
    check s.color.calibrations.len == 0 and s.toneCurve.len == 0
    check s.white == [16383'f32, 16383, 16383]
    s.crop = rawRect(16, 12, 4800, 3200)
    applyCameraProfile(s)
    check s.crop == rawRect(16, 12, 4800, 3200)
    for (model, w, h) in [("NIKON D5100", 2496, 1640), ("NIKON D5200", 4992, 3280)]:
      var o = nef(model, w, h)
      applyCameraProfile(o)
      check o.crop == o.active

  test "measured image areas crop only the borders":
    var s = nef("NIKON D800E", 7424, 4924)
    applyCameraProfile(s)
    check s.crop == rawRect(8, 6, 7360, 4912)
    check s.color.calibrations.len == 0 and s.toneCurve.len == 0 and s.white == [16383'f32, 16383, 16383]
    s = sensorOf("SONY", "ILCE-7", 6048, 4024)
    applyCameraProfile(s)
    check s.crop == rawRect(12, 12, 6000, 4000)
    s = nef("NIKON D800E", 2496, 1646)
    applyCameraProfile(s)
    check s.crop == rawRect(0, 0, 2496, 1646)
    s = nef("NIKON D800", 7424, 4924)
    applyCameraProfile(s)
    check s.crop == rawRect(0, 0, 7424, 4924)

  test "other cameras and sizes are left alone":
    for (model, w, h) in [("NIKON D4S", 4992, 3292), ("NIKON D4", 2496, 1646), ("NIKON D800", 7424, 4924)]:
      var s = nef(model, w, h)
      applyCameraProfile(s)
      check s.crop == rawRect(0, 0, w, h)
      check s.toneCurve.len == 0 and s.color.calibrations.len == 0 and s.white == [16383'f32, 16383, 16383]
      check s.warnings.len == 1

  test "the D4 forward matrix maps neutral to D50 and the curve reaches white":
    let n = apply(D4Forward, [1.0, 1.0, 1.0])
    check abs(n[0] - 0.9642) < 1e-3 and abs(n[1] - 1.0) < 1e-3 and abs(n[2] - 0.8252) < 1e-3
    for i in 1 ..< D4Tone.len: check D4Tone[i][0] > D4Tone[i - 1][0] and D4Tone[i][1] >= D4Tone[i - 1][1]
    check D4Tone[^1] == [1'f32, 1'f32]

suite "raw: CR2 helpers":
  test "red-row measurement never panics on odd geometry":
    let data = newSeqWith(64 * 32, 1000'u16)
    for area in [rawRect(0, 0, 0, 0), rawRect(0, 0, 64, 32), rawRect(60, 30, 100, 100), rawRect(high(int) - 1, 0, 4, 4),
                 rawRect(0, high(int) - 1, 4, 4)]:
      check measuredRedRow(data, 64, area, 0, 4000).isNone
    check measuredRedRow(newSeq[uint16](), 64, rawRect(0, 0, 64, 32), 0, 4000).isNone
    check measuredRedRow(data, 0, rawRect(0, 0, 64, 32), 0, 4000).isNone
    check measuredRedRow(data, 64, rawRect(0, 0, 64, 32), NaN, NaN).isNone

  test "red-row measurement finds both phases":
    for redRow in [0, 1]:
      let (w, h) = (64, 64)
      var data: seq[uint16]
      for i in 0 ..< w * h:
        let (x, y) = (i mod w, i div w)
        let tex = 1'f32 + 0.2'f32 * float32(((x div 3) xor (y div 5)) and 1)
        let c = if (y and 1) == redRow: [0, 1][x and 1] else: [1, 2][x and 1]
        data.add uint16(100'f32 + [500'f32, 1000, 667][c] * tex)
      check measuredRedRow(data, w, rawRect(0, 0, w, h), 100, 4000) == some(redRow)

  test "ColorData white balance is read as words; a short entry reads none":
    var spec = Cr2Spec(width: 8, height: 8, data: newSeqWith(64, 2048'u16), precision: 12, components: 1,
                       wbRggb: some([1613'u16, 951, 951, 1605]), orientation: 1)
    let wb = decode(spec.build()).cameraWb.get
    check abs(wb[0] - 1613.0 / 951.0) < 1e-9 and wb[1] == 1.0 and abs(wb[2] - 1605.0 / 951.0) < 1e-9
    spec.wbRggb = none(array[4, uint16])
    check decode(spec.build()).cameraWb.isNone

suite "raw: demosaic":
  proc mosaicPlane(rgb: openArray[array[3, float32]]; w, h: int; phase: array[4, uint8]): seq[float32] =
    for i in 0 ..< w * h: result.add rgb[i][phase[((i div w) and 1) * 2 + ((i mod w) and 1)]]
  proc run(cfa: openArray[float32]; w, h: int; phase: array[4, uint8]; m: Demosaic): seq[float32] =
    demosaic(paddedFromPlane(cfa, w, h), phase, m, Identity)
  proc psnr(a: openArray[float32]; b: openArray[array[3, float32]]; w, h, border: int): float64 =
    var se = 0.0
    var n = 0
    for y in border ..< h - border:
      for x in border ..< w - border:
        for c in 0 .. 2:
          let d = float64(a[(y * w + x) * 3 + c] - b[y * w + x][c])
          se += d * d
          inc n
    10.0 * log10(1.0 / max(se / float64(n), 1e-12))
  proc stripes(w, h: int): seq[array[3, float32]] =
    for i in 0 ..< w * h:
      let (x, y) = (i mod w, i div w)
      let v = if y < h div 2: float32((x div 3) mod 2) else: float32((y div 3) mod 2)
      let g = 0.1'f32 + 0.8'f32 * v
      result.add [g, g, g]
  proc smooth(w, h: int): seq[array[3, float32]] =
    for i in 0 ..< w * h:
      let (x, y) = (float32(i mod w) / float32(w), float32(i div w) / float32(h))
      result.add [0.2'f32 + 0.6'f32 * x, 0.3'f32 + 0.4'f32 * (x * y), 0.8'f32 - 0.5'f32 * y]

  test "padding reflects":
    let p = paddedFromPlane([1'f32, 2, 3, 4, 5, 6], 3, 2)
    let at = Pad * p.stride + Pad
    check p.data[at - 2 .. at + 4] == @[3'f32, 2, 1, 2, 3, 2, 1]
    let r = (Pad - 1) * p.stride + Pad
    check p.data[r .. r + 2] == @[4'f32, 5, 6]

  test "a flat colour is exact":
    let (w, h) = (16, 12)
    let rgb = newSeqWith(w * h, [0.2'f32, 0.5, 0.8])
    for phase in [[0'u8, 1, 1, 2], [2'u8, 1, 1, 0], [1'u8, 0, 2, 1], [1'u8, 2, 0, 1]]:
      for m in [dmBilinear, dmMhc, dmAhd]:
        let o = run(mosaicPlane(rgb, w, h, phase), w, h, phase, m)
        for i in 0 ..< w * h:
          for c in 0 .. 2: check abs(o[i * 3 + c] - rgb[i][c]) < 1e-5

  test "a smooth gradient demosaics accurately":
    let (w, h) = (64, 48)
    let rgb = smooth(w, h)
    let phase = [0'u8, 1, 1, 2]
    let cfa = mosaicPlane(rgb, w, h, phase)
    for m in [dmBilinear, dmMhc, dmAhd]: check psnr(run(cfa, w, h, phase, m), rgb, w, h, 2) > 40.0

  test "AHD beats bilinear on edges":
    let (w, h) = (48, 48)
    let rgb = stripes(w, h)
    let phase = [0'u8, 1, 1, 2]
    let cfa = mosaicPlane(rgb, w, h, phase)
    let bil = psnr(run(cfa, w, h, phase, dmBilinear), rgb, w, h, 3)
    check psnr(run(cfa, w, h, phase, dmAhd), rgb, w, h, 3) > bil + 3.0
    check psnr(run(cfa, w, h, phase, dmMhc), rgb, w, h, 3) > bil

  test "the bilinear fast path matches the kernels":
    let (w, h) = (20, 14)
    var cfa: seq[float32]
    for i in 0 ..< w * h: cfa.add float32((i * 7919) mod 1000) / 1000'f32
    for phase in [[0'u8, 1, 1, 2], [1'u8, 2, 0, 1]]:
      let p = paddedFromPlane(cfa, w, h)
      let fast = demosaic(p, phase, dmBilinear, Identity)
      for y in 0 ..< h:
        for x in 0 ..< w:
          for c in 0 .. 2: check abs(fast[(y * w + x) * 3 + c] - bilinearKernelValue(p, phase, x, y, c)) < 1e-6

  test "the Lab table is accurate":
    let l = newLabF()
    for i in 0 ..< 10000:
      let t = float32(i) / 4000'f32
      check abs(l.lookup(t) - labfExact(t)) < 2e-4

  proc pattern(rows: openArray[string]): Pattern =
    result.w = rows[0].len
    result.h = rows.len
    for r in rows:
      for c in r: result.colors.add(if c == 'R': 0'u8 elif c == 'G': 1'u8 else: 2'u8)
  const XtransRows = ["GGRGGB", "GGBGGR", "BRGRBG", "GGBGGR", "GGRGGB", "RBGBRG"]
  proc mosaicPat(rgb: openArray[array[3, float32]]; w: int; pat: Pattern): seq[float32] =
    for i in 0 ..< rgb.len: result.add rgb[i][pat.at(i mod w, i div w)]

  test "X-Trans: a flat colour is exact at every phase":
    let (w, h) = (25, 19)
    let rgb = newSeqWith(w * h, [0.2'f32, 0.5, 0.8])
    let pat = pattern(XtransRows)
    for (sx, sy) in [(0, 0), (1, 0), (2, 3), (5, 5)]:
      var shifted = pat
      for i in 0 ..< 36: shifted.colors[i] = pat.at(i mod 6 + sx, i div 6 + sy)
      let o = demosaicCfa(paddedFromPlane(mosaicPat(rgb, w, shifted), w, h), shifted)
      for i in 0 ..< w * h:
        for c in 0 .. 2: check abs(o[i * 3 + c] - rgb[i][c]) < 1e-5

  test "X-Trans quality":
    let pat = pattern(XtransRows)
    block:
      let (w, h) = (66, 48)
      let rgb = smooth(w, h)
      check psnr(demosaicCfa(paddedFromPlane(mosaicPat(rgb, w, pat), w, h), pat), rgb, w, h, 2) > 40.0
    let (w, h) = (48, 48)
    let rgb = stripes(w, h)
    let cfa = mosaicPat(rgb, w, pat)
    let adaptive = psnr(demosaicCfa(paddedFromPlane(cfa, w, h), pat), rgb, w, h, 3)
    var plain: seq[float32]
    for i in 0 ..< w * h:
      let (x, y) = (i mod w, i div w)
      for c in 0'u8 .. 2'u8:
        var v: seq[float32]
        for dy in -1 .. 1:
          for dx in -1 .. 1:
            let (xx, yy) = (x + dx, y + dy)
            if xx >= 0 and yy >= 0 and xx < w and yy < h and pat.at(xx, yy) == c: v.add cfa[yy * w + xx]
        plain.add(if v.len == 0: 0.5'f32 else: foldl(v, a + b, -0'f32) / float32(v.len))
    check adaptive > psnr(plain, rgb, w, h, 3) + 6.0

  test "odd patterns and tiny planes do not panic":
    for rows in [@["RGG", "GGR", "GRG"], @["G"], @["RG", "BR"]]:
      let pat = pattern(rows)
      for (w, h) in [(1, 1), (2, 3), (7, 5), (13, 1)]:
        let o = demosaicCfa(paddedFromPlane(newSeqWith(w * h, 0.5'f32), w, h), pat)
        check o.len == w * h * 3
        check o.allIt(it == it and abs(it) != Inf)
    let bad = Pattern(w: 6, h: 6, colors: newSeqWith(4, 1'u8))
    check demosaicCfa(paddedFromPlane([0.5'f32, 0.5, 0.5, 0.5], 2, 2), bad).allIt(it == 0)
    for (w, h) in [(1, 1), (2, 1), (1, 3), (2, 2), (3, 3), (7, 9), (40, 1)]:
      for m in [dmBilinear, dmMhc, dmAhd]:
        check run(newSeqWith(w * h, 0.5'f32), w, h, [0'u8, 1, 1, 2], m).len == w * h * 3

suite "raw: develop helpers":
  test "the gamma table matches powf":
    let lut = gammaWithCurve(newSeq[array[2, float32]]())
    for i in 0 .. 100_000:
      let v = float32(i) / 100_000'f32
      check abs(float32(lut.encode(v)) - pow(v, 1'f32 / 1.8'f32) * 65535'f32) <= 1'f32
    check lut.encode(-1) == 0 and lut.encode(2) == 65535 and lut.encode(NaN) == 0

  test "the tone curve is folded into the gamma table":
    proc code(v: float32): float32 = pow(v, 1'f32 / 1.8'f32) * 65535'f32
    let lut = gammaWithCurve([[0.25'f32, 0.5], [1'f32, 1]])
    proc near(got: uint16; want: float32): bool = abs(float32(got) - want) <= 2'f32
    check near(lut.encode(0.25), code(0.5))
    check near(lut.encode(0.5), code(0.75))
    check near(lut.encode(0.125), code(0.25))
    check lut.encode(0) == 0 and lut.encode(1) == 65535
    var prev = 0'u16
    for i in 0 ..< 1000:
      let e = lut.encode(float32(i) / 999'f32)
      check e >= prev
      prev = e
    check near(gammaWithCurve(newSeq[array[2, float32]]()).encode(0.25), code(0.25))

  test "orientations":
    var src: seq[uint16]
    for i in 0'u16 ..< 6: src.add [i, i, i]
    proc px(v: seq[uint16]): seq[uint16] =
      for i in countup(0, v.len - 1, 3): result.add v[i]
    check px(orient(src, 3, 2, 1)[0]) == @[0'u16, 1, 2, 3, 4, 5]
    check px(orient(src, 3, 2, 2)[0]) == @[2'u16, 1, 0, 5, 4, 3]
    check px(orient(src, 3, 2, 3)[0]) == @[5'u16, 4, 3, 2, 1, 0]
    check px(orient(src, 3, 2, 4)[0]) == @[3'u16, 4, 5, 0, 1, 2]
    let (v, w, h) = orient(src, 3, 2, 6)
    check (w, h) == (2, 3)
    check px(v) == @[3'u16, 0, 4, 1, 5, 2]
    check px(orient(src, 3, 2, 8)[0]) == @[2'u16, 5, 1, 4, 0, 3]
    check px(orient(src, 3, 2, 5)[0]) == @[0'u16, 3, 1, 4, 2, 5]
    check px(orient(src, 3, 2, 7)[0]) == @[5'u16, 2, 4, 1, 3, 0]

suite "raw: camera look":
  test "the tone curve validates and interpolates":
    check newToneCurve([[0.0, 0.0]]).isNone
    check newToneCurve([[0.0, 0.0], [0.5, 0.4], [0.5, 0.6], [1.0, 1.0]]).isNone
    check newToneCurve([[0.0, 0.0], [0.5, 0.6], [0.7, 0.4], [1.0, 1.0]]).isNone
    check newToneCurve([[0.0, 0.0], [0.5, NaN], [1.0, 1.0]]).isNone
    check newToneCurve([[0.1, 0.0], [1.0, 1.0]]).isNone
    let t = newToneCurve([[0.0, 0.0], [0.04, 0.16], [1.0, 1.0]]).get
    check abs(t.eval(0.04) - 0.16) < 1e-12
    check abs(t.eval(0.01) - 0.04) < 1e-12
    check t.eval(-1.0) == 0.0 and t.eval(2.0) == 1.0 and t.eval(NaN) == 0.0
    check abs(t.inverse(0.16) - 0.04) < 1e-12
    let (x, d) = t.inverseAndSlope(0.5)
    check abs(t.eval(x) - 0.5) < 1e-12
    let num = (t.eval(x + 1e-6) - t.eval(x - 1e-6)) / 2e-6
    check abs(d - num) < 1e-6 * max(num, 1.0)
    let k = t.knots
    check abs(k[1][0] - 0.04) < 1e-12 and abs(k[1][1] - 0.16) < 1e-12

  test "the Lab helpers: zero error on identical colours, white is L* 100":
    check labDeltaE([0.2, 0.3, 0.4], [0.2, 0.3, 0.4]) < 1e-12
    let w = labOf([1.0, 1.0, 1.0])
    check abs(w[0] - 100.0) < 1e-9 and abs(w[1]) < 1e-9 and abs(w[2]) < 1e-9

  test "develop ignores an unusable look":
    let (w, h) = (64, 48)
    let data = mosaic(scene(w, h), w, [0'u8, 1, 1, 2], 0, 4095)
    let s = decode(tiffEp("NIKON CORPORATION", w, h, data, [0'u8, 1, 1, 2], 12))
    var nanM: Mat3
    for r in nanM.mitems:
      for v in r.mitems: v = NaN
    var zero: Mat3
    for m in [nanM, zero, [[1e9, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]]:
      var o = defaultDevelopOptions()
      o.whiteBalance = multipliersWb([1.5, 1.0, 1.25])
      o.cameraLook = some(CameraLook(toXyz: m, deltaE: 1.0, deltaEFallback: 2.0))
      let d = developSensor(s, o)
      check d.warnings.anyIt("not usable" in it)
      check d.warnings.anyIt("sRGB primaries" in it)
