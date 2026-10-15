## Selection, histogram, tone and HDR algorithms (ports of the Rust unit tests). Bit parity with
## Rust is checked by `tools/algocheck.py` (see PORTING.md).

import std/[unittest, options, math, sequtils, algorithm]
import photocraft/[geom, color, raster]
import photocraft/algo/[selection, histogram, tone, hdr, util, exif, quantize]

proc img(w, h: int; f: proc(x, y: int): array[4, float32]): seq[array[4, float32]] =
  for y in 0 ..< h:
    for x in 0 ..< w: result.add f(x, y)

proc count(m: seq[float32]): int = m.countIt(it > 0)

suite "selection":
  test "wand contiguous vs global":
    let px = img(20, 10, proc(x, y: int): array[4, float32] =
      (if x in 2 ..< 6 or x in 12 ..< 16: [1'f32, 0, 0, 1] else: [1'f32, 1, 1, 1]))
    let a = rect(0, 0, 20, 10)
    check magicWand(px, a, (3, 3), 10, true, false).count == 40
    check magicWand(px, a, (3, 3), 10, false, false).count == 80
    let aa = magicWand(px, a, (3, 3), 10, true, true)
    check aa[3 * 20 + 6] > 0 and aa[3 * 20 + 6] < 1
    check magicWand(px, a, (50, 3), 10, true, false).allIt(it == 0)

  test "wand tolerance":
    let px = img(10, 1, proc(x, y: int): array[4, float32] = [float32(x) * 10 / 255, 0, 0, 1])
    check magicWand(px, rect(0, 0, 10, 1), (0, 0), 25, true, false).count == 3

  test "wand ignores hidden RGB only when both pixels are fully transparent":
    let area = rect(0, 0, 6, 1)
    let pixels: seq[array[4, uint8]] = @[[255'u8, 0, 0, 0], [0'u8, 255, 0, 0], [255'u8, 0, 0, 128], [253'u8, 0, 0, 128],
                                          [0'u8, 0, 255, 0], [255'u8, 0, 0, 255]]
    var floats: seq[array[4, float32]]
    for p in pixels: floats.add [float32(p[0]) / 255, float32(p[1]) / 255, float32(p[2]) / 255, float32(p[3]) / 255]
    for (seed, tol, contig, expected) in [((0, 0), 0'f32, true, [1'f32, 1, 0, 0, 0, 0]),
                                          ((0, 0), 0'f32, false, [1'f32, 1, 0, 0, 1, 0]),
                                          ((4, 0), 32'f32, false, [1'f32, 1, 0, 0, 1, 0]),
                                          ((2, 0), 0'f32, false, [0'f32, 0, 1, 0, 0, 0]),
                                          ((2, 0), 2'f32, true, [0'f32, 0, 1, 1, 0, 0]),
                                          ((5, 0), 32'f32, false, [0'f32, 0, 0, 0, 0, 1])]:
      check magicWand(floats, area, seed, tol, contig, false) == @expected
      let region = wandRegion(pixels, area, seed, tol, contig, false).get
      for x, v in expected: check region.at(x, 0) == v
    var plain = pixels
    for p in plain.mitems:
      if p[3] == 0: p = [0'u8, 0, 0, 0]
    for contig in [false, true]:
      check wandRegion(pixels, area, (0, 0), 0, contig, true) == wandRegion(plain, area, (0, 0), 0, contig, true)

  test "color range falloff and samples":
    let px = @[[1'f32, 0, 0, 1], [0.9'f32, 0, 0, 1], [0'f32, 0, 1, 1], [1'f32, 0, 0, 0]]
    let m = colorRange(px, [1'f32, 0, 0], 40)
    check m[0] == 1 and m[1] > 0.3 and m[1] < 1 and m[2] == 0 and m[3] == 0
    let (red, blue, green) = ([1'f32, 0, 0, 1], [0'f32, 0, 1, 1], [0'f32, 1, 0, 1])
    let row = @[red, blue, green, red]
    proc s(c: array[4, float32]; at = none((float32, float32))): RangeSample = RangeSample(color: [c[0], c[1], c[2]], at: at)
    check colorRangeSamples(row, 4, [s(red), s(blue)], 10, none(float32)) == @[1'f32, 1, 0, 1]
    check colorRangeSamples(row, 4, [s(red)], 40, none(float32)) == colorRange(row, [1'f32, 0, 0], 40)
    check colorRangeSamples(row, 4, [s(red, some((0'f32, 0'f32)))], 10, some(2'f32)) == @[1'f32, 0, 0, 0]
    check colorRangeSamples(@[red, red, red, red], 4, [s(red, some((0'f32, 0'f32)))], 10, some(2'f32)) == @[1'f32, 0.5, 0, 0]
    check colorRangeSamples(row, 4, [], 10, none(float32)).allIt(it == 0)

  test "lab range matches Photoshop":
    const Ps = [([61.9989'f32, 25.5977, 20.5742], 40'f32, 0.9686'f32), ([67.9988'f32, 25.5977, 20.5742], 40'f32, 0.4824'f32),
                ([79.9985'f32, 25.5977, 20.5742], 100'f32, 0.4824'f32), ([60'f32, 13.5508, 20.5742], 100'f32, 0.7647'f32),
                ([60'f32, 25.5977, 30.6133], 40'f32, 0.0863'f32)]
    let r = LabRange.point([60'f32, 25, 19.9961])
    for (lab, fz, ps) in Ps: check abs(labRangeCoverage(lab, r, fz) - ps) <= 2 / 255
    check labRangeCoverage([66'f32, 25, 20], r, 40) > labRangeCoverage([60'f32, 31, 20], r, 40)
    for fz in [1'f32, 10, 40]: check labRangeCoverage([60'f32, 25, 19.9961], r, fz) == 1
    for v in [NaN.float32, Inf.float32, NegInf.float32]:
      for fz in [v, 40'f32]:
        let k = labRangeCoverage([v, 0, 0], LabRange.point([50'f32, 10, -5]), fz)
        check k >= 0 and k <= 1

  test "subtract reshapes the box like Photoshop":
    for (lo, hi, m, fz, wantLo, wantHi) in [(0'f32, 20'f32, 5'f32, 40'f32, 5'f32, 20'f32), (0'f32, 20'f32, 0'f32, 40'f32, 2.68'f32, 20'f32),
                                            (0'f32, 20'f32, -13'f32, 40'f32, 0.4'f32, 20'f32), (0'f32, 20'f32, 33'f32, 40'f32, 0'f32, 19.6'f32),
                                            (0'f32, 2'f32, 3'f32, 40'f32, 0'f32, 0.32'f32)]:
      let r = LabRange(lo: [50'f32, lo, 0], hi: [50'f32, hi, 0]).subtract([50'f32, m, 0], fz)
      check abs(r.lo[1] - wantLo) < 0.03 and abs(r.hi[1] - wantHi) < 0.03

  test "modify, polygon and combine":
    let (w, h) = (40, 30)
    var m = newSeq[float32](w * h)
    for y in 10 ..< 20:
      for x in 10 ..< 30: m[y * w + x] = 1
    check expand(m, w, h, 2).count > m.count
    check contract(m, w, h, 2).count < m.count
    let b = border(m, w, h, 4)
    check b[15 * w + 10] == 1 and b[15 * w + 20] == 0
    check smooth(m, w, h, 3).allIt(it == 0 or it == 1)
    let f = feather(m, w, h, 3)
    check f[15 * w + 20] > 0.9 and f[15 * w + 10] > 0.3 and f[15 * w + 10] < 0.7
    let p = polygon([(5'f32, 5'f32), (35'f32, 5'f32), (20'f32, 25'f32)], rect(0, 0, w, h), true)
    check p[10 * w + 20] == 1 and p[0] == 0
    let s = combine(none(Surface), m, rect(0, 0, w, h), smReplace).get
    let both = combine(some(s), m, rect(0, 0, w, h), smSubtract)
    check both.isNone
    let e = ellipseSurface(rect(0, 0, 600, 300), rect(0, 0, 1000, 1000), true)
    check e.tileCount > 0 and e.pixel(300, 150)[0] == 1 and e.pixel(2, 2)[0] == 0

suite "histogram":
  test "exact bins and channel totals":
    var px: seq[array[4, float32]]
    for i in 0 ..< Bins: px.add [float32(i) / 255, float32(255 - i) / 255, 0.5, 0.25]
    let h = fromRgba(px)
    check h.samples == 256
    check h.channels[0].allIt(it == 1) and h.channels[1].allIt(it == 1)
    check h.channels[2][128] == 256
  test "empty, transparent, invalid and HDR are explicit":
    check fromRgba(newSeq[array[4, float32]]()) == RgbHistogram()
    let h = fromRgba([[0.7'f32, 0.9, 0.4, 0], [NaN.float32, 0, 0, 1], [0'f32, Inf, 0, 1], [0'f32, 0, 0, NaN], [-2'f32, 4, 0.5, 1]])
    check (h.samples, h.transparent, h.invalid) == (1'u64, 1'u64, 3'u64)
    check h.underflow == [1'u64, 0, 0] and h.overflow == [0'u64, 1, 0]
  test "clipping uses exact endpoints":
    check clipping([0.0001'f32, 0.9999, 0.5, 1]) == (0'u8, 0'u8)
    check clipping([-0.1'f32, 1, 0, 1]) == (5'u8, 2'u8)
    check clipping([0'f32, 1, 0, 0]) == (0'u8, 0'u8)

suite "tone":
  proc ramp(w, h: int): seq[array[4, float32]] =
    for i in 0 ..< w * h:
      let x = float32(i mod w) / float32(w - 1)
      result.add [x, x * 0.8'f32, x * 0.6'f32 + 0.1'f32, 1]
  proc lumaOf(p: array[4, float32]): float32 = 0.299'f32 * p[0] + 0.587'f32 * p[1] + 0.114'f32 * p[2]

  test "shadows lift dark pixels more than bright, zero amounts are the identity":
    var px = ramp(32, 8)
    let orig = px
    var p = defaultShadowsHighlights()
    p.blackClip = 0
    p.whiteClip = 0
    shadowsHighlights(px, 32, 8, p)
    check lumaOf(px[3]) - lumaOf(orig[3]) > 0.02
    check lumaOf(px[3]) - lumaOf(orig[3]) > lumaOf(px[30]) - lumaOf(orig[30])
    var px2 = orig
    p.shadowAmount = 0
    p.highlightAmount = 0
    p.color = 0
    shadowsHighlights(px2, 32, 8, p)
    check px2 == orig

  test "replace color only touches matching pixels":
    var px = @[[1'f32, 0, 0, 1], [0'f32, 0, 1, 1]]
    let m = replaceColor(px, [1'f32, 0, 0], 40, 120, 0, 0)
    check m[1] == 0 and px[0][1] > 0.9 and px[0][0] < 0.1 and px[1] == [0'f32, 0, 1, 1]

  test "match color moves statistics toward the source; neutralize goes gray":
    var src, tgt: seq[array[4, float32]]
    for i in 0 ..< 64:
      src.add [0.8'f32, 0.4'f32 + float32(i mod 8) * 0.02'f32, 0.2, 1]
      tgt.add [0.2'f32, 0.3, 0.6'f32 + float32(i mod 8) * 0.03'f32, 1]
    let s = labStats(src, none(seq[float32])).get
    let t = labStats(tgt, none(seq[float32])).get
    matchColor(tgt, t, some(s), defaultMatchColor())
    let after = labStats(tgt, none(seq[float32])).get
    for k in 0 .. 2: check abs(after.mean[k] - s.mean[k]) < abs(t.mean[k] - s.mean[k]) * 0.2 + 1
    var tint: seq[array[4, float32]]
    for i in 0 ..< 16: tint.add [0.7'f32, 0.5, 0.3'f32 + float32(i) * 0.01'f32, 1]
    let ts = labStats(tint, none(seq[float32])).get
    var o = defaultMatchColor()
    o.neutralize = true
    matchColor(tint, ts, none(LabStats), o)
    let n = labStats(tint, none(seq[float32])).get
    check abs(n.mean[1]) < 2 and abs(n.mean[2]) < 2

  test "HDR toning compresses range and keeps bounds; an inverting curve flips the order":
    var px = ramp(40, 10)
    var p = defaultHdrToning()
    p.strength = 2
    p.detail = 0
    p.saturation = 0
    hdrToning(px, 40, 10, p)
    check lumaOf(px[38]) > lumaOf(px[2])
    check px.allIt(it[0] >= 0 and it[0] <= 1 and it[1] >= 0 and it[1] <= 1 and it[2] >= 0 and it[2] <= 1)
    px = ramp(40, 10)
    p.strength = 0
    p.curve = @[(0'f32, 1'f32), (1'f32, 0'f32)]
    hdrToning(px, 40, 10, p)
    check lumaOf(px[2]) > lumaOf(px[38])

suite "hdr":
  proc scene(w, h: int): seq[array[3, float32]] =
    for i in 0 ..< w * h:
      let (x, y) = (float32(i mod w) / float32(w), float32(i div w) / float32(h))
      let base = 0.002'f32 * pow(2'f32, x * 12)
      let tex = 1'f32 + 0.3'f32 * sin(y * 40)
      result.add [base * tex, base * tex * 0.8'f32, base * tex * 0.6'f32]
  proc camera(e: float32): float32 = min(pow(max(e, 0'f32), 0.45'f32), 1'f32)
  proc shoot(sc: seq[array[3, float32]]; dt: float32): seq[array[4, float32]] =
    for r in sc:
      result.add [round(camera(r[0] * dt) * 255) / 255, round(camera(r[1] * dt) * 255) / 255,
                  round(camera(r[2] * dt) * 255) / 255, 1]

  test "exposure estimation and merge recover radiance":
    let (w, h) = (192, 64)
    let sc = scene(w, h)
    let dts = [0.25'f32, 1, 4, 16]
    var shots: seq[seq[array[4, float32]]]
    for d in dts: shots.add shoot(sc, d)
    let est = estimateExposures(shots)
    for k in 1 .. 3: check est[k] / est[k - 1] >= 2 and est[k] / est[k - 1] < 8
    let m = merge(w, h, shots, MergeOptions(exposures: dts.mapIt(float64(it))))
    proc probe(x: int): float64 = float64(m.px[(h div 2) * w + x][1]) / float64(sc[(h div 2) * w + x][1])
    let k0 = probe(w div 2)
    for x in [10, 50, 100, 150, 170]: check abs(ln(probe(x) / k0)) < 0.25
    check m.stops > 9
    for k in 1 ..< 256: check m.response[0][k] > m.response[0][k - 1]

  test "ghosts come from the base exposure":
    let (w, h) = (64, 64)
    let sc = newSeqWith(w * h, [0.2'f32, 0.2, 0.2])
    var shots = @[shoot(sc, 0.5), shoot(sc, 1), shoot(sc, 2)]
    for y in 20 ..< 40:
      for x in 20 ..< 40: shots[2][y * w + x] = [0.2'f32, 0.2, 0.2, 1]
    proc opts(g: bool): MergeOptions = MergeOptions(exposures: @[0.5, 1.0, 2.0], removeGhosts: g, ghostBase: some(1))
    let plain = merge(w, h, shots, opts(false))
    let clean = merge(w, h, shots, opts(true))
    proc at(m: Merged; x, y: int): float32 = m.px[y * w + x][1]
    check abs(clean.at(30, 30) / clean.at(5, 5) - 1) < 0.15
    check abs(plain.at(30, 30) / plain.at(5, 5) - 1) > 0.2
    check clean.ghostFraction > 0.05 and clean.ghostBase == 1

  test "MTB finds shifts":
    let (w, h) = (128, 96)
    var im = newSeq[float32](w * h)
    for i in 0 ..< w * h:
      let (x, y) = (float32(i mod w), float32(i div w))
      im[i] = 0.5'f32 + 0.4'f32 * (sin(x * 0.21'f32) * cos(y * 0.17'f32))
    let (dx, dy) = (5, -3)
    var shifted = newSeq[float32](w * h)
    for i in 0 ..< w * h:
      let (x, y) = (i mod w - dx, i div w - dy)
      shifted[i] = im[clamp(y, 0, h - 1) * w + clamp(x, 0, w - 1)] * 0.6'f32
    check mtbOffset(w, h, shifted, im, 4) == (dx, dy)

  test "tone mapping methods produce the display range":
    let (w, h) = (96, 32)
    let sc = scene(w, h)
    for m in [ToneMethod(kind: tmLocalAdaptation, toning: (var t = defaultHdrToning(); t.radius = 7; t.strength = 0.52; t)),
              ToneMethod(kind: tmExposureGamma, exposure: 0, gamma: 1), ToneMethod(kind: tmHighlightCompression),
              ToneMethod(kind: tmEqualizeHistogram)]:
      var px = sc.mapIt([it[0], it[1], it[2], 1'f32])
      toneMap(px, w, h, m)
      check px.allIt(it[0] >= 0 and it[0] <= 1 and it[1] >= 0 and it[1] <= 1 and it[2] >= 0 and it[2] <= 1)
      proc row(x: int): float32 = px[(h div 2) * w + x][1]
      check row(w - 2) > row(2)
      if m.kind != tmExposureGamma: check row(4) > 0.01 and row(w - 4) < 0.999

suite "exif":
  test "round trips and survives garbage":
    let info = CameraInfo(make: some("Generic"), model: some("Camera 1"), lens: some("24-70mm"), exposureTime: some(0.004),
                          fNumber: some(8.0), iso: some(200.0), focalLength: some(35.0), focalLength35mm: some(52.0),
                          dateTaken: some("2024:02:17 12:40:19"))
    let b = build(info)
    check read(b) == info
    check abs(read(b).exposureFactor.get - 0.004 * 2 / 64) < 1e-9
    check read(@[byte('E'), byte('x'), byte('i'), byte('f'), 0, 0] & b) == info
    check read(cast[seq[byte]]("nonsense")) == CameraInfo()
    for cut in [5, 12, 30, b.len - 3]: discard read(b[0 ..< cut])

  test "hostile payloads never raise":
    proc entry(tag, typ: uint16; count, value: uint32): seq[byte] =
      for v in [uint64(tag), uint64(typ)]:
        result.add byte(v and 0xFF)
        result.add byte(v shr 8)
      for v in [count, value]:
        for k in 0 .. 3: result.add byte((v shr (8 * k)) and 0xFF)
    var b = @[byte('I'), byte('I'), 42, 0, 8, 0, 0, 0]
    let entries = [entry(0x010F, 2, high(uint32), 8), entry(0x0110, 2, 16, high(uint32) - 1),
                   entry(0x829A, 5, high(uint32), 20), entry(0x829D, 5, 1, high(uint32) - 3),
                   entry(0x9003, 2, 0x4000_0000, 8), entry(0x8769, 4, 1, 8), entry(0x8769, 4, 1, high(uint32))]
    b.add byte(entries.len + 3)
    b.add 0
    for e in entries: b.add e
    check read(b) == CameraInfo()
    check read(@[byte('M'), byte('M'), 0, 42, 255, 255, 255, 255]) == CameraInfo()
    var x = 0x2545_F491'u32
    for _ in 0 ..< 200:
      var n = @[byte('I'), byte('I'), 42, 0, 8, 0, 0, 0]
      for _ in 0 ..< 96:
        x = x xor (x shl 13)
        x = x xor (x shr 17)
        x = x xor (x shl 5)
        n.add byte(x and 0xFF)
      discard read(n)

suite "quantize":
  proc gradient(w, h: int): seq[array[4, float32]] =
    for i in 0 ..< w * h: result.add [float32(i mod w) / float32(w - 1), float32(i div w) / float32(h - 1), 0.3'f32, 1]

  test "fixed palettes have the expected sizes":
    check webPalette().len == 216 and macPalette().len == 256 and windowsPalette().len == 256
    check uniformPalette(27).len == 27
    var m = macPalette()
    m.sort(proc(a, b: Rgb8): int = cmp(int(a[0]) shl 16 or int(a[1]) shl 8 or int(a[2]), int(b[0]) shl 16 or int(b[1]) shl 8 or int(b[2])))
    m = deduplicate(m, isSorted = true)
    check m.len == 256

  test "exact palette and failure":
    check buildPalette([[1'f32, 0, 0, 1], [0'f32, 0, 1, 1], [1'f32, 0, 0, 1]], pkExact, 256, fcNone).len == 2
    expect QuantizeError: discard buildPalette(gradient(64, 64), pkExact, 256, fcNone)

  test "adaptive and perceptual respect count and forced colours":
    for kind in [pkAdaptive, pkPerceptual, pkSelective]:
      let pal = buildPalette(gradient(40, 40), kind, 16, fcBlackWhite)
      check pal.len <= 16 and pal.len >= 8
      check pal[0 .. 1] == @[[0'u8, 0, 0], [255'u8, 255, 255]]

  test "forced colours filling the count leave no room and do not crash":
    for kind in [pkAdaptive, pkPerceptual, pkSelective]:
      check buildPalette(newSeqWith(16, [1'f32, 1, 1, 1]), kind, 2, fcBlackWhite) == @[[0'u8, 0, 0], [255'u8, 255, 255]]
      check buildPalette(gradient(8, 8), kind, 8, fcPrimaries).len == 8

  test "quantize maps to the palette with each dither":
    let pal = @[[0'u8, 0, 0], [255'u8, 255, 255]]
    for d in Dither:
      var px: seq[array[4, float32]]
      for i in 0 ..< 64:
        let v = float32(i) / 63
        px.add [v, v, v, 1'f32]
      let idx = quantize(px, 8, pal, d, 1, none(int))
      check px.allIt(it[0] == 0 or it[0] == 1)
      check idx.len == 64
      if d == dtDiffusion:
        var mean = 0'f32
        for p in px: mean += p[0]
        check abs(mean / 64 - 0.5) < 0.08
    var px = @[[1'f32, 1, 1, 0.2]]
    let idx = quantize(px, 1, pal, dtNone, 1, some(1))
    check idx[0] == 1 and px[0][3] == 0

  test "bitmap methods are binary and track tone":
    for m in [BitmapMethod(kind: bmThreshold), BitmapMethod(kind: bmPattern), BitmapMethod(kind: bmDiffusion),
              BitmapMethod(kind: bmHalftone, cell: 6, angle: 45, shape: hsRound)]:
      var g = newSeqWith(32 * 32, 0.25'f32)
      toBitmap(g, 32, m)
      check g.allIt(it == 0 or it == 1)
      var mean = 0'f32
      for v in g: mean += v
      mean /= float32(g.len)
      if m.kind != bmThreshold: check abs(mean - 0.25) < 0.15
      else: check mean == 0
