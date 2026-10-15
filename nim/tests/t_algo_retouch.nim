## Magic Eraser, stack modes, trap, gradient/bucket tools and the retouch kernels (ports of the
## Rust unit tests). Bit parity with Rust is checked by `tools/algocheck.py` (see PORTING.md).

import std/[unittest, options, math, sequtils]
import photocraft/[geom, color, raster]
import photocraft/algo/[selection, erase, stack, trap, paint, retouch]

proc fullRegion(b: Rect; v: uint8): Region = Region(bbox: b, mask: newSeqWith(b.width * b.height, v))

suite "magic eraser":
  test "erases alpha in every format and keeps colour on partial erase":
    for fmt in [pixelFormat(cmRgb, stU8, true), pixelFormat(cmRgb, stU16, true), pixelFormat(cmRgb, stF32, true),
                pixelFormat(cmCmyk, stU8, true), pixelFormat(cmGrayscale, stU16, true)]:
      var fill = newSeq[float32](fmt.channels)
      fill[^1] = 1
      fill[0] = 0.4
      var s = newSurface(fmt)
      s.fillRect(rect(0, 0, 500, 300), fill)
      let before = s.pixel(400, 10)
      let d = magicErase(s, fullRegion(rect(0, 0, 300, 300), 255), 1, none(Surface), none(seq[float32]))
      check d == rect(0, 0, 300, 300)
      check s.rgba(10, 10)[3] == 0
      check s.pixel(400, 10) == before
      var p = newSurface(fmt)
      p.fillRect(rect(0, 0, 10, 10), fill)
      discard magicErase(p, fullRegion(rect(0, 0, 10, 10), 255), 0.5, none(Surface), none(seq[float32]))
      let q = p.pixel(3, 3)
      check abs(q[^1] - 0.5) < 0.01 and abs(q[0] - 0.4) < 0.01

  test "respects the selection and the transparency lock":
    var s = newSurface(pixelFormat(cmRgb, stU8, true))
    s.fillRect(rect(0, 0, 20, 10), [0'f32, 0, 0, 1])
    var m = newSeq[float32](20 * 10)
    for y in 0 ..< 10:
      for x in 0 ..< 10: m[y * 20 + x] = 1
    let sel = maskToSurface(m, rect(0, 0, 20, 10))
    discard magicErase(s, fullRegion(rect(0, 0, 20, 10), 255), 1, some(sel), none(seq[float32]))
    check s.rgba(5, 5)[3] == 0 and s.rgba(15, 5)[3] == 1
    var l = newSurface(pixelFormat(cmRgb, stU8, true))
    l.fillRect(rect(0, 0, 10, 10), [0'f32, 0, 0, 1])
    discard magicErase(l, fullRegion(rect(0, 0, 10, 10), 255), 1, none(Surface), some(@[1'f32, 1, 1]))
    check l.rgba(5, 5) == [1'f32, 1, 1, 1]

  test "degenerate inputs do nothing":
    var s = newSurface(pixelFormat(cmRgb, stU8, true))
    s.fillRect(rect(0, 0, 4, 4), [0'f32, 0, 0, 1])
    let orig = s
    check magicErase(s, Region(bbox: rect(0, 0, 4, 4), mask: @[255'u8, 255, 255]), 1, none(Surface), none(seq[float32])).isEmpty
    check magicErase(s, fullRegion(rect(0, 0, 4, 4), 255), NaN, none(Surface), none(seq[float32])).isEmpty
    check magicErase(s, fullRegion(EmptyRect, 255), 1, none(Surface), none(seq[float32])).isEmpty
    check s == orig
    var g = newSurface(pixelFormat(cmGrayscale, stU8, false))
    check magicErase(g, fullRegion(rect(0, 0, 4, 4), 255), 1, none(Surface), none(seq[float32])).isEmpty

suite "stack modes":
  proc px(v: float32): seq[array[4, float32]] = @[[v, v, v, 1'f32]]
  test "basic statistics":
    let layers = @[px(0.1), px(0.2), px(0.2), px(0.2), px(0.9)]
    proc at(s: Stat): float32 = combine(layers, s)[0][0]
    check abs(at(stMean) - 0.32) < 1e-6 and abs(at(stMedian) - 0.2) < 1e-6
    check at(stMinimum) == 0.1'f32 and at(stMaximum) == 0.9'f32
    check abs(at(stRange) - 0.8) < 1e-6 and abs(at(stSummation) - 1.6) < 1e-6
    check abs(at(stStandardDeviation) - sqrt(at(stVariance))) < 1e-6
    check at(stSkewness) > 0.5 and at(stEntropy) > 0 and at(stEntropy) <= 1 and at(stKurtosis) > 0
  test "HDR and signed samples, transparent samples ignored, median removes an outlier":
    for m in [stMean, stMedian]:
      check combine(@[px(2), px(4)], m)[0] == [3'f32, 3, 3, 1]
      check combine(@[px(-2), px(-4)], m)[0] == [-3'f32, -3, -3, 1]
    check combine(@[@[[0.5'f32, 0.5, 0.5, 1]], @[[1'f32, 0, 0, 0]]], stMean)[0] == [0.5'f32, 0.5, 0.5, 1]
    check combine(@[@[[1'f32, 0, 0, 0]]], stMean)[0][3] == 0
    check combine(@[px(0.5), px(0.5), px(1), px(0.5)], stMedian)[0][0] == 0.5

suite "trap":
  proc twoBlocks(): (seq[float32], int, int) =
    let (w, h) = (8, 1)
    var px = newSeq[float32](w * h * 4)
    for x in 0 ..< w:
      if x < 4: px[x * 4] = 1 else: px[x * 4 + 1] = 1
    (px, w, h)
  test "traps the internal colour edge":
    var (px, w, h) = twoBlocks()
    trap(px, w, h, 4, 1)
    check px[3 * 4 + 1] > 0.5 and px[4 * 4] > 0.5
    check px[0 * 4 + 1] == 0 and px[7 * 4] == 0
  test "leaves paper untouched; width zero and malformed buffers are the identity":
    var px = newSeq[float32](6 * 4)
    px[2 * 4] = 1
    trap(px, 6, 1, 4, 2)
    for i in [0, 1, 3, 4, 5]: check px[i * 4] == 0
    var (q, w, h) = twoBlocks()
    let orig = q
    trap(q, w, h, 4, 0)
    check q == orig
    trap(q, w + 1, h, 4, 1)
    check q == orig

suite "gradient and bucket tools":
  test "gradient t shapes":
    let (f, t) = ((0'f32, 0'f32), (10'f32, 0'f32))
    check toolGradientT(gsLinear, f, t, 5, 3) == 0.5 and toolGradientT(gsLinear, f, t, -5, 0) == 0
    check toolGradientT(gsRadial, f, t, 0, 5) == 0.5 and toolGradientT(gsReflected, f, t, -5, 0) == 0.5
    check toolGradientT(gsDiamond, f, t, 2.5, 2.5) == 0.5
    check abs(toolGradientT(gsAngle, f, t, 0, 5) - 0.25) < 1e-5
  test "bucket fills the contiguous region":
    var s = newSurface(pixelFormat(cmRgb, stF32, true))
    let a = rect(0, 0, 10, 5)
    s.fillRect(a, [1'f32, 1, 1, 1])
    s.fillRect(rect(0, 0, 5, 5), [0'f32, 0, 0, 1])
    check bucketFill(s, a, (1, 1), 10, true, false, [1'f32, 0, 0, 1], 1, none(Surface), false)
    check s.pixel(2, 2) == @[1'f32, 0, 0, 1] and s.pixel(8, 2) == @[1'f32, 1, 1, 1]
    check not bucketFill(s, a, (50, 1), 10, true, false, [1'f32, 0, 0, 1], 1, none(Surface), false)

suite "retouch kernels":
  test "dodge midtones affects midtones most; ranges target their tones":
    proc g(v: float32): float32 = toneCurve(v, 0.5, trMidtones, false) - v
    check g(0.5) > 0.1 and g(0.5) > 2 * g(0.1) and g(0.5) > 2 * g(0.95)
    check dodgeBurn([0'f32, 0, 0], 0.5, trMidtones, false, true) == [0'f32, 0, 0]
    proc d(v: float32; r: ToneRange): float32 = toneCurve(v, 0.5, r, false) - v
    check d(0.1, trShadows) > d(0.9, trShadows) and d(0.9, trHighlights) > d(0.1, trHighlights)
  test "protect tones keeps hue; sponge changes saturation and keeps luma":
    let c = [0.6'f32, 0.3, 0.2]
    let p = dodgeBurn(c, 0.8, trMidtones, false, true)
    check p.allIt(it >= 0 and it <= 1) and abs(p[0] / p[1] - c[0] / c[1]) < 0.05
    let c2 = [0.8'f32, 0.4, 0.3]
    let dsat = sponge(c2, 0.5, false, false)
    check saturation(dsat) < saturation(c2) and abs(luma(dsat) - luma(c2)) < 1e-4
    let sat = sponge(c2, 0.5, true, false)
    check saturation(sat) > saturation(c2) and sat.allIt(it >= 0 and it <= 1)
    let (v, nv) = (sponge([1'f32, 0.1, 0.1], 0.5, true, true), sponge([1'f32, 0.1, 0.1], 0.5, true, false))
    check abs(v[1] - 0.1) <= abs(nv[1] - 0.1)
    check sponge([0.5'f32, 0.5, 0.5], 1, false, false) == [0.5'f32, 0.5, 0.5]
  test "blur reduces variance, sharpen increases it, transparent colour does not bleed":
    let (w, h) = (16, 16)
    var data = newSeq[float32](w * h)
    for i in 0 ..< data.len: data[i] = float32((i * 7919) mod 13) / 12
    proc variance(v: seq[float32]): float32 =
      let m = v.foldl(a + b) / float32(v.len)
      v.mapIt((it - m) * (it - m)).foldl(a + b) / float32(v.len)
    let b = localBlur(data, w, h, 1, none(int), (0, 0, w, h), 2)
    check variance(b) < variance(data) * 0.2
    let sm = localBlur(data, w, h, 1, none(int), (0, 0, w, h), 1)
    check variance(localSharpen(sm, w, h, 1, none(int), (0, 0, w, h), 2, false)) > variance(sm)
    var rgba = newSeq[float32](4 * 4 * 4)
    for i in 0 ..< 16:
      rgba[i * 4] = 1
      rgba[i * 4 + 3] = 1
    rgba[5 * 4 + 1] = 1
    rgba[5 * 4] = 0
    rgba[5 * 4 + 3] = 0
    let r = localBlur(rgba, 4, 4, 4, some(3), (0, 0, 4, 4), 1)
    for i in 0 ..< 16:
      if i != 5: check r[i * 4] > 0.99 and r[i * 4 + 1] < 0.01
