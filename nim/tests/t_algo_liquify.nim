## Liquify (ports of the Rust unit tests). Bit parity with Rust is checked by `tools/algocheck.py`
## (`tools/algocases/liquify.json`, see PORTING.md).

import std/[unittest, options, math, json]
import photocraft/[geom, raster, color]
import photocraft/algo/liquify

proc sample(st: SampleType): Surface =
  result = newSurface(pixelFormat(cmRgb, st, true))
  for y in 0 ..< 64:
    for x in 0 ..< 96:
      let v = float32((x div 4 + y div 4) mod 2)
      result.writePixel(x, y, [v, float32(x) / 96'f32, float32(y) / 64'f32, 1'f32])

proc bounds(): Rect = rect(0, 0, 96, 64)

proc worst(a, b: Surface; r: Rect): float32 =
  let (x, y) = (a.readRegion(r), b.readRegion(r))
  for i in 0 ..< x.len: result = max(result, abs(x[i] - y[i]))

proc lasso(points: openArray[array[2, float64]]; amount: Option[float64]): LiquifyStroke =
  result = newLiquifyStroke(ltLassoMask, 1)
  for p in points: result.points.add @[p[0], p[1]]
  result.amount = amount

proc inside(p: array[2, float64]; poly: openArray[array[2, float64]]): bool =
  ## Reference even-odd test (ray casting), to check the scanline fill against.
  var j = poly.len - 1
  for i, a in poly:
    let b = poly[j]
    if (a[1] > p[1]) != (b[1] > p[1]) and p[0] < (b[0] - a[0]) * (p[1] - a[1]) / (b[1] - a[1]) + a[0]:
      result = not result
    j = i

proc stroke(tool: LiquifyTool; size: float64; pts: seq[seq[float64]]): LiquifyStroke =
  result = newLiquifyStroke(tool, size)
  result.points = pts

suite "liquify":
  test "a segment to a far-off point returns":
    var f = newLiquifyField(rect(0, 0, 24, 16), 2)
    let s = newLiquifyStroke(ltForwardWarp, 30)
    let dirty = f.strokeSegment(s, [0.0, 0.0, 1.0], [1e15, 0.0, 1.0])
    check not dirty.isEmpty and dirty.x1 <= 64
    discard f.strokeSegment(s, [-1e308, 5.0, 1.0], [1e308, 5.0, 1.0])
    discard f.strokeSegment(newLiquifyStroke(ltBloat, 30), [5.0, -1e15, 1.0], [5.0, 1e15, 1.0])
    # A valid stroke the Rust crate panics on (a dab margin overflowing `usize`).
    discard f.applyStroke(stroke(ltForwardWarp, 40, @[@[20.0, 20.0], @[1e200, 30.0]]))
    # Unvalidated strokes (NaN points, absurd sizes) do nothing harmful either.
    for st in [stroke(ltTwirlCw, 1e300, @[@[NaN, 3.0], @[4.0, Inf]]), stroke(ltSmooth, NaN, @[@[1.0]]),
               stroke(ltForwardWarp, -5, @[@[1.0, 2.0], @[NegInf, 2.0]])]:
      discard f.applyStroke(st)
      check st.validate().isSome
  test "lasso freezes exactly the nodes inside a concave polygon":
    let poly = [[10.0, 8.0], [70.0, 8.0], [70.0, 20.0], [30.0, 22.5], [33.0, 40.0], [80.0, 41.0], [60.0, 58.0], [9.0, 55.0]]
    for cell in [1.0, 2.0, 3.5]:
      var f = newLiquifyField(bounds(), cell)
      check f.applyStroke(lasso(poly, none(float64))) == rect(9, 8, 80, 58)
      var ok = true
      for j in 0 ..< f.h:
        for i in 0 ..< f.w:
          let want = (if inside(f.nodePos(i, j), poly): 1'f32 else: 0'f32)
          ok = ok and f.freeze[j * f.w + i] == want
      check ok and 1'f32 in f.freeze
      check fromStrokes(bounds(), cell, [lasso(poly, none(float64))]) == f
  test "lasso thaws with amount zero":
    var f = newLiquifyField(bounds(), 2)
    discard f.applyStroke(newLiquifyStroke(ltFreezeAll, 1))
    discard f.applyStroke(lasso([[20.0, 20.0], [40.0, 20.0], [40.0, 40.0], [20.0, 40.0]], some(0.0)))
    check f.freezeAt(30, 30) == 0 and f.freezeAt(5, 5) == 1
  test "lasso ignores degenerate and hostile polygons":
    let empty = newLiquifyField(bounds(), 2)
    let cases = @[newSeq[array[2, float64]](), @[[10.0, 10.0], [50.0, 50.0]], @[[NaN, 10.0], [50.0, Inf], [20.0, 30.0]],
                  @[[-50.0, -50.0], [-10.0, -50.0], [-10.0, -10.0]], @[[200.0, 10.0], [300.0, 10.0], [250.0, 50.0]],
                  @[[10.0, 1e12], [50.0, 1e12], [30.0, 2e12]]]
    for poly in cases:
      var f = empty
      check f.applyStroke(lasso(poly, none(float64))) == EmptyRect
      check f == empty
    var f = empty
    discard f.applyStroke(lasso([[-1e15, -1e15], [1e15, -1e15], [1e15, 1e15], [-1e15, 1e15]], none(float64)))
    var all = true
    for v in f.freeze: all = all and v == 1
    check all
    var g = empty
    discard g.applyStroke(lasso([[10.0, 10.0], [NaN, 0.0], [50.0, 10.0], [50.0, 50.0], [10.0, 50.0]], none(float64)))
    check g.freezeAt(30, 30) == 1
  test "the brush fades to nothing at its rim at every density":
    for density in [0.0, 0.25, 0.5, 0.75, 1.0, NaN, -3.0, 7.0]:
      check falloff(0, density) == 1 and falloff(1, density) == 0
      check falloff(0.98 * 0.98, density) < 0.02
      var prev = 1.0
      for k in 1 .. 100:
        let v = falloff(float64(k) / 100, density)
        check v <= prev and v >= 0
        prev = v
    for k in 1 ..< 100:
      let t2 = float64(k) / 100
      check falloff(t2, 1) >= falloff(t2, 0.5) and falloff(t2, 0.5) >= falloff(t2, 0)
    check falloff(0.5, 1) > 0.5 and falloff(0.5, 0) < 0.2
  test "forward warp leaves the brush rim in place":
    let (r, drag) = (20.0, 4.0)
    for density in [0.0, 50.0, 100.0]:
      var s = stroke(ltForwardWarp, 2 * r, @[@[48.0, 32.0], @[48.0 + drag, 32.0]])
      s.density = density
      let f = fromStrokes(bounds(), 1, [s])
      var rim = 0
      for j in 0 ..< f.h:
        for i in 0 ..< f.w:
          let p = f.nodePos(i, j)
          let t = hypot(p[0] - 48, p[1] - 32) / r
          if t >= 0.97 and t < 1:
            let d = f.d[j * f.w + i]
            check float64(hypot(d[0], d[1])) < 0.03 * drag
            inc rim
      check rim > 20 and f.sample(48, 32)[0] < -0.9 * drag
  test "no strokes is identity at every depth":
    for st in [stU8, stU16, stF32]:
      let s = sample(st)
      check applyLiquify(s, newLiquifyField(bounds(), 2)) == s
      var s0 = stroke(ltForwardWarp, 30, @[@[40.0, 30.0], @[60.0, 30.0]])
      s0.pressure = 0
      check fromStrokes(bounds(), 2, [s0]).isIdentity
  test "full-resolution preview matches the unit-scale proxy and partial updates":
    let canvas = rect(0, 0, 96, 64)
    for st in [stU8, stU16, stF32]:
      let source = sample(st)
      var field = fromStrokes(canvas, 2, [stroke(ltForwardWarp, 32, @[@[38.0, 30.0, 1.0], @[52.0, 34.0, 0.8]])])
      let proxy = newProxyImage(source, canvas, 96)
      check proxy.scale == 1
      var want = newSeq[array[4, uint8]](proxy.w * proxy.h)
      proxy.render(field, [0, 0, proxy.w, proxy.h], want)
      var got = newSeq[array[4, uint8]](proxy.w * proxy.h)
      var scratch: seq[array[4, float32]]
      check renderFullResolution(source, field, canvas, canvas, got, scratch)
      check got == want
      let dirty = field.applyStroke(stroke(ltBloat, 16, @[@[43.0, 32.0, 1.0]]))
      proxy.render(field, proxy.proxyRect(dirty), want)
      check renderFullResolution(source, field, canvas, dirty, got, scratch)
      check got == want
  test "forward warp displaces along the stroke and reconstruct restores":
    var f = fromStrokes(bounds(), 2, [stroke(ltForwardWarp, 40, @[@[40.0, 32.0], @[52.0, 32.0]])])
    let d = f.sample(52, 32)
    check d[0] < -4 and abs(d[1]) < 0.5
    var img = newSurface(pixelFormat(cmGrayscale, stF32, true))
    img.fillRect(bounds(), [0'f32, 1])
    img.fillRect(rect(40, 28, 42, 36), [1'f32, 1])
    let o = applyLiquify(img, f)
    var brightest = 30
    for x in 30 ..< 70:
      if o.pixel(x, 32)[0] >= o.pixel(brightest, 32)[0]: brightest = x
    check brightest > 44
    var rec = newLiquifyStroke(ltReconstructAll, 10)
    rec.amount = some(100.0)
    discard f.applyStroke(rec)
    check f.isIdentity and applyLiquify(img, f) == img
  test "the reconstruct brush reduces the field and freeze protects":
    let s = stroke(ltForwardWarp, 40, @[@[30.0, 32.0], @[45.0, 32.0]])
    var f = fromStrokes(bounds(), 2, [s])
    let before = f.maxDisplacement
    var pts: seq[seq[float64]]
    for _ in 0 ..< 30: pts.add @[40.0, 32.0]
    discard f.applyStroke(stroke(ltReconstruct, 80, pts))
    check f.maxDisplacement < before * 0.2
    var fr = stroke(ltFreeze, 60, @[@[40.0, 32.0]])
    fr.density = 100
    var g = newLiquifyField(bounds(), 2)
    discard g.applyStroke(fr)
    check g.freezeAt(40, 32) > 0.99
    discard g.applyStroke(s)
    check abs(g.sample(40, 32)[0]) < 1e-3
    var th = fr
    th.tool = ltThaw
    discard g.applyStroke(th)
    check g.freezeAt(40, 32) < 0.01
    discard g.applyStroke(newLiquifyStroke(ltFreezeAll, 1))
    discard g.applyStroke(newLiquifyStroke(ltInvertFreeze, 1))
    var zero = true
    for v in g.freeze: zero = zero and v == 0
    check zero
    discard g.applyStroke(fr)
    discard g.applyStroke(newLiquifyStroke(ltInvertFreeze, 1))
    check g.freezeAt(40, 32) < 0.01 and g.freezeAt(5, 5) > 0.99
  test "twirl, pucker, bloat and push left have the right sense":
    proc at(tool: LiquifyTool; pts: seq[seq[float64]]): LiquifyField = fromStrokes(bounds(), 2, [stroke(tool, 40, pts)])
    var c: seq[seq[float64]]
    for _ in 0 ..< 10: c.add @[48.0, 32.0]
    check at(ltPucker, c).sample(58, 32)[0] > 0.1 and at(ltBloat, c).sample(58, 32)[0] < -0.1
    check at(ltTwirlCw, c).sample(58, 32)[1] < -0.1 and at(ltTwirlCcw, c).sample(58, 32)[1] > 0.1
    check at(ltPushLeft, @[@[48.0, 44.0], @[48.0, 20.0]]).sample(48, 32)[0] > 0.5
    var g = fromStrokes(bounds(), 2, [stroke(ltForwardWarp, 12, @[@[40.0, 32.0], @[50.0, 32.0]])])
    let m0 = g.maxDisplacement
    var pts: seq[seq[float64]]
    for _ in 0 ..< 20: pts.add @[45.0, 32.0]
    discard g.applyStroke(stroke(ltSmooth, 60, pts))
    check g.maxDisplacement < m0
  test "replay is deterministic and depths agree":
    let s = stroke(ltTwirlCw, 50, @[@[48.0, 32.0, 0.5], @[50.0, 33.0, 1.0], @[52.0, 34.0]])
    let a = fromStrokes(bounds(), 2, [s])
    check a == fromStrokes(bounds(), 2, [s])
    check worst(applyLiquify(sample(stU8), a), applyLiquify(sample(stF32), a), bounds()) < 2 / 255 + 1e-4
  test "the proxy matches the full render roughly":
    let src = sample(stU8)
    var pts: seq[seq[float64]]
    for _ in 0 ..< 8: pts.add @[48.0, 32.0]
    let f = fromStrokes(bounds(), 2, [stroke(ltBloat, 40, pts)])
    let p = newProxyImage(src, bounds(), 96)
    check (p.w, p.h, p.scale) == (96, 64, 1.0)
    var o = newSeq[array[4, uint8]](p.w * p.h)
    p.render(f, [0, 0, p.w, p.h], o)
    let full = applyLiquify(src, f)
    var bad = 0
    for y in 0 ..< 64:
      for x in 0 ..< 96:
        if abs(float32(o[y * 96 + x][1]) / 255 - full.pixel(x, y)[1]) > 0.1: inc bad
    check bad < 96 * 64 div 50
  test "strokes round-trip through JSON":
    var s = stroke(ltTwirlCcw, 33.5, @[@[1.0, 2.0], @[3.0, 4.0, 0.5]])
    s.amount = some(40.0)
    check strokeFromJson(s.toJson) == s
    let d = strokeFromJson(parseJson("""{"points": [[1, 2]]}"""))
    check d.tool == ltForwardWarp and d.size == 100 and d.density == 50 and d.pressure == 100 and d.rate == 80 and d.amount.isNone
    expect ValueError: discard strokeFromJson(parseJson("""{"tool": "melt"}"""))
    expect ValueError: discard strokeFromJson(parseJson("""{"size": "big"}"""))
