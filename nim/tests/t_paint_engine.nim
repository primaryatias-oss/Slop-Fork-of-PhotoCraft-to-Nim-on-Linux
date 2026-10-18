## Brush engine (`paint/src/lib.rs`, `brush_tests.rs` and `render_reference.rs` tests): tips,
## spacing, dynamics, masks, smoothing, determinism, formats, and the computed-tip fast path
## against the scalar rasteriser bit for bit. The renderer is included so its internals can be
## checked.

import std/[unittest, sequtils, sets, algorithm]
include photocraft/paint/render
import photocraft/paint/presets, photocraft/serde

const
  Rgba8 = PixelFormat(mode: cmRgb, sample: stU8, alpha: true)
  Rgba16 = PixelFormat(mode: cmRgb, sample: stU16, alpha: true)
  Rgba32 = PixelFormat(mode: cmRgb, sample: stF32, alpha: true)
  Graya8 = PixelFormat(mode: cmGrayscale, sample: stU8, alpha: true)
  Gray8 = PixelFormat(mode: cmGrayscale, sample: stU8, alpha: false)
  Cmyka8 = PixelFormat(mode: cmCmyk, sample: stU8, alpha: true)

template tweak(base, body: untyped): untyped =
  block:
    var it {.inject.} = base
    body
    it

proc tb(): BrushSettings = defaultBrush().tweak((it.pressureSize = false; it.spacing = 0.25))

proc line(x0, x1, y: float64): seq[StrokePoint] = @[strokePoint(x0, y, 1), strokePoint(x1, y, 1)]

proc dabsOf(b: BrushSettings; pts: openArray[StrokePoint]): seq[Dab] = dabs(Stroke(brush: b, points: @pts))

proc raster(b: BrushSettings; d: Dab): (Rect, seq[float32]) =
  let ctx = newBrushContext(b)
  let r = ctx.dabRect(d, false)
  var v: seq[float32]
  ctx.rasterize(d, false, r, v)
  (r, v)

proc at(r: (Rect, seq[float32]); x, y: int): float32 = r[1][(y - r[0].y0) * r[0].width + (x - r[0].x0)]

proc paint(b: BrushSettings; pts: openArray[StrokePoint]): Surface =
  result = newSurface(Rgba32)
  discard renderStroke(result, b, pts, none(Surface), false, 1)

proc alphaSum(s: Surface; r: Rect): float32 =
  for y in r.y0 ..< r.y1:
    for x in r.x0 ..< r.x1: result += s.rgba(x, y)[3]

proc libStroke(points: openArray[(float64, float64)]; size: float32): Stroke =
  result = Stroke(brush: defaultBrush().tweak((it.size = size; it.pressureSize = false)))
  for (x, y) in points: result.points.add strokePoint(x, y, 1)

proc pen(x, y: float64; tiltX, tiltY, rotation: float32): StrokePoint =
  result = strokePoint(x, y, 1)
  result.tiltX = tiltX
  result.tiltY = tiltY
  result.rotation = rotation

proc extents(b: BrushSettings; d: Dab): (int, int) =
  ## Coverage extent (pixels above half coverage) of a rasterised dab along x and y through its centre.
  let r = raster(b, d)
  let (cx, cy) = (int(floor(d.center.x)), int(floor(d.center.y)))
  var w, h = 0
  for x in r[0].x0 ..< r[0].x1:
    if at(r, x, cy) > 0.5: inc w
  for y in r[0].y0 ..< r[0].y1:
    if at(r, cx, y) > 0.5: inc h
  (w, h)

proc allDabs(b: BrushSettings; pts: openArray[StrokePoint]): (seq[Dab], seq[Dab]) =
  var g = newDabGenerator(b, 1)
  g.push(pts, result[0], result[1])
  g.finish(result[0], result[1])

proc compare(ctx: BrushContext; dab: Dab; dual: bool; rect: Rect; withDepth: bool) =
  ## The fast path against the scalar loop; reused, dirty buffers also check clearing and resizing.
  var (actual, expected) = (newSeqWith(19, 0.75'f32), newSeqWith(19, 0.75'f32))
  var (ad, ed) = (newSeqWith(23, 0.25'f32), newSeqWith(23, 0.25'f32))
  ctx.rasterizeInto(dab, dual, rect, actual, ad, withDepth)
  ctx.rasterizeInto(dab, dual, rect, expected, ed, withDepth, scalarOnly = true)
  check actual.len == expected.len
  check actual.mapIt(cast[uint32](it)) == expected.mapIt(cast[uint32](it))
  check ad.mapIt(cast[uint32](it)) == ed.mapIt(cast[uint32](it))

proc crossSection(b: BrushSettings; pts: openArray[StrokePoint]; x: int): seq[float32] =
  var s = newSurface(Rgba32)
  discard renderStroke(s, b, pts, none(Surface), false, 1)
  for y in 100 ..< 170: result.add s.rgba(x, y)[3]

proc softEdgeWidth(profile: seq[float32]): int =
  proc below(v: float32): int =
    for i, a in profile:
      if a < v: return i
    profile.len
  max(below(0.1) - below(0.9), 0)

proc softLowFlow(fmt: PixelFormat): Surface =
  result = surfaceWithDefault(fmt, [1'f32, 1, 1, 1])
  let b = defaultBrush().tweak((it.size = 120; it.hardness = 0; it.flow = 0.05; it.opacity = 0.3;
                                it.color = [0'f32, 0, 0, 1]; it.pressureSize = false))
  discard renderStroke(result, b, line(0, 300, 100), none(Surface), false, 1)

proc worstRowError(got, want: Surface): float32 =
  for y in 30 ..< 170:
    var g, w = 0'f32
    for x in 100 ..< 200:
      g += got.rgba(x, y)[0]
      w += want.rgba(x, y)[0]
    result = max(result, abs(g - w) / 100 * 255)

suite "paint: basics (lib.rs)":
  test "dab spacing":
    let s = libStroke([(0.0, 0.0), (100.0, 0.0)], 10)
    let d = dabs(s)
    check d.len in 95 .. 105
    var s2 = s
    s2.brush = defaultBrush().tweak((it.spacing = 1; it.pressureSize = false; it.size = 10))
    check dabs(s2).len == 11

  test "coverage profile":
    check dabCoverage(0, 10, 1) == 1
    check dabCoverage(20, 10, 1) == 0
    let softMid = dabCoverage(5, 10, 0)
    check softMid > 0.2 and softMid < 0.8
    check dabCoverage(9.9, 10, 1) > 0.5

  test "hard brush paints a solid line":
    var s = newSurface(Rgba8)
    let dmg = applyStroke(s, libStroke([(10.0, 10.0), (90.0, 10.0)], 8), none(Surface), false)
    check dmg.contains(50, 10)
    check s.pixel(50, 10) == @[0'f32, 0, 0, 1]
    check s.pixel(50, 30)[3] == 0

  test "opacity caps overlap":
    var s = newSurface(Rgba8)
    var st = libStroke([(10.0, 10.0), (60.0, 10.0), (10.0, 10.0)], 10)
    st.brush.opacity = 0.5
    discard applyStroke(s, st, none(Surface), false)
    check abs(s.pixel(30, 10)[3] - 0.5) < 0.02

  test "selection limits paint":
    var s = newSurface(Rgba8)
    var sel = newSurface(Gray8)
    sel.fillRect(rect(0, 0, 50, 100), [1'f32])
    discard applyStroke(s, libStroke([(10.0, 10.0), (90.0, 10.0)], 8), some(sel), false)
    check s.pixel(20, 10)[3] == 1
    check s.pixel(70, 10)[3] == 0

  test "eraser removes alpha":
    var s = newSurface(Rgba8)
    s.fillRect(rect(0, 0, 100, 20), [1'f32, 0, 0, 1])
    var st = libStroke([(10.0, 10.0), (90.0, 10.0)], 8)
    st.brush.erase = true
    discard applyStroke(s, st, none(Surface), false)
    check s.pixel(50, 10)[3] == 0
    check s.pixel(50, 1)[3] == 1

  test "lock transparency preserves alpha":
    var s = newSurface(Rgba8)
    s.fillRect(rect(0, 0, 50, 20), [1'f32, 1, 1, 1])
    discard applyStroke(s, libStroke([(10.0, 10.0), (90.0, 10.0)], 8), none(Surface), true)
    check s.pixel(20, 10) == @[0'f32, 0, 0, 1]
    check s.pixel(70, 10)[3] == 0

  test "pressure changes size":
    var st = libStroke([(0.0, 0.0), (100.0, 0.0)], 20)
    st.brush.pressureSize = true
    st.points[0].pressure = 0.1
    st.points[1].pressure = 1
    let d = dabs(st)
    check d[0].radius < d[^1].radius

  test "smoothing keeps endpoints":
    var pts: seq[StrokePoint]
    for i in 0 ..< 20: pts.add strokePoint(float64(i), (if i mod 2 == 0: 0.0 else: 10.0), 1)
    let s = brush.smooth(pts, 0.8)
    check s[0] == pts[0]
    check s[^1] == pts[^1]
    var jitter, orig = 0.0
    for i in 1 ..< 19:
      jitter += abs(s[i].y - 5)
      orig += abs(pts[i].y - 5)
    check jitter < orig

  test "works at all depths":
    for f in [Rgba8, Rgba16, Rgba32, Graya8, Cmyka8]:
      var s = newSurface(f)
      var st = libStroke([(5.0, 5.0), (40.0, 5.0)], 6)
      st.brush.color = [1'f32, 1, 1, 1]
      discard applyStroke(s, st, none(Surface), false)
      let c = s.rgba(20, 5)
      check c[3] > 0.99 and c[0] > 0.98

suite "paint: tips":
  test "round tip roundness and angle":
    var d = roundDab(point(50, 50), 20, 1)
    d.roundness = 0.25
    var r = raster(tb(), d)
    check at(r, 65, 50) > 0.99
    check at(r, 50, 58) < 0.01
    check at(r, 50, 53) > 0.99
    d.angle = float32(PI / 2)
    r = raster(tb(), d)
    check at(r, 50, 35) > 0.99
    check at(r, 58, 50) < 0.01
    d.angle = float32(PI / 4)
    r = raster(tb(), d)
    check at(r, 60, 40) > 0.99
    check at(r, 60, 60) < 0.01

  test "hardness profile is monotonic":
    for h in [0'f32, 0.5, 1]:
      let r = raster(tb().tweak((it.hardness = h)), roundDab(point(50, 50), 20, 1))
      let prof = toSeq(0 ..< 22).mapIt(at(r, 50 + it, 50))
      for i in 1 ..< prof.len: check prof[i] <= prof[i - 1] + 1e-6
      check prof[0] > 0.99
      check prof[21] == 0
      let mid = at(r, 60, 50)
      if h == 1: check mid > 0.99
      elif h == 0: check mid > 0.2 and mid < 0.8

  test "a hard edge is sharp":
    let r = raster(tb().tweak((it.hardness = 1)), roundDab(point(50, 50), 20, 1))
    let prof = toSeq(0 ..< 22).mapIt(at(r, 50 + it, 50))
    check prof.countIt(it >= 0.01 and it < 0.99) <= 3
    check prof[17] > 0.99
    check prof[^1] == 0
    let rs = raster(tb().tweak((it.hardness = 0)), roundDab(point(50, 50), 20, 1))
    let profs = toSeq(0 ..< 22).mapIt(at(rs, 50 + it, 50))
    check profs.countIt(it >= 0.01 and it < 0.99) >= 10

  test "a sampled tip is scaled and oriented":
    let tip = grayTileFromFn(16, 16, proc(x, y: uint32): float32 = (if x < 8: 1'f32 else: 0'f32))
    let b = tb().tweak((it.tip = TipShape(kind: tkSampled, sampled: tip)))
    let d = roundDab(point(50, 50), 20, 1)
    var r = raster(b, d)
    check at(r, 40, 50) > 0.99 and at(r, 60, 50) < 0.01
    var fd = d
    fd.flipX = true
    r = raster(b, fd)
    check at(r, 40, 50) < 0.01 and at(r, 60, 50) > 0.99
    var rd = d
    rd.angle = float32(PI / 2)
    r = raster(b, rd)
    check at(r, 50, 60) > 0.99 and at(r, 50, 40) < 0.01
    r = raster(b, roundDab(point(50, 50), 2, 1))
    check r[1].foldl(a + b, 0'f32) > 1

  test "the pencil is aliased":
    let b = tb().tweak((it.aliased = true; it.hardness = 0; it.size = 9))
    var s = paint(b, line(10, 50, 20.5))
    for y in 10 ..< 32:
      for x in 0 ..< 64:
        let a = s.rgba(x, y)[3]
        check a == 0 or a == 1
    check s.rgba(30, 20)[3] == 1
    s = paint(b.tweak((it.aliased = false)), line(10, 50, 20.3))
    var partial = false
    for y in 10 ..< 32:
      for x in 0 ..< 64:
        let a = s.rgba(x, y)[3]
        if a >= 0.01 and a < 0.99: partial = true
    check partial

  test "pencil dabs sit on the pixel grid":
    check gridCenter(10, 5, 1) == (10.5'f32, 5.5'f32)
    check gridCenter(10.9, 5.2, 3) == (10.5'f32, 5.5'f32)
    check gridCenter(10.4, 5.6, 2) == (10'f32, 6'f32)
    check gridSquare(10.2, 5.7, 1) == [10.0, 5.0, 11.0, 6.0]
    check gridSquare(10.2, 5.7, 4) == [8.0, 4.0, 12.0, 8.0]
    check gridSquare(NaN, 0, NaN).len == 4
    let b = tb().tweak((it.aliased = true; it.hardness = 1; it.size = 1))
    var s = paint(b, [strokePoint(7, 4, 1)])
    for y in 0 ..< 16:
      for x in 0 ..< 16: check s.rgba(x, y)[3] == (if (x, y) == (7, 4): 1'f32 else: 0'f32)
    s = paint(b, line(2, 20, 9.3))
    for x in 2 ..< 20:
      check s.rgba(x, 9)[3] == 1
      check s.rgba(x, 8)[3] + s.rgba(x, 10)[3] == 0
    let b4 = b.tweak((it.size = 4))
    s = paint(b4, [strokePoint(10.2, 5.7, 1)])
    let sq = gridSquare(10.2, 5.7, 4)
    for y in 0 ..< 16:
      for x in 0 ..< 24:
        let inside = float64(x) >= sq[0] and float64(x) < sq[2] and float64(y) >= sq[1] and float64(y) < sq[3]
        check inside or s.rgba(x, y)[3] == 0
    check s.rgba(9, 5)[3] == 1

  test "a 1 px pencil line is a clean 8-connected staircase":
    let b = tb().tweak((it.aliased = true; it.hardness = 1; it.size = 1))
    for (p0, p1) in [((2.5, 2.5), (22.5, 9.5)), ((3.5, 30.5), (10.5, 5.5)), ((1.2, 1.7), (25.8, 4.1)),
                     ((5.5, 5.5), (17.5, 17.5)), ((20.3, 3.6), (4.9, 12.2))]:
      let s = paint(b, [strokePoint(p0[0], p0[1], 1), strokePoint(p1[0], p1[1], 1)])
      var painted: seq[(int, int)]
      for y in 0 ..< 40:
        for x in 0 ..< 40:
          if s.rgba(x, y)[3] > 0: painted.add (x, y)
      let (a, e) = ((floor(p0[0]), floor(p0[1])), (floor(p1[0]), floor(p1[1])))
      let steep = abs(e[1] - a[1]) > abs(e[0] - a[0])
      proc major(p: (float64, float64)): (float64, float64) = (if steep: (p[1], p[0]) else: p)
      let ((m0, n0), (m1, n1)) = (major(a), major(e))
      check float64(painted.len) == abs(m1 - m0) + 1
      var cols = initHashSet[int64]()
      for (x, y) in painted:
        let (m, n) = major((float64(x), float64(y)))
        let ideal = n0 + (n1 - n0) * (m - m0) / (m1 - m0)
        check abs(n - ideal) <= 0.5 + 1e-9
        cols.incl int64(m)
      check cols.len == painted.len

suite "paint: spacing and dynamics":
  test "spacing sets the dab count":
    for (spacing, len, expect) in [(0.25'f32, 100.0, 21), (1'f32, 100.0, 6), (0.5'f32, 200.0, 21)]:
      check dabsOf(tb().tweak((it.spacing = spacing; it.size = 20)), line(0, len, 0)).len == expect
    let b = tb().tweak((it.spacing = 0.5; it.size = 20))
    let pts = toSeq(0 .. 20).mapIt(strokePoint(float64(it) * 10, 0, 1))
    let d = dabsOf(b, pts)
    check d.len == 21
    for i in 1 ..< d.len: check abs((d[i].center.x - d[i - 1].center.x) - 10) < 1e-6

  test "a stroke has no seams":
    for spacing in [0.1'f32, 0.25, 0.5]:
      let s = paint(tb().tweak((it.spacing = spacing; it.size = 20)), line(20, 180, 100.5))
      for y in 98 .. 102:
        for x in 40 .. 160: check s.rgba(x, y)[3] > 0.99

  test "pressure controls size with a minimum":
    let b = tb().tweak((it.size = 40; it.shapeDynamics.enabled = true;
                        it.shapeDynamics.size = Dynamic(control: ctlPenPressure, minimum: 0.25, fadeSteps: 25)))
    let d = dabsOf(b, [strokePoint(0, 0, 0), strokePoint(200, 0, 1)])
    check abs(d[0].radius - 5) < 1e-3
    check abs(d[^1].radius - 20) < 0.5
    for i in 1 ..< d.len: check d[i].radius >= d[i - 1].radius - 1e-4

  test "fade ramps over steps":
    let b = tb().tweak((it.size = 20; it.spacing = 0.5; it.shapeDynamics.enabled = true;
                        it.shapeDynamics.size = Dynamic(control: ctlFade, fadeSteps: 10)))
    let d = dabsOf(b, line(0, 300, 0))
    check abs(d[0].radius - 10) < 1e-3
    check d[5].radius < d[0].radius * 0.6
    for x in d[10 .. ^1]: check x.radius <= 0.5 + 1e-6
    let t = tb().tweak((it.transfer.enabled = true; it.transfer.opacity = Dynamic(control: ctlFade, fadeSteps: 4)))
    let dt = dabsOf(t, line(0, 100, 0))
    check dt[0].opacity == 1
    check dt[4].opacity == 0

  test "tilt, direction and rotation drive the angle":
    proc mk(c: Control): BrushSettings = tb().tweak((it.shapeDynamics.enabled = true; it.shapeDynamics.angle = controlled(c)))
    var p = strokePoint(0, 0, 1)
    p.tiltY = -45
    var q = p
    q.x = 50
    var d = dabsOf(mk(ctlPenTilt), [p, q])
    check abs(toDegrees(d[0].angle) - 90) < 1e-3
    d = dabsOf(mk(ctlDirection), [strokePoint(0, 0, 1), strokePoint(0, 50, 1)])
    for x in d: check abs(toDegrees(x.angle) + 90) < 1e-3
    var r = strokePoint(0, 0, 1)
    r.rotation = 30
    var r2 = r
    r2.x = 20
    d = dabsOf(mk(ctlRotation), [r, r2])
    check abs(toDegrees(d[0].angle) - 30) < 1e-3
    let b = tb().tweak((it.shapeDynamics.enabled = true; it.shapeDynamics.size = controlled(ctlPenTilt); it.size = 20))
    var flat = strokePoint(0, 0, 1)
    flat.tiltX = 90
    check dabsOf(b, [flat])[0].radius <= 0.5 + 1e-6

  test "jitter is deterministic per seed":
    let b = tb().tweak((it.size = 30; it.seed = 42; it.shapeDynamics.enabled = true; it.shapeDynamics.size = jitterDyn(1);
                        it.shapeDynamics.angle = jitterDyn(1);
                        it.shapeDynamics.roundness = Dynamic(jitter: 1, minimum: 0.2, fadeSteps: 25);
                        it.shapeDynamics.flipXJitter = true; it.shapeDynamics.flipYJitter = true;
                        it.scattering = Scattering(enabled: true, scatter: jitterDyn(2), bothAxes: true, count: 3,
                                                   countJitter: jitterDyn(1))))
    let pts = line(0, 300, 50)
    let a = dabsOf(b, pts)
    check a == dabsOf(b, pts)
    check a != dabsOf(b.tweak((it.seed = 43)), pts)
    check a.anyIt(it.radius < 10) and a.allIt(it.radius <= 15 + 1e-4)
    check a.allIt(it.roundness >= 0.2 - 1e-6 and it.roundness <= 1)
    check a.anyIt(it.flipX) and a.anyIt(not it.flipX)
    var r = newStrokeRenderer(b, none(PixelFormat), 1).recordDabs
    let many = toSeq(0 .. 30).mapIt(strokePoint(float64(it) * 10, 50, 1))
    var i = 0
    while i < many.len:
      r.push(many[i ..< min(i + 4, many.len)])
      i += 4
    r.finish()
    check r.dabs == dabsOf(b, many)

  test "scattering stays in bounds":
    for both in [false, true]:
      let b = tb().tweak((it.size = 20; it.scattering = Scattering(enabled: true, scatter: jitterDyn(2), bothAxes: both,
                                                                   count: 4, countJitter: defaultDynamic())))
      let d = dabsOf(b, line(0, 400, 100))
      check d.len == 81 * 4
      check d.allIt(abs(it.center.y - 100) <= 20 + 1e-6)
      check d.anyIt(abs(it.center.y - 100) > 10)
      if not both: check d.allIt(abs(it.center.x / 5 - round(it.center.x / 5)) < 1e-6)
      else: check d.anyIt(abs(it.center.x / 5 - round(it.center.x / 5)) > 0.1)

  test "zero scatter keeps dabs on the stroke":
    let on = tb().tweak((it.scattering.enabled = true))
    for b in [tb(), on]:
      let d = dabsOf(b, line(10, 90, 40.5))
      check d.len > 4
      check d.allIt(abs(it.center.y - 40.5) < 1e-9)

  test "colour dynamics ranges":
    let base = tb().tweak((it.color = [1'f32, 0, 0, 1]; it.background = [0'f32, 0, 1, 1]; it.seed = 5))
    var b = base.tweak((it.colorDynamics.enabled = true; it.colorDynamics.fgBg = jitterDyn(1)))
    var d = dabsOf(b, line(0, 200, 0))
    check d.allIt(abs(it.color[1]) < 1e-5 and abs(it.color[0] + it.color[2] - 1) < 1e-3 or max(it.color[0], it.color[2]) >= 0.999)
    check d.anyIt(it.color[2] > 0.5) and d.anyIt(it.color[0] > 0.5)
    b = base.tweak((it.colorDynamics.enabled = true; it.colorDynamics.hueJitter = 1))
    d = dabsOf(b, line(0, 200, 0))
    for x in d:
      check abs(max(x.color[0], max(x.color[1], x.color[2])) - 1) < 1e-4
      check abs(min(x.color[0], min(x.color[1], x.color[2]))) < 1e-4
    check d.anyIt(it.color[1] > 0.5 or it.color[2] > 0.5)
    b = base.tweak((it.colorDynamics.enabled = true; it.colorDynamics.purity = -1))
    d = dabsOf(b, line(0, 20, 0))
    check d.allIt(abs(it.color[0] - it.color[1]) < 1e-5 and abs(it.color[1] - it.color[2]) < 1e-5)
    b = base.tweak((it.colorDynamics.enabled = true; it.colorDynamics.perTip = false; it.colorDynamics.hueJitter = 1))
    d = dabsOf(b, line(0, 200, 0))
    for i in 1 ..< d.len: check d[i].color == d[i - 1].color
    b = base.tweak((it.size = 10; it.spacing = 3; it.colorDynamics.enabled = true; it.colorDynamics.fgBg = jitterDyn(1)))
    let s = paint(b, line(10, 310, 20))
    let colours = toSeq(0 ..< 11).mapIt(s.rgba(10 + it * 30, 20))
    check colours.anyIt(it[2] > 0.6) and colours.anyIt(it[0] > 0.6)

  test "transfer opacity caps and flow builds":
    var b = tb().tweak((it.transfer.enabled = true; it.transfer.opacity = controlled(ctlPenPressure); it.size = 10))
    let s = paint(b, [strokePoint(10, 10, 0.4), strokePoint(90, 10, 0.4)])
    check abs(s.rgba(50, 10)[3] - 0.4) < 0.01
    b = tb().tweak((it.flow = 0.1; it.opacity = 0.8; it.size = 10))
    let one = paint(b, [strokePoint(50, 10, 1)]).rgba(50, 10)[3]
    let many = paint(b, line(40, 60, 10)).rgba(50, 10)[3]
    check abs(one - 0.08) < 0.01
    check many > one * 3 and many <= 0.8 + 1e-4
    b = tb().tweak((it.transfer.enabled = true; it.transfer.flow = jitterDyn(1)))
    let d = dabsOf(b, line(0, 100, 0))
    check d.anyIt(it.alpha < 0.5) and d.allIt(it.alpha <= 1)

  test "build-up accumulates over time":
    let b = tb().tweak((it.buildUp = true; it.buildUpRate = 20; it.flow = 0.05; it.size = 20; it.hardness = 1))
    proc hold(ms: float64): (int, float32) =
      var a = strokePoint(50, 50, 1)
      var z = a
      z.time = ms
      (dabsOf(b, [a, z]).len, paint(b, [a, z]).rgba(50, 50)[3])
    let (n0, a0) = hold(0)
    let (n1, a1) = hold(500)
    let (n2, a2) = hold(2000)
    check (n0, n1, n2) == (1, 11, 41)
    check a0 < a1 and a1 < a2
    let nb = b.tweak((it.buildUp = false))
    var z = strokePoint(50, 50, 1)
    z.time = 2000
    check dabsOf(nb, [strokePoint(50, 50, 1), z]).len == 1

  test "a huge airbrush time gap emits a bounded catch-up":
    let b = tb().tweak((it.buildUp = true; it.buildUpRate = 1000))
    let a = strokePoint(20, 20, 1)
    var pause = a
    pause.time = 200
    check dabsOf(b, [a, pause]).len == 201
    var z = a
    z.time = F64Max
    let d = dabsOf(b, [a, z])
    check d.len <= 2
    check d[^1].center == point(20, 20)
    var inf = a
    inf.time = Inf
    check dabsOf(b, [a, inf]).len <= 1

  test "unbounded brush dimensions are safe in the public renderer":
    let b = tb().tweak((it.size = F32Max; it.dualBrush.enabled = true; it.dualBrush.size = Inf))
    let ctx = newBrushContext(b)
    check ctx.brush.size == MaxBrushSize
    check ctx.brush.dualBrush.size == 0.5
    check dabsOf(b, [strokePoint(10, 10, 1)]).allIt(it.radius <= MaxBrushSize / 2)

  test "texture modulates coverage":
    let tile = grayTileFromFn(8, 8, proc(x, y: uint32): float32 = (if x < 4: 1'f32 else: 0'f32))
    proc tex(mode: MaskMode; depth: float32; eachTip: bool): BrushSettings =
      tb().tweak((it.size = 40; it.texture = defaultTexture(); it.texture.enabled = true;
                  it.texture.pattern = Pattern(kind: pkTile, tile: tile); it.texture.mode = mode;
                  it.texture.depth = depth; it.texture.eachTip = eachTip))
    for eachTip in [false, true]:
      let s = paint(tex(mmMultiply, 1, eachTip), line(20, 80, 30))
      check s.rgba(46, 30)[3] < 0.1
      check s.rgba(41, 30)[3] > 0.9
      let v = paint(tex(mmMultiply, 0.5, eachTip), line(20, 80, 30)).rgba(46, 30)[3]
      if eachTip: check v > 0.5 and v < 1
      else: check abs(v - 0.5) < 0.02
    var b = tex(mmMultiply, 1, false)
    b.texture.invert = true
    var s = paint(b, line(20, 80, 30))
    check s.rgba(46, 30)[3] > 0.9 and s.rgba(41, 30)[3] < 0.1
    b.texture.invert = false
    b.texture.brightness = 1
    s = paint(b, line(20, 80, 30))
    check s.rgba(46, 30)[3] > 0.9
    for m in MaskMode:
      for t in [0'f32, 0.3, 1]:
        check maskCombine(m, 0, t, 1) == 0
        let v = maskCombine(m, 0.7, t, 0.8)
        check v >= 0 and v <= 1
      check maskCombine(m, 0.6, 1, 1) >= 0.6 - 1e-5 or m == mmOverlay
    b = tex(mmMultiply, 0.8, true)
    b.texture.depthJitter = Dynamic(jitter: 1, minimum: 0.25, fadeSteps: 25)
    let d = dabsOf(b, line(0, 200, 0))
    check d.allIt(it.depth <= 0.8 + 1e-6 and it.depth >= 0.8 * 0.25 - 1e-6)
    check d.anyIt(it.depth < 0.6)

  test "the dual brush intersects":
    let b = tb().tweak((it.size = 30; it.dualBrush.enabled = true; it.dualBrush.size = 6; it.dualBrush.spacing = 5;
                        it.dualBrush.hardness = 1))
    var s = paint(b, line(20, 200, 30))
    check s.rgba(20, 30)[3] > 0.9 and s.rgba(50, 30)[3] > 0.9
    check s.rgba(35, 30)[3] < 0.01
    check s.rgba(20, 50)[3] == 0
    s = paint(b.tweak((it.dualBrush = defaultDualBrush())), line(20, 200, 30))
    check s.rgba(35, 30)[3] > 0.9

  test "wet edges shape the stroke, not each dab":
    let s = paint(tb().tweak((it.size = 40; it.hardness = 0.8; it.wetEdges = true)), line(20, 220, 50))
    let centre = toSeq(60 ..< 180).mapIt(s.rgba(it, 50)[3])
    let (lo, hi) = (centre.min, centre.max)
    check hi - lo < 0.02
    check hi < 0.6
    for x in [80, 120, 160]: check s.rgba(x, 66)[3] > hi + 0.15

  test "wet edges and noise":
    let b = tb().tweak((it.size = 40; it.hardness = 0; it.wetEdges = true))
    let centre = paint(b, line(20, 120, 50)).rgba(70, 50)[3]
    check centre < 0.6
    let single = paint(b, [strokePoint(70, 50, 1)]).rgba(70, 50)[3]
    check abs(centre - single) < 0.05
    let d = paint(b.tweak((it.hardness = 1)), [strokePoint(50, 50, 1)])
    check d.rgba(68, 50)[3] > d.rgba(50, 50)[3] + 0.2
    let soft = tb().tweak((it.size = 40; it.hardness = 0))
    let a = paint(soft, [strokePoint(50, 50, 1)])
    let n = paint(soft.tweak((it.noise = true)), [strokePoint(50, 50, 1)])
    proc extreme(s: Surface): int =
      for y in 30 ..< 70:
        for x in 30 ..< 70:
          let v = s.rgba(x, y)[3]
          if v < 0.15 or v > 0.85: inc result
    check extreme(n) > extreme(a) + 100

suite "paint: smoothing":
  test "smoothing modes":
    let zig = toSeq(0 ..< 40).mapIt(strokePoint(float64(it) * 5, (if it mod 2 == 0: 0.0 else: 6.0), 1))
    proc dev(pts: openArray[StrokePoint]): float64 =
      for p in pts: result += abs(p.y - 3)
      result /= float64(pts.len)
    let cfg = defaultSmoothing().tweak((it.amount = 0.8))
    let sm = smoothPoints(cfg, 1, zig)
    check dev(sm[5 ..< sm.len - 1]) < dev(zig) * 0.5
    check (sm[^1].x, sm[^1].y) == (zig[^1].x, zig[^1].y)
    let no = smoothPoints(cfg.tweak((it.catchUpOnEnd = false)), 1, zig)
    check no[^1].x < zig[^1].x - 1
    let ps = defaultSmoothing().tweak((it.amount = 0.2; it.pulledString = true; it.catchUpOnEnd = false))
    let jitter = toSeq(0 ..< 20).mapIt(strokePoint((if it mod 2 == 0: 0.0 else: 5.0), 0, 1))
    check smoothPoints(ps, 1, jitter).len == 1
    var far = smoothPoints(ps, 1, [strokePoint(0, 0, 1), strokePoint(100, 0, 1)])
    check abs(far[^1].x - 80) < 1e-4
    far = smoothPoints(ps, 2, [strokePoint(0, 0, 1), strokePoint(100, 0, 1)])
    check abs(far[^1].x - 90) < 1e-4
    let a = strokePoint(0, 0, 1)
    var b = a
    b.x = 100
    b.time = 16
    var c = b
    c.time = 500
    for (catchUp, ok) in [(true, proc(x: float64): bool = x > 99), (false, proc(x: float64): bool = x < 70)]:
      var s = newSmoother(defaultSmoothing().tweak((it.amount = 0.8; it.catchUp = catchUp; it.catchUpOnEnd = false)), 1)
      var outp: seq[StrokePoint]
      for p in [a, b, c]: s.push(p, outp)
      check ok(outp[^1].x)

  test "the smoothing string runs from the lagging brush to the pointer":
    let cfg = defaultSmoothing().tweak((it.amount = 0.8))
    var s = newSmoother(cfg, 1)
    var outp: seq[StrokePoint]
    check s.smoothingString.isNone
    s.push(strokePoint(0, 0, 1), outp)
    check s.smoothingString.isNone
    s.push(strokePoint(100, 0, 1), outp)
    let (br, pointer) = s.smoothingString.get
    check br[0] > 0 and br[0] < 100
    check pointer == [100.0, 0.0]
    var off = newSmoother(cfg.tweak((it.amount = 0)), 1)
    off.push(strokePoint(0, 0, 1), outp)
    off.push(strokePoint(100, 0, 1), outp)
    check off.smoothingString.isNone

  test "smoothing pulls by time, not by how often the device reports":
    let cfg = defaultSmoothing().tweak((it.amount = 0.8; it.catchUp = true; it.catchUpOnEnd = false))
    proc run(interval: float64): float64 =
      var s = newSmoother(cfg, 1)
      var outp: seq[StrokePoint]
      s.push(strokePoint(0, 0, 1), outp)
      var t = 0.0
      while t < 32:
        t += interval
        var p = strokePoint(100, 0, 1)
        p.time = t
        s.push(p, outp)
      outp[^1].x
    let (fast, slow) = (run(1), run(16))
    check abs(slow - 36) < 1e-4
    check abs(fast - slow) < 1e-4
    var s = newSmoother(cfg, 1)
    var outp: seq[StrokePoint]
    for x in [0.0, 100.0]: s.push(strokePoint(x, 0, 1), outp)
    check abs(outp[^1].x - 20) < 1e-4

  test "without catch-up the brush stops while the pointer pauses":
    var s = newSmoother(defaultSmoothing().tweak((it.amount = 0.8; it.catchUp = false; it.catchUpOnEnd = false)), 1)
    var outp: seq[StrokePoint]
    s.push(strokePoint(0, 0, 1), outp)
    var p = strokePoint(100, 0, 1)
    p.time = 16
    s.push(p, outp)
    let n = outp.len
    for t in [32.0, 48.0, 64.0]:
      p.time = t
      s.push(p, outp)
    check outp.len == n

suite "paint: pose, chunks, formats, presets":
  test "pose overrides stylus values":
    let b = tb().tweak((it.size = 40; it.pose.enabled = true; it.pose.pressure = 0.5; it.pose.overridePressure = true;
                        it.shapeDynamics.enabled = true; it.shapeDynamics.size = controlled(ctlPenPressure)))
    check dabsOf(b, line(0, 50, 0)).allIt(abs(it.radius - 10) < 1e-4)

  test "chunked rendering matches one shot":
    let b = tb().tweak((it.size = 24; it.seed = 9; it.shapeDynamics.enabled = true; it.shapeDynamics.size = jitterDyn(0.5);
                        it.shapeDynamics.angle = jitterDyn(1);
                        it.scattering = Scattering(enabled: true, scatter: jitterDyn(1), bothAxes: true, count: 2,
                                                   countJitter: defaultDynamic());
                        it.texture.enabled = true; it.texture.depth = 0.5;
                        it.colorDynamics.enabled = true; it.colorDynamics.hueJitter = 0.5; it.color = [1'f32, 0, 0, 1]))
    let pts = toSeq(0 ..< 60).mapIt(strokePoint(20 + float64(it) * 4, 60 + sin(float64(it) * 0.3) * 30, 1))
    var one = newSurface(Rgba8)
    let d1 = renderStroke(one, b, pts, none(Surface), false, 1)
    var inc = newSurface(Rgba8)
    let pre = inc
    var r = newStrokeRenderer(b, some(inc.format), 1)
    var i = 0
    while i < pts.len:
      r.push(pts[i ..< min(i + 7, pts.len)])
      discard r.composite(pre, inc, none(Surface), false, false)
      i += 7
    r.finish()
    discard r.composite(pre, inc, none(Surface), false, false)
    check d1 == r.bounds
    var same = true
    for y in d1.y0 ..< d1.y1:
      for x in d1.x0 ..< d1.x1:
        if one.pixel(x, y) != inc.pixel(x, y): same = false
    check same

  test "paints every depth and model":
    let b = tb().tweak((it.size = 8; it.color = [1'f32, 0, 0, 1]))
    let fancy = b.tweak((it.shapeDynamics.enabled = true; it.shapeDynamics.size = jitterDyn(0.3);
                         it.colorDynamics.enabled = true; it.colorDynamics.brightnessJitter = 0.1;
                         it.texture.enabled = true; it.texture.depth = 0.2))
    for mode in [cmRgb, cmGrayscale, cmCmyk]:
      for sample in [stU8, stU16, stF32]:
        for alpha in [true, false]:
          let fmt = pixelFormat(mode, sample, alpha)
          var s = surfaceWithDefault(fmt, fromRgba(fmt, [1'f32, 1, 1, 1]))
          for br in [b, fancy]: discard renderStroke(s, br, line(5, 40, 5), none(Surface), false, 1)
          let c = s.rgba(20, 5)
          let expect = toRgba(fmt, fromRgba(fmt, [1'f32, 0, 0, 1]))
          check abs(c[0] - expect[0]) < 0.15 and abs(c[1] - expect[1]) < 0.12
          check s.pixel(20, 30) == s.pixel(5000, 5000)

  test "blend mode and dissolve":
    var s = newSurface(Rgba32)
    s.fillRect(rect(0, 0, 100, 20), [0.5'f32, 0.5, 0.5, 1])
    discard renderStroke(s, tb().tweak((it.size = 10; it.color = [0.5'f32, 0.5, 0.5, 1]; it.mode = bmMultiply)),
                         line(10, 90, 10), none(Surface), false, 1)
    check abs(s.rgba(50, 10)[0] - 0.25) < 1e-4
    var t = newSurface(Rgba32)
    discard renderStroke(t, tb().tweak((it.size = 30; it.opacity = 0.5; it.mode = bmDissolve)), line(20, 80, 20),
                         none(Surface), false, 1)
    let vals = toSeq(30 ..< 70).mapIt(t.rgba(it, 20)[3])
    check vals.allIt(it == 0 or it == 1) and 0'f32 in vals and 1'f32 in vals

  test "presets render and round-trip":
    let ps = builtin()
    check ps.len >= 12
    for p in ps:
      let back = fromJson(parseJson($toJ(p)), BrushPreset)
      check back == p
      var s = newSurface(Rgba8)
      var pts: seq[StrokePoint]
      for i in 0 ..< 20:
        var q = strokePoint(40 + float64(i) * 8, 60, 0.8)
        q.time = float64(i) * 16
        pts.add q
      let dmg = renderStroke(s, p.brush, pts, none(Surface), false, 1)
      check not dmg.isEmpty
      check alphaSum(s, dmg) > 10
    let b = brushFromJson(parseJson("""{"size": 50, "scattering": {"enabled": true, "scatter": {"jitter": 2}}, "pressure_size": false}"""))
    check b.size == 50
    check b.scattering.enabled and b.scattering.count == 1 and not b.pressureSize
    check b.hardness == 1

  test "protect texture keeps the pattern":
    let ps = builtin()
    let chalk = ps.find("chalk").get.brush
    let canvas = ps.find("Canvas Texture").get.brush
    let cur = chalk.tweak((it.protectTexture = true))
    let next = canvas.withProtectedTexture(cur)
    check next.texture.pattern == chalk.texture.pattern
    check next.texture.mode == canvas.texture.mode
    check canvas.withProtectedTexture(chalk).texture.pattern == canvas.texture.pattern

  test "the tail preview shows the stroke as finishing it would":
    let fmt = pixelFormat(cmRgb, stU16, true)
    let b = tb().tweak((it.size = 9; it.smoothing.amount = 0.7))
    let pts = toSeq(0 ..< 12).mapIt(strokePoint(5 + float64(it) * 9, 20 + float64(it mod 3) * 7, 1))
    let pre = newSurface(fmt)
    var live = pre
    var r = newStrokeRenderer(b, some(fmt), 1)
    r.push(pts)
    discard r.composite(pre, live, none(Surface), false, false)
    var tail = r.tailPreview
    check tail.isSome
    discard tail.get.composite(pre, live, none(Surface), false, false)
    var done = pre
    discard renderStroke(done, b, pts, none(Surface), false, 1)
    var same = true
    for y in 0 ..< 60:
      for x in 0 ..< 120:
        if live.rgba(x, y) != done.rgba(x, y): same = false
    check same
    var r2 = newStrokeRenderer(tb(), some(fmt), 1)
    r2.push(pts)
    check r2.tailPreview.isNone

suite "paint: tilt, projection, speed spacing, locks":
  test "tilt scale squashes the tip height with pen tilt":
    proc mk(scale: float32): BrushSettings =
      tb().tweak((it.size = 40; it.shapeDynamics.enabled = true;
                  it.shapeDynamics.size = Dynamic(control: ctlPenTilt, minimum: 1, fadeSteps: 25);
                  it.shapeDynamics.tiltScale = scale))
    let upright = [pen(50, 50, 0, 0, 0)]
    let tilted = [pen(50, 50, 45, 0, 0)]
    check dabsOf(mk(0), tilted)[0].roundness == 1
    check dabsOf(mk(2), upright)[0].roundness == 1
    check abs(dabsOf(mk(1), tilted)[0].roundness - 0.75) < 1e-4
    let d2 = dabsOf(mk(2), tilted)[0]
    check abs(d2.roundness - 0.5) < 1e-4
    let (w, h) = extents(mk(2), d2)
    check w >= 38 and abs(h - 20) <= 2
    var b = mk(2)
    b.shapeDynamics.size.control = ctlPenPressure
    check dabsOf(b, tilted)[0].roundness == 1
    let a = paint(mk(0), [pen(20, 40, 60, 0, 0), pen(120, 40, 60, 0, 0)])
    let c = paint(mk(2), [pen(20, 40, 60, 0, 0), pen(120, 40, 60, 0, 0)])
    check alphaSum(c, rect(0, 0, 160, 80)) < alphaSum(a, rect(0, 0, 160, 80)) * 0.75

  test "brush projection foreshortens along the tilt and turns with rotation":
    proc mk(on: bool): BrushSettings = tb().tweak((it.size = 40; it.shapeDynamics.enabled = true; it.shapeDynamics.brushProjection = on))
    let pts = [pen(50, 50, 60, 0, 0)]
    var d = dabsOf(mk(true), pts)[0]
    check abs(d.projScale - 0.5) < 1e-4 and abs(d.projAngle) < 1e-5
    var (w, h) = extents(mk(true), d)
    check abs(w - 20) <= 2 and h >= 38
    d = dabsOf(mk(true), [pen(50, 50, 0, 60, 0)])[0]
    (w, h) = extents(mk(true), d)
    check w >= 38 and abs(h - 20) <= 2
    d = dabsOf(mk(false), pts)[0]
    check (d.projScale, d.angle) == (1'f32, 0'f32)
    check extents(mk(false), d) == (40, 40)
    var b = mk(true)
    b.roundness = 0.5
    d = dabsOf(b, [pen(50, 50, 0, 0, 90)])[0]
    check abs(toDegrees(d.angle) - 90) < 1e-3
    (w, h) = extents(b, d)
    check w < h
    let ctx = newBrushContext(mk(true))
    var dd = roundDab(point(50, 50), 20, 1)
    dd.projScale = 0.3
    let r = ctx.dabRect(dd, true)
    var a1, c1: seq[float32]
    ctx.rasterize(dd, true, r, a1)
    dd.projScale = 1
    ctx.rasterize(dd, true, r, c1)
    check a1 == c1

  test "spacing off spaces dabs by pointer speed":
    var b = tb().tweak((it.size = 20; it.spacingEnabled = false))
    proc timed(ms: float64): seq[StrokePoint] =
      var z = strokePoint(100, 10, 1)
      z.time = ms
      @[strokePoint(0, 10, 1), z]
    check (dabsOf(b, timed(400)).len, dabsOf(b, timed(40)).len) == (51, 6)
    check dabsOf(b, timed(1e9)).len <= 201
    let pts = [strokePoint(0, 10, 1), strokePoint(90, 10, 1), strokePoint(100, 10, 1)]
    let checked = b.tweak((it.spacingEnabled = true))
    check dabsOf(b, pts).len == dabsOf(checked, pts).len
    check dabsOf(b, pts).len > 3
    b.spacingEnabled = true
    check dabsOf(b, timed(400)).len == dabsOf(b, timed(40)).len
    b.spacingEnabled = false
    var tp: seq[StrokePoint]
    for i in 0 ..< 20:
      var q = strokePoint(float64(i) * 7, 10, 1)
      q.time = float64(i) * 13
      tp.add q
    let one = dabsOf(b, tp)
    var g = newDabGenerator(b, 1)
    var outp, dual: seq[Dab]
    var i = 0
    while i < tp.len:
      g.push(tp[i ..< min(i + 3, tp.len)], outp, dual)
      i += 3
    g.finish(outp, dual)
    check one == outp

  test "locked sections survive picking a preset":
    let chalk = builtin().find("chalk").get.brush
    var cur = tb().tweak((it.scattering.enabled = true; it.scattering.count = 5; it.transfer.enabled = true;
                          it.transfer.wetness = jitterDyn(0.4); it.noise = true))
    var next = chalk.pickedOver(cur)
    check next.scattering == chalk.scattering
    check next.noise == chalk.noise
    cur.locks = SectionLocks(scattering: true, transfer: true, noise: true)
    next = chalk.pickedOver(cur)
    check next.scattering == cur.scattering
    check next.transfer == cur.transfer
    check next.noise
    check next.locks == cur.locks
    check next.texture == chalk.texture
    check next.tip == chalk.tip

  test "transfer wetness and mix vary per dab":
    let b = tb().tweak((it.transfer.enabled = true; it.transfer.wetness = controlled(ctlPenPressure);
                        it.transfer.mix = Dynamic(jitter: 1, minimum: 0.25, fadeSteps: 25)))
    let pts = [strokePoint(0, 0, 0.3), strokePoint(60, 0, 0.3)]
    var d = dabsOf(b, pts)
    check d.allIt(abs(it.wet - 0.3) < 1e-5)
    check d.allIt(it.mix >= 0.25 and it.mix <= 1) and d.anyIt(it.mix < 0.9)
    d = dabsOf(tb(), pts)
    check d.allIt(it.wet == 1 and it.mix == 1)
    let full = b.tweak((it.spacingEnabled = false; it.shapeDynamics = defaultShapeDynamics();
                        it.shapeDynamics.tiltScale = 1.5; it.shapeDynamics.brushProjection = true;
                        it.locks = SectionLocks(texture: true); it.mixer = defaultMixerSettings();
                        it.mixer.wet = 0.9; it.mixer.sampleAllLayers = true))
    check brushFromJson(toJ(full)) == full
    let old = brushFromJson(parseJson("""{"size": 12, "shapeDynamics": {"enabled": true}, "transfer": {"enabled": true}}"""))
    check old.spacingEnabled and not old.shapeDynamics.brushProjection and old.shapeDynamics.tiltScale == 0 and
      old.locks == SectionLocks()
    check old.mixer == defaultMixerSettings()

  test "a huge scatter is capped and renders at every depth":
    let pts = line(10, 40, 10)
    proc with(j: float32): BrushSettings =
      tb().tweak((it.size = 20; it.scattering = Scattering(enabled: true, scatter: jitterDyn(j), bothAxes: true, count: 4,
                                                          countJitter: defaultDynamic());
                  it.dualBrush.enabled = true; it.dualBrush.size = 6; it.dualBrush.scatter = j; it.dualBrush.bothAxes = true))
    let capped = allDabs(with(MaxScatter), pts)
    proc near(ds: seq[Dab]; r: float64): bool = ds.allIt(abs(it.center.x - 25) <= 15 + r and abs(it.center.y - 10) <= r)
    check near(capped[0], float64(MaxScatter) * 10) and near(capped[1], float64(MaxScatter) * 3)
    for j in [MaxScatter + 0.01'f32, 1e6, 1e38, F32Max, Inf]:
      let b = with(j)
      check allDabs(b, pts) == capped
      let ctx = newBrushContext(b)
      for d in capped[0]:
        let r = ctx.dabRect(d, false)
        check not r.isEmpty and r.width <= 24 and r.height <= 24
      for sample in [stU8, stU16, stF32]:
        var s = newSurface(pixelFormat(cmRgb, sample, true))
        let damage = renderStroke(s, b, pts, none(Surface), false, 1)
        check not damage.isEmpty and damage.x0 > -200 and damage.x1 < 250
    let nothing = allDabs(with(0), pts)
    check allDabs(with(NaN), pts) == nothing
    check nothing[0].allIt(it.center.y == 10)

  test "an ordinary scatter is unchanged by the cap":
    let b = tb().tweak((it.size = 20; it.scattering = Scattering(enabled: true, scatter: jitterDyn(2), bothAxes: true, count: 3,
                                                                 countJitter: defaultDynamic());
                        it.dualBrush.enabled = true; it.dualBrush.size = 6; it.dualBrush.scatter = 1.5; it.dualBrush.bothAxes = true))
    check b.boundedForRender == b
    let (ds, duals) = allDabs(b, line(10, 200, 50))
    check ds.anyIt(abs(it.center.y - 50) > 10) and ds.allIt(abs(it.center.y - 50) <= 20)
    check duals.anyIt(abs(it.center.y - 50) > 1) and duals.allIt(abs(it.center.y - 50) <= 4.5)

  test "the dab rect of a far or non-finite centre does not overflow":
    let ctx = newBrushContext(tb())
    for c in [Inf, NegInf, NaN, 1e300, -1e300, float64(high(int32)), float64(low(int32))]:
      let d = roundDab(point(c, c), 10, 1)
      for r in [ctx.dabRect(d, false), ctx.dabRect(d, true), rectAround(d, d.radius)]:
        check r.x0 <= r.x1 and r.y0 <= r.y1
    let huge = roundDab(point(5, 5), Inf, 1)
    let r = rectAround(huge, huge.radius)
    check r.x0 <= r.x1 and r.y0 <= r.y1

  test "a soft stroke keeps most of its tip's softness":
    let b = defaultBrush().tweak((it.size = 100; it.hardness = 0; it.pressureSize = false))
    let s = softEdgeWidth(crossSection(b, line(0, 400, 100.5), 200))
    let d = softEdgeWidth(crossSection(b, [strokePoint(200.5, 100.5, 1)], 200))
    check d >= 20 and s * 100 >= d * 65

  test "a tiny dab's coverage does not depend on where it lands in a pixel":
    proc total(x, y: float64): float32 =
      var s = newSurface(Rgba32)
      discard renderStroke(s, defaultBrush().tweak((it.size = 1; it.hardness = 1; it.pressureSize = false)),
                           [strokePoint(x, y, 1)], none(Surface), false, 1)
      for py in 0 ..< 10:
        for px in 0 ..< 10: result += s.rgba(px, py)[3]
    let sums = [(5.5, 5.5), (5.75, 5.5), (5.0, 5.0), (5.3, 5.8)].mapIt(total(it[0], it[1]))
    check sums.max - sums.min < 0.03 * sums.max

  test "the tip falls off from the hard core to zero at the edge":
    check tipFalloff(0, 10, 0) == 1
    check tipFalloff(10, 10, 0) == 0
    check tipFalloff(4, 10, 0.5) == 1
    let v = toSeq(0 .. 20).mapIt(tipFalloff(float32(it) * 0.5, 10, 0))
    for i in 1 ..< v.len: check v[i] <= v[i - 1]
    check abs(tipFalloff(5, 10, 0) - pow(0.75'f32, 4)) < 1e-6

  test "the half coverage radius is the tip's 50 % contour":
    for h in [0'f32, 0.25, 0.5, 0.75, 0.99]:
      let d = halfCoverageRadius(10.5, h)
      check abs(tipFalloff(d, 10.5, h) - 0.5) < 1e-3
    check halfCoverageRadius(10.5, 1) == 10.5
    check classify(halfCoverageRadius(10.5, NaN)) notin {fcNan, fcInf, fcNegInf}

  test "an 8-bit soft stroke is dithered instead of banded":
    check worstRowError(softLowFlow(Rgba8), softLowFlow(Rgba32)) < 0.15

  test "dithering never moves an exact colour or an untouched pixel":
    var s = surfaceWithDefault(Rgba8, [1'f32, 1, 1, 1])
    discard renderStroke(s, defaultBrush().tweak((it.size = 20; it.hardness = 1; it.color = [0.2'f32, 0.4, 0.6, 1];
                                                  it.pressureSize = false)), line(10, 90, 50), none(Surface), false, 1)
    proc q(v: float32): int = int(round(v * 255))
    for x in 15 ..< 85:
      let p = s.rgba(x, 50)
      check (q(p[0]), q(p[1]), q(p[2])) == (q(0.2), q(0.4), q(0.6))
    check s.rgba(50, 5) == [1'f32, 1, 1, 1]

suite "paint: computed-tip fast path (render_reference.rs)":
  test "computed tips match the scalar loop bit for bit":
    for aliased in [false, true]:
      for hardness in [-0.5'f32, 0, 0.1, 0.5, 0.9999, 1, 1.5]:
        let ctx = newBrushContext(defaultBrush().tweak((it.aliased = aliased; it.hardness = hardness)))
        for radius in [0.5'f32, 1, 2.99, 3, 3.01, 15, 31.5]:
          for roundness in [0'f32, 0.1, 0.4, 1]:
            for alpha in [-0.5'f32, -0'f32, 0, 0.37, 1]:
              var d = roundDab(point(-31.25, 63.75), radius, alpha)
              d.roundness = roundness
              d.flipX = true
              d.flipY = true
              compare(ctx, d, false, ctx.dabRect(d, false), false)

  test "large corner dabs keep off-canvas coverage":
    for (hardness, aliased) in [(0'f32, false), (1'f32, false), (1'f32, true)]:
      let ctx = newBrushContext(defaultBrush().tweak((it.hardness = hardness; it.aliased = aliased)))
      let d = roundDab(point(-1050.25, -1050.75), 1500, 0.73)
      let r = ctx.dabRect(d, false)
      compare(ctx, d, false, r, false)
      var coverage: seq[float32]
      ctx.rasterize(d, false, r, coverage)
      var offCanvas = false
      for i, v in coverage:
        if v > 0 and r.x0 + i mod r.width < 0:
          offCanvas = true
          break
      check offCanvas
      compare(ctx, d, false, rect(0, 0, 32, 32), false)
      compare(ctx, d, false, rect(0, 0, 0, 0), false)

  test "feature and exceptional dabs match the scalar loop":
    let tip = grayTileFromFn(8, 5, proc(x, y: uint32): float32 = float32((x + y) mod 7) / 6)
    for feature in 0 ..< 7:
      var b = defaultBrush().tweak((it.hardness = 0.25))
      case feature
      of 0: b.noise = true
      of 1: b.wetEdges = true
      of 2:
        b.aliased = true
        b.square = true
      of 3: b.tip = TipShape(kind: tkSampled, sampled: tip)
      of 4:
        b.texture.enabled = true
        b.texture.eachTip = true
        b.texture.pattern = Pattern(kind: pkTile, tile: tip)
      of 5: b.dualBrush.tip = TipShape(kind: tkSampled, sampled: tip)
      else: discard
      let ctx = newBrushContext(b)
      for dual in [false, true]:
        for withDepth in [false, true]:
          for (angle, projScale) in [(0'f32, 1'f32), (0.7'f32, 1'f32), (0'f32, 0.6'f32)]:
            var d = roundDab(point(63.5, -0.25), 15, 0.47)
            d.angle = angle
            d.roundness = 0.4
            d.projScale = projScale
            compare(ctx, d, dual, ctx.dabRect(d, dual), withDepth)
    let ctx = newBrushContext(defaultBrush())
    for value in [NaN.float32, cast[float32](0x7f80_0123'u32), Inf.float32, NegInf.float32]:
      for field in 0 ..< 4:
        var d = roundDab(point(0, 0), 15, 1)
        case field
        of 0: d.alpha = value
        of 1: d.angle = value
        of 2: d.radius = value
        else: d.roundness = value
        compare(ctx, d, false, rect(-4, -4, 4, 4), false)
      let hctx = newBrushContext(defaultBrush().tweak((it.hardness = value)))
      compare(hctx, roundDab(point(0, 0), 15, 1), false, rect(-16, -16, 16, 16), false)

  test "computed tips match at pixel and float boundaries":
    for aliased in [false, true]:
      for center in [point(0, -0.0), point(0.5, 0.5), point(255.9999, -256.0001), point(16_777_216.25, -16_777_216.75)]:
        for radius in [3'f32, 3.5, 15.25, 113.75]:
          for hardness in [0'f32, 0.6, 1]:
            let ctx = newBrushContext(defaultBrush().tweak((it.hardness = hardness; it.aliased = aliased)))
            var d = roundDab(center, radius, 0.137)
            d.roundness = 0.375
            compare(ctx, d, false, ctx.dabRect(d, false), false)
