## Tests for photocraft/geom and photocraft/geom/warp (ported from crates/geom).
import std/[unittest, math, options]
import photocraft/geom, photocraft/geom/warp

suite "geom":
  test "rect basics":
    let r = rectXywh(10, 20, 30, 40)
    check r.width == 30
    check r.height == 40
    check r.contains(10, 20)
    check not r.contains(40, 20)
    check rect(5, 5, 5, 10).isEmpty

  test "intersect and union":
    let a = rect(0, 0, 10, 10)
    let b = rect(5, 5, 15, 15)
    check a.intersect(b) == rect(5, 5, 10, 10)
    check a.union(b) == rect(0, 0, 15, 15)
    check a.intersect(rect(20, 20, 30, 30)) == EmptyRect
    check EmptyRect.union(a) == a

  test "tile coords handle negatives":
    check tileContaining(0, 0) == tileCoord(0, 0)
    check tileContaining(255, 255) == tileCoord(0, 0)
    check tileContaining(256, 0) == tileCoord(1, 0)
    check tileContaining(-1, -1) == tileCoord(-1, -1)
    check tileContaining(-256, -257) == tileCoord(-1, -2)

  test "rect tiles enumeration":
    check rect(-10, 0, 300, 10).tileList == @[tileCoord(-1, 0), tileCoord(0, 0), tileCoord(1, 0)]
    check EmptyRect.tileList.len == 0
    check rect(0, 0, 256, 256).tileList.len == 1
    check rect(0, 0, 257, 257).tileList.len == 4

  test "affine inverse roundtrip":
    let t = translateAffine(5.0, -3.0).then(rotateAffine(0.7)).then(scaleAffine(2.5))
    let inv = t.inverse.get
    let p = point(12.0, -7.5)
    let q = inv.apply(t.apply(p))
    check abs(q.x - p.x) < 1e-9 and abs(q.y - p.y) < 1e-9
    check scaleAffine(0.0).inverse.isNone

  test "affine then order":
    let t = translateAffine(10.0, 0.0).then(scaleAffine(2.0))
    check t.apply(point(1.0, 0.0)) == point(22.0, 0.0)

  test "translate and inflate saturate on overflow":
    let r = rect(int(int32.high) - 10, int(int32.low) + 10, int(int32.high), int(int32.low) + 20)
    let t = r.translate(100, -100)
    check t.x0 == int(int32.high)
    check t.y0 == int(int32.low)
    let grown = rect(int(int32.high) - 5, int(int32.low) + 5, int(int32.high) - 1, int(int32.low) + 10).inflate(10)
    check grown.x1 == int(int32.high)
    check grown.y0 == int(int32.low)

const B = [10.0, 20.0, 110.0, 70.0]

proc tangentDefect(m: BezierMesh): float64 =
  let nx = m.nx
  for a in 1 ..< m.us.len - 1:
    let hl = m.us[a] - m.us[a - 1]
    let hr = m.us[a + 1] - m.us[a]
    for j in 0 ..< m.ny:
      let l = m.points[j * nx + 3 * a - 1]
      let c = m.points[j * nx + 3 * a]
      let r = m.points[j * nx + 3 * a + 1]
      for k in 0..1: result = max(result, abs((c[k] - l[k]) / hl - (r[k] - c[k]) / hr))
  for b in 1 ..< m.vs.len - 1:
    let hl = m.vs[b] - m.vs[b - 1]
    let hr = m.vs[b + 1] - m.vs[b]
    for i in 0 ..< nx:
      let l = m.points[(3 * b - 1) * nx + i]
      let c = m.points[3 * b * nx + i]
      let r = m.points[(3 * b + 1) * nx + i]
      for k in 0..1: result = max(result, abs((c[k] - l[k]) / hl - (r[k] - c[k]) / hr))

proc foldFraction(m: BezierMesh): float64 =
  const n = 80
  const h = 1e-4
  var bad = 0
  for j in 0 ..< n:
    for i in 0 ..< n:
      let s = (float64(i) + 0.5) / n
      let t = (float64(j) + 0.5) / n
      let q = m.eval(s, t)
      let qs = m.eval(s + h, t)
      let qt = m.eval(s, t + h)
      if (qs[0] - q[0]) * (qt[1] - q[1]) - (qt[0] - q[0]) * (qs[1] - q[1]) <= 0.0: inc bad
  float64(bad) / float64(n * n)

proc close(a, b: (float64, float64); eps: float64): bool = abs(a[0] - b[0]) < eps and abs(a[1] - b[1]) < eps

suite "warp":
  test "dragging an anchor or handle keeps the surface smooth":
    var base = identityMesh([0.0, 0.0, 300.0, 300.0], 1, 1)
    for s in [0.3, 0.55, 0.9]:
      check base.splitU(s)
      check base.splitV(s)
    let nx = base.nx
    for (i, j) in [(6, 6), (7, 6), (5, 6), (6, 7), (6, 5), (3, 9), (9, 3), (0, 6), (6, 0), (4, 3), (3, 4), (12, 12)]:
      var m = base
      m.dragPoint(base.points, j * nx + i, [23.0, -31.0])
      check m.points[j * nx + i] == [base.points[j * nx + i][0] + 23.0, base.points[j * nx + i][1] - 31.0]
      check tangentDefect(m) < 1e-6

  test "a pull within a quarter cell never folds":
    let base = identityMesh([0.0, 0.0, 300.0, 300.0], 4, 4)
    for (s, t) in [(0.42, 0.42), (0.75, 0.2), (0.2, 0.2), (0.5, 0.5)]:
      for (dx, dy) in [(-1.0, 0.0), (0.0, 1.0), (0.7, 0.7)]:
        var m = base
        m.pull(base.points, s, t, [dx * 19.0, dy * 19.0])
        check foldFraction(m) == 0.0

  test "pull on the border bows it and leaves corners":
    let base = identityMesh([0.0, 0.0, 800.0, 1200.0], 1, 1)
    var m = base
    m.pull(base.points, 1.0, 0.5, [170.0, 0.0])
    let at = m.eval(1.0, 0.5)
    check abs(at[0] - 970.0) < 1e-3 and abs(at[1] - 600.0) < 1e-3
    let nx = base.nx
    var moved: seq[int]
    for k in 0 ..< m.points.len:
      if m.points[k] != base.points[k]: moved.add k
    check moved == @[nx + 3, 2 * nx + 3]

  test "pull moves the grabbed point exactly":
    let base = identityMesh(B, 3, 2)
    for (s, t) in [(0.5, 0.5), (0.1, 0.9), (0.4, 0.25), (0.0, 0.5), (1.0, 0.3)]:
      var m = base
      let before = base.eval(s, t)
      m.pull(base.points, s, t, [7.0, -4.5])
      let after = m.eval(s, t)
      check abs(after[0] - before[0] - 7.0) < 1e-4 and abs(after[1] - before[1] + 4.5) < 1e-4

  test "styles parse and round trip names":
    for s in WarpStyle:
      check parseWarpStyle(s.id) == some(s)
      check parseWarpStyle(s.psdName) == some(s)
      check parseWarpStyle(s.label) == some(s)
    check parseWarpStyle("spiral").isNone

  test "presets at bend zero are identity":
    for s in Presets:
      let w = warpPreset(s, 0.0, B)
      check w.isIdentity
      check w.map(33.0, 44.0) == (33.0, 44.0)

  test "identity mesh is exact and fit reproduces affine":
    let m = identityMesh(B, 1, 1)
    check (m.nx, m.ny) == (4, 4)
    let w = warpCustom(m, B)
    check w.isIdentity
    check close(w.map(37.5, 61.25), (37.5, 61.25), 1e-9)
    let f = proc(s, t: float64): P2 = [3.0 + 2.0 * s - t, 1.0 + 0.5 * s + 4.0 * t]
    let m2 = fit(f, @[0.0, 1.0], @[0.0, 0.4, 1.0])
    for (s, t) in [(0.1, 0.9), (0.5, 0.2), (0.77, 0.41)]:
      let p = m2.eval(s, t)
      let e = f(s, t)
      check abs(p[0] - e[0]) < 1e-9 and abs(p[1] - e[1]) < 1e-9

  test "splits are exact and removable":
    var m = identityMesh(B, 1, 1)
    m.points[5] = [50.0, 10.0]
    var before: seq[P2]
    for j in 0..10:
      for i in 0..10: before.add m.eval(float64(i) / 10, float64(j) / 10)
    let orig = m
    check m.splitU(0.3)
    check m.splitV(0.6)
    check not m.splitU(0.3)
    check (m.nx, m.ny) == (7, 7)
    check m.isValid
    var k = 0
    for j in 0..10:
      for i in 0..10:
        let a = m.eval(float64(i) / 10, float64(j) / 10)
        check abs(a[0] - before[k][0]) < 1e-9 and abs(a[1] - before[k][1]) < 1e-9
        inc k
    let (ls, lt) = m.paramAt(m.eval(0.42, 0.77))
    check abs(ls - 0.42) < 1e-6 and abs(lt - 0.77) < 1e-6
    check m.removeSplitU(1)
    check m.removeSplitV(1)
    check m.us == orig.us
    for i in 0 ..< m.points.len:
      check abs(m.points[i][0] - orig.points[i][0]) < 1e-9

  test "presets match photoshop":
    const Square = [128.0, 128.0, 384.0, 384.0]
    const Wide = [128.0, 208.0, 384.0, 304.0]
    const Tall = [208.0, 128.0, 304.0, 384.0]
    type Case = (WarpStyle, float64, float64, float64, bool, array[4, float64], seq[(P2, P2)])
    let cases: seq[Case] = @[
      (wsArc, 50.0, 0.0, 0.0, false, Square, @[([272.5, 155.5], [296.5, 104.5]), ([331.5, 182.5], [424.5, 168.5]), ([259.5, 157.5], [264.5, 104.5])]),
      (wsArc, -100.0, 0.0, 0.0, false, Square, @[([277.5, 354.5], [344.5, 472.5]), ([140.5, 218.5], [40.5, 168.5]), ([172.5, 237.5], [56.5, 264.5])]),
      (wsFlag, 50.0, 0.0, 0.0, false, Square, @[([136.5, 143.5], [136.5, 120.5]), ([136.5, 159.5], [136.5, 136.5])]),
      (wsTwist, -50.0, 0.0, 0.0, false, Square, @[([148.5, 205.5], [152.5, 216.5]), ([197.5, 212.5], [200.5, 232.5])]),
      (wsFish, -100.0, 0.0, 0.0, false, Square, @[([296.5, 355.5], [296.5, 440.5]), ([344.5, 298.5], [344.5, 344.5])]),
      (wsBulge, 50.0, 50.0, 0.0, false, Square, @[([293.5, 147.5], [296.5, 88.5]), ([293.5, 375.5], [296.5, 440.5])]),
      (wsArc, 50.0, 0.0, -50.0, false, Square, @[([181.5, 350.5], [184.5, 328.5]), ([291.5, 372.5], [280.5, 344.5])]),
      (wsArc, 50.0, 0.0, 0.0, true, Square, @[([155.5, 272.5], [104.5, 296.5]), ([182.5, 331.5], [168.5, 424.5])]),
      (wsArc, 50.0, 0.0, 0.0, false, Wide, @[([144.5, 269.6], [120.5, 264.5]), ([271.5, 220.42], [280.5, 168.5])]),
      (wsBulge, 50.0, 0.0, 0.0, false, Tall, @[([249.11, 374.5], [248.5, 392.5])]),
      (wsFlag, 50.0, 0.0, 0.0, true, Wide, @[([137.5, 296.42], [184.5, 296.5])]),
      (wsRise, 50.0, 0.0, 0.0, true, Tall, @[([250.97, 152.5], [344.5, 152.5])]),
      (wsBulge, 60.0, 20.0, -10.0, false, Wide, @[([208.5, 224.15], [200.5, 184.5])]),
      (wsArc, 50.0, 30.0, 0.0, true, Square, @[([331.5, 327.5], [296.5, 360.5])])]
    for (style, bend, hd, vd, vertical, bounds, pairs) in cases:
      var w = warpPreset(style, bend, bounds)
      w.hDistort = hd
      w.vDistort = vd
      w.vertical = vertical
      for (src, want) in pairs:
        let got = w.map(src[0], src[1])
        check hypot(got[0] - want[0], got[1] - want[1]) < 1.5

  test "preset fits closely as mesh and affine composes":
    let w = warpPreset(wsBulge, 40.0, B)
    let m = warpCustom(w.toMesh(4, 4), B)
    for (x, y) in [(20.0, 30.0), (60.0, 45.0), (100.0, 65.0)]:
      check close(w.map(x, y), m.map(x, y), 0.5)
    let t = warpCustom(identityMesh(B, 1, 1), B).thenAffine([1.0, 0.0, 0.0, 1.0, 5.0, -2.0])
    check close(t.map(50.0, 50.0), (55.0, 48.0), 1e-9)
    let ob = warpNone(B).outputBounds
    check abs(ob[0] - 10.0) < 1e-9 and abs(ob[3] - 70.0) < 1e-9

  test "warp json round trip":
    let w = warpCustom(identityMesh(B, 2, 1), B)
    let back = warpFromJson(w.toJson).get
    check back.style == wsCustom
    check back.mesh.get.points == w.mesh.get.points
