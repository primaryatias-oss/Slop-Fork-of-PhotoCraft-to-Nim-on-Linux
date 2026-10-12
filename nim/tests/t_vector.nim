## Vector tests: exact-area coverage, path operations, strokes, dashes, shapes, editing, tracing.
import std/[unittest, math, options]
import photocraft/[doc, vector]

proc area(v: seq[float32]): float64 =
  for x in v: result += float64(x)

suite "vector":
  test "rectangle coverage is exact, also at fractional edges":
    let p = rectPath(2.25, 3.5, 10.5, 4.0)
    let cov = pathCoverage(p, rect(0, 0, 20, 10))
    check abs(area(cov) - 42.0) < 1e-4
    check cov[5 * 20 + 2] == 0.75'f32

  test "ellipse area and path operations":
    let e = ellipsePath(0, 0, 40, 20)
    check abs(area(pathCoverage(e, rect(-2, -2, 44, 24))) - PI * 20 * 10) < 0.5
    var p = rectPath(0, 0, 10, 10)
    p.subpaths.add rectPath(5, 0, 10, 10).subpaths[0].withOp(poSubtract)
    check abs(area(pathCoverage(p, rect(0, 0, 20, 10))) - 50) < 1e-3
    p.subpaths[1].op = poIntersect
    check abs(area(pathCoverage(p, rect(0, 0, 20, 10))) - 50) < 1e-3
    p.subpaths[1].op = poExclude
    check abs(area(pathCoverage(p, rect(0, 0, 20, 10))) - 100) < 1e-3
    p.inverted = true
    check abs(area(pathCoverage(p, rect(0, 0, 20, 10))) - 100) < 1e-3

  test "strokes, caps and dashes":
    let line = newPath(@[polyline([(0.0, 5.0), (20.0, 5.0)])])
    var st = defaultStrokeStyle()
    st.width = 2
    check abs(area(strokeRasterizer(line, st, 0.01).render(rect(-5, 0, 25, 10))) - 40) < 1e-3
    st.cap = lcapSquare
    check abs(area(strokeRasterizer(line, st, 0.01).render(rect(-5, 0, 25, 10))) - 44) < 1e-3
    st.cap = lcapButt
    st.dashes = @[5.0, 5.0]
    check abs(area(strokeRasterizer(line, st, 0.01).render(rect(-5, 0, 25, 10))) - 20) < 1e-3

  test "shape layers render fill and stroke":
    var sh = ShapeLayer(path: roundedRect(2, 2, 20, 10, [3.0, 3, 3, 3]), fill: some(solidFill(rgb(1, 0, 0))))
    sh.stroke = some(defaultShapeStroke())
    let s = renderShape(sh, Rgba8, rect(0, 0, 32, 16))
    check s.contentBounds.width > 0
    var px: array[4, float32]
    s.readPixel(12, 7, px)
    check px[0] > 0.99 and px[3] > 0.99
    check validateShape(sh.path, sh.stroke).isNone

  test "editing and hit testing":
    var p = rectPath(0, 0, 10, 10)
    let nk = p.addAnchor([0, 0], 0.5)
    check nk == [0, 1] and p.subpaths[0].knots[1].anchor == point(5, 0)
    p.moveAnchors([[0, 1]], [0.0, -2.0])
    check p.subpaths[0].knots[1].anchor == point(5, -2)
    let (found, h) = p.hit(point(5.1, -2), 1.0, [])
    check found and h.kind == hitAnchor and h.knot == [0, 1]
    check not p.deleteAnchor([0, 1])
    check p.subpaths[0].knots.len == 4

  test "trace a mask back into a path":
    var mask = newSeq[float32](40 * 40)
    for y in 10 ..< 30:
      for x in 5 ..< 35: mask[y * 40 + x] = 1
    let path = traceMask(mask, rect(0, 0, 40, 40), 0, 1.0)
    check path.subpaths.len == 1
    check abs(area(pathCoverage(path, rect(0, 0, 40, 40))) - 600) < 20
    let (ok, sub) = fitTrace([[0.0, 0.0], [10.0, 0.0], [20.0, 0.0], [20.0, 10.0]], 1.0, false)
    check ok and sub.knots.len >= 2

  test "maps keep straight lines straight under affine maps":
    let p = rectPath(0, 0, 10, 10)
    let m = mapPath(p, proc (q: Point): Point = point(q.x * 2 + 1, q.y), 0.1)
    check abs(area(pathCoverage(m, rect(0, 0, 30, 12))) - 200) < 1e-3
