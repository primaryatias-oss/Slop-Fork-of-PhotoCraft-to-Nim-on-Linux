## Vanishing Point (ports of the Rust unit tests). Bit parity with Rust is checked by
## `tools/algocheck.py` (`tools/algocases/vanishing.json`, see PORTING.md).

import std/[unittest, options, math]
import photocraft/[geom, raster, color]
import photocraft/algo/[vanishing, transform]

type V3 = array[3, float64]
proc sub(a, b: V3): V3 = [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
proc dot(a, b: V3): float64 = a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
proc cross(a, b: V3): V3 = [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]]
proc norm(a: V3): V3 =
  let l = sqrt(dot(a, a))
  [a[0] / l, a[1] / l, a[2] / l]

proc project(f: float64; c: array[2, float64]; p: V3): array[2, float64] = [c[0] + f * p[0] / p[2], c[1] + f * p[1] / p[2]]

proc floorQuad(f: float64; c: array[2, float64]): VpPlane =
  ## A 2 × 1 rectangle on a tilted plane, rotated about the vertical axis too.
  let (rx, ry) = (0.6, 0.4)
  proc rot(p: V3): V3 =
    let (s, co) = (sin(rx), cos(rx))
    let q = [p[0], co * p[1] - s * p[2], s * p[1] + co * p[2]]
    let (s2, c2) = (sin(ry), cos(ry))
    [c2 * q[0] + s2 * q[2], q[1], -s2 * q[0] + c2 * q[2]]
  for i, p in [[-1.0, -0.5, 0.0], [1.0, -0.5, 0.0], [1.0, 0.5, 0.0], [-1.0, 0.5, 0.0]]:
    let r = rot(p)
    result.corners[i] = project(f, c, [r[0], r[1], r[2] + 4.0])

suite "vanishing point":
  test "focal and metric aspect from one plane":
    let c = [400.0, 300.0]
    let pl = floorQuad(500, c)
    let f = pl.focal(c).get
    check abs(f - 500) < 1
    let (lu, lv) = pl.lift(f, c).lengths
    check abs(lu / lv - 2) < 0.01
  test "plane check and corner order":
    let c = [400.0, 300.0]
    check floorQuad(500, c).check(c) == pcValid
    let square = VpPlane(corners: [[10.0, 10.0], [110.0, 10.0], [110.0, 110.0], [10.0, 110.0]])
    check square.check(c) == pcValid
    check VpPlane(corners: [[10.0, 10.0], [110.0, 110.0], [110.0, 10.0], [10.0, 110.0]]).check(c) == pcInvalid
    check VpPlane(corners: [[10.0, 10.0], [110.0, 10.0], [40.0, 40.0], [10.0, 110.0]]).check(c) == pcInvalid
    check VpPlane(corners: [[10.0, 10.0], [300.0, 10.0], [300.0, 40.0], [200.0, 40.0]]).check(c) == pcQuestionable
    check VpPlane(corners: [[NaN, 10.0], [110.0, 10.0], [110.0, 110.0], [10.0, 110.0]]).check(c) == pcInvalid
    check VpPlane(corners: [[110.0, 110.0], [110.0, 10.0], [10.0, 10.0], [10.0, 110.0]]).normalized == square
  test "a torn-off plane is perpendicular and shares the edge":
    let c = [400.0, 300.0]
    var sc = newScene(floorQuad(500, c), c, none(float64), 800)
    let k = sc.tearOff(0, edTop, 90, 1).get
    let p = sc.planes[k]
    check abs(p.corners[3][0] - sc.planes[0].corners[0][0]) < 1e-6 and abs(p.corners[2][0] - sc.planes[0].corners[1][0]) < 1e-6
    let (a, b) = (sc.lifted(0), sc.lifted(k))
    let n1 = norm(cross(sub(a.pts[1], a.pts[0]), sub(a.pts[3], a.pts[0])))
    let n2 = norm(cross(sub(b.pts[1], b.pts[0]), sub(b.pts[3], b.pts[0])))
    check abs(dot(n1, n2)) < 0.02
    check sc.tearOff(9, edTop, 90, 1).isNone and sc.frame(-1).isNone
  test "paste and clone in perspective":
    let c = [400.0, 300.0]
    let sc = newScene(floorQuad(500, c), c, none(float64), 800)
    var img = newSurface(pixelFormat(cmRgb, stU8, true))
    img.fillRect(rect(0, 0, 100, 50), [1'f32, 0, 0, 1])
    let o = paste(sc, 0, img, rect(0, 0, 100, 50), [0.25, 0.25], 0.5).get
    let h = sc.planes[0].homography.get
    let (x, y) = h.apply(0.5, 0.5)
    check o.rgba(int(x), int(y))[3] > 0.9
    let (x2, y2) = h.apply(0.1, 0.1)
    check o.rgba(int(x2), int(y2))[3] < 0.1
    var s = newSurface(pixelFormat(cmRgb, stU8, true))
    s.fillRect(rect(0, 0, 800, 600), [0.2'f32, 0.2, 0.2, 1])
    let (sx, sy) = h.apply(0.5, 0.8)
    s.fillRect(rect(int(sx) - 6, int(sy) - 6, int(sx) + 6, int(sy) + 6), [1'f32, 0, 0, 1])
    let (dx, dy) = h.apply(0.5, 0.3)
    check cloneStroke(sc, s, [sx, sy], [[dx, dy]], 10, 0.8, 1) == 1
    check s.rgba(int(dx), int(dy))[0] > 0.8
    check cloneStroke(sc, s, [sx, sy], newSeq[array[2, float64]](), 10, 0.8, 1) == 0
