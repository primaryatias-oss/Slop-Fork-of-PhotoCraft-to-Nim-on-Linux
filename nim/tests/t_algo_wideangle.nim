## Adaptive Wide Angle (ports of the Rust unit tests). Bit parity with Rust is checked by
## `tools/algocheck.py` (`tools/algocases/wideangle.json`, see PORTING.md).

import std/[unittest, options, json]
import photocraft/[geom, raster, color]
import photocraft/algo/[wideangle, transform]

proc wa(model: WideModel; focal = 0.0; scale = 100.0; constraints: seq[Constraint] = @[]): WideAngle =
  result = defaultWideAngle()
  result.model = model
  result.focalLength = focal
  result.scale = scale
  result.constraints = constraints

suite "adaptive wide angle":
  test "camera models round trip":
    let frame = rect(0, 0, 600, 400)
    for model in [wmFisheye, wmPerspective]:
      let cam = newCamera(wa(model, 15), frame)
      for (x, y) in [(10.0, 20.0), (300.0, 200.0), (590.0, 350.0)]:
        let p = cam.pixel(cam.ray(x, y)).get
        check abs(p[0] - x) < 1e-6 and abs(p[1] - y) < 1e-6
    let eq = newCamera(defaultWideAngle(), rect(0, 0, 800, 400))
    check eq.model == wmFullSpherical
    let p = eq.pixel(eq.ray(123, 77)).get
    check abs(p[0] - 123) < 1e-6 and abs(p[1] - 77) < 1e-6
    let cam = newCamera(wa(wmPerspective), frame)
    check abs(cam.arc([50.0, 50.0], [550.0, 80.0], 10)[5][1] - 65) < 2
    let fish = newCamera(wa(wmFisheye, 8), frame)
    check fish.arc([50.0, 60.0], [550.0, 60.0], 10)[5][1] < 50
  test "constraints become straight and level":
    let frame = rect(0, 0, 480, 320)
    let p = wa(wmFisheye, 8, constraints = @[Constraint(a: [60.0, 70.0], b: [420.0, 70.0], orientation: orHorizontal),
                                              Constraint(a: [80.0, 60.0], b: [80.0, 260.0], orientation: orFree)])
    let cam = newCamera(p, frame)
    var before = 0.0
    for q in cam.arc([60.0, 70.0], [420.0, 70.0], 24): before = max(before, abs(q[1] - 70))
    check before > 10
    let mesh = solve(p, frame)
    check mesh.residual < 2
    var (lo, hi) = (Inf, NegInf)
    for q in mesh.curves[0]:
      let y = mesh.map(frame, q)[1]
      lo = min(lo, y)
      hi = max(hi, y)
    check hi - lo < 2
    let c = mesh.map(frame, [300.0, 250.0])
    check abs(c[0] - 300) < 80 and abs(c[1] - 250) < 40
  test "no constraints is the identity and renders":
    let frame = rect(0, 0, 64, 48)
    var s = newSurface(pixelFormat(cmRgb, stU8, true))
    s.fillRect(frame, [0.2'f32, 0.4, 0.6, 1])
    let mesh = solve(defaultWideAngle(), frame)
    for (o, sv) in mesh.verts: check abs(o[0] - sv[0]) < 1e-9 and abs(o[1] - sv[1]) < 1e-9
    let o = wideangle.apply(s, frame, wa(wmAuto, scale = 80), ipBilinear)
    check o.pixel(32, 24)[3] > 0.99 and o.pixel(1, 1)[3] < 0.01
  test "no gaps across tiles":
    let frame = rect(0, 0, 772, 517)
    var s = newSurface(pixelFormat(cmRgb, stU8, true))
    s.fillRect(frame, [0.2'f32, 0.4, 0.6, 1])
    for p in [defaultWideAngle(), wa(wmFisheye, 12, constraints = @[Constraint(a: [20.0, 30.0], b: [700.0, 35.0], orientation: orHorizontal)])]:
      let o = wideangle.apply(s, frame, p, ipBilinear)
      var holes = 0
      for y in 80 ..< 440:
        for x in 80 ..< 690:
          if o.rgba(x, y)[3] < 0.5: inc holes
      check holes == 0
  test "options parse from JSON":
    let p = wideAngleFromJson(parseJson("""{"model": "fisheye", "focalLength": 10, "constraints": [{"a": [1, 2], "b": [3, 4], "orientation": "vertical"}]}"""))
    check p.model == wmFisheye and p.focalLength == 10 and p.cropFactor == 1 and p.constraints[0].orientation == orVertical
    check parseWideModel("spherical") == some(wmFullSpherical) and parseWideModel("x").isNone
    expect ValueError: discard wideAngleFromJson(parseJson("""{"model": "orthographic"}"""))
    expect ValueError: discard wideAngleFromJson(parseJson("""{"constraints": [{"a": [1, 2]}]}"""))
