## Blur family through the dispatcher: invariants and adversarial parameters. Pixel parity with
## Rust is checked by `tools/filtercheck.py` (see PORTING.md).

import std/[unittest, options, json, math]
import photocraft/[geom, color, raster, serde, algo]

proc testSurface(fmt: PixelFormat): Surface =
  result = newSurface(fmt)
  for y in 0 ..< 64:
    for x in 0 ..< 64:
      if (x div 8 + y div 8) mod 3 == 0: continue
      result.writePixel(x, y, [float32(x) / 64, float32(y) / 64, 0.5'f32, 1])

proc run(js: string; s: Surface): Surface =
  let p = fromJson(parseJson(js), FilterParams)
  let b = rect(0, 0, 64, 64)
  apply(s, p, outputArea(p, s.contentBounds, b), b)

suite "blur filters":
  let fmt = pixelFormat(cmRgb, stF32, true)
  let s = testSurface(fmt)

  test "a constant opaque image keeps its colour":
    var c = newSurface(fmt)
    c.fillRect(rect(0, 0, 64, 64), [0.25'f32, 0.5, 0.75, 1])
    for js in ["""{"filter":"gaussianBlur","radius":3}""", """{"filter":"gaussianBlur","radius":20}""",
               """{"filter":"boxBlur","radius":12}""", """{"filter":"motionBlur","angle":30,"distance":9}""",
               """{"filter":"surfaceBlur","radius":4,"threshold":20}""", """{"filter":"shapeBlur","radius":6,"shape":"star"}"""]:
      let o = run(js, c)
      let px = o.pixel(32, 32)
      check abs(px[0] - 0.25) < 1e-4 and abs(px[2] - 0.75) < 1e-4 and px[3] > 0.5

  test "every blur kind is registered":
    for k in [fltGaussianBlur, fltBoxBlur, fltMotionBlur, fltRadialBlur, fltSurfaceBlur, fltUnsharpMask,
              fltSmartSharpen, fltHighPass, fltSmartBlur, fltLensBlur, fltShapeBlur, fltTiltShift, fltIrisBlur,
              fltFieldBlur, fltSpinBlur, fltPathBlur]:
      check isAvailable(k)

  test "adversarial parameters never crash":
    for js in ["""{"filter":"gaussianBlur","radius":-5}""", """{"filter":"gaussianBlur","radius":1e9}""",
               """{"filter":"boxBlur","radius":-1}""", """{"filter":"motionBlur","angle":1e30,"distance":3}""",
               """{"filter":"motionBlur","angle":0,"distance":-4000}""",
               """{"filter":"radialBlur","amount":1e9,"method":"spin","quality":"best","centerX":1e20,"centerY":0}""",
               """{"filter":"surfaceBlur","radius":0,"threshold":-3}""", """{"filter":"shapeBlur","radius":1e9,"shape":"heart"}""",
               """{"filter":"lensBlur","radius":-3,"blades":0,"curvature":1e9,"rotation":0,"depth":"layerMask","focalDistance":0,"invertDepth":false,"brightness":1e9,"threshold":0,"noise":1e9,"distribution":"gaussian","monochromatic":false,"seed":0}""",
               """{"filter":"irisBlur","pins":[]}""", """{"filter":"fieldBlur","pins":[]}""",
               """{"filter":"spinBlur","pins":[{"x":0.5,"y":0.5,"radiusX":0,"radiusY":-1,"angle":0,"blurAngle":1e9}]}""",
               """{"filter":"pathBlur","paths":[{"points":[[0.5,0.5]],"speed":10,"taper":0}]}""",
               """{"filter":"tiltShift","blur":-10,"centerX":0.5,"centerY":0.5,"angle":0,"focus":-1,"transition":0}"""]:
      discard run(js, s)

  test "sharpening leaves transparent pixels alone and high pass is grey on flat areas":
    var c = newSurface(fmt)
    c.fillRect(rect(0, 0, 64, 64), [0.3'f32, 0.3, 0.3, 1])
    let o = run("""{"filter":"highPass","radius":4}""", c)
    check abs(o.pixel(30, 30)[0] - 0.5) < 1e-5
    let u = run("""{"filter":"unsharpMask","amount":200,"radius":2,"threshold":0}""", s)
    check u.pixel(0, 0)[3] == 0
