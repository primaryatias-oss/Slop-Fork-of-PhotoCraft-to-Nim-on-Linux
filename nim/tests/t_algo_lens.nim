## Lens Correction (ports of the Rust unit tests). Bit parity with Rust is checked by
## `tools/algocheck.py` (`tools/algocases/lens.json`, see PORTING.md).

import std/[unittest, options, math, json]
import photocraft/[geom, raster, color]
import photocraft/algo/lens

proc gridSurface(fmt: PixelFormat; w, h: int): Surface =
  result = newSurface(fmt)
  let n = fmt.channels
  var row = newSeq[float32](w * n)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let v = (if x mod 16 < 2 or y mod 16 < 2: 0.9'f32 else: 0.3'f32)
      let px = fromRgba(fmt, [v, v * 0.8'f32, v * 0.6'f32, 1'f32])
      for c in 0 ..< n: row[x * n + c] = px[c]
    result.writeRegion(rect(0, y, w, y + 1), row)

proc with(f: proc(l: var LensCorrection)): LensCorrection =
  result = defaultLensCorrection()
  f(result)

suite "lens correction":
  test "identity is lossless at all depths":
    for sample in [stU8, stU16, stF32]:
      let s = gridSurface(pixelFormat(cmRgb, sample, true), 96, 64)
      let o = correct(s, rect(0, 0, 96, 64), defaultLensCorrection())
      let (a, b) = (s.readRegion(rect(0, 0, 96, 64)), o.readRegion(rect(0, 0, 96, 64)))
      var err = 0'f32
      for i in 0 ..< a.len: err = max(err, abs(a[i] - b[i]))
      check err < 2 / 255
  test "distortion round trip and edges":
    let frame = rect(0, 0, 200, 100)
    let m = newLensMap(with(proc(l: var LensCorrection) = l.distortion = 50), frame)
    let s = m.sample(100, 50).get
    check abs(s.p[0] - 100) < 1e-9 and abs(s.p[1] - 50) < 1e-9
    let c = m.sample(0, 0).get
    check c.p[0] > 5 and c.p[1] > 2
    let src = gridSurface(pixelFormat(cmRgb, stU8, false), 200, 100)
    let neg = with(proc(l: var LensCorrection) = l.distortion = -60)
    let t = correct(src, frame, neg)
    check t.format.alpha and t.pixel(0, 0)[3] < 0.01 and t.pixel(100, 50)[3] > 0.99
    var e = neg
    e.edge = edgeExtension()
    check not correct(src, frame, e).format.alpha
    var col = neg
    col.edge = edgeColor([1'f32, 0, 0, 1])
    let p = correct(src, frame, col).pixel(0, 0)
    check p[0] > 0.99 and p[1] < 0.01
  test "vignette, CA, perspective and straighten":
    let frame = rect(0, 0, 300, 200)
    let m = newLensMap(with(proc(l: var LensCorrection) = l.vignetteAmount = 100), frame)
    check abs(m.sample(150, 100).get.gain - 1) < 1e-6 and m.sample(1, 1).get.gain > 2
    check newLensMap(with(proc(l: var LensCorrection) = l.vignetteAmount = -100), frame).sample(1, 1).get.gain < 0.5
    let ca = newLensMap(with(proc(l: var LensCorrection) = l.redCyan = -100), frame).sample(10, 10).get
    check ca.pr[0] < ca.p[0]
    let v = newLensMap(with(proc(l: var LensCorrection) = l.vertical = -50), frame)
    let (tl, tr) = (v.sample(0, 0).get.p, v.sample(300, 0).get.p)
    let (bl, br) = (v.sample(0, 200).get.p, v.sample(300, 200).get.p)
    check tr[0] - tl[0] < br[0] - bl[0]
    check abs(straightenAngle([0.0, 0.0], [100.0, 100.0 * tan(degToRad(5.0))]) + 5) < 1e-9
    check abs(straightenAngle([0.0, 0.0], [3.0, 100.0]) - 1.718) < 0.01
    let lc = with(proc(l: var LensCorrection) = l.angle = 5)
    let s = autoScale(lc, frame)
    check s > 105 and s < 130
    var lc2 = lc
    lc2.scale = s
    let px = newLensMap(lc2, frame).sample(0, 0).get.p[0]
    check px >= 0 and px <= 300
  test "generic profiles and CMYK":
    let (wide, tele, mid) = (genericProfile(16), genericProfile(135), genericProfile(30))
    check wide.k1 < -0.05 and tele.k1 > 0 and wide.vignette[0] < tele.vignette[0]
    check mid.k1 < -0.025 and mid.k1 > -0.06
    check genericProfile(17.5).name == "Generic Lens (18 mm equivalent)"
    var s = newSurface(pixelFormat(cmCmyk, stU8, false))
    s.fillRect(rect(0, 0, 40, 40), [0.5'f32, 0.5, 0.5, 0.5])
    var lc = defaultLensCorrection()
    lc.vignetteAmount = 100
    lc.edge = edgeExtension()
    let o = correct(s, rect(0, 0, 40, 40), lc)
    check o.pixel(0, 0)[0] < 0.4 and abs(o.pixel(20, 20)[0] - 0.5) < 0.02
  test "auto scale terminates on a zero or negative scale (Rust loops forever there)":
    for sc in [0.0, -50.0]:
      var lc = with(proc(l: var LensCorrection) = l.angle = 5)
      lc.scale = sc
      let r = autoScale(lc, rect(0, 0, 300, 200))
      check r > 0 and r < 1000
  test "options parse from JSON":
    let lc = lensCorrectionFromJson(parseJson("""{"distortion": 20, "edge": {"color": [1, 0, 0, 1]},
      "profile": {"name": "p", "k1": 0.1, "k2": 0, "p1": 0, "p2": 0, "vignette": [0, 0, 0], "ca_red": 0, "ca_blue": 0}}"""))
    check lc.distortion == 20 and lc.edge == edgeColor([1'f32, 0, 0, 1]) and lc.profile.get.k1 == 0.1 and lc.scale == 100
    expect ValueError: discard lensCorrectionFromJson(parseJson("""{"edge": "mirror"}"""))
    expect ValueError: discard lensCorrectionFromJson(parseJson("""{"profile": {"k1": 0.1}}"""))
