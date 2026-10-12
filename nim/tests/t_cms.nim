## Tests for photocraft/cms (ported from crates/cms tests).
import std/[unittest, math, options]
import photocraft/cms

proc lab(): Profile = bLabD50.profile
proc srgbP(): Profile = bSrgb.profile
proc cmyk(): Profile = bCoatedCmyk.profile

proc dec(o: openArray[float32]): Vec3 = [float64(o[0]) * 100.0, float64(o[1]) * 255.0 - 128.0, float64(o[2]) * 255.0 - 128.0]
proc enc(l: Vec3): array[3, float32] = [float32(l[0] / 100.0), float32((l[1] + 128.0) / 255.0), float32((l[2] + 128.0) / 255.0)]

proc toLab(p: Profile; dev: openArray[float32]; intent: Intent): Vec3 =
  let t = newTransform(p, lab(), intent, false)
  var o: array[3, float32]
  t.eval(dev, o)
  dec(o)

suite "cms math and curves":
  test "lab roundtrip":
    for xyz in [[0.2, 0.3, 0.1], [0.9642, 1.0, 0.8249], [0.001, 0.002, 0.0005]]:
      let back = labToXyz(xyzToLab(xyz, D50), D50)
      for i in 0..2: check abs(back[i] - xyz[i]) < 1e-12

  test "bradford maps white":
    let d65 = xyToXyz(D65xy)
    let w = apply(bradford(d65, D50), d65)
    for i in 0..2: check abs(w[i] - D50[i]) < 1e-12

  test "srgb matches formula":
    let c = srgbTrc()
    for i in 0..255:
      let v = float64(i) / 255.0
      let want = if v <= 0.04045: v / 12.92 else: pow((v + 0.055) / 1.055, 2.4)
      check abs(c.eval64(v) - want) < 1e-12
      check abs(c.evalInverse64(want) - v) < 1e-9

  test "parametric inverses":
    let curves = [paraCurve(1, [2.0, 0.9, 0.1, 0.0, 0.0, 0.0, 0.0]), paraCurve(2, [2.0, 0.9, 0.1, 0.05, 0.0, 0.0, 0.0]),
                  srgbTrc(), paraCurve(4, [2.4, 0.9, 0.05, 0.08, 0.04, 0.01, 0.002]), gammaCurve(1.8)]
    for c in curves:
      for i in 1 ..< 100:
        let x = float64(i) / 100.0
        check abs(c.evalInverse64(c.eval64(x)) - x) < 1e-6

  test "table inverse":
    let c = tableCurve(gammaCurve(2.2).sample(1024))
    for i in 0..100:
      let x = float32(i) / 100.0'f32
      check abs(c.evalInverse(c.eval(x)) - x) < 1e-4
    check abs(tableCurve(@[1.0'f32, 0.5, 0.0]).evalInverse(0.25) - 0.75) < 1e-6

suite "cms profiles and transforms":
  test "builtins encode and reparse":
    for b in Builtin:
      let p = b.profile
      let bytes = p.toBytes
      let q = parseProfile(bytes)
      check q.description == b.description
      check q.colorSpace == p.colorSpace
      check q.toBytes == bytes
      check builtinFromId(b.id) == some(b)
      discard newTransform(p, lab(), RelativeColorimetric, false)
      discard newTransform(lab(), p, RelativeColorimetric, false)
    check builtinFromId("Adobe RGB (1998)") == some(bAdobeRgbCompat)

  test "srgb known lab values":
    let w = toLab(srgbP(), [1.0'f32, 1, 1], RelativeColorimetric)
    check abs(w[0] - 100.0) < 0.01 and abs(w[1]) < 0.01 and abs(w[2]) < 0.01
    check abs(toLab(srgbP(), [0.0'f32, 0, 0], RelativeColorimetric)[0]) < 0.01
    check deltaE76(toLab(srgbP(), [1.0'f32, 0, 0], RelativeColorimetric), [54.29, 80.80, 69.89]) < 0.15
    check deltaE76(toLab(srgbP(), [0.0'f32, 1, 0], RelativeColorimetric), [87.82, -79.29, 80.99]) < 0.3
    check deltaE76(toLab(srgbP(), [0.0'f32, 0, 1], RelativeColorimetric), [29.57, 68.30, -112.03]) < 0.3
    for bi in [bSrgb, bDisplayP3, bAdobeRgbCompat, bProPhotoCompat, bLinearSrgb, bRec2020]:
      let m = toLab(bi.profile, [0.5'f32, 0.5, 0.5], RelativeColorimetric)
      check abs(m[1]) < 0.02 and abs(m[2]) < 0.02

  test "rgb lab rgb roundtrip 8-bit and float":
    let to = newTransform(srgbP(), lab(), RelativeColorimetric, false)
    let back = newTransform(lab(), srgbP(), RelativeColorimetric, false)
    var src8: seq[uint8]
    for r in countup(0, 255, 5):
      for g in countup(0, 255, 7):
        for b in countup(0, 255, 9):
          src8.add [uint8(r), uint8(g), uint8(b)]
    var f = newSeq[float32](src8.len)
    for i, v in src8: f[i] = float32(v) / 255.0'f32
    var labf = newSeq[float32](f.len)
    to.convertF32(f, 3, labf, 3, false)
    var rgbf = newSeq[float32](f.len)
    back.convertF32(labf, 3, rgbf, 3, false)
    var worst = 0.0'f32
    for i in 0 ..< src8.len: worst = max(worst, abs(float32(src8[i]) - rgbf[i] * 255.0'f32))
    check worst <= 1.0
    var lab16 = newSeq[uint16](labf.len)
    for i, v in labf: lab16[i] = uint16(clamp(v, 0.0'f32, 1.0'f32) * 65535.0'f32 + 0.5'f32)
    var rgb16 = newSeq[uint16](lab16.len)
    back.convertU16(lab16, 3, rgb16, 3, false)
    worst = 0
    for i in 0 ..< src8.len: worst = max(worst, abs(float32(src8[i]) - float32(rgb16[i]) / 257.0'f32))
    check worst <= 1.0
    var lab8 = newSeq[uint8](src8.len)
    to.convertU8(src8, 3, lab8, 3, false)
    worst = 0
    for i in 0 ..< lab8.len: worst = max(worst, abs(float32(lab8[i]) - labf[i] * 255.0'f32))
    check worst <= 1.0

  test "rgb to rgb matrix shaper paths agree":
    for dst in [bAdobeRgbCompat, bProPhotoCompat, bDisplayP3, bLinearSrgb]:
      let t = newTransform(srgbP(), dst.profile, RelativeColorimetric, false)
      var src: seq[uint8]
      for v in 0..255: src.add [uint8(v), uint8(v div 2), uint8(255 - v)]
      var o = newSeq[uint8](src.len)
      t.convertU8(src, 3, o, 3, false)
      var worst = 0.0'f32
      for px in 0 ..< 256:
        var e: array[3, float32]
        t.eval([float32(src[3 * px]) / 255, float32(src[3 * px + 1]) / 255, float32(src[3 * px + 2]) / 255], e)
        for k in 0..2: worst = max(worst, abs(float32(o[3 * px + k]) - clamp(e[k], 0.0'f32, 1.0'f32) * 255.0'f32))
      check worst <= 0.6

  test "identity transform is identity":
    let t = newTransform(srgbP(), srgbP(), RelativeColorimetric, true)
    var src: seq[uint8]
    for v in 0..255: src.add [uint8(v), uint8(255 - v), uint8(v div 3), 77'u8]
    var o = newSeq[uint8](src.len)
    t.convertU8(src, 4, o, 4, true)
    check src == o

  test "cmyk profile is monotonic and in range":
    let p = cmyk()
    check p.colorSpace.kind == csCmyk
    for ink in 0..3:
      var prev = Inf
      for i in 0..32:
        var v = [0.1'f32, 0.1, 0.1, 0.1]
        v[ink] = float32(i) / 32.0'f32
        let l = toLab(p, v, RelativeColorimetric)[0]
        check l <= prev + 0.05
        prev = l
    check deltaE76(toLab(p, [0.0'f32, 0, 0, 0], RelativeColorimetric), [100.0, 0.0, 0.0]) < 0.5
    for intent in [Perceptual, RelativeColorimetric]:
      let t = newTransform(srgbP(), p, intent, true)
      let back = newTransform(p, lab(), RelativeColorimetric, false)
      var prevK = -1.0'f32
      for v in countdown(255, 0, 5):
        let g = float32(v) / 255.0'f32
        var o: array[4, float32]
        t.eval([g, g, g], o)
        check o[0] + o[1] + o[2] + o[3] <= 3.02
        check o[3] >= prevK - 0.01
        prevK = o[3]
        var l: array[3, float32]
        back.eval(o, l)
        let lb = dec(l)
        check abs(lb[1]) < 3.0 and abs(lb[2]) < 3.0

  test "in-gamut colours survive cmyk roundtrip":
    let to = newTransform(lab(), cmyk(), RelativeColorimetric, false)
    let back = newTransform(cmyk(), lab(), RelativeColorimetric, false)
    for target in [[50.0, 0.0, 0.0], [70.0, 10.0, 10.0], [60.0, -20.0, -20.0], [80.0, 0.0, 40.0], [40.0, 30.0, -10.0]]:
      var c: array[4, float32]
      to.eval(enc(target), c)
      var l: array[3, float32]
      back.eval(c, l)
      check deltaE76(dec(l), target) < 2.0

  test "intents differ as expected":
    let per = newTransform(srgbP(), cmyk(), Perceptual, false)
    let rel = newTransform(srgbP(), cmyk(), RelativeColorimetric, false)
    var a, b: array[4, float32]
    per.eval([0.0'f32, 0.2, 1.0], a)
    rel.eval([0.0'f32, 0.2, 1.0], b)
    var d = 0.0'f32
    for k in 0..3: d += abs(a[k] - b[k])
    check d > 0.03
    let rel2 = newTransform(cmyk(), srgbP(), RelativeColorimetric, false)
    let abs2 = newTransform(cmyk(), srgbP(), AbsoluteColorimetric, false)
    var w1, w2: array[3, float32]
    rel2.eval([0.0'f32, 0, 0, 0], w1)
    abs2.eval([0.0'f32, 0, 0, 0], w2)
    check w1[0] > 0.995 and w1[1] > 0.995 and w1[2] > 0.995
    check w2[0] < 0.97 and w2[2] > w2[0]

  test "black point compensation":
    let bl = blackPoint(cmyk(), RelativeColorimetric, false)
    check bl > 0.0
    check blackPoint(srgbP(), RelativeColorimetric, true) == 0.0

  test "proof and display lut":
    let t = proofTransform(srgbP(), cmyk(), srgbP(), RelativeColorimetric, true, false)
    var o: array[3, float32]
    t.eval([0.5'f32, 0.5, 0.5], o)
    check abs(o[0] - o[1]) < 0.02 and abs(o[1] - o[2]) < 0.03
    t.eval([0.0'f32, 0.0, 1.0], o)
    check o[0] > 0.1 or o[1] > 0.1
    let sim = proofTransform(srgbP(), cmyk(), srgbP(), RelativeColorimetric, true, true)
    sim.eval([1.0'f32, 1.0, 1.0], o)
    check o[0] < 0.97
    let disp = newTransform(srgbP(), bDisplayP3.profile, RelativeColorimetric, false)
    let lut = lut3dFromTransform(disp, 33)
    check lut.data.len == 33 * 33 * 33
    var worst = 0.0'f32
    for i in 0'u32 ..< 500:
      let c = [float32(i * 7 mod 101) / 100, float32(i * 13 mod 101) / 100, float32(i * 29 mod 101) / 100]
      var e: array[3, float32]
      disp.eval(c, e)
      let s = lut.sample(c)
      for k in 0..2: worst = max(worst, abs(s[k] - e[k]))
    check worst < 2.0 / 255.0

  test "gray profiles":
    let t = newTransform(bSGray.profile, srgbP(), RelativeColorimetric, false)
    for v in [0.0'f32, 0.25, 0.5, 0.75, 1.0]:
      var o: array[3, float32]
      t.eval([v], o)
      for c in o: check abs(c - v) < 1e-3

  test "same colors and gamut check":
    check sameColors(srgbP(), srgbP())
    check not sameColors(srgbP(), bDisplayP3.profile)
    let g = newGamutCheck(srgbP(), cmyk(), DefaultGamutThreshold)
    check g.outOfGamut([0.0'f32, 0.0, 1.0])
    check not g.outOfGamut([0.5'f32, 0.5, 0.5])

  test "half floats":
    for v in [0.0'f32, 1.0, -2.5, 0.333, 65504.0]:
      check abs(f16ToF32(f32ToF16(v)) - v) <= abs(v) * 1e-3

  test "cube lut parse and write":
    let l = identityLut(3)
    let back = parseCube(writeCube(l))
    check back.size == 3
    check back.data.len == 81
    check builtinLook("warm").isSome
    expect LutError: discard parseCube("LUT_3D_SIZE 2\n0 0 0\n")
