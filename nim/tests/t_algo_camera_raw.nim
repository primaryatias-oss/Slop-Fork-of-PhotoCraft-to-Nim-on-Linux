## Filter › Camera Raw Filter (`camera_raw.rs` tests) plus no-crash checks.

import std/[unittest, json, math, sequtils, strutils]
import photocraft/algo/[util, camera_raw]

proc img(w, h: int): seq[array[4, float32]] =
  for i in 0 ..< w * h:
    let (x, y) = (float32(i mod w) / float32(w), float32(i div w) / float32(h))
    result.add [0.2'f32 + 0.6'f32 * x, 0.3'f32 + 0.4'f32 * y, 0.5'f32 - 0.3'f32 * x * y, 1'f32]

proc mean(px: seq[array[4, float32]]; c: int): float32 =
  var s = 0'f32
  for q in px: s += q[c]
  s / float32(px.len)

proc cr(j: JsonNode): CameraRaw = cameraRawFromJson(j)

proc validates(p: CameraRaw): bool =
  try:
    p.validate
    true
  except ValueError: false

suite "camera raw":
  test "new curves are validated but saved curves still load and render":
    for field in ["pointCurve", "pointCurveRed", "pointCurveGreen", "pointCurveBlue"]:
      var long17 = newJArray()
      for i in 0 ..< 17: long17.add %*[i * 15, i * 15]
      for points in [%*[[128, 128]], %*[[255, 255], [0, 0]], %*[[0, 0], [0, 255]], %*[[0, -1], [255, 255]],
                     %*[[0, 0], [256, 255]], %*[[0, 0], [1e30, 255]], long17]:
        check not cr(%*{field: points}).validates
      var long16 = newJArray()
      for i in 0 ..< 16: long16.add %*[i * 17, i * 17]
      for points in [newJArray(), %*[[0, 0], [255, 255]], long16]:
        check cr(%*{field: points}).validates
    check cr(newJObject()) == defaultCameraRaw()
    # An older editor could stack two points on one input and exceed 16 points; such a saved
    # Smart Filter must still develop instead of silently disappearing.
    var legacy: seq[array[2, float32]]
    for i in 0 ..< 20: legacy.add [float32(i) * 12, float32(i) * 12]
    legacy.insert([60'f32, 200], 5)
    var p = defaultCameraRaw()
    p.pointCurve = legacy
    check not p.validates
    var edited = p
    edited.exposure = 1
    edited.validateChanges(p) # an unchanged stored curve stays editable
    var broken = p
    broken.pointCurve = @[[10'f32, 10]]
    expect ValueError: broken.validateChanges(p)
    var px = img(4, 4)
    develop(px, 4, 4, p, false)
    check px.allIt(it.allIt(it.isFinite))

  test "defaults are identity":
    var a = img(40, 30)
    let b = a
    develop(a, 40, 30, defaultCameraRaw(), false)
    check a == b

  test "basic panel moves the right way":
    let (w, h) = (64, 48)
    let base = img(w, h)
    proc run(j: JsonNode): seq[array[4, float32]] =
      result = base
      develop(result, w, h, cr(j), false)
    let warm = run(%*{"temperature": 50})
    check mean(warm, 0) > mean(base, 0) and mean(warm, 2) < mean(base, 2)
    check mean(run(%*{"exposure": 1}), 1) > mean(base, 1) + 0.1
    check run(%*{"shadows": 100})[0][1] > base[0][1]
    check run(%*{"highlights": -100})[w * h - 1][0] < base[w * h - 1][0]
    let con = run(%*{"contrast": 100})
    check con[0][1] < base[0][1] and con[w * h - 1][0] > base[w * h - 1][0]
    check run(%*{"saturation": -100}).allIt(abs(it[0] - it[1]) < 1e-4 and abs(it[1] - it[2]) < 1e-4)
    proc spread(q: array[4, float32]): float32 = max(max(q[0], q[1]), q[2]) - min(min(q[0], q[1]), q[2])
    check spread(run(%*{"vibrance": 100})[w * h div 2]) > spread(base[w * h div 2])
    for j in [%*{"clarity": 80}, %*{"texture": 80}, %*{"dehaze": 60}, %*{"dehaze": -60}, %*{"whites": 50, "blacks": -50}]:
      let o = run(j)
      check o.allIt(it[0 .. 2].allIt(it.isFinite and it >= 0'f32 and it <= 1'f32))
      check o != base

  test "curves, hsl, grading, detail and effects":
    let lut = curveLut([[0'f32, 0], [64'f32, 40], [192'f32, 220], [255'f32, 255]], 256)
    check abs(lut[64] - 40'f32 / 255) < 0.01 and abs(lut[192] - 220'f32 / 255) < 0.01
    for i in 1 ..< lut.len: check lut[i] >= lut[i - 1]
    for hh in countup(0, 359, 7):
      var s = 0'f32
      for v in bandWeights(float32(hh)): s += v
      check abs(s - 1) < 1e-4
    let (w, h) = (48, 32)
    var hs: array[8, float32]
    hs[0] = -100
    var p = defaultCameraRaw()
    p.hslSat = hs
    var a = newSeqWith(w * h, [0.8'f32, 0.2, 0.2, 1])
    develop(a, w, h, p, false)
    check a[0][0] - a[0][1] < 0.1 # reds desaturated
    var b = newSeqWith(w * h, [0.2'f32, 0.2, 0.8, 1])
    develop(b, w, h, p, false)
    check abs(b[0][2] - 0.8) < 1e-4 # blues untouched
    # Grading: blue shadows tint dark pixels, leave bright ones nearly alone.
    var g = @[[0.1'f32, 0.1, 0.1, 1], [0.95'f32, 0.95, 0.95, 1]]
    develop(g, 2, 1, cr(%*{"gradeShadows": {"hue": 220, "sat": 80, "lum": 0}}), false)
    check g[0][2] > g[0][0] + 0.05 and abs(g[1][2] - g[1][0]) < 0.02
    # Point curve + parametric.
    var c = img(w, h)
    develop(c, w, h, cr(%*{"pointCurve": [[0, 30], [255, 255]], "curveShadows": 50}), false)
    check c.allIt(it[1] >= 30'f32 / 255 - 1e-4)
    # Noise reduction reduces variance; sharpening increases it; grain is deterministic.
    var noisy: seq[array[4, float32]]
    for i in 0 ..< w * h:
      let n = hash01(int64(i), 0, 3) * 0.2'f32
      noisy.add [0.4'f32 + n, 0.4'f32 + n, 0.4'f32 + n, 1]
    proc variance(a: seq[array[4, float32]]): float32 =
      let m = mean(a, 1)
      for q in a: result += (q[1] - m) * (q[1] - m)
      result /= float32(a.len)
    var nr = noisy
    develop(nr, w, h, cr(%*{"noiseLuminance": 80}), false)
    check variance(nr) < variance(noisy) * 0.5
    var sh = noisy
    develop(sh, w, h, cr(%*{"sharpenAmount": 100}), false)
    check variance(sh) > variance(noisy)
    var (g1, g2) = (img(w, h), img(w, h))
    let gp = cr(%*{"grainAmount": 50, "seed": 4})
    develop(g1, w, h, gp, false)
    develop(g2, w, h, gp, false)
    check g1 == g2
    check g1 != img(w, h)
    # Post-crop vignette darkens corners, leaves the centre.
    var v = newSeqWith(w * h, [0.5'f32, 0.5, 0.5, 1])
    develop(v, w, h, cr(%*{"vignetteAmount": -80}), false)
    check v[0][0] < 0.35 and abs(v[(h div 2) * w + w div 2][0] - 0.5) < 0.01

  test "float documents keep overrange":
    var a = newSeqWith(16, [2'f32, 1.5, 0.5, 1])
    develop(a, 4, 4, cr(%*{"exposure": 0.5}), true)
    check a[0][0] > 2

  test "json round trip and serde forms":
    let p = cr(%*{"exposure": 0.1, "pointCurve": [[0, 10], [255, 250]], "gradeGlobal": [30, 40], "seed": 7,
                  "vignetteStyle": "paintOverlay", "hslLum": [1, 2, 3, 4, 5, 6, 7, 8]})
    check p.gradeGlobal == Wheel(hue: 30, sat: 40, lum: 0)
    check cr(parseJson($p.toJ)) == p
    check ($p.toJ).contains("\"exposure\":0.1,")
    check cr(%*[1, 2]).tint == 2
    for bad in [%*{"curveSplits": [1, 2]}, %*{"seed": -1}, %*{"seed": 1.5}, %*{"exposure": "1"}, %*"x",
                %*{"gradeShadows": [1, 2, 3, 4]}, %*{"pointCurve": [[1, 2], [3]]}, %*{"exposure": nil}]:
      expect ValueError: discard cr(bad)

  test "hostile input does not crash":
    var px = img(5, 5)
    expect ValueError: develop(px, 7, 5, cr(%*{"exposure": 1}), false)
    var e: seq[array[4, float32]]
    develop(e, 0, 0, cr(%*{"exposure": 1}), false)
    for j in [%*{"exposure": 1e30, "dehaze": 100, "clarity": 100, "texture": 100},
              %*{"grainAmount": 50, "grainSize": -25}, %*{"pixelScale": 0, "sharpenAmount": 50, "noiseColor": 50},
              %*{"noiseLuminance": 1e30, "noiseColor": 1e30}, %*{"vignetteAmount": -1e30, "vignetteMidpoint": -1e9},
              %*{"pointCurve": [[1e30, -1e30], [0, 0], [0, 0]], "curveSplits": [-1e30, 1e30, 0], "curveDarks": 1e30}]:
      var q = img(9, 7)
      develop(q, 9, 7, cr(j), true)
      var r = @[[NaN.float32, Inf.float32, -Inf.float32, 1]]
      develop(r, 1, 1, cr(j), false)
    check curveLut([[0'f32, 0], [255'f32, 255]], 0).len == 0
