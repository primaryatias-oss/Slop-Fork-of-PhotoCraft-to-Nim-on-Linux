## Colour science for raw development (port of `raw/src/color.rs`): 3×3 matrix helpers,
## chromaticity conversions, correlated colour temperature, Bradford adaptation and the camera →
## XYZ (D50) → output-space mapping of the DNG specification (Adobe DNG Specification 1.7,
## chapter 6 "Mapping Camera Color Space to CIE XYZ Space").

import std/[options, math, algorithm]
import ../rmath

type
  Mat3* = array[3, array[3, float64]]

  Calibration* = object
    ## One DNG calibration (ColorMatrixN with its companions).
    temperature*: float64            ## illuminant temperature in kelvin
    colorMatrix*: Mat3               ## XYZ → reference camera (ColorMatrixN)
    forwardMatrix*: Option[Mat3]     ## white-balanced camera → XYZ D50 (ForwardMatrixN)
    cameraCalibration*: Mat3         ## reference camera → individual camera (CameraCalibrationN)

  ColorInfo* = object
    ## Colour description of a raw file.
    calibrations*: seq[Calibration]  ## DNG calibrations (0, 1 or 2), sorted by temperature
    analogBalance*: Option[array[3, float64]]
    asShotNeutral*: Option[array[3, float64]] ## camera coordinates of a neutral
    asShotWhiteXy*: Option[array[2, float64]]

const
  Identity*: Mat3 = [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]
  D50*: array[3, float64] = [0.9642, 1.0, 0.8249] ## CIE D50 white (the ICC PCS illuminant)
  D50Xy*: array[2, float64] = [0.3457, 0.3585]
  D65Xy*: array[2, float64] = [0.3127, 0.3290]
  Romm*: array[3, array[2, float64]] = [[0.7347, 0.2653], [0.1596, 0.8404], [0.0366, 0.0001]]
    ## ROMM RGB (ProPhoto) primaries, ISO 22028-2; white D50
  Rec709*: array[3, array[2, float64]] = [[0.64, 0.33], [0.30, 0.60], [0.15, 0.06]]

proc isFin(v: float64): bool {.inline.} = v == v and v != Inf and v != NegInf

proc mul*(a, b: Mat3): Mat3 =
  for i in 0 .. 2:
    for j in 0 .. 2:
      var s = -0.0 # Rust's float `sum` starts at -0.0
      for k in 0 .. 2: s += a[i][k] * b[k][j]
      result[i][j] = s

proc apply*(m: Mat3; v: array[3, float64]): array[3, float64] =
  for i in 0 .. 2: result[i] = m[i][0] * v[0] + m[i][1] * v[1] + m[i][2] * v[2]

proc diag*(v: array[3, float64]): Mat3 = [[v[0], 0.0, 0.0], [0.0, v[1], 0.0], [0.0, 0.0, v[2]]]

proc scale*(m: Mat3; k: float64): Mat3 =
  for i in 0 .. 2:
    for j in 0 .. 2: result[i][j] = m[i][j] * k

proc invert*(m: Mat3): Option[Mat3] =
  ## Inverse, or none when (nearly) singular or not finite.
  let (a, b, c) = (m[0][0], m[0][1], m[0][2])
  let (d, e, f) = (m[1][0], m[1][1], m[1][2])
  let (g, h, i) = (m[2][0], m[2][1], m[2][2])
  let det = a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
  if not isFin(det) or abs(det) < 1e-12: return
  let r = 1.0 / det
  let o: Mat3 = [[(e * i - f * h) * r, (c * h - b * i) * r, (b * f - c * e) * r],
                 [(f * g - d * i) * r, (a * i - c * g) * r, (c * d - a * f) * r],
                 [(d * h - e * g) * r, (b * g - a * h) * r, (a * e - b * d) * r]]
  for row in o:
    for v in row:
      if not isFin(v): return
  some(o)

proc fromSlice*(v: openArray[float64]): Option[Mat3] =
  ## A row-major 3×3 matrix from 9 values.
  if v.len < 9: return
  for k in 0 ..< 9:
    if not isFin(v[k]): return
  some([[v[0], v[1], v[2]], [v[3], v[4], v[5]], [v[6], v[7], v[8]]])

proc xyToXyz*(xy: array[2, float64]): array[3, float64] =
  let y = rmax(xy[1], 1e-6)
  [xy[0] / y, 1.0, (1.0 - xy[0] - xy[1]) / y]

proc xyzToXy*(v: array[3, float64]): Option[array[2, float64]] =
  let s = v[0] + v[1] + v[2]
  if isFin(s) and s > 1e-9: result = some([v[0] / s, v[1] / s])

proc cct*(xy: array[2, float64]): float64 =
  ## Correlated colour temperature from chromaticity (McCamy 1992); clamped to 2000–50000 K.
  let n = (xy[0] - 0.3320) / (0.1858 - xy[1])
  let t = 449.0 * (n * n * n) + 3525.0 * (n * n) + 6823.3 * n + 5520.33
  if isFin(t): clamp(t, 2000.0, 50000.0) else: 5000.0

proc illuminantTemperature*(code: uint32): Option[float64] =
  ## Approximate temperature of an EXIF `LightSource` code; none for unknown.
  case code
  of 17: some(2856.0)
  of 18: some(4874.0)
  of 19: some(6774.0)
  of 20: some(5503.0)
  of 21: some(6504.0)
  of 22: some(7504.0)
  of 23: some(5003.0)
  of 24: some(3200.0)
  of 3: some(2850.0)
  of 1, 4, 9: some(5500.0)
  of 10: some(6500.0)
  of 11: some(7500.0)
  of 12: some(6430.0)
  of 13: some(5000.0)
  of 14: some(4150.0)
  of 15: some(3525.0)
  of 16: some(2925.0)
  of 2: some(4200.0)
  else: none(float64)

proc bradford*(src, dst: array[3, float64]): Mat3 =
  ## Bradford chromatic adaptation from white `src` to white `dst` (XYZ).
  const M: Mat3 = [[0.8951, 0.2664, -0.1614], [-0.7502, 1.7135, 0.0367], [0.0389, -0.0685, 1.0296]]
  let mi = invert(M)
  if mi.isNone: return Identity
  let s = apply(M, src)
  let d = apply(M, dst)
  for v in s:
    if abs(v) < 1e-9: return Identity
  mul(mi.get, mul(diag([d[0] / s[0], d[1] / s[1], d[2] / s[2]]), M))

proc rgbToXyz*(prim: array[3, array[2, float64]]; white: array[2, float64]): Mat3 =
  ## RGB → XYZ matrix for primaries and a white point (chromaticities), white mapping to Y = 1.
  let cols = [xyToXyz(prim[0]), xyToXyz(prim[1]), xyToXyz(prim[2])]
  let p: Mat3 = [[cols[0][0], cols[1][0], cols[2][0]], [cols[0][1], cols[1][1], cols[2][1]], [cols[0][2], cols[1][2], cols[2][2]]]
  let pi = invert(p)
  if pi.isNone: return Identity
  let s = apply(pi.get, xyToXyz(white))
  mul(p, diag(s))

proc xyzD50ToProphoto*(): Mat3 =
  ## XYZ (D50) → linear ProPhoto RGB.
  invert(rgbToXyz(Romm, D50Xy)).get(Identity)

proc srgbToXyzD50*(): Mat3 =
  ## Linear sRGB (D65) → XYZ adapted to D50: the documented neutral camera matrix used when a
  ## file carries no colour calibration.
  mul(bradford(xyToXyz(D65Xy), xyToXyz(D50Xy)), rgbToXyz(Rec709, D65Xy))

proc lerpMat(a, b: Mat3; g: float64): Mat3 =
  for i in 0 .. 2:
    for j in 0 .. 2: result[i][j] = a[i][j] * g + b[i][j] * (1.0 - g)

proc weight(c: ColorInfo; t: float64): float64 =
  ## Weight of the first calibration at temperature `t` (inverse-temperature interpolation).
  if c.calibrations.len == 2 and abs(c.calibrations[0].temperature - c.calibrations[1].temperature) > 1.0:
    let (t1, t2) = (c.calibrations[0].temperature, c.calibrations[1].temperature)
    if t <= t1: 1.0
    elif t >= t2: 0.0
    else: clamp((1.0 / t - 1.0 / t2) / (1.0 / t1 - 1.0 / t2), 0.0, 1.0)
  else: 1.0

proc at(c: ColorInfo; t: float64): Option[(Mat3, Option[Mat3], Mat3)] =
  ## The interpolated `(AB · CC · CM, ForwardMatrix, AB · CC)` at temperature `t`.
  if c.calibrations.len == 0: return
  let first = c.calibrations[0]
  let g = c.weight(t)
  let second = c.calibrations.len > 1
  let cm = if second: lerpMat(first.colorMatrix, c.calibrations[1].colorMatrix, g) else: first.colorMatrix
  let cc = if second: lerpMat(first.cameraCalibration, c.calibrations[1].cameraCalibration, g) else: first.cameraCalibration
  var fm = none(Mat3)
  if first.forwardMatrix.isSome:
    if not second: fm = first.forwardMatrix
    elif c.calibrations[1].forwardMatrix.isSome:
      fm = some(lerpMat(first.forwardMatrix.get, c.calibrations[1].forwardMatrix.get, g))
  let ab = diag(c.analogBalance.get([1.0, 1.0, 1.0]))
  let abcc = mul(ab, cc)
  some((mul(abcc, cm), fm, abcc))

proc neutralToXy*(c: ColorInfo; neutral: array[3, float64]): array[2, float64] =
  ## Chromaticity of the white whose camera coordinates are `neutral`, by fixed-point iteration
  ## over the temperature-dependent matrix.
  var xy = D50Xy
  for _ in 0 ..< 30:
    let m = c.at(cct(xy))
    if m.isNone: return xy
    let camToXyz = invert(m.get[0])
    if camToXyz.isNone: return xy
    let next = xyzToXy(apply(camToXyz.get, neutral))
    if next.isNone: return xy
    let n = next.get
    if not (n[0] >= 0.0 and n[0] < 1.0) or not (n[1] >= 0.0 and n[1] < 1.0): return xy
    let done = abs(n[0] - xy[0]) + abs(n[1] - xy[1]) < 1e-7
    xy = n
    if done: break
  xy

proc maxOf(v: array[3, float64]): float64 =
  result = -F64Max
  for x in v: result = rmax(result, x)

proc xyToNeutral*(c: ColorInfo; xy: array[2, float64]): Option[array[3, float64]] =
  ## Camera coordinates of a white with chromaticity `xy`, max component 1.
  let m = c.at(cct(xy))
  if m.isNone: return
  let n = apply(m.get[0], xyToXyz(xy))
  let mx = maxOf(n)
  if mx > 1e-9 and n[0] > 0 and n[1] > 0 and n[2] > 0 and isFin(n[0]) and isFin(n[1]) and isFin(n[2]):
    result = some([n[0] / mx, n[1] / mx, n[2] / mx])

proc balancedToXyzD50*(c: ColorInfo; neutral: array[3, float64]): Option[Mat3] =
  ## The matrix from white-balanced camera values (camera values divided by
  ## `neutral / max(neutral)`) to XYZ D50, scaled so that the balanced white (1, 1, 1) maps to D50
  ## with Y = 1. None without calibrations.
  let nmax = maxOf(neutral)
  if nmax != nmax or nmax <= 0: return
  for v in neutral:
    if v != v or v <= 0: return
  let xy = c.neutralToXy(neutral)
  let m0 = c.at(cct(xy))
  if m0.isNone: return
  let (xyzToCam, fm, abcc) = m0.get
  let unbalance = diag([neutral[0] / nmax, neutral[1] / nmax, neutral[2] / nmax])
  var m: Mat3
  if fm.isSome:
    # CameraToXYZ_D50 = FM · D · Inverse(AB · CC), D = Invert(diag(ReferenceNeutral)).
    let abccInv = invert(abcc)
    if abccInv.isNone: return
    let reference = apply(abccInv.get, neutral)
    for v in reference:
      if v != v or v <= 0: return
    let d = diag([1.0 / reference[0], 1.0 / reference[1], 1.0 / reference[2]])
    m = mul(mul(fm.get, mul(d, abccInv.get)), unbalance)
  else:
    let camToXyz = invert(xyzToCam)
    if camToXyz.isNone: return
    let adapt = bradford(xyToXyz(xy), D50)
    m = mul(mul(adapt, camToXyz.get), unbalance)
  # Normalize: balanced white → Y = 1.
  let y = apply(m, [1.0, 1.0, 1.0])[1]
  if isFin(y) and y > 1e-9: result = some(scale(m, 1.0 / y))

proc sortCalibrations*(cal: var seq[Calibration]) =
  ## Sorted by temperature (stable, like the Rust `sort_by` with `total_cmp`).
  cal.sort(proc(a, b: Calibration): int = cmp(a.temperature, b.temperature))
