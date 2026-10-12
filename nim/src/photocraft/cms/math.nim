## Small colour-science helpers: 3×3 matrices, CIE XYZ ↔ L*a*b*, chromatic adaptation.
##
## All formulas are from public standards: CIE 15 (L*a*b*), ICC.1:2010 Annex A (PCS, D50),
## and the Bradford cone response matrix published by Lam (1985).

import std/[math, options]

type
  Vec3* = array[3, float64]
  Mat3* = array[3, array[3, float64]] ## Row-major 3×3 matrix.

const
  D50*: Vec3 = [0.9642, 1.0, 0.8249]   ## ICC PCS illuminant (D50).
  D65xy*: array[2, float64] = [0.3127, 0.3290]
  Identity3*: Mat3 = [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]

proc mul*(a, b: Mat3): Mat3 =
  for i in 0..2:
    for j in 0..2:
      result[i][j] = a[i][0] * b[0][j] + a[i][1] * b[1][j] + a[i][2] * b[2][j]

proc apply*(m: Mat3; v: Vec3): Vec3 =
  [m[0][0] * v[0] + m[0][1] * v[1] + m[0][2] * v[2],
   m[1][0] * v[0] + m[1][1] * v[1] + m[1][2] * v[2],
   m[2][0] * v[0] + m[2][1] * v[1] + m[2][2] * v[2]]

proc det*(m: Mat3): float64 =
  m[0][0] * (m[1][1] * m[2][2] - m[1][2] * m[2][1]) - m[0][1] * (m[1][0] * m[2][2] - m[1][2] * m[2][0]) +
    m[0][2] * (m[1][0] * m[2][1] - m[1][1] * m[2][0])

proc isFiniteF*(v: float64): bool {.inline.} = classify(v) notin {fcNan, fcInf, fcNegInf}
proc isFiniteF*(v: float32): bool {.inline.} = classify(v) notin {fcNan, fcInf, fcNegInf}

proc invert*(m: Mat3): Option[Mat3] =
  ## Inverse, or none when (nearly) singular.
  let d = det(m)
  if not isFiniteF(d) or abs(d) < 1e-12: return none(Mat3)
  let inv = 1.0 / d
  some [
    [(m[1][1] * m[2][2] - m[1][2] * m[2][1]) * inv, (m[0][2] * m[2][1] - m[0][1] * m[2][2]) * inv, (m[0][1] * m[1][2] - m[0][2] * m[1][1]) * inv],
    [(m[1][2] * m[2][0] - m[1][0] * m[2][2]) * inv, (m[0][0] * m[2][2] - m[0][2] * m[2][0]) * inv, (m[0][2] * m[1][0] - m[0][0] * m[1][2]) * inv],
    [(m[1][0] * m[2][1] - m[1][1] * m[2][0]) * inv, (m[0][1] * m[2][0] - m[0][0] * m[2][1]) * inv, (m[0][0] * m[1][1] - m[0][1] * m[1][0]) * inv]]

proc xyToXyz*(xy: array[2, float64]): Vec3 =
  ## XYZ (Y = 1) of a chromaticity `(x, y)`.
  [xy[0] / xy[1], 1.0, (1.0 - xy[0] - xy[1]) / xy[1]]

const Bradford: Mat3 = [[0.8951, 0.2664, -0.1614], [-0.7502, 1.7135, 0.0367], [0.0389, -0.0685, 1.0296]]

proc bradford*(src, dst: Vec3): Mat3 =
  ## Bradford chromatic adaptation matrix from white `src` to white `dst` (both XYZ).
  let inv = invert(Bradford).get(Identity3) # constant, determinant ~1.7
  let s = apply(Bradford, src)
  let d = apply(Bradford, dst)
  let scale: Mat3 = [[d[0] / s[0], 0.0, 0.0], [0.0, d[1] / s[1], 0.0], [0.0, 0.0, d[2] / s[2]]]
  mul(inv, mul(scale, Bradford))

proc rgbToXyzMatrix*(r, g, b: array[2, float64]; white: Vec3): Option[Mat3] =
  ## RGB → XYZ matrix (relative to `white`) for primaries given as chromaticities.
  let xr = xyToXyz(r)
  let xg = xyToXyz(g)
  let xb = xyToXyz(b)
  let m: Mat3 = [[xr[0], xg[0], xb[0]], [xr[1], xg[1], xb[1]], [xr[2], xg[2], xb[2]]]
  let inv = invert(m)
  if inv.isNone: return none(Mat3)
  let s = apply(inv.get, white)
  some [[m[0][0] * s[0], m[0][1] * s[1], m[0][2] * s[2]],
        [m[1][0] * s[0], m[1][1] * s[1], m[1][2] * s[2]],
        [m[2][0] * s[0], m[2][1] * s[1], m[2][2] * s[2]]]

const
  Eps = 216.0 / 24389.0
  Kappa = 24389.0 / 27.0

proc labF(t: float64): float64 {.inline.} =
  if t > Eps: cbrt(t) else: (Kappa * t + 16.0) / 116.0

proc labFinv(f: float64): float64 {.inline.} =
  let f3 = f * f * f
  if f3 > Eps: f3 else: (116.0 * f - 16.0) / Kappa

proc xyzToLab*(xyz, white: Vec3): Vec3 =
  ## CIE XYZ → L*a*b* relative to `white` (CIE 15).
  let fx = labF(xyz[0] / white[0])
  let fy = labF(xyz[1] / white[1])
  let fz = labF(xyz[2] / white[2])
  [116.0 * fy - 16.0, 500.0 * (fx - fy), 200.0 * (fy - fz)]

proc labToXyz*(lab, white: Vec3): Vec3 =
  let fy = (lab[0] + 16.0) / 116.0
  let fx = fy + lab[1] / 500.0
  let fz = fy - lab[2] / 200.0
  [labFinv(fx) * white[0], labFinv(fy) * white[1], labFinv(fz) * white[2]]

proc xyzToLabF32*(xyz: array[3, float32]): array[3, float32] {.inline.} =
  const W = [0.9642'f32, 1.0, 0.8249]
  const E = float32(216.0 / 24389.0)
  const K = float32(24389.0 / 27.0)
  template f(t: float32): float32 = (if t > E: cbrt(t) else: (K * t + 16.0'f32) / 116.0'f32)
  let fx = f(xyz[0] / W[0])
  let fy = f(xyz[1] / W[1])
  let fz = f(xyz[2] / W[2])
  [116.0'f32 * fy - 16.0'f32, 500.0'f32 * (fx - fy), 200.0'f32 * (fy - fz)]

proc labToXyzF32*(lab: array[3, float32]): array[3, float32] {.inline.} =
  const W = [0.9642'f32, 1.0, 0.8249]
  const E = float32(216.0 / 24389.0)
  const K = float32(24389.0 / 27.0)
  let fy = (lab[0] + 16.0'f32) / 116.0'f32
  let fx = fy + lab[1] / 500.0'f32
  let fz = fy - lab[2] / 200.0'f32
  template finv(f: float32): float32 =
    (let f3 = f * f * f; if f3 > E: f3 else: (116.0'f32 * f - 16.0'f32) / K)
  [finv(fx) * W[0], finv(fy) * W[1], finv(fz) * W[2]]

proc deltaE76*(a, b: Vec3): float64 =
  sqrt((a[0] - b[0]) ^ 2 + (a[1] - b[1]) ^ 2 + (a[2] - b[2]) ^ 2)

proc s15f16Round*(v: float64): float64 =
  ## Quantizes to ICC s15Fixed16Number and back.
  round(v * 65536.0) / 65536.0

proc d50Quantized*(): Vec3 = [s15f16Round(D50[0]), s15f16Round(D50[1]), s15f16Round(D50[2])]
