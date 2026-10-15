## Colour conversions between the document colour models.
##
## CMYK ↔ RGB goes through the ICC colour management module (`photocraft/cms`) with the
## built-in coated CMYK profile and sRGB (relative colorimetric with black point
## compensation, Photoshop's default). A document's embedded CMYK profile replaces the built-in
## one while its `CmykSpace` is active on the thread (`withCmykSpace`). Lab uses the exact D50
## formulas; gray is treated as sGray (gray `v` = sRGB `(v, v, v)`).

import std/[math, options, hashes]
import ../rmath
import ../cms

type
  CmykSpace* = ref object
    ## A document's own CMYK profile, linked to sRGB both ways (relative colorimetric + BPC).
    toSrgb, fromSrgb: Transform
    id*: uint64 ## content hash of the profile

  IccRef* = ref seq[byte] ## Shared ICC profile bytes (identity is the cache key).

var activeCmyk {.threadvar.}: CmykSpace
var spaceCache: seq[(IccRef, CmykSpace)]
var cmykToSrgbT, srgbToCmykT: Transform
var builtinTried: bool

proc buildSpace(bytes: seq[byte]): CmykSpace =
  var p: Profile
  try: p = parseProfile(bytes)
  except CatchableError: return nil
  if p.colorSpace.kind != csCmyk: return nil
  let coated = bCoatedCmyk.profile
  if p.contentHash == coated.contentHash: return nil
  let s = srgb()
  try:
    CmykSpace(toSrgb: newTransform(p, s, RelativeColorimetric, true),
              fromSrgb: newTransform(s, p, RelativeColorimetric, true), id: p.contentHash)
  except CatchableError: nil

proc cmykSpaceFor*(icc: IccRef): CmykSpace =
  ## The space of a document's embedded profile bytes: nil when there are none, they do not
  ## parse as a CMYK profile, or they are the built-in coated CMYK. Cached by identity.
  if icc.isNil: return nil
  {.cast(gcsafe).}:
    for (k, s) in spaceCache:
      if k == icc: return s
    let s = buildSpace(icc[])
    if spaceCache.len > 16: spaceCache.delete(0)
    spaceCache.add (icc, s)
    s

proc activeCmykSpace*(): CmykSpace = activeCmyk
proc setActiveCmykSpace*(s: CmykSpace) = activeCmyk = s

template withCmykSpace*(space: CmykSpace; body: untyped) =
  ## Runs `body` with `space` as this thread's CMYK profile (nil: the built-in coated CMYK).
  let prevSpace = activeCmykSpace()
  setActiveCmykSpace(space)
  try:
    body
  finally:
    setActiveCmykSpace(prevSpace)


proc ensureBuiltin() =
  {.cast(gcsafe).}:
    if not builtinTried:
      builtinTried = true
      try:
        cmykToSrgbT = newTransform(bCoatedCmyk.profile, srgb(), RelativeColorimetric, true)
        srgbToCmykT = newTransform(srgb(), bCoatedCmyk.profile, RelativeColorimetric, true)
      except CatchableError: discard

proc cmykToRgbNaive*(c: array[4, float32]): array[3, float32] =
  ## Naive CMYK → RGB (no profile): `(1 − c)(1 − k)`.
  let k = 1.0'f32 - c[3]
  [(1.0'f32 - c[0]) * k, (1.0'f32 - c[1]) * k, (1.0'f32 - c[2]) * k]

proc rgbToCmykNaive*(rgb: array[3, float32]): array[4, float32] =
  ## Naive RGB → CMYK with full GCR.
  let k = 1.0'f32 - max(rgb[0], max(rgb[1], rgb[2]))
  if k >= 1.0'f32 - 1e-6'f32: return [0.0'f32, 0, 0, 1]
  let d = 1.0'f32 - k
  [(1.0'f32 - rgb[0] - k) / d, (1.0'f32 - rgb[1] - k) / d, (1.0'f32 - rgb[2] - k) / d, k]

proc cmykToRgb*(c: array[4, float32]): array[3, float32] =
  ## Colour-managed CMYK → sRGB (active document CMYK profile, else the built-in coated CMYK).
  if not activeCmyk.isNil:
    activeCmyk.toSrgb.evalFast(c, result)
    return
  ensureBuiltin()
  {.cast(gcsafe).}:
    if cmykToSrgbT.isNil: return cmykToRgbNaive(c)
    cmykToSrgbT.evalFast(c, result)

proc rgbToCmyk*(rgb: array[3, float32]): array[4, float32] =
  ## Colour-managed sRGB → CMYK.
  let v = [clamp(rgb[0], 0.0'f32, 1.0'f32), clamp(rgb[1], 0.0'f32, 1.0'f32), clamp(rgb[2], 0.0'f32, 1.0'f32)]
  if not activeCmyk.isNil:
    activeCmyk.fromSrgb.evalFast(v, result)
    return
  ensureBuiltin()
  {.cast(gcsafe).}:
    if srgbToCmykT.isNil: return rgbToCmykNaive(rgb)
    srgbToCmykT.evalFast(v, result)

proc srgbToLinear*(v: float32): float32 {.inline.} =
  if v <= 0.04045'f32: v / 12.92'f32 else: pow((v + 0.055'f32) / 1.055'f32, 2.4'f32)

proc linearToSrgb*(v: float32): float32 {.inline.} =
  if v <= 0.0031308'f32: v * 12.92'f32 else: 1.055'f32 * pow(v, 1.0'f32 / 2.4'f32) - 0.055'f32

const
  D50f*: array[3, float32] = [0.96422'f32, 1.0, 0.82521]
  SrgbToXyzD50*: array[3, array[3, float32]] = [[0.436074'f32, 0.385064, 0.143080], [0.222504'f32, 0.716878, 0.060618], [0.013932'f32, 0.097104, 0.714173]]
  XyzD50ToSrgb*: array[3, array[3, float32]] = [[3.133856'f32, -1.616867, -0.490615], [-0.978768'f32, 1.916142, 0.033454], [0.071945'f32, -0.228991, 1.405243]]

proc mat3Mul*(m: array[3, array[3, float32]]; v: array[3, float32]): array[3, float32] {.inline.} =
  for i in 0..2: result[i] = m[i][0] * v[0] + m[i][1] * v[1] + m[i][2] * v[2]

proc labToSrgb*(lab: array[3, float32]): array[3, float32] =
  ## CIE L*a*b* (D50) → encoded sRGB (Bradford-adapted to D65).
  let fy = (lab[0] + 16.0'f32) / 116.0'f32
  let fx = fy + lab[1] / 500.0'f32
  let fz = fy - lab[2] / 200.0'f32
  proc finv(t: float32): float32 =
    if t > 6.0'f32 / 29.0'f32: t * t * t else: 3.0'f32 * pow(6.0'f32 / 29.0'f32, 2.0'f32) * (t - 4.0'f32 / 29.0'f32)
  let xyz = [finv(fx) * D50f[0], finv(fy) * D50f[1], finv(fz) * D50f[2]]
  let lin = mat3Mul(XyzD50ToSrgb, xyz)
  for i in 0..2: result[i] = linearToSrgb(clamp(lin[i], 0.0'f32, 1.0'f32))

proc srgbToLab*(rgb: array[3, float32]): array[3, float32] =
  ## Encoded sRGB → CIE L*a*b* (D50).
  let lin = [srgbToLinear(rgb[0]), srgbToLinear(rgb[1]), srgbToLinear(rgb[2])]
  let xyz = mat3Mul(SrgbToXyzD50, lin)
  proc f(t: float32): float32 =
    if t > pow(6.0'f32 / 29.0'f32, 3.0'f32): rmath.cbrt(t) else: t / (3.0'f32 * pow(6.0'f32 / 29.0'f32, 2.0'f32)) + 4.0'f32 / 29.0'f32
  let fx = f(xyz[0] / D50f[0])
  let fy = f(xyz[1] / D50f[1])
  let fz = f(xyz[2] / D50f[2])
  [116.0'f32 * fy - 16.0'f32, 500.0'f32 * (fx - fy), 200.0'f32 * (fy - fz)]

proc rgbToGray*(rgb: array[3, float32]): float32 =
  ## Rec.601 luma used by Photoshop's Grayscale conversion default.
  0.299'f32 * rgb[0] + 0.587'f32 * rgb[1] + 0.114'f32 * rgb[2]
