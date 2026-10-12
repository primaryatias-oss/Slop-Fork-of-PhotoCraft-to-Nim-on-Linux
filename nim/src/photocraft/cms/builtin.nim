## Built-in profiles. All are generated from published colorimetric definitions (primaries,
## white points, transfer functions) and dedicated to the public domain (CC0-1.0), so documents
## that embed them carry no licence obligations.

import std/[options, strutils]
import ./curve, ./math as cmath, ./pipeline, ./profile, ./write, ./synth

const BuiltinCopyright* = synth.Copyright

type
  Builtin* = enum
    bSrgb, bDisplayP3, bAdobeRgbCompat, bProPhotoCompat, bLinearSrgb, bRec2020, bGrayGamma22,
    bSGray, bLabD50, bCoatedCmyk

const CoatedCmykIcc* = staticRead("../../../assets/profiles/photocraft-coated-cmyk.icc")

proc id*(b: Builtin): string =
  case b
  of bSrgb: "srgb"
  of bDisplayP3: "display-p3"
  of bAdobeRgbCompat: "adobe-rgb-compat"
  of bProPhotoCompat: "prophoto-compat"
  of bLinearSrgb: "linear-srgb"
  of bRec2020: "rec2020"
  of bGrayGamma22: "gray-gamma-2.2"
  of bSGray: "sgray"
  of bLabD50: "lab-d50"
  of bCoatedCmyk: "coated-cmyk"

proc description*(b: Builtin): string =
  case b
  of bSrgb: "sRGB IEC61966-2.1"
  of bDisplayP3: "Display P3"
  of bAdobeRgbCompat: "Adobe RGB (1998)-compatible (Photocraft)"
  of bProPhotoCompat: "ProPhoto RGB-compatible (ROMM primaries, Photocraft)"
  of bLinearSrgb: "Linear sRGB (Photocraft)"
  of bRec2020: "Rec. ITU-R BT.2020 (Photocraft)"
  of bGrayGamma22: "Gray Gamma 2.2 (Photocraft)"
  of bSGray: "sGray (sRGB tone curve, Photocraft)"
  of bLabD50: "Lab D50 identity (Photocraft)"
  of bCoatedCmyk: synth.Description

proc builtinFromId*(s: string): Option[Builtin] =
  ## Looks up an id (case-insensitive; a few aliases and the descriptions are accepted).
  var n = ""
  for c in s.toLowerAscii:
    if c.isAlphaNumeric: n.add c
  case n
  of "srgb", "srgbiec6196621", "srgbiec61966": return some(bSrgb)
  of "p3", "displayp3": return some(bDisplayP3)
  of "adobergb", "adobergb1998", "adobergbcompat", "clayrgb": return some(bAdobeRgbCompat)
  of "prophoto", "prophotorgb", "prophotocompat", "romm", "rommrgb": return some(bProPhotoCompat)
  of "linearsrgb", "srgblinear", "linear": return some(bLinearSrgb)
  of "rec2020", "bt2020": return some(bRec2020)
  of "gray", "graygamma22", "gamma22": return some(bGrayGamma22)
  of "sgray", "graysrgb": return some(bSGray)
  of "lab", "labd50", "cielab": return some(bLabD50)
  of "cmyk", "coatedcmyk", "defaultcmyk", "photocraftcoatedcmyk": return some(bCoatedCmyk)
  else: discard
  for b in Builtin:
    if cmpIgnoreCase(b.description, s) == 0: return some(b)
  none(Builtin)

proc d65(): Vec3 = xyToXyz(D65xy)

proc colorants(r, g, b: array[2, float64]; white: Vec3): (Mat3, Option[Mat3]) =
  ## Colorant matrix adapted to D50 and quantized to s15Fixed16, rows nudged so RGB (1,1,1)
  ## maps exactly to the stored D50 white.
  let m = rgbToXyzMatrix(r, g, b, white).get(Identity3)
  let d50 = d50Quantized()
  var adapt = none(Mat3)
  if not (abs(white[0] - D50[0]) < 1e-3 and abs(white[2] - D50[2]) < 1e-3):
    adapt = some(bradford(white, D50))
  var m50: Mat3
  if adapt.isSome: m50 = mul(adapt.get, m)
  else:
    let s = apply(m, [1.0, 1.0, 1.0])
    m50 = m
    for i in 0..2:
      for j in 0..2: m50[i][j] *= D50[i] / s[i]
  var q: Mat3
  for i in 0..2:
    for j in 0..2: q[i][j] = s15f16Round(m50[i][j])
  for i in 0..2:
    let err = d50[i] - (q[i][0] + q[i][1] + q[i][2])
    var k = 0
    for j in 1..2:
      if abs(q[i][j]) > abs(q[i][k]): k = j
    q[i][k] = s15f16Round(q[i][k] + err)
  var chad = none(Mat3)
  if adapt.isSome:
    var a = adapt.get
    for i in 0..2:
      for j in 0..2: a[i][j] = s15f16Round(a[i][j])
    chad = some(a)
  (q, chad)

proc rgbProfile(desc: string; prim: array[3, array[2, float64]]; white: Vec3; trc: Curve): Profile =
  let (m, chad) = colorants(prim[0], prim[1], prim[2], white)
  Profile(version: (4'u8, 0x30'u8), class: profileClass(pcDisplay), colorSpace: colorSpace(csRgb), pcs: pcsXyz,
          renderingIntent: Perceptual, description: desc, copyright: BuiltinCopyright,
          whitePoint: d50Quantized(), chad: chad, matrix: some(m), trc: some([trc, trc, trc])).withEncodedBytes

proc grayProfile*(desc: string; trc: Curve): Profile =
  Profile(version: (4'u8, 0x30'u8), class: profileClass(pcDisplay), colorSpace: colorSpace(csGray), pcs: pcsXyz,
          renderingIntent: Perceptual, description: desc, copyright: BuiltinCopyright,
          whitePoint: d50Quantized(), grayTrc: some(trc)).withEncodedBytes

proc labProfile(): Profile =
  let ident = Lut(kind: lkAb, inputs: 3, outputs: 3,
                  stages: @[curvesStage(@[identityCurve(), identityCurve(), identityCurve()])])
  Profile(version: (4'u8, 0x30'u8), class: profileClass(pcColorSpace), colorSpace: colorSpace(csLab), pcs: pcsLab,
          renderingIntent: Perceptual, description: bLabD50.description, copyright: BuiltinCopyright,
          whitePoint: d50Quantized(), a2b: [some(ident), none(Lut), none(Lut)],
          b2a: [some(ident), none(Lut), none(Lut)]).withEncodedBytes

proc build(b: Builtin): Profile =
  const
    srgbP = [[0.64, 0.33], [0.30, 0.60], [0.15, 0.06]]
    p3 = [[0.680, 0.320], [0.265, 0.690], [0.150, 0.060]]
    adobe = [[0.64, 0.33], [0.21, 0.71], [0.15, 0.06]]
    romm = [[0.7347, 0.2653], [0.1596, 0.8404], [0.0366, 0.0001]]
    bt2020 = [[0.708, 0.292], [0.170, 0.797], [0.131, 0.046]]
  case b
  of bSrgb: rgbProfile(b.description, srgbP, d65(), srgbTrc())
  of bDisplayP3: rgbProfile(b.description, p3, d65(), srgbTrc())
  of bAdobeRgbCompat: rgbProfile(b.description, adobe, d65(), gammaCurve(563.0 / 256.0))
  of bProPhotoCompat: rgbProfile(b.description, romm, D50, gammaCurve(1.8))
  of bLinearSrgb: rgbProfile(b.description, srgbP, d65(), gammaCurve(1.0))
  of bRec2020:
    let a = 1.099
    rgbProfile(b.description, bt2020, d65(), paraCurve(3, [1.0 / 0.45, 1.0 / a, (a - 1.0) / a, 1.0 / 4.5, 0.081, 0.0, 0.0]))
  of bGrayGamma22: grayProfile(b.description, gammaCurve(2.2))
  of bSGray: grayProfile(b.description, srgbTrc())
  of bLabD50: labProfile()
  of bCoatedCmyk:
    try: parseProfile(cast[seq[byte]](CoatedCmykIcc))
    except CatchableError: coatedCmyk()

var cells: array[Builtin, Profile]
var linearGrayCell: Profile

proc profile*(b: Builtin): Profile =
  ## The profile (built once per process).
  {.cast(gcsafe).}:
    if cells[b].isNil: cells[b] = build(b)
    cells[b]

proc defaultFor*(cs: ColorSpace): Option[Profile] =
  ## The default profile for a colour space: sRGB, sGray, the coated CMYK profile, Lab D50.
  case cs.kind
  of csRgb: some(bSrgb.profile)
  of csGray: some(bSGray.profile)
  of csCmyk: some(bCoatedCmyk.profile)
  of csLab: some(bLabD50.profile)
  else: none(Profile)

proc srgb*(): Profile = bSrgb.profile

proc linearGray*(): Profile =
  ## Gray with a linear tone curve (what a grayscale OpenEXR holds).
  {.cast(gcsafe).}:
    if linearGrayCell.isNil: linearGrayCell = grayProfile("Linear Gray (Photocraft)", gammaCurve(1.0))
    linearGrayCell
