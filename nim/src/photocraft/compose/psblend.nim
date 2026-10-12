## Photoshop-exact blend functions where they differ from the generic ones in `color/blend`
## (Vivid Light, Hard Mix, Color Burn/Dodge edge cases), the compositing formulas (including
## Photoshop's Fill behaviour for its "special eight" modes), Lab mixing for Lab documents, and
## the text blending gamma.

import std/math
import ../color/blend as generic, ../color/convert
export generic.BlendMode

const
  Edge* = 1e-4'f32
  TextGammaDefault* = 1.45'f32

var labMix* {.threadvar.}: bool   ## Normal blending mixes in CIELAB (Lab documents)
var hdrBlend* {.threadvar.}: bool ## 32-bit documents: Add/Divide and HSL modes don't clip at 1
var textGammaSetting = TextGammaDefault

template withBlendSpace*(lab, hdr: bool; body: untyped) =
  let prevLab = labMix
  let prevHdr = hdrBlend
  labMix = lab
  hdrBlend = hdr
  try: body
  finally:
    labMix = prevLab
    hdrBlend = prevHdr

proc vividLightPs(cb, cs: float32): float32 =
  if cs <= 0.5: (if cs <= 0: 0'f32 else: 1 - min((1 - cb) / (2 * cs), 1'f32))
  elif cs >= 1: 1
  else: min(cb / (2 * (1 - cs)), 1'f32)

proc colorBurnPs(cb, cs: float32): float32 =
  if cb >= 1 - Edge: 1 elif cs <= 0: 0 else: 1 - min((1 - cb) / cs, 1'f32)

proc colorDodgePs(cb, cs: float32): float32 =
  if cb <= Edge: 0 elif cs >= 1: 1 else: min(cb / (1 - cs), 1'f32)

proc vividLightGeneric(cb, cs: float32): float32 =
  if cs <= 0.5:
    if cb >= 1 - Edge: 1 elif cs <= 0: 0 else: 1 - min((1 - cb) / (2 * cs), 1'f32)
  elif cb <= Edge: 0
  elif cs >= 1: 1
  else: min(cb / (2 * (1 - cs)), 1'f32)

proc hardMixPs(cb, cs: float32): float32 = (if vividLightGeneric(cb, cs) >= 0.5 - 1e-6: 1 else: 0)

proc blendRgbPs*(mode: BlendMode; cb, cs: array[3, float32]): array[3, float32] =
  ## `B(Cb, Cs)` with Photoshop's variants.
  case mode
  of bmVividLight:
    for i in 0..2: result[i] = vividLightPs(cb[i], cs[i])
  of bmHardMix:
    for i in 0..2: result[i] = hardMixPs(cb[i], cs[i])
  of bmColorBurn:
    for i in 0..2: result[i] = colorBurnPs(cb[i], cs[i])
  of bmColorDodge:
    for i in 0..2: result[i] = colorDodgePs(cb[i], cs[i])
  else:
    result = if hdrBlend: blendRgbHdr(mode, cb, cs) else: blendRgb(mode, cb, cs)

proc fillIsSpecial*(mode: BlendMode): bool =
  ## Photoshop's "special eight": below 100% Fill they blend a source pulled toward the mode's
  ## neutral colour instead of using Fill as coverage.
  mode in {bmColorBurn, bmLinearBurn, bmColorDodge, bmLinearDodge, bmVividLight, bmLinearLight, bmHardMix, bmDifference}

proc blendRgbFill*(mode: BlendMode; cb, cs: array[3, float32]; fill: float32): array[3, float32] =
  let f = clamp(fill, 0'f32, 1'f32)
  if f >= 1 or not fillIsSpecial(mode): return blendRgbPs(mode, cb, cs)
  proc toward(neutral: float32): array[3, float32] =
    for i in 0..2: result[i] = neutral + (cs[i] - neutral) * f
  case mode
  of bmHardMix:
    for i in 0..2: result[i] = clamp((cb[i] - f * (1 - cs[i])) / (1 - f), 0'f32, 1'f32)
  of bmColorBurn, bmLinearBurn: result = blendRgbPs(mode, cb, toward(1))
  of bmVividLight, bmLinearLight: result = blendRgbPs(mode, cb, toward(0.5))
  else: result = blendRgbPs(mode, cb, toward(0))

proc setTextGamma*(gamma: float32) =
  ## Color Settings › "Blend Text Colors Using Gamma" (1 = off), clamped to 1.00–2.20.
  textGammaSetting = if gamma == gamma and abs(gamma) != Inf: clamp(gamma, 1'f32, 2.2'f32) else: TextGammaDefault
proc textGamma*(): float32 = textGammaSetting

proc textEncode*(v, gamma: float32): float32 {.inline.} = pow(srgbToLinear(max(v, 0)), 1 / gamma)
proc textDecode*(v, gamma: float32): float32 {.inline.} = linearToSrgb(pow(max(v, 0), gamma))

proc compositePs*(mode: BlendMode; backdrop, source: array[4, float32]; opacity: float32): array[4, float32] =
  ## Straight-alpha source over straight-alpha backdrop (W3C/PDF general formula).
  let ab = backdrop[3]
  let asrc = source[3] * opacity
  if asrc <= 0: return backdrop
  let ao = asrc + ab * (1 - asrc)
  if mode == bmNormal and labMix and ab > 0 and ao > 0:
    let lb = srgbToLab([backdrop[0], backdrop[1], backdrop[2]])
    let ls = srgbToLab([source[0], source[1], source[2]])
    let kb = ab * (1 - asrc) / ao
    let k = asrc / ao
    var m: array[3, float32]
    for i in 0..2: m[i] = lb[i] * kb + ls[i] * k
    let r = labToSrgb(m)
    return [r[0], r[1], r[2], ao]
  if mode == bmNormal:
    if ao <= 0: return
    let kb = ab * (1 - asrc)
    let inv = 1 / ao
    return [(kb * backdrop[0] + asrc * source[0]) * inv, (kb * backdrop[1] + asrc * source[1]) * inv,
            (kb * backdrop[2] + asrc * source[2]) * inv, ao]
  let cb = [backdrop[0], backdrop[1], backdrop[2]]
  let cs = [source[0], source[1], source[2]]
  let b = blendRgbPs(mode, cb, cs)
  if ao <= 0: return
  for i in 0..2: result[i] = ((1 - asrc) * ab * cb[i] + (1 - ab) * asrc * cs[i] + asrc * ab * b[i]) / ao
  result[3] = ao

proc compositeGamma*(mode: BlendMode; backdrop, source: array[4, float32]; opacity, gamma: float32): array[4, float32] =
  ## `compositePs` mixing coverage in the text blending space of `gamma` (1 = plain).
  if gamma == 1: return compositePs(mode, backdrop, source, opacity)
  let ab = backdrop[3]
  let asrc = source[3] * opacity
  if asrc <= 0: return backdrop
  let ao = asrc + ab * (1 - asrc)
  if ao <= 0: return
  let cb = [backdrop[0], backdrop[1], backdrop[2]]
  let cs = [source[0], source[1], source[2]]
  let b = blendRgbPs(mode, cb, cs)
  let (wb, ws, wbs) = ((1 - asrc) * ab / ao, (1 - ab) * asrc / ao, asrc * ab / ao)
  for i in 0..2:
    result[i] = textDecode(wb * textEncode(cb[i], gamma) + ws * textEncode(cs[i], gamma) + wbs * textEncode(b[i], gamma), gamma)
  result[3] = ao

proc compositeFill*(mode: BlendMode; backdrop, source: array[4, float32]; opacity, fill, gamma: float32): array[4, float32] =
  ## A layer with separate `opacity` and `fill` (Fill is part of the blend for the special eight).
  if fill >= 1 or not fillIsSpecial(mode):
    return compositeGamma(mode, backdrop, source, opacity * clamp(fill, 0'f32, 1'f32), gamma)
  let ab = backdrop[3]
  let over = source[3] * opacity
  let alone = over * clamp(fill, 0'f32, 1'f32)
  if over <= 0: return backdrop
  let ao = ab + (1 - ab) * alone
  if ao <= 0: return
  let cb = [backdrop[0], backdrop[1], backdrop[2]]
  let cs = [source[0], source[1], source[2]]
  let b = blendRgbFill(mode, cb, cs, fill)
  let (wb, wbs, ws) = (ab * (1 - over) / ao, ab * over / ao, (1 - ab) * alone / ao)
  for i in 0..2:
    result[i] = if gamma == 1: wb * cb[i] + wbs * b[i] + ws * cs[i]
                else: textDecode(wb * textEncode(cb[i], gamma) + wbs * textEncode(b[i], gamma) + ws * textEncode(cs[i], gamma), gamma)
  result[3] = ao
