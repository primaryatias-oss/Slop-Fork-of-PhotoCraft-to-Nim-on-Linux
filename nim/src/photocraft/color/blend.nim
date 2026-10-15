## Photoshop and Paint.NET blend modes (scalar reference implementation).
##
## Formulas follow the public PDF / ISO 32000-2 and W3C Compositing definitions, with
## Photoshop's variants where they differ (Soft Light, Hard Mix, Darker/Lighter Color, Linear
## Burn/Dodge, Vivid/Linear/Pin Light, Subtract, Divide).

import std/[math, options, algorithm]
import ../rmath

type
  BlendMode* = enum
    bmPassThrough = "PassThrough", bmNormal = "Normal", bmDissolve = "Dissolve", bmDarken = "Darken",
    bmMultiply = "Multiply", bmColorBurn = "ColorBurn", bmLinearBurn = "LinearBurn",
    bmDarkerColor = "DarkerColor", bmLighten = "Lighten", bmScreen = "Screen",
    bmColorDodge = "ColorDodge", bmLinearDodge = "LinearDodge", bmLighterColor = "LighterColor",
    bmOverlay = "Overlay", bmSoftLight = "SoftLight", bmHardLight = "HardLight",
    bmVividLight = "VividLight", bmLinearLight = "LinearLight", bmPinLight = "PinLight",
    bmHardMix = "HardMix", bmDifference = "Difference", bmExclusion = "Exclusion",
    bmSubtract = "Subtract", bmDivide = "Divide", bmHue = "Hue", bmSaturation = "Saturation",
    bmColor = "Color", bmLuminosity = "Luminosity", bmReflect = "Reflect", bmGlow = "Glow",
    bmNegation = "Negation", bmXor = "Xor", bmPaintNetColorBurn = "PaintNetColorBurn",
    bmPaintNetColorDodge = "PaintNetColorDodge"

const
  PaintNetModes*: array[6, BlendMode] = [bmReflect, bmGlow, bmNegation, bmXor, bmPaintNetColorBurn, bmPaintNetColorDodge]
  LayerModes*: array[27, BlendMode] = [bmNormal, bmDissolve, bmDarken, bmMultiply, bmColorBurn,
    bmLinearBurn, bmDarkerColor, bmLighten, bmScreen, bmColorDodge, bmLinearDodge, bmLighterColor,
    bmOverlay, bmSoftLight, bmHardLight, bmVividLight, bmLinearLight, bmPinLight, bmHardMix,
    bmDifference, bmExclusion, bmSubtract, bmDivide, bmHue, bmSaturation, bmColor, bmLuminosity]

proc layerModes*(): seq[BlendMode] =
  ## All editable layer modes, with Paint.NET modes after Photoshop's menu order.
  for m in LayerModes: result.add m
  for m in PaintNetModes: result.add m

proc hasPsdEquivalent*(m: BlendMode): bool = m notin PaintNetModes

proc label*(m: BlendMode): string =
  case m
  of bmPassThrough: "Pass Through"
  of bmNormal: "Normal"
  of bmDissolve: "Dissolve"
  of bmDarken: "Darken"
  of bmMultiply: "Multiply"
  of bmColorBurn: "Color Burn"
  of bmLinearBurn: "Linear Burn"
  of bmDarkerColor: "Darker Color"
  of bmLighten: "Lighten"
  of bmScreen: "Screen"
  of bmColorDodge: "Color Dodge"
  of bmLinearDodge: "Linear Dodge (Add)"
  of bmLighterColor: "Lighter Color"
  of bmOverlay: "Overlay"
  of bmSoftLight: "Soft Light"
  of bmHardLight: "Hard Light"
  of bmVividLight: "Vivid Light"
  of bmLinearLight: "Linear Light"
  of bmPinLight: "Pin Light"
  of bmHardMix: "Hard Mix"
  of bmDifference: "Difference"
  of bmExclusion: "Exclusion"
  of bmSubtract: "Subtract"
  of bmDivide: "Divide"
  of bmHue: "Hue"
  of bmSaturation: "Saturation"
  of bmColor: "Color"
  of bmLuminosity: "Luminosity"
  of bmReflect: "Reflect"
  of bmGlow: "Glow"
  of bmNegation: "Negation"
  of bmXor: "XOR"
  of bmPaintNetColorBurn: "Color Burn (Paint.NET)"
  of bmPaintNetColorDodge: "Color Dodge (Paint.NET)"

proc psdKey*(m: BlendMode): string =
  ## PSD 4-character blend key. Modes without an equivalent fall back to Normal.
  case m
  of bmPassThrough: "pass"
  of bmNormal: "norm"
  of bmDissolve: "diss"
  of bmDarken: "dark"
  of bmMultiply: "mul "
  of bmColorBurn: "idiv"
  of bmLinearBurn: "lbrn"
  of bmDarkerColor: "dkCl"
  of bmLighten: "lite"
  of bmScreen: "scrn"
  of bmColorDodge: "div "
  of bmLinearDodge: "lddg"
  of bmLighterColor: "lgCl"
  of bmOverlay: "over"
  of bmSoftLight: "sLit"
  of bmHardLight: "hLit"
  of bmVividLight: "vLit"
  of bmLinearLight: "lLit"
  of bmPinLight: "pLit"
  of bmHardMix: "hMix"
  of bmDifference: "diff"
  of bmExclusion: "smud"
  of bmSubtract: "fsub"
  of bmDivide: "fdiv"
  of bmHue: "hue "
  of bmSaturation: "sat "
  of bmColor: "colr"
  of bmLuminosity: "lum "
  of bmReflect, bmGlow, bmNegation, bmXor, bmPaintNetColorBurn, bmPaintNetColorDodge: "norm"

proc blendFromPsdKey*(key: string): Option[BlendMode] =
  if bmPassThrough.psdKey == key: return some(bmPassThrough)
  for m in LayerModes:
    if m.psdKey == key: return some(m)
  none(BlendMode)

proc isSeparable*(m: BlendMode): bool =
  m notin {bmHue, bmSaturation, bmColor, bmLuminosity, bmDarkerColor, bmLighterColor}

proc reflect(cb, cs: float32): float32 {.inline.} =
  if cs >= 1.0'f32: 1.0'f32 else: min(cb * cb / (1.0'f32 - cs), 1.0'f32)

proc vividLightHardMix(cb, cs: float32): float32 {.inline.} =
  const Edge = 1e-4'f32
  if cs <= 0.5'f32:
    if cb >= 1.0'f32 - Edge: 1.0
    elif cs <= 0.0'f32: 0.0'f32
    else: 1.0'f32 - min((1.0'f32 - cb) / (2.0'f32 * cs), 1.0'f32)
  elif cb <= Edge: 0.0
  elif cs >= 1.0'f32: 1.0'f32
  else: min(cb / (2.0'f32 * (1.0'f32 - cs)), 1.0'f32)

proc colorBurn(cb, cs: float32): float32 {.inline.} =
  if cb >= 1.0'f32: 1.0'f32
  elif cs <= 0.0'f32: 0.0'f32
  else: 1.0'f32 - min((1.0'f32 - cb) / cs, 1.0'f32)

proc colorDodge(cb, cs: float32): float32 {.inline.} =
  if cb <= 0.0'f32: 0.0'f32
  elif cs >= 1.0'f32: 1.0'f32
  else: min(cb / (1.0'f32 - cs), 1.0'f32)

proc hardLight(cb, cs: float32): float32 {.inline.} =
  if cs <= 0.5'f32: cb * 2.0'f32 * cs
  else:
    let s = 2.0'f32 * cs - 1.0'f32
    cb + s - cb * s

proc softLightPs(cb, cs: float32): float32 {.inline.} =
  ## Photoshop's Soft Light (differs from the W3C/PDF definition in the upper half).
  if cs <= 0.5'f32: 2.0'f32 * cb * cs + cb * cb * (1.0'f32 - 2.0'f32 * cs)
  else: 2.0'f32 * cb * (1.0'f32 - cs) + sqrt(max(cb, 0.0'f32)) * (2.0'f32 * cs - 1.0'f32)

proc channel(mode: BlendMode; cb, cs, hi: float32): float32 {.inline.} =
  case mode
  of bmNormal, bmDissolve, bmPassThrough: cs
  of bmDarken: min(cb, cs)
  of bmMultiply: cb * cs
  of bmColorBurn: colorBurn(cb, cs)
  of bmLinearBurn: max(cb + cs - 1.0'f32, 0.0'f32)
  of bmLighten: max(cb, cs)
  of bmScreen: cb + cs - cb * cs
  of bmColorDodge: colorDodge(cb, cs)
  of bmLinearDodge: min(cb + cs, hi)
  of bmOverlay: hardLight(cs, cb)
  of bmSoftLight: softLightPs(cb, cs)
  of bmHardLight: hardLight(cb, cs)
  of bmVividLight:
    if cs <= 0.0'f32: 0.0'f32
    elif cs >= 1.0'f32: 1.0'f32
    elif cs <= 0.5'f32: colorBurn(cb, 2.0'f32 * cs)
    else: colorDodge(cb, 2.0'f32 * (cs - 0.5'f32))
  of bmLinearLight: clamp(cb + 2.0'f32 * cs - 1.0'f32, 0.0'f32, 1.0'f32)
  of bmPinLight:
    if cs <= 0.5'f32: min(cb, 2.0'f32 * cs) else: max(cb, 2.0'f32 * cs - 1.0'f32)
  of bmHardMix:
    if vividLightHardMix(cb, cs) >= 0.5'f32 - 1e-6'f32: 1.0 else: 0.0
  of bmDifference: abs(cb - cs)
  of bmExclusion: cb + cs - 2.0'f32 * cb * cs
  of bmSubtract: max(cb - cs, 0.0'f32)
  of bmDivide:
    if cs <= 0.0'f32: (if cb <= 0.0'f32: 0.0'f32 else: 1.0'f32)
    else: min(cb / cs, hi)
  of bmReflect: reflect(cb, cs)
  of bmGlow: reflect(cs, cb)
  of bmNegation: 1.0'f32 - abs(1.0'f32 - cb - cs)
  of bmXor:
    let b = uint8(round(clamp(cb, 0.0'f32, 1.0'f32) * 255.0'f32))
    let s = uint8(round(clamp(cs, 0.0'f32, 1.0'f32) * 255.0'f32))
    float32(b xor s) / 255.0'f32
  of bmPaintNetColorBurn:
    if cs <= 0.0'f32: 0.0'f32 else: max(1.0'f32 - (1.0'f32 - cb) / cs, 0.0'f32)
  of bmPaintNetColorDodge:
    if cs >= 1.0'f32: 1.0'f32 else: min(cb / (1.0'f32 - cs), 1.0'f32)
  of bmDarkerColor, bmLighterColor, bmHue, bmSaturation, bmColor, bmLuminosity: cs

proc blendChannel*(mode: BlendMode; cb, cs: float32): float32 {.inline.} =
  ## Separable per-channel blend function `B(cb, cs)`.
  channel(mode, cb, cs, 1.0'f32)

proc blendChannelHdr*(mode: BlendMode; cb, cs: float32): float32 {.inline.} =
  ## `blendChannel` for 32-bit float documents (Add and Divide don't clip at 1).
  channel(mode, cb, cs, F32Max)

proc lum*(c: array[3, float32]): float32 {.inline.} = 0.3'f32 * c[0] + 0.59'f32 * c[1] + 0.11'f32 * c[2]

proc clipColor(c: array[3, float32]; top: float32): array[3, float32] =
  let l = lum(c)
  let n = min(c[0], min(c[1], c[2]))
  let x = max(c[0], max(c[1], c[2]))
  result = c
  if n < 0.0'f32:
    let d = l - n
    for v in result.mitems: v = (if l > 0.0'f32 and d > 0.0'f32: l + (v - l) * l / d else: l)
  if x > top:
    let d = x - l
    for v in result.mitems: v = (if l < top and d > 0.0'f32: l + (v - l) * (top - l) / d else: l)

proc setLumWithin(c: array[3, float32]; l, top: float32): array[3, float32] =
  let d = l - lum(c)
  clipColor([c[0] + d, c[1] + d, c[2] + d], top)

proc setLum*(c: array[3, float32]; l: float32): array[3, float32] =
  ## `c` with its luminosity moved to `l`, clipped back into gamut towards grey.
  setLumWithin(c, l, 1.0'f32)

proc sat(c: array[3, float32]): float32 = max(c[0], max(c[1], c[2])) - min(c[0], min(c[1], c[2]))

proc setSat(c: array[3, float32]; s: float32): array[3, float32] =
  var idx = [0, 1, 2]
  idx.sort(proc(a, b: int): int = cmp(c[a], c[b]))
  let (imin, imid, imax) = (idx[0], idx[1], idx[2])
  if c[imax] > c[imin]:
    result[imid] = (c[imid] - c[imin]) * s / (c[imax] - c[imin])
    result[imax] = s
  result[imin] = 0.0

proc blendRgbWithin*(mode: BlendMode; cb, cs: array[3, float32]; top: float32): array[3, float32] =
  ## `blendRgb` with Hue / Saturation / Color / Luminosity clipped at `top` instead of 1.
  case mode
  of bmHue: setLumWithin(setSat(cs, sat(cb)), lum(cb), top)
  of bmSaturation: setLumWithin(setSat(cb, sat(cs)), lum(cb), top)
  of bmColor: setLumWithin(cs, lum(cb), top)
  of bmLuminosity: setLumWithin(cb, lum(cs), top)
  of bmDarkerColor: (if lum(cs) < lum(cb): cs else: cb)
  of bmLighterColor: (if lum(cs) > lum(cb): cs else: cb)
  else: [blendChannel(mode, cb[0], cs[0]), blendChannel(mode, cb[1], cs[1]), blendChannel(mode, cb[2], cs[2])]

proc blendRgb*(mode: BlendMode; cb, cs: array[3, float32]): array[3, float32] =
  ## Blend a whole RGB triple: `B(Cb, Cs)` including non-separable modes.
  blendRgbWithin(mode, cb, cs, 1.0'f32)

proc blendRgbHdr*(mode: BlendMode; cb, cs: array[3, float32]): array[3, float32] =
  ## `blendRgb` for 32-bit float documents.
  if mode.isSeparable:
    [blendChannelHdr(mode, cb[0], cs[0]), blendChannelHdr(mode, cb[1], cs[1]), blendChannelHdr(mode, cb[2], cs[2])]
  else: blendRgbWithin(mode, cb, cs, F32Max)

proc composite*(mode: BlendMode; backdrop, source: array[4, float32]; opacity: float32): array[4, float32] =
  ## Composite a straight-alpha source over a straight-alpha backdrop (W3C/PDF general formula).
  ## Dissolve must be resolved by the caller before calling.
  let ab = backdrop[3]
  let asrc = source[3] * opacity
  if asrc <= 0.0'f32: return backdrop
  let cb = [backdrop[0], backdrop[1], backdrop[2]]
  let cs = [source[0], source[1], source[2]]
  let b = blendRgb(mode, cb, cs)
  let ao = asrc + ab * (1.0'f32 - asrc)
  if ao <= 0.0'f32: return [0.0'f32, 0, 0, 0]
  for i in 0..2:
    let premul = (1.0'f32 - asrc) * ab * cb[i] + (1.0'f32 - ab) * asrc * cs[i] + asrc * ab * b[i]
    result[i] = premul / ao
  result[3] = ao
