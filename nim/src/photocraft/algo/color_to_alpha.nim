## Filter ▸ Other ▸ Color to Alpha: removes a known colour from the pixels, turning it into
## transparency while keeping what was mixed with it (port of `color_to_alpha.rs`).
##
## A pixel `P` that is a semi-transparent colour `Q` over a background `C` obeys
## `P = a·Q + (1 − a)·C`; the smallest `a` keeping `Q` in 0–1 is the max over channels of
## `(P−C)/(1−C)` (P > C) or `(C−P)/C` (P < C). Then `Q = C + (P − C)/a`.

import std/math
import ../geom, ./params, ./fxutil

proc finite(v: float32): bool {.inline.} = v.classify notin {fcNan, fcInf, fcNegInf}

proc opacity*(colour, key: openArray[float32]): float32 =
  ## The opacity (0–1) a pixel needs to be a mix of some in-range colour and `key`.
  var a = 0'f32
  for i in 0 ..< min(colour.len, key.len):
    let (p, c) = (colour[i], key[i])
    let ai = (if p > c: (if c < 1: (p - c) / (1 - c) else: 1'f32)
              elif p < c: (if c > 0: (c - p) / c else: 1'f32)
              else: 0'f32)
    if ai.finite: a = max(a, ai)
  clamp(a, 0'f32, 1'f32)

proc remap*(a, transparency, opacity: float32): float32 =
  ## `a <= transparency` → 0, `a >= opacity` → 1, linear in between.
  if a <= transparency: 0'f32
  elif a >= opacity: 1'f32
  else: clamp((a - transparency) / (opacity - transparency), 0'f32, 1'f32)

proc unmix*(colour: var openArray[float32]; key: openArray[float32]; transparency, opacityThreshold: float32): float32 =
  ## Unmixes `key` from one pixel's colour channels in place; returns the alpha factor.
  let a = remap(opacity(colour, key), transparency, opacityThreshold)
  if a >= 1: return 1
  for i in 0 ..< min(colour.len, key.len):
    let c = key[i]
    colour[i] = (if a <= 0: c else: clamp(c + (colour[i] - c) / a, 0'f32, 1'f32))
  max(a, 0)

proc colorToAlpha*(src: Image; o: Rect; ctx: Ctx; color: array[4, float32]; transparency, opacityThreshold: float32): seq[float32] =
  ## The filter kernel: `color` is straight sRGB converted to the document's model. Surfaces
  ## without alpha are returned unchanged.
  let n = src.ch
  result = src.crop(o)
  if not ctx.alpha or n < 2: return
  let cc = min(ncol(ctx, n), MaxC)
  let keyAll = native(ctx, [color[0], color[1], color[2], 1'f32])
  var i = 0
  while i + n <= result.len:
    let a = unmix(result.toOpenArray(i, i + cc - 1), keyAll.toOpenArray(0, cc - 1), transparency, opacityThreshold)
    result[i + n - 1] *= a
    i += n
