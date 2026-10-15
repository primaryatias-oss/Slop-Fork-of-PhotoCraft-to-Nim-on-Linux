## Per-pixel and per-dab kernels for the toning and focus retouching tools: Dodge, Burn, Sponge,
## Blur, Sharpen (port of `retouch.rs`). The brush engine decides *where* and *how strongly* each
## dab applies; these functions decide *what* happens to the pixels.
##
## Tone curves are our own design, chosen to match the observed behaviour of Photoshop's tools:
## the selected range is affected most, pure black and white stay put for Midtones, and "Protect
## Tones" works on luminance (scaling RGB so hue and saturation are kept, with gamut compression
## instead of per-channel clipping).

import std/[math, options]
import ../geom, ../rmath

type
  ToneRange* = enum
    ## Tonal range targeted by Dodge/Burn.
    trShadows = "shadows", trMidtones = "midtones", trHighlights = "highlights"

proc luma*(c: array[3, float32]): float32 {.inline.} =
  ## Rec. 601 luma of a straight RGB triple.
  0.299'f32 * c[0] + 0.587'f32 * c[1] + 0.114'f32 * c[2]

proc sq(v: float32): float32 {.inline.} = v * v

proc toneCurve*(v0, e0: float32; range: ToneRange; burn: bool): float32 =
  ## The dodge/burn transfer for one value `v` (0..1) with strength `e` (0..1).
  let v = rmax(v0, 0'f32)
  let e = clamp(e0, 0'f32, 1'f32)
  case range
  of trMidtones:
    # Gamma bend, weighted to peak at mid-grey: 0 and 1 are fixed points.
    let w = clamp(4'f32 * v * (1'f32 - v), 0'f32, 1'f32)
    if burn: v + w * (pow(rmin(v, 1'f32), 1'f32 + e) - v)
    else: v + w * (pow(rmin(v, 1'f32), 1'f32 / (1'f32 + e)) - v)
  of trShadows:
    # `v + e·w·(target − v)` with weight (1−v)²: black lifts when dodging shadows.
    if burn: v - e * sq(1'f32 - rmin(v, 1'f32)) * v
    else: v + e * sq(1'f32 - rmin(v, 1'f32)) * (1'f32 - rmin(v, 1'f32))
  of trHighlights:
    if burn: v - e * sq(rmin(v, 1'f32)) * v
    else: v + e * sq(rmin(v, 1'f32)) * (1'f32 - rmin(v, 1'f32))

proc compressGamut(c: array[3, float32]; l0: float32): array[3, float32] =
  ## Brings an RGB triple back into 0..1 while keeping its luma (mix toward grey), instead of
  ## clipping each channel (which would shift hue).
  let l = clamp(l0, 0'f32, 1'f32)
  let (lo, hi) = (rmin(rmin(c[0], c[1]), c[2]), rmax(rmax(c[0], c[1]), c[2]))
  var t = 1'f32
  if hi > 1 and hi - l > 1e-9'f32: t = rmin(t, (1'f32 - l) / (hi - l))
  if lo < 0 and l - lo > 1e-9'f32: t = rmin(t, l / (l - lo))
  [l + (c[0] - l) * t, l + (c[1] - l) * t, l + (c[2] - l) * t]

proc dodgeBurn*(c: array[3, float32]; e: float32; range: ToneRange; burn, protectTones: bool): array[3, float32] =
  ## Dodge (`burn = false`) or Burn one straight-RGB colour with strength `e` (0..1). With
  ## `protectTones`, the curve is applied to luma and the colour scaled to the new luma (hue and
  ## saturation preserved, gamut-compressed); otherwise each channel goes through the curve.
  if e <= 0: return c
  if protectTones:
    let l = luma(c)
    let nl = toneCurve(l, e, range, burn)
    let o = (if l > 1e-6'f32: [c[0] * nl / l, c[1] * nl / l, c[2] * nl / l] else: [nl, nl, nl])
    compressGamut(o, nl)
  else:
    [toneCurve(c[0], e, range, burn), toneCurve(c[1], e, range, burn), toneCurve(c[2], e, range, burn)]

proc saturation*(c: array[3, float32]): float32 =
  ## HSV-style saturation (chroma / max) of an RGB triple.
  let (lo, hi) = (rmin(rmin(c[0], c[1]), c[2]), rmax(rmax(c[0], c[1]), c[2]))
  if hi <= 1e-6'f32: 0'f32 else: clamp((hi - lo) / hi, 0'f32, 1'f32)

proc sponge*(c: array[3, float32]; amount: float32; saturate, vibrance: bool): array[3, float32] =
  ## Sponge: moves a colour away from (`saturate`) or toward its luma grey by `amount` (0..1).
  ## `vibrance` damps the change where it would clip. Saturation is gamut-compressed around luma.
  var a = clamp(amount, 0'f32, 1'f32)
  if a <= 0: return c
  let s = saturation(c)
  if vibrance: a *= (if saturate: 1'f32 - s else: 0.25'f32 + 0.75'f32 * s)
  let l = luma(c)
  let k = (if saturate: 1'f32 + a else: 1'f32 - a)
  compressGamut([l + (c[0] - l) * k, l + (c[1] - l) * k, l + (c[2] - l) * k], l)

proc localBlur*(data: openArray[float32]; w, h, ch: int; alpha: Option[int]; win: (int, int, int, int);
                sigma: float32): seq[float32] =
  ## Gaussian blur of the window `[x0, x1) × [y0, y1)` of an interleaved `w × h × ch` buffer,
  ## reading neighbours with edge clamping. With an alpha channel (`alpha = some(index)`), colour
  ## is blurred premultiplied so transparent pixels don't bleed their (meaningless) colour.
  ## Returns the window's blurred pixels (`(x1−x0) × (y1−y0) × ch`).
  let (x0, y0, x1, y1) = win
  let (ow, oh) = (max(x1 - x0, 0), max(y1 - y0, 0))
  if w <= 0 or h <= 0 or ch <= 0 or data.len < w * h * ch: return newSeq[float32](ow * oh * max(ch, 0))
  let r = min(asI32(float64(rmax(ceil(sigma * 3'f32), 1'f32))), 1 shl 16)
  var k = newSeq[float32](2 * r + 1)
  for i in -r .. r: k[i + r] = exp(-float32(i * i) / rmax(2'f32 * sigma * sigma, 1e-6'f32))
  var ks = -0'f32
  for v in k: ks += v
  for v in k.mitems: v = v / ks
  var px = newSeq[float32](ch)
  template get(x, y: int) =
    let (xx, yy) = (clamp(x, 0, w - 1), clamp(y, 0, h - 1))
    let i = (yy * w + xx) * ch
    for c in 0 ..< ch: px[c] = data[i + c]
    if alpha.isSome:
      let av = px[alpha.get]
      for c in 0 ..< ch:
        if c != alpha.get: px[c] *= av
  # Horizontal pass over the window rows extended vertically by r.
  let th = oh + 2 * r
  var tmp = newSeq[float32](ow * th * ch)
  for ty in 0 ..< th:
    let y = y0 + ty - r
    for ox in 0 ..< ow:
      let o = (ty * ow + ox) * ch
      for i, kv in k:
        get(x0 + ox + i - r, y)
        for c in 0 ..< ch: tmp[o + c] += px[c] * kv
  result = newSeq[float32](ow * oh * ch)
  for oy in 0 ..< oh:
    for ox in 0 ..< ow:
      let o = (oy * ow + ox) * ch
      for i, kv in k:
        let s = ((oy + i) * ow + ox) * ch
        for c in 0 ..< ch: result[o + c] += tmp[s + c] * kv
      if alpha.isSome:
        let av = result[o + alpha.get]
        for c in 0 ..< ch:
          if c != alpha.get:
            result[o + c] = (if av > 1e-6'f32: result[o + c] / av
                             else: data[(clamp(y0 + oy, 0, h - 1) * w + clamp(x0 + ox, 0, w - 1)) * ch + c])

proc localSharpen*(data: openArray[float32]; w, h, ch: int; alpha: Option[int]; win: (int, int, int, int);
                   amount: float32; protectDetail: bool): seq[float32] =
  ## Unsharp-mask sharpen of a window (same layout as `localBlur`): `v + amount·(v − blur(v))`.
  ## With `protectDetail`, each result is clamped to the 3×3 neighbourhood's range, which stops
  ## halos and noise from being amplified into new extremes. Alpha is left unchanged.
  let (x0, y0, x1, y1) = win
  let ow = max(x1 - x0, 0)
  let blur = localBlur(data, w, h, ch, alpha, win, 1'f32)
  result = newSeq[float32](blur.len)
  if w <= 0 or h <= 0 or data.len < w * h * ch: return
  for y in y0 ..< y1:
    for x in x0 ..< x1:
      let o = ((y - y0) * ow + (x - x0)) * ch
      let i = (clamp(y, 0, h - 1) * w + clamp(x, 0, w - 1)) * ch
      for c in 0 ..< ch:
        let v = data[i + c]
        if alpha.isSome and alpha.get == c:
          result[o + c] = v
          continue
        var s = v + amount * (v - blur[o + c])
        if protectDetail:
          var (lo, hi) = (v, v)
          for (dx, dy) in [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]:
            let (nx, ny) = (clamp(x + dx, 0, w - 1), clamp(y + dy, 0, h - 1))
            let nv = data[(ny * w + nx) * ch + c]
            lo = rmin(lo, nv)
            hi = rmax(hi, nv)
          s = clamp(s, lo, hi)
        result[o + c] = s
