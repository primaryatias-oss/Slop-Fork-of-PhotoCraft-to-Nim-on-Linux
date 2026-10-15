## Destructive tone and colour adjustments that need more than a per-pixel formula:
## Shadows/Highlights, Replace Color, Match Color and HDR Toning (Image › Adjustments)
## (port of `tone.rs`).
##
## All work on straight (non-premultiplied) RGBA buffers in 0..=1, row-major `w × h`, so callers
## convert any colour model / bit depth to RGBA first. Formulas are documented approximations of
## Photoshop's behaviour, derived from its manual and observation, not from its implementation.

import std/[math, options, algorithm]
import ../color/convert
import ../rmath, ./fxutil, ./other2, ./selection, ./util

type
  Px4 = array[4, float32]

  ShadowsHighlights* = object
    ## Image › Adjustments › Shadows/Highlights parameters (percentages as 0..=100).
    shadowAmount*, shadowTone*, shadowRadius*: float32
    highlightAmount*, highlightTone*, highlightRadius*: float32
    color*: float32   ## colour (saturation) correction in -100..=100, applied where tones changed
    midtone*: float32 ## midtone contrast in -100..=100
    blackClip*, whiteClip*: float32 ## percent of pixels clipped to black / white (0..=50)

  LabStats* = object
    ## Lab mean and standard deviation of the opaque pixels, `[L, a, b]` each.
    mean*, std*: array[3, float32]

  MatchColor* = object
    ## Image › Adjustments › Match Color options (Photoshop ranges).
    luminance*: float32 ## 1..=200 (100 = matched luminance)
    intensity*: float32 ## colour intensity 1..=200 (100 = matched chroma; 1 ≈ gray)
    fade*: float32      ## 0..=100: blend back toward the original
    neutralize*: bool   ## remove the colour cast (target a/b means → 0)

  HdrToning* = object
    ## Image › Adjustments › HDR Toning ("Local Adaptation" method) parameters.
    radius*: float32   ## edge glow radius in px (1..=500)
    strength*: float32 ## edge glow strength 0.1..=4
    gamma*: float32    ## 0.1..=2 (1 = neutral; lower = more contrast)
    exposure*: float32 ## -5..=5 stops
    detail*: float32   ## -100..=300 %
    shadow*, highlight*: float32
    vibrance*, saturation*: float32 ## -100..=100
    curve*: seq[(float32, float32)] ## toning curve on luminance, (input, output) in 0..=1; empty = identity

proc defaultShadowsHighlights*(): ShadowsHighlights =
  ShadowsHighlights(shadowAmount: 35, shadowTone: 50, shadowRadius: 30, highlightAmount: 0, highlightTone: 50,
                    highlightRadius: 30, color: 20, midtone: 0, blackClip: 0.01, whiteClip: 0.01)

proc defaultMatchColor*(): MatchColor = MatchColor(luminance: 100, intensity: 100, fade: 0, neutralize: false)

proc defaultHdrToning*(): HdrToning =
  HdrToning(radius: 30, strength: 0.5, gamma: 1, exposure: 0, detail: 30, shadow: 0, highlight: 0, vibrance: 0,
            saturation: 20)

proc defaultOf*(_: typedesc[ShadowsHighlights]): ShadowsHighlights = defaultShadowsHighlights()
proc defaultOf*(_: typedesc[MatchColor]): MatchColor = defaultMatchColor()
proc defaultOf*(_: typedesc[HdrToning]): HdrToning = defaultHdrToning()

proc luma(c: array[3, float32]): float32 {.inline.} = 0.299'f32 * c[0] + 0.587'f32 * c[1] + 0.114'f32 * c[2]

proc smoothstep(e0, e1, x: float32): float32 {.inline.} =
  let t = clamp((x - e0) / (e1 - e0), 0'f32, 1'f32)
  t * t * (3'f32 - 2'f32 * t)

proc blurredLuma(px: openArray[Px4]; w, h: int; radius: float32): seq[float32] =
  ## Blurred luminance (Gaussian, `radius` px ≈ 2σ like Photoshop's radius fields).
  result = newSeq[float32](px.len)
  for i, p in px: result[i] = luma([p[0], p[1], p[2]])
  gaussBlurN(result, w, h, 1, rmax(radius, 0'f32) / 2'f32)

proc clipStretch(px: var openArray[Px4]; black, white: float32) =
  ## Stretches levels so `black`% of pixels clip to 0 and `white`% to 1 (luminance histogram).
  if black <= 0 and white <= 0: return
  var hist: array[1024, uint32]
  var n = 0'u32
  for q in px:
    if q[3] > 0:
      inc hist[asIdx(round(clamp(luma([q[0], q[1], q[2]]), 0'f32, 1'f32) * 1023'f32))]
      inc n
  if n == 0: return
  proc find(pct: float32; rev: bool): float32 =
    let target = asU32(clamp(pct, 0'f32, 50'f32) / 100'f32 * float32(n))
    var acc = 0'u32
    for k in 0 ..< 1024:
      let i = (if rev: 1023 - k else: k)
      acc += hist[i]
      if acc > target: return float32(i) / 1023'f32
    if rev: 0'f32 else: 1'f32
  let lo = find(black, false)
  let hi = find(white, true)
  if hi - lo < 1e-3'f32: return
  for q in px.mitems:
    if q[3] > 0:
      for c in 0 .. 2: q[c] = clamp((q[c] - lo) / (hi - lo), 0'f32, 1'f32)

proc shadowsHighlights*(px: var openArray[Px4]; w, h: int; p: ShadowsHighlights) =
  ## Local tone mapping: a blurred-luminance mask selects shadows (dark neighbourhoods, up to the
  ## tonal width) and highlights; shadows are lifted and highlights pulled down by a gain that
  ## preserves hue, then saturation is corrected where the gain moved tones, midtone contrast is
  ## applied, and the result is stretched by the clip percentages.
  if px.len == 0: return
  let ls = blurredLuma(px, w, h, p.shadowRadius)
  let lh = (if abs(p.highlightRadius - p.shadowRadius) < 1e-3'f32: ls else: blurredLuma(px, w, h, p.highlightRadius))
  let (sa, ha) = (p.shadowAmount / 100'f32, p.highlightAmount / 100'f32)
  let (st, ht) = (rmax(p.shadowTone / 100'f32, 0.01'f32), rmax(p.highlightTone / 100'f32, 0.01'f32))
  let color = p.color / 100'f32
  let mid = p.midtone / 100'f32
  for i in 0 ..< min(px.len, min(ls.len, lh.len)):
    var q = px[i]
    if q[3] <= 0: continue
    let c = [q[0], q[1], q[2]]
    # Mask weights: 1 in deep shadows (resp. bright highlights), fading out at the tonal width.
    let ms = 1'f32 - smoothstep(0'f32, st, ls[i])
    let mh = smoothstep(1'f32 - ht, 1'f32, lh[i])
    var o = c
    if sa > 0 and ms > 0:
      # Lift: a gamma-like curve on the pixel, stronger for darker neighbourhoods.
      let g = 1'f32 / (1'f32 + 2'f32 * sa * ms)
      for k in 0 .. 2: o[k] = pow(rmax(o[k], 0'f32), g)
    if ha > 0 and mh > 0:
      let g = 1'f32 / (1'f32 + 2'f32 * ha * mh)
      for k in 0 .. 2: o[k] = 1'f32 - pow(1'f32 - rmin(o[k], 1'f32), g)
    if mid != 0:
      let k = 1'f32 + mid * clamp(1'f32 - ms - mh, 0'f32, 1'f32)
      for j in 0 .. 2: o[j] = (o[j] - 0.5'f32) * k + 0.5'f32
    let changed = rmin(ms * sa + mh * ha, 1'f32)
    if color != 0 and changed > 0:
      let l1 = luma(o)
      let s = 1'f32 + color * changed
      for j in 0 .. 2: o[j] = l1 + (o[j] - l1) * s
    q[0] = clamp(o[0], 0'f32, 1'f32)
    q[1] = clamp(o[1], 0'f32, 1'f32)
    q[2] = clamp(o[2], 0'f32, 1'f32)
    px[i] = q
  clipStretch(px, p.blackClip, p.whiteClip)

proc replaceColor*(px: var openArray[Px4]; color: array[3, float32]; fuzziness, hue, saturation, lightness: float32): seq[float32] =
  ## Image › Adjustments › Replace Color: Color Range-style selection (fuzziness in 0..=200
  ## levels) around `color`, then a hue (±180°), saturation and lightness (±100) shift weighted by
  ## it. Returns the selection mask (what the dialog previews).
  let mask = colorRange(px, color, fuzziness)
  for i in 0 ..< min(px.len, mask.len):
    let k = mask[i]
    if k <= 0: continue
    let c = [px[i][0], px[i][1], px[i][2]]
    let hsl = rgbToHsl(c)
    let (hh, ss, ll) = (hsl[0], hsl[1], hsl[2])
    let s = (if saturation >= 0: ss + (1'f32 - ss) * saturation / 100'f32 else: ss * (1'f32 + saturation / 100'f32))
    let l = (if lightness >= 0: ll + (1'f32 - ll) * lightness / 100'f32 else: ll * (1'f32 + lightness / 100'f32))
    let o = hslToRgb([remEuclidF(hh + hue / 360'f32, 1'f32), clamp(s, 0'f32, 1'f32), clamp(l, 0'f32, 1'f32)])
    for j in 0 .. 2: px[i][j] = clamp(c[j] + (o[j] - c[j]) * k, 0'f32, 1'f32)
  mask

proc labStats*(px: openArray[Px4]; mask: Option[seq[float32]]): Option[LabStats] =
  ## Statistics for Match Color (pixels weighted by alpha × optional mask).
  var sum, sq: array[3, float64]
  var wsum = 0.0
  for i, q in px:
    let mv = (if mask.isNone: 1'f32 elif i < mask.get.len: mask.get[i] else: 0'f32)
    let w = float64(q[3] * mv)
    if w <= 0: continue
    let lab = srgbToLab([q[0], q[1], q[2]])
    for k in 0 .. 2:
      sum[k] += w * float64(lab[k])
      sq[k] += w * float64(lab[k]) * float64(lab[k])
    wsum += w
  if wsum <= 0: return none(LabStats)
  var st: LabStats
  for k in 0 .. 2:
    st.mean[k] = float32(sum[k] / wsum)
    let m = sum[k] / wsum
    st.std[k] = float32(sqrt(rmax(sq[k] / wsum - m * m, 0.0)))
  some(st)

proc matchColor*(px: var openArray[Px4]; target: LabStats; source: Option[LabStats]; o: MatchColor) =
  ## Reinhard-style colour transfer in Lab: shift and scale each channel of `px` (statistics
  ## `target`) toward `source`'s statistics; with no source only Neutralize / intensity /
  ## luminance act on the image's own statistics.
  let src = (if source.isSome: source.get else: target)
  let lum = o.luminance / 100'f32
  let inten = o.intensity / 100'f32
  let fade = clamp(o.fade / 100'f32, 0'f32, 1'f32)
  proc scale(k: int): float32 = (if target.std[k] > 1e-4'f32: src.std[k] / target.std[k] else: 1'f32)
  var dstMean = src.mean
  if o.neutralize:
    dstMean[1] = 0
    dstMean[2] = 0
  for q in px.mitems:
    if not (q[3] > 0): continue
    let c = [q[0], q[1], q[2]]
    let lab = srgbToLab(c)
    let l = ((lab[0] - target.mean[0]) * scale(0) + dstMean[0]) * lum
    let a = ((lab[1] - target.mean[1]) * scale(1) + dstMean[1]) * inten
    let b = ((lab[2] - target.mean[2]) * scale(2) + dstMean[2]) * inten
    let res = labToSrgb([clamp(l, 0'f32, 100'f32), a, b])
    for i in 0 .. 2: q[i] = clamp(res[i] + (c[i] - res[i]) * fade, 0'f32, 1'f32)

proc curveEval(pts: openArray[(float32, float32)]; x: float32): float32 =
  if pts.len < 2: return x
  if x <= pts[0][0]: return pts[0][1]
  for i in 0 ..< pts.len - 1:
    let (a, b) = (pts[i], pts[i + 1])
    if x <= b[0]:
      let span = rmax(b[0] - a[0], 1e-6'f32)
      return a[1] + (b[1] - a[1]) * (x - a[0]) / span
  pts[^1][1]

proc hdrToning*(px: var openArray[Px4]; w, h: int; p: HdrToning) =
  ## Local adaptation tone mapping (Durand–Dorsey style base/detail split in log luminance): the
  ## blurred log luminance (radius) is the base layer, compressed toward its mean by `strength`;
  ## detail is boosted; then exposure, gamma, shadow/highlight lift, the toning curve, and
  ## vibrance/saturation. Like Photoshop the result is a flattened, low-dynamic-range image.
  if px.len == 0: return
  let eps = 1e-4'f32
  var logl = newSeq[float32](px.len)
  for i, q in px: logl[i] = ln(rmax(luma([q[0], q[1], q[2]]), 0'f32) + eps)
  var base = logl
  gaussBlurN(base, w, h, 1, rmax(p.radius, 1'f32) / 2'f32)
  var cnt = 0
  for q in px:
    if q[3] > 0: inc cnt
  let n = float32(max(cnt, 1))
  var msum = 0'f32
  for i, q in px:
    if q[3] > 0 and i < base.len: msum += base[i]
  let mean = msum / n
  let compress = 1'f32 / (1'f32 + rmax(p.strength, 0'f32))
  let detail = 1'f32 + p.detail / 100'f32
  let exposure = pow(2'f32, p.exposure)
  let gamma = clamp(p.gamma, 0.1'f32, 4'f32)
  var sorted = p.curve
  sorted.sort(proc(a, b: (float32, float32)): int = totalCmp(a[0], b[0]))
  for i in 0 ..< min(px.len, base.len):
    var q = px[i]
    if q[3] <= 0: continue
    let c = [q[0], q[1], q[2]]
    let l0 = rmax(luma(c), 0'f32) + eps
    let newLog = mean + (base[i] - mean) * compress + (logl[i] - base[i]) * detail
    # The contract is "lower = more contrast", i.e. gamma is the divisor of the exponent.
    var l1 = pow(rmax(exp(newLog) * exposure, 0'f32), 1'f32 / gamma)
    l1 += p.shadow / 100'f32 * powi(1'f32 - l1, 3) * 0.5'f32 - p.highlight / 100'f32 * powi(l1, 3) * 0.5'f32
    l1 = curveEval(sorted, clamp(l1, 0'f32, 1'f32))
    let ratio = l1 / l0
    var o = [c[0] * ratio, c[1] * ratio, c[2] * ratio]
    # Saturation then vibrance (less saturated colours move more), around the new luminance.
    let ll = luma(o)
    let satNow = block:
      let (mx, mn) = (rmax(rmax(o[0], o[1]), o[2]), rmin(rmin(o[0], o[1]), o[2]))
      if mx > 0: (mx - mn) / mx else: 0'f32
    let s = 1'f32 + p.saturation / 100'f32 + p.vibrance / 100'f32 * (1'f32 - satNow)
    for k in 0 .. 2: o[k] = ll + (o[k] - ll) * rmax(s, 0'f32)
    q[0] = clamp(o[0], 0'f32, 1'f32)
    q[1] = clamp(o[1], 0'f32, 1'f32)
    q[2] = clamp(o[2], 0'f32, 1'f32)
    px[i] = q
