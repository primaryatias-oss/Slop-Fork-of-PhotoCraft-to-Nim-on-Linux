## Filter › Camera Raw Filter: a float develop pipeline over RGB pixels (port of `camera_raw.rs`).
##
## Stages, in order (each skipped when its controls are neutral, so defaults are an identity):
##
## 1. **White balance and exposure** in linear light (the document's values decoded with the sRGB
##    curve as a perceptual working encoding; encoding is undone exactly at the end).
## 2. **Dehaze**: K. He, J. Sun, X. Tang, *Single Image Haze Removal Using Dark Channel Prior*,
##    CVPR 2009 (dark channel, atmospheric light from its brightest 0.1 %, transmission refined
##    with the guided filter); negative amounts add haze toward the atmospheric light.
## 3. **Tone** on luminance with colour ratios kept: Highlights / Shadows as exposure changes
##    weighted by an edge-preserving base layer (guided filter: K. He, J. Sun, X. Tang, *Guided
##    Image Filtering*, ECCV 2010), so local contrast survives; Whites / Blacks move the ends of
##    the curve; Contrast is an S-curve about middle grey; Clarity (large radius, midtone
##    weighted) and Texture (small radius) add band-pass local contrast.
## 4. **Presence**: Vibrance (weighted toward less saturated colours) and Saturation.
## 5. **Tone Curve**: parametric regions (Highlights, Lights, Darks, Shadows with movable splits)
##    then point curves (master and per channel), monotone cubic interpolation (F. Fritsch,
##    R. Carlson, *Monotone Piecewise Cubic Interpolation*, SIAM J. Numer. Anal. 1980).
## 6. **Color Mixer (HSL)**: eight hue bands (red, orange, yellow, green, aqua, blue, purple,
##    magenta) with hat-function weights on the hue circle.
## 7. **Color Grading**: shadows / midtones / highlights / global wheels (hue, saturation,
##    luminance) with Blending and Balance.
## 8. **Detail**: sharpening on luminance (unsharp mask with Detail as halo control and Masking as
##    an edge mask) and noise reduction (luminance by guided filter, colour by chroma blur).
## 9. **Effects**: post-crop vignette (amount, midpoint, roundness, feather, highlights,
##    style) and grain (amount, size, roughness; position-hashed, deterministic).

import std/[math, json, algorithm, strutils]
import ./util, ./fxutil

const MaxCurvePoints* = 16 ## most points a Camera Raw point curve accepts from commands

type
  Wheel* = object
    ## One colour grading wheel.
    hue*: float32 ## degrees 0..360
    sat*: float32 ## 0..=100
    lum*: float32 ## −100..=100

  CameraRaw* = object
    ## Camera Raw settings in Adobe Camera Raw's slider units.
    temperature*, tint*, exposure*, contrast*, highlights*, shadows*, whites*, blacks*: float32
    texture*, clarity*, dehaze*, vibrance*, saturation*: float32
    curveHighlights*, curveLights*, curveDarks*, curveShadows*: float32
    curveSplits*: array[3, float32] ## region splits (percent)
    pointCurve*, pointCurveRed*, pointCurveGreen*, pointCurveBlue*: seq[array[2, float32]]
      ## point curves as `[input, output]` in 0..=255 (empty = linear)
    hslHue*, hslSat*, hslLum*: array[8, float32]
      ## Color mixer: red, orange, yellow, green, aqua, blue, purple, magenta
    gradeShadows*, gradeMidtones*, gradeHighlights*, gradeGlobal*: Wheel
    gradeBlending*, gradeBalance*: float32
    sharpenAmount*, sharpenRadius*, sharpenDetail*, sharpenMasking*: float32
    noiseLuminance*, noiseLuminanceDetail*, noiseColor*, noiseColorDetail*: float32
    grainAmount*, grainSize*, grainRoughness*: float32
    vignetteAmount*, vignetteMidpoint*, vignetteRoundness*, vignetteFeather*, vignetteHighlights*: float32
    vignetteStyle*: string ## `highlightPriority | colorPriority | paintOverlay`
    seed*: uint32
    pixelScale*: float32 ## pixel-size multiplier for previews on downsampled proxies

const HslBands*: array[8, float32] = [0'f32, 30, 60, 120, 180, 240, 270, 300]
  ## hue band centres of the Color Mixer (degrees)

proc defaultCameraRaw*(): CameraRaw =
  CameraRaw(curveSplits: [25'f32, 50, 75], gradeBlending: 50, sharpenRadius: 1, sharpenDetail: 25,
            noiseLuminanceDetail: 50, noiseColorDetail: 50, grainSize: 25, grainRoughness: 50,
            vignetteMidpoint: 50, vignetteFeather: 50, vignetteStyle: "highlightPriority", pixelScale: 1)

proc validateCurve*(points: openArray[array[2, float32]]) =
  ## Checks a point curve from untrusted input: empty (linear) or 2..=16 finite points in 0..=255
  ## with inputs increasing by at least one level; raises `ValueError` otherwise. Deserialization
  ## stays lenient on purpose (`curveLut` sorts and de-duplicates), so stored curves keep rendering.
  if points.len == 1: raise newException(ValueError, "a nonempty curve needs at least two points")
  if points.len > MaxCurvePoints: raise newException(ValueError, "at most " & $MaxCurvePoints & " curve points")
  for p in points:
    for v in p:
      if not (v.isFinite and v >= 0'f32 and v <= 255'f32):
        raise newException(ValueError, "curve coordinates must be finite and in 0..=255")
  for i in 1 ..< points.len:
    if not (points[i][0] - points[i - 1][0] >= 1'f32):
      raise newException(ValueError, "curve inputs must increase by at least one level")

proc neutral(w: Wheel): bool = w.sat == 0'f32 and w.lum == 0'f32

proc luma(c: array[3, float32]): float32 {.inline.} = 0.2126'f32 * c[0] + 0.7152'f32 * c[1] + 0.0722'f32 * c[2]

proc smoothstep(e0, e1, x: float32): float32 {.inline.} =
  let t = clamp((x - e0) / (e1 - e0), 0'f32, 1'f32)
  t * t * (3'f32 - 2'f32 * t)

proc boxMean(v: openArray[float32]; w, h, r0: int): seq[float32] =
  ## Box mean of a single-channel buffer (radius `r`, edge clamped): rows, then columns with
  ## running column sums (f64 accumulators, like the Rust banded pass).
  if r0 <= 0 or w <= 0 or h <= 0: return @v
  let r = min(r0, MaxBoxRadius)
  let n = float64(2 * r + 1)
  var tmp = newSeq[float32](w * h)
  for y in 0 ..< h:
    let base = y * w
    template at(k: int): float64 = float64(v[base + clamp(k, 0, w - 1)])
    var acc = -0.0 # Rust's Iterator::sum starts at -0.0
    for k in -r .. r: acc += at(k)
    for x in 0 ..< w:
      tmp[base + x] = float32(acc / n)
      acc += at(x + r + 1) - at(x - r)
  result = newSeq[float32](w * h)
  var acc = newSeq[float64](w)
  template row(y: int): int = clamp(y, 0, h - 1) * w
  for y in -r .. r:
    let o = row(y)
    for x in 0 ..< w: acc[x] += float64(tmp[o + x])
  for y in 0 ..< h:
    for x in 0 ..< w: result[y * w + x] = float32(acc[x] / n)
    let (ad, sb) = (row(y + r + 1), row(y - r))
    for x in 0 ..< w: acc[x] += float64(tmp[ad + x] - tmp[sb + x])

proc gauss(v: openArray[float32]; w, h: int; sigma: float32): seq[float32] =
  ## Approximate Gaussian (three box passes).
  if sigma < 0.3'f32: return @v
  result = @v
  for r in gaussBoxRadii(sigma): result = boxMean(result, w, h, r)

proc guided(i, p: openArray[float32]; w, h, r: int; eps: float32): seq[float32] =
  ## Guided filter of `p` guided by `i` (He et al. 2010).
  let mi = boxMean(i, w, h, r)
  let mp = boxMean(p, w, h, r)
  var ip, ii = newSeq[float32](w * h)
  for k in 0 ..< w * h:
    ip[k] = i[k] * p[k]
    ii[k] = i[k] * i[k]
  let mip = boxMean(ip, w, h, r)
  let mii = boxMean(ii, w, h, r)
  var a, b = newSeq[float32](w * h)
  for k in 0 ..< w * h:
    a[k] = (mip[k] - mi[k] * mp[k]) / (mii[k] - mi[k] * mi[k] + eps)
    b[k] = mp[k] - a[k] * mi[k]
  let ma = boxMean(a, w, h, r)
  let mb = boxMean(b, w, h, r)
  result = newSeq[float32](w * h)
  for k in 0 ..< w * h: result[k] = ma[k] * i[k] + mb[k]

proc guidedFast(i, p: openArray[float32]; w, h, r: int; eps: float32): seq[float32] =
  ## Fast guided filter (K. He, J. Sun, *Fast Guided Filter*, arXiv 2015): the linear
  ## coefficients are computed on a `s×` subsampled copy and upsampled bilinearly.
  let s = clamp(r div 8, 1, 8)
  if s == 1: return guided(i, p, w, h, r, eps)
  let (sw, sh) = ((w + s - 1) div s, (h + s - 1) div s)
  proc down(v: openArray[float32]): seq[float32] =
    var o, c = newSeq[float32](sw * sh)
    for y in 0 ..< h:
      for x in 0 ..< w:
        let k = (y div s) * sw + x div s
        o[k] += v[y * w + x]
        c[k] += 1'f32
    result = newSeq[float32](sw * sh)
    for k in 0 ..< sw * sh: result[k] = o[k] / rmax(c[k], 1'f32)
  let (si, sp) = (down(i), down(p))
  let rs = max(r div s, 1)
  let mi = boxMean(si, sw, sh, rs)
  let mp = boxMean(sp, sw, sh, rs)
  var ip, ii = newSeq[float32](sw * sh)
  for k in 0 ..< sw * sh:
    ip[k] = si[k] * sp[k]
    ii[k] = si[k] * si[k]
  let mip = boxMean(ip, sw, sh, rs)
  let mii = boxMean(ii, sw, sh, rs)
  var a, b = newSeq[float32](sw * sh)
  for k in 0 ..< sw * sh:
    a[k] = (mip[k] - mi[k] * mp[k]) / (mii[k] - mi[k] * mi[k] + eps)
    b[k] = mp[k] - a[k] * mi[k]
  let (ma, mb) = (boxMean(a, sw, sh, rs), boxMean(b, sw, sh, rs))
  result = newSeq[float32](w * h)
  let sf = float32(s)
  for y in 0 ..< h:
    let fy = (float32(y) + 0.5'f32) / sf - 0.5'f32
    for x in 0 ..< w:
      let fx = (float32(x) + 0.5'f32) / sf - 0.5'f32
      let av = bilinear(ma, sw, sh, 1, 0, fx, fy)
      let bv = bilinear(mb, sw, sh, 1, 0, fx, fy)
      result[y * w + x] = av * i[y * w + x] + bv

proc curveLut*(pts: openArray[array[2, float32]]; n: int): seq[float32] =
  ## Monotone cubic (Fritsch–Carlson) through `pts` (0..=255, sorted and de-duplicated here),
  ## evaluated into an `n`-entry LUT over 0..=1.
  var p0: seq[array[2, float32]]
  for q in pts: p0.add [q[0] / 255'f32, q[1] / 255'f32]
  p0.sort(proc(a, b: array[2, float32]): int = totalCmp(a[0], b[0]))
  var p: seq[array[2, float32]]
  for q in p0:
    if p.len > 0 and abs(q[0] - p[^1][0]) < 1e-6'f32: continue
    p.add q
  if p.len < 2:
    for k in 0 ..< n: result.add float32(k) / float32(n - 1)
    return
  let m = p.len
  var d = newSeq[float32](m - 1)
  for k in 0 ..< m - 1: d[k] = (p[k + 1][1] - p[k][1]) / rmax(p[k + 1][0] - p[k][0], 1e-6'f32)
  var t = newSeq[float32](m)
  t[0] = d[0]
  t[m - 1] = d[m - 2]
  for k in 1 ..< m - 1:
    t[k] = (if d[k - 1] * d[k] <= 0'f32: 0'f32 else: (d[k - 1] + d[k]) / 2'f32)
  for k in 0 ..< m - 1:
    if d[k] == 0'f32:
      t[k] = 0
      t[k + 1] = 0
      continue
    let (a, b) = (t[k] / d[k], t[k + 1] / d[k])
    let s = a * a + b * b
    if s > 9'f32:
      let tau = 3'f32 / sqrt(s)
      t[k] = tau * a * d[k]
      t[k + 1] = tau * b * d[k]
  for i in 0 ..< n:
    let x = float32(i) / float32(n - 1)
    if x <= p[0][0]:
      result.add p[0][1]
      continue
    if x >= p[m - 1][0]:
      result.add p[m - 1][1]
      continue
    var k = m - 2
    for j in 0 ..< m - 1:
      if x <= p[j + 1][0]:
        k = j
        break
    let hh = p[k + 1][0] - p[k][0]
    let s = (x - p[k][0]) / hh
    let (h00, h10, h01, h11) = (2'f32 * s * s * s - 3'f32 * s * s + 1'f32, s * s * s - 2'f32 * s * s + s,
                                -2'f32 * s * s * s + 3'f32 * s * s, s * s * s - s * s)
    result.add clamp(h00 * p[k][1] + h10 * hh * t[k] + h01 * p[k + 1][1] + h11 * hh * t[k + 1], 0'f32, 1'f32)

proc lutEval(lut: openArray[float32]; v: float32): float32 {.inline.} =
  if not (v >= 0'f32 and v <= 1'f32):
    # Extend linearly outside (float documents).
    return (if v < 0'f32: v + lut[0] else: lut[^1] + (v - 1'f32))
  let x = v * float32(lut.len - 1)
  let i = min(asIdx(x), lut.len - 2)
  let t = x - float32(i)
  lut[i] + (lut[i + 1] - lut[i]) * t

proc parametricLut(p: CameraRaw; n: int): seq[float32] =
  ## Parametric tone curve as a LUT (regions split at `curveSplits`, each slider ±100 moves its
  ## region by up to a quarter of the range, smoothly).
  var s: array[3, float32]
  for k in 0 .. 2: s[k] = clamp(p.curveSplits[k], 5'f32, 95'f32) / 100'f32
  let centres = [s[0] / 2'f32, (s[0] + s[1]) / 2'f32, (s[1] + s[2]) / 2'f32, (s[2] + 1'f32) / 2'f32]
  let amounts = [p.curveShadows, p.curveDarks, p.curveLights, p.curveHighlights]
  let widths = [rmax(s[0], 0.1'f32), rmax(s[1] - s[0], 0.1'f32), rmax(s[2] - s[1], 0.1'f32), rmax(1'f32 - s[2], 0.1'f32)]
  result = newSeq[float32](n)
  for i in 0 ..< n:
    let x = float32(i) / float32(n - 1)
    var y = x
    for k in 0 .. 3:
      let d = (x - centres[k]) / (widths[k] * 1.2'f32)
      let bump = exp(-d * d * 2'f32) * x * (1'f32 - x) * 4'f32
      y += amounts[k] / 100'f32 * 0.25'f32 * bump
    result[i] = clamp(y, 0'f32, 1'f32)
  for i in 1 ..< n: result[i] = rmax(result[i], result[i - 1])

proc rgbToHsv(c: array[3, float32]): array[3, float32] =
  let (mx, mn) = (rmax(rmax(c[0], c[1]), c[2]), rmin(rmin(c[0], c[1]), c[2]))
  let d = mx - mn
  let h = (if d <= 1e-9'f32: 0'f32
           elif mx == c[0]: 60'f32 * remEuclidF((c[1] - c[2]) / d, 6'f32)
           elif mx == c[1]: 60'f32 * ((c[2] - c[0]) / d + 2'f32)
           else: 60'f32 * ((c[0] - c[1]) / d + 4'f32))
  [h, (if mx > 1e-9'f32: d / mx else: 0'f32), mx]

proc hsvToRgb(h0, s, v: float32): array[3, float32] =
  let h = remEuclidF(h0, 360'f32) / 60'f32
  let c = v * s
  let x = c * (1'f32 - abs((h mod 2'f32) - 1'f32))
  let (r, g, b) = (case asU32(h)
    of 0: (c, x, 0'f32)
    of 1: (x, c, 0'f32)
    of 2: (0'f32, c, x)
    of 3: (0'f32, x, c)
    of 4: (x, 0'f32, c)
    else: (c, 0'f32, x))
  let m = v - c
  [r + m, g + m, b + m]

proc bandWeights*(h: float32): array[8, float32] =
  ## Weights of the eight HSL bands at hue `h` (hat functions between neighbouring centres).
  for k in 0 ..< 8:
    let (c, prev, next) = (HslBands[k], HslBands[(k + 7) mod 8], HslBands[(k + 1) mod 8])
    let diff = ((h - c + 540'f32) mod 360'f32) - 180'f32
    let span = (if diff >= 0'f32: remEuclidF(next - c, 360'f32) else: remEuclidF(c - prev, 360'f32))
    result[k] = rmax(1'f32 - abs(diff) / rmax(span, 1e-3'f32), 0'f32)

proc isLinear(c: openArray[array[2, float32]]): bool =
  if c.len < 2: return true
  for p in c:
    if not (abs(p[0] - p[1]) < 1e-3'f32): return false
  true

proc validate*(p: CameraRaw) =
  ## Rejects malformed point curves in new settings (see `validateCurve`); raises `ValueError`.
  for (name, curve) in [("pointCurve", p.pointCurve), ("pointCurveRed", p.pointCurveRed),
                        ("pointCurveGreen", p.pointCurveGreen), ("pointCurveBlue", p.pointCurveBlue)]:
    try: validateCurve(curve)
    except ValueError as e: raise newException(ValueError, name & ": " & e.msg)

proc validateChanges*(p, stored: CameraRaw) =
  ## `validate` for an edit of `stored` settings: a curve left as stored is accepted, so editing
  ## another control of an older Smart Filter still works.
  for (name, curve, kept) in [("pointCurve", p.pointCurve, stored.pointCurve),
                              ("pointCurveRed", p.pointCurveRed, stored.pointCurveRed),
                              ("pointCurveGreen", p.pointCurveGreen, stored.pointCurveGreen),
                              ("pointCurveBlue", p.pointCurveBlue, stored.pointCurveBlue)]:
    if curve != kept:
      try: validateCurve(curve)
      except ValueError as e: raise newException(ValueError, name & ": " & e.msg)

proc wbNeutral(p: CameraRaw): bool = p.temperature == 0'f32 and p.tint == 0'f32 and p.exposure == 0'f32

proc toneNeutral(p: CameraRaw): bool =
  for v in [p.contrast, p.highlights, p.shadows, p.whites, p.blacks, p.texture, p.clarity]:
    if v != 0'f32: return false
  true

proc hslNeutral(p: CameraRaw): bool =
  for k in 0 ..< 8:
    if p.hslHue[k] != 0'f32 or p.hslSat[k] != 0'f32 or p.hslLum[k] != 0'f32: return false
  true

proc gradeNeutral(p: CameraRaw): bool =
  p.gradeShadows.neutral and p.gradeMidtones.neutral and p.gradeHighlights.neutral and p.gradeGlobal.neutral

proc parametricNeutral(p: CameraRaw): bool =
  p.curveHighlights == 0'f32 and p.curveLights == 0'f32 and p.curveDarks == 0'f32 and p.curveShadows == 0'f32

proc curvesNeutral(p: CameraRaw): bool =
  p.parametricNeutral and isLinear(p.pointCurve) and isLinear(p.pointCurveRed) and isLinear(p.pointCurveGreen) and
    isLinear(p.pointCurveBlue)

proc isIdentity*(p: CameraRaw): bool =
  ## True when every control is neutral (the filter is an identity).
  p.wbNeutral and p.toneNeutral and p.dehaze == 0'f32 and p.vibrance == 0'f32 and p.saturation == 0'f32 and
    p.curvesNeutral and p.hslNeutral and p.gradeNeutral and p.sharpenAmount == 0'f32 and p.noiseLuminance == 0'f32 and
    p.noiseColor == 0'f32 and p.grainAmount == 0'f32 and p.vignetteAmount == 0'f32

proc whiteBalanceGains*(temperature, tint: float32): array[3, float32] =
  ## Camera Raw's white-balance channel gains for Temperature / Tint (−100…100), relative to the
  ## as-shot balance: warmer raises red and lowers blue (±35 % at the ends); a negative tint
  ## raises green, a positive one lowers it toward magenta (±25 %). Shared by the filter (in
  ## linear RGB) and the raw open (on the camera's white-balance multipliers).
  let t = temperature / 100'f32
  let tn = tint / 100'f32
  [1'f32 + 0.35'f32 * t, 1'f32 - 0.25'f32 * tn, 1'f32 - 0.35'f32 * t]

proc asI64f(v: float32): int64 {.inline.} =
  ## Rust `v as i64`: saturating, NaN → 0.
  if v != v: 0'i64
  elif v >= 9.223372e18'f32: high(int64)
  elif v <= -9.223372e18'f32: low(int64)
  else: int64(v)

proc wrapAdd(a, b: int64): int64 {.inline.} = cast[int64](cast[uint64](a) + cast[uint64](b))

proc valueNoise(x, y: float32; seed: uint64): float32 =
  let (xi, yi) = (floor(x), floor(y))
  let (tx, ty) = (x - xi, y - yi)
  let (sx, sy) = (tx * tx * (3'f32 - 2'f32 * tx), ty * ty * (3'f32 - 2'f32 * ty))
  let (ix, iy) = (asI64f(xi), asI64f(yi))
  template hh(dx, dy: int64): float32 = hash01(wrapAdd(ix, dx), wrapAdd(iy, dy), seed)
  let top = hh(0, 0) + (hh(1, 0) - hh(0, 0)) * sx
  let bot = hh(0, 1) + (hh(1, 1) - hh(0, 1)) * sx
  top + (bot - top) * sy

proc dehaze(px: var seq[array[4, float32]]; w, h: int; amount: float32) =
  # Transmission on a ≤ 768 px proxy, upsampled.
  let k = max((max(w, h) + 767) div 768, 1)
  let (sw, sh) = ((w + k - 1) div k, (h + k - 1) div k)
  var small = newSeq[array[3, float32]](sw * sh)
  var cnt = newSeq[float32](sw * sh)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = (y div k) * sw + x div k
      for c in 0 .. 2: small[i][c] += clamp(px[y * w + x][c], 0'f32, 1'f32)
      cnt[i] += 1'f32
  for i in 0 ..< small.len:
    for c in 0 .. 2: small[i][c] = small[i][c] / rmax(cnt[i], 1'f32)
  let r = max(max(sw, sh) div 60, 2)
  proc minFilter(v: seq[float32]): seq[float32] =
    var a = newSeq[float32](sw * sh)
    for y in 0 ..< sh:
      for x in 0 ..< sw:
        var m = F32Max
        for xx in max(x - r, 0) .. min(x + r, sw - 1): m = rmin(m, v[y * sw + xx])
        a[y * sw + x] = m
    result = newSeq[float32](sw * sh)
    for y in 0 ..< sh:
      for x in 0 ..< sw:
        var m = F32Max
        for yy in max(y - r, 0) .. min(y + r, sh - 1): m = rmin(m, a[yy * sw + x])
        result[y * sw + x] = m
  var dark0 = newSeq[float32](sw * sh)
  for i, c in small: dark0[i] = rmin(rmin(c[0], c[1]), c[2])
  let dark = minFilter(dark0)
  # Atmospheric light: mean colour of the brightest 0.1 % of the dark channel.
  var idx = newSeq[int](sw * sh)
  for i in 0 ..< idx.len: idx[i] = i
  idx.sort(proc(a, b: int): int = totalCmp(dark[b], dark[a]))
  let top = max(sw * sh div 1000, 1)
  var air: array[3, float32]
  for q in 0 ..< min(top, idx.len):
    for c in 0 .. 2: air[c] += small[idx[q]][c] / float32(top)
  for c in 0 .. 2: air[c] = rmax(air[c], 0.2'f32)
  var nd0 = newSeq[float32](sw * sh)
  for i, c in small: nd0[i] = rmin(rmin(c[0] / air[0], c[1] / air[1]), c[2] / air[2])
  let normDark = minFilter(nd0)
  var tRaw, guide = newSeq[float32](sw * sh)
  for i in 0 ..< sw * sh:
    tRaw[i] = 1'f32 - 0.95'f32 * normDark[i]
    guide[i] = luma(small[i])
  let t = guided(guide, tRaw, sw, sh, r * 2, 1e-3'f32)
  let ks = float32(k)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let tv = clamp(bilinear(t, sw, sh, 1, 0, (float32(x) + 0.5'f32) / ks - 0.5'f32, (float32(y) + 0.5'f32) / ks - 0.5'f32),
                     0.1'f32, 1'f32)
      for c in 0 .. 2:
        let i = px[y * w + x][c]
        let j = (i - air[c]) / tv + air[c]
        px[y * w + x][c] = (if amount >= 0'f32: i + (j - i) * amount
                            else: i + (air[c] - i) * (-amount) * 0.5'f32 * (1'f32 - tv * 0.5'f32))

proc detail(px: var seq[array[4, float32]]; w, h: int; p: CameraRaw; ps: float32) =
  # Work in Y + chroma (BT.709 luma, B−Y, R−Y).
  let n0 = w * h
  var y, cb, cr = newSeq[float32](n0)
  for i in 0 ..< n0:
    let q = px[i]
    y[i] = luma([q[0], q[1], q[2]])
    cb[i] = q[2] - y[i]
    cr[i] = q[0] - y[i]
  if p.noiseLuminance > 0'f32:
    let n = p.noiseLuminance / 100'f32
    let r = asIdx(rmax(round((1'f32 + 3'f32 * n) * ps), 1'f32))
    let e = n * 0.06'f32
    let smooth = guided(y, y, w, h, r, e * e + 1e-6'f32)
    let keep = p.noiseLuminanceDetail / 100'f32 * 0.5'f32
    for i in 0 ..< n0: y[i] = smooth[i] + (y[i] - smooth[i]) * keep
  if p.noiseColor > 0'f32:
    let sigma = rmax(p.noiseColor / 100'f32 * 6'f32 * ps, 0.3'f32)
    let (bcb, bcr) = (gauss(cb, w, h, sigma), gauss(cr, w, h, sigma))
    let keep = p.noiseColorDetail / 100'f32 * 0.3'f32
    for i in 0 ..< n0:
      cb[i] = bcb[i] + (cb[i] - bcb[i]) * keep
      cr[i] = bcr[i] + (cr[i] - bcr[i]) * keep
  if p.sharpenAmount > 0'f32:
    let sigma = rmax(clamp(p.sharpenRadius, 0.5'f32, 3'f32) * ps, 0.3'f32)
    let blur = gauss(y, w, h, sigma)
    # Masking: an edge mask from the gradient of a smoothed luminance.
    var mask: seq[float32]
    let masked = p.sharpenMasking > 0'f32
    if masked:
      let s = gauss(y, w, h, 1'f32 * rmax(ps, 0.5'f32))
      let t = p.sharpenMasking / 100'f32 * 0.08'f32
      mask = newSeq[float32](n0)
      for i in 0 ..< n0:
        let (x, yy) = (i mod w, i div w)
        let gx = s[yy * w + min(x + 1, w - 1)] - s[yy * w + max(x - 1, 0)]
        let gy = s[min(yy + 1, h - 1) * w + x] - s[max(yy - 1, 0) * w + x]
        mask[i] = smoothstep(t * 0.5'f32, t + 1e-4'f32, hypot(gx, gy))
    let amt = p.sharpenAmount / 100'f32 * 1.2'f32
    let det = p.sharpenDetail / 100'f32
    for i in 0 ..< n0:
      let hp0 = y[i] - blur[i]
      # Low Detail damps large (halo-producing) differences.
      let limit = 0.03'f32 + det * 0.3'f32
      let hp = clamp(hp0, -limit, limit) * (0.5'f32 + 0.5'f32 * det)
      let m = (if masked: mask[i] else: 1'f32)
      y[i] += amt * hp * m
  for i in 0 ..< n0:
    let (l, b, r) = (y[i], cb[i], cr[i])
    let (rr, bb) = (r + l, b + l)
    let g = (l - 0.2126'f32 * rr - 0.0722'f32 * bb) / 0.7152'f32
    px[i][0] = rr
    px[i][1] = g
    px[i][2] = bb

proc vignette(px: var seq[array[4, float32]]; w, h: int; p: CameraRaw) =
  let amt = p.vignetteAmount / 100'f32
  let mid = clamp(p.vignetteMidpoint, 0'f32, 100'f32) / 100'f32
  let feather = clamp(p.vignetteFeather, 0'f32, 100'f32) / 100'f32
  let round = clamp(p.vignetteRoundness, -100'f32, 100'f32) / 100'f32
  let hl = clamp(p.vignetteHighlights, 0'f32, 100'f32) / 100'f32
  let style = p.vignetteStyle
  let (cx, cy) = (float32(w) / 2'f32, float32(h) / 2'f32)
  # Roundness: 0 follows the frame's aspect, +1 a circle, −1 a rounded rectangle.
  var (ax, ay) = (cx, cy)
  if round >= 0'f32:
    let r = rmin(cx, cy) + (rmax(cx, cy) - rmin(cx, cy)) * (1'f32 - round)
    ax = (if cx >= cy: r else: cx)
    ay = (if cy > cx: r else: cy)
  let pw = 2'f32 + rmax(-round, 0'f32) * 6'f32
  let inner = 1'f32 - mid * 0.8'f32
  let edge0 = inner - feather * 0.5'f32 * inner
  let edge1 = inner + feather * 0.5'f32 * (1.4'f32 - inner) + 0.02'f32
  for y in 0 ..< h:
    for x in 0 ..< w:
      let (dx, dy) = (abs((float32(x) + 0.5'f32 - cx) / ax), abs((float32(y) + 0.5'f32 - cy) / ay))
      let r = pow(pow(dx, pw) + pow(dy, pw), 1'f32 / pw)
      let m = smoothstep(edge0, edge1, r)
      if m <= 0'f32: continue
      var q = px[y * w + x]
      let l = clamp(luma([q[0], q[1], q[2]]), 0'f32, 1'f32)
      if style == "paintOverlay":
        let target = (if amt < 0'f32: 0'f32 else: 1'f32)
        let k = m * abs(amt)
        for c in 0 .. 2: q[c] += (target - q[c]) * k
      else:
        var k = pow(2'f32, amt * 2'f32 * m)
        if amt < 0'f32 and hl > 0'f32:
          # Highlights: let bright areas punch through a darkening vignette.
          let protect = smoothstep(0.6'f32, 1'f32, l) * hl
          k += (1'f32 - k) * protect
        if style == "colorPriority":
          let nl = l * k
          for c in 0 .. 2: q[c] += nl - l
        else:
          for c in 0 .. 2: q[c] *= k
      px[y * w + x] = q

let Tau32 = 6.2831855'f32 # Rust's f32::consts::TAU

proc develop*(px: var seq[array[4, float32]]; w, h: int; p: CameraRaw; float: bool) =
  ## Runs the Camera Raw pipeline on straight RGBA pixels (`w × h`, display-encoded RGB).
  ## `float` keeps values above 1 (32-bit documents). Raises `ValueError` when `px` doesn't
  ## hold `w × h` pixels.
  if px.len == 0 or p.isIdentity: return
  if w <= 0 or h <= 0 or px.len != w * h: raise newException(ValueError, "pixel buffer does not match its size")
  let ps = clamp(p.pixelScale, 0.01'f32, 1'f32)
  let long = float32(max(w, h))
  let n0 = w * h
  # 1. White balance + exposure in linear light.
  if not p.wbNeutral:
    var g = whiteBalanceGains(p.temperature, p.tint)
    let norm = luma(g)
    let e = pow(2'f32, p.exposure)
    for c in 0 .. 2: g[c] = g[c] / norm * e
    for q in px.mitems:
      for c in 0 .. 2: q[c] = linearToSrgb(rmax(srgbToLinear(q[c]) * g[c], 0'f32))
  # 2. Dehaze.
  if p.dehaze != 0'f32: dehaze(px, w, h, p.dehaze / 100'f32)
  # 3. Tone on luminance.
  if not p.toneNeutral:
    var l, lc = newSeq[float32](n0)
    for i in 0 ..< n0:
      l[i] = rmax(luma([px[i][0], px[i][1], px[i][2]]), 0'f32)
      lc[i] = rmin(l[i], 1'f32)
    let hasBase = p.highlights != 0'f32 or p.shadows != 0'f32
    let base = (if hasBase: guidedFast(lc, lc, w, h, max(asIdx(long * 0.015'f32), 2), 0.02'f32) else: @[])
    let clar = (if p.clarity != 0'f32: guidedFast(lc, lc, w, h, max(asIdx(long * 0.02'f32), 3), 0.005'f32) else: @[])
    let tex = (if p.texture != 0'f32: gauss(lc, w, h, rmax(long * 0.002'f32, 1.5'f32 * ps)) else: @[])
    let kCon = p.contrast / 100'f32 * 0.6'f32
    for i in 0 ..< n0:
      let l0 = l[i]
      var lv = lc[i]
      if hasBase:
        let bv = clamp(base[i], 0'f32, 1'f32)
        let hm = smoothstep(0.45'f32, 0.95'f32, bv)
        let sm = 1'f32 - smoothstep(0.05'f32, 0.5'f32, bv)
        lv *= pow(2'f32, p.highlights / 100'f32 * 1.2'f32 * hm + p.shadows / 100'f32 * 1.4'f32 * sm)
      if p.clarity != 0'f32:
        let e = 2'f32 * clamp(lv, 0'f32, 1'f32) - 1'f32
        let mw = 1'f32 - e * e
        lv += p.clarity / 100'f32 * 0.8'f32 * (lc[i] - clar[i]) * mw
      if p.texture != 0'f32:
        lv += p.texture / 100'f32 * 0.9'f32 * clamp(lc[i] - tex[i], -0.1'f32, 0.1'f32)
      if p.whites != 0'f32:
        lv += p.whites / 100'f32 * 0.25'f32 * smoothstep(0.4'f32, 1'f32, lv)
      if p.blacks != 0'f32:
        lv += p.blacks / 100'f32 * 0.15'f32 * (1'f32 - smoothstep(0'f32, 0.45'f32, lv))
      if kCon != 0'f32:
        let xx = clamp(lv, 0'f32, 1'f32)
        lv += -kCon * sin(Tau32 * xx) / Tau32
      let lv2 = rmax(lv, 0'f32) + (l0 - lc[i])
      let ratio = (if l0 > 1e-5'f32: lv2 / l0 else: 1'f32)
      for c in 0 .. 2:
        px[i][c] = (if l0 > 1e-5'f32: px[i][c] * ratio else: px[i][c] + (lv2 - l0))
  # 4–7: per-pixel colour stages.
  let needColor = p.vibrance != 0'f32 or p.saturation != 0'f32 or not p.curvesNeutral or not p.hslNeutral or not p.gradeNeutral
  if needColor:
    let hasPar = not p.parametricNeutral
    let par = (if hasPar: parametricLut(p, 1024) else: @[])
    proc pts(c: seq[array[2, float32]]): seq[float32] = (if isLinear(c): @[] else: curveLut(c, 1024))
    let master = pts(p.pointCurve)
    let chans = [pts(p.pointCurveRed), pts(p.pointCurveGreen), pts(p.pointCurveBlue)]
    let hsl = not p.hslNeutral
    let grade = not p.gradeNeutral
    proc wheelTint(wh: Wheel): array[3, float32] =
      let c = hsvToRgb(wh.hue, 1'f32, 1'f32)
      let l = luma(c)
      for k in 0 .. 2: result[k] = (c[k] - l) * wh.sat / 100'f32 * 0.3'f32
    let tints = [wheelTint(p.gradeShadows), wheelTint(p.gradeMidtones), wheelTint(p.gradeHighlights), wheelTint(p.gradeGlobal)]
    let blend = 0.15'f32 + clamp(p.gradeBlending, 0'f32, 100'f32) / 100'f32 * 0.35'f32
    let bal = clamp(p.gradeBalance, -100'f32, 100'f32) / 100'f32 * 0.25'f32
    let lums = [p.gradeShadows.lum, p.gradeMidtones.lum, p.gradeHighlights.lum, p.gradeGlobal.lum]
    for q in px.mitems:
      var c = [q[0], q[1], q[2]]
      if p.vibrance != 0'f32 or p.saturation != 0'f32:
        let l = luma(c)
        let (mx, mn) = (rmax(rmax(c[0], c[1]), c[2]), rmin(rmin(c[0], c[1]), c[2]))
        let sat = (if mx > 1e-6'f32: (mx - mn) / mx else: 0'f32)
        # Vibrance protects skin-ish hues (oranges) a little, like ACR.
        let hue = rgbToHsv(c)[0]
        let skin = clamp(1'f32 - abs(hue - 25'f32) / 25'f32, 0'f32, 1'f32) * 0.5'f32
        let k = 1'f32 + p.saturation / 100'f32 + p.vibrance / 100'f32 * (1'f32 - sat) * (1'f32 - skin)
        let kk = rmax(k, 0'f32)
        for ch in 0 .. 2: c[ch] = l + (c[ch] - l) * kk
      if hasPar:
        for ch in 0 .. 2: c[ch] = lutEval(par, c[ch])
      if master.len > 0:
        for ch in 0 .. 2: c[ch] = lutEval(master, c[ch])
      for ch in 0 .. 2:
        if chans[ch].len > 0: c[ch] = lutEval(chans[ch], c[ch])
      if hsl:
        let hsv = rgbToHsv([rmax(c[0], 0'f32), rmax(c[1], 0'f32), rmax(c[2], 0'f32)])
        let (hh, s, v) = (hsv[0], hsv[1], hsv[2])
        if s > 1e-4'f32:
          let wts = bandWeights(hh)
          var (dh, ds, dl) = (0'f32, 0'f32, 0'f32)
          for k in 0 ..< 8:
            dh += wts[k] * p.hslHue[k]
            ds += wts[k] * p.hslSat[k]
            dl += wts[k] * p.hslLum[k]
          let nh = hh + dh / 100'f32 * 30'f32
          let ns = clamp(s * (1'f32 + ds / 100'f32), 0'f32, 1'f32)
          var outc = hsvToRgb(nh, ns, v)
          # Keep the luminance, then apply the luminance shift weighted by saturation.
          let (l0, l1) = (luma(c), luma(outc))
          if l1 > 1e-6'f32:
            for ch in 0 .. 2: outc[ch] = outc[ch] * l0 / l1
          let k = pow(2'f32, dl / 100'f32 * 1'f32 * s)
          for ch in 0 .. 2: c[ch] = outc[ch] * k
      if grade:
        let l = clamp(luma(c), 0'f32, 1'f32)
        let sh = 1'f32 - smoothstep(0'f32, 0.5'f32 + bal + blend, l)
        let hi = smoothstep(0.5'f32 + bal - blend, 1'f32, l)
        let mid = rmax(1'f32 - sh - hi, 0'f32)
        let masks = [sh, mid, hi, 1'f32]
        for k in 0 .. 3:
          for ch in 0 .. 2: c[ch] += masks[k] * tints[k][ch]
          if lums[k] != 0'f32:
            let f = pow(2'f32, masks[k] * lums[k] / 100'f32 * 0.6'f32)
            for ch in 0 .. 2: c[ch] = c[ch] * f
      q[0] = c[0]
      q[1] = c[1]
      q[2] = c[2]
  # 8. Detail.
  if p.noiseLuminance > 0'f32 or p.noiseColor > 0'f32 or p.sharpenAmount > 0'f32: detail(px, w, h, p, ps)
  # 9. Effects.
  if p.vignetteAmount != 0'f32: vignette(px, w, h, p)
  if p.grainAmount > 0'f32:
    let cell = 1'f32 + p.grainSize / 25'f32
    let rough = p.grainRoughness / 100'f32
    let amt = p.grainAmount / 100'f32 * 0.12'f32
    let seed = uint64(p.seed)
    let pscale = 1'f32 / ps
    for y in 0 ..< h:
      for x in 0 ..< w:
        # Sample in full-resolution coordinates so previews match the result.
        let (fx, fy) = (float32(x) * pscale / cell, float32(y) * pscale / cell)
        let n = valueNoise(fx, fy, seed) * (1'f32 - rough) + (valueNoise(fx * 2.3'f32, fy * 2.3'f32, seed + 7) - 0'f32) * rough
        var q = px[y * w + x]
        let l = clamp(luma([q[0], q[1], q[2]]), 0'f32, 1'f32)
        let g = (n - 0.5'f32) * 2'f32 * amt * (0.4'f32 + 2.4'f32 * l * (1'f32 - l))
        for c in 0 .. 2: q[c] += g
        px[y * w + x] = q
  if not float:
    for q in px.mitems:
      for c in 0 .. 2: q[c] = clamp(q[c], 0'f32, 1'f32)

# ---------------------------------------------------------------- JSON (serde camelCase, defaults)

proc f32J(n: JsonNode; name: string): float32 =
  case n.kind
  of JInt: float32(n.getBiggestInt)
  of JFloat: float32(n.getFloat)
  else: raise newException(ValueError, "invalid type for " & name & ": expected f32")

proc arrJ[N: static int](n: JsonNode; name: string): array[N, float32] =
  if n.kind != JArray: raise newException(ValueError, "invalid type for " & name & ": expected an array")
  if n.len != N: raise newException(ValueError, "invalid length " & $n.len & " for " & name & ", expected " & $N)
  for i in 0 ..< N: result[i] = f32J(n[i], name)

proc curveJ(n: JsonNode; name: string): seq[array[2, float32]] =
  if n.kind != JArray: raise newException(ValueError, "invalid type for " & name & ": expected a sequence")
  for q in n: result.add arrJ[2](q, name)

const CameraRawFields = ["temperature", "tint", "exposure", "contrast", "highlights", "shadows", "whites", "blacks",
  "texture", "clarity", "dehaze", "vibrance", "saturation", "curveHighlights", "curveLights", "curveDarks", "curveShadows",
  "curveSplits", "pointCurve", "pointCurveRed", "pointCurveGreen", "pointCurveBlue", "hslHue", "hslSat", "hslLum",
  "gradeShadows", "gradeMidtones", "gradeHighlights", "gradeGlobal", "gradeBlending", "gradeBalance", "sharpenAmount",
  "sharpenRadius", "sharpenDetail", "sharpenMasking", "noiseLuminance", "noiseLuminanceDetail", "noiseColor",
  "noiseColorDetail", "grainAmount", "grainSize", "grainRoughness", "vignetteAmount", "vignetteMidpoint",
  "vignetteRoundness", "vignetteFeather", "vignetteHighlights", "vignetteStyle", "seed", "pixelScale"]
  ## serde's field order (a struct may also be written as a sequence of its fields)

proc seqAsObject(j: JsonNode; fields: openArray[string]; what: string): JsonNode =
  ## serde's sequence form of a struct: fields in order, missing trailing ones default.
  if j.len > fields.len: raise newException(ValueError, "invalid length " & $j.len & " for " & what)
  result = newJObject()
  for i in 0 ..< j.len: result[fields[i]] = j[i]

proc wheelFromJson*(j0: JsonNode): Wheel =
  ## A grading wheel from `{hue, sat, lum}` or `[hue, sat, lum]` (missing fields 0); raises
  ## `ValueError`.
  let j = (if j0.kind == JArray: seqAsObject(j0, ["hue", "sat", "lum"], "a colour grading wheel") else: j0)
  if j.kind != JObject: raise newException(ValueError, "a colour grading wheel is an object")
  for k, v in j:
    case k
    of "hue": result.hue = f32J(v, k)
    of "sat": result.sat = f32J(v, k)
    of "lum": result.lum = f32J(v, k)
    else: discard

proc cameraRawFromJson*(j0: JsonNode): CameraRaw =
  ## Settings from their JSON form (camelCase, missing fields take their defaults; or serde's
  ## sequence form); raises `ValueError` on a malformed value. Curves are not validated here (see
  ## `validate`).
  let j = (if j0.kind == JArray: seqAsObject(j0, CameraRawFields, "Camera Raw settings") else: j0)
  if j.kind != JObject: raise newException(ValueError, "Camera Raw settings must be an object")
  result = defaultCameraRaw()
  for k, v in j:
    case k
    of "temperature": result.temperature = f32J(v, k)
    of "tint": result.tint = f32J(v, k)
    of "exposure": result.exposure = f32J(v, k)
    of "contrast": result.contrast = f32J(v, k)
    of "highlights": result.highlights = f32J(v, k)
    of "shadows": result.shadows = f32J(v, k)
    of "whites": result.whites = f32J(v, k)
    of "blacks": result.blacks = f32J(v, k)
    of "texture": result.texture = f32J(v, k)
    of "clarity": result.clarity = f32J(v, k)
    of "dehaze": result.dehaze = f32J(v, k)
    of "vibrance": result.vibrance = f32J(v, k)
    of "saturation": result.saturation = f32J(v, k)
    of "curveHighlights": result.curveHighlights = f32J(v, k)
    of "curveLights": result.curveLights = f32J(v, k)
    of "curveDarks": result.curveDarks = f32J(v, k)
    of "curveShadows": result.curveShadows = f32J(v, k)
    of "curveSplits": result.curveSplits = arrJ[3](v, k)
    of "pointCurve": result.pointCurve = curveJ(v, k)
    of "pointCurveRed": result.pointCurveRed = curveJ(v, k)
    of "pointCurveGreen": result.pointCurveGreen = curveJ(v, k)
    of "pointCurveBlue": result.pointCurveBlue = curveJ(v, k)
    of "hslHue": result.hslHue = arrJ[8](v, k)
    of "hslSat": result.hslSat = arrJ[8](v, k)
    of "hslLum": result.hslLum = arrJ[8](v, k)
    of "gradeShadows": result.gradeShadows = wheelFromJson(v)
    of "gradeMidtones": result.gradeMidtones = wheelFromJson(v)
    of "gradeHighlights": result.gradeHighlights = wheelFromJson(v)
    of "gradeGlobal": result.gradeGlobal = wheelFromJson(v)
    of "gradeBlending": result.gradeBlending = f32J(v, k)
    of "gradeBalance": result.gradeBalance = f32J(v, k)
    of "sharpenAmount": result.sharpenAmount = f32J(v, k)
    of "sharpenRadius": result.sharpenRadius = f32J(v, k)
    of "sharpenDetail": result.sharpenDetail = f32J(v, k)
    of "sharpenMasking": result.sharpenMasking = f32J(v, k)
    of "noiseLuminance": result.noiseLuminance = f32J(v, k)
    of "noiseLuminanceDetail": result.noiseLuminanceDetail = f32J(v, k)
    of "noiseColor": result.noiseColor = f32J(v, k)
    of "noiseColorDetail": result.noiseColorDetail = f32J(v, k)
    of "grainAmount": result.grainAmount = f32J(v, k)
    of "grainSize": result.grainSize = f32J(v, k)
    of "grainRoughness": result.grainRoughness = f32J(v, k)
    of "vignetteAmount": result.vignetteAmount = f32J(v, k)
    of "vignetteMidpoint": result.vignetteMidpoint = f32J(v, k)
    of "vignetteRoundness": result.vignetteRoundness = f32J(v, k)
    of "vignetteFeather": result.vignetteFeather = f32J(v, k)
    of "vignetteHighlights": result.vignetteHighlights = f32J(v, k)
    of "vignetteStyle":
      if v.kind != JString: raise newException(ValueError, "invalid type for vignetteStyle: expected a string")
      result.vignetteStyle = v.getStr
    of "seed":
      if v.kind != JInt or v.getBiggestInt < 0 or v.getBiggestInt > int64(high(uint32)):
        raise newException(ValueError, "invalid value for seed: expected u32")
      result.seed = uint32(v.getBiggestInt)
    of "pixelScale": result.pixelScale = f32J(v, k)
    else: discard

proc j32(v: float32): JsonNode =
  ## An f32 as serde_json writes it (shortest round-trip form; null when not finite).
  if not v.isFinite: newJNull() else: newJFloat(parseFloat($v))

proc toJ*(w: Wheel): JsonNode = %*{"hue": j32(w.hue), "sat": j32(w.sat), "lum": j32(w.lum)}

proc toJ*(p: CameraRaw): JsonNode =
  ## The JSON form (camelCase, every field), as the Rust serializer writes it.
  proc arr(v: openArray[float32]): JsonNode =
    result = newJArray()
    for x in v: result.add j32(x)
  proc curve(c: seq[array[2, float32]]): JsonNode =
    result = newJArray()
    for q in c: result.add arr(q)
  result = newJObject()
  for (k, v) in [("temperature", p.temperature), ("tint", p.tint), ("exposure", p.exposure), ("contrast", p.contrast),
                 ("highlights", p.highlights), ("shadows", p.shadows), ("whites", p.whites), ("blacks", p.blacks),
                 ("texture", p.texture), ("clarity", p.clarity), ("dehaze", p.dehaze), ("vibrance", p.vibrance),
                 ("saturation", p.saturation), ("curveHighlights", p.curveHighlights), ("curveLights", p.curveLights),
                 ("curveDarks", p.curveDarks), ("curveShadows", p.curveShadows)]:
    result[k] = j32(v)
  result["curveSplits"] = arr(p.curveSplits)
  result["pointCurve"] = curve(p.pointCurve)
  result["pointCurveRed"] = curve(p.pointCurveRed)
  result["pointCurveGreen"] = curve(p.pointCurveGreen)
  result["pointCurveBlue"] = curve(p.pointCurveBlue)
  result["hslHue"] = arr(p.hslHue)
  result["hslSat"] = arr(p.hslSat)
  result["hslLum"] = arr(p.hslLum)
  result["gradeShadows"] = toJ(p.gradeShadows)
  result["gradeMidtones"] = toJ(p.gradeMidtones)
  result["gradeHighlights"] = toJ(p.gradeHighlights)
  result["gradeGlobal"] = toJ(p.gradeGlobal)
  for (k, v) in [("gradeBlending", p.gradeBlending), ("gradeBalance", p.gradeBalance), ("sharpenAmount", p.sharpenAmount),
                 ("sharpenRadius", p.sharpenRadius), ("sharpenDetail", p.sharpenDetail), ("sharpenMasking", p.sharpenMasking),
                 ("noiseLuminance", p.noiseLuminance), ("noiseLuminanceDetail", p.noiseLuminanceDetail),
                 ("noiseColor", p.noiseColor), ("noiseColorDetail", p.noiseColorDetail), ("grainAmount", p.grainAmount),
                 ("grainSize", p.grainSize), ("grainRoughness", p.grainRoughness), ("vignetteAmount", p.vignetteAmount),
                 ("vignetteMidpoint", p.vignetteMidpoint), ("vignetteRoundness", p.vignetteRoundness),
                 ("vignetteFeather", p.vignetteFeather), ("vignetteHighlights", p.vignetteHighlights)]:
    result[k] = j32(v)
  result["vignetteStyle"] = %p.vignetteStyle
  result["seed"] = %p.seed
  result["pixelScale"] = j32(p.pixelScale)
