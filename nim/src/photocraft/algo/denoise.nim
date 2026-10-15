## Reduce Noise: edge-preserving luminance smoothing, chroma smoothing guided by the luminance,
## and optional detail sharpening (port of `denoise.rs`). Smoothing uses the guided filter (He,
## Sun & Tang, ECCV 2010): O(1) per pixel whatever the radius, and edge-aware.

import std/math
import ../geom, ../color
import params, image, fxutil, util

type DenoiseSpec* = object
  strength*, preserve*, color*, sharpen*: float32
  jpeg*: bool

const
  MaxLumaR = 5
  MaxChromaR = 9

proc reach*(): float32 =
  ## Pixels read beyond an output tile (two box passes per guided filter, plus sharpening).
  float32(2 * MaxLumaR + 2 * MaxChromaR + 6)

proc guided(g, p: openArray[float32]; w, h, r: int; eps: float32): seq[float32] =
  ## Guided filter of `p` with guide `g` (both `w × h`), box radius `r`.
  let len = w * h
  var m = newSeq[float32](len * 4)
  for i in 0 ..< len:
    m[i * 4] = g[i]
    m[i * 4 + 1] = p[i]
    m[i * 4 + 2] = g[i] * g[i]
    m[i * 4 + 3] = g[i] * p[i]
  boxBlurN(m, w, h, 4, r)
  var ab = newSeq[float32](len * 2)
  for i in 0 ..< len:
    let (mg, mp, mgg, mgp) = (m[i * 4], m[i * 4 + 1], m[i * 4 + 2], m[i * 4 + 3])
    let v = rmax(mgg - mg * mg, 0)
    let a = (mgp - mg * mp) / (v + eps)
    ab[i * 2] = a
    ab[i * 2 + 1] = mp - a * mg
  boxBlurN(ab, w, h, 2, r)
  result = newSeq[float32](len)
  for i in 0 ..< len: result[i] = ab[i * 2] * g[i] + ab[i * 2 + 1]

type Model = enum mGray, mYcc, mLab, mViaRgb

proc reduceNoise*(src: Image; o: Rect; ctx: Ctx; spec: DenoiseSpec): seq[float32] =
  let n = src.ch
  let win = src.rect
  let (ww, wh) = (win.width, win.height)
  let len = ww * wh
  let s = clamp(spec.strength, 0'f32, 10'f32) / 10
  let col = clamp(spec.color, 0'f32, 100'f32) / 100
  let sh = clamp(spec.sharpen, 0'f32, 100'f32) / 100
  if (s <= 0 and col <= 0 and sh <= 0 and not spec.jpeg) or len == 0: return src.crop(o)
  # Decompose into luma + two chroma planes.
  let model = case ctx.mode
    of cmGrayscale, cmBitmap, cmDuotone: mGray
    of cmRgb: mYcc
    of cmLab: mLab
    else: mViaRgb
  var yv = newSeq[float32](len)
  var c1 = newSeq[float32](if model == mGray: 0 else: len)
  var c2 = newSeq[float32](if model == mGray: 0 else: len)
  var px: array[MaxC, float32]
  for i in 0 ..< len:
    let (x, y) = (win.x0 + i mod ww, win.y0 + i div ww)
    for c in 0 ..< n: px[c] = src.get(x, y, c)
    case model
    of mGray: yv[i] = px[0]
    of mLab:
      yv[i] = px[0]
      c1[i] = px[1]
      c2[i] = px[2]
    of mYcc, mViaRgb:
      let c = if model == mYcc: [px[0], px[1], px[2], 1'f32] else: rgba(ctx, px.toOpenArray(0, n - 1))
      let l = 0.299'f32 * c[0] + 0.587'f32 * c[1] + 0.114'f32 * c[2]
      yv[i] = l
      c1[i] = c[2] - l
      c2[i] = c[0] - l
  # JPEG deblocking: soften luma across the 8×8 grid lines (document-anchored).
  if spec.jpeg:
    let orig = yv
    for yy in 1 ..< max(wh - 1, 1):
      for xx in 1 ..< ww - 1:
        let (dx, dy) = (remEuclid(win.x0 + xx, 8), remEuclid(win.y0 + yy, 8))
        if dx == 0 or dx == 7 or dy == 0 or dy == 7:
          var a = 0'f32
          for j in 0 .. 2:
            for k in 0 .. 2: a += orig[(yy + j - 1) * ww + xx + k - 1]
          yv[yy * ww + xx] = 0.5'f32 * orig[yy * ww + xx] + 0.5'f32 * a / 9
  # Luminance: edge-preserving smoothing; Preserve Details shrinks the regularisation.
  if s > 0:
    let r = asUsize(float64(round(1 + clamp(spec.strength, 0'f32, 10'f32) * 0.4'f32)))
    let keep = clamp(spec.preserve, 0'f32, 100'f32) / 100
    let e0 = 0.004'f32 + 0.05'f32 * s
    let eps = e0 * e0 * (1.05'f32 - keep)
    let d = guided(yv, yv, ww, wh, min(r, MaxLumaR), eps)
    let k = sqrt(s)
    for i in 0 ..< len: yv[i] += (d[i] - yv[i]) * k
  # Chroma: guided by the cleaned luma so colour edges stay put.
  if col > 0 and model != mGray:
    let r = asUsize(float64(round(1 + col * 8)))
    let e0 = 0.01'f32 + 0.05'f32 * col
    let eps = e0 * e0
    for plane in [addr c1, addr c2]:
      let d = guided(yv, plane[], ww, wh, min(r, MaxChromaR), eps)
      for i in 0 ..< len: plane[][i] += (d[i] - plane[][i]) * col
  # Sharpen Details: unsharp mask on luma.
  if sh > 0:
    var bl = yv
    gaussBlurN(bl, ww, wh, 1, 1)
    for i in 0 ..< len: yv[i] += (yv[i] - bl[i]) * sh * 1.2'f32
  result = src.crop(o)
  var i = 0
  while i * n + n <= result.len:
    let at = i * n
    let (x, y) = xy(o, i)
    let k = (y - win.y0) * ww + (x - win.x0)
    case model
    of mGray: result[at] = yv[k]
    of mLab:
      result[at] = yv[k]
      result[at + 1] = c1[k]
      result[at + 2] = c2[k]
    of mYcc, mViaRgb:
      let l = yv[k]
      let (b, r) = (c1[k] + l, c2[k] + l)
      let g = (l - 0.299'f32 * r - 0.114'f32 * b) / 0.587'f32
      if model == mYcc:
        result[at] = r
        result[at + 1] = g
        result[at + 2] = b
      else:
        let a = if ctx.alpha: result[at + n - 1] else: 1'f32
        setRgba(ctx, result.toOpenArray(at, at + n - 1), [r, g, b, a])
    inc i
