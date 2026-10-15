## Photographic relight: recover shading with a blur (Retinex) and replace it with a Lambertian
## lamp whose direction comes from angle / elevation (port of `relight.rs`).

import std/math
import ../geom
import params, image, fxutil, util

const
  SigmaMin* = 2'f32   ## softness maps onto this σ range
  SigmaMax* = 96'f32
  StepMax = 32'f32    ## finite-difference step is σ, but never more than this many pixels
  MaxLumaPixels = 8_000_000  ## luma field allocated for one call; larger outputs are split in bands

type RelightParams* = object
  ## Dialog units (degrees / 0–100), matching the command params.
  angle*, elevation*, intensity*, ambient*, warmth*, softness*: float32

proc sigma*(softness: float32; bounds: Rect): float32 =
  ## Blur σ from softness and the reference bounds, clamped to `[SigmaMin, SigmaMax]`.
  let side = float32(min(bounds.width, bounds.height))
  let s = (clamp(softness, 1'f32, 100'f32) / 100) * 0.08'f32 * rmax(side, 1)
  clamp(s, SigmaMin, SigmaMax)

proc stepForSigma*(sig: float32): int = asI32(float64(clamp(round(sig), 1'f32, StepMax)))

proc readRadius*(sig: float32): int =
  ## Pixels read outside an output tile: `3σ + 1` of the shading blur plus the step.
  let blur = asI32(float64(ceil(rmax(sig, 0) * 3))) + 1
  satAdd(blur, stepForSigma(sig))

proc haloRadiusMax*(): int = readRadius(SigmaMax)
  ## Conservative halo when the document size is unknown.

proc haloRadius*(softness: float32; bounds: Rect): int =
  ## Halo for a concrete document size (`haloRadiusMax` when `bounds` is empty).
  if bounds.isEmpty: haloRadiusMax() else: readRadius(sigma(softness, bounds))

proc norm3(v: array[3, float32]): array[3, float32] =
  let m = sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2])
  if not m.isFinite or m < 1e-9'f32: [0'f32, 0, 1] else: [v[0] / m, v[1] / m, v[2] / m]

proc lightDir(angle, elevation: float32): array[3, float32] =
  ## Infinite-light direction: 0° from the right, 90° from above (image space, y down).
  let a = degToRad(angle)
  let (s, co) = (sin(a), cos(a))
  let e = degToRad(clamp(elevation, 0'f32, 90'f32))
  norm3([co * cos(e), -s * cos(e), sin(e)])

proc lightColor(warmth: float32): array[3, float32] =
  let w = clamp(warmth, -100'f32, 100'f32) / 100
  if w >= 0: [1 + 0.15'f32 * w, 1 - 0.05'f32 * w, 1 - 0.25'f32 * w]
  else:
    let t = -w
    [1 - 0.20'f32 * t, 1 - 0.10'f32 * t, 1 + 0.15'f32 * t]

proc rec709Y(c: array[4, float32]): float32 = 0.2126'f32 * c[0] + 0.7152'f32 * c[1] + 0.0722'f32 * c[2]

proc finiteAll(v: openArray[float32]): bool =
  for x in v:
    if not x.isFinite: return false
  true

proc sampleY(buf: openArray[float32]; w, h: int; r: Rect; x, y: int): float32 =
  if r.isEmpty or w == 0 or h == 0: return 0.04
  let xx = clamp(x, r.x0, max(r.x1 - 1, r.x0))
  let yy = clamp(y, r.y0, max(r.y1 - 1, r.y0))
  let (ox, oy) = (xx - r.x0, yy - r.y0)
  if ox >= w or oy >= h: return 0.04
  let i = oy * w + ox
  if i < buf.len and buf[i].isFinite: buf[i] else: 0.04

proc blurChannel(src: openArray[float32]; w, h: int; sigma: float32): seq[float32] =
  ## Approximate Gaussian of a single-channel field; non-finite samples are held out via a
  ## parallel weight channel so they do not poison neighbours.
  let n = w * h
  if n == 0 or src.len < n: return newSeqWithValue(n, 0.04'f32)
  var packed = newSeq[float32](n * 2)
  for i in 0 ..< n:
    let v = src[i]
    if not v.isFinite: continue
    packed[i * 2] = v
    packed[i * 2 + 1] = 1
  gaussBlurN(packed, w, h, 2, sigma)
  result = newSeq[float32](n)
  for i in 0 ..< n:
    let (acc, wgt) = (packed[i * 2], packed[i * 2 + 1])
    result[i] = if wgt > 1e-6'f32 and acc.isFinite and wgt.isFinite: acc / wgt else: 0.5'f32

proc readRgba(src: Image; ctx: Ctx; x, y: int): array[4, float32] =
  let n = min(src.ch, MaxC)
  var tmp: array[MaxC, float32]
  for c in 0 ..< n: tmp[c] = src.get(x, y, c)
  rgba(ctx, tmp.toOpenArray(0, n - 1))

proc relightRect(src: Image; o: Rect; ctx: Ctx; p: RelightParams; sig: float32; step: int): seq[float32] =
  let n = src.ch
  result = src.crop(o)
  # Finite differences at the shading scale (σ), not 1 px.
  const Scale = 2'f32
  let yRect = o.inflate(readRadius(sig))
  if yRect.isEmpty: return
  let (yw, yh) = (yRect.width, yRect.height)
  if yw * yh <= 0: return
  var ybuf = newSeqWithValue(yw * yh, 0.5'f32)
  for y in yRect.y0 ..< yRect.y1:
    for x in yRect.x0 ..< yRect.x1:
      let slot = (y - yRect.y0) * yw + (x - yRect.x0)
      let c = readRgba(src, ctx, x, y)
      if not finiteAll(c):
        ybuf[slot] = NaN.float32
        continue
      let a = if ctx.alpha: c[3] else: 1'f32
      ybuf[slot] = if a > 0 and a.isFinite: rec709Y(c) else: NaN.float32
  let sbuf = blurChannel(ybuf, yw, yh, sig)
  let ldir = lightDir(p.angle, p.elevation)
  let lcol = lightColor(p.warmth)
  let amb = clamp(if p.ambient.isFinite: p.ambient else: 0'f32, 0'f32, 100'f32) / 100
  let inten = clamp(p.intensity, 0'f32, 100'f32) / 100
  var i = 0
  while i * n + n <= result.len:
    let at = i * n
    let (x, y) = xy(o, i)
    block pixel:
      if not finiteAll(result.toOpenArray(at, at + n - 1)): break pixel
      let a = if ctx.alpha: result[at + n - 1] else: 1'f32
      if ctx.alpha and a <= 0: break pixel
      let c = rgba(ctx, result.toOpenArray(at, at + n - 1))
      if not finiteAll(c): break pixel
      let s0 = rmax(sampleY(sbuf, yw, yh, yRect, x, y), 0.04'f32)
      let nx = (sampleY(sbuf, yw, yh, yRect, x - step, y) - sampleY(sbuf, yw, yh, yRect, x + step, y)) * Scale
      let ny = (sampleY(sbuf, yw, yh, yRect, x, y - step) - sampleY(sbuf, yw, yh, yRect, x, y + step)) * Scale
      let nrm = norm3([nx, ny, 1'f32])
      let ndl = rmax(nrm[0] * ldir[0] + nrm[1] * ldir[1] + nrm[2] * ldir[2], 0)
      let shade = amb + inten * ndl
      var outC = c
      for k in 0 ..< 3: outC[k] = (c[k] / s0) * shade * lcol[k]
      if not finiteAll(outC): break pixel
      setRgba(ctx, result.toOpenArray(at, at + n - 1), outC)
      if ctx.alpha: result[at + n - 1] = a
    inc i

proc lumaBandRows(o: Rect; reach: int): int =
  ## Horizontal strip height so the inflated luma field stays within `MaxLumaPixels` (0: none).
  let margin = 2 * max(reach, 0)
  let yw = o.width + margin
  let yh = o.height + margin
  if yw == 0: return 0
  let n = yw * yh
  if n <= 0 or n <= MaxLumaPixels: return 0
  let maxYh = max(MaxLumaPixels div yw, 1)
  let inner = min(max(maxYh - margin, 1), int(high(int32)))
  if inner >= o.height: 0 else: max(inner, 1)

proc relight*(src: Image; o: Rect; ctx: Ctx; p: RelightParams): seq[float32] =
  ## Relight `o` of `src`. Identity when `intensity` is 0 (or not finite).
  let n = src.ch
  if o.isEmpty or n == 0: return src.crop(o)
  if not p.intensity.isFinite or p.intensity == 0: return src.crop(o)
  let sig = sigma(p.softness, ctx.bounds)
  let step = stepForSigma(sig)
  let bandH = lumaBandRows(o, readRadius(sig))
  if bandH == 0: return relightRect(src, o, ctx, p, sig, step)
  result = src.crop(o)
  var y = o.y0
  while y < o.y1:
    let y1 = min(satAdd(y, bandH), o.y1)
    let band = rect(o.x0, y, o.x1, y1)
    if band.isEmpty: break
    let part = relightRect(src, band, ctx, p, sig, step)
    let w = o.width * n
    for yy in band.y0 ..< band.y1:
      let so = (yy - band.y0) * w
      let d = (yy - o.y0) * w
      for k in 0 ..< w: result[d + k] = part[so + k]
    y = y1
