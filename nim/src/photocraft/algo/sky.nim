## Select › Sky: classical sky segmentation, no learned model (port of `segment/sky.rs`).
##
## 1. At a reduced working resolution every pixel gets a *sky likelihood*: a colour prior (blue
##    and bright, or bright and unsaturated for overcast / cloud, or a bright warm glow for
##    sunsets) times a smoothness term (sky has little gradient energy).
## 2. Seeds are likely-sky pixels in the top rows; the sky region grows from them through
##    4-connected neighbours that stay likely-sky and change colour gradually.
## 3. Sky seen through gaps is added when it fits a colour model (GMM) of the grown sky. Small
##    holes are filled.
## 4. The binary working-resolution mask is upsampled and refined at full resolution with a guided
##    filter on the luminance, tile by tile and only where the mask is mixed.

import std/[math, options, algorithm]
import ../geom
import ./util, ./segment, ./matting

const WorkPx* = 160_000 ## working-resolution budget (pixels)

type SkyParams* = object
  threshold*: float32 ## 0–100: higher is stricter
  softness*: float32  ## 0–100: edge softness of the refined mask

proc defaultSkyParams*(): SkyParams = SkyParams(threshold: 50, softness: 50)

proc smooth(e0, e1, x: float32): float32 {.inline.} =
  let t = clamp((x - e0) / (e1 - e0), 0'f32, 1'f32)
  t * t * (3'f32 - 2'f32 * t)

proc luma(p: array[3, float32]): float32 {.inline.} = 0.299'f32 * p[0] + 0.587'f32 * p[1] + 0.114'f32 * p[2]

proc colourPrior*(p: array[3, float32]): float32 =
  ## How sky-like a colour is on its own (0–1).
  let (r, g, b) = (p[0], p[1], p[2])
  let y = luma(p)
  let mx = rmax(rmax(r, g), b)
  let mn = rmin(rmin(r, g), b)
  let sat = (if mx > 1e-4'f32: (mx - mn) / mx else: 0'f32)
  # Blue sky: blue dominates red clearly and isn't far below green (excludes foliage).
  let blue = smooth(0.02, 0.12, b - r) * smooth(-0.12, 0, b - g) * smooth(0.18, 0.4, y)
  # Overcast / clouds / haze: bright and nearly neutral.
  let white = smooth(0.5, 0.75, y) * (1'f32 - smooth(0.12, 0.3, sat))
  # Sunset glow: bright and warm, kept weaker (warm walls are common too).
  let warm = 0.6'f32 * smooth(0.55, 0.8, y) * smooth(0, 0.1, r - b) * (1'f32 - smooth(0.05, 0.25, g - b - 0.2'f32))
  rmax(rmax(blue, white), warm)

proc likelihood*(img: RgbImage): seq[float32] =
  ## Per-pixel sky likelihood of a working-resolution image (colour prior × smoothness).
  let (w, h) = (img.w, img.h)
  var y = newSeq[float32](w * h)
  for i, p in img.px: y[i] = luma(p)
  proc at(x, yy: int): float32 = y[clamp(yy, 0, h - 1) * w + clamp(x, 0, w - 1)]
  var grad = newSeq[float32](w * h)
  for yy in 0 ..< h:
    for x in 0 ..< w:
      let gx = (at(x + 1, yy - 1) + 2'f32 * at(x + 1, yy) + at(x + 1, yy + 1)) - (at(x - 1, yy - 1) + 2'f32 * at(x - 1, yy) + at(x - 1, yy + 1))
      let gy = (at(x - 1, yy + 1) + 2'f32 * at(x, yy + 1) + at(x + 1, yy + 1)) - (at(x - 1, yy - 1) + 2'f32 * at(x, yy - 1) + at(x + 1, yy - 1))
      grad[yy * w + x] = sqrt(gx * gx + gy * gy) / 8'f32
  let tex = boxMean(grad, w, h, 2)
  result = newSeq[float32](w * h)
  for i, p in img.px: result[i] = colourPrior(p) * (1'f32 - smooth(0.015, 0.07, tex[i]))

proc skyMaskLow*(img: RgbImage; threshold: float32): Option[seq[bool]] =
  ## Binary sky mask at working resolution (none when no sky touches the top of the image).
  let (w, h) = (img.w, img.h)
  if w < 2 or h < 2: return none(seq[bool])
  let like = likelihood(img)
  let t = 0.12'f32 + 0.6'f32 * (clamp(threshold, 0'f32, 100'f32) / 100'f32)
  # Seeds: likely sky in the top rows.
  let top = max(h div 40, 1)
  var mask = newSeq[bool](w * h)
  var stack: seq[int]
  for i in 0 ..< top * w:
    if like[i] > t:
      mask[i] = true
      stack.add i
  if stack.len * 20 < top * w: return none(seq[bool]) # less than 5 % of the top edge looks like sky
  # Region growing with a gradual-change constraint.
  const maxStep = 0.12'f32
  while stack.len > 0:
    let i = stack.pop
    let (x, y) = (i mod w, i div w)
    for (ok, j) in [(x > 0, i - 1), (x + 1 < w, i + 1), (y > 0, i - w), (y + 1 < h, i + w)]:
      if not ok or mask[j] or like[j] <= t: continue
      let (a, b) = (img.px[i], img.px[j])
      let d = sqrt((a[0] - b[0]) * (a[0] - b[0]) + (a[1] - b[1]) * (a[1] - b[1]) + (a[2] - b[2]) * (a[2] - b[2]))
      if d < maxStep:
        mask[j] = true
        stack.add j
  # Sky seen through gaps: likely-sky pixels that fit the grown sky's colours.
  var samples: seq[array[3, float32]]
  for i, m in mask:
    if m: samples.add img.px[i]
  if samples.len >= 16:
    let g = fit(subsample(samples, 4000), 3, 1e-4)
    if g.isSome:
      var nl: seq[float32]
      for p in subsample(samples, 2000): nl.add g.get.negLog(p)
      nl.sort(proc(a, b: float32): int = totalCmp(a, b))
      let cut = nl[min(nl.len * 95 div 100, nl.len - 1)]
      for i in 0 ..< mask.len:
        if not mask[i] and like[i] > t + 0.1'f32 and g.get.negLog(img.px[i]) <= cut: mask[i] = true
  let cleaned = cleanMask(mask, w, h, 0.002, 0.01)
  for v in cleaned:
    if v: return some(cleaned)
  none(seq[bool])

proc selectSky*(sampler: Sampler; canvas: Rect; p: SkyParams): Option[Region] =
  ## Selects the sky over `canvas`. None if no sky was found.
  if canvas.width < 4 or canvas.height < 4: return none(Region)
  let step = scaleFor(canvas, WorkPx)
  let img = sampler.rgbScaled(canvas, step)
  let (lw, lh) = (img.w, img.h)
  let lowO = skyMaskLow(img, p.threshold)
  if lowO.isNone: return none(Region)
  var lowf = newSeq[float32](lowO.get.len)
  for i, v in lowO.get: lowf[i] = (if v: 1'f32 else: 0'f32)
  let soft = clamp(p.softness, 0'f32, 100'f32) / 100'f32
  let r = max(asUsize(float64(ceil(float32(step) * (1'f32 + 2'f32 * soft)))), 3)
  let eps = 1e-4'f32 + 4e-3'f32 * soft
  # Contrast after filtering: soft 0 → narrow ramp, 1 → the filter output as is.
  let k = 0.08'f32 + 0.42'f32 * soft
  let (w, h) = (canvas.width, canvas.height)
  proc coarse(x, y: int): float32 =
    # Bilinear upsample of the working mask (pixel centres).
    let fx = (float32(x - canvas.x0) + 0.5'f32) / float32(step) - 0.5'f32
    let fy = (float32(y - canvas.y0) + 0.5'f32) / float32(step) - 0.5'f32
    let (x0, y0) = (floor(fx), floor(fy))
    let (ax, ay) = (fx - x0, fy - y0)
    proc g(xx, yy: float32): float32 =
      lowf[asIdx(clamp(yy, 0'f32, float32(lh) - 1'f32)) * lw + asIdx(clamp(xx, 0'f32, float32(lw) - 1'f32))]
    let top = g(x0, y0) * (1'f32 - ax) + g(x0 + 1'f32, y0) * ax
    let bot = g(x0, y0 + 1'f32) * (1'f32 - ax) + g(x0 + 1'f32, y0 + 1'f32) * ax
    top * (1'f32 - ay) + bot * ay
  const T = 256
  var mask = newSeq[uint8](w * h)
  var ty = canvas.y0
  while ty < canvas.y1:
    var tx = canvas.x0
    while tx < canvas.x1:
      let t = rect(tx, ty, min(tx + T, canvas.x1), min(ty + T, canvas.y1))
      tx += T
      let win = t.inflate(r).intersect(canvas)
      let (ww, wh) = (win.width, win.height)
      var pm = newSeq[float32](ww * wh)
      for i in 0 ..< ww * wh: pm[i] = coarse(win.x0 + i mod ww, win.y0 + i div ww)
      var (lo, hi) = (F32Max, -F32Max)
      for v in pm:
        lo = rmin(lo, v)
        hi = rmax(hi, v)
      var q: seq[float32]
      let uniform = hi <= 0'f32 or lo >= 1'f32
      if not uniform:
        var guide = newSeq[float32](ww * wh)
        for i, px in sampler.rgb(win).px: guide[i] = luma(px)
        q = guidedFilterGray(guide, pm, ww, wh, r, eps)
      for y in t.y0 ..< t.y1:
        let o = (y - canvas.y0) * w + (t.x0 - canvas.x0)
        for x in t.x0 ..< t.x1:
          mask[o + x - t.x0] = (if uniform: (if lo >= 1'f32: 255'u8 else: 0'u8)
                                else: asU8(round(smooth(0.5'f32 - k, 0.5'f32 + k, q[(y - win.y0) * ww + (x - win.x0)]) * 255'f32)))
    ty += T
  trimRegion(Region(bbox: canvas, mask: mask))
