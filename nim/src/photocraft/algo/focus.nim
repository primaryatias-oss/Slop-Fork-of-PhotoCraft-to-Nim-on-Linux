## Focus Area (heuristic): selects the in-focus parts of an image (port of `segment/focus.rs`).
##
## Sharpness is local Laplacian energy (Nayar & Nakagawa, "Shape from Focus", PAMI 1994) on a
## working copy, normalised by its 99th percentile. `noise` subtracts a noise floor; `range` (0–1,
## Photoshop's "In-Focus Range") lowers the threshold as it grows. The thresholded map is smoothed
## with a guided filter (image as guide) so it follows object edges, cleaned, and upsampled with a
## soft edge.

import std/[math, options, algorithm]
import ../geom
import ./util, ./segment, ./matting

const WorkPx* = 1_000_000 ## working-resolution budget (pixels)

proc sharpness*(img: RgbImage; noise: float32): seq[float32] =
  ## Per-pixel normalised sharpness (`0..=1`) of `img`.
  let (w, h) = (img.w, img.h)
  if w * h == 0: return @[]
  var y = newSeq[float32](w * h)
  for i, p in img.px: y[i] = 0.299'f32 * p[0] + 0.587'f32 * p[1] + 0.114'f32 * p[2]
  proc at(x, yy: int): float32 = y[yy * w + x]
  var lap = newSeq[float32](w * h)
  for yy in 0 ..< h:
    for x in 0 ..< w:
      let c = at(x, yy)
      lap[yy * w + x] = abs(2'f32 * c - at(max(x - 1, 0), yy) - at(min(x + 1, w - 1), yy)) +
                        abs(2'f32 * c - at(x, max(yy - 1, 0)) - at(x, min(yy + 1, h - 1)))
  let r = max(min(w, h) div 60, 3)
  let e = boxMean(lap, w, h, r)
  var sorted = e
  let k = min(asUsize(float64(float32(sorted.len) * 0.99'f32)), max(sorted.len - 1, 0))
  sorted.sort(proc(a, b: float32): int = totalCmp(a, b))
  let p99 = sorted[k]
  let floor = clamp(noise, 0'f32, 1'f32) * 0.5'f32
  result = newSeq[float32](e.len)
  for i, v in e: result[i] = rmax(rmin(v / rmax(p99, 1e-6'f32), 1'f32) - floor, 0'f32) / rmax(1'f32 - floor, 1e-3'f32)

proc focusArea*(sampler: Sampler; canvas: Rect; range, noise: float32): Option[Region] =
  ## Focus Area selection over `canvas`.
  if canvas.width < 4 or canvas.height < 4: return none(Region)
  let step = scaleFor(canvas, WorkPx)
  let img = sampler.rgbScaled(canvas, step)
  let (w, h) = (img.w, img.h)
  let s = sharpness(img, noise)
  let q = 1'f32 - clamp(range, 0'f32, 1'f32)
  let t = 0.05'f32 + 0.9'f32 * (q * q)
  var bin = newSeq[float32](s.len)
  for i, v in s: bin[i] = (if v >= t: 1'f32 else: 0'f32)
  let r = max(min(w, h) div 50, 2)
  let smoothv = guidedFilterColor(img, bin, r, 1e-2)
  var mask = newSeq[bool](smoothv.len)
  for i, v in smoothv: mask[i] = v >= 0.5'f32
  let cleaned = cleanMask(mask, w, h, 0.05, 0.02)
  var any = false
  for v in cleaned:
    if v: any = true
  if not any: return none(Region)
  # Soft upsampling: bilinear field, steepened to about a working pixel of transition.
  var field = newSeq[float32](cleaned.len)
  for i, m in cleaned: field[i] = (if m: clamp(smoothv[i], 0.5'f32, 1'f32) else: clamp(smoothv[i], 0'f32, 0.5'f32))
  let (fw, fh) = (canvas.width, canvas.height)
  let st = float32(step)
  var outp = newSeq[uint8](fw * fh)
  for yy in 0 ..< fh:
    let fy = clamp((float32(yy) + 0.5'f32) / st - 0.5'f32, 0'f32, float32(h - 1))
    let (y0, ty) = (asIdx(floor(fy)), fy - trunc(fy))
    let y1 = min(y0 + 1, h - 1)
    for xx in 0 ..< fw:
      let fx = clamp((float32(xx) + 0.5'f32) / st - 0.5'f32, 0'f32, float32(w - 1))
      let (x0, tx) = (asIdx(floor(fx)), fx - trunc(fx))
      let x1 = min(x0 + 1, w - 1)
      let top = field[y0 * w + x0] * (1'f32 - tx) + field[y0 * w + x1] * tx
      let bot = field[y1 * w + x0] * (1'f32 - tx) + field[y1 * w + x1] * tx
      let v = top * (1'f32 - ty) + bot * ty
      outp[yy * fw + xx] = asU8(round(clamp((v - 0.5'f32) * 3'f32 + 0.5'f32, 0'f32, 1'f32) * 255'f32))
  trimRegion(Region(bbox: canvas, mask: outp))
