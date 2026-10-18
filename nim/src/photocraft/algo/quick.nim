## Quick Selection brush, behaving like Photoshop's (port of `segment/quick.rs`).
##
## Photoshop 25.4 was measured as a black box (synthetic images and public-domain paintings, no
## Photoshop code or assets): the brush footprint is always selected; beyond it the selection
## grows to image edges, but every selected pixel costs a little, so a click selects a whole
## object only when it is small enough to be worth it for that brush; a stroke is one set of
## seeds; colours elsewhere make no difference; it works on the whole document up to 512 × 512
## pixels and at exactly half resolution above, the mask then scaled up bilinearly.
##
## The model is one min cut (Boykov & Jolly, ICCV 2001) over the working image minimising
## `Σ_{selected p beyond the footprint} AREA·(1 + DIST·d(p)^POW) + Σ_{cut p~q} w(|I_p − I_q|) / |p − q|`
## with `w(D) = FLOOR + 1 / (1 + (D / SIGMA)^K)`, the (slightly grown) footprint hard foreground
## and `d` the distance from it. Larger windows are cut coarse to fine (Lombaert, Sun, Grady & Xu,
## "A Multilevel Banded Graph Cuts Method for Fast Image Segmentation", ICCV 2005).

import std/[math, options]
import ../geom
import ./util, ./segment, ./selection_distance

type
  Tuning* = object
    ## The model's constants; `defaultTuning` is the fit to Photoshop.
    area*, dist*, pow*: float32 ## cost of each selected pixel beyond the footprint
    floor*, sigma*, k*: float32 ## cut weight between neighbours
    grow*, growFrac*: float32  ## the footprint grows by `grow + growFrac * radius` working pixels
    directPx*: int             ## larger working windows are cut coarse to fine

proc defaultTuning*(): Tuning =
  Tuning(area: 0.004079, dist: 0.002651, pow: 0.98885, floor: 0.018847, sigma: 0.048450, k: 1.98549,
         grow: 1.69371, growFrac: 0.103003, directPx: 300_000)

const Band = 3'f32 ## band half-width (pixels) re-cut at each finer level

proc workingStep*(w, h: int): int =
  ## The working step for a document of `w × h` pixels: the whole document up to 512 × 512
  ## pixels, half resolution above.
  if w * h <= 512 * 512: 1 else: 2

proc strokeMask*(pts: openArray[(float32, float32)]; radius: float32; w, h: int): seq[bool] =
  ## Pixels within `radius` (working pixels) of the polyline `pts`, plus each point's own pixel.
  result = newSeq[bool](w * h)
  var segs: seq[((float32, float32), (float32, float32))]
  if pts.len == 1: segs.add (pts[0], pts[0])
  else:
    for i in 1 ..< pts.len: segs.add (pts[i - 1], pts[i])
  for (a, b) in segs:
    let x0 = asIdx(rmax(floor(rmin(a[0], b[0]) - radius), 0'f32))
    let y0 = asIdx(rmax(floor(rmin(a[1], b[1]) - radius), 0'f32))
    let x1 = min(asIdx(rmax(ceil(rmax(a[0], b[0]) + radius), 0'f32)), w)
    let y1 = min(asIdx(rmax(ceil(rmax(a[1], b[1]) + radius), 0'f32)), h)
    let (dx, dy) = (b[0] - a[0], b[1] - a[1])
    let len2 = dx * dx + dy * dy
    for y in y0 ..< y1:
      for x in x0 ..< x1:
        let (px, py) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
        let t = (if len2 > 0'f32: clamp(((px - a[0]) * dx + (py - a[1]) * dy) / len2, 0'f32, 1'f32) else: 0'f32)
        let (qx, qy) = (a[0] + t * dx - px, a[1] + t * dy - py)
        if qx * qx + qy * qy <= radius * radius: result[y * w + x] = true
  for (x, y) in pts:
    if x >= 0'f32 and y >= 0'f32 and asIdx(x) < w and asIdx(y) < h: result[asIdx(y) * w + asIdx(x)] = true

proc upsample*(low: seq[bool]; lw, lh, step, w, h: int): seq[uint8] =
  ## Bilinear upsampling (pixel centres aligned, edges clamped) of a working-resolution selection
  ## to 8-bit coverage, as Photoshop scales its half-resolution result.
  if step == 1:
    for b in low: result.add(if b: 255'u8 else: 0'u8)
    return
  proc at(x, y: int64): float32 =
    if low[int(clamp(y, 0'i64, int64(lh) - 1)) * lw + int(clamp(x, 0'i64, int64(lw) - 1))]: 1'f32 else: 0'f32
  let s = float32(step)
  result = newSeq[uint8](w * h)
  for y in 0 ..< h:
    let fy = (float32(y) + 0.5'f32) / s - 0.5'f32
    let (iy, ty) = (int64(floor(fy)), fy - floor(fy))
    for x in 0 ..< w:
      let fx = (float32(x) + 0.5'f32) / s - 0.5'f32
      let (ix, tx) = (int64(floor(fx)), fx - floor(fx))
      let v = (at(ix, iy) * (1'f32 - tx) + at(ix + 1, iy) * tx) * (1'f32 - ty) +
              (at(ix, iy + 1) * (1'f32 - tx) + at(ix + 1, iy + 1) * tx) * ty
      result[y * w + x] = asU8(round(v * 255'f32))

proc cutLevels(img: RgbImage; fixed: seq[uint8]; costFg: seq[float32]; weight: proc(d2: float32): float32;
               scale: float32; directPx: int; optimized: bool): seq[bool] =
  ## Min cut with only foreground costs, coarse to fine above `directPx` pixels.
  let (w, h) = (img.w, img.h)
  let zeros = newSeq[float32](w * h)
  let sc = scale
  let wf = proc(d2: float32): float32 = sc * weight(d2)
  if w * h <= directPx or w < 8 or h < 8:
    return (if optimized: gridCutWith(img, costFg, zeros, fixed, wf) else: gridCutReference(img, costFg, zeros, fixed, wf))
  let (cw, ch) = ((w + 1) div 2, (h + 1) div 2)
  let coarseImg = img.downsample(2)
  var cfix = newSeq[uint8](cw * ch)
  var ccost = newSeq[float32](cw * ch)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let c = (y div 2) * cw + x div 2
      ccost[c] += costFg[y * w + x]
      if fixed[y * w + x] == HardFg: cfix[c] = HardFg
  let coarse = cutLevels(coarseImg, cfix, ccost, weight, scale * 2'f32, directPx, optimized)
  var proj = newSeq[bool](w * h)
  for i in 0 ..< w * h: proj[i] = coarse[(i div w div 2) * cw + (i mod w) div 2]
  # Band: within `Band` of the projected boundary.
  var edge = newSeq[bool](w * h)
  var anyEdge = false
  for i in 0 ..< w * h:
    let (x, y) = (i mod w, i div w)
    let v = proj[i]
    edge[i] = (x > 0 and proj[i - 1] != v) or (x + 1 < w and proj[i + 1] != v) or (y > 0 and proj[i - w] != v) or
              (y + 1 < h and proj[i + w] != v)
    anyEdge = anyEdge or edge[i]
  if not anyEdge:
    result = newSeq[bool](w * h)
    for i in 0 ..< w * h: result[i] = proj[i] or fixed[i] == HardFg
    return
  let near = edt(edge, w, h)
  var banded = newSeq[uint8](w * h)
  for i in 0 ..< w * h:
    banded[i] = (if fixed[i] == HardFg: HardFg elif near[i] <= Band: Free elif proj[i]: HardFg else: HardBg)
  if optimized: gridCutWith(img, costFg, zeros, banded, wf) else: gridCutReference(img, costFg, zeros, banded, wf)

proc selectWorkingUsing(img: RgbImage; seeds: seq[bool]; radius: float32; t: Tuning; optimized: bool): seq[bool] =
  let (w, h) = (img.w, img.h)
  let d = edt(seeds, w, h)
  let grow = rmax(t.grow + t.growFrac * radius, 0'f32)
  var costFg = newSeq[float32](d.len)
  var fixed = newSeq[uint8](d.len)
  for i, dv in d:
    costFg[i] = t.area * (1'f32 + t.dist * pow(rmax(dv - grow, 0'f32), t.pow))
    fixed[i] = (if dv <= grow: HardFg else: Free)
  let (floor, sigma, k) = (t.floor, rmax(t.sigma, 1e-4'f32), t.k)
  let weight = proc(d2: float32): float32 = floor + 1'f32 / (1'f32 + pow(sqrt(d2) / sigma, k))
  cutLevels(img, fixed, costFg, weight, 1, max(t.directPx, 1024), optimized)

proc selectWorking*(img: RgbImage; seeds: seq[bool]; radius: float32; t: Tuning): seq[bool] =
  ## The model at working resolution: `seeds` (the brush footprint, `radius` working pixels) are
  ## selected; returns the selection.
  selectWorkingUsing(img, seeds, radius, t, true)

proc quickSelectUsing*(sampler: Sampler; canvas: Rect; points: openArray[(float32, float32)]; size: float32; t: Tuning;
                       optimized: bool): Option[Region] =
  if points.len == 0 or canvas.isEmpty: return none(Region)
  let r = rmax(size, 1'f32) / 2'f32
  var (x0, y0, x1, y1) = (F32Max, F32Max, -F32Max, -F32Max)
  for (x, y) in points:
    x0 = rmin(x0, x - r)
    y0 = rmin(y0, y - r)
    x1 = rmax(x1, x + r)
    y1 = rmax(y1, y + r)
  proc cl(v: float32): int = asI32(float64(clamp(v, -1073741824'f32, 1073741824'f32)))
  let bbox = rect(cl(floor(x0)), cl(floor(y0)), cl(ceil(x1)) + 1, cl(ceil(y1)) + 1).intersect(canvas)
  if bbox.isEmpty: return none(Region)
  let step = workingStep(canvas.width, canvas.height)
  let rw = r / float32(step)
  # Work in a window around the stroke; grow it while the selection runs into its border.
  var margin = asI32(float64(rmax(48'f32, 2'f32 * rw))) * step
  while true:
    proc snap0(v, o: int): int = o + floorDiv(v - o, step) * step
    let grown = bbox.inflate(margin).intersect(canvas)
    let window = rect(snap0(grown.x0, canvas.x0), snap0(grown.y0, canvas.y0), grown.x1, grown.y1)
    let img = sampler.rgbScaled(window, step)
    let (lw, lh) = (img.w, img.h)
    let s = float32(step)
    var pts: seq[(float32, float32)]
    for (x, y) in points: pts.add ((x - float32(window.x0)) / s, (y - float32(window.y0)) / s)
    let seeds = strokeMask(pts, rmax(rw, 0.5'f32), lw, lh)
    var anySeed = false
    for v in seeds:
      if v: anySeed = true
    if not anySeed: return none(Region)
    # A selection that reaches a window side inside the canvas is recomputed in a larger window.
    let inner = [window.x0 > canvas.x0, window.y0 > canvas.y0, window.x1 < canvas.x1, window.y1 < canvas.y1]
    let low = selectWorkingUsing(img, seeds, rw, t, optimized)
    proc touches(side: int): bool =
      case side
      of 0:
        for y in 0 ..< lh:
          if low[y * lw]: return true
      of 1:
        for x in 0 ..< lw:
          if low[x]: return true
      of 2:
        for y in 0 ..< lh:
          if low[y * lw + lw - 1]: return true
      else:
        for x in 0 ..< lw:
          if low[(lh - 1) * lw + x]: return true
      false
    var again = false
    for k in 0 .. 3:
      if inner[k] and touches(k): again = true
    if again:
      margin = (if margin > int(high(int32)) div 2: int(high(int32)) else: margin * 2)
      continue
    let mask = upsample(low, lw, lh, step, window.width, window.height)
    return trimRegion(Region(bbox: window, mask: mask))

proc quickSelectWith*(sampler: Sampler; canvas: Rect; points: openArray[(float32, float32)]; size: float32; t: Tuning): Option[Region] =
  ## `quickSelect` with explicit model constants.
  quickSelectUsing(sampler, canvas, points, size, t, true)

proc quickSelect*(sampler: Sampler; canvas: Rect; points: openArray[(float32, float32)]; size: float32): Option[Region] =
  ## Grows a selection from a brush stroke (`points` in document pixels, brush diameter `size`) on
  ## the document `canvas`. Returns the new region (to be added to or subtracted from the current
  ## selection), or none when the stroke misses the canvas.
  quickSelectWith(sampler, canvas, points, size, defaultTuning())
