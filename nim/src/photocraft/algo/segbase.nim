## Shared pieces of smart selection (port of the non-graph parts of `segment/mod.rs`): RGB working
## images, samplers, connected components and mask cleanup, region trimming and the xorshift RNG.
##
## Colour analysis runs on straight RGB samples in `0..=1` read through a `Sampler`, so any bit
## depth or colour model can be segmented; results are 8-bit coverage `Region`s.

import std/[math, options]
import ../geom, ../raster
import ./util, ./selection
export selection.Region

type
  RgbImage* = object
    ## Row-major RGB image, samples in `0..=1`.
    w*, h*: int
    px*: seq[array[3, float32]]

  Sampler* = object
    ## Where the segmentation algorithms read colours from (a layer, the composite, a test
    ## image): straight RGBA (`0..=1`) over a rect of document pixels, row-major.
    rgba*: proc(r: Rect): seq[Rgba]

  SegRng* = object
    ## Deterministic pseudo-random numbers (xorshift) for sampling and tests.
    state: uint64

const
  Free* = 0'u8   ## pixel label constraint for `gridCut`: free
  HardFg* = 1'u8 ## forced foreground
  HardBg* = 2'u8 ## forced background

proc newRgbImage*(w, h: int): RgbImage = RgbImage(w: w, h: h, px: newSeq[array[3, float32]](max(w * h, 0)))

proc rgbFromFn*(w, h: int; f: proc(x, y: int): array[3, float32]): RgbImage =
  result = RgbImage(w: w, h: h)
  for y in 0 ..< h:
    for x in 0 ..< w: result.px.add f(x, y)

proc at*(img: RgbImage; x, y: int): array[3, float32] {.inline.} = img.px[y * img.w + x]

proc rgbFromRgba*(px: openArray[Rgba]; w, h: int): RgbImage =
  ## Straight RGBA to RGB, compositing transparent pixels over mid gray (so transparency is a
  ## colour the models can separate).
  result = RgbImage(w: w, h: h, px: newSeq[array[3, float32]](px.len))
  for i, p in px:
    let a = clamp(p[3], 0'f32, 1'f32)
    for c in 0 .. 2: result.px[i][c] = clamp(p[c], 0'f32, 1'f32) * a + 0.5'f32 * (1'f32 - a)

proc downsample*(img: RgbImage; step: int): RgbImage =
  ## Box-average downsampling by an integer `step` (output `ceil(w/step) × ceil(h/step)`).
  if step <= 1: return img
  let (ow, oh) = ((img.w + step - 1) div step, (img.h + step - 1) div step)
  result = newRgbImage(ow, oh)
  for oy in 0 ..< oh:
    for ox in 0 ..< ow:
      var acc: array[3, float32]
      var n = 0'f32
      for y in oy * step ..< min((oy + 1) * step, img.h):
        for x in ox * step ..< min((ox + 1) * step, img.w):
          let p = img.at(x, y)
          for c in 0 .. 2: acc[c] += p[c]
          n += 1
      for c in 0 .. 2: result.px[oy * ow + ox][c] = acc[c] / n

proc rgb*(s: Sampler; r: Rect): RgbImage =
  ## RGB over `r` at full resolution.
  rgbFromRgba(s.rgba(r), r.width, r.height)

proc rgbScaled*(s: Sampler; r: Rect; step: int): RgbImage =
  ## RGB over `r`, box-downsampled by `step`, read in horizontal strips so the full-resolution
  ## area is never held in memory at once.
  if step <= 1: return s.rgb(r)
  let (w, h) = (r.width, r.height)
  let (ow, oh) = ((w + step - 1) div step, (h + step - 1) div step)
  let rowsPer = max((64 + step - 1) div step, 1)
  result = RgbImage(w: ow, h: oh)
  var oy0 = 0
  while oy0 < oh:
    let oy1 = min(oy0 + rowsPer, oh)
    let y0 = r.y0 + oy0 * step
    let y1 = min(r.y0 + oy1 * step, r.y1)
    result.px.add s.rgb(rect(r.x0, y0, r.x1, y1)).downsample(step).px
    oy0 = oy1

proc surfaceSampler*(s: Surface): Sampler =
  ## Samples a raster surface (any format; converted to straight RGBA).
  let surf = s
  Sampler(rgba: proc(r: Rect): seq[Rgba] =
    result = newSeq[Rgba](r.width * r.height)
    if not r.isEmpty: surf.readRgbaInto(r, result))

proc imageSampler*(img: RgbImage; origin: (int, int) = (0, 0)): Sampler =
  ## Samples an in-memory RGB image placed at `origin` (tests, benchmarks).
  let im = img
  Sampler(rgba: proc(r: Rect): seq[Rgba] =
    for y in r.y0 ..< r.y1:
      for x in r.x0 ..< r.x1:
        let (ix, iy) = (x - origin[0], y - origin[1])
        if ix < 0 or iy < 0 or ix >= im.w or iy >= im.h: result.add [0'f32, 0, 0, 0]
        else:
          let p = im.at(ix, iy)
          result.add [p[0], p[1], p[2], 1'f32])

proc scaleFor*(area: Rect; maxPx: int): int =
  ## Integer working-resolution step so that `area` holds at most about `maxPx` pixels.
  let n = float64(area.width) * float64(area.height)
  max(asUsize(ceil(sqrt(n / float64(max(maxPx, 1))))), 1)

proc d2*(a, b: array[3, float32]): float32 {.inline.} =
  let (x, y, z) = (a[0] - b[0], a[1] - b[1], a[2] - b[2])
  x * x + y * y + z * z

const Sqrt2f = 1.4142135'f32
const Nb* = [(1, 0, 1'f32), (0, 1, 1'f32), (1, 1, Sqrt2f), (-1, 1, Sqrt2f)]
  ## the 4 forward neighbour offsets of an 8-connected grid and their lengths

proc contrastBeta*(img: RgbImage): float32 =
  ## GrabCut's contrast parameter: `β = 1 / (2 ⟨‖z_m − z_n‖²⟩)` over 8-neighbour pairs.
  let (w, h) = (img.w, img.h)
  var sum = 0.0
  var n = 0'u64
  for y in 0 ..< h:
    for x in 0 ..< w:
      let a = img.px[y * w + x]
      for (dx, dy, _) in Nb:
        let (nx, ny) = (x + dx, y + dy)
        if nx >= 0 and nx < w and ny < h:
          sum += float64(d2(a, img.px[ny * w + nx]))
          n += 1
  if n == 0 or sum <= 1e-12: 0'f32 else: float32(float64(n) / (2.0 * sum))

proc components*(mask: openArray[bool]; w, h: int): (seq[uint32], seq[int]) =
  ## Labels 8-connected components of `mask`; returns (labels with 0 = background, sizes indexed
  ## by label).
  var lab = newSeq[uint32](w * h)
  var sizes = @[0]
  var stack: seq[int]
  for s in 0 ..< w * h:
    if not mask[s] or lab[s] != 0: continue
    let l = uint32(sizes.len)
    sizes.add 0
    lab[s] = l
    stack.add s
    while stack.len > 0:
      let i = stack.pop
      sizes[l] += 1
      let (x, y) = (i mod w, i div w)
      for dy in -1 .. 1:
        for dx in -1 .. 1:
          let (nx, ny) = (x + dx, y + dy)
          if nx < 0 or ny < 0 or nx >= w or ny >= h: continue
          let j = ny * w + nx
          if mask[j] and lab[j] == 0:
            lab[j] = l
            stack.add j
  (lab, sizes)

proc keepSeeded*(mask, seeds: openArray[bool]; w, h: int): seq[bool] =
  ## Keeps the connected components of `mask` that contain a `seed` pixel.
  let (lab, sizes) = components(mask, w, h)
  var keep = newSeq[bool](sizes.len)
  for i, l in lab:
    if i < seeds.len and seeds[i] and l != 0: keep[l] = true
  result = newSeq[bool](lab.len)
  for i, l in lab: result[i] = keep[l] and l != 0

proc components4(mask: openArray[bool]; w, h: int): (seq[uint32], seq[int]) =
  var lab = newSeq[uint32](w * h)
  var sizes = @[0]
  var stack: seq[int]
  for s in 0 ..< w * h:
    if not mask[s] or lab[s] != 0: continue
    let l = uint32(sizes.len)
    sizes.add 0
    lab[s] = l
    stack.add s
    while stack.len > 0:
      let i = stack.pop
      sizes[l] += 1
      let (x, y) = (i mod w, i div w)
      for (ok, j) in [(x > 0, i - 1), (x + 1 < w, i + 1), (y > 0, i - w), (y + 1 < h, i + w)]:
        if ok and mask[j] and lab[j] == 0:
          lab[j] = l
          stack.add j
  (lab, sizes)

proc cleanMask*(mask: openArray[bool]; w, h: int; frac, holeFrac: float32): seq[bool] =
  ## Keeps components at least `frac` of the largest one and fills holes (4-connected background
  ## areas not touching the border) smaller than `holeFrac` of the kept area.
  let (lab, sizes) = components(mask, w, h)
  var largest = 0
  for k in 1 ..< sizes.len: largest = max(largest, sizes[k])
  result = newSeq[bool](w * h)
  if largest == 0: return
  let mn = asUsize(float64(ceil(float32(largest) * frac)))
  var area = 0
  for i, l in lab:
    result[i] = l != 0 and sizes[l] >= mn
    if result[i]: inc area
  let maxHole = asUsize(float64(float32(area) * holeFrac))
  var inv = newSeq[bool](w * h)
  for i in 0 ..< w * h: inv[i] = not result[i]
  let (hl, hs) = components4(inv, w, h)
  var touches = newSeq[bool](hs.len)
  for x in 0 ..< w:
    touches[hl[x]] = true
    touches[hl[(h - 1) * w + x]] = true
  for y in 0 ..< h:
    touches[hl[y * w]] = true
    touches[hl[y * w + w - 1]] = true
  for i, l in hl:
    if l != 0 and not touches[l] and hs[l] <= maxHole: result[i] = true

proc antialiasU8*(mask: var seq[uint8]; w, h: int) =
  ## Softens a hard 0/255 mask's edges: edge pixels blend with their 3×3 neighbourhood (the same
  ## rule as the magic wand's anti-aliasing).
  let src = mask
  for y in 0 ..< h:
    for x in 0 ..< w:
      let v = src[y * w + x]
      var sum = 0'u32
      var edge = false
      for dy in -1 .. 1:
        for dx in -1 .. 1:
          let (nx, ny) = (x + dx, y + dy)
          let n = (if nx < 0 or ny < 0 or nx >= w or ny >= h: v else: src[ny * w + nx])
          edge = edge or n != v
          sum += uint32(n)
      if edge:
        let avg = float32(sum) / (9'f32 * 255'f32)
        let f = (if v > 127: 0.5'f32 + 0.5'f32 * avg else: 0.5'f32 * avg)
        mask[y * w + x] = asU8(round(f * 255'f32))

proc trimRegion*(r: Region): Option[Region] =
  ## Shrinks a region's box to its non-zero pixels (none if empty).
  let w = r.bbox.width
  if w == 0: return none(Region)
  let h = r.mask.len div w
  var (x0, y0, x1, y1) = (high(int), high(int), 0, 0)
  for y in 0 ..< h:
    var (a, b) = (-1, -1)
    for x in 0 ..< w:
      if r.mask[y * w + x] != 0:
        if a < 0: a = x
        b = x
    if a >= 0:
      x0 = min(x0, a)
      x1 = max(x1, b + 1)
      y0 = min(y0, y)
      y1 = y + 1
  if x0 == high(int): return none(Region)
  if x0 == 0 and y0 == 0 and x1 == w and y1 == h: return some(r)
  var mask = newSeqOfCap[uint8]((x1 - x0) * (y1 - y0))
  for y in y0 ..< y1: mask.add r.mask[y * w + x0 ..< y * w + x1]
  let b = r.bbox
  some(Region(bbox: rect(b.x0 + x0, b.y0 + y0, b.x0 + x1, b.y0 + y1), mask: mask))

proc newSegRng*(seed: uint64): SegRng = SegRng(state: (seed * 0x9E37_79B9_7F4A_7C15'u64) or 1)

proc nextU64*(r: var SegRng): uint64 =
  var x = r.state
  x = x xor (x shl 13)
  x = x xor (x shr 7)
  x = x xor (x shl 17)
  r.state = x
  x

proc f32*(r: var SegRng): float32 =
  ## Uniform in `[0, 1)`.
  float32(r.nextU64 shr 40) / float32(1'u64 shl 24)

proc normal*(r: var SegRng): float32 =
  ## Standard normal (Box–Muller).
  let u = rmax(r.f32, 1e-7'f32)
  let v = r.f32
  sqrt(-2'f32 * ln(u)) * cos(6.2831855'f32 * v)

proc subsample*[T](v: openArray[T]; max: int): seq[T] =
  ## Every `k`-th element so that at most `max` remain.
  if v.len <= max: return @v
  let k = (v.len + max(max, 1) - 1) div max(max, 1)
  var i = 0
  while i < v.len:
    result.add v[i]
    i += k
