## Edit › Content-Aware Fill: patch-based completion (`inpaint.completeMasked`) with Photoshop's
## sampling area, colour adaptation and geometric adaptation (port of `content_aware.rs`).
##
## Photoshop 25.4 was measured as a black box (synthetic and public-domain images; none of its
## code or assets were used):
##
## * **Sampling window** (`samplingWindow`): the fill copies only from a square centred on the
##   selection's bounding box, of side `4·√(max(w, 50)·max(h, 50))`, slid inside the canvas.
## * **Auto sampling area** (`autoSampling`): within that square, only what looks like the
##   selection's surroundings (clusters of local mean colour and contrast around the hole, one
##   contrast-sensitive min cut).
## * **Colour adaptation** None / Default / High / Very High (`colorLevel`): the copies may take a
##   gain and a bias towards their surroundings (S. Darabi et al., *Image Melding*, SIGGRAPH 2012).
## * **Rotation / scale / mirror adaptation**: the sampling window is extended with rotated,
##   rescaled and mirrored copies of itself, laid out side by side (separated by gaps that are no
##   image at all), as in the generalised PatchMatch of Barnes et al. (ECCV 2010).
##
## Buffers are interleaved `w × h × ch` normalised floats (any model/depth); deterministic.

import std/[math, options, algorithm]
import ../geom, ../raster
import ./util, ./inpaint, ./poisson, ./segment, ./selection_distance

type
  FillOptions* = object
    ## Options for `fill`.
    colorAdaptation*: Adapt ## colour adaptation of the copies (`colorLevel`)
    rotations*: seq[float32] ## extra source rotations in degrees
    scale*: bool             ## add downscaled and upscaled source copies
    mirror*: bool            ## add a horizontally mirrored source copy
    seed*: uint64

  Variant = object
    w, h: int
    img: seq[float32]
    ok: seq[bool]

proc colorLevel*(level: string): Adapt =
  ## Colour adaptation for Photoshop's levels: none, default, high, veryHigh.
  case level
  of "none": AdaptNone
  of "high": Adapt(gain: (0.8'f32, 1.25'f32), bias: 0.08)
  of "veryHigh": Adapt(gain: (0.67'f32, 1.5'f32), bias: 0.14)
  else: Adapt(gain: (0.9'f32, 1.1'f32), bias: 0.04)

proc defaultFillOptions*(): FillOptions = FillOptions(colorAdaptation: colorLevel("default"), seed: 1)

proc rotationLevel*(level: string): seq[float32] =
  ## Rotation set for Photoshop's adaptation levels: none, low, medium, high, full.
  case level
  of "low": @[-10'f32, 10]
  of "medium": @[-20'f32, 20]
  of "high": @[-35'f32, 35]
  of "full": @[90'f32, 180, 270]
  else: @[]

proc samplingWindow*(hole, canvas: Rect): Rect =
  ## Photoshop's sampling window for a selection with bounding box `hole`: the square of side
  ## `4·√(max(w, 50)·max(h, 50))` centred on it, slid inside `canvas` (and clipped to it only where
  ## the canvas is smaller than the square).
  if hole.isEmpty or canvas.isEmpty: return EmptyRect
  let (w, h) = (float64(max(hole.width, 50)), float64(max(hole.height, 50)))
  let side = int64(min(round(4.0 * sqrt(w * h)), float64(high(int32))))
  proc span(c2: int64; extent: int; lo, hi: int): (int, int) =
    let sd = max(side, int64(extent))
    if sd >= int64(hi) - int64(lo): return (lo, hi)
    let a = clamp(floorDiv(c2 - sd, 2'i64), int64(lo), int64(hi) - sd)
    (int(a), int(a + sd))
  let (x0, x1) = span(int64(hole.x0) + int64(hole.x1), hole.width, canvas.x0, canvas.x1)
  let (y0, y1) = span(int64(hole.y0) + int64(hole.y1), hole.height, canvas.y0, canvas.y1)
  rect(x0, y0, x1, y1)

proc sampleBilinear(w, h, ch: int; img: openArray[float32]; ok: openArray[bool]; x, y: float32;
                    outp: var seq[float32]): bool =
  ## Sample the source bilinearly at (x, y); false when any tap is unusable.
  if x < 0'f32 or y < 0'f32 or x > float32(w - 1) or y > float32(h - 1): return false
  let (x0, y0) = (asIdx(floor(x)), asIdx(floor(y)))
  let (x1, y1) = (min(x0 + 1, w - 1), min(y0 + 1, h - 1))
  let (tx, ty) = (x - float32(x0), y - float32(y0))
  for (xx, yy) in [(x0, y0), (x1, y0), (x0, y1), (x1, y1)]:
    if not ok[yy * w + xx]: return false
  for c in 0 ..< ch:
    template p(xx, yy: int): float32 = img[(yy * w + xx) * ch + c]
    let top = p(x0, y0) + (p(x1, y0) - p(x0, y0)) * tx
    let bot = p(x0, y1) + (p(x1, y1) - p(x0, y1)) * tx
    outp[c] = top + (bot - top) * ty
  true

proc variant(w, h, ch: int; img: openArray[float32]; ok: openArray[bool]; deg, scale: float32; mirror: bool;
             ctl: Interrupt): Variant =
  ## Transformed copy of the source: rotation (degrees) and scale about the centre, optional
  ## mirror. Raises `Cancelled`.
  let a = -(deg * 0.017453292519943295'f32)
  let (s, c) = (sin(a), cos(a))
  # Output size: the rotated, scaled bounding box.
  let (fw, fh) = (float32(w) * scale, float32(h) * scale)
  let ow = max(asIdx(round(fw * abs(c) + fh * abs(s))), 1)
  let oh = max(asIdx(round(fw * abs(s) + fh * abs(c))), 1)
  if ow.float * oh.float * ch.float > 4e9: raise newException(ValueError, "adapted source too large")
  let (icx, icy) = ((float32(w) - 1'f32) / 2'f32, (float32(h) - 1'f32) / 2'f32)
  let (ocx, ocy) = ((float32(ow) - 1'f32) / 2'f32, (float32(oh) - 1'f32) / 2'f32)
  result = Variant(w: ow, h: oh, img: newSeq[float32](ow * oh * ch), ok: newSeq[bool](ow * oh))
  var px = newSeq[float32](ch)
  for y in 0 ..< oh:
    if ctl.cancelled: break
    for x in 0 ..< ow:
      let (dx, dy) = ((float32(x) - ocx) / scale, (float32(y) - ocy) / scale)
      var sx = icx + dx * c - dy * s
      let sy = icy + dx * s + dy * c
      if mirror: sx = (float32(w) - 1'f32) - sx
      if sampleBilinear(w, h, ch, img, ok, sx, sy, px):
        result.ok[y * ow + x] = true
        for k in 0 ..< ch: result.img[(y * ow + x) * ch + k] = px[k]
  ctl.check

proc fillWith*(w, h, ch: int; img: seq[float32]; hole, source: seq[bool]; opts: FillOptions; ctl: Interrupt): seq[float32] =
  ## Fill `hole` in `img`, copying only from pixels where `source` is true (and not in the hole).
  ## Returns the full buffer (unchanged outside the hole); falls back to the membrane fill when
  ## nothing can be sampled. Raises `Cancelled`, or `ValueError` on buffers that don't match.
  if w < 0 or h < 0 or ch < 0 or img.len != w * h * ch or hole.len != w * h or source.len != w * h:
    raise newException(ValueError, "buffers do not match the image size")
  var anyHole = false
  for b in hole:
    if b:
      anyHole = true
      break
  if not anyHole: return img
  var params = defaultCompleteParams()
  params.seed = opts.seed
  params.adapt = opts.colorAdaptation
  let gap = 2 * params.patchRadius + 2
  var ok = newSeq[bool](w * h)
  for i in 0 ..< w * h: ok[i] = not hole[i] and source[i]
  # Variants: the identity (which holds the hole) first, then the adaptations.
  var extra: seq[Variant]
  if opts.mirror: extra.add variant(w, h, ch, img, ok, 0, 1, true, ctl)
  for deg in opts.rotations: extra.add variant(w, h, ch, img, ok, deg, 1, false, ctl)
  if opts.scale:
    extra.add variant(w, h, ch, img, ok, 0, 0.8, false, ctl)
    extra.add variant(w, h, ch, img, ok, 0, 1.25, false, ctl)
  proc role(s: int): Px = (if hole[s]: pxHole elif ok[s]: pxSource else: pxKnown)
  let cancel = proc(): bool = ctl.cancelled
  let progressFn = proc(f: float32) = ctl.progress(f * 0.99'f32)
  let inner = newInterrupt(cancel, progressFn)
  var outp: seq[float32]
  if extra.len == 0:
    var roles = newSeq[Px](w * h)
    for i in 0 ..< w * h: roles[i] = role(i)
    outp = img
    if not completeMaskedOwned(w, h, ch, outp, roles, params, inner):
      outp = membraneFillWith(w, h, ch, img, hole, ctl)
  else:
    # Mosaic: identity at the left, variants in a column to its right, gaps that are no image.
    var colW, colH = 0
    for v in extra:
      colW = max(colW, v.w)
      colH += v.h + gap
    let mw = w + gap + colW
    let mh = max(h, colH)
    var mimg = newSeq[float32](mw * mh * ch)
    var roles = newSeq[Px](mw * mh)
    for y in 0 ..< h:
      for x in 0 ..< w:
        let (s, d) = (y * w + x, y * mw + x)
        for c in 0 ..< ch: mimg[d * ch + c] = img[s * ch + c]
        roles[d] = role(s)
    var oy = 0
    for v in extra:
      ctl.check
      for y in 0 ..< v.h:
        for x in 0 ..< v.w:
          let (s, d) = (y * v.w + x, (oy + y) * mw + w + gap + x)
          for c in 0 ..< ch: mimg[d * ch + c] = v.img[s * ch + c]
          if v.ok[s]: roles[d] = pxSource
      oy += v.h + gap
    ctl.check
    let f = completeMasked(mw, mh, ch, mimg, roles, params, inner)
    if f.isSome:
      outp = img
      for y in 0 ..< h:
        for x in 0 ..< w:
          if hole[y * w + x]:
            let (s, d) = ((y * mw + x) * ch, (y * w + x) * ch)
            for c in 0 ..< ch: outp[d + c] = f.get[s + c]
    else:
      outp = membraneFillWith(w, h, ch, img, hole, ctl)
  ctl.check
  ctl.progress(1)
  outp

proc fill*(w, h, ch: int; img: seq[float32]; hole, source: seq[bool]; opts: FillOptions): seq[float32] =
  ## `fillWith`, never cancelled.
  fillWith(w, h, ch, img, hole, source, opts, NoInterrupt)

proc boxMeanSat(w, h: int; v: openArray[float32]; r: int): seq[float32] =
  ## Box mean of radius `r` over a `w × h` plane (window clipped), by summed areas.
  let w1 = w + 1
  var s = newSeq[float64](w1 * (h + 1))
  for y in 0 ..< h:
    var row = 0.0
    for x in 0 ..< w:
      row += float64(v[y * w + x])
      s[(y + 1) * w1 + x + 1] = s[y * w1 + x + 1] + row
  result = newSeq[float32](w * h)
  for y in 0 ..< h:
    let (y0, y1) = (max(y - r, 0), min(y + r + 1, h))
    for x in 0 ..< w:
      let (x0, x1) = (max(x - r, 0), min(x + r + 1, w))
      let sum = s[y1 * w1 + x1] + s[y0 * w1 + x0] - s[y0 * w1 + x1] - s[y1 * w1 + x0]
      result[y * w + x] = float32(sum / float64((x1 - x0) * (y1 - y0)))

const AutoSide = 384 ## largest side `autoSampling` works at

proc autoSamplingWith*(w, h, ch: int; img: seq[float32]; hole: seq[bool]; ctl: Interrupt): seq[bool] =
  ## Content-Aware Fill's Auto sampling area within a window (the whole `w × h` buffer): the
  ## pixels that look like the hole's surroundings. Falls back to everything outside the hole when
  ## the area would be too small to copy from. Raises `Cancelled`.
  if w < 0 or h < 0 or ch < 0 or img.len != w * h * ch or hole.len != w * h:
    raise newException(ValueError, "buffers do not match the image size")
  proc everything(): seq[bool] =
    result = newSeq[bool](hole.len)
    for i, b in hole: result[i] = not b
  var anyHole = false
  for b in hole:
    if b: anyHole = true
  if w == 0 or h == 0 or ch == 0 or not anyHole: return everything()
  # Work at a reduced scale for big windows (the area is smooth at this scale).
  let k = max((max(w, h) + AutoSide - 1) div AutoSide, 1)
  let (sw, sh) = ((w + k - 1) div k, (h + k - 1) div k)
  let nc = min(ch, 3)
  let stride = nc + 2
  var sums = newSeq[float32](sw * sh * stride)
  for sy in 0 ..< sh:
    if ctl.cancelled: break
    for y in sy * k ..< min((sy + 1) * k, h):
      for x in 0 ..< w:
        let o = (sy * sw + x div k) * stride
        let p = y * w + x
        if hole[p]: sums[o + nc + 1] = 1
        for c in 0 ..< nc: sums[o + c] += img[p * ch + c]
        sums[o + nc] += 1
  ctl.check
  var planes = newSeq[seq[float32]](nc)
  for c in 0 ..< nc:
    planes[c] = newSeq[float32](sw * sh)
    for i in 0 ..< sw * sh: planes[c][i] = sums[i * stride + c] / rmax(sums[i * stride + nc], 1'f32)
  var shole = newSeq[bool](sw * sh)
  for i in 0 ..< sw * sh: shole[i] = sums[i * stride + nc + 1] > 0'f32
  # Features: local means and local contrast (of the luma-ish average of the colour planes).
  const r = 3
  var means = newSeq[seq[float32]](nc)
  for c in 0 ..< nc: means[c] = boxMeanSat(sw, sh, planes[c], r)
  var luma, luma2 = newSeq[float32](sw * sh)
  for i in 0 ..< sw * sh:
    var s = -0.0'f32 # Rust's Iterator::sum starts at -0.0
    for c in 0 ..< nc: s += planes[c][i]
    luma[i] = s / float32(nc)
    luma2[i] = luma[i] * luma[i]
  let (lm, lm2) = (boxMeanSat(sw, sh, luma, r), boxMeanSat(sw, sh, luma2, r))
  var contrast = newSeq[float32](sw * sh)
  for i in 0 ..< sw * sh: contrast[i] = sqrt(rmax(lm2[i] - lm[i] * lm[i], 0'f32))
  let nf = nc + 1
  proc feat(i: int; outp: var seq[float32]) =
    for c in 0 ..< nc: outp[c] = means[c][i]
    outp[nc] = 2'f32 * contrast[i]
  # The ring: outside the hole, within a few pixels of it.
  let dist = edt(shole, sw, sh)
  let ringW = 6'f32 / float32(k) + 1'f32
  var ring: seq[int]
  for i in 0 ..< sw * sh:
    if not shole[i] and dist[i] <= ringW: ring.add i
  if ring.len == 0: return everything()
  ctl.check
  # A few cluster centres of the ring's features (k-means, deterministic start).
  let kc = min(6, ring.len)
  var f = newSeq[float32](nf)
  var centres = newSeq[seq[float32]](kc)
  for j in 0 ..< kc:
    feat(ring[j * ring.len div kc], f)
    centres[j] = f
  proc dd(a, b: seq[float32]): float32 =
    result = -0.0'f32
    for i in 0 ..< min(a.len, b.len): result += (a[i] - b[i]) * (a[i] - b[i])
  for _ in 0 ..< 8:
    var acc = newSeq[seq[float32]](kc)
    for a in acc.mitems: a = newSeq[float32](nf)
    var n = newSeq[int](kc)
    for i in ring:
      feat(i, f)
      var j = 0
      for q in 1 ..< kc:
        if totalCmp(dd(f, centres[q]), dd(f, centres[j])) < 0: j = q
      for c in 0 ..< nf: acc[j][c] += f[c]
      n[j] += 1
    for j in 0 ..< kc:
      if n[j] > 0:
        for c in 0 ..< nf: centres[j][c] = acc[j][c] / float32(n[j])
  proc nearest(i: int; f: var seq[float32]): float32 =
    feat(i, f)
    var m = Inf.float32
    for c in centres: m = rmin(m, dd(f, c))
    sqrt(m)
  # How much the ring itself varies around its clusters sets the scale of "similar".
  var spread = newSeq[float32](ring.len)
  for q, i in ring: spread[q] = nearest(i, f)
  let qi = min(spread.len * 9 div 10, spread.len - 1)
  spread.sort(proc(a, b: float32): int = totalCmp(a, b))
  let tau = rmax(spread[qi] * 1.6'f32, 0.03'f32)
  var costFg, costBg = newSeq[float32](sw * sh)
  var fixed = newSeq[uint8](sw * sh)
  for i in 0 ..< sw * sh:
    if shole[i] or dist[i] <= 1.5'f32:
      fixed[i] = HardFg
      continue
    let d = nearest(i, f) / tau
    costFg[i] = d * d
    costBg[i] = 1
  var rgb = newRgbImage(sw, sh)
  for i in 0 ..< sw * sh:
    for c in 0 .. 2: rgb.px[i][c] = means[min(c, nc - 1)][i]
  # Contrast-sensitive smoothness (fitted to Photoshop's Auto overlays).
  ctl.check
  let area = gridCutCancellable(rgb, costFg, costBg, fixed,
    proc(d2: float32): float32 = 8'f32 * exp(-d2 / (2'f32 * 0.05'f32 * 0.05'f32)) + 0.05'f32,
    proc(): bool = ctl.cancelled)
  if area.isNone: raise newException(Cancelled, "cancelled")
  var outp = newSeq[bool](w * h)
  var nOut = 0
  var nHole = 0
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = y * w + x
      outp[i] = not hole[i] and area.get[(y div k) * sw + x div k]
      if outp[i]: inc nOut
      if hole[i]: inc nHole
  # Too little to copy from (fewer source pixels than twice the hole): sample everything.
  if nOut < 2 * max(nHole, 64): return everything()
  outp

proc autoSampling*(w, h, ch: int; img: seq[float32]; hole: seq[bool]): seq[bool] =
  ## `autoSamplingWith`, never cancelled.
  autoSamplingWith(w, h, ch, img, hole, NoInterrupt)

proc colorAmount*(color: uint8): Adapt =
  ## The Patch Tool's and the Content-Aware Move Tool's Color option (0–10) as the colour
  ## adaptation of the copied content.
  if color == 0: return AdaptNone
  let c = float32(min(color, 10'u8))
  Adapt(gain: (1'f32 - 0.033'f32 * c, 1'f32 + 0.05'f32 * c), bias: 0.04'f32 * c)

proc levelTransform*(ch: int; frm, to: openArray[float32]; a: Adapt): seq[(float32, float32)] =
  ## Gain and bias per channel taking the samples `frm` to the level and contrast of the samples
  ## `to` (both interleaved, `ch` per sample), within `a`.
  proc moments(v: openArray[float32]; c: int): (float64, float64, float64) =
    var (s, s2, n) = (0.0, 0.0, 0.0)
    let st = max(ch, 1)
    for i in 0 ..< v.len div st:
      let x = float64(v[i * st + c])
      s += x
      s2 += x * x
      n += 1
    (s, s2, n)
  for c in 0 ..< ch:
    let (fs, fs2, fnn) = moments(frm, c)
    let (ts, ts2, tn) = moments(to, c)
    if a.isNone or fnn == 0.0 or tn == 0.0:
      result.add (1'f32, 0'f32)
      continue
    # Both sets scaled to the same count for the fit.
    let k = fnn / tn
    result.add fit(a, float32(fnn), float32(ts * k), float32(ts2 * k), float32(fs), float32(fs2))
