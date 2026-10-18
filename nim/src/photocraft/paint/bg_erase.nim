## Background Eraser (port of `paint/src/bg_erase.rs`): along a stroke, sample the colour under
## the brush hotspot and erase pixels of similar colour inside the brush to transparency.
##
## Colours are compared in the surface's own colour channels (RGB, CMYK, Gray, Lab… at any depth),
## so nothing is converted through RGB; only alpha changes.

import std/[options, math]
import ../geom, ../raster, ../rmath, ../serde
import ./brush, ./retouch, ./replace

type
  BgEraseSettings* = object
    sampling*: Sampling
    limits*: Limits
    tolerance*: float32           ## 0..1: largest per-channel difference that is erased
    protectForeground*: bool      ## keep pixels that match the foreground colour
    foreground*: array[4, float32] ## straight RGBA foreground colour (Protect Foreground Color)
    background*: array[4, float32] ## straight RGBA background swatch (`smBackgroundSwatch`)

proc defaultBgEraseSettings*(): BgEraseSettings =
  BgEraseSettings(sampling: smContinuous, limits: lmContiguous, tolerance: 0.5,
                  foreground: [0'f32, 0, 0, 1], background: [1'f32, 1, 1, 1])

proc defaultOf*(_: typedesc[BgEraseSettings]): BgEraseSettings = defaultBgEraseSettings()
proc serdeCamel*(_: typedesc[BgEraseSettings]): bool = true

proc dist(a, b: openArray[float32]): float32 =
  ## Largest per-channel difference over the colour channels.
  for i in 0 ..< min(a.len, b.len): result = rmax(result, abs(a[i] - b[i]))

proc applyBackgroundEraser*(target: var Surface; stroke: Stroke; bs: BgEraseSettings;
                            selection: Option[Surface]): Rect =
  ## Erase along `stroke`. The brush opacity is ignored (the Background Eraser has none); the
  ## selection limits the effect. Returns the damaged rectangle; empty when the surface has no
  ## alpha channel.
  let fmt = target.format
  let a = alphaIndex(fmt)
  if a < 0: return EmptyRect
  let nc = a
  let tol = if classify(bs.tolerance) notin {fcNan, fcInf, fcNegInf}: clamp(bs.tolerance, 0'f32, 1'f32) else: 0'f32
  let soft = bs.limits != lmFindEdges
  proc native(c: array[4, float32]): seq[float32] = fromRgba(fmt, [c[0], c[1], c[2], 1'f32])[0 ..< nc]
  let swatch = native(bs.background)
  let fg = native(bs.foreground)
  var onceSet = false
  var onceVal: Option[seq[float32]]
  proc matchW(d: float32): float32 =
    if d > tol: 0'f32
    elif soft and tol > 0: clamp((tol - d) / (tol * 0.25'f32), 0'f32, 1'f32) # soft over the last quarter
    else: 1'f32
  var st = stroke
  st.brush.opacity = 1
  st.brush.erase = false
  applyDabStroke(target, st, selection, false, 0, proc(work: var PxRegion; fp: Footprint) =
    let r = fp.rect.intersect(work.rect)
    if r.isEmpty: return
    let (cx, cy) = (asI32(floor(fp.dab.center.x)), asI32(floor(fp.dab.center.y)))
    # The hotspot's colour (none where it is already transparent or off the working area).
    proc hotspot(work: PxRegion): Option[seq[float32]] =
      if not work.rect.contains(cx, cy): return none(seq[float32])
      let o = work.offset(cx, cy)
      if work.data[o + a] > 0: some(work.data[o ..< o + nc]) else: none(seq[float32])
    let sampled =
      case bs.sampling
      of smContinuous: hotspot(work)
      of smOnce:
        if not onceSet:
          onceVal = hotspot(work)
          onceSet = true
        onceVal
      of smBackgroundSwatch: some(swatch)
    if sampled.isNone: return
    let smp = sampled.get
    let (w, h) = (r.width, r.height)
    # Per-pixel match weight. Erasing only lowers alpha, so the working copy still holds the
    # pre-stroke colours.
    var weight = newSeq[float32](w * h)
    for y in r.y0 ..< r.y1:
      for x in r.x0 ..< r.x1:
        if fp.at(x, y) <= 0: continue
        let o = work.offset(x, y)
        if work.data[o + a] <= 0: continue
        template c: untyped = work.data.toOpenArray(o, o + nc - 1)
        if bs.protectForeground and dist(c, fg) <= tol: continue
        weight[(y - r.y0) * w + (x - r.x0)] = matchW(dist(c, smp))
    if bs.limits != lmDiscontiguous:
      keepConnected(weight, w, h, clamp(cx, r.x0, r.x1 - 1) - r.x0, clamp(cy, r.y0, r.y1 - 1) - r.y0)
    for y in r.y0 ..< r.y1:
      for x in r.x0 ..< r.x1:
        let k = fp.at(x, y) * weight[(y - r.y0) * w + (x - r.x0)]
        if k <= 0: continue
        let o = work.offset(x, y)
        work.data[o + a] *= 1'f32 - rmin(k, 1'f32)
        if work.data[o + a] <= 0:
          for c in 0 ..< work.ch: work.data[o + c] = 0)
