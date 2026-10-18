## Generic dab-driven stroke machinery for the retouching tools (port of `paint/src/retouch.rs`):
## Clone Stamp, Healing Brush, Spot Healing, Dodge/Burn/Sponge, Blur/Sharpen, Smudge, History
## Brush.
##
## Two stroke shapes cover every retouching tool:
##
## * **Accumulate, then composite** (`strokeCoverage` + `applyCoverage`): the stroke's dabs build up
##   one coverage map (flow builds up, capped at 1), and a *paint image* computed once from the
##   pre-stroke state is composited through it at the stroke's opacity.
## * **Sequential dabs** (`applyDabStroke`): each dab edits a working copy in place (so dab *n*
##   sees the result of dab *n − 1*, as Dodge, Blur or Smudge need), and the finished working copy
##   is mixed back at the stroke's opacity.

import std/[math, options]
import ../geom, ../color, ../raster, ../rmath
import ./brush, ./dynamics, ./render, ./pixels
export pixels

type
  DabEffect* = proc(work: var PxRegion; fp: Footprint) {.closure.}

proc dabRect*(d: Dab): Rect =
  ## Bounding rectangle of a dab (1 px of anti-aliasing slack included).
  rectAround(d, d.radius)

proc footprint*(d: Dab; index: int; hardness: float32): Footprint =
  ## Rasterize one dab's coverage (round tip, hardness falloff).
  let rect = dabRect(d)
  let (cx, cy) = (float32(d.center.x), float32(d.center.y))
  var cov = newSeqOfCap[float32](rect.width * rect.height)
  for y in rect.y0 ..< rect.y1:
    for x in rect.x0 ..< rect.x1:
      let ddx = float32(x) + 0.5'f32 - cx
      let ddy = float32(y) + 0.5'f32 - cy
      let dist = sqrt(ddx * ddx + ddy * ddy)
      cov.add dabCoverage(dist, d.radius, hardness) * d.alpha
  Footprint(dab: d, index: index, rect: rect, cov: cov)

proc strokeBounds*(ds: openArray[Dab]): Rect =
  ## Union of the dabs' rectangles.
  result = EmptyRect
  for d in ds: result = result.union(dabRect(d))

proc strokeCoverage*(stroke: Stroke): (Rect, seq[float32]) =
  ## Accumulated coverage of a whole stroke: dabs build up (flow), capped at each dab's opacity
  ## ceiling; stroke-level masks (dual brush, texture) applied. Returns the stroke bounds and
  ## `width × height` coverage values. Uses the full brush engine (tips and dynamics).
  var r = newStrokeRenderer(stroke.brush, none(PixelFormat), 1)
  r.push(stroke.points)
  r.finish()
  r.denseCoverage

proc applyCoverage*(target: var Surface; rect: Rect; cov: openArray[float32]; opacity: float32;
                    selection: Option[Surface]; lockTransparency: bool; paint: PxRegion; blend: BlendMode): Rect =
  ## Composite a paint image through a coverage map onto `target`. `paint` must cover `rect`;
  ## `cov` has one value per pixel of `rect`. The per-pixel weight is `cov × opacity × selection`.
  ## Non-normal blend modes composite in straight RGBA.
  if rect.isEmpty: return EmptyRect
  let fmt = target.format
  let n = fmt.channels
  var region = target.readRegion(rect)
  let w = rect.width
  let a = alphaIndex(fmt)
  var enc: array[8, float32]
  for i, c in cov:
    if (i + 1) * n > region.len: break
    let x = rect.x0 + i mod w
    let y = rect.y0 + i div w
    let sel = if selection.isSome: selection.get.sampleChannel(x, y, 0) else: 1'f32
    let k = rmin(c * opacity * sel, 1'f32)
    if k <= 0: continue
    if lockTransparency and a >= 0 and region[i * n + a] <= 0: continue
    let so = paint.offset(x, y)
    if blend == bmNormal:
      overNative(fmt, region.toOpenArray(i * n, (i + 1) * n - 1), paint.data.toOpenArray(so, so + paint.ch - 1), k, lockTransparency)
    else:
      let d = toRgba(fmt, region.toOpenArray(i * n, (i + 1) * n - 1))
      let s = toRgba(fmt, paint.data.toOpenArray(so, so + paint.ch - 1))
      var o = color.composite(blend, d, s, k)
      if lockTransparency: o[3] = d[3]
      discard fromRgbaInto(fmt, o, enc)
      for ch in 0 ..< n: region[i * n + ch] = enc[ch]
  target.writeRegion(rect, region)
  rect

proc mixBack*(target: var Surface; orig, work: PxRegion; rect: Rect; opacity: float32;
              selection: Option[Surface]; lockTransparency: bool) =
  ## Mix a sequential-dab stroke's working pixels `work` back over the original `orig` at
  ## `opacity × selection` inside `rect`, and write that area to `target`.
  let fmt = target.format
  let a = alphaIndex(fmt)
  var o = orig.crop(rect)
  let n = o.ch
  let op = clamp(opacity, 0'f32, 1'f32)
  for y in rect.y0 ..< rect.y1:
    for x in rect.x0 ..< rect.x1:
      let k = op * (if selection.isSome: selection.get.sampleChannel(x, y, 0) else: 1'f32)
      if k <= 0: continue
      let wo = work.offset(x, y)
      let i = o.index(x, y) * n
      if a >= 0 and not lockTransparency:
        # Alpha changes: mix premultiplied, so colour under transparent pixels doesn't bleed in.
        mixPremultiplied(o.data.toOpenArray(i, i + n - 1), work.data.toOpenArray(wo, wo + n - 1), a, k)
      else:
        for c in 0 ..< n:
          if lockTransparency and c == a: continue
          o.data[i + c] += (work.data[wo + c] - o.data[i + c]) * k
  o.write(target)

proc applyDabStroke*(target: var Surface; stroke: Stroke; selection: Option[Surface];
                     lockTransparency: bool; halo: int; effect: DabEffect): Rect =
  ## Run a sequential-dab stroke: `effect(work, footprint)` edits the working copy for each dab in
  ## order; the result is mixed back at `stroke.brush.opacity × selection`. The working copy covers
  ## the stroke bounds grown by `halo` pixels so neighbourhood effects (blur, sharpen) can read
  ## around each dab; only the stroke bounds are written back. Returns the damaged rectangle.
  let ds = dabs(stroke)
  if ds.len == 0: return EmptyRect
  let ctx = newBrushContext(stroke.brush)
  var bounds = EmptyRect
  for d in ds: bounds = bounds.union(ctx.dabRect(d, false))
  let outer = rect(satSub(bounds.x0, halo), satSub(bounds.y0, halo), satAdd(bounds.x1, halo), satAdd(bounds.y1, halo))
  let orig = readPxRegion(target, outer)
  var work = orig
  for i, d in ds: effect(work, ctx.footprint(d, i))
  mixBack(target, orig, work, bounds, stroke.brush.opacity, selection, lockTransparency)
  bounds

# ---------------------------------------------------------------- smudge

type
  Smudge* = object
    ## Smudge state: a colour buffer carried from dab to dab (centred on the dab), which is laid
    ## down at `strength` and then re-picks-up the pixels under the dab. Use with `applyDabStroke`.
    strength*: float32           ## 0..1: how much carried colour each dab lays down (and keeps)
    finger*: Option[seq[float32]]
      ## Finger painting: start the stroke with this native-channel colour instead of the pixels
      ## under the first dab.
    radius: int
    alpha: int                   ## alpha index (-1 none): carry premultiplied when set
    carry: seq[float32]
    hasCarry: bool

proc newSmudge*(strength: float32; finger: Option[seq[float32]]; maxBrushSize: float32): Smudge =
  ## The carried buffer spans `maxBrushSize` (capped at `MaxBrushSize`) plus a 2 px margin.
  Smudge(strength: clamp(strength, 0'f32, 1'f32), finger: finger,
         radius: min(asI32(float64(ceil(maxBrushSize / 2'f32))), int(MaxBrushSize / 2)) + 2, alpha: -1)

proc withAlpha*(s: Smudge; a: int): Smudge =
  ## Mix in premultiplied space using alpha channel `a` (the format's `alphaIndex`; -1 for none).
  result = s
  result.alpha = a

proc dab*(sm: var Smudge; work: var PxRegion; fp: Footprint) =
  ## Apply one dab to the working copy.
  let (r, n) = (sm.radius, work.ch)
  let side = 2 * r + 1
  let (cx, cy) = (asI32(floor(fp.dab.center.x)), asI32(floor(fp.dab.center.y)))
  let wr = work.rect
  let alpha = if sm.alpha >= 0 and sm.alpha < n: sm.alpha else: -1
  if not sm.hasCarry:
    sm.carry = newSeq[float32](side * side * n)
    sm.hasCarry = true
    for qy in 0 ..< side:
      for qx in 0 ..< side:
        var v: seq[float32]
        if sm.finger.isSome:
          v = sm.finger.get
          if v.len < n:
            for _ in v.len ..< n: v.add 1'f32
          else: v.setLen(n)
          premultiply(v, alpha)
        else:
          v = work.pxSeq(clamp(cx + qx - r, wr.x0, wr.x1 - 1), clamp(cy + qy - r, wr.y0, wr.y1 - 1))
          premultiply(v, alpha)
        let q = (qy * side + qx) * n
        for c in 0 ..< n: sm.carry[q + c] = v[c]
  # Lay down the carried colour.
  for y in max(fp.rect.y0, wr.y0) ..< min(fp.rect.y1, wr.y1):
    for x in max(fp.rect.x0, wr.x0) ..< min(fp.rect.x1, wr.x1):
      let k = fp.at(x, y) * sm.strength
      let (qx, qy) = (x - cx + r, y - cy + r)
      if k <= 0 or qx < 0 or qy < 0 or qx >= side or qy >= side: continue
      let q = (qy * side + qx) * n
      let o = work.offset(x, y)
      if alpha >= 0:
        # Back to straight for the mix (premultiplied lerp towards the carried colour).
        let ca = sm.carry[q + alpha]
        var buf: array[16, float32]
        let m = min(n, buf.len)
        for c in 0 ..< m: buf[c] = sm.carry[q + c]
        if ca > 1e-6'f32:
          for c in 0 ..< m:
            if c != alpha: buf[c] /= ca
        mixPremultiplied(work.data.toOpenArray(o, o + n - 1), buf.toOpenArray(0, m - 1), alpha, k)
      else:
        for c in 0 ..< n: work.data[o + c] += (sm.carry[q + c] - work.data[o + c]) * k
  # Pick up: the carried colour keeps `strength` of itself.
  let keep = sm.strength
  for qy in 0 ..< side:
    for qx in 0 ..< side:
      let (x, y) = (cx + qx - r, cy + qy - r)
      if not wr.contains(x, y): continue
      let q = (qy * side + qx) * n
      let o = work.offset(x, y)
      let pa = if alpha >= 0: work.data[o + alpha] else: 1'f32
      for c in 0 ..< n:
        let v = if alpha >= 0 and alpha != c: work.data[o + c] * pa else: work.data[o + c]
        sm.carry[q + c] = sm.carry[q + c] * keep + v * (1'f32 - keep)
