## Retouching tools as commands (port of `retouch_cmds.rs` and its `patch`, `content_aware_move`,
## `remove` and `pattern_stamp` submodules): Clone Stamp, Pattern Stamp, Healing Brush, Spot
## Healing Brush, Patch, Content-Aware Move, Remove, Dodge, Burn, Sponge, Blur, Sharpen, Smudge and
## History Brush.
##
## Every command takes a Photoshop-style brush (`points`, `size`, `hardness`, `opacity`, `flow`,
## `spacing`, `layer`), respects the active selection as a mask and the layer's transparency lock, and
## works at any depth (8/16/32f) and colour model. Like the Brush, each paints the targeted surface:
## the layer's pixels, its mask (`"target":"mask"`), an alpha channel or the Quick Mask (`"target"`,
## filled in from the Channels panel when absent). The pixel algorithms live in `algo` (`poisson`,
## `inpaint`, `retouch`, `content_aware`, `remove`) and the dab machinery in `paint/retouch`; this
## module only parses parameters and wires them together. The live previews of a stroke while it is
## drawn (`LiveRetouch`, `LiveDab`) land with the UI.

import std/[json, options, math, tables]
import ./core
import ../compose, ../paint, ../rmath
import ../color as pcolor
import ../algo/[poisson, inpaint, holes]
import ../algo/remove as aremove
import ../algo/retouch as aretouch
import ../algo/content_aware as caware
import ../algo/transform as atransform

# ---------------------------------------------------------------- shared helpers

proc retouchPixelE(s: Session): string =
  ## A pixel layer, a layer with a mask, or a targeted alpha channel / Quick Mask (as the Brush).
  if s.editsChannel: return ""
  let d = s.active
  if d.isNil: return "no document open"
  if d.activeLayer.isNone: return "no active layer"
  let l = d.doc[].findLayer(d.activeLayer.get)
  if l.isNil: return "no active layer"
  if l[].content.kind == lkRaster or l[].mask.isSome: ""
  else: "active layer is " & l[].content.article & " " & l[].content.kindName & " layer, not a pixel layer"

proc targetsPixels(p: JsonNode): bool =
  ## The stroke paints layer pixels (not a mask or channel), so other layers can be sampled.
  targetOf(p).kind == tgPixels

proc num(p: JsonNode; k: string; d: float32): float32 =
  let v = p.pget(k).asF64
  if v.isSome: float32(v.get) else: d

proc flag(p: JsonNode; k: string; d: bool): bool = p.pget(k).asBool.get(d)

proc str(p: JsonNode; k, d: string): string = p.pget(k).asStr.get(d)

proc point(p: JsonNode; k: string): Option[(float64, float64)] =
  let a = p.pget(k)
  if not a.isArray or a.len < 2: return
  let (x, y) = (a.elems[0].asF64, a.elems[1].asF64)
  if x.isSome and y.isSome: some((x.get, y.get)) else: none((float64, float64))

proc alphaOpt(fmt: PixelFormat): Option[int] =
  let a = alphaIndex(fmt)
  if a >= 0: some(a) else: none(int)

proc strokeBrush*(base: BrushSettings; p: JsonNode): BrushSettings =
  ## The brush a retouching stroke paints with: the tool's brush (`base`, its tip, hardness and
  ## shape dynamics) with the shared params of `p` applied. UI previews of a stroke (the Spot
  ## Healing Brush's trail) render their coverage with it, so they match what the command changes.
  proc pct(k: string; d, lo, hi: float32): float32 = clamp(num(p, k, d), lo, hi) / 100'f32
  result = base
  result.size = rmax(num(p, "size", base.size), 1'f32)
  result.hardness = pct("hardness", base.hardness * 100'f32, 0, 100)
  result.opacity = pct("opacity", 100, 1, 100)
  result.flow = pct("flow", 100, 1, 100)
  # Brush Settings › Brush Tip Shape › Spacing, as the Brush uses it (#2140).
  result.spacing = pct("spacing", base.spacing * 100'f32, 1, 1000)
  result.erase = false

proc parseBrush(s: Session; p: JsonNode; cmdId: string): (Stroke, Option[LayerId]) =
  ## Parse the shared brush params into a stroke on a layer (none when an alpha channel or the
  ## Quick Mask is targeted).
  let arr = p.pget("points")
  if not arr.isArray: raise bad(cmdId, "missing `points`")
  var pts: seq[StrokePoint]
  for v in arr.elems:
    if not v.isArray or v.len < 2: continue
    let (x, y) = (v.elems[0].asF64, v.elems[1].asF64)
    if x.isNone or y.isNone: continue
    let pr = (if v.len > 2: v.elems[2].asF64.get(1.0) else: 1.0)
    pts.add strokePoint(x.get, y.get, float32(pr))
  if pts.len == 0: raise bad(cmdId, "`points` is empty")
  checkCoords(pts, cmdId)
  let brush = strokeBrush(s.tools.brush, p)
  validateBrush(brush, cmdId)
  let stroke = Stroke(brush: brush, points: pts)
  if isChannelTarget(p): return (stroke, none(LayerId))
  let l = p.pget("layer").asU64
  if l.isSome: return (stroke, some(LayerId(l.get)))
  let d = s.active
  if d.isNil or d.activeLayer.isNone: raise other("no active layer")
  (stroke, d.activeLayer)

type StrokeBody = proc(pre: Document; surf: var Surface; sel: Option[Surface]; lock: bool): Rect

proc runStroke(s: var Session; label: string; id: Option[LayerId]; p: JsonNode; f: StrokeBody): Rect =
  ## Run `f` on the targeted surface (layer pixels, layer mask, alpha channel or Quick Mask: the
  ## Brush's paint target) inside one undoable step. `f` gets the pre-stroke document, the surface,
  ## the selection and the transparency lock.
  let dmg = s.editWith(label, proc(doc: var Document; active: var Option[LayerId]): Rect =
    let pre = doc
    let (surf, lock) = targetSurface(doc, id, p)
    # No prune: retouching only rewrites pixels inside the stroke, and scanning every tile of a
    # large layer would dominate the cost of a dab.
    f(pre, surf[], pre.selection, lock))
  let st = s.active
  if not st.isNil: st.lastDamage = some(dmg)
  dmg

proc damageJson(r: Rect): JsonNode = %*[r.x0, r.y0, r.width, r.height]

proc setDamage(s: var Session; r: Rect) =
  let st = s.active
  if not st.isNil: st.lastDamage = some(r)

template staged(body: untyped): untyped =
  ## An algorithm stage: its cancellation (or a buffer mismatch) is the job's `Cancelled`.
  try: body
  except Cancelled, ValueError: raise cancelledErr()

# ---------------------------------------------------------------- sampling sources

type RetouchSample = enum
  ## Which pixels Clone Stamp / Healing Brush sample.
  rsCurrent, rsCurrentAndBelow, rsAll

proc sampleLayers(p: JsonNode; cmdId: string): RetouchSample =
  let v = str(p, "sampleLayer", "current")
  case v
  of "current": rsCurrent
  of "currentAndBelow": rsCurrentAndBelow
  of "all": rsAll
  else: raise bad(cmdId, "unknown sampleLayer `" & v & "` (current|currentAndBelow|all)")

proc compositeRegion(pre: Document; id: Option[LayerId]; which: RetouchSample; rc: Rect; fmt: PixelFormat): PxRegion =
  ## Composite pixels of `rc` in `fmt` (native channels): the whole document, or the target layer
  ## and everything below it.
  var doc = pre
  if which == rsCurrentAndBelow and id.isSome:
    # Hide everything composited after (above) the target in the bottom-to-top walk.
    var walk: seq[(LayerPath, LayerId)]
    for (path, _, l) in pre.walk: walk.add (path, l.id)
    var pos = -1
    for i, w in walk:
      if w[1] == id.get:
        pos = i
        break
    if pos >= 0:
      for i in pos + 1 ..< walk.len:
        let l = doc.layerAtPtr(walk[i][0])
        if not l.isNil: l.visible = false
  let buf = compose.render(doc, rc)
  let n = fmt.channels
  result = newPxRegion(rc, n)
  var enc: array[8, float32]
  for i, px in buf.px:
    if (i + 1) * n > result.data.len: break
    discard fromRgbaInto(fmt, px, enc)
    for c in 0 ..< n: result.data[i * n + c] = enc[c]

proc sampleAt(pre: Document; id: Option[LayerId]; surf: Surface; which: RetouchSample; rc: Rect): PxRegion =
  ## Pixels to sample at `rc` (document coordinates, already offset to the source position).
  if which == rsCurrent: readPxRegion(surf, rc)
  else: compositeRegion(pre, id, which, rc, surf.format)

proc cloneSample(pre: Document; id: Option[LayerId]; surf: Surface; which: RetouchSample; rc: Rect;
                 m: CloneMapping; cmdId: string): PxRegion =
  ## Source pixels for destination `rc`: a translated read, or a bilinear resample when the clone
  ## source is scaled, rotated or flipped.
  if m.isTranslation:
    let off = m.offset
    let src = rc.translate(off[0], off[1])
    # `translate` saturates at the i32 edges, so a far source (issue #1111) yields a window smaller
    # than `rc`; relabelling that as `rc` would index past the sampled pixels.
    if src.width != rc.width or src.height != rc.height:
      raise bad(cmdId, "source offset [" & $off[0] & ", " & $off[1] & "] is too far: the sample window leaves the coordinate range")
    result = sampleAt(pre, id, surf, which, src)
    result.rect = rc
    return
  let src = sampleAt(pre, id, surf, which, m.sourceRect(rc))
  resampleClone(src, rc, m, alphaOpt(surf.format))

proc clonePreview*(s: Session; rc: Rect; m: CloneMapping; sampleLayer: string): Buffer =
  ## Read-only clone overlay. Shares the stroke sampler without anchoring a source or editing
  ## history. Both destination and transformed source allocations are bounded for interactive use.
  const cmdId = "clone.preview"
  proc fin(v: float64): bool = v == v and abs(v) < 1_000_000.0
  for v in [m.source[0], m.source[1], m.anchor[0], m.anchor[1], m.m[0], m.m[1], m.m[2], m.m[3]]:
    if not fin(v): raise bad(cmdId, "invalid preview mapping")
  var ok = true
  for v in [rc.x0, rc.y0, rc.x1, rc.y1]:
    if abs(v) >= 1_000_000: ok = false
  if ok:
    for (x, y) in [(rc.x0, rc.y0), (rc.x1, rc.y1), (rc.x0, rc.y1), (rc.x1, rc.y0)]:
      let (mx, my) = m.map(float64(x), float64(y))
      if not (fin(mx) and fin(my)): ok = false
  if not ok: raise bad(cmdId, "preview coordinates exceed their budget")
  proc bounded(r: Rect): bool = r.width > 0 and r.height > 0 and r.width <= 1024 and r.height <= 1024
  if not bounded(rc) or not bounded(m.sourceRect(rc)): raise bad(cmdId, "preview region exceeds its budget")
  let d = s.active
  if d.isNil: raise noDocument()
  if d.activeLayer.isNone: raise bad(cmdId, "no active layer")
  let id = d.activeLayer.get
  let l = d.doc[].findLayer(id)
  let hasSurface = not l.isNil and (case l[].content.kind
    of lkRaster: true
    of lkText: l[].content.text.cache.isSome
    of lkShape: l[].content.shape.cache.isSome
    of lkSmart: l[].content.smart.cache.isSome
    else: false)
  if not hasSurface: raise bad(cmdId, "no pixel surface")
  let surf = l[].surface
  let which = sampleLayers(%*{"sampleLayer": sampleLayer}, cmdId)
  let region = cloneSample(d.doc[], some(id), surf, which, rc, m, cmdId)
  let fmt = surf.format
  let n = fmt.channels
  result = Buffer(rect: rc)
  for i in 0 ..< region.data.len div max(n, 1):
    result.px.add toRgba(fmt, region.data.toOpenArray(i * n, i * n + n - 1))

proc cloneResult(dmg: Rect; off: (int, int); aligned: bool; p: JsonNode; stroke: Stroke): JsonNode =
  ## Result JSON shared by Clone Stamp and Healing Brush: the offset used and where the next stroke
  ## should sample from if the UI keeps Photoshop's "Aligned" semantics.
  let f = stroke.points[0]
  let source = point(p, "source").get((f.x + float64(off[0]), f.y + float64(off[1])))
  # Aligned: the offset persists (next stroke samples at its own first point + offset).
  # Non-aligned: every stroke restarts at the original source point.
  %*{"damage": damageJson(dmg), "offset": [off[0], off[1]], "aligned": aligned,
     "nextSource": (if aligned: newJNull() else: %*[source[0], source[1]])}

proc blendParam(p: JsonNode; cmdId: string): BlendMode =
  let m = p.pget("mode").asStr
  if m.isNone: return bmNormal
  let b = blendFromStr(m.get)
  if b.isNone: raise bad(cmdId, "unknown blend mode `" & m.get & "`")
  b.get

proc clampSamples(fmt: PixelFormat; data: var seq[float32]) =
  ## Clamp results into the storable range: integer depths to 0..1, float depths only below at 0;
  ## alpha always 0..1.
  let n = fmt.channels
  if n <= 0: return
  let a = alphaIndex(fmt)
  let isFloat = fmt.sample == stF32
  for i in 0 ..< data.len:
    let c = i mod n
    data[i] = (if c == a or not isFloat: clamp(data[i], 0'f32, 1'f32) else: rmax(data[i], 0'f32))

# ---------------------------------------------------------------- Clone Stamp / Healing Brush / History Brush

proc cloneStamp(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "paint.cloneStamp"
  let (stroke, id) = parseBrush(s, p, cmdId)
  let which = sampleLayers(p, cmdId)
  let mode = blendParam(p, cmdId)
  let aligned = flag(p, "aligned", true)
  let f = stroke.points[0]
  let m = cloneMapping(s, p, (f.x, f.y), cmdId)
  let off = m.offset
  let dmg = runStroke(s, "Clone Stamp", id, p, proc(pre: Document; surf: var Surface; sel: Option[Surface]; lock: bool): Rect =
    let (bounds, cov) = strokeCoverage(stroke)
    # Samples come from the pre-stroke state, so pixels painted earlier in the stroke are never re-cloned.
    let paint = cloneSample(pre, id, surf, which, bounds, m, cmdId)
    applyCoverage(surf, bounds, cov, stroke.brush.opacity, sel, lock, paint, mode))
  cloneResult(dmg, off, aligned, p, stroke)

proc padCoverage(bounds: Rect; cov: seq[float32]; m: int): (Rect, seq[float32]) =
  ## Grow a stroke's coverage map by `m` pixels of zero coverage (a Dirichlet boundary for the solve).
  let g = bounds.inflate(m)
  let (w, gw) = (bounds.width, g.width)
  var outp = newSeq[float32](gw * g.height)
  for i, c in cov:
    let (x, y) = (i mod w + m, i div w + m)
    outp[y * gw + x] = c
  (g, outp)

proc healRegion(fmt: PixelFormat; src, dst: PxRegion; mask: seq[bool]): PxRegion =
  ## Seamless-clone `src` into `dst` over the mask (both regions cover the same rect). The solve runs
  ## only on the mask's bounding box plus a one-pixel Dirichlet ring; the rest is `dst`.
  let w = dst.width
  var bbox = EmptyRect
  for i, m in mask:
    if m:
      let (x, y) = (dst.rect.x0 + i mod w, dst.rect.y0 + i div w)
      bbox = bbox.union(rect(x, y, x + 1, y + 1))
  result = dst
  if bbox.isEmpty: return
  let r = bbox.inflate(1).intersect(dst.rect)
  let (sc, dc) = (src.crop(r), dst.crop(r))
  var m = newSeqOfCap[bool](r.width * r.height)
  for y in r.y0 ..< r.y1:
    for x in r.x0 ..< r.x1: m.add mask[dst.index(x, y)]
  var data = seamlessClone(dc.width, dc.height, dc.ch, sc.data, dc.data, m)
  clampSamples(fmt, data)
  let (n, rw) = (dst.ch, r.width)
  for y in r.y0 ..< r.y1:
    let o = result.index(r.x0, y) * n
    let i = (y - r.y0) * rw * n
    for k in 0 ..< rw * n: result.data[o + k] = data[i + k]

proc healingBrush(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "paint.healingBrush"
  let (stroke, id) = parseBrush(s, p, cmdId)
  let which = sampleLayers(p, cmdId)
  let mode = blendParam(p, cmdId)
  let aligned = flag(p, "aligned", true)
  let f = stroke.points[0]
  let m = cloneMapping(s, p, (f.x, f.y), cmdId)
  let off = m.offset
  let dmg = runStroke(s, "Healing Brush", id, p, proc(pre: Document; surf: var Surface; sel: Option[Surface]; lock: bool): Rect =
    let (bounds, cov0) = strokeCoverage(stroke)
    let (g, cov) = padCoverage(bounds, cov0, 2)
    let fmt = surf.format
    let src = cloneSample(pre, id, surf, which, g, m, cmdId)
    # The destination the texture is fitted to: the layer itself, or what the user sees when
    # sampling several layers (so healing onto an empty layer matches the composite).
    let dst = (if which == rsCurrent: readPxRegion(surf, g) else: compositeRegion(pre, id, which, g, fmt))
    var mask = newSeq[bool](cov.len)
    for i, c in cov: mask[i] = c > 0
    let healed = healRegion(fmt, src, dst, mask)
    applyCoverage(surf, g, cov, stroke.brush.opacity, sel, lock, healed, mode))
  cloneResult(dmg, off, aligned, p, stroke)

proc historySource(s: Session; id: Option[LayerId]; p: JsonNode; cmdId: string): Surface =
  ## The History Brush's source: the targeted surface (pixels, mask, channel) as it was in history
  ## state `state` (default: the oldest state still held, the "Open" snapshot unless trimmed).
  let st = s.active
  if st.isNil: raise noDocument()
  let explicit = p.pget("state").asU64
  var source: DocRef
  if explicit.isSome:
    let i = explicit.get
    if i == uint64(st.history.pastLen): source = st.doc
    else:
      let h = (if i <= uint64(high(int)): st.history.state(int(i)) else: none(DocRef))
      if h.isNone: raise bad(cmdId, "no history state " & $i & " (0..=" & $st.history.pastLen & ")")
      source = h.get
  else:
    let h = st.history.state(0)
    source = (if h.isSome: h.get else: st.doc)
  try: readTargetSurface(source[], id, p)
  except EngineError:
    raise other("the target did not exist (as pixels, a mask or a channel) in that history state")

proc historyBrush(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "paint.historyBrush"
  let (stroke, id) = parseBrush(s, p, cmdId)
  let mode = blendParam(p, cmdId)
  let srcSurface = historySource(s, id, p, cmdId)
  let dmg = runStroke(s, "History Brush", id, p, proc(pre: Document; surf: var Surface; sel: Option[Surface]; lock: bool): Rect =
    let fmt = surf.format
    let src = (if srcSurface.format == fmt: srcSurface else: srcSurface.convert(fmt))
    let (bounds, cov) = strokeCoverage(stroke)
    let paint = readPxRegion(src, bounds)
    applyCoverage(surf, bounds, cov, stroke.brush.opacity, sel, lock, paint, mode))
  %*{"damage": damageJson(dmg)}

# ---------------------------------------------------------------- Spot Healing

proc dilateSquare(w, h: int; m: seq[bool]; r: int): seq[bool] =
  ## Dilate a mask by `r` pixels (square structuring element).
  var tmp = newSeq[bool](w * h)
  for y in 0 ..< h:
    for x in 0 ..< w:
      if m[y * w + x]:
        for xx in max(x - r, 0) ..< min(x + r + 1, w): tmp[y * w + xx] = true
  result = newSeq[bool](w * h)
  for y in 0 ..< h:
    for x in 0 ..< w:
      if tmp[y * w + x]:
        for yy in max(y - r, 0) ..< min(y + r + 1, h): result[yy * w + x] = true

proc proximitySourcePatch(img: seq[float32]; w, h, ch: int; mask: seq[bool]; dx, dy: int): Option[seq[float32]] =
  ## Copy a translated Proximity Match patch from the *pre-healing* pixels. Reading the output here
  ## would make overlapping offsets repeatedly copy the pixels just written, leaving streaks instead
  ## of the selected source texture.
  if w == 0 or h == 0 or ch == 0 or mask.len != w * h or img.len != mask.len * ch: return
  var outp = img
  for p, masked in mask:
    if not masked: continue
    let x = p mod w + dx
    let y = p div w + dy
    if x < 0 or y < 0 or x >= w or y >= h: return
    let src = (y * w + x) * ch
    let dst = p * ch
    for c in 0 ..< ch: outp[dst + c] = img[src + c]
  some(outp)

type SpotType = enum stContentAware, stCreateTexture, stProximityMatch

proc spotHealSurface(surf: var Surface; pre: Document; stroke: Stroke; kind: SpotType; all: bool;
                     sel: Option[Surface]; lock: bool): Rect =
  ## Fill the stroke area with no source, then gradient-domain blend it into its surroundings. With
  ## `all` (Sample All Layers) it heals what is visible, so it works on an empty layer above the
  ## image. Content-Aware takes its texture from around the stroke, as Edit › Fill does from its
  ## window (`samplingWindowFor` of the stroke's bounds).
  let (bounds, cov) = strokeCoverage(stroke)
  let size = stroke.brush.size
  let regionRect = samplingWindowFor(bounds, pre.bounds)
  if regionRect.isEmpty: return EmptyRect
  # Proximity Match looks for its source anywhere in that window.
  let margin = max(regionRect.width, regionRect.height) div 2
  let fmt = surf.format
  let img = (if all: compositeRegion(pre, none(LayerId), rsAll, regionRect, fmt) else: readPxRegion(surf, regionRect))
  let (w, h, ch) = (img.width, img.height, img.ch)
  let bw = bounds.width
  var strokeCov = newSeq[float32](w * h)
  for i, c in cov:
    let (x, y) = (bounds.x0 + i mod bw, bounds.y0 + i div bw)
    if regionRect.contains(x, y): strokeCov[img.index(x, y)] = c
  var hole = newSeq[bool](w * h)
  var any = false
  for i, c in strokeCov:
    hole[i] = c > 0
    if hole[i]: any = true
  if not any: return EmptyRect
  # Synthesise over a slightly larger domain than the Poisson region, so the solve's boundary pixels
  # carry synthesised values that differ from the untouched surroundings: that mismatch is what the
  # membrane corrects (otherwise the Dirichlet data would be all zero and the blend a no-op).
  let domain = dilateSquare(w, h, hole, 2)
  let healMask = dilateSquare(w, h, hole, 1)
  proc contentAware(data: seq[float32]): seq[float32] =
    let r = inpaint.complete(w, h, ch, data, domain, defaultCompleteParams())
    if r.isSome: r.get else: membraneFill(w, h, ch, data, domain)
  var filled: seq[float32]
  case kind
  of stContentAware: filled = contentAware(img.data)
  of stCreateTexture:
    filled = synthesize(w, h, ch, img.data, domain, int(clamp(size / 4'f32, 6'f32, 32'f32)), 0x5eed'u64)
  of stProximityMatch:
    let ring = int(clamp(size / 8'f32, 3'f32, 16'f32))
    let off = bestOffset(w, h, ch, img.data, domain, ring, margin)
    if off.isSome:
      let patch = proximitySourcePatch(img.data, w, h, ch, domain, int(off.get[0]), int(off.get[1]))
      filled = (if patch.isSome: patch.get else: contentAware(img.data))
    else: filled = contentAware(img.data)
  let src = PxRegion(rect: regionRect, ch: ch, data: filled)
  let healed = healRegion(fmt, src, img, healMask)
  let dmg = bounds.intersect(regionRect)
  var dmgCov = newSeqOfCap[float32](dmg.width * dmg.height)
  for y in dmg.y0 ..< dmg.y1:
    for x in dmg.x0 ..< dmg.x1: dmgCov.add strokeCov[img.index(x, y)]
  applyCoverage(surf, dmg, dmgCov, stroke.brush.opacity, sel, lock, healed, bmNormal)

proc sampleAllLayers(p: JsonNode): bool =
  ## "Sample All Layers" for Spot Healing, Blur, Sharpen and Smudge (layer pixels only: a mask or
  ## channel has nothing to sample from other layers).
  flag(p, "sampleAllLayers", false) and targetsPixels(p)

proc spotHealing(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "paint.spotHealing"
  let (stroke, id) = parseBrush(s, p, cmdId)
  let t = str(p, "type", "contentAware")
  let kind = case t
    of "contentAware": stContentAware
    of "createTexture": stCreateTexture
    of "proximityMatch": stProximityMatch
    else: raise bad(cmdId, "unknown type `" & t & "` (contentAware|createTexture|proximityMatch)")
  let all = sampleAllLayers(p)
  let dmg = runStroke(s, "Spot Healing Brush", id, p, proc(pre: Document; surf: var Surface; sel: Option[Surface]; lock: bool): Rect =
    spotHealSurface(surf, pre, stroke, kind, all, sel, lock))
  %*{"damage": damageJson(dmg)}

# ---------------------------------------------------------------- Dodge / Burn / Sponge, Blur / Sharpen, Smudge

proc perDab(e, spacing: float32): float32 {.inline.} =
  ## Per-dab strength such that one pass over a point totals roughly `e`, whatever the spacing
  ## (about `1/spacing` dabs overlap each point on the stroke's centre line).
  1'f32 - pow(1'f32 - clamp(e, 0'f32, 1'f32), clamp(spacing, 0.01'f32, 1'f32))

proc colorDab(fmt: PixelFormat; work: var PxRegion; fp: Footprint; strength, spacing: float32;
              f: proc(c: array[3, float32]; k: float32): array[3, float32]) =
  ## Apply an RGB colour transform `f(rgb, strength)` to each covered pixel of a dab (through
  ## straight RGBA, so it works for Gray/CMYK/Lab too; untouched pixels are never round-tripped).
  let r = fp.rect.intersect(work.rect)
  let n = fmt.channels
  let a = alphaIndex(fmt)
  var enc: array[8, float32]
  for y in r.y0 ..< r.y1:
    for x in r.x0 ..< r.x1:
      let k = perDab(fp.at(x, y) * strength, spacing)
      if k <= 0: continue
      let o = work.offset(x, y)
      let rgba = toRgba(fmt, work.data.toOpenArray(o, o + n - 1))
      let c = f([rgba[0], rgba[1], rgba[2]], k)
      discard fromRgbaInto(fmt, [c[0], c[1], c[2], rgba[3]], enc)
      for ch in 0 ..< n:
        if ch != a: work.data[o + ch] = enc[ch]

type ToneCell = ref object
  cov: seq[float32]
  orig: seq[float32]

proc toneStroke(fmt: PixelFormat; toned: proc(c: array[3, float32]): array[3, float32]): DabEffect =
  ## Dodge and Burn: a stroke tones each pixel once, from its original colour, mixed in by the
  ## stroke's coverage there (`toned` is the full-exposure colour). Dabs build up coverage as in a
  ## brush stroke, capped at full coverage, so Exposure caps a stroke the way Opacity caps a paint
  ## stroke: overlapping dabs and passes within one stroke don't compound the tone curve, and a new
  ## stroke builds on the last.
  # Side of the cells that remember, per touched pixel, the coverage applied so far and the original
  # pixel (copied from the working pixels the first time a dab reaches it).
  const Cell = 64
  var cells = initTable[(int, int), ToneCell]()
  let a = alphaIndex(fmt)
  let n = fmt.channels
  result = proc(work: var PxRegion; fp: Footprint) =
    let r = fp.rect.intersect(work.rect)
    if r.isEmpty: return
    var enc: array[8, float32]
    for cy in divEuclid(r.y0, Cell) .. divEuclid(r.y1 - 1, Cell):
      for cx in divEuclid(r.x0, Cell) .. divEuclid(r.x1 - 1, Cell):
        let cr = rect(cx * Cell, cy * Cell, cx * Cell + Cell, cy * Cell + Cell)
        let part = r.intersect(cr)
        if part.isEmpty: continue
        const len = Cell * Cell
        # Coverage -1: not touched yet, so `orig` isn't filled in for that pixel.
        var cell = cells.getOrDefault((cx, cy), nil)
        if cell.isNil:
          cell = ToneCell(cov: newSeq[float32](len), orig: newSeq[float32](len * n))
          for v in cell.cov.mitems: v = -1
          cells[(cx, cy)] = cell
        for y in part.y0 ..< part.y1:
          for x in part.x0 ..< part.x1:
            var k = clamp(fp.at(x, y), 0'f32, 1'f32)
            if k <= 0: continue
            let i = (y - cr.y0) * Cell + (x - cr.x0)
            let o = work.offset(x, y)
            if cell.cov[i] < 0:
              # First touch: the working pixel is still the original.
              for c in 0 ..< n: cell.orig[i * n + c] = work.data[o + c]
              cell.cov[i] = 0
            # Dabs build up the stroke's coverage as a brush stroke's do (flow), up to full
            # coverage; the tone is applied once, from the original.
            let prev = cell.cov[i]
            k = prev + k * (1'f32 - prev)
            if k <= prev: continue
            cell.cov[i] = k
            let src = toRgba(fmt, cell.orig.toOpenArray(i * n, i * n + n - 1))
            let t = toned([src[0], src[1], src[2]])
            let m = [src[0] + (t[0] - src[0]) * k, src[1] + (t[1] - src[1]) * k, src[2] + (t[2] - src[2]) * k]
            discard fromRgbaInto(fmt, [m[0], m[1], m[2], src[3]], enc)
            for c in 0 ..< n:
              if c != a: work.data[o + c] = enc[c]

proc toneRange(p: JsonNode; cmdId: string): ToneRange =
  let v = str(p, "range", "midtones")
  case v
  of "shadows": trShadows
  of "midtones": trMidtones
  of "highlights": trHighlights
  else: raise bad(cmdId, "unknown range `" & v & "` (shadows|midtones|highlights)")

type
  DabToolKind = enum dtTone, dtSponge, dtFocus, dtSmudge
  DabTool = object
    ## A sequential-dab tool's settings (Dodge, Burn, Sponge, Blur, Sharpen, Smudge), parsed once and
    ## turned into its per-dab effect.
    case kind: DabToolKind
    of dtTone:
      burn: bool
      range: ToneRange
      exposure: float32
      protectTones: bool
    of dtSponge:
      saturate, vibrance: bool
    of dtFocus:
      sharpen: bool
      focusStrength: float32
      protectDetail: bool
      sigma: float32
    of dtSmudge:
      smudgeStrength: float32
      finger: Option[array[4, float32]]
      maxSize: float32

proc parseDabTool(s: Session; cmdId: string; p: JsonNode; stroke: Stroke): (DabTool, string) =
  ## The tool for `cmdId` and its history label.
  case cmdId
  of "paint.dodge", "paint.burn":
    let burn = cmdId == "paint.burn"
    let rng = toneRange(p, cmdId)
    let tool = DabTool(kind: dtTone, burn: burn, range: rng,
                       exposure: clamp(num(p, "exposure", 50), 1'f32, 100'f32) / 100'f32,
                       protectTones: flag(p, "protectTones", true))
    (tool, (if burn: "Burn Tool" else: "Dodge Tool"))
  of "paint.sponge":
    let m = str(p, "mode", "desaturate")
    let saturate = case m
      of "desaturate": false
      of "saturate": true
      else: raise bad(cmdId, "unknown mode `" & m & "` (desaturate|saturate)")
    (DabTool(kind: dtSponge, saturate: saturate, vibrance: flag(p, "vibrance", true)), "Sponge Tool")
  of "paint.blur", "paint.sharpen":
    let sharpen = cmdId == "paint.sharpen"
    let tool = DabTool(kind: dtFocus, sharpen: sharpen,
                       focusStrength: clamp(num(p, "strength", 50), 1'f32, 100'f32) / 100'f32,
                       protectDetail: flag(p, "protectDetail", true),
                       sigma: clamp(stroke.brush.size * 0.03'f32, 1'f32, 4'f32))
    (tool, (if sharpen: "Sharpen Tool" else: "Blur Tool"))
  of "paint.smudge":
    let finger = (if flag(p, "fingerPainting", false): some(s.tools.foreground) else: none(array[4, float32]))
    (DabTool(kind: dtSmudge, smudgeStrength: clamp(num(p, "strength", 50), 1'f32, 100'f32) / 100'f32,
             finger: finger, maxSize: stroke.brush.size), "Smudge Tool")
  else: raise bad(cmdId, "not a sequential-dab tool")

proc halo(t: DabTool): int =
  ## How far around each dab the effect reads (Blur and Sharpen read a neighbourhood).
  if t.kind == dtFocus: asI32(float64(ceil(t.sigma * 3'f32))) + 1 else: 0

proc effect(t: DabTool; fmt: PixelFormat; spacing: float32): DabEffect =
  ## The per-dab effect on a working copy in `fmt`, for a brush of `spacing`.
  let sp = spacing
  case t.kind
  of dtTone:
    let (exposure, rng, burn, protect) = (t.exposure, t.range, t.burn, t.protectTones)
    result = toneStroke(fmt, proc(c: array[3, float32]): array[3, float32] = dodgeBurn(c, exposure, rng, burn, protect))
  of dtSponge:
    # Flow is already in each dab's coverage; a full-flow pass moves colours half-way.
    let (saturate, vibrance) = (t.saturate, t.vibrance)
    result = proc(work: var PxRegion; fp: Footprint) =
      colorDab(fmt, work, fp, 0.5, sp, proc(c: array[3, float32]; k: float32): array[3, float32] =
        sponge(c, k, saturate, vibrance))
  of dtFocus:
    let a = alphaOpt(fmt)
    let (sharpen, strength, protect, sigma) = (t.sharpen, t.focusStrength, t.protectDetail, t.sigma)
    result = proc(work: var PxRegion; fp: Footprint) =
      let r = fp.rect.intersect(work.rect)
      if r.isEmpty: return
      let (w, h, n) = (work.width, work.height, work.ch)
      let win = (r.x0 - work.rect.x0, r.y0 - work.rect.y0, r.x1 - work.rect.x0, r.y1 - work.rect.y0)
      let outp = (if sharpen: localSharpen(work.data, w, h, n, a, win, 1, protect)
                  else: localBlur(work.data, w, h, n, a, win, sigma))
      let rw = r.width
      for y in r.y0 ..< r.y1:
        for x in r.x0 ..< r.x1:
          let k = perDab(fp.at(x, y) * strength, sp)
          if k <= 0: continue
          let o = ((y - r.y0) * rw + (x - r.x0)) * n
          let q = work.offset(x, y)
          for c in 0 ..< n: work.data[q + c] += (outp[o + c] - work.data[q + c]) * k
  of dtSmudge:
    let fingerPx = (if t.finger.isSome: some(fromRgba(fmt, t.finger.get)) else: none(seq[float32]))
    # Premultiplied mixing: transparent pixels carry no colour, so no dark fringes.
    var sm = newSmudge(t.smudgeStrength, fingerPx, t.maxSize).withAlpha(alphaIndex(fmt))
    result = proc(work: var PxRegion; fp: Footprint) = sm.dab(work, fp)

type AllLayers = ref object
  ## Sample All Layers for the sequential-dab tools: each dab's effect runs on a copy of the visible
  ## composite (kept across dabs, so a dab sees the earlier ones), and the result is laid onto the
  ## layer under the dab. An empty layer above the image so picks up the blurred or smudged picture,
  ## and pixels the effect didn't change look as before (they are what was already visible).
  pre: Document
  fmt: PixelFormat
  visible: PxRegion
  ready: bool

proc dab(al: AllLayers; work: var PxRegion; fp: Footprint; effect: DabEffect) =
  if not al.ready:
    al.visible = compositeRegion(al.pre, none(LayerId), rsAll, work.rect, al.fmt)
    al.ready = true
  if al.visible.rect != work.rect or al.visible.ch != work.ch:
    effect(work, fp)
    return
  effect(al.visible, fp)
  let r = fp.rect.intersect(work.rect)
  let n = work.ch
  for y in r.y0 ..< r.y1:
    for x in r.x0 ..< r.x1:
      if fp.at(x, y) > 0:
        let o = work.offset(x, y)
        for c in 0 ..< n: work.data[o + c] = al.visible.data[o + c]

proc dabCmd(s: var Session; p: JsonNode; cmdId: string): JsonNode =
  ## Dodge, Burn, Sponge, Blur, Sharpen and Smudge: each dab edits a working copy in order
  ## (`applyDabStroke`). Blur, Sharpen and Smudge can sample all layers.
  let (stroke, id) = parseBrush(s, p, cmdId)
  let (tool, label) = parseDabTool(s, cmdId, p, stroke)
  let all = tool.kind in {dtFocus, dtSmudge} and sampleAllLayers(p)
  let dmg = runStroke(s, label, id, p, proc(pre: Document; surf: var Surface; sel: Option[Surface]; lock: bool): Rect =
    let fmt = surf.format
    let eff = tool.effect(fmt, stroke.brush.spacing)
    if all:
      let sampler = AllLayers(pre: pre, fmt: fmt)
      applyDabStroke(surf, stroke, sel, lock, tool.halo, proc(work: var PxRegion; fp: Footprint) = sampler.dab(work, fp, eff))
    else: applyDabStroke(surf, stroke, sel, lock, tool.halo, eff))
  %*{"damage": damageJson(dmg)}

# ---------------------------------------------------------------- Patch (`patch.rs`)

const MaxPatchPixels = 16'u64 shl 20
  ## Largest patch, move or removal, in pixels of the selection's (or stroke's) bounding box.

proc patchEnabledE(s: Session): string =
  ## A pixel target and a selection to patch with.
  let why = retouchPixelE(s)
  if why.len > 0: return why
  let d = s.active
  if d.isNil: return "no document open"
  if d.doc.selection.isSome: "" else: "select the area to patch first"

proc targetLayer(s: Session; p: JsonNode): Option[LayerId] =
  ## The layer `p` targets (none for an alpha channel or the Quick Mask), as `parseBrush`.
  if isChannelTarget(p): return none(LayerId)
  let l = p.pget("layer").asU64
  if l.isSome: return some(LayerId(l.get))
  let d = s.active
  if d.isNil or d.activeLayer.isNone: raise other("no active layer")
  d.activeLayer

proc dragOffset(cmdId: string; p: JsonNode): (int, int) =
  ## Integer drag offset, rejecting non-finite and absurd values.
  let o = point(p, "offset")
  if o.isNone: raise bad(cmdId, "missing `offset` ([dx, dy])")
  let (dx, dy) = o.get
  proc ok(v: float64): bool = v == v and abs(v) != Inf and abs(v) < 1e7
  if not ok(dx) or not ok(dy): raise bad(cmdId, "`offset` must be two finite numbers")
  (int(round(dx)), int(round(dy)))

proc selectionArea(sel: Surface; canvas: Rect): Rect =
  ## Selected area within the canvas: the selection's content bounds, or the whole canvas when the
  ## selection covers everything outside its tiles (an inverted selection).
  let dp = sel.defaultPixel
  let outside = (if dp.len > 0: dp[0] else: 0'f32)
  let b = (if outside > 0: canvas else: sel.contentBounds)
  b.intersect(canvas)

proc coverageOf(sel: Surface; rc: Rect): seq[float32] =
  ## Selection coverage over `rc` (row-major, 0..1).
  let n = max(sel.channels, 1)
  let data = sel.readRegion(rc)
  result = newSeq[float32](data.len div n)
  for i in 0 ..< result.len: result[i] = clamp(data[i * n], 0'f32, 1'f32)

proc healRegionCoarse(fmt: PixelFormat; src, dst: PxRegion; mask: seq[bool]; step: int): PxRegion =
  ## Seamless clone with the membrane solved `step` times coarser: the Poisson result is the source
  ## plus a harmonic (smooth) correction of the boundary mismatch, so that correction is solved on a
  ## grid of `step`×`step` cells and interpolated back, while the texture stays the full-resolution
  ## source. The live preview's approximation of `healRegion` (`step` 1 is exactly it).
  if step <= 1: return healRegion(fmt, src, dst, mask)
  let (w, h, n) = (dst.width, dst.height, dst.ch)
  let (cw, chh) = ((w + step - 1) div step, (h + step - 1) div step)
  # A cell is unknown when every pixel in it is patched; otherwise it holds the mean mismatch
  # `dst - src` of its unpatched pixels (the Dirichlet data of the coarse problem).
  var diff = newSeq[float32](cw * chh * n)
  var hole = newSeq[bool](cw * chh)
  for v in hole.mitems: v = true
  var count = newSeq[uint32](cw * chh)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = y * w + x
      if mask[i]: continue
      let c = (y div step) * cw + x div step
      hole[c] = false
      inc count[c]
      for k in 0 ..< n: diff[c * n + k] += dst.data[i * n + k] - src.data[i * n + k]
  for c, m in count:
    if m > 0:
      for k in 0 ..< n: diff[c * n + k] /= float32(m)
  let membrane = membraneFill(cw, chh, n, diff, hole)
  # Bilinear lookup of the membrane (cell centres at (c + 0.5) · step).
  proc lerpAt(p, cells: int): (int, int, float32) =
    let f = clamp((float32(p) + 0.5'f32) / float32(step) - 0.5'f32, 0'f32, float32(cells - 1))
    let c0 = int(floor(f))
    (c0, min(c0 + 1, cells - 1), f - float32(c0))
  var xs = newSeq[(int, int, float32)](w)
  for x in 0 ..< w: xs[x] = lerpAt(x, cw)
  result = dst
  for y in 0 ..< h:
    let (y0, y1, ty) = lerpAt(y, chh)
    for x in 0 ..< w:
      let i = y * w + x
      if not mask[i]: continue
      let (x0, x1, tx) = xs[x]
      for k in 0 ..< n:
        template v(cx, cy: int): float32 = membrane[(cy * cw + cx) * n + k]
        let m = (v(x0, y0) * (1 - tx) + v(x1, y0) * tx) * (1 - ty) + (v(x0, y1) * (1 - tx) + v(x1, y1) * tx) * ty
        result.data[i * n + k] = src.data[i * n + k] + m
  clampSamples(fmt, result.data)

type PatchPlan = object
  ## What a patch does, checked: the target layer, the selected area, and where it heals from and to.
  id: Option[LayerId]
  canvas, area: Rect
  dstShift, srcShift: (int, int) ## shift of the repaired area from the selection, of the texture from it
  destination: bool
  offset: (int, int)

proc patchPlan(s: Session; p: JsonNode): PatchPlan =
  const cmdId = "paint.patch"
  let m = str(p, "mode", "source")
  let destination = case m
    of "source": false
    of "destination": true
    else: raise bad(cmdId, "unknown mode `" & m & "` (source|destination)")
  let (dx, dy) = dragOffset(cmdId, p)
  let id = targetLayer(s, p)
  let d = s.active
  if d.isNil: raise other("no document open")
  let canvas = d.doc[].bounds
  if d.doc.selection.isNone: raise bad(cmdId, "select the area to patch first")
  let area = selectionArea(d.doc.selection.get, canvas)
  if area.isEmpty: raise bad(cmdId, "the selection is empty")
  let pixels = uint64(area.width) * uint64(area.height)
  if pixels > MaxPatchPixels:
    raise bad(cmdId, "the selection is too large to patch (" & $pixels & " pixels, at most " & $MaxPatchPixels & ")")
  if (dx, dy) == (0, 0): raise bad(cmdId, "drag the selection to the area to sample from")
  # Both the texture and the repaired area must lie on the canvas: off-canvas pixels are empty.
  if not canvas.containsRect(area.translate(dx, dy)): raise bad(cmdId, "the dragged patch must stay inside the canvas")
  let (dstShift, srcShift) = (if destination: ((dx, dy), (-dx, -dy)) else: ((0, 0), (dx, dy)))
  PatchPlan(id: id, canvas: canvas, area: area, dstShift: dstShift, srcShift: srcShift,
            destination: destination, offset: (dx, dy))

proc patchSurface(surf: var Surface; sel: Surface; plan: PatchPlan; lock: bool; step: int): Rect =
  ## Heal `surf` at the selection (shifted by `dstShift`) with texture read `srcShift` away, the
  ## membrane solved `step` times coarser (1 = exact). Returns the damaged rectangle.
  # The repaired area plus a two-pixel ring: the ring is the Dirichlet boundary of the solve. It is
  # clipped to where both the repaired and the sampled pixels lie on the canvas, so a patch at the
  # canvas edge sees a free (Neumann) boundary there instead of the empty pixels beyond it.
  let dstArea = plan.area.translate(plan.dstShift[0], plan.dstShift[1])
  let g = dstArea.inflate(2).intersect(plan.canvas).intersect(plan.canvas.translate(-plan.srcShift[0], -plan.srcShift[1]))
  if g.isEmpty: return EmptyRect
  let fmt = surf.format
  let dst = readPxRegion(surf, g)
  var src = readPxRegion(surf, g.translate(plan.srcShift[0], plan.srcShift[1]))
  src.rect = g
  # Selection coverage moved to the repaired place.
  let cov = coverageOf(sel, g.translate(-plan.dstShift[0], -plan.dstShift[1]))
  var mask = newSeq[bool](cov.len)
  for i, c in cov: mask[i] = c > 0
  let healed = healRegionCoarse(fmt, src, dst, mask, step)
  applyCoverage(surf, g, cov, 1, none(Surface), lock, healed, bmNormal)

proc patchPreview*(s: Session; p: JsonNode; maxCells: uint64; minStep: uint32): (Document, Rect) =
  ## The live preview of `paint.patch` with params `p`: the active document with the patch applied
  ## (nothing is recorded), and the rectangle it changed. For large patches the membrane is solved
  ## coarser, so the preview costs about the same whatever the selection's size.
  let plan = patchPlan(s, p)
  let d = s.active
  if d.isNil: raise other("no document open")
  let pixels = uint64(plan.area.width + 4) * uint64(plan.area.height + 4)
  # Smallest step whose grid fits `maxCells`: step² · cells ≥ pixels.
  let need = int(ceil(sqrt(float64(pixels) / float64(max(maxCells, 1'u64)))))
  let step = clamp(max(need, int(minStep)), 1, 64)
  var doc = d.doc[]
  if d.doc.selection.isNone: raise bad("paint.patch", "select the area to patch first")
  let sel = d.doc.selection.get
  let (surf, lock) = targetSurface(doc, plan.id, p)
  let dmg = patchSurface(surf[], sel, plan, lock, step)
  (doc, dmg)

proc levelParam(cmdId: string; p: JsonNode; k: string; default, lo, hi: uint8): uint8 =
  ## Optional whole-number param in `lo..hi`; absent or null gives `default`.
  let v = p.pget(k)
  if v.isNull: return default
  let x = v.asF64
  if x.isSome and x.get == x.get and abs(x.get) != Inf and x.get >= float64(lo) and x.get <= float64(hi):
    return uint8(round(x.get))
  raise bad(cmdId, "`" & k & "` must be a number " & $lo & ".." & $hi)

# ---------------------------------------------------------------- Content-Aware Move (`content_aware_move.rs`)

const CamMaxMargin = 512 ## cap on the sampling margin around the content (the window grows with it)

type
  CamTarget = object
    ## The pixels a step reads and writes: a layer (or, with none, the channel `p` targets), and
    ## whether reading samples the visible composite (Sample All Layers).
    id: Option[LayerId]
    allLayers: bool

  CamPlan = object
    ## What a move does, checked before any work starts.
    target: CamTarget
    canvas, area: Rect
    offset: (int, int)
    map: Option[Homography] ## Transform On Drop: where a source point lands; none for a plain move
    dest: Rect              ## where the content lands, on the canvas
    extend: bool
    structure, color: uint8

proc camEnabledE(s: Session): string =
  ## A pixel target and a selection to move.
  let why = retouchPixelE(s)
  if why.len > 0: return why
  let d = s.active
  if d.isNil: return "no document open"
  if d.doc.selection.isSome: "" else: "select the area to move first"

proc homog(m: array[9, float64]): Homography = Homography(m: m)

proc camPlacement(p: JsonNode; area: Rect; off: (int, int)): Option[Homography] =
  ## Transform On Drop's `scaleX` / `scaleY` (%) and `rotate` (degrees): the map from a source point
  ## to where it lands, about the centre of `area` and then moved by `off`. None when the params
  ## leave the content as it is (a plain move).
  const cmdId = "paint.contentAwareMove"
  proc number(k: string; default: float64): float64 =
    let v = p.pget(k)
    if v.isNull: return default
    let x = v.asF64
    if x.isNone or x.get != x.get or abs(x.get) == Inf: raise bad(cmdId, "`" & k & "` must be a finite number")
    x.get
  proc scale(k: string): float64 =
    let v = number(k, 100)
    # 1% to 10000% either way (a negative scale flips): beyond that the content collapses to
    # nothing or outgrows any selection the move can take.
    if abs(v) >= 1 and abs(v) <= 10_000: v / 100
    else: raise bad(cmdId, "`" & k & "` must be 1..10000 % (or -10000..-1 to flip)")
  let (sx, sy) = (scale("scaleX"), scale("scaleY"))
  var deg = number("rotate", 0)
  if abs(deg) > 36_000: raise bad(cmdId, "`rotate` must be -36000..36000 degrees")
  deg = deg mod 360.0
  if deg < 0: deg += 360.0
  if sx == 1 and sy == 1 and deg == 0: return none(Homography)
  let (cx, cy) = (float64(area.x0 + area.x1) / 2, float64(area.y0 + area.y1) / 2)
  proc t(x, y: float64): Homography = homog([1.0, 0, x, 0, 1, y, 0, 0, 1])
  let rad = degToRad(deg)
  let (sa, ca) = (sin(rad), cos(rad))
  let rot = homog([ca, -sa, 0, sa, ca, 0, 0, 0, 1])
  let sc = homog([sx, 0, 0, 0, sy, 0, 0, 0, 1])
  some(t(cx + float64(off[0]), cy + float64(off[1])).mul(rot).mul(sc).mul(t(-cx, -cy)))

proc mappedBounds(h: Homography; r: Rect): Rect =
  ## The pixel bounds of `r` mapped by `h`.
  var (x0, y0, x1, y1) = (Inf, Inf, -Inf, -Inf)
  for (cx, cy) in [(r.x0, r.y0), (r.x1, r.y0), (r.x1, r.y1), (r.x0, r.y1)]:
    let (x, y) = h.apply(float64(cx), float64(cy))
    x0 = rmin(x0, x)
    y0 = rmin(y0, y)
    x1 = rmax(x1, x)
    y1 = rmax(y1, y)
  proc fin(v: float64): bool = v == v and abs(v) != Inf
  if not (fin(x0) and fin(y0) and fin(x1) and fin(y1)): return EmptyRect
  # Rounded inwards by a hair so an exact scale doesn't grow the bounds by a pixel.
  proc cl(v: float64): int = int(clamp(v, -1e8, 1e8))
  rect(cl(floor(x0 + 1e-6)), cl(floor(y0 + 1e-6)), cl(ceil(x1 - 1e-6)), cl(ceil(y1 - 1e-6)))

proc camResample(src: PxRegion; inv: Homography; rc: Rect): PxRegion =
  ## `src` resampled (bilinear, edges clamped) through `inv` (destination → source) over `rc`: the
  ## content as it lands. Works on float samples, so at any depth.
  let (sw, sh, n) = (src.rect.width, src.rect.height, src.ch)
  result = newPxRegion(rc, n)
  if sw == 0 or sh == 0: return
  proc px(x, y, c: int): float32 =
    let i = (y * sw + x) * n + c
    if i < src.data.len: src.data[i] else: 0'f32
  let w = rc.width
  for i in 0 ..< rc.width * rc.height:
    let (qx, qy) = (rc.x0 + i mod w, rc.y0 + i div w)
    let (sx, sy) = inv.apply(float64(qx) + 0.5, float64(qy) + 0.5)
    let fx = clamp(sx - 0.5 - float64(src.rect.x0), 0.0, float64(sw - 1))
    let fy = clamp(sy - 0.5 - float64(src.rect.y0), 0.0, float64(sh - 1))
    if not (fx == fx and abs(fx) != Inf and fy == fy and abs(fy) != Inf): continue
    let (x0, y0) = (int(floor(fx)), int(floor(fy)))
    let (x1, y1) = (min(x0 + 1, sw - 1), min(y0 + 1, sh - 1))
    let (tx, ty) = (float32(fx - float64(x0)), float32(fy - float64(y0)))
    for c in 0 ..< n:
      let top = px(x0, y0, c) * (1 - tx) + px(x1, y0, c) * tx
      let bottom = px(x0, y1, c) * (1 - tx) + px(x1, y1, c) * tx
      result.data[i * n + c] = top * (1 - ty) + bottom * ty

proc landedMask(cov: seq[float32]; area: Rect; inv: Homography; rc: Rect): seq[bool] =
  ## The selection's shape as it lands over `rc`: `cov` (coverage over `area`) sampled through `inv`
  ## (destination → source), nearest, set wherever it is above zero.
  let w = rc.width
  result = newSeq[bool](w * rc.height)
  for i in 0 ..< result.len:
    let (qx, qy) = (rc.x0 + i mod w, rc.y0 + i div w)
    let (sx, sy) = inv.apply(float64(qx) + 0.5, float64(qy) + 0.5)
    let (x, y) = (floor(sx), floor(sy))
    if not (x >= float64(area.x0) and x < float64(area.x1)) or not (y >= float64(area.y0) and y < float64(area.y1)):
      continue
    let j = (int(y) - area.y0) * area.width + (int(x) - area.x0)
    result[i] = j < cov.len and cov[j] > 0

proc camPlan(s: Session; p: JsonNode): CamPlan =
  const cmdId = "paint.contentAwareMove"
  let m = str(p, "mode", "move")
  let extend = case m
    of "move": false
    of "extend": true
    else: raise bad(cmdId, "unknown mode `" & m & "` (move|extend)")
  let structure = levelParam(cmdId, p, "structure", 4, 1, 7)
  let color = levelParam(cmdId, p, "color", 0, 0, 10)
  let (dx, dy) = dragOffset(cmdId, p)
  let id = targetLayer(s, p)
  let d = s.active
  if d.isNil: raise noDocument()
  let canvas = d.doc[].bounds
  if d.doc.selection.isNone: raise bad(cmdId, "select the area to move first")
  let area = selectionArea(d.doc.selection.get, canvas)
  if area.isEmpty: raise bad(cmdId, "the selection is empty")
  let pixels = uint64(area.width) * uint64(area.height)
  if pixels > MaxPatchPixels:
    raise bad(cmdId, "the selection is too large to move (" & $pixels & " pixels, at most " & $MaxPatchPixels & ")")
  let map = camPlacement(p, area, (dx, dy))
  var dest: Rect
  if map.isNone:
    if (dx, dy) == (0, 0): raise bad(cmdId, "drag the selection to where the content should go")
    # Content dropped beyond the edge would be lost: keep it on the canvas, as the Patch Tool does.
    dest = area.translate(dx, dy)
    if not canvas.containsRect(dest): raise bad(cmdId, "the moved selection must stay inside the canvas")
  else:
    let landed = mappedBounds(map.get, area)
    let lp = uint64(landed.width) * uint64(landed.height)
    if lp > MaxPatchPixels:
      raise bad(cmdId, "the transformed selection is too large to move (" & $lp & " pixels, at most " & $MaxPatchPixels & ")")
    dest = landed.intersect(canvas)
    if dest.isEmpty: raise bad(cmdId, "the transformed selection must land on the canvas")
  let allLayers = flag(p, "sampleAllLayers", false) and targetsPixels(p)
  CamPlan(target: CamTarget(id: id, allLayers: allLayers), canvas: canvas, area: area, offset: (dx, dy),
          map: map, dest: dest, extend: extend, structure: structure, color: color)

proc bandWidth(structure: uint8; minSide: uint32): int =
  ## Width in pixels of the edge band re-synthesised at the new place, for Structure 1..7 and content
  ## `minSide` pixels across. Photoshop gives the same result for 3 to 7: the band is about a patch
  ## wide whatever the content's size; 2 and 1 widen it (1 so far that the content itself is
  ## re-synthesised).
  let side = float32(minSide)
  let w = case structure
    of 0, 1: rmax(side * 0.3'f32, 12'f32)
    of 2: rmax(side * 0.18'f32, 9'f32)
    else: 6'f32
  min(int(round(w)), 128)

proc erodeSquare(w, h: int; m: seq[bool]; r: int): seq[bool] =
  ## The cells of a `w × h` mask whose whole `(2r+1)²` neighbourhood, as far as it lies on the grid,
  ## is set (a square erosion through a summed-area table). The grid's border doesn't erode: a
  ## selection reaching the canvas edge keeps its content there.
  if r == 0: return m
  let w1 = w + 1
  var sat = newSeq[uint32](w1 * (h + 1))
  for y in 0 ..< h:
    var row = 0'u32
    for x in 0 ..< w:
      if y * w + x < m.len and m[y * w + x]: inc row
      sat[(y + 1) * w1 + x + 1] = sat[y * w1 + x + 1] + row
  result = newSeq[bool](w * h)
  for y in 0 ..< h:
    let (y0, y1) = (max(y - r, 0), min(y + r + 1, h))
    for x in 0 ..< w:
      if not (y * w + x < m.len and m[y * w + x]): continue
      let (x0, x1) = (max(x - r, 0), min(x + r + 1, w))
      let setCount = sat[y1 * w1 + x1] + sat[y0 * w1 + x0] - sat[y0 * w1 + x1] - sat[y1 * w1 + x0]
      result[y * w + x] = int(setCount) == (x1 - x0) * (y1 - y0)

proc camWindow(r, canvas: Rect): Rect =
  ## Where a fill samples from around `r`: Photoshop's Content-Aware window, at most `CamMaxMargin`
  ## pixels around `r`.
  samplingWindowFor(r, canvas).intersect(r.inflate(CamMaxMargin))

proc readTarget(doc: var Document; t: CamTarget; p: JsonNode; rc: Rect): PxRegion =
  ## The pixels a step works on: the targeted surface, or with Sample All Layers the visible
  ## composite (encoded in the target's format).
  let (surf, _) = targetSurface(doc, t.id, p)
  if t.allLayers: return compositeRegion(doc, none(LayerId), rsAll, rc, surf[].format)
  readPxRegion(surf[], rc)

proc replaceMasked(doc: var Document; t: CamTarget; p: JsonNode; outp: PxRegion; mask: seq[bool]): Rect =
  ## Write `outp` over `mask` into the target, replacing the pixels (the result already holds what
  ## should show there). With the transparency lock, alpha is kept and transparent pixels stay as
  ## they are. Returns the written rectangle. Shared with the Remove Tool.
  let (surf, lock) = targetSurface(doc, t.id, p)
  let a = alphaIndex(surf[].format)
  let rc = outp.rect
  var px = surf[].readRegion(rc)
  let n = outp.ch
  if px.len != outp.data.len or mask.len * n != px.len:
    raise other("internal error: fill buffers disagree in size")
  for i, m in mask:
    if not m: continue
    let o = i * n
    if lock and a >= 0:
      if px[o + a] > 0:
        for c in 0 ..< n:
          if c != a: px[o + c] = outp.data[o + c]
    else:
      for c in 0 ..< n: px[o + c] = outp.data[o + c]
  surf[].writeRegion(rc, px)
  rc

proc maskOf(sel: Surface; rc: Rect): seq[bool] =
  ## Selection coverage over `rc` as a mask (set wherever it is above zero).
  let cov = coverageOf(sel, rc)
  result = newSeq[bool](cov.len)
  for i, c in cov: result[i] = c > 0

proc camFillOptions(): FillOptions =
  ## Fill options: the fill's own colour adaptation (Edit › Content-Aware Fill's default), seeded so
  ## the result is reproducible.
  result = defaultFillOptions()
  result.seed = 0x00ca_4e00'u64

proc outerRing(gw, gh: int; m: seq[bool]; w: int): seq[bool] =
  ## A ring of `w` pixels just outside `m` (within the grid).
  var inside = newSeq[bool](m.len)
  for i, v in m: inside[i] = not v
  let keep = erodeSquare(gw, gh, inside, w)
  result = newSeq[bool](m.len)
  for i in 0 ..< m.len: result[i] = not keep[i] and not m[i]

proc land(doc: var Document; t: CamTarget; p: JsonNode; win: Rect; selected: seq[bool]; content: PxRegion;
          avoid: seq[bool]; structure, color: uint8; stage: (float32, float32); ctx: JobCtx; label: string): Rect =
  ## Content landing in `win`: the pixels of the window, with `selected` (the selection's shape
  ## there) to be replaced by `content`. The content's middle is copied, its level fitted to the new
  ## surroundings by `color`; a `bandWidth(structure)` band inside the selection's edge is
  ## re-synthesised from the surroundings and that middle, never sampling `avoid`. Writes the result
  ## into the target and returns the rectangle it changed.
  let (w, h) = (win.width, win.height)
  let here = readTarget(doc, t, p, win)
  let n = here.ch
  var (sx0, sy0, sx1, sy1) = (w, h, 0, 0)
  for i, sv in selected:
    if sv:
      let (x, y) = (i mod w, i div w)
      sx0 = min(sx0, x)
      sy0 = min(sy0, y)
      sx1 = max(sx1, x + 1)
      sy1 = max(sy1, y + 1)
  let side = uint32(min(max(sx1 - sx0, 0), max(sy1 - sy0, 0)))
  let core = erodeSquare(w, h, selected, bandWidth(structure, side))
  ctx.check()
  # Colour: the content's surroundings (where it came from) against the new ones, both just outside
  # the selection's shape.
  let ring = outerRing(w, h, selected, 6)
  proc pick(r: PxRegion): seq[float32] =
    for i, k in ring:
      if k:
        for c in 0 ..< n: result.add r.data[i * n + c]
  let gb = levelTransform(n, pick(content), pick(here), colorAmount(color))
  let fmt = targetSurface(doc, t.id, p)[0][].format
  let alpha = alphaIndex(fmt)
  var buf = here
  for i, c0 in core:
    if not c0: continue
    for c in 0 ..< gb.len:
      let (g, b) = gb[c]
      let v = content.data[i * n + c]
      # Colour channels are fitted; alpha is copied.
      buf.data[i * n + c] = (if c == alpha: v else: g * v + b)
  clampSamples(fmt, buf.data)
  # Structure: the band between the kept core and the selection's edge comes from the surroundings
  # and the kept core, never from what `avoid` marks.
  var band = newSeq[bool](selected.len)
  var anyBand = false
  for i in 0 ..< selected.len:
    band[i] = selected[i] and not core[i]
    if band[i]: anyBand = true
  if anyBand:
    var allowed = newSeq[bool](avoid.len)
    for i, o in avoid: allowed[i] = not o
    var opts = camFillOptions()
    opts.colorAdaptation = (if color == 0: colorLevel("default") else: colorAmount(color))
    let data = buf.data
    buf.data = staged(fillWith(w, h, n, data, band, allowed, opts, ctx.interrupt(stage[0], stage[1], label)))
  replaceMasked(doc, t, p, buf, selected)

proc runMove(doc: var Document; sel: Surface; plan: CamPlan; p: JsonNode; ctx: JobCtx; label: string): (Surface, Rect, Option[Surface]) =
  ## The new target surface, the rectangle it changed and (Transform On Drop) the transformed
  ## selection: the content pasted at the offset, then (`move`) its old place filled. Works on `doc`,
  ## a copy of the document.
  const cmdId = "paint.contentAwareMove"
  let (dx, dy) = plan.offset
  let opts = camFillOptions()
  let split = (if plan.extend: 1'f32 else: 0.4'f32)
  var inv = none(Homography)
  if plan.map.isSome:
    let i = plan.map.get.inverse
    if i.isNone: raise bad(cmdId, "the transform collapses the selection")
    inv = i
  # The selection's coverage over the area, which the transformed shape is sampled from.
  let cov = (if inv.isSome: coverageOf(sel, plan.area) else: newSeq[float32]())
  # 1. The content at its new place: the content with its original surroundings laid over the
  # window there; never sampling the content's old place.
  let win = camWindow(plan.dest, plan.canvas)
  var content: PxRegion
  var moved: seq[bool]
  if inv.isNone:
    content = readTarget(doc, plan.target, p, win.translate(-dx, -dy))
    content.rect = win
    moved = maskOf(sel, win.translate(-dx, -dy))
  else:
    # Only the content and a ring around it (the colour fit's reference) matter: read the source
    # behind that, a few pixels past the area, and resample it.
    var src = mappedBounds(inv.get, plan.dest.inflate(8)).intersect(plan.area.inflate(CamMaxMargin)).intersect(plan.canvas)
    if src.isEmpty: src = plan.area
    content = camResample(readTarget(doc, plan.target, p, src), inv.get, win)
    moved = landedMask(cov, plan.area, inv.get, win)
  let oldPlace = maskOf(sel, win)
  var damage = land(doc, plan.target, p, win, moved, content, oldPlace, plan.structure, plan.color, (0'f32, split), ctx, label)
  # 2. Move: fill the place the content left (where the content didn't land on it).
  if not plan.extend:
    ctx.check()
    let win2 = camWindow(plan.area, plan.canvas)
    let (w, h) = (win2.width, win2.height)
    let img = readTarget(doc, plan.target, p, win2)
    let landed = (if inv.isNone: maskOf(sel, win2.translate(-dx, -dy)) else: landedMask(cov, plan.area, inv.get, win2))
    let selHere = maskOf(sel, win2)
    var hole = newSeq[bool](selHere.len)
    var anyHole = false
    for i in 0 ..< hole.len:
      hole[i] = selHere[i] and not landed[i]
      if hole[i]: anyHole = true
    if anyHole:
      var allowed = newSeq[bool](landed.len)
      for i, l in landed: allowed[i] = not l
      let data = staged(fillWith(w, h, img.ch, img.data, hole, allowed, opts, ctx.interrupt(split, 1, label)))
      damage = damage.union(replaceMasked(doc, plan.target, p, PxRegion(rect: win2, ch: img.ch, data: data), hole))
  let surf = targetSurface(doc, plan.target.id, p)[0][]
  # The selection follows the transformed content, as Select › Transform Selection moves it.
  let movedSel = (if plan.map.isSome: some(warpGray(sel, plan.map.get, ipBilinear)) else: none(Surface))
  (surf, damage, movedSel)

proc contentAwareMove(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "paint.contentAwareMove"
  let plan = camPlan(s, p)
  let label = (if plan.extend: "Content-Aware Extend" else: "Content-Aware Move")
  let d = s.active
  if d.isNil: raise noDocument()
  let doc = d.doc[]
  runJob(s, label, true,
    proc(ctx: JobCtx): (Surface, Rect, Option[Surface]) =
      ctx.progress(0, label)
      var work = doc
      if doc.selection.isNone: raise bad(cmdId, "select the area to move first")
      runMove(work, doc.selection.get, plan, p, ctx, label),
    proc(s: var Session; r: (Surface, Rect, Option[Surface])): JsonNode =
      let (surf, damage, movedSel) = r
      let (dx, dy) = plan.offset
      s.edit(label, proc(doc: var Document; active: var Option[LayerId]) =
        targetSurface(doc, plan.target.id, p)[0][] = surf
        # The selection follows the content, ready for another drag.
        if movedSel.isSome:
          doc.selection = (if movedSel.get.contentBounds.isEmpty: none(Surface) else: movedSel)
        elif doc.selection.isSome:
          doc.selection = some(shiftSurface(doc.selection.get, dx, dy)))
      s.setDamage(damage)
      result = %*{"damage": damageJson(damage), "offset": [dx, dy], "mode": (if plan.extend: "extend" else: "move")}
      if plan.map.isSome: result["transformed"] = %true)

# ---------------------------------------------------------------- Patch commands

proc patchContentAware(s: var Session; p: JsonNode): JsonNode =
  ## Patch: Content-Aware. The selection is filled from the place it was dragged to, as the
  ## Content-Aware Move Tool lands its content.
  const cmdId = "paint.patch"
  const label = "Patch (Content-Aware)"
  let plan = patchPlan(s, p)
  let structure = levelParam(cmdId, p, "structure", 4, 1, 7)
  let color = levelParam(cmdId, p, "color", 0, 0, 10)
  let target = CamTarget(id: plan.id, allLayers: flag(p, "sampleAllLayers", false) and targetsPixels(p))
  let d = s.active
  if d.isNil: raise noDocument()
  let doc = d.doc[]
  let (dx, dy) = plan.offset
  let (area, canvas) = (plan.area, plan.canvas)
  runJob(s, label, true,
    proc(ctx: JobCtx): (Surface, Rect) =
      ctx.progress(0, label)
      var work = doc
      if doc.selection.isNone: raise bad(cmdId, "select the area to patch first")
      let sel = doc.selection.get
      let win = camWindow(area, canvas)
      var content = readTarget(work, target, p, win.translate(dx, dy))
      content.rect = win
      let selected = maskOf(sel, win)
      let avoid = newSeq[bool](selected.len)
      let damage = land(work, target, p, win, selected, content, avoid, structure, color, (0'f32, 1'f32), ctx, label)
      (targetSurface(work, target.id, p)[0][], damage),
    proc(s: var Session; r: (Surface, Rect)): JsonNode =
      let (surf, damage) = r
      s.edit(label, proc(doc: var Document; active: var Option[LayerId]) =
        targetSurface(doc, target.id, p)[0][] = surf)
      s.setDamage(damage)
      %*{"damage": damageJson(damage), "offset": [dx, dy], "contentAware": true})

proc patch(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "paint.patch"
  if flag(p, "contentAware", false): return patchContentAware(s, p)
  let plan = patchPlan(s, p)
  let label = (if plan.destination: "Patch (Destination)" else: "Patch")
  let dmg = runStroke(s, label, plan.id, p, proc(pre: Document; surf: var Surface; sel: Option[Surface]; lock: bool): Rect =
    if pre.selection.isNone: raise bad(cmdId, "select the area to patch first")
    patchSurface(surf, pre.selection.get, plan, lock, 1))
  %*{"damage": damageJson(dmg), "offset": [plan.offset[0], plan.offset[1]]}

# ---------------------------------------------------------------- Remove (`remove.rs`)

const
  RemoveMaxMargin = 512 ## cap on the sampling margin around the stroke
  RemoveGrow = 2        ## how far the hole grows beyond the stroke, in pixels
  RemoveSeed = 0x0052_e30e'u64

type RemovePlan = object
  ## What a removal does, checked before any work starts.
  id: Option[LayerId]
  stroke: Stroke
  window: Rect ## where the fill samples from: the stroke plus a margin, on the canvas
  closeLoops: bool
  allLayers: bool

proc samplingMargin(area: Rect): int =
  ## Three quarters of the area's larger side, at least 32 pixels.
  let ext = uint64(max(area.width, area.height))
  let v = ext * 3 div 4
  max((if v > uint64(high(int32)): int(high(int32)) else: int(v)), 32)

proc removeStrokeBounds(stroke: Stroke): Rect =
  ## The stroke's bounding box (points ± the brush radius), before any coverage is rendered.
  let r = float64(stroke.brush.size) / 2 + 2
  var (x0, y0, x1, y1) = (F64Max, F64Max, -F64Max, -F64Max)
  for q in stroke.points:
    x0 = rmin(x0, q.x)
    y0 = rmin(y0, q.y)
    x1 = rmax(x1, q.x)
    y1 = rmax(y1, q.y)
  # `checkCoords` keeps points within ±MaxCoord and sizes are capped, so these fit an i32.
  rect(asI32(floor(x0 - r)), asI32(floor(y0 - r)), asI32(ceil(x1 + r)), asI32(ceil(y1 + r)))

proc removePlan(s: Session; p: JsonNode): RemovePlan =
  const cmdId = "paint.remove"
  let (stroke, id) = parseBrush(s, p, cmdId)
  let d = s.active
  if d.isNil: raise noDocument()
  let canvas = d.doc[].bounds
  let bounds = removeStrokeBounds(stroke)
  let pixels = uint64(bounds.width) * uint64(bounds.height)
  if pixels > MaxPatchPixels:
    raise bad(cmdId, "the stroke is too large to remove (" & $pixels & " pixels, at most " & $MaxPatchPixels & ")")
  let area = bounds.intersect(canvas)
  if area.isEmpty: raise bad(cmdId, "the stroke is outside the canvas")
  let window = area.inflate(min(samplingMargin(area), RemoveMaxMargin)).intersect(canvas)
  RemovePlan(id: id, stroke: stroke, window: window, closeLoops: flag(p, "closeLoops", true),
             allLayers: flag(p, "sampleAllLayers", false) and targetsPixels(p))

proc removeHole(plan: RemovePlan; sel: Option[Surface]): seq[bool] =
  ## The hole over the plan's window: the stroke's footprint, its enclosed areas and a margin,
  ## limited to the selection.
  let win = plan.window
  let (w, h) = (win.width, win.height)
  let (bounds, cov) = strokeCoverage(plan.stroke)
  var m = newSeq[bool](w * h)
  let bw = max(bounds.width, 1)
  for i, c in cov:
    let (x, y) = (bounds.x0 + i mod bw, bounds.y0 + i div bw)
    if c > 0 and win.contains(x, y):
      let j = (y - win.y0) * w + (x - win.x0)
      if j < m.len: m[j] = true
  if plan.closeLoops: m = holes.closeLoops(w, h, m)
  m = holes.dilate(w, h, m, RemoveGrow)
  if sel.isSome:
    let c = coverageOf(sel.get, win)
    for i in 0 ..< min(m.len, c.len): m[i] = m[i] and c[i] > 0
  m

proc runRemove(doc: var Document; plan: RemovePlan; p: JsonNode; ctx: JobCtx; label: string): Option[(PxRegion, seq[bool])] =
  ## The filled window and the hole it fills, computed on `doc` (a snapshot). None when the layer's
  ## own window is fully transparent: nothing to remove and nothing to fill from.
  const cmdId = "paint.remove"
  let hole = removeHole(plan, doc.selection)
  var anySet, allSet = false
  allSet = true
  for m in hole:
    if m: anySet = true
    else: allSet = false
  if not anySet: raise bad(cmdId, "the stroke doesn't cover anything that can change (check the selection)")
  if allSet: raise bad(cmdId, "nothing is left around the stroke to fill it from")
  ctx.check()
  let win = plan.window
  let (surf, _) = targetSurface(doc, plan.id, p)
  let fmt = surf[].format
  let img = (if plan.allLayers: compositeRegion(doc, none(LayerId), rsAll, win, fmt) else: readPxRegion(surf[], win))
  # A no-op without the fill or a history step, as the other retouch tools on an empty layer.
  let a = alphaIndex(fmt)
  if not plan.allLayers and a >= 0:
    var clear = true
    var i = 0
    while i + img.ch <= img.data.len:
      if a < img.ch and img.data[i + a] > 0:
        clear = false
        break
      i += img.ch
    if clear: return none((PxRegion, seq[bool]))
  let (w, h) = (win.width, win.height)
  var data = staged(removeWith(w, h, img.ch, img.data, hole, RemoveSeed, ctx.interrupt(0, 1, label)))
  # The fill's seam blending can step slightly past the storable range.
  clampSamples(fmt, data)
  some((PxRegion(rect: win, ch: img.ch, data: data), hole))

proc removeCmd(s: var Session; p: JsonNode): JsonNode =
  const label = "Remove"
  let plan = removePlan(s, p)
  let d = s.active
  if d.isNil: raise noDocument()
  let doc = d.doc[]
  let id = plan.id
  runJob(s, label, true,
    proc(ctx: JobCtx): Option[(PxRegion, seq[bool])] =
      ctx.progress(0, label)
      var work = doc
      runRemove(work, plan, p, ctx, label),
    proc(s: var Session; filled: Option[(PxRegion, seq[bool])]): JsonNode =
      if filled.isNone: return %*{"changed": false}
      let (outp, hole) = filled.get
      let damage = s.editWith(label, proc(doc: var Document; active: var Option[LayerId]): Rect =
        replaceMasked(doc, CamTarget(id: id, allLayers: false), p, outp, hole))
      s.setDamage(damage)
      %*{"damage": damageJson(damage)})

# ---------------------------------------------------------------- Pattern Stamp (`pattern_stamp.rs`)

const
  MaxStampStrokePixels = 67_108_864'u64
    ## A single stroke's accumulated coverage must stay at or under this many pixels (8192²).
  StreamPhaseX = 101'u64
  StreamPhaseY = 102'u64

proc patternStampE(s: Session): string =
  let why = retouchPixelE(s)
  if why.len > 0: return why
  if resolvePattern(s, "").isNone: "no pattern selected" else: ""

proc stampScale(p: JsonNode; cmdId: string): float32 =
  let v = p.pget("scale")
  if v.isNil: return 1
  let x = v.asF64
  if x.isNone: raise bad(cmdId, "`scale` must be a number (1..1000 %)")
  if not (x.get >= 1 and x.get <= 1000): raise bad(cmdId, "scale must be 1..1000 %")
  float32(x.get) / 100'f32

proc stampAngle(p: JsonNode; cmdId: string): float32 =
  let v = p.pget("angle")
  if v.isNil: return 0
  let a = v.asF64
  if a.isNone: raise bad(cmdId, "`angle` must be a number (degrees)")
  if a.get != a.get or abs(a.get) == Inf: raise bad(cmdId, "`angle` must be finite")
  float32(a.get)

proc stampPhase(p: JsonNode; cmdId: string; aligned: bool; first: StrokePoint): (float32, float32) =
  let v = p.pget("phase")
  if v.isNil: return (if aligned: (0'f32, 0'f32) else: (float32(first.x), float32(first.y)))
  if not v.isArray: raise bad(cmdId, "`phase` must be [px, py]")
  let x = (if v.len > 0: v.elems[0].asF64 else: none(float64))
  if x.isNone: raise bad(cmdId, "`phase` must be [px, py]")
  let y = (if v.len > 1: v.elems[1].asF64 else: none(float64))
  if y.isNone: raise bad(cmdId, "`phase` must be [px, py]")
  if x.get != x.get or abs(x.get) == Inf or y.get != y.get or abs(y.get) == Inf:
    raise bad(cmdId, "`phase` coordinates must be finite")
  (float32(x.get), float32(y.get))

proc rejectHugeStroke(cmdId: string; stroke: Stroke) =
  let pad = float64(rmax(stroke.brush.size, 1'f32)) * 0.5 + 4.0
  var (minX, minY, maxX, maxY) = (Inf, Inf, -Inf, -Inf)
  for q in stroke.points:
    minX = rmin(minX, q.x)
    minY = rmin(minY, q.y)
    maxX = rmax(maxX, q.x)
    maxY = rmax(maxY, q.y)
  proc fin(v: float64): bool = v == v and abs(v) != Inf
  if not (fin(minX) and fin(minY) and fin(maxX) and fin(maxY)): raise bad(cmdId, "point coordinates must be finite")
  let w = rmax(ceil(maxX - minX + pad * 2), 1.0)
  let h = rmax(ceil(maxY - minY + pad * 2), 1.0)
  if not (fin(w) and fin(h)) or w > 1e9 or h > 1e9:
    raise bad(cmdId, "stroke too large (at most " & $MaxStampStrokePixels & " px)")
  let area = uint64(w) * uint64(h)
  if area > MaxStampStrokePixels:
    raise bad(cmdId, "stroke too large (" & $int64(w) & "×" & $int64(h) & " px, at most " & $MaxStampStrokePixels & ")")

proc rejectBounds(cmdId: string; bounds: Rect) =
  if bounds.isEmpty: return
  let (w, h) = (uint64(bounds.width), uint64(bounds.height))
  if w * h > MaxStampStrokePixels:
    raise bad(cmdId, "stroke too large (" & $w & "×" & $h & " px, at most " & $MaxStampStrokePixels & ")")

proc stampPlacement(phase: (float32, float32); scale, angle: float32): Placement =
  placement(EmptyRect, false, phase, scale, angle)

proc rgbaToRegion(fmt: PixelFormat; rc: Rect; rgba: seq[array[4, float32]]): PxRegion =
  let n = fmt.channels
  result = newPxRegion(rc, n)
  var enc: array[8, float32]
  for i, px in rgba:
    if (i + 1) * n > result.data.len: break
    discard fromRgbaInto(fmt, px, enc)
    for c in 0 ..< n: result.data[i * n + c] = enc[c]

proc paintAligned(surf: var Surface; stroke: Stroke; sel: Option[Surface]; lock: bool; tile: PatternTile;
                  phase: (float32, float32); scale, angle: float32; mode: BlendMode; cmdId: string): Rect =
  let (bounds, cov) = strokeCoverage(stroke)
  rejectBounds(cmdId, bounds)
  if bounds.isEmpty: return EmptyRect
  let rgba = renderPattern(tile, stampPlacement(phase, scale, angle), bounds)
  let paint = rgbaToRegion(surf.format, bounds, rgba)
  applyCoverage(surf, bounds, cov, stroke.brush.opacity, sel, lock, paint, mode)

proc stampDab(work: var PxRegion; fmt: PixelFormat; tile: PatternTile; fp: Footprint; phase: (float32, float32);
              scale, angle: float32; seed: uint64; tw, th: float32; mode: BlendMode) =
  let r = fp.rect.intersect(work.rect)
  if r.isEmpty: return
  let i = uint64(fp.index)
  let jx = randSigned(seed, i, StreamPhaseX) * tw
  let jy = randSigned(seed, i, StreamPhaseY) * th
  let rgba = renderPattern(tile, stampPlacement((phase[0] + jx, phase[1] + jy), scale, angle), r)
  let rw = r.width
  let n = work.ch
  var enc: array[8, float32]
  for y in r.y0 ..< r.y1:
    for x in r.x0 ..< r.x1:
      let k = fp.at(x, y)
      if k <= 0: continue
      let j = (y - r.y0) * rw + (x - r.x0)
      if j >= rgba.len: continue
      let px = rgba[j]
      discard fromRgbaInto(fmt, px, enc)
      let o = work.offset(x, y)
      if mode == bmNormal:
        overNative(fmt, work.data.toOpenArray(o, o + n - 1), enc.toOpenArray(0, n - 1), k, false)
      else:
        let dpx = toRgba(fmt, work.data.toOpenArray(o, o + n - 1))
        let c = pcolor.composite(mode, dpx, px, k)
        discard fromRgbaInto(fmt, c, enc)
        for ch in 0 ..< n: work.data[o + ch] = enc[ch]

proc patternStamp(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "paint.patternStamp"
  var (stroke, id) = parseBrush(s, p, cmdId)
  let mode = blendParam(p, cmdId)
  let aligned = flag(p, "aligned", true)
  let impressionist = flag(p, "impressionist", false)
  let pat = resolvePatternParam(s, cmdId, p)
  if pat.isEmpty: raise bad(cmdId, "the pattern is empty")
  let scale = stampScale(p, cmdId)
  let angle = stampAngle(p, cmdId)
  let phase = stampPhase(p, cmdId, aligned, stroke.points[0])
  let tile = newPatternTile(pat)
  if tile.isNil: raise bad(cmdId, "the pattern is empty")
  rejectHugeStroke(cmdId, stroke)
  let sv = p.pget("seed").asU64
  let seed = (if sv.isSome: sv.get else: seedFromBytes(if p.pget("points").isNil: "" else: jsonText(p.pget("points"))))
  stroke.brush.seed = seed
  let tw = float32(pat.width) * scale
  let th = float32(pat.height) * scale
  let st = stroke
  let dmg = s.editWith("Pattern Stamp", proc(doc: var Document; active: var Option[LayerId]): Rect =
    ensureInDoc(doc, pat)
    let pre = doc
    let (surf, lock) = targetSurface(doc, id, p)
    if impressionist:
      let fmt = surf[].format
      # Halo is the dab footprint only: Impressionist jitters phase, it does not blur.
      applyDabStroke(surf[], st, pre.selection, lock, 0, proc(work: var PxRegion; fp: Footprint) =
        stampDab(work, fmt, tile, fp, phase, scale, angle, seed, tw, th, mode))
    else: paintAligned(surf[], st, pre.selection, lock, tile, phase, scale, angle, mode, cmdId))
  s.setDamage(dmg)
  %*{"damage": damageJson(dmg), "phase": [phase[0], phase[1]], "aligned": aligned, "pattern": pat.id}

# ---------------------------------------------------------------- registry

const BrushParams = """"points":[[x,y,pressure?],…],"size":1..5000 px=tool size,"hardness":0..100=tool hardness,"opacity":1..100=100,"flow":1..100=100,"spacing":1..1000 (% of size)=Brush Settings spacing,"layer":id?=active,"target":"pixels"|"mask"|"quickMask"|{"channel":i}=Channels panel target"""

proc brushParams(extra: string): string = "{" & BrushParams & extra & "}"

proc retouchSpecs*(): seq[CommandSpec] =
  ## Retouching command specs.
  @[
    cmd("paint.cloneStamp", "Clone Stamp", [], "",
        brushParams(""","source":[sx,sy] (sampled under the first point) | "offset":[dx,dy],"aligned":bool=true,"sampleLayer":"current|currentAndBelow|all"="current","mode":"normal|multiply|…"="normal" → {"damage","offset","aligned","nextSource"}"""),
        retouchPixelE, runProc(cloneStamp(s, p))),
    cmd("paint.patternStamp", "Pattern Stamp", [], "",
        brushParams(""","pattern":id|name?=Patterns panel current,"scale":1..1000 %=100,"angle":deg=0,"aligned":bool=true,"impressionist":bool=false,"mode":"normal|multiply|…"="normal","phase":[px,py]? → {"damage","phase","aligned","pattern"}"""),
        patternStampE, runProc(patternStamp(s, p))),
    cmd("paint.healingBrush", "Healing Brush", [], "",
        brushParams(""","source":[sx,sy] | "offset":[dx,dy],"aligned":bool=true,"sampleLayer":"current|currentAndBelow|all"="current","mode":"normal|…"="normal" → {"damage","offset","aligned","nextSource"}"""),
        retouchPixelE, runProc(healingBrush(s, p))),
    cmd("paint.spotHealing", "Spot Healing Brush", [], "",
        brushParams(""","type":"contentAware|createTexture|proximityMatch"="contentAware","sampleAllLayers":bool=false"""),
        retouchPixelE, runProc(spotHealing(s, p))),
    cmd("paint.patch", "Patch", [], "",
        """{"offset":[dx,dy] (how far the selection was dragged),"mode":"source|destination"="source","contentAware":bool=false (fill the selection content-aware from the dragged-to place; a background job),"structure":1..7=4 (content-aware: 3..7 copy the middle exactly, 1..2 re-synthesise more of it),"color":0..10=0 (content-aware: how far the copy's level adapts to the selection's surroundings),"sampleAllLayers":bool=false (content-aware),"layer":id?=active,"target":"pixels"|"mask"|"quickMask"|{"channel":i}=Channels panel target} → {"damage","offset"}""",
        patchEnabledE, runProc(patch(s, p))),
    cmd("paint.contentAwareMove", "Content-Aware Move", [], "",
        """{"offset":[dx,dy] (how far the selection was dragged),"mode":"move|extend"="move","structure":1..7=4 (3..7 copy the content exactly but for a band about a patch wide along its edge; 2 widens the band, 1 re-synthesises the content too),"color":0..10=0 (0 keeps the content's level; higher values bring it towards its new place, by up to about 0.04 × color),"sampleAllLayers":bool=false,"scaleX":%=100,"scaleY":%=100 (Transform On Drop: 1..10000, negative flips),"rotate":deg=0 (Transform On Drop, clockwise; scale and rotation are about the selection's centre, before the offset; a transformed patch may reach past the canvas edge),"layer":id?=active,"target":"pixels"|"mask"|"quickMask"|{"channel":i}=Channels panel target} → {"damage","offset","mode","transformed"?} (a background job; the selection moves, and is transformed, with the content)""",
        camEnabledE, runProc(contentAwareMove(s, p))),
    cmd("paint.remove", "Remove", [], "",
        brushParams(""","closeLoops":bool=true (a stroke around an object removes what it encloses),"sampleAllLayers":bool=false → {"damage"} (a background job)"""),
        retouchPixelE, runProc(removeCmd(s, p))),
    cmd("paint.dodge", "Dodge", [], "",
        brushParams(""","range":"shadows|midtones|highlights"="midtones","exposure":1..100=50,"protectTones":bool=true"""),
        retouchPixelE, runProc(dabCmd(s, p, "paint.dodge"))),
    cmd("paint.burn", "Burn", [], "",
        brushParams(""","range":"shadows|midtones|highlights"="midtones","exposure":1..100=50,"protectTones":bool=true"""),
        retouchPixelE, runProc(dabCmd(s, p, "paint.burn"))),
    cmd("paint.sponge", "Sponge", [], "",
        brushParams(""","mode":"desaturate|saturate"="desaturate","vibrance":bool=true (flow = sponge strength)"""),
        retouchPixelE, runProc(dabCmd(s, p, "paint.sponge"))),
    cmd("paint.blur", "Blur", [], "",
        brushParams(""","strength":1..100=50,"sampleAllLayers":bool=false"""),
        retouchPixelE, runProc(dabCmd(s, p, "paint.blur"))),
    cmd("paint.sharpen", "Sharpen", [], "",
        brushParams(""","strength":1..100=50,"protectDetail":bool=true,"sampleAllLayers":bool=false"""),
        retouchPixelE, runProc(dabCmd(s, p, "paint.sharpen"))),
    cmd("paint.smudge", "Smudge", [], "",
        brushParams(""","strength":1..100=50,"fingerPainting":bool=false (starts with the foreground colour),"sampleAllLayers":bool=false"""),
        retouchPixelE, runProc(dabCmd(s, p, "paint.smudge"))),
    cmd("paint.historyBrush", "History Brush", [], "",
        brushParams(""","state":index (into history.entries; default 0 = the oldest held state, normally the Open snapshot)"""),
        retouchPixelE, runProc(historyBrush(s, p))),
  ]
