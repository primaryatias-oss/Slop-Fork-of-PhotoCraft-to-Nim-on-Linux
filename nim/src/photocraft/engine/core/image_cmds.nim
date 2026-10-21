## Image menu (port of `image_cmds.rs`): Image Size, Canvas Size, Crop, Trim, Mode (colour model
## and bit depth) conversions, Duplicate. `forEachSurface` lives in `pixels.nim`. Part of the
## `engine/core` unit: included, not imported.

proc compound*[R](s: var Session; label: string; f: proc(s: var Session): R): R =
  ## Several edits as one undo step named `label` (`analysis_cmds::compound`): a failure rolls the
  ## document, its history and the journal back to before the first.
  let st = s.activeOrRaise()
  let before = st.doc
  let history = st.history
  let prior = st[].layerTarget
  let journal = s.journal.len
  # Color Sampler points are view state a step may move (crop, rotation): roll them back too.
  let samplers = st.colorSamplers
  var r: R
  try:
    r = f(s)
  except CatchableError:
    s.journal.setLen(journal)
    let st2 = s.active
    if st2.isNil: raise noDocument()
    st2.history = history
    st2.doc = before
    st2.colorSamplers = samplers
    inc st2.revision
    st2.lastDamage = none(Rect)
    raise
  let st2 = s.active
  if st2.isNil: raise noDocument()
  st2.history = history
  # As in `edit`: undo targets what was selected just before the step.
  st2.history.setCurrentLayers(prior)
  st2.history.record(label, before, st2[].layerTarget)
  discard st2.history.trim(st2.doc[])
  st2.coalesce = none(string)
  r

proc convertLayersDepth*(layers: var seq[Layer]; depth: SampleType) =
  ## Converts every pixel surface of a layer tree (masks included) to `depth`.
  forEachSurface(layers, true, proc(surf: var Surface; isMask: bool) =
    surf = surf.convert(surf.format.withSample(depth)))

proc forEachLayer*(layers: var seq[Layer]; f: proc(l: var Layer)) =
  for l in layers.mitems:
    f(l)
    if l.content.kind == lkGroup: forEachLayer(l.content.group.children, f)

proc reidentifyCopiedLayers*(doc: var Document) =
  ## Assign new layer ids to a copied document and update every saved layer-comp reference
  ## (references to already-deleted layers stay as they are).
  var ids = initTable[uint64, LayerId]()
  forEachLayer(doc.layers, proc(l: var Layer) =
    let previous = l.id
    l.id = freshLayerId()
    ids[uint64(previous)] = l.id)
  proc remap(comp: var LayerComp; ids: Table[uint64, LayerId]) =
    for st in comp.states.mitems:
      if uint64(st.layer) in ids: st.layer = ids[uint64(st.layer)]
  for comp in doc.layerComps.mitems: remap(comp, ids)
  if doc.lastDocumentState.isSome: remap(doc.lastDocumentState.get, ids)

proc scaleEffects*(fx: var Effects; k: float32) =
  for e in fx.items.mitems:
    case e.kind
    of ekDropShadow, ekInnerShadow:
      e.shadow.distance *= k
      e.shadow.size *= k
    of ekOuterGlow, ekInnerGlow: e.glow.size *= k
    of ekStroke: e.stroke.size *= k
    of ekSatin:
      e.satin.distance *= k
      e.satin.size *= k
    of ekBevelEmboss:
      e.bevel.size *= k
      e.bevel.soften *= k
    of ekPatternOverlay: e.scale *= k
    of ekGradientOverlay, ekColorOverlay: discard
    if e.kind == ekStroke and e.stroke.paint.kind == fpPattern: e.stroke.paint.scale *= k
  # Effects changed: the preserved PSD block no longer applies.
  if k != 1: fx.psdRaw = nil

type
  MethodKind = enum mkNone, mkFilter, mkAutomatic
  ResampleMethod = object
    ## Image Size's resampling methods; `mkNone` changes the resolution only.
    kind: MethodKind
    filter: Resample

proc parseResampleMethod(s: string): ResampleMethod =
  case s
  of "none": ResampleMethod(kind: mkNone)
  of "automatic": ResampleMethod(kind: mkAutomatic)
  else:
    let f = case s
      of "nearest", "nearestNeighbor": rsNearest
      of "bilinear": rsBilinear
      of "bicubicSmoother": rsBicubicSmoother
      of "bicubicSharper": rsBicubicSharper
      of "bicubicAutomatic": rsBicubicAutomatic
      of "lanczos": rsLanczos
      of "preserveDetails": rsPreserveDetails
      else: rsBicubic
    ResampleMethod(kind: mkFilter, filter: f)

const MaxResampleBytes = 8'u64 shl 30
  ## The largest raster Image Size resamples to: the decoders' default allocation budget.

proc satMulU64(a, b: uint64): uint64 =
  if a != 0 and b > high(uint64) div a: high(uint64) else: a * b

proc checkResampleBudget*(cmdId: string; doc: Document; nw, nh: int; hint: string) =
  ## Refuses to resample `doc` to `nw` x `nh` px when the raster alone would pass the budget.
  let bpp = uint64(max(doc.pixelFormat.bytesPerPixel, 1))
  let bytes = satMulU64(satMulU64(uint64(nw), uint64(nh)), bpp)
  if bytes <= MaxResampleBytes: return
  let mp = satMulU64(uint64(nw), uint64(nh)) div 1_000_000
  let maxMp = MaxResampleBytes div bpp div 1_000_000
  raise bad(cmdId, $nw & " x " & $nh & " px is " & $mp & " megapixels; resampling at this bit depth is limited to " &
    $maxMp & " megapixels. Choose a smaller size" & hint)

proc resampleDoc(doc: var Document; nw, nh: int; filter: Resample) =
  ## Resamples the whole document (pixels, masks, channels, selection, effects, vectors, guides).
  let (ow, oh) = (doc.size.width, doc.size.height)
  let (sx, sy) = (float64(nw) / float64(max(ow, 1)), float64(nh) / float64(max(oh, 1)))
  if abs(sx - 1) < 1e-12 and abs(sy - 1) < 1e-12: return
  # Content that reaches the canvas edge keeps it: a Background stays opaque to the border.
  let canvas = rectXywh(0, 0, int(ow), int(oh))
  forEachSurface(doc.layers, true, proc(surf: var Surface; isMask: bool) =
    surf = resizeSurfaceInCanvas(surf, sx, sy, if isMask: rsBilinear else: filter, canvas))
  let k = float32((sx + sy) / 2)
  forEachLayer(doc.layers, proc(l: var Layer) = scaleEffects(l.effects, k))
  for ch in doc.channels.mitems: ch.surface = resizeSurfaceInCanvas(ch.surface, sx, sy, rsBilinear, canvas)
  if doc.quickMask.isSome:
    doc.quickMask.get.surface = resizeSurfaceInCanvas(doc.quickMask.get.surface, sx, sy, rsBilinear, canvas)
  if doc.selection.isSome: doc.selection = some(resizeSurfaceInCanvas(doc.selection.get, sx, sy, rsBilinear, canvas))
  # Vector geometry, guides and marks scale with the pixels; vectors re-render sharp.
  transformGeometry(doc, Affine(m: [sx, 0.0, 0.0, sy, 0.0, 0.0]))
  doc.size = Size(width: nw, height: nh)
  refreshDoc(doc, rfAll)

proc clampSide(v: float64; lo, hi: float64): int = int(clamp(round(v), lo, hi))

proc imageSize(s: var Session; p: JsonNode): JsonNode =
  let d = s.activeOrRaise()
  let (ow, oh) = (d.doc.size.width, d.doc.size.height)
  let resample = parseResampleMethod(p.getStrOr("resample", "bicubic"))
  let pw = p.pget("width").asF64
  let ph = p.pget("height").asF64
  # A missing dimension keeps the aspect ratio.
  var (nwf, nhf) = (float64(ow), float64(oh))
  if pw.isSome and ph.isSome: (nwf, nhf) = (pw.get, ph.get)
  elif pw.isSome: (nwf, nhf) = (pw.get, pw.get * float64(oh) / float64(max(ow, 1)))
  elif ph.isSome: (nwf, nhf) = (ph.get * float64(ow) / float64(max(oh, 1)), ph.get)
  let (nw, nh) = (clampSide(nwf, 1, 300_000), clampSide(nhf, 1, 300_000))
  # Each side may reach 300000 px, but resampling writes real pixels: refuse a canvas whose raster
  # alone would pass the budget, before allocating any of it.
  if resample.kind != mkNone and (nw, nh) != (ow, oh):
    checkResampleBudget("image.imageSize", d.doc[], nw, nh, ", or turn Resample off to change only the resolution")
  let rv = p.pget("resolution").asF64
  let dpi = if rv.isSome: some(float32(rv.get)) else: none(float32)
  if resample.kind != mkNone and (nw, nh) != (ow, oh):
    # Color Sampler points scale with the pixels.
    let (sx, sy) = (float64(nw) / float64(max(ow, 1)), float64(nh) / float64(max(oh, 1)))
    s.setSamplerMap(Affine(m: [sx, 0.0, 0.0, sy, 0.0, 0.0]))
  s.editDo("Image Size"):
    if dpi.isSome: doc.resolutionDpi = clamp(dpi.get, 1'f32, 10_000'f32)
    let filter = case resample.kind
      of mkNone: return
      of mkFilter: resample.filter
      of mkAutomatic: (if nw > ow or nh > oh: rsPreserveDetails else: rsBicubicSharper)
    resampleDoc(doc, nw, nh, filter)
  # The size the document has now: without resampling `width`/`height` are ignored.
  let size = s.activeOrRaise().doc.size
  %*{"width": size.width, "height": size.height}

proc translateDoc*(doc: var Document; cmdId: string; dx, dy: int) =
  ## Moves every surface, channel, selection and guide by `(dx, dy)`. Fails, before moving
  ## anything, when some pixels would land outside the i32 coordinate range.
  if dx == 0 and dy == 0: return
  proc fits(r: Rect; dx, dy: int): bool =
    proc ok(v: int): bool = v >= int(int32.low) and v <= int(int32.high)
    r.isEmpty or (ok(r.x0 + dx) and ok(r.x1 + dx) and ok(r.y0 + dy) and ok(r.y1 + dy))
  var good = true
  forEachSurface(doc.layers, true, proc(surf: var Surface; isMask: bool) =
    good = good and fits(surf.contentBounds, dx, dy))
  for ch in doc.channels: good = good and fits(ch.surface.contentBounds, dx, dy)
  if doc.quickMask.isSome: good = good and fits(doc.quickMask.get.surface.contentBounds, dx, dy)
  if doc.selection.isSome: good = good and fits(doc.selection.get.contentBounds, dx, dy)
  if not good:
    raise bad(cmdId, "moving the document by (" & $dx & ", " & $dy & ") would push its pixels outside the 32-bit coordinate range")
  forEachSurface(doc.layers, true, proc(surf: var Surface; isMask: bool) = surf = translateSurface(surf, dx, dy))
  for ch in doc.channels.mitems: ch.surface = translateSurface(ch.surface, dx, dy)
  if doc.quickMask.isSome: doc.quickMask.get.surface = translateSurface(doc.quickMask.get.surface, dx, dy)
  if doc.selection.isSome: doc.selection = some(translateSurface(doc.selection.get, dx, dy))
  # Type, shapes, smart objects, vector masks, paths, guides, slices, notes…
  transformGeometry(doc, Affine(m: [1.0, 0.0, 0.0, 1.0, float64(dx), float64(dy)]))

proc fillKeeping(surf: var Surface; fill: Rect; keep: Rect; color: array[4, float32]) =
  ## Fills `fill` with `color`, keeping the pixels inside `keep`.
  let fmt = surf.format
  let px = fromRgba(fmt, color)
  let saved = surf.toInterleaved(keep)
  surf.fillRect(fill, px)
  if not keep.isEmpty: surf.writeInterleaved(keep, saved)

proc cropDoc(doc: var Document; cmdId: string; r: Rect; deletePixels: bool; extension: array[4, float32]) =
  ## Crops the document to `r` (in current document coordinates). A frame that extends past the
  ## canvas fills the Background's new area with `extension`; other layers get transparency.
  # The origin moves to (0, 0); `-i32::MIN` has no i32 value.
  if r.x0 == int(int32.low) or r.y0 == int(int32.low):
    raise bad(cmdId, "the crop origin (" & $r.x0 & ", " & $r.y0 & ") can't be moved to (0, 0) within the 32-bit coordinate range")
  let (dx, dy) = (-r.x0, -r.y0)
  let oldCanvas = doc.bounds
  let extends = r.x0 < oldCanvas.x0 or r.y0 < oldCanvas.y0 or r.x1 > oldCanvas.x1 or r.y1 > oldCanvas.y1
  if deletePixels:
    forEachSurface(doc.layers, false, proc(surf: var Surface; isMask: bool) = surf = cropSurface(surf, r))
  translateDoc(doc, cmdId, dx, dy)
  doc.size = Size(width: r.width, height: r.height)
  # A Background cannot hold transparent pixels: the area an extending Crop adds is filled with
  # the extension (background) colour.
  if extends and doc.layers.len > 0 and isBackground(doc.layers[0]) and doc.layers[0].content.kind == lkRaster:
    let old = oldCanvas.translate(dx, dy)
    template surf: untyped = doc.layers[0].content.raster
    let keep = surf.contentBounds.intersect(old)
    fillKeeping(surf, rectXywh(0, 0, r.width, r.height), keep, [extension[0], extension[1], extension[2], 1'f32])
  refreshDoc(doc, rfShapes)

proc anchorFactors(a: string): (float64, float64) =
  let x = if "Left" in a or a == "left": 0.0 elif "Right" in a or a == "right": 1.0 else: 0.5
  let y = if a.startsWith("top"): 0.0 elif a.startsWith("bottom"): 1.0 else: 0.5
  (x, y)

proc extensionColor(s: Session; p: JsonNode): array[4, float32] =
  let v = p.getStrOr("extensionColor", "background")
  case v
  of "foreground": s.tools.foreground
  of "white": [1'f32, 1, 1, 1]
  of "black": [0'f32, 0, 0, 1]
  of "transparent": [0'f32, 0, 0, 0]
  else:
    if v.startsWith("#"):
      var h = v
      var i = 0
      while i < h.len and h[i] == '#': inc i
      h = h[i .. ^1]
      proc c(h: string; i: int): float32 =
        if i + 2 > h.len: return 0
        let b = hexByte(h[i ..< i + 2])
        if b.isSome: float32(b.get) / 255 else: 0'f32
      [c(h, 0), c(h, 2), c(h, 4), 1'f32]
    else: s.tools.background

proc canvasSize(s: var Session; p: JsonNode): JsonNode =
  let d = s.activeOrRaise()
  let (ow, oh) = (float64(d.doc.size.width), float64(d.doc.size.height))
  let relative = p.getBoolOr("relative", false)
  let wv = p.pget("width").asF64
  let hv = p.pget("height").asF64
  let w = if wv.isSome: wv.get elif relative: 0.0 else: ow
  let h = if hv.isSome: hv.get elif relative: 0.0 else: oh
  let (nwf, nhf) = if relative: (ow + w, oh + h) else: (w, h)
  let (nw, nh) = (clampSide(nwf, 1, 300_000), clampSide(nhf, 1, 300_000))
  let (ax, ay) = anchorFactors(p.getStrOr("anchor", "center"))
  let dx = asI32(round((float64(nw) - ow) * ax))
  let dy = asI32(round((float64(nh) - oh) * ay))
  let ext = extensionColor(s, p)
  s.setSamplerMap(Affine(m: [1.0, 0.0, 0.0, 1.0, float64(dx), float64(dy)]))
  s.editDo("Canvas Size"):
    translateDoc(doc, "image.canvasSize", dx, dy)
    doc.size = Size(width: nw, height: nh)
    refreshDoc(doc, rfShapes)
    let canvas = doc.bounds
    let old = rectXywh(dx, dy, int(ow), int(oh))
    # A Background cannot contain transparent pixels: a transparent extension that adds canvas
    # area makes it a normal paintable layer first. A shrink or no-op must not convert it.
    if ext[3] == 0 and (nw > int(ow) or nh > int(oh)) and doc.layers.len > 0 and isBackground(doc.layers[0]):
      unlockBackground(doc.layers[0])
    # The locked Background layer is extended with the extension colour.
    if ext[3] > 0 and doc.layers.len > 0 and doc.layers[0].name == "Background" and doc.layers[0].locks.transparency and
       doc.layers[0].content.kind == lkRaster:
      template surf: untyped = doc.layers[0].content.raster
      let keep = surf.contentBounds.intersect(old)
      fillKeeping(surf, canvas, keep, ext)
  %*{"width": nw, "height": nh, "offset": [dx, dy]}

const
  MaxCropSide = 300_000
    ## The largest side an explicit crop may give the canvas (the `file.new` and Canvas Size limit).
  MaxCropResolution = 10_000.0
    ## The highest resolution (ppi) a crop may set, as for Image Size.
  MaxCropAngle = 3600.0
    ## The largest crop angle accepted (degrees), as for Image Rotation › Arbitrary.

type CropTarget = object
  ## The Crop tool's W x H x Resolution: the size (px) the cropped area is resampled to and the
  ## resolution the document gets, both optional.
  size: Option[(int, int)]
  resolution: Option[float32]
  filter: Resample

proc apply(t: CropTarget; doc: var Document) =
  ## Applies the target to a document that has just been cropped (inside the crop's history step).
  if t.size.isSome: resampleDoc(doc, t.size.get[0], t.size.get[1], t.filter)
  if t.resolution.isSome: doc.resolutionDpi = t.resolution.get

proc cropTarget(p: JsonNode): CropTarget =
  const cmdId = "image.crop"
  proc side(p: JsonNode; key: string): Option[int] =
    let v = p.pget(key)
    if v.isNull: return
    let n = v.asU64
    if n.isSome and n.get >= 1 and n.get <= uint64(MaxCropSide): return some(int(n.get))
    raise bad(cmdId, "`" & key & "` = " & jsonText(v) & " must be a whole number of pixels from 1 to " & $MaxCropSide)
  let tw = side(p, "targetWidth")
  let th = side(p, "targetHeight")
  if tw.isSome and th.isSome: result.size = some((tw.get, th.get))
  elif tw.isSome or th.isSome: raise bad(cmdId, "`targetWidth` and `targetHeight` go together")
  let rv = p.pget("resolution")
  if not rv.isNull:
    let r = rv.asF64
    if r.isNone or r.get != r.get or abs(r.get) == Inf or r.get < 1 or r.get > MaxCropResolution:
      raise bad(cmdId, "`resolution` = " & jsonText(rv) & " must be a number of pixels per inch from 1 to " & displayFloat(MaxCropResolution))
    result.resolution = some(float32(r.get))
  let sv = p.pget("resample")
  var filter = some(rsBicubic)
  if not sv.isNull:
    if sv.kind != JString: raise bad(cmdId, "`resample` must be a string")
    let m = parseResampleMethod(sv.str)
    filter = case m.kind
      of mkNone: none(Resample)
      of mkFilter: some(m.filter)
      # The direction is known only once the frame is: bicubic's own automatic choice.
      of mkAutomatic: some(rsBicubicAutomatic)
  if filter.isSome: result.filter = filter.get
  elif result.size.isNone: result.filter = rsBicubic
  else: raise bad(cmdId, "a `targetWidth` x `targetHeight` crop needs resampling: `resample` can't be \"none\"")

proc cropResult(s: Session; frame: JsonNode): JsonNode =
  ## A crop's result: the frame it cropped to, plus the document's size and resolution afterwards.
  result = frame
  let d = s.active
  if not d.isNil and result.isObject:
    result["documentWidth"] = %d.doc.size.width
    result["documentHeight"] = %d.doc.size.height
    result["resolution"] = %float64(d.doc.resolutionDpi)

proc cropAngle(p: JsonNode): Option[float64] =
  ## `image.crop`'s optional `angle` in degrees, normalised to (-180, 180]; none when absent, null
  ## or (after normalising) zero.
  let v = p.pget("angle")
  if v.isNull: return
  let a0 = v.asF64
  if a0.isNone: raise bad("image.crop", "`angle` must be a number of degrees")
  let a1 = a0.get
  if a1 != a1 or abs(a1) == Inf or abs(a1) > MaxCropAngle:
    raise bad("image.crop", "`angle` must be a finite number of degrees within ±" & displayFloat(MaxCropAngle))
  var a = remEuclidF(a1, 360.0)
  if a > 180: a -= 360
  if abs(a) >= 1e-9: some(a) else: none(float64)

proc rotatedCrop(s: var Session; r: Rect; angle: float64; delete: bool; target: CropTarget): JsonNode =
  ## The Crop tool's rotated frame: the document is rotated counter-clockwise by `angle` (Image
  ## Rotation › Arbitrary), which makes the frame upright, then cropped to it, in one undo step.
  const cmdId = "image.crop"
  let size = s.activeOrRaise().doc.size
  let (w, h) = (float64(r.width), float64(r.height))
  let (cx, cy) = (float64(r.x0) + w / 2, float64(r.y0) + h / 2)
  let nsz = rotatedSize(size, -angle)
  let (sn, cs) = (sin(degToRad(-angle)), cos(degToRad(-angle)))
  let (dx, dy) = (cx - float64(size.width) / 2, cy - float64(size.height) / 2)
  let (ncx, ncy) = (float64(nsz.width) / 2 + cs * dx - sn * dy, float64(nsz.height) / 2 + sn * dx + cs * dy)
  let (x, y) = (round(ncx - w / 2), round(ncy - h / 2))
  let far = bad(cmdId, "the rotated frame is outside the 32-bit coordinate range")
  let limit = float64(int32.high div 2)
  if not (x == x and abs(x) != Inf and y == y and abs(y) != Inf) or abs(x) > limit or abs(y) > limit: raise far
  let (xi, yi) = (int(x), int(y))
  proc endOf(o, len: int): Option[int] =
    let e = o + len
    if e > int(int32.high) or e < int(int32.low): none(int) else: some(e)
  let x1 = endOf(xi, r.width)
  let y1 = endOf(yi, r.height)
  if x1.isNone or y1.isNone: raise far
  let tgt = rect(xi, yi, x1.get, y1.get)
  discard compound(s, "Crop", proc(s: var Session): bool =
    discard rotateArbitrary(s, %*{"angle": angle, "direction": "ccw"})
    # Color Sampler points follow the crop's origin shift too (the rotation moved them above).
    s.setSamplerMap(Affine(m: [1.0, 0.0, 0.0, 1.0, -float64(tgt.x0), -float64(tgt.y0)]))
    let extension = s.tools.background
    s.editDo("Crop"):
      cropDoc(doc, cmdId, tgt, delete, extension)
      doc.selection = none(Surface)
      target.apply(doc)
    true)
  cropResult(s, %*{"x": r.x0, "y": r.y0, "width": r.width, "height": r.height, "angle": angle})

proc cropCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "image.crop"
  let delete = p.getBoolOr("deleteCroppedPixels", true)
  # Values beyond i32 would wrap into a rectangle unrelated to the numbers passed; reject them.
  let x = intI32(cmdId, p, "x")
  let y = intI32(cmdId, p, "y")
  let w = intI32(cmdId, p, "width")
  let h = intI32(cmdId, p, "height")
  var explicit = none(Rect)
  if x.isSome and y.isSome and w.isSome and h.isSome and w.get > 0 and h.get > 0:
    # The new canvas size: refuse what File › New, Image Size and Canvas Size won't make.
    if w.get > MaxCropSide or h.get > MaxCropSide:
      raise bad(cmdId, $w.get & " x " & $h.get & " exceeds the " & $MaxCropSide & " px limit per side")
    proc endOf(o, len: int32; ko, kl: string): int =
      let e = int64(o) + int64(len)
      if e > int64(int32.high): raise bad(cmdId, "`" & ko & "` + `" & kl & "` = " & $e & " is outside the 32-bit coordinate range")
      int(e)
    let x1 = endOf(x.get, w.get, "x", "width")
    let y1 = endOf(y.get, h.get, "y", "height")
    explicit = some(rect(int(x.get), int(y.get), x1, y1))
  let target = cropTarget(p)
  if target.size.isSome:
    let d = s.activeOrRaise()
    checkResampleBudget(cmdId, d.doc[], target.size.get[0], target.size.get[1], "")
  let a = cropAngle(p)
  if a.isSome:
    if explicit.isNone: raise bad(cmdId, "an `angle` needs an explicit `x`, `y`, `width` and `height`")
    return rotatedCrop(s, explicit.get, a.get, delete, target)
  # An explicit rectangle (the Crop tool) may extend past the canvas; a selection crop is clamped.
  var r = EmptyRect
  if explicit.isSome: r = explicit.get
  else:
    let d = s.active
    if not d.isNil and d.doc.selection.isSome: r = d.doc.selection.get.contentBounds.intersect(d.doc[].bounds)
  if r.isEmpty: raise other("nothing to crop: pass x/y/width/height or make a selection")
  s.setSamplerMap(Affine(m: [1.0, 0.0, 0.0, 1.0, -float64(r.x0), -float64(r.y0)]))
  let extension = s.tools.background
  s.editDo("Crop"):
    cropDoc(doc, cmdId, r, delete, extension)
    doc.selection = none(Surface)
    target.apply(doc)
  cropResult(s, %*{"x": r.x0, "y": r.y0, "width": r.width, "height": r.height})

proc trimCmd(s: var Session; p: JsonNode): JsonNode =
  let d = s.activeOrRaise()
  let canvas = d.doc[].bounds
  let buf = flatten(d.doc[])
  let based = p.getStrOr("basedOn", "transparent")
  let w = canvas.width
  var reference = none(array[4, float32])
  case based
  of "topLeft": (if buf.px.len > 0: reference = some(buf.px[0]))
  of "bottomRight": (if buf.px.len > 0: reference = some(buf.px[^1]))
  else: discard
  proc differs(q: array[4, float32]; reference: Option[array[4, float32]]): bool =
    if reference.isNone: return q[3] > 0
    let r = reference.get
    for c in 0 .. 3:
      if abs(q[c] * q[3] - r[c] * r[3]) > 0.5'f32 / 255 or (c == 3 and abs(q[3] - r[3]) > 0.5'f32 / 255): return true
    false
  var b = EmptyRect
  for i, q in buf.px:
    if differs(q, reference):
      let (x, y) = (i mod w, i div w)
      b = if b.isEmpty: rect(x, y, x + 1, y + 1) else: b.union(rect(x, y, x + 1, y + 1))
  if b.isEmpty: raise other("nothing to trim to")
  proc side(p: JsonNode; k: string): bool = p.getBoolOr(k, true)
  let r = rect(if side(p, "left"): b.x0 else: canvas.x0, if side(p, "top"): b.y0 else: canvas.y0,
               if side(p, "right"): b.x1 else: canvas.x1, if side(p, "bottom"): b.y1 else: canvas.y1)
  s.setSamplerMap(Affine(m: [1.0, 0.0, 0.0, 1.0, -float64(r.x0), -float64(r.y0)]))
  let extension = s.tools.background
  s.editDo("Trim"):
    cropDoc(doc, "image.trim", r, true, extension)
  %*{"x": r.x0, "y": r.y0, "width": r.width, "height": r.height}

proc convertDepth(s: var Session; depth: SampleType): JsonNode =
  ## Image → Mode → 8/16/32 Bits/Channel.
  let current = s.activeOrRaise().doc.depth
  # History::record clears redo even when an edit closure leaves the document unchanged.
  if current == depth: return jnull()
  s.editDo("Bit Depth"):
    convertLayersDepth(doc.layers, depth)
    for ch in doc.channels.mitems: ch.surface = ch.surface.convert(ch.surface.format.withSample(depth))
    if doc.quickMask.isSome:
      doc.quickMask.get.surface = doc.quickMask.get.surface.convert(doc.quickMask.get.surface.format.withSample(depth))
    doc.depth = depth
  jnull()

proc duplicateImage(s: var Session; p: JsonNode): JsonNode =
  ## Image → Duplicate.
  let d = s.activeOrRaise()
  var doc = d.doc[]
  doc.id = freshDocId()
  let nv = p.pget("name")
  doc.name = if not nv.isNil and nv.kind == JString: nv.str else: d.doc.name & " copy"
  if p.getBoolOr("mergedOnly", false):
    let buf = flatten(doc)
    let fmt = doc.pixelFormat
    var surf = newSurface(fmt)
    var vals = newSeqOfCap[float32](buf.px.len * fmt.channels)
    for q in buf.px: vals.add fromRgba(fmt, q)
    surf.writeRegion(doc.bounds, vals)
    surf.prune()
    doc.layers = @[newLayer("Background", rasterContent(surf))]
    # A merged-only copy has no original layers for saved comps to restore.
    doc.layerComps.setLen(0)
    doc.lastAppliedComp = none(uint32)
    doc.lastDocumentState = none(LayerComp)
  reidentifyCopiedLayers(doc)
  let name = doc.name
  let index = s.addDocument(doc)
  %*{"document": index, "name": name}

proc imageSpecs*(): seq[CommandSpec] =
  const modeParams = """{"profile":"<builtin id>|working|/path/to/profile.icc"=working,"intent":"perceptual|relative|saturation|absolute"="relative","bpc":bool=true}"""
  @[
    cmd("image.imageSize", "Image Size…", ["Image"], "",
        """{"width":px,"height":px,"resolution":ppi,"resample":"automatic|preserveDetails|bicubicSmoother|bicubicSharper|bicubicAutomatic|bicubic|nearest|bilinear|lanczos|none"="bicubic"} → {width,height} (the document's size after the command; "none" changes the resolution only)""",
        hasDocE, imageSize),
    cmd("image.canvasSize", "Canvas Size…", ["Image"], "",
        """{"width":px,"height":px,"relative":bool=false,"anchor":"topLeft|top|topRight|left|center|right|bottomLeft|bottom|bottomRight"="center","extensionColor":"background|foreground|white|black|transparent|#rrggbb"="background"}""",
        hasDocE, canvasSize),
    cmd("image.crop", "Crop", ["Image"], "",
        """{"x":px,"y":px,"width":px,"height":px,"angle":-3600..3600=0,"deleteCroppedPixels":bool=true,"targetWidth":px,"targetHeight":px,"resolution":ppi,"resample":"bicubic|bilinear|nearest|lanczos|preserveDetails"="bicubic"} → {x,y,width,height,documentWidth,documentHeight,resolution} (targetWidth/targetHeight, both or neither: the cropped area is resampled to exactly that size, as Image Size, in the same undo step; resolution: the document's new ppi)""",
        hasDocE, cropCmd),
    cmd("image.trim", "Trim…", ["Image"], "",
        """{"basedOn":"transparent|topLeft|bottomRight"="transparent","top":bool=true,"bottom":bool=true,"left":bool=true,"right":bool=true}""",
        hasDocE, trimCmd),
    cmd("image.mode.rgb", "RGB Color", ["Image", "Mode"], "", modeParams, hasDocE, runProc(convertMode(s, cmRgb, p))),
    cmd("image.mode.grayscale", "Grayscale", ["Image", "Mode"], "", modeParams, hasDocE, runProc(convertMode(s, cmGrayscale, p))),
    cmd("image.mode.cmyk", "CMYK Color", ["Image", "Mode"], "", modeParams, hasDocE, runProc(convertMode(s, cmCmyk, p))),
    cmd("image.mode.lab", "Lab Color", ["Image", "Mode"], "", modeParams, hasDocE, runProc(convertMode(s, cmLab, p))),
    cmd("image.mode.bits8", "8 Bits/Channel", ["Image", "Mode"], "", "{}", hasDocE, runProc(convertDepth(s, stU8))),
    cmd("image.mode.bits16", "16 Bits/Channel", ["Image", "Mode"], "", "{}", hasDocE, runProc(convertDepth(s, stU16))),
    cmd("image.mode.bits32", "32 Bits/Channel", ["Image", "Mode"], "", "{}", hasDocE, runProc(convertDepth(s, stF32))),
    cmd("image.duplicate", "Duplicate…", ["Image"], "", """{"name":str,"mergedOnly":bool=false}""", hasDocE, duplicateImage),
  ]
