## Everyday Photoshop commands: clipboard (Cut/Copy/Copy Merged/Paste/Paste in Place), Layer via
## Copy/Cut, Merge Visible, Auto Tone/Contrast/Color, Toggle Last State, Transform Again and the
## guide commands (port of `edit_cmds.rs`). Part of the `engine/core` unit: included, not imported.

const PasteTarget* = "\"target\":\"pixels\"|\"mask\"|\"quickMask\"|{\"channel\":i}=Channels panel target (not pixels: the luminosity pasted into that mask or channel)"

proc takesWhole(d: DocState; id: LayerId): bool =
  ## Photoshop's layer-based copy: with no selection, Copy takes a smart object, type or shape
  ## layer, or a group, whole.
  if d.doc.selection.isSome or d.selectedOrdered.len > 1 or not layered(d.doc.mode): return false
  let l = d.doc[].findLayer(id)
  not l.isNil and l.content.kind in {lkSmart, lkText, lkShape, lkGroup}

proc scratchDoc(doc: Document): Document =
  ## A scratch document in the source's colour, to hold copied layers.
  result = newDocument("", doc.size, doc.mode, doc.depth)
  result.iccProfile = doc.iccProfile
  result.resolutionDpi = doc.resolutionDpi
  result.patterns = doc.patterns

proc wholeLayer(d: DocState; id: LayerId): Option[DocRef] =
  let l = d.doc[].findLayer(id)
  if l.isNil or not takesWhole(d, id): return
  let copies = DocRef()
  copies[] = scratchDoc(d.doc[])
  copies.layers.add l[]
  some(copies)

proc multiCopyIds(d: DocState): seq[LayerId] =
  ## The layers Copy takes when several are selected and nothing is selected in the pixels.
  if d.doc.selection.isSome or not layered(d.doc.mode): return
  let selected = d.selectedOrdered
  if selected.len < 2: return
  topLevel(d.doc[], selected)

proc wholeLayers(d: DocState; ids: openArray[LayerId]): Option[DocRef] =
  let copies = DocRef()
  copies[] = scratchDoc(d.doc[])
  for id in ids:
    let p = d.doc[].findLayer(id)
    if p.isNil: return none(DocRef)
    var l = p[]
    if isBackground(l):
      l.name = "Layer 0"
      l.locks = Locks()
    copies.layers.add l
  some(copies)

proc hasClipOnlyE(s: Session): string = (if s.clipboard.isSome: "" else: "the clipboard is empty")

proc ecActiveLayer(s: Session): (ptr Layer, string) =
  let d = s.active
  if d.isNil: return (nil, "no document open")
  if d.activeLayer.isNone: return (nil, "no active layer")
  let l = d.doc[].findLayer(d.activeLayer.get)
  if l.isNil: (nil, "no active layer") else: (l, "")

proc hasPixelsE(s: Session): string =
  let (l, why) = ecActiveLayer(s)
  if l.isNil: return why
  case l.content.kind
  of lkRaster: ""
  of lkSmart: "the smart object is not directly editable"
  else: "the active layer has no pixels"

proc hasLayerPixelsE(s: Session): string =
  let (l, why) = ecActiveLayer(s)
  if l.isNil: return why
  if l[].hasSurface: "" else: "the active layer has no pixels"

proc canCopyE(s: Session): string =
  if copiesAlphaChannel(s): return ""
  let d = s.active
  if not d.isNil and multiCopyIds(d[]).len > 0: return ""
  let e = hasLayerPixelsE(s)
  if e.len == 0: return ""
  if not d.isNil and d.activeLayer.isSome and takesWhole(d[], d.activeLayer.get): return ""
  e

proc hasClipE(s: Session): string =
  if s.active.isNil: return "no document open"
  hasClipOnlyE(s)

proc activeId(s: Session): LayerId =
  let d = s.active
  if d.isNil or d.activeLayer.isNone: raise other("no active layer")
  d.activeLayer.get

proc lift(src: Surface; sel: Option[Surface]; canvas: Rect): Clip =
  ## The pixels of `src` inside the selection (or all of them), as a straight-alpha surface.
  let fmt = src.format
  let withAlpha = PixelFormat(mode: fmt.mode, sample: fmt.sample, alpha: true)
  let area = if sel.isSome: sel.get.contentBounds.intersect(canvas) else: src.contentBounds.intersect(canvas)
  var outp = newSurface(withAlpha)
  if area.isEmpty: return Clip(surface: outp, bounds: EmptyRect)
  let n = withAlpha.channels
  var px: seq[float32]
  if fmt.alpha: px = allocReadRegion(src, area, "copying pixels")
  else:
    let opaque = allocReadRegion(src, area, "copying pixels")
    let k = max(fmt.channels, 1)
    px = allocFilled(opaque.len div k * n, 0'f32, "copying pixels")
    var at = 0
    for i in 0 ..< opaque.len div k:
      for j in 0 ..< k: px[at + j] = opaque[i * k + j]
      at += k
      px[at] = 1
      inc at
  if sel.isSome:
    let mask = allocReadRegion(sel.get, area, "applying the selection to copied pixels")
    let mk = max(sel.get.format.channels, 1)
    for i in 0 ..< min(px.len div n, mask.len div mk):
      px[i * n + n - 1] *= mask[i * mk]
  outp.writeRegion(area, px)
  outp.prune()
  Clip(surface: outp, bounds: outp.contentBounds)

proc mergedSurface(doc: Document): Surface =
  ## Merged composite of the visible document as a surface in the document's format.
  let fmt = doc.pixelFormat
  flattenToSurface(doc, PixelFormat(mode: fmt.mode, sample: fmt.sample, alpha: true))

proc copyCmd(s: var Session; merged: bool): JsonNode =
  let d = s.activeOrRaise()
  let canvas = d.doc[].bounds
  var clip: Clip
  if merged: clip = lift(mergedSurface(d.doc[]), d.doc.selection, canvas)
  else:
    if d.activeLayer.isNone: raise other("no active layer")
    let id = d.activeLayer.get
    let several = multiCopyIds(d[])
    let layers = if several.len == 0: wholeLayer(d[], id) else: wholeLayers(d[], several)
    let l = d.doc[].findLayer(id)
    if not l.isNil and l[].hasSurface and several.len == 0:
      clip = lift(l[].surface, d.doc.selection, canvas)
      clip.layers = layers
    elif layers.isSome:
      # A group has no pixels of its own: the clipboard pixels are what it shows.
      var shown = layers.get[]
      for x in shown.layers.mitems: x.visible = true
      clip = lift(mergedSurface(shown), none(Surface), canvas)
      clip.layers = layers
    else: raise other("the active layer has no pixels")
  if clip.bounds.isEmpty: raise other("Could not copy: the selected area is empty")
  let b = clip.bounds
  s.clipboard = some(clip)
  %*{"bounds": [b.x0, b.y0, b.width, b.height]}

proc copyMask(s: var Session): JsonNode =
  ## Copy with the layer mask targeted: the mask's values as a grayscale clip.
  let d = s.activeOrRaise()
  let canvas = d.doc[].bounds
  if d.activeLayer.isNone: raise other("no active layer")
  let id = d.activeLayer.get
  let l = d.doc[].findLayer(id)
  if l.isNil: raise noLayer(id)
  if l.mask.isNone: raise other("the active layer has no layer mask")
  let mask = l.mask.get
  let sel = d.doc.selection
  let area = if sel.isSome: sel.get.contentBounds.intersect(canvas) else: canvas
  if area.isEmpty: raise other("Could not copy: the selected area is empty")
  let n = max(mask.surface.format.channels, 1)
  let values = allocReadRegion(mask.surface, area, "copying the mask")
  var cover: seq[float32]
  var mk = 1
  if sel.isSome:
    cover = sel.get.readRegion(area)
    mk = max(sel.get.format.channels, 1)
  var px = allocFilled(values.len div n * 2, 0'f32, "copying the mask")
  for i in 0 ..< values.len div n:
    px[i * 2] = values[i * n]
    px[i * 2 + 1] = if sel.isSome: (if i * mk < cover.len: cover[i * mk] else: 0'f32) else: 1'f32
  let fmt = PixelFormat(mode: cmGrayscale, sample: mask.surface.format.sample, alpha: true)
  var surface = newSurface(fmt)
  surface.writeRegion(area, px)
  surface.prune()
  let b = surface.contentBounds
  if b.isEmpty: raise other("Could not copy: the selected area is empty")
  s.clipboard = some(Clip(surface: surface, bounds: b))
  %*{"bounds": [b.x0, b.y0, b.width, b.height], "target": "mask"}

proc clearArea*(doc: var Document; id: LayerId; area: Rect; sel: Option[Surface]; bg: array[4, float32]) =
  ## Cut / Clear on a layer: makes the selected pixels transparent. The Background is filled with
  ## the background colour instead (Photoshop).
  let l = doc.findLayer(id)
  if l.isNil: raise noLayer(id)
  let isBg = isBackground(l[])
  let surf = paintSurface(doc, id, newJNull())
  if isBg: tryFillSurface(surf[], area, bg, sel, true)
  else: tryClearSurface(surf[], area, sel)
  surf[].prune()

proc clearSelected(doc: var Document; id: LayerId; background: array[4, float32]) =
  let sel = doc.selection
  let canvas = doc.bounds
  let area = if sel.isSome: sel.get.contentBounds.intersect(canvas) else: canvas
  clearArea(doc, id, area, sel, background)

proc pasteOffset*(clip: Clip; canvas: Rect; p: JsonNode; inPlace: bool): (int, int) =
  ## How far a paste moves the clipboard pixels.
  if inPlace or (clip.bounds.intersect(canvas) == clip.bounds and p.pget("center").isNil): return (0, 0)
  let c = p.pget("center")
  var cx = float64(canvas.x0 + canvas.x1) / 2.0
  var cy = float64(canvas.y0 + canvas.y1) / 2.0
  if c.isArray and c.len >= 2:
    cx = c.elems[0].asF64.get(0.0)
    cy = c.elems[1].asF64.get(0.0)
  let b = clip.bounds
  (asI32(round(cx - float64(b.x0 + b.x1) / 2.0)), asI32(round(cy - float64(b.y0 + b.y1) / 2.0)))

proc shifted*(surface: Surface; dx, dy: int): Surface =
  if dx == 0 and dy == 0: surface else: translateSurface(surface, dx, dy)

proc pasteToTarget*(s: var Session; p: JsonNode; inPlace: bool; limit: Option[Surface]; label: string): JsonNode =
  ## Paste into the targeted layer mask, alpha channel or Quick Mask instead of a new layer.
  allocCheckpoint("pasting pixels")
  if s.clipboard.isNone: raise other("the clipboard is empty")
  let clip = s.clipboard.get
  let d = s.activeOrRaise()
  let canvas = d.doc[].bounds
  let layer = d.activeLayer
  let (dx, dy) = pasteOffset(clip, canvas, p, inPlace)
  let src = shifted(clip.surface, dx, dy)
  let area = src.contentBounds.intersect(canvas)
  s.editDo(label):
    let (surf, _) = targetSurface(doc, layer, p)
    compositeOver(surf[], src, area, limit)
    surf[].prune()
    doc.selection = none(Surface)
  %*{"target": (if p.pget("target").isNil: newJNull() else: p.pget("target")), "offset": [dx, dy]}

proc newFromClipboard(s: var Session): JsonNode =
  ## A new document the size of the clipboard image, holding it as its one layer.
  if s.clipboard.isNone: raise other("the clipboard is empty")
  let clip = s.clipboard.get
  let b = clip.bounds
  let w = b.width
  let h = b.height
  if w == 0 or h == 0: raise other("the clipboard image has no size")
  let fmt = clip.surface.format
  let target = PixelFormat(mode: fmt.mode, sample: fmt.sample, alpha: true)
  let moved = if b.x0 == 0 and b.y0 == 0: clip.surface else: translateSurface(clip.surface, -b.x0, -b.y0)
  var doc = newDocument("Untitled", size(w, h), fmt.mode, fmt.sample)
  var l = rasterLayer(doc.nextLayerName("Layer"), target)
  l.pixelsMut[] = if moved.format == target: moved else: moved.convert(target)
  doc.layers.add l
  let i = s.addDocument(doc)
  %*{"document": i, "width": w, "height": h}

proc pasteLayers(s: var Session; layers: Document; dx, dy: int): JsonNode =
  ## Paste of whole copied layers: fresh copies, converted to the document's colour, moved as
  ## the pixels would be, above the active layer.
  let d = s.activeOrRaise()
  var copies = layers
  for l in copies.layers.mitems: l = l.duplicate
  adaptCopies(s, copies, d.doc[])
  translateCopies(copies, dx, dy)
  var patterns: seq[DocPattern]
  for pat in copies.patterns:
    var have = false
    for q in d.doc.patterns:
      if q.id == pat.id: have = true
    if not have: patterns.add pat
  let id = s.editGet("Paste", LayerId):
    doc.patterns.add patterns
    for l in copies.layers: active = some(doc.insertAbove(active, l))
    doc.selection = none(Surface)
    if active.isNone: raise other("nothing to paste")
    active.get
  %*{"layer": id, "offset": [dx, dy]}

proc pasteCmd(s: var Session; p: JsonNode; inPlace: bool): JsonNode =
  ## Paste as a new layer (or into the targeted mask or channel).
  allocCheckpoint("pasting pixels")
  if s.clipboard.isNone: raise other("the clipboard is empty")
  let clip = s.clipboard.get
  let d = s.active
  if d.isNil:
    # Nothing open to paste into: the clipboard becomes a document of its own.
    return newFromClipboard(s)
  if targetOf(p).kind != tgPixels: return pasteToTarget(s, p, inPlace, none(Surface), "Paste")
  # A targeted colour channel takes the paste instead of a new layer.
  let k = pasteColorTarget(s, p)
  if k.isSome: return channelPaste(s, p, k.get, inPlace, none(Surface), "Paste")
  let canvas = d.doc[].bounds
  let fmt = d.doc[].pixelFormat
  let (dx, dy) = pasteOffset(clip, canvas, p, inPlace)
  if clip.layers.isSome and layered(d.doc.mode): return pasteLayers(s, clip.layers.get[], dx, dy)
  let moved = shifted(clip.surface, dx, dy)
  let target = PixelFormat(mode: fmt.mode, sample: fmt.sample, alpha: true)
  let surf = if moved.format == target: moved else: moved.convert(target)
  let before = d.doc[]
  let id = s.editGet("Paste", LayerId):
    var l = rasterLayer(doc.nextLayerName("Layer"), target)
    l.pixelsMut[] = surf
    let id = doc.insertAbove(active, l)
    active = some(id)
    doc.selection = none(Surface)
    id
  # Only where the pasted pixels land needs recompositing.
  noteInsert(s, before)
  %*{"layer": id, "offset": [dx, dy]}

proc layerVia(s: var Session; cut: bool): JsonNode =
  let d = s.activeOrRaise()
  let id = activeId(s)
  let hasSel = d.doc.selection.isSome
  if not hasSel and not cut and multi(s, newJNull()):
    # No selection, several layers selected: copy every one of them.
    return duplicateSelected(s, true, "Layer Via Copy")
  if not hasSel and not cut:
    # No selection: Layer via Copy duplicates the whole layer (Photoshop).
    let nid = s.editGet("Layer Via Copy", LayerId):
      let src = doc.findLayer(id)
      if src.isNil: raise noLayer(id)
      var dup = src[].duplicate
      dup.name = doc.copyName(src.name)
      dup.locks = Locks()
      let nid = doc.insertAbove(some(id), dup)
      active = some(nid)
      nid
    return %*{"layer": nid}
  let l = d.doc[].findLayer(id)
  if l.isNil or not l[].hasSurface: raise other("the active layer has no pixels")
  let clip = lift(l[].surface, d.doc.selection, d.doc[].bounds)
  if clip.bounds.isEmpty: raise other("Could not complete the command: the selected area is empty")
  let bg = s.tools.background
  let label = if cut: "Layer Via Cut" else: "Layer Via Copy"
  let nid = s.editGet(label, LayerId):
    if cut: clearSelected(doc, id, bg)
    var nl = rasterLayer(doc.nextLayerName("Layer"), clip.surface.format)
    nl.pixelsMut[] = clip.surface
    let nid = doc.insertAbove(some(id), nl)
    active = some(nid)
    # Photoshop deselects: a following Free Transform moves the whole new layer.
    doc.selection = none(Surface)
    nid
  %*{"layer": nid}

proc mergeVisible(s: var Session): JsonNode =
  s.editDo("Merge Visible"):
    var visible: seq[int]
    for i, l in doc.layers:
      if l.visible: visible.add i
    var anyGroup = false
    for i in visible:
      if doc.layers[i].isGroup: anyGroup = true
    if visible.len < 2 and not anyGroup: raise other("there are not enough visible layers to merge")
    let lowest = visible[0]
    let base = doc.layers[lowest]
    let isBg = base.name == "Background" and base.locks.transparency
    var solo = doc
    solo.layers.keepItIf(it.visible)
    let f0 = doc.pixelFormat
    let fmt = if isBg: f0 else: PixelFormat(mode: f0.mode, sample: f0.sample, alpha: true)
    var merged = rasterLayer(base.name, fmt)
    merged.locks = base.locks
    merged.pixelsMut[] = compositeLayers(solo, fmt, if isBg: some([1'f32, 1, 1]) else: none(array[3, float32]))
    let mid = merged.id
    var outp: seq[Layer]
    for i, l in doc.layers:
      if i == lowest: outp.add merged
      elif not l.visible: outp.add l
    doc.layers = outp
    active = some(mid)
  jnull()

proc histograms(surf: Surface; area: Rect): array[3, array[256, uint64]] =
  ## Histogram of a pixel layer (8-bit bins per channel) inside the selection.
  let step = int(max((uint64(area.width) * uint64(area.height)) div 4_000_000'u64, 1'u64))
  let st = max(int(sqrt(float64(step))), 1)
  var s2 = st
  while s2 * s2 > step: dec s2
  while (s2 + 1) * (s2 + 1) <= step: inc s2
  let stride = max(s2, 1)
  var row = newSeq[array[4, uint8]](area.width)
  var y = area.y0
  while y < area.y1:
    surf.readRgba8Into(rect(area.x0, y, area.x1, y + 1), row)
    var i = 0
    while i < row.len:
      let p = row[i]
      if p[3] != 0:
        for c in 0 ..< 3: inc result[c][p[c]]
      i += stride
    y += stride

proc clipPoints(h: array[256, uint64]; clip: float64): (float32, float32) =
  ## Black and white points with `clip` (fraction) clipped at each end.
  var total = 0'u64
  for v in h: total += v
  if total == 0: return (0'f32, 1'f32)
  let lim = uint64(float64(total) * clip)
  var acc = 0'u64
  var lo = 0
  while lo < 254 and acc + h[lo] <= lim:
    acc += h[lo]
    inc lo
  acc = 0
  var hi = 255
  while hi > lo + 1 and acc + h[hi] <= lim:
    acc += h[hi]
    dec hi
  (float32(lo) / 255'f32, float32(hi) / 255'f32)

proc histMedian(h: array[256, uint64]): float32 =
  var total = 0'u64
  for v in h: total += v
  var acc = 0'u64
  for i, v in h:
    acc += v
    if acc * 2 >= total: return float32(i) / 255'f32
  0.5'f32

proc autoAdjust(s: var Session; kind: string): JsonNode =
  ## Image › Auto Tone / Auto Contrast / Auto Color (0.1% clipping).
  let id = activeId(s)
  let d = s.activeOrRaise()
  let l = d.doc[].findLayer(id)
  if l.isNil or not l[].hasSurface: raise other("the active layer has no pixels")
  let area = (if d.doc.selection.isSome: d.doc.selection.get.contentBounds else: d.doc[].bounds).intersect(d.doc[].bounds)
  let h = histograms(l[].surface, area)
  let clip = 0.001
  var per = [defaultLevelsChannel(), defaultLevelsChannel(), defaultLevelsChannel()]
  case kind
  of "contrast":
    # Same points for all channels (preserves colour), from the combined histogram.
    var all: array[256, uint64]
    for c in 0 ..< 3:
      for i in 0 ..< 256: all[i] += h[c][i]
    let (lo, hi) = clipPoints(all, clip)
    for p in per.mitems:
      p.inBlack = lo
      p.inWhite = hi
  else:
    for c in 0 ..< 3:
      let (lo, hi) = clipPoints(h[c], clip)
      per[c].inBlack = lo
      per[c].inWhite = hi
    if kind == "color":
      # Neutralise midtones: move each channel's median onto the average median.
      var meds: array[3, float32]
      for c in 0 ..< 3: meds[c] = histMedian(h[c])
      let target = (-0'f32 + meds[0] + meds[1] + meds[2]) / 3'f32
      for c in 0 ..< 3:
        let p = per[c]
        let t = clamp((meds[c] - p.inBlack) / rmax(p.inWhite - p.inBlack, 1e-3'f32), 0.02'f32, 0.98'f32)
        let want = clamp((target - p.inBlack) / rmax(p.inWhite - p.inBlack, 1e-3'f32), 0.02'f32, 0.98'f32)
        per[c].gamma = clamp(ln(t) / ln(want), 0.1'f32, 9.99'f32)
  let adj = Adjustment(kind: akLevels, levelsMaster: defaultLevelsChannel(), levelsPerChannel: per, space: tsRgb,
                       levelsBlack: defaultLevelsChannel())
  let label = case kind
    of "contrast": "Auto Contrast"
    of "color": "Auto Color"
    else: "Auto Tone"
  s.editDo(label):
    let sel = doc.selection
    let mode = doc.mode
    let surf = paintSurface(doc, id, newJNull())
    adjustSurface(surf[], adj, sel, mode)
  jnull()

proc toggleLastState(s: var Session): JsonNode =
  let st = s.activeOrRaise()
  let redo = st.history.canRedo
  %(if redo: s.redo() else: s.undo())

proc transformBounds*(doc: Document; l: Layer): Rect

proc transformAgainParams(s: Session): JsonNode =
  ## The `edit.transform` params that repeat the last transform on the active layer.
  var last: JsonNode
  for i in countdown(s.journal.len - 1, 0):
    if s.journal[i][0] == "edit.transform":
      last = s.journal[i][1]
      break
  if last.isNil: raise other("there is no transform to repeat")
  let rect = last.pget("rect")
  let quad = last.pget("quad")
  let interp = last.pget("interpolation")
  result = %*{"interpolation": (if interp.isNil: %"bicubic" else: interp)}
  if not rect.isNil and not quad.isNil:
    proc nums(x: JsonNode): seq[float64] =
      if x.isArray:
        for v in x.elems:
          let f = v.asF64
          if f.isSome: result.add f.get
    let r = nums(rect)
    var q: seq[seq[float64]]
    if quad.isArray:
      for row in quad.elems: q.add nums(row)
    var okq = q.len == 4
    for row in q:
      if row.len < 2: okq = false
    if r.len != 4 or not okq: raise other("the last transform can't be repeated")
    let h = rectToQuad([r[0], r[1], r[2], r[3]], [[q[0][0], q[0][1]], [q[1][0], q[1][1]], [q[2][0], q[2][1]], [q[3][0], q[3][1]]])
    if h.isNone: raise other("the last transform can't be repeated")
    let d = s.activeOrRaise()
    let l = d.doc[].findLayer(activeId(s))
    if l.isNil: raise other("no active layer")
    let b = transformBounds(d.doc[], l[])
    if b.isEmpty: raise other("nothing to transform")
    let nr = [float64(b.x0), float64(b.y0), float64(b.x1), float64(b.y1)]
    var corners = newJArray()
    for (x, y) in [(nr[0], nr[1]), (nr[2], nr[1]), (nr[2], nr[3]), (nr[0], nr[3])]:
      let (a, bb) = h.get.apply(x, y)
      corners.add %*[a, bb]
    result["rect"] = %nr
    result["quad"] = corners
  elif not last.pget("matrix").isNil:
    result["matrix"] = last.pget("matrix")
  else: raise other("the last transform can't be repeated")

proc transformAgain(s: var Session): JsonNode =
  let p = transformAgainParams(s)
  s.execute("edit.transform", p)

proc transformAgainCopy(s: var Session): JsonNode =
  ## Transform Again on a copy: duplicates the active layer and repeats the last transform on
  ## the copy, as one history step.
  let p = transformAgainParams(s)
  let id = activeId(s)
  let journal = s.journal.len
  let dup = if multi(s, newJNull()): %*{"layer": id} else: newJObject()
  discard s.execute("layer.duplicate", dup)
  try:
    result = s.execute("edit.transform", p)
    let st = s.activeOrRaise()
    discard st.history.purgeLast()
    st.history.setCurrentLabel("Transform Again")
  except CatchableError:
    # The copy couldn't be transformed: take it back, leaving nothing to redo.
    discard s.undo()
    s.journal.setLen(min(journal, s.journal.len))
    let st = s.active
    if not st.isNil: st.history.clearRedo()
    raise

const MaxGuidePx = 1_000_000.0

proc pointSizePerInch*(s: Session): float64

proc guidePosition(s: Session; p: JsonNode; vertical: bool): Option[float64] =
  ## A guide `position` in document pixels: a number, or text with a unit ("50%", "2.5 cm").
  let v = p.pget("position")
  if v.isNil: return none(float64)
  var px: float64
  let n = v.asF64
  if n.isSome: px = n.get
  elif v.kind == JString:
    let parsed = parseLength(v.str)
    if parsed.isNone:
      raise other("`position` " & escapeJson(v.str) & " is not a length: give a number of pixels, or a number with px, in, cm, mm, pt, pc or %")
    let doc = s.activeOrRaise().doc
    let extent = if vertical: doc.size.width else: doc.size.height
    let unit = if parsed.get[1].isSome: parsed.get[1].get else: unPixels
    px = unit.toPx(parsed.get[0], float64(doc.resolutionDpi), float64(extent), pointSizePerInch(s))
  else: raise other("`position` must be a number of pixels or text with a unit")
  if px != px or abs(px) == Inf or abs(px) > MaxGuidePx:
    raise other("`position` is out of range (at most 1000000 px from the canvas origin)")
  some(px)

const GuideColors* = [("lightBlue", [0x33'u8, 0x66, 0xff]), ("lightRed", [0xff'u8, 0x33, 0x33]),
  ("green", [0x33'u8, 0xff, 0x33]), ("mediumBlue", [0x33'u8, 0x33, 0xff]), ("yellow", [0xff'u8, 0xcc, 0x00]),
  ("magenta", [0xff'u8, 0x00, 0xff]), ("cyan", [0x4a'u8, 0xff, 0xff]), ("lightGray", [0xe7'u8, 0xe7, 0xe7]),
  ("black", [0x00'u8, 0x00, 0x00])]

proc guideColor(p: JsonNode): Option[array[3, uint8]] =
  ## A guide `color`: one of `GuideColors`' names or "#rrggbb".
  let v = p.pget("color")
  if v.isNil: return
  if v.kind != JString: raise other("`color` must be a colour name or \"#rrggbb\"")
  for (name, rgbv) in GuideColors:
    if cmpIgnoreCase(name, v.str) == 0: return some(rgbv)
  let h = prefsParseHex(v.str)
  if h.isSome: return h
  var names: seq[string]
  for (n, _) in GuideColors: names.add n
  raise other("`color` " & escapeJson(v.str) & " is not a guide colour: use \"#rrggbb\" or one of " & names.join(", "))

proc guideCmd(s: var Session; p: JsonNode; op: string): JsonNode =
  ## View › New Guide / guide moves (undoable).
  let vertical = p.getStrOr("orientation", "horizontal") == "vertical"
  let pos = guidePosition(s, p, vertical)
  let color = if op == "new": guideColor(p) else: none(array[3, uint8])
  let iv = p.pget("index").asU64
  let index = if iv.isSome: some(int(min(iv.get, uint64(high(int))))) else: none(int)
  let label = case op
    of "new": "New Guide"
    of "move": "Move Guide"
    of "delete": "Delete Guide"
    else: "Clear Guides"
  s.editDo(label):
    let gp = if vertical: addr doc.guides.vertical else: addr doc.guides.horizontal
    template g: untyped = gp[]
    case op
    of "new":
      if pos.isNone: raise other("missing `position`")
      let at = pos.get
      g.add at
      if color.isSome: doc.guides.setGuideColor(vertical, at, color)
    of "move":
      if index.isNone or index.get >= g.len: raise other("no such guide")
      if pos.isNone: raise other("missing `position`")
      let to = pos.get
      let frm = g[index.get]
      g[index.get] = to
      # The guide takes its colour along.
      let rgbv = doc.guides.guideColor(vertical, frm)
      if rgbv.isSome: doc.guides.setGuideColor(vertical, to, rgbv)
      doc.guides.pruneColors()
    of "delete":
      if index.isNone or index.get >= g.len: raise other("no such guide")
      g.delete(index.get)
      doc.guides.pruneColors()
    else:
      doc.guides.horizontal.setLen(0)
      doc.guides.vertical.setLen(0)
      doc.guides.colors.setLen(0)
  jnull()

proc editSpecs*(): seq[CommandSpec] =
  @[
    cmd("edit.cut", "Cut", ["Edit"], "Cmd+X", "{} (a targeted colour channel cuts that channel only)", hasPixelsE, runProc do:
      # Refuse a locked layer before copying, so a refused Cut leaves the clipboard alone.
      let id = activeId(s)
      checkPixelsUnlocked(s.activeOrRaise().doc[], id)
      let k = pasteColorTarget(s, p)
      if k.isSome: return channelCut(s, id, k.get)
      let r = copyCmd(s, false)
      let bg = s.tools.background
      s.editDo("Cut Pixels"): clearSelected(doc, id, bg)
      r),
    cmd("edit.copy", "Copy", ["Edit"], "Cmd+C",
        """{"target":"mask"? = the targeted mask, copied as grayscale (#2928)} (a targeted colour or alpha channel copies that channel, as grayscale)""",
        canCopyE, runProc do:
      if isMaskTarget(p): return copyMask(s)
      let r = channelCopy(s)
      if r.isSome: r.get else: copyCmd(s, false)),
    cmd("edit.copyMerged", "Copy Merged", ["Edit"], "Cmd+Shift+C", "{}", hasDocE, runProc(copyCmd(s, true))),
    cmd("edit.paste", "Paste", ["Edit"], "Cmd+V",
        "{\"center\":[x,y]? (view centre; default keeps the position when it overlaps the canvas)," & PasteTarget & "} (with no document open: a new document from the clipboard)",
        hasClipOnlyE, runProc(pasteCmd(s, p, false))),
    cmd("file.newFromClipboard", "New from Clipboard", ["File"], "",
        "{} (a new document the size of the clipboard image, holding it as one layer)", hasClipOnlyE, runProc(newFromClipboard(s))),
    cmd("edit.pasteSpecial.pasteInPlace", "Paste in Place", ["Edit", "Paste Special"], "Cmd+Shift+V", "{" & PasteTarget & "}",
        hasClipE, runProc(pasteCmd(s, p, true))),
    cmd("layer.new.layerViaCopy", "Layer via Copy", ["Layer", "New"], "Cmd+J", "{}", hasDocE, runProc(layerVia(s, false))),
    cmd("layer.new.layerViaCut", "Layer via Cut", ["Layer", "New"], "Cmd+Shift+J", "{}", hasPixelsE, runProc(layerVia(s, true))),
    cmd("layer.mergeVisible", "Merge Visible", ["Layer"], "Cmd+Shift+E", "{}", hasDocE, runProc(mergeVisible(s))),
    cmd("image.autoTone", "Auto Tone", ["Image"], "Cmd+Shift+L", "{}", hasPixelsE, runProc(autoAdjust(s, "tone"))),
    cmd("image.autoContrast", "Auto Contrast", ["Image"], "Cmd+Alt+Shift+L", "{}", hasPixelsE, runProc(autoAdjust(s, "contrast"))),
    cmd("image.autoColor", "Auto Color", ["Image"], "Cmd+Shift+B", "{}", hasPixelsE, runProc(autoAdjust(s, "color"))),
    cmd("edit.toggleLastState", "Toggle Last State", ["Edit"], "Cmd+Alt+Z", "{}", hasDocE, runProc(toggleLastState(s))),
    cmd("edit.transform.again", "Again", ["Edit", "Transform"], "Cmd+Shift+T", "{}", hasDocE, runProc(transformAgain(s))),
    cmd("edit.transform.againCopy", "Transform Again on a Copy", [], "Cmd+Alt+Shift+T",
        "{} (duplicates the active layer and repeats the last transform on the copy: step and repeat)", hasDocE,
        runProc(transformAgainCopy(s))),
    cmd("view.newGuide", "New Guide…", ["View"], "",
        """{"orientation":"horizontal|vertical","position":px or "<number><unit>","color":"lightBlue|lightRed|green|mediumBlue|yellow|magenta|cyan|lightGray|black" or "#rrggbb"?} (units px, in, cm, mm, pt, pc or %; % is of the width for a vertical guide, the height for a horizontal one; without a color the guide uses the guide colour in Preferences)""",
        hasDocE, runProc(guideCmd(s, p, "new"))),
    cmd("view.moveGuide", "Move Guide", [], "",
        """{"orientation":"horizontal|vertical","index":n,"position":px or "<number><unit>"} (units as for view.newGuide)""",
        hasDocE, runProc(guideCmd(s, p, "move"))),
    cmd("view.deleteGuide", "Delete Guide", [], "", """{"orientation":"horizontal|vertical","index":n}""", hasDocE,
        runProc(guideCmd(s, p, "delete"))),
    cmd("view.clearGuides", "Clear Guides", ["View"], "", "{}", hasDocE, runProc(guideCmd(s, p, "clear"))),
  ]
