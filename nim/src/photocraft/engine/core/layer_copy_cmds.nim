## Copy layers into another open document (port of `layer_copy_cmds.rs`): what dragging layers
## onto another document's tab does. Part of the `engine/core` unit: included, not imported.

const LayerCopyCmd = "layer.copyToDocument"

proc lcBad(msg: string): ref EngineError = bad(LayerCopyCmd, msg)

proc layerCopyEnabledE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if d.activeLayer.isNone: return "no active layer"
  if s.documents.len < 2: "no other document is open" else: ""

proc lcDocIndex(s: Session; p: JsonNode; key: string): Option[int] =
  let v = p.pget(key)
  if v.isNil: return
  let i = v.asU64
  if i.isNone: raise lcBad("`" & key & "` must be a document index")
  if i.get >= uint64(s.documents.len): raise lcBad("no document " & $i.get & " (there are " & $s.documents.len & ")")
  some(int(i.get))

proc lcPoint(p: JsonNode; key: string): Option[array[2, float64]] =
  let v = p.pget(key)
  if v.isNil: return
  if v.isArray and v.len == 2:
    let a = v.elems[0].asF64
    let b = v.elems[1].asF64
    if a.isSome and b.isSome:
      let xy = [a.get, b.get]
      var ok = true
      for c in xy:
        if c != c or abs(c) == Inf or abs(c) > 1e7: ok = false
      if ok: return some(xy)
  raise lcBad("`" & key & "` must be [x, y] (pixels)")

proc layered*(mode: ColorMode): bool =
  ## Modes whose layers convert through the colour engine.
  mode in {cmRgb, cmGrayscale, cmCmyk, cmLab}

proc rectCenter(r: Rect): array[2, float64] =
  [(float64(r.x0) + float64(r.x1)) / 2.0, (float64(r.y0) + float64(r.y1)) / 2.0]

proc adaptCopies*(s: Session; copies: var Document; dest: Document) =
  ## Readies `copies` for landing in `dest`: link groups and smart-object contents ids are
  ## remapped, and the pixels convert to `dest`'s profile and depth.
  var used = initHashSet[uint64]()
  for (_, l) in dest.walkPtrs:
    if l.linkGroup.isSome: used.incl l.linkGroup.get
  let ng = dest.unusedLinkGroup
  if ng.isNone: raise other("no free linked-layer group id")
  var nextGroup = ng.get
  var groups: seq[(LayerId, uint64)]
  for (_, l) in copies.walkPtrs:
    if l.linkGroup.isSome: groups.add (l.id, l.linkGroup.get)
  var remapped = initTable[uint64, uint64]()
  for (_, group) in groups:
    if group in remapped: continue
    while nextGroup in used: nextGroup = if nextGroup == high(uint64): 1'u64 else: nextGroup + 1
    remapped[group] = nextGroup
    used.incl nextGroup
    nextGroup = if nextGroup == high(uint64): 1'u64 else: nextGroup + 1
  for (id, group) in groups:
    let l = copies.layerPtr(id)
    if not l.isNil: l.linkGroup = some(remapped[group])
  var contents = initTable[uint64, SmartContentsId]()
  var smartIds: seq[LayerId]
  for (_, l) in copies.walkPtrs:
    if l.content.kind == lkSmart: smartIds.add l.id
  for id in smartIds:
    let l = copies.layerPtr(id)
    if not l.isNil and l.content.kind == lkSmart:
      let old = uint64(l.content.smart.contentsId)
      if old notin contents: contents[old] = freshContentsId()
      l.content.smart.contentsId = contents[old]
  if copies.mode != dest.mode or copies.iccProfile != dest.iccProfile:
    let profile = documentProfile(dest)
    convertDocument(copies, profile, colorSettingsIntent(s), s.color.settings.bpc)
  if copies.depth != dest.depth:
    let depth = dest.depth
    forEachSurface(copies.layers, true, proc(surf: var Surface; isMask: bool) =
      surf = surf.convert(surf.format.withSample(depth)))

proc translateCopies*(copies: var Document; dx, dy: int) =
  ## Moves every layer of `copies` by whole pixels (content, masks, vectors, smart placement).
  if dx != 0 or dy != 0:
    let snapshot = copies
    for l in copies.layers.mitems:
      translateLayer(snapshot, l, dx, dy)
      translateVectors(snapshot, l, float64(dx), float64(dy))

proc copyToDocument(s: var Session; p: JsonNode): JsonNode =
  let destO = lcDocIndex(s, p, "document")
  if destO.isNone: raise lcBad("missing `document` (the destination's index)")
  let dest = destO.get
  let srcO = lcDocIndex(s, p, "source")
  let src = if srcO.isSome: srcO.get elif s.activeIndex.isSome: s.activeIndex.get else: raise noDocument()
  if src == dest: raise lcBad("the destination is the source document (use layer.duplicate)")
  let fromSt = s.docAt(src)
  let sdoc = fromSt.doc[]
  let ddoc = s.docAt(dest).doc[]
  var ids: seq[LayerId]
  let lv = p.pget("layers")
  if lv.isNil: ids = fromSt[].selectedOrdered
  else:
    var ok = lv.isArray
    if ok:
      for x in lv.elems:
        let u = x.asU64
        if u.isNone:
          ok = false
          break
        ids.add LayerId(u.get)
    if not ok: raise lcBad("`layers` must be an array of layer ids")
  for id in ids:
    if sdoc.findLayer(id).isNil: raise noLayer(id)
  ids = topLevel(sdoc, ids)
  if ids.len == 0: raise lcBad("no layers to copy")
  for (doc, role) in [(unsafeAddr sdoc, "source"), (unsafeAddr ddoc, "destination")]:
    if not layered(doc.mode): raise other("layers can't be copied with a " & $doc.mode & " " & role & " document")
  var copies = newDocument("", sdoc.size, sdoc.mode, sdoc.depth)
  copies.iccProfile = sdoc.iccProfile
  copies.resolutionDpi = sdoc.resolutionDpi
  for id in ids:
    let srcLayer = sdoc.findLayer(id)
    if srcLayer.isNil: raise noLayer(id)
    var l = srcLayer[].duplicate
    # A dragged Background arrives as an ordinary layer.
    if isBackground(srcLayer[]) and sdoc.layers.len > 0 and sdoc.layers[0].id == id:
      l.name = ddoc.nextLayerName("Layer")
      l.locks = Locks()
    copies.layers.add l
  let nm = p.pget("name").asStr
  if nm.isSome and nm.get.len > 0 and copies.layers.len == 1: copies.layers[0].name = nm.get
  adaptCopies(s, copies, ddoc)
  var bounds = EmptyRect
  for l in copies.layers:
    let b = layerBounds(l)
    if b.isSome: bounds = bounds.union(b.get)
  let content = if bounds.isEmpty: sdoc.bounds else: bounds
  var dxf, dyf: float64
  if p.getBoolOr("center", false):
    let c = rectCenter(content)
    let d = rectCenter(ddoc.bounds)
    (dxf, dyf) = (d[0] - c[0], d[1] - c[1])
  else:
    let at = lcPoint(p, "at")
    if at.isSome:
      let c = rectCenter(content)
      (dxf, dyf) = (at.get[0] - c[0], at.get[1] - c[1])
    else:
      let o = lcPoint(p, "offset")
      if o.isSome: (dxf, dyf) = (o.get[0], o.get[1])
  let dx = asI32(round(dxf))
  let dy = asI32(round(dyf))
  translateCopies(copies, dx, dy)
  var patterns: seq[DocPattern]
  for pat in sdoc.patterns:
    var have = false
    for q in ddoc.patterns:
      if q.id == pat.id: have = true
    if not have: patterns.add pat
  discard s.setActive(dest)
  let label = if ids.len == 1: "Duplicate Layer" else: "Duplicate Layers"
  var newIds: seq[LayerId]
  try:
    newIds = s.editGet(label, seq[LayerId]):
      doc.patterns.add patterns
      var above = active
      var made: seq[LayerId]
      for l in copies.layers:
        let id = doc.insertAbove(above, l)
        above = some(id)
        made.add id
      active = above
      made
  except CatchableError:
    # Nothing changed: the source stays the active document.
    discard s.setActive(src)
    raise
  let last = if newIds.len > 0: some(newIds[^1]) else: none(LayerId)
  reselect(s, newIds, last)
  %*{"document": dest, "layers": idsJson(newIds), "offset": [dx, dy]}

proc layerCopySpecs*(): seq[CommandSpec] =
  @[cmd(LayerCopyCmd, "Copy Layers to Document", [], "",
        """{"document":index (destination),"layers":[id,…]?=the source's selected layers,"source":index?=active,"center":bool=false (centre on the destination's canvas),"at":[x,y]? (centre the copies on this point),"offset":[dx,dy]?=[0,0] (from their place in the source),"name":str? (one layer: the copy's name)}""",
        layerCopyEnabledE, copyToDocument)]
