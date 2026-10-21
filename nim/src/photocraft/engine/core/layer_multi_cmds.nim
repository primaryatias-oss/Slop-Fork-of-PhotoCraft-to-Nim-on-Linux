## Multi-layer selection and the commands that act on it: ⌘/⇧-click selection, Select › All /
## Deselect / Find Layers, Layer › Align and Distribute, Link Layers, Merge Layers, Group from
## Layers, Arrange › Reverse, Lock Layers and Rename Layer; plus the damage bookkeeping
## (`samePixels`, `stepDamage`) the session uses after every edit (port of `layer_multi_cmds.rs`).
## Part of the `engine/core` unit: included, not imported.

# ---------------------------------------------------------------- tree walking

iterator walkPtrs*(d: Document): (LayerPath, ptr Layer) =
  ## `Document::walk` without copies: depth-first, bottom to top, a group before its children.
  var stack: seq[(ptr seq[Layer], int, LayerPath)] = @[(unsafeAddr d.layers, 0, newSeq[int]())]
  while stack.len > 0:
    let (ls, i, prefix) = stack[^1]
    if i >= ls[].len:
      discard stack.pop()
      continue
    stack[^1][1] = i + 1
    let path = prefix & i
    let lp = unsafeAddr ls[][i]
    yield (path, lp)
    if lp[].content.kind == lkGroup:
      stack.add (unsafeAddr lp[].content.group.children, 0, path)

proc startsWith(p, q: LayerPath): bool =
  if q.len > p.len: return false
  for i in 0 ..< q.len:
    if p[i] != q[i]: return false
  true

# ---------------------------------------------------------------- selection state

proc selected*(s: Session): seq[LayerId] =
  ## Selected layers of the active document, bottom to top.
  let st = s.active
  if st.isNil: @[] else: st[].selectedLayers

proc targets*(s: Session; p: JsonNode): seq[LayerId] =
  ## The layers a layer command acts on: `layer`, else every selected layer, else the active one.
  if not p.pget("layer").isNil: return @[layerParam(s, p)]
  let sel = selected(s)
  if sel.len == 0: @[layerParam(s, p)] else: sel

proc maskTargets*(s: Session; p: JsonNode): seq[LayerId] =
  ## `targets` for a command that adds a layer mask: with several layers selected, those that
  ## can take one.
  let ids = targets(s, p)
  if ids.len < 2: return ids
  let d = s.activeOrRaise()
  for id in ids:
    let l = d.doc[].findLayer(id)
    if not l.isNil and l.mask.isNone and not isBackground(l[]): result.add id
  if result.len == 0: raise other("none of the selected layers can take a layer mask")

proc topLevel*(doc: Document; ids: openArray[LayerId]): seq[LayerId] =
  ## Ids in `ids` with no ancestor also in `ids`, bottom to top.
  var paths: seq[(LayerPath, LayerId)]
  for (p, l) in doc.walkPtrs:
    if l.id in ids: paths.add (p, l.id)
  for (p, id) in paths:
    var nested = false
    for (q, _) in paths:
      if q.len < p.len and p.startsWith(q):
        nested = true
        break
    if not nested: result.add id

proc setSelection*(s: var Session; ids: seq[LayerId]; active, anchor: Option[LayerId]) =
  ## Replaces the layer selection without a history step.
  let st = s.activeOrRaise()
  for id in ids:
    if st.doc[].findLayer(id).isNil: raise noLayer(id)
  if active.isSome and st.doc[].findLayer(active.get).isNil: raise noLayer(active.get)
  let act = if active.isSome: active elif ids.len > 0: some(ids[^1]) else: none(LayerId)
  st.selectedLayers = ids
  if act.isSome and act.get notin st.selectedLayers: st.selectedLayers.add act.get
  st.activeLayer = act
  st.layerAnchor = if anchor.isSome: anchor else: act
  fixSelection(st[])
  let clean = st.savedRevision == st.revision
  inc st.revision
  if clean: st.savedRevision = st.revision
  st.lastDamage = some(EmptyRect)

proc reselect*(s: var Session; ids: seq[LayerId]; active: Option[LayerId]) =
  ## After a structural edit, makes `ids` the selection (dropping any that no longer exist).
  let st = s.active
  if st.isNil: return
  st.selectedLayers = ids
  if active.isSome:
    st.activeLayer = active
    st.layerAnchor = active
  fixSelection(st[])
  st.history.setCurrentLayers(st[].layerTarget)

proc rangeOrder(layers: seq[Layer]; includeCollapsed: bool; outp: var seq[LayerId]) =
  ## Layer rows in the Layers panel's visual order.
  for i in countdown(layers.len - 1, 0):
    outp.add layers[i].id
    if layers[i].content.kind == lkGroup and (layers[i].content.group.expanded or includeCollapsed):
      rangeOrder(layers[i].content.group.children, includeCollapsed, outp)

proc selectLayerCmd*(s: var Session; p: JsonNode): JsonNode =
  ## `layer.select`: `{"layer": id, "mode": "replace|toggle|range|add"}`.
  let v = p.pget("layer").asU64
  if v.isNone: raise bad("layer.select", "missing `layer`")
  let id = LayerId(v.get)
  let mode = p.getStrOr("mode", "replace")
  let st = s.activeOrRaise()
  if st.doc[].findLayer(id).isNil: raise noLayer(id)
  var sel = st[].selectedLayers
  case mode
  of "replace": s.selectLayer(id)
  of "add":
    if id notin sel: sel.add id
    setSelection(s, sel, some(id), some(id))
  of "toggle":
    if id in sel:
      if sel.len == 1:
        # Photoshop keeps at least the clicked layer targeted.
        return %*{"selected": [uint64(id)]}
      sel.keepItIf(it != id)
      let act = if st.activeLayer == some(id): (if sel.len > 0: some(sel[^1]) else: none(LayerId)) else: st.activeLayer
      setSelection(s, sel, act, act)
    else:
      sel.add id
      setSelection(s, sel, some(id), some(id))
  of "range":
    var anchor = id
    if st.layerAnchor.isSome and not st.doc[].findLayer(st.layerAnchor.get).isNil: anchor = st.layerAnchor.get
    elif st.activeLayer.isSome: anchor = st.activeLayer.get
    var order: seq[LayerId]
    rangeOrder(st.doc.layers, false, order)
    if anchor notin order or id notin order:
      order.setLen(0)
      rangeOrder(st.doc.layers, true, order)
    let a = max(order.find(anchor), 0)
    let b = max(order.find(id), 0)
    setSelection(s, order[min(a, b) .. max(a, b)], some(id), some(anchor))
  else: raise bad("layer.select", "unknown mode `" & mode & "`")
  %*{"selected": idsJson(selected(s))}

proc isLockedBackground*(l: Layer): bool =
  l.name == "Background" and l.locks.transparency and l.content.kind == lkRaster

proc selectAllLayers(s: var Session): JsonNode =
  let d = s.activeOrRaise()
  var ids: seq[LayerId]
  for (_, l) in d.doc[].walkPtrs:
    if not isLockedBackground(l[]): ids.add l.id
  if ids.len == 0: raise other("there are no layers to select")
  let active = if d.activeLayer.isSome and d.activeLayer.get in ids: d.activeLayer else: some(ids[^1])
  let n = ids.len
  setSelection(s, ids, active, none(LayerId))
  %*{"selected": n}

proc findLayers(s: var Session; p: JsonNode): JsonNode =
  let nm = p.pget("name").asStr
  if nm.isNone: raise bad("select.findLayers", "missing `name`")
  let needle = unicode.toLower(nm.get)
  let d = s.activeOrRaise()
  var ids: seq[LayerId]
  for (_, l) in d.doc[].walkPtrs:
    if needle in unicode.toLower(l.name): ids.add l.id
  if ids.len == 0: raise other("no layer name contains \"" & needle & "\"")
  let outp = idsJson(ids)
  setSelection(s, ids, none(LayerId), none(LayerId))
  %*{"selected": outp}

proc linkGroups(doc: Document; ids: openArray[LayerId]): seq[uint64] =
  for id in ids:
    let l = doc.findLayer(id)
    if not l.isNil and l.linkGroup.isSome: result.add l.linkGroup.get
  result.sort()
  result = deduplicate(result, isSorted = true)

proc selectLinked(s: var Session): JsonNode =
  let d = s.activeOrRaise()
  let groups = linkGroups(d.doc[], d[].selectedLayers)
  var ids: seq[LayerId]
  for (_, l) in d.doc[].walkPtrs:
    if l.linkGroup.isSome and l.linkGroup.get in groups: ids.add l.id
  let active = if d.activeLayer.isSome and d.activeLayer.get in ids: d.activeLayer
               elif ids.len > 0: some(ids[^1]) else: none(LayerId)
  let n = ids.len
  setSelection(s, ids, active, active)
  %*{"selected": n}

# ---------------------------------------------------------------- enabled predicates

proc lmHasLayerE(s: Session): string = (if selected(s).len == 0: "no layer selected" else: "")
proc twoPlusE(s: Session): string = (if selected(s).len >= 2: "" else: "select two or more layers")
proc canAlignE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if d[].selectedLayers.len == 0: "no layer selected" else: ""
proc canDistributeE(s: Session): string =
  if selected(s).len >= 3: "" else: "select three or more layers to distribute"
proc canLinkE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  let sel = d[].selectedLayers
  if sel.len == 0: return "no layer selected"
  if sel.len == 1:
    let l = d.doc[].findLayer(sel[0])
    if l.isNil or l.linkGroup.isNone: return "select two or more layers to link"
  ""
proc hasLinkedE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if linkGroups(d.doc[], d[].selectedLayers).len == 0: "the selected layers are not linked" else: ""

# ---------------------------------------------------------------- moving layers

proc withLinks*(doc: Document; ids: openArray[LayerId]): seq[LayerId] =
  ## `ids` plus every layer linked to one of them.
  let groups = linkGroups(doc, ids)
  result = @ids
  if groups.len > 0:
    for (_, l) in doc.walkPtrs:
      if l.linkGroup.isSome and l.linkGroup.get in groups and l.id notin result: result.add l.id

proc moveLayers*(doc: var Document; moves: openArray[(LayerId, int, int)]) =
  ## Moves layers by whole pixels the way the Move tool does.
  let snapshot = doc
  for (id, dx, dy) in moves:
    if dx == 0 and dy == 0: continue
    let locks = doc.effectiveLocks(id)
    let l = doc.layerOrRaise(id)
    if locks.position or locks.all: raise other("layer \"" & l.name & "\" is position-locked")
    translateLayer(snapshot, l[], dx, dy)
    translateVectors(snapshot, l[], float64(dx), float64(dy))

proc moveTargets*(doc: Document; roots: openArray[LayerId]): seq[LayerId] =
  ## The layers a move of `roots` takes along: their linked layers, without nested ones.
  topLevel(doc, withLinks(doc, roots))

proc translateCmd*(s: var Session; p: JsonNode): JsonNode =
  ## `layer.translate`: the explicit layer, or every selected layer, plus their linked layers.
  let dx = int(intI32("layer.translate", p, "dx").get(0))
  let dy = int(intI32("layer.translate", p, "dy").get(0))
  if dx == 0 and dy == 0: return jnull()
  let lv = p.pget("layer").asU64
  let roots = if lv.isSome: @[LayerId(lv.get)] else: selected(s)
  if roots.len == 0: raise other("no active layer")
  let before = s.activeOrRaise().doc[]
  let ids = s.editGet("Move", seq[LayerId]):
    let ids = moveTargets(doc, roots)
    if ids.len == 0: raise noLayer(roots[0])
    var moves: seq[(LayerId, int, int)]
    for id in ids: moves.add (id, dx, dy)
    moveLayers(doc, moves)
    # A board moved past any canvas edge grows the canvas.
    var board = false
    for id in ids:
      let l = doc.findLayer(id)
      if not l.isNil and l[].artboard.isSome: board = true
    if board: fitCanvas(doc)
    ids
  noteDamage(s, before, ids)
  jnull()

proc shiftSurface*(s: Surface; dx, dy: int): Surface =
  ## `translate_surface` (content scan per call; the Rust build caches it per tile).
  s.translated(dx, dy, s.contentBounds)

proc shiftShown(doc: Document; l: var Layer; dx, dy: int) =
  ## `translateLayer` plus the vector side, without re-rendering anything.
  let a = translateAffine(float64(dx), float64(dy))
  if l.effects.reference.isSome:
    let r = l.effects.reference.get
    l.effects.reference = some((r[0] + float64(dx), r[1] + float64(dy)))
  if l.mask.isSome and l.mask.get.linked:
    l.mask.get.surface = shiftSurface(l.mask.get.surface, dx, dy)
  if l.vectorMask.isSome and l.vectorMask.get.linked:
    l.vectorMask.get.path = l.vectorMask.get.path.transform(a)
  case l.content.kind
  of lkRaster: l.content.raster = shiftSurface(l.content.raster, dx, dy)
  of lkText:
    l.content.text.transform = a.mul(l.content.text.transform)
    if l.content.text.cache.isSome:
      l.content.text.cache = some(shiftSurface(l.content.text.cache.get, dx, dy))
  of lkShape:
    # The rendered shape is cut at the canvas: one that reaches past it renders again.
    let canvas = doc.bounds
    var inside = false
    if l.content.shape.cache.isSome:
      let b = l.content.shape.cache.get.contentBounds
      inside = b.isEmpty or (b.x0 > canvas.x0 and b.y0 > canvas.y0 and b.x1 < canvas.x1 and b.y1 < canvas.y1)
    transformShape(l.content.shape, a)
    if l.content.shape.cache.isSome and inside:
      l.content.shape.cache = some(shiftSurface(l.content.shape.cache.get, dx, dy))
    else: refreshShape(doc, l.content.shape)
  of lkSmart: shiftSmart(l.content.smart, dx, dy)
  of lkGroup:
    if l.content.group.artboard.isSome:
      l.content.group.artboard.get.rect = l.content.group.artboard.get.rect.translate(dx, dy)
    for c in l.content.group.children.mitems: shiftShown(doc, c, dx, dy)
  of lkAdjustment, lkFill: discard

proc moved*(doc: Document; ids: openArray[LayerId]; dx, dy: int): Document =
  ## `doc` with the layers `ids` moved by whole pixels (the Move tool's live preview).
  result = doc
  for id in ids:
    let locks = doc.effectiveLocks(id)
    let l = result.layerOrRaise(id)
    if locks.position or locks.all: raise other("layer \"" & l.name & "\" is position-locked")
    shiftShown(doc, l[], dx, dy)

# ---------------------------------------------------------------- damage

proc layersDamage*(before, after: Document; ids: openArray[LayerId]): Option[Rect] =
  ## The document pixels an edit of the layers `ids` can have changed, or none for anywhere.
  var outp = EmptyRect
  for d in [unsafeAddr before, unsafeAddr after]:
    let canvas = d[].bounds
    for id in ids:
      let l = d[].findLayer(id)
      if l.isNil: return none(Rect)
      let b = changeBounds(l[], canvas)
      if b.isNone: return none(Rect)
      if not b.get.isEmpty: outp = if outp.isEmpty: b.get else: outp.union(b.get)
  some(outp)

proc noteDamage*(s: var Session; before: Document; ids: openArray[LayerId]) =
  ## Reports `layersDamage` as the active document's last change.
  let st = s.active
  if st.isNil: return
  if before.size != st.doc.size: return
  st.lastDamage = layersDamage(before, st.doc[], ids)

proc surfacesEqual(a, b: Surface): bool =
  ## Identical pixels by copy-on-write tile identity.
  if a.format != b.format or a.defaultBytes != b.defaultBytes or a.tileCount != b.tileCount: return false
  for c, t in a.tiles:
    let u = b.tile(c)
    if u.isNil or cast[pointer](u) != cast[pointer](t): return false
  true

proc optSurfacesEqual(a, b: Option[Surface]): bool =
  if a.isNone or b.isNone: a.isNone and b.isNone else: surfacesEqual(a.get, b.get)

proc masksEqual(a, b: Option[LayerMask]): bool =
  if a.isNone or b.isNone: return a.isNone and b.isNone
  let x = a.get
  let y = b.get
  x.enabled == y.enabled and x.linked == y.linked and x.density == y.density and x.feather == y.feather and
    surfacesEqual(x.surface, y.surface)

proc textsEqual(a, b: TextLayer): bool =
  a.text == b.text and a.fontFamily == b.fontFamily and a.sizePt == b.sizePt and a.color == b.color and
    a.transform == b.transform and a.runs == b.runs and a.paragraphs == b.paragraphs and a.shape == b.shape and
    a.orientation == b.orientation and a.antialias == b.antialias and a.warp == b.warp and
    optSurfacesEqual(a.cache, b.cache) and a.psdRaw == b.psdRaw

proc smartsEqual(a, b: SmartObject): bool =
  if not (a.filtersEnabled == b.filtersEnabled and a.transform == b.transform and a.smartFilters == b.smartFilters and
          a.stackMode == b.stackMode and a.perspective == b.perspective and a.warp == b.warp): return false
  if a.source.kind != b.source.kind: return false
  case a.source.kind
  of ssEmbedded:
    if not (a.source.fileName == b.source.fileName and a.source.bytes == b.source.bytes): return false
  of ssLinked:
    if a.source.path != b.source.path: return false
  optSurfacesEqual(a.cache, b.cache) and a.psdRaw == b.psdRaw and masksEqual(a.filterMask, b.filterMask)

proc alphaChannelsEqual(a, b: AlphaChannel): bool =
  a.name == b.name and a.spot == b.spot and a.color == b.color and a.opacity == b.opacity and
    a.indicates == b.indicates and surfacesEqual(a.surface, b.surface)

proc shapesEqual(a, b: ShapeLayer): bool =
  a.path == b.path and a.fill == b.fill and a.stroke == b.stroke and a.live == b.live and a.cache == b.cache and
    a.psdRaw == b.psdRaw

proc layerPixelsEqual(a, b: Layer): bool =
  ## Whether two layers render to identical pixels in place (group children not compared).
  if not (a.id == b.id and a.visible == b.visible and a.blend == b.blend and a.opacity == b.opacity and
          a.fillOpacity == b.fillOpacity and a.clipped == b.clipped and masksEqual(a.mask, b.mask) and
          a.vectorMask == b.vectorMask and a.effects == b.effects and a.excludedChannels == b.excludedChannels and
          a.blendIf == b.blendIf and a.advanced == b.advanced): return false
  if a.content.kind != b.content.kind: return false
  let same = case a.content.kind
    of lkRaster: surfacesEqual(a.content.raster, b.content.raster)
    of lkGroup: a.content.group.artboard == b.content.group.artboard
    of lkFill: a.content.fill == b.content.fill
    of lkAdjustment: a.content.adjustment == b.content.adjustment
    of lkText: textsEqual(a.content.text, b.content.text)
    of lkShape: shapesEqual(a.content.shape, b.content.shape)
    of lkSmart: smartsEqual(a.content.smart, b.content.smart)
  if not same: return false
  if a.fillCache.isNone or b.fillCache.isNone: return a.fillCache.isNone and b.fillCache.isNone
  a.fillCache.get.fill == b.fillCache.get.fill and surfacesEqual(a.fillCache.get.surface, b.fillCache.get.surface)

proc docPixelsEqual(a, b: Document): bool =
  ## Document-level fields the compositor reads.
  if not (a.size == b.size and cast[uint32](a.resolutionDpi) == cast[uint32](b.resolutionDpi) and a.mode == b.mode and
          a.depth == b.depth and a.iccProfile == b.iccProfile and a.channels.len == b.channels.len): return false
  for i in 0 ..< a.channels.len:
    if not alphaChannelsEqual(a.channels[i], b.channels[i]): return false
  if a.globalLight != b.globalLight: return false
  if a.quickMask.isSome != b.quickMask.isSome: return false
  if a.quickMask.isSome and not alphaChannelsEqual(a.quickMask.get, b.quickMask.get): return false
  a.patterns == b.patterns and a.colorTable == b.colorTable and a.duotone == b.duotone

proc samePixelsWalk(xs, ys: seq[Layer]; depth: int): bool =
  if depth >= 256 or xs.len != ys.len: return false
  for i in 0 ..< xs.len:
    let x = unsafeAddr xs[i]
    let y = unsafeAddr ys[i]
    if x.video.isSome or y.video.isSome or not layerPixelsEqual(x[], y[]): return false
    let xg = x.content.kind == lkGroup
    let yg = y.content.kind == lkGroup
    if xg != yg: return false
    if xg and not samePixelsWalk(x.content.group.children, y.content.group.children, depth + 1): return false
  true

proc samePixels*(a, b: Document): bool =
  ## Whether `a` and `b` composite to the same pixels (they differ at most in fields the
  ## compositor never reads). Pixels compare by tile identity.
  docPixelsEqual(a, b) and samePixelsWalk(a.layers, b.layers, 0)

proc containedLayer(l: Layer; depth: int): bool =
  if depth >= 256 or l.video.isSome or l.excludedChannels != 0: return false
  if l.content.kind == lkGroup:
    for c in l.content.group.children:
      if not containedLayer(c, depth + 1): return false
  true

proc addedBounds(layers: seq[Layer]; k: int; canvas: Rect): Option[Rect] =
  ## The pixels adding `layers[k]` can change, when that is bounded.
  if k < 0 or k >= layers.len: return
  let l = unsafeAddr layers[k]
  let newBase = not l.clipped and k + 1 < layers.len and layers[k + 1].clipped
  if newBase or not containedLayer(l[], 0): return
  changeBounds(l[], canvas)

proc inertAt(layers: seq[Layer]; k: int): bool =
  ## Whether `layers[k]` draws nothing wherever it sits.
  if k < 0 or k >= layers.len: return false
  let l = unsafeAddr layers[k]
  var empty = false
  case l.content.kind
  of lkRaster:
    let s = unsafeAddr l.content.raster
    empty = s[].tileCount == 0 and s[].format.alpha
    if empty:
      for b in s[].defaultBytes:
        if b != 0: empty = false
  of lkGroup: empty = l.content.group.children.len == 0 and l.content.group.artboard.isNone
  else: discard
  empty and l.blend in {bmNormal, bmPassThrough} and l.mask.isNone and l.vectorMask.isNone and
    not hasEffects(l[]) and l.fillCache.isNone and l.video.isNone and l.advanced == defaultAdvancedBlending() and
    (l.clipped or k + 1 >= layers.len or not layers[k + 1].clipped)

proc damageWalk(xs, ys: seq[Layer]; a, b: Document; added: bool; outp: var Rect): bool =
  var i, j = 0
  while true:
    let hasX = i < xs.len
    let hasY = j < ys.len
    if not hasX and not hasY: return true
    if hasX and hasY and xs[i].id == ys[j].id: discard
    elif inertAt(ys, j):
      inc j
      continue
    elif added and hasY and (block:
        var only = true
        for x in xs:
          if x.id == ys[j].id: only = false
        only):
      let r = addedBounds(ys, j, b.bounds)
      if r.isNone: return false
      if not r.get.isEmpty: outp = if outp.isEmpty: r.get else: outp.union(r.get)
      inc j
      continue
    elif inertAt(xs, i):
      inc i
      continue
    else: return false
    let x = unsafeAddr xs[i]
    let y = unsafeAddr ys[j]
    inc i
    inc j
    if x.video.isSome or y.video.isSome: return false
    # Clipping and channel exclusion change how other layers render: no bounded damage.
    if x.clipped != y.clipped or x.excludedChannels != y.excludedChannels: return false
    if not layerPixelsEqual(x[], y[]):
      let bx = changeBounds(x[], a.bounds)
      let by = changeBounds(y[], b.bounds)
      if bx.isNone or by.isNone: return false
      outp = if outp.isEmpty: bx.get.union(by.get) else: outp.union(bx.get).union(by.get)
    let xg = x.content.kind == lkGroup
    let yg = y.content.kind == lkGroup
    if xg and yg:
      if not damageWalk(x.content.group.children, y.content.group.children, a, b, added, outp): return false
    elif xg != yg: return false

proc damageBetween(a, b: Document; boundedAdditions: bool): Option[Rect] =
  if not docPixelsEqual(a, b): return none(Rect)
  var outp = EmptyRect
  if damageWalk(a.layers, b.layers, a, b, boundedAdditions, outp): some(outp) else: none(Rect)

proc stepDamage*(a, b: Document): Option[Rect] =
  ## Pixels a single history step can have changed between `a` and `b`; none = everything.
  damageBetween(a, b, false)

proc insertDamage*(before, after: Document): Option[Rect] =
  ## `stepDamage` for an edit that may also have added self-contained layers.
  damageBetween(before, after, true)

proc noteInertInsert*(s: var Session; id: LayerId) =
  ## Reports that adding layer `id` changed no pixels when it draws nothing.
  let st = s.active
  if st.isNil: return
  let path = st.doc[].pathOf(id)
  if path.isNone or path.get.len == 0: return
  let k = path.get[^1]
  let parent = path.get[0 ..< ^1]
  if parent.len == 0:
    if inertAt(st.doc.layers, k): st.lastDamage = some(EmptyRect)
  else:
    let pl = st.doc[].layerAt(parent)
    if pl.isSome and pl.get.content.kind == lkGroup and inertAt(pl.get.content.group.children, k):
      st.lastDamage = some(EmptyRect)

proc noteInsert*(s: var Session; before: Document) =
  ## Reports the pixels an edit that added layers changed.
  let st = s.active
  if st.isNil: return
  st.lastDamage = insertDamage(before, st.doc[])

proc layerBounds*(l: Layer): Option[Rect] =
  ## Content bounds used by Align/Distribute: the layer's pixels, or the union of a group's
  ## children. Fill and adjustment layers have none.
  var r = EmptyRect
  if l.content.kind == lkGroup:
    for c in l.content.group.children:
      let b = layerBounds(c)
      if b.isSome: r = r.union(b.get)
  elif l.hasSurface: r = l.surface.contentBounds
  if r.isEmpty: none(Rect) else: some(r)

# ---------------------------------------------------------------- align and distribute

type Edge = enum edTop, edVCenter, edBottom, edLeft, edHCenter, edRight

proc parseEdge(k: string): Option[Edge] =
  case k
  of "topEdges": some(edTop)
  of "verticalCenters": some(edVCenter)
  of "bottomEdges": some(edBottom)
  of "leftEdges": some(edLeft)
  of "horizontalCenters": some(edHCenter)
  of "rightEdges": some(edRight)
  else: none(Edge)

proc vertical(e: Edge): bool = e in {edTop, edVCenter, edBottom}

proc value2(e: Edge; r: Rect): int64 =
  ## The edge's coordinate, doubled so centres stay integral.
  let (a, b) = if e.vertical: (r.y0, r.y1) else: (r.x0, r.x1)
  case e
  of edTop, edLeft: 2 * int64(a)
  of edBottom, edRight: 2 * int64(b)
  of edVCenter, edHCenter: int64(a) + int64(b)

proc delta(e: Edge; d: int): (int, int) = (if e.vertical: (0, d) else: (d, 0))

proc boundedSelection(doc: Document; sel: openArray[LayerId]): seq[(LayerId, Rect)] =
  for id in topLevel(doc, sel):
    let l = doc.findLayer(id)
    if l.isNil: continue
    let b = layerBounds(l[])
    if b.isSome: result.add (id, b.get)

proc dedupMoves(doc: Document; moves: seq[(LayerId, int, int)]): seq[(LayerId, int, int)] =
  ## One move per layer (first wins), skipping layers nested in another moved layer.
  for m in moves:
    var dup = false
    for o in result:
      if o[0] == m[0]: dup = true
    if not dup: result.add m
  var ids: seq[LayerId]
  for m in result: ids.add m[0]
  let keep = topLevel(doc, ids)
  result.keepItIf(it[0] in keep)

proc floorDiv2(v: int64): int64 =
  ## `div_euclid(2)`.
  if v >= 0: v div 2 else: -((-v + 1) div 2)

proc alignCmd(s: var Session; p: JsonNode; kind: string): JsonNode =
  let cmdId = "layer.align." & kind
  let edgeO = parseEdge(kind)
  if edgeO.isNone: raise bad(cmdId, "unknown edge")
  let edge = edgeO.get
  let sel = selected(s)
  let to = p.getStrOr("to", "auto")
  let count = s.editGet("Align", int):
    let items = boundedSelection(doc, sel)
    if items.len == 0: raise other("the selected layers have no content to align")
    var target: Rect
    case to
    of "canvas": target = doc.bounds
    of "selection":
      if doc.selection.isNone: raise other("there is no selection to align to")
      target = doc.selection.get.contentBounds
    of "auto", "layers":
      if doc.selection.isSome and sel.len == 1 and to == "auto": target = doc.selection.get.contentBounds
      elif doc.selection.isNone and sel.len == 1 and to == "auto": target = doc.bounds
      else:
        target = EmptyRect
        for (_, b) in items: target = target.union(b)
    else: raise bad(cmdId, "`to` must be auto|layers|selection|canvas, not `" & to & "`")
    if target.isEmpty: raise other("nothing to align to")
    let t2 = edge.value2(target)
    var moves: seq[(LayerId, int, int)]
    for (id, r) in items:
      let d = int(floorDiv2(t2 - edge.value2(r)))
      let (dx, dy) = edge.delta(d)
      # Linked layers travel with the layer they are linked to.
      for l in withLinks(doc, [id]): moves.add (l, dx, dy)
    let ms = dedupMoves(doc, moves)
    moveLayers(doc, ms)
    var n = 0
    for m in ms:
      if m[1] != 0 or m[2] != 0: inc n
    n
  %*{"moved": count}

proc distributeCmd(s: var Session; kind: string): JsonNode =
  let sel = selected(s)
  let count = s.editGet("Distribute", int):
    var items = boundedSelection(doc, sel)
    if items.len < 3: raise other("distributing needs three or more layers with content")
    let n = items.len
    var moves: seq[(LayerId, int, int)]
    if kind in ["horizontally", "vertically"]:
      # Equal gaps between neighbours; the outermost layers stay put.
      let v = kind == "vertically"
      proc lo(r: Rect): int64 = int64(if v: r.y0 else: r.x0)
      proc hi(r: Rect): int64 = int64(if v: r.y1 else: r.x1)
      items.sort(proc(a, b: (LayerId, Rect)): int = cmp((lo(a[1]) + hi(a[1]), lo(a[1])), (lo(b[1]) + hi(b[1]), lo(b[1]))))
      let span = hi(items[n - 1][1]) - lo(items[0][1])
      var total = 0'i64
      for (_, r) in items: total += hi(r) - lo(r)
      let gap = float64(span - total) / float64(n - 1)
      var cursor = float64(lo(items[0][1]))
      for (id, r) in items:
        let want = int64(round(cursor))
        let d = int(want - lo(r))
        moves.add (if v: (id, 0, d) else: (id, d, 0))
        cursor += float64(hi(r) - lo(r)) + gap
    else:
      let edgeO = parseEdge(kind)
      if edgeO.isNone: raise bad("layer.distribute", "unknown kind `" & kind & "`")
      let edge = edgeO.get
      items.sort(proc(a, b: (LayerId, Rect)): int = cmp(edge.value2(a[1]), edge.value2(b[1])))
      let first = float64(edge.value2(items[0][1]))
      let last = float64(edge.value2(items[n - 1][1]))
      for i, (id, r) in items:
        let want2 = first + (last - first) * float64(i) / float64(n - 1)
        let d = asI32(round((want2 - float64(edge.value2(r))) / 2.0))
        let (dx, dy) = edge.delta(d)
        moves.add (id, dx, dy)
    var all: seq[(LayerId, int, int)]
    for (id, dx, dy) in moves:
      for l in withLinks(doc, [id]): all.add (l, dx, dy)
    let ms = dedupMoves(doc, all)
    moveLayers(doc, ms)
    var c = 0
    for m in ms:
      if m[1] != 0 or m[2] != 0: inc c
    c
  %*{"moved": count}

# ---------------------------------------------------------------- reorder

proc checkGroupDepth*(doc: Document; what: string) =
  ## Refuses an edit that left `doc` nested deeper than `MaxGroupDepth` groups.
  if doc.maxGroupDepth > MaxGroupDepth:
    raise other(what & " would nest layers deeper than " & $MaxGroupDepth & " groups")

proc moveTo*(s: var Session; p: JsonNode): JsonNode =
  ## Reorders one or several layers to a single destination in one undoable edit.
  const cmdId = "layer.moveTo"
  let t = p.pget("target").asU64
  if t.isNone: raise bad(cmdId, "missing `target`")
  let target = LayerId(t.get)
  let pos = p.getStrOr("position", "above")
  if pos notin ["above", "below", "into"]: raise bad(cmdId, "position must be above, below, or into")
  # ⌥-drag in the Layers panel: duplicates land at the target and the layers stay put.
  let copy = p.getBoolOr("copy", false)
  let batch = p.pget("layers")
  if not batch.isNil and not p.pget("layer").isNil: raise bad(cmdId, "give either `layer` or `layers`, not both")
  var requested: seq[LayerId]
  if not batch.isNil:
    if not batch.isArray or batch.len == 0: raise bad(cmdId, "`layers` must be a nonempty array of ids")
    for item in batch.elems:
      let v = item.asU64
      if v.isNone: raise bad(cmdId, "every layer id must be an unsigned integer")
      if LayerId(v.get) in requested: raise bad(cmdId, "duplicate layer id")
      requested.add LayerId(v.get)
  else: requested = @[layerParam(s, p)]
  let st = s.activeOrRaise()
  let doc0 = st.doc
  let targetPath = doc0[].pathOf(target)
  if targetPath.isNone: raise noLayer(target)
  for id in requested:
    if doc0[].findLayer(id).isNil: raise noLayer(id)
  let movedIds = topLevel(doc0[], requested)
  if movedIds.len == 1 and movedIds[0] == target and batch.isNil and not copy:
    # Preserve single-layer drag-to-self semantics.
    return jnull()
  # Also rejects dragging a selected group onto any of its descendants.
  for id in movedIds:
    let path = doc0[].pathOf(id)
    if path.isNone: raise noLayer(id)
    let besideItself = copy and id == target and pos != "into"
    if targetPath.get.startsWith(path.get) and not besideItself:
      raise bad(cmdId, "target cannot be one of the moved layers or its descendant")
  if pos == "into":
    let tl = doc0[].findLayer(target)
    if tl.isNil or not tl[].isGroup: raise bad(cmdId, "target is not a group")
  let originalActive = st.activeLayer
  let background = backgroundId(doc0[])
  let hasBatch = not batch.isNil
  let (count, copies) = s.editGet((if copy: "Duplicate Layer" else: "Reorder Layers"), (int, seq[LayerId])):
    let before = if copy: ClipBefore() else: readBefore(doc, movedIds)
    var moved: seq[Layer]
    for id in movedIds:
      if copy: moved.add layerCopy(doc, id)
      else:
        let r = doc.remove(id)
        if r.isNone: raise noLayer(id)
        moved.add r.get
    var copies: seq[LayerId]
    if copy:
      for l in moved: copies.add l.id
    # Look up the destination after removing every source: its indices may have shifted.
    let path = doc.pathOf(target)
    if path.isNone: raise noLayer(target)
    if pos == "into":
      let tl = doc.layerAtPtr(path.get)
      if tl.isNil or tl[].content.kind != lkGroup: raise bad(cmdId, "target is not a group")
      for l in moved: tl[].content.group.children.add l
    else:
      let idx = path.get[^1]
      let parent = path.get[0 ..< ^1]
      var siblings: ptr seq[Layer]
      if parent.len == 0: siblings = addr doc.layers
      else:
        let pl = doc.layerAtPtr(parent)
        if pl.isNil or pl[].content.kind != lkGroup: raise bad(cmdId, "target's parent is not a group")
        siblings = addr pl[].content.group.children
      let at = if pos == "below": idx else: idx + 1
      siblings[].insert(moved, at)
    checkGroupDepth(doc, "Reorder Layer")
    keepBackgroundAtBottom(background, doc)
    settle(doc, (if copy: copies else: movedIds), before, false)
    if copy: active = (if copies.len > 0: some(copies[^1]) else: none(LayerId))
    elif hasBatch:
      active = if originalActive.isSome and originalActive.get in requested: originalActive
               else: some(requested[^1])
    else: active = (if movedIds.len > 0: some(movedIds[0]) else: none(LayerId))
    (movedIds.len, copies)
  if copy:
    if hasBatch:
      let act = if copies.len > 0: some(copies[^1]) else: none(LayerId)
      reselect(s, copies, act)
      return %*{"moved": count, "layers": idsJson(copies)}
    return %*{"layer": (if copies.len > 0: %copies[0] else: newJNull())}
  if hasBatch:
    let act = if originalActive.isSome and originalActive.get in requested: originalActive else: some(requested[^1])
    reselect(s, requested, act)
    %*{"moved": count}
  else: jnull()

# ---------------------------------------------------------------- structural commands

proc linkLayersCmd(s: var Session): JsonNode =
  let sel = selected(s)
  let linked = s.editGet("Link Layers", bool):
    var groups: seq[Option[uint64]]
    for id in sel:
      let l = doc.findLayer(id)
      groups.add (if l.isNil: none(uint64) else: l.linkGroup)
    # Already one link group: unlink, like Photoshop's toggle.
    let first = if groups.len > 0: groups[0] else: none(uint64)
    var unlink = first.isSome
    for g in groups:
      if g != first: unlink = false
    var next = none(uint64)
    if not unlink:
      next = doc.unusedLinkGroup
      if next.isNone: raise bad("layer.linkLayers", "no free link group id is left")
    for id in sel: doc.layerOrRaise(id).linkGroup = next
    # A group left with a single member is no longer a link.
    var counts = initTable[uint64, int]()
    for (_, l) in doc.walkPtrs:
      if l.linkGroup.isSome: counts.mgetOrPut(l.linkGroup.get, 0) += 1
    var lonely: seq[LayerId]
    for (_, l) in doc.walkPtrs:
      if l.linkGroup.isSome and counts.getOrDefault(l.linkGroup.get, 0) < 2: lonely.add l.id
    for id in lonely:
      let l = doc.layerPtr(id)
      if not l.isNil: l.linkGroup = none(uint64)
    not unlink
  %*{"linked": linked}

proc lockLayersCmd(s: var Session; p: JsonNode): JsonNode =
  let sel = selected(s)
  var given: seq[(string, bool)]
  for k in ["transparency", "pixels", "position", "artboard", "all"]:
    let v = p.pget(k).asBool
    if v.isSome: given.add (k, v.get)
  s.editDo("Lock Layers"):
    # No explicit locks: toggle "lock all" (on unless every selected layer is already locked).
    var allLocked = true
    for id in sel:
      let l = doc.findLayer(id)
      if l.isNil or not l.locks.all: allLocked = false
    let g = if given.len == 0: @[("all", not allLocked)] else: given
    for id in sel:
      let l = doc.layerOrRaise(id)
      for (k, v) in g:
        case k
        of "transparency": l.locks.transparency = v
        of "pixels": l.locks.pixels = v
        of "position": l.locks.position = v
        of "artboard": l.locks.artboard = v
        else: l.locks.all = v
  %*{"layers": sel.len}

proc renameLayerCmd(s: var Session; p: JsonNode): JsonNode =
  let nm = p.pget("name").asStr
  if nm.isNone or nm.get.strip.len == 0: raise bad("layer.renameLayer", "missing `name`")
  let name = nm.get
  let id = layerParam(s, p)
  s.editDo("Rename Layer"): doc.layerOrRaise(id).name = name
  jnull()

proc reverseCmd(s: var Session): JsonNode =
  let sel = selected(s)
  s.editDo("Reverse"):
    let background = backgroundId(doc)
    let ids = topLevel(doc, sel)
    if ids.len < 2: raise other("select two or more layers to reverse")
    var paths: seq[LayerPath]
    var layers: seq[Layer]
    for id in ids:
      let p = doc.pathOf(id)
      if p.isNone: raise noLayer(id)
      paths.add p.get
    for id in ids:
      let l = doc.findLayer(id)
      if l.isNil: raise noLayer(id)
      layers.add l[]
    for i, path in paths:
      let slot = doc.layerAtPtr(path)
      if slot.isNil: raise other("layer tree changed")
      slot[] = layers[layers.len - 1 - i]
    keepBackgroundAtBottom(background, doc)
  jnull()

proc groupLayers*(s: var Session; p: JsonNode): JsonNode =
  ## Group Layers (⌘G): the explicit layer, or every selected layer, moves into a new group
  ## placed where the top-most of them was.
  let lv = p.pget("layer").asU64
  let ids = if lv.isSome: @[LayerId(lv.get)] else: selected(s)
  if ids.len == 0: raise other("no layer selected")
  let name = p.pget("name").asStr
  let gid = s.editGet("Group Layers", LayerId):
    let tops = topLevel(doc, ids)
    if tops.len == 0: raise noLayer(LayerId(0))
    let top = tops[^1]
    let nm = if name.isSome: name.get else: doc.nextLayerName("Group")
    let gid = doc.insertAbove(some(top), groupLayer(nm, @[]))
    var children: seq[Layer]
    for id in tops:
      let r = doc.remove(id)
      if r.isNone: raise noLayer(id)
      children.add r.get
    let g = doc.layerPtr(gid)
    if g.isNil or g[].content.kind != lkGroup: raise noLayer(gid)
    g[].content.group.children = children
    checkGroupDepth(doc, "Group Layers")
    active = some(gid)
    gid
  reselect(s, @[gid], some(gid))
  %*{"layer": gid}

proc mergeLayersCmd(s: var Session): JsonNode =
  let sel = selected(s)
  if sel.len < 2: return s.execute("layer.mergeDown", newJObject())
  let mid = s.editGet("Merge Layers", LayerId):
    let ids = topLevel(doc, sel)
    if ids.len == 0: raise other("Merge Layers needs two or more layers")
    let topId = ids[^1]
    let topL = doc.findLayer(topId)
    if topL.isNil: raise noLayer(topId)
    let top = topL[]
    var solo = doc
    solo.layers = @[]
    for id in ids:
      let l = doc.findLayer(id)
      if not l.isNil and l.visible: solo.layers.add l[]
    if solo.layers.len == 0: raise other("the selected layers are all hidden")
    let f0 = doc.pixelFormat
    let fmt = PixelFormat(mode: f0.mode, sample: f0.sample, alpha: true)
    var merged = rasterLayer(top.name, fmt)
    merged.locks = top.locks
    merged.pixelsMut[] = compositeLayers(solo, fmt, none(array[3, float32]))
    let mid = merged.id
    # Hidden selected layers are discarded, as in Photoshop.
    for id in ids[0 ..< ^1]: discard doc.remove(id)
    let slot = doc.layerPtr(topId)
    if slot.isNil: raise noLayer(topId)
    slot[] = merged
    active = some(mid)
    mid
  reselect(s, @[mid], some(mid))
  %*{"layer": mid}

proc deletionNeighbours*(doc: Document; target: LayerId): seq[LayerId] =
  ## Next visible layer row below the deleted target, then rows above in reverse order.
  proc rows(expandAll: bool): seq[LayerId] =
    var stack: seq[ptr Layer]
    for i in 0 ..< doc.layers.len: stack.add unsafeAddr doc.layers[i]
    while stack.len > 0:
      let layer = stack.pop()
      result.add layer.id
      if layer.content.kind == lkGroup and (expandAll or layer.content.group.expanded):
        for i in 0 ..< layer.content.group.children.len: stack.add unsafeAddr layer.content.group.children[i]
  var order = rows(false)
  if target notin order: order = rows(true)
  let index = order.find(target)
  if index < 0: return
  for i in index + 1 ..< order.len: result.add order[i]
  for i in countdown(index - 1, 0): result.add order[i]

proc deleteSelected*(s: var Session): JsonNode =
  ## Delete Layer with several layers selected.
  let sel = selected(s)
  s.editDo("Delete Layers"):
    let ids = topLevel(doc, sel)
    let neighbours = if active.isSome: deletionNeighbours(doc, active.get) else: @[]
    let before = readBefore(doc, ids)
    for id in ids:
      if doc.remove(id).isNone: raise noLayer(id)
    settle(doc, [], before, false)
    if doc.layers.len == 0: raise other("a document must keep at least one layer")
    active = none(LayerId)
    for c in neighbours:
      if not doc.findLayer(c).isNil:
        active = some(c)
        break
  %*{"deleted": sel.len}

proc duplicateSelected*(s: var Session; inPlace: bool; label: string): JsonNode =
  ## Duplicate Layer with several layers selected: each copy goes above its original and the
  ## copies become the selection.
  let sel = selected(s)
  let before = s.activeOrRaise().doc[]
  let oldActive = s.activeOrRaise().activeLayer
  let (copies, act) = s.editGet(label, (seq[LayerId], Option[LayerId])):
    var copies: seq[LayerId]
    var newActive = none(LayerId)
    for id in topLevel(doc, sel):
      let dup = layerCopy(doc, id)
      let nid = doc.insertAbove(some(id), dup)
      if not inPlace: placeCopy(doc, nid)
      if some(id) == oldActive: newActive = some(nid)
      copies.add nid
    let a = if newActive.isSome: newActive elif copies.len > 0: some(copies[^1]) else: none(LayerId)
    active = a
    (copies, a)
  noteInsert(s, before)
  let outp = idsJson(copies)
  reselect(s, copies, act)
  %*{"layers": outp, "layer": (if act.isSome: %act.get else: newJNull())}

proc setVisibleSelected*(s: var Session; visible: bool): JsonNode =
  ## Shows or hides every selected layer in one step.
  let sel = selected(s)
  s.editDo(if visible: "Show Layers" else: "Hide Layers"):
    for id in sel: doc.layerOrRaise(id).visible = visible
  jnull()

proc multi*(s: Session; p: JsonNode): bool =
  ## Is more than one layer selected (so single-layer commands act on the whole set)?
  p.pget("layer").isNil and selected(s).len > 1

# ---------------------------------------------------------------- registry

proc alignSpec(k, label, params: string): CommandSpec =
  cmd("layer.align." & k, label, ["Layer", "Align"], "", params, canAlignE, runProc(alignCmd(s, p, k)))

proc distributeSpec(k, label, params: string): CommandSpec =
  cmd("layer.distribute." & k, label, ["Layer", "Distribute"], "", params, canDistributeE, runProc(distributeCmd(s, k)))

proc layerMultiSpecs*(): seq[CommandSpec] =
  const Align = """{"to":"auto|layers|selection|canvas"="auto"} (auto: the selection bounds with one layer and an active selection, else the selected layers' bounds)"""
  result = @[
    cmd("select.allLayers", "All Layers", ["Select"], "Cmd+Alt+A", "{}", hasDocE, runProc(selectAllLayers(s))),
    cmd("select.deselectLayers", "Deselect Layers", ["Select"], "", "{}", hasDocE, runProc do:
      setSelection(s, @[], none(LayerId), none(LayerId))
      let st = s.active
      if not st.isNil:
        st.activeLayer = none(LayerId)
        st.selectedLayers.setLen(0)
      jnull()),
    cmd("select.findLayers", "Find Layers", ["Select"], "Cmd+Alt+Shift+F", """{"name":str (case-insensitive substring)}""",
        hasDocE, findLayers),
    cmd("layer.selectLinkedLayers", "Select Linked Layers", ["Layer"], "", "{}", hasLinkedE, runProc(selectLinked(s))),
  ]
  for (kind, label) in [("topEdges", "Top Edges"), ("verticalCenters", "Vertical Centers"), ("bottomEdges", "Bottom Edges"),
                        ("leftEdges", "Left Edges"), ("horizontalCenters", "Horizontal Centers"), ("rightEdges", "Right Edges")]:
    result.add alignSpec(kind, label, Align)
  for (kind, label, params) in [("topEdges", "Top Edges", "{}"), ("verticalCenters", "Vertical Centers", "{}"),
                                ("bottomEdges", "Bottom Edges", "{}"), ("leftEdges", "Left Edges", "{}"),
                                ("horizontalCenters", "Horizontal Centers", "{}"), ("rightEdges", "Right Edges", "{}"),
                                ("horizontally", "Horizontally", "{} (equal horizontal gaps)"),
                                ("vertically", "Vertically", "{} (equal vertical gaps)")]:
    result.add distributeSpec(kind, label, params)
  result.add @[
    cmd("layer.linkLayers", "Link Layers", ["Layer"], "", "{} (toggles: unlinks when the selection is already one link group)",
        canLinkE, runProc(linkLayersCmd(s))),
    cmd("layer.mergeLayers", "Merge Layers", ["Layer"], "Cmd+E", "{} (one layer selected: Merge Down)", lmHasLayerE,
        runProc(mergeLayersCmd(s))),
    cmd("layer.new.groupFromLayers", "Group from Layers…", ["Layer", "New"], "", """{"name":str?}""", lmHasLayerE, groupLayers),
    cmd("layer.arrange.reverse", "Reverse", ["Layer", "Arrange"], "", "{}", twoPlusE, runProc(reverseCmd(s))),
    cmd("layer.lockLayers", "Lock Layers…", ["Layer"], "",
        """{"transparency":bool?,"pixels":bool?,"position":bool?,"artboard":bool?,"all":bool?} (none given: toggle lock all)""",
        lmHasLayerE, lockLayersCmd),
    cmd("layer.renameLayer", "Rename Layer", ["Layer"], "", """{"layer":id?,"name":str}""", lmHasLayerE, renameLayerCmd),
  ]
