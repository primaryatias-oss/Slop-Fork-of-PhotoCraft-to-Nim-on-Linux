## Edit › Free Transform / Transform: projective transforms of layers (and the selection) (port of
## `transform_cmds.rs`). Part of the `engine/core` unit: included, not imported.

proc mapShape*(sh: var ShapeLayer; h: Option[Homography]; warp: Option[JsonNode])
proc mapPath*(path: Path; h: Option[Homography]; warp: Option[JsonNode]): Path
proc smartPlacement*(sm: SmartObject): Homography
proc setPlacement*(sm: var SmartObject; h: Homography)

proc tfBad(msg: string): ref EngineError = bad("edit.transform", msg)

proc warpSurfaceE(src: Surface; srcRect: Rect; h: Homography; interp: Interp): Surface =
  ## `warp_surface(..)?`: an allocation failure becomes the engine's memory error.
  try: warpSurface(src, srcRect, h, interp)
  except AllocationError as e: raise other("not enough memory for this operation (" & e.msg & "); the document was not changed")

proc movesWhole*(layer: Layer): bool =
  ## Layers a transform moves as a whole, even with a selection: groups, and the kinds that keep
  ## the transform instead of pixels (type, shapes, smart objects).
  layer.content.kind in {lkGroup, lkText, lkShape, lkSmart}

proc transformBounds*(doc: Document; l: Layer): Rect =
  ## Document-space bounds a transform of `l` starts from (what Free Transform frames): the
  ## content within the selection, or all of it for a layer that `movesWhole`.
  var content = EmptyRect
  if l.content.kind == lkGroup:
    for c in l.content.group.children: content = content.union(transformBounds(doc, c))
  elif l.hasSurface: content = contentBounds(l.surface)
  if content.isEmpty and l.mask.isSome: content = contentBounds(l.mask.get.surface)
  if doc.selection.isSome and not movesWhole(l): content.intersect(doc.selection.get.contentBounds)
  else: content

proc warpGray*(s: Surface; h: Homography; interp: Interp): Surface =
  ## Warp a surface whose pixels outside the content read as `default` (masks, selections): warp
  ## the content with alpha, then flatten back onto the default value.
  let dp = s.defaultPixel
  let default = if dp.len > 0: dp[0] else: 0'f32
  let fmt = s.format
  let src = s.contentBounds
  result = surfaceWithDefault(fmt, [default])
  if src.isEmpty: return
  # Content-only copy (default 0 + alpha) so the "outside" is transparent during the warp.
  let withAlpha = PixelFormat(mode: fmt.mode, sample: fmt.sample, alpha: true)
  var tmp = newSurface(withAlpha)
  let v = allocReadRegion(s, src, "transforming a mask or channel")
  if v.len > high(int) div 2: raise tfBad("the mask is too large to transform")
  var lifted = allocFilled(v.len * 2, 0'f32, "transforming a mask or channel")
  for i, g in v:
    lifted[2 * i] = g
    lifted[2 * i + 1] = 1
  tmp.tryWriteRegion(src, lifted)
  let w = warpSurfaceE(tmp, src, h, interp)
  # Clear the old content region, then composite the warped content over it, as two sparse
  # regions (a far move would otherwise allocate a buffer spanning both).
  let oldLen = src.width * src.height
  let old = allocFilled(oldLen, default, "transforming a mask or channel")
  result.tryWriteRegion(src, old)
  let b = w.contentBounds
  if not b.isEmpty:
    let px = allocReadRegion(w, b, "flattening a transformed mask or channel")
    var flat = allocFilled(px.len div 2, 0'f32, "flattening a transformed mask or channel")
    for i in 0 ..< flat.len:
      flat[i] = px[2 * i] * px[2 * i + 1] + default * (1 - px[2 * i + 1])
    result.tryWriteRegion(b, flat)
  result.prune()

proc splitGraySelected*(s: Surface; sel: Surface): Option[(Surface, Surface)] =
  ## Split a single-channel surface whose outside reads as its default (a mask, a channel) by a
  ## selection: (the selected values as grey + alpha = selection, the surface with the selected
  ## values replaced by the default). None for surfaces that aren't a single grey channel.
  let fmt = s.format
  if fmt.alpha or fmt.channels != 1: return
  let dp = s.defaultPixel
  let default = if dp.len > 0: dp[0] else: 0'f32
  var rest = s
  var lifted = newSurface(PixelFormat(mode: fmt.mode, sample: fmt.sample, alpha: true))
  # Everything selected moves, including default-valued parts outside the painted content.
  let area = sel.contentBounds
  if area.isEmpty: return some((lifted, rest))
  let v = allocReadRegion(s, area, "transforming a selected mask or channel")
  let w = area.width
  if v.len > high(int) div 2: raise tfBad("the selected mask is too large to transform")
  checkCapacity(v.len * 2, sizeof(float32), "transforming a selected mask or channel")
  var lp = newSeqOfCap[float32](v.len * 2)
  var rp = newSeqOfCap[float32](v.len)
  for i, g in v:
    let (x, y) = (area.x0 + i mod w, area.y0 + i div w)
    let k = sel.sampleChannel(x, y, 0)
    lp.add g
    lp.add k
    rp.add g * (1 - k) + default * k
  lifted.tryWriteRegion(area, lp)
  lifted.prune()
  rest.tryWriteRegion(area, rp)
  rest.prune()
  some((lifted, rest))

proc moveGraySelected*(s: Surface; sel: Surface; warp: proc(lifted: Surface; src: Rect): Surface;
                       whole: proc(s: Surface): Surface): Surface =
  ## `warpGraySelected` with any geometric warp: `warp` moves the lifted grey + alpha values
  ## (given their bounds), `whole` moves `s` when it isn't a single grey channel.
  let split = splitGraySelected(s, sel)
  if split.isNone: return whole(s)
  var (lifted, outp) = split.get
  let src = lifted.contentBounds
  if src.isEmpty: return outp
  let w = warp(lifted, src)
  let b = w.contentBounds
  if not b.isEmpty:
    let moved = allocReadRegion(w, b, "compositing transformed mask pixels")
    let under = allocReadRegion(outp, b, "compositing transformed mask pixels")
    var flat = allocFilled(under.len, 0'f32, "compositing transformed mask pixels")
    for i in 0 ..< min(under.len, moved.len div 2):
      flat[i] = moved[2 * i] * moved[2 * i + 1] + under[i] * (1 - moved[2 * i + 1])
    outp.tryWriteRegion(b, flat)
  outp.prune()
  outp

proc warpGraySelected*(s: Surface; sel: Surface; h: Homography; interp: Interp): Surface =
  ## `warpGray` limited to a selection: only the selected values move (the vacated area reads as
  ## the default), as the selected pixels of the layer do.
  moveGraySelected(s, sel, proc(lifted: Surface; src: Rect): Surface = warpSurfaceE(lifted, src, h, interp),
                   proc(s: Surface): Surface = warpGray(s, h, interp))

proc loneTarget*(doc: Document; active: Option[LayerId]; p: JsonNode): Option[Surface] =
  ## The surface Free Transform moves by itself when the params target one (`"target"`): an
  ## unlinked layer mask, an alpha channel or the Quick Mask. None means the layer, with its
  ## linked masks.
  let t = targetOf(p)
  case t.kind
  of tgPixels: none(Surface)
  of tgMask:
    if active.isNone: raise other("no active layer")
    let l = doc.findLayer(active.get)
    if l.isNil: raise noLayer(active.get)
    if l.mask.isNone: raise other("layer has no mask")
    if not l.mask.get.linked: some(l.mask.get.surface) else: none(Surface)
  of tgAlpha:
    if t.index < 0 or t.index >= doc.channels.len:
      raise other("no alpha channel " & $t.index & " (document has " & $doc.channels.len & ")")
    some(doc.channels[t.index].surface)
  of tgQuickMask:
    if doc.quickMask.isNone: raise other("not in Quick Mask mode")
    some(doc.quickMask.get.surface)

proc targetBounds*(doc: Document; surf: Surface): Rect =
  ## The frame Free Transform starts from on a lone target: its painted content (the whole canvas
  ## when nothing is painted), within the selection.
  var content = surf.contentBounds
  if content.isEmpty: content = doc.bounds
  if doc.selection.isSome: content.intersect(doc.selection.get.contentBounds) else: content

proc groupLocks*(doc: Document; id: LayerId): Locks =
  ## The locks `id` inherits from the groups around it.
  let p = doc.pathOf(id)
  if p.isNone or p.get.len == 0: return Locks()
  doc.locksAt(p.get[0 ..< p.get.len - 1])

proc transformLocks(l: Layer; group: Locks): Locks =
  ## The locks a transform of `l` meets: its own and `group`'s. The Background's position lock
  ## doesn't count: Photoshop turns it into a normal layer first.
  var own = l.locks
  if own.position and l.name == "Background":
    own.position = false
    own.transparency = false
  own.union(group)

proc lockedForTransform*(doc: Document; id: LayerId): bool =
  ## Would a transform of the layer `id` be refused because it, a group around it, or (for a
  ## group) a layer inside it is locked in place?
  let l = doc.findLayer(id)
  if l.isNil: return false
  var stack = @[(l, groupLocks(doc, id))]
  while stack.len > 0:
    let (x, group) = stack.pop()
    let locks = transformLocks(x[], group)
    if locks.position or locks.all: return true
    if x.content.kind == lkGroup:
      for c in x.content.group.children: stack.add (unsafeAddr c, locks)
  false

proc compositeOver*(dst: var Surface; top: Surface) =
  ## Normal-blend `top` over `dst` (both straight alpha, same format).
  let b = top.contentBounds
  if b.isEmpty: return
  let n = dst.format.channels
  let a = n - 1
  let t = top.readRegion(b)
  var d = dst.readRegion(b)
  var i = 0
  while i + n <= d.len and i + n <= t.len:
    let (ta, da) = (t[i + a], d[i + a])
    let oa = ta + da * (1 - ta)
    if oa > 0:
      for c in 0 ..< a: d[i + c] = (t[i + c] * ta + d[i + c] * da * (1 - ta)) / oa
    d[i + a] = oa
    i += n
  dst.writeRegion(b, d)

proc splitSelected*(surf: Surface; sel: Surface): (Surface, Surface) =
  ## Split a layer surface by a selection: (selected pixels, everything else), both with alpha.
  let fmt = surf.format
  let withAlpha = PixelFormat(mode: fmt.mode, sample: fmt.sample, alpha: true)
  var lifted = newSurface(withAlpha)
  var rest = surf.tryConvert(withAlpha)
  let src = surf.contentBounds
  if src.isEmpty: return (lifted, rest)
  let n = withAlpha.channels
  let px = allocReadRegion(rest, src, "transforming selected pixels")
  var lp = allocFilled(px.len, 0'f32, "transforming selected pixels")
  for i in 0 ..< px.len: lp[i] = px[i]
  var rp = px
  let w = src.width
  # A pixel the selection fully lifts out is reset to the surface's (transparent) default.
  var restClear = rest.defaultPixel
  if restClear.len > 0 and restClear[^1] > 0:
    for v in restClear.mitems: v = 0
  var i = 0
  var k0 = 0
  while k0 + n <= lp.len:
    let (x, y) = (src.x0 + i mod w, src.y0 + i div w)
    let k = sel.sampleChannel(x, y, 0)
    lp[k0 + n - 1] *= k
    rp[k0 + n - 1] *= 1 - k
    if rp[k0 + n - 1] <= 0:
      for c in 0 ..< min(n, restClear.len): rp[k0 + c] = restClear[c]
    inc i
    k0 += n
  lifted.tryWriteRegion(src, lp)
  lifted.prune()
  rest.tryWriteRegion(src, rp)
  (lifted, rest)

proc transformLayer*(docSel: Option[Surface]; group: Locks; l: var Layer; h: Homography; affine: Option[Affine];
                     interp: Interp) =
  ## `group` holds the locks `l` inherits from the groups around it (`groupLocks`).
  allocCheckpoint("transforming pixels")
  let locks = transformLocks(l, group)
  # Photoshop turns the Background into a normal layer before transforming it.
  if l.locks.position and l.name == "Background":
    l.locks.position = false
    l.locks.transparency = false
    l.name = "Layer 0"
  if locks.position or locks.all: raise other("layer \"" & l.name & "\" is locked")
  if h.isIdentity:
    if l.content.kind == lkGroup:
      for c in l.content.group.children.mitems: transformLayer(none(Surface), locks, c, h, affine, interp)
    return
  # With a selection only the selected pixels move, and so only the same region of a linked mask.
  let maskSel = if movesWhole(l): none(Surface) else: docSel
  case l.content.kind
  of lkGroup:
    for c in l.content.group.children.mitems: transformLayer(none(Surface), locks, c, h, affine, interp)
  of lkText:
    if affine.isNone:
      raise other("Distort and Perspective need rasterized type (Type › Rasterize Type Layer)")
    l.content.text.transform = affine.get.mul(l.content.text.transform)
  of lkShape:
    if affine.isSome: transformShape(l.content.shape, affine.get)
    else: mapShape(l.content.shape, some(h), none(JsonNode))
  of lkSmart:
    # Smart objects keep the transform and re-render from their source afterwards.
    if affine.isSome: transformPlacement(l.content.smart, affine.get)
    else: setPlacement(l.content.smart, h.mul(smartPlacement(l.content.smart)))
    if l.content.smart.cache.isSome:
      let c = l.content.smart.cache.get
      l.content.smart.cache = some(warpSurfaceE(c, c.contentBounds, h, interp))
    if l.content.smart.filterMask.isSome:
      l.content.smart.filterMask.get.surface = warpGray(l.content.smart.filterMask.get.surface, h, interp)
  else:
    let surf = l.surfaceMut
    if not surf.isNil:
      let src = surf[].contentBounds
      if docSel.isSome:
        # Only the selected pixels move: lift them, clear them, warp and paste back.
        var (lifted, rest) = splitSelected(surf[], docSel.get)
        let moved = warpSurfaceE(lifted, src, h, interp)
        compositeOver(rest, moved)
        rest.prune()
        surf[] = rest
      else: surf[] = warpSurfaceE(surf[], src, h, interp)
  if l.mask.isSome and l.mask.get.linked:
    l.mask.get.surface =
      if maskSel.isSome: warpGraySelected(l.mask.get.surface, maskSel.get, h, interp)
      else: warpGray(l.mask.get.surface, h, interp)
  if l.vectorMask.isSome and l.vectorMask.get.linked:
    l.vectorMask.get.path =
      if affine.isSome: l.vectorMask.get.path.transform(affine.get)
      else: mapPath(l.vectorMask.get.path, some(h), none(JsonNode))

proc refreshRendered*(doc: Document; l: var Layer) =
  ## `transform_cmds::refresh_text`: type, shapes and smart objects re-render from their geometry.
  case l.content.kind
  of lkText: refreshText(doc, l.content.text)
  of lkShape: refreshShape(doc, l.content.shape)
  of lkSmart:
    # Unavailable sources keep the warped cache.
    try: refreshSmartLayer(doc, l)
    except EngineError: discard
  of lkGroup:
    for c in l.content.group.children.mitems: refreshRendered(doc, c)
  else: discard

proc quadParam(p: JsonNode): Option[array[4, array[2, float64]]] =
  let a = p.pget("quad")
  if not a.isArray or a.len != 4: return
  var q: array[4, array[2, float64]]
  for i, v in a.elems:
    if not v.isArray or v.len < 2: return
    let x = v.elems[0].asF64
    let y = v.elems[1].asF64
    if x.isNone or y.isNone: return
    q[i] = [x.get, y.get]
  some(q)

proc withPanelTarget(s: Session; p: JsonNode): JsonNode =
  ## `p`, with the Channels panel's targeted alpha channel or Quick Mask as its `"target"` when it
  ## names neither a target nor a layer.
  result = if p.isObject: p.copy else: newJObject()
  if result.pget("target").isNil and result.pget("layer").isNil:
    let t = targetedChannel(s)
    if t.isSome and t.get.isObject:
      for k, v in t.get.fields: result[k] = v

proc transformCmd(s: var Session; p0: JsonNode): JsonNode =
  let p = withPanelTarget(s, p0)
  let st = s.activeOrRaise()
  var id = none(LayerId)
  let lv = p.pget("layer").asU64
  if lv.isSome: id = some(LayerId(lv.get))
  else: id = st.activeLayer
  let lone = loneTarget(st.doc[], id, p)
  if lone.isSome and isChannelTarget(p): id = none(LayerId)
  elif id.isNone: raise other("no active layer")
  var rect: array[4, float64]
  let rv = p.pget("rect")
  if rv.isArray and rv.len == 4:
    for i in 0 .. 3:
      let x = rv.elems[i].asF64
      rect[i] = if x.isSome: x.get else: 0
  else:
    var b: Rect
    if lone.isSome: b = targetBounds(st.doc[], lone.get)
    elif id.isSome:
      let l = st.doc[].findLayer(id.get)
      if l.isNil: raise noLayer(id.get)
      b = transformBounds(st.doc[], l[])
    else: b = EmptyRect
    if b.isEmpty: raise other("nothing to transform")
    rect = [float64(b.x0), float64(b.y0), float64(b.x1), float64(b.y1)]
  var h: Homography
  let q = quadParam(p)
  let mv = p.pget("matrix")
  if q.isSome:
    let r = rectToQuad(rect, q.get)
    if r.isNone: raise tfBad("degenerate quad")
    h = r.get
  elif mv.isArray:
    var v: seq[float64]
    for x in mv.elems:
      let f = x.asF64
      if f.isSome: v.add f.get
    if v.len != 6: raise tfBad("matrix must be [a, b, c, d, e, f]")
    # Affine [a b c d e f] maps (x, y) → (a·x + c·y + e, b·x + d·y + f).
    h = Homography(m: [v[0], v[2], v[4], v[1], v[3], v[5], 0.0, 0.0, 1.0])
  else: raise tfBad("pass `quad` (where the rect's corners go) or `matrix`")
  if h.inverse.isNone: raise tfBad("the transform collapses the layer")
  let m = h.m
  let affine = if abs(m[6]) < 1e-12 and abs(m[7]) < 1e-12:
                 some(Affine(m: [m[0] / m[8], m[3] / m[8], m[1] / m[8], m[4] / m[8], m[2] / m[8], m[5] / m[8]]))
               else: none(Affine)
  let interp = parseInterp(p.getStrOr("interpolation", "bicubic"))
  let isLone = lone.isSome
  s.editDo("Free Transform"):
    let sel = doc.selection
    if isLone:
      # A targeted unlinked mask, alpha channel or Quick Mask transforms by itself.
      if not h.isIdentity:
        let (surf, _) = targetSurface(doc, id, p)
        surf[] = if sel.isSome: warpGraySelected(surf[], sel.get, h, interp) else: warpGray(surf[], h, interp)
        if doc.selection.isSome:
          let ws = warpGray(doc.selection.get, h, ipBilinear)
          doc.selection = if ws.contentBounds.isEmpty: none(Surface) else: some(ws)
      return
    if id.isNone: raise other("no active layer")
    let lid = id.get
    let l0 = doc.findLayer(lid)
    let isGroup = not l0.isNil and l0[].isGroup
    let group = groupLocks(doc, lid)
    let l = doc.layerOrRaise(lid)
    transformLayer(if isGroup: none(Surface) else: sel, group, l[], h, affine, interp)
    if not h.isIdentity:
      # Type layers re-render from their new transform.
      let snapshot = doc
      let l2 = doc.findLayer(lid)
      if not l2.isNil: refreshRendered(snapshot, l2[])
      # The selection outline moves with the pixels.
      if doc.selection.isSome:
        let ws = warpGray(doc.selection.get, h, ipBilinear)
        doc.selection = if ws.contentBounds.isEmpty: none(Surface) else: some(ws)
  %*{"rect": rect}

proc tfHasLayerE(s: Session): string =
  let st = s.active
  if st.isNil or st.activeLayer.isNone: "no active layer" else: ""

proc transformSpecs*(): seq[CommandSpec] =
  @[cmd("edit.transform", "Free Transform", [], "",
        """{"layer":id?,"rect":[x0,y0,x1,y1]? (source frame; default = layer content ∩ selection),"quad":[[x,y]×4]? (where the frame's corners go, clockwise from top-left),"matrix":[a,b,c,d,e,f]? (affine alternative),"interpolation":"bicubic|bilinear|nearest"="bicubic","target":"pixels"|"mask"|"quickMask"|{"channel":i}=Channels panel target (an unlinked mask, an alpha channel or the Quick Mask transforms alone; a linked mask moves with its layer)}""",
        tfHasLayerE, transformCmd)]
