## Floating selection (port of `float_cmds.rs`): the selected pixels of a layer cut out and moved
## as a piece, without touching the document until the piece is dropped.
##
## - `select.float {"dx","dy","copy"}` cuts the selected pixels of the active layer into a floating
##   piece (the first time; `copy` leaves them and floats a duplicate) and moves it by whole pixels.
## - `select.drop` drops the piece into its layer and moves the selection with it, as one step.
## - Any other command drops the piece first, except Undo, which puts it back where it was cut.
##
## While floating, the document is unchanged; `displayed` gives the document with the piece at its
## offset, for the canvas. Part of the `engine/core` unit: included, not imported.

proc newCutParts*(doc: Document; layer: LayerId; copy: bool; background: array[4, float32]; mask: bool): CutParts =
  ## Split `layer` of `doc` by its selection. With `copy` the selected pixels stay in the layer
  ## too. Without, they leave a transparent hole, or on the Background (and in a mask) one filled
  ## with `background`.
  if doc.selection.isNone: raise other("no selection")
  let sel = doc.selection.get
  let l = doc.findLayer(layer)
  if l.isNil: raise noLayer(layer)
  var surf: Surface
  if mask:
    if l.mask.isNone: raise other("the layer has no mask")
    surf = l.mask.get.surface
  else:
    if l.content.kind != lkRaster: raise other("the layer has no pixels to move")
    surf = l.content.raster
  let fmt = surf.format
  let withAlpha = PixelFormat(mode: fmt.mode, sample: fmt.sample, alpha: true)
  let full = if fmt == withAlpha: surf else: surf.convert(withAlpha)
  # A mask has no alpha: it keeps its format, and the cut leaves background colour.
  var rest = if mask: surf else: full
  var piece = newSurface(withAlpha)
  let fill = not copy and (mask or isBackground(l[]))
  # A mask's untouched pixels (its default, e.g. white) move too, so take the whole canvas.
  let b = sel.contentBounds.intersect(if mask: doc.bounds else: surf.contentBounds)
  if not b.isEmpty:
    let n = withAlpha.channels
    let a = n - 1
    var rp = full.readRegion(b)
    var pp = rp
    let w = b.width
    var i = 0
    var k = 0
    while k + n <= pp.len:
      let cov = sel.sampleChannel(b.x0 + i mod w, b.y0 + i div w, 0)
      if cov <= 0:
        for c in 0 ..< n: pp[k + c] = 0
      else: pp[k + a] *= cov
      if not copy and not fill: rp[k + a] *= 1 - cov
      inc i
      k += n
    if not mask: rest.writeRegion(b, rp)
    if fill: tryFillSurface(rest, b, background, some(sel), true)
    rest.prune()
    piece.writeRegion(b, pp)
    piece.prune()
  CutParts(layer: layer, rest: rest, piece: piece, mask: mask)

proc place(c: CutParts; doc: var Document; dx, dy: int) =
  ## Put the piece, moved by (dx, dy), over the cut-out layer (or mask) of `doc`.
  let l = doc.layerOrRaise(c.layer)
  var surf: ptr Surface
  if c.mask:
    if l.mask.isNone: raise other("the layer has no mask")
    surf = addr l.mask.get.surface
  else:
    surf = l[].surfaceMut
    if surf.isNil: raise other("the layer has no pixels to move")
  var outp = c.rest
  let piece = translateSurface(c.piece, dx, dy)
  if c.mask: compositeOver(outp, piece, piece.contentBounds, none(Surface))
  else: compositeOver(outp, piece)
  surf[] = outp

proc moved*(c: CutParts; doc: Document; dx, dy: int): Document =
  ## `doc` with the piece moved by whole pixels (dx, dy) over the cut-out layer.
  result = doc
  c.place(result, dx, dy)

proc floatingOf*(st: DocState): Option[Floating] =
  ## The floating piece of `st`, if it is still valid (the document hasn't changed under it).
  if st.floating.isSome and st.floating.get.revision == st.revision and st.doc.selection.isSome: st.floating
  else: none(Floating)

proc displayed*(st: DocState; extra: (int, int)): Option[Document] =
  ## The document as shown while a piece floats `extra` further than its offset; none when
  ## nothing floats.
  let f = floatingOf(st)
  if f.isNone: return
  try: some(f.get.parts[].moved(st.doc[], f.get.offset[0] + extra[0], f.get.offset[1] + extra[1]))
  except EngineError: none(Document)

proc floatInt(p: JsonNode; key: string): int =
  let v = p.pget(key).asF64
  if v.isNone or v.get != v.get or abs(v.get) == Inf: 0 else: int(clamp(round(v.get), -1e7, 1e7))

proc lockedForFloat*(doc: Document; l: Layer): bool =
  ## Do `l`'s locks stop its selected pixels floating? The Background is position-locked, but its
  ## selected pixels move (Photoshop).
  let locks = doc.effectiveLocks(l.id)
  locks.all or (locks.position and not isBackground(l))

proc canFloatE(s: Session): string =
  let st = s.active
  if st.isNil: return "no document"
  if floatingOf(st[]).isSome: return ""
  if st.doc.selection.isNone: return "there is no selection"
  if st.activeLayer.isNone: return "no active layer"
  let l = st.doc[].findLayer(st.activeLayer.get)
  if l.isNil: return "no active layer"
  if l.content.kind != lkRaster: return "the active layer has no pixels to move"
  if lockedForFloat(st.doc[], l[]): return "layer \"" & l.name & "\" is locked"
  ""

proc satI32(v: int): int = clamp(v, int(int32.low), int(int32.high))

proc floatCmd(s: var Session; p: JsonNode): JsonNode =
  let (dx, dy) = (floatInt(p, "dx"), floatInt(p, "dy"))
  let copy = p.getBoolOr("copy", false)
  let background = s.tools.background
  let st = s.activeOrRaise()
  if floatingOf(st[]).isNone:
    if st.activeLayer.isNone: raise other("no active layer")
    let layer = st.activeLayer.get
    let parts = new(CutParts)
    parts[] = newCutParts(st.doc[], layer, copy, background, isMaskTarget(p))
    st.floating = some(Floating(layer: layer, offset: (0, 0), revision: st.revision, parts: parts))
  st.floating.get.offset = (satI32(st.floating.get.offset[0] + dx), satI32(st.floating.get.offset[1] + dy))
  let f = st.floating.get
  %*{"layer": uint64(f.layer), "offset": [f.offset[0], f.offset[1]]}

proc dropFloating*(s: var Session): JsonNode =
  ## Drop the floating piece into its layer and move the selection with it (one history step).
  let st = s.active
  if st.isNil: return %*{"dropped": false}
  let f = floatingOf(st[])
  st.floating = none(Floating)
  if f.isNone: return %*{"dropped": false}
  let (dx, dy) = f.get.offset
  if dx == 0 and dy == 0: return %*{"dropped": true, "offset": [0, 0]}
  let parts = f.get.parts
  s.editDo("Move"):
    parts[].place(doc, dx, dy)
    if doc.selection.isSome: doc.selection = some(translateSurface(doc.selection.get, dx, dy))
  %*{"dropped": true, "offset": [dx, dy]}

proc keepsFloating(id: string): bool =
  ## Commands that leave a floating piece floating: its own, and ones that don't touch documents.
  id in ["select.float", "select.drop"] or id.startsWith("view.") or id.startsWith("window.") or
    id.startsWith("help.") or id.startsWith("prefs.")

proc floatBeforeCommand*(s: var Session; id: string): Option[JsonNode] =
  ## Before any command: Undo puts a floating piece back (and is used up: returns its result);
  ## every other command but the floating ones drops it first.
  let st = s.active
  if st.isNil: return
  if floatingOf(st[]).isNone:
    st.floating = none(Floating)
    return
  if id in ["edit.undo", "edit.stepBackward"]:
    st.floating = none(Floating)
    return some(%*{"floating": "returned"})
  if keepsFloating(id): return
  discard dropFloating(s)

proc floatSpecs*(): seq[CommandSpec] =
  @[
    cmd("select.float", "Float Selection", [], "",
        """{"dx":px=0,"dy":px=0,"copy":bool=false,"target":"pixels"|"mask"="pixels" (mask: float the selected part of the layer's mask, the hole takes the background colour)} → {layer, offset} (cuts the selected pixels of the active layer into a floating piece the first time, or with copy leaves them and floats a duplicate; on the Background the hole takes the background colour; then moves it by whole pixels; dropped by select.drop or any other command, put back by edit.undo)""",
        canFloatE, floatCmd),
    cmd("select.drop", "Drop Floating Selection", [], "",
        """{} → {dropped, offset} (drops the floating piece into its layer and moves the selection with it: one history step)""",
        proc(s: Session): string = (if s.active.isNil: "no document" else: ""), runProc(dropFloating(s))),
  ]
