## Layer › New › Background from Layer, and the rule that keeps the Background at the bottom
## (port of `background_cmds.rs`). Part of the `engine/core` unit: included, not imported.

const BackgroundCmd = "layer.new.backgroundFromLayer"

proc backgroundId*(doc: Document): Option[LayerId] =
  ## The document's Background layer, if it has one (always the bottom layer).
  if doc.layers.len > 0 and isBackground(doc.layers[0]): some(doc.layers[0].id) else: none(LayerId)

proc keepBackgroundAtBottom*(bg: Option[LayerId]; doc: Document) =
  ## Refuses an edit that moved the Background off the bottom of the stack.
  if bg.isSome and (doc.layers.len == 0 or doc.layers[0].id != bg.get):
    raise other("the Background layer stays at the bottom of the stack (Layer › New › Layer from Background makes it a normal layer)")

proc convertible(l: Layer): string =
  if l.content.kind in {lkGroup, lkAdjustment}:
    l.content.article & " " & l.content.kindName & " layer can't become the Background"
  else: ""

proc canConvertE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if backgroundId(d.doc[]).isSome: return "the document already has a Background layer"
  if d.activeLayer.isNone: return "no active layer"
  let l = d.doc[].findLayer(d.activeLayer.get)
  if l.isNil: return "no active layer"
  convertible(l[])

proc backgroundFromLayer(s: var Session; p: JsonNode): JsonNode =
  let v = p.pget("layer")
  var id: LayerId
  if v.isNull: id = layerParam(s, p)
  else:
    let u = v.asU64
    if u.isNone: raise bad(BackgroundCmd, "`layer` must be a layer id")
    id = LayerId(u.get)
  let bg = s.tools.background
  s.editDo("Background From Layer"):
    if backgroundId(doc).isSome: raise other("the document already has a Background layer")
    let lp = doc.findLayer(id)
    if lp.isNil: raise noLayer(id)
    let why = convertible(lp[])
    if why.len > 0: raise other(why)
    # Render the layer alone, as it looks, over the background colour.
    var only = lp[]
    only.visible = true
    only.clipped = false
    let visible = lp.visible
    let fmt = doc.pixelFormat
    let rest = doc.layers
    doc.layers = @[only]
    let pixels = flattenToSurface(doc, fmt, some([bg[0], bg[1], bg[2]]))
    doc.layers = rest
    var outl = rasterLayer("Background", fmt)
    outl.id = id
    outl.visible = visible
    outl.locks.transparency = true
    outl.locks.position = true
    outl.pixelsMut[] = pixels
    if doc.remove(id).isNone: raise noLayer(id)
    doc.layers.insert(outl, 0)
    active = some(id)
  # The Background is the one targeted layer now.
  s.selectLayer(id)
  %*{"layer": id}

proc backgroundSpecs*(): seq[CommandSpec] =
  @[cmd(BackgroundCmd, "Background From Layer", ["Layer", "New"], "",
        """{"layer":id?} (the active layer by default; transparent pixels take the background colour and the layer drops to the bottom as the locked Background)""",
        canConvertE, backgroundFromLayer)]
