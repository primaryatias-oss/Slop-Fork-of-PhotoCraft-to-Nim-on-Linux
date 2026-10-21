## Keyboard-only commands Photoshop has no menu items for, each an ordinary command with a default
## shortcut (rebindable, recordable, drivable by agents): layer navigation (`layer_nav_cmds.rs`),
## the brush keys (`brush_key_cmds.rs`), the fill keys (`fill_key_cmds.rs`) and stamping
## (`stamp_cmds.rs`).

import std/[json, options, math]
import ./core
import ../compose

# ---------------------------------------------------------------- layer_nav_cmds.rs

proc hasLayersE(s: Session): string =
  let st = s.active
  if st.isNil: return "no document open"
  if st.doc.layers.len == 0: "the document has no layers" else: ""

proc panelRows*(doc: Document): seq[LayerId] =
  ## The Layers panel's rows, top to bottom: each group before its children, collapsed groups
  ## without them.
  proc rec(layers: seq[Layer]; outp: var seq[LayerId]) =
    for i in countdown(layers.high, 0):
      let l = layers[i]
      outp.add l.id
      if l.content.kind == lkGroup and l.content.group.expanded: rec(l.content.group.children, outp)
  rec(doc.layers, result)

proc rowOf(doc: Document; rows: seq[LayerId]; id: LayerId): Option[int] =
  ## Where `id` is in `rows`; a layer inside a collapsed group counts as that group's row (the
  ## nearest enclosing one that has a row).
  let path = doc.pathOf(id)
  if path.isNone: return
  for n in countdown(path.get.len, 1):
    let l = doc.layerAt(path.get[0 ..< n])
    if l.isSome:
      let i = rows.find(l.get.id)
      if i >= 0: return some(i)

type NavStep = enum nsAbove, nsBelow, nsTop, nsBottom

proc navigate(s: var Session; step: NavStep; add: bool): JsonNode =
  ## Selects (or with `add`, adds to the selection) the row `step` away from the active layer. At
  ## the top or bottom row the selection stays as it is.
  let st = s.active
  if st.isNil: raise noDocument()
  let rows = panelRows(st.doc[])
  if rows.len == 0: raise other("the document has no layers")
  let last = rows.len - 1
  let current = (if st.activeLayer.isSome: rowOf(st.doc[], rows, st.activeLayer.get) else: none(int))
  let target =
    case step
    of nsTop: 0
    of nsBottom: last
    of nsAbove: (if current.isNone: 0 else: max(current.get - 1, 0))
    of nsBelow: (if current.isNone: last else: min(current.get + 1, last))
  let id = rows[target]
  if add:
    var sel = st[].selectedOrdered
    if id notin sel: sel.add id
    let anchor = st.layerAnchor
    setSelection(s, sel, some(id), anchor)
  else: s.selectLayer(id)
  let st2 = s.active
  if st2.isNil: raise noDocument()
  var ids: seq[uint64]
  for l in st2[].selectedOrdered: ids.add uint64(l)
  %*{"layer": uint64(id), "selected": ids}

proc layerNavSpecs*(): seq[CommandSpec] =
  const params = "{} (follows the Layers panel's rows; collapsed groups are one row)"
  @[cmd("layer.selectAbove", "Select Layer Above", [], "Alt+]", params, hasLayersE, runProc(navigate(s, nsAbove, false))),
    cmd("layer.selectBelow", "Select Layer Below", [], "Alt+[", params, hasLayersE, runProc(navigate(s, nsBelow, false))),
    cmd("layer.selectTop", "Select Top Layer", [], "Alt+.", params, hasLayersE, runProc(navigate(s, nsTop, false))),
    cmd("layer.selectBottom", "Select Bottom Layer", [], "Alt+,", params, hasLayersE, runProc(navigate(s, nsBottom, false))),
    cmd("layer.addAboveToSelection", "Add Layer Above to Selection", [], "Alt+Shift+]", params, hasLayersE,
        runProc(navigate(s, nsAbove, true))),
    cmd("layer.addBelowToSelection", "Add Layer Below to Selection", [], "Alt+Shift+[", params, hasLayersE,
        runProc(navigate(s, nsBelow, true)))]

# ---------------------------------------------------------------- brush_key_cmds.rs

const BrushKeyMaxSize* = 5000'f32 ## Photoshop's largest brush diameter, in pixels

proc sizeStep*(size: float32; up: bool): float32 =
  ## `[` / `]`: 25 % smaller or larger, and at least a pixel either way so small brushes still grow
  ## and shrink.
  let next = (if up: max(round(size * 1.25'f32), size + 1) else: min(round(size / 1.25'f32), size - 1))
  clamp(next, 1'f32, BrushKeyMaxSize)

proc hardnessStep*(hardness: float32; up: bool): float32 =
  ## ⇧[ / ⇧]: to the previous or next quarter (0, 25, 50, 75, 100 %).
  let quarter = round(hardness * 4)
  let next = (if up: quarter + 1 else: quarter - 1)
  clamp(next, 0'f32, 4'f32) / 4

proc brushKeySpecs*(): seq[CommandSpec] =
  @[cmd("tools.decreaseBrushSize", "Decrease Brush Size", [], "[", "{}", enabledProc(""),
        runProc(setBrush(s, %*{"brush": {"size": sizeStep(s.tools.brush.size, false)}}))),
    cmd("tools.increaseBrushSize", "Increase Brush Size", [], "]", "{}", enabledProc(""),
        runProc(setBrush(s, %*{"brush": {"size": sizeStep(s.tools.brush.size, true)}}))),
    cmd("tools.decreaseBrushHardness", "Decrease Brush Hardness", [], "Shift+[", "{}", enabledProc(""),
        runProc(setBrush(s, %*{"brush": {"hardness": hardnessStep(s.tools.brush.hardness, false)}}))),
    cmd("tools.increaseBrushHardness", "Increase Brush Hardness", [], "Shift+]", "{}", enabledProc(""),
        runProc(setBrush(s, %*{"brush": {"hardness": hardnessStep(s.tools.brush.hardness, true)}})))]

# ---------------------------------------------------------------- fill_key_cmds.rs

proc fillKeyE(s: Session): string =
  let f = findSpec("edit.fill")
  if f.isNil: return "Fill is unavailable"
  f[].enabled(s)

proc fillKey(s: var Session; p: JsonNode; cmdId: string; background, preserve: bool): JsonNode =
  ## `edit.fill` with `color` (the foreground or background colour) and Preserve Transparency.
  var q = newJObject()
  if p.isObject:
    for k, v in p.pairs: q[k] = v
  elif not p.isNull: raise bad(cmdId, "expected an object")
  # The colour and transparency handling are what the key means; don't let params override them.
  for k in ["color", "contents", "pattern", "preserveTransparency", "opacity"]:
    if q.hasKey(k): q.delete(k)
  let c = (if background: s.tools.background else: s.tools.foreground)
  q["color"] = %*[c[0], c[1], c[2], 1.0]
  q["preserveTransparency"] = %preserve
  let f = findSpec("edit.fill")
  if f.isNil: raise other("Fill is unavailable")
  f[].run(s, q)

proc fillKeySpecs*(): seq[CommandSpec] =
  const params = """{"layer":id?,"target":"pixels"|"mask"|"quickMask"|{"channel":i}?}"""
  @[cmd("edit.fillForeground", "Fill with Foreground Color", [], "Alt+Backspace", params, fillKeyE,
        runProc(fillKey(s, p, "edit.fillForeground", false, false))),
    cmd("edit.fillBackground", "Fill with Background Color", [], "Cmd+Backspace", params, fillKeyE,
        runProc(fillKey(s, p, "edit.fillBackground", true, false))),
    cmd("edit.fillForegroundPreserve", "Fill with Foreground Color, Preserve Transparency", [], "Alt+Shift+Backspace",
        params, fillKeyE, runProc(fillKey(s, p, "edit.fillForegroundPreserve", false, true))),
    cmd("edit.fillBackgroundPreserve", "Fill with Background Color, Preserve Transparency", [], "Cmd+Shift+Backspace",
        params, fillKeyE, runProc(fillKey(s, p, "edit.fillBackgroundPreserve", true, true)))]

# ---------------------------------------------------------------- stamp_cmds.rs

proc stampHasDocE(s: Session): string = (if s.active.isNil: "no document" else: "")

proc stampHasLayerE(s: Session): string =
  let st = s.active
  if st.isNil: return "no document"
  if st.activeLayer.isSome and not st.doc[].findLayer(st.activeLayer.get).isNil: "" else: "no active layer"

proc checkObject(p: JsonNode; cmdId: string) =
  if not (p.isObject or p.isNull): raise bad(cmdId, "expected an object")

proc withAlpha(doc: Document): PixelFormat =
  ## The document's pixel format with an alpha channel (a stamp has transparency).
  let f = doc.pixelFormat
  pixelFormat(f.mode, f.sample, true)

proc stampLayer(doc: Document; name: string): Layer =
  ## A new pixel layer holding the composite of `doc`'s visible layers, rendered in bands.
  let fmt = withAlpha(doc)
  result = rasterLayer(name, fmt)
  pixelsMut(result)[] = flattenToSurface(doc, fmt)

proc selectOnly(s: var Session; id: LayerId) =
  ## Make `id` the only selected layer and the active one.
  let st = s.active
  if st.isNil: return
  st.selectedLayers = @[id]
  st.activeLayer = some(id)
  st.layerAnchor = some(id)
  fixSelection(st[])

proc stampVisible(s: var Session; p: JsonNode): JsonNode =
  checkObject(p, "layer.stampVisible")
  let id = s.editWith("Stamp Visible", proc(doc: var Document; active: var Option[LayerId]): LayerId =
    var any = false
    for (path, _, l) in doc.walk:
      if l.isGroup: continue
      var vis = true
      for n in 1 .. path.len:
        let a = doc.layerAt(path[0 ..< n])
        if a.isNone or not a.get.visible:
          vis = false
          break
      if vis:
        any = true
        break
    if not any: raise other("there are no visible layers to stamp")
    let layer = stampLayer(doc, doc.nextLayerName("Layer"))
    result = doc.insertAbove(active, layer)
    active = some(result))
  selectOnly(s, id)
  %*{"layer": uint64(id)}

proc stampSelected(s: var Session; sel: seq[LayerId]): JsonNode =
  ## Several layers selected: a copy of them merged into a new layer above the top one.
  let id = s.editWith("Stamp Layers", proc(doc: var Document; active: var Option[LayerId]): LayerId =
    let ids = topLevel(doc, sel)
    if ids.len == 0: raise other("no layers selected")
    let top = ids[^1]
    var solo = doc
    solo.layers = @[]
    for i in ids:
      let l = doc.findLayer(i)
      if not l.isNil and l[].visible: solo.layers.add l[]
    if solo.layers.len == 0: raise other("the selected layers are all hidden")
    let tl = doc.findLayer(top)
    let name = (if tl.isNil: "Layer" else: tl[].name) & " (merged)"
    let layer = stampLayer(solo, name)
    result = doc.insertAbove(some(top), layer)
    active = some(result))
  selectOnly(s, id)
  %*{"layer": uint64(id)}

proc stampDown(s: var Session; p: JsonNode): JsonNode =
  checkObject(p, "layer.stampDown")
  let sel = selected(s)
  if sel.len >= 2: return stampSelected(s, sel)
  let st = s.active
  if st.isNil: raise noDocument()
  if st.activeLayer.isNone: raise other("no active layer")
  let id = st.activeLayer.get
  s.edit("Stamp Down", proc(doc: var Document; active: var Option[LayerId]) =
    let path = doc.pathOf(id)
    if path.isNone or path.get.len == 0: raise noLayer(id)
    let idx = path.get[^1]
    if idx == 0: raise other("there is no layer below to stamp into")
    var below = path.get[0 ..< path.get.len - 1]
    below.add idx - 1
    let lo = doc.layerAt(below)
    if lo.isNone: raise noLayer(id)
    let lower = lo.get
    let up = doc.findLayer(id)
    if up.isNil: raise noLayer(id)
    let upper = up[]
    if lower.content.kind != lkRaster: raise other("the layer below is not a pixel layer")
    let locks = doc.effectiveLocks(lower.id)
    if locks.all or locks.pixels: raise other("the layer below is locked")
    if not upper.visible: raise other("the layer is hidden")
    # The two layers on their own, composited in bands; the lower one keeps its id, name and blend
    # mode, as Merge Down does.
    var tmp = doc
    var base = lower
    base.visible = true
    tmp.layers = @[base, upper]
    let f = doc.pixelFormat
    let fmt = (if lower.locks.transparency: f else: pixelFormat(f.mode, f.sample, true))
    let surf = flattenToSurface(tmp, fmt)
    var merged = newLayer(lower.name, LayerContent(kind: lkRaster, raster: surf))
    merged.id = lower.id
    merged.blend = lower.blend
    merged.visible = lower.visible
    merged.locks = lower.locks
    merged.opacity = 1
    let slot = doc.layerAtPtr(below)
    if slot.isNil: raise noLayer(lower.id)
    slot[] = merged)
  %*{"layer": uint64(id)}

proc stampSpecs*(): seq[CommandSpec] =
  @[cmd("layer.stampVisible", "Stamp Visible", [], "Cmd+Alt+Shift+E",
        """{} → {layer} (every visible layer merged into a new layer above the active one; the originals stay)""",
        stampHasDocE, runProc(stampVisible(s, p))),
    cmd("layer.stampDown", "Stamp Down", [], "Cmd+Alt+E",
        """{} → {layer} (a copy of the active layer merged into the pixel layer below; several selected: into a new layer above them)""",
        stampHasLayerE, runProc(stampDown(s, p)))]

proc stampAltVariant*(id: string): Option[string] =
  ## The ⌥ variant of a Layer menu merge command (⌥ + Merge Down / Merge Visible / Merge Layers).
  case id
  of "layer.mergeVisible": some("layer.stampVisible")
  of "layer.mergeDown", "layer.mergeLayers": some("layer.stampDown")
  else: none(string)
