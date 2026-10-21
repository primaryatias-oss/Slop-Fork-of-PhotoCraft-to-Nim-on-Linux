## Small Layers-panel and Layer-menu commands, one Rust module each:
## `layer_label_cmds.rs` (Layer Color), `layer_mask_props_cmds.rs` (mask density and feather),
## `group_view_cmds.rs` (expand/collapse groups), `fx_view_cmds.rs` (expand/collapse effects
## lists), `fx_visibility_cmds.rs` (the eyes on effects rows) and `rasterize_style_cmds.rs`
## (Layer › Rasterize › Layer Style).

import std/[json, options, sequtils]
import ./core
import ../compose
import ../compose/effects as ceffects

proc isFin(v: float64): bool {.inline.} = v == v and abs(v) != Inf

proc optBool(p: JsonNode; key, cmdId: string): Option[bool] =
  let v = p.pget(key)
  if v.isNull: return none(bool)
  let b = v.asBool
  if b.isNone: raise bad(cmdId, "`" & key & "` must be true or false")
  b

proc docE(s: Session): string = (if s.active.isNil: "no document open" else: "")

# ---------------------------------------------------------------- layer_label_cmds.rs

const LabelCmd = "layer.setLabelColor"

proc labelE(s: Session): string =
  let st = s.active
  if st.isNil: return "no document open"
  if st[].selectedOrdered.len == 0: "no layer selected" else: ""

proc setLabel(s: var Session; p: JsonNode): JsonNode =
  ## Undoable layer colour labels. A group applies the label to its existing descendants, without
  ## changing pixels, visibility, locks or the layer selection.
  if not p.isObject: raise bad(LabelCmd, "expected an object")
  let c = p.pget("color").asStr
  let color = (if c.isSome: labelColorFromId(c.get) else: none(LabelColor))
  if color.isNone:
    raise bad(LabelCmd, "`color` must be a layer colour id (none, red, orange, yellow, green, seafoam, blue, indigo, magenta, fuchsia, violet, gray)")
  let st = s.active
  if st.isNil: raise noDocument()
  var roots: seq[LayerId]
  let lv = p.pget("layer")
  if not lv.isNil:
    let id = lv.asU64
    if id.isNone: raise bad(LabelCmd, "`layer` must be an unsigned integer")
    roots = @[LayerId(id.get)]
  else: roots = st[].selectedOrdered
  if roots.len == 0: raise bad(LabelCmd, "no layer selected")
  let walk = st.doc[].walk
  var paths: seq[LayerPath]
  for id in roots:
    var found = false
    for (path, _, l) in walk:
      if l.id == id:
        paths.add path
        found = true
        break
    if not found: raise noLayer(id)
  var changed = false
  var targets: seq[(LayerPath, LayerId)]
  for (path, _, l) in walk:
    var inside = false
    for root in paths:
      if path.len >= root.len and path[0 ..< root.len] == root:
        inside = true
        break
    if inside:
      if l.label != color.get: changed = true
      targets.add (path, l.id)
  if changed:
    let col = color.get
    s.edit("Layer Color", proc(doc: var Document; active: var Option[LayerId]) =
      for (path, id) in targets:
        let l = doc.layerAtPtr(path)
        if l.isNil: raise noLayer(id)
        l.label = col)
    let st2 = s.active
    if not st2.isNil: st2.lastDamage = some(EmptyRect)
  %*{"color": color.get.id, "layers": targets.mapIt(uint64(it[1])), "changed": changed}

# ---------------------------------------------------------------- layer_mask_props_cmds.rs

const MaskEditCmd* = "layer.layerMask.edit"

proc maskNumber(p: JsonNode; name: string; max: float64; maxText: string): Option[float32] =
  let v = p.pget(name)
  if v.isNil: return none(float32)
  let n = v.asF64
  if n.isSome and isFin(n.get) and n.get >= 0 and n.get <= max: return some(float32(n.get))
  raise bad(MaskEditCmd, "`" & name & "` must be a finite number within 0.." & maxText)

proc maskEdit(s: var Session; p: JsonNode): JsonNode =
  ## Non-destructive pixel-mask properties: density and feather leave the mask pixels intact.
  if not p.isObject: raise bad(MaskEditCmd, "params must be an object")
  let d0 = maskNumber(p, "density", 100, "100")
  let density0 = (if d0.isSome: some(d0.get / 100'f32) else: none(float32))
  let feather0 = maskNumber(p, "feather", 1000, "1000")
  if density0.isNone and feather0.isNone: raise bad(MaskEditCmd, "density or feather is required")
  let st = s.active
  if st.isNil: raise noDocument()
  let dv = p.pget("document")
  if not dv.isNil and dv.asU64 != some(uint64(st.doc.id)): raise bad(MaskEditCmd, "the target document is no longer active")
  var id: LayerId
  let lv = p.pget("layer")
  if not lv.isNil:
    let v = lv.asU64
    if v.isNone: raise bad(MaskEditCmd, "`layer` must be an unsigned layer id")
    id = LayerId(v.get)
  else:
    if st.activeLayer.isNone: raise bad(MaskEditCmd, "no layer given and no active layer")
    id = st.activeLayer.get
  let layer = st.doc[].findLayer(id)
  if layer.isNil: raise noLayer(id)
  if layer[].mask.isNone: raise bad(MaskEditCmd, "the target layer has no pixel mask")
  let mask = layer[].mask.get
  let density = density0.get(mask.density)
  let feather = feather0.get(mask.feather)
  if (density, feather) != (mask.density, mask.feather):
    s.edit("Edit Layer Mask", proc(doc: var Document; active: var Option[LayerId]) =
      let l = doc.findLayer(id)
      if l.isNil: raise noLayer(id)
      if l[].mask.isNone: raise bad(MaskEditCmd, "the target layer has no pixel mask")
      l[].mask.get.density = density
      l[].mask.get.feather = feather)
  %*{"layer": uint64(id), "density": density * 100'f32, "feather": feather}

# ---------------------------------------------------------------- group_view_cmds.rs

const ExpandCmd = "layer.setExpanded"

proc hasGroupE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  for (_, _, l) in d.doc[].walk:
    if l.isGroup: return ""
  "the document has no layer groups"

proc setAllExpanded(layers: var seq[Layer]; expanded: bool): int =
  for l in layers.mitems:
    if l.content.kind == lkGroup:
      l.content.group.expanded = expanded
      result += 1 + setAllExpanded(l.content.group.children, expanded)

proc layerParam(p: JsonNode; all: bool; st: ptr DocState; cmdId: string): Option[LayerId] =
  ## `layer` (none with `all` and no layer; else the active layer when absent).
  let v = p.pget("layer")
  if v.isNull:
    if all: return none(LayerId)
    if st.activeLayer.isNone: raise bad(cmdId, "no layer given and no active layer")
    return st.activeLayer
  let id = v.asU64
  if id.isNone: raise bad(cmdId, "`layer` must be a layer id")
  some(LayerId(id.get))

proc setGroupExpanded(s: var Session; p: JsonNode): JsonNode =
  ## Open or close a group (no `expanded`: toggle). With `all`, every group in the document takes
  ## the new state; without a layer, `all` toggles from "any group open". A view change: no
  ## history step, and a clean document stays clean.
  let want = optBool(p, "expanded", ExpandCmd)
  let all = optBool(p, "all", ExpandCmd).get(false)
  let st = s.active
  if st.isNil: raise noDocument()
  let layer = layerParam(p, all, st, ExpandCmd)
  var current = false
  if layer.isSome:
    let l = st.doc[].findLayer(layer.get)
    if l.isNil: raise noLayer(layer.get)
    if l[].content.kind != lkGroup: raise bad(ExpandCmd, "layer " & $uint64(layer.get) & " is not a group")
    current = l[].content.group.expanded
  else:
    for (_, _, l) in st.doc[].walk:
      if l.content.kind == lkGroup and l.content.group.expanded: current = true
  let expanded = want.get(not current)
  # Copy-on-write: history snapshots keep their own state.
  var doc = st.doc[]
  var changed = 0
  if all: changed = setAllExpanded(doc.layers, expanded)
  else:
    if layer.isNone: raise bad(ExpandCmd, "no layer")
    let l = doc.findLayer(layer.get)
    if l.isNil or l[].content.kind != lkGroup: raise noLayer(layer.get)
    l[].content.group.expanded = expanded
    changed = 1
  let r = DocRef()
  r[] = doc
  st.doc = r
  %*{"expanded": expanded, "groups": changed}

# ---------------------------------------------------------------- fx_view_cmds.rs

const FxExpandCmd = "layer.setEffectsExpanded"
const FxVisibleCmd = "layer.setEffectsVisible"

proc hasEffectsE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  for (_, _, l) in d.doc[].walk:
    if l.effects.items.len > 0: return ""
  "no layer has effects"

proc setEffectsExpanded(s: var Session; p: JsonNode): JsonNode =
  ## Show or hide a layer's effects list (no `expanded`: toggle). With `all`, every layer with
  ## effects takes the new state. View state: no history step.
  let want = optBool(p, "expanded", FxExpandCmd)
  let all = optBool(p, "all", FxExpandCmd).get(false)
  let st = s.active
  if st.isNil: raise noDocument()
  let layer = layerParam(p, all, st, FxExpandCmd)
  var withFx: seq[LayerId]
  for (_, _, l) in st.doc[].walk:
    if l.effects.items.len > 0: withFx.add l.id
  var current = false
  if layer.isSome:
    let l = st.doc[].findLayer(layer.get)
    if l.isNil: raise noLayer(layer.get)
    if l[].effects.items.len == 0: raise bad(FxExpandCmd, "layer " & $uint64(layer.get) & " has no effects")
    current = layer.get notin st.fxCollapsed
  else:
    for id in withFx:
      if id notin st.fxCollapsed: current = true
  let expanded = want.get(not current)
  let targets = (if all: withFx elif layer.isSome: @[layer.get] else: newSeq[LayerId]())
  # Forget ids no longer in the document so the list can't grow without bound.
  var kept: seq[LayerId]
  for id in st.fxCollapsed:
    if not st.doc[].findLayer(id).isNil and id notin targets: kept.add id
  st.fxCollapsed = kept
  if not expanded: st.fxCollapsed.add targets
  %*{"expanded": expanded, "layers": targets.len}

# ---------------------------------------------------------------- fx_visibility_cmds.rs

proc setEffectsVisible(s: var Session; p: JsonNode): JsonNode =
  ## Show or hide a layer's effects (no `visible`: toggle). Without `index` it is the whole list
  ## (the "Effects" row); with it, the effect at that position in the layer's list. One history
  ## step per toggle.
  var want = none(bool)
  let vv = p.pget("visible")
  if not vv.isNull:
    let b = vv.asBool
    if b.isNone: raise bad(FxVisibleCmd, "`visible` must be true or false")
    want = b
  var index = none(int)
  let iv = p.pget("index")
  if not iv.isNull:
    let i = iv.asU64
    if i.isNone or i.get > uint64(high(int)): raise bad(FxVisibleCmd, "`index` must be an effect position (0, 1, …)")
    index = some(int(i.get))
  let st = s.active
  if st.isNil: raise noDocument()
  var id: LayerId
  let lv = p.pget("layer")
  if lv.isNull:
    if st.activeLayer.isNone: raise bad(FxVisibleCmd, "no layer given and no active layer")
    id = st.activeLayer.get
  else:
    let v = lv.asU64
    if v.isNone: raise bad(FxVisibleCmd, "`layer` must be a layer id")
    id = LayerId(v.get)
  let l = st.doc[].findLayer(id)
  if l.isNil: raise noLayer(id)
  if l[].effects.items.len == 0: raise bad(FxVisibleCmd, "layer " & $uint64(id) & " has no effects")
  var current: bool
  var name: string
  if index.isSome:
    if index.get >= l[].effects.items.len:
      raise bad(FxVisibleCmd, "layer " & $uint64(id) & " has no effect at index " & $index.get)
    current = l[].effects.items[index.get].enabled
    name = l[].effects.items[index.get].label
  else:
    current = l[].effects.enabled
    name = "Layer Effects"
  let visible = want.get(not current)
  if visible != current:
    let label = (if visible: "Show " else: "Hide ") & name
    s.edit(label, proc(doc: var Document; active: var Option[LayerId]) =
      let l = doc.findLayer(id)
      if l.isNil: raise noLayer(id)
      if index.isSome:
        if index.get >= l[].effects.items.len:
          raise bad(FxVisibleCmd, "layer " & $uint64(id) & " has no effect at index " & $index.get)
        l[].effects.items[index.get].setEnabled(visible)
      else: l[].effects.enabled = visible)
  %*{"layer": uint64(id), "index": (if index.isSome: %index.get else: newJNull()), "visible": visible}

# ---------------------------------------------------------------- rasterize_style_cmds.rs

const RasterizeStyleCmd = "layer.rasterize.layerStyle"

proc canBake*(l: Layer): bool =
  ## Whether Rasterize › Layer Style applies to `l`: visible effects on a layer whose content can
  ## become pixels (not a group or an adjustment layer).
  ceffects.hasEffects(l) and l.content.kind notin {lkGroup, lkAdjustment}

proc rasterizeStyleE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  var ids = d[].selectedOrdered
  if d.activeLayer.isSome: ids.add d.activeLayer.get
  for id in ids:
    let l = d.doc[].findLayer(id)
    if not l.isNil and canBake(l[]): return ""
  "no selected layer has layer effects to rasterize"

proc bakeOne(s: var Session; id: LayerId; key: string): bool =
  ## Bake one layer (type, shape, fill and Smart Object content is rasterized first, in the same
  ## history step). Returns false when the layer has nothing to bake.
  let d = s.active
  if d.isNil: raise noDocument()
  let l0 = d.doc[].findLayer(id)
  if l0.isNil or not canBake(l0[]): return false
  let kind = kindOf(s, id)
  if kind.isSome: discard rasterizeOne(s, id, kind, key)
  s.coalesceRequest = some(key)
  try:
    s.edit("Rasterize Layer Style", proc(doc: var Document; active: var Option[LayerId]) =
      let lp = doc.findLayer(id)
      if lp.isNil: raise noLayer(id)
      let l = lp[]
      if l.content.kind != lkRaster: raise other("the layer could not be rasterized")
      let canvas = doc.bounds
      let fmt = doc.pixelFormat
      # The pixels (also those past the canvas) and everything the effects reach from them.
      let area = compose.layerBounds(l, canvas).inflate(max(ceffects.margin(l), 0))
      var alone = l
      alone.visible = true
      alone.opacity = 1
      alone.blend = bmNormal
      alone.clipped = false
      # These depend on the layers beneath and stay on the layer, applied to the baked pixels.
      alone.advanced.knockout = koNone
      alone.blendIf = BlendIf()
      alone.excludedChannels = 0
      var data: seq[float32]
      if not area.isEmpty:
        # Render the layer on its own with the document's settings, then put the real stack back.
        let stack = doc.layers
        doc.layers = @[alone]
        let buf = compose.render(doc, area)
        doc.layers = stack
        let n = fmt.channels
        data = newSeq[float32](buf.px.len * n)
        for i, px in buf.px: discard fromRgbaInto(fmt, px, data.toOpenArray(i * n, i * n + n - 1))
      var px = newSurface(fmt)
      if not area.isEmpty: px.writeRegion(area, data)
      px.prune()
      let t = doc.findLayer(id)
      if t.isNil: raise noLayer(id)
      t[].content = LayerContent(kind: lkRaster, raster: px)
      t[].fillCache = none(FillCache)
      # Fill opacity, the masks and the effects are in the pixels now.
      t[].fillOpacity = 1
      t[].mask = none(LayerMask)
      t[].vectorMask = none(VectorMask)
      t[].effects.items.setLen(0)
      t[].effects.psdRaw = nil
      t[].effects.reference = none((float64, float64))
      t[].effects.enabled = true
      t[].advanced.blendInterior = false
      t[].advanced.layerMaskHidesEffects = false
      t[].advanced.vectorMaskHidesEffects = false)
  finally:
    s.coalesceRequest = none(string)
  true

proc rasterizeLayerStyle(s: var Session; p: JsonNode): JsonNode =
  ## With a layer, just that one; otherwise every selected layer with effects, in one history step.
  let ids = targets(s, p)
  let many = ids.len > 1
  let key = stepKey(s, "rasterizeLayerStyle")
  var done: seq[uint64]
  for id in ids:
    if bakeOne(s, id, key): done.add uint64(id)
  if done.len == 0:
    raise other(if many: "none of the selected layers has layer effects" else: "the layer has no layer effects")
  let st = s.active
  var active = done[0]
  if not st.isNil and st.activeLayer.isSome and uint64(st.activeLayer.get) in done: active = uint64(st.activeLayer.get)
  %*{"layer": active, "layers": done}

# ---------------------------------------------------------------- specs

proc layerLabelSpecs*(): seq[CommandSpec] =
  @[cmd(LabelCmd, "Layer Color", [], "",
        """{"color":"none|red|orange|yellow|green|seafoam|blue|indigo|magenta|fuchsia|violet|gray","layer":id?} (no layer: selected layers; groups include descendants)""",
        labelE, runProc(setLabel(s, p)))]

proc rasterizeStyleSpecs*(): seq[CommandSpec] =
  @[cmd(RasterizeStyleCmd, "Layer Style", ["Layer", "Rasterize"], "",
        """{"layer":id?} (no layer: every selected layer with effects; bakes the effects, fill opacity and masks into the pixels)""",
        rasterizeStyleE, runProc(rasterizeLayerStyle(s, p)))]

proc groupViewSpecs*(): seq[CommandSpec] =
  @[cmd(ExpandCmd, "Expand/Collapse Group", [], "",
        """{"layer":id?,"expanded":bool?,"all":bool?} (no expanded: toggle; all: every group; not an undo step)""",
        hasGroupE, runProc(setGroupExpanded(s, p)), journal = false)]

proc fxViewSpecs*(): seq[CommandSpec] =
  @[cmd(FxExpandCmd, "Expand/Collapse Effects", [], "",
        """{"layer":id?,"expanded":bool?,"all":bool?} (no expanded: toggle; all: every layer with effects; view state, not an undo step)""",
        hasEffectsE, runProc(setEffectsExpanded(s, p)), journal = false)]

proc fxVisibilitySpecs*(): seq[CommandSpec] =
  @[cmd(FxVisibleCmd, "Show/Hide Effects", [], "",
        """{"layer":id?,"index":n?,"visible":bool?} (no index: all of the layer's effects, the "Effects" row; index: the effect at that position in the layer's list; no visible: toggle)""",
        hasEffectsE, runProc(setEffectsVisible(s, p)))]

proc layerMaskPropsSpecs*(): seq[CommandSpec] =
  @[cmd(MaskEditCmd, "Edit Layer Mask", [], "",
        """{"layer":id? (default active),"document":id? (reject a stale dialog),"density":0..100?,"feather":0..1000? (pixels)} → {layer,density,feather}; at least one property required; mask pixels are preserved""",
        docE, runProc(maskEdit(s, p)))]
