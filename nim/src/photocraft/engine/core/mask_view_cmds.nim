## View › layer mask: ⌥-click a layer-mask thumbnail shows the mask as a grayscale image;
## ⇧⌥-click (or `\`) shows it as a rubylith overlay (port of `mask_view_cmds.rs`). View state on
## `ChannelView.layerMask`: no history step, and pixel commands without a `target` edit the mask
## while it is shown. Part of the `engine/core` unit: included, not imported.

const
  MaskViewId* = "view.layerMask"
  MaskOverlayId* = "layer.toggleMaskOverlay"

proc maskViewBad(msg: string): ref EngineError = bad(MaskViewId, msg)

proc currentMaskView*(st: DocState): Option[LayerMaskView] =
  ## The mask view of `st` when it is live: its layer is the active layer and still has a mask.
  let v = st.channelView.layerMask
  if v.isNone: return
  let l = st.doc[].findLayer(v.get.layer)
  if st.activeLayer == some(v.get.layer) and not l.isNil and l.mask.isSome: v else: none(LayerMaskView)

proc fixMaskView*(st: var DocState) =
  ## Drops a mask view that no longer applies.
  if st.channelView.layerMask.isSome and currentMaskView(st).isNone:
    st.channelView.layerMask = none(LayerMaskView)

proc maskModeName(v: Option[LayerMaskView]): string =
  if v.isNone: "off"
  elif v.get.mode == mvGray: "gray"
  else: "overlay"

proc maskViewJson*(st: DocState): JsonNode =
  ## The view as JSON (`{"layer","mode"}`, or null when off); part of `document.inspect`.
  let v = currentMaskView(st)
  if v.isSome: %*{"layer": v.get.layer, "mode": maskModeName(v)} else: newJNull()

proc setMaskView(s: var Session; p: JsonNode): JsonNode =
  let m = p.pget("mode")
  let mode = if m.isNull: "toggleGray"
             elif m.kind == JString: m.str
             else: raise maskViewBad("`mode` must be a string")
  let st = s.activeOrRaise()
  let lv = p.pget("layer")
  var layer: LayerId
  if lv.isNull:
    if st.activeLayer.isNone: raise maskViewBad("no layer given and no active layer")
    layer = st.activeLayer.get
  else:
    let u = lv.asU64
    if u.isNone: raise maskViewBad("`layer` must be a layer id")
    layer = LayerId(u.get)
  let l = st.doc[].findLayer(layer)
  if l.isNil: raise noLayer(layer)
  let hasMask = l.mask.isSome
  let name = l.name
  let cur = currentMaskView(st[])
  let shown = if cur.isSome and cur.get.layer == layer: some(cur.get.mode) else: none(MaskViewMode)
  var want: Option[MaskViewMode]
  case mode
  of "off": want = none(MaskViewMode)
  of "gray": want = some(mvGray)
  of "overlay": want = some(mvOverlay)
  of "toggleGray": want = (if shown != some(mvGray): some(mvGray) else: none(MaskViewMode))
  of "toggleOverlay": want = (if shown != some(mvOverlay): some(mvOverlay) else: none(MaskViewMode))
  else: raise maskViewBad("unknown mode `" & mode & "` (off|gray|overlay|toggleGray|toggleOverlay)")
  if want.isSome and not hasMask: raise maskViewBad("layer \"" & name & "\" has no layer mask")
  # ⌥-clicking another layer's mask selects that layer, as in Photoshop.
  if want.isSome and s.activeOrRaise().activeLayer != some(layer): s.selectLayer(layer)
  let st2 = s.activeOrRaise()
  let next = if want.isSome: some(LayerMaskView(layer: layer, mode: want.get)) else: none(LayerMaskView)
  # "off" for a layer whose mask isn't the one shown leaves the other view alone.
  let applies = next.isSome or (st2.channelView.layerMask.isSome and st2.channelView.layerMask.get.layer == layer)
  if applies and st2.channelView.layerMask != next:
    st2.channelView.layerMask = next
    fixView(st2[])
    let clean = st2.savedRevision == st2.revision
    inc st2.revision
    if clean: st2.savedRevision = st2.revision
    st2.lastDamage = some(EmptyRect)
  let v = currentMaskView(st2[])
  %*{"layer": layer, "mode": maskModeName(if v.isSome and v.get.layer == layer: v else: none(LayerMaskView))}

proc activeHasMaskE(s: Session): string =
  let st = s.active
  if st.isNil: return "no document open"
  if st.activeLayer.isNone: return "no active layer"
  let l = st.doc[].findLayer(st.activeLayer.get)
  if l.isNil: return "no active layer"
  if l.mask.isSome: "" else: "layer \"" & l.name & "\" has no layer mask"

proc toggleMaskOverlay(s: var Session; p: JsonNode): JsonNode =
  var layer = newJNull()
  if p.isObject:
    let l = p.pget("layer")
    if not l.isNil: layer = l
  elif not p.isNull: raise bad(MaskOverlayId, "expected an object")
  setMaskView(s, %*{"layer": layer, "mode": "toggleOverlay"})

proc maskViewSpecs*(): seq[CommandSpec] =
  @[
    cmd(MaskViewId, "View Layer Mask", [], "",
        """{"layer":id?,"mode":"off"|"gray"|"overlay"|"toggleGray"|"toggleOverlay"="toggleGray"} (gray: the mask alone, ⌥-click its thumbnail; overlay: red rubylith over the composite, ⇧⌥-click or `\`; view state, not an undo step; painting while shown paints the mask)""",
        hasDocE, setMaskView, journal = false),
    cmd(MaskOverlayId, "Toggle Layer Mask Overlay", [], "\\",
        """{"layer":id?} (shows or hides the active layer's mask as a 50% red rubylith over the composite, the `\` key; view state, not an undo step; painting while shown paints the mask)""",
        activeHasMaskE, toggleMaskOverlay, journal = false),
  ]
