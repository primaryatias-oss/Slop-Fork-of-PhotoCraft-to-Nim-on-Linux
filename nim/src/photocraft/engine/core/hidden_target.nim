## Painting on or moving a hidden layer is refused, as in Photoshop (port of `hidden_target.rs`).
## Part of the `engine/core` unit: included, not imported.

const
  HiddenTargetMessage* = "Could not complete your request because the target layer is hidden."
  HiddenGated = ["paint.stroke", "paint.pencil", "paint.mixerBrush", "paint.colorReplacement", "paint.bucket",
    "paint.gradient", "paint.magicEraser", "paint.backgroundEraser", "paint.cloneStamp", "paint.healingBrush",
    "paint.spotHealing", "paint.patch", "paint.dodge", "paint.burn", "paint.sponge", "paint.blur", "paint.sharpen",
    "paint.smudge", "paint.historyBrush", "layer.translate"]

proc shownLayer(doc: Document; id: LayerId): bool =
  ## The layer and every group around it are visible.
  let path = doc.pathOf(id)
  if path.isNone: return false
  for n in 1 .. path.get.len:
    let l = doc.layerAt(path.get[0 ..< n])
    if l.isNone or not l.get.visible: return false
  true

proc hiddenTargetRefusal(s: Session; id: string; params: JsonNode): string =
  if id notin HiddenGated: return ""
  let p = injectTarget(s, id, params)
  if isChannelTarget(p): return ""
  let st = s.active
  if st.isNil: return ""
  let lv = p.pget("layer").asU64
  let layer = if lv.isSome: some(LayerId(lv.get)) else: st.activeLayer
  if layer.isNone: return ""
  if not st.doc[].findLayer(layer.get).isNil and not shownLayer(st.doc[], layer.get): HiddenTargetMessage else: ""
