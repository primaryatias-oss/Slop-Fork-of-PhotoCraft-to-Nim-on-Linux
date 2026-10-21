## Layer › New › Layer: Photoshop's New Layer options (name, Use Previous Layer to Create
## Clipping Mask, color label, mode, opacity, Fill with neutral color), in one history step
## (port of `new_layer.rs`). Part of the `engine/core` unit: included, not imported.

const NewLayerCmd = "layer.new.layer"

proc neutralColor*(mode: BlendMode): Option[(array[3, float32], string)] =
  ## The color that leaves everything below unchanged in `mode`, and its name.
  case mode
  of bmDarken, bmMultiply, bmColorBurn, bmLinearBurn, bmDarkerColor, bmDivide: some(([1'f32, 1, 1], "white"))
  of bmLighten, bmScreen, bmColorDodge, bmLinearDodge, bmLighterColor, bmDifference, bmExclusion, bmSubtract:
    some(([0'f32, 0, 0], "black"))
  of bmOverlay, bmSoftLight, bmHardLight, bmVividLight, bmLinearLight, bmPinLight: some(([0.5'f32, 0.5, 0.5], "50% gray"))
  else: none((array[3, float32], string))

proc newLayerRun*(s: var Session; p: JsonNode): JsonNode =
  ## `layer.new.layer`: a pixel layer above the active one, joining the clip group it lands in.
  proc flag(k: string): bool =
    let v = p.pget(k)
    if v.isNull: return false
    let b = v.asBool
    if b.isNone: raise bad(NewLayerCmd, "`" & k & "` must be true or false")
    b.get
  let clip = flag("clipToPrevious")
  let fillNeutral = flag("fillNeutral")
  var mode = bmNormal
  let mv = p.pget("mode")
  if not mv.isNull:
    if mv.kind != JString: raise bad(NewLayerCmd, "`mode` must be a blend mode name")
    let m = blendFromStr(mv.str)
    if m.isNone or m.get == bmPassThrough: raise bad(NewLayerCmd, "unknown layer blend mode `" & mv.str & "`")
    mode = m.get
  var opacity = 1'f32
  let ov = p.pget("opacity")
  if not ov.isNull:
    let o = ov.asF64
    if o.isNone or o.get != o.get or abs(o.get) == Inf: raise bad(NewLayerCmd, "`opacity` must be a number from 0 to 1")
    opacity = float32(clamp(o.get, 0.0, 1.0))
  var label = lcNone
  let cv = p.pget("color")
  if not cv.isNull:
    let c = if cv.kind == JString: labelColorFromId(cv.str) else: none(LabelColor)
    if c.isNone: raise bad(NewLayerCmd, "`color` must be a layer color id, not " & jsonText(cv))
    label = c.get
  var neutral = none(array[3, float32])
  if fillNeutral:
    let n = neutralColor(mode)
    if n.isNone: raise bad(NewLayerCmd, mode.label & " has no neutral color to fill with")
    neutral = some(n.get[0])
  var name = none(string)
  let nv = p.pget("name")
  if not nv.isNull:
    if nv.kind != JString: raise bad(NewLayerCmd, "`name` must be a string")
    name = some(nv.str)
  let id = s.editGet("New Layer", LayerId):
    var layer = rasterLayer(if name.isSome: name.get else: doc.nextLayerName("Layer"), doc.pixelFormat)
    layer.blend = mode
    layer.opacity = opacity
    layer.label = label
    if neutral.isSome:
      let rgbv = neutral.get
      let bounds = doc.bounds
      let surface = layer.surfaceMut
      let px = fromRgba(surface[].format, [rgbv[0], rgbv[1], rgbv[2], 1'f32])
      surface[].fillRect(bounds, px)
    let id = doc.insertAbove(active, layer)
    settle(doc, [id], ClipBefore(), clip)
    active = some(id)
    id
  noteInertInsert(s, id)
  %*{"layer": id}
