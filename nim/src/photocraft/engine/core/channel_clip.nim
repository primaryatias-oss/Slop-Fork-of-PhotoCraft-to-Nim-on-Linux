## Copy, Cut and Paste with a single colour or alpha channel targeted (port of `channel_clip.rs`).
## Part of the `engine/core` unit: included, not imported.

proc pasteColorTarget*(s: Session; p: JsonNode): Option[int] =
  ## The colour channel a paste writes into: the Channels panel targets one and the params name
  ## no other target.
  if not p.pget("target").isNil: return
  let st = s.active
  if st.isNil: return
  if st.channelView.target.kind == ctColor: some(st.channelView.target.index) else: none(int)

proc channelCopy*(s: var Session): Option[JsonNode] =
  ## Copies the targeted colour or alpha channel as a grayscale clip (none when no single
  ## channel is targeted: Copy copies the layer).
  let d = s.active
  if d.isNil: return
  let doc = d.doc
  let canvas = doc[].bounds
  var src: Surface
  var k = 0
  case d.channelView.target.kind
  of ctComposite, ctLayerMask, ctVectorMask: return none(JsonNode)
  of ctColor:
    k = d.channelView.target.index
    if d.activeLayer.isNone: raise other("no active layer")
    let l = doc[].findLayer(d.activeLayer.get)
    if l.isNil or not l[].hasSurface: raise other("the active layer has no pixels")
    src = l[].surface
  of ctAlpha:
    let i = d.channelView.target.index
    if i >= doc.channels.len: raise other("no alpha channel " & $i & " (document has " & $doc.channels.len & ")")
    src = doc.channels[i].surface
  let n = src.format.channels
  if k >= n: raise other("no colour channel " & $k & " in this layer")
  let sel = doc.selection
  let area = if sel.isSome: sel.get.contentBounds.intersect(canvas) else: canvas
  if area.isEmpty: raise other("Could not copy: the selected area is empty")
  let fmt = PixelFormat(mode: cmGrayscale, sample: doc[].pixelFormat.sample, alpha: true)
  let values = src.readRegion(area)
  var cover: seq[float32]
  var mk = 1
  if sel.isSome:
    cover = sel.get.readRegion(area)
    mk = max(sel.get.format.channels, 1)
  var px = newSeqOfCap[float32](values.len div n * 2)
  for i in 0 ..< values.len div n:
    px.add values[i * n + k]
    px.add (if sel.isSome: (if i * mk < cover.len: cover[i * mk] else: 0'f32) else: 1'f32)
  var surface = newSurface(fmt)
  surface.writeRegion(area, px)
  surface.prune()
  let bounds = surface.contentBounds
  if bounds.isEmpty: raise other("Could not copy: the selected area is empty")
  s.clipboard = some(Clip(surface: surface, bounds: bounds))
  some(%*{"bounds": [bounds.x0, bounds.y0, bounds.width, bounds.height], "channel": k})

proc pasteOffset*(clip: Clip; canvas: Rect; p: JsonNode; inPlace: bool): (int, int)
proc shifted*(surface: Surface; dx, dy: int): Surface

proc channelPaste*(s: var Session; p: JsonNode; k: int; inPlace: bool; limit: Option[Surface]; label: string): JsonNode =
  ## Pastes the clip's luminosity into colour channel `k` of the active layer.
  if s.clipboard.isNone: raise other("the clipboard is empty")
  let clip = s.clipboard.get
  let d = s.activeOrRaise()
  let canvas = d.doc[].bounds
  if d.activeLayer.isNone: raise other("no active layer")
  let id = d.activeLayer.get
  let (dx, dy) = pasteOffset(clip, canvas, p, inPlace)
  let src = shifted(clip.surface, dx, dy)
  let area = src.contentBounds.intersect(canvas)
  s.editDo(label):
    let surf = paintSurface(doc, id, newJNull())
    let n = surf[].format.channels
    if k >= n: raise other("no colour channel " & $k & " in this layer")
    if not area.isEmpty:
      var rgba = newSeq[Rgba](area.width * area.height)
      src.readRgbaInto(area, rgba)
      var weight: seq[float32]
      var mk = 1
      if limit.isSome:
        weight = limit.get.readRegion(area)
        mk = max(limit.get.format.channels, 1)
      var px = surf[].readRegion(area)
      for i in 0 ..< min(px.len div n, rgba.len):
        let sp = rgba[i]
        let w = sp[3] * (if limit.isSome: (if i * mk < weight.len: weight[i * mk] else: 0'f32) else: 1'f32)
        let v = rgbToGray([sp[0], sp[1], sp[2]])
        px[i * n + k] = px[i * n + k] + (v - px[i * n + k]) * clamp(w, 0'f32, 1'f32)
      surf[].writeRegion(area, px)
      surf[].prune()
    doc.selection = none(Surface)
  %*{"layer": id, "channel": k, "offset": [dx, dy]}

proc channelCut*(s: var Session; id: LayerId; k: int): JsonNode =
  ## Cut with colour channel `k` targeted: copy it, then fill it with the background colour.
  let r = channelCopy(s)
  if r.isNone: raise other("no colour channel is targeted")
  let bg = s.tools.background
  s.editDo("Cut Pixels"): clearColorChannel(doc, id, k, bg)
  r.get

proc copiesAlphaChannel*(s: Session): bool =
  ## Copy reads only: with an alpha channel targeted it needs no pixel layer.
  let st = s.active
  not st.isNil and st.channelView.target.kind == ctAlpha and st.channelView.target.index < st.doc.channels.len
