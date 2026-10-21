## Channels: alpha and spot channels, Save / Load Selection, Quick Mask, channel targeting and
## visibility (the Channels panel), Apply Image / Calculations, Split / Merge Channels and Merge
## Spot Channel (port of `channel_cmds.rs`). Part of the `engine/core` unit: included, not imported.
##
## Pixel commands take `"target": {"channel": i}` or `"target": "quickMask"`; dispatch fills that
## in from the targeted channel when the caller gave none (`injectTarget`). Targeting one colour
## channel keeps them from touching the others (`restrictToColor`).

# ---------------------------------------------------------------- view state

proc colorVisible*(v: ChannelView; k: int): bool = not (k < v.colorHidden.len and v.colorHidden[k])
proc alphaShown*(v: ChannelView; i: int): bool = i < v.alphaVisible.len and v.alphaVisible[i]
proc setColorVisible*(v: var ChannelView; k: int; vis: bool) =
  if v.colorHidden.len <= k: v.colorHidden.setLen(k + 1)
  v.colorHidden[k] = not vis
proc setAlphaVisible*(v: var ChannelView; i: int; vis: bool) =
  if v.alphaVisible.len <= i: v.alphaVisible.setLen(i + 1)
  v.alphaVisible[i] = vis
proc visibleColors*(v: ChannelView; colors: int): int =
  for k in 0 ..< colors:
    if v.colorVisible(k): inc result

proc editMode(doc: Document): ColorMode = doc.pixelFormat.mode
proc colorCount*(doc: Document): int = editMode(doc).colorChannels

proc shownLayerMask*(v: ChannelView; doc: Document): Option[(LayerMask, MaskViewMode)] =
  ## The layer mask shown on the canvas and how, if its layer still has a mask.
  if v.layerMask.isNone: return
  let l = doc.findLayer(v.layerMask.get.layer)
  if l.isNil or l.mask.isNone: return
  some((l.mask.get, v.layerMask.get.mode))

proc isPlain*(v: ChannelView; doc: Document): bool =
  ## True when the canvas shows just the normal composite.
  let colors = colorCount(doc)
  if v.visibleColors(colors) != colors: return false
  for i in 0 ..< doc.channels.len:
    if v.alphaShown(i): return false
  (doc.quickMask.isNone or v.quickMaskHidden) and v.shownLayerMask(doc).isNone

proc `==`*(a, b: ChannelTarget): bool = a.kind == b.kind and (a.kind notin {ctColor, ctAlpha} or a.index == b.index)

proc composite(): ChannelTarget = ChannelTarget(kind: ctComposite)

proc toJ*(t: ChannelTarget): JsonNode =
  let kind = case t.kind
    of ctComposite: "composite"
    of ctColor: "color"
    of ctAlpha: "alpha"
    of ctLayerMask: "layerMask"
    of ctVectorMask: "vectorMask"
  if t.kind in {ctColor, ctAlpha}: %*{"kind": kind, "index": t.index} else: %*{"kind": kind}

proc fixView*(st: var DocState) =
  ## Keeps the view valid after the document changed (undo, deleted channels, mode changes).
  let n = st.doc.channels.len
  let colors = colorCount(st.doc[])
  let quick = st.doc.quickMask.isSome
  block:
    template v: untyped = st.channelView
    if v.alphaVisible.len > n: v.alphaVisible.setLen(n)
    if v.colorHidden.len > colors: v.colorHidden.setLen(colors)
    case v.target.kind
    of ctAlpha:
      if v.target.index >= n: v.target = composite()
    of ctColor:
      if v.target.index >= colors or colors < 2: v.target = composite()
    of ctLayerMask:
      let l = if st.activeLayer.isSome: st.doc[].findLayer(st.activeLayer.get) else: nil
      if l.isNil or l.mask.isNone: v.target = composite()
    of ctVectorMask:
      let l = if st.activeLayer.isSome: st.doc[].findLayer(st.activeLayer.get) else: nil
      if l.isNil or l.vectorMask.isNone: v.target = composite()
    of ctComposite: discard
  fixMaskView(st)
  # An alpha/spot eye, Quick Mask, or the active layer's mask can be the only thing on the
  # canvas. Otherwise the colour channels come back.
  var shown = false
  for i in 0 ..< n:
    if st.channelView.alphaShown(i): shown = true
  if quick and not st.channelView.quickMaskHidden: shown = true
  if st.activeLayer.isSome:
    let l = st.doc[].findLayer(st.activeLayer.get)
    if not l.isNil and l.mask.isSome: shown = true
  if st.channelView.visibleColors(colors) == 0 and not shown:
    st.channelView.colorHidden.setLen(0)

# ---------------------------------------------------------------- names and formats

proc colorNames*(mode: ColorMode): seq[string] =
  ## Colour channel names of a document mode (the rows under the composite).
  case mode
  of cmCmyk: @["Cyan", "Magenta", "Yellow", "Black"]
  of cmLab: @["Lightness", "a", "b"]
  of cmGrayscale: @["Gray"]
  else: @["Red", "Green", "Blue"]

proc compositeName*(mode: ColorMode): string =
  case mode
  of cmCmyk: "CMYK"
  of cmLab: "Lab"
  of cmGrayscale: "Gray"
  else: "RGB"

proc channelFormat*(doc: Document): PixelFormat =
  ## Pixel format of alpha channels: grayscale at the document depth, no transparency.
  PixelFormat(mode: cmGrayscale, sample: doc.depth, alpha: false)

proc nextAlphaName(doc: Document; base: string): string =
  for n in 1 .. doc.channels.len + 1:
    let c = base & " " & $n
    var used = false
    for ch in doc.channels:
      if ch.name == c: used = true
    if not used: return c
  base

# ---------------------------------------------------------------- planes

proc readPlane*(s: Surface; r: Rect): seq[float32] =
  let v = s.readRegion(r)
  let ch = s.channels
  if ch > 1:
    result = newSeq[float32](v.len div ch)
    for i in 0 ..< result.len: result[i] = v[i * ch]
  else: result = v

proc planeSurface*(vals: openArray[float32]; r: Rect; fmt: PixelFormat): Surface =
  result = newSurface(fmt)
  var clamped = newSeq[float32](vals.len)
  for i, v in vals: clamped[i] = clamp(v, 0'f32, 1'f32)
  result.writeRegion(r, clamped)
  result.prune()

proc materialize(s: var Surface; r: Rect) =
  ## Makes every canvas pixel explicit so filters that work on content bounds also see the
  ## untouched parts of a channel.
  let v = s.readRegion(r)
  s.writeRegion(r, v)

proc luminosity(rgba: array[4, float32]): float32 =
  0.299'f32 * rgba[0] + 0.587'f32 * rgba[1] + 0.114'f32 * rgba[2]

# ---------------------------------------------------------------- channel references

type
  ChanRefKind* = enum
    crComposite, crColor, crAlpha, crQuickMask, crSelection, crTransparency, crLayerMask, crVectorMask
  ChanRef* = object
    kind*: ChanRefKind
    index*: int

proc chanRef(k: ChanRefKind; i = 0): ChanRef = ChanRef(kind: k, index: i)

proc parseRef*(v: JsonNode; doc: Document): Option[ChanRef] =
  ## A channel named in params: "composite", a colour name, an alpha index (number) or name,
  ## "quickMask", "selection", "transparency", "mask", "vectorMask".
  let mode = editMode(doc)
  if v.isNil: return
  case v.kind
  of JInt:
    let u = v.asU64
    if u.isSome: some(chanRef(crAlpha, int(min(u.get, uint64(high(int)))))) else: none(ChanRef)
  of JFloat: none(ChanRef)
  of JObject:
    let c = v.pget("color").asU64
    if c.isSome: return some(chanRef(crColor, int(min(c.get, uint64(high(int))))))
    let a = v.pget("alpha").asU64
    if a.isSome: some(chanRef(crAlpha, int(min(a.get, uint64(high(int)))))) else: none(ChanRef)
  of JString:
    let l = v.str.toLowerAscii
    case l
    of "composite", "merged": return some(chanRef(crComposite))
    of "quickmask", "quick mask": return some(chanRef(crQuickMask))
    of "selection": return some(chanRef(crSelection))
    of "transparency": return some(chanRef(crTransparency))
    of "mask", "layermask": return some(chanRef(crLayerMask))
    of "vectormask", "vector mask": return some(chanRef(crVectorMask))
    else: discard
    if l == compositeName(mode).toLowerAscii: return some(chanRef(crComposite))
    let names = colorNames(mode)
    for k, n in names:
      if cmpIgnoreCase(n, l) == 0 and n.len == l.len:
        return some(if names.len == 1: chanRef(crComposite) else: chanRef(crColor, k))
    for i, c in doc.channels:
      if c.name == v.str: return some(chanRef(crAlpha, i))
    none(ChanRef)
  else: none(ChanRef)

proc docIndex(s: Session; p: JsonNode; key: string): int =
  let v = intParam(p, key)
  if v.isSome and v.get >= 0:
    if v.get < int64(s.documents.len): return int(v.get)
    raise other("no open document " & $v.get)
  if s.activeIndex.isNone: raise noDocument()
  s.activeIndex.get

proc alphaIndex(doc: Document; p: JsonNode; key, cmdId: string): int =
  ## Alpha channel index from `p[key]` (number or name), checked against the document.
  let r = parseRef(p.pget(key), doc)
  if r.isSome and r.get.kind == crAlpha:
    if r.get.index < doc.channels.len: return r.get.index
    raise bad(cmdId, "no alpha channel " & $r.get.index & " (document has " & $doc.channels.len & ")")
  raise bad(cmdId, "`" & key & "` must name an alpha channel (index or name)")

# ---------------------------------------------------------------- sources

proc nativePixels(doc: Document; layer: Option[LayerId]): (seq[float32], int) =
  ## Native pixel values of a layer or of the merged image over the canvas.
  let r = doc.bounds
  let fmt = doc.pixelFormat
  let n = fmt.channels
  if layer.isSome:
    let id = layer.get
    let l = doc.findLayer(id)
    if l.isNil: raise noLayer(id)
    if not l[].hasSurface: raise other("layer \"" & l.name & "\" has no pixels")
    let s = l[].surface
    if s.format == fmt: return (s.readRegion(r), n)
    let raw = s.readRegion(r)
    let sf = s.format
    let sn = sf.channels
    var outp = newSeqOfCap[float32](raw.len div max(sn, 1) * n)
    for i in 0 ..< raw.len div max(sn, 1):
      for v in fromRgba(fmt, toRgba(sf, raw.toOpenArray(i * sn, i * sn + sn - 1))): outp.add v
    return (outp, n)
  let buf = render(doc, r)
  var outp = newSeqOfCap[float32](buf.px.len * n)
  for p in buf.px:
    for v in fromRgba(fmt, p): outp.add v
  (outp, n)

proc sourceLayer(p: JsonNode): Option[LayerId] =
  let v = p.pget("layer").asU64
  if v.isSome: some(LayerId(v.get)) else: none(LayerId)

proc refPlanes(doc: Document; layer, active: Option[LayerId]; r: ChanRef): seq[seq[float32]] =
  ## Planes of channel `r` (`layer` none: merged). A composite gives one plane per colour channel.
  let area = doc.bounds
  let colors = colorCount(doc)
  case r.kind
  of crComposite, crColor:
    let (px, n) = nativePixels(doc, layer)
    var planes: seq[seq[float32]]
    for c in 0 ..< colors:
      var pl = newSeq[float32](px.len div max(n, 1))
      for i in 0 ..< pl.len: pl[i] = px[i * n + c]
      planes.add pl
    if r.kind == crColor:
      if r.index >= planes.len: raise other("no colour channel " & $r.index)
      return @[planes[r.index]]
    planes
  of crAlpha:
    if r.index >= doc.channels.len: raise other("no alpha channel " & $r.index)
    @[readPlane(doc.channels[r.index].surface, area)]
  of crQuickMask:
    if doc.quickMask.isNone: raise other("not in Quick Mask mode")
    @[readPlane(doc.quickMask.get.surface, area)]
  of crSelection:
    @[maskFromSurface(doc.selection, area)]
  of crTransparency:
    let id = if layer.isSome: layer elif active.isSome: active else: raise other("transparency needs a layer")
    let l = doc.findLayer(id.get)
    if l.isNil: raise noLayer(id.get)
    let count = area.width * area.height
    if l[].hasSurface:
      var px = newSeq[Rgba](count)
      l[].surface.readRgbaInto(area, px)
      var outp = newSeq[float32](count)
      for i, p in px: outp[i] = p[3]
      return @[outp]
    if l.content.kind == lkShape:
      # A shape can have no pixel cache yet: use its renderer.
      let px = compileShape(l.content.shape, DefaultTolerance, area).renderRgba(area)
      var outp = newSeq[float32](px.len)
      for i, p in px: outp[i] = p[3]
      return @[outp]
    # Adjustment and fill layers cover the whole canvas.
    var outp = newSeq[float32](count)
    for v in outp.mitems: v = 1
    @[outp]
  of crLayerMask:
    let id = if layer.isSome: layer elif active.isSome: active else: raise other("layer mask needs a layer")
    let l = doc.findLayer(id.get)
    if l.isNil: raise noLayer(id.get)
    if l.mask.isNone: raise other("layer \"" & l.name & "\" has no mask")
    @[readPlane(l.mask.get.surface, area)]
  of crVectorMask:
    let id = if layer.isSome: layer elif active.isSome: active else: raise other("vector mask needs a layer")
    let l = doc.findLayer(id.get)
    if l.isNil: raise noLayer(id.get)
    if l.vectorMask.isNone: raise other("layer \"" & l.name & "\" has no vector mask")
    # The path's shape, whether or not the mask is enabled.
    var shape = l.vectorMask.get
    shape.enabled = true
    shape.density = 1
    shape.feather = 0
    @[vectorMaskValues(shape, area)]

proc grayPlane(doc: Document; layer, active: Option[LayerId]; r: ChanRef): seq[float32] =
  ## One grayscale plane (composites reduce to luminosity, as in Calculations).
  var planes = refPlanes(doc, layer, active, r)
  if planes.len == 1: return planes[0]
  let fmt = doc.pixelFormat
  let n = if planes.len > 0: planes[0].len else: 0
  var px = newSeq[float32](fmt.channels)
  result = newSeq[float32](n)
  for i in 0 ..< n:
    for c, pl in planes: px[c] = pl[i]
    if fmt.alpha: px[planes.len] = 1
    result[i] = luminosity(toRgba(fmt, px))

proc source(s: Session; p: JsonNode; cmdId: string; gray: bool): seq[seq[float32]] =
  ## A source spec `{document?, layer?, channel, invert?}` reduced to planes over the canvas.
  let di = docIndex(s, p, "document")
  let st = s.docAt(di)
  let target = s.activeOrRaise()
  if st.doc.size != target.doc.size:
    raise bad(cmdId, "source document \"" & st.doc.name & "\" is " & $st.doc.size.width & "×" & $st.doc.size.height &
      "; it must match the target size")
  let ch = p.pget("channel")
  let r = if ch.isNil: some(chanRef(crComposite)) else: parseRef(ch, st.doc[])
  if r.isNone: raise bad(cmdId, "unknown `channel`")
  let layer = sourceLayer(p)
  result = if gray: @[grayPlane(st.doc[], layer, st.activeLayer, r.get)] else: refPlanes(st.doc[], layer, st.activeLayer, r.get)
  if result.len == 0: raise bad(cmdId, "the source has no channels")
  if p.getBoolOr("invert", false):
    for pl in result.mitems:
      for v in pl.mitems: v = 1 - v

# ---------------------------------------------------------------- blending

type
  BlendingKind = enum blMode, blAdd, blSubtract
  Blending* = object
    kind: BlendingKind
    mode: BlendMode
    scale, offset: float32

proc parseBlending*(p: JsonNode): Blending =
  ## Raises `ValueError` with the message on an unsupported blending.
  let name = p.getStrOr("blending", "multiply")
  let scale = float32(clamp(p.getF64Or("scale", 1.0), 1.0, 2.0))
  let offset = float32(clamp(p.getF64Or("offset", 0.0), -255.0, 255.0)) / 255'f32
  case name.toLowerAscii
  of "add": Blending(kind: blAdd, scale: scale, offset: offset)
  of "subtract": Blending(kind: blSubtract, scale: scale, offset: offset)
  else:
    let m = blendFromStr(name.toLowerAscii)
    if m.isSome and m.get.isSeparable and m.get notin {bmDissolve, bmPassThrough}: Blending(kind: blMode, mode: m.get)
    else: raise newException(ValueError, "unsupported blending `" & name.toLowerAscii & "`")

proc apply*(b: Blending; base, top: float32): float32 =
  let v = case b.kind
    of blMode: blendChannel(b.mode, base, top)
    of blAdd: (base + top) / b.scale + b.offset
    of blSubtract: (base - top) / b.scale + b.offset
  clamp(v, 0'f32, 1'f32)

proc sourceSpec(p: JsonNode; key, prefix: string): Option[JsonNode] =
  ## A source spec from `p[key]`, or from flat `<prefix>Document` / `Layer` / `Channel` /
  ## `Invert` keys (what the generic dialog form sends).
  let v = p.pget(key)
  if not v.isNil: return some(v)
  var m = newJObject()
  for (k, field) in [("document", "Document"), ("layer", "Layer"), ("channel", "Channel"), ("invert", "Invert")]:
    let x = p.pget(prefix & field)
    if x.isNull: continue
    if x.kind == JString and (x.str.len == 0 or (k == "layer" and x.str == "merged")): continue
    m[k] = x
  if m.len > 0: some(m) else: none(JsonNode)

proc maskPlane(s: Session; p: JsonNode; cmdId: string): Option[seq[float32]] =
  let m = sourceSpec(p, "mask", "mask")
  if m.isNone or m.get.isNull or (m.get.kind == JBool and not m.get.bval): return
  let c = m.get.pget("channel")
  if c.isNil or (c.kind == JString and c.str == "none"): return
  some(source(s, m.get, cmdId, true)[0])

# ---------------------------------------------------------------- targets for pixel commands

type
  TargetKind* = enum tgPixels, tgMask, tgAlpha, tgQuickMask
  PixelTarget* = object
    kind*: TargetKind
    index*: int

proc targetOf*(p: JsonNode): PixelTarget =
  let t = p.pget("target")
  if t.isNil: return PixelTarget(kind: tgPixels)
  case t.kind
  of JString:
    case t.str
    of "mask": PixelTarget(kind: tgMask)
    of "quickMask": PixelTarget(kind: tgQuickMask)
    else: PixelTarget(kind: tgPixels)
  of JObject:
    let i = t.pget("channel").asU64
    if i.isSome: PixelTarget(kind: tgAlpha, index: int(min(i.get, uint64(high(int))))) else: PixelTarget(kind: tgPixels)
  else: PixelTarget(kind: tgPixels)

proc isChannelTarget*(p: JsonNode): bool =
  ## The target is an alpha channel or the Quick Mask (no layer involved).
  targetOf(p).kind in {tgAlpha, tgQuickMask}

proc targetSurface*(doc: var Document; layer: Option[LayerId]; p: JsonNode): (ptr Surface, bool) =
  ## The surface a pixel command writes to and whether the layer's transparency is locked.
  let t = targetOf(p)
  case t.kind
  of tgAlpha:
    if t.index >= doc.channels.len:
      raise other("no alpha channel " & $t.index & " (document has " & $doc.channels.len & ")")
    (addr doc.channels[t.index].surface, false)
  of tgQuickMask:
    if doc.quickMask.isNone: raise other("not in Quick Mask mode")
    (addr doc.quickMask.get.surface, false)
  else:
    if layer.isNone: raise other("no active layer")
    let lock = doc.effectiveLocks(layer.get).transparency and t.kind == tgPixels
    (paintSurface(doc, layer.get, p), lock)

proc readTargetSurface*(doc: Document; layer: Option[LayerId]; p: JsonNode): Surface =
  let t = targetOf(p)
  case t.kind
  of tgAlpha:
    if t.index >= doc.channels.len:
      raise other("no alpha channel " & $t.index & " (document has " & $doc.channels.len & ")")
    doc.channels[t.index].surface
  of tgQuickMask:
    if doc.quickMask.isNone: raise other("not in Quick Mask mode")
    doc.quickMask.get.surface
  of tgPixels:
    if layer.isNone: raise other("no active layer")
    let l = doc.findLayer(layer.get)
    if l.isNil or not l[].hasSurface: raise noLayer(layer.get)
    l[].surface
  of tgMask:
    if layer.isNone: raise other("no active layer")
    let l = doc.findLayer(layer.get)
    if l.isNil or l.mask.isNone: raise noLayer(layer.get)
    l.mask.get.surface

proc channelSurfaceForFilter*(doc: var Document; layer: Option[LayerId]; p: JsonNode): ptr Surface =
  ## The grayscale surface filters and adjustments edit instead of the layer's pixels (made
  ## explicit over the canvas), if the params target one; nil for the layer's pixels.
  if targetOf(p).kind == tgPixels: return nil
  let b = doc.bounds
  let (surf, _) = targetSurface(doc, layer, p)
  materialize(surf[], b)
  surf

proc adjDefaultForOk(kind: string): bool =
  try:
    discard adjDefaultFor(kind, cmRgb)
    true
  except EngineError: false

const FillKeyIds* = ["edit.fillForeground", "edit.fillBackground", "edit.fillForegroundPreserve",
                     "edit.fillBackgroundPreserve"]

proc editsMask(id: string): bool =
  ## Commands that edit a targeted layer mask in place of the layer.
  (id.startsWith("image.adjustments.") and adjDefaultForOk(id["image.adjustments.".len .. ^1])) or
    filterParamsFor(id, newJNull()).isSome or id == "edit.fill" or id == "edit.copy" or id in FillKeyIds

proc maskTargetEnabled(s: Session; id: string; p: JsonNode): Option[string] =
  ## The precondition of `id` when `p` targets the active layer's mask: the layer needs a mask.
  if targetOf(p).kind != tgMask or not editsMask(id): return
  let (l, why) = activeLayerOf(s)
  if l.isNil: return some(why)
  some(if l.mask.isSome: "" else: "the active layer has no layer mask")

proc routed(id: string): bool =
  ## Commands whose target follows the Channels panel.
  id.startsWith("filter.") or id == "plugin.run" or id.startsWith("image.adjustments.") or
    id in ["paint.stroke", "paint.pencil", "paint.bucket", "paint.gradient", "paint.mixerBrush", "edit.fill",
           "image.applyImage"] or
    id in FillKeyIds or
    id in ["edit.copy", "edit.paste", "edit.pasteSpecial.pasteInPlace", "edit.pasteSpecial.pasteInto",
           "edit.pasteSpecial.pasteOutside"] or
    id in ["edit.transform.rotate180", "edit.transform.rotate90Cw", "edit.transform.rotate90Ccw",
           "edit.transform.flipHorizontal", "edit.transform.flipVertical"] or
    id in ["paint.cloneStamp", "paint.patternStamp", "paint.healingBrush", "paint.spotHealing", "paint.dodge",
           "paint.burn", "paint.sponge", "paint.blur", "paint.sharpen", "paint.smudge", "paint.historyBrush",
           "paint.redEye", "paint.remove"]

proc followsTarget*(id: string): bool =
  ## Whether `id` edits the targeted channel or mask when its params name no `"target"`.
  routed(id) or id == "select.float"

proc injectTarget*(s: Session; id: string; params: JsonNode): JsonNode =
  ## Fills in `"target"` from the targeted channel (or Quick Mask mode) when the caller gave none.
  if not routed(id) or not params.pget("target").isNil: return params
  let st = s.active
  if st.isNil: return params
  var t: JsonNode
  let tg = st.channelView.target
  if tg.kind == ctAlpha and tg.index < st.doc.channels.len: t = %*{"channel": tg.index}
  elif tg.kind == ctComposite and st.doc.quickMask.isSome: t = %"quickMask"
  elif tg.kind == ctLayerMask: t = %"mask"
  elif tg.kind == ctComposite and currentMaskView(st[]).isSome: t = %"mask"
  else: return params
  if params.isObject: params.withField("target", t) else: %*{"target": t}

proc colorRestriction(s: Session; id: string; params: JsonNode): Option[int] =
  ## The colour channel pixel commands are limited to, if a single colour channel is targeted.
  if not routed(id) or isChannelTarget(params) or isMaskTarget(params): return
  let st = s.active
  if st.isNil: return
  if st.channelView.target.kind == ctColor: some(st.channelView.target.index) else: none(int)

proc restrictToColor*(before: Document; after: var Document; id: LayerId; k: int) =
  ## After a pixel edit with colour channel `k` targeted, puts the other channels (and the
  ## layer's transparency) of layer `id` back as they were. Only changed tiles are touched.
  let oldL = before.findLayer(id)
  if oldL.isNil or not oldL[].hasSurface: return
  let old = oldL[].surface
  let l = after.layerPtr(id)
  if l.isNil: return
  let nw = l[].surfaceMut
  if nw.isNil: return
  if old.format != nw[].format: return
  var coords: seq[TileCoord]
  for c in nw[].sortedCoords:
    let o = old.tile(c)
    if o.isNil or o != nw[].tile(c): coords.add c
  for c in old.sortedCoords:
    if nw[].tile(c).isNil: coords.add c
  let n = nw[].channels
  for c in coords:
    let r = c.rect
    let o = old.readRegion(r)
    var v = nw[].readRegion(r)
    for i in 0 ..< v.len div max(n, 1):
      for ch in 0 ..< n:
        if ch != k: v[i * n + ch] = o[i * n + ch]
    nw[].writeRegion(r, v)
  nw[].prune()

proc targetedColor*(s: Session): Option[int] =
  ## The colour channel Clear is limited to: the Channels panel targets a single one.
  let st = s.active
  if st.isNil: return
  if st.channelView.target.kind == ctColor: some(st.channelView.target.index) else: none(int)

proc clearColorChannel*(doc: var Document; id: LayerId; k: int; background: array[4, float32]) =
  ## Clear with colour channel `k` targeted: fill the selected area (or the canvas) of that
  ## channel only with the background colour's value for it.
  let canvas = doc.bounds
  let sel = doc.selection
  let area = if sel.isSome: sel.get.contentBounds.intersect(canvas) else: canvas
  let surf = paintSurface(doc, id, newJNull())
  let fmt = surf[].format
  let n = fmt.channels
  let bgv = fromRgba(fmt, [background[0], background[1], background[2], 1'f32])
  if k >= n or k >= bgv.len: raise other("no colour channel " & $k & " in this layer")
  let v = bgv[k]
  if area.isEmpty: return
  var cover: seq[float32]
  var mk = 1
  if sel.isSome:
    cover = sel.get.readRegion(area)
    mk = max(sel.get.format.channels, 1)
  var px = surf[].readRegion(area)
  for i in 0 ..< px.len div max(n, 1):
    let w = if sel.isSome: (if i * mk < cover.len: cover[i * mk] else: 0'f32) else: 1'f32
    px[i * n + k] = px[i * n + k] + (v - px[i * n + k]) * clamp(w, 0'f32, 1'f32)
  surf[].writeRegion(area, px)
  surf[].prune()

proc targetedChannel*(s: Session): Option[JsonNode] =
  ## The alpha channel or Quick Mask the Channels panel targets, as `"target"` params.
  let st = s.active
  if st.isNil: return
  let tg = st.channelView.target
  if tg.kind == ctAlpha and tg.index < st.doc.channels.len: some(%*{"target": {"channel": tg.index}})
  elif tg.kind == ctComposite and st.doc.quickMask.isSome: some(%*{"target": "quickMask"})
  else: none(JsonNode)

proc clearChannel*(doc: var Document; target: JsonNode; background: array[4, float32]) =
  ## Clear with an alpha channel or the Quick Mask targeted: fills the selected area with the
  ## background colour, as gray.
  let canvas = doc.bounds
  let sel = doc.selection
  let area = if sel.isSome: sel.get.contentBounds.intersect(canvas) else: canvas
  if area.isEmpty: return
  let (surf, _) = targetSurface(doc, none(LayerId), target)
  fillColor(surf[], area, [background[0], background[1], background[2], 1'f32], sel)

proc editsChannel*(s: Session): bool =
  ## The active document targets an alpha channel or is in Quick Mask mode.
  let st = s.active
  if st.isNil: return false
  let tg = st.channelView.target
  (st.doc.quickMask.isSome and tg.kind == ctComposite) or (tg.kind == ctAlpha and tg.index < st.doc.channels.len)

# ---------------------------------------------------------------- enabled predicates

proc hasChannelsE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if d.doc.channels.len > 0: "" else: "the document has no alpha channels"
proc hasSpotE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  for c in d.doc.channels:
    if c.spot.isSome: return ""
  "the document has no spot channels"
proc canSplitE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if colorCount(d.doc[]) + d.doc.channels.len < 2: "only one channel to split" else: ""
proc canMergeE(s: Session): string =
  if s.active.isNil: return "no document open"
  if mergeable(s).len >= 2: "" else: "Merge Channels needs at least two open grayscale documents of the same size"

# ---------------------------------------------------------------- Save / Load Selection

proc chTouch(st: var DocState) =
  let clean = st.savedRevision == st.revision
  inc st.revision
  if clean: st.savedRevision = st.revision
  st.lastDamage = none(Rect)

proc saveSelection(s: var Session; p: JsonNode): JsonNode =
  let cmdId = "select.saveSelection"
  let op = p.getStrOr("operation", "new")
  if op notin ["new", "replace", "add", "subtract", "intersect"]: raise bad(cmdId, "unknown operation `" & op & "`")
  let di = docIndex(s, p, "document")
  let src = s.activeOrRaise()
  let area = src.doc[].bounds
  let selection = maskFromSurface(src.doc.selection, area)
  if s.docAt(di).doc.size != src.doc.size: raise bad(cmdId, "the destination document must have the same size")
  let channel = if p.pget("channel").isNil: newJNull() else: p.pget("channel")
  let nm = p.pget("name").asStr
  let name = if nm.isSome and nm.get.strip.len > 0: some(nm.get.strip) else: none(string)
  let prev = s.activeIndex
  discard s.setActive(di)
  var res: int
  var err: ref Exception
  try:
    res = s.editGet("Save Selection", int):
      let fmt = channelFormat(doc)
      var existing = none(int)
      if channel.kind == JNull or (channel.kind == JString and channel.str == "new"): discard
      else:
        let r = parseRef(channel, doc)
        if r.isSome and r.get.kind == crAlpha and r.get.index < doc.channels.len: existing = some(r.get.index)
        else: raise bad(cmdId, "`channel` must be \"new\" or an existing alpha channel")
      if existing.isNone or op == "new":
        let nm2 = if name.isSome: name.get else: nextAlphaName(doc, "Alpha")
        doc.channels.add newAlphaChannel(nm2, planeSurface(selection, area, fmt))
        doc.channels.len - 1
      else:
        let i = existing.get
        let old = readPlane(doc.channels[i].surface, area)
        var vals = newSeq[float32](min(old.len, selection.len))
        case op
        of "add":
          for j in 0 ..< vals.len: vals[j] = rmax(old[j], selection[j])
        of "subtract":
          for j in 0 ..< vals.len: vals[j] = rmax(old[j] - selection[j], 0'f32)
        of "intersect":
          for j in 0 ..< vals.len: vals[j] = rmin(old[j], selection[j])
        else: vals = selection
        doc.channels[i].surface = planeSurface(vals, area, fmt)
        if name.isSome: doc.channels[i].name = name.get
        i
  except CatchableError as e: err = e
  if prev.isSome: discard s.setActive(prev.get)
  if not err.isNil: raise err
  let chName = s.docAt(di).doc.channels[res].name
  %*{"channel": res, "name": chName, "document": di}

proc loadSelection(s: var Session; p: JsonNode): JsonNode =
  let cmdId = "select.loadSelection"
  let op = p.getStrOr("operation", "new")
  let mode = case op
    of "new", "replace": smReplace
    of "add": smAdd
    of "subtract": smSubtract
    of "intersect": smIntersect
    else: raise bad(cmdId, "unknown operation `" & op & "`")
  if p.pget("channel").isNil: raise bad(cmdId, "missing `channel`")
  # Composite and colour channels load their luminosity / values, as ⌘-clicking them does.
  let planes = source(s, p, cmdId, true)
  if planes.len == 0: raise bad(cmdId, "the channel has no pixels")
  let plane = planes[0]
  let doc0 = s.activeOrRaise().doc
  let r = parseRef(p.pget("channel"), doc0[])
  let label = if r.isSome and r.get.kind == crTransparency: "Load Transparency"
              elif r.isSome and r.get.kind == crLayerMask: "Load Layer Mask"
              else: "Load Selection"
  let selected = s.editGet(label, bool):
    doc.selection = combine(doc.selection, plane, doc.bounds, mode)
    doc.selection.isSome
  %*{"selected": selected}

# ---------------------------------------------------------------- Quick Mask

proc unselectedSurface(area: Rect; fmt: PixelFormat; indicates: ColorIndicates): Surface =
  result = newSurface(fmt)
  if indicates != ciSelectedAreas: result.fillRect(area, [1'f32])

proc quickMaskFrom(doc: Document; o: QuickMaskOptions): AlphaChannel =
  ## The Quick Mask channel for the current selection: white where selected.
  let area = doc.bounds
  let fmt = channelFormat(doc)
  let surface = if doc.selection.isSome: planeSurface(maskFromSurface(doc.selection, area), area, fmt)
                else: unselectedSurface(area, fmt, o.indicates)
  result = newAlphaChannel("Quick Mask", surface)
  result.color = o.color
  result.opacity = o.opacity
  result.indicates = o.indicates

proc uniform(vals: openArray[float32]; target: float32): bool =
  for v in vals:
    if abs(v - target) > 0.5'f32 / 255'f32: return false
  true

proc selectionFrom(doc: Document; ch: AlphaChannel): Option[Surface] =
  ## The selection a Quick Mask channel turns back into.
  let area = doc.bounds
  let vals = readPlane(ch.surface, area)
  if doc.quickMaskUnselected and ch.indicates != ciSelectedAreas and uniform(vals, 1): return
  combine(none(Surface), vals, area, smReplace)

proc quickMaskCmd(s: var Session; p: JsonNode): JsonNode =
  let on = s.activeOrRaise().doc.quickMask.isSome
  let want = p.getBoolOr("on", not on)
  if want == on: return %*{"quickMask": on}
  let opts = s.quickMaskOptions
  if want:
    s.editDo("Edit in Quick Mask"):
      doc.quickMask = some(quickMaskFrom(doc, opts))
      doc.quickMaskUnselected = doc.selection.isNone
      doc.selection = none(Surface)
    let st = s.active
    if not st.isNil:
      st.channelView.target = composite()
      st.channelView.quickMaskHidden = false
  else:
    s.editDo("Exit Quick Mask"):
      if doc.quickMask.isSome:
        let q = doc.quickMask.get
        doc.quickMask = none(AlphaChannel)
        doc.selection = selectionFrom(doc, q)
  %*{"quickMask": want}

# ---------------------------------------------------------------- channel management

proc indicatesParam(v: JsonNode): Option[ColorIndicates] =
  let s = v.asStr
  if s.isNone: return
  case s.get
  of "masked", "maskedAreas": some(ciMaskedAreas)
  of "selected", "selectedAreas": some(ciSelectedAreas)
  else: none(ColorIndicates)

proc colorValue(p: JsonNode; key: string): Option[Color] =
  if p.pget(key).isNil: return
  let c = colorParam(p, key, [1'f32, 0, 0, 1])
  some(rgb(c[0], c[1], c[2]))

proc newChannelCmd(s: var Session; p: JsonNode): JsonNode =
  let fill = p.getStrOr("fill", "black")
  let name = p.pget("name").asStr
  let indicates = indicatesParam(p.pget("indicates"))
  let color = colorValue(p, "color")
  let op = p.pget("opacity").asF64
  let opacity = if op.isSome: some(clamp(float32(op.get) / 100'f32, 0'f32, 1'f32)) else: none(float32)
  let i = s.editGet("New Channel", int):
    let area = doc.bounds
    let fmt = channelFormat(doc)
    var surface: Surface
    case fill
    of "white":
      var ones = newSeq[float32](area.width * area.height)
      for v in ones.mitems: v = 1
      surface = planeSurface(ones, area, fmt)
    of "selection": surface = planeSurface(maskFromSurface(doc.selection, area), area, fmt)
    else: surface = newSurface(fmt)
    var ch = newAlphaChannel(if name.isSome: name.get else: nextAlphaName(doc, "Alpha"), surface)
    if indicates.isSome: ch.indicates = indicates.get
    if color.isSome: ch.color = color.get
    if opacity.isSome: ch.opacity = opacity.get
    doc.channels.add ch
    doc.channels.len - 1
  # Photoshop targets and shows a new channel on its own.
  targetChannel(s, ChannelTarget(kind: ctAlpha, index: i))
  %*{"channel": i}

proc newSpotCmd(s: var Session; p: JsonNode): JsonNode =
  let color = colorValue(p, "color").get(rgb(1, 0, 0))
  let solidity = float32(clamp(p.getF64Or("solidity", 0.0), 0.0, 100.0)) / 100'f32
  let name = p.pget("name").asStr
  let fromSel = p.getBoolOr("fromSelection", true)
  let i = s.editGet("New Spot Channel", int):
    let area = doc.bounds
    let fmt = channelFormat(doc)
    let surface = if fromSel and doc.selection.isSome: planeSurface(maskFromSurface(doc.selection, area), area, fmt)
                  else: newSurface(fmt)
    var ch = newAlphaChannel(if name.isSome: name.get else: nextAlphaName(doc, "Spot Color"), surface)
    ch.spot = some((color, solidity))
    doc.channels.add ch
    doc.channels.len - 1
  let st = s.active
  if not st.isNil:
    st.channelView.setAlphaVisible(i, true)
    chTouch(st[])
  %*{"channel": i}

proc grayDocument(name: string; like: Document; plane: openArray[float32]): Document =
  ## A one-layer grayscale document whose pixels are `plane`.
  result = newDocument(name, like.size, cmGrayscale, like.depth)
  result.resolutionDpi = like.resolutionDpi
  let fmt = result.pixelFormat
  var bg = rasterLayer("Background", fmt)
  bg.locks.transparency = true
  bg.locks.position = true
  var data = newSeq[float32](plane.len * 2)
  for i, v in plane:
    data[i * 2] = clamp(v, 0'f32, 1'f32)
    data[i * 2 + 1] = 1
  bg.content.raster.writeRegion(like.bounds, data)
  bg.content.raster.prune()
  result.layers.add bg

proc duplicateChannel(s: var Session; p: JsonNode): JsonNode =
  let cmdId = "channel.duplicate"
  let src = s.activeOrRaise()
  let srcDoc = src.doc
  let area = srcDoc[].bounds
  let rr = parseRef(p.pget("channel"), srcDoc[])
  if rr.isNone: raise bad(cmdId, "missing or unknown `channel`")
  let r = rr.get
  var ch: AlphaChannel
  case r.kind
  of crAlpha:
    if r.index >= srcDoc.channels.len: raise bad(cmdId, "no alpha channel " & $r.index)
    ch = srcDoc.channels[r.index]
  of crQuickMask:
    if srcDoc.quickMask.isNone: raise bad(cmdId, "not in Quick Mask mode")
    ch = srcDoc.quickMask.get
  else:
    let plane = grayPlane(srcDoc[], none(LayerId), src.activeLayer, r)
    ch = newAlphaChannel("", planeSurface(plane, area, channelFormat(srcDoc[])))
  let base = case r.kind
    of crAlpha, crQuickMask: ch.name
    of crColor: colorNames(editMode(srcDoc[]))[r.index]
    else: compositeName(editMode(srcDoc[]))
  let nm = p.pget("name").asStr
  ch.name = if nm.isSome: nm.get else: base & " copy"
  if p.getBoolOr("invert", false):
    var v = readPlane(ch.surface, area)
    for x in v.mitems: x = 1 - x
    ch.surface = planeSurface(v, area, ch.surface.format)
  let d = p.pget("document")
  if not d.isNil and d.kind == JString and d.str == "new":
    # Photoshop makes a multichannel document; ours is grayscale with the channel as pixels.
    let nd = grayDocument(ch.name, srcDoc[], readPlane(ch.surface, area))
    let i = s.addDocument(nd)
    return %*{"document": i}
  let di = docIndex(s, p, "document")
  if s.docAt(di).doc.size != srcDoc.size: raise bad(cmdId, "the destination document must have the same size")
  let prev = s.activeIndex
  discard s.setActive(di)
  var res: int
  var err: ref Exception
  try:
    res = s.editGet("Duplicate Channel", int):
      let fmt = channelFormat(doc)
      var c = ch
      if c.surface.format != fmt: c.surface = c.surface.convert(fmt)
      doc.channels.add c
      doc.channels.len - 1
  except CatchableError as e: err = e
  if prev.isSome: discard s.setActive(prev.get)
  if not err.isNil: raise err
  %*{"channel": res, "document": di}

proc fixViewNow(st: var DocState) =
  fixView(st)
  chTouch(st)

proc deleteChannel(s: var Session; p: JsonNode): JsonNode =
  let cmdId = "channel.delete"
  let st0 = s.activeOrRaise()
  var i: int
  if not p.pget("channel").isNil: i = alphaIndex(st0.doc[], p, "channel", cmdId)
  elif st0.channelView.target.kind == ctAlpha: i = st0.channelView.target.index
  else: raise bad(cmdId, "missing `channel`")
  s.editDo("Delete Channel"):
    if i >= doc.channels.len: raise other("no alpha channel " & $i)
    doc.channels.delete(i)
  let st = s.active
  if not st.isNil:
    template v: untyped = st.channelView
    if i < v.alphaVisible.len: v.alphaVisible.delete(i)
    if v.target.kind == ctAlpha and v.target.index == i:
      v.colorHidden.setLen(0)
      v.target = composite()
    elif v.target.kind == ctAlpha and v.target.index > i:
      v.target = ChannelTarget(kind: ctAlpha, index: v.target.index - 1)
    fixViewNow(st[])
  jnull()

proc renameChannel(s: var Session; p: JsonNode): JsonNode =
  let cmdId = "channel.rename"
  let i = alphaIndex(s.activeOrRaise().doc[], p, "channel", cmdId)
  let nm = p.pget("name").asStr
  if nm.isNone or nm.get.strip.len == 0: raise bad(cmdId, "missing `name`")
  let name = nm.get.strip
  s.editDo("Rename Channel"): doc.channels[i].name = name
  jnull()

proc moveChannel(s: var Session; p: JsonNode): JsonNode =
  let cmdId = "channel.move"
  let doc0 = s.activeOrRaise().doc
  let n = doc0.channels.len
  let i = alphaIndex(doc0[], p, "channel", cmdId)
  let t = p.pget("to").asU64
  if t.isNone: raise bad(cmdId, "missing `to`")
  let to = int(min(t.get, uint64(n - 1)))
  if to == i: return %*{"channel": i}
  s.editDo("Channel Order"):
    let c = doc.channels[i]
    doc.channels.delete(i)
    doc.channels.insert(c, to)
  let st = s.active
  if not st.isNil:
    template v: untyped = st.channelView
    v.alphaVisible.setLen(n)
    let vis = v.alphaVisible[i]
    v.alphaVisible.delete(i)
    v.alphaVisible.insert(vis, to)
    if v.target.kind == ctAlpha:
      let tt = v.target.index
      let remapped = if tt == i: to
                     elif i < tt and tt <= to: tt - 1
                     elif to <= tt and tt < i: tt + 1
                     else: tt
      v.target = ChannelTarget(kind: ctAlpha, index: remapped)
    chTouch(st[])
  %*{"channel": to}

proc targetChannel(s: var Session; t: ChannelTarget) =
  ## Makes `t` the edit target and shows it the way Photoshop does when its row is clicked.
  let st = s.active
  if not st.isNil:
    let colors = colorCount(st.doc[])
    let n = st.doc.channels.len
    template v: untyped = st.channelView
    v.target = t
    case t.kind
    of ctComposite, ctLayerMask, ctVectorMask: v.colorHidden.setLen(0)
    of ctColor:
      v.colorHidden = newSeq[bool](colors)
      for c in 0 ..< colors: v.colorHidden[c] = c != t.index
      v.alphaVisible.setLen(0)
    of ctAlpha:
      v.colorHidden = newSeq[bool](colors)
      for c in 0 ..< colors: v.colorHidden[c] = true
      v.alphaVisible = newSeq[bool](n)
      for c in 0 ..< n: v.alphaVisible[c] = c == t.index
    fixViewNow(st[])
  discard s.tools.targetMask(t.kind == ctLayerMask)

proc parseTarget(s: Session; p: JsonNode; cmdId: string): ChannelTarget =
  let doc = s.activeOrRaise().doc
  let ch = p.pget("channel")
  let r = if ch.isNil: some(chanRef(crComposite)) else: parseRef(ch, doc[])
  let colors = colorCount(doc[])
  if r.isSome:
    case r.get.kind
    of crComposite: return composite()
    of crColor:
      if r.get.index < colors and colors > 1: return ChannelTarget(kind: ctColor, index: r.get.index)
    of crAlpha:
      if r.get.index < doc.channels.len: return ChannelTarget(kind: ctAlpha, index: r.get.index)
    of crLayerMask:
      let (l, why) = activeLayerOf(s)
      if l.isNil: raise bad(cmdId, why)
      if l.mask.isNone: raise bad(cmdId, "the active layer has no layer mask")
      return ChannelTarget(kind: ctLayerMask)
    of crVectorMask:
      let (l, why) = activeLayerOf(s)
      if l.isNil: raise bad(cmdId, why)
      if l.vectorMask.isNone: raise bad(cmdId, "the active layer has no vector mask")
      return ChannelTarget(kind: ctVectorMask)
    else: discard
  raise bad(cmdId, "`channel` must be \"composite\", \"mask\", \"vectorMask\", a colour channel name or an alpha channel")

proc channelsJson*(st: DocState): JsonNode =
  ## The Channels panel rows and view state of a document (also part of `inspect`).
  let doc = st.doc
  let mode = editMode(doc[])
  let v = st.channelView
  let colors = colorCount(doc[])
  var colorRows = newJArray()
  for k, n in colorNames(mode): colorRows.add %*{"name": n, "visible": v.colorVisible(k)}
  var alpha = newJArray()
  for i, c in doc.channels:
    let spot = if c.spot.isSome: %*{"color": c.spot.get[0].toRgb, "solidity": c.spot.get[1]} else: newJNull()
    alpha.add %*{"index": i, "name": c.name, "visible": v.alphaShown(i), "spot": spot, "color": c.color.toRgb,
                 "opacity": c.opacity, "indicates": $c.indicates}
  let quick = if doc.quickMask.isSome:
      let q = doc.quickMask.get
      %*{"visible": not v.quickMaskHidden, "color": q.color.toRgb, "opacity": q.opacity, "indicates": $q.indicates}
    else: newJNull()
  %*{"composite": compositeName(mode), "colors": colorRows, "alpha": alpha, "quickMask": quick,
     "target": toJ(v.target), "compositeVisible": v.visibleColors(colors) == colors,
     "layerMaskView": maskViewJson(st)}

proc channelList(s: Session): JsonNode =
  let st = s.active
  if st.isNil: newJNull() else: channelsJson(st[])

proc targetCmd(s: var Session; p: JsonNode): JsonNode =
  let t = parseTarget(s, p, "channel.target")
  targetChannel(s, t)
  channelList(s)

const SlotKey = (when defined(macosx): "⌘" else: "Ctrl+")

proc targetSlot(s: var Session; slot: int): JsonNode =
  ## ⌘2 composite, ⌘3… colour channels then alpha channels.
  let doc = s.activeOrRaise().doc
  let colors = colorCount(doc[])
  let shownColors = if colors > 1: colors else: 0
  var t: ChannelTarget
  if slot == 2: t = composite()
  elif slot - 3 < shownColors: t = ChannelTarget(kind: ctColor, index: slot - 3)
  elif slot - 3 - shownColors < doc.channels.len: t = ChannelTarget(kind: ctAlpha, index: slot - 3 - shownColors)
  else: raise other("no channel for " & SlotKey & $slot)
  targetChannel(s, t)
  channelList(s)

proc slotOk(n: int): EnabledProc =
  result = proc(s: Session): string =
    let d = s.active
    if d.isNil: return "no document open"
    let colors = colorCount(d.doc[])
    let shown = if colors > 1: colors else: 0
    if n - 3 < shown + d.doc.channels.len: "" else: "no channel for " & SlotKey & $n

proc setVisibleCmd(s: var Session; p: JsonNode): JsonNode =
  let cmdId = "channel.setVisible"
  let st = s.activeOrRaise()
  let doc = st.doc
  let rr = parseRef(p.pget("channel"), doc[])
  if rr.isNone: raise bad(cmdId, "missing or unknown `channel`")
  let r = rr.get
  let colors = colorCount(doc[])
  let n = doc.channels.len
  template v: untyped = st.channelView
  proc visible(cur: bool): bool = p.getBoolOr("visible", not cur)
  case r.kind
  of crComposite:
    let all = v.visibleColors(colors) == colors
    let on = visible(all)
    v.colorHidden = newSeq[bool](colors)
    for x in v.colorHidden.mitems: x = not on
  of crColor:
    if r.index >= colors: raise bad(cmdId, "that channel has no visibility")
    let on = visible(v.colorVisible(r.index))
    v.setColorVisible(r.index, on)
  of crAlpha:
    if r.index >= n: raise bad(cmdId, "that channel has no visibility")
    let on = visible(v.alphaShown(r.index))
    v.setAlphaVisible(r.index, on)
  of crQuickMask: v.quickMaskHidden = not visible(not v.quickMaskHidden)
  else: raise bad(cmdId, "that channel has no visibility")
  fixViewNow(st[])
  channelList(s)

proc channelOptions(s: var Session; p: JsonNode): JsonNode =
  let cmdId = "channel.options"
  let doc0 = s.activeOrRaise().doc
  let rr = parseRef(p.pget("channel"), doc0[])
  if rr.isNone: raise bad(cmdId, "missing or unknown `channel`")
  let r = rr.get
  let nm = p.pget("name").asStr
  let name = if nm.isSome and nm.get.strip.len > 0: some(nm.get.strip) else: none(string)
  let color = colorValue(p, "color")
  let opF = p.pget("opacity").asF64
  let opacity = if opF.isSome: some(clamp(float32(opF.get) / 100'f32, 0'f32, 1'f32)) else: none(float32)
  let solF = p.pget("solidity").asF64
  let solidity = if solF.isSome: some(clamp(float32(solF.get) / 100'f32, 0'f32, 1'f32)) else: none(float32)
  let kind = p.pget("indicates").asStr
  proc applyOpts(ch: var AlphaChannel) =
    if name.isSome: ch.name = name.get
    if kind.isSome:
      if kind.get == "spot":
        let (c, sol) = if ch.spot.isSome: ch.spot.get else: (ch.color, 0'f32)
        ch.spot = some((color.get(c), solidity.get(sol)))
        return
      let ind = indicatesParam(%kind.get)
      if ind.isNone: raise bad(cmdId, "unknown `indicates` " & kind.get)
      ch.indicates = ind.get
      ch.spot = none((Color, float32))
    if ch.spot.isSome:
      let (c, sol) = ch.spot.get
      ch.spot = some((color.get(c), (if solidity.isSome: solidity.get elif opacity.isSome: opacity.get else: sol)))
    else:
      ch.color = color.get(ch.color)
      ch.opacity = opacity.get(ch.opacity)
  if r.kind == crAlpha and r.index < doc0.channels.len:
    let i = r.index
    s.editDo("Channel Options"): applyOpts(doc.channels[i])
  elif r.kind == crQuickMask:
    # Quick Mask Options persist for the session; the live Quick Mask follows them.
    var probe = newAlphaChannel("Quick Mask", newSurface(Gray8))
    probe.color = s.quickMaskOptions.color
    probe.opacity = s.quickMaskOptions.opacity
    probe.indicates = s.quickMaskOptions.indicates
    applyOpts(probe)
    if probe.spot.isSome: raise bad(cmdId, "Quick Mask can't be a spot channel")
    s.quickMaskOptions = QuickMaskOptions(color: probe.color, opacity: probe.opacity, indicates: probe.indicates)
    let d = s.active
    if not d.isNil and d.doc.quickMask.isSome:
      let o = s.quickMaskOptions
      s.editDo("Quick Mask Options"):
        let area = doc.bounds
        let fmt = channelFormat(doc)
        let unselected = doc.quickMaskUnselected
        if doc.quickMask.isSome:
          template q: untyped = doc.quickMask.get
          # An untouched no-selection mask stays colourless when the colour switches.
          if unselected and q.indicates != o.indicates:
            let empty = if q.indicates == ciSelectedAreas: 0'f32 else: 1'f32
            if uniform(readPlane(q.surface, area), empty): q.surface = unselectedSurface(area, fmt, o.indicates)
          q.color = o.color
          q.opacity = o.opacity
          q.indicates = o.indicates
  else: raise bad(cmdId, "`channel` must be an alpha/spot channel or \"quickMask\"")
  channelList(s)

proc mergeSpot(s: var Session; p: JsonNode): JsonNode =
  ## Merge Spot Channel: flattens the image and prints the spot ink into the colour channels.
  let cmdId = "channel.mergeSpot"
  let st = s.activeOrRaise()
  let doc0 = st.doc
  var i: int
  if not p.pget("channel").isNil: i = alphaIndex(doc0[], p, "channel", cmdId)
  elif st.channelView.target.kind == ctAlpha: i = st.channelView.target.index
  else:
    i = -1
    for k, c in doc0.channels:
      if c.spot.isSome:
        i = k
        break
    if i < 0: raise bad(cmdId, "no spot channel")
  if i >= doc0.channels.len or doc0.channels[i].spot.isNone: raise bad(cmdId, "not a spot channel")
  let (inkC, solidity) = doc0.channels[i].spot.get
  s.editDo("Merge Spot Channel"):
    let area = doc.bounds
    let fmt = doc.pixelFormat
    let comp = render(doc, area)
    let k = readPlane(doc.channels[i].surface, area)
    let ink = inkC.toRgb
    var data = newSeqOfCap[float32](comp.px.len * fmt.channels)
    for j in 0 ..< min(comp.px.len, k.len):
      let c = comp.px[j]
      let v = clamp(k[j], 0'f32, 1'f32)
      var rgbv: array[3, float32]
      for q in 0 ..< 3:
        let over = solidity * ink[q] + (1 - solidity) * c[q] * ink[q]
        rgbv[q] = c[q] + (over - c[q]) * v
      for x in fromRgba(fmt, [rgbv[0], rgbv[1], rgbv[2], 1'f32]): data.add x
    var bg = rasterLayer("Background", fmt)
    bg.locks.transparency = true
    bg.locks.position = true
    bg.content.raster.writeRegion(area, data)
    bg.content.raster.prune()
    let id = bg.id
    doc.layers = @[bg]
    doc.channels.delete(i)
    active = some(id)
  let st2 = s.active
  if not st2.isNil:
    st2.channelView = ChannelView()
    fixViewNow(st2[])
  jnull()

proc splitChannels(s: var Session; p: JsonNode): JsonNode =
  ## Split Channels: one grayscale document per colour and alpha channel; closes the original.
  if s.activeIndex.isNone: raise noDocument()
  let idx = s.activeIndex.get
  let doc = s.docAt(idx).doc[]
  let area = doc.bounds
  let (px, n) = nativePixels(doc, none(LayerId))
  let names = colorNames(editMode(doc))
  proc short(name: string): string = (if name.len > 0: $name[0].toUpperAscii else: "")
  var docs: seq[Document]
  for c, name in names:
    var plane = newSeq[float32](px.len div max(n, 1))
    for j in 0 ..< plane.len: plane[j] = px[j * n + c]
    docs.add grayDocument(doc.name & "_" & short(name), doc, plane)
  for ch in doc.channels:
    docs.add grayDocument(doc.name & "_" & ch.name, doc, readPlane(ch.surface, area))
  if p.getBoolOr("closeOriginal", true): discard s.close(idx)
  var made = newJArray()
  for d in docs: made.add %s.addDocument(d)
  %*{"documents": made}

proc mergeable(s: Session): seq[int] =
  ## Open grayscale documents with the active document's size, active first.
  let a = s.active
  if a.isNil: return
  proc ok(d: DocState): bool = d.doc.mode == cmGrayscale and d.doc.size == a.doc.size
  if s.activeIndex.isSome and ok(a[]): result.add s.activeIndex.get
  for i, d in s.documents:
    if some(i) != s.activeIndex and ok(d): result.add i

proc mergeChannels(s: var Session; p: JsonNode): JsonNode =
  let cmdId = "channel.merge"
  let m = p.getStrOr("mode", "rgb")
  let mode = case m
    of "rgb": cmRgb
    of "cmyk": cmCmyk
    of "lab": cmLab
    else: raise bad(cmdId, "unknown mode `" & m & "`")
  var srcs: seq[int]
  let docsP = p.pget("documents")
  if docsP.isArray:
    for v in docsP.elems:
      let i = v.asU64
      if i.isNone or i.get >= uint64(s.documents.len): raise bad(cmdId, "bad document index")
      srcs.add int(i.get)
  else:
    for i in mergeable(s):
      if srcs.len < mode.colorChannels: srcs.add i
  if srcs.len < mode.colorChannels:
    raise bad(cmdId, $mode & " needs " & $mode.colorChannels & " grayscale documents, got " & $srcs.len)
  # Each source closes by its current index: refuse a repeated index.
  var seen = initHashSet[int]()
  for i in srcs:
    if i in seen: raise bad(cmdId, "document " & $i & " is listed more than once")
    seen.incl i
  if srcs.len == 0 or srcs[0] >= s.documents.len: raise bad(cmdId, "bad document index")
  let first = s.docAt(srcs[0]).doc[]
  var planes: seq[seq[float32]]
  for i in srcs:
    let d = s.docAt(i).doc
    if d.size != first.size or d.mode != cmGrayscale:
      raise bad(cmdId, "\"" & d.name & "\" is not a grayscale document of the same size")
    planes.add grayPlane(d[], none(LayerId), none(LayerId), chanRef(crComposite))
  var outd = newDocument(p.getStrOr("name", "Untitled"), first.size, mode, first.depth)
  outd.resolutionDpi = first.resolutionDpi
  let fmt = outd.pixelFormat
  let cc = mode.colorChannels
  let npx = planes[0].len
  var data = newSeqOfCap[float32](npx * fmt.channels)
  for i in 0 ..< npx:
    for pl in planes[0 ..< cc]: data.add clamp(pl[i], 0'f32, 1'f32)
    data.add 1
  var bg = rasterLayer("Background", fmt)
  bg.locks.transparency = true
  bg.locks.position = true
  bg.content.raster.writeRegion(outd.bounds, data)
  bg.content.raster.prune()
  outd.layers.add bg
  let area = outd.bounds
  let cf = channelFormat(outd)
  for k in cc ..< planes.len:
    outd.channels.add newAlphaChannel("Alpha " & $(k - cc + 1), planeSurface(planes[k], area, cf))
  var close = srcs
  close.sort()
  for i in countdown(close.len - 1, 0): discard s.close(close[i])
  let i = s.addDocument(outd)
  %*{"document": i}

# ---------------------------------------------------------------- Apply Image / Calculations

proc applyImage(s: var Session; p: JsonNode): JsonNode =
  let cmdId = "image.applyImage"
  var blending: Blending
  try: blending = parseBlending(p)
  except ValueError as e: raise bad(cmdId, e.msg)
  let opacity = float32(clamp(p.getF64Or("opacity", 100.0), 0.0, 100.0)) / 100'f32
  let preserve = p.getBoolOr("preserveTransparency", false)
  let srcSpec = sourceSpec(p, "source", "source").get(newJObject())
  let src = source(s, srcSpec, cmdId, false)
  let mask = maskPlane(s, p, cmdId)
  let st = s.active
  let layer = if st.isNil: none(LayerId) else: st.activeLayer
  s.editDo("Apply Image"):
    let area = doc.bounds
    let selection = if doc.selection.isSome: some(maskFromSurface(doc.selection, area)) else: none(seq[float32])
    proc weight(i: int): float32 =
      opacity * (if mask.isSome: mask.get[i] else: 1'f32) * (if selection.isSome: selection.get[i] else: 1'f32)
    if isChannelTarget(p):
      # A grayscale target gets the luminosity of a colour source (one plane).
      var srcp: seq[float32]
      if src.len == 1: srcp = src[0]
      else:
        srcp = newSeq[float32](src[0].len)
        for i in 0 ..< srcp.len:
          var sum = -0'f32
          for pl in src: sum += pl[i]
          srcp[i] = sum / float32(src.len)
      let (surf, _) = targetSurface(doc, none(LayerId), p)
      let base = readPlane(surf[], area)
      var outp = newSeq[float32](base.len)
      for i, b in base: outp[i] = b + (blending.apply(b, srcp[i]) - b) * weight(i)
      surf[] = planeSurface(outp, area, surf[].format)
      return
    if layer.isNone: raise other("no active layer")
    let fmt = doc.pixelFormat
    let surf = paintSurface(doc, layer.get, newJNull())
    let sf = surf[].format
    if sf != fmt: raise other("the target layer's format doesn't match the document")
    let n = sf.channels
    let colors = sf.mode.colorChannels
    var v = surf[].readRegion(area)
    for i in 0 ..< v.len div max(n, 1):
      let o = i * n
      let baseA = if sf.alpha: clamp(v[o + colors], 0'f32, 1'f32) else: 1'f32
      var w = weight(i)
      if preserve: w *= baseA
      if w <= 0: continue
      # Painted over the layer like a brush: the blend acts where the layer has pixels, and
      # alpha grows by source-over.
      let outA = if preserve: baseA else: w + baseA * (1 - w)
      for c in 0 ..< colors:
        let top = (if c < src.len: src[c] else: src[0])[i]
        let b = v[o + c]
        let blended = top + (blending.apply(b, top) - top) * baseA
        v[o + c] = if preserve: b + (blended - b) * w else: (w * blended + baseA * (1 - w) * b) / outA
      if sf.alpha: v[o + colors] = outA
    surf[].writeRegion(area, v)
    surf[].prune()
  jnull()

proc calculations(s: var Session; p: JsonNode): JsonNode =
  let cmdId = "image.calculations"
  var blending: Blending
  try: blending = parseBlending(p)
  except ValueError as e: raise bad(cmdId, e.msg)
  let opacity = float32(clamp(p.getF64Or("opacity", 100.0), 0.0, 100.0)) / 100'f32
  let s1 = source(s, sourceSpec(p, "source1", "source1").get(newJObject()), cmdId, true)[0]
  let s2 = source(s, sourceSpec(p, "source2", "source2").get(newJObject()), cmdId, true)[0]
  let mask = maskPlane(s, p, cmdId)
  # Source 1 is blended onto Source 2.
  var outp = newSeq[float32](min(s1.len, s2.len))
  for i in 0 ..< outp.len:
    let b = s2[i]
    let t = s1[i]
    outp[i] = b + (blending.apply(b, t) - b) * opacity * (if mask.isSome: mask.get[i] else: 1'f32)
  let name = p.pget("name").asStr
  let res = p.getStrOr("result", "newChannel")
  case res
  of "newChannel":
    let i = s.editGet("Calculations", int):
      let nm = if name.isSome: name.get else: nextAlphaName(doc, "Alpha")
      let area = doc.bounds
      let fmt = channelFormat(doc)
      doc.channels.add newAlphaChannel(nm, planeSurface(outp, area, fmt))
      doc.channels.len - 1
    %*{"channel": i}
  of "newDocument":
    let like = s.activeOrRaise().doc[]
    let d = grayDocument(name.get("Untitled"), like, outp)
    let i = s.addDocument(d)
    %*{"document": i}
  of "selection":
    let selected = s.editGet("Calculations", bool):
      doc.selection = combine(none(Surface), outp, doc.bounds, smReplace)
      doc.selection.isSome
    %*{"selected": selected}
  else: raise bad(cmdId, "unknown result `" & res & "` (newChannel|newDocument|selection)")

proc hasApplyTargetE(s: Session): string =
  if s.editsChannel: return ""
  let d = s.active
  if d.isNil: return "no document open"
  if d.activeLayer.isNone: return "no active layer"
  let l = d.doc[].findLayer(d.activeLayer.get)
  if l.isNil: return "no active layer"
  if l.content.kind == lkRaster: ""
  else: "Apply Image needs a pixel layer (active layer is " & l.content.article & " " & l.content.kindName & " layer)"

proc slotSpec(slot: int): CommandSpec =
  cmd("channel.target.slot" & $slot, "Target Channel " & $slot, [], "Cmd+" & $slot, "{}", slotOk(slot),
      runProc(targetSlot(s, slot)))

proc channelSpecs*(): seq[CommandSpec] =
  result = @[
    cmd("select.saveSelection", "Save Selection…", ["Select"], "",
        """{"name":str?,"channel":"new"|index|name="new","document":index?,"operation":"new|replace|add|subtract|intersect"="new"}""",
        hasSelectionE, saveSelection),
    cmd("select.loadSelection", "Load Selection…", ["Select"], "",
        """{"channel":index|name|"composite"|"red|green|blue|…"|"transparency"|"mask"|"vectorMask"|"quickMask"|"selection","layer":id?,"document":index?,"invert":bool=false,"operation":"new|add|subtract|intersect"="new"}""",
        hasDocE, loadSelection),
    cmd("select.editInQuickMaskMode", "Edit in Quick Mask Mode", ["Select"], "Q", """{"on":bool?=toggle}""", hasDocE, quickMaskCmd),
    cmd("image.applyImage", "Apply Image…", ["Image"], "",
        """{"source":{"document":index?,"layer":id|"merged"="merged","channel":"composite"|"red|…"|index|"selection"|"transparency"|"mask"="composite","invert":bool=false},"blending":"normal|multiply|screen|overlay|softLight|hardLight|colorDodge|colorBurn|darken|lighten|difference|exclusion|linearBurn|linearDodge|add|subtract|…"="multiply","opacity":0..100=100,"scale":1..2=1,"offset":-255..255=0,"preserveTransparency":bool=false,"mask":{"document","layer","channel","invert"}?,"sourceChannel|sourceDocument|sourceLayer|sourceInvert|maskChannel|…":flat form of source/mask?}""",
        hasApplyTargetE, applyImage),
    cmd("image.calculations", "Calculations…", ["Image"], "",
        """{"source1":{"document","layer","channel","invert"},"source2":{…},"blending":"multiply|…"="multiply","opacity":0..100=100,"scale":1..2=1,"offset":-255..255=0,"mask":{…}?,"result":"newChannel|newDocument|selection"="newChannel","name":str?}""",
        hasDocE, calculations),
    cmd("channel.new", "New Channel…", [], "",
        """{"name":str?,"fill":"black|white|selection"="black","color":"#rrggbb"="#ff0000","opacity":0..100=50,"indicates":"masked|selected"="masked"}""",
        hasDocE, newChannelCmd),
    cmd("channel.newSpot", "New Spot Channel…", [], "",
        """{"name":str?,"color":"#rrggbb"="#ff0000","solidity":0..100=0,"fromSelection":bool=true}""", hasDocE, newSpotCmd),
    cmd("channel.duplicate", "Duplicate Channel…", [], "",
        """{"channel":index|name|"red|…"|"composite"|"quickMask","name":str?,"document":index|"new"?=active,"invert":bool=false}""",
        hasDocE, duplicateChannel),
    cmd("channel.delete", "Delete Channel", [], "", """{"channel":index|name?=targeted}""", hasChannelsE, deleteChannel),
    cmd("channel.rename", "Rename Channel", [], "", """{"channel":index|name,"name":str}""", hasChannelsE, renameChannel),
    cmd("channel.move", "Reorder Channel", [], "", """{"channel":index|name,"to":index}""", hasChannelsE, moveChannel),
    cmd("channel.target", "Target Channel", [], "",
        """{"channel":"composite"|"mask"|"vectorMask"|"red|green|blue|…"|index|name="composite"}""", hasDocE, targetCmd),
    cmd("channel.setVisible", "Channel Visibility", [], "",
        """{"channel":"composite"|"red|…"|index|name|"quickMask","visible":bool?=toggle}""", hasDocE, setVisibleCmd),
    cmd("channel.options", "Channel Options…", [], "",
        """{"channel":index|name|"quickMask","name":str?,"indicates":"masked|selected|spot"?,"color":"#rrggbb"?,"opacity":0..100?,"solidity":0..100?}""",
        hasDocE, channelOptions),
    cmd("channel.mergeSpot", "Merge Spot Channel", [], "", """{"channel":index|name?=targeted or first spot}""", hasSpotE, mergeSpot),
    cmd("channel.split", "Split Channels", [], "", """{"closeOriginal":bool=true}""", canSplitE, splitChannels),
    cmd("channel.merge", "Merge Channels…", [], "",
        """{"mode":"rgb|cmyk|lab"="rgb","documents":[index,…]?=open grayscale docs of the active size,"name":str?}""",
        canMergeE, mergeChannels),
    cmd("channel.list", "Channels", [], "", "{}", hasDocE, runProc(channelList(s)), journal = false),
    cmd("channel.target.composite", "Target Composite Channel", [], "Cmd+2", "{}", hasDocE, runProc(targetSlot(s, 2))),
  ]
  for n in 3 .. 9: result.add slotSpec(n)
