## Everyday Photoshop commands that build on the core ones (port of `extra_cmds.rs`): Edit ›
## Stroke, the fixed Transform presets, Paste Into/Outside, Reselect, Equalize, Reveal All, Layer
## from Background, Copy/Paste Layer Style, Hide All Effects, layer-mask toggles, Rasterize
## variants, Delete Hidden / Empty Layers, Ungroup, Hide/Show Layers, Show Only This Layer,
## Average and Clouds. Part of the `engine/core` unit: included, not imported.

proc isBackground*(l: Layer): bool =
  l.name == "Background" and l.locks.transparency and l.locks.position and l.content.kind == lkRaster

proc xActiveLayer(s: Session): (ptr Layer, string) =
  let d = s.active
  if d.isNil: return (nil, "no document open")
  if d.activeLayer.isNone: return (nil, "no active layer")
  let l = d.doc[].findLayer(d.activeLayer.get)
  if l.isNil: (nil, "no active layer") else: (l, "")

proc xHasLayerE(s: Session): string = xActiveLayer(s)[1]

proc xHasPixelsE(s: Session): string =
  let (l, why) = xActiveLayer(s)
  if l.isNil: return why
  if l.content.kind == lkRaster: "" else: "active layer is " & l.content.article & " " & l.content.kindName & " layer, not a pixel layer"

proc xHasMaskE(s: Session): string =
  let (l, why) = xActiveLayer(s)
  if l.isNil: return why
  if l.mask.isSome: "" else: "the active layer has no layer mask"

proc hasClipAndSelectionE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if s.clipboard.isNone: return "the clipboard is empty"
  if d.doc.selection.isSome: "" else: "Paste Into needs a selection"

proc hasBackgroundE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if d.doc.layers.len > 0 and isBackground(d.doc.layers[0]): "" else: "the document has no Background layer"

proc styleTargets*(s: Session; p: JsonNode; fits: proc(l: Layer): bool): seq[LayerId] =
  ## The layers a layer-style command acts on: as `targets`, but with several layers selected
  ## only those that `fits` (the Background carries no style).
  let ids = targets(s, p)
  if ids.len < 2: return ids
  let d = s.activeOrRaise()
  for id in ids:
    let l = d.doc[].findLayer(id)
    if not l.isNil and not isBackground(l[]) and fits(l[]): result.add id
  if result.len == 0: raise other("none of the selected layers can take this layer style change")

proc stepKey*(s: Session; what: string): string =
  ## A key that makes the sub-commands of one command share a single history step.
  what & "#" & $(if s.active.isNil: 0'u64 else: s.active.revision)

# ---------------------------------------------------------------- pixel helpers

proc mapPixels(s: var Session; label: string; p: JsonNode;
               f: proc(area: Rect; px: var seq[array[4, float32]]; cover: seq[float32])): JsonNode =
  ## Rewrites the active pixel layer's RGBA over the selection bounds (or the canvas), blended
  ## back by coverage; a targeted channel or mask is rewritten instead.
  let id = layerParam(s, newJNull())
  let channel = targetOf(p).kind != tgPixels
  s.editDo(label):
    let canvas = doc.bounds
    let selection = doc.selection
    let area = if selection.isSome: selection.get.contentBounds.intersect(canvas) else: canvas
    var surf: ptr Surface
    var lockAlpha = false
    if channel:
      surf = channelSurfaceForFilter(doc, some(id), p)
      if surf.isNil: return
    else:
      let locks = doc.effectiveLocks(id)
      let l = doc.layerOrRaise(id)
      if locks.pixels or locks.all:
        raise other("Could not complete your request because the layer \"" & l.name & "\" is locked")
      surf = l[].surfaceMut
      if surf.isNil: raise other("not a pixel layer")
      lockAlpha = locks.transparency
    if area.isEmpty: return
    let fmt = surf[].format
    let n = fmt.channels
    var raw = surf[].readRegion(area)
    var orig = newSeq[array[4, float32]](raw.len div n)
    for i in 0 ..< orig.len: orig[i] = toRgba(fmt, raw.toOpenArray(i * n, i * n + n - 1))
    let w = area.width
    var cover = newSeq[float32](orig.len)
    for i in 0 ..< orig.len:
      cover[i] = if selection.isSome: selection.get.sampleChannel(area.x0 + i mod w, area.y0 + i div w, 0) else: 1'f32
    var px = orig
    f(area, px, cover)
    var enc: array[8, float32]
    for i in 0 ..< orig.len:
      let k = cover[i]
      if k <= 0: continue
      var v: array[4, float32]
      for c in 0 ..< 4: v[c] = orig[i][c] + (px[i][c] - orig[i][c]) * k
      if lockAlpha: v[3] = orig[i][3]
      discard fromRgbaInto(fmt, v, enc)
      for c in 0 ..< n: raw[i * n + c] = enc[c]
    surf[].writeRegion(area, raw)
    surf[].prune()
  jnull()

proc strokeCmd(s: var Session; p: JsonNode): JsonNode =
  ## Edit › Stroke: a band along the selection edge (or the layer's opaque edge).
  let wv = p.pget("width").asF64
  let width = int(ceil(clamp(if wv.isSome and wv.get == wv.get and abs(wv.get) != Inf: wv.get else: 1.0, 1.0, 250.0)))
  let color = colorParam(p, "color", s.tools.foreground)
  var mode = bmNormal
  let mv = p.pget("mode")
  if not mv.isNull:
    if mv.kind == JString:
      let m = blendFromStr(mv.str)
      if m.isNone or m.get == bmPassThrough: raise bad("edit.stroke", "unknown blend mode `" & mv.str & "`")
      mode = m.get
    else: raise bad("edit.stroke", "mode must be a blend mode name, not " & jsonText(mv))
  var opacity = 1'f32
  let ov = p.pget("opacity")
  if not ov.isNull:
    let o = ov.asF64
    if o.isNone or o.get != o.get or abs(o.get) == Inf: raise bad("edit.stroke", "opacity must be a finite number from 0 to 100")
    opacity = float32(clamp(o.get, 0.0, 100.0) / 100.0)
  var preserve = false
  let pv = p.pget("preserveTransparency")
  if not pv.isNull:
    if pv.kind != JBool: raise bad("edit.stroke", "preserveTransparency must be true or false")
    preserve = pv.bval
  let location = p.getStrOr("location", "center")
  let id = layerParam(s, p)
  s.editDo("Stroke"):
    let canvas = doc.bounds
    let w = canvas.width
    let h = canvas.height
    var edge: seq[float32]
    if doc.selection.isSome: edge = maskFromSurface(doc.selection, canvas)
    else:
      let l = doc.findLayer(id)
      if l.isNil: raise noLayer(id)
      if not l[].hasSurface: raise other("not a pixel layer")
      var px = newSeq[Rgba](w * h)
      l[].surface.readRgbaInto(canvas, px)
      edge = newSeq[float32](px.len)
      for i, q in px: edge[i] = q[3]
    # Past the canvas nothing is selected: stroke on a padded copy.
    let pad = width + 1
    let pw = w + 2 * pad
    let ph = h + 2 * pad
    var padded = newSeq[float32](pw * ph)
    for y in 0 ..< h:
      for x in 0 ..< w: padded[(y + pad) * pw + pad + x] = edge[y * w + x]
    let (outPx, inPx) = case location
      of "inside": (0, width)
      of "outside": (width, 0)
      else: (width div 2, (width + 1) div 2)
    var hard = true
    for v in padded:
      if not (v <= 0 or v >= 1): hard = false
    let inR = if hard and inPx > 0: float32(inPx) - 0.5'f32 else: float32(inPx)
    var outer = selection.expand(padded, pw, ph, float32(outPx))
    var inner = selection.contract(padded, pw, ph, inR)
    if hard:
      for v in outer.mitems: v = if v > 0: 1'f32 else: 0'f32
      for v in inner.mitems: v = if v >= 1: 1'f32 else: 0'f32
    var band = newSeq[float32](w * h)
    for y in 0 ..< h:
      for x in 0 ..< w:
        let k = (y + pad) * pw + pad + x
        band[y * w + x] = clamp(outer[k] - inner[k], 0'f32, 1'f32)
    let bandS = maskToSurface(band, canvas)
    let lock = doc.effectiveLocks(id).transparency
    let surf = paintSurface(doc, id, newJNull())
    let area = bandS.contentBounds.intersect(canvas)
    if not area.isEmpty:
      blendColorMask(surf[], area, color, bandS, mode, opacity, preserve or lock)
      surf[].prune()
  jnull()

proc equalizeCmd(s: var Session; p: JsonNode): JsonNode =
  ## Histogram equalization: one CDF over the R, G and B values of the selected pixels.
  mapPixels(s, "Equalize", p, proc(area: Rect; px: var seq[array[4, float32]]; cover: seq[float32]) =
    var hist: array[256, float64]
    for i, q in px:
      if cover[i] > 0 and q[3] > 0:
        for c in 0 ..< 3: hist[int(round(clamp(q[c], 0'f32, 1'f32) * 255'f32))] += 1.0
    var total = -0.0
    for v in hist: total += v
    if total == 0: return
    var first = 0.0
    for v in hist:
      if v > 0:
        first = v
        break
    var lut: array[256, float32]
    var acc = 0.0
    for i in 0 ..< 256:
      acc += hist[i]
      lut[i] = if total > first: float32(clamp((acc - first) / (total - first), 0.0, 1.0)) else: float32(i) / 255'f32
    for q in px.mitems:
      for c in 0 ..< 3: q[c] = lut[int(round(clamp(q[c], 0'f32, 1'f32) * 255'f32))])

proc averageCmd(s: var Session; p: JsonNode): JsonNode =
  ## Filter › Blur › Average: the selection (or layer) filled with its mean colour.
  mapPixels(s, "Average", p, proc(area: Rect; px: var seq[array[4, float32]]; cover: seq[float32]) =
    var sum: array[3, float64]
    var wsum, asum, n = 0.0
    for i, q in px:
      let k = cover[i]
      let w = float64(q[3] * k)
      for c in 0 ..< 3: sum[c] += float64(q[c]) * w
      wsum += w
      asum += float64(q[3] * k)
      n += float64(k)
    if wsum <= 0: return
    let avg = [float32(sum[0] / wsum), float32(sum[1] / wsum), float32(sum[2] / wsum), float32(asum / rmax(n, 1e-9))]
    for q in px.mitems: q = avg)

proc cloudHash(x, y: int; seed: uint32): float32 =
  var h = (cast[uint32](int32(x)) * 0x8da6_b343'u32) xor (cast[uint32](int32(y)) * 0xd816_3841'u32) xor (seed * 0xcb1a_b31f'u32)
  h = h xor (h shr 13)
  h = h * 0x5bd1_e995'u32
  h = h xor (h shr 15)
  float32(h and 0x00ff_ffff'u32) / float32(0x00ff_ffff)

proc cloudsValue*(x, y, base: float32; seed: uint32): float32 =
  ## Smooth value noise, summed over octaves (fBm), in 0..1.
  var sum = 0'f32
  var amp = 1'f32
  var freq = 1'f32 / base
  var norm = 0'f32
  for o in 0'u32 ..< 8:
    let fx = x * freq
    let fy = y * freq
    let ix = asI32(float64(floor(fx)))
    let iy = asI32(float64(floor(fy)))
    let tx = fx - float32(ix)
    let ty = fy - float32(iy)
    let sx = tx * tx * (3'f32 - 2'f32 * tx)
    let sy = ty * ty * (3'f32 - 2'f32 * ty)
    let sd = seed + o * 7919'u32
    let top = cloudHash(ix, iy, sd) + (cloudHash(ix + 1, iy, sd) - cloudHash(ix, iy, sd)) * sx
    let bot = cloudHash(ix, iy + 1, sd) + (cloudHash(ix + 1, iy + 1, sd) - cloudHash(ix, iy + 1, sd)) * sx
    sum += (top + (bot - top) * sy) * amp
    norm += amp
    amp *= 0.5'f32
    freq *= 2'f32
  clamp((sum / norm - 0.5'f32) * 2.2'f32 + 0.5'f32, 0'f32, 1'f32)

proc cloudsCmd(s: var Session; p: JsonNode; difference: bool): JsonNode =
  ## Filter › Render › Clouds / Difference Clouds, between the foreground and background colours.
  let fg = s.tools.foreground
  let bg = s.tools.background
  let seed = uint32(p.pget("seed").asU64.get(0) and 0xffff_ffff'u64)
  let size = if s.active.isNil: 256'f32 else: float32(max(s.active.doc.size.width, s.active.doc.size.height))
  let base = clamp(size / 4'f32, 16'f32, 512'f32)
  mapPixels(s, (if difference: "Difference Clouds" else: "Clouds"), p,
    proc(area: Rect; px: var seq[array[4, float32]]; cover: seq[float32]) =
      let w = area.width
      for i in 0 ..< px.len:
        let x = float32(area.x0 + i mod w)
        let y = float32(area.y0 + i div w)
        let t = cloudsValue(x, y, base, seed)
        var c: array[3, float32]
        for k in 0 ..< 3: c[k] = fg[k] + (bg[k] - fg[k]) * t
        if difference:
          for k in 0 ..< 3: px[i][k] = abs(px[i][k] - c[k])
        else: px[i] = [c[0], c[1], c[2], 1'f32])

# ---------------------------------------------------------------- transform presets

const TransformTarget = """{"target":"pixels"|"mask"|"quickMask"|{"channel":i}=Channels panel target (an unlinked mask, an alpha channel or the Quick Mask turns alone)}"""


proc transformPreset(s: var Session; p: JsonNode; kind: string): JsonNode =
  let d = s.activeOrRaise()
  var b: Rect
  var params: JsonNode
  let lone = loneTarget(d.doc[], d.activeLayer, p)
  if lone.isSome:
    b = targetBounds(d.doc[], lone.get)
    params = %*{"target": (if p.pget("target").isNil: newJNull() else: p.pget("target"))}
  else:
    if d.activeLayer.isNone: raise other("no active layer")
    let id = d.activeLayer.get
    let l = d.doc[].findLayer(id)
    if l.isNil: raise noLayer(id)
    b = transformBounds(d.doc[], l[])
    params = %*{"layer": id}
  if b.isEmpty: raise other("nothing to transform")
  # Integer offsets keep these exact pixel permutations on the grid (nearest sampling).
  let sx = float64(b.x0 + b.x1)
  let sy = float64(b.y0 + b.y1)
  let cx = sx / 2.0
  let cy = sy / 2.0
  let m = case kind
    of "rotate180": [-1.0, 0.0, 0.0, -1.0, sx, sy]
    of "rotate90Cw": [0.0, 1.0, -1.0, 0.0, round(cx + cy), round(cy - cx)]
    of "rotate90Ccw": [0.0, -1.0, 1.0, 0.0, round(cx - cy), round(cx + cy)]
    of "flipHorizontal": [-1.0, 0.0, 0.0, 1.0, sx, 0.0]
    else: [1.0, 0.0, 0.0, -1.0, 0.0, sy]
  params["matrix"] = %m
  params["interpolation"] = %"nearest"
  s.execute("edit.transform", params)

# ---------------------------------------------------------------- selection / clipboard

proc reselectTarget(s: Session): Option[Surface] =
  let d = s.active
  if d.isNil or d.doc.selection.isSome: return
  for i in countdown(d.history.pastLen - 1, 0):
    let doc = d.history.state(i)
    if doc.isSome and doc.get.selection.isSome: return doc.get.selection

proc canReselectE(s: Session): string =
  if s.active.isNil: return "no document open"
  if reselectTarget(s).isSome: "" else: "there is no selection to restore"

proc pasteInto(s: var Session; p: JsonNode; outside: bool): JsonNode =
  let d = s.activeOrRaise()
  if d.doc.selection.isNone: raise other("Paste Into needs a selection")
  let selection = d.doc.selection.get
  let canvas = d.doc[].bounds
  let b = selection.contentBounds
  # Where the paste shows: the selection, or everything but it.
  var limit: Surface
  if outside:
    let area = b.intersect(canvas)
    var inv = maskFromSurface(some(selection), area)
    for v in inv.mitems: v = 1 - v
    limit = surfaceWithDefault(Gray8, [1'f32])
    limit.writeRegion(area, inv)
  else: limit = selection
  let label = if outside: "Paste Outside" else: "Paste Into"
  var params = %*{"center": [float64(b.x0 + b.x1) / 2.0, float64(b.y0 + b.y1) / 2.0]}
  if not p.pget("center").isNil: params["center"] = p.pget("center")
  let k = pasteColorTarget(s, p)
  if k.isSome: return channelPaste(s, params, k.get, false, some(limit), label)
  if targetOf(p).kind != tgPixels:
    params["target"] = (if p.pget("target").isNil: newJNull() else: p.pget("target"))
    return pasteToTarget(s, params, false, some(limit), label)
  let key = stepKey(s, "pasteInto")
  params["coalesce"] = %key
  params["target"] = %"pixels"
  let before = s.activeOrRaise().doc[]
  let r = s.execute("edit.paste", params)
  let id = layerParam(s, newJNull())
  discard s.execute("layer.layerMask.revealAll", %*{"layer": id, "coalesce": key})
  s.coalesceRequest = some(key)
  try:
    s.editDo(label):
      let l = doc.layerOrRaise(id)
      var m = revealAllMask()
      m.surface = limit
      m.linked = false
      l.mask = some(m)
  finally:
    s.coalesceRequest = none(string)
  noteInsert(s, before)
  r

proc revealAllBounds*(doc: Document): Rect =
  ## The canvas grown to every layer's pixels.
  result = doc.bounds
  for (_, l) in doc.walkPtrs:
    if l[].hasSurface: result = result.union(l[].surface.contentBounds)

# ---------------------------------------------------------------- layers

proc unlockBackground*(l: var Layer) =
  l.name = "Layer 0"
  l.locks.transparency = false
  l.locks.position = false

proc backgroundToLayerForMask*(doc: var Document; id: LayerId) =
  ## Before `id` gets a layer mask: the Background becomes a normal layer ("Layer 0") first.
  if doc.layers.len > 0 and doc.layers[0].id == id and isBackground(doc.layers[0]):
    unlockBackground(doc.layers[0])

proc layerFromBackground(s: var Session): JsonNode =
  let id = s.editGet("Layer From Background", LayerId):
    if doc.layers.len == 0 or not isBackground(doc.layers[0]): raise other("the document has no Background layer")
    unlockBackground(doc.layers[0])
    active = some(doc.layers[0].id)
    doc.layers[0].id
  %*{"layer": id}

proc toggleMask(s: var Session; p: JsonNode; key: string): JsonNode =
  let id = layerParam(s, p)
  let label = if key == "enabled": "Enable/Disable Layer Mask" else: "Link/Unlink Layer Mask"
  let v = s.editGet(label, bool):
    let l = doc.layerPtr(id)
    if l.isNil or l.mask.isNone: raise other("the layer has no layer mask")
    template m: untyped = l.mask.get
    if key == "enabled":
      m.enabled = p.getBoolOr(key, not m.enabled)
      m.enabled
    else:
      m.linked = p.getBoolOr(key, not m.linked)
      m.linked
  result = newJObject()
  result[key] = %v

proc setAllEffectsRec(ls: var seq[Layer]; on: bool) =
  for l in ls.mitems:
    if l.effects.items.len > 0: l.effects.enabled = on
    if l.content.kind == lkGroup: setAllEffectsRec(l.content.group.children, on)

proc setAllEffects(s: var Session; enabled: bool): JsonNode =
  s.editDo(if enabled: "Show All Effects" else: "Hide All Effects"): setAllEffectsRec(doc.layers, enabled)
  jnull()

proc deleteWhereRec(ls: var seq[Layer]; pred: proc(l: Layer): bool): int =
  let before = ls.len
  ls.keepItIf(not pred(it))
  result = before - ls.len
  for l in ls.mitems:
    if l.content.kind == lkGroup: result += deleteWhereRec(l.content.group.children, pred)

proc deleteWhere(s: var Session; label: string; pred: proc(l: Layer): bool): JsonNode =
  ## Removes every layer matching `pred` (whole subtrees), keeping at least one layer.
  let n = s.editGet(label, int):
    let n = deleteWhereRec(doc.layers, pred)
    if doc.layers.len == 0: raise other("a document must keep at least one layer")
    if active.isNone or doc.findLayer(active.get).isNil: active = doc.topLayer
    n
  %*{"deleted": n}

proc isEmptyLayer(l: Layer): bool =
  case l.content.kind
  of lkRaster: l.content.raster.contentBounds.isEmpty and l.mask.isNone and l.effects.items.len == 0
  of lkGroup:
    for c in l.content.group.children:
      if not isEmptyLayer(c): return false
    true
  else: false

proc ungroupCmd(s: var Session; p: JsonNode): JsonNode =
  let id = layerParam(s, p)
  s.editDo("Ungroup Layers"):
    let path = doc.pathOf(id)
    if path.isNone or path.get.len == 0: raise noLayer(id)
    let g = doc.findLayer(id)
    if g.isNil: raise noLayer(id)
    if g.content.kind != lkGroup: raise other("the active layer is not a group")
    let children = g.content.group.children
    let (sib, idx) = doc.siblingsAt(path.get)
    if sib.isNil: raise noLayer(id)
    sib[].delete(idx)
    let top = if children.len > 0: some(children[^1].id) else: none(LayerId)
    for k, l in children: sib[].insert(l, idx + k)
    active = if top.isSome: top else: doc.topLayer
  jnull()

proc setVisibleCmd2(s: var Session; p: JsonNode; visible: bool): JsonNode =
  if multi(s, p): return setVisibleSelected(s, visible)
  let id = layerParam(s, p)
  s.editDo(if visible: "Show Layer" else: "Hide Layer"): doc.layerOrRaise(id).visible = visible
  jnull()

proc shownAlone(doc: Document; path: LayerPath): bool =
  ## Is the layer at `path` shown alone: it and its groups visible, every layer outside hidden?
  for (p, l) in doc.walkPtrs:
    let ok = if path.startsWith(p): l.visible else: p.startsWith(path) or not l.visible
    if not ok: return false
  true

proc showOnly(s: var Session; p: JsonNode): JsonNode =
  ## ⌥-click on a layer's eye: show only that layer, or restore every layer's visibility.
  let id = layerParam(s, p)
  let st = s.activeOrRaise()
  let doc0 = st.doc
  let path = doc0[].pathOf(id)
  if path.isNone: raise noLayer(id)
  var saved = none((LayerId, seq[(LayerId, bool)]))
  if st.showOnly.isSome:
    let pp = doc0[].pathOf(st.showOnly.get[0])
    if pp.isSome and shownAlone(doc0[], pp.get): saved = st.showOnly
  var walk: seq[(LayerPath, LayerId, bool)]
  for (wp, l) in doc0[].walkPtrs: walk.add (wp, l.id, l.visible)
  let solo = not shownAlone(doc0[], path.get)
  var rows: seq[(LayerPath, bool)]
  var next = none((LayerId, seq[(LayerId, bool)]))
  var label: string
  if solo:
    var snapshot: seq[(LayerId, bool)]
    if saved.isSome: snapshot = saved.get[1]
    else:
      for (_, lid, vis) in walk: snapshot.add (lid, vis)
    for (wp, _, _) in walk:
      if wp.len <= path.get.len or not wp.startsWith(path.get): rows.add (wp, path.get.startsWith(wp))
    label = "Show Only This Layer"
    next = some((id, snapshot))
  else:
    var before = initTable[uint64, bool]()
    if saved.isSome:
      for (lid, v) in saved.get[1]: before[uint64(lid)] = v
    for (wp, lid, vis) in walk:
      rows.add (wp, if saved.isNone: true else: before.getOrDefault(uint64(lid), vis))
    label = "Show Layers"
  s.editDo(label):
    for (rp, visible) in rows:
      let l = doc.layerAtPtr(rp)
      if not l.isNil: l.visible = visible
  let st2 = s.active
  if not st2.isNil: st2.showOnly = next
  %*{"shownAlone": solo}

proc contentPixels(doc: Document; l: Layer): Surface =
  ## Pixels of a fill or smart-object layer's content alone (no mask, effects or opacity).
  var tmp = l
  tmp.mask = none(LayerMask)
  tmp.vectorMask = none(VectorMask)
  tmp.effects.items.setLen(0)
  tmp.opacity = 1
  tmp.fillOpacity = 1
  tmp.blend = bmNormal
  tmp.clipped = false
  tmp.visible = true
  let canvas = doc.bounds
  let buf = renderLayer(tmp, canvas)
  let fmt = doc.pixelFormat
  let n = fmt.channels
  var data = newSeq[float32](buf.px.len * n)
  for i, q in buf.px: discard fromRgbaInto(fmt, q, data.toOpenArray(i * n, i * n + n - 1))
  result = newSurface(fmt)
  result.writeRegion(canvas, data)
  result.prune()

proc kindOf*(s: Session; id: LayerId): Option[string] =
  let d = s.active
  if d.isNil: return
  let l = d.doc[].findLayer(id)
  if l.isNil: return
  case l.content.kind
  of lkText: some("type")
  of lkShape: some("shape")
  of lkFill: some("fill")
  of lkSmart: some("smart")
  else: none(string)

proc rasterizeContent(s: var Session; id: LayerId; only: Option[string]; key: string): bool =
  let kind = kindOf(s, id)
  if kind.isNone or (only.isSome and only.get != kind.get): return false
  case kind.get
  of "type": discard s.execute("type.rasterize", %*{"layer": id, "coalesce": key})
  of "shape": discard s.execute("layer.rasterize.shape", %*{"layer": id, "coalesce": key})
  else:
    let label = if kind.get == "fill": "Rasterize Fill Content" else: "Rasterize Smart Object"
    s.coalesceRequest = some(key)
    try:
      s.editDo(label):
        let snap = doc.findLayer(id)
        if snap.isNil: raise noLayer(id)
        let px = contentPixels(doc, snap[])
        let l = doc.layerOrRaise(id)
        l.content = rasterContent(px)
        l.fillCache = none(FillCache)
    finally:
      s.coalesceRequest = none(string)
  true

proc rasterizeOne*(s: var Session; id: LayerId; only: Option[string]; key: string): bool =
  ## Rasterizes one layer; `only` restricts it to one kind. Without `only` a vector mask becomes
  ## a pixel mask too. False when there was nothing to do.
  let d = s.active
  var vectorMask = false
  if only.isNone and not d.isNil:
    let l = d.doc[].findLayer(id)
    vectorMask = not l.isNil and l.vectorMask.isSome
  let content = rasterizeContent(s, id, only, key)
  if vectorMask: discard s.execute("layer.rasterize.vectorMask", %*{"layer": id, "coalesce": key})
  content or vectorMask

proc rasterizeCmd*(s: var Session; p: JsonNode; only: Option[string]): JsonNode =
  ## Layer › Rasterize › Layer / Type / Shape / Fill Content / Smart Object.
  let ids = targets(s, p)
  let many = ids.len > 1
  let key = stepKey(s, "rasterize")
  var done: seq[uint64]
  for id in ids:
    if rasterizeOne(s, id, only, key): done.add uint64(id)
  if done.len == 0:
    if only.isSome and not many: raise other("the layer is not a " & only.get & " layer")
    if only.isSome: raise other("none of the selected layers is a " & only.get & " layer")
    if not many: raise other("the layer has nothing to rasterize")
    raise other("none of the selected layers has anything to rasterize")
  let d = s.active
  var act = done[0]
  if not d.isNil and d.activeLayer.isSome and uint64(d.activeLayer.get) in done: act = uint64(d.activeLayer.get)
  %*{"layer": act, "layers": done}

proc rasterizeAll*(s: var Session): JsonNode =
  var ids: seq[LayerId]
  let d = s.active
  if not d.isNil:
    for (_, l) in d.doc[].walkPtrs: ids.add l.id
  let key = stepKey(s, "rasterizeAll")
  var n = 0
  for id in ids:
    if rasterizeOne(s, id, none(string), key): inc n
  %*{"rasterized": n}

proc pasteLayerStyle(s: var Session; p: JsonNode): JsonNode =
  let ids = styleTargets(s, p, proc(l: Layer): bool = true)
  if s.styleClipboard.isNone: raise other("no layer style has been copied")
  var (fx, blend, fill, advanced) = s.styleClipboard.get
  s.editDo("Paste Layer Style"):
    # A style copied from a document in another mode takes this one's colours.
    let mode = doc.mode
    var items: seq[Effect]
    for e in fx.items: items.add e.inMode(mode)
    fx.items = items
    for id in ids:
      let l = doc.layerOrRaise(id)
      l.effects = fx
      l.blend = blend
      l.fillOpacity = fill
      l.advanced = advanced
  jnull()

proc extraSpecs*(): seq[CommandSpec] =
  @[
    cmd("edit.stroke", "Stroke…", ["Edit"], "",
        """{"width":1..250=1,"color":"#rrggbb|[r,g,b,a]"=foreground,"location":"inside|center|outside"="center","mode":"any layer blend mode"="normal","opacity":0..100=100,"preserveTransparency":bool=false}""",
        xHasPixelsE, strokeCmd),
    cmd("edit.transform.rotate180", "Rotate 180°", ["Edit", "Transform"], "", TransformTarget, xHasLayerE,
        runProc(transformPreset(s, p, "rotate180"))),
    cmd("edit.transform.rotate90Cw", "Rotate 90° Clockwise", ["Edit", "Transform"], "", TransformTarget, xHasLayerE,
        runProc(transformPreset(s, p, "rotate90Cw"))),
    cmd("edit.transform.rotate90Ccw", "Rotate 90° Counter Clockwise", ["Edit", "Transform"], "", TransformTarget, xHasLayerE,
        runProc(transformPreset(s, p, "rotate90Ccw"))),
    cmd("edit.transform.flipHorizontal", "Flip Horizontal", ["Edit", "Transform"], "", TransformTarget, xHasLayerE,
        runProc(transformPreset(s, p, "flipHorizontal"))),
    cmd("edit.transform.flipVertical", "Flip Vertical", ["Edit", "Transform"], "", TransformTarget, xHasLayerE,
        runProc(transformPreset(s, p, "flipVertical"))),
    cmd("edit.pasteSpecial.pasteInto", "Paste Into", ["Edit", "Paste Special"], "Cmd+Alt+Shift+V",
        "{\"center\":[x,y]?," & PasteTarget & "}", hasClipAndSelectionE, runProc(pasteInto(s, p, false))),
    cmd("edit.pasteSpecial.pasteOutside", "Paste Outside", ["Edit", "Paste Special"], "",
        "{\"center\":[x,y]?," & PasteTarget & "}", hasClipAndSelectionE, runProc(pasteInto(s, p, true))),
    cmd("select.reselect", "Reselect", ["Select"], "Cmd+Shift+D", "{}", canReselectE, runProc do:
      let m = reselectTarget(s)
      if m.isNone: raise other("there is no selection to restore")
      let sel = m.get
      s.editDo("Reselect"): doc.selection = some(sel)
      %*{"selected": true}),
    cmd("image.adjustments.equalize", "Equalize", ["Image", "Adjustments"], "", "{}", xHasPixelsE, equalizeCmd),
    cmd("image.revealAll", "Reveal All", ["Image"], "", "{}", hasDocE, runProc do:
      let d = s.activeOrRaise()
      let all = revealAllBounds(d.doc[])
      if all == d.doc[].bounds: return %*{"changed": false}
      discard s.execute("image.crop", %*{"x": all.x0, "y": all.y0, "width": all.width, "height": all.height,
                                         "deleteCroppedPixels": false})
      %*{"changed": true}),
    cmd("layer.new.layerFromBackground", "Layer From Background…", ["Layer", "New"], "", "{}", hasBackgroundE,
        runProc(layerFromBackground(s))),
    cmd("layer.layerStyle.copyLayerStyle", "Copy Layer Style", ["Layer", "Layer Style"], "", """{"layer":id?}""", xHasLayerE, runProc do:
      let id = layerParam(s, p)
      let d = s.active
      let l = if d.isNil: nil else: d.doc[].findLayer(id)
      if l.isNil: raise noLayer(id)
      s.styleClipboard = some((l.effects, l.blend, l.fillOpacity, l.advanced))
      jnull()),
    cmd("layer.layerStyle.pasteLayerStyle", "Paste Layer Style", ["Layer", "Layer Style"], "",
        """{"layer":id?} (no layer: every selected layer but the Background)""",
        enabledProc do:
          let e = xHasLayerE(s)
          if e.len > 0: return e
          if s.styleClipboard.isSome: "" else: "no layer style has been copied",
        pasteLayerStyle),
    cmd("layer.layerStyle.hideAllEffects", "Hide All Effects", ["Layer", "Layer Style"], "", "{}", hasDocE, runProc(setAllEffects(s, false))),
    cmd("layer.layerStyle.showAllEffects", "Show All Effects", [], "", "{}", hasDocE, runProc(setAllEffects(s, true))),
    cmd("layer.layerMask.enabled", "Disable Layer Mask", ["Layer", "Layer Mask"], "",
        """{"layer":id?,"enabled":bool? (default: toggle)}""", xHasMaskE, runProc(toggleMask(s, p, "enabled"))),
    cmd("layer.layerMask.linked", "Unlink Layer Mask", ["Layer", "Layer Mask"], "",
        """{"layer":id?,"linked":bool? (default: toggle)}""", xHasMaskE, runProc(toggleMask(s, p, "linked"))),
    cmd("layer.rasterize.layer", "Layer", ["Layer", "Rasterize"], "", """{"layer":id?} (no layer: every selected layer it applies to)""",
        xHasLayerE, runProc(rasterizeCmd(s, p, none(string)))),
    cmd("layer.rasterize.allLayers", "All Layers", ["Layer", "Rasterize"], "", "{}", hasDocE, runProc(rasterizeAll(s))),
    cmd("layer.rasterize.type", "Type", ["Layer", "Rasterize"], "", """{"layer":id?} (no layer: every selected layer it applies to)""",
        xHasLayerE, runProc(rasterizeCmd(s, p, some("type")))),
    cmd("layer.rasterize.fillContent", "Fill Content", ["Layer", "Rasterize"], "",
        """{"layer":id?} (no layer: every selected layer it applies to)""", xHasLayerE, runProc(rasterizeCmd(s, p, some("fill")))),
    cmd("layer.rasterize.smartObject", "Smart Object", ["Layer", "Rasterize"], "",
        """{"layer":id?} (no layer: every selected layer it applies to)""", xHasLayerE, runProc(rasterizeCmd(s, p, some("smart")))),
    cmd("layer.delete.hiddenLayers", "Hidden Layers", ["Layer", "Delete"], "", "{}", hasDocE,
        runProc(deleteWhere(s, "Delete Hidden Layers", proc(l: Layer): bool = not l.visible))),
    cmd("file.scripts.deleteAllEmptyLayers", "Delete All Empty Layers", ["File", "Scripts"], "", "{}", hasDocE,
        runProc(deleteWhere(s, "Delete All Empty Layers", isEmptyLayer))),
    cmd("layer.ungroupLayers", "Ungroup Layers", ["Layer"], "Cmd+Shift+G", """{"layer":id?}""",
        enabledProc do:
          let (l, why) = xActiveLayer(s)
          if l.isNil: return why
          if l[].isGroup: "" else: "the active layer is not a group",
        ungroupCmd),
    cmd("layer.hideLayers", "Hide Layers", ["Layer"], "Cmd+,", """{"layer":id?}""", xHasLayerE, runProc(setVisibleCmd2(s, p, false))),
    cmd("layer.showLayers", "Show Layers", [], "", """{"layer":id?}""", xHasLayerE, runProc(setVisibleCmd2(s, p, true))),
    cmd("layer.showOnly", "Show Only This Layer", [], "",
        """{"layer":id?} → {shownAlone} (⌥-click a layer's eye; again restores the other layers' visibility)""", xHasLayerE, showOnly),
    cmd("filter.blur.average", "Average", ["Filter", "Blur"], "", "{}", xHasPixelsE, averageCmd),
    cmd("filter.render.clouds", "Clouds", ["Filter", "Render"], "", """{"seed":u32=0}""", xHasPixelsE, runProc(cloudsCmd(s, p, false))),
    cmd("filter.render.differenceClouds", "Difference Clouds", ["Filter", "Render"], "", """{"seed":u32=0}""", xHasPixelsE,
        runProc(cloudsCmd(s, p, true))),
  ]
