## Brush engine commands (`brush_cmds.rs`): the Brush/Pencil/Mixer Brush/Color Replacement tools,
## brush presets and the session brush (`tools.setBrush`). Part of the `engine/core` unit:
## included, not imported.
##
## Brushes are `BrushSettings` serialised as camelCase JSON; any command taking a `"brush"` object
## deep-merges it onto the session brush, so partial objects work. Strokes are deterministic: the
## jitter seed is the `seed` param or a hash of the points, so a journaled command replays to
## identical pixels. Live strokes (the canvas preview while drawing) land with the UI.

const
  MaxCoord* = 1_000_000.0 ## largest stroke coordinate accepted
  BlockScreenPx* = 16.0   ## edge of the Eraser's Block in screen pixels
  MaxTip* = 2500'u32      ## largest sampled tip side

proc bnum(p: JsonNode; k: string): Option[float32] =
  let v = p.pget(k).asF64
  if v.isSome: some(float32(v.get)) else: none(float32)

proc bflag(p: JsonNode; k: string; d: bool): bool = p.pget(k).asBool.get(d)

proc brushColor(v: JsonNode; d: array[4, float32]): array[4, float32] =
  if v.isNil: return d
  case v.kind
  of JArray:
    if v.len < 3: return d
    var c: seq[float32]
    for x in v.elems: c.add float32(x.asF64.get(0.0))
    [c[0], c[1], c[2], (if c.len > 3: c[3] else: 1'f32)]
  of JString:
    var h = v.str
    while h.startsWith("#"): h = h[1 .. ^1]
    let (r, g, b) = (hexU8(h, 0), hexU8(h, 2), hexU8(h, 4))
    if r.isSome and g.isSome and b.isSome and (h.len == 6 or h.len == 8):
      [r.get, g.get, b.get, hexU8(h, 6).get(1'f32)]
    else: d
  else: d

proc checkCoords*(pts: openArray[StrokePoint]; cmdId: string) =
  ## A stroke runs dab by dab along its length: an absurd coordinate would mean billions of dabs.
  for q in pts:
    if not (abs(q.x) <= MaxCoord and abs(q.y) <= MaxCoord):
      raise bad(cmdId, "point coordinates must be finite and within ±1000000")

proc parsePoints*(p: JsonNode; cmdId: string): seq[StrokePoint] =
  ## `points`: arrays `[x, y, pressure?, tiltX?, tiltY?, rotation?, timeMs?, wheel?]` or objects
  ## `{"x":…, "y":…, "pressure":…, "tiltX":…, …, "time":…}`.
  let arr = p.pget("points")
  if not arr.isArray: raise bad(cmdId, "missing `points`")
  for v in arr.elems:
    case v.kind
    of JArray:
      template g(i: int): Option[float64] = (if i < v.len: v.elems[i].asF64 else: none(float64))
      if g(0).isNone or g(1).isNone: continue
      var sp = strokePoint(g(0).get, g(1).get, float32(g(2).get(1.0)))
      sp.tiltX = float32(g(3).get(0.0))
      sp.tiltY = float32(g(4).get(0.0))
      sp.rotation = float32(g(5).get(0.0))
      sp.time = g(6).get(0.0)
      sp.wheel = float32(g(7).get(1.0))
      result.add sp
    of JObject:
      var sp = defaultStrokePoint()
      try:
        withSerdeStrict: fromJ(v, sp)
        result.add sp
      except SerdeError: discard
    else: discard
  if result.len == 0: raise bad(cmdId, "`points` is empty")
  checkCoords(result, cmdId)

proc mergeJson(base: var JsonNode; patch: JsonNode) =
  ## Deep-merge `patch` into `base` (objects merge key by key; anything else replaces).
  if base.kind == JObject and patch.kind == JObject:
    if base.len == 1 and patch.len == 1:
      var bk, pk: string
      for k in base.fields.keys: bk = k
      for k in patch.fields.keys: pk = k
      # A different variant of an externally tagged enum replaces the old one.
      if bk != pk:
        base = patch.copy
        return
    for k, v in patch.fields:
      let bv = base.fields.getOrDefault(k)
      if not bv.isNil and bv.kind == JObject and v.kind == JObject: mergeJson(base.fields[k], v)
      else: base[k] = v.copy
  else: base = patch.copy

proc mergeBrush*(base: BrushSettings; patch: JsonNode; cmdId: string): BrushSettings =
  ## Apply a JSON brush patch onto a brush.
  # Bitmaps the patch doesn't touch skip the JSON round trip.
  proc touches(path: openArray[string]): bool =
    var v = patch
    for k in path:
      let x = v.pget(k)
      if x.isNil: return not v.isObject
      v = x
    true
  var light = base
  let keepTip = not touches(["tip"])
  let keepDual = not touches(["dualBrush", "tip"])
  let keepPattern = not touches(["texture", "pattern"])
  if keepTip: light.tip = TipShape(kind: tkRound)
  if keepDual: light.dualBrush.tip = TipShape(kind: tkRound)
  if keepPattern: light.texture.pattern = defaultPattern()
  var v = toJ(light)
  mergeJson(v, patch)
  try:
    withSerdeStrict: result = brushFromJson(v)
  except SerdeError as e: raise bad(cmdId, "invalid brush: " & e.msg)
  if keepTip: result.tip = base.tip
  if keepDual: result.dualBrush.tip = base.dualBrush.tip
  if keepPattern: result.texture.pattern = base.texture.pattern

proc validateBrush*(brush: BrushSettings; cmdId: string) =
  ## Refuses brush sizes and scatter amounts the rasterizer can't take.
  let mx = MaxBrushSize
  if brush.size != brush.size or abs(brush.size) == Inf or brush.size > mx:
    raise bad(cmdId, "brush size must be finite and at most " & displayFloat(mx) & " px")
  if brush.dualBrush.enabled and (brush.dualBrush.size != brush.dualBrush.size or abs(brush.dualBrush.size) == Inf or
                                   brush.dualBrush.size > mx):
    raise bad(cmdId, "dual brush size must be finite and at most " & displayFloat(mx) & " px")
  # Scatter may exceed 1 (100 %) but not the 1000 % maximum.
  proc inRange(v: float32): bool = v >= 0 and v <= MaxScatter
  let sc = brush.scattering.scatter.jitter
  if brush.scattering.enabled and not inRange(sc):
    raise bad(cmdId, "scatter amount " & debugFloat(sc) & " is out of range: it must be between 0 and " &
      displayFloat(MaxScatter) & " (1000 %)")
  let ds = brush.dualBrush.scatter
  if brush.dualBrush.enabled and not inRange(ds):
    raise bad(cmdId, "dual brush scatter amount " & debugFloat(ds) & " is out of range: it must be between 0 and " &
      displayFloat(MaxScatter) & " (1000 %)")

proc findPreset(s: Session; name, cmdId: string): BrushPreset =
  let pr = presets.find(s.tools.presets, name)
  if pr.isNone: raise bad(cmdId, "no brush preset named `" & name & "`")
  pr.get

proc loadBrushTips*(s: Session; b: var BrushSettings): string =
  ## Loads a library preset's stored tips (no preset store is attached in this build: a stored tip
  ## that isn't loaded is unavailable). "" when the brush is ready.
  proc check(r: StoredTile): string =
    if r.full.isNil: "brush tip " & r.key & " is not available: no preset store is attached" else: ""
  if b.tip.kind == tkStored:
    let e = check(b.tip.stored)
    if e.len > 0: return e
  if b.dualBrush.tip.kind == tkStored:
    let e = check(b.dualBrush.tip.stored)
    if e.len > 0: return e
  if b.texture.pattern.kind == pkStored:
    let e = check(b.texture.pattern.stored)
    if e.len > 0: return e
  ""

proc brushPresetsChanged*(s: var Session) = inc s.tools.presetsRev

proc resolveBrush*(s: Session; p: JsonNode; cmdId: string): BrushSettings =
  ## The brush for a stroke command: session brush ← `preset` ← `brush` ← legacy scalar params;
  ## colours from the session; seed from `seed` or the points.
  var b = s.tools.brush
  let name = p.pget("preset").asStr
  if name.isSome: b = findPreset(s, name.get, cmdId).brush.pickedOver(s.tools.brush)
  let patch = p.pget("brush")
  if patch.isObject: b = mergeBrush(b, patch, cmdId)
  let e = s.loadBrushTips(b)
  if e.len > 0: raise bad(cmdId, e)
  let size = bnum(p, "size")
  if size.isSome: b.size = max(size.get, 0.5'f32)
  let hardness = bnum(p, "hardness")
  if hardness.isSome: b.hardness = clamp(hardness.get, 0'f32, 1'f32)
  let opacity = bnum(p, "opacity")
  if opacity.isSome: b.opacity = clamp(opacity.get, 0'f32, 1'f32)
  let flow = bnum(p, "flow")
  if flow.isSome: b.flow = clamp(flow.get, 0'f32, 1'f32)
  let spacing = bnum(p, "spacing")
  if spacing.isSome: b.spacing = clamp(spacing.get, 0.01'f32, 10'f32)
  let smoothing = bnum(p, "smoothing")
  if smoothing.isSome: b.smoothing.amount = clamp(smoothing.get, 0'f32, 1'f32)
  b.color = brushColor(p.pget("color"), s.tools.foreground)
  b.background = s.tools.background
  b.erase = bflag(p, "erase", false)
  let seed = p.pget("seed").asU64
  b.seed = (if seed.isSome: seed.get
            else: seedFromBytes(if p.pget("points").isNil: "" else: jsonText(p.pget("points"))))
  validateBrush(b, cmdId)
  b

proc brushLayerId(s: Session; p: JsonNode): LayerId =
  let v = p.pget("layer").asU64
  if v.isSome: return LayerId(v.get)
  let st = s.active
  if st.isNil or st.activeLayer.isNone: raise other("no active layer")
  st.activeLayer.get

proc damageJson(s: var Session; dmg: Rect): JsonNode =
  let st = s.active
  if not st.isNil: st.lastDamage = some(dmg)
  %*{"damage": [dmg.x0, dmg.y0, dmg.width, dmg.height]}

proc strokeTarget(s: Session; p: JsonNode; brush: BrushSettings): (Option[LayerId], BrushSettings, float32) =
  ## The target layer (none for a channel), brush and zoom of a stroke. On a mask or channel the
  ## eraser paints the background colour.
  let gray = isMaskTarget(p) or isChannelTarget(p)
  let id = (if isChannelTarget(p): none(LayerId) else: some(brushLayerId(s, p)))
  var b = brush
  if gray and b.erase:
    b.erase = false
    b.color = s.tools.background
  (id, b, bnum(p, "zoom").get(1'f32))

proc eraseLocked(brush: var BrushSettings; lock: bool; bg: array[4, float32]) =
  ## Erasing a layer with locked transparency paints the background colour instead.
  if brush.erase and lock:
    brush.erase = false
    brush.color = bg

proc applyAutoErase(brush: var BrushSettings; surf: Surface; start: Option[StrokePoint];
                    fg, bg: array[4, float32]) =
  ## Pencil Auto Erase: a stroke that starts on a pixel of the foreground colour paints the
  ## background colour instead.
  if start.isNone: return
  let p0 = start.get
  let c = surf.rgba(asI32(floor(p0.x)), asI32(floor(p0.y)))
  if c[3] > 0 and abs(c[0] - fg[0]) < 1.5'f32 / 255 and abs(c[1] - fg[1]) < 1.5'f32 / 255 and
      abs(c[2] - fg[2]) < 1.5'f32 / 255:
    brush.color = bg

proc strokeWith(s: var Session; cmdId: string; p: JsonNode; label: string; brush0: BrushSettings;
                pts: seq[StrokePoint]; autoErase: bool): JsonNode =
  ## Stroke with a resolved brush onto the target layer (pixels, mask or channel).
  let bg = s.tools.background
  let fg = brush0.color
  let st0 = s.active
  let symmetry = (if st0.isNil: none(PaintingSymmetry) else: st0.symmetry)
  let (id, brush1, zoom) = strokeTarget(s, p, brush0)
  let cmyk = (if st0.isNil: nil else: cmykSpace(st0.doc[]))
  var dmg: Rect
  withCmykSpace(cmyk):
    dmg = s.editWith(label, proc(doc: var Document; active: var Option[LayerId]): Rect =
      let sel = doc.selection
      let (surf, lock) = targetSurface(doc, id, p)
      var brush = brush1
      eraseLocked(brush, lock, bg)
      if autoErase: applyAutoErase(brush, surf[], (if pts.len > 0: some(pts[0]) else: none(StrokePoint)), fg, bg)
      if symmetry.isSome:
        let reflected = symmetry.get.reflectedPasses(pts)
        var copies: seq[StrokeRenderer]
        for rp in reflected:
          if hasDistinctMirror(pts, rp):
            var renderer = newStrokeRenderer(brush, some(surf[].format), zoom)
            renderer.push(rp)
            renderer.finish()
            copies.add renderer
        if copies.len > 0:
          let pre = surf[]
          var original = newStrokeRenderer(brush, some(surf[].format), zoom)
          original.push(pts)
          original.finish()
          return original.compositeUnionMany(copies, pre, surf[], sel, lock)
      renderStroke(surf[], brush, pts, sel, lock, zoom))
  damageJson(s, dmg)

proc blockSize*(zoom: float64): float32 =
  ## The Eraser's Block edge in document pixels at view `zoom`: `16 / zoom`, rounded.
  let z = (if zoom == zoom and abs(zoom) != Inf and zoom > 0: zoom else: 1.0)
  float32(clamp(round(BlockScreenPx / z), 1.0, float64(MaxBrushSize)))

proc blockBrush(b: BrushSettings; p: JsonNode): BrushSettings =
  ## The Eraser's Block mode: a hard, aliased square of 16 screen pixels at the stroke's `zoom`.
  var zoom = 1.0
  let z = p.pget("zoom")
  if not z.isNil:
    let v = z.asF64
    if v.isNone or not (v.get == v.get and abs(v.get) != Inf and v.get > 0):
      raise bad("paint.pencil", "`zoom` must be a positive number")
    zoom = v.get
  result = defaultBrush()
  result.size = blockSize(zoom)
  result.aliased = true
  result.square = true
  result.pressureSize = false
  result.pressureOpacity = false
  result.spacing = 0.25
  result.color = b.color
  result.background = b.background
  result.erase = b.erase
  result.smoothing = b.smoothing
  result.seed = b.seed

proc clearMode(p: JsonNode): bool =
  ## Photoshop's Clear painting mode (`"mode": "clear"`): erase with the brush.
  let m = p.pget("mode").asStr
  m.isSome and cmpIgnoreCase(m.get, "clear") == 0

proc withBlendMode(b0: BrushSettings; p: JsonNode): BrushSettings =
  ## Applies the options-bar blend `mode` to a brush (`clear` erases with the brush).
  result = b0
  result.erase = result.erase or clearMode(p)
  if result.erase: result.mode = bmNormal
  else:
    let m = p.pget("mode").asStr
    if m.isSome:
      let bm = blendFromStr(m.get)
      if bm.isSome: result.mode = bm.get

proc pencilBrush(s: Session; p: JsonNode): BrushSettings =
  ## The Pencil's brush: aliased, hard and at full flow, with the options-bar mode.
  result = withBlendMode(resolveBrush(s, p, "paint.pencil"), p)
  if bflag(p, "block", false): return blockBrush(result, p)
  result.aliased = true
  if bnum(p, "hardness").isNone: result.hardness = 1
  if bnum(p, "flow").isNone: result.flow = 1

proc paintStroke*(s: var Session; p: JsonNode): JsonNode =
  ## `paint.stroke`: the Brush (and Eraser) tool.
  let pts = parsePoints(p, "paint.stroke")
  let brush = withBlendMode(resolveBrush(s, p, "paint.stroke"), p)
  let label = (if brush.erase and not clearMode(p): "Eraser" else: "Brush Tool")
  strokeWith(s, "paint.stroke", p, label, brush, pts, false)

proc pencilCmd(s: var Session; p: JsonNode): JsonNode =
  let pts = parsePoints(p, "paint.pencil")
  let brush = pencilBrush(s, p)
  let label = (if brush.erase and not clearMode(p): "Eraser" else: "Pencil")
  strokeWith(s, "paint.pencil", p, label, brush, pts, bflag(p, "autoErase", false))

proc pct(p: JsonNode; k: string; d: float32): float32 = clamp(bnum(p, k).get(d), 0'f32, 100'f32) / 100

proc mixerBrushCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "paint.mixerBrush"
  let pts = parsePoints(p, cmdId)
  let brush = resolveBrush(s, p, cmdId)
  # The brush's Mixer Brush options, overridden by the scalar params.
  let bm = brush.mixer
  let m = MixerSettings(wet: pct(p, "wet", bm.wet * 100), load: pct(p, "load", bm.load * 100),
                        mix: pct(p, "mix", bm.mix * 100), flow: pct(p, "flow", bm.flow * 100),
                        sampleAllLayers: bflag(p, "sampleAllLayers", bm.sampleAllLayers))
  let sampleAll = m.sampleAllLayers
  let clean = bflag(p, "cleanAfterStroke", true)
  let loadAfter = bflag(p, "loadAfterStroke", true)
  var state = s.tools.mixer
  if loadAfter or state.reservoir.isNone: state.load(brushColor(p.pget("color"), s.tools.foreground))
  let id = (if isChannelTarget(p): none(LayerId) else: some(brushLayerId(s, p)))
  let stroke = Stroke(brush: brush, points: pts)
  var st = state
  let dmg = s.editWith("Mixer Brush", proc(doc: var Document; active: var Option[LayerId]): Rect =
    let sel = doc.selection
    var sample = none(Surface)
    if sampleAll:
      let ds = dabs(stroke)
      let ctx = newBrushContext(stroke.brush)
      var area = EmptyRect
      for d in ds: area = area.union(ctx.dabRect(d, false))
      let buf = compose.render(doc, area)
      var surf = newSurface(Rgba32f)
      var flat = newSeq[float32](buf.px.len * 4)
      for i, px in buf.px:
        for c in 0 ..< 4: flat[i * 4 + c] = px[c]
      if not area.isEmpty: surf.writeRegion(area, flat)
      sample = some(surf)
    let (surf, lock) = targetSurface(doc, id, p)
    applyMixerStroke(surf[], sample, stroke, m, st, sel, lock))
  if clean: st.clean()
  s.tools.mixer = st
  damageJson(s, dmg)

proc parseSerdeKey[T](p: JsonNode; cmdId, key: string; d: T): T =
  ## `p.get(key).cloned().map(serde_json::from_value)`, serde's error named after the key.
  result = d
  let v = p.pget(key)
  if v.isNil: return
  try:
    withSerdeStrict: fromJ(v, result)
  except SerdeError as e: raise bad(cmdId, "`" & key & "`: " & e.msg)

proc colorReplacementSetup(s: Session; p: JsonNode): (Stroke, ReplaceSettings, LayerId) =
  ## `paint.colorReplacement`'s stroke and settings. It paints layer pixels only.
  const cmdId = "paint.colorReplacement"
  let t = p.pget("target")
  if not t.isNil and t.asStr != some("pixels"):
    raise bad(cmdId, "Color Replacement paints layer pixels only (`target` " & jsonText(t) & " is not \"pixels\")")
  let pts = parsePoints(p, cmdId)
  let brush = resolveBrush(s, p, cmdId)
  var rs = defaultReplaceSettings()
  rs.mode = parseSerdeKey(p, cmdId, "mode", rpColor)
  rs.sampling = parseSerdeKey(p, cmdId, "sampling", smContinuous)
  rs.limits = parseSerdeKey(p, cmdId, "limits", lmContiguous)
  rs.tolerance = pct(p, "tolerance", 30)
  rs.antiAlias = bflag(p, "antiAlias", true)
  rs.color = brush.color
  rs.background = s.tools.background
  let id = brushLayerId(s, p)
  (Stroke(brush: brush, points: pts), rs, id)

proc colorReplacementCmd(s: var Session; p: JsonNode): JsonNode =
  let (stroke, rs, id) = colorReplacementSetup(s, p)
  let dmg = s.editWith("Color Replacement", proc(doc: var Document; active: var Option[LayerId]): Rect =
    let sel = doc.selection
    let lock = doc.effectiveLocks(id).transparency
    let surf = paintSurface(doc, id, newJObject())
    applyColorReplacement(surf[], stroke, rs, sel, lock))
  damageJson(s, dmg)

proc brushJson*(b: BrushSettings): JsonNode = toJ(b)

proc presetsList(s: var Session; p: JsonNode): JsonNode =
  let full = bflag(p, "full", false)
  var list = newJArray()
  for pr in s.tools.presets:
    var v = %*{"name": pr.name, "builtin": pr.builtin, "size": pr.brush.size, "hardness": pr.brush.hardness,
               "tip": (if pr.brush.tip.kind == tkRound: "round" else: "sampled")}
    if full:
      # Full settings embed the bitmaps (a tip that can't be loaded stays a reference).
      var b = pr.brush
      discard s.loadBrushTips(b)
      v["brush"] = brushJson(b)
    list.add v
  %*{"presets": list}

proc nameParam(p: JsonNode; cmdId: string): string =
  let v = p.pget("name").asStr
  if v.isSome:
    let n = unicode.strip(v.get)
    if n.len > 0: return n
  raise bad(cmdId, "missing `name`")

proc upsert(s: var Session; preset: BrushPreset) =
  ## Replace the preset of that name, or append it at the end of the list.
  var found = false
  for x in s.tools.presets.mitems:
    if cmpIgnoreCase(x.name, preset.name) == 0:
      x = preset
      found = true
      break
  if not found: s.tools.presets.add preset
  s.brushPresetsChanged()

proc presetsSave(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "brush.presets.save"
  let name = nameParam(p, cmdId)
  let patch = p.pget("brush")
  let brush = (if patch.isObject: mergeBrush(s.tools.brush, patch, cmdId) else: s.tools.brush)
  upsert(s, BrushPreset(name: name, brush: brush, builtin: false))
  # The saved brush is the current brush now (Photoshop selects the brush it just saved).
  s.tools.currentPreset = some(name)
  %*{"name": name, "count": s.tools.presets.len}

proc presetsDelete(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "brush.presets.delete"
  let name = nameParam(p, cmdId)
  let before = s.tools.presets.len
  s.tools.presets.keepItIf(cmpIgnoreCase(it.name, name) != 0)
  if s.tools.presets.len == before: raise bad(cmdId, "no brush preset named `" & name & "`")
  if s.tools.currentPreset.isSome and cmpIgnoreCase(s.tools.currentPreset.get, name) == 0:
    s.tools.currentPreset = none(string)
  s.brushPresetsChanged()
  %*{"count": s.tools.presets.len}

proc presetsUpdate(s: var Session; p: JsonNode): JsonNode =
  ## Overwrite an existing preset with the current brush (or the current brush plus a `brush`
  ## patch), in place. A built-in that is updated becomes the user's preset.
  const cmdId = "brush.presets.update"
  var name: string
  if not p.pget("name").isNull: name = nameParam(p, cmdId)
  elif s.tools.currentPreset.isSome: name = s.tools.currentPreset.get
  else: raise bad(cmdId, "the current brush was not picked from a preset")
  let patch = p.pget("brush")
  let brush = (if patch.isObject: mergeBrush(s.tools.brush, patch, cmdId) else: s.tools.brush)
  var idx = -1
  for i, x in s.tools.presets:
    if cmpIgnoreCase(x.name, name) == 0:
      idx = i
      break
  if idx < 0: raise bad(cmdId, "no brush preset named `" & name & "`")
  s.tools.presets[idx].brush = brush
  s.tools.presets[idx].builtin = false
  let (n, group) = (s.tools.presets[idx].name, s.tools.presets[idx].group)
  s.brushPresetsChanged()
  %*{"name": n, "group": group}

proc presetsSetCurrent(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "brush.presets.setCurrent"
  let name = nameParam(p, cmdId)
  if not s.tools.presets.anyIt(cmpIgnoreCase(it.name, name) == 0):
    raise bad(cmdId, "no brush preset named `" & name & "`")
  s.tools.currentPreset = some(name)
  %*{"name": name}

proc hasSelectionAndPixelsE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document"
  if d.doc.selection.isNone: return "no selection"
  if d.activeLayer.isNone: return "no active layer"
  let l = d.doc[].findLayer(d.activeLayer.get)
  if l.isNil: return "no active layer"
  if l[].content.kind == lkRaster: "" else: "active layer is not a pixel layer"

proc defineFromSelection(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "brush.defineFromSelection"
  let name = nameParam(p, cmdId)
  let d = s.active
  if d.isNil: raise noDocument()
  if d.doc.selection.isNone: raise bad(cmdId, "no selection")
  let sel = d.doc.selection.get
  if d.activeLayer.isNone: raise other("no active layer")
  let l = d.doc[].findLayer(d.activeLayer.get)
  if l.isNil or not l[].hasSurface: raise bad(cmdId, "not a pixel layer")
  let surf = l[].surface
  let area = sel.contentBounds.intersect(d.doc[].bounds)
  if area.isEmpty: raise bad(cmdId, "the selection is empty")
  # Paint amount = darkness × alpha × selection (black = full paint, like Photoshop).
  let (w, h) = (area.width, area.height)
  var v = newSeq[float32](w * h)
  let sc = sel.channels
  let sv = sel.readRegion(area)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let c = surf.rgba(area.x0 + x, area.y0 + y)
      let lv = lum([c[0], c[1], c[2]])
      v[y * w + x] = clamp((1 - lv) * c[3] * sv[(y * w + x) * sc], 0'f32, 1'f32)
  # Crop to the painted extent.
  var (x0, y0, x1, y1) = (w, h, 0, 0)
  for y in 0 ..< h:
    for x in 0 ..< w:
      if v[y * w + x] > 1e-4'f32:
        x0 = min(x0, x)
        y0 = min(y0, y)
        x1 = max(x1, x + 1)
        y1 = max(y1, y + 1)
  if x1 <= x0 or y1 <= y0: raise bad(cmdId, "the selected pixels contain no paint (all white or transparent)")
  let (tw, th) = (uint32(x1 - x0), uint32(y1 - y0))
  if max(tw, th) > MaxTip:
    raise bad(cmdId, "selection too large for a brush tip (" & $tw & "×" & $th & "; max " & $MaxTip & " px)")
  var data: seq[float32]
  for y in y0 ..< y1: data.add v.toOpenArray(y * w + x0, y * w + x1 - 1)
  var brush = defaultBrush()
  brush.tip = TipShape(kind: tkSampled, sampled: grayTileFromF32(tw, th, data))
  brush.size = float32(max(tw, th))
  brush.spacing = 0.25
  brush.pressureSize = false
  upsert(s, BrushPreset(name: name, brush: brush, builtin: false))
  s.tools.brush = brush
  s.tools.currentPreset = some(name)
  %*{"name": name, "width": tw, "height": th}

proc setBrush*(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "tools.setBrush"
  var b = s.tools.brush
  var picked = none(string)
  let name = p.pget("preset").asStr
  if name.isSome:
    b = findPreset(s, name.get, cmdId).brush.pickedOver(s.tools.brush)
    picked = name
  let reset = bflag(p, "reset", false)
  if reset: b = defaultBrush()
  var patch = (if p.isNil: newJNull() else: p.copy)
  if patch.isObject:
    for k in ["preset", "reset", "coalesce"]: patch.dropKey(k)
    let inner = patch.pget("brush")
    if not inner.isNil:
      patch.dropKey("brush")
      let tmp = mergeBrush(b, inner, cmdId)
      # Before the second merge: a non-finite value would serialise as `null` there and be reported
      # as a type error instead of naming the value.
      validateBrush(tmp, cmdId)
      b = tmp
  b = mergeBrush(b, patch, cmdId)
  validateBrush(b, cmdId)
  # The picked preset stays "the current brush" across later edits. Only once the brush is
  # accepted (a `reset` wins over a `preset`).
  if reset: s.tools.currentPreset = none(string)
  elif picked.isSome: s.tools.currentPreset = picked
  # The tool's brush always paints with full tips.
  let e = s.loadBrushTips(b)
  if e.len > 0: raise bad(cmdId, e)
  let before = s.tools.brush
  s.tools.brush = b
  # A coalesced gesture (one slider drag) journals as one call: remember the brush it started from.
  let key = p.pget("coalesce").asStr
  let keyed = key.isSome and p.pget("preset").isNil and p.pget("reset").isNil
  if keyed:
    let k = key.get
    let continuing = s.tools.brushGesture.isSome and s.tools.brushGesture.get[0] == k and s.journal.len > 0 and
      s.journal[^1][0] == cmdId and s.journal[^1][1].pget("coalesce").asStr == some(k)
    if not continuing: s.tools.brushGesture = some((k, before))
  else: s.tools.brushGesture = none((string, BrushSettings))
  brushJson(s.tools.brush)

proc lightJson(b: BrushSettings; skipTip, skipDual, skipPattern: bool): JsonNode =
  ## The brush as JSON with the bitmaps `skip` names replaced by placeholders.
  var c = b
  if skipTip: c.tip = TipShape(kind: tkRound)
  if skipDual: c.dualBrush.tip = TipShape(kind: tkRound)
  if skipPattern: c.texture.pattern = defaultPattern()
  toJ(c)

proc brushPatch*(old, new: BrushSettings): JsonNode =
  ## Fields of `new` that differ from `old`, nested objects diffed key by key: the minimal
  ## `tools.setBrush` `brush` patch that turns `old` into `new` (`{}` when nothing changed).
  let skipTip = old.tip == new.tip
  let skipDual = old.dualBrush.tip == new.dualBrush.tip
  let skipPattern = old.texture.pattern == new.texture.pattern
  proc diff(a, b: JsonNode): JsonNode =
    if a.kind == JObject and b.kind == JObject:
      var o = newJObject()
      for k, bv in b.sortedPairs:
        let av = a.fields.getOrDefault(k)
        if av.isNil: o[k] = bv.copy
        else:
          let d = diff(av, bv)
          if not d.isNil: o[k] = d
      return (if o.len > 0: o else: nil)
    # Enums with payloads (tips, patterns) are replaced whole.
    if a != b: b.copy else: nil
  let d = diff(lightJson(old, skipTip, skipDual, skipPattern), lightJson(new, skipTip, skipDual, skipPattern))
  if d.isNil: newJObject() else: d

proc brushCoalesceJournal*(s: var Session; id: string; params: JsonNode): bool =
  ## Consecutive `tools.setBrush` calls with the same `coalesce` key are one journal entry: the
  ## patch from the brush before the gesture to the brush now.
  if id != "tools.setBrush": return false
  let key = params.pget("coalesce").asStr
  if key.isNone or s.tools.brushGesture.isNone: return false
  let (gk, start) = s.tools.brushGesture.get
  if gk != key.get: return false
  let patch = brushPatch(start, s.tools.brush)
  if s.journal.len > 0 and s.journal[^1][0] == id and s.journal[^1][1].pget("coalesce").asStr == key:
    s.journal[^1][1] = %*{"brush": patch, "coalesce": key.get}
    return true
  false

proc alwaysE(s: Session): string = ""

proc brushSpecs*(): seq[CommandSpec] =
  @[
    cmd("paint.pencil", "Pencil", [], "",
        "{\"points\":[[x,y,pressure?,tiltX?,tiltY?,rotation?,timeMs?,wheel?],…],\"brush\":{…}?,\"preset\":name?,\"size\":0.5..5000 px?,\"opacity\":0..1?,\"color\":\"#rrggbb\"?=foreground,\"mode\":\"normal|multiply|screen|…|clear (erases with this brush)\"=\"normal\",\"erase\":bool?,\"block\":bool=false (a hard 16×16 screen-px square, 16/zoom doc px; size, opacity, flow, hardness and dynamics ignored: the Eraser's Block mode),\"zoom\":view zoom=1,\"autoErase\":bool=false,\"seed\":u64?,\"target\":\"pixels\"|\"mask\"|\"quickMask\"|{\"channel\":i}=Channels panel target}",
        hasPaintableE, runProc(pencilCmd(s, p))),
    cmd("paint.mixerBrush", "Mixer Brush", [], "",
        "{\"points\":[…],\"brush\":{…}?,\"preset\":name?,\"size\":0.5..5000 px?,\"wet\":0..100=brush.mixer.wet,\"load\":0..100=brush.mixer.load,\"mix\":0..100=brush.mixer.mix,\"flow\":0..100=brush.mixer.flow,\"color\":\"#rrggbb\"?=foreground,\"sampleAllLayers\":bool=brush.mixer.sampleAllLayers,\"cleanAfterStroke\":bool=true,\"loadAfterStroke\":bool=true,\"seed\":u64?}",
        hasPaintableE, runProc(mixerBrushCmd(s, p))),
    cmd("paint.colorReplacement", "Color Replacement", [], "",
        "{\"points\":[…],\"brush\":{…}?,\"size\":px?,\"mode\":\"hue|saturation|color|luminosity\"=\"color\",\"sampling\":\"continuous|once|backgroundSwatch\"=\"continuous\",\"limits\":\"contiguous|discontiguous|findEdges\"=\"contiguous\",\"tolerance\":0..100=30,\"antiAlias\":bool=true,\"color\":\"#rrggbb\"?=foreground,\"target\":\"pixels\"=\"pixels\",\"seed\":u64?}",
        hasPaintableE, runProc(colorReplacementCmd(s, p))),
    cmd("brush.presets.list", "List Brush Presets", [], "", "{\"full\":bool=false}", alwaysE,
        runProc(presetsList(s, p)), journal = false),
    cmd("brush.presets.save", "Save Brush Preset", [], "", "{\"name\":string,\"brush\":{…BrushSettings}?=current brush}",
        alwaysE, runProc(presetsSave(s, p))),
    cmd("brush.presets.update", "Update Brush Preset", [], "",
        "{\"name\":string?=the preset the current brush was picked from,\"brush\":{…BrushSettings}?=current brush}",
        alwaysE, runProc(presetsUpdate(s, p))),
    cmd("brush.presets.delete", "Delete Brush Preset", [], "", "{\"name\":string}", alwaysE,
        runProc(presetsDelete(s, p))),
    cmd("brush.presets.setCurrent", "Set Current Brush Preset", [], "", "{\"name\":string}", alwaysE,
        runProc(presetsSetCurrent(s, p))),
    cmd("brush.defineFromSelection", "Define Brush Preset…", [], "", "{\"name\":string}", hasSelectionAndPixelsE,
        runProc(defineFromSelection(s, p))),
    cmd("brush.get", "Get Brush", [], "", "{}", alwaysE, runProc(brushJson(s.tools.brush)), journal = false),
    cmd("tools.setBrush", "Set Brush", [], "",
        "{\"preset\":name?,\"reset\":bool?,…BrushSettings fields (camelCase, deep-merged)}", alwaysE,
        runProc(setBrush(s, p))),
  ]
