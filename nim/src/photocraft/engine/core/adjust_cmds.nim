## Image › Adjustments beyond the per-pixel adjustment-layer kinds (port of `adjust_cmds.rs`):
##
## * parameter parsing for Selective Color and Color Lookup (both also adjustment layers; their
##   commands are generated with the other kinds in `commands.nim`),
## * the destructive-only adjustments: Shadows/Highlights, Replace Color, Match Color and HDR
##   Toning (maths in `algo/tone`).
##
## Destructive adjustments convert the target (layer pixels, or a targeted alpha channel / Quick
## Mask) to straight RGBA, so every colour model and bit depth is handled, and blend the result
## through the selection like the other Image › Adjustments commands. Part of the `engine/core`
## unit: included, not imported.

proc addSmartFilter*(doc: var Document; id: LayerId; sf: SmartFilter; sel: Option[Surface])

proc jIdx(v: JsonNode; i: int): JsonNode =
  ## `Value::get(i)`: the element of an array, nil otherwise.
  if v.isArray and i < v.len: v.elems[i] else: nil

proc acNum(p: JsonNode; key: string; default: float32): float32 =
  let v = p.pget(key).asF64
  if v.isSome: float32(v.get) else: default

proc given(p: JsonNode; key: string): JsonNode =
  ## `key`'s value; a null counts as missing (as in `adjust_params`).
  let v = p.pget(key)
  if v.isNull: nil else: v

proc optStr(cmdId: string; p: JsonNode; key: string): Option[string] =
  let v = given(p, key)
  if v.isNil: return
  if v.kind == JString: return some(v.str)
  raise bad(cmdId, "`" & key & "` must be a string")

proc optBool(cmdId: string; p: JsonNode; key: string): Option[bool] =
  let v = given(p, key)
  if v.isNil: return
  if v.kind == JBool: return some(v.bval)
  raise bad(cmdId, "`" & key & "` must be true or false")

proc percent(cmdId, key: string; v: JsonNode): float32 =
  ## A percentage in -100..100 (clamped); anything but a finite number is an error.
  let x = v.asF64
  if x.isNone or x.get != x.get or abs(x.get) == Inf: raise bad(cmdId, "`" & key & "` must be a number")
  clamp(float32(x.get), -100'f32, 100'f32)

proc selectiveFromParams*(p: JsonNode; base: Option[Adjustment]): Adjustment =
  ## Selective Color from params: per-range arrays (`"reds":[c,m,y,k]` in percent) and the
  ## dialog's single-range form (`"colors":"reds","cyan":…`); `"method":"relative|absolute"`.
  const cmdId = "selectiveColor"
  var relative = true
  var adj: array[9, array[4, float32]]
  if base.isSome and base.get.kind == akSelectiveColor: (relative, adj) = (base.get.relative, base.get.selective)
  let m = optStr(cmdId, p, "method")
  if m.isSome:
    case m.get
    of "relative": relative = true
    of "absolute": relative = false
    else: raise bad(cmdId, "`method` must be \"relative\" or \"absolute\", not `" & m.get & "`")
  let rb = optBool(cmdId, p, "relative")
  if rb.isSome: relative = rb.get
  for i, key in SelectiveRanges:
    let v = given(p, key)
    if v.isNil: continue
    if not v.isArray or v.len != 4: raise bad(cmdId, "`" & key & "` must be an array of 4 numbers [c, m, y, k]")
    for k in 0 .. 3: adj[i][k] = percent(cmdId, key, v.elems[k])
  let c = optStr(cmdId, p, "colors")
  if c.isSome:
    let i = SelectiveRanges.find(c.get)
    if i < 0: raise bad(cmdId, "unknown range `" & c.get & "` (ranges: " & SelectiveRanges.join(", ") & ")")
    for k, key in ["cyan", "magenta", "yellow", "black"]:
      let v = given(p, key)
      if not v.isNil: adj[i][k] = percent(cmdId, key, v)
  Adjustment(kind: akSelectiveColor, relative: relative, selective: adj)

proc builtinIds(): string =
  var ids: seq[string]
  for b in BuiltinLooks: ids.add b[0]
  ids.join(", ")

proc baseName(path: string): string =
  var i = path.len
  while i > 0 and path[i - 1] notin {'/', '\\'}: dec i
  path[i .. ^1]

proc readLutFile(path: string): Option[seq[byte]] =
  when defined(js): none(seq[byte])
  else:
    try:
      let s = readFile(path)
      some(@(s.toOpenArrayByte(0, s.len - 1)))
    except IOError, OSError: none(seq[byte])

proc debugTriple(a: array[3, float32]): string =
  "[" & debugFloat(a[0]) & ", " & debugFloat(a[1]) & ", " & debugFloat(a[2]) & "]"

proc lookupFromParams*(p: JsonNode; base: Option[Adjustment]): Adjustment =
  ## Color Lookup from params: `"lut"` (a built-in look id or `"none"`), `"file"` (a .cube / .3dl /
  ## .look path, native only) or `"data"` (the file's text, with `"fileName"` naming its format),
  ## `"interpolation":"trilinear|tetrahedral"`, `"dither":bool`. The table is embedded.
  const cmdId = "colorLookup"
  var (name, lut, size, tetrahedral, dither) = ("", SharedF32(nil), 0'u32, false, false)
  if base.isSome and base.get.kind == akColorLookup:
    let b = base.get
    (name, lut, size, tetrahedral, dither) = (b.lutName, b.lut, b.lutSize, b.tetrahedral, b.dither)
  var loaded = none((LutFile, string))
  let id = optStr(cmdId, p, "lut")
  if id.isSome:
    if id.get == "none" or id.get.len == 0:
      name = ""
      lut = nil
      size = 0
    else:
      let f = builtinLook(id.get)
      if f.isNone: raise bad(cmdId, "unknown look `" & id.get & "` (built-ins: " & builtinIds() & ")")
      loaded = some((f.get, f.get.title))
  let fileName = optStr(cmdId, p, "fileName")
  let text = optStr(cmdId, p, "data")
  if text.isSome:
    let fname = if fileName.isSome: fileName.get else: "lut.cube"
    var f: LutFile
    try: f = parseLutFile(fname, text.get.toOpenArrayByte(0, text.get.len - 1))
    except LutError as e: raise bad(cmdId, e.msg)
    loaded = some((f, baseName(fname)))
  let path = optStr(cmdId, p, "file")
  if path.isSome and path.get.len > 0:
    let bytes = readLutFile(path.get)
    if bytes.isNone: raise bad(cmdId, "can't read " & path.get)
    var f: LutFile
    try: f = parseLutFile(path.get, bytes.get)
    except LutError as e: raise bad(cmdId, path.get & ": " & e.msg)
    loaded = some((f, baseName(path.get)))
  if loaded.isSome:
    let (f, label) = loaded.get
    if not f.domainIsDefault:
      raise bad(cmdId, "`" & label & "` declares DOMAIN_MIN " & debugTriple(f.domainMin) & " / DOMAIN_MAX " &
        debugTriple(f.domainMax) & ": Color Lookup applies the plain 0..1 domain, so the look would shift; honouring a custom domain is not supported yet")
    name = label
    size = uint32(f.size)
    lut = SharedF32(data: f.data)
  let interp = optStr(cmdId, p, "interpolation")
  if interp.isSome:
    case interp.get
    of "trilinear": tetrahedral = false
    of "tetrahedral": tetrahedral = true
    else: raise bad(cmdId, "`interpolation` must be \"trilinear\" or \"tetrahedral\", not `" & interp.get & "`")
  let t = optBool(cmdId, p, "tetrahedral")
  if t.isSome: tetrahedral = t.get
  let d = optBool(cmdId, p, "dither")
  if d.isSome: dither = d.get
  Adjustment(kind: akColorLookup, lutName: name, lut: lut, lutSize: size, tetrahedral: tetrahedral, dither: dither)

proc updateAdjustment*(existing: Adjustment; p: JsonNode): Option[Adjustment] =
  ## `layer.setAdjustment` for the kinds edited one range / option at a time: params merge into
  ## the current values; a call with no parameters resets to defaults. None for other kinds.
  var empty = true
  if p.isObject:
    for k, _ in p.fields:
      if k notin ["layer", "coalesce", "__kind"]: empty = false
  let base = if empty: none(Adjustment) else: some(existing)
  case existing.kind
  of akSelectiveColor: some(selectiveFromParams(p, base))
  of akColorLookup: some(lookupFromParams(p, base))
  else: none(Adjustment)

# ---------------------------------------------------------------- destructive helpers

type RgbaFn = proc(px: var seq[array[4, float32]]; r: Rect)

proc processSurface(surf: var Surface; selection: Option[Surface]; f: RgbaFn) =
  ## Runs `f` on the surface's content as straight RGBA (`w × h` = content bounds), blending the
  ## result through the selection.
  let r = surf.contentBounds
  if r.isEmpty: return
  let fmt = surf.format
  let n = fmt.channels
  let raw = surf.readRegion(r)
  var px = newSeq[array[4, float32]](raw.len div n)
  for i in 0 ..< px.len: px[i] = toRgba(fmt, raw.toOpenArray(i * n, i * n + n - 1))
  let orig = px
  let w = r.width
  f(px, r)
  var outp = newSeqOfCap[float32](raw.len)
  var enc: array[8, float32]
  for i in 0 ..< min(px.len, orig.len):
    let k = if selection.isSome: selection.get.sampleChannel(r.x0 + i mod w, r.y0 + i div w, 0) else: 1'f32
    var mixed: array[4, float32]
    for c in 0 .. 3: mixed[c] = orig[i][c] + (px[i][c] - orig[i][c]) * k
    let m = fromRgbaInto(fmt, mixed, enc)
    for c in 0 ..< m: outp.add enc[c]
  surf.writeRegion(r, outp)

proc rgbaEdit(s: var Session; label: string; p: JsonNode; f: RgbaFn): JsonNode =
  ## One history step running `f` on the active pixel layer (or targeted channel or layer mask).
  var id = none(LayerId)
  try: id = some(layerParam(s, newJNull()))
  except EngineError: discard
  if targetOf(p).kind != tgPixels:
    s.editDo(label):
      let sel = doc.selection
      let surf = channelSurfaceForFilter(doc, id, p)
      if not surf.isNil:
        processSurface(surf[], sel, f)
        surf[].prune()
    return jnull()
  if id.isNone: raise other("no active layer")
  let lid = id.get
  s.editDo(label):
    let sel = doc.selection
    let surf = paintSurface(doc, lid, jnull())
    processSurface(surf[], sel, f)
  jnull()

proc acHasPixelsE(s: Session): string =
  if editsChannel(s): return ""
  let d = s.active
  if d.isNil: return "no document open"
  if d.activeLayer.isNone: return "no active layer"
  let l = d.doc[].findLayer(d.activeLayer.get)
  if l.isNil: return "no active layer"
  if l.content.kind == lkRaster: ""
  else: "active layer is " & l.content.article & " " & l.content.kindName & " layer, not a pixel layer"

proc hasAdjustableE*(s: Session): string =
  ## Image › Adjustments targets: a pixel layer, a smart object (recorded as a smart filter), or a
  ## targeted alpha channel / Quick Mask.
  if editsChannel(s): return ""
  let d = s.active
  if d.isNil: return "no document open"
  if d.activeLayer.isNone: return "no active layer"
  let l = d.doc[].findLayer(d.activeLayer.get)
  if l.isNil: return "no active layer"
  if l.content.kind == lkRaster: return ""
  if l.content.kind == lkSmart and l.content.smart.cache.isSome: return ""
  "active layer is " & l.content.article & " " & l.content.kindName & " layer, not a pixel layer"

proc adjustAsSmartFilter*(s: var Session; kind, label: string; p: JsonNode): bool =
  ## Image › Adjustments on a smart object record `image.adjustments.<kind>` as a smart filter:
  ## true when it was recorded (one history step, the selection becomes the filter mask); false
  ## when the command should edit pixels instead.
  if isMaskTarget(p) or isChannelTarget(p): return false
  let st = s.active
  if st.isNil or st.activeLayer.isNone: return false
  let id = st.activeLayer.get
  let l = st.doc[].findLayer(id)
  if l.isNil or l.content.kind != lkSmart: return false
  # The recorded params are the command's own, minus the engine's private `__kind` key.
  var params = if p.isNil: newJNull() else: p.copy
  params.dropKey("__kind")
  let sf = SmartFilter(command: "image.adjustments." & kind, params: params, blend: bmNormal, opacity: 1, visible: true)
  let sel = st.doc.selection
  s.editDo(label):
    addSmartFilter(doc, id, sf, sel)
  true

proc shadowsHighlightsParams(p: JsonNode): ShadowsHighlights =
  let d = defaultShadowsHighlights()
  ShadowsHighlights(
    shadowAmount: clamp(acNum(p, "shadowAmount", d.shadowAmount), 0'f32, 100'f32),
    shadowTone: clamp(acNum(p, "shadowTone", d.shadowTone), 0'f32, 100'f32),
    shadowRadius: clamp(acNum(p, "shadowRadius", d.shadowRadius), 0'f32, 2500'f32),
    highlightAmount: clamp(acNum(p, "highlightAmount", d.highlightAmount), 0'f32, 100'f32),
    highlightTone: clamp(acNum(p, "highlightTone", d.highlightTone), 0'f32, 100'f32),
    highlightRadius: clamp(acNum(p, "highlightRadius", d.highlightRadius), 0'f32, 2500'f32),
    color: clamp(acNum(p, "color", d.color), -100'f32, 100'f32),
    midtone: clamp(acNum(p, "midtone", d.midtone), -100'f32, 100'f32),
    blackClip: clamp(acNum(p, "blackClip", d.blackClip), 0'f32, 50'f32),
    whiteClip: clamp(acNum(p, "whiteClip", d.whiteClip), 0'f32, 50'f32))

proc adjustAsFilter*(kind: string; p: JsonNode; surf: Surface): Option[Surface] =
  ## Image › Adjustments run as a smart filter: the adjustment applied to a copy of `surf`. None
  ## for kinds or params it can't run.
  var outs = surf
  if kind == "shadowsHighlights":
    let sh = shadowsHighlightsParams(p)
    processSurface(outs, none(Surface), proc(px: var seq[array[4, float32]]; r: Rect) =
      shadowsHighlights(px, r.width, r.height, sh))
  else:
    let mode = surf.format.mode
    var adj: Adjustment
    try: adj = adjFromParams(kind, p, none(Adjustment), mode)
    except EngineError: return none(Surface)
    adjustSurface(outs, adj, none(Surface), mode)
  outs.prune()
  some(outs)

proc shadowsHighlightsCmd(s: var Session; p: JsonNode): JsonNode =
  if adjustAsSmartFilter(s, "shadowsHighlights", "Shadows/Highlights", p): return jnull()
  let sh = shadowsHighlightsParams(p)
  rgbaEdit(s, "Shadows/Highlights", p, proc(px: var seq[array[4, float32]]; r: Rect) =
    shadowsHighlights(px, r.width, r.height, sh))

proc replaceColorCmd(s: var Session; p: JsonNode): JsonNode =
  let c = colorParam(p, "color", s.tools.foreground)
  let fuzz = clamp(acNum(p, "fuzziness", 40), 0'f32, 200'f32)
  let h = clamp(acNum(p, "hue", 0), -180'f32, 180'f32)
  let sat = clamp(acNum(p, "saturation", 0), -100'f32, 100'f32)
  let l = clamp(acNum(p, "lightness", 0), -100'f32, 100'f32)
  rgbaEdit(s, "Replace Color", p, proc(px: var seq[array[4, float32]]; r: Rect) =
    discard replaceColor(px, [c[0], c[1], c[2]], fuzz, h, sat, l))

proc statsPixels(doc: Document; layer: Option[uint64]; useSelection: bool): (seq[array[4, float32]], Option[seq[float32]]) =
  ## Straight RGBA pixels and an optional coverage mask for a document's layer (or its composite).
  let area = doc.bounds
  var px: seq[array[4, float32]]
  if layer.isSome:
    let l = doc.findLayer(LayerId(layer.get))
    if l.isNil: raise noLayer(LayerId(layer.get))
    px = renderLayer(l[], area).px
  else: px = flatten(doc).px
  let mask = if doc.selection.isSome and useSelection: some(maskFromSurface(doc.selection, area)) else: none(seq[float32])
  (px, mask)

proc matchColorCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "image.adjustments.matchColor"
  let o = MatchColor(luminance: clamp(acNum(p, "luminance", 100), 1'f32, 200'f32),
                     intensity: clamp(acNum(p, "intensity", 100), 1'f32, 200'f32),
                     fade: clamp(acNum(p, "fade", 0), 0'f32, 100'f32),
                     neutralize: p.getBoolOr("neutralize", false))
  var source = none(LabStats)
  let si = p.pget("source").asI64
  if si.isSome and si.get >= 0:
    let i = si.get
    if i >= int64(s.docs.len): raise bad(cmdId, "no document " & $i)
    let layer = p.pget("sourceLayer").asU64
    let (px, mask) = statsPixels(s.docs[int(i)].doc[], layer, p.getBoolOr("useSelectionInSource", false))
    let st = labStats(px, mask)
    if st.isNone: raise bad(cmdId, "the source has no opaque pixels")
    source = st
  # Source "None" with default options matches the image to its own statistics: a no-op.
  let targetSel = p.getBoolOr("useSelectionInTarget", false)
  let st = s.active
  let sel = if st.isNil: none(Surface) else: st.doc.selection
  rgbaEdit(s, "Match Color", p, proc(px: var seq[array[4, float32]]; r: Rect) =
    let mask = if sel.isSome and targetSel: some(maskFromSurface(sel, r)) else: none(seq[float32])
    let target = labStats(px, mask)
    if target.isNone: return
    matchColor(px, target.get, source, o))

proc hdrToningCmd(s: var Session; p: JsonNode): JsonNode =
  let d = defaultHdrToning()
  var curve: seq[(float32, float32)]
  let cv = p.pget("curve")
  if cv.isArray:
    for v in cv.elems:
      let a = jIdx(v, 0).asF64
      let b = jIdx(v, 1).asF64
      if a.isSome and b.isSome: curve.add (float32(a.get) / 255, float32(b.get) / 255)
  let h = HdrToning(
    radius: clamp(acNum(p, "radius", d.radius), 1'f32, 500'f32),
    strength: clamp(acNum(p, "strength", d.strength), 0.1'f32, 4'f32),
    gamma: clamp(acNum(p, "gamma", d.gamma), 0.1'f32, 2'f32),
    exposure: clamp(acNum(p, "exposure", d.exposure), -5'f32, 5'f32),
    detail: clamp(acNum(p, "detail", d.detail), -100'f32, 300'f32),
    shadow: clamp(acNum(p, "shadow", d.shadow), -100'f32, 100'f32),
    highlight: clamp(acNum(p, "highlight", d.highlight), -100'f32, 100'f32),
    vibrance: clamp(acNum(p, "vibrance", d.vibrance), -100'f32, 100'f32),
    saturation: clamp(acNum(p, "saturation", d.saturation), -100'f32, 100'f32),
    curve: curve)
  # Like Photoshop, HDR Toning flattens the image first (one history step).
  s.editDo("HDR Toning"):
    let buf = flatten(doc).overBackground([1'f32, 1, 1])
    let (w, hgt) = (buf.rect.width, buf.rect.height)
    var px = buf.px
    hdrToning(px, w, hgt, h)
    let fmt = doc.pixelFormat
    var data: seq[float32]
    for q in px: data.add fromRgba(fmt, q)
    var bg = rasterLayer("Background", fmt)
    bg.locks.transparency = true
    let surf = pixelsMut(bg)
    if surf.isNil: raise other("not a pixel layer")
    surf[].writeRegion(doc.bounds, data)
    active = some(bg.id)
    doc.layers = @[bg]
    doc.selection = none(Surface)
  jnull()

proc listLooks(s: var Session; p: JsonNode): JsonNode =
  ## The Color Lookup built-ins (id, label) for UIs and agents.
  result = newJArray()
  for (id, label) in BuiltinLooks: result.add %*{"id": id, "label": label}

proc adjustSpecs*(): seq[CommandSpec] =
  @[
    cmd("image.adjustments.shadowsHighlights", "Shadows/Highlights…", ["Image", "Adjustments"], "",
        """{"shadowAmount":0..100=35,"shadowTone":0..100=50,"shadowRadius":0..2500=30,"highlightAmount":0..100=0,"highlightTone":0..100=50,"highlightRadius":0..2500=30,"color":-100..100=20,"midtone":-100..100=0,"blackClip":0..50=0.01,"whiteClip":0..50=0.01}""",
        hasAdjustableE, shadowsHighlightsCmd),
    cmd("image.adjustments.replaceColor", "Replace Color…", ["Image", "Adjustments"], "",
        """{"color":json,"fuzziness":0..200=40,"hue":-180..180=0,"saturation":-100..100=0,"lightness":-100..100=0} (color: "#rrggbb", default the foreground colour)""",
        acHasPixelsE, replaceColorCmd),
    cmd("image.adjustments.matchColor", "Match Color…", ["Image", "Adjustments"], "",
        """{"source":doc,"sourceLayer":json,"luminance":1..200=100,"intensity":1..200=100,"fade":0..100=0,"neutralize":bool=false,"useSelectionInSource":bool=false,"useSelectionInTarget":bool=false} (source: document index; sourceLayer: layer id, default the merged image)""",
        acHasPixelsE, matchColorCmd),
    cmd("image.adjustments.hdrToning", "HDR Toning…", ["Image", "Adjustments"], "",
        """{"radius":1..500=30,"strength":0.1..4=0.5,"gamma":0.1..2=1,"exposure":-5..5=0,"detail":-100..300=30,"shadow":-100..100=0,"highlight":-100..100=0,"vibrance":-100..100=0,"saturation":-100..100=20,"curve":[[in,out],…] 0..255} (flattens the image)""",
        hasDocE, hdrToningCmd),
    cmd("image.adjustments.colorLookup.list", "List Color Lookup Looks", [], "", "{}", always, listLooks, journal = false),
  ]
