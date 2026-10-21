## The command table's shared helpers and its first part (port of `commands.rs`; the rest of
## `build()` concatenates every module's `specs()` in `engine.nim`). Part of the `engine/core`
## unit: included, not imported.

# ---------------------------------------------------------------- spec construction

proc cmd*(id, label: string; menu: openArray[string]; shortcut: string; params: string;
          enabled: EnabledProc; run: RunProc; journal = true): CommandSpec =
  ## `cmd!`: a command spec (`shortcut` "" = none).
  CommandSpec(id: id, label: label, menu: @menu, shortcut: (if shortcut.len > 0: some(shortcut) else: none(string)),
              params: params, enabled: enabled, run: run, journal: journal)

template runProc*(body: untyped): RunProc =
  ## A command's `run`: `s` (var Session) and `p` (params) are in scope; returns a JsonNode.
  (proc(s {.inject.}: var Session; p {.inject.}: JsonNode): JsonNode {.closure.} = body)

template enabledProc*(body: untyped): EnabledProc =
  ## An `enabled` predicate: `s` in scope; "" when the command can run.
  (proc(s {.inject.}: Session): string {.closure.} = body)

template editDo*(s: var Session; label: string; body: untyped) =
  ## `s.edit(label, |doc, active| { body })`: `doc` and `active` are in scope.
  s.edit(label, proc(doc {.inject.}: var Document; active {.inject.}: var Option[LayerId]) {.closure.} = body)

template editGet*(s: var Session; label: string; R: typedesc; body: untyped): untyped =
  ## `edit` whose body returns an `R`.
  s.editWith(label, proc(doc {.inject.}: var Document; active {.inject.}: var Option[LayerId]): R {.closure.} = body)

proc jnull*(): JsonNode {.inline.} = newJNull()

proc idsJson*(ids: openArray[LayerId]): JsonNode =
  result = newJArray()
  for id in ids: result.add %id

# ---------------------------------------------------------------- layers in place

proc layerMut*(d: var Document; id: LayerId): ptr Layer =
  ## `doc.layer_mut(id)`: nil when absent.
  d.layerPtr(id)

proc layerOrRaise*(d: var Document; id: LayerId): ptr Layer =
  result = d.layerPtr(id)
  if result.isNil: raise noLayer(id)

proc surfaceMut*(l: var Layer): ptr Surface =
  ## `Layer::surface_mut`: the pixels of a pixel layer or the rendered cache of a type, shape or
  ## smart object layer; nil when it has none.
  case l.content.kind
  of lkRaster: addr l.content.raster
  of lkText: (if l.content.text.cache.isSome: addr l.content.text.cache.get else: nil)
  of lkShape: (if l.content.shape.cache.isSome: addr l.content.shape.cache.get else: nil)
  of lkSmart: (if l.content.smart.cache.isSome: addr l.content.smart.cache.get else: nil)
  else: nil

proc surfaceOf*(l: Layer): Option[Surface] =
  ## `Layer::surface`: the layer's pixels, when it has any.
  if l.hasSurface: some(l.surface) else: none(Surface)

proc pixelsMut*(l: var Layer): ptr Surface =
  ## `crate::pixels_mut`: the layer's pixels or an error.
  result = l.surfaceMut
  if result.isNil: raise other("layer LayerId(" & $uint64(l.id) & ") has no pixels")

proc childrenOf*(l: Layer): Option[seq[Layer]] =
  if l.content.kind == lkGroup: some(l.content.group.children) else: none(seq[Layer])

proc childrenMut*(l: var Layer): ptr seq[Layer] =
  if l.content.kind == lkGroup: addr l.content.group.children else: nil

proc siblingsAt*(d: var Document; path: openArray[int]): (ptr seq[Layer], int) =
  ## The sibling list holding the layer at `path`, and its index there (nil when invalid).
  if path.len == 0: return (nil, -1)
  if path.len == 1: return (addr d.layers, path[0])
  let p = d.layerAtPtr(path[0 ..< ^1])
  if p.isNil or p[].content.kind != lkGroup: return (nil, -1)
  (addr p[].content.group.children, path[^1])

# ---------------------------------------------------------------- enabled predicates

proc always*(s: Session): string = ""

proc hasDocE*(s: Session): string =
  if s.active.isNil: "no document open" else: ""

proc hasLayerE*(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if d.activeLayer.isSome and not d.doc[].findLayer(d.activeLayer.get).isNil: "" else: "no active layer"

proc hasPixelLayerE*(s: Session): string =
  let (l, why) = activeLayerOf(s)
  if l.isNil: return why
  if l.content.kind == lkRaster: ""
  else: "active layer is " & l.content.article & " " & l.content.kindName & " layer, not a pixel layer"

proc hasPixelOrChannelE*(s: Session): string =
  ## A pixel layer, or a targeted alpha channel / Quick Mask.
  if s.editsChannel: return ""
  hasPixelLayerE(s)

proc hasPaintableE*(s: Session): string =
  ## A layer that can be painted: pixels, or any layer with a mask.
  if s.editsChannel: return ""
  let (l, why) = activeLayerOf(s)
  if l.isNil: return why
  if l.content.kind == lkRaster or l.mask.isSome: ""
  else: "active layer is " & l.content.article & " " & l.content.kindName & " layer without a mask"

proc canUndoE(s: Session): string =
  let d = s.active
  if not d.isNil and d.history.canUndo: "" else: "nothing to undo"
proc canRedoE(s: Session): string =
  let d = s.active
  if not d.isNil and d.history.canRedo: "" else: "nothing to redo"
proc hasSelectionE*(s: Session): string =
  let d = s.active
  if not d.isNil and d.doc.selection.isSome: "" else: "no selection"

# ---------------------------------------------------------------- locks and targets

proc isMaskTarget*(p: JsonNode): bool =
  let t = p.pget("target")
  not t.isNil and t.kind == JString and t.str == "mask"

proc checkAllUnlocked(doc: Document; id: LayerId) =
  let locks = doc.effectiveLocks(id)
  let l = doc.findLayer(id)
  if l.isNil: raise noLayer(id)
  if locks.all:
    raise other("Could not complete your request because the layer \"" & l.name & "\" is locked")

proc checkPixelsUnlocked*(doc: Document; id: LayerId) =
  ## Refuses a layer whose pixels are locked (directly or through a group).
  let locks = doc.effectiveLocks(id)
  let l = doc.findLayer(id)
  if l.isNil: raise noLayer(id)
  if locks.pixels or locks.all:
    raise other("Could not complete your request because the layer \"" & l.name & "\" is locked")

proc paintSurface*(doc: var Document; id: LayerId; p: JsonNode): ptr Surface =
  ## The surface a paint command writes to: the layer's pixels, or its mask with `"target":"mask"`.
  if isMaskTarget(p): checkAllUnlocked(doc, id) else: checkPixelsUnlocked(doc, id)
  let l = doc.layerOrRaise(id)
  if isMaskTarget(p):
    if l.mask.isNone: raise other("layer has no mask")
    return addr l.mask.get.surface
  else:
    result = l[].surfaceMut
    if result.isNil: raise other("not a pixel layer")

# ---------------------------------------------------------------- params

proc f32Or*(p: JsonNode; key: string; default: float32): float32 =
  let v = p.pget(key).asF64
  if v.isSome: float32(v.get) else: default

proc layerParam*(s: Session; p: JsonNode): LayerId =
  ## `"layer"`, else the active layer.
  let v = p.pget("layer").asU64
  if v.isSome: return LayerId(v.get)
  let d = s.active
  if d.isNil or d.activeLayer.isNone: raise other("no active layer")
  d.activeLayer.get

proc blendFromStr*(s: string): Option[BlendMode] =
  proc norm(x: string): string =
    for c in x.toLowerAscii:
      if c notin {' ', '_', '-', '(', ')'}: result.add c
  let want = norm(s)
  for m in @[bmPassThrough] & layerModes():
    if norm(m.label) == want or norm($m) == want: return some(m)
  none(BlendMode)

proc maskAtDepth*(mask: LayerMask; depth: SampleType): LayerMask =
  ## New pixel masks use the document's depth, as PSD saves and imports do.
  result = mask
  let format = mask.surface.format.withSample(depth)
  if mask.surface.format != format:
    try: result.surface = mask.surface.tryConvert(format)
    except AllocationError as e: raise allocationFailed(e)

proc selectionMask*(doc: Document): Option[LayerMask] =
  ## The active selection as the mask of a layer being created (none without a selection).
  if doc.selection.isNone: return
  var m = revealAllMask()
  m.surface = doc.selection.get
  some(maskAtDepth(m, doc.depth))

proc maskNewLayer*(doc: Document; l: var Layer; p: JsonNode; cmdId: string) =
  ## Masks a fill or adjustment layer being created: the selected path becomes its vector mask;
  ## without one the selection becomes its layer mask; without either a white mask.
  let v = p.pget("path")
  var path: Option[Path]
  if v.isNull: discard
  elif v.kind == JString and isWork(some(v.str)):
    if doc.workPath.isNone: raise bad(cmdId, "no work path")
    path = doc.workPath
  elif v.kind == JString:
    for sp in doc.paths:
      if sp.name == v.str:
        path = some(sp.path)
        break
    if path.isNone: raise bad(cmdId, "no path named \"" & v.str & "\"")
  else:
    try: path = some(parsePath(v))
    except EngineError as e: raise bad(cmdId, e.detail)
  if path.isSome: l.vectorMask = some(newVectorMask(path.get))
  else:
    let m = selectionMask(doc)
    l.mask = some(maskAtDepth(if m.isSome: m.get else: revealAllMask(), doc.depth))

const NewLayerPath* = "\"path\":\"work\"|saved path name|{…path}? (the active path: becomes the vector mask; else the selection is the layer mask, or white with no selection)"

proc newAdjustment(s: var Session; adj: Adjustment; p: JsonNode; cmdId: string): JsonNode =
  let label = "New " & adj.label & " Layer"
  let name = adj.label
  let id = s.editGet(label, LayerId):
    var l = newLayer(doc.nextLayerName(name), adjustmentContent(adj))
    maskNewLayer(doc, l, p, cmdId)
    let id = doc.insertAbove(active, l)
    active = some(id)
    id
  %*{"layer": id}

proc destructiveAdjust(s: var Session; label: string; adj: Adjustment; p: JsonNode): JsonNode =
  if isMaskTarget(p):
    # The targeted layer mask: ⌘I inverts it, as in Photoshop.
    let id = layerParam(s, jnull())
    s.editDo(label):
      let sel = doc.selection
      let l = doc.layerOrRaise(id)
      if l.mask.isNone: raise other("layer has no mask")
      adjustMask(l.mask.get.surface, adj, sel)
    return jnull()
  if isChannelTarget(p):
    # Alpha channel / Quick Mask target: the adjustment runs on the grayscale channel.
    s.editDo(label):
      let sel = doc.selection
      let surf = channelSurfaceForFilter(doc, none(LayerId), p)
      if not surf.isNil:
        adjustSurface(surf[], adj, sel, cmGrayscale)
        surf[].prune()
    return jnull()
  let id = layerParam(s, jnull())
  s.editDo(label):
    let sel = doc.selection
    let mode = doc.mode
    let surf = paintSurface(doc, id, jnull())
    adjustSurface(surf[], adj, sel, mode)
  jnull()

proc adjustmentFromParams*(kind: string; p: JsonNode): Adjustment =
  ## Lenient adjustment from params for previews: bad params fall back to the kind's defaults.
  try: return adjFromParams(kind, p, none(Adjustment), cmRgb)
  except EngineError: discard
  try: return adjDefaultFor(kind, cmRgb)
  except EngineError: discard
  Adjustment(kind: akInvert)

proc docMode(s: Session): ColorMode =
  let d = s.active
  if d.isNil: cmRgb else: d.doc.mode

proc adjustmentKind*(a: Adjustment): string =
  case a.kind
  of akBrightnessContrast: "brightnessContrast"
  of akLevels: "levels"
  of akCurves: "curves"
  of akExposure: "exposure"
  of akVibrance: "vibrance"
  of akHueSaturation: "hueSaturation"
  of akColorBalance: "colorBalance"
  of akBlackWhite: "blackWhite"
  of akPhotoFilter: "photoFilter"
  of akChannelMixer: "channelMixer"
  of akPosterize: "posterize"
  of akThreshold: "threshold"
  of akGradientMap: "gradientMap"
  of akSelectiveColor: "selectiveColor"
  of akColorLookup: "colorLookup"
  else: "invert"

proc injectKind(id: string; params: JsonNode): JsonNode =
  ## Commands generated from a table share one `run`; the kind is recovered from the id.
  var k = ""
  if id.startsWith("layer.newAdjustmentLayer."): k = id["layer.newAdjustmentLayer.".len .. ^1]
  elif id.startsWith("image.adjustments."): k = id["image.adjustments.".len .. ^1]
  else: return params
  if params.isObject: params.withField("__kind", %k)
  else: %*{"__kind": k}

# ---------------------------------------------------------------- layer helpers

proc translateLayer*(doc: Document; l: var Layer; dx, dy: int) =
  ## Moves a layer's pixels, linked mask and type by whole pixels (vectors move via
  ## `translateVectors`).
  if l.effects.reference.isSome:
    let r = l.effects.reference.get
    l.effects.reference = some((r[0] + float64(dx), r[1] + float64(dy)))
  if l.mask.isSome and l.mask.get.linked:
    l.mask.get.surface = shiftSurface(l.mask.get.surface, dx, dy)
  case l.content.kind
  of lkRaster: l.content.raster = shiftSurface(l.content.raster, dx, dy)
  of lkText:
    template t: untyped = l.content.text
    t.transform = translateAffine(float64(dx), float64(dy)).mul(t.transform)
    # A whole-pixel move shifts the rendered text and its PSD position as they are.
    let hasRaw = not t.psdRaw.isNil
    let raw = if hasRaw: translateTysh(t.psdRaw.data, float64(dx), float64(dy)) else: none(seq[byte])
    if t.cache.isSome and hasRaw and raw.isSome:
      t.cache = some(shiftSurface(t.cache.get, dx, dy))
      t.psdRaw = shared(raw.get)
    elif t.cache.isSome and not hasRaw:
      t.cache = some(shiftSurface(t.cache.get, dx, dy))
    else:
      refreshText(doc, t)
  of lkGroup:
    if l.content.group.artboard.isSome:
      l.content.group.artboard.get.rect = l.content.group.artboard.get.rect.translate(dx, dy)
    for c in l.content.group.children.mitems: translateLayer(doc, c, dx, dy)
  of lkSmart: shiftSmart(l.content.smart, dx, dy)
  else: discard

proc layerCopy*(doc: Document; id: LayerId): Layer =
  ## A copy of layer `id` as Duplicate Layer makes it: fresh ids, "… copy", and a copy of the
  ## Background is an ordinary, unlocked layer.
  let src = doc.findLayer(id)
  if src.isNil: raise noLayer(id)
  let fromBackground = src.name == "Background" and src.locks.transparency and doc.layers.len > 0 and
    doc.layers[0].id == id
  result = src[].duplicate
  result.name = doc.copyName(result.name)
  if fromBackground: result.locks = Locks()

# ---------------------------------------------------------------- commands

proc turnCanvasCmd(s: var Session; label: string; t: Turn): JsonNode =
  ## Image › Image Rotation by a right angle or a flip (pixels, vectors, guides… all move).
  let d = s.active
  if not d.isNil:
    s.setSamplerMap(t.affine(float64(d.doc.size.width), float64(d.doc.size.height)))
  s.editDo(label):
    turnCanvas(doc, t)
  jnull()

proc setClipped(s: var Session; p: JsonNode; clip: bool): JsonNode =
  ## Layer › Create / Release Clipping Mask: the `layer`, or the selection (Create clips every
  ## selected layer but the lowest, Release releases all).
  let label = if clip: "Create Clipping Mask" else: "Release Clipping Mask"
  var ids: seq[LayerId]
  if not p.pget("layer").isNil:
    ids = @[layerParam(s, p)]
  else:
    let st = s.activeOrRaise()
    let selected = st[].selectedOrdered
    if selected.len == 0: ids = @[layerParam(s, p)]
    elif selected.len == 1: ids = selected
    elif clip: ids = selected[1 .. ^1]
    else: ids = selected
  s.editDo(label):
    for id in ids: doc.layerOrRaise(id).clipped = clip
  %*{"layers": idsJson(ids)}

proc arrange(s: var Session; p: JsonNode; delta: int): JsonNode =
  let id = layerParam(s, p)
  s.editDo("Arrange"):
    # The Background stays at the bottom: it doesn't move and nothing goes below it.
    let bg = backgroundId(doc)
    if bg == some(id): raise other("the Background layer can't be moved")
    let steps = if delta == high(int32): 10_000 elif delta == low(int32): -10_000 else: delta
    let dir = (if steps > 0: 1 elif steps < 0: -1 else: 0)
    let before = readBefore(doc, [id])
    var moved = false
    for _ in 0 ..< abs(steps):
      if not doc.shift(id, dir): break
      var ok = true
      try: keepBackgroundAtBottom(bg, doc)
      except EngineError: ok = false
      if not ok:
        discard doc.shift(id, -dir)
        break
      moved = true
    if not moved: raise other("layer can't move further")
    settle(doc, [id], before, false)
  jnull()

proc setMask(s: var Session; p: JsonNode; label: string; mask: Option[LayerMask]): JsonNode =
  ## Adds (some) or removes (none) a layer mask; adding acts on every selected layer that can
  ## take one, in one history step.
  let ids = if mask.isSome: maskTargets(s, p) else: @[layerParam(s, p)]
  s.editDo(label):
    let m = if mask.isSome: some(maskAtDepth(mask.get, doc.depth)) else: none(LayerMask)
    for id in ids:
      if m.isSome: backgroundToLayerForMask(doc, id)
      doc.layerOrRaise(id).mask = m
  jnull()

const
  MaxDimension* = 300_000
  MaxResolution* = 30_000.0

proc adjTable(): seq[(string, string, string)] =
  @[
    ("brightnessContrast", "Brightness/Contrast…", """{"brightness":-150..150=0,"contrast":-50..100=0,"legacy":bool=false}"""),
    ("levels", "Levels…", """{"inBlack":0..253=0,"gamma":0.01..9.99=1,"inWhite":2..255=255,"outBlack":0..255=0,"outWhite":0..255=255,"red":json,"green":json,"blue":json,"eyedropper":json} (top level = composite; per channel {"inBlack","gamma","inWhite","outBlack","outWhite"} under red/green/blue, gray, cyan/magenta/yellow/black or lightness/a/b; eyedropper = the dialog's Set Black/Neutral Gray/White Point picker {"point":"black|gray|white","at":[x,y],"size":1..101=1} or {"point",…,"color":[r,g,b] 0..1}: black/white set each channel's input black/white to the sample, gray each channel's gamma so the sample turns neutral at its luma; RGB and Grayscale (no gray) only, applied over the given levels)"""),
    ("curves", "Curves…", """{"points":json,"red":json,"green":json,"blue":json,"pencil":bool=false,"smooth":0..10=0,"eyedropper":json} (curves as [[in,out],…] in 0..255, 2..19 points: points = composite; red/green/blue, gray, cyan/magenta/yellow/black or lightness/a/b per channel; pencil = drawn curves (Draw to modify the curve): each curve is 256 output levels in 0..255 (point curves given are drawn in, and turning pencil off fits points to the drawing); smooth = Smooth passes over the drawn curves; eyedropper = the dialog's Set Black/Neutral Gray/White Point picker {"point":"black|gray|white","at":[x,y],"size":1..101=1} or {"point",…,"color":[r,g,b] 0..1}, RGB and Grayscale (no gray) only, applied over the given curves)"""),
    ("exposure", "Exposure…", """{"exposure":-20..20=0,"offset":-0.5..0.5=0,"gamma":0.01..9.99=1}"""),
    ("vibrance", "Vibrance…", """{"vibrance":-100..100=0,"saturation":-100..100=0}"""),
    ("hueSaturation", "Hue/Saturation…", """{"hue":-180..180=0,"saturation":-100..100=0,"lightness":-100..100=0,"colorize":bool=false,"reds":json,"yellows":json,"greens":json,"cyans":json,"blues":json,"magentas":json} (per range {"hue","saturation","lightness","range":[4 hue degrees]}; colorize: hue 0..360, saturation 0..100)"""),
    ("colorBalance", "Color Balance…", """{"shadows":json,"midtones":json,"highlights":json,"preserveLuminosity":bool=true} (each tone [cyan-red, magenta-green, yellow-blue] in -100..100)"""),
    ("blackWhite", "Black & White…", """{"reds":-200..300=40,"yellows":-200..300=60,"greens":-200..300=40,"cyans":-200..300=60,"blues":-200..300=20,"magentas":-200..300=80,"tint":bool=false,"tintColor":"#rrggbb"}"""),
    ("photoFilter", "Photo Filter…", """{"filter":"warming85|warmingLBA|warming81|cooling80|coolingLBB|cooling82|red|orange|yellow|green|cyan|blue|violet|magenta|sepia|deepRed|deepBlue|deepEmerald|deepYellow|underwater","color":"#rrggbb","density":0..100=25,"preserveLuminosity":bool=true}"""),
    ("channelMixer", "Channel Mixer…", """{"red":json,"green":json,"blue":json,"gray":json,"monochrome":bool=false} (per output channel [red %, green %, blue %, constant %] in -200..200; gray = the monochrome mix)"""),
    ("invert", "Invert", "{}"),
    ("posterize", "Posterize…", """{"levels":2..255=4}"""),
    ("threshold", "Threshold…", """{"level":1..255=128}"""),
    ("gradientMap", "Gradient Map…", """{"stops":json,"reverse":bool=false,"dither":bool=false} (stops [[location 0..1, "#rrggbb"], …], 2..64)"""),
    ("selectiveColor", "Selective Color…", """{"method":"relative|absolute"="relative","colors":"reds|yellows|greens|cyans|blues|magentas|whites|neutrals|blacks"="reds","cyan":-100..100=0,"magenta":-100..100=0,"yellow":-100..100=0,"black":-100..100=0,"reds":json} (per-range [c,m,y,k] arrays: reds yellows greens cyans blues magentas whites neutrals blacks)"""),
    ("colorLookup", "Color Lookup…", """{"lut":"none|warm|cool|tealOrange|bleachBypass|fadedFilm|dayForNight|monoContrast|crossProcess"="none","file":text,"interpolation":"trilinear|tetrahedral"="trilinear","dither":bool=false,"data":json} (file: .cube/.3dl/.look path; data: file text + "fileName")"""),
  ]

proc fileNew(s: var Session; p: JsonNode): JsonNode =
  validateNewParams(s, p)
  # A size given as a float (`512.0`, as JSON from a UI field) is still that size.
  proc px(k: string; d: int): int =
    let v = p.pget(k)
    if v.isNil: return d
    let u = v.asU64
    if u.isSome: return int(clamp(u.get, 1'u64, uint64(MaxDimension)))
    let f = v.asF64
    if f.isSome and f.get == f.get and abs(f.get) != Inf: return int(clamp(round(f.get), 1.0, float64(MaxDimension)))
    d
  let w = px("width", 1920)
  let h = px("height", 1080)
  let mode = colorModeParam(p.getStrOr("mode", "rgb")).get(cmRgb)
  let depthV = p.pget("depth").asU64
  let depth = sampleTypeParam(if depthV.isSome: depthV.get else: 8).get(stU8)
  let name = p.getStrOr("name", "Untitled")
  let res = float32(clamp(p.getF64Or("resolution", 72.0), 1.0, MaxResolution))
  var doc: Document
  case p.getStrOr("background", "white")
  of "backgroundColor":
    let bgc = backgroundColorParam(p, s.tools.background, "file.new")
    doc = withBackground(name, size(w, h), mode, depth, rgba(bgc[0], bgc[1], bgc[2], 1))
  of "transparent":
    doc = newDocument(name, size(w, h), mode, depth)
    doc.layers.add rasterLayer("Layer 1", doc.pixelFormat)
  of "black":
    doc = withBackground(name, size(w, h), mode, depth, Black)
  else:
    let c = parseHex(p.getStrOr("background", "white")).get([1'f32, 1, 1, 1])
    doc = withBackground(name, size(w, h), mode, depth, rgba(c[0], c[1], c[2], c[3]))
  doc.resolutionDpi = res
  let i = s.addDocument(doc)
  %*{"document": i}

proc selectRect(s: var Session; p: JsonNode): JsonNode =
  proc get(k: string): int =
    let v = intI32("select.rect", p, k)
    if v.isNone: raise bad("select.rect", "missing `" & k & "`")
    int(v.get)
  let x = get("x")
  let y = get("y")
  let ww = get("width")
  let hh = get("height")
  let r = rectXywh(x, y, max(ww, 0), max(hh, 0))
  let mode = p.getStrOr("mode", "replace")
  let ellipse = p.getBoolOr("ellipse", false)
  let line = p.pget("line").asStr
  var step: string
  if line.isSome:
    case line.get
    of "row": step = "Single Row Marquee"
    of "column": step = "Single Column Marquee"
    else: raise bad("select.rect", "unknown `line` " & escapeJson(line.get) & " (row|column)")
  elif ellipse: step = "Elliptical Marquee"
  else: step = "Rectangular Marquee"
  let aa = p.getBoolOr("antiAlias", true)
  let feather = float32(clamp(p.getF64Or("feather", 0.0), 0.0, 1000.0))
  s.editDo(step):
    let area = doc.bounds
    var shape: Surface
    if ellipse: shape = ellipseSurface(r, area, aa)
    else:
      shape = newSurface(Gray8)
      shape.fillRect(r.intersect(area), [1'f32])
    if feather > 0:
      let m = selection.feather(maskFromSurface(some(shape), area), area.width, area.height, feather)
      doc.selection = combine(doc.selection, m, area, parseSelectionMode(mode))
      return
    let old = doc.selection
    doc.selection = none(Surface)
    var combined: Surface
    if mode == "add" and old.isSome: combined = combineSurfaces(old.get, shape, area, proc(a, b: float32): float32 = max(a, b))
    elif mode == "subtract" and old.isSome: combined = combineSurfaces(old.get, shape, area, proc(a, b: float32): float32 = a * (1 - b))
    elif mode == "intersect" and old.isSome: combined = combineSurfaces(old.get, shape, area, proc(a, b: float32): float32 = min(a, b))
    elif mode in ["subtract", "intersect"]: combined = newSurface(Gray8)
    else: combined = shape
    # Nothing selected on the canvas deselects.
    doc.selection = if not combined.contentBounds.isEmpty: some(combined) else: none(Surface)
  jnull()

proc setProps(s: var Session; p: JsonNode): JsonNode =
  let id = layerParam(s, p)
  var onlyVisibility = false
  if p.isObject and p.fields.hasKey("visible"):
    onlyVisibility = true
    for k in p.fields.keys:
      if k notin ["layer", "visible", "coalesce"]: onlyVisibility = false
  let label = if onlyVisibility: "Layer Visibility" else: "Layer Properties"
  let before = s.activeOrRaise().doc[]
  # Clipping and channel changes reach the layers around it: recomposite everything.
  let local = p.pget("clipped").isNil and p.pget("channels").isNil
  s.editDo(label):
    let l = doc.layerOrRaise(id)
    let name = p.pget("name").asStr
    if name.isSome: l.name = name.get
    let vis = p.pget("visible").asBool
    if vis.isSome: l.visible = vis.get
    let op = p.pget("opacity").asF64
    if op.isSome: l.opacity = clamp(float32(op.get), 0'f32, 1'f32)
    let fill = p.pget("fill").asF64
    if fill.isSome: l.fillOpacity = clamp(float32(fill.get), 0'f32, 1'f32)
    let blend = p.pget("blend").asStr
    if blend.isSome:
      let m = blendFromStr(blend.get)
      if m.isNone: raise bad("layer.setProps", "unknown blend mode `" & blend.get & "`")
      l.blend = m.get
    let clipped = p.pget("clipped").asBool
    if clipped.isSome: l.clipped = clipped.get
    let locked = p.pget("locked").asBool
    if locked.isSome: l.locks.all = locked.get
    let ch = p.pget("channels")
    if ch.isArray:
      # Blending Options › Advanced Blending › Channels: unchecked = left out.
      var m = 0'u32
      for i, v in ch.elems:
        if i >= 32: break
        if v.asBool == some(false): m = m or (1'u32 shl i)
      l.excludedChannels = m
    let locks = p.pget("locks")
    if locks.isObject:
      for k, v in locks.sortedPairs:
        let b = v.asBool.get(false)
        case k
        of "transparency": l.locks.transparency = b
        of "pixels": l.locks.pixels = b
        of "position": l.locks.position = b
        of "artboard": l.locks.artboard = b
        of "all": l.locks.all = b
        else: raise bad("layer.setProps", "unknown lock `" & k & "`")
  if local: noteDamage(s, before, [id])
  jnull()

proc mergeDownCmd(s: var Session; p: JsonNode): JsonNode =
  let id = layerParam(s, p)
  s.editDo("Merge Down"):
    let path = doc.pathOf(id)
    if path.isNone or path.get.len == 0: raise noLayer(id)
    let idx = path.get[^1]
    let parent = path.get[0 ..< ^1]
    if idx == 0: raise other("no layer below to merge into")
    let below = parent & (idx - 1)
    let lowerO = doc.layerAt(below)
    if lowerO.isNone: raise noLayer(id)
    let lower = lowerO.get
    let upperP = doc.findLayer(id)
    if upperP.isNil: raise noLayer(id)
    let upper = upperP[]
    let merged = mergeDown(doc.bounds, lower, upper, doc.pixelFormat)
    discard doc.remove(id)
    let slot = doc.layerAtPtr(below)
    if slot.isNil: raise noLayer(lower.id)
    slot[] = merged
    active = some(lower.id)
  jnull()

proc flattenImageCmd(s: var Session; p: JsonNode): JsonNode =
  s.editDo("Flatten Image"):
    let fmt = doc.pixelFormat
    var bg = rasterLayer("Background", fmt)
    bg.locks.transparency = true
    bg.locks.position = true
    bg.pixelsMut[] = flattenToSurface(doc, fmt, some([1'f32, 1, 1]))
    active = some(bg.id)
    doc.layers = @[bg]
  jnull()

proc duplicateCmd(s: var Session; p: JsonNode): JsonNode =
  let inPlace = p.getBoolOr("inPlace", false)
  let nameV = p.pget("name").asStr
  let name = if nameV.isSome and nameV.get.len > 0: nameV else: none(string)
  if multi(s, p): return duplicateSelected(s, inPlace, "Duplicate Layers")
  let id = layerParam(s, p)
  let before = s.activeOrRaise().doc[]
  let nid = s.editGet("Duplicate Layer", LayerId):
    var dup = layerCopy(doc, id)
    if name.isSome: dup.name = name.get
    let nid = doc.insertAbove(some(id), dup)
    if not inPlace: placeCopy(doc, nid)
    active = some(nid)
    nid
  noteInsert(s, before)
  %*{"layer": nid}

proc deleteCmd(s: var Session; p: JsonNode): JsonNode =
  if multi(s, p): return deleteSelected(s)
  let id = layerParam(s, p)
  s.editDo("Delete Layer"):
    let neighbours = deletionNeighbours(doc, id)
    let before = readBefore(doc, [id])
    if doc.remove(id).isNone: raise noLayer(id)
    settle(doc, [], before, false)
    if doc.layers.len == 0: raise other("a document must keep at least one layer")
    if active.isSome and doc.findLayer(active.get).isNil:
      active = none(LayerId)
      for c in neighbours:
        if not doc.findLayer(c).isNil:
          active = some(c)
          break
  jnull()

proc setAdjustmentCmd(s: var Session; p: JsonNode): JsonNode =
  let id = layerParam(s, p)
  let eyedropped = layerEyedropper(s, id, p)
  s.editDo("Modify Adjustment"):
    let mode = doc.mode
    let l = doc.layerOrRaise(id)
    if l.content.kind != lkAdjustment: raise other("not an adjustment layer")
    template adj: untyped = l.content.adjustment
    if adj.kind == akUnsupported: raise other("this adjustment can't be edited (kept as imported)")
    let kind = adjustmentKind(adj)
    # No params resets to the defaults; otherwise params merge over the current values.
    if eyedropped.isSome: adj = eyedropped.get
    elif adjUserKeys(p).len == 0: adj = adjDefaultFor(kind, mode)
    else: adj = adjFromParams(kind, p, some(adj), mode)
  jnull()

proc solidColorFillCmd(s: var Session; p: JsonNode): JsonNode =
  let c = colorParam(p, "color", s.tools.foreground)
  let id = s.editGet("New Color Fill Layer", LayerId):
    let fill = solidFill(rgba(c[0], c[1], c[2], c[3])).inMode(doc.mode)
    var l = newLayer(doc.nextLayerName("Color Fill"), fillContent(fill))
    maskNewLayer(doc, l, p, "layer.newFillLayer.solidColor")
    let id = doc.insertAbove(active, l)
    active = some(id)
    id
  %*{"layer": id}

proc gradientFillCmd(s: var Session; p: JsonNode): JsonNode =
  let a = colorParam(p, "from", s.tools.foreground)
  let b = colorParam(p, "to", s.tools.background)
  let angle = f32Or(p, "angle", 90)
  let style = gradientStyle(p.getStrOr("style", "linear"))
  let reverse = p.getBoolOr("reverse", false)
  let id = s.editGet("New Gradient Fill Layer", LayerId):
    let fill = gradientFill(@[(0'f32, rgba(a[0], a[1], a[2], a[3])), (1'f32, rgba(b[0], b[1], b[2], b[3]))],
                            angle, 1, style, reverse).inMode(doc.mode)
    var l = newLayer(doc.nextLayerName("Gradient Fill"), fillContent(fill))
    maskNewLayer(doc, l, p, "layer.newFillLayer.gradient")
    let id = doc.insertAbove(active, l)
    active = some(id)
    id
  %*{"layer": id}

proc clearCmd(s: var Session; p: JsonNode): JsonNode =
  let bg = s.tools.background
  # A targeted alpha channel or Quick Mask is filled instead.
  let t = targetedChannel(s)
  if t.isSome:
    s.editDo("Clear"): clearChannel(doc, t.get, bg)
    return jnull()
  let id = layerParam(s, p)
  # A targeted colour channel clears that channel only.
  let k = targetedColor(s)
  if k.isSome:
    s.editDo("Clear"): clearColorChannel(doc, id, k.get, bg)
    return jnull()
  s.editDo("Clear"):
    let sel = doc.selection
    let area = if sel.isSome: sel.get.contentBounds else: doc.bounds
    clearArea(doc, id, area, sel, bg)
  jnull()

proc documentPixel(s: var Session; p: JsonNode): JsonNode =
  proc coord(k: string): int =
    let v = intParam(p, k).get(0)
    if v < int64(low(int32)) or v > int64(high(int32)): raise bad("document.pixel", k & " must fit in 32 bits")
    int(v)
  let x = coord("x")
  let y = coord("y")
  let d = s.activeOrRaise()
  let buf = render(d.doc[], rectXywh(x, y, 1, 1))
  let px = if buf.px.len > 0: buf.px[0] else: [0'f32, 0, 0, 0]
  %*[px[0], px[1], px[2], px[3]]

proc commandListCmd(s: var Session; p: JsonNode): JsonNode =
  result = newJArray()
  for c in commandSpecs():
    var menu = newJArray()
    for m in c.menu: menu.add %m
    result.add %*{"id": c.id, "label": c.label, "menu": menu,
                  "shortcut": (if c.shortcut.isSome: %c.shortcut.get else: newJNull()),
                  "params": c.params, "enabled": c.enabled(s).len == 0}

proc baseSpecs*(): seq[CommandSpec] =
  ## The first part of `commands::build()`, before the modules' `specs()`.
  result = @[
    # File
    cmd("file.new", "New…", ["File"], "Cmd+N",
        """{"width":1..300000=1920,"height":1..300000=1080 (pixel counts are rounded),"mode":"rgb|gray|grayscale|cmyk|lab"="rgb","depth":8|16|32=8,"background":"white|black|backgroundColor|transparent|#rrggbb"="white","backgroundColor":[r,g,b]? (0..1; defaults to toolbox colour),"resolution":1..30000=72 (ppi),"name":str}""",
        always, fileNew),
    cmd("file.close", "Close", ["File"], "Cmd+W", """{"document":index?}""", hasDocE, runProc do:
      let v = p.pget("document").asU64
      let i = if v.isSome: int(min(v.get, uint64(high(int)))) elif s.activeIndex.isSome: s.activeIndex.get else: raise noDocument()
      if s.close(i).isNone: raise noDocument()
      jnull()),
    # Edit
    cmd("edit.undo", "Undo", ["Edit"], "Cmd+Z", "{}", canUndoE, runProc(%s.undo())),
    cmd("edit.redo", "Redo", ["Edit"], "Cmd+Shift+Z", "{}", canRedoE, runProc(%s.redo())),
    cmd("edit.fill", "Fill…", ["Edit"], "Shift+F5",
        """{"contents":"foreground|background|color|contentAware|pattern|history|black|gray|white"="color","color":"#rrggbb|[r,g,b,a]"=foreground (contents=color),"pattern":id|name (contents=pattern),"scale":%=100,"angle":deg,"state":index? (contents=history; default the oldest state),"colorAdaptation":bool=true (contents=contentAware),"mode":"normal|multiply|…"="normal","opacity":0..100=100,"preserveTransparency":bool=false,"target":"pixels"|"mask"|"quickMask"|{"channel":i}?}""",
        hasPixelOrChannelE, fillCmd),
    cmd("edit.clear", "Clear", ["Edit"], "Delete", "{} (a targeted channel or Quick Mask is cleared alone)",
        hasPixelOrChannelE, clearCmd),
    # Select
    cmd("select.all", "All", ["Select"], "Cmd+A", "{}", hasDocE, runProc do:
      s.editDo("Select All"):
        var m = newSurface(Gray8)
        m.fillRect(doc.bounds, [1'f32])
        doc.selection = some(m)
      jnull()),
    cmd("select.deselect", "Deselect", ["Select"], "Cmd+D", "{}", hasSelectionE, runProc do:
      s.editDo("Deselect"): doc.selection = none(Surface)
      jnull()),
    cmd("select.inverse", "Inverse", ["Select"], "Cmd+Shift+I", "{}", hasSelectionE, runProc do:
      s.editDo("Inverse"):
        let old = if doc.selection.isSome: doc.selection.get else: newSurface(Gray8)
        doc.selection = none(Surface)
        let b = doc.bounds
        var data = old.readRegion(b)
        for v in data.mitems: v = 1 - v
        var m = newSurface(Gray8)
        m.writeRegion(b, data)
        m.prune()
        doc.selection = some(m)
      jnull()),
    cmd("select.rect", "Rectangular Selection", [], "",
        """{"x":i32,"y":i32,"width":u32,"height":u32,"mode":"replace|add|subtract|intersect"="replace","ellipse":bool=false,"line":"row|column"?,"antiAlias":bool=true,"feather":px=0}""",
        hasDocE, selectRect),
    # Layer
    cmd("layer.new.layer", "Layer…", ["Layer", "New"], "Cmd+Shift+N",
        """{"name":str?,"clipToPrevious":bool=false (Use Previous Layer to Create Clipping Mask; inside a clip group the layer joins it anyway),"color":"none|red|orange|yellow|green|seafoam|blue|indigo|magenta|fuchsia|violet|gray"="none","mode":"Normal|Multiply|…"="Normal","opacity":0..1=1,"fillNeutral":bool=false (fill with the mode's neutral color: modes without one are an error)}""",
        hasDocE, newLayerRun),
    cmd("layer.new.group", "Group…", ["Layer", "New"], "", """{"name":str?}""", hasDocE, runProc do:
      let id = s.editGet("New Group", LayerId):
        let n = p.pget("name").asStr
        let name = if n.isSome: n.get else: doc.nextLayerName("Group")
        let id = doc.insertAbove(active, groupLayer(name, @[]))
        active = some(id)
        id
      noteInertInsert(s, id)
      %*{"layer": id}),
    cmd("layer.groupLayers", "Group Layers", ["Layer"], "Cmd+G",
        """{"layer":id?,"name":str?} (no layer: every selected layer)""", hasLayerE, groupLayers),
    cmd("layer.duplicate", "Duplicate Layer…", ["Layer"], "",
        """{"layer":id?,"inPlace":bool=false (artboards: keep the copy on the original instead of beside it),"name":str? (one layer: the copy's name)} (no layer: every selected layer)""",
        hasLayerE, duplicateCmd),
    cmd("layer.delete", "Delete Layer", ["Layer", "Delete"], "", """{"layer":id?} (no layer: every selected layer)""",
        hasLayerE, deleteCmd),
    cmd("layer.select", "Select Layer", [], "",
        """{"layer":id,"mode":"replace|toggle|range|add"="replace"} (toggle = ⌘-click, range = ⇧-click)""",
        hasDocE, selectLayerCmd),
    cmd("layer.setProps", "Layer Properties", [], "",
        """{"layer":id?,"name":str?,"visible":bool?,"opacity":0..1?,"fill":0..1?,"blend":"Multiply|…"?,"clipped":bool?,"locked":bool?,"locks":{"transparency","pixels","position","artboard","all":bool}?,"channels":[bool,…]? (Advanced Blending: which colour channels blend, R G B / C M Y K / L a b)}""",
        hasLayerE, setProps),
    cmd("layer.arrange.bringForward", "Bring Forward", ["Layer", "Arrange"], "Cmd+]", """{"layer":id?}""", hasLayerE,
        runProc(arrange(s, p, 1))),
    cmd("layer.arrange.sendBackward", "Send Backward", ["Layer", "Arrange"], "Cmd+[", """{"layer":id?}""", hasLayerE,
        runProc(arrange(s, p, -1))),
    cmd("layer.arrange.bringToFront", "Bring to Front", ["Layer", "Arrange"], "Cmd+Shift+]", """{"layer":id?}""", hasLayerE,
        runProc(arrange(s, p, high(int32)))),
    cmd("layer.arrange.sendToBack", "Send to Back", ["Layer", "Arrange"], "Cmd+Shift+[", """{"layer":id?}""", hasLayerE,
        runProc(arrange(s, p, low(int32)))),
    cmd("layer.createClippingMask", "Create Clipping Mask", ["Layer"], "Cmd+Alt+G",
        """{"layer":id?} (no layer: every selected layer but the lowest clips to the layer below it)""", hasLayerE,
        runProc(setClipped(s, p, true))),
    cmd("layer.releaseClippingMask", "Release Clipping Mask", ["Layer"], "", """{"layer":id?} (no layer: every selected layer)""",
        hasLayerE, runProc(setClipped(s, p, false))),
    cmd("layer.layerMask.revealAll", "Reveal All", ["Layer", "Layer Mask"], "",
        """{"layer":id?} (no layer: every selected layer that can take a mask)""", hasLayerE,
        runProc(setMask(s, p, "Add Layer Mask", some(revealAllMask())))),
    cmd("layer.layerMask.hideAll", "Hide All", ["Layer", "Layer Mask"], "",
        """{"layer":id?} (no layer: every selected layer that can take a mask)""", hasLayerE,
        runProc(setMask(s, p, "Add Layer Mask", some(hideAllMask())))),
    cmd("layer.layerMask.revealSelection", "Reveal Selection", ["Layer", "Layer Mask"], "",
        """{"layer":id?} (no layer: every selected layer that can take a mask)""", hasSelectionE, runProc do:
      let d = s.active
      if d.isNil or d.doc.selection.isNone: raise other("no selection")
      var mask = revealAllMask()
      mask.surface = d.doc.selection.get
      setMask(s, p, "Add Layer Mask", some(mask))),
    cmd("layer.layerMask.delete", "Delete", ["Layer", "Layer Mask"], "", """{"layer":id?}""", hasLayerE,
        runProc(setMask(s, p, "Delete Layer Mask", none(LayerMask)))),
    cmd("layer.mergeDown", "Merge Down", ["Layer"], "", """{"layer":id?}""", hasLayerE, mergeDownCmd),
    cmd("layer.flattenImage", "Flatten Image", ["Layer"], "", "{}", hasDocE, flattenImageCmd),
    cmd("layer.newFillLayer.solidColor", "Solid Color…", ["Layer", "New Fill Layer"], "",
        """{"color":"#rrggbb"=foreground,"path":"work"|saved path name|{…path}? (the active path: becomes the vector mask, making a shape; else the selection is the layer mask)}""",
        hasDocE, solidColorFillCmd),
    cmd("layer.newFillLayer.gradient", "Gradient…", ["Layer", "New Fill Layer"], "",
        """{"from":"#rrggbb","to":"#rrggbb","angle":deg=90,"style":"linear|radial|angle|reflected|diamond","reverse":bool,"path":"work"|saved path name|{…path}? (the active path: becomes the vector mask; else the selection is the layer mask)}""",
        hasDocE, gradientFillCmd),
    cmd("layer.setAdjustment", "Adjustment Properties", [], "",
        """{"layer":id?, …params of that adjustment kind} (Levels, Curves: "eyedropper":{"point":"black|gray|white","at":[x,y],"size":1..101=1} or {"point",…,"color":[r,g,b] 0..1} = the Properties panel's Set Black/Neutral Gray/White Point, sampling the image below the layer, RGB and Grayscale (no gray) only)""",
        hasLayerE, setAdjustmentCmd),
    cmd("layer.moveTo", "Reorder Layer", [], "",
        """{"layer":id?| "layers":[id,…]?, "target":id,"position":"above|below|into"="above","copy":bool=false} (layers: one undoable move, document order preserved; copy: place duplicates there and leave the layers, as ⌥-dragging a Layers panel row)""",
        hasLayerE, moveTo),
    cmd("layer.translate", "Move Layer", [], "",
        """{"layer":id?,"dx":i32,"dy":i32} (no layer: every selected layer; linked layers follow)""", hasLayerE, translateCmd),
    # Image
    cmd("image.imageRotation.flipCanvasHorizontal", "Flip Canvas Horizontal", ["Image", "Image Rotation"], "", "{}", hasDocE,
        runProc(turnCanvasCmd(s, "Flip Canvas Horizontal", tFlipHorizontal))),
    cmd("image.imageRotation.flipCanvasVertical", "Flip Canvas Vertical", ["Image", "Image Rotation"], "", "{}", hasDocE,
        runProc(turnCanvasCmd(s, "Flip Canvas Vertical", tFlipVertical))),
    cmd("image.imageRotation.180", "180°", ["Image", "Image Rotation"], "", "{}", hasDocE,
        runProc(turnCanvasCmd(s, "Rotate Canvas", tRotate180))),
    cmd("image.imageRotation.90cw", "90° Clockwise", ["Image", "Image Rotation"], "", "{}", hasDocE,
        runProc(turnCanvasCmd(s, "Rotate Canvas", tCw90))),
    cmd("image.imageRotation.90ccw", "90° Counter Clockwise", ["Image", "Image Rotation"], "", "{}", hasDocE,
        runProc(turnCanvasCmd(s, "Rotate Canvas", tCcw90))),
    cmd("image.adjustments.desaturate", "Desaturate", ["Image", "Adjustments"], "Cmd+Shift+U", "{}", hasPixelLayerE,
        runProc(destructiveAdjust(s, "Desaturate",
          Adjustment(kind: akHueSaturation, hue: 0, saturation: -100, lightness: 0, colorize: false, ranges: defaultHueRanges()),
          jnull()))),
    # Paint
    cmd("paint.stroke", "Brush Stroke", [], "",
        """{"points":[[x,y,pressure?,tiltX?,tiltY?,rotation?,timeMs?,wheel?],…],"brush":{…BrushSettings}?,"preset":name?,"size":0.5..5000 px?,"hardness":0..1?,"opacity":0..1?,"flow":0..1?,"spacing":0..10?,"color":"#rrggbb"?=foreground,"mode":"normal|multiply|screen|…|clear (erases with this brush)"="normal","erase":bool?,"smoothing":0..1?,"zoom":number=1,"seed":u64?,"target":"pixels"|"mask"|"quickMask"|{"channel":i}=Channels panel target}""",
        hasPaintableE, paintStroke),
    cmd("tools.setColors", "Set Colors", [], "", """{"foreground":"#rrggbb"?,"background":"#rrggbb"?}""", always, runProc do:
      s.tools.foreground = colorParam(p, "foreground", s.tools.foreground)
      s.tools.background = colorParam(p, "background", s.tools.background)
      jnull()),
    cmd("tools.swapColors", "Switch Foreground and Background Colors", [], "X", "{}", always, runProc do:
      swap(s.tools.foreground, s.tools.background)
      jnull()),
    cmd("tools.defaultColors", "Default Foreground and Background Colors", [], "D", "{}", always, runProc do:
      if s.tools.maskTargeted:
        s.tools.foreground = MaskColors[0]
        s.tools.background = MaskColors[1]
      else:
        s.tools.foreground = [0'f32, 0, 0, 1]
        s.tools.background = [1'f32, 1, 1, 1]
      jnull()),
    # Queries (not journaled)
    cmd("session.inspect", "Inspect Session", [], "", "{}", always, runProc(inspectSession(s)), journal = false),
    cmd("document.inspect", "Inspect Document", [], "", """{"document":index?}""", hasDocE, runProc do:
      let v = p.pget("document")
      if not v.isNil:
        let i = v.asU64
        if i.isNone: raise bad("document.inspect", "`document` must be an index")
        let d = s.docAt(int(min(i.get, uint64(high(int)))))
        if d.isNil: raise other("no document at index " & $i.get)
        inspectDocument(d[])
      else:
        inspectDocument(s.activeOrRaise()[]), journal = false),
    cmd("document.activate", "Activate Document", [], "", """{"document":index}""", hasDocE, runProc do:
      let i = p.pget("document").asU64
      if i.isNone: raise bad("document.activate", "missing `document`")
      let idx = int(min(i.get, uint64(high(int))))
      if s.setActive(idx): jnull() else: raise other("no document at index " & $i.get), journal = false),
    cmd("document.move", "Move Document", [], "", """{"document":index?,"to":index}""", hasDocE, runProc do:
      proc index(key: string): Option[int] =
        let v = p.pget(key)
        if v.isNil: return none(int)
        let u = v.asU64
        if u.isNone: raise bad("document.move", "`" & key & "` must be a tab index")
        some(int(min(u.get, uint64(high(int)))))
      let fromI = index("document")
      let src = if fromI.isSome: fromI.get elif s.activeIndex.isSome: s.activeIndex.get else: raise noDocument()
      let toI = index("to")
      if toI.isNone: raise bad("document.move", "missing `to`")
      let to = s.moveDocument(src, toI.get)
      if to.isNone: raise noDocument()
      %*{"document": to.get}, journal = false),
    cmd("command.list", "List Commands", [], "", "{}", always, commandListCmd, journal = false),
    cmd("document.pixel", "Read Composite Pixel", [], "", """{"x":i32,"y":i32}""", hasDocE, documentPixel, journal = false),
  ]
  # Adjustment layers + destructive adjustments, generated from one list.
  for (kind, label, params) in adjTable():
    result.add cmd("layer.newAdjustmentLayer." & kind, label, ["Layer", "New Adjustment Layer"], "",
      params & " + {" & NewLayerPath & "}", hasDocE, runProc do:
        let kind = p.getStrOr("__kind", "invert")
        let eyedropped = if kind in ["levels", "curves"]: eyedropperFromParams(s, kind, p) else: none(Adjustment)
        let adj = if eyedropped.isSome: eyedropped.get else: adjFromParams(kind, p, none(Adjustment), docMode(s))
        newAdjustment(s, adj, p, "layer.newAdjustmentLayer." & kind))
    let shortcut = case kind
      of "levels": "Cmd+L"
      of "curves": "Cmd+M"
      of "hueSaturation": "Cmd+U"
      of "colorBalance": "Cmd+B"
      of "invert": "Cmd+I"
      of "blackWhite": "Cmd+Alt+Shift+B"
      else: ""
    result.add cmd("image.adjustments." & kind, label, ["Image", "Adjustments"], shortcut, params, hasAdjustableE, runProc do:
      let kind = p.getStrOr("__kind", "invert")
      let eyedropped = if kind in ["levels", "curves"]: eyedropperFromParams(s, kind, p) else: none(Adjustment)
      # A smart filter re-runs its params, so it records what an eyedropper resolved to.
      var recorded = p
      if eyedropped.isSome:
        let resolved = adjToParams(eyedropped.get)
        if p.isObject and resolved.isObject:
          recorded = newJObject()
          for k, v in p.fields:
            if k != "eyedropper": recorded[k] = v
          for k, v in resolved.fields: recorded[k] = v
      let adj = if eyedropped.isSome: eyedropped.get else: adjFromParams(kind, p, none(Adjustment), docMode(s))
      let label = adj.label
      # A smart object records the adjustment as a smart filter (Photoshop).
      if adjustAsSmartFilter(s, kind, label, recorded): return jnull()
      destructiveAdjust(s, label, adj, p))
