## Procedures of engine modules not ported yet. Each raises "not ported yet" (or keeps the
## neutral behaviour for the dispatch hooks) until its module lands and replaces it. Part of the
## `engine/core` unit: included, not imported.

proc notPorted(what: string): ref EngineError = other("`" & what & "` is not ported to Nim yet")

# ---------------------------------------------------------------- dispatch hooks (jobs.rs)

proc onCloseDocument(s: var Session; index: int) = discard
proc beforeCommandHook(s: var Session; id: string): Option[JsonNode] = none(JsonNode)
proc validateCommandParams(s: var Session; id: string; params: JsonNode) = discard
proc redirectCommand(s: var Session; id: string): Option[(string, JsonNode)] = none((string, JsonNode))
proc hiddenTargetRefusal(s: Session; id: string; params: JsonNode): string = ""
proc afterCommandHooks(s: var Session; id: string) = discard
proc replayable(id: string): bool = false
proc coalesceJournal(s: var Session; id: string; params: JsonNode): bool = false

# ---------------------------------------------------------------- extra_cmds

proc isBackground*(l: Layer): bool =
  l.name == "Background" and l.locks.transparency and l.locks.position and l.content.kind == lkRaster

proc unlockBackground*(l: var Layer) =
  l.name = "Layer 0"
  l.locks.transparency = false
  l.locks.position = false

proc backgroundToLayerForMask*(doc: var Document; id: LayerId) =
  ## Before `id` gets a layer mask: the Background becomes a normal layer ("Layer 0") first.
  if doc.layers.len > 0 and doc.layers[0].id == id and isBackground(doc.layers[0]):
    unlockBackground(doc.layers[0])

# ---------------------------------------------------------------- document_preset_cmds

proc colorModeParam*(mode: string): Option[ColorMode] =
  case mode
  of "rgb": some(cmRgb)
  of "gray", "grayscale": some(cmGrayscale)
  of "cmyk": some(cmCmyk)
  of "lab": some(cmLab)
  else: none(ColorMode)

proc sampleTypeParam*(depth: uint64): Option[SampleType] =
  case depth
  of 8: some(stU8)
  of 16: some(stU16)
  of 32: some(stF32)
  else: none(SampleType)

proc f32Triple*(v: JsonNode): Option[array[3, float32]] =
  ## serde's `[f32; 3]`: exactly three numbers.
  if not v.isArray or v.len != 3: return
  var c: array[3, float32]
  for i in 0 ..< 3:
    let f = v.elems[i].asF64
    if f.isNone: return
    c[i] = float32(f.get)
  some(c)

proc backgroundColorParam*(p: JsonNode; fallback: array[4, float32]; cmdId: string): array[4, float32] =
  ## A saved Background Color must not follow later changes to the toolbox colour.
  let v = p.pget("backgroundColor")
  var c: array[3, float32]
  if v.isNil: c = [fallback[0], fallback[1], fallback[2]]
  else:
    let t = f32Triple(v)
    if t.isNone: raise bad(cmdId, "`backgroundColor` must be [r,g,b] in 0..1")
    c = t.get
  for x in c:
    if x != x or abs(x) == Inf or x < 0 or x > 1: raise bad(cmdId, "`backgroundColor` must be [r,g,b] in 0..1")
  [c[0], c[1], c[2], 1'f32]

proc fmtNum(f: float64): string =
  ## Rust's `{}` for an f64 (`300000`, `30000`).
  if f == trunc(f) and abs(f) < 1e16: $int64(f) else: $f

proc validateNewParams*(s: Session; p: JsonNode) =
  ## Validates creation params before dispatch can add a document.
  const cmdId = "file.new"
  if p.isNull: return
  if not p.isObject: raise bad(cmdId, "params must be a JSON object")
  for key, value in p.fields:
    var valid = false
    var expected = ""
    case key
    of "width", "height", "resolution":
      let mx = if key == "resolution": MaxResolution else: float64(MaxDimension)
      let n = value.asF64
      valid = n.isSome and n.get == n.get and abs(n.get) != Inf and n.get >= 1 and n.get <= mx
      expected = "a number within 1..=" & fmtNum(mx)
    of "mode":
      valid = value.asStr.isSome and colorModeParam(value.str).isSome
      expected = "rgb, gray, grayscale, cmyk or lab"
    of "depth":
      let d = value.asU64
      valid = d.isSome and sampleTypeParam(d.get).isSome
      expected = "8, 16 or 32"
    of "background":
      let v = value.asStr
      if v.isSome:
        if v.get in ["white", "black", "transparent", "backgroundColor"]: valid = true
        else:
          let hex = if v.get.startsWith("#"): v.get[1 .. ^1] else: v.get
          valid = hex.allCharsInSet(HexDigits) and parseHex(v.get).isSome
      expected = "white, black, transparent, backgroundColor or a valid hex colour"
    of "backgroundColor":
      discard backgroundColorParam(p, s.tools.background, cmdId)
      continue
    of "name":
      valid = value.kind == JString
      expected = "a string"
    else: raise bad(cmdId, "unknown parameter `" & key & "`")
    if not valid: raise bad(cmdId, "`" & key & "` must be " & expected)
  if p.getStrOr("background", "") == "backgroundColor" and p.pget("background").asStr.isSome:
    discard backgroundColorParam(p, s.tools.background, cmdId)

# ---------------------------------------------------------------- not ported yet

proc isWork*(name: Option[string]): bool = raise notPorted("vector_cmds::is_work")
proc parsePath*(v: JsonNode): Path = raise notPorted("vector_cmds::parse_path")
proc translateVectors*(doc: Document; l: var Layer; dx, dy: float64) = discard
proc transformShape*(sh: var ShapeLayer; a: Affine) = raise notPorted("vector_cmds::transform_shape")
proc refreshShape*(doc: Document; sh: var ShapeLayer) = raise notPorted("vector_cmds::refresh_shape")
proc adjFromParams*(kind: string; p: JsonNode; current: Option[Adjustment]; mode: ColorMode): Adjustment =
  raise notPorted("adjust_params::from_params")
proc adjDefaultFor*(kind: string; mode: ColorMode): Adjustment = raise notPorted("adjust_params::default_for")
proc layerEyedropper*(s: Session; id: LayerId; p: JsonNode): Option[Adjustment] = none(Adjustment)
proc adjUserKeys*(p: JsonNode): seq[string] = raise notPorted("adjust_params::user_keys")
proc eyedropperFromParams*(s: Session; kind: string; p: JsonNode): Option[Adjustment] = none(Adjustment)
proc adjToParams*(a: Adjustment): JsonNode = raise notPorted("adjust_params::to_params")
proc refreshText*(doc: Document; t: var TextLayer) = raise notPorted("type_cmds::refresh")
proc shiftSmart*(sm: var SmartObject; dx, dy: int) = raise notPorted("smart_cmds::shift_smart")
proc transformPlacement*(sm: var SmartObject; a: Affine) = raise notPorted("smart_cmds::transform_placement")
proc refreshSmartLayer*(doc: Document; l: var Layer) = raise notPorted("smart_cmds::refresh_layer")
proc fillCmd*(s: var Session; p: JsonNode): JsonNode = raise notPorted("fill_cmds::fill")
proc fillColor*(surf: var Surface; area: Rect; color: array[4, float32]; sel: Option[Surface]) =
  raise notPorted("fill_cmds::fill_color")
proc clearArea*(doc: var Document; id: LayerId; area: Rect; sel: Option[Surface]; bg: array[4, float32]) =
  raise notPorted("edit_cmds::clear_area")
proc placeCopy*(doc: var Document; id: LayerId) = discard
proc fitCanvas*(doc: var Document) = discard
proc gradientStyle*(name: string): GradientStyle = raise notPorted("layer_style::gradient_style")
proc paintStroke*(s: var Session; p: JsonNode): JsonNode = raise notPorted("brush_cmds::paint_stroke")
proc hasAdjustableE*(s: Session): string = hasPixelOrChannelE(s)
proc adjustAsSmartFilter*(s: var Session; kind, label: string; p: JsonNode): bool = false
proc filterParamsFor*(id: string; p: JsonNode): Option[FilterParams] = none(FilterParams)
proc symmetryInspect*(p: PaintingSymmetry): JsonNode = newJNull()
