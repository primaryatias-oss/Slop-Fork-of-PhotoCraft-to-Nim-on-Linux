## photocraft/doc: the document model (pure data): layers of every kind, masks, effects,
## adjustments, text, vectors, smart objects, channels, guides, comps, slices, variables, video.
## Types live in `doc/types.nim`; this module adds constructors, tree operations and the
## serde-compatible JSON of the enum-like types.

import std/[sets, strutils, strformat, algorithm, math, unicode, tables, hashes, sequtils]
import doc/types
export types

# ---------------------------------------------------------------- ids

var nextIdCounter {.threadvar.}: uint64

proc nextId(): uint64 =
  if nextIdCounter == 0: nextIdCounter = 1
  result = nextIdCounter
  inc nextIdCounter

proc `==`*(a, b: LayerId): bool {.borrow.}
proc `<`*(a, b: LayerId): bool {.borrow.}
proc `$`*(a: LayerId): string = $uint64(a)
proc hash*(a: LayerId): Hash = hash(uint64(a))
proc `==`*(a, b: SmartContentsId): bool {.borrow.}
proc `==`*(a, b: DocId): bool {.borrow.}
proc `$`*(a: DocId): string = $uint64(a)
proc freshLayerId*(): LayerId = LayerId(nextId())
proc freshContentsId*(): SmartContentsId = SmartContentsId(nextId())
proc freshDocId*(): DocId = DocId(nextId())

proc resetIdsForTesting*() =
  ## Restarts the id counter at 1, as in a fresh process (cross-checks against the Rust build).
  nextIdCounter = 1

proc ensureIdsAbove*(maxId: uint64) =
  ## Advances the id counter past `maxId` so ids loaded from a file never collide with new ones.
  if maxId < high(uint64) and nextIdCounter <= maxId: nextIdCounter = maxId + 1

proc toJ*(x: LayerId): JsonNode = newJInt(BiggestInt(uint64(x)))
proc fromJ*(n: JsonNode; x: var LayerId) =
  var v: uint64
  fromJ(n, v)
  x = LayerId(v)
proc toJ*(x: SmartContentsId): JsonNode = newJInt(BiggestInt(uint64(x)))
proc toJ*(x: DocId): JsonNode = newJInt(BiggestInt(uint64(x)))

# ---------------------------------------------------------------- shared bytes

proc shared*(b: seq[byte]): SharedBytes = SharedBytes(data: b)
proc `==`*(a, b: SharedBytes): bool =
  if a.isNil or b.isNil: return a.isNil and b.isNil
  cast[pointer](a) == cast[pointer](b) or a.data == b.data
proc `==`*(a, b: SharedF32): bool =
  if a.isNil or b.isNil: return a.isNil and b.isNil
  cast[pointer](a) == cast[pointer](b) or a.data == b.data
proc toJ*(x: SharedBytes): JsonNode =
  if x.isNil: newJNull() else: toJ(x.data)
proc fromJ*(n: JsonNode; x: var SharedBytes) =
  if n.kind == JNull: x = nil
  else:
    var d: seq[byte]
    fromJ(n, d)
    x = SharedBytes(data: d)
proc toJ*(x: SharedF32): JsonNode =
  if x.isNil: newJNull() else: toJ(x.data)
proc fromJ*(n: JsonNode; x: var SharedF32) =
  if n.kind == JNull: x = nil
  else:
    var d: seq[float32]
    fromJ(n, d)
    x = SharedF32(data: d)

# ---------------------------------------------------------------- variant equality
# The enum-like types are flat objects holding every variant's fields; equality compares only
# the active variant (Rust's derived `PartialEq` on the enum), through its serde form.
proc `==`*(a, b: Adjustment): bool {.noSideEffect.}
proc `==`*(a, b: Contour): bool {.noSideEffect.}
proc `==`*(a, b: FxPaint): bool {.noSideEffect.}
proc `==`*(a, b: Effect): bool {.noSideEffect.}
proc `==`*(a, b: LiveShape): bool {.noSideEffect.}
proc `==`*(a, b: Fill): bool {.noSideEffect.}
proc `==`*(a, b: TextShape): bool {.noSideEffect.}
proc `==`*(a, b: VideoSource): bool {.noSideEffect.}
proc `==`*(a, b: ArtboardBackground): bool {.noSideEffect.}
proc `==`*(a, b: VarKind): bool {.noSideEffect.}
proc `==`*(a, b: DataValue): bool {.noSideEffect.}
proc `==`*(a, b: SmartSource): bool {.noSideEffect.} =
  if a.kind != b.kind: return false
  case a.kind
  of ssEmbedded: a.fileName == b.fileName and a.bytes == b.bytes
  of ssLinked: a.path == b.path

# ---------------------------------------------------------------- small types

proc union*(a, b: Locks): Locks =
  Locks(transparency: a.transparency or b.transparency, pixels: a.pixels or b.pixels,
        position: a.position or b.position, artboard: a.artboard or b.artboard, all: a.all or b.all)

const LabelColorOrder* = [lcNone, lcRed, lcOrange, lcYellow, lcGreen, lcSeafoam, lcBlue, lcIndigo,
                          lcMagenta, lcFuchsia, lcViolet, lcGray]
proc id*(c: LabelColor): string = toLowerAscii($c)
proc label*(c: LabelColor): string = (if c == lcNone: "No Color" else: $c)
proc labelColorFromId*(id: string): Option[LabelColor] =
  for c in LabelColorOrder:
    if c.id == id: return some(c)

# ---------------------------------------------------------------- adjustments

const CurveMapLen* = 256

proc isIdentityCurve*(pts: openArray[CurvePoint]): bool =
  if pts.len == 2: pts[0].input == 0 and pts[0].output == 0 and pts[1].input == 1 and pts[1].output == 1
  else: pts.len < 2

proc finite(v: float32): bool {.inline.} = v == v and v != Inf and v != NegInf

proc curveMap*(outputs: openArray[float32]): seq[CurvePoint] =
  ## A 256-point pencil curve from per-level outputs (missing/non-finite repeat the previous).
  let last = float32(CurveMapLen - 1)
  var prev = 0'f32
  for k in 0 ..< CurveMapLen:
    let o = if k < outputs.len and outputs[k].finite: clamp(outputs[k], 0'f32, 1'f32) else: prev
    prev = o
    result.add CurvePoint(input: float32(k) / last, output: o)

proc curveMapOutputs*(points: openArray[CurvePoint]): seq[float32] =
  var pts: seq[(float32, float32)]
  for p in points:
    if p.input.finite and p.output.finite: pts.add (p.input, p.output)
  pts.sort(proc (a, b: (float32, float32)): int = cmp(a[0], b[0]))
  let last = float32(CurveMapLen - 1)
  for k in 0 ..< CurveMapLen:
    let x = float32(k) / last
    var y = x
    if pts.len >= 2:
      let (a, b) = (pts[0], pts[^1])
      if x <= a[0]: y = a[1]
      elif x >= b[0]: y = b[1]
      else:
        var i = 0
        while i < pts.len and pts[i][0] < x: inc i
        if i >= 1 and i < pts.len and pts[i][0] > pts[i - 1][0]:
          let (p0, p1) = (pts[i - 1], pts[i])
          y = p0[1] + (p1[1] - p0[1]) * (x - p0[0]) / (p1[0] - p0[0])
        elif i < pts.len: y = pts[i][1]
        else: y = b[1]
    result.add clamp(y, 0'f32, 1'f32)

proc lut3dLen*(size: uint32): Option[int] =
  if size < 2 or size > 1024: return none(int)
  some(int(size) * int(size) * int(size) * 3)

proc defaultLevelsChannel*(): LevelsChannel = LevelsChannel(inWhite: 1, gamma: 1, outWhite: 1)
proc defaultOf*(_: typedesc[LevelsChannel]): LevelsChannel = defaultLevelsChannel()

proc neutralHueRange*(i: int): HueRange =
  let c = 60'f32 * float32(i)
  HueRange(bounds: [c - 45, c - 15, c + 15, c + 45])

proc defaultHueRanges*(): array[6, HueRange] =
  for i in 0..5: result[i] = neutralHueRange(i)

proc remEuclid(a, b: float32): float32 =
  result = a mod b
  if result < 0: result += b

proc canonicalBounds*(b: array[4, float32]): array[4, float32] =
  for v in b:
    if not v.finite: return [-45'f32, -15, 15, 45]
  result[0] = b[0]
  for k in 1..3: result[k] = min(result[k - 1] + remEuclid(b[k] - result[k - 1], 360), result[0] + 359)
  let centre = (result[1] + result[2]) / 2
  let shift = remEuclid(centre + 30, 360) - 30 - centre
  for v in result.mitems: v += shift

proc isNeutral*(r: HueRange): bool = r.hue == 0 and r.saturation == 0 and r.lightness == 0

proc weight*(r: HueRange; deg: float32): float32 =
  let a = r.bounds[0]
  template rel(x: float32): float32 = (if x.finite: remEuclid(x - a, 360) else: 0'f32)
  let b = rel(r.bounds[1])
  let c = max(rel(r.bounds[2]), b)
  let d = max(rel(r.bounds[3]), c)
  let h = rel(deg)
  if h <= b: (if b <= 0: 1'f32 else: h / b)
  elif h <= c: 1'f32
  elif h <= d: (if d <= c: 1'f32 else: (d - h) / (d - c))
  else: 0'f32

proc identityLine(): seq[CurvePoint] = @[CurvePoint(input: 0, output: 0), CurvePoint(input: 1, output: 1)]

proc newAdjustment*(kind: AdjustmentKind): Adjustment =
  ## The neutral settings of each adjustment kind.
  result.kind = kind
  result.levelsMaster = defaultLevelsChannel()
  for c in result.levelsPerChannel.mitems: c = defaultLevelsChannel()
  result.levelsBlack = defaultLevelsChannel()
  result.ranges = defaultHueRanges()
  case kind
  of akCurves:
    result.curvesMaster = identityLine()
    for c in result.curvesPerChannel.mitems: c = identityLine()
  of akExposure: result.gamma = 1
  of akChannelMixer:
    result.matrix = [[1'f32, 0, 0, 0], [0'f32, 1, 0, 0], [0'f32, 0, 1, 0]]
  of akPhotoFilter:
    result.filterColor = [0.925'f32, 0.541, 0.0]
    result.density = 0.25
    result.preserveLuminosity = true
  of akColorBalance: result.preserveLuminosity = true
  of akBlackWhite: result.weights = [0.4'f32, 0.6, 0.4, 0.6, 0.2, 0.8]
  of akPosterize: result.levels = 4
  of akThreshold: result.level = 0.5
  of akGradientMap: result.gmStops = @[(0'f32, [0'f32, 0, 0]), (1'f32, [1'f32, 1, 1])]
  of akSelectiveColor: result.relative = true
  else: discard

proc defaultOf*(_: typedesc[Adjustment]): Adjustment = newAdjustment(akInvert)

proc label*(a: Adjustment): string =
  case a.kind
  of akBrightnessContrast: "Brightness/Contrast"
  of akLevels: "Levels"
  of akCurves: "Curves"
  of akExposure: "Exposure"
  of akVibrance: "Vibrance"
  of akHueSaturation: "Hue/Saturation"
  of akColorBalance: "Color Balance"
  of akBlackWhite: "Black & White"
  of akPhotoFilter: "Photo Filter"
  of akChannelMixer: "Channel Mixer"
  of akColorLookup: "Color Lookup"
  of akInvert: "Invert"
  of akPosterize: "Posterize"
  of akThreshold: "Threshold"
  of akGradientMap: "Gradient Map"
  of akSelectiveColor: "Selective Color"
  of akUnsupported: "Adjustment"

proc identityCurves*(): Adjustment = newAdjustment(akCurves)
proc identityLevels*(): Adjustment = newAdjustment(akLevels)
proc defaultHueSaturation*(): Adjustment = newAdjustment(akHueSaturation)

# serde: externally tagged, field names as in Rust
proc toJ*(a: Adjustment): JsonNode =
  var o = newJObject()
  case a.kind
  of akBrightnessContrast:
    o["brightness"] = toJ(a.brightness); o["contrast"] = toJ(a.contrast); o["legacy"] = toJ(a.legacy)
  of akLevels:
    o["master"] = toJ(a.levelsMaster); o["per_channel"] = toJ(a.levelsPerChannel)
    o["space"] = toJ(a.space); o["black"] = toJ(a.levelsBlack)
  of akCurves:
    o["master"] = toJ(a.curvesMaster); o["per_channel"] = toJ(a.curvesPerChannel)
    o["space"] = toJ(a.space); o["black"] = toJ(a.curvesBlack); o["pencil"] = toJ(a.pencil)
  of akExposure:
    o["exposure"] = toJ(a.exposure); o["offset"] = toJ(a.offset); o["gamma"] = toJ(a.gamma)
  of akVibrance:
    o["vibrance"] = toJ(a.vibrance); o["saturation"] = toJ(a.saturation)
  of akHueSaturation:
    o["hue"] = toJ(a.hue); o["saturation"] = toJ(a.saturation); o["lightness"] = toJ(a.lightness)
    o["colorize"] = toJ(a.colorize); o["ranges"] = toJ(a.ranges)
  of akColorBalance:
    o["shadows"] = toJ(a.shadows); o["midtones"] = toJ(a.midtones); o["highlights"] = toJ(a.highlights)
    o["preserve_luminosity"] = toJ(a.preserveLuminosity)
  of akBlackWhite:
    o["weights"] = toJ(a.weights); o["tint"] = toJ(a.tint)
  of akPhotoFilter:
    o["color"] = toJ(a.filterColor); o["density"] = toJ(a.density); o["preserve_luminosity"] = toJ(a.preserveLuminosity)
  of akChannelMixer:
    o["matrix"] = toJ(a.matrix); o["monochrome"] = toJ(a.monochrome)
  of akColorLookup:
    o["name"] = toJ(a.lutName); o["lut"] = toJ(a.lut); o["size"] = toJ(a.lutSize)
    o["tetrahedral"] = toJ(a.tetrahedral); o["dither"] = toJ(a.dither)
  of akInvert: return newJString("Invert")
  of akPosterize: o["levels"] = toJ(a.levels)
  of akThreshold: o["level"] = toJ(a.level)
  of akGradientMap:
    o["stops"] = toJ(a.gmStops); o["reverse"] = toJ(a.reverse); o["dither"] = toJ(a.dither)
  of akSelectiveColor:
    o["relative"] = toJ(a.relative); o["adjustments"] = toJ(a.selective)
  of akUnsupported:
    o["psd_key"] = toJ(a.psdKey); o["raw"] = toJ(a.raw)
  tagged($a.kind, o)

proc fromJ*(n: JsonNode; a: var Adjustment) =
  let (tag, p) = untag(n)
  let k = enumFromStr(tag, AdjustmentKind)
  if k.isNone: raise serdeError("unknown adjustment " & tag)
  a = newAdjustment(k.get)
  if p.isNil: return
  if p.kind != JObject: raise serdeError("adjustment payload must be an object")
  template f(key: string; dst: untyped) =
    if p.hasKey(key): fromJ(p[key], dst)
  case a.kind
  of akBrightnessContrast:
    f("brightness", a.brightness); f("contrast", a.contrast); f("legacy", a.legacy)
  of akLevels:
    f("master", a.levelsMaster); f("per_channel", a.levelsPerChannel); f("space", a.space); f("black", a.levelsBlack)
  of akCurves:
    f("master", a.curvesMaster); f("per_channel", a.curvesPerChannel); f("space", a.space)
    f("black", a.curvesBlack); f("pencil", a.pencil)
  of akExposure:
    f("exposure", a.exposure); f("offset", a.offset); f("gamma", a.gamma)
  of akVibrance:
    f("vibrance", a.vibrance); f("saturation", a.saturation)
  of akHueSaturation:
    f("hue", a.hue); f("saturation", a.saturation); f("lightness", a.lightness); f("colorize", a.colorize); f("ranges", a.ranges)
  of akColorBalance:
    f("shadows", a.shadows); f("midtones", a.midtones); f("highlights", a.highlights); f("preserve_luminosity", a.preserveLuminosity)
  of akBlackWhite:
    f("weights", a.weights); f("tint", a.tint)
  of akPhotoFilter:
    f("color", a.filterColor); f("density", a.density); f("preserve_luminosity", a.preserveLuminosity)
  of akChannelMixer:
    f("matrix", a.matrix); f("monochrome", a.monochrome)
  of akColorLookup:
    f("name", a.lutName); f("lut", a.lut); f("size", a.lutSize); f("tetrahedral", a.tetrahedral); f("dither", a.dither)
  of akInvert: discard
  of akPosterize: f("levels", a.levels)
  of akThreshold: f("level", a.level)
  of akGradientMap:
    f("stops", a.gmStops); f("reverse", a.reverse); f("dither", a.dither)
  of akSelectiveColor:
    f("relative", a.relative); f("adjustments", a.selective)
  of akUnsupported:
    f("psd_key", a.psdKey); f("raw", a.raw)

# ---------------------------------------------------------------- effects

proc defaultGradient*(): Gradient =
  Gradient(stops: @[(0'f32, Black), (1'f32, White)], style: gsLinear, angle: 90, scale: 1, align: true)
proc defaultOf*(_: typedesc[Gradient]): Gradient = defaultGradient()
proc defaultGlobalLight*(): GlobalLight = GlobalLight(angle: 120, altitude: 30)
proc defaultOf*(_: typedesc[GlobalLight]): GlobalLight = defaultGlobalLight()
proc defaultBevelContour*(): BevelContour = BevelContour(range: 0.5)
proc defaultOf*(_: typedesc[BevelContour]): BevelContour = defaultBevelContour()
proc fxCommon*(blend: BlendMode; opacity: float32): FxCommon = FxCommon(enabled: true, blend: blend, opacity: opacity)
proc defaultOf*(_: typedesc[FxCommon]): FxCommon = fxCommon(bmNormal, 1)

proc isCorner*(c: Contour; i: int): bool = c.kind == ckCustom and i < c.corners.len and c.corners[i]

proc toJ*(c: Contour): JsonNode =
  case c.kind
  of ckLinear: newJString("Linear")
  of ckCustom:
    var o = %*{"name": c.name}
    o["points"] = toJ(c.points)
    if c.corners.len > 0: o["corners"] = toJ(c.corners)
    tagged("Custom", o)

proc fromJ*(n: JsonNode; c: var Contour) =
  let (tag, p) = untag(n)
  c = Contour()
  case tag
  of "Linear": c.kind = ckLinear
  of "Custom":
    c.kind = ckCustom
    if p != nil:
      if p.hasKey("name"): fromJ(p["name"], c.name)
      if p.hasKey("points"): fromJ(p["points"], c.points)
      if p.hasKey("corners"): fromJ(p["corners"], c.corners)
  else: raise serdeError("unknown contour " & tag)

proc toJ*(p: FxPaint): JsonNode =
  case p.kind
  of fpColor: tagged("Color", toJ(p.color))
  of fpGradient: tagged("Gradient", toJ(p.gradient))
  of fpPattern: tagged("Pattern", %*{"name": p.name, "id": p.id, "scale": toJ(p.scale)})

proc fromJ*(n: JsonNode; p: var FxPaint) =
  let (tag, v) = untag(n)
  p = FxPaint(gradient: defaultGradient(), scale: 1)
  case tag
  of "Color":
    p.kind = fpColor
    if v != nil: fromJ(v, p.color)
  of "Gradient":
    p.kind = fpGradient
    if v != nil: fromJ(v, p.gradient)
  of "Pattern":
    p.kind = fpPattern
    if v != nil:
      if v.hasKey("name"): fromJ(v["name"], p.name)
      if v.hasKey("id"): fromJ(v["id"], p.id)
      if v.hasKey("scale"): fromJ(v["scale"], p.scale)
  else: raise serdeError("unknown paint " & tag)

proc defaultOf*(_: typedesc[FxPaint]): FxPaint = FxPaint(kind: fpColor, color: Black, gradient: defaultGradient(), scale: 1)

proc label*(e: Effect): string =
  case e.kind
  of ekDropShadow: "Drop Shadow"
  of ekInnerShadow: "Inner Shadow"
  of ekOuterGlow: "Outer Glow"
  of ekInnerGlow: "Inner Glow"
  of ekStroke: "Stroke"
  of ekColorOverlay: "Color Overlay"
  of ekGradientOverlay: "Gradient Overlay"
  of ekPatternOverlay: "Pattern Overlay"
  of ekSatin: "Satin"
  of ekBevelEmboss: "Bevel & Emboss"

proc enabled*(e: Effect): bool =
  case e.kind
  of ekDropShadow, ekInnerShadow: e.shadow.common.enabled
  of ekOuterGlow, ekInnerGlow: e.glow.common.enabled
  of ekStroke: e.stroke.common.enabled
  of ekColorOverlay, ekGradientOverlay, ekPatternOverlay: e.common.enabled
  of ekSatin: e.satin.common.enabled
  of ekBevelEmboss: e.bevel.enabled

proc setEnabled*(e: var Effect; on: bool) =
  case e.kind
  of ekDropShadow, ekInnerShadow: e.shadow.common.enabled = on
  of ekOuterGlow, ekInnerGlow: e.glow.common.enabled = on
  of ekStroke: e.stroke.common.enabled = on
  of ekColorOverlay, ekGradientOverlay, ekPatternOverlay: e.common.enabled = on
  of ekSatin: e.satin.common.enabled = on
  of ekBevelEmboss: e.bevel.enabled = on

proc inMode*(e: Effect; mode: ColorMode): Effect =
  ## The effect with its colours in `mode`'s model.
  result = e
  template cc(c: var Color) = c = c.inMode(mode)
  template paint(p: var FxPaint) =
    case p.kind
    of fpColor: cc(p.color)
    of fpGradient:
      for s in p.gradient.stops.mitems: cc(s[1])
    of fpPattern: discard
  case result.kind
  of ekDropShadow, ekInnerShadow: cc(result.shadow.color)
  of ekOuterGlow, ekInnerGlow: paint(result.glow.paint)
  of ekStroke: paint(result.stroke.paint)
  of ekColorOverlay: cc(result.color)
  of ekGradientOverlay:
    for s in result.gradient.stops.mitems: cc(s[1])
  of ekSatin: cc(result.satin.color)
  of ekBevelEmboss:
    cc(result.bevel.highlightColor)
    cc(result.bevel.shadowColor)
  of ekPatternOverlay: discard

proc newEffect*(kind: EffectKind): Effect =
  ## Photoshop's defaults for each layer style.
  result = Effect(kind: kind, gradient: defaultGradient(), scale: 1, link: true)
  let contour = Contour(kind: ckLinear)
  case kind
  of ekDropShadow, ekInnerShadow:
    result.shadow = Shadow(common: fxCommon(bmMultiply, 0.75), color: Black, angle: 120, useGlobalLight: true,
      distance: 5, size: 5, contour: contour, knocksOut: kind == ekDropShadow)
  of ekOuterGlow, ekInnerGlow:
    result.glow = Glow(common: fxCommon(bmScreen, 0.75),
      paint: FxPaint(kind: fpColor, color: rgb(1, 1, 0.745), gradient: defaultGradient(), scale: 1),
      size: 5, contour: contour, range: 0.5, source: gsEdge)
  of ekStroke:
    result.stroke = StrokeFx(common: fxCommon(bmNormal, 1), size: 3, position: spOutside,
      paint: FxPaint(kind: fpColor, color: rgb(1, 0, 0), gradient: defaultGradient(), scale: 1))
  of ekColorOverlay:
    result.common = fxCommon(bmNormal, 1)
    result.color = rgb(1, 0, 0)
  of ekGradientOverlay: result.common = fxCommon(bmNormal, 1)
  of ekPatternOverlay: result.common = fxCommon(bmNormal, 1)
  of ekSatin:
    result.satin = Satin(common: fxCommon(bmMultiply, 0.5), color: Black, angle: 19, distance: 11, size: 14,
      contour: contour, antiAlias: true, invert: true)
  of ekBevelEmboss:
    result.bevel = Bevel(enabled: true, style: bsInnerBevel, technique: btSmooth, depth: 1, up: true, size: 5,
      angle: 120, altitude: 30, useGlobalLight: true, glossContour: contour,
      highlight: fxCommon(bmScreen, 0.75), highlightColor: White, shadow: fxCommon(bmMultiply, 0.75), shadowColor: Black)

proc defaultDropShadow*(): Effect = newEffect(ekDropShadow)
proc defaultOf*(_: typedesc[Effect]): Effect = newEffect(ekColorOverlay)

proc toJ*(e: Effect): JsonNode =
  case e.kind
  of ekDropShadow, ekInnerShadow: tagged($e.kind, toJ(e.shadow))
  of ekOuterGlow, ekInnerGlow: tagged($e.kind, toJ(e.glow))
  of ekStroke: tagged($e.kind, toJ(e.stroke))
  of ekColorOverlay: tagged($e.kind, %*{"common": toJ(e.common), "color": toJ(e.color)})
  of ekGradientOverlay: tagged($e.kind, %*{"common": toJ(e.common), "gradient": toJ(e.gradient), "dither": e.dither})
  of ekPatternOverlay:
    tagged($e.kind, %*{"common": toJ(e.common), "name": e.name, "id": e.id, "scale": toJ(e.scale),
                       "angle": toJ(e.angle), "link": e.link, "phase": toJ(e.phase)})
  of ekSatin: tagged($e.kind, toJ(e.satin))
  of ekBevelEmboss: tagged($e.kind, toJ(e.bevel))

proc fromJ*(n: JsonNode; e: var Effect) =
  let (tag, p) = untag(n)
  let k = enumFromStr(tag, EffectKind)
  if k.isNone or p.isNil: raise serdeError("unknown effect " & tag)
  e = newEffect(k.get)
  template f(key: string; dst: untyped) =
    if p.hasKey(key): fromJ(p[key], dst)
  case e.kind
  of ekDropShadow, ekInnerShadow: fromJ(p, e.shadow)
  of ekOuterGlow, ekInnerGlow: fromJ(p, e.glow)
  of ekStroke: fromJ(p, e.stroke)
  of ekColorOverlay:
    f("common", e.common); f("color", e.color)
  of ekGradientOverlay:
    f("common", e.common); f("gradient", e.gradient); f("dither", e.dither)
  of ekPatternOverlay:
    f("common", e.common); f("name", e.name); f("id", e.id); f("scale", e.scale)
    f("angle", e.angle); f("link", e.link); f("phase", e.phase)
  of ekSatin: fromJ(p, e.satin)
  of ekBevelEmboss: fromJ(p, e.bevel)

proc newEffects*(): Effects = Effects(enabled: true)

# ---------------------------------------------------------------- blending options

proc toPsd*(k: Knockout): uint8 = uint8(ord(k))
proc knockoutFromPsd*(v: uint8): Knockout =
  case v
  of 0: koNone
  of 1: koShallow
  else: koDeep
proc knockoutFromName*(s: string): Option[Knockout] =
  case s.toLowerAscii
  of "none": some(koNone)
  of "shallow": some(koShallow)
  of "deep": some(koDeep)
  else: none(Knockout)

proc defaultAdvancedBlending*(): AdvancedBlending =
  AdvancedBlending(knockout: koNone, blendClipped: true, transparencyShapes: true)
proc defaultOf*(_: typedesc[AdvancedBlending]): AdvancedBlending = defaultAdvancedBlending()
proc serdeCamel*(_: typedesc[AdvancedBlending]): bool = true
proc isDefault*(a: AdvancedBlending): bool = a == defaultAdvancedBlending()

const FullRange* = BlendRange(black: [0'u8, 0], white: [255'u8, 255])
proc defaultOf*(_: typedesc[BlendRange]): BlendRange = FullRange
proc isFull*(r: BlendRange): bool = r == FullRange
proc blendRangeFromBytes*(b: array[4, uint8]): BlendRange = BlendRange(black: [b[0], b[1]], white: [b[2], b[3]])
proc toBytes*(r: BlendRange): array[4, uint8] = [r.black[0], r.black[1], r.white[0], r.white[1]]
proc weight*(r: BlendRange; v: float32): float32 =
  proc ramp(lo, hi: uint8; x: float32): float32 =
    let (l, h) = (float32(lo), float32(hi))
    if h <= l: (if x >= l: 1'f32 else: 0'f32) else: clamp((x - l) / (h - l), 0'f32, 1'f32)
  ramp(r.black[0], r.black[1], v) * ramp(255'u8 - r.white[1], 255'u8 - r.white[0], 255 - v)
proc isDefault*(b: BlendIf): bool =
  for p in b.ranges:
    for r in p:
      if not r.isFull: return false
  true
proc get*(b: BlendIf; i: int): array[2, BlendRange] =
  if i >= 0 and i < b.ranges.len: b.ranges[i] else: [FullRange, FullRange]
proc set*(b: var BlendIf; i: int; pair: array[2, BlendRange]) =
  while b.ranges.len <= i: b.ranges.add [FullRange, FullRange]
  b.ranges[i] = pair
  while b.ranges.len > 0 and b.ranges[^1][0].isFull and b.ranges[^1][1].isFull: b.ranges.setLen(b.ranges.len - 1)

# ---------------------------------------------------------------- vectors

proc cornerKnot*(x, y: float64): Knot =
  let p = Point(x: x, y: y)
  Knot(anchor: p, inCtrl: p, outCtrl: p)
proc smoothKnot*(anchor, inCtrl, outCtrl: Point): Knot = Knot(anchor: anchor, inCtrl: inCtrl, outCtrl: outCtrl, smooth: true)
proc transform*(k: Knot; a: Affine): Knot =
  Knot(anchor: a.apply(k.anchor), inCtrl: a.apply(k.inCtrl), outCtrl: a.apply(k.outCtrl), smooth: k.smooth)

proc polygon*(pts: openArray[(float64, float64)]): Subpath =
  result.closed = true
  for (x, y) in pts: result.knots.add cornerKnot(x, y)
proc polyline*(pts: openArray[(float64, float64)]): Subpath =
  result = polygon(pts)
  result.closed = false
proc withOp*(s: Subpath; op: PathOp): Subpath =
  result = s
  result.op = op
proc segments*(s: Subpath): seq[array[4, Point]] =
  let n = s.knots.len
  if n < 2: return
  let count = if s.closed: n else: n - 1
  for i in 0 ..< count:
    let a = s.knots[i]
    let b = s.knots[(i + 1) mod n]
    result.add [a.anchor, a.outCtrl, b.inCtrl, b.anchor]

proc newPath*(subpaths: seq[Subpath]): Path = Path(subpaths: subpaths)
proc isEmpty*(p: Path): bool =
  for s in p.subpaths:
    if s.knots.len > 0: return false
  true
proc effectiveOp*(p: Path; i: int): PathOp = (if i == 0: poCombine else: p.subpaths[i].op)
proc components*(p: Path): seq[HSlice[int, int]] =
  for i, s in p.subpaths:
    if result.len > 0 and s.op == poJoin: result[^1].b = i
    else: result.add i .. i
proc transform*(p: Path; a: Affine): Path =
  result = p
  for s in result.subpaths.mitems:
    for k in s.knots.mitems: k = k.transform(a)
proc controlBounds*(p: Path): Option[(float64, float64, float64, float64)] =
  var first = true
  var b: (float64, float64, float64, float64)
  for s in p.subpaths:
    for k in s.knots:
      for q in [k.anchor, k.inCtrl, k.outCtrl]:
        if first:
          b = (q.x, q.y, q.x, q.y)
          first = false
        else: b = (min(b[0], q.x), min(b[1], q.y), max(b[2], q.x), max(b[3], q.y))
  if first: none((float64, float64, float64, float64)) else: some(b)

proc newVectorMask*(path: Path): VectorMask = VectorMask(path: path, enabled: true, linked: true, density: 1)
proc defaultOf*(_: typedesc[VectorMask]): VectorMask = newVectorMask(Path())

proc toJ*(s: LiveShape): JsonNode =
  result = %*{"kind": $s.kind}
  case s.kind
  of lsRect:
    result["rect"] = toJ(s.rect); result["radii"] = toJ(s.radii)
  of lsEllipse: result["rect"] = toJ(s.rect)
  of lsPolygon:
    result["rect"] = toJ(s.rect); result["sides"] = toJ(s.sides); result["starRatio"] = toJ(s.starRatio)
  of lsLine:
    result["from"] = toJ(s.`from`); result["to"] = toJ(s.to); result["weight"] = toJ(s.weight)

proc fromJ*(n: JsonNode; s: var LiveShape) =
  if n.kind != JObject or not n.hasKey("kind"): raise serdeError("live shape needs a kind")
  s = LiveShape()
  fromJ(n["kind"], s.kind)
  template f(key: string; dst: untyped) =
    if n.hasKey(key): fromJ(n[key], dst)
  f("rect", s.rect); f("radii", s.radii); f("sides", s.sides); f("starRatio", s.starRatio)
  f("from", s.`from`); f("to", s.to); f("weight", s.weight)

# ---------------------------------------------------------------- fills

proc solidFill*(c: Color): Fill = Fill(kind: fkSolid, color: c, scale: 1, align: true, link: true)
proc gradientFill*(stops: seq[(float32, Color)]; angle, scale: float32; style: GradientStyle; reverse: bool): Fill =
  Fill(kind: fkGradient, stops: stops, angle: angle, scale: scale, style: style, reverse: reverse, align: true, link: true)
proc patternFill*(name, id: string; scale: float32): Fill =
  Fill(kind: fkPattern, name: name, id: id, scale: scale, align: true, link: true)
proc defaultOf*(_: typedesc[Fill]): Fill = solidFill(Black)

proc inMode*(f: Fill; mode: ColorMode): Fill =
  result = f
  case result.kind
  of fkSolid: result.color = result.color.inMode(mode)
  of fkGradient:
    for s in result.stops.mitems: s[1] = s[1].inMode(mode)
  of fkPattern: discard

proc toJ*(f: Fill): JsonNode =
  case f.kind
  of fkSolid: tagged("Solid", toJ(f.color))
  of fkGradient:
    tagged("Gradient", %*{"stops": toJ(f.stops), "angle": toJ(f.angle), "scale": toJ(f.scale), "style": $f.style,
      "reverse": f.reverse, "opacity_stops": toJ(f.opacityStops), "midpoints": toJ(f.midpoints),
      "offset": toJ(f.offset), "dither": f.dither, "align": f.align})
  of fkPattern:
    tagged("Pattern", %*{"name": f.name, "scale": toJ(f.scale), "id": f.id, "angle": toJ(f.angle),
      "link": f.link, "phase": toJ(f.phase)})

proc fromJ*(n: JsonNode; f: var Fill) =
  let (tag, p) = untag(n)
  f = Fill(scale: 1, align: true, link: true)
  if p.isNil: raise serdeError("fill " & tag & " needs data")
  template g(key: string; dst: untyped) =
    if p.hasKey(key): fromJ(p[key], dst)
  case tag
  of "Solid":
    f.kind = fkSolid
    fromJ(p, f.color)
  of "Gradient":
    f.kind = fkGradient
    g("stops", f.stops); g("angle", f.angle); g("scale", f.scale); g("style", f.style); g("reverse", f.reverse)
    g("opacity_stops", f.opacityStops); g("midpoints", f.midpoints); g("offset", f.offset); g("dither", f.dither); g("align", f.align)
  of "Pattern":
    f.kind = fkPattern
    g("name", f.name); g("scale", f.scale); g("id", f.id); g("angle", f.angle); g("link", f.link); g("phase", f.phase)
  else: raise serdeError("unknown fill " & tag)

proc defaultShapeStroke*(): ShapeStroke =
  ShapeStroke(width: 3, paint: solidFill(Black), opacity: 1, align: saCenter, cap: lcapButt, join: ljMiter, miterLimit: 100)
proc defaultOf*(_: typedesc[ShapeStroke]): ShapeStroke = defaultShapeStroke()

proc validate*(s: ShapeStroke): Option[string] =
  ## `none` when valid, else the reason.
  if not s.width.finite or s.width < 0 or s.width > 1_000_000: return some("stroke width must be finite and between 0 and 1000000 px")
  if not s.opacity.finite or s.opacity < 0 or s.opacity > 1: return some("stroke opacity must be between 0 and 100%")
  if not s.miterLimit.finite or s.miterLimit < 1 or s.miterLimit > 500: return some("miter limit must be between 1 and 500")
  if not s.dashOffset.finite or abs(s.dashOffset) > 1_000_000: return some("dash offset must be finite and within ±1000000 widths")
  var total = 0'f32
  for d in s.dashes:
    if not d.finite or d < 0 or d > 10000: return some("dash pattern must contain at most 32 finite lengths between 0 and 10000 widths")
    total += d
  if s.dashes.len > 32: return some("dash pattern must contain at most 32 finite lengths between 0 and 10000 widths")
  if s.dashes.len > 0 and total < 0.01'f32: return some("dash pattern total must be at least 0.01 widths")
  none(string)

# ---------------------------------------------------------------- text

proc defaultCharStyle*(): CharStyle =
  CharStyle(weight: 400, sizePt: 12, color: Black, horizontalScale: 1, verticalScale: 1, kerning: kMetrics, caps: capsNormal, ligatures: true)
proc defaultOf*(_: typedesc[CharStyle]): CharStyle = defaultCharStyle()
proc defaultParagraphStyle*(): ParagraphStyle = ParagraphStyle(align: taLeft, autoLeading: 1.2, direction: tdAuto)
proc defaultOf*(_: typedesc[ParagraphStyle]): ParagraphStyle = defaultParagraphStyle()
proc isJustified*(a: TextAlign): bool = a in {taJustifyLeft, taJustifyCenter, taJustifyRight, taJustifyAll}

proc toJ*(s: CharStyle): JsonNode =
  result = newJObject()
  for name, v in s.fieldPairs:
    when name == "styleSheet":
      if v.isSome: result["style_sheet"] = toJ(v)
    else: result[snakeCase(name)] = toJ(v)
proc toJ*(s: ParagraphStyle): JsonNode =
  result = newJObject()
  for name, v in s.fieldPairs:
    when name == "styleSheet":
      if v.isSome: result["style_sheet"] = toJ(v)
    else: result[snakeCase(name)] = toJ(v)

proc toJ*(s: TextShape): JsonNode =
  case s.kind
  of tsPoint: newJString("Point")
  of tsBox: tagged("Box", %*{"x": toJ(s.x), "y": toJ(s.y), "width": toJ(s.width), "height": toJ(s.height)})
proc fromJ*(n: JsonNode; s: var TextShape) =
  let (tag, p) = untag(n)
  s = TextShape()
  case tag
  of "Point": s.kind = tsPoint
  of "Box":
    s.kind = tsBox
    if p != nil:
      for (k, dst) in [("x", addr s.x), ("y", addr s.y), ("width", addr s.width), ("height", addr s.height)]:
        if p.hasKey(k): fromJ(p[k], dst[])
  else: raise serdeError("unknown text shape " & tag)

proc defaultTextLayer*(): TextLayer =
  TextLayer(sizePt: 12, color: Black, transform: IdentityAffine, shape: TextShape(kind: tsPoint),
            orientation: orHorizontal, antialias: aaSmooth)
proc defaultOf*(_: typedesc[TextLayer]): TextLayer = defaultTextLayer()

proc isCharBoundary(s: string; i: int): bool =
  i <= 0 or i >= s.len or (byte(s[i]) and 0xC0'u8) != 0x80'u8

proc normalizeRuns[S](text: string; runs: seq[(int, S)]; base: S): seq[(int, S)] =
  ## Stretches/truncates runs to cover `text` exactly on char boundaries (one run for empty text).
  let total = text.len
  if total == 0:
    return @[(0, if runs.len > 0: runs[0][1] else: base)]
  var at = 0
  for (ln, style) in runs:
    if at >= total: break
    var e = min(at + max(ln, 0), total)
    while not isCharBoundary(text, e): inc e
    if e > at:
      result.add (e - at, style)
      at = e
  if at < total:
    if result.len > 0: result[^1][0] += total - at
    else: result.add (total, base)
  if result.len == 0: result.add (0, base)

proc charRuns*(t: TextLayer): seq[TextRun] =
  var base = defaultCharStyle()
  base.fontFamily = t.fontFamily
  base.sizePt = t.sizePt
  base.color = t.color
  var rs: seq[(int, CharStyle)]
  for r in t.runs: rs.add (r.len, r.style)
  for (ln, st) in normalizeRuns(t.text, rs, base): result.add TextRun(len: ln, style: st)

proc paragraphRuns*(t: TextLayer): seq[ParagraphRun] =
  var rs: seq[(int, ParagraphStyle)]
  for r in t.paragraphs: rs.add (r.len, r.style)
  var runs = normalizeRuns(t.text, rs, defaultParagraphStyle())
  if t.text.len > 0 and t.text[^1] in {'\r', '\n'} and t.paragraphs.len > 0 and t.paragraphs[^1].len <= 1:
    var sum = 0
    for r in t.paragraphs[0 ..< ^1]: sum += r.len
    if sum == t.text.len: runs.add (0, t.paragraphs[^1].style)
  for (ln, st) in runs: result.add ParagraphRun(len: ln, style: st)

proc syncSummary*(t: var TextLayer) =
  if t.runs.len > 0:
    t.fontFamily = t.runs[0].style.fontFamily
    t.sizePt = t.runs[0].style.sizePt
    t.color = t.runs[0].style.color

# ---------------------------------------------------------------- text styles

const
  BasicParagraph* = "Basic Paragraph"
  NoCharacterStyle* = "None"

proc defaultTextStyles*(): TextStyles =
  TextStyles(basic: ParagraphStyleDef(id: 0, name: BasicParagraph, paraAttrs: newJObject(), charAttrs: newJObject()))
proc defaultOf*(_: typedesc[TextStyles]): TextStyles = defaultTextStyles()

proc attrsObject[T](s: T): JsonNode =
  result = toJ(s)
  if result.kind == JObject:
    if result.hasKey("style_sheet"): result.delete("style_sheet")
  else: result = newJObject()

proc applyAttrs*[T](base: T; attrs: JsonNode): T =
  ## `base` with `attrs` (a JSON object of field overrides) applied. Raises `SerdeError` on an
  ## unknown attribute or a bad value.
  result = base
  if attrs.isNil or attrs.kind != JObject or attrs.len == 0: return
  let known = toJ(base)
  for k, v in attrs.fields:
    if k == "style_sheet": continue
    if not known.hasKey(k) and not (k in ["leading_pt", "postscript_name", "language"]):
      raise serdeError("unknown style attribute " & escape(k))
  var merged = known
  for k, v in attrs.fields:
    if k != "style_sheet": merged[k] = v
  fromJ(merged, result)

proc diffAttrs*[T](style, base: T): JsonNode =
  let (a, b) = (attrsObject(style), attrsObject(base))
  result = newJObject()
  for k, v in a.fields:
    if not b.hasKey(k) or b[k] != v: result[k] = v

proc isEmpty*(s: TextStyles): bool =
  s.character.len == 0 and s.paragraph.len == 0 and (s.basic.charAttrs.isNil or s.basic.charAttrs.len == 0) and
    (s.basic.paraAttrs.isNil or s.basic.paraAttrs.len == 0)

proc charStyleIndex*(s: TextStyles; id: uint32): int =
  for i, d in s.character:
    if d.id == id: return i
  -1
proc paraStyle*(s: TextStyles; id: uint32): Option[ParagraphStyleDef] =
  if id == 0: return some(s.basic)
  for d in s.paragraph:
    if d.id == id: return some(d)

proc nextCharId*(s: TextStyles): Option[uint32] =
  var m = 0'u32
  for d in s.character: m = max(m, d.id)
  if m == high(uint32): none(uint32) else: some(m + 1)
proc nextParaId*(s: TextStyles): Option[uint32] =
  var m = 0'u32
  for d in s.paragraph: m = max(m, d.id)
  if m == high(uint32): none(uint32) else: some(m + 1)

proc uniqueName*(s: TextStyles; paragraph: bool; base: string): string =
  var i = 1
  while true:
    let n = &"{base} {i}"
    var taken = if paragraph: n == BasicParagraph else: n == NoCharacterStyle
    if paragraph:
      for d in s.paragraph:
        if d.name == n: taken = true
    else:
      for d in s.character:
        if d.name == n: taken = true
    if not taken: return n
    inc i

proc resolvePara*(s: TextStyles; para: Option[uint32]): ParagraphStyle =
  result = try: applyAttrs(defaultParagraphStyle(), s.basic.paraAttrs) except SerdeError: defaultParagraphStyle()
  if para.isSome:
    let d = s.paraStyle(para.get)
    if d.isSome:
      try: result = applyAttrs(result, d.get.paraAttrs) except SerdeError: discard
  result.styleSheet = if para.isSome and para.get != 0 and s.paraStyle(para.get).isSome: para else: none(uint32)

proc resolveChar*(s: TextStyles; para, chr: Option[uint32]; defaultFamily: string): CharStyle =
  var base = defaultCharStyle()
  base.fontFamily = defaultFamily
  result = try: applyAttrs(base, s.basic.charAttrs) except SerdeError: base
  if para.isSome and para.get != 0:
    let d = s.paraStyle(para.get)
    if d.isSome:
      try: result = applyAttrs(result, d.get.charAttrs) except SerdeError: discard
  var c = none(uint32)
  if chr.isSome and s.charStyleIndex(chr.get) >= 0:
    c = chr
    try: result = applyAttrs(result, s.character[s.charStyleIndex(chr.get)].attrs) except SerdeError: discard
  result.styleSheet = c

proc charOverrides*(s: TextStyles; run: CharStyle; para: Option[uint32]; defaultFamily: string): JsonNode =
  diffAttrs(run, s.resolveChar(para, run.styleSheet, defaultFamily))
proc paraOverrides*(s: TextStyles; p: ParagraphStyle): JsonNode = diffAttrs(p, s.resolvePara(p.styleSheet))

proc restyleChar*(s, old: TextStyles; run: CharStyle; oldPara, newPara, newChr: Option[uint32]; family: string): CharStyle =
  let overrides = old.charOverrides(run, oldPara, family)
  let base = s.resolveChar(newPara, newChr, family)
  try: applyAttrs(base, overrides) except SerdeError: base

proc restylePara*(s, old: TextStyles; p: ParagraphStyle; newPara: Option[uint32]): ParagraphStyle =
  let overrides = old.paraOverrides(p)
  let base = s.resolvePara(newPara)
  result = try: applyAttrs(base, overrides) except SerdeError: base
  result.styleSheet = if newPara.isSome and newPara.get != 0 and s.paraStyle(newPara.get).isSome: newPara else: none(uint32)

# ---------------------------------------------------------------- masks, smart objects, video

proc revealAllMask*(): LayerMask =
  LayerMask(surface: surfaceWithDefault(Gray8, [1.0'f32]), enabled: true, linked: true, density: 1)
proc hideAllMask*(): LayerMask =
  LayerMask(surface: surfaceWithDefault(Gray8, [0.0'f32]), enabled: true, linked: true, density: 1)

proc value*(m: LayerMask; x, y: int): float32 =
  ## Effective mask value at a pixel, including density.
  if not m.enabled: return 1
  1 - m.density * (1 - m.surface.sampleChannel(x, y, 0))

proc valuesInto*(m: LayerMask; r: Rect; o: var seq[float32]) =
  let n = r.width * r.height
  if not m.enabled:
    o.setLen(n)
    for v in o.mitems: v = 1
    return
  m.surface.readRegionInto(r, o)
  let ch = m.surface.channels
  if ch > 1:
    for i in 0 ..< n: o[i] = o[i * ch]
    o.setLen(n)
  if m.density < 1:
    for v in o.mitems: v = 1 - m.density * (1 - v)

proc newSmartObject*(source: SmartSource; transform: Affine; cache: Option[Surface]): SmartObject =
  SmartObject(contentsId: freshContentsId(), source: source, transform: transform, cache: cache, filtersEnabled: true)

proc toJ*(v: VideoSource): JsonNode =
  case v.kind
  of vsBlank: newJString("Blank")
  of vsFile: tagged("File", %*{"path": v.path})
proc fromJ*(n: JsonNode; v: var VideoSource) =
  let (tag, p) = untag(n)
  case tag
  of "Blank": v = VideoSource(kind: vsBlank)
  of "File":
    v = VideoSource(kind: vsFile)
    if p != nil and p.hasKey("path"): fromJ(p["path"], v.path)
  else: raise serdeError("unknown video source " & tag)

proc newVideoData*(frames: seq[Surface]; fps: float32): VideoData =
  VideoData(frames: frames, source: VideoSource(kind: vsBlank), fps: fps, showAltered: true)

proc `==`*(a, b: VideoData): bool =
  a.frames.len == b.frames.len and a.originalFrames.isSome == b.originalFrames.isSome and
    (a.originalFrames.isNone or a.originalFrames.get.len == b.originalFrames.get.len) and
    a.source == b.source and a.fps == b.fps and a.showAltered == b.showAltered

proc validFps(fps: float32): float32 = (if fps.finite and fps > 0: fps else: 30)
proc newTimeline*(duration: int; fps: float32): Timeline =
  let d = max(duration, 1)
  Timeline(fps: validFps(fps), duration: d, workEnd: d)
proc clamp*(t: var Timeline) =
  t.duration = max(t.duration, 1)
  t.fps = validFps(t.fps)
  t.current = min(max(t.current, 0), t.duration - 1)
  t.workStart = min(max(t.workStart, 0), t.duration - 1)
  t.workEnd = clamp(t.workEnd, t.workStart + 1, t.duration)
proc step*(t: var Timeline; delta: int) =
  t.current = clamp(t.current + delta, 0, max(t.duration - 1, 0))
proc time*(t: Timeline): float32 = float32(t.current) / max(t.fps, 1e-3)

# ---------------------------------------------------------------- comps, artboards, variables

proc toJ*(b: ArtboardBackground): JsonNode =
  if b.kind == abCustom: tagged("custom", toJ(b.color)) else: newJString($b.kind)
proc fromJ*(n: JsonNode; b: var ArtboardBackground) =
  let (tag, p) = untag(n)
  let k = enumFromStr(tag, ArtboardBackgroundKind)
  if k.isNone: raise serdeError("unknown artboard background " & tag)
  b = ArtboardBackground(kind: k.get, color: White)
  if b.kind == abCustom and p != nil: fromJ(p, b.color)

proc rgba*(b: ArtboardBackground): Option[array[4, float32]] =
  case b.kind
  of abWhite: some([1'f32, 1, 1, 1])
  of abBlack: some([0'f32, 0, 0, 1])
  of abTransparent: none(array[4, float32])
  of abCustom:
    let c = b.color.toRgb
    if b.color.alpha > 0: some([c[0], c[1], c[2], b.color.alpha]) else: none(array[4, float32])
proc psdType*(b: ArtboardBackground): int32 = int32(ord(b.kind) + 1)
proc newArtboard*(r: Rect): Artboard = Artboard(rect: r, background: ArtboardBackground(kind: abWhite, color: White))

proc capturedInfo*(c: LayerComp): int32 =
  int32(c.applyVisibility) or (int32(c.applyPosition) shl 1) or (int32(c.applyAppearance) shl 2)
proc setCapturedInfo*(c: var LayerComp; v: int32) =
  c.applyVisibility = (v and 1) != 0
  c.applyPosition = (v and 2) != 0
  c.applyAppearance = (v and 4) != 0
proc state*(c: LayerComp; id: LayerId): Option[CompLayerState] =
  for s in c.states:
    if s.layer == id: return some(s)

proc toJ*(k: VarKind): JsonNode =
  result = %*{"type": $k.kind}
  if k.kind == vkPixelReplacement:
    result["method"] = toJ(k.`method`); result["align"] = toJ(k.align); result["clip"] = toJ(k.clip)
proc fromJ*(n: JsonNode; k: var VarKind) =
  if n.kind != JObject or not n.hasKey("type"): raise serdeError("variable kind needs a type")
  k = VarKind(align: paCenter)
  fromJ(n["type"], k.kind)
  if n.hasKey("method"): fromJ(n["method"], k.`method`)
  if n.hasKey("align"): fromJ(n["align"], k.align)
  if n.hasKey("clip"): fromJ(n["clip"], k.clip)

proc toJ*(v: DataValue): JsonNode =
  result = %*{"variable": v.variable, "kind": $v.kind}
  result["value"] = if v.kind == dvVisibility: newJBool(v.visible) else: newJString(v.text)
proc fromJ*(n: JsonNode; v: var DataValue) =
  if n.kind != JObject: raise serdeError("data value must be an object")
  v = DataValue()
  if n.hasKey("variable"): fromJ(n["variable"], v.variable)
  if n.hasKey("kind"): fromJ(n["kind"], v.kind)
  if n.hasKey("value"):
    if v.kind == dvVisibility: fromJ(n["value"], v.visible) else: fromJ(n["value"], v.text)

proc isEmpty*(v: Variables): bool = v.defs.len == 0 and v.dataSets.len == 0
proc defIndex*(v: Variables; name: string): int =
  for i, d in v.defs:
    if d.name == name: return i
  -1
proc dataSetIndex*(v: Variables; name: string): int =
  for i, d in v.dataSets:
    if d.name == name: return i
  -1

# ---------------------------------------------------------------- analysis, modes, patterns, slices

proc defaultMeasurementScale*(): MeasurementScale = MeasurementScale(pixelLength: 1, logicalLength: 1, units: "pixels")
proc defaultOf*(_: typedesc[MeasurementScale]): MeasurementScale = defaultMeasurementScale()
proc factor*(s: MeasurementScale): float64 = (if s.pixelLength > 0: s.logicalLength / s.pixelLength else: 1)
proc isDefault*(s: MeasurementScale): bool = s == defaultMeasurementScale()
proc describe*(s: MeasurementScale): string =
  let p = if s.pixelLength == trunc(s.pixelLength): $int64(s.pixelLength) else: $s.pixelLength
  &"{p} pixels = {s.logicalLength:.4f} {s.units}"
proc defaultCountGroup*(): CountGroup =
  CountGroup(name: "Count Group 1", color: rgb(1, 0, 0), markerSize: 1, labelSize: 8, visible: true)
proc defaultOf*(_: typedesc[CountGroup]): CountGroup = defaultCountGroup()
proc defaultMeasurement*(): Measurement = Measurement(scale: defaultMeasurementScale())
proc defaultOf*(_: typedesc[Measurement]): Measurement = defaultMeasurement()
proc isEmpty*(m: Measurement): bool = m == defaultMeasurement()
proc countTotal*(m: Measurement): int =
  for g in m.countGroups: result += g.points.len
proc defaultNote*(): Note = Note(color: rgb(1, 1, 0.51))
proc defaultOf*(_: typedesc[Note]): Note = defaultNote()

proc nearest*(t: ColorTable; c: array[3, float32]): int =
  var best = Inf
  for i, e in t.colors:
    var d = 0'f64
    for k in 0..2: d += pow(float64(c[k]) - float64(e[k]) / 255, 2)
    if d < best:
      best = d
      result = i
proc rgb*(t: ColorTable; i: int): array[3, float32] =
  if i >= 0 and i < t.colors.len:
    for k in 0..2: result[k] = float32(t.colors[i][k]) / 255

proc newDuotoneInk*(name: string; color: array[3, float32]): DuotoneInk =
  DuotoneInk(name: name, color: color, curve: identityLine())

proc curveEval(pts: openArray[CurvePoint]; x: float32): float32 =
  if pts.len == 0: return x
  if pts.len == 1: return pts[0].output
  if x <= pts[0].input: return pts[0].output
  for i in 0 ..< pts.len - 1:
    if x <= pts[i + 1].input:
      let span = max(pts[i + 1].input - pts[i].input, 1e-6'f32)
      return pts[i].output + (pts[i + 1].output - pts[i].output) * (x - pts[i].input) / span
  pts[^1].output

proc density*(ink: DuotoneInk; grayDensity: float32): float32 = clamp(curveEval(ink.curve, clamp(grayDensity, 0'f32, 1'f32)), 0'f32, 1'f32)
proc render*(d: Duotone; gray: float32): array[3, float32] =
  let density = 1 - clamp(gray, 0'f32, 1'f32)
  result = [1'f32, 1, 1]
  for ink in d.inks:
    let dd = curveEval(ink.curve, density)
    for k in 0..2: result[k] *= 1 - dd * (1 - ink.color[k])
proc displayAdjustment*(d: Duotone): Adjustment =
  result = newAdjustment(akGradientMap)
  result.gmStops = @[]
  for i in 0..32:
    let t = float32(i) / 32
    result.gmStops.add (t, d.render(t))

proc label*(m: StackMode): string =
  case m
  of smStandardDeviation: "Standard Deviation"
  else: capitalizeAscii($m)

proc fnv(h: uint64; bytes: openArray[byte]): uint64 =
  result = h
  for b in bytes: result = (result xor uint64(b)) * 0x100000001b3'u64

proc newPattern*(name: string; surface: Surface; width, height: int): Pattern =
  ## A pattern with a content-derived id (stable across sessions).
  let px = surface.readRegion(rect(0, 0, width, height))
  var h1 = fnv(0xcbf29ce484222325'u64, name.toOpenArrayByte(0, name.len - 1))
  var le4: array[4, byte]
  for v in [uint32(width), uint32(height)]:
    for i in 0..3: le4[i] = byte((v shr (8 * i)) and 0xFF)
    h1 = fnv(h1, le4)
  for v in px:
    let bits = cast[uint32](v)
    for i in 0..3: le4[i] = byte((bits shr (8 * i)) and 0xFF)
    h1 = fnv(h1, le4)
  let fmt = $surface.format
  let h2 = fnv(h1 xor 0x9e3779b97f4a7c15'u64, fmt.toOpenArrayByte(0, fmt.len - 1))
  let id = &"{uint32(h1 shr 32):08x}-{uint16((h1 shr 16) and 0xFFFF):04x}-{uint16(h1 and 0xFFFF):04x}-{uint16(h2 shr 48):04x}-{h2 and 0xffffffffffff'u64:012x}"
  Pattern(id: id, name: name, width: width, height: height, surface: surface)

proc patternDisplayName*(name: string): string =
  var n = name
  while n.len > 0 and n[^1] == '\0': n.setLen(n.len - 1)
  if n.startsWith("$$$/"):
    let eq = n.find('=')
    if eq >= 0: return n[eq + 1 .. ^1]
  n
proc displayName*(p: Pattern): string = patternDisplayName(p.name)
proc rect*(p: Pattern): Rect = rect(0, 0, p.width, p.height)
proc isEmpty*(p: Pattern): bool = p.width == 0 or p.height == 0
proc findPattern*(patterns: openArray[Pattern]; id, name: string): int =
  ## Index of the pattern with `id`, else with `name` (also by display name). -1 when none.
  if id.len > 0:
    for i, p in patterns:
      if p.id == id: return i
  if name.len == 0: return -1
  for i, p in patterns:
    if p.name == name: return i
  let dn = patternDisplayName(name).toLowerAscii
  for i, p in patterns:
    if p.displayName.toLowerAscii == dn: return i
  -1

proc serdeCamel*(_: typedesc[WebSlice]): bool = true
proc serdeCamel*(_: typedesc[Slices]): bool = true
proc code*(o: SliceOrigin): uint32 = uint32(ord(o))
proc sliceOriginFromCode*(c: uint32): SliceOrigin =
  case c
  of 0: soAuto
  of 1: soLayer
  else: soUser
proc code*(k: SliceKind): uint32 =
  case k
  of skNoImage: 0
  of skImage: 1
  of skTable: 2
proc sliceKindFromCode*(c: uint32): SliceKind =
  case c
  of 0: skNoImage
  of 2: skTable
  else: skImage
proc sliceKindFromId*(s: string): Option[SliceKind] =
  case s
  of "image", "img": some(skImage)
  of "noImage", "none", "no-image": some(skNoImage)
  of "table": some(skTable)
  else: none(SliceKind)
proc isEmpty*(s: Slices): bool = s.list.len == 0
proc nextId*(s: Slices): Option[uint32] =
  var m = 0'u32
  for x in s.list: m = max(m, x.id)
  if m == high(uint32): none(uint32) else: some(m + 1)
proc sliceIndex*(s: Slices; id: uint32): int =
  for i, x in s.list:
    if x.id == id: return i
  -1

proc autoSlices*(canvas: Rect; taken: openArray[Rect]): seq[Rect] =
  ## Auto slices: the canvas minus the user/layer slices, as row runs merged vertically.
  if canvas.isEmpty: return
  var xs = @[canvas.x0, canvas.x1]
  var ys = @[canvas.y0, canvas.y1]
  for r0 in taken:
    let r = r0.intersect(canvas)
    if r.isEmpty: continue
    xs.add [r.x0, r.x1]
    ys.add [r.y0, r.y1]
  xs.sort(); xs = deduplicate(xs, isSorted = true)
  ys.sort(); ys = deduplicate(ys, isSorted = true)
  template covered(px, py: int): bool =
    var hit = false
    for r in taken:
      if px >= r.x0 and px < r.x1 and py >= r.y0 and py < r.y1:
        hit = true
        break
    hit
  var runs: seq[Rect]
  for yi in 0 ..< ys.len - 1:
    var start = -1
    var has = false
    for xi in 0 ..< xs.len - 1:
      let free = not covered(xs[xi], ys[yi])
      if free and not has:
        start = xs[xi]
        has = true
      elif not free and has:
        runs.add rect(start, ys[yi], xs[xi], ys[yi + 1])
        has = false
    if has: runs.add rect(start, ys[yi], xs[^1], ys[yi + 1])
  for r in runs:
    var merged = false
    for p in result.mitems:
      if p.x0 == r.x0 and p.x1 == r.x1 and p.y1 == r.y0:
        p.y1 = r.y1
        merged = true
        break
    if not merged: result.add r

# ---------------------------------------------------------------- layers

proc kindName*(c: LayerContent): string = $c.kind
proc article*(c: LayerContent): string = (if c.kind == lkAdjustment: "an" else: "a")

proc rasterContent*(s: Surface): LayerContent = LayerContent(kind: lkRaster, raster: s)
proc groupContent*(children: seq[Layer]): LayerContent =
  LayerContent(kind: lkGroup, group: Group(children: children, expanded: true))
proc adjustmentContent*(a: Adjustment): LayerContent = LayerContent(kind: lkAdjustment, adjustment: a)
proc fillContent*(f: Fill): LayerContent = LayerContent(kind: lkFill, fill: f)
proc textContent*(t: TextLayer): LayerContent = LayerContent(kind: lkText, text: t)
proc shapeContent*(s: ShapeLayer): LayerContent = LayerContent(kind: lkShape, shape: s)
proc smartContent*(s: SmartObject): LayerContent = LayerContent(kind: lkSmart, smart: s)

proc newLayer*(name: string; content: LayerContent): Layer =
  Layer(id: freshLayerId(), name: name, visible: true, blend: bmNormal, opacity: 1, fillOpacity: 1,
        effects: newEffects(), label: lcNone, content: content, advanced: defaultAdvancedBlending())

proc rasterLayer*(name: string; format: PixelFormat): Layer = newLayer(name, rasterContent(newSurface(format)))
proc groupLayer*(name: string; children: seq[Layer]): Layer =
  result = newLayer(name, groupContent(children))
  result.blend = bmPassThrough

proc isGroup*(l: Layer): bool = l.content.kind == lkGroup
proc hasSurface*(l: Layer): bool =
  case l.content.kind
  of lkRaster: true
  of lkText: l.content.text.cache.isSome
  of lkShape: l.content.shape.cache.isSome
  of lkSmart: l.content.smart.cache.isSome
  else: false

proc surface*(l: Layer): Surface =
  ## The layer's pixels (raster content or the rendered cache); an empty surface when none.
  case l.content.kind
  of lkRaster: l.content.raster
  of lkText: l.content.text.cache.get(Surface())
  of lkShape: l.content.shape.cache.get(Surface())
  of lkSmart: l.content.smart.cache.get(Surface())
  else: Surface()

proc artboard*(l: Layer): Option[Artboard] =
  if l.content.kind == lkGroup: l.content.group.artboard else: none(Artboard)

proc reassignIds(l: var Layer) =
  l.id = freshLayerId()
  l.psdId = none(uint32)
  if l.content.kind == lkGroup:
    for c in l.content.group.children.mitems: reassignIds(c)

proc duplicate*(l: Layer): Layer =
  ## Deep copy with fresh ids (Duplicate Layer).
  result = l
  reassignIds(result)

# ---------------------------------------------------------------- guides, channels

proc guideColor*(g: Guides; vertical: bool; position: float64): Option[array[3, uint8]] =
  for c in g.colors:
    if c.vertical == vertical and c.position == position: return some(c.rgb)
proc setGuideColor*(g: var Guides; vertical: bool; position: float64; rgb: Option[array[3, uint8]]) =
  g.colors.keepItIf(not (it.vertical == vertical and it.position == position))
  if rgb.isSome: g.colors.add GuideColor(vertical: vertical, position: position, rgb: rgb.get)
proc pruneColors*(g: var Guides) =
  let (h, v) = (g.horizontal, g.vertical)
  g.colors.keepItIf(if it.vertical: it.position in v else: it.position in h)
proc toJ*(g: Guides): JsonNode =
  ## `colors` is left out when empty (serde `skip_serializing_if`).
  result = %*{"horizontal": toJ(g.horizontal), "vertical": toJ(g.vertical)}
  if g.colors.len > 0: result["colors"] = toJ(g.colors)

const AlphaDefaultColor* = Color(mode: cmRgb, c: [1.0'f32, 0, 0, 0], alpha: 1.0)
proc newAlphaChannel*(name: string; surface: Surface): AlphaChannel =
  AlphaChannel(name: name, surface: surface, color: AlphaDefaultColor, opacity: 0.5, indicates: ciMaskedAreas)
proc overlayCoverage*(c: AlphaChannel; v: float32): float32 =
  if c.spot.isSome or c.indicates == ciSelectedAreas: v else: 1 - v

# ---------------------------------------------------------------- document

proc newDocument*(name: string; size: Size; mode: ColorMode; depth: SampleType): Document =
  Document(id: freshDocId(), name: name, size: size, resolutionDpi: 72, mode: mode, depth: depth,
           globalLight: defaultGlobalLight(), measurement: defaultMeasurement(), textStyles: defaultTextStyles())

proc pixelFormat*(d: Document): PixelFormat =
  ## Pixel format for new raster layers in this document.
  let mode = case d.mode
    of cmIndexed, cmMultichannel: cmRgb
    of cmBitmap, cmDuotone: cmGrayscale
    else: d.mode
  PixelFormat(mode: mode, sample: d.depth, alpha: true)

proc bounds*(d: Document): Rect = rect(0, 0, d.size.width, d.size.height)

proc withBackground*(name: string; size: Size; mode: ColorMode; depth: SampleType; fill: Color): Document =
  ## A new document with a filled "Background" layer (File › New).
  result = newDocument(name, size, mode, depth)
  var bg = rasterLayer("Background", result.pixelFormat)
  bg.locks.transparency = true
  bg.locks.position = true
  let c = fill.toRgb
  let px = fromRgba(bg.content.raster.format, [c[0], c[1], c[2], fill.alpha])
  bg.content.raster.fillRect(result.bounds, px)
  result.layers.add bg

proc maxGroupDepth*(d: Document): int =
  var stack: seq[(ptr seq[Layer], int)] = @[(unsafeAddr d.layers, 0)]
  while stack.len > 0:
    let (ls, depth) = stack.pop()
    if ls[].len == 0: continue
    result = max(result, depth)
    for l in ls[]:
      if l.content.kind == lkGroup and l.content.group.children.len > 0:
        stack.add (unsafeAddr l.content.group.children, depth + 1)

iterator walkRec(layers: ptr seq[Layer]): (LayerPath, int, ptr Layer) =
  ## Depth-first, bottom-to-top, iteratively.
  var stack: seq[(ptr seq[Layer], int, LayerPath)] = @[(layers, 0, newSeq[int]())]
  while stack.len > 0:
    let (ls, i, prefix) = stack[^1]
    if i >= ls[].len:
      discard stack.pop()
      continue
    stack[^1][1] = i + 1
    let path = prefix & i
    let lp = addr ls[][i]
    yield (path, path.len - 1, lp)
    if lp[].content.kind == lkGroup:
      stack.add (addr lp[].content.group.children, 0, path)

proc walk*(d: Document): seq[(LayerPath, int, Layer)] =
  ## Depth-first walk yielding `(path, depth, layer)` bottom-to-top.
  for (p, depth, l) in walkRec(unsafeAddr d.layers): result.add (p, depth, l[])

iterator layerPtrs*(d: var Document): (LayerPath, ptr Layer) =
  for (p, _, l) in walkRec(addr d.layers): yield (p, l)

iterator allLayers*(d: Document): Layer =
  for (_, _, l) in walkRec(unsafeAddr d.layers): yield l[]

proc layerCount*(d: Document): int =
  for _ in d.allLayers: inc result

proc pathOf*(d: Document; id: LayerId): Option[LayerPath] =
  for (p, _, l) in walkRec(unsafeAddr d.layers):
    if l[].id == id: return some(p)
  none(LayerPath)

proc layerAtPtr*(d: var Document; path: openArray[int]): ptr Layer =
  if path.len == 0 or path[0] < 0 or path[0] >= d.layers.len: return nil
  result = addr d.layers[path[0]]
  for k in 1 ..< path.len:
    if result[].content.kind != lkGroup: return nil
    let ch = addr result[].content.group.children
    if path[k] < 0 or path[k] >= ch[].len: return nil
    result = addr ch[][path[k]]

proc layerAt*(d: Document; path: openArray[int]): Option[Layer] =
  let p = layerAtPtr(cast[var Document](unsafeAddr d), path)
  if p == nil: none(Layer) else: some(p[])

proc layerPtr*(d: var Document; id: LayerId): ptr Layer =
  ## Mutable access to the layer `id` (nil when absent). Valid until the tree changes.
  for (_, l) in d.layerPtrs:
    if l[].id == id: return l
  nil

proc layer*(d: Document; id: LayerId): Option[Layer] =
  for l in d.allLayers:
    if l.id == id: return some(l)
  none(Layer)

proc hasLayer*(d: Document; id: LayerId): bool = d.pathOf(id).isSome

proc locksAt*(d: Document; path: openArray[int]): Locks =
  ## Locks in force on the layer at `path`: its own and those of every enclosing group.
  for n in 1 .. path.len:
    let l = d.layerAt(path[0 ..< n])
    if l.isSome: result = result.union(l.get.locks)

proc effectiveLocks*(d: Document; id: LayerId): Locks =
  let p = d.pathOf(id)
  if p.isSome: d.locksAt(p.get) else: Locks()

proc unusedLinkGroup*(d: Document): Option[uint64] =
  var used: seq[uint64]
  for l in d.allLayers:
    if l.linkGroup.isSome: used.add l.linkGroup.get
  used.sort()
  used = deduplicate(used, isSorted = true)
  if used.len == 0: return some(1'u64)
  if used[^1] < high(uint64): return some(used[^1] + 1)
  var free = 1'u64
  for g in used:
    if g > free: break
    if g == free:
      if free == high(uint64): return none(uint64)
      inc free
  some(free)

proc siblingsPtr(d: var Document; path: openArray[int]): (ptr seq[Layer], int) =
  if path.len == 0: return (nil, -1)
  if path.len == 1: return (addr d.layers, path[0])
  let p = d.layerAtPtr(path[0 ..< ^1])
  if p == nil or p[].content.kind != lkGroup: return (nil, -1)
  (addr p[].content.group.children, path[^1])

proc childrenPtr*(d: var Document; parent: Option[LayerId]): ptr seq[Layer] =
  ## The child list of `parent` (the root when none); nil when it is not a group.
  if parent.isNone: return addr d.layers
  let p = d.layerPtr(parent.get)
  if p == nil or p[].content.kind != lkGroup: nil else: addr p[].content.group.children

proc insertAbove*(d: var Document; above: Option[LayerId]; layer: Layer): LayerId =
  ## Inserts `layer` directly above `above` (or at the top of the root).
  result = layer.id
  if above.isSome:
    let p = d.pathOf(above.get)
    if p.isSome:
      let (sib, idx) = d.siblingsPtr(p.get)
      if sib != nil:
        sib[].insert(layer, idx + 1)
        return
  d.layers.add layer

proc remove*(d: var Document; id: LayerId): Option[Layer] =
  let p = d.pathOf(id)
  if p.isNone: return none(Layer)
  let (sib, idx) = d.siblingsPtr(p.get)
  if sib == nil: return none(Layer)
  result = some(sib[][idx])
  sib[].delete(idx)

proc shift*(d: var Document; id: LayerId; delta: int): bool =
  ## Moves a layer up (+1) or down (-1) among its siblings. False at the ends.
  let p = d.pathOf(id)
  if p.isNone: return false
  let (sib, idx) = d.siblingsPtr(p.get)
  if sib == nil: return false
  let to = idx + delta
  if to < 0 or to >= sib[].len: return false
  let l = sib[][idx]
  sib[].delete(idx)
  sib[].insert(l, to)
  true

proc layerNames(d: Document): HashSet[string] =
  for l in d.allLayers: result.incl l.name

proc nextLayerName*(d: Document; base: string): string =
  let names = d.layerNames
  for n in 1 .. names.len + 1:
    let c = &"{base} {n}"
    if c notin names: return c
  base

proc copyName*(d: Document; name: string): string =
  ## "Layer 1 copy", then "Layer 1 copy 2", …; a name already ending in "copy (N)" is numbered
  ## from its root.
  var root = name
  let at = name.rfind(" copy")
  if at >= 0:
    let rest = name[at + 5 .. ^1]
    if rest.len == 0 or (rest.len > 1 and rest[0] == ' ' and rest[1 .. ^1].allCharsInSet(Digits)): root = name[0 ..< at]
  let names = d.layerNames
  let first = root & " copy"
  if first notin names: return first
  for n in 2 .. names.len + 2:
    let c = &"{root} copy {n}"
    if c notin names: return c
  first

proc topLayer*(d: Document): Option[LayerId] =
  if d.layers.len > 0: some(d.layers[^1].id) else: none(LayerId)

proc artboards*(d: Document): seq[(LayerId, string, Artboard)] =
  for l in d.layers:
    if l.content.kind == lkGroup and l.content.group.artboard.isSome:
      result.add (l.id, l.name, l.content.group.artboard.get)
proc hasArtboards*(d: Document): bool =
  for l in d.layers:
    if l.artboard.isSome: return true
proc compIndex*(d: Document; id: uint32): int =
  for i, c in d.layerComps:
    if c.id == id: return i
  -1
proc artboardOf*(d: Document; id: LayerId): Option[LayerId] =
  let p = d.pathOf(id)
  if p.isNone: return
  let top = d.layers[p.get[0]]
  if top.artboard.isSome: some(top.id) else: none(LayerId)

proc positionBounds*(l: Layer): Rect =
  case l.content.kind
  of lkGroup: (if l.content.group.artboard.isSome: l.content.group.artboard.get.rect else: Rect())
  of lkAdjustment, lkFill: Rect()
  else: (if l.hasSurface: l.surface.contentBounds else: Rect())
proc layerPosition*(l: Layer): Option[(int, int)] =
  let r = l.positionBounds
  if r.isEmpty: none((int, int)) else: some((r.x0, r.y0))
proc captureStates*(d: Document): seq[CompLayerState] =
  for l in d.allLayers:
    result.add CompLayerState(layer: l.id, visible: some(l.visible), position: l.layerPosition,
      appearance: some(CompAppearance(blend: l.blend, opacity: l.opacity, fillOpacity: l.fillOpacity, effects: l.effects)))
proc missingLayers*(c: LayerComp; d: Document): seq[LayerId] =
  var live = initHashSet[uint64]()
  for l in d.allLayers: live.incl uint64(l.id)
  for s in c.states:
    if uint64(s.layer) notin live: result.add s.layer
proc nextCompId*(d: Document): uint32 =
  var m = 0'u32
  for c in d.layerComps: m = max(m, c.id)
  result = if m == high(uint32): 1'u32 else: m + 1
  while d.compIndex(result) >= 0: result = if result == high(uint32): 1'u32 else: result + 1

type ResolvedSlice* = object
  number*: int
  id*: Option[uint32]
  origin*: SliceOrigin
  rect*: Rect
  name*: string
  kind*: SliceKind

proc sliceBaseName*(d: Document): string =
  let dot = d.name.rfind('.')
  let n = if dot >= 0: d.name[0 ..< dot] else: d.name
  for r in n.runes:
    if r.isAlpha or (int(r) < 128 and char(int(r)) in {'0'..'9', '-', '_'}): result.add r.toUTF8
    else: result.add '_'
  if result.len == 0: result = "Untitled"

proc resolveSlices*(d: Document): seq[ResolvedSlice] =
  let base = d.sliceBaseName
  let canvas = d.bounds
  var stored: seq[(uint32, Rect)]
  for s in d.slices.list:
    let r = s.rect.intersect(canvas)
    if not r.isEmpty: stored.add (s.id, r)
  var rects: seq[Rect]
  for (_, r) in stored: rects.add r
  var all: seq[(Option[uint32], Rect)]
  for (id, r) in stored: all.add (some(id), r)
  for r in autoSlices(canvas, rects): all.add (none(uint32), r)
  all.sort(proc (a, b: (Option[uint32], Rect)): int = cmp((a[1].y0, a[1].x0), (b[1].y0, b[1].x0)))
  let single = all.len == 1
  for i, (id, rect) in all:
    let idx = if id.isSome: d.slices.sliceIndex(id.get) else: -1
    let number = i + 1
    var name = if idx >= 0: d.slices.list[idx].name else: ""
    if name.len == 0: name = (if single: base else: base & "_" & align($number, 2, '0'))
    result.add ResolvedSlice(number: number, id: id, rect: rect, name: name,
      origin: (if idx >= 0: d.slices.list[idx].origin else: soAuto),
      kind: (if idx >= 0: d.slices.list[idx].kind else: skImage))

proc `==`*(a, b: Adjustment): bool {.noSideEffect.} =
  {.cast(noSideEffect).}:
    result = a.kind == b.kind and toJ(a) == toJ(b)
proc `==`*(a, b: Contour): bool {.noSideEffect.} =
  {.cast(noSideEffect).}:
    result = a.kind == b.kind and toJ(a) == toJ(b)
proc `==`*(a, b: FxPaint): bool {.noSideEffect.} =
  {.cast(noSideEffect).}:
    result = a.kind == b.kind and toJ(a) == toJ(b)
proc `==`*(a, b: Effect): bool {.noSideEffect.} =
  {.cast(noSideEffect).}:
    result = a.kind == b.kind and toJ(a) == toJ(b)
proc `==`*(a, b: LiveShape): bool {.noSideEffect.} =
  {.cast(noSideEffect).}:
    result = a.kind == b.kind and toJ(a) == toJ(b)
proc `==`*(a, b: Fill): bool {.noSideEffect.} =
  {.cast(noSideEffect).}:
    result = a.kind == b.kind and toJ(a) == toJ(b)
proc `==`*(a, b: TextShape): bool {.noSideEffect.} =
  {.cast(noSideEffect).}:
    result = a.kind == b.kind and toJ(a) == toJ(b)
proc `==`*(a, b: VideoSource): bool {.noSideEffect.} =
  {.cast(noSideEffect).}:
    result = a.kind == b.kind and toJ(a) == toJ(b)
proc `==`*(a, b: ArtboardBackground): bool {.noSideEffect.} =
  {.cast(noSideEffect).}:
    result = a.kind == b.kind and toJ(a) == toJ(b)
proc `==`*(a, b: VarKind): bool {.noSideEffect.} =
  {.cast(noSideEffect).}:
    result = a.kind == b.kind and toJ(a) == toJ(b)
proc `==`*(a, b: DataValue): bool {.noSideEffect.} =
  {.cast(noSideEffect).}:
    result = a.kind == b.kind and toJ(a) == toJ(b)
