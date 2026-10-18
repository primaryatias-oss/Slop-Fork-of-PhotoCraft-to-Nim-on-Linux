## The brush model (port of `paint/src/brush.rs` plus the data types of `paint/src/lib.rs`): tip
## shape plus Photoshop-style dynamics sections, all plain serde data.
##
## Every section is optional (an `enabled` flag, or a neutral default), so a partial JSON object
## such as `{"size": 40, "scattering": {"enabled": true, "scatter": {"jitter": 2.0}}}` deserialises
## onto the defaults. Field names are camelCase in JSON, matching the command parameters.
##
## Units: sizes are pixels, angles are degrees, every percentage is a 0..1 fraction (scatter and
## texture scale may exceed 1, like Photoshop's 1000 % scatter).

import std/[math, json, options]
import ../geom, ../color, ../rmath, ../serde
import ./tile
export tile

const
  MaxBrushSize* = 5000'f32 ## Largest brush diameter accepted by the rasterizer and brush controls.
  MaxScatter* = 10'f32
    ## Largest Scatter amount (primary and Dual Brush): 10 = 1000 %, the Brush Settings slider's
    ## top. Beyond it a dab could land arbitrarily far away.

type
  Control* = enum
    ## What drives a dynamic parameter (Photoshop's "Control" pop-ups).
    ctlOff = "off"
    ctlFade = "fade"                         ## ramp over `fadeSteps` spacing steps
    ctlPenPressure = "penPressure"
    ctlPenTilt = "penTilt"
    ctlStylusWheel = "stylusWheel"
    ctlRotation = "rotation"                 ## barrel rotation of the stylus
    ctlInitialDirection = "initialDirection" ## angle only: the first stroke segment's direction
    ctlDirection = "direction"               ## angle only: the current stroke direction

  Dynamic* = object
    ## One dynamic parameter: random jitter plus a controller with a floor.
    jitter*: float32   ## 0..1 random variation per dab (for scatter: the amount, may exceed 1)
    control*: Control
    fadeSteps*: uint32 ## steps for `ctlFade`
    minimum*: float32  ## 0..1 floor the control/jitter may reduce the value to

  TipKind* = enum tkRound = "round", tkSampled = "sampled", tkStored = "stored"

  TipShape* = object
    ## The brush tip bitmap source. A loaded stored tip serialises as `sampled` (its full bitmap).
    case kind*: TipKind
    of tkRound: discard   # computed round/elliptical tip (hardness, angle, roundness)
    of tkSampled: sampled*: GrayTile # 1 = full paint; scaled so its larger side equals `size`
    of tkStored: stored*: StoredTile # kept outside the settings by its owner, loaded on demand

  ShapeDynamics* = object
    enabled*: bool
    size*: Dynamic       ## size jitter / control; `minimum` is Minimum Diameter
    tiltScale*: float32  ## 0..2: with size on Pen Tilt, how much a tilted pen squashes the tip
    angle*: Dynamic      ## angle jitter (fraction of ±180°) / control
    roundness*: Dynamic  ## roundness jitter / control; `minimum` is Minimum Roundness
    flipXJitter*, flipYJitter*: bool
    brushProjection*: bool ## the pen's tilt and rotation project the tip

  Scattering* = object
    enabled*: bool
    scatter*: Dynamic    ## `jitter` = amount (1 = up to one radius); control scales it
    bothAxes*: bool      ## scatter radially instead of only perpendicular to the stroke
    count*: uint32       ## dabs per spacing step (1..16)
    countJitter*: Dynamic

  MaskMode* = enum
    ## How a grayscale mask (texture or dual tip) combines with the brush coverage.
    mmMultiply = "multiply", mmSubtract = "subtract", mmDarken = "darken", mmOverlay = "overlay",
    mmColorDodge = "colorDodge", mmColorBurn = "colorBurn", mmLinearBurn = "linearBurn",
    mmHardMix = "hardMix", mmLinearHeight = "linearHeight", mmHeight = "height"

  PatternStyle* = enum
    ## Procedural pattern styles (generated deterministically; no bundled pattern files).
    psNoise = "noise", psCanvas = "canvas", psPaper = "paper", psDots = "dots"

  PatternKind* = enum pkProcedural = "procedural", pkTile = "tile", pkStored = "stored"

  Pattern* = object
    ## A texture pattern: a procedural generator or an explicit grayscale tile (tiled infinitely).
    case kind*: PatternKind
    of pkProcedural:
      style*: PatternStyle
      size*: uint32
      seed*: uint32
    of pkTile: tile*: GrayTile
    of pkStored: stored*: StoredTile

  Texture* = object
    enabled*: bool
    pattern*: Pattern
    invert*: bool
    scale*: float32      ## pattern scale (1 = 100 %)
    brightness*: float32 ## -1..1
    contrast*: float32   ## -1..1
    eachTip*: bool       ## texture every dab (enables depth jitter) instead of the stroke
    mode*: MaskMode
    depth*: float32      ## 0..1
    depthJitter*: Dynamic

  DualBrush* = object
    ## A second tip stroked along the same path; the stroke is masked by it.
    enabled*: bool
    mode*: MaskMode
    tip*: TipShape
    size*, hardness*, roundness*, angle*, spacing*, scatter*: float32
    bothAxes*: bool
    count*: uint32
    flip*: bool

  ColorDynamics* = object
    enabled*: bool
    perTip*: bool        ## vary every dab (true) or once per stroke (false)
    fgBg*: Dynamic       ## foreground/background mix jitter / control
    hueJitter*: float32  ## 0..1 fraction of ±half the hue wheel
    saturationJitter*: float32
    brightnessJitter*: float32
    purity*: float32     ## -1 (fully desaturated) .. 1 (fully saturated)

  Transfer* = object
    ## Per-dab opacity ceiling and flow, plus the Mixer Brush's wetness and mix.
    enabled*: bool
    opacity*, flow*: Dynamic
    wetness*: Dynamic    ## Mixer Brush only: scales the Wet setting per dab
    mix*: Dynamic        ## Mixer Brush only: scales the Mix setting per dab

  SectionLocks* = object
    ## Sections whose settings stay when another brush preset is picked.
    shapeDynamics*, scattering*, texture*, dualBrush*, colorDynamics*, transfer*, pose*, noise*,
      wetEdges*, buildUp*, smoothing*, protectTexture*: bool

  Pose* = object
    ## Brush Pose: stylus values used instead of (override) or for missing device data.
    enabled*: bool
    tiltX*, tiltY*: float32 ## degrees, -90..90
    rotation*: float32      ## degrees 0..360
    pressure*: float32      ## 0..1
    overrideTilt*, overrideRotation*, overridePressure*: bool

  Smoothing* = object
    amount*: float32       ## 0..1
    pulledString*: bool    ## lazy mouse: paint only when the string of `amount × 100 px` is taut
    catchUp*: bool         ## keep catching up with the pointer while it pauses
    catchUpOnEnd*: bool    ## finish the stroke at the last pointer position
    adjustForZoom*: bool   ## divide the string length by the view zoom

  MixerSettings* = object
    ## Mixer Brush options (all 0..1).
    wet*, load*, mix*, flow*: float32
    sampleAllLayers*: bool ## pick up colour from all visible layers

  BrushSettings* = object
    ## A complete brush: tip shape, dynamics, and stroke-level options.
    size*: float32          ## diameter in pixels
    hardness*: float32      ## 0 (soft) .. 1 (hard); computed tips only
    spacing*: float32       ## between dabs as a fraction of the diameter
    spacingEnabled*: bool   ## off: the pointer's speed sets the spacing
    opacity*: float32       ## maximum coverage for the whole stroke
    flow*: float32          ## per-dab coverage
    pressureSize*: bool     ## shortcut for Shape Dynamics › Size › Pen Pressure
    pressureOpacity*: bool  ## shortcut for Transfer › Opacity › Pen Pressure
    color*: array[4, float32]      ## straight RGBA (display RGB)
    background*: array[4, float32] ## for Color Dynamics' foreground/background jitter
    erase*: bool
    mode*: BlendMode
    tip*: TipShape
    angle*: float32         ## degrees, counter-clockwise
    roundness*: float32     ## 0..1 (1 = circle)
    flipX*, flipY*: bool
    aliased*: bool          ## Pencil: no anti-aliasing
    square*: bool           ## with `aliased`, a square tip (the Eraser's Block mode)
    shapeDynamics*: ShapeDynamics
    scattering*: Scattering
    texture*: Texture
    dualBrush*: DualBrush
    colorDynamics*: ColorDynamics
    transfer*: Transfer
    pose*: Pose
    noise*: bool
    wetEdges*: bool
    buildUp*: bool          ## airbrush: keep depositing dabs while the pointer lingers
    buildUpRate*: float32   ## dabs per second
    smoothing*: Smoothing
    protectTexture*: bool
    locks*: SectionLocks
    mixer*: MixerSettings
    seed*: uint64           ## random seed for jitters, so strokes replay

  BrushPreset* = object
    ## A named brush preset (session tool state; serde round-trips).
    name*: string
    brush*: BrushSettings
    builtin*: bool        ## shipped with PhotoCraft
    group*: string        ## preset group (folder) in the Brushes panel
    folder*: seq[string]  ## folders inside `group`, outermost first

  StrokePoint* = object
    ## One input sample. Missing stylus data defaults to "mouse": full pressure, no tilt/rotation.
    x*, y*: float64
    pressure*: float32       ## 0..1, 1 for mouse
    tiltX*, tiltY*: float32  ## degrees -90..90
    rotation*: float32       ## barrel rotation in degrees 0..360
    wheel*: float32          ## airbrush stylus wheel 0..1
    time*: float64           ## milliseconds

  Stroke* = object
    brush*: BrushSettings
    points*: seq[StrokePoint]

  Dab* = object
    ## A dab placed along the stroke (after dynamics).
    center*: Point
    radius*: float32
    alpha*: float32          ## flow: per-dab build-up amount
    angle*: float32          ## radians, counter-clockwise
    roundness*: float32
    flipX*, flipY*: bool
    opacity*: float32        ## opacity ceiling this dab builds up to
    color*: array[4, float32] ## straight RGBA paint colour (Color Dynamics)
    depth*: float32          ## texture depth for per-tip texturing
    index*: uint64           ## position in the stroke (0 = first)
    projAngle*, projScale*: float32 ## Brush Projection direction and factor (1 = none)
    wet*, mix*: float32      ## Mixer Brush multipliers of Wet and Mix

# ---------------------------------------------------------------- defaults

proc defaultDynamic*(): Dynamic = Dynamic(fadeSteps: 25)
proc controlled*(c: Control): Dynamic = Dynamic(control: c, fadeSteps: 25)
proc jitterDyn*(j: float32): Dynamic = Dynamic(jitter: j, fadeSteps: 25)
proc isActive*(d: Dynamic): bool = d.jitter > 0 or d.control != ctlOff

proc defaultPattern*(): Pattern = Pattern(kind: pkProcedural, style: psPaper, size: 128, seed: 1)
proc procedural*(style: PatternStyle; size, seed: uint32): Pattern =
  Pattern(kind: pkProcedural, style: style, size: size, seed: seed)

proc defaultShapeDynamics*(): ShapeDynamics =
  ShapeDynamics(size: defaultDynamic(), angle: defaultDynamic(), roundness: defaultDynamic())
proc defaultScattering*(): Scattering =
  Scattering(scatter: defaultDynamic(), count: 1, countJitter: defaultDynamic())
proc defaultTexture*(): Texture =
  Texture(pattern: defaultPattern(), scale: 1, mode: mmMultiply, depth: 1, depthJitter: defaultDynamic())
proc defaultDualBrush*(): DualBrush =
  DualBrush(mode: mmMultiply, tip: TipShape(kind: tkRound), size: 20, hardness: 1, roundness: 1,
            spacing: 0.25, count: 1)
proc defaultColorDynamics*(): ColorDynamics = ColorDynamics(perTip: true, fgBg: defaultDynamic())
proc defaultTransfer*(): Transfer =
  Transfer(opacity: defaultDynamic(), flow: defaultDynamic(), wetness: defaultDynamic(), mix: defaultDynamic())
proc defaultPose*(): Pose = Pose(pressure: 1)
proc defaultSmoothing*(): Smoothing = Smoothing(catchUp: true, catchUpOnEnd: true, adjustForZoom: true)
proc defaultMixerSettings*(): MixerSettings = MixerSettings(wet: 0.5, load: 0.5, mix: 0.5, flow: 1)

proc defaultBrush*(): BrushSettings =
  BrushSettings(size: 20, hardness: 1, spacing: 0.1, spacingEnabled: true, opacity: 1, flow: 1,
                pressureSize: true, color: [0'f32, 0, 0, 1], background: [1'f32, 1, 1, 1],
                mode: bmNormal, tip: TipShape(kind: tkRound), roundness: 1,
                shapeDynamics: defaultShapeDynamics(), scattering: defaultScattering(),
                texture: defaultTexture(), dualBrush: defaultDualBrush(),
                colorDynamics: defaultColorDynamics(), transfer: defaultTransfer(), pose: defaultPose(),
                buildUpRate: 20, smoothing: defaultSmoothing(), mixer: defaultMixerSettings())

proc defaultStrokePoint*(): StrokePoint = StrokePoint(pressure: 1, wheel: 1)
proc strokePoint*(x, y: float64; pressure: float32): StrokePoint =
  StrokePoint(x: x, y: y, pressure: pressure, wheel: 1)

proc roundDab*(center: Point; radius, alpha: float32): Dab =
  ## A plain round dab.
  Dab(center: center, radius: radius, alpha: alpha, roundness: 1, opacity: 1, color: [0'f32, 0, 0, 1],
      depth: 1, projScale: 1, wet: 1, mix: 1)

proc defaultOf*(_: typedesc[Dynamic]): Dynamic = defaultDynamic()
proc defaultOf*(_: typedesc[ShapeDynamics]): ShapeDynamics = defaultShapeDynamics()
proc defaultOf*(_: typedesc[Scattering]): Scattering = defaultScattering()
proc defaultOf*(_: typedesc[Texture]): Texture = defaultTexture()
proc defaultOf*(_: typedesc[DualBrush]): DualBrush = defaultDualBrush()
proc defaultOf*(_: typedesc[ColorDynamics]): ColorDynamics = defaultColorDynamics()
proc defaultOf*(_: typedesc[Transfer]): Transfer = defaultTransfer()
proc defaultOf*(_: typedesc[Pose]): Pose = defaultPose()
proc defaultOf*(_: typedesc[Smoothing]): Smoothing = defaultSmoothing()
proc defaultOf*(_: typedesc[MixerSettings]): MixerSettings = defaultMixerSettings()
proc defaultOf*(_: typedesc[BrushSettings]): BrushSettings = defaultBrush()
proc defaultOf*(_: typedesc[StrokePoint]): StrokePoint = defaultStrokePoint()
proc defaultOf*(_: typedesc[Pattern]): Pattern = defaultPattern()

proc serdeCamel*(_: typedesc[Dynamic]): bool = true
proc serdeCamel*(_: typedesc[ShapeDynamics]): bool = true
proc serdeCamel*(_: typedesc[Scattering]): bool = true
proc serdeCamel*(_: typedesc[Texture]): bool = true
proc serdeCamel*(_: typedesc[DualBrush]): bool = true
proc serdeCamel*(_: typedesc[ColorDynamics]): bool = true
proc serdeCamel*(_: typedesc[Transfer]): bool = true
proc serdeCamel*(_: typedesc[SectionLocks]): bool = true
proc serdeCamel*(_: typedesc[Pose]): bool = true
proc serdeCamel*(_: typedesc[Smoothing]): bool = true
proc serdeCamel*(_: typedesc[MixerSettings]): bool = true
proc serdeCamel*(_: typedesc[BrushSettings]): bool = true
proc serdeCamel*(_: typedesc[StrokePoint]): bool = true

# ---------------------------------------------------------------- tip shapes and patterns

proc `==`*(a, b: TipShape): bool =
  if a.kind != b.kind: return false
  case a.kind
  of tkRound: true
  of tkSampled: a.sampled == b.sampled
  of tkStored: a.stored == b.stored

proc `==`*(a, b: Pattern): bool =
  if a.kind != b.kind: return false
  case a.kind
  of pkProcedural: a.style == b.style and a.size == b.size and a.seed == b.seed
  of pkTile: a.tile == b.tile
  of pkStored: a.stored == b.stored

proc bitmap*(t: TipShape): Option[GrayTile] =
  ## The bitmap a sampled tip paints with (a stored tip's full bitmap, or its preview while it
  ## isn't loaded); none for a computed tip.
  case t.kind
  of tkRound: none(GrayTile)
  of tkSampled: some(t.sampled)
  of tkStored: some(t.stored.bitmap)

proc bitmapSize*(t: TipShape): Option[(uint32, uint32)] =
  ## The tip's real pixel size (a stored tip's, not its preview's); none for a computed tip.
  case t.kind
  of tkRound: none((uint32, uint32))
  of tkSampled: some((t.sampled.width, t.sampled.height))
  of tkStored: some((t.stored.width, t.stored.height))

proc bitmap*(p: Pattern): Option[GrayTile] =
  ## The tile a bitmap pattern textures with; none for a procedural pattern.
  case p.kind
  of pkProcedural: none(GrayTile)
  of pkTile: some(p.tile)
  of pkStored: some(p.stored.bitmap)

proc toJ*(t: TipShape): JsonNode =
  case t.kind
  of tkRound: newJString("round")
  of tkSampled: tagged("sampled", toJ(t.sampled))
  of tkStored:
    if t.stored.isLoaded: tagged("sampled", toJ(t.stored.full[])) else: tagged("stored", toJ(t.stored))

proc fromJ*(n: JsonNode; t: var TipShape) =
  let (tag, payload) = untag(n)
  case tag
  of "round":
    if not payload.isNil and payload.kind != JNull: raise serdeError("invalid type: expected unit variant round")
    t = TipShape(kind: tkRound)
  of "sampled":
    if payload.isNil: raise serdeError("invalid type: unit variant, expected newtype variant")
    var g: GrayTile
    fromJ(payload, g)
    t = TipShape(kind: tkSampled, sampled: g)
  of "stored":
    if payload.isNil: raise serdeError("invalid type: unit variant, expected newtype variant")
    var s: StoredTile
    fromJ(payload, s)
    t = TipShape(kind: tkStored, stored: s)
  else: raise serdeError("unknown variant `" & tag & "`, expected one of `round`, `sampled`, `stored`")

proc toJ*(p: Pattern): JsonNode =
  case p.kind
  of pkProcedural: tagged("procedural", %*{"style": $p.style, "size": p.size, "seed": p.seed})
  of pkTile: tagged("tile", toJ(p.tile))
  of pkStored:
    if p.stored.isLoaded: tagged("tile", toJ(p.stored.full[])) else: tagged("stored", toJ(p.stored))

proc fromJ*(n: JsonNode; p: var Pattern) =
  let (tag, payload) = untag(n)
  if payload.isNil: raise serdeError("invalid type: unit variant, expected struct or newtype variant")
  case tag
  of "procedural":
    if payload.kind != JObject: raise serdeError("expected struct variant Pattern::Procedural")
    var r = Pattern(kind: pkProcedural)
    for key in ["style", "size", "seed"]:
      if payload.getOrDefault(key).isNil: raise serdeError("missing field `" & key & "`")
    fromJ(payload["style"], r.style)
    fromJ(payload["size"], r.size)
    fromJ(payload["seed"], r.seed)
    p = r
  of "tile":
    var g: GrayTile
    fromJ(payload, g)
    p = Pattern(kind: pkTile, tile: g)
  of "stored":
    var s: StoredTile
    fromJ(payload, s)
    p = Pattern(kind: pkStored, stored: s)
  else: raise serdeError("unknown variant `" & tag & "`, expected one of `procedural`, `tile`, `stored`")

# ---------------------------------------------------------------- brush settings serde

proc toJ*(b: BrushSettings): JsonNode =
  result = objectToJ(b)
  if not b.square: result.delete("square")

proc fromJ*(n: JsonNode; b: var BrushSettings) =
  objectFromJ(n, b)
  # serde aliases: the snake_case spellings of the pressure shortcuts.
  if n.kind == JObject:
    let ps = n.getOrDefault("pressure_size")
    if not ps.isNil: fromJ(ps, b.pressureSize)
    let po = n.getOrDefault("pressure_opacity")
    if not po.isNil: fromJ(po, b.pressureOpacity)

proc brushFromJson*(n: JsonNode): BrushSettings =
  ## A brush from JSON, missing fields defaulted; raises SerdeError on malformed data.
  result = defaultBrush()
  fromJ(n, result)

proc toJson*(b: BrushSettings): JsonNode = toJ(b)

proc toJ*(p: BrushPreset): JsonNode =
  result = %*{"name": p.name, "brush": toJ(p.brush), "builtin": p.builtin}
  if p.group.len > 0: result["group"] = %p.group
  if p.folder.len > 0: result["folder"] = toJ(p.folder)

proc fromJ*(n: JsonNode; p: var BrushPreset) =
  if n.kind != JObject: raise serdeError("expected a brush preset object")
  for key in ["name", "brush"]:
    if n.getOrDefault(key).isNil: raise serdeError("missing field `" & key & "`")
  var r = BrushPreset(brush: defaultBrush())
  fromJ(n["name"], r.name)
  fromJ(n["brush"], r.brush)
  for key, v in n.pairs:
    case key
    of "builtin": fromJ(v, r.builtin)
    of "group": fromJ(v, r.group)
    of "folder": fromJ(v, r.folder)
    else: discard
  p = r

proc toJ*(s: Stroke): JsonNode = %*{"brush": toJ(s.brush), "points": toJ(s.points)}

proc fromJ*(n: JsonNode; s: var Stroke) =
  if n.kind != JObject: raise serdeError("expected a stroke object")
  for key in ["brush", "points"]:
    if n.getOrDefault(key).isNil: raise serdeError("missing field `" & key & "`")
  var r = Stroke(brush: defaultBrush())
  fromJ(n["brush"], r.brush)
  fromJ(n["points"], r.points)
  s = r

# ---------------------------------------------------------------- brush operations

proc boundedForRender*(b: BrushSettings): BrushSettings =
  ## Copy the settings with the primary and dual diameters and Scatter amounts constrained for
  ## rasterization: a final guard for direct users of the infallible paint API, which must not
  ## turn malformed brush dimensions into giant allocations, nor a huge Scatter amount into
  ## non-finite dab centres.
  proc safeSize(size: float32): float32 =
    if classify(size) notin {fcNan, fcInf, fcNegInf}: clamp(size, 0.5'f32, MaxBrushSize) else: 0.5'f32
  proc safeScatter(amount: float32): float32 =
    if amount != amount: 0'f32 else: clamp(amount, 0'f32, MaxScatter)
  result = b
  result.size = safeSize(result.size)
  result.dualBrush.size = safeSize(result.dualBrush.size)
  result.scattering.scatter.jitter = safeScatter(result.scattering.scatter.jitter)
  result.dualBrush.scatter = safeScatter(result.dualBrush.scatter)

proc withProtectedTexture*(b: BrushSettings; current: BrushSettings): BrushSettings =
  ## Apply Protect Texture: if `current` protects its texture, a newly chosen brush with a
  ## texture keeps `current`'s pattern and scale.
  result = b
  if current.protectTexture and current.texture.enabled and result.texture.enabled:
    result.texture.pattern = current.texture.pattern
    result.texture.scale = current.texture.scale
    result.protectTexture = true

proc pickedOver*(b: BrushSettings; current: BrushSettings): BrushSettings =
  ## This brush (a preset) picked while `current` is the tool's brush: Smoothing is a tool option,
  ## so it stays the tool's, a protected texture stays too, and so does every section `current`
  ## has locked (the locks themselves are tool state and carry over).
  result = b.withProtectedTexture(current)
  result.smoothing = current.smoothing
  let l = current.locks
  if l.shapeDynamics: result.shapeDynamics = current.shapeDynamics
  if l.scattering: result.scattering = current.scattering
  if l.texture: result.texture = current.texture
  if l.dualBrush: result.dualBrush = current.dualBrush
  if l.colorDynamics: result.colorDynamics = current.colorDynamics
  if l.transfer: result.transfer = current.transfer
  if l.pose: result.pose = current.pose
  if l.noise: result.noise = current.noise
  if l.wetEdges: result.wetEdges = current.wetEdges
  if l.buildUp:
    result.buildUp = current.buildUp
    result.buildUpRate = current.buildUpRate
  if l.protectTexture: result.protectTexture = current.protectTexture
  result.locks = current.locks

# ---------------------------------------------------------------- tip geometry (lib.rs, render.rs)

proc tipFalloff*(d, radius, hardness: float32): float32 {.inline.} =
  ## Opacity of a round tip at distance `d` from its centre, without edge anti-aliasing: 1 in the
  ## hard core (`hardness` of the radius), then `(1 - t²)⁴` down to 0 at the radius.
  let inner = radius * clamp(hardness, 0'f32, 1'f32)
  if d <= inner or radius - inner < 1e-3'f32: return 1
  let t = clamp((d - inner) / (radius - inner), 0'f32, 1'f32)
  let s = 1'f32 - t * t
  let s2 = s * s
  s2 * s2

proc halfCoverageRadius*(radius, hardness: float32): float32 {.inline.} =
  ## Distance from the centre where `tipFalloff` drops to 50 % (the normal brush tip cursor).
  let h = if hardness != hardness: 0'f32 else: clamp(hardness, 0'f32, 1'f32)
  let t = sqrt(1'f32 - sqrt(sqrt(0.5'f32)))
  radius * (h + t * (1'f32 - h))

proc dabCoverage*(d, radius, hardness: float32): float32 {.inline.} =
  ## Coverage of a round dab at distance `d` from its centre (anti-aliased edge, hardness falloff).
  if d >= radius + 0.5'f32: return 0
  let edge = rmin(clamp(radius + 0.5'f32 - d, 0'f32, 1'f32), 1'f32)
  edge * tipFalloff(d, radius, hardness)

proc gridCenter*(x, y: float64; diameter: float32): (float32, float32) =
  ## Where an aliased (Pencil) dab of `diameter` pixels centred near (`x`, `y`) lands on the pixel
  ## grid: the centre of the pixel holding the point for odd diameters, the nearest pixel corner
  ## for even ones.
  let n = if classify(diameter) notin {fcNan, fcInf, fcNegInf}: asI64(float64(rmax(round(diameter), 1'f32))) else: 1'i64
  proc snap(v: float64): float64 = (if n mod 2 == 1: floor(v) + 0.5 else: round(v))
  (float32(snap(x)), float32(snap(y)))

proc gridSquare*(x, y: float64; diameter: float32): array[4, float64] =
  ## The whole-pixel square `[x0, y0, x1, y1]` an aliased dab can touch: the Pencil's cursor.
  let n = if classify(diameter) notin {fcNan, fcInf, fcNegInf}: float64(clamp(round(diameter), 1'f32, 100_000'f32)) else: 1.0
  let (cx, cy) = gridCenter(x, y, float32(n))
  let h = n / 2
  [float64(cx) - h, float64(cy) - h, float64(cx) + h, float64(cy) + h]

proc smooth*(points: openArray[StrokePoint]; amount: float32): seq[StrokePoint] =
  ## Exponential moving-average smoothing of input points (simplified Smoothing %).
  let a = float64(1'f32 - clamp(amount, 0'f32, 0.95'f32))
  var cur: Option[StrokePoint]
  for p in points:
    var n = p
    if cur.isSome:
      let c = cur.get
      n.x = c.x + (p.x - c.x) * a
      n.y = c.y + (p.y - c.y) * a
    result.add n
    cur = some(n)
  if result.len > 0: result[^1] = points[^1]
