## Filter parameters: every filter's settings (`FilterParams`, internally tagged JSON
## `{"filter": "gaussianBlur", "radius": 4}` like the Rust crate), their option enums, the
## Filter Gallery catalogue, and the per-tile context filters run with.

import std/[json, strutils, algorithm, options, math]
import ../geom, ../color, ../serde, ./image
export image

type
  Halo* = object
    ## How far outside a tile a filter reads: a pixel radius, or the whole canvas.
    bounds*: bool
    radius*: int

  Ctx* = object
    bounds*: Rect
    mode*: ColorMode
    alpha*: bool

  RadialMethod* = enum rmSpin = "spin", rmZoom = "zoom"
  RadialQuality* = enum rqDraft = "draft", rqGood = "good", rqBest = "best"
  Distribution* = enum diUniform = "uniform", diGaussian = "gaussian"
  UndefinedAreas* = enum uaWrap = "wrap", uaRepeat = "repeat", uaTransparent = "transparent"
  Preserve* = enum prSquareness = "squareness", prRoundness = "roundness"
  SpherizeMode* = enum smNormal = "normal", smHorizontalOnly = "horizontalOnly", smVerticalOnly = "verticalOnly"
  WaveType* = enum wtSine = "sine", wtTriangle = "triangle", wtSquare = "square"
  RippleSize* = enum rsSmall = "small", rsMedium = "medium", rsLarge = "large"
  PolarMode* = enum pmRectangularToPolar = "rectangularToPolar", pmPolarToRectangular = "polarToRectangular"
  MezzotintType* = enum
    mzFineDots = "fineDots", mzMediumDots = "mediumDots", mzGrainyDots = "grainyDots", mzCoarseDots = "coarseDots",
    mzShortLines = "shortLines", mzMediumLines = "mediumLines", mzLongLines = "longLines",
    mzShortStrokes = "shortStrokes", mzMediumStrokes = "mediumStrokes", mzLongStrokes = "longStrokes"
  DiffuseMode* = enum dmNormal = "normal", dmDarkenOnly = "darkenOnly", dmLightenOnly = "lightenOnly", dmAnisotropic = "anisotropic"
  ExtrudeType* = enum etBlocks = "blocks", etPyramids = "pyramids"
  TileFill* = enum tfBackground = "background", tfForeground = "foreground", tfInverse = "inverse", tfUnaltered = "unaltered"
  WindMethod* = enum wmWind = "wind", wmBlast = "blast", wmStagger = "stagger"
  ZigZagStyle* = enum zzAroundCenter = "aroundCenter", zzOutFromCenter = "outFromCenter", zzPondRipples = "pondRipples"
  LensType* = enum ltZoom = "zoom", ltPrime35 = "prime35", ltPrime105 = "prime105", ltMoviePrime = "moviePrime"
  LightKind* = enum lkSpot = "spot", lkPoint = "point", lkInfinite = "infinite"
  TextureChannel* = enum tcNone = "none", tcRed = "red", tcGreen = "green", tcBlue = "blue", tcAlpha = "alpha", tcLuminance = "luminance"
  BlurQuality* = enum bqLow = "low", bqMedium = "medium", bqHigh = "high"
  SmartBlurMode* = enum sbNormal = "normal", sbEdgeOnly = "edgeOnly", sbOverlayEdge = "overlayEdge"
  DepthSource* = enum dsNone = "none", dsTransparency = "transparency", dsLayerMask = "layerMask"
  BlurShape* = enum
    bshCircle = "circle", bshRing = "ring", bshSquare = "square", bshDiamond = "diamond", bshTriangle = "triangle",
    bshHexagon = "hexagon", bshStar = "star", bshHeart = "heart", bshCross = "cross"
  HsbModel* = enum hmRgb = "rgb", hmHsb = "hsb", hmHsl = "hsl"

  Light* = object
    kind*: LightKind
    x*, y*, z*: float32
    targetX*, targetY*: float32
    angle*, elevation*: float32
    color*: array[3, float32]
    intensity*, cone*, hotspot*, radius*: float32
  IrisPin* = object
    x*, y*, radiusX*, radiusY*, angle*, roundness*, feather*, blur*: float32
  FieldPin* = object
    x*, y*, blur*: float32
  SpinPin* = object
    x*, y*, radiusX*, radiusY*, angle*, blurAngle*: float32
  BlurPath* = object
    points*: seq[array[2, float32]]
    speed*, taper*: float32

  GalleryFilter* = enum
    gfColoredPencil = "coloredPencil", gfCutout = "cutout", gfDryBrush = "dryBrush", gfFilmGrain = "filmGrain",
    gfFresco = "fresco", gfNeonGlow = "neonGlow", gfPaintDaubs = "paintDaubs", gfPaletteKnife = "paletteKnife",
    gfPlasticWrap = "plasticWrap", gfPosterEdges = "posterEdges", gfRoughPastels = "roughPastels",
    gfSmudgeStick = "smudgeStick", gfSponge = "sponge", gfUnderpainting = "underpainting", gfWatercolor = "watercolor",
    gfAccentedEdges = "accentedEdges", gfAngledStrokes = "angledStrokes", gfCrosshatch = "crosshatch",
    gfDarkStrokes = "darkStrokes", gfInkOutlines = "inkOutlines", gfSpatter = "spatter", gfSprayedStrokes = "sprayedStrokes",
    gfSumiE = "sumiE", gfDiffuseGlow = "diffuseGlow", gfGlass = "glass", gfOceanRipple = "oceanRipple",
    gfBasRelief = "basRelief", gfChalkCharcoal = "chalkCharcoal", gfCharcoal = "charcoal", gfChrome = "chrome",
    gfConteCrayon = "conteCrayon", gfGraphicPen = "graphicPen", gfHalftonePattern = "halftonePattern",
    gfNotePaper = "notePaper", gfPhotocopy = "photocopy", gfPlaster = "plaster", gfReticulation = "reticulation",
    gfStamp = "stamp", gfTornEdges = "tornEdges", gfWaterPaper = "waterPaper", gfGlowingEdges = "glowingEdges",
    gfCraquelure = "craquelure", gfGrain = "grain", gfMosaicTiles = "mosaicTiles", gfPatchwork = "patchwork",
    gfStainedGlass = "stainedGlass", gfTexturizer = "texturizer"
  GalleryParamKindTag* = enum gpRange, gpChoice, gpBool
  GalleryParam* = object
    key*: string
    case kind*: GalleryParamKindTag
    of gpRange: min*, max*, default*: float32
    of gpChoice: choices*: seq[string]
    of gpBool: flag*: bool
  GalleryEffect* = object
    filter*: GalleryFilter
    values*: seq[(string, float32)] ## sorted by key (Rust BTreeMap)
    foreground*, background*, color*: array[4, float32]

  FilterKind* = enum
    fltGaussianBlur = "gaussianBlur", fltBoxBlur = "boxBlur", fltMotionBlur = "motionBlur", fltRadialBlur = "radialBlur",
    fltSurfaceBlur = "surfaceBlur", fltUnsharpMask = "unsharpMask", fltSmartSharpen = "smartSharpen",
    fltHighPass = "highPass", fltAddNoise = "addNoise", fltMedian = "median", fltDustAndScratches = "dustAndScratches",
    fltMinimum = "minimum", fltMaximum = "maximum", fltOffset = "offset", fltMosaic = "mosaic", fltEmboss = "emboss",
    fltFindEdges = "findEdges", fltSolarize = "solarize", fltInvert = "invert", fltDesaturate = "desaturate",
    fltTwirl = "twirl", fltPinch = "pinch", fltSpherize = "spherize", fltWave = "wave", fltRipple = "ripple",
    fltPolarCoordinates = "polarCoordinates", fltColorHalftone = "colorHalftone", fltCrystallize = "crystallize",
    fltFacet = "facet", fltFragment = "fragment", fltMezzotint = "mezzotint", fltPointillize = "pointillize",
    fltDiffuse = "diffuse", fltExtrude = "extrude", fltOilPaint = "oilPaint", fltTiles = "tiles",
    fltTraceContour = "traceContour", fltWind = "wind", fltDisplace = "displace", fltShear = "shear",
    fltZigZag = "zigZag", fltFibers = "fibers", fltLensFlare = "lensFlare", fltLightingEffects = "lightingEffects",
    fltRelight = "relight", fltReduceNoise = "reduceNoise", fltSmartBlur = "smartBlur", fltLensBlur = "lensBlur",
    fltShapeBlur = "shapeBlur", fltTiltShift = "tiltShift", fltIrisBlur = "irisBlur", fltFieldBlur = "fieldBlur",
    fltSpinBlur = "spinBlur", fltPathBlur = "pathBlur", fltCustom = "custom", fltHsbHsl = "hsbHsl",
    fltColorToAlpha = "colorToAlpha", fltDeInterlace = "deInterlace", fltNtscColors = "ntscColors",
    fltFilterGallery = "filterGallery"

  FilterParams* = object
    ## Every filter's parameters in one flat object; `kind` says which fields apply (see
    ## `fieldsOf`). Fields whose JSON name is shared by variants with different types carry a
    ## prefix here (`radialQuality`/`blurQuality` are both `"quality"` in JSON, and so on).
    kind*: FilterKind
    radius*, amount*, angle*, distance*, threshold*: float32
    `method`*: RadialMethod
    radialQuality*: RadialQuality
    centerX*, centerY*: float32
    reduceNoise*: float32
    distribution*: Distribution
    monochromatic*: bool
    seed*: uint32
    preserve*: Preserve
    offsetH*, offsetV*: int32
    undefined*: UndefinedAreas
    cellSize*, height*: float32
    spherizeMode*: SpherizeMode
    generators*: uint32
    wavelengthMin*, wavelengthMax*, amplitudeMin*, amplitudeMax*: float32
    waveType*: WaveType
    rippleSize*: RippleSize
    polarMode*: PolarMode
    maxRadius*: float32
    angles*: array[4, float32]
    mezzotintKind*: MezzotintType
    background*, foreground*: array[4, float32]
    diffuseMode*: DiffuseMode
    extrudeKind*: ExtrudeType
    size*, depth*: float32
    levelBased*, solidFront*, maskIncomplete*: bool
    stylization*, cleanliness*, scale*, bristleDetail*: float32
    lighting*: bool
    shine*: float32
    count*: uint32
    maxOffset*: float32
    fill*: TileFill
    level*: float32
    upper*: bool
    windMethod*: WindMethod
    fromRight*: bool
    horizontal*, vertical*: float32
    stretch*: bool
    map*: ref Image          ## Displace map (not serialized)
    points*: seq[array[2, float32]]
    ridges*: float32
    zigzagStyle*: ZigZagStyle
    variance*, strength*: float32
    brightness*: float32
    lens*: LensType
    lights*: seq[Light]
    gloss*, metallic*, exposure*, ambience*: float32
    texture*: TextureChannel
    whiteIsHigh*: bool
    elevation*, intensity*, ambient*, warmth*, softness*: float32
    preserveDetails*, reduceColorNoise*, sharpenDetails*: float32
    removeJpegArtifact*: bool
    blurQuality*: BlurQuality
    smartBlurMode*: SmartBlurMode
    blades*: uint32
    curvature*, rotation*: float32
    depthSource*: DepthSource
    focalDistance*: float32
    invertDepth*: bool
    noise*: float32
    depthMap*: ref Image     ## Lens Blur depth map (not serialized)
    shape*: BlurShape
    blur*, focus*, transition*: float32
    irisPins*: seq[IrisPin]
    fieldPins*: seq[FieldPin]
    spinPins*: seq[SpinPin]
    paths*: seq[BlurPath]
    kernel*: seq[float32]
    offset*: float32
    input*, output*: HsbModel
    color*: array[4, float32]
    transparencyThreshold*, opacityThreshold*: float32
    eliminateEven*, interpolate*: bool
    effects*: seq[GalleryEffect]

proc radiusHalo*(r: int): Halo = Halo(bounds: false, radius: r)
const BoundsHalo* = Halo(bounds: true)

# ---------------------------------------------------------------- defaults and JSON of the parts

proc defaultLight*(): Light =
  Light(kind: lkSpot, x: 0.25, y: 0.2, z: 0.6, targetX: 0.5, targetY: 0.55, angle: 135, elevation: 45,
        color: [1'f32, 1, 1], intensity: 75, cone: 45, hotspot: 50, radius: 0.8)
proc defaultIrisPin*(): IrisPin = IrisPin(x: 0.5, y: 0.5, radiusX: 0.35, radiusY: 0.25, feather: 0.5, blur: 15)
proc defaultFieldPin*(): FieldPin = FieldPin(x: 0.5, y: 0.5, blur: 15)
proc defaultSpinPin*(): SpinPin = SpinPin(x: 0.5, y: 0.5, radiusX: 0.3, radiusY: 0.3, blurAngle: 15)
proc defaultBlurPath*(): BlurPath = BlurPath(points: @[[0.2'f32, 0.5], [0.8'f32, 0.5]], speed: 50)
proc defaultOf*(_: typedesc[Light]): Light = defaultLight()
proc defaultOf*(_: typedesc[IrisPin]): IrisPin = defaultIrisPin()
proc defaultOf*(_: typedesc[FieldPin]): FieldPin = defaultFieldPin()
proc defaultOf*(_: typedesc[SpinPin]): SpinPin = defaultSpinPin()
proc defaultOf*(_: typedesc[BlurPath]): BlurPath = defaultBlurPath()
proc serdeCamel*(_: typedesc[Light]): bool = true
proc serdeCamel*(_: typedesc[IrisPin]): bool = true
proc serdeCamel*(_: typedesc[FieldPin]): bool = true
proc serdeCamel*(_: typedesc[SpinPin]): bool = true
proc serdeCamel*(_: typedesc[BlurPath]): bool = true

# ---------------------------------------------------------------- Filter Gallery catalogue

type GalleryInfo = tuple[key, name, category, doc: string]

const GalleryTable: array[GalleryFilter, GalleryInfo] = [
  ("coloredPencil", "Colored Pencil", "Artistic", """{"pencilWidth":1..24=4,"strokePressure":0..15=8,"paperBrightness":0..50=25}"""),
  ("cutout", "Cutout", "Artistic", """{"numberOfLevels":2..8=4,"edgeSimplicity":0..10=4,"edgeFidelity":1..3=2}"""),
  ("dryBrush", "Dry Brush", "Artistic", """{"brushSize":0..10=2,"brushDetail":0..10=8,"texture":1..3=1}"""),
  ("filmGrain", "Film Grain", "Artistic", """{"grain":0..20=4,"highlightArea":0..20=0,"intensity":0..10=10}"""),
  ("fresco", "Fresco", "Artistic", """{"brushSize":0..10=2,"brushDetail":0..10=8,"texture":1..3=1}"""),
  ("neonGlow", "Neon Glow", "Artistic", """{"glowSize":-24..24=5,"glowBrightness":0..50=15,"glowColor":json}"""),
  ("paintDaubs", "Paint Daubs", "Artistic", """{"brushSize":1..50=8,"sharpness":0..40=7,"brushType":"simple|lightRough|darkRough|wideSharp|wideBlurry|sparkle"}"""),
  ("paletteKnife", "Palette Knife", "Artistic", """{"strokeSize":1..50=25,"strokeDetail":1..3=3,"softness":0..10=0}"""),
  ("plasticWrap", "Plastic Wrap", "Artistic", """{"highlightStrength":0..20=15,"detail":1..15=9,"smoothness":1..15=7}"""),
  ("posterEdges", "Poster Edges", "Artistic", """{"edgeThickness":0..10=2,"edgeIntensity":0..10=1,"posterization":0..6=2}"""),
  ("roughPastels", "Rough Pastels", "Artistic", """{"strokeLength":0..40=6,"strokeDetail":1..20=4,"texture":"canvas|brick|burlap|sandstone","scaling":50..200=100,"relief":0..50=20,"light":"bottom|bottomLeft|left|topLeft|top|topRight|right|bottomRight","invert":bool}"""),
  ("smudgeStick", "Smudge Stick", "Artistic", """{"strokeLength":0..10=2,"highlightArea":0..20=0,"intensity":0..10=10}"""),
  ("sponge", "Sponge", "Artistic", """{"brushSize":0..10=2,"definition":0..25=12,"smoothness":1..15=5}"""),
  ("underpainting", "Underpainting", "Artistic", """{"brushSize":0..40=6,"textureCoverage":0..40=16,"texture":"canvas|brick|burlap|sandstone","scaling":50..200=100,"relief":0..50=4,"light":"top|topRight|right|bottomRight|bottom|bottomLeft|left|topLeft","invert":bool}"""),
  ("watercolor", "Watercolor", "Artistic", """{"brushDetail":1..14=9,"shadowIntensity":0..10=1,"texture":1..3=1}"""),
  ("accentedEdges", "Accented Edges", "Brush Strokes", """{"edgeWidth":1..14=2,"edgeBrightness":0..50=38,"smoothness":1..15=5}"""),
  ("angledStrokes", "Angled Strokes", "Brush Strokes", """{"directionBalance":0..100=50,"strokeLength":3..50=15,"sharpness":0..10=3}"""),
  ("crosshatch", "Crosshatch", "Brush Strokes", """{"strokeLength":3..50=9,"sharpness":0..20=6,"strength":1..3=1}"""),
  ("darkStrokes", "Dark Strokes", "Brush Strokes", """{"balance":0..10=5,"blackIntensity":0..10=6,"whiteIntensity":0..10=2}"""),
  ("inkOutlines", "Ink Outlines", "Brush Strokes", """{"strokeLength":1..50=4,"darkIntensity":0..50=20,"lightIntensity":0..50=10}"""),
  ("spatter", "Spatter", "Brush Strokes", """{"sprayRadius":0..25=10,"smoothness":1..15=5}"""),
  ("sprayedStrokes", "Sprayed Strokes", "Brush Strokes", """{"strokeLength":0..20=12,"sprayRadius":0..25=7,"strokeDirection":"rightDiagonal|horizontal|leftDiagonal|vertical"}"""),
  ("sumiE", "Sumi-e", "Brush Strokes", """{"strokeWidth":3..15=10,"strokePressure":0..15=2,"contrast":0..40=16}"""),
  ("diffuseGlow", "Diffuse Glow", "Distort", """{"graininess":0..10=6,"glowAmount":0..20=10,"clearAmount":0..20=15,"background":json}"""),
  ("glass", "Glass", "Distort", """{"distortion":0..20=5,"smoothness":1..15=3,"texture":"frosted|blocks|canvas|tinyLens","scaling":50..200=100,"invert":bool}"""),
  ("oceanRipple", "Ocean Ripple", "Distort", """{"rippleSize":1..15=9,"rippleMagnitude":0..20=9}"""),
  ("basRelief", "Bas Relief", "Sketch", """{"detail":1..15=13,"smoothness":1..15=3,"light":"bottom|bottomLeft|left|topLeft|top|topRight|right|bottomRight","foreground":json,"background":json}"""),
  ("chalkCharcoal", "Chalk & Charcoal", "Sketch", """{"charcoalArea":0..20=6,"chalkArea":0..20=6,"strokePressure":0..5=1,"foreground":json,"background":json}"""),
  ("charcoal", "Charcoal", "Sketch", """{"charcoalThickness":1..7=1,"detail":0..5=5,"lightDarkBalance":0..100=50,"foreground":json,"background":json}"""),
  ("chrome", "Chrome", "Sketch", """{"detail":0..10=4,"smoothness":0..10=7}"""),
  ("conteCrayon", "Conté Crayon", "Sketch", """{"foregroundLevel":1..15=11,"backgroundLevel":1..15=7,"texture":"canvas|brick|burlap|sandstone","scaling":50..200=100,"relief":0..50=4,"light":"top|topRight|right|bottomRight|bottom|bottomLeft|left|topLeft","invert":bool,"foreground":json,"background":json}"""),
  ("graphicPen", "Graphic Pen", "Sketch", """{"strokeLength":1..15=15,"lightDarkBalance":0..100=50,"strokeDirection":"rightDiagonal|horizontal|leftDiagonal|vertical","foreground":json,"background":json}"""),
  ("halftonePattern", "Halftone Pattern", "Sketch", """{"size":1..12=1,"contrast":0..50=5,"patternType":"dot|circle|line","foreground":json,"background":json}"""),
  ("notePaper", "Note Paper", "Sketch", """{"imageBalance":0..50=25,"graininess":0..20=10,"relief":0..25=11,"foreground":json,"background":json}"""),
  ("photocopy", "Photocopy", "Sketch", """{"detail":1..24=7,"darkness":1..50=8,"foreground":json,"background":json}"""),
  ("plaster", "Plaster", "Sketch", """{"imageBalance":0..50=20,"smoothness":1..15=2,"light":"top|topRight|right|bottomRight|bottom|bottomLeft|left|topLeft","foreground":json,"background":json}"""),
  ("reticulation", "Reticulation", "Sketch", """{"density":0..50=12,"foregroundLevel":0..50=40,"backgroundLevel":0..50=5,"foreground":json,"background":json}"""),
  ("stamp", "Stamp", "Sketch", """{"lightDarkBalance":0..50=25,"smoothness":1..50=5,"foreground":json,"background":json}"""),
  ("tornEdges", "Torn Edges", "Sketch", """{"imageBalance":0..50=25,"smoothness":1..15=11,"contrast":1..25=17,"foreground":json,"background":json}"""),
  ("waterPaper", "Water Paper", "Sketch", """{"fiberLength":3..50=15,"brightness":0..100=60,"contrast":0..100=80}"""),
  ("glowingEdges", "Glowing Edges", "Stylize", """{"edgeWidth":1..14=2,"edgeBrightness":0..20=6,"smoothness":1..15=5}"""),
  ("craquelure", "Craquelure", "Texture", """{"crackSpacing":2..100=15,"crackDepth":0..10=6,"crackBrightness":0..10=9}"""),
  ("grain", "Grain", "Texture", """{"intensity":0..100=40,"contrast":0..100=50,"grainType":"regular|soft|sprinkles|clumped|contrasty|enlarged|stippled|horizontal|vertical|speckle","foreground":json,"background":json}"""),
  ("mosaicTiles", "Mosaic Tiles", "Texture", """{"tileSize":2..100=12,"groutWidth":1..15=3,"lightenGrout":0..10=9}"""),
  ("patchwork", "Patchwork", "Texture", """{"squareSize":0..10=4,"relief":0..25=8}"""),
  ("stainedGlass", "Stained Glass", "Texture", """{"cellSize":2..50=10,"borderThickness":1..20=4,"lightIntensity":0..10=3,"foreground":json}"""),
  ("texturizer", "Texturizer", "Texture", """{"texture":"canvas|brick|burlap|sandstone","scaling":50..200=100,"relief":0..50=4,"light":"top|topRight|right|bottomRight|bottom|bottomLeft|left|topLeft","invert":bool}"""),
]

const
  GalleryCategories* = ["Artistic", "Brush Strokes", "Distort", "Sketch", "Stylize", "Texture"]
  NeonDefault* = [0'f32, 0.62, 1, 1]

proc key*(f: GalleryFilter): string = GalleryTable[f].key
proc commandId*(f: GalleryFilter): string = "filter.gallery." & GalleryTable[f].key
proc name*(f: GalleryFilter): string = GalleryTable[f].name
proc category*(f: GalleryFilter): string = GalleryTable[f].category
proc paramsDoc*(f: GalleryFilter): string = GalleryTable[f].doc

proc galleryFromKey*(k0: string): Option[GalleryFilter] =
  let k = (if k0.startsWith("filter.gallery."): k0["filter.gallery.".len .. ^1] else: k0)
  for f in GalleryFilter:
    if f.key == k or cmpIgnoreCase(f.name, k) == 0: return some(f)

proc params*(f: GalleryFilter): seq[GalleryParam] =
  let inner = f.paramsDoc.strip.strip(chars = {'{', '}'})
  for part in inner.split(','):
    let i = part.find(':')
    if i < 0: continue
    let key = part[0 ..< i].strip.strip(chars = {'"'})
    let v = part[i + 1 .. ^1].strip
    if v.startsWith("\""):
      result.add GalleryParam(key: key, kind: gpChoice, choices: v.strip(chars = {'"'}).split('|'))
    elif v == "bool":
      result.add GalleryParam(key: key, kind: gpBool, flag: false)
    elif '=' in v:
      let (range, def) = (v.split('=')[0], v.split('=')[1])
      let rs = range.split("..")
      proc p(s: string): float32 =
        try: float32(parseFloat(s.strip)) except ValueError: 0
      let (lo, hi) = (if rs.len == 2: (rs[0], rs[1]) else: ("0", "1"))
      result.add GalleryParam(key: key, kind: gpRange, min: p(lo), max: p(hi), default: p(def))

proc usesColours*(f: GalleryFilter): bool =
  f in {gfColoredPencil, gfNeonGlow} or "\"foreground\"" in f.paramsDoc or "\"background\"" in f.paramsDoc

proc putValue(e: var GalleryEffect; key: string; v: float32) =
  for kv in e.values.mitems:
    if kv[0] == key:
      kv[1] = v
      return
  e.values.add (key, v)
  e.values.sort(proc(a, b: (string, float32)): int = cmp(a[0], b[0]))

proc newGalleryEffect*(f: GalleryFilter): GalleryEffect =
  result = GalleryEffect(filter: f, foreground: [0'f32, 0, 0, 1], background: [1'f32, 1, 1, 1], color: NeonDefault)
  for p in f.params:
    let v = (case p.kind
      of gpRange: p.default
      of gpChoice: 0'f32
      of gpBool: (if p.flag: 1'f32 else: 0'f32))
    result.putValue(p.key, v)

proc set*(e: var GalleryEffect; key: string; v: float32) =
  for p in e.filter.params:
    if p.key == key:
      let x = (case p.kind
        of gpRange: clamp(v, p.min, p.max)
        of gpChoice: clamp(round(v), 0'f32, float32(p.choices.len - 1))
        of gpBool: (if v != 0: 1'f32 else: 0'f32))
      e.putValue(key, x)
      return

proc setChoice*(e: var GalleryEffect; key, name: string): bool =
  for p in e.filter.params:
    if p.key == key and p.kind == gpChoice:
      for i, n in p.choices:
        if cmpIgnoreCase(n, name) == 0:
          e.putValue(key, float32(i))
          return true
      return false

proc get*(e: GalleryEffect; key: string): float32 =
  for (k, v) in e.values:
    if k == key: return v
  for p in e.filter.params:
    if p.key == key:
      return (case p.kind
        of gpRange: p.default
        of gpBool: (if p.flag: 1'f32 else: 0'f32)
        of gpChoice: 0'f32)

proc flag*(e: GalleryEffect; key: string): bool = e.get(key) != 0

proc choice*(e: GalleryEffect; key: string): string =
  for p in e.filter.params:
    if p.key == key and p.kind == gpChoice:
      let i = int(max(e.get(key), 0))
      return (if i < p.choices.len: p.choices[i] else: p.choices[0])
  ""

proc toJ*(e: GalleryEffect): JsonNode =
  result = %*{"filter": $e.filter}
  var vals = newJObject()
  for (k, v) in e.values: vals[k] = toJ(v)
  result["values"] = vals
  result["foreground"] = toJ(e.foreground)
  result["background"] = toJ(e.background)
  result["color"] = toJ(e.color)

proc fromJ*(n: JsonNode; e: var GalleryEffect) =
  if n.kind != JObject: raise newException(SerdeError, "gallery effect: expected an object")
  e = GalleryEffect(foreground: [0'f32, 0, 0, 1], background: [1'f32, 1, 1, 1], color: NeonDefault)
  if not n.hasKey("filter"): raise newException(SerdeError, "gallery effect: missing filter")
  fromJ(n["filter"], e.filter)
  if n.hasKey("values") and n["values"].kind == JObject:
    for k, v in n["values"]:
      var f: float32
      fromJ(v, f)
      e.putValue(k, f)
  if n.hasKey("foreground"): fromJ(n["foreground"], e.foreground)
  if n.hasKey("background"): fromJ(n["background"], e.background)
  if n.hasKey("color"): fromJ(n["color"], e.color)

proc defaultOf*(_: typedesc[GalleryEffect]): GalleryEffect = newGalleryEffect(gfColoredPencil)

# ---------------------------------------------------------------- FilterParams

const FieldsOf: array[FilterKind, seq[string]] = [
  @["radius"], @["radius"], @["angle", "distance"],
  @["amount", "method", "radialQuality", "centerX", "centerY"],
  @["radius", "threshold"], @["amount", "radius", "threshold"], @["amount", "radius", "reduceNoise"],
  @["radius"], @["amount", "distribution", "monochromatic", "seed"], @["radius"], @["radius", "threshold"],
  @["radius", "preserve"], @["radius", "preserve"], @["offsetH", "offsetV", "undefined"], @["cellSize"],
  @["angle", "height", "amount"], @[], @[], @[], @[], @["angle"], @["amount"], @["amount", "spherizeMode"],
  @["generators", "wavelengthMin", "wavelengthMax", "amplitudeMin", "amplitudeMax", "waveType", "undefined", "seed"],
  @["amount", "rippleSize"], @["polarMode"], @["maxRadius", "angles"], @["cellSize", "seed"], @[], @[],
  @["mezzotintKind", "seed"], @["cellSize", "seed", "background"], @["diffuseMode", "seed"],
  @["extrudeKind", "size", "depth", "levelBased", "solidFront", "maskIncomplete", "seed"],
  @["stylization", "cleanliness", "scale", "bristleDetail", "lighting", "angle", "shine"],
  @["count", "maxOffset", "fill", "foreground", "background", "seed"], @["level", "upper"],
  @["windMethod", "fromRight", "seed"], @["horizontal", "vertical", "stretch", "undefined"],
  @["points", "undefined"], @["amount", "ridges", "zigzagStyle"],
  @["variance", "strength", "seed", "foreground", "background"], @["brightness", "centerX", "centerY", "lens"],
  @["lights", "gloss", "metallic", "exposure", "ambience", "texture", "height", "whiteIsHigh"],
  @["angle", "elevation", "intensity", "ambient", "warmth", "softness"],
  @["strength", "preserveDetails", "reduceColorNoise", "sharpenDetails", "removeJpegArtifact"],
  @["radius", "threshold", "blurQuality", "smartBlurMode"],
  @["radius", "blades", "curvature", "rotation", "depthSource", "focalDistance", "invertDepth", "brightness",
    "threshold", "noise", "distribution", "monochromatic", "seed"],
  @["radius", "shape"], @["blur", "centerX", "centerY", "angle", "focus", "transition"],
  @["irisPins"], @["fieldPins"], @["spinPins"], @["paths"], @["kernel", "scale", "offset"], @["input", "output"],
  @["color", "transparencyThreshold", "opacityThreshold"], @["eliminateEven", "interpolate"], @[], @["effects"],
]

proc fieldsOf*(k: FilterKind): seq[string] = FieldsOf[k]

proc jsonName*(field: string): string =
  ## The serde (camelCase) name of a `FilterParams` field.
  case field
  of "radialQuality", "blurQuality": "quality"
  of "offsetH": "horizontal"
  of "offsetV": "vertical"
  of "spherizeMode", "polarMode", "diffuseMode", "smartBlurMode": "mode"
  of "rippleSize": "size"
  of "mezzotintKind", "extrudeKind": "kind"
  of "windMethod": "method"
  of "zigzagStyle": "style"
  of "depthSource": "depth"
  of "irisPins", "fieldPins", "spinPins": "pins"
  else: field

proc newFilterParams*(kind: FilterKind): FilterParams =
  ## A filter's parameters with the serde defaults of its optional fields.
  FilterParams(kind: kind, radialQuality: rqGood, preserve: prSquareness, rippleSize: rsMedium,
               zigzagStyle: zzPondRipples, blurQuality: bqMedium)

proc `==`*(a, b: FilterParams): bool =
  ## Parameters are equal when the fields of their filter are (maps compare by identity).
  if a.kind != b.kind: return false
  let fields = FieldsOf[a.kind]
  for name, va, vb in fieldPairs(a, b):
    when name notin ["kind"]:
      if name in fields and va != vb: return false
  if a.kind == fltDisplace and a.map != b.map: return false
  if a.kind == fltLensBlur and a.depthMap != b.depthMap: return false
  true

proc toJ*(p: FilterParams): JsonNode =
  result = newJObject()
  result["filter"] = newJString($p.kind)
  let fields = FieldsOf[p.kind]
  for name, v in fieldPairs(p):
    when name notin ["kind", "map", "depthMap"]:
      if name in fields: result[jsonName(name)] = toJ(v)

proc fromJ*(n: JsonNode; p: var FilterParams) =
  if n.kind != JObject or not n.hasKey("filter"): raise newException(SerdeError, "filter params: expected {\"filter\": ...}")
  var kind: FilterKind
  fromJ(n["filter"], kind)
  p = newFilterParams(kind)
  let fields = FieldsOf[kind]
  for name, v in fieldPairs(p):
    when name notin ["kind", "map", "depthMap"]:
      if name in fields:
        let j = jsonName(name)
        if n.hasKey(j): fromJ(n[j], v)

proc label*(p: FilterParams): string =
  ## The filter's menu name.
  case p.kind
  of fltGaussianBlur: "Gaussian Blur"
  of fltBoxBlur: "Box Blur"
  of fltMotionBlur: "Motion Blur"
  of fltRadialBlur: "Radial Blur"
  of fltSurfaceBlur: "Surface Blur"
  of fltUnsharpMask: "Unsharp Mask"
  of fltSmartSharpen: "Smart Sharpen"
  of fltHighPass: "High Pass"
  of fltAddNoise: "Add Noise"
  of fltMedian: "Median"
  of fltDustAndScratches: "Dust & Scratches"
  of fltMinimum: "Minimum"
  of fltMaximum: "Maximum"
  of fltOffset: "Offset"
  of fltMosaic: "Mosaic"
  of fltEmboss: "Emboss"
  of fltFindEdges: "Find Edges"
  of fltSolarize: "Solarize"
  of fltInvert: "Invert"
  of fltDesaturate: "Desaturate"
  of fltTwirl: "Twirl"
  of fltPinch: "Pinch"
  of fltSpherize: "Spherize"
  of fltWave: "Wave"
  of fltRipple: "Ripple"
  of fltPolarCoordinates: "Polar Coordinates"
  of fltColorHalftone: "Color Halftone"
  of fltCrystallize: "Crystallize"
  of fltFacet: "Facet"
  of fltFragment: "Fragment"
  of fltMezzotint: "Mezzotint"
  of fltPointillize: "Pointillize"
  of fltDiffuse: "Diffuse"
  of fltExtrude: "Extrude"
  of fltOilPaint: "Oil Paint"
  of fltTiles: "Tiles"
  of fltTraceContour: "Trace Contour"
  of fltWind: "Wind"
  of fltDisplace: "Displace"
  of fltShear: "Shear"
  of fltZigZag: "ZigZag"
  of fltFibers: "Fibers"
  of fltLensFlare: "Lens Flare"
  of fltLightingEffects: "Lighting Effects"
  of fltRelight: "Relight"
  of fltReduceNoise: "Reduce Noise"
  of fltSmartBlur: "Smart Blur"
  of fltLensBlur: "Lens Blur"
  of fltShapeBlur: "Shape Blur"
  of fltTiltShift: "Tilt-Shift"
  of fltIrisBlur: "Iris Blur"
  of fltFieldBlur: "Field Blur"
  of fltSpinBlur: "Spin Blur"
  of fltPathBlur: "Path Blur"
  of fltCustom: "Custom"
  of fltHsbHsl: "HSB/HSL"
  of fltColorToAlpha: "Color to Alpha"
  of fltDeInterlace: "De-Interlace"
  of fltNtscColors: "NTSC Colors"
  of fltFilterGallery: (if p.effects.len == 1: p.effects[0].filter.name else: "Filter Gallery")

proc makesTransparency*(p: FilterParams): bool = p.kind == fltColorToAlpha
