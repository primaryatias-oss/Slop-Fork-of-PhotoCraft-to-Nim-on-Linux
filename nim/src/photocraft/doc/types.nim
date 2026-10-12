## The PhotoCraft document model types: pure data, no rendering, no UI.
##
## Layer order: inside every group (and the root), `children[0]` is the bottom layer, matching
## compositing order and the PSD file order. UIs display the list reversed.
##
## Rust enums that carry data are flat objects here: a `kind` field plus the fields of every
## variant (only the active variant's fields are meaningful). This keeps value semantics,
## structural equality and in-place kind changes simple.

import std/[json, options]
import ../geom, ../geom/warp, ../color, ../raster, ../serde
export geom, warp, color, raster, json, options, serde

type
  SharedBytes* = ref object
    ## Immutable shared bytes (Rust `Arc<Vec<u8>>`): clones of a document share them.
    data*: seq[byte]
  SharedF32* = ref object
    data*: seq[float32]

  LayerId* = distinct uint64
  SmartContentsId* = distinct uint64
  DocId* = distinct uint64

  Locks* = object
    transparency*, pixels*, position*, artboard*, all*: bool

  LabelColor* = enum
    lcNone = "None", lcRed = "Red", lcOrange = "Orange", lcYellow = "Yellow", lcGreen = "Green",
    lcBlue = "Blue", lcViolet = "Violet", lcGray = "Gray", lcSeafoam = "Seafoam",
    lcIndigo = "Indigo", lcMagenta = "Magenta", lcFuchsia = "Fuchsia"

  # ---------------------------------------------------------------- adjustments

  CurvePoint* = object
    input*, output*: float32
  LevelsChannel* = object
    inBlack*, inWhite*, gamma*, outBlack*, outWhite*: float32
  ToneSpace* = enum tsRgb = "Rgb", tsCmyk = "Cmyk", tsLab = "Lab"
  HueRange* = object
    hue*, saturation*, lightness*: float32
    bounds*: array[4, float32]
  AdjustmentKind* = enum
    akBrightnessContrast = "BrightnessContrast", akLevels = "Levels", akCurves = "Curves",
    akExposure = "Exposure", akVibrance = "Vibrance", akHueSaturation = "HueSaturation",
    akColorBalance = "ColorBalance", akBlackWhite = "BlackWhite", akPhotoFilter = "PhotoFilter",
    akChannelMixer = "ChannelMixer", akColorLookup = "ColorLookup", akInvert = "Invert",
    akPosterize = "Posterize", akThreshold = "Threshold", akGradientMap = "GradientMap",
    akSelectiveColor = "SelectiveColor", akUnsupported = "Unsupported"
  Adjustment* = object
    kind*: AdjustmentKind
    brightness*, contrast*: float32           ## BrightnessContrast
    legacy*: bool
    levelsMaster*: LevelsChannel              ## Levels
    levelsPerChannel*: array[3, LevelsChannel]
    levelsBlack*: LevelsChannel
    space*: ToneSpace                         ## Levels, Curves
    curvesMaster*: seq[CurvePoint]            ## Curves
    curvesPerChannel*: array[3, seq[CurvePoint]]
    curvesBlack*: seq[CurvePoint]
    pencil*: bool
    exposure*, offset*, gamma*: float32       ## Exposure
    vibrance*: float32                        ## Vibrance (+ saturation)
    hue*, saturation*, lightness*: float32    ## HueSaturation (saturation shared with Vibrance)
    colorize*: bool
    ranges*: array[6, HueRange]
    shadows*, midtones*, highlights*: array[3, float32]  ## ColorBalance
    preserveLuminosity*: bool                 ## ColorBalance, PhotoFilter
    weights*: array[6, float32]               ## BlackWhite
    tint*: Option[array[3, float32]]
    filterColor*: array[3, float32]           ## PhotoFilter
    density*: float32
    matrix*: array[3, array[4, float32]]      ## ChannelMixer
    monochrome*: bool
    lutName*: string                          ## ColorLookup
    lut*: SharedF32
    lutSize*: uint32
    tetrahedral*: bool
    dither*: bool                             ## ColorLookup, GradientMap
    levels*: uint32                           ## Posterize
    level*: float32                           ## Threshold
    gmStops*: seq[(float32, array[3, float32])]  ## GradientMap
    reverse*: bool
    relative*: bool                           ## SelectiveColor
    selective*: array[9, array[4, float32]]
    psdKey*: string                           ## Unsupported
    raw*: seq[byte]

  # ---------------------------------------------------------------- effects

  GradientStyle* = enum
    gsLinear = "Linear", gsRadial = "Radial", gsAngle = "Angle", gsReflected = "Reflected", gsDiamond = "Diamond"
  Gradient* = object
    stops*: seq[(float32, Color)]
    opacityStops*: seq[(float32, float32)]
    style*: GradientStyle
    angle*, scale*: float32
    reverse*, align*: bool
    offset*: (float32, float32)
  ContourKind* = enum ckLinear = "Linear", ckCustom = "Custom"
  Contour* = object
    kind*: ContourKind
    name*: string
    points*: seq[CurvePoint]
    corners*: seq[bool]
  FxPaintKind* = enum fpColor = "Color", fpGradient = "Gradient", fpPattern = "Pattern"
  FxPaint* = object
    kind*: FxPaintKind
    color*: Color
    gradient*: Gradient
    name*, id*: string
    scale*: float32
  FxCommon* = object
    enabled*: bool
    blend*: BlendMode
    opacity*: float32
  Shadow* = object
    common*: FxCommon
    color*: Color
    angle*: float32
    useGlobalLight*: bool
    distance*, spread*, size*: float32
    contour*: Contour
    antiAlias*: bool
    noise*: float32
    knocksOut*: bool
  GlowTechnique* = enum gtSofter = "Softer", gtPrecise = "Precise"
  GlowSource* = enum gsCenter = "Center", gsEdge = "Edge"
  Glow* = object
    common*: FxCommon
    paint*: FxPaint
    technique*: GlowTechnique
    spread*, size*: float32
    contour*: Contour
    antiAlias*: bool
    range*, jitter*, noise*: float32
    source*: GlowSource
  StrokePosition* = enum spOutside = "Outside", spInside = "Inside", spCenter = "Center"
  StrokeFx* = object
    common*: FxCommon
    size*: float32
    position*: StrokePosition
    paint*: FxPaint
  Satin* = object
    common*: FxCommon
    color*: Color
    angle*, distance*, size*: float32
    contour*: Contour
    antiAlias*, invert*: bool
  BevelStyle* = enum
    bsOuterBevel = "OuterBevel", bsInnerBevel = "InnerBevel", bsEmboss = "Emboss",
    bsPillowEmboss = "PillowEmboss", bsStrokeEmboss = "StrokeEmboss"
  BevelTechnique* = enum btSmooth = "Smooth", btChiselHard = "ChiselHard", btChiselSoft = "ChiselSoft"
  BevelContour* = object
    contour*: Contour
    range*: float32
    antiAlias*: bool
  BevelTexture* = object
    name*, id*: string
    scale*, depth*: float32
    invert*, link*: bool
    phase*: (float32, float32)
  Bevel* = object
    enabled*: bool
    style*: BevelStyle
    technique*: BevelTechnique
    depth*: float32
    up*: bool
    size*, soften*, angle*, altitude*: float32
    useGlobalLight*: bool
    glossContour*: Contour
    highlight*: FxCommon
    highlightColor*: Color
    shadow*: FxCommon
    shadowColor*: Color
    contour*: Option[BevelContour]
    texture*: Option[BevelTexture]
  EffectKind* = enum
    ekDropShadow = "DropShadow", ekInnerShadow = "InnerShadow", ekOuterGlow = "OuterGlow",
    ekInnerGlow = "InnerGlow", ekStroke = "Stroke", ekColorOverlay = "ColorOverlay",
    ekGradientOverlay = "GradientOverlay", ekPatternOverlay = "PatternOverlay", ekSatin = "Satin",
    ekBevelEmboss = "BevelEmboss"
  Effect* = object
    kind*: EffectKind
    shadow*: Shadow       ## DropShadow, InnerShadow
    glow*: Glow           ## OuterGlow, InnerGlow
    stroke*: StrokeFx     ## Stroke
    common*: FxCommon     ## ColorOverlay, GradientOverlay, PatternOverlay
    color*: Color         ## ColorOverlay
    gradient*: Gradient   ## GradientOverlay
    dither*: bool
    name*, id*: string    ## PatternOverlay
    scale*, angle*: float32
    link*: bool
    phase*: (float32, float32)
    satin*: Satin         ## Satin
    bevel*: Bevel         ## BevelEmboss
  GlobalLight* = object
    angle*, altitude*: float32
  Effects* = object
    enabled*: bool
    items*: seq[Effect]
    psdRaw*: SharedBytes             ## original `lfx2`/`lfxs` data, written back verbatim
    reference*: Option[(float64, float64)]

  # ---------------------------------------------------------------- blending options

  Knockout* = enum koNone = "none", koShallow = "shallow", koDeep = "deep"
  AdvancedBlending* = object
    knockout*: Knockout
    blendInterior*, blendClipped*, transparencyShapes*: bool
    layerMaskHidesEffects*, vectorMaskHidesEffects*: bool
  BlendRange* = object
    black*, white*: array[2, uint8]
  BlendIf* = object
    ranges*: seq[array[2, BlendRange]]

  # ---------------------------------------------------------------- vectors

  Knot* = object
    anchor*, inCtrl*, outCtrl*: Point
    smooth*: bool
  PathOp* = enum poCombine = "Combine", poSubtract = "Subtract", poIntersect = "Intersect", poExclude = "Exclude", poJoin = "Join"
  FillRule* = enum frNonZero = "NonZero", frEvenOdd = "EvenOdd"
  Subpath* = object
    closed*: bool
    knots*: seq[Knot]
    op*: PathOp
  Path* = object
    subpaths*: seq[Subpath]
    fillRule*: FillRule
    inverted*: bool
  NamedPath* = object
    name*: string
    path*: Path
    psdRaw*: SharedBytes
  ClippingPath* = object
    name*: string
    flatness*: float32
  VectorMask* = object
    path*: Path
    enabled*, linked*: bool
    density*, feather*: float32
  StrokeAlign* = enum saInside = "Inside", saCenter = "Center", saOutside = "Outside"
  LineCap* = enum lcapButt = "Butt", lcapRound = "Round", lcapSquare = "Square"
  LineJoin* = enum ljMiter = "Miter", ljRound = "Round", ljBevel = "Bevel"
  LiveShapeKind* = enum lsRect = "rect", lsEllipse = "ellipse", lsPolygon = "polygon", lsLine = "line"
  LiveShape* = object
    kind*: LiveShapeKind
    rect*: array[4, float64]   ## Rect, Ellipse, Polygon
    radii*: array[4, float64]  ## Rect
    sides*: uint32             ## Polygon
    starRatio*: float64
    `from`*, to*: array[2, float64]  ## Line
    weight*: float64

  # ---------------------------------------------------------------- fills

  FillKind* = enum fkSolid = "Solid", fkGradient = "Gradient", fkPattern = "Pattern"
  Fill* = object
    kind*: FillKind
    color*: Color                         ## Solid
    stops*: seq[(float32, Color)]         ## Gradient
    angle*, scale*: float32               ## Gradient, Pattern
    style*: GradientStyle
    reverse*: bool
    opacityStops*: seq[(float32, float32)]
    midpoints*: seq[float32]
    offset*: (float32, float32)
    dither*, align*: bool
    name*, id*: string                    ## Pattern
    link*: bool
    phase*: (float32, float32)
  FillCache* = object
    fill*: Fill
    surface*: Surface

  ShapeStroke* = object
    width*: float32
    paint*: Fill
    opacity*: float32
    align*: StrokeAlign
    cap*: LineCap
    join*: LineJoin
    miterLimit*: float32
    dashes*: seq[float32]
    dashOffset*: float32
  ShapeLayer* = object
    path*: Path
    fill*: Option[Fill]
    stroke*: Option[ShapeStroke]
    live*: Option[LiveShape]
    cache*: Option[Surface]
    psdRaw*: SharedBytes

  # ---------------------------------------------------------------- text

  Kerning* = enum kMetrics = "Metrics", kOptical = "Optical", kOff = "Off"
  Caps* = enum capsNormal = "Normal", capsSmallCaps = "SmallCaps", capsAllCaps = "AllCaps"
  FontFeature* = object
    tag*: string
    value*: uint16
  FontVariation* = object
    axis*: string
    value*: float32
  CharStyle* = object
    fontFamily*, fontStyle*: string
    postscriptName*: Option[string]
    weight*: uint16
    italic*: bool
    sizePt*: float32
    color*: Color
    tracking*: float32
    leadingPt*: Option[float32]
    baselineShiftPt*, horizontalScale*, verticalScale*: float32
    underline*, strikethrough*, fauxBold*, fauxItalic*: bool
    kerning*: Kerning
    kern*: float32
    caps*: Caps
    ligatures*, discretionaryLigatures*: bool
    features*: seq[FontFeature]
    variations*: seq[FontVariation]
    language*: Option[string]
    styleSheet*: Option[uint32]
  TextRun* = object
    len*: int
    style*: CharStyle
  TextAlign* = enum
    taLeft = "Left", taCenter = "Center", taRight = "Right", taJustifyLeft = "JustifyLeft",
    taJustifyCenter = "JustifyCenter", taJustifyRight = "JustifyRight", taJustifyAll = "JustifyAll"
  TextDirection* = enum tdAuto = "Auto", tdLtr = "Ltr", tdRtl = "Rtl"
  ParagraphStyle* = object
    align*: TextAlign
    firstLineIndentPt*, startIndentPt*, endIndentPt*, spaceBeforePt*, spaceAfterPt*: float32
    autoLeading*: float32
    direction*: TextDirection
    hyphenate*: bool
    styleSheet*: Option[uint32]
  ParagraphRun* = object
    len*: int
    style*: ParagraphStyle
  TextShapeKind* = enum tsPoint = "Point", tsBox = "Box"
  TextShape* = object
    kind*: TextShapeKind
    x*, y*, width*, height*: float32
  Orientation* = enum orHorizontal = "Horizontal", orVertical = "Vertical"
  AntiAlias* = enum
    aaNone = "None", aaSharp = "Sharp", aaCrisp = "Crisp", aaStrong = "Strong", aaSmooth = "Smooth",
    aaWindows = "Windows", aaWindowsLcd = "WindowsLcd"
  TextWarp* = object
    style*: string
    value*, horizontalDistortion*, verticalDistortion*: float32
    horizontal*: bool
  TextLayer* = object
    text*: string
    fontFamily*: string
    sizePt*: float32
    color*: Color
    transform*: Affine
    cache*: Option[Surface]
    psdRaw*: SharedBytes
    runs*: seq[TextRun]
    paragraphs*: seq[ParagraphRun]
    shape*: TextShape
    orientation*: Orientation
    antialias*: AntiAlias
    warp*: Option[TextWarp]
  CharacterStyleDef* = object
    id*: uint32
    name*: string
    attrs*: JsonNode
  ParagraphStyleDef* = object
    id*: uint32
    name*: string
    paraAttrs*, charAttrs*: JsonNode
  TextStyles* = object
    character*: seq[CharacterStyleDef]
    paragraph*: seq[ParagraphStyleDef]
    basic*: ParagraphStyleDef
    psdRaw*: Option[JsonNode]

  # ---------------------------------------------------------------- smart objects

  StackMode* = enum
    smEntropy = "entropy", smKurtosis = "kurtosis", smMaximum = "maximum", smMean = "mean",
    smMedian = "median", smMinimum = "minimum", smRange = "range", smSkewness = "skewness",
    smStandardDeviation = "standardDeviation", smSummation = "summation", smVariance = "variance"
  SmartSourceKind* = enum ssEmbedded, ssLinked
  SmartSource* = object
    kind*: SmartSourceKind
    fileName*: string
    bytes*: SharedBytes
    path*: string
  SmartFilter* = object
    command*: string
    params*: JsonNode
    blend*: BlendMode
    opacity*: float32
    visible*: bool
  LayerMask* = object
    surface*: Surface  ## grayscale; untouched pixels read as its default (0 hide, 1 reveal)
    enabled*, linked*: bool
    density*, feather*: float32
  SmartObject* = object
    contentsId*: SmartContentsId
    source*: SmartSource
    transform*: Affine
    smartFilters*: seq[SmartFilter]
    cache*: Option[Surface]
    psdRaw*: SharedBytes
    filtersEnabled*: bool
    filterMask*: Option[LayerMask]
    warp*: Option[Warp]
    stackMode*: Option[StackMode]
    perspective*: Option[array[9, float64]]

  # ---------------------------------------------------------------- comps and artboards

  ArtboardBackgroundKind* = enum abWhite = "white", abBlack = "black", abTransparent = "transparent", abCustom = "custom"
  ArtboardBackground* = object
    kind*: ArtboardBackgroundKind
    color*: Color
  Artboard* = object
    rect*: Rect
    background*: ArtboardBackground
    preset*: string
  CompAppearance* = object
    blend*: BlendMode
    opacity*, fillOpacity*: float32
    effects*: Effects
  CompLayerState* = object
    layer*: LayerId
    visible*: Option[bool]
    position*: Option[(int, int)]
    appearance*: Option[CompAppearance]
  LayerComp* = object
    id*: uint32
    name*, comment*: string
    applyVisibility*, applyPosition*, applyAppearance*: bool
    states*: seq[CompLayerState]

  # ---------------------------------------------------------------- video

  VideoSourceKind* = enum vsBlank = "Blank", vsFile = "File"
  VideoSource* = object
    kind*: VideoSourceKind
    path*: string
  VideoData* = object
    frames*: seq[Surface]
    originalFrames*: Option[seq[Surface]]
    source*: VideoSource
    fps*: float32
    showAltered*: bool
  Timeline* = object
    fps*: float32
    duration*, current*, workStart*, workEnd*: int

  # ---------------------------------------------------------------- layers

  LayerContentKind* = enum
    lkRaster = "Pixel", lkGroup = "Group", lkAdjustment = "Adjustment", lkFill = "Fill",
    lkText = "Type", lkShape = "Shape", lkSmart = "Smart Object"
  Group* = object
    children*: seq[Layer]
    expanded*: bool
    artboard*: Option[Artboard]
  LayerContent* = object
    kind*: LayerContentKind
    raster*: Surface
    group*: Group
    adjustment*: Adjustment
    fill*: Fill
    text*: TextLayer
    shape*: ShapeLayer
    smart*: SmartObject
  PsdBlock* = object
    key*: string        ## four characters
    data*: SharedBytes
  Layer* = object
    id*: LayerId
    name*: string
    visible*: bool
    locks*: Locks
    blend*: BlendMode
    opacity*: float32       ## content and effects
    fillOpacity*: float32   ## content only
    clipped*: bool
    mask*: Option[LayerMask]
    vectorMask*: Option[VectorMask]
    effects*: Effects
    label*: LabelColor
    content*: LayerContent
    psdBlocks*: seq[PsdBlock]
    psdId*: Option[uint32]
    fillCache*: Option[FillCache]
    linkGroup*: Option[uint64]
    excludedChannels*: uint32
    blendIf*: BlendIf
    advanced*: AdvancedBlending
    video*: Option[VideoData]

  # ---------------------------------------------------------------- document parts

  GuideColor* = object
    vertical*: bool
    position*: float64
    rgb*: array[3, uint8]
  Guides* = object
    horizontal*, vertical*: seq[float64]
    colors*: seq[GuideColor]
  ColorIndicates* = enum ciMaskedAreas = "maskedAreas", ciSelectedAreas = "selectedAreas"
  AlphaChannel* = object
    name*: string
    surface*: Surface
    spot*: Option[(Color, float32)]
    color*: Color
    opacity*: float32
    indicates*: ColorIndicates
  PsdGlobalBlock* = object
    signature*, key*: string
    data*: SharedBytes
  PsdResource* = object
    id*: uint16
    name*: string
    data*: SharedBytes
  Metadata* = object
    xmp*: Option[string]
    exif*: SharedBytes
    text*: seq[(string, string)]
    psdResources*: seq[PsdResource]
    psdGlobalBlocks*: seq[PsdGlobalBlock]
  Pattern* = object
    id*, name*: string
    width*, height*: int
    surface*: Surface
  ColorTable* = object
    colors*: seq[array[3, uint8]]
    transparent*: Option[uint8]
  DuotoneInk* = object
    name*: string
    color*: array[3, float32]
    curve*: seq[CurvePoint]
  Duotone* = object
    inks*: seq[DuotoneInk]
    psdRaw*: Option[seq[byte]]
  MeasurementScale* = object
    pixelLength*, logicalLength*: float64
    units*: string
  CountGroup* = object
    name*: string
    color*: Color
    markerSize*, labelSize*: uint32
    visible*: bool
    points*: seq[array[2, float64]]
  Ruler* = object
    start*, `end`*: array[2, float64]
    protractor*: Option[array[2, float64]]
  Measurement* = object
    scale*: MeasurementScale
    countGroups*: seq[CountGroup]
    activeCountGroup*: int
    ruler*: Option[Ruler]
  Note* = object
    author*: string
    color*: Color
    text*: string
    position*: array[2, float64]
    popup*: array[4, float64]
    open*: bool
    modified*: string
  SliceOrigin* = enum soAuto = "auto", soLayer = "layer", soUser = "user"
  SliceKind* = enum skImage = "image", skNoImage = "noImage", skTable = "table"
  WebSlice* = object
    ## A web slice (Slice tool, layer-based slices; PSD resource 1050).
    id*, groupId*: uint32
    origin*: SliceOrigin
    layer*: Option[LayerId]
    name*: string
    rect*: Rect
    kind*: SliceKind
    url*, target*, message*, alt*: string
    cellTextIsHtml*: bool
    cellText*: string
    horizontalAlign*, verticalAlign*: uint32
    background*: Option[array[4, uint8]]
    outsets*: array[4, int32]
  Slices* = object
    groupName*: string
    list*: seq[WebSlice]
  PixelMethod* = enum pmFit = "fit", pmFill = "fill", pmAsIs = "asIs", pmConform = "conform"
  PixelAlign* = enum
    paTopLeft = "topLeft", paTopCenter = "topCenter", paTopRight = "topRight",
    paCenterLeft = "centerLeft", paCenter = "center", paCenterRight = "centerRight",
    paBottomLeft = "bottomLeft", paBottomCenter = "bottomCenter", paBottomRight = "bottomRight"
  VarKindKind* = enum vkVisibility = "visibility", vkTextReplacement = "textReplacement", vkPixelReplacement = "pixelReplacement"
  VarKind* = object
    kind*: VarKindKind
    `method`*: PixelMethod
    align*: PixelAlign
    clip*: bool
  VariableDef* = object
    name*: string
    layer*: LayerId
    kind*: VarKind
  DataValueKind* = enum dvVisibility = "visibility", dvText = "text", dvPixels = "pixels"
  DataValue* = object
    variable*: string
    kind*: DataValueKind
    visible*: bool
    text*: string   ## Text and Pixels (a file path)
  DataSet* = object
    name*: string
    values*: seq[DataValue]
  Variables* = object
    defs*: seq[VariableDef]
    dataSets*: seq[DataSet]
    active*: Option[int]

  Document* = object
    id*: DocId
    name*: string
    size*: Size
    resolutionDpi*: float32
    mode*: ColorMode
    depth*: SampleType
    iccProfile*: SharedBytes
    layers*: seq[Layer]              ## bottom-to-top
    channels*: seq[AlphaChannel]
    guides*: Guides
    selection*: Option[Surface]
    metadata*: Metadata
    globalLight*: GlobalLight
    paths*: seq[NamedPath]
    workPath*: Option[Path]
    clippingPath*: Option[ClippingPath]
    quickMask*: Option[AlphaChannel]
    quickMaskUnselected*: bool
    patterns*: seq[Pattern]
    colorTable*: Option[ColorTable]
    duotone*: Option[Duotone]
    layerComps*: seq[LayerComp]
    lastAppliedComp*: Option[uint32]
    lastDocumentState*: Option[LayerComp]
    measurement*: Measurement
    notes*: seq[Note]
    textStyles*: TextStyles
    slices*: Slices
    variables*: Variables
    timeline*: Option[Timeline]

  LayerPath* = seq[int]

const MaxGroupDepth* = 100
  ## Deepest group nesting a document may hold; deeper trees are refused by import and save.
