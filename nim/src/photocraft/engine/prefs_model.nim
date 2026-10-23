## The preferences document (the data half of `prefs.rs`): Edit › Preferences sections,
## keyboard-shortcut, menu and toolbar customisation, as plain serde data with Photoshop's
## defaults. The session owns one (`Session.prefs`); the dotted-path access, validation and the
## `prefs.*` commands live in `core/prefs_logic.nim` and `prefs_cmds.nim`.
##
## JSON keys are camelCase (`edit.preferences.<section>` ids); missing keys keep their defaults
## and unknown keys are ignored, so files from older and newer versions load. Maps that Rust
## keeps in a `BTreeMap` are kept sorted by key.

import std/[json, tables, algorithm]
import ../serde
import ../doc/types as doctypes

type
  Unit* = enum
    ## Length units for rulers, the Info panel and size dialogs.
    unPixels = "pixels", unInches = "inches", unCentimeters = "cm", unMillimeters = "mm",
    unPoints = "points", unPicas = "picas", unPercent = "percent"
  TypeUnit* = enum tuPoints = "points", tuPixels = "pixels", tuMillimeters = "mm"
  PointSize* = enum psPostScript = "postScript", psTraditional = "traditional"
  Interpolation* = enum
    ipBicubicAutomatic = "bicubicAutomatic", ipNearest = "nearestNeighbor", ipBilinear = "bilinear",
    ipBicubic = "bicubic", ipBicubicSmoother = "bicubicSmoother", ipBicubicSharper = "bicubicSharper",
    ipPreserveDetails = "preserveDetails"
  ColorPickerKind* = enum cpAdobe = "adobe", cpSystem = "system"
  Theme* = enum
    thPro = "pro", thProMedium = "proMedium", thProLightGray = "proLightGray", thProLight = "proLight",
    thStudio = "studio", thStudioLight = "studioLight", thClassic = "classic", thSolarizedDark = "solarizedDark",
    thAdwaita = "adwaita", thAdwaitaDark = "adwaitaDark", thBreezeLight = "breezeLight", thBreezeDark = "breezeDark"
  AppearanceMode* = enum amAuto = "auto", amDark = "dark", amLight = "light"
  DarkTheme* = enum
    dtPro = "pro", dtProMedium = "proMedium", dtStudio = "studio", dtSolarizedDark = "solarizedDark",
    dtAdwaitaDark = "adwaitaDark", dtBreezeDark = "breezeDark"
  LightTheme* = enum
    ltStudioLight = "studioLight", ltProLightGray = "proLightGray", ltProLight = "proLight", ltClassic = "classic",
    ltAdwaita = "adwaita", ltBreezeLight = "breezeLight"
  CanvasColor* = enum
    ccDefault = "default", ccBlack = "black", ccDarkGray = "darkGray", ccMediumGray = "mediumGray",
    ccLightGray = "lightGray", ccCustom = "custom"
  CanvasBorder* = enum cbDropShadow = "dropShadow", cbLine = "line", cbNone = "none"
  UiScale* = enum
    usAuto = "auto", usP75 = "75", usP80 = "80", usP85 = "85", usP90 = "90", usP95 = "95", usP100 = "100",
    usP125 = "125", usP150 = "150", usP175 = "175", usP200 = "200", usP250 = "250", usP300 = "300"
  GpuBackend* = enum gbAuto = "auto", gbVulkan = "vulkan", gbDx12 = "dx12", gbMetal = "metal", gbGl = "gl", gbCpu = "cpu"
    ## Graphics backend of the desktop app's window and GPU canvas (applies at next launch).
  RenderingMode* = enum rmAuto = "auto", rmGpu = "gpu", rmCpu = "cpu"
    ## Rendering policy, independent of the advanced graphics backend selection.
  LinuxDisplayServer* = enum ldsAuto = "auto", ldsX11 = "x11"
    ## Linux display server of the desktop app's window (applies at next launch).
  UiFontSize* = enum ufTiny = "tiny", ufSmall = "small", ufMedium = "medium", ufLarge = "large"
  LogDestination* = enum ldMetadata = "metadata", ldTextFile = "textFile", ldBoth = "both"
  LogDetail* = enum lgSessionsOnly = "sessionsOnly", lgConcise = "concise", lgDetailed = "detailed"
  Ask* = enum akAlways = "always", akNever = "never", akAsk = "ask"
  QuickExportFormat* = enum qePng = "png", qeJpg = "jpg", qeGif = "gif", qeWebp = "webp"
  ExportLocation* = enum elAsk = "ask", elSameFolder = "sameFolder"
  ExportMetadata* = enum emNone = "none", emCopyright = "copyright", emAll = "all"
  PaintingCursor* = enum
    ## Painting cursors: Standard (tool icon), Precise (crosshair), Normal Brush Tip (the 50%
    ## opacity contour), Full Size Brush Tip (the whole diameter).
    pcStandard = "standard", pcPrecise = "precise", pcNormalTip = "normalTip", pcFullSizeTip = "fullSizeTip"
  RightClickPaint* = enum rcBrushPicker = "brushPicker", rcErase = "erase"
  OtherCursor* = enum ocStandard = "standard", ocPrecise = "precise"
  CheckerSize* = enum csNone = "none", csSmall = "small", csMedium = "medium", csLarge = "large"
  CheckerColors* = enum
    ckLight = "light", ckMedium = "medium", ckDark = "dark", ckRed = "red", ckOrange = "orange",
    ckGreen = "green", ckBlue = "blue", ckPurple = "purple", ckCustom = "custom"
  LineStyle* = enum lsLines = "lines", lsDashed = "dashedLines", lsDots = "dots"
  TextEngine* = enum teModern = "modern", teEastAsian = "eastAsian", teWorldReady = "worldReady"
  FontPreview* = enum
    fpOff = "off", fpSmall = "small", fpMedium = "medium", fpLarge = "large", fpExtraLarge = "extraLarge", fpHuge = "huge"
  RawColorSpace* = enum rcsSrgb = "srgb", rcsAdobeRgb = "adobeRgb", rcsProPhoto = "proPhoto", rcsDisplayP3 = "displayP3"
  RawDepth* = enum rdEight = "8", rdSixteen = "16"
  RawSharpen* = enum rsNone = "none", rsScreen = "screen", rsGlossy = "glossy", rsMatte = "matte"

  StrMap* = OrderedTable[string, string]   ## a `BTreeMap<String, String>` (kept sorted)
  ValueMap* = OrderedTable[string, JsonNode] ## a `BTreeMap<String, Value>` (kept sorted)

  General* = object
    colorPicker*: ColorPickerKind
    imageInterpolation*: Interpolation ## default resampling for Image Size and transforms
    beepWhenDone*, exportClipboard*, resizeImageDuringPlace*, alwaysCreateSmartObjectsWhenPlacing*: bool
    zoomWithScrollWheel*: bool         ## scroll wheel zooms instead of panning
    animatedZoom*, zoomResizesWindows*, useLegacyFreeTransform*, autoShowHomeScreen*: bool

  Interface* = object
    theme*: Theme                       ## legacy single-theme preference
    appearanceMode*: AppearanceMode
    darkTheme*: DarkTheme
    lightTheme*: LightTheme
    favoriteThemes*: seq[Theme]         ## theme shortcuts in the appearance button's context menu
    canvasColor*: CanvasColor
    canvasCustomColor*: string
    showCanvasDots*: bool
    canvasBorder*: CanvasBorder
    uiScale*: UiScale
    language*: string                   ## `auto` or a language code such as `en`, `ja`
    uiFontSize*: UiFontSize
    showChannelsInColor*, dynamicColorSliders*, showMenuColors*, showTooltips*, enableAnimations*: bool
    showBoundingBoxWhenDraggingLayer*, systemTitleBar*, notificationAutoHide*: bool
    notificationDurationSeconds*: uint32

  Workspace* = object
    autoCollapseIconPanels*, autoShowHiddenPanels*, openDocumentsAsTabs*: bool
    enableFloatingDocumentWindowDocking*, largeTabs*, enableNarrowOptionsBar*, rememberWorkspaceChanges*: bool

  Tools* = object
    showTooltips*, useShiftKeyForToolSwitch*, zoomClickedPointToCenter*, enableFlickPanning*: bool
    varyRoundBrushHardnessOnHud*, snapVectorToolsAndTransformsToPixelGrid*, showTransformationValues*: bool
    showHighlightOnRollover*, overscroll*, doubleClickLayerMaskLaunchesSelectAndMask*: bool
    rightClickWithPaintingTools*: RightClickPaint
    useTabletPressure*: bool
    pressureCurve*: seq[array[2, float32]] ## `[input, output]` control points in 0..1

  HistoryLog* = object
    enabled*: bool
    destination*: LogDestination
    filePath*: string
    detail*: LogDetail

  FileHandling* = object
    imagePreviews*: Ask
    lowercaseExtension*, saveInBackground*, autosave*: bool
    autosaveMinutes*: uint32
    recoverOnLaunch*, ignoreExifProfileTag*, askBeforeSavingLayeredTiff*: bool
    maximizePsdCompatibility*: Ask
    rasterizeSvgGroupsDeeperThan*: uint32
    recentFileCount*: uint32
    recentFiles*: seq[string]

  Export* = object
    quickExportFormat*: QuickExportFormat
    quickExportLocation*: ExportLocation
    jpegQuality*: uint32
    webpLossless*: bool
    webpQuality*: uint32
    metadata*: ExportMetadata
    convertToSrgb*: bool

  Performance* = object
    memoryUsageMb*, historyStates*, cacheLevels*, cacheTileSize*: uint32
    useGpu*: bool
    renderingMode*: Option[RenderingMode]
    gpuBackend*: GpuBackend
    lowResolutionPreviews*: bool
    linuxDisplayServer*: LinuxDisplayServer
    effectCacheMb*: uint32
    legacyCompositing*: bool

  ScratchDisk* = object
    path*: string
    enabled*: bool
  ScratchDisks* = object
    disks*: seq[ScratchDisk]

  Cursors* = object
    painting*: PaintingCursor
    showCrosshairInBrushTip*, showOnlyCrosshairWhilePainting*: bool
    other*: OtherCursor
    brushPreviewColor*: string

  TransparencyAndGamut* = object
    gridSize*: CheckerSize
    gridColors*: CheckerColors
    customLight*, customDark*, gamutWarningColor*: string
    gamutWarningOpacity*: uint32

  UnitsAndRulers* = object
    rulers*: Unit
    typeUnits*: TypeUnit
    columnWidth*, gutter*, printResolution*, screenResolution*: float64
    pointSize*: PointSize
    showRulers*: bool

  GuidesGridAndSlices* = object
    guideColor*: string
    guideStyle*: LineStyle
    smartGuideColor*, gridColor*: string
    gridStyle*: LineStyle
    gridlineEvery*: float64
    gridUnit*: Unit
    subdivisions*: uint32
    sliceColor*: string
    showSliceNumbers*: bool

  PlugIns* = object
    useAdditionalPluginsFolder*: bool
    additionalPluginsFolder*: string
    showExtensionPanels*, allowScriptsToConnect*, generatorEnabled*: bool

  TypePrefs* = object
    smartQuotes*, missingGlyphProtection*, showFontNamesInEnglish*, useEscToCommit*: bool
    textEngine*: TextEngine
    fontPreview*: FontPreview
    fillNewTypeLayersWithPlaceholder*: bool
    recentFonts*: uint32

  EnhancedControls* = object
    scrubbySliderAcceleration*, touchGestures*, zoomWithTrackpadPinch*, rotateViewWithTrackpad*: bool

  RawDefaults* = object
    colorSpace*: RawColorSpace
    bitDepth*: RawDepth
    resolution*: float64
    sharpenFor*: RawSharpen
    openAsSmartObject*, applyAutoTone*, openInCameraRaw*: bool

  Integrations* = object
    allowAgentControl*: bool
    controlPort*: uint32

  MenuCustomization* = object
    hidden*: seq[string]
    colors*: StrMap ## command id → colour name

  ToolbarCustomization* = object
    hidden*, order*: seq[string]

  ScriptBinding* = object
    ## One event → script binding (File › Scripts › Script Events Manager).
    event*: string
    script*: Option[string]
    steps*: Option[JsonNode]
    name*: string

  ScriptEvents* = object
    enabled*: bool
    bindings*: seq[ScriptBinding]

  Preferences* = object
    ## All preferences. JSON keys are the `edit.preferences.<section>` ids.
    general*: General
    `interface`*: Interface
    workspace*: Workspace
    tools*: Tools
    historyLog*: HistoryLog
    fileHandling*: FileHandling
    `export`*: Export
    performance*: Performance
    scratchDisks*: ScratchDisks
    cursors*: Cursors
    transparencyAndGamut*: TransparencyAndGamut
    unitsAndRulers*: UnitsAndRulers
    guidesGridAndSlices*: GuidesGridAndSlices
    plugIns*: PlugIns
    typePrefs* {.jsonName: "type".}: TypePrefs
    enhancedControls*: EnhancedControls
    rawDefaults*: RawDefaults
    integrations*: Integrations
    shortcuts*: StrMap   ## command id → shortcut (`""` removes the default)
    menus*: MenuCustomization
    toolbar*: ToolbarCustomization
    userDictionary*: seq[string]
    workspaces*: ValueMap
    workspaceLocked*: bool
    panelLayout*: JsonNode
    dialogs*: ValueMap
    brushPicker*: JsonNode
    toolOptions*: JsonNode
    scriptEvents*: ScriptEvents

  PrefsStore* = object
    ## The session's preferences plus a revision counter that moves on every change (frontends
    ## persist when it moves).
    values*: Preferences
    rev*: uint64

# ---------------------------------------------------------------- serde shape

proc serdeCamel*(T: typedesc[General]): bool = true
proc serdeCamel*(T: typedesc[Interface]): bool = true
proc serdeCamel*(T: typedesc[Workspace]): bool = true
proc serdeCamel*(T: typedesc[Tools]): bool = true
proc serdeCamel*(T: typedesc[HistoryLog]): bool = true
proc serdeCamel*(T: typedesc[FileHandling]): bool = true
proc serdeCamel*(T: typedesc[Export]): bool = true
proc serdeCamel*(T: typedesc[Performance]): bool = true
proc serdeCamel*(T: typedesc[ScratchDisk]): bool = true
proc serdeCamel*(T: typedesc[ScratchDisks]): bool = true
proc serdeCamel*(T: typedesc[Cursors]): bool = true
proc serdeCamel*(T: typedesc[TransparencyAndGamut]): bool = true
proc serdeCamel*(T: typedesc[UnitsAndRulers]): bool = true
proc serdeCamel*(T: typedesc[GuidesGridAndSlices]): bool = true
proc serdeCamel*(T: typedesc[PlugIns]): bool = true
proc serdeCamel*(T: typedesc[TypePrefs]): bool = true
proc serdeCamel*(T: typedesc[EnhancedControls]): bool = true
proc serdeCamel*(T: typedesc[RawDefaults]): bool = true
proc serdeCamel*(T: typedesc[Integrations]): bool = true
proc serdeCamel*(T: typedesc[MenuCustomization]): bool = true
proc serdeCamel*(T: typedesc[ToolbarCustomization]): bool = true
proc serdeCamel*(T: typedesc[ScriptBinding]): bool = true
proc serdeCamel*(T: typedesc[ScriptEvents]): bool = true
proc serdeCamel*(T: typedesc[Preferences]): bool = true

proc sortByKey*[V](t: var OrderedTable[string, V]) =
  t.sort(proc(a, b: (string, V)): int = cmp(a[0], b[0]))

proc toJ*(x: StrMap): JsonNode =
  result = newJObject()
  var keys: seq[string]
  for k in x.keys: keys.add k
  keys.sort(cmp[string])
  for k in keys: result[k] = newJString(x[k])

proc toJ*(x: ValueMap): JsonNode =
  result = newJObject()
  var keys: seq[string]
  for k in x.keys: keys.add k
  keys.sort(cmp[string])
  for k in keys: result[k] = (if x[k].isNil: newJNull() else: x[k])

proc fromJ*(n: JsonNode; x: var StrMap) =
  if n.kind != JObject:
    if serdeStrict: raise serdeError("invalid type: " & serdeUnexpected(n) & ", expected a map")
    raise serdeError("expected an object")
  x.clear()
  var keys: seq[string]
  for k in n.fields.keys: keys.add k
  keys.sort(cmp[string])
  for k in keys:
    var v: string
    fromJ(n.fields[k], v)
    x[k] = v

proc fromJ*(n: JsonNode; x: var ValueMap) =
  if n.kind != JObject:
    if serdeStrict: raise serdeError("invalid type: " & serdeUnexpected(n) & ", expected a map")
    raise serdeError("expected an object")
  x.clear()
  var keys: seq[string]
  for k in n.fields.keys: keys.add k
  keys.sort(cmp[string])
  for k in keys: x[k] = n.fields[k]

proc toJ*(x: ScriptBinding): JsonNode =
  %*{"event": x.event, "script": (if x.script.isSome: newJString(x.script.get) else: newJNull()),
     "steps": (if x.steps.isSome and not x.steps.get.isNil: x.steps.get else: newJNull()), "name": x.name}

proc fromJ*(n: JsonNode; x: var ScriptBinding) =
  objectFromJ(n, x)
  if x.steps.isSome and (x.steps.get.isNil or x.steps.get.kind == JNull): x.steps = none(JsonNode)

# ---------------------------------------------------------------- defaults

proc defaultOf*(T: typedesc[General]): General =
  General(colorPicker: cpAdobe, imageInterpolation: ipBicubicAutomatic, beepWhenDone: false, exportClipboard: true,
          resizeImageDuringPlace: true, alwaysCreateSmartObjectsWhenPlacing: true, zoomWithScrollWheel: false,
          animatedZoom: true, zoomResizesWindows: false, useLegacyFreeTransform: false, autoShowHomeScreen: true)

proc defaultOf*(T: typedesc[Interface]): Interface =
  Interface(theme: thProMedium, appearanceMode: amDark, darkTheme: dtProMedium, lightTheme: ltStudioLight,
            favoriteThemes: @[], canvasColor: ccDefault, canvasCustomColor: "#282828", showCanvasDots: true,
            canvasBorder: cbDropShadow, uiScale: usAuto, language: "auto", uiFontSize: ufSmall,
            showChannelsInColor: false, dynamicColorSliders: true, showMenuColors: true, showTooltips: true,
            enableAnimations: true, showBoundingBoxWhenDraggingLayer: false, systemTitleBar: false,
            notificationAutoHide: true, notificationDurationSeconds: 6)

proc defaultOf*(T: typedesc[Workspace]): Workspace =
  Workspace(autoCollapseIconPanels: false, autoShowHiddenPanels: true, openDocumentsAsTabs: true,
            enableFloatingDocumentWindowDocking: true, largeTabs: false, enableNarrowOptionsBar: false,
            rememberWorkspaceChanges: true)

proc defaultOf*(T: typedesc[Tools]): Tools =
  Tools(showTooltips: true, useShiftKeyForToolSwitch: true, zoomClickedPointToCenter: false, enableFlickPanning: true,
        varyRoundBrushHardnessOnHud: true, snapVectorToolsAndTransformsToPixelGrid: true,
        showTransformationValues: true, showHighlightOnRollover: true, overscroll: true,
        doubleClickLayerMaskLaunchesSelectAndMask: true, rightClickWithPaintingTools: rcBrushPicker,
        useTabletPressure: true, pressureCurve: @[[0'f32, 0'f32], [1'f32, 1'f32]])

proc defaultOf*(T: typedesc[HistoryLog]): HistoryLog =
  HistoryLog(enabled: false, destination: ldMetadata, filePath: "", detail: lgConcise)

proc defaultOf*(T: typedesc[FileHandling]): FileHandling =
  FileHandling(imagePreviews: akAlways, lowercaseExtension: true, saveInBackground: true, autosave: true,
               autosaveMinutes: 10, recoverOnLaunch: true, ignoreExifProfileTag: false,
               askBeforeSavingLayeredTiff: true, maximizePsdCompatibility: akAlways,
               rasterizeSvgGroupsDeeperThan: uint32(MaxGroupDepth), recentFileCount: 20, recentFiles: @[])

proc defaultOf*(T: typedesc[Export]): Export =
  Export(quickExportFormat: qePng, quickExportLocation: elAsk, jpegQuality: 85, webpLossless: true, webpQuality: 85,
         metadata: emCopyright, convertToSrgb: true)

proc defaultOf*(T: typedesc[Performance]): Performance =
  Performance(memoryUsageMb: 8192, historyStates: 50, cacheLevels: 4, cacheTileSize: 8192, useGpu: true,
              renderingMode: none(RenderingMode), gpuBackend: gbAuto, lowResolutionPreviews: true,
              linuxDisplayServer: ldsAuto, effectCacheMb: 768, legacyCompositing: false)

proc defaultOf*(T: typedesc[ScratchDisk]): ScratchDisk = ScratchDisk()
proc defaultOf*(T: typedesc[ScratchDisks]): ScratchDisks =
  ScratchDisks(disks: @[ScratchDisk(path: "(system temp)", enabled: true)])

proc defaultOf*(T: typedesc[Cursors]): Cursors =
  Cursors(painting: pcNormalTip, showCrosshairInBrushTip: false, showOnlyCrosshairWhilePainting: false,
          other: ocStandard, brushPreviewColor: "#ff0000")

proc defaultOf*(T: typedesc[TransparencyAndGamut]): TransparencyAndGamut =
  TransparencyAndGamut(gridSize: csMedium, gridColors: ckLight, customLight: "#ffffff", customDark: "#cccccc",
                       gamutWarningColor: "#808080", gamutWarningOpacity: 100)

proc defaultOf*(T: typedesc[UnitsAndRulers]): UnitsAndRulers =
  UnitsAndRulers(rulers: unPixels, typeUnits: tuPoints, columnWidth: 180.0, gutter: 12.0, printResolution: 300.0,
                 screenResolution: 72.0, pointSize: psPostScript, showRulers: false)

proc defaultOf*(T: typedesc[GuidesGridAndSlices]): GuidesGridAndSlices =
  GuidesGridAndSlices(guideColor: "#4affff", guideStyle: lsLines, smartGuideColor: "#ff00ff", gridColor: "#8c8c8c",
                      gridStyle: lsLines, gridlineEvery: 1.0, gridUnit: unInches, subdivisions: 4,
                      sliceColor: "#38b5ff", showSliceNumbers: true)

proc defaultOf*(T: typedesc[PlugIns]): PlugIns =
  PlugIns(useAdditionalPluginsFolder: false, additionalPluginsFolder: "", showExtensionPanels: true,
          allowScriptsToConnect: false, generatorEnabled: false)

proc defaultOf*(T: typedesc[TypePrefs]): TypePrefs =
  TypePrefs(smartQuotes: true, missingGlyphProtection: true, showFontNamesInEnglish: true, useEscToCommit: true,
            textEngine: teWorldReady, fontPreview: fpMedium, fillNewTypeLayersWithPlaceholder: true, recentFonts: 10)

proc defaultOf*(T: typedesc[EnhancedControls]): EnhancedControls =
  EnhancedControls(scrubbySliderAcceleration: true, touchGestures: true, zoomWithTrackpadPinch: true,
                   rotateViewWithTrackpad: false)

proc defaultOf*(T: typedesc[RawDefaults]): RawDefaults =
  RawDefaults(colorSpace: rcsAdobeRgb, bitDepth: rdSixteen, resolution: 300.0, sharpenFor: rsNone,
              openAsSmartObject: false, applyAutoTone: false, openInCameraRaw: true)

proc defaultOf*(T: typedesc[Integrations]): Integrations = Integrations(allowAgentControl: true, controlPort: 0)
proc defaultOf*(T: typedesc[MenuCustomization]): MenuCustomization = MenuCustomization()
proc defaultOf*(T: typedesc[ToolbarCustomization]): ToolbarCustomization = ToolbarCustomization()
proc defaultOf*(T: typedesc[ScriptBinding]): ScriptBinding = ScriptBinding()
proc defaultOf*(T: typedesc[ScriptEvents]): ScriptEvents = ScriptEvents()

proc defaultOf*(T: typedesc[Preferences]): Preferences =
  Preferences(general: defaultOf(General), `interface`: defaultOf(Interface), workspace: defaultOf(Workspace),
              tools: defaultOf(Tools), historyLog: defaultOf(HistoryLog), fileHandling: defaultOf(FileHandling),
              `export`: defaultOf(Export), performance: defaultOf(Performance), scratchDisks: defaultOf(ScratchDisks),
              cursors: defaultOf(Cursors), transparencyAndGamut: defaultOf(TransparencyAndGamut),
              unitsAndRulers: defaultOf(UnitsAndRulers), guidesGridAndSlices: defaultOf(GuidesGridAndSlices),
              plugIns: defaultOf(PlugIns), typePrefs: defaultOf(TypePrefs), enhancedControls: defaultOf(EnhancedControls),
              rawDefaults: defaultOf(RawDefaults), integrations: defaultOf(Integrations),
              panelLayout: newJNull(), brushPicker: newJNull(), toolOptions: newJNull())

proc defaultPrefs*(): Preferences = defaultOf(Preferences)

proc toJ*(x: Preferences): JsonNode =
  ## The preferences as JSON (camelCase keys).
  result = objectToJ(x)
  for k in ["panelLayout", "brushPicker", "toolOptions"]:
    if result[k].isNil: result[k] = newJNull()

proc perInch*(p: PointSize): float64 = (if p == psPostScript: 72.0 else: 72.27)
  ## Points per inch.

proc prefsStore*(): PrefsStore = PrefsStore(values: defaultPrefs())
