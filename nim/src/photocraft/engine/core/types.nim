## Engine types (`lib.rs`, `commands.rs`, `jobs.rs` and the view/tool state the command modules
## keep on the session). Part of the `engine/core` unit: included, not imported.

type
  EngineErrorKind* = enum
    eeUnknownCommand, eeDisabled, eeBadParams, eeNoDocument, eeNoLayer, eeOther, eeCancelled

  EngineError* = object of CatchableError
    ## Why a command failed. `msg` is the Rust `Display` text.
    kind*: EngineErrorKind
    cmd*: string ## the command (unknown, disabled, bad params)
    detail*: string ## the reason or message, without the prefix

  # ---------------------------------------------------------------- jobs (jobs.rs)

  JobId* = distinct uint64

  JobShared = ref object
    cancel: bool
    progress: float32
    message: string

  JobCtx* = object
    ## What a job's work sees: report progress, check for cancellation. Copies share state.
    shared: JobShared

  JobOutcomeKind* = enum joDone, joFailed, joCancelled

  JobOutcome* = object
    case kind*: JobOutcomeKind
    of joDone: value*: JsonNode
    of joFailed: error*: string
    of joCancelled: discard

  JobEvent* = object
    id*: JobId
    command*, label*: string
    document*: Option[DocId]
    outcome*: JobOutcome

  JobInfo* = object
    id*: JobId
    command*, label*: string
    document*: Option[DocId]
    state*: string ## running, done, failed or cancelled
    progress*: float32
    message*: string
    elapsedMs*: float64
    result*: Option[JsonNode]
    error*: Option[string]

  StartedKind* = enum stDone, stJob

  Started* = object
    ## How `start` ended: the command ran to completion, or it runs in the background.
    case kind*: StartedKind
    of stDone: value*: JsonNode
    of stJob: job*: JobId

  Jobs* = object
    ## The session's job table. This port runs job work inline (no worker threads yet), so
    ## `running` stays empty and `start` always completes at once.
    next: uint64
    spawn: bool
    events: seq[JobEvent]
    recent: seq[JobInfo]
    inline: Option[JobCtx]

  # ---------------------------------------------------------------- channel view (channel_cmds.rs)

  ChannelTargetKind* = enum
    ctComposite  ## the layers (all colour channels); in Quick Mask mode the Quick Mask
    ctColor      ## one colour channel of the active layer
    ctAlpha      ## an alpha or spot channel
    ctLayerMask  ## the active layer's layer mask
    ctVectorMask ## the active layer's vector mask

  ChannelTarget* = object
    ## What pixel commands edit (Channels panel click / ⌘2…⌘9).
    kind*: ChannelTargetKind
    index*: int ## colour channel (ctColor) or alpha channel (ctAlpha)

  MaskViewMode* = enum
    mvGray    ## the mask alone, as a grayscale image (⌥-click)
    mvOverlay ## the composite with the mask as a red overlay (⇧⌥-click, `\`)

  LayerMaskView* = object
    layer*: LayerId
    mode*: MaskViewMode

  ChannelView* = object
    ## Per-document Channels panel state: the target and the eye toggles. Not part of history.
    target*: ChannelTarget
    colorHidden*: seq[bool]  ## hidden colour channels (missing entries are visible)
    alphaVisible*: seq[bool] ## visible alpha channels (missing entries are hidden)
    quickMaskHidden*: bool
    layerMask*: Option[LayerMaskView]

  QuickMaskOptions* = object
    color*: Color
    opacity*: float32
    indicates*: ColorIndicates

  # ---------------------------------------------------------------- colour management (color_cmds.rs)

  Policy* = enum
    ## Colour management policy for documents whose profile differs from the working space.
    poPreserve = "preserve", poConvert = "convert", poOff = "off"

  ColorSettings* = object
    ## Edit › Color Settings.
    workingRgb*, workingCmyk*, workingGray*: string
    policyRgb*, policyCmyk*, policyGray*: Policy
    askOnMismatch*, askOnPaste*, askOnMissing*: bool
    intent*: string
    bpc*, dither*: bool
    blendTextGamma*: float32
    monitorProfile*: string

  ProofKindTag* = enum
    pkProfile, pkPlate, pkCmyPlate, pkLegacyMacintoshRgb, pkProtanopia, pkDeuteranopia
  ProofKind* = object
    ## What a proof simulates (`proof_sim::ProofKind`).
    kind*: ProofKindTag
    plate*: uint8

  ProofSetup* = object
    ## View › Proof Setup settings.
    name*: string
    profile*: Profile
    intent*: Intent
    bpc*, simulatePaper*: bool
    kind*: ProofKind

  ProofView* = object
    ## Per-document proofing state.
    setup*: ProofSetup
    enabled*, gamutWarning*: bool
    gamutThreshold*: float32

  HdrPreview* = object
    ## View › 32-bit Preview Options.
    highlightCompression*: bool
    exposure*, gamma*: float32

  MonitorDetectionKind* = enum mdUnsupported, mdPending, mdFailed, mdFound, mdRetained
  MonitorDetection* = object
    ## What the platform reported about the displays' profiles (Monitor Profile = `auto`).
    kind*: MonitorDetectionKind
    reason*: string ## mdFailed, mdRetained

  Display* = object
    ## A display as the platform reports it.
    id*: uint32
    name*: string
    frame*: array[4, float64]
    profileName*: Option[string]
    icc*: SharedBytes

  ColorState* = object
    ## Session-wide colour settings and caches (`Session.color`).
    settings*: ColorSettings
    proofs*: Table[uint64, ProofView]
    displays*: seq[Display]
    monitorDetection*: MonitorDetection
    mainDisplay*: Option[uint32]
    hdr*: Table[uint64, HdrPreview]

  # ---------------------------------------------------------------- session-wide state of the modules

  PatternLibrary* = object
    ## The app-level pattern library (Window › Patterns).
    items*: seq[DocPattern]

  PresetGroup*[T] = object
    ## A named folder of presets.
    name*: string
    items*: seq[T]

  StopColorKind* = enum scForeground, scBackground, scRgb
  StopColor* = object
    ## A gradient stop colour: fixed, or the current foreground/background colour.
    case kind*: StopColorKind
    of scRgb: rgb*: array[3, float32]
    else: discard
  GradientPreset* = object
    name*: string
    stops*: seq[(float32, StopColor)] ## colour stops `(location 0..1, colour)`, sorted
    opacity*: seq[(float32, float32)] ## transparency stops `(location, opacity 0..1)`; empty = opaque

  StylePreset* = object
    name*: string
    effects*: seq[Effect]
    blend*: Option[BlendMode]
    fillOpacity*: Option[float32]

  ShapePreset* = object
    name*: string
    path*: Path

  ToolPreset* = object
    name*: string
    tool*: string    ## tool name as the shell spells it, case-insensitive
    options*: JsonNode ## opaque options; the engine reads `brush` and `foreground`

  CloneSource* = object
    source*: Option[array[2, float64]] ## sample point (document px), set by ⌥-click
    anchor*: Option[array[2, float64]] ## destination paired with `source`
    layer*: Option[uint64]             ## layer the source was set on (informational)
    scale*: array[2, float64]          ## W and H in percent
    rotation*: float64                 ## degrees, counter-clockwise
    flipH*, flipV*: bool
    linked*: bool                      ## the W/H link (maintain aspect ratio)
  CloneOverlay* = object
    show*: bool
    opacity*: float32 ## percent
    clipped*: bool
    autoHide*: bool
    invert*: bool
    blend*: string    ## normal | darken | lighten | difference
  CloneSources* = object
    slots*: array[5, CloneSource]
    active*: int
    overlay*: CloneOverlay
  CloneMapping* = object
    ## How a clone stroke samples: destination `d` reads the source at
    ## `source + m · (d − anchor)` (`m` row-major 2 × 2).
    source*, anchor*: (float64, float64)
    m*: array[4, float64]

  SwatchColorKind* = enum swRgb = "rgb", swHsb = "hsb", swCmyk = "cmyk", swLab = "lab", swGray = "gray", swBook = "book"
  SwatchColor* = object
    ## A swatch colour in its own model.
    case kind*: SwatchColorKind
    of swRgb, swHsb, swLab: v3*: array[3, float32]
    of swCmyk: v4*: array[4, float32]
    of swGray: gray*: float32
    of swBook:
      space*: uint16
      values*: array[4, uint16]
  SwatchKind* = enum skProcess = "process", skGlobal = "global", skSpot = "spot"
  Swatch* = object
    name*: string
    color*: SwatchColor
    kind*: SwatchKind

  DocumentSettings* = object
    ## File › New settings (`document_preset_cmds`).
    width*, height*: uint32
    resolution*: float32
    mode*: string
    depth*: uint8
    background*: string
    backgroundColor*: Option[array[3, float32]]
    unit*: string
    resolutionUnit*: string
  DocumentPreset* = object
    name*: string
    settings*: DocumentSettings

  CustomShape* = object
    ## Edit › Define Custom Shape result.
    name*: string
    path*: Path
  EditState* = object
    ## Edit-menu state kept by the session (`edit_menu_cmds`).
    customShapes*: seq[CustomShape]

  PresetState* = object
    ## Window › Gradients, Patterns (groups), Styles, Shapes, Tool Presets, Clone Source and
    ## Swatches, plus the panel state the engine needs (`presets`).
    gradients*: seq[PresetGroup[GradientPreset]]
    gradient*: GradientPreset ## the Gradient tool's current gradient
    styles*: seq[PresetGroup[StylePreset]]
    shapes*: seq[PresetGroup[ShapePreset]]
    patternGroups*: seq[PresetGroup[string]] ## pattern ids per folder
    pattern*: Option[string] ## the Patterns panel's selection (a library pattern id)
    toolPresets*: seq[ToolPreset]
    documents*: seq[DocumentPreset]
    recentDocuments*: seq[DocumentSettings]
    clone*: CloneSources
    layerDefaults*: Table[string, JsonNode] ## Layer Style › Make Default, per effect kind
    swatches*: seq[PresetGroup[Swatch]]
    rev*: uint64 ## bumped on every preset change

  # ---------------------------------------------------------------- per-document tool and view state

  Clip* = object
    ## Pixels on the session clipboard, with where they came from (for Paste in Place).
    surface*: Surface
    bounds*: Rect
    layers*: Option[DocRef] ## the copied layers themselves, when Copy took them whole

  Snapshot* = object
    ## A History panel snapshot: the document as it was when taken, and the layers it targeted.
    id*: uint64
    name*: string
    doc*: DocRef
    layers*: LayerTarget

  CutParts* = object
    ## A layer split by its selection (`float_cmds`): the layer with the selected pixels cut out,
    ## and the cut pixels (cropped to the selection).
    layer*: LayerId
    rest*, piece*: Surface
    mask*: bool ## the pieces are the layer's mask (`"target":"mask"`), not its pixels

  Floating* = object
    ## A piece floating `offset` whole pixels from where it was cut, over document `revision`.
    layer*: LayerId
    offset*: (int, int)
    revision*: uint64
    parts*: ref CutParts

  SymmetryAxis* = object
    ## A symmetry path sampled into straight segments.
    source*: string
    segments*: seq[array[2, array[2, float64]]]
  PaintingSymmetryKind* = enum psPath, psPreset
  PaintingSymmetry* = object
    ## Per-document painting tool state, never stored in the document's paths or pixels.
    case kind*: PaintingSymmetryKind
    of psPath: axis*: SymmetryAxis
    of psPreset: preset*: PresetSymmetry

  # ---------------------------------------------------------------- documents

  DocState* = object
    ## Per-document editing state.
    doc*: DocRef
    history*: History
    activeLayer*: Option[LayerId]
      ## the primary ("key") layer: what single-layer commands act on
    selectedLayers*: seq[LayerId] ## every selected layer, in selection order
    layerAnchor*: Option[LayerId] ## anchor of ⇧-click range selection
    path*: Option[string]
    sourceReadOnly*: bool
    revision*, savedRevision*: uint64
    lastDamage*: Option[Rect] ## area changed by the latest revision (none = everything)
    coalesce*: Option[string]
    channelView*: ChannelView
    isolatedLayers*: seq[LayerId]
    fxCollapsed*: seq[LayerId]
    showOnly*: Option[(LayerId, seq[(LayerId, bool)])]
    colorSamplers*: seq[array[2, float64]]
    symmetry*: Option[PaintingSymmetry]
    floating*: Option[Floating]
    snapshots*: seq[Snapshot]
    nextSnapshot*: uint64

  ToolState* = object
    ## App-wide tool state commands read (colours, brush…).
    foreground*, background*: array[4, float32]
    brush*: BrushSettings
    presets*: seq[BrushPreset]
    presetsRev*: uint64
    mixer*: MixerState
    brushGesture*: Option[(string, BrushSettings)]
    currentPreset*: Option[string]
    maskTargeted*: bool
    otherColors*: array[2, array[4, float32]]

  SmartLink* = object
    ## An open Edit Contents document and the shared contents it updates when saved or closed.
    child*, parent*: DocId
    layer*: LayerId
    contentsId*: SmartContentsId

  Stamp* = (uint64, int64, int64)
    ## A file's size and modification time (seconds and nanoseconds since 1970).
  LinkScan* = (uint64, seq[(string, seq[uint64], Option[Stamp])])
    ## Stamps of a document's linked files: (document id, [(file, layer ids, stamp)]).

  RunProc* = proc(s: var Session; p: JsonNode): JsonNode
  EnabledProc* = proc(s: Session): string
    ## "" when the command can run; otherwise why not.
  AuthorizeProc* = proc(id: string; params: JsonNode)
    ## Raises `EngineError` to refuse a command.

  CommandSpec* = object
    ## Metadata and implementation of one command.
    id*, label*: string
    menu*: seq[string] ## menu placement, e.g. `["Layer", "New"]`; empty = not in menus
    shortcut*: Option[string] ## egui-style notation (`Cmd+Shift+N`)
    params*: string ## human/agent-readable parameter description
    enabled*: EnabledProc
    run*: RunProc
    journal*: bool ## record in the session journal (false for queries)

  Session* = object
    docs: seq[DocState]
    activeIdx: Option[int]
    tools*: ToolState
    journal*: seq[(string, JsonNode)] ## executed commands (action recording, debugging)
    coalesceRequest*: Option[string]
    quickMaskOptions*: QuickMaskOptions
    patterns*: PatternLibrary
    prefs*: PrefsStore
    presets*: PresetState
    color*: ColorState
    clipboard*: Option[Clip]
    styleClipboard*: Option[(Effects, BlendMode, float32, AdvancedBlending)]
    pathFillClipboard*: Option[Fill]          ## Copy Fill (Paths panel / shape layers)
    pathStrokeClipboard*: Option[ShapeStroke] ## Copy Complete Stroke
    colorRestrict: Option[int]
    authorize*: AuthorizeProc
    smartLinks*: seq[SmartLink]                  ## open Edit Contents documents
    editState*: EditState
    typeDefaults*: Option[(CharStyle, ParagraphStyle)] ## Type › Save Default Type Styles
    linkStamps*: Table[(uint64, string), Stamp]  ## last seen stamps of linked files, per document
    linkScan*: Option[LinkScan]                  ## a finished look at the linked files
    jobs: Jobs
    samplersRemoved: int
    samplerMap: Option[Affine]
