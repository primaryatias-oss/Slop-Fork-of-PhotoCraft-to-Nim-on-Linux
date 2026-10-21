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

  # ---------------------------------------------------------------- per-document tool and view state

  Snapshot* = object
    ## A History panel snapshot: the document as it was when taken, and the layers it targeted.
    id*: uint64
    name*: string
    doc*: DocRef
    layers*: LayerTarget

  CutParts* = object
    ## The pieces a floating selection carries (`edit_cmds`; filled in when it lands).
    discard

  Floating* = object
    ## A piece floating `offset` whole pixels from where it was cut, over document `revision`.
    layer*: LayerId
    offset*: (int, int)
    revision*: uint64
    parts*: ref CutParts

  PaintingSymmetry* = object
    ## Painting symmetry preset or sampled path (`symmetry_cmds`; filled in when it lands).
    discard

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
    nextSnapshot: uint64

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
    coalesceRequest: Option[string]
    quickMaskOptions*: QuickMaskOptions
    colorRestrict: Option[int]
    authorize*: AuthorizeProc
    jobs: Jobs
    samplersRemoved: int
    samplerMap: Option[Affine]
