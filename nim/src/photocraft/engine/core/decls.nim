## Forward declarations of the procedures one `engine/core` file uses before the file that
## defines them is included. Part of the `engine/core` unit: included, not imported.

proc editsChannel*(s: Session): bool
proc isWork*(name: Option[string]): bool
proc parsePath*(v: JsonNode): Path
proc isChannelTarget*(p: JsonNode): bool
proc channelSurfaceForFilter*(doc: var Document; layer: Option[LayerId]; p: JsonNode): ptr Surface
proc adjFromParams*(kind: string; p: JsonNode; current: Option[Adjustment]; mode: ColorMode): Adjustment
proc adjDefaultFor*(kind: string; mode: ColorMode): Adjustment
proc shiftSurface*(s: Surface; dx, dy: int): Surface
proc translateVectors*(doc: Document; l: var Layer; dx, dy: float64)
proc refreshText*(doc: Document; t: var TextLayer)
proc shiftSmart*(sm: var SmartObject; dx, dy: int)
proc backgroundId*(doc: Document): Option[LayerId]
proc keepBackgroundAtBottom*(bg: Option[LayerId]; doc: Document)
proc maskTargets*(s: Session; p: JsonNode): seq[LayerId]
proc backgroundToLayerForMask*(doc: var Document; id: LayerId)
proc fillCmd*(s: var Session; p: JsonNode): JsonNode
proc clearArea*(doc: var Document; id: LayerId; area: Rect; sel: Option[Surface]; bg: array[4, float32])
proc targetedChannel*(s: Session): Option[JsonNode]
proc clearChannel*(doc: var Document; target: JsonNode; background: array[4, float32])
proc targetedColor*(s: Session): Option[int]
proc clearColorChannel*(doc: var Document; id: LayerId; k: int; background: array[4, float32])
proc newLayerRun*(s: var Session; p: JsonNode): JsonNode
proc noteInertInsert*(s: var Session; id: LayerId)
proc groupLayers*(s: var Session; p: JsonNode): JsonNode
proc multi*(s: Session; p: JsonNode): bool
proc duplicateSelected*(s: var Session; inPlace: bool; label: string): JsonNode
proc noteInsert*(s: var Session; before: Document)
proc deleteSelected*(s: var Session): JsonNode
proc deletionNeighbours*(doc: Document; target: LayerId): seq[LayerId]
proc selectLayerCmd*(s: var Session; p: JsonNode): JsonNode
proc noteDamage*(s: var Session; before: Document; ids: openArray[LayerId])
proc moveTo*(s: var Session; p: JsonNode): JsonNode
proc translateCmd*(s: var Session; p: JsonNode): JsonNode
proc placeCopy*(doc: var Document; id: LayerId)
proc layerEyedropper*(s: Session; id: LayerId; p: JsonNode): Option[Adjustment]
proc adjUserKeys*(p: JsonNode): seq[string]
proc eyedropperFromParams*(s: Session; kind: string; p: JsonNode): Option[Adjustment]
proc adjToParams*(a: Adjustment): JsonNode
proc gradientStyle*(name: string): GradientStyle
proc paintStroke*(s: var Session; p: JsonNode): JsonNode
proc inspectSession*(s: Session): JsonNode
proc inspectDocument*(d: DocState): JsonNode
proc hasAdjustableE*(s: Session): string
proc adjustAsSmartFilter*(s: var Session; kind, label: string; p: JsonNode): bool
proc validateNewParams*(s: Session; p: JsonNode)
proc backgroundColorParam*(p: JsonNode; fallback: array[4, float32]; cmdId: string): array[4, float32]
proc colorModeParam*(mode: string): Option[ColorMode]
proc sampleTypeParam*(depth: uint64): Option[SampleType]
proc filterParamsFor*(id: string; p: JsonNode): Option[FilterParams]
proc fillColor*(surf: var Surface; area: Rect; color: array[4, float32]; sel: Option[Surface])
proc mergeable(s: Session): seq[int]
proc targetChannel(s: var Session; t: ChannelTarget)
proc isBackground*(l: Layer): bool
proc setVisibleSelected*(s: var Session; visible: bool): JsonNode
proc fitCanvas*(doc: var Document)
proc transformShape*(sh: var ShapeLayer; a: Affine)
proc refreshShape*(doc: Document; sh: var ShapeLayer)
proc transformPlacement*(sm: var SmartObject; a: Affine)
proc refreshSmartLayer*(doc: Document; l: var Layer)
proc symmetryInspect*(p: PaintingSymmetry): JsonNode
proc loneTarget*(doc: Document; active: Option[LayerId]; p: JsonNode): Option[Surface]
proc targetBounds*(doc: Document; surf: Surface): Rect
proc transformBounds*(doc: Document; l: Layer): Rect
proc floatBeforeCommand*(s: var Session; id: string): Option[JsonNode]
