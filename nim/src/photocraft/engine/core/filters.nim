## `filter.*` commands (Photoshop's Filter menu) backed by `algo` (port of `filters.rs` and
## `filters/validation.rs`).
##
## Every command acts on the active pixel layer (or the cached pixels of a smart object, recording
## a smart filter), respects the selection, and is undoable. `filter.lastFilter` re-runs the most
## recent filter command. Part of the `engine/core` unit: included, not imported.

proc filterParamsExt*(id: string; p: JsonNode): Option[FilterParams]
proc filtersPrepare*(s: Session; id: string; p: JsonNode): JsonNode
proc filtersResolve*(s: Session; fp: var FilterParams; p: JsonNode)
proc resolveInLayer*(fp: var FilterParams; layer: Layer; bounds: Rect)
proc galleryValidateParams*(id: string; p: JsonNode)
proc distortApplyToSurface*(id: string; params: JsonNode; surf: Surface; canvas: Rect): Option[Surface]
proc lensApplyToSurface*(id: string; params: JsonNode; surf: Surface; canvas: Rect): Option[Surface]
proc pluginApplyToSurface*(id: string; params: JsonNode; surf: Surface; selection: Option[Surface]; canvas: Rect): Option[Surface]

const PluginRunId* = "plugin.run"

# ---------------------------------------------------------------- param readers

proc fltF(p: JsonNode; k: string; d: float32): float32 =
  let v = p.pget(k).asF64
  if v.isSome: float32(v.get) else: d

proc fltI64(p: JsonNode; k: string): Option[int64] =
  ## `v.as_i64().or_else(|| v.as_f64().map(|f| f.round() as i64))`.
  let v = p.pget(k)
  let i = v.asI64
  if i.isSome: return i
  let f = v.asF64
  if f.isSome: return some(rmath.asI64(round(f.get)))

proc fltI(p: JsonNode; k: string; d: int32): int32 =
  ## Clamped like the neighbouring filter params: an out-of-range value saturates.
  let v = fltI64(p, k)
  if v.isNone: d else: int32(clamp(v.get, int64(int32.low), int64(int32.high)))

proc fltB(p: JsonNode; k: string; d: bool): bool =
  let v = p.pget(k)
  if not v.isNil and v.kind == JBool: v.bval else: d

proc fltS(p: JsonNode; k, d: string): string =
  let v = p.pget(k)
  if not v.isNil and v.kind == JString: v.str else: d

proc fltSeed(p: JsonNode): uint32 =
  ## New calls accept the entire documented u32 range; keep legacy record decoding otherwise.
  let u = p.pget("seed").asU64
  if u.isSome and u.get <= uint64(uint32.high): return uint32(u.get)
  cast[uint32](fltI(p, "seed", 0))

proc fltUndefined(p: JsonNode): UndefinedAreas =
  case fltS(p, "undefinedAreas", "wrap")
  of "repeat": uaRepeat
  of "transparent": uaTransparent
  else: uaWrap

proc fltPreserve(p: JsonNode): Preserve =
  ## Minimum / Maximum › Preserve (Photoshop's default is Squareness).
  if fltS(p, "preserve", "squareness") == "roundness": prRoundness else: prSquareness

proc filterPreset(id: string): Option[(string, FilterParams)] =
  ## One-step presets (no dialog): the history step they record and the fixed parameters.
  proc mk(kind: FilterKind; radius = 0'f32; amount = 0'f32; threshold = 0'f32): FilterParams =
    result = newFilterParams(kind)
    result.radius = radius
    result.amount = amount
    result.threshold = threshold
  case id
  of "filter.blur.blur": some(("Blur", mk(fltGaussianBlur, radius = 0.6)))
  of "filter.blur.blurMore": some(("Blur More", mk(fltGaussianBlur, radius = 1.4)))
  of "filter.sharpen.sharpen": some(("Sharpen", mk(fltUnsharpMask, amount = 60, radius = 0.5, threshold = 0)))
  of "filter.sharpen.sharpenMore": some(("Sharpen More", mk(fltUnsharpMask, amount = 150, radius = 0.6, threshold = 0)))
  of "filter.sharpen.sharpenEdges": some(("Sharpen Edges", mk(fltUnsharpMask, amount = 100, radius = 0.8, threshold = 6)))
  of "filter.noise.despeckle": some(("Despeckle", mk(fltSurfaceBlur, radius = 2, threshold = 12)))
  else: none((string, FilterParams))

proc filterParamsFor*(id: string; p: JsonNode): Option[FilterParams] =
  ## The algorithm parameters for a filter command id from JSON params (Photoshop dialog units).
  ## Stored smart filters and previews keep this tolerant decoding; new invocations pass through
  ## `filtersValidateParams` first.
  let pre = filterPreset(id)
  if pre.isSome: return some(pre.get[1])
  template mk(k: FilterKind; body: untyped): untyped =
    block:
      var fp {.inject.} = newFilterParams(k)
      body
      some(fp)
  case id
  of "filter.blur.gaussianBlur":
    mk(fltGaussianBlur):
      fp.radius = clamp(fltF(p, "radius", 1), 0.1'f32, 1000'f32)
  of "filter.blur.boxBlur":
    mk(fltBoxBlur):
      fp.radius = clamp(fltF(p, "radius", 1), 1'f32, 2000'f32)
  of "filter.blur.motionBlur":
    mk(fltMotionBlur):
      fp.angle = fltF(p, "angle", 0)
      fp.distance = clamp(fltF(p, "distance", 10), 1'f32, 2000'f32)
  of "filter.blur.radialBlur":
    mk(fltRadialBlur):
      fp.amount = clamp(fltF(p, "amount", 10), 1'f32, 100'f32)
      fp.`method` = if fltS(p, "method", "spin") == "zoom": rmZoom else: rmSpin
      fp.radialQuality = case fltS(p, "quality", "good")
                         of "draft": rqDraft
                         of "best": rqBest
                         else: rqGood
      fp.centerX = fltF(p, "centerX", 0.5)
      fp.centerY = fltF(p, "centerY", 0.5)
  of "filter.blur.surfaceBlur":
    mk(fltSurfaceBlur):
      fp.radius = clamp(fltF(p, "radius", 5), 1'f32, 100'f32)
      fp.threshold = clamp(fltF(p, "threshold", 15), 2'f32, 255'f32)
  of "filter.sharpen.unsharpMask":
    mk(fltUnsharpMask):
      fp.amount = clamp(fltF(p, "amount", 50), 1'f32, 500'f32)
      fp.radius = clamp(fltF(p, "radius", 1), 0.1'f32, 1000'f32)
      fp.threshold = clamp(fltF(p, "threshold", 0), 0'f32, 255'f32)
  of "filter.sharpen.smartSharpen":
    mk(fltSmartSharpen):
      fp.amount = clamp(fltF(p, "amount", 100), 1'f32, 500'f32)
      fp.radius = clamp(fltF(p, "radius", 1), 0.1'f32, 64'f32)
      fp.reduceNoise = clamp(fltF(p, "reduceNoise", 10), 0'f32, 100'f32)
  of "filter.other.highPass":
    mk(fltHighPass):
      fp.radius = clamp(fltF(p, "radius", 10), 0.1'f32, 1000'f32)
  of "filter.noise.addNoise":
    mk(fltAddNoise):
      fp.amount = clamp(fltF(p, "amount", 12.5), 0.1'f32, 400'f32)
      fp.distribution = if fltS(p, "distribution", "uniform") == "gaussian": diGaussian else: diUniform
      fp.monochromatic = fltB(p, "monochromatic", false)
      fp.seed = fltSeed(p)
  of "filter.noise.median":
    mk(fltMedian):
      fp.radius = clamp(fltF(p, "radius", 1), 1'f32, 500'f32)
  of "filter.noise.dustAndScratches":
    mk(fltDustAndScratches):
      fp.radius = clamp(fltF(p, "radius", 1), 1'f32, 500'f32)
      fp.threshold = clamp(fltF(p, "threshold", 0), 0'f32, 255'f32)
  of "filter.other.minimum":
    mk(fltMinimum):
      fp.radius = clamp(fltF(p, "radius", 1), 0.2'f32, 500'f32)
      fp.preserve = fltPreserve(p)
  of "filter.other.maximum":
    mk(fltMaximum):
      fp.radius = clamp(fltF(p, "radius", 1), 0.2'f32, 500'f32)
      fp.preserve = fltPreserve(p)
  of "filter.other.offset":
    mk(fltOffset):
      fp.offsetH = fltI(p, "horizontal", 0)
      fp.offsetV = fltI(p, "vertical", 0)
      fp.undefined = fltUndefined(p)
  of "filter.pixelate.mosaic":
    mk(fltMosaic):
      fp.cellSize = clamp(fltF(p, "cellSize", 10), 2'f32, 200'f32)
  of "filter.stylize.emboss":
    mk(fltEmboss):
      fp.angle = fltF(p, "angle", 135)
      fp.height = clamp(fltF(p, "height", 3), 1'f32, 100'f32)
      fp.amount = clamp(fltF(p, "amount", 100), 1'f32, 500'f32)
  of "filter.stylize.findEdges": some(newFilterParams(fltFindEdges))
  of "filter.stylize.solarize": some(newFilterParams(fltSolarize))
  of "filter.distort.twirl":
    mk(fltTwirl):
      fp.angle = clamp(fltF(p, "angle", 50), -999'f32, 999'f32)
  of "filter.distort.pinch":
    mk(fltPinch):
      fp.amount = clamp(fltF(p, "amount", 50), -100'f32, 100'f32)
  of "filter.distort.spherize":
    mk(fltSpherize):
      fp.amount = clamp(fltF(p, "amount", 100), -100'f32, 100'f32)
      fp.spherizeMode = case fltS(p, "mode", "normal")
                        of "horizontalOnly": smHorizontalOnly
                        of "verticalOnly": smVerticalOnly
                        else: smNormal
  of "filter.distort.wave":
    mk(fltWave):
      fp.generators = uint32(clamp(fltI(p, "generators", 5), 1'i32, 999'i32))
      fp.wavelengthMin = clamp(fltF(p, "wavelengthMin", 10), 1'f32, 998'f32)
      fp.wavelengthMax = clamp(fltF(p, "wavelengthMax", 120), 2'f32, 999'f32)
      fp.amplitudeMin = clamp(fltF(p, "amplitudeMin", 5), 1'f32, 998'f32)
      fp.amplitudeMax = clamp(fltF(p, "amplitudeMax", 35), 1'f32, 999'f32)
      fp.waveType = case fltS(p, "type", "sine")
                    of "triangle": wtTriangle
                    of "square": wtSquare
                    else: wtSine
      fp.undefined = fltUndefined(p)
      fp.seed = fltSeed(p)
  of "filter.distort.ripple":
    mk(fltRipple):
      fp.amount = clamp(fltF(p, "amount", 100), -999'f32, 999'f32)
      fp.rippleSize = case fltS(p, "size", "medium")
                      of "small": rsSmall
                      of "large": rsLarge
                      else: rsMedium
  of "filter.distort.polarCoordinates":
    mk(fltPolarCoordinates):
      fp.polarMode = if fltS(p, "mode", "rectangularToPolar") == "polarToRectangular": pmPolarToRectangular
                     else: pmRectangularToPolar
  else: filterParamsExt(id, p)

proc applyFilterToSurface*(id: string; params: JsonNode; surf: Surface; bounds: Rect; selection: Option[Surface];
                           canvas: Rect): Option[Surface] =
  ## Runs filter command `id` with JSON `params` on `surf` (the pixel function behind every
  ## `filter.*` command; smart filters re-run it on re-render). `bounds` frames distortions;
  ## `selection` limits the result. Neighbourhood filters repeat the edge pixels of `canvas` ∪ the
  ## surface's content, and the output is clipped to it. None for ids that aren't pixel filters.
  let d = distortApplyToSurface(id, params, surf, canvas)
  if d.isSome: return d
  let l = lensApplyToSurface(id, params, surf, canvas)
  if l.isSome: return l
  const adjPrefix = "image.adjustments."
  if id.startsWith(adjPrefix): return adjustAsFilter(id[adjPrefix.len .. ^1], params, surf)
  if id == PluginRunId: return pluginApplyToSurface(id, params, surf, selection, canvas)
  let fp = filterParamsFor(id, params)
  if fp.isNone: return
  let selBounds = if selection.isSome: some(selection.get.contentBounds) else: none(Rect)
  let content = surf.contentBounds
  let area = outputArea(fp.get, content, bounds, selBounds)
  some(applyIn(surf, fp.get, area, bounds, selection, canvas.union(content)))

proc hasFilterableLayerE*(s: Session): string =
  ## A targeted alpha channel or Quick Mask is filtered instead of the layer (channel_cmds).
  if editsChannel(s): return ""
  let d = s.active
  if d.isNil: return "no document open"
  if d.activeLayer.isNone: return "no active layer"
  let l = d.doc[].findLayer(d.activeLayer.get)
  if l.isNil: return "no active layer"
  if l.content.kind == lkRaster: return ""
  if l.content.kind == lkSmart and l.content.smart.cache.isSome: return ""
  "filters need a pixel layer (active layer is " & l.content.article & " " & l.content.kindName & " layer)"

# ---------------------------------------------------------------- validation

type
  RuleKind = enum rkNumber, rkUnsigned, rkChoice, rkBool
  Rule = object
    case kind: RuleKind
    of rkNumber: lo, hi: float64
    of rkUnsigned: max: uint64
    of rkChoice: choices: string
    of rkBool: discard

proc num(lo, hi: float64): Rule = Rule(kind: rkNumber, lo: lo, hi: hi)
proc unsigned(max: uint64): Rule = Rule(kind: rkUnsigned, max: max)
proc choice(c: string): Rule = Rule(kind: rkChoice, choices: c)
proc boolRule(): Rule = Rule(kind: rkBool)

proc accepts(r: Rule; v: JsonNode): bool =
  case r.kind
  of rkNumber:
    let n = v.asF64
    n.isSome and n.get == n.get and abs(n.get) != Inf and n.get >= r.lo and n.get <= r.hi
  of rkUnsigned:
    let n = v.asU64
    n.isSome and n.get <= r.max
  of rkChoice: v.kind == JString and v.str in r.choices.split('|')
  of rkBool: v.kind == JBool

proc expected(r: Rule): string =
  case r.kind
  of rkNumber:
    if r.lo == float64.low and r.hi == float64.high: "a finite number"
    else: "a number in " & displayFloat(r.lo) & ".." & displayFloat(r.hi)
  of rkUnsigned: "an integer in 0.." & $r.max
  of rkChoice: "one of " & r.choices
  of rkBool: "a boolean"

proc fieldRules(id: string): Option[seq[(string, Rule)]] =
  ## The core filters' public dialog-unit contract (defaults remain in `filterParamsFor`).
  let u32max = uint64(uint32.high)
  case id
  of "filter.blur.gaussianBlur", "filter.other.highPass": some(@[("radius", num(0.1, 1000))])
  of "filter.blur.boxBlur": some(@[("radius", num(1, 2000))])
  of "filter.blur.motionBlur": some(@[("angle", num(-360, 360)), ("distance", num(1, 2000))])
  of "filter.blur.radialBlur":
    some(@[("amount", num(1, 100)), ("method", choice("spin|zoom")), ("quality", choice("draft|good|best")),
           ("centerX", num(0, 1)), ("centerY", num(0, 1))])
  of "filter.blur.surfaceBlur": some(@[("radius", num(1, 100)), ("threshold", num(2, 255))])
  of "filter.sharpen.unsharpMask": some(@[("amount", num(1, 500)), ("radius", num(0.1, 1000)), ("threshold", num(0, 255))])
  of "filter.sharpen.smartSharpen": some(@[("amount", num(1, 500)), ("radius", num(0.1, 64)), ("reduceNoise", num(0, 100))])
  of "filter.noise.addNoise":
    some(@[("amount", num(0.1, 400)), ("distribution", choice("uniform|gaussian")), ("monochromatic", boolRule()),
           ("seed", unsigned(u32max))])
  of "filter.noise.median": some(@[("radius", num(1, 500))])
  of "filter.noise.dustAndScratches": some(@[("radius", num(1, 500)), ("threshold", num(0, 255))])
  of "filter.pixelate.mosaic": some(@[("cellSize", num(2, 200))])
  of "filter.stylize.emboss": some(@[("angle", num(-180, 180)), ("height", num(1, 100)), ("amount", num(1, 500))])
  of "filter.distort.twirl": some(@[("angle", num(-999, 999))])
  of "filter.distort.pinch": some(@[("amount", num(-100, 100))])
  of "filter.distort.spherize": some(@[("amount", num(-100, 100)), ("mode", choice("normal|horizontalOnly|verticalOnly"))])
  of "filter.distort.wave":
    some(@[("generators", num(1, 999)), ("wavelengthMin", num(1, 998)), ("wavelengthMax", num(2, 999)),
           ("amplitudeMin", num(1, 998)), ("amplitudeMax", num(1, 999)), ("type", choice("sine|triangle|square")),
           ("undefinedAreas", choice("wrap|repeat")), ("seed", unsigned(u32max))])
  of "filter.distort.ripple": some(@[("amount", num(-999, 999)), ("size", choice("small|medium|large"))])
  of "filter.distort.polarCoordinates": some(@[("mode", choice("rectangularToPolar|polarToRectangular"))])
  of "filter.other.minimum", "filter.other.maximum": some(@[("radius", num(0.2, 500)), ("preserve", choice("squareness|roundness"))])
  of "filter.other.offset":
    # The hint specifies pixels, with no range: keep the existing rounding/saturation.
    some(@[("horizontal", num(float64.low, float64.high)), ("vertical", num(float64.low, float64.high)),
           ("undefinedAreas", choice("wrap|repeat|transparent"))])
  of "filter.blur.blur", "filter.blur.blurMore", "filter.sharpen.sharpen", "filter.sharpen.sharpenMore",
     "filter.sharpen.sharpenEdges", "filter.noise.despeckle", "filter.stylize.findEdges", "filter.stylize.solarize":
    some(newSeq[(string, Rule)]())
  else: none(seq[(string, Rule)])

proc choiceRules(id: string): Option[seq[(string, string)]] =
  ## Extended filters: only their documented choices are checked, since `filtersPrepare` injects
  ## extra keys and their numeric ranges clamp in `filterParamsFor`.
  case id
  of "filter.pixelate.mezzotint":
    some(@[("type", "fineDots|mediumDots|grainyDots|coarseDots|shortLines|mediumLines|longLines|shortStrokes|mediumStrokes|longStrokes")])
  of "filter.stylize.diffuse": some(@[("mode", "normal|darkenOnly|lightenOnly|anisotropic")])
  of "filter.stylize.extrude": some(@[("type", "blocks|pyramids"), ("depthMode", "random|levelBased")])
  of "filter.stylize.tiles": some(@[("fill", "background|foreground|inverse|unaltered")])
  of "filter.stylize.traceContour": some(@[("edge", "lower|upper")])
  of "filter.stylize.wind": some(@[("method", "wind|blast|stagger"), ("direction", "fromRight|fromLeft")])
  of "filter.distort.displace": some(@[("fit", "stretch|tile"), ("undefinedAreas", "repeat|wrap")])
  of "filter.distort.shear": some(@[("undefinedAreas", "wrap|repeat")])
  of "filter.distort.zigZag": some(@[("style", "pondRipples|outFromCenter|aroundCenter")])
  of "filter.render.lensFlare": some(@[("lens", "zoom|prime35|prime105|moviePrime")])
  of "filter.render.lightingEffects": some(@[("lightType", "spot|point|infinite"), ("texture", "none|red|green|blue|alpha|luminance")])
  of "filter.blur.smartBlur": some(@[("quality", "high|medium|low"), ("mode", "normal|edgeOnly|overlayEdge")])
  of "filter.blur.lensBlur":
    some(@[("shape", "hexagon|triangle|square|pentagon|heptagon|octagon"), ("depthMap", "none|transparency|layerMask"),
           ("distribution", "uniform|gaussian")])
  of "filter.blur.shapeBlur": some(@[("shape", "circle|ring|square|diamond|triangle|hexagon|star|heart|cross")])
  of "filter.other.hsbHsl": some(@[("inputMode", "rgb|hsb|hsl"), ("rowOrder", "hsb|hsl|rgb")])
  of "filter.video.deInterlace": some(@[("eliminate", "oddFields|evenFields"), ("createBy", "interpolation|duplication")])
  else: none(seq[(string, string)])

proc filtersValidateParams*(id: string; params: JsonNode) =
  ## Runs at dispatch and in the shared runner (direct specs / Last Filter). Filters outside the
  ## core and extended families are unaffected.
  let cr = choiceRules(id)
  if cr.isSome:
    for (key, allowed) in cr.get:
      let v = params.pget(key)
      if not v.isNil and not choice(allowed).accepts(v):
        raise bad(id, "`" & key & "` must be " & choice(allowed).expected)
    return
  let fields = fieldRules(id)
  if fields.isNone:
    galleryValidateParams(id, params)
    return
  if params.isNull: return
  if not params.isObject: raise bad(id, "params must be a JSON object")
  for key, value in params.sortedPairs:
    var valid = false
    var exp = ""
    var found = false
    for (name, rule) in fields.get:
      if name == key:
        (valid, exp, found) = (rule.accepts(value), rule.expected, true)
        break
    if not found:
      case key
      of "layer": (valid, exp) = (value.asU64.isSome, "an unsigned layer id")
      of "coalesce": (valid, exp) = (value.kind == JString, "a string")
      of "target":
        let okStr = value.kind == JString and value.str in ["pixels", "mask", "quickMask"]
        let okObj = value.isObject and value.len == 1 and value.pget("channel").asU64.isSome
        (valid, exp) = (okStr or okObj, "pixels, mask, quickMask or {\"channel\":index}")
      else: raise bad(id, "unknown parameter `" & key & "`")
    if not valid: raise bad(id, "`" & key & "` must be " & exp)

# ---------------------------------------------------------------- running

proc keepAlpha(old: Surface; new: var Surface) =
  ## Lock transparency: put the layer's alpha back after a filter, in the tiles it changed.
  if old.format != new.format or not new.format.alpha: return
  var coords: seq[TileCoord]
  for c, t in new.tiles.pairs:
    let o = old.tile(c)
    if o.isNil or o != t: coords.add c
  for c in old.tiles.keys:
    if new.tile(c).isNil: coords.add c
  let n = new.channels
  for c in coords:
    let r = c.rect
    let o = old.readRegion(r)
    var v = new.readRegion(r)
    var i = n - 1
    while i < v.len and i < o.len:
      v[i] = o[i]
      i += n
    new.writeRegion(r, v)
  new.prune()

proc runFilter*(s: var Session; id: string; p: JsonNode): JsonNode =
  filtersValidateParams(id, p)
  # Session state some filters read (colours) becomes explicit params; external inputs resolve here.
  let prepared = filtersPrepare(s, id, p)
  let p = prepared
  let fp0 = filterParamsFor(id, p)
  if fp0.isNone: raise other("unknown filter " & id)
  var fp = fp0.get
  filtersResolve(s, fp, p)
  var layer: LayerId
  let lv = p.pget("layer").asU64
  if lv.isSome: layer = LayerId(lv.get)
  else:
    let st = s.active
    if st.isNil or st.activeLayer.isNone: raise other("no active layer")
    layer = st.activeLayer.get
  let pre = filterPreset(id)
  let label = if pre.isSome: pre.get[0] else: fp.label
  let msg = label
  var params = p.copy
  params.dropKey("__kind")
  let fid = id
  let layerId = uint64(layer)
  editJob(s, label,
    proc(doc: var Document; active: var Option[LayerId]; ctx: JobCtx): FilterParams =
      proc filter(surf: Surface; fp: FilterParams; area, bounds: Rect; sel: Option[Surface]; extent: Rect): Surface =
        let r = applyInWith(surf, fp, area, bounds, sel, extent, ctx.interrupt(0, 1, msg))
        if r.isNone: raise cancelledErr()
        r.get
      let selection = doc.selection
      let selBounds = if selection.isSome: some(selection.get.contentBounds) else: none(Rect)
      # Distortions centre on the selection when there is one, else the canvas.
      let docBounds = doc.bounds
      let bounds = if selBounds.isSome and not selBounds.get.isEmpty: selBounds.get else: docBounds
      # Neighbourhood filters repeat edge pixels at the canvas edge (or past it, where the layer
      # has off-canvas pixels) instead of fading in transparency, as Photoshop does.
      let chSurf = channelSurfaceForFilter(doc, some(layer), p)
      if not chSurf.isNil:
        let content = docBounds.union(chSurf[].contentBounds)
        let area = outputArea(fp, content, bounds, selBounds)
        chSurf[] = filter(chSurf[], fp, area, bounds, selection, docBounds.union(content))
        return fp
      # Filters that make pixels transparent (Color to Alpha) turn the Background into a normal
      # layer first, as the erasers do: it can't hold transparency.
      if fp.makesTransparency: backgroundToLayerForMask(doc, layer)
      let locks = doc.effectiveLocks(layer)
      let l = doc.findLayer(layer)
      if l.isNil: raise noLayer(layer)
      if locks.pixels or locks.all:
        raise other("Could not complete your request because the layer \"" & l.name & "\" is locked")
      # Putting the old alpha back (below) would leave unmixed colours fully opaque.
      if locks.transparency and fp.makesTransparency and l.content.kind == lkRaster:
        raise other("Could not complete your request because the transparency of layer \"" & l.name & "\" is locked")
      var fp2 = fp
      resolveInLayer(fp2, l[], bounds)
      case l.content.kind
      of lkRaster: discard
      of lkSmart:
        # Non-destructive: record the filter and re-render the smart object from its source.
        let sf = SmartFilter(command: fid, params: params, blend: bmNormal, opacity: 1, visible: true)
        addSmartFilter(doc, layer, sf, selection)
        return fp2
      else: raise other("not a pixel layer")
      let surf = addr l.content.raster
      let before = if locks.transparency: some(surf[]) else: none(Surface)
      let content = surf[].contentBounds
      let area = outputArea(fp2, content, bounds, selBounds)
      surf[] = filter(surf[], fp2, area, bounds, selection, docBounds.union(content))
      if before.isSome: keepAlpha(before.get, surf[])
      fp2,
    proc(fp: FilterParams): JsonNode = %*{"layer": layerId, "filter": fp.toJ})

proc lastFilter*(s: Session): Option[(string, JsonNode)] =
  ## The most recent filter command in the session journal, retargeted at the active layer.
  for i in countdown(s.journal.len - 1, 0):
    let (id, params) = s.journal[i]
    if filterParamsFor(id, newJNull()).isSome and id.startsWith("filter."):
      var q = params.copy
      q.dropKey("layer")
      return some((id, q))

proc filterSpec(id, label: string; menu: openArray[string]; params: string): CommandSpec =
  cmd(id, label, menu, "", params, hasFilterableLayerE, runProc(runFilter(s, id, p)))

proc filterSpecs*(): seq[CommandSpec] =
  result = @[
    filterSpec("filter.blur.gaussianBlur", "Gaussian Blur…", ["Filter", "Blur"], """{"radius":0.1..1000=1}"""),
    filterSpec("filter.blur.blur", "Blur", ["Filter", "Blur"], "{}"),
    filterSpec("filter.blur.blurMore", "Blur More", ["Filter", "Blur"], "{}"),
    filterSpec("filter.sharpen.sharpen", "Sharpen", ["Filter", "Sharpen"], "{}"),
    filterSpec("filter.sharpen.sharpenMore", "Sharpen More", ["Filter", "Sharpen"], "{}"),
    filterSpec("filter.sharpen.sharpenEdges", "Sharpen Edges", ["Filter", "Sharpen"], "{}"),
    filterSpec("filter.noise.despeckle", "Despeckle", ["Filter", "Noise"], "{}"),
    filterSpec("filter.blur.boxBlur", "Box Blur…", ["Filter", "Blur"], """{"radius":1..2000=1}"""),
    filterSpec("filter.blur.motionBlur", "Motion Blur…", ["Filter", "Blur"], """{"angle":-360..360=0,"distance":1..2000=10}"""),
    filterSpec("filter.blur.radialBlur", "Radial Blur…", ["Filter", "Blur"],
      """{"amount":1..100=10,"method":"spin|zoom","quality":"draft|good|best"="good","centerX":0..1=0.5,"centerY":0..1=0.5}"""),
    filterSpec("filter.blur.surfaceBlur", "Surface Blur…", ["Filter", "Blur"], """{"radius":1..100=5,"threshold":2..255=15}"""),
    filterSpec("filter.sharpen.unsharpMask", "Unsharp Mask…", ["Filter", "Sharpen"],
      """{"amount":1..500=50,"radius":0.1..1000=1,"threshold":0..255=0}"""),
    filterSpec("filter.sharpen.smartSharpen", "Smart Sharpen…", ["Filter", "Sharpen"],
      """{"amount":1..500=100,"radius":0.1..64=1,"reduceNoise":0..100=10}"""),
    filterSpec("filter.noise.addNoise", "Add Noise…", ["Filter", "Noise"],
      """{"amount":0.1..400=12.5,"distribution":"uniform|gaussian","monochromatic":bool,"seed":u32=0}"""),
    filterSpec("filter.noise.median", "Median…", ["Filter", "Noise"], """{"radius":1..500=1}"""),
    filterSpec("filter.noise.dustAndScratches", "Dust & Scratches…", ["Filter", "Noise"], """{"radius":1..500=1,"threshold":0..255=0}"""),
    filterSpec("filter.pixelate.mosaic", "Mosaic…", ["Filter", "Pixelate"], """{"cellSize":2..200=10}"""),
    filterSpec("filter.stylize.emboss", "Emboss…", ["Filter", "Stylize"], """{"angle":-180..180=135,"height":1..100=3,"amount":1..500=100}"""),
    filterSpec("filter.stylize.findEdges", "Find Edges", ["Filter", "Stylize"], "{}"),
    filterSpec("filter.stylize.solarize", "Solarize", ["Filter", "Stylize"], "{}"),
    filterSpec("filter.distort.twirl", "Twirl…", ["Filter", "Distort"], """{"angle":-999..999=50}"""),
    filterSpec("filter.distort.pinch", "Pinch…", ["Filter", "Distort"], """{"amount":-100..100=50}"""),
    filterSpec("filter.distort.spherize", "Spherize…", ["Filter", "Distort"], """{"amount":-100..100=100,"mode":"normal|horizontalOnly|verticalOnly"}"""),
    filterSpec("filter.distort.wave", "Wave…", ["Filter", "Distort"],
      """{"generators":1..999=5,"wavelengthMin":1..998=10,"wavelengthMax":2..999=120,"amplitudeMin":1..998=5,"amplitudeMax":1..999=35,"type":"sine|triangle|square","undefinedAreas":"wrap|repeat","seed":u32=0}"""),
    filterSpec("filter.distort.ripple", "Ripple…", ["Filter", "Distort"], """{"amount":-999..999=100,"size":"small|medium|large"}"""),
    filterSpec("filter.distort.polarCoordinates", "Polar Coordinates…", ["Filter", "Distort"], """{"mode":"rectangularToPolar|polarToRectangular"}"""),
    filterSpec("filter.other.highPass", "High Pass…", ["Filter", "Other"], """{"radius":0.1..1000=10}"""),
    filterSpec("filter.other.minimum", "Minimum…", ["Filter", "Other"], """{"radius":0.2..500=1,"preserve":"squareness|roundness"}"""),
    filterSpec("filter.other.maximum", "Maximum…", ["Filter", "Other"], """{"radius":0.2..500=1,"preserve":"squareness|roundness"}"""),
    filterSpec("filter.other.offset", "Offset…", ["Filter", "Other"],
      """{"horizontal":px=0,"vertical":px=0,"undefinedAreas":"wrap|repeat|transparent"}"""),
  ]
  result.add cmd("filter.lastFilter", "Last Filter", ["Filter"], "Cmd+Alt+F", "{}",
    proc(s: Session): string =
      let why = hasFilterableLayerE(s)
      if why.len > 0: return why
      if lastFilter(s).isNone: "no filter applied yet" else: "",
    runProc do:
      let lf = lastFilter(s)
      if lf.isNone: raise other("no filter applied yet")
      runFilter(s, lf.get[0], lf.get[1]))
