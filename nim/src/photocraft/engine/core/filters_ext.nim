## The second batch of `filter.*` commands (port of `filters_ext.rs`): Pixelate, Stylize, Distort
## (Displace, Shear, ZigZag), Render (Fibers, Lens Flare, Lighting Effects, Relight), Reduce
## Noise, Smart/Lens/Shape Blur, Blur Gallery, Other (Custom, HSB/HSL) and Video.
##
## They run through `runFilter` (selection, smart objects, history and `filter.lastFilter` work
## the same as for every other filter). This module adds what some of them need from the
## session: the foreground/background colours, a displacement map from another document or a
## file, and a layer mask as Lens Blur depth. Part of the `engine/core` unit: included, not
## imported.

proc galleryParamsFor*(id: string; p: JsonNode): Option[FilterParams]
proc galleryUsesColours*(id: string): bool
proc colorToAlphaParams*(p: JsonNode): FilterParams
proc fileImport*(name: string; bytes: openArray[byte]): Document

const
  ColorToAlphaId* = "filter.other.colorToAlpha"
  FxBlack = [0'f32, 0, 0, 1]
  FxWhite = [1'f32, 1, 1, 1]

proc fxI(p: JsonNode; k: string; d: int64): int64 =
  let v = fltI64(p, k)
  if v.isSome: v.get else: d

proc fxSeed(p: JsonNode): uint32 = uint32(cast[uint64](fxI(p, "seed", 0)) and 0xFFFF_FFFF'u64)

proc fxColour*(v: JsonNode): Option[array[4, float32]] =
  ## Straight RGBA colour from `[r, g, b(, a)]` (0–1) or `#rrggbb`.
  if v.isNil: return
  if v.isArray and v.len >= 3:
    proc c(a: JsonNode; j: int; d: float32): float32 =
      let x = if j < a.len: a.elems[j].asF64 else: none(float64)
      if x.isSome: float32(x.get) else: d
    return some([c(v, 0, 0), c(v, 1, 0), c(v, 2, 0), c(v, 3, 1)])
  if v.kind == JString and v.str.len == 7 and v.str.startsWith("#"):
    let h = v.str
    proc ch(h: string; j: int): float32 =
      let b = hexByte(h[j ..< j + 2])
      if b.isSome: float32(b.get) / 255 else: 0'f32
    return some([ch(h, 1), ch(h, 3), ch(h, 5), 1'f32])

proc fxColourOr(p: JsonNode; k: string; d: array[4, float32]): array[4, float32] =
  let c = fxColour(p.pget(k))
  if c.isSome: c.get else: d

proc fxUndefined(p: JsonNode; d: string): UndefinedAreas =
  case fltS(p, "undefinedAreas", d)
  of "repeat": uaRepeat
  of "transparent": uaTransparent
  else: uaWrap

proc fxList[T](p: JsonNode; k: string): Option[seq[T]] =
  ## A JSON array param deserialized into `T`s (invalid entries are skipped).
  let a = p.pget(k)
  if not a.isArray: return
  var v: seq[T]
  for e in a.elems:
    try: v.add fromJson(e, T)
    except SerdeError, ValueError, KeyError: discard
  if v.len > 0: some(v) else: none(seq[T])

proc filterParamsExt*(id: string; p: JsonNode): Option[FilterParams] =
  ## Algorithm parameters for the ids this module owns (Photoshop dialog units).
  proc blur(p: JsonNode; d: float32): float32 = clamp(fltF(p, "blur", d), 0'f32, 500'f32)
  template mk(k: FilterKind; body: untyped): untyped =
    block:
      var fp {.inject.} = newFilterParams(k)
      body
      some(fp)
  case id
  # ---- Pixelate ----
  of "filter.pixelate.colorHalftone":
    mk(fltColorHalftone):
      fp.maxRadius = clamp(fltF(p, "maxRadius", 8), 4'f32, 127'f32)
      fp.angles = [fltF(p, "channel1", 108), fltF(p, "channel2", 162), fltF(p, "channel3", 90), fltF(p, "channel4", 45)]
  of "filter.pixelate.crystallize":
    mk(fltCrystallize):
      fp.cellSize = clamp(fltF(p, "cellSize", 10), 3'f32, 300'f32)
      fp.seed = fxSeed(p)
  of "filter.pixelate.facet": some(newFilterParams(fltFacet))
  of "filter.pixelate.fragment": some(newFilterParams(fltFragment))
  of "filter.pixelate.mezzotint":
    mk(fltMezzotint):
      fp.mezzotintKind = case fltS(p, "type", "fineDots")
                         of "mediumDots": mzMediumDots
                         of "grainyDots": mzGrainyDots
                         of "coarseDots": mzCoarseDots
                         of "shortLines": mzShortLines
                         of "mediumLines": mzMediumLines
                         of "longLines": mzLongLines
                         of "shortStrokes": mzShortStrokes
                         of "mediumStrokes": mzMediumStrokes
                         of "longStrokes": mzLongStrokes
                         else: mzFineDots
      fp.seed = fxSeed(p)
  of "filter.pixelate.pointillize":
    mk(fltPointillize):
      fp.cellSize = clamp(fltF(p, "cellSize", 5), 3'f32, 300'f32)
      fp.seed = fxSeed(p)
      fp.background = fxColourOr(p, "background", FxWhite)
  # ---- Stylize ----
  of "filter.stylize.diffuse":
    mk(fltDiffuse):
      fp.diffuseMode = case fltS(p, "mode", "normal")
                       of "darkenOnly": dmDarkenOnly
                       of "lightenOnly": dmLightenOnly
                       of "anisotropic": dmAnisotropic
                       else: dmNormal
      fp.seed = fxSeed(p)
  of "filter.stylize.extrude":
    mk(fltExtrude):
      fp.extrudeKind = if fltS(p, "type", "blocks") == "pyramids": etPyramids else: etBlocks
      fp.size = clamp(fltF(p, "size", 30), 2'f32, 255'f32)
      fp.depth = clamp(fltF(p, "depth", 30), 1'f32, 255'f32)
      fp.levelBased = fltS(p, "depthMode", "random") == "levelBased"
      fp.solidFront = fltB(p, "solidFrontFaces", false)
      fp.maskIncomplete = fltB(p, "maskIncompleteBlocks", false)
      fp.seed = fxSeed(p)
  of "filter.stylize.oilPaint":
    mk(fltOilPaint):
      fp.stylization = clamp(fltF(p, "stylization", 4), 0.1'f32, 10'f32)
      fp.cleanliness = clamp(fltF(p, "cleanliness", 5), 0'f32, 10'f32)
      fp.scale = clamp(fltF(p, "scale", 1), 0.1'f32, 10'f32)
      fp.bristleDetail = clamp(fltF(p, "bristleDetail", 5), 0'f32, 10'f32)
      fp.lighting = fltB(p, "lighting", true)
      fp.angle = fltF(p, "angle", -60)
      fp.shine = clamp(fltF(p, "shine", 1), 0'f32, 10'f32)
  of "filter.stylize.tiles":
    mk(fltTiles):
      fp.count = uint32(clamp(fxI(p, "count", 10), 1'i64, 99'i64))
      fp.maxOffset = clamp(fltF(p, "maxOffset", 10), 1'f32, 99'f32)
      fp.fill = case fltS(p, "fill", "background")
                of "foreground": tfForeground
                of "inverse": tfInverse
                of "unaltered": tfUnaltered
                else: tfBackground
      fp.foreground = fxColourOr(p, "foreground", FxBlack)
      fp.background = fxColourOr(p, "background", FxWhite)
      fp.seed = fxSeed(p)
  of "filter.stylize.traceContour":
    mk(fltTraceContour):
      fp.level = clamp(fltF(p, "level", 128), 0'f32, 255'f32)
      fp.upper = fltS(p, "edge", "lower") == "upper"
  of "filter.stylize.wind":
    mk(fltWind):
      fp.windMethod = case fltS(p, "method", "wind")
                      of "blast": wmBlast
                      of "stagger": wmStagger
                      else: wmWind
      fp.fromRight = fltS(p, "direction", "fromRight") != "fromLeft"
      fp.seed = fxSeed(p)
  # ---- Distort ----
  of "filter.distort.displace":
    mk(fltDisplace):
      fp.horizontal = clamp(fltF(p, "horizontal", 10), -999'f32, 999'f32)
      fp.vertical = clamp(fltF(p, "vertical", 10), -999'f32, 999'f32)
      fp.stretch = fltS(p, "fit", "stretch") != "tile"
      fp.undefined = fxUndefined(p, "repeat")
  of "filter.distort.shear":
    mk(fltShear):
      let pts = fxList[array[2, float32]](p, "points")
      if pts.isSome: fp.points = pts.get
      else:
        let a = clamp(fltF(p, "amount", 0), -100'f32, 100'f32) / 100
        fp.points = @[[0'f32, 0], [0.5'f32, a], [1'f32, 0]]
      fp.undefined = fxUndefined(p, "wrap")
  of "filter.distort.zigZag":
    mk(fltZigZag):
      fp.amount = clamp(fltF(p, "amount", 10), -100'f32, 100'f32)
      fp.ridges = clamp(fltF(p, "ridges", 5), 0'f32, 20'f32)
      fp.zigzagStyle = case fltS(p, "style", "pondRipples")
                       of "outFromCenter": zzOutFromCenter
                       of "aroundCenter": zzAroundCenter
                       else: zzPondRipples
  # ---- Render ----
  of "filter.render.fibers":
    mk(fltFibers):
      fp.variance = clamp(fltF(p, "variance", 16), 1'f32, 64'f32)
      fp.strength = clamp(fltF(p, "strength", 4), 1'f32, 64'f32)
      fp.seed = fxSeed(p)
      fp.foreground = fxColourOr(p, "foreground", FxBlack)
      fp.background = fxColourOr(p, "background", FxWhite)
  of "filter.render.lensFlare":
    mk(fltLensFlare):
      fp.brightness = clamp(fltF(p, "brightness", 100), 10'f32, 300'f32)
      fp.centerX = clamp(fltF(p, "centerX", 0.5), -0.5'f32, 1.5'f32)
      fp.centerY = clamp(fltF(p, "centerY", 0.5), -0.5'f32, 1.5'f32)
      fp.lens = case fltS(p, "lens", "zoom")
                of "prime35": ltPrime35
                of "prime105": ltPrime105
                of "moviePrime": ltMoviePrime
                else: ltZoom
  of "filter.render.relight":
    mk(fltRelight):
      fp.angle = clamp(fltF(p, "angle", 45), -180'f32, 180'f32)
      fp.elevation = clamp(fltF(p, "elevation", 40), 0'f32, 90'f32)
      fp.intensity = clamp(fltF(p, "intensity", 40), 0'f32, 100'f32)
      fp.ambient = clamp(fltF(p, "ambient", 55), 0'f32, 100'f32)
      fp.warmth = clamp(fltF(p, "warmth", 0), -100'f32, 100'f32)
      fp.softness = clamp(fltF(p, "softness", 25), 1'f32, 100'f32)
  of "filter.render.lightingEffects":
    mk(fltLightingEffects):
      let lights = fxList[Light](p, "lights")
      if lights.isSome: fp.lights = lights.get
      else:
        let c = fxColourOr(p, "color", FxWhite)
        fp.lights = @[Light(
          kind: (case fltS(p, "lightType", "spot")
                 of "point": lkPoint
                 of "infinite": lkInfinite
                 else: lkSpot),
          x: fltF(p, "lightX", 0.25), y: fltF(p, "lightY", 0.2), z: clamp(fltF(p, "lightZ", 0.6), 0'f32, 10'f32),
          targetX: fltF(p, "targetX", 0.5), targetY: fltF(p, "targetY", 0.55), angle: fltF(p, "angle", 135),
          elevation: clamp(fltF(p, "elevation", 45), 0'f32, 90'f32), color: [c[0], c[1], c[2]],
          intensity: clamp(fltF(p, "intensity", 75), -100'f32, 100'f32), cone: clamp(fltF(p, "cone", 45), 1'f32, 89'f32),
          hotspot: clamp(fltF(p, "hotspot", 50), 0'f32, 100'f32), radius: clamp(fltF(p, "radius", 0.8), 0.01'f32, 10'f32))]
      fp.gloss = clamp(fltF(p, "gloss", 0), -100'f32, 100'f32)
      fp.metallic = clamp(fltF(p, "metallic", 0), -100'f32, 100'f32)
      fp.exposure = clamp(fltF(p, "exposure", 0), -100'f32, 100'f32)
      fp.ambience = clamp(fltF(p, "ambience", 8), -100'f32, 100'f32)
      fp.texture = case fltS(p, "texture", "none")
                   of "red": tcRed
                   of "green": tcGreen
                   of "blue": tcBlue
                   of "alpha": tcAlpha
                   of "luminance": tcLuminance
                   else: tcNone
      fp.height = clamp(fltF(p, "height", 50), 0'f32, 100'f32)
      fp.whiteIsHigh = fltB(p, "whiteIsHigh", true)
  # ---- Noise ----
  of "filter.noise.reduceNoise":
    mk(fltReduceNoise):
      fp.strength = clamp(fltF(p, "strength", 6), 0'f32, 10'f32)
      fp.preserveDetails = clamp(fltF(p, "preserveDetails", 60), 0'f32, 100'f32)
      fp.reduceColorNoise = clamp(fltF(p, "reduceColorNoise", 45), 0'f32, 100'f32)
      fp.sharpenDetails = clamp(fltF(p, "sharpenDetails", 25), 0'f32, 100'f32)
      fp.removeJpegArtifact = fltB(p, "removeJpegArtifact", false)
  # ---- Blur ----
  of "filter.blur.smartBlur":
    mk(fltSmartBlur):
      fp.radius = clamp(fltF(p, "radius", 3), 0.1'f32, 100'f32)
      fp.threshold = clamp(fltF(p, "threshold", 25), 0.1'f32, 100'f32)
      fp.blurQuality = case fltS(p, "quality", "high")
                       of "low": bqLow
                       of "medium": bqMedium
                       else: bqHigh
      fp.smartBlurMode = case fltS(p, "mode", "normal")
                         of "edgeOnly": sbEdgeOnly
                         of "overlayEdge": sbOverlayEdge
                         else: sbNormal
  of "filter.blur.lensBlur":
    mk(fltLensBlur):
      fp.radius = clamp(fltF(p, "radius", 15), 0'f32, 100'f32)
      fp.blades = case fltS(p, "shape", "hexagon")
                  of "triangle": 3'u32
                  of "square": 4'u32
                  of "pentagon": 5'u32
                  of "heptagon": 7'u32
                  of "octagon": 8'u32
                  else: 6'u32
      fp.curvature = clamp(fltF(p, "bladeCurvature", 0), 0'f32, 100'f32)
      fp.rotation = fltF(p, "rotation", 0)
      fp.depthSource = case fltS(p, "depthMap", "none")
                       of "transparency": dsTransparency
                       of "layerMask": dsLayerMask
                       else: dsNone
      fp.focalDistance = clamp(fltF(p, "focalDistance", 0), 0'f32, 255'f32)
      fp.invertDepth = fltB(p, "invert", false)
      fp.brightness = clamp(fltF(p, "brightness", 0), 0'f32, 100'f32)
      fp.threshold = clamp(fltF(p, "threshold", 255), 0'f32, 255'f32)
      fp.noise = clamp(fltF(p, "noise", 0), 0'f32, 100'f32)
      fp.distribution = if fltS(p, "distribution", "uniform") == "gaussian": diGaussian else: diUniform
      fp.monochromatic = fltB(p, "monochromatic", false)
      fp.seed = fxSeed(p)
  of "filter.blur.shapeBlur":
    mk(fltShapeBlur):
      fp.radius = clamp(fltF(p, "radius", 10), 5'f32, 1000'f32)
      fp.shape = case fltS(p, "shape", "circle")
                 of "ring": bshRing
                 of "square": bshSquare
                 of "diamond": bshDiamond
                 of "triangle": bshTriangle
                 of "hexagon": bshHexagon
                 of "star": bshStar
                 of "heart": bshHeart
                 of "cross": bshCross
                 else: bshCircle
  # ---- Blur Gallery ----
  of "filter.blurGallery.tiltShift":
    mk(fltTiltShift):
      fp.blur = blur(p, 15)
      fp.centerX = fltF(p, "centerX", 0.5)
      fp.centerY = fltF(p, "centerY", 0.5)
      fp.angle = fltF(p, "angle", 0)
      fp.focus = clamp(fltF(p, "focus", 0.1), 0'f32, 2'f32)
      fp.transition = clamp(fltF(p, "transition", 0.15), 0.001'f32, 2'f32)
  of "filter.blurGallery.irisBlur":
    mk(fltIrisBlur):
      let pins = fxList[IrisPin](p, "pins")
      fp.irisPins = if pins.isSome: pins.get
                    else: @[IrisPin(x: fltF(p, "centerX", 0.5), y: fltF(p, "centerY", 0.5),
                                    radiusX: rmax(fltF(p, "radiusX", 0.35), 0.001'f32), radiusY: rmax(fltF(p, "radiusY", 0.25), 0.001'f32),
                                    angle: fltF(p, "angle", 0), roundness: clamp(fltF(p, "roundness", 0), 0'f32, 100'f32),
                                    feather: clamp(fltF(p, "feather", 0.5), 0'f32, 0.99'f32), blur: blur(p, 15))]
  of "filter.blurGallery.fieldBlur":
    mk(fltFieldBlur):
      let pins = fxList[FieldPin](p, "pins")
      fp.fieldPins = if pins.isSome: pins.get
                     else: @[FieldPin(x: fltF(p, "centerX", 0.5), y: fltF(p, "centerY", 0.5), blur: blur(p, 15))]
  of "filter.blurGallery.spinBlur":
    mk(fltSpinBlur):
      let pins = fxList[SpinPin](p, "pins")
      if pins.isSome:
        fp.spinPins = pins.get
        for pin in fp.spinPins.mitems:
          pin.angle = if pin.angle == pin.angle and abs(pin.angle) != Inf: clamp(pin.angle, -360'f32, 360'f32) else: 0'f32
          pin.blurAngle = if pin.blurAngle == pin.blurAngle and abs(pin.blurAngle) != Inf: clamp(pin.blurAngle, 0'f32, 360'f32) else: 0'f32
      else:
        let a = fltF(p, "angle", 0)
        fp.spinPins = @[SpinPin(x: fltF(p, "centerX", 0.5), y: fltF(p, "centerY", 0.5),
                                radiusX: rmax(fltF(p, "radiusX", 0.3), 0.001'f32), radiusY: rmax(fltF(p, "radiusY", 0.3), 0.001'f32),
                                angle: (if a == a and abs(a) != Inf: clamp(a, -360'f32, 360'f32) else: 0'f32),
                                blurAngle: clamp(fltF(p, "blurAngle", 15), 0'f32, 360'f32))]
  of "filter.blurGallery.pathBlur":
    mk(fltPathBlur):
      let paths = fxList[BlurPath](p, "paths")
      if paths.isSome:
        # Entry speeds bypass the scalar clamp; the halo casts one to i32 and adds (#708).
        fp.paths = paths.get
        for b in fp.paths.mitems: b.speed = clamp(b.speed, 0'f32, 500'f32)
      else:
        fp.paths = @[BlurPath(points: @[[fltF(p, "startX", 0.2), fltF(p, "startY", 0.5)], [fltF(p, "endX", 0.8), fltF(p, "endY", 0.5)]],
                              speed: clamp(fltF(p, "speed", 50), 0'f32, 500'f32), taper: clamp(fltF(p, "taper", 0), 0'f32, 100'f32))]
  # ---- Other ----
  of "filter.other.custom":
    mk(fltCustom):
      let k = p.pget("kernel")
      if k.isArray:
        fp.kernel = @[]
        for i in 0 ..< min(k.len, 25):
          let v = k.elems[i].asF64
          fp.kernel.add float32(clamp(if v.isSome: v.get else: 0'f64, -999'f64, 999'f64))
      else:
        fp.kernel = newSeq[float32](25)
        fp.kernel[12] = 1
      fp.scale = clamp(fltF(p, "scale", 1), 1'f32, 9999'f32)
      fp.offset = clamp(fltF(p, "offset", 0), -9999'f32, 9999'f32)
  of "filter.other.hsbHsl":
    proc m(p: JsonNode; k, d: string): HsbModel =
      case fltS(p, k, d)
      of "hsb": hmHsb
      of "hsl": hmHsl
      else: hmRgb
    mk(fltHsbHsl):
      fp.input = m(p, "inputMode", "rgb")
      fp.output = m(p, "rowOrder", "hsb")
  of ColorToAlphaId: some(colorToAlphaParams(p))
  # ---- Video ----
  of "filter.video.deInterlace":
    mk(fltDeInterlace):
      fp.eliminateEven = fltS(p, "eliminate", "oddFields") == "evenFields"
      fp.interpolate = fltS(p, "createBy", "interpolation") != "duplication"
  of "filter.video.ntscColors": some(newFilterParams(fltNtscColors))
  else: galleryParamsFor(id, p)

proc filterUsesColours(id: string): bool =
  ## Ids whose rendering uses the foreground/background colours.
  id in ["filter.render.fibers", "filter.stylize.tiles", "filter.pixelate.pointillize"] or galleryUsesColours(id)

proc colourJson(c: array[4, float32]): JsonNode = %*[float64(c[0]), float64(c[1]), float64(c[2]), float64(c[3])]

proc filtersPrepare*(s: Session; id: string; p: JsonNode): JsonNode =
  ## Fills in session state a filter reads (the current colours) as explicit params, so the
  ## recorded params (journal, smart filters) reproduce the result.
  result = if p.isObject: p.copy else: newJObject()
  if filterUsesColours(id):
    if not result.hasKey("foreground"): result["foreground"] = colourJson(s.tools.foreground)
    if not result.hasKey("background"): result["background"] = colourJson(s.tools.background)

proc mapFromDocument(doc: Document; layer: Option[uint64]): Option[Image] =
  ## A flattened document (or one of its layers) as an RGBA map image.
  let l = if layer.isSome: doc.findLayer(LayerId(layer.get)) else: nil
  var buf: Buffer
  if not l.isNil:
    let r = l[].surface.contentBounds
    buf = renderLayer(l[], if r.isEmpty: doc.bounds else: r)
  else: buf = flatten(doc)
  if buf.rect.isEmpty: return
  var data = newSeqOfCap[float32](buf.px.len * 4)
  for q in buf.px:
    for c in q: data.add c
  some(Image(rect: buf.rect, ch: 4, data: data))

proc filtersResolve*(s: Session; fp: var FilterParams; p: JsonNode) =
  ## Resolves inputs that live outside the target layer: Displace's map (`mapPath`, or
  ## `mapDocument` index with optional `mapLayer` id).
  if fp.kind != fltDisplace or not fp.map.isNil: return
  let layer = p.pget("mapLayer").asU64
  var img: Option[Image]
  let path = p.pget("mapPath")
  let mapDoc = p.pget("mapDocument")
  if not path.isNil and path.kind == JString and path.str.len > 0:
    var bytes: seq[byte]
    try: bytes = readBytesRust(path.str)
    except OSError as e: raise other("cannot read displacement map `" & path.str & "`: " & e.msg)
    var imported: Document
    try: imported = fileImport(path.str, bytes)
    except EngineError as e: raise other("cannot open displacement map `" & path.str & "`: " & e.msg)
    img = mapFromDocument(imported, layer)
  elif not mapDoc.isNil:
    var found = -1
    if mapDoc.kind == JString:
      for i, ds in s.docs:
        if ds.doc.name == mapDoc.str or (ds.path.isSome and ds.path.get == mapDoc.str):
          found = i
          break
    else:
      let k = mapDoc.asU64
      if k.isSome and k.get < uint64(s.docs.len): found = int(k.get)
    if found < 0: raise other("displacement map document not found")
    img = mapFromDocument(s.docs[found].doc[], layer)
  else:
    raise other("Displace needs a displacement map: pass `mapPath` or `mapDocument`")
  if img.isNone: raise other("the displacement map is empty")
  new(fp.map)
  fp.map[] = img.get

proc resolveInLayer*(fp: var FilterParams; layer: Layer; bounds: Rect) =
  ## Resolves inputs read from the target layer itself: the layer mask as Lens Blur depth.
  if fp.kind == fltLensBlur and fp.depthSource == dsLayerMask and fp.depthMap.isNil and layer.mask.isSome:
    new(fp.depthMap)
    fp.depthMap[] = readImage(layer.mask.get.surface, bounds)

proc relightBad(msg: string): ref EngineError = bad("filter.render.relight", msg)

proc relightNum(p: JsonNode; key: string; lo, hi: float32) =
  let v = p.pget(key)
  if v.isNil: return
  if v.kind notin {JInt, JFloat}: raise relightBad("\"" & key & "\" must be a number")
  let x = float32(v.asF64.get)
  if x != x or abs(x) == Inf: raise relightBad("\"" & key & "\" must be finite")
  if x < lo or x > hi: raise relightBad("\"" & key & "\" must be in " & displayFloat(lo) & ".." & displayFloat(hi))

proc validateRelight(p: JsonNode) =
  if not p.isObject: raise relightBad("params must be a JSON object")
  relightNum(p, "angle", -180, 180)
  relightNum(p, "elevation", 0, 90)
  relightNum(p, "intensity", 0, 100)
  relightNum(p, "ambient", 0, 100)
  relightNum(p, "warmth", -100, 100)
  relightNum(p, "softness", 1, 100)

proc relightEnabled(s: Session): string =
  let why = hasFilterableLayerE(s)
  if why.len > 0: return why
  if editsChannel(s): return ""
  let (l, lwhy) = activeLayerOf(s)
  if l.isNil: return lwhy
  let d = s.active
  if d.isNil: return "no document open"
  let locks = d.doc[].effectiveLocks(l.id)
  if locks.pixels or locks.all: "the layer \"" & l.name & "\" is locked" else: ""

proc filtersExtSpecs*(): seq[CommandSpec] =
  @[
    # Pixelate
    filterSpec("filter.pixelate.colorHalftone", "Color Halftone…", ["Filter", "Pixelate"],
      """{"maxRadius":4..127=8,"channel1":-360..360=108,"channel2":-360..360=162,"channel3":-360..360=90,"channel4":-360..360=45}"""),
    filterSpec("filter.pixelate.crystallize", "Crystallize…", ["Filter", "Pixelate"], """{"cellSize":3..300=10,"seed":u32=0}"""),
    filterSpec("filter.pixelate.facet", "Facet", ["Filter", "Pixelate"], "{}"),
    filterSpec("filter.pixelate.fragment", "Fragment", ["Filter", "Pixelate"], "{}"),
    filterSpec("filter.pixelate.mezzotint", "Mezzotint…", ["Filter", "Pixelate"],
      """{"type":"fineDots|mediumDots|grainyDots|coarseDots|shortLines|mediumLines|longLines|shortStrokes|mediumStrokes|longStrokes","seed":u32=0}"""),
    filterSpec("filter.pixelate.pointillize", "Pointillize…", ["Filter", "Pixelate"], """{"cellSize":3..300=5,"seed":u32=0,"background":json}"""),
    # Stylize
    filterSpec("filter.stylize.diffuse", "Diffuse…", ["Filter", "Stylize"], """{"mode":"normal|darkenOnly|lightenOnly|anisotropic","seed":u32=0}"""),
    filterSpec("filter.stylize.extrude", "Extrude…", ["Filter", "Stylize"],
      """{"type":"blocks|pyramids","size":2..255=30,"depth":1..255=30,"depthMode":"random|levelBased","solidFrontFaces":bool,"maskIncompleteBlocks":bool,"seed":u32=0}"""),
    filterSpec("filter.stylize.oilPaint", "Oil Paint…", ["Filter", "Stylize"],
      """{"stylization":0.1..10=4,"cleanliness":0..10=5,"scale":0.1..10=1,"bristleDetail":0..10=5,"lighting":bool=true,"angle":-180..180=-60,"shine":0..10=1}"""),
    filterSpec("filter.stylize.tiles", "Tiles…", ["Filter", "Stylize"],
      """{"count":1..99=10,"maxOffset":1..99=10,"fill":"background|foreground|inverse|unaltered","seed":u32=0,"foreground":json,"background":json}"""),
    filterSpec("filter.stylize.traceContour", "Trace Contour…", ["Filter", "Stylize"], """{"level":0..255=128,"edge":"lower|upper"}"""),
    filterSpec("filter.stylize.wind", "Wind…", ["Filter", "Stylize"], """{"method":"wind|blast|stagger","direction":"fromRight|fromLeft","seed":u32=0}"""),
    # Distort
    filterSpec("filter.distort.displace", "Displace…", ["Filter", "Distort"],
      """{"horizontal":-999..999=10,"vertical":-999..999=10,"fit":"stretch|tile","undefinedAreas":"repeat|wrap","mapDocument":doc,"mapPath":text,"mapLayer":json}"""),
    filterSpec("filter.distort.shear", "Shear…", ["Filter", "Distort"], """{"amount":-100..100=0,"undefinedAreas":"wrap|repeat","points":json}"""),
    filterSpec("filter.distort.zigZag", "ZigZag…", ["Filter", "Distort"],
      """{"amount":-100..100=10,"ridges":0..20=5,"style":"pondRipples|outFromCenter|aroundCenter"}"""),
    # Render
    filterSpec("filter.render.fibers", "Fibers…", ["Filter", "Render"],
      """{"variance":1..64=16,"strength":1..64=4,"seed":u32=0,"foreground":json,"background":json}"""),
    filterSpec("filter.render.lensFlare", "Lens Flare…", ["Filter", "Render"],
      """{"brightness":10..300=100,"centerX":0..1=0.5,"centerY":0..1=0.5,"lens":"zoom|prime35|prime105|moviePrime"}"""),
    filterSpec("filter.render.lightingEffects", "Lighting Effects…", ["Filter", "Render"],
      """{"lightType":"spot|point|infinite","intensity":-100..100=75,"lightX":0..1=0.25,"lightY":0..1=0.2,"lightZ":0..2=0.6,"targetX":0..1=0.5,"targetY":0..1=0.55,"cone":1..89=45,"hotspot":0..100=50,"angle":-180..180=135,"elevation":0..90=45,"gloss":-100..100=0,"metallic":-100..100=0,"exposure":-100..100=0,"ambience":-100..100=8,"texture":"none|red|green|blue|alpha|luminance","height":0..100=50,"whiteIsHigh":bool=true,"lights":json}"""),
    cmd("filter.render.relight", "Relight…", ["Filter", "Render"], "",
        """{"angle":-180..180=45,"elevation":0..90=40,"intensity":0..100=40,"ambient":0..100=55,"warmth":-100..100=0,"softness":1..100=25}""",
        relightEnabled, runProc do:
      validateRelight(p)
      runFilter(s, "filter.render.relight", p)),
    # Noise
    filterSpec("filter.noise.reduceNoise", "Reduce Noise…", ["Filter", "Noise"],
      """{"strength":0..10=6,"preserveDetails":0..100=60,"reduceColorNoise":0..100=45,"sharpenDetails":0..100=25,"removeJpegArtifact":bool}"""),
    # Blur
    filterSpec("filter.blur.smartBlur", "Smart Blur…", ["Filter", "Blur"],
      """{"radius":0.1..100=3,"threshold":0.1..100=25,"quality":"high|medium|low","mode":"normal|edgeOnly|overlayEdge"}"""),
    filterSpec("filter.blur.lensBlur", "Lens Blur…", ["Filter", "Blur"],
      """{"radius":0..100=15,"shape":"hexagon|triangle|square|pentagon|heptagon|octagon","bladeCurvature":0..100=0,"rotation":0..360=0,"depthMap":"none|transparency|layerMask","focalDistance":0..255=0,"invert":bool,"brightness":0..100=0,"threshold":0..255=255,"noise":0..100=0,"distribution":"uniform|gaussian","monochromatic":bool,"seed":u32=0}"""),
    filterSpec("filter.blur.shapeBlur", "Shape Blur…", ["Filter", "Blur"],
      """{"radius":5..1000=10,"shape":"circle|ring|square|diamond|triangle|hexagon|star|heart|cross"}"""),
    # Blur Gallery
    filterSpec("filter.blurGallery.tiltShift", "Tilt-Shift…", ["Filter", "Blur Gallery"],
      """{"blur":0..500=15,"centerX":0..1=0.5,"centerY":0..1=0.5,"angle":-90..90=0,"focus":0..1=0.1,"transition":0.01..1=0.15}"""),
    filterSpec("filter.blurGallery.irisBlur", "Iris Blur…", ["Filter", "Blur Gallery"],
      """{"blur":0..500=15,"centerX":0..1=0.5,"centerY":0..1=0.5,"radiusX":0.01..1=0.35,"radiusY":0.01..1=0.25,"angle":-180..180=0,"roundness":0..100=0,"feather":0..0.99=0.5,"pins":json}"""),
    filterSpec("filter.blurGallery.fieldBlur", "Field Blur…", ["Filter", "Blur Gallery"],
      """{"blur":0..500=15,"centerX":0..1=0.5,"centerY":0..1=0.5,"pins":json}"""),
    filterSpec("filter.blurGallery.spinBlur", "Spin Blur…", ["Filter", "Blur Gallery"],
      """{"blurAngle":0..360=15,"centerX":0..1=0.5,"centerY":0..1=0.5,"radiusX":0.01..1=0.3,"radiusY":0.01..1=0.3,"angle":-180..180=0,"pins":json}"""),
    filterSpec("filter.blurGallery.pathBlur", "Path Blur…", ["Filter", "Blur Gallery"],
      """{"speed":0..500=50,"taper":0..100=0,"startX":0..1=0.2,"startY":0..1=0.5,"endX":0..1=0.8,"endY":0..1=0.5,"paths":json}"""),
    # Other
    filterSpec("filter.other.custom", "Custom…", ["Filter", "Other"], """{"kernel":int[25],"scale":1..9999=1,"offset":-9999..9999=0}"""),
    filterSpec("filter.other.hsbHsl", "HSB/HSL", ["Filter", "Other"], """{"inputMode":"rgb|hsb|hsl","rowOrder":"hsb|hsl|rgb"}"""),
    # Video
    filterSpec("filter.video.deInterlace", "De-Interlace…", ["Filter", "Video"],
      """{"eliminate":"oddFields|evenFields","createBy":"interpolation|duplication"}"""),
    filterSpec("filter.video.ntscColors", "NTSC Colors", ["Filter", "Video"], "{}"),
  ]
