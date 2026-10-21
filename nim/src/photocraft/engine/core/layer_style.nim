## `layer.layerStyle.*` commands: add/replace layer effects on a layer (port of `layer_style.rs`),
## with the effect factory defaults, contour presets and `.shc` contour files. Part of the
## `engine/core` unit: included, not imported.

proc gradientStyle*(name: string): GradientStyle =
  ## Parses a gradient style name (`linear`, `radial`, `angle`, `reflected`, `diamond`).
  case name.toLowerAscii
  of "radial": gsRadial
  of "angle": gsAngle
  of "reflected": gsReflected
  of "diamond": gsDiamond
  else: gsLinear

proc lsF(p: JsonNode; k: string; d: float32): float32 =
  let v = p.pget(k).asF64
  if v.isSome: float32(v.get) else: d
proc lsB(p: JsonNode; k: string; d: bool): bool = p.getBoolOr(k, d)
proc lsFOf(p: JsonNode; k: string): Option[float32] =
  let v = p.pget(k).asF64
  if v.isSome: some(float32(v.get)) else: none(float32)
proc lsBOf(p: JsonNode; k: string): Option[bool] = p.pget(k).asBool

proc lsColorOf(p: JsonNode; k: string): Option[Color] =
  let v = p.pget(k)
  if v.isArray and v.len >= 3:
    return some(rgb(float32(v.elems[0].asF64.get(0.0)), float32(v.elems[1].asF64.get(0.0)), float32(v.elems[2].asF64.get(0.0))))
  if not v.isNil and v.kind == JString:
    var s = v.str
    while s.startsWith("#"): s = s[1 .. ^1]
    proc h(i: int): float32 =
      if i + 2 > s.len: return 0
      let a = s[i]
      let b = s[i + 1]
      if a notin HexDigits or b notin HexDigits: return 0
      float32(parseHexInt(s[i .. i + 1])) / 255'f32
    if s.len >= 6: return some(rgb(h(0), h(2), h(4)))
  none(Color)

proc lsColor(p: JsonNode; k: string; d: array[3, float32]): Color =
  let c0 = lsColorOf(p, k)
  let c = if c0.isSome: c0.get.toRgb else: d
  rgb(c[0], c[1], c[2])

proc lsGradient(p: JsonNode): Gradient =
  result = defaultGradient()
  result.stops = @[(0'f32, lsColor(p, "from", [0'f32, 0, 0])), (1'f32, lsColor(p, "to", [1'f32, 1, 1]))]
  result.style = gradientStyle(p.getStrOr("style", "linear"))
  result.angle = lsF(p, "angle", 90)
  result.scale = lsF(p, "scale", 100) / 100'f32
  result.reverse = lsB(p, "reverse", false)

proc effectDefaults*(kind: string): JsonNode =
  ## Factory defaults for each effect kind, in the command's parameter units.
  case kind
  of "dropShadow": %*{"blend": "Multiply", "color": "#000000", "opacity": 75, "angle": 120, "useGlobalLight": true, "distance": 5, "spread": 0, "size": 5, "contour": "Linear", "antiAlias": false, "noise": 0, "knocksOut": true}
  of "innerShadow": %*{"blend": "Multiply", "color": "#000000", "opacity": 75, "angle": 120, "useGlobalLight": true, "distance": 5, "choke": 0, "size": 5, "contour": "Linear", "antiAlias": false, "noise": 0}
  of "outerGlow": %*{"blend": "Screen", "opacity": 75, "color": "#ffffbe", "spread": 0, "size": 5, "range": 50, "contour": "Linear", "antiAlias": false, "noise": 0}
  of "innerGlow": %*{"blend": "Screen", "opacity": 75, "color": "#ffffbe", "source": "edge", "choke": 0, "size": 5, "contour": "Linear", "antiAlias": false, "noise": 0}
  of "stroke": %*{"size": 3, "position": "outside", "blend": "Normal", "opacity": 100, "color": "#000000"}
  of "colorOverlay": %*{"blend": "Normal", "color": "#ff0000", "opacity": 100}
  of "gradientOverlay": %*{"blend": "Normal", "opacity": 100, "from": "#000000", "to": "#ffffff", "reverse": false, "style": "linear", "angle": 90, "scale": 100}
  of "patternOverlay": %*{"blend": "Normal", "opacity": 100, "pattern": "", "angle": 0, "scale": 100, "link": true}
  of "bevelEmboss": %*{"style": "inner", "depth": 100, "direction": "up", "size": 5, "soften": 0, "angle": 120, "useGlobalLight": true, "altitude": 30, "glossContour": "Linear", "contour": false, "contourName": "Linear", "contourRange": 50, "contourAntiAlias": false}
  of "satin": %*{"blend": "Multiply", "color": "#000000", "opacity": 50, "angle": 19, "distance": 11, "size": 14, "contour": "Linear", "antiAlias": true, "invert": true}
  else: newJObject()

type ContourPoint* = (float32, float32, bool)

let Contours*: seq[(string, seq[ContourPoint])] = @[
  ("Linear", @[(0'f32, 0'f32, false), (1'f32, 1'f32, false)]),
  ("Cone", @[(0'f32, 0'f32, false), (0.5'f32, 1'f32, true), (1'f32, 0'f32, false)]),
  ("Cone - Inverted", @[(0'f32, 1'f32, false), (0.5'f32, 0'f32, true), (1'f32, 1'f32, false)]),
  ("Cove - Deep", @[(0'f32, 0'f32, false), (0.3'f32, 0.02'f32, false), (0.6'f32, 0.12'f32, false), (0.85'f32, 0.4'f32, false), (1'f32, 1'f32, false)]),
  ("Cove - Shallow", @[(0'f32, 0'f32, false), (0.5'f32, 0.22'f32, false), (1'f32, 1'f32, false)]),
  ("Gaussian", @[(0'f32, 0'f32, false), (0.25'f32, 0.08'f32, false), (0.5'f32, 0.5'f32, false), (0.75'f32, 0.92'f32, false), (1'f32, 1'f32, false)]),
  ("Half Round", @[(0'f32, 0'f32, false), (0.04'f32, 0.28'f32, false), (0.15'f32, 0.53'f32, false), (0.35'f32, 0.8'f32, false), (0.65'f32, 0.95'f32, false), (1'f32, 1'f32, false)]),
  ("Ring", @[(0'f32, 0'f32, false), (0.25'f32, 0.7'f32, false), (0.5'f32, 1'f32, false), (0.75'f32, 0.7'f32, false), (1'f32, 0'f32, false)]),
  ("Ring - Double", @[(0'f32, 0'f32, false), (0.25'f32, 1'f32, false), (0.5'f32, 0'f32, true), (0.75'f32, 1'f32, false), (1'f32, 0'f32, false)]),
  ("Rolling Slope - Descending", @[(0'f32, 1'f32, false), (0.25'f32, 0.62'f32, false), (0.5'f32, 0.58'f32, false), (0.75'f32, 0.28'f32, false), (1'f32, 0'f32, false)]),
  ("Rounded Steps", @[(0'f32, 0'f32, false), (0.08'f32, 0.01'f32, false), (0.165'f32, 0.167'f32, false), (0.25'f32, 0.323'f32, false),
    (0.333'f32, 0.333'f32, true), (0.413'f32, 0.343'f32, false), (0.5'f32, 0.5'f32, false), (0.587'f32, 0.657'f32, false),
    (0.667'f32, 0.667'f32, true), (0.747'f32, 0.677'f32, false), (0.833'f32, 0.833'f32, false), (0.92'f32, 0.99'f32, false), (1'f32, 1'f32, false)]),
  ("Sawtooth 1", @[(0'f32, 0'f32, false), (0.3'f32, 1'f32, true), (0.34'f32, 0'f32, true), (0.64'f32, 1'f32, true), (0.68'f32, 0'f32, true), (1'f32, 1'f32, false)]),
]

let LegacyContours: seq[(string, seq[ContourPoint])] = @[
  ("Cone (Inverted)", @[(0'f32, 1'f32, false), (0.5'f32, 0'f32, false), (1'f32, 0'f32, false)]),
  ("Domed", @[(0'f32, 0'f32, false), (0.25'f32, 1'f32, false), (0.75'f32, 1'f32, false), (1'f32, 0'f32, false)]),
  ("Domed (Inverted)", @[(0'f32, 1'f32, false), (0.25'f32, 0'f32, false), (0.75'f32, 0'f32, false), (1'f32, 1'f32, false)]),
  ("Diagonal (Descending)", @[(0'f32, 1'f32, false), (1'f32, 0'f32, false)]),
]

const MaxContourPoints* = 64

proc presetContour(name: string; pts: openArray[ContourPoint]): Contour =
  var corners: seq[bool]
  var any = false
  var points: seq[CurvePoint]
  for (i, o, c) in pts:
    corners.add c
    if c: any = true
    points.add CurvePoint(input: i, output: o)
  Contour(kind: ckCustom, name: name, points: points, corners: (if any: corners else: @[]))

proc builtinContour*(name: string): Option[Contour] =
  ## The built-in contour called `name` (`Linear` is the identity); none for other names.
  if cmpIgnoreCase(name, "Linear") == 0: return some(Contour(kind: ckLinear))
  for i in 1 ..< Contours.len:
    if Contours[i][0] == name: return some(presetContour(Contours[i][0], Contours[i][1]))
  for (n, pts) in LegacyContours:
    if n == name: return some(presetContour(n, pts))
  none(Contour)

proc shortNum(x: float32): JsonNode =
  ## `x` as the shortest decimal that reads back as the same f32.
  try: %parseFloat($x)
  except ValueError: newJNull()

proc contourParam*(c: Contour): JsonNode =
  ## The param naming a contour: "Linear" or a preset's name, else the curve itself.
  if c.kind == ckLinear: return %"Linear"
  let b = builtinContour(c.name)
  if b.isSome and b.get == c: return %c.name
  var pts = newJArray()
  for q in c.points: pts.add %*[shortNum(q.input), shortNum(q.output)]
  var corners = newJArray()
  for i in 0 ..< c.points.len: corners.add %(i < c.corners.len and c.corners[i])
  %*{"name": c.name, "points": pts, "corners": corners}

proc contourFromParam*(v: JsonNode): Option[Contour] =
  ## The contour a `contour` param names: a preset's name, or a curve object.
  if v.isNil: return
  case v.kind
  of JString: builtinContour(v.str)
  of JObject:
    let raw = v.pget("points")
    if not raw.isArray or raw.len < 2 or raw.len > MaxContourPoints: return
    let flags = v.pget("corners")
    var pts: seq[ContourPoint]
    for i, q in raw.elems:
      if not q.isArray or q.len < 2: return
      let x0 = q.elems[0].asF64
      let y0 = q.elems[1].asF64
      if x0.isNone or y0.isNone: return
      let x = float32(x0.get)
      let y = float32(y0.get)
      if x != x or abs(x) == Inf or y != y or abs(y) == Inf: return
      let corner = flags.isArray and i < flags.len and flags.elems[i].asBool.get(false)
      pts.add (clamp(x, 0'f32, 1'f32), clamp(y, 0'f32, 1'f32), corner)
    pts.sort(proc(a, b: ContourPoint): int = cmp(a[0], b[0]), SortOrder.Ascending)
    var dedup: seq[ContourPoint]
    for q in pts:
      if dedup.len > 0 and abs(dedup[^1][0] - q[0]) < 1e-6'f32: continue
      dedup.add q
    if dedup.len < 2: return
    some(presetContour(v.getStrOr("name", "Custom"), dedup))
  else: none(Contour)

proc contoursFromShc*(bytes: openArray[byte]): seq[JsonNode] =
  ## The contours of an `.shc` contour preset file, as `contour` params.
  var file: seq[ShcContour]
  try: file = parseShc(bytes)
  except CatchableError as e: raise other("not a contour file: " & e.msg)
  for c in file:
    var pts = newJArray()
    var corners = newJArray()
    for q in c.points:
      pts.add %*[q.input / 255'f32, q.output / 255'f32]
      corners.add %(not q.continuous)
    let parsed = contourFromParam(%*{"name": c.name, "points": pts, "corners": corners})
    if parsed.isSome: result.add contourParam(parsed.get)

proc contourToShc*(contour: Contour): seq[byte] =
  ## An `.shc` file holding `contour`.
  let (name, points) = if contour.kind == ckLinear: ("Linear", @[CurvePoint(input: 0, output: 0), CurvePoint(input: 1, output: 1)])
                       else: (contour.name, contour.points)
  var pts: seq[ShcPoint]
  for i, q in points: pts.add ShcPoint(input: q.input * 255'f32, output: q.output * 255'f32, continuous: not contour.isCorner(i))
  try: writeShc([ShcContour(name: name, points: pts)])
  except CatchableError as e: raise other("can't write the contour: " & e.msg)

proc freshEffect(kind: string): Option[Effect] =
  ## An effect of `kind` with its factory settings.
  proc shadow(inner: bool): Shadow =
    Shadow(common: fxCommon(bmMultiply, 0.75), color: Black, angle: 120, useGlobalLight: true, distance: 5, spread: 0,
           size: 5, contour: Contour(kind: ckLinear), antiAlias: false, noise: 0, knocksOut: not inner)
  proc glow(): Glow =
    Glow(common: fxCommon(bmScreen, 0.75), paint: FxPaint(kind: fpColor, color: rgb(1, 1, 190'f32 / 255'f32)),
         technique: gtSofter, spread: 0, size: 5, contour: Contour(kind: ckLinear), antiAlias: false, range: 0.5,
         jitter: 0, noise: 0, source: gsEdge)
  case kind
  of "dropShadow": some(Effect(kind: ekDropShadow, shadow: shadow(false)))
  of "innerShadow": some(Effect(kind: ekInnerShadow, shadow: shadow(true)))
  of "outerGlow": some(Effect(kind: ekOuterGlow, glow: glow()))
  of "innerGlow": some(Effect(kind: ekInnerGlow, glow: glow()))
  of "stroke": some(Effect(kind: ekStroke, stroke: StrokeFx(common: fxCommon(bmNormal, 1), size: 3, position: spOutside,
                                                            paint: FxPaint(kind: fpColor, color: rgb(1, 0, 0)))))
  of "colorOverlay": some(Effect(kind: ekColorOverlay, common: fxCommon(bmNormal, 1), color: rgb(1, 0, 0)))
  of "gradientOverlay": some(Effect(kind: ekGradientOverlay, common: fxCommon(bmNormal, 1), gradient: defaultGradient(), dither: false))
  of "patternOverlay": some(Effect(kind: ekPatternOverlay, common: fxCommon(bmNormal, 1), name: "", id: "", scale: 1, angle: 0,
                                   link: true, phase: (0'f32, 0'f32)))
  of "satin": some(Effect(kind: ekSatin, satin: Satin(common: fxCommon(bmMultiply, 0.5), color: Black, angle: 19, distance: 11,
                                                      size: 14, contour: Contour(kind: ckLinear), antiAlias: true, invert: true)))
  of "bevelEmboss": some(Effect(kind: ekBevelEmboss, bevel: Bevel(enabled: true, style: bsInnerBevel, technique: btSmooth, depth: 1,
      up: true, size: 5, soften: 0, angle: 120, altitude: 30, useGlobalLight: true, glossContour: Contour(kind: ckLinear),
      highlight: fxCommon(bmScreen, 0.75), highlightColor: White, shadow: fxCommon(bmMultiply, 0.75), shadowColor: Black)))
  else: none(Effect)

proc overlayGradient(g: var Gradient; p: JsonNode) =
  ## Edits an existing gradient in place: only the end colours and geometry.
  let a = lsColorOf(p, "from")
  if a.isSome and g.stops.len > 0: g.stops[0][1] = a.get
  let b = lsColorOf(p, "to")
  if b.isSome and g.stops.len > 0: g.stops[^1][1] = b.get
  let st = p.pget("style").asStr
  if st.isSome: g.style = gradientStyle(st.get)
  let an = lsFOf(p, "angle")
  if an.isSome: g.angle = an.get
  let sc = lsFOf(p, "scale")
  if sc.isSome: g.scale = clamp(sc.get / 100'f32, 0.01'f32, 10'f32)
  let rv = lsBOf(p, "reverse")
  if rv.isSome: g.reverse = rv.get

proc overlayEffect*(fx: var Effect; p: JsonNode) =
  ## Applies the params present in `p` onto an existing effect; absent keys leave it unchanged.
  proc setColor(c: var Color; k: string) =
    let x = lsColorOf(p, k)
    if x.isSome: c = x.get
  proc setContour(c: var Contour; key: string) =
    let x = contourFromParam(p.pget(key))
    if x.isSome: c = x.get
  proc setBool(b: var bool; key: string) =
    let x = lsBOf(p, key)
    if x.isSome: b = x.get
  proc setF(v: var float32; key: string) =
    let x = lsFOf(p, key)
    if x.isSome: v = x.get
  proc common(c: var FxCommon) =
    let e = lsBOf(p, "enabled")
    if e.isSome: c.enabled = e.get
    let bl = p.pget("blend").asStr
    if bl.isSome:
      let m = blendFromStr(bl.get)
      if m.isSome: c.blend = m.get
    let o = lsFOf(p, "opacity")
    if o.isSome: c.opacity = clamp(o.get / 100'f32, 0'f32, 1'f32)
  case fx.kind
  of ekDropShadow, ekInnerShadow:
    let inner = fx.kind == ekInnerShadow
    common(fx.shadow.common)
    setColor(fx.shadow.color, "color")
    setF(fx.shadow.angle, "angle")
    setBool(fx.shadow.useGlobalLight, "useGlobalLight")
    setF(fx.shadow.distance, "distance")
    let sp = lsFOf(p, if inner: "choke" else: "spread")
    if sp.isSome: fx.shadow.spread = clamp(sp.get / 100'f32, 0'f32, 1'f32)
    setF(fx.shadow.size, "size")
    setContour(fx.shadow.contour, "contour")
    setBool(fx.shadow.antiAlias, "antiAlias")
    let nz = lsFOf(p, "noise")
    if nz.isSome: fx.shadow.noise = clamp(nz.get / 100'f32, 0'f32, 1'f32)
    if not inner: setBool(fx.shadow.knocksOut, "knocksOut")
  of ekOuterGlow, ekInnerGlow:
    let inner = fx.kind == ekInnerGlow
    common(fx.glow.common)
    let c = lsColorOf(p, "color")
    if c.isSome: fx.glow.paint = FxPaint(kind: fpColor, color: c.get)
    let t = p.pget("technique").asStr
    if t.isSome: fx.glow.technique = if t.get == "precise": gtPrecise else: gtSofter
    let sp = lsFOf(p, if inner: "choke" else: "spread")
    if sp.isSome: fx.glow.spread = clamp(sp.get / 100'f32, 0'f32, 1'f32)
    setF(fx.glow.size, "size")
    setContour(fx.glow.contour, "contour")
    setBool(fx.glow.antiAlias, "antiAlias")
    let rg = lsFOf(p, "range")
    if rg.isSome: fx.glow.range = clamp(rg.get / 100'f32, 0.01'f32, 1'f32)
    let nz = lsFOf(p, "noise")
    if nz.isSome: fx.glow.noise = clamp(nz.get / 100'f32, 0'f32, 1'f32)
    if inner:
      let src = p.pget("source").asStr
      if src.isSome: fx.glow.source = if src.get == "center": gsCenter else: gsEdge
  of ekStroke:
    common(fx.stroke.common)
    setF(fx.stroke.size, "size")
    let pos = p.pget("position").asStr
    if pos.isSome:
      fx.stroke.position = case pos.get
        of "inside": spInside
        of "center": spCenter
        else: spOutside
    if not p.pget("from").isNil: fx.stroke.paint = FxPaint(kind: fpGradient, gradient: lsGradient(p))
    else:
      let c = lsColorOf(p, "color")
      if c.isSome: fx.stroke.paint = FxPaint(kind: fpColor, color: c.get)
  of ekColorOverlay:
    common(fx.common)
    setColor(fx.color, "color")
  of ekGradientOverlay:
    common(fx.common)
    overlayGradient(fx.gradient, p)
    setBool(fx.dither, "dither")
  of ekPatternOverlay:
    common(fx.common)
    let pat = p.pget("pattern").asStr
    if pat.isSome:
      fx.name = pat.get
      fx.id = ""
    let sc = lsFOf(p, "scale")
    if sc.isSome: fx.scale = clamp(sc.get / 100'f32, 0.01'f32, 10'f32)
    setF(fx.angle, "angle")
    setBool(fx.link, "link")
    let px = lsFOf(p, "phaseX")
    if px.isSome: fx.phase[0] = px.get
    let py = lsFOf(p, "phaseY")
    if py.isSome: fx.phase[1] = py.get
  of ekSatin:
    common(fx.satin.common)
    setColor(fx.satin.color, "color")
    setF(fx.satin.angle, "angle")
    setF(fx.satin.distance, "distance")
    setF(fx.satin.size, "size")
    setBool(fx.satin.invert, "invert")
    setContour(fx.satin.contour, "contour")
    setBool(fx.satin.antiAlias, "antiAlias")
  of ekBevelEmboss:
    template bv: untyped = fx.bevel
    setBool(bv.enabled, "enabled")
    let st = p.pget("style").asStr
    if st.isSome:
      bv.style = case st.get
        of "outer": bsOuterBevel
        of "emboss": bsEmboss
        of "pillow": bsPillowEmboss
        of "stroke": bsStrokeEmboss
        else: bsInnerBevel
    let tq = p.pget("technique").asStr
    if tq.isSome:
      bv.technique = case tq.get
        of "chiselHard": btChiselHard
        of "chiselSoft": btChiselSoft
        else: btSmooth
    let dp = lsFOf(p, "depth")
    if dp.isSome: bv.depth = dp.get / 100'f32
    let dir = p.pget("direction").asStr
    if dir.isSome: bv.up = dir.get != "down"
    setF(bv.size, "size")
    setF(bv.soften, "soften")
    setF(bv.angle, "angle")
    setF(bv.altitude, "altitude")
    setBool(bv.useGlobalLight, "useGlobalLight")
    setContour(bv.glossContour, "glossContour")
    # The Contour element: `contour` turns it on/off; its curve, Range and Anti-aliased edit it.
    let ce = lsBOf(p, "contour")
    if ce == some(false): bv.contour = none(BevelContour)
    elif ce == some(true) and bv.contour.isNone: bv.contour = some(defaultBevelContour())
    if bv.contour.isSome:
      setContour(bv.contour.get.contour, "contourName")
      let r = lsFOf(p, "contourRange")
      if r.isSome: bv.contour.get.range = clamp(r.get / 100'f32, 0.01'f32, 1'f32)
      setBool(bv.contour.get.antiAlias, "contourAntiAlias")
    let tex = p.pget("texture").asStr
    if tex.isSome and tex.get.len > 0:
      bv.texture = some(BevelTexture(name: tex.get, id: tex.get,
        scale: clamp(lsF(p, "textureScale", 100) / 100'f32, 0.01'f32, 10'f32),
        depth: clamp(lsF(p, "textureDepth", 100) / 100'f32, -10'f32, 10'f32),
        invert: lsB(p, "textureInvert", false), link: lsB(p, "textureLink", true), phase: (0'f32, 0'f32)))

proc effectFromParams*(kind: string; p: JsonNode): Option[Effect] =
  ## Builds an effect of `kind` from JSON params.
  var fx = freshEffect(kind)
  if fx.isNone: return
  overlayEffect(fx.get, p)
  fx

proc effectKindOf*(e: Effect): string =
  case e.kind
  of ekDropShadow: "dropShadow"
  of ekInnerShadow: "innerShadow"
  of ekOuterGlow: "outerGlow"
  of ekInnerGlow: "innerGlow"
  of ekStroke: "stroke"
  of ekColorOverlay: "colorOverlay"
  of ekGradientOverlay: "gradientOverlay"
  of ekPatternOverlay: "patternOverlay"
  of ekSatin: "satin"
  of ekBevelEmboss: "bevelEmboss"

const MaxEffects = 256

proc replaceEffects(s: var Session; p: JsonNode): JsonNode =
  ## Replaces a layer's whole effect list in one history step (the Layer Style dialog's apply).
  const cmdId = "layer.layerStyle.replace"
  let lv = p.pget("layer").asU64
  var id: LayerId
  if lv.isSome: id = LayerId(lv.get)
  else:
    let d = s.active
    if d.isNil or d.activeLayer.isNone: raise other("no active layer")
    id = d.activeLayer.get
  let entries = p.pget("effects")
  if not entries.isArray: raise bad(cmdId, "need `effects`: [{kind, params, fx?}]")
  if entries.len > MaxEffects: raise bad(cmdId, "at most " & $MaxEffects & " effects")
  var list: seq[Effect]
  var pats: seq[DocPattern]
  for e in entries.elems:
    let kind = e.pget("kind").asStr
    if kind.isNone: raise bad(cmdId, "each effect needs a `kind` string")
    let pv = e.pget("params")
    var params: JsonNode
    if pv.isNull: params = newJObject()
    elif pv.kind == JObject: params = pv
    else: raise bad(cmdId, "`params` must be an object")
    var fx: Effect
    let snap = e.pget("fx")
    if not snap.isNil:
      try: fromJ(snap, fx)
      except CatchableError as err: raise bad(cmdId, "bad effect snapshot: " & err.msg)
      if effectKindOf(fx) != kind.get:
        raise bad(cmdId, "effect snapshot is a " & effectKindOf(fx) & ", not `" & kind.get & "`")
      overlayEffect(fx, params)
    else:
      let f = effectFromParams(kind.get, params)
      if f.isNone: raise bad(cmdId, "unknown effect " & kind.get)
      fx = f.get
    let (rfx, pat) = resolveEffect(s, fx)
    if pat.isSome: pats.add pat.get
    list.add rfx
  let count = list.len
  let label = if list.len == 0: "Clear Layer Style" else: "Edit Layer Style"
  s.editDo(label):
    for pat in pats: ensureInDoc(doc, pat)
    let mode = doc.mode
    let l = doc.layerOrRaise(id)
    var items: seq[Effect]
    for e in list: items.add e.inMode(mode)
    l.effects.items = items
    l.effects.enabled = true
  %*{"layer": id, "effects": count}

proc setEffect(s: var Session; p: JsonNode; kind: string): JsonNode =
  let fx0 = effectFromParams(kind, p)
  if fx0.isNone: raise other("unknown effect " & kind)
  let (fx, pattern) = resolveEffect(s, fx0.get)
  let label = "Layer Style: " & fx.label
  let add = lsB(p, "add", false)
  let lv = p.pget("layer").asU64
  var id: LayerId
  if lv.isSome: id = LayerId(lv.get)
  else:
    let d = s.active
    if d.isNil or d.activeLayer.isNone: raise other("no active layer")
    id = d.activeLayer.get
  s.editDo(label):
    if pattern.isSome: ensureInDoc(doc, pattern.get)
    # Colours in the document's mode, as Image › Mode keeps them.
    let f = fx.inMode(doc.mode)
    let l = doc.layerOrRaise(id)
    l.effects.enabled = true
    var slot = -1
    for i, e in l.effects.items:
      if e.kind == f.kind:
        slot = i
        break
    if slot >= 0 and not add: l.effects.items[slot] = f
    else: l.effects.items.add f
  %*{"layer": id}

proc presetsChanged*(s: var Session)

proc reqStrKind(p: JsonNode; cmdId: string): string =
  let kind = p.pget("kind").asStr
  if kind.isNone: raise bad(cmdId, "need `kind`")
  if effectFromParams(kind.get, newJNull()).isNone: raise bad(cmdId, "unknown effect " & kind.get)
  kind.get

proc makeDefault(s: var Session; p: JsonNode): JsonNode =
  ## Stores the user default for one effect kind ("Make Default" in the Layer Style dialog).
  const cmdId = "layer.layerStyle.makeDefault"
  let kind = reqStrKind(p, cmdId)
  let params = p.pget("params")
  if not params.isObject: raise bad(cmdId, "`params` must be an object")
  s.presets.layerDefaults[kind] = params
  presetsChanged(s)
  %*{"kind": kind}

proc defaultForCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "layer.layerStyle.defaultFor"
  let kind = reqStrKind(p, cmdId)
  let v = s.presets.layerDefaults.getOrDefault(kind, nil)
  if v.isObject: %*{"params": v, "user": true} else: %*{"params": effectDefaults(kind), "user": false}

proc deleteEffect(s: var Session; p: JsonNode; effect: JsonNode): JsonNode =
  ## `layer.layerStyle.clear` with an `effect` index: deletes that one effect.
  const cmdId = "layer.layerStyle.clear"
  let iv = effect.asU64
  if iv.isNone: raise bad(cmdId, "`effect` must be an effect index")
  let index = int(min(iv.get, uint64(high(int))))
  let d = s.activeOrRaise()
  var id: LayerId
  let lv = p.pget("layer")
  if not lv.isNull:
    let u = lv.asU64
    if u.isNone: raise bad(cmdId, "`layer` must be a layer id")
    id = LayerId(u.get)
  else:
    if d.activeLayer.isNone: raise bad(cmdId, "no active layer")
    id = d.activeLayer.get
  let l = d.doc[].findLayer(id)
  if l.isNil: raise noLayer(id)
  let count = l.effects.items.len
  if index >= count: raise bad(cmdId, "the layer has " & $count & " effect(s), no effect " & $index)
  s.editDo("Delete Effect"):
    let l = doc.layerOrRaise(id)
    if index < l.effects.items.len: l.effects.items.delete(index)
  jnull()

proc styleSpec(kind, label, params: string): CommandSpec =
  cmd("layer.layerStyle." & kind, label, ["Layer", "Layer Style"], "", params, hasLayerE, runProc(setEffect(s, p, kind)))

proc layerStyleSpecs*(): seq[CommandSpec] =
  @[
    styleSpec("dropShadow", "Drop Shadow…", """{"color":"#rrggbb","opacity":0..100=75,"blend":str="multiply","angle":deg=120,"useGlobalLight":bool,"distance":px=5,"spread":0..100,"size":px=5,"contour":"Linear|Cone|Cone - Inverted|Cove - Deep|Cove - Shallow|Gaussian|Half Round|Ring|Ring - Double|Rolling Slope - Descending|Rounded Steps|Sawtooth 1"|{"name":str,"points":[[in 0..1,out 0..1],…],"corners":[bool,…]}="Linear","antiAlias":bool,"noise":0..100,"knocksOut":bool,"add":bool,"layer":id}"""),
    styleSpec("innerShadow", "Inner Shadow…", """{"color":"#rrggbb","opacity":0..100=75,"blend":str,"angle":deg,"distance":px,"choke":0..100,"size":px,"contour":name|curve,"antiAlias":bool,"noise":0..100,"add":bool}"""),
    styleSpec("outerGlow", "Outer Glow…", """{"color":"#rrggbb","opacity":0..100=75,"blend":str="screen","technique":"softer|precise","spread":0..100,"size":px,"range":0..100,"contour":name|curve,"antiAlias":bool,"noise":0..100,"add":bool}"""),
    styleSpec("innerGlow", "Inner Glow…", """{"color":"#rrggbb","opacity":0..100=75,"blend":str="screen","technique":"softer|precise","source":"edge|center","choke":0..100,"size":px,"contour":name|curve,"antiAlias":bool,"noise":0..100,"add":bool}"""),
    styleSpec("stroke", "Stroke…", """{"size":px=3,"position":"outside|inside|center","color":"#rrggbb","from":"#rrggbb","to":"#rrggbb","style":str,"angle":deg,"opacity":0..100,"blend":str,"add":bool}"""),
    styleSpec("colorOverlay", "Color Overlay…", """{"color":"#rrggbb","opacity":0..100=100,"blend":str,"add":bool}"""),
    styleSpec("gradientOverlay", "Gradient Overlay…", """{"from":"#rrggbb","to":"#rrggbb","style":"linear|radial|angle|reflected|diamond","angle":deg=90,"scale":10..150=100,"reverse":bool,"opacity":0..100,"blend":str,"add":bool}"""),
    styleSpec("patternOverlay", "Pattern Overlay…", """{"pattern":id|name?=first library pattern,"opacity":0..100=100,"blend":str,"scale":1..1000=100,"angle":deg=0,"link":bool=true,"phaseX":px,"phaseY":px,"add":bool}"""),
    styleSpec("bevelEmboss", "Bevel & Emboss…", """{"style":"inner|outer|emboss|pillow|stroke","technique":"smooth|chiselHard|chiselSoft","contour":bool,"contourName":name|curve,"contourRange":1..100=50,"contourAntiAlias":bool,"glossContour":name|curve,"texture":pattern id|name?,"textureScale":1..1000=100,"textureDepth":-1000..1000=100,"textureInvert":bool,"textureLink":bool=true,"depth":1..1000=100,"direction":"up|down","size":px=5,"soften":px,"angle":deg,"altitude":deg,"add":bool}"""),
    styleSpec("satin", "Satin…", """{"color":"#rrggbb","opacity":0..100=50,"blend":str,"angle":deg,"distance":px,"size":px,"contour":name|curve,"antiAlias":bool=true,"invert":bool,"add":bool}"""),
    cmd("layer.layerStyle.replace", "Edit Layer Style", [], "",
        """{"layer":id,"effects":[{"kind":"dropShadow|innerShadow|outerGlow|innerGlow|stroke|colorOverlay|gradientOverlay|patternOverlay|satin|bevelEmboss","params":{…layer.layerStyle.<kind> params…},"fx":<effect snapshot carried through the Layer Style dialog, optional>}]} (replaces the layer's whole effect list; an entry with `fx` edits that effect with the params present, one without builds from params)""",
        hasLayerE, replaceEffects),
    cmd("layer.layerStyle.makeDefault", "Make Layer Style Default", [], "",
        """{"kind":"stroke|dropShadow|innerShadow|outerGlow|innerGlow|colorOverlay|gradientOverlay|patternOverlay|satin|bevelEmboss","params":{…param set…}} (stores the user default the Layer Style dialog's Reset to Default restores)""",
        always, makeDefault, journal = false),
    cmd("layer.layerStyle.defaultFor", "Layer Style Default", [], "",
        """{"kind":str} → {"params":{…},"user":bool} (the saved user default, else the factory defaults)""", always, defaultForCmd,
        journal = false),
    cmd("layer.layerStyle.clear", "Clear Layer Style", ["Layer", "Layer Style"], "",
        """{"layer":id? (no layer: every selected layer that has a style),"effect":index? (only that effect of "layer", as an effect row dropped on the Layers panel's Delete button)}""",
        hasLayerE, runProc do:
      let effect = p.pget("effect")
      if not effect.isNull: return deleteEffect(s, p, effect)
      let ids = styleTargets(s, p, proc(l: Layer): bool = l.effects.items.len > 0)
      s.editDo("Clear Layer Style"):
        for id in ids:
          let l = doc.layerOrRaise(id)
          var fx = newEffects()
          fx.enabled = true
          l.effects = fx
      jnull()),
  ]
