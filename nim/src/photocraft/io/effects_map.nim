## Layer styles ↔ PSD `lfx2` (object-based effects descriptor, incl. the `*Multi` lists for
## repeated shadows, fills and strokes) and the legacy `lrfx` block.

import std/[options]
import ../color, ../doc, ../psd, ./blocks

const BlendNames = [(bmNormal, "Nrml"), (bmDissolve, "Dslv"), (bmDarken, "Drkn"), (bmMultiply, "Mltp"),
  (bmColorBurn, "CBrn"), (bmLinearBurn, "linearBurn"), (bmDarkerColor, "darkerColor"), (bmLighten, "Lghn"),
  (bmScreen, "Scrn"), (bmColorDodge, "CDdg"), (bmLinearDodge, "linearDodge"), (bmLighterColor, "lighterColor"),
  (bmOverlay, "Ovrl"), (bmSoftLight, "SftL"), (bmHardLight, "HrdL"), (bmVividLight, "vividLight"),
  (bmLinearLight, "linearLight"), (bmPinLight, "pinLight"), (bmHardMix, "hardMix"), (bmDifference, "Dfrn"),
  (bmExclusion, "Xclu"), (bmSubtract, "blendSubtraction"), (bmDivide, "blendDivide"), (bmHue, "H   "),
  (bmSaturation, "Strt"), (bmColor, "Clr "), (bmLuminosity, "Lmns"), (bmPassThrough, "passThrough")]

const BlendLongNames = [(bmNormal, "normal"), (bmDissolve, "dissolve"), (bmDarken, "darken"), (bmMultiply, "multiply"),
  (bmColorBurn, "colorBurn"), (bmLighten, "lighten"), (bmScreen, "screen"), (bmColorDodge, "colorDodge"),
  (bmOverlay, "overlay"), (bmSoftLight, "softLight"), (bmHardLight, "hardLight"), (bmDifference, "difference"),
  (bmExclusion, "exclusion"), (bmHue, "hue"), (bmSaturation, "saturation"), (bmColor, "color"),
  (bmLuminosity, "luminosity")]

proc blendFromId*(v: string): Option[BlendMode] =
  for (m, n) in BlendNames:
    if n == v: return some(m)
  for (m, n) in BlendLongNames:
    if n == v: return some(m)
  none(BlendMode)

proc blendOf(d: Descriptor; key: string; default: BlendMode): BlendMode =
  let e = enumOf(d, key)
  if e.isNone: return default
  blendFromId(e.get).get(default)

proc blendLongValue*(m: BlendMode): Value =
  for (b, n) in BlendLongNames:
    if b == m: return vEnum("BlnM", n)
  for (b, n) in BlendNames:
    if b == m: return vEnum("BlnM", n)
  vEnum("BlnM", "normal")

proc blendValue(m: BlendMode): Value =
  for (b, n) in BlendNames:
    if b == m: return vEnum("BlnM", n)
  vEnum("BlnM", "Nrml")

proc f(d: Descriptor; key: string; default: float32): float32 =
  let v = num(d.get(key))
  if v.isSome: float32(v.get) else: default

proc pct(d: Descriptor; key: string; default: float32): float32 = f(d, key, default * 100) / 100

proc colorAt(d: Descriptor; key: string): Color =
  let c = getD(d, key)
  if c.isNone: return Black
  colorFromDesc(c.get).get(Black)

proc contour(d: Descriptor; key: string): Contour =
  let c = getD(d, key)
  if c.isNone: return Contour(kind: ckLinear)
  let name = textOf(c.get, "Nm  ")
  var points: seq[CurvePoint]
  var corners: seq[bool]
  let crv = c.get.getList("Crv ")
  if crv.isSome:
    for it in crv.get:
      if it.kind != vkDescriptor: continue
      points.add CurvePoint(input: f(it.desc, "Hrzn", 0) / 255, output: f(it.desc, "Vrtc", 0) / 255)
      corners.add isFalse(it.desc, "Cnty")
  if true notin corners: corners.setLen(0)
  let linear = points.len == 2 and points[0].input == 0 and points[0].output == 0 and points[1].input == 1 and points[1].output == 1
  if points.len == 0 or (linear and (name.len == 0 or name == "Linear")): Contour(kind: ckLinear)
  else: Contour(kind: ckCustom, name: name, points: points, corners: corners)

proc contourValue(c: Contour): Value =
  let (name, points) = (if c.kind == ckLinear: ("Linear", @[CurvePoint(input: 0, output: 0), CurvePoint(input: 1, output: 1)])
                        else: (c.name, c.points))
  var pts: seq[Value]
  for i, p in points:
    var d = newDescriptor("CrPt").with("Hrzn", vDouble(float64(p.input * 255))).with("Vrtc", vDouble(float64(p.output * 255)))
    if c.isCorner(i): d = d.with("Cnty", vBool(false))
    pts.add vDesc(d)
  vDesc(newDescriptor("ShpC").with("Nm  ", Value(kind: vkText, text: ustrNul(name))).with("Crv ", vList(pts)))

proc common(d: Descriptor; blend: BlendMode; opacity: float32): FxCommon =
  FxCommon(enabled: not isFalse(d, "enab"), blend: blendOf(d, "Md  ", blend), opacity: pct(d, "Opct", opacity))

proc gradientFrom(d: Descriptor): Gradient =
  var stops: seq[(float32, Color)]
  var opacityStops: seq[(float32, float32)]
  let g = getD(d, "Grad")
  if g.isSome: (stops, opacityStops) = gradientStopsWith(g.get, gradientMethod(d))
  let off = getD(d, "Ofst")
  result = Gradient(stops: (if stops.len == 0: defaultGradient().stops else: stops), opacityStops: opacityStops,
                    style: gradientStyle(d), angle: f(d, "Angl", 90), scale: pct(d, "Scl ", 1),
                    reverse: boolOf(d, "Rvrs"), align: not isFalse(d, "Algn"))
  if off.isSome: result.offset = (pct(off.get, "Hrzn", 0), pct(off.get, "Vrtc", 0))

proc parseShadow(d: Descriptor; inner: bool): Shadow =
  Shadow(common: common(d, bmMultiply, 0.75), color: colorAt(d, "Clr "), angle: f(d, "lagl", 120),
         useGlobalLight: not isFalse(d, "uglg"), distance: f(d, "Dstn", 5), spread: pct(d, "Ckmt", 0),
         size: f(d, "blur", 5), contour: contour(d, "TrnS"), antiAlias: boolOf(d, "AntA"), noise: pct(d, "Nose", 0),
         knocksOut: not inner and not isFalse(d, "layerConceals"))

proc parseGlow(d: Descriptor; inner: bool): Glow =
  let paint = (if d.get("Grad").isSome: FxPaint(kind: fpGradient, gradient: gradientFrom(d), scale: 1)
               else: FxPaint(kind: fpColor, color: colorAt(d, "Clr "), scale: 1))
  Glow(common: common(d, bmScreen, 0.75), paint: paint,
       technique: (if enumOf(d, "GlwT") == some("PrBL"): gtPrecise else: gtSofter),
       spread: pct(d, "Ckmt", 0), size: f(d, "blur", 5), contour: contour(d, "TrnS"), antiAlias: boolOf(d, "AntA"),
       range: pct(d, "Inpr", 0.5), jitter: pct(d, "ShdN", 0), noise: pct(d, "Nose", 0),
       source: (if inner and enumOf(d, "glwS") == some("SrcC"): gsCenter else: gsEdge))

proc parseStroke(d: Descriptor): StrokeFx =
  var paint: FxPaint
  case enumOf(d, "PntT").get("")
  of "GrFl": paint = FxPaint(kind: fpGradient, gradient: gradientFrom(d), scale: 1)
  of "Ptrn":
    let p = getD(d, "Ptrn")
    let (name, id) = (if p.isSome: (textOf(p.get, "Nm  "), textOf(p.get, "Idnt")) else: ("", ""))
    paint = FxPaint(kind: fpPattern, name: name, id: id, scale: pct(d, "Scl ", 1))
  else: paint = FxPaint(kind: fpColor, color: colorAt(d, "Clr "), scale: 1)
  StrokeFx(common: common(d, bmNormal, 1), size: f(d, "Sz  ", 3),
           position: (case enumOf(d, "Styl").get("")
             of "InsF": spInside
             of "CtrF": spCenter
             else: spOutside), paint: paint)

proc parseBevel(d: Descriptor): Bevel =
  result = Bevel(enabled: not isFalse(d, "enab"),
    style: (case enumOf(d, "bvlS").get("")
      of "OtrB": bsOuterBevel
      of "Embs": bsEmboss
      of "PlEb": bsPillowEmboss
      of "strokeEmboss": bsStrokeEmboss
      else: bsInnerBevel),
    technique: (case enumOf(d, "bvlT").get("")
      of "PrBL": btChiselHard
      of "Slmt": btChiselSoft
      else: btSmooth),
    depth: pct(d, "srgR", 1), up: enumOf(d, "bvlD") != some("Out "), size: f(d, "blur", 5), soften: f(d, "Sftn", 0),
    angle: f(d, "lagl", 120), altitude: f(d, "Lald", 30), useGlobalLight: not isFalse(d, "uglg"),
    glossContour: contour(d, "TrnS"),
    highlight: FxCommon(enabled: true, blend: blendOf(d, "hglM", bmScreen), opacity: pct(d, "hglO", 0.75)),
    highlightColor: (let hc = getD(d, "hglC"); if hc.isSome: colorFromDesc(hc.get).get(White) else: White),
    shadow: FxCommon(enabled: true, blend: blendOf(d, "sdwM", bmMultiply), opacity: pct(d, "sdwO", 0.75)),
    shadowColor: colorAt(d, "sdwC"))
  if boolOf(d, "useShape"):
    result.contour = some(BevelContour(contour: contour(d, "MpgS"), range: pct(d, "Inpr", 0.5), antiAlias: boolOf(d, "AntA")))
  if boolOf(d, "useTexture"):
    let (name, id) = patternRef(d)
    let (_, link, phase) = patternPlacement(d)
    result.texture = some(BevelTexture(name: name, id: id, scale: pct(d, "Scl ", 1), depth: pct(d, "textureDepth", 1),
                                       invert: boolOf(d, "InvT"), link: link, phase: phase))

proc parseOne(key: string; d: Descriptor): Option[Effect] =
  if isFalse(d, "present"): return none(Effect)
  var e: Effect
  case key
  of "DrSh":
    e = newEffect(ekDropShadow)
    e.shadow = parseShadow(d, false)
  of "IrSh":
    e = newEffect(ekInnerShadow)
    e.shadow = parseShadow(d, true)
  of "OrGl":
    e = newEffect(ekOuterGlow)
    e.glow = parseGlow(d, false)
  of "IrGl":
    e = newEffect(ekInnerGlow)
    e.glow = parseGlow(d, true)
  of "FrFX":
    e = newEffect(ekStroke)
    e.stroke = parseStroke(d)
  of "SoFi":
    e = newEffect(ekColorOverlay)
    e.common = common(d, bmNormal, 1)
    e.color = colorAt(d, "Clr ")
  of "GrFl":
    e = newEffect(ekGradientOverlay)
    e.common = common(d, bmNormal, 1)
    e.gradient = gradientFrom(d)
    e.dither = boolOf(d, "Dthr")
  of "patternFill":
    e = newEffect(ekPatternOverlay)
    e.common = common(d, bmNormal, 1)
    (e.name, e.id) = patternRef(d)
    e.scale = pct(d, "Scl ", 1)
    (e.angle, e.link, e.phase) = patternPlacement(d)
  of "ChFX":
    e = newEffect(ekSatin)
    e.satin = Satin(common: common(d, bmMultiply, 0.5), color: colorAt(d, "Clr "), angle: f(d, "lagl", 19),
                    distance: f(d, "Dstn", 11), size: f(d, "blur", 14), contour: contour(d, "MpgS"),
                    antiAlias: boolOf(d, "AntA"), invert: boolOf(d, "Invr"))
  of "ebbl":
    e = newEffect(ekBevelEmboss)
    e.bevel = parseBevel(d)
  else: return none(Effect)
  some(e)

const Kinds = [("DrSh", "dropShadowMulti"), ("IrSh", "innerShadowMulti"), ("OrGl", ""), ("IrGl", ""), ("ebbl", ""),
               ("ChFX", ""), ("SoFi", "solidFillMulti"), ("GrFl", "gradientFillMulti"), ("patternFill", ""),
               ("FrFX", "frameFXMulti")]

proc parseLfx2*(data: openArray[byte]): Option[(bool, seq[Effect])] =
  if data.len <= 4: return none((bool, seq[Effect]))
  let dd = parsePrefixVersioned(data.toOpenArray(4, data.len - 1))
  if dd.isNone: return none((bool, seq[Effect]))
  let d = dd.get
  let master = not isFalse(d, "masterFXSwitch")
  var outp: seq[Effect]
  for (single, multi) in Kinds:
    let lst = (if multi.len > 0: d.getList(multi) else: none(seq[Value]))
    if lst.isSome:
      for it in lst.get:
        if it.kind == vkDescriptor:
          let e = parseOne(single, it.desc)
          if e.isSome: outp.add e.get
    else:
      let x = getD(d, single)
      if x.isSome:
        let e = parseOne(single, x.get)
        if e.isSome: outp.add e.get
  some((master, outp))

proc unit(u: string; v: float32): Value = vUnit(u, float64(v))

proc withCommon(d: Descriptor; c: FxCommon): Descriptor =
  d.with("enab", vBool(c.enabled)).with("present", vBool(true)).with("showInDialog", vBool(true))
    .with("Md  ", blendValue(c.blend)).with("Opct", unit("#Prc", c.opacity * 100))

proc gradientKeys(d0: Descriptor; g: Gradient): Descriptor =
  d0.with("Grad", vDesc(gradientDesc(g.stops, g.opacityStops))).with("Angl", unit("#Ang", g.angle))
    .with("Type", gradientStyleValue(g.style)).with("Rvrs", vBool(g.reverse)).with("Algn", vBool(g.align))
    .with("Scl ", unit("#Prc", g.scale * 100))
    .with("Ofst", vDesc(newDescriptor("Pnt ").with("Hrzn", unit("#Prc", g.offset[0] * 100)).with("Vrtc", unit("#Prc", g.offset[1] * 100))))

proc writeOne(e: Effect): (string, Descriptor) =
  case e.kind
  of ekDropShadow, ekInnerShadow:
    let inner = e.kind == ekInnerShadow
    let s = e.shadow
    let k = (if inner: "IrSh" else: "DrSh")
    var d = withCommon(newDescriptor(k), s.common).with("Clr ", vDesc(colorToDesc(s.color)))
      .with("uglg", vBool(s.useGlobalLight)).with("lagl", unit("#Ang", s.angle)).with("Dstn", unit("#Pxl", s.distance))
      .with("Ckmt", unit("#Pxl", s.spread * 100)).with("blur", unit("#Pxl", s.size)).with("Nose", unit("#Prc", s.noise * 100))
      .with("AntA", vBool(s.antiAlias)).with("TrnS", contourValue(s.contour))
    if not inner: d = d.with("layerConceals", vBool(s.knocksOut))
    (k, d)
  of ekOuterGlow, ekInnerGlow:
    let inner = e.kind == ekInnerGlow
    let g = e.glow
    let k = (if inner: "IrGl" else: "OrGl")
    var d = withCommon(newDescriptor(k), g.common)
    case g.paint.kind
    of fpGradient: d = gradientKeys(d, g.paint.gradient)
    of fpColor: d = d.with("Clr ", vDesc(colorToDesc(g.paint.color)))
    of fpPattern: d = d.with("Clr ", vDesc(colorToDesc(White)))
    d = d.with("GlwT", vEnum("BETE", (if g.technique == gtPrecise: "PrBL" else: "SfBL")))
      .with("Ckmt", unit("#Pxl", g.spread * 100)).with("blur", unit("#Pxl", g.size)).with("Nose", unit("#Prc", g.noise * 100))
      .with("ShdN", unit("#Prc", g.jitter * 100)).with("AntA", vBool(g.antiAlias)).with("TrnS", contourValue(g.contour))
      .with("Inpr", unit("#Prc", g.range * 100))
    if inner: d = d.with("glwS", vEnum("IGSr", (if g.source == gsCenter: "SrcC" else: "SrcE")))
    (k, d)
  of ekStroke:
    let s = e.stroke
    let styl = (case s.position
      of spOutside: "OutF"
      of spInside: "InsF"
      of spCenter: "CtrF")
    var d = withCommon(newDescriptor("FrFX"), s.common).with("Styl", vEnum("FStl", styl)).with("Sz  ", unit("#Pxl", s.size))
    case s.paint.kind
    of fpColor: d = d.with("PntT", vEnum("FrFl", "SClr")).with("Clr ", vDesc(colorToDesc(s.paint.color)))
    of fpGradient: d = gradientKeys(d.with("PntT", vEnum("FrFl", "GrFl")), s.paint.gradient)
    of fpPattern:
      d = d.with("PntT", vEnum("FrFl", "Ptrn")).with("Ptrn", vDesc(newDescriptor("Ptrn")
        .with("Nm  ", Value(kind: vkText, text: ustrNul(s.paint.name))).with("Idnt", Value(kind: vkText, text: ustrNul(s.paint.id)))))
        .with("Scl ", unit("#Prc", s.paint.scale * 100))
    ("FrFX", d)
  of ekColorOverlay: ("SoFi", withCommon(newDescriptor("SoFi"), e.common).with("Clr ", vDesc(colorToDesc(e.color))))
  of ekGradientOverlay:
    ("GrFl", gradientKeys(withCommon(newDescriptor("GrFl"), e.common), e.gradient).with("Dthr", vBool(e.dither)))
  of ekPatternOverlay:
    ("patternFill", withPatternPlacement(withCommon(newDescriptor("patternFill"), e.common)
      .with("Ptrn", vDesc(patternRefDesc(e.name, e.id))).with("Scl ", unit("#Prc", e.scale * 100)), e.angle, e.link, e.phase))
  of ekSatin:
    let s = e.satin
    ("ChFX", withCommon(newDescriptor("ChFX"), s.common).with("Clr ", vDesc(colorToDesc(s.color)))
      .with("AntA", vBool(s.antiAlias)).with("Invr", vBool(s.invert)).with("lagl", unit("#Ang", s.angle))
      .with("Dstn", unit("#Pxl", s.distance)).with("blur", unit("#Pxl", s.size)).with("MpgS", contourValue(s.contour)))
  of ekBevelEmboss:
    let b = e.bevel
    let style = (case b.style
      of bsOuterBevel: "OtrB"
      of bsInnerBevel: "InrB"
      of bsEmboss: "Embs"
      of bsPillowEmboss: "PlEb"
      of bsStrokeEmboss: "strokeEmboss")
    let tech = (case b.technique
      of btSmooth: "SfBL"
      of btChiselHard: "PrBL"
      of btChiselSoft: "Slmt")
    var d = newDescriptor("ebbl").with("enab", vBool(b.enabled)).with("present", vBool(true)).with("showInDialog", vBool(true))
      .with("hglM", blendValue(b.highlight.blend)).with("hglC", vDesc(colorToDesc(b.highlightColor)))
      .with("hglO", unit("#Prc", b.highlight.opacity * 100)).with("sdwM", blendValue(b.shadow.blend))
      .with("sdwC", vDesc(colorToDesc(b.shadowColor))).with("sdwO", unit("#Prc", b.shadow.opacity * 100))
      .with("bvlT", vEnum("bvlT", tech)).with("bvlS", vEnum("BESl", style)).with("uglg", vBool(b.useGlobalLight))
      .with("lagl", unit("#Ang", b.angle)).with("Lald", unit("#Ang", b.altitude)).with("srgR", unit("#Prc", b.depth * 100))
      .with("blur", unit("#Pxl", b.size)).with("bvlD", vEnum("BESs", (if b.up: "In  " else: "Out ")))
      .with("TrnS", contourValue(b.glossContour)).with("Sftn", unit("#Pxl", b.soften))
    if b.contour.isSome:
      let c = b.contour.get
      d = d.with("useShape", vBool(true)).with("MpgS", contourValue(c.contour)).with("AntA", vBool(c.antiAlias))
        .with("Inpr", unit("#Prc", c.range * 100))
    else: d = d.with("useShape", vBool(false))
    if b.texture.isSome:
      let t = b.texture.get
      d = withPatternPlacement(d.with("useTexture", vBool(true)).with("InvT", vBool(t.invert))
        .with("Scl ", unit("#Prc", t.scale * 100)).with("textureDepth", unit("#Prc", t.depth * 100))
        .with("Ptrn", vDesc(patternRefDesc(t.name, t.id))), 0, t.link, t.phase)
    else: d = d.with("useTexture", vBool(false))
    ("ebbl", d)

proc writeLfx2*(master: bool; items: openArray[Effect]): seq[byte] =
  var d = newDescriptor("null").with("Scl ", unit("#Prc", 100)).with("masterFXSwitch", vBool(master))
  var written: seq[(string, Descriptor)]
  for e in items: written.add writeOne(e)
  for (single, multi) in Kinds:
    var ofKind: seq[Descriptor]
    for (k, x) in written:
      if k == single: ofKind.add x
    if ofKind.len == 0: continue
    if ofKind.len == 1 or multi.len == 0: d = d.with(single, vDesc(ofKind[0]))
    else:
      var l: seq[Value]
      for x in ofKind: l.add vDesc(x)
      d = d.with(multi, vList(l))
  result = @[0'u8, 0, 0, 0]
  result.add versioned(d).toBytes

proc fixed(v: uint32): float32 = float32(cast[int32](v)) / 65536

proc parseLrfx*(data: openArray[byte]): Option[(bool, seq[Effect])] =
  proc be16(d: openArray[byte]; at: int): int = (if at + 2 <= d.len: (int(d[at]) shl 8) or int(d[at + 1]) else: -1)
  proc be32(d: openArray[byte]; at: int): int64 =
    if at + 4 > d.len: -1 else: int64((uint32(d[at]) shl 24) or (uint32(d[at + 1]) shl 16) or (uint32(d[at + 2]) shl 8) or uint32(d[at + 3]))
  proc colorAt(d: openArray[byte]; at: int): Option[Color] =
    var c: array[3, float32]
    for k in 0 .. 2:
      let v = be16(d, at + 2 + k * 2)
      if v < 0: return none(Color)
      c[k] = float32(v shr 8) / 255
    some(rgb(c[0], c[1], c[2]))
  proc blendAt(d: openArray[byte]; at: int): BlendMode =
    if at + 8 > d.len: return bmNormal
    blendFromPsdKey(char(d[at + 4]) & char(d[at + 5]) & char(d[at + 6]) & char(d[at + 7])).get(bmNormal)
  let count = be16(data, 2)
  if count < 0: return none((bool, seq[Effect]))
  var at = 4
  var outp: seq[Effect]
  template need(cond: bool) =
    if not cond: return none((bool, seq[Effect]))
  for _ in 0 ..< count:
    need(at + 12 <= data.len)
    need(data[at] == byte('8') and data[at + 1] == byte('B') and data[at + 2] == byte('I') and data[at + 3] == byte('M'))
    let key = char(data[at + 4]) & char(data[at + 5]) & char(data[at + 6]) & char(data[at + 7])
    let size = be32(data, at + 8)
    need(size >= 0)
    let b = at + 12
    case key
    of "dsdw", "isdw":
      let (blur, angle, dist) = (be32(data, b + 4), be32(data, b + 12), be32(data, b + 16))
      need(blur >= 0 and angle >= 0 and dist >= 0 and b + 40 < data.len)
      let color = colorAt(data, b + 20)
      need(color.isSome)
      let s = Shadow(common: FxCommon(enabled: data[b + 38] != 0, blend: blendAt(data, b + 30), opacity: float32(data[b + 40]) / 255),
                     color: color.get, angle: fixed(uint32(angle)), useGlobalLight: data[b + 39] != 0,
                     distance: fixed(uint32(dist)), size: fixed(uint32(blur)), contour: Contour(kind: ckLinear),
                     knocksOut: key == "dsdw")
      var e = newEffect(if key == "dsdw": ekDropShadow else: ekInnerShadow)
      e.shadow = s
      outp.add e
    of "oglw", "iglw":
      let blur = be32(data, b + 4)
      need(blur >= 0 and b + 31 < data.len)
      let color = colorAt(data, b + 12)
      need(color.isSome)
      let g = Glow(common: FxCommon(enabled: data[b + 30] != 0, blend: blendAt(data, b + 22), opacity: float32(data[b + 31]) / 255),
                   paint: FxPaint(kind: fpColor, color: color.get, scale: 1), technique: gtSofter, size: fixed(uint32(blur)),
                   contour: Contour(kind: ckLinear), range: 0.5, source: gsEdge)
      var e = newEffect(if key == "oglw": ekOuterGlow else: ekInnerGlow)
      e.glow = g
      outp.add e
    of "sofi":
      need(b + 23 < data.len)
      let color = colorAt(data, b + 12)
      need(color.isSome)
      var e = newEffect(ekColorOverlay)
      e.common = FxCommon(enabled: data[b + 23] != 0, blend: blendAt(data, b + 4), opacity: float32(data[b + 22]) / 255)
      e.color = color.get
      outp.add e
    else: discard
    at = b + int(size)
  some((true, outp))
