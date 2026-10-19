## Smart objects ↔ PSD placed-layer data: the `SoLd`/`SoLE` descriptor (identity, transform quad,
## warp, size, smart filter stack), legacy `PlLd`, smart filters (known ones become editable
## commands, the rest are kept verbatim as `psd.unsupportedFilter`), and smart filter masks and
## unfiltered pixels (`FEid`). Camera Raw filters of the verified processing version are editable
## (`camera_raw_map`).

import std/[options, json, math, strutils]
import ../geom, ../geom/warp, ../color, ../raster, ../doc, ../psd, ../blake3, ./blocks, ./effects_map, ./pixels, ./camera_raw_map

const UnsupportedFilter* = "psd.unsupportedFilter"

type
  FilterStack* = object
    enabled*: bool
    filters*: seq[SmartFilter]
    maskEnabled*, maskLinked*: bool
  Placed* = object
    descriptor*: Descriptor
    idnt*, placed*: string
    transform*: Affine
    stack*: Option[FilterStack]
  Known = object
    class: string
    filterId: int32
    name, command: string
  PlacedSpec* = object
    idnt*, placed*: string
    transform*: Affine
    perspective*: Option[array[9, float64]]
    size*: (float64, float64)
    dpi*: float64
    warp*: Option[Warp]
    filterFx*: Option[Descriptor]

proc defaultFilterStack*(): FilterStack = FilterStack(enabled: true, maskEnabled: true)

proc code(c: string): int32 =
  cast[int32]((uint32(c[0]) shl 24) or (uint32(c[1]) shl 16) or (uint32(c[2]) shl 8) or uint32(c[3]))

let KnownFilters = [
  Known(class: "GsnB", filterId: code("GsnB"), name: "Gaussian Blur...", command: "filter.blur.gaussianBlur"),
  Known(class: "boxblur", filterId: 840, name: "Box Blur...", command: "filter.blur.boxBlur"),
  Known(class: "MtnB", filterId: code("MtnB"), name: "Motion Blur...", command: "filter.blur.motionBlur"),
  Known(class: "UnsM", filterId: code("UnsM"), name: "Unsharp Mask...", command: "filter.sharpen.unsharpMask"),
  Known(class: "AdNs", filterId: code("AdNs"), name: "Add Noise...", command: "filter.noise.addNoise"),
  Known(class: "Mdn ", filterId: code("Mdn "), name: "Median...", command: "filter.noise.median"),
  Known(class: "HghP", filterId: code("HghP"), name: "High Pass...", command: "filter.other.highPass"),
  Known(class: "Mxm ", filterId: code("Mxm "), name: "Maximum...", command: "filter.other.maximum"),
  Known(class: "Mnm ", filterId: code("Mnm "), name: "Minimum...", command: "filter.other.minimum"),
  Known(class: "Msc ", filterId: code("Msc "), name: "Mosaic...", command: "filter.pixelate.mosaic"),
  Known(class: "Embs", filterId: code("Embs"), name: "Emboss...", command: "filter.stylize.emboss"),
  Known(class: "Lvls", filterId: code("Lvls"), name: "Levels...", command: "image.adjustments.levels"),
  Known(class: "Crvs", filterId: code("Crvs"), name: "Curves...", command: "image.adjustments.curves"),
  Known(class: "adaptCorrect", filterId: 781, name: "Shado&ws/Highlights...", command: "image.adjustments.shadowsHighlights")]

const Channels = [("Cmps", ""), ("Rd  ", "red"), ("Grn ", "green"), ("Bl  ", "blue")]

proc boolOr(d: Descriptor; key: string; default: bool): bool = d.getBool(key).get(default)

proc soldDescriptor*(data: openArray[byte]): Option[Descriptor] =
  if data.len < 8 or char(data[0]) & char(data[1]) & char(data[2]) & char(data[3]) != "soLD": return none(Descriptor)
  parsePrefixVersioned(data.toOpenArray(8, data.len - 1))

proc pxOf(d: Descriptor; key: string): Option[float64] =
  let v = num(d.get(key))
  if v.isSome and v.get.classify notin {fcNan, fcInf, fcNegInf}: v else: none(float64)

proc intOf(d: Descriptor; key: string): Option[int32] =
  let p = d.getPtr(key)
  if p != nil and p.kind == vkInteger: some(p.i) else: none(int32)

proc channelKey(d: Descriptor): Option[string] =
  let p = d.getPtr("Chnl")
  if p == nil or p.kind != vkReference or p.refs.len == 0 or p.refs[0].kind != rkEnumerated: return none(string)
  for (c, k) in Channels:
    if p.refs[0].value.s == c: return some(k)
  none(string)

proc adjustmentList(d: Descriptor): Option[seq[Descriptor]] =
  let l = d.getList("Adjs")
  if l.isNone: return none(seq[Descriptor])
  var outp: seq[Descriptor]
  for v in l.get:
    if v.kind != vkDescriptor: return none(seq[Descriptor])
    outp.add v.desc
  some(outp)

proc levelsParams(f: Descriptor): Option[JsonNode] =
  let adjs = adjustmentList(f)
  if adjs.isNone: return none(JsonNode)
  var outp = newJObject()
  for a in adjs.get:
    let key = channelKey(a)
    if key.isNone: return none(JsonNode)
    var o = newJObject()
    proc pair(a: Descriptor; k: string): Option[(int32, int32)] =
      let l = a.getList(k)
      if l.isNone or l.get.len != 2 or l.get[0].kind != vkInteger or l.get[1].kind != vkInteger: return none((int32, int32))
      some((l.get[0].i, l.get[1].i))
    let inp = pair(a, "Inpt")
    if inp.isSome:
      o["inBlack"] = %inp.get[0]
      o["inWhite"] = %inp.get[1]
    let g = pxOf(a, "Gmm ")
    if g.isSome: o["gamma"] = %g.get
    let otp = pair(a, "Otpt")
    if otp.isSome:
      o["outBlack"] = %otp.get[0]
      o["outWhite"] = %otp.get[1]
    if key.get.len == 0:
      for k, v in o: outp[k] = v
    else: outp[key.get] = o
  some(outp)

proc curvesParams(f: Descriptor): Option[JsonNode] =
  let adjs = adjustmentList(f)
  if adjs.isNone: return none(JsonNode)
  var outp = newJObject()
  for a in adjs.get:
    let key = channelKey(a)
    if key.isNone: return none(JsonNode)
    let name = (if key.get.len == 0: "points" else: key.get)
    let mp = a.getList("Mpng")
    if mp.isSome:
      var arr = newJArray()
      for v in mp.get:
        if v.kind != vkInteger: return none(JsonNode)
        arr.add %clamp(v.i, 0'i32, 255'i32)
      if arr.len != 256: return none(JsonNode)
      outp[name] = arr
      outp["pencil"] = %true
      continue
    let pts = a.getList("Crv ")
    if pts.isNone: return none(JsonNode)
    var arr = newJArray()
    for p in pts.get:
      if p.kind != vkDescriptor: return none(JsonNode)
      let (x, y) = (pxOf(p.desc, "Hrzn"), pxOf(p.desc, "Vrtc"))
      if x.isNone or y.isNone: return none(JsonNode)
      arr.add %*[x.get, y.get]
    outp[name] = arr
  some(outp)

proc knownParams(class: string; f: Option[Descriptor]): Option[(string, JsonNode)] =
  var k: Known
  var found = false
  for x in KnownFilters:
    if x.class == class:
      k = x
      found = true
  if not found: return none((string, JsonNode))
  if class == "adaptCorrect" and f.isNone: return some((k.command, newJObject()))
  if f.isNone: return none((string, JsonNode))
  let d = f.get
  template need(x: untyped): untyped =
    (let v = x; if v.isNone: return none((string, JsonNode)); v.get)
  var params: JsonNode
  case class
  of "GsnB", "boxblur", "Mdn ", "HghP", "Mxm ", "Mnm ": params = %*{"radius": need(pxOf(d, "Rds "))}
  of "MtnB": params = %*{"angle": need(intOf(d, "Angl")), "distance": need(pxOf(d, "Dstn"))}
  of "UnsM": params = %*{"amount": need(pxOf(d, "Amnt")), "radius": need(pxOf(d, "Rds ")), "threshold": need(intOf(d, "Thsh"))}
  of "AdNs":
    let dist = (case enumOf(d, "Dstr").get("")
      of "Unfr": "uniform"
      of "Gsn ": "gaussian"
      else: return none((string, JsonNode)))
    let mono = need(d.getBool("Mnch"))
    params = %*{"amount": need(pxOf(d, "Nose")), "distribution": dist, "monochromatic": mono, "seed": intOf(d, "FlRs").get(0)}
  of "Msc ": params = %*{"cellSize": need(pxOf(d, "ClSz"))}
  of "Embs": params = %*{"angle": need(intOf(d, "Angl")), "height": need(intOf(d, "Hght")), "amount": need(intOf(d, "Amnt"))}
  of "Lvls": params = need(levelsParams(d))
  of "Crvs": params = need(curvesParams(d))
  else: return none((string, JsonNode))
  some((k.command, params))

proc unit(u: string; v: float64): Value = vUnit(u, v)

proc filterFromItem*(item: Descriptor): SmartFilter =
  let opts = getD(item, "blendOptions")
  let op = (if opts.isSome: pxOf(opts.get, "Opct") else: none(float64))
  let opacity = (if op.isSome: float32(clamp(op.get / 100, 0, 1)) else: 1'f32)
  let bm = (if opts.isSome: enumOf(opts.get, "Md  ") else: none(string))
  let blend = (if bm.isSome: blendFromId(bm.get).get(bmNormal) else: bmNormal)
  let visible = boolOr(item, "enab", true)
  let fltr = getD(item, "Fltr")
  var class = (if fltr.isSome: some(fltr.get.classId.s) else: none(string))
  let filterId = intOf(item, "filterID").get(0)
  if class.isNone:
    for k in KnownFilters:
      if k.filterId == filterId: class = some(k.class)
  var known = none((string, JsonNode))
  if class == some(CameraRawClass):
    let p = importParams(item)
    if p.isSome: known = some((CameraRawCommand, p.get))
  elif class.isSome: known = knownParams(class.get, fltr)
  var command: string
  var params: JsonNode
  if known.isSome:
    (command, params) = known.get
  else:
    let name = textOf(item, "Nm  ")
    var neutral = item
    for e in neutral.items.mitems:
      if e[0].s == "blendOptions": e[1] = blendOptions(1, bmNormal)
      elif e[0].s == "enab": e[1] = vBool(true)
    command = UnsupportedFilter
    params = %*{"name": name.replace("&", ""), "filterID": filterId, "psd": bytesToHex(neutral.toBytes)}
  SmartFilter(command: command, params: params, blend: blend, opacity: opacity, visible: visible)

proc filterStack*(fx: Descriptor): FilterStack =
  result = FilterStack(enabled: boolOr(fx, "enab", true), maskEnabled: boolOr(fx, "filterMaskEnable", true),
                       maskLinked: boolOr(fx, "filterMaskLinked", false))
  let l = fx.getList("filterFXList")
  if l.isSome:
    for v in l.get:
      if v.kind == vkDescriptor: result.filters.add filterFromItem(v.desc)

proc parseSold*(data: openArray[byte]): Option[Placed] =
  let d = soldDescriptor(data)
  if d.isNone: return none(Placed)
  let (idnt, transform) = parseSmart("SoLd", data)
  let fx = getD(d.get, "filterFX")
  some(Placed(descriptor: d.get, idnt: idnt, placed: textOf(d.get, "placed"), transform: transform,
              stack: (if fx.isSome: some(filterStack(fx.get)) else: none(FilterStack))))

proc isMeta(k: string): bool = k == "layer" or k.startsWith("__")

proc pf(p: JsonNode; key: string; default: float64): float64 =
  if p.kind == JObject and p.hasKey(key) and p[key].kind in {JInt, JFloat}:
    let v = p[key].getFloat
    if v.classify notin {fcNan, fcInf, fcNegInf}: return v
  default

proc pi(p: JsonNode; key: string; default: float64): Value =
  vInt(int32(clamp(round(pf(p, key, default)), float64(low(int32)), float64(high(int32)))))

proc channelRef(c: string): Value =
  Value(kind: vkReference, refs: @[ReferenceItem(kind: rkEnumerated, class: Class(name: ustrNul(""), classId: id("Chnl")),
                                                 typeId: id("Chnl"), value: Id(code: true, s: c))])

proc adjustments(p: JsonNode; curves: bool): Value =
  if p.kind != JObject: raise newException(ValueError, "parameters are not an object")
  let allowed = (if curves: @["points", "red", "green", "blue", "pencil"]
                 else: @["inBlack", "inWhite", "gamma", "outBlack", "outWhite", "red", "green", "blue"])
  for k, _ in p:
    if not isMeta(k) and k notin allowed: raise newException(ValueError, "setting `" & k & "` has no PSD equivalent")
  var list: seq[Value]
  for (c, key) in Channels:
    var o: JsonNode
    if key.len == 0: o = p
    elif p.hasKey(key): o = p[key]
    else: continue
    var a = newDescriptor(if curves: "CrvA" else: "LvlA").with("Chnl", channelRef(c))
    if curves:
      let src = (if key.len == 0: (if p.hasKey("points"): p["points"] else: nil) else: o)
      if src.isNil or src.kind != JArray: continue
      if p.hasKey("pencil") and p["pencil"].kind == JBool and p["pencil"].getBool:
        var m: seq[Value]
        for v in src:
          if v.kind notin {JInt, JFloat}: raise newException(ValueError, "a pencil-drawn curve is not 256 output levels")
          m.add vInt(int32(clamp(round(v.getFloat), 0, 255)))
        if m.len != 256: raise newException(ValueError, "a pencil-drawn curve is not 256 output levels")
        a = a.with("Mpng", vList(m))
        list.add vDesc(a)
        continue
      var pts: seq[Value]
      for q in src:
        if q.kind != JArray or q.len < 2 or q[0].kind notin {JInt, JFloat} or q[1].kind notin {JInt, JFloat}:
          raise newException(ValueError, "a curve point is not [input, output]")
        pts.add vDesc(newDescriptor("Pnt ").with("Hrzn", vDouble(q[0].getFloat)).with("Vrtc", vDouble(q[1].getFloat)))
      a = a.with("Crv ", vList(pts))
    else:
      if key.len == 0:
        var any = false
        for k in ["inBlack", "inWhite", "gamma", "outBlack", "outWhite"]:
          if o.hasKey(k): any = true
        if not any: continue
      a = a.with("Inpt", vList(@[pi(o, "inBlack", 0), pi(o, "inWhite", 255)])).with("Gmm ", vDouble(pf(o, "gamma", 1)))
      if o.hasKey("outBlack") or o.hasKey("outWhite"):
        a = a.with("Otpt", vList(@[pi(o, "outBlack", 0), pi(o, "outWhite", 255)]))
    list.add vDesc(a)
  vList(list)

proc fltrFor(k: Known; p: JsonNode): Option[Descriptor] =
  let d = newDescriptor(k.class)
  case k.class
  of "GsnB", "boxblur", "Mdn ", "HghP", "Mxm ", "Mnm ":
    if p.kind == JObject and p.hasKey("preserve") and p["preserve"].kind == JString and p["preserve"].getStr == "roundness":
      raise newException(ValueError, "“preserve roundness” has no PSD equivalent")
    some(d.with("Rds ", unit("#Pxl", pf(p, "radius", 1))))
  of "MtnB": some(d.with("Angl", pi(p, "angle", 0)).with("Dstn", unit("#Pxl", pf(p, "distance", 10))))
  of "UnsM":
    some(d.with("Amnt", unit("#Prc", pf(p, "amount", 50))).with("Rds ", unit("#Pxl", pf(p, "radius", 1))).with("Thsh", pi(p, "threshold", 0)))
  of "AdNs":
    let gauss = p.kind == JObject and p.hasKey("distribution") and p["distribution"].kind == JString and p["distribution"].getStr == "gaussian"
    let mono = p.kind == JObject and p.hasKey("monochromatic") and p["monochromatic"].kind == JBool and p["monochromatic"].getBool
    some(d.with("Dstr", vEnum("Dstr", (if gauss: "Gsn " else: "Unfr"))).with("Nose", unit("#Prc", pf(p, "amount", 12.5)))
      .with("Mnch", vBool(mono)).with("FlRs", pi(p, "seed", 0)))
  of "Msc ": some(d.with("ClSz", unit("#Pxl", pf(p, "cellSize", 10))))
  of "Embs": some(d.with("Angl", pi(p, "angle", 135)).with("Hght", pi(p, "height", 3)).with("Amnt", pi(p, "amount", 100)))
  of "Lvls": some(d.with("Adjs", adjustments(p, false)))
  of "Crvs": some(d.with("Adjs", adjustments(p, true)))
  of "adaptCorrect":
    if p.kind == JObject:
      for k, _ in p:
        if not isMeta(k): raise newException(ValueError, "only default Shadows/Highlights settings are written to PSD")
    none(Descriptor)
  else: raise newException(ValueError, "not a recognized filter")

proc itemForFilter*(f: SmartFilter): Descriptor =
  ## The PSD smart filter item; raises ValueError when the filter has no PSD equivalent.
  if f.command == CameraRawCommand:
    var d = exportItem(f.params)
    setKey(d, "blendOptions", blendOptions(f.opacity, f.blend))
    setKey(d, "enab", vBool(f.visible))
    return d
  if f.command == UnsupportedFilter:
    let hex = (if not f.params.isNil and f.params.kind == JObject and f.params.hasKey("psd") and f.params["psd"].kind == JString:
                 hexToBytes(f.params["psd"].getStr) else: none(seq[byte]))
    if hex.isNone: raise newException(ValueError, "its filter data is missing")
    var d: Descriptor
    try: d = descriptorFromBytes(hex.get)
    except CatchableError as e: raise newException(ValueError, "its filter data is unreadable (" & e.msg & ")")
    setKey(d, "blendOptions", blendOptions(f.opacity, f.blend))
    setKey(d, "enab", vBool(f.visible))
    return d
  var k: Known
  var found = false
  for x in KnownFilters:
    if x.command == f.command:
      k = x
      found = true
  if not found: raise newException(ValueError, "this filter has no PSD equivalent")
  let fltr = fltrFor(k, (if f.params.isNil: newJObject() else: f.params))
  result = newDescriptor("filterFX").with("Nm  ", Value(kind: vkText, text: ustrNul(k.name)))
    .with("blendOptions", blendOptions(f.opacity, f.blend)).with("enab", vBool(f.visible))
    .with("hasoptions", vBool(fltr.isSome))
    .with("FrgC", vDesc(newDescriptor("RGBC").with("Rd  ", vDouble(0)).with("Grn ", vDouble(0)).with("Bl  ", vDouble(0))))
    .with("BckC", vDesc(newDescriptor("RGBC").with("Rd  ", vDouble(255)).with("Grn ", vDouble(255)).with("Bl  ", vDouble(255))))
  if fltr.isSome: result = result.with("Fltr", vDesc(fltr.get))
  result = result.with("filterID", vInt(k.filterId))

proc filterFx*(stack: FilterStack; layer: string; warnings: var seq[string]): Descriptor =
  var list: seq[Value]
  for f in stack.filters:
    try: list.add vDesc(itemForFilter(f))
    except ValueError as e:
      warnings.add "layer \"" & layer & "\": smart filter " & f.command & " was not written to PSD: " & e.msg
  newDescriptor("filterFXStyle").with("enab", vBool(stack.enabled)).with("validAtPosition", vBool(true))
    .with("filterMaskEnable", vBool(stack.maskEnabled)).with("filterMaskLinked", vBool(stack.maskLinked))
    .with("filterMaskExtendWithWhite", vBool(true)).with("filterFXList", vList(list))

proc applyHomography(p: array[9, float64]; x, y: float64): (float64, float64) =
  let w = p[6] * x + p[7] * y + p[8]
  let ww = (if abs(w) < 1e-12: 1e-12 else: w)
  ((p[0] * x + p[1] * y + p[2]) / ww, (p[3] * x + p[4] * y + p[5]) / ww)

proc quadPoints(s: PlacedSpec): array[8, float64] =
  let (w, h) = s.size
  let m = s.transform.m
  for i, (x, y) in [(0.0, 0.0), (w, 0.0), (w, h), (0.0, h)]:
    let (px, py) = (if s.perspective.isSome: applyHomography(s.perspective.get, x, y)
                    else: (m[0] * x + m[2] * y + m[4], m[1] * x + m[3] * y + m[5]))
    result[2 * i] = px
    result[2 * i + 1] = py

proc quad(s: PlacedSpec): Value =
  var l: seq[Value]
  for v in quadPoints(s): l.add vDouble(v)
  vList(l)

proc quadMoved(t: Descriptor; s: PlacedSpec): bool =
  let pts = t.getList("Trnf")
  if pts.isNone: return true
  var old: seq[float64]
  for v in pts.get:
    let x = numOf(v)
    if x.isSome: old.add x.get
  let nw = quadPoints(s)
  if old.len != 8: return true
  for i in 0 ..< 8:
    if abs(old[i] - nw[i]) > 1e-6 * (1 + abs(old[i])): return true
  false

proc warpDesc(w0: Option[Warp]; size: (float64, float64); warnings: var seq[string]): Value =
  let none0 = warpNone([0.0, 0.0, size[0], size[1]])
  var w = w0.get(none0)
  var mesh: Option[BezierMesh]
  if w.style == wsCustom:
    if w.mesh.isSome and w.mesh.get.points.len == 16: mesh = w.mesh
    else:
      warnings.add "a custom warp with more than one patch was written to PSD without its warp"
      w = none0
  let b = w.bounds
  let bounds = newDescriptor("classFloatRect").with("Top ", vDouble(b[1])).with("Left", vDouble(b[0]))
    .with("Btom", vDouble(b[3])).with("Rght", vDouble(b[2]))
  var d = newDescriptor("warp").with("warpStyle", vEnum("warpStyle", w.style.psdName)).with("warpValue", vDouble(w.bend))
    .with("warpPerspective", vDouble(w.hDistort)).with("warpPerspectiveOther", vDouble(w.vDistort))
    .with("warpRotate", vEnum("Ornt", (if w.vertical: "Vrtc" else: "Hrzn"))).with("bounds", vDesc(bounds))
    .with("uOrder", vInt(4)).with("vOrder", vInt(4))
  if mesh.isSome:
    var xs, ys: seq[float64]
    for p in mesh.get.points:
      xs.add p[0]
      ys.add p[1]
    let body = newDescriptor("rationalPoint").with("Hrzn", Value(kind: vkUnitFloats, unitN: "#Pxl", values: xs))
      .with("Vrtc", Value(kind: vkUnitFloats, unitN: "#Pxl", values: ys))
    d = d.with("customEnvelopeWarp", vDesc(newDescriptor("customEnvelopeWarp")
      .with("meshPoints", Value(kind: vkObjectArray, prefix: 16, body: body))))
  vDesc(d)

proc ratio(n, d: int32): Value = vDesc(newDescriptor("null").with("numerator", vInt(n)).with("denominator", vInt(d)))

proc pad4(v: var seq[byte]) =
  while v.len mod 4 != 0: v.add 0'u8

proc soldBytes*(tmpl: Option[Descriptor]; s: PlacedSpec; warnings: var seq[string]): seq[byte] =
  let warp = warpDesc(s.warp, s.size, warnings)
  var d: Descriptor
  if tmpl.isSome:
    d = tmpl.get
    if textOf(d, "placed") != s.placed or d.getPtr("placed") == nil: setKey(d, "placed", Value(kind: vkText, text: ustrNul(s.placed)))
    if quadMoved(tmpl.get, s):
      setKey(d, "Trnf", quad(s))
      setKey(d, "nonAffineTransform", quad(s))
    setKey(d, "warp", warp)
    if s.filterFx.isSome: setKey(d, "filterFX", vDesc(s.filterFx.get))
    else:
      var kept: seq[(Id, Value)]
      for e in d.items:
        if e[0].s != "filterFX": kept.add e
      d.items = kept
  else:
    d = newDescriptor("null").with("Idnt", Value(kind: vkText, text: ustrNul(s.idnt)))
      .with("placed", Value(kind: vkText, text: ustrNul(s.placed))).with("PgNm", vInt(1)).with("totalPages", vInt(1))
      .with("Crop", vInt(1)).with("frameStep", ratio(0, 600)).with("duration", ratio(0, 600)).with("frameCount", vInt(1))
      .with("Annt", vInt(16)).with("Type", vInt(2)).with("Trnf", quad(s)).with("nonAffineTransform", quad(s))
      .with("warp", warp).with("Sz  ", vDesc(newDescriptor("Pnt ").with("Wdth", vDouble(s.size[0])).with("Hght", vDouble(s.size[1]))))
      .with("Rslt", unit("#Rsl", s.dpi))
    if s.filterFx.isSome: d = d.with("filterFX", vDesc(s.filterFx.get))
    d = d.with("comp", vInt(-1)).with("compInfo", vDesc(newDescriptor("null").with("compID", vInt(-1)).with("originalCompID", vInt(-1))))
  result = @[byte('s'), byte('o'), byte('L'), byte('D'), 0, 0, 0, 4]
  result.add versioned(d).toBytes
  pad4(result)

proc plldBytes*(s: PlacedSpec; warnings: var seq[string]): seq[byte] =
  result = @[byte('p'), byte('l'), byte('c'), byte('L'), 0, 0, 0, 3]
  let n = min(s.idnt.len, 255)
  result.add byte(n)
  for i in 0 ..< n: result.add byte(s.idnt[i])
  for v in [1'i32, 1, 16, 2]:
    let u = cast[uint32](v)
    for k in countdown(3, 0): result.add byte((u shr (8 * k)) and 0xff)
  for x in quadPoints(s):
    let u = cast[uint64](x)
    for k in countdown(7, 0): result.add byte((u shr (8 * k)) and 0xff)
  result.add [0'u8, 0, 0, 0]
  let w = warpDesc(s.warp, s.size, warnings)
  result.add versioned(w.desc).toBytes
  pad4(result)

proc storedSize*(d: Descriptor): Option[(float64, float64)] =
  let sz = getD(d, "Sz  ")
  if sz.isNone: return none((float64, float64))
  let (w, h) = (num(sz.get.get("Wdth")), num(sz.get.get("Hght")))
  if w.isNone or h.isNone or w.get <= 0 or h.get <= 0: return none((float64, float64))
  some((w.get, h.get))

proc maskFromItem*(item: FilterEffectsItem; sample: SampleType; stack: FilterStack): Option[doc.LayerMask] =
  ## The smart filter mask of an `FEid` item; raises ValueError when it cannot be decoded.
  if item.mask.isNone: return none(doc.LayerMask)
  let dm = decodedMask(item)
  if dm.isNone: raise newException(ValueError, "the smart filter mask could not be decoded")
  let (r, plane) = dm.get
  if int(item.depth) != psdDepth(sample):
    raise newException(ValueError, "the smart filter mask is " & $item.depth & "-bit in a " & $sample & " document")
  let (w, h) = r.size
  let fmt = pixelFormat(cmGrayscale, sample, false)
  var surface = surfaceWithDefault(fmt, [1'f32])
  let bytes = interleave([plane], [true], [zeroSample(sample)], w * h, sample, [false])
  surface.writeInterleaved(rect(int(r.left), int(r.top), int(r.right), int(r.bottom)), bytes)
  surface.prune()
  if surface.contentBounds.isEmpty: return none(doc.LayerMask)
  some(doc.LayerMask(surface: surface, enabled: stack.maskEnabled, linked: stack.maskLinked, density: 1, feather: 0))

proc encodePlane(compression: Compression; plane: openArray[byte]; w, h, depth: int): EffectsPlane =
  try: encodeEffectsPlane(compression, plane, w, h, depth)
  except CatchableError:
    try: encodeEffectsPlane(cRaw, plane, w, h, depth)
    except CatchableError: EffectsPlane(compression: cRaw, data: @plane)

proc filterMaskItem(placed: string; mask: Option[doc.LayerMask]; bounds: Rect; sample: SampleType): FilterEffectsItem =
  let fmt = pixelFormat(cmGrayscale, sample, false)
  var area = (if mask.isSome: mask.get.surface.contentBounds.union(bounds) else: bounds)
  if area.isEmpty: area = rect(0, 0, 1, 1)
  let r = PsdRect(top: int32(area.y0), left: int32(area.x0), bottom: int32(area.y1), right: int32(area.x1))
  let (w, h) = (area.width, area.height)
  var plane: seq[byte]
  if mask.isSome:
    let s = (if mask.get.surface.format == fmt: mask.get.surface else: mask.get.surface.convert(fmt))
    plane = deinterleave(s.toInterleaved(area), 1, sample, [false])[0]
  else:
    for _ in 0 ..< w * h: encodeBe(1, sample, plane)
  let depth = psdDepth(sample)
  let compression = (if sample == stF32: cZipPrediction else: cRle)
  FilterEffectsItem(id: placed, version: 1, rect: r, depth: uint32(depth), maxChannels: 24,
                    slots: newSeq[Option[EffectsPlane]](26), mask: some((r, encodePlane(compression, plane, w, h, depth))))

proc feidItem*(placed: string; unfiltered: Surface; mask: Option[doc.LayerMask]; bounds: Rect; docFmt: PixelFormat): Option[FilterEffectsItem] =
  let sample = docFmt.sample
  let px = (if unfiltered.format == docFmt: unfiltered else: unfiltered.convert(docFmt))
  let content = px.contentBounds
  if content.isEmpty: return none(FilterEffectsItem)
  let r = content.union(bounds)
  let cc = docFmt.mode.colorChannels
  let depth = psdDepth(sample)
  let compression = (if sample == stF32: cZipPrediction else: cRle)
  var invert = newSeq[bool](cc)
  for v in invert.mitems: v = docFmt.mode == cmCmyk
  invert.add false
  var vals = px.readRegion(r)
  let white = fromRgba(docFmt, [1'f32, 1, 1, 1])
  var i = 0
  while i + cc + 1 <= vals.len:
    if vals[i + cc] <= 0:
      for k in 0 ..< cc: vals[i + k] = white[k]
    i += cc + 1
  var filled = newSurface(docFmt)
  filled.writeRegion(r, vals)
  var bytes = filled.toInterleaved(r)
  if docFmt.mode == cmLab and sample == stU16: lab16Chroma(bytes, cc + 1, false)
  let planes = deinterleave(bytes, cc + 1, sample, invert)
  let (w, h) = (r.width, r.height)
  var item = filterMaskItem(placed, mask, bounds, sample)
  item.rect = PsdRect(top: int32(r.y0), left: int32(r.x0), bottom: int32(r.y1), right: int32(r.x1))
  item.slots = newSeq[Option[EffectsPlane]](26)
  for k, p in planes:
    let slot = (if k == cc: 25 else: k)
    if slot < 26: item.slots[slot] = some(encodePlane(compression, p, w, h, depth))
  some(item)

proc uuidFrom*(seed: openArray[byte]): string =
  let x = bytesToHex(blake3(seed)[0 ..< 16])
  x[0 ..< 8] & "-" & x[8 ..< 12] & "-" & x[12 ..< 16] & "-" & x[16 ..< 20] & "-" & x[20 ..< 32]
