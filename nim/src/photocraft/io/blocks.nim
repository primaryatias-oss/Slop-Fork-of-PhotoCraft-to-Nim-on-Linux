## Shared PSD tagged-block helpers: blending-option flags, Blend If ranges, locks, label colours,
## colour/gradient/pattern descriptors, fill layers (`SoCo`/`GdFl`/`PtFl`), placed-layer warps and
## transforms, and the gradient baking used for Photoshop's interpolation methods.

import std/[options, math, algorithm]
import ../geom, ../geom/warp, ../color, ../doc, ../psd, ../algo/util

# ---------------------------------------------------------------- gradient baking

type BakeMethod* = enum bmClassic, bmPerceptual, bmLinear, bmSmooth

proc methodFromCode*(code: Option[string]): BakeMethod =
  if code.isNone: return bmClassic
  case code.get
  of "Perc": bmPerceptual
  of "Lnr ": bmLinear
  of "Smoo": bmSmooth
  else: bmClassic

proc lin(c: float32): float32 = (if c <= 0.04045: c / 12.92 else: pow((c + 0.055'f32) / 1.055'f32, 2.4'f32))
proc enc(c0: float32): float32 =
  let c = clamp(c0, 0'f32, 1'f32)
  if c <= 0.003_130_8: 12.92'f32 * c else: 1.055'f32 * pow(c, 1 / 2.4'f32) - 0.055'f32

proc toOklab(rgb: array[3, float32]): array[3, float32] =
  let (r, g, b) = (lin(rgb[0]), lin(rgb[1]), lin(rgb[2]))
  let l = cbrt(0.412_221_46'f32 * r + 0.536_332_55'f32 * g + 0.051_445_995'f32 * b)
  let m = cbrt(0.211_903_5'f32 * r + 0.680_699_5'f32 * g + 0.107_396_96'f32 * b)
  let s = cbrt(0.088_302_46'f32 * r + 0.281_718_85'f32 * g + 0.629_978_7'f32 * b)
  [0.210_454_26'f32 * l + 0.793_617_8'f32 * m - 0.004_072_047'f32 * s,
   1.977_998_5'f32 * l - 2.428_592_2'f32 * m + 0.450_593_7'f32 * s,
   0.025_904_037'f32 * l + 0.782_771_77'f32 * m - 0.808_675_77'f32 * s]

proc fromOklab(p: array[3, float32]): array[3, float32] =
  let l = pow(p[0] + 0.396_337_78'f32 * p[1] + 0.215_803_76'f32 * p[2], 3)
  let m = pow(p[0] - 0.105_561_346'f32 * p[1] - 0.063_854_17'f32 * p[2], 3)
  let s = pow(p[0] - 0.089_484_18'f32 * p[1] - 1.291_485_5'f32 * p[2], 3)
  [enc(4.076_741_7'f32 * l - 3.307_711_6'f32 * m + 0.230_969_94'f32 * s),
   enc(-1.268_438'f32 * l + 2.609_757_4'f32 * m - 0.341_319_38'f32 * s),
   enc(-0.004_196_086_3'f32 * l - 0.703_418_6'f32 * m + 1.707_614_7'f32 * s)]

proc intoSpace(m: BakeMethod; rgb: array[3, float32]): array[3, float32] =
  case m
  of bmClassic: rgb
  of bmLinear: [lin(rgb[0]), lin(rgb[1]), lin(rgb[2])]
  of bmPerceptual, bmSmooth: toOklab(rgb)

proc outOfSpace(m: BakeMethod; p: array[3, float32]): array[3, float32] =
  case m
  of bmClassic: [clamp(p[0], 0'f32, 1'f32), clamp(p[1], 0'f32, 1'f32), clamp(p[2], 0'f32, 1'f32)]
  of bmLinear: [enc(p[0]), enc(p[1]), enc(p[2])]
  of bmPerceptual, bmSmooth: fromOklab(p)

const SmoothEase = [0'f32, 0.0438, 0.099, 0.1644, 0.2303, 0.2988, 0.3667, 0.433, 0.4974, 0.5642, 0.6302,
                    0.6985, 0.7656, 0.8333, 0.8945, 0.9517, 1.0]

proc smoothEase(u: float32): float32 =
  let x = clamp(u, 0'f32, 1'f32) * 16
  let i = min(int(floor(x)), 15)
  SmoothEase[i] + (SmoothEase[i + 1] - SmoothEase[i]) * (x - float32(i))

proc evalStops(stops: seq[(float32, array[3, float32])]; mids: openArray[float32]; smooth, t: float32;
               m: BakeMethod): array[3, float32] =
  let n = stops.len
  if t <= stops[0][0]: return stops[0][1]
  if t >= stops[n - 1][0]: return stops[n - 1][1]
  var i = 0
  for k in 0 ..< n - 1:
    if t >= stops[k][0] and t <= stops[k + 1][0]:
      i = k
      break
  let (t0, t1) = (stops[i][0], stops[i + 1][0])
  let h = max(t1 - t0, 1e-6'f32)
  var u = (t - t0) / h
  let mid = clamp((if i < mids.len: mids[i] else: 0.5'f32), 0.05'f32, 0.95'f32)
  u = (if u <= mid: 0.5'f32 * u / mid else: 0.5'f32 + 0.5'f32 * (u - mid) / (1 - mid))
  let (p0, p1) = (stops[i][1], stops[i + 1][1])
  if m == bmSmooth:
    let e = smoothEase(u)
    for c in 0 .. 2: result[c] = p0[c] + (p1[c] - p0[c]) * e
    return
  proc tangent(stops: seq[(float32, array[3, float32])]; k, n: int): array[3, float32] =
    let (a, b) = (if k == 0: (0, 1) elif k == n - 1: (n - 2, n - 1) else: (k - 1, k + 1))
    let d = max(stops[b][0] - stops[a][0], 1e-6'f32)
    let half = (if k == 0 or k == n - 1: 0.5'f32 else: 1'f32)
    for c in 0 .. 2: result[c] = (stops[b][1][c] - stops[a][1][c]) / d * half
  let (m0, m1) = (tangent(stops, i, n), tangent(stops, i + 1, n))
  let (u2, u3) = (u * u, u * u * u)
  let (h00, h10, h01, h11) = (2 * u3 - 3 * u2 + 1, u3 - 2 * u2 + u, -2 * u3 + 3 * u2, u3 - u2)
  for c in 0 .. 2:
    let linear = p0[c] + (p1[c] - p0[c]) * u
    let spline = h00 * p0[c] + h10 * h * m0[c] + h01 * p1[c] + h11 * h * m1[c]
    let value = linear + (spline - linear) * smooth
    let (lo, hi) = (min(p0[c], p1[c]), max(p0[c], p1[c]))
    result[c] = (if lo.classify notin {fcInf, fcNegInf, fcNan} and hi.classify notin {fcInf, fcNegInf, fcNan}:
                   clamp(value, lo, hi) else: value)

proc bake*(stops0: seq[(float32, Color)]; midpoints: openArray[float32]; smoothness: float32; m: BakeMethod): seq[(float32, Color)] =
  ## Samples a gradient with Photoshop's smoothness, midpoints and interpolation method into 97
  ## plain stops (Classic gradients without any of those come back unchanged).
  var plainMids = true
  for x in midpoints:
    if abs(x - 0.5) >= 1e-3: plainMids = false
  var lab = m == bmClassic
  for (_, c) in stops0:
    if c.mode != cmLab: lab = false
  if stops0.len < 2 or (smoothness <= 0 and m == bmClassic and plainMids and not lab): return stops0
  var s = stops0
  s.sort(proc(a, b: (float32, Color)): int = totalCmp(a[0], b[0]))
  proc alphaAt(s: seq[(float32, Color)]; t: float32): float32 =
    var best = -1
    for i, x in s:
      if best < 0 or abs(x[0] - t) < abs(s[best][0] - t): best = i
    if best < 0: 1'f32 else: s[best][1].alpha
  var pts: seq[(float32, array[3, float32])]
  for (t, c) in s:
    pts.add (t, (if lab: [c.c[0], c.c[1], c.c[2]] else: intoSpace(m, c.toRgb)))
  const N = 96
  for k in 0 .. N:
    let t = float32(k) / N
    let v = evalStops(pts, midpoints, clamp(smoothness, 0'f32, 1'f32), t, m)
    let c = (if lab: Color(mode: cmLab, c: [clamp(v[0], 0'f32, 1'f32), clamp(v[1], 0'f32, 1'f32),
                                           clamp(v[2], 0'f32, 1'f32), 0], alpha: alphaAt(s, t))
             else:
               let rgb = outOfSpace(m, v)
               rgba(rgb[0], rgb[1], rgb[2], alphaAt(s, t)))
    if result.len > 0 and abs(result[^1][0] - t) < 1e-6: continue
    result.add (t, c)

# ---------------------------------------------------------------- flags, locks, labels

proc parseBrst*(data: openArray[byte]): uint32 =
  var i = 0
  while i + 4 <= data.len:
    let v = (uint32(data[i]) shl 24) or (uint32(data[i + 1]) shl 16) or (uint32(data[i + 2]) shl 8) or uint32(data[i + 3])
    if v < 32: result = result or (1'u32 shl v)
    i += 4

proc brstData*(mask: uint32): Option[seq[byte]] =
  if mask == 0: return none(seq[byte])
  var d: seq[byte]
  for i in 0'u32 ..< 32:
    if (mask and (1'u32 shl i)) != 0: d.add [0'u8, 0, 0, byte(i)]
  some(d)

proc advancedFlags*(a: AdvancedBlending): array[6, (string, uint8, uint8)] =
  [("knko", a.knockout.toPsd, 0'u8), ("infx", uint8(a.blendInterior), 0'u8), ("clbl", uint8(a.blendClipped), 1'u8),
   ("tsly", uint8(a.transparencyShapes), 1'u8), ("lmgm", uint8(a.layerMaskHidesEffects), 0'u8),
   ("vmgm", uint8(a.vectorMaskHidesEffects), 0'u8)]

proc advancedFromBlocks*(lookup: proc(key: string): Option[seq[byte]]): AdvancedBlending =
  result = defaultAdvancedBlending()
  proc flag(lookup: proc(key: string): Option[seq[byte]]; k: string): Option[uint8] =
    let d = lookup(k)
    if d.isSome and d.get.len > 0: some(d.get[0]) else: none(uint8)
  let k = flag(lookup, "knko")
  if k.isSome: result.knockout = knockoutFromPsd(k.get)
  template setFlag(key: string; f: untyped) =
    let v = flag(lookup, key)
    if v.isSome: f = v.get != 0
  setFlag("infx", result.blendInterior)
  setFlag("clbl", result.blendClipped)
  setFlag("tsly", result.transparencyShapes)
  setFlag("lmgm", result.layerMaskHidesEffects)
  setFlag("vmgm", result.vectorMaskHidesEffects)

proc putAdvanced*(a: AdvancedBlending; raw: var seq[(string, seq[byte])]) =
  for (key, v, default) in advancedFlags(a):
    var found = false
    for e in raw.mitems:
      if e[0] == key:
        if e[1].len < 4: e[1].setLen(4)
        e[1][0] = v
        found = true
        break
    if not found and v != default: raw.add (key, @[v, 0'u8, 0, 0])

proc blendIfFromRanges*(r: BlendingRanges): BlendIf =
  var ranges: seq[array[2, doc.BlendRange]]
  for e in r.ranges:
    ranges.add [blendRangeFromBytes(e.source), blendRangeFromBytes(e.dest)]
  var keep = 0
  for i in countdown(ranges.len - 1, 0):
    if not (ranges[i][0].isFull and ranges[i][1].isFull):
      keep = i + 1
      break
  ranges.setLen(keep)
  BlendIf(ranges: ranges)

proc rangesFromBlendIf*(b: BlendIf; channels: int): BlendingRanges =
  if b.isDefault: return fullBlendingRanges(channels)
  let n = max(b.ranges.len, channels + 1)
  var data: seq[byte]
  for i in 0 ..< n:
    let pair = b.get(i)
    for v in pair[0].toBytes: data.add v
    for v in pair[1].toBytes: data.add v
  BlendingRanges(data: data)

proc locksFromLspf*(v: uint32): Locks =
  Locks(transparency: (v and 1) != 0, pixels: (v and 2) != 0, position: (v and 4) != 0,
        artboard: (v and 0x08) != 0 or (v and 0x10) != 0, all: (v and 0x8000_0000'u32) != 0)

proc lspfFromLocks*(l: Locks): uint32 =
  uint32(l.transparency) or (uint32(l.pixels) shl 1) or (uint32(l.position) shl 2) or (uint32(l.artboard) shl 3) or
    (uint32(l.all) shl 31)

proc labelFromIndex*(v: uint16): LabelColor =
  if v >= 1 and v <= 11: LabelColor(int(v)) else: lcNone

proc labelIndex*(l: LabelColor): uint16 = uint16(ord(l))

# ---------------------------------------------------------------- descriptor values

proc num*(v: Option[Value]): Option[float64] =
  if v.isNone: return none(float64)
  let x = v.get
  case x.kind
  of vkDouble: some(x.d)
  of vkUnitFloat: some(x.uv)
  of vkInteger: some(float64(x.i))
  else: none(float64)

proc numOf*(v: Value): Option[float64] = num(some(v))

proc getD*(d: Descriptor; key: string): Option[Descriptor] =
  let p = d.getPtr(key)
  if p != nil and p.kind in {vkDescriptor, vkGlobalObject}: some(p.desc) else: none(Descriptor)

proc enumOf*(d: Descriptor; key: string): Option[string] =
  let p = d.getPtr(key)
  if p != nil and p.kind == vkEnumerated: some(p.enumValue.s) else: none(string)

proc boolOf*(d: Descriptor; key: string): bool =
  let p = d.getPtr(key)
  p != nil and p.kind == vkBoolean and p.b

proc isFalse*(d: Descriptor; key: string): bool =
  let p = d.getPtr(key)
  p != nil and p.kind == vkBoolean and not p.b

proc textOf*(d: Descriptor; key: string): string =
  let p = d.getPtr(key)
  if p != nil and p.kind == vkText: $p.text else: ""

proc colorFromDesc*(d: Descriptor): Option[Color] =
  template g(k: string): Option[float64] = num(d.get(k))
  template need(k: string): float32 =
    (let v = g(k); if v.isNone: return none(Color); float32(v.get))
  let cls = d.classId.s
  if cls == "RGBC" and d.get("redFloat").isSome or (cls notin ["RGBC", "CMYC", "Grsc", "LbCl"] and d.get("redFloat").isSome):
    return some(rgb(need("redFloat"), need("greenFloat"), need("blueFloat")))
  case cls
  of "CMYC":
    some(Color(mode: cmCmyk, c: [need("Cyn ") / 100, need("Mgnt") / 100, need("Ylw ") / 100, need("Blck") / 100], alpha: 1))
  of "Grsc": some(gray(1 - need("Gry ") / 100))
  of "LbCl":
    some(Color(mode: cmLab, c: [need("Lmnc") / 100, (need("A   ") + 128) / 255, (need("B   ") + 128) / 255, 0], alpha: 1))
  else: some(rgb(need("Rd  ") / 255, need("Grn ") / 255, need("Bl  ") / 255))

proc colorToDesc*(c: Color): Descriptor =
  template dv(v: float32): Value = vDouble(float64(v))
  case c.mode
  of cmCmyk:
    newDescriptor("CMYC").with("Cyn ", dv(c.c[0] * 100)).with("Mgnt", dv(c.c[1] * 100)).with("Ylw ", dv(c.c[2] * 100)).with("Blck", dv(c.c[3] * 100))
  of cmGrayscale: newDescriptor("Grsc").with("Gry ", dv((1 - c.c[0]) * 100))
  of cmLab:
    newDescriptor("LbCl").with("Lmnc", dv(c.c[0] * 100)).with("A   ", dv(c.c[1] * 255 - 128)).with("B   ", dv(c.c[2] * 255 - 128))
  else:
    let v = c.toRgb
    newDescriptor("RGBC").with("Rd  ", dv(v[0] * 255)).with("Grn ", dv(v[1] * 255)).with("Bl  ", dv(v[2] * 255))

proc gradientMethod*(d: Descriptor): Option[string] =
  result = enumOf(d, "gradientsInterpolationMethod")
  if result.isNone: result = enumOf(d, "gs99")

proc gradientStyle*(d: Descriptor): GradientStyle =
  case enumOf(d, "Type").get("")
  of "Rdl ": gsRadial
  of "Angl": gsAngle
  of "Rflc": gsReflected
  of "Dmnd": gsDiamond
  else: gsLinear

proc gradientStyleValue*(s: GradientStyle): Value =
  let v = (case s
    of gsLinear: "Lnr "
    of gsRadial: "Rdl "
    of gsAngle: "Angl"
    of gsReflected: "Rflc"
    of gsDiamond: "Dmnd")
  vEnum("GrdT", v)

type RawStops* = object
  stops*: seq[(float32, Color)]
  mids*: seq[float32]
  opacity*: seq[(float32, float32)]
  smooth*: float32

proc gradientStopsRaw(grad: Descriptor): RawStops =
  var stops: seq[(float32, Color)]
  var mids: seq[float32]
  let clrs = grad.getList("Clrs")
  if clrs.isSome:
    for it in clrs.get:
      if it.kind != vkDescriptor: continue
      let loc = float32(num(it.desc.get("Lctn")).get(0)) / 4096
      let cd = getD(it.desc, "Clr ")
      if cd.isSome:
        let c = colorFromDesc(cd.get)
        if c.isSome:
          stops.add (loc, c.get)
          mids.add float32(num(it.desc.get("Mdpn")).get(50)) / 100
  var order = newSeq[int](stops.len)
  for i in 0 ..< order.len: order[i] = i
  order.sort(proc(a, b: int): int = totalCmp(stops[a][0], stops[b][0]))
  for k in 1 ..< order.len:
    result.mids.add (if order[k] < mids.len: mids[order[k]] else: 0.5'f32)
  result.smooth = float32(num(grad.get("Intr")).get(0) / 4096)
  for i in order: result.stops.add stops[i]
  let trns = grad.getList("Trns")
  if trns.isSome:
    for it in trns.get:
      if it.kind != vkDescriptor: continue
      let loc = float32(num(it.desc.get("Lctn")).get(0)) / 4096
      result.opacity.add (loc, float32(num(it.desc.get("Opct")).get(100)) / 100)

proc gradientStopsWith*(grad: Descriptor; meth: Option[string]): (seq[(float32, Color)], seq[(float32, float32)]) =
  let raw = gradientStopsRaw(grad)
  let stops = (if raw.stops.len >= 2: bake(raw.stops, raw.mids, raw.smooth, methodFromCode(meth)) else: raw.stops)
  (stops, raw.opacity)

proc gradientStopsEditable*(grad: Descriptor; meth: Option[string]): RawStops =
  result = gradientStopsRaw(grad)
  let m = methodFromCode(meth)
  var lab = true
  for (_, c) in result.stops:
    if c.mode != cmLab: lab = false
  if result.stops.len >= 2 and (result.smooth > 0 or m != bmClassic or lab):
    result.stops = bake(result.stops, result.mids, result.smooth, m)
    result.mids.setLen(0)
  var plain = true
  for x in result.mids:
    if abs(x - 0.5) >= 1e-3: plain = false
  if plain: result.mids.setLen(0)
  var opaque = true
  for o in result.opacity:
    if abs(o[1] - 1) >= 1e-4: opaque = false
  if opaque: result.opacity.setLen(0)

proc gradientDescWith*(stops0: openArray[(float32, Color)]; opacity: openArray[(float32, float32)];
                       mids: openArray[float32]): Descriptor =
  var stops = @stops0
  if mids.len > 0: stops.sort(proc(a, b: (float32, Color)): int = totalCmp(a[0], b[0]))
  var clrs: seq[Value]
  for i, (t, c) in stops:
    let mid = (if i >= 1 and i - 1 < mids.len: mids[i - 1] else: 0.5'f32)
    clrs.add vDesc(newDescriptor("Clrt").with("Clr ", vDesc(colorToDesc(c))).with("Type", vEnum("Clry", "UsrS"))
                   .with("Lctn", vInt(int32(round(t * 4096)))).with("Mdpn", vInt(int32(round(clamp(mid, 0.05'f32, 0.95'f32) * 100)))))
  var op: seq[(float32, float32)]
  if opacity.len == 0:
    for (t, c) in stops: op.add (t, c.alpha)
  else: op = @opacity
  var trns: seq[Value]
  for (t, a) in op:
    trns.add vDesc(newDescriptor("TrnS").with("Opct", vUnit("#Prc", float64(a * 100)))
                   .with("Lctn", vInt(int32(round(t * 4096)))).with("Mdpn", vInt(50)))
  newDescriptor("Grdn").with("Nm  ", Value(kind: vkText, text: ustrNul("Custom"))).with("GrdF", vEnum("GrdF", "CstS"))
    .with("Intr", vDouble(0)).with("Clrs", vList(clrs)).with("Trns", vList(trns))

proc gradientDesc*(stops: openArray[(float32, Color)]; opacity: openArray[(float32, float32)]): Descriptor =
  gradientDescWith(stops, opacity, [])

proc smoothedOpacityStops(grad: Descriptor): seq[(float32, float32)] =
  var stops: seq[(float32, Color)]
  var mids: seq[float32]
  let trns = grad.getList("Trns")
  if trns.isSome:
    for it in trns.get:
      if it.kind != vkDescriptor: continue
      let a = float32(num(it.desc.get("Opct")).get(100)) / 100
      stops.add (float32(num(it.desc.get("Lctn")).get(0)) / 4096, rgba(a, a, a, 1))
      mids.add float32(num(it.desc.get("Mdpn")).get(50)) / 100
  var order = newSeq[int](stops.len)
  for i in 0 ..< order.len: order[i] = i
  order.sort(proc(a, b: int): int = totalCmp(stops[a][0], stops[b][0]))
  var m2: seq[float32]
  for k in 1 ..< order.len:
    if order[k] < mids.len: m2.add mids[order[k]]
  let smooth = float32(num(grad.get("Intr")).get(0) / 4096)
  let baked = (if stops.len >= 2: bake(stops, m2, smooth, bmClassic) else: stops)
  for (t, c) in baked: result.add (t, c.toRgb[0])

# ---------------------------------------------------------------- patterns

proc patternRef*(d: Descriptor): (string, string) =
  let p = getD(d, "Ptrn")
  if p.isNone: return ("", "")
  (textOf(p.get, "Nm  "), textOf(p.get, "Idnt"))

proc patternRefDesc*(name, id: string): Descriptor =
  result = newDescriptor("Ptrn").with("Nm  ", Value(kind: vkText, text: ustrNul(name)))
  if id.len > 0: result = result.with("Idnt", Value(kind: vkText, text: ustrNul(id)))

proc patternPlacement*(d: Descriptor): (float32, bool, (float32, float32)) =
  let angle = float32(num(d.get("Angl")).get(0))
  let link = not isFalse(d, "Algn")
  let p = getD(d, "phase")
  let phase = (if p.isSome: (float32(num(p.get.get("Hrzn")).get(0)), float32(num(p.get.get("Vrtc")).get(0))) else: (0'f32, 0'f32))
  (angle, link, phase)

proc withPatternPlacement*(d0: Descriptor; angle: float32; link: bool; phase: (float32, float32)): Descriptor =
  result = d0
  if angle != 0: result = result.with("Angl", vUnit("#Ang", float64(angle)))
  result = result.with("Algn", vBool(link)).with("phase", vDesc(newDescriptor("Pnt ")
    .with("Hrzn", vDouble(float64(phase[0]))).with("Vrtc", vDouble(float64(phase[1])))))

# ---------------------------------------------------------------- fill layers

proc parsePrefixVersioned*(data: openArray[byte]): Option[Descriptor] =
  try:
    let (v, _) = parseVersionedPrefix(data)
    some(v.descriptor)
  except CatchableError: none(Descriptor)

proc fillFromDesc*(key: string; d: Descriptor): Option[Fill] =
  case key
  of "SoCo":
    let cd = getD(d, "Clr ")
    if cd.isNone: return none(Fill)
    let c = colorFromDesc(cd.get)
    if c.isNone: return none(Fill)
    some(solidFill(c.get))
  of "GdFl":
    let angle = float32(num(d.get("Angl")).get(90))
    let scale = (let s = num(d.get("Scl ")); if s.isSome: float32(s.get) / 100 else: 1'f32)
    let g = getD(d, "Grad")
    var stops: seq[(float32, Color)]
    var midpoints: seq[float32]
    var opacityStops: seq[(float32, float32)]
    if g.isSome:
      let raw = gradientStopsEditable(g.get, gradientMethod(d))
      stops = raw.stops
      midpoints = raw.mids
      opacityStops = smoothedOpacityStops(g.get)
    var opaque = true
    for o in opacityStops:
      if o[1] < 1 - 1e-4: opaque = false
    if opaque: opacityStops.setLen(0)
    if stops.len == 0: stops = @[(0'f32, Black), (1'f32, White)]
    let o = getD(d, "Ofst")
    let offset = (if o.isSome: (float32(num(o.get.get("Hrzn")).get(0)) / 100, float32(num(o.get.get("Vrtc")).get(0)) / 100)
                  else: (0'f32, 0'f32))
    var f = gradientFill(stops, angle, scale, gradientStyle(d), boolOf(d, "Rvrs"))
    f.opacityStops = opacityStops
    f.midpoints = midpoints
    f.offset = offset
    f.dither = boolOf(d, "Dthr")
    f.align = not isFalse(d, "Algn")
    some(f)
  of "PtFl":
    let (name, id) = patternRef(d)
    let scale = (let s = num(d.get("Scl ")); if s.isSome: float32(s.get) / 100 else: 1'f32)
    let (angle, link, phase) = patternPlacement(d)
    some(Fill(kind: fkPattern, name: name, id: id, scale: scale, angle: angle, link: link, phase: phase))
  else: none(Fill)

proc parseFill*(key: string; data: openArray[byte]): Option[Fill] =
  let d = parsePrefixVersioned(data)
  if d.isNone: none(Fill) else: fillFromDesc(key, d.get)

proc fillToDesc*(f: Fill): (string, Descriptor) =
  case f.kind
  of fkSolid: ("SoCo", newDescriptor("null").with("Clr ", vDesc(colorToDesc(f.color))))
  of fkGradient:
    template prc(v: float32): Value = vUnit("#Prc", float64(v * 100))
    var d = newDescriptor("null").with("Angl", vUnit("#Ang", float64(f.angle))).with("Type", gradientStyleValue(f.style))
      .with("Scl ", prc(f.scale)).with("Grad", vDesc(gradientDescWith(f.stops, f.opacityStops, f.midpoints)))
    if f.reverse: d = d.with("Rvrs", vBool(true))
    if f.dither: d = d.with("Dthr", vBool(true))
    if not f.align: d = d.with("Algn", vBool(false))
    if f.offset != (0'f32, 0'f32):
      d = d.with("Ofst", vDesc(newDescriptor("Pnt ").with("Hrzn", prc(f.offset[0])).with("Vrtc", prc(f.offset[1]))))
    ("GdFl", d)
  of fkPattern:
    let d = newDescriptor("null").with("Scl ", vUnit("#Prc", float64(f.scale * 100))).with("Ptrn", vDesc(patternRefDesc(f.name, f.id)))
    ("PtFl", withPatternPlacement(d, f.angle, f.link, f.phase))

proc writeFill*(f: Fill): (string, seq[byte]) =
  let (k, d) = fillToDesc(f)
  (k, versioned(d).toBytes)

# ---------------------------------------------------------------- placed layers

proc parsePlacedWarp*(key: string; data: openArray[byte]): Option[Warp] =
  if key != "SoLd" and key != "SoLE" or data.len <= 8: return none(Warp)
  let d = parsePrefixVersioned(data.toOpenArray(8, data.len - 1))
  if d.isNone: return none(Warp)
  let w = getD(d.get, "warp")
  if w.isNone: return none(Warp)
  let ws = enumOf(w.get, "warpStyle")
  if ws.isNone: return none(Warp)
  let style = parseWarpStyle(ws.get)
  if style.isNone: return none(Warp)
  let b = getD(w.get, "bounds")
  if b.isNone: return none(Warp)
  let (l, t, r, bt) = (num(b.get.get("Left")), num(b.get.get("Top ")), num(b.get.get("Rght")), num(b.get.get("Btom")))
  if l.isNone or t.isNone or r.isNone or bt.isNone: return none(Warp)
  var warp = warpPreset(style.get, num(w.get.get("warpValue")).get(0), [l.get, t.get, r.get, bt.get])
  warp.hDistort = num(w.get.get("warpPerspective")).get(0)
  warp.vDistort = num(w.get.get("warpPerspectiveOther")).get(0)
  warp.vertical = enumOf(w.get, "warpRotate") == some("Vrtc")
  if style.get == wsCustom:
    let env = getD(w.get, "customEnvelopeWarp")
    if env.isNone: return none(Warp)
    let p = env.get.getPtr("meshPoints")
    if p == nil or p.kind != vkObjectArray: return none(Warp)
    proc vals(body: Descriptor; k: string): Option[seq[float64]] =
      let q = body.getPtr(k)
      if q != nil and q.kind == vkUnitFloats: some(q.values) else: none(seq[float64])
    let (xs, ys) = (vals(p.body, "Hrzn"), vals(p.body, "Vrtc"))
    if xs.isNone or ys.isNone or xs.get.len != 16 or ys.get.len != 16: return none(Warp)
    var mesh = BezierMesh(us: @[0.0, 1.0], vs: @[0.0, 1.0])
    for i in 0 ..< 16: mesh.points.add [xs.get[i], ys.get[i]]
    if not mesh.isValid: return none(Warp)
    warp.mesh = some(mesh)
  if warp.isIdentity: none(Warp) else: some(warp)

proc parseTysh*(data: openArray[byte]): Option[(string, Affine)] =
  if data.len < 52: return none((string, Affine))
  var m: array[6, float64]
  for i in 0 ..< 6:
    var v = 0'u64
    for k in 0 ..< 8: v = (v shl 8) or uint64(data[2 + i * 8 + k])
    m[i] = cast[float64](v)
  let d = parsePrefixVersioned(data.toOpenArray(52, data.len - 1))
  let text = (if d.isSome: textOf(d.get, "Txt ") else: "")
  some((text, Affine(m: m)))

proc parseSmart*(key: string; data: openArray[byte]): (string, Affine) =
  result = ("", IdentityAffine)
  if (key != "SoLd" and key != "SoLE") or data.len <= 8: return
  let d = parsePrefixVersioned(data.toOpenArray(8, data.len - 1))
  if d.isNone: return
  result[0] = textOf(d.get, "Idnt")
  let pts = d.get.getList("Trnf")
  let sz = getD(d.get, "Sz  ")
  if pts.isSome and sz.isSome:
    var p: seq[float64]
    for v in pts.get:
      let x = numOf(v)
      if x.isSome: p.add x.get
    let (w, h) = (num(sz.get.get("Wdth")).get(0), num(sz.get.get("Hght")).get(0))
    if p.len == 8 and w > 0 and h > 0:
      result[1] = Affine(m: [(p[2] - p[0]) / w, (p[3] - p[1]) / w, (p[6] - p[0]) / h, (p[7] - p[1]) / h, p[0], p[1]])

proc rectToQuad*(r: array[4, float64]; q: array[4, array[2, float64]]): Option[array[9, float64]] =
  ## The homography mapping the rectangle's corners (TL, TR, BR, BL) onto `q`.
  let src = [[r[0], r[1]], [r[2], r[1]], [r[2], r[3]], [r[0], r[3]]]
  var a = newSeq[float64](64)
  var b = newSeq[float64](8)
  for i in 0 .. 3:
    let (x, y) = (src[i][0], src[i][1])
    let (u, v) = (q[i][0], q[i][1])
    let r0 = 2 * i
    let r1 = 2 * i + 1
    for k, val in [x, y, 1.0, 0.0, 0.0, 0.0, -u * x, -u * y]: a[r0 * 8 + k] = val
    b[r0] = u
    for k, val in [0.0, 0.0, 0.0, x, y, 1.0, -v * x, -v * y]: a[r1 * 8 + k] = val
    b[r1] = v
  let h = solveDense(8, a, b)
  if h.len != 8: return none(array[9, float64])
  some([h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7], 1.0])

proc parseSmartPerspective*(key: string; data: openArray[byte]): Option[array[9, float64]] =
  if (key != "SoLd" and key != "SoLE") or data.len <= 8: return none(array[9, float64])
  let d = parsePrefixVersioned(data.toOpenArray(8, data.len - 1))
  if d.isNone: return none(array[9, float64])
  let sz = getD(d.get, "Sz  ")
  if sz.isNone: return none(array[9, float64])
  let (w, h) = (num(sz.get.get("Wdth")), num(sz.get.get("Hght")))
  if w.isNone or h.isNone: return none(array[9, float64])
  proc quad(d: Descriptor; key: string): Option[array[4, array[2, float64]]] =
    let pts = d.getList(key)
    if pts.isNone: return none(array[4, array[2, float64]])
    var p: seq[float64]
    for v in pts.get:
      let x = numOf(v)
      if x.isSome: p.add x.get
    if p.len != 8: return none(array[4, array[2, float64]])
    for v in p:
      if v.classify in {fcNan, fcInf, fcNegInf}: return none(array[4, array[2, float64]])
    some([[p[0], p[1]], [p[2], p[3]], [p[4], p[5]], [p[6], p[7]]])
  var q = quad(d.get, "nonAffineTransform")
  if q.isNone: q = quad(d.get, "Trnf")
  if q.isNone: return none(array[9, float64])
  var mx = 0.0
  for pt in q.get:
    for v in pt: mx = max(mx, abs(v))
  let tol = 1e-6 * (1 + mx)
  let qq = q.get
  var affine = true
  for i in 0 .. 1:
    if abs(qq[0][i] + qq[2][i] - qq[1][i] - qq[3][i]) > tol: affine = false
  if affine or w.get <= 0 or h.get <= 0: return none(array[9, float64])
  rectToQuad([0.0, 0.0, w.get, h.get], qq)

proc effectsEnabled*(lfx2: openArray[byte]): bool =
  if lfx2.len <= 4: return true
  let d = parsePrefixVersioned(lfx2.toOpenArray(4, lfx2.len - 1))
  if d.isNone: return true
  d.get.getBool("masterFXSwitch").get(true)
