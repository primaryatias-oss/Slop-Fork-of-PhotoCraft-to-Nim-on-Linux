## Paths and shape layers ↔ PSD: path records (saved/work/clipping paths), vector masks
## (`vmsk`/`vsms`), shape fills (`vscg`), stroke styles (`vstk`) and live shape origins (`vogk`).

import std/[options, math]
import ../geom, ../color, ../doc, ../psd, ./blocks

const
  SavedPathsFirst* = 2000'u16
  SavedPathsLast* = 2997'u16
  WorkPath* = 1025'u16
  ClippingPathId* = 2999'u16

proc opFromPsd(v: int16): PathOp =
  case v
  of 0: poExclude
  of 2: poSubtract
  of 3: poIntersect
  of -1: poJoin
  else: poCombine

proc opToPsd(op: PathOp): int16 =
  case op
  of poCombine: 1
  of poSubtract: 2
  of poIntersect: 3
  of poExclude: 0
  of poJoin: -1

proc pathFromRecords*(p: PathData; w, h: int): Path =
  let (fw, fh) = (float64(w), float64(h))
  for s in p.subpaths:
    var sp = Subpath(closed: s.closed, op: opFromPsd(s.operation))
    for k in s.knots:
      sp.knots.add Knot(anchor: point(k.anchor[0] * fw, k.anchor[1] * fh), inCtrl: point(k.pre[0] * fw, k.pre[1] * fh),
                        outCtrl: point(k.post[0] * fw, k.post[1] * fh), smooth: k.linked)
    result.subpaths.add sp
  result.fillRule = frNonZero
  result.inverted = p.initialFill

proc pathToRecords*(p: Path; w, h: int): PathData =
  let (fw, fh) = (float64(max(w, 1)), float64(max(h, 1)))
  var subs: seq[PsdSubpath]
  for s in p.subpaths:
    var ps = PsdSubpath(closed: s.closed, operation: opToPsd(s.op))
    for k in s.knots:
      ps.knots.add PsdKnot(linked: k.smooth, pre: (k.inCtrl.x / fw, k.inCtrl.y / fh), anchor: (k.anchor.x / fw, k.anchor.y / fh),
                           post: (k.outCtrl.x / fw, k.outCtrl.y / fh))
    subs.add ps
  fromSubpaths(subs, p.inverted)

proc pathFromResource*(data: openArray[byte]; w, h: int): Option[Path] =
  try: some(pathFromRecords(parsePathData(data), w, h))
  except CatchableError: none(Path)

proc layerPathFromRecords(p: PathData; w, h: int): Path =
  result = pathFromRecords(p, w, h)
  if result.subpaths.len == 0: return
  result.inverted = result.subpaths[0].op == poSubtract
  if result.inverted:
    for i in 1 ..< result.subpaths.len:
      case result.subpaths[i].op
      of poCombine: result.subpaths[i].op = poSubtract
      of poSubtract: result.subpaths[i].op = poCombine
      else: discard
    result.subpaths[0].op = poCombine

proc pathFromVmsk*(data: openArray[byte]; w, h: int): Option[(Path, uint32)] =
  var b: VectorMaskBlock
  try: b = parseVectorMask(data)
  except CatchableError: return none((Path, uint32))
  var p = layerPathFromRecords(b.path, w, h)
  if (b.flags and vmInvert) != 0: p.inverted = not p.inverted
  some((p, b.flags))

proc vmskBytes*(path: Path; flags0: uint32; w, h: int): seq[byte] =
  var p = path
  var flags = flags0 and not vmInvert
  if p.inverted and p.subpaths.len > 0:
    flags = flags or vmInvert
    p.inverted = false
  if p.subpaths.len > 0 and p.subpaths[0].op == poSubtract: p.subpaths[0].op = poCombine
  VectorMaskBlock(version: 3, flags: flags, path: pathToRecords(p, w, h)).toBytes

proc vectorMaskFromBlock*(data: openArray[byte]; w, h: int): Option[VectorMask] =
  let r = pathFromVmsk(data, w, h)
  if r.isNone: return none(VectorMask)
  var (path, flags) = r.get
  if path.subpaths.len == 0: path.inverted = not path.inverted
  some(VectorMask(path: path, enabled: (flags and vmDisabled) == 0, linked: (flags and vmNotLinked) == 0, density: 1, feather: 0))

proc vectorMaskBytes*(m: VectorMask; w, h: int): seq[byte] =
  var flags = 0'u32
  if not m.enabled: flags = flags or vmDisabled
  if not m.linked: flags = flags or vmNotLinked
  var path = m.path
  if path.subpaths.len == 0: path.inverted = not path.inverted
  vmskBytes(path, flags, w, h)

proc vectorMaskBlockMatches*(data: openArray[byte]; m: VectorMask; w, h: int): bool =
  let b = vectorMaskFromBlock(data, w, h)
  b.isSome and b.get.path == m.path and b.get.enabled == m.enabled and b.get.linked == m.linked

proc fillFromVscg*(data: openArray[byte]): Option[Fill] =
  if data.len < 4: return none(Fill)
  let key = char(data[0]) & char(data[1]) & char(data[2]) & char(data[3])
  let d = parsePrefixVersioned(data.toOpenArray(4, data.len - 1))
  if d.isNone: none(Fill) else: fillFromDesc(key, d.get)

proc vscgBytes*(f: Fill): seq[byte] =
  let (k, d) = fillToDesc(f)
  for c in k: result.add byte(c)
  result.add versioned(d).toBytes

type VstkInfo* = object
  stroke*: Option[ShapeStroke]
  fillEnabled*: bool

proc contentFill(d: Descriptor): Option[Fill] =
  let key = (case d.classId.s
    of "solidColorLayer": "SoCo"
    of "gradientLayer": "GdFl"
    of "patternLayer": "PtFl"
    else: (if d.get("Clr ").isSome: "SoCo" elif d.get("Grad").isSome: "GdFl" else: "PtFl"))
  fillFromDesc(key, d)

proc unitPx(v: Option[Value]; dpi: float32): Option[float64] =
  if v.isNone: return none(float64)
  let x = v.get
  if x.kind == vkUnitFloat:
    if x.unit == "#Pnt": return some(x.uv * float64(dpi) / 72)
    if x.unit == "#Mlm": return some(x.uv * float64(dpi) / 25.4)
  num(v)

proc parseVstk*(data: openArray[byte]; dpi: float32): Option[VstkInfo] =
  let dd = parsePrefixVersioned(data)
  if dd.isNone: return none(VstkInfo)
  let d = dd.get
  let fillEnabled = not isFalse(d, "fillEnabled")
  if not boolOf(d, "strokeEnabled"): return some(VstkInfo(fillEnabled: fillEnabled))
  let cap = (case enumOf(d, "strokeStyleLineCapType").get("")
    of "strokeStyleRoundCap": lcapRound
    of "strokeStyleSquareCap": lcapSquare
    else: lcapButt)
  let join = (case enumOf(d, "strokeStyleLineJoinType").get("")
    of "strokeStyleRoundJoin": ljRound
    of "strokeStyleBevelJoin": ljBevel
    else: ljMiter)
  let align = (case enumOf(d, "strokeStyleLineAlignment").get("")
    of "strokeStyleAlignInside": saInside
    of "strokeStyleAlignOutside": saOutside
    else: saCenter)
  var dashes: seq[float32]
  let l = d.getList("strokeStyleLineDashSet")
  if l.isSome:
    for v in l.get:
      let x = numOf(v)
      if x.isSome: dashes.add float32(x.get)
  let cd = getD(d, "strokeStyleContent")
  let paint = (if cd.isSome: contentFill(cd.get).get(solidFill(Black)) else: solidFill(Black))
  let op = num(d.get("strokeStyleOpacity"))
  let stroke = ShapeStroke(width: float32(unitPx(d.get("strokeStyleLineWidth"), dpi).get(1)), paint: paint,
                           opacity: (if op.isSome: float32(op.get) / 100 else: 1'f32), align: align, cap: cap, join: join,
                           miterLimit: float32(num(d.get("strokeStyleMiterLimit")).get(100)), dashes: dashes,
                           dashOffset: float32(num(d.get("strokeStyleLineDashOffset")).get(0)))
  some(VstkInfo(stroke: some(stroke), fillEnabled: fillEnabled))

proc vstkBytes*(stroke: Option[ShapeStroke]; fillEnabled: bool; dpi: float32): seq[byte] =
  let s = stroke.get(defaultShapeStroke())
  var (_, content) = fillToDesc(s.paint)
  content.classId = id(case s.paint.kind
    of fkSolid: "solidColorLayer"
    of fkGradient: "gradientLayer"
    of fkPattern: "patternLayer")
  var dashes: seq[Value]
  for v in s.dashes: dashes.add vUnit("#Nne", float64(v))
  let d = newDescriptor("strokeStyle").with("strokeStyleVersion", vInt(2)).with("strokeEnabled", vBool(stroke.isSome))
    .with("fillEnabled", vBool(fillEnabled)).with("strokeStyleLineWidth", vUnit("#Pxl", float64(s.width)))
    .with("strokeStyleLineDashOffset", vUnit("#Pnt", float64(s.dashOffset))).with("strokeStyleMiterLimit", vDouble(float64(s.miterLimit)))
    .with("strokeStyleLineCapType", vEnum("strokeStyleLineCapType", (case s.cap
      of lcapButt: "strokeStyleButtCap"
      of lcapRound: "strokeStyleRoundCap"
      of lcapSquare: "strokeStyleSquareCap")))
    .with("strokeStyleLineJoinType", vEnum("strokeStyleLineJoinType", (case s.join
      of ljMiter: "strokeStyleMiterJoin"
      of ljRound: "strokeStyleRoundJoin"
      of ljBevel: "strokeStyleBevelJoin")))
    .with("strokeStyleLineAlignment", vEnum("strokeStyleLineAlignment", (case s.align
      of saInside: "strokeStyleAlignInside"
      of saCenter: "strokeStyleAlignCenter"
      of saOutside: "strokeStyleAlignOutside")))
    .with("strokeStyleScaleLock", vBool(false)).with("strokeStyleStrokeAdjust", vBool(false))
    .with("strokeStyleLineDashSet", vList(dashes)).with("strokeStyleBlendMode", vEnum("BlnM", "Nrml"))
    .with("strokeStyleOpacity", vUnit("#Prc", float64(s.opacity * 100))).with("strokeStyleContent", vDesc(content))
    .with("strokeStyleResolution", vDouble(float64(dpi)))
  versioned(d).toBytes

proc rectOf(d: Descriptor): Option[array[4, float64]] =
  let b = getD(d, "keyOriginShapeBBox")
  if b.isNone: return none(array[4, float64])
  let (t, l, bt, r) = (num(b.get.get("Top ")), num(b.get.get("Left")), num(b.get.get("Btom")), num(b.get.get("Rght")))
  if t.isNone or l.isNone or bt.isNone or r.isNone: return none(array[4, float64])
  some([l.get, t.get, r.get - l.get, bt.get - t.get])

proc pointOf(d: Descriptor; key: string): Option[array[2, float64]] =
  let p = getD(d, key)
  if p.isNone: return none(array[2, float64])
  let (x, y) = (num(p.get.get("Hrzn")), num(p.get.get("Vrtc")))
  if x.isNone or y.isNone: none(array[2, float64]) else: some([x.get, y.get])

proc liveFromVogk*(data: openArray[byte]): Option[LiveShape] =
  if data.len <= 4: return none(LiveShape)
  let dd = parsePrefixVersioned(data.toOpenArray(4, data.len - 1))
  if dd.isNone: return none(LiveShape)
  let list = dd.get.getList("keyDescriptorList")
  if list.isNone or list.get.len != 1 or list.get[0].kind != vkDescriptor: return none(LiveShape)
  let o = list.get[0].desc
  if boolOf(o, "keyShapeInvalidated"): return none(LiveShape)
  let t = getD(o, "Trnf")
  if t.isSome:
    for (k, v) in [("xx", 1.0), ("xy", 0.0), ("yx", 0.0), ("yy", 1.0), ("tx", 0.0), ("ty", 0.0)]:
      if abs(num(t.get.get(k)).get(v) - v) >= 1e-9: return none(LiveShape)
  let tp = o.getPtr("keyOriginType")
  if tp == nil or tp.kind != vkInteger: return none(LiveShape)
  case tp.i
  of 1:
    let r = rectOf(o)
    if r.isSome: some(LiveShape(kind: lsRect, rect: r.get)) else: none(LiveShape)
  of 2:
    let rr = getD(o, "keyOriginRRectRadii")
    let r = rectOf(o)
    if rr.isNone or r.isNone: return none(LiveShape)
    template g(k: string): float64 = num(rr.get.get(k)).get(0)
    some(LiveShape(kind: lsRect, rect: r.get, radii: [g("topLeft"), g("topRight"), g("bottomRight"), g("bottomLeft")]))
  of 5:
    let r = rectOf(o)
    if r.isSome: some(LiveShape(kind: lsEllipse, rect: r.get)) else: none(LiveShape)
  of 4:
    let (a, b) = (pointOf(o, "keyOriginLineStart"), pointOf(o, "keyOriginLineEnd"))
    if a.isNone or b.isNone: return none(LiveShape)
    some(LiveShape(kind: lsLine, `from`: a.get, to: b.get, weight: num(o.get("keyOriginLineWeight")).get(1)))
  else: none(LiveShape)

proc pxv(v: float64): Value = vUnit("#Pxl", v)

proc vogkBytes*(s: LiveShape; dpi: float32): Option[seq[byte]] =
  proc bbox(r: array[4, float64]): Value =
    vDesc(newDescriptor("unitRect").with("unitValueQuadVersion", vInt(1)).with("Top ", pxv(r[1])).with("Left", pxv(r[0]))
      .with("Btom", pxv(r[1] + r[3])).with("Rght", pxv(r[0] + r[2])))
  proc base(ty: int32; dpi: float32): Descriptor =
    newDescriptor("null").with("keyOriginType", vInt(ty)).with("keyOriginResolution", vDouble(float64(dpi)))
  var o: Descriptor
  case s.kind
  of lsRect:
    if s.radii == [0.0, 0.0, 0.0, 0.0]: o = base(1, dpi).with("keyOriginShapeBBox", bbox(s.rect))
    else:
      o = base(2, dpi).with("keyOriginRRectRadii", vDesc(newDescriptor("radii").with("unitValueQuadVersion", vInt(1))
        .with("topRight", pxv(s.radii[1])).with("topLeft", pxv(s.radii[0])).with("bottomLeft", pxv(s.radii[3]))
        .with("bottomRight", pxv(s.radii[2])))).with("keyOriginShapeBBox", bbox(s.rect))
  of lsEllipse: o = base(5, dpi).with("keyOriginShapeBBox", bbox(s.rect))
  of lsLine:
    proc pnt(p: array[2, float64]): Value =
      vDesc(newDescriptor("Pnt ").with("Hrzn", vDouble(p[0])).with("Vrtc", vDouble(p[1])))
    let r = [min(s.`from`[0], s.to[0]), min(s.`from`[1], s.to[1]), abs(s.`from`[0] - s.to[0]), abs(s.`from`[1] - s.to[1])]
    o = base(4, dpi).with("keyOriginShapeBBox", bbox(r)).with("keyOriginLineEnd", pnt(s.to))
      .with("keyOriginLineStart", pnt(s.`from`)).with("keyOriginLineWeight", vDouble(s.weight))
      .with("keyOriginLineArrowSt", vBool(false)).with("keyOriginLineArrowEnd", vBool(false))
      .with("keyOriginLineArrWdth", vDouble(0)).with("keyOriginLineArrLngth", vDouble(0)).with("keyOriginLineArrConc", vInt(0))
  of lsPolygon: return none(seq[byte])
  o = o.with("keyOriginIndex", vInt(0))
  let d = newDescriptor("null").with("keyDescriptorList", vList(@[vDesc(o)]))
  var outp = @[0'u8, 0, 0, 1]
  outp.add versioned(d).toBytes
  some(outp)

proc shapeFromBlocks*(sh: var ShapeLayer; lookup: proc(key: string): Option[seq[byte]]; w, h: int; dpi: float32) =
  var d = lookup("vsms")
  if d.isNone: d = lookup("vmsk")
  if d.isSome:
    let p = pathFromVmsk(d.get, w, h)
    if p.isSome: sh.path = p.get[0]
  let f = lookup("vscg")
  if f.isSome:
    let fill = fillFromVscg(f.get)
    if fill.isSome: sh.fill = fill
  let s = lookup("vstk")
  if s.isSome:
    let v = parseVstk(s.get, dpi)
    if v.isSome:
      sh.stroke = v.get.stroke
      if not v.get.fillEnabled: sh.fill = none(Fill)
  let g = lookup("vogk")
  sh.live = (if g.isSome: liveFromVogk(g.get) else: none(LiveShape))

proc hasFillBlock(raw: seq[(string, seq[byte])]): bool =
  for (k, d) in raw:
    case k
    of "vscg":
      if fillFromVscg(d).isSome: return true
    of "SoCo", "GdFl", "PtFl":
      if parseFill(k, d).isSome: return true
    else: discard

proc shapeBlocks*(sh: ShapeLayer; raw: var seq[(string, seq[byte])]; w, h: int; dpi: float32) =
  proc find(raw: seq[(string, seq[byte])]; k: string): int =
    for i, (key, _) in raw:
      if key == k: return i
    -1
  var pos = find(raw, "vsms")
  if pos < 0: pos = find(raw, "vmsk")
  proc keep(d: openArray[byte]; sh: ShapeLayer; w, h: int): bool =
    let p = pathFromVmsk(d, w, h)
    p.isSome and p.get[0] == sh.path
  if not (pos >= 0 and keep(raw[pos][1], sh, w, h)):
    let data = (if not sh.psdRaw.isNil and keep(sh.psdRaw.data, sh, w, h): sh.psdRaw.data else: vmskBytes(sh.path, 0, w, h))
    if pos >= 0: raw[pos][1] = data else: raw.add ("vmsk", data)
  if sh.fill.isSome:
    let i = find(raw, "vscg")
    if i >= 0 and fillFromVscg(raw[i][1]) != sh.fill: raw[i][1] = vscgBytes(sh.fill.get)
  var present = false
  for (k, _) in raw:
    if k in ["vscg", "SoCo", "GdFl", "PtFl"]: present = true
  if sh.fill.isNone and not present: raw.add ("vscg", vscgBytes(solidFill(Black)))
  let undecodableFill = sh.fill.isNone and present and not hasFillBlock(raw)
  let vi = find(raw, "vstk")
  if vi >= 0:
    let v = parseVstk(raw[vi][1], dpi)
    let same = v.isSome and v.get.stroke == sh.stroke and
               (v.get.fillEnabled == sh.fill.isSome or (v.get.fillEnabled and undecodableFill))
    if not same: raw[vi][1] = vstkBytes(sh.stroke, sh.fill.isSome, dpi)
  elif sh.stroke.isSome or (sh.fill.isNone and hasFillBlock(raw)):
    raw.add ("vstk", vstkBytes(sh.stroke, sh.fill.isSome, dpi))
  let gi = find(raw, "vogk")
  let current = (if gi >= 0: liveFromVogk(raw[gi][1]) else: none(LiveShape))
  if current != sh.live or (gi < 0 and sh.live.isSome):
    let generated = (if sh.live.isSome: vogkBytes(sh.live.get, dpi) else: none(seq[byte]))
    if gi >= 0 and generated.isSome: raw[gi][1] = generated.get
    elif gi >= 0: raw.delete(gi)
    elif generated.isSome: raw.add ("vogk", generated.get)
