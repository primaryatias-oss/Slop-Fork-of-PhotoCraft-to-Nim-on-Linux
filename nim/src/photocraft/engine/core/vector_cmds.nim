## Vector commands (port of `vector_cmds.rs`): shape layers (`shape.*`), document paths (`path.*`,
## Paths panel), selection ⇄ path, and layer vector masks (`layer.vectorMask.*`).
##
## Coordinates are document pixels. Paths in parameters and results use a compact JSON form:
## `{"subpaths":[{"closed":true,"op":"combine|subtract|intersect|exclude","knots":[[x,y],
## {"anchor":[x,y],"in":[x,y],"out":[x,y],"smooth":true}, …]}],"fillRule":"nonzero|evenodd",
## "inverted":false}`. A knot given as `[x, y]` is a corner with retracted handles. Colours are
## `"#rrggbb"`, `"#rrggbbaa"` or `[r,g,b,a]` (0..1); opacities are percentages (0..100).
##
## Every edit re-renders the shape layer's cache in one undoable step. Part of the `engine/core`
## unit: included, not imported.

proc warpFromParams*(cmdId: string; p: JsonNode; frame: array[4, float64]): Warp
proc validateBrush*(brush: BrushSettings; cmdId: string)

const
  ShapeBlocks = ["vmsk", "vsms", "vogk", "vstk", "vscg", "SoCo", "GdFl", "PtFl"]
    ## Keys PSD uses for a shape layer's vector data; dropped when a shape is rasterized.
  PathForm* = """path: {"subpaths":[{"closed":bool=true,"op":"combine|subtract|intersect|exclude","knots":[[x,y] | {"anchor":[x,y],"in":[x,y],"out":[x,y],"smooth":bool}]}],"fillRule":"nonzero|evenodd","inverted":bool}"""
  CombineParams = """{"layer":id?,"subpath":index? (default all but the first)} (no layer/subpath and two or more shape layers selected: merges them into the top-most, whose components above the bottom layer's take the operation)"""

proc vecHasLayerE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if d.activeLayer.isNone or d.doc[].findLayer(d.activeLayer.get).isNil: "no active layer" else: ""

proc vecHasSelectionE(s: Session): string =
  let d = s.active
  if d.isNil or d.doc.selection.isNone: "no selection" else: ""

proc activeShape(s: Session): (ptr ShapeLayer, string) =
  let d = s.active
  if d.isNil: return (nil, "no document open")
  if d.activeLayer.isNone: return (nil, "no active layer")
  let l = d.doc[].findLayer(d.activeLayer.get)
  if l.isNil: return (nil, "no active layer")
  if l.content.kind != lkShape: return (nil, "active layer is not a shape layer")
  (addr l.content.shape, "")

proc hasShapeFillE(s: Session): string =
  let (sh, why) = activeShape(s)
  if sh.isNil: why elif sh.fill.isNone: "shape has no fill" else: ""

proc hasShapeStrokeE(s: Session): string =
  let (sh, why) = activeShape(s)
  if sh.isNil: why elif sh.stroke.isNone: "shape has no stroke" else: ""

proc canPasteShapeFillE(s: Session): string =
  let (sh, why) = activeShape(s)
  if sh.isNil: why elif s.pathFillClipboard.isNone: "no shape fill copied" else: ""

proc canPasteShapeStrokeE(s: Session): string =
  let (sh, why) = activeShape(s)
  if sh.isNil: why elif s.pathStrokeClipboard.isNone: "no shape stroke copied" else: ""

proc vecLayerId*(s: Session; p: JsonNode): LayerId =
  ## `layer`, else the active layer.
  let v = p.pget("layer").asU64
  if v.isSome: return LayerId(v.get)
  let d = s.active
  if d.isNil or d.activeLayer.isNone: raise other("no active layer")
  d.activeLayer.get

proc f64p(p: JsonNode; k: string): Option[float64] = p.pget(k).asF64

proc vecPt*(v: JsonNode): Option[Point] =
  if v.isNil: return
  if v.isArray and v.len >= 2:
    let x = v.elems[0].asF64
    let y = v.elems[1].asF64
    if x.isSome and y.isSome: return some(point(x.get, y.get))
  elif v.isObject:
    let x = v.pget("x").asF64
    let y = v.pget("y").asF64
    if x.isSome and y.isSome: return some(point(x.get, y.get))

proc nums*(p: JsonNode; k: string; n: static int): Option[array[n, float64]] =
  let a = p.pget(k)
  if not a.isArray or a.len < n: return
  var outp: array[n, float64]
  for i in 0 ..< n:
    let v = a.elems[i].asF64
    if v.isNone: return
    outp[i] = v.get
  some(outp)

proc radiiParam(p: JsonNode): Option[array[4, float64]] =
  ## `[tl, tr, br, bl]` from an array or a single number.
  var v = p.pget("radii")
  if v.isNil: v = p.pget("radius")
  if v.isNil: return
  if v.kind in {JInt, JFloat}:
    let r = v.asF64.get
    return some([r, r, r, r])
  nums(p, "radii", 4)

proc vecColor*(v: JsonNode): Option[Color] =
  if v.isNil: return
  if v.isArray and v.len >= 3:
    var c: seq[float32]
    for x in v.elems:
      let f = x.asF64
      c.add float32(if f.isSome: f.get else: 0'f64)
    return some(rgba(c[0], c[1], c[2], if c.len > 3: c[3] else: 1'f32))
  if v.kind == JString:
    var s = v.str
    var i = 0
    while i < s.len and s[i] == '#': inc i
    s = s[i .. ^1]
    proc h(s: string; i: int): Option[float32] =
      if i + 2 > s.len: return
      let b = hexByte(s[i ..< i + 2])
      if b.isSome: some(float32(b.get) / 255) else: none(float32)
    if s.len == 6:
      let (r, g, b) = (h(s, 0), h(s, 2), h(s, 4))
      if r.isSome and g.isSome and b.isSome: return some(rgb(r.get, g.get, b.get))
    elif s.len == 8:
      let (r, g, b, a) = (h(s, 0), h(s, 2), h(s, 4), h(s, 6))
      if r.isSome and g.isSome and b.isSome and a.isSome: return some(rgba(r.get, g.get, b.get, a.get))

proc vecHex(c: Color): string =
  let v = c.toRgb
  proc q(v: float32): string = toHex(int(asU8(round(clamp(v, 0'f32, 1'f32) * 255))), 2).toLowerAscii
  if c.alpha < 1: "#" & q(v[0]) & q(v[1]) & q(v[2]) & q(c.alpha) else: "#" & q(v[0]) & q(v[1]) & q(v[2])

proc opFrom(s: string): Option[PathOp] =
  case s.toLowerAscii
  of "combine", "add", "union": some(poCombine)
  of "subtract", "minus": some(poSubtract)
  of "intersect": some(poIntersect)
  of "exclude", "xor": some(poExclude)
  of "join": some(poJoin)
  else: none(PathOp)

proc opName(op: PathOp): string =
  case op
  of poCombine: "combine"
  of poSubtract: "subtract"
  of poIntersect: "intersect"
  of poExclude: "exclude"
  of poJoin: "join"

proc parsePath*(v: JsonNode): Path =
  ## Parses a path from the compact JSON form (or the serde form); raises `other(msg)`.
  let subs = v.pget("subpaths")
  if not subs.isArray: raise other("path needs `subpaths`")
  for i, s in subs.elems:
    let knotsV = s.pget("knots")
    if not knotsV.isArray: raise other("subpath " & $i & " needs `knots`")
    var knots: seq[Knot]
    for j, k in knotsV.elems:
      if k.isArray:
        let a = vecPt(k)
        if a.isNone: raise other("knot " & $i & "." & $j & ": expected [x, y]")
        knots.add Knot(anchor: a.get, inCtrl: a.get, outCtrl: a.get)
      else:
        let a = vecPt(k.pget("anchor"))
        if a.isNone: raise other("knot " & $i & "." & $j & ": missing anchor")
        proc get(k: JsonNode; keys: openArray[string]): Option[Point] =
          for key in keys:
            let q = vecPt(k.pget(key))
            if q.isSome: return q
        let ic = get(k, ["in", "inCtrl", "in_ctrl"])
        let oc = get(k, ["out", "outCtrl", "out_ctrl"])
        knots.add Knot(anchor: a.get, inCtrl: (if ic.isSome: ic.get else: a.get), outCtrl: (if oc.isSome: oc.get else: a.get),
                       smooth: k.getBoolOr("smooth", false))
    var op = poCombine
    let ov = s.pget("op")
    if not ov.isNil and ov.kind == JString:
      let o = opFrom(ov.str)
      if o.isNone: raise other("subpath " & $i & ": unknown op `" & ov.str & "`")
      op = o.get
    result.subpaths.add Subpath(closed: s.getBoolOr("closed", true), knots: knots, op: op)
  var fr = v.pget("fillRule")
  if fr.isNil: fr = v.pget("fill_rule")
  let frs = if not fr.isNil and fr.kind == JString: fr.str.toLowerAscii else: ""
  result.fillRule = if frs in ["evenodd", "even-odd", "even_odd"]: frEvenOdd else: frNonZero
  result.inverted = v.getBoolOr("inverted", false)

proc ptJson(q: Point): JsonNode = %*[q.x, q.y]

proc pathJson*(p: Path): JsonNode =
  ## The compact JSON form of a path.
  var subs = newJArray()
  for s in p.subpaths:
    var knots = newJArray()
    for k in s.knots:
      knots.add %*{"anchor": ptJson(k.anchor), "in": ptJson(k.inCtrl), "out": ptJson(k.outCtrl), "smooth": k.smooth}
    subs.add %*{"closed": s.closed, "op": opName(s.op), "knots": knots}
  %*{"subpaths": subs, "fillRule": (if p.fillRule == frEvenOdd: "evenodd" else: "nonzero"), "inverted": p.inverted}

proc fillJson(f: Fill): JsonNode =
  case f.kind
  of fkSolid: %vecHex(f.color)
  of fkGradient:
    var stops = newJArray()
    for (t, c) in f.stops: stops.add %*[float64(t), vecHex(c)]
    %*{"gradient": {"stops": stops, "angle": float64(f.angle), "scale": float64(f.scale * 100),
                    "style": ($f.style).toLowerAscii, "reverse": f.reverse}}
  of fkPattern: %*{"pattern": f.name, "scale": float64(f.scale * 100)}

proc parseFill(v: JsonNode): Option[Fill] =
  ## `"#rrggbb"` / `[r,g,b,a]` / `{"gradient":{…}}` / `{"pattern":name,"scale"}`; `null` → none
  ## (no fill). Raises `other(msg)` for anything else.
  if v.isNull: return none(Fill)
  let c = vecColor(v)
  if c.isSome: return some(solidFill(c.get))
  let g = v.pget("gradient")
  if not g.isNil:
    var stops: seq[(float32, Color)]
    let sv = g.pget("stops")
    if sv.isArray:
      for s in sv.elems:
        let t = jIdx(s, 0).asF64
        let cc = vecColor(jIdx(s, 1))
        if t.isSome and cc.isSome: stops.add (float32(t.get), cc.get)
    else: stops = @[(0'f32, Black), (1'f32, White)]
    let style = case g.getStrOr("style", "linear")
      of "radial": gsRadial
      of "angle": gsAngle
      of "reflected": gsReflected
      of "diamond": gsDiamond
      else: gsLinear
    let angle = f64p(g, "angle")
    let scale = f64p(g, "scale")
    return some(gradientFill(stops, float32(if angle.isSome: angle.get else: 90.0),
                             float32(if scale.isSome: scale.get else: 100.0) / 100, style, g.getBoolOr("reverse", false)))
  let name = v.pget("pattern")
  if not name.isNil and name.kind == JString:
    let scale = f64p(v, "scale")
    var f = patternFill(name.str, "", float32(if scale.isSome: scale.get else: 100.0) / 100)
    f.angle = 0
    f.link = true
    f.phase = (0'f32, 0'f32)
    return some(f)
  raise other("unrecognised fill " & jsonText(v))

proc strokeJson(s: ShapeStroke): JsonNode =
  var dashes = newJArray()
  for d in s.dashes: dashes.add %float64(d)
  %*{"width": float64(s.width), "fill": fillJson(s.paint), "opacity": float64(s.opacity * 100),
     "align": ($s.align).toLowerAscii, "cap": ($s.cap).toLowerAscii, "join": ($s.join).toLowerAscii,
     "miterLimit": float64(s.miterLimit), "dashes": dashes, "dashOffset": float64(s.dashOffset)}

proc parseStroke*(v: JsonNode; base: Option[ShapeStroke]; fg: Color): Option[ShapeStroke] =
  ## Applies stroke keys onto `base` (`null` → no stroke); raises `other(msg)`.
  if v.isNull or (v.kind == JBool and not v.bval): return none(ShapeStroke)
  if not v.isObject: raise other("stroke must be an object or null")
  for (key, lo, hi) in [("width", 0.0, 1_000_000.0), ("opacity", 0.0, 100.0), ("miterLimit", 1.0, 500.0),
                        ("dashOffset", -1_000_000.0, 1_000_000.0)]:
    let value = v.pget(key)
    if not value.isNil:
      let n = value.asF64
      if not (n.isSome and n.get == n.get and abs(n.get) != Inf and n.get >= lo and n.get <= hi):
        raise other("`" & key & "` must be a finite number between " & displayFloat(lo) & " and " & displayFloat(hi))
  for key in ["align", "cap", "join"]:
    let value = v.pget(key)
    if not value.isNil and value.kind != JString: raise other("`" & key & "` must be a string")
  var s = if base.isSome: base.get else: (var d = defaultShapeStroke(); d.paint = solidFill(fg); d)
  let w = f64p(v, "width")
  if w.isSome: s.width = float32(w.get)
  var c = v.pget("color")
  if c.isNil: c = v.pget("fill")
  if not c.isNil:
    let f = parseFill(c)
    if f.isNone: raise other("stroke colour cannot be null")
    s.paint = f.get
  let o = f64p(v, "opacity")
  if o.isSome: s.opacity = float32(o.get / 100)
  let a = v.pget("align")
  if not a.isNil:
    s.align = case a.str
      of "inside": saInside
      of "outside": saOutside
      of "center", "centre": saCenter
      else: raise other("unknown align `" & a.str & "`")
  let cp = v.pget("cap")
  if not cp.isNil:
    s.cap = case cp.str
      of "butt": lcapButt
      of "round": lcapRound
      of "square": lcapSquare
      else: raise other("unknown cap `" & cp.str & "`")
  let j = v.pget("join")
  if not j.isNil:
    s.join = case j.str
      of "miter": ljMiter
      of "round": ljRound
      of "bevel": ljBevel
      else: raise other("unknown join `" & j.str & "`")
  let m = f64p(v, "miterLimit")
  if m.isSome: s.miterLimit = float32(m.get)
  let d = v.pget("dashes")
  if not d.isNil:
    if d.kind == JNull: s.dashes = @[]
    elif d.isArray and d.len <= 32:
      var dashes: seq[float32]
      for x in d.elems:
        let n = x.asF64
        if not (n.isSome and n.get == n.get and abs(n.get) != Inf and n.get >= 0 and n.get <= 10000):
          raise other("dash lengths must be finite numbers between 0 and 10000 widths")
        dashes.add float32(n.get)
      s.dashes = dashes
    else: raise other("`dashes` must be an array (multiples of the width)")
  let off = f64p(v, "dashOffset")
  if off.isSome: s.dashOffset = float32(off.get)
  let err = s.validate
  if err.isSome: raise other(err.get)
  some(s)

proc liveFromParams(cmdId, kind: string; p: JsonNode; prev: Option[LiveShape]): LiveShape =
  ## Builds a live shape from `kind` + geometry params.
  let prevRect = if prev.isSome and prev.get.kind in {lsRect, lsEllipse, lsPolygon}: some(prev.get.rect) else: none(array[4, float64])
  proc rect(p: JsonNode; prevRect: Option[array[4, float64]]; cmdId: string): array[4, float64] =
    let r = nums(p, "rect", 4)
    if r.isSome: return r.get
    if prevRect.isSome: return prevRect.get
    raise bad(cmdId, "`rect`: [x, y, w, h] is required")
  case kind
  of "rect", "rectangle":
    let r = if prev.isSome and prev.get.kind == lsRect: prev.get.radii else: [0.0, 0.0, 0.0, 0.0]
    let rc = rect(p, prevRect, cmdId)
    let rp = radiiParam(p)
    LiveShape(kind: lsRect, rect: rc, radii: if rp.isSome: rp.get else: r)
  of "roundedRect", "roundRect":
    var r = [10.0, 10.0, 10.0, 10.0]
    if prev.isSome and prev.get.kind == lsRect:
      for v in prev.get.radii:
        if v > 0:
          r = prev.get.radii
          break
    let rc = rect(p, prevRect, cmdId)
    let rp = radiiParam(p)
    LiveShape(kind: lsRect, rect: rc, radii: if rp.isSome: rp.get else: r)
  of "ellipse": LiveShape(kind: lsEllipse, rect: rect(p, prevRect, cmdId))
  of "polygon", "star":
    var (ps, pr) = (5'u32, if kind == "star": 0.5 else: 1.0)
    if prev.isSome and prev.get.kind == lsPolygon: (ps, pr) = (prev.get.sides, prev.get.starRatio)
    let sv = f64p(p, "sides")
    let sides = if sv.isSome: uint32(clamp(round(sv.get), 3.0, 100.0)) else: ps
    let rv = f64p(p, "starRatio")
    let ratio = if rv.isSome: clamp(rv.get, 0.01, 1.0) elif kind == "star" and pr >= 1.0: 0.5 else: pr
    let rc = rect(p, prevRect, cmdId)
    LiveShape(kind: lsPolygon, rect: rc, sides: sides, starRatio: if kind == "polygon" and rv.isNone: 1.0 else: ratio)
  of "line":
    var (pf, pto, pw) = (none(array[2, float64]), none(array[2, float64]), 1.0)
    if prev.isSome and prev.get.kind == lsLine: (pf, pto, pw) = (some(prev.get.`from`), some(prev.get.to), prev.get.weight)
    var fromV = nums(p, "from", 2)
    if fromV.isNone: fromV = pf
    if fromV.isNone: raise bad(cmdId, "`from`: [x, y] is required")
    var toV = nums(p, "to", 2)
    if toV.isNone: toV = pto
    if toV.isNone: raise bad(cmdId, "`to`: [x, y] is required")
    let wv = f64p(p, "weight")
    LiveShape(kind: lsLine, `from`: fromV.get, to: toV.get, weight: rmax(if wv.isSome: wv.get else: pw, 0.0))
  else: raise bad(cmdId, "unknown kind `" & kind & "`")

proc shapePath*(p: JsonNode): Path =
  ## The path `shape.create` draws for live-shape params `p` (the Shape tools preview it).
  livePath(liveFromParams("shape.create", p.getStrOr("kind", "rect"), p, none(LiveShape)))

proc liveKind(l: LiveShape): string =
  case l.kind
  of lsRect:
    for r in l.radii:
      if r > 0: return "roundedRect"
    "rect"
  of lsEllipse: "ellipse"
  of lsPolygon: (if l.starRatio < 1: "star" else: "polygon")
  of lsLine: "line"

proc refreshShape*(doc: Document; sh: var ShapeLayer) =
  ## Re-renders a shape layer's cache for `doc` (canvas-clipped, document pixel format).
  sh.cache = some(renderShape(sh, doc.pixelFormat, doc.bounds))

proc transformShape*(sh: var ShapeLayer; a: Affine) =
  ## Applies an affine transform to a shape's path, keeping the live shape when the transform is a
  ## translation plus axis-aligned scale (otherwise the shape stops being parametric).
  sh.path = sh.path.transform(a)
  let (m0, m1, m2, m3, tx, ty) = (a.m[0], a.m[1], a.m[2], a.m[3], a.m[4], a.m[5])
  let axisAligned = abs(m1) < 1e-12 and abs(m2) < 1e-12 and m0 > 0 and m3 > 0
  let live = sh.live
  sh.live = none(LiveShape)
  if live.isSome:
    let l = live.get
    if axisAligned:
      proc r(r: array[4, float64]): array[4, float64] = [r[0] * m0 + tx, r[1] * m3 + ty, r[2] * m0, r[3] * m3]
      proc pt(p: array[2, float64]): array[2, float64] = [p[0] * m0 + tx, p[1] * m3 + ty]
      case l.kind
      of lsRect:
        let k = min(m0, m3)
        var radii = l.radii
        for v in radii.mitems: v *= k
        sh.live = some(LiveShape(kind: lsRect, rect: r(l.rect), radii: radii))
      of lsEllipse: sh.live = some(LiveShape(kind: lsEllipse, rect: r(l.rect)))
      of lsPolygon: sh.live = some(LiveShape(kind: lsPolygon, rect: r(l.rect), sides: l.sides, starRatio: l.starRatio))
      of lsLine:
        # A line's width scales perpendicular to its direction (#3063).
        let (dx, dy) = (l.to[0] - l.`from`[0], l.to[1] - l.`from`[1])
        let oldLength = hypot(dx, dy)
        let newLength = hypot(dx * m0, dy * m3)
        let widthScale = if oldLength > 0 and newLength > 0: m0 * m3 * oldLength / newLength else: min(m0, m3)
        sh.live = some(LiveShape(kind: lsLine, `from`: pt(l.`from`), to: pt(l.to), weight: l.weight * widthScale))
    elif (abs(m1) < 1e-12 and abs(m2) < 1e-12) or (abs(m0) < 1e-12 and abs(m3) < 1e-12):
      # Flips and 90° turns keep rectangles, ellipses and lines live (Image Rotation).
      proc pt(p: array[2, float64]): array[2, float64] = [m0 * p[0] + m2 * p[1] + tx, m1 * p[0] + m3 * p[1] + ty]
      proc r(r: array[4, float64]): array[4, float64] =
        let (p, q) = (pt([r[0], r[1]]), pt([r[0] + r[2], r[1] + r[3]]))
        [min(p[0], q[0]), min(p[1], q[1]), abs(q[0] - p[0]), abs(q[1] - p[1])]
      let k = min(abs(m0) + abs(m1), abs(m2) + abs(m3))
      case l.kind
      of lsRect:
        var uniform = true
        for v in l.radii:
          if not (abs(v - l.radii[0]) < 1e-9): uniform = false
        if uniform:
          var radii = l.radii
          for v in radii.mitems: v *= k
          sh.live = some(LiveShape(kind: lsRect, rect: r(l.rect), radii: radii))
      of lsEllipse: sh.live = some(LiveShape(kind: lsEllipse, rect: r(l.rect)))
      of lsLine: sh.live = some(LiveShape(kind: lsLine, `from`: pt(l.`from`), to: pt(l.to), weight: l.weight * k))
      else: discard
  sh.psdRaw = nil

proc translateVectors*(doc: Document; l: var Layer; dx, dy: float64) =
  ## Moves a layer's vector content by `(dx, dy)`: shape paths (re-rendered) and linked vector
  ## masks, recursing into groups.
  let a = translateAffine(dx, dy)
  if l.vectorMask.isSome and l.vectorMask.get.linked: l.vectorMask.get.path = l.vectorMask.get.path.transform(a)
  case l.content.kind
  of lkShape:
    transformShape(l.content.shape, a)
    refreshShape(doc, l.content.shape)
  of lkGroup:
    for c in l.content.group.children.mitems: translateVectors(doc, c, dx, dy)
  else: discard

proc finitePath(path: Path): bool =
  for s in path.subpaths:
    for k in s.knots:
      for q in [k.anchor, k.inCtrl, k.outCtrl]:
        if q.x != q.x or abs(q.x) == Inf or q.y != q.y or abs(q.y) == Inf: return false
  true

proc mapPath*(path: Path; h: Option[Homography]; warp: Option[Warp]): Path =
  ## `path` through Distort / Perspective (`h`) or Warp; refused when the map sends a point to
  ## infinity (past a perspective's horizon).
  if warp.isSome:
    let w = warp.get
    result = shapes.mapPath(path, proc(p: Point): Point =
      let (x, y) = w.map(p.x, p.y)
      point(x, y), 0.1)
  elif h.isSome:
    let hh = h.get
    result = mapPoints(path, proc(p: Point): Point =
      let (x, y) = hh.apply(p.x, p.y)
      point(x, y))
  else: result = path
  if not finitePath(result): raise other("the distortion sends part of the path out of bounds")

proc mapShape*(sh: var ShapeLayer; h: Option[Homography]; warp: Option[Warp]) =
  ## Distort, Perspective or Warp of a shape layer: the shape stays a shape, its path mapped.
  sh.path = mapPath(sh.path, h, warp)
  sh.live = none(LiveShape)
  sh.psdRaw = nil

proc shapeInfo(s: Session; id: LayerId): JsonNode =
  let d = s.activeOrRaise()
  let l = d.doc[].findLayer(id)
  if l.isNil: raise noLayer(id)
  if l.content.kind != lkShape:
    raise other("layer " & $uint64(id) & " is " & l.content.article & " " & l.content.kindName & " layer, not a shape layer")
  let sh = l.content.shape
  var bounds = newJNull()
  if sh.cache.isSome:
    let r = sh.cache.get.contentBounds
    if not r.isEmpty: bounds = %*[r.x0, r.y0, r.width, r.height]
  %*{"layer": uint64(id), "name": l.name, "path": pathJson(sh.path),
     "fill": (if sh.fill.isSome: fillJson(sh.fill.get) else: newJNull()),
     "stroke": (if sh.stroke.isSome: strokeJson(sh.stroke.get) else: newJNull()),
     "live": (if sh.live.isSome: toJ(sh.live.get) else: newJNull()),
     "kind": (if sh.live.isSome: liveKind(sh.live.get) else: "path"), "bounds": bounds}

proc withShape*(s: var Session; id: LayerId; label: string; f: proc(sh: var ShapeLayer; l: var Layer)) =
  ## One history step editing shape layer `id` (re-rendered after `f`).
  let st = s.active
  if not st.isNil:
    let locks = st.doc[].effectiveLocks(id)
    if locks.all or locks.pixels: raise other("shape layer is locked")
  s.editDo(label):
    let snapshot = doc
    let l = doc.layerOrRaise(id)
    if l.content.kind != lkShape:
      raise other("layer " & $uint64(id) & " is " & l.content.article & " " & l.content.kindName & " layer, not a shape layer")
    var sh = l.content.shape
    f(sh, l[])
    refreshShape(snapshot, sh)
    l.content.shape = sh

proc defaultShapeName(kind: string): string =
  case kind
  of "rect", "rectangle": "Rectangle"
  of "roundedRect", "roundRect": "Rounded Rectangle"
  of "ellipse": "Ellipse"
  of "polygon": "Polygon"
  of "star": "Star"
  of "line": "Line"
  else: "Shape"

proc fgColor(s: Session): Color =
  let fg = s.tools.foreground
  rgba(fg[0], fg[1], fg[2], fg[3])

proc checkShape(cmdId: string; path: Path; stroke: Option[ShapeStroke]) =
  let e = validateShape(path, stroke)
  if e.isSome: raise bad(cmdId, e.get)

template badOf(cmdId: string; body: untyped): untyped =
  ## Runs `body`, turning its `other(msg)` errors into `bad(cmdId, msg)`.
  try: body
  except EngineError as e:
    if e.kind == eeOther: raise bad(cmdId, e.detail) else: raise

proc shapeCreate(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "shape.create"
  let kind = p.getStrOr("kind", "rect")
  var path: Path
  var live = none(LiveShape)
  if kind == "path":
    let v = p.pget("path")
    if v.isNil: raise bad(cmdId, "kind \"path\" needs `path`")
    path = badOf(cmdId, parsePath(v))
  else:
    let l = liveFromParams(cmdId, kind, p, none(LiveShape))
    path = livePath(l)
    live = some(l)
  let fgc = fgColor(s)
  let fv = p.pget("fill")
  let fill = if not fv.isNil: badOf(cmdId, parseFill(fv)) else: some(solidFill(fgc))
  let sv = p.pget("stroke")
  let stroke = if not sv.isNil: badOf(cmdId, parseStroke(sv, none(ShapeStroke), fgc)) else: none(ShapeStroke)
  checkShape(cmdId, path, stroke)
  # Add to an existing shape layer with a path operation (Shape tool in combine/subtract… mode).
  let target = p.pget("addTo").asU64
  if target.isSome:
    let ov = p.pget("op")
    let op = if not ov.isNil and ov.kind == JString: opFrom(ov.str) else: some(poCombine)
    if op.isNone: raise bad(cmdId, "unknown `op`")
    let id = LayerId(target.get)
    withShape(s, id, "Add Shape", proc(sh: var ShapeLayer; l: var Layer) =
      for sp0 in path.subpaths:
        var sp = sp0
        sp.op = op.get
        sh.path.subpaths.add sp
      sh.live = none(LiveShape)
      sh.psdRaw = nil
      checkShape(cmdId, sh.path, sh.stroke))
    return shapeInfo(s, id)
  let nv = p.pget("name")
  let name = if not nv.isNil and nv.kind == JString: some(nv.str) else: none(string)
  var sh = ShapeLayer(path: path, fill: fill, stroke: stroke, live: live)
  let id = s.editGet("New Shape Layer", LayerId):
    refreshShape(doc, sh)
    let nm = if name.isSome: name.get else: doc.nextLayerName(defaultShapeName(kind))
    let id = doc.insertAbove(active, newLayer(nm, shapeContent(sh)))
    active = some(id)
    id
  shapeInfo(s, id)

proc shapeEdit(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "shape.edit"
  let id = vecLayerId(s, p)
  let fgc = fgColor(s)
  let nv = p.pget("name")
  let name = if not nv.isNil and nv.kind == JString: some(nv.str) else: none(string)
  withShape(s, id, "Edit Shape", proc(sh: var ShapeLayer; layer: var Layer) =
    let pv = p.pget("path")
    if not pv.isNil:
      sh.path = badOf(cmdId, parsePath(pv))
      sh.live = none(LiveShape)
      sh.psdRaw = nil
    var geom = false
    for k in ["kind", "rect", "radii", "radius", "sides", "starRatio", "from", "to", "weight"]:
      if not p.pget(k).isNil: geom = true
    if geom:
      let kv = p.pget("kind")
      var kind = if not kv.isNil and kv.kind == JString: some(kv.str)
                 elif sh.live.isSome: some(liveKind(sh.live.get))
                 else: none(string)
      if kind.isNone: raise bad(cmdId, "this shape has no live-shape parameters; pass `kind` or `path`")
      # Explicit radii on a plain rectangle keep it a (now rounded) rectangle.
      if kind.get == "rect":
        let rp = radiiParam(p)
        if rp.isSome:
          for v in rp.get:
            if v > 0:
              kind = some("roundedRect")
              break
      let l = liveFromParams(cmdId, kind.get, p, sh.live)
      # Re-entering unchanged values must not regenerate a transformed outline.
      if sh.live != some(l):
        sh.path = livePath(l)
        sh.live = some(l)
        sh.psdRaw = nil
    let m = nums(p, "transform", 6)
    if m.isSome: transformShape(sh, Affine(m: m.get))
    let mv = nums(p, "move", 2)
    if mv.isSome: transformShape(sh, translateAffine(mv.get[0], mv.get[1]))
    let ov = p.pget("op")
    if not ov.isNil and ov.kind == JString:
      let op = opFrom(ov.str)
      if op.isNone: raise bad(cmdId, "unknown op `" & ov.str & "`")
      let sp = p.pget("subpath").asU64
      if sp.isSome:
        if sp.get >= uint64(sh.path.subpaths.len): raise bad(cmdId, "no such subpath")
        sh.path.subpaths[int(sp.get)].op = op.get
      else:
        for i in 1 ..< sh.path.subpaths.len: sh.path.subpaths[i].op = op.get
      sh.live = none(LiveShape)
    let fr = p.pget("fillRule")
    if not fr.isNil and fr.kind == JString:
      sh.path.fillRule = if fr.str.toLowerAscii == "evenodd": frEvenOdd else: frNonZero
    let fv = p.pget("fill")
    if not fv.isNil: sh.fill = badOf(cmdId, parseFill(fv))
    let sv = p.pget("stroke")
    if not sv.isNil: sh.stroke = badOf(cmdId, parseStroke(sv, sh.stroke, fgc))
    if name.isSome: layer.name = name.get
    checkShape(cmdId, sh.path, sh.stroke))
  shapeInfo(s, id)

proc shapeRasterize(s: var Session; p: JsonNode): JsonNode =
  let id = vecLayerId(s, p)
  s.editDo("Rasterize Shape"):
    let snapshot = doc
    let l = doc.layerOrRaise(id)
    if l.content.kind != lkShape: raise other("layer " & $uint64(id) & " is not a shape layer")
    if l.content.shape.cache.isNone: refreshShape(snapshot, l.content.shape)
    let fmt = snapshot.pixelFormat
    var surface = if l.content.shape.cache.isSome: l.content.shape.cache.get else: newSurface(fmt)
    l.content.shape.cache = none(Surface)
    if surface.format != fmt: surface = surface.convert(fmt)
    l.content = rasterContent(surface)
    l.psdBlocks.keepItIf(it.key notin ShapeBlocks)
  %*{"layer": uint64(id)}

# ---------------------------------------------------------------- document paths

proc isWork*(name: Option[string]): bool =
  name.isNone or name.get.len == 0 or name.get.toLowerAscii == "work" or name.get == "Work Path"

proc resolvePath(s: Session; name: Option[string]): Path =
  ## The path a command targets: `"work"`/absent → the work path (falling back to the active
  ## layer's shape path or vector mask), `"layer"` → the active layer's, else a saved path.
  let d = s.activeOrRaise()
  proc layerPath(d: ptr DocState): Option[Path] =
    if d.activeLayer.isNone: return
    let l = d.doc[].findLayer(d.activeLayer.get)
    if l.isNil: return
    if l.content.kind == lkShape: return some(l.content.shape.path)
    if l.vectorMask.isSome: return some(l.vectorMask.get.path)
  if name == some("layer"):
    let lp = layerPath(d)
    if lp.isNone: raise other("active layer has no shape path or vector mask")
    return lp.get
  if isWork(name):
    if d.doc.workPath.isSome: return d.doc.workPath.get
    let lp = layerPath(d)
    if lp.isNone: raise other("no work path")
    return lp.get
  let n = if name.isSome: name.get else: ""
  for q in d.doc.paths:
    if q.name == n: return q.path
  raise other("no path named \"" & n & "\"")

proc pathMut*(doc: var Document; name: string; layer: Option[LayerId]): ptr Path =
  ## The path an edit targets, except a shape layer's: `layer`'s vector mask when given, else the
  ## work path or a saved path.
  if layer.isSome:
    let l = doc.layerOrRaise(layer.get)
    if l.vectorMask.isNone: raise other("active layer has no shape path or vector mask")
    return addr l.vectorMask.get.path
  if isWork(some(name)):
    if doc.workPath.isNone: raise other("no work path")
    return addr doc.workPath.get
  for q in doc.paths.mitems:
    if q.name == name: return addr q.path
  raise other("no path named \"" & name & "\"")

proc knotCount(p: Path): int =
  for s in p.subpaths: result += s.knots.len

proc pathsList(s: Session): JsonNode =
  let d = s.activeOrRaise()
  let doc = d.doc
  var layer = newJNull()
  if d.activeLayer.isSome:
    let l = doc[].findLayer(d.activeLayer.get)
    if not l.isNil:
      if l.content.kind == lkShape:
        layer = %*{"kind": "shape", "layer": uint64(l.id), "name": l.name & " Shape Path", "knots": knotCount(l.content.shape.path)}
      elif l.vectorMask.isSome:
        layer = %*{"kind": "vectorMask", "layer": uint64(l.id), "name": l.name & " Vector Mask", "knots": knotCount(l.vectorMask.get.path)}
  var paths = newJArray()
  for p in doc.paths: paths.add %*{"name": p.name, "knots": knotCount(p.path), "subpaths": p.path.subpaths.len}
  %*{"paths": paths,
     "workPath": (if doc.workPath.isSome: %*{"knots": knotCount(doc.workPath.get), "subpaths": doc.workPath.get.subpaths.len} else: newJNull()),
     "clippingPath": (if doc.clippingPath.isSome: %*{"name": doc.clippingPath.get.name, "flatness": float64(doc.clippingPath.get.flatness)} else: newJNull()),
     "layerPath": layer}

proc optName(p: JsonNode): Option[string] =
  let v = p.pget("name")
  if not v.isNil and v.kind == JString: some(v.str) else: none(string)

proc pathSet(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "path.set"
  let pv = p.pget("path")
  if pv.isNil: raise bad(cmdId, "missing `path`")
  let path = badOf(cmdId, parsePath(pv))
  let name = optName(p)
  var append = none(PathOp)
  let ov = p.pget("op")
  if not ov.isNil and ov.kind == JString:
    append = opFrom(ov.str)
    if append.isNone: raise bad(cmdId, "unknown op `" & ov.str & "`")
  proc merge(old: Option[Path]; new: Path; append: Option[PathOp]): Path =
    if old.isSome and append.isSome:
      result = old.get
      for sp0 in new.subpaths:
        var sp = sp0
        sp.op = append.get
        result.subpaths.add sp
    else: result = new
  s.editDo(if isWork(name): "Work Path" else: "Save Path"):
    if isWork(name): doc.workPath = some(merge(doc.workPath, path, append))
    else:
      let n = if name.isSome: name.get else: ""
      var found = false
      for q in doc.paths.mitems:
        if q.name == n:
          q.path = merge(some(q.path), path, append)
          found = true
          break
      if not found: doc.paths.add NamedPath(name: n, path: path)
  let res = resolvePath(s, name)
  %*{"name": (if name.isSome: name.get else: "work"), "path": pathJson(res)}

proc pathDelete(s: var Session; p: JsonNode): JsonNode =
  let name = optName(p)
  s.editDo("Delete Path"):
    if isWork(name):
      if doc.workPath.isNone: raise other("no work path")
      doc.workPath = none(Path)
      return
    let n = if name.isSome: name.get else: ""
    let before = doc.paths.len
    doc.paths.keepItIf(it.name != n)
    if doc.paths.len == before: raise other("no path named \"" & n & "\"")
    if doc.clippingPath.isSome and doc.clippingPath.get.name == n: doc.clippingPath = none(ClippingPath)
  jnull()

proc clippingPathSet(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "path.clippingPath.set"
  let nv = p.pget("name")
  if nv.isNil or nv.kind != JString or nv.str.len == 0: raise bad(cmdId, "missing saved path `name`")
  let name = nv.str
  var flatness = 0'f32
  let fv = p.pget("flatness")
  if not fv.isNil:
    let v = fv.asF64
    if not (v.isSome and v.get == v.get and abs(v.get) != Inf and v.get >= 0 and v.get <= 100):
      raise bad(cmdId, "`flatness` must be between 0 and 100")
    flatness = float32(v.get)
  s.editDo("Clipping Path"):
    var found = false
    for q in doc.paths:
      if q.name == name: found = true
    if not found: raise other("no saved path named \"" & name & "\"")
    doc.clippingPath = some(ClippingPath(name: name, flatness: flatness))
  %*{"name": name, "flatness": float64(flatness)}

proc clippingPathClear(s: var Session; p: JsonNode): JsonNode =
  s.editDo("Clear Clipping Path"):
    if doc.clippingPath.isNone: raise other("no clipping path")
    doc.clippingPath = none(ClippingPath)
  jnull()

proc transformTarget(s: Session; name: string; layer: Option[LayerId]): Path =
  ## The path `path.transform` edits: `layer`'s shape path or vector mask, else the work path or a
  ## saved path.
  if layer.isNone: return resolvePath(s, some(name))
  let d = s.activeOrRaise()
  let l = d.doc[].findLayer(layer.get)
  if l.isNil: raise noLayer(layer.get)
  if l.content.kind == lkShape: return l.content.shape.path
  if l.vectorMask.isSome: return l.vectorMask.get.path
  raise other("target layer has no shape path or vector mask")

proc coords(p: JsonNode; key: string; n: int): Option[seq[float64]] =
  ## `n` numbers at `key` (`[x0, y0, x1, y1]` or `[[x, y] × 4]`, flattened): finite, magnitude at
  ## most 1000000. None when `key` is absent.
  const cmdId = "path.transform"
  let v = p.pget(key)
  if v.isNil: return
  var flat: seq[JsonNode]
  if v.isArray:
    var allArrays = true
    for e in v.elems:
      if not e.isArray: allArrays = false
    if allArrays:
      for e in v.elems:
        for x in e.elems: flat.add x
    else: flat = v.elems
  var vals: seq[float64]
  for x in flat:
    let f = x.asF64
    if f.isSome and f.get == f.get and abs(f.get) != Inf and abs(f.get) <= 1_000_000: vals.add f.get
  if flat.len != n or vals.len != n:
    raise bad(cmdId, "`" & key & "` must hold " & $n & " finite numbers with magnitude at most 1000000")
  some(vals)

type
  PathMapKind = enum pmAffine, pmProjective, pmWarp
  PathMap = object
    ## How `path.transform` moves the path.
    case kind: PathMapKind
    of pmAffine: a: Affine
    of pmProjective: h: Homography
    of pmWarp: w: Warp

proc pathTransform(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "path.transform"
  let name = p.getStrOr("name", "work")
  let layerId = if name == "layer": some(vecLayerId(s, p)) else: none(LayerId)
  # The box Distort, Perspective and Warp frame: `rect`, else the path's control bounds.
  proc frame(s: Session): array[4, float64] =
    let rc = coords(p, "rect", 4)
    if rc.isSome: result = [rc.get[0], rc.get[1], rc.get[2], rc.get[3]]
    else:
      let b = transformTarget(s, name, layerId).controlBounds
      if b.isNone: raise bad(cmdId, "path has no points to transform")
      result = [b.get[0], b.get[1], b.get[2], b.get[3]]
    if result[2] - result[0] <= 0 or result[3] - result[1] <= 0:
      raise bad(cmdId, "`rect` must have a positive width and height (a straight path needs a `rect`)")
  var map: PathMap
  let mv = p.pget("matrix")
  if not mv.isNil:
    if not mv.isArray or mv.len != 6: raise bad(cmdId, "`matrix` must contain six numbers")
    var m: array[6, float64]
    for i in 0 .. 5:
      let v = mv.elems[i].asF64
      if not (v.isSome and v.get == v.get and abs(v.get) != Inf and abs(v.get) <= 1_000_000):
        raise bad(cmdId, "matrix entries must be finite numbers with magnitude at most 1000000")
      m[i] = v.get
    map = PathMap(kind: pmAffine, a: Affine(m: m))
  else:
    let q = coords(p, "quad", 8)
    if q.isSome:
      # Distort and Perspective: where the frame's corners go, clockwise from top-left.
      let quad = [[q.get[0], q.get[1]], [q.get[2], q.get[3]], [q.get[4], q.get[5]], [q.get[6], q.get[7]]]
      let hh = rectToQuad(frame(s), quad)
      var ok = hh.isSome
      if ok:
        for v in hh.get.m:
          if v != v or abs(v) == Inf: ok = false
        if ok and hh.get.inverse.isNone: ok = false
      if not ok: raise bad(cmdId, "degenerate quad")
      let m = hh.get.m
      if abs(m[6]) < 1e-12 and abs(m[7]) < 1e-12 and abs(m[8]) > 1e-12:
        # A parallelogram is affine: a live shape stays live.
        map = PathMap(kind: pmAffine, a: Affine(m: [m[0] / m[8], m[3] / m[8], m[1] / m[8], m[4] / m[8], m[2] / m[8], m[5] / m[8]]))
      else: map = PathMap(kind: pmProjective, h: hh.get)
    elif not p.pget("warp").isNil or not p.pget("style").isNil or not p.pget("mesh").isNil:
      let w = warpFromParams(cmdId, p, frame(s))
      for v in [w.bend, w.hDistort, w.vDistort, w.bounds[0], w.bounds[1], w.bounds[2], w.bounds[3]]:
        if v != v or abs(v) == Inf or abs(v) > 1_000_000:
          raise bad(cmdId, "warp values must be finite numbers with magnitude at most 1000000")
      map = PathMap(kind: pmWarp, w: w)
    else:
      proc number(p: JsonNode; key: string; default: float64): float64 =
        let v = p.pget(key)
        if v.isNil: return default
        let n = v.asF64
        if not (n.isSome and n.get == n.get and abs(n.get) != Inf and abs(n.get) <= 1_000_000):
          raise bad(cmdId, "`" & key & "` must be a finite number with magnitude at most 1000000")
        n.get
      var any = false
      for key in ["translateX", "translateY", "scaleX", "scaleY", "angle"]:
        if not p.pget(key).isNil: any = true
      if not any:
        raise bad(cmdId, "pass `matrix`, `quad`, a warp (`style`, `mesh` or `warp`) or translate/scale/angle parameters")
      let (tx, ty) = (number(p, "translateX", 0), number(p, "translateY", 0))
      let (sx, sy) = (number(p, "scaleX", 1), number(p, "scaleY", 1))
      let angle = number(p, "angle", 0)
      # An explicit `layer` parameter determines both the transformed path and its pivot.
      let b = transformTarget(s, name, layerId).controlBounds
      if b.isNone: raise bad(cmdId, "path has no points to transform")
      let (x0, y0, x1, y1) = b.get
      let (cx, cy) = ((x0 + x1) * 0.5, (y0 + y1) * 0.5)
      map = PathMap(kind: pmAffine, a: translateAffine(-cx, -cy).then(Affine(m: [sx, 0.0, 0.0, sy, 0.0, 0.0]))
                                         .then(rotateAffine(degToRad(angle))).then(translateAffine(cx + tx, cy + ty)))
  let outp = case map.kind
    of pmAffine: %*{"name": name, "matrix": map.a.m}
    of pmProjective: %*{"name": name, "homography": map.h.m}
    of pmWarp: %*{"name": name, "warp": true}
  if layerId.isSome:
    let d = s.active
    let l = if d.isNil: nil else: d.doc[].findLayer(layerId.get)
    if not l.isNil and l.content.kind == lkShape:
      let m = map
      withShape(s, layerId.get, "Transform Path", proc(shape: var ShapeLayer; l: var Layer) =
        case m.kind
        of pmAffine: transformShape(shape, m.a)
        of pmProjective: mapShape(shape, some(m.h), none(Warp))
        of pmWarp: mapShape(shape, none(Homography), some(m.w))
        shape.psdRaw = nil)
      return outp
  let m = map
  s.editDo("Transform Path"):
    let path = pathMut(doc, name, layerId)
    path[] = case m.kind
      of pmAffine: path[].transform(m.a)
      of pmProjective: mapPath(path[], some(m.h), none(Warp))
      of pmWarp: mapPath(path[], none(Homography), some(m.w))
  outp

proc pathRename(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "path.rename"
  let name = optName(p)
  let tv = p.pget("to")
  if tv.isNil or tv.kind != JString or tv.str.len == 0: raise bad(cmdId, "missing `to`")
  let to = tv.str
  if isWork(some(to)): raise bad(cmdId, "`to` cannot name the work path")
  s.editDo(if isWork(name): "Save Path" else: "Rename Path"):
    for q in doc.paths:
      if q.name == to: raise other("a path named \"" & to & "\" already exists")
    if isWork(name):
      # Photoshop: saving the work path turns it into a named path.
      if doc.workPath.isNone: raise other("no work path")
      let path = doc.workPath.get
      doc.workPath = none(Path)
      doc.paths.add NamedPath(name: to, path: path)
      return
    let n = if name.isSome: name.get else: ""
    var found = false
    for q in doc.paths.mitems:
      if q.name == n:
        q.name = to
        found = true
        break
    if not found: raise other("no path named \"" & n & "\"")
    if doc.clippingPath.isSome and doc.clippingPath.get.name == n: doc.clippingPath.get.name = to
  %*{"name": to}

proc pathToSelection(s: var Session; p: JsonNode): JsonNode =
  let path = resolvePath(s, optName(p))
  let fv = f64p(p, "feather")
  let fth = float32(if fv.isSome: fv.get else: 0.0)
  let aa = p.getBoolOr("antiAlias", true)
  let mode = parseSelectionMode(p.getStrOr("mode", "replace"))
  let area = s.activeOrRaise().doc[].bounds
  var mask = fillRasterizer(path, DefaultTolerance).render(area)
  if not aa:
    for v in mask.mitems: v = if v >= 0.5: 1'f32 else: 0'f32
  if fth > 0: mask = feather(mask, area.width, area.height, fth)
  let selected = s.editGet("Make Selection", bool):
    doc.selection = combine(doc.selection, mask, area, mode)
    doc.selection.isSome
  %*{"selected": selected}

proc selectToWorkPath(s: var Session; p: JsonNode): JsonNode =
  let tv = f64p(p, "tolerance")
  let tol = clamp(if tv.isSome: tv.get else: 2.0, 0.5, 10.0)
  let d = s.activeOrRaise()
  let area = d.doc[].bounds
  if d.doc.selection.isNone: raise other("no selection")
  let mask = maskFromSurface(d.doc.selection, area)
  let path = traceMask(mask, area, 0, tol)
  let knots = knotCount(path)
  let subpaths = path.subpaths.len
  s.editDo("Make Work Path"):
    doc.workPath = some(path)
  %*{"subpaths": subpaths, "knots": knots}

proc paintCoverage(surf: var Surface; area: Rect; cov: seq[float32]; src: array[4, float32]; opacity: float32;
                   mode: BlendMode; lockAlpha: bool) =
  ## Composites `src` × coverage onto a surface over `area` (any pixel format).
  let fmt = surf.format
  let ch = fmt.channels
  var vals = surf.readRegion(area)
  var i = 0
  var k = 0
  while k + ch <= vals.len and i < cov.len:
    let c = cov[i]
    if c > 0:
      let dst = toRgba(fmt, vals.toOpenArray(k, k + ch - 1))
      var outp = compositePs(mode, dst, [src[0], src[1], src[2], src[3] * c], opacity)
      if lockAlpha: outp[3] = dst[3]
      var enc: array[8, float32]
      let m = fromRgbaInto(fmt, outp, enc)
      for j in 0 ..< min(m, ch): vals[k + j] = enc[j]
    inc i
    k += ch
  surf.writeRegion(area, vals)
  surf.prune()

proc pathFill(s: var Session; p: JsonNode): JsonNode =
  let path = resolvePath(s, optName(p))
  let cv = vecColor(p.pget("color"))
  let c = if cv.isSome: cv.get else: fgColor(s)
  let rgbv = c.toRgb
  let src = [rgbv[0], rgbv[1], rgbv[2], c.alpha]
  let ov = f64p(p, "opacity")
  let opacity = float32(clamp((if ov.isSome: ov.get else: 100.0) / 100, 0.0, 1.0))
  let mv = p.pget("mode")
  let bm = if not mv.isNil and mv.kind == JString: blendFromStr(mv.str) else: none(BlendMode)
  let mode = if bm.isSome: bm.get else: bmNormal
  let fv = f64p(p, "feather")
  let fth = float32(if fv.isSome: fv.get else: 0.0)
  let aa = p.getBoolOr("antiAlias", true)
  let id = vecLayerId(s, p)
  s.editDo("Fill Path"):
    var area = doc.bounds
    let r = fillRasterizer(path, DefaultTolerance)
    # The feather reaches 3σ = 3 × radius, plus a pixel.
    let featherPad = int(clamp(int64(asI32(ceil(fth))) * 3 + 1, int64(int32.low), int64(int32.high)))
    let pb = r.pixelBounds
    if pb.isSome: area = pb.get.inflate(featherPad).intersect(area)
    if area.isEmpty: return
    var cov = r.render(area)
    if not aa:
      for v in cov.mitems: v = if v >= 0.5: 1'f32 else: 0'f32
    if fth > 0: cov = feather(cov, area.width, area.height, fth)
    # An active selection limits the fill, soft edges included.
    if doc.selection.isSome:
      let mask = maskFromSurface(doc.selection, area)
      for i in 0 ..< min(cov.len, mask.len): cov[i] *= mask[i]
    let locks = doc.effectiveLocks(id)
    let l = doc.layerOrRaise(id)
    if locks.pixels or locks.all:
      raise other("Could not complete your request because the layer \"" & l.name & "\" is locked")
    let surf = l[].surfaceMut
    if surf.isNil: raise other("Fill Path needs a pixel layer")
    paintCoverage(surf[], area, cov, src, opacity, mode, locks.transparency)
  jnull()

const
  StrokePathTools* = [("backgroundEraser", "paint.backgroundEraser"), ("cloneStamp", "paint.cloneStamp"),
    ("patternStamp", "paint.patternStamp"), ("healingBrush", "paint.healingBrush"), ("spotHealingBrush", "paint.spotHealing"),
    ("historyBrush", "paint.historyBrush"), ("blur", "paint.blur"), ("sharpen", "paint.sharpen"), ("smudge", "paint.smudge"),
    ("dodge", "paint.dodge"), ("burn", "paint.burn"), ("sponge", "paint.sponge"), ("colorReplacement", "paint.colorReplacement"),
    ("mixerBrush", "paint.mixerBrush")]
    ## Stroke Path's brush-based tools besides the Brush, Pencil and Eraser: each paints the path
    ## with its own `paint.*` command.
  MaxPressurePoints = 200_000.0
    ## At most this many points per subpath when Simulate Pressure resamples it.

proc strokePoints(pts: seq[(float64, float64)]; closed, simulate: bool): seq[StrokePoint] =
  ## The stroke points along one flattened subpath; with `simulate` the pressure rises from 0 at
  ## its start to full at the middle and falls back to 0 at its end.
  var line = pts
  if closed and pts.len > 0: line.add pts[0]
  proc full(line: seq[(float64, float64)]): seq[StrokePoint] =
    for (x, y) in line: result.add strokePoint(x, y, 1)
  if not simulate: return full(line)
  proc seg(a, b: (float64, float64)): float64 = hypot(b[0] - a[0], b[1] - a[1])
  var total = 0.0
  for i in 0 ..< line.len - 1: total += seg(line[i], line[i + 1])
  if not (total == total and abs(total) != Inf and total > 0): return full(line)
  let step = max(total / MaxPressurePoints, 1.0)
  proc pressure(d, total: float64): float32 = float32(clamp(sin(d / total * PI), 0.0, 1.0))
  var walked = 0.0
  for i in 0 ..< line.len - 1:
    let (a, b) = (line[i], line[i + 1])
    let len = seg(a, b)
    let n = int(clamp(ceil(len / step), 1.0, MaxPressurePoints))
    for k in 0 ..< n:
      let t = float64(k) / float64(n)
      result.add strokePoint(a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t, pressure(walked + len * t, total))
    walked += len
  if line.len > 0: result.add strokePoint(line[^1][0], line[^1][1], 0)

proc strokeWithCommand(s: var Session; p: JsonNode; cmdName: string; lines: seq[Polyline]; simulate: bool): JsonNode =
  ## Stroke Path with a tool that has its own `paint.*` command, once per subpath, in one step.
  var params = if p.isObject: p.copy else: newJObject()
  for k in ["name", "tool", "simulatePressure", "coalesce"]: params.dropKey(k)
  let journal = s.journal.len
  var dmg: Option[Rect]
  try:
    dmg = compound(s, "Stroke Path", proc(s: var Session): Option[Rect] =
      # The union of what each subpath's command changed; none when one of them didn't say.
      var dmg = some(EmptyRect)
      for pl in lines:
        let pts = strokePoints(pl.pts, pl.closed, simulate)
        if pts.len == 0: continue
        var call = params.copy
        var arr = newJArray()
        for q in pts: arr.add %*[q.x, q.y, float64(q.pressure)]
        call["points"] = arr
        discard s.execute(cmdName, call)
        let st = s.active
        let r = if st.isNil: none(Rect) else: st.lastDamage
        if dmg.isSome and r.isSome:
          let (a, b) = (dmg.get, r.get)
          dmg = some(if a.isEmpty: b elif b.isEmpty: a else: a.union(b))
        else: dmg = none(Rect)
      dmg)
  finally:
    s.journal.setLen(journal)
  let st = s.active
  if not st.isNil: st.lastDamage = dmg
  let d = if dmg.isSome: dmg.get elif st.isNil: EmptyRect else: st.doc[].bounds
  %*{"damage": [d.x0, d.y0, d.width, d.height]}

proc pathStroke(s: var Session; p: JsonNode): JsonNode =
  let path = resolvePath(s, optName(p))
  let tool = p.getStrOr("tool", "brush")
  var simulate = false
  let sv = p.pget("simulatePressure")
  if not sv.isNull:
    if sv.kind != JBool: raise bad("path.stroke", "`simulatePressure` must be true or false")
    simulate = sv.bval
  for (t, cmdName) in StrokePathTools:
    if t == tool: return strokeWithCommand(s, p, cmdName, flattenPath(path, 0.1), simulate)
  var brush = s.tools.brush
  let fg = s.tools.foreground
  let sz = f64p(p, "size")
  if sz.isSome: brush.size = float32(sz.get)
  let hd = f64p(p, "hardness")
  if hd.isSome: brush.hardness = float32(hd.get)
  let op = f64p(p, "opacity")
  if op.isSome: brush.opacity = float32(op.get / 100)
  let cv = vecColor(p.pget("color"))
  if cv.isSome:
    let v = cv.get.toRgb
    brush.color = [v[0], v[1], v[2], cv.get.alpha]
  else: brush.color = fg
  brush.erase = tool == "eraser"
  # The path is the exact geometry: stroke smoothing would cut its corners.
  brush.smoothing.amount = 0
  case tool
  of "pencil": brush.hardness = 1
  of "brush", "eraser": discard
  else:
    var tools = @["brush", "pencil", "eraser"]
    for (t, _) in StrokePathTools: tools.add t
    raise bad("path.stroke", "unknown tool `" & tool & "` (" & tools.join("|") & ")")
  validateBrush(brush, "path.stroke")
  let lines = flattenPath(path, 0.1)
  let id = vecLayerId(s, p)
  let bg = s.tools.background
  let dmg = s.editGet("Stroke Path", Rect):
    let sel = doc.selection
    let locks = doc.effectiveLocks(id)
    let l = doc.layerOrRaise(id)
    if locks.pixels or locks.all:
      raise other("Could not complete your request because the layer \"" & l.name & "\" is locked")
    let lock = locks.transparency
    let surf = l[].surfaceMut
    if surf.isNil: raise other("Stroke Path needs a pixel layer")
    var b = brush
    if b.erase and lock:
      # Locked transparency (e.g. the Background): the Eraser paints the background colour.
      b.erase = false
      b.color = bg
    var dmg = EmptyRect
    for pl in lines:
      let pts = strokePoints(pl.pts, pl.closed, simulate)
      if pts.len == 0: continue
      let r = applyStroke(surf[], Stroke(brush: b, points: pts), sel, lock)
      dmg = if dmg.isEmpty: r else: dmg.union(r)
    dmg
  %*{"damage": [dmg.x0, dmg.y0, dmg.width, dmg.height]}

# ---------------------------------------------------------------- vector masks

proc vectorMaskInfo(s: Session; id: LayerId): JsonNode =
  let d = s.activeOrRaise()
  let l = d.doc[].findLayer(id)
  if l.isNil: raise noLayer(id)
  if l.vectorMask.isSome:
    let v = l.vectorMask.get
    %*{"layer": uint64(id), "path": pathJson(v.path), "enabled": v.enabled, "linked": v.linked,
       "density": float64(v.density * 100), "feather": float64(v.feather)}
  else: %*{"layer": uint64(id), "vectorMask": nil}

proc vectorMaskAdd(s: var Session; p: JsonNode; fromPath: bool): JsonNode =
  const cmdId = "layer.vectorMask.add"
  let id = vecLayerId(s, p)
  var path: Path
  let pv = p.pget("path")
  if not pv.isNil: path = badOf(cmdId, parsePath(pv))
  elif fromPath or not p.pget("name").isNil:
    let name = optName(p)
    # Never fall back to the layer's own path here.
    if isWork(name):
      let d = s.active
      if d.isNil or d.doc.workPath.isNone: raise other("no work path")
      path = d.doc.workPath.get
    else: path = resolvePath(s, name)
  if p.getBoolOr("hide", false): path.inverted = not path.inverted
  s.editDo("Add Vector Mask"):
    let locked = doc.effectiveLocks(id).all
    let l = doc.layerOrRaise(id)
    if l.content.kind == lkShape: raise other("a shape layer's path is already its vector mask")
    if locked: raise other("layer \"" & l.name & "\" is locked")
    l.vectorMask = some(newVectorMask(path))
  vectorMaskInfo(s, id)

proc vectorMaskEdit(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "layer.vectorMask.edit"
  let id = vecLayerId(s, p)
  let pv = p.pget("path")
  let path = if pv.isNil: none(Path) else: some(badOf(cmdId, parsePath(pv)))
  s.editDo("Edit Vector Mask"):
    let l = doc.layerOrRaise(id)
    if l.vectorMask.isNone: raise other("layer has no vector mask")
    template vm: untyped = l.vectorMask.get
    if path.isSome: vm.path = path.get
    let en = p.pget("enabled")
    if not en.isNil and en.kind == JBool: vm.enabled = en.bval
    let lk = p.pget("linked")
    if not lk.isNil and lk.kind == JBool: vm.linked = lk.bval
    let dn = f64p(p, "density")
    if dn.isSome: vm.density = float32(clamp(dn.get / 100, 0.0, 1.0))
    let ft = f64p(p, "feather")
    if ft.isSome: vm.feather = float32(rmax(ft.get, 0.0))
    let iv = p.pget("invert")
    if not iv.isNil and iv.kind == JBool and iv.bval: vm.path.inverted = not vm.path.inverted
  vectorMaskInfo(s, id)

proc vectorMaskDelete(s: var Session; p: JsonNode): JsonNode =
  let id = vecLayerId(s, p)
  s.editDo("Delete Vector Mask"):
    let l = doc.layerOrRaise(id)
    if l.vectorMask.isNone: raise other("layer has no vector mask")
    l.vectorMask = none(VectorMask)
  jnull()

proc vectorMaskRasterize(s: var Session; p: JsonNode): JsonNode =
  ## Layer › Rasterize › Vector Mask: multiplies the vector mask into the pixel mask; when a
  ## feather or the pixel mask's density shapes the rendered edge, the pixel mask becomes exactly
  ## what the compositor showed.
  let id = vecLayerId(s, p)
  s.editDo("Rasterize Vector Mask"):
    let area = doc.bounds
    let depth = doc.depth
    let l = doc.layerOrRaise(id)
    let vmEnabled = l.vectorMask.isSome and l.vectorMask.get.enabled
    let soft = vmEnabled and ((l.vectorMask.isSome and featherSigma(l.vectorMask.get.feather) > 0) or
      (l.mask.isSome and l.mask.get.enabled and (featherSigma(l.mask.get.feather) > 0 or l.mask.get.density < 1)))
    let shown = if soft: combinedMask(l[], area) else: none(Surface)
    if l.vectorMask.isNone: raise other("layer has no vector mask")
    let vm = l.vectorMask.get
    l.vectorMask = none(VectorMask)
    var mask: LayerMask
    if l.mask.isSome: mask = l.mask.get
    else:
      mask = revealAllMask()
      mask.surface = surfaceWithDefault(PixelFormat(mode: cmGrayscale, sample: depth, alpha: false), [1'f32])
    l.mask = none(LayerMask)
    if shown.isSome:
      # Past the canvas the pixel mask keeps its own (density-applied) default.
      let dp = mask.surface.defaultPixel
      let d = if dp.len > 0: dp[0] else: 1'f32
      var surface = surfaceWithDefault(mask.surface.format, [1 - mask.density * (1 - d)])
      surface.writeRegion(area, shown.get.readRegion(area))
      surface.prune()
      mask.surface = surface
      mask.feather = 0
      mask.density = 1
      l.mask = some(mask)
      return
    let vals = vectorMaskValues(vm, area)
    let old = mask.surface.readRegion(area)
    var merged = newSeq[float32](min(old.len, vals.len))
    for i in 0 ..< merged.len: merged[i] = old[i] * vals[i]
    mask.surface.writeRegion(area, merged)
    mask.surface.prune()
    l.mask = some(mask)
  %*{"layer": uint64(id)}

proc toggleVectorMask(s: var Session; p: JsonNode; key: string): JsonNode =
  let id = vecLayerId(s, p)
  let d = s.activeOrRaise()
  let l = d.doc[].findLayer(id)
  if l.isNil: raise noLayer(id)
  if l.vectorMask.isNone: raise other("layer has no vector mask")
  let cur = if key == "enabled": l.vectorMask.get.enabled else: l.vectorMask.get.linked
  let v = p.pget(key)
  let val = if not v.isNil and v.kind == JBool: v.bval else: not cur
  var q = %*{"layer": uint64(id)}
  q[key] = %val
  vectorMaskEdit(s, q)

proc bakeComponents(sh: var ShapeLayer; tol: float64) =
  ## Bakes `sh`'s path operations into plain combined outlines, traced from the exact coverage
  ## at 4× resolution.
  const K = 4.0
  let up = sh.path.transform(scaleAffine(K))
  let r = fillRasterizer(up, DefaultTolerance)
  let pb = r.pixelBounds
  if pb.isNone:
    sh.path.subpaths.setLen(0)
    return
  let b = pb.get.inflate(2)
  if uint64(b.width) * uint64(b.height) > 400_000_000'u64: raise other("shape too large to merge")
  let cov = r.render(b)
  let traced = traceMask(cov, b, 0, tol * K)
  sh.path = traced.transform(scaleAffine(1.0 / K))
  sh.live = none(LiveShape)
  sh.psdRaw = nil

proc combineTargets(s: Session; p: JsonNode; narrow: openArray[string]): Option[seq[LayerId]] =
  ## The shape layers Layer › Combine Shapes merges into one: with none of the `narrow` params and
  ## two or more shape layers selected, those layers bottom-to-top.
  for k in narrow:
    if not p.pget(k).isNil: return
  let d = s.activeOrRaise()
  let sel = d[].selectedOrdered
  var ids: seq[LayerId]
  for id in topLevel(d.doc[], sel):
    let l = d.doc[].findLayer(id)
    if not l.isNil and l.content.kind == lkShape: ids.add id
  if ids.len > 1: some(ids) else: none(seq[LayerId])

proc mergeShapeLayers(s: var Session; ids: seq[LayerId]; op: Option[PathOp]; label: string; bake: Option[float64]): JsonNode =
  ## Combine Shapes with several shape layers selected: merged into the top-most one.
  if ids.len == 0: raise other("Combine Shapes needs two or more shape layers")
  let keep = ids[^1]
  let st = s.active
  if not st.isNil:
    for id in ids:
      let locks = st.doc[].effectiveLocks(id)
      if locks.all or locks.pixels: raise other("a selected shape layer is locked")
  s.editDo(label):
    var path = none(Path)
    for id in ids:
      let l = doc.findLayer(id)
      if l.isNil: raise noLayer(id)
      if l.content.kind != lkShape: raise other("layer " & $uint64(id) & " is not a shape layer")
      if path.isNone:
        path = some(l.content.shape.path)
        continue
      for j, sp0 in l.content.shape.path.subpaths:
        var sp = sp0
        sp.op = if op.isSome: op.get elif j == 0: poCombine else: sp.op
        path.get.subpaths.add sp
    if path.isNone: raise other("Combine Shapes needs two or more shape layers")
    let snapshot = doc
    let l = doc.layerOrRaise(keep)
    if l.content.kind != lkShape: raise other("layer " & $uint64(keep) & " is not a shape layer")
    var merged = l.content.shape
    merged.path = path.get
    merged.live = none(LiveShape)
    merged.psdRaw = nil
    merged.cache = none(Surface)
    if bake.isSome: bakeComponents(merged, bake.get)
    checkShape(label, merged.path, merged.stroke)
    refreshShape(snapshot, merged)
    l.content.shape = merged
    for id in ids:
      if id != keep: discard doc.remove(id)
    active = some(keep)
  reselect(s, @[keep], some(keep))
  shapeInfo(s, keep)

proc mergeComponents(s: var Session; p: JsonNode): JsonNode =
  let tv = f64p(p, "tolerance")
  let tol = clamp(if tv.isSome: tv.get else: 0.1, 0.05, 10.0)
  let ids = combineTargets(s, p, ["layer"])
  if ids.isSome: return mergeShapeLayers(s, ids.get, none(PathOp), "Merge Shape Components", some(tol))
  let id = vecLayerId(s, p)
  withShape(s, id, "Merge Shape Components", proc(sh: var ShapeLayer; l: var Layer) = bakeComponents(sh, tol))
  shapeInfo(s, id)

proc combineShapes(s: var Session; p: JsonNode; op, label: string): JsonNode =
  let ids = combineTargets(s, p, ["layer", "subpath"])
  if ids.isSome:
    let o = opFrom(op)
    if o.isNone: raise other("unknown op `" & op & "`")
    return mergeShapeLayers(s, ids.get, o, label, none(float64))
  shapeEdit(s, p.withField("op", %op))

proc selectionToShape(s: var Session; p: JsonNode): JsonNode =
  let tv = f64p(p, "tolerance")
  let tol = clamp(if tv.isSome: tv.get else: 2.0, 0.5, 10.0)
  let d = s.activeOrRaise()
  let area = d.doc[].bounds
  let mask = maskFromSurface(d.doc.selection, area)
  let path = traceMask(mask, area, 0, tol)
  var q = %*{"kind": "path", "path": pathJson(path)}
  for k in ["fill", "name", "stroke"]:
    let v = p.pget(k)
    if not v.isNil: q[k] = v
  shapeCreate(s, q)

proc vectorSpecs*(): seq[CommandSpec] =
  const stroke = """"stroke":{"width":px,"color":"#rrggbb"|fill,"opacity":0..100,"align":"inside|center|outside","cap":"butt|round|square","join":"miter|round|bevel","miterLimit":n,"dashes":[multiples of width],"dashOffset":n}|null"""
  const fill = """"fill":"#rrggbb"|[r,g,b,a]|{"gradient":{"stops":[[t,"#hex"]],"angle":deg,"scale":%,"style":"linear|radial|angle|reflected|diamond","reverse":bool}}|{"pattern":name}|null (no fill)"""
  @[
    cmd("shape.create", "New Shape Layer", ["Layer", "New"], "",
        """{"kind":"rect|roundedRect|ellipse|polygon|star|line|path"="rect","rect":[x,y,w,h] (rect/roundedRect/ellipse/polygon/star),"radii":[tl,tr,br,bl]|r (roundedRect=10),"sides":3..100=5,"starRatio":0..1 (star=0.5),"from":[x,y],"to":[x,y],"weight":px=1 (line),"path":{…} (kind path),""" &
          fill & "=foreground," & stroke & """=none,"name":str?,"addTo":layerId? + "op":"combine|subtract|intersect|exclude" (append to an existing shape layer)} → shape.info. """ & PathForm,
        hasDocE, shapeCreate),
    cmd("shape.edit", "Edit Shape", [], "",
        """{"layer":id?,"path":{…}? (replaces; drops live shape),"kind":str?,"rect":[x,y,w,h]?,"radii":[tl,tr,br,bl]|r?,"sides":n?,"starRatio":r?,"from":[x,y]?,"to":[x,y]?,"weight":px? (live-shape params regenerate the path),"move":[dx,dy]?,"transform":[a,b,c,d,e,f]?,"op":"combine|…"? + "subpath":index? (default all but the first),"fillRule":"nonzero|evenodd"?,""" &
          fill & "?," & stroke & """? (merged into the current stroke),"name":str?} → shape.info""",
        vecHasLayerE, shapeEdit),
    cmd("shape.info", "Shape Layer Info", [], "", """{"layer":id?} → {layer,name,kind,path,fill,stroke,live,bounds:[x,y,w,h]}""",
        vecHasLayerE, runProc(shapeInfo(s, vecLayerId(s, p))), journal = false),
    cmd("shape.rasterize", "Rasterize Shape", ["Layer", "Rasterize"], "", """{"layer":id?}""", vecHasLayerE, shapeRasterize),
    cmd("path.list", "List Paths", [], "",
        """{} → {paths:[{name,knots,subpaths}],workPath,clippingPath,layerPath (active layer's shape path / vector mask)}""",
        hasDocE, runProc(pathsList(s)), journal = false),
    cmd("path.info", "Path Info", [], "", """{"name":str|"work"|"layer"="work"} → {name,path}""", hasDocE, runProc do:
      let name = optName(p)
      let path = resolvePath(s, name)
      %*{"name": (if name.isSome: name.get else: "work"), "path": pathJson(path)}, journal = false),
    cmd("path.set", "Set Path", [], "",
        """{"name":str|"work"="work","path":{…},"op":"combine|subtract|intersect|exclude"? (append to the existing path with this op instead of replacing)}. """ & PathForm,
        hasDocE, pathSet),
    cmd("path.delete", "Delete Path", [], "", """{"name":str|"work"="work"}""", hasDocE, pathDelete),
    cmd("path.transform", "Free Transform Path", [], "",
        """{"name":str|"work"|"layer"="work","layer":id? (with layer target),"matrix":[a,b,c,d,e,f] | "quad":[[x,y]×4] (Distort/Perspective: where the frame's corners go, clockwise from top-left) | "style"|"mesh"|"warp":… (Warp, as edit.transform.warp) | "translateX":px?,"translateY":px?,"scaleX":factor?,"scaleY":factor?,"angle":degrees?,"rect":[x0,y0,x1,y1]? (the quad's or warp's frame; default = path control bounds)} (numeric transforms pivot on path bounds center)""",
        hasDocE, pathTransform),
    cmd("path.clippingPath.set", "Clipping Path", [], "", """{"name":savedPathName,"flatness":0..100=0} (PSD export clipping path)""",
        hasDocE, clippingPathSet),
    cmd("path.clippingPath.clear", "Clear Clipping Path", [], "", "{}", hasDocE, clippingPathClear),
    cmd("path.style.copyFill", "Copy Fill", [], "", "{} (copies active shape layer fill)", hasShapeFillE, runProc do:
      let (sh, why) = activeShape(s)
      if sh.isNil: raise other(why)
      if sh.fill.isNone: raise other("shape has no fill")
      s.pathFillClipboard = sh.fill
      jnull()),
    cmd("path.style.copyStroke", "Copy Complete Stroke", [], "", "{} (copies active shape layer stroke)", hasShapeStrokeE, runProc do:
      let (sh, why) = activeShape(s)
      if sh.isNil: raise other(why)
      if sh.stroke.isNone: raise other("shape has no stroke")
      s.pathStrokeClipboard = sh.stroke
      jnull()),
    cmd("path.style.pasteFill", "Paste Fill", [], "", """{"layer":id?} (pastes copied fill onto active shape layer)""",
        canPasteShapeFillE, runProc do:
      if s.pathFillClipboard.isNone: raise other("no shape fill copied")
      let fill = s.pathFillClipboard
      let id = vecLayerId(s, p)
      withShape(s, id, "Paste Fill", proc(sh: var ShapeLayer; l: var Layer) = sh.fill = fill)
      shapeInfo(s, id)),
    cmd("path.style.pasteStroke", "Paste Complete Stroke", [], "", """{"layer":id?} (pastes copied stroke onto active shape layer)""",
        canPasteShapeStrokeE, runProc do:
      if s.pathStrokeClipboard.isNone: raise other("no shape stroke copied")
      let stroke = s.pathStrokeClipboard
      let id = vecLayerId(s, p)
      withShape(s, id, "Paste Complete Stroke", proc(sh: var ShapeLayer; l: var Layer) = sh.stroke = stroke)
      shapeInfo(s, id)),
    cmd("path.rename", "Rename Path", [], "", """{"name":str|"work"="work","to":str} (renaming the work path saves it)""", hasDocE, pathRename),
    cmd("path.toSelection", "Make Selection from Path", [], "Cmd+Enter",
        """{"name":str|"work"|"layer"="work","feather":px=0,"antiAlias":bool=true,"mode":"replace|add|subtract|intersect"="replace"}""",
        hasDocE, pathToSelection),
    cmd("select.toWorkPath", "Make Work Path", [], "", """{"tolerance":0.5..10 px=2} → {subpaths,knots}""", vecHasSelectionE, selectToWorkPath),
    cmd("path.fill", "Fill Path", [], "",
        """{"name":str|"work"|"layer"="work","layer":id? (pixel layer, default active),"color":"#rrggbb"=foreground,"opacity":0..100=100,"mode":"normal|multiply|…"="normal","feather":px=0,"antiAlias":bool=true}""",
        vecHasLayerE, pathFill),
    cmd("path.stroke", "Stroke Path", [], "",
        """{"name":str|"work"|"layer"="work","layer":id? (pixel layer),"tool":"brush|pencil|eraser|backgroundEraser|cloneStamp|patternStamp|healingBrush|spotHealingBrush|historyBrush|blur|sharpen|smudge|dodge|burn|sponge|colorReplacement|mixerBrush"="brush","simulatePressure":bool=false (taper both ends when the brush follows pen pressure),"size":0.5..5000 px?,"hardness":0..1?,"opacity":0..100?,"color":"#rrggbb"=foreground} (current brush settings otherwise; tools past the Eraser run their paint.* command along the path and take its params)""",
        vecHasLayerE, pathStroke),
    cmd("layer.vectorMask.add", "Add Vector Mask", ["Layer", "Vector Mask"], "",
        """{"layer":id?,"path":{…}? | "name":str|"work"? (copy a document path),"hide":bool=false (Hide All = inverted)} → vector mask info. Without a path the mask reveals all.""",
        vecHasLayerE, runProc(vectorMaskAdd(s, p, false))),
    cmd("layer.vectorMask.fromPath", "Vector Mask from Current Path", ["Layer", "Vector Mask"], "",
        """{"layer":id?,"name":str|"work"="work"}""", vecHasLayerE, runProc(vectorMaskAdd(s, p, true))),
    cmd("layer.vectorMask.edit", "Edit Vector Mask", [], "",
        """{"layer":id?,"path":{…}?,"enabled":bool?,"linked":bool?,"density":0..100?,"feather":px?,"invert":true?}. """ & PathForm,
        vecHasLayerE, vectorMaskEdit),
    cmd("layer.vectorMask.delete", "Delete Vector Mask", ["Layer", "Vector Mask"], "", """{"layer":id?}""", vecHasLayerE, vectorMaskDelete),
    cmd("layer.rasterize.vectorMask", "Rasterize Vector Mask", [], "", """{"layer":id?} (multiplied into the layer mask)""",
        vecHasLayerE, vectorMaskRasterize),
    cmd("layer.vectorMask.revealAll", "Vector Mask: Reveal All", [], "", """{"layer":id?}""", vecHasLayerE,
        runProc(vectorMaskAdd(s, p.withField("hide", %false), false))),
    cmd("layer.vectorMask.hideAll", "Vector Mask: Hide All", [], "", """{"layer":id?}""", vecHasLayerE,
        runProc(vectorMaskAdd(s, p.withField("hide", %true), false))),
    cmd("layer.vectorMask.currentPath", "Vector Mask: Current Path", [], "", """{"layer":id?,"name":str|"work"="work"}""", vecHasLayerE,
        runProc(vectorMaskAdd(s, p, true))),
    cmd("layer.vectorMask.enabled", "Enable Vector Mask", [], "", """{"layer":id?,"enabled":bool? (default: toggle)}""", vecHasLayerE,
        runProc(toggleVectorMask(s, p, "enabled"))),
    cmd("layer.vectorMask.linked", "Link Vector Mask", [], "", """{"layer":id?,"linked":bool? (default: toggle)}""", vecHasLayerE,
        runProc(toggleVectorMask(s, p, "linked"))),
    cmd("layer.rasterize.shape", "Rasterize Shape", [], "", """{"layer":id?} (no layer: every selected shape layer)""", vecHasLayerE,
        runProc(if not p.pget("layer").isNil: shapeRasterize(s, p) else: rasterizeCmd(s, p, some("shape")))),
    cmd("layer.combineShapes.unite", "Unite Shapes", [], "", CombineParams, vecHasLayerE,
        runProc(combineShapes(s, p, "combine", "Unite Shapes"))),
    cmd("layer.combineShapes.subtractFrontShape", "Subtract Front Shape", [], "", CombineParams, vecHasLayerE,
        runProc(combineShapes(s, p, "subtract", "Subtract Front Shape"))),
    cmd("layer.combineShapes.intersectShapeAreas", "Intersect Shape Areas", [], "", CombineParams, vecHasLayerE,
        runProc(combineShapes(s, p, "intersect", "Intersect Shape Areas"))),
    cmd("layer.combineShapes.excludeOverlappingShapes", "Exclude Overlapping Shapes", [], "", CombineParams, vecHasLayerE,
        runProc(combineShapes(s, p, "exclude", "Exclude Overlapping Shapes"))),
    cmd("layer.combineShapes.mergeShapeComponents", "Merge Shape Components", [], "",
        """{"layer":id?,"tolerance":px=0.1} (bakes the path operations into plain combined outlines, traced from the exact coverage; no layer and several shape layers selected: merges them into the top-most first)""",
        vecHasLayerE, mergeComponents),
    cmd("select.convertToShape", "Convert Selection to Shape", [], "",
        """{"tolerance":px=2,"fill":"#rrggbb"=foreground,"name":str?} → shape.info""", vecHasSelectionE, selectionToShape),
    cmd("layer.vectorMask.info", "Vector Mask Info", [], "", """{"layer":id?} → {layer,path,enabled,linked,density,feather}""",
        vecHasLayerE, runProc(vectorMaskInfo(s, vecLayerId(s, p))), journal = false),
  ]
