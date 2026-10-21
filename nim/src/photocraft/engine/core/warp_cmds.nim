## Edit › Transform › Warp, Layer › Smart Objects › Warp and split warps (port of
## `warp_cmds.rs`).
##
## A warp is a `Warp` (preset style + bend/distortions, or a custom bicubic Bezier mesh) defined
## over a source box in document space. Pixel layers are resampled once; smart objects store the
## warp in their source space and re-render from the source. The split and grid commands edit a
## mesh: one passed in (`"warp"`, returned edited) or the active smart object's stored warp (a
## history step). Part of the `engine/core` unit: included, not imported.

proc smartRefresh*(doc: var Document; id: LayerId): bool

const MaxWarpSide = 100_000
  ## Largest warp frame side: far above any real layer, far below what would exhaust memory.

proc warpHasLayerE(s: Session): string =
  let d = s.active
  if d.isNil or d.activeLayer.isNone: "no active layer" else: ""

proc activeSmartE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document"
  if d.activeLayer.isSome:
    let l = d.doc[].findLayer(d.activeLayer.get)
    if not l.isNil and l.content.kind == lkSmart: return ""
  "the active layer is not a smart object"

proc rectF(r: Rect): array[4, float64] = [float64(r.x0), float64(r.y0), float64(r.x1), float64(r.y1)]

proc homApply(h: Homography; p: array[2, float64]): array[2, float64] =
  let (x, y) = h.apply(p[0], p[1])
  [x, y]

proc boxOf(c: openArray[array[2, float64]]): array[4, float64] =
  result = [float64.high, float64.high, float64.low, float64.low]
  for p in c: result = [rmin(result[0], p[0]), rmin(result[1], p[1]), rmax(result[2], p[0]), rmax(result[3], p[1])]

proc cornersOf(b: array[4, float64]): array[4, array[2, float64]] =
  [[b[0], b[1]], [b[2], b[1]], [b[2], b[3]], [b[0], b[3]]]

proc warpBounds*(doc: Document; layer: Layer): Rect =
  ## The box a warp of `layer` starts from (Free Transform's frame). For a warped smart object
  ## that is its (unwarped) source box, so re-warping replaces the warp over the same frame.
  if layer.content.kind == lkSmart and layer.content.smart.warp.isSome:
    let w = layer.content.smart.warp.get
    let t = smartPlacement(layer.content.smart)
    var c: array[4, array[2, float64]]
    for i, p in cornersOf(w.bounds): c[i] = homApply(t, p)
    let b = boxOf(c)
    return rect(asI32(round(b[0])), asI32(round(b[1])), asI32(round(b[2])), asI32(round(b[3])))
  transformBounds(doc, layer)

proc smartWarpDocSpace*(layer: Layer): Option[Warp] =
  ## A smart object's stored warp expressed in document space (for editing), if it has one.
  if layer.content.kind != lkSmart or layer.content.smart.warp.isNone: return
  let w = layer.content.smart.warp.get
  let t = smartPlacement(layer.content.smart)
  let mesh = w.toMesh(1, 1).mapPoints(proc(p: P2): P2 = homApply(t, p))
  var c: array[4, array[2, float64]]
  for i, p in cornersOf(w.bounds): c[i] = homApply(t, p)
  some(warpCustom(mesh, boxOf(c)))

# ---------------------------------------------------------------- serde-compatible parsing

proc deF64(v: JsonNode): float64 =
  if v.isNil or v.kind notin {JInt, JFloat}: raise serdeInvalidType(v, "f64")
  v.asF64.get

proc deF64Array(v: JsonNode; n: int): seq[float64] =
  ## serde's `[f64; n]`.
  if not v.isArray: raise serdeInvalidType(v, "an array of length " & $n)
  if v.len < n: raise newException(ValueError, "invalid length " & $v.len & ", expected an array of length " & $n)
  for i in 0 ..< n: result.add deF64(v.elems[i])
  if v.len > n: raise newException(ValueError, "invalid length " & $v.len & ", expected fewer elements in array")

proc meshFromValue(v: JsonNode): BezierMesh =
  ## serde's `BezierMesh` from a JSON value (fields in key order, then the missing ones).
  if not v.isObject: raise serdeInvalidType(v, "struct BezierMesh")
  var seen: set[0 .. 2]
  for k, x in v.sortedPairs:
    case k
    of "us", "vs":
      if not x.isArray: raise serdeInvalidType(x, "a sequence")
      var vals: seq[float64]
      for e in x.elems: vals.add deF64(e)
      if k == "us":
        result.us = vals
        seen.incl 0
      else:
        result.vs = vals
        seen.incl 1
    of "points":
      if not x.isArray: raise serdeInvalidType(x, "a sequence")
      for e in x.elems:
        let q = deF64Array(e, 2)
        result.points.add [q[0], q[1]]
      seen.incl 2
    else: discard
  for i, name in ["us", "vs", "points"]:
    if i notin seen: raise newException(ValueError, "missing field `" & name & "`")

proc warpFromValue(v: JsonNode): Warp =
  ## serde's `Warp` from a JSON value.
  if not v.isObject: raise serdeInvalidType(v, "struct Warp")
  var hasStyle, hasBounds = false
  for k, x in v.sortedPairs:
    case k
    of "style":
      if x.kind == JString:
        let st = parseWarpStyle(x.str)
        var exact = false
        for w in WarpStyle:
          if $w == x.str: exact = true
        if not exact or st.isNone:
          var names: seq[string]
          for w in WarpStyle: names.add "`" & $w & "`"
          raise newException(ValueError, "unknown variant `" & x.str & "`, expected one of " & names.join(", "))
        result.style = st.get
      elif x.kind == JObject: raise serdeInvalidType(x, "string or map")
      else: raise serdeInvalidType(x, "string or map")
      hasStyle = true
    of "bend": result.bend = deF64(x)
    of "hDistort": result.hDistort = deF64(x)
    of "vDistort": result.vDistort = deF64(x)
    of "vertical":
      if x.kind != JBool: raise serdeInvalidType(x, "a boolean")
      result.vertical = x.bval
    of "bounds":
      let b = deF64Array(x, 4)
      result.bounds = [b[0], b[1], b[2], b[3]]
      hasBounds = true
    of "mesh":
      if x.kind == JNull: result.mesh = none(BezierMesh)
      else: result.mesh = some(meshFromValue(x))
    else: discard
  if not hasStyle: raise newException(ValueError, "missing field `style`")
  if not hasBounds: raise newException(ValueError, "missing field `bounds`")

proc warpFromParams*(cmdId: string; p: JsonNode; frame: array[4, float64]): Warp =
  ## The warp params: a full `"warp"` object, or `"style"` (+ `"bend"`, `"hDistort"`,
  ## `"vDistort"`, `"vertical"`), `"mesh"` and `"grid"` over `frame`.
  let wv = p.pget("warp")
  if not wv.isNil:
    var w: Warp
    try: w = warpFromValue(wv)
    except ValueError as e: raise bad(cmdId, "bad `warp`: " & e.msg)
    if w.mesh.isSome and not w.mesh.get.isValid: raise bad(cmdId, "malformed warp mesh")
    return w
  proc num(p: JsonNode; k: string; d: float64): float64 =
    let v = p.pget(k).asF64
    if v.isSome: v.get else: d
  var style: WarpStyle
  let sv = p.pget("style")
  if not sv.isNil and sv.kind == JString:
    let st = parseWarpStyle(sv.str)
    if st.isNone: raise bad(cmdId, "unknown warp style \"" & sv.str & "\"")
    style = st.get
  elif not p.pget("mesh").isNil or not p.pget("grid").isNil: style = wsCustom
  else: raise bad(cmdId, "pass `style` (arc, flag, …, custom), `mesh` or `warp`")
  result = warpPreset(style, num(p, "bend", 50), frame)
  result.hDistort = num(p, "hDistort", 0)
  result.vDistort = num(p, "vDistort", 0)
  result.vertical = p.getBoolOr("vertical", false)
  if style == wsCustom:
    let mv = p.pget("mesh")
    var mesh: BezierMesh
    if not mv.isNil:
      try: mesh = meshFromValue(mv)
      except ValueError as e: raise bad(cmdId, "bad `mesh`: " & e.msg)
      if not mesh.isValid: raise bad(cmdId, "malformed mesh (need (3·cols+1)×(3·rows+1) points and knots 0..1)")
    else:
      let g = p.pget("grid")
      proc at(g: JsonNode; i: int): int =
        let v = (if g.isArray and i < g.len: g.elems[i] else: nil).asU64
        if v.isSome: int(min(v.get, uint64(high(int32)))) else: 1
      mesh = identityMesh(frame, at(g, 0), at(g, 1))
    result.mesh = some(mesh)

# ---------------------------------------------------------------- applying

proc warpLayer(docSel: Option[Surface]; group: Locks; l: var Layer; w: Warp; rc: Rect; interp: Interp) =
  if l.locks.position and l.name == "Background":
    l.locks.position = false
    l.locks.transparency = false
    l.name = "Layer 0"
  let locks = l.locks.union(group)
  if locks.position or locks.all: raise other("layer \"" & l.name & "\" is locked")
  let map: ForwardMap = proc(x, y: float64): (float64, float64) = w.map(x, y)
  case l.content.kind
  of lkGroup:
    for c in l.content.group.children.mitems: warpLayer(none(Surface), locks, c, w, rc, interp)
  of lkText: raise other("Warp needs rasterized type (or use Type › Warp Text)")
  # The path is warped and the shape stays a shape.
  of lkShape: mapShape(l.content.shape, none(Homography), some(w))
  of lkSmart:
    # Store in source space: undo the placement around the warp.
    let inv = smartPlacement(l.content.smart).inverse
    if inv.isNone: raise other("the smart object's transform is degenerate")
    var c: array[4, array[2, float64]]
    for i, p in cornersOf(w.bounds): c[i] = homApply(inv.get, p)
    let sb = boxOf(c)
    var srcWarp = none(Warp)
    case w.style
    of wsNone: discard
    of wsCustom:
      let iv = inv.get
      srcWarp = some(warpCustom(w.toMesh(1, 1).mapPoints(proc(p: P2): P2 = homApply(iv, p)), sb))
    else:
      var x = w
      x.bounds = sb
      x.mesh = none(BezierMesh)
      srcWarp = some(x)
    l.content.smart.warp = if srcWarp.isSome and not srcWarp.get.isIdentity: srcWarp else: none(Warp)
    # Fallback appearance for sources that can't be re-rendered.
    if l.content.smart.cache.isSome:
      l.content.smart.cache = some(warpMeshSurface(l.content.smart.cache.get, rc, map, interp))
    if l.content.smart.filterMask.isSome:
      l.content.smart.filterMask.get.surface = warpMeshGray(l.content.smart.filterMask.get.surface, map, interp)
  else:
    let surf = l.surfaceMut
    if not surf.isNil:
      if docSel.isSome:
        var (lifted, rest) = splitSelected(surf[], docSel.get)
        let moved = warpMeshSurface(lifted, rc, map, interp)
        compositeOver(rest, moved)
        rest.prune()
        surf[] = rest
      else:
        # Content outside the warp box stays where it is.
        let fmt = surf[].format
        let warped = warpMeshSurface(surf[], rc, map, interp)
        var rest = surf[].convert(PixelFormat(mode: fmt.mode, sample: fmt.sample, alpha: true))
        tryClearSurface(rest, rc, none(Surface))
        compositeOver(rest, warped)
        rest.prune()
        surf[] = rest
  if l.mask.isSome and l.mask.get.linked: l.mask.get.surface = warpMeshGray(l.mask.get.surface, map, interp)
  if l.vectorMask.isSome and l.vectorMask.get.linked:
    l.vectorMask.get.path = mapPath(l.vectorMask.get.path, none(Homography), some(w))

proc warpTarget(s: Session; cmdId: string; p: JsonNode): (LayerId, Rect) =
  let st = s.activeOrRaise()
  var id: LayerId
  let lv = p.pget("layer").asU64
  if lv.isSome: id = LayerId(lv.get)
  else:
    if st.activeLayer.isNone: raise other("no active layer")
    id = st.activeLayer.get
  let layer = st.doc[].findLayer(id)
  if layer.isNil: raise noLayer(id)
  var rc: Rect
  let rv = p.pget("rect")
  if rv.isArray and rv.len == 4:
    var v: array[4, int]
    for i in 0 .. 3:
      let f = rv.elems[i].asF64
      v[i] = asI32(round(if f.isSome: f.get else: 0.0))
    rc = rect(v[0], v[1], v[2], v[3])
  else: rc = warpBounds(st.doc[], layer[])
  if rc.isEmpty: raise bad(cmdId, "nothing to warp")
  let (w, h) = (rc.width, rc.height)
  if w > MaxWarpSide or h > MaxWarpSide:
    raise bad(cmdId, "rect too large for warp: " & $w & " x " & $h & " (max " & $MaxWarpSide & " per side)")
  (id, rc)

proc typeWarp(cmdId: string; w: Warp): Option[TextWarp] =
  ## The Warp Text settings a transform warp gives a type layer: a preset style (or none) keeps the
  ## type editable.
  proc cl(v: float64): float32 = (if v == v and abs(v) != Inf: float32(clamp(v, -100.0, 100.0)) else: 0'f32)
  case w.style
  of wsNone: none(TextWarp)
  of wsCustom:
    if w.isIdentity: none(TextWarp)
    else: raise bad(cmdId, "type can only take a warp style (arc, flag, …); a custom mesh needs rasterized type")
  else:
    some(TextWarp(style: w.style.psdName, value: cl(w.bend), horizontalDistortion: cl(w.hDistort),
                  verticalDistortion: cl(w.vDistort), horizontal: not w.vertical))

proc textWarpJson(t: Option[TextWarp]): JsonNode =
  if t.isNone: return newJNull()
  let w = t.get
  %*{"style": w.style, "value": float64(w.value), "horizontal_distortion": float64(w.horizontalDistortion),
     "vertical_distortion": float64(w.verticalDistortion), "horizontal": w.horizontal}

proc warpTypeLayer(s: var Session; cmdId: string; id: LayerId; w: Warp): JsonNode =
  ## Warp on a type layer sets its Warp Text style: one history step, and the type stays editable.
  let tw = typeWarp(cmdId, w)
  let d = s.active
  let l0 = if d.isNil: nil else: d.doc[].findLayer(id)
  if not l0.isNil and l0.content.kind == lkText and l0.content.text.warp == tw:
    return %*{"layer": uint64(id), "changed": false}
  s.editDo("Warp"):
    let group = groupLocks(doc, id)
    let snapshot = doc
    let l = doc.layerOrRaise(id)
    let locks = l.locks.union(group)
    if locks.position or locks.all: raise other("layer \"" & l.name & "\" is locked")
    if l.content.kind != lkText: raise other("not a type layer")
    l.content.text.warp = tw
    refreshText(snapshot, l.content.text)
  %*{"layer": uint64(id), "changed": true, "warp": textWarpJson(tw)}

proc applyWarp(s: var Session; p: JsonNode; cmdId: string): JsonNode =
  let (id, rc) = warpTarget(s, cmdId, p)
  let w = warpFromParams(cmdId, p, rectF(rc))
  let d = s.activeOrRaise()
  let l = d.doc[].findLayer(id)
  let kind = if l.isNil: lkRaster else: l.content.kind
  if kind == lkText: return warpTypeLayer(s, cmdId, id, w)
  if w.isIdentity and kind != lkSmart: return %*{"layer": uint64(id), "changed": false}
  let interp = parseInterp(p.getStrOr("interpolation", "bicubic"))
  s.editDo("Warp"):
    let sel = doc.selection
    let l0 = doc.findLayer(id)
    let isGroup = not l0.isNil and l0[].isGroup
    let group = groupLocks(doc, id)
    let l = doc.layerOrRaise(id)
    warpLayer(if isGroup: none(Surface) else: sel, group, l[], w, rc, interp)
    let snapshot = doc
    let l2 = doc.findLayer(id)
    if not l2.isNil: refreshRendered(snapshot, l2[])
    if doc.selection.isSome:
      let ws = warpMeshGray(doc.selection.get, proc(x, y: float64): (float64, float64) = w.map(x, y), ipBilinear)
      doc.selection = if ws.contentBounds.isEmpty: none(Surface) else: some(ws)
  %*{"layer": uint64(id), "changed": true, "rect": [rc.x0, rc.y0, rc.x1, rc.y1]}

type Split* = enum spCrosswise, spHorizontal, spVertical, spRemove

proc commitDefaultSections(mesh: var BezierMesh) =
  ## The default warp draws rule-of-thirds guides on its single patch: make those real sections
  ## before a split, so the cut divides the grid on screen instead of replacing it.
  discard mesh.splitU(1.0 / 3.0)
  discard mesh.splitU(2.0 / 3.0)
  discard mesh.splitV(1.0 / 3.0)
  discard mesh.splitV(2.0 / 3.0)

proc splitWarp*(w: Warp; at: Option[array[2, float64]]; how: Split): Option[Warp] =
  ## Applies a split (or removes the split nearest `at`) on `w`'s mesh (presets become custom).
  var mesh = w.toMesh(1, 1)
  if how != spRemove and w.style == wsCustom and mesh.us.len == 2 and mesh.vs.len == 2: commitDefaultSections(mesh)
  var (s, t) = if at.isSome: mesh.paramAt(at.get) else: (0.5, 0.5)
  proc mid(k: seq[float64]; v: float64): float64 =
    var i = k.len - 2
    for j in 0 ..< k.len - 1:
      if v <= k[j + 1]:
        i = j
        break
    (k[i] + k[i + 1]) / 2
  if at.isNone: (s, t) = (mid(mesh.us, s), mid(mesh.vs, t))
  var ok = false
  case how
  of spCrosswise:
    let a = mesh.splitU(s)
    let b = mesh.splitV(t)
    ok = a or b
  # A horizontal split line divides rows; a vertical one divides columns.
  of spHorizontal: ok = mesh.splitV(t)
  of spVertical: ok = mesh.splitU(s)
  of spRemove:
    proc near(k: seq[float64]; v: float64): int =
      ## The interior knot nearest `v` (the first of equal distances), or -1.
      result = -1
      var best = 0.0
      for i in 1 ..< k.len - 1:
        let d = abs(k[i] - v)
        if result < 0 or d < best:
          result = i
          best = d
    let (iu, iv) = (near(mesh.us, s), near(mesh.vs, t))
    let du = if iu >= 0: abs(mesh.us[iu] - s) else: float64.high
    let dv = if iv >= 0: abs(mesh.vs[iv] - t) else: float64.high
    if iu >= 0 and du <= dv: ok = mesh.removeSplitU(iu)
    elif iv >= 0: ok = mesh.removeSplitV(iv)
    elif iu >= 0: ok = mesh.removeSplitU(iu)
  if ok: some(warpCustom(mesh, w.bounds)) else: none(Warp)

proc paramRect(p: JsonNode): array[4, float64] =
  let r = p.pget("rect")
  if not r.isArray: return [0.0, 0.0, 1.0, 1.0]
  proc at(r: JsonNode; i: int; d: float64): float64 =
    let v = (if i < r.len: r.elems[i] else: nil).asF64
    if v.isSome: v.get else: d
  [at(r, 0, 0), at(r, 1, 0), at(r, 2, 1), at(r, 3, 1)]

proc inlineWarp(cmdId: string; p: JsonNode): Option[Warp] =
  ## A warp passed inline (`warp`, `mesh` or `style`); none means "use the active smart object".
  if p.pget("warp").isNil and p.pget("mesh").isNil and p.pget("style").isNil: return
  some(warpFromParams(cmdId, p, paramRect(p)))

proc smartWarpOrBox(layer: Layer; sm: SmartObject; corners: bool): Warp =
  ## The smart object's stored warp, else no warp over its pixels' box in source space.
  if sm.warp.isSome: return sm.warp.get
  let b = if layer.hasSurface: layer.surface.contentBounds else: EmptyRect
  let inv = smartPlacement(sm).inverse
  if not corners:
    let ih = if inv.isSome: inv.get else: IdentityHomography
    let c = [homApply(ih, [float64(b.x0), float64(b.y0)]), homApply(ih, [float64(b.x1), float64(b.y1)])]
    return warpNone([min(c[0][0], c[1][0]), min(c[0][1], c[1][1]), max(c[0][0], c[1][0]), max(c[0][1], c[1][1])])
  var c = cornersOf(rectF(b))
  if inv.isSome:
    for q in c.mitems: q = homApply(inv.get, q)
  warpNone(boxOf(c))

proc splitCmd(s: var Session; p: JsonNode; cmdId: string; how: Split): JsonNode =
  var at = none(array[2, float64])
  let av = p.pget("at")
  if av.isArray and av.len >= 2:
    let x = av.elems[0].asF64
    let y = av.elems[1].asF64
    if x.isSome and y.isSome: at = some([x.get, y.get])
  let iw = inlineWarp(cmdId, p)
  if iw.isSome:
    let outp = splitWarp(iw.get, at, how)
    if outp.isNone: raise bad(cmdId, "nothing to split or remove there")
    return %*{"warp": outp.get.toJson}
  # The active smart object's stored warp.
  let st = s.activeOrRaise()
  if st.activeLayer.isNone: raise other("no active layer")
  let id = st.activeLayer.get
  let layer = st.doc[].findLayer(id)
  if layer.isNil: raise noLayer(id)
  if layer.content.kind != lkSmart: raise bad(cmdId, "pass the warp being edited (`warp`), or select a warped smart object")
  let sm = layer.content.smart
  let w = smartWarpOrBox(layer[], sm, false)
  let t = smartPlacement(sm)
  var atSrc = none(array[2, float64])
  if at.isSome:
    let inv = t.inverse
    if inv.isSome: atSrc = some(homApply(inv.get, at.get))
  let outp = splitWarp(w, atSrc, how)
  if outp.isNone: raise bad(cmdId, "nothing to split or remove there")
  let ow = outp.get
  s.editDo(if how == spRemove: "Remove Warp Split" else: "Split Warp"):
    let l = doc.findLayer(id)
    if not l.isNil and l.content.kind == lkSmart: l.content.smart.warp = some(ow)
    discard smartRefresh(doc, id)
  %*{"layer": uint64(id), "warp": ow.toJson}

proc gridSize(cmdId: string; p: JsonNode): int =
  ## Square preset size: `"default"` is 1; numbers outside 1..=64 are rejected, not clamped.
  var v = p.pget("size")
  if v.isNil: v = p.pget("grid")
  if v.isNil: raise bad(cmdId, "pass `size` (1, 3, 4, 5 or \"default\")")
  if v.isArray: raise bad(cmdId, "pass `size` as a number or \"default\", \"3\", \"4\" or \"5\"")
  var n: uint64
  case v.kind
  of JString:
    n = case v.str
      of "default": 1
      of "3", "3x3": 3
      of "4", "4x4": 4
      of "5", "5x5": 5
      else: raise bad(cmdId, "grid size must be default, 3, 4 or 5")
  of JInt, JFloat:
    let u = v.asU64
    if u.isNone: raise bad(cmdId, "grid size must be a whole number from 1 to 64")
    n = u.get
  else: raise bad(cmdId, "grid size must be a number or default, 3, 4 or 5")
  if n < 1 or n > 64: raise bad(cmdId, "grid size must be from 1 to 64")
  int(n)

proc resizeWarp(w: Warp; n: int): Warp =
  ## Even `n`×`n` patches fitted to `w`; the surface stays, knots sit at `i/n`.
  let mesh = w.toMesh(1, 1)
  var knots: seq[float64]
  for i in 0 .. n: knots.add float64(i) / float64(n)
  warpCustom(fit(proc(s, t: float64): P2 = mesh.eval(s, t), knots, knots), w.bounds)

proc gridCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "edit.transform.warpGrid"
  let n = gridSize(cmdId, p)
  let iw = inlineWarp(cmdId, p)
  if iw.isSome: return %*{"warp": resizeWarp(iw.get, n).toJson}
  let st = s.activeOrRaise()
  if st.activeLayer.isNone: raise other("no active layer")
  let id = st.activeLayer.get
  let layer = st.doc[].findLayer(id)
  if layer.isNil: raise noLayer(id)
  if layer.content.kind != lkSmart: raise bad(cmdId, "pass the warp being edited (`warp`), or select a smart object")
  let w = smartWarpOrBox(layer[], layer.content.smart, true)
  let outp = resizeWarp(w, n)
  s.editDo("Warp Grid"):
    let l = doc.findLayer(id)
    if not l.isNil and l.content.kind == lkSmart: l.content.smart.warp = some(outp)
    discard smartRefresh(doc, id)
  %*{"layer": uint64(id), "warp": outp.toJson}

proc warpSpecs*(): seq[CommandSpec] =
  const P = """{"layer":id?,"rect":[x0,y0,x1,y1]? (warp box; default = layer content ∩ selection),"style":"custom|none|arc|arcLower|arcUpper|arch|bulge|shellLower|shellUpper|flag|wave|fish|rise|fisheye|inflate|squeeze|twist","bend":%=50,"hDistort":%,"vDistort":%,"vertical":bool,"mesh":{"us":[0,…,1],"vs":[0,…,1],"points":[[x,y]…]} ((3c+1)×(3r+1) control points, row-major, document px),"grid":[cols,rows]?,"warp":{full warp object}?,"interpolation":"bicubic|bilinear|nearest"} — on a type layer a style (or none) sets its Warp Text and the type stays editable"""
  const S = """{"warp":{…}? (the warp being edited; returned split),"rect":[x0,y0,x1,y1]?,"at":[x,y]? (document point; default = middle of the patch)} — without `warp`, edits the active smart object's warp. A 1×1 custom mesh is divided at the thirds first (the guides drawn on the default grid), so splitWarpCrosswise on an identity custom warp returns a 4×4 mesh (knots at 0, 1/3, 1/2, 2/3 and 1), not a 2×2. A preset is fitted as one patch and split without that step."""
  const G = """{"warp":{…}? (the warp being edited; returned with the new mesh),"rect":[x0,y0,x1,y1]?,"size":n|"default"|"3"|"4"|"5" (square patches, 1..=64; 1 and "default" are the single-patch grid)} — without `warp`/`style`/`mesh`, edits the active smart object's warp. The surface is kept and the new lines are real splits at i/n."""
  @[
    cmd("edit.transform.warp", "Warp", ["Edit", "Transform"], "", P, warpHasLayerE, runProc(applyWarp(s, p, "edit.transform.warp"))),
    cmd("layer.smartObjects.warp", "Warp", ["Layer", "Smart Objects"], "", P, activeSmartE,
        runProc(applyWarp(s, p, "layer.smartObjects.warp"))),
    cmd("edit.transform.splitWarpCrosswise", "Split Warp Crosswise", ["Edit", "Transform"], "", S, warpHasLayerE,
        runProc(splitCmd(s, p, "edit.transform.splitWarpCrosswise", spCrosswise))),
    cmd("edit.transform.splitWarpHorizontally", "Split Warp Horizontally", ["Edit", "Transform"], "", S, warpHasLayerE,
        runProc(splitCmd(s, p, "edit.transform.splitWarpHorizontally", spHorizontal))),
    cmd("edit.transform.splitWarpVertically", "Split Warp Vertically", ["Edit", "Transform"], "", S, warpHasLayerE,
        runProc(splitCmd(s, p, "edit.transform.splitWarpVertically", spVertical))),
    cmd("edit.transform.removeWarpSplit", "Remove Warp Split", ["Edit", "Transform"], "", S, warpHasLayerE,
        runProc(splitCmd(s, p, "edit.transform.removeWarpSplit", spRemove))),
    cmd("edit.transform.warpGrid", "Warp Grid", [], "", G, warpHasLayerE, gridCmd),
  ]
