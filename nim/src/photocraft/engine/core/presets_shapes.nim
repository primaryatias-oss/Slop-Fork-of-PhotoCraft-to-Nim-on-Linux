## Window › Shapes and the Custom Shape tool (`presets/shapes.rs`): vector shape presets in groups,
## placed as shape layers through `shape.create`. Built-in shapes were drawn from scratch for
## PhotoCraft in a 100 × 100 box with a small path language (`parseShapePath`): `M x y`, `L x y…`,
## `C x1 y1 x2 y2 x y…`, `Q x1 y1 x y…`, `Z`, plus `O cx cy r` (a circle) and `!` before `M`/`O`
## to subtract that subpath. Shapes defined with Edit › Define Custom Shape appear in a trailing
## "Custom Shapes" group. Part of the `engine/core` unit: included, not imported.

const CustomShapesGroup* = "Custom Shapes"

proc circleSubpath(cx, cy, r: float64): Subpath =
  ## Four cubic quarter arcs (κ = 0.5523).
  let k = r * 0.552_284_75
  proc knot(x, y, ix, iy, ox, oy: float64): Knot = smoothKnot(Point(x: x, y: y), Point(x: ix, y: iy), Point(x: ox, y: oy))
  Subpath(closed: true, op: poCombine, knots: @[
    knot(cx, cy - r, cx - k, cy - r, cx + k, cy - r),
    knot(cx + r, cy, cx + r, cy - k, cx + r, cy + k),
    knot(cx, cy + r, cx + k, cy + r, cx - k, cy + r),
    knot(cx - r, cy, cx - r, cy + k, cx - r, cy - k)])

proc parseShapePath*(src: string): Path =
  ## Parse the path language described in the module docs; raises `ValueError` with the reason.
  let toks = unicode.splitWhitespace(src.replace(",", " ").replace("!", " ! ")).filterIt(it.len > 0)
  var i = 0
  proc num(): float64 =
    if i >= toks.len: raise newException(ValueError, "unexpected end")
    let v = rustParseF64(toks[i])
    if v.isNone: raise newException(ValueError, "expected a number at `" & toks[i] & "`")
    inc i
    v.get
  proc isNum(k: int): bool = k < toks.len and rustParseF64(toks[k]).isSome
  var subs: seq[Subpath]
  var cur = none(Subpath)
  var op = poCombine
  proc finish(closed: bool) =
    if cur.isSome:
      var s = cur.get
      cur = none(Subpath)
      s.closed = closed
      if closed and s.knots.len > 1:
        let (f, l) = (s.knots[0].anchor, s.knots[^1].anchor)
        if abs(f.x - l.x) < 1e-9 and abs(f.y - l.y) < 1e-9:
          let last = s.knots.pop()
          s.knots[0].inCtrl = last.inCtrl
      if s.knots.len > 0: subs.add s
  while i < toks.len:
    let t = toks[i]
    inc i
    case t
    of "!": op = poSubtract
    of "M":
      finish(true)
      let x = num()
      let y = num()
      cur = some(Subpath(closed: true, knots: @[cornerKnot(x, y)], op: op))
      op = poCombine
    of "L":
      if cur.isNone: raise newException(ValueError, "L before M")
      while isNum(i):
        let x = num()
        let y = num()
        cur.get.knots.add cornerKnot(x, y)
    of "C", "Q":
      if cur.isNone: raise newException(ValueError, "curve before M")
      while isNum(i):
        if cur.get.knots.len == 0: raise newException(ValueError, "curve before M")
        let last = cur.get.knots[^1].anchor
        var c1, c2, p: Point
        if t == "C":
          c1 = Point(x: num(), y: num())
          c2 = Point(x: num(), y: num())
          p = Point(x: num(), y: num())
        else:
          let q = Point(x: num(), y: num())
          p = Point(x: num(), y: num())
          c1 = Point(x: last.x + (q.x - last.x) * 2.0 / 3.0, y: last.y + (q.y - last.y) * 2.0 / 3.0)
          c2 = Point(x: p.x + (q.x - p.x) * 2.0 / 3.0, y: p.y + (q.y - p.y) * 2.0 / 3.0)
        cur.get.knots[^1].outCtrl = c1
        cur.get.knots.add Knot(anchor: p, inCtrl: c2, outCtrl: p, smooth: false)
    of "Z": finish(true)
    of "O":
      finish(true)
      let cx = num()
      let cy = num()
      let r = num()
      var c = circleSubpath(cx, cy, r)
      c.op = op
      op = poCombine
      subs.add c
    else: raise newException(ValueError, "unknown command `" & t & "`")
  finish(true)
  if subs.len == 0: raise newException(ValueError, "empty path")
  newPath(subs)

proc starSrc(n: int; ratio, rot: float64): string =
  ## Star / burst polygon with `n` points in the 100 box (`ratio` = inner / outer radius).
  for k in 0 ..< 2 * n:
    let r = (if k mod 2 == 0: 50.0 else: 50.0 * ratio)
    let a = rot + PI * float64(k) / float64(n) - PI / 2
    result.add (if k == 0: "M" else: "L") & " " & fixed(50.0 + r * cos(a), 3) & " " & fixed(50.0 + r * sin(a), 3) & " "
  result.add "Z"

proc sunSrc(): string =
  result = "O 50 50 22 "
  for k in 0 ..< 12:
    let a = TAU * float64(k) / 12.0
    proc p(r, da: float64): (float64, float64) = (50.0 + r * cos(a + da), 50.0 + r * sin(a + da))
    let (b1, b2, tip) = (p(28.0, -0.12), p(28.0, 0.12), p(50.0, 0.0))
    result.add "M " & fixed(b1[0], 3) & " " & fixed(b1[1], 3) & " L " & fixed(tip[0], 3) & " " & fixed(tip[1], 3) &
      " L " & fixed(b2[0], 3) & " " & fixed(b2[1], 3) & " Z "

proc flowerSrc(): string =
  for k in 0 ..< 6:
    let a = TAU * float64(k) / 6.0 - PI / 2
    result.add "O " & fixed(50.0 + 30.0 * cos(a), 3) & " " & fixed(50.0 + 30.0 * sin(a), 3) & " 18 "
  result.add "O 50 50 20 ! O 50 50 9"

proc shoutSrc(): string =
  ## A jagged burst stretched into a bubble, with a tail.
  let n = 14
  for k in 0 ..< 2 * n:
    let r = (if k mod 2 == 0: 1.0 else: 0.78)
    let a = PI * float64(k) / float64(n)
    result.add (if k == 0: "M" else: "L") & " " & fixed(50.0 + 50.0 * r * cos(a), 3) & " " & fixed(40.0 + 40.0 * r * sin(a), 3) & " "
  result.add "Z M 30 70 L 14 100 L 46 74 Z"

proc builtinShapes*(): seq[PresetGroup[ShapePreset]] =
  ## Built-in groups (our own drawings).
  proc mk(list: openArray[(string, string)]): seq[ShapePreset] =
    for (n, src) in list:
      try: result.add ShapePreset(name: n, path: parseShapePath(src))
      except ValueError: discard
  @[
    newGroup("Symbols", mk([
      ("Heart", "M 50 92 C 20 70 0 52 0 30 C 0 12 14 0 28 0 C 40 0 47 8 50 16 C 53 8 60 0 72 0 C 86 0 100 12 100 30 C 100 52 80 70 50 92 Z"),
      ("Star", starSrc(5, 0.4, 0.0)),
      ("Six-Point Star", starSrc(6, 0.55, 0.0)),
      ("Burst", starSrc(16, 0.72, 0.0)),
      ("Check Mark", "M 5 55 L 20 40 L 38 58 L 82 12 L 97 27 L 38 88 Z"),
      ("Cross", "M 20 5 L 50 35 L 80 5 L 95 20 L 65 50 L 95 80 L 80 95 L 50 65 L 20 95 L 5 80 L 35 50 L 5 20 Z"),
      ("Plus", "M 38 0 L 62 0 L 62 38 L 100 38 L 100 62 L 62 62 L 62 100 L 38 100 L 38 62 L 0 62 L 0 38 L 38 38 Z"),
      ("Ring", "O 50 50 50 ! O 50 50 32"),
      ("Lightning", "M 58 0 L 18 56 L 44 56 L 34 100 L 82 38 L 55 38 L 70 0 Z"),
      ("Diamond", "M 50 0 L 90 50 L 50 100 L 10 50 Z")])),
    newGroup("Arrows", mk([
      ("Arrow Right", "M 0 35 L 55 35 L 55 15 L 100 50 L 55 85 L 55 65 L 0 65 Z"),
      ("Arrow Left", "M 100 35 L 45 35 L 45 15 L 0 50 L 45 85 L 45 65 L 100 65 Z"),
      ("Arrow Up", "M 35 100 L 35 45 L 15 45 L 50 0 L 85 45 L 65 45 L 65 100 Z"),
      ("Arrow Down", "M 35 0 L 35 55 L 15 55 L 50 100 L 85 55 L 65 55 L 65 0 Z"),
      ("Double Arrow", "M 0 50 L 25 20 L 25 38 L 75 38 L 75 20 L 100 50 L 75 80 L 75 62 L 25 62 L 25 80 Z"),
      ("Chevron", "M 10 0 L 45 0 L 95 50 L 45 100 L 10 100 L 60 50 Z"),
      ("Curved Arrow", "M 8 92 C 8 45 35 22 68 22 L 68 4 L 98 34 L 68 64 L 68 44 C 48 44 30 58 30 92 Z")])),
    newGroup("Speech Bubbles", mk([
      ("Speech Bubble", "M 15 5 L 85 5 C 93 5 100 12 100 20 L 100 60 C 100 68 93 75 85 75 L 45 75 L 20 98 L 25 75 L 15 75 C 7 75 0 68 0 60 L 0 20 C 0 12 7 5 15 5 Z"),
      ("Round Bubble", "M 50 5 C 78 5 100 22 100 42 C 100 62 78 79 50 79 C 44 79 38 78 33 77 L 12 95 L 18 72 C 7 65 0 54 0 42 C 0 22 22 5 50 5 Z"),
      ("Thought Bubble", "M 50 5 C 78 5 100 20 100 38 C 100 56 78 71 50 71 C 22 71 0 56 0 38 C 0 20 22 5 50 5 Z O 22 82 8 O 9 95 5"),
      ("Shout Bubble", shoutSrc())])),
    newGroup("Nature", mk([
      ("Leaf", "M 50 0 C 80 20 95 50 80 75 C 72 88 60 92 53 92 L 53 100 L 47 100 L 47 92 C 40 92 28 88 20 75 C 5 50 20 20 50 0 Z ! M 48.5 22 L 51.5 22 L 51.5 88 L 48.5 88 Z"),
      ("Sun", sunSrc()),
      ("Crescent Moon", "O 45 50 45 ! O 66 40 38"),
      ("Cloud", "M 25 80 C 10 80 0 70 0 58 C 0 46 10 37 22 37 C 24 20 38 10 54 10 C 70 10 82 20 85 34 C 95 35 100 45 100 57 C 100 70 90 80 78 80 Z"),
      ("Raindrop", "M 50 0 C 60 25 85 45 85 68 C 85 87 69 100 50 100 C 31 100 15 87 15 68 C 15 45 40 25 50 0 Z"),
      ("Flower", flowerSrc()),
      ("Tree", "M 50 0 L 85 45 L 68 45 L 92 75 L 56 75 L 56 100 L 44 100 L 44 75 L 8 75 L 32 45 L 15 45 Z")])),
    newGroup("Animals", mk([
      ("Fish", "M 0 50 C 15 25 45 15 70 32 L 96 12 L 88 50 L 96 88 L 70 68 C 45 85 15 75 0 50 Z ! O 20 45 4"),
      ("Cat", "M 15 10 L 35 30 C 45 27 55 27 65 30 L 85 10 L 88 50 C 92 75 72 95 50 95 C 28 95 8 75 12 50 Z ! O 36 58 6 ! O 64 58 6"),
      ("Bird", "M 0 40 C 15 30 35 32 50 50 C 65 32 85 30 100 40 C 85 40 65 48 50 70 C 35 48 15 40 0 40 Z"),
      ("Butterfly", "M 50 30 C 40 5 5 0 5 25 C 5 45 30 50 45 50 C 25 55 10 75 25 90 C 38 100 48 80 50 65 C 52 80 62 100 75 90 C 90 75 75 55 55 50 C 70 50 95 45 95 25 C 95 0 60 5 50 30 Z"),
      ("Paw Print", "M 50 50 C 68 50 82 66 82 80 C 82 92 70 96 60 94 C 55 93 52 91 50 91 C 48 91 45 93 40 94 C 30 96 18 92 18 80 C 18 66 32 50 50 50 Z O 18 42 10 O 37 22 11 O 63 22 11 O 82 42 10"),
      ("Rabbit", "O 50 72 26 M 34 52 C 22 30 24 2 34 2 C 44 2 46 30 44 50 Z M 66 52 C 78 30 76 2 66 2 C 56 2 54 30 56 50 Z")])),
  ]

proc allShapeGroups*(s: Session): seq[PresetGroup[ShapePreset]] =
  ## Every shape the panel shows: the preset groups plus Define Custom Shape results.
  result = s.presets.shapes
  var custom: seq[ShapePreset]
  for c in s.editState.customShapes: custom.add ShapePreset(name: c.name, path: c.path)
  if custom.len > 0: result.add newGroup(CustomShapesGroup, custom)

proc lookupShape(s: Session; name: string; group: Option[string]; cmdId: string): ShapePreset =
  let groups = allShapeGroups(s)
  let at = findPreset(groups, name, group)
  if at.isNone: raise bad(cmdId, "no shape \"" & name & "\" (see shape.presets.list)")
  groups[at.get[0]].items[at.get[1]]

proc fitPath*(path: Path; rect: array[4, float64]; keep: bool): Path =
  ## Map `path` (its control bounds) onto `rect` = [x, y, w, h]; `keep` preserves the aspect ratio
  ## (centred in the rect).
  let cb = path.controlBounds
  if cb.isNone: return path
  let (x0, y0, x1, y1) = cb.get
  let bw = max(x1 - x0, 1e-9)
  let bh = max(y1 - y0, 1e-9)
  var sx = rect[2] / bw
  var sy = rect[3] / bh
  var ox = rect[0]
  var oy = rect[1]
  if keep:
    let k = min(sx, sy)
    ox += (rect[2] - bw * k) / 2.0
    oy += (rect[3] - bh * k) / 2.0
    sx = k
    sy = k
  path.transform(Affine(m: [sx, 0.0, 0.0, sy, ox - x0 * sx, oy - y0 * sy]))

proc shapeThumbnail*(path: Path; size: uint32): seq[float32] =
  ## Coverage (0..1) of `path` fitted into a `size`² swatch with a small margin.
  let size = clamp(size, 4'u32, 512'u32)
  let m = max(float64(size) * 0.08, 1.0)
  let fitted = fitPath(path, [m, m, float64(size) - 2.0 * m, float64(size) - 2.0 * m], true)
  pathCoverage(fitted, rect(0, 0, int(size), int(size)))

proc currentPath*(doc: Document; active: Option[LayerId]; spec: Option[string]): Option[Path] =
  ## The path Define Custom Shape (and New Shape Preset) uses (`edit_menu_cmds::current_path`).
  proc ok(p: Path): bool = p.subpaths.len > 0
  if spec.isSome:
    if spec.get == "work":
      if doc.workPath.isSome and ok(doc.workPath.get): return doc.workPath
      return
    for n in doc.paths:
      if n.name == spec.get:
        return (if ok(n.path): some(n.path) else: none(Path))
    return
  if doc.workPath.isSome and ok(doc.workPath.get): return doc.workPath
  if active.isSome:
    let l = doc.findLayer(active.get)
    if not l.isNil:
      if l[].content.kind == lkShape and ok(l[].content.shape.path): return some(l[].content.shape.path)
      if l[].vectorMask.isSome and ok(l[].vectorMask.get.path): return some(l[].vectorMask.get.path)
  if doc.paths.len > 0 and ok(doc.paths[^1].path): return some(doc.paths[^1].path)

proc shapePresetsList(s: var Session): JsonNode =
  var groups = newJArray()
  for g in allShapeGroups(s):
    var items = newJArray()
    for it in g.items: items.add %*{"name": it.name, "subpaths": it.path.subpaths.len}
    groups.add %*{"name": g.name, "shapes": items}
  %*{"groups": groups}

proc shapePresetsPlace(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "shape.presets.place"
  let name = reqStr(p, "preset", cmdId)
  let shape = lookupShape(s, name, strParam(p, "group"), cmdId)
  let d = s.active
  if d.isNil: raise noDocument()
  let w = float64(d.doc.size.width)
  let h = float64(d.doc.size.height)
  let keep = p.getBoolOr("keepAspect", true)
  var r: array[4, float64]
  let a = p.pget("rect")
  if a.isArray and a.len == 4:
    var v: array[4, float64]
    for i in 0 ..< 4: v[i] = a.elems[i].asF64.get(0.0)
    # Negative sizes (dragging up/left) flip into a positive rect.
    let (x, wd) = (if v[2] < 0: (v[0] + v[2], -v[2]) else: (v[0], v[2]))
    let (y, hd) = (if v[3] < 0: (v[1] + v[3], -v[3]) else: (v[1], v[3]))
    if wd < 1.0 or hd < 1.0: raise bad(cmdId, "the shape rect is empty")
    r = [x, y, wd, hd]
  else:
    let side = min(w, h) * 0.5
    r = [(w - side) / 2.0, (h - side) / 2.0, side, side]
  let path = fitPath(shape.path, r, keep)
  var q = %*{"kind": "path", "path": pathJson(path), "name": strParam(p, "name").get(shape.name)}
  for k in ["fill", "stroke", "addTo", "op"]:
    let v = p.pget(k)
    if not v.isNil: q[k] = v.copy
  result = callCommand(s, "shape.create", q)
  result["preset"] = %shape.name

proc shapePresetsNew(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "shape.presets.new"
  var path: Path
  let v = p.pget("path")
  if v.isObject:
    try: path = parsePath(v)
    except EngineError: raise
    except CatchableError as e: raise bad(cmdId, e.msg)
  elif not v.isNil and v.kind == JString and unicode.strip(v.str, trailing = false).len > 0 and
      unicode.strip(v.str, trailing = false)[0] in {'M', 'O', '!'}:
    try: path = parseShapePath(v.str)
    except ValueError as e: raise bad(cmdId, e.msg)
  else:
    let d = s.active
    if d.isNil: raise bad(cmdId, "no document (pass `path`)")
    let cp = currentPath(d.doc[], d.activeLayer, v.asStr)
    if cp.isNone: raise bad(cmdId, "select a path or shape layer first")
    path = cp.get
  if path.isEmpty: raise bad(cmdId, "the path is empty")
  # Normalise into the 100 box so thumbnails and placing don't depend on where it was drawn.
  path = fitPath(path, [0.0, 0.0, 100.0, 100.0], true)
  let name = uniqueName(s.presets.shapes, strParam(p, "name").get("Shape"))
  let gi = groupIndex(s.presets.shapes, strParam(p, "group"))
  let group = s.presets.shapes[gi].name
  s.presets.shapes[gi].items.add ShapePreset(name: name, path: path)
  s.presetsChanged()
  %*{"name": name, "group": group}

proc shapePresetsEdit(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "shape.presets.edit"
  let action = reqStr(p, "action", cmdId)
  # Define Custom Shape results live in the edit state; rename/delete them there.
  proc custom(s: Session; n: string): bool =
    let g = strParam(p, "group")
    (g.isNone or g.get == CustomShapesGroup) and findPreset(s.presets.shapes, n, none(string)).isNone and
      s.editState.customShapes.anyIt(it.name == n)
  if action == "rename" and custom(s, reqStr(p, "preset", cmdId)):
    let n = reqStr(p, "preset", cmdId)
    let to = reqStr(p, "name", cmdId)
    for c in s.editState.customShapes.mitems:
      if c.name == n:
        c.name = to
        break
    result = %*{"name": to}
  elif action == "delete" and p.pget("preset").asStr.isSome and custom(s, p.pget("preset").str):
    let n = reqStr(p, "preset", cmdId)
    s.editState.customShapes.keepItIf(it.name != n)
    result = %*{"deleted": 1}
  else: result = editGroups(s.presets.shapes, action, p, cmdId)
  s.presetsChanged()

proc shapePresetsReset(s: var Session; p: JsonNode): JsonNode =
  var b = builtinShapes()
  if p.getBoolOr("append", false):
    for g in b.mitems: g.name = uniqueGroup(s.presets.shapes, g.name)
    s.presets.shapes.add b
  else: s.presets.shapes = b
  s.presetsChanged()
  %*{"groups": s.presets.shapes.len}

proc shapePresetSpecs*(): seq[CommandSpec] =
  @[
    cmd("shape.presets.list", "Shape Presets", [], "", "{} → {groups:[{name,shapes:[{name,subpaths}]}]}", presetAlwaysE,
        runProc(shapePresetsList(s)), journal = false),
    cmd("shape.presets.place", "Place Custom Shape", [], "",
        "{\"preset\":name,\"group\":name?,\"rect\":[x,y,w,h]? (default: centred, half the canvas),\"keepAspect\":bool=true,\"fill\":…?=foreground,\"stroke\":…?,\"name\":str?,\"addTo\":layerId?,\"op\":\"combine|subtract|intersect|exclude\"?} → shape.info (Custom Shape tool / Shapes panel; fill and stroke as in shape.create)",
        presetHasDocE, runProc(shapePresetsPlace(s, p))),
    cmd("shape.presets.new", "New Shape Preset", [], "",
        "{\"name\":str=\"Shape\",\"group\":name?,\"path\":{…path}|\"M x y L …\" (preset path language)|\"work\"|saved path name? (default: work path, active shape layer or vector mask)}",
        presetAlwaysE, runProc(shapePresetsNew(s, p))),
    cmd("shape.presets.edit", "Edit Shape Presets", [], "", GroupEditParams, presetAlwaysE, runProc(shapePresetsEdit(s, p))),
    cmd("shape.presets.reset", "Restore Default Shapes", [], "", "{\"append\":bool=false}", presetAlwaysE,
        runProc(shapePresetsReset(s, p))),
  ]
