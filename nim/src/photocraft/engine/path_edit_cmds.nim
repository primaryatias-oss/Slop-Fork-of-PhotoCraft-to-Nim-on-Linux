## Point-level path edits (port of `path_edit_cmds.rs`): what Photoshop's Direct Selection (A),
## Convert Point and Add / Delete Anchor Point tools do on the canvas. The geometry is
## `vector/edit`; the canvas previews a drag with it and commits the whole drag as one command.
##
## Every command targets a path like `path.transform`: `"name":"work"` (default), a saved path's
## name, or `"layer"` with `"layer":id?` for a shape layer's path (re-rendered; it stops being a live
## shape) or a layer's vector mask. Knots are addressed by `subpath` and `knot` index, as listed by
## `path.info`, and the result is `{name, path}` in the compact JSON form.

import std/[json, options]
import ./core
import ../vector/edit as vedit

const MaxPathCoord = 1_000_000.0 ## coordinates and offsets beyond this are mistakes

proc hasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")

proc finite(v: array[2, float64]): bool =
  for c in v:
    if c != c or abs(c) == Inf or abs(c) > MaxPathCoord: return false
  true

proc offsetParam(cmdId: string; p: JsonNode): array[2, float64] =
  let d = nums(p, "move", 2)
  if d.isNone or not finite(d.get):
    raise bad(cmdId, "`move` must be [dx, dy] with finite numbers of magnitude at most 1000000")
  d.get

proc pointParam(cmdId: string; p: JsonNode; key: string): Option[Point] =
  let v = p.pget(key)
  if v.isNil: return none(Point)
  let q = vecPt(v)
  if q.isNone or not finite([q.get.x, q.get.y]):
    raise bad(cmdId, "`" & key & "` must be [x, y] with finite numbers of magnitude at most 1000000")
  q

proc indexParam(cmdId: string; p: JsonNode; key: string): int =
  let v = p.pget(key).asU64
  if v.isNone or v.get > uint64(high(int)): raise bad(cmdId, "missing `" & key & "` index")
  int(v.get)

proc knotParam(cmdId: string; p: JsonNode): KnotRef =
  let sp = indexParam(cmdId, p, "subpath")
  [sp, indexParam(cmdId, p, "knot")]

type PathEdit = proc(path: var Path)

proc editPath(s: var Session; cmdId: string; p: JsonNode; label: string; f: PathEdit): JsonNode =
  ## Applies `f` to the targeted path in one undoable step labelled `label`.
  let name = p.pget("name").asStr.get("work")
  let layer = (if name == "layer": some(vecLayerId(s, p)) else: none(LayerId))
  var shape = false
  if layer.isSome:
    let st = s.active
    if not st.isNil:
      let l = st.doc[].findLayer(layer.get)
      shape = not l.isNil and l[].content.kind == lkShape
  proc run(path: var Path) =
    try: f(path)
    except ValueError as e: raise bad(cmdId, e.msg)
  var path: JsonNode
  if shape:
    withShape(s, layer.get, label, proc(sh: var ShapeLayer; l: var Layer) =
      run(sh.path)
      sh.live = none(LiveShape)
      sh.psdRaw = nil
      path = pathJson(sh.path))
  else:
    path = s.editWith(label, proc(doc: var Document; active: var Option[LayerId]): JsonNode =
      let target = pathMut(doc, name, layer)
      run(target[])
      pathJson(target[]))
  %*{"name": name, "path": path}

proc moveAnchorsCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "path.moveAnchors"
  let a = p.pget("anchors")
  if not a.isArray or a.len == 0: raise bad(cmdId, "`anchors` must list [subpath, knot] pairs")
  var anchors: seq[KnotRef]
  for e in a.elems:
    let ok = e.isArray and e.len >= 2 and e.elems[0].asU64.isSome and e.elems[1].asU64.isSome and
             e.elems[0].asU64.get <= uint64(high(int)) and e.elems[1].asU64.get <= uint64(high(int))
    if not ok: raise bad(cmdId, "each anchor is [subpath, knot]")
    anchors.add [int(e.elems[0].asU64.get), int(e.elems[1].asU64.get)]
  let d = offsetParam(cmdId, p)
  editPath(s, cmdId, p, "Drag Anchor Points", proc(path: var Path) = vedit.moveAnchors(path, anchors, d))

proc moveHandleCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "path.moveHandle"
  let k = knotParam(cmdId, p)
  let hv = p.pget("handle").asStr
  let (ok, h) = (if hv.isSome: parseHandle(hv.get) else: (false, hIn))
  if not ok: raise bad(cmdId, "`handle` must be \"in\" or \"out\"")
  let to = pointParam(cmdId, p, "to")
  if to.isNone: raise bad(cmdId, "missing `to`")
  let independent = p.pget("independent").asBool.get(false)
  let target = to.get
  editPath(s, cmdId, p, "Drag Direction Point", proc(path: var Path) = vedit.moveHandle(path, k, h, target, independent))

proc bendSegmentCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "path.bendSegment"
  let k = knotParam(cmdId, p)
  var t = 0.5
  let tv = p.pget("t")
  if not tv.isNil:
    let x = tv.asF64
    if x.isNone or not (x.get >= 0 and x.get <= 1): raise bad(cmdId, "`t` must be between 0 and 1")
    t = x.get
  let d = offsetParam(cmdId, p)
  editPath(s, cmdId, p, "Drag Segment", proc(path: var Path) = vedit.bendSegment(path, k, t, d))

proc convertPointCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "path.convertPoint"
  let k = knotParam(cmdId, p)
  let o = pointParam(cmdId, p, "out")
  editPath(s, cmdId, p, "Convert Point", proc(path: var Path) =
    vedit.convertPoint(path, k, o.isSome, (if o.isSome: o.get else: Point())))

proc addAnchorCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "path.addAnchor"
  let k = knotParam(cmdId, p)
  var t = 0.5
  let tv = p.pget("t")
  if not tv.isNil:
    let x = tv.asF64
    if x.isNone or not (x.get > 0 and x.get < 1): raise bad(cmdId, "`t` must be strictly between 0 and 1")
    t = x.get
  var added = none(KnotRef)
  result = editPath(s, cmdId, p, "Add Anchor Point", proc(path: var Path) = added = some(vedit.addAnchor(path, k, t)))
  if added.isSome: result["anchor"] = %*{"subpath": added.get[0], "knot": added.get[1]}

proc deleteAnchorCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "path.deleteAnchor"
  let k = knotParam(cmdId, p)
  var removed = false
  result = editPath(s, cmdId, p, "Delete Anchor Point", proc(path: var Path) = removed = vedit.deleteAnchor(path, k))
  result["subpathRemoved"] = %removed

proc pathEditSpecs*(): seq[CommandSpec] =
  const target = """"name":str|"work"|"layer"="work","layer":id? (with "layer": a shape layer's path or a vector mask)"""
  proc spec(id, label, params: string; run: RunProc): CommandSpec =
    cmd(id, label, [], "", "{" & target & "," & params & "} → {name,path}", hasDocE, run)
  @[spec("path.moveAnchors", "Drag Anchor Points",
         """"anchors":[[subpath,knot],…],"move":[dx,dy] (anchors move with their handles; Direct Selection drag)""",
         runProc(moveAnchorsCmd(s, p))),
    spec("path.moveHandle", "Drag Direction Point",
         """"subpath":i,"knot":j,"handle":"in|out","to":[x,y],"independent":bool=false (a smooth point turns its other handle to stay collinear; independent = ⌥ / Convert Point, makes a corner)""",
         runProc(moveHandleCmd(s, p))),
    spec("path.bendSegment", "Drag Segment",
         """"subpath":i,"knot":j (the segment leaving it),"t":0..1=0.5 (where it was grabbed),"move":[dx,dy] (a straight segment moves with its anchors; a curve reshapes through the dragged point)""",
         runProc(bendSegmentCmd(s, p))),
    spec("path.convertPoint", "Convert Point",
         """"subpath":i,"knot":j,"out":[x,y]? (none: corner with retracted handles; given: smooth with this out handle and its mirror)""",
         runProc(convertPointCmd(s, p))),
    spec("path.addAnchor", "Add Anchor Point",
         """"subpath":i,"knot":j (the segment leaving it),"t":0<t<1=0.5 (where on the segment) (a curve is split there without changing its shape and gets a smooth anchor; a straight segment gets a corner) (the result also has anchor:{subpath,knot})""",
         runProc(addAnchorCmd(s, p))),
    spec("path.deleteAnchor", "Delete Anchor Point",
         """"subpath":i,"knot":j (its neighbours are joined, keeping their handles; a subpath's last anchor removes the subpath; the path's only anchor is refused) (the result also has subpathRemoved:bool)""",
         runProc(deleteAnchorCmd(s, p)))]
