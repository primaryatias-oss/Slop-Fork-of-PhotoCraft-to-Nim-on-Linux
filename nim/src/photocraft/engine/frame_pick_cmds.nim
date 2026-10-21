## Frames (port of `frame_cmds.rs`: Layer › New › Frame from Layers and the Frame tool's
## `layer.new.frame`) and the Move tool's Auto-Select (`pick_cmds.rs`: `layer.pickAt`,
## `layer.selectInRect`).
##
## A frame is a group clipped to a rectangle or ellipse by a layer mask, the same visual result
## Photoshop's Frame tool produces. Auto-Select picks the topmost visible, not fully locked layer
## with pixels at a canvas point; like Photoshop it hits a type layer anywhere inside its text's
## bounds.

import std/[json, options, math]
import ./core
import ../compose

# ---------------------------------------------------------------- frame_cmds.rs

proc frameHasLayerE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document"
  if d.selectedLayers.len > 0 or d.activeLayer.isSome: "" else: "select a layer"

proc clipMask(r: Rect): LayerMask =
  ## A reveal mask that is on inside `r` and off (hidden) everywhere else.
  result = hideAllMask()
  let (w, h) = (r.width, r.height)
  if w > 0 and h > 0:
    var ones = newSeq[float32](w * h)
    for v in ones.mitems: v = 1
    result.surface.writeRegion(r, ones)
    result.surface.prune()

proc frameGroup(s: var Session; grouped: JsonNode): JsonNode =
  ## Clips the group `layer.new.groupFromLayers` returned to its contents' bounds.
  let g = grouped.pget("layer").asU64
  if g.isNone: raise other("grouping did not return a layer")
  let gid = LayerId(g.get)
  let d = s.active
  if d.isNil: raise noDocument()
  let gl = d.doc[].findLayer(gid)
  if gl.isNil: raise noLayer(gid)
  let b = layerBounds(gl[])
  let r = (if b.isSome and not b.get.isEmpty: b.get else: d.doc[].bounds)
  s.edit("Frame from Layers", proc(doc: var Document; active: var Option[LayerId]) =
    let l = doc.findLayer(gid)
    if l.isNil: raise noLayer(gid)
    l[].mask = some(clipMask(r)))
  %*{"layer": uint64(gid), "frame": [r.x0, r.y0, r.width, r.height]}

proc frameFromLayers(s: var Session; p: JsonNode): JsonNode =
  ## Groups the selected layers, then turns the group into a frame, as one history step.
  var gp = newJObject()
  let n = p.pget("name")
  if not n.isNil: gp["name"] = n
  let journal = s.journal.len
  let grouped = s.execute("layer.new.groupFromLayers", gp)
  try:
    result = frameGroup(s, grouped)
  except CatchableError:
    # The group couldn't be framed: take the grouping back, leaving nothing to redo and no
    # grouping step to record.
    discard s.undo()
    s.journal.setLen(min(journal, s.journal.len))
    let st = s.active
    if not st.isNil: st.history.clearRedo()
    raise
  # Fold the grouping step into the frame step.
  let st = s.active
  if not st.isNil: discard st.history.purgeLast()

const MaxFrameSide = 300_000 ## the largest frame side (px) `layer.new.frame` accepts

proc ellipseMask(r, area: Rect): LayerMask =
  ## A reveal mask that is on inside the ellipse inscribed in `r` and off everywhere else, with
  ## 4x4-supersampled edges (as the Elliptical Marquee). Only pixels inside `area` are written.
  result = hideAllMask()
  let (cx, cy) = ((float64(r.x0) + float64(r.x1)) / 2.0, (float64(r.y0) + float64(r.y1)) / 2.0)
  let (rx, ry) = (float64(max(r.width, 1)) / 2.0, float64(max(r.height, 1)) / 2.0)
  proc inside(x, y: float64): bool =
    let (dx, dy) = ((x - cx) / rx, (y - cy) / ry)
    dx * dx + dy * dy <= 1.0
  let cut = r.intersect(area)
  for y in cut.y0 ..< cut.y1:
    for x in cut.x0 ..< cut.x1:
      let (fx, fy) = (float64(x), float64(y))
      var n = 0
      for i in 0 ..< 16:
        if inside(fx + (float64(i mod 4) + 0.5) / 4.0, fy + (float64(i div 4) + 0.5) / 4.0): inc n
      if n > 0: result.surface.writePixel(x, y, [float32(n) / 16'f32])
  result.surface.prune()

proc newFrame(s: var Session; p: JsonNode): JsonNode =
  ## Frame tool (K): a new frame (a group clipped to the drawn rectangle or ellipse) as one history
  ## step. With `layer` (a pixel layer, not the Background), that layer becomes the frame's
  ## content; else it is empty.
  const cmdId = "layer.new.frame"
  proc get(k: string): int =
    let v = intI32(cmdId, p, k)
    if v.isNone: raise bad(cmdId, "missing `" & k & "`")
    int(v.get)
  let (x, y, w, h) = (get("x"), get("y"), get("width"), get("height"))
  if w < 1 or w > MaxFrameSide or h < 1 or h > MaxFrameSide:
    raise bad(cmdId, "`width` and `height` must be 1..=" & $MaxFrameSide)
  if x + w > int(high(int32)) or y + h > int(high(int32)):
    raise bad(cmdId, "the frame is outside the 32-bit coordinate range")
  let r = rect(x, y, x + w, y + h)
  var ellipse = false
  let sv = p.pget("shape")
  if not sv.isNil:
    if sv.kind == JString and sv.str == "rectangle": ellipse = false
    elif sv.kind == JString and sv.str == "ellipse": ellipse = true
    else: raise bad(cmdId, "`shape` must be \"rectangle\" or \"ellipse\"")
  var content = none(LayerId)
  let lv = p.pget("layer")
  if not lv.isNull:
    let id = lv.asU64
    if id.isNone: raise bad(cmdId, "`layer` = " & jsonText(lv) & " is not a layer id")
    content = some(LayerId(id.get))
  let name = p.pget("name").asStr
  let gid = s.editWith("New Frame", proc(doc: var Document; active: var Option[LayerId]): LayerId =
    let mask = (if ellipse: ellipseMask(r, doc.bounds) else: clipMask(r.intersect(doc.bounds)))
    let nm = (if name.isSome: name.get else: doc.nextLayerName("Frame"))
    var gid: LayerId
    if content.isSome:
      let id = content.get
      let l = doc.findLayer(id)
      if l.isNil: raise noLayer(id)
      if l[].content.kind != lkRaster or core.isBackground(l[]):
        raise bad(cmdId, "only a pixel layer (not the Background) can go in a frame")
      gid = doc.insertAbove(some(id), groupLayer(nm, @[]))
      let child = doc.remove(id)
      if child.isNone: raise noLayer(id)
      let g = doc.findLayer(gid)
      if g.isNil: raise noLayer(gid)
      let kids = childrenMut(g[])
      if kids.isNil: raise noLayer(gid)
      kids[].add child.get
      checkGroupDepth(doc, "New Frame")
    else: gid = doc.insertAbove(active, groupLayer(nm, @[]))
    let g = doc.findLayer(gid)
    if g.isNil: raise noLayer(gid)
    g[].mask = some(mask)
    active = some(gid)
    gid)
  reselect(s, @[gid], some(gid))
  %*{"layer": uint64(gid), "frame": [x, y, w, h], "shape": (if ellipse: "ellipse" else: "rectangle")}

proc frameSpecs*(): seq[CommandSpec] =
  @[cmd("layer.new.frame", "New Frame", [], "",
        """{"x":i32,"y":i32,"width":u32,"height":u32,"shape":"rectangle|ellipse"="rectangle","layer":id?,"name":str?} → {layer, frame:[x,y,w,h], shape}: Frame tool; a group clipped to the shape, holding pixel layer `layer` if given""",
        enabledProc(if s.active.isNil: "no document open" else: ""), runProc(newFrame(s, p))),
    cmd("layer.new.frameFromLayers", "Frame from Layers", ["Layer", "New"], "",
        """{"name":str?} → {layer, frame:[x,y,w,h]}: groups the selected layers and clips them to a frame rect""",
        frameHasLayerE, runProc(frameFromLayers(s, p)))]

# ---------------------------------------------------------------- pick_cmds.rs

proc pickHasDocE(s: Session): string = (if s.active.isNil: "no document" else: "")

proc allVisible(doc: Document; path: LayerPath): bool =
  for n in 1 .. path.len:
    let a = doc.layerAt(path[0 ..< n])
    if a.isNone or not a.get.visible: return false
  true

proc hasOwnSurface(l: Layer): bool =
  case l.content.kind
  of lkRaster: true
  of lkText: l.content.text.cache.isSome
  of lkShape: l.content.shape.cache.isSome
  of lkSmart: l.content.smart.cache.isSome
  else: false

proc layersHit(doc: Document; x, y: int; typeBounds: bool): seq[LayerId] =
  ## Layers with visible pixels at (x, y), topmost first; with `typeBounds` a type layer is hit
  ## anywhere inside the bounds of its rendered text, as Photoshop's Auto-Select does.
  if x >= int(high(int32)) or y >= int(high(int32)): return
  let rows = doc.walk
  for i in countdown(rows.high, 0):
    let (path, _, l) = rows[i]
    # The layer and every enclosing group must be visible.
    if not allVisible(doc, path): continue
    # An artboard clips its layers to the board: pixels past its edge can't be picked. The board
    # itself is hit anywhere on it, below its layers.
    if path.len > 0 and path[0] < doc.layers.len:
      let board = doc.layers[path[0]].artboard
      if board.isSome and not board.get.rect.contains(x, y): continue
    if l.artboard.isSome:
      result.add l.id
      continue
    if l.content.kind in {lkGroup, lkAdjustment}: continue
    var alpha = 0'f32
    if l.content.kind == lkFill: alpha = 1
    elif l.content.kind == lkText and typeBounds and l.content.text.cache.isSome and
         l.content.text.cache.get.contentBounds.contains(x, y): alpha = 1
    elif hasOwnSurface(l):
      var px: array[1, array[4, float32]]
      l.surface.readRgbaInto(rectXywh(x, y, 1, 1), px)
      alpha = px[0][3]
    var mask = 1'f32
    if l.mask.isSome and l.mask.get.enabled:
      var v: array[1, float32]
      l.mask.get.surface.readPixel(x, y, v)
      mask = v[0]
    if alpha * mask > 0: result.add l.id

proc layersAt*(doc: Document; x, y: int): seq[LayerId] =
  ## Layers with visible pixels at (x, y), topmost first (hidden layers and hidden groups skipped).
  layersHit(doc, x, y, false)

proc topGroup(doc: Document; id: LayerId): LayerId =
  ## The outermost group containing `id` (the layer itself when it isn't in a group). An artboard
  ## is not a group here: Group mode stops at the outermost group on the board.
  for (path, _, l) in doc.walk:
    if l.id == id:
      let first = (if path.len >= 1: doc.layerAt(path[0 ..< 1]) else: none(Layer))
      let depth = (if first.isSome and first.get.artboard.isSome: 2 else: 1)
      if path.len < depth: return id
      let t = doc.layerAt(path[0 ..< depth])
      return (if t.isSome: t.get.id else: id)
  id

proc autoSelectTarget*(doc: Document; x, y: int; group: bool): Option[LayerId] =
  ## The layer an Auto-Select click at (x, y) picks (with `group`, its outermost group): the topmost
  ## hit, a type layer by its text bounds, clicking through fully locked layers.
  for id in layersHit(doc, x, y, true):
    if not doc.effectiveLocks(id).all:
      return some(if group: topGroup(doc, id) else: id)

proc pick(s: var Session; p: JsonNode): JsonNode =
  let xv = p.pget("x").asF64
  if xv.isNone: raise bad("layer.pickAt", "missing `x`")
  let x = asI32(floor(xv.get))
  let yv = p.pget("y").asF64
  if yv.isNone: raise bad("layer.pickAt", "missing `y`")
  let y = asI32(floor(yv.get))
  let st = s.active
  if st.isNil: raise noDocument()
  let doc = st.doc
  # The right-click list names the layers with pixels under the pointer.
  if p.pget("list").asBool.get(false):
    var names = newJArray()
    for id in layersHit(doc[], x, y, false):
      let l = doc[].findLayer(id)
      if not l.isNil: names.add %*{"layer": uint64(l[].id), "name": l[].name}
    return %*{"layers": names}
  let group = p.pget("target").asStr == some("group")
  let target = autoSelectTarget(doc[], x, y, group)
  if target.isNone: return %*{"layer": nil}
  if p.pget("select").asBool.get(true):
    let mode = p.pget("mode").asStr.get("replace")
    # Like Photoshop, a plain click on one of several selected layers keeps them all selected, so
    # the drag that follows moves the whole selection.
    let a = s.active
    let keeps = mode == "replace" and not a.isNil and a[].isLayerSelected(target.get) and a[].selectedOrdered.len > 1
    if not keeps: discard s.execute("layer.select", %*{"layer": uint64(target.get), "mode": mode})
  %*{"layer": uint64(target.get)}

proc layersInRect*(doc: Document; r: Rect; group: bool): seq[LayerId] =
  ## Layers whose content touches document rect `r`, top to bottom: visible, not fully locked, not
  ## the locked Background. A layer on an artboard counts only where the board shows it. With
  ## `group`, each counts as its outermost group (deduplicated).
  if r.isEmpty: return
  let canvas = doc.bounds
  let rows = doc.walk
  for i in countdown(rows.high, 0):
    let (path, _, l) = rows[i]
    if l.content.kind in {lkGroup, lkAdjustment} or isLockedBackground(l) or doc.effectiveLocks(l.id).all or
       not allVisible(doc, path): continue
    var b = compose.layerBounds(l, canvas)
    if path.len > 0 and path[0] < doc.layers.len:
      let board = doc.layers[path[0]].artboard
      if board.isSome: b = b.intersect(board.get.rect)
    if b.isEmpty or b.intersect(r).isEmpty: continue
    let id = (if group: topGroup(doc, l.id) else: l.id)
    if id notin result: result.add id

proc selectInRect(s: var Session; p: JsonNode): JsonNode =
  ## `layer.selectInRect`: the Move tool's marquee with Auto-Select, dragged from an empty spot.
  proc num(k: string): float64 =
    let v = p.pget(k).asF64
    if v.isNone or v.get != v.get or abs(v.get) == Inf: raise bad("layer.selectInRect", "missing or invalid `" & k & "`")
    v.get
  let (x, y, w, h) = (num("x"), num("y"), num("width"), num("height"))
  proc px(v: float64): int = int(clamp(v, float64(low(int32) div 2), float64(high(int32) div 2)))
  # A rect dragged up or left has a negative size.
  let r = rect(px(floor(min(x, x + w))), px(floor(min(y, y + h))), px(ceil(max(x, x + w))), px(ceil(max(y, y + h))))
  let st = s.active
  if st.isNil: raise noDocument()
  let doc = st.doc
  let group = p.pget("target").asStr == some("group")
  let hits = layersInRect(doc[], r, group)
  let mode = p.pget("mode").asStr.get("replace")
  let add = case mode
    of "replace": false
    of "add": true
    else: raise bad("layer.selectInRect", "unknown mode `" & mode & "`")
  # Nothing touched leaves the layer selection as it is.
  if hits.len > 0:
    var ids = (if add and not s.active.isNil: s.active[].selectedOrdered else: newSeq[LayerId]())
    var extra: seq[LayerId]
    for id in hits:
      if id notin ids: extra.add id
    ids.add extra
    setSelection(s, ids, some(hits[0]), some(hits[0]))
  var outp: seq[uint64]
  for id in hits: outp.add uint64(id)
  %*{"layers": outp}

proc pickSpecs*(): seq[CommandSpec] =
  @[cmd("layer.selectInRect", "Select Layers in Marquee", [], "",
        """{"x":px,"y":px,"width":px,"height":px,"target":"layer|group"="layer","mode":"replace|add"="replace"} → {layers} (the layers whose content touches the rect, topmost first; none leaves the selection as it is)""",
        pickHasDocE, runProc(selectInRect(s, p))),
    cmd("layer.pickAt", "Auto-Select Layer", [], "",
        """{"x":px,"y":px,"target":"layer|group"="layer","select":bool=true,"mode":"replace|toggle|add"="replace","list":bool=false (return every layer with pixels there, topmost first)} → {layer} | {layers}""",
        pickHasDocE, runProc(pick(s, p)), journal = false)]
