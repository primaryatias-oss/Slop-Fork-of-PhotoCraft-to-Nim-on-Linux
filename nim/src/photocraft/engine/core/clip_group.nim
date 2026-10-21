## Clip groups follow layers as they are added, moved and deleted, as in Photoshop (port of
## `clip_group.rs`; the rules are documented there). Part of the `engine/core` unit: included,
## not imported.

proc clipSiblings(doc: Document; path: LayerPath): (ptr seq[Layer], int) =
  ## The sibling list holding the layer at `path`, and the layer's index in it.
  if path.len == 0: return (nil, -1)
  if path.len == 1: return (unsafeAddr doc.layers, path[0])
  let p = cast[var Document](unsafeAddr doc).layerAtPtr(path[0 ..< ^1])
  if p.isNil or p[].content.kind != lkGroup: return (nil, -1)
  (addr p[].content.group.children, path[^1])

proc clipNeighbour(doc: Document; id: LayerId; delta: int): ptr Layer =
  ## The layer `delta` slots above (+) or below (−) `id` among its siblings.
  let path = doc.pathOf(id)
  if path.isNone: return nil
  let (list, idx) = clipSiblings(doc, path.get)
  if list.isNil: return nil
  let j = idx + delta
  if j < 0 or j >= list[].len: nil else: addr list[][j]

proc groupOf(doc: Document; id: LayerId): seq[LayerId] =
  ## The clip group `id` belongs to, base first, when it has one.
  let path = doc.pathOf(id)
  if path.isNone: return
  let (list, idx) = clipSiblings(doc, path.get)
  if list.isNil: return
  var base = -1
  for i in countdown(min(idx, list[].len - 1), 0):
    if not list[][i].clipped:
      base = i
      break
  if base < 0: return
  var group: seq[LayerId]
  for i in base ..< list[].len:
    if i == base or list[][i].clipped: group.add list[][i].id
    else: break
  if group.len > 1: group else: @[]

proc clippedTo(doc: Document; id: LayerId): seq[LayerId] =
  ## The layers clipped to `id`: the clipped run directly above it.
  let g = groupOf(doc, id)
  if g.len > 0 and g[0] == id: g[1 .. ^1] else: @[]

proc cmpPath*(a, b: LayerPath): int =
  ## Lexicographic order of layer paths (Rust's `Vec<usize>` ordering).
  for i in 0 ..< min(a.len, b.len):
    if a[i] != b[i]: return cmp(a[i], b[i])
  cmp(a.len, b.len)

type ClipBefore* = object
  ## The clip groups of layers about to move or be deleted, read before they leave their slots.
  moving: seq[LayerId]
  clipped: seq[LayerId]
  groups: seq[(LayerId, seq[LayerId])]
  left: seq[LayerId]

proc readBefore*(doc: Document; moving: openArray[LayerId]): ClipBefore =
  for id in moving:
    let l = doc.findLayer(id)
    if not l.isNil and l.clipped: result.clipped.add id
  for id in moving:
    let g = groupOf(doc, id)
    if g.len > 0: result.groups.add (id, g)
  var left: seq[LayerId]
  for id in moving:
    for c in clippedTo(doc, id):
      if c notin moving: left.add c
  # `dedup`: drop consecutive repeats.
  for x in left:
    if result.left.len == 0 or result.left[^1] != x: result.left.add x
  result.moving = @moving

proc group(b: ClipBefore; id: LayerId): seq[LayerId] =
  for (m, g) in b.groups:
    if m == id: return g

proc setClippedFlag(doc: var Document; id: LayerId; clipped: bool) =
  let l = doc.layerPtr(id)
  if not l.isNil: l.clipped = clipped

proc releaseOrphans*(doc: var Document) =
  ## Rule 4: releases the clipped layers at the bottom of a sibling list.
  var stack: seq[ptr seq[Layer]] = @[addr doc.layers]
  while stack.len > 0:
    let list = stack.pop()
    for l in list[].mitems:
      if not l.clipped: break
      l.clipped = false
    for l in list[].mitems:
      if l.content.kind == lkGroup: stack.add addr l.content.group.children

proc settle*(doc: var Document; placed: openArray[LayerId]; before: ClipBefore; force: bool) =
  ## After `placed` were added or moved: each joins or leaves clip groups (rules 1–2), topmost
  ## first, or is clipped anyway when `force`. Then layers that lost their base are released
  ## (rule 3), and any clipped layer with nothing below it (rule 4).
  var order: seq[(LayerPath, LayerId)]
  for id in placed:
    let p = doc.pathOf(id)
    if p.isSome: order.add (p.get, id)
  order.sort(proc(a, b: (LayerPath, LayerId)): int =
    let c = cmpPath(a[0], b[0])
    if c != 0: c else: cmp(uint64(a[1]), uint64(b[1])))
  for k in countdown(order.len - 1, 0):
    let id = order[k][1]
    let group = before.group(id)
    proc travelling(n: LayerId): bool = n in before.moving and n in group
    let above = clipNeighbour(doc, id, 1)
    let joins = not above.isNil and above.clipped and not travelling(above.id)
    let below = clipNeighbour(doc, id, -1)
    let stays = id in before.clipped and not below.isNil and (below.clipped or below.id in group)
    setClippedFlag(doc, id, force or joins or stays)
  for id in before.left:
    if id notin placed: setClippedFlag(doc, id, false)
  releaseOrphans(doc)
