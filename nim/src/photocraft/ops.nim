## Undo/redo history. Pixel tiles are shared copy-on-write refs, so a document copy costs
## O(layers + tiles) pointer copies, not pixel copies: history stores whole-document snapshots
## per step (one state per History panel row). Memory is bounded by `maxStates` plus an
## approximate byte budget counting only tiles not shared with the current document.

import std/[deques, sets, tables, options]
import ./doc

type
  DocRef* = ref Document
  LayerTarget* = object
    ## The layers a state targeted: the active layer and every selected layer.
    active*: Option[LayerId]
    selected*: seq[LayerId]
  HistoryState* = object
    label*: string
    doc*: DocRef
    layers*: LayerTarget
  TileRefs = object
    refs: Table[uint, (int, int)]  ## tile address -> (count, bytes)
    bytes: int64
  History* = object
    undoStack: Deque[HistoryState]
    redoStack: seq[HistoryState]
    tiles: Option[TileRefs]
    maxStates*: int
    maxBytes*: int    ## 0 = unlimited
    currentLabel: string
    currentLayers: LayerTarget

proc newHistory*(maxStates = 50): History =
  History(maxStates: max(maxStates, 1), currentLabel: "Open")

iterator surfaces(d: Document): Surface =
  for l in d.allLayers:
    if l.hasSurface: yield l.surface
    if l.mask.isSome: yield l.mask.get.surface
  for c in d.channels: yield c.surface
  if d.quickMask.isSome: yield d.quickMask.get.surface

proc address(t: Tile): uint = cast[uint](t)

proc add(t: var TileRefs; d: Document) =
  for s in d.surfaces:
    for _, tile in s.tiles:
      let k = tile.address
      if k notin t.refs:
        t.refs[k] = (0, tile.data.len)
        t.bytes += tile.data.len
      t.refs[k][0] += 1

proc remove(t: var TileRefs; d: Document) =
  for s in d.surfaces:
    for _, tile in s.tiles:
      let k = tile.address
      if k in t.refs:
        t.refs[k][0] -= 1
        if t.refs[k][0] <= 0:
          t.bytes -= t.refs[k][1]
          t.refs.del k

proc bytesWith(t: TileRefs; current: Document): int64 =
  result = t.bytes
  var seen = initHashSet[uint]()
  for s in current.surfaces:
    for _, tile in s.tiles:
      let k = tile.address
      if k notin t.refs and not seen.containsOrIncl(k): result += tile.data.len

proc tileBytes(d: Document; seen: var HashSet[uint]): int =
  for s in d.surfaces:
    for _, tile in s.tiles:
      if not seen.containsOrIncl(tile.address): result += tile.data.len

proc clearRedo*(h: var History) =
  if h.tiles.isSome:
    for s in h.redoStack: h.tiles.get.remove(s.doc[])
  h.redoStack.setLen(0)

proc record*(h: var History; label: string; before: DocRef; layers: LayerTarget) =
  ## Records that `before` was replaced by a new current document via step `label`.
  if h.maxBytes == 0: h.tiles = none(TileRefs)
  if h.tiles.isSome: h.tiles.get.add(before[])
  let prevLabel = h.currentLabel
  let prevLayers = h.currentLayers
  h.currentLabel = label
  h.currentLayers = layers
  h.undoStack.addLast HistoryState(label: prevLabel, doc: before, layers: prevLayers)
  h.clearRedo()
  while h.undoStack.len > h.maxStates:
    let s = h.undoStack.popFirst()
    if h.tiles.isSome: h.tiles.get.remove(s.doc[])

proc canUndo*(h: History): bool = h.undoStack.len > 0
proc canRedo*(h: History): bool = h.redoStack.len > 0
proc undoLabel*(h: History): Option[string] = (if h.canUndo: some(h.currentLabel) else: none(string))
proc redoLabel*(h: History): Option[string] = (if h.canRedo: some(h.redoStack[^1].label) else: none(string))
proc redoLabels*(h: History): seq[string] =
  for i in countdown(h.redoStack.high, 0): result.add h.redoStack[i].label
proc setCurrentLayers*(h: var History; layers: LayerTarget) = h.currentLayers = layers
proc currentLabel*(h: History): string = h.currentLabel
proc setCurrentLabel*(h: var History; label: string) = h.currentLabel = label

proc undo*(h: var History; current: DocRef): Option[(DocRef, LayerTarget)] =
  ## `current` becomes redoable; returns the document to make current and its layer target.
  if h.undoStack.len == 0: return
  let prev = h.undoStack.popLast()
  if h.tiles.isSome:
    h.tiles.get.remove(prev.doc[])
    h.tiles.get.add(current[])
  h.redoStack.add HistoryState(label: h.currentLabel, doc: current, layers: h.currentLayers)
  h.currentLabel = prev.label
  h.currentLayers = prev.layers
  some((prev.doc, prev.layers))

proc redo*(h: var History; current: DocRef): Option[(DocRef, LayerTarget)] =
  if h.redoStack.len == 0: return
  let next = h.redoStack.pop()
  if h.tiles.isSome:
    h.tiles.get.remove(next.doc[])
    h.tiles.get.add(current[])
  h.undoStack.addLast HistoryState(label: h.currentLabel, doc: current, layers: h.currentLayers)
  h.currentLabel = next.label
  h.currentLayers = next.layers
  some((next.doc, next.layers))

proc entries*(h: History): seq[string] =
  ## History panel rows: past labels oldest→newest, then the current label.
  for s in h.undoStack: result.add s.label
  result.add h.currentLabel

proc state*(h: History; i: int): Option[DocRef] =
  if i >= 0 and i < h.undoStack.len: some(h.undoStack[i].doc) else: none(DocRef)

proc pastLen*(h: History): int = h.undoStack.len

proc purgeLast*(h: var History): bool =
  h.clearRedo()
  if h.undoStack.len == 0: return false
  let s = h.undoStack.popLast()
  if h.tiles.isSome: h.tiles.get.remove(s.doc[])
  true

proc clear*(h: var History) =
  h.undoStack.clear()
  h.redoStack.setLen(0)
  h.tiles = none(TileRefs)

proc uniqueBytes*(h: History; current: Document): int =
  ## Approximate pixel bytes held by history and not shared with `current`.
  var seen = initHashSet[uint]()
  discard tileBytes(current, seen)
  for s in h.undoStack: result += tileBytes(s.doc[], seen)
  for s in h.redoStack: result += tileBytes(s.doc[], seen)

proc pixelBytes*(h: History; current: Document): int =
  if h.tiles.isSome: return int(h.tiles.get.bytesWith(current))
  var seen = initHashSet[uint]()
  result = tileBytes(current, seen)
  for s in h.undoStack: result += tileBytes(s.doc[], seen)
  for s in h.redoStack: result += tileBytes(s.doc[], seen)

proc trim*(h: var History; current: Document): int =
  ## Drops the oldest undo states that don't fit `maxBytes` (always keeps the newest one).
  if h.maxBytes == 0 or h.undoStack.len <= 1: return 0
  if h.tiles.isNone:
    var t: TileRefs
    for s in h.undoStack: t.add(s.doc[])
    for s in h.redoStack: t.add(s.doc[])
    h.tiles = some(t)
  if h.tiles.get.bytes <= h.maxBytes and h.tiles.get.bytesWith(current) <= h.maxBytes: return 0
  var seen = initHashSet[uint]()
  var total = tileBytes(current, seen)
  for i in countdown(h.redoStack.high, 0): total += tileBytes(h.redoStack[i].doc[], seen)
  var keep = 0
  for i in countdown(h.undoStack.len - 1, 0):
    total += tileBytes(h.undoStack[i].doc[], seen)
    if total > h.maxBytes and keep >= 1: break
    inc keep
  result = h.undoStack.len - keep
  for _ in 0 ..< result:
    let s = h.undoStack.popFirst()
    h.tiles.get.remove(s.doc[])
