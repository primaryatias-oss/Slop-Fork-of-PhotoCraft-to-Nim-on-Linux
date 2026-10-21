## History panel actions beyond stepping (port of `history_cmds.rs`): Delete (the trash button),
## New Document ("Create new document from current state") and snapshots (the camera button).
##
## States are indexed like `History.entries`: 0 is the oldest state still held, the last index is
## the current document. Snapshots are named copies of the document that outlive the states; they
## live on `DocState.snapshots` for the session only, addressed by an id that is never reused.

import std/[json, options, strutils]
import ./core
import ../compose

proc hasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")

proc canDeleteE(s: Session): string =
  let d = s.active
  if not d.isNil and d.history.canUndo: "" else: "the oldest state can't be deleted"

proc stateIndex(s: Session; cmdId: string; p: JsonNode): (int, int) =
  ## The `state` param as an index into the active document's history entries, defaulting to the
  ## current state.
  let st = s.active
  if st.isNil: raise noDocument()
  let current = st.history.pastLen
  let v = p.pget("state")
  if v.isNull: return (current, current)
  let n = intParam(p, "state")
  if n.isNone: raise bad(cmdId, "`state` must be a number")
  if n.get < 0 or n.get > int64(current): raise bad(cmdId, "`state` = " & $n.get & " is not a state (0..=" & $current & ")")
  (int(n.get), current)

proc deleteState(s: var Session; p: JsonNode): JsonNode =
  ## History panel › Delete: drop the state and every state after it, as Photoshop does in linear
  ## history. The document returns to the state before it, and the deleted steps can't be redone.
  const cmdId = "history.deleteState"
  let (i, current) = stateIndex(s, cmdId, p)
  if i == 0: raise bad(cmdId, "the oldest state can't be deleted")
  # Step back to the state before `i`; the steps from `i` on are then redo states.
  for _ in i .. current:
    if not s.undo(): raise other("the history can't step back right now")
  let st = s.active
  if st.isNil: raise noDocument()
  st.history.clearRedo()
  let entries = st.history.entries
  %*{"states": entries.len, "current": (if entries.len > 0: %entries[^1] else: newJNull())}

proc newDocumentFromState(s: var Session; p: JsonNode): JsonNode =
  ## History panel › New Document: a new untitled document holding the document as it was at a
  ## state (the current one by default), named after that state, with a history of its own.
  const cmdId = "history.newDocument"
  let (i, current) = stateIndex(s, cmdId, p)
  let st = s.active
  if st.isNil: raise noDocument()
  let src = (if i == current: some(st.doc) else: st.history.state(i))
  if src.isNone: raise bad(cmdId, "state " & $i & " is not held")
  let entries = st.history.entries
  let label = (if i < entries.len: entries[i] else: "")
  var doc = src.get[]
  doc.id = freshDocId()
  let n = p.pget("name").asStr
  doc.name =
    if n.isSome and strip(n.get).len > 0: n.get
    elif strip(label).len > 0: label
    else: doc.name & " copy"
  reidentifyCopiedLayers(doc)
  let name = doc.name
  let index = s.addDocument(doc)
  %*{"document": index, "name": name}

proc snapshotIndex(s: Session; cmdId: string; p: JsonNode): int =
  ## The `snapshot` param: the index of the active document's snapshot with that id.
  let st = s.active
  if st.isNil: raise noDocument()
  let id = p.pget("snapshot").asU64
  if id.isNone: raise bad(cmdId, "`snapshot` must be a snapshot id")
  for i, snap in st.snapshots:
    if snap.id == id.get: return i
  raise bad(cmdId, "no snapshot " & $id.get)

proc nameParam(cmdId: string; p: JsonNode): Option[string] =
  ## A non-blank `name` param, trimmed; none when absent.
  let v = p.pget("name")
  if v.isNull: return none(string)
  if v.kind == JString and strip(v.str).len > 0: return some(strip(v.str))
  raise bad(cmdId, "`name` must be a non-empty string")

proc newSnapshot(s: var Session; p: JsonNode): JsonNode =
  ## History panel › Create new snapshot: a named copy of the document, "Snapshot N" by default.
  ## `from` is the New Snapshot dialog's From: the full document (default), its layers merged into
  ## one, or only the active layer. Not a history state.
  const cmdId = "history.newSnapshot"
  let name = nameParam(cmdId, p)
  var frm = "fullDocument"
  let fv = p.pget("from")
  if not fv.isNull:
    if fv.kind == JString and fv.str in ["fullDocument", "mergedLayers", "currentLayer"]: frm = fv.str
    else: raise bad(cmdId, "`from` must be \"fullDocument\", \"mergedLayers\" or \"currentLayer\"")
  let st = s.active
  if st.isNil: raise noDocument()
  proc oneLayer(doc: Document; layer: Layer): (DocRef, LayerTarget) =
    let r = DocRef()
    r[] = doc
    r.layers = @[layer]
    (r, LayerTarget(active: some(layer.id), selected: @[layer.id]))
  var doc: DocRef
  var layers: LayerTarget
  case frm
  of "mergedLayers":
    let fmt = st.doc[].pixelFormat
    (doc, layers) = oneLayer(st.doc[], newLayer("Merged Layers", LayerContent(kind: lkRaster, raster: flattenToSurface(st.doc[], fmt))))
  of "currentLayer":
    let l = (if st.activeLayer.isSome: st.doc[].findLayer(st.activeLayer.get) else: nil)
    if l.isNil: raise bad(cmdId, "no active layer")
    (doc, layers) = oneLayer(st.doc[], l[])
  else: (doc, layers) = (st.doc, st[].layerTarget)
  let id = st.nextSnapshot
  st.nextSnapshot = (if id == high(uint64): id else: id + 1)
  let nm = (if name.isSome: name.get else: "Snapshot " & $id)
  st.snapshots.add Snapshot(id: id, name: nm, doc: doc, layers: layers)
  %*{"snapshot": id, "name": nm}

proc selectSnapshot(s: var Session; p: JsonNode): JsonNode =
  ## Clicking a snapshot: the document returns to it as one new, undoable state named after it.
  const cmdId = "history.selectSnapshot"
  let i = snapshotIndex(s, cmdId, p)
  let st0 = s.active
  if st0.isNil or i >= st0.snapshots.len: raise noDocument()
  let snap = st0.snapshots[i]
  s.edit(snap.name, proc(doc: var Document; active: var Option[LayerId]) =
    # Keep the identity (colour state, views and caches are keyed by it) and the tab name.
    let (id, name) = (doc.id, doc.name)
    doc = snap.doc[]
    doc.id = id
    doc.name = name
    if snap.layers.active.isSome and not doc.findLayer(snap.layers.active.get).isNil: active = snap.layers.active
    else: active = doc.topLayer)
  let st = s.active
  if st.isNil: raise noDocument()
  if st.activeLayer == snap.layers.active:
    st.selectedLayers = snap.layers.selected
    fixSelection(st[])
  %*{"snapshot": snap.id, "name": snap.name}

proc renameSnapshot(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "history.renameSnapshot"
  let i = snapshotIndex(s, cmdId, p)
  let name = nameParam(cmdId, p)
  if name.isNone: raise bad(cmdId, "`name` is required")
  let st = s.active
  if st.isNil: raise noDocument()
  if i >= st.snapshots.len: raise bad(cmdId, "no such snapshot")
  st.snapshots[i].name = name.get
  %*{"snapshot": st.snapshots[i].id, "name": name.get}

proc deleteSnapshot(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "history.deleteSnapshot"
  let i = snapshotIndex(s, cmdId, p)
  let st = s.active
  if st.isNil: raise noDocument()
  let snap = st.snapshots[i]
  st.snapshots.delete(i)
  %*{"snapshot": snap.id, "snapshots": st.snapshots.len}

proc historySpecs*(): seq[CommandSpec] =
  @[cmd("history.deleteState", "Delete State", [], "",
        """{"state":index?} (History entry, 0 = oldest; default the current state) → {states,current}: removes the state and every later one""",
        canDeleteE, runProc(deleteState(s, p))),
    cmd("history.newDocument", "New Document from State", [], "",
        """{"state":index?,"name":str?} (History entry, 0 = oldest; default the current state) → {document,name}""",
        hasDocE, runProc(newDocumentFromState(s, p))),
    cmd("history.newSnapshot", "New Snapshot", [], "",
        """{"name":str?="Snapshot N","from":"fullDocument"|"mergedLayers"|"currentLayer"="fullDocument"} → {snapshot,name}: a named copy of the document for the session (not a history state)""",
        hasDocE, runProc(newSnapshot(s, p))),
    cmd("history.selectSnapshot", "Select Snapshot", [], "",
        """{"snapshot":id} → {snapshot,name}: the document returns to the snapshot as a new history state named after it""",
        hasDocE, runProc(selectSnapshot(s, p))),
    cmd("history.renameSnapshot", "Rename Snapshot", [], "", """{"snapshot":id,"name":str} → {snapshot,name}""",
        hasDocE, runProc(renameSnapshot(s, p))),
    cmd("history.deleteSnapshot", "Delete Snapshot", [], "",
        """{"snapshot":id} → {snapshot,snapshots}: the document and its history are unchanged""",
        hasDocE, runProc(deleteSnapshot(s, p)))]
