## The session (port of `lib.rs`, the dispatch half of `jobs.rs` and the registry of
## `commands.rs`): open documents, history, `edit`, undo/redo, and command dispatch. Part of the
## `engine/core` unit: included, not imported.

# ---------------------------------------------------------------- errors

proc engineError(kind: EngineErrorKind; cmd, detail: string): ref EngineError =
  let msg = case kind
    of eeUnknownCommand: "unknown command `" & cmd & "`"
    of eeDisabled: "command `" & cmd & "` is not available right now: " & detail
    of eeBadParams: "invalid parameters for `" & cmd & "`: " & detail
    of eeNoDocument: "no active document"
    of eeNoLayer: "no such layer " & detail
    of eeOther: detail
    of eeCancelled: "cancelled"
  (ref EngineError)(kind: kind, cmd: cmd, detail: detail, msg: msg)

proc unknownCommand*(id: string): ref EngineError = engineError(eeUnknownCommand, id, "")
proc disabledErr*(id, why: string): ref EngineError = engineError(eeDisabled, id, why)
proc bad*(cmd, msg: string): ref EngineError = engineError(eeBadParams, cmd, msg)
proc noDocument*(): ref EngineError = engineError(eeNoDocument, "", "")
proc noLayer*(id: LayerId): ref EngineError = engineError(eeNoLayer, "", "LayerId(" & $uint64(id) & ")")
proc other*(msg: string): ref EngineError = engineError(eeOther, "", msg)
proc cancelledErr*(): ref EngineError = engineError(eeCancelled, "", "")

proc allocationFailed*(e: ref AllocationError): ref EngineError =
  other("not enough memory for this operation (" & e.msg & "); the document was not changed")

# ---------------------------------------------------------------- JSON params (serde_json Value semantics)

proc `%`*(id: LayerId): JsonNode = %uint64(id)
proc `%`*(id: DocId): JsonNode = %uint64(id)
proc `%`*(id: JobId): JsonNode = %uint64(id)
proc `==`*(a, b: JobId): bool {.borrow.}

proc isNull*(n: JsonNode): bool {.inline.} = n.isNil or n.kind == JNull

proc pget*(p: JsonNode; key: string): JsonNode =
  ## `p.get(key)`: the field, or nil when `p` is not an object or has no such field.
  if p.isNil or p.kind != JObject: return nil
  p.fields.getOrDefault(key, nil)

proc asU64*(n: JsonNode): Option[uint64] =
  if not n.isNil and n.kind == JInt and n.num >= 0: some(uint64(n.num)) else: none(uint64)

proc asI64*(n: JsonNode): Option[int64] =
  if not n.isNil and n.kind == JInt: some(int64(n.num)) else: none(int64)

proc asF64*(n: JsonNode): Option[float64] =
  if n.isNil: none(float64)
  elif n.kind == JFloat: some(n.fnum)
  elif n.kind == JInt: some(float64(n.num))
  else: none(float64)

proc asBool*(n: JsonNode): Option[bool] =
  if not n.isNil and n.kind == JBool: some(n.bval) else: none(bool)

proc asStr*(n: JsonNode): Option[string] =
  if not n.isNil and n.kind == JString: some(n.str) else: none(string)

proc dropKey*(n: JsonNode; key: string) =
  ## Removes `key` from object `n` when present (`Map::remove`).
  if not n.isNil and n.kind == JObject and n.fields.hasKey(key): n.fields.del(key)

proc isObject*(n: JsonNode): bool {.inline.} = not n.isNil and n.kind == JObject
proc isArray*(n: JsonNode): bool {.inline.} = not n.isNil and n.kind == JArray

proc getStrOr*(p: JsonNode; key, default: string): string =
  let v = p.pget(key).asStr
  if v.isSome: v.get else: default
proc getBoolOr*(p: JsonNode; key: string; default: bool): bool =
  let v = p.pget(key).asBool
  if v.isSome: v.get else: default
proc getF64Or*(p: JsonNode; key: string; default: float64): float64 =
  let v = p.pget(key).asF64
  if v.isSome: v.get else: default

proc intParam*(p: JsonNode; key: string): Option[int64] =
  ## Integer parameter that also accepts JSON floats (UIs send `12.0`); rounds.
  let v = p.pget(key)
  if v.isNil: return
  if v.kind == JInt: return some(int64(v.num))
  let f = v.asF64
  if f.isSome and f.get == f.get and abs(f.get) != Inf: some(asI64(round(f.get))) else: none(int64)

proc intI32*(cmd: string; p: JsonNode; key: string): Option[int32] =
  ## `intParam` narrowed to i32 for geometry: a bad-params error when out of range.
  let v = intParam(p, key)
  if v.isNone: return
  if v.get < int64(low(int32)) or v.get > int64(high(int32)):
    raise bad(cmd, "`" & key & "` = " & $v.get & " is outside the 32-bit coordinate range")
  some(int32(v.get))

iterator sortedPairs*(n: JsonNode): (string, JsonNode) =
  ## An object's fields in serde_json's order (its `Map` is a `BTreeMap`: keys sorted bytewise).
  if not n.isNil and n.kind == JObject:
    var keys: seq[string]
    for k in n.fields.keys: keys.add k
    keys.sort(system.cmp[string])
    for k in keys: yield (k, n.fields[k])

proc serdeFloat*(x: float64): string =
  ## serde_json's (ryu's) text for a finite f64: shortest digits, `1e20`, `1.5e-7`, `0.0001`.
  if x != x or abs(x) == Inf: return "null"
  let t = $x
  var neg = false
  var m = t
  if m.startsWith("-"):
    neg = true
    m = m[1 .. ^1]
  var exp = 0
  let e = m.find('e')
  if e >= 0:
    exp = parseInt(m[e + 1 .. ^1].replace("+", ""))
    m = m[0 ..< e]
  let dot = m.find('.')
  var digits = m
  if dot >= 0:
    digits = m[0 ..< dot] & m[dot + 1 .. ^1]
    exp -= m.len - dot - 1
  # Normalise: no leading zeros, no trailing zeros (moved into the exponent).
  var lead = 0
  while lead < digits.len - 1 and digits[lead] == '0': inc lead
  digits = digits[lead .. ^1]
  while digits.len > 1 and digits[^1] == '0':
    digits.setLen(digits.len - 1)
    inc exp
  result = if neg: "-" else: ""
  if digits == "0": return result & "0.0"
  let length = digits.len
  let k = exp
  let kk = length + k
  if k >= 0 and kk <= 16:
    result.add digits & repeat('0', k) & ".0"
  elif kk > 0 and kk <= 16:
    result.add digits[0 ..< kk] & "." & digits[kk .. ^1]
  elif kk > -5 and kk <= 0:
    result.add "0." & repeat('0', -kk) & digits
  elif length == 1:
    result.add digits & "e" & $(kk - 1)
  else:
    result.add digits[0] & "." & digits[1 .. ^1] & "e" & $(kk - 1)

proc serdeString*(s: string): string =
  ## serde_json's quoted string.
  result = "\""
  for c in s:
    case c
    of '"': result.add "\\\""
    of '\\': result.add "\\\\"
    of '\n': result.add "\\n"
    of '\r': result.add "\\r"
    of '\t': result.add "\\t"
    of '\b': result.add "\\b"
    of '\f': result.add "\\f"
    of '\0' .. '\x07', '\x0B', '\x0E' .. '\x1F':
      result.add "\\u00" & toHex(ord(c), 2).toLowerAscii
    else: result.add c
  result.add '"'

proc jsonText*(v: JsonNode): string =
  ## serde_json's compact form of a value (for messages): keys sorted, ryu floats.
  if v.isNil: return "null"
  case v.kind
  of JNull: "null"
  of JBool: (if v.bval: "true" else: "false")
  of JInt: $v.num
  of JFloat: serdeFloat(v.fnum)
  of JString: serdeString(v.str)
  of JArray:
    var parts: seq[string]
    for x in v.elems: parts.add jsonText(x)
    "[" & parts.join(",") & "]"
  of JObject:
    var parts: seq[string]
    for k, x in v.sortedPairs: parts.add serdeString(k) & ":" & jsonText(x)
    "{" & parts.join(",") & "}"

proc u32Id*(cmd, key: string; v: JsonNode): uint32 =
  ## An id from a JSON value: a bad-params error unless a whole number in u32 range.
  let n = v.asU64
  if n.isNone or n.get > uint64(high(uint32)):
    raise bad(cmd, "`" & key & "` = " & jsonText(v) & " is not a valid id (0..=" & $high(uint32) & ")")
  uint32(n.get)

proc u32IdParam*(cmd: string; p: JsonNode; key: string): Option[uint32] =
  let v = p.pget(key)
  if v.isNil: none(uint32) else: some(u32Id(cmd, key, v))

proc parseHex*(s: string): Option[array[4, float32]] =
  var t = s
  while t.startsWith("#"): t = t[1 .. ^1]
  template b(i: int): float32 =
    (if i + 2 > t.len: return none(array[4, float32])
     else:
       var v = 0
       for c in t[i ..< i + 2]:
         let d = case c
           of '0'..'9': ord(c) - ord('0')
           of 'a'..'f': ord(c) - ord('a') + 10
           of 'A'..'F': ord(c) - ord('A') + 10
           else: return none(array[4, float32])
         v = v * 16 + d
       float32(v) / 255'f32)
  case t.len
  of 6: some([b(0), b(2), b(4), 1'f32])
  of 8: some([b(0), b(2), b(4), b(6)])
  else: none(array[4, float32])

proc colorParam*(p: JsonNode; key: string; default: array[4, float32]): array[4, float32] =
  let v = p.pget(key)
  if v.isArray and v.len >= 3:
    var c: seq[float32]
    for x in v.elems: c.add float32(x.asF64.get(0.0))
    return [c[0], c[1], c[2], (if c.len > 3: c[3] else: 1'f32)]
  if not v.isNil and v.kind == JString:
    let h = parseHex(v.str)
    return if h.isSome: h.get else: default
  default

proc withField*(p: JsonNode; key: string; v: JsonNode): JsonNode =
  ## A copy of params `p` (an object, or null) with `key` set.
  result = newJObject()
  if p.isObject:
    for k, x in p.fields: result[k] = x
  result[key] = v

# ---------------------------------------------------------------- jobs (inline)

proc newJobCtx*(): JobCtx = JobCtx(shared: JobShared())

proc progress*(c: JobCtx; fraction: float32; message: string) =
  if c.shared.isNil: return
  if fraction == fraction and abs(fraction) != Inf:
    c.shared.progress = max(c.shared.progress, clamp(fraction, 0'f32, 1'f32))
  if message.len > 0: c.shared.message = message

proc cancelled*(c: JobCtx): bool = not c.shared.isNil and c.shared.cancel
proc cancel*(c: JobCtx) =
  if not c.shared.isNil: c.shared.cancel = true
proc check*(c: JobCtx) =
  if c.cancelled: raise cancelledErr()
proc fraction*(c: JobCtx): float32 = (if c.shared.isNil: 0'f32 else: c.shared.progress)
proc message*(c: JobCtx): string = (if c.shared.isNil: "" else: c.shared.message)

proc interrupt*(c: JobCtx; lo, hi: float32; message: string): Interrupt =
  ## An algorithm-level interrupt whose progress maps onto `lo..hi` of this job.
  c.progress(lo, message)
  let shared = c.shared
  Interrupt(
    cancelFn: proc(): bool = not shared.isNil and shared.cancel,
    progressFn: proc(f: float32) =
      if not shared.isNil:
        let v = lo + (hi - lo) * f
        if v == v: shared.progress = max(shared.progress, clamp(v, 0'f32, 1'f32)))

# ---------------------------------------------------------------- documents

proc newDocState*(doc: Document; path: Option[string]): DocState =
  let active = doc.topLayer
  result = DocState(doc: DocRef(), history: newHistory(), activeLayer: active, path: path,
                    revision: 1, savedRevision: 1, nextSnapshot: 1)
  result.doc[] = doc
  result.history.setCurrentLayers(LayerTarget(active: active, selected: (if active.isSome: @[active.get] else: @[])))
  if active.isSome:
    result.selectedLayers = @[active.get]
    result.layerAnchor = active

proc layerIds*(d: Document): seq[LayerId] =
  ## Every layer id, bottom to top in tree order (`walk` without the layers).
  for (_, _, l) in d.walk: result.add l.id

proc selectedOrdered*(st: DocState): seq[LayerId] =
  ## The selected layers in bottom-to-top document order, always including the active layer.
  if st.activeLayer.isNone: return
  let active = st.activeLayer.get
  let useSet = active in st.selectedLayers
  for id in st.doc[].layerIds:
    if (useSet and id in st.selectedLayers) or (not useSet and id == active): result.add id

proc isLayerSelected*(st: DocState; id: LayerId): bool =
  st.activeLayer == some(id) or
    (st.activeLayer.isSome and st.activeLayer.get in st.selectedLayers and id in st.selectedLayers)

proc isDirty*(st: DocState): bool = st.revision != st.savedRevision

proc layerTarget(st: DocState): LayerTarget = LayerTarget(active: st.activeLayer, selected: st.selectedLayers)

const MaskColors* = [[1'f32, 1, 1, 1], [0'f32, 0, 0, 1]]
  ## Photoshop's colour pair for a layer mask that has none yet: white on black.

proc newToolState*(): ToolState =
  result = ToolState(foreground: [0'f32, 0, 0, 1], background: [1'f32, 1, 1, 1], brush: defaultBrush(),
                     presets: builtin(), otherColors: MaskColors)
  # Photoshop's Brush tool starts at 10 % Smoothing.
  result.brush.smoothing.amount = 0.1

proc targetMask*(t: var ToolState; mask: bool): bool =
  ## Switches the edit target between layer pixels and a layer mask, swapping colour pairs.
  if t.maskTargeted == mask: return false
  t.maskTargeted = mask
  let other = t.otherColors
  t.otherColors = [t.foreground, t.background]
  t.foreground = other[0]
  t.background = other[1]
  true

proc newPatternLibrary*(): PatternLibrary
proc defaultColorSettings*(): ColorSettings

proc newSession*(): Session =
  Session(tools: newToolState(), quickMaskOptions: QuickMaskOptions(color: AlphaDefaultColor, opacity: 0.5,
          indicates: ciMaskedAreas), patterns: newPatternLibrary(),
          color: ColorState(settings: defaultColorSettings()))

proc documents*(s: Session): lent seq[DocState] = s.docs
proc activeIndex*(s: Session): Option[int] = s.activeIdx

proc active*(s: Session): ptr DocState =
  ## The active document's state, or nil.
  if s.activeIdx.isSome and s.activeIdx.get < s.docs.len: unsafeAddr s.docs[s.activeIdx.get] else: nil

proc activeOrRaise*(s: Session): ptr DocState =
  result = s.active
  if result.isNil: raise noDocument()

proc docAt*(s: Session; i: int): ptr DocState =
  if i >= 0 and i < s.docs.len: unsafeAddr s.docs[i] else: nil

proc findLayer*(d: Document; id: LayerId): ptr Layer =
  ## The layer with `id`, or nil (a pointer into `d`, valid until `d` changes).
  d.unsafeAddr[].layerPtr(id)

proc activeLayerOf*(s: Session): (ptr Layer, string) =
  ## The active document's active layer, or nil and why not (for `enabled` predicates).
  let st = s.active
  if st.isNil: return (nil, "no document open")
  if st.activeLayer.isNone: return (nil, "no active layer")
  let l = st.doc[].findLayer(st.activeLayer.get)
  if l.isNil: (nil, "no active layer") else: (l, "")

proc moveItem*[T](v: var seq[T]; i, to: int): Option[int] =
  ## Moves item `i` to position `to`, clamped to the end.
  if i < 0 or i >= v.len: return
  let x = v[i]
  v.delete(i)
  let t = clamp(to, 0, v.len)
  v.insert(x, t)
  some(t)

proc takeSamplersRemoved*(s: var Session): int =
  result = s.samplersRemoved
  s.samplersRemoved = 0

proc setSamplerMap*(s: var Session; a: Affine) = s.samplerMap = some(a)

proc fixView*(st: var DocState)
proc fixSelection*(st: var DocState)
proc restrictToColor*(before: Document; after: var Document; id: LayerId; k: int)
proc samePixels*(a, b: Document): bool
proc stepDamage*(a, b: Document): Option[Rect]
proc onCloseDocument(s: var Session; index: int)

proc syncToolsMaskTarget*(s: var Session) =
  let st = s.active
  let mask = not st.isNil and st.channelView.target.kind == ctLayerMask
  discard s.tools.targetMask(mask)

proc setActive*(s: var Session; index: int): bool =
  if index >= 0 and index < s.docs.len:
    s.activeIdx = some(index)
    s.syncToolsMaskTarget()
    true
  else: false

proc moveDocument*(s: var Session; src, to: int): Option[int] =
  let activeId = (if s.active.isNil: none(DocId) else: some(s.active.doc.id))
  result = moveItem(s.docs, src, to)
  if result.isNone: return
  s.activeIdx = none(int)
  if activeId.isSome:
    for i, d in s.docs:
      if d.doc.id == activeId.get: s.activeIdx = some(i)

proc addDocument*(s: var Session; doc: sink Document; path = none(string)): int =
  ## Adds a document and makes it active; its id is replaced when another open document has it.
  var d = doc
  while true:
    var clash = false
    for st in s.docs:
      if st.doc.id == d.id: clash = true
    if not clash: break
    d.id = freshDocId()
  s.docs.add newDocState(d, path)
  s.activeIdx = some(s.docs.len - 1)
  s.docs.len - 1

proc close*(s: var Session; index: int): Option[DocState] =
  if index < 0 or index >= s.docs.len: return
  s.onCloseDocument(index)
  result = some(s.docs[index])
  s.docs.delete(index)
  if s.docs.len == 0: s.activeIdx = none(int)
  else:
    let a = s.activeIdx
    s.activeIdx = some(
      if a.isSome and a.get > index: a.get - 1
      elif a.isSome and a.get < index: a.get
      else: min(index, s.docs.len - 1))

proc touch*(st: var DocState) =
  ## Bumps the revision for a view-only change without dirtying a clean document.
  let clean = st.savedRevision == st.revision
  inc st.revision
  if clean: st.savedRevision = st.revision
  st.lastDamage = none(Rect)

proc edit*(s: var Session; label: string; f: proc(doc: var Document; active: var Option[LayerId])) =
  ## Applies an undoable edit to the active document.
  let restrict = s.colorRestrict
  let samplerMap = s.samplerMap
  s.samplerMap = none(Affine)
  let st = s.activeOrRaise()
  let before = st.doc
  # Selecting layers is not a step: the state this edit leaves targets what was selected.
  let prior = st[].layerTarget
  var doc = before[]
  var active = st.activeLayer
  f(doc, active)
  if restrict.isSome and active.isSome: restrictToColor(before[], doc, active.get, restrict.get)
  let nd = DocRef()
  nd[] = move doc
  st.doc = nd
  st.activeLayer = active
  fixSelection(st[])
  fixView(st[])
  var removed = 0
  let bounds = st.doc[].bounds
  if samplerMap.isSome:
    let a = samplerMap.get
    let n = st.colorSamplers.len
    var kept: seq[array[2, float64]]
    for p in st.colorSamplers:
      let q = a.apply(point(p[0], p[1]))
      if bounds.contains(asI32(floor(q.x)), asI32(floor(q.y))): kept.add [q.x, q.y]
    st.colorSamplers = kept
    removed = n - kept.len
  elif st.doc.size != before.size:
    let n = st.colorSamplers.len
    var kept: seq[array[2, float64]]
    for p in st.colorSamplers:
      if bounds.contains(asI32(floor(p[0])), asI32(floor(p[1]))): kept.add p
    st.colorSamplers = kept
    removed = n - kept.len
  # An edit that changes no pixels leaves the canvas as it is.
  let unchanged = samePixels(before[], st.doc[])
  let key = s.coalesceRequest
  let layers = st[].layerTarget
  if key.isNone or st.coalesce != key or not st.history.canUndo:
    st.history.setCurrentLayers(prior)
    st.history.record(label, before, layers)
    discard st.history.trim(st.doc[])
  else:
    st.history.setCurrentLayers(layers)
  st.coalesce = key
  inc st.revision
  st.lastDamage = if unchanged: some(EmptyRect) else: none(Rect)
  s.samplersRemoved += removed

proc editWith*[R](s: var Session; label: string; f: proc(doc: var Document; active: var Option[LayerId]): R): R =
  ## `edit` whose body returns a value.
  var res: R
  s.edit(label, proc(doc: var Document; active: var Option[LayerId]) = res = f(doc, active))
  res

proc selectLayer*(s: var Session; id: LayerId) =
  ## Makes `id` the active layer without a history step.
  let st = s.activeOrRaise()
  if st.doc[].findLayer(id).isNil: raise noLayer(id)
  st.activeLayer = some(id)
  st.selectedLayers = @[id]
  st.layerAnchor = some(id)
  fixView(st[])
  let clean = st.savedRevision == st.revision
  inc st.revision
  if clean: st.savedRevision = st.revision
  st.lastDamage = some(EmptyRect)

proc fixSelection*(st: var DocState) =
  ## Keeps the layer selection valid after the document or the active layer changed.
  if st.activeLayer.isNone:
    st.selectedLayers.setLen(0)
    return
  let active = st.activeLayer.get
  if active notin st.selectedLayers:
    st.selectedLayers = @[active]
  else:
    let live = st.doc[].layerIds
    var kept: seq[LayerId]
    for id in st.selectedLayers:
      if id in live and id notin kept: kept.add id
    st.selectedLayers = kept
  if st.layerAnchor.isSome and st.layerAnchor.get notin st.selectedLayers:
    st.layerAnchor = some(active)

proc fixActive(st: var DocState) =
  if st.activeLayer.isNone or st.doc[].findLayer(st.activeLayer.get).isNil:
    st.activeLayer = st.doc[].topLayer
  fixSelection(st)
  fixView(st)

proc restoreTarget(st: var DocState; layers: LayerTarget) =
  ## Undo / redo: target the layers the restored state targeted, as far as they still exist.
  if layers.active.isNone and layers.selected.len == 0:
    st.activeLayer = none(LayerId)
    st.selectedLayers.setLen(0)
    st.layerAnchor = none(LayerId)
    fixView(st)
    return
  if layers.active.isSome and not st.doc[].findLayer(layers.active.get).isNil:
    st.activeLayer = layers.active
    st.selectedLayers = layers.selected
  fixActive(st)

proc activeJobBusy(s: Session): bool = false
  ## A background job running on the active document (never, while jobs run inline).

proc undo*(s: var Session): bool =
  if s.activeJobBusy: return false
  let st = s.active
  if st.isNil: return false
  st.coalesce = none(string)
  let r = st.history.undo(st.doc)
  if r.isNone: return false
  var (d, layers) = r.get
  # Save As changes file identity outside history, just like the saved path.
  if d.name != st.doc.name:
    let c = DocRef()
    c[] = d[]
    c.name = st.doc.name
    d = c
  let damage = stepDamage(d[], st.doc[])
  st.doc = d
  restoreTarget(st[], layers)
  inc st.revision
  st.lastDamage = damage
  true

proc redo*(s: var Session): bool =
  if s.activeJobBusy: return false
  let st = s.active
  if st.isNil: return false
  st.coalesce = none(string)
  let r = st.history.redo(st.doc)
  if r.isNone: return false
  var (d, layers) = r.get
  if d.name != st.doc.name:
    let c = DocRef()
    c[] = d[]
    c.name = st.doc.name
    d = c
  let damage = stepDamage(st.doc[], d[])
  st.doc = d
  restoreTarget(st[], layers)
  inc st.revision
  st.lastDamage = damage
  true

# ---------------------------------------------------------------- jobs

proc runJob*[T](s: var Session; label: string; lockDocument: bool; work: proc(ctx: JobCtx): T;
                apply: proc(s: var Session; t: T): JsonNode): JsonNode =
  ## A job-capable command's heavy part: `work` then `apply`. This port runs both inline.
  let ctx = if s.jobs.inline.isSome: s.jobs.inline.get else: newJobCtx()
  let t = work(ctx)
  apply(s, t)

proc editJob*[R](s: var Session; label: string;
                 f: proc(doc: var Document; active: var Option[LayerId]; ctx: JobCtx): R;
                 finish: proc(r: R): JsonNode): JsonNode =
  ## A document edit whose body is the job: exactly `edit` (one undo step named `label`).
  let ctx = if s.jobs.inline.isSome: s.jobs.inline.get else: newJobCtx()
  let r = s.editWith(label, proc(doc: var Document; active: var Option[LayerId]): R = f(doc, active, ctx))
  finish(r)

proc setInlineJobCtx*(s: var Session; ctx: Option[JobCtx]) = s.jobs.inline = ctx

proc hasJobs*(s: Session): bool = false
proc jobsWithRecent*(s: Session): seq[JobInfo] = s.jobs.recent
proc pollJobs*(s: var Session): seq[JobEvent] =
  result = s.jobs.events
  s.jobs.events.setLen(0)
proc cancelJob*(s: var Session; id: JobId): bool = false
proc cancelAllJobs*(s: var Session) = discard
proc cancelJobsOn(s: var Session; doc: DocId) = discard

proc jobConflict(s: Session; id: string; journal: bool): string = ""
  ## Why `id` can't run because of a background job ("" = no conflict; never, inline).

# ---------------------------------------------------------------- registry and dispatch

var registry: seq[CommandSpec]
var registryIndex: Table[string, int]

proc installRegistry*(specs: seq[CommandSpec]) =
  ## Installs the command table (the `engine` module builds it in the Rust order). The first
  ## spec wins when an id is duplicated, as in the Rust registry.
  registry = specs
  registryIndex = initTable[string, int](specs.len * 2)
  for i, spec in specs:
    if spec.id notin registryIndex: registryIndex[spec.id] = i

proc commandSpecs*(): lent seq[CommandSpec] = registry

proc findSpec*(id: string): ptr CommandSpec =
  ## The command `id`, or nil.
  let i = registryIndex.getOrDefault(id, -1)
  if i >= 0: unsafeAddr registry[i] else: nil

proc injectTarget*(s: Session; id: string; params: JsonNode): JsonNode
proc maskTargetEnabled(s: Session; id: string; p: JsonNode): Option[string]
proc colorRestriction(s: Session; id: string; params: JsonNode): Option[int]
proc injectKind(id: string; params: JsonNode): JsonNode
proc beforeCommandHook(s: var Session; id: string): Option[JsonNode]
proc validateCommandParams(s: var Session; id: string; params: JsonNode)
proc redirectCommand(s: var Session; id: string): Option[(string, JsonNode)]
proc hiddenTargetRefusal(s: Session; id: string; params: JsonNode): string
proc afterCommandHooks(s: var Session; id: string)
proc replayable(id: string): bool
proc coalesceJournal(s: var Session; id: string; params: JsonNode): bool

proc namesExistingLayer(s: Session; id: string; params: JsonNode): bool =
  ## A layer command whose `"layer"` param names an existing layer needs no active layer.
  if id notin ["layer.hideLayers", "layer.showLayers", "layer.renameLayer"]: return false
  let l = params.pget("layer").asU64
  let st = s.active
  l.isSome and not st.isNil and not st.doc[].findLayer(LayerId(l.get)).isNil

proc precondition(s: Session; spec: CommandSpec; params: JsonNode): string =
  if s.namesExistingLayer(spec.id, params): return ""
  let m = s.maskTargetEnabled(spec.id, params)
  if m.isSome: m.get else: spec.enabled(s)

proc explicitLayerEnabled(s: var Session; spec: CommandSpec; params: JsonNode): Option[string] =
  ## The precondition of a command documenting `"layer":id`, checked on that layer when the
  ## params name an existing one (it stays inactive).
  if "\"layer\":id" notin spec.params: return
  let l = params.pget("layer").asU64
  if l.isNone: return
  let st = s.active
  if st.isNil or st.doc[].findLayer(LayerId(l.get)).isNil: return
  let prev = st.activeLayer
  st.activeLayer = some(LayerId(l.get))
  let r = spec.enabled(s)
  st.activeLayer = prev
  some(r)

proc disabledReasonWith*(s: Session; id: string; params: JsonNode): Option[string] =
  ## Why the command can't run now with `params` (none = it can).
  let spec = findSpec(id)
  if spec.isNil: return some("unknown command `" & id & "`")
  let why = s.precondition(spec[], s.injectTarget(id, params))
  if why.len > 0: return some(why)
  let j = s.jobConflict(id, spec.journal)
  if j.len > 0: some(j) else: none(string)

proc disabledReason*(s: Session; id: string): Option[string] = s.disabledReasonWith(id, newJNull())
proc isEnabledWith*(s: Session; id: string; params: JsonNode): bool = s.disabledReasonWith(id, params).isNone
proc isEnabled*(s: Session; id: string): bool = s.isEnabledWith(id, newJNull())

proc afterCommand(s: var Session; id: string; params: JsonNode; journal: bool; nestedFrom: Option[int]) =
  ## Bookkeeping after a command succeeded.
  s.afterCommandHooks(id)
  if not journal: return
  if nestedFrom.isSome and replayable(id): s.journal.setLen(nestedFrom.get)
  if not s.coalesceJournal(id, params): s.journal.add (id, params)

proc dispatch(s: var Session; id: string; params: JsonNode; background: bool): Started =
  let spec = findSpec(id)
  if spec.isNil: raise unknownCommand(id)
  let params = if params.isNil: newJNull() else: params
  # Params are named fields: anything but an object (or null) is a caller mistake.
  if not (params.kind == JObject or params.kind == JNull):
    raise bad(id, "params must be a JSON object")
  # Untrusted sessions gate every command, including nested ones.
  if not s.authorize.isNil: s.authorize(id, params)
  let redirect = s.redirectCommand(id)
  if redirect.isSome and redirect.get[0] != id:
    return s.dispatch(redirect.get[0], redirect.get[1], background)
  s.validateCommandParams(id, params)
  let early = s.beforeCommandHook(id)
  if early.isSome: return Started(kind: stDone, value: early.get)
  let runParams = s.injectTarget(id, injectKind(id, params))
  let ex = s.explicitLayerEnabled(spec[], params)
  let why = if ex.isSome: ex.get else: s.precondition(spec[], runParams)
  if why.len > 0: raise disabledErr(id, why)
  let conflict = s.jobConflict(id, spec.journal)
  if conflict.len > 0: raise disabledErr(id, conflict)
  let refusal = s.hiddenTargetRefusal(id, params)
  if refusal.len > 0: raise other(refusal)
  s.coalesceRequest = params.pget("coalesce").asStr
  s.colorRestrict = s.colorRestriction(id, runParams)
  s.jobs.spawn = background
  let nestedFrom = s.journal.len
  var r: JsonNode
  try:
    r = spec.run(s, runParams)
  except EngineError:
    raise
  except CatchableError as e:
    # An error another layer raised that the command did not convert (Rust converts each one
    # at its call site, mostly as `Other(e.to_string())`).
    raise other(e.msg)
  except Defect:
    # Last-resort guard (AGENTS.md, Never crash): `edit` only commits after its body returns,
    # so the documents are unchanged.
    raise other("`" & id & "` failed with an internal error (logged); the document is unchanged")
  finally:
    s.jobs.spawn = false
    s.coalesceRequest = none(string)
    s.colorRestrict = none(int)
  s.afterCommand(id, params, spec.journal, some(nestedFrom))
  Started(kind: stDone, value: (if r.isNil: newJNull() else: r))

proc execute*(s: var Session; id: string; params: JsonNode = nil): JsonNode =
  ## Runs a command by id with JSON params and returns its JSON result. Raises `EngineError`.
  s.dispatch(id, params, false).value

proc start*(s: var Session; id: string; params: JsonNode = nil): Started =
  ## Runs a command, in the background when it supports it (inline in this port).
  s.dispatch(id, params, true)
