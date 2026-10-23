## Headless caret geometry for type layers (port of `type_caret_cmds.rs`): `type.hitTest`,
## `type.caret`, `type.navigate`.
##
## Queries (`journal: false`, no history step). Positions are document pixels, so an agent never
## touches text space; offsets are character indices, the same unit as `type.edit` and
## `type.setStyle`. The Type tool calls the same word, line and hit-test helpers.

import std/[json, options, math, unicode]
import ./core
import ../text
import ../text/layout as tlayout
import ../rfmt

proc caretHasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")

proc reqF64(p: JsonNode; key, cmd: string): float64 =
  let v = p.pget(key)
  if v.isNil: raise bad(cmd, "missing `" & key & "`")
  let n = v.asF64
  if n.isNone or n.get.classify in {fcNan, fcInf, fcNegInf}: raise bad(cmd, "`" & key & "` must be a finite number")
  n.get

proc optF64(p: JsonNode; key, cmd: string): Option[float64] =
  ## `x` when present. Absent is fine; a present non-number is not.
  let v = p.pget(key)
  if v.isNil or v.kind == JNull: none(float64) else: some(reqF64(p, key, cmd))

proc reqIndex(p: JsonNode; key, cmd: string): int =
  let v = p.pget(key)
  if v.isNil: raise bad(cmd, "missing `" & key & "`")
  let n = v.asU64
  if n.isNone: raise bad(cmd, "`" & key & "` must be a character index")
  int(n.get)

proc checkIndex(text: string; index: int; cmd: string) =
  let n = text.runeLen
  if index > n: raise bad(cmd, "index " & $index & " is past the end of the text (" & $n & " characters)")

proc resolveLayer(s: Session; p: JsonNode; cmd: string): LayerId =
  let v = p.pget("layer")
  if not v.isNil and v.kind != JNull:
    let id = v.asU64
    if id.isNone: raise bad(cmd, "`layer` must be a layer id")
    return LayerId(id.get)
  let d = s.active
  if d.isNil or d.activeLayer.isNone: raise other("no active layer")
  d.activeLayer.get

type Frame = object
  ## A type layer laid out once, plus the bits of it the queries need.
  id: LayerId
  text: string
  transform: Affine
  ink: Option[Rect]   ## rendered ink in document pixels (empty text has none)
  layout: TextLayout

proc frame(doc: Document; id: LayerId; cmd: string): Frame =
  let l = doc.findLayer(id)
  if l.isNil: raise noLayer(id)
  if l[].content.kind != lkText:
    raise bad(cmd, "layer " & $uint64(id) & " is " & l[].content.article & " " & l[].content.kindName & " layer, not a type layer")
  let t = l[].content.text
  var ink = none(Rect)
  if t.cache.isSome:
    let r = t.cache.get.contentBounds
    if not r.isEmpty: ink = some(r)
  Frame(id: id, text: t.text, transform: t.transform, ink: ink, layout: shared().layout(t, doc.resolutionDpi))

proc toTextSpace(aff: Affine; x, y: float64): (float32, float32) =
  let q = aff.inverse.get(IdentityAffine).apply(point(x, y))
  (float32(q.x), float32(q.y))

proc docPoint(aff: Affine; x, y: float32; cmd: string): JsonNode =
  let q = aff.apply(point(float64(x), float64(y)))
  if q.x.classify in {fcNan, fcInf, fcNegInf} or q.y.classify in {fcNan, fcInf, fcNegInf}:
    raise bad(cmd, "the caret is not a finite point")
  %*[q.x, q.y]

proc contains(f: Frame; x, y: float64): bool =
  ## Same hit as the Type tool, without its zoom slop: the laid-out line boxes or the rendered ink.
  let (tx, ty) = toTextSpace(f.transform, x, y)
  textPointInside(f.layout, tx, ty, 0) or
    (f.ink.isSome and x >= float64(f.ink.get.x0) and x <= float64(f.ink.get.x1) and
     y >= float64(f.ink.get.y0) and y <= float64(f.ink.get.y1))

proc topmost(doc: Document; x, y: float64): Option[Frame] =
  ## Topmost visible type layer under the point. `walk` is bottom-up; the Type tool hits the top first.
  var ids: seq[LayerId]
  for (path, _, l) in doc.walk:
    if l.content.kind != lkText: continue
    var shown = true
    for n in 1 .. path.len:
      let a = doc.layerAt(path[0 ..< n])
      if a.isNone or not a.get.visible:
        shown = false
        break
    if shown: ids.add l.id
  for i in countdown(ids.high, 0):
    var f: Frame
    try: f = frame(doc, ids[i], "type.hitTest")
    except EngineError: continue
    if f.contains(x, y): return some(f)

proc column(layout: TextLayout; aff: Affine; text: string; index: int; docX: Option[float64]): float32 =
  ## Line-space column for a document x. Translate and scale keep that x as the same column on
  ## every line; when a rotation makes document x independent of the column, the caret's own
  ## column is kept (there is nothing to solve for).
  let (x, top, bottom) = layout.caret(byteIndex(text, index))
  if docX.isNone: return x
  let y = float64(top + bottom) * 0.5
  let (a, c, e) = (aff.m[0], aff.m[2], aff.m[4])
  # Horizontal: text = (line_x, line_y). Vertical: text = (-line_y, line_x).
  let (coef, along) = (if layout.vertical: (c, docX.get - e + a * y) else: (a, docX.get - e - c * y))
  if abs(coef) < 1e-8: return x
  let solved = along / coef
  if solved.classify in {fcNan, fcInf, fcNegInf}: x else: float32(solved)

proc hitTestCmd(s: Session; p: JsonNode): JsonNode =
  const cmdId = "type.hitTest"
  let x = reqF64(p, "x", cmdId)
  let y = reqF64(p, "y", cmdId)
  let st = s.active
  if st.isNil: raise noDocument()
  let doc = st.doc[]
  let v = p.pget("layer")
  var f: Frame
  if not v.isNil and v.kind != JNull:
    let id = v.asU64
    if id.isNone: raise bad(cmdId, "`layer` must be a layer id")
    f = frame(doc, LayerId(id.get), cmdId)
  else:
    let hit = topmost(doc, x, y)
    if hit.isNone: raise bad(cmdId, "no type layer under the point")
    f = hit.get
  let (tx, ty) = toTextSpace(f.transform, x, y)
  let (index, line) = hitChar(f.layout, f.text, tx, ty)
  %*{"layer": uint64(f.id), "index": index, "line": line, "inside": f.contains(x, y)}

proc caretCmd(s: Session; p: JsonNode): JsonNode =
  const cmdId = "type.caret"
  let index = reqIndex(p, "index", cmdId)
  let id = resolveLayer(s, p, cmdId)
  let st = s.active
  if st.isNil: raise noDocument()
  let f = frame(st.doc[], id, cmdId)
  checkIndex(f.text, index, cmdId)
  let b = byteIndex(f.text, index)
  let seg = f.layout.caretSegment(b)
  let p0 = docPoint(f.transform, seg[0][0], seg[0][1], cmdId)
  let p1 = docPoint(f.transform, seg[1][0], seg[1][1], cmdId)
  %*{"index": index, "line": lineIndex(f.layout, b), "segment": [p0, p1]}

proc navigateCmd(s: Session; p: JsonNode): JsonNode =
  const cmdId = "type.navigate"
  let index = reqIndex(p, "index", cmdId)
  let mv = p.pget("move").asStr
  if mv.isNone: raise bad(cmdId, "missing `move`")
  let docX = optF64(p, "x", cmdId)
  let id = resolveLayer(s, p, cmdId)
  let st = s.active
  if st.isNil: raise noDocument()
  let f = frame(st.doc[], id, cmdId)
  checkIndex(f.text, index, cmdId)
  let next = case mv.get
    of "wordPrev": wordBoundary(f.text, index, false)
    of "wordNext": wordBoundary(f.text, index, true)
    of "linePrev", "lineNext":
      let dir = (if mv.get == "linePrev": -1 else: 1)
      lineStep(f.layout, f.text, index, column(f.layout, f.transform, f.text, index, docX), dir)
    of "lineStart": lineEdge(f.layout, f.text, index, false)
    of "lineEnd": lineEdge(f.layout, f.text, index, true)
    of "start": 0
    of "end": f.text.runeLen
    else:
      raise bad(cmdId, "move " & debugStr(mv.get) & " must be wordPrev, wordNext, linePrev, lineNext, lineStart, lineEnd, start or end")
  %*{"index": next}

proc typeCaretSpecs*(): seq[CommandSpec] =
  @[cmd("type.hitTest", "Hit-Test Type", [], "",
        """{"layer":id?, "x":px, "y":px} → {"layer","index":char,"line","inside":bool}. No layer: the topmost visible type layer whose laid-out text or rendered pixels contain the point (same order as the Type tool); a miss is an error. With a layer, index is the nearest caret even when inside is false""",
        enabledProc(caretHasDocE(s)), runProc(hitTestCmd(s, p)), journal = false),
    cmd("type.caret", "Type Caret", [], "",
        """{"layer":id?, "index":char (0..=length; empty text is 0)} → {"index","line","segment":[[x,y],[x,y]]} caret segment in document pixels, for rotated and vertical type too""",
        enabledProc(caretHasDocE(s)), runProc(caretCmd(s, p)), journal = false),
    cmd("type.navigate", "Navigate Type", [], "",
        """{"layer":id?, "index":char, "move":"wordPrev|wordNext|linePrev|lineNext|lineStart|lineEnd|start|end", "x":px?} → {"index"}. Words are alphanumeric runs. x is the document x to keep across linePrev/lineNext (a caret segment's x); omitted uses the caret's own column. Empty text returns index 0""",
        enabledProc(caretHasDocE(s)), runProc(navigateCmd(s, p)), journal = false)]
