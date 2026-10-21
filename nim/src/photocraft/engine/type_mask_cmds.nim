## `type.createSelection` (port of `type_mask_cmds.rs`): the Horizontal and Vertical Type Mask
## tools' result. Typed text becomes a selection in the shape of its glyphs (their anti-aliased
## coverage) instead of a type layer.
##
## The tools edit a temporary type layer in one coalesced history step and commit with
## `{"layer": id, "removeLayer": true}`, so the whole session is one undo step that leaves no layer
## behind. Scripts can skip the layer and pass `type.create`'s keys directly.

import std/[json, options]
import ./core
import ../text
import ../algo/selection as asel

const TypeMaskCmd = "type.createSelection"

proc tbad(msg: string): ref EngineError = bad(TypeMaskCmd, msg)

proc antialiasParam(p: JsonNode): Option[AntiAlias] =
  let v = p.pget("antialias")
  if v.isNil: return none(AntiAlias)
  if v.kind == JString:
    case v.str
    of "none": return some(aaNone)
    of "sharp": return some(aaSharp)
    of "crisp": return some(aaCrisp)
    of "strong": return some(aaStrong)
    of "smooth": return some(aaSmooth)
    else: discard
  raise tbad("antialias must be none, sharp, crisp, strong or smooth")

proc glyphCoverage(doc: Document; t0: TextLayer): (seq[float32], Rect) =
  ## Glyph coverage of `t0` (0..=1 per pixel) over its rendered bounds inside the canvas. The text
  ## colour's own alpha doesn't thin the selection: every run renders opaque.
  var t = t0
  for r in t.runs.mitems: r.style.color.alpha = 1
  let surface = shared().render(t, doc.resolutionDpi, doc.pixelFormat)[1].surface
  let area = surface.contentBounds.intersect(doc.bounds)
  if area.isEmpty: return (newSeq[float32](), EmptyRect)
  var px = newSeq[array[4, float32]](area.width * area.height)
  surface.readRgbaInto(area, px)
  var m = newSeq[float32](px.len)
  for i, q in px: m[i] = q[3]
  (m, area)

proc widen(m: seq[float32]; area, to: Rect): seq[float32] =
  ## Coverage of `area`'s mask spread over `to` (which contains `area`), zero elsewhere.
  if area == to: return m
  let (w, aw) = (to.width, area.width)
  result = newSeq[float32](w * to.height)
  let step = max(aw, 1)
  var y = 0
  while (y + 1) * step <= m.len:
    let start = (area.y0 - to.y0) * w + y * w + (area.x0 - to.x0)
    if start >= 0 and start + aw <= result.len:
      for k in 0 ..< aw: result[start + k] = m[y * step + k]
    inc y

proc createSelection(s: var Session; p: JsonNode): JsonNode =
  var mode = smReplace
  let ov = p.pget("operation")
  if not ov.isNil:
    let o = ov.asStr
    if o.isNone: raise tbad("operation must be new, add, subtract or intersect")
    case o.get
    of "new", "replace": mode = smReplace
    of "add": mode = smAdd
    of "subtract": mode = smSubtract
    of "intersect": mode = smIntersect
    else: raise tbad("operation must be new, add, subtract or intersect")
  let aa = antialiasParam(p)
  var remove = false
  let rv = p.pget("removeLayer")
  if not rv.isNil:
    let b = rv.asBool
    if b.isNone: raise tbad("removeLayer must be true or false")
    remove = b.get
  let st = s.active
  if st.isNil: raise noDocument()
  let doc = st.doc[]
  var layer = none(LayerId)
  let lv = p.pget("layer")
  if not lv.isNull:
    let id = lv.asU64
    if id.isNone: raise tbad("layer must be a layer id")
    layer = some(LayerId(id.get))
  if remove and layer.isNone: raise tbad("removeLayer needs a layer")
  var t: TextLayer
  if layer.isSome:
    let l = doc.findLayer(layer.get)
    if l.isNil: raise noLayer(layer.get)
    if l[].content.kind != lkText: raise tbad("layer " & $uint64(layer.get) & " is not a type layer")
    t = l[].content.text
  else: t = newTextLayer(s, p, TypeMaskCmd)
  if aa.isSome: t.antialias = aa.get
  let (mask, textArea) = glyphCoverage(doc, t)
  let selected = s.editWith("Type Mask", proc(doc: var Document; active: var Option[LayerId]): Option[Rect] =
    let old = (if doc.selection.isSome: doc.selection.get.contentBounds.intersect(doc.bounds) else: EmptyRect)
    # Replace and Intersect leave nothing outside the text; Add and Subtract keep the rest.
    let area =
      if mode in {smAdd, smSubtract} and not old.isEmpty: (if textArea.isEmpty: old else: old.union(textArea))
      else: textArea
    doc.selection = (if area.isEmpty: none(Surface) else: asel.combine(doc.selection, widen(mask, textArea, area), area, mode))
    if remove and layer.isSome:
      let id = layer.get
      let neighbours = deletionNeighbours(doc, id)
      if doc.remove(id).isNone: raise noLayer(id)
      if doc.layers.len == 0: raise other("a document must keep at least one layer")
      if active.isSome and doc.findLayer(active.get).isNil:
        active = none(LayerId)
        for c in neighbours:
          if not doc.findLayer(c).isNil:
            active = some(c)
            break
    if doc.selection.isSome: some(doc.selection.get.contentBounds) else: none(Rect))
  # The Type Mask tools' whole typing session is this one step.
  let st2 = s.active
  if not st2.isNil: st2.history.setCurrentLabel("Type Mask")
  %*{"selected": selected.isSome,
     "bounds": (if selected.isSome: %*[selected.get.x0, selected.get.y0, selected.get.x1, selected.get.y1] else: newJNull())}

proc typeMaskSpecs*(): seq[CommandSpec] =
  @[cmd(TypeMaskCmd, "Type Mask", [], "",
        """{"layer":id? (a type layer whose glyphs become the selection),"removeLayer":bool=false (delete that layer in the same step: the Type Mask tools' temporary text),"operation":"new|add|subtract|intersect"="new","antialias":"none|sharp|crisp|strong|smooth"?, …without "layer", type.create's keys: "text","x","y" or "box","orientation","font","size","align",…}""",
        enabledProc(if s.active.isNil: "no document open" else: ""), runProc(createSelection(s, p)))]
