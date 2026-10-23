## The Type menu (port of `type_extra_cmds.rs`): anti-aliasing, orientation, point ⇄ paragraph
## conversion, Create Work Path, Convert to Shape, Rasterize, Warp Text, the OpenType toggles,
## Paste Lorem Ipsum, Update All Text Layers, missing-font replacement and the default type styles.
##
## Everything edits the typed `TextLayer` model and re-renders through `refreshText`, so the PSD
## `TySh` data (and with it warp, anti-aliasing and the OpenType flags) is regenerated and the
## layer saves as editable text.

import std/[json, options, strutils, unicode, algorithm, sequtils, tables]
import ./core
import ../text
import ../text/render as trender
import ../text/warp as twarp
import ../text/fonts as tfonts
import ../text/layout as tlayout
import ../rmath

const LoremIpsum* = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
  ## Photoshop's Paste Lorem Ipsum filler.

# ---------------------------------------------------------------- predicates

proc hasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")

proc activeText(s: Session): (ptr TextLayer, string) =
  let d = s.active
  if d.isNil: return (nil, "no document open")
  if d.activeLayer.isSome:
    let l = d.doc[].findLayer(d.activeLayer.get)
    if not l.isNil and l[].content.kind == lkText: return (addr l[].content.text, "")
  (nil, "the active layer is not a type layer")

proc hasTextE(s: Session): string = activeText(s)[1]

proc hasPointTextE(s: Session): string =
  let (t, why) = activeText(s)
  if t.isNil: return why
  if t[].shape.kind == tsPoint: "" else: "the type layer is already paragraph text"

proc hasBoxTextE(s: Session): string =
  let (t, why) = activeText(s)
  if t.isNil: return why
  if t[].shape.kind == tsBox: "" else: "the type layer is already point text"

proc anyTextE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  for (_, _, l) in d.doc[].walk:
    if l.content.kind == lkText: return ""
  "the document has no type layers"

proc hasDefaultsE(s: Session): string =
  let why = hasFormatTargetE(s)
  if why.len > 0: return why
  if s.typeDefaults.isNone: "no default type styles have been saved" else: ""

# ---------------------------------------------------------------- helpers

proc withText[R](s: var Session; p: JsonNode; label: string; f: proc(t: var TextLayer; doc: Document): R): R =
  ## Edits a type layer as one history step and re-renders it.
  let id = layerParam(s, p)
  s.editWith(label, proc(doc: var Document; active: var Option[LayerId]): R =
    let snapshot = doc
    let l = doc.findLayer(id)
    if l.isNil: raise noLayer(id)
    let autoNamed = isAutoNamed(l[])
    if l[].content.kind != lkText:
      raise other("layer " & $uint64(id) & " is " & l[].content.article & " " & l[].content.kindName & " layer, not a type layer")
    result = f(l[].content.text, snapshot)
    refreshText(snapshot, l[].content.text)
    if autoNamed: l[].name = layerName(l[].content.text.text))

proc layoutOf(doc: Document; t: TextLayer): TextLayout = shared().layout(t, doc.resolutionDpi)

proc thenTranslate(m: Affine; tx, ty: float64): Affine =
  ## `m ∘ translate(tx, ty)`.
  let (a, b, c, d, e, f) = (m.m[0], m.m[1], m.m[2], m.m[3], m.m[4], m.m[5])
  Affine(m: [a, b, c, d, a * tx + c * ty + e, b * tx + d * ty + f])

proc firstAlign(t: TextLayer): TextAlign =
  let runs = t.paragraphRuns
  if runs.len > 0: runs[0].style.align else: defaultParagraphStyle().align

proc charIndex(text: string; byteAt: int): int = runeLen(text[0 ..< min(byteAt, text.len)])

proc byteIndex(text: string; ci: int): int =
  var n = 0
  var i = 0
  while i < text.len:
    if n == ci: return i
    i += runeLenAt(text, i)
    inc n
  text.len

# ---------------------------------------------------------------- anti-aliasing / orientation

proc aaName*(a: AntiAlias): string =
  case a
  of aaNone: "none"
  of aaSharp: "sharp"
  of aaCrisp: "crisp"
  of aaStrong: "strong"
  of aaSmooth: "smooth"
  of aaWindows: "windows"
  of aaWindowsLcd: "windowsLcd"

proc setAa(s: var Session; p: JsonNode; aa: AntiAlias): JsonNode =
  discard withTextTargets(s, p, "Anti-Alias", proc(t: var TextLayer; r: (int, int)) = t.antialias = aa)
  %*{"antialias": aaName(aa)}

proc setOrientation(s: var Session; p: JsonNode; o: Orientation): JsonNode =
  discard withTextTargets(s, p, "Change Text Orientation", proc(t: var TextLayer; r: (int, int)) = t.orientation = o)
  %*{"orientation": (if o == orVertical: "vertical" else: "horizontal")}

# ---------------------------------------------------------------- point ⇄ paragraph

proc toParagraph(s: var Session; p: JsonNode): JsonNode =
  let r = withText(s, p, "Convert to Paragraph Text", proc(t: var TextLayer; doc: Document): JsonNode =
    let l = layoutOf(doc, t)
    var maxSize = t.sizePt
    for run in t.charRuns: maxSize = rmax(maxSize, run.style.sizePt)
    let sizePx = maxSize * doc.resolutionDpi / 72'f32
    let pruns = t.paragraphRuns
    let ps = (if pruns.len > 0: pruns[0].style else: defaultParagraphStyle())
    let indents = rmax(ps.startIndentPt + ps.endIndentPt, 0'f32) * doc.resolutionDpi / 72'f32
    if l.vertical:
      # Line space: u along the columns, v across them (first column centred on v = 0).
      let lb = l.lineBounds.get([0'f32, -sizePx / 2, sizePx, sizePx / 2])
      let (u0, v0, u1, v1) = (lb[0], lb[1], lb[2], lb[3])
      let height = rmax(u1 - u0, 1'f32) + indents + rmax(sizePx * 0.5'f32, 4'f32)
      let width = rmax(v1 - v0, 1'f32) + sizePx * 0.5'f32
      let y = (case ps.align
               of taCenter, taJustifyCenter: -height / 2
               of taRight, taJustifyRight: -height
               else: 0'f32)
      # The box's right edge is the first column's right edge, so the columns stay put.
      let x = -v0 - width
      t.shape = TextShape(kind: tsBox, x: 0, y: 0, width: width, height: height)
      t.transform = thenTranslate(t.transform, float64(x), float64(y))
      return %*[x, y, width, height]
    let b = l.bounds.get([0'f32, -t.sizePt, t.sizePt, 0'f32])
    let (x0, y0, x1, y1) = (b[0], b[1], b[2], b[3])
    # A little slack so no line re-wraps; the box keeps the text where it was.
    let width = rmax(x1 - x0, 1'f32) + indents + rmax(sizePx * 0.5'f32, 4'f32)
    let height = rmax(y1 - y0, 1'f32) + sizePx * 0.5'f32
    let x = (case ps.align
             of taCenter, taJustifyCenter: -width / 2
             of taRight, taJustifyRight: -width
             else: 0'f32)
    var probe = t
    probe.shape = TextShape(kind: tsBox, x: x, y: y0, width: width, height: height)
    # Box text puts the first baseline at the box top plus the ascender height: shift the box so
    # that baseline stays at the point-text anchor (y = 0).
    let pl = layoutOf(doc, probe)
    let b0 = (if pl.lines.len > 0: pl.lines[0].baseline else: 0'f32)
    let y = y0 - b0
    t.shape = TextShape(kind: tsBox, x: 0, y: 0, width: width, height: height)
    t.transform = thenTranslate(t.transform, float64(x), float64(y))
    %*[x, y, width, height])
  %*{"box": r}

proc toPoint(s: var Session; p: JsonNode): JsonNode =
  let r = withText(s, p, "Convert to Point Text", proc(t: var TextLayer; doc: Document): int =
    if t.shape.kind != tsBox: raise other("already point text")
    let l = layoutOf(doc, t)
    # Vertical type: columns run along the box height.
    let (x, width) = (if l.vertical: (t.shape.y, t.shape.height) else: (t.shape.x, t.shape.width))
    let baseline = (if l.lines.len > 0: l.lines[0].baseline else: 0'f32)
    # Photoshop inserts a hard return at every soft line break.
    var breaks: seq[int]
    for i in 0 ..< max(l.lines.len - 1, 0):
      if l.lines[i].paragraph == l.lines[i + 1].paragraph: breaks.add l.lines[i + 1].range[0]
    breaks.sort()
    breaks = breaks.deduplicate(isSorted = true)
    for k in countdown(breaks.high, 0): replaceText(t, breaks[k], breaks[k], "\n")
    let ax = (case firstAlign(t)
              of taCenter, taJustifyCenter: x + width / 2
              of taRight, taJustifyRight: x + width
              else: x)
    t.shape = TextShape(kind: tsPoint)
    # `baseline` is in line space (it already includes the box edge).
    let (tx, ty) = l.toText(ax, baseline)
    t.transform = thenTranslate(t.transform, float64(tx), float64(ty))
    breaks.len)
  %*{"inserted": r}

# ---------------------------------------------------------------- outlines → paths

proc signedArea(knots: seq[Knot]): float64 =
  let n = knots.len
  for i in 0 ..< n:
    let (a, b) = (knots[i].anchor, knots[(i + 1) mod n].anchor)
    result += a.x * b.y - b.x * a.y
  result /= 2

proc contours(els: seq[PathEl]): seq[seq[Knot]] =
  ## One glyph's outline elements as closed subpaths.
  var cur: seq[Knot]
  proc pt(v: array[2, float64]): Point = Point(x: v[0], y: v[1])
  proc finish(cur: var seq[Knot]; outp: var seq[seq[Knot]]) =
    if cur.len >= 2:
      # The closing point duplicates the first anchor: fold its incoming handle in.
      let (f, l) = (cur[0], cur[^1])
      if abs(f.anchor.x - l.anchor.x) < 1e-9 and abs(f.anchor.y - l.anchor.y) < 1e-9:
        cur[0].inCtrl = l.inCtrl
        cur.setLen(cur.len - 1)
    if cur.len >= 2:
      outp.add cur
      cur = @[]
    else: cur.setLen(0)
  for el in els:
    case el.kind
    of peMove:
      finish(cur, result)
      cur.add cornerKnot(el.pts[0][0], el.pts[0][1])
    of peLine: cur.add cornerKnot(el.pts[0][0], el.pts[0][1])
    of peQuad:
      # Exact quadratic → cubic elevation.
      if cur.len == 0: continue
      let (c, a) = (el.pts[0], el.pts[1])
      let s0 = cur[^1].anchor
      cur[^1].outCtrl = Point(x: s0.x + 2.0 / 3.0 * (c[0] - s0.x), y: s0.y + 2.0 / 3.0 * (c[1] - s0.y))
      let c2 = Point(x: a[0] + 2.0 / 3.0 * (c[0] - a[0]), y: a[1] + 2.0 / 3.0 * (c[1] - a[1]))
      cur.add Knot(anchor: pt(a), inCtrl: c2, outCtrl: pt(a), smooth: false)
    of peCurve:
      if cur.len == 0: continue
      cur[^1].outCtrl = pt(el.pts[0])
      cur.add Knot(anchor: pt(el.pts[2]), inCtrl: pt(el.pts[1]), outCtrl: pt(el.pts[2]), smooth: false)
    of peClose: finish(cur, result)
  finish(cur, result)

proc textPath*(doc: Document; t: TextLayer): Path =
  ## The type layer's glyph outlines as a document-space path. Within each glyph, contours wound
  ## like its largest contour combine and the others (counters) subtract.
  let l = layoutOf(doc, t)
  let warp = layoutWarp(l, t.warp)
  var subpaths: seq[Subpath]
  for glyph in outlines(l, t.transform, warp):
    let cs = contours(glyph)
    if cs.len == 0: continue
    var outer = signedArea(cs[0])
    for k in 1 ..< cs.len:
      let a = signedArea(cs[k])
      if not (abs(a) < abs(outer)): outer = a
    var fills, holes: seq[seq[Knot]]
    for k in cs:
      if signum(signedArea(k)) == signum(outer): fills.add k else: holes.add k
    for knots in fills: subpaths.add Subpath(closed: true, knots: knots, op: poCombine)
    for knots in holes: subpaths.add Subpath(closed: true, knots: knots, op: poSubtract)
  newPath(subpaths)

proc createWorkPath(s: var Session; p: JsonNode): JsonNode =
  let id = layerParam(s, p)
  let d = s.active
  if d.isNil: raise noDocument()
  let l = d.doc[].findLayer(id)
  if l.isNil or l[].content.kind != lkText: raise other("not a type layer")
  let path = textPath(d.doc[], l[].content.text)
  let n = path.subpaths.len
  s.edit("Create Work Path", proc(doc: var Document; active: var Option[LayerId]) = doc.workPath = some(path))
  %*{"subpaths": n}

proc convertToShape(s: var Session; p: JsonNode): JsonNode =
  let id = layerParam(s, p)
  let n = s.editWith("Convert to Shape", proc(doc: var Document; active: var Option[LayerId]): int =
    let snapshot = doc
    let l = doc.findLayer(id)
    if l.isNil: raise noLayer(id)
    if l[].content.kind != lkText: raise other("not a type layer")
    let t = l[].content.text
    let runs = t.charRuns
    let color = (if runs.len > 0: runs[0].style.color else: t.color)
    var sh = ShapeLayer(path: textPath(snapshot, t), fill: some(Fill(kind: fkSolid, color: color)))
    refreshShape(snapshot, sh)
    result = sh.path.subpaths.len
    l[].content = LayerContent(kind: lkShape, shape: sh)
    l[].psdBlocks.keepItIf(it.key != "TySh"))
  %*{"layer": uint64(id), "subpaths": n}

# ---------------------------------------------------------------- warp

proc warpText(s: var Session; p: JsonNode): JsonNode =
  const C = "type.warpText"
  let style = p.pget("style").asStr.get("arc")
  let psd = psdStyle(style)
  if psd.isNone: raise bad(C, "unknown warp style \"" & style & "\"")
  proc f(k: string; d: float64): float32 = float32(clamp(p.pget(k).asF64.get(d), -100.0, 100.0))
  let warp = (if psd.get != "warpNone":
                some(TextWarp(style: psd.get, value: f("bend", 50), horizontalDistortion: f("horizontalDistortion", 0),
                              verticalDistortion: f("verticalDistortion", 0),
                              horizontal: p.pget("orientation").asStr != some("vertical")))
              else: none(TextWarp))
  let outp = %*{"warp": toJ(warp)}
  discard withTextTargets(s, p, "Warp Text", proc(t: var TextLayer; r: (int, int)) = t.warp = warp)
  outp

# ---------------------------------------------------------------- OpenType toggles

proc featureOn*(st: CharStyle; tag: string): bool =
  ## Is the OpenType toggle `tag` on for a character style? (`liga`/`dlig` have dedicated fields.)
  case tag
  of "liga": st.ligatures
  of "dlig": st.discretionaryLigatures
  else: st.features.anyIt(it.tag == tag and it.value > 0)

proc setFeature(st: var CharStyle; tag: string; on: bool) =
  case tag
  of "liga": st.ligatures = on
  of "dlig": st.discretionaryLigatures = on
  else:
    st.features.keepItIf(it.tag != tag)
    if on: st.features.add FontFeature(tag: tag, value: 1)

proc toggleOpenType(s: var Session; p: JsonNode; tag: string): JsonNode =
  let ids = formatTargets(s, p)
  let st = s.active
  if st.isNil: raise noDocument()
  var rep = none(LayerId)
  if st.activeLayer.isSome and st.activeLayer.get in ids: rep = st.activeLayer
  elif ids.len > 0: rep = some(ids[0])
  var cur = false
  if rep.isSome:
    let l = st.doc[].findLayer(rep.get)
    if not l.isNil and l[].content.kind == lkText:
      let runs = l[].content.text.charRuns
      cur = runs.len > 0 and featureOn(runs[0].style, tag)
  let on = p.pget("on").asBool.get(not cur)
  discard withTextTargets(s, p, "Change OpenType Feature", proc(t: var TextLayer; r: (int, int)) =
    styleRange(t, min(r[0], r[1]), max(r[0], r[1]), proc(cs: var CharStyle) = setFeature(cs, tag, on)))
  %*{"feature": tag, "on": on}

# ---------------------------------------------------------------- lorem ipsum / update / fonts / defaults

proc pasteLorem(s: var Session; p: JsonNode): JsonNode =
  if hasTextE(s).len == 0 and p.pget("new").asBool != some(true):
    let caret = withText(s, p, "Paste Lorem Ipsum", proc(t: var TextLayer; doc: Document): int =
      let c = p.pget("at").asU64
      let at = (if c.isSome: byteIndex(t.text, int(min(c.get, uint64(high(int))))) else: t.text.len)
      replaceText(t, at, at, LoremIpsum)
      charIndex(t.text, at + LoremIpsum.len))
    return %*{"caret": caret}
  # No type layer active: a paragraph text box over most of the canvas.
  let d = s.active
  if d.isNil: raise noDocument()
  let (w, h) = (float64(d.doc.size.width), float64(d.doc.size.height))
  let size = p.pget("size").asF64.get(clamp(h / 20.0 * 72.0 / float64(d.doc.resolutionDpi), 6.0, 72.0))
  let r = s.execute("type.create", %*{"box": [w * 0.1, h * 0.1, w * 0.8, h * 0.8], "text": LoremIpsum, "size": size})
  %*{"layer": (let v = r.pget("layer"); if v.isNil: newJNull() else: v)}

proc updateAll(s: var Session): JsonNode =
  var n = 0
  s.edit("Update All Text Layers", proc(doc: var Document; active: var Option[LayerId]) =
    let snapshot = doc
    for (_, _, l) in snapshot.walk:
      if l.content.kind != lkText: continue
      let tl = doc.findLayer(l.id)
      if not tl.isNil and tl[].content.kind == lkText:
        refreshText(snapshot, tl[].content.text)
        inc n)
  %*{"updated": n}

proc installedFamilies(): seq[string] =
  for f in shared().fonts.familyNames: result.add unicode.toLower(f)

proc missingFonts*(doc: Document): seq[string] =
  ## Font families used by type layers that the font database cannot supply.
  let have = installedFamilies()
  for (_, _, l) in doc.walk:
    if l.content.kind != lkText: continue
    for r in l.content.text.charRuns:
      let f = r.style.fontFamily
      if f.len > 0 and unicode.toLower(f) notin have and f notin result: result.add f

proc replaceFonts(s: var Session; map: JsonNode; all: bool): JsonNode =
  ## Replace missing families per `map` (missing → installed; unmapped ones → the default family).
  let d = s.active
  if d.isNil: raise noDocument()
  let missing = missingFonts(d.doc[])
  proc pick(f: string): Option[string] =
    if f notin missing: return
    let v = map.pget(f).asStr
    if v.isSome: return v
    if all: some(DefaultFamily) else: none(string)
  var ids: seq[LayerId]
  for (_, _, l) in d.doc[].walk:
    if l.content.kind == lkText: ids.add l.id
  if missing.len == 0: return %*{"missing": newJArray(), "replaced": 0}
  var n = 0
  s.edit("Replace Missing Fonts", proc(doc: var Document; active: var Option[LayerId]) =
    let snapshot = doc
    for id in ids:
      let l = doc.findLayer(id)
      if l.isNil or l[].content.kind != lkText: continue
      var changed = false
      let runs = l[].content.text.charRuns
      var at = 0
      for r in runs:
        let to = pick(r.style.fontFamily)
        if to.isSome:
          let target = to.get
          styleRange(l[].content.text, at, at + r.len, proc(st: var CharStyle) =
            st.fontFamily = target
            st.postscriptName = none(string))
          changed = true
        at += r.len
      if changed:
        refreshText(snapshot, l[].content.text)
        inc n)
  %*{"missing": missing, "replaced": n}

proc resolveMissing(s: var Session; p: JsonNode): JsonNode =
  let m = p.pget("map")
  if m.isObject: return replaceFonts(s, m, false)
  let d = s.active
  if d.isNil: raise noDocument()
  %*{"missing": missingFonts(d.doc[])}

proc saveDefaults(s: var Session): JsonNode =
  let (t, why) = activeText(s)
  if t.isNil: raise other(why)
  let runs = t[].charRuns
  let pruns = t[].paragraphRuns
  let c = (if runs.len > 0: runs[0].style else: defaultCharStyle())
  let pp = (if pruns.len > 0: pruns[0].style else: defaultParagraphStyle())
  result = %*{"font": c.fontFamily, "size": c.sizePt}
  s.typeDefaults = some((c, pp))

proc loadDefaults(s: var Session; p: JsonNode): JsonNode =
  if s.typeDefaults.isNone: raise other("no default type styles have been saved")
  let (c, pp) = s.typeDefaults.get
  discard withTextTargets(s, p, "Load Default Type Styles", proc(t: var TextLayer; r: (int, int)) =
    let n = t.text.len
    styleRange(t, 0, n, proc(st: var CharStyle) = st = c)
    styleParagraphs(t, 0, n, proc(st: var ParagraphStyle) = st = pp))
  %*{"font": c.fontFamily, "size": c.sizePt}

# ---------------------------------------------------------------- registry

proc typeExtraSpecs*(): seq[CommandSpec] =
  const
    LP = """{"layer":id?}"""
    FP = """{"layer":id? or "layers":[id,…]? (default selected Type layers)}"""
    OT = """{"layer":id? or "layers":[id,…]? (default selected Type layers),"on":bool? (default: toggle representative),"range":[startChar,endChar]? (single layer; default all)}"""
  template aa(id, label: string; v: AntiAlias): CommandSpec =
    cmd(id, label, ["Type", "Anti-Alias"], "", FP, enabledProc(hasFormatTargetE(s)), runProc(setAa(s, p, v)))
  template ot(id, label, tag: string): CommandSpec =
    cmd(id, label, ["Type", "OpenType"], "", OT, enabledProc(hasFormatTargetE(s)), runProc(toggleOpenType(s, p, tag)))
  @[aa("type.antiAlias.none", "None", aaNone), aa("type.antiAlias.sharp", "Sharp", aaSharp),
    aa("type.antiAlias.crisp", "Crisp", aaCrisp), aa("type.antiAlias.strong", "Strong", aaStrong),
    aa("type.antiAlias.smooth", "Smooth", aaSmooth), aa("type.antiAlias.windowsLcd", "Windows LCD", aaWindowsLcd),
    aa("type.antiAlias.windows", "Windows", aaWindows),
    cmd("type.orientation.horizontal", "Horizontal", ["Type", "Orientation"], "", FP, enabledProc(hasFormatTargetE(s)),
        runProc(setOrientation(s, p, orHorizontal))),
    cmd("type.orientation.vertical", "Vertical", ["Type", "Orientation"], "", FP, enabledProc(hasFormatTargetE(s)),
        runProc(setOrientation(s, p, orVertical))),
    ot("type.openType.standardLigatures", "Standard Ligatures", "liga"),
    ot("type.openType.contextualAlternates", "Contextual Alternates", "calt"),
    ot("type.openType.discretionaryLigatures", "Discretionary Ligatures", "dlig"),
    ot("type.openType.swash", "Swash", "swsh"), ot("type.openType.oldstyle", "Oldstyle", "onum"),
    ot("type.openType.stylisticAlternates", "Stylistic Alternates", "salt"),
    ot("type.openType.titlingAlternates", "Titling Alternates", "titl"),
    ot("type.openType.ornaments", "Ornaments", "ornm"), ot("type.openType.ordinals", "Ordinals", "ordn"),
    ot("type.openType.fractions", "Fractions", "frac"),
    cmd("type.createWorkPath", "Create Work Path", ["Type"], "", LP, enabledProc(hasTextE(s)), runProc(createWorkPath(s, p))),
    cmd("type.convertToShape", "Convert to Shape", ["Type"], "", LP, enabledProc(hasTextE(s)), runProc(convertToShape(s, p))),
    cmd("type.rasterizeTypeLayer", "Rasterize Type Layer", ["Type"], "", LP, enabledProc(hasTextE(s)),
        runProc(s.execute("type.rasterize", p.copy))),
    cmd("type.convertToParagraphText", "Convert to Paragraph Text", ["Type"], "", LP, enabledProc(hasPointTextE(s)),
        runProc(toParagraph(s, p))),
    cmd("type.convertToPointText", "Convert to Point Text", ["Type"], "", LP, enabledProc(hasBoxTextE(s)), runProc(toPoint(s, p))),
    cmd("type.warpText", "Warp Text…", ["Type"], "",
        """{"layer":id? or "layers":[id,…]? (default selected Type layers),"style":"none|arc|arcLower|arcUpper|arch|bulge|shellLower|shellUpper|flag|wave|fish|rise|fisheye|inflate|squeeze|twist"="arc","orientation":"horizontal|vertical"="horizontal","bend":-100..100=50,"horizontalDistortion":-100..100=0,"verticalDistortion":-100..100=0}""",
        enabledProc(hasFormatTargetE(s)), runProc(warpText(s, p))),
    cmd("type.updateAllTextLayers", "Update All Text Layers", ["Type"], "", "{}", enabledProc(anyTextE(s)), runProc(updateAll(s))),
    cmd("type.replaceAllMissingFonts", "Replace All Missing Fonts", ["Type"], "", "{} (with the default family)",
        enabledProc(anyTextE(s)), runProc(replaceFonts(s, newJObject(), true))),
    cmd("type.resolveMissingFonts", "Resolve Missing Fonts…", ["Type"], "",
        """{"map":{"Missing Family":"Installed Family"}?} (no map: list the missing families)""",
        enabledProc(anyTextE(s)), runProc(resolveMissing(s, p))),
    cmd("type.pasteLoremIpsum", "Paste Lorem Ipsum", ["Type"], "",
        """{"layer":id?,"at":char? (default end),"new":bool=false (new paragraph text layer)}""",
        enabledProc(hasDocE(s)), runProc(pasteLorem(s, p))),
    cmd("type.saveDefaultTypeStyles", "Save Default Type Styles", ["Type"], "",
        "{} (from the active type layer; new type layers start from them)", enabledProc(hasTextE(s)), runProc(saveDefaults(s))),
    cmd("type.loadDefaultTypeStyles", "Load Default Type Styles", ["Type"], "", FP, enabledProc(hasDefaultsE(s)),
        runProc(loadDefaults(s, p)))]
