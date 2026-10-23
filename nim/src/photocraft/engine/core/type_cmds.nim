## `type.*` commands (`type_cmds.rs`): create and edit type (text) layers. Part of the
## `engine/core` unit: included, not imported.
##
## Text offsets in parameters and results are character indices (Unicode scalar values), so JSON
## clients never split a UTF-8 sequence. Sizes, leading, indents and spacing are in points
## (converted with the document resolution); tracking is in 1/1000 em; positions and boxes are
## document pixels. Every edit re-renders the layer with the type engine and regenerates its PSD
## `TySh` data, so the layer saves as editable text.

const
  KernMin* = -1000.0 ## Photoshop's manual kerning range (1/1000 em)
  KernMax* = 10_000.0
  TypeSizeMinPt* = 0.1
  TypeSizeMaxPt* = 1296.0
  TrackingMin* = -1000.0
  TrackingMax* = 10_000.0

proc typeHasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")

proc typeLayerId(s: Session; p: JsonNode): LayerId =
  let v = p.pget("layer").asU64
  if v.isSome: return LayerId(v.get)
  let st = s.active
  if st.isNil or st.activeLayer.isNone: raise other("no active layer")
  st.activeLayer.get

proc f32p(p: JsonNode; k: string): Option[float32] =
  let v = p.pget(k).asF64
  if v.isSome: some(float32(v.get)) else: none(float32)

proc hexU8(s: string; i: int): Option[float32] =
  ## `s.get(i..i + 2)` parsed by `u8::from_str_radix(_, 16)` (a leading `+` is allowed), / 255.
  if i < 0 or i + 2 > s.len: return
  for k in [i, i + 2]:
    if k < s.len and (byte(s[k]) and 0xC0'u8) == 0x80'u8: return # not a char boundary
  var digits = s[i ..< i + 2]
  if digits.len > 1 and digits[0] == '+': digits = digits[1 .. ^1]
  if digits.len == 0 or not digits.allCharsInSet(HexDigits): return
  some(float32(parseHexInt(digits)) / 255'f32)

proc textColor(v: JsonNode): Option[Color] =
  if v.isNil: return
  case v.kind
  of JArray:
    if v.len < 3: return
    var c: seq[float32]
    for x in v.elems: c.add float32(x.asF64.get(0.0))
    some(rgba(c[0], c[1], c[2], (if c.len > 3: c[3] else: 1'f32)))
  of JString:
    var s = v.str
    while s.startsWith("#"): s = s[1 .. ^1]
    case s.len
    of 6:
      let (r, g, b) = (hexU8(s, 0), hexU8(s, 2), hexU8(s, 4))
      if r.isNone or g.isNone or b.isNone: return
      some(rgb(r.get, g.get, b.get))
    of 8:
      let (r, g, b, a) = (hexU8(s, 0), hexU8(s, 2), hexU8(s, 4), hexU8(s, 6))
      if r.isNone or g.isNone or b.isNone or a.isNone: return
      some(rgba(r.get, g.get, b.get, a.get))
    else: none(Color)
  else: none(Color)

proc normText*(s: string): string =
  ## Normalises line breaks to `\n`.
  s.replace("\r\n", "\n").replace("\r", "\n")

proc byteAt(text: string; ci: uint64): int =
  ## The byte offset of character `ci` (the text's length past its end).
  var n = 0'u64
  var i = 0
  while i < text.len:
    if n == ci: return i
    i += max(runeLenAt(text, i), 1)
    inc n
  text.len

proc charAt(text: string; bi: int): int =
  ## The number of characters before byte `bi`.
  let stop = min(max(bi, 0), text.len)
  var i = 0
  while i < stop:
    i += max(runeLenAt(text, i), 1)
    inc result

proc rangeParam*(text: string; p: JsonNode): (int, int) =
  ## `range: [start, end]` in characters → byte range (whole text when absent).
  let r = p.pget("range")
  if r.isArray and r.len == 2:
    let a = r.elems[0].asU64.get(0)
    let b = r.elems[1].asU64.get(a)
    return (byteAt(text, min(a, b)), byteAt(text, max(a, b)))
  (0, text.len)

proc checkKerning*(p: JsonNode): string =
  ## Validates the `kerning` key of character params ("" when fine).
  let v = p.pget("kerning")
  if v.isNil: return ""
  case v.kind
  of JInt, JFloat:
    let k = v.asF64.get
    if k >= KernMin and k <= KernMax: "" else: "kerning must be between -1000 and 10000 (1/1000 em)"
  of JString:
    if v.str.toLowerAscii in ["metrics", "optical", "off", "none", "0"]: ""
    else: "kerning must be a number (1/1000 em) or \"metrics\", \"optical\" or \"off\""
  else: "kerning must be a number (1/1000 em) or \"metrics\", \"optical\" or \"off\""

proc checkSizeTracking*(p: JsonNode): string =
  ## Validates text metrics that can otherwise exceed the rasterizer's supported range.
  let size = p.pget("size")
  if not size.isNil:
    let v = size.asF64
    if v.isNone or v.get != v.get or abs(v.get) == Inf or v.get < TypeSizeMinPt or v.get > TypeSizeMaxPt:
      return "size must be between 0.1 and 1296 pt"
  let tracking = p.pget("tracking")
  if not tracking.isNil:
    let v = tracking.asF64
    if v.isNone or v.get != v.get or abs(v.get) == Inf or v.get < TrackingMin or v.get > TrackingMax:
      return "tracking must be between -1000 and 10000 (1/1000 em)"
  ""

proc applyCharProps*(s: var CharStyle; p: JsonNode): bool =
  ## Applies character-style keys from JSON. Returns true if any key was present.
  var any = false
  let fontNode = (if not p.pget("font").isNil: p.pget("font") else: p.pget("family"))
  if fontNode.asStr.isSome:
    s.fontFamily = fontNode.str
    s.postscriptName = none(string)
    any = true
  let fs = p.pget("fontStyle").asStr
  if fs.isSome:
    let v = fs.get
    s.fontStyle = v
    s.postscriptName = none(string)
    # The style sets the weight, so a weight slider value set earlier stops overriding it.
    s.variations.keepItIf(it.axis != "wght")
    let l = unicode.toLower(v)
    s.italic = l.contains("italic") or l.contains("oblique")
    let g = guessFromPostscript("X-" & v.replace(" ", ""))
    s.weight = g.weight
    let face = shared().fonts.namedFace(s.fontFamily, v)
    if face.isSome:
      s.weight = uint16(clamp(round(face.get.weight), 0'f32, 65535'f32))
      s.italic = face.get.italic
      s.postscriptName = face.get.postscriptName
    any = true
  let w = p.pget("weight").asF64
  if w.isSome:
    let weight = uint16(clamp(w.get, 1.0, 1000.0))
    if s.weight != weight:
      s.postscriptName = none(string)
      s.fontStyle = ""
    s.variations.keepItIf(it.axis != "wght")
    s.weight = weight
    any = true
  let it = p.pget("italic").asBool
  if it.isSome:
    if s.italic != it.get:
      s.postscriptName = none(string)
      s.fontStyle = ""
    s.italic = it.get
    any = true
  let ps = p.pget("postscriptName").asStr
  if ps.isSome:
    s.postscriptName = ps
    any = true
  let size = f32p(p, "size")
  if size.isSome:
    s.sizePt = max(size.get, 0.01'f32)
    any = true
  let c = textColor(p.pget("color"))
  if c.isSome:
    s.color = c.get
    any = true
  let tr = f32p(p, "tracking")
  if tr.isSome:
    s.tracking = tr.get
    any = true
  let leading = p.pget("leading")
  if not leading.isNil:
    case leading.kind
    of JNull:
      s.leadingPt = none(float32)
      any = true
    of JString:
      if leading.str == "auto":
        s.leadingPt = none(float32)
        any = true
    of JInt, JFloat:
      s.leadingPt = some(float32(leading.asF64.get))
      any = true
    else: discard
  let bs = f32p(p, "baselineShift")
  if bs.isSome:
    s.baselineShiftPt = bs.get
    any = true
  let hs = f32p(p, "horizontalScale")
  if hs.isSome:
    s.horizontalScale = max(hs.get / 100'f32, 0.01'f32)
    any = true
  let vs = f32p(p, "verticalScale")
  if vs.isSome:
    s.verticalScale = max(vs.get / 100'f32, 0.01'f32)
    any = true
  template flag(key: string; field: untyped) =
    let b = p.pget(key).asBool
    if b.isSome:
      field = b.get
      any = true
  flag("underline", s.underline)
  flag("strikethrough", s.strikethrough)
  flag("fauxBold", s.fauxBold)
  flag("fauxItalic", s.fauxItalic)
  flag("ligatures", s.ligatures)
  flag("discretionaryLigatures", s.discretionaryLigatures)
  let kerning = p.pget("kerning")
  if not kerning.isNil:
    case kerning.kind
    of JInt, JFloat:
      # A number is Photoshop's manual kerning: no automatic pair kerning plus the value.
      let k = kerning.asF64.get
      if k == k and abs(k) != Inf:
        s.kerning = kOff
        s.kern = float32(clamp(k, KernMin, KernMax))
        any = true
    of JString:
      s.kerning = (case kerning.str.toLowerAscii
                   of "optical": kOptical
                   of "none", "off", "0": kOff
                   else: kMetrics)
      s.kern = 0
      any = true
    else: discard
  let caps = p.pget("caps").asStr
  if caps.isSome:
    s.caps = (case caps.get
              of "small", "smallCaps": capsSmallCaps
              of "all", "allCaps": capsAllCaps
              else: capsNormal)
    any = true
  let features = p.pget("features")
  if features.isObject:
    for tag, v in features.sortedPairs:
      let u = v.asU64
      let value = (if u.isSome: uint16(u.get and 0xFFFF'u64)
                   elif v.asBool.isSome: uint16(ord(v.bval))
                   else: 1'u16)
      s.features.keepItIf(it.tag != tag)
      s.features.add FontFeature(tag: tag, value: value)
    any = true
  let variations = p.pget("variations")
  if variations.isObject:
    for axis, v in variations.sortedPairs:
      s.variations.keepItIf(it.axis != axis)
      let x = v.asF64
      if x.isSome: s.variations.add FontVariation(axis: axis, value: float32(x.get))
    any = true
  let language = p.pget("language")
  if not language.isNil:
    s.language = language.asStr
    any = true
  any

proc applyParaProps*(s: var ParagraphStyle; p: JsonNode): bool =
  ## Applies paragraph-style keys from JSON. Returns true if any key was present.
  var any = false
  let align = p.pget("align").asStr
  if align.isSome:
    s.align = (case align.get
               of "center": taCenter
               of "right": taRight
               of "justify", "justifyLeft": taJustifyLeft
               of "justifyCenter": taJustifyCenter
               of "justifyRight": taJustifyRight
               of "justifyAll": taJustifyAll
               else: taLeft)
    any = true
  template pt(key: string; field: untyped; percent = false) =
    let v = f32p(p, key)
    if v.isSome:
      field = (if percent: v.get / 100'f32 else: v.get)
      any = true
  pt("firstLineIndent", s.firstLineIndentPt)
  pt("startIndent", s.startIndentPt)
  pt("endIndent", s.endIndentPt)
  pt("spaceBefore", s.spaceBeforePt)
  pt("spaceAfter", s.spaceAfterPt)
  pt("autoLeading", s.autoLeading, true)
  let dir = p.pget("direction").asStr
  if dir.isSome:
    s.direction = (case dir.get
                   of "ltr": tdLtr
                   of "rtl": tdRtl
                   else: tdAuto)
    any = true
  let h = p.pget("hyphenate").asBool
  if h.isSome:
    s.hyphenate = h.get
    any = true
  any

proc pushMerge[S](o: var seq[(int, S)]; len: int; st: S) =
  if len == 0: return
  if o.len > 0 and o[^1][1] == st: o[^1][0] += len
  else: o.add (len, st)

proc styleRange*(t: var TextLayer; a, b: int; f: proc(st: var CharStyle)) =
  ## Applies `f` to the character style of bytes `a..b` (splitting runs at the edges).
  # A reversed range styles nothing.
  let b = max(b, a)
  let runs = t.charRuns
  if t.text.len == 0:
    var st = (if runs.len > 0: runs[0].style else: defaultCharStyle())
    f(st)
    t.runs = @[TextRun(len: 0, style: st)]
    t.syncSummary()
    return
  var o: seq[(int, CharStyle)]
  var at = 0
  for r in runs:
    let (rs, re) = (at, at + r.len)
    at = re
    let ca = clamp(a, rs, re)
    let cb = clamp(b, rs, re)
    pushMerge(o, ca - rs, r.style)
    var st = r.style
    f(st)
    pushMerge(o, cb - ca, st)
    pushMerge(o, re - cb, r.style)
  t.runs = @[]
  for (len, style) in o: t.runs.add TextRun(len: len, style: style)
  t.syncSummary()

proc styleParagraphs*(t: var TextLayer; a, b: int; f: proc(st: var ParagraphStyle)) =
  ## Applies `f` to every paragraph touching bytes `a..b` (paragraph runs follow paragraphs).
  let old = t.paragraphRuns
  var starts: seq[int]
  var acc = 0
  for r in old:
    starts.add acc
    acc += r.len
  proc styleAt(off: int): ParagraphStyle =
    var k = 0
    for i in countdown(starts.high, 0):
      if starts[i] <= off:
        k = i
        break
    old[k].style
  var o: seq[(int, ParagraphStyle)]
  let trailingBreak = t.text.len > 0 and t.text[^1] in {'\r', '\n'}
  for (ps, pe) in splitParagraphs(t.text):
    var st = styleAt(ps)
    let terminalEmpty = ps == pe
    let touches = (ps < b and pe > a) or
      (a == b and a >= ps and (a < pe or terminalEmpty or (pe == t.text.len and not trailingBreak)))
    if touches: f(st)
    if terminalEmpty and trailingBreak:
      # This paragraph has no bytes but keeps its own style for the next typed character and for
      # the PSD's final paragraph marker.
      o.add (0, st)
    else:
      pushMerge(o, pe - ps, st)
      if o.len == 0: o.add (0, st)
  t.paragraphs = @[]
  for (len, style) in o: t.paragraphs.add ParagraphRun(len: len, style: style)

proc adjustRuns[S](runs: seq[(int, S)]; a, b, ins: int; trailingParagraph: bool): seq[(int, S)] =
  var total = 0
  for r in runs: total += r[0]
  var host = 0
  if trailingParagraph and a == b and runs.len > 0 and runs[^1][0] == 0 and a == total:
    # Inserted text belongs to a styled final empty paragraph, not to the preceding paragraph
    # whose break ends at this byte offset.
    host = runs.len - 1
  else:
    var s = 0
    for i, r in runs:
      let e = s + r[0]
      if (if a == 0: e > 0 or s == 0 else: a > s and a <= e):
        host = i
        break
      s = e
  var at = 0
  for i, (len, st) in runs:
    let (rs, re) = (at, at + len)
    at = re
    let keep = (clamp(a, rs, re) - rs) + (re - clamp(b, rs, re))
    pushMerge(result, keep + (if i == host: ins else: 0), st)

proc replaceText*(t: var TextLayer; a, b: int; newText: string) =
  ## Replaces bytes `a..b` with `newText`, keeping styles: inserted text takes the style of the
  ## character before it (or after it at the start).
  let a2 = min(a, t.text.len)
  let b2 = max(min(b, t.text.len), a2)
  let ins = normText(newText)
  var cr: seq[(int, CharStyle)]
  for r in t.charRuns: cr.add (r.len, r.style)
  var pr: seq[(int, ParagraphStyle)]
  for r in t.paragraphRuns: pr.add (r.len, r.style)
  let firstC = (if cr.len > 0: cr[0][1] else: defaultCharStyle())
  let firstP = (if pr.len > 0: pr[0][1] else: defaultParagraphStyle())
  var trailingStyle = none(ParagraphStyle)
  if t.text.len > 0 and t.text[^1] in {'\r', '\n'} and pr.len > 0 and pr[^1][0] == 0:
    trailingStyle = some(pr[^1][1])
  cr = adjustRuns(cr, a2, b2, ins.len, false)
  pr = adjustRuns(pr, a2, b2, ins.len, true)
  t.text = t.text[0 ..< a2] & ins & t.text[b2 .. ^1]
  if t.text.len > 0 and t.text[^1] in {'\r', '\n'} and trailingStyle.isSome: pr.add (0, trailingStyle.get)
  if cr.len == 0: cr.add (t.text.len, firstC)
  if pr.len == 0: pr.add (t.text.len, firstP)
  t.runs = @[]
  for (len, style) in cr: t.runs.add TextRun(len: len, style: style)
  t.paragraphs = @[]
  for (len, style) in pr: t.paragraphs.add ParagraphRun(len: len, style: style)
  t.syncSummary()

proc refreshText*(doc: Document; t: var TextLayer) =
  ## Re-renders the layer's pixels and regenerates its PSD `TySh` data (`type_cmds::refresh`).
  let dpi = doc.resolutionDpi
  let (layout, r) = shared().render(t, dpi, doc.pixelFormat)
  t.cache = some(r.surface)
  t.psdRaw = SharedBytes(data: buildTysh(t, dpi, layout.bounds))

proc layerName*(text: string): string =
  ## The name a type layer gets from its text: its first non-empty line, at most 40 characters.
  var first = ""
  var lines = text.split('\n')
  if text.endsWith("\n"): lines.setLen(lines.len - 1)
  for line in lines:
    var l = line
    if l.endsWith("\r"): l.setLen(l.len - 1)
    let trimmed = unicode.strip(l)
    if trimmed.len > 0:
      first = trimmed
      break
  var n = 0
  var i = 0
  while i < first.len and n < 40:
    i += max(runeLenAt(first, i), 1)
    inc n
  let name = first[0 ..< min(i, first.len)]
  if name.len == 0: "Type Layer" else: name

proc isAutoNamed*(l: Layer): bool =
  ## Whether a type layer still has the name it got from its text, so the name follows edits.
  l.content.kind == lkText and l.name == layerName(l.content.text.text)

proc notTypeLayer(id: LayerId; c: LayerContent): ref EngineError =
  other("layer " & $uint64(id) & " is " & c.article & " " & c.kindName & " layer, not a type layer")

proc withTextLayer(s: var Session; p: JsonNode; label: string;
                   f: proc(t: var TextLayer; doc: Document; rename: var Option[string])) =
  ## Runs `f` on a type layer as one undo step. `f` may set an explicit layer name, applied in the
  ## same step; otherwise an auto-named layer follows its text.
  let id = typeLayerId(s, p)
  block:
    let st = s.active
    if st.isNil: raise noDocument()
    let layer = st.doc[].findLayer(id)
    if layer.isNil: raise noLayer(id)
    if st.doc[].effectiveLocks(id).all:
      raise other("Could not complete your request because the layer \"" & layer[].name & "\" is locked")
  let damage = s.editWith(label, proc(doc: var Document; active: var Option[LayerId]): Option[Rect] =
    let snapshot = doc
    let l = doc.layerPtr(id)
    if l.isNil: raise noLayer(id)
    let autoNamed = isAutoNamed(l[])
    if l[].content.kind != lkText: raise notTypeLayer(id, l[].content)
    template t: untyped = l[].content.text
    let before = (if t.cache.isSome: some(t.cache.get.tileBounds) else: none(Rect))
    var rename = none(string)
    f(t, snapshot, rename)
    refreshText(snapshot, t)
    # Only this layer's pixels changed: the canvas recomposites their old and new area.
    if before.isSome and t.cache.isSome: result = some(before.get.union(t.cache.get.tileBounds))
    let name = (if rename.isSome: rename elif autoNamed: some(layerName(t.text)) else: none(string))
    if name.isSome: l[].name = name.get)
  let st = s.active
  if not st.isNil: st.lastDamage = damage

proc formatTargets*(s: Session; p: JsonNode): seq[LayerId] =
  ## Direct formatting follows the selected Type layers, while an explicit layer or character
  ## range keeps its single-layer scope. Selected groups are not expanded.
  let st = s.active
  if st.isNil: raise noDocument()
  let single = not p.pget("layer").isNil or (not p.pget("range").isNil and p.pget("layers").isNil)
  var ids: seq[LayerId]
  if single: ids = @[typeLayerId(s, p)]
  elif not p.pget("layers").isNil:
    let v = p.pget("layers")
    if not v.isArray: raise other("layers must be an array of layer IDs")
    for x in v.elems:
      let u = x.asU64
      if u.isNone: raise other("layers must contain layer IDs")
      ids.add LayerId(u.get)
  else: ids = st[].selectedOrdered
  for id in ids:
    let l = st.doc[].findLayer(id)
    if l.isNil: raise noLayer(id)
    if l[].content.kind == lkText:
      if id notin result: result.add id
    elif single: raise notTypeLayer(id, l[].content)
  if result.len == 0: raise other("select a type layer")

proc hasFormatTargetE*(s: Session): string =
  try:
    discard formatTargets(s, newJNull())
    ""
  except EngineError as e: e.msg

proc withTextTargets*(s: var Session; p: JsonNode; label: string;
                      f: proc(t: var TextLayer; range: (int, int))): seq[LayerId] =
  ## One history step for a formatting batch.
  let ids = formatTargets(s, p)
  let damage = s.editWith(label, proc(doc: var Document; active: var Option[LayerId]): Option[Rect] =
    let snapshot = doc
    result = some(EmptyRect)
    for id in ids:
      let l = doc.layerPtr(id)
      if l.isNil or l[].content.kind != lkText: raise noLayer(id)
      template t: untyped = l[].content.text
      let before = (if t.cache.isSome: some(t.cache.get.tileBounds) else: none(Rect))
      let range = (if p.pget("layers").isNil or not p.pget("layer").isNil: rangeParam(t.text, p) else: (0, t.text.len))
      f(t, range)
      refreshText(snapshot, t)
      # An unknown old/new cache keeps the edit's full-refresh fallback.
      if result.isSome and before.isSome and t.cache.isSome:
        result = some(result.get.union(before.get).union(t.cache.get.tileBounds))
      else: result = none(Rect))
  let st = s.active
  if not st.isNil: st.lastDamage = damage
  ids

proc shownStyleParams(t: TextLayer; p: JsonNode): JsonNode =
  ## UI size/leading are measured after the layer transform; command callers otherwise use points.
  if p.pget("metricsAsShown").asBool != some(true): return p
  let m = t.transform.m
  let scale32 = float32(sqrt(abs(m[0] * m[3] - m[1] * m[2])))
  let scale = (if scale32 == scale32 and abs(scale32) != Inf and scale32 > 1e-3'f32: float64(scale32) else: 1.0)
  result = p.copy
  for key in ["size", "leading"]:
    let v = p.pget(key).asF64
    if v.isSome:
      let value = v.get / scale
      if value != value or abs(value) == Inf or abs(value) > float64(F32Max):
        raise bad("type.setStyle", "shown size and leading must convert to finite point values")
      result[key] = %value

proc typeInfo*(s: Session; p: JsonNode): JsonNode =
  let id = typeLayerId(s, p)
  let d = s.active
  if d.isNil: raise noDocument()
  let l = d.doc[].findLayer(id)
  if l.isNil: raise noLayer(id)
  if l[].content.kind != lkText: raise other("layer " & $uint64(id) & " is not a type layer")
  template t: untyped = l[].content.text
  let text = t.text
  var runs = newJArray()
  var at = 0
  for r in t.charRuns:
    runs.add %*{"start": charAt(text, at), "end": charAt(text, at + r.len), "style": toJ(r.style)}
    at += r.len
  var paragraphs = newJArray()
  at = 0
  for r in t.paragraphRuns:
    paragraphs.add %*{"start": charAt(text, at), "end": charAt(text, at + r.len), "style": toJ(r.style)}
    at += r.len
  let layout = shared().layout(t, d.doc.resolutionDpi)
  var lines = newJArray()
  for ln in layout.lines:
    lines.add %*{"start": charAt(text, ln.range[0]), "end": charAt(text, ln.range[1]), "baseline": ln.baseline,
                 "x0": ln.x0, "x1": ln.x1, "ascent": ln.ascent, "descent": ln.descent}
  let bounds = (if t.cache.isSome:
                  (let r = t.cache.get.contentBounds; %*[r.x0, r.y0, r.x1, r.y1])
                else: newJNull())
  %*{"layer": uint64(id), "text": text, "shape": toJ(t.shape), "transform": toJ(t.transform.m),
     "antialias": toJ(t.antialias), "orientation": toJ(t.orientation), "warp": toJ(t.warp), "runs": runs,
     "paragraphs": paragraphs, "lines": lines, "bounds": bounds, "pxPerPt": layout.pxPerPt}

proc newTextLayer*(s: Session; p: JsonNode; cmdId: string): TextLayer =
  ## A type layer (not yet rendered or added) from `type.create`'s params, starting from the
  ## default type styles in the foreground colour.
  let o = p.pget("orientation")
  var orientation = orHorizontal
  if not o.isNil:
    if o.asStr == some("horizontal"): orientation = orHorizontal
    elif o.asStr == some("vertical"): orientation = orVertical
    else: raise bad(cmdId, "orientation must be horizontal or vertical")
  let kerr = checkKerning(p)
  if kerr.len > 0: raise bad(cmdId, kerr)
  let serr = checkSizeTracking(p)
  if serr.len > 0: raise bad(cmdId, serr)
  let text = normText(p.pget("text").asStr.get(""))
  # Type › Save Default Type Styles sets the starting styles; the colour is always the
  # foreground colour, as in Photoshop.
  var style, para: (CharStyle, ParagraphStyle)
  if s.typeDefaults.isSome: style = s.typeDefaults.get
  else:
    var c = defaultCharStyle()
    c.fontFamily = DefaultFamily
    style = (c, defaultParagraphStyle())
  let fg = s.tools.foreground
  style[0].color = rgba(fg[0], fg[1], fg[2], fg[3])
  discard applyCharProps(style[0], p)
  discard applyParaProps(style[1], p)
  var shape = TextShape(kind: tsPoint)
  var transform: Affine
  let b = p.pget("box")
  if b.isArray and b.len == 4:
    var v: array[4, float32]
    for i in 0 ..< 4: v[i] = float32(b.elems[i].asF64.get(0.0))
    if v[2] <= 0 or v[3] <= 0: raise bad(cmdId, "box width and height must be positive")
    shape = TextShape(kind: tsBox, x: 0, y: 0, width: v[2], height: v[3])
    transform = translateAffine(float64(v[0]), float64(v[1]))
  else:
    transform = translateAffine(float64(f32p(p, "x").get(0'f32)), float64(f32p(p, "y").get(0'f32)))
  result = defaultTextLayer()
  result.runs = @[TextRun(len: text.len, style: style[0])]
  result.paragraphs = @[ParagraphRun(len: text.len, style: style[1])]
  result.text = text
  result.shape = shape
  result.transform = transform
  result.orientation = orientation
  result.syncSummary()

proc typeCreate(s: var Session; p: JsonNode): JsonNode =
  let t0 = newTextLayer(s, p, "type.create")
  let name = p.pget("name").asStr.get(layerName(t0.text))
  let id = s.editWith("New Type Layer", proc(doc: var Document; active: var Option[LayerId]): LayerId =
    var t = t0
    refreshText(doc, t)
    result = doc.insertAbove(active, newLayer(name, textContent(t)))
    active = some(result))
  var bounds = newJNull()
  let st = s.active
  if not st.isNil:
    let l = st.doc[].findLayer(id)
    if not l.isNil and l[].hasSurface:
      let r = l[].surface.contentBounds
      bounds = %*[r.x0, r.y0, r.x1, r.y1]
  %*{"layer": uint64(id), "bounds": bounds}

proc typeEdit(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "type.edit"
  let id = typeLayerId(s, p)
  let name = p.pget("name").asStr
  var e = checkKerning(p)
  if e.len > 0: raise bad(cmdId, e)
  e = checkSizeTracking(p)
  if e.len > 0: raise bad(cmdId, e)
  let runsNode = p.pget("runs")
  if runsNode.isArray:
    for i, r in runsNode.elems:
      e = checkKerning(r)
      if e.len > 0: raise bad(cmdId, e)
      # Checked before anything changes, so a bad run leaves the layer untouched.
      let start = r.pget("start").asU64.get(0)
      let stop = r.pget("end").asU64
      if stop.isSome and stop.get < start:
        raise bad(cmdId, "runs[" & $i & "]: `end` (" & $stop.get & ") is before `start` (" & $start & ")")
      e = checkSizeTracking(r)
      if e.len > 0: raise bad(cmdId, e)
  var kernPair = none((uint64, float64))
  let k = p.pget("kernPair")
  if not k.isNil:
    let at = k.pget("at").asU64
    if at.isNone: raise bad(cmdId, "kernPair.at must be a caret position (character index)")
    let by = k.pget("by").asF64
    if by.isNone or by.get != by.get or abs(by.get) == Inf or abs(by.get) > KernMax:
      raise bad(cmdId, "kernPair.by must be a number of 1/1000 em")
    kernPair = some((at.get, by.get))
  let label = (if kernPair.isSome: "Kerning" else: "Edit Type")
  withTextLayer(s, p, label, proc(t: var TextLayer; doc: Document; rename: var Option[string]) =
    if kernPair.isSome:
      # Photoshop's Alt+←/→: the pair before the caret becomes manually kerned, starting from what
      # it shows now (its metrics/optical or manual value).
      let (at, by) = kernPair.get
      let n = uint64(t.text.runeLen)
      if at == 0 or at >= n: raise bad(cmdId, "kernPair needs a caret between two characters")
      let a = byteAt(t.text, at - 1)
      let b = byteAt(t.text, at)
      let now = shared().pairKerning(t, doc.resolutionDpi, a)
      if now.isNone: raise bad(cmdId, "no kerning pair at the caret (line break or line end)")
      let value = float32(clamp(float64(round(now.get)) + by, KernMin, KernMax))
      styleRange(t, a, b, proc(st: var CharStyle) =
        st.kerning = kOff
        st.kern = value)
    if not p.pget("kerning").isNil:
      let (a, b) = rangeParam(t.text, p)
      let kp = %*{"kerning": p.pget("kerning")}
      styleRange(t, a, b, proc(st: var CharStyle) = discard applyCharProps(st, kp))
    let text = p.pget("text").asStr
    if text.isSome: replaceText(t, 0, t.text.len, text.get)
    let r = p.pget("replace")
    if not r.isNil:
      let a = byteAt(t.text, r.pget("start").asU64.get(0))
      let b = byteAt(t.text, r.pget("end").asU64.get(high(uint64)))
      replaceText(t, a, max(b, a), r.pget("text").asStr.get(""))
    let runs = p.pget("runs")
    if runs.isArray:
      for run in runs.elems:
        let a = byteAt(t.text, run.pget("start").asU64.get(0))
        let b = byteAt(t.text, run.pget("end").asU64.get(high(uint64)))
        let rp = run
        styleRange(t, a, b, proc(st: var CharStyle) = discard applyCharProps(st, rp))
    let lin = t.transform.m
    let box = p.pget("box")
    if box.isArray and box.len == 4:
      var v: array[4, float64]
      for i in 0 ..< 4: v[i] = box.elems[i].asF64.get(0.0)
      t.shape = TextShape(kind: tsBox, x: 0, y: 0, width: float32(max(v[2], 1.0)), height: float32(max(v[3], 1.0)))
      t.transform = Affine(m: [lin[0], lin[1], lin[2], lin[3], v[0], v[1]])
    let pt = p.pget("point")
    if pt.isArray and pt.len == 2:
      t.shape = TextShape(kind: tsPoint)
      t.transform = Affine(m: [lin[0], lin[1], lin[2], lin[3], pt.elems[0].asF64.get(0.0), pt.elems[1].asF64.get(0.0)])
    let mv = p.pget("move")
    if mv.isArray and mv.len == 2:
      t.transform.m[4] += mv.elems[0].asF64.get(0.0)
      t.transform.m[5] += mv.elems[1].asF64.get(0.0)
    let tm = p.pget("transform")
    if tm.isArray and tm.len == 6:
      for i, v in tm.elems: t.transform.m[i] = v.asF64.get(t.transform.m[i])
    let aa = p.pget("antialias").asStr
    if aa.isSome:
      t.antialias = (case aa.get
                     of "none": aaNone
                     of "sharp": aaSharp
                     of "crisp": aaCrisp
                     of "strong": aaStrong
                     else: aaSmooth)
    # The rename lands in the same undo step as the edit.
    rename = name)
  typeInfo(s, %*{"layer": uint64(id)})

proc typeSetStyle(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "type.setStyle"
  let e = checkKerning(p)
  if e.len > 0: raise bad(cmdId, e)
  let ids = withTextTargets(s, p, "Set Type Style", proc(t: var TextLayer; range: (int, int)) =
    let (a, b) = range
    let props = shownStyleParams(t, p)
    let serr = checkSizeTracking(props)
    if serr.len > 0: raise bad(cmdId, serr)
    var probe = defaultCharStyle()
    if applyCharProps(probe, props):
      styleRange(t, a, b, proc(st: var CharStyle) = discard applyCharProps(st, props))
    var pprobe = defaultParagraphStyle()
    if applyParaProps(pprobe, p):
      styleParagraphs(t, a, b, proc(st: var ParagraphStyle) = discard applyParaProps(st, p)))
  if ids.len == 0: raise other("select a type layer")
  typeInfo(s, %*{"layer": uint64(ids[0])})

proc typeRasterize(s: var Session; p: JsonNode): JsonNode =
  let id = typeLayerId(s, p)
  s.editDo("Rasterize Type"):
    let snapshot = doc
    let l = doc.layerPtr(id)
    if l.isNil: raise noLayer(id)
    if l[].content.kind != lkText: raise other("layer " & $uint64(id) & " is not a type layer")
    if l[].content.text.cache.isNone: refreshText(snapshot, l[].content.text)
    var surface = (if l[].content.text.cache.isSome: l[].content.text.cache.get else: newSurface(snapshot.pixelFormat))
    if surface.format != snapshot.pixelFormat: surface = surface.convert(snapshot.pixelFormat)
    l[].content = rasterContent(surface)
    l[].psdBlocks.keepItIf(it.key != "TySh")
  %*{"layer": uint64(id)}

proc typeFonts(p: JsonNode): JsonNode =
  let eng = shared()
  let family = p.pget("family").asStr
  if family.isSome:
    var faces = newJArray()
    for x in eng.fonts.faces(family.get):
      var axes = newJArray()
      for a in x.axes: axes.add toJ(a)
      faces.add %*{"family": x.family, "style": x.style, "postscriptName": toJ(x.postscriptName), "weight": x.weight,
                   "italic": x.italic, "axes": axes}
    return %*{"faces": faces}
  %*{"families": eng.fonts.familyNames}

proc hasTypeLayerE(s: Session): string =
  let st = s.active
  if st.isNil: return "no document open"
  if st.activeLayer.isSome:
    let l = st.doc[].findLayer(st.activeLayer.get)
    if not l.isNil and l[].content.kind == lkText: return ""
  "the active layer is not a type layer"

proc stepSize(s: var Session; p: JsonNode; up: bool; defaultBy: float32): JsonNode =
  ## Grows or shrinks every character in the range by `by` points as the layer appears, so mixed
  ## sizes keep their differences and a scaled layer steps in the points its size field shows.
  let id = typeLayerId(s, p)
  var by = defaultBy
  let given = f32p(p, "by")
  if given.isSome and given.get == given.get and abs(given.get) != Inf: by = given.get
  by = min(abs(by), float32(TypeSizeMaxPt))
  withTextLayer(s, p, "Change Type Size", proc(t: var TextLayer; doc: Document; rename: var Option[string]) =
    let m = t.transform.m
    var scale = float32(sqrt(abs(m[0] * m[3] - m[1] * m[2])))
    if not (scale == scale and abs(scale) != Inf and scale > 1e-3'f32): scale = 1
    let delta = (if up: by else: -by) / scale
    let (a, b) = rangeParam(t.text, p)
    styleRange(t, a, b, proc(st: var CharStyle) =
      st.sizePt = clamp(st.sizePt + delta, float32(TypeSizeMinPt), float32(TypeSizeMaxPt))))
  typeInfo(s, %*{"layer": uint64(id)})

proc refreshTypeLayers*(s: var Session; layers: openArray[(DocId, LayerId)]): seq[(DocId, LayerId)] =
  ## Re-renders the pixels of type layers whose font arrived after they were drawn. The layers'
  ## model is unchanged, so this is no history step and leaves a clean document clean. Unknown
  ## documents and layers are skipped.
  var docs: seq[DocId]
  for (d, _) in layers:
    if d notin docs: docs.add d
  for docId in docs:
    var index = -1
    for i, st in s.docs:
      if st.doc.id == docId:
        index = i
        break
    if index < 0: continue
    let snapshot = s.docs[index].doc[]
    var doc = snapshot
    var changed = false
    for (d, id) in layers:
      if d != docId: continue
      let l = doc.layerPtr(id)
      if not l.isNil and l[].content.kind == lkText:
        refreshText(snapshot, l[].content.text)
        changed = true
    if changed:
      template st: untyped = s.docs[index]
      let clean = st.savedRevision == st.revision
      let nd = DocRef()
      nd[] = move doc
      st.doc = nd
      inc st.revision
      if clean: st.savedRevision = st.revision
      st.lastDamage = none(Rect)

const TypeCharParams = "\"font\":str,\"fontStyle\":str,\"weight\":100..900,\"italic\":bool,\"size\":0.1..=1296 pt,\"color\":\"#rrggbb\"|[r,g,b,a],\"tracking\":-1000..=10000 (1/1000 em),\"leading\":pt|\"auto\",\"baselineShift\":pt,\"horizontalScale\":%,\"verticalScale\":%,\"underline\":bool,\"strikethrough\":bool,\"fauxBold\":bool,\"fauxItalic\":bool,\"kerning\":1/1000em (manual, after each character)|\"metrics\"|\"optical\"|\"off\",\"caps\":\"normal|small|all\",\"ligatures\":bool,\"discretionaryLigatures\":bool,\"features\":{\"ss01\":1},\"variations\":{\"wght\":650},\"language\":str"

proc typeSpecs*(): seq[CommandSpec] =
  const sizeParams = "{\"layer\":id?,\"range\":[startChar,endChar]? (default all),\"by\":pt? (as the layer appears)}"
  @[
    cmd("type.create", "New Type Layer", ["Layer", "New"], "",
        "{\"x\":px,\"y\":px (baseline anchor of point text),\"text\":str,\"box\":[x,y,w,h]? (paragraph text),\"name\":str?,\"align\":\"left|center|right|justify…\"?,\"orientation\":\"horizontal|vertical\"=\"horizontal\", …character keys: \"font\",\"size\":pt=12,\"color\",…}",
        typeHasDocE, runProc(typeCreate(s, p))),
    cmd("type.edit", "Edit Type", [], "",
        "{\"layer\":id?,\"text\":str? (replace all, styles kept),\"replace\":{\"start\":char,\"end\":char,\"text\":str}?,\"runs\":[{\"start\":char,\"end\":char,…character keys}]?,\"box\":[x,y,w,h]? (to paragraph text),\"point\":[x,y]? (to point text),\"move\":[dx,dy]?,\"transform\":[a,b,c,d,e,f]?,\"antialias\":\"none|sharp|crisp|strong|smooth\"?,\"name\":str?,\"kerning\":1/1000em|\"metrics\"|\"optical\"|\"off\"? (with \"range\":[startChar,endChar]?, default all),\"kernPair\":{\"at\":caretChar,\"by\":1/1000em}? (Alt+←/→: the pair before the caret becomes manual, its current kerning + by)}",
        typeHasDocE, runProc(typeEdit(s, p))),
    cmd("type.setStyle", "Set Type Style", [], "",
        "{\"layer\":id? or \"layers\":[id,…]? (default selected Type layers),\"range\":[startChar,endChar]? (single layer; default all),\"metricsAsShown\":bool=false (size/leading include each layer's transform scale), " &
          TypeCharParams & ", paragraph keys: \"align\":\"left|center|right|justify|justifyCenter|justifyRight|justifyAll\",\"firstLineIndent\":pt,\"startIndent\":pt,\"endIndent\":pt,\"spaceBefore\":pt,\"spaceAfter\":pt,\"autoLeading\":%,\"direction\":\"auto|ltr|rtl\",\"hyphenate\":bool}",
        typeHasDocE, runProc(typeSetStyle(s, p))),
    cmd("type.rasterize", "Rasterize Type", ["Layer", "Rasterize"], "", "{\"layer\":id?}", typeHasDocE,
        runProc(typeRasterize(s, p))),
    cmd("type.info", "Type Layer Info", [], "",
        "{\"layer\":id?} → text, runs/paragraphs (char offsets + styles), shape, transform, laid-out lines (text space px), bounds",
        typeHasDocE, runProc(typeInfo(s, p)), journal = false),
    cmd("type.fonts", "List Fonts", [], "", "{\"family\":str? (faces of one family)}",
        enabledProc(""), runProc(typeFonts(p)), journal = false),
    cmd("type.increaseSize", "Increase Type Size", [], "Cmd+Shift+.", sizeParams, hasTypeLayerE,
        runProc(stepSize(s, p, true, 2))),
    cmd("type.decreaseSize", "Decrease Type Size", [], "Cmd+Shift+,", sizeParams, hasTypeLayerE,
        runProc(stepSize(s, p, false, 2))),
    cmd("type.increaseSizeMore", "Increase Type Size by 10", [], "Cmd+Alt+Shift+.", sizeParams, hasTypeLayerE,
        runProc(stepSize(s, p, true, 10))),
    cmd("type.decreaseSizeMore", "Decrease Type Size by 10", [], "Cmd+Alt+Shift+,", sizeParams, hasTypeLayerE,
        runProc(stepSize(s, p, false, 10))),
  ]
