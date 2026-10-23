## Character Styles and Paragraph Styles (port of `type_styles_cmds.rs`; Window › Character
## Styles / Paragraph Styles).
##
## `type.characterStyle.*` and `type.paragraphStyle.*`: new, duplicate, delete, rename, set
## (Style Options), apply, redefine (from the selected text), clear override and list. The style
## model is the document's `TextStyles`; text runs keep fully resolved attributes plus a style
## reference, so whatever differs from the referenced styles is a local override ("+").
##
## Text targets: `layer` (one type layer) or `layers` (several), default the selected type
## layers; `range: [startChar, endChar]` restricts a single layer to part of its text.
## Changing a style's definition re-renders every type layer that uses it, keeping overrides.

import std/[json, options, algorithm, sets, sequtils, tables]
import ./core
import ../text/fonts as tfonts
import ../text/layout as tlayout
import ../textutil

const Family = tfonts.DefaultFamily

proc exhaustedId(paragraph: bool): ref EngineError =
  other((if paragraph: "paragraph" else: "character") & " style id space exhausted")

proc stylesHasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")

# ---------------------------------------------------------------- JSON attribute sets

proc attrsOf(n: JsonNode): JsonNode =
  ## A style's attribute set (an empty object for a missing one).
  if n.isNil or n.kind != JObject: newJObject() else: n

proc extendJ(dst: var JsonNode; src: JsonNode) =
  ## `Map::extend`: later keys replace earlier ones.
  if dst.isNil or dst.kind != JObject: dst = newJObject()
  if src.isNil or src.kind != JObject: return
  for k, v in src.fields: dst[k] = v

proc dropKey(n: JsonNode; k: string) =
  ## `Map::remove`: no error for a missing key.
  if not n.isNil and n.kind == JObject and n.hasKey(k): n.delete(k)

proc sortedKeys(n: JsonNode): seq[string] =
  if n.isNil or n.kind != JObject: return
  for k in n.fields.keys: result.add k
  result.sort(cmp[string])

proc cloneStyles(st: TextStyles): TextStyles =
  ## A deep copy: attribute sets are shared `JsonNode`s, and history keeps the old document.
  result = st
  for d in result.character.mitems: d.attrs = copy(attrsOf(d.attrs))
  for d in result.paragraph.mitems:
    d.paraAttrs = copy(attrsOf(d.paraAttrs))
    d.charAttrs = copy(attrsOf(d.charAttrs))
  result.basic.paraAttrs = copy(attrsOf(result.basic.paraAttrs))
  result.basic.charAttrs = copy(attrsOf(result.basic.charAttrs))

proc charStyleDef(st: TextStyles; id: uint32): Option[CharacterStyleDef] =
  let i = st.charStyleIndex(id)
  if i >= 0: some(st.character[i]) else: none(CharacterStyleDef)

proc paraStylePtr(st: var TextStyles; id: uint32): ptr ParagraphStyleDef =
  if id == 0: return addr st.basic
  for d in st.paragraph.mitems:
    if d.id == id: return addr d
  nil

proc charStylePtr(st: var TextStyles; id: uint32): ptr CharacterStyleDef =
  for d in st.character.mitems:
    if d.id == id: return addr d
  nil

proc validateAttrs[T](base: T; attrs: JsonNode): string =
  ## `text_styles::validate`: "" when `attrs` only names fields of `T` with values of the right
  ## type, else serde's message.
  try:
    withSerdeStrict:
      discard applyAttrs(base, attrs)
    ""
  except SerdeError as e: e.msg

proc modelKeys(n: JsonNode): HashSet[string] =
  for k in n.fields.keys:
    if k != "style_sheet": result.incl k

# ---------------------------------------------------------------- targets

proc textOf(doc: Document; id: LayerId): ptr TextLayer =
  let l = doc.findLayer(id)
  if l.isNil or l[].content.kind != lkText: nil else: addr l[].content.text

proc styleTargets(s: Session; p: JsonNode): seq[LayerId] =
  ## Type layers a style command acts on: `layer`, `layers`, else the selected type layers.
  let st = s.active
  if st.isNil: return
  var ids: seq[LayerId]
  let layer = p.pget("layer").asU64
  if layer.isSome: ids = @[LayerId(layer.get)]
  elif p.pget("layers").isArray:
    for x in p.pget("layers").elems:
      let u = x.asU64
      if u.isSome: ids.add LayerId(u.get)
  else: ids = st[].selectedOrdered
  for id in ids:
    if not textOf(st.doc[], id).isNil: result.add id

proc hasStyleTargetE(s: Session): string =
  let why = stylesHasDocE(s)
  if why.len > 0: return why
  if styleTargets(s, newJNull()).len == 0: "select a type layer" else: ""

# ---------------------------------------------------------------- attribute parameters

proc perturbedChar(): CharStyle =
  ## A character style with every field changed from the default, to see which fields a set of
  ## Character-panel keys touches.
  CharStyle(fontFamily: "\x01", fontStyle: "\x01", postscriptName: some("\x01"), weight: 1, italic: true,
            sizePt: -1, color: rgba(0.123, 0.456, 0.789, 0.5), tracking: -12345, leadingPt: some(-1'f32),
            baselineShiftPt: -1, horizontalScale: -1, verticalScale: -1, underline: true, strikethrough: true,
            fauxBold: true, fauxItalic: true, kerning: kOff, kern: -12345, caps: capsAllCaps, ligatures: false,
            discretionaryLigatures: true, features: @[FontFeature(tag: "\x01", value: 9)],
            variations: @[FontVariation(axis: "\x01", value: -1)], language: some("\x01"))

proc perturbedPara(): ParagraphStyle =
  ParagraphStyle(align: taJustifyAll, firstLineIndentPt: -1, startIndentPt: -1, endIndentPt: -1, spaceBeforePt: -1,
                 spaceAfterPt: -1, autoLeading: -1, direction: tdRtl, hyphenate: true)

proc touched[T](default, perturbed: T; apply: proc(x: var T)): JsonNode =
  ## Fields set by Character/Paragraph-panel keys (`size`, `font`, `align`, …): the fields whose
  ## value is the same whatever they were before.
  var (a, b) = (default, perturbed)
  apply(a)
  apply(b)
  let (oa, ob) = (toJ(a), toJ(b))
  result = newJObject()
  if oa.kind != JObject or ob.kind != JObject: return
  for k, v in oa.fields:
    if k != "style_sheet" and ob.hasKey(k) and ob[k] == v: result[k] = v

proc parseAttrs(cmd: string; v: JsonNode): (JsonNode, JsonNode) =
  ## Splits `attrs` into character and paragraph attribute sets. Keys may be model field names
  ## (`size_pt`, `align`: values as in `type.info`) or the Character/Paragraph panel keys of
  ## `type.setStyle` (`size`, `font`, `align`, …).
  if v.isNil or v.kind != JObject:
    if v.isNil or v.kind == JNull: return (newJObject(), newJObject())
    raise bad(cmd, "`attrs` must be an object")
  let charKeys = modelKeys(toJ(defaultCharStyle()))
  let paraKeys = modelKeys(toJ(defaultParagraphStyle()))
  var (c, pa, friendly) = (newJObject(), newJObject(), newJObject())
  for k in sortedKeys(v):
    let x = v[k]
    # Names shared by the model and the panels (`color`, `align`, `caps`, …) are read as
    # model values when they parse as such, else as panel values ("#ff0000", "center").
    let one = newJObject()
    one[k] = x
    if k in charKeys and validateAttrs(defaultCharStyle(), one).len == 0: c[k] = x
    elif k in paraKeys and validateAttrs(defaultParagraphStyle(), one).len == 0: pa[k] = x
    else: friendly[k] = x
  if friendly.len > 0:
    let f = friendly
    let fc = touched(defaultCharStyle(), perturbedChar(), proc(st: var CharStyle) = discard applyCharProps(st, f))
    let fp = touched(defaultParagraphStyle(), perturbedPara(), proc(st: var ParagraphStyle) = discard applyParaProps(st, f))
    if fc.len == 0 and fp.len == 0:
      var names: seq[string]
      for k in sortedKeys(friendly): names.add k
      var msg = "unknown style attributes: "
      for i, n in names:
        if i > 0: msg.add ", "
        msg.add n
      raise bad(cmd, msg)
    c.extendJ(fc)
    pa.extendJ(fp)
  let ec = validateAttrs(defaultCharStyle(), c)
  if ec.len > 0: raise bad(cmd, ec)
  let ep = validateAttrs(defaultParagraphStyle(), pa)
  if ep.len > 0: raise bad(cmd, ep)
  (c, pa)

# ---------------------------------------------------------------- restyling

type StyleOp = object
  ## What to do to the targeted text while re-resolving against new styles.
  setChar: Option[Option[uint32]]  ## set the character style of the range (`some(none)` = "None")
  setPara: Option[Option[uint32]]  ## set the paragraph style of paragraphs touching the range
  clearChar: bool                  ## drop character overrides in the range
  clearPara: bool                  ## drop paragraph overrides of paragraphs touching the range

type StyleTarget = (LayerId, Option[(int, int)])
  ## A type layer and an optional byte range in it.

proc restyle(t: var TextLayer; old, new: TextStyles; range: Option[(int, int)]; op: StyleOp): bool =
  ## Re-resolves every run and paragraph of `t` against `new` (keeping overrides made against
  ## `old`), applying `op` to bytes `a..b`. Returns whether anything changed.
  let before = (t.runs, t.paragraphs)
  let text = t.text
  let (a, b) = range.get((0, text.len))
  let whole = range.isNone
  # Paragraphs.
  let oldParas = t.paragraphRuns
  var starts: seq[int]
  var acc = 0
  for r in oldParas:
    starts.add acc
    acc += r.len
  proc paraAt(off: int): ParagraphStyle =
    var idx = 0
    for i in countdown(starts.high, 0):
      if starts[i] <= off:
        idx = i
        break
    oldParas[idx].style
  var paras: seq[((int, int), ParagraphStyle, Option[uint32])] # range, new style, old ref
  for pr in tlayout.splitParagraphs(text):
    let oldP = paraAt(pr[0])
    let touches = whole or (pr[0] < b and pr[1] > a) or (a == b and a >= pr[0] and (a < pr[1] or pr[1] == text.len))
    let newRef = (if op.setPara.isSome and touches: op.setPara.get else: oldP.styleSheet)
    let np = (if op.clearPara and touches: new.resolvePara(newRef) else: new.restylePara(old, oldP, newRef))
    paras.add (pr, np, oldP.styleSheet)
  # Character runs, split at paragraph boundaries and the range edges.
  var cuts: seq[int]
  for p in paras: cuts.add p[0][0]
  cuts.add [a, b, text.len]
  var runs: seq[(int, CharStyle)]
  var at = 0
  for r in t.charRuns:
    let (rs, re) = (at, at + r.len)
    at = re
    var edges: seq[int]
    for c in cuts:
      if c > rs and c < re: edges.add c
    edges.sort()
    edges = edges.deduplicate(isSorted = true)
    edges.add re
    var s0 = rs
    for e in edges:
      if e <= s0 and rs != re: continue
      var pi = 0
      for i in countdown(paras.high, 0):
        if paras[i][0][0] <= s0:
          pi = i
          break
      let (oldPref, newPref) = (paras[pi][2], paras[pi][1].styleSheet)
      let inside = whole or (s0 >= a and e <= b and a < b)
      let chr = (if op.setChar.isSome and inside: op.setChar.get else: r.style.styleSheet)
      let st = (if op.clearChar and inside: new.resolveChar(newPref, chr, Family)
                else: new.restyleChar(old, r.style, oldPref, newPref, chr, Family))
      if runs.len > 0 and runs[^1][1] == st: runs[^1][0] += e - s0
      else: runs.add (e - s0, st)
      s0 = e
  var pruns: seq[(int, ParagraphStyle)]
  for (r, st, _) in paras:
    if pruns.len > 0 and pruns[^1][1] == st: pruns[^1][0] += r[1] - r[0]
    else: pruns.add (r[1] - r[0], st)
  if runs.len == 0:
    let cr = t.charRuns
    let first = (if cr.len > 0: cr[0].style else: defaultCharStyle())
    let pref = (if pruns.len > 0: pruns[0][1].styleSheet else: none(uint32))
    runs.add (0, new.restyleChar(old, first, none(uint32), pref, op.setChar.get(first.styleSheet), Family))
  t.runs = @[]
  for (len, style) in runs: t.runs.add TextRun(len: len, style: style)
  t.paragraphs = @[]
  for (len, style) in pruns: t.paragraphs.add ParagraphRun(len: len, style: style)
  t.syncSummary()
  (t.runs, t.paragraphs) != before

proc applyStyles(doc: var Document; new: TextStyles; target: seq[StyleTarget]; op: StyleOp): int =
  ## Replaces the document's styles with `new` and re-resolves every type layer, applying `op`
  ## to `target` (layer + optional byte range). Returns the number of layers that changed.
  let old = doc.textStyles
  doc.textStyles = cloneStyles(new)
  let snapshot = doc
  var ids: seq[LayerId]
  for (_, _, l) in doc.walk:
    if l.content.kind == lkText: ids.add l.id
  for id in ids:
    let l = doc.findLayer(id)
    if l.isNil or l[].content.kind != lkText: continue
    var tgt = none(Option[(int, int)])
    for (tid, r) in target:
      if tid == id:
        tgt = some(r)
        break
    let did = (if tgt.isSome: restyle(l[].content.text, old, new, tgt.get, op)
               else: restyle(l[].content.text, old, new, some((0, 0)), StyleOp()))
    if did:
      refreshText(snapshot, l[].content.text)
      inc result

proc commit(s: var Session; label: string; new: TextStyles; target: seq[StyleTarget]; op: StyleOp): int =
  ## The style change and the re-resolved layers as one history step.
  s.editWith(label, proc(doc: var Document; active: var Option[LayerId]): int = applyStyles(doc, new, target, op))

proc targetRanges(s: Session; p: JsonNode): seq[StyleTarget] =
  ## Target list with byte ranges (`range` applies only to a single layer).
  let st = s.active
  if st.isNil: raise noDocument()
  let ids = styleTargets(s, p)
  if ids.len == 0: raise other("no type layer to apply the style to")
  let ranged = not p.pget("range").isNil and p.pget("layers").isNil
  for id in ids:
    var r = none((int, int))
    if ranged:
      let t = textOf(st.doc[], id)
      if not t.isNil: r = some(rangeParam(t[].text, p))
    result.add (id, r)

type Sample = object
  run: CharStyle
  para: ParagraphStyle
  layer: LayerId
  span: (int, int)

proc sample(s: Session; p: JsonNode): Option[Sample] =
  ## The run and paragraph at the start of the target (for new/redefine), with the layer and the
  ## run's byte range.
  let st = s.active
  if st.isNil: return
  let ids = styleTargets(s, p)
  if ids.len == 0: return
  let t = textOf(st.doc[], ids[0])
  if t.isNil: return
  let (a, _) = rangeParam(t[].text, p)
  var at = 0
  var found = false
  var run: CharStyle
  var span: (int, int)
  for r in t[].charRuns:
    let (s0, e0) = (at, at + r.len)
    at = e0
    if a < e0 or e0 == t[].text.len:
      run = r.style
      span = (s0, e0)
      found = true
      break
  if not found: return
  at = 0
  for r in t[].paragraphRuns:
    let hit = a < at + r.len or at + r.len == t[].text.len
    at += r.len
    if hit: return some(Sample(run: run, para: r.style, layer: ids[0], span: span))

proc current(s: Session; p: JsonNode): JsonNode =
  ## Character / paragraph style state of the target: (char id, char override, para id, para
  ## override), over the whole target (mixed ids → null); null without a target.
  let st = s.active
  if st.isNil: return newJNull()
  let styles = st.doc[].textStyles
  var tr: seq[StyleTarget]
  try: tr = targetRanges(s, p)
  except EngineError: return newJNull()
  var chars, paras: seq[uint32]
  var cOver, pOver = false
  for (id, r) in tr:
    let t = textOf(st.doc[], id)
    if t.isNil: return newJNull()
    let (a, b) = r.get((0, t[].text.len))
    let tlen = t[].text.len
    var at = 0
    var paraSpans: seq[(int, int, Option[uint32])]
    for x in t[].paragraphRuns:
      let (s0, e0) = (at, at + x.len)
      at = e0
      if (s0 < b and e0 > a) or (a == b and a >= s0 and (a < e0 or e0 == tlen)) or tlen == 0:
        paras.add x.style.styleSheet.get(0)
        pOver = pOver or styles.paraOverrides(x.style).len > 0
      paraSpans.add (s0, e0, x.style.styleSheet)
    at = 0
    for x in t[].charRuns:
      let (s0, e0) = (at, at + x.len)
      at = e0
      if (s0 < b and e0 > a) or (a == b and a >= s0 and a <= e0) or tlen == 0:
        var pref = none(uint32)
        for i in countdown(paraSpans.high, 0):
          if paraSpans[i][0] <= s0:
            pref = paraSpans[i][2]
            break
        chars.add x.style.styleSheet.get(0)
        cOver = cOver or styles.charOverrides(x.style, pref, Family).len > 0
  proc one(v: seq[uint32]): JsonNode =
    if v.len == 0: return newJNull()
    for x in v:
      if x != v[0]: return newJNull()
    newJInt(BiggestInt(v[0]))
  %*{"character": one(chars), "characterOverride": cOver, "paragraph": one(paras), "paragraphOverride": pOver}

# ---------------------------------------------------------------- commands

proc styles(s: Session): TextStyles =
  let st = s.active
  if st.isNil: raise noDocument()
  cloneStyles(st.doc[].textStyles)

proc charJson(d: CharacterStyleDef; styles: TextStyles): JsonNode =
  %*{"id": d.id, "name": d.name, "attrs": attrsOf(d.attrs), "resolved": toJ(styles.resolveChar(none(uint32), some(d.id), Family))}

proc paraJson(d: ParagraphStyleDef; styles: TextStyles): JsonNode =
  %*{"id": d.id, "name": d.name, "paragraphAttrs": attrsOf(d.paraAttrs), "characterAttrs": attrsOf(d.charAttrs),
     "resolved": {"paragraph": toJ(styles.resolvePara(some(d.id))),
                  "character": toJ(styles.resolveChar(some(d.id), none(uint32), Family))}}

proc list(s: Session; p: JsonNode; paragraph: bool): JsonNode =
  let st = styles(s)
  let cur = current(s, p)
  var v = newJArray()
  if paragraph:
    v.add paraJson(st.basic, st)
    for d in st.paragraph: v.add paraJson(d, st)
  else:
    v.add %*{"id": 0, "name": NoCharacterStyle, "attrs": newJObject()}
    for d in st.character: v.add charJson(d, st)
  %*{"styles": v, "current": cur}

proc newStyle(s: var Session; p: JsonNode; paragraph: bool): JsonNode =
  let cmd = if paragraph: "type.paragraphStyle.new" else: "type.characterStyle.new"
  var st = styles(s)
  var (ca, pa) = parseAttrs(cmd, p.pget("attrs"))
  # With a type layer targeted, the style starts from the text's formatting (as Photoshop).
  let fromText = p.pget("fromSelection").asBool.get(true)
  let smp = (if fromText: sample(s, p) else: none(Sample))
  if smp.isSome:
    let (run, para) = (smp.get.run, smp.get.para)
    if paragraph:
      let baseC = st.resolveChar(none(uint32), none(uint32), Family)
      var r = run
      r.styleSheet = none(uint32)
      var c = diffAttrs(r, baseC)
      c.extendJ(ca)
      ca = c
      var pp = diffAttrs(para, st.resolvePara(none(uint32)))
      pp.extendJ(pa)
      pa = pp
    else:
      let baseC = st.resolveChar(para.styleSheet, none(uint32), Family)
      var c = diffAttrs(run, baseC)
      c.extendJ(ca)
      ca = c
  let nameArg = p.pget("name").asStr
  let name = (if nameArg.isSome: nameArg.get
              else: st.uniqueName(paragraph, if paragraph: "Paragraph Style" else: "Character Style"))
  var id: uint32
  if paragraph:
    let nid = st.nextParaId
    if nid.isNone: raise exhaustedId(true)
    id = nid.get
    st.paragraph.add ParagraphStyleDef(id: id, name: name, paraAttrs: pa, charAttrs: ca)
  else:
    let nid = st.nextCharId
    if nid.isNone: raise exhaustedId(false)
    id = nid.get
    st.character.add CharacterStyleDef(id: id, name: name, attrs: ca)
  var target: seq[StyleTarget]
  var op: StyleOp
  if p.pget("apply").asBool.get(false):
    if paragraph: op.setPara = some(some(id)) else: op.setChar = some(some(id))
    target = targetRanges(s, p)
  discard commit(s, if paragraph: "New Paragraph Style" else: "New Character Style", st, target, op)
  %*{"id": id, "name": name}

proc lookupId(cmd: string; st: TextStyles; p: JsonNode; paragraph, allowDefault: bool): uint32 =
  let given = u32IdParam(cmd, p, "id")
  if given.isNone: raise bad(cmd, "missing `id`")
  let id = given.get
  let ok = if paragraph: st.paraStyle(id).isSome else: st.charStyleIndex(id) >= 0
  if not (ok or (allowDefault and id == 0)): raise bad(cmd, "no style with id " & $id)
  if id == 0 and not allowDefault:
    raise bad(cmd, if paragraph: "Basic Paragraph can't be changed this way" else: "\"None\" is not a style")
  id

proc duplicate(s: var Session; p: JsonNode; paragraph: bool): JsonNode =
  let cmd = if paragraph: "type.paragraphStyle.duplicate" else: "type.characterStyle.duplicate"
  var st = styles(s)
  let id = lookupId(cmd, st, p, paragraph, paragraph)
  var newId: uint32
  var name: string
  if paragraph:
    let srcOpt = st.paraStyle(id)
    let src = (if srcOpt.isSome: srcOpt.get else: ParagraphStyleDef(paraAttrs: newJObject(), charAttrs: newJObject()))
    name = src.name & " copy"
    let nid = st.nextParaId
    if nid.isNone: raise exhaustedId(true)
    newId = nid.get
    var pos = 0
    for i, d in st.paragraph:
      if d.id == id:
        pos = i + 1
        break
    st.paragraph.insert(ParagraphStyleDef(id: newId, name: name, paraAttrs: copy(attrsOf(src.paraAttrs)),
                                          charAttrs: copy(attrsOf(src.charAttrs))), pos)
  else:
    let srcOpt = st.charStyleDef(id)
    let src = (if srcOpt.isSome: srcOpt.get else: CharacterStyleDef(attrs: newJObject()))
    name = src.name & " copy"
    let nid = st.nextCharId
    if nid.isNone: raise exhaustedId(false)
    newId = nid.get
    var pos = 0
    for i, d in st.character:
      if d.id == id:
        pos = i + 1
        break
    st.character.insert(CharacterStyleDef(id: newId, name: name, attrs: copy(attrsOf(src.attrs))), pos)
  discard commit(s, "Duplicate Style", st, @[], StyleOp())
  %*{"id": newId, "name": name}

proc deleteStyle(s: var Session; p: JsonNode; paragraph: bool): JsonNode =
  let cmd = if paragraph: "type.paragraphStyle.delete" else: "type.characterStyle.delete"
  let st = styles(s)
  let id = lookupId(cmd, st, p, paragraph, false)
  # Text keeps its look: references are dropped and the attributes become overrides.
  let n = s.editWith("Delete Style", proc(doc: var Document; active: var Option[LayerId]): int =
    if paragraph: doc.textStyles.paragraph.keepItIf(it.id != id)
    else: doc.textStyles.character.keepItIf(it.id != id)
    let snapshot = doc
    var ids: seq[LayerId]
    for (_, _, l) in doc.walk: ids.add l.id
    for lid in ids:
      let l = doc.findLayer(lid)
      if l.isNil or l[].content.kind != lkText: continue
      template t: untyped = l[].content.text
      var hit = false
      if paragraph:
        for r in t.paragraphs.mitems:
          if r.style.styleSheet == some(id):
            r.style.styleSheet = none(uint32)
            hit = true
      else:
        for r in t.runs.mitems:
          if r.style.styleSheet == some(id):
            r.style.styleSheet = none(uint32)
            hit = true
      if hit:
        refreshText(snapshot, t)
        inc result)
  %*{"deleted": id, "layers": n}

proc trimmedName(p: JsonNode): Option[string] =
  ## `name`, trimmed, when non-empty.
  let n = p.pget("name").asStr
  if n.isNone: return
  let t = trimRust(n.get)
  if t.len > 0: some(t) else: none(string)

proc rename(s: var Session; p: JsonNode; paragraph: bool): JsonNode =
  let cmd = if paragraph: "type.paragraphStyle.rename" else: "type.characterStyle.rename"
  var st = styles(s)
  let id = lookupId(cmd, st, p, paragraph, false)
  let name = trimmedName(p)
  if name.isNone: raise bad(cmd, "missing `name`")
  if paragraph:
    let d = st.paraStylePtr(id)
    if not d.isNil: d[].name = name.get
  else:
    let d = st.charStylePtr(id)
    if not d.isNil: d[].name = name.get
  discard commit(s, "Rename Style", st, @[], StyleOp())
  %*{"id": id, "name": name.get}

proc prepareOptions(st0: TextStyles; p: JsonNode; paragraph: bool): (TextStyles, uint32) =
  let cmd = if paragraph: "type.paragraphStyle.set" else: "type.characterStyle.set"
  var st = st0
  let id = lookupId(cmd, st, p, paragraph, paragraph)
  let (ca, pa) = parseAttrs(cmd, p.pget("attrs"))
  let replace = p.pget("replace").asBool.get(false)
  var clear: seq[string]
  if p.pget("clear").isArray:
    for x in p.pget("clear").elems:
      if x.asStr.isSome: clear.add x.str
  let name = trimmedName(p)
  proc merge(dst: var JsonNode; src: JsonNode) =
    if replace or dst.isNil or dst.kind != JObject: dst = newJObject()
    dst.extendJ(src)
    for k in clear: dst.dropKey(k)
  if paragraph:
    let d = st.paraStylePtr(id)
    if d.isNil: raise bad(cmd, "no such style")
    merge(d[].charAttrs, ca)
    if replace or d[].paraAttrs.isNil or d[].paraAttrs.kind != JObject: d[].paraAttrs = newJObject()
    d[].paraAttrs.extendJ(pa)
    for k in clear: d[].paraAttrs.dropKey(k)
    if name.isSome and id != 0: d[].name = name.get
  else:
    if pa.len > 0: raise bad(cmd, "character styles have no paragraph attributes")
    let d = st.charStylePtr(id)
    if d.isNil: raise bad(cmd, "no such style")
    merge(d[].attrs, ca)
    if name.isSome: d[].name = name.get
  (st, id)

proc previewOptions*(doc: Document; p: JsonNode; paragraph: bool): Document =
  ## Apply Style Options to an isolated document, using exactly the command's resolution and
  ## rendering logic. The source document and the caller's session/history remain untouched.
  let (new, _) = prepareOptions(cloneStyles(doc.textStyles), p, paragraph)
  result = doc
  discard applyStyles(result, new, @[], StyleOp())

proc setOptions(s: var Session; p: JsonNode; paragraph: bool): JsonNode =
  let (st, id) = prepareOptions(styles(s), p, paragraph)
  let layers = commit(s, "Style Options", st, @[], StyleOp())
  result = list(s, newJObject(), paragraph)
  result["layers"] = %layers
  result["id"] = %id

proc applyStyle(s: var Session; p: JsonNode; paragraph: bool): JsonNode =
  let cmd = if paragraph: "type.paragraphStyle.apply" else: "type.characterStyle.apply"
  let st = styles(s)
  let idNode = p.pget("id")
  let id = (if idNode.isNil or idNode.kind == JNull: 0'u32 else: lookupId(cmd, st, p, paragraph, true))
  let target = targetRanges(s, p)
  let clear = p.pget("clearOverrides").asBool.get(false)
  let r = (if id != 0: some(id) else: none(uint32))
  let op = (if paragraph: StyleOp(setPara: some(r), clearPara: clear, clearChar: clear)
            else: StyleOp(setChar: some(r), clearChar: clear))
  let n = commit(s, if paragraph: "Apply Paragraph Style" else: "Apply Character Style", st, target, op)
  %*{"id": id, "layers": n, "current": current(s, p)}

proc clearOverride(s: var Session; p: JsonNode; paragraph: bool): JsonNode =
  let st = styles(s)
  let target = targetRanges(s, p)
  let op = (if paragraph: StyleOp(clearPara: true, clearChar: true) else: StyleOp(clearChar: true))
  let n = commit(s, "Clear Override", st, target, op)
  %*{"layers": n, "current": current(s, p)}

proc redefine(s: var Session; p: JsonNode; paragraph: bool): JsonNode =
  let cmd = if paragraph: "type.paragraphStyle.redefine" else: "type.characterStyle.redefine"
  var st = styles(s)
  let smp = sample(s, p)
  if smp.isNone: raise bad(cmd, "select type to redefine the style from")
  let (run, para, layer, span) = (smp.get.run, smp.get.para, smp.get.layer, smp.get.span)
  var id: uint32
  if u32IdParam(cmd, p, "id").isSome: id = lookupId(cmd, st, p, paragraph, paragraph)
  elif paragraph: id = para.styleSheet.get(0)
  elif run.styleSheet.isSome: id = run.styleSheet.get
  else: raise bad(cmd, "the selected text has no character style")
  if paragraph:
    var charKeys: HashSet[string]
    if run.styleSheet.isSome:
      let d = st.charStyleDef(run.styleSheet.get)
      if d.isSome:
        for k in attrsOf(d.get.attrs).fields.keys: charKeys.incl k
    let ov = st.charOverrides(run, (if id != 0: some(id) else: none(uint32)), Family)
    var cOver = newJObject()
    for k, v in ov.fields:
      if k notin charKeys: cOver[k] = v
    let pOver = diffAttrs(para, st.resolvePara(some(id)))
    let d = st.paraStylePtr(id)
    if d.isNil: raise bad(cmd, "no such style")
    d[].charAttrs.extendJ(cOver)
    d[].paraAttrs.extendJ(pOver)
  else:
    var r = run
    r.styleSheet = some(id)
    let over = st.charOverrides(r, para.styleSheet, Family)
    let d = st.charStylePtr(id)
    if d.isNil: raise bad(cmd, "no such style")
    d[].attrs.extendJ(over)
  # The sampled run gets the style (its overrides are now part of it); others follow.
  let target = @[(layer, some(span))]
  let op = (if paragraph: StyleOp(setPara: some(if id != 0: some(id) else: none(uint32)))
            else: StyleOp(setChar: some(some(id))))
  let n = commit(s, if paragraph: "Redefine Paragraph Style" else: "Redefine Character Style", st, target, op)
  %*{"id": id, "layers": n, "current": current(s, p)}

proc typeStylesSpecs*(): seq[CommandSpec] =
  const
    TargetDoc = """"layer":id? | "layers":[id]? (default: the selected type layers),"range":[startChar,endChar]? (one layer; default all text)"""
    AttrsDoc = """"attrs":{model fields as in type.info ("size_pt":36,"font_family":"Inter","align":"Center",…) or Character/Paragraph panel keys as in type.setStyle ("size":36,"font":"Inter","color":"#rrggbb","align":"center",…)}"""
  template styleSpecs(paragraph: static bool; prefix, noun: static string): seq[CommandSpec] =
    @[cmd(prefix & ".new", "New " & noun & " Style", [], "",
          """{"name":str?,""" & AttrsDoc & """?,"fromSelection":bool=true (start from the targeted text's formatting),"apply":bool=false,""" & TargetDoc & """} → {"id","name"}""",
          enabledProc(stylesHasDocE(s)), runProc(newStyle(s, p, paragraph))),
      cmd(prefix & ".duplicate", "Duplicate Style", [], "", """{"id":u32}""", enabledProc(stylesHasDocE(s)),
          runProc(duplicate(s, p, paragraph))),
      cmd(prefix & ".delete", "Delete Style", [], "", """{"id":u32} (text keeps its formatting as overrides)""",
          enabledProc(stylesHasDocE(s)), runProc(deleteStyle(s, p, paragraph))),
      cmd(prefix & ".rename", "Rename Style", [], "", """{"id":u32,"name":str}""", enabledProc(stylesHasDocE(s)),
          runProc(rename(s, p, paragraph))),
      cmd(prefix & ".set", noun & " Style Options", [], "",
          """{"id":u32 (paragraph: 0 = Basic Paragraph),"name":str?,""" & AttrsDoc & ""","replace":bool=false (drop attributes not given),"clear":[field]?} (text using the style updates, overrides kept)""",
          enabledProc(stylesHasDocE(s)), runProc(setOptions(s, p, paragraph))),
      cmd(prefix & ".apply", "Apply " & noun & " Style", [], "",
          """{"id":u32|0|null (0/null: None / Basic Paragraph),"clearOverrides":bool=false (Alt-click),""" & TargetDoc & "}",
          enabledProc(hasStyleTargetE(s)), runProc(applyStyle(s, p, paragraph))),
      cmd(prefix & ".redefine", "Redefine " & noun & " Style", [], "",
          """{"id":u32? (default: the targeted text's style),""" & TargetDoc & "} (the style takes the formatting of the start of the targeted text)",
          enabledProc(hasStyleTargetE(s)), runProc(redefine(s, p, paragraph))),
      cmd(prefix & ".clearOverride", "Clear Override", [], "", "{" & TargetDoc & "}", enabledProc(hasStyleTargetE(s)),
          runProc(clearOverride(s, p, paragraph))),
      cmd(prefix & ".list", "List " & noun & " Styles", [], "",
          "{" & TargetDoc & """} → {"styles":[{"id","name",attributes,"resolved"}],"current":{"character":id|null (mixed),"characterOverride":bool,"paragraph":id|null,"paragraphOverride":bool}}""",
          enabledProc(stylesHasDocE(s)), runProc(list(s, p, paragraph)), journal = false)]
  result = styleSpecs(false, "type.characterStyle", "Character")
  result.add styleSpecs(true, "type.paragraphStyle", "Paragraph")
