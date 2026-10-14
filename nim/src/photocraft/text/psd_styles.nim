## Named character and paragraph styles in the PSD text engine data (port of
## `photocraft_text::psd_styles`).
##
## Every type layer's `EngineData` carries the document's style sheets in its `ResourceDict`
## (and `DocumentResources`): `StyleSheetSet` (character style sheets: `Name`,
## `StyleSheetData`) and `ParagraphSheetSet` (`Name`, `DefaultStyleSheet`, `Properties`), with
## `TheNormalStyleSheet` / `TheNormalParagraphSheet` naming the defaults. Runs refer to a sheet
## from their `StyleSheet` / `ParagraphSheet` dictionaries.
##
## How Photoshop links a run to a named sheet isn't in Adobe's published PSD spec. We read both
## a `Parent` index and a `Name` on the run's sheet, and write `Parent` (plus the run's fully
## resolved attributes, so the text looks the same whatever a reader makes of the reference).
## Named sheets are written with fully resolved attributes too. Unverified against Photoshop.

import std/options
import ../doc
import ../psd/descriptor
import engine_data, fonts, psd_text

type PsdStyleSheets* = object
  ## Style sheets read from one type layer.
  normalChar*: CharStyle          ## the default ("Normal") character sheet, resolved
  normalPara*: ParagraphStyle
  charSheets*: seq[(int, string, CharStyle)]
    ## named character sheets other than the normal one: (set index, name, resolved style)
  paraSheets*: seq[(int, string, ParagraphStyle, Option[CharStyle])]
    ## named paragraph sheets: (set index, name, paragraph style, character style of its
    ## `DefaultStyleSheet` when that isn't the normal sheet)
  runChar*: seq[Option[int]]      ## per `StyleRun` entry: the character sheet it refers to
  runCharLens*: seq[int]          ## per `StyleRun` entry: its length in UTF-8 bytes of the text
  runPara*: seq[Option[int]]      ## per `ParagraphRun` entry: the paragraph sheet it refers to

proc kOf(dpi: float32): float32 = 72'f32 / (if dpi > 0: dpi else: 72'f32)

proc names(set: Option[EdValue]): seq[string] =
  let a = set.asArray
  if a.isNone: return
  for s in a.get: result.add s.get("Name").asStr.get("")

proc asIndex(v: Option[int64]): int =
  ## `i64 as usize`: negative values index nothing.
  if v.isNone: 0 elif v.get < 0: high(int) else: int(v.get)

proc reference(sheet: Option[EdValue]; setNames: openArray[string]; normal: int): Option[int] =
  ## Index a run's sheet dictionary refers to (`Parent` index, else `Name`), skipping `normal`.
  if sheet.isNone: return
  let s = sheet.get
  var i = -1
  let parent = s.get("Parent").asI64
  if parent.isSome: i = (if parent.get < 0: high(int) else: int(parent.get))
  else:
    let n = s.get("Name").asStr
    if n.isNone: return
    i = setNames.find(n.get)
    if i < 0: return
  if i != normal and i < setNames.len: some(i) else: none(int)

proc readStyleSheets*(tysh: openArray[byte]; dpi: float32): Option[PsdStyleSheets] =
  ## Reads the style sheets of a `TySh` block. None when it has no engine data.
  let t = parseTysh(tysh)
  if t.isNone: return
  let ev = engineData(t.get.text)
  if ev.isNone: return
  let e = ev.get
  let k = kOf(dpi)
  var resO = e.get("ResourceDict")
  if resO.isNone: resO = e.get("DocumentResources")
  if resO.isNone: return
  let res = resO.get
  let fonts = names(res.get("FontSet"))
  let sset = res.get("StyleSheetSet").asArray.get(@[])
  let pset = res.get("ParagraphSheetSet").asArray.get(@[])
  let normalC = asIndex(res.get("TheNormalStyleSheet").asI64)
  let normalP = asIndex(res.get("TheNormalParagraphSheet").asI64)
  let empty = edDict()
  let baseData = if normalC < sset.len: sset[normalC].get("StyleSheetData") else: none(EdValue)
  let normalChar = charStyle(none(EdValue), baseData.get(empty), fonts, k)
  let baseProps = if normalP < pset.len: pset[normalP].get("Properties") else: none(EdValue)
  let normalPara = paraStyle(none(EdValue), baseProps.get(empty), k)
  var o = PsdStyleSheets(normalChar: normalChar, normalPara: normalPara)
  for i, s in sset:
    if i == normalC: continue
    let name = s.get("Name").asStr.get("")
    let data = s.get("StyleSheetData").get(empty)
    o.charSheets.add (i, name, charStyle(baseData, data, fonts, k))
  for i, s in pset:
    if i == normalP: continue
    let name = s.get("Name").asStr.get("")
    let props = s.get("Properties").get(empty)
    var cs = none(CharStyle)
    let dss = s.get("DefaultStyleSheet").asI64
    if dss.isSome:
      let d = asIndex(dss)
      if d != normalC and d < sset.len:
        cs = some(charStyle(baseData, sset[d].get("StyleSheetData").get(empty), fonts, k))
    o.paraSheets.add (i, name, paraStyle(baseProps, props, k), cs)
  let snames = names(res.get("StyleSheetSet"))
  let pnames = names(res.get("ParagraphSheetSet"))
  let ra = e.path(["EngineDict", "StyleRun", "RunArray"]).asArray
  if ra.isSome:
    for r in ra.get: o.runChar.add reference(r.get("StyleSheet"), snames, normalC)
    let edText = e.path(["EngineDict", "Editor", "Text"]).asStr.get("")
    o.runCharLens = utf16ToByteLengths(edText, arrF(e.path(["EngineDict", "StyleRun", "RunLengthArray"])))
  let pa = e.path(["EngineDict", "ParagraphRun", "RunArray"]).asArray
  if pa.isSome:
    for r in pa.get: o.runPara.add reference(r.get("ParagraphSheet"), pnames, normalP)
  some(o)

proc hasNamedSheets*(tysh: openArray[byte]): bool =
  ## Whether a `TySh` block carries named sheets beyond the normal ones.
  let t = parseTysh(tysh)
  if t.isNone: return false
  let e = engineData(t.get.text)
  if e.isNone: return false
  let res = e.get.get("ResourceDict")
  if res.isNone: return false
  proc n(key: string): int = res.get(key).asArray.get(@[]).len
  n("StyleSheetSet") > 1 or n("ParagraphSheetSet") > 1

proc writeStyleSheets*(tysh: openArray[byte]; layer: TextLayer; styles: TextStyles; dpi: float32): Option[seq[byte]] =
  ## Rewrites the style sheets of a `TySh` block from the document's styles and the layer's run
  ## references. The normal sheets are kept; other sheets are replaced by `styles`.
  let tp = parseTysh(tysh)
  if tp.isNone: return
  var t = tp.get
  let ev = engineData(t.text)
  if ev.isNone: return
  var e = ev.get
  let k = kOf(dpi)
  for key in ["ResourceDict", "DocumentResources"]:
    let rd = e.getMut(key)
    if rd == nil: continue
    var fonts = names(rd[].get("FontSet"))
    let normalC = asIndex(rd[].get("TheNormalStyleSheet").asI64)
    let normalP = asIndex(rd[].get("TheNormalParagraphSheet").asI64)
    proc keep(a: Option[seq[EdValue]]; i: int): Option[EdValue] =
      if a.isNone or a.get.len == 0: none(EdValue)
      elif i < a.get.len: some(a.get[i])
      else: some(a.get[0])
    let keepC = keep(rd[].get("StyleSheetSet").asArray, normalC)
    let keepP = keep(rd[].get("ParagraphSheetSet").asArray, normalP)
    proc fontIndex(fonts: var seq[string]; s: CharStyle): int =
      let ps = postscriptFor(s)
      let i = fonts.find(ps)
      if i >= 0: return i
      fonts.add ps
      fonts.len - 1
    var sset: seq[EdValue]
    if keepC.isSome: sset.add keepC.get
    for d in styles.character:
      let st = styles.resolveChar(none(uint32), some(d.id), DefaultFamily)
      let fi = fonts.fontIndex(st)
      sset.add edDict(@[("Name", edString(d.name)), ("Parent", edInt(0)), ("StyleSheetData", styleSheetData(st, fi, k))])
    var pset: seq[EdValue]
    if keepP.isSome: pset.add keepP.get
    for d in styles.paragraph:
      let p = styles.resolvePara(some(d.id))
      pset.add edDict(@[("Name", edString(d.name)), ("DefaultStyleSheet", edInt(0)), ("Parent", edInt(0)),
                        ("Properties", paragraphProperties(p, k))])
    rd[].set("StyleSheetSet", edArray(sset))
    rd[].set("ParagraphSheetSet", edArray(pset))
    rd[].set("TheNormalStyleSheet", edInt(0))
    rd[].set("TheNormalParagraphSheet", edInt(0))
    var fs: seq[EdValue]
    for f in fonts: fs.add fontEntry(f)
    rd[].set("FontSet", edArray(fs))
  proc charIx(id: Option[uint32]): Option[int64] =
    if id.isNone: return
    for i, d in styles.character:
      if d.id == id.get: return some(int64(i) + 1)
  proc paraIx(id: Option[uint32]): Option[int64] =
    if id.isNone: return
    for i, d in styles.paragraph:
      if d.id == id.get: return some(int64(i) + 1)
  let runs = layer.charRuns
  # Each entry lies inside one model run: find it by the entry's start offset.
  let edText = e.path(["EngineDict", "Editor", "Text"]).asStr.get("")
  let entryLens = utf16ToByteLengths(edText, arrF(e.path(["EngineDict", "StyleRun", "RunLengthArray"])))
  proc runAt(off: int): Option[TextRun] =
    var stop = 0
    for r in runs:
      stop = if stop > high(int) - r.len: high(int) else: stop + r.len
      if off < stop: return some(r)
    if runs.len > 0: some(runs[^1]) else: none(TextRun)
  let ed = e.getMut("EngineDict")
  if ed != nil:
    let sr = ed[].getMut("StyleRun")
    if sr != nil:
      let a = sr[].getMut("RunArray")
      if a != nil and a[].kind == ekArray:
        var off = 0
        for i in 0 ..< a[].elems.len:
          let start = off
          let add = if i < entryLens.len: entryLens[i] else: 0
          off = if off > high(int) - add: high(int) else: off + add
          let sheet = a[].elems[i].getMut("StyleSheet")
          if sheet != nil:
            sheet[].remove("Parent")
            sheet[].remove("Name")
            let r = runAt(start)
            let ix = charIx(if r.isSome: r.get.style.styleSheet else: none(uint32))
            if ix.isSome: sheet[].set("Parent", edInt(ix.get))
  let paras = layer.paragraphRuns
  if ed != nil:
    let pr = ed[].getMut("ParagraphRun")
    if pr != nil:
      let a = pr[].getMut("RunArray")
      if a != nil and a[].kind == ekArray:
        for i in 0 ..< a[].elems.len:
          let sheet = a[].elems[i].getMut("ParagraphSheet")
          if sheet != nil:
            sheet[].remove("Parent")
            sheet[].remove("Name")
            let ix = paraIx(if i < paras.len: paras[i].style.styleSheet else: none(uint32))
            if ix.isSome: sheet[].set("Parent", edInt(ix.get))
  let raw = writeEngineData(e)
  var found = false
  for i in 0 ..< t.text.items.len:
    if t.text.items[i][0].s == "EngineData":
      t.text.items[i][1] = Value(kind: vkRawData, raw: raw)
      found = true
      break
  if not found: return
  some(writeTysh(t))
