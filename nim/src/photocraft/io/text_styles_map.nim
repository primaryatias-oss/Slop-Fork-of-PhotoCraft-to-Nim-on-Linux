## Character and paragraph styles ⇄ the PSD text engine's style sheets (Rust
## `text_styles_map.rs`, over `text/psd_styles`). Import merges the named sheets of every type
## layer into the document's `TextStyles` by name and sets the runs' style references; export
## rewrites each type layer's sheets from the document's styles. Files without named styles are
## left as they are (byte-exact).

import std/[options, tables, json]
import ../doc
import ../text/psd_styles

proc link(t: var TextLayer; runChar: openArray[Option[int]]; runCharLens: openArray[int];
          cmap: Table[int, uint32]; runPara: openArray[Option[int]]; pmap: Table[int, uint32]) =
  proc lookup(ix: Option[int]; m: Table[int, uint32]): Option[uint32] =
    if ix.isSome and m.hasKey(ix.get): some(m[ix.get]) else: none(uint32)
  if runCharLens.len == runChar.len and runChar.len > 0:
    # Each model run takes the sheet of the `StyleRun` entry its first character is in.
    var off = 0
    var i = 0
    var stop = runCharLens[0]
    for r in t.runs.mitems:
      while off >= stop and i + 1 < runCharLens.len:
        inc i
        stop = if stop > high(int) - runCharLens[i]: high(int) else: stop + runCharLens[i]
      r.style.styleSheet = lookup(runChar[i], cmap)
      off = if off > high(int) - r.len: high(int) else: off + r.len
  elif t.runs.len == runChar.len:
    for k in 0 ..< t.runs.len: t.runs[k].style.styleSheet = lookup(runChar[k], cmap)
  if t.paragraphs.len == runPara.len:
    for k in 0 ..< t.paragraphs.len: t.paragraphs[k].style.styleSheet = lookup(runPara[k], pmap)

proc withoutPostscript(attrs: JsonNode): JsonNode =
  result = attrs
  if result.hasKey("postscript_name"): result.delete("postscript_name")

proc importStyles*(d: var Document): seq[string] =
  ## Builds `d.textStyles` from the type layers' engine data and links the runs.
  let dpi = d.resolutionDpi
  var styles = d.textStyles
  var charIds, paraIds: Table[string, uint32]
  for c in styles.character: charIds[c.name] = c.id
  for p in styles.paragraph: paraIds[p.name] = p.id
  for (_, lp) in d.layerPtrs:
    if lp[].content.kind != lkText: continue
    let raw = lp[].content.text.psdRaw
    if raw.isNil or not hasNamedSheets(raw.data): continue
    let shO = readStyleSheets(raw.data, dpi)
    if shO.isNone: continue
    let sh = shO.get
    var cmap = initTable[int, uint32]()
    for (ix, name, st) in sh.charSheets:
      var id: uint32
      if charIds.hasKey(name): id = charIds[name]
      else:
        let nid = styles.nextCharId
        if nid.isNone:
          result.add "character style " & escapeJson(name) & " was not imported: style id space exhausted"
          continue
        id = nid.get
        styles.character.add CharacterStyleDef(id: id, name: name, attrs: withoutPostscript(diffAttrs(st, sh.normalChar)))
        charIds[name] = id
      cmap[ix] = id
    var pmap = initTable[int, uint32]()
    for (ix, name, p, cs) in sh.paraSheets:
      var id: uint32
      if paraIds.hasKey(name): id = paraIds[name]
      else:
        let nid = styles.nextParaId
        if nid.isNone:
          result.add "paragraph style " & escapeJson(name) & " was not imported: style id space exhausted"
          continue
        id = nid.get
        let charAttrs = if cs.isSome: withoutPostscript(diffAttrs(cs.get, sh.normalChar)) else: newJObject()
        styles.paragraph.add ParagraphStyleDef(id: id, name: name, paraAttrs: diffAttrs(p, sh.normalPara), charAttrs: charAttrs)
        paraIds[name] = id
      pmap[ix] = id
    link(lp[].content.text, sh.runChar, sh.runCharLens, cmap, sh.runPara, pmap)
  d.textStyles = styles

proc exportTysh*(data: openArray[byte]; t: TextLayer; styles: TextStyles; dpi: float32): Option[seq[byte]] =
  ## The `TySh` data to write for a type layer, with its style sheets regenerated from `styles`;
  ## none when neither the document nor the data has named styles (write as is).
  let named = styles.character.len > 0 or styles.paragraph.len > 0
  if not named and not hasNamedSheets(data): return none(seq[byte])
  writeStyleSheets(data, t, styles, dpi)
