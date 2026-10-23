## Edit › Check Spelling… and glyph insertion (port of `type_spell_cmds.rs`; Window › Glyphs).
##
## `edit.checkSpelling` checks type layers against the bundled English dictionary
## (`text/spell`, SCOWL size 50) plus the user dictionary in the preferences (`userDictionary`).
## Headless, it lists misspellings with suggestions and changes, changes all, or adds words to the
## dictionary; the UI's dialog drives the same actions. Offsets are character indices.
##
## `type.insertText` inserts a string (a glyph) into a type layer at a character position or over
## a range.

import std/[json, options, sets, unicode, algorithm, strutils]
import ./core
import ../text/spell as tspell
import ../rfmt
import ../textutil

const SpellCmd = "edit.checkSpelling"

proc spellByteAt(text: string; ci: int): int =
  ## The byte offset of character `ci` (the text's length past its end).
  var n = 0
  var i = 0
  while i < text.len:
    if n == ci: return i
    i += max(runeLenAt(text, i), 1)
    inc n
  text.len

proc spellCharAt(text: string; bi: int): int =
  ## The number of characters before byte `bi`.
  let stop = min(max(bi, 0), text.len)
  var i = 0
  while i < stop:
    i += max(runeLenAt(text, i), 1)
    inc result

proc charCount(s: string): int = spellCharAt(s, s.len)

proc typeLayers(doc: Document): seq[LayerId] =
  ## Type layers top to bottom (Layers panel order), skipping fully locked ones.
  let all = doc.walk
  for i in countdown(all.high, 0):
    let (path, _, l) = all[i]
    if l.content.kind == lkText and not doc.locksAt(path).all: result.add l.id

proc hasTypeE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if typeLayers(d.doc[]).len == 0: "the document has no type layers" else: ""

proc layerText(doc: Document; id: LayerId): Option[string] =
  let l = doc.findLayer(id)
  if l.isNil or l[].content.kind != lkText: none(string) else: some(l[].content.text.text)

proc accepted(s: Session; p: JsonNode): HashSet[string] =
  ## The user dictionary plus `ignore` words, lowercased.
  for w in s.prefs.values.userDictionary: result.incl unicode.toLower(w)
  let a = p.pget("ignore")
  if a.isArray:
    for x in a.elems:
      if x.asStr.isSome: result.incl unicode.toLower(x.str)

proc layersParam(s: Session; p: JsonNode): seq[LayerId] =
  let st = s.active
  if st.isNil: return
  let all = typeLayers(st.doc[])
  if p.pget("allLayers").asBool == some(false) or not p.pget("layer").isNil:
    var active = none(LayerId)
    try: active = some(layerParam(s, p))
    except EngineError: discard
    for id in all:
      if some(id) == active: result.add id
  else: result = all

proc suggestionsMax(p: JsonNode): int = int(p.pget("suggestions").asU64.get(5))

proc spellList(s: Session; p: JsonNode): JsonNode =
  let st = s.active
  if st.isNil: raise noDocument()
  let dict = tspell.english()
  let ok = accepted(s, p)
  let max = suggestionsMax(p)
  var found = newJArray()
  for id in layersParam(s, p):
    let t = layerText(st.doc[], id)
    if t.isNone: continue
    for m in dict.misspellings(t.get, ok):
      var sugg = newJArray()
      for w in dict.suggest(m.word, max): sugg.add %w
      found.add %*{"layer": uint64(id), "start": spellCharAt(t.get, m.start), "end": spellCharAt(t.get, m.`end`),
                 "word": m.word, "suggestions": sugg}
  %*{"misspellings": found, "count": found.len}

proc spellChange(s: var Session; p: JsonNode): JsonNode =
  ## Replaces one occurrence (verified to still be `word` when given).
  let layer = p.pget("layer").asU64
  if layer.isNone: raise bad(SpellCmd, "`change` needs `layer`")
  let id = LayerId(layer.get)
  let start = p.pget("start").asU64
  if start.isNone: raise bad(SpellCmd, "`change` needs `start`")
  let stop = p.pget("end").asU64
  if stop.isNone: raise bad(SpellCmd, "`change` needs `end`")
  let to = p.pget("replace").asStr
  if to.isNone: raise bad(SpellCmd, "`change` needs `replace`")
  let st = s.active
  if st.isNil: raise noDocument()
  let t = layerText(st.doc[], id)
  if t.isNone: raise bad(SpellCmd, "layer " & $uint64(id) & " is not a type layer")
  let (startC, endC) = (int(start.get), int(stop.get))
  let (a, b) = (spellByteAt(t.get, startC), spellByteAt(t.get, max(endC, startC)))
  let w = p.pget("word").asStr
  if w.isSome and t.get[a ..< b] != w.get:
    raise bad(SpellCmd, "the text at " & $startC & ".." & $endC & " is " & debugStr(t.get[a ..< b]) & ", not " & debugStr(w.get))
  let replacement = to.get
  s.edit("Check Spelling", proc(doc: var Document; active: var Option[LayerId]) =
    let snapshot = doc
    let l = doc.findLayer(id)
    if not l.isNil and l[].content.kind == lkText:
      replaceText(l[].content.text, a, b, replacement)
      refreshText(snapshot, l[].content.text))
  %*{"changed": 1, "layer": uint64(id), "start": startC, "end": startC + charCount(replacement)}

proc spellChangeAll(s: var Session; p: JsonNode): JsonNode =
  ## Replaces every whole-word, case-sensitive occurrence of `word` in the checked layers.
  let word = p.pget("word").asStr
  if word.isNone or word.get.len == 0: raise bad(SpellCmd, "`changeAll` needs `word`")
  let to = p.pget("replace").asStr
  if to.isNone: raise bad(SpellCmd, "`changeAll` needs `replace`")
  let st = s.active
  if st.isNil: raise noDocument()
  var plan: seq[(LayerId, seq[(int, int)])]
  for id in layersParam(s, p):
    let t = layerText(st.doc[], id)
    if t.isNone: continue
    var hits: seq[(int, int)]
    for (a, b) in tspell.words(t.get):
      if t.get[a ..< b] == word.get: hits.add (a, b)
    if hits.len > 0: plan.add (id, hits)
  var count = 0
  for (_, h) in plan: count += h.len
  if count > 0:
    let replacement = to.get
    s.edit("Check Spelling", proc(doc: var Document; active: var Option[LayerId]) =
      let snapshot = doc
      for (id, hits) in plan:
        let l = doc.findLayer(id)
        if l.isNil or l[].content.kind != lkText: continue
        for i in countdown(hits.high, 0):
          replaceText(l[].content.text, hits[i][0], hits[i][1], replacement)
        refreshText(snapshot, l[].content.text))
  %*{"changed": count, "layers": plan.len}

proc hasWhiteSpace(s: string): bool =
  for r in s.runes:
    if r.isWhiteSpace: return true

proc userDictJson(s: Session): JsonNode =
  result = newJArray()
  for w in s.prefs.values.userDictionary: result.add %w

proc spellAddWord(s: var Session; p: JsonNode): JsonNode =
  let given = p.pget("word").asStr
  let word = (if given.isSome: trimRust(given.get) else: "")
  if given.isNone or word.len == 0 or word.hasWhiteSpace:
    raise bad(SpellCmd, "`add` needs a single `word`")
  var added = false
  s.editPrefs(proc(pr: var Preferences) =
    for w in pr.userDictionary:
      if cmpIgnoreCase(w, word) == 0: return
    pr.userDictionary.add word
    # A stable sort by the lowercase form (Rust's `sort_by_key(to_lowercase)`).
    pr.userDictionary.sort(proc(x, y: string): int = cmp(unicode.toLower(x), unicode.toLower(y)))
    added = true)
  %*{"added": added, "word": word, "userDictionary": userDictJson(s)}

proc spellRemoveWord(s: var Session; p: JsonNode): JsonNode =
  let word = p.pget("word").asStr.get("")
  var removed = false
  s.editPrefs(proc(pr: var Preferences) =
    var kept: seq[string]
    for w in pr.userDictionary:
      if cmpIgnoreCase(w, word) != 0: kept.add w
    removed = kept.len != pr.userDictionary.len
    pr.userDictionary = kept)
  %*{"removed": removed, "userDictionary": userDictJson(s)}

proc checkSpelling(s: var Session; p: JsonNode): JsonNode =
  let action = p.pget("action").asStr.get("list")
  case action
  of "list", "check": spellList(s, p)
  of "change": spellChange(s, p)
  of "changeAll": spellChangeAll(s, p)
  of "add", "addToDictionary": spellAddWord(s, p)
  of "removeFromDictionary": spellRemoveWord(s, p)
  of "suggest":
    let w = p.pget("word").asStr.get("")
    let ok = accepted(s, p)
    let d = tspell.english()
    var sugg = newJArray()
    for x in d.suggest(w, suggestionsMax(p)): sugg.add %x
    %*{"word": w, "correct": d.check(w, ok), "suggestions": sugg}
  else: raise bad(SpellCmd, "unknown action " & debugStr(action))

proc insertText(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "type.insertText"
  let id = layerParam(s, p)
  let given = p.pget("text").asStr
  if given.isNone or given.get.len == 0: raise bad(cmdId, "missing `text`")
  let newText = normText(given.get)
  let st = s.active
  if st.isNil: raise noDocument()
  let t = layerText(st.doc[], id)
  if t.isNone: raise bad(cmdId, "layer " & $uint64(id) & " is not a type layer")
  let n = charCount(t.get)
  let r = p.pget("range")
  let at = p.pget("at").asU64
  var (a, b) = (n, n)
  if r.isArray and r.len == 2:
    let x = int(r.elems[0].asU64.get(0))
    let y = int(r.elems[1].asU64.get(uint64(x)))
    (a, b) = (min(min(x, y), n), min(max(x, y), n))
  elif at.isSome: (a, b) = (int(at.get), int(at.get))
  (a, b) = (min(a, n), min(b, n))
  let (ba, bb) = (spellByteAt(t.get, a), spellByteAt(t.get, b))
  let label = p.pget("label").asStr.get("Insert Glyph")
  s.edit(label, proc(doc: var Document; active: var Option[LayerId]) =
    let snapshot = doc
    let l = doc.findLayer(id)
    if not l.isNil and l[].content.kind == lkText:
      replaceText(l[].content.text, ba, bb, newText)
      refreshText(snapshot, l[].content.text))
  %*{"layer": uint64(id), "caret": a + charCount(newText)}

proc hasTextLayerE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if d.activeLayer.isSome:
    let l = d.doc[].findLayer(d.activeLayer.get)
    if not l.isNil and l[].content.kind == lkText: return ""
  "the active layer is not a type layer"

proc typeSpellSpecs*(): seq[CommandSpec] =
  @[cmd(SpellCmd, "Check Spelling…", ["Edit"], "",
        """{"action":"list|change|changeAll|addToDictionary|removeFromDictionary|suggest"="list","allLayers":bool=true,"layer":id? (check one layer),"ignore":[word]? (Ignore All for this check),"suggestions":n=5; change: "layer","start","end" (chars),"word"? (verified),"replace"; changeAll: "word","replace"; addToDictionary/removeFromDictionary/suggest: "word"} → list: {"misspellings":[{"layer","start","end","word","suggestions"}],"count"}""",
        enabledProc(hasTypeE(s)), runProc(checkSpelling(s, p))),
    cmd("type.insertText", "Insert Glyph", [], "",
        """{"layer":id?,"text":str,"at":char? (default: end of text),"range":[startChar,endChar]? (replaced),"label":str?} → {"layer","caret"}""",
        enabledProc(hasTextLayerE(s)), runProc(insertText(s, p)))]
