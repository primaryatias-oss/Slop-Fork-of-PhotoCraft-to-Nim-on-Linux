## Character and Paragraph Styles (`type_styles_cmds`): the lifecycle, Basic Paragraph, strict
## attribute parsing, exhausted ids, isolated previews, and a size that overflows the font
## scaler's 16.16 arithmetic (it used to fail with an internal error).

import std/[unittest, json, options, strutils]
import photocraft/[engine, format]

proc session(text: string): (Session, uint64) =
  var s = newSession()
  discard s.execute("file.new", %*{"width": 400, "height": 200, "background": "transparent"})
  let r = s.execute("type.create", %*{"x": 10, "y": 50, "text": text, "size": 12})
  (s, uint64(r["layer"].getBiggestInt))

proc textLayer(s: Session; id: uint64): TextLayer =
  let l = s.active.doc[].findLayer(LayerId(id))
  doAssert not l.isNil and l[].content.kind == lkText
  l[].content.text

proc runAt(t: TextLayer; ci: int): CharStyle =
  var b = 0
  var n = 0
  while b < t.text.len and n < ci:
    inc b
    while b < t.text.len and (ord(t.text[b]) and 0xC0) == 0x80: inc b
    inc n
  var at = 0
  let runs = t.charRuns
  for r in runs:
    if b < at + r.len: return r.style
    at += r.len
  runs[^1].style

proc err(s: var Session; cmd: string; p: JsonNode): string =
  try:
    discard s.execute(cmd, p)
    ""
  except CatchableError as e: e.msg

suite "type styles":
  test "character style lifecycle":
    var (s, id) = session("Hello world")
    let r = s.execute("type.characterStyle.new", %*{"attrs": {"size": 30, "underline": true}, "fromSelection": false})
    let cs = uint32(r["id"].getInt)
    check r["name"].getStr == "Character Style 1"
    let l = s.execute("type.characterStyle.list", %*{})
    check l["styles"][0]["name"].getStr == "None"
    check l["styles"][1]["attrs"]["size_pt"].getFloat == 30.0
    check l["styles"][1]["attrs"]["underline"].getBool
    discard s.execute("type.characterStyle.apply", %*{"id": cs, "layer": id, "range": [6, 11]})
    var t = s.textLayer(id)
    check t.runAt(7).sizePt == 30 and t.runAt(7).underline and t.runAt(7).styleSheet == some(cs)
    check t.runAt(0).styleSheet.isNone
    var cur = s.execute("type.characterStyle.list", %*{"layer": id, "range": [6, 11]})["current"]
    check cur["character"].getInt == int(cs) and not cur["characterOverride"].getBool
    # A local change is an override; redefining the style changes other text too.
    discard s.execute("type.setStyle", %*{"layer": id, "range": [6, 8], "italic": true})
    cur = s.execute("type.characterStyle.list", %*{"layer": id, "range": [6, 8]})["current"]
    check cur["characterOverride"].getBool
    discard s.execute("type.characterStyle.set", %*{"id": cs, "attrs": {"size_pt": 40.0}})
    t = s.textLayer(id)
    check t.runAt(6).sizePt == 40 and t.runAt(6).italic
    check t.runAt(9).sizePt == 40 and not t.runAt(9).italic
    check t.runAt(0).sizePt == 12
    discard s.execute("type.characterStyle.clearOverride", %*{"layer": id, "range": [6, 11]})
    t = s.textLayer(id)
    check not t.runAt(6).italic and t.runAt(6).styleSheet == some(cs)
    discard s.execute("type.characterStyle.rename", %*{"id": cs, "name": "Emphasis"})
    check s.execute("type.characterStyle.duplicate", %*{"id": cs})["name"].getStr == "Emphasis copy"
    discard s.execute("type.characterStyle.delete", %*{"id": cs})
    t = s.textLayer(id)
    check t.runAt(7).sizePt == 40 and t.runAt(7).styleSheet.isNone
    check s.execute("type.characterStyle.list", %*{})["styles"].len == 2
    discard s.execute("edit.undo", %*{})
    check s.textLayer(id).runAt(7).styleSheet == some(cs)

  test "paragraph styles and Basic Paragraph":
    var (s, id) = session("One\nTwo")
    let cur = s.execute("type.paragraphStyle.list", %*{"layer": id})["current"]
    check cur["paragraph"].getInt == 0 and not cur["paragraphOverride"].getBool
    let ps = uint32(s.execute("type.paragraphStyle.new", %*{"name": "Head", "fromSelection": false,
                                                         "attrs": {"align": "center", "size": 24, "weight": 700}})["id"].getInt)
    discard s.execute("type.paragraphStyle.apply", %*{"id": ps, "layer": id, "range": [0, 1]})
    var t = s.textLayer(id)
    check t.paragraphRuns[0].style.styleSheet == some(ps)
    check t.paragraphRuns[0].style.align == taCenter
    check t.paragraphRuns[^1].style.styleSheet.isNone
    check t.runAt(0).sizePt == 24 and t.runAt(0).weight == 700
    check t.runAt(5).sizePt == 12
    discard s.execute("type.paragraphStyle.set", %*{"id": 0, "attrs": {"spaceBefore": 6}})
    t = s.textLayer(id)
    check t.paragraphRuns[^1].style.spaceBeforePt == 6 and t.paragraphRuns[0].style.spaceBeforePt == 6
    check s.err("type.paragraphStyle.delete", %*{"id": 0}).contains("Basic Paragraph can't be changed this way")
    check s.err("type.paragraphStyle.rename", %*{"id": 0, "name": "x"}).len > 0
    check s.err("type.characterStyle.apply", %*{"id": 99}).contains("no style with id 99")
    check s.err("type.characterStyle.new", %*{"attrs": {"bogus": 1}}).contains("unknown style attributes: bogus")
    check s.err("type.characterStyle.new", %*{"attrs": {"size_pt": "big"}}).len > 0
    discard s.execute("type.paragraphStyle.delete", %*{"id": ps})
    t = s.textLayer(id)
    check t.paragraphRuns[0].style.align == taCenter and t.paragraphRuns[0].style.styleSheet.isNone

  test "attribute values follow serde: a struct needs every field":
    var (s, _) = session("x")
    # `{}` is no `Color` (serde: missing field `mode`), nor a panel colour.
    check s.err("type.characterStyle.new", %*{"fromSelection": false, "attrs": {"color": {}}}).contains("unknown style attributes: color")
    let r = s.execute("type.characterStyle.new", %*{"fromSelection": false,
                                                    "attrs": {"color": "#ff0000", "caps": "all", "kerning": "Off"}})
    let st = s.active.doc[].textStyles.resolveChar(none(uint32), some(uint32(r["id"].getInt)), "Inter")
    check st.color.toRgb == [1'f32, 0, 0] and st.caps == capsAllCaps and st.kerning == kOff

  test "exhausted style ids fail without a history step":
    var (s, _) = session("x")
    var doc = s.active.doc[]
    doc.textStyles.character.add CharacterStyleDef(id: high(uint32), name: "Max", attrs: newJObject())
    doc.textStyles.paragraph.add ParagraphStyleDef(id: high(uint32), name: "Max", paraAttrs: newJObject(), charAttrs: newJObject())
    let nd = DocRef()
    nd[] = doc
    s.active.doc = nd
    let revision = s.active.revision
    check s.err("type.characterStyle.new", %*{"fromSelection": false}).contains("character style id space exhausted")
    check s.err("type.characterStyle.duplicate", %*{"id": high(uint32)}).contains("character style id space exhausted")
    check s.err("type.paragraphStyle.new", %*{"fromSelection": false}).contains("paragraph style id space exhausted")
    check s.err("type.paragraphStyle.duplicate", %*{"id": high(uint32)}).contains("paragraph style id space exhausted")
    check s.active.revision == revision

  test "previews leave the source document alone":
    var (s, _) = session("Text")
    let doc = s.active.doc[]
    let before = doc.textStyles
    for paragraph in [false, true]:
      for p in [%*{}, %*{"id": "bad"}, %*{"id": 999}, %*{"id": 0, "attrs": 3}]:
        expect EngineError: discard previewOptions(doc, p, paragraph)
    let p = %*{"id": 0, "attrs": {"color": "#112233"}}
    expect EngineError: discard previewOptions(doc, p, false)
    let shown = previewOptions(doc, p, true)
    check shown.textStyles != before
    check s.active.doc[].textStyles == before

  test "a style with an enormous size renders instead of failing":
    var (s, id) = session("Hi")
    let r = s.execute("type.characterStyle.new", %*{"attrs": {"size_pt": 1e30}, "apply": true, "layer": id})
    check r["id"].getInt == 1
    check s.textLayer(id).runAt(0).sizePt == 1e30'f32
