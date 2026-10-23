## Check Spelling, Insert Glyph and the headless caret queries (`type_spell_cmds`,
## `type_caret_cmds`), ported from the Rust unit tests.

import std/[unittest, json, math, strutils, unicode]
import photocraft/engine

proc session(w = 300, h = 100): Session =
  result = newSession()
  discard result.execute("file.new", %*{"width": w, "height": h, "background": "white"})

proc create(s: var Session; p: JsonNode): int =
  s.execute("type.create", p)["layer"].getInt

proc textOf(s: var Session; id: int): string =
  s.execute("type.info", %*{"layer": id})["text"].getStr

proc fails(s: var Session; cmd: string; p: JsonNode): bool =
  try:
    discard s.execute(cmd, p)
    false
  except EngineError: true

proc go(s: var Session; id, index: int; mv: string; x = NaN): int =
  var p = %*{"layer": id, "index": index, "move": mv}
  if x == x: p["x"] = %x
  s.execute("type.navigate", p)["index"].getInt

suite "check spelling and glyphs":
  test "lists, changes and learns words":
    var s = session()
    let id = s.create(%*{"x": 10, "y": 50, "text": "Teh quick brwon fox, teh end. Photocraft", "size": 12})
    let r = s.execute("edit.checkSpelling", %*{})
    var words: seq[string]
    for m in r["misspellings"]: words.add m["word"].getStr
    check words == @["Teh", "brwon", "teh", "Photocraft"]
    check r["misspellings"][0]["start"].getInt == 0 and r["misspellings"][0]["end"].getInt == 3
    check r["misspellings"][0]["suggestions"][0].getStr == "The"
    discard s.execute("edit.checkSpelling", %*{"action": "change", "layer": id, "start": 0, "end": 3, "word": "Teh", "replace": "The"})
    check s.fails("edit.checkSpelling", %*{"action": "change", "layer": id, "start": 0, "end": 3, "word": "Teh", "replace": "X"})
    check s.execute("edit.checkSpelling", %*{"action": "changeAll", "word": "brwon", "replace": "brown"})["changed"].getInt == 1
    check s.textOf(id) == "The quick brown fox, teh end. Photocraft"
    discard s.execute("edit.undo", %*{})
    check s.textOf(id).contains("brwon")
    discard s.execute("edit.redo", %*{})
    check s.execute("edit.checkSpelling", %*{"ignore": ["teh"]})["count"].getInt == 1
    discard s.execute("edit.checkSpelling", %*{"action": "addToDictionary", "word": "Photocraft"})
    check s.prefs.values.userDictionary == @["Photocraft"]
    check s.execute("edit.checkSpelling", %*{})["count"].getInt == 1
    discard s.execute("edit.checkSpelling", %*{"action": "removeFromDictionary", "word": "photocraft"})
    check s.prefs.values.userDictionary.len == 0
    check s.fails("edit.checkSpelling", %*{"action": "bogus"})
    check not s.execute("edit.checkSpelling", %*{"action": "suggest", "word": "recieve"})["correct"].getBool

  test "disabled without type":
    var s = newSession()
    check not s.isEnabled("edit.checkSpelling")
    discard s.execute("file.new", %*{"width": 10, "height": 10})
    check not s.isEnabled("edit.checkSpelling")
    check not s.isEnabled("type.insertText")

  test "insert a glyph at the caret and over a range":
    var s = session()
    let id = s.create(%*{"x": 10, "y": 50, "text": "ab", "size": 12})
    discard s.execute("type.insertText", %*{"layer": id, "text": "→", "at": 1})
    check s.textOf(id) == "a→b"
    let r = s.execute("type.insertText", %*{"layer": id, "text": "€", "range": [0, 2]})
    check s.textOf(id) == "€b" and r["caret"].getInt == 1
    discard s.execute("type.insertText", %*{"text": "!"})
    check s.textOf(id) == "€b!"
    check s.fails("type.insertText", %*{"text": ""})
    let crlf = s.execute("type.insertText", %*{"layer": id, "text": "x\r\ny", "at": 0})
    check crlf["caret"].getInt == 3 and s.textOf(id) == "x\ny€b!"

suite "caret queries":
  test "caret midpoints hit-test back to the same character":
    var s = session(400, 300)
    for (p, inside) in [(%*{"x": 40, "y": 70, "text": "AäB", "size": 28}, true),
                        (%*{"box": [15, 20, 64, 180], "text": "one two three four six seven eight nine", "size": 16}, true)]:
      let id = s.create(p)
      let text = s.textOf(id)
      for i in 0 .. text.runeLen:
        let c = s.execute("type.caret", %*{"layer": id, "index": i})
        check c["index"].getInt == i
        let seg = c["segment"]
        let x = (seg[0][0].getFloat + seg[1][0].getFloat) / 2
        let y = (seg[0][1].getFloat + seg[1][1].getFloat) / 2
        let h = s.execute("type.hitTest", %*{"layer": id, "x": x, "y": y})
        check h["index"].getInt == i and h["line"] == c["line"] and h["layer"].getInt == id
        if inside: check h["inside"].getBool

  test "navigate words, lines and edges, keeping a column":
    var s = session(400, 300)
    var id = s.create(%*{"x": 20, "y": 60, "text": "hello big world", "size": 18})
    check s.go(id, 0, "wordNext") == 5 and s.go(id, 5, "wordNext") == 9 and s.go(id, 9, "wordNext") == 15
    check s.go(id, 15, "wordPrev") == 10 and s.go(id, 7, "wordPrev") == 6 and s.go(id, 0, "wordPrev") == 0
    check s.go(id, 4, "start") == 0 and s.go(id, 4, "end") == 15
    check s.go(id, 4, "lineStart") == 0 and s.go(id, 4, "lineEnd") == 15
    id = s.create(%*{"x": 24, "y": 200, "text": "WWWWWW\nI", "size": 28})
    let lines = s.execute("type.info", %*{"layer": id})["lines"]
    let (end0, start1, end1) = (lines[0]["end"].getInt, lines[1]["start"].getInt, lines[1]["end"].getInt)
    let x = s.execute("type.caret", %*{"layer": id, "index": 0})["segment"][0][0].getFloat
    check s.go(id, end0, "lineNext", x) == start1
    check s.go(id, end0, "lineNext") == end1
    check s.go(id, start1, "linePrev", x) == lines[0]["start"].getInt
    # Queries take no history step.
    let rev = s.active.revision
    discard s.execute("type.caret", %*{"layer": id, "index": 1})
    check s.active.revision == rev

  test "hit-testing skips hidden layers and hidden groups":
    var s = session(400, 300)
    let lower = s.create(%*{"x": 50, "y": 80, "text": "AAAA", "size": 36})
    let upper = s.create(%*{"x": 50, "y": 80, "text": "BBBB", "size": 36})
    let c = s.execute("type.caret", %*{"layer": upper, "index": 1})
    let x = (c["segment"][0][0].getFloat + c["segment"][1][0].getFloat) / 2
    let y = (c["segment"][0][1].getFloat + c["segment"][1][1].getFloat) / 2
    check s.execute("type.hitTest", %*{"x": x, "y": y})["layer"].getInt == upper
    discard s.execute("layer.setProps", %*{"layer": upper, "visible": false})
    check s.execute("type.hitTest", %*{"x": x, "y": y})["layer"].getInt == lower
    check s.execute("type.hitTest", %*{"layer": upper, "x": x, "y": y})["layer"].getInt == upper
    check s.fails("type.hitTest", %*{"x": -40.0, "y": -40.0})
    # A point infinitely far away still lands on the nearest line (Rust's `min_by(total_cmp)`).
    check s.execute("type.hitTest", %*{"layer": lower, "x": x, "y": 1e300})["line"].getInt == 0

  test "bad params are errors":
    var s = session(400, 300)
    let id = s.create(%*{"x": 10, "y": 40, "text": "Hi", "size": 20})
    let pixel = s.execute("layer.new.layer", %*{})["layer"].getInt
    for (cmd, p) in [("type.hitTest", %*{}), ("type.hitTest", %*{"x": "1", "y": 2}), ("type.hitTest", %*{"layer": id, "x": 1}),
                     ("type.hitTest", %*{"layer": "nope", "x": 1, "y": 2}), ("type.hitTest", %*{"layer": pixel, "x": 1, "y": 2}),
                     ("type.caret", %*{"layer": id}), ("type.caret", %*{"layer": id, "index": -1}),
                     ("type.caret", %*{"layer": id, "index": 1.5}), ("type.caret", %*{"layer": id, "index": 99}),
                     ("type.navigate", %*{"layer": id, "index": 0}), ("type.navigate", %*{"layer": id, "index": 0, "move": "left"}),
                     ("type.navigate", %*{"layer": id, "index": 0, "move": "lineNext", "x": "no"})]:
      check s.fails(cmd, p)
    let far = s.execute("type.hitTest", %*{"layer": id, "x": 390, "y": 290})
    check not far["inside"].getBool and far["index"].getInt <= 2
