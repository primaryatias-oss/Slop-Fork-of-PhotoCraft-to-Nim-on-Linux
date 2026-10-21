## Note tool annotations (port of `notes_cmds.rs`): Window › Notes and File › Import › Notes….
## Notes are document data (`Note`), saved in `.pcraft` and as the PSD `Anno` block; each change is
## one history step.

import std/[json, options, os]
import ./core

proc hasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")

proc hasNotesE(s: Session): string =
  let why = hasDocE(s)
  if why.len > 0: return why
  if s.active.doc.notes.len > 0: "" else: "the document has no notes"

proc pdfDate(): string =
  ## PDF-style date (`D:YYYYMMDDhhmmssZ`) as Photoshop stores in notes; empty without a clock.
  let iso = nowIso()
  if iso.len < 19: return ""
  result = "D:"
  for c in iso[0 ..< 19]:
    if c in {'0'..'9'}: result.add c
  result.add 'Z'

proc defaultNote(): Note =
  ## Photoshop's default note colour (pale yellow).
  Note(color: rgb(1, 1, 0.51))

proc noteJson*(i: int; n: Note): JsonNode =
  %*{"index": i, "author": n.author, "text": n.text, "color": n.color.toRgb, "position": n.position, "open": n.open,
     "modified": n.modified}

proc listJson(d: Document): JsonNode =
  var a = newJArray()
  for i, n in d.notes: a.add noteJson(i, n)
  %*{"notes": a}

proc colorOf(p: JsonNode): Option[Color] =
  if p.pget("color").isNil: return
  let c = colorParam(p, "color", [1'f32, 1, 0.51, 1])
  some(rgb(c[0], c[1], c[2]))

proc indexOf(s: Session; p: JsonNode; cmdId: string): int =
  let st = s.active
  if st.isNil: raise noDocument()
  let n = st.doc.notes.len
  let i = p.pget("index").asU64
  if i.isNone: raise bad(cmdId, "pass `index`")
  if i.get < uint64(n): int(i.get)
  else: raise bad(cmdId, "no note " & $i.get & " (the document has " & $n & ")")

proc addNote(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "notes.add"
  let x = p.pget("x").asF64
  if x.isNone: raise bad(cmdId, "pass `x` and `y`")
  let y = p.pget("y").asF64
  if y.isNone: raise bad(cmdId, "pass `x` and `y`")
  let (xv, yv) = (x.get, y.get)
  if xv != xv or abs(xv) == Inf or yv != yv or abs(yv) == Inf: raise bad(cmdId, "coordinates must be finite")
  var n = defaultNote()
  n.author = p.pget("author").asStr.get("")
  n.text = p.pget("text").asStr.get("")
  n.position = [xv, yv]
  n.popup = [xv + 20.0, yv + 20.0, xv + 260.0, yv + 160.0]
  n.open = p.pget("open").asBool.get(true)
  n.modified = pdfDate()
  let c = colorOf(p)
  if c.isSome: n.color = c.get
  let note = n
  let i = s.editWith("New Note", proc(d: var Document; active: var Option[LayerId]): int =
    d.notes.add note
    d.notes.len - 1)
  let st = s.active
  if st.isNil: raise noDocument()
  %*{"index": i, "note": noteJson(i, st.doc.notes[i])}

proc setNote(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "notes.set"
  let i = indexOf(s, p, cmdId)
  let st = s.active
  if st.isNil: raise noDocument()
  let old = st.doc.notes[i]
  var n = old
  let t = p.pget("text").asStr
  if t.isSome: n.text = t.get
  let a = p.pget("author").asStr
  if a.isSome: n.author = a.get
  let c = colorOf(p)
  if c.isSome: n.color = c.get
  let o = p.pget("open").asBool
  if o.isSome: n.open = o.get
  let (x, y) = (p.pget("x").asF64, p.pget("y").asF64)
  if x.isSome or y.isSome:
    let (nx, ny) = (x.get(n.position[0]), y.get(n.position[1]))
    if nx != nx or abs(nx) == Inf or ny != ny or abs(ny) == Inf: raise bad(cmdId, "coordinates must be finite")
    let (dx, dy) = (nx - n.position[0], ny - n.position[1])
    n.position = [nx, ny]
    # The popup travels with its icon.
    n.popup = [n.popup[0] + dx, n.popup[1] + dy, n.popup[2] + dx, n.popup[3] + dy]
  if n == old: return %*{"index": i, "note": noteJson(i, n), "changed": false}
  # Content edits update the modification date (opening/closing the popup doesn't).
  if n.text != old.text or n.author != old.author: n.modified = pdfDate()
  let label = (if n.position != old.position: "Move Note" else: "Edit Note")
  let outp = noteJson(i, n)
  let note = n
  s.edit(label, proc(d: var Document; active: var Option[LayerId]) =
    if i < d.notes.len: d.notes[i] = note)
  %*{"index": i, "note": outp, "changed": true}

proc deleteNote(s: var Session; p: JsonNode): JsonNode =
  if p.pget("all").asBool == some(true):
    let n = (if s.active.isNil: 0 else: s.active.doc.notes.len)
    s.edit("Delete All Notes", proc(d: var Document; active: var Option[LayerId]) = d.notes.setLen(0))
    return %*{"deleted": n, "remaining": 0}
  let i = indexOf(s, p, "notes.delete")
  s.edit("Delete Note", proc(d: var Document; active: var Option[LayerId]) =
    if i < d.notes.len: d.notes.delete(i))
  %*{"deleted": 1, "remaining": (if s.active.isNil: 0 else: s.active.doc.notes.len)}

proc listNotes(s: var Session): JsonNode =
  let st = s.active
  if st.isNil: raise noDocument()
  listJson(st.doc[])

proc importNotesFrom*(s: var Session; name: string; bytes: openArray[byte]): JsonNode =
  ## File › Import › Notes… from file bytes (what a file picker returns). Notes outside the canvas
  ## are pulled inside, as Photoshop does when sizes differ.
  let src = fileImport(name, bytes)
  if src.notes.len == 0: raise other(name & " has no notes")
  let st = s.active
  if st.isNil: raise noDocument()
  let (w, h) = (float64(st.doc.size.width), float64(st.doc.size.height))
  var notes = src.notes
  for n in notes.mitems:
    let (nx, ny) = (clamp(n.position[0], 0.0, max(w - 16.0, 0.0)), clamp(n.position[1], 0.0, max(h - 20.0, 0.0)))
    let (dx, dy) = (nx - n.position[0], ny - n.position[1])
    n.position = [nx, ny]
    n.popup = [n.popup[0] + dx, n.popup[1] + dy, n.popup[2] + dx, n.popup[3] + dy]
  let count = notes.len
  let added = notes
  s.edit("Import Notes", proc(d: var Document; active: var Option[LayerId]) = d.notes.add added)
  let st2 = s.active
  if st2.isNil: raise noDocument()
  result = listJson(st2.doc[])
  result["imported"] = %count

proc importNotes(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "file.import.notes"
  let path = p.pget("path").asStr
  if path.isNone: raise bad(cmdId, "pass `path` (.psd, .psb or .pcraft)")
  let bytes = readFileOr(path.get)
  let name = (let n = extractFilename(path.get); if n.len > 0: n else: path.get)
  importNotesFrom(s, name, bytes)

proc notesSpecs*(): seq[CommandSpec] =
  @[cmd("notes.add", "New Note", [], "",
        """{"x":px,"y":px,"text":str="","author":str="","color":"#rrggbb"|[r,g,b]=pale yellow,"open":bool=true}""",
        hasDocE, runProc(addNote(s, p))),
    cmd("notes.set", "Edit Note", [], "",
        """{"index":n,"text":str?,"author":str?,"color":"#rrggbb"|[r,g,b]?,"x":px?,"y":px?,"open":bool?}""",
        hasNotesE, runProc(setNote(s, p))),
    cmd("notes.delete", "Delete Note", [], "", """{"index":n}|{"all":true}""", hasNotesE, runProc(deleteNote(s, p))),
    cmd("notes.list", "Notes", [], "", """{} → notes (index, author, text, colour, position, open, modified)""",
        hasDocE, runProc(listNotes(s)), journal = false),
    cmd("file.import.notes", "Notes…", ["File", "Import"], "", """{"path":".psd|.psb|.pcraft with notes"}""",
        hasDocE, runProc(importNotes(s, p)))]
