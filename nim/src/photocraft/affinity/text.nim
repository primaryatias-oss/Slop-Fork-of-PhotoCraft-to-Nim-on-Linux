## Artistic and frame text (port of `affinity/src/text.rs`): the story's characters with their
## font, size, colour, tracking and leading, the paragraph alignment and the first baseline.
## Layout (line breaks in frames, kerning, OpenType features) is left to the importing application.

import std/[options, unicode]
import ./stream, ./types, ./paint

const MaxChars = 1 shl 20
  ## characters read per text node (stories are split into blocks; this caps hostile ones)

proc slotFloat(v: Option[SValue]; i: int): Option[float64] =
  if v.isNone or v.get.kind != svArray or i >= v.get.arr.len: return
  let x = v.get.arr[i]
  if x.kind == svFloat and x.f == x.f and abs(x.f) != Inf: some(x.f) else: none(float64)

proc slotInt(v: Option[SValue]; i: int): Option[int64] =
  if v.isNone or v.get.kind != svArray or i >= v.get.arr.len: return
  let x = v.get.arr[i]
  case x.kind
  of svInt: some(x.i)
  of svUInt: (if x.u <= uint64(high(int64)): some(int64(x.u)) else: none(int64))
  else: none(int64)

proc runAttrs(r: var Reader; attrs: ObjId; text: string; world: AfAffine): AfTextRun =
  let doubles = r.s.field(attrs, "Doub")
  let ints = r.s.field(attrs, "Ints")
  var size = 12.0 * r.dpi / 72.0
  let sz = slotFloat(doubles, 0)
  if sz.isSome and sz.get > 0.0 and sz.get < 1e6: size = sz.get
  let tracking = slotFloat(doubles, 4).get(0.0)
  var leading = none(float64)
  if slotInt(ints, 4) == some(1'i64):
    let l = slotFloat(doubles, 13)
    if l.isSome and l.get > 0.0: leading = l
  var objects: seq[Option[ObjId]]
  let o = r.s.field(attrs, "Objs")
  if o.isSome and o.get.kind == svArray:
    for v in o.get.arr:
      objects.add(if v.kind == svObj and v.obj >= 0: some(v.obj) else: none(ObjId))
  var fill = AfPaint(kind: apNone)
  if objects.len > 0 and objects[0].isSome:
    let f = descriptor(r, objects[0].get, world)
    if f.isSome: fill = f.get
  if objects.len > 1 and objects[1].isSome:
    let s = descriptor(r, objects[1].get, world)
    if s.isSome and s.get.kind != apNone: r.warn("outlined (stroked) text")
  let hs = slotFloat(doubles, 10)
  if hs.isSome and abs(hs.get - 1.0) > 1e-3: r.warn("horizontally scaled text")
  let font = r.s.child(attrs, "DFnt")
  template get(tag: string): string =
    (if font.isSome: r.s.str(font.get, tag).get("") else: "")
  AfTextRun(
    text: text,
    postscript: get("Post"),
    family: get("Famy"),
    weight: (if font.isSome: r.s.intOf(font.get, "Wegt").get(400) else: 400),
    italic: (if font.isSome: r.s.boolean(font.get, "Ital").get(false) else: false),
    size: size,
    tracking: tracking,
    leading: leading,
    fill: fill)

proc paragraphAlign(r: var Reader; p: ObjId): AfAlign =
  let v = slotInt(r.s.field(p, "Ints"), 0)
  if v.isNone: return alLeft
  case v.get
  of 0: alLeft
  of 1: alCenter
  of 2: alRight
  of 3: alJustify
  else:
    r.warn("an unknown paragraph alignment (imported as left)")
    alLeft

proc read*(r: var Reader; id: ObjId; world: AfAffine): Option[AfText] =
  let story = r.s.child(id, "StSt")
  if story.isNone: return
  let frame = r.s.child(id, "TxtH")
  if frame.isNone: return
  let artistic = r.s.isClass(frame.get, "ArFr")
  let fb = r.s.floatsN(frame.get, "FrmB", 4)
  if fb.isNone: return
  let bounds = rect(fb.get)
  var runs: seq[AfTextRun]
  var align = none(AfAlign)
  var total = 0
  var placeholders = false
  for blk in r.s.children(story.get, "Blok"):
    let glyphs = r.s.child(blk, "Glyp")
    if glyphs.isNone: return
    var chars: seq[Rune]
    let utf = r.s.str(glyphs.get, "Utf8")
    if utf.isSome: chars = toRunes(utf.get)
    else:
      # Inline objects (page numbers, anchors) each take one index before the segment text.
      for seg in r.s.children(glyphs.get, "Mixd"):
        let inline = r.s.children(seg, "Glys").len
        for _ in 0 ..< inline: chars.add Rune(0xFFFC)
        placeholders = placeholders or inline > 0
        chars.add toRunes(r.s.str(seg, "Utf8").get(""))
    total += chars.len
    if total > MaxChars:
      r.warn("text longer than a million characters (truncated)")
      break
    var start = 0
    let gatt = r.s.child(blk, "GAtt")
    let runIds = if gatt.isSome: r.s.children(gatt.get, "Runs") else: @[]
    for run in runIds:
      let ix = r.s.intOf(run, "Indx")
      if ix.isNone or ix.get < 0: return
      let e = int(min(ix.get, int64(chars.len)))
      if e <= start: continue
      var text = ""
      for k in start ..< e:
        let c = chars[k]
        if int32(c) != 0 and int32(c) != 0xFFFC: text.add $c
      start = e
      let attrs = r.s.child(run, "Item")
      if attrs.isNone: continue
      if text.len > 0: runs.add runAttrs(r, attrs.get, text, world)
    if align.isNone:
      let p = r.s.child(blk, "PAtt")
      if p.isSome:
        let pr = r.s.children(p.get, "Runs")
        if pr.len > 0:
          let item = r.s.child(pr[0], "Item")
          if item.isSome: align = some(paragraphAlign(r, item.get))
  if placeholders: r.warn("text fields such as page numbers (left out of the text)")
  let al = align.get(alLeft)
  let firstBaseline = bounds.y0 + r.s.f64(frame.get, "ArtV").get(0.0)
  let anchorX = case al
    of alCenter: (bounds.x0 + bounds.x1) / 2.0
    of alRight: bounds.x1
    else: bounds.x0
  if not artistic:
    let cw = r.s.field(frame.get, "ColW")
    if r.s.children(frame.get, "ColW").len > 1 or (cw.isSome and cw.get.kind == svArray and cw.get.arr.len > 1):
      r.warn("text frames with several columns (imported as one column)")
    if r.s.isClass(id, "TxtC"):
      r.warn("text frames with a curved outline (imported as rectangular frames)")
  some(AfText(runs: runs, align: al, anchor: pt(anchorX, firstBaseline),
              frame: (if artistic: none(AfRect) else: some(bounds)), transform: world))
