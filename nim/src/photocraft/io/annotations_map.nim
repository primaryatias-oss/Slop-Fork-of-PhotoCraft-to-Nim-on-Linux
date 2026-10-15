## Notes (the `Anno` global block) and the measurement scale (resource 1074).

import std/[options, math, strutils, unicode]
import ../color, ../doc, ../psd

const
  MeasurementScaleId* = 1074'u16
  CountInfo* = 1080'u16
  Anno* = "Anno"

type
  Rd = object
    d: seq[byte]
    p: int
  EntryKind = enum ekText, ekRaw
  Entry = object
    case kind: EntryKind
    of ekText: note: Note
    of ekRaw: raw: seq[byte]

proc take(r: var Rd; n: int): Option[seq[byte]] =
  if n < 0 or r.p + n > r.d.len: return none(seq[byte])
  result = some(r.d[r.p ..< r.p + n])
  r.p += n

proc u8(r: var Rd): Option[uint8] =
  if r.p + 1 > r.d.len: return none(uint8)
  result = some(r.d[r.p])
  inc r.p

proc u16(r: var Rd): Option[uint16] =
  if r.p + 2 > r.d.len: return none(uint16)
  result = some((uint16(r.d[r.p]) shl 8) or uint16(r.d[r.p + 1]))
  r.p += 2

proc u32(r: var Rd): Option[uint32] =
  if r.p + 4 > r.d.len: return none(uint32)
  result = some((uint32(r.d[r.p]) shl 24) or (uint32(r.d[r.p + 1]) shl 16) or (uint32(r.d[r.p + 2]) shl 8) or uint32(r.d[r.p + 3]))
  r.p += 4

proc pascal(r: var Rd): Option[string] =
  let n = r.u8
  if n.isNone: return none(string)
  let s = r.take(int(n.get))
  if s.isNone: return none(string)
  if (int(n.get) + 1) mod 2 == 1:
    if r.take(1).isNone: return none(string)
  var str = ""
  for c in s.get: str.add char(c)
  some(str)

proc pascalOut(o: var seq[byte]; s: string) =
  var bytes: seq[byte]
  for r in s.runes:
    if bytes.len >= 255: break
    bytes.add (if int(r) < 256: byte(int(r)) else: byte('?'))
  o.add byte(bytes.len)
  o.add bytes
  if (bytes.len + 1) mod 2 == 1: o.add 0'u8

proc colorFrom(space: uint16; c: array[4, uint16]): Color =
  proc f(v: uint16): float32 = float32(v) / 65535
  case space
  of 1:
    let (h, s, v) = (f(c[0]) * 6, f(c[1]), f(c[2]))
    let i = floorMod(int(floor(h)), 6)
    let fr = h - floor(h)
    let (p, q, t) = (v * (1 - s), v * (1 - s * fr), v * (1 - s * (1 - fr)))
    case i
    of 0: rgb(v, t, p)
    of 1: rgb(q, v, p)
    of 2: rgb(p, v, t)
    of 3: rgb(p, q, v)
    of 4: rgb(t, p, v)
    else: rgb(v, p, q)
  of 2:
    let k = 1 - f(c[3])
    rgb(f(c[0]) * (1 - k), f(c[1]) * (1 - k), f(c[2]) * (1 - k))
  of 8:
    let g = 1 - clamp(float32(c[0]) / 10000, 0'f32, 1'f32)
    rgb(g, g, g)
  else: rgb(f(c[0]), f(c[1]), f(c[2]))

proc decodeText(data: openArray[byte]): string =
  if data.len >= 2 and data[0] == 0xFE and data[1] == 0xFF:
    var units: seq[uint16]
    var i = 2
    while i + 1 < data.len:
      units.add (uint16(data[i]) shl 8) or uint16(data[i + 1])
      i += 2
    result = fromUtf16(units)
  else:
    for c in data: result.add $Rune(int(c))
  while result.endsWith('\0'): result.setLen(result.len - 1)
  result = result.replace("\r\n", "\n").replace("\r", "\n")

proc encodeText(s: string): seq[byte] =
  result = @[0xFE'u8, 0xFF]
  for u in utf16Units(s.replace("\r\n", "\n").replace("\n", "\r")):
    result.add byte(u shr 8)
    result.add byte(u and 0xff)

proc parseEntries(data: openArray[byte]): Option[seq[Entry]] =
  var r = Rd(d: @data)
  if r.u16.isNone or r.u16.isNone: return none(seq[Entry])
  let count = r.u32
  if count.isNone: return none(seq[Entry])
  var outp: seq[Entry]
  template need(x: untyped): untyped =
    (let v = x; if v.isNone: return none(seq[Entry]); v.get)
  for _ in 0'u32 ..< count.get:
    let start = r.p
    let len = int(need(r.u32))
    let stop = start + len
    if len < 0 or stop > data.len or stop < start: return none(seq[Entry])
    let raw = data[start ..< stop]
    let kind = need(r.take(4))
    if char(kind[0]) & char(kind[1]) & char(kind[2]) & char(kind[3]) != "txtA":
      outp.add Entry(kind: ekRaw, raw: @raw)
      r.p = stop
      continue
    let open = need(r.u8) != 0
    discard need(r.u8)
    discard need(r.u16)
    var icon, popup: array[4, int32]
    for k in 0 .. 3: icon[k] = cast[int32](need(r.u32))
    for k in 0 .. 3: popup[k] = cast[int32](need(r.u32))
    let space = need(r.u16)
    var c: array[4, uint16]
    for k in 0 .. 3: c[k] = need(r.u16)
    let author = need(r.pascal)
    discard need(r.pascal)
    let modified = need(r.pascal)
    discard need(r.u32)
    discard need(r.take(4))
    let n = int(need(r.u32))
    let text = decodeText(need(r.take(n)))
    outp.add Entry(kind: ekText, note: Note(author: author, color: colorFrom(space, c), text: text,
      position: [float64(icon[1]), float64(icon[0])],
      popup: [float64(popup[1]), float64(popup[0]), float64(popup[3]), float64(popup[2])], open: open, modified: modified))
    r.p = stop
  some(outp)

proc parseAnno*(data: openArray[byte]): Option[seq[Note]] =
  let e = parseEntries(data)
  if e.isNone: return none(seq[Note])
  var notes: seq[Note]
  for x in e.get:
    if x.kind == ekText: notes.add x.note
  some(notes)

proc putU16(o: var seq[byte]; v: uint16) =
  o.add byte(v shr 8)
  o.add byte(v and 0xff)
proc putU32(o: var seq[byte]; v: uint32) =
  for k in countdown(3, 0): o.add byte((v shr (8 * k)) and 0xff)
proc putI32(o: var seq[byte]; v: int32) = o.putU32(cast[uint32](v))

proc writeNote(n: Note): seq[byte] =
  var b: seq[byte] = @[byte('t'), byte('x'), byte('t'), byte('A')]
  b.add uint8(n.open)
  b.add 0x1C'u8
  b.putU16(1)
  let (x, y) = (int32(round(n.position[0])), int32(round(n.position[1])))
  for v in [y, x, y +% 20, x +% 16]: b.putI32(v)
  let pp = (if n.popup == [0.0, 0.0, 0.0, 0.0]: [n.position[0] + 20, n.position[1] + 20, n.position[0] + 260, n.position[1] + 160]
            else: n.popup)
  for v in [pp[1], pp[0], pp[3], pp[2]]: b.putI32(int32(round(v)))
  b.putU16(0)
  for v in n.color.toRgb: b.putU16(uint16(round(clamp(v, 0'f32, 1'f32) * 65535)))
  b.putU16(0)
  pascalOut(b, n.author)
  pascalOut(b, "")
  pascalOut(b, n.modified)
  let text = encodeText(n.text)
  b.putU32(uint32(text.len + 12))
  b.add [byte('t'), byte('x'), byte('t'), byte('C')]
  b.putU32(uint32(text.len))
  b.add text
  result.putU32(uint32(b.len + 4))
  result.add b

proc writeAnno*(notes: openArray[Note]; rawExtra: openArray[seq[byte]]): seq[byte] =
  result.putU16(2)
  result.putU16(1)
  result.putU32(uint32(notes.len + rawExtra.len))
  for n in notes: result.add writeNote(n)
  for r in rawExtra: result.add r

proc annoBlock(d: Document): Option[seq[byte]] =
  for b in d.metadata.psdGlobalBlocks:
    if b.key == Anno: return some(b.data.data)
  none(seq[byte])

proc notesFromBlocks*(d: Document): seq[Note] =
  let b = annoBlock(d)
  if b.isNone: return
  parseAnno(b.get).get(@[])

proc annoUnreadable*(d: Document): bool =
  let b = annoBlock(d)
  b.isSome and parseEntries(b.get).isNone

proc exportBlocks*(d: Document; blocks0: seq[PsdGlobalBlock]): seq[PsdGlobalBlock] =
  var blocks = blocks0
  var at = -1
  for i, b in blocks:
    if b.key == Anno:
      at = i
      break
  let entries = (if at >= 0: parseEntries(blocks[at].data.data) else: none(seq[Entry]))
  var parsed: Option[seq[Note]]
  if entries.isSome:
    var ns: seq[Note]
    for e in entries.get:
      if e.kind == ekText: ns.add e.note
    parsed = some(ns)
  let want = parseAnno(writeAnno(d.notes, []))
  proc same(a, b: seq[Note]): bool =
    if a.len != b.len: return false
    for i in 0 ..< a.len:
      var x = a[i]
      x.color = b[i].color
      if x != b[i]: return false
      let (cx, cy) = (a[i].color.toRgb, b[i].color.toRgb)
      for k in 0 .. 2:
        if abs(cx[k] - cy[k]) >= 1'f32 / 512: return false
    true
  if (parsed.isSome and want.isSome and same(parsed.get, want.get)) or (entries.isNone and d.notes.len == 0):
    return blocks
  var extra: seq[seq[byte]]
  if entries.isSome:
    for e in entries.get:
      if e.kind == ekRaw: extra.add e.raw
  let fresh = (if d.notes.len > 0 or extra.len > 0: some(writeAnno(d.notes, extra)) else: none(seq[byte]))
  if at >= 0 and fresh.isSome: blocks[at].data = SharedBytes(data: fresh.get)
  elif at >= 0: blocks.delete(at)
  elif fresh.isSome: blocks.add PsdGlobalBlock(signature: "8BIM", key: Anno, data: SharedBytes(data: fresh.get))
  blocks

proc findValue(d: Descriptor; keys: openArray[string]): Option[Value] =
  for k in keys:
    let v = d.get(k)
    if v.isSome: return v
  for (_, v) in d.items:
    if v.kind == vkDescriptor:
      for k in keys:
        let x = v.desc.get(k)
        if x.isSome: return x
  none(Value)

proc numV(v: Option[Value]): Option[float64] =
  if v.isNone: return none(float64)
  case v.get.kind
  of vkDouble: some(v.get.d)
  of vkInteger: some(float64(v.get.i))
  of vkLargeInteger: some(float64(v.get.li))
  of vkUnitFloat: some(v.get.uv)
  else: none(float64)

proc scaleFromResource*(data: openArray[byte]): Option[MeasurementScale] =
  var d: Descriptor
  try: d = parseVersionedPrefix(data)[0].descriptor
  except CatchableError: return none(MeasurementScale)
  let pl = numV(findValue(d, ["pixelLength", "PxlL"]))
  let ll = numV(findValue(d, ["logicalLength", "LglL"]))
  if pl.isNone or ll.isNone: return none(MeasurementScale)
  let u = findValue(d, ["logicalUnits", "LglU"])
  some(MeasurementScale(pixelLength: pl.get, logicalLength: ll.get,
                        units: (if u.isSome and u.get.kind == vkText: $u.get.text else: "pixels")))

proc writeScaleResource*(s: MeasurementScale): seq[byte] =
  let d = newDescriptor("null").with("pixelLength", vDouble(s.pixelLength)).with("logicalLength", vDouble(s.logicalLength))
    .with("logicalUnits", Value(kind: vkText, text: ustr(s.units)))
  result = versioned(d).toBytes
  if result.len mod 2 == 1: result.add 0'u8

proc rawScale*(d: Document): Option[MeasurementScale] =
  for r in d.metadata.psdResources:
    if r.id == MeasurementScaleId: return scaleFromResource(r.data.data)
  none(MeasurementScale)

proc keepRawScale*(d: Document): bool =
  let s = rawScale(d)
  if s.isSome: s.get == d.measurement.scale else: d.measurement.scale.isDefault

proc freshScaleResource*(d: Document): Option[seq[byte]] =
  var hasRaw = false
  for r in d.metadata.psdResources:
    if r.id == MeasurementScaleId: hasRaw = true
  if hasRaw and keepRawScale(d): return none(seq[byte])
  if d.measurement.scale.isDefault: none(seq[byte]) else: some(writeScaleResource(d.measurement.scale))

proc keepResource*(d: Document; id: uint16): bool =
  case id
  of MeasurementScaleId: keepRawScale(d)
  of CountInfo: d.measurement.countTotal == 0
  else: true
