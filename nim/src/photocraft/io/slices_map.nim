## Web slices ↔ PSD resource 1050.

import std/[options, tables, sets, unicode, strutils]
import ../geom, ../doc, ../psd

const Slices* = SlicesId

proc raw(d: Document): Option[seq[byte]] =
  for r in d.metadata.psdResources:
    if r.id == SlicesId: return some(r.data.data)
  none(seq[byte])

proc slicesFromPsd*(raw: Option[seq[byte]]; d: Document): doc.Slices =
  if raw.isNone: return
  var res: SlicesResource
  try: res = parseSlices(raw.get)
  except CatchableError: return
  var byPsdId: Table[uint32, LayerId]
  for (_, _, l) in d.walk:
    if l.psdId.isSome: byPsdId[l.psdId.get] = l.id
  result.groupName = res.groupName
  for s in res.slices:
    if s.origin == 0: continue
    var layer = none(LayerId)
    if s.origin == 1 and s.layerId.isSome and s.layerId.get in byPsdId: layer = some(byPsdId[s.layerId.get])
    result.list.add WebSlice(id: s.id, groupId: s.groupId, origin: (if s.origin == 1 and layer.isSome: soLayer else: soUser),
      layer: layer, name: s.name, rect: rect(int(s.rect[0]), int(s.rect[1]), int(s.rect[2]), int(s.rect[3])),
      kind: sliceKindFromCode(s.kind), url: s.url, target: s.target, message: s.message, alt: s.alt,
      cellTextIsHtml: s.cellTextIsHtml, cellText: s.cellText, horizontalAlign: s.horizontalAlign,
      verticalAlign: s.verticalAlign, background: (if s.color[0] != 0: some(s.color) else: none(array[4, uint8])),
      outsets: s.outsets)

proc importSlices*(d: var Document) = d.slices = slicesFromPsd(raw(d), d)

proc unchanged*(d: Document): bool = slicesFromPsd(raw(d), d) == d.slices

proc baseName*(d: Document): string =
  var n = d.name
  let dot = n.rfind('.')
  if dot >= 0: n = n[0 ..< dot]
  for r in n.runes:
    if r.isAlpha or (r.int32 >= ord('0') and r.int32 <= ord('9')) or r == Rune('-') or r == Rune('_'): result.add $r
    else: result.add '_'
  if result.len == 0: result = "Untitled"

proc exportResource*(d: Document; layerIds: Table[LayerId, uint32]): (Option[Option[seq[byte]]], Option[string]) =
  ## (none: keep the original resource; some(none): drop it; some(some(b)): write b, warning).
  if unchanged(d): return (none(Option[seq[byte]]), none(string))
  if d.slices.isEmpty: return (some(none(seq[byte])), none(string))
  var records: seq[SliceRecord]
  for s in d.slices.list:
    var r = SliceRecord(id: s.id, groupId: s.groupId, origin: s.origin.code, name: s.name, kind: s.kind.code,
      rect: [int32(s.rect.x0), int32(s.rect.y0), int32(s.rect.x1), int32(s.rect.y1)], url: s.url, target: s.target,
      message: s.message, alt: s.alt, cellTextIsHtml: s.cellTextIsHtml, cellText: s.cellText,
      horizontalAlign: s.horizontalAlign, verticalAlign: s.verticalAlign, color: s.background.get([0'u8, 0, 0, 0]),
      outsets: s.outsets)
    if s.layer.isSome and s.layer.get in layerIds: r.layerId = some(layerIds[s.layer.get])
    if r.origin == 1 and r.layerId.isNone: r.origin = 2
    records.add r
  var taken: seq[Rect]
  var used: HashSet[uint32]
  for s in d.slices.list:
    taken.add s.rect
    used.incl s.id
  var nextId = d.slices.nextId
  if nextId.isNone: nextId = some(1'u32)
  var omitted = false
  for r in autoSlices(d.bounds, taken):
    if nextId.isNone:
      omitted = true
      break
    var id = nextId.get
    while id in used:
      if id == high(uint32):
        omitted = true
        break
      inc id
    if omitted: break
    used.incl id
    nextId = (if id == high(uint32): none(uint32) else: some(id + 1))
    records.add SliceRecord(id: id, origin: 0, kind: 1, rect: [int32(r.x0), int32(r.y0), int32(r.x1), int32(r.y1)])
  let groupName = (if d.slices.groupName.len == 0: baseName(d) else: d.slices.groupName)
  let res = SlicesResource(version: 6, bounds: [0'i32, 0, int32(d.size.height), int32(d.size.width)], groupName: groupName,
                           slices: records)
  (some(some(res.toBytes)), (if omitted: some("auto slices omitted because the slice id space is exhausted") else: none(string)))
