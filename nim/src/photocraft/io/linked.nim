## Smart-object source files stored in a PSD (`lnk2`/`lnk3`/`lnkD`/`lnkE` global blocks):
## parsing, lookup by unique id, encoding new entries, and rebuilding a block with entries
## dropped or appended. Rust `linked.rs`.

import std/options
import ../doc, ../psd
import common

type
  LinkedFile* = object
    uuid*, fileName*: string
    bytes*: seq[byte]

  Rd = object
    p: int

proc take(r: var Rd; d: openArray[byte]; n: int): Option[seq[byte]] =
  if n < 0 or r.p > d.len or n > d.len - r.p: return none(seq[byte])
  result = some(d[r.p ..< r.p + n])
  r.p += n

proc skip(r: var Rd; d: openArray[byte]; n: int): bool =
  if n < 0 or r.p > d.len or n > d.len - r.p: return false
  r.p += n
  true

proc u8(r: var Rd; d: openArray[byte]): Option[uint8] =
  if r.p >= d.len: return none(uint8)
  result = some(d[r.p])
  inc r.p

proc u32(r: var Rd; d: openArray[byte]): Option[uint32] =
  if r.p + 4 > d.len: return none(uint32)
  result = some(beU32(d, r.p))
  r.p += 4

proc u64(r: var Rd; d: openArray[byte]): Option[uint64] =
  if r.p + 8 > d.len: return none(uint64)
  var v = 0'u64
  for i in 0 ..< 8: v = (v shl 8) or uint64(d[r.p + i])
  r.p += 8
  some(v)

proc beU64At(d: openArray[byte]; at: int): uint64 =
  for i in 0 ..< 8: result = (result shl 8) or uint64(d[at + i])

proc putU32(o: var seq[byte]; v: uint32) =
  for s in [24, 16, 8, 0]: o.add byte((v shr s) and 0xff)

proc putU64(o: var seq[byte]; v: uint64) =
  for s in [56, 48, 40, 32, 24, 16, 8, 0]: o.add byte((v shr s) and 0xff)

proc unicode(r: var Rd; d: openArray[byte]): Option[string] =
  let n = r.u32(d)
  if n.isNone: return none(string)
  let cnt = int(n.get)
  if cnt > (d.len - r.p) div 2: return none(string)
  var units = newSeq[uint16](cnt)
  for i in 0 ..< cnt: units[i] = beU16(d, r.p + 2 * i)
  r.p += 2 * cnt
  var s = fromUtf16(units)
  while s.len > 0 and s[^1] == '\0': s.setLen(s.len - 1)
  some(s)

proc parseItem(item: openArray[byte]): Option[LinkedFile] =
  var r = Rd()
  let kind = r.take(item, 4)
  if kind.isNone or r.u32(item).isNone: return
  let n = r.u8(item)
  if n.isNone: return
  let uuid = r.take(item, int(n.get))
  if uuid.isNone: return
  let fileName = r.unicode(item)
  if fileName.isNone or not r.skip(item, 8): return
  let len = r.u64(item)
  if len.isNone or len.get > uint64(high(int)): return
  let hasDesc = r.u8(item)
  if hasDesc.isNone: return
  if hasDesc.get != 0:
    if r.p > item.len: return
    try:
      let (_, used) = parseVersionedPrefix(item.toOpenArray(r.p, item.len - 1))
      r.p += used
    except CatchableError: return
  if kind.get != @[byte('l'), byte('i'), byte('F'), byte('D')]: return
  let bytes = r.take(item, int(len.get))
  if bytes.isNone: return
  some(LinkedFile(uuid: lossyUtf8(uuid.get), fileName: fileName.get, bytes: bytes.get))

proc parseLinkedFiles*(data: openArray[byte]): seq[LinkedFile] =
  var r = Rd()
  while true:
    let len = r.u64(data)
    if len.isNone or len.get > uint64(high(int)): break
    let item = r.take(data, int(len.get))
    if item.isNone: break
    let f = parseItem(item.get)
    if f.isSome: result.add f.get
    r.p = (r.p + 3) div 4 * 4

proc findLinkedFile*(globals: openArray[PsdGlobalBlock]; uuid: string): Option[LinkedFile] =
  if uuid.len == 0: return
  for b in globals:
    if b.key notin ["lnk2", "lnk3", "lnkD"] or b.data.isNil: continue
    for f in parseLinkedFiles(b.data.data):
      if f.uuid == uuid: return some(f)

proc findLinkedFile*(meta: Metadata; uuid: string): Option[LinkedFile] =
  findLinkedFile(meta.psdGlobalBlocks, uuid)

proc fileType*(b: openArray[byte]): string =
  template at(i: int): int = (if i < b.len: int(b[i]) else: -1)
  if at(0) == ord('8') and at(1) == ord('B') and at(2) == ord('P') and at(3) == ord('S'):
    if at(4) == 0 and at(5) == 2: return "8BPB"
    return "8BPS"
  if at(0) == 0x89 and at(1) == ord('P') and at(2) == ord('N') and at(3) == ord('G'): return "PNGf"
  if at(0) == 0xFF and at(1) == 0xD8: return "JPEG"
  if (at(0) == ord('I') and at(1) == ord('I') and at(2) == 42 and at(3) == 0) or
     (at(0) == ord('M') and at(1) == ord('M') and at(2) == 0 and at(3) == 42): return "TIFF"
  if at(0) == ord('G') and at(1) == ord('I') and at(2) == ord('F'): return "GIFf"
  "    "

proc encodeLinkedFile*(f: LinkedFile): seq[byte] =
  var item: seq[byte]
  for c in "liFD": item.add byte(c)
  item.putU32(7)
  let n = min(f.uuid.len, 255)
  item.add byte(n)
  for i in 0 ..< n: item.add byte(f.uuid[i])
  var units = utf16Units(f.fileName)
  units.add 0
  item.putU32(uint32(units.len))
  for u in units:
    item.add byte(u shr 8)
    item.add byte(u and 0xff)
  for c in fileType(f.bytes): item.add byte(c)
  for c in "8BIM": item.add byte(c)
  item.putU64(uint64(f.bytes.len))
  item.add 0
  item.add f.bytes
  item.putU32(0)        # child document id (version 5)
  item.putU64(0)        # asset modification time, f64 0.0 (version 6)
  item.add 0            # asset locked (version 7)
  result.putU64(uint64(item.len))
  result.add item
  while result.len mod 4 != 0: result.add 0

proc itemUuid(item: openArray[byte]): Option[string] =
  if item.len <= 8: return
  let n = int(item[8])
  if 9 + n > item.len: return
  some(lossyUtf8(item.toOpenArray(9, 8 + n)))

proc splitItems(data: openArray[byte]): seq[(Option[string], seq[byte])] =
  var at = 0
  while at < data.len:
    let restLen = data.len - at
    var endp = -1
    if restLen >= 8:
      let l = beU64At(data, at)
      if l <= uint64(high(int) - 8) and int(l) + 8 <= restLen: endp = int(l) + 8
    if endp < 0:
      result.add (none(string), data[at ..< data.len])
      break
    let padded = min((endp + 3) div 4 * 4, restLen)
    result.add (itemUuid(data.toOpenArray(at + 8, at + endp - 1)), data[at ..< at + padded])
    at += padded

proc rebuildBlock*(data: openArray[byte]; keep: proc(u: string): bool; add: openArray[LinkedFile]): Option[seq[byte]] =
  ## The block with entries `keep` rejects dropped and `add` appended; `none` when unchanged.
  let items = splitItems(data)
  var dropped = false
  for (u, _) in items:
    if u.isSome and not keep(u.get): dropped = true
  if not dropped and add.len == 0: return none(seq[byte])
  var o: seq[byte]
  for (u, bytes) in items:
    if u.isNone or keep(u.get): o.add bytes
  for f in add: o.add encodeLinkedFile(f)
  some(o)

proc blockUuids*(data: openArray[byte]): seq[string] =
  for (u, _) in splitItems(data):
    if u.isSome: result.add u.get

proc copyItem*(data: openArray[byte]; uuid, newUuid: string): Option[seq[byte]] =
  ## The entry `uuid` re-encoded under `newUuid` (an independent copy of a linked source).
  var raw: seq[byte]
  var found = false
  for (u, b) in splitItems(data):
    if u.isSome and u.get == uuid:
      raw = b
      found = true
      break
  if not found or raw.len < 8: return
  let l = beU64At(raw, 0)
  if l > uint64(raw.len - 8): return
  let item = raw[8 ..< 8 + int(l)]
  if item.len <= 8: return
  let oldEnd = 9 + int(item[8])
  if newUuid.len > 255 or oldEnd > item.len: return
  var copy = item[0 ..< 8]
  copy.add byte(newUuid.len)
  for c in newUuid: copy.add byte(c)
  copy.add item[oldEnd ..< item.len]
  var o: seq[byte]
  o.putU64(uint64(copy.len))
  o.add copy
  while o.len mod 4 != 0: o.add 0
  some(o)
