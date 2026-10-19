## The tagged object stream inside `doc.dat` (port of `affinity/src/stream.rs` and `access.rs`): a
## self-describing tree of objects whose fields carry a type code and a four-character tag.
## Parsing needs no schema; unknown fields are kept, so the document mapping can name what it
## could not import. The typed accessors return none when a field is missing or has another type.

import std/[options, tables]
import ../textutil
import ./container

type
  Tag* = distinct uint32
    ## A four-character tag, valued as its readable characters read big-endian (`Desc`, `Chld`).

  ObjId* = int ## index of an object in `Stream.objects`

  SValueKind* = enum
    svInt, svUInt, svFloat, svBool, svEnum, svStr, svInts, svFloats, svBytes, svBlob, svEntry, svFlags, svObj, svArray

  SValue* = object
    ## A field value.
    case kind*: SValueKind
    of svInt: i*: int64
    of svUInt: u*: uint64
    of svFloat: f*: float64
    of svBool: b*: bool
    of svEnum: enumId*, enumVersion*: uint16
    of svStr, svEntry: s*: string ## a string, or the name of another archive entry (`svEntry`)
    of svInts: ints*: seq[int64]  ## fixed-length i32 vectors
    of svFloats: floats*: seq[float64] ## fixed-length f32/f64 vectors (points, rectangles, transforms)
    of svBytes, svBlob: bytes*: seq[byte] ## fixed-size records or structs (`svBytes`), blobs
    of svFlags: flags*: uint64
    of svObj: obj*: int ## the object, or -1 for a null object
    of svArray: arr*: seq[SValue]

  ObjKind* = enum
    okRoot       ## the stream's root
    okShared     ## defined once and referenced by id elsewhere
    okInline
    okPositional ## untagged fields identified by position (curve data)

  SObject* = object
    class*: Tag
    kind*: ObjKind
    chain*: seq[Tag] ## class chain, most derived first (`ShpN`, `VNod`, `Node`)
    fields*: seq[(Tag, SValue)] ## in stream order; positional fields have tag 0

  Stream* = object
    root*: ObjId
    objects*: seq[SObject]

const
  MaxDepth* = 384 ## deepest object nesting accepted
  MaxObjects = 8_000_000
  MaxValues* = 1 shl 24
    ## Most values (fields and array elements) one stream may decode: a small compressed entry
    ## could otherwise ask for tens of gigabytes (a bool array packs eight values in a byte).

proc `==`*(a, b: Tag): bool {.borrow.}

proc tagOf*(s: string): Tag =
  ## The tag of a four-character string.
  var v = 0'u32
  for i in 0 ..< min(s.len, 4): v = (v shl 8) or uint32(byte(s[i]))
  Tag(v)

proc `$`*(t: Tag): string =
  let v = uint32(t)
  var b: seq[byte]
  for sh in [24, 16, 8, 0]: b.add byte((v shr sh) and 0xFF)
  lossyUtf8(b)

proc `==`*(a, b: SValue): bool {.noSideEffect.} =
  if a.kind != b.kind: return false
  case a.kind
  of svInt: a.i == b.i
  of svUInt: a.u == b.u
  of svFloat: a.f == b.f
  of svBool: a.b == b.b
  of svEnum: a.enumId == b.enumId and a.enumVersion == b.enumVersion
  of svStr, svEntry: a.s == b.s
  of svInts: a.ints == b.ints
  of svFloats: a.floats == b.floats
  of svBytes, svBlob: a.bytes == b.bytes
  of svFlags: a.flags == b.flags
  of svObj: a.obj == b.obj
  of svArray:
    if a.arr.len != b.arr.len: return false
    for i in 0 ..< a.arr.len:
      if not (a.arr[i] == b.arr[i]): return false
    true

proc get*(o: SObject; tag: Tag): Option[SValue] =
  for (t, v) in o.fields:
    if t == tag: return some(v)

# ---------------------------------------------------------------- parsing

type Parser = object
  c: Cursor
  objects: seq[SObject]
  shared: Table[uint32, ObjId] ## shared ids, defined at first use and referenced backwards
  depth: int
  values: int ## values decoded so far, against `MaxValues`

proc push(p: var Parser; o: SObject): ObjId =
  if p.objects.len >= MaxObjects: raise limit("too many objects")
  p.objects.add o
  p.objects.len - 1

proc spend(p: var Parser; n: int) =
  ## Counts `n` more values against `MaxValues`, before anything is allocated for them.
  if n < 0 or p.values > MaxValues - n: raise limit("too many values in the document stream")
  p.values += n

proc newVals(p: var Parser; n: int): seq[SValue] =
  p.spend(n)
  newSeqOfCap[SValue](min(n, 1 shl 16))

proc count(p: var Parser; minBytes: int): int =
  ## Element count of an array: each element takes at least `minBytes` bytes, so a count the
  ## remaining input cannot hold is rejected before anything is allocated.
  let n = int(p.c.u32)
  let m = max(minBytes, 1)
  if n > high(int) div m or n * m > p.c.remaining: raise malformed("array longer than its data")
  n

proc str(p: var Parser): string =
  let n = int(p.c.u32)
  lossyUtf8(p.c.take(n))

proc f32At(p: var Parser): float64 = float64(cast[float32](p.c.u32))
proc f64At(p: var Parser): float64 = cast[float64](p.c.u64)

proc floatsOf(p: var Parser; n: int; wide: bool): SValue =
  result = SValue(kind: svFloats)
  for _ in 0 ..< n: result.floats.add(if wide: p.f64At else: p.f32At)

proc fields(p: var Parser; tagged: bool; class: Tag): seq[(Tag, SValue)]
proc inlineObj(p: var Parser; class: Option[Tag]): SValue
proc sharedObj(p: var Parser): SValue

proc scalar(p: var Parser; code: uint8; tagged: bool; class: Tag): SValue =
  case code
  of 0x01: SValue(kind: svUInt, u: uint64(p.c.u8))
  of 0x02: SValue(kind: svUInt, u: uint64(p.c.u16))
  of 0x03, 0x2f, 0x34: SValue(kind: svUInt, u: uint64(p.c.u32))
  of 0x04: SValue(kind: svUInt, u: p.c.u64)
  of 0x05: SValue(kind: svInt, i: int64(cast[int8](p.c.u8)))
  of 0x06: SValue(kind: svInt, i: int64(cast[int16](p.c.u16)))
  of 0x07: SValue(kind: svInt, i: int64(cast[int32](p.c.u32)))
  of 0x08: SValue(kind: svInt, i: cast[int64](p.c.u64))
  of 0x09: SValue(kind: svFloat, f: p.f32At)
  of 0x0a: SValue(kind: svFloat, f: p.f64At)
  of 0x15 .. 0x19:
    var v = SValue(kind: svInts)
    for _ in 0 ..< int(code) - 0x13: v.ints.add int64(cast[int32](p.c.u32))
    v
  of 0x1f .. 0x23: p.floatsOf(int(code) - 0x1d, false)
  of 0x24 .. 0x28: p.floatsOf(int(code) - 0x22, true)
  of 0x29: SValue(kind: svBool, b: p.c.u8 != 0)
  of 0x2a:
    let id = p.c.u16
    SValue(kind: svEnum, enumId: id, enumVersion: p.c.u16)
  of 0x2b, 0x2e: SValue(kind: svStr, s: p.str)
  of 0x2c:
    let n = int(p.c.u16)
    SValue(kind: svBytes, bytes: p.c.take(n))
  of 0x35 .. 0x74: SValue(kind: svBytes, bytes: p.c.take(int(code) - 0x34))
  of 0x2d:
    let n = int(p.c.u32)
    SValue(kind: svBlob, bytes: p.c.take(n))
  of 0x33:
    discard p.c.u32
    let n = int(p.c.u32)
    SValue(kind: svEntry, s: lossyUtf8(p.c.take(n)))
  of 0x75:
    discard p.c.u16
    let n = int(p.c.u8)
    if n > 8: raise malformed("flags wider than 64 bits")
    let b = p.c.take(n)
    var v = 0'u64
    for k in countdown(n - 1, 0): v = (v shl 8) or uint64(b[k])
    SValue(kind: svFlags, flags: v)
  of 0x30:
    let f = p.fields(false, class)
    let id = p.push(SObject(class: class, kind: okPositional, fields: f))
    SValue(kind: svObj, obj: id)
  of 0x31, 0x32:
    if not tagged: raise malformed("object without a field tag")
    if code == 0x31: p.sharedObj() else: p.inlineObj(none(Tag))
  else: raise unsupported("unknown field type in document stream")

proc arrayOf(p: var Parser; code: uint8; class: Tag): SValue =
  let minBytes = case code
    of 0x01, 0x05, 0x29: 1
    of 0x02, 0x06: 2
    of 0x03, 0x07, 0x09, 0x2f, 0x34: 4
    of 0x04, 0x08, 0x0a: 8
    of 0x15 .. 0x19: 4 * (int(code) - 0x13)
    of 0x1f .. 0x23: 4 * (int(code) - 0x1d)
    of 0x24 .. 0x28: 8 * (int(code) - 0x22)
    of 0x35 .. 0x74: int(code) - 0x34
    else: 1
  case code
  of 0x29:
    let n = int(p.c.u32)
    let bits = p.c.take((n + 7) div 8)
    var v = p.newVals(n)
    for i in 0 ..< n: v.add SValue(kind: svBool, b: ((bits[i div 8] shr (i mod 8)) and 1) != 0)
    SValue(kind: svArray, arr: v)
  of 0x2a:
    let n = p.count(2)
    let version = p.c.u16
    var v = p.newVals(n)
    for _ in 0 ..< n: v.add SValue(kind: svEnum, enumId: p.c.u16, enumVersion: version)
    SValue(kind: svArray, arr: v)
  of 0x2b, 0x2e:
    discard p.c.u32
    let n = p.count(4)
    var v = p.newVals(n)
    for _ in 0 ..< n: v.add SValue(kind: svStr, s: p.str)
    SValue(kind: svArray, arr: v)
  of 0x2c:
    let n = int(p.c.u32)
    let size = int(p.c.u16)
    if (size != 0 and n > high(int) div size) or n * size > p.c.remaining: raise malformed("array longer than its data")
    var v = p.newVals(n)
    for _ in 0 ..< n: v.add SValue(kind: svBytes, bytes: p.c.take(size))
    SValue(kind: svArray, arr: v)
  of 0x32:
    let n = p.count(1)
    let cls = Tag(p.c.u32)
    discard p.c.u16
    var v = p.newVals(n)
    for _ in 0 ..< n: v.add p.inlineObj(some(cls))
    SValue(kind: svArray, arr: v)
  of 0x2d, 0x33, 0x75: raise malformed("array of an unarrayable type")
  else:
    let n = p.count(minBytes)
    var v = p.newVals(n)
    for _ in 0 ..< n: v.add p.scalar(code, true, class)
    SValue(kind: svArray, arr: v)

proc fields(p: var Parser; tagged: bool; class: Tag): seq[(Tag, SValue)] =
  inc p.depth
  if p.depth > MaxDepth: raise limit("objects nested too deeply")
  while true:
    let t = p.c.u8
    let code = t and 0x7F
    if code == 0: break
    p.spend(1)
    let tag = if tagged: Tag(p.c.u32) else: Tag(0)
    let v = if (t and 0x80) != 0: p.arrayOf(code, class) else: p.scalar(code, tagged, class)
    result.add (tag, v)
  dec p.depth

proc inlineObj(p: var Parser; class: Option[Tag]): SValue =
  ## Inline object; `class` is given once for a whole array of them.
  case p.c.u8
  of 0: return SValue(kind: svObj, obj: -1)
  of 1: discard
  else: raise malformed("object marker")
  var cls: Tag
  if class.isSome: cls = class.get
  else:
    cls = Tag(p.c.u32)
    discard p.c.u16
  let f = p.fields(true, cls)
  let id = p.push(SObject(class: cls, kind: okInline, chain: @[cls], fields: f))
  SValue(kind: svObj, obj: id)

proc sharedObj(p: var Parser): SValue =
  let flag = p.c.u8
  if flag == 0: return SValue(kind: svObj, obj: -1)
  let key = p.c.u32
  case flag
  of 2:
    if key notin p.shared: raise malformed("reference to an undefined object")
    return SValue(kind: svObj, obj: p.shared[key])
  of 1: discard
  else: raise malformed("object marker")
  if key in p.shared: raise malformed("object defined twice")
  # Register before the fields: an object's own fields may refer back to it.
  let id = p.push(SObject(class: Tag(0), kind: okShared))
  p.shared[key] = id
  var chain: seq[Tag]
  var f: seq[(Tag, SValue)]
  while true:
    case p.c.u8
    of 0:
      let tag = Tag(p.c.u32)
      discard p.c.u16
      chain.add tag
      f.add p.fields(true, tag)
    of 1:
      chain.add Tag(p.c.u32)
      break
    of 2: break
    else: raise malformed("object class chain")
    if chain.len > 64: raise limit("object class chain too long")
  let class = if chain.len > 0: chain[0] else: Tag(0)
  f.add p.fields(true, class)
  p.objects[id] = SObject(class: class, kind: okShared, chain: chain, fields: f)
  SValue(kind: svObj, obj: id)

proc parseStream*(bytes: openArray[byte]): Stream =
  ## Parses a stream (`00 FF 4B 53` header). Bytes after the root's terminator are ignored.
  var p = Parser(c: cursor(bytes, 0))
  if not p.c.takeIs("\x00\xffKS"): raise malformed("document stream signature")
  let version = p.c.u16
  if version > 2: raise unsupported("document stream version")
  let class = Tag(p.c.u32)
  discard p.c.u16
  if version == 2: discard p.c.u32
  let root = p.push(SObject(class: class, kind: okRoot, chain: @[class]))
  let f = p.fields(true, class)
  p.objects[root].fields = f
  Stream(root: root, objects: move p.objects)

# ---------------------------------------------------------------- typed access

proc obj*(s: Stream; id: ObjId): Option[SObject] =
  if id >= 0 and id < s.objects.len: some(s.objects[id]) else: none(SObject)

template objRef(s: Stream; id: ObjId): untyped = s.objects[id]

proc fieldPtr*(s: Stream; id: ObjId; tag: string): ptr SValue =
  ## The field `tag` of object `id`, or nil.
  if id < 0 or id >= s.objects.len: return nil
  let t = tagOf(tag)
  for i in 0 ..< s.objects[id].fields.len:
    if s.objects[id].fields[i][0] == t: return unsafeAddr s.objects[id].fields[i][1]
  nil

proc field*(s: Stream; id: ObjId; tag: string): Option[SValue] =
  let p = s.fieldPtr(id, tag)
  if p != nil: some(p[]) else: none(SValue)

proc hasField*(s: Stream; id: ObjId; tag: string): bool = s.fieldPtr(id, tag) != nil

proc class*(s: Stream; id: ObjId): Option[Tag] =
  if id >= 0 and id < s.objects.len: some(s.objects[id].class) else: none(Tag)

proc isClass*(s: Stream; id: ObjId; class: string): bool = s.class(id) == some(tagOf(class))

proc child*(s: Stream; id: ObjId; tag: string): Option[ObjId] =
  ## A child object (shared, linked or inline); none for null or missing.
  let p = s.fieldPtr(id, tag)
  if p != nil and p.kind == svObj and p.obj >= 0: some(p.obj) else: none(ObjId)

proc children*(s: Stream; id: ObjId; tag: string): seq[ObjId] =
  ## An array of objects (nulls skipped).
  let p = s.fieldPtr(id, tag)
  if p != nil and p.kind == svArray:
    for v in p.arr:
      if v.kind == svObj and v.obj >= 0: result.add v.obj

proc f64*(s: Stream; id: ObjId; tag: string): Option[float64] =
  let p = s.fieldPtr(id, tag)
  if p != nil and p.kind == svFloat and p.f == p.f and abs(p.f) != Inf: some(p.f) else: none(float64)

proc intOf*(s: Stream; id: ObjId; tag: string): Option[int64] =
  let p = s.fieldPtr(id, tag)
  if p == nil: return
  case p.kind
  of svInt: some(p.i)
  of svUInt: (if p.u <= uint64(high(int64)): some(int64(p.u)) else: none(int64))
  else: none(int64)

proc boolean*(s: Stream; id: ObjId; tag: string): Option[bool] =
  let p = s.fieldPtr(id, tag)
  if p != nil and p.kind == svBool: some(p.b) else: none(bool)

proc str*(s: Stream; id: ObjId; tag: string): Option[string] =
  let p = s.fieldPtr(id, tag)
  if p != nil and p.kind == svStr: some(p.s) else: none(string)

proc enumeration*(s: Stream; id: ObjId; tag: string): Option[(uint16, uint16)] =
  let p = s.fieldPtr(id, tag)
  if p != nil and p.kind == svEnum: some((p.enumId, p.enumVersion)) else: none((uint16, uint16))

proc floatsN*(s: Stream; id: ObjId; tag: string; n: int): Option[seq[float64]] =
  ## A fixed vector of `n` finite floats (points, rectangles, transforms).
  let p = s.fieldPtr(id, tag)
  if p == nil or p.kind != svFloats or p.floats.len != n: return
  for f in p.floats:
    if not (f == f and abs(f) != Inf): return
  some(p.floats)

proc bytesOf*(s: Stream; id: ObjId; tag: string): Option[seq[byte]] =
  let p = s.fieldPtr(id, tag)
  if p != nil and p.kind in {svBytes, svBlob}: some(p.bytes) else: none(seq[byte])

proc entry*(s: Stream; id: ObjId; tag: string): Option[string] =
  let p = s.fieldPtr(id, tag)
  if p != nil and p.kind == svEntry: some(p.s) else: none(string)

proc positional*(s: Stream; id: ObjId): seq[SValue] =
  ## Untagged fields of a positional object, in order.
  if id >= 0 and id < s.objects.len:
    for (_, v) in s.objects[id].fields: result.add v
