## Paint.NET documents (`.pdn`, PDN3): a .NET BinaryFormatter (NRBF) object graph followed by
## the layers' BGRA pixel memory in gzip chunks (Rust `pdn.rs` + `pdn/nrbf.rs`). Import only.

import std/[options, tables, strutils, unicode]
import ../geom, ../color, ../raster, ../doc, ../zlib
import common

const
  MaxPixelBytes = 1 shl 30
  MaxLayers = 1024
  MaxItems = 1_000_000
  MaxStrings = 64 shl 20
  MaxDepth = 64
  Modes = [bmNormal, bmMultiply, bmLinearDodge, bmPaintNetColorBurn, bmPaintNetColorDodge, bmReflect, bmGlow,
           bmOverlay, bmDifference, bmNegation, bmLighten, bmDarken, bmScreen, bmXor]
  OldModes = ["Normal", "Multiply", "Additive", "ColorBurn", "ColorDodge", "Reflect", "Glow", "Overlay",
              "Difference", "Negation", "Lighten", "Darken", "Screen", "Xor"]

type
  PdnReader = object
    bytes: seq[byte]
    pos: int

  NClass = ref object
    name: string
    names: seq[string]
    primitives: seq[int]   ## primitive type tag, -1 for a member value record

  ValueKind = enum vNull, vBool, vInt, vString, vRef, vArray, vObject
  Value = ref object
    case kind: ValueKind
    of vNull: discard
    of vBool: b: bool
    of vInt: i: int64
    of vString: s: string
    of vRef: id: int32
    of vArray: items: seq[Value]
    of vObject:
      class: NClass
      members: seq[Value]

  Graph = object
    root: Value
    objects: Table[int32, Value]
    order: seq[int32]

  Parser = object
    graph: Graph
    classes: Table[int32, NClass]
    items, strings: int

proc invalid(msg: string): ref IoFailure = ioError(iePdn, msg)

proc take(r: var PdnReader; len: int): seq[byte] =
  if len < 0 or r.pos > r.bytes.len or len > r.bytes.len - r.pos: raise invalid("truncated document")
  result = r.bytes[r.pos ..< r.pos + len]
  r.pos += len

proc skip(r: var PdnReader; len: int) =
  if len < 0 or r.pos > r.bytes.len or len > r.bytes.len - r.pos: raise invalid("truncated document")
  r.pos += len

proc byte1(r: var PdnReader): uint8 =
  if r.pos >= r.bytes.len: raise invalid("truncated document")
  result = r.bytes[r.pos]
  inc r.pos

proc leI32(r: var PdnReader): int32 =
  if r.pos + 4 > r.bytes.len: raise invalid("truncated document")
  result = cast[int32](uint32(r.bytes[r.pos]) or (uint32(r.bytes[r.pos + 1]) shl 8) or
                       (uint32(r.bytes[r.pos + 2]) shl 16) or (uint32(r.bytes[r.pos + 3]) shl 24))
  r.pos += 4

proc leU(r: var PdnReader; n: int): uint64 =
  if r.pos + n > r.bytes.len: raise invalid("truncated document")
  for i in countdown(n - 1, 0): result = (result shl 8) or uint64(r.bytes[r.pos + i])
  r.pos += n

proc beU32(r: var PdnReader): int =
  if r.pos + 4 > r.bytes.len: raise invalid("truncated document")
  result = (int(r.bytes[r.pos]) shl 24) or (int(r.bytes[r.pos + 1]) shl 16) or (int(r.bytes[r.pos + 2]) shl 8) or
           int(r.bytes[r.pos + 3])
  r.pos += 4

proc count(r: var PdnReader): int =
  let n = r.leI32()
  if n < 0: raise invalid("negative count")
  if n > MaxItems: raise invalid("serialized array or class is too large")
  int(n)

# ---------------------------------------------------------------- graph access

proc resolve(g: Graph; v0: Value): Value =
  var v = v0
  for _ in 0 ..< MaxDepth:
    if v.isNil: raise invalid("missing value")
    if v.kind != vRef: return v
    if v.id notin g.objects: raise invalid("missing object reference")
    v = g.objects[v.id]
  raise invalid("cyclic or excessively deep object reference")

proc className(g: Graph; v: Value): string =
  let r = g.resolve(v)
  if r.kind != vObject: raise invalid("expected a serialized object")
  r.class.name

proc optionalField(g: Graph; v: Value; name: string): Option[Value] =
  let r = g.resolve(v)
  if r.kind != vObject: raise invalid("expected a serialized object")
  let i = r.class.names.find(name)
  if i >= 0 and i < r.members.len: some(r.members[i]) else: none(Value)

proc field(g: Graph; v: Value; name: string): Value =
  let f = g.optionalField(v, name)
  if f.isNone: raise invalid("missing field " & name)
  f.get

proc integer(g: Graph; v: Value): int64 =
  let r = g.resolve(v)
  if r.kind != vInt: raise invalid("expected an integer")
  r.i

proc boolean(g: Graph; v: Value): bool =
  let r = g.resolve(v)
  if r.kind != vBool: raise invalid("expected a boolean")
  r.b

proc str(g: Graph; v: Value): string =
  let r = g.resolve(v)
  if r.kind != vString: raise invalid("expected a string")
  r.s

proc arr(g: Graph; v: Value): seq[Value] =
  let r = g.resolve(v)
  if r.kind != vArray: raise invalid("expected an array")
  r.items

# ---------------------------------------------------------------- NRBF parser

proc budget(p: var Parser; n: int) =
  if n > MaxItems - p.items: raise invalid("too many serialized objects or members")
  p.items += n

proc nstring(p: var Parser; r: var PdnReader): string =
  var len = 0
  var shift = 0
  while shift < 35:
    let b = r.byte1()
    if shift == 28 and b > 7: raise invalid("invalid string length")
    len = len or (int(b and 127) shl shift)
    if (b and 128) == 0:
      if len > MaxStrings - p.strings: raise invalid("serialized strings are too large")
      p.strings += len
      let bytes = r.take(len)
      var s = newString(len)
      for i in 0 ..< len: s[i] = char(bytes[i])
      if validateUtf8(s) >= 0: raise invalid("invalid UTF-8 string")
      return s
    shift += 7
  raise invalid("invalid string length")

proc primitive(p: var Parser; r: var PdnReader; tag: int): Value =
  case tag
  of 1:
    case r.byte1()
    of 0: Value(kind: vBool, b: false)
    of 1: Value(kind: vBool, b: true)
    else: raise invalid("invalid boolean")
  of 2: Value(kind: vInt, i: int64(r.byte1()))
  of 3:
    let first = r.byte1()
    let len = if first <= 0x7f: 1 elif first >= 0xc2 and first <= 0xdf: 2 elif first >= 0xe0 and first <= 0xef: 3
              else: raise invalid("invalid character")
    var s = $char(first)
    for b in r.take(len - 1): s.add char(b)
    if validateUtf8(s) >= 0: raise invalid("invalid character")
    Value(kind: vString, s: s)
  of 5, 18: Value(kind: vString, s: p.nstring(r))
  of 6:
    r.skip(8)
    Value(kind: vNull)
  of 7: Value(kind: vInt, i: int64(cast[int16](uint16(r.leU(2)))))
  of 8: Value(kind: vInt, i: int64(cast[int32](uint32(r.leU(4)))))
  of 9, 12: Value(kind: vInt, i: cast[int64](r.leU(8)))
  of 10: Value(kind: vInt, i: int64(cast[int8](r.byte1())))
  of 11:
    r.skip(4)
    Value(kind: vNull)
  of 13, 16: Value(kind: vInt, i: cast[int64](r.leU(8)))
  of 14: Value(kind: vInt, i: int64(r.leU(2)))
  of 15: Value(kind: vInt, i: int64(r.leU(4)))
  of 17: Value(kind: vNull)
  else: raise invalid("unsupported primitive type " & $tag)

proc typeInfo(p: var Parser; r: var PdnReader; tag: int): int =
  case tag
  of 0: int(r.byte1())
  of 1, 2, 5, 6: -1
  of 3:
    discard p.nstring(r)
    -1
  of 4:
    discard p.nstring(r)
    discard r.leI32()
    -1
  of 7:
    discard r.byte1()
    -1
  else: raise invalid("unsupported binary type " & $tag)

proc reserve(p: var Parser; id: int32) =
  if id == 0 or id in p.graph.objects: raise invalid("duplicate or zero object id")
  p.graph.objects[id] = Value(kind: vNull)
  p.graph.order.add id

proc record(p: var Parser; r: var PdnReader; tag: int; depth: int; ctl: Interrupt): Value

proc value(p: var Parser; r: var PdnReader; depth: int; ctl: Interrupt): Value =
  while true:
    let tag = int(r.byte1())
    if tag == 12: discard p.record(r, tag, depth, ctl)
    else: return p.record(r, tag, depth, ctl)

proc obj(p: var Parser; r: var PdnReader; id: int32; class: NClass; depth: int; ctl: Interrupt): Value =
  p.reserve(id)
  p.budget(class.names.len)
  var members: seq[Value]
  for prim in class.primitives:
    members.add (if prim >= 0: p.primitive(r, prim) else: p.value(r, depth + 1, ctl))
  p.graph.objects[id] = Value(kind: vObject, class: class, members: members)
  Value(kind: vRef, id: id)

proc array(p: var Parser; r: var PdnReader; id: int32; len: int; prim: int; depth: int; ctl: Interrupt): Value =
  p.reserve(id)
  p.budget(len)
  if prim == 2:
    r.skip(len)
    return Value(kind: vRef, id: id)
  var values: seq[Value]
  while values.len < len:
    if ctl.cancelled: raise ioError(ieCancelled)
    if prim >= 0:
      values.add p.primitive(r, prim)
      continue
    let tag = int(r.byte1())
    if tag == 13 or tag == 14:
      let c = if tag == 13: int(r.byte1()) else: r.count()
      if c == 0 or c > len - values.len: raise invalid("invalid array null run")
      for _ in 0 ..< c: values.add Value(kind: vNull)
    elif tag == 12: discard p.record(r, tag, depth + 1, ctl)
    else: values.add p.record(r, tag, depth + 1, ctl)
  p.graph.objects[id] = Value(kind: vArray, items: values)
  Value(kind: vRef, id: id)

proc record(p: var Parser; r: var PdnReader; tag: int; depth: int; ctl: Interrupt): Value =
  if ctl.cancelled: raise ioError(ieCancelled)
  p.budget(1)
  if depth > MaxDepth: raise invalid("serialized objects nested too deeply")
  case tag
  of 1:
    let id = r.leI32()
    let metadata = r.leI32()
    if metadata notin p.classes: raise invalid("missing class metadata")
    p.obj(r, id, p.classes[metadata], depth, ctl)
  of 2 .. 5:
    let id = r.leI32()
    let name = p.nstring(r)
    let c = r.count()
    p.budget(c)
    var names: seq[string]
    for _ in 0 ..< c: names.add p.nstring(r)
    var prims = newSeq[int](c)
    for x in prims.mitems: x = -1
    if tag >= 4:
      let types = r.take(c)
      for i, t in types: prims[i] = p.typeInfo(r, int(t))
    if tag == 3 or tag == 5: discard r.leI32()
    let class = NClass(name: name, names: names, primitives: prims)
    if id in p.classes: raise invalid("duplicate class metadata")
    p.classes[id] = class
    p.obj(r, id, class, depth, ctl)
  of 6:
    let id = r.leI32()
    p.reserve(id)
    p.graph.objects[id] = Value(kind: vString, s: p.nstring(r))
    Value(kind: vRef, id: id)
  of 7:
    let id = r.leI32()
    let kind = r.byte1()
    let rank = r.count()
    if kind > 5 or rank == 0 or rank > 32: raise invalid("invalid array dimensions")
    var len = 1
    for _ in 0 ..< rank:
      let d = r.count()
      if d != 0 and len > high(int) div d: raise invalid("array size overflow")
      len *= d
    if kind >= 3:
      for _ in 0 ..< rank:
        if r.leI32() != 0: raise invalid("nonzero array lower bounds are unsupported")
    let t = int(r.byte1())
    let prim = p.typeInfo(r, t)
    p.array(r, id, len, prim, depth, ctl)
  of 8:
    let prim = int(r.byte1())
    p.primitive(r, prim)
  of 9: Value(kind: vRef, id: r.leI32())
  of 10: Value(kind: vNull)
  of 12:
    discard r.leI32()
    discard p.nstring(r)
    Value(kind: vNull)
  of 15 .. 17:
    let id = r.leI32()
    let len = r.count()
    let prim = if tag == 15: int(r.byte1()) else: -1
    p.array(r, id, len, prim, depth, ctl)
  else: raise invalid("unsupported NRBF record " & $tag)

proc parseNrbf(r: var PdnReader; ctl: Interrupt): Graph =
  if r.byte1() != 0: raise invalid("missing NRBF stream header")
  let root = r.leI32()
  discard r.leI32()
  if root == 0 or r.leI32() != 1 or r.leI32() != 0: raise invalid("unsupported NRBF stream version")
  var p = Parser(graph: Graph(root: Value(kind: vRef, id: root)))
  while true:
    let tag = int(r.byte1())
    if tag == 11: break
    discard p.record(r, tag, 0, ctl)
  discard p.graph.resolve(p.graph.root)
  p.graph

# ---------------------------------------------------------------- document

proc countField(g: Graph; o: Value; f: string): int =
  let v = g.integer(g.field(o, f))
  if v < 0 or v > int64(high(int32)) * 4: raise invalid("invalid " & f)
  int(v)

proc blendOf(g: Graph; layer, props: Value): BlendMode =
  let m = g.optionalField(props, "blendMode")
  if m.isSome:
    let rv = g.resolve(m.get)
    let mode = if rv.kind == vInt: rv.i else: g.integer(g.field(m.get, "value__"))
    if mode < 0: raise invalid("invalid blend mode")
    if mode >= Modes.len: raise invalid("unsupported blend mode " & $mode)
    return Modes[mode]
  let p2 = g.field(layer, "properties")
  let name = g.className(g.field(p2, "blendOp"))
  const pre = "PaintDotNet.UserBlendOps+"
  if not name.startsWith(pre) or not name.endsWith("BlendOp"): raise invalid("unsupported blend operation " & name)
  let mid = name[pre.len ..< name.len - "BlendOp".len]
  let i = OldModes.find(mid)
  if i < 0: raise invalid("unsupported blend operation " & mid)
  Modes[i]

proc pixels(r: var PdnReader; length: int; ctl: Interrupt): seq[byte] =
  let version = r.byte1()
  if version > 1: raise invalid("unsupported pixel compression " & $version)
  let chunkSize = r.beU32()
  if chunkSize == 0: raise invalid("zero pixel chunk size")
  let chunks = (length + chunkSize - 1) div chunkSize
  if chunks > 1_000_000: raise invalid("too many pixel chunks")
  if version == 1 and r.bytes.len - r.pos < length: raise invalid("uncompressed pixel data is truncated")
  var seen = newSeq[bool](chunks)
  result = newSeq[byte](length)
  for _ in 0 ..< chunks:
    if ctl.cancelled: raise ioError(ieCancelled)
    let index = r.beU32()
    if index >= chunks: raise invalid("pixel chunk index out of bounds")
    if seen[index]: raise invalid("duplicate pixel chunk")
    seen[index] = true
    let size = r.beU32()
    let raw = r.take(size)
    if chunkSize != 0 and index > high(int) div chunkSize: raise invalid("pixel offset overflow")
    let start = index * chunkSize
    let n = min(chunkSize, max(length - start, 0))
    if start + n > length: raise invalid("invalid pixel chunk range")
    if version == 0:
      var data: seq[byte]
      try: data = gzipDecompressExact(raw, n)
      except ZlibError as e:
        if "excess" in e.msg or "checksum" in e.msg or "size" in e.msg or "expected" in e.msg:
          raise invalid("pixel chunk has excess data")
        raise invalid("invalid gzip pixel chunk: " & e.msg)
      if n > 0: copyMem(addr result[start], unsafeAddr data[0], n)
    else:
      if raw.len != n: raise invalid("incorrect uncompressed pixel chunk size")
      if n > 0: copyMem(addr result[start], unsafeAddr raw[0], n)

proc importPdn*(name: string; bytes: openArray[byte]; ctl: Interrupt): ImportResult =
  var r = PdnReader(bytes: @bytes)
  if r.take(4) != cast[seq[byte]]("PDN3"): raise invalid("unsupported document signature (expected PDN3)")
  let hs = r.take(3)
  r.skip(int(hs[0]) or (int(hs[1]) shl 8) or (int(hs[2]) shl 16))
  if r.take(2) != @[0'u8, 1]: raise invalid("unsupported PDN3 serialization version")
  let g = parseNrbf(r, ctl)
  let root = g.root
  if g.className(root) != "PaintDotNet.Document": raise invalid("root object is not a Paint.NET document")
  let width = g.countField(root, "width")
  let height = g.countField(root, "height")
  let rowBytes = width * 4
  if width == 0 or height == 0 or width > int(high(int32)) or height > int(high(int32)) or
     rowBytes * height > MaxPixelBytes:
    raise invalid("canvas is empty or exceeds the 1 GiB pixel limit")
  let list = g.field(root, "layers")
  let items = g.arr(g.field(list, "ArrayList+_items"))
  let n = g.countField(list, "ArrayList+_size")
  if n == 0 or n > MaxLayers: raise invalid("document must have 1..1024 bitmap layers")
  if n > items.len: raise invalid("layer list count exceeds its array")
  var layouts = initTable[int32, int]()
  var output: seq[(int32, Layer)]
  for k in 0 ..< n:
    if ctl.cancelled: raise ioError(ieCancelled)
    let layer = items[k]
    if g.className(layer) != "PaintDotNet.BitmapLayer": raise invalid("only PDN3 bitmap layers are supported")
    if g.countField(layer, "Layer+width") != width or g.countField(layer, "Layer+height") != height:
      raise invalid("layer dimensions do not match the canvas")
    let surface = g.field(layer, "surface")
    if g.className(surface) != "PaintDotNet.Surface" or g.countField(surface, "width") != width or
       g.countField(surface, "height") != height:
      raise invalid("invalid bitmap surface")
    let stride = g.countField(surface, "stride")
    if stride < rowBytes: raise invalid("surface stride is too short for BGRA32 pixels")
    let memory = g.field(surface, "scan0")
    if memory.kind != vRef: raise invalid("missing pixel memory reference")
    if memory.id in layouts and layouts[memory.id] != stride:
      raise invalid("shared pixel memory has inconsistent strides")
    layouts[memory.id] = stride
    let props = g.field(layer, "Layer+properties")
    let opacity = g.countField(props, "opacity")
    if opacity > 255: raise invalid("opacity exceeds 255")
    var l = rasterLayer(g.str(g.field(props, "name")), pixelFormat(cmRgb, stU8, true))
    l.visible = g.boolean(g.field(props, "visible"))
    l.opacity = float32(opacity) / 255
    l.blend = blendOf(g, layer, props)
    output.add (memory.id, l)
  var blocks: seq[(int32, Value)]
  for id in g.order:
    let v = g.objects[id]
    if v.kind == vObject and v.class.name == "PaintDotNet.MemoryBlock": blocks.add (id, v)
  var total = 0
  for (id, blk) in blocks:
    let length = g.countField(blk, "length64")
    if id notin layouts: raise invalid("unreferenced pixel memory block")
    let stride = layouts[id]
    if length != stride * height: raise invalid("pixel memory length does not match the surface")
    if g.boolean(g.field(blk, "hasParent")) or not g.boolean(g.field(blk, "deferred")):
      raise invalid("unsupported non-deferred or parented pixel memory")
    total += length
    if total > MaxPixelBytes: raise invalid("decoded pixels exceed the 1 GiB limit")
  var surfaces = initTable[int32, Surface]()
  for i, (id, blk) in blocks:
    let length = g.countField(blk, "length64")
    var data = pixels(r, length, ctl)
    let stride = layouts[id]
    var surface = newSurface(pixelFormat(cmRgb, stU8, true))
    var y = 0
    while (y + 1) * stride <= data.len:
      if ctl.cancelled: raise ioError(ieCancelled)
      let o = y * stride
      var row = data[o ..< o + rowBytes]
      var p = 0
      while p + 3 < row.len:
        swap(row[p], row[p + 2])
        p += 4
      surface.writeInterleaved(rectXywh(0, y, width, 1), row)
      inc y
    surfaces[id] = surface
    ctl.progress(0.05'f32 + 0.95'f32 * float32(i + 1) / float32(blocks.len))
  if r.pos != r.bytes.len: raise invalid("unexpected trailing data")
  var d = newDocument(name, size(width, height), cmRgb, stU8)
  for (id, layer0) in output:
    var layer = layer0
    if id notin surfaces: raise invalid("missing pixel memory block")
    layer.content = rasterContent(surfaces[id])
    d.layers.add layer
  var warnings: seq[string]
  let meta = g.optionalField(root, "userMetadataItems")
  if meta.isSome and g.resolve(meta.get).kind != vNull and g.arr(meta.get).len > 0:
    warnings.add "Paint.NET document metadata was not imported"
  ImportResult(document: d, warnings: warnings, sourceReadOnly: true)
