## Photoshop ActionDescriptor structures (OSType-tagged key/value trees), found inside many
## tagged blocks (`lfx2`, `SoCo`, `GdFl`, `TySh`, `PlLd`, …), usually wrapped in a
## `VersionedDescriptor` (version 16). Parsing and writing are exact: `write(parse(b)) == b`.

import std/options
import ./io

const MaxDepth* = 64  ## maximum nesting depth accepted when parsing

type
  UnicodeString* = object
    ## UTF-16 code units, preserved exactly (including trailing NULs and unpaired surrogates).
    units*: seq[uint16]

  Id* = object
    ## A class/key/type identifier: a 4-character code (stored with length 0) or a string.
    code*: bool
    s*: string

  Class* = object
    name*: UnicodeString
    classId*: Id

  RefKind* = enum rkProperty, rkClass, rkEnumerated, rkOffset, rkIdentifier, rkIndex, rkName
  ReferenceItem* = object
    class*: Class
    case kind*: RefKind
    of rkProperty: key*: Id
    of rkEnumerated: typeId*, value*: Id
    of rkOffset: offset*: int32
    of rkIdentifier, rkIndex: num*: uint32
    of rkName: name*: UnicodeString
    of rkClass: discard

  ValueKind* = enum
    vkReference, vkDescriptor, vkGlobalObject, vkList, vkDouble, vkUnitFloat, vkUnitFloats,
    vkText, vkEnumerated, vkInteger, vkLargeInteger, vkBoolean, vkClass, vkGlobalClass, vkAlias,
    vkPath, vkRawData, vkObjectArray

  Value* = object
    case kind*: ValueKind
    of vkReference: refs*: seq[ReferenceItem]
    of vkDescriptor, vkGlobalObject: desc*: Descriptor
    of vkList: list*: seq[Value]
    of vkDouble: d*: float64
    of vkUnitFloat:
      unit*: FourCC
      uv*: float64
    of vkUnitFloats:
      unitN*: FourCC
      values*: seq[float64]
    of vkText: text*: UnicodeString
    of vkEnumerated: enumType*, enumValue*: Id
    of vkInteger: i*: int32
    of vkLargeInteger: li*: int64
    of vkBoolean: b*: bool
    of vkClass, vkGlobalClass: cls*: Class
    of vkAlias, vkPath, vkRawData: raw*: seq[byte]
    of vkObjectArray:
      prefix*: uint32
      body*: Descriptor

  Descriptor* = object
    name*: UnicodeString
    classId*: Id
    items*: seq[(Id, Value)]

  VersionedDescriptor* = object
    version*: uint32
    descriptor*: Descriptor

# ---------------------------------------------------------------- constructors

proc ustr*(s: string): UnicodeString = UnicodeString(units: utf16Units(s))
proc ustrNul*(s: string): UnicodeString =
  result = UnicodeString(units: utf16Units(s))
  result.units.add 0
proc `$`*(u: UnicodeString): string =
  ## Lossy conversion, stripping trailing NULs.
  var e = u.units.len
  while e > 0 and u.units[e - 1] == 0: dec e
  fromUtf16(u.units.toOpenArray(0, e - 1))

proc id*(s: string): Id = Id(code: s.len == 4, s: s)
proc `$`*(i: Id): string = i.s
proc isId*(i: Id; s: string): bool = i.s == s

proc newDescriptor*(classId: string): Descriptor = Descriptor(classId: id(classId))
proc with*(d: Descriptor; key: string; v: Value): Descriptor =
  result = d
  result.items.add (id(key), v)

proc get*(d: Descriptor; key: string): Option[Value] =
  for (k, v) in d.items:
    if k.s == key: return some(v)
  none(Value)

proc getPtr*(d: Descriptor; key: string): ptr Value =
  for i in 0 ..< d.items.len:
    if d.items[i][0].s == key: return unsafeAddr d.items[i][1]
  nil

proc vDouble*(x: float64): Value = Value(kind: vkDouble, d: x)
proc vInt*(x: int32): Value = Value(kind: vkInteger, i: x)
proc vBool*(x: bool): Value = Value(kind: vkBoolean, b: x)
proc vText*(s: string): Value = Value(kind: vkText, text: ustrNul(s))
proc vUnit*(unit: string; x: float64): Value = Value(kind: vkUnitFloat, unit: unit, uv: x)
proc vEnum*(t, v: string): Value = Value(kind: vkEnumerated, enumType: id(t), enumValue: id(v))
proc vDesc*(d: Descriptor): Value = Value(kind: vkDescriptor, desc: d)
proc vList*(l: seq[Value]): Value = Value(kind: vkList, list: l)

# ---------------------------------------------------------------- equality

func `==`*(a, b: ReferenceItem): bool =
  if a.kind != b.kind or a.class != b.class: return false
  case a.kind
  of rkProperty: a.key == b.key
  of rkEnumerated: a.typeId == b.typeId and a.value == b.value
  of rkOffset: a.offset == b.offset
  of rkIdentifier, rkIndex: a.num == b.num
  of rkName: a.name == b.name
  of rkClass: true

func `==`*(a, b: Descriptor): bool
func `==`*(a, b: Value): bool =
  if a.kind != b.kind: return false
  case a.kind
  of vkReference: a.refs == b.refs
  of vkDescriptor, vkGlobalObject: a.desc == b.desc
  of vkList: a.list == b.list
  of vkDouble: a.d == b.d or (a.d != a.d and b.d != b.d)
  of vkUnitFloat: a.unit == b.unit and a.uv == b.uv
  of vkUnitFloats: a.unitN == b.unitN and a.values == b.values
  of vkText: a.text == b.text
  of vkEnumerated: a.enumType == b.enumType and a.enumValue == b.enumValue
  of vkInteger: a.i == b.i
  of vkLargeInteger: a.li == b.li
  of vkBoolean: a.b == b.b
  of vkClass, vkGlobalClass: a.cls == b.cls
  of vkAlias, vkPath, vkRawData: a.raw == b.raw
  of vkObjectArray: a.prefix == b.prefix and a.body == b.body

func `==`*(a, b: Descriptor): bool =
  if a.name != b.name or a.classId != b.classId or a.items.len != b.items.len: return false
  for i in 0 ..< a.items.len:
    if a.items[i][0] != b.items[i][0] or a.items[i][1] != b.items[i][1]: return false
  true

# ---------------------------------------------------------------- reading

proc readUstr*(r: var Reader): UnicodeString = UnicodeString(units: readUnicodeUnits(r))
proc write*(u: UnicodeString; o: var seq[byte]) = writeUnicodeUnits(o, u.units)

proc readId(r: var Reader): Id =
  let n = r.u32()
  if n == 0: Id(code: true, s: r.fourcc())
  else:
    let b = r.bytesU64(uint64(n))
    var s = newString(b.len)
    for i, c in b: s[i] = char(c)
    Id(code: false, s: s)

proc write*(i: Id; o: var seq[byte]) =
  if i.code and i.s.len == 4:
    o.putU32(0)
    o.put(i.s)
  else:
    o.putU32(uint32(i.s.len))
    o.put(i.s)

proc readClass(r: var Reader): Class =
  result.name = readUstr(r)
  result.classId = readId(r)

proc write(c: Class; o: var seq[byte]) =
  c.name.write(o)
  c.classId.write(o)

proc readDescriptor(r: var Reader; depth: int): Descriptor
proc readValue(r: var Reader; depth: int): Value

proc readReferenceItem(r: var Reader): ReferenceItem =
  let ty = r.fourcc()
  case ty
  of "prop":
    let c = readClass(r)
    ReferenceItem(kind: rkProperty, class: c, key: readId(r))
  of "Clss": ReferenceItem(kind: rkClass, class: readClass(r))
  of "Enmr":
    let c = readClass(r)
    let t = readId(r)
    ReferenceItem(kind: rkEnumerated, class: c, typeId: t, value: readId(r))
  of "rele":
    let c = readClass(r)
    ReferenceItem(kind: rkOffset, class: c, offset: r.i32())
  of "Idnt": ReferenceItem(kind: rkIdentifier, num: r.u32())
  of "indx": ReferenceItem(kind: rkIndex, num: r.u32())
  of "name":
    let c = readClass(r)
    ReferenceItem(kind: rkName, class: c, name: readUstr(r))
  else: raise unsupported("reference item type " & ty)

proc readTypedValue(r: var Reader; ty: string; depth: int): Value =
  case ty
  of "obj ":
    let n = r.u32()
    r.checkCount(uint64(n), 8)
    var items: seq[ReferenceItem]
    for _ in 0'u32 ..< n: items.add readReferenceItem(r)
    Value(kind: vkReference, refs: items)
  of "Objc": Value(kind: vkDescriptor, desc: readDescriptor(r, depth + 1))
  of "GlbO": Value(kind: vkGlobalObject, desc: readDescriptor(r, depth + 1))
  of "VlLs":
    let n = r.u32()
    r.checkCount(uint64(n), 4)
    var items: seq[Value]
    for _ in 0'u32 ..< n: items.add readValue(r, depth + 1)
    Value(kind: vkList, list: items)
  of "doub": Value(kind: vkDouble, d: r.f64())
  of "UntF":
    let u = r.fourcc()
    Value(kind: vkUnitFloat, unit: u, uv: r.f64())
  of "UnFl":
    let u = r.fourcc()
    let n = r.u32()
    r.checkCount(uint64(n), 8)
    var vs: seq[float64]
    for _ in 0'u32 ..< n: vs.add r.f64()
    Value(kind: vkUnitFloats, unitN: u, values: vs)
  of "TEXT": Value(kind: vkText, text: readUstr(r))
  of "enum":
    let t = readId(r)
    Value(kind: vkEnumerated, enumType: t, enumValue: readId(r))
  of "long": Value(kind: vkInteger, i: r.i32())
  of "comp": Value(kind: vkLargeInteger, li: r.i64())
  of "bool": Value(kind: vkBoolean, b: r.u8() != 0)
  of "type": Value(kind: vkClass, cls: readClass(r))
  of "GlbC": Value(kind: vkGlobalClass, cls: readClass(r))
  of "alis": Value(kind: vkAlias, raw: r.bytesU64(uint64(r.u32())))
  of "Pth ": Value(kind: vkPath, raw: r.bytesU64(uint64(r.u32())))
  of "tdta": Value(kind: vkRawData, raw: r.bytesU64(uint64(r.u32())))
  of "ObAr":
    let p = r.u32()
    Value(kind: vkObjectArray, prefix: p, body: readDescriptor(r, depth + 1))
  else: raise unsupported("descriptor OSType " & ty)

proc readValue(r: var Reader; depth: int): Value =
  if depth > MaxDepth: raise limitExceeded("descriptor nesting too deep")
  let ty = r.fourcc()
  readTypedValue(r, ty, depth)

proc readDescriptor(r: var Reader; depth: int): Descriptor =
  if depth > MaxDepth: raise limitExceeded("descriptor nesting too deep")
  result.name = readUstr(r)
  result.classId = readId(r)
  let n = r.u32()
  r.checkCount(uint64(n), 10)
  for _ in 0'u32 ..< n:
    let k = readId(r)
    result.items.add (k, readValue(r, depth + 1))

proc readDescriptor*(r: var Reader): Descriptor = readDescriptor(r, 0)

# ---------------------------------------------------------------- writing

proc write*(d: Descriptor; o: var seq[byte])

proc typeCode(v: Value): string =
  case v.kind
  of vkReference: "obj "
  of vkDescriptor: "Objc"
  of vkGlobalObject: "GlbO"
  of vkList: "VlLs"
  of vkDouble: "doub"
  of vkUnitFloat: "UntF"
  of vkUnitFloats: "UnFl"
  of vkText: "TEXT"
  of vkEnumerated: "enum"
  of vkInteger: "long"
  of vkLargeInteger: "comp"
  of vkBoolean: "bool"
  of vkClass: "type"
  of vkGlobalClass: "GlbC"
  of vkAlias: "alis"
  of vkPath: "Pth "
  of vkRawData: "tdta"
  of vkObjectArray: "ObAr"

proc write(it: ReferenceItem; o: var seq[byte]) =
  case it.kind
  of rkProperty:
    o.put("prop"); it.class.write(o); it.key.write(o)
  of rkClass:
    o.put("Clss"); it.class.write(o)
  of rkEnumerated:
    o.put("Enmr"); it.class.write(o); it.typeId.write(o); it.value.write(o)
  of rkOffset:
    o.put("rele"); it.class.write(o); o.putI32(it.offset)
  of rkIdentifier:
    o.put("Idnt"); o.putU32(it.num)
  of rkIndex:
    o.put("indx"); o.putU32(it.num)
  of rkName:
    o.put("name"); it.class.write(o); it.name.write(o)

proc write*(v: Value; o: var seq[byte]) =
  o.put(typeCode(v))
  case v.kind
  of vkReference:
    o.putU32(uint32(v.refs.len))
    for it in v.refs: it.write(o)
  of vkDescriptor, vkGlobalObject: v.desc.write(o)
  of vkList:
    o.putU32(uint32(v.list.len))
    for it in v.list: it.write(o)
  of vkDouble: o.putF64(v.d)
  of vkUnitFloat:
    o.put(v.unit); o.putF64(v.uv)
  of vkUnitFloats:
    o.put(v.unitN)
    o.putU32(uint32(v.values.len))
    for x in v.values: o.putF64(x)
  of vkText: v.text.write(o)
  of vkEnumerated:
    v.enumType.write(o); v.enumValue.write(o)
  of vkInteger: o.putI32(v.i)
  of vkLargeInteger: o.putI64(v.li)
  of vkBoolean: o.putU8(if v.b: 1 else: 0)
  of vkClass, vkGlobalClass: v.cls.write(o)
  of vkAlias, vkPath, vkRawData:
    o.putU32(uint32(v.raw.len)); o.put(v.raw)
  of vkObjectArray:
    o.putU32(v.prefix); v.body.write(o)

proc write*(d: Descriptor; o: var seq[byte]) =
  d.name.write(o)
  d.classId.write(o)
  o.putU32(uint32(d.items.len))
  for (k, v) in d.items:
    k.write(o)
    v.write(o)

proc toBytes*(d: Descriptor): seq[byte] = d.write(result)

proc descriptorFromBytes*(data: openArray[byte]): Descriptor =
  var r = newReader(data)
  result = readDescriptor(r, 0)
  if not r.isEmpty: raise invalid("trailing bytes after descriptor")

proc versioned*(d: Descriptor): VersionedDescriptor = VersionedDescriptor(version: 16, descriptor: d)

proc readVersioned*(r: var Reader): VersionedDescriptor =
  let v = r.u32()
  if v != 16: raise invalid("descriptor version " & $v & ", expected 16")
  VersionedDescriptor(version: v, descriptor: readDescriptor(r, 0))

proc write*(v: VersionedDescriptor; o: var seq[byte]) =
  o.putU32(v.version)
  v.descriptor.write(o)

proc toBytes*(v: VersionedDescriptor): seq[byte] = v.write(result)

proc versionedFromBytes*(data: openArray[byte]): VersionedDescriptor =
  var r = newReader(data)
  result = readVersioned(r)
  if not r.isEmpty: raise invalid("trailing bytes after versioned descriptor")

proc parseVersionedPrefix*(data: openArray[byte]): (VersionedDescriptor, int) =
  ## A versioned descriptor at the start of `data` and the bytes it used (trailing data allowed).
  var r = newReader(data)
  let v = readVersioned(r)
  (v, r.pos)

# ---------------------------------------------------------------- typed getters

proc getDouble*(d: Descriptor; key: string): Option[float64] =
  let p = d.getPtr(key)
  if p == nil: return
  case p.kind
  of vkDouble: some(p.d)
  of vkUnitFloat: some(p.uv)
  of vkInteger: some(float64(p.i))
  else: none(float64)

proc getInt*(d: Descriptor; key: string): Option[int] =
  let p = d.getPtr(key)
  if p == nil: return
  case p.kind
  of vkInteger: some(int(p.i))
  of vkLargeInteger: some(int(p.li))
  of vkDouble: some(int(p.d))
  else: none(int)

proc getBool*(d: Descriptor; key: string): Option[bool] =
  let p = d.getPtr(key)
  if p != nil and p.kind == vkBoolean: some(p.b) else: none(bool)

proc getText*(d: Descriptor; key: string): Option[string] =
  let p = d.getPtr(key)
  if p != nil and p.kind == vkText: some($p.text) else: none(string)

proc getEnum*(d: Descriptor; key: string): Option[string] =
  let p = d.getPtr(key)
  if p != nil and p.kind == vkEnumerated: some(p.enumValue.s) else: none(string)

proc getDesc*(d: Descriptor; key: string): Option[Descriptor] =
  let p = d.getPtr(key)
  if p != nil and p.kind in {vkDescriptor, vkGlobalObject}: some(p.desc) else: none(Descriptor)

proc getList*(d: Descriptor; key: string): Option[seq[Value]] =
  let p = d.getPtr(key)
  if p != nil and p.kind == vkList: some(p.list) else: none(seq[Value])
