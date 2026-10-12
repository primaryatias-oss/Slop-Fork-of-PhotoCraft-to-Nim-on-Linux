## Smart-filter caches: the global `FEid`/`FXid` ("Filter Effects") block. One section per smart
## object (padded to 4), each item holding the unfiltered placed pixels in slots and the filter
## mask after them. RLE row counts are four bytes wide in every file version.

import std/options
import ./io, ./compression, ./model

const FxMaxSlots* = 64'u32

type
  EffectsPlane* = object
    compression*: Compression
    data*: seq[byte]
  FilterEffectsItem* = object
    id*: string
    version*: uint32
    rect*: PsdRect
    depth*, maxChannels*: uint32
    slots*: seq[Option[EffectsPlane]]
    mask*: Option[(PsdRect, EffectsPlane)]
  FilterEffects* = object
    version*: uint32
    items*: seq[FilterEffectsItem]

proc encodeEffectsPlane*(c: Compression; decoded: openArray[byte]; w, h, depth: int): EffectsPlane =
  EffectsPlane(compression: c, data: encodePlanes(c, decoded, PlaneLayout(planes: 1, width: w, height: h, depth: depth, version: vPsb)))

proc decode*(p: EffectsPlane; w, h, depth: int): seq[byte] =
  let l = PlaneLayout(planes: 1, width: w, height: h, depth: depth, version: vPsb)
  let n = uint64(p.data.len)
  let most = case p.compression.code
    of 0: n
    of 1: n * 64
    else: n * 1032 + 64
  if l.totalBytes > most: raise invalid("filter effects: plane larger than its data can hold")
  decodePlanes(p.compression, p.data, l)

proc readRectFx(r: var Reader): PsdRect =
  PsdRect(top: r.i32(), left: r.i32(), bottom: r.i32(), right: r.i32())

proc writeRectFx(o: var seq[byte]; r: PsdRect) =
  o.putI32(r.top); o.putI32(r.left); o.putI32(r.bottom); o.putI32(r.right)

proc readPlane(r: var Reader): EffectsPlane =
  let ln = r.u64()
  if ln < 2: raise invalid("filter effects: plane shorter than its compression field")
  var s = r.sub(ln)
  result.compression = Compression(code: s.u16())
  result.data = s.peekRest()

proc decodedMask*(it: FilterEffectsItem): Option[(PsdRect, seq[byte])] =
  if it.mask.isNone: return
  try:
    let (r, p) = it.mask.get
    let (w, h) = r.size
    some((r, p.decode(w, h, int(it.depth))))
  except PsdError: none((PsdRect, seq[byte]))

proc readItem(body: var Reader): FilterEffectsItem =
  result.id = decodeLegacyName(readPascal(body, 1))
  result.version = body.u32()
  let itemLen = body.u64()
  var it = body.sub(itemLen)
  result.rect = readRectFx(it)
  result.depth = it.u32()
  result.maxChannels = it.u32()
  if result.maxChannels > FxMaxSlots: raise limitExceeded("filter effects: too many channels")
  for _ in 0'u32 ..< result.maxChannels + 2:
    result.slots.add(if it.u32() != 0: some(readPlane(it)) else: none(EffectsPlane))
  if not it.isEmpty: raise invalid("filter effects item " & result.id & ": " & $it.remaining & " unexpected bytes")
  if not body.isEmpty and body.u8() != 0:
    let r = readRectFx(body)
    result.mask = some((r, readPlane(body)))

proc parseFilterEffects*(data: openArray[byte]): FilterEffects =
  var r = newReader(data)
  result.version = r.u32()
  if result.version < 1 or result.version > 3: raise invalid("filter effects version " & $result.version)
  while true:
    let ln = r.u64()
    var body = r.sub(ln)
    while not body.isEmpty: result.items.add readItem(body)
    r.skip(int(min((4 - ln mod 4) mod 4, uint64(r.remaining))))
    var allZero = true
    for i in r.pos ..< r.size:
      if r.at(i) != 0: allZero = false
    if allZero and r.remaining <= 3: break
    if r.remaining < 8: raise invalid("filter effects: " & $r.remaining & " unexpected bytes after the data")

proc writePlane(o: var seq[byte]; p: EffectsPlane) =
  o.putU64(uint64(p.data.len + 2))
  o.putU16(p.compression.code)
  o.put(p.data)

proc toBytes*(fx: FilterEffects): seq[byte] =
  result.putU32(fx.version)
  for it in fx.items:
    var body: seq[byte]
    writePascal(body, it.id.toOpenArrayByte(0, it.id.len - 1), 1)
    body.putU32(it.version)
    let at = body.beginLen(true)
    writeRectFx(body, it.rect)
    body.putU32(it.depth)
    body.putU32(it.maxChannels)
    for i in 0 ..< int(it.maxChannels) + 2:
      if i < it.slots.len and it.slots[i].isSome:
        body.putU32(1)
        writePlane(body, it.slots[i].get)
      else: body.putU32(0)
    body.endLen(at, true)
    if it.mask.isSome:
      body.putU8(1)
      writeRectFx(body, it.mask.get[0])
      writePlane(body, it.mask.get[1])
    else: body.putU8(0)
    result.putU64(uint64(body.len))
    result.put(body)
    while result.len mod 4 != 0: result.add 0
  if fx.items.len == 0: result.putU64(0)

# ---------------------------------------------------------------- strict block checks

import ./descriptor

proc checkPlacedDescriptor(d: Descriptor) =
  let idnt = d.getPtr("Idnt")
  if idnt == nil or idnt.kind != vkText: raise invalid("no Idnt (unique id)")
  let trnf = d.getPtr("Trnf")
  var ok = trnf != nil and trnf.kind == vkList and trnf.list.len == 8
  if ok:
    for v in trnf.list:
      if v.kind != vkDouble: ok = false
  if not ok: raise invalid("Trnf is not eight doubles")
  let fx = d.getPtr("filterFX")
  if fx == nil: return
  if fx.kind != vkDescriptor: raise invalid("filterFX is not an object")
  let list = fx.desc.getPtr("filterFXList")
  if list == nil or list.kind != vkList: raise invalid("filterFX has no filterFXList")
  for i, item in list.list:
    if item.kind != vkDescriptor: raise invalid("filter " & $i & " is not an object")
    let f = item.desc
    template isK(k: string; kinds: set[ValueKind]): bool = (let p = f.getPtr(k); p != nil and p.kind in kinds)
    let fltr = f.getPtr("Fltr")
    if not (isK("Nm  ", {vkText}) and isK("blendOptions", {vkDescriptor}) and isK("enab", {vkBoolean}) and
            isK("filterID", {vkInteger}) and (fltr == nil or fltr.kind == vkDescriptor)):
      raise invalid("filter " & $i & " lacks Nm/blendOptions/enab/filterID")

proc checkEmbeddedItem(item: openArray[byte]) =
  var r = newReader(item)
  r.skip(4)
  let version = r.u32()
  if version < 1 or version > 16: raise invalid("version " & $version)
  discard readPascal(r, 1)
  discard readUnicodeUnits(r)
  r.skip(8)
  let ln = r.u64()
  if r.u8() != 0: discard readVersioned(r)
  discard r.bytesU64(ln)

proc expectKind(r: var Reader; kind: string) =
  let found = r.fourcc()
  if found != kind: raise invalid("expected " & kind & " structure, found " & found)

proc checkStructure*(b: TaggedBlock) =
  ## Strictly re-parses blocks whose data carries its own sizes (`PlLd`, `SoLd`/`SoLE`,
  ## `lnk2`/`lnk3`/`lnkD`, `lfx2`, `FEid`/`FXid`); other keys are not checked.
  var r = newReader(b.data)
  case b.key
  of "PlLd":
    expectKind(r, "plcL")
    discard r.u32()
    discard readPascal(r, 1)
    r.skip(4 * 4 + 8 * 8)
    discard r.u32()
    discard readVersioned(r)
  of "SoLd", "SoLE":
    expectKind(r, "soLD")
    discard r.u32()
    let d = readVersioned(r)
    try: checkPlacedDescriptor(d.descriptor)
    except PsdError as e: raise invalid(b.key & ": " & e.msg)
  of "lfx2":
    discard r.u32()
    discard readVersioned(r)
  of "lnk2", "lnk3", "lnkD":
    while r.remaining >= 8:
      let ln = r.u64()
      let item = r.bytesU64(ln)
      let kind = if item.len >= 4: toFourCC(item) else: ""
      if kind notin ["liFD", "liFE", "liFA"]: raise invalid(b.key & ": linked item of unknown type")
      if kind == "liFD":
        try: checkEmbeddedItem(item)
        except PsdError as e: raise invalid(b.key & ": embedded file: " & e.msg)
      r.skip(int(min((4 - ln mod 4) mod 4, uint64(r.remaining))))
  of "FEid", "FXid":
    try: discard parseFilterEffects(b.data)
    except PsdError as e: raise invalid(b.key & ": " & e.msg)
    return
  else: return
  let rest = r.peekRest()
  var bad = rest.len > 3
  for x in rest:
    if x != 0: bad = true
  if bad: raise invalid(b.key & ": " & $rest.len & " unexpected bytes after the block structure")
