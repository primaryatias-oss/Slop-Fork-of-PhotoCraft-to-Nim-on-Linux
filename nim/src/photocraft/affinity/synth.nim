## Synthetic containers for tests and fuzz seeds (port of `affinity/src/synth.rs`).
##
## These builders write the layout the reader accepts, so tests can describe documents in a few
## lines. They are **not** an Affinity exporter: nothing written here has been opened in Affinity.
## Compressed entries use PhotoCraft's own zlib and zstd encoders, so their bytes differ from the
## Rust builders' (flate2 and ruzstd); stored entries and every table are byte-identical.

import ./stream, ./container
import ../zlib, ../zstd

type
  FKind* = enum
    fkU8, fkU32, fkI32, fkF32, fkF64, fkBool, fkEnum, fkStr, fkF64s, fkStruct, fkRecords, fkU8s,
    fkEntry, fkDef, fkRef, fkNull, fkObj, fkPos, fkShared, fkObjs, fkRaw

  F* = object
    ## A field value to encode.
    case kind*: FKind
    of fkU8: u8v: uint8
    of fkU32: u32v: uint32
    of fkI32: i32v: int32
    of fkF32: f32v: float32
    of fkF64: f64v: float64
    of fkBool: boolv: bool
    of fkEnum: enumId, enumVer: uint16
    of fkStr, fkEntry: str: string
    of fkF64s: f64s: seq[float64] ## 2..6 values (points, rectangles, transforms)
    of fkStruct, fkU8s, fkRaw: bytes: seq[byte] ## struct of 1..64 bytes; u8 array; raw bytes
    of fkRecords:
      recSize: uint16
      records: seq[seq[byte]] ## array of fixed-size records, all `recSize` bytes
    of fkDef:
      defId: uint32
      chain: seq[Tag]
      defFields: seq[(Tag, F)] ## shared object: defines `defId` with a class chain and fields
    of fkRef: refId: uint32 ## link back to a shared object
    of fkNull: discard
    of fkObj:
      class: Tag
      objFields: seq[(Tag, F)] ## inline object
    of fkPos: items: seq[F] ## positional (untagged) object
    of fkShared: shared: seq[F] ## array of shared objects / links
    of fkObjs:
      objsClass: Tag
      objs: seq[seq[(Tag, F)]] ## array of inline objects of one class

  Method* = enum
    ## How an entry is stored.
    mStored, mZlib, mZstd

proc tag*(s: string): Tag = tagOf(s)

proc FU8*(v: uint8): F = F(kind: fkU8, u8v: v)
proc FU32*(v: uint32): F = F(kind: fkU32, u32v: v)
proc FI32*(v: int32): F = F(kind: fkI32, i32v: v)
proc FF32*(v: float32): F = F(kind: fkF32, f32v: v)
proc FF64*(v: float64): F = F(kind: fkF64, f64v: v)
proc FBool*(v: bool): F = F(kind: fkBool, boolv: v)
proc FEnum*(id, ver: uint16): F = F(kind: fkEnum, enumId: id, enumVer: ver)
proc FStr*(s: string): F = F(kind: fkStr, str: s)
proc FF64s*(v: openArray[float64]): F = F(kind: fkF64s, f64s: @v)
proc FStruct*(b: openArray[byte]): F = F(kind: fkStruct, bytes: @b)
proc FRecords*(size: uint16; recs: seq[seq[byte]]): F = F(kind: fkRecords, recSize: size, records: recs)
proc FU8s*(b: openArray[byte]): F = F(kind: fkU8s, bytes: @b)
proc FEntry*(name: string): F = F(kind: fkEntry, str: name)
proc FDef*(id: uint32; chain: seq[Tag]; fields: seq[(Tag, F)]): F = F(kind: fkDef, defId: id, chain: chain, defFields: fields)
proc FRef*(id: uint32): F = F(kind: fkRef, refId: id)
proc FNull*(): F = F(kind: fkNull)
proc FObj*(class: Tag; fields: seq[(Tag, F)]): F = F(kind: fkObj, class: class, objFields: fields)
proc FPos*(items: seq[F]): F = F(kind: fkPos, items: items)
proc FShared*(items: seq[F]): F = F(kind: fkShared, shared: items)
proc FObjs*(class: Tag; items: seq[seq[(Tag, F)]]): F = F(kind: fkObjs, objsClass: class, objs: items)
proc FRaw*(b: openArray[byte]): F = F(kind: fkRaw, bytes: @b)

proc le(o: var seq[byte]; v: uint64; n: int) =
  for k in 0 ..< n: o.add byte((v shr (8 * k)) and 0xFF)

proc putTag(o: var seq[byte]; t: Tag) = le(o, uint64(uint32(t)), 4)

proc code(f: F): uint8 =
  case f.kind
  of fkU8: 0x01
  of fkU32: 0x03
  of fkI32: 0x07
  of fkF32: 0x09
  of fkF64: 0x0a
  of fkBool: 0x29
  of fkEnum: 0x2a
  of fkStr: 0x2b
  of fkF64s: uint8(0x22 + f.f64s.len)
  of fkStruct: uint8(0x34 + f.bytes.len)
  of fkRecords: 0x80 or 0x2c
  of fkU8s: 0x80 or 0x01
  of fkEntry: 0x33
  of fkDef, fkRef, fkNull: 0x31
  of fkObj: 0x32
  of fkPos: 0x30
  of fkShared: 0x80 or 0x31
  of fkObjs: 0x80 or 0x32
  of fkRaw: 0

proc fieldsInto(o: var seq[byte]; fields: openArray[(Tag, F)]; tagged: bool)

proc payload(o: var seq[byte]; f: F) =
  case f.kind
  of fkU8: o.add f.u8v
  of fkU32: le(o, f.u32v, 4)
  of fkI32: le(o, uint64(cast[uint32](f.i32v)), 4)
  of fkF32: le(o, cast[uint32](f.f32v), 4)
  of fkF64: le(o, cast[uint64](f.f64v), 8)
  of fkBool: o.add(if f.boolv: 1'u8 else: 0'u8)
  of fkEnum:
    le(o, f.enumId, 2)
    le(o, f.enumVer, 2)
  of fkStr:
    le(o, uint64(f.str.len), 4)
    for c in f.str: o.add byte(c)
  of fkF64s:
    for x in f.f64s: le(o, cast[uint64](x), 8)
  of fkStruct, fkRaw: o.add f.bytes
  of fkRecords:
    le(o, uint64(f.records.len), 4)
    le(o, f.recSize, 2)
    for r in f.records: o.add r
  of fkU8s:
    le(o, uint64(f.bytes.len), 4)
    o.add f.bytes
  of fkEntry:
    putTag(o, tag("Data"))
    le(o, uint64(f.str.len), 4)
    for c in f.str: o.add byte(c)
  of fkDef:
    o.add 1
    le(o, f.defId, 4)
    # Class chain: each level but the last carries no fields here; the last ends the chain.
    for i in 0 ..< f.chain.len - 1:
      o.add 0
      putTag(o, f.chain[i])
      le(o, 0, 2)
      o.add 0
    o.add 1
    putTag(o, f.chain[^1])
    fieldsInto(o, f.defFields, true)
  of fkRef:
    o.add 2
    le(o, f.refId, 4)
  of fkNull: o.add 0
  of fkObj:
    o.add 1
    putTag(o, f.class)
    le(o, 0, 2)
    fieldsInto(o, f.objFields, true)
  of fkPos:
    for it in f.items:
      o.add code(it)
      payload(o, it)
    o.add 0
  of fkShared:
    le(o, uint64(f.shared.len), 4)
    for it in f.shared: payload(o, it)
  of fkObjs:
    le(o, uint64(f.objs.len), 4)
    putTag(o, f.objsClass)
    le(o, 0, 2)
    for fields in f.objs:
      o.add 1
      fieldsInto(o, fields, true)

proc fieldsInto(o: var seq[byte]; fields: openArray[(Tag, F)]; tagged: bool) =
  for (t, f) in fields:
    if f.kind == fkRaw:
      o.add f.bytes
      continue
    o.add code(f)
    if tagged: putTag(o, t)
    payload(o, f)
  o.add 0

proc stream*(root: openArray[(Tag, F)]): seq[byte] =
  ## A `doc.dat` stream with root class `Pers`.
  result = @[0x00'u8, 0xFF, byte('K'), byte('S')]
  le(result, 2, 2)
  putTag(result, tag("Pers"))
  le(result, 0, 2)
  le(result, 30, 4)
  fieldsInto(result, root, true)

proc container*(entries: openArray[(string, seq[byte], Method)]; thumbnailPng: seq[byte] = @[];
                withThumbnail = false): seq[byte] =
  ## A version-12 container with one `#FT4` table holding `entries`, then an optional thumbnail
  ## record (`withThumbnail`).
  result = newSeq[byte](72)
  for i in 0 ..< 4: result[i] = byte(Magic[i])
  result[4] = 12
  result[6] = 4
  for i, c in "nsrP": result[8 + i] = byte(c)
  for i, c in "#Inf": result[12 + i] = byte(c)
  for i, c in "Prot": result[64 + i] = byte(c)
  var records: seq[seq[byte]]
  for i, (name, data, meth) in entries:
    let packed = case meth
      of mStored: data
      of mZlib: zlibCompress(data, 1)
      of mZstd: zstdCompress(data)
    let offset = uint64(result.len)
    for c in "#Fil": result.add byte(c)
    result.add packed
    var r: seq[byte]
    le(r, uint64(i + 1), 4)
    r.add 0
    le(r, offset, 8)
    le(r, uint64(data.len), 8)
    le(r, uint64(packed.len), 8)
    le(r, crc32(data), 4)
    r.add(case meth
      of mStored: 0'u8
      of mZlib: 1'u8
      of mZstd: 2'u8)
    le(r, 0, 4)
    le(r, 0, 4)
    le(r, uint64(name.len), 2)
    for c in name: r.add byte(c)
    records.add r
  let fat = uint64(result.len)
  for c in "#FT4": result.add byte(c)
  le(result, 0, 8)
  le(result, 1_700_000_000'u64, 8)
  for _ in 0 ..< 24: result.add 0
  le(result, uint64(records.len), 4)
  for _ in 0 ..< 8: result.add 0
  le(result, 0, 2)
  result.add 0
  for r in records: result.add r
  let thumb = uint64(result.len)
  for k in 0 ..< 8: result[16 + k] = byte((fat shr (8 * k)) and 0xFF)
  if withThumbnail:
    for k in 0 ..< 8: result[24 + k] = byte((thumb shr (8 * k)) and 0xFF)
    for c in "\xff\xff\xff\xffThmb": result.add byte(c)
    le(result, 1, 4)
    le(result, uint64(thumbnailPng.len + 13), 4)
    le(result, 29, 4)
    le(result, 0, 4)
    le(result, uint64(thumbnailPng.len), 4)
    result.add 1
    result.add thumbnailPng
