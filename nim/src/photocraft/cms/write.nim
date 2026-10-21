## ICC v4.3 profile writer (ICC.1:2010), used for built-in and synthetic profiles, plus the
## byte-level identity helpers of `Profile` (bytes, content hash).

import ../rusthash
import std/[options, hashes, math]
import ./curve, ./clut, ./math as cmath, ./pipeline, ./profile

proc beU32(o: var seq[byte]; v: uint32) =
  o.add byte((v shr 24) and 0xFF); o.add byte((v shr 16) and 0xFF)
  o.add byte((v shr 8) and 0xFF); o.add byte(v and 0xFF)
proc beU16(o: var seq[byte]; v: int) =
  o.add byte((v shr 8) and 0xFF); o.add byte(v and 0xFF)
proc addStr(o: var seq[byte]; s: string) =
  for c in s: o.add byte(ord(c))

proc s15(o: var seq[byte]; v: float64) = o.beU32(cast[uint32](int32(round(v * 65536.0))))

proc xyzType(v: Vec3): seq[byte] =
  result.addStr "XYZ \0\0\0\0"
  for c in v: result.s15(c)

iterator runesOfUtf8(s: string): int =
  var i = 0
  while i < s.len:
    let c = ord(s[i])
    var cp, n: int
    if c < 0x80: (cp, n) = (c, 1)
    elif c < 0xE0: (cp, n) = (c and 0x1F, 2)
    elif c < 0xF0: (cp, n) = (c and 0x0F, 3)
    else: (cp, n) = (c and 0x07, 4)
    for k in 1 ..< n:
      if i + k < s.len: cp = (cp shl 6) or (ord(s[i + k]) and 0x3F)
    yield cp
    i += n

proc mluc(text: string): seq[byte] =
  var units: seq[int]
  for r in text.runesOfUtf8:
    if r >= 0x10000:
      let v = r - 0x10000
      units.add 0xD800 + (v shr 10)
      units.add 0xDC00 + (v and 0x3FF)
    else: units.add r
  result.addStr "mluc\0\0\0\0"
  result.beU32 1
  result.beU32 12
  result.addStr "enUS"
  result.beU32 uint32(units.len * 2)
  result.beU32 28
  for u in units: result.beU16 u

proc u16q(v: float32): int = int(clamp(v, 0.0'f32, 1.0'f32) * 65535.0'f32 + 0.5'f32)

proc curveElem(c: Curve): seq[byte] =
  case c.kind
  of ckIdentity:
    result.addStr "curv\0\0\0\0"
    result.beU32 0
  of ckGamma:
    result.addStr "para\0\0\0\0"
    result.add [0'u8, 0, 0, 0]
    result.s15(c.gamma)
  of ckParametric:
    result.addStr "para\0\0\0\0"
    result.beU16 c.pkind
    result.add [0'u8, 0]
    for i in 0 ..< paramCount(c.pkind): result.s15(c.p[i])
  of ckTable:
    result.addStr "curv\0\0\0\0"
    result.beU32 uint32(c.table.len)
    for v in c.table: result.beU16 u16q(v)

proc pad4(v: var seq[byte]) =
  while v.len mod 4 != 0: v.add 0

proc curveTable(c: Curve; n: int): seq[float32] =
  if c.kind == ckTable and c.table.len == n: c.table else: c.sample(n)

proc tableLen(cs: seq[Curve]): int =
  result = 2
  var any = false
  for c in cs:
    let l = case c.kind
      of ckTable: c.table.len
      of ckIdentity: 2
      else: 1024
    if not any or l > result: result = l
    any = true
  result = clamp(result, 2, 4096)

proc encodeMft(stages: seq[Stage]; wide: bool): Option[seq[byte]] =
  var i = 0
  var mat: seq[float64]
  if stages.len > 0 and stages[0].kind == skMatrix and stages[0].rows == 3 and stages[0].cols == 3:
    mat = stages[0].m
    i = 1
  if stages.len - i != 3 or stages[i].kind != skCurves or stages[i + 1].kind != skClut or stages[i + 2].kind != skCurves:
    return none(seq[byte])
  let a = stages[i].curves
  let clut = stages[i + 1].clut
  let b = stages[i + 2].curves
  if clut.grid.len == 0: return none(seq[byte])
  let g = clut.grid[0]
  for x in clut.grid:
    if x != g: return none(seq[byte])
  var o: seq[byte]
  o.addStr(if wide: "mft2\0\0\0\0" else: "mft1\0\0\0\0")
  o.add [byte(clut.inputs), byte(clut.outputs), byte(g), 0]
  if mat.len == 0: mat = @[1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0]
  for v in mat: o.s15(v)
  let (nIn, nOut) = if wide: (tableLen(a), tableLen(b)) else: (256, 256)
  if wide:
    o.beU16 nIn
    o.beU16 nOut
  proc put(o: var seq[byte]; v: float32) =
    if wide: o.beU16 u16q(v)
    else: o.add byte(clamp(v, 0.0'f32, 1.0'f32) * 255.0'f32 + 0.5'f32)
  for c in a:
    for v in curveTable(c, nIn): o.put v
  for v in clut.data: o.put v
  for c in b:
    for v in curveTable(c, nOut): o.put v
  some(o)

proc encodeAb(stages: seq[Stage]; a2b: bool; inputs, outputs: int): Option[seq[byte]] =
  var aCurves, mCurves, bCurves: Option[seq[Curve]]
  var clut: Option[Clut]
  var matrix: Option[(seq[float64], seq[float64])]
  if a2b:
    var k = 0
    if stages.len >= 2 and stages[0].kind == skCurves and stages[1].kind == skClut:
      aCurves = some(stages[0].curves)
      clut = some(stages[1].clut)
      k = 2
    let rest = stages[k .. ^1]
    if rest.len == 3 and rest[0].kind == skCurves and rest[1].kind == skMatrix and rest[2].kind == skCurves:
      mCurves = some(rest[0].curves)
      matrix = some((rest[1].m, rest[1].offset))
      bCurves = some(rest[2].curves)
    elif rest.len == 1 and rest[0].kind == skCurves:
      bCurves = some(rest[0].curves)
    else: return none(seq[byte])
  else:
    var k = 0
    if stages.len > 0 and stages[0].kind == skCurves:
      bCurves = some(stages[0].curves)
      k = 1
    if k + 1 < stages.len and stages[k].kind == skMatrix and stages[k + 1].kind == skCurves:
      matrix = some((stages[k].m, stages[k].offset))
      mCurves = some(stages[k + 1].curves)
      k += 2
    if k + 1 < stages.len and stages[k].kind == skClut and stages[k + 1].kind == skCurves:
      clut = some(stages[k].clut)
      aCurves = some(stages[k + 1].curves)
      k += 2
    if k != stages.len: return none(seq[byte])
  var o: seq[byte]
  o.addStr(if a2b: "mAB \0\0\0\0" else: "mBA \0\0\0\0")
  o.add [byte(inputs), byte(outputs), 0, 0]
  for _ in 0 ..< 20: o.add 0
  var offs: array[5, uint32]
  proc curves(o: var seq[byte]; cs: seq[Curve]) =
    for c in cs:
      o.add curveElem(c)
      o.pad4
  if bCurves.isNone: return none(seq[byte])
  offs[0] = uint32(o.len)
  o.curves(bCurves.get)
  if matrix.isSome:
    offs[1] = uint32(o.len)
    for v in matrix.get[0]: o.s15(v)
    for v in matrix.get[1]: o.s15(v)
    offs[2] = uint32(o.len)
    if mCurves.isNone: return none(seq[byte])
    o.curves(mCurves.get)
  if clut.isSome:
    let c = clut.get
    offs[3] = uint32(o.len)
    var g: array[16, byte]
    for k, n in c.grid: g[k] = byte(n)
    o.add g
    o.add [2'u8, 0, 0, 0]
    for v in c.data: o.beU16 u16q(v)
    o.pad4
    offs[4] = uint32(o.len)
    if aCurves.isNone: return none(seq[byte])
    o.curves(aCurves.get)
  for k, v in offs:
    o[12 + k * 4] = byte((v shr 24) and 0xFF)
    o[13 + k * 4] = byte((v shr 16) and 0xFF)
    o[14 + k * 4] = byte((v shr 8) and 0xFF)
    o[15 + k * 4] = byte(v and 0xFF)
  some(o)

proc encode*(p: Profile): seq[byte] =
  ## Encodes `p` as ICC v4.3 bytes (identical tag data is shared between tags).
  var tags: seq[(string, seq[byte])]
  tags.add ("desc", mluc(p.description))
  tags.add ("cprt", mluc(p.copyright))
  tags.add ("wtpt", xyzType(p.whitePoint))
  if p.blackPoint.isSome: tags.add ("bkpt", xyzType(p.blackPoint.get))
  if p.chad.isSome:
    var o: seq[byte]
    o.addStr "sf32\0\0\0\0"
    for row in p.chad.get:
      for v in row: o.s15(v)
    tags.add ("chad", o)
  if p.matrix.isSome:
    let m = p.matrix.get
    tags.add ("rXYZ", xyzType([m[0][0], m[1][0], m[2][0]]))
    tags.add ("gXYZ", xyzType([m[0][1], m[1][1], m[2][1]]))
    tags.add ("bXYZ", xyzType([m[0][2], m[1][2], m[2][2]]))
  if p.trc.isSome:
    let t = p.trc.get
    tags.add ("rTRC", curveElem(t[0]))
    tags.add ("gTRC", curveElem(t[1]))
    tags.add ("bTRC", curveElem(t[2]))
  if p.grayTrc.isSome: tags.add ("kTRC", curveElem(p.grayTrc.get))
  for (luts, names, a2b) in [(p.a2b, ["A2B0", "A2B1", "A2B2"], true), (p.b2a, ["B2A0", "B2A1", "B2A2"], false)]:
    for k in 0..2:
      if luts[k].isSome:
        let l = luts[k].get
        let data = case l.kind
          of lkLut16: encodeMft(l.stages, true)
          of lkLut8: encodeMft(l.stages, false)
          of lkAb: encodeAb(l.stages, a2b, l.inputs, l.outputs)
        if data.isSome: tags.add (names[k], data.get)
  let tableLen = 4 + tags.len * 12
  var data: seq[byte]
  var entries: seq[(string, uint32, uint32)]
  var placed: seq[(int, uint32, uint32)]
  for i, (sig, d) in tags:
    var found = false
    for (j, off, len) in placed:
      if tags[j][1] == d:
        entries.add (sig, off, len)
        found = true
        break
    if found: continue
    let off = uint32(128 + tableLen + data.len)
    data.add d
    data.pad4
    entries.add (sig, off, uint32(d.len))
    placed.add (i, off, uint32(d.len))
  let size = 128 + tableLen + data.len
  var o = newSeqOfCap[byte](size)
  o.beU32 uint32(size)
  o.add [0'u8, 0, 0, 0]
  o.add [4'u8, 0x30, 0, 0]
  o.beU32 p.class.sig
  o.beU32 p.colorSpace.sig
  o.addStr(if p.pcs == pcsXyz: "XYZ " else: "Lab ")
  for v in [2026, 1, 1, 0, 0, 0]: o.beU16 v
  o.addStr "acsp"
  for _ in 0 ..< 4 * 4 + 8: o.add 0
  o.beU32 uint32(ord(p.renderingIntent))
  for v in D50: o.s15(v)
  for _ in 0 ..< 20: o.add 0
  o.setLen(128)
  o.beU32 uint32(entries.len)
  for (sig, off, len) in entries:
    o.addStr sig
    o.beU32 off
    o.beU32 len
  o.add data
  o

proc toBytes*(p: Profile): seq[byte] =
  ## The ICC bytes of this profile: the original bytes when parsed, otherwise a v4 encoding.
  if p.bytes.isSome: p.bytes.get else: encode(p)

proc withEncodedBytes*(p: Profile): Profile =
  ## Re-encodes the model as ICC v4 bytes and caches them (used for built-in profiles).
  p.bytes = none(seq[byte])
  p.hashCache = none(uint64)
  p.bytes = some(encode(p))
  p

proc contentHash*(p: Profile): uint64 =
  ## Hash of the profile's ICC bytes (stable within a process; used as a cache key).
  if p.hashCache.isNone:
    p.hashCache = some(rustHashBytes(p.toBytes))
  p.hashCache.get
