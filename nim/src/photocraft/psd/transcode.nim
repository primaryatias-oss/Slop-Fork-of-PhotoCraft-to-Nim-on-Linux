## Field-by-field byte-order conversion of Photoshop structures (Rust `psd::tiff::transcode`).
##
## Every function reads one structure in the source order and writes it in the target order, so
## output lengths equal input lengths and every length field stays valid. Layouts follow the
## Adobe PSD specification; the walker only needs to know where the integers, doubles and
## four-character keys are, not what they mean. Blocks of unknown layout are kept verbatim
## (marked `MIB8`/`46B8`) on the way in and dropped, with a warning, on the way out.

import std/options
import io, model, descriptor, filter_effects

type
  ByteOrder* = enum boBig, boLittle

  Tx = ref object
    data: seq[byte]
    r: Reader
    o: seq[byte]
    src, dst: ByteOrder
    version: Version
    warnings: seq[string]
    depth: int

  Body = proc(t: Tx)

proc isLittle*(o: ByteOrder): bool = o == boLittle

proc newTx(data: openArray[byte]; version: Version; src, dst: ByteOrder): Tx =
  result = Tx(data: @data, src: src, dst: dst, version: version)
  result.r = newReader(result.data, src.isLittle)

proc remaining(t: Tx): int = t.r.remaining
proc sameOrder(t: Tx): bool = t.src == t.dst

proc restStartsWith(t: Tx; at: int; s: string): bool =
  let p = t.r.pos + at
  if p + s.len > t.data.len: return false
  for i, c in s:
    if t.data[p + i] != byte(c): return false
  true

proc isSigAt(t: Tx; at: int; little: bool): bool =
  if little: t.restStartsWith(at, "MIB8") or t.restStartsWith(at, "46B8")
  else:
    t.restStartsWith(at, "8BIM") or t.restStartsWith(at, "8B64") or t.restStartsWith(at, "MIB8") or
      t.restStartsWith(at, "46B8")

proc putN(t: Tx; v: uint64; n: int) =
  if t.dst.isLittle:
    for i in 0 ..< n: t.o.add byte((v shr (8 * i)) and 0xff)
  else:
    for i in countdown(n - 1, 0): t.o.add byte((v shr (8 * i)) and 0xff)

proc u8(t: Tx): uint8 =
  result = t.r.u8()
  t.o.add result
proc u16(t: Tx): uint16 =
  result = t.r.u16()
  t.putN(uint64(result), 2)
proc i16(t: Tx): int16 = cast[int16](t.u16())
proc u32(t: Tx): uint32 =
  result = t.r.u32()
  t.putN(uint64(result), 4)
proc i32(t: Tx): int32 = cast[int32](t.u32())
proc u64(t: Tx): uint64 =
  result = t.r.u64()
  t.putN(result, 8)
proc f64(t: Tx) = discard t.u64()
proc f32(t: Tx) = discard t.u32()

proc raw(t: Tx; n: int) = t.o.add t.r.bytes(n)
proc rest(t: Tx) = t.raw(t.remaining)

proc key(t: Tx): string =
  ## A four-character key: stored reversed in little-endian data. Returns the canonical spelling.
  var k = t.r.bytes(4)
  if t.src.isLittle: k = @[k[3], k[2], k[1], k[0]]
  result = newString(4)
  for i in 0 ..< 4: result[i] = char(k[i])
  if t.dst.isLittle: t.o.add [k[3], k[2], k[1], k[0]]
  else: t.o.add k

proc lenOf(t: Tx; long: bool): uint64 = (if long: t.u64() else: uint64(t.u32()))

proc u16s(t: Tx; n: int) =
  for _ in 0 ..< n: discard t.u16()

proc u16sUpto(t: Tx; n: int) =
  for _ in 0 ..< n:
    if t.remaining < 2: break
    discard t.u16()

proc i32s(t: Tx; n: int) =
  for _ in 0 ..< n: discard t.i32()

proc f64s(t: Tx; n: int) =
  for _ in 0 ..< n: t.f64()

proc u16sToEnd(t: Tx) =
  while t.remaining >= 2: discard t.u16()
  t.rest()

proc u32sToEnd(t: Tx) =
  while t.remaining >= 4: discard t.u32()
  t.rest()

proc unicode(t: Tx) =
  let n = t.u32()
  t.r.checkCount(uint64(n), 2)
  t.u16s(int(n))

proc pascal(t: Tx; align: int) =
  let n = int(t.u8())
  t.raw(n)
  let total = 1 + n
  t.raw((align - total mod align) mod align)

proc child(t: Tx; data: seq[byte]): Tx =
  result = newTx(data, t.version, t.src, t.dst)
  result.depth = t.depth

proc region(t: Tx; len: uint64; f: Body) =
  let data = t.r.bytesU64(len)
  let c = t.child(data)
  f(c)
  c.rest()
  if c.o.len != data.len: raise invalid("byte-order conversion changed a structure's length")
  t.warnings.add c.warnings
  t.o.add c.o

proc patchLen(t: Tx; at: int; long: bool; n: uint64) =
  if long:
    if at + 8 <= t.o.len:
      for i in 0 ..< 8:
        t.o[at + i] = (if t.dst.isLittle: byte((n shr (8 * i)) and 0xff) else: byte((n shr (8 * (7 - i))) and 0xff))
  else:
    if n > uint64(high(uint32)): raise limitExceeded("structure too large for a 32-bit length")
    if at + 4 <= t.o.len:
      for i in 0 ..< 4:
        t.o[at + i] = (if t.dst.isLittle: byte((n shr (8 * i)) and 0xff) else: byte((n shr (8 * (3 - i))) and 0xff))

proc sizedRegionCounting(t: Tx; long, inclusive: bool; f: Body) =
  var len = t.r.lenField(long)
  let at = t.o.len
  for _ in 0 ..< (if long: 8 else: 4): t.o.add 0
  if inclusive:
    if len < 4: raise invalid("length shorter than its own field")
    len -= 4
  let data = t.r.bytesU64(len)
  let c = t.child(data)
  f(c)
  c.rest()
  t.warnings.add c.warnings
  t.patchLen(at, long, uint64(c.o.len) + (if inclusive: 4'u64 else: 0'u64))
  t.o.add c.o

proc sizedRegion(t: Tx; long: bool; f: Body) = t.sizedRegionCounting(long, false, f)
proc sizedRegionInclusive(t: Tx; f: Body) = t.sizedRegionCounting(false, true, f)

proc attempt(t: Tx; f: Body): bool =
  ## Tries `f` on the rest; a match leaves at most three trailing zero bytes. On a miss nothing
  ## is consumed or written.
  let (reader, written, warnCount) = (t.r, t.o.len, t.warnings.len)
  var ok = true
  try: f(t)
  except CatchableError: ok = false
  if ok:
    if t.remaining > 3: ok = false
    else:
      for i in 0 ..< t.remaining:
        if t.data[t.r.pos + i] != 0: ok = false
  if not ok:
    t.r = reader
    t.o.setLen(written)
    t.warnings.setLen(warnCount)
  ok

proc rleCounts(t: Tx; rows: int) =
  for _ in 0 ..< rows:
    if t.version.isPsb:
      if t.remaining < 4: break
      discard t.u32()
    else:
      if t.remaining < 2: break
      discard t.u16()

proc channelData(t: Tx; rows: int; longCounts: bool) =
  let saved = t.version
  if longCounts: t.version = vPsb
  try:
    let compression = t.u16()
    if compression == 1: t.rleCounts(rows)
  finally:
    t.version = saved
  t.rest()

proc height(top, bottom: int32): int = max(int(bottom) - int(top), 0)

# ---------------------------------------------------------------- descriptors

proc descriptorBody(t: Tx)

proc id(t: Tx) =
  let n = t.u32()
  if n == 0: discard t.key()
  else: t.raw(int(n))

proc class(t: Tx) =
  t.unicode()
  t.id()

proc referenceItem(t: Tx) =
  let ty = t.key()
  case ty
  of "prop":
    t.class(); t.id()
  of "Clss": t.class()
  of "Enmr":
    t.class(); t.id(); t.id()
  of "rele":
    t.class(); discard t.i32()
  of "Idnt", "indx": discard t.u32()
  of "name":
    t.class(); t.unicode()
  else: raise unsupported("reference item type " & ty)

proc value(t: Tx) =
  if t.depth > MaxDepth: raise limitExceeded("descriptor nesting too deep")
  let ty = t.key()
  case ty
  of "obj ":
    let n = t.u32()
    t.r.checkCount(uint64(n), 8)
    for _ in 0 ..< int(n): t.referenceItem()
  of "Objc", "GlbO": t.descriptorBody()
  of "VlLs":
    let n = t.u32()
    t.r.checkCount(uint64(n), 4)
    inc t.depth
    for _ in 0 ..< int(n): t.value()
    dec t.depth
  of "doub": t.f64()
  of "UntF":
    discard t.key(); t.f64()
  of "UnFl":
    discard t.key()
    let n = t.u32()
    t.r.checkCount(uint64(n), 8)
    t.f64s(int(n))
  of "TEXT": t.unicode()
  of "enum":
    t.id(); t.id()
  of "long": discard t.i32()
  of "comp": discard t.u64()
  of "bool": discard t.u8()
  of "type", "GlbC": t.class()
  of "alis", "Pth ", "tdta":
    let n = t.u32()
    t.raw(int(n))
  of "ObAr":
    discard t.u32()
    t.descriptorBody()
  else: raise unsupported("descriptor OSType " & ty)

proc descriptorBody(t: Tx) =
  if t.depth > MaxDepth: raise limitExceeded("descriptor nesting too deep")
  inc t.depth
  t.unicode()
  t.id()
  let n = t.u32()
  t.r.checkCount(uint64(n), 10)
  for _ in 0 ..< int(n):
    t.id()
    t.value()
  dec t.depth

proc versionedDescriptor(t: Tx) =
  discard t.u32()
  t.descriptorBody()

proc descriptorShaped(t: Tx): bool =
  t.attempt(proc(x: Tx) = x.versionedDescriptor()) or
    t.attempt(proc(x: Tx) =
      discard x.u32()
      x.versionedDescriptor())

# ---------------------------------------------------------------- image resources

proc pathRecords(t: Tx) =
  while t.remaining >= 26:
    let selector = t.u16()
    case selector
    of 1, 2, 4, 5: t.i32s(6)
    of 0, 3: t.u16s(12)
    of 7:
      t.i32s(5); t.raw(4)
    of 8:
      discard t.u16(); t.raw(22)
    else: t.raw(24)

proc resourceData(t: Tx; id: uint16) =
  case id
  of 1005:
    discard t.u32(); discard t.u16(); discard t.u16(); discard t.u32(); discard t.u16(); discard t.u16()
  of 1024, 1026: t.u16sToEnd()
  of 1037, 1049: discard t.u32()
  of 1069:
    discard t.u16(); t.u32sToEnd()
  of 1057:
    discard t.u32(); discard t.u8(); t.unicode(); t.unicode(); discard t.u32()
  of 1032:
    discard t.u32(); discard t.u32(); discard t.u32()
    let n = t.u32()
    for _ in 0 ..< int(n):
      if t.remaining < 5: break
      discard t.u32(); discard t.u8()
  of 2000 .. 2997, 2999: t.pathRecords()
  else: discard

proc resources(t: Tx) =
  while t.remaining >= 12 and t.isSigAt(0, t.src.isLittle):
    discard t.key()
    let id = t.u16()
    t.pascal(2)
    var peek = t.r
    let odd = peek.u32() mod 2 == 1
    t.sizedRegion(false, proc(x: Tx) = x.resourceData(id))
    if odd and t.remaining > 0: t.raw(1)
  t.rest()

# ---------------------------------------------------------------- tagged blocks

proc blocks(t: Tx)
proc layerInfo(t: Tx)

proc padAfter(t: Tx): int =
  ## The smallest k in 0..3 after which the next block (or the end) starts.
  for k in 0 .. 3:
    if k == t.remaining or (k < t.remaining and t.isSigAt(k, t.src.isLittle)): return k
  0

proc metadataSetting(t: Tx) =
  let n = t.r.u32()
  t.r.checkCount(uint64(n), 16)
  let countAt = t.o.len
  t.o.add [0'u8, 0, 0, 0]
  var kept = 0'u32
  for _ in 0 ..< int(n):
    let before = t.o.len
    discard t.key()
    let key = t.key()
    discard t.u8()
    t.raw(3)
    let same = t.sameOrder
    try:
      t.sizedRegion(false, proc(x: Tx) =
        case key
        of "cmls", "mlst", "cust", "tmln", "sgrp", "cinf": x.versionedDescriptor()
        of "mdyn": x.u32sToEnd()
        else:
          if same: discard
          elif x.descriptorShaped(): discard
          else: raise unsupported("metadata item " & key))
      inc kept
    except PsdError as e:
      if same: raise e
      t.o.setLen(before)
      t.warnings.add "shmd item " & key & " dropped: " & e.msg
  t.patchLen(countAt, false, uint64(kept))

proc pattern(t: Tx) =
  discard t.u32()
  let mode = t.u32()
  discard t.u16(); discard t.u16()
  t.unicode()
  t.pascal(1)
  if mode == 2:
    t.raw(768)
    var peek = t.r
    var nextIsVersion = false
    try: nextIsVersion = peek.u32() == 3
    except PsdError: discard
    if not nextIsVersion and t.remaining >= 4: t.raw(4)
  discard t.u32()
  t.sizedRegion(false, proc(v: Tx) =
    v.i32s(4)
    let count = v.u32()
    for _ in 0 ..< int(min(uint64(count) + 2, uint64(high(int32)))):
      if v.remaining < 4: break
      if v.u32() == 0: continue
      var peek = v.r
      if peek.u32() == 0:
        discard v.u32()
        continue
      v.sizedRegion(false, proc(c: Tx) =
        discard c.u32()
        let ct = c.i32()
        discard c.i32()
        let cb = c.i32()
        discard c.i32()
        discard c.u16()
        let comp = c.u8()
        if comp == 1:
          let saved = c.version
          c.version = vPsd
          try: c.rleCounts(height(ct, cb))
          finally: c.version = saved))

proc effectsPlane(t: Tx; rows: int) =
  t.sizedRegion(true, proc(p: Tx) = p.channelData(rows, true))

proc filterEffects(t: Tx) =
  discard t.u32()
  t.sizedRegion(true, proc(x: Tx) =
    while not x.r.isEmpty:
      x.pascal(1)
      discard x.u32()
      x.sizedRegion(true, proc(it: Tx) =
        let top = it.i32()
        discard it.i32()
        let bottom = it.i32()
        discard it.i32()
        let rectH = height(top, bottom)
        discard it.u32()
        let mx = it.u32()
        if mx > FxMaxSlots: raise limitExceeded("filter effects: too many channels")
        for _ in 0 ..< int(mx) + 2:
          if it.u32() != 0: it.effectsPlane(rectH))
      if not x.r.isEmpty and x.u8() != 0:
        let top = x.i32()
        discard x.i32()
        let bottom = x.i32()
        discard x.i32()
        x.effectsPlane(height(top, bottom)))

proc legacyEffects(t: Tx) =
  discard t.u16()
  let n = t.u16()
  for _ in 0 ..< int(n):
    if t.remaining < 12: break
    discard t.key()
    let key = t.key()
    t.sizedRegion(false, proc(x: Tx) =
      proc colour(y: Tx) = y.u16s(5)
      proc blend(y: Tx) =
        discard y.key(); discard y.key()
      case key
      of "cmnS":
        discard x.u32(); discard x.u8(); discard x.u16()
      of "dsdw", "isdw":
        discard x.u32(); x.i32s(4); colour(x); blend(x)
        discard x.u8(); discard x.u8(); discard x.u8()
        if x.remaining >= 10: colour(x)
      of "oglw":
        discard x.u32(); x.i32s(2); colour(x); blend(x)
        discard x.u8(); discard x.u8()
        if x.remaining >= 10: colour(x)
      of "iglw":
        discard x.u32(); x.i32s(2); colour(x); blend(x)
        discard x.u8(); discard x.u8()
        if x.remaining >= 11:
          discard x.u8(); colour(x)
      of "bevl":
        discard x.u32(); x.i32s(3); blend(x); blend(x); colour(x); colour(x)
        for _ in 0 ..< 6: discard x.u8()
        if x.remaining >= 20:
          colour(x); colour(x)
      of "sofi":
        discard x.u32(); blend(x); colour(x)
        discard x.u8(); discard x.u8()
        if x.remaining >= 10: colour(x)
      else: discard)

proc curves(t: Tx) =
  discard t.u8()
  discard t.u16()
  let bits = t.u32()
  for bit in 0 ..< 32:
    if (bits and (1'u32 shl bit)) == 0: continue
    if t.remaining < 2: break
    let n = t.u16()
    t.u16s(int(n) * 2)
  if t.remaining >= 10:
    discard t.key()
    discard t.u16()
    let n = t.u32()
    for _ in 0 ..< int(n):
      if t.remaining < 4: break
      discard t.u16()
      let pts = t.u16()
      t.u16s(int(pts) * 2)

proc gradientMap(t: Tx) =
  let version = t.u16()
  discard t.u8(); discard t.u8()
  if version == 3: discard t.key()
  t.unicode()
  let n = t.u16()
  for _ in 0 ..< int(n):
    if t.remaining < 20: break
    discard t.u32(); discard t.u32(); t.u16s(6)
  let nt = t.u16()
  for _ in 0 ..< int(nt):
    if t.remaining < 10: break
    discard t.u32(); discard t.u32(); discard t.u16()
  for _ in 0 ..< 4:
    if t.remaining < 2: return
    discard t.u16()
  if t.remaining >= 4: discard t.u32()
  t.u16sToEnd()

proc linkedLayers(t: Tx) =
  while t.remaining >= 8:
    var peek = t.r
    let len = peek.u64()
    t.sizedRegion(true, proc(x: Tx) =
      let kind = x.key()
      let version = x.u32()
      x.pascal(1)
      x.unicode()
      discard x.key(); discard x.key()
      let dataLen = x.u64()
      if x.u8() != 0: x.versionedDescriptor()
      if kind == "liFE" and version > 3:
        discard x.i32()
        for _ in 0 ..< 4: discard x.u8()
        x.f64()
      if kind == "liFE": discard x.u64()
      if kind == "liFA": x.raw(8)
      if dataLen > uint64(high(int)): raise limitExceeded("linked file length")
      if kind == "liFD": x.raw(int(dataLen))
      if version >= 5: x.unicode()
      if version >= 6: x.f64()
      if version >= 7: discard x.u8()
      if kind == "liFE" and version == 2: x.raw(int(dataLen)))
    let pad = int((4 - len mod 4) mod 4)
    t.raw(min(pad, t.remaining))

proc annotations(t: Tx) =
  discard t.u16(); discard t.u16()
  let n = t.u32()
  for _ in 0 ..< int(n):
    if t.remaining < 4: break
    t.sizedRegionInclusive(proc(x: Tx) =
      discard x.key(); discard x.u8(); discard x.u8(); discard x.u16()
      x.i32s(8); x.u16s(5)
      x.pascal(2); x.pascal(2); x.pascal(2)
      x.sizedRegionInclusive(proc(d: Tx) =
        discard d.key(); discard d.u32()))

proc blockBody(t: Tx; key: string): bool =
  ## Transcodes a block's data by key; false for keys without a known layout.
  case key
  of "Layr", "Lr16", "Lr32": t.layerInfo()
  of "luni": t.unicode()
  of "lsct", "lsdk":
    discard t.u32()
    if t.remaining >= 8:
      discard t.key(); discard t.key()
    if t.remaining >= 4: discard t.u32()
  of "lyid", "lspf", "lyvr", "sn2P", "vowv": discard t.u32()
  of "Txt2": t.rest()
  of "lnsr": discard t.key()
  of "clbl", "infx", "knko", "tsly", "lmgm", "vmgm", "iOpa": discard t.u8()
  of "lclr": t.u16sUpto(4)
  of "fxrp":
    while t.remaining >= 8: t.f64()
  of "brst": t.u32sToEnd()
  of "LMsk": t.u16sUpto(6)
  of "FMsk": t.u16sToEnd()
  of "shmd": t.metadataSetting()
  of "lfx2", "lmfx", "lfxs", "CAI ":
    discard t.u32(); t.versionedDescriptor()
  of "SoCo", "GdFl", "PtFl", "vstk", "blwh", "vibA", "CgEd", "artb", "artd", "cinf", "cust", "PxSD", "GenI",
     "OCIO", "extn", "pths":
    t.versionedDescriptor()
  of "clrL":
    discard t.u16(); t.versionedDescriptor()
  of "lnk2", "lnk3", "lnkD", "lnkE": t.linkedLayers()
  of "Anno": t.annotations()
  of "vogk":
    discard t.u32(); t.versionedDescriptor()
  of "vscg":
    discard t.key(); t.versionedDescriptor()
  of "SoLd", "SoLE":
    discard t.key(); discard t.u32(); t.versionedDescriptor()
  of "PlLd":
    discard t.key(); discard t.u32(); t.pascal(1)
    for _ in 0 ..< 4: discard t.u32()
    t.f64s(8)
    discard t.u32()
    t.versionedDescriptor()
  of "TySh":
    discard t.u16(); t.f64s(6); discard t.u16(); t.versionedDescriptor()
    discard t.u16(); t.versionedDescriptor(); t.i32s(4)
  of "vmsk", "vsms":
    discard t.u32(); discard t.u32(); t.pathRecords()
  of "Patt", "Pat2", "Pat3":
    while t.remaining >= 4:
      var peek = t.r
      let len = peek.u32()
      if len == 0:
        discard t.u32()
        continue
      t.sizedRegion(false, pattern)
      let pad = int((4 - len mod 4) mod 4)
      t.raw(min(pad, t.remaining))
  of "FEid", "FXid": t.filterEffects()
  of "lrFX": t.legacyEffects()
  of "levl":
    discard t.u16()
    for _ in 0 ..< 29 * 5:
      if t.remaining < 2: break
      discard t.u16()
    if t.remaining >= 4: discard t.key()
    t.u16sToEnd()
  of "curv": t.curves()
  of "hue2":
    discard t.u16(); discard t.u8(); discard t.u8(); t.u16sToEnd()
  of "brit": t.u16sUpto(3)
  of "thrs", "post": discard t.u16()
  of "nvrt": discard
  of "expA":
    discard t.u16()
    for _ in 0 ..< 3: t.f32()
  of "blnc": t.u16sUpto(9)
  of "mixr", "selc": t.u16sToEnd()
  of "phfl":
    let v = t.u16()
    case v
    of 2:
      t.u16s(5); discard t.u32(); discard t.u8()
    of 3:
      for _ in 0 ..< 4: discard t.u32()
      discard t.u8()
    else: raise unsupported("photo filter version " & $v)
  of "grdm": t.gradientMap()
  else:
    if not t.sameOrder: return t.descriptorShaped()
    return false
  true

proc foreignMarker(sig: string): string = (if sig == "8B64": "46B8" else: "MIB8")

proc writeMarker(t: Tx; at: int; m: string) =
  for i in 0 ..< 4: t.o[at + i] = byte(m[i])

proc oneBlock(t: Tx) =
  let before = t.o.len
  if not t.src.isLittle and (t.restStartsWith(0, "MIB8") or t.restStartsWith(0, "46B8")):
    let sig = t.r.bytes(4)
    var key = t.r.bytes(4)
    var k = key
    if t.dst.isLittle: k = @[k[3], k[2], k[1], k[0]]
    var ks = newString(4)
    for i in 0 ..< 4: ks[i] = char(k[i])
    let long = usesLongLength(t.version, ks)
    let len = t.r.lenField(long)
    let data = t.r.bytesU64(len)
    if t.dst.isLittle: key = @[key[3], key[2], key[1], key[0]]
    t.o.add sig
    t.o.add key
    let n = t.o.len
    for _ in 0 ..< (if long: 8 else: 4): t.o.add 0
    t.patchLen(n, long, len)
    t.o.add data
    t.o.add t.r.bytes(t.padAfter)
    return
  let sigAt = t.o.len
  let sig = t.key()
  let key = t.key()
  let long = usesLongLength(t.version, key)
  let len = t.r.lenField(long)
  let lenAt = t.o.len
  for _ in 0 ..< (if long: 8 else: 4): t.o.add 0
  let data = t.r.bytesU64(len)
  let c = newTx(data, t.version, t.src, t.dst)
  c.depth = t.depth + 1
  var handled = false
  var err = ""
  var failed = false
  try:
    if c.depth > MaxDepth: raise limitExceeded("tagged block nesting too deep")
    handled = if data.len == 0: true else: c.blockBody(key)
  except PsdError as e:
    failed = true
    err = e.msg
  let padBytes = t.r.bytes(t.padAfter)
  if not failed:
    if handled:
      c.rest()
      t.warnings.add c.warnings
      t.patchLen(lenAt, long, uint64(c.o.len))
      t.o.add c.o
      t.o.add padBytes
    elif t.sameOrder:
      t.patchLen(lenAt, long, uint64(data.len))
      t.o.add data
      t.o.add padBytes
    elif t.src.isLittle:
      t.writeMarker(sigAt, foreignMarker(sig))
      t.patchLen(lenAt, long, uint64(data.len))
      t.o.add data
      t.o.add padBytes
    else:
      t.o.setLen(before)
      t.warnings.add key & " block (" & $data.len &
        " bytes) dropped: its layout is unknown and cannot be converted to the file's byte order"
  else:
    if t.sameOrder: raise invalid(key & " block: " & err)
    elif t.src.isLittle:
      t.writeMarker(sigAt, foreignMarker(sig))
      t.patchLen(lenAt, long, uint64(data.len))
      t.o.add data
      t.o.add padBytes
      t.warnings.add key & " block (" & $data.len & " bytes) kept as stored, in little-endian layout: " & err
    else:
      t.o.setLen(before)
      t.warnings.add key & " block (" & $data.len & " bytes) dropped: " & err

proc blocks(t: Tx) =
  while true:
    if t.remaining < 12 or not t.isSigAt(0, t.src.isLittle):
      t.rest()
      return
    t.oneBlock()

# ---------------------------------------------------------------- layer info

proc mask(t: Tx): (int, int) =
  if t.remaining == 0: return (0, 0)
  let whole = t.r.peekRest()
  let top = t.i32()
  discard t.i32()
  let bottom = t.i32()
  discard t.i32()
  discard t.u8()
  let flags = t.u8()
  var realH = 0
  if t.remaining <= 2: return (height(top, bottom), 0)
  let hasParameters = (flags and mfParameters) != 0
  let realFirst = hasParameters and realBeforeParameters(whole)
  proc readReal(x: Tx) =
    if x.remaining >= 18:
      discard x.u8(); discard x.u8()
      let rt = x.i32()
      discard x.i32()
      let rb = x.i32()
      discard x.i32()
      realH = height(rt, rb)
  if realFirst: readReal(t)
  if hasParameters:
    let pf = t.u8()
    let need = maskParameterBytes(pf)
    if need > t.remaining: raise eof(t.r.pos, need - t.remaining)
    if (pf and 1) != 0: discard t.u8()
    if (pf and 2) != 0: t.f64()
    if (pf and 4) != 0: discard t.u8()
    if (pf and 8) != 0: t.f64()
  if not realFirst: readReal(t)
  (height(top, bottom), realH)

proc layerInfo(t: Tx) =
  let count = t.i16()
  let n = abs(int(count))
  t.r.checkCount(uint64(n), 34)
  var records: seq[(seq[(int16, uint64)], array[3, int])]
  for _ in 0 ..< n:
    let top = t.i32()
    discard t.i32()
    let bottom = t.i32()
    discard t.i32()
    let nch = t.u16()
    if int(nch) > MaxLayerChannels: raise limitExceeded("too many channels in layer")
    var chans: seq[(int16, uint64)]
    for _ in 0 ..< int(nch):
      let id = t.i16()
      let len = t.lenOf(t.version.isPsb)
      chans.add (id, len)
    let sig = t.key()
    if sig != "8BIM": raise badSignature("8BIM (layer blend mode)", sig)
    discard t.key()
    for _ in 0 ..< 4: discard t.u8()
    var heights = [height(top, bottom), 0, 0]
    var maskHeights = (0, 0)
    t.sizedRegion(false, proc(x: Tx) =
      x.sizedRegion(false, proc(m: Tx) = maskHeights = m.mask())
      x.sizedRegion(false, proc(y: Tx) = y.rest())
      x.pascal(4)
      x.blocks())
    heights[1] = maskHeights[0]
    heights[2] = maskHeights[1]
    records.add (chans, heights)
  for (chans, heights) in records:
    for (id, len) in chans:
      if len == 0: continue
      if len == 1: raise invalid("channel data length 1")
      let rows = case id
        of -2: heights[1]
        of -3: heights[2]
        else: heights[0]
      t.region(len, proc(x: Tx) = x.channelData(rows, false))
  t.rest()

# ---------------------------------------------------------------- entry points

proc transcodeBlocks*(data: openArray[byte]; version: Version; src, dst: ByteOrder): (seq[byte], seq[string]) =
  ## Converts the tagged blocks after the 37724 header string from `src` to `dst`.
  let t = newTx(data, version, src, dst)
  t.blocks()
  (t.o, t.warnings)

proc transcodeLayerInfo*(data: openArray[byte]; version: Version; src, dst: ByteOrder): (seq[byte], seq[string]) =
  let t = newTx(data, version, src, dst)
  t.layerInfo()
  (t.o, t.warnings)

proc transcodeResources*(data: openArray[byte]; src, dst: ByteOrder): (seq[byte], seq[string]) =
  ## Converts image-resource blocks (a 34377 tag) between orders.
  let t = newTx(data, vPsd, src, dst)
  t.resources()
  (t.o, t.warnings)


