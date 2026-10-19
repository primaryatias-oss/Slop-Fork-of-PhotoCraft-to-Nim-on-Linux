## A small, bounds-checked TIFF 6.0 / TIFF-EP structure reader (port of `raw/src/tiff.rs`), plus
## the crate's error type and decoding limits.
##
## Only what raw decoding needs: IFD entries with typed value access, the IFD chain, SubIFDs (tag
## 330), the EXIF IFD (34665) and IFDs embedded at other offsets (maker notes). Every offset is
## checked against the buffer; reads that fall outside return none. The walk is bounded in IFD
## count and entry count so hostile files cannot loop or blow up memory.

import std/[options, strutils]
import ../textutil
import ./rutil

type
  RawErrorKind* = enum
    reNotRaw        ## not a camera raw file this port recognizes
    reUnsupported   ## recognized but not decoded yet
    reMalformed     ## damaged or inconsistent
    reLimitExceeded ## declared sizes exceed the `Limits`

  RawError* = object of CatchableError
    kind*: RawErrorKind
    detail*: string ## the message without the kind prefix

  RawLimits* = object
    ## Decompression-bomb guards, checked on header values before allocating.
    maxWidth*, maxHeight*: uint32
    maxPixels*: uint64 ## maximum `width * height`
    maxAlloc*: uint64  ## maximum bytes of one buffer

proc rawError*(kind: RawErrorKind; detail = ""): ref RawError =
  ## The error with the Rust `Display` text as its message.
  let msg = case kind
    of reNotRaw: "not a camera raw file"
    of reUnsupported: "unsupported camera raw: " & detail
    of reMalformed: "malformed camera raw: " & detail
    of reLimitExceeded: "camera raw exceeds limits: " & detail
  (ref RawError)(kind: kind, detail: detail, msg: msg)

proc malformed*(detail: string): ref RawError = rawError(reMalformed, detail)
proc unsupported*(detail: string): ref RawError = rawError(reUnsupported, detail)
proc notRaw*(): ref RawError = rawError(reNotRaw)

proc defaultRawLimits*(): RawLimits =
  RawLimits(maxWidth: 1 shl 18, maxHeight: 1 shl 18, maxPixels: 1'u64 shl 28, maxAlloc: 2'u64 shl 30)

proc satMul(a, b: uint64): uint64 {.inline.} =
  if a != 0 and b > high(uint64) div a: high(uint64) else: a * b

proc check*(l: RawLimits; width, height, bytesPerPixel: uint64) =
  ## Checks a `width × height` buffer of `bytesPerPixel` bytes per pixel.
  if width == 0 or height == 0: raise malformed("zero-sized image " & $width & "x" & $height)
  if width > uint64(l.maxWidth) or height > uint64(l.maxHeight):
    raise rawError(reLimitExceeded, "dimensions " & $width & "x" & $height & " exceed " & $l.maxWidth & "x" & $l.maxHeight)
  let pixels = satMul(width, height)
  if pixels > l.maxPixels: raise rawError(reLimitExceeded, $pixels & " pixels exceed " & $l.maxPixels)
  let bytes = satMul(pixels, bytesPerPixel)
  if bytes > l.maxAlloc or bytes > uint64(high(int)):
    raise rawError(reLimitExceeded, $bytes & " bytes exceed " & $l.maxAlloc)

proc check*(l: RawLimits; width, height, bytesPerPixel: int) =
  l.check(uint64(max(width, 0)), uint64(max(height, 0)), uint64(max(bytesPerPixel, 0)))

# ---------------------------------------------------------------- tags

const
  TagNewSubfileType* = 254'u16
  TagImageWidth* = 256'u16
  TagImageLength* = 257'u16
  TagBitsPerSample* = 258'u16
  TagCompression* = 259'u16
  TagPredictor* = 317'u16
  TagPhotometric* = 262'u16
  TagMake* = 271'u16
  TagModel* = 272'u16
  TagStripOffsets* = 273'u16
  TagOrientation* = 274'u16
  TagSamplesPerPixel* = 277'u16
  TagRowsPerStrip* = 278'u16
  TagStripByteCounts* = 279'u16
  TagPlanarConfiguration* = 284'u16
  TagTileWidth* = 322'u16
  TagTileLength* = 323'u16
  TagTileOffsets* = 324'u16
  TagTileByteCounts* = 325'u16
  TagSubIfds* = 330'u16
  TagSampleFormat* = 339'u16
  TagJpegInterchangeFormat* = 513'u16
  TagJpegInterchangeFormatLength* = 514'u16
  TagCfaRepeatPatternDim* = 33421'u16
  TagCfaPattern* = 33422'u16
  TagExifIfd* = 34665'u16
  TagExifCfaPattern* = 41730'u16
  TagMakerNote* = 37500'u16
  TagDngVersion* = 50706'u16
  TagUniqueCameraModel* = 50708'u16
  TagCfaPlaneColor* = 50710'u16
  TagCfaLayout* = 50711'u16
  TagLinearizationTable* = 50712'u16
  TagBlackLevelRepeatDim* = 50713'u16
  TagBlackLevel* = 50714'u16
  TagBlackLevelDeltaH* = 50715'u16
  TagBlackLevelDeltaV* = 50716'u16
  TagWhiteLevel* = 50717'u16
  TagDefaultScale* = 50718'u16
  TagDefaultCropOrigin* = 50719'u16
  TagDefaultCropSize* = 50720'u16
  TagColorMatrix1* = 50721'u16
  TagColorMatrix2* = 50722'u16
  TagCameraCalibration1* = 50723'u16
  TagCameraCalibration2* = 50724'u16
  TagAnalogBalance* = 50727'u16
  TagAsShotNeutral* = 50728'u16
  TagAsShotWhiteXy* = 50729'u16
  TagBaselineExposure* = 50730'u16
  TagCalibrationIlluminant1* = 50778'u16
  TagCalibrationIlluminant2* = 50779'u16
  TagActiveArea* = 50829'u16
  TagForwardMatrix1* = 50964'u16
  TagForwardMatrix2* = 50965'u16
  TagOpcodeList1* = 51008'u16
  TagOpcodeList2* = 51009'u16
  TagOpcodeList3* = 51022'u16
  TagBaselineExposureOffset* = 51109'u16
  TagProfileGainTableMap* = 52525'u16
  TagCr2Slice* = 50752'u16 ## CR2 private tag in the raw IFD: slice count, slice width, last width

  MaxIfds = 64         ## most IFDs a walk will visit
  MaxEntries = 4096    ## most entries read from one IFD
  MaxValues* = 1 shl 20 ## most values decoded from one entry by the list accessors

type
  Entry* = object
    ## One directory entry.
    tag*, typ*: uint16
    count*: uint32
    at*: int ## absolute offset of the value bytes (may be out of range; accessors check)

  Ifd* = object
    ## One image file directory.
    offset*: int
    entries*: seq[Entry]

  Tiff* = object
    ## A TIFF-structured buffer. Pass it by parameter (it owns its bytes).
    data*: seq[byte]
    le*: bool
    firstIfd*: int ## offset of IFD0 from the header

proc typeSize*(typ: uint16): int =
  case typ
  of 1, 2, 6, 7: 1
  of 3, 8: 2
  of 4, 9, 11, 13: 4
  of 5, 10, 12: 8
  else: 0

proc unit*(e: Entry): int {.inline.} = typeSize(e.typ)

proc get*(ifd: Ifd; tag: uint16): Option[Entry] =
  for e in ifd.entries:
    if e.tag == tag: return some(e)

proc has*(ifd: Ifd; tag: uint16): bool = ifd.get(tag).isSome

proc inRange*(t: Tiff; at, len: int): bool {.inline.} =
  ## Whether `len` bytes at `at` lie inside the buffer.
  at >= 0 and len >= 0 and at <= t.data.len and len <= t.data.len - at

proc hasMagic(d: openArray[byte]; m: string): bool =
  if d.len < m.len: return false
  for i, c in m:
    if d[i] != byte(c): return false
  true

proc u16At*(t: Tiff; at: int): Option[uint16] =
  if not t.inRange(at, 2): return
  let (a, b) = (uint16(t.data[at]), uint16(t.data[at + 1]))
  some(if t.le: a or (b shl 8) else: (a shl 8) or b)

proc u32At*(t: Tiff; at: int): Option[uint32] =
  if not t.inRange(at, 4): return
  var v = 0'u32
  for k in 0 .. 3:
    let byt = uint32(t.data[at + k])
    v = if t.le: v or (byt shl (8 * k)) else: (v shl 8) or byt
  some(v)

proc u64At(t: Tiff; at: int): Option[uint64] =
  if not t.inRange(at, 8): return
  var v = 0'u64
  for k in 0 .. 7:
    let byt = uint64(t.data[at + k])
    v = if t.le: v or (byt shl (8 * k)) else: (v shl 8) or byt
  some(v)

proc newTiff*(data: openArray[byte]): Option[Tiff] =
  ## Parses the 8-byte header. Accepts the TIFF magics and the TIFF-like variants used by camera
  ## raws (ORF `IIRO`/`IIRS`/`MMOR`, RW2 `IIU\0`).
  if data.len < 4: return
  var le: bool
  if data.hasMagic("II*\0") or data.hasMagic("IIRO") or data.hasMagic("IIRS") or data.hasMagic("IIU\0"): le = true
  elif data.hasMagic("MM\0*") or data.hasMagic("MMOR"): le = false
  else: return
  var t = Tiff(data: @data, le: le)
  let f = t.u32At(4)
  if f.isNone: return
  t.firstIfd = int(f.get)
  some(t)

proc satAddI(a, b: int): int {.inline.} = (if a > high(int) - b: high(int) else: a + b)

proc ifdAt*(t: Tiff; offset, base: int): Option[Ifd] =
  ## Reads the IFD at `offset`. Value offsets are relative to `base` (0 for ordinary IFDs; maker
  ## notes sometimes use their own base).
  let n0 = t.u16At(offset)
  if n0.isNone: return
  let n = int(n0.get)
  if n == 0 or n > MaxEntries: return
  var entries = newSeqOfCap[Entry](n)
  for i in 0 ..< n:
    let e = offset + 2 + 12 * i
    let (tag, typ, count) = (t.u16At(e), t.u16At(e + 2), t.u32At(e + 4))
    if tag.isNone or typ.isNone or count.isNone: break
    let size = typeSize(typ.get) * int(count.get)
    var at: int
    if size <= 4: at = e + 8
    else:
      let o = t.u32At(e + 8)
      if o.isNone: return
      at = satAddI(int(o.get), base)
    entries.add Entry(tag: tag.get, typ: typ.get, count: count.get, at: at)
  some(Ifd(offset: offset, entries: entries))

proc nextIfd*(t: Tiff; ifd: Ifd): int =
  ## Offset of the IFD that follows the one at `ifd` (0 = none).
  let v = t.u32At(ifd.offset + 2 + 12 * ifd.entries.len)
  if v.isSome: int(v.get) else: 0

proc uint*(t: Tiff; e: Entry; i: int): Option[uint32] =
  ## Value `i` of an entry as an unsigned integer (BYTE/SHORT/LONG/IFD and their signed variants;
  ## negative values are rejected).
  if i < 0 or i >= int(e.count): return
  let at = e.at + i * e.unit
  case e.typ
  of 1, 7:
    if t.inRange(at, 1): result = some(uint32(t.data[at]))
  of 6:
    if t.inRange(at, 1) and t.data[at] < 128: result = some(uint32(t.data[at]))
  of 3:
    let v = t.u16At(at)
    if v.isSome: result = some(uint32(v.get))
  of 8:
    let v = t.u16At(at)
    if v.isSome and v.get < 0x8000: result = some(uint32(v.get))
  of 4, 13: result = t.u32At(at)
  of 9:
    let v = t.u32At(at)
    if v.isSome and v.get < 0x8000_0000'u32: result = v
  else: discard

proc float*(t: Tiff; e: Entry; i: int): Option[float64] =
  ## Value `i` as a float (any numeric type, rationals included); none when not finite.
  if i < 0 or i >= int(e.count): return
  let at = e.at + i * e.unit
  var v: float64
  case e.typ
  of 1, 7:
    if not t.inRange(at, 1): return
    v = float64(t.data[at])
  of 6:
    if not t.inRange(at, 1): return
    v = float64(cast[int8](t.data[at]))
  of 3:
    let x = t.u16At(at)
    if x.isNone: return
    v = float64(x.get)
  of 8:
    let x = t.u16At(at)
    if x.isNone: return
    v = float64(cast[int16](x.get))
  of 4, 13:
    let x = t.u32At(at)
    if x.isNone: return
    v = float64(x.get)
  of 9:
    let x = t.u32At(at)
    if x.isNone: return
    v = float64(cast[int32](x.get))
  of 5:
    let (n, d) = (t.u32At(at), t.u32At(at + 4))
    if n.isNone or d.isNone or d.get == 0: return
    v = float64(n.get) / float64(d.get)
  of 10:
    let (n, d) = (t.u32At(at), t.u32At(at + 4))
    if n.isNone or d.isNone or d.get == 0: return
    v = float64(cast[int32](n.get)) / float64(cast[int32](d.get))
  of 11:
    let x = t.u32At(at)
    if x.isNone: return
    v = float64(cast[float32](x.get))
  of 12:
    let x = t.u64At(at)
    if x.isNone: return
    v = cast[float64](x.get)
  else: return
  if v == v and v != Inf and v != NegInf: result = some(v)

proc uints*(t: Tiff; e: Entry): seq[uint32] =
  ## All values as unsigned integers (empty if any is unreadable).
  let n = min(int(e.count), MaxValues)
  result = newSeqOfCap[uint32](min(n, 4096))
  for i in 0 ..< n:
    let v = t.uint(e, i)
    if v.isNone: return @[]
    result.add v.get

proc floats*(t: Tiff; e: Entry): seq[float64] =
  ## All values as floats (empty if any is unreadable).
  let n = min(int(e.count), MaxValues)
  result = newSeqOfCap[float64](min(n, 4096))
  for i in 0 ..< n:
    let v = t.float(e, i)
    if v.isNone: return @[]
    result.add v.get

proc rawRange*(t: Tiff; e: Entry): Option[(int, int)] =
  ## The raw value bytes of an entry as `(offset, length)`.
  let len = e.unit * int(e.count)
  if t.inRange(e.at, len): result = some((e.at, len))

proc rawBytes*(t: Tiff; e: Entry): Option[seq[byte]] =
  let r = t.rawRange(e)
  if r.isSome: result = some(t.data[r.get[0] ..< r.get[0] + r.get[1]])

proc ascii*(t: Tiff; e: Entry): Option[string] =
  ## An ASCII value, trimmed of NULs and whitespace.
  let r = t.rawRange(e)
  if r.isNone: return
  let (at, len) = r.get
  var stop = at + len
  for k in at ..< at + len:
    if t.data[k] == 0:
      stop = k
      break
  let s = lossyUtf8(t.data.toOpenArray(at, stop - 1)).strip(chars = Whitespace + {'\0'})
  if s.len > 0: result = some(s)

proc tagUint*(t: Tiff; ifd: Ifd; tag: uint16): Option[uint32] =
  let e = ifd.get(tag)
  if e.isSome: result = t.uint(e.get, 0)

proc tagUints*(t: Tiff; ifd: Ifd; tag: uint16): seq[uint32] =
  let e = ifd.get(tag)
  if e.isSome: result = t.uints(e.get)

proc tagFloats*(t: Tiff; ifd: Ifd; tag: uint16): seq[float64] =
  let e = ifd.get(tag)
  if e.isSome: result = t.floats(e.get)

proc tagAscii*(t: Tiff; ifd: Ifd; tag: uint16): Option[string] =
  let e = ifd.get(tag)
  if e.isSome: result = t.ascii(e.get)

proc allIfds*(t: Tiff): seq[Ifd] =
  ## Every IFD reachable from the header: the main chain, SubIFDs and the EXIF IFD, depth-first,
  ## deduplicated and bounded.
  var seen: seq[int]
  var stack = @[t.firstIfd]
  while stack.len > 0:
    let off = stack.pop()
    if off == 0 or off in seen or seen.len >= MaxIfds: continue
    seen.add off
    let ifd = t.ifdAt(off, 0)
    if ifd.isNone: continue
    let next = t.nextIfd(ifd.get)
    if next != 0: stack.add next
    for tg in [TagSubIfds, TagExifIfd]:
      let e = ifd.get.get(tg)
      if e.isSome:
        # Reverse so the first SubIFD is visited first.
        var v = t.uints(e.get)
        if v.len > 16: v.setLen(16)
        for k in countdown(v.high, 0): stack.add int(v[k])
    result.add ifd.get

proc orientation*(v: Option[uint32]): uint16 =
  ## The EXIF orientation (1..=8) of a raw tag value; anything else is the default 1. The range is
  ## checked on the `uint32` before narrowing.
  if v.isSome and v.get in 1'u32 .. 8'u32: uint16(v.get) else: 1'u16

proc dump*(data: openArray[byte]): string =
  ## A human-readable listing of every IFD and entry (debugging aid).
  let tt = newTiff(data)
  if tt.isNone: return "not a TIFF-structured file"
  let t = tt.get
  for ifd in t.allIfds:
    result.add "IFD @" & $ifd.offset & "\n"
    for e in ifd.entries:
      var preview: string
      if e.typ == 2: preview = t.ascii(e).get("")
      else:
        let n = min(int(e.count), 12)
        var v: seq[string]
        for i in 0 ..< n:
          let f = t.float(e, i)
          v.add(if f.isSome: displayF64(f.get) else: "?")
        preview = v.join(" ") & (if int(e.count) > n: " …" else: "")
      result.add "  " & align($e.tag, 5) & " (0x" & toHex(e.tag, 4) & ") type " & align($e.typ, 2) &
        " count " & align($e.count, 7) & " @" & align($e.at, 9) & ": " & preview & "\n"
