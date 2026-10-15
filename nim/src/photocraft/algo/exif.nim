## Minimal EXIF reader for the camera settings the photography commands use (exposure, aperture,
## ISO, focal length, camera and lens names), port of `exif.rs`. Input is the raw TIFF-structured
## EXIF payload (`II*\0` / `MM\0*`, as stored in `Document.metadata.exif`); malformed data yields
## none fields, never an exception. Tag numbers are from the EXIF 2.32 specification (CIPA DC-008).

import std/[options, math, strutils]
import ../textutil

type
  CameraInfo* = object
    ## Camera settings read from EXIF.
    make*, model*, lens*: Option[string]
    exposureTime*: Option[float64]   ## seconds
    fNumber*, iso*: Option[float64]
    focalLength*: Option[float64]    ## millimetres
    focalLength35mm*: Option[float64] ## 35 mm-equivalent focal length in millimetres
    dateTaken*: Option[string]       ## DateTimeOriginal as stored (`YYYY:MM:DD HH:MM:SS`)

  Tiff = object
    b: seq[byte]
    le: bool

proc exposureFactor*(c: CameraInfo): Option[float64] =
  ## Linear exposure factor `t · ISO / N²` (relative brightness of the image for one scene).
  if c.exposureTime.isNone: return none(float64)
  let t = c.exposureTime.get
  let n = max(c.fNumber.get(1.0), 0.1)
  some(t * c.iso.get(100.0) / 100.0 / (n * n))

proc u16(t: Tiff; o: int): Option[uint16] =
  if o < 0 or o > t.b.len - 2: return none(uint16)
  let (a, b) = (uint16(t.b[o]), uint16(t.b[o + 1]))
  some(if t.le: a or (b shl 8) else: (a shl 8) or b)

proc u32(t: Tiff; o: int): Option[uint32] =
  if o < 0 or o > t.b.len - 4: return none(uint32)
  var v = 0'u32
  for k in 0 .. 3:
    let byt = uint32(t.b[o + k])
    v = (if t.le: v or (byt shl (8 * k)) else: (v shl 8) or byt)
  some(v)

proc ifd(t: Tiff; off: int): seq[(uint16, uint16, uint32, int)] =
  ## Entries of the IFD at `off`: (tag, type, count, value offset field position).
  let n = t.u16(off)
  if n.isNone: return
  for k in 0 ..< int(n.get):
    let e = off + 2 + k * 12
    let (tag, typ, count) = (t.u16(e), t.u16(e + 2), t.u32(e + 4))
    if tag.isSome and typ.isSome and count.isSome: result.add (tag.get, typ.get, count.get, e + 8)

proc dataAt(t: Tiff; typ: uint16; count: uint32; field: int): Option[int] =
  let unit = (case typ
    of 1, 2, 6, 7: 1'u64
    of 3, 8: 2'u64
    of 4, 9, 11: 4'u64
    of 5, 10, 12: 8'u64
    else: return none(int))
  let size = unit * uint64(count)
  var at: uint64
  if size <= 4: at = uint64(field)
  else:
    let p = t.u32(field)
    if p.isNone: return none(int)
    at = uint64(p.get)
  # The whole value must lie inside the payload, not just its first element.
  if at + size > uint64(t.b.len): return none(int)
  some(int(at))

proc number(t: Tiff; typ: uint16; count: uint32; field: int): Option[float64] =
  let at = t.dataAt(typ, count, field)
  if at.isNone: return none(float64)
  let a = at.get
  case typ
  of 3:
    let v = t.u16(a)
    if v.isSome: some(float64(v.get)) else: none(float64)
  of 4:
    let v = t.u32(a)
    if v.isSome: some(float64(v.get)) else: none(float64)
  of 5:
    let (n, d) = (t.u32(a), t.u32(a + 4))
    if n.isNone or d.isNone or d.get == 0: none(float64) else: some(float64(n.get) / float64(d.get))
  of 10:
    let (n, d) = (t.u32(a), t.u32(a + 4))
    if n.isNone or d.isNone or d.get == 0: none(float64)
    else: some(float64(cast[int32](n.get)) / float64(cast[int32](d.get)))
  else: none(float64)

proc text(t: Tiff; typ: uint16; count: uint32; field: int): Option[string] =
  if typ != 2: return none(string)
  let at = t.dataAt(typ, count, field)
  if at.isNone: return none(string)
  let a = at.get
  if uint64(a) + uint64(count) > uint64(t.b.len): return none(string)
  var s = lossyUtf8(t.b.toOpenArray(a, a + int(count) - 1))
  var e = s.len
  while e > 0 and s[e - 1] == '\0': dec e
  s = trimRust(s[0 ..< e])
  if s.len > 0: some(s) else: none(string)

proc read*(exif: openArray[byte]): CameraInfo =
  ## Reads `CameraInfo` from a TIFF-structured EXIF payload (an `Exif\0\0` prefix is skipped).
  var b = @exif
  if b.len >= 6 and b[0 .. 5] == @[byte('E'), byte('x'), byte('i'), byte('f'), 0, 0]: b = b[6 .. ^1]
  if b.len < 4: return
  let le =
    if b[0] == byte('I') and b[1] == byte('I') and b[2] == 42 and b[3] == 0: true
    elif b[0] == byte('M') and b[1] == byte('M') and b[2] == 0 and b[3] == 42: false
    else: return
  let t = Tiff(b: b, le: le)
  let ifd0 = t.u32(4)
  if ifd0.isNone: return
  var queue = @[int(ifd0.get)]
  var seen: seq[int]
  while queue.len > 0:
    let off = queue.pop()
    if off in seen or seen.len > 8: continue
    seen.add off
    for (tag, typ, count, field) in t.ifd(off):
      case tag
      of 0x010F: result.make = t.text(typ, count, field)
      of 0x0110: result.model = t.text(typ, count, field)
      of 0x829A: result.exposureTime = t.number(typ, count, field)
      of 0x829D: result.fNumber = t.number(typ, count, field)
      of 0x8827: result.iso = t.number(typ, count, field)
      of 0x920A: result.focalLength = t.number(typ, count, field)
      of 0xA405:
        let v = t.number(typ, count, field)
        result.focalLength35mm = (if v.isSome and v.get > 0: v else: none(float64))
      of 0xA434: result.lens = t.text(typ, count, field)
      of 0x9003: result.dateTaken = t.text(typ, count, field)
      of 0x8769:
        let p = t.u32(field)
        if p.isSome: queue.add int(p.get)
      else: discard

proc putLe(o: var seq[byte]; v: uint64; n: int) =
  for k in 0 ..< n: o.add byte((v shr (8 * k)) and 0xFF)

proc build*(info: CameraInfo): seq[byte] =
  ## Builds a little-endian EXIF payload (used by tests and by synthetic fixtures).
  type Entry = (uint16, uint16, uint32, seq[byte])
  var ifd0, exif: seq[Entry]
  proc text(s: string): seq[byte] =
    for c in s: result.add byte(c)
    result.add 0
  proc rational(v: float64): seq[byte] =
    let d = 10000'u32
    let n = (let x = round(v * float64(d)); if x != x or x <= 0: 0'u32 elif x >= 4294967295.0: high(uint32) else: uint32(x))
    result.putLe(n, 4)
    result.putLe(d, 4)
  proc u16le(v: float64): seq[byte] =
    let n = (if v != v or v <= 0: 0'u16 elif v >= 65535: high(uint16) else: uint16(v))
    result.putLe(n, 2)
  if info.make.isSome: ifd0.add (0x010F'u16, 2'u16, uint32(info.make.get.len + 1), text(info.make.get))
  if info.model.isSome: ifd0.add (0x0110'u16, 2'u16, uint32(info.model.get.len + 1), text(info.model.get))
  if info.exposureTime.isSome: exif.add (0x829A'u16, 5'u16, 1'u32, rational(info.exposureTime.get))
  if info.fNumber.isSome: exif.add (0x829D'u16, 5'u16, 1'u32, rational(info.fNumber.get))
  if info.iso.isSome: exif.add (0x8827'u16, 3'u16, 1'u32, u16le(info.iso.get))
  if info.dateTaken.isSome: exif.add (0x9003'u16, 2'u16, uint32(info.dateTaken.get.len + 1), text(info.dateTaken.get))
  if info.focalLength.isSome: exif.add (0x920A'u16, 5'u16, 1'u32, rational(info.focalLength.get))
  if info.focalLength35mm.isSome: exif.add (0xA405'u16, 3'u16, 1'u32, u16le(info.focalLength35mm.get))
  if info.lens.isSome: exif.add (0xA434'u16, 2'u16, uint32(info.lens.get.len + 1), text(info.lens.get))
  proc ifdLen(n: int): int = 2 + 12 * n + 4
  let ifd0Off = 8
  let n0 = ifd0.len + 1
  let exifOff = ifd0Off + ifdLen(n0)
  var dataOff = exifOff + ifdLen(exif.len)
  result = @[byte('I'), byte('I'), 42, 0]
  result.putLe(uint64(ifd0Off), 4)
  var data: seq[byte]
  proc writeIfd(o: var seq[byte]; entries: seq[Entry]; extra: Option[uint32]) =
    let n = entries.len + (if extra.isSome: 1 else: 0)
    o.putLe(uint64(n), 2)
    for (tag, typ, count, bytes) in entries:
      o.putLe(tag, 2)
      o.putLe(typ, 2)
      o.putLe(count, 4)
      if bytes.len <= 4:
        var f = bytes
        f.setLen(4)
        o.add f
      else:
        o.putLe(uint64(dataOff), 4)
        data.add bytes
        dataOff += bytes.len
    if extra.isSome:
      o.putLe(0x8769, 2)
      o.putLe(4, 2)
      o.putLe(1, 4)
      o.putLe(extra.get, 4)
    o.putLe(0, 4)
  writeIfd(result, ifd0, some(uint32(exifOff)))
  writeIfd(result, exif, none(uint32))
  result.add data
