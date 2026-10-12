## Image metadata helpers: EXIF/TIFF IFD0 orientation and resolution, XMP `tiff:` properties,
## Photoshop's ResolutionInfo (8BIM 0x03ED), the EXIF ColorSpace tag, and turning pixels
## upright. Everything read here is untrusted: offsets are bounds-checked and anything
## malformed reads as "absent". Port of `crates/codecs/src/{orientation,resolution,exif_color}.rs`.

import std/[options, strutils, math, algorithm]
import ./image

const
  TagOrientation = 0x0112
  TagXRes = 282
  TagYRes = 283
  TagResUnit = 296
  TypeShort = 3
  TypeLong = 4
  TypeRational = 5
  CmPerInch = 2.54
  MaxPpi = 1_000_000.0
  SamePpi = 1e-3
  ExifColorSpaceSrgb* = 1

type
  Order* = enum oLittle, oBig
  Ifd0* = object
    order*: Order
    first*, count*, entryBytes*: int
    big*: bool

proc rd16*(o: Order; b: openArray[byte]; at: int): Option[int] =
  if at < 0 or at + 2 > b.len: return none(int)
  some(if o == oLittle: int(b[at]) or (int(b[at + 1]) shl 8) else: (int(b[at]) shl 8) or int(b[at + 1]))

proc rd32*(o: Order; b: openArray[byte]; at: int): Option[int64] =
  if at < 0 or at + 4 > b.len: return none(int64)
  var v: int64
  if o == oLittle:
    for i in countdown(3, 0): v = (v shl 8) or int64(b[at + i])
  else:
    for i in 0..3: v = (v shl 8) or int64(b[at + i])
  some(v)

proc rd64*(o: Order; b: openArray[byte]; at: int): Option[int64] =
  if at < 0 or at + 8 > b.len: return none(int64)
  var v: uint64
  if o == oLittle:
    for i in countdown(7, 0): v = (v shl 8) or uint64(b[at + i])
  else:
    for i in 0..7: v = (v shl 8) or uint64(b[at + i])
  if v > uint64(high(int64)): none(int64) else: some(int64(v))

proc wr16(o: Order; b: var seq[byte]; at, v: int) =
  if o == oLittle: (b[at] = byte(v and 0xFF); b[at + 1] = byte((v shr 8) and 0xFF))
  else: (b[at] = byte((v shr 8) and 0xFF); b[at + 1] = byte(v and 0xFF))

proc wr32(o: Order; b: var seq[byte]; at: int; v: int64) =
  for i in 0..3:
    let s = if o == oLittle: 8 * i else: 8 * (3 - i)
    b[at + i] = byte((v shr s) and 0xFF)

proc tiffBodyStart*(b: openArray[byte]): int =
  ## Offset of the TIFF payload (a JPEG-style `Exif\0\0` prefix is skipped).
  if b.len >= 6 and b[0] == byte('E') and b[1] == byte('x') and b[2] == byte('i') and b[3] == byte('f') and b[4] == 0 and b[5] == 0: 6 else: 0

proc ifd0Entries*(b: openArray[byte]): Option[Ifd0] =
  ## IFD0 of a TIFF structure (classic or BigTIFF).
  if b.len < 8: return none(Ifd0)
  var order: Order
  var big = false
  if b[0] == byte('I') and b[1] == byte('I') and b[2] == 42 and b[3] == 0: order = oLittle
  elif b[0] == byte('M') and b[1] == byte('M') and b[2] == 0 and b[3] == 42: order = oBig
  elif b[0] == byte('I') and b[1] == byte('I') and b[2] == 43 and b[3] == 0: (order, big) = (oLittle, true)
  elif b[0] == byte('M') and b[1] == byte('M') and b[2] == 0 and b[3] == 43: (order, big) = (oBig, true)
  else: return none(Ifd0)
  var ifd, countBytes, entryBytes, nextBytes, header: int
  if big:
    if rd16(order, b, 4) != some(8) or rd16(order, b, 6) != some(0): return none(Ifd0)
    let v = rd64(order, b, 8)
    if v.isNone: return none(Ifd0)
    (ifd, countBytes, entryBytes, nextBytes, header) = (int(v.get), 8, 20, 8, 16)
  else:
    let v = rd32(order, b, 4)
    if v.isNone: return none(Ifd0)
    (ifd, countBytes, entryBytes, nextBytes, header) = (int(v.get), 2, 12, 4, 8)
  if ifd < header: return none(Ifd0)
  let cnt = if big: rd64(order, b, ifd) else: (let c = rd16(order, b, ifd); if c.isSome: some(int64(c.get)) else: none(int64))
  if cnt.isNone or cnt.get > 100_000: return none(Ifd0)
  let first = ifd + countBytes
  let nextIfd = first + int(cnt.get) * entryBytes
  if nextIfd + nextBytes > b.len: return none(Ifd0)
  some(Ifd0(order: order, first: first, count: int(cnt.get), entryBytes: entryBytes, big: big))

proc entryCount(d: Ifd0; b: openArray[byte]; e: int): Option[int64] =
  if d.big: rd64(d.order, b, e + 4) else: rd32(d.order, b, e + 4)

# ---------------------------------------------------------------- orientation

proc orientationValue(d: Ifd0; b: openArray[byte]; e: int): Option[(int, int)] =
  ## (value offset, type) of a well-formed Orientation entry.
  if rd16(d.order, b, e) != some(TagOrientation) or entryCount(d, b, e) != some(1'i64): return none((int, int))
  let ty = rd16(d.order, b, e + 2)
  if ty.isNone or ty.get notin [TypeShort, TypeLong]: return none((int, int))
  some((e + (if d.big: 12 else: 8), ty.get))

proc exifOrientation*(exif: openArray[byte]): int =
  ## The orientation (1–8) in a TIFF-structured block; 1 when missing or malformed.
  let s = tiffBodyStart(exif)
  template b: untyped = exif.toOpenArray(s, exif.len - 1)
  let d = ifd0Entries(b)
  if d.isNone: return 1
  for i in 0 ..< d.get.count:
    let e = d.get.first + i * d.get.entryBytes
    if rd16(d.get.order, b, e) == some(TagOrientation):
      let v = orientationValue(d.get, b, e)
      if v.isNone: return 1
      let (at, ty) = v.get
      let val = if ty == TypeShort: (let x = rd16(d.get.order, b, at); if x.isSome: int64(x.get) else: 0'i64)
                else: rd32(d.get.order, b, at).get(0)
      return if val >= 1 and val <= 8: int(val) else: 1
  1

proc uprightExif*(exif: seq[byte]): seq[byte] =
  ## `exif` with every well-formed Orientation entry rewritten to 1.
  result = exif
  let s = tiffBodyStart(exif)
  template b: untyped = exif.toOpenArray(s, exif.len - 1)
  let d = ifd0Entries(b)
  if d.isNone: return
  for i in 0 ..< d.get.count:
    let e = d.get.first + i * d.get.entryBytes
    let v = orientationValue(d.get, b, e)
    if v.isSome:
      let (at, ty) = v.get
      if ty == TypeShort: wr16(d.get.order, result, s + at, 1)
      else: wr32(d.get.order, result, s + at, 1)

proc uprightXmp*(xmp: string): string =
  ## `xmp` with any `tiff:Orientation` set to 1.
  const Key = "tiff:Orientation"
  if Key notin xmp: return xmp
  var i = 0
  while true:
    let k = xmp.find(Key, i)
    if k < 0: break
    result.add xmp[i ..< k + Key.len]
    var j = k + Key.len
    while j < xmp.len and xmp[j] in {' ', '\t', '\r', '\n', '=', '"', '\'', '>'}: inc j
    if j > k + Key.len and xmp[j - 1] in {'"', '\'', '>'}:
      var e = j
      while e < xmp.len and xmp[e] in Digits: inc e
      if e > j:
        result.add xmp[k + Key.len ..< j]
        result.add '1'
        i = e
        continue
    i = k + Key.len
  result.add xmp[i .. ^1]

proc walk(o, w, h, y2: int): (int, int) {.inline.} =
  case o
  of 2: (y2 * w + w - 1, -1)
  of 3: ((h - 1 - y2) * w + w - 1, -1)
  of 4: ((h - 1 - y2) * w, 1)
  of 5: (y2, w)
  of 6: ((h - 1) * w + y2, -w)
  of 7: ((h - 1) * w + (w - 1 - y2), -w)
  of 8: (w - 1 - y2, w)
  else: (y2 * w, 1)

proc oriented*(img: Image; o: int): Image =
  ## The image turned upright for orientation `o` (1–8). Width/height (and DPI) swap for 5–8;
  ## EXIF and XMP orientation are rewritten to 1.
  if o < 2 or o > 8: return img
  let w = img.width
  let h = img.height
  let swap = o >= 5
  let (ow, oh) = if swap: (h, w) else: (w, h)
  let bpp = img.layout.channels * img.sample.bytes
  var data = newSeq[byte](img.data.len)
  for y2 in 0 ..< oh:
    let (base, step) = walk(o, w, h, y2)
    var i = base
    for x in 0 ..< ow:
      if i >= 0 and i < w * h:
        copyMem(addr data[(y2 * ow + x) * bpp], unsafeAddr img.data[i * bpp], bpp)
      i += step
  result = img
  result.width = ow
  result.height = oh
  result.data = data
  if result.meta.exif.isSome: result.meta.exif = some(uprightExif(result.meta.exif.get))
  if result.meta.xmp.isSome: result.meta.xmp = some(uprightXmp(result.meta.xmp.get))
  if swap and result.meta.dpi.isSome:
    let (x, y) = result.meta.dpi.get
    result.meta.dpi = some((y, x))

# ---------------------------------------------------------------- resolution

proc ppiPair(x, y: float64): Option[(float32, float32)] =
  proc ok(v: float64): bool = classify(v) notin {fcNan, fcInf, fcNegInf} and v > 0.0 and v <= MaxPpi
  if ok(x) and ok(y): some((float32(x), float32(y))) else: none((float32, float32))

proc same(a, b: (float32, float32)): bool =
  abs(float64(a[0]) - float64(b[0])) < SamePpi and abs(float64(a[1]) - float64(b[1])) < SamePpi

proc rational(ppi: float32): (int64, int64) =
  let p = float64(ppi)
  if abs(p - round(p)) < SamePpi: (int64(round(p)), 1'i64) else: (int64(round(p * 1000.0)), 1000'i64)

proc valueAt(d: Ifd0; b: openArray[byte]; e, ty: int; count: int64): Option[int] =
  let size = case ty
    of TypeShort: 2'i64 * count
    of TypeRational: 8'i64 * count
    else: return none(int)
  let (field, inline) = if d.big: (e + 12, 8'i64) else: (e + 8, 4'i64)
  var at: int64
  if size <= inline: at = field
  else:
    let v = if d.big: rd64(d.order, b, field) else: rd32(d.order, b, field)
    if v.isNone: return none(int)
    at = v.get
  if at < 0 or at + size > b.len: none(int) else: some(int(at))

type ResField = object
  tag, ty: int
  count: int64
  value: Option[int]

proc resFields(d: Ifd0; b: openArray[byte]): seq[ResField] =
  for i in 0 ..< d.count:
    let e = d.first + i * d.entryBytes
    let tag = rd16(d.order, b, e)
    if tag.isNone or tag.get notin [TagXRes, TagYRes, TagResUnit]: continue
    let ty = rd16(d.order, b, e + 2).get(0)
    let cnt = entryCount(d, b, e).get(0)
    result.add ResField(tag: tag.get, ty: ty, count: cnt, value: valueAt(d, b, e, ty, cnt))

proc firstOf(fs: seq[ResField]; tag: int): Option[ResField] =
  for f in fs:
    if f.tag == tag: return some(f)
  none(ResField)

proc rat(d: Ifd0; b: openArray[byte]; f: Option[ResField]): Option[float64] =
  if f.isNone or f.get.ty != TypeRational or f.get.count != 1 or f.get.value.isNone: return none(float64)
  let n = rd32(d.order, b, f.get.value.get)
  let dd = rd32(d.order, b, f.get.value.get + 4)
  if n.isNone or dd.isNone or dd.get == 0: return none(float64)
  some(float64(n.get) / float64(dd.get))

proc exifResolution*(exif: openArray[byte]): Option[(float32, float32)] =
  ## The resolution (ppi) in IFD0 of an EXIF block or TIFF structure.
  let s = tiffBodyStart(exif)
  template b: untyped = exif.toOpenArray(s, exif.len - 1)
  let d = ifd0Entries(b)
  if d.isNone: return
  let fs = resFields(d.get, b)
  var unit = 2
  let u = firstOf(fs, TagResUnit)
  if u.isSome:
    if u.get.ty != TypeShort or u.get.count != 1 or u.get.value.isNone: return
    unit = rd16(d.get.order, b, u.get.value.get).get(0)
  let scale = case unit
    of 2: 1.0
    of 3: CmPerInch
    else: return
  let x = rat(d.get, b, firstOf(fs, TagXRes))
  let y = rat(d.get, b, firstOf(fs, TagYRes))
  if x.isNone or y.isNone: return
  ppiPair(x.get * scale, y.get * scale)

proc exifWithResolution*(exif: seq[byte]; ppi: (float32, float32)): seq[byte] =
  ## `exif` with its IFD0 resolution values set to `ppi` in place (unit inch).
  result = exif
  if ppiPair(float64(ppi[0]), float64(ppi[1])).isNone: return
  let s = tiffBodyStart(exif)
  template b: untyped = exif.toOpenArray(s, exif.len - 1)
  let d = ifd0Entries(b)
  if d.isNone: return
  let cur = exifResolution(exif)
  if cur.isSome and same(cur.get, ppi): return
  for f in resFields(d.get, b):
    if f.value.isNone or f.count != 1: continue
    if f.tag == TagResUnit and f.ty == TypeShort: wr16(d.get.order, result, s + f.value.get, 2)
    elif f.tag in [TagXRes, TagYRes] and f.ty == TypeRational:
      let (n, dd) = rational(if f.tag == TagXRes: ppi[0] else: ppi[1])
      wr32(d.get.order, result, s + f.value.get, n)
      wr32(d.get.order, result, s + f.value.get + 4, dd)

proc photoshopResolution*(res: openArray[byte]): Option[(float32, float32)] =
  ## The resolution in a Photoshop image resource block (8BIM 0x03ED, Fixed 16.16 ppi).
  var i = 0
  template be16(at: int): int = (int(res[at]) shl 8) or int(res[at + 1])
  template be32(at: int): int64 = (int64(res[at]) shl 24) or (int64(res[at + 1]) shl 16) or (int64(res[at + 2]) shl 8) or int64(res[at + 3])
  while i + 12 <= res.len:
    let isBim = res[i] == byte('8') and res[i + 1] == byte('B') and res[i + 2] == byte('I') and res[i + 3] == byte('M')
    let id = be16(i + 4)
    let nameLen = int(res[i + 6])
    let nameSz = ((nameLen + 1) + 1) and not 1
    let sizeAt = i + 6 + nameSz
    if sizeAt + 4 > res.len: return
    let size = int(be32(sizeAt))
    let data = sizeAt + 4
    if size < 0 or data + size > res.len: return
    if isBim and id == 0x03ED:
      if size < 16: return
      return ppiPair(float64(be32(data)) / 65536.0, float64(be32(data + 8)) / 65536.0)
    i = data + size + (size mod 2)

proc xmpValues(xmp, key: string): seq[(int, int)] =
  ## Text ranges of every value of XMP property `key` (`key="v"`, `key='v'` or `<key>v</key>`).
  var frm = 0
  while true:
    let k = xmp.find(key, frm)
    if k < 0: break
    frm = k + key.len
    if k == 0 or not (xmp[k - 1] == '<' or xmp[k - 1] in Whitespace): continue
    var p = frm
    var close: char
    var start: int
    var q = p
    while q < xmp.len and xmp[q] in Whitespace: inc q
    if q < xmp.len and xmp[q] == '=':
      inc q
      while q < xmp.len and xmp[q] in Whitespace: inc q
      if q < xmp.len and xmp[q] in {'"', '\''}:
        close = xmp[q]
        start = q + 1
      else: continue
    elif p < xmp.len and xmp[p] == '>':
      close = '<'
      start = p + 1
    else: continue
    let e = xmp.find(close, start)
    if e < 0: continue
    result.add (start, e)

proc xmpNumber(s0: string): Option[float64] =
  let s = s0.strip
  try:
    let slash = s.find('/')
    if slash >= 0:
      let n = parseFloat(s[0 ..< slash].strip)
      let d = parseFloat(s[slash + 1 .. ^1].strip)
      if d == 0.0: none(float64) else: some(n / d)
    else: some(parseFloat(s))
  except ValueError: none(float64)

proc xmpResolution*(xmp: string): Option[(float32, float32)] =
  ## The resolution (ppi) in an XMP packet's `tiff:` properties.
  proc first(key: string): Option[string] =
    let v = xmpValues(xmp, key)
    if v.len == 0: none(string) else: some(xmp[v[0][0] ..< v[0][1]])
  var unit = 2.0
  let u = first("tiff:ResolutionUnit")
  if u.isSome:
    let n = xmpNumber(u.get)
    if n.isNone: return
    unit = n.get
  let scale = if unit == 2.0: 1.0 elif unit == 3.0: CmPerInch else: return
  let x = first("tiff:XResolution")
  let y = first("tiff:YResolution")
  if x.isNone or y.isNone: return
  let xn = xmpNumber(x.get)
  let yn = xmpNumber(y.get)
  if xn.isNone or yn.isNone: return
  ppiPair(xn.get * scale, yn.get * scale)

proc xmpWithResolution*(xmp: string; ppi: (float32, float32)): string =
  ## `xmp` with its `tiff:` resolution properties set to `ppi` (unit inch).
  if ppiPair(float64(ppi[0]), float64(ppi[1])).isNone: return xmp
  let cur = xmpResolution(xmp)
  if cur.isSome and same(cur.get, ppi): return xmp
  proc text(v: float32): string =
    let (n, d) = rational(v)
    $n & "/" & $d
  var edits: seq[(int, int, string)]
  for (key, value) in [("tiff:XResolution", text(ppi[0])), ("tiff:YResolution", text(ppi[1])), ("tiff:ResolutionUnit", "2")]:
    for (a, b) in xmpValues(xmp, key): edits.add (a, b, value)
  if edits.len == 0: return xmp
  edits.sort(proc(a, b: (int, int, string)): int = cmp(a[0], b[0]))
  var at = 0
  for (a, b, v) in edits:
    if a < at: continue
    result.add xmp[at ..< a]
    result.add v
    at = b
  result.add xmp[at .. ^1]

proc exifWithoutThumbnail(exif: seq[byte]): seq[byte] =
  result = exif
  let s = tiffBodyStart(exif)
  let d = ifd0Entries(exif.toOpenArray(s, exif.len - 1))
  if d.isNone: return
  let next = s + d.get.first + d.get.count * d.get.entryBytes
  let width = if d.get.big: 8 else: 4
  if next + width <= result.len:
    for i in 0 ..< width: result[next + i] = 0

proc exportExif*(exif: seq[byte]; ppi: Option[(float32, float32)]): seq[byte] =
  ## The EXIF to write next to upright pixels: Orientation 1, no source thumbnail, resolution.
  result = exifWithoutThumbnail(uprightExif(exif))
  if ppi.isSome: result = exifWithResolution(result, ppi.get)

proc exportXmp*(xmp: string; ppi: Option[(float32, float32)]): string =
  result = uprightXmp(xmp)
  if ppi.isSome: result = xmpWithResolution(result, ppi.get)

proc resolutionFromMetadata*(exif: Option[seq[byte]]; xmp: Option[string]): Option[(float32, float32)] =
  ## XMP, then EXIF.
  if xmp.isSome:
    let r = xmpResolution(xmp.get)
    if r.isSome: return r
  if exif.isSome: return exifResolution(exif.get)

proc exifColorSpace*(exif: openArray[byte]): Option[int] =
  ## The EXIF ColorSpace value (0xA001 in the Exif sub-IFD): 1 = sRGB, 0xFFFF = uncalibrated.
  let s = tiffBodyStart(exif)
  template b: untyped = exif.toOpenArray(s, exif.len - 1)
  let d = ifd0Entries(b)
  if d.isNone or d.get.big: return
  let o = d.get.order
  var ptrE = -1
  for i in 0 ..< d.get.count:
    let e = d.get.first + i * 12
    if rd16(o, b, e) == some(0x8769):
      ptrE = e
      break
  if ptrE < 0: return
  let ty = rd16(o, b, ptrE + 2)
  if ty.isNone or ty.get notin [TypeLong, 13] or rd32(o, b, ptrE + 4) != some(1'i64): return
  let ifd = int(rd32(o, b, ptrE + 8).get(0))
  if ifd < 8: return
  let n = rd16(o, b, ifd)
  if n.isNone: return
  let first = ifd + 2
  if first + n.get * 12 > b.len: return
  for i in 0 ..< n.get:
    let e = first + i * 12
    if rd16(o, b, e) == some(0xA001):
      if rd32(o, b, e + 4) != some(1'i64): return
      let t = rd16(o, b, e + 2)
      if t == some(TypeShort): return rd16(o, b, e + 8)
      if t == some(TypeLong):
        let v = rd32(o, b, e + 8)
        if v.isSome and v.get <= 0xFFFF: return some(int(v.get))
      return
