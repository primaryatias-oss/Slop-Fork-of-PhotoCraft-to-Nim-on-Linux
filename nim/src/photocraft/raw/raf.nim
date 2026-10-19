## Fujifilm RAF (port of `raw/src/raf.rs`).
##
## A RAF file starts with a big-endian header: the magic `FUJIFILMCCD-RAW `, a format version, a
## camera id and the model name (32 bytes at 28), then (offset, length) pairs for the camera's JPEG
## preview (at 84), a metadata directory (at 92) and the CFA region (at 100). The metadata
## directory holds (tag, size, data) records (ExifTool's FujiFilm RAF table). The CFA region is a
## small TIFF whose IFD0 points (tag 0xF000) at a Fuji IFD with the raw size, bits, strip, black
## level and white balance; offsets in it are relative to the CFA region.
##
## Sample storage (observed): 16-bit containers in the CFA TIFF's byte order; 12-bit samples packed
## LSB first; or 14-bit samples packed into little-endian 32-bit words, MSB first. A shorter strip
## is Fujifilm's own lossless compression, reported as unsupported. XTransLayout lists the 6×6
## pattern in reverse order; FujiLayout lists the 2×2 Bayer pattern the same way.

import std/[options, strutils]
import ../textutil
import ./tiff, ./sensor, ./cr2

const
  RafMagic* = "FUJIFILMCCD-RAW"
  CropTopLeft = 0x0110'u16
  CroppedSize = 0x0111'u16
  FujiLayout = 0x0130'u16
  XTransLayout = 0x0131'u16
  WbGrgbLevels = 0x2FF0'u16
  RawExposureBias = 0x9650'u16
  FujiIfd = 0xF000'u16
  RawWidth = 0xF001'u16
  RawHeight = 0xF002'u16
  RawBits = 0xF003'u16
  StripOffset = 0xF007'u16
  StripByteCount = 0xF008'u16
  RafBlackLevel = 0xF00A'u16
  WbGrbLevels = 0xF00E'u16
  MaxRecords = 512

type
  Packing* = enum
    pkU16Le, pkU16Be     ## 16-bit containers in the given byte order
    pkLsbFirst           ## little-endian bit stream, LSB first (12-bit bodies)
    pkWords32MsbFirst    ## little-endian 32-bit words, MSB first (14-bit bodies)

  Record = tuple[tag: uint16, at, len: int]

proc hasMagic*(bytes: openArray[byte]): bool =
  if bytes.len < RafMagic.len: return false
  for i, c in RafMagic:
    if bytes[i] != byte(c): return false
  true

proc be32(b: openArray[byte]; at: int): Option[int] =
  if at < 0 or at + 4 > b.len: return
  some(int((uint32(b[at]) shl 24) or (uint32(b[at + 1]) shl 16) or (uint32(b[at + 2]) shl 8) or uint32(b[at + 3])))

proc be16(b: openArray[byte]; at: int): Option[uint16] =
  if at < 0 or at + 2 > b.len: return
  some((uint16(b[at]) shl 8) or uint16(b[at + 1]))

proc rafPreviewRange*(bytes: openArray[byte]): Option[(int, int)] =
  ## Offset and length of the preview JPEG (header bytes 84 and 88).
  let (a, b) = (be32(bytes, 84), be32(bytes, 88))
  if a.isSome and b.isSome: result = some((a.get, b.get))

proc records(bytes: openArray[byte]): seq[Record] =
  ## The metadata directory's records as (tag, absolute offset, length).
  let (at0, len0) = (be32(bytes, 92), be32(bytes, 96))
  if at0.isNone or len0.isNone: return
  let (at, len) = (at0.get, len0.get)
  if at + len > bytes.len: return
  template dir: untyped = bytes.toOpenArray(at, at + len - 1)
  let n = min(be32(dir, 0).get(0), MaxRecords)
  var p = 4
  for _ in 0 ..< n:
    let (tag, size) = (be16(dir, p), be16(dir, p + 2))
    if tag.isNone or size.isNone: break
    let start = p + 4
    if start + int(size.get) > dir.len: break
    result.add (tag.get, at + start, int(size.get))
    p = start + int(size.get)

proc record(recs: openArray[Record]; tag: uint16): Option[(int, int)] =
  for r in recs:
    if r.tag == tag: return some((r.at, r.len))

proc recordBytes(bytes: openArray[byte]; recs: openArray[Record]; tag: uint16): Option[seq[byte]] =
  let r = record(recs, tag)
  if r.isSome: result = some(@(bytes.toOpenArray(r.get[0], r.get[0] + r.get[1] - 1)))

proc pair(bytes: openArray[byte]; recs: openArray[Record]; tag: uint16): Option[(int, int)] =
  ## A record holding two big-endian 16-bit values.
  let r = record(recs, tag)
  if r.isNone: return
  template d: untyped = bytes.toOpenArray(r.get[0], r.get[0] + r.get[1] - 1)
  let (a, b) = (be16(d, 0), be16(d, 2))
  if a.isSome and b.isSome: result = some((int(a.get), int(b.get)))

proc layoutCfa*(xtrans, fuji: Option[seq[byte]]): Option[Cfa] =
  ## The CFA from XTransLayout (6×6) or FujiLayout (2×2), both stored reversed.
  if xtrans.isSome and xtrans.get.len == 36:
    var colors: seq[uint8]
    for k in countdown(35, 0):
      let c = xtrans.get[k]
      if c > 2: return none(Cfa)
      colors.add c
    return some(Cfa(width: 6, height: 6, colors: colors))
  if fuji.isNone or fuji.get.len != 4: return
  var colors: seq[uint8]
  for k in countdown(3, 0):
    let c = fuji.get[k]
    colors.add(if (c and 3) == 3: 1'u8 else: c and 3)
  let cfa = Cfa(width: 2, height: 2, colors: colors)
  if cfa.isBayer: result = some(cfa)

proc packedSample(src: openArray[byte]; i, bits: int; packing: Packing): uint16 {.inline.} =
  ## Reads `bits`-bit (≤ 16) sample `i` of a packed stream (bytes past the end read as 0).
  template at(k: int): uint32 = (if k >= 0 and k < src.len: uint32(src[k]) else: 0'u32)
  let pos = i * bits
  let (byt, shift) = (pos div 8, pos mod 8)
  let mask = (1'u32 shl min(bits, 16)) - 1
  case packing
  of pkLsbFirst:
    var w = 0'u32
    for k in 0 .. 3: w = w or (at(byt + k) shl (8 * k))
    uint16((w shr shift) and mask)
  of pkWords32MsbFirst:
    # Logical byte k is physical byte k with its position in the 32-bit word reversed.
    var w = 0'u32
    for k in 0 .. 3:
      let l = byt + k
      w = (w shl 8) or at((l and not 3) or (3 - (l and 3)))
    uint16((w shr (32 - shift - min(bits, 16))) and mask)
  of pkU16Le: uint16(at(2 * i) or (at(2 * i + 1) shl 8))
  of pkU16Be: uint16((at(2 * i) shl 8) or at(2 * i + 1))

proc unpackRaf*(src: openArray[byte]; width, height, bits: int; packing: Packing): seq[uint16] =
  ## Unpacks a `width × height` strip.
  result = newSeq[uint16](width * height)
  for k in 0 ..< result.len: result[k] = packedSample(src, k, bits, packing)

proc previewExif(bytes: openArray[byte]): (Option[uint16], Option[string], Option[string]) =
  ## Orientation, make and model from the preview JPEG's EXIF (APP1) block.
  let r = rafPreviewRange(bytes)
  if r.isNone: return
  let (off, len) = r.get
  if off < 0 or len < 0 or off + len > bytes.len or len < 12: return
  let j = off
  if bytes[j] != 0xFF or bytes[j + 1] != 0xD8 or bytes[j + 2] != 0xFF or bytes[j + 3] != 0xE1: return
  for k, c in "Exif\0\0":
    if bytes[j + 6 + k] != byte(c): return
  let t = newTiff(bytes.toOpenArray(j + 12, off + len - 1))
  if t.isNone: return
  let ifd0 = t.get.ifdAt(t.get.firstIfd, 0)
  if ifd0.isNone: return
  let o = t.get.tagUint(ifd0.get, TagOrientation)
  let orient = if o.isSome and o.get in 1'u32 .. 8'u32: some(uint16(o.get)) else: none(uint16)
  (orient, t.get.tagAscii(ifd0.get, TagMake), t.get.tagAscii(ifd0.get, TagModel))

proc headerModel(bytes: openArray[byte]): Option[string] =
  ## The model name in the RAF header (bytes 28..60).
  if bytes.len < 60: return
  var stop = 60
  for k in 28 ..< 60:
    if bytes[k] == 0:
      stop = k
      break
  let s = lossyUtf8(bytes.toOpenArray(28, stop - 1)).strip
  if s.len > 0: result = some(s)

proc decodeRaf*(bytes: openArray[byte]; limits: RawLimits): Sensor =
  let cfaAt0 = be32(bytes, 100)
  if cfaAt0.isNone: raise malformed("RAF header is truncated")
  let cfaAt = cfaAt0.get
  let cfaLen = be32(bytes, 104).get(0)
  if cfaAt > bytes.len: raise malformed("RAF CFA region lies outside the file")
  var regionLen = bytes.len - cfaAt
  if cfaLen > 0 and cfaLen <= regionLen: regionLen = cfaLen
  let tt = newTiff(bytes.toOpenArray(cfaAt, cfaAt + regionLen - 1))
  if tt.isNone: raise unsupported("Fujifilm RAF of an early FinePix / SuperCCD body (no CFA TIFF) is not decoded yet")
  let t = tt.get
  let ifd0 = t.ifdAt(t.firstIfd, 0)
  if ifd0.isNone: raise malformed("RAF CFA TIFF has no IFD0")
  let fujiAt = t.tagUint(ifd0.get, FujiIfd)
  if fujiAt.isNone: raise unsupported("RAF without a Fuji raw IFD")
  let fuji0 = t.ifdAt(int(fujiAt.get), 0)
  if fuji0.isNone: raise malformed("RAF Fuji raw IFD is unreadable")
  let fuji = fuji0.get
  let w0 = t.tagUint(fuji, RawWidth)
  if w0.isNone: raise malformed("RAF has no raw width")
  let h0 = t.tagUint(fuji, RawHeight)
  if h0.isNone: raise malformed("RAF has no raw height")
  let (width, height) = (int(w0.get), int(h0.get))
  let bits = int(t.tagUint(fuji, RawBits).get(14))
  if bits notin 8 .. 16: raise unsupported("RAF with " & $bits & "-bit samples")
  limits.check(uint64(width), uint64(height), 2)
  let off0 = t.tagUint(fuji, StripOffset)
  if off0.isNone: raise malformed("RAF has no strip offset")
  let offset = int(off0.get)
  let count = int(t.tagUint(fuji, StripByteCount).get(0))
  if offset > t.data.len: raise malformed("RAF raw data lies outside the file")
  var srcLen = t.data.len - offset
  if count > 0 and count <= srcLen: srcLen = count
  let pixels = width * height
  let packed = (pixels * bits + 7) div 8
  var packing: Packing
  if count >= pixels * 2: packing = if t.le: pkU16Le else: pkU16Be
  elif count >= packed and bits == 12: packing = pkLsbFirst
  elif count >= packed and bits == 14: packing = pkWords32MsbFirst
  elif count >= packed: raise unsupported("Fujifilm RAF with packed " & $bits & "-bit samples")
  else: raise unsupported("Fujifilm compressed RAF is not decoded (no public description of the format)")
  let need = if packing in {pkU16Le, pkU16Be}: pixels * 2 else: packed
  if srcLen < need: raise malformed("RAF raw data is truncated")
  let data = unpackRaf(t.data.toOpenArray(offset, offset + srcLen - 1), width, height, bits, packing)
  let recs = records(bytes)
  template recBytes(tag: uint16): Option[seq[byte]] = recordBytes(bytes, recs, tag)
  let cfa = layoutCfa(recBytes(XTransLayout), recBytes(FujiLayout))
  if cfa.isNone: raise unsupported("Fujifilm RAF without a recognised CFA layout")
  let full = rawRect(0, 0, width, height)
  var warnings: seq[string]
  let bl = t.tagFloats(fuji, RafBlackLevel)
  var black: BlackLevels
  var anyNeg = false
  for v in bl:
    if v < 0: anyNeg = true
  if bl.len == 0 or anyNeg:
    warnings.add "RAF: black level not recorded; assumed 0"
    black = uniformBlack(0)
  else:
    # One value per CFA position; the order is not established and the values differ by at most
    # a couple of units, so use their mean.
    var s = -0.0
    for v in bl: s += v
    black = uniformBlack(float32(s / float64(bl.len)))
  let white = clipLevel(data, bits)
  var cameraWb = none(array[3, float64])
  let grb = t.tagFloats(fuji, WbGrbLevels)
  if grb.len >= 3 and grb[0] > 0: cameraWb = some([grb[1] / grb[0], 1.0, grb[2] / grb[0]])
  else:
    let d = recBytes(WbGrgbLevels)
    if d.isSome:
      var v: seq[float64]
      for k in 0 .. 3:
        let x = be16(d.get, 2 * k)
        if x.isNone: break
        v.add float64(x.get)
      if v.len == 4 and v[0] + v[2] > 0:
        cameraWb = some([2.0 * v[1] / (v[0] + v[2]), 1.0, 2.0 * v[3] / (v[0] + v[2])])
  if cameraWb.isSome:
    for x in cameraWb.get:
      if not (x >= 0.25 and x < 8.0):
        cameraWb = none(array[3, float64])
        break
  var crop = full
  let (tl, cs) = (pair(bytes, recs, CropTopLeft), pair(bytes, recs, CroppedSize))
  if tl.isSome and cs.isSome:
    crop = rawRect(tl.get[1], tl.get[0], cs.get[1], cs.get[0]).intersect(full)
  if crop.isEmpty: crop = full
  # RawExposureBias: how far the raw was exposed below the camera's rendering (dynamic-range
  # modes); undone like a DNG BaselineExposure.
  var baselineExposure = 0.0
  let eb = recBytes(RawExposureBias)
  if eb.isSome:
    let (n, den) = (be16(eb.get, 0), be16(eb.get, 2))
    if n.isSome and den.isSome and cast[int16](den.get) != 0:
      let v = -float64(cast[int16](n.get)) / float64(cast[int16](den.get))
      if v == v and abs(v) <= 8.0: baselineExposure = v
  let (orient, make, model) = previewExif(bytes)
  Sensor(format: rfRaf, make: (if make.isSome: make else: some("FUJIFILM")),
         model: (if model.isSome: model else: headerModel(bytes)), width: width, height: height, samples: 1,
         data: data, cfa: cfa, black: black, white: [white, white, white], active: full, crop: crop,
         cameraWb: cameraWb, orientation: orient.get(1), baselineExposure: baselineExposure, warnings: warnings)
