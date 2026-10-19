## TIFF/EP-structured raws (NEF, ARW, PEF and others) (port of `raw/src/tiffep.rs`) whose sensor
## data is stored uncompressed or as lossless JPEG in a standard CFA IFD (TIFF/EP, ISO 12234-2),
## plus Sony's compressed ARW (`sony`) and Nikon's Huffman-compressed NEF (`nefc`). Other
## vendor-specific compressions are reported as unsupported.
##
## Black and white levels use DNG-style tags when the camera writes them, then the vendor's
## publicly documented tags (Nikon maker note BlackLevel, Sony raw-IFD BlackLevel), else 0 with a
## warning — cRAW files without a BlackLevel tag get the 512 that their tone curve gives the black
## code. The white level falls back to the clipping point found in the data. The as-shot white
## balance comes from the documented vendor tags (Nikon WB_RBLevels, Sony WB_RGGBLevels).

import std/[options, strutils, math]
import ./tiff, ./sensor, ./cr2, ./nefc, ./sony, ./cameras

const
  PhotometricCfa = 32803'u32
  NikonWbRbLevels = 0x000C'u16
  NikonBlackLevel = 0x003D'u16
  SonyBlackLevel = 0x7310'u16
  SonyWbRggbLevels = 0x7313'u16

proc pixelsOf(t: Tiff; i: Ifd): uint64 =
  uint64(t.tagUint(i, TagImageWidth).get(0)) * uint64(t.tagUint(i, TagImageLength).get(0))

proc rawIfd(t: Tiff; ifds: openArray[Ifd]): Option[Ifd] =
  ## The CFA IFD with the most pixels (the last of equal maxima, like `max_by_key`).
  var best = -1
  for k, i in ifds:
    if t.tagUint(i, TagPhotometric) == some(PhotometricCfa):
      if best < 0 or pixelsOf(t, i) >= pixelsOf(t, ifds[best]): best = k
  if best >= 0: result = some(ifds[best])

proc isSonyNonCfa*(t: Tiff; ifds: openArray[Ifd]): bool =
  ## Sony ARW files whose raw IFD is not CFA data (the reduced-size lossless "M" / "S" YCbCr
  ## variants): recognised, but not decoded.
  var hasType, sonyMake = false
  for i in ifds:
    if i.has(SonyRawFileType): hasType = true
    let m = t.tagAscii(i, TagMake)
    if m.isSome and m.get.toUpperAscii.startsWith("SONY"): sonyMake = true
  hasType and sonyMake

proc readU16(bb: openArray[byte]; le: bool; o: int): Option[int] =
  if o + 2 > bb.len: return
  let (a, c) = (int(bb[o]), int(bb[o + 1]))
  some(if le: a or (c shl 8) else: (a shl 8) or c)

proc tiffEpCfa*(t: Tiff; raw: Ifd; ifds: openArray[Ifd]): Option[Cfa] =
  ## CFA from the TIFF/EP tags of the raw IFD, or the EXIF CFAPattern.
  let dim = t.tagUints(raw, TagCfaRepeatPatternDim)
  let pat = t.tagUints(raw, TagCfaPattern)
  var rows, cols: int
  var colors: seq[uint32]
  if dim.len == 2 and pat.len == int(dim[0]) * int(dim[1]) and pat.len > 0:
    (rows, cols, colors) = (int(dim[0]), int(dim[1]), pat)
  else:
    # EXIF CFAPattern: two 16-bit counts (columns, rows) then the colours.
    var e = none(Entry)
    for i in ifds:
      let x = i.get(TagExifCfaPattern)
      if x.isSome:
        e = x
        break
    if e.isNone: return
    let b = t.rawBytes(e.get)
    if b.isNone: return
    let bb = b.get
    template rd(o: int): Option[int] = readU16(bb, t.le, o)
    let (c, r) = (rd(0), rd(2))
    if c.isNone or r.isNone: return
    (rows, cols) = (r.get, c.get)
    if 4 + rows * cols > bb.len: return
    for k in 0 ..< rows * cols: colors.add uint32(bb[4 + k])
  if rows == 0 or cols == 0 or rows > 16 or cols > 16: return
  var cs: seq[uint8]
  for c in colors:
    if c > 2: return
    cs.add uint8(c)
  some(Cfa(width: cols, height: rows, colors: cs))

proc nikonMakerNote(t: Tiff; ifds: openArray[Ifd]): Option[(Tiff, Ifd)] =
  ## Nikon maker note: "Nikon\0", version, then an embedded TIFF header whose offsets are relative
  ## to itself.
  var e = none(Entry)
  for i in ifds:
    let x = i.get(TagMakerNote)
    if x.isSome:
      e = x
      break
  if e.isNone: return
  let r = t.rawRange(e.get)
  if r.isNone: return
  let (at, len) = r.get
  if len < 6: return
  for k, c in "Nikon\0":
    if t.data[at + k] != byte(c): return
  let base = e.get.at + 10
  if base > t.data.len: return
  let inner = newTiff(t.data.toOpenArray(base, t.data.len - 1))
  if inner.isNone: return
  let ifd = inner.get.ifdAt(inner.get.firstIfd, 0)
  if ifd.isNone: return
  some((inner.get, ifd.get))

proc rggbByPosition*(cfa: Cfa; v: openArray[float64]): Option[array[4, float32]] =
  ## Four levels listed in R, G, G, B order, placed at their 2×2 CFA positions (data parity).
  if v.len != 4: return
  for x in v:
    if x != x or x == Inf or x == NegInf or x < 0: return
  let (r, g1, g2, b) = (v[0], v[1], v[2], v[3])
  var greens = 0
  var outp: array[4, float32]
  for k in 0 .. 3:
    outp[k] = case cfa.color(k and 1, k shr 1)
      of 0: float32(r)
      of 2: float32(b)
      else:
        inc greens
        float32(if greens == 1: g1 elif greens == 2: g2 else: g1)
  some(outp)

proc isPadding(data: openArray[uint16]; w, h: int; fill: uint16; x: int): bool =
  var c = 0
  for y in 0 ..< h:
    let i = y * w + x
    if i < data.len and data[i] == fill: inc c
  c * 10 >= h * 9

proc paddedWidth*(data: openArray[uint16]; w, h, bits: int): int =
  ## Width without the trailing padding columns some Nikon bodies append (at most 128), kept even
  ## for the CFA phase. A padding column holds the last column's value in at least 90% of its rows,
  ## and that value is dark (below a quarter of `2^bits`).
  if w < 256 or h < 2 or data.len < w * h: return w
  let fi = (h div 2) * w + w - 1
  if fi >= data.len: return w
  let fill = data[fi]
  if uint32(fill) >= (1'u32 shl min(bits, 16)) div 4: return w
  template padding(x: int): bool = isPadding(data, w, h, fill, x)
  var aw = w
  while aw > w - 128 and padding(aw - 1): dec aw
  aw and not 1

proc decodeTiffEp*(t: Tiff; format: RawFormat; limits: RawLimits): Sensor =
  let ifds = t.allIfds
  let raw0 = rawIfd(t, ifds)
  if raw0.isNone:
    if isSonyNonCfa(t, ifds): raise unsupported("Sony reduced-size (M / S) lossless ARW")
    raise unsupported($format & ": no CFA image found")
  let raw = raw0.get
  let nikon = if format == rfNef: nikonMakerNote(t, ifds) else: none((Tiff, Ifd))
  let craw = format == rfArw and isCraw(t, raw)
  let plane =
    if craw: readCraw(t, raw, limits)
    elif format == rfNef and t.tagUint(raw, TagCompression) == some(NikonCompression): readCompressed(t, raw, nikon, limits)
    else: readPlane(t, raw, limits, jlQuads)
  if plane.samples != 1: raise unsupported("CFA data with " & $plane.samples & " samples per pixel")
  let cfa = tiffEpCfa(t, raw, ifds)
  if cfa.isNone: raise unsupported($format & ": CFA pattern not recorded")
  if not cfa.get.isBayer: raise unsupported($cfa.get.height & "x" & $cfa.get.width & " non-Bayer CFA")
  var warnings: seq[string]
  # Nikon pads some sensor rows on the right; they are not image data.
  let width = if format == rfNef: paddedWidth(plane.data, plane.width, plane.height, plane.bits) else: plane.width
  let full = rawRect(0, 0, width, plane.height)
  var make, model: Option[string]
  for i in ifds:
    if make.isNone: make = t.tagAscii(i, TagMake)
    if model.isNone: model = t.tagAscii(i, TagModel)
  let bl = t.tagFloats(raw, TagBlackLevel)
  var vendorBlack: seq[float64]
  if nikon.isSome:
    # Nikon's BlackLevel is in 14-bit units whatever the sample depth.
    for v in nikon.get[0].tagFloats(nikon.get[1], NikonBlackLevel): vendorBlack.add(if plane.bits == 12: v / 4.0 else: v)
  else: vendorBlack = t.tagFloats(raw, SonyBlackLevel)
  var black: BlackLevels
  let byPos = rggbByPosition(cfa.get, vendorBlack)
  if bl.len > 0: black = uniformBlack(float32(bl[0]))
  elif byPos.isSome: black = BlackLevels(rows: 2, cols: 2, values: @(byPos.get))
  elif craw:
    # First-generation cRAW files carry no BlackLevel tag; the tone curve maps the black code to
    # 512, so their decoded data has a 512 black level.
    black = uniformBlack(512)
  elif nikon.isSome:
    # Nikon bodies that write no BlackLevel tag subtract the black level in camera.
    black = uniformBlack(0)
  else:
    warnings.add $format & ": black level not recorded in a documented tag; assumed 0"
    black = uniformBlack(0)
  let wl = t.tagFloats(raw, TagWhiteLevel)
  let white = if wl.len > 0 and wl[0] > 0: float32(wl[0]) else: clipLevel(plane.data, plane.bits)
  var cameraWb = none(array[3, float64])
  if nikon.isSome:
    let v = nikon.get[0].tagFloats(nikon.get[1], NikonWbRbLevels)
    if v.len >= 2: cameraWb = some([v[0], 1.0, v[1]])
  else:
    let v = t.tagFloats(raw, SonyWbRggbLevels)
    if v.len == 4 and v[1] + v[2] > 0: cameraWb = some([2.0 * v[0] / (v[1] + v[2]), 1.0, 2.0 * v[3] / (v[1] + v[2])])
  if cameraWb.isSome:
    for x in cameraWb.get:
      if not (x >= 0.25 and x < 8.0):
        cameraWb = none(array[3, float64])
        break
  # DNG-style DefaultCropOrigin / DefaultCropSize (some vendors write them in the raw IFD).
  var crop = full
  let (o, s) = (t.tagFloats(raw, TagDefaultCropOrigin), t.tagFloats(raw, TagDefaultCropSize))
  if o.len == 2 and s.len == 2:
    proc f(v: float64): int = (if v == v and v != Inf and v >= 0: int(min(round(v), 9.2e18)) else: 0)
    let r = rawRect(f(o[0]), f(o[1]), f(s[0]), f(s[1])).intersect(full)
    if not r.isEmpty: crop = r
  let ifd0 = t.ifdAt(t.firstIfd, 0)
  result = Sensor(format: format, make: make, model: model, width: plane.width, height: plane.height,
                  samples: 1, data: plane.data, cfa: cfa, black: black, white: [white, white, white],
                  active: full, crop: crop, cameraWb: cameraWb,
                  orientation: orientation(if ifd0.isSome: t.tagUint(ifd0.get, TagOrientation) else: none(uint32)),
                  warnings: warnings)
  applyCameraProfile(result)
