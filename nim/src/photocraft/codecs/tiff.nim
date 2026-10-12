## TIFF 6.0 and BigTIFF, written from the specification: reads strips or tiles, chunky or
## planar, compression none / LZW / Deflate / PackBits, horizontal and floating-point predictors,
## 1–16-bit integer and 32-bit float samples, gray (min-is-black/white), RGB, palette and CMYK
## with extra (alpha) samples; ICC (34675), XMP (700), resolution, text tags, and Photoshop's
## image resources (34377) and layer data (37724) as opaque metadata. Writes little-endian
## classic TIFF (BigTIFF past 4 GiB or on request).

import std/[options, strutils, math]
from std/unicode import validateUtf8
import ./image, ./meta, ../zlib

const
  TagNewSubfileType = 254
  TagWidth = 256
  TagHeight = 257
  TagBits = 258
  TagCompression = 259
  TagPhotometric = 262
  TagImageDescription = 270
  TagMake = 271
  TagModel = 272
  TagStripOffsets = 273
  TagOrientation = 274
  TagSamples = 277
  TagRowsPerStrip = 278
  TagStripByteCounts = 279
  TagXRes = 282
  TagYRes = 283
  TagPlanar = 284
  TagResUnit = 296
  TagSoftware = 305
  TagDateTime = 306
  TagArtist = 315
  TagPredictor = 317
  TagColorMap = 320
  TagTileWidth = 322
  TagTileLength = 323
  TagTileOffsets = 324
  TagTileByteCounts = 325
  TagSubIfds = 330
  TagExtraSamples = 338
  TagSampleFormat = 339
  TagXmp = 700
  TagCopyright = 33432
  TagPhotoshop = 34377
  TagIcc = 34675
  TagImageSourceData = 37724
  TextTags = [(TagImageDescription, "ImageDescription"), (TagMake, "Make"), (TagModel, "Model"), (TagSoftware, "Software"),
               (TagDateTime, "DateTime"), (TagArtist, "Artist"), (TagCopyright, "Copyright")]

type
  Entry = object
    tag, ty: int
    count: int64
    valueAt: int64  # offset of the value bytes (inline or remote)
  Ifd = object
    at: int64
    entries: seq[Entry]
    next: int64

  TiffFile = object
    order: Order
    big: bool

  TiffPageKind* = enum tpPage = "page", tpReduced = "reduced", tpMask = "mask"
  TiffPage* = object
    index*: int
    width*, height*: int
    kind*: TiffPageKind
    samples*, bits*: int
    compression*: int
    tiled*: bool
  TiffInfo* = object
    pages*: seq[TiffPage]
    defaultPage*: int

proc err(msg: string): ref CodecError = malformed(fmtTiff, msg)

proc typeSize(ty: int): int =
  case ty
  of 1, 2, 6, 7: 1
  of 3, 8: 2
  of 4, 9, 11, 13: 4
  of 5, 10, 12, 16, 17, 18: 8
  else: 0

proc parseHeader(b: openArray[byte]): (TiffFile, int64) =
  if b.len < 8: raise err("not a TIFF file")
  var f: TiffFile
  if b[0] == byte('I') and b[1] == byte('I'): f.order = oLittle
  elif b[0] == byte('M') and b[1] == byte('M'): f.order = oBig
  else: raise err("not a TIFF file")
  let magic = rd16(f.order, b, 2).get(0)
  if magic == 42:
    (f, rd32(f.order, b, 4).get(0))
  elif magic == 43:
    f.big = true
    (f, rd64(f.order, b, 8).get(0))
  else: raise err("not a TIFF file")

proc readIfd(f: TiffFile; b: openArray[byte]; at: int64): Ifd =
  if at < 8 or at >= b.len: raise err("IFD offset out of range")
  result.at = at
  let a = int(at)
  let count = if f.big: rd64(f.order, b, a).get(-1) else: int64(rd16(f.order, b, a).get(-1))
  if count < 0 or count > 100_000: raise err("bad IFD")
  let eb = if f.big: 20 else: 12
  let first = a + (if f.big: 8 else: 2)
  if first + int(count) * eb > b.len: raise err("IFD is truncated")
  for i in 0 ..< int(count):
    let e = first + i * eb
    let tag = rd16(f.order, b, e).get(0)
    let ty = rd16(f.order, b, e + 2).get(0)
    let cnt = if f.big: rd64(f.order, b, e + 4).get(0) else: rd32(f.order, b, e + 4).get(0)
    let size = int64(typeSize(ty)) * cnt
    let inline = if f.big: 8'i64 else: 4'i64
    let field = e + (if f.big: 12 else: 8)
    let valueAt = if size <= inline: int64(field) else: (if f.big: rd64(f.order, b, field).get(-1) else: rd32(f.order, b, field).get(-1))
    result.entries.add Entry(tag: tag, ty: ty, count: cnt, valueAt: valueAt)
  let nextAt = first + int(count) * eb
  result.next = if f.big: rd64(f.order, b, nextAt).get(0) else: rd32(f.order, b, nextAt).get(0)

proc find(d: Ifd; tag: int): Option[Entry] =
  for e in d.entries:
    if e.tag == tag: return some(e)
  none(Entry)

proc values(f: TiffFile; b: openArray[byte]; e: Entry; maxCount = 1 shl 24): seq[int64] =
  if e.count > maxCount or e.valueAt < 0: return
  let sz = typeSize(e.ty)
  if sz == 0 or e.valueAt + int64(sz) * e.count > b.len: return
  let at = int(e.valueAt)
  for i in 0 ..< int(e.count):
    let p = at + i * sz
    let v = case e.ty
      of 1, 2, 7: int64(b[p])
      of 6: int64(cast[int8](b[p]))
      of 3: int64(rd16(f.order, b, p).get(0))
      of 8: int64(cast[int16](uint16(rd16(f.order, b, p).get(0))))
      of 4, 13: rd32(f.order, b, p).get(0)
      of 9: int64(cast[int32](uint32(rd32(f.order, b, p).get(0))))
      of 16, 18: rd64(f.order, b, p).get(0)
      of 5: (let n = rd32(f.order, b, p).get(0); let d = rd32(f.order, b, p + 4).get(1); if d == 0: 0'i64 else: n div d)
      else: 0'i64
    result.add v

proc value(f: TiffFile; b: openArray[byte]; d: Ifd; tag: int; default: int64): int64 =
  let e = d.find(tag)
  if e.isNone: return default
  let v = values(f, b, e.get, 64)
  if v.len == 0: default else: v[0]

proc rationalValue(f: TiffFile; b: openArray[byte]; d: Ifd; tag: int): Option[float64] =
  let e = d.find(tag)
  if e.isNone or e.get.ty != 5 or e.get.count < 1 or e.get.valueAt < 0: return
  let n = rd32(f.order, b, int(e.get.valueAt))
  let dd = rd32(f.order, b, int(e.get.valueAt) + 4)
  if n.isNone or dd.isNone or dd.get == 0: return
  some(float64(n.get) / float64(dd.get))

proc rawBytes(b: openArray[byte]; e: Entry): Option[seq[byte]] =
  let size = int64(typeSize(e.ty)) * e.count
  if e.valueAt < 0 or size < 0 or e.valueAt + size > b.len or size > (1 shl 30): return
  some(@(b.toOpenArray(int(e.valueAt), int(e.valueAt + size) - 1)))

# ---------------------------------------------------------------- decompression

proc lzwDecodeTiff(src: openArray[byte]; expected: int): seq[byte] =
  ## TIFF LZW (MSB-first codes, early change).
  result = newSeqOfCap[byte](expected)
  var table = newSeq[seq[byte]](4096)
  for i in 0 ..< 256: table[i] = @[byte(i)]
  var next = 258
  var width = 9
  var bitPos = 0
  var prev: seq[byte]
  let nbits = src.len * 8
  while bitPos + width <= nbits:
    var code = 0
    for k in 0 ..< width:
      let bp = bitPos + k
      code = (code shl 1) or ((int(src[bp shr 3]) shr (7 - (bp and 7))) and 1)
    bitPos += width
    if code == 257: break
    if code == 256:
      next = 258
      width = 9
      prev = @[]
      continue
    var entry: seq[byte]
    if code < next and (code < 256 or table[code].len > 0):
      entry = table[code]
    elif code == next and prev.len > 0:
      entry = prev & prev[0]
    else: break
    result.add entry
    if prev.len > 0 and next < 4096:
      table[next] = prev & entry[0]
      inc next
    prev = entry
    if next + 1 >= (1 shl width) and width < 12: inc width
    if result.len >= expected: break

proc packbitsDecode(src: openArray[byte]; expected: int): seq[byte] =
  result = newSeqOfCap[byte](expected)
  var p = 0
  while p < src.len and result.len < expected:
    let n = int(cast[int8](src[p]))
    inc p
    if n >= 0:
      for k in 0 .. n:
        if p + k < src.len: result.add src[p + k]
      p += n + 1
    elif n != -128:
      if p < src.len:
        for _ in 0 .. -n: result.add src[p]
      inc p

proc packbitsEncode*(src: openArray[byte]): seq[byte] =
  var i = 0
  while i < src.len:
    var run = 1
    while i + run < src.len and run < 128 and src[i + run] == src[i]: inc run
    if run >= 2:
      result.add byte(257 - run)
      result.add src[i]
      i += run
    else:
      var j = i + 1
      while j < src.len and j - i < 128 and not (j + 1 < src.len and src[j] == src[j + 1]): inc j
      result.add byte(j - i - 1)
      result.add src.toOpenArray(i, j - 1)
      i = j

proc lzwEncodeTiff*(src: openArray[byte]): seq[byte] =
  var acc: uint64 = 0
  var nb = 0
  var width = 9
  template emit(code: int) =
    acc = (acc shl width) or uint64(code)
    nb += width
    while nb >= 8:
      result.add byte((acc shr (nb - 8)) and 0xFF)
      nb -= 8
  var dict = newSeq[array[256, int16]](4096)
  for d in dict.mitems:
    for v in d.mitems: v = -1
  var next = 258
  emit(256)
  if src.len == 0:
    emit(257)
    if nb > 0: result.add byte((acc shl (8 - nb)) and 0xFF)
    return
  var cur = int(src[0])
  for i in 1 ..< src.len:
    let k = int(src[i])
    let nx = dict[cur][k]
    if nx >= 0: cur = int(nx)
    else:
      emit(cur)
      dict[cur][k] = int16(next)
      inc next
      if next == 4094:
        emit(256)
        for d in dict.mitems:
          for v in d.mitems: v = -1
        next = 258
        width = 9
      elif next >= (1 shl width) and width < 12: inc width
      cur = k
  emit(cur)
  if next + 1 >= (1 shl width) and width < 12: inc width
  emit(257)
  if nb > 0: result.add byte((acc shl (8 - nb)) and 0xFF)

proc decompress(comp: int; src: openArray[byte]; expected: int): seq[byte] =
  case comp
  of 1: @src
  of 5: lzwDecodeTiff(src, expected)
  of 8, 32946:
    try: zlibDecompress(src, maxOut = expected + 1024, checkAdler = false)
    except ZlibError as e: raise err("deflate: " & e.msg)
  of 32773: packbitsDecode(src, expected)
  else: raise unsupported(fmtTiff, "compression " & $comp)

# ---------------------------------------------------------------- page listing

proc collectIfds(f: TiffFile; b: openArray[byte]; first: int64): seq[(Ifd, bool)] =
  var at = first
  var seen = 0
  while at != 0 and seen < 10_000:
    let d = readIfd(f, b, at)
    result.add (d, false)
    let sub = d.find(TagSubIfds)
    if sub.isSome:
      for off in values(f, b, sub.get, 64):
        try: result.add (readIfd(f, b, off), true)
        except CodecError: discard
    at = d.next
    inc seen

proc tiffInfo*(b: openArray[byte]): TiffInfo =
  ## Lists the pages (IFDs and SubIFDs) without decoding pixels.
  let (f, first) = parseHeader(b)
  result.defaultPage = -1
  for i, (d, _) in collectIfds(f, b, first):
    let nst = value(f, b, d, TagNewSubfileType, 0)
    let kind = if (nst and 4) != 0: tpMask elif (nst and 1) != 0: tpReduced else: tpPage
    let p = TiffPage(index: i, width: int(value(f, b, d, TagWidth, 0)), height: int(value(f, b, d, TagHeight, 0)), kind: kind,
                     samples: int(value(f, b, d, TagSamples, 1)), bits: int(value(f, b, d, TagBits, 1)),
                     compression: int(value(f, b, d, TagCompression, 1)), tiled: d.find(TagTileOffsets).isSome)
    result.pages.add p
    if result.defaultPage < 0 and kind == tpPage: result.defaultPage = i
  if result.defaultPage < 0: result.defaultPage = 0

# ---------------------------------------------------------------- decode

proc decodeIfd(f: TiffFile; b: openArray[byte]; d: Ifd; limits: Limits): Image =
  let w = int(value(f, b, d, TagWidth, 0))
  let h = int(value(f, b, d, TagHeight, 0))
  if w <= 0 or h <= 0: raise err("zero-sized image")
  let spp = int(value(f, b, d, TagSamples, 1))
  let bitsE = d.find(TagBits)
  var bits = 1
  if bitsE.isSome:
    let bv = values(f, b, bitsE.get, 64)
    if bv.len > 0: bits = int(bv[0])
  let comp = int(value(f, b, d, TagCompression, 1))
  let photo = int(value(f, b, d, TagPhotometric, (if spp >= 3: 2 else: 1)))
  let planar = int(value(f, b, d, TagPlanar, 1))
  let predictor = int(value(f, b, d, TagPredictor, 1))
  let sfmt = int(value(f, b, d, TagSampleFormat, 1))
  if spp < 1 or spp > 16: raise unsupported(fmtTiff, $spp & " samples per pixel")
  if bits notin [1, 2, 4, 8, 16, 32]: raise unsupported(fmtTiff, $bits & "-bit samples")
  if bits == 32 and sfmt != 3: raise unsupported(fmtTiff, "32-bit integer samples")
  if photo notin [0, 1, 2, 3, 5]: raise unsupported(fmtTiff, "photometric interpretation " & $photo)
  let colorCh = case photo
    of 0, 1, 3: 1
    of 5: 4
    else: 3
  if spp < colorCh: raise err("too few samples for the colour model")
  var extraAlpha = false
  let es = d.find(TagExtraSamples)
  if spp > colorCh:
    if es.isSome:
      let ev = values(f, b, es.get, 16)
      extraAlpha = ev.len > 0 and ev[0] in [1'i64, 2]
    else: extraAlpha = spp == colorCh + 1
  let outSample = if bits == 32: isF32 elif bits == 16: isU16 else: isU8
  let layout = case photo
    of 0, 1: (if extraAlpha: clGrayA else: clGray)
    of 3: (if extraAlpha: clRgba else: clRgb)
    of 5: (if extraAlpha: clCmykA else: clCmyk)
    else: (if extraAlpha: clRgba else: clRgb)
  limits.check(w, h, layout, outSample)
  # chunk geometry
  let tiled = d.find(TagTileOffsets).isSome
  var cw, ch: int
  var offsE, cntE: Option[Entry]
  if tiled:
    cw = int(value(f, b, d, TagTileWidth, 0))
    ch = int(value(f, b, d, TagTileLength, 0))
    offsE = d.find(TagTileOffsets)
    cntE = d.find(TagTileByteCounts)
  else:
    cw = w
    ch = int(min(value(f, b, d, TagRowsPerStrip, int64(h)), int64(h)))
    offsE = d.find(TagStripOffsets)
    cntE = d.find(TagStripByteCounts)
  if cw <= 0 or ch <= 0 or offsE.isNone: raise err("missing image data layout")
  let offs = values(f, b, offsE.get)
  var cnts = if cntE.isSome: values(f, b, cntE.get) else: @[]
  let samplesPerChunk = if planar == 2: 1 else: spp
  let rowBytes = (cw * samplesPerChunk * bits + 7) div 8
  let across = (w + cw - 1) div cw
  let down = (h + ch - 1) div ch
  let planes = if planar == 2: spp else: 1
  if offs.len < across * down * planes: raise err("too few strips or tiles")
  # raw sample buffer: all samples as normalised floats would be big; keep native bits
  var colormap: seq[int64]
  if photo == 3:
    let cm = d.find(TagColorMap)
    if cm.isNone: raise err("palette image without a colour map")
    colormap = values(f, b, cm.get)
    if colormap.len < 3 * (1 shl bits): raise err("short colour map")
  var img = newImage(w, h, layout, outSample)
  let outCh = layout.channels
  var truncated = false
  for plane in 0 ..< planes:
    for cy in 0 ..< down:
      for cx in 0 ..< across:
        let idx = plane * across * down + cy * across + cx
        let off = offs[idx]
        var n = if idx < cnts.len: cnts[idx] else: int64(b.len) - off
        if off < 0 or off >= b.len:
          truncated = true
          continue
        n = min(n, int64(b.len) - off)
        let rows = if tiled: ch else: min(ch, h - cy * ch)
        let expected = rowBytes * rows
        var data = decompress(comp, b.toOpenArray(int(off), int(off + n) - 1), expected)
        if data.len < expected:
          truncated = true
          data.setLen(expected)
        # predictors
        if predictor == 2:
          for r in 0 ..< rows:
            let ro = r * rowBytes
            case bits
            of 8:
              for x in samplesPerChunk ..< cw * samplesPerChunk: data[ro + x] = byte((int(data[ro + x]) + int(data[ro + x - samplesPerChunk])) and 0xFF)
            of 16:
              for x in samplesPerChunk ..< cw * samplesPerChunk:
                let a = rd16(f.order, data, ro + 2 * x).get(0)
                let p = rd16(f.order, data, ro + 2 * (x - samplesPerChunk)).get(0)
                let v = (a + p) and 0xFFFF
                if f.order == oLittle: (data[ro + 2 * x] = byte(v and 0xFF); data[ro + 2 * x + 1] = byte(v shr 8))
                else: (data[ro + 2 * x] = byte(v shr 8); data[ro + 2 * x + 1] = byte(v and 0xFF))
            else: discard
        elif predictor == 3 and bits == 32:
          let n4 = cw * samplesPerChunk * 4
          var tmp = newSeq[byte](n4)
          for r in 0 ..< rows:
            let ro = r * rowBytes
            for x in samplesPerChunk ..< n4: data[ro + x] = byte((int(data[ro + x]) + int(data[ro + x - samplesPerChunk])) and 0xFF)
            let cnt = cw * samplesPerChunk
            for i in 0 ..< cnt:
              for k in 0..3: tmp[4 * i + k] = data[ro + k * cnt + i]
            for i in 0 ..< cnt:
              # big-endian byte order in the predictor layout
              let u = (uint32(tmp[4 * i]) shl 24) or (uint32(tmp[4 * i + 1]) shl 16) or (uint32(tmp[4 * i + 2]) shl 8) or uint32(tmp[4 * i + 3])
              if f.order == oLittle:
                data[ro + 4 * i] = byte(u and 0xFF); data[ro + 4 * i + 1] = byte((u shr 8) and 0xFF)
                data[ro + 4 * i + 2] = byte((u shr 16) and 0xFF); data[ro + 4 * i + 3] = byte(u shr 24)
              else:
                for k in 0..3: data[ro + 4 * i + k] = tmp[4 * i + k]
        # scatter into the image
        for r in 0 ..< rows:
          let y = cy * ch + r
          if y >= h: break
          let ro = r * rowBytes
          for xi in 0 ..< cw:
            let x = cx * cw + xi
            if x >= w: break
            for s in 0 ..< samplesPerChunk:
              let sIdx = if planar == 2: plane else: s
              let si = xi * samplesPerChunk + s
              var vNorm: float32
              var v16: int
              case bits
              of 1, 2, 4:
                let bit = si * bits
                let raw = (int(data[ro + bit div 8]) shr (8 - bits - bit mod 8)) and ((1 shl bits) - 1)
                v16 = raw
                vNorm = float32(raw) / float32((1 shl bits) - 1)
              of 8:
                v16 = int(data[ro + si])
                vNorm = float32(v16) / 255.0'f32
              of 16:
                v16 = rd16(f.order, data, ro + 2 * si).get(0)
                vNorm = float32(v16) / 65535.0'f32
              else:
                let u = uint32(rd32(f.order, data, ro + 4 * si).get(0))
                vNorm = cast[float32](u)
              if photo == 3:
                if sIdx == 0:
                  let n = 1 shl bits
                  let idx2 = min(v16, n - 1)
                  for c in 0..2:
                    let cv = colormap[c * n + idx2]
                    let o = (y * w + x) * outCh + c
                    if outSample == isU8: img.data[o] = byte(cv shr 8)
                    else: cast[ptr uint16](addr img.data[2 * o])[] = uint16(cv)
                elif sIdx == 1 and extraAlpha:
                  let o = (y * w + x) * outCh + 3
                  if outSample == isU8: img.data[o] = byte(v16 and 0xFF)
                  else: cast[ptr uint16](addr img.data[2 * o])[] = uint16(v16)
                continue
              var oc = sIdx
              if sIdx >= colorCh:
                if not (extraAlpha and sIdx == colorCh): continue
                oc = outCh - 1
              if photo == 0 and sIdx == 0: vNorm = 1.0'f32 - vNorm
              let o = (y * w + x) * outCh + oc
              case outSample
              of isU8: img.data[o] = (if bits == 8 and not (photo == 0 and sIdx == 0): byte(v16) else: byte(round(clamp(vNorm, 0.0'f32, 1.0'f32) * 255)))
              of isU16: cast[ptr uint16](addr img.data[2 * o])[] = (if photo == 0 and sIdx == 0: uint16(65535 - v16) else: uint16(v16))
              else: cast[ptr float32](addr img.data[4 * o])[] = vNorm
  # metadata
  let iccE = d.find(TagIcc)
  if iccE.isSome: img.icc = rawBytes(b, iccE.get)
  let xmpE = d.find(TagXmp)
  if xmpE.isSome:
    let x = rawBytes(b, xmpE.get)
    if x.isSome:
      let s = x.get.toStr
      if s.validateUtf8 == -1: img.meta.xmp = some(s.strip(leading = false, chars = {'\0'}))
  let psE = d.find(TagPhotoshop)
  if psE.isSome: img.meta.photoshopResources = rawBytes(b, psE.get)
  let lyE = d.find(TagImageSourceData)
  if lyE.isSome: img.meta.photoshopLayers = rawBytes(b, lyE.get)
  let xr = rationalValue(f, b, d, TagXRes)
  let yr = rationalValue(f, b, d, TagYRes)
  let unit = value(f, b, d, TagResUnit, 2)
  if xr.isSome and yr.isSome and unit in [2'i64, 3] and xr.get > 0 and yr.get > 0:
    let s = if unit == 3: 2.54 else: 1.0
    img.meta.dpi = some((float32(xr.get * s), float32(yr.get * s)))
  if img.meta.dpi.isNone and img.meta.photoshopResources.isSome:
    img.meta.dpi = photoshopResolution(img.meta.photoshopResources.get)
  for (tag, name) in TextTags:
    let e = d.find(tag)
    if e.isSome and e.get.ty == 2:
      let r = rawBytes(b, e.get)
      if r.isSome:
        let s = r.get.toStr.strip(leading = false, chars = {'\0'})
        if s.len > 0 and s.validateUtf8 == -1: img.meta.text.add (name, s)
  if truncated: img.warnings.add DecodeWarning(kind: dwTruncated, format: fmtTiff)
  img

proc decodeTiffPage*(b: openArray[byte]; page: int; limits: Limits): Image =
  let (f, first) = parseHeader(b)
  let ifds = collectIfds(f, b, first)
  if page < 0 or page >= ifds.len: raise err("no page " & $page)
  decodeIfd(f, b, ifds[page][0], limits)

proc decodeTiff*(b: openArray[byte]; limits: Limits): Image =
  ## Decodes the default (first full-resolution) page. Raises `CodecError`.
  let info = tiffInfo(b)
  result = decodeTiffPage(b, info.defaultPage, limits)
  var pages = 0
  for p in info.pages:
    if p.kind == tpPage: inc pages
  if pages > 1: result.warnings.add DecodeWarning(kind: dwMorePages, total: some(pages))

proc tiffOrientation*(b: openArray[byte]): int =
  ## The Orientation (1–8) of the default page; 1 when absent or malformed.
  try:
    let (f, first) = parseHeader(b)
    let info = tiffInfo(b)
    let ifds = collectIfds(f, b, first)
    let v = value(f, b, ifds[info.defaultPage][0], TagOrientation, 1)
    if v >= 1 and v <= 8: int(v) else: 1
  except CatchableError: 1

proc photoshopTags*(b: openArray[byte]): (Option[seq[byte]], Option[seq[byte]]) =
  ## The first IFD's Photoshop image resources (34377) and layer data (37724).
  try:
    let (f, first) = parseHeader(b)
    let d = readIfd(f, b, first)
    let r = d.find(TagPhotoshop)
    let l = d.find(TagImageSourceData)
    let rv = if r.isSome: rawBytes(b, r.get) else: none(seq[byte])
    let lv = if l.isSome: rawBytes(b, l.get) else: none(seq[byte])
    (rv, lv)
  except CatchableError: (none(seq[byte]), none(seq[byte]))

proc writesLittleEndian*(): bool = true

# ---------------------------------------------------------------- encode

type TagVal = object
  tag, ty: int
  count: int64
  data: seq[byte]

proc encodeTiff*(src: Image; layout: ChannelLayout; sample: ImgSample; opts: EncodeOptions): seq[byte] =
  ## Encodes a single-page TIFF.
  let img = src.convert(layout, sample)
  let w = img.width
  let h = img.height
  let spp = layout.channels
  let bps = sample.bytes
  let bits = bps * 8
  let rowBytes = w * spp * bps
  let rowsPerStrip = max(1, min(h, (64 * 1024) div max(rowBytes, 1)))
  let comp = case opts.tiffCompression
    of tcNone: 1
    of tcLzw: 5
    of tcDeflate: 8
    of tcPackBits: 32773
  let predictor = if comp in [5, 8] and sample in [isU8, isU16]: 2 else: 1
  var strips: seq[seq[byte]]
  var y = 0
  while y < h:
    let rows = min(rowsPerStrip, h - y)
    var raw = img.data[y * rowBytes ..< (y + rows) * rowBytes]
    if predictor == 2:
      for r in 0 ..< rows:
        let ro = r * rowBytes
        if sample == isU8:
          for x in countdown(w * spp - 1, spp): raw[ro + x] = byte((int(raw[ro + x]) - int(raw[ro + x - spp])) and 0xFF)
        else:
          for x in countdown(w * spp - 1, spp):
            let a = int(cast[ptr uint16](addr raw[ro + 2 * x])[])
            let p = int(cast[ptr uint16](addr raw[ro + 2 * (x - spp)])[])
            cast[ptr uint16](addr raw[ro + 2 * x])[] = uint16((a - p) and 0xFFFF)
    case comp
    of 5: strips.add lzwEncodeTiff(raw)
    of 8: strips.add zlibCompress(raw, 6)
    of 32773:
      var o: seq[byte]
      for r in 0 ..< rows: o.add packbitsEncode(raw.toOpenArray(r * rowBytes, (r + 1) * rowBytes - 1))
      strips.add o
    else: strips.add raw
    y += rows
  var total = 0'i64
  for s in strips: total += s.len
  let big = opts.tiffBigtiff or total > 3_900_000_000'i64
  var tags: seq[TagVal]
  proc u16s(vals: openArray[int]): seq[byte] =
    for v in vals: result.add [byte(v and 0xFF), byte((v shr 8) and 0xFF)]
  proc u32s(vals: openArray[int64]): seq[byte] =
    for v in vals: result.add [byte(v and 0xFF), byte((v shr 8) and 0xFF), byte((v shr 16) and 0xFF), byte((v shr 24) and 0xFF)]
  proc u64s(vals: openArray[int64]): seq[byte] =
    for v in vals:
      for k in 0..7: result.add byte((v shr (8 * k)) and 0xFF)
  proc addShort(tag: int; vals: openArray[int]) = tags.add TagVal(tag: tag, ty: 3, count: vals.len, data: u16s(vals))
  proc addLong(tag: int; vals: openArray[int64]) = tags.add TagVal(tag: tag, ty: 4, count: vals.len, data: u32s(vals))
  tags.add TagVal(tag: TagWidth, ty: 4, count: 1, data: u32s([int64(w)]))
  tags.add TagVal(tag: TagHeight, ty: 4, count: 1, data: u32s([int64(h)]))
  var bl: seq[int]
  for _ in 0 ..< spp: bl.add bits
  addShort(TagBits, bl)
  addShort(TagCompression, [comp])
  let photo = if layout.isGray: 1 elif layout.isCmyk: 5 else: 2
  addShort(TagPhotometric, [photo])
  if opts.embedMetadata:
    for (k, v) in img.meta.text:
      for (tag, name) in TextTags:
        if name == k:
          var d = v.toBytes
          d.add 0
          tags.add TagVal(tag: tag, ty: 2, count: d.len, data: d)
  # strip offsets are patched after layout
  tags.add TagVal(tag: TagStripOffsets, ty: (if big: 16 else: 4), count: strips.len)
  addShort(TagSamples, [spp])
  addLong(TagRowsPerStrip, [int64(rowsPerStrip)])
  var counts: seq[int64]
  for s in strips: counts.add s.len
  tags.add TagVal(tag: TagStripByteCounts, ty: (if big: 16 else: 4), count: counts.len, data: (if big: u64s(counts) else: u32s(counts)))
  if opts.embedMetadata and img.meta.dpi.isSome:
    let (x, yv) = img.meta.dpi.get
    tags.add TagVal(tag: TagXRes, ty: 5, count: 1, data: u32s([int64(round(x * 1000)), 1000'i64]))
    tags.add TagVal(tag: TagYRes, ty: 5, count: 1, data: u32s([int64(round(yv * 1000)), 1000'i64]))
  addShort(TagPlanar, [1])
  if opts.embedMetadata and img.meta.dpi.isSome: addShort(TagResUnit, [2])
  if predictor != 1: addShort(TagPredictor, [predictor])
  if layout.hasAlpha: addShort(TagExtraSamples, [2])
  var sf: seq[int]
  for _ in 0 ..< spp: sf.add(if sample == isF32: 3 else: 1)
  addShort(TagSampleFormat, sf)
  if opts.embedMetadata and img.meta.xmp.isSome:
    let d = img.meta.xmp.get.toBytes
    tags.add TagVal(tag: TagXmp, ty: 1, count: d.len, data: d)
  if opts.embedMetadata and img.meta.photoshopResources.isSome:
    let d = img.meta.photoshopResources.get
    tags.add TagVal(tag: TagPhotoshop, ty: 7, count: d.len, data: d)
  if opts.embedIcc and img.icc.isSome:
    let d = img.icc.get
    tags.add TagVal(tag: TagIcc, ty: 7, count: d.len, data: d)
  if opts.embedMetadata and img.meta.photoshopLayers.isSome:
    let d = img.meta.photoshopLayers.get
    tags.add TagVal(tag: TagImageSourceData, ty: 7, count: d.len, data: d)
  # sort tags
  for i in 1 ..< tags.len:
    var j = i
    while j > 0 and tags[j - 1].tag > tags[j].tag:
      swap(tags[j - 1], tags[j])
      dec j
  # layout: header, strips, remote tag values, IFD
  if big:
    result = @[byte('I'), byte('I'), 43, 0, 8, 0, 0, 0]
    result.add u64s([0'i64])
  else:
    result = @[byte('I'), byte('I'), 42, 0, 0, 0, 0, 0]
  var stripOffs: seq[int64]
  for s in strips:
    if result.len mod 2 == 1: result.add 0
    stripOffs.add result.len
    result.add s
  for t in tags.mitems:
    if t.tag == TagStripOffsets: t.data = if big: u64s(stripOffs) else: u32s(stripOffs)
  let inline = if big: 8 else: 4
  var remote: seq[int64] = newSeq[int64](tags.len)
  for i, t in tags:
    if t.data.len > inline:
      if result.len mod 2 == 1: result.add 0
      remote[i] = result.len
      result.add t.data
  if result.len mod 2 == 1: result.add 0
  let ifdAt = result.len
  if big: result.add u64s([int64(tags.len)]) else: result.add u16s([tags.len])
  for i, t in tags:
    result.add u16s([t.tag, t.ty])
    if big: result.add u64s([t.count]) else: result.add u32s([t.count])
    if t.data.len > inline:
      if big: result.add u64s([remote[i]]) else: result.add u32s([remote[i]])
    else:
      var v = t.data
      while v.len < inline: v.add 0
      result.add v
  if big: result.add u64s([0'i64]) else: result.add u32s([0'i64])
  if big:
    for k in 0..7: result[8 + k] = byte((ifdAt shr (8 * k)) and 0xFF)
  else:
    for k in 0..3: result[4 + k] = byte((ifdAt shr (8 * k)) and 0xFF)
