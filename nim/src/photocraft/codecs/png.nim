## PNG: 1/2/4/8/16-bit, every colour type (palette and tRNS expanded), Adam7 read and write,
## iCCP, eXIf, tEXt/zTXt/iTXt (XMP in `XML:com.adobe.xmp`), pHYs; APNG files decode their
## default image and report the frame count. Written from the PNG specification (ISO/IEC 15948).

import std/[options, strutils, math]
from std/unicode import validateUtf8
import ./image, ../zlib

const
  XmpKeyword* = "XML:com.adobe.xmp"
  MetersPerInch = 0.0254'f32
  Signature = [0x89'u8, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
  Adam7: array[7, (int, int, int, int)] = [(0, 0, 8, 8), (4, 0, 8, 8), (0, 4, 4, 8), (2, 0, 4, 4), (0, 2, 2, 4), (1, 0, 2, 2), (0, 1, 1, 2)]

proc err(msg: string): ref CodecError = malformed(fmtPng, msg)

proc be32(b: openArray[byte]; p: int): int =
  (int(b[p]) shl 24) or (int(b[p + 1]) shl 16) or (int(b[p + 2]) shl 8) or int(b[p + 3])

proc paeth(a, b, c: int): int {.inline.} =
  let p = a + b - c
  let pa = abs(p - a)
  let pb = abs(p - b)
  let pc = abs(p - c)
  if pa <= pb and pa <= pc: a elif pb <= pc: b else: c

proc unfilter(raw: openArray[byte]; pos: var int; rows, rowBytes, bpp: int; outp: var seq[byte]) =
  ## Reverses the per-row filters of `rows` rows starting at `pos` into `outp` (rows × rowBytes).
  outp = newSeq[byte](rows * rowBytes)
  for y in 0 ..< rows:
    if pos >= raw.len: raise err("image data is truncated")
    let ft = raw[pos]
    inc pos
    if pos + rowBytes > raw.len: raise err("image data is truncated")
    let o = y * rowBytes
    let po = o - rowBytes
    for x in 0 ..< rowBytes:
      let v = int(raw[pos + x])
      let a = if x >= bpp: int(outp[o + x - bpp]) else: 0
      let b = if y > 0: int(outp[po + x]) else: 0
      let c = if y > 0 and x >= bpp: int(outp[po + x - bpp]) else: 0
      let r = case ft
        of 0: v
        of 1: v + a
        of 2: v + b
        of 3: v + (a + b) div 2
        of 4: v + paeth(a, b, c)
        else: raise err("bad filter type " & $ft)
      outp[o + x] = byte(r and 0xFF)
    pos += rowBytes

proc latin1ToUtf8(s: openArray[byte]): string =
  for b in s:
    if b < 0x80: result.add char(b)
    else:
      result.add char(0xC0 or (int(b) shr 6))
      result.add char(0x80 or (int(b) and 0x3F))


proc chunkTypeDebug(kind: string): string =
  ## The png crate's `Debug` form of a chunk type.
  template lower(c: char): bool = c >= 'a' and c <= 'z'
  template upper(c: char): bool = c >= 'A' and c <= 'Z'
  "ChunkType { type: " & kind & ", critical: " & $upper(kind[0]) & ", private: " & $lower(kind[1]) &
    ", reserved: " & $lower(kind[2]) & ", safecopy: " & $lower(kind[3]) & " }"

proc deflateMessage(e: ref ZlibError): string =
  ## The png crate's message for an image-data inflate failure (`fdeflate` error names) where the
  ## failure maps onto one; this inflater's own message otherwise.
  let name =
    if e of ZlibEof or e.msg == "zlib stream too short": "InsufficientInput"
    elif e.msg in ["bad zlib header", "preset dictionaries are not supported"]: "BadZlibHeader"
    elif e.msg == "invalid block type": "InvalidBlockType"
    elif e.msg == "stored block length mismatch": "InvalidUncompressedBlockLength"
    elif e.msg == "distance too far back": "DistanceTooFarBack"
    else: ""
  if name.len > 0: "Corrupt deflate stream. " & name else: e.msg

proc decodePng*(bytes: openArray[byte]; limits: Limits): Image =
  ## Decodes a PNG file. Raises `CodecError`.
  if bytes.len < 8: raise err("not a PNG file")
  for i in 0..7:
    if bytes[i] != Signature[i]: raise err("not a PNG file")
  var p = 8
  var width, height, depth, colorType, interlace = -1
  var palette: seq[array[3, uint8]]
  var trns: seq[byte]
  var idat: seq[byte]
  var icc = none(seq[byte])
  var meta: Metadata
  var frames = 0
  var hasFctlBeforeIdat = false
  var seenIdat = false
  var textBudget = int(min(limits.maxAlloc, int64(high(int32))))
  var plain, compressed, intl: seq[(string, string)]
  while p + 8 <= bytes.len:
    let length = be32(bytes, p)
    if length < 0 or length > high(int32): raise err("bad chunk length")
    var kind = ""
    for i in 0..3: kind.add char(bytes[p + 4 + i])
    let ds = p + 8
    # The png crate checks these lengths as soon as it reads the chunk header.
    let lengthOk = case kind
      of "IHDR": length == 13
      of "PLTE": length >= 3 and length <= 768
      of "IEND": length == 0
      of "fcTL": length == 26
      else: true
    if not lengthOk: raise err("Chunk length wrong: " & chunkTypeDebug(kind))
    if ds + length + 4 > bytes.len:
      if seenIdat and kind != "IDAT": break  # tolerate a broken tail after the pixels
      raise err("chunk " & kind & " is truncated")
    let crcStored = uint32(be32(bytes, ds + length))
    let crcOk = crc32(bytes.toOpenArray(p + 4, ds + length - 1)) == crcStored
    template data(): untyped = bytes.toOpenArray(ds, ds + length - 1)
    case kind
    of "IHDR":
      if length < 13: raise err("IHDR too short")
      width = be32(bytes, ds)
      height = be32(bytes, ds + 4)
      depth = int(bytes[ds + 8])
      colorType = int(bytes[ds + 9])
      interlace = int(bytes[ds + 12])
      if width <= 0 or height <= 0: raise err("zero-sized image")
      limits.checkBytes(width, height, 8)
      let okDepth = case colorType
        of 0: depth in [1, 2, 4, 8, 16]
        of 2, 4, 6: depth in [8, 16]
        of 3: depth in [1, 2, 4, 8]
        else: false
      if not okDepth: raise err("invalid colour type/bit depth " & $colorType & "/" & $depth)
      if interlace notin [0, 1]: raise err("unknown interlace method")
    of "PLTE":
      for i in 0 ..< length div 3: palette.add [bytes[ds + 3 * i], bytes[ds + 3 * i + 1], bytes[ds + 3 * i + 2]]
    of "tRNS": trns = @(data)
    of "IDAT":
      seenIdat = true
      if idat.len + length > int(min(limits.maxAlloc, int64(high(int32)))): raise limitExceeded("PNG compressed data exceeds max_alloc")
      idat.add data
    of "acTL":
      if length >= 8: frames = be32(bytes, ds)
    of "fcTL":
      if not seenIdat: hasFctlBeforeIdat = true
    of "iCCP":
      if crcOk:
        var z = ds
        while z < ds + length and bytes[z] != 0: inc z
        if z + 2 <= ds + length:
          try: icc = some(zlibDecompress(bytes.toOpenArray(z + 2, ds + length - 1), maxOut = 64 shl 20))
          except ZlibError: discard
    of "eXIf":
      if crcOk: meta.exif = some(@(data))
    of "pHYs":
      if length >= 9 and bytes[ds + 8] == 1:
        let x = be32(bytes, ds)
        let y = be32(bytes, ds + 4)
        if x > 0 and y > 0: meta.dpi = some((float32(x) * MetersPerInch, float32(y) * MetersPerInch))
    of "tEXt", "zTXt", "iTXt":
      if not crcOk: discard
      else:
        var z = ds
        let e = ds + length
        while z < e and bytes[z] != 0: inc z
        if z >= e or z == ds or z - ds > 79: discard
        else:
          let keyword = latin1ToUtf8(bytes.toOpenArray(ds, z - 1))
          var text = none(string)
          var v = z + 1
          try:
            case kind
            of "tEXt": text = some(latin1ToUtf8(bytes.toOpenArray(v, e - 1)))
            of "zTXt":
              if v < e and bytes[v] == 0:
                text = some(latin1ToUtf8(zlibDecompress(bytes.toOpenArray(v + 1, e - 1), maxOut = textBudget)))
            else:
              if v + 2 <= e:
                let flag = bytes[v]
                let meth = bytes[v + 1]
                v += 2
                if flag <= 1 and not (flag == 1 and meth != 0):
                  var l = v
                  while l < e and bytes[l] != 0: inc l
                  var t = l + 1
                  while t < e and bytes[t] != 0: inc t
                  if t < e:
                    let body = if flag == 1: zlibDecompress(bytes.toOpenArray(t + 1, e - 1), maxOut = textBudget) else: @(bytes.toOpenArray(t + 1, e - 1))
                    var s = body.toStr
                    if s.validateUtf8 == -1: text = some(s)
          except ZlibError: discard
          if text.isSome:
            textBudget -= keyword.len + text.get.len
            if textBudget < 0: raise limitExceeded("PNG decoded text metadata exceeds max_alloc")
            if keyword == XmpKeyword: meta.xmp = text
            elif kind == "tEXt": plain.add (keyword, text.get)
            elif kind == "zTXt": compressed.add (keyword, text.get)
            else: intl.add (keyword, text.get)
    of "IEND": break
    else: discard
    p = ds + length + 4
  if width < 0: raise err("missing IHDR")
  if idat.len == 0:
    raise err(if seenIdat: "Corrupt deflate stream. InsufficientInput" else: "missing IDAT")
  if colorType == 3 and palette.len == 0: raise err("missing palette")
  let channelsIn = case colorType
    of 0: 1
    of 2: 3
    of 3: 1
    of 4: 2
    else: 4
  let bitsPP = channelsIn * depth
  let bpp = max(1, bitsPP div 8)
  var raw: seq[byte]
  try: raw = zlibDecompress(idat, maxOut = int(min(limits.maxAlloc, int64(high(int32)))) + height * 8, checkAdler = false)
  except ZlibError as e: raise err(deflateMessage(e))
  # Output layout
  var layout: ChannelLayout
  var hasTrnsKey = false
  case colorType
  of 0:
    layout = if trns.len >= 2: clGrayA else: clGray
    hasTrnsKey = trns.len >= 2
  of 2:
    layout = if trns.len >= 6: clRgba else: clRgb
    hasTrnsKey = trns.len >= 6
  of 3: layout = if trns.len > 0: clRgba else: clRgb
  of 4: layout = clGrayA
  else: layout = clRgba
  let sample = if depth == 16: isU16 else: isU8
  var img = newImage(width, height, layout, sample)
  let outCh = layout.channels
  proc store(img: var Image; x, y: int; row: seq[byte]; rx: int) =
    ## Writes pixel `rx` of an unfiltered row as pixel (x, y).
    let o = (y * img.width + x) * outCh
    template put8(k: int; v: int) = img.data[o + k] = byte(v)
    template put16(k: int; v: int) = cast[ptr uint16](addr img.data[2 * (o + k)])[] = uint16(v)
    if depth < 8:
      let bitPos = rx * depth
      let b = int(row[bitPos div 8])
      let shift = 8 - depth - (bitPos mod 8)
      let v = (b shr shift) and ((1 shl depth) - 1)
      if colorType == 3:
        if v >= palette.len: raise err("palette index out of range")
        put8(0, int(palette[v][0])); put8(1, int(palette[v][1])); put8(2, int(palette[v][2]))
        if outCh == 4: put8(3, if v < trns.len: int(trns[v]) else: 255)
      else:
        let g = v * 255 div ((1 shl depth) - 1)
        put8(0, g)
        if hasTrnsKey: put8(1, if v == ((int(trns[0]) shl 8) or int(trns[1])): 0 else: 255)
      return
    if depth == 8:
      let s = rx * channelsIn
      case colorType
      of 3:
        let v = int(row[s])
        if v >= palette.len: raise err("palette index out of range")
        put8(0, int(palette[v][0])); put8(1, int(palette[v][1])); put8(2, int(palette[v][2]))
        if outCh == 4: put8(3, if v < trns.len: int(trns[v]) else: 255)
      of 0:
        put8(0, int(row[s]))
        if hasTrnsKey: put8(1, if int(row[s]) == ((int(trns[0]) shl 8) or int(trns[1])): 0 else: 255)
      of 2:
        for k in 0..2: put8(k, int(row[s + k]))
        if hasTrnsKey:
          let key = int(row[s]) == ((int(trns[0]) shl 8) or int(trns[1])) and int(row[s + 1]) == ((int(trns[2]) shl 8) or int(trns[3])) and
                    int(row[s + 2]) == ((int(trns[4]) shl 8) or int(trns[5]))
          put8(3, if key: 0 else: 255)
      else:
        for k in 0 ..< channelsIn: put8(k, int(row[s + k]))
      return
    # 16-bit
    let s = rx * channelsIn * 2
    var vals: array[4, int]
    for k in 0 ..< channelsIn: vals[k] = (int(row[s + 2 * k]) shl 8) or int(row[s + 2 * k + 1])
    for k in 0 ..< channelsIn: put16(k, vals[k])
    if hasTrnsKey:
      var key = true
      for k in 0 ..< channelsIn:
        if vals[k] != ((int(trns[2 * k]) shl 8) or int(trns[2 * k + 1])): key = false
      put16(channelsIn, if key: 0 else: 65535)
  var pos = 0
  if interlace == 0:
    let rowBytes = (width * bitsPP + 7) div 8
    var rows: seq[byte]
    unfilter(raw, pos, height, rowBytes, bpp, rows)
    var row = newSeq[byte](rowBytes)
    for y in 0 ..< height:
      copyMem(addr row[0], addr rows[y * rowBytes], rowBytes)
      for x in 0 ..< width: store(img, x, y, row, x)
  else:
    for (x0, y0, dx, dy) in Adam7:
      if x0 >= width or y0 >= height: continue
      let pw = (width - x0 + dx - 1) div dx
      let ph = (height - y0 + dy - 1) div dy
      let rowBytes = (pw * bitsPP + 7) div 8
      var rows: seq[byte]
      unfilter(raw, pos, ph, rowBytes, bpp, rows)
      var row = newSeq[byte](rowBytes)
      for j in 0 ..< ph:
        copyMem(addr row[0], addr rows[j * rowBytes], rowBytes)
        for i in 0 ..< pw: store(img, x0 + i * dx, y0 + j * dy, row, i)
  img.icc = icc
  meta.text.add plain
  meta.text.add compressed
  meta.text.add intl
  img.meta = meta
  let images = if frames > 0: frames + (if hasFctlBeforeIdat: 0 else: 1) else: 1
  if images > 1: img.warnings.add DecodeWarning(kind: dwMoreFrames, total: some(images))
  img

# ---------------------------------------------------------------- encoder

proc chunk(o: var seq[byte]; kind: string; data: openArray[byte]) =
  let n = data.len
  o.add [byte((n shr 24) and 0xFF), byte((n shr 16) and 0xFF), byte((n shr 8) and 0xFF), byte(n and 0xFF)]
  let start = o.len
  for c in kind: o.add byte(c)
  o.add data
  let c = crc32(o.toOpenArray(start, o.len - 1))
  o.add [byte(c shr 24), byte((c shr 16) and 0xFF), byte((c shr 8) and 0xFF), byte(c and 0xFF)]

proc filterRow(row: openArray[byte]; prev: openArray[byte]; hasPrev: bool; bpp: int; outp: var seq[byte]) =
  ## Adaptive filter choice (minimum sum of absolute differences), as libpng does.
  let n = row.len
  var best = -1
  var bestSum = high(int)
  var cand = newSeq[byte](n)
  var bestBuf = newSeq[byte](n)
  for ft in 0..4:
    var sum = 0
    for x in 0 ..< n:
      let a = if x >= bpp: int(row[x - bpp]) else: 0
      let b = if hasPrev: int(prev[x]) else: 0
      let c = if hasPrev and x >= bpp: int(prev[x - bpp]) else: 0
      let v = int(row[x])
      let f = case ft
        of 0: v
        of 1: v - a
        of 2: v - b
        of 3: v - (a + b) div 2
        else: v - paeth(a, b, c)
      let fb = byte(f and 0xFF)
      cand[x] = fb
      sum += (if fb < 128: int(fb) else: 256 - int(fb))
      if sum >= bestSum: break
    if sum < bestSum:
      bestSum = sum
      best = ft
      swap(bestBuf, cand)
  outp.add byte(best)
  outp.add bestBuf

proc level(c: PngCompression): int =
  case c
  of pcNone: 0
  of pcFast: 1
  of pcDefault: 6
  of pcBest: 9

proc isLatin1Keyword(k: string): bool =
  if k.len == 0 or k.len > 79: return false
  for c in k:
    if not (c in {' '..'~'}): return false
  true

proc writeHeaderChunks(o: var seq[byte]; img: Image; colorType, depth: int; opts: EncodeOptions) =
  var ihdr: seq[byte]
  for v in [img.width, img.height]:
    ihdr.add [byte((v shr 24) and 0xFF), byte((v shr 16) and 0xFF), byte((v shr 8) and 0xFF), byte(v and 0xFF)]
  ihdr.add [byte(depth), byte(colorType), 0, 0, (if opts.pngInterlaced: 1'u8 else: 0'u8)]
  o.chunk("IHDR", ihdr)
  if opts.embedIcc and img.icc.isSome:
    var d = "ICC Profile".toBytes
    d.add [0'u8, 0]
    d.add zlibCompress(img.icc.get, 6)
    o.chunk("iCCP", d)
  if opts.embedMetadata:
    if img.meta.dpi.isSome:
      let (x, y) = img.meta.dpi.get
      if x > 0 and y > 0:
        var d: seq[byte]
        for v in [int(round(x / MetersPerInch)), int(round(y / MetersPerInch))]:
          d.add [byte((v shr 24) and 0xFF), byte((v shr 16) and 0xFF), byte((v shr 8) and 0xFF), byte(v and 0xFF)]
        d.add 1
        o.chunk("pHYs", d)
    if img.meta.exif.isSome: o.chunk("eXIf", img.meta.exif.get)
    for (k, v) in img.meta.text:
      if not isLatin1Keyword(k): continue
      var ascii = true
      for c in v:
        if ord(c) >= 128: ascii = false
      var d = k.toBytes
      d.add 0
      if ascii:
        d.add v.toBytes
        o.chunk("tEXt", d)
      else:
        d.add [0'u8, 0, 0, 0]
        d.add v.toBytes
        o.chunk("iTXt", d)
    if img.meta.xmp.isSome:
      var d = XmpKeyword.toBytes
      d.add [0'u8, 0, 0, 0, 0]
      d.add img.meta.xmp.get.toBytes
      o.chunk("iTXt", d)

proc encodePng*(src: Image; layout: ChannelLayout; sample: ImgSample; opts: EncodeOptions): seq[byte] =
  ## Encodes `src` converted to `layout`/`sample` (Gray/GrayA/Rgb/Rgba at 8 or 16 bits).
  let img = src.convert(layout, sample)
  let colorType = case img.layout
    of clGray: 0
    of clGrayA: 4
    of clRgb: 2
    of clRgba: 6
    else: raise encodeErr(fmtPng, "unsupported layout " & $img.layout)
  let depth = case img.sample
    of isU8: 8
    of isU16: 16
    else: raise encodeErr(fmtPng, "unsupported sample " & $img.sample)
  var data = img.data
  if depth == 16:
    for i in 0 ..< data.len div 2:
      let v = cast[ptr uint16](addr data[2 * i])[]
      data[2 * i] = byte(v shr 8)
      data[2 * i + 1] = byte(v and 0xFF)
  let bpp = img.layout.channels * img.sample.bytes
  let w = img.width
  let h = img.height
  var filtered: seq[byte]
  if opts.pngInterlaced:
    for (x0, y0, dx, dy) in Adam7:
      if x0 >= w or y0 >= h: continue
      var y = y0
      while y < h:
        filtered.add 0
        var x = x0
        while x < w:
          let o = (y * w + x) * bpp
          for k in 0 ..< bpp: filtered.add data[o + k]
          x += dx
        y += dy
  else:
    let stride = w * bpp
    filtered = newSeqOfCap[byte]((stride + 1) * h)
    for y in 0 ..< h:
      if opts.pngCompression == pcNone:
        filtered.add 0
        filtered.add data.toOpenArray(y * stride, (y + 1) * stride - 1)
      else:
        filterRow(data.toOpenArray(y * stride, (y + 1) * stride - 1),
                  data.toOpenArray(max(y - 1, 0) * stride, max(y - 1, 0) * stride + stride - 1), y > 0, bpp, filtered)
  result = @Signature
  result.writeHeaderChunks(img, colorType, depth, opts)
  let z = zlibCompress(filtered, level(opts.pngCompression))
  var p = 0
  while p < z.len:
    let n = min(z.len - p, 1 shl 20)
    result.chunk("IDAT", z.toOpenArray(p, p + n - 1))
    p += n
  result.chunk("IEND", [])

proc encodePngIndexed*(width, height: int; indices: openArray[uint8]; palette: openArray[array[3, uint8]];
                       transparent: Option[uint8]; dpi: Option[(float32, float32)]): seq[byte] =
  ## An 8-bit palette PNG (`indices` row-major, `palette` 1–256 entries covering every index); the
  ## `transparent` index gets a tRNS entry, `dpi` a pHYs chunk.
  if width <= 0 or height <= 0 or indices.len != width * height:
    raise invalidImage("index data does not match the size")
  var covered = palette.len > 0 and palette.len <= 256
  if covered:
    for i in indices:
      if int(i) >= palette.len:
        covered = false
        break
  if not covered: raise invalidImage("palette must have 1..=256 entries covering every index")
  result = @Signature
  var ihdr: seq[byte]
  for v in [width, height]:
    ihdr.add [byte((v shr 24) and 0xFF), byte((v shr 16) and 0xFF), byte((v shr 8) and 0xFF), byte(v and 0xFF)]
  ihdr.add [8'u8, 3, 0, 0, 0]
  result.chunk("IHDR", ihdr)
  var pl: seq[byte]
  for c in palette: pl.add c
  result.chunk("PLTE", pl)
  if transparent.isSome and int(transparent.get) < palette.len:
    var trns = newSeq[byte](int(transparent.get) + 1)
    for v in trns.mitems: v = 255
    trns[int(transparent.get)] = 0
    result.chunk("tRNS", trns)
  if dpi.isSome:
    let (x, y) = dpi.get
    if x == x and y == y and x > 0 and y > 0 and x < Inf and y < Inf:
      var d: seq[byte]
      for v in [int(round(x / MetersPerInch)), int(round(y / MetersPerInch))]:
        d.add [byte((v shr 24) and 0xFF), byte((v shr 16) and 0xFF), byte((v shr 8) and 0xFF), byte(v and 0xFF)]
      d.add 1
      result.chunk("pHYs", d)
  var filtered: seq[byte]
  for y in 0 ..< height:
    filtered.add 0
    filtered.add indices.toOpenArray(y * width, (y + 1) * width - 1)
  result.chunk("IDAT", zlibCompress(filtered, 9))
  result.chunk("IEND", [])
