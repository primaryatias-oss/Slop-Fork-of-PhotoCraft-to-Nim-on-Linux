## JPEG (ITU-T T.81): decodes baseline, extended and progressive Huffman JPEGs (gray, YCbCr,
## CMYK, YCCK; any sampling factors; restart intervals; truncated files decode with a warning)
## and encodes baseline JPEG (gray, RGB 4:4:4 or 4:2:0, CMYK). Metadata (APP0 JFIF density,
## APP1 EXIF/XMP, APP2 multi-segment ICC, APP13 Photoshop resources, APP14 Adobe) is read by a
## marker scanner, as the Rust codec does. Written from the specification.

import std/[options, math, strutils, algorithm]
from std/unicode import validateUtf8
import ./image, ./meta, ../zlib, ../rmath

const
  ExifHeader = "Exif\0\0"
  XmpHeader = "http://ns.adobe.com/xap/1.0/\0"
  IccHeader = "ICC_PROFILE\0"
  PhotoshopHeader = "Photoshop 3.0\0"
  MaxSegment = 65533

  ZigZag: array[64, int] = [0, 1, 8, 16, 9, 2, 3, 10, 17, 24, 32, 25, 18, 11, 4, 5, 12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13, 6, 7, 14, 21, 28, 35, 42, 49, 56, 57, 50, 43, 36, 29, 22, 15, 23, 30, 37, 44, 51, 58, 59, 52, 45,
    38, 31, 39, 46, 53, 60, 61, 54, 47, 55, 62, 63]

proc err(msg: string): ref CodecError = malformed(fmtJpeg, msg)

proc hasPrefix(b: openArray[byte]; s: string): bool =
  if b.len < s.len: return false
  for i, c in s:
    if b[i] != byte(c): return false
  true

type JpegMeta* = object
  icc*: Option[seq[byte]]
  exif*: Option[seq[byte]]
  xmp*: Option[string]
  dpi*: Option[(float32, float32)]
  adobeTransform*: Option[int]
  frame*: Option[(int, int, int)]

proc scanMetadata*(b: openArray[byte]): JpegMeta =
  ## Walk the marker segments up to SOS and collect metadata.
  var iccParts: seq[(int, int, seq[byte])]
  var photoshop: seq[byte]
  var jfifDpi = none((float32, float32))
  var i = 2
  while i + 4 <= b.len:
    if b[i] != 0xFF: break
    let marker = int(b[i + 1])
    if marker == 0xFF:
      inc i
      continue
    if marker == 0xD8 or (marker >= 0xD0 and marker <= 0xD7) or marker == 0x01:
      i += 2
      continue
    if marker == 0xDA or marker == 0xD9: break
    let len = (int(b[i + 2]) shl 8) or int(b[i + 3])
    if len < 2 or i + 2 + len > b.len: break
    let s = i + 4
    let e = i + 2 + len
    template seg(): untyped = b.toOpenArray(s, e - 1)
    case marker
    of 0xE0:
      if e - s >= 12 and seg.hasPrefix("JFIF\0"):
        let unit = b[s + 7]
        let x = float32((int(b[s + 8]) shl 8) or int(b[s + 9]))
        let y = float32((int(b[s + 10]) shl 8) or int(b[s + 11]))
        if x > 0 and y > 0:
          if unit == 1: jfifDpi = some((x, y))
          elif unit == 2: jfifDpi = some((x * 2.54'f32, y * 2.54'f32))
    of 0xE1:
      if seg.hasPrefix(ExifHeader) and result.exif.isNone: result.exif = some(@(b.toOpenArray(s + ExifHeader.len, e - 1)))
      elif seg.hasPrefix(XmpHeader) and result.xmp.isNone:
        var x = b.toOpenArray(s + XmpHeader.len, e - 1).toStr
        if x.validateUtf8 == -1: result.xmp = some(x.strip(leading = false, chars = {'\0'}))
    of 0xED:
      if seg.hasPrefix(PhotoshopHeader) and photoshop.len < (1 shl 24): photoshop.add b.toOpenArray(s + PhotoshopHeader.len, e - 1)
    of 0xE2:
      if e - s >= 14 and seg.hasPrefix(IccHeader): iccParts.add (int(b[s + 12]), int(b[s + 13]), @(b.toOpenArray(s + 14, e - 1)))
    of 0xC0..0xCF:
      if marker notin [0xC4, 0xC8, 0xCC] and e - s >= 6 and result.frame.isNone:
        let h = (int(b[s + 1]) shl 8) or int(b[s + 2])
        let w = (int(b[s + 3]) shl 8) or int(b[s + 4])
        result.frame = some((w, h, int(b[s + 5])))
    of 0xEE:
      if e - s >= 12 and seg.hasPrefix("Adobe"): result.adobeTransform = some(int(b[s + 11]))
    else: discard
    i = e
  result.dpi = photoshopResolution(photoshop)
  if result.dpi.isNone and result.xmp.isSome: result.dpi = xmpResolution(result.xmp.get)
  if result.dpi.isNone and result.exif.isSome: result.dpi = exifResolution(result.exif.get)
  if result.dpi.isNone: result.dpi = jfifDpi
  if iccParts.len > 0:
    iccParts.sort(proc(a, b: (int, int, seq[byte])): int = cmp(a[0], b[0]))
    let total = iccParts[0][1]
    var ok = iccParts.len == total
    for k, p in iccParts:
      if p[0] != k + 1 or p[1] != total: ok = false
    if ok or iccParts.len == 1:
      var icc: seq[byte]
      for p in iccParts: icc.add p[2]
      result.icc = some(icc)

# ---------------------------------------------------------------- decoder

type
  HuffTable = object
    lookup: array[512, int16]   # (sym shl 4) or len, -1 = slow
    maxcode: array[18, int]
    valptr: array[17, int]
    mincode: array[17, int]
    vals: seq[uint8]
    present: bool

  Component = object
    id, h, v, tq: int
    bpl, bpc: int           # blocks per line / column (padded to MCU)
    coefs: seq[int16]       # bpl * bpc * 64
    dcTable, acTable: int
    pred: int

  Decoder = object
    data: ptr UncheckedArray[byte]
    len, pos: int
    bitBuf: uint32
    bitCnt: int
    markerHit: bool
    eobRun: int
    qt: array[4, array[64, int]]
    dc, ac: array[4, HuffTable]
    comps: seq[Component]
    width, height: int
    progressive: bool
    hmax, vmax, mcux, mcuy: int
    restartInterval: int
    truncated: bool
    adobe: int

proc buildTable(counts: array[16, int]; vals: seq[uint8]): HuffTable =
  result.vals = vals
  result.present = true
  for i in 0 ..< 512: result.lookup[i] = -1
  var code = 0
  var k = 0
  for l in 1..16:
    result.valptr[l] = k
    result.mincode[l] = code
    for _ in 0 ..< counts[l - 1]:
      if l <= 9:
        let shift = 9 - l
        let base = code shl shift
        for f in 0 ..< (1 shl shift):
          if k < vals.len: result.lookup[base + f] = int16((int(vals[k]) shl 4) or l)
      inc code
      inc k
    result.maxcode[l] = if counts[l - 1] > 0: code - 1 else: -1
    code = code shl 1
  result.maxcode[17] = high(int)

proc fill(d: var Decoder) {.inline.} =
  while d.bitCnt <= 24:
    var b = 0
    if not d.markerHit and d.pos < d.len:
      b = int(d.data[d.pos])
      if b == 0xFF:
        let nb = if d.pos + 1 < d.len: int(d.data[d.pos + 1]) else: 0xD9
        if nb == 0x00:
          d.pos += 2
        elif nb == 0xFF:
          inc d.pos
          continue
        else:
          d.markerHit = true
          b = 0
      else:
        inc d.pos
    elif not d.markerHit:
      d.truncated = true
      d.markerHit = true
    d.bitBuf = d.bitBuf or (uint32(b) shl (24 - d.bitCnt))
    d.bitCnt += 8

proc getBits(d: var Decoder; n: int): int {.inline.} =
  if n == 0: return 0
  d.fill
  result = int(d.bitBuf shr (32 - n))
  d.bitBuf = d.bitBuf shl n
  d.bitCnt -= n

proc getBit(d: var Decoder): int {.inline.} = d.getBits(1)

proc extend(v, n: int): int {.inline.} =
  if n == 0: 0 elif v < (1 shl (n - 1)): v - (1 shl n) + 1 else: v

proc decodeHuff(d: var Decoder; t: HuffTable): int {.inline.} =
  if not t.present: raise err("missing Huffman table")
  d.fill
  let look = t.lookup[int(d.bitBuf shr 23)]
  if look >= 0:
    let l = int(look and 15)
    d.bitBuf = d.bitBuf shl l
    d.bitCnt -= l
    return int(look shr 4)
  var code = 0
  for l in 1..16:
    code = (code shl 1) or d.getBit
    if t.maxcode[l] >= 0 and code <= t.maxcode[l]:
      let idx = t.valptr[l] + code - t.mincode[l]
      if idx < 0 or idx >= t.vals.len: raise err("bad Huffman code")
      return int(t.vals[idx])
  # corrupt data: behave like the end of the scan
  0

proc resetBits(d: var Decoder) =
  d.bitBuf = 0
  d.bitCnt = 0
  d.markerHit = false
  d.eobRun = 0
  for c in d.comps.mitems: c.pred = 0

proc u16at(d: Decoder; p: int): int =
  if p + 2 > d.len: raise err("unexpected end of file")
  (int(d.data[p]) shl 8) or int(d.data[p + 1])

proc decodeBlockBaseline(d: var Decoder; c: var Component; blk: int) =
  let t = d.decodeHuff(d.dc[c.dcTable])
  let diff = if t == 0: 0 else: extend(d.getBits(t), t)
  c.pred += diff
  c.coefs[blk] = int16(c.pred)
  var k = 1
  while k < 64:
    let rs = d.decodeHuff(d.ac[c.acTable])
    let s = rs and 15
    let r = rs shr 4
    if s == 0:
      if r == 15:
        k += 16
        continue
      break
    k += r
    if k > 63: break
    c.coefs[blk + ZigZag[k]] = int16(extend(d.getBits(s), s))
    inc k

proc decodeDcFirst(d: var Decoder; c: var Component; blk, al: int) =
  let t = d.decodeHuff(d.dc[c.dcTable])
  let diff = if t == 0: 0 else: extend(d.getBits(t), t)
  c.pred += diff
  c.coefs[blk] = int16(c.pred shl al)

proc decodeDcRefine(d: var Decoder; c: var Component; blk, al: int) =
  if d.getBit == 1: c.coefs[blk] = int16(int(c.coefs[blk]) or (1 shl al))

proc decodeAcFirst(d: var Decoder; c: var Component; blk, ss, se, al: int) =
  if d.eobRun > 0:
    dec d.eobRun
    return
  var k = ss
  while k <= se:
    let rs = d.decodeHuff(d.ac[c.acTable])
    let s = rs and 15
    let r = rs shr 4
    if s == 0:
      if r < 15:
        d.eobRun = (1 shl r) - 1
        if r > 0: d.eobRun += d.getBits(r)
        break
      k += 16
      continue
    k += r
    if k > 63: break
    c.coefs[blk + ZigZag[k]] = int16(extend(d.getBits(s), s) * (1 shl al))
    inc k

proc decodeAcRefine(d: var Decoder; c: var Component; blk, ss, se, al: int) =
  let p1 = 1 shl al
  let m1 = -1 shl al
  var k = ss
  if d.eobRun <= 0:
    while k <= se:
      let rs = d.decodeHuff(d.ac[c.acTable])
      let s = rs and 15
      var r = rs shr 4
      var value = 0
      if s == 0:
        if r < 15:
          d.eobRun = 1 shl r
          if r > 0: d.eobRun += d.getBits(r)
          break
      else:
        value = if d.getBit == 1: p1 else: m1
      while k <= se:
        let z = blk + ZigZag[k]
        if c.coefs[z] != 0:
          if d.getBit == 1:
            if (int(c.coefs[z]) and p1) == 0:
              c.coefs[z] = int16(if c.coefs[z] >= 0: int(c.coefs[z]) + p1 else: int(c.coefs[z]) + m1)
        else:
          if r == 0:
            if value != 0: c.coefs[z] = int16(value)
            inc k
            break
          dec r
        inc k
  if d.eobRun > 0:
    while k <= se:
      let z = blk + ZigZag[k]
      if c.coefs[z] != 0:
        if d.getBit == 1:
          if (int(c.coefs[z]) and p1) == 0:
            c.coefs[z] = int16(if c.coefs[z] >= 0: int(c.coefs[z]) + p1 else: int(c.coefs[z]) + m1)
      inc k
    dec d.eobRun

proc decodeScan(d: var Decoder; scomps: seq[int]; ss, se, ah, al: int) =
  d.resetBits
  proc blockOp(d: var Decoder; ci, blk: int) =
    if not d.progressive: decodeBlockBaseline(d, d.comps[ci], blk)
    elif ss == 0:
      if ah == 0: decodeDcFirst(d, d.comps[ci], blk, al) else: decodeDcRefine(d, d.comps[ci], blk, al)
    else:
      if ah == 0: decodeAcFirst(d, d.comps[ci], blk, ss, se, al) else: decodeAcRefine(d, d.comps[ci], blk, ss, se, al)
  var total: int
  var single = scomps.len == 1
  var sbx, sby: int
  if single:
    let c = d.comps[scomps[0]]
    sbx = (int(ceil(float64(d.width) * float64(c.h) / float64(d.hmax))) + 7) div 8
    sby = (int(ceil(float64(d.height) * float64(c.v) / float64(d.vmax))) + 7) div 8
    total = sbx * sby
  else:
    total = d.mcux * d.mcuy
  var n = 0
  var restartsLeft = d.restartInterval
  while n < total:
    if single:
      let ci = scomps[0]
      let bx = n mod sbx
      let by = n div sbx
      blockOp(d, ci, (by * d.comps[ci].bpl + bx) * 64)
    else:
      let mx = n mod d.mcux
      let my = n div d.mcux
      for ci in scomps:
        let c = d.comps[ci]
        for v in 0 ..< c.v:
          for h in 0 ..< c.h:
            let bx = mx * c.h + h
            let by = my * c.v + v
            blockOp(d, ci, (by * c.bpl + bx) * 64)
    inc n
    if d.truncated: return
    if d.restartInterval > 0:
      dec restartsLeft
      if restartsLeft == 0 and n < total:
        # find the RSTn marker
        d.bitBuf = 0
        d.bitCnt = 0
        var p = d.pos
        while p + 1 < d.len and not (d.data[p] == 0xFF and d.data[p + 1] >= 0xD0 and d.data[p + 1] <= 0xD7):
          if d.data[p] == 0xFF and d.data[p + 1] notin {0x00'u8, 0xFF}: break
          inc p
        if p + 1 < d.len and d.data[p] == 0xFF and d.data[p + 1] >= 0xD0 and d.data[p + 1] <= 0xD7:
          d.pos = p + 2
        else:
          d.pos = p
        d.resetBits
        restartsLeft = d.restartInterval
  # skip to the next marker
  d.bitBuf = 0
  d.bitCnt = 0
  while d.pos + 1 < d.len and not (d.data[d.pos] == 0xFF and d.data[d.pos + 1] != 0 and not (d.data[d.pos + 1] >= 0xD0 and d.data[d.pos + 1] <= 0xD7)):
    inc d.pos

var idctCos: array[8, array[8, float32]]
var idctReady: bool

proc initIdct() =
  {.cast(gcsafe).}:
    if idctReady: return
    for x in 0..7:
      for u in 0..7:
        let cu = if u == 0: float32(1.0 / sqrt(2.0)) else: 1.0'f32
        idctCos[x][u] = cu * float32(cos(float64(2 * x + 1) * float64(u) * PI / 16.0)) / 2.0'f32
    idctReady = true

proc idct8x8(coefs: openArray[int16]; off: int; q: array[64, int]; outp: var array[64, uint8]) =
  var tmp: array[64, float32]
  var blk: array[64, float32]
  for i in 0 ..< 64: blk[i] = float32(int(coefs[off + i]) * q[i])
  {.cast(gcsafe).}:
    # rows
    for y in 0..7:
      var nonzero = false
      for u in 1..7:
        if blk[y * 8 + u] != 0: nonzero = true
      if not nonzero:
        let v = blk[y * 8] * idctCos[0][0]
        for x in 0..7: tmp[y * 8 + x] = v
      else:
        for x in 0..7:
          var s = 0.0'f32
          for u in 0..7: s += idctCos[x][u] * blk[y * 8 + u]
          tmp[y * 8 + x] = s
    # columns
    for x in 0..7:
      for y in 0..7:
        var s = 0.0'f32
        for v in 0..7: s += idctCos[y][v] * tmp[v * 8 + x]
        outp[y * 8 + x] = asU8(clamp(round(s + 128.0'f32), 0.0'f32, 255.0'f32))

proc decodeJpeg*(bytes: openArray[byte]; limits: Limits): Image =
  ## Decodes a JPEG file. Raises `CodecError`.
  if bytes.len < 4 or bytes[0] != 0xFF or bytes[1] != 0xD8: raise err("not a JPEG file")
  let m = scanMetadata(bytes)
  if m.frame.isSome:
    let (w, h, nc) = m.frame.get
    limits.checkBytes(w, h, max(nc, 1))
  initIdct()
  var d = Decoder(data: cast[ptr UncheckedArray[byte]](unsafeAddr bytes[0]), len: bytes.len, pos: 2, adobe: -1)
  if m.adobeTransform.isSome: d.adobe = m.adobeTransform.get
  var frameSeen = false
  var anyScan = false
  while d.pos + 1 < d.len:
    if d.data[d.pos] != 0xFF:
      inc d.pos
      continue
    let marker = int(d.data[d.pos + 1])
    d.pos += 2
    case marker
    of 0xFF:
      dec d.pos
    of 0xD8, 0x01, 0xD0..0xD7: discard
    of 0xD9: break
    of 0xDB:
      let len = d.u16at(d.pos)
      var p = d.pos + 2
      let e = d.pos + len
      while p < e and p < d.len:
        let pq = int(d.data[p]) shr 4
        let tq = int(d.data[p]) and 15
        inc p
        if tq > 3: raise err("bad quantization table id")
        for k in 0 ..< 64:
          var v: int
          if pq == 0:
            if p >= d.len: raise err("truncated DQT")
            v = int(d.data[p]); inc p
          else:
            v = d.u16at(p); p += 2
          d.qt[tq][ZigZag[k]] = v
      d.pos = e
    of 0xC4:
      let len = d.u16at(d.pos)
      var p = d.pos + 2
      let e = d.pos + len
      while p < e:
        if p + 17 > d.len: raise err("truncated DHT")
        let tc = int(d.data[p]) shr 4
        let th = int(d.data[p]) and 15
        if th > 3 or tc > 1: raise err("bad Huffman table id")
        var counts: array[16, int]
        var total = 0
        for i in 0..15:
          counts[i] = int(d.data[p + 1 + i])
          total += counts[i]
        p += 17
        if p + total > d.len or total > 256: raise err("truncated DHT")
        var vals = newSeq[uint8](total)
        for i in 0 ..< total: vals[i] = d.data[p + i]
        p += total
        let t = buildTable(counts, vals)
        if tc == 0: d.dc[th] = t else: d.ac[th] = t
      d.pos = e
    of 0xC0, 0xC1, 0xC2:
      if frameSeen: raise err("multiple frames")
      frameSeen = true
      d.progressive = marker == 0xC2
      let len = d.u16at(d.pos)
      if d.pos + len > d.len or len < 8: raise err("truncated SOF")
      if d.data[d.pos + 2] != 8: raise unsupported(fmtJpeg, "only 8-bit precision is supported")
      d.height = d.u16at(d.pos + 3)
      d.width = d.u16at(d.pos + 5)
      let nc = int(d.data[d.pos + 7])
      if d.width == 0 or d.height == 0: raise err("zero-sized frame")
      if nc notin [1, 3, 4]: raise unsupported(fmtJpeg, $nc & " components")
      if len < 8 + nc * 3: raise err("truncated SOF")
      d.hmax = 1
      d.vmax = 1
      for i in 0 ..< nc:
        let o = d.pos + 8 + i * 3
        var c = Component(id: int(d.data[o]), h: int(d.data[o + 1]) shr 4, v: int(d.data[o + 1]) and 15, tq: int(d.data[o + 2]) and 3)
        if c.h < 1 or c.h > 4 or c.v < 1 or c.v > 4: raise err("bad sampling factors")
        d.hmax = max(d.hmax, c.h)
        d.vmax = max(d.vmax, c.v)
        d.comps.add c
      limits.checkBytes(d.width, d.height, nc)
      d.mcux = (d.width + 8 * d.hmax - 1) div (8 * d.hmax)
      d.mcuy = (d.height + 8 * d.vmax - 1) div (8 * d.vmax)
      for c in d.comps.mitems:
        c.bpl = d.mcux * c.h
        c.bpc = d.mcuy * c.v
        c.coefs = newSeq[int16](c.bpl * c.bpc * 64)
      d.pos += len
    of 0xC3, 0xC5..0xC7, 0xC9..0xCB, 0xCD..0xCF:
      raise unsupported(fmtJpeg, "lossless, hierarchical and arithmetic-coded JPEG are not supported")
    of 0xDD:
      d.restartInterval = d.u16at(d.pos + 2)
      d.pos += d.u16at(d.pos)
    of 0xDA:
      if not frameSeen: raise err("scan before frame")
      let len = d.u16at(d.pos)
      let ns = int(d.data[d.pos + 2])
      if ns < 1 or ns > 4 or len < 6 + 2 * ns: raise err("bad scan header")
      var scomps: seq[int]
      for i in 0 ..< ns:
        let cid = int(d.data[d.pos + 3 + 2 * i])
        let tbl = int(d.data[d.pos + 4 + 2 * i])
        var found = -1
        for k, c in d.comps:
          if c.id == cid: found = k
        if found < 0: raise err("scan references an unknown component")
        d.comps[found].dcTable = (tbl shr 4) and 3
        d.comps[found].acTable = tbl and 3
        scomps.add found
      let ss = int(d.data[d.pos + 3 + 2 * ns])
      let se = int(d.data[d.pos + 4 + 2 * ns])
      let ah = int(d.data[d.pos + 5 + 2 * ns]) shr 4
      let al = int(d.data[d.pos + 5 + 2 * ns]) and 15
      d.pos += len
      if d.pos >= d.len:
        d.truncated = true
        break
      anyScan = true
      decodeScan(d, scomps, ss, min(se, 63), ah, al)
      if d.truncated: break
    else:
      if d.pos + 2 > d.len: break
      d.pos += d.u16at(d.pos)
  if not frameSeen: raise err("no frame")
  if not anyScan: raise err("the file ends before any image data")
  # IDCT each component into a plane
  let nc = d.comps.len
  var planes = newSeq[seq[uint8]](nc)
  var blk: array[64, uint8]
  for ci in 0 ..< nc:
    let c = d.comps[ci]
    let pw = c.bpl * 8
    var plane = newSeq[uint8](pw * c.bpc * 8)
    for by in 0 ..< c.bpc:
      for bx in 0 ..< c.bpl:
        idct8x8(c.coefs, (by * c.bpl + bx) * 64, d.qt[c.tq], blk)
        for y in 0..7:
          let row = (by * 8 + y) * pw + bx * 8
          for x in 0..7: plane[row + x] = blk[y * 8 + x]
    planes[ci] = plane
  # colour conversion
  let w = d.width
  let h = d.height
  let layout = if nc == 1: clGray elif nc == 4: clCmyk else: clRgb
  var img = newImage(w, h, layout, isU8)
  proc sampleAt(ci, x, y: int): float32 =
    let c = d.comps[ci]
    let pw = c.bpl * 8
    if c.h == d.hmax and c.v == d.vmax:
      return float32(planes[ci][y * pw + x])
    # bilinear (centred) upsampling
    let sx = (float32(x) + 0.5'f32) * float32(c.h) / float32(d.hmax) - 0.5'f32
    let sy = (float32(y) + 0.5'f32) * float32(c.v) / float32(d.vmax) - 0.5'f32
    let maxX = (w * c.h + d.hmax - 1) div d.hmax - 1
    let maxY = (h * c.v + d.vmax - 1) div d.vmax - 1
    let x0 = clamp(int(floor(sx)), 0, maxX)
    let y0 = clamp(int(floor(sy)), 0, maxY)
    let x1 = min(x0 + 1, maxX)
    let y1 = min(y0 + 1, maxY)
    let fx = clamp(sx - float32(x0), 0.0'f32, 1.0'f32)
    let fy = clamp(sy - float32(y0), 0.0'f32, 1.0'f32)
    let a = float32(planes[ci][y0 * pw + x0])
    let b = float32(planes[ci][y0 * pw + x1])
    let cc = float32(planes[ci][y1 * pw + x0])
    let dd = float32(planes[ci][y1 * pw + x1])
    (a * (1 - fx) + b * fx) * (1 - fy) + (cc * (1 - fx) + dd * fx) * fy
  proc clip(v: float32): uint8 {.inline.} = asU8(clamp(round(v), 0.0'f32, 255.0'f32))
  let transform = if nc == 3: (if d.adobe == 0: 0 else: 1)
                  elif nc == 4: (if d.adobe == 2: 2 else: 0)
                  else: 0
  for y in 0 ..< h:
    for x in 0 ..< w:
      let o = (y * w + x) * layout.channels
      case nc
      of 1: img.data[o] = clip(sampleAt(0, x, y))
      of 3:
        let yy = sampleAt(0, x, y)
        let cb = sampleAt(1, x, y)
        let cr = sampleAt(2, x, y)
        if transform == 1:
          img.data[o] = clip(yy + 1.402'f32 * (cr - 128))
          img.data[o + 1] = clip(yy - 0.344136'f32 * (cb - 128) - 0.714136'f32 * (cr - 128))
          img.data[o + 2] = clip(yy + 1.772'f32 * (cb - 128))
        else:
          img.data[o] = clip(yy); img.data[o + 1] = clip(cb); img.data[o + 2] = clip(cr)
      else:
        let a = sampleAt(0, x, y)
        let b = sampleAt(1, x, y)
        let c = sampleAt(2, x, y)
        let k = sampleAt(3, x, y)
        if transform == 2:
          # YCCK: convert to (inverted) CMY then un-invert everything to ink amounts
          let cb = b - 128
          let cr = c - 128
          let r = a + 1.402'f32 * cr
          let g = a - 0.344136'f32 * cb - 0.714136'f32 * cr
          let bb = a + 1.772'f32 * cb
          img.data[o] = clip(r); img.data[o + 1] = clip(g); img.data[o + 2] = clip(bb)
          img.data[o + 3] = 255'u8 - clip(k)
        else:
          # Adobe-style CMYK is stored inverted (255 = no ink)
          img.data[o] = 255'u8 - clip(a); img.data[o + 1] = 255'u8 - clip(b)
          img.data[o + 2] = 255'u8 - clip(c); img.data[o + 3] = 255'u8 - clip(k)
  img.icc = m.icc
  img.meta = Metadata(exif: m.exif, xmp: m.xmp, dpi: m.dpi)
  if d.truncated: img.warnings.add DecodeWarning(kind: dwTruncated, format: fmtJpeg)
  img

# ---------------------------------------------------------------- encoder

const
  StdLumQ: array[64, int] = [16, 11, 10, 16, 24, 40, 51, 61, 12, 12, 14, 19, 26, 58, 60, 55, 14, 13, 16, 24, 40, 57, 69, 56,
    14, 17, 22, 29, 51, 87, 80, 62, 18, 22, 37, 56, 68, 109, 103, 77, 24, 35, 55, 64, 81, 104, 113, 92, 49, 64, 78, 87,
    103, 121, 120, 101, 72, 92, 95, 98, 112, 100, 103, 99]
  StdChrQ: array[64, int] = [17, 18, 24, 47, 99, 99, 99, 99, 18, 21, 26, 66, 99, 99, 99, 99, 24, 26, 56, 99, 99, 99, 99, 99,
    47, 66, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99]
  DcLumBits = [0, 1, 5, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0]
  DcLumVals = [0'u8, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
  DcChrBits = [0, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0]
  DcChrVals = [0'u8, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
  AcLumBits = [0, 2, 1, 3, 3, 2, 4, 3, 5, 5, 4, 4, 0, 0, 1, 0x7d]
  AcLumVals = [0x01'u8, 0x02, 0x03, 0x00, 0x04, 0x11, 0x05, 0x12, 0x21, 0x31, 0x41, 0x06, 0x13, 0x51, 0x61, 0x07, 0x22, 0x71,
    0x14, 0x32, 0x81, 0x91, 0xa1, 0x08, 0x23, 0x42, 0xb1, 0xc1, 0x15, 0x52, 0xd1, 0xf0, 0x24, 0x33, 0x62, 0x72, 0x82,
    0x09, 0x0a, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39,
    0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x63, 0x64,
    0x65, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x83, 0x84, 0x85, 0x86, 0x87,
    0x88, 0x89, 0x8a, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8,
    0xa9, 0xaa, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9,
    0xca, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9,
    0xea, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa]
  AcChrBits = [0, 2, 1, 2, 4, 4, 3, 4, 7, 5, 4, 4, 0, 1, 2, 0x77]
  AcChrVals = [0x00'u8, 0x01, 0x02, 0x03, 0x11, 0x04, 0x05, 0x21, 0x31, 0x06, 0x12, 0x41, 0x51, 0x07, 0x61, 0x71, 0x13, 0x22,
    0x32, 0x81, 0x08, 0x14, 0x42, 0x91, 0xa1, 0xb1, 0xc1, 0x09, 0x23, 0x33, 0x52, 0xf0, 0x15, 0x62, 0x72, 0xd1, 0x0a,
    0x16, 0x24, 0x34, 0xe1, 0x25, 0xf1, 0x17, 0x18, 0x19, 0x1a, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x35, 0x36, 0x37, 0x38,
    0x39, 0x3a, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x63,
    0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x82, 0x83, 0x84, 0x85,
    0x86, 0x87, 0x88, 0x89, 0x8a, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6,
    0xa7, 0xa8, 0xa9, 0xaa, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
    0xc8, 0xc9, 0xca, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8,
    0xe9, 0xea, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa]

type
  EncTable = object
    code: array[256, int]
    size: array[256, int]
  BitWriter = object
    buf: seq[byte]
    acc: uint32
    cnt: int

proc encTable(bits: openArray[int]; vals: openArray[uint8]): EncTable =
  var code = 0
  var k = 0
  for l in 1..16:
    for _ in 0 ..< bits[l - 1]:
      result.code[vals[k]] = code
      result.size[vals[k]] = l
      inc code
      inc k
    code = code shl 1

proc put(w: var BitWriter; code, size: int) {.inline.} =
  w.acc = (w.acc shl size) or uint32(code and ((1 shl size) - 1))
  w.cnt += size
  while w.cnt >= 8:
    let b = byte((w.acc shr (w.cnt - 8)) and 0xFF)
    w.buf.add b
    if b == 0xFF: w.buf.add 0
    w.cnt -= 8

proc flush(w: var BitWriter) =
  if w.cnt > 0: w.put((1 shl (8 - w.cnt)) - 1, 8 - w.cnt)

proc scaledQ(base: array[64, int]; quality: int): array[64, int] =
  let q = clamp(quality, 1, 100)
  let s = if q < 50: 5000 div q else: 200 - 2 * q
  for i in 0 ..< 64: result[i] = clamp((base[i] * s + 50) div 100, 1, 255)

proc magnitude(v: int): (int, int) {.inline.} =
  var a = abs(v)
  var n = 0
  while a > 0:
    inc n
    a = a shr 1
  let bits = if v < 0: v + (1 shl n) - 1 else: v
  (n, bits)

proc fdctQuant(blk: array[64, float32]; q: array[64, int]; outp: var array[64, int]) =
  var tmp: array[64, float32]
  {.cast(gcsafe).}:
    for y in 0..7:
      for u in 0..7:
        var s = 0.0'f32
        for x in 0..7: s += idctCos[x][u] * blk[y * 8 + x]
        tmp[y * 8 + u] = s
    for u in 0..7:
      for v in 0..7:
        var s = 0.0'f32
        for y in 0..7: s += idctCos[y][v] * tmp[y * 8 + u]
        outp[v * 8 + u] = int(round(s / float32(q[v * 8 + u])))

proc writeSegment(o: var seq[byte]; marker: int; payload: openArray[byte]) =
  o.add [0xFF'u8, byte(marker)]
  let n = payload.len + 2
  o.add [byte(n shr 8), byte(n and 0xFF)]
  o.add payload

proc exifSegment*(exif: seq[byte]; dpi: Option[(float32, float32)]): Option[seq[byte]] =
  ## The EXIF APP1 payload, or none when it doesn't fit in one segment.
  var seg = ExifHeader.toBytes
  seg.add exportExif(exif, dpi)
  if seg.len <= MaxSegment: some(seg) else: none(seq[byte])

proc xmpSegment*(xmp: string; dpi: Option[(float32, float32)]): Option[seq[byte]] =
  var seg = XmpHeader.toBytes
  seg.add exportXmp(xmp, dpi).toBytes
  if seg.len <= MaxSegment: some(seg) else: none(seq[byte])

proc encodeJpeg*(src: Image; layout: ChannelLayout; opts: EncodeOptions): seq[byte] =
  ## Baseline JPEG of `src` converted to `layout` (Gray, Rgb or Cmyk).
  let img = src.convert(layout, isU8)
  if img.width > 65535 or img.height > 65535: raise encodeErr(fmtJpeg, "JPEG dimensions are limited to 65535")
  if layout notin [clGray, clRgb, clCmyk]: raise encodeErr(fmtJpeg, "unsupported layout " & $layout)
  initIdct()
  let w = img.width
  let h = img.height
  let nc = layout.channels
  let sub = opts.jpegChromaSubsampling and layout == clRgb
  let ql = scaledQ(StdLumQ, opts.jpegQuality)
  let qc = scaledQ(StdChrQ, opts.jpegQuality)
  var o = @[0xFF'u8, 0xD8]
  # APP0 JFIF
  var jfif = "JFIF\0".toBytes
  jfif.add [1'u8, 1]
  var dx, dy = 1
  var unit = 0'u8
  if opts.embedMetadata and img.meta.dpi.isSome and img.meta.dpi.get[0] >= 1 and img.meta.dpi.get[1] >= 1:
    unit = 1
    dx = int(min(round(img.meta.dpi.get[0]), 65535.0'f32))
    dy = int(min(round(img.meta.dpi.get[1]), 65535.0'f32))
  jfif.add [unit, byte(dx shr 8), byte(dx and 0xFF), byte(dy shr 8), byte(dy and 0xFF), 0, 0]
  o.writeSegment(0xE0, jfif)
  if layout == clCmyk:
    o.writeSegment(0xEE, "Adobe".toBytes & @[0'u8, 100, 0, 0, 0, 0, 0])
  if opts.embedMetadata:
    if img.meta.exif.isSome:
      let s = exifSegment(img.meta.exif.get, img.meta.dpi)
      if s.isSome: o.writeSegment(0xE1, s.get)
    if img.meta.xmp.isSome:
      let s = xmpSegment(img.meta.xmp.get, img.meta.dpi)
      if s.isSome: o.writeSegment(0xE1, s.get)
  if opts.embedIcc and img.icc.isSome:
    let icc = img.icc.get
    const chunkMax = 65519
    let parts = max(1, (icc.len + chunkMax - 1) div chunkMax)
    if parts <= 255:
      for k in 0 ..< parts:
        var seg = IccHeader.toBytes
        seg.add [byte(k + 1), byte(parts)]
        seg.add icc.toOpenArray(k * chunkMax, min(icc.len, (k + 1) * chunkMax) - 1)
        o.writeSegment(0xE2, seg)
  # DQT
  var dqt: seq[byte]
  dqt.add 0
  for k in 0 ..< 64: dqt.add byte(ql[ZigZag[k]])
  if nc == 3:
    dqt.add 1
    for k in 0 ..< 64: dqt.add byte(qc[ZigZag[k]])
  o.writeSegment(0xDB, dqt)
  # SOF0
  var sof = @[8'u8, byte(h shr 8), byte(h and 0xFF), byte(w shr 8), byte(w and 0xFF), byte(nc)]
  for c in 0 ..< nc:
    let samp = if c == 0 and sub: 0x22'u8 else: 0x11'u8
    let tq = if nc == 3 and c > 0: 1'u8 else: 0'u8
    sof.add [byte(c + 1), samp, tq]
  o.writeSegment(0xC0, sof)
  # DHT
  proc dht(o: var seq[byte]; cls, id: int; bits: openArray[int]; vals: openArray[uint8]) =
    var p = @[byte((cls shl 4) or id)]
    for b in bits: p.add byte(b)
    p.add vals
    o.writeSegment(0xC4, p)
  o.dht(0, 0, DcLumBits, DcLumVals)
  o.dht(1, 0, AcLumBits, AcLumVals)
  if nc == 3:
    o.dht(0, 1, DcChrBits, DcChrVals)
    o.dht(1, 1, AcChrBits, AcChrVals)
  # SOS
  var sos = @[byte(nc)]
  for c in 0 ..< nc:
    sos.add [byte(c + 1), (if nc == 3 and c > 0: 0x11'u8 else: 0x00'u8)]
  sos.add [0'u8, 63, 0]
  o.writeSegment(0xDA, sos)
  let dcL = encTable(DcLumBits, DcLumVals)
  let acL = encTable(AcLumBits, AcLumVals)
  let dcC = encTable(DcChrBits, DcChrVals)
  let acC = encTable(AcChrBits, AcChrVals)
  var bw: BitWriter
  bw.buf = newSeqOfCap[byte](w * h)
  # component planes
  var planes = newSeq[seq[float32]](nc)
  for c in 0 ..< nc: planes[c] = newSeq[float32](w * h)
  for i in 0 ..< w * h:
    let p = i * nc
    if nc == 3:
      let r = float32(img.data[p])
      let g = float32(img.data[p + 1])
      let b = float32(img.data[p + 2])
      planes[0][i] = 0.299'f32 * r + 0.587'f32 * g + 0.114'f32 * b
      planes[1][i] = -0.168736'f32 * r - 0.331264'f32 * g + 0.5'f32 * b + 128
      planes[2][i] = 0.5'f32 * r - 0.418688'f32 * g - 0.081312'f32 * b + 128
    elif nc == 4:
      for k in 0..3: planes[k][i] = 255.0'f32 - float32(img.data[p + k])  # Adobe-inverted
    else:
      planes[0][i] = float32(img.data[p])
  var preds: array[4, int]
  proc encodeBlock(bw: var BitWriter; blk: array[64, float32]; q: array[64, int]; dc, ac: EncTable; pred: var int) =
    var co: array[64, int]
    var shifted = blk
    for k in 0 ..< 64: shifted[k] -= 128
    fdctQuant(shifted, q, co)
    let diff = co[0] - pred
    pred = co[0]
    let (n, bits) = magnitude(diff)
    bw.put(dc.code[n], dc.size[n])
    if n > 0: bw.put(bits, n)
    var run = 0
    for k in 1 ..< 64:
      let v = co[ZigZag[k]]
      if v == 0:
        inc run
      else:
        while run >= 16:
          bw.put(ac.code[0xF0], ac.size[0xF0])
          run -= 16
        let (s, vb) = magnitude(v)
        let sym = (run shl 4) or s
        bw.put(ac.code[sym], ac.size[sym])
        bw.put(vb, s)
        run = 0
    if run > 0: bw.put(ac.code[0], ac.size[0])
  proc blockAt(plane: seq[float32]; bx, by, scale: int): array[64, float32] =
    for y in 0..7:
      for x in 0..7:
        if scale == 1:
          let sx = min(bx * 8 + x, w - 1)
          let sy = min(by * 8 + y, h - 1)
          result[y * 8 + x] = plane[sy * w + sx]
        else:
          var s = 0.0'f32
          for dy2 in 0..1:
            for dx2 in 0..1:
              let sx = min((bx * 8 + x) * 2 + dx2, w - 1)
              let sy = min((by * 8 + y) * 2 + dy2, h - 1)
              s += plane[sy * w + sx]
          result[y * 8 + x] = s / 4
  if sub:
    let mx = (w + 15) div 16
    let my = (h + 15) div 16
    for by in 0 ..< my:
      for bx in 0 ..< mx:
        for v in 0..1:
          for u in 0..1:
            encodeBlock(bw, blockAt(planes[0], bx * 2 + u, by * 2 + v, 1), ql, dcL, acL, preds[0])
        encodeBlock(bw, blockAt(planes[1], bx, by, 2), qc, dcC, acC, preds[1])
        encodeBlock(bw, blockAt(planes[2], bx, by, 2), qc, dcC, acC, preds[2])
  else:
    let mx = (w + 7) div 8
    let my = (h + 7) div 8
    for by in 0 ..< my:
      for bx in 0 ..< mx:
        for c in 0 ..< nc:
          if nc == 3 and c > 0: encodeBlock(bw, blockAt(planes[c], bx, by, 1), qc, dcC, acC, preds[c])
          else: encodeBlock(bw, blockAt(planes[c], bx, by, 1), ql, dcL, acL, preds[c])
  bw.flush
  o.add bw.buf
  o.add [0xFF'u8, 0xD9]
  o
