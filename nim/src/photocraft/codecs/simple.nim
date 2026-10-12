## The smaller raster formats, each written from its public description: BMP (Windows bitmap:
## 1/4/8/16/24/32-bit, RLE4/RLE8, bitfields), GIF (87a/89a, LZW, first frame), TGA (Truevision:
## colour-mapped, true-colour, gray, RLE), ICO (BMP or PNG entries), Netpbm (P1–P7, PFM), QOI,
## Radiance HDR (RGBE, flat and RLE scanlines) and FITS (primary HDU / first IMAGE extension).

import std/[options, strutils, math, algorithm, tables]
import ./image, ./png, ../zlib

# ---------------------------------------------------------------- helpers

proc le16(b: openArray[byte]; p: int): int {.inline.} = int(b[p]) or (int(b[p + 1]) shl 8)
proc le32(b: openArray[byte]; p: int): int {.inline.} =
  int(cast[int32](uint32(b[p]) or (uint32(b[p + 1]) shl 8) or (uint32(b[p + 2]) shl 16) or (uint32(b[p + 3]) shl 24)))
proc leU32(b: openArray[byte]; p: int): uint32 {.inline.} =
  uint32(b[p]) or (uint32(b[p + 1]) shl 8) or (uint32(b[p + 2]) shl 16) or (uint32(b[p + 3]) shl 24)
proc put16(o: var seq[byte]; v: int) = o.add [byte(v and 0xFF), byte((v shr 8) and 0xFF)]
proc put32(o: var seq[byte]; v: int) = o.add [byte(v and 0xFF), byte((v shr 8) and 0xFF), byte((v shr 16) and 0xFF), byte((v shr 24) and 0xFF)]

template need(f: Format; b: openArray[byte]; n: int) =
  if n > b.len or n < 0: raise malformed(f, "file is truncated")

# ---------------------------------------------------------------- BMP

proc decodeBmpAt(b: openArray[byte]; start: int; limits: Limits; icoMask: bool): Image =
  const F = fmtBmp
  need(F, b, start + 40)
  let dib = start
  let hsize = le32(b, dib)
  var w, h, bpp, comp, nColors: int
  var topDown = false
  if hsize == 12:
    w = le16(b, dib + 4); h = le16(b, dib + 6); bpp = le16(b, dib + 10)
  else:
    if hsize < 40: raise malformed(F, "unknown header size " & $hsize)
    w = le32(b, dib + 4); h = le32(b, dib + 8); bpp = le16(b, dib + 14); comp = le32(b, dib + 16)
    nColors = le32(b, dib + 32)
  if h < 0:
    topDown = true
    h = -h
  if icoMask: h = h div 2
  if w <= 0 or h <= 0: raise malformed(F, "zero-sized image")
  limits.checkBytes(w, h, 4)
  var masks = [0'u32, 0, 0, 0]
  var hasMasks = false
  if comp == 3 or comp == 6:
    if hsize >= 52:
      for k in 0..2: masks[k] = leU32(b, dib + 40 + 4 * k)
      if hsize >= 56: masks[3] = leU32(b, dib + 52)
    else:
      need(F, b, dib + 40 + 12)
      for k in 0..2: masks[k] = leU32(b, dib + 40 + 4 * k)
    hasMasks = true
  elif bpp == 16:
    masks = [0x7C00'u32, 0x03E0, 0x001F, 0]
    hasMasks = true
  elif bpp == 32:
    masks = [0x00FF0000'u32, 0x0000FF00, 0x000000FF, (if icoMask: 0xFF000000'u32 else: 0'u32)]
    hasMasks = true
  var palette: seq[array[4, uint8]]
  if bpp <= 8:
    let entries = if nColors > 0: min(nColors, 256) else: 1 shl bpp
    let psz = if hsize == 12: 3 else: 4
    let pstart = dib + hsize + (if comp == 3 and hsize == 40: 12 else: 0)
    need(F, b, pstart + entries * psz)
    for i in 0 ..< entries: palette.add [b[pstart + i * psz + 2], b[pstart + i * psz + 1], b[pstart + i * psz], 255'u8]
  var dataOff: int
  if icoMask:
    dataOff = dib + hsize + palette.len * 4 + (if hasMasks and comp == 3 and hsize == 40: 12 else: 0)
  else:
    dataOff = le32(b, start - 14 + 10)
  let alpha = (hasMasks and masks[3] != 0) or icoMask
  var img = newImage(w, h, (if alpha: clRgba else: clRgb), isU8)
  let ch = img.layout.channels
  proc setPx(img: var Image; x, y: int; r, g, b2, a: uint8) =
    let yy = if topDown: y else: h - 1 - y
    let o = (yy * w + x) * ch
    img.data[o] = r; img.data[o + 1] = g; img.data[o + 2] = b2
    if ch == 4: img.data[o + 3] = a
  proc maskVal(v, m: uint32): uint8 =
    if m == 0: return 255
    var shift = 0
    var mm = m
    while (mm and 1) == 0:
      mm = mm shr 1
      inc shift
    let maxv = mm
    uint8((uint64((v and m) shr shift) * 255 + maxv div 2) div maxv)
  if comp == 1 or comp == 2:
    # RLE8 / RLE4
    var p = dataOff
    var x = 0
    var y = 0
    while p + 1 < b.len and y < h:
      let n = int(b[p]); let c = int(b[p + 1]); p += 2
      if n > 0:
        for k in 0 ..< n:
          if x >= w: break
          let idx = if comp == 1: c else: (if k mod 2 == 0: c shr 4 else: c and 15)
          if idx < palette.len:
            let pc = palette[idx]
            setPx(img, x, y, pc[0], pc[1], pc[2], 255)
          inc x
      else:
        case c
        of 0: x = 0; inc y
        of 1: break
        of 2:
          if p + 1 >= b.len: break
          x += int(b[p]); y += int(b[p + 1]); p += 2
        else:
          for k in 0 ..< c:
            let idx = if comp == 1:
                if p + k >= b.len: 0 else: int(b[p + k])
              else:
                let bb = if p + k div 2 >= b.len: 0 else: int(b[p + k div 2])
                if k mod 2 == 0: bb shr 4 else: bb and 15
            if x < w and idx < palette.len:
              let pc = palette[idx]
              setPx(img, x, y, pc[0], pc[1], pc[2], 255)
            inc x
          let used = if comp == 1: c else: (c + 1) div 2
          p += used + (used and 1)
    return img
  let stride = ((w * bpp + 31) div 32) * 4
  need(F, b, dataOff + stride * h)
  for y in 0 ..< h:
    let row = dataOff + y * stride
    for x in 0 ..< w:
      case bpp
      of 1, 2, 4, 8:
        let bit = x * bpp
        let v = (int(b[row + bit div 8]) shr (8 - bpp - bit mod 8)) and ((1 shl bpp) - 1)
        let pc = if v < palette.len: palette[v] else: [0'u8, 0, 0, 255]
        setPx(img, x, y, pc[0], pc[1], pc[2], 255)
      of 16:
        let v = uint32(le16(b, row + 2 * x))
        setPx(img, x, y, maskVal(v, masks[0]), maskVal(v, masks[1]), maskVal(v, masks[2]), maskVal(v, masks[3]))
      of 24:
        let o = row + 3 * x
        setPx(img, x, y, b[o + 2], b[o + 1], b[o], 255)
      of 32:
        let v = leU32(b, row + 4 * x)
        if hasMasks: setPx(img, x, y, maskVal(v, masks[0]), maskVal(v, masks[1]), maskVal(v, masks[2]), maskVal(v, masks[3]))
        else: setPx(img, x, y, b[row + 4 * x + 2], b[row + 4 * x + 1], b[row + 4 * x], 255)
      else: raise unsupported(F, $bpp & " bits per pixel")
  if icoMask and bpp < 32:
    # AND mask: 1-bit transparency after the colour data
    let mstride = ((w + 31) div 32) * 4
    let moff = dataOff + stride * h
    if moff + mstride * h <= b.len:
      for y in 0 ..< h:
        for x in 0 ..< w:
          let bit = (int(b[moff + y * mstride + x div 8]) shr (7 - x mod 8)) and 1
          let yy = if topDown: y else: h - 1 - y
          img.data[(yy * w + x) * 4 + 3] = if bit == 1: 0 else: 255
  img

proc decodeBmp*(b: openArray[byte]; limits: Limits): Image =
  if b.len < 54 or b[0] != byte('B') or b[1] != byte('M'): raise malformed(fmtBmp, "not a BMP file")
  decodeBmpAt(b, 14, limits, false)

proc encodeBmp*(src: Image; layout: ChannelLayout): seq[byte] =
  ## 24-bit (Rgb) or 32-bit BITFIELDS (Rgba) bottom-up BMP.
  let alpha = layout.hasAlpha
  let img = src.convert(if alpha: clRgba else: clRgb, isU8)
  let w = img.width
  let h = img.height
  let bpp = if alpha: 32 else: 24
  let stride = ((w * bpp + 31) div 32) * 4
  let hsize = if alpha: 108 else: 40
  let dataOff = 14 + hsize
  result.add "BM".toBytes
  result.put32(dataOff + stride * h)
  result.put32(0)
  result.put32(dataOff)
  result.put32(hsize)
  result.put32(w); result.put32(h)
  result.put16(1); result.put16(bpp)
  result.put32(if alpha: 3 else: 0)
  result.put32(stride * h)
  result.put32(2835); result.put32(2835)
  result.put32(0); result.put32(0)
  if alpha:
    result.put32(0x00FF0000); result.put32(0x0000FF00); result.put32(0x000000FF); result.put32(int(0xFF000000'u32))
    result.add "BGRs".toBytes.reversed
    for _ in 0 ..< 48: result.add 0
  for y in countdown(h - 1, 0):
    let start = result.len
    for x in 0 ..< w:
      let o = (y * w + x) * img.layout.channels
      result.add [img.data[o + 2], img.data[o + 1], img.data[o]]
      if alpha: result.add img.data[o + 3]
    while result.len - start < stride: result.add 0

# ---------------------------------------------------------------- GIF

proc lzwDecode(data: seq[byte]; minCode: int; maxOut: int): seq[byte] =
  let clear = 1 shl minCode
  let eoi = clear + 1
  var prefix = newSeq[int](4096)
  var suffix = newSeq[byte](4096)
  var lengths = newSeq[int](4096)
  for i in 0 ..< clear:
    suffix[i] = byte(i)
    lengths[i] = 1
  var codeSize = minCode + 1
  var next = eoi + 1
  var prev = -1
  var bitPos = 0
  var stack: seq[byte]
  while true:
    if (bitPos + codeSize + 7) div 8 > data.len: break
    var code = 0
    for k in 0 ..< codeSize:
      let bp = bitPos + k
      code = code or (((int(data[bp div 8]) shr (bp mod 8)) and 1) shl k)
    bitPos += codeSize
    if code == clear:
      codeSize = minCode + 1
      next = eoi + 1
      prev = -1
      continue
    if code == eoi: break
    if prev < 0:
      if code >= clear: break
      result.add byte(code)
      prev = code
      continue
    var cur = code
    var first: byte
    if code < next:
      stack.setLen(0)
      var c = code
      while c >= clear and stack.len < 4096:
        stack.add suffix[c]
        c = prefix[c]
      stack.add byte(c)
      first = byte(c)
      for i in countdown(stack.len - 1, 0): result.add stack[i]
    elif code == next:
      stack.setLen(0)
      var c = prev
      while c >= clear and stack.len < 4096:
        stack.add suffix[c]
        c = prefix[c]
      stack.add byte(c)
      first = byte(c)
      for i in countdown(stack.len - 1, 0): result.add stack[i]
      result.add first
    else: break
    if next < 4096:
      prefix[next] = prev
      suffix[next] = first
      inc next
      if next == (1 shl codeSize) and codeSize < 12: inc codeSize
    prev = cur
    if result.len > maxOut: break

proc decodeGif*(b0: openArray[byte]; limits: Limits): Image =
  let bp = if b0.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr b0[0]) else: nil
  let bl = b0.len
  template b: untyped = toOpenArray(bp, 0, bl - 1)
  const F = fmtGif
  if b.len < 13: raise malformed(F, "not a GIF file")
  let sw = le16(b, 6)
  let sh = le16(b, 8)
  let flags = int(b[10])
  let bg = int(b[11])
  var p = 13
  var gct: seq[array[3, uint8]]
  if (flags and 0x80) != 0:
    let n = 1 shl ((flags and 7) + 1)
    need(F, b, p + 3 * n)
    for i in 0 ..< n: gct.add [b[p + 3 * i], b[p + 3 * i + 1], b[p + 3 * i + 2]]
    p += 3 * n
  discard bg
  if sw <= 0 or sh <= 0: raise malformed(F, "zero-sized image")
  limits.checkBytes(sw, sh, 4)
  var img = newImage(sw, sh, clRgba, isU8)
  var transparent = -1
  var frames = 0
  var decoded = false
  var complete = false
  while p < b.len:
    let blk = b[p]
    inc p
    if blk == 0x3B:
      complete = true
      break
    elif blk == 0x21:
      if p >= b.len: break
      let label = b[p]
      inc p
      var first = true
      while p < b.len:
        let n = int(b[p])
        inc p
        if n == 0: break
        if label == 0xF9 and first and n >= 4 and p + 3 < b.len and not decoded:
          if (b[p] and 1) != 0: transparent = int(b[p + 3])
        first = false
        p += n
    elif blk == 0x2C:
      inc frames
      need(F, b, p + 9)
      let fx = le16(b, p); let fy = le16(b, p + 2); let fw = le16(b, p + 4); let fh = le16(b, p + 6)
      let fflags = int(b[p + 8])
      p += 9
      var lct = gct
      if (fflags and 0x80) != 0:
        let n = 1 shl ((fflags and 7) + 1)
        need(F, b, p + 3 * n)
        lct = @[]
        for i in 0 ..< n: lct.add [b[p + 3 * i], b[p + 3 * i + 1], b[p + 3 * i + 2]]
        p += 3 * n
      need(F, b, p + 1)
      let minCode = int(b[p])
      inc p
      var data: seq[byte]
      while p < b.len:
        let n = int(b[p])
        inc p
        if n == 0: break
        if p + n > b.len: break
        data.add b.toOpenArray(p, p + n - 1)
        p += n
      if not decoded:
        if minCode < 2 or minCode > 11: raise malformed(F, "bad LZW code size")
        let px = lzwDecode(data, minCode, fw * fh)
        let interlaced = (fflags and 0x40) != 0
        var rows: seq[int]
        if interlaced:
          for (start, step) in [(0, 8), (4, 8), (2, 4), (1, 2)]:
            var r = start
            while r < fh:
              rows.add r
              r += step
        else:
          for r in 0 ..< fh: rows.add r
        for i in 0 ..< min(px.len, fw * fh):
          let ry = rows[i div fw]
          let x = fx + i mod fw
          let y = fy + ry
          if x >= sw or y >= sh: continue
          let idx = int(px[i])
          let o = (y * sw + x) * 4
          if idx == transparent or idx >= lct.len: continue
          img.data[o] = lct[idx][0]; img.data[o + 1] = lct[idx][1]; img.data[o + 2] = lct[idx][2]; img.data[o + 3] = 255
        decoded = true
    else: break
  if not decoded: raise malformed(F, "no image")
  if frames > 1 or not complete:
    img.warnings.add DecodeWarning(kind: dwMoreFrames, total: (if complete: some(frames) else: none(int)))
    if frames <= 1: img.warnings.setLen(0)
  img

proc quantizeMedianCut*(px: seq[array[4, uint8]]; maxColors: int): seq[array[3, uint8]] =
  ## A median-cut palette of the opaque pixels (≤ maxColors entries).
  var counts = initTable[uint32, int]()
  for p in px:
    if p[3] >= 128:
      let k = (uint32(p[0]) shl 16) or (uint32(p[1]) shl 8) or uint32(p[2])
      counts.mgetOrPut(k, 0) += 1
  if counts.len <= maxColors:
    for k in counts.keys: result.add [uint8(k shr 16), uint8((k shr 8) and 0xFF), uint8(k and 0xFF)]
    result.sort(proc(a, b: array[3, uint8]): int = cmp((a[0], a[1], a[2]), (b[0], b[1], b[2])))
    return
  type Box = seq[(array[3, int], int)]
  var colors: Box
  for k, c in counts: colors.add ([int(k shr 16), int((k shr 8) and 0xFF), int(k and 0xFF)], c)
  var boxes = @[colors]
  while boxes.len < maxColors:
    var bi = -1
    var bestRange = -1
    var bestAxis = 0
    for i, bx in boxes:
      if bx.len < 2: continue
      for a in 0..2:
        var lo = 255
        var hi = 0
        for (c, _) in bx:
          lo = min(lo, c[a]); hi = max(hi, c[a])
        if hi - lo > bestRange:
          bestRange = hi - lo; bi = i; bestAxis = a
    if bi < 0: break
    var bx = boxes[bi]
    let axis = bestAxis
    bx.sort(proc(x, y: (array[3, int], int)): int = cmp(x[0][axis], y[0][axis]))
    var total = 0
    for (_, c) in bx: total += c
    var acc = 0
    var cut = 1
    for i, (_, c) in bx:
      acc += c
      if acc * 2 >= total:
        cut = max(1, min(i + 1, bx.len - 1))
        break
    boxes[bi] = bx[0 ..< cut]
    boxes.add bx[cut .. ^1]
  for bx in boxes:
    var s = [0.0, 0.0, 0.0]
    var n = 0.0
    for (c, cnt) in bx:
      for a in 0..2: s[a] += float(c[a] * cnt)
      n += float(cnt)
    if n > 0: result.add [uint8(round(s[0] / n)), uint8(round(s[1] / n)), uint8(round(s[2] / n))]

proc nearest(pal: seq[array[3, uint8]]; r, g, b: int): int =
  var best = high(int)
  for i, c in pal:
    let d = (int(c[0]) - r) ^ 2 + (int(c[1]) - g) ^ 2 + (int(c[2]) - b) ^ 2
    if d < best:
      best = d
      result = i

proc lzwEncode(indices: seq[byte]; minCode: int): seq[byte] =
  let clear = 1 shl minCode
  let eoi = clear + 1
  var codeSize = minCode + 1
  var next = eoi + 1
  var dict = initTable[(int, int), int]()
  var acc: uint32 = 0
  var nbits = 0
  template emit(code: int) =
    acc = acc or (uint32(code) shl nbits)
    nbits += codeSize
    while nbits >= 8:
      result.add byte(acc and 0xFF)
      acc = acc shr 8
      nbits -= 8
  emit(clear)
  if indices.len == 0:
    emit(eoi)
    if nbits > 0: result.add byte(acc and 0xFF)
    return
  var cur = int(indices[0])
  for i in 1 ..< indices.len:
    let k = int(indices[i])
    let key = (cur, k)
    if dict.hasKey(key): cur = dict[key]
    else:
      emit(cur)
      if next < 4096:
        dict[key] = next
        inc next
        if next > (1 shl codeSize) and codeSize < 12: inc codeSize
      else:
        emit(clear)
        dict.clear()
        codeSize = minCode + 1
        next = eoi + 1
      cur = k
  emit(cur)
  emit(eoi)
  if nbits > 0: result.add byte(acc and 0xFF)

proc encodeGif*(src: Image): seq[byte] =
  ## A single-frame GIF89a with a median-cut palette (binary transparency).
  let img = src.convert(clRgba, isU8)
  let w = img.width
  let h = img.height
  var px = newSeq[array[4, uint8]](w * h)
  var anyTransparent = false
  for i in 0 ..< w * h:
    px[i] = [img.data[4 * i], img.data[4 * i + 1], img.data[4 * i + 2], img.data[4 * i + 3]]
    if px[i][3] < 128: anyTransparent = true
  var pal = quantizeMedianCut(px, if anyTransparent: 255 else: 256)
  if pal.len == 0: pal.add [0'u8, 0, 0]
  let tIndex = if anyTransparent: pal.len else: -1
  if anyTransparent: pal.add [0'u8, 0, 0]
  var bits = 1
  while (1 shl bits) < pal.len: inc bits
  while pal.len < (1 shl bits): pal.add [0'u8, 0, 0]
  var cache = initTable[uint32, int]()
  var idx = newSeq[byte](w * h)
  for i, p in px:
    if p[3] < 128 and tIndex >= 0: idx[i] = byte(tIndex)
    else:
      let k = (uint32(p[0]) shl 16) or (uint32(p[1]) shl 8) or uint32(p[2])
      if not cache.hasKey(k): cache[k] = nearest(pal[0 ..< (if tIndex >= 0: tIndex else: pal.len)], int(p[0]), int(p[1]), int(p[2]))
      idx[i] = byte(cache[k])
  result.add "GIF89a".toBytes
  result.put16(w); result.put16(h)
  result.add [byte(0x80 or ((bits - 1) shl 4) or (bits - 1)), 0, 0]
  for c in pal: result.add c
  if tIndex >= 0: result.add [0x21'u8, 0xF9, 4, 1, 0, 0, byte(tIndex), 0]
  result.add 0x2C
  result.put16(0); result.put16(0); result.put16(w); result.put16(h)
  result.add 0
  let minCode = max(2, bits)
  result.add byte(minCode)
  let data = lzwEncode(idx, minCode)
  var p = 0
  while p < data.len:
    let n = min(255, data.len - p)
    result.add byte(n)
    result.add data.toOpenArray(p, p + n - 1)
    p += n
  result.add 0
  result.add 0x3B

# ---------------------------------------------------------------- TGA

proc decodeTga*(b0: openArray[byte]; limits: Limits): Image =
  let bp = if b0.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr b0[0]) else: nil
  let bl = b0.len
  template b: untyped = toOpenArray(bp, 0, bl - 1)
  const F = fmtTga
  need(F, b, 18)
  let idLen = int(b[0])
  let cmapType = int(b[1])
  let imgType = int(b[2])
  let cmapFirst = le16(b, 3)
  let cmapLen = le16(b, 5)
  let cmapDepth = int(b[7])
  let w = le16(b, 12)
  let h = le16(b, 14)
  let depth = int(b[16])
  let desc = int(b[17])
  if imgType notin [1, 2, 3, 9, 10, 11]: raise unsupported(F, "image type " & $imgType)
  if w == 0 or h == 0: raise malformed(F, "zero-sized image")
  limits.checkBytes(w, h, 4)
  var p = 18 + idLen
  var cmap: seq[array[4, uint8]]
  proc readColor(b: openArray[byte]; at, bits: int): array[4, uint8] =
    case bits
    of 15, 16:
      let v = le16(b, at)
      let r = (v shr 10) and 31
      let g = (v shr 5) and 31
      let bb = v and 31
      [uint8(r * 255 div 31), uint8(g * 255 div 31), uint8(bb * 255 div 31), (if bits == 16 and (v and 0x8000) == 0 and false: 0'u8 else: 255'u8)]
    of 24: [b[at + 2], b[at + 1], b[at], 255'u8]
    of 32: [b[at + 2], b[at + 1], b[at], b[at + 3]]
    else: [b[at], b[at], b[at], 255'u8]
  if cmapType == 1:
    let cb = (cmapDepth + 7) div 8
    need(F, b, p + cmapLen * cb)
    for i in 0 ..< cmapLen: cmap.add readColor(b, p + i * cb, cmapDepth)
    p += cmapLen * cb
  let gray = imgType in [3, 11]
  let alphaBits = desc and 15
  let hasAlpha = (depth == 32 and (alphaBits > 0 or true)) or (gray and depth == 16) or (cmapType == 1 and cmapDepth == 32)
  let layout = if gray: (if depth == 16: clGrayA else: clGray) else: (if hasAlpha: clRgba else: clRgb)
  var img = newImage(w, h, layout, isU8)
  let ch = layout.channels
  let bpp = (depth + 7) div 8
  let rle = imgType >= 9
  let topDown = (desc and 0x20) != 0
  let rightLeft = (desc and 0x10) != 0
  var n = 0
  let total = w * h
  proc store(img: var Image; i: int; at: int) =
    let x0 = i mod w
    let y0 = i div w
    let x = if rightLeft: w - 1 - x0 else: x0
    let y = if topDown: y0 else: h - 1 - y0
    let o = (y * w + x) * ch
    if gray:
      img.data[o] = b[at]
      if ch == 2: img.data[o + 1] = b[at + 1]
    elif cmapType == 1:
      let idx = (if bpp == 2: le16(b, at) else: int(b[at])) - cmapFirst
      if idx >= 0 and idx < cmap.len:
        let c = cmap[idx]
        for k in 0 ..< ch: img.data[o + k] = c[k]
    else:
      let c = readColor(b, at, depth)
      for k in 0 ..< ch: img.data[o + k] = c[k]
  while n < total:
    if rle:
      need(F, b, p + 1)
      let hdr = int(b[p])
      inc p
      let count = (hdr and 0x7F) + 1
      if (hdr and 0x80) != 0:
        need(F, b, p + bpp)
        for k in 0 ..< count:
          if n >= total: break
          store(img, n, p)
          inc n
        p += bpp
      else:
        need(F, b, p + bpp * count)
        for k in 0 ..< count:
          if n >= total: break
          store(img, n, p)
          p += bpp
          inc n
    else:
      need(F, b, p + bpp)
      store(img, n, p)
      p += bpp
      inc n
  img

proc encodeTga*(src: Image; layout: ChannelLayout): seq[byte] =
  ## Uncompressed true-colour or gray TGA, top-down.
  let img = src.convert(layout, isU8)
  let ch = layout.channels
  let gray = layout.isGray
  result = @[0'u8, 0, (if gray: 3'u8 else: 2'u8), 0, 0, 0, 0, 0, 0, 0, 0, 0]
  result.put16(img.width); result.put16(img.height)
  result.add byte(ch * 8)
  result.add byte(0x20 or (if layout.hasAlpha: 8 else: 0))
  for i in 0 ..< img.width * img.height:
    let o = i * ch
    if gray:
      for k in 0 ..< ch: result.add img.data[o + k]
    else:
      result.add [img.data[o + 2], img.data[o + 1], img.data[o]]
      if ch == 4: result.add img.data[o + 3]
  result.add [0'u8, 0, 0, 0, 0, 0, 0, 0]
  result.add "TRUEVISION-XFILE.\0".toBytes

# ---------------------------------------------------------------- ICO

proc decodeIco*(b0: openArray[byte]; limits: Limits): Image =
  let bp = if b0.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr b0[0]) else: nil
  let bl = b0.len
  template b: untyped = toOpenArray(bp, 0, bl - 1)
  const F = fmtIco
  need(F, b, 6)
  let n = le16(b, 4)
  if n == 0: raise malformed(F, "no images")
  need(F, b, 6 + 16 * n)
  var best = -1
  var bestArea = -1
  for i in 0 ..< n:
    let e = 6 + 16 * i
    let w = if b[e] == 0: 256 else: int(b[e])
    let h = if b[e + 1] == 0: 256 else: int(b[e + 1])
    if w * h > bestArea:
      bestArea = w * h
      best = i
  let e = 6 + 16 * best
  let size = le32(b, e + 8)
  let off = le32(b, e + 12)
  need(F, b, off + size)
  template sub: untyped = b.toOpenArray(off, off + size - 1)
  if size >= 8 and sub[0] == 0x89 and sub[1] == byte('P'):
    return decodePng(sub, limits).convert(clRgba, isU8)
  decodeBmpAt(b, off, limits, true).convert(clRgba, isU8)

proc encodeIco*(src: Image): seq[byte] =
  ## One PNG-compressed entry (≤ 256×256).
  let png = encodePng(src, clRgba, isU8, defaultEncodeOptions())
  result = @[0'u8, 0, 1, 0, 1, 0]
  result.add [byte(if src.width >= 256: 0 else: src.width), byte(if src.height >= 256: 0 else: src.height), 0, 0]
  result.put16(1); result.put16(32)
  result.put32(png.len); result.put32(22)
  result.add png

# ---------------------------------------------------------------- Netpbm

proc decodePnm*(b0: openArray[byte]; limits: Limits): Image =
  let bp = if b0.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr b0[0]) else: nil
  let bl = b0.len
  template b: untyped = toOpenArray(bp, 0, bl - 1)
  const F = fmtPnm
  need(F, b, 3)
  let kind = char(b[1])
  var p = 2
  proc token(): string =
    while p < b.len:
      if char(b[p]) == '#':
        while p < b.len and b[p] != byte('\n'): inc p
      elif char(b[p]) in Whitespace: inc p
      else: break
    while p < b.len and char(b[p]) notin Whitespace:
      result.add char(b[p])
      inc p
  proc num(): int =
    try: parseInt(token())
    except ValueError: raise malformed(F, "bad header number")
  var w, h, maxv, depth: int
  var tupl = ""
  if kind == '7':
    while true:
      let t = token()
      if t.len == 0: raise malformed(F, "truncated PAM header")
      case t
      of "WIDTH": w = num()
      of "HEIGHT": h = num()
      of "DEPTH": depth = num()
      of "MAXVAL": maxv = num()
      of "TUPLTYPE": tupl = token()
      of "ENDHDR": break
      else: discard
    while p < b.len and b[p] != byte('\n'): inc p
    inc p
  elif kind in {'F', 'f'}:
    w = num(); h = num()
    let scale = (try: parseFloat(token()) except ValueError: raise malformed(F, "bad PFM scale"))
    inc p
    let ch = if kind == 'F': 3 else: 1
    if w <= 0 or h <= 0: raise malformed(F, "zero-sized image")
    limits.checkBytes(w, h, ch * 4)
    need(F, b, p + w * h * ch * 4)
    var img = newImage(w, h, (if ch == 3: clRgb else: clGray), isF32)
    let little = scale < 0
    for y in 0 ..< h:
      for x in 0 ..< w * ch:
        let at = p + ((h - 1 - y) * w * ch + x) * 4
        var u: uint32
        if little: u = leU32(b, at)
        else: u = (uint32(b[at]) shl 24) or (uint32(b[at + 1]) shl 16) or (uint32(b[at + 2]) shl 8) or uint32(b[at + 3])
        cast[ptr float32](addr img.data[(y * w * ch + x) * 4])[] = cast[float32](u)
    return img
  else:
    w = num(); h = num()
    maxv = if kind in {'1', '4'}: 1 else: num()
    depth = case kind
      of '1', '2', '4', '5': 1
      else: 3
    inc p
  if w <= 0 or h <= 0: raise malformed(F, "zero-sized image")
  if maxv < 1 or maxv > 65535: raise malformed(F, "bad maxval")
  if depth < 1 or depth > 4: raise unsupported(F, "depth " & $depth)
  let layout = case depth
    of 1: clGray
    of 2: clGrayA
    of 3: clRgb
    else: (if tupl.startsWith("CMYK"): clCmyk else: clRgba)
  let sample = if maxv > 255: isU16 else: isU8
  limits.check(w, h, layout, sample)
  var img = newImage(w, h, layout, sample)
  let n = w * h * depth
  proc put(i, v: int) =
    let scaled = if kind in {'1', '4'}: (if v == 1: 0 else: 255) else: (if sample == isU8: v * 255 div maxv else: v * 65535 div maxv)
    if sample == isU8: img.data[i] = byte(scaled) else: cast[ptr uint16](addr img.data[2 * i])[] = uint16(scaled)
  case kind
  of '1', '2', '3':
    for i in 0 ..< n:
      if kind == '1':
        while p < b.len and char(b[p]) notin {'0', '1'}: inc p
        if p >= b.len: raise malformed(F, "truncated data")
        put(i, ord(b[p]) - ord('0'))
        inc p
      else: put(i, num())
  of '4':
    let stride = (w + 7) div 8
    need(F, b, p + stride * h)
    for y in 0 ..< h:
      for x in 0 ..< w: put(y * w + x, (int(b[p + y * stride + x div 8]) shr (7 - x mod 8)) and 1)
  else:
    let bps = if maxv > 255: 2 else: 1
    need(F, b, p + n * bps)
    for i in 0 ..< n:
      let v = if bps == 1: int(b[p + i]) else: (int(b[p + 2 * i]) shl 8) or int(b[p + 2 * i + 1])
      put(i, min(v, maxv))
  img

proc encodePnm*(src: Image; layout: ChannelLayout; sample: ImgSample): seq[byte] =
  ## PGM/PPM (P5/P6), PAM (P7) for alpha/CMYK, PFM for float.
  if sample.isFloat:
    let lay = if layout.isGray: clGray else: clRgb
    let img = src.convert(lay, isF32)
    let ch = lay.channels
    result.add ((if ch == 3: "PF" else: "Pf") & "\n" & $img.width & " " & $img.height & "\n-1.0\n").toBytes
    for y in countdown(img.height - 1, 0):
      for x in 0 ..< img.width * ch:
        let v = cast[uint32](cast[ptr float32](unsafeAddr img.data[(y * img.width * ch + x) * 4])[])
        result.put32(int(v))
    return
  let img = src.convert(layout, sample)
  let maxv = if sample == isU16: 65535 else: 255
  case layout
  of clGray: result.add ("P5\n" & $img.width & " " & $img.height & "\n" & $maxv & "\n").toBytes
  of clRgb: result.add ("P6\n" & $img.width & " " & $img.height & "\n" & $maxv & "\n").toBytes
  else:
    let tupl = case layout
      of clGrayA: "GRAYSCALE_ALPHA"
      of clRgba: "RGB_ALPHA"
      of clCmyk: "CMYK"
      else: "CMYK_ALPHA"
    result.add ("P7\nWIDTH " & $img.width & "\nHEIGHT " & $img.height & "\nDEPTH " & $layout.channels & "\nMAXVAL " & $maxv &
                "\nTUPLTYPE " & tupl & "\nENDHDR\n").toBytes
  if sample == isU16:
    for i in 0 ..< img.data.len div 2:
      let v = cast[ptr uint16](unsafeAddr img.data[2 * i])[]
      result.add [byte(v shr 8), byte(v and 0xFF)]
  else: result.add img.data

# ---------------------------------------------------------------- QOI

proc qoiHash(c: array[4, uint8]): int {.inline.} = (int(c[0]) * 3 + int(c[1]) * 5 + int(c[2]) * 7 + int(c[3]) * 11) mod 64

proc decodeQoi*(b0: openArray[byte]; limits: Limits): Image =
  let bp = if b0.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr b0[0]) else: nil
  let bl = b0.len
  template b: untyped = toOpenArray(bp, 0, bl - 1)
  const F = fmtQoi
  need(F, b, 14)
  let w = (int(b[4]) shl 24) or (int(b[5]) shl 16) or (int(b[6]) shl 8) or int(b[7])
  let h = (int(b[8]) shl 24) or (int(b[9]) shl 16) or (int(b[10]) shl 8) or int(b[11])
  let chans = int(b[12])
  if w <= 0 or h <= 0 or chans notin [3, 4]: raise malformed(F, "bad header")
  limits.checkBytes(w, h, chans)
  var img = newImage(w, h, (if chans == 4: clRgba else: clRgb), isU8)
  var index: array[64, array[4, uint8]]
  var px = [0'u8, 0, 0, 255]
  var p = 14
  var run = 0
  for i in 0 ..< w * h:
    if run > 0: dec run
    elif p < b.len:
      let b1 = int(b[p])
      inc p
      if b1 == 0xFE:
        need(F, b, p + 3)
        px[0] = b[p]; px[1] = b[p + 1]; px[2] = b[p + 2]; p += 3
      elif b1 == 0xFF:
        need(F, b, p + 4)
        px = [b[p], b[p + 1], b[p + 2], b[p + 3]]; p += 4
      else:
        case b1 shr 6
        of 0: px = index[b1 and 63]
        of 1:
          px[0] = uint8((int(px[0]) + ((b1 shr 4) and 3) - 2) and 0xFF)
          px[1] = uint8((int(px[1]) + ((b1 shr 2) and 3) - 2) and 0xFF)
          px[2] = uint8((int(px[2]) + (b1 and 3) - 2) and 0xFF)
        of 2:
          need(F, b, p + 1)
          let b2 = int(b[p]); inc p
          let vg = (b1 and 63) - 32
          px[0] = uint8((int(px[0]) + vg - 8 + ((b2 shr 4) and 15)) and 0xFF)
          px[1] = uint8((int(px[1]) + vg) and 0xFF)
          px[2] = uint8((int(px[2]) + vg - 8 + (b2 and 15)) and 0xFF)
        else: run = b1 and 63
      index[qoiHash(px)] = px
    for k in 0 ..< chans: img.data[i * chans + k] = px[k]
  img

proc encodeQoi*(src: Image; layout: ChannelLayout): seq[byte] =
  let lay = if layout.hasAlpha: clRgba else: clRgb
  let img = src.convert(lay, isU8)
  let chans = lay.channels
  result = "qoif".toBytes
  for v in [img.width, img.height]: result.add [byte(v shr 24), byte((v shr 16) and 0xFF), byte((v shr 8) and 0xFF), byte(v and 0xFF)]
  result.add [byte(chans), 0]
  var index: array[64, array[4, uint8]]
  var prev = [0'u8, 0, 0, 255]
  var run = 0
  let n = img.width * img.height
  for i in 0 ..< n:
    var px = [0'u8, 0, 0, 255]
    for k in 0 ..< chans: px[k] = img.data[i * chans + k]
    if px == prev:
      inc run
      if run == 62 or i == n - 1:
        result.add byte(0xC0 or (run - 1))
        run = 0
      continue
    if run > 0:
      result.add byte(0xC0 or (run - 1))
      run = 0
    let h = qoiHash(px)
    if index[h] == px: result.add byte(h)
    else:
      index[h] = px
      if px[3] == prev[3]:
        let vr = int(px[0]) - int(prev[0])
        let vg = int(px[1]) - int(prev[1])
        let vb = int(px[2]) - int(prev[2])
        let wr = ((vr + 128) and 0xFF) - 128
        let wg = ((vg + 128) and 0xFF) - 128
        let wb = ((vb + 128) and 0xFF) - 128
        let vgr = wr - wg
        let vgb = wb - wg
        if wr >= -2 and wr <= 1 and wg >= -2 and wg <= 1 and wb >= -2 and wb <= 1:
          result.add byte(0x40 or ((wr + 2) shl 4) or ((wg + 2) shl 2) or (wb + 2))
        elif vgr >= -8 and vgr <= 7 and wg >= -32 and wg <= 31 and vgb >= -8 and vgb <= 7:
          result.add [byte(0x80 or (wg + 32)), byte(((vgr + 8) shl 4) or (vgb + 8))]
        else: result.add [0xFE'u8, px[0], px[1], px[2]]
      else: result.add [0xFF'u8, px[0], px[1], px[2], px[3]]
    prev = px
  result.add [0'u8, 0, 0, 0, 0, 0, 0, 1]

# ---------------------------------------------------------------- Radiance HDR

proc decodeHdr*(b0: openArray[byte]; limits: Limits): Image =
  let bp = if b0.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr b0[0]) else: nil
  let bl = b0.len
  template b: untyped = toOpenArray(bp, 0, bl - 1)
  const F = fmtHdr
  var p = 0
  proc line(): string =
    while p < b.len and b[p] != byte('\n'):
      result.add char(b[p])
      inc p
    inc p
  var fmtOk = true
  while true:
    if p >= b.len: raise malformed(F, "truncated header")
    let l = line()
    if l.len == 0: break
    if l.startsWith("FORMAT=") and l != "FORMAT=32-bit_rle_rgbe": fmtOk = false
  if not fmtOk: raise unsupported(F, "only 32-bit_rle_rgbe is supported")
  let res = line().splitWhitespace
  if res.len != 4: raise malformed(F, "bad resolution line")
  var w, h: int
  try:
    h = parseInt(res[1]); w = parseInt(res[3])
  except ValueError: raise malformed(F, "bad resolution line")
  let flipY = res[0] == "+Y"
  if w <= 0 or h <= 0: raise malformed(F, "zero-sized image")
  limits.checkBytes(w, h, 12)
  var img = newImage(w, h, clRgb, isF32)
  var scan = newSeq[array[4, uint8]](w)
  for y in 0 ..< h:
    need(F, b, p + 4)
    if w >= 8 and w < 32768 and b[p] == 2 and b[p + 1] == 2 and (int(b[p + 2]) shl 8 or int(b[p + 3])) == w:
      p += 4
      for c in 0..3:
        var x = 0
        while x < w:
          need(F, b, p + 1)
          var n = int(b[p]); inc p
          if n > 128:
            n -= 128
            need(F, b, p + 1)
            let v = b[p]; inc p
            for _ in 0 ..< n:
              if x < w: scan[x][c] = v
              inc x
          else:
            need(F, b, p + n)
            for k in 0 ..< n:
              if x < w: scan[x][c] = b[p + k]
              inc x
            p += n
    else:
      need(F, b, p + 4 * w)
      for x in 0 ..< w:
        scan[x] = [b[p], b[p + 1], b[p + 2], b[p + 3]]
        p += 4
    let yy = if flipY: h - 1 - y else: y
    for x in 0 ..< w:
      let e = int(scan[x][3])
      let f = if e == 0: 0.0'f32 else: float32(pow(2.0, float64(e - 136)))
      for c in 0..2: cast[ptr float32](addr img.data[((yy * w + x) * 3 + c) * 4])[] = float32(scan[x][c]) * f
  img

proc encodeHdr*(src: Image): seq[byte] =
  let img = src.convert(clRgb, isF32)
  let w = img.width
  let h = img.height
  result = ("#?RADIANCE\nFORMAT=32-bit_rle_rgbe\n\n-Y " & $h & " +X " & $w & "\n").toBytes
  for y in 0 ..< h:
    for x in 0 ..< w:
      var rgb: array[3, float32]
      for c in 0..2: rgb[c] = max(cast[ptr float32](unsafeAddr img.data[((y * w + x) * 3 + c) * 4])[], 0.0'f32)
      let m = max(rgb[0], max(rgb[1], rgb[2]))
      if m < 1e-32:
        result.add [0'u8, 0, 0, 0]
      else:
        var e: int
        let f = frexp(float64(m), e)
        let s = f * 256.0 / float64(m)
        result.add [byte(min(255.0, float64(rgb[0]) * s)), byte(min(255.0, float64(rgb[1]) * s)), byte(min(255.0, float64(rgb[2]) * s)), byte(e + 128)]

# ---------------------------------------------------------------- FITS

const FitsBlock = 2880

proc decodeFits*(b0: openArray[byte]; limits: Limits): Image =
  let bp = if b0.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr b0[0]) else: nil
  let bl = b0.len
  template b: untyped = toOpenArray(bp, 0, bl - 1)
  const F = fmtFits
  var cards = initTable[string, string]()
  var text: seq[(string, string)]
  var p = 0
  var ended = false
  while p + 80 <= b.len and not ended:
    for k in 0 ..< FitsBlock div 80:
      if p + 80 > b.len: break
      var card = newString(80)
      for i in 0 ..< 80: card[i] = char(b[p + i])
      p += 80
      let key = card[0 ..< 8].strip
      if key == "END":
        ended = true
        p = ((p + FitsBlock - 1) div FitsBlock) * FitsBlock
        break
      if card.len > 9 and card[8] == '=':
        var v = card[10 .. ^1]
        let slash = v.find(" /")
        if slash >= 0 and not v.strip.startsWith("'"): v = v[0 ..< slash]
        v = v.strip
        if v.startsWith("'"):
          let e = v.rfind('\'')
          v = if e > 0: v[1 ..< e].replace("''", "'").strip(leading = false) else: v[1 .. ^1]
        if not cards.hasKey(key): cards[key] = v
        if key notin ["SIMPLE", "BITPIX", "NAXIS", "NAXIS1", "NAXIS2", "NAXIS3", "EXTEND", "BZERO", "BSCALE", "BLANK", "ROWORDER"]:
          text.add (key, v)
      elif key in ["COMMENT", "HISTORY"]:
        text.add (key, card[8 .. ^1].strip)
  if not ended: raise malformed(F, "missing END card")
  proc ival(k: string; d = 0): int =
    try: parseInt(cards.getOrDefault(k, $d).strip) except ValueError: d
  proc fval(k: string; d: float): float =
    try: parseFloat(cards.getOrDefault(k, $d).strip) except ValueError: d
  let bitpix = ival("BITPIX")
  let naxis = ival("NAXIS")
  if naxis < 2: raise unsupported(F, "no 2-D image in the primary HDU")
  let w = ival("NAXIS1")
  let h = ival("NAXIS2")
  let planes = if naxis >= 3: ival("NAXIS3", 1) else: 1
  if bitpix notin [8, 16, 32, 64, -32, -64]: raise malformed(F, "invalid BITPIX " & $bitpix)
  if w <= 0 or h <= 0: raise malformed(F, "zero-sized image")
  let rgb = planes == 3
  let bps = abs(bitpix) div 8
  limits.checkBytes(w, h, (if rgb: 12 else: 4))
  need(F, b, p + w * h * (if rgb: 3 else: 1) * bps)
  let bzero = fval("BZERO", 0.0)
  let bscale = fval("BSCALE", 1.0)
  let topDown = cards.getOrDefault("ROWORDER", "").toUpperAscii == "TOP-DOWN"
  let ch = if rgb: 3 else: 1
  var vals = newSeq[float64](w * h * ch)
  var mx = 0.0
  var allInt = bitpix > 0 and bscale == 1.0
  for c in 0 ..< ch:
    for i in 0 ..< w * h:
      let at = p + ((c * w * h) + i) * bps
      var v: float64
      case bitpix
      of 8: v = float64(b[at])
      of 16: v = float64(cast[int16](uint16((int(b[at]) shl 8) or int(b[at + 1]))))
      of 32: v = float64(cast[int32]((uint32(b[at]) shl 24) or (uint32(b[at + 1]) shl 16) or (uint32(b[at + 2]) shl 8) or uint32(b[at + 3])))
      of 64:
        var u: uint64
        for k in 0..7: u = (u shl 8) or uint64(b[at + k])
        v = float64(cast[int64](u))
      of -32: v = float64(cast[float32]((uint32(b[at]) shl 24) or (uint32(b[at + 1]) shl 16) or (uint32(b[at + 2]) shl 8) or uint32(b[at + 3])))
      else:
        var u: uint64
        for k in 0..7: u = (u shl 8) or uint64(b[at + k])
        v = cast[float64](u)
      v = bzero + bscale * v
      if v != v: v = 0
      let x = i mod w
      let y = i div w
      let yy = if topDown: y else: h - 1 - y
      vals[(yy * w + x) * ch + c] = v
      mx = max(mx, v)
      if v < 0 or v != round(v): allInt = false
  let layout = if rgb: clRgb else: clGray
  if allInt and mx <= 255 and bitpix == 8:
    result = newImage(w, h, layout, isU8)
    for i, v in vals: result.data[i] = byte(v)
  elif allInt and mx <= 65535:
    result = newImage(w, h, layout, isU16)
    for i, v in vals: cast[ptr uint16](addr result.data[2 * i])[] = uint16(v)
  else:
    result = newImage(w, h, layout, isF32)
    let scale = if mx <= 2.0: 1.0 elif mx <= 65535: 65535.0 else: mx
    for i, v in vals: cast[ptr float32](addr result.data[4 * i])[] = float32(v / scale)
  result.meta.text = text
  if naxis >= 3 and planes != 3 and planes > 1:
    result.warnings.add DecodeWarning(kind: dwMorePages, total: some(planes))

proc encodeFits*(src: Image; layout: ChannelLayout; sample: ImgSample): seq[byte] =
  let lay = if layout.isGray: clGray else: clRgb
  let img = src.convert(lay, sample)
  let ch = lay.channels
  let bitpix = case sample
    of isU8: 8
    of isU16: 16
    else: -32
  var hdr: seq[string]
  proc card(k, v: string) = hdr.add alignLeft(k, 8) & "= " & align(v, 20)
  card("SIMPLE", "T")
  card("BITPIX", $bitpix)
  card("NAXIS", if ch == 3: "3" else: "2")
  card("NAXIS1", $img.width)
  card("NAXIS2", $img.height)
  if ch == 3: card("NAXIS3", "3")
  if bitpix == 16:
    card("BZERO", "32768")
    card("BSCALE", "1")
  hdr.add alignLeft("ROWORDER", 8) & "= 'BOTTOM-UP'"
  for (k, v) in img.meta.text:
    if k in ["COMMENT", "HISTORY"]: hdr.add alignLeft(k, 8) & v
    elif k.len <= 8 and k.allCharsInSet({'A'..'Z', '0'..'9', '_', '-'}):
      hdr.add alignLeft(k, 8) & "= '" & v.replace("'", "''") & "'"
  hdr.add "END"
  for c in hdr: result.add alignLeft(c[0 ..< min(80, c.len)], 80).toBytes
  while result.len mod FitsBlock != 0: result.add byte(' ')
  let bps = abs(bitpix) div 8
  for c in 0 ..< ch:
    for y in countdown(img.height - 1, 0):
      for x in 0 ..< img.width:
        let i = (y * img.width + x) * ch + c
        case bitpix
        of 8: result.add img.data[i]
        of 16:
          let v = int(cast[ptr uint16](unsafeAddr img.data[2 * i])[]) - 32768
          let u = cast[uint16](int16(v))
          result.add [byte(u shr 8), byte(u and 0xFF)]
        else:
          let u = cast[uint32](cast[ptr float32](unsafeAddr img.data[4 * i])[])
          result.add [byte(u shr 24), byte((u shr 16) and 0xFF), byte((u shr 8) and 0xFF), byte(u and 0xFF)]
  discard bps
  while result.len mod FitsBlock != 0: result.add 0
