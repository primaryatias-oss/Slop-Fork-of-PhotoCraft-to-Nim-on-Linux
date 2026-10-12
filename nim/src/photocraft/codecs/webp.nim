## WebP (RIFF container): lossy VP8 and lossless VP8L decoding (first frame of animations),
## `ALPH` alpha (raw or VP8L-compressed, with the spatial filters), ICCP/EXIF/XMP chunks.
## Encoding: lossless VP8L (subtract-green transform, LZ77 with distance codes, one set of
## prefix codes) or lossy VP8 (`vp8.encodeFrame`) with an uncompressed `ALPH` chunk.
## Written from RFC 9649 (WebP) and RFC 6386 (VP8).

import std/[options, strutils, sequtils]
import ./image, ./meta, ./vp8, ../zlib
from std/unicode import validateUtf8

proc err(msg: string): ref CodecError = malformed(fmtWebP, msg)

proc le32(b: openArray[byte]; p: int): int = int(b[p]) or (int(b[p + 1]) shl 8) or (int(b[p + 2]) shl 16) or (int(b[p + 3]) shl 24)
proc le24(b: openArray[byte]; p: int): int = int(b[p]) or (int(b[p + 1]) shl 8) or (int(b[p + 2]) shl 16)

# ---------------------------------------------------------------- VP8L decoding

const
  CodeLengthOrder = [17, 18, 0, 1, 2, 3, 4, 5, 16, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
  DistanceMap: array[120, (int, int)] = [
    (0, 1), (1, 0), (1, 1), (-1, 1), (0, 2), (2, 0), (1, 2), (-1, 2), (2, 1), (-2, 1), (2, 2), (-2, 2), (0, 3), (3, 0),
    (1, 3), (-1, 3), (3, 1), (-3, 1), (2, 3), (-2, 3), (3, 2), (-3, 2), (0, 4), (4, 0), (1, 4), (-1, 4), (4, 1), (-4, 1),
    (3, 3), (-3, 3), (2, 4), (-2, 4), (4, 2), (-4, 2), (0, 5), (3, 4), (-3, 4), (4, 3), (-4, 3), (5, 0), (1, 5), (-1, 5),
    (5, 1), (-5, 1), (2, 5), (-2, 5), (5, 2), (-5, 2), (4, 4), (-4, 4), (3, 5), (-3, 5), (5, 3), (-5, 3), (0, 6), (6, 0),
    (1, 6), (-1, 6), (6, 1), (-6, 1), (2, 6), (-2, 6), (6, 2), (-6, 2), (4, 5), (-4, 5), (5, 4), (-5, 4), (3, 6), (-3, 6),
    (6, 3), (-6, 3), (0, 7), (7, 0), (1, 7), (-1, 7), (5, 5), (-5, 5), (7, 1), (-7, 1), (4, 6), (-4, 6), (6, 4), (-6, 4),
    (2, 7), (-2, 7), (7, 2), (-7, 2), (3, 7), (-3, 7), (7, 3), (-7, 3), (5, 6), (-5, 6), (6, 5), (-6, 5), (8, 0), (4, 7),
    (-4, 7), (7, 4), (-7, 4), (8, 1), (8, 2), (6, 6), (-6, 6), (8, 3), (5, 7), (-5, 7), (7, 5), (-7, 5), (8, 4), (6, 7),
    (-6, 7), (7, 6), (-7, 6), (8, 5), (7, 7), (-7, 7), (8, 6), (8, 7)]

type
  LBits = object
    data: ptr UncheckedArray[byte]
    len, pos: int
    buf: uint64
    cnt: int
    eos: bool

  LHuff = object
    single: int          ## >= 0 when the code has one symbol (zero bits)
    fast: seq[int32]     ## 2^10 entries: (sym shl 5) or len, -1 = slow
    counts: array[16, int]
    symbols: seq[int]

  HGroup = array[5, LHuff]

  Transform = object
    kind: int
    bits: int
    data: seq[uint32]
    xsize: int

proc fill(b: var LBits) {.inline.} =
  while b.cnt <= 56:
    if b.pos < b.len:
      b.buf = b.buf or (uint64(b.data[b.pos]) shl b.cnt)
      inc b.pos
      b.cnt += 8
    else:
      break

proc readBits(b: var LBits; n: int): int {.inline.} =
  if n == 0: return 0
  b.fill
  if b.cnt < n:
    b.eos = true
    raise err("VP8L data is truncated")
  result = int(b.buf and ((1'u64 shl n) - 1))
  b.buf = b.buf shr n
  b.cnt -= n

proc buildLHuff(lengths: openArray[int]): LHuff =
  var nonzero = 0
  var last = 0
  for i, l in lengths:
    if l > 0:
      inc nonzero
      last = i
  result.single = -1
  if nonzero == 0: raise err("empty prefix code")
  if nonzero == 1:
    result.single = last
    return
  for l in lengths:
    if l > 15: raise err("bad code length")
    inc result.counts[l]
  result.counts[0] = 0
  var left = 1
  for i in 1..15:
    left = (left shl 1) - result.counts[i]
    if left < 0: raise err("over-subscribed prefix code")
  if left != 0: raise err("incomplete prefix code")
  var offs: array[16, int]
  for i in 1..15: offs[i] = offs[i - 1] + result.counts[i - 1]
  result.symbols = newSeq[int](lengths.len)
  var o = offs
  for s, l in lengths:
    if l > 0:
      result.symbols[o[l]] = s
      inc o[l]
  result.fast = newSeq[int32](1024)
  for v in result.fast.mitems: v = -1
  var code = 0
  var k = 0
  for l in 1..10:
    for _ in 0 ..< result.counts[l]:
      var rev = 0
      for bit in 0 ..< l:
        if (code and (1 shl bit)) != 0: rev = rev or (1 shl (l - 1 - bit))
      var i = rev
      while i < 1024:
        result.fast[i] = int32((result.symbols[k] shl 5) or l)
        i += 1 shl l
      inc code
      inc k
    code = code shl 1

proc readSym(b: var LBits; h: LHuff): int {.inline.} =
  if h.single >= 0: return h.single
  b.fill
  let e = h.fast[int(b.buf and 1023)]
  if e >= 0 and int(e and 31) <= b.cnt:
    let l = int(e and 31)
    b.buf = b.buf shr l
    b.cnt -= l
    return int(e shr 5)
  var code = 0
  var first = 0
  var index = 0
  for l in 1..15:
    code = code or b.readBits(1)
    let count = h.counts[l]
    if code - count < first: return h.symbols[index + (code - first)]
    index += count
    first += count
    first = first shl 1
    code = code shl 1
  raise err("invalid prefix code")

proc readCode(b: var LBits; alphabet: int): LHuff =
  var lengths = newSeq[int](alphabet)
  if b.readBits(1) == 1:
    let n = b.readBits(1) + 1
    let first8 = b.readBits(1)
    let s0 = b.readBits(1 + 7 * first8)
    if s0 >= alphabet: raise err("bad simple code symbol")
    lengths[s0] = 1
    if n == 2:
      let s1 = b.readBits(8)
      if s1 >= alphabet: raise err("bad simple code symbol")
      lengths[s1] = 1
    if n == 1 or lengths.count(1) == 1:
      return LHuff(single: s0)
    return buildLHuff(lengths)
  var cl: array[19, int]
  let ncl = b.readBits(4) + 4
  if ncl > 19: raise err("too many code length codes")
  for i in 0 ..< ncl: cl[CodeLengthOrder[i]] = b.readBits(3)
  let clh = buildLHuff(cl)
  var maxSym = alphabet
  if b.readBits(1) == 1:
    let nb = 2 + 2 * b.readBits(3)
    maxSym = 2 + b.readBits(nb)
    if maxSym > alphabet: raise err("max symbol exceeds the alphabet")
  var sym = 0
  var prev = 8
  while sym < alphabet:
    if maxSym == 0: break
    dec maxSym
    let c = b.readSym(clh)
    if c < 16:
      lengths[sym] = c
      inc sym
      if c != 0: prev = c
    else:
      let (extra, offset) = case c
        of 16: (2, 3)
        of 17: (3, 3)
        else: (7, 11)
      let rep = b.readBits(extra) + offset
      if sym + rep > alphabet: raise err("code length repeat overflows")
      let v = if c == 16: prev else: 0
      for _ in 0 ..< rep:
        lengths[sym] = v
        inc sym
  buildLHuff(lengths)

proc lzValue(b: var LBits; prefix: int): int =
  if prefix < 4: return prefix + 1
  let extra = (prefix - 2) shr 1
  let offset = (2 + (prefix and 1)) shl extra
  offset + b.readBits(extra) + 1

proc decodeImageData(b: var LBits; xsize, ysize: int; level0: bool): seq[uint32]

proc subImage(b: var LBits; xsize, ysize: int): seq[uint32] = decodeImageData(b, xsize, ysize, false)

proc decodeImageData(b: var LBits; xsize, ysize: int; level0: bool): seq[uint32] =
  var cacheBits = 0
  if b.readBits(1) == 1:
    cacheBits = b.readBits(4)
    if cacheBits < 1 or cacheBits > 11: raise err("bad colour cache size")
  var metaBits = 0
  var metaImg: seq[uint32]
  var metaW = 0
  var numGroups = 1
  if level0 and b.readBits(1) == 1:
    metaBits = b.readBits(3) + 2
    metaW = (xsize + (1 shl metaBits) - 1) shr metaBits
    let metaH = (ysize + (1 shl metaBits) - 1) shr metaBits
    metaImg = subImage(b, metaW, metaH)
    for v in metaImg: numGroups = max(numGroups, int((v shr 8) and 0xFFFF) + 1)
    if numGroups > 1 shl 16: raise err("too many prefix code groups")
  let cacheSize = if cacheBits > 0: 1 shl cacheBits else: 0
  var groups = newSeq[HGroup](numGroups)
  for g in 0 ..< numGroups:
    groups[g][0] = readCode(b, 256 + 24 + cacheSize)
    groups[g][1] = readCode(b, 256)
    groups[g][2] = readCode(b, 256)
    groups[g][3] = readCode(b, 256)
    groups[g][4] = readCode(b, 40)
  var cache = newSeq[uint32](cacheSize)
  let total = xsize * ysize
  result = newSeq[uint32](total)
  var pos = 0
  var lastCached = 0
  template cacheInsert(upTo: int) =
    if cacheSize > 0:
      while lastCached < upTo:
        let v = result[lastCached]
        cache[int((0x1e35a7bd'u32 * v) shr (32 - cacheBits))] = v
        inc lastCached
  while pos < total:
    let x = pos mod xsize
    let y = pos div xsize
    let g = if metaBits > 0: int((metaImg[(y shr metaBits) * metaW + (x shr metaBits)] shr 8) and 0xFFFF) else: 0
    let s = b.readSym(groups[g][0])
    if s < 256:
      let r = b.readSym(groups[g][1])
      let bl = b.readSym(groups[g][2])
      let a = b.readSym(groups[g][3])
      result[pos] = (uint32(a) shl 24) or (uint32(r) shl 16) or (uint32(s) shl 8) or uint32(bl)
      inc pos
      cacheInsert(pos)
    elif s < 256 + 24:
      let length = lzValue(b, s - 256)
      let dsym = b.readSym(groups[g][4])
      let dcode = lzValue(b, dsym)
      var dist: int
      if dcode > 120: dist = dcode - 120
      else:
        let (dx, dy) = DistanceMap[dcode - 1]
        dist = max(dy * xsize + dx, 1)
      if dist > pos or pos + length > total: raise err("backward reference out of range")
      for k in 0 ..< length: result[pos + k] = result[pos + k - dist]
      pos += length
      cacheInsert(pos)
    else:
      let idx = s - 256 - 24
      if idx >= cacheSize: raise err("colour cache index out of range")
      cacheInsert(pos)
      result[pos] = cache[idx]
      inc pos
      cacheInsert(pos)

proc avg2(a, b: uint32): uint32 {.inline.} = (((a xor b) and 0xfefefefe'u32) shr 1) + (a and b)

proc ch(v: uint32; s: int): int {.inline.} = int((v shr s) and 0xFF)

proc selectP(l, t, tl: uint32): uint32 =
  var pl, pt = 0
  for s in [24, 16, 8, 0]:
    let p = ch(l, s) + ch(t, s) - ch(tl, s)
    pl += abs(p - ch(l, s))
    pt += abs(p - ch(t, s))
  if pl < pt: l else: t

proc clampAddSubFull(a, b, c: uint32): uint32 =
  for s in [24, 16, 8, 0]:
    result = result or (uint32(clamp(ch(a, s) + ch(b, s) - ch(c, s), 0, 255)) shl s)

proc clampAddSubHalf(a, b: uint32): uint32 =
  for s in [24, 16, 8, 0]:
    let x = ch(a, s)
    result = result or (uint32(clamp(x + (x - ch(b, s)) div 2, 0, 255)) shl s)

proc addPixels(a, b: uint32): uint32 {.inline.} =
  (((a and 0xff00ff00'u32) + (b and 0xff00ff00'u32)) and 0xff00ff00'u32) or
    (((a and 0x00ff00ff'u32) + (b and 0x00ff00ff'u32)) and 0x00ff00ff'u32)

proc inversePredict(px: var seq[uint32]; w, h: int; t: Transform) =
  let bw = (w + (1 shl t.bits) - 1) shr t.bits
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = y * w + x
      var pred: uint32
      if y == 0: pred = (if x == 0: 0xff000000'u32 else: px[i - 1])
      elif x == 0: pred = px[i - w]
      else:
        let mode = int((t.data[(y shr t.bits) * bw + (x shr t.bits)] shr 8) and 0xF)
        let L = px[i - 1]
        let T = px[i - w]
        let TL = px[i - w - 1]
        let TR = px[i - w + 1]
        pred = case mode
          of 0: 0xff000000'u32
          of 1: L
          of 2: T
          of 3: TR
          of 4: TL
          of 5: avg2(avg2(L, TR), T)
          of 6: avg2(L, TL)
          of 7: avg2(L, T)
          of 8: avg2(TL, T)
          of 9: avg2(T, TR)
          of 10: avg2(avg2(L, TL), avg2(T, TR))
          of 11: selectP(L, T, TL)
          of 12: clampAddSubFull(L, T, TL)
          of 13: clampAddSubHalf(avg2(L, T), TL)
          else: 0xff000000'u32
      px[i] = addPixels(px[i], pred)

proc colorDelta(t, c: int8): int {.inline.} = (int(t) * int(c)) shr 5

proc inverseColor(px: var seq[uint32]; w, h: int; t: Transform) =
  let bw = (w + (1 shl t.bits) - 1) shr t.bits
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = y * w + x
      let m = t.data[(y shr t.bits) * bw + (x shr t.bits)]
      let g2r = cast[int8](m and 0xFF)
      let g2b = cast[int8]((m shr 8) and 0xFF)
      let r2b = cast[int8]((m shr 16) and 0xFF)
      let argb = px[i]
      let green = cast[int8]((argb shr 8) and 0xFF)
      var nr = int((argb shr 16) and 0xFF)
      var nb = int(argb and 0xFF)
      nr = (nr + colorDelta(g2r, green)) and 0xFF
      nb = (nb + colorDelta(g2b, green)) and 0xFF
      nb = (nb + colorDelta(r2b, cast[int8](nr))) and 0xFF
      px[i] = (argb and 0xff00ff00'u32) or (uint32(nr) shl 16) or uint32(nb)

proc decodeVp8l(data: openArray[byte]; hasHeader: bool; w0, h0: int; limits: Limits): (int, int, seq[uint32]) =
  var b = LBits(data: if data.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr data[0]) else: nil, len: data.len)
  var w = w0
  var h = h0
  if hasHeader:
    if b.readBits(8) != 0x2f: raise err("bad VP8L signature")
    w = b.readBits(14) + 1
    h = b.readBits(14) + 1
    discard b.readBits(1)
    if b.readBits(3) != 0: raise err("unknown VP8L version")
  limits.checkBytes(w, h, 4)
  var transforms: seq[Transform]
  var xsize = w
  var seen: set[0..3]
  while b.readBits(1) == 1:
    let kind = b.readBits(2)
    if kind in seen: raise err("repeated transform")
    seen.incl kind
    var t = Transform(kind: kind, xsize: xsize)
    case kind
    of 0, 1:
      t.bits = b.readBits(3) + 2
      t.data = subImage(b, (xsize + (1 shl t.bits) - 1) shr t.bits, (h + (1 shl t.bits) - 1) shr t.bits)
    of 3:
      let n = b.readBits(8) + 1
      var table = subImage(b, n, 1)
      for i in 1 ..< n: table[i] = addPixels(table[i], table[i - 1])
      t.data = table
      t.bits = if n <= 2: 3 elif n <= 4: 2 elif n <= 16: 1 else: 0
      xsize = (xsize + (1 shl t.bits) - 1) shr t.bits
    else: discard
    transforms.add t
  var px = decodeImageData(b, xsize, h, true)
  for i in countdown(transforms.len - 1, 0):
    let t = transforms[i]
    case t.kind
    of 0: inversePredict(px, t.xsize, h, t)
    of 1: inverseColor(px, t.xsize, h, t)
    of 2:
      for v in px.mitems:
        let g = (v shr 8) and 0xFF
        v = (v and 0xff00ff00'u32) or ((((v shr 16) + g) and 0xFF) shl 16) or (((v and 0xFF) + g) and 0xFF)
    of 3:
      let outW = t.xsize
      var o = newSeq[uint32](outW * h)
      let bitsPer = 8 shr t.bits
      let mask = (1 shl bitsPer) - 1
      let packedW = (outW + (1 shl t.bits) - 1) shr t.bits
      for y in 0 ..< h:
        for x in 0 ..< outW:
          let packed = px[y * packedW + (x shr t.bits)]
          let idx = int((packed shr 8) shr ((x and ((1 shl t.bits) - 1)) * bitsPer)) and mask
          o[y * outW + x] = if idx < t.data.len: t.data[idx] else: 0'u32
      px = o
    else: discard
  (w, h, px)

# ---------------------------------------------------------------- VP8L encoding

type BitW = object
  buf: seq[byte]
  acc: uint64
  cnt: int

proc put(o: var BitW; v: uint32; n: int) {.inline.} =
  o.acc = o.acc or (uint64(v) shl o.cnt)
  o.cnt += n
  while o.cnt >= 8:
    o.buf.add byte(o.acc and 0xFF)
    o.acc = o.acc shr 8
    o.cnt -= 8

proc flushW(o: var BitW) =
  if o.cnt > 0:
    o.buf.add byte(o.acc and 0xFF)
    o.acc = 0
    o.cnt = 0

proc huffLengths(freqs: seq[int]; maxLen: int): seq[int] =
  ## Length-limited Huffman code lengths (boundary package-merge would be optimal; this uses
  ## repeated rebalancing which is near-optimal for image data).
  let n = freqs.len
  result = newSeq[int](n)
  var syms: seq[int]
  for i in 0 ..< n:
    if freqs[i] > 0: syms.add i
  if syms.len == 0: return
  if syms.len == 1:
    result[syms[0]] = 1
    return
  var f = freqs
  while true:
    # Huffman via a simple heap substitute (sorted insertion; alphabets are small).
    type Node = tuple[w: int, kids: seq[int]]
    var nodes: seq[Node]
    for s in syms: nodes.add (w: max(f[s], 1), kids: @[s])
    var depth = newSeq[int](n)
    while nodes.len > 1:
      var a = 0
      var b = 1
      if nodes[b].w < nodes[a].w: swap(a, b)
      for i in 2 ..< nodes.len:
        if nodes[i].w < nodes[a].w: (b = a; a = i)
        elif nodes[i].w < nodes[b].w: b = i
      let merged = (w: nodes[a].w + nodes[b].w, kids: nodes[a].kids & nodes[b].kids)
      for k in merged.kids: inc depth[k]
      let hi = max(a, b)
      let lo = min(a, b)
      nodes.delete(hi)
      nodes.delete(lo)
      nodes.add merged
    var ok = true
    for s in syms:
      if depth[s] > maxLen: ok = false
    if ok:
      for s in syms: result[s] = depth[s]
      return
    # flatten the distribution and retry
    for s in syms: f[s] = (f[s] + 1) div 2 + 1

proc canonical(lengths: seq[int]): seq[uint32] =
  var blCount: array[16, int]
  for l in lengths: inc blCount[l]
  blCount[0] = 0
  var next: array[16, int]
  var code = 0
  for bits in 1..15:
    code = (code + blCount[bits - 1]) shl 1
    next[bits] = code
  result = newSeq[uint32](lengths.len)
  for i, l in lengths:
    if l != 0:
      var rev = 0
      for bit in 0 ..< l:
        if (next[l] and (1 shl bit)) != 0: rev = rev or (1 shl (l - 1 - bit))
      result[i] = uint32(rev)
      inc next[l]

proc writeCode(o: var BitW; lengths: seq[int]) =
  ## Stores a prefix code: the simple form for ≤ 2 symbols below 256, else the normal form.
  var used: seq[int]
  for i, l in lengths:
    if l > 0: used.add i
  if used.len == 0:
    # a one-symbol simple code for symbol 0
    o.put(1, 1); o.put(0, 1); o.put(0, 1); o.put(0, 1)
    return
  if used.len <= 2 and used[^1] < 256:
    o.put(1, 1)
    o.put(uint32(used.len - 1), 1)
    if used[0] < 2:
      o.put(0, 1)
      o.put(uint32(used[0]), 1)
    else:
      o.put(1, 1)
      o.put(uint32(used[0]), 8)
    if used.len == 2: o.put(uint32(used[1]), 8)
    return
  o.put(0, 1)
  # code length code over the plain lengths (no repeat codes, simple and valid)
  var clFreq = newSeq[int](19)
  for l in lengths: inc clFreq[l]
  var clLens = huffLengths(clFreq, 7)
  var nonzero = 0
  for l in clLens:
    if l > 0: inc nonzero
  if nonzero == 1:
    for i, l in clLens:
      if l > 0:
        clLens[(i + 1) mod 19] = 1
        break
  var ncl = 19
  while ncl > 4 and clLens[CodeLengthOrder[ncl - 1]] == 0: dec ncl
  o.put(uint32(ncl - 4), 4)
  for i in 0 ..< ncl: o.put(uint32(clLens[CodeLengthOrder[i]]), 3)
  o.put(0, 1) # max_symbol = alphabet size
  let codes = canonical(clLens)
  for l in lengths: o.put(codes[l], clLens[l])

proc encodeVp8lStream(argb: seq[uint32]; w, h: int; header: bool; hasAlpha: bool): seq[byte] =
  var o: BitW
  if header:
    o.put(0x2f, 8)
    o.put(uint32(w - 1), 14)
    o.put(uint32(h - 1), 14)
    o.put(if hasAlpha: 1'u32 else: 0'u32, 1)
    o.put(0, 3)
  # subtract-green transform
  o.put(1, 1)
  o.put(2, 2)
  var px = argb
  for v in px.mitems:
    let g = (v shr 8) and 0xFF
    v = (v and 0xff00ff00'u32) or (((((v shr 16) and 0xFF) + 256 - g) and 0xFF) shl 16) or ((((v and 0xFF) + 256 - g) and 0xFF))
  o.put(0, 1) # no more transforms
  o.put(0, 1) # no colour cache
  o.put(0, 1) # no meta prefix codes
  # LZ77 tokens (hash chain on 2-pixel keys; distance code 1 = left, 2 = above handled via map)
  type Tok = object
    lit: bool
    px: uint32
    len, dist: int
  var toks: seq[Tok]
  let total = px.len
  const HashBits = 16
  var head = newSeq[int32](1 shl HashBits)
  for v in head.mitems: v = -1
  var prev = newSeq[int32](total)
  proc hkey(i: int): int =
    let a = px[i]
    let b = if i + 1 < total: px[i + 1] else: 0'u32
    int(((a * 0x9E3779B1'u32) xor (b * 0x85EBCA77'u32)) shr (32 - HashBits))
  var i = 0
  while i < total:
    var bestLen = 0
    var bestDist = 0
    if i + 1 < total:
      # the pixel above and to the left are always worth trying
      for cand in [i - w, i - 1]:
        if cand >= 0:
          var l = 0
          while i + l < total and l < 4096 and px[cand + l] == px[i + l]: inc l
          if l > bestLen: (bestLen = l; bestDist = i - cand)
      var c = int(head[hkey(i)])
      var chain = 32
      while c >= 0 and chain > 0 and i - c <= (1 shl 20) - 120:
        var l = 0
        while i + l < total and l < 4096 and px[c + l] == px[i + l]: inc l
        if l > bestLen: (bestLen = l; bestDist = i - c)
        c = int(prev[c])
        dec chain
    if bestLen >= 3:
      toks.add Tok(lit: false, len: bestLen, dist: bestDist)
      for k in 0 ..< bestLen:
        if i + k + 1 < total:
          let hk = hkey(i + k)
          prev[i + k] = head[hk]
          head[hk] = int32(i + k)
      i += bestLen
    else:
      toks.add Tok(lit: true, px: px[i])
      if i + 1 < total:
        let hk = hkey(i)
        prev[i] = head[hk]
        head[hk] = int32(i)
      inc i
  proc prefixOf(v0: int): (int, int, int) =
    ## (prefix symbol, extra bits count, extra value) for a value ≥ 1
    let v = v0 - 1
    if v < 4: return (v, 0, 0)
    var highest = 0
    while (v shr (highest + 1)) != 0: inc highest
    let second = (v shr (highest - 1)) and 1
    let extra = highest - 1
    (2 * highest + second, extra, v and ((1 shl extra) - 1))
  proc distCode(dist: int): int =
    for k, (mx, my) in DistanceMap:
      if max(my * w + mx, 1) == dist: return k + 1
    dist + 120
  var fg = newSeq[int](256 + 24)
  var fr, fb, fa = newSeq[int](256)
  var fd = newSeq[int](40)
  var dcodes = newSeq[int](toks.len)
  for k, t in toks:
    if t.lit:
      inc fg[int((t.px shr 8) and 0xFF)]
      inc fr[int((t.px shr 16) and 0xFF)]
      inc fb[int(t.px and 0xFF)]
      inc fa[int(t.px shr 24)]
    else:
      inc fg[256 + prefixOf(t.len)[0]]
      dcodes[k] = distCode(t.dist)
      inc fd[prefixOf(dcodes[k])[0]]
  let lg = huffLengths(fg, 15)
  let lr = huffLengths(fr, 15)
  let lb = huffLengths(fb, 15)
  let la = huffLengths(fa, 15)
  let ld = huffLengths(fd, 15)
  for l in [lg, lr, lb, la, ld]: o.writeCode(l)
  let cg = canonical(lg)
  let cr = canonical(lr)
  let cb = canonical(lb)
  let ca = canonical(la)
  let cd = canonical(ld)
  proc nsym(l: seq[int]): int =
    for x in l:
      if x > 0: inc result
  let (sg, sr, sb, sa, sd) = (nsym(lg) <= 1, nsym(lr) <= 1, nsym(lb) <= 1, nsym(la) <= 1, nsym(ld) <= 1)
  for k, t in toks:
    if t.lit:
      let g = int((t.px shr 8) and 0xFF)
      if not sg: o.put(cg[g], lg[g])
      let r = int((t.px shr 16) and 0xFF)
      if not sr: o.put(cr[r], lr[r])
      let bb = int(t.px and 0xFF)
      if not sb: o.put(cb[bb], lb[bb])
      let a = int(t.px shr 24)
      if not sa: o.put(ca[a], la[a])
    else:
      let (ps, pe, pv) = prefixOf(t.len)
      if not sg: o.put(cg[256 + ps], lg[256 + ps])
      if pe > 0: o.put(uint32(pv), pe)
      let (ds, de, dv) = prefixOf(dcodes[k])
      if not sd: o.put(cd[ds], ld[ds])
      if de > 0: o.put(uint32(dv), de)
  o.flushW
  o.buf

# ---------------------------------------------------------------- container

proc decodeWebp*(bytes: openArray[byte]; limits: Limits): Image =
  ## Decodes a WebP file (first frame). Raises `CodecError`.
  template b: untyped = bytes
  if b.len < 12 or b[0] != byte('R') or b[8] != byte('W'): raise err("not a WebP file")
  var p = 12
  var vp8Data, vp8lData, alph: Option[(int, int)]
  var icc, exif = none(seq[byte])
  var xmp = none(string)
  var canvasW, canvasH = 0
  var frames = 0
  while p + 8 <= b.len:
    var fourcc = ""
    for i in 0..3: fourcc.add char(b[p + i])
    let size = le32(b, p + 4)
    let ds = p + 8
    if size < 0 or ds + size > b.len:
      if vp8Data.isSome or vp8lData.isSome: break
      raise err("chunk " & fourcc & " is truncated")
    case fourcc
    of "VP8X":
      if size >= 10:
        canvasW = le24(b, ds + 4) + 1
        canvasH = le24(b, ds + 7) + 1
    of "VP8 ":
      if vp8Data.isNone and vp8lData.isNone: vp8Data = some((ds, size))
    of "VP8L":
      if vp8Data.isNone and vp8lData.isNone: vp8lData = some((ds, size))
    of "ALPH":
      if alph.isNone: alph = some((ds, size))
    of "ICCP": icc = some(@(b.toOpenArray(ds, ds + size - 1)))
    of "EXIF": exif = some(@(b.toOpenArray(ds, ds + size - 1)))
    of "XMP ":
      let s = b.toOpenArray(ds, ds + size - 1).toStr
      if s.validateUtf8 == -1: xmp = some(s)
    of "ANMF":
      inc frames
      if frames == 1 and size >= 16:
        # the first frame's sub-chunks (offset 16 in the ANMF payload)
        var q = ds + 16
        while q + 8 <= ds + size:
          var fc = ""
          for i in 0..3: fc.add char(b[q + i])
          let sz = le32(b, q + 4)
          if q + 8 + sz > b.len: break
          if fc == "VP8 " and vp8Data.isNone: vp8Data = some((q + 8, sz))
          elif fc == "VP8L" and vp8lData.isNone: vp8lData = some((q + 8, sz))
          elif fc == "ALPH" and alph.isNone: alph = some((q + 8, sz))
          q += 8 + sz + (sz and 1)
    else: discard
    p = ds + size + (size and 1)
  var img: Image
  if vp8lData.isSome:
    let (s, n) = vp8lData.get
    let (w, h, px) = decodeVp8l(b.toOpenArray(s, s + n - 1), true, 0, 0, limits)
    var alpha = false
    for v in px:
      if (v shr 24) != 255:
        alpha = true
        break
    img = newImage(w, h, (if alpha: clRgba else: clRgb), isU8)
    let c = img.layout.channels
    for i, v in px:
      img.data[i * c] = byte((v shr 16) and 0xFF)
      img.data[i * c + 1] = byte((v shr 8) and 0xFF)
      img.data[i * c + 2] = byte(v and 0xFF)
      if alpha: img.data[i * c + 3] = byte(v shr 24)
  elif vp8Data.isSome:
    let (s, n) = vp8Data.get
    var fr: DecodedFrame
    try: fr = decodeFrame(b.toOpenArray(s, s + n - 1))
    except Vp8Error as e: raise err(e.msg)
    limits.checkBytes(fr.width, fr.height, 4)
    let w = fr.width
    let h = fr.height
    let hasAlpha = alph.isSome
    img = newImage(w, h, (if hasAlpha: clRgba else: clRgb), isU8)
    yuvToRgb(fr, img.data, img.layout.channels)
    if hasAlpha:
      let (as0, an) = alph.get
      if an < 1: raise err("empty ALPH chunk")
      let hdr = int(b[as0])
      let comp = hdr and 3
      let filt = (hdr shr 2) and 3
      var a = newSeq[uint8](w * h)
      if comp == 0:
        if an - 1 < w * h: raise err("ALPH chunk is truncated")
        for i in 0 ..< w * h: a[i] = b[as0 + 1 + i]
      elif comp == 1:
        let (_, _, px) = decodeVp8l(b.toOpenArray(as0 + 1, as0 + an - 1), false, w, h, limits)
        for i in 0 ..< w * h: a[i] = uint8((px[i] shr 8) and 0xFF)
      else: raise unsupported(fmtWebP, "alpha compression " & $comp)
      # unfilter
      for y in 0 ..< h:
        for x in 0 ..< w:
          let i = y * w + x
          var pred = 0
          case filt
          of 1: pred = if x > 0: int(a[i - 1]) elif y > 0: int(a[i - w]) else: 0
          of 2: pred = if y > 0: int(a[i - w]) elif x > 0: int(a[i - 1]) else: 0
          of 3:
            if y == 0: pred = if x > 0: int(a[i - 1]) else: 0
            elif x == 0: pred = int(a[i - w])
            else: pred = clamp(int(a[i - 1]) + int(a[i - w]) - int(a[i - w - 1]), 0, 255)
          else: discard
          a[i] = uint8((int(a[i]) + pred) and 0xFF)
      for i in 0 ..< w * h: img.data[i * 4 + 3] = a[i]
  else: raise err("no image data")
  img.icc = icc
  img.meta.exif = exif
  img.meta.xmp = xmp
  if frames > 1: img.warnings.add DecodeWarning(kind: dwMoreFrames, total: some(frames))
  discard canvasW
  discard canvasH
  img

proc chunk(o: var seq[byte]; fourcc: string; payload: openArray[byte]) =
  for c in fourcc: o.add byte(c)
  let n = payload.len
  o.add [byte(n and 0xFF), byte((n shr 8) and 0xFF), byte((n shr 16) and 0xFF), byte((n shr 24) and 0xFF)]
  o.add payload
  if n mod 2 == 1: o.add 0

proc riff(w, h: int; frameKind: string; frame: seq[byte]; alpha: Option[seq[byte]]; icc, exif: Option[seq[byte]];
          xmp: Option[string]; alphaFlag: bool): seq[byte] =
  var body = "WEBP".toBytes
  let extended = alpha.isSome or icc.isSome or exif.isSome or xmp.isSome
  if extended:
    var flags = 0'u8
    if icc.isSome: flags = flags or 0x20
    if alpha.isSome or alphaFlag: flags = flags or 0x10
    if exif.isSome: flags = flags or 0x08
    if xmp.isSome: flags = flags or 0x04
    var x = @[flags, 0'u8, 0, 0]
    for v in [w - 1, h - 1]: x.add [byte(v and 0xFF), byte((v shr 8) and 0xFF), byte((v shr 16) and 0xFF)]
    body.chunk("VP8X", x)
    if icc.isSome: body.chunk("ICCP", icc.get)
    if alpha.isSome:
      var a = @[0'u8]
      a.add alpha.get
      body.chunk("ALPH", a)
  body.chunk(frameKind, frame)
  if exif.isSome: body.chunk("EXIF", exif.get)
  if xmp.isSome: body.chunk("XMP ", xmp.get.toBytes)
  result = "RIFF".toBytes
  let n = body.len
  result.add [byte(n and 0xFF), byte((n shr 8) and 0xFF), byte((n shr 16) and 0xFF), byte((n shr 24) and 0xFF)]
  result.add body

proc encodeWebp*(src: Image; layout: ChannelLayout; opts: EncodeOptions): seq[byte] =
  ## Lossless VP8L or lossy VP8 (per `opts.webpLossless`), with metadata chunks.
  let hasAlpha = layout.hasAlpha
  let img = src.convert(if hasAlpha: clRgba else: clRgb, isU8)
  let w = img.width
  let h = img.height
  if w > 16383 or h > 16383: raise encodeErr(fmtWebP, "WebP holds at most 16383 pixels per side")
  let icc = if opts.embedIcc: img.icc else: none(seq[byte])
  let exif = if opts.embedMetadata and img.meta.exif.isSome: some(exportExif(img.meta.exif.get, img.meta.dpi)) else: none(seq[byte])
  let xmp = if opts.embedMetadata and img.meta.xmp.isSome: some(exportXmp(img.meta.xmp.get, img.meta.dpi)) else: none(string)
  let c = img.layout.channels
  if opts.webpLossless:
    var px = newSeq[uint32](w * h)
    for i in 0 ..< w * h:
      let a = if hasAlpha: uint32(img.data[i * c + 3]) else: 255'u32
      px[i] = (a shl 24) or (uint32(img.data[i * c]) shl 16) or (uint32(img.data[i * c + 1]) shl 8) or uint32(img.data[i * c + 2])
    let stream = encodeVp8lStream(px, w, h, true, hasAlpha)
    return riff(w, h, "VP8L", stream, none(seq[byte]), icc, exif, xmp, hasAlpha)
  let yuv = yuvFromRgb(img.data, w, h, c)
  var frame: seq[byte]
  try: frame = encodeFrame(yuv, opts.webpQuality)
  except Vp8Error as e: raise encodeErr(fmtWebP, e.msg)
  var alpha = none(seq[byte])
  if hasAlpha:
    var a = newSeq[byte](w * h)
    var opaque = true
    for i in 0 ..< w * h:
      a[i] = img.data[i * 4 + 3]
      if a[i] != 255: opaque = false
    if not opaque: alpha = some(a)
  riff(w, h, "VP8 ", frame, alpha, icc, exif, xmp, alpha.isSome)
