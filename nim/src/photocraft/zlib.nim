## DEFLATE (RFC 1951), zlib (RFC 1950) and gzip (RFC 1952) in pure Nim, plus CRC-32 and
## Adler-32. Replaces the `flate2` crate the Rust code uses (the Nim standard library has no
## compressor). The decoder enforces an output limit so hostile streams cannot exhaust memory.

type
  ZlibError* = object of CatchableError

const DefaultMaxOutput* = 1 shl 31 ## 2 GiB

# ---------------------------------------------------------------- checksums

proc makeCrcTable(): array[256, uint32] =
  for n in 0 ..< 256:
    var c = uint32(n)
    for _ in 0 ..< 8:
      c = if (c and 1) != 0: 0xEDB88320'u32 xor (c shr 1) else: c shr 1
    result[n] = c

const CrcTable = makeCrcTable()

proc crc32Update*(crc: uint32; data: openArray[byte]): uint32 =
  var c = not crc
  for b in data: c = CrcTable[(c xor uint32(b)) and 0xFF] xor (c shr 8)
  not c

proc crc32*(data: openArray[byte]): uint32 = crc32Update(0, data)

proc adler32Update*(adler: uint32; data: openArray[byte]): uint32 =
  var a = adler and 0xFFFF
  var b = adler shr 16
  var i = 0
  while i < data.len:
    let n = min(5552, data.len - i)
    for k in 0 ..< n:
      a += uint32(data[i + k])
      b += a
    a = a mod 65521
    b = b mod 65521
    i += n
  (b shl 16) or a

proc adler32*(data: openArray[byte]): uint32 = adler32Update(1, data)

# ---------------------------------------------------------------- inflate

const
  LenBase = [3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258]
  LenExtra = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0]
  DistBase = [1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577]
  DistExtra = [0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13]
  ClOrder = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

type
  Huff = object
    ## Canonical Huffman decoding table: fast lookup of up to 9 bits, then a slow path.
    fast: array[512, uint16]  ## (symbol shl 4) or length; 0 = slow path
    counts: array[16, uint16]
    symbols: seq[uint16]
    maxLen: int

  BitIn = object
    data: ptr UncheckedArray[byte]
    len, pos: int
    bitBuf: uint64
    bitCnt: int

proc zerr(msg: string): ref ZlibError = newException(ZlibError, msg)

proc refill(b: var BitIn) {.inline.} =
  while b.bitCnt <= 56:
    if b.pos < b.len:
      b.bitBuf = b.bitBuf or (uint64(b.data[b.pos]) shl b.bitCnt)
      inc b.pos
      b.bitCnt += 8
    else:
      break

proc need(b: var BitIn; n: int) {.inline.} =
  if b.bitCnt < n:
    b.refill
    if b.bitCnt < n: raise zerr("unexpected end of deflate stream")

proc bits(b: var BitIn; n: int): int {.inline.} =
  if n == 0: return 0
  b.need(n)
  result = int(b.bitBuf and ((1'u64 shl n) - 1))
  b.bitBuf = b.bitBuf shr n
  b.bitCnt -= n

proc buildHuff(lengths: openArray[int]): Huff =
  for l in lengths:
    if l < 0 or l > 15: raise zerr("bad code length")
    inc result.counts[l]
  result.counts[0] = 0
  var offs: array[16, int]
  for i in 1 ..< 16: offs[i] = offs[i - 1] + int(result.counts[i - 1])
  # Kraft check (over-subscribed sets are invalid; incomplete sets are allowed).
  var left = 1
  for i in 1 ..< 16:
    left = left shl 1
    left -= int(result.counts[i])
    if left < 0: raise zerr("over-subscribed huffman code")
  result.symbols = newSeq[uint16](lengths.len)
  var o = offs
  for sym, l in lengths:
    if l != 0:
      result.symbols[o[l]] = uint16(sym)
      inc o[l]
  for i in 1 ..< 16:
    if result.counts[i] > 0: result.maxLen = i
  # fast table: canonical codes, bit-reversed
  var code = 0
  var k = 0
  for l in 1..9:
    for _ in 0 ..< int(result.counts[l]):
      let sym = result.symbols[k]
      # reverse `code` over l bits
      var rev = 0
      for bit in 0 ..< l:
        if (code and (1 shl bit)) != 0: rev = rev or (1 shl (l - 1 - bit))
      var idx = rev
      while idx < 512:
        result.fast[idx] = (sym shl 4) or uint16(l)
        idx += 1 shl l
      inc code
      inc k
    code = code shl 1

proc decodeSym(b: var BitIn; h: Huff): int {.inline.} =
  b.refill
  if b.bitCnt >= 9 or b.bitCnt > 0:
    let e = h.fast[int(b.bitBuf and 511)]
    if e != 0:
      let l = int(e and 15)
      if l <= b.bitCnt:
        b.bitBuf = b.bitBuf shr l
        b.bitCnt -= l
        return int(e shr 4)
  # slow canonical decode
  var code = 0
  var first = 0
  var index = 0
  for l in 1..15:
    code = code or b.bits(1)
    let count = int(h.counts[l])
    if code - count < first: return int(h.symbols[index + (code - first)])
    index += count
    first += count
    first = first shl 1
    code = code shl 1
  raise zerr("invalid huffman code")

var fixedLit, fixedDist: Huff
var fixedReady: bool

proc fixedTables() =
  {.cast(gcsafe).}:
    if fixedReady: return
    var l: array[288, int]
    for i in 0..143: l[i] = 8
    for i in 144..255: l[i] = 9
    for i in 256..279: l[i] = 7
    for i in 280..287: l[i] = 8
    fixedLit = buildHuff(l)
    var d: array[30, int]
    for i in 0 ..< 30: d[i] = 5
    fixedDist = buildHuff(d)
    fixedReady = true

proc inflateInto(b: var BitIn; outp: var seq[byte]; maxOut: int) =
  fixedTables()
  var final = false
  while not final:
    final = b.bits(1) == 1
    let btype = b.bits(2)
    case btype
    of 0:
      # stored: drop to byte boundary
      let drop = b.bitCnt mod 8
      discard b.bits(drop)
      let len = b.bits(16)
      let nlen = b.bits(16)
      if (len xor 0xFFFF) != nlen: raise zerr("stored block length mismatch")
      if outp.len + len > maxOut: raise zerr("inflated data exceeds the limit")
      for _ in 0 ..< len: outp.add byte(b.bits(8))
    of 1, 2:
      var lit, dist: Huff
      if btype == 1:
        {.cast(gcsafe).}:
          lit = fixedLit
          dist = fixedDist
      else:
        let hlit = b.bits(5) + 257
        let hdist = b.bits(5) + 1
        let hclen = b.bits(4) + 4
        var cl: array[19, int]
        for i in 0 ..< hclen: cl[ClOrder[i]] = b.bits(3)
        let clh = buildHuff(cl)
        var lens = newSeq[int](hlit + hdist)
        var i = 0
        while i < hlit + hdist:
          let sym = decodeSym(b, clh)
          if sym < 16:
            lens[i] = sym
            inc i
          elif sym == 16:
            if i == 0: raise zerr("repeat with no previous length")
            let prev = lens[i - 1]
            let r = 3 + b.bits(2)
            if i + r > lens.len: raise zerr("code lengths overflow")
            for _ in 0 ..< r:
              lens[i] = prev
              inc i
          else:
            let r = if sym == 17: 3 + b.bits(3) else: 11 + b.bits(7)
            if i + r > lens.len: raise zerr("code lengths overflow")
            i += r
        if lens[256] == 0: raise zerr("no end-of-block code")
        lit = buildHuff(lens.toOpenArray(0, hlit - 1))
        dist = buildHuff(lens.toOpenArray(hlit, hlit + hdist - 1))
      while true:
        let sym = decodeSym(b, lit)
        if sym < 256:
          if outp.len >= maxOut: raise zerr("inflated data exceeds the limit")
          outp.add byte(sym)
        elif sym == 256: break
        else:
          let li = sym - 257
          if li >= 29: raise zerr("bad length symbol")
          let length = LenBase[li] + b.bits(LenExtra[li])
          let ds = decodeSym(b, dist)
          if ds >= 30: raise zerr("bad distance symbol")
          let d = DistBase[ds] + b.bits(DistExtra[ds])
          if d > outp.len: raise zerr("distance too far back")
          if outp.len + length > maxOut: raise zerr("inflated data exceeds the limit")
          let start = outp.len - d
          let base = outp.len
          outp.setLen(base + length)
          for k in 0 ..< length: outp[base + k] = outp[start + k]
    else: raise zerr("invalid block type")

proc bitIn(data: openArray[byte]; start: int): BitIn =
  BitIn(data: if data.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr data[0]) else: nil, len: data.len, pos: start)

proc inflateRaw*(data: openArray[byte]; maxOut = DefaultMaxOutput): seq[byte] =
  ## Decompresses a raw DEFLATE stream. Raises `ZlibError`.
  var b = bitIn(data, 0)
  inflateInto(b, result, maxOut)

proc inflateRawPrefix*(data: openArray[byte]; consumed: var int; maxOut = DefaultMaxOutput): seq[byte] =
  ## Like `inflateRaw`, also reporting how many input bytes the stream used.
  var b = bitIn(data, 0)
  inflateInto(b, result, maxOut)
  consumed = b.pos - b.bitCnt div 8

proc zlibDecompress*(data: openArray[byte]; maxOut = DefaultMaxOutput; checkAdler = true): seq[byte] =
  ## Decompresses a zlib stream (header, DEFLATE data, Adler-32). Raises `ZlibError`.
  if data.len < 2: raise zerr("zlib stream too short")
  let cmf = int(data[0])
  let flg = int(data[1])
  if (cmf and 0x0F) != 8 or ((cmf shl 8) or flg) mod 31 != 0: raise zerr("bad zlib header")
  if (flg and 0x20) != 0: raise zerr("preset dictionaries are not supported")
  var b = bitIn(data, 2)
  inflateInto(b, result, maxOut)
  if checkAdler:
    let p = b.pos - b.bitCnt div 8
    if p + 4 <= data.len:
      let want = (uint32(data[p]) shl 24) or (uint32(data[p + 1]) shl 16) or (uint32(data[p + 2]) shl 8) or uint32(data[p + 3])
      if want != adler32(result): raise zerr("zlib checksum mismatch")

proc gzipDecompress*(data: openArray[byte]; maxOut = DefaultMaxOutput): seq[byte] =
  ## Decompresses a gzip member. Raises `ZlibError`.
  if data.len < 18 or data[0] != 0x1F or data[1] != 0x8B or data[2] != 8: raise zerr("not a gzip stream")
  let flg = int(data[3])
  var p = 10
  if (flg and 4) != 0:
    if p + 2 > data.len: raise zerr("truncated gzip header")
    p += 2 + (int(data[p]) or (int(data[p + 1]) shl 8))
  if (flg and 8) != 0:
    while p < data.len and data[p] != 0: inc p
    inc p
  if (flg and 16) != 0:
    while p < data.len and data[p] != 0: inc p
    inc p
  if (flg and 2) != 0: p += 2
  if p >= data.len: raise zerr("truncated gzip header")
  var b = bitIn(data, p)
  inflateInto(b, result, maxOut)

# ---------------------------------------------------------------- deflate

type
  BitOut = object
    buf: seq[byte]
    acc: uint64
    cnt: int

proc put(o: var BitOut; v: uint32; n: int) {.inline.} =
  o.acc = o.acc or (uint64(v) shl o.cnt)
  o.cnt += n
  while o.cnt >= 8:
    o.buf.add byte(o.acc and 0xFF)
    o.acc = o.acc shr 8
    o.cnt -= 8

proc flushBits(o: var BitOut) =
  if o.cnt > 0:
    o.buf.add byte(o.acc and 0xFF)
    o.acc = 0
    o.cnt = 0

proc revBits(code, len: int): uint32 {.inline.} =
  var r = 0
  for i in 0 ..< len:
    if (code and (1 shl i)) != 0: r = r or (1 shl (len - 1 - i))
  uint32(r)

proc codesFromLengths(lens: openArray[int]): seq[uint32] =
  ## Canonical codes, already bit-reversed for LSB-first output.
  var blCount: array[16, int]
  for l in lens: inc blCount[l]
  blCount[0] = 0
  var next: array[16, int]
  var code = 0
  for bits in 1..15:
    code = (code + blCount[bits - 1]) shl 1
    next[bits] = code
  result = newSeq[uint32](lens.len)
  for i, l in lens:
    if l != 0:
      result[i] = revBits(next[l], l)
      inc next[l]

proc sort(s: var seq[int]; cmpf: proc(a, b: int): int) =
  # small insertion/merge sort to avoid importing std/algorithm into the public namespace
  if s.len < 2: return
  var tmp = newSeq[int](s.len)
  var width = 1
  while width < s.len:
    var i = 0
    while i < s.len:
      let mid = min(i + width, s.len)
      let hi = min(i + 2 * width, s.len)
      var a = i
      var b = mid
      var k = i
      while a < mid and b < hi:
        if cmpf(s[a], s[b]) <= 0:
          tmp[k] = s[a]; inc a
        else:
          tmp[k] = s[b]; inc b
        inc k
      while a < mid:
        tmp[k] = s[a]; inc a; inc k
      while b < hi:
        tmp[k] = s[b]; inc b; inc k
      i += 2 * width
    swap(s, tmp)
    width *= 2

proc limitedLengths(freqs: seq[int]; maxLen: int): seq[int] =
  ## Huffman code lengths limited to `maxLen` (package-merge by repeated rebalancing).
  let n = freqs.len
  result = newSeq[int](n)
  var syms: seq[int]
  for i in 0 ..< n:
    if freqs[i] > 0: syms.add i
  if syms.len == 0: return
  if syms.len == 1:
    result[syms[0]] = 1
    return
  # Build a Huffman tree with a simple O(n log n) two-queue method.
  type Node = object
    w: int
    l, r: int  # children (-1 for leaves)
    sym: int
  var nodes: seq[Node]
  for s in syms: nodes.add Node(w: freqs[s], l: -1, r: -1, sym: s)
  var order = newSeq[int](nodes.len)
  for i in 0 ..< nodes.len: order[i] = i
  # sort leaves by weight
  for i in 1 ..< order.len:
    var j = i
    while j > 0 and nodes[order[j - 1]].w > nodes[order[j]].w:
      swap(order[j - 1], order[j])
      dec j
  var q1 = order
  var q2: seq[int]
  var i1, i2 = 0
  proc takeMin(): int =
    if i1 < q1.len and (i2 >= q2.len or nodes[q1[i1]].w <= nodes[q2[i2]].w):
      result = q1[i1]
      inc i1
    else:
      result = q2[i2]
      inc i2
  while (q1.len - i1) + (q2.len - i2) > 1:
    let a = takeMin()
    let b = takeMin()
    nodes.add Node(w: nodes[a].w + nodes[b].w, l: a, r: b, sym: -1)
    q2.add nodes.len - 1
  let root = takeMin()
  # depths
  var stack = @[(root, 0)]
  var depthCount: array[64, int]
  var leafDepth = newSeq[(int, int)]()
  while stack.len > 0:
    let (nd, d) = stack.pop
    if nodes[nd].l < 0:
      leafDepth.add (nodes[nd].sym, d)
      inc depthCount[min(d, 63)]
    else:
      stack.add (nodes[nd].l, d + 1)
      stack.add (nodes[nd].r, d + 1)
  # Limit lengths (zlib-style overflow fix on the bl_count histogram).
  var blCount: array[64, int]
  var overflow = 0
  for d in 0 ..< 64:
    if d > maxLen:
      blCount[maxLen] += depthCount[d]
      overflow += depthCount[d]
    else: blCount[d] += depthCount[d]
  # Kraft fix-up
  proc kraft(): int =
    var s = 0
    for d in 1..maxLen: s += blCount[d] shl (maxLen - d)
    s
  while kraft() > (1 shl maxLen):
    # move one leaf from the deepest level < maxLen down
    var bits = maxLen - 1
    while bits > 0 and blCount[bits] == 0: dec bits
    if bits == 0: break
    dec blCount[bits]
    blCount[bits + 1] += 2
    dec blCount[maxLen]
  # Assign lengths: most frequent symbols get the shortest codes.
  var bySym = syms
  bySym.sort(proc(a, b: int): int =
    if freqs[a] != freqs[b]: cmp(freqs[b], freqs[a]) else: cmp(a, b))
  var k = 0
  for d in 1..maxLen:
    for _ in 0 ..< blCount[d]:
      if k < bySym.len:
        result[bySym[k]] = d
        inc k

proc lenSym(length: int): (int, int, int) {.inline.} =
  ## (symbol index 0..28, extra bits, extra value)
  var i = 28
  while LenBase[i] > length: dec i
  (i, LenExtra[i], length - LenBase[i])

proc distSym(d: int): (int, int, int) {.inline.} =
  var i = 29
  while DistBase[i] > d: dec i
  (i, DistExtra[i], d - DistBase[i])

type Token = object
  litLen: uint16 ## literal byte (< 256) or match length (>= 3, flagged by dist > 0)
  dist: uint16

proc writeBlock(o: var BitOut; tokens: seq[Token]; final: bool) =
  var lf = newSeq[int](286)
  var df = newSeq[int](30)
  for t in tokens:
    if t.dist == 0: inc lf[t.litLen]
    else:
      inc lf[257 + lenSym(int(t.litLen))[0]]
      inc df[distSym(int(t.dist))[0]]
  inc lf[256]
  var ll = limitedLengths(lf, 15)
  var dl = limitedLengths(df, 15)
  var anyDist = false
  for l in dl:
    if l > 0: anyDist = true
  if not anyDist: dl[0] = 1
  var hlit = 286
  while hlit > 257 and ll[hlit - 1] == 0: dec hlit
  var hdist = 30
  while hdist > 1 and dl[hdist - 1] == 0: dec hdist
  # run-length encode the code lengths
  var all: seq[int]
  for i in 0 ..< hlit: all.add ll[i]
  for i in 0 ..< hdist: all.add dl[i]
  var rle: seq[(int, int)] # (symbol, extra)
  var i = 0
  while i < all.len:
    let v = all[i]
    var run = 1
    while i + run < all.len and all[i + run] == v: inc run
    if v == 0 and run >= 3:
      var r = run
      while r >= 3:
        if r >= 11:
          let n = min(r, 138)
          rle.add (18, n - 11)
          r -= n
        else:
          let n = min(r, 10)
          rle.add (17, n - 3)
          r -= n
      for _ in 0 ..< r: rle.add (0, 0)
    elif v != 0 and run >= 4:
      rle.add (v, 0)
      var r = run - 1
      while r >= 3:
        let n = min(r, 6)
        rle.add (16, n - 3)
        r -= n
      for _ in 0 ..< r: rle.add (v, 0)
    else:
      for _ in 0 ..< run: rle.add (v, 0)
    i += run
  var cf = newSeq[int](19)
  for (s, _) in rle: inc cf[s]
  let cl = limitedLengths(cf, 7)
  var hclen = 19
  while hclen > 4 and cl[ClOrder[hclen - 1]] == 0: dec hclen
  # Compare with the fixed-code cost; choose the cheaper.
  var dynCost = 5 + 5 + 4 + hclen * 3
  for (s, _) in rle:
    dynCost += cl[s] + (if s == 16: 2 elif s == 17: 3 elif s == 18: 7 else: 0)
  var fixCost = 0
  for t in tokens:
    if t.dist == 0:
      dynCost += ll[t.litLen]
      fixCost += (if t.litLen < 144: 8 else: 9)
    else:
      let (ls, le, _) = lenSym(int(t.litLen))
      let (ds, de, _) = distSym(int(t.dist))
      dynCost += ll[257 + ls] + le + dl[ds] + de
      fixCost += (if 257 + ls < 280: 7 else: 8) + le + 5 + de
  dynCost += ll[256]
  fixCost += 7
  o.put(if final: 1 else: 0, 1)
  var lcodes, dcodes: seq[uint32]
  var llens, dlens: seq[int]
  if fixCost <= dynCost:
    o.put(1, 2)
    llens = newSeq[int](288)
    for k in 0..143: llens[k] = 8
    for k in 144..255: llens[k] = 9
    for k in 256..279: llens[k] = 7
    for k in 280..287: llens[k] = 8
    dlens = newSeq[int](30)
    for k in 0 ..< 30: dlens[k] = 5
  else:
    o.put(2, 2)
    o.put(uint32(hlit - 257), 5)
    o.put(uint32(hdist - 1), 5)
    o.put(uint32(hclen - 4), 4)
    for k in 0 ..< hclen: o.put(uint32(cl[ClOrder[k]]), 3)
    let ccodes = codesFromLengths(cl)
    for (s, e) in rle:
      o.put(ccodes[s], cl[s])
      if s == 16: o.put(uint32(e), 2)
      elif s == 17: o.put(uint32(e), 3)
      elif s == 18: o.put(uint32(e), 7)
    llens = ll
    dlens = dl
  lcodes = codesFromLengths(llens)
  dcodes = codesFromLengths(dlens)
  for t in tokens:
    if t.dist == 0:
      o.put(lcodes[t.litLen], llens[t.litLen])
    else:
      let (ls, le, lv) = lenSym(int(t.litLen))
      o.put(lcodes[257 + ls], llens[257 + ls])
      if le > 0: o.put(uint32(lv), le)
      let (ds, de, dv) = distSym(int(t.dist))
      o.put(dcodes[ds], dlens[ds])
      if de > 0: o.put(uint32(dv), de)
  o.put(lcodes[256], llens[256])

proc deflateRaw*(data: openArray[byte]; level = 6): seq[byte] =
  ## Compresses into a raw DEFLATE stream. `level` 0 stores; 1..9 trade speed for size.
  var o: BitOut
  if level <= 0 or data.len == 0:
    var p = 0
    if data.len == 0:
      o.put(1, 1); o.put(1, 2); o.put(0, 7)  # empty fixed block with EOB
      o.flushBits
      return o.buf
    while p < data.len:
      let n = min(65535, data.len - p)
      let final = p + n >= data.len
      o.put(if final: 1 else: 0, 1)
      o.put(0, 2)
      o.flushBits
      o.buf.add byte(n and 0xFF); o.buf.add byte(n shr 8)
      o.buf.add byte((not n) and 0xFF); o.buf.add byte(((not n) shr 8) and 0xFF)
      for k in 0 ..< n: o.buf.add data[p + k]
      p += n
    return o.buf
  let dp = cast[ptr UncheckedArray[byte]](unsafeAddr data[0])
  let dlen = data.len
  const WinSize = 32768
  const HashBits = 15
  const HashSize = 1 shl HashBits
  let maxChain = case level
    of 1: 4
    of 2: 8
    of 3: 16
    of 4..5: 32
    of 6: 128
    of 7: 256
    else: 1024
  let lazy = level >= 4
  var head = newSeq[int32](HashSize)
  for h in head.mitems: h = -1
  var prev = newSeq[int32](WinSize)
  proc hash3(p: int): int =
    ((int(dp[p]) shl 10) xor (int(dp[p + 1]) shl 5) xor int(dp[p + 2])) and (HashSize - 1)
  proc insert(p: int) =
    if p + 2 < dlen:
      let h = hash3(p)
      prev[p and (WinSize - 1)] = head[h]
      head[h] = int32(p)
  proc longest(p: int): (int, int) =
    if p + 2 >= dlen: return (0, 0)
    var bestLen = 2
    var bestDist = 0
    var cand = int(head[hash3(p)])
    var chain = maxChain
    let maxLen = min(258, dlen - p)
    while cand >= 0 and p - cand <= WinSize and chain > 0:
      if cand < p and dp[cand + bestLen] == dp[p + bestLen] and dp[cand] == dp[p]:
        var l = 0
        while l < maxLen and dp[cand + l] == dp[p + l]: inc l
        if l > bestLen:
          bestLen = l
          bestDist = p - cand
          if l >= maxLen: break
      let nxt = int(prev[cand and (WinSize - 1)])
      if nxt >= cand: break
      cand = nxt
      dec chain
    if bestDist == 0: (0, 0) else: (bestLen, bestDist)
  var tokens: seq[Token]
  const BlockTokens = 1 shl 15
  var p = 0
  while p < dlen:
    var (l, d) = longest(p)
    if lazy and l >= 3 and l < 32 and p + 1 < dlen:
      insert(p)
      let (l2, d2) = longest(p + 1)
      if l2 > l:
        tokens.add Token(litLen: uint16(dp[p]), dist: 0)
        inc p
        l = l2
        d = d2
        if l >= 3:
          tokens.add Token(litLen: uint16(l), dist: uint16(d))
          for k in 0 ..< l: insert(p + k)
          p += l
        continue
      if l >= 3:
        tokens.add Token(litLen: uint16(l), dist: uint16(d))
        for k in 1 ..< l: insert(p + k)
        p += l
      else:
        tokens.add Token(litLen: uint16(dp[p]), dist: 0)
        inc p
    elif l >= 3:
      tokens.add Token(litLen: uint16(l), dist: uint16(d))
      for k in 0 ..< l: insert(p + k)
      p += l
    else:
      tokens.add Token(litLen: uint16(dp[p]), dist: 0)
      insert(p)
      inc p
    if tokens.len >= BlockTokens and p < dlen:
      writeBlock(o, tokens, false)
      tokens.setLen(0)
  writeBlock(o, tokens, true)
  o.flushBits
  o.buf

proc zlibCompress*(data: openArray[byte]; level = 6): seq[byte] =
  ## Compresses into a zlib stream.
  result = @[0x78'u8, (if level >= 7: 0xDA'u8 elif level <= 1: 0x01'u8 else: 0x9C'u8)]
  result.add deflateRaw(data, level)
  let a = adler32(data)
  result.add [byte(a shr 24), byte((a shr 16) and 0xFF), byte((a shr 8) and 0xFF), byte(a and 0xFF)]

proc gzipCompress*(data: openArray[byte]; level = 6): seq[byte] =
  result = @[0x1F'u8, 0x8B, 8, 0, 0, 0, 0, 0, 0, 255]
  result.add deflateRaw(data, level)
  let c = crc32(data)
  let n = uint32(data.len and 0xFFFFFFFF)
  for v in [c, n]:
    result.add [byte(v and 0xFF), byte((v shr 8) and 0xFF), byte((v shr 16) and 0xFF), byte(v shr 24)]

proc toBytes*(s: string): seq[byte] =
  result = newSeq[byte](s.len)
  for i, c in s: result[i] = byte(c)

proc toStr*(b: openArray[byte]): string =
  result = newString(b.len)
  for i, c in b: result[i] = char(c)
