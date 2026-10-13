## Zstandard (RFC 8878), replacing the Rust `ruzstd` crate: a complete frame decoder (raw, RLE
## and compressed blocks; Huffman literals incl. 4 streams and treeless; FSE sequences with
## predefined, RLE, compressed and repeat tables; repeat offsets; XXH64 content checksums;
## skippable and concatenated frames) and a compact encoder (greedy hash-chain LZ77, raw
## literals, predefined FSE tables; raw/RLE blocks when smaller).

import std/bitops

type ZstdError* = object of CatchableError

proc zerr(msg: string): ref ZstdError = newException(ZstdError, "zstd: " & msg)

# ---------------------------------------------------------------- XXH64

const
  P1 = 11400714785074694791'u64
  P2 = 14029467366897019727'u64
  P3 = 1609587929392839161'u64
  P4 = 9650029242287828579'u64
  P5 = 2870177450012600261'u64

proc rotl64(x: uint64; r: int): uint64 {.inline.} = (x shl r) or (x shr (64 - r))
proc le64(d: openArray[byte]; i: int): uint64 {.inline.} =
  for k in countdown(7, 0): result = (result shl 8) or uint64(d[i + k])
proc le32(d: openArray[byte]; i: int): uint32 {.inline.} =
  for k in countdown(3, 0): result = (result shl 8) or uint32(d[i + k])

proc xxh64*(d: openArray[byte]; seed = 0'u64): uint64 =
  var i = 0
  let n = d.len
  var h: uint64
  template round(acc, input: uint64): uint64 = rotl64(acc + input * P2, 31) * P1
  if n >= 32:
    var v1 = seed + P1 + P2
    var v2 = seed + P2
    var v3 = seed
    var v4 = seed - P1
    while i + 32 <= n:
      v1 = round(v1, le64(d, i))
      v2 = round(v2, le64(d, i + 8))
      v3 = round(v3, le64(d, i + 16))
      v4 = round(v4, le64(d, i + 24))
      i += 32
    h = rotl64(v1, 1) + rotl64(v2, 7) + rotl64(v3, 12) + rotl64(v4, 18)
    for v in [v1, v2, v3, v4]:
      h = (h xor round(0, v)) * P1 + P4
  else:
    h = seed + P5
  h += uint64(n)
  while i + 8 <= n:
    h = rotl64(h xor round(0, le64(d, i)), 27) * P1 + P4
    i += 8
  if i + 4 <= n:
    h = rotl64(h xor (uint64(le32(d, i)) * P1), 23) * P2 + P3
    i += 4
  while i < n:
    h = rotl64(h xor (uint64(d[i]) * P5), 11) * P1
    inc i
  h = h xor (h shr 33)
  h *= P2
  h = h xor (h shr 29)
  h *= P3
  h xor (h shr 32)

# ---------------------------------------------------------------- bit readers

type
  FwdBits = object
    ## Little-endian forward bit reader (FSE table descriptions).
    data: ptr UncheckedArray[byte]
    len: int
    pos: int   ## bit position
  BackBits = object
    ## Backward bit reader of a zstd bitstream (reads from the end towards the start).
    data: ptr UncheckedArray[byte]
    len: int
    bitPos: int ## bits still unread; may go negative (reads past the start give zeros)

proc fwd(d: openArray[byte]): FwdBits =
  FwdBits(data: (if d.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr d[0]) else: nil), len: d.len)

proc read(b: var FwdBits; n: int): int =
  for k in 0 ..< n:
    let p = b.pos + k
    if p div 8 >= b.len: raise zerr("truncated table description")
    if ((b.data[p div 8] shr (p mod 8)) and 1) != 0: result = result or (1 shl k)
  b.pos += n

proc bytesUsed(b: FwdBits): int = (b.pos + 7) div 8

proc back(d: openArray[byte]): BackBits =
  if d.len == 0: raise zerr("empty bitstream")
  let last = d[^1]
  if last == 0: raise zerr("bitstream has no end marker")
  result = BackBits(data: cast[ptr UncheckedArray[byte]](unsafeAddr d[0]), len: d.len)
  result.bitPos = (d.len - 1) * 8 + fastLog2(int(last))

proc read(b: var BackBits; n: int): uint64 {.inline.} =
  if n == 0: return 0
  let lo = b.bitPos - n
  let hiByte = (b.bitPos - 1) shr 3
  let loByte = lo shr 3   # arithmetic shift: floor for negatives
  var v = 0'u64
  var k = hiByte
  while k >= loByte:
    v = (v shl 8) or (if k >= 0 and k < b.len: uint64(b.data[k]) else: 0'u64)
    dec k
  v = v shr (lo - loByte * 8)
  b.bitPos = lo
  v and ((1'u64 shl n) - 1)

proc peek(b: BackBits; n: int): uint64 {.inline.} =
  var c = b
  c.read(n)

# ---------------------------------------------------------------- FSE

type
  FseEntry = object
    symbol: uint8
    numBits: uint8
    base: uint16
  FseTable = object
    accLog: int
    entries: seq[FseEntry]

proc buildFse(freqs: openArray[int]; accLog: int): FseTable =
  let size = 1 shl accLog
  result = FseTable(accLog: accLog, entries: newSeq[FseEntry](size))
  var stateDesc = newSeq[int](freqs.len)
  var high = size
  for s, f in freqs:
    if f == -1:
      dec high
      if high < 0: raise zerr("bad FSE distribution")
      result.entries[high].symbol = uint8(s)
      stateDesc[s] = 1
  let step = (size shr 1) + (size shr 3) + 3
  let mask = size - 1
  var pos = 0
  for s, f in freqs:
    if f > 0:
      stateDesc[s] = f
      for _ in 0 ..< f:
        result.entries[pos].symbol = uint8(s)
        pos = (pos + step) and mask
        while pos >= high: pos = (pos + step) and mask
  if pos != 0: raise zerr("bad FSE distribution")
  for i in 0 ..< size:
    let s = int(result.entries[i].symbol)
    let nextDesc = stateDesc[s]
    inc stateDesc[s]
    if nextDesc <= 0: raise zerr("bad FSE distribution")
    let nb = accLog - fastLog2(nextDesc)
    result.entries[i].numBits = uint8(nb)
    result.entries[i].base = uint16((nextDesc shl nb) - size)

proc rleTable(symbol: int): FseTable =
  FseTable(accLog: 0, entries: @[FseEntry(symbol: uint8(symbol), numBits: 0, base: 0)])

proc readFseFreqs(b: var FwdBits; maxSymbol, maxLog: int): (seq[int], int) =
  let accLog = 5 + b.read(4)
  if accLog > maxLog: raise zerr("FSE accuracy log too large")
  var remaining = 1 shl accLog
  var freqs: seq[int]
  while remaining > 0 and freqs.len <= maxSymbol:
    let bits = fastLog2(remaining + 1) + 1
    var val = b.read(bits)
    let lowerMask = (1 shl (bits - 1)) - 1
    let threshold = (1 shl bits) - 1 - (remaining + 1)
    if (val and lowerMask) < threshold:
      b.pos -= 1
      val = val and lowerMask
    elif val > lowerMask:
      val -= threshold
    let proba = val - 1
    remaining -= abs(proba)
    freqs.add proba
    if proba == 0:
      var rep = b.read(2)
      while true:
        for _ in 0 ..< rep:
          if freqs.len <= maxSymbol: freqs.add 0
        if rep == 3: rep = b.read(2) else: break
  if remaining != 0 or freqs.len > maxSymbol + 1: raise zerr("corrupt FSE table description")
  (freqs, accLog)

proc decodeState(t: FseTable; state: var int; b: var BackBits): int {.inline.} =
  let e = t.entries[state]
  result = int(e.symbol)
  state = int(e.base) + int(b.read(int(e.numBits)))

# ---------------------------------------------------------------- Huffman

type
  HufTable = object
    maxBits: int
    symbol: seq[uint8]
    bits: seq[uint8]

proc buildHuf(weights0: seq[int]): HufTable =
  if weights0.len == 0 or weights0.len > 255: raise zerr("bad Huffman weights")
  var weights = weights0
  var sum = 0
  for w in weights:
    if w > 11: raise zerr("Huffman weight too large")
    if w > 0: sum += 1 shl (w - 1)
  if sum == 0: raise zerr("bad Huffman weights")
  let maxBits = fastLog2(sum) + 1
  let leftover = (1 shl maxBits) - sum
  if leftover <= 0 or (leftover and (leftover - 1)) != 0: raise zerr("bad Huffman weights")
  weights.add fastLog2(leftover) + 1
  if maxBits > 11: raise zerr("Huffman code too long")
  var bitsOf = newSeq[int](weights.len)
  var rankCount = newSeq[int](maxBits + 2)
  for i, w in weights:
    if w > 0:
      bitsOf[i] = maxBits + 1 - w
      inc rankCount[bitsOf[i]]
  let size = 1 shl maxBits
  result = HufTable(maxBits: maxBits, symbol: newSeq[uint8](size), bits: newSeq[uint8](size))
  var rankIdx = newSeq[int](maxBits + 2)
  rankIdx[maxBits] = 0
  for i in countdown(maxBits, 1):
    rankIdx[i - 1] = rankIdx[i] + rankCount[i] * (1 shl (maxBits - i))
  for i in 0 ..< weights.len:
    let nb = bitsOf[i]
    if nb == 0: continue
    let code = rankIdx[nb]
    let n = 1 shl (maxBits - nb)
    if code + n > size: raise zerr("bad Huffman table")
    for k in code ..< code + n:
      result.symbol[k] = uint8(i)
      result.bits[k] = uint8(nb)
    rankIdx[nb] += n

proc fseWeights(src: openArray[byte]): seq[int] =
  let h = src.len
  var fb = fwd(src)
  let (freqs, accLog) = readFseFreqs(fb, 255, 6)
  let t = buildFse(freqs, accLog)
  let used = fb.bytesUsed
  if used >= h: raise zerr("bad Huffman weights stream")
  var bb = back(src.toOpenArray(used, h - 1))
  var s1 = int(bb.read(accLog))
  var s2 = int(bb.read(accLog))
  while true:
    if result.len > 255: raise zerr("too many Huffman weights")
    result.add t.decodeState(s1, bb)
    if bb.bitPos < 0:
      result.add int(t.entries[s2].symbol)
      break
    result.add t.decodeState(s2, bb)
    if bb.bitPos < 0:
      result.add int(t.entries[s1].symbol)
      break

proc readHufTable(d: openArray[byte]; pos: var int): HufTable =
  if pos >= d.len: raise zerr("truncated Huffman header")
  let h = int(d[pos])
  inc pos
  var weights: seq[int]
  if h >= 128:
    let n = h - 127
    let nbytes = (n + 1) div 2
    if pos + nbytes > d.len: raise zerr("truncated Huffman weights")
    for i in 0 ..< n:
      let b = d[pos + i div 2]
      weights.add (if i mod 2 == 0: int(b shr 4) else: int(b and 15))
    pos += nbytes
  else:
    if pos + h > d.len: raise zerr("truncated Huffman weights")
    weights = fseWeights(d.toOpenArray(pos, pos + h - 1))
    pos += h
  buildHuf(weights)

proc decodeHufStream(t: HufTable; src: openArray[byte]; n: int; outp: var seq[byte]) =
  var b = back(src)
  for _ in 0 ..< n:
    let idx = int(b.peek(t.maxBits))
    outp.add t.symbol[idx]
    b.bitPos -= int(t.bits[idx])
  if b.bitPos != 0: raise zerr("Huffman stream not fully consumed")

# ---------------------------------------------------------------- sequences tables

const
  LLBase = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 20, 22, 24, 28, 32, 40, 48, 64,
            128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536]
  LLBits = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 4, 6, 7, 8, 9, 10, 11, 12,
            13, 14, 15, 16]
  MLBase = [3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
            30, 31, 32, 33, 34, 35, 37, 39, 41, 43, 47, 51, 59, 67, 83, 99, 131, 259, 515, 1027, 2051, 4099,
            8195, 16387, 32771, 65539]
  MLBits = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  LLDefault = [4, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 1, 1, 1, 1, 1,
               -1, -1, -1, -1]
  MLDefault = [1, 4, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
               1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1]
  OFDefault = [1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1]
  BlockMax = 128 * 1024

# ---------------------------------------------------------------- frame decoding

type
  FrameState = object
    hasHuf: bool
    huf: HufTable
    ll, ml, oft: FseTable
    haveLl, haveMl, haveOf: bool
    rep: array[3, int]

proc decodeLiterals(d: openArray[byte]; pos: var int; st: var FrameState; lits: var seq[byte]) =
  if pos >= d.len: raise zerr("truncated literals header")
  let b0 = int(d[pos])
  let ltype = b0 and 3
  let sf = (b0 shr 2) and 3
  lits.setLen(0)
  template need(n: int) =
    if pos + n > d.len: raise zerr("truncated literals")
  if ltype <= 1:
    var size: int
    case sf
    of 0, 2:
      size = b0 shr 3
      pos += 1
    of 1:
      need(2)
      size = (b0 shr 4) + (int(d[pos + 1]) shl 4)
      pos += 2
    else:
      need(3)
      size = (b0 shr 4) + (int(d[pos + 1]) shl 4) + (int(d[pos + 2]) shl 12)
      pos += 3
    if size > BlockMax: raise zerr("literals too large")
    if ltype == 0:
      need(size)
      lits.add d.toOpenArray(pos, pos + size - 1)
      pos += size
    else:
      need(1)
      for _ in 0 ..< size: lits.add d[pos]
      pos += 1
    return
  var regen, comp, streams: int
  case sf
  of 0, 1:
    need(3)
    let h = b0 or (int(d[pos + 1]) shl 8) or (int(d[pos + 2]) shl 16)
    regen = (h shr 4) and 0x3FF
    comp = (h shr 14) and 0x3FF
    streams = (if sf == 0: 1 else: 4)
    pos += 3
  of 2:
    need(4)
    let h = b0 or (int(d[pos + 1]) shl 8) or (int(d[pos + 2]) shl 16) or (int(d[pos + 3]) shl 24)
    regen = (h shr 4) and 0x3FFF
    comp = (h shr 18) and 0x3FFF
    streams = 4
    pos += 4
  else:
    need(5)
    let h = int64(b0) or (int64(d[pos + 1]) shl 8) or (int64(d[pos + 2]) shl 16) or (int64(d[pos + 3]) shl 24) or
            (int64(d[pos + 4]) shl 32)
    regen = int((h shr 4) and 0x3FFFF)
    comp = int((h shr 22) and 0x3FFFF)
    streams = 4
    pos += 5
  if regen > BlockMax: raise zerr("literals too large")
  need(comp)
  let endPos = pos + comp
  var p = pos
  if ltype == 2:
    st.huf = readHufTable(d.toOpenArray(0, endPos - 1), p)
    st.hasHuf = true
  elif not st.hasHuf:
    raise zerr("treeless literals without a previous Huffman table")
  template body: untyped = d.toOpenArray(p, endPos - 1)
  if streams == 1:
    decodeHufStream(st.huf, body, regen, lits)
  else:
    if body.len < 6: raise zerr("truncated literal jump table")
    let s1 = int(body[0]) or (int(body[1]) shl 8)
    let s2 = int(body[2]) or (int(body[3]) shl 8)
    let s3 = int(body[4]) or (int(body[5]) shl 8)
    let total = body.len - 6
    let s4 = total - s1 - s2 - s3
    if s4 < 1 or s1 < 1 or s2 < 1 or s3 < 1: raise zerr("bad literal jump table")
    let per = (regen + 3) div 4
    let last = regen - 3 * per
    if last < 0: raise zerr("bad literal sizes")
    var off = 6
    for (sz, n) in [(s1, per), (s2, per), (s3, per), (s4, last)]:
      decodeHufStream(st.huf, body.toOpenArray(off, off + sz - 1), n, lits)
      off += sz
  pos = endPos

proc readTable(d: openArray[byte]; pos: var int; mode: int; def: openArray[int]; defLog, maxSym, maxLog: int;
               have: var bool; t: var FseTable) =
  case mode
  of 0:
    t = buildFse(def, defLog)
  of 1:
    if pos >= d.len: raise zerr("truncated RLE table")
    if int(d[pos]) > maxSym: raise zerr("bad RLE symbol")
    t = rleTable(int(d[pos]))
    inc pos
  of 2:
    var fb = fwd(d.toOpenArray(pos, d.len - 1))
    let (freqs, accLog) = readFseFreqs(fb, maxSym, maxLog)
    t = buildFse(freqs, accLog)
    pos += fb.bytesUsed
  else:
    if not have: raise zerr("repeat table without a previous table")
  have = true

proc decodeBlock(d: openArray[byte]; st: var FrameState; outp: var seq[byte]; frameStart, window, maxOut: int) =
  var pos = 0
  var lits: seq[byte]
  decodeLiterals(d, pos, st, lits)
  if pos >= d.len: raise zerr("truncated sequences header")
  var nbSeq = int(d[pos])
  inc pos
  if nbSeq >= 128:
    if nbSeq < 255:
      if pos >= d.len: raise zerr("truncated sequences header")
      nbSeq = ((nbSeq - 128) shl 8) + int(d[pos])
      inc pos
    else:
      if pos + 2 > d.len: raise zerr("truncated sequences header")
      nbSeq = int(d[pos]) + (int(d[pos + 1]) shl 8) + 0x7F00
      pos += 2
  var litPos = 0
  template emitLiterals(n: int) =
    if litPos + n > lits.len: raise zerr("literal length exceeds literals")
    if outp.len + n > maxOut: raise zerr("output larger than allowed")
    for k in 0 ..< n: outp.add lits[litPos + k]
    litPos += n
  if nbSeq > 0:
    if pos >= d.len: raise zerr("truncated sequences header")
    let modes = int(d[pos])
    inc pos
    if (modes and 3) != 0: raise zerr("reserved sequence mode bits set")
    readTable(d, pos, (modes shr 6) and 3, LLDefault, 6, 35, 9, st.haveLl, st.ll)
    readTable(d, pos, (modes shr 4) and 3, OFDefault, 5, 31, 8, st.haveOf, st.oft)
    readTable(d, pos, (modes shr 2) and 3, MLDefault, 6, 52, 9, st.haveMl, st.ml)
    if pos >= d.len: raise zerr("missing sequence bitstream")
    var b = back(d.toOpenArray(pos, d.len - 1))
    var sLl = int(b.read(st.ll.accLog))
    var sOf = int(b.read(st.oft.accLog))
    var sMl = int(b.read(st.ml.accLog))
    for i in 0 ..< nbSeq:
      let llCode = int(st.ll.entries[sLl].symbol)
      let ofCode = int(st.oft.entries[sOf].symbol)
      let mlCode = int(st.ml.entries[sMl].symbol)
      if llCode > 35 or mlCode > 52 or ofCode > 31: raise zerr("bad sequence code")
      let offsetValue = int((1'u64 shl ofCode) + b.read(ofCode))
      let ml = MLBase[mlCode] + int(b.read(MLBits[mlCode]))
      let ll = LLBase[llCode] + int(b.read(LLBits[llCode]))
      if i < nbSeq - 1:
        discard st.ll.decodeState(sLl, b)
        discard st.ml.decodeState(sMl, b)
        discard st.oft.decodeState(sOf, b)
      if b.bitPos < 0: raise zerr("sequence bitstream overflow")
      var offset: int
      if offsetValue <= 3:
        var idx = offsetValue - 1
        if ll == 0: inc idx
        if idx == 0: offset = st.rep[0]
        else:
          offset = (if idx < 3: st.rep[idx] else: st.rep[0] - 1)
          if idx > 1: st.rep[2] = st.rep[1]
          st.rep[1] = st.rep[0]
          st.rep[0] = offset
      else:
        offset = offsetValue - 3
        st.rep[2] = st.rep[1]
        st.rep[1] = st.rep[0]
        st.rep[0] = offset
      emitLiterals(ll)
      if offset <= 0 or offset > outp.len - frameStart or offset > window:
        raise zerr("match offset out of range")
      if outp.len + ml > maxOut: raise zerr("output larger than allowed")
      let start = outp.len - offset
      for k in 0 ..< ml: outp.add outp[start + k]
    if b.bitPos != 0: raise zerr("sequence bitstream not fully consumed")
  emitLiterals(lits.len - litPos)

proc zstdDecompress*(data: openArray[byte]; maxOut = high(int)): seq[byte] =
  ## Decompresses every frame in `data`; raises `ZstdError` on corrupt input or when the output
  ## would exceed `maxOut` bytes.
  var pos = 0
  if data.len == 0: raise zerr("empty input")
  while pos < data.len:
    if pos + 4 > data.len: raise zerr("truncated frame header")
    let magic = le32(data, pos)
    if (magic and 0xFFFFFFF0'u32) == 0x184D2A50'u32:
      if pos + 8 > data.len: raise zerr("truncated skippable frame")
      let size = int(le32(data, pos + 4))
      pos += 8
      if pos + size > data.len: raise zerr("truncated skippable frame")
      pos += size
      continue
    if magic != 0xFD2FB528'u32: raise zerr("bad magic number")
    pos += 4
    if pos >= data.len: raise zerr("truncated frame header")
    let desc = int(data[pos])
    inc pos
    let fcsFlag = desc shr 6
    let single = ((desc shr 5) and 1) != 0
    if ((desc shr 3) and 1) != 0: raise zerr("reserved frame header bit set")
    let checksum = ((desc shr 2) and 1) != 0
    let dictFlag = desc and 3
    var window = 0
    if not single:
      if pos >= data.len: raise zerr("truncated frame header")
      let wd = int(data[pos])
      inc pos
      let exp = wd shr 3
      let mant = wd and 7
      let wlog = 10 + exp
      if wlog > 41: raise zerr("window too large")
      let base = 1'i64 shl wlog
      window = int(min(base + (base div 8) * mant, int64(high(int32))))
    let dictBytes = [0, 1, 2, 4][dictFlag]
    if pos + dictBytes > data.len: raise zerr("truncated frame header")
    var dictId = 0'u64
    for k in 0 ..< dictBytes: dictId = dictId or (uint64(data[pos + k]) shl (8 * k))
    pos += dictBytes
    if dictId != 0: raise zerr("dictionaries are not supported")
    let fcsBytes = (case fcsFlag
      of 0: (if single: 1 else: 0)
      of 1: 2
      of 2: 4
      else: 8)
    if pos + fcsBytes > data.len: raise zerr("truncated frame header")
    var fcs = -1'i64
    if fcsBytes > 0:
      var v = 0'u64
      for k in 0 ..< fcsBytes: v = v or (uint64(data[pos + k]) shl (8 * k))
      if fcsBytes == 2: v += 256
      fcs = (if v > uint64(high(int64)): high(int64) else: int64(v))
      pos += fcsBytes
    if single:
      if fcs < 0: raise zerr("single segment frame without content size")
      window = int(min(fcs, int64(high(int32))))
    if fcs >= 0 and fcs > int64(maxOut) - int64(result.len): raise zerr("output larger than allowed")
    let frameStart = result.len
    var st = FrameState(rep: [1, 4, 8])
    let blockMax = (if window > 0: min(window, BlockMax) else: BlockMax)
    while true:
      if pos + 3 > data.len: raise zerr("truncated block header")
      let bh = int(data[pos]) or (int(data[pos + 1]) shl 8) or (int(data[pos + 2]) shl 16)
      pos += 3
      let last = (bh and 1) != 0
      let btype = (bh shr 1) and 3
      let bsize = bh shr 3
      case btype
      of 0:
        if pos + bsize > data.len: raise zerr("truncated raw block")
        if bsize > blockMax: raise zerr("block too large")
        if result.len + bsize > maxOut: raise zerr("output larger than allowed")
        result.add data.toOpenArray(pos, pos + bsize - 1)
        pos += bsize
      of 1:
        if pos + 1 > data.len: raise zerr("truncated RLE block")
        if bsize > blockMax: raise zerr("block too large")
        if result.len + bsize > maxOut: raise zerr("output larger than allowed")
        let v = data[pos]
        for _ in 0 ..< bsize: result.add v
        pos += 1
      of 2:
        if pos + bsize > data.len: raise zerr("truncated compressed block")
        if bsize > blockMax: raise zerr("block too large")
        let before = result.len
        decodeBlock(data.toOpenArray(pos, pos + bsize - 1), st, result, frameStart,
                    (if window > 0: window else: high(int)), maxOut)
        if result.len - before > BlockMax: raise zerr("block decompresses past 128 KiB")
        pos += bsize
      else: raise zerr("reserved block type")
      if last: break
    if fcs >= 0 and int64(result.len - frameStart) != fcs: raise zerr("content size mismatch")
    if checksum:
      if pos + 4 > data.len: raise zerr("truncated checksum")
      let want = le32(data, pos)
      let got = uint32(xxh64(result.toOpenArray(frameStart, result.len - 1)) and 0xFFFF_FFFF'u64)
      if want != got: raise zerr("checksum mismatch")
      pos += 4

# ---------------------------------------------------------------- encoding

type
  BitWriter = object
    buf: seq[byte]
    acc: uint64
    n: int
  CTable = object
    accLog: int
    stateTable: seq[uint16]
    deltaNbBits: seq[int32]
    deltaFindState: seq[int32]
  Sequence = object
    lit, ml, offsetValue: int

proc addBits(w: var BitWriter; v: uint64; n: int) {.inline.} =
  if n == 0: return
  w.acc = w.acc or ((v and ((1'u64 shl n) - 1)) shl w.n)
  w.n += n
  while w.n >= 8:
    w.buf.add byte(w.acc and 0xff)
    w.acc = w.acc shr 8
    w.n -= 8

proc close(w: var BitWriter): seq[byte] =
  w.addBits(1, 1)
  if w.n > 0:
    w.buf.add byte(w.acc and 0xff)
    w.acc = 0
    w.n = 0
  w.buf

proc buildCTable(norm: openArray[int]; accLog: int): CTable =
  let size = 1 shl accLog
  let mask = size - 1
  let step = (size shr 1) + (size shr 3) + 3
  var high = size - 1
  var cumul = newSeq[int](norm.len + 1)
  var tableSymbol = newSeq[int](size)
  for u in 1 .. norm.len:
    if norm[u - 1] == -1:
      cumul[u] = cumul[u - 1] + 1
      tableSymbol[high] = u - 1
      dec high
    else:
      cumul[u] = cumul[u - 1] + norm[u - 1]
  var pos = 0
  for s, f in norm:
    for _ in 0 ..< max(f, 0):
      tableSymbol[pos] = s
      pos = (pos + step) and mask
      while pos > high: pos = (pos + step) and mask
  result = CTable(accLog: accLog, stateTable: newSeq[uint16](size), deltaNbBits: newSeq[int32](norm.len),
                  deltaFindState: newSeq[int32](norm.len))
  var c = cumul
  for u in 0 ..< size:
    let s = tableSymbol[u]
    result.stateTable[c[s]] = uint16(size + u)
    inc c[s]
  var total = 0
  for s, f in norm:
    case f
    of 0: result.deltaNbBits[s] = int32(((accLog + 1) shl 16) - size)
    of -1, 1:
      result.deltaNbBits[s] = int32((accLog shl 16) - size)
      result.deltaFindState[s] = int32(total - 1)
      inc total
    else:
      let maxBitsOut = accLog - fastLog2(f - 1)
      let minStatePlus = f shl maxBitsOut
      result.deltaNbBits[s] = int32((maxBitsOut shl 16) - minStatePlus)
      result.deltaFindState[s] = int32(total - f)
      total += f

proc initState(t: CTable; s: int): int =
  let nbBitsOut = (int(t.deltaNbBits[s]) + (1 shl 15)) shr 16
  let v = (nbBitsOut shl 16) - int(t.deltaNbBits[s])
  int(t.stateTable[(v shr nbBitsOut) + int(t.deltaFindState[s])])

proc encodeSym(w: var BitWriter; t: CTable; state: var int; s: int) {.inline.} =
  let nbBitsOut = (state + int(t.deltaNbBits[s])) shr 16
  w.addBits(uint64(state), nbBitsOut)
  state = int(t.stateTable[(state shr nbBitsOut) + int(t.deltaFindState[s])])

proc codeOf(base: openArray[int]; v: int): int =
  result = 0
  for i in 1 ..< base.len:
    if base[i] <= v: result = i else: break

var ctLL {.threadvar.}, ctML {.threadvar.}, ctOF {.threadvar.}: CTable
var ctReady {.threadvar.}: bool

proc tables() =
  if not ctReady:
    ctLL = buildCTable(LLDefault, 6)
    ctML = buildCTable(MLDefault, 6)
    ctOF = buildCTable(OFDefault, 5)
    ctReady = true

proc literalsHeader(n: int; outp: var seq[byte]) =
  if n < 32: outp.add byte(n shl 3)
  elif n < 4096:
    outp.add byte(((n and 0xF) shl 4) or (1 shl 2))
    outp.add byte(n shr 4)
  else:
    outp.add byte(((n and 0xF) shl 4) or (3 shl 2))
    outp.add byte((n shr 4) and 0xff)
    outp.add byte((n shr 12) and 0xff)

proc compressBlock(data: openArray[byte]; start, stop, window: int; head: var seq[int32];
                   chain: var seq[int32]): seq[byte] =
  ## A compressed block for `data[start ..< stop]` (matches may reach back into earlier data).
  const HashBits = 16
  const MinMatch = 4
  const MaxChain = 16
  var seqs: seq[Sequence]
  var lits: seq[byte]
  var anchor = start
  var i = start
  template hash4(p: int): int =
    int(((uint32(data[p]) or (uint32(data[p + 1]) shl 8) or (uint32(data[p + 2]) shl 16) or
          (uint32(data[p + 3]) shl 24)) * 2654435761'u32) shr (32 - HashBits))
  while i + MinMatch <= stop:
    let h = hash4(i)
    var cand = int(head[h])
    var bestLen = 0
    var bestOff = 0
    var tries = 0
    while cand >= 0 and i - cand <= window and tries < MaxChain:
      var l = 0
      let lim = stop - i
      while l < lim and data[cand + l] == data[i + l]: inc l
      if l > bestLen:
        bestLen = l
        bestOff = i - cand
        if l >= 256: break
      cand = int(chain[cand])
      inc tries
    chain[i] = head[h]
    head[h] = int32(i)
    if bestLen >= MinMatch and bestLen <= 131074:
      for k in anchor ..< i: lits.add data[k]
      seqs.add Sequence(lit: i - anchor, ml: bestLen, offsetValue: bestOff + 3)
      let e = i + bestLen
      inc i
      while i < e:
        if i + MinMatch <= stop:
          let h2 = hash4(i)
          chain[i] = head[h2]
          head[h2] = int32(i)
        inc i
      anchor = e
    else:
      inc i
  for k in anchor ..< stop: lits.add data[k]
  literalsHeader(lits.len, result)
  result.add lits
  let n = seqs.len
  if n < 128: result.add byte(n)
  elif n < 0x7F00:
    result.add byte((n shr 8) + 128)
    result.add byte(n and 0xff)
  else:
    result.add 255'u8
    result.add byte((n - 0x7F00) and 0xff)
    result.add byte((n - 0x7F00) shr 8)
  if n == 0: return
  result.add 0'u8 # all three tables predefined
  tables()
  var w: BitWriter
  var llc = newSeq[int](n)
  var mlc = newSeq[int](n)
  var ofc = newSeq[int](n)
  for k, s in seqs:
    llc[k] = codeOf(LLBase, s.lit)
    mlc[k] = codeOf(MLBase, s.ml)
    ofc[k] = fastLog2(s.offsetValue)
  let last = n - 1
  var stMl = initState(ctML, mlc[last])
  var stOf = initState(ctOF, ofc[last])
  var stLl = initState(ctLL, llc[last])
  w.addBits(uint64(seqs[last].lit - LLBase[llc[last]]), LLBits[llc[last]])
  w.addBits(uint64(seqs[last].ml - MLBase[mlc[last]]), MLBits[mlc[last]])
  w.addBits(uint64(seqs[last].offsetValue), ofc[last])
  for k in countdown(n - 2, 0):
    w.encodeSym(ctOF, stOf, ofc[k])
    w.encodeSym(ctML, stMl, mlc[k])
    w.encodeSym(ctLL, stLl, llc[k])
    w.addBits(uint64(seqs[k].lit - LLBase[llc[k]]), LLBits[llc[k]])
    w.addBits(uint64(seqs[k].ml - MLBase[mlc[k]]), MLBits[mlc[k]])
    w.addBits(uint64(seqs[k].offsetValue), ofc[k])
  w.addBits(uint64(stMl), ctML.accLog)
  w.addBits(uint64(stOf), ctOF.accLog)
  w.addBits(uint64(stLl), ctLL.accLog)
  result.add w.close

proc zstdCompress*(data: openArray[byte]): seq[byte] =
  ## A single zstd frame holding `data` (content size recorded, no checksum).
  const WindowLog = 23
  let window = 1 shl WindowLog
  result = @[0x28'u8, 0xB5, 0x2F, 0xFD]
  let n = data.len
  let single = n <= window
  if single:
    result.add byte((3 shl 6) or (1 shl 5))
  else:
    result.add byte(3 shl 6)
    result.add byte((WindowLog - 10) shl 3)
  for k in 0 ..< 8: result.add byte((uint64(n) shr (8 * k)) and 0xff)
  if n == 0:
    result.add [1'u8, 0, 0]   # last, raw, size 0
    return
  var head = newSeq[int32](1 shl 16)
  for v in head.mitems: v = -1
  var chain = newSeq[int32](n)
  var pos = 0
  while pos < n:
    let stop = min(pos + BlockMax, n)
    let last = stop == n
    let size = stop - pos
    var allSame = true
    for k in pos + 1 ..< stop:
      if data[k] != data[pos]:
        allSame = false
        break
    var hdr: int
    if allSame:
      hdr = (size shl 3) or (1 shl 1) or int(last)
      result.add [byte(hdr and 0xff), byte((hdr shr 8) and 0xff), byte(hdr shr 16)]
      result.add data[pos]
      # keep the match finder's tables current for later blocks
      for k in pos ..< stop: chain[k] = -1
    else:
      let comp = compressBlock(data, pos, stop, window, head, chain)
      if comp.len < size:
        hdr = (comp.len shl 3) or (2 shl 1) or int(last)
        result.add [byte(hdr and 0xff), byte((hdr shr 8) and 0xff), byte(hdr shr 16)]
        result.add comp
      else:
        hdr = (size shl 3) or int(last)
        result.add [byte(hdr and 0xff), byte((hdr shr 8) and 0xff), byte(hdr shr 16)]
        result.add data.toOpenArray(pos, stop - 1)
    pos = stop
