## BLAKE3 hashing (default hash mode, 32-byte output), replacing the Rust `blake3` crate for
## content-addressed blob names in the native format. Straight port of the reference algorithm.

const
  Iv = [0x6A09E667'u32, 0xBB67AE85'u32, 0x3C6EF372'u32, 0xA54FF53A'u32,
        0x510E527F'u32, 0x9B05688C'u32, 0x1F83D9AB'u32, 0x5BE0CD19'u32]
  Perm = [2, 6, 3, 10, 7, 0, 4, 13, 1, 11, 12, 5, 9, 14, 15, 8]
  ChunkStart = 1'u32
  ChunkEnd = 2'u32
  Parent = 4'u32
  Root = 8'u32
  BlockLen = 64
  ChunkLen = 1024

type
  Words16 = array[16, uint32]
  Cv = array[8, uint32]
  Output = object
    inputCv: Cv
    blockWords: Words16
    counter: uint64
    blockLen: uint32
    flags: uint32
  ChunkState = object
    cv: Cv
    chunkCounter: uint64
    blk: array[BlockLen, byte]
    blkLen: int
    blocksCompressed: int
  Blake3* = object
    ## Incremental hasher.
    chunk: ChunkState
    stack: seq[Cv]

proc rotr(x: uint32; n: int): uint32 {.inline.} = (x shr n) or (x shl (32 - n))

template g(s: var Words16; a, b, c, d: int; mx, my: uint32) =
  s[a] = s[a] + s[b] + mx
  s[d] = rotr(s[d] xor s[a], 16)
  s[c] = s[c] + s[d]
  s[b] = rotr(s[b] xor s[c], 12)
  s[a] = s[a] + s[b] + my
  s[d] = rotr(s[d] xor s[a], 8)
  s[c] = s[c] + s[d]
  s[b] = rotr(s[b] xor s[c], 7)

proc compress(cv: Cv; m0: Words16; counter: uint64; blockLen, flags: uint32): Words16 =
  var s: Words16 = [cv[0], cv[1], cv[2], cv[3], cv[4], cv[5], cv[6], cv[7],
                    Iv[0], Iv[1], Iv[2], Iv[3], uint32(counter and 0xffff_ffff'u64), uint32(counter shr 32),
                    blockLen, flags]
  var m = m0
  for r in 0 ..< 7:
    g(s, 0, 4, 8, 12, m[0], m[1])
    g(s, 1, 5, 9, 13, m[2], m[3])
    g(s, 2, 6, 10, 14, m[4], m[5])
    g(s, 3, 7, 11, 15, m[6], m[7])
    g(s, 0, 5, 10, 15, m[8], m[9])
    g(s, 1, 6, 11, 12, m[10], m[11])
    g(s, 2, 7, 8, 13, m[12], m[13])
    g(s, 3, 4, 9, 14, m[14], m[15])
    if r < 6:
      var p: Words16
      for i in 0 ..< 16: p[i] = m[Perm[i]]
      m = p
  for i in 0 ..< 8:
    s[i] = s[i] xor s[i + 8]
    s[i + 8] = s[i + 8] xor cv[i]
  s

proc words(b: openArray[byte]): Words16 =
  for i in 0 ..< 16:
    result[i] = uint32(b[4 * i]) or (uint32(b[4 * i + 1]) shl 8) or (uint32(b[4 * i + 2]) shl 16) or (uint32(b[4 * i + 3]) shl 24)

proc chainingValue(o: Output): Cv =
  let w = compress(o.inputCv, o.blockWords, o.counter, o.blockLen, o.flags)
  for i in 0 ..< 8: result[i] = w[i]

proc newChunkState(key: Cv; counter: uint64): ChunkState = ChunkState(cv: key, chunkCounter: counter)

proc len(c: ChunkState): int = BlockLen * c.blocksCompressed + c.blkLen
proc startFlag(c: ChunkState): uint32 = (if c.blocksCompressed == 0: ChunkStart else: 0)

proc update(c: var ChunkState; input: openArray[byte]) =
  var i = 0
  while i < input.len:
    if c.blkLen == BlockLen:
      let w = compress(c.cv, words(c.blk), c.chunkCounter, BlockLen, c.startFlag)
      for k in 0 ..< 8: c.cv[k] = w[k]
      inc c.blocksCompressed
      c.blk = default(array[BlockLen, byte])
      c.blkLen = 0
    let take = min(BlockLen - c.blkLen, input.len - i)
    for k in 0 ..< take: c.blk[c.blkLen + k] = input[i + k]
    c.blkLen += take
    i += take

proc output(c: ChunkState): Output =
  Output(inputCv: c.cv, blockWords: words(c.blk), counter: c.chunkCounter, blockLen: uint32(c.blkLen),
         flags: c.startFlag or ChunkEnd)

proc parentOutput(left, right: Cv): Output =
  var w: Words16
  for i in 0 ..< 8:
    w[i] = left[i]
    w[i + 8] = right[i]
  Output(inputCv: Iv, blockWords: w, counter: 0, blockLen: BlockLen, flags: Parent)

proc newBlake3*(): Blake3 = Blake3(chunk: newChunkState(Iv, 0))

proc update*(h: var Blake3; input: openArray[byte]) =
  var i = 0
  while i < input.len:
    if h.chunk.len == ChunkLen:
      var cv = h.chunk.output.chainingValue
      var total = h.chunk.chunkCounter + 1
      while (total and 1) == 0:
        cv = parentOutput(h.stack.pop, cv).chainingValue
        total = total shr 1
      h.stack.add cv
      h.chunk = newChunkState(Iv, h.chunk.chunkCounter + 1)
    let take = min(ChunkLen - h.chunk.len, input.len - i)
    h.chunk.update(input.toOpenArray(i, i + take - 1))
    i += take

proc finish*(h: Blake3): array[32, byte] =
  var o = h.chunk.output
  var n = h.stack.len
  while n > 0:
    dec n
    o = parentOutput(h.stack[n], o.chainingValue)
  let w = compress(o.inputCv, o.blockWords, 0, o.blockLen, o.flags or Root)
  for i in 0 ..< 8:
    for k in 0 ..< 4: result[4 * i + k] = byte((w[i] shr (8 * k)) and 0xff)

proc blake3*(data: openArray[byte]): array[32, byte] =
  var h = newBlake3()
  h.update(data)
  h.finish

proc blake3Hex*(data: openArray[byte]): string =
  ## Lower-case hex of the 32-byte hash (Rust `blake3::hash(b).to_hex()`).
  const digits = "0123456789abcdef"
  for b in blake3(data):
    result.add digits[int(b shr 4)]
    result.add digits[int(b and 15)]
