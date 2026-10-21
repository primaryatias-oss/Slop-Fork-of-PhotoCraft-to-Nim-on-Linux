## Rust's `std::collections::hash_map::DefaultHasher` (SipHash-1-3 with zero keys), for values
## that the Rust build exposes as fingerprints (profile content hashes) so both builds agree.

type SipHasher13* = object
  v0, v1, v2, v3: uint64
  tail: uint64
  ntail: int
  length: int

proc rotl(x: uint64; b: int): uint64 {.inline.} = (x shl b) or (x shr (64 - b))

proc sipRound(h: var SipHasher13) {.inline.} =
  h.v0 += h.v1
  h.v1 = rotl(h.v1, 13)
  h.v1 = h.v1 xor h.v0
  h.v0 = rotl(h.v0, 32)
  h.v2 += h.v3
  h.v3 = rotl(h.v3, 16)
  h.v3 = h.v3 xor h.v2
  h.v0 += h.v3
  h.v3 = rotl(h.v3, 21)
  h.v3 = h.v3 xor h.v0
  h.v2 += h.v1
  h.v1 = rotl(h.v1, 17)
  h.v1 = h.v1 xor h.v2
  h.v2 = rotl(h.v2, 32)

proc initSipHasher13*(k0 = 0'u64; k1 = 0'u64): SipHasher13 =
  SipHasher13(v0: k0 xor 0x736f6d6570736575'u64, v1: k1 xor 0x646f72616e646f6d'u64,
              v2: k0 xor 0x6c7967656e657261'u64, v3: k1 xor 0x7465646279746573'u64)

proc compress(h: var SipHasher13; m: uint64) {.inline.} =
  h.v3 = h.v3 xor m
  h.sipRound()
  h.v0 = h.v0 xor m

proc write*(h: var SipHasher13; data: openArray[byte]) =
  for b in data:
    h.tail = h.tail or (uint64(b) shl (8 * h.ntail))
    inc h.ntail
    if h.ntail == 8:
      h.compress(h.tail)
      h.tail = 0
      h.ntail = 0
  h.length += data.len

proc writeU64*(h: var SipHasher13; v: uint64) =
  var b: array[8, byte]
  for i in 0 ..< 8: b[i] = byte((v shr (8 * i)) and 0xff)
  h.write(b)

proc finish*(h: SipHasher13): uint64 =
  var s = h
  let b = (uint64(s.length and 0xff) shl 56) or s.tail
  s.compress(b)
  s.v2 = s.v2 xor 0xff
  for _ in 0 ..< 3: s.sipRound()
  s.v0 xor s.v1 xor s.v2 xor s.v3

proc rustHashBytes*(bytes: openArray[byte]): uint64 =
  ## `<[u8]>::hash` / `Vec<u8>::hash` into a fresh `DefaultHasher`: the length (usize) first.
  var h = initSipHasher13()
  h.writeU64(uint64(bytes.len))
  h.write(bytes)
  h.finish()
