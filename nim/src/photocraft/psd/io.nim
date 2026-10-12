## Bounds-checked big-endian reader and writer helpers, and the error type of every fallible
## PSD operation. Parsers never crash on malformed input: every failure raises `PsdError`.

import std/[strutils, unicode]

type
  PsdErrorKind* = enum
    peUnexpectedEof, peInvalidSignature, peUnsupportedVersion, peLimitExceeded, peInvalid,
    peDecompress, peUnsupported
  PsdError* = object of CatchableError
    kind*: PsdErrorKind

  FourCC* = string  ## a four-byte key or signature, stored as a 4-character string

  Reader* = object
    ## A cursor over borrowed bytes. The bytes must outlive the reader.
    data: ptr UncheckedArray[byte]
    len: int
    pos*: int
    little*: bool

proc psdError*(kind: PsdErrorKind; msg: string): ref PsdError =
  let prefix = case kind
    of peUnexpectedEof: "unexpected end of data: "
    of peInvalidSignature: "invalid signature: "
    of peUnsupportedVersion: "unsupported file version "
    of peLimitExceeded: "limit exceeded: "
    of peInvalid: "invalid data: "
    of peDecompress: "decompression failed: "
    of peUnsupported: "unsupported: "
  result = newException(PsdError, prefix & msg)
  result.kind = kind

proc invalid*(msg: string): ref PsdError = psdError(peInvalid, msg)
proc limitExceeded*(msg: string): ref PsdError = psdError(peLimitExceeded, msg)
proc unsupported*(msg: string): ref PsdError = psdError(peUnsupported, msg)
proc decompressErr*(msg: string): ref PsdError = psdError(peDecompress, msg)
proc eof*(offset, needed: int): ref PsdError =
  psdError(peUnexpectedEof, "at offset " & $offset & " (needed " & $needed & " more bytes)")
proc badSignature*(expected: string; found: string): ref PsdError =
  psdError(peInvalidSignature, escape(found) & ", expected " & expected)

proc newReader*(b: openArray[byte]; little = false): Reader =
  Reader(data: if b.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr b[0]) else: nil, len: b.len, little: little)

proc remaining*(r: Reader): int {.inline.} = r.len - r.pos
proc isEmpty*(r: Reader): bool {.inline.} = r.remaining == 0
proc size*(r: Reader): int {.inline.} = r.len

template restView*(r: Reader): untyped =
  ## The unread bytes as an openArray (no copy).
  toOpenArray(r.dataPtr, r.pos, r.len - 1)

proc dataPtr*(r: Reader): ptr UncheckedArray[byte] {.inline.} = r.data

proc at*(r: Reader; i: int): byte {.inline.} =
  ## Byte at absolute position `i` (caller checks bounds).
  r.data[i]

proc peekRest*(r: Reader): seq[byte] =
  result = newSeq[byte](r.remaining)
  if result.len > 0: copyMem(addr result[0], addr r.data[r.pos], result.len)

proc need(r: Reader; n: int) {.inline.} =
  if n < 0 or n > r.remaining: raise eof(r.pos, max(n - r.remaining, 0))

proc bytes*(r: var Reader; n: int): seq[byte] =
  r.need(n)
  result = newSeq[byte](n)
  if n > 0: copyMem(addr result[0], addr r.data[r.pos], n)
  r.pos += n

proc bytesU64*(r: var Reader; n: uint64): seq[byte] =
  if n > uint64(high(int)): raise limitExceeded("length exceeds address space")
  r.bytes(int(n))

proc skip*(r: var Reader; n: int) =
  r.need(n)
  r.pos += n

proc sub*(r: var Reader; n: uint64): Reader =
  ## A reader over the next `n` bytes (consumed from `r`).
  if n > uint64(high(int)): raise limitExceeded("length exceeds address space")
  r.need(int(n))
  result = Reader(data: if n > 0: cast[ptr UncheckedArray[byte]](addr r.data[r.pos]) else: nil, len: int(n), little: r.little)
  r.pos += int(n)

proc subAt*(r: Reader; start, n: int): Reader =
  ## A reader over `n` bytes at absolute `start`.
  if start < 0 or n < 0 or start + n > r.len: raise eof(start, n)
  Reader(data: if n > 0: cast[ptr UncheckedArray[byte]](addr r.data[start]) else: nil, len: n, little: r.little)

proc u8*(r: var Reader): uint8 =
  r.need(1)
  result = r.data[r.pos]
  inc r.pos

proc fourcc*(r: var Reader): FourCC =
  r.need(4)
  result = newString(4)
  for i in 0..3: result[i] = char(r.data[r.pos + i])
  r.pos += 4

proc readN(r: var Reader; n: int): uint64 =
  r.need(n)
  if r.little:
    for i in countdown(n - 1, 0): result = (result shl 8) or uint64(r.data[r.pos + i])
  else:
    for i in 0 ..< n: result = (result shl 8) or uint64(r.data[r.pos + i])
  r.pos += n

proc u16*(r: var Reader): uint16 = uint16(r.readN(2))
proc i16*(r: var Reader): int16 = cast[int16](uint16(r.readN(2)))
proc u32*(r: var Reader): uint32 = uint32(r.readN(4))
proc i32*(r: var Reader): int32 = cast[int32](uint32(r.readN(4)))
proc u64*(r: var Reader): uint64 = r.readN(8)
proc i64*(r: var Reader): int64 = cast[int64](r.readN(8))
proc f64*(r: var Reader): float64 = cast[float64](r.readN(8))
proc f32*(r: var Reader): float32 = cast[float32](uint32(r.readN(4)))

proc lenField*(r: var Reader; long: bool): uint64 =
  if long: r.u64() else: uint64(r.u32())

proc checkCount*(r: Reader; count, minItemSize: uint64) =
  ## Raises unless `count * minItemSize` bytes remain (validates counts before allocating).
  let need = if minItemSize != 0 and count > high(uint64) div minItemSize: high(uint64) else: count * minItemSize
  if need > uint64(r.remaining):
    raise eof(r.pos, int(min(need - uint64(r.remaining), uint64(high(int)))))

# ---------------------------------------------------------------- writing

proc putU8*(o: var seq[byte]; v: uint8) {.inline.} = o.add v
proc putU16*(o: var seq[byte]; v: uint16) = o.add [byte(v shr 8), byte(v and 0xFF)]
proc putI16*(o: var seq[byte]; v: int16) = o.putU16(cast[uint16](v))
proc putU32*(o: var seq[byte]; v: uint32) =
  o.add [byte(v shr 24), byte((v shr 16) and 0xFF), byte((v shr 8) and 0xFF), byte(v and 0xFF)]
proc putI32*(o: var seq[byte]; v: int32) = o.putU32(cast[uint32](v))
proc putU64*(o: var seq[byte]; v: uint64) =
  for i in countdown(7, 0): o.add byte((v shr (8 * i)) and 0xFF)
proc putI64*(o: var seq[byte]; v: int64) = o.putU64(cast[uint64](v))
proc putF64*(o: var seq[byte]; v: float64) = o.putU64(cast[uint64](v))
proc putF32*(o: var seq[byte]; v: float32) = o.putU32(cast[uint32](v))
proc put*(o: var seq[byte]; b: openArray[byte]) {.inline.} = o.add b
proc put*(o: var seq[byte]; s: string) =
  for c in s: o.add byte(c)

proc putLen*(o: var seq[byte]; v: uint64; long: bool) =
  if long: o.putU64(v)
  else:
    if v > uint64(high(uint32)): raise limitExceeded("section too large for 32-bit length (use PSB)")
    o.putU32(uint32(v))

proc beginLen*(o: var seq[byte]; long: bool): int =
  result = o.len
  o.setLen(o.len + (if long: 8 else: 4))

proc endLen*(o: var seq[byte]; at: int; long: bool) =
  let hdr = if long: 8 else: 4
  let n = uint64(o.len - at - hdr)
  if long:
    for i in 0..7: o[at + i] = byte((n shr (8 * (7 - i))) and 0xFF)
  else:
    if n > uint64(high(uint32)): raise limitExceeded("section too large for 32-bit length (use PSB)")
    for i in 0..3: o[at + i] = byte((n shr (8 * (3 - i))) and 0xFF)

proc beU16*(b: openArray[byte]; at: int): uint16 = (uint16(b[at]) shl 8) or uint16(b[at + 1])
proc beU32*(b: openArray[byte]; at: int): uint32 =
  (uint32(b[at]) shl 24) or (uint32(b[at + 1]) shl 16) or (uint32(b[at + 2]) shl 8) or uint32(b[at + 3])

proc toFourCC*(b: openArray[byte]): FourCC =
  result = newString(4)
  for i in 0..3: result[i] = (if i < b.len: char(b[i]) else: '\0')

proc fourccBytes*(s: string): seq[byte] =
  for i in 0..3: result.add (if i < s.len: byte(s[i]) else: 0'u8)

# ---------------------------------------------------------------- strings

proc readPascal*(r: var Reader; align: int): seq[byte] =
  ## A Pascal string (length byte + bytes) padded to a multiple of `align`. Returns the name bytes.
  let n = int(r.u8())
  result = r.bytes(n)
  let total = 1 + n
  r.skip((align - total mod align) mod align)

proc writePascal*(o: var seq[byte]; name: openArray[byte]; align: int) =
  let n = min(name.len, 255)
  o.putU8(uint8(n))
  for i in 0 ..< n: o.add name[i]
  let total = 1 + n
  for _ in 0 ..< (align - total mod align) mod align: o.add 0

proc decodeLegacyName*(b: openArray[byte]): string =
  ## UTF-8 when valid, otherwise Latin-1.
  var s = newString(b.len)
  for i, c in b: s[i] = char(c)
  if validateUtf8(s) == -1: return s
  result = ""
  for c in b: result.add Rune(int(c)).toUTF8

proc encodeLegacyName*(s: string): seq[byte] =
  ## Non-ASCII characters become `?`.
  for r in s.runes:
    if result.len >= 255: break
    let v = int(r)
    result.add(if v >= 0x20 and v < 0x7F: byte(v) else: byte('?'))

proc readUnicodeUnits*(r: var Reader): seq[uint16] =
  let n = uint64(r.u32())
  r.checkCount(n, 2)
  result = newSeqOfCap[uint16](int(n))
  for _ in 0'u64 ..< n: result.add r.u16()

proc writeUnicodeUnits*(o: var seq[byte]; units: openArray[uint16]) =
  o.putU32(uint32(units.len))
  for u in units: o.putU16(u)

proc utf16Units*(s: string): seq[uint16] =
  for r in s.runes:
    var v = int(r)
    if v >= 0x10000:
      v -= 0x10000
      result.add uint16(0xD800 + (v shr 10))
      result.add uint16(0xDC00 + (v and 0x3FF))
    else: result.add uint16(v)

proc fromUtf16*(units: openArray[uint16]): string =
  ## Lossy: unpaired surrogates become U+FFFD.
  var i = 0
  while i < units.len:
    let u = int(units[i])
    if u >= 0xD800 and u < 0xDC00 and i + 1 < units.len and int(units[i + 1]) >= 0xDC00 and int(units[i + 1]) < 0xE000:
      result.add Rune(0x10000 + ((u - 0xD800) shl 10) + (int(units[i + 1]) - 0xDC00)).toUTF8
      i += 2
    elif u >= 0xD800 and u < 0xE000:
      result.add Rune(0xFFFD).toUTF8
      inc i
    else:
      result.add Rune(u).toUTF8
      inc i
