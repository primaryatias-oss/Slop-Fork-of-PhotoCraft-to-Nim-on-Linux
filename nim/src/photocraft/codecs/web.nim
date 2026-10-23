## Web-optimised writers for Save for Web (port of `codecs/web.rs`): indexed GIF (our own LZW
## encoder, GIF89a spec), animated GIF and WBMP (WAP Wireless Bitmap, type 0) with its reader.
## (The JPEG writer of `web.rs` is the JPEG codec's encoder.)

import std/[options, tables]
import ./image

type
  BitSink = object
    ## Packs variable-length LZW codes LSB-first into GIF data sub-blocks.
    bytes: seq[byte]
    acc: uint32
    nbits: uint32

  GifFrame* = object
    ## One frame of an animated GIF: palette indices, its (local) palette, an optional
    ## transparent index, and the on-screen delay in centiseconds.
    indices*: seq[uint8]
    palette*: seq[array[3, uint8]]
    transparent*: Option[uint8]
    delayCs*: uint16

proc put(s: var BitSink; code: uint16; size: uint32) =
  s.acc = s.acc or (uint32(code) shl s.nbits)
  s.nbits += size
  while s.nbits >= 8:
    s.bytes.add byte(s.acc and 0xFF)
    s.acc = s.acc shr 8
    s.nbits -= 8

proc finish(s: var BitSink): seq[byte] =
  if s.nbits > 0: s.bytes.add byte(s.acc and 0xFF)
  s.bytes

proc lzwEncode*(indices: openArray[uint8]; minCodeSize: uint8): seq[byte] =
  ## GIF variable-code-length LZW compression of `indices` (each < 2^`minCodeSize`).
  let min = uint32(clamp(minCodeSize, 2'u8, 8'u8))
  let clear = uint16(1'u32 shl min)
  let eoi = clear + 1
  var size = min + 1
  var next = eoi + 1
  var dict = initTable[uint32, uint16](4096)
  var outp = BitSink(bytes: newSeqOfCap[byte](indices.len div 2 + 16))
  outp.put(clear, size)
  if indices.len == 0:
    outp.put(eoi, size)
    return outp.finish()
  var prefix = uint16(indices[0])
  for i in 1 ..< indices.len:
    let k = indices[i]
    let key = (uint32(prefix) shl 8) or uint32(k)
    let c = dict.getOrDefault(key, 0xFFFF'u16)
    if c != 0xFFFF'u16:
      prefix = c
      continue
    outp.put(prefix, size)
    # The decoder adds its entries one code late, so widen once the next free code no longer fits
    # (before assigning it), exactly like the classic compress-based encoders.
    if uint32(next) >= (1'u32 shl size) and size < 12: inc size
    if next < 4096:
      dict[key] = next
      inc next
    else:
      outp.put(clear, size)
      dict.clear()
      size = min + 1
      next = eoi + 1
    prefix = uint16(k)
  outp.put(prefix, size)
  if uint32(next) >= (1'u32 shl size) and size < 12: inc size
  outp.put(eoi, size)
  outp.finish()

proc interlacedRows(h: int): seq[int] =
  ## Rows in GIF interlaced order.
  for (start, step) in [(0, 8), (4, 8), (2, 4), (1, 2)]:
    var y = start
    while y < h:
      result.add y
      y += step

proc le16(v: int): array[2, byte] = [byte(v and 0xFF), byte((v shr 8) and 0xFF)]

proc tableBits(n: int): int =
  for b in 1 .. 8:
    if (1 shl b) >= n: return b
  8

proc addBlocks(outp: var seq[byte]; data: seq[byte]) =
  var i = 0
  while i < data.len:
    let n = min(255, data.len - i)
    outp.add byte(n)
    outp.add data[i ..< i + n]
    i += n

proc encodeGifIndexed*(width, height: uint32; indices: openArray[uint8]; palette: openArray[array[3, uint8]];
                       transparent: Option[uint8]; interlaced: bool): seq[byte] =
  ## An indexed GIF89a: `palette` (1..=256 entries), one index per pixel, optional transparent
  ## index (Graphic Control Extension) and interlacing.
  let (w, h) = (int(width), int(height))
  if width == 0 or height == 0 or width > 65535 or height > 65535 or indices.len != w * h:
    raise invalidImage("GIF size must be 1..=65535 and match the index data")
  var covered = palette.len > 0 and palette.len <= 256
  if covered:
    for i in indices:
      if int(i) >= palette.len:
        covered = false
        break
  if not covered: raise invalidImage("palette must have 1..=256 entries covering every index")
  let bits = tableBits(palette.len)
  result = @[byte('G'), byte('I'), byte('F'), byte('8'), byte('9'), byte('a')]
  result.add le16(w)
  result.add le16(h)
  result.add byte(0x80 or ((bits - 1) shl 4) or (bits - 1))
  result.add (if transparent.isSome: transparent.get else: 0'u8)
  result.add 0'u8
  for i in 0 ..< (1 shl bits):
    let c = (if i < palette.len: palette[i] else: [0'u8, 0, 0])
    result.add [c[0], c[1], c[2]]
  if transparent.isSome: result.add [0x21'u8, 0xF9, 0x04, 0x01, 0, 0, transparent.get, 0]
  result.add 0x2C'u8
  result.add [0'u8, 0, 0, 0]
  result.add le16(w)
  result.add le16(h)
  result.add (if interlaced: 0x40'u8 else: 0'u8)
  let min = uint8(max(bits, 2))
  result.add min
  var data: seq[byte]
  if interlaced:
    var v = newSeqOfCap[uint8](indices.len)
    for y in interlacedRows(h): v.add indices[y * w ..< (y + 1) * w]
    data = lzwEncode(v, min)
  else: data = lzwEncode(indices, min)
  result.addBlocks(data)
  result.add 0'u8
  result.add 0x3B'u8

proc encodeGifAnimated*(width, height: uint32; frames: openArray[GifFrame]; loopForever: bool): seq[byte] =
  ## An animated GIF (GIF89a). Each frame carries its own local colour table; `loopForever` adds
  ## the NETSCAPE2.0 looping extension.
  let (w, h) = (int(width), int(height))
  if width == 0 or height == 0 or width > 65535 or height > 65535: raise invalidImage("GIF size must be 1..=65535")
  if frames.len == 0: raise invalidImage("an animated GIF needs at least one frame")
  result = @[byte('G'), byte('I'), byte('F'), byte('8'), byte('9'), byte('a')]
  result.add le16(w)
  result.add le16(h)
  result.add [0x70'u8, 0, 0] # no global colour table, 8-bit colour resolution
  if loopForever:
    result.add [0x21'u8, 0xFF, 0x0B]
    for c in "NETSCAPE2.0": result.add byte(c)
    result.add [0x03'u8, 0x01, 0x00, 0x00, 0x00] # loop count 0 = forever
  for f in frames:
    var ok = f.palette.len > 0 and f.palette.len <= 256 and f.indices.len == w * h
    if ok:
      for i in f.indices:
        if int(i) >= f.palette.len:
          ok = false
          break
    if not ok: raise invalidImage("every frame's palette must have 1..=256 entries covering its indices")
    let bits = tableBits(f.palette.len)
    # Graphic Control Extension: delay and optional transparency. An opaque frame is left in place
    # (disposal 1), one with a transparent index is cleared afterwards (disposal 2).
    let packed = (if f.transparent.isSome: 0x09'u8 else: 0x04'u8)
    result.add [0x21'u8, 0xF9, 0x04, packed]
    result.add le16(int(f.delayCs))
    result.add (if f.transparent.isSome: f.transparent.get else: 0'u8)
    result.add 0'u8
    # Image Descriptor with a Local Colour Table.
    result.add 0x2C'u8
    result.add [0'u8, 0, 0, 0]
    result.add le16(w)
    result.add le16(h)
    result.add byte(0x80 or (bits - 1))
    for i in 0 ..< (1 shl bits):
      let c = (if i < f.palette.len: f.palette[i] else: [0'u8, 0, 0])
      result.add [c[0], c[1], c[2]]
    let min = uint8(max(bits, 2))
    result.add min
    result.addBlocks(lzwEncode(f.indices, min))
    result.add 0'u8
  result.add 0x3B'u8

proc putMultibyte(outp: var seq[byte]; v0: uint32) =
  var groups = @[byte(v0 and 0x7f)]
  var v = v0 shr 7
  while v > 0:
    groups.add byte(v and 0x7f) or 0x80
    v = v shr 7
  for i in countdown(groups.high, 0): outp.add groups[i]

proc encodeWbmp*(width, height: uint32; white: openArray[bool]): seq[byte] =
  ## WBMP type 0 (1 bit per pixel, rows padded to bytes, 1 = white). `white[i]` per pixel.
  let (w, h) = (int(width), int(height))
  if width == 0 or height == 0 or white.len != w * h: raise invalidImage("WBMP size must match the pixel data")
  result = @[0'u8, 0'u8]
  result.putMultibyte(width)
  result.putMultibyte(height)
  let stride = (w + 7) div 8
  for y in 0 ..< h:
    var row = newSeq[byte](stride)
    for x in 0 ..< w:
      if white[y * w + x]: row[x div 8] = row[x div 8] or byte(0x80 shr (x mod 8))
    result.add row

proc decodeWbmp*(bytes: openArray[byte]): (uint32, uint32, seq[bool]) =
  ## Reads a WBMP type 0 image: (width, height, white per pixel).
  template bad(): untyped = invalidImage("not a type 0 WBMP")
  var i = 0
  proc next(i: var int): byte =
    if i >= bytes.len: raise bad()
    result = bytes[i]
    inc i
  if next(i) != 0 or (next(i) and 0x9f) != 0: raise bad()
  proc mb(i: var int): uint32 =
    var v = 0'u64
    for _ in 0 ..< 5:
      let b = next(i)
      v = v * 128
      if v > uint64(high(uint32)): raise bad()
      v = v or uint64(b and 0x7f)
      if (b and 0x80) == 0: return uint32(v)
    raise bad()
  let w = mb(i)
  let h = mb(i)
  if w == 0 or h == 0 or uint64(w) * uint64(h) > (1'u64 shl 28): raise bad()
  let stride = (int(w) + 7) div 8
  if i + stride * int(h) > bytes.len: raise bad()
  var px = newSeqOfCap[bool](int(w) * int(h))
  for y in 0 ..< int(h):
    for x in 0 ..< int(w):
      px.add (bytes[i + y * stride + x div 8] and byte(0x80 shr (x mod 8))) != 0
  (w, h, px)
