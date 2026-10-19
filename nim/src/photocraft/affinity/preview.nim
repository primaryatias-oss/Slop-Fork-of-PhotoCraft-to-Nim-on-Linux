## The indexed PNG thumbnail Affinity stores next to the archive (port of
## `affinity/src/preview.rs`): Affinity's own render of the document, at most 512 pixels on the
## long side in real files, never the document itself.

import ./container
import ../zlib

const
  MaxPreviewBytes* = 16 shl 20
  MaxPreviewDimension* = 4096'u32

type Preview* = object
  ## The embedded thumbnail, at its own dimensions, never the native document dimensions.
  pngStart*, pngLen*: int ## the PNG's range in the file
  width*, height*: uint32

proc rangeAt(len, offset, length: int): int =
  ## Checks that `offset ..< offset + length` lies in `len` bytes; returns the end.
  if length < 0 or offset > high(int) - length: raise malformed("offset overflow")
  let e = offset + length
  if offset < 0 or e > len: raise malformed("truncated record")
  e

proc le32(b: openArray[byte]; base, offset: int): uint32 =
  discard rangeAt(b.len - base, offset, 4)
  let i = base + offset
  uint32(b[i]) or (uint32(b[i + 1]) shl 8) or (uint32(b[i + 2]) shl 16) or (uint32(b[i + 3]) shl 24)

proc be32(b: openArray[byte]; base, limit, offset: int): uint32 =
  discard rangeAt(limit, offset, 4)
  let i = base + offset
  (uint32(b[i]) shl 24) or (uint32(b[i + 1]) shl 16) or (uint32(b[i + 2]) shl 8) or uint32(b[i + 3])

proc eqAt(b: openArray[byte]; at: int; s: string): bool =
  for k in 0 ..< s.len:
    if b[at + k] != byte(s[k]): return false
  true

proc isAlpha(c: byte): bool = (c >= byte('A') and c <= byte('Z')) or (c >= byte('a') and c <= byte('z'))

proc preview*(bytes: openArray[byte]): Preview =
  ## Reads only the indexed `Thmb` record (identical in container versions 8 to 12), checking every
  ## PNG chunk CRC. Compressed pixels must still be validated by the consuming image decoder, with
  ## allocation limits.
  let h = header(bytes)
  if h.thumbnailOffset == 0: raise unsupported("no embedded preview")
  # (Rust holds any u64 offset; one beyond `int` is past the end of any file.)
  if h.thumbnailOffset > uint64(high(int)): raise malformed("thumbnail offset outside file")
  let offset = int(h.thumbnailOffset)
  if offset < 72: raise malformed("thumbnail overlaps the file header")
  if offset > bytes.len: raise malformed("thumbnail offset outside file")
  let recLen = bytes.len - offset
  discard rangeAt(recLen, 0, 8)
  if not eqAt(bytes, offset, "\xff\xff\xff\xffThmb"): raise malformed("missing indexed thumbnail")
  if le32(bytes, offset, 8) != 1: raise unsupported("thumbnail record version or encoding")
  discard rangeAt(recLen, 28, 1)
  if bytes[offset + 28] != 1: raise unsupported("thumbnail record version or encoding")
  let size = int(le32(bytes, offset, 24))
  if size > MaxPreviewBytes: raise limit("encoded preview exceeds 16 MiB")
  if le32(bytes, offset, 16) != 29 or le32(bytes, offset, 20) != 0 or
     uint64(le32(bytes, offset, 12)) != uint64(size) + 13:
    raise malformed("inconsistent thumbnail record lengths")
  discard rangeAt(recLen, 29, size)
  let png = offset + 29 ## the PNG's start in `bytes`; `size` bytes long
  discard rangeAt(size, 0, 8)
  if not eqAt(bytes, png, "\x89PNG\r\n\x1a\n") or be32(bytes, png, size, 8) != 13 or
     (discard rangeAt(size, 12, 4); not eqAt(bytes, png + 12, "IHDR")):
    raise malformed("thumbnail is not a PNG")
  let width = be32(bytes, png, size, 16)
  let height = be32(bytes, png, size, 20)
  if width == 0 or height == 0 or width > MaxPreviewDimension or height > MaxPreviewDimension:
    raise limit("preview dimensions must be 1..4096")
  # Bound work by bytes, not a file-supplied chunk count, and require exactly one complete PNG.
  var pos = 8
  var palette, imageData, imageDataEnded = false
  while true:
    let length = int(be32(bytes, png, size, pos))
    let chunkSize = length + 12
    discard rangeAt(size, pos, chunkSize)
    let chunk = png + pos
    if crc32(bytes.toOpenArray(chunk + 4, chunk + chunkSize - 5)) != be32(bytes, chunk, chunkSize, chunkSize - 4):
      raise malformed("PNG chunk checksum")
    var kind = ""
    for k in 0 ..< 4: kind.add char(bytes[chunk + 4 + k])
    for k in 0 ..< 4:
      if not isAlpha(byte(kind[k])): raise malformed("PNG chunk type")
    case kind
    of "IHDR":
      if pos != 8: raise malformed("duplicate PNG header")
    of "PLTE":
      if palette or imageData: raise malformed("PNG palette order")
      palette = true
    of "IDAT":
      if imageDataEnded: raise malformed("non-contiguous PNG image data")
      imageData = true
    of "IEND":
      if not imageData: raise malformed("PNG has no image data")
    else:
      if kind[0] >= 'A' and kind[0] <= 'Z': raise unsupported("unknown critical PNG chunk")
    if imageData and kind != "IDAT": imageDataEnded = true
    # Native sample previews are plain, single-frame PNGs. Pixel allocation limits do not
    # necessarily bound aggregate compressed metadata; leave these layouts unsupported.
    if kind in ["zTXt", "iTXt", "iCCP"]: raise unsupported("compressed PNG metadata in the preview")
    if kind in ["acTL", "fcTL", "fdAT"]: raise unsupported("animated PNG preview")
    pos += chunkSize
    if kind == "IEND":
      if length != 0 or pos != size: raise malformed("PNG end or trailing data")
      break
  Preview(pngStart: png, pngLen: size, width: width, height: height)

proc png*(p: Preview; bytes: openArray[byte]): seq[byte] =
  ## The thumbnail's PNG bytes.
  bytes[p.pngStart ..< p.pngStart + p.pngLen]
