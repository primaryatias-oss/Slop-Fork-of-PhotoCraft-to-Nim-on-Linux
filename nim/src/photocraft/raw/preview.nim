## Finding the camera-rendered JPEG preview embedded in a raw file (port of `raw/src/preview.rs`).

import std/options
import ./tiff, ./raf, ./orf

type
  Preview* = object
    ## An embedded baseline-JPEG preview: `len` bytes at `offset` of the raw file.
    offset*, len*: int
    width*, height*: uint32

proc dctJpegSize*(b: openArray[byte]): Option[(uint32, uint32)] =
  ## Dimensions of a JPEG whose frame is DCT-coded (baseline / extended / progressive), or none
  ## (lossless JPEG raw data, or not a JPEG).
  if b.len < 3 or b[0] != 0xFF or b[1] != 0xD8 or b[2] != 0xFF: return
  var pos = 2
  for _ in 0 ..< 256:
    if pos + 1 >= b.len or b[pos] != 0xFF: return
    let m = b[pos + 1]
    if m == 0xFF:
      inc pos
      continue
    if pos + 3 >= b.len: return
    let len = (int(b[pos + 2]) shl 8) or int(b[pos + 3])
    if m in 0xC0'u8 .. 0xC2'u8:
      if pos + 8 >= b.len: return
      let h = (uint32(b[pos + 5]) shl 8) or uint32(b[pos + 6])
      let w = (uint32(b[pos + 7]) shl 8) or uint32(b[pos + 8])
      if w > 0 and h > 0: return some((w, h))
      return
    if m == 0xC3 or m in 0xC5'u8 .. 0xC7'u8 or m in 0xC9'u8 .. 0xCB'u8 or m in 0xCD'u8 .. 0xCF'u8 or m == 0xDA or
        m == 0xD9 or len < 2: return
    pos += 2 + len

proc candidate(data: openArray[byte]; offset, len: int; outp: var seq[Preview]) =
  if offset < 0 or len < 0 or offset > data.len or len > data.len - offset: return
  let s = dctJpegSize(data.toOpenArray(offset, offset + len - 1))
  if s.isSome: outp.add Preview(offset: offset, len: len, width: s.get[0], height: s.get[1])

proc embeddedPreviews*(bytes: openArray[byte]): seq[Preview] =
  ## Every embedded DCT-JPEG preview of a raw file, each stream once.
  var found: seq[Preview]
  if raf.hasMagic(bytes):
    # RAF header: big-endian JPEG offset and length at bytes 84 and 88.
    let r = rafPreviewRange(bytes)
    if r.isSome: candidate(bytes, r.get[0], r.get[1], found)
  else:
    let tt = newTiff(bytes)
    if tt.isSome:
      let t = tt.get
      # Olympus ORF: the preview is referenced from the maker note.
      if bytes.len >= 4 and (bytes[0 .. 3] == cast[seq[byte]]("IIRO") or bytes[0 .. 3] == cast[seq[byte]]("IIRS") or
                             bytes[0 .. 3] == cast[seq[byte]]("MMOR")):
        let r = orfPreviewRange(t)
        if r.isSome: candidate(bytes, r.get[0], r.get[1], found)
      for ifd in t.allIfds:
        let (off, len) = (t.tagUint(ifd, TagJpegInterchangeFormat), t.tagUint(ifd, TagJpegInterchangeFormatLength))
        if off.isSome and len.isSome: candidate(bytes, int(off.get), int(len.get), found)
        let c = t.tagUint(ifd, TagCompression)
        if c == some(6'u32) or c == some(7'u32):
          let (offs, lens) = (t.tagUints(ifd, TagStripOffsets), t.tagUints(ifd, TagStripByteCounts))
          if offs.len == 1 and lens.len == 1: candidate(bytes, int(offs[0]), int(lens[0]), found)
        # Panasonic RW2 JpgFromRaw (0x002E) holds a complete JPEG.
        let e = ifd.get(0x002E)
        if e.isSome and e.get.typ == 7:
          let r = t.rawRange(e.get)
          if r.isSome:
            let s = dctJpegSize(bytes.toOpenArray(r.get[0], r.get[0] + r.get[1] - 1))
            if s.isSome: found.add Preview(offset: r.get[0], len: r.get[1], width: s.get[0], height: s.get[1])
  var seen: seq[(int, int)]
  for p in found:
    if (p.offset, p.len) notin seen:
      seen.add (p.offset, p.len)
      result.add p

proc embeddedPreview*(bytes: openArray[byte]): Option[Preview] =
  ## The largest embedded JPEG preview of a raw file (the last of equal sizes).
  for p in embeddedPreviews(bytes):
    if result.isNone or uint64(p.width) * uint64(p.height) >= uint64(result.get.width) * uint64(result.get.height):
      result = some(p)
