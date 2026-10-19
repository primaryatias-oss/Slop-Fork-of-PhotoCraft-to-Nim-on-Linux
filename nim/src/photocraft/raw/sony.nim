## Sony compressed ARW ("cRAW", ARW 2) (port of `raw/src/sony.rs`): a fixed-rate lossy code of
## one byte per pixel.
##
## * Each row is cut into runs of 32 pixels: two interleaved runs of 16 same-colour pixels, the run
##   of even columns stored first, then the run of odd columns, 16 bytes each.
## * A 16-byte block, read as a little-endian 128-bit number from the least significant bit, holds
##   an 11-bit maximum, an 11-bit minimum, the 4-bit positions of the maximum and of the minimum,
##   and fourteen 7-bit deltas for the other positions in order.
## * The step of the deltas is the smallest power of two `s` with `128 * s > max - min`.
## * The 11-bit codes go through a five-segment linear tone curve whose step doubles at each knot;
##   the knots are the four values of the SonyToneCurve tag (0x7010); the output is 14-bit.

import std/options
import ./tiff, ./sensor

const
  SonyRawFileType* = 0x7000'u16 ## SonyRawFileType (ExifTool): 2 = compressed RAW
  SonyToneCurve* = 0x7010'u16   ## the four knots of the cRAW tone curve
  Max14 = 16383'u32

proc toneLut*(knots: openArray[uint32]): Option[seq[uint16]] =
  ## The 2048-entry code → 14-bit value table of a tone curve given by its four knots, or none when
  ## the knots are not increasing or out of range.
  if knots.len != 4: return
  let pts = [0'u32, knots[0], knots[1], knots[2], knots[3], 1'u32 shl 14]
  for i in 1 ..< pts.len:
    if pts[i - 1] > pts[i]: return
  var lut = newSeq[uint16](2048)
  for code in 0'u32 ..< 2048'u32:
    # On the knots' scale a code is 8 × code; the first segment maps it to 2 × code.
    let x = code * 8
    var y = 0'u32
    for seg in 0 ..< pts.len - 1:
      if x > pts[seg]: y += (min(x, pts[seg + 1]) - pts[seg]) shl seg
    lut[code] = uint16(min(y div 4, Max14))
  some(lut)

proc field(b: openArray[byte]; at, bits: int): uint16 {.inline.} =
  var v = 0'u32
  for i in 0 ..< bits:
    let p = at + i
    v = v or (((uint32(b[p shr 3]) shr (p and 7)) and 1) shl i)
  uint16(v)

proc decodeBlock*(b: openArray[byte]): array[16, uint16] =
  ## Decodes one 16-byte block into 16 codes.
  let mx = field(b, 0, 11)
  let mn = field(b, 11, 11)
  let imax = int(field(b, 22, 4))
  let imin = int(field(b, 26, 4))
  let range = if mx > mn: mx - mn else: 0'u16
  var sh = 0
  while (128'u16 shl sh) <= range: inc sh
  var at = 30
  for i in 0 ..< 16:
    result[i] =
      if i == imax: mx
      elif i == imin: mn
      elif at + 7 <= 128:
        let d = field(b, at, 7)
        at += 7
        min(mn + (d shl sh), 2047'u16)
      else: mn # only when a damaged block names the same position twice

proc isCraw*(t: Tiff; ifd: Ifd): bool =
  ## Whether the raw IFD holds cRAW data: Sony's private compression with one byte per pixel.
  if t.tagUint(ifd, TagCompression) != some(32767'u32): return false
  let (w, h) = (t.tagUint(ifd, TagImageWidth).get(0), t.tagUint(ifd, TagImageLength).get(0))
  var bytes = 0'u64
  for c in t.tagUints(ifd, TagStripByteCounts): bytes += uint64(c)
  let kind = t.tagUint(ifd, SonyRawFileType)
  if kind.isSome: kind.get == 2 else: bytes > 0 and bytes == uint64(w) * uint64(h)

proc readCraw*(t: Tiff; ifd: Ifd; limits: RawLimits): Plane =
  ## Reads the cRAW image of a raw IFD.
  let w0 = t.tagUint(ifd, TagImageWidth)
  if w0.isNone: raise malformed("raw image has no width")
  let h0 = t.tagUint(ifd, TagImageLength)
  if h0.isNone: raise malformed("raw image has no height")
  let (width, height) = (int(w0.get), int(h0.get))
  if width mod 32 != 0: raise unsupported("Sony compressed ARW " & $width & " pixels wide (not a multiple of 32)")
  let lut = toneLut(t.tagUints(ifd, SonyToneCurve))
  if lut.isNone: raise unsupported("Sony compressed ARW without a usable tone curve (0x7010)")
  limits.check(uint64(width), uint64(height), 2)
  # One byte per pixel: locate every row in the strips.
  var rows = newSeqOfCap[int](height)
  for s in segments(t, ifd, width, height):
    let n = min(s.h, max(height - s.y, 0))
    for r in 0 ..< n:
      let at = s.offset + r * width
      if (r + 1) * width > s.len: raise malformed("compressed ARW strip is shorter than its rows")
      if not t.inRange(at, width): raise malformed("compressed ARW data lies outside the file")
      rows.add at
  if rows.len != height: raise malformed("compressed ARW strips do not cover the image")
  var data = newSeq[uint16](width * height)
  for r in 0 ..< height:
    let src = rows[r]
    for g in 0 ..< width div 32:
      let e = decodeBlock(t.data.toOpenArray(src + 32 * g, src + 32 * g + 15))
      let o = decodeBlock(t.data.toOpenArray(src + 32 * g + 16, src + 32 * g + 31))
      let base = r * width + 32 * g
      for k in 0 ..< 16:
        data[base + 2 * k] = lut.get[int(e[k])]
        data[base + 2 * k + 1] = lut.get[int(o[k])]
  Plane(width: width, height: height, samples: 1, bits: 14, data: data)
