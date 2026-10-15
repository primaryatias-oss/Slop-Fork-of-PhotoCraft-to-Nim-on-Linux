## Sample-level helpers for moving pixels between PSD planes (big-endian, per channel) and
## surfaces (host-endian, interleaved): depth mapping, (de)interleaving with inversion, Lab
## 16-bit chroma scaling, matting.

import std/math
import ../color, ../rmath

proc psdDepth*(s: SampleType): int =
  case s
  of stU8: 8
  of stU16: 16
  of stF32: 32

proc sampleForDepth*(depth: int): SampleType =
  case depth
  of 16: stU16
  of 32: stF32
  else: stU8

proc invertSample(b: var openArray[byte]; off: int; s: SampleType) =
  case s
  of stU8: b[off] = 255'u8 - b[off]
  of stU16:
    var v: uint16
    copyMem(addr v, addr b[off], 2)
    v = 65535'u16 - v
    copyMem(addr b[off], addr v, 2)
  of stF32:
    var v: float32
    copyMem(addr v, addr b[off], 4)
    v = 1 - v
    copyMem(addr b[off], addr v, 4)

proc interleave*(planes: openArray[seq[byte]]; present: openArray[bool]; fill: openArray[seq[byte]]; n: int;
                 s: SampleType; invert: openArray[bool]): seq[byte] =
  ## Big-endian planes → host-endian interleaved pixels; a missing (or short) plane uses `fill`.
  let bps = s.bytes
  let ch = planes.len
  result = newSeq[byte](n * ch * bps)
  for p in 0 ..< n:
    for c in 0 ..< ch:
      let dst = (p * ch + c) * bps
      if present[c] and planes[c].len >= (p + 1) * bps:
        let src = p * bps
        for k in 0 ..< bps: result[dst + k] = planes[c][src + bps - 1 - k]
        if invert[c]: invertSample(result, dst, s)
      else:
        for k in 0 ..< bps: result[dst + k] = fill[c][k]

proc deinterleave*(bytes: openArray[byte]; ch: int; s: SampleType; invert: openArray[bool]): seq[seq[byte]] =
  ## Host-endian interleaved pixels → big-endian planes.
  let bps = s.bytes
  let n = bytes.len div max(ch * bps, 1)
  result = newSeq[seq[byte]](ch)
  var tmp: array[4, byte]
  for c in 0 ..< ch:
    result[c] = newSeq[byte](n * bps)
    for p in 0 ..< n:
      let src = (p * ch + c) * bps
      for k in 0 ..< bps: tmp[k] = bytes[src + k]
      if invert[c]: invertSample(tmp, 0, s)
      for k in 0 ..< bps: result[c][p * bps + k] = tmp[bps - 1 - k]

proc maxSample*(s: SampleType): seq[byte] =
  case s
  of stU8: result = @[255'u8]
  of stU16: result = @[255'u8, 255]
  of stF32:
    var v = 1'f32
    result = newSeq[byte](4)
    copyMem(addr result[0], addr v, 4)

proc zeroSample*(s: SampleType): seq[byte] = newSeq[byte](s.bytes)

const Lab16ChromaMax* = 65280'f32

proc lab16Chroma*(bytes: var openArray[byte]; ch: int; toDoc: bool) =
  ## PSD stores 16-bit Lab a/b with 0x8000 = neutral on a 0..65280 scale; documents use 0..65535.
  if ch < 3: return
  let k = (if toDoc: 65535.0 / float64(Lab16ChromaMax) else: float64(Lab16ChromaMax) / 65535.0)
  var i = 0
  while i + ch * 2 <= bytes.len:
    for c in 1 .. 2:
      var v: uint16
      copyMem(addr v, addr bytes[i + 2 * c], 2)
      var w = uint16(min(round(float64(v) * k), 65535.0))
      copyMem(addr bytes[i + 2 * c], addr w, 2)
    i += ch * 2

proc encodeBe*(v: float32; s: SampleType; outp: var seq[byte]) =
  case s
  of stU8: outp.add byte(clamp(v, 0'f32, 1'f32) * 255 + 0.5'f32)
  of stU16:
    let x = asU16(clamp(v, 0'f32, 1'f32) * 65535 + 0.5'f32)
    outp.add byte(x shr 8)
    outp.add byte(x and 0xff)
  of stF32:
    let x = cast[uint32](v)
    for k in countdown(3, 0): outp.add byte((x shr (8 * k)) and 0xff)

proc decodeBe*(bytes: openArray[byte]; index: int; s: SampleType): float32 =
  case s
  of stU8: float32(bytes[index]) / 255
  of stU16: float32((uint16(bytes[2 * index]) shl 8) or uint16(bytes[2 * index + 1])) / 65535
  of stF32:
    let o = 4 * index
    cast[float32]((uint32(bytes[o]) shl 24) or (uint32(bytes[o + 1]) shl 16) or (uint32(bytes[o + 2]) shl 8) or uint32(bytes[o + 3]))

proc matte*(c, a, white: float32): float32 = c * a + white * (1 - a)
proc unmatte*(m, a, white: float32): float32 = (if a <= 0: white else: (m - white * (1 - a)) / a)
