## 3D lookup tables for applying a display transform (and half-float helpers).

import std/math
import ./pipeline, ./transform, ./profile

type
  Lut3d* = object
    ## An `size³` RGBA table: entry `r + g·size + b·size²` holds the output for `(r, g, b)/(size−1)`.
    size*: int
    data*: seq[array[4, float32]]

proc lut3dFromTransform*(t: Transform; size: int): Lut3d =
  ## Samples a 3-input transform (1-output transforms are replicated to gray).
  if t.inputs != 3 or (t.outputs != 3 and t.outputs != 1):
    raise newCmsError(ceInvalid, "display LUTs need a 3→3 or 3→1 transform")
  if size < 2: raise newCmsError(ceInvalid, "LUT size")
  result.size = size
  result.data = newSeq[array[4, float32]](size * size * size)
  let s = float32(size - 1)
  var o: PipeVec
  for b in 0 ..< size:
    for g in 0 ..< size:
      for r in 0 ..< size:
        t.eval([float32(r) / s, float32(g) / s, float32(b) / s], o)
        let v = if t.outputs == 1: [o[0], o[0], o[0]] else: [o[0], o[1], o[2]]
        result.data[r + g * size + b * size * size] = [v[0], v[1], v[2], 1.0'f32]

proc sample*(l: Lut3d; rgb: array[3, float32]): array[3, float32] =
  ## Trilinear lookup.
  let n = l.size
  proc loc(v: float32): (int, float32) =
    let p = clamp(v, 0.0'f32, 1.0'f32) * float32(n - 1)
    let i = min(int(p), n - 2)
    (i, p - float32(i))
  let (r0, fr) = loc(rgb[0])
  let (g0, fg) = loc(rgb[1])
  let (b0, fb) = loc(rgb[2])
  for k in 0..2:
    proc c(r, g, b: int): float32 = l.data[r + g * n + b * n * n][k]
    let x00 = c(r0, g0, b0) + (c(r0 + 1, g0, b0) - c(r0, g0, b0)) * fr
    let x10 = c(r0, g0 + 1, b0) + (c(r0 + 1, g0 + 1, b0) - c(r0, g0 + 1, b0)) * fr
    let x01 = c(r0, g0, b0 + 1) + (c(r0 + 1, g0, b0 + 1) - c(r0, g0, b0 + 1)) * fr
    let x11 = c(r0, g0 + 1, b0 + 1) + (c(r0 + 1, g0 + 1, b0 + 1) - c(r0, g0 + 1, b0 + 1)) * fr
    let y0 = x00 + (x10 - x00) * fg
    let y1 = x01 + (x11 - x01) * fg
    result[k] = y0 + (y1 - y0) * fb

proc f32ToF16*(v: float32): uint16 =
  ## IEEE 754 binary16 bits of `v` (round to nearest even; overflow → ±inf, NaN kept).
  let x = cast[uint32](v)
  let sign = uint16((x shr 16) and 0x8000)
  let exp = int((x shr 23) and 0xFF)
  let man = x and 0x7F_FFFF
  if exp == 0xFF: return sign or 0x7C00 or (if man != 0: 0x200'u16 else: 0'u16)
  let e = exp - 127 + 15
  if e >= 0x1F: return sign or 0x7C00
  if e <= 0:
    if e < -10: return sign
    let m = man or 0x80_0000
    let shift = uint32(14 - e)
    let half = 1'u32 shl (shift - 1)
    let rem = m and ((1'u32 shl shift) - 1)
    var r = m shr shift
    if rem > half or (rem == half and (r and 1) == 1): inc r
    return sign or uint16(r)
  var r = (uint32(e) shl 10) or (man shr 13)
  let rem = man and 0x1FFF
  if rem > 0x1000 or (rem == 0x1000 and (r and 1) == 1): inc r
  sign or uint16(r)

proc f16ToF32*(h: uint16): float32 =
  ## binary16 bits → f32.
  let sign = if (h and 0x8000) != 0: -1.0'f32 else: 1.0'f32
  let e = int((h shr 10) and 0x1F)
  let m = float32(h and 0x3FF)
  case e
  of 0: sign * m * pow(2.0'f32, -24.0'f32)
  of 0x1F: (if m == 0.0: sign * Inf.float32 else: NaN.float32)
  else: sign * (1.0'f32 + m / 1024.0'f32) * pow(2.0'f32, float32(e - 15))

proc toRgba8*(l: Lut3d): seq[uint8] =
  for p in l.data:
    for v in p: result.add uint8(clamp(v, 0.0'f32, 1.0'f32) * 255.0'f32 + 0.5'f32)
