## Math functions whose Rust implementations differ from the C library's, so the port's numbers
## match the Rust build bit for bit. Rust's `f32::cbrt`/`f64::cbrt` come from its bundled `libm`
## (FreeBSD's `cbrtf`, CORE-MATH's correctly rounded `cbrt`), not from glibc; every other function
## the port uses (sin, exp, powf, hypot, atan2, …) is glibc's on both sides.

import std/math

proc cFma(x, y, z: float64): float64 {.importc: "fma", header: "<math.h>".}

proc cbrt*(x: float32): float32 =
  ## Rust `f32::cbrt` (FreeBSD `s_cbrtf.c`: two Newton steps in double precision).
  const
    B1 = 709958130'u32  # (127 - 127.0/3 - 0.03306235651) * 2^23
    B2 = 642849266'u32  # (127 - 127.0/3 - 24/3 - 0.03306235651) * 2^23
  var ui = cast[uint32](x)
  var hx = ui and 0x7fffffff'u32
  if hx >= 0x7f800000'u32: return x + x       # NaN, ±inf
  if hx < 0x00800000'u32:                     # zero or subnormal
    if hx == 0: return x
    ui = cast[uint32](x * cast[float32](0x4b800000'u32))
    hx = ui and 0x7fffffff'u32
    hx = hx div 3 + B2
  else:
    hx = hx div 3 + B1
  ui = ui and 0x80000000'u32
  ui = ui or hx
  let xd = float64(x)
  var t = float64(cast[float32](ui))
  var r = t * t * t
  t = t * (xd + xd + r) / (xd + r + r)
  r = t * t * t
  t = t * (xd + xd + r) / (xd + r + r)
  float32(t)

proc cbrt*(x: float64): float64 =
  ## Rust `f64::cbrt` (CORE-MATH, correctly rounded; round to nearest).
  const
    Escale = [1.0, cast[float64](0x3ff428a2f98d728b'u64), cast[float64](0x3ff965fea53d6e3d'u64)]   # 1, 2^(1/3), 2^(2/3)
    C = [cast[float64](0x3fe1b0babccfef9c'u64), cast[float64](0x3fe2c9a3e94d1da5'u64), -cast[float64](0x3fc4dc30b1a1ddba'u64), cast[float64](0x3f97a8d3e4ec9b07'u64)]
    U0 = cast[float64](0x3fd5555555555555'u64)
    U1 = cast[float64](0x3fcc71c71c71c71c'u64)
    Rsc = [1.0, -1.0, 0.5, -0.5, 0.25, -0.25]
    Off = cast[float64](0x3ca0000000000000'u64)
    SigMask = (1'u64 shl 52) - 1
    ExpSat = 0x7ff'u32
  let hx = cast[uint64](x)
  var mant = hx and SigMask
  let sign = hx shr 63
  var e = uint32(hx shr 52) and ExpSat
  if ((e + 1) and ExpSat) < 2:
    let ix = hx and not (1'u64 shl 63)
    if e == ExpSat or ix == 0: return x + x
    var nz = 0'u32
    var probe = ix
    while (probe and (1'u64 shl 63)) == 0:
      inc nz
      probe = probe shl 1
    nz -= 11
    mant = (mant shl nz) and SigMask
    e = e - (nz - 1)
  e = e + 3072
  let cvt1 = mant or (0x3ff'u64 shl 52)
  var cvt5 = cvt1
  let et = e div 3
  let it = e mod 3
  cvt5 += uint64(it) shl 52
  cvt5 = cvt5 or (sign shl 63)
  let zz = cast[float64](cvt5)
  var isc = cast[uint64](Escale[it])
  isc = isc or (sign shl 63)
  let z = cast[float64](cvt1)
  let r = 1.0 / z
  let rr = r * Rsc[(int(it) shl 1) or int(sign)]
  let z2 = z * z
  let c0 = C[0] + z * C[1]
  let c2 = C[2] + z * C[3]
  var y = c0 + z2 * c2
  var y2 = y * y
  var h = y2 * (y * r) - 1.0
  y -= (h * y) * (U0 - U1 * h)
  y *= cast[float64](isc)
  y2 = y * y
  var y2l = cFma(y, y, -y2)
  var y3 = y2 * y
  var y3l = cFma(y, y2, -y3) + y * y2l
  h = ((y3 - zz) + y3l) * rr
  var dy = h * (y * U0)
  var y1 = y - dy
  dy = (y - y1) - dy
  var ady = abs(dy)
  var ady0 = abs(ady - Off)
  var ady1 = abs(ady - (cast[float64](0x3cb0000000000000'u64) + Off))
  if ady0 < cast[float64](0x3b40000000000000'u64) or ady1 < cast[float64](0x3b40000000000000'u64):
    y2 = y1 * y1
    y2l = cFma(y1, y1, -y2)
    y3 = y2 * y1
    y3l = cFma(y1, y2, -y3) + y1 * y2l
    h = ((y3 - zz) + y3l) * rr
    dy = h * (y1 * U0)
    y = y1 - dy
    dy = (y1 - y) - dy
    y1 = y
    ady = abs(dy)
    ady0 = abs(ady - Off)
    ady1 = abs(ady - (cast[float64](0x3cb0000000000000'u64) + Off))
    if ady0 < cast[float64](0x39d0000000000000'u64) or ady1 < cast[float64](0x39d0000000000000'u64):
      let azz = abs(zz)
      if azz == cast[float64](0x4009b78223aa307c'u64): y1 = copySign(cast[float64](0x3ff79d15d0e8d59c'u64), zz)
      if azz == cast[float64](0x401a202bfc89ddff'u64): y1 = copySign(cast[float64](0x3ffde87aa837820f'u64), zz)
  var cvt3 = cast[uint64](y1)
  cvt3 = cvt3 + (uint64(et - 342'u32 - 1023'u32) shl 52)
  let m0 = cvt3 shl 30
  let m1 = m0 shr 63
  if (m0 xor m1) <= (1'u64 shl 30):
    var cvt4 = cast[uint64](y1)
    cvt4 = (cvt4 + (164'u64 shl 15)) and 0xffffffffffff0000'u64
    if abs((cast[float64](cvt4) - y1) - dy) < cast[float64](0x3c30000000000000'u64) or abs(zz) == 1.0:
      cvt3 = (cvt3 + (1'u64 shl 15)) and 0xffffffffffff0000'u64
  cast[float64](cvt3)

# ---------------------------------------------------------------- Rust `as` casts from f32

proc asIdx*(v: float32): int {.inline.} =
  ## Rust `v as usize` (64-bit, capped at `high(int)`): NaN and negatives → 0.
  if v != v or v <= 0: 0
  elif v >= 9.2e18'f32: high(int)
  else: int(v)

proc asU8*(v: float32): uint8 {.inline.} =
  ## Rust `v as u8`: saturating, NaN → 0.
  if v != v or v <= 0: 0'u8
  elif v >= 255: 255'u8
  else: uint8(v)

proc asU16*(v: float32): uint16 {.inline.} =
  ## Rust `v as u16`: saturating, NaN → 0.
  if v != v or v <= 0: 0'u16
  elif v >= 65535: 65535'u16
  else: uint16(v)

proc asU32*(v: float32): uint32 {.inline.} =
  ## Rust `v as u32`: saturating, NaN → 0.
  if v != v or v <= 0: 0'u32
  elif v >= 4294967295'f32: high(uint32)
  else: uint32(v)

const F32Max* = 3.4028234663852886e38'f32
  ## Rust's `f32::MAX` (Nim's `high(float32)` is +∞).

proc toDegrees*(v: float32): float32 {.inline.} =
  ## Rust's `f32::to_degrees` (one multiplication by the rounded constant).
  v * 57.2957795130823208767981548141051703'f32
proc toDegrees*(v: float64): float64 {.inline.} =
  ## Rust's `f64::to_degrees`: `v * (180 / π)`.
  v * (180.0 / PI)
proc toRadians*(v: float32): float32 {.inline.} =
  ## Rust's `f32::to_radians`: `v * (π / 180)` with the constant rounded once (= `degToRad`).
  v * 0.017453292519943295'f32
proc toRadians*(v: float64): float64 {.inline.} =
  ## Rust's `f64::to_radians`.
  v * (PI / 180.0)

# ---------------------------------------------------------------- Rust float semantics

proc rmax*[T: SomeFloat](a, b: T): T {.inline.} =
  ## Rust `f32::max`/`f64::max`: a NaN operand is ignored.
  if a != a: b elif b != b: a else: max(a, b)
proc rmin*[T: SomeFloat](a, b: T): T {.inline.} =
  ## Rust `f32::min`/`f64::min`: a NaN operand is ignored.
  if a != a: b elif b != b: a else: min(a, b)
proc remEuclidF*[T: SomeFloat](a, b: T): T {.inline.} =
  ## Rust `f32::rem_euclid`.
  let r = a mod b
  if r < 0: r + abs(b) else: r
proc signum*[T: SomeFloat](x: T): T {.inline.} =
  ## Rust `f32::signum`: ±1 by the sign bit (also for ±0), NaN for NaN.
  if x != x: x else: copySign(T(1), x)
