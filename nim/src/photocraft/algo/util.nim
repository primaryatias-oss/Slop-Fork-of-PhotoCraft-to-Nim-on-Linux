## Shared numeric helpers for the filters: dense and sparse least squares, a splitmix RNG,
## coordinate hashing, sRGB transfer, bilinear sampling and Catmull-Rom weights. (The Rust
## crate's rayon helpers become plain loops: the Nim port runs single-threaded.)

import std/math
import ../rmath
export rmath

proc isFinite*(v: SomeFloat): bool {.inline.} = v.classify notin {fcInf, fcNegInf, fcNan}

proc totalCmp*(a, b: float64): int

proc solveDense*(n: int; a0, b0: seq[float64]): seq[float64] =
  ## Gaussian elimination with partial pivoting; empty when singular or non-finite.
  var a = a0
  var b = b0
  if a.len < n * n or b.len < n: return @[]
  for col in 0 ..< n:
    # Rust's `max_by` over `total_cmp`: the last of equal maxima (NaN ranks highest).
    var piv = col
    for i in col + 1 ..< n:
      if totalCmp(abs(a[i * n + col]), abs(a[piv * n + col])) >= 0: piv = i
    if abs(a[piv * n + col]) < 1e-14: return @[]
    if piv != col:
      for k in 0 ..< n: swap(a[col * n + k], a[piv * n + k])
      swap(b[col], b[piv])
    let d = a[col * n + col]
    for r in col + 1 ..< n:
      let f = a[r * n + col] / d
      if f == 0: continue
      for k in col ..< n: a[r * n + k] -= f * a[col * n + k]
      b[r] -= f * b[col]
  var x = newSeq[float64](n)
  for r in countdown(n - 1, 0):
    var s = 0.0
    for k in r + 1 ..< n: s += a[r * n + k] * x[k]
    x[r] = (b[r] - s) / a[r * n + r]
  for v in x:
    if not v.isFinite: return @[]
  x

type
  Normal* = object
    ## Normal equations AᵀA x = Aᵀb accumulated row by row.
    n*: int
    ata*, atb*: seq[float64]
  SparseLs* = object
    ## Sparse least squares solved by CGLS.
    n*: int
    rows: seq[(seq[(int, float64)], float64)]
  Rng* = object
    state*: uint64

proc newNormal*(n: int): Normal = Normal(n: n, ata: newSeq[float64](n * n), atb: newSeq[float64](n))

proc add*(m: var Normal; row: openArray[(int, float64)]; rhs, w: float64) =
  let w2 = w * w
  for (i, a) in row:
    m.atb[i] += w2 * a * rhs
    for (j, b) in row: m.ata[i * m.n + j] += w2 * a * b

proc solve*(m: Normal): seq[float64] = solveDense(m.n, m.ata, m.atb)

proc newSparseLs*(n: int): SparseLs = SparseLs(n: n)

proc add*(m: var SparseLs; row: seq[(int, float64)]; rhs, w: float64) =
  var r = newSeqOfCap[(int, float64)](row.len)
  for (i, c) in row: r.add (i, c * w)
  m.rows.add (r, rhs * w)

proc solve*(m: SparseLs; x0: openArray[float64]; iters: int): seq[float64] =
  let n = m.n
  var x = @x0
  proc ax(m: SparseLs; x: seq[float64]): seq[float64] =
    result = newSeq[float64](m.rows.len)
    for k, (r, _) in m.rows:
      var s = 0.0
      for (i, c) in r: s += c * x[i]
      result[k] = s
  proc atv(m: SparseLs; v: seq[float64]): seq[float64] =
    result = newSeq[float64](m.n)
    for k, (r, _) in m.rows:
      for (i, c) in r: result[i] += c * v[k]
  let axx = ax(m, x)
  var r = newSeq[float64](m.rows.len)
  for k in 0 ..< m.rows.len: r[k] = m.rows[k][1] - axx[k]
  var s = atv(m, r)
  var p = s
  var gamma = 0.0
  for v in s: gamma += v * v
  let g0 = max(gamma, 1e-30)
  for _ in 0 ..< iters:
    if gamma <= g0 * 1e-20: break
    let q = ax(m, p)
    var qq = 0.0
    for v in q: qq += v * v
    if qq <= 1e-300: break
    let alpha = gamma / qq
    for i in 0 ..< n: x[i] += alpha * p[i]
    for k in 0 ..< r.len: r[k] -= alpha * q[k]
    s = atv(m, r)
    var g1 = 0.0
    for v in s: g1 += v * v
    let beta = g1 / gamma
    gamma = g1
    for i in 0 ..< n: p[i] = s[i] + beta * p[i]
  x

proc newRng*(seed: uint64): Rng = Rng(state: seed)

proc next*(r: var Rng): uint64 =
  r.state = r.state + 0x9E37_79B9_7F4A_7C15'u64
  var z = r.state
  z = (z xor (z shr 30)) * 0xBF58_476D_1CE4_E5B9'u64
  z = (z xor (z shr 27)) * 0x94D0_49BB_1331_11EB'u64
  z xor (z shr 31)

proc below*(r: var Rng; n: int): int = int(r.next mod uint64(max(n, 1)))

proc hash01*(x, y: int64; seed: uint64): float32 =
  ## A uniform [0, 1) value for a coordinate pair (photo tools' hash).
  var z = (cast[uint64](x) * 0x9E37_79B9_7F4A_7C15'u64) xor (cast[uint64](y) * 0xC2B2_AE3D_27D4_EB4F'u64) xor
          (seed * 0x1656_67B1_9E37_79F9'u64)
  z = (z xor (z shr 30)) * 0xBF58_476D_1CE4_E5B9'u64
  z = (z xor (z shr 27)) * 0x94D0_49BB_1331_11EB'u64
  z = z xor (z shr 31)
  float32(z shr 40) / float32(1'u64 shl 24)

proc srgbToLinear*(v: float32): float32 =
  if v <= 0.04045'f32: v / 12.92'f32 else: pow((v + 0.055'f32) / 1.055'f32, 2.4'f32)

proc linearToSrgb*(v: float32): float32 =
  if v <= 0.003_130_8'f32: v * 12.92'f32 else: 1.055'f32 * pow(v, 1 / 2.4'f32) - 0.055'f32

proc bilinear*(buf: openArray[float32]; w, h, ch, c: int; x0, y0: float32): float32 =
  if w <= 0 or h <= 0: return 0
  let x = clamp(x0, 0'f32, float32(w - 1))
  let y = clamp(y0, 0'f32, float32(h - 1))
  let (xi, yi) = (int(floor(x)), int(floor(y)))
  let (x1, y1) = (min(xi + 1, w - 1), min(yi + 1, h - 1))
  let (tx, ty) = (x - float32(xi), y - float32(yi))
  template p(xx, yy: int): float32 = buf[(yy * w + xx) * ch + c]
  let top = p(xi, yi) + (p(x1, yi) - p(xi, yi)) * tx
  let bot = p(xi, y1) + (p(x1, y1) - p(xi, y1)) * tx
  top + (bot - top) * ty

proc catmullRom*(t: float32): array[4, float32] =
  let t2 = t * t
  let t3 = t2 * t
  [-0.5'f32 * t3 + t2 - 0.5'f32 * t, 1.5'f32 * t3 - 2.5'f32 * t2 + 1, -1.5'f32 * t3 + 2 * t2 + 0.5'f32 * t, 0.5'f32 * t3 - 0.5'f32 * t2]

proc totalCmp*(a, b: float32): int =
  ## Rust's `f32::total_cmp` (NaNs ordered after +inf, -0 before +0).
  var x = cast[int32](a)
  var y = cast[int32](b)
  x = x xor int32(cast[uint32](x shr 31) shr 1)
  y = y xor int32(cast[uint32](y shr 31) shr 1)
  cmp(x, y)

proc totalCmp*(a, b: float64): int =
  ## Rust's `f64::total_cmp`.
  var x = cast[int64](a)
  var y = cast[int64](b)
  x = x xor int64(cast[uint64](x shr 63) shr 1)
  y = y xor int64(cast[uint64](y shr 63) shr 1)
  cmp(x, y)

# ---------------------------------------------------------------- Rust float semantics

proc powi*[T: SomeFloat](x: T; n: int): T =
  ## Rust's run-time `powi` as LLVM expands it (binary powering, `Res * CurSquare` order).
  var val = abs(n)
  var res: T
  var have = false
  var cur = x
  while val != 0:
    if (val and 1) != 0:
      if have: res = res * cur
      else:
        res = cur
        have = true
    cur = cur * cur
    val = val shr 1
  if not have: res = T(1)
  if n < 0: T(1) / res else: res
proc newSeqWithValue*[T](n: int; v: T): seq[T] =
  ## `vec![v; n]`.
  result = newSeq[T](max(n, 0))
  for x in result.mitems: x = v
