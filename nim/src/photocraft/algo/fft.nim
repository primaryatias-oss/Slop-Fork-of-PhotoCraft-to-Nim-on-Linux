## A std-only complex FFT standing in for the `rustfft` crate: mixed-radix Cooley-Tukey (radix
## 2/3/4/5 butterflies plus a generic butterfly for other small primes, after KissFFT's layout)
## and Bluestein's chirp-z algorithm for lengths with large prime factors. Both directions are
## unnormalised, as in rustfft (forward then inverse scales by `n`).

import std/[math, complex]
export complex

const BluesteinPrime = 13 ## largest prime factor handled by a direct butterfly

type
  FftPlan* = ref object
    ## A transform of one length and direction, reusable for any number of buffers.
    n*: int
    inverse*: bool
    factors: seq[int]          # (radix, remaining length) pairs, outermost first
    tw: seq[Complex64]         # exp(∓2πi k/n), k in 0..<n
    blue: bool
    m: int                     # Bluestein convolution length (a power of two ≥ 2n − 1)
    chirp: seq[Complex64]      # exp(∓πi k²/n), k in 0..<n
    bkernel: seq[Complex64]    # forward FFT of the conjugate chirp, scaled by 1/m
    fwd, inv: FftPlan          # length-m plans for Bluestein

proc factorize(n: int): seq[int] =
  ## KissFFT's factorisation: fours first, then twos, then odd factors.
  var n = n
  var p = 4
  let floorSqrt = int(floor(sqrt(float64(n))))
  while n > 1:
    while n mod p != 0:
      case p
      of 4: p = 2
      of 2: p = 3
      else: p += 2
      if p > floorSqrt: p = n
    n = n div p
    result.add p
    result.add n

proc isPow2(n: int): bool = n > 0 and (n and (n - 1)) == 0

proc process*(p: FftPlan; data: var openArray[Complex64]; scratch: var seq[Complex64])

proc newFftPlan*(n: int; inverse = false): FftPlan =
  ## Plans a length-`n` transform (`n` ≥ 1); raises ValueError for a non-positive length.
  if n <= 0: raise newException(ValueError, "fft: length must be positive, got " & $n)
  result = FftPlan(n: n, inverse: inverse)
  let sign = (if inverse: 1.0 else: -1.0)
  let factors = factorize(n)
  var largest = 1
  var i = 0
  while i < factors.len:
    largest = max(largest, factors[i])
    i += 2
  if largest > BluesteinPrime and not isPow2(n):
    result.blue = true
    var m = 1
    while m < 2 * n - 1: m *= 2
    result.m = m
    result.chirp = newSeq[Complex64](n)
    let twoN = 2 * n
    for k in 0 ..< n:
      # k² mod 2n keeps the phase accurate for long transforms.
      let q = int((int64(k) * int64(k)) mod int64(twoN))
      let a = sign * PI * float64(q) / float64(n)
      result.chirp[k] = complex64(cos(a), sin(a))
    result.fwd = newFftPlan(m, false)
    result.inv = newFftPlan(m, true)
    var b = newSeq[Complex64](m)
    for k in 0 ..< n:
      let c = conjugate(result.chirp[k])
      b[k] = c
      if k > 0: b[m - k] = c
    var scratch: seq[Complex64]
    result.fwd.process(b, scratch)
    let s = 1.0 / float64(m)
    for v in b.mitems: v = v * s
    result.bkernel = b
  else:
    result.factors = factors
    result.tw = newSeq[Complex64](n)
    for k in 0 ..< n:
      let a = sign * 2 * PI * float64(k) / float64(n)
      result.tw[k] = complex64(cos(a), sin(a))

proc scratchLen*(p: FftPlan): int =
  ## Scratch elements `process` uses (it grows the scratch buffer itself when needed).
  if p.blue: 2 * p.m else: p.n

# ---------------------------------------------------------------- butterflies (KissFFT layout)

proc bfly2(p: FftPlan; f: var openArray[Complex64]; o, fstride, m: int) =
  var tw = 0
  for k in 0 ..< m:
    let t = f[o + m + k] * p.tw[tw]
    tw += fstride
    f[o + m + k] = f[o + k] - t
    f[o + k] = f[o + k] + t

proc bfly3(p: FftPlan; f: var openArray[Complex64]; o, fstride, m: int) =
  let m2 = 2 * m
  let epi3 = p.tw[fstride * m]
  var tw1, tw2 = 0
  for k in 0 ..< m:
    let i = o + k
    let s1 = f[i + m] * p.tw[tw1]
    let s2 = f[i + m2] * p.tw[tw2]
    let s3 = s1 + s2
    var s0 = s1 - s2
    tw1 += fstride
    tw2 += 2 * fstride
    f[i + m] = complex64(f[i].re - 0.5 * s3.re, f[i].im - 0.5 * s3.im)
    s0 = complex64(s0.re * epi3.im, s0.im * epi3.im)
    f[i] = f[i] + s3
    f[i + m2] = complex64(f[i + m].re + s0.im, f[i + m].im - s0.re)
    f[i + m] = complex64(f[i + m].re - s0.im, f[i + m].im + s0.re)

proc bfly4(p: FftPlan; f: var openArray[Complex64]; o, fstride, m: int) =
  let (m2, m3) = (2 * m, 3 * m)
  var tw1, tw2, tw3 = 0
  for k in 0 ..< m:
    let i = o + k
    let s0 = f[i + m] * p.tw[tw1]
    let s1 = f[i + m2] * p.tw[tw2]
    let s2 = f[i + m3] * p.tw[tw3]
    let s5 = f[i] - s1
    f[i] = f[i] + s1
    let s3 = s0 + s2
    let s4 = s0 - s2
    f[i + m2] = f[i] - s3
    tw1 += fstride
    tw2 += 2 * fstride
    tw3 += 3 * fstride
    f[i] = f[i] + s3
    if p.inverse:
      f[i + m] = complex64(s5.re - s4.im, s5.im + s4.re)
      f[i + m3] = complex64(s5.re + s4.im, s5.im - s4.re)
    else:
      f[i + m] = complex64(s5.re + s4.im, s5.im - s4.re)
      f[i + m3] = complex64(s5.re - s4.im, s5.im + s4.re)

proc bfly5(p: FftPlan; f: var openArray[Complex64]; o, fstride, m: int) =
  let ya = p.tw[fstride * m]
  let yb = p.tw[fstride * 2 * m]
  for u in 0 ..< m:
    let (i0, i1, i2, i3, i4) = (o + u, o + u + m, o + u + 2 * m, o + u + 3 * m, o + u + 4 * m)
    let s0 = f[i0]
    let s1 = f[i1] * p.tw[u * fstride]
    let s2 = f[i2] * p.tw[2 * u * fstride]
    let s3 = f[i3] * p.tw[3 * u * fstride]
    let s4 = f[i4] * p.tw[4 * u * fstride]
    let s7 = s1 + s4
    let s10 = s1 - s4
    let s8 = s2 + s3
    let s9 = s2 - s3
    f[i0] = complex64(f[i0].re + s7.re + s8.re, f[i0].im + s7.im + s8.im)
    let s5 = complex64(s0.re + s7.re * ya.re + s8.re * yb.re, s0.im + s7.im * ya.re + s8.im * yb.re)
    let s6 = complex64(s10.im * ya.im + s9.im * yb.im, -(s10.re * ya.im) - s9.re * yb.im)
    f[i1] = s5 - s6
    f[i4] = s5 + s6
    let s11 = complex64(s0.re + s7.re * yb.re + s8.re * ya.re, s0.im + s7.im * yb.re + s8.im * ya.re)
    let s12 = complex64(-(s10.im * yb.im) + s9.im * ya.im, s10.re * yb.im - s9.re * ya.im)
    f[i2] = s11 + s12
    f[i3] = s11 - s12

proc bflyGeneric(p: FftPlan; f: var openArray[Complex64]; o, fstride, m, radix: int) =
  var scratch = newSeq[Complex64](radix)
  let n = p.n
  for u in 0 ..< m:
    var k = u
    for q1 in 0 ..< radix:
      scratch[q1] = f[o + k]
      k += m
    k = u
    for q1 in 0 ..< radix:
      var twidx = 0
      var acc = scratch[0]
      for q in 1 ..< radix:
        twidx += fstride * k
        if twidx >= n: twidx -= n
        acc = acc + scratch[q] * p.tw[twidx]
      f[o + k] = acc
      k += m

proc work(p: FftPlan; fout: var openArray[Complex64]; o: int; fin: openArray[Complex64]; fi, fstride, fac: int) =
  let radix = p.factors[fac]
  let m = p.factors[fac + 1]
  if m == 1:
    var s = fi
    for i in o ..< o + radix:
      fout[i] = fin[s]
      s += fstride
  else:
    var s = fi
    var q = o
    for _ in 0 ..< radix:
      p.work(fout, q, fin, s, fstride * radix, fac + 2)
      s += fstride
      q += m
  case radix
  of 2: p.bfly2(fout, o, fstride, m)
  of 3: p.bfly3(fout, o, fstride, m)
  of 4: p.bfly4(fout, o, fstride, m)
  of 5: p.bfly5(fout, o, fstride, m)
  else: p.bflyGeneric(fout, o, fstride, m, radix)

proc process*(p: FftPlan; data: var openArray[Complex64]; scratch: var seq[Complex64]) =
  ## Transforms `data` (exactly `p.n` elements) in place; `scratch` is a reusable buffer.
  if data.len != p.n:
    raise newException(ValueError, "fft: buffer of " & $data.len & " elements for a length-" & $p.n & " plan")
  if p.n == 1: return
  if p.blue:
    let (n, m) = (p.n, p.m)
    var buf = newSeq[Complex64](m)
    for k in 0 ..< n: buf[k] = data[k] * p.chirp[k]
    p.fwd.process(buf, scratch)
    for k in 0 ..< m: buf[k] = buf[k] * p.bkernel[k]
    p.inv.process(buf, scratch)
    for k in 0 ..< n: data[k] = buf[k] * p.chirp[k]
    return
  scratch.setLen(p.n)
  for i in 0 ..< p.n: scratch[i] = data[i]
  p.work(data, 0, scratch, 0, 1, 0)

proc process*(p: FftPlan; data: var seq[Complex64]) =
  ## `process` with a temporary scratch buffer.
  var scratch: seq[Complex64]
  p.process(data, scratch)

proc fft*(data: openArray[Complex64]; inverse = false): seq[Complex64] =
  ## One-shot unnormalised transform of `data` (empty input gives an empty result).
  result = @data
  if result.len == 0: return
  newFftPlan(result.len, inverse).process(result)
