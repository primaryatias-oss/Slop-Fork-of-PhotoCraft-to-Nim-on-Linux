## Image stack statistics (Layer › Smart Objects › Stack Mode): a per-pixel, per-channel statistic
## across N aligned images, e.g. median to remove passers-by or mean to reduce noise (port of
## `stack.rs`).
##
## Inputs are straight RGBA buffers of equal size. Only samples with alpha > 0 contribute; the
## output alpha is the largest input alpha. Moments use the population definitions; skewness and
## kurtosis are standardised (kurtosis is excess-free, a normal sample gives ≈ 3), scaled into
## 0..=1 for display; entropy is the Shannon entropy (bits) of the 8-bit values over log2(N).

import std/[math, algorithm, tables]
import ../rmath, ./util

type
  Stat* = enum
    stEntropy = "Entropy", stKurtosis = "Kurtosis", stMaximum = "Maximum", stMean = "Mean", stMedian = "Median",
    stMinimum = "Minimum", stRange = "Range", stSkewness = "Skewness", stStandardDeviation = "StandardDeviation",
    stSummation = "Summation", stVariance = "Variance"

proc stat(v: var seq[float32]; s: Stat): float32 =
  let n = float32(v.len)
  if v.len == 0: return 0
  var sum = -0'f32
  for x in v: sum += x
  let mean = sum / n
  template m(k: int): float32 =
    (var t = -0'f32; (for x in v: t += powi(x - mean, k)); t / n)
  template vmin(): float32 =
    (var r = F32Max; (for x in v: r = rmin(r, x)); r)
  template vmax(): float32 =
    (var r = -F32Max; (for x in v: r = rmax(r, x)); r)
  case s
  of stMean: mean
  of stSummation: sum
  of stMinimum: vmin()
  of stMaximum: vmax()
  of stRange: vmax() - vmin()
  of stMedian:
    v.sort(proc(a, b: float32): int = totalCmp(a, b))
    let k = v.len
    if k mod 2 == 1: v[k div 2] else: (v[k div 2 - 1] + v[k div 2]) / 2'f32
  of stVariance: m(2)
  of stStandardDeviation: sqrt(m(2))
  of stSkewness:
    let sd = sqrt(m(2))
    # Map −2..2 to 0..1 (0.5 = symmetric).
    if sd < 1e-6'f32: 0.5'f32 else: clamp(m(3) / powi(sd, 3) / 4'f32 + 0.5'f32, 0'f32, 1'f32)
  of stKurtosis:
    let variance = m(2)
    # 0..10 → 0..1.
    if variance < 1e-12'f32: 0'f32 else: clamp(m(4) / (variance * variance) / 10'f32, 0'f32, 1'f32)
  of stEntropy:
    if v.len < 2: return 0
    var counts = initOrderedTable[uint8, uint32]()
    for x in v: counts.mgetOrPut(asU8(round(clamp(x, 0'f32, 1'f32) * 255'f32)), 0) += 1
    # Rust sums its HashMap in an arbitrary (per-process) order; this sums by sample value, so
    # the last bit can differ from a given Rust run.
    counts.sort(proc(a, b: (uint8, uint32)): int = cmp(a[0], b[0]))
    var h = -0'f32
    for c in counts.values:
      let p = float32(c) / n
      h += -p * log2(p)
    h / log2(n)

proc combine*(layers: openArray[seq[array[4, float32]]]; s: Stat): seq[array[4, float32]] =
  ## Combines `layers` (each `len` pixels) with statistic `s`. Colour results retain their float
  ## range; quantisation/clipping belongs to the destination surface, not the statistic.
  var len = 0
  for l in layers: len = max(len, l.len)
  result = newSeq[array[4, float32]](len)
  var samples: array[3, seq[float32]]
  for i in 0 ..< len:
    for v in samples.mitems: v.setLen(0)
    var alpha = 0'f32
    for l in layers:
      if i < l.len and l[i][3] > 0:
        for c in 0 .. 2: samples[c].add l[i][c]
        alpha = rmax(alpha, l[i][3])
    if alpha <= 0: continue
    for c in 0 .. 2: result[i][c] = stat(samples[c], s)
    result[i][3] = alpha
