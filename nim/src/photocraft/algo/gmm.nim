## Gaussian mixture colour models with full 3×3 covariances (port of `segment/gmm.rs`).
##
## Fitting: k-means++ seeding (Arthur & Vassilvitskii, SODA 2007), Lloyd iterations, then a few
## EM steps (Dempster, Laird & Rubin 1977). GrabCut's per-iteration update (assign every sample to
## its most likely component, re-estimate) is `refit`. Covariances get a small ridge so flat colour
## areas stay well-conditioned.

import std/[math, options]
import ./util, ./segbase

const DefaultReg* = 2e-5'f32 ## default covariance ridge: about one 8-bit level of standard deviation

type
  Component* = object
    ## One Gaussian component.
    weight*: float32
    mean*: array[3, float32]
    cov*: array[3, array[3, float32]]
    inv: array[3, array[3, float64]]
    logNorm: float64 ## `ln(weight) − ½ ln det Σ − 3/2 ln 2π`

  Gmm* = object
    ## A Gaussian mixture over RGB (`0..=1`).
    comps*: seq[Component]
    reg*: float32 ## ridge added to covariance diagonals

  Acc = object
    n: float64
    s: array[3, float64]
    ss: array[3, array[3, float64]]

proc newComponent(weight: float64; mean: array[3, float64]; cov: array[3, array[3, float64]]; reg: float64): Option[Component] =
  var c = cov
  for i in 0 .. 2: c[i][i] += reg
  let det = c[0][0] * (c[1][1] * c[2][2] - c[1][2] * c[2][1]) - c[0][1] * (c[1][0] * c[2][2] - c[1][2] * c[2][0]) +
            c[0][2] * (c[1][0] * c[2][1] - c[1][1] * c[2][0])
  if det <= 1e-30 or weight <= 0.0: return none(Component)
  let inv = [
    [(c[1][1] * c[2][2] - c[1][2] * c[2][1]) / det, (c[0][2] * c[2][1] - c[0][1] * c[2][2]) / det, (c[0][1] * c[1][2] - c[0][2] * c[1][1]) / det],
    [(c[1][2] * c[2][0] - c[1][0] * c[2][2]) / det, (c[0][0] * c[2][2] - c[0][2] * c[2][0]) / det, (c[0][2] * c[1][0] - c[0][0] * c[1][2]) / det],
    [(c[1][0] * c[2][1] - c[1][1] * c[2][0]) / det, (c[0][1] * c[2][0] - c[0][0] * c[2][1]) / det, (c[0][0] * c[1][1] - c[0][1] * c[1][0]) / det]]
  let logNorm = ln(weight) - 0.5 * ln(det) - 1.5 * ln(2.0 * PI)
  var r = Component(weight: float32(weight), inv: inv, logNorm: logNorm)
  for i in 0 .. 2:
    r.mean[i] = float32(mean[i])
    for j in 0 .. 2: r.cov[i][j] = float32(c[i][j])
  some(r)

proc logWeighted*(c: Component; z: array[3, float32]): float64 {.inline.} =
  ## `ln(weight · N(z; μ, Σ))`.
  let d = [float64(z[0] - c.mean[0]), float64(z[1] - c.mean[1]), float64(z[2] - c.mean[2])]
  let m = c.inv
  let q = d[0] * (m[0][0] * d[0] + m[0][1] * d[1] + m[0][2] * d[2]) +
          d[1] * (m[1][0] * d[0] + m[1][1] * d[1] + m[1][2] * d[2]) +
          d[2] * (m[2][0] * d[0] + m[2][1] * d[1] + m[2][2] * d[2])
  c.logNorm - 0.5 * q

proc add(a: var Acc; z: array[3, float32]; w: float64) =
  let zz = [float64(z[0]), float64(z[1]), float64(z[2])]
  a.n += w
  for i in 0 .. 2:
    a.s[i] += w * zz[i]
    for j in 0 .. 2: a.ss[i][j] += w * zz[i] * zz[j]

proc component(a: Acc; total: float64; reg: float32): Option[Component] =
  if a.n <= 0.0: return none(Component)
  let m = [a.s[0] / a.n, a.s[1] / a.n, a.s[2] / a.n]
  var c: array[3, array[3, float64]]
  for i in 0 .. 2:
    for j in 0 .. 2: c[i][j] = a.ss[i][j] / a.n - m[i] * m[j]
  newComponent(a.n / total, m, c, float64(reg))

proc nearest(centers: openArray[array[3, float32]]; z: array[3, float32]): int =
  var bd = F32Max
  for i, c in centers:
    let d = d2(c, z)
    if d < bd:
      bd = d
      result = i

proc kmeans*(samples: openArray[array[3, float32]]; k, iters: int): seq[array[3, float32]] =
  ## k-means with k-means++ seeding (deterministic seed). Drops empty clusters.
  if samples.len == 0: return @[]
  var rng = newSegRng(0x5EED'u64 + uint64(samples.len))
  result = @[samples[int(rng.nextU64 mod uint64(samples.len))]]
  var dist = newSeq[float32](samples.len)
  for i, z in samples: dist[i] = d2(z, result[0])
  while result.len < k:
    var total = -0.0
    for d in dist: total += float64(d)
    if total <= 1e-12: break
    var t = float64(rng.f32) * total
    var pick = samples.len - 1
    for i, d in dist:
      t -= float64(d)
      if t <= 0.0:
        pick = i
        break
    let c = samples[pick]
    result.add c
    for i, z in samples: dist[i] = rmin(dist[i], d2(z, c))
  for _ in 0 ..< iters:
    var sums = newSeq[(array[3, float64], int)](result.len)
    for z in samples:
      let i = nearest(result, z)
      for c in 0 .. 2: sums[i][0][c] += float64(z[c])
      sums[i][1] += 1
    var nw: seq[array[3, float32]]
    for s in sums:
      if s[1] > 0:
        nw.add [float32(s[0][0] / float64(s[1])), float32(s[0][1] / float64(s[1])), float32(s[0][2] / float64(s[1]))]
    let done = nw == result
    result = nw
    if done: break

proc fromLabels*(samples: openArray[array[3, float32]]; labels: openArray[int]; k: int; reg: float32): Option[Gmm] =
  ## Component statistics from hard assignments.
  var acc = newSeq[Acc](k)
  for i, z in samples:
    if i < labels.len and labels[i] >= 0 and labels[i] < k: acc[labels[i]].add(z, 1.0)
  let total = float64(samples.len)
  var comps: seq[Component]
  for a in acc:
    let c = a.component(total, reg)
    if c.isSome: comps.add c.get
  if comps.len > 0: some(Gmm(comps: comps, reg: reg)) else: none(Gmm)

proc emStep(g: Gmm; samples: openArray[array[3, float32]]): Option[Gmm] =
  ## One EM step (soft responsibilities).
  let k = g.comps.len
  var acc = newSeq[Acc](k)
  var lw = newSeq[float64](k)
  for z in samples:
    var mx = -F64Max
    for c in 0 ..< k:
      lw[c] = g.comps[c].logWeighted(z)
      mx = rmax(mx, lw[c])
    var sum = 0.0
    for c in 0 ..< k:
      lw[c] = exp(lw[c] - mx)
      sum += lw[c]
    for c in 0 ..< k:
      let r = lw[c] / sum
      if r > 1e-6: acc[c].add(z, r)
  let total = float64(samples.len)
  var comps: seq[Component]
  for a in acc:
    let c = a.component(total, g.reg)
    if c.isSome: comps.add c.get
  if comps.len > 0: some(Gmm(comps: comps, reg: g.reg)) else: none(Gmm)

proc fitEm*(samples: openArray[array[3, float32]]; k: int; reg: float32; emIters: int): Option[Gmm] =
  ## `fit` with an explicit number of EM iterations after k-means.
  if samples.len == 0: return none(Gmm)
  let k = clamp(k, 1, samples.len)
  let centers = kmeans(samples, k, 10)
  var labels = newSeq[int](samples.len)
  for i, z in samples: labels[i] = nearest(centers, z)
  let g0 = fromLabels(samples, labels, centers.len, reg)
  if g0.isNone: return none(Gmm)
  var g = g0.get
  for _ in 0 ..< emIters:
    let n = g.emStep(samples)
    if n.isSome: g = n.get
  some(g)

proc fit*(samples: openArray[array[3, float32]]; k: int; reg: float32): Option[Gmm] =
  ## Fits `k` components (fewer if there are not enough distinct samples). None without samples.
  fitEm(samples, k, reg, 3)

proc componentOf*(g: Gmm; z: array[3, float32]): int =
  ## Index of the most likely component for `z`.
  var bv = -F64Max
  for i, c in g.comps:
    let v = c.logWeighted(z)
    if v > bv:
      bv = v
      result = i

proc refit*(g: Gmm; samples: openArray[array[3, float32]]): Option[Gmm] =
  ## GrabCut update: assign each sample to its most likely component and re-estimate.
  var labels = newSeq[int](samples.len)
  for i, z in samples: labels[i] = g.componentOf(z)
  fromLabels(samples, labels, g.comps.len, g.reg)

proc logProb*(g: Gmm; z: array[3, float32]): float64 =
  ## `ln p(z)`.
  var mx = -F64Max
  var vals: array[16, float64]
  let n = min(g.comps.len, 16)
  for i in 0 ..< n:
    vals[i] = g.comps[i].logWeighted(z)
    mx = rmax(mx, vals[i])
  var s = -0.0 # Rust's Iterator::sum starts at -0.0
  for i in 0 ..< n: s += exp(vals[i] - mx)
  mx + ln(s)

proc negLog*(g: Gmm; z: array[3, float32]): float32 =
  ## `−ln p(z)`, clamped to a finite range.
  float32(clamp(-g.logProb(z), -50.0, 200.0))
