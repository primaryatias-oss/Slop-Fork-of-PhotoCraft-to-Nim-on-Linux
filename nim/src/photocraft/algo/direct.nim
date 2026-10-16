## Direct (intensity-based) registration for Edit › Auto-Align Layers, for layers that nearly
## coincide: a focus bracket, an exposure bracket, a burst (port of `direct.rs`). Feature
## matching (`panorama`) wants distinctive corners seen from two places; such layers are the same
## view with a sub-pixel shift, a little scale (focus breathing) and rotation, and much of each
## layer defocused, so there is often nothing to match and nothing to verify. Here the whole
## overlap is the evidence: the pose that makes the warped target look like the reference.
##
## The target is registered to the reference coarse-to-fine on a Gaussian pyramid of the
## luminance. The cost of a pose is the RMS of the difference between the reference and the
## target warped by it, the mean difference removed (so exposure and tone changes don't count),
## over the pixels both cover. Each level runs a bounded Nelder-Mead search whose simplex starts
## one pixel of that level wide and stops at a tenth of one, so the coarse levels only hand the
## next one a start within its pixel and the finest level searched settles sub-pixel. The pose
## is resolution independent (shifts as fractions of the size, a scale, an angle), so a fit that
## stops a few levels short of full resolution still applies at full resolution.
##
## `Pose` is the transform searched: a similarity (shift, scale, rotation) about the layer's
## centre, with aspect and shear for an affine fit and two perspective terms for a projective
## one; `Motion` says which of them move. Everything is deterministic.

import std/[math, options, algorithm]
import ./[util, transform, panorama]

const PoseN* = 8 ## the number of pose parameters (the length of `toVec`)

type
  Pose* = object
    ## A layer's transform onto the reference, about the layer's centre.
    dx*: float64     ## shift, as a fraction of the width
    dy*: float64     ## shift, as a fraction of the height
    scale*: float64
    rot*: float64    ## rotation in radians
    aspect*: float64 ## vertical over horizontal scale (1 = isotropic)
    shear*: float64  ## horizontal shear per unit of height (0 = none)
    px*, py*: float64 ## perspective: the divisor `1 + px·u/w + py·v/h` over the centred pixel

  Plane* = object
    ## A luminance plane with, where the layer does not cover its whole area, its coverage.
    w*, h*: int
    luma*: seq[float32]
    valid*: Option[seq[bool]]

  Registration* = object
    ## The outcome of `register`.
    pose*: Pose
    h*: Homography      ## the pose as a homography of the target's pixels onto the reference's
    rms*: float64       ## the cost at the pose, at the finest level searched
    overlap*: int       ## pixels of the finest level searched that both layers cover
    correlation*: float64 ## Pearson correlation of the reference with the warped target

  Lvl* = object
    ## One pyramid level: the plane, its coverage and its size.
    luma*: seq[float32]
    valid*: Option[seq[bool]]
    w*, h*: int

const PoseIdentity* = Pose(dx: 0.0, dy: 0.0, scale: 1.0, rot: 0.0, aspect: 1.0, shear: 0.0, px: 0.0, py: 0.0)
## The search box's half-width around the start, per parameter: 10 % of the layer in shift,
## 10 % in scale, 5° in rotation, 5 % in aspect and shear, 5 % in each perspective term.
const Span: array[PoseN, float64] = [0.10, 0.10, 0.10, 0.087_266_462_599_716_47, 0.05, 0.05, 0.05, 0.05]

proc toVec*(p: Pose): array[PoseN, float64] =
  [p.dx, p.dy, p.scale, p.rot, p.aspect, p.shear, p.px, p.py]

proc fromVec*(v: array[PoseN, float64]): Pose =
  Pose(dx: v[0], dy: v[1], scale: v[2], rot: v[3], aspect: v[4], shear: v[5], px: v[6], py: v[7])

proc free*(motion: Motion): array[PoseN, bool] =
  ## Which parameters a search over `motion` may move.
  case motion
  of moTranslation: [true, true, false, false, false, false, false, false]
  of moEuclidean: [true, true, false, true, false, false, false, false]
  of moSimilarity: [true, true, true, true, false, false, false, false]
  of moHomography: [true, true, true, true, true, true, true, true]

proc matrix*(p: Pose; w, h: int): Homography =
  ## The homography mapping the layer's pixels onto the reference's.
  let (cx, cy) = (float64(w) / 2.0, float64(h) / 2.0)
  let (c, s) = (cos(p.rot), sin(p.rot))
  let sc = p.scale
  let (a, b, d, e) = (sc * c, sc * (c * p.shear - s * p.aspect), sc * s, sc * (s * p.shear + c * p.aspect))
  let (gx, gy) = (p.px / float64(w), p.py / float64(h))
  let (a2, b2, d2, e2) = (a + cx * gx, b + cx * gy, d + cy * gx, e + cy * gy)
  let tx = cx + p.dx * float64(w) - (a2 * cx + b2 * cy)
  let ty = cy + p.dy * float64(h) - (d2 * cx + e2 * cy)
  Homography(m: [a2, b2, tx, d2, e2, ty, gx, gy, 1.0 - gx * cx - gy * cy])

const K: array[5, float32] = [1.0'f32 / 16.0'f32, 4.0'f32 / 16.0'f32, 6.0'f32 / 16.0'f32, 4.0'f32 / 16.0'f32,
                              1.0'f32 / 16.0'f32]

proc reduce(l: Lvl): Lvl =
  ## Gaussian REDUCE of a plane and its coverage (5-tap binomial, clamped borders, phase-0
  ## subsample); a reduced pixel is covered when more than half its support is.
  let (w, h) = ((l.w + 1) div 2, (l.h + 1) div 2)
  var cover: seq[float32]
  if l.valid.isSome:
    for b in l.valid.get: cover.add(if b: 1.0'f32 else: 0.0'f32)
  proc pass(src: seq[float32]): seq[float32] =
    var tmp = newSeq[float32](w * l.h)
    for y in 0 ..< l.h:
      let base = y * l.w
      for x in 0 ..< w:
        var acc = -0.0'f32 # Rust's Iterator::sum starts at -0.0
        for k in 0 ..< 5:
          acc += K[k] * src[base + min(max(2 * x + k - 2, 0), l.w - 1)]
        tmp[y * w + x] = acc
    result = newSeq[float32](w * h)
    for y in 0 ..< h:
      for k in 0 ..< 5:
        let sy = min(max(2 * y + k - 2, 0), l.h - 1)
        let wt = K[k]
        for x in 0 ..< w:
          result[y * w + x] += wt * tmp[sy * w + x]
  result = Lvl(luma: pass(l.luma), w: w, h: h)
  if cover.len > 0:
    var v = newSeq[bool](w * h)
    let p = pass(cover)
    for i in 0 ..< p.len: v[i] = p[i] > 0.5'f32
    result.valid = some(v)

proc levels*(p: Plane): seq[Lvl] =
  ## The search pyramid of a plane: level 0 is the plane, each next level half its size, while
  ## the height exceeds 64 and the width 8.
  result = @[Lvl(luma: p.luma, valid: p.valid, w: p.w, h: p.h)]
  while result[^1].h > 64 and result[^1].w > 8:
    result.add(reduce(result[^1]))

proc spline4(t: float64): array[4, float64] {.inline.} =
  ## The cubic B-spline (4 taps) at fraction `t`.
  [((-1.0 / 3.0 * t + 0.8) * t - 0.466_666_67) * t,
   ((t - 1.8) * t - 0.2) * t + 1.0,
   ((1.2 - t) * t + 0.8) * t,
   ((1.0 / 3.0 * t - 0.2) * t - 0.133_333_34) * t]

proc affineRow(inv: Homography; y: float64): Option[(float64, float64, float64, float64)] {.inline.} =
  ## The source points along output row `y` under `inv`: `(x0, y0, dx, dy)` such that pixel `x`
  ## reads `(x0 + dx·x, y0 + dy·x)`; none for a projective `inv`, handled pixel by pixel.
  let m = inv.m
  if m[6] == 0.0 and m[7] == 0.0 and m[8] == 1.0: some((m[1] * y + m[2], m[4] * y + m[5], m[0], m[3]))
  else: none((float64, float64, float64, float64))

proc asI64(v: float64): int64 {.inline.} =
  if v != v: 0'i64
  elif v >= 9.223372036854775807e18: high(int64)
  elif v <= -9.223372036854775808e18: low(int64)
  else: int64(v)

proc sample*(src: openArray[float32]; w, h: int; sx, sy: float64): float32 {.inline.} =
  ## `src` sampled at `(sx, sy)` with the cubic B-spline, edge-clamped.
  let (fx0, fy0) = (floor(sx), floor(sy))
  let wx = spline4(sx - fx0)
  let wy = spline4(sy - fy0)
  let (x0, y0) = (asI64(fx0), asI64(fy0))
  var acc = 0.0
  for j in 0 ..< 4:
    let yy = int(clamp(y0 + int64(j) - 1, 0'i64, int64(h) - 1))
    var r = 0.0
    for i in 0 ..< 4:
      let xx = int(clamp(x0 + int64(i) - 1, 0'i64, int64(w) - 1))
      r += wx[i] * float64(src[yy * w + xx])
    acc += wy[j] * r
  float32(acc)

proc covered(valid: Option[seq[bool]]; w: int; sx, sy: float64): bool {.inline.} =
  ## Whether the target covers the source point (nearest pixel of its coverage).
  valid.isNone or valid.get()[int(sy + 0.5) * w + int(sx + 0.5)]

proc cost*(reference, target: Lvl; pose: Pose): (float64, int, float64) =
  ## The cost of `pose`: the RMS of `reference − target ∘ pose`, the mean removed, over the
  ## pixels of the reference that both cover; 1e9 when fewer than 16 do. Also the count and the
  ## correlation of r and t over the same pixels (0 when either has no spread).
  let invO = pose.matrix(target.w, target.h).inverse()
  if invO.isNone: return (1e9, 0, 0.0)
  let inv = invO.get
  let (xmax, ymax) = (float64(target.w - 1), float64(target.h - 1))
  var (sr, st, sr2, st2, srt, cnt) = (0.0, 0.0, 0.0, 0.0, 0.0, 0)
  for y in 0 ..< reference.h:
    var (rsr, rst, rsr2, rst2, rsrt, rcnt) = (0.0, 0.0, 0.0, 0.0, 0.0, 0)
    let yf = float64(y)
    let aff = affineRow(inv, yf)
    for x in 0 ..< reference.w:
      let r0 = reference.luma[y * reference.w + x]
      if reference.valid.isSome and not reference.valid.get()[y * reference.w + x]: continue
      var sx, sy: float64
      if aff.isSome:
        let (x0, y0, dx, dy) = aff.get
        sx = x0 + dx * float64(x)
        sy = y0 + dy * float64(x)
      else:
        (sx, sy) = inv.apply(float64(x), yf)
      let inside = sx >= 0.0 and sx <= xmax and sy >= 0.0 and sy <= ymax
      if not inside or not covered(target.valid, target.w, sx, sy): continue
      let (r, t) = (float64(r0), float64(sample(target.luma, target.w, target.h, sx, sy)))
      rsr += r
      rst += t
      rsr2 += r * r
      rst2 += t * t
      rsrt += r * t
      rcnt += 1
    sr += rsr
    st += rst
    sr2 += rsr2
    st2 += rst2
    srt += rsrt
    cnt += rcnt
  if cnt < 16: return (1e9, cnt, 0.0)
  let n = float64(cnt)
  let (vr, vt, cov) = (max(sr2 - sr * sr / n, 0.0), max(st2 - st * st / n, 0.0), srt - sr * st / n)
  let rms = sqrt(max((vr + vt - 2.0 * cov) / n, 0.0))
  let correlation = if vr > 0.0 and vt > 0.0: clamp(cov / sqrt(vr * vt), -1.0, 1.0) else: 0.0
  (rms, cnt, correlation)

proc paramGain(w, h: int): array[PoseN, float64] =
  ## Full-resolution pixels a point at the layer's edge moves per unit of each parameter.
  let (w, h) = (float64(w), float64(h))
  let r = max(w, h) / 2.0
  [w, h, r, r, h / 2.0, h / 2.0, w / 4.0, h / 4.0]

const StepPx = 1.0 ## the simplex's first step at a level: one pixel of that level
const TolPx = 0.1  ## the simplex's stopping size at a level: a tenth of a pixel of that level

proc levelSteps*(free: seq[int]; lvl, w, h: int): (seq[float64], seq[float64]) =
  ## The search's schedule at pyramid level `lvl` over the parameters `free`.
  let gain = paramGain(w, h)
  let px = float64(1'u64 shl min(lvl, 60))
  for k in free:
    result[0].add(StepPx * px / gain[k])
  for k in free:
    result[1].add(TolPx * px / gain[k])

proc converged(simplex: seq[seq[float64]]; fv: seq[float64]; tol: seq[float64]): bool =
  ## Nelder-Mead's stopping rule on a simplex sorted by `fv`.
  let n = tol.len
  if fv.len == 0 or n >= fv.len or simplex.len == 0: return true
  let (first, last, best) = (fv[0], fv[n], simplex[0])
  if abs(last - first) > 1e-4 * (1.0 + abs(first)): return false
  for i in 1 ..< simplex.len:
    for k in 0 ..< n:
      if not (abs(simplex[i][k] - best[k]) <= tol[k]): return false
  true

proc nelderMead*(f: proc(x: seq[float64]): float64; x0, lo, hi, step, tol: seq[float64]): seq[float64] =
  ## Bounded Nelder-Mead: minimises `f` over the box `[lo, hi]` from `x0`, the first simplex `x0`
  ## moved by `step[k]` along each axis, until converged or 200 iterations.
  let n = x0.len
  if n == 0: return @[]
  proc clampV(v: var seq[float64]) =
    for k in 0 ..< n: v[k] = clamp(v[k], lo[k], hi[k])
  var simplex = @[x0]
  for k in 0 ..< n:
    var v = x0
    v[k] += step[k]
    clampV(v)
    simplex.add(v)
  var fv: seq[float64]
  for v in simplex: fv.add(f(v))
  let (alpha, gamma, rho, sigma) = (1.0, 2.0, 0.5, 0.5)
  for _ in 0 ..< 200:
    var idx = newSeq[int](n + 1)
    for i in 0 .. n: idx[i] = i
    let fvs = fv
    idx.sort(proc(a, b: int): int = totalCmp(fvs[a], fvs[b]))
    var ns: seq[seq[float64]]
    var nf: seq[float64]
    for i in idx:
      ns.add(simplex[i])
      nf.add(fv[i])
    simplex = ns
    fv = nf
    if converged(simplex, fv, tol): break
    var c = newSeq[float64](n)
    for i in 0 ..< n:
      for k in 0 ..< n:
        c[k] += simplex[i][k] / float64(n)
    var xr = newSeq[float64](n)
    for k in 0 ..< n: xr[k] = c[k] + alpha * (c[k] - simplex[n][k])
    clampV(xr)
    let fr = f(xr)
    if fr < fv[0]:
      var xe = newSeq[float64](n)
      for k in 0 ..< n: xe[k] = c[k] + gamma * (xr[k] - c[k])
      clampV(xe)
      let fe = f(xe)
      if fe < fr:
        simplex[n] = xe
        fv[n] = fe
      else:
        simplex[n] = xr
        fv[n] = fr
    elif fr < fv[n - 1]:
      simplex[n] = xr
      fv[n] = fr
    else:
      var xc = newSeq[float64](n)
      for k in 0 ..< n: xc[k] = c[k] + rho * (simplex[n][k] - c[k])
      clampV(xc)
      let fc = f(xc)
      if fc < fv[n]:
        simplex[n] = xc
        fv[n] = fc
      else:
        let best = simplex[0]
        for i in 1 .. n:
          for k in 0 ..< n:
            simplex[i][k] = best[k] + sigma * (simplex[i][k] - best[k])
          clampV(simplex[i])
          fv[i] = f(simplex[i])
  var best = 0
  for i in 1 .. n:
    if totalCmp(fv[i], fv[best]) < 0: best = i
  simplex[best]

proc checkPlane(p: Plane) =
  if p.w < 0 or p.h < 0 or p.luma.len != p.w * p.h:
    raise newException(ValueError, "plane size does not match its luminance buffer")
  if p.valid.isSome and p.valid.get.len != p.w * p.h:
    raise newException(ValueError, "plane size does not match its coverage")

proc register*(reference, target: Plane; motion: Motion; init: Pose; coarsen: int): Registration =
  ## Registers `target` to `reference` with the parameters `motion` moves, from `init` (the
  ## previous layer's pose, or the identity), stopping `coarsen` levels short of full resolution
  ## (at least one level is searched). Raises ValueError on a plane whose buffers don't match
  ## its size.
  checkPlane(reference)
  checkPlane(target)
  let (w, h) = (target.w, target.h)
  let pref = levels(reference)
  let ptgt = levels(target)
  let n = min(pref.len, ptgt.len)
  let fm = free(motion)
  var freeIdx: seq[int]
  for k in 0 ..< PoseN:
    if fm[k]: freeIdx.add(k)
  let iv = init.toVec
  var cur = iv
  var lo, hi: seq[float64]
  for k in freeIdx:
    lo.add(iv[k] - Span[k])
    hi.add(iv[k] + Span[k])
  let finest = min(max(coarsen, 0), max(n - 1, 0))
  var (rms, overlap, correlation) = (1e9, 0, 0.0)
  var lvl = n - 1
  while lvl >= finest:
    let snap = cur
    let fi = freeIdx
    let (rl, tl) = (pref[lvl], ptgt[lvl])
    let f = proc(xf: seq[float64]): float64 =
      var v = snap
      for k, idx in fi: v[idx] = xf[k]
      cost(rl, tl, fromVec(v))[0]
    var x0: seq[float64]
    for k in freeIdx: x0.add(cur[k])
    let (step, tol) = levelSteps(freeIdx, lvl, w, h)
    let best = nelderMead(f, x0, lo, hi, step, tol)
    for k, idx in freeIdx: cur[idx] = best[k]
    (rms, overlap, correlation) = cost(rl, tl, fromVec(cur))
    dec lvl
  let pose = fromVec(cur)
  Registration(pose: pose, h: pose.matrix(w, h), rms: rms, overlap: overlap, correlation: correlation)

proc warp*(src: Plane; pose: Pose): (seq[float32], seq[bool]) =
  ## `src` (with its coverage) warped by `pose` into a plane of the same size with the cubic
  ## B-spline, and the coverage of the result. Raises ValueError on mismatched buffers.
  checkPlane(src)
  let (w, h) = (src.w, src.h)
  var outp = newSeq[float32](w * h)
  var valid = newSeq[bool](w * h)
  let invO = pose.matrix(w, h).inverse()
  if invO.isNone: return (outp, valid)
  let inv = invO.get
  let (xmax, ymax) = (float64(w - 1), float64(h - 1))
  for y in 0 ..< h:
    let yf = float64(y)
    let aff = affineRow(inv, yf)
    for x in 0 ..< w:
      var sx, sy: float64
      if aff.isSome:
        let (x0, y0, dx, dy) = aff.get
        sx = x0 + dx * float64(x)
        sy = y0 + dy * float64(x)
      else:
        (sx, sy) = inv.apply(float64(x), yf)
      let inside = sx >= 0.0 and sx <= xmax and sy >= 0.0 and sy <= ymax
      valid[y * w + x] = inside and covered(src.valid, w, sx, sy)
      if inside:
        outp[y * w + x] = sample(src.luma, w, h, sx, sy)
  (outp, valid)
