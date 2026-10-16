## Image registration for Edit › Auto-Align Layers (port of `features.rs`): corners, binary
## descriptors, matching and robust model fitting.
##
## * Corners: C. Harris, M. Stephens, *A Combined Corner and Edge Detector*, 1988 (structure
##   tensor `det − k·trace²`, Gaussian window), with non-maximum suppression and a minimum
##   spacing so corners spread over the image.
## * Descriptors: steered BRIEF as in E. Rublee et al., *ORB*, ICCV 2011 — 256 intensity
##   comparisons on a smoothed 31×31 patch, rotated by the patch's intensity-centroid orientation.
## * Matching: brute-force Hamming distance with D. Lowe's ratio test and a mutual-nearest check.
## * Fitting: RANSAC over translation, similarity or homography models; homographies use the
##   normalised DLT of R. Hartley (PAMI 1997). The winning model is refit to all inliers.
##
## Everything is deterministic (fixed-seed RNG). Images are `w × h` luminance in `0..=1`.

import std/[math, options, algorithm, bitops]
import ./[util, transform]

type
  Corner* = object
    ## A detected corner.
    x*, y*, score*: float32

  Descriptor* = array[4, uint64] ## a 256-bit descriptor

  Model* = enum
    ## Model fitted by `ransac`.
    mTranslation
    mSimilarity ## rotation, uniform scale and translation
    mHomography

const PatchRadius* = 15 ## patch radius used by descriptors (corners closer to the border are dropped)

type FRng* = object
  ## The module's SplitMix64 generator (exported for tests).
  state: uint64

proc newFRng*(seed: uint64): FRng = FRng(state: seed)

proc next*(r: var FRng): uint64 =
  r.state += 0x9E37_79B9_7F4A_7C15'u64
  var z = r.state
  z = (z xor (z shr 30)) * 0xBF58_476D_1CE4_E5B9'u64
  z = (z xor (z shr 27)) * 0x94D0_49BB_1331_11EB'u64
  z xor (z shr 31)

proc asI64r(v: float32): int64 {.inline.} =
  ## Rust `v as i64`: saturating, NaN → 0.
  if v != v: 0'i64
  elif v >= 9.223372e18'f32: high(int64)
  elif v <= -9.223372e18'f32: low(int64)
  else: int64(v)

proc unit*(r: var FRng): float64 = float64(r.next shr 11) / float64(1'u64 shl 53)
proc below(r: var FRng; n: int): int = int(r.next mod uint64(max(n, 1)))

proc blur(w, h: int; v: openArray[float32]; r: int): seq[float32] =
  proc pass(src: seq[float32]; horizontal: bool): seq[float32] =
    result = newSeq[float32](src.len)
    let (n, m) = (if horizontal: (h, w) else: (w, h))
    for line in 0 ..< n:
      template at(k0: int): float32 =
        block:
          let k = clamp(k0, 0, m - 1)
          (if horizontal: src[line * w + k] else: src[k * w + line])
      var acc = -0'f32
      for k in -r .. r: acc += at(k)
      for k in 0 ..< m:
        let i = (if horizontal: line * w + k else: k * w + line)
        result[i] = acc / float32(2 * r + 1)
        acc += at(k + r + 1) - at(k - r)
  pass(pass(@v, true), false)

proc harris*(w, h: int; img: openArray[float32]; max: int; minDist: float32; valid: Option[seq[bool]]): seq[Corner] =
  ## Harris corners: at most `max` corners at least `minDist` pixels apart, strongest first, only
  ## where `valid` (if given) is true for the whole descriptor patch.
  if w < 2 * PatchRadius + 3 or h < 2 * PatchRadius + 3 or img.len < w * h: return @[]
  if valid.isSome and valid.get.len < w * h: return @[]
  var xx, yy, xy = newSeq[float32](w * h)
  for y in 1 ..< h - 1:
    for x in 1 ..< w - 1:
      template p(dx, dy: int): float32 = img[(y + dy) * w + x + dx]
      # Sobel.
      let gx = (p(1, -1) + 2'f32 * p(1, 0) + p(1, 1)) - (p(-1, -1) + 2'f32 * p(-1, 0) + p(-1, 1))
      let gy = (p(-1, 1) + 2'f32 * p(0, 1) + p(1, 1)) - (p(-1, -1) + 2'f32 * p(0, -1) + p(1, -1))
      let i = y * w + x
      xx[i] = gx * gx
      yy[i] = gy * gy
      xy[i] = gx * gy
  # Two box passes approximate the Gaussian window.
  xx = blur(w, h, blur(w, h, xx, 1), 1)
  yy = blur(w, h, blur(w, h, yy, 1), 1)
  xy = blur(w, h, blur(w, h, xy, 1), 1)
  var r = newSeq[float32](w * h)
  for i in 0 ..< w * h:
    let t = xx[i] + yy[i]
    r[i] = xx[i] * yy[i] - xy[i] * xy[i] - 0.04'f32 * (t * t)
  var peak = 0'f32
  for v in r: peak = rmax(peak, v)
  if peak <= 0: return @[]
  let m = PatchRadius + 1
  var cand: seq[Corner]
  for y in m ..< h - m:
    for x in m ..< w - m:
      let v = r[y * w + x]
      if v < peak * 0.01'f32: continue
      var isMax = true
      for dy in -1 .. 1:
        for dx in -1 .. 1:
          if not (dx == 0 and dy == 0) and not (r[(y + dy) * w + x + dx] <= v): isMax = false
      if not isMax: continue
      if valid.isSome:
        var all = true
        var yy2 = y - PatchRadius
        while yy2 <= y + PatchRadius and all:
          var xx2 = x - PatchRadius
          while xx2 <= x + PatchRadius:
            if not valid.get[yy2 * w + xx2]:
              all = false
              break
            xx2 += 5
          yy2 += 5
        if not all: continue
      cand.add Corner(x: float32(x), y: float32(y), score: v)
  cand.sort(proc(a, b: Corner): int =
    var c = totalCmp(b.score, a.score)
    if c == 0: c = totalCmp(a.y, b.y)
    if c == 0: c = totalCmp(a.x, b.x)
    c)
  # Minimum spacing via a coarse occupancy grid.
  let cell = rmax(minDist, 1'f32)
  let (gw, gh) = (asIdx(ceil(float32(w) / cell)) + 1, asIdx(ceil(float32(h) / cell)) + 1)
  var grid = newSeq[seq[(float32, float32)]](gw * gh)
  for c in cand:
    let (gx, gy) = (asIdx(c.x / cell), asIdx(c.y / cell))
    var near = false
    for yy3 in max(gy - 1, 0) .. min(gy + 1, gh - 1):
      for xx3 in max(gx - 1, 0) .. min(gx + 1, gw - 1):
        for (px, py) in grid[yy3 * gw + xx3]:
          if hypot(px - c.x, py - c.y) < minDist: near = true
    if near: continue
    grid[gy * gw + gx].add (c.x, c.y)
    result.add c
    if result.len >= max: break

proc makePairs(): array[256, array[4, float32]] =
  ## The 256 BRIEF test pairs (fixed, Gaussian around the centre, σ = 31/5, clipped to the patch).
  var rng = FRng(state: 0x0B21_EF00'u64)
  proc gauss(rng: var FRng): float32 =
    # Box–Muller.
    let u = rmax(rng.unit, 1e-12)
    let v = rng.unit
    float32(clamp(sqrt(-2.0 * ln(u)) * cos(TAU * v) * 31.0 / 5.0, -13.0, 13.0))
  for q in result.mitems:
    let a = gauss(rng)
    let b = gauss(rng)
    let c = gauss(rng)
    let d = gauss(rng)
    q = [a, b, c, d]

let Pairs = makePairs()

proc describe*(w, h: int; img: openArray[float32]; corners: openArray[Corner]; upright: bool): seq[Descriptor] =
  ## Steered-BRIEF descriptors for `corners` (orientation from the intensity centroid unless
  ## `upright`). Returns one descriptor per corner, in order.
  if w <= 0 or h <= 0 or img.len < w * h: return newSeq[Descriptor](corners.len)
  let smooth = blur(w, h, img, 2)
  template at(x, y: float32): float32 =
    smooth[int(clamp(asI64r(round(y)), 0'i64, int64(h - 1))) * w + int(clamp(asI64r(round(x)), 0'i64, int64(w - 1)))]
  for c in corners:
    var s, co: float32
    if upright: (s, co) = (0'f32, 1'f32)
    else:
      var (m10, m01) = (0'f32, 0'f32)
      let r = PatchRadius
      for dy in -r .. r:
        for dx in -r .. r:
          if dx * dx + dy * dy <= r * r:
            let v = at(c.x + float32(dx), c.y + float32(dy))
            m10 += float32(dx) * v
            m01 += float32(dy) * v
      let a = arctan2(m01, m10)
      (s, co) = (sin(a), cos(a))
    var d: Descriptor
    for i, p in Pairs:
      let (ax, ay) = (c.x + p[0] * co - p[1] * s, c.y + p[0] * s + p[1] * co)
      let (bx, by) = (c.x + p[2] * co - p[3] * s, c.y + p[2] * s + p[3] * co)
      if at(ax, ay) < at(bx, by): d[i div 64] = d[i div 64] or (1'u64 shl (i mod 64))
    result.add d

proc hamming*(a, b: Descriptor): uint32 =
  for i in 0 .. 3: result += uint32(popcount(a[i] xor b[i]))

proc matchDescriptors*(a, b: openArray[Descriptor]; ratio: float32): seq[(int, int)] =
  ## Matches `(index in a, index in b)` passing the ratio test and the mutual-nearest check.
  proc best(d: Descriptor; set: openArray[Descriptor]): Option[(int, uint32, uint32)] =
    var first = (high(int), high(uint32))
    var second = high(uint32)
    for j, e in set:
      let hd = hamming(d, e)
      if hd < first[1]:
        second = first[1]
        first = (j, hd)
      elif hd < second: second = hd
    if first[0] != high(int): some((first[0], first[1], second)) else: none((int, uint32, uint32))
  for i in 0 ..< a.len:
    let bb = best(a[i], b)
    if bb.isNone: continue
    let (j, d1, d2) = bb.get
    if d1 > 80 or (d2 != high(uint32) and float32(d1) > ratio * float32(d2)): continue
    let back = best(b[j], a)
    if back.isSome and back.get[0] == i: result.add (i, j)

# ---------------------------------------------------------------- model fitting

proc solveSys(n: int; a0, b0: seq[float64]): Option[seq[float64]] =
  ## Solve the `n × n` system `a · x = b` (row-major) by Gaussian elimination with partial
  ## pivoting.
  var a = a0
  var b = b0
  for c in 0 ..< n:
    # `max_by` keeps the last of equal maxima.
    var p = c
    for i in c + 1 ..< n:
      if totalCmp(abs(a[i * n + c]), abs(a[p * n + c])) >= 0: p = i
    if abs(a[p * n + c]) < 1e-12: return none(seq[float64])
    if p != c:
      for k in 0 ..< n: swap(a[c * n + k], a[p * n + k])
      swap(b[c], b[p])
    for r in c + 1 ..< n:
      let f = a[r * n + c] / a[c * n + c]
      for k in c ..< n: a[r * n + k] -= f * a[c * n + k]
      b[r] -= f * b[c]
  var x = newSeq[float64](n)
  for r in countdown(n - 1, 0):
    var s = -0.0
    for k in r + 1 ..< n: s += a[r * n + k] * x[k]
    x[r] = (b[r] - s) / a[r * n + r]
  some(x)

proc leastSquares(rows: seq[(seq[float64], float64)]; n: int): Option[seq[float64]] =
  ## Least squares `min |A x − b|` through the normal equations.
  var ata = newSeq[float64](n * n)
  var atb = newSeq[float64](n)
  for (r, b) in rows:
    for i in 0 ..< n:
      atb[i] += r[i] * b
      for j in 0 ..< n: ata[i * n + j] += r[i] * r[j]
  solveSys(n, ata, atb)

proc normaliser(pts: openArray[array[2, float64]]): Homography =
  ## Hartley normalisation: centroid to the origin, mean distance √2.
  let n = float64(max(pts.len, 1))
  var (sx, sy) = (-0.0, -0.0)
  for p in pts:
    sx += p[0]
    sy += p[1]
  let (cx, cy) = (sx / n, sy / n)
  var sd = -0.0
  for p in pts: sd += hypot(p[0] - cx, p[1] - cy)
  let d = sd / n
  let s = (if d > 1e-12: sqrt(2.0) / d else: 1.0)
  homography([s, 0.0, -s * cx, 0.0, s, -s * cy, 0.0, 0.0, 1.0])

proc fit*(model: Model; src, dst: openArray[array[2, float64]]): Option[Homography] =
  ## Fit a model mapping `src[i]` → `dst[i]` by least squares (exact for minimal sets).
  let n = min(src.len, dst.len)
  case model
  of mTranslation:
    if n == 0: return none(Homography)
    var (tx, ty) = (0.0, 0.0)
    for i in 0 ..< n:
      tx = tx + dst[i][0] - src[i][0]
      ty = ty + dst[i][1] - src[i][1]
    some(homography([1.0, 0.0, tx / float64(n), 0.0, 1.0, ty / float64(n), 0.0, 0.0, 1.0]))
  of mSimilarity:
    if n < 2: return none(Homography)
    # x' = a·x − b·y + tx, y' = b·x + a·y + ty.
    var rows: seq[(seq[float64], float64)]
    for i in 0 ..< n:
      let (s, d) = (src[i], dst[i])
      rows.add (@[s[0], -s[1], 1.0, 0.0], d[0])
      rows.add (@[s[1], s[0], 0.0, 1.0], d[1])
    let v = leastSquares(rows, 4)
    if v.isNone: return none(Homography)
    let q = v.get
    some(homography([q[0], -q[1], q[2], q[1], q[0], q[3], 0.0, 0.0, 1.0]))
  of mHomography:
    if n < 4: return none(Homography)
    let (ts, td) = (normaliser(src.toOpenArray(0, n - 1)), normaliser(dst.toOpenArray(0, n - 1)))
    var rows: seq[(seq[float64], float64)]
    for i in 0 ..< n:
      let (x, y) = ts.apply(src[i][0], src[i][1])
      let (u, v) = td.apply(dst[i][0], dst[i][1])
      rows.add (@[x, y, 1.0, 0.0, 0.0, 0.0, -u * x, -u * y], u)
      rows.add (@[0.0, 0.0, 0.0, x, y, 1.0, -v * x, -v * y], v)
    let hv = leastSquares(rows, 8)
    if hv.isNone: return none(Homography)
    let h = hv.get
    let hn = homography([h[0], h[1], h[2], h[3], h[4], h[5], h[6], h[7], 1.0])
    let tdi = td.inverse
    if tdi.isNone: return none(Homography)
    let m = tdi.get.mul(hn).mul(ts)
    let k = m.m[8]
    if not (abs(k) > 1e-12): return none(Homography)
    var o: array[9, float64]
    for i in 0 .. 8: o[i] = m.m[i] / k
    some(homography(o))

proc minPoints(model: Model): int =
  case model
  of mTranslation: 1
  of mSimilarity: 2
  of mHomography: 4

proc ransacInliers*(h: Homography; src, dst: openArray[array[2, float64]]; n: int; threshold: float64): seq[int] =
  ## The indices `i < n` whose `src` point `h` maps within `threshold` of its `dst` point.
  for i in 0 ..< n:
    let (x, y) = h.apply(src[i][0], src[i][1])
    if x == x and x - x == 0.0 and hypot(x - dst[i][0], y - dst[i][1]) <= threshold: result.add i

proc ransac*(model: Model; src, dst: openArray[array[2, float64]]; iters: int; threshold: float64; seed: uint64): Option[(Homography, seq[int])] =
  ## RANSAC: the model with the most correspondences within `threshold` pixels after `iters`
  ## random minimal samples, refit to its inliers. Returns the model and the inlier indices.
  let k = minPoints(model)
  let n = min(src.len, dst.len)
  if n < k: return none((Homography, seq[int]))
  var rng = FRng(state: seed xor 0xA11C_0DE5'u64)
  template inliersOf(h: Homography): seq[int] = ransacInliers(h, src, dst, n, threshold)
  var best = none((Homography, seq[int]))
  for _ in 0 ..< max(iters, 1):
    var idx = newSeqOfCap[int](k)
    while idx.len < k:
      let i = rng.below(n)
      if i notin idx: idx.add i
      if n == k:
        idx.setLen(0)
        for q in 0 ..< k: idx.add q
    var s, d: seq[array[2, float64]]
    for i in idx:
      s.add src[i]
      d.add dst[i]
    let h = fit(model, s, d)
    if h.isNone: continue
    let inl = inliersOf(h.get)
    if best.isNone or inl.len > best.get[1].len: best = some((h.get, inl))
  if best.isNone: return none((Homography, seq[int]))
  let (h, inl) = best.get
  if inl.len < k: return none((Homography, seq[int]))
  # Refit to all inliers, then re-collect.
  var s, d: seq[array[2, float64]]
  for i in inl:
    s.add src[i]
    d.add dst[i]
  let h2 = fit(model, s, d).get(h)
  let inl2 = inliersOf(h2)
  some(if inl2.len >= inl.len: (h2, inl2) else: (h, inl))

proc register*(w, h: int; reference, moving: openArray[float32]; validRef, validMov: Option[seq[bool]];
               model: Model): Option[(Homography, int)] =
  ## Register `moving` onto `reference` (both `w × h` luminance; optional validity masks for
  ## transparent areas). Returns the model mapping `moving` coordinates to `reference`
  ## coordinates and the inlier count.
  let upright = model == mTranslation
  let minDist = clamp(float32(max(w, h)) / 60'f32, 4'f32, 24'f32)
  let ca = harris(w, h, reference, 600, minDist, validRef)
  let cb = harris(w, h, moving, 600, minDist, validMov)
  if ca.len < 4 or cb.len < 4: return none((Homography, int))
  let (da, db) = (describe(w, h, reference, ca, upright), describe(w, h, moving, cb, upright))
  let matches = matchDescriptors(db, da, 0.85'f32)
  var src, dst: seq[array[2, float64]]
  for (i, j) in matches:
    src.add [float64(cb[i].x), float64(cb[i].y)]
    dst.add [float64(ca[j].x), float64(ca[j].y)]
  let threshold = clamp(float64(max(w, h)) / 400.0, 1.5, 4.0)
  let r = ransac(model, src, dst, 1000, threshold, 7)
  if r.isNone: return none((Homography, int))
  let (hm, inl) = r.get
  if inl.len >= max(minPoints(model), 6): some((hm, inl.len)) else: none((Homography, int))
