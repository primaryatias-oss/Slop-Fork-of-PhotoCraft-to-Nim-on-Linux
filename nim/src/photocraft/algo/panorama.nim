## Panorama stitching for File › Automate › Photomerge and Edit › Auto-Align / Auto-Blend Layers
## (port of `panorama.rs`).
##
## The pipeline follows M. Brown, D. G. Lowe, *Automatic Panoramic Image Stitching using
## Invariant Features*, IJCV 2007, with classical parts throughout:
##
## 1. **Features**: Harris corners + steered BRIEF (`features`).
## 2. **Pair matching**: ratio-test matches, RANSAC homography, Brown–Lowe verification.
## 3. **Focal length**: from the pairwise homographies of a rotating camera (Shum & Szeliski,
##    IJCV 2000), or from EXIF.
## 4. **Projection + motion model** per layout: Perspective (plane, homography), Cylindrical and
##    Spherical (rigid motion on the cylinder / sphere), Collage (similarity), Reposition
##    (translation). Auto picks perspective unless the result is too stretched.
## 5. **Chained alignment + bundle adjustment**: a maximum spanning tree over the match graph,
##    then Levenberg–Marquardt on the distance between all matched points (optionally refining
##    one radial distortion coefficient and the focal length).
## 6. **Photometric**: gain compensation jointly with a radial vignetting model
##    `log V(r) = a·r² + b·r⁴` (D. Goldman, PAMI 2010), by linear least squares in log space.
## 7. **Seams**: per new image, a minimum-cost path through its overlap with the composite.
## 8. **Multi-band blending**: Laplacian pyramids (Burt & Adelson 1983) weighted by the Gaussian
##    pyramid of each seam mask, holes filled by push-pull.
##
## Everything is deterministic. Registration works on luminance in `0..=1` at a reduced scale;
## `Placement.scaled` maps the result back to full resolution.

import std/[math, options, algorithm]
import ./[util, transform, features]

type
  Layout* = enum
    ## Photomerge layouts.
    lyAuto = "auto", lyPerspective = "perspective", lyCylindrical = "cylindrical", lySpherical = "spherical",
    lyCollage = "collage", lyReposition = "reposition"

  Projection* = enum
    ## Surface an image is projected onto before its 2-D motion.
    prPlane, prCylinder, prSphere

  Motion* = enum
    ## 2-D motion models in projected space.
    moTranslation
    moEuclidean  ## rotation + translation
    moSimilarity ## rotation, uniform scale, translation
    moHomography

  Placement* = object
    ## Where one image lands in the panorama: image px → (undistort) → projection → 2-D motion.
    projection*: Projection
    focal*: float64            ## focal length in px (the projection radius)
    center*: array[2, float64] ## projection centre in image px (the image centre)
    k1*: float64               ## radial distortion on radii normalised by the focal length
    h*: Homography             ## projected coordinates → panorama coordinates

  ImageFeatures* = object
    ## Keypoints and descriptors of one image (registration scale).
    w*, h*: int
    points*: seq[array[2, float64]]
    desc*: seq[Descriptor]

  PreparedImage* = (int, int, seq[float32], Option[seq[bool]]) ## (w, h, luma, valid)

  PairMatch* = object
    ## A verified image pair: point correspondences (image `a` px, image `b` px).
    a*, b*: int
    ptsA*, ptsB*: seq[array[2, float64]]
    homography*: Homography ## `b → a` in image px

  AlignOptions* = object
    layout*: Layout
    focal*: Option[float64]   ## focal length in registration px (EXIF); estimated when none
    reference*: Option[int]   ## image that stays put (none: the best-connected one)
    geometric*: bool          ## refine a radial distortion coefficient

  Alignment* = object
    layout*: Layout           ## the layout actually used (Auto resolved)
    reference*: int
    placements*: seq[Option[Placement]] ## none for images that couldn't be connected
    focal*, k1*: float64
    pairs*: seq[(int, int, int)] ## verified pairs (a, b, inliers)
    rms*: float64             ## RMS reprojection error (px, registration scale) after bundle adjustment

  RoiImage* = object
    ## One image warped into the panorama: an axis-aligned region with colour channels and
    ## coverage.
    x0*, y0*: int
    w*, h*: int
    ch*: int             ## colour channels per pixel (no alpha)
    px*: seq[float32]
    alpha*: seq[float32] ## coverage 0..=1

  Photometric* = object
    ## Gains (per image, per channel) and the vignetting model found by `photometric`.
    gains*: seq[seq[float32]]
    vignette*: array[2, float64] ## `log V(r) = a·r² + b·r⁴`

  PairData = object
    a, b: int
    pa, pb: seq[array[2, float64]]

proc parseLayout*(s: string): Option[Layout] =
  for l in Layout:
    if $l == s: return some(l)
  none(Layout)

proc name*(l: Layout): string = $l

proc projection(l: Layout): Projection =
  case l
  of lyCylindrical: prCylinder
  of lySpherical: prSphere
  else: prPlane

proc motion(l: Layout): Motion =
  case l
  of lyAuto, lyPerspective: moHomography
  of lyCylindrical, lySpherical: moEuclidean
  of lyCollage: moSimilarity
  of lyReposition: moTranslation

proc minPoints(m: Motion): int =
  case m
  of moTranslation: 1
  of moEuclidean, moSimilarity: 2
  of moHomography: 4

proc nParams(m: Motion): int =
  case m
  of moTranslation: 2
  of moEuclidean: 3
  of moSimilarity: 4
  of moHomography: 8

proc toParams(mo: Motion; h: Homography): seq[float64] =
  var m: array[9, float64]
  for i in 0 .. 8: m[i] = h.m[i] / h.m[8]
  case mo
  of moTranslation: @[m[2], m[5]]
  of moEuclidean: @[arctan2(m[3], m[0]), m[2], m[5]]
  of moSimilarity: @[m[0], m[3], m[2], m[5]]
  of moHomography: @(m.toOpenArray(0, 7))

proc toHomography(mo: Motion; p: openArray[float64]): Homography =
  case mo
  of moTranslation: homography([1.0, 0.0, p[0], 0.0, 1.0, p[1], 0.0, 0.0, 1.0])
  of moEuclidean:
    let (s, c) = (sin(p[0]), cos(p[0]))
    homography([c, -s, p[1], s, c, p[2], 0.0, 0.0, 1.0])
  of moSimilarity: homography([p[0], -p[1], p[2], p[1], p[0], p[3], 0.0, 0.0, 1.0])
  of moHomography: homography([p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], 1.0])

proc fitMotion*(motion: Motion; src, dst: openArray[array[2, float64]]): Option[Homography] =
  ## Least-squares fit of `motion` mapping `src[i]` → `dst[i]`.
  case motion
  of moTranslation: fit(mTranslation, src, dst)
  of moSimilarity: fit(mSimilarity, src, dst)
  of moHomography: fit(mHomography, src, dst)
  of moEuclidean:
    let n = min(src.len, dst.len)
    if n < 2: return none(Homography)
    let nf = float64(n)
    var cs, cd = [0.0, 0.0]
    for i in 0 ..< n:
      cs = [cs[0] + src[i][0] / nf, cs[1] + src[i][1] / nf]
      cd = [cd[0] + dst[i][0] / nf, cd[1] + dst[i][1] / nf]
    var (sxx, sxy) = (0.0, 0.0)
    for i in 0 ..< n:
      let (a, b) = ([src[i][0] - cs[0], src[i][1] - cs[1]], [dst[i][0] - cd[0], dst[i][1] - cd[1]])
      sxx += a[0] * b[0] + a[1] * b[1]
      sxy += a[0] * b[1] - a[1] * b[0]
    let th = arctan2(sxy, sxx)
    let (s, c) = (sin(th), cos(th))
    some(homography([c, -s, cd[0] - (c * cs[0] - s * cs[1]), s, c, cd[1] - (s * cs[0] + c * cs[1]), 0.0, 0.0, 1.0]))

proc ransacMotion*(motion: Motion; src, dst: openArray[array[2, float64]]; iters: int; threshold: float64;
                   seed: uint64): Option[(Homography, seq[int])] =
  ## RANSAC over `Motion` models; returns the refit model and its inlier indices.
  let k = motion.minPoints
  let n = min(src.len, dst.len)
  if n < k: return none((Homography, seq[int]))
  var rng = newRng(seed xor 0x5EED_0FA1'u64)
  template inliersOf(h: Homography): seq[int] = ransacInliers(h, src, dst, n, threshold)
  var best = none((Homography, seq[int]))
  for _ in 0 ..< max(iters, 1):
    var idx = newSeqOfCap[int](k)
    if n == k:
      for q in 0 ..< k: idx.add q
    while idx.len < k:
      let i = rng.below(n)
      if i notin idx: idx.add i
    var s, d: seq[array[2, float64]]
    for i in idx:
      s.add src[i]
      d.add dst[i]
    let h = fitMotion(motion, s, d)
    if h.isNone: continue
    let inl = inliersOf(h.get)
    if best.isNone or inl.len > best.get[1].len: best = some((h.get, inl))
  if best.isNone: return none((Homography, seq[int]))
  let (h, inl) = best.get
  if inl.len < k: return none((Homography, seq[int]))
  var s, d: seq[array[2, float64]]
  for i in inl:
    s.add src[i]
    d.add dst[i]
  let h2 = fitMotion(motion, s, d).get(h)
  let inl2 = inliersOf(h2)
  some(if inl2.len >= inl.len: (h2, inl2) else: (h, inl))

# ---------------------------------------------------------------- placement

proc project*(p: Placement; x, y: float64): array[2, float64] =
  ## Image px → projected coordinates (centred, in px of the focal length).
  var (dx, dy) = (x - p.center[0], y - p.center[1])
  if p.k1 != 0.0:
    let s = 1.0 + p.k1 * (dx * dx + dy * dy) / (p.focal * p.focal)
    dx *= s
    dy *= s
  let f = p.focal
  case p.projection
  of prPlane: [dx, dy]
  of prCylinder: [f * arctan2(dx, f), f * dy / hypot(dx, f)]
  of prSphere: [f * arctan2(dx, f), f * arctan2(dy, hypot(dx, f))]

proc unproject*(p: Placement; a, b: float64): Option[(float64, float64)] =
  ## Projected → image px (none outside the valid hemisphere).
  let f = p.focal
  var dx, dy: float64
  case p.projection
  of prPlane: (dx, dy) = (a, b)
  of prCylinder:
    let th = a / f
    if abs(th) >= 1.55: return none((float64, float64))
    (dx, dy) = (f * tan(th), b / cos(th))
  of prSphere:
    let (th, ph) = (a / f, b / f)
    if abs(th) >= 1.55 or abs(ph) >= 1.55: return none((float64, float64))
    (dx, dy) = (f * tan(th), f * tan(ph) / cos(th))
  if p.k1 != 0.0:
    # Invert r_u = r_d (1 + k1 r_d² / f²) by Newton steps on r_d.
    let ru = hypot(dx, dy)
    if ru > 1e-9:
      var rd = ru
      for _ in 0 ..< 8:
        let g = rd * (1.0 + p.k1 * rd * rd / (f * f)) - ru
        let dg = 1.0 + 3.0 * p.k1 * rd * rd / (f * f)
        if abs(dg) < 1e-9: break
        rd -= g / dg
      if not (rd == rd and rd - rd == 0.0 and rd > 0.0): return none((float64, float64))
      dx *= rd / ru
      dy *= rd / ru
  some((dx + p.center[0], dy + p.center[1]))

proc forward*(p: Placement; x, y: float64): (float64, float64) =
  ## Image px → panorama px.
  let q = p.project(x, y)
  p.h.apply(q[0], q[1])

proc inverse*(p: Placement; u, v: float64): Option[(float64, float64)] =
  ## Panorama px → image px.
  let inv = p.h.inverse
  if inv.isNone: return none((float64, float64))
  let m = inv.get.m
  let w = m[6] * u + m[7] * v + m[8]
  if w <= 1e-12: return none((float64, float64))
  let (a, b) = inv.get.apply(u, v)
  p.unproject(a, b)

proc scaled*(p: Placement; k: float64): Placement =
  ## The same placement for images `k×` larger (registration ran at `1/k` scale).
  let s = homography([k, 0.0, 0.0, 0.0, k, 0.0, 0.0, 0.0, 1.0])
  let si = homography([1.0 / k, 0.0, 0.0, 0.0, 1.0 / k, 0.0, 0.0, 0.0, 1.0])
  Placement(projection: p.projection, focal: p.focal * k, center: [p.center[0] * k, p.center[1] * k], k1: p.k1,
            h: s.mul(p.h).mul(si))

proc translated*(p: Placement; dx, dy: float64): Placement =
  ## Shifted in the panorama.
  result = p
  result.h = homography([1.0, 0.0, dx, 0.0, 1.0, dy, 0.0, 0.0, 1.0]).mul(p.h)

proc bounds*(p: Placement; w, h: float64): array[4, float64] =
  ## Bounds `[x0, y0, x1, y1]` of a `w × h` image in the panorama (sampled along its edges).
  result = [F64Max, F64Max, -F64Max, -F64Max]
  let n = 32
  for i in 0 .. n:
    let t = float64(i) / float64(n)
    for (x, y) in [(t * w, 0.0), (t * w, h), (0.0, t * h), (w, t * h)]:
      let (u, v) = p.forward(x, y)
      if u == u and u - u == 0.0 and v == v and v - v == 0.0:
        result = [rmin(result[0], u), rmin(result[1], v), rmax(result[2], u), rmax(result[3], v)]

# ---------------------------------------------------------------- features and matching

proc detect*(w, h: int; luma: openArray[float32]; valid: Option[seq[bool]]; max: int): ImageFeatures =
  ## Detects up to `max` corners of a `w × h` luminance image and describes them.
  let minDist = clamp(float32(max(w, h)) / 70'f32, 3'f32, 20'f32)
  let corners = harris(w, h, luma, max, minDist, valid)
  result = ImageFeatures(w: w, h: h, desc: describe(w, h, luma, corners, false))
  for c in corners: result.points.add [float64(c.x), float64(c.y)]

proc detectMany*(imgs: openArray[PreparedImage]; max: int): seq[ImageFeatures] =
  ## `detect` for several images.
  for (w, h, l, v) in imgs: result.add detect(w, h, l, v, max)

proc matchPairs*(feats: openArray[ImageFeatures]): seq[PairMatch] =
  ## Matches every pair and keeps those that pass RANSAC + the Brown–Lowe verification.
  let n = feats.len
  for a in 0 ..< n:
    for b in a + 1 ..< n:
      let (fa, fb) = (feats[a], feats[b])
      if fa.points.len < 6 or fb.points.len < 6: continue
      let m = matchDescriptors(fb.desc, fa.desc, 0.8'f32)
      if m.len < 8: continue
      var src, dst: seq[array[2, float64]]
      for (i, j) in m:
        if i >= fb.points.len or j >= fa.points.len: continue # descriptors without points
        src.add fb.points[i]
        dst.add fa.points[j]
      let thr = clamp(float64(max(fa.w, fa.h)) / 300.0, 1.5, 4.0)
      let r = ransacMotion(moHomography, src, dst, 1200, thr, uint64(a * 131 + b))
      if r.isNone: continue
      let (hm, inl) = r.get
      if float64(inl.len) < 8.0 + 0.3 * float64(m.len) or inl.len < 10: continue
      var pm = PairMatch(a: a, b: b, homography: hm)
      for i in inl:
        pm.ptsA.add dst[i]
        pm.ptsB.add src[i]
      result.add pm

proc focalFromHomography*(h: Homography; ca, cb: array[2, float64]): Option[float64] =
  ## Focal length (px) of a rotating camera from a homography between two of its images (image
  ## px, centred at `ca` / `cb`), or none when the homography doesn't constrain it.
  let ta = homography([1.0, 0.0, -ca[0], 0.0, 1.0, -ca[1], 0.0, 0.0, 1.0])
  let tb = homography([1.0, 0.0, cb[0], 0.0, 1.0, cb[1], 0.0, 0.0, 1.0])
  let m = ta.mul(h).mul(tb).m
  proc ok(c: (float64, float64)): Option[(float64, float64)] =
    let (n, d) = c
    if abs(d) > 1e-12 and n / d > 0.0: some((n / d, abs(d))) else: none((float64, float64))
  proc pick(c1, c2: (float64, float64)): Option[float64] =
    # Each candidate is (numerator, denominator) of f²; prefer the better-conditioned one.
    let (a, b) = (ok(c1), ok(c2))
    if a.isSome and b.isSome: some(if a.get[1] >= b.get[1]: a.get[0] else: b.get[0])
    elif a.isSome: some(a.get[0])
    elif b.isSome: some(b.get[0])
    else: none(float64)
  # Columns of K⁻¹HK orthogonal and of equal norm (focal of b) …
  let f1 = pick((-(m[0] * m[1] + m[3] * m[4]), m[6] * m[7]),
                (m[1] * m[1] + m[4] * m[4] - m[0] * m[0] - m[3] * m[3], m[6] * m[6] - m[7] * m[7]))
  # … rows likewise (focal of a).
  let f0 = pick((-(m[2] * m[5]), m[0] * m[3] + m[1] * m[4]),
                (m[5] * m[5] - m[2] * m[2], m[0] * m[0] + m[1] * m[1] - m[3] * m[3] - m[4] * m[4]))
  if f0.isSome and f1.isSome: some(sqrt(sqrt(f0.get * f1.get)))
  elif f0.isSome: some(sqrt(f0.get))
  elif f1.isSome: some(sqrt(f1.get))
  else: none(float64)

proc basePlacement(f: ImageFeatures; projection: Projection; focal, k1: float64): Placement =
  Placement(projection: projection, focal: focal, center: [float64(f.w) / 2.0, float64(f.h) / 2.0], k1: k1, h: IdentityHomography)

# ---------------------------------------------------------------- alignment

proc alignLayout(feats: openArray[ImageFeatures]; matches: openArray[PairMatch]; opts: AlignOptions; layout: Layout;
                 focal: float64): Option[Alignment] =
  let feats = @feats # captured by the bundle-adjustment closures
  let n = feats.len
  let projection = layout.projection
  let motion = layout.motion
  # Pairwise motions in projected space.
  var pairs: seq[PairData]
  var edges: seq[(int, int, int, Homography)]
  for m in matches:
    if m.a >= n or m.b >= n: continue
    let (pa, pb) = (basePlacement(feats[m.a], projection, focal, 0.0), basePlacement(feats[m.b], projection, focal, 0.0))
    var qa, qb: seq[array[2, float64]]
    for p in m.ptsA: qa.add pa.project(p[0], p[1])
    for p in m.ptsB: qb.add pb.project(p[0], p[1])
    let thr = clamp(float64(max(feats[m.a].w, feats[m.a].h)) / 250.0, 2.0, 5.0)
    let r = ransacMotion(motion, qb, qa, 600, thr, uint64(m.a * 977 + m.b))
    if r.isNone: continue
    let (hm, inl) = r.get
    if inl.len < 8 or float64(inl.len) < 0.5 * float64(m.ptsA.len): continue
    edges.add (m.a, m.b, inl.len, hm)
    # Bundle adjustment uses at most 80 evenly spread inliers per pair.
    let step = max((inl.len + 79) div 80, 1)
    var pd = PairData(a: m.a, b: m.b)
    var q = 0
    while q < inl.len:
      pd.pa.add m.ptsA[inl[q]]
      pd.pb.add m.ptsB[inl[q]]
      q += step
    pairs.add pd
  if edges.len == 0: return none(Alignment)
  var degree = newSeq[int](n)
  for e in edges:
    degree[e[0]] += e[2]
    degree[e[1]] += e[2]
  var reference = 0
  if opts.reference.isSome and opts.reference.get >= 0 and opts.reference.get < n: reference = opts.reference.get
  else:
    # Best connected; ties go to the middle image (`max_by` keeps the last of equal maxima).
    proc cmpRef(i, j: int): int =
      let c = cmp(degree[i], degree[j])
      if c != 0: c else: cmp(abs(j - n div 2), abs(i - n div 2))
    for i in 1 ..< n:
      if cmpRef(i, reference) >= 0: reference = i
  # Maximum spanning tree (Prim) from the reference; transforms map projected → panorama.
  var tf = newSeq[Option[Homography]](n)
  let rc = [float64(feats[reference].w) / 2.0, float64(feats[reference].h) / 2.0]
  tf[reference] = some(homography([1.0, 0.0, rc[0], 0.0, 1.0, rc[1], 0.0, 0.0, 1.0]))
  while true:
    var best = none((int, int, Homography))
    for e in edges:
      # e[3] maps b → a.
      var cand = none((int, Homography))
      if tf[e[0]].isSome and tf[e[1]].isNone: cand = some((e[1], tf[e[0]].get.mul(e[3])))
      elif tf[e[0]].isNone and tf[e[1]].isSome:
        let inv = e[3].inverse
        if inv.isSome: cand = some((e[0], tf[e[1]].get.mul(inv.get)))
      if cand.isSome and (best.isNone or e[2] > best.get[1]): best = some((cand.get[0], e[2], cand.get[1]))
    if best.isNone: break
    tf[best.get[0]] = some(best.get[2])
  var placed: seq[int]
  for i in 0 ..< n:
    if tf[i].isSome: placed.add i
  if placed.len < 2: return none(Alignment)
  # Bundle adjustment (Levenberg–Marquardt, numeric Jacobian).
  var movable: seq[int]
  for i in placed:
    if i != reference: movable.add i
  let mp = motion.nParams
  let refineFocal = projection != prPlane
  let nGlob = int(opts.geometric) + int(refineFocal)
  var params: seq[float64]
  for i in movable: params.add motion.toParams(tf[i].get)
  if opts.geometric: params.add 0.0
  if refineFocal: params.add ln(focal)
  let nPar = params.len
  proc decode(p: seq[float64]): seq[Option[Placement]] =
    let k1 = (if opts.geometric: p[movable.len * mp] else: 0.0)
    let f = (if refineFocal: exp(p[nPar - 1]) else: focal)
    result = newSeq[Option[Placement]](n)
    for i in 0 ..< n:
      var h: Homography
      if i == reference:
        if tf[reference].isNone: continue
        h = tf[reference].get
      else:
        let k = movable.find(i)
        if k < 0: continue
        h = motion.toHomography(p.toOpenArray(k * mp, (k + 1) * mp - 1))
      var pl = basePlacement(feats[i], projection, f, k1)
      pl.h = h
      result[i] = some(pl)
  proc residuals(p: seq[float64]): seq[float64] =
    let pl = decode(p)
    for pd in pairs:
      if pl[pd.a].isNone or pl[pd.b].isNone: continue
      let (a, b) = (pl[pd.a].get, pl[pd.b].get)
      for k in 0 ..< min(pd.pa.len, pd.pb.len):
        let (ua, va) = a.forward(pd.pa[k][0], pd.pa[k][1])
        let (ub, vb) = b.forward(pd.pb[k][0], pd.pb[k][1])
        result.add ua - ub
        result.add va - vb
    # Soft priors keep the global terms sane when few pairs constrain them.
    if opts.geometric: result.add p[movable.len * mp] * 20.0
    if refineFocal: result.add (p[nPar - 1] - ln(focal)) * 2.0
  proc cost(r: openArray[float64]): float64 =
    result = -0.0
    for v in r: result += v * v
  var cur = residuals(params)
  var lambda = 1e-3
  if nPar > 0 and movable.len > 0:
    for _ in 0 ..< 40:
      let mRes = cur.len
      var cols = newSeq[seq[float64]](nPar)
      for j in 0 ..< nPar:
        let step = 1e-6 * (1.0 + abs(params[j]))
        var pp = params
        pp[j] += step
        let rp = residuals(pp)
        pp[j] -= 2.0 * step
        let rm = residuals(pp)
        cols[j] = newSeq[float64](mRes)
        for i in 0 ..< mRes: cols[j][i] = (rp[i] - rm[i]) / (2.0 * step)
      var jtj = newSeq[float64](nPar * nPar)
      var jtr = newSeq[float64](nPar)
      for a in 0 ..< nPar:
        var s = -0.0
        for i in 0 ..< min(cols[a].len, cur.len): s += cols[a][i] * cur[i]
        jtr[a] = -s
        for b in a ..< nPar:
          var v = -0.0
          for i in 0 ..< min(cols[a].len, cols[b].len): v += cols[a][i] * cols[b][i]
          jtj[a * nPar + b] = v
          jtj[b * nPar + a] = v
      var improved = false
      for _ in 0 ..< 8:
        var am = jtj
        for d in 0 ..< nPar: am[d * nPar + d] += lambda * (jtj[d * nPar + d] + 1e-9)
        let delta = solveDense(nPar, am, jtr)
        if delta.len == 0:
          lambda *= 10.0
          continue
        var trial = newSeq[float64](nPar)
        for i in 0 ..< nPar: trial[i] = params[i] + delta[i]
        let rt = residuals(trial)
        if cost(rt) < cost(cur):
          let gain = cost(cur) - cost(rt)
          params = trial
          cur = rt
          lambda = rmax(lambda * 0.3, 1e-9)
          improved = gain > 1e-10 * rmax(cost(cur), 1e-12)
          break
        lambda *= 10.0
      if not improved: break
  let placements = decode(params)
  var nRes = 0
  for p in pairs: nRes += p.pa.len
  nRes = max(nRes, 1)
  let rms = sqrt(cost(cur.toOpenArray(0, max(cur.len - nGlob, 0) - 1)) / float64(nRes))
  let k1 = (if opts.geometric: params[movable.len * mp] else: 0.0)
  let focalOut = (if refineFocal: exp(params[nPar - 1]) else: focal)
  var pr: seq[(int, int, int)]
  for e in edges: pr.add (e[0], e[1], e[2])
  some(Alignment(layout: layout, reference: reference, placements: placements, focal: focalOut, k1: k1, pairs: pr, rms: rms))

proc tooStretched(feats: openArray[ImageFeatures]; a: Alignment): bool =
  ## A perspective panorama is too stretched when an image grows or shrinks a lot, flips, or the
  ## field of view exceeds about 110°.
  var (x0, x1) = (F64Max, -F64Max)
  for i in 0 ..< min(a.placements.len, feats.len):
    if a.placements[i].isNone: continue
    let p = a.placements[i].get
    let (w, h) = (float64(feats[i].w), float64(feats[i].h))
    let corners = [(0.0, 0.0), (w, 0.0), (w, h), (0.0, h)]
    var c: array[4, (float64, float64)]
    for k, q in corners: c[k] = p.forward(q[0], q[1])
    # Signed area (shoelace); orientation must be preserved.
    var area = -0.0
    for k in 0 .. 3: area += c[k][0] * c[(k + 1) mod 4][1] - c[(k + 1) mod 4][0] * c[k][1]
    area /= 2.0
    let ratio = area / (w * h)
    if not (ratio == ratio and ratio - ratio == 0.0) or not (ratio >= 0.3 and ratio <= 3.0): return true
    let m = p.h.m
    for q in corners:
      let pq = p.project(q[0], q[1])
      if m[6] * pq[0] + m[7] * pq[1] + m[8] <= 0.0: return true
    for (u, _) in c:
      x0 = rmin(x0, u)
      x1 = rmax(x1, u)
  # Horizontal field of view through the reference's focal length.
  let fov = 2.0 * arctan((x1 - x0) / 2.0 / a.focal)
  fov > toRadians(110.0)

proc align*(feats: openArray[ImageFeatures]; matches: openArray[PairMatch]; opts: AlignOptions): Option[Alignment] =
  ## Aligns images from their features. None when fewer than two images connect.
  let n = feats.len
  if n < 2 or matches.len == 0: return none(Alignment)
  var focal: float64
  if opts.focal.isSome and opts.focal.get > 1.0: focal = opts.focal.get
  else:
    var fs: seq[float64]
    for m in matches:
      if m.a >= n or m.b >= n: continue
      let (fa, fb) = (feats[m.a], feats[m.b])
      let f = focalFromHomography(m.homography, [float64(fa.w) / 2.0, float64(fa.h) / 2.0], [float64(fb.w) / 2.0, float64(fb.h) / 2.0])
      if f.isSome and f.get == f.get and f.get - f.get == 0.0: fs.add f.get
    var side = 1
    for f in feats: side = max(side, max(f.w, f.h))
    let sidef = float64(side)
    var kept: seq[float64]
    for f in fs:
      if f > sidef * 0.2 and f < sidef * 20.0: kept.add f
    kept.sort(proc(a, b: float64): int = totalCmp(a, b))
    focal = (if kept.len == 0: sidef else: kept[kept.len div 2])
  if opts.layout != lyAuto: return alignLayout(feats, matches, opts, opts.layout, focal)
  # Auto: perspective unless it is too stretched, then cylindrical, or spherical when the
  # panorama is also tall.
  let persp = alignLayout(feats, matches, opts, lyPerspective, focal)
  if persp.isSome and not tooStretched(feats, persp.get):
    var r = persp.get
    r.layout = lyPerspective
    return some(r)
  let cyl = alignLayout(feats, matches, opts, lyCylindrical, focal)
  if cyl.isNone: return none(Alignment)
  var (y0, y1) = (F64Max, -F64Max)
  for i in 0 ..< min(cyl.get.placements.len, feats.len):
    if cyl.get.placements[i].isSome:
      let b = cyl.get.placements[i].get.bounds(float64(feats[i].w), float64(feats[i].h))
      y0 = rmin(y0, b[1])
      y1 = rmax(y1, b[3])
  if (y1 - y0) / cyl.get.focal > 1.4: return alignLayout(feats, matches, opts, lySpherical, cyl.get.focal)
  cyl

# ---------------------------------------------------------------- photometric

proc usable(im: RoiImage; ch: int): bool =
  ## Whether the ROI's buffers hold its `w × h` pixels of `ch` channels (hostile input guard).
  im.w >= 0 and im.h >= 0 and im.alpha.len >= im.w * im.h and im.px.len >= im.w * im.h * ch

proc at(im: RoiImage; x, y: int): Option[int] =
  let (lx, ly) = (x - im.x0, y - im.y0)
  if lx >= 0 and ly >= 0 and lx < im.w and ly < im.h: some(ly * im.w + lx) else: none(int)

proc factor*(p: Photometric; i, c: int; r2: float64): float32 =
  ## Multiplier that corrects a pixel of image `i` at normalised squared radius `r2`.
  let v = exp(p.vignette[0] * r2 + p.vignette[1] * r2 * r2)
  float32(1.0 / (float64(p.gains[i][c]) * v))

proc photometric*(imgs: openArray[RoiImage]; placements: Option[seq[Placement]]; sizes: seq[(int, int)];
                  gain, vignette0: bool): Photometric =
  ## Estimates exposure gains (`gain`) and a shared vignetting falloff (`vignette`) from the
  ## overlaps of `imgs` (`placements`/`sizes` give each panorama pixel's source radius). Without
  ## placements the vignetting model is not estimated.
  let vignette = vignette0 and placements.isSome
  let n = imgs.len
  let ch = (if n > 0: max(imgs[0].ch, 0) else: 1)
  var identity = Photometric(gains: newSeq[seq[float32]](n), vignette: [0.0, 0.0])
  for g in identity.gains.mitems:
    g = newSeq[float32](ch)
    for v in g.mitems: v = 1
  if n == 0 or (not gain and not vignette): return identity
  # Overlap samples on a grid: (a, b, colours, radii²).
  var (x0, y0, x1, y1) = (high(int32).int, high(int32).int, low(int32).int, low(int32).int)
  for im in imgs:
    x0 = min(x0, im.x0)
    y0 = min(y0, im.y0)
    x1 = max(x1, im.x0 + im.w)
    y1 = max(y1, im.y0 + im.h)
  let step = int(ceil(sqrt(float64(max(int64(x1 - x0) * int64(y1 - y0) div 40_000, 1'i64)))))
  proc r2Of(i, u, v: int): Option[float64] =
    if placements.isNone: return some(0.0)
    if i >= placements.get.len or i >= sizes.len: return none(float64)
    let s = placements.get[i].inverse(float64(u) + 0.5, float64(v) + 0.5)
    if s.isNone: return none(float64)
    let (sx, sy) = s.get
    let (w, h) = (float64(sizes[i][0]), float64(sizes[i][1]))
    let (dx, dy) = (sx - w / 2.0, sy - h / 2.0)
    some((dx * dx + dy * dy) / (w * w / 4.0 + h * h / 4.0))
  var samples: seq[(int, int, seq[float32], seq[float32], float64, float64)]
  var y = y0
  while y < y1:
    var x = x0
    while x < x1:
      var cover: seq[(int, int)]
      for i in 0 ..< n:
        if not imgs[i].usable(ch): continue
        let k = imgs[i].at(x, y)
        if k.isSome and imgs[i].alpha[k.get] > 0.999'f32: cover.add (i, k.get)
      for p in 0 ..< cover.len:
        for q in p + 1 ..< cover.len:
          let ((a, ka), (b, kb)) = (cover[p], cover[q])
          let ca = imgs[a].px[ka * ch ..< (ka + 1) * ch]
          let cb = imgs[b].px[kb * ch ..< (kb + 1) * ch]
          var skip = false
          for v in ca & cb:
            if not (v >= 0.02'f32 and v <= 0.98'f32): skip = true
          if skip: continue
          # Disagreeing content (parallax, moving objects) says nothing about exposure.
          for c in 0 ..< min(ca.len, cb.len):
            if abs(ln(ca[c] / cb[c])) > 1'f32: skip = true
          if skip: continue
          let (ra, rb) = (r2Of(a, x, y), r2Of(b, x, y))
          if ra.isNone or rb.isNone: continue
          samples.add (a, b, ca, cb, ra.get, rb.get)
      x += step
    y += step
  if samples.len < 20: return identity
  # Unknowns: log gain per image (per channel), then a, b (shared, from mean log intensity).
  let sigmaN = 0.05
  let sigmaG = (if gain: 0.15 else: 1e-4)
  result = identity
  let nv = (if vignette: 2 else: 0)
  # Vignetting is solved on the channel mean, then gains per channel with it fixed.
  var ne = newNormal(n + nv)
  for (a, b, ca, cb, ra, rb) in samples:
    var la, lb = -0.0
    for v in ca: la += ln(float64(v))
    for v in cb: lb += ln(float64(v))
    la /= float64(ch)
    lb /= float64(ch)
    var row = @[(a, 1.0), (b, -1.0)]
    if vignette:
      row.add (n, ra - rb)
      row.add (n + 1, ra * ra - rb * rb)
    ne.add(row, la - lb, 1.0 / sigmaN)
  for i in 0 ..< n: ne.add([(i, 1.0)], 0.0, 1.0 / sigmaG)
  if vignette:
    ne.add([(n, 1.0)], 0.0, 0.5)
    ne.add([(n + 1, 1.0)], 0.0, 0.5)
  let sol = ne.solve()
  if sol.len == 0: return
  if vignette:
    # Vignetting only darkens; clamp a runaway brightening fit.
    result.vignette = [clamp(sol[n], -2.0, 0.5), clamp(sol[n + 1], -2.0, 2.0)]
  if gain:
    for c in 0 ..< ch:
      var ne2 = newNormal(n)
      for (a, b, ca, cb, ra, rb) in samples:
        let va = result.vignette[0] * ra + result.vignette[1] * ra * ra
        let vb = result.vignette[0] * rb + result.vignette[1] * rb * rb
        ne2.add([(a, 1.0), (b, -1.0)], (ln(float64(ca[c])) - va) - (ln(float64(cb[c])) - vb), 1.0 / sigmaN)
      for i in 0 ..< n: ne2.add([(i, 1.0)], 0.0, 1.0 / sigmaG)
      let g = ne2.solve()
      if g.len > 0:
        for i in 0 ..< n: result.gains[i][c] = float32(clamp(exp(g[i]), 0.2, 5.0))

# ---------------------------------------------------------------- seams

proc seamLabels*(w, h: int; luma: openArray[seq[float32]]; cover: openArray[seq[bool]]; order: openArray[int]): seq[int32] =
  ## Seam labels on a `w × h` grid (`-1` = no image): images are added in `order`; each new image
  ## takes the part of its overlap with the composite that lies on its side of the cheapest path
  ## through the overlap (cost = colour difference).
  if w <= 0 or h <= 0: return @[]
  result = newSeq[int32](w * h)
  for v in result.mitems: v = -1
  for img in order:
    if img < 0 or img >= cover.len or img >= luma.len or cover[img].len < w * h or luma[img].len < w * h: continue
    let cov = cover[img]
    var overlap = newSeq[bool](w * h)
    var (sx, sy, no) = (0.0, 0.0, 0)
    var (nx, ny, nn) = (0.0, 0.0, 0)
    for i in 0 ..< w * h:
      if not cov[i]: continue
      nx += float64(i mod w)
      ny += float64(i div w)
      inc nn
      if result[i] >= 0:
        overlap[i] = true
        sx += float64(i mod w)
        sy += float64(i div w)
        inc no
      else: result[i] = int32(img)
    if no == 0 or nn == no: continue
    let (ox, oy) = (sx / float64(no), sy / float64(no))
    let (cx, cy) = (nx / float64(nn), ny / float64(nn))
    let vertical = abs(cx - ox) >= abs(cy - oy)
    # Image on the increasing side along the seam's cross axis?
    let positive = (if vertical: cx > ox else: cy > oy)
    template cost(i: int): float32 = 0.002'f32 + abs(luma[img][i] - luma[int(result[i])][i])
    let (lines, len) = (if vertical: (h, w) else: (w, h))
    template idx(line, k: int): int = (if vertical: line * w + k else: k * w + line)
    # DP over lines (rows for a vertical seam), within the overlap.
    var acc = newSeq[float32](w * h)
    for v in acc.mitems: v = Inf.float32
    var frm = newSeq[int](w * h)
    for v in frm.mitems: v = high(int)
    var prevLine = -1
    for line in 0 ..< lines:
      var any = false
      for k in 0 ..< len:
        if overlap[idx(line, k)]:
          any = true
          break
      if not any: continue
      for k in 0 ..< len:
        let i = idx(line, k)
        if not overlap[i]: continue
        var best = (0'f32, high(int))
        if prevLine >= 0 and prevLine + 1 == line:
          best = (Inf.float32, high(int))
          for dk in [-1, 0, 1]:
            let kk = k + dk
            if kk < 0 or kk >= len: continue
            let j = idx(prevLine, kk)
            if acc[j] < best[0]: best = (acc[j], kk)
          if best[1] == high(int): best = (0'f32, high(int))
        acc[i] = best[0] + cost(i)
        frm[i] = best[1]
      prevLine = line
    # Backtrack from the cheapest end; lines not reached keep their own best cell.
    var seam = newSeq[int](lines)
    for v in seam.mitems: v = high(int)
    var line = lines
    while line > 0:
      dec line
      if seam[line] != high(int): continue
      var kbest = -1
      for k in 0 ..< len:
        if overlap[idx(line, k)] and (kbest < 0 or totalCmp(acc[idx(line, k)], acc[idx(line, kbest)]) < 0): kbest = k
      if kbest < 0: continue
      var (l, k) = (line, kbest)
      while true:
        seam[l] = k
        let f = frm[idx(l, k)]
        if f == high(int) or l == 0: break
        dec l
        if seam[l] != high(int): break
        k = f
    for ln2 in 0 ..< lines:
      let s = seam[ln2]
      if s == high(int): continue
      for k in 0 ..< len:
        let i = idx(ln2, k)
        if overlap[i] and ((positive and k > s) or (not positive and k < s)): result[i] = int32(img)

# ---------------------------------------------------------------- multi-band blending

proc blendLevels*(w, h: int): int =
  ## Number of pyramid levels used by `multiband` for a seam mask of this size.
  var s = min(w, h)
  result = 1
  while s > 16 and result < 7:
    s = s div 2
    inc result

let BinomK = [1'f32, 4'f32, 6'f32, 4'f32, 1'f32]

proc reduceL(w, h, ch: int; src: openArray[float32]): (int, int, seq[float32]) =
  let (nw, nh) = ((w + 1) div 2, (h + 1) div 2)
  # Separable 5-tap binomial, then decimate.
  var tmp = newSeq[float32](nw * h * ch)
  for y in 0 ..< h:
    for x in 0 ..< nw:
      for c in 0 ..< ch:
        var s = 0'f32
        for t, kv in BinomK:
          let xx = clamp(2 * x + t - 2, 0, w - 1)
          s += kv * src[(y * w + xx) * ch + c]
        tmp[(y * nw + x) * ch + c] = s / 16'f32
  var o = newSeq[float32](nw * nh * ch)
  for y in 0 ..< nh:
    for x in 0 ..< nw:
      for c in 0 ..< ch:
        var s = 0'f32
        for t, kv in BinomK:
          let yy = clamp(2 * y + t - 2, 0, h - 1)
          s += kv * tmp[(yy * nw + x) * ch + c]
        o[(y * nw + x) * ch + c] = s / 16'f32
  (nw, nh, o)

proc expandL(sw, sh, ch: int; src: openArray[float32]; w, h: int): seq[float32] =
  ## Bilinear upsampling at half-pixel-aligned positions.
  result = newSeq[float32](w * h * ch)
  for y in 0 ..< h:
    let fy = clamp((float32(y) + 0.5'f32) / 2'f32 - 0.5'f32, 0'f32, float32(sh - 1))
    let (y0, ty) = (int(floor(fy)), fy - floor(fy))
    let y1 = min(y0 + 1, sh - 1)
    for x in 0 ..< w:
      let fx = clamp((float32(x) + 0.5'f32) / 2'f32 - 0.5'f32, 0'f32, float32(sw - 1))
      let (x0, tx) = (int(floor(fx)), fx - floor(fx))
      let x1 = min(x0 + 1, sw - 1)
      for c in 0 ..< ch:
        template p(xx, yy: int): float32 = src[(yy * sw + xx) * ch + c]
        let top = p(x0, y0) + (p(x1, y0) - p(x0, y0)) * tx
        let bot = p(x0, y1) + (p(x1, y1) - p(x0, y1)) * tx
        result[(y * w + x) * ch + c] = top + (bot - top) * ty

proc pushPull(w, h, ch: int; px: var seq[float32]; wt: seq[float32]; depth = 0) =
  ## Fills pixels with zero `wt` from their surroundings (push-pull), so pyramid levels don't pull
  ## in black from outside an image.
  if (w <= 1 and h <= 1) or depth > 64: return
  let (nw, nh) = ((w + 1) div 2, (h + 1) div 2)
  var cpx = newSeq[float32](nw * nh * ch)
  var cw = newSeq[float32](nw * nh)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let (i, j) = (y * w + x, (y div 2) * nw + x div 2)
      let a = wt[i]
      cw[j] += a
      for c in 0 ..< ch: cpx[j * ch + c] += a * px[i * ch + c]
  for j in 0 ..< nw * nh:
    if cw[j] > 0'f32:
      for c in 0 ..< ch: cpx[j * ch + c] /= cw[j]
      cw[j] = 1
  var hasZero, hasPos = false
  for v in cw:
    if v == 0'f32: hasZero = true
    if v > 0'f32: hasPos = true
  if hasZero and hasPos: pushPull(nw, nh, ch, cpx, cw, depth + 1)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = y * w + x
      if wt[i] < 1'f32:
        let j = (y div 2) * nw + x div 2
        let a = wt[i]
        for c in 0 ..< ch: px[i * ch + c] = a * px[i * ch + c] + (1'f32 - a) * cpx[j * ch + c]

proc multiband*(w, h: int; imgs: openArray[RoiImage]; weights: openArray[seq[float32]]; levels0: int): (seq[float32], seq[float32]) =
  ## Multi-band blend of `imgs` into a `w × h` panorama (origin 0,0) with one weight map per image
  ## over the same grid as its ROI. Returns the colour channels (`ch` per pixel) and the total
  ## coverage.
  let ch = (if imgs.len > 0: imgs[0].ch else: 1)
  if w <= 0 or h <= 0 or ch < 0: return (newSeq[float32](), newSeq[float32]())
  let levels = clamp(levels0, 1, 60) # Rust shifts would overflow beyond
  let align = 1 shl (levels - 1)
  # Level sizes of the panorama.
  var sizes = @[(w, h)]
  for _ in 1 ..< levels: sizes.add (((sizes[^1][0] + 1) div 2), ((sizes[^1][1] + 1) div 2))
  var acc = newSeq[seq[float32]](levels)
  var wsum = newSeq[seq[float32]](levels)
  for l in 0 ..< levels:
    acc[l] = newSeq[float32](sizes[l][0] * sizes[l][1] * ch)
    wsum[l] = newSeq[float32](sizes[l][0] * sizes[l][1])
  let margin = align * 4
  # Each image on its ROI grown by a margin and aligned to the coarsest level's grid.
  for k in 0 ..< min(imgs.len, weights.len):
    let im = imgs[k]
    if not im.usable(ch) or weights[k].len < im.w * im.h: continue
    let gx0 = (max(im.x0 - margin, 0) div align) * align
    let gy0 = (max(im.y0 - margin, 0) div align) * align
    let gx1 = min(im.x0 + im.w + margin, w)
    let gy1 = min(im.y0 + im.h + margin, h)
    let (gw, gh) = (max(gx1 - gx0, 1), max(gy1 - gy0, 1))
    var px = newSeq[float32](gw * gh * ch)
    var cov = newSeq[float32](gw * gh)
    var wt = newSeq[float32](gw * gh)
    for y in 0 ..< im.h:
      for x in 0 ..< im.w:
        let (gx, gy) = (im.x0 + x - gx0, im.y0 + y - gy0)
        if gx < 0 or gy < 0 or gx >= gw or gy >= gh: continue
        let (i, g) = (y * im.w + x, gy * gw + gx)
        cov[g] = im.alpha[i]
        wt[g] = weights[k][i] * rmin(im.alpha[i], 1'f32)
        for c in 0 ..< ch: px[g * ch + c] = im.px[i * ch + c]
    var bin = newSeq[float32](cov.len)
    for i, a in cov: bin[i] = (if a > 0.5'f32: 1'f32 else: 0'f32)
    pushPull(gw, gh, ch, px, bin)
    # Laplacian of the colours, Gaussian of the weights, accumulated level by level.
    var (cw, chh, cur, cwt) = (gw, gh, px, wt)
    for l in 0 ..< levels:
      var band: seq[float32]
      let g = cwt
      let (lw, lh) = (cw, chh)
      if l + 1 == levels: band = cur
      else:
        let (nw, nh, down) = reduceL(cw, chh, ch, cur)
        let up = expandL(nw, nh, ch, down, cw, chh)
        band = newSeq[float32](cur.len)
        for i in 0 ..< cur.len: band[i] = cur[i] - up[i]
        let (_, _, wdown) = reduceL(cw, chh, 1, cwt)
        cw = nw
        chh = nh
        cur = down
        cwt = wdown
      let (ox, oy) = (gx0 shr l, gy0 shr l)
      let (pw, ph) = sizes[l]
      for y in 0 ..< lh:
        let py = oy + y
        if py >= ph: break
        for x in 0 ..< lw:
          let pxx = ox + x
          if pxx >= pw: break
          let (i, p) = (y * lw + x, py * pw + pxx)
          let kk = g[i]
          if kk <= 0'f32: continue
          wsum[l][p] += kk
          for c in 0 ..< ch: acc[l][p * ch + c] += kk * band[i * ch + c]
  for l in 0 ..< levels:
    let (pw, ph) = sizes[l]
    for p in 0 ..< pw * ph:
      let k = wsum[l][p]
      for c in 0 ..< ch: acc[l][p * ch + c] = (if k > 1e-8'f32: acc[l][p * ch + c] / k else: 0'f32)
  # Collapse.
  var cur = acc[levels - 1]
  for l in countdown(levels - 2, 0):
    let (pw, ph) = sizes[l]
    let (sw, sh) = sizes[l + 1]
    let up = expandL(sw, sh, ch, cur, pw, ph)
    cur = newSeq[float32](acc[l].len)
    for i in 0 ..< cur.len: cur[i] = acc[l][i] + up[i]
  # Coverage: union of the images.
  var coverage = newSeq[float32](w * h)
  for im in imgs:
    if not im.usable(ch): continue
    for y in 0 ..< im.h:
      for x in 0 ..< im.w:
        let (pxx, py) = (im.x0 + x, im.y0 + y)
        if pxx < 0 or py < 0 or pxx >= w or py >= h: continue
        let p = py * w + pxx
        coverage[p] = rmax(coverage[p], im.alpha[y * im.w + x])
  (cur, coverage)
