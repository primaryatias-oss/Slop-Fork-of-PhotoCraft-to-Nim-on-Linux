## Multiresolution blending for Edit › Auto-Blend Layers (port of `pyramid.rs`).
##
## P. J. Burt, E. H. Adelson, *A Multiresolution Spline With Application to Image Mosaics*, ACM
## TOG 1983: each image is split into a Laplacian pyramid (band-pass levels plus a low-pass
## residual), the blend weights into a Gaussian pyramid, every level is blended with its own
## smoothed weights and the result collapsed, so seams are invisible at every scale.
##
## Weights come from `stackWeights` (focus stacking: per pixel, the sharpest image wins, by the
## smoothed magnitude of the luminance Laplacian) or `panoramaWeights` (each pixel goes to the
## image whose opaque area it is deepest inside).
##
## `fuseStack` is the focus stack's merge: E. H. Adelson et al., *Pyramid methods in image
## processing*, RCA Engineer 1984 (the multifocus composite) with the selection rules of W. Wang,
## F. Chang, *A Multi-focus Image Fusion Method Based on Laplacian Pyramid*, Journal of Computers
## 6(12), 2011, plus halo control.
##
## Buffers are interleaved `w × h × ch` normalised floats of any colour model and depth;
## deterministic.

import std/[math, options]
import ./util

type
  Level* = object
    ## One pyramid level.
    w*, h*: int
    px*: seq[float32]

  StackFusion* = object
    ## Focus-stack fusion settings for `fuseStack`.
    energyRadius*: int ## region-energy window radius for the band-pass levels (1 = 3×3 binomial; 0 = |L|)
    halo*: float32     ## halo control hardness: 0 = off; default 2, capped at `HaloMax`
    guide*: int        ## the guide level for halo control (0 = the finest band-pass level)
    levels*: Option[int] ## pyramid levels, the residual included; none = `stackLevels`

const
  K = [1'f32 / 16, 4'f32 / 16, 6'f32 / 16, 4'f32 / 16, 1'f32 / 16]
  HaloMax* = 8'f32 ## largest useful halo hardness
  HaloRef = 1e-4'f32 ## reference energy the halo weights are relative to
  HaloFloor = 1e-8'f32 ## floor added to the energy before weighing
  Covered = 0.5'f32 ## coverage below which an image has no say at a pixel
  NoEnergy = -1'f32 ## the energy of an image where it does not cover the pixel

proc defaultStackFusion*(): StackFusion = StackFusion(energyRadius: 1, halo: 2, guide: 2, levels: none(int))

proc reduce*(l: Level; ch: int): Level =
  ## 5-tap binomial filter + decimation by two (Burt & Adelson's REDUCE).
  let (w, h) = ((l.w + 1) div 2, (l.h + 1) div 2)
  # Horizontal pass on even columns, then vertical on even rows.
  var tmp = newSeq[float32](w * l.h * ch)
  for y in 0 ..< l.h:
    for x in 0 ..< w:
      for k, wt in K:
        let sx = clamp(2 * x + k - 2, 0, l.w - 1)
        for c in 0 ..< ch: tmp[(y * w + x) * ch + c] += wt * l.px[(y * l.w + sx) * ch + c]
  var px = newSeq[float32](w * h * ch)
  for y in 0 ..< h:
    for k, wt in K:
      let sy = clamp(2 * y + k - 2, 0, l.h - 1)
      for x in 0 ..< w:
        for c in 0 ..< ch: px[(y * w + x) * ch + c] += wt * tmp[(sy * w + x) * ch + c]
  Level(w: w, h: h, px: px)

proc expand*(l: Level; ch, w, h: int): seq[float32] =
  ## Upsample `l` to `w × h` (EXPAND: zero insertion + the same filter, ×4 gain).
  var tmp = newSeq[float32](w * l.h * ch)
  for y in 0 ..< l.h:
    for x in 0 ..< w:
      for k, wt in K:
        let t = x + k - 2
        if euclMod(t, 2) != 0: continue
        let sx = clamp(t div 2, 0, l.w - 1)
        for c in 0 ..< ch: tmp[(y * w + x) * ch + c] += 2'f32 * wt * l.px[(y * l.w + sx) * ch + c]
  result = newSeq[float32](w * h * ch)
  for y in 0 ..< h:
    for k, wt in K:
      let t = y + k - 2
      if euclMod(t, 2) != 0: continue
      let sy = clamp(t div 2, 0, l.h - 1)
      for x in 0 ..< w:
        for c in 0 ..< ch: result[(y * w + x) * ch + c] += 2'f32 * wt * tmp[(sy * w + x) * ch + c]

proc gaussian*(w, h, ch: int; px: openArray[float32]; levels: int): seq[Level] =
  ## Gaussian pyramid with `levels` levels (level 0 = the input).
  result = @[Level(w: w, h: h, px: @px)]
  while result.len < max(levels, 1):
    let last = result[^1]
    if last.w <= 1 and last.h <= 1: break
    result.add reduce(last, ch)

proc laplacian*(w, h, ch: int; px: openArray[float32]; levels: int): seq[Level] =
  ## Laplacian pyramid: band-pass levels, the last level is the low-pass residual.
  let g = gaussian(w, h, ch, px, levels)
  for i in 0 ..< g.len:
    if i + 1 == g.len: result.add g[i]
    else:
      let up = expand(g[i + 1], ch, g[i].w, g[i].h)
      var d = newSeq[float32](g[i].px.len)
      for k in 0 ..< d.len: d[k] = g[i].px[k] - up[k]
      result.add Level(w: g[i].w, h: g[i].h, px: d)

proc collapse*(pyr: openArray[Level]; ch: int): seq[float32] =
  ## Rebuild an image from its Laplacian pyramid.
  if pyr.len == 0: return @[]
  var cur = pyr[^1]
  for i in countdown(pyr.len - 2, 0):
    let l = pyr[i]
    let up = expand(cur, ch, l.w, l.h)
    var px = newSeq[float32](l.px.len)
    for k in 0 ..< px.len: px[k] = l.px[k] + up[k]
    cur = Level(w: l.w, h: l.h, px: px)
  cur.px

proc autoLevels*(w, h: int): int =
  ## Number of levels so the coarsest level is a few pixels across.
  result = 1
  var s = min(w, h)
  while s > 8 and result < 10:
    s = s div 2
    inc result

proc validBuf(len, w, h, ch: int): bool =
  w >= 0 and h >= 0 and ch >= 0 and (w == 0 or h <= high(int) div w) and
    (w * h == 0 or ch <= high(int) div (w * h)) and len >= w * h * ch

proc blend*(w, h, ch: int; images: openArray[seq[float32]]; weights: openArray[seq[float32]]; levels0: int): seq[float32] =
  ## Blend `images` with per-pixel `weights` (any non-negative values; normalised per level).
  ## Raises `ValueError` when the counts or buffer sizes do not match.
  if images.len != weights.len: raise newException(ValueError, "blend: one weight map per image")
  for i in 0 ..< images.len:
    if not validBuf(images[i].len, w, h, ch) or not validBuf(weights[i].len, w, h, 1):
      raise newException(ValueError, "blend: buffer sizes do not match the image")
  let levels = max(levels0, 1)
  var acc, wsum: seq[Level]
  var started = false
  for n in 0 ..< images.len:
    let lp = laplacian(w, h, ch, images[n], levels)
    let gp = gaussian(w, h, 1, weights[n], levels)
    if not started:
      for l in lp: acc.add Level(w: l.w, h: l.h, px: newSeq[float32](l.px.len))
      for l in gp: wsum.add Level(w: l.w, h: l.h, px: newSeq[float32](l.px.len))
      started = true
    for li in 0 ..< min(lp.len, gp.len):
      let (l, g) = (lp[li], gp[li])
      for i in 0 ..< l.w * l.h:
        let k = g.px[i]
        wsum[li].px[i] += k
        for c in 0 ..< ch: acc[li].px[i * ch + c] += k * l.px[i * ch + c]
  if not started: return newSeq[float32](w * h * ch)
  for li in 0 ..< min(acc.len, wsum.len):
    for i in 0 ..< acc[li].w * acc[li].h:
      let k = wsum[li].px[i]
      for c in 0 ..< ch:
        acc[li].px[i * ch + c] = (if k > 1e-6'f32: acc[li].px[i * ch + c] / k else: 0'f32)
  result = collapse(acc, ch)
  for v in result.mitems: v = clamp(v, 0'f32, 1'f32)

proc stackLevels*(w, h: int): int =
  ## Levels for `fuseStack`: as many band-pass levels as keep the residual's short side ≥ 32 px.
  result = 1
  var s = min(w, h)
  while (s + 1) div 2 >= 32 and result < 10:
    s = (s + 1) div 2
    inc result

proc haloWeight*(re, p: float32): float32 =
  ## `((re + floor) / ref)^p`, its exponent clamped to ±60.
  exp(clamp(p * ln((rmax(re, 0'f32) + HaloFloor) / HaloRef), -60'f32, 60'f32))

proc binomial*(r: int): seq[float32] =
  ## Binomial window weights of radius `r` (row 2r of Pascal's triangle / 4^r).
  let n = 2 * r + 1
  var row = newSeq[uint64](n)
  for v in row.mitems: v = 1
  for i in 1 ..< n:
    for j in countdown(i - 1, 1): row[j] += row[j - 1]
  var s = 0'u64
  for v in row: s += v
  let sf = float32(s)
  for v in row: result.add float32(v) / sf

proc windowSum*(w, h: int; src: openArray[float32]; wt: openArray[float32]): seq[float32] =
  ## Separable weighted window sum of a plane (`wt` of odd length, clamped borders).
  let r = wt.len div 2
  if r == 0 or w == 0 or h == 0: return @src
  var tmp = newSeq[float32](w * h)
  for y in 0 ..< h:
    for x in 0 ..< w:
      var s = -0'f32
      for t, k in wt: s += k * src[y * w + min(max(x + t - r, 0), w - 1)]
      tmp[y * w + x] = s
  result = newSeq[float32](w * h)
  for y in 0 ..< h:
    for t, k in wt:
      let sy = min(max(y + t - r, 0), h - 1)
      for x in 0 ..< w: result[y * w + x] += k * tmp[sy * w + x]

proc regionEnergy(l, cover: Level; win: openArray[float32]): seq[float32] =
  ## Region energy of a band-pass luminance level: the window sum of L² (Wang & Chang eq. 13);
  ## `NoEnergy` where the image's coverage is below `Covered`.
  var e = newSeq[float32](l.px.len)
  for i, v in l.px: e[i] = v * v
  result = windowSum(l.w, l.h, e, win)
  for i in 0 ..< min(result.len, cover.px.len):
    if cover.px[i] < Covered: result[i] = NoEnergy

proc deviationEntropy*(w, h: int; y: openArray[float32]): (seq[float32], seq[float32]) =
  ## Local deviation (Wang & Chang eq. 10) and local entropy (eq. 11, 256 gray levels) of a plane
  ## over a 5×5 window, clamped borders.
  const R = 2
  const Bins = 256
  let n = (2 * R + 1) * (2 * R + 1)
  let invN = 1'f32 / float32(n)
  var dev = newSeq[float32](w * h)
  var ent = newSeq[float32](w * h)
  if w == 0 or h == 0: return (dev, ent)
  var hist: array[Bins, uint32]
  var touched = newSeqOfCap[int](n)
  for i in 0 ..< h:
    for j in 0 ..< w:
      var (s, s2) = (0'f32, 0'f32)
      touched.setLen(0)
      for dy in 0 .. 2 * R:
        let yy = min(max(i + dy - R, 0), h - 1)
        for dx in 0 .. 2 * R:
          let v = y[yy * w + min(max(j + dx - R, 0), w - 1)]
          s += v
          s2 += v * v
          let q = min(asIdx(clamp(v, 0'f32, 1'f32) * float32(Bins - 1) + 0.5'f32), Bins - 1)
          if hist[q] == 0: touched.add q
          hist[q] += 1
      let mean = s * invN
      dev[i * w + j] = rmax(s2 * invN - mean * mean, 0'f32)
      var e = 0'f32
      for q in touched:
        let p = float32(hist[q]) * invN
        e -= p * ln(p)
        hist[q] = 0
      ent[i * w + j] = e
  (dev, ent)

type Residual = object
  ## One image's residual (low-pass) level, with its luminance and coverage at that level.
  px: Level
  luma, cover: seq[float32]

proc fuseResiduals(tops: seq[Residual]; ch: int): Level =
  ## Fuse the images' residuals by Wang & Chang's region-information rule (eq. 12), generalised
  ## to N images as a Pareto rule; the fused value is the mean of the non-dominated images.
  if tops.len == 0: return Level()
  let (w, h, n) = (tops[0].px.w, tops[0].px.h, tops.len)
  var measures: seq[(seq[float32], seq[float32])]
  for k in 0 ..< n: measures.add deviationEntropy(w, h, tops[k].luma)
  var o = newSeq[float32](w * h * ch)
  var keep = newSeq[bool](n)
  for i in 0 ..< w * h:
    var anyCover = false
    for t in tops:
      if t.cover[i] >= Covered: anyCover = true
    template eligible(a: int): bool = (not anyCover) or tops[a].cover[i] >= Covered
    var kept = 0
    for a in 0 ..< n:
      let (da, ea) = (measures[a][0][i], measures[a][1][i])
      var dominated = false
      if eligible(a):
        for b in 0 ..< n:
          let (db, eb) = (measures[b][0][i], measures[b][1][i])
          if b != a and eligible(b) and db >= da and eb >= ea and (db > da or eb > ea):
            dominated = true
            break
      keep[a] = eligible(a) and not dominated
      if keep[a]: inc kept
    let inv = 1'f32 / float32(max(kept, 1))
    for k in 0 ..< n:
      if keep[k]:
        for c in 0 ..< ch: o[i * ch + c] += tops[k].px.px[i * ch + c] * inv
  Level(w: w, h: h, px: o)

proc foldHalo(acc: var seq[Level]; wsum: var seq[seq[float32]]; src: Option[seq[Level]]; g: int; wgt0: Level; ch: int) =
  ## Halo control: fold the levels coarser than the guide `g` as `Σ w·L` and `Σ w`, the guide's
  ## weights `wgt` REDUCEd to each level's size. `src` is the image's pyramid; none scales the
  ## first image's levels, already in `acc`, in place.
  var wgt = wgt0
  for li in g + 1 ..< acc.len:
    wgt = reduce(wgt, 1)
    if src.isSome:
      if li - g - 1 < wsum.len:
        for i in 0 ..< min(wsum[li - g - 1].len, wgt.px.len): wsum[li - g - 1][i] += wgt.px[i]
      let sp = src.get[li].px
      for i in 0 ..< acc[li].px.len div max(ch, 1):
        let k = wgt.px[i]
        for c in 0 ..< ch: acc[li].px[i * ch + c] += k * sp[i * ch + c]
    else:
      for i in 0 ..< acc[li].px.len div max(ch, 1):
        let k = wgt.px[i]
        for c in 0 ..< ch: acc[li].px[i * ch + c] *= k
      wsum.add wgt.px

proc fuseStack*(w, h, ch: int; images, luma, alpha: openArray[seq[float32]]; p: StackFusion): seq[float32] =
  ## Multifocus composite of `images` (interleaved `w × h × ch`; `luma[k]` is image k's
  ## luminance, `alpha[k]` its coverage): per Laplacian-pyramid coefficient, the image whose
  ## luminance band has the largest region energy wins, the residual follows the deviation +
  ## entropy rule, and with halo control the levels coarser than `p.guide` are the mean of the
  ## images weighed by the guide's region energy. Ties keep the earlier image, and an image has no
  ## say where its coverage is below one half. Output is clamped to 0..=1. Raises `ValueError`
  ## when a buffer does not match the size.
  let count = min(images.len, min(luma.len, alpha.len))
  for k in 0 ..< count:
    if not validBuf(images[k].len, w, h, ch) or not validBuf(luma[k].len, w, h, 1) or not validBuf(alpha[k].len, w, h, 1):
      raise newException(ValueError, "fuseStack: buffer sizes do not match the image")
  # Levels the pyramid can actually have (REDUCE stops at 1 × 1).
  let levels = block:
    let want = max(p.levels.get(stackLevels(w, h)), 1)
    var (n, a, b) = (1, w, h)
    while (a > 1 or b > 1) and n < want:
      (a, b, n) = ((a + 1) div 2, (b + 1) div 2, n + 1)
    n
  let bands = levels - 1
  let halo = (if bands > 0 and p.halo > 0'f32: some((min(p.guide, bands - 1), rmin(p.halo, HaloMax))) else: none((int, float32)))
  # The band-pass levels that pick their own winner: all of them, or up to the guide.
  let nsel = (if halo.isSome: halo.get[0] + 1 else: bands)
  let win = binomial(max(p.energyRadius, 0))
  var acc: seq[Level]
  var best: seq[seq[float32]]  # per selecting level, the winning energy so far
  var wsum: seq[seq[float32]]  # with halo control, per level coarser than the guide, the weight sum so far
  var tops: seq[Residual]
  for k in 0 ..< count:
    let lp = laplacian(w, h, ch, images[k], levels)
    let ll = laplacian(w, h, 1, luma[k], levels)
    let ga = gaussian(w, h, 1, alpha[k], levels)
    if lp.len == 0 or ll.len == 0 or ga.len == 0: continue
    if lp.len != levels or ll.len != levels or ga.len != levels: continue
    if halo.isNone: tops.add Residual(px: lp[^1], luma: ll[^1].px, cover: ga[^1].px)
    var energies: seq[seq[float32]]
    for li in 0 ..< nsel: energies.add regionEnergy(ll[li], ga[li], win)
    var wgt = none(Level)
    if halo.isSome:
      let (g, hard) = halo.get
      var wp = newSeq[float32](energies[g].len)
      for i, e in energies[g]: wp[i] = (if e < 0'f32: 0'f32 else: haloWeight(e, hard))
      wgt = some(Level(w: ll[g].w, h: ll[g].h, px: wp))
    if acc.len == 0:
      acc = lp
      best = energies
      if halo.isSome: foldHalo(acc, wsum, none(seq[Level]), halo.get[0], wgt.get, ch)
      continue
    for li in 0 ..< nsel:
      let lvl = lp[li]
      for i in 0 ..< energies[li].len:
        if energies[li][i] > best[li][i]:
          best[li][i] = energies[li][i]
          for c in 0 ..< ch: acc[li].px[i * ch + c] = lvl.px[i * ch + c]
    if halo.isSome: foldHalo(acc, wsum, some(lp), halo.get[0], wgt.get, ch)
  if acc.len == 0: return newSeq[float32](w * h * ch)
  if halo.isSome:
    # The weighted means: Σ w·L / Σ w.
    let g = halo.get[0]
    for li in g + 1 ..< levels:
      let s = wsum[li - g - 1]
      for i in 0 ..< acc[li].px.len div max(ch, 1):
        let k = s[i]
        for c in 0 ..< ch: acc[li].px[i * ch + c] = (if k > 0'f32: acc[li].px[i * ch + c] / k else: 0'f32)
  else:
    acc[^1] = fuseResiduals(tops, ch)
  result = collapse(acc, ch)
  for v in result.mitems: v = clamp(v, 0'f32, 1'f32)

proc boxBlur1(w, h: int; v: openArray[float32]; r: int): seq[float32] =
  proc pass(src: seq[float32]; horizontal: bool): seq[float32] =
    result = newSeq[float32](src.len)
    let (n, m) = (if horizontal: (h, w) else: (w, h))
    for line in 0 ..< n:
      template at(k0: int): float32 =
        block:
          let k = clamp(k0, 0, m - 1)
          (if horizontal: src[line * w + k] else: src[k * w + line])
      var s = -0'f32
      for k in -r .. r: s += at(k)
      for k in 0 ..< m:
        let i = (if horizontal: line * w + k else: k * w + line)
        result[i] = s / float32(2 * r + 1)
        s += at(k + r + 1) - at(k - r)
  pass(pass(@v, true), false)

proc winnerTakeAll(scores: seq[seq[float32]]; n: int): seq[seq[float32]] =
  ## One-hot winner maps: weight 1 for the image with the largest score at each pixel.
  result = newSeq[seq[float32]](scores.len)
  for s in result.mitems: s = newSeq[float32](n)
  for i in 0 ..< n:
    var best = (0, -F32Max)
    for k, s in scores:
      if s[i] > best[1]: best = (k, s[i])
    if best[1] > -F32Max: result[best[0]][i] = 1

proc stackWeights*(w, h: int; luma, alpha: openArray[seq[float32]]): seq[seq[float32]] =
  ## Focus-stack weights: per pixel, the image with the most local detail (smoothed |∇²L|) wins.
  ## `luma[k]` is image k's luminance, `alpha[k]` its coverage. Raises `ValueError` on a buffer
  ## that does not match the size.
  let radius = clamp(min(w, h) div 64, 2, 12)
  var scores: seq[seq[float32]]
  for k in 0 ..< min(luma.len, alpha.len):
    let (l, a) = (luma[k], alpha[k])
    if not validBuf(l.len, w, h, 1) or not validBuf(a.len, w, h, 1):
      raise newException(ValueError, "stackWeights: buffer sizes do not match the image")
    var lap = newSeq[float32](w * h)
    for y in 0 ..< h:
      for x in 0 ..< w:
        template p(xx, yy: int): float32 = l[yy * w + xx]
        let (xl, xr, yu, yd) = (max(x - 1, 0), min(x + 1, w - 1), max(y - 1, 0), min(y + 1, h - 1))
        lap[y * w + x] = abs(p(xl, y) + p(xr, y) + p(x, yu) + p(x, yd) - 4'f32 * p(x, y))
    let b = boxBlur1(w, h, lap, radius)
    var s = newSeq[float32](w * h)
    for i in 0 ..< w * h: s[i] = (if a[i] > 0.5'f32: b[i] else: -1'f32)
    scores.add s
  winnerTakeAll(scores, w * h)

proc panoramaWeights*(w, h: int; alpha: openArray[seq[float32]]): seq[seq[float32]] =
  ## Panorama weights: each pixel goes to the image whose opaque area it lies deepest inside
  ## (chamfer distance to that image's transparent pixels), so seams run through overlaps.
  var scores: seq[seq[float32]]
  for a in alpha:
    if not validBuf(a.len, w, h, 1): raise newException(ValueError, "panoramaWeights: buffer sizes do not match the image")
    # Two-pass 3-4 chamfer distance transform from transparent pixels / the border.
    let inf = 1.0e9'f32
    var d = newSeq[float32](w * h)
    for i in 0 ..< w * h: d[i] = (if a[i] > 0.5'f32: inf else: 0'f32)
    template at(x, y: int): float32 = (if x < 0 or y < 0 or x >= w or y >= h: 0'f32 else: d[y * w + x])
    for y in 0 ..< h:
      for x in 0 ..< w:
        let i = y * w + x
        d[i] = rmin(rmin(rmin(rmin(d[i], at(x - 1, y) + 3'f32), at(x, y - 1) + 3'f32), at(x - 1, y - 1) + 4'f32), at(x + 1, y - 1) + 4'f32)
    for y in countdown(h - 1, 0):
      for x in countdown(w - 1, 0):
        let i = y * w + x
        d[i] = rmin(rmin(rmin(rmin(d[i], at(x + 1, y) + 3'f32), at(x, y + 1) + 3'f32), at(x + 1, y + 1) + 4'f32), at(x - 1, y + 1) + 4'f32)
    var s = newSeq[float32](w * h)
    for i in 0 ..< w * h: s[i] = (if d[i] <= 0'f32: -1'f32 else: d[i])
    scores.add s
  winnerTakeAll(scores, w * h)
