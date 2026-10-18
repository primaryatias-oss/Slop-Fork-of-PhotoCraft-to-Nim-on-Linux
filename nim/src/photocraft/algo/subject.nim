## Select Subject (classical, heuristic; port of `segment/subject.rs`).
##
## Photoshop uses a learned model; this approximates it with salient-object detection feeding a
## GrabCut:
##
## 1. SLIC superpixels on a working-resolution copy.
## 2. Per-superpixel saliency from "Saliency Optimization from Robust Background Detection"
##    (W. Zhu, S. Liang, Y. Wei and J. Sun, CVPR 2014): boundary connectivity over geodesic colour
##    distances gives a background probability; background-weighted contrast gives foreground
##    cues; a least-squares optimisation with smoothness between neighbours combines them.
## 3. Blended with frequency-tuned saliency (R. Achanta et al., CVPR 2009).
## 4. Otsu's threshold on the saliency map seeds a GrabCut trimap; the result is cleaned and
##    re-cut at full resolution.

import std/[math, options, heapqueue]
import ../geom
import ./util, ./segment, ./slic, ./grabcut

const
  WorkPx* = 110_000 ## working-resolution budget (pixels)
  SigmaClr = 10'f32
  SigmaSpa = 0.25'f32

proc blurLab(img: RgbImage): RgbImage =
  let (w, h) = (img.w, img.h)
  var cur = img
  for _ in 0 ..< 2:
    var tmp = cur
    for y in 0 ..< h:
      for x in 0 ..< w:
        let (l, c, r) = (cur.at(max(x - 1, 0), y), cur.at(x, y), cur.at(min(x + 1, w - 1), y))
        for k in 0 .. 2: tmp.px[y * w + x][k] = (l[k] + 2'f32 * c[k] + r[k]) / 4'f32
    for y in 0 ..< h:
      for x in 0 ..< w:
        let (u, c, d) = (tmp.at(x, max(y - 1, 0)), tmp.at(x, y), tmp.at(x, min(y + 1, h - 1)))
        for k in 0 .. 2: cur.px[y * w + x][k] = (u[k] + 2'f32 * c[k] + d[k]) / 4'f32
  cur

proc dijkstra(src: int; adj: seq[seq[int]]; wt: proc(a, b: int): float32; dist: var seq[float32]) =
  ## Geodesic distances from `src` (the final distances do not depend on the order of ties).
  for v in dist.mitems: v = Inf.float32
  dist[src] = 0
  var heap = initHeapQueue[(float32, int)]()
  heap.push (0'f32, src)
  while heap.len > 0:
    let (d, u) = heap.pop
    if d > dist[u]: continue
    for v in adj[u]:
      let nd = d + wt(u, v)
      if nd < dist[v]:
        dist[v] = nd
        heap.push (nd, v)

proc otsu*(v: openArray[float32]): float32 =
  ## Otsu's threshold of values in `0..=1` (256 bins).
  var hist: array[256, uint64]
  for x in v: hist[asIdx(round(clamp(x, 0'f32, 1'f32) * 255'f32))] += 1
  let total = float64(v.len)
  var sum = -0.0
  for i, c in hist: sum += float64(i) * float64(c)
  var (wb, sb, best, bt) = (0.0, 0.0, -1.0, 0)
  for t, c in hist:
    wb += float64(c)
    if wb == 0.0: continue
    let wf = total - wb
    if wf == 0.0: break
    sb += float64(t) * float64(c)
    let (mb, mf) = (sb / wb, (sum - sb) / wf)
    let between = wb * wf * (mb - mf) * (mb - mf)
    if between > best:
      best = between
      bt = t
  (float32(bt) + 0.5'f32) / 255'f32

proc saliency*(img: RgbImage): seq[float32] =
  ## Per-pixel saliency in `0..=1`.
  let (w, h) = (img.w, img.h)
  if w * h == 0: return @[]
  var lab = newSeq[array[3, float32]](img.px.len)
  for i, p in img.px: lab[i] = rgbToLab(p)
  let target = clamp((w * h) div 150, 16, 400)
  let sp = slicLab(lab, w, h, target, 10, 8)
  let n = sp.count
  # Region statistics.
  var meanD = newSeq[array[3, float64]](n)
  var posD = newSeq[array[2, float64]](n)
  var cnt = newSeq[float64](n)
  var onBorder = newSeq[bool](n)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let l = int(sp.labels[y * w + x])
      let c = lab[y * w + x]
      for k in 0 .. 2: meanD[l][k] += float64(c[k])
      posD[l][0] += float64(x) / float64(w)
      posD[l][1] += float64(y) / float64(h)
      cnt[l] += 1.0
      if x == 0 or y == 0 or x + 1 == w or y + 1 == h: onBorder[l] = true
  var mean = newSeq[array[3, float32]](n)
  var pos = newSeq[array[2, float32]](n)
  for l in 0 ..< n:
    let c = rmax(cnt[l], 1.0)
    for k in 0 .. 2: mean[l][k] = float32(meanD[l][k] / c)
    for k in 0 .. 1: pos[l][k] = float32(posD[l][k] / c)
  let dapp = proc(a, b: int): float32 = sqrt(d2(mean[a], mean[b]))
  # Adjacency.
  var adj = newSeq[seq[int]](n)
  proc link(a, b: int) =
    if a != b and b notin adj[a]:
      adj[a].add b
      adj[b].add a
  for y in 0 ..< h:
    for x in 0 ..< w:
      let l = int(sp.labels[y * w + x])
      if x + 1 < w: link(l, int(sp.labels[y * w + x + 1]))
      if y + 1 < h: link(l, int(sp.labels[(y + 1) * w + x]))
  # Boundary connectivity from all-pairs geodesic distances (Dijkstra per region).
  var wbg = newSeq[float32](n)
  var geo = newSeq[float32](n)
  for p in 0 ..< n:
    dijkstra(p, adj, dapp, geo)
    var (area, len) = (0'f32, 0'f32)
    for q in 0 ..< n:
      let s = exp(-geo[q] * geo[q] / (2'f32 * SigmaClr * SigmaClr))
      area += s
      if onBorder[q]: len += s
    let bnd = len / sqrt(rmax(area, 1e-6'f32))
    wbg[p] = 1'f32 - exp(-bnd * bnd / 2'f32)
  # Background-weighted contrast.
  var wctr = newSeq[float32](n)
  for p in 0 ..< n:
    var s = 0'f32
    for q in 0 ..< n:
      let (dx, dy) = (pos[p][0] - pos[q][0], pos[p][1] - pos[q][1])
      let wspa = exp(-(dx * dx + dy * dy) / (2'f32 * SigmaSpa * SigmaSpa))
      s += dapp(p, q) * wspa * wbg[q]
    wctr[p] = s
  var mx = 0'f32
  for v in wctr: mx = rmax(mx, v)
  mx = rmax(mx, 1e-6'f32)
  var wfg = newSeq[float32](n)
  for i, v in wctr: wfg[i] = v / mx
  # Saliency optimisation: min Σ wbg s² + Σ wfg (s−1)² + Σ_{i~j} w_ij (s_i − s_j)² (Gauss–Seidel).
  var wij = newSeq[seq[float32]](n)
  for p in 0 ..< n:
    for q in adj[p]:
      let d = dapp(p, q)
      wij[p].add exp(-(d * d) / (2'f32 * SigmaClr * SigmaClr)) + 0.1'f32
  var s = wfg
  for _ in 0 ..< 200:
    for p in 0 ..< n:
      var (num, den) = (wfg[p], wbg[p] + wfg[p])
      for k, q in adj[p]:
        num += wij[p][k] * s[q]
        den += wij[p][k]
      s[p] = num / rmax(den, 1e-6'f32)
  # Frequency-tuned saliency.
  var meanAll: array[3, float64]
  for c in lab:
    for k in 0 .. 2: meanAll[k] += float64(c[k])
  let ma = [float32(meanAll[0] / float64(lab.len)), float32(meanAll[1] / float64(lab.len)), float32(meanAll[2] / float64(lab.len))]
  let blurred = blurLab(RgbImage(w: w, h: h, px: lab))
  var ft = newSeq[float32](blurred.px.len)
  var ftmax = 0'f32
  for i, c in blurred.px:
    ft[i] = sqrt(d2(c, ma))
    ftmax = rmax(ftmax, ft[i])
  ftmax = rmax(ftmax, 1e-6'f32)
  var (smin, smax) = (F32Max, -F32Max)
  for v in s:
    smin = rmin(smin, v)
    smax = rmax(smax, v)
  let srange = rmax(smax - smin, 1e-6'f32)
  result = newSeq[float32](sp.labels.len)
  for i, l in sp.labels: result[i] = 0.75'f32 * (s[l] - smin) / srange + 0.25'f32 * ft[i] / ftmax
  var (lo, hi) = (F32Max, -F32Max)
  for v in result:
    lo = rmin(lo, v)
    hi = rmax(hi, v)
  let r = rmax(hi - lo, 1e-6'f32)
  for v in result.mitems: v = (v - lo) / r

proc selectSubject*(sampler: Sampler; canvas: Rect): Option[Region] =
  ## Selects the most salient object on the canvas.
  if canvas.width < 4 or canvas.height < 4: return none(Region)
  let step = scaleFor(canvas, WorkPx)
  let img = sampler.rgbScaled(canvas, step)
  let (w, h) = (img.w, img.h)
  let sal = saliency(img)
  let t = clamp(otsu(sal), 0.15'f32, 0.85'f32)
  var trimap = newSeq[uint8](w * h)
  var anyFg = false
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = y * w + x
      let border = x == 0 or y == 0 or x + 1 == w or y + 1 == h
      trimap[i] = (if sal[i] >= t: PrFg elif border or sal[i] < 0.4'f32 * t: Bg else: PrBg)
      if sal[i] >= t: anyFg = true
  if not anyFg: return none(Region)
  let models = grabcut(img, trimap, 6)
  if models.isNone: return none(Region)
  var low = newSeq[bool](w * h)
  for i, v in trimap: low[i] = v == PrFg or v == Fg
  let cleaned = cleanMask(low, w, h, 0.2, 0.05)
  var any = false
  for v in cleaned:
    if v: any = true
  if not any: return none(Region)
  finishRegion(sampler, canvas, step, cleaned, w, h, models)
