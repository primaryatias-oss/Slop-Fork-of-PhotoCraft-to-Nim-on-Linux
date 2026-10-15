## Non-local patch-based completion, the Remove Tool's fill (port of `nonlocal.rs`): A. Newson,
## A. Almansa, Y. Gousseau, P. Pérez, *Non-Local Patch-Based Image Inpainting*, IPOL 7 (2017).
## Coarse to fine, it alternates a PatchMatch search for each patch's nearest fully-known patch
## (C. Barnes et al., SIGGRAPH 2009) with a similarity-weighted vote of the matches (Y. Wexler,
## E. Shechtman, M. Irani, TPAMI 2007), with:
##
## * **texture features** in the patch distance (local mean of `|∂x|` and `|∂y|`, weighted by
##   `Params.lambda`), filled along with the colour;
## * **onion-peel initialisation** at the coarsest level;
## * **the match field upsampled** between levels, not the image;
## * **each final pixel from the best patch covering it** instead of the vote;
## * **seams hidden in the gradient domain**: a membrane carries the colour mismatch along the
##   hole's edge into the fill, and steps between areas copied from different places are spread
##   out by a smooth correction (after S. Darabi et al., *Image Melding*, 2012);
## * **the number of levels follows the hole's thickness**.
##
## Buffers are interleaved `w × h × ch` normalised floats with a `w × h` hole mask. Deterministic
## for a seed (the Rust build's parallel bands each have their own generator; run in order here).

import std/[math, options, algorithm, deques]
import ../geom, ../rmath, ../raster
import ./util, ./prng, ./holes, ./poisson

type
  Params* = object
    ## Parameters for `completeWith`. The defaults are the paper's, except for the PatchMatch
    ## passes per iteration (two instead of ten, for speed).
    patchRadius*: int ## patch radius (patches are `2r+1` square)
    lambda*: float32  ## weight of the texture features against the colour in the patch distance
    maxIters*: int    ## most EM iterations (search, then vote) per level
    pmPasses*: int    ## PatchMatch passes per EM iteration
    tolerance*: float32 ## a level is done once an iteration changes the hole by less than this
    seed*: uint64

  Level = object
    w, h: int
    img: seq[float32]
    hole: seq[bool]

  Solver = object
    w, h, cha, ch: int
    r: int32
    img: seq[float32]
    hole: seq[bool]
    valid: seq[bool]
    validList: seq[(int32, int32)]
    targets: seq[bool]

  Copied = (int, int, float32)

proc defaultParams*(): Params =
  Params(patchRadius: 3, lambda: 50, maxIters: 10, pmPasses: 2, tolerance: 0.1'f32 / 255'f32, seed: 1)

proc defaultOf*(_: typedesc[Params]): Params = defaultParams()

proc kth(v: var seq[float32]; k: int): float32 =
  ## `select_nth_unstable_by(k, total_cmp)`: the k-th smallest value.
  v.sort(proc(a, b: float32): int = totalCmp(a, b))
  v[k]

proc validSources(w, h: int; hole: openArray[bool]; r: int): (seq[bool], seq[(int32, int32)]) =
  ## Fully-known patch centres (the whole patch on the grid and outside the hole).
  var valid = newSeq[bool](w * h)
  var list: seq[(int32, int32)]
  if w < 2 * r + 1 or h < 2 * r + 1: return (valid, list)
  # Summed-area table of the hole.
  let w1 = w + 1
  var sat = newSeq[uint32](w1 * (h + 1))
  for y in 0 ..< h:
    var row = 0'u32
    for x in 0 ..< w:
      row += uint32(ord(hole[y * w + x]))
      sat[(y + 1) * w1 + x + 1] = sat[y * w1 + x + 1] + row
  for y in r ..< h - r:
    for x in r ..< w - r:
      let (x0, y0, x1, y1) = (x - r, y - r, x + r + 1, y + r + 1)
      if sat[y1 * w1 + x1] + sat[y0 * w1 + x0] == sat[y0 * w1 + x1] + sat[y1 * w1 + x0]:
        valid[y * w + x] = true
        list.add (int32(x), int32(y))
  (valid, list)

proc textureFeatures(w, h, ch: int; img: seq[float32]; hole: seq[bool]; side: int): (seq[float32], seq[float32]) =
  ## The texture features at every known cell: the mean of `|∂x|` and of `|∂y|` of the channel
  ## mean over a `side`-wide window, counting only derivatives between two known cells. Hole
  ## cells get 0 (the completion fills them in).
  var g = newSeq[float32](w * h)
  for i in 0 ..< w * h:
    var s = -0'f32
    for c in 0 ..< ch: s += img[i * ch + c]
    g[i] = s / float32(ch)
  let w1 = w + 1
  proc table(dx, dy: int): seq[float32] =
    var sum = newSeq[float64](w1 * (h + 1))
    var cnt = newSeq[uint32](w1 * (h + 1))
    for y in 0 ..< h:
      var (rs, rc) = (0.0, 0'u32)
      for x in 0 ..< w:
        let i = y * w + x
        let (nx, ny) = (x + dx, y + dy)
        if nx < w and ny < h and not hole[i] and not hole[ny * w + nx]:
          rs += float64(abs(g[ny * w + nx] - g[i]))
          rc += 1
        sum[(y + 1) * w1 + x + 1] = sum[y * w1 + x + 1] + rs
        cnt[(y + 1) * w1 + x + 1] = cnt[y * w1 + x + 1] + rc
    let half = side div 2
    result = newSeq[float32](w * h)
    for i in 0 ..< w * h:
      if hole[i]: continue
      let (x, y) = (i mod w, i div w)
      let (x0, y0, x1, y1) = (max(x - half, 0), max(y - half, 0), min(x + half + 1, w), min(y + half + 1, h))
      let s = sum[y1 * w1 + x1] + sum[y0 * w1 + x0] - sum[y0 * w1 + x1] - sum[y1 * w1 + x0]
      let n = cnt[y1 * w1 + x1] + cnt[y0 * w1 + x0] - cnt[y0 * w1 + x1] - cnt[y1 * w1 + x0]
      result[i] = (if n == 0: 0'f32 else: float32(s / float64(n)))
  (table(1, 0), table(0, 1))

let Kd = [0.6412'f32, 0.8007'f32, 0.6412'f32]

proc downsample(l: Level; cha, ch: int): Level =
  ## The next coarser level: colour from a 3×3 Gaussian (σ = 1.5) over the known cells, texture
  ## features by nearest-neighbour subsampling, and a cell is in the hole when any of the four it
  ## covers is.
  let (w, h) = ((l.w + 1) div 2, (l.h + 1) div 2)
  result = Level(w: w, h: h, img: newSeq[float32](w * h * cha), hole: newSeq[bool](w * h))
  for y in 0 ..< h:
    for x in 0 ..< w:
      let (fx, fy) = (x * 2, y * 2)
      let o = (y * w + x) * cha
      var inHole = false
      for dy in 0 .. 1:
        for dx in 0 .. 1:
          if fx + dx < l.w and fy + dy < l.h and l.hole[(fy + dy) * l.w + fx + dx]: inHole = true
      result.hole[y * w + x] = inHole
      var wsum = 0'f32
      for ky in 0 .. 2:
        for kx in 0 .. 2:
          let (sx, sy) = (fx + kx - 1, fy + ky - 1)
          if sx < 0 or sy < 0 or sx >= l.w or sy >= l.h: continue
          let si = sy * l.w + sx
          if l.hole[si]: continue
          let k = Kd[kx] * Kd[ky]
          for c in 0 ..< ch: result.img[o + c] += l.img[si * cha + c] * k
          wsum += k
      if wsum > 0:
        for c in 0 ..< ch: result.img[o + c] /= wsum
      let si = (fy * l.w + fx) * cha
      for c in ch ..< cha: result.img[o + c] = l.img[si + c]

proc levelCount*(w, h: int; hole: openArray[bool]; r: int): int =
  ## How many levels: `⌈log2(2·No/N)⌉` (at least 1), `No` the hole's thickness in erosions and `N`
  ## the patch side, while the coarsest level stays at least three patches across.
  let n = 2 * r + 1
  let no = min(holeRadius(w, h, hole), 1'u32 shl 20)
  let want = asUsize(rmax(ceil(log2(2.0 * float64(no) / float64(n))), 1.0))
  var (lw, lh) = (w, h)
  result = 1
  while result < want and lw div 2 >= 3 * n and lh div 2 >= 3 * n:
    (lw, lh) = ((lw + 1) div 2, (lh + 1) div 2)
    inc result

proc isValid(s: Solver; x, y: int32): bool {.inline.} =
  x >= 0 and y >= 0 and int(x) < s.w and int(y) < s.h and s.valid[int(y) * s.w + int(x)]

proc randomSource(s: Solver; rng: var PatchRng): (int32, int32) =
  let n = uint64(max(s.validList.len, 1))
  let k = int(rng.next mod n)
  if k < s.validList.len: s.validList[k] else: (0'i32, 0'i32)

proc dist(s: Solver; t, src: (int32, int32); cutoff: float32; known: ptr seq[bool]): float32 =
  ## Mean squared difference between the target patch at `t` (clipped to the grid; with `known`,
  ## only its known cells) and the source patch at `src`. Gives up (∞) once it can't beat `cutoff`.
  let (w, h, cha, r) = (int32(s.w), int32(s.h), s.cha, s.r)
  let full = float32((2 * r + 1) * (2 * r + 1)) * float32(cha)
  var sum = 0'f32
  var n = 0
  for dy in -r .. r:
    let ty = t[1] + dy
    if ty < 0 or ty >= h: continue
    for dx in -r .. r:
      let tx = t[0] + dx
      if tx < 0 or tx >= w: continue
      let tc = int(ty) * s.w + int(tx)
      if known != nil and not known[][tc]: continue
      let (ti, si) = (tc * cha, (int(src[1] + dy) * s.w + int(src[0] + dx)) * cha)
      for c in 0 ..< cha:
        let d = s.img[ti + c] - s.img[si + c]
        sum += d * d
      n += cha
    # The mean can only end up at or above sum / (full patch size).
    if known == nil and sum > cutoff * full: return Inf.float32
  if n == 0: Inf.float32 else: sum / float32(n)

proc patchmatch(s: Solver; nnf: var seq[(int32, int32)]; cost: var seq[float32]; pass: int; seed: uint64;
                radius: int32; ctl: Interrupt) =
  ## One PatchMatch pass over the targets, in row bands (propagation stays in a band, each band
  ## has its own generator).
  const Band = 16
  let w = s.w
  let reverse = pass mod 2 == 1
  let bands = (s.h + Band - 1) div Band
  for bi in 0 ..< bands:
    if ctl.cancelled: return
    var rng = newPatchRng(seed xor (uint64(bi) * 0x2545_F491_4F6C_DD1D'u64) xor (uint64(pass) shl 40))
    let base = bi * Band
    let rows = min(Band, s.h - base)
    let step: int32 = (if reverse: -1 else: 1)
    for ry in 0 ..< rows:
      let ly = (if reverse: rows - 1 - ry else: ry)
      let y = base + ly
      for rx in 0 ..< w:
        let x = (if reverse: w - 1 - rx else: rx)
        if not s.targets[y * w + x]: continue
        let li = y * w + x
        let t = (int32(x), int32(y))
        var best = nnf[li]
        var bc = cost[li]
        template tryCand(cand: (int32, int32)) =
          let cd = cand
          if cd != best and s.isValid(cd[0], cd[1]):
            let d = s.dist(t, cd, bc, nil)
            if d < bc:
              best = cd
              bc = d
        # Propagation from the neighbours visited before, within this band.
        let nx = int32(x) - step
        if nx >= 0 and int(nx) < w:
          let c = nnf[y * w + int(nx)]
          tryCand((c[0] + step, c[1]))
        let nly = int32(ly) - step
        if nly >= 0 and int(nly) < rows:
          let c = nnf[(base + int(nly)) * w + x]
          tryCand((c[0], c[1] + step))
        # Random search in exponentially shrinking windows.
        var rad = radius
        while rad >= 1:
          let cx = best[0] + rng.range(-rad, rad)
          let cy = best[1] + rng.range(-rad, rad)
          tryCand((cx, cy))
          rad = rad div 2
        nnf[li] = best
        cost[li] = bc

proc refreshCosts(s: Solver; nnf: seq[(int32, int32)]; cost: var seq[float32]) =
  ## Refresh every target's distance to its current match (hole cells change between iterations).
  for i in 0 ..< cost.len:
    cost[i] = (if s.targets[i]: s.dist((int32(i mod s.w), int32(i div s.w)), nnf[i], Inf.float32, nil) else: Inf.float32)

iterator sources(s: Solver; x, y: int; nnf: seq[(int32, int32)]; cost: seq[float32]): (int, float32) =
  ## The cells of the patches covering hole cell `(x, y)`: each match's source cell for it, with
  ## that patch's cost.
  let (w, h, r) = (int32(s.w), int32(s.h), s.r)
  for dy in -r .. r:
    let ty = int32(y) + dy
    if ty < 0 or ty >= h: continue
    for dx in -r .. r:
      let tx = int32(x) + dx
      if tx < 0 or tx >= w: continue
      let ti = int(ty) * s.w + int(tx)
      if not s.targets[ti] or cost[ti].classify in {fcNan, fcInf, fcNegInf}: continue
      let sv = nnf[ti]
      let (sx, sy) = (sv[0] - dx, sv[1] - dy)
      if sx >= 0 and sy >= 0 and sx < w and sy < h: yield (int(sy) * s.w + int(sx), cost[ti])

proc vote(s: var Solver; nnf: seq[(int32, int32)]; cost: seq[float32]; uniform: bool): float32 =
  ## Re-estimates the hole by the similarity-weighted vote of the overlapping matches (`uniform`:
  ## all weights equal). Returns the mean absolute change of the colour channels.
  var cs: seq[float32]
  for i, c in cost:
    if s.targets[i] and c.classify notin {fcNan, fcInf, fcNegInf}: cs.add c
  # σ² from the 75th percentile of the match costs, as Wexler et al. and the paper.
  let sigma2 = (if cs.len == 0: 1'f32 else: rmax(kth(cs, min(cs.len * 3 div 4, cs.len - 1)), 1e-6'f32))
  let cha = s.cha
  var updates: seq[(int, seq[float32])]
  for y in 0 ..< s.h:
    for x in 0 ..< s.w:
      if not s.hole[y * s.w + x]: continue
      var v = newSeq[float32](cha)
      var wsum = 0'f32
      for (si, c) in s.sources(x, y, nnf, cost):
        let wt = (if uniform: 1'f32 else: rmax(exp(-c / (2'f32 * sigma2)), 1e-8'f32))
        for k in 0 ..< cha: v[k] += s.img[si * cha + k] * wt
        wsum += wt
      if wsum > 0:
        for k in 0 ..< cha: v[k] /= wsum
        updates.add (y * s.w + x, v)
  var change = 0.0
  var n = 0
  for (i, v) in updates:
    for c in 0 ..< s.ch: change += float64(abs(v[c] - s.img[i * cha + c]))
    n += s.ch
    for c in 0 ..< cha: s.img[i * cha + c] = v[c]
  if n == 0: 0'f32 else: float32(change / float64(n))

proc copyBest(s: var Solver; nnf: seq[(int32, int32)]; cost: seq[float32]): seq[Copied] =
  ## The final reconstruction (the paper's eq. 5): each hole cell copies the source cell of the
  ## lowest-cost patch covering it. Returns `(hole cell, source cell, that patch's cost)`.
  let cha = s.cha
  for y in 0 ..< s.h:
    for x in 0 ..< s.w:
      if not s.hole[y * s.w + x]: continue
      var best = (-1, 0'f32)
      for (si, c) in s.sources(x, y, nnf, cost):
        if best[0] < 0 or c < best[1]: best = (si, c)
      if best[0] >= 0: result.add (y * s.w + x, best[0], best[1])
  for (i, si, _) in result:
    for c in 0 ..< cha: s.img[i * cha + c] = s.img[si * cha + c]

proc blendSeams(s: var Solver; copied: seq[Copied]; textureScale: float32) =
  ## Hides the colour seam along the hole's edge (gradient-domain blending, Pérez et al. 2003): at
  ## each known cell next to the hole, the difference between its value and the one the copied
  ## source predicts there, as a median along the edge, is spread over the hole as a harmonic
  ## membrane and added to the fill.
  const EdgeMedian = 8
  let MaxShift = 0.05'f32
  let (w, h, cha, ch) = (s.w, s.h, s.cha, s.ch)
  if copied.len == 0: return
  let unscale = (if textureScale > 0: 0.5'f32 / textureScale else: 0'f32)
  # Work on the hole's bounding box grown by one cell, which holds the ring of known cells.
  var (x0, y0, x1, y1) = (w, h, 0, 0)
  for (p, _, _) in copied:
    (x0, y0, x1, y1) = (min(x0, p mod w), min(y0, p div w), max(x1, p mod w + 1), max(y1, p div w + 1))
  (x0, y0, x1, y1) = (max(x0 - 1, 0), max(y0 - 1, 0), min(x1 + 1, w), min(y1 + 1, h))
  let (bw, bh) = (max(x1 - x0, 0), max(y1 - y0, 0))
  template local(i: int): int = (i div w - y0) * bw + (i mod w - x0)
  proc inside(x, y: int): bool = x >= 0 and y >= 0 and x < w and y < h
  var shifts: seq[seq[float32]]
  for c in 0 ..< ch:
    var v = newSeq[float32](bw * bh)
    var n = newSeq[uint8](bw * bh)
    for (p, sc, _) in copied:
      let (px, py, sx, sy) = (p mod w, p div w, sc mod w, sc div w)
      for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        let (qx, qy, tx, ty) = (px + dx, py + dy, sx + dx, sy + dy)
        if not inside(qx, qy) or not inside(tx, ty): continue
        let (q, t) = (qy * w + qx, ty * w + tx)
        if s.hole[q]: continue
        let lq = local(q)
        v[lq] += s.img[q * cha + c] - s.img[t * cha + c]
        if n[lq] < 255: inc n[lq]
    for k in 0 ..< v.len:
      if n[k] > 0: v[k] /= float32(n[k])
    # Each edge cell takes the median difference of the edge cells around it, capped at
    # `MaxShift` plus twice the local texture.
    let raw = v
    var near: seq[float32]
    for lq in 0 ..< bw * bh:
      if n[lq] == 0: continue
      let (qx, qy) = (lq mod bw, lq div bw)
      near.setLen(0)
      for yy in max(qy - EdgeMedian, 0) ..< min(qy + EdgeMedian + 1, bh):
        for xx in max(qx - EdgeMedian, 0) ..< min(qx + EdgeMedian + 1, bw):
          if n[yy * bw + xx] > 0: near.add raw[yy * bw + xx]
      if near.len > 0:
        let m = kth(near, near.len div 2)
        let q = (y0 + qy) * w + x0 + qx
        let texture = (s.img[q * cha + ch] + s.img[q * cha + ch + 1]) * unscale
        let cap = MaxShift + 2'f32 * texture
        v[lq] = clamp(m, -cap, cap)
    # Known cells off the ring take the value of the nearest ring cell (breadth first); then the
    # hole is solved.
    var queue = initDeque[int]()
    var seen = newSeq[bool](bw * bh)
    for lq in 0 ..< bw * bh:
      if n[lq] > 0:
        queue.addLast lq
        seen[lq] = true
    template inHole(lq: int): bool = s.hole[(y0 + lq div bw) * w + x0 + lq mod bw]
    while queue.len > 0:
      let lq = queue.popFirst()
      let (x, y) = (lq mod bw, lq div bw)
      for (ok, nb) in [(x > 0, lq - 1), (x + 1 < bw, lq + 1), (y > 0, lq - bw), (y + 1 < bh, lq + bw)]:
        if ok and not seen[nb] and not inHole(nb):
          seen[nb] = true
          v[nb] = v[lq]
          queue.addLast nb
    var unknown = newSeq[bool](bw * bh)
    for lq in 0 ..< bw * bh: unknown[lq] = inHole(lq)
    solveMembrane(bw, bh, unknown, v)
    shifts.add v
  for (p, _, _) in copied:
    let lp = local(p)
    for c in 0 ..< ch: s.img[p * cha + c] += shifts[c][lp]

proc fuseSeams(s: var Solver; copied: seq[Copied]; textureScale: float32; iters: int; ctl: Interrupt) =
  ## Removes the visible seams between areas copied from different places (gradient-domain fusion,
  ## after Darabi et al., *Image Melding*, 2012): where the step across a seam departs from what the
  ## better-matched side's source predicts by more than `SeamContrast` times the local texture, a
  ## correction `δ` is solved for those steps only (`Δδ = div e`, `δ = 0` at known cells,
  ## red-black SOR) and added to the fill.
  let SeamContrast = 3'f32
  let Omega = 1.8'f32
  let (w, h, cha, ch) = (s.w, s.h, s.cha, s.ch)
  var src = newSeq[(int, float32)](w * h)
  for k in 0 ..< src.len: src[k] = (-1, Inf.float32)
  for (p, sp, cp) in copied: src[p] = (sp, cp)
  proc cell(x, y: int): int = (if x >= 0 and y >= 0 and x < w and y < h: y * w + x else: -1)
  let unscale = (if textureScale > 0: 0.5'f32 / textureScale else: 0'f32)
  var dv = newSeq[float32](w * h * ch)
  var any = false
  var e = newSeq[float32](ch)
  for (p, sp, cp) in copied:
    let (px, py) = (p mod w, p div w)
    let texture = (s.img[p * cha + ch] + s.img[p * cha + ch + 1]) * unscale
    for (dx, dy) in [(1, 0), (0, 1)]:
      let q = cell(px + dx, py + dy)
      if q < 0: continue
      let (sq, cq) = src[q]
      if sq < 0: continue
      # The step each side's source predicts (p's source one cell on, q's one cell back).
      let fromP = cell(sp mod w + dx, sp div w + dy)
      if fromP == sq: continue
      let fromQ = cell(sq mod w - dx, sq div w - dy)
      var a, b: int
      if fromP >= 0 and fromQ >= 0:
        if cp <= cq: (a, b) = (sp, fromP) else: (a, b) = (fromQ, sq)
      elif fromP >= 0: (a, b) = (sp, fromP)
      elif fromQ >= 0: (a, b) = (fromQ, sq)
      else: continue
      var es = -0'f32
      for c in 0 ..< ch:
        e[c] = (s.img[b * cha + c] - s.img[a * cha + c]) - (s.img[q * cha + c] - s.img[p * cha + c])
        es += abs(e[c])
      if es / float32(ch) <= SeamContrast * texture: continue
      for c in 0 ..< ch:
        dv[p * ch + c] += e[c]
        dv[q * ch + c] -= e[c]
      any = true
  if not any: return
  # δ_p ← (Σ δ_q − div_p) / deg over the in-grid neighbours, δ = 0 at known cells; only cells
  # within `FuseBand` of a seam are solved for.
  const FuseBand = 16
  var seams = newSeq[bool](w * h)
  for i in 0 ..< w * h:
    for c in 0 ..< ch:
      if dv[i * ch + c] != 0: seams[i] = true
  let band = dilate(w, h, seams, FuseBand)
  var halves: array[2, seq[int]]
  for colour in 0 .. 1:
    for (p, _, _) in copied:
      if band[p] and (p mod w + p div w) mod 2 == colour: halves[colour].add p
  var delta = newSeq[float32](w * h * ch)
  var next: seq[float32]
  for _ in 0 ..< iters:
    if ctl.cancelled: return
    for cells in halves:
      next.setLen(cells.len * ch)
      for k in 0 ..< next.len: next[k] = 0
      for ci, p in cells:
        let (x, y) = (p mod w, p div w)
        var deg = 0'f32
        let o = ci * ch
        for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
          let q = cell(x + dx, y + dy)
          if q < 0: continue
          deg += 1
          for c in 0 ..< ch: next[o + c] += delta[q * ch + c]
        for c in 0 ..< ch:
          let cur = delta[p * ch + c]
          next[o + c] = cur + Omega * ((next[o + c] - dv[p * ch + c]) / rmax(deg, 1'f32) - cur)
      for ci, p in cells:
        for c in 0 ..< ch: delta[p * ch + c] = next[ci * ch + c]
  for (p, _, _) in copied:
    for c in 0 ..< ch: s.img[p * cha + c] += delta[p * ch + c]

proc onionPeel(s: var Solver; nnf: var seq[(int32, int32)]; has: var seq[bool]; seed: uint64; ctl: Interrupt) =
  ## Onion-peel initialisation (the paper's §3.4): fill the hole one boundary layer at a time. Each
  ## known patch centre next to the layer gets a match by its known cells; each layer cell is then
  ## the weighted vote of those matches. Leaves those matches in `nnf` (`has` marks them).
  let (w, h, r, cha) = (s.w, s.h, int(s.r), s.cha)
  var known = newSeq[bool](w * h)
  var unknown: seq[int]
  for i in 0 ..< w * h:
    known[i] = not s.hole[i]
    if s.hole[i]: unknown.add i
  var centre = newSeq[bool](w * h)
  var layerCost = newSeqWithValue(w * h, Inf.float32)
  var layerNo = 0'u64
  while true:
    ctl.check()
    proc neighbourKnown(i: int): bool =
      let (x, y) = (i mod w, i div w)
      (x > 0 and known[i - 1]) or (x + 1 < w and known[i + 1]) or (y > 0 and known[i - w]) or (y + 1 < h and known[i + w])
    var layer: seq[int]
    for i in unknown:
      if neighbourKnown(i): layer.add i
    if layer.len == 0: return
    # Known centres whose patch reaches into the layer.
    var centres: seq[int]
    for i in layer:
      let (x, y) = (i mod w, i div w)
      for cy in max(y - r, 0) .. min(y + r, h - 1):
        for cx in max(x - r, 0) .. min(x + r, w - 1):
          let ci = cy * w + cx
          if known[ci] and not centre[ci]:
            centre[ci] = true
            centres.add ci
    for ci in centres: centre[ci] = false
    var found: seq[(int, (int32, int32), float32)]
    for ci in centres:
      let t = (int32(ci mod w), int32(ci div w))
      var rng = newPatchRng(seed xor (uint64(ci) * 0x9E37_79B9'u64) xor (layerNo shl 48))
      var best = (0'i32, 0'i32)
      var bc = Inf.float32
      template tryCand(cand: (int32, int32)) =
        let cd = cand
        if s.isValid(cd[0], cd[1]):
          let d = s.dist(t, cd, bc, addr known)
          if d < bc:
            best = cd
            bc = d
      if has[ci]: tryCand(nnf[ci])
      # Neighbours' matches, shifted (propagation), and a few random sources.
      for (ox, oy) in [(-1'i32, 0'i32), (1'i32, 0'i32), (0'i32, -1'i32), (0'i32, 1'i32)]:
        let (nx, ny) = (t[0] + ox, t[1] + oy)
        if nx >= 0 and ny >= 0 and int(nx) < w and int(ny) < h and has[int(ny) * w + int(nx)]:
          let sv = nnf[int(ny) * w + int(nx)]
          tryCand((sv[0] - ox, sv[1] - oy))
      for _ in 0 ..< 8: tryCand(s.randomSource(rng))
      for _ in 0 ..< 2:
        var rad = int32(max(w, h))
        while rad >= 1:
          let cx = best[0] + rng.range(-rad, rad)
          let cy = best[1] + rng.range(-rad, rad)
          tryCand((cx, cy))
          rad = rad div 2
      found.add (ci, best, bc)
    var cs: seq[float32]
    for (ci, sv, c) in found:
      if c.classify notin {fcNan, fcInf, fcNegInf}:
        nnf[ci] = sv
        has[ci] = true
        layerCost[ci] = c
        cs.add c
    let sigma2 = (if cs.len == 0: 1'f32 else: rmax(kth(cs, min(cs.len * 3 div 4, cs.len - 1)), 1e-6'f32))
    var values: seq[(int, seq[float32])]
    for i in layer:
      let (x, y) = (i mod w, i div w)
      var v = newSeq[float32](cha)
      var wsum = 0'f32
      for cy in max(y - r, 0) .. min(y + r, h - 1):
        for cx in max(x - r, 0) .. min(x + r, w - 1):
          let ci = cy * w + cx
          if layerCost[ci].classify in {fcNan, fcInf, fcNegInf}: continue
          let sv = nnf[ci]
          let (sx, sy) = (int(sv[0]) + x - cx, int(sv[1]) + y - cy)
          if sx < 0 or sy < 0 or sx >= w or sy >= h: continue
          let si = sy * w + sx
          let wt = rmax(exp(-layerCost[ci] / (2'f32 * sigma2)), 1e-8'f32)
          for k in 0 ..< cha: v[k] += s.img[si * cha + k] * wt
          wsum += wt
      if wsum > 0:
        for k in 0 ..< cha: v[k] /= wsum
        values.add (i, v)
    for (ci, _, _) in found: layerCost[ci] = Inf.float32
    # No known patch reaches the layer: leave the rest to the EM iterations.
    if values.len == 0: return
    for (i, v) in values:
      for k in 0 ..< cha: s.img[i * cha + k] = v[k]
      known[i] = true
    var rest: seq[int]
    for i in unknown:
      if not known[i]: rest.add i
    unknown = rest
    inc layerNo

proc completeWith*(w, h, ch: int; img: seq[float32]; hole: seq[bool]; p: Params; ctl: Interrupt): Option[seq[float32]] =
  ## Completes `hole` in `img` (see the module docs). None when no fully-known patch exists to copy
  ## from; raises `Cancelled` when `ctl` was cancelled. A buffer or mask of the wrong size, or no
  ## hole, comes back unchanged.
  if w <= 0 or h <= 0 or w > high(int) div h: return some(img)
  let n = w * h
  var anyHole = false
  for m in hole:
    if m:
      anyHole = true
      break
  if ch <= 0 or hole.len != n or img.len != n * ch or not anyHole: return some(img)
  let r = max(p.patchRadius, 1)
  let levels = levelCount(w, h, hole, r)
  # Texture features at full resolution, averaged over about a coarsest-level cell (`2^levels`).
  let (tx, ty) = textureFeatures(w, h, ch, img, hole, 1 shl min(levels, 8))
  let cha = ch + 2
  let k = sqrt(rmax(p.lambda, 0'f32))
  var data = newSeqOfCap[float32](n * cha)
  for i in 0 ..< n:
    for c in 0 ..< ch: data.add img[i * ch + c]
    data.add tx[i] * k
    data.add ty[i] * k
  var pyramid = @[Level(w: w, h: h, img: data, hole: hole)]
  while pyramid.len < levels:
    ctl.check()
    let nxt = downsample(pyramid[^1], cha, ch)
    # A level with nothing to copy from can't seed the finer ones: stop above it.
    if validSources(nxt.w, nxt.h, nxt.hole, r)[1].len == 0: break
    pyramid.add nxt
  var totalPx = 0
  for l in pyramid: totalPx += l.w * l.h
  var donePx = 0
  var prev: Option[(int, seq[(int32, int32)])]
  var res: Option[seq[float32]]
  for li in countdown(pyramid.len - 1, 0):
    ctl.check()
    let level = pyramid[li]
    let (lw, lh) = (level.w, level.h)
    let (valid, validList) = validSources(lw, lh, level.hole, r)
    if validList.len == 0: return none(seq[float32])
    let ri = int32(r)
    var targets = newSeq[bool](lw * lh)
    for i in 0 ..< lw * lh:
      if not level.hole[i]: continue
      let (x, y) = (i mod lw, i div lw)
      for yy in max(y - r, 0) .. min(y + r, lh - 1):
        for xx in max(x - r, 0) .. min(x + r, lw - 1): targets[yy * lw + xx] = true
    var s = Solver(w: lw, h: lh, cha: cha, ch: ch, r: ri, img: level.img, hole: level.hole, valid: valid,
                   validList: validList, targets: targets)
    var nnf = newSeq[(int32, int32)](lw * lh)
    var cost = newSeqWithValue(lw * lh, Inf.float32)
    var rng = newPatchRng(p.seed xor (uint64(li) shl 20))
    if prev.isNone:
      # Coarsest level: onion peel, then random matches for the targets it didn't reach.
      var has = newSeq[bool](lw * lh)
      s.onionPeel(nnf, has, p.seed xor 0x0c10_4e00'u64, ctl)
      for i in 0 ..< lw * lh:
        if s.targets[i] and not has[i]: nnf[i] = s.randomSource(rng)
    else:
      let (pw, pnnf) = prev.get
      # Upsample the coarser match field, then rebuild the hole from it.
      for y in 0 ..< lh:
        for x in 0 ..< lw:
          let i = y * lw + x
          if not s.targets[i]: continue
          let pi = (y div 2) * pw + x div 2
          let c = (if pi < pnnf.len: pnnf[pi] else: (-1'i32, -1'i32))
          let cand = (c[0] * 2 + int32(x mod 2), c[1] * 2 + int32(y mod 2))
          nnf[i] =
            if s.isValid(cand[0], cand[1]): cand
            elif s.isValid(c[0] * 2, c[1] * 2): (c[0] * 2, c[1] * 2)
            else: s.randomSource(rng)
      # Uniform weights: the costs only need to be finite.
      for i in 0 ..< cost.len:
        if s.targets[i]: cost[i] = 0
      discard s.vote(nnf, cost, true)
    # Wide random search at the coarsest level, where the structure is decided; finer levels
    # refine the upsampled field locally.
    let coarsest = li + 1 == pyramid.len
    let full = int32(max(lw, lh))
    for it in 0 ..< max(p.maxIters, 1):
      ctl.check()
      s.refreshCosts(nnf, cost)
      let radius = (if coarsest or it == 0: full else: min(8'i32 * (2 * ri + 1), full))
      for pass in 0 ..< max(p.pmPasses, 1):
        ctl.check()
        s.patchmatch(nnf, cost, pass + it * p.pmPasses, p.seed + uint64(li * 1000 + it), radius, ctl)
      ctl.check()
      let change = s.vote(nnf, cost, false)
      ctl.check()
      if change < p.tolerance: break
    if li == 0:
      s.refreshCosts(nnf, cost)
      let copied = s.copyBest(nnf, cost)
      ctl.check()
      s.blendSeams(copied, k)
      ctl.check()
      s.fuseSeams(copied, k, 100, ctl)
      ctl.check()
      res = some(s.img)
    donePx += lw * lh
    ctl.progress(float32(donePx) / float32(max(totalPx, 1)))
    prev = some((lw, nnf))
  if res.isNone: return none(seq[float32])
  let d = res.get
  # The colour channels; known cells exactly as given.
  var o = img
  for i in 0 ..< n:
    if hole[i]:
      for c in 0 ..< ch: o[i * ch + c] = d[i * cha + c]
  some(o)
