## File › Automate › Merge to HDR Pro: exposure estimation, alignment, camera response recovery,
## radiance merge, ghost removal and the 16/8-bit tone mapping methods (port of `hdr.rs`).
##
## * **Alignment**: G. Ward, *Fast, Robust Image Registration for Compositing High Dynamic Range
##   Photographs from Hand-Held Exposures*, JGT 2003 — median threshold bitmaps with an exclusion
##   band around the median, XOR-counted over an image pyramid.
## * **Response curve**: P. Debevec, J. Malik, *Recovering High Dynamic Range Radiance Maps from
##   Photographs*, SIGGRAPH 1997 — `g(Z) = ln E + ln Δt` by weighted linear least squares with a
##   smoothness term and `g(128) = 0`, per channel, on stratified sample pixels.
## * **Merge**: the hat-weighted average of `g(Z) − ln Δt` over the exposures.
## * **Ghosts**: where the per-exposure radiance estimates disagree the result comes from one base
##   exposure only; the mask is feathered.
## * **Tone mapping** for 16/8-bit output: Local Adaptation (`tone.hdrToning`), Exposure and
##   Gamma, Highlight Compression (Reinhard et al. 2002, global operator) and Equalize Histogram
##   (histogram equalisation of log luminance, after Ward Larson et al. 1997).
##
## Pixels are straight RGBA in `0..=1` (display-encoded inputs); radiance is linear.

import std/[math, options, algorithm]
import ../geom, ../rmath, ./util, ./fxutil, ./tone

type
  Px4 = array[4, float32]

  MergeOptions* = object
    exposures*: seq[float64]   ## linear exposure factors (time × gain), one per image
    removeGhosts*: bool
    ghostBase*: Option[int]    ## exposure used where ghosts are removed (none: the best exposed)
    response*: Option[array[3, seq[float64]]] ## response curves per channel (none: recovered)

  Merged* = object
    ## Result of `merge`.
    px*: seq[Px4]              ## linear radiance, straight RGBA
    response*: array[3, seq[float64]]
    ghostBase*: int
    ghostFraction*: float64    ## fraction of pixels handled as ghosts
    stops*: float64            ## dynamic range in stops (1st..99.9th percentile of luminance)

  ToneMethodKind* = enum tmLocalAdaptation, tmExposureGamma, tmHighlightCompression, tmEqualizeHistogram
  ToneMethod* = object
    ## The four 16/8-bit conversion methods of Merge to HDR Pro / HDR Toning.
    case kind*: ToneMethodKind
    of tmLocalAdaptation: toning*: HdrToning
    of tmExposureGamma: exposure*, gamma*: float32
    of tmHighlightCompression, tmEqualizeHistogram: discard

proc luma(c: Px4): float32 {.inline.} = 0.2126'f32 * c[0] + 0.7152'f32 * c[1] + 0.0722'f32 * c[2]

proc hat(z: float64): float64 =
  ## Hat weight on a 0..=255 code value.
  rmax((if z <= 127.5: z else: 255.0 - z), 0.0)

proc inRange(v, lo, hi: float32): bool {.inline.} = v >= lo and v < hi

proc estimateExposures*(imgs: openArray[seq[Px4]]): seq[float64] =
  ## Relative exposures (linear factors, the darkest = 1) estimated from the images themselves:
  ## consecutive images (sorted by brightness) are compared on pixels well exposed in both.
  let n = imgs.len
  var mean = newSeq[float64](n)
  for k, im in imgs:
    var s = 0.0
    for p in im: s += float64(luma(p))
    mean[k] = s / float64(max(im.len, 1))
  var order = newSeq[int](n)
  for k in 0 ..< n: order[k] = k
  order.sort(proc(a, b: int): int = totalCmp(mean[a], mean[b]))
  result = newSeqWithValue(n, 1.0)
  for k in 1 ..< n:
    let (a, b) = (order[k - 1], order[k])
    var ratios: seq[float64]
    var i = 0
    while i < min(imgs[a].len, imgs[b].len):
      let (la, lb) = (luma(imgs[a][i]), luma(imgs[b][i]))
      if inRange(la, 0.08'f32, 0.92'f32) and inRange(lb, 0.08'f32, 0.92'f32):
        ratios.add float64(srgbToLinear(lb)) / float64(rmax(srgbToLinear(la), 1e-6'f32))
      i += 7
    ratios.sort(proc(x, y: float64): int = totalCmp(x, y))
    let r = (if ratios.len == 0: 2.0 else: rmax(ratios[ratios.len div 2], 1.0))
    result[b] = result[a] * r

# ---------- alignment ----------

proc shrink(w, h: int; g: openArray[float32]): (int, int, seq[float32]) =
  let (nw, nh) = (max(w div 2, 1), max(h div 2, 1))
  var o = newSeq[float32](nw * nh)
  template p(xx, yy: int): float32 = g[min(yy, h - 1) * w + min(xx, w - 1)]
  for y in 0 ..< nh:
    for x in 0 ..< nw:
      o[y * nw + x] = 0.25'f32 * (p(2 * x, 2 * y) + p(2 * x + 1, 2 * y) + p(2 * x, 2 * y + 1) + p(2 * x + 1, 2 * y + 1))
  (nw, nh, o)

proc bitmaps(g: openArray[float32]): (seq[bool], seq[bool]) =
  var s = @g
  s.sort(proc(a, b: float32): int = totalCmp(a, b))
  let med = s[s.len div 2]
  var t = newSeq[bool](g.len)
  var e = newSeq[bool](g.len)
  for i, v in g:
    t[i] = v > med
    e[i] = abs(v - med) > 4'f32 / 255'f32
  (t, e)

proc mtbOffset*(w, h: int; reference, img: openArray[float32]; levels: int): (int, int) =
  ## Translation `(dx, dy)` that aligns grey image `img` to `reference` (Ward's MTB), searching up
  ## to `2^levels` px.
  if levels <= 0 or w < 16 or h < 16 or reference.len < w * h or img.len < w * h: return (0, 0)
  let (curX, curY) = block:
    let (sw, sh, sr) = shrink(w, h, reference)
    let (_, _, si) = shrink(w, h, img)
    let (x, y) = mtbOffset(sw, sh, sr, si, levels - 1)
    (x * 2, y * 2)
  let (tr, er) = bitmaps(reference)
  let (ti, ei) = bitmaps(img)
  var best = (high(uint64), 0, 0)
  for dy in -1 .. 1:
    for dx in -1 .. 1:
      let (ox, oy) = (curX + dx, curY + dy)
      var err = 0'u64
      for y in 0 ..< h:
        let sy = y - oy
        if sy < 0 or sy >= h: continue
        for x in 0 ..< w:
          let sx = x - ox
          if sx < 0 or sx >= w: continue
          let (a, b) = (y * w + x, sy * w + sx)
          if tr[a] != ti[b] and er[a] and ei[b]: inc err
      if err < best[0]: best = (err, ox, oy)
  (best[1], best[2])

# ---------- response curve ----------

proc samplePixels*(w, h: int; middle: openArray[Px4]; count: int): seq[int] =
  ## Sample pixel indices for response recovery: a spatial grid filtered to pixels whose
  ## neighbourhood is flat in the middle exposure, stratified by its intensity.
  if middle.len < w * h: return
  var byBin = newSeq[seq[int]](32)
  let step = max(asUsize(rmax(sqrt(float64(w * h div max(count * 40, 1))), 1.0)), 1)
  var y = 2
  while y < max(h - 2, 0):
    var x = 2
    while x < max(w - 2, 0):
      let i = y * w + x
      let l = luma(middle[i])
      var flat = true
      for j in [i - 2, i + 2, i - 2 * w, i + 2 * w]:
        if not (abs(luma(middle[j]) - l) < 0.04'f32):
          flat = false
          break
      if flat: byBin[min(asIdx(l * 31.99'f32), 31)].add i
      x += step
    y += step
  # Round-robin over the bins so every intensity is represented.
  var k = 0
  proc anyLeft(): bool =
    for b in byBin:
      if k < b.len: return true
    false
  while result.len < count and anyLeft():
    for b in byBin:
      if k < b.len:
        let idx = k * 7 mod max(b.len, 1)
        if idx < b.len:
          result.add b[idx]
          if result.len >= count: break
    inc k
  result.sort()
  var dedup: seq[int]
  for v in result:
    if dedup.len == 0 or dedup[^1] != v: dedup.add v
  result = dedup

proc responseCurve*(z: openArray[seq[uint8]]; logDt: openArray[float64]; lambda: float64): seq[float64] =
  ## Debevec–Malik response curve: `z[j][i]` is the code value (0..=255) of sample `i` in
  ## exposure `j`; returns `g(0..=255)` (log exposure per code value, `g(128) = 0`), smoothed by
  ## `lambda`.
  let p = min(z.len, logDt.len)
  let n = (if z.len > 0: z[0].len else: 0)
  var ne = newNormal(256 + n)
  for j in 0 ..< p:
    for i in 0 ..< min(n, z[j].len):
      let zz = int(z[j][i])
      let w = hat(float64(zz)) + 1.0
      ne.add([(zz, 1.0), (256 + i, -1.0)], logDt[j], w)
  ne.add([(128, 1.0)], 0.0, 100.0)
  for k in 1 ..< 255:
    let w = sqrt(lambda) * (hat(float64(k)) + 1.0)
    ne.add([(k - 1, 1.0), (k, -2.0), (k + 1, 1.0)], 0.0, w)
  let x = ne.solve()
  if x.len < 256:
    # Fall back to the sRGB curve.
    result = newSeq[float64](256)
    for k in 0 ..< 256:
      result[k] = ln(float64(rmax(srgbToLinear(float32(k) / 255'f32), 1e-5'f32))) - ln(float64(srgbToLinear(128'f32 / 255'f32)))
    return
  result = x[0 ..< 256]
  # Enforce monotonicity (noise at the extremes can fold the curve).
  for k in countdown(127, 0): result[k] = rmin(result[k], result[k + 1] - 1e-4)
  for k in 129 ..< 256: result[k] = rmax(result[k], result[k - 1] + 1e-4)

proc gAt(g: openArray[float64]; v: float32): float64 =
  let x = float64(clamp(v, 0'f32, 1'f32) * 255'f32)
  let i = min(asUsize(floor(x)), 254)
  let t = x - float64(i)
  g[i] * (1.0 - t) + g[i + 1] * t

proc merge*(w, h: int; imgs: openArray[seq[Px4]]; opts: MergeOptions): Merged =
  ## Merges aligned exposures (`w × h` each) into a radiance map, scaled so the base exposure's
  ## well-exposed pixels keep their linear values.
  let p = min(imgs.len, opts.exposures.len)
  let np = w * h
  for k in 0 ..< p:
    if imgs[k].len < np: raise newException(ValueError, "merge: an exposure is smaller than the image")
  if p == 0: return Merged(px: newSeq[Px4](np))
  var logDt = newSeq[float64](opts.exposures.len)
  for k, e in opts.exposures: logDt[k] = ln(rmax(e, 1e-12))
  proc well(im: seq[Px4]): int =
    var i = 0
    while i < im.len:
      if inRange(luma(im[i]), 0.1'f32, 0.9'f32): inc result
      i += 3
  var base = 0
  if opts.ghostBase.isSome and opts.ghostBase.get < p and opts.ghostBase.get >= 0: base = opts.ghostBase.get
  else:
    # `max_by_key`: the last of equal maxima.
    var bestW = -1
    for j in 0 ..< p:
      let v = well(imgs[j])
      if v >= bestW:
        bestW = v
        base = j
  var response: array[3, seq[float64]]
  if opts.response.isSome: response = opts.response.get
  else:
    var order = newSeq[int](p)
    for k in 0 ..< p: order[k] = k
    order.sort(proc(a, b: int): int = totalCmp(opts.exposures[a], opts.exposures[b]))
    let mid = order[p div 2]
    let samples = samplePixels(w, h, imgs[mid], 160)
    for c in 0 .. 2:
      var z = newSeq[seq[uint8]](p)
      for j in 0 ..< p:
        for i in samples: z[j].add asU8(round(clamp(imgs[j][i][c], 0'f32, 1'f32) * 255'f32))
      response[c] = responseCurve(z, logDt[0 ..< p], 40.0)
  for c in 0 .. 2:
    if response[c].len < 256: raise newException(ValueError, "merge: a response curve needs 256 entries")
  var px = newSeq[Px4](np)
  var ghost = newSeq[float32](np)
  for i in 0 ..< np:
    var alpha = 0'f32
    for c in 0 .. 2:
      var (num, den) = (0.0, 0.0)
      for j in 0 ..< p:
        let v = imgs[j][i][c]
        let wt = hat(float64(clamp(v, 0'f32, 1'f32)) * 255.0)
        num += wt * (gAt(response[c], v) - logDt[j])
        den += wt
      let le =
        if den > 1e-6: num / den
        else:
          # Saturated everywhere: darkest exposure if bright, brightest if dark.
          var s = 0'f32
          for j in 0 ..< p: s += imgs[j][i][c]
          let bright = s / float32(p) > 0.5'f32
          # `min_by`: the first of equal minima.
          var jb = 0
          for j in 1 ..< p:
            let ord = (if bright: totalCmp(logDt[j], logDt[jb]) else: totalCmp(logDt[jb], logDt[j]))
            if ord < 0: jb = j
          gAt(response[c], imgs[jb][i][c]) - logDt[jb]
      px[i][c] = float32(exp(le))
    for j in 0 ..< p: alpha = rmax(alpha, imgs[j][i][3])
    px[i][3] = alpha
  var ghostFraction = 0.0
  if opts.removeGhosts and p > 1:
    # Weighted variance of per-exposure log radiance (luminance).
    var score = newSeq[float32](np)
    for i in 0 ..< np:
      var (s, s2, sw) = (0.0, 0.0, 0.0)
      for j in 0 ..< p:
        let q = imgs[j][i]
        let l = luma(q)
        let wt = hat(float64(l) * 255.0)
        if wt < 20: continue
        var lr = 0.0
        for c in 0 .. 2: lr += 0.33 * (gAt(response[c], q[c]) - logDt[j])
        s += wt * lr
        s2 += wt * lr * lr
        sw += wt
      if sw <= 0: continue
      let m = s / sw
      score[i] = float32(sqrt(rmax(s2 / sw - m * m, 0.0)))
    for i in 0 ..< np: ghost[i] = (if score[i] > 0.35'f32: 1'f32 else: 0'f32)
    # Grow and feather the mask.
    let r = rmax(float32(max(w, h)) / 200'f32, 2'f32)
    gaussBlurN(ghost, w, h, 1, r)
    for g in ghost.mitems: g = rmin(g * 3'f32, 1'f32)
    var cnt = 0
    for g in ghost:
      if g > 0.5'f32: inc cnt
    ghostFraction = float64(cnt) / float64(max(np, 1))
    let ld = logDt[base]
    for i in 0 ..< np:
      if ghost[i] <= 0: continue
      let q = imgs[base][i]
      for c in 0 .. 2:
        let e = float32(exp(gAt(response[c], q[c]) - ld))
        px[i][c] = px[i][c] * (1'f32 - ghost[i]) + e * ghost[i]
  # Scale per channel so the base exposure's well-exposed pixels map to their linear values.
  for c in 0 .. 2:
    var r: seq[float64]
    var i = 0
    while i < np:
      let v = imgs[base][i][c]
      if inRange(v, 0.15'f32, 0.85'f32) and px[i][c] > 0:
        r.add float64(srgbToLinear(v)) / (float64(px[i][c]) * opts.exposures[base])
      i += 37
    r.sort(proc(x, y: float64): int = totalCmp(x, y))
    let k = (if r.len == 0: 1.0 else: r[r.len div 2]) * opts.exposures[base]
    for q in px.mitems: q[c] = float32(float64(q[c]) * k)
  var l: seq[float32]
  var i = 0
  while i < px.len:
    if px[i][3] > 0: l.add rmax(luma(px[i]), 1e-9'f32)
    i += 13
  l.sort(proc(x, y: float32): int = totalCmp(x, y))
  let stops = (if l.len == 0: 0.0
               else: log2(float64(l[asUsize(float64(l.len) * 0.999) mod l.len]) / float64(l[l.len div 100])))
  Merged(px: px, response: response, ghostBase: base, ghostFraction: ghostFraction, stops: stops)

# ---------- tone mapping ----------

proc toneMap*(px: var openArray[Px4]; w, h: int; m: ToneMethod) =
  ## Tone maps linear radiance to display-encoded (sRGB curve) values in `0..=1`.
  var cnt = 0
  var ls = 0.0
  for q in px:
    if q[3] > 0:
      inc cnt
      ls += ln(float64(rmax(luma(q), 1e-6'f32)))
  let n = max(cnt, 1)
  let logMean = float32(exp(ls / float64(n)))
  case m.kind
  of tmLocalAdaptation:
    # Key the scene to middle grey, compress in log luminance, then encode.
    let k = 0.18'f32 / rmax(logMean, 1e-6'f32)
    for q in px.mitems:
      for c in 0 .. 2: q[c] *= k
    hdrToning(px, w, h, m.toning)
    for q in px.mitems:
      for c in 0 .. 2: q[c] = linearToSrgb(clamp(q[c], 0'f32, 1'f32))
  of tmExposureGamma:
    let k = pow(2'f32, m.exposure)
    let g = clamp(m.gamma, 0.1'f32, 10'f32)
    for q in px.mitems:
      for c in 0 .. 2: q[c] = pow(linearToSrgb(clamp(q[c] * k, 0'f32, 1'f32)), 1'f32 / g)
  of tmHighlightCompression:
    let k = 0.18'f32 / rmax(logMean, 1e-6'f32)
    for q in px.mitems:
      let l = luma(q) * k
      let ld = l / (1'f32 + l)
      let r = (if l > 1e-9'f32: ld / l * k else: 0'f32)
      for c in 0 .. 2: q[c] = linearToSrgb(clamp(q[c] * r, 0'f32, 1'f32))
  of tmEqualizeHistogram:
    const Bins = 1024
    var lv = newSeq[float32](px.len)
    for i, q in px: lv[i] = ln(rmax(luma(q), 1e-7'f32))
    var (lo, hi) = (F32Max, -F32Max)
    for i, q in px:
      if q[3] > 0:
        lo = rmin(lo, lv[i])
        hi = rmax(hi, lv[i])
    let span = rmax(hi - lo, 1e-6'f32)
    var hist = newSeq[float64](Bins)
    for i, q in px:
      if q[3] > 0:
        let b = asIdx(((lv[i] - lo) / span) * float32(Bins - 1))
        if b < Bins: hist[b] += 1.0
    var tot = 0.0
    for v in hist: tot += v
    let total = rmax(tot, 1.0)
    var cdf = newSeq[float32](Bins)
    var acc = 0.0
    for b in 0 ..< Bins:
      acc += hist[b]
      cdf[b] = float32(acc / total)
    for i in 0 ..< px.len:
      let target = cdf[min(asIdx(clamp((lv[i] - lo) / span, 0'f32, 1'f32) * float32(Bins - 1)), Bins - 1)]
      # Display value from the CDF (encoded), keep colour ratios in linear light.
      let tl = srgbToLinear(target)
      let l = rmax(luma(px[i]), 1e-7'f32)
      for c in 0 .. 2: px[i][c] = linearToSrgb(clamp(px[i][c] / l * tl, 0'f32, 1'f32))
