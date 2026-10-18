## GrabCut and rectangle Object Selection (port of `segment/grabcut.rs`).
##
## C. Rother, V. Kolmogorov and A. Blake, "GrabCut: Interactive Foreground Extraction using
## Iterated Graph Cuts", SIGGRAPH 2004: a trimap (definite / probable foreground and background),
## 5-component full-covariance GMMs per side, and iterated "assign components → learn GMMs → min
## cut" steps with the contrast-sensitive smoothness term `γ e^{−β‖z_m − z_n‖²} / dist(m, n)`.

import std/[math, options]
import ../geom
import ./segment

const
  Bg* = 0'u8   ## definite background
  Fg* = 1'u8   ## definite foreground
  PrBg* = 2'u8 ## probable background
  PrFg* = 3'u8 ## probable foreground
  Gamma* = 50'f32 ## GrabCut's smoothness weight
  K* = 5          ## components per GMM
  MaxSamples = 40_000

proc isFg(t: uint8): bool = t == Fg or t == PrFg

proc grabcut*(img: RgbImage; trimap: var seq[uint8]; iters: int): Option[(Gmm, Gmm)] =
  ## Runs `iters` GrabCut iterations, updating the probable labels of `trimap` in place. Returns
  ## the final (foreground, background) models, or none if either side has no pixels.
  let beta = contrastBeta(img)
  var fixed = newSeq[uint8](trimap.len)
  for i, t in trimap: fixed[i] = (if t == Fg: HardFg elif t == Bg: HardBg else: Free)
  var models = none((Gmm, Gmm))
  for _ in 0 ..< max(iters, 1):
    var fs, bs: seq[array[3, float32]]
    for i in 0 ..< min(img.px.len, trimap.len):
      if isFg(trimap[i]): fs.add img.px[i] else: bs.add img.px[i]
    let (fss, bss) = (subsample(fs, MaxSamples), subsample(bs, MaxSamples))
    var fg, bg: Gmm
    if models.isNone:
      let f = fit(fss, K, DefaultReg)
      if f.isNone: return none((Gmm, Gmm))
      let b = fit(bss, K, DefaultReg)
      if b.isNone: return none((Gmm, Gmm))
      (fg, bg) = (f.get, b.get)
    else:
      let (f0, b0) = models.get
      fg = f0.refit(fss).get(f0)
      bg = b0.refit(bss).get(b0)
    let (cf, cb) = dataCosts(img, fg, bg)
    let cut = gridCut(img, cf, cb, fixed, Gamma, beta)
    var changed = 0
    for i in 0 ..< min(trimap.len, cut.len):
      let t = trimap[i]
      if t == PrBg or t == PrFg:
        let nt = (if cut[i]: PrFg else: PrBg)
        if nt != t: inc changed
        trimap[i] = nt
    models = some((fg, bg))
    if changed * 2000 < trimap.len: break
  models

proc objectSelect*(sampler: Sampler; canvas, rect0: Rect; workPx: int): Option[Region] =
  ## Object Selection (rectangle mode): GrabCut initialised from `rect` (outside = background,
  ## inside = probable foreground). Runs on a working copy of about `workPx` pixels, then re-cuts
  ## the boundary at full resolution. `canvas` bounds the analysis.
  let r = rect0.intersect(canvas)
  if r.width < 2 or r.height < 2: return none(Region)
  let margin = max(asI32(float64(ceil(float32(max(r.width, r.height)) * 0.15'f32))), 8)
  let window = r.inflate(margin).intersect(canvas)
  let step = scaleFor(window, workPx)
  let img = sampler.rgbScaled(window, step)
  let (lw, lh) = (img.w, img.h)
  var trimap = newSeq[uint8](lw * lh)
  var anyBg = false
  for y in 0 ..< lh:
    for x in 0 ..< lw:
      # Working pixel (x, y) covers doc pixels [x0 + x·step, …); its centre decides.
      let (cx, cy) = (window.x0 + x * step + step div 2, window.y0 + y * step + step div 2)
      if r.contains(cx, cy): trimap[y * lw + x] = PrFg
      else: anyBg = true
  if not anyBg:
    # The rectangle covers the whole canvas: use a thin outer ring as background.
    let ring = max(min(lw, lh) div 50, 1)
    for y in 0 ..< lh:
      for x in 0 ..< lw:
        if x < ring or y < ring or x + ring >= lw or y + ring >= lh: trimap[y * lw + x] = Bg
  let models = grabcut(img, trimap, 8)
  if models.isNone: return none(Region)
  var low = newSeq[bool](lw * lh)
  for i, t in trimap: low[i] = isFg(t)
  let cleaned = cleanMask(low, lw, lh, 0.1, 0.002)
  finishRegion(sampler, window, step, cleaned, lw, lh, models)
