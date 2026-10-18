## Smart selection's graph cuts (port of the graph parts of `segment/mod.rs`): one min cut over an
## 8-connected pixel grid (Boykov & Jolly, ICCV 2001; max-flow by Boykov & Kolmogorov, PAMI
## 2004), GMM data costs, and turning a working-resolution labelling into a full-resolution region
## with the boundary re-cut in a narrow band.

import std/[math, options]
import ../geom
import ./util, ./segbase, ./gmm, ./maxflow
export segbase, gmm, maxflow

proc addFixedLink(g: var Graph; node: int; fixed: uint8; wt: float32) =
  if fixed == HardFg: g.addTweights(node, wt, 0) # labelling `node` background cuts the link
  else: g.addTweights(node, 0, wt)

proc never(): bool = false

proc gridCutImpl(img: RgbImage; costFg, costBg: openArray[float32]; fixed: openArray[uint8];
                 weight: proc(d2: float32): float32; cancel: proc(): bool; optimized: bool): Option[seq[bool]] =
  let (w, h) = (img.w, img.h)
  if w < 0 or h < 0: return some(newSeq[bool]())
  let n = w * h
  if n >= int(high(uint32)) - 2: return some(newSeq[bool]())
  if img.px.len < n or costFg.len < n or costBg.len < n or fixed.len < n: return some(newSeq[bool]())
  var idx = newSeq[uint32](n)
  var count = 0'u32
  for i in 0 ..< n:
    if fixed[i] == Free:
      idx[i] = count
      count += 1
    else: idx[i] = high(uint32)
  var outp = newSeq[bool](n)
  for i in 0 ..< n: outp[i] = fixed[i] == HardFg
  if count == 0: return some(outp)
  var g = initGraph(int(count), int(count) * 4)
  for i in 0 ..< n:
    if idx[i] != high(uint32): g.addTweights(int(idx[i]), costBg[i], costFg[i])
  # Links in raster order of their first pixel (the Rust crate's sparse traversal visits the same
  # links in the same order).
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = y * w + x
      let a = img.px[i]
      for (dx, dy, len) in Nb:
        let (nx, ny) = (x + dx, y + dy)
        if nx < 0 or nx >= w or ny >= h: continue
        let j = ny * w + nx
        let (fi, fj) = (idx[i] != high(uint32), idx[j] != high(uint32))
        if optimized and not fi and not fj: continue
        let wt = weight(d2(a, img.px[j])) / len
        if fi and fj: g.addEdge(int(idx[i]), int(idx[j]), wt, wt)
        elif fi: g.addFixedLink(int(idx[i]), fixed[j], wt)
        elif fj: g.addFixedLink(int(idx[j]), fixed[i], wt)
  if optimized:
    if g.maxflowWith(cancel).isNone: return none(seq[bool])
  else:
    discard g.maxflowBk
  for i in 0 ..< n:
    if idx[i] != high(uint32): outp[i] = g.inSource(int(idx[i]))
  some(outp)

proc gridCutWith*(img: RgbImage; costFg, costBg: openArray[float32]; fixed: openArray[uint8];
                  weight: proc(d2: float32): float32): seq[bool] =
  ## Binary labelling of a grid by one min cut: minimises `Σ D_p(l_p) + Σ_{p~q} [l_p ≠ l_q]
  ## weight(‖z_p−z_q‖²) / dist(p,q)` over the 8-connected grid. `costFg[i]` / `costBg[i]` are the
  ## data costs of labelling pixel `i` foreground / background; `fixed[i]` is `Free`, `HardFg` or
  ## `HardBg`. Returns `true` for foreground (empty for inconsistent sizes).
  let r = gridCutImpl(img, costFg, costBg, fixed, weight, never, true)
  if r.isSome: r.get else: @[]

proc gridCut*(img: RgbImage; costFg, costBg: openArray[float32]; fixed: openArray[uint8]; gamma, beta: float32): seq[bool] =
  ## `gridCutWith` with GrabCut's weight `γ e^{-β‖z_p−z_q‖²}`.
  gridCutWith(img, costFg, costBg, fixed, proc(d2: float32): float32 = gamma * exp(-beta * d2))

proc gridCutCancellable*(img: RgbImage; costFg, costBg: openArray[float32]; fixed: openArray[uint8];
                         weight: proc(d2: float32): float32; cancel: proc(): bool): Option[seq[bool]] =
  ## `gridCutWith` that stops when `cancel` returns true: none then.
  gridCutImpl(img, costFg, costBg, fixed, weight, cancel, true)

proc gridCutReference*(img: RgbImage; costFg, costBg: openArray[float32]; fixed: openArray[uint8];
                       weight: proc(d2: float32): float32): seq[bool] =
  ## The cut as first written: every link weighed, Boykov–Kolmogorov only (for tests).
  let r = gridCutImpl(img, costFg, costBg, fixed, weight, never, false)
  if r.isSome: r.get else: @[]

proc dataCosts*(img: RgbImage; fg, bg: Gmm): (seq[float32], seq[float32]) =
  ## Data costs `(−ln p(z|fg), −ln p(z|bg))` for every pixel.
  result = (newSeq[float32](img.px.len), newSeq[float32](img.px.len))
  for i, p in img.px:
    result[0][i] = fg.negLog(p)
    result[1][i] = bg.negLog(p)

proc refineBand(sampler: Sampler; window: Rect; step: int; low, band: seq[bool]; lw, lh: int; fg, bg: Gmm;
                mask: var seq[uint8]) =
  ## Full-resolution graph cut restricted to the band blocks (see `finishRegion`).
  const T = 192
  const Halo = 12
  let w = window.width
  proc blk(x, y: int): int = min((y - window.y0) div step, lh - 1) * lw + min((x - window.x0) div step, lw - 1)
  var ty = window.y0
  while ty < window.y1:
    var tx = window.x0
    while tx < window.x1:
      let t = rect(tx, ty, min(tx + T, window.x1), min(ty + T, window.y1))
      tx += T
      let (bx0, by0) = ((t.x0 - window.x0) div step, (t.y0 - window.y0) div step)
      let (bx1, by1) = (min((t.x1 - 1 - window.x0) div step, lw - 1), min((t.y1 - 1 - window.y0) div step, lh - 1))
      var any = false
      for by in by0 .. by1:
        for bx in bx0 .. bx1:
          if band[by * lw + bx]: any = true
      if not any: continue
      let r = t.inflate(Halo).intersect(window)
      let img = sampler.rgb(r)
      let (rw, rh) = (r.width, r.height)
      var fixed = newSeq[uint8](rw * rh)
      var cfg, cbg = newSeq[float32](rw * rh)
      for yy in 0 ..< rh:
        for xx in 0 ..< rw:
          let i = yy * rw + xx
          let b = blk(r.x0 + xx, r.y0 + yy)
          let lab = low[b]
          if not band[b]:
            fixed[i] = (if lab: HardFg else: HardBg)
            continue
          let p = img.px[i]
          # Colour likelihoods plus a weak prior towards the working-resolution label.
          cfg[i] = fg.negLog(p) + (if lab: 0'f32 else: 0.4'f32)
          cbg[i] = bg.negLog(p) + (if lab: 0.4'f32 else: 0'f32)
      let beta = contrastBeta(img)
      let cut = gridCut(img, cfg, cbg, fixed, 50, beta)
      if cut.len < rw * rh: continue
      for yy in 0 ..< t.height:
        let sy = t.y0 - r.y0 + yy
        let sx = t.x0 - r.x0
        let o = (t.y0 - window.y0 + yy) * w + (t.x0 - window.x0)
        for xx in 0 ..< t.width: mask[o + xx] = (if cut[sy * rw + sx + xx]: 255'u8 else: 0'u8)
    ty += T

proc finishRegion*(sampler: Sampler; window: Rect; step: int; low: seq[bool]; lw, lh: int;
                   models: Option[(Gmm, Gmm)]): Option[Region] =
  ## Turns a working-resolution labelling over `window` (sampled with `step`) into a
  ## full-resolution region. With `step > 1` the boundary is re-cut at full resolution in a band
  ## about `step` pixels wide around the upsampled boundary, using the given colour models, tile
  ## by tile. The result is anti-aliased by one pixel.
  let (w, h) = (window.width, window.height)
  var mask = newSeq[uint8](w * h)
  if step <= 1:
    for i in 0 ..< min(mask.len, low.len): mask[i] = (if low[i]: 255'u8 else: 0'u8)
  else:
    # Band blocks: working pixels with a differently labelled pixel within one block.
    var band = newSeq[bool](lw * lh)
    for y in 0 ..< lh:
      for x in 0 ..< lw:
        let v = low[y * lw + x]
        block nb:
          for dy in -1 .. 1:
            for dx in -1 .. 1:
              let (nx, ny) = (x + dx, y + dy)
              if nx >= 0 and ny >= 0 and nx < lw and ny < lh and low[ny * lw + nx] != v:
                band[y * lw + x] = true
                break nb
    for y in 0 ..< h:
      let ly = min(y div step, lh - 1)
      for x in 0 ..< w: mask[y * w + x] = (if low[ly * lw + min(x div step, lw - 1)]: 255'u8 else: 0'u8)
    if models.isSome:
      refineBand(sampler, window, step, low, band, lw, lh, models.get[0], models.get[1], mask)
  antialiasU8(mask, w, h)
  trimRegion(Region(bbox: window, mask: mask))
