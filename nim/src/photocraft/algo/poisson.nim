## Gradient-domain ("Poisson") blending, the core of the Healing Brush and Spot Healing (port of
## `poisson.rs`).
##
## Method: P. Pérez, M. Gangnet, A. Blake, *Poisson Image Editing*, ACM SIGGRAPH 2003. Seamless
## cloning finds `f` over a region Ω minimising `∬_Ω |∇f − v|²` with `f = f*` (the destination) on
## the boundary, which gives `Δf = div v`. With the guidance field taken as the source gradient,
## the solution splits as `f = g + h` where `h` is the *membrane* (harmonic) interpolant of the
## boundary mismatch, so the healed pixels keep the source's texture while their colour and
## lighting are pulled to match the destination around the stroke.
##
## The Laplace equation is solved on the 4-connected pixel grid by successive over-relaxation,
## accelerated *cascadically*: the problem is restricted to a half-resolution grid recursively,
## solved there, bilinearly prolonged as the initial guess, and only a few SOR sweeps are needed per
## level to remove the remaining high-frequency error.
##
## Buffers are interleaved at the API: `w × h × ch` normalised floats in any colour model (channels
## are solved independently).

import std/math
import ../rmath, ../raster

const
  Tol = 2e-5'f32 ## convergence threshold (max per-sweep update) for the SOR sweeps
  FineIters = 30 ## SOR sweeps (cap) on every level above the coarsest
let FineOmega = 1.7'f32

type StopProc = proc(): bool

proc sor(w, h: int; unknown: openArray[bool]; v: var seq[float32]; maxIter: int; omega: float32; stop: StopProc) =
  ## SOR sweeps over the unknown pixels until the largest update drops below `Tol`.
  var cells: seq[(int, uint8)]
  for i in 0 ..< w * h:
    if unknown[i]:
      let (x, y) = (i mod w, i div w)
      let m = uint8(x > 0) or (uint8(x + 1 < w) shl 1) or (uint8(y > 0) shl 2) or (uint8(y + 1 < h) shl 3)
      cells.add (i, m)
  for _ in 0 ..< maxIter:
    if stop(): return
    var maxD = 0'f32
    for (i, m) in cells:
      var s, n: float32
      if m == 0b1111:
        s = v[i - 1] + v[i + 1] + v[i - w] + v[i + w]
        n = 4
      else:
        s = 0
        n = 0
        if (m and 1) != 0:
          s += v[i - 1]
          n += 1
        if (m and 2) != 0:
          s += v[i + 1]
          n += 1
        if (m and 4) != 0:
          s += v[i - w]
          n += 1
        if (m and 8) != 0:
          s += v[i + w]
          n += 1
        if n == 0: continue
      let d = omega * (s / n - v[i])
      v[i] += d
      maxD = rmax(maxD, abs(d))
    if maxD < Tol: break

proc solveRec(w, h: int; unknown: openArray[bool]; v: var seq[float32]; depth: int; stop: StopProc) =
  if stop(): return
  var nUnknown = 0
  var known: seq[float32]
  for i in 0 ..< w * h:
    if unknown[i]: inc nUnknown
    else: known.add v[i]
  if nUnknown == 0 or known.len == 0: return # nothing to interpolate from
  if w >= 8 and h >= 8 and nUnknown > 64 and depth < 16:
    # Restrict: a coarse pixel is known if any of its children is (value = mean of known children).
    let (cw, chh) = ((w + 1) div 2, (h + 1) div 2)
    var cu = newSeq[bool](cw * chh)
    var cv = newSeq[float32](cw * chh)
    for cy in 0 ..< chh:
      for cx in 0 ..< cw:
        var s = 0'f32
        var n = 0'u32
        for (dx, dy) in [(0, 0), (1, 0), (0, 1), (1, 1)]:
          let (x, y) = (cx * 2 + dx, cy * 2 + dy)
          if x < w and y < h and not unknown[y * w + x]:
            s += v[y * w + x]
            inc n
        if n > 0:
          cv[cy * cw + cx] = s / float32(n)
        else:
          cu[cy * cw + cx] = true
    solveRec(cw, chh, cu, cv, depth + 1, stop)
    # Prolong: bilinear sample of the coarse solution as the initial guess.
    for y in 0 ..< h:
      let fy = clamp((float32(y) + 0.5'f32) / 2'f32 - 0.5'f32, 0'f32, float32(chh - 1))
      let y0 = int(floor(fy))
      let y1 = min(y0 + 1, chh - 1)
      let ty = fy - float32(y0)
      for x in 0 ..< w:
        if not unknown[y * w + x]: continue
        let fx = clamp((float32(x) + 0.5'f32) / 2'f32 - 0.5'f32, 0'f32, float32(cw - 1))
        let x0 = int(floor(fx))
        let x1 = min(x0 + 1, cw - 1)
        let tx = fx - float32(x0)
        let a = cv[y0 * cw + x0] + (cv[y0 * cw + x1] - cv[y0 * cw + x0]) * tx
        let b = cv[y1 * cw + x0] + (cv[y1 * cw + x1] - cv[y1 * cw + x0]) * tx
        v[y * w + x] = a + (b - a) * ty
    sor(w, h, unknown, v, FineIters, FineOmega, stop)
  else:
    var s = -0'f32
    for k in known: s += k
    let mean = s / float32(known.len)
    for i in 0 ..< w * h:
      if unknown[i]: v[i] = mean
    sor(w, h, unknown, v, 2000, 1.85'f32, stop)

proc never(): bool = false

proc solveMembrane*(w, h: int; unknown: openArray[bool]; v: var seq[float32]) =
  ## Solves `Δv = 0` on `unknown` pixels with Dirichlet values from the known pixels of `v`
  ## (out-of-grid neighbours are ignored, i.e. a Neumann boundary). `v` holds the known values on
  ## entry and the solution on exit. Cascadic multigrid + SOR.
  if w < 0 or h < 0 or unknown.len != w * h or v.len != w * h:
    raise newException(ValueError, "solveMembrane: buffer sizes do not match the grid")
  solveRec(w, h, unknown, v, 0, never)

proc solveMembraneReference*(w, h: int; unknown: openArray[bool]; v: var seq[float32]; tol: float32; maxIter: int) =
  ## Reference solver for tests: plain Gauss–Seidel from a zero start until the update is below
  ## `tol`.
  if unknown.len != w * h or v.len != w * h: return
  for i in 0 ..< v.len:
    if unknown[i]: v[i] = 0
  for _ in 0 ..< maxIter:
    var maxD = 0'f32
    for y in 0 ..< h:
      for x in 0 ..< w:
        let i = y * w + x
        if not unknown[i]: continue
        var s, n: float32
        for (nx, ny) in [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]:
          if nx >= 0 and ny >= 0 and nx < w and ny < h:
            s += v[ny * w + nx]
            n += 1
        if n > 0:
          let nv = s / n
          maxD = rmax(maxD, abs(nv - v[i]))
          v[i] = nv
    if maxD < tol: break

proc perChannel(w, h, ch: int; f: proc(c: int): seq[float32]): seq[float32] =
  result = newSeq[float32](w * h * ch)
  for c in 0 ..< ch:
    let p = f(c)
    for i, v in p: result[i * ch + c] = v

proc seamlessClone*(w, h, ch: int; src, dst: seq[float32]; mask: seq[bool]): seq[float32] =
  ## Seamless cloning (Pérez et al. 2003, guidance field `v = ∇src`): returns `f` with `f = dst`
  ## outside `mask` and, inside, `src` plus the membrane that removes the boundary mismatch.
  ## `src`/`dst` are interleaved `w × h × ch`; `mask` has `w × h` entries (`true` = healed).
  if src.len != w * h * ch or dst.len != w * h * ch or mask.len != w * h:
    raise newException(ValueError, "seamlessClone: buffer sizes do not match the grid")
  perChannel(w, h, ch, proc(c: int): seq[float32] =
    var hv = newSeq[float32](w * h)
    for i in 0 ..< w * h: hv[i] = (if mask[i]: 0'f32 else: dst[i * ch + c] - src[i * ch + c])
    solveMembrane(w, h, mask, hv)
    result = newSeq[float32](w * h)
    for i in 0 ..< w * h: result[i] = (if mask[i]: src[i * ch + c] + hv[i] else: dst[i * ch + c]))

proc membraneFill*(w, h, ch: int; img: seq[float32]; hole: seq[bool]): seq[float32] =
  ## Membrane fill (no source texture): harmonic interpolation of the known pixels into `hole`.
  if img.len < w * h * ch or hole.len != w * h:
    raise newException(ValueError, "membraneFill: buffer sizes do not match the grid")
  perChannel(w, h, ch, proc(c: int): seq[float32] =
    result = newSeq[float32](w * h)
    for i in 0 ..< w * h: result[i] = (if hole[i]: 0'f32 else: img[i * ch + c])
    solveMembrane(w, h, hole, result))

proc membraneFillWith*(w, h, ch: int; img: seq[float32]; hole: seq[bool]; ctl: Interrupt): seq[float32] =
  ## `membraneFill` that stops at the next SOR sweep once `ctl` is cancelled (raises `Cancelled`).
  ctl.check()
  let stop = proc(): bool = ctl.cancelled
  result = perChannel(w, h, ch, proc(c: int): seq[float32] =
    result = newSeq[float32](w * h)
    for i in 0 ..< w * h: result[i] = (if hole.len == w * h and hole[i]: 0'f32 elif i * ch + c < img.len: img[i * ch + c] else: 0'f32)
    if hole.len == w * h: solveRec(w, h, hole, result, 0, stop))
  ctl.check()
