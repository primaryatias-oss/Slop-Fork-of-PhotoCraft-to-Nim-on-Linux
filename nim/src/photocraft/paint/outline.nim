## Brush tip outlines for the painting cursor (port of `paint/src/outline.rs`): closed rings in
## units of the brush radius (1 = size / 2), screen-oriented (x right, y down), ready for the UI to
## scale by the radius.

import std/[math, options, tables, algorithm]
import ../rmath
import ./brush, ./tile

const
  EllipseSegments = 64   ## segments in the outline of a computed round tip
  MaxPoints = 512        ## budget for all traced ring vertices; the smallest rings go first
  MinArea = 0.9'f32      ## a traced ring smaller than this many bitmap pixels squared is noise
  FullThreshold = 1'f32 / 255'f32 ## "full size" outlines everything above 1/255
  HalfThreshold = 0.5'f32        ## the normal cursor outlines the 50 % contour

type
  Pt2 = array[2, float32]
  Ring* = seq[array[2, float32]]

proc isFin(v: float32): bool {.inline.} = v == v and v != Inf and v != NegInf

proc sample(t: GrayTile; x, y: float32): float32 =
  ## Bilinear sample with zero outside (tips fade to nothing at their edges).
  let (x0f, y0f) = (floor(x), floor(y))
  let (tx, ty) = (x - x0f, y - y0f)
  let (x0, y0) = (asI64(x0f), asI64(y0f))
  template get(xx, yy: int64): float32 =
    (if xx < 0 or yy < 0 or xx >= int64(t.width) or yy >= int64(t.height): 0'f32 else: t.get(uint32(xx), uint32(yy)))
  let a = get(x0, y0) + (get(x0 + 1, y0) - get(x0, y0)) * tx
  let b = get(x0, y0 + 1) + (get(x0 + 1, y0 + 1) - get(x0, y0 + 1)) * tx
  a + (b - a) * ty

proc area(r: openArray[Pt2]): float32 =
  ## Shoelace area of a ring.
  if r.len < 3: return 0
  var s = 0'f32
  for i in 0 ..< r.len:
    let (a, b) = (r[i], r[(i + 1) mod r.len])
    s += a[0] * b[1] - b[0] * a[1]
  abs(s * 0.5'f32)

proc key(p: Pt2): (int64, int64) {.inline.} = (asI64(round(p[0] * 4096'f32)), asI64(round(p[1] * 4096'f32)))

proc stitch(segs: seq[(Pt2, Pt2)]): seq[Ring] =
  ## Link segments end-to-end into closed rings (quantised endpoint keys absorb float noise).
  var ends = initTable[(int64, int64), seq[int]]()
  for i, (a, b) in segs:
    ends.mgetOrPut(key(a), @[]).add i
    ends.mgetOrPut(key(b), @[]).add i
  var used = newSeq[bool](segs.len)
  for s in 0 ..< segs.len:
    if used[s]: continue
    used[s] = true
    var ring: Ring = @[segs[s][0], segs[s][1]]
    while true:
      let tail = ring[^1]
      var next = -1
      ends.withValue(key(tail), c):
        for i in c[]:
          if not used[i]:
            next = i
            break
      if next < 0: break
      used[next] = true
      let (a, b) = segs[next]
      let p = if key(a) == key(tail): b else: a
      if key(p) == key(ring[0]): break
      ring.add p
    if ring.len >= 3: result.add ring

proc march(v: seq[float32]; w, h: int; thr: float32): seq[Ring] =
  ## Marching squares over `v` (w by h samples at lattice nodes), outside treated as 0 so every
  ## contour closes; returns rings in node coordinates.
  template val(x, y: int): float32 = (if x < 0 or y < 0 or x >= w or y >= h: 0'f32 else: v[y * w + x])
  proc lerp(v0, v1: float32; a, b: (float32, float32)): Pt2 =
    let t = if abs(v1 - v0) < 1e-9'f32: 0.5'f32 else: clamp((thr - v0) / (v1 - v0), 0'f32, 1'f32)
    [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t]
  var segs: seq[(Pt2, Pt2)]
  for y in -1 ..< h:
    for x in -1 ..< w:
      let (va, vb, vc, vd) = (val(x, y), val(x + 1, y), val(x + 1, y + 1), val(x, y + 1))
      let idx = (if va >= thr: 1 else: 0) or (if vb >= thr: 2 else: 0) or (if vc >= thr: 4 else: 0) or (if vd >= thr: 8 else: 0)
      if idx == 0 or idx == 15: continue
      let (fx, fy) = (float32(x), float32(y))
      let top = lerp(va, vb, (fx, fy), (fx + 1, fy))
      let right = lerp(vb, vc, (fx + 1, fy), (fx + 1, fy + 1))
      let bottom = lerp(vd, vc, (fx, fy + 1), (fx + 1, fy + 1))
      let left = lerp(va, vd, (fx, fy), (fx, fy + 1))
      let mid = (va + vb + vc + vd) * 0.25'f32 >= thr
      case idx
      of 1, 14: segs.add (top, left)
      of 2, 13: segs.add (top, right)
      of 3, 12: segs.add (right, left)
      of 4, 11: segs.add (right, bottom)
      of 6, 9: segs.add (top, bottom)
      of 7, 8: segs.add (left, bottom)
      of 5, 10:
        let opposite = (idx == 5) != mid
        if opposite:
          segs.add (top, right)
          segs.add (left, bottom)
        else:
          segs.add (top, left)
          segs.add (right, bottom)
      else: discard
  stitch(segs)

proc segDist2(p, a, b: Pt2): float32 =
  ## Squared distance from `p` to the segment `a`-`b`.
  let (abx, aby) = (b[0] - a[0], b[1] - a[1])
  let t = clamp(((p[0] - a[0]) * abx + (p[1] - a[1]) * aby) / rmax(abx * abx + aby * aby, 1e-12'f32), 0'f32, 1'f32)
  let (ex, ey) = (p[0] - a[0] - abx * t, p[1] - a[1] - aby * t)
  ex * ex + ey * ey

proc dp(pts: openArray[Pt2]; tol: float32; outp: var Ring) =
  ## Iterative Douglas-Peucker: keep the points of `pts` farther than `tol` from the chord.
  let n = pts.len
  if n == 0: return
  if n < 3:
    for p in pts: outp.add p
    return
  var keep = newSeq[bool](n)
  keep[0] = true
  keep[n - 1] = true
  var stack = @[(0, n - 1)]
  while stack.len > 0:
    let (i, j) = stack.pop()
    if j <= i + 1: continue
    var (dmax, k) = (0'f32, i)
    for m in i + 1 ..< j:
      let d = segDist2(pts[m], pts[i], pts[j])
      if d > dmax:
        dmax = d
        k = m
    if dmax > tol * tol:
      keep[k] = true
      stack.add (i, k)
      stack.add (k, j)
  for i, p in pts:
    if keep[i]: outp.add p

proc simplify(ring: Ring; tol: float32): Ring =
  ## Douglas-Peucker on a closed ring (split at the point farthest from the first).
  if ring.len <= 4: return ring
  var (far, best) = (0, 0'f32)
  for i in 1 ..< ring.len:
    let p = ring[i]
    let (ddx, ddy) = (p[0] - ring[0][0], p[1] - ring[0][1])
    let d = ddx * ddx + ddy * ddy
    if d > best:
      best = d
      far = i
  var open = ring[far .. ^1]
  open.add ring[0]
  dp(ring.toOpenArray(0, far), tol, result)
  if result.len > 0: discard result.pop()
  dp(open, tol, result)
  if result.len > 0: discard result.pop()

proc trace(t: GrayTile; thr: float32; sn, cs, fx, fy, ro: float32): Option[seq[Ring]] =
  ## Trace the contour(s) above `thr` (resampled ~2x, capped at 160 px a side) and map them with
  ## the same tip transform as `BrushContext.rasterize`.
  let (tw, th) = (float32(t.width), float32(t.height))
  let big = rmax(tw, th)
  let f = if big <= 160: 2'f32 else: 160'f32 / big
  let (gw, gh) = (max(asIdx(ceil(tw * f)), 1), max(asIdx(ceil(th * f)), 1))
  var vals = newSeq[float32](gw * gh)
  for gy in 0 ..< gh:
    for gx in 0 ..< gw:
      vals[gy * gw + gx] = sample(t, (float32(gx) + 0.5'f32) / f - 0.5'f32, (float32(gy) + 0.5'f32) / f - 0.5'f32)
  var rings: seq[Ring]
  for r in march(vals, gw, gh, thr):
    if area(r) >= MinArea * f * f: rings.add r
  if rings.len == 0: return none(seq[Ring])
  rings.sort(proc(a, b: Ring): int = cmp(area(b), area(a)))
  var tol = 0.5'f32
  var thin: seq[Ring]
  while true:
    thin.setLen(0)
    var total = 0
    for r in rings:
      thin.add simplify(r, tol)
      total += thin[^1].len
    if total <= MaxPoints or tol >= 8: break
    tol *= 1.5'f32
  var budget = MaxPoints
  var kept: seq[Ring]
  for r in thin:
    if r.len >= 3 and r.len <= budget:
      budget -= r.len
      kept.add r
  if kept.len == 0: return none(seq[Ring])
  let s = 2'f32 / big
  var mapped: seq[Ring]
  for r in kept:
    var m: Ring
    for g in r:
      let (px, py) = ((g[0] + 0.5'f32) / f - 0.5'f32, (g[1] + 0.5'f32) / f - 0.5'f32)
      let (u, v) = ((px - tw / 2'f32) * s * fx, -(py - th / 2'f32) * s * ro * fy)
      let (ux, uy) = (u * cs - v * sn, u * sn + v * cs)
      if not isFin(ux) or not isFin(uy): return none(seq[Ring])
      m.add [ux, -uy]
    mapped.add m
  some(mapped)

proc tipOutline*(b: BrushSettings; full: bool): Option[seq[Ring]] =
  ## The tip outline as closed rings, or none when the tip has no usable shape. `full` outlines the
  ## whole tip; otherwise the 50 % contour (the normal brush tip cursor).
  if not isFin(b.size) or b.size <= 0 or not isFin(b.angle) or not isFin(b.roundness): return none(seq[Ring])
  let ro = rmin(rmax(clamp(b.roundness, 0'f32, 1'f32), 0.5'f32 / (b.size / 2'f32)), 1'f32)
  let rad = toRadians(b.angle)
  let (sn, cs) = (sin(rad), cos(rad))
  let (fx, fy) = ((if b.flipX: -1'f32 else: 1'f32), (if b.flipY: -1'f32 else: 1'f32))
  case b.tip.kind
  of tkRound:
    # The normal cursor is the 50 % contour: the soft tip falls off as (1 - t²)⁴.
    let f = if full: 1'f32 else: halfCoverageRadius(1, b.hardness)
    var ring: Ring
    for i in 0 ..< EllipseSegments:
      let t = float32(TAU) * float32(i) / float32(EllipseSegments)
      # Tip-local (y up), rotated CCW by the angle, then flipped to screen y down.
      let (lx, ly) = (cos(t) * f, sin(t) * f * ro)
      ring.add [lx * cs - ly * sn, -(lx * sn + ly * cs)]
    some(@[ring])
  of tkSampled, tkStored:
    let t = b.tip.bitmap
    if t.isNone or not t.get.isValid: return none(seq[Ring])
    trace(t.get, (if full: FullThreshold else: HalfThreshold), sn, cs, fx, fy, ro)
