## Point-level path editing (Direct Selection, Convert Point, Add/Delete Anchor) and hit testing.
## Knots are addressed as `[subpath, knot]`. Errors are reported as `ValueError`.

import std/[math, algorithm]
import ../doc

type
  Handle* = enum hIn = "in", hOut = "out"
  KnotRef* = array[2, int]
  HitKind* = enum hitAnchor, hitHandle, hitSegment
  Hit* = object
    kind*: HitKind
    knot*: KnotRef
    handle*: Handle
    t*: float64

proc parseHandle*(s: string): (bool, Handle) =
  case s
  of "in": (true, hIn)
  of "out": (true, hOut)
  else: (false, hIn)

proc addD(p: Point; d: array[2, float64]): Point = point(p.x + d[0], p.y + d[1])
proc dist(a, b: Point): float64 = hypot(a.x - b.x, a.y - b.y)

proc knotPtr(path: var Path; r: KnotRef): ptr Knot =
  if r[0] < 0 or r[0] >= path.subpaths.len or r[1] < 0 or r[1] >= path.subpaths[r[0]].knots.len:
    raise newException(ValueError, "no knot " & $r[1] & " in subpath " & $r[0])
  addr path.subpaths[r[0]].knots[r[1]]

proc segmentEnd(path: Path; r: KnotRef): KnotRef =
  if r[0] < 0 or r[0] >= path.subpaths.len: raise newException(ValueError, "no subpath " & $r[0])
  let sp = path.subpaths[r[0]]
  let n = sp.knots.len
  let k = r[1]
  if k >= 0 and k < n and k + 1 < n: [r[0], k + 1]
  elif k >= 0 and k < n and sp.closed and n > 1: [r[0], 0]
  else: raise newException(ValueError, "no segment leaves knot " & $k & " of subpath " & $r[0])

proc handleOf(k: Knot; h: Handle): Point = (if h == hIn: k.inCtrl else: k.outCtrl)
proc setHandle(k: var Knot; h: Handle; p: Point) =
  if h == hIn: k.inCtrl = p else: k.outCtrl = p

proc keepSmooth(k: var Knot; h: Handle) =
  if not k.smooth: return
  let a = k.anchor
  let moved = k.handleOf(h)
  let o = if h == hIn: hOut else: hIn
  let opp = k.handleOf(o)
  let (ln, d) = (dist(opp, a), dist(moved, a))
  if ln > 1e-9 and d > 1e-9:
    k.setHandle(o, point(a.x - (moved.x - a.x) / d * ln, a.y - (moved.y - a.y) / d * ln))

proc moveAnchors*(path: var Path; anchors: openArray[KnotRef]; d: array[2, float64]) =
  var list = @anchors
  list.sort(proc (a, b: KnotRef): int = cmp((a[0], a[1]), (b[0], b[1])))
  var uniq: seq[KnotRef]
  for r in list:
    if uniq.len == 0 or uniq[^1] != r: uniq.add r
  for r in uniq: discard knotPtr(path, r)
  for r in uniq:
    let k = knotPtr(path, r)
    k[] = Knot(anchor: addD(k.anchor, d), inCtrl: addD(k.inCtrl, d), outCtrl: addD(k.outCtrl, d), smooth: k.smooth)

proc moveHandle*(path: var Path; r: KnotRef; h: Handle; to: Point; independent: bool) =
  let k = knotPtr(path, r)
  k[].setHandle(h, to)
  if independent: k.smooth = false
  keepSmooth(k[], h)

proc convertPoint*(path: var Path; r: KnotRef; hasOut: bool; o: Point) =
  ## Makes the knot smooth with its out handle at `o` (in mirrored), or a corner when not `hasOut`.
  let k = knotPtr(path, r)
  let a = k.anchor
  k[] = if hasOut: smoothKnot(a, point(2 * a.x - o.x, 2 * a.y - o.y), o) else: cornerKnot(a.x, a.y)

proc bendSegment*(path: var Path; r: KnotRef; t0: float64; d: array[2, float64]) =
  let e = segmentEnd(path, r)
  let a = knotPtr(path, r)[]
  let b = knotPtr(path, e)[]
  if dist(a.outCtrl, a.anchor) < 1e-9 and dist(b.inCtrl, b.anchor) < 1e-9:
    moveAnchors(path, [r, e], d)
    return
  let t = if t0 == t0 and abs(t0) != Inf: clamp(t0, 0.05, 0.95) else: 0.5
  let u = 1 - t
  let s = 1 / (3 * t * u * (u * u + t * t))
  let (w1, w2) = (u * s, t * s)
  let ka = knotPtr(path, r)
  ka.outCtrl = addD(ka.outCtrl, [d[0] * w1, d[1] * w1])
  keepSmooth(ka[], hOut)
  let kb = knotPtr(path, e)
  kb.inCtrl = addD(kb.inCtrl, [d[0] * w2, d[1] * w2])
  keepSmooth(kb[], hIn)

proc lerp(a, b: Point; t: float64): Point = point(a.x + (b.x - a.x) * t, a.y + (b.y - a.y) * t)

proc addAnchor*(path: var Path; r: KnotRef; t: float64): KnotRef =
  if not (t == t and t > 0 and t < 1): raise newException(ValueError, "`t` must be strictly between 0 and 1")
  let e = segmentEnd(path, r)
  let a = knotPtr(path, r)[]
  let b = knotPtr(path, e)[]
  var nk: Knot
  if dist(a.outCtrl, a.anchor) < 1e-9 and dist(b.inCtrl, b.anchor) < 1e-9:
    let p = lerp(a.anchor, b.anchor, t)
    nk = cornerKnot(p.x, p.y)
  else:
    let (p01, p12, p23) = (lerp(a.anchor, a.outCtrl, t), lerp(a.outCtrl, b.inCtrl, t), lerp(b.inCtrl, b.anchor, t))
    let (p012, p123) = (lerp(p01, p12, t), lerp(p12, p23, t))
    knotPtr(path, r).outCtrl = p01
    knotPtr(path, e).inCtrl = p23
    nk = smoothKnot(lerp(p012, p123, t), p012, p123)
  let at = r[1] + 1
  path.subpaths[r[0]].knots.insert(nk, min(at, path.subpaths[r[0]].knots.len))
  [r[0], at]

proc deleteAnchor*(path: var Path; r: KnotRef): bool =
  ## Deletes a knot; returns true when its subpath was removed with it.
  discard knotPtr(path, r)
  var total = 0
  for sp in path.subpaths: total += sp.knots.len
  if total <= 1: raise newException(ValueError, "can't delete the path's only anchor point; delete the path instead")
  if path.subpaths[r[0]].knots.len <= 1:
    path.subpaths.delete(r[0])
    return true
  path.subpaths[r[0]].knots.delete(r[1])
  false

proc subpathAnchors*(path: Path; s: int): seq[KnotRef] =
  if s >= 0 and s < path.subpaths.len:
    for k in 0 ..< path.subpaths[s].knots.len: result.add [s, k]

proc anchorsIn*(path: Path; box: array[4, float64]): seq[KnotRef] =
  let (x0, x1, y0, y1) = (min(box[0], box[2]), max(box[0], box[2]), min(box[1], box[3]), max(box[1], box[3]))
  for s, sp in path.subpaths:
    for k, kn in sp.knots:
      if kn.anchor.x >= x0 and kn.anchor.x <= x1 and kn.anchor.y >= y0 and kn.anchor.y <= y1: result.add [s, k]

proc curvatureKnots*(anchors: openArray[array[2, float64]]; corners: openArray[bool]; closed: bool): seq[array[3, array[2, float64]]] =
  ## Curvature Pen: smooth handles from the neighbouring anchors (corners stay sharp).
  let n = anchors.len
  for a in anchors: result.add [a, a, a]
  proc d2(a, b: array[2, float64]): float64 = hypot(b[0] - a[0], b[1] - a[1])
  proc unit(a, b: array[2, float64]; ok: var bool): array[2, float64] =
    let l = d2(a, b)
    ok = l > 1e-9
    if ok: [(b[0] - a[0]) / l, (b[1] - a[1]) / l] else: [0.0, 0.0]
  template corner(i: int): bool = (i < corners.len and corners[i])
  for i in 0 ..< n:
    if n < 3 or corner(i) or not (closed or (i > 0 and i + 1 < n)): continue
    let p = anchors[i]
    let prev = anchors[(i + n - 1) mod n]
    let next = anchors[(i + 1) mod n]
    var ok: bool
    var d = unit(prev, next, ok)
    if not ok:
      d = unit(p, next, ok)
      if not ok: continue
    let (li, lo) = (d2(prev, p) / 3, d2(p, next) / 3)
    result[i] = [p, [p[0] - d[0] * li, p[1] - d[1] * li], [p[0] + d[0] * lo, p[1] + d[1] * lo]]
  if not closed and n >= 3:
    for (e, nb, takeIn) in [(0, 1, true), (n - 1, n - 2, false)]:
      if corner(e): continue
      let p = anchors[e]
      let aim = if takeIn: result[nb][1] else: result[nb][2]
      var ok: bool
      let d = unit(p, aim, ok)
      if not ok: continue
      let l = d2(p, anchors[nb]) / 3
      let h = [p[0] + d[0] * l, p[1] + d[1] * l]
      if takeIn: result[e][2] = h else: result[e][1] = h

proc evalCubic*(seg: array[4, Point]; t: float64): Point =
  let u = 1 - t
  let (a, b, c, d) = (u * u * u, 3 * u * u * t, 3 * u * t * t, t * t * t)
  point(a * seg[0].x + b * seg[1].x + c * seg[2].x + d * seg[3].x, a * seg[0].y + b * seg[1].y + c * seg[2].y + d * seg[3].y)

proc hit*(path: Path; p: Point; tol: float64; handles: openArray[(KnotRef, Handle)]): (bool, Hit) =
  ## The nearest handle (of `handles`), else anchor, else segment within `tol` of `p`.
  var best = Inf
  var found: Hit
  for (r, h) in handles:
    if r[0] >= 0 and r[0] < path.subpaths.len and r[1] >= 0 and r[1] < path.subpaths[r[0]].knots.len:
      let d = dist(path.subpaths[r[0]].knots[r[1]].handleOf(h), p)
      if d <= tol and d < best:
        best = d
        found = Hit(kind: hitHandle, knot: r, handle: h)
  if best < Inf: return (true, found)
  for s, sp in path.subpaths:
    for k, kn in sp.knots:
      let d = dist(kn.anchor, p)
      if d <= tol and d < best:
        best = d
        found = Hit(kind: hitAnchor, knot: [s, k])
  if best < Inf: return (true, found)
  const N = 64
  for s, sp in path.subpaths:
    for k, seg in sp.segments:
      var bd = Inf
      var bt = 0.5
      for i in 0 .. N:
        let t = float64(i) / N
        let d = dist(evalCubic(seg, t), p)
        if d < bd: (bd, bt) = (d, t)
      let step = 1.0 / N
      let center = bt
      for i in 0 .. 16:
        let t = clamp(center - step + 2 * step * float64(i) / 16, 0.0, 1.0)
        let d = dist(evalCubic(seg, t), p)
        if d < bd: (bd, bt) = (d, t)
      if bd <= tol and bd < best:
        best = bd
        found = Hit(kind: hitSegment, knot: [s, k], t: bt)
  (best < Inf, found)
