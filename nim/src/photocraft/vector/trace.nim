## Tracing: marching-squares contours of a coverage mask and least-squares cubic fitting
## (Schneider's algorithm), used by Make Work Path and the Freeform Pen.

import std/[math, tables, algorithm]
import ../doc

type P = (float64, float64)

proc sub(a, b: P): P {.inline.} = (a[0] - b[0], a[1] - b[1])
proc add(a, b: P): P {.inline.} = (a[0] + b[0], a[1] + b[1])
proc mul(a: P; k: float64): P {.inline.} = (a[0] * k, a[1] * k)
proc dot(a, b: P): float64 {.inline.} = a[0] * b[0] + a[1] * b[1]
proc vlen(a: P): float64 {.inline.} = hypot(a[0], a[1])
proc norm(a: P): P {.inline.} =
  let l = vlen(a)
  if l > 1e-12: (a[0] / l, a[1] / l) else: (0.0, 0.0)

type EdgeKey = (int, int, uint8)

proc contours*(values: openArray[float32]; rc: Rect; outside: float32): seq[seq[P]] =
  ## Closed iso-0.5 contours of `values` (row-major over `rc`), outside reads `outside`.
  let vals = @values
  let w = rc.width
  let h = rc.height
  template get(x, y: int): float32 =
    (if x < 0 or y < 0 or x >= w or y >= h: outside else: vals[y * w + x])
  const lvl = 0.5'f32
  var next = initTable[EdgeKey, EdgeKey]()
  for cy in -1 ..< h:
    for cx in -1 ..< w:
      let idx = int(get(cx, cy) >= lvl) or (int(get(cx + 1, cy) >= lvl) shl 1) or
                (int(get(cx + 1, cy + 1) >= lvl) shl 2) or (int(get(cx, cy + 1) >= lvl) shl 3)
      if idx == 0 or idx == 15: continue
      let top = (cx, cy, 0'u8)
      let bottom = (cx, cy + 1, 0'u8)
      let left = (cx, cy, 1'u8)
      let right = (cx + 1, cy, 1'u8)
      template seg(a, b: EdgeKey) = next[a] = b
      let centre = (get(cx, cy) + get(cx + 1, cy) + get(cx + 1, cy + 1) + get(cx, cy + 1)) / 4
      case idx
      of 1: seg(left, top)
      of 2: seg(top, right)
      of 3: seg(left, right)
      of 4: seg(right, bottom)
      of 5:
        if centre >= lvl:
          seg(left, bottom); seg(right, top)
        else:
          seg(left, top); seg(right, bottom)
      of 6: seg(top, bottom)
      of 7: seg(left, bottom)
      of 8: seg(bottom, left)
      of 9: seg(bottom, top)
      of 10:
        if centre >= lvl:
          seg(top, left); seg(bottom, right)
        else:
          seg(top, right); seg(bottom, left)
      of 11: seg(bottom, right)
      of 12: seg(right, left)
      of 13: seg(right, top)
      of 14: seg(top, left)
      else: discard
  proc pointOn(a, b: (int, int)): P =
    let (va, vb) = (get(a[0], a[1]), get(b[0], b[1]))
    let t = if abs(vb - va) > 1e-9: clamp(float64((lvl - va) / (vb - va)), 0.0, 1.0) else: 0.5
    let (ax, ay) = (float64(a[0]) + 0.5, float64(a[1]) + 0.5)
    let (bx, by) = (float64(b[0]) + 0.5, float64(b[1]) + 0.5)
    (float64(rc.x0) + ax + (bx - ax) * t, float64(rc.y0) + ay + (by - ay) * t)
  proc edgePoint(e: EdgeKey): P =
    if e[2] == 0: pointOn((e[0], e[1]), (e[0] + 1, e[1])) else: pointOn((e[0], e[1]), (e[0], e[1] + 1))
  var keys: seq[EdgeKey]
  for k in next.keys: keys.add k
  keys.sort(proc (a, b: EdgeKey): int = cmp((a[1], a[0], a[2]), (b[1], b[0], b[2])))
  for start in keys:
    if start notin next: continue
    var lp: seq[P]
    var cur = start
    while cur in next:
      let n = next[cur]
      next.del cur
      lp.add edgePoint(cur)
      cur = n
      if cur == start: break
    var dd: seq[P]
    for p in lp:
      if dd.len == 0 or abs(dd[^1][0] - p[0]) >= 1e-9 or abs(dd[^1][1] - p[1]) >= 1e-9: dd.add p
    if dd.len >= 3: result.add dd

proc walk(lp: openArray[P]; i: int; window: float64; fwd, closed: bool): P =
  let n = lp.len
  var acc = 0.0
  var cur = i
  for _ in 0 ..< n:
    if not closed and (if fwd: cur + 1 >= n else: cur == 0): break
    let nx = if fwd: (cur + 1) mod n else: (cur + n - 1) mod n
    let d = vlen(sub(lp[nx], lp[cur]))
    if acc + d >= window:
      return add(lp[cur], mul(sub(lp[nx], lp[cur]), (window - acc) / max(d, 1e-12)))
    acc += d
    cur = nx
  lp[cur]

proc corners(lp: openArray[P]; window: float64; closed: bool): seq[int] =
  let n = lp.len
  let cosThresh = cos(degToRad(60.0))
  var turn = newSeq[float64](n)
  for i in 0 ..< n:
    if not closed and (i == 0 or i + 1 == n): turn[i] = 1
    else:
      let a = norm(sub(lp[i], walk(lp, i, window, false, closed)))
      let b = norm(sub(walk(lp, i, window, true, closed), lp[i]))
      turn[i] = dot(a, b)
  for i in 0 ..< n:
    if turn[i] >= cosThresh: continue
    var isMin = true
    var acc = 0.0
    var j = i
    while acc < window:
      if not closed and j + 1 >= n: break
      let nx = (j + 1) mod n
      acc += vlen(sub(lp[nx], lp[j]))
      j = nx
      if j == i: break
      if turn[j] < turn[i]:
        isMin = false
        break
    acc = 0
    j = i
    while isMin and acc < window:
      if not closed and j == 0: break
      let pv = (j + n - 1) mod n
      acc += vlen(sub(lp[j], lp[pv]))
      j = pv
      if j == i: break
      if turn[j] <= turn[i]: isMin = false
    if isMin: result.add i

proc bezier(b: array[4, P]; t: float64): P =
  let mt = 1 - t
  let (a, bb, c, d) = (mt * mt * mt, 3 * mt * mt * t, 3 * mt * t * t, t * t * t)
  (a * b[0][0] + bb * b[1][0] + c * b[2][0] + d * b[3][0], a * b[0][1] + bb * b[1][1] + c * b[2][1] + d * b[3][1])
proc bezierD1(b: array[4, P]; t: float64): P =
  let mt = 1 - t
  add(add(mul(mul(sub(b[1], b[0]), 3), mt * mt), mul(mul(sub(b[2], b[1]), 3), 2 * mt * t)), mul(mul(sub(b[3], b[2]), 3), t * t))
proc bezierD2(b: array[4, P]; t: float64): P =
  let r0 = mul(add(sub(b[2], mul(b[1], 2)), b[0]), 6)
  let r1 = mul(add(sub(b[3], mul(b[2], 2)), b[1]), 6)
  add(mul(r0, 1 - t), mul(r1, t))

proc chordParams(pts: openArray[P]): seq[float64] =
  result = newSeq[float64](pts.len)
  for i in 1 ..< pts.len: result[i] = result[i - 1] + vlen(sub(pts[i], pts[i - 1]))
  let total = result[^1]
  if total > 0:
    for v in result.mitems: v /= total

proc generate(pts: openArray[P]; u: seq[float64]; t1, t2: P): array[4, P] =
  let (p0, p3) = (pts[0], pts[^1])
  var c00, c01, c11, x0, x1: float64
  for i, t in u:
    let mt = 1 - t
    let (b1, b2) = (3 * mt * mt * t, 3 * mt * t * t)
    let (a1, a2) = (mul(t1, b1), mul(t2, b2))
    c00 += dot(a1, a1)
    c01 += dot(a1, a2)
    c11 += dot(a2, a2)
    let tmp = sub(pts[i], add(mul(p0, mt * mt * mt + b1), mul(p3, b2 + t * t * t)))
    x0 += dot(a1, tmp)
    x1 += dot(a2, tmp)
  let det = c00 * c11 - c01 * c01
  var (al, ar) = if abs(det) > 1e-12: ((x0 * c11 - x1 * c01) / det, (c00 * x1 - c01 * x0) / det) else: (0.0, 0.0)
  let seg = vlen(sub(p3, p0))
  let eps = 1e-6 * seg
  if al < eps or ar < eps:
    al = seg / 3
    ar = seg / 3
  [p0, add(p0, mul(t1, al)), add(p3, mul(t2, ar)), p3]

proc maxError(pts: openArray[P]; b: array[4, P]; u: seq[float64]): (float64, int) =
  result = (0.0, pts.len div 2)
  for i in 1 ..< pts.len - 1:
    let d = sub(bezier(b, u[i]), pts[i])
    let e = dot(d, d)
    if e >= result[0]: result = (e, i)

proc reparam(pts: openArray[P]; u: seq[float64]; b: array[4, P]): seq[float64] =
  for i, p in pts:
    let t = u[i]
    let q = bezier(b, t)
    let (d1, d2) = (bezierD1(b, t), bezierD2(b, t))
    let num = dot(sub(q, p), d1)
    let den = dot(d1, d1) + dot(sub(q, p), d2)
    result.add(if abs(den) < 1e-12: t else: clamp(t - num / den, 0.0, 1.0))

proc fitCubic(pts: seq[P]; t1, t2: P; err2: float64; o: var seq[array[4, P]]; depth: int) =
  let n = pts.len
  if n == 2 or depth > 40:
    let d = vlen(sub(pts[n - 1], pts[0])) / 3
    o.add [pts[0], add(pts[0], mul(t1, d)), add(pts[n - 1], mul(t2, d)), pts[n - 1]]
    return
  var u = chordParams(pts)
  var b = generate(pts, u, t1, t2)
  var (e, split) = maxError(pts, b, u)
  if e < err2:
    o.add b
    return
  if e < err2 * 16:
    for _ in 0 ..< 6:
      u = reparam(pts, u, b)
      b = generate(pts, u, t1, t2)
      (e, split) = maxError(pts, b, u)
      if e < err2:
        o.add b
        return
  split = clamp(split, 1, n - 2)
  var tc = norm(sub(pts[split - 1], pts[split + 1]))
  if vlen(tc) < 0.5: tc = norm(sub(pts[split - 1], pts[split]))
  fitCubic(pts[0 .. split], t1, tc, err2, o, depth + 1)
  fitCubic(pts[split .. ^1], mul(tc, -1), t2, err2, o, depth + 1)

proc pt(p: P): Point = point(p[0], p[1])

proc fitClosed(lp: seq[P]; tol, window: float64): Subpath =
  let n = lp.len
  let cs = corners(lp, window, true)
  var segs: seq[array[4, P]]
  var cornerAtStart: seq[bool]
  if cs.len == 0:
    var pts = lp
    pts.add lp[0]
    let t = norm(sub(lp[1 mod n], lp[n - 1]))
    fitCubic(pts, t, mul(t, -1), tol * tol, segs, 0)
    for _ in 0 ..< segs.len: cornerAtStart.add false
  else:
    for k, c in cs:
      let e = cs[(k + 1) mod cs.len]
      var pts = @[lp[c]]
      var j = c
      while true:
        j = (j + 1) mod n
        pts.add lp[j]
        if j == e: break
      let m = pts.len
      let t1 = norm(sub(pts[min(1, m - 1)], pts[0]))
      let t2 = norm(sub(pts[max(m - 2, 0)], pts[m - 1]))
      let before = segs.len
      fitCubic(pts, t1, t2, tol * tol, segs, 0)
      for q in before ..< segs.len: cornerAtStart.add(q == before)
  var knots: seq[Knot]
  for i, s in segs:
    knots.add Knot(anchor: pt(s[0]), inCtrl: pt(s[0]), outCtrl: pt(s[1]), smooth: not cornerAtStart[i])
  let m = segs.len
  for i in 0 ..< m: knots[(i + 1) mod m].inCtrl = pt(segs[i][2])
  Subpath(closed: true, knots: knots, op: poCombine)

proc segmentDistance(q, a, b: P): float64 =
  let ab = sub(b, a)
  let l2 = dot(ab, ab)
  let t = if l2 > 1e-18: clamp(dot(sub(q, a), ab) / l2, 0.0, 1.0) else: 0.0
  vlen(sub(q, add(a, mul(ab, t))))

proc fitOpen(lp: seq[P]; tol, window: float64): Subpath =
  let n = lp.len
  var cuts = @[0]
  cuts.add corners(lp, window, false)
  cuts.add n - 1
  var dd: seq[int]
  for c in cuts:
    if dd.len == 0 or dd[^1] != c: dd.add c
  var segs: seq[array[4, P]]
  var cornerAtStart: seq[bool]
  for wi in 0 ..< dd.len - 1:
    let pts = lp[dd[wi] .. dd[wi + 1]]
    let m = pts.len
    if m < 2: continue
    let (a, b) = (pts[0], pts[m - 1])
    let before = segs.len
    var straight = true
    for q in pts:
      if segmentDistance(q, a, b) > tol: straight = false
    if straight:
      let third = mul(sub(b, a), 1 / 3)
      segs.add [a, add(a, third), sub(b, third), b]
    else:
      let reach = max(min(2 * window, vlen(sub(b, a)) / 3), window)
      let t1 = norm(sub(walk(pts, 0, reach, true, false), a))
      let t2 = norm(sub(walk(pts, m - 1, reach, false, false), b))
      fitCubic(pts, t1, t2, tol * tol, segs, 0)
    for k in before ..< segs.len: cornerAtStart.add(k == before)
  var knots: seq[Knot]
  for i, s in segs:
    let inCtrl = if i == 0: s[0] else: segs[i - 1][2]
    knots.add Knot(anchor: pt(s[0]), inCtrl: pt(inCtrl), outCtrl: pt(s[1]), smooth: i > 0 and not cornerAtStart[i])
  if segs.len > 0:
    let s = segs[^1]
    knots.add Knot(anchor: pt(s[3]), inCtrl: pt(s[2]), outCtrl: pt(s[3]))
  Subpath(closed: false, knots: knots, op: poCombine)

proc traceMask*(values: openArray[float32]; rc: Rect; outside: float32; tolerance: float64): Path =
  ## A path outlining the >= 0.5 areas of a mask (Make Work Path); inner loops exclude.
  let loops = contours(values, rc, outside)
  var subs: seq[Subpath]
  for i, lp in loops:
    if lp.len < 3: continue
    var s = fitClosed(lp, max(tolerance * 0.5, 0.05), 1.5)
    s.op = if i == 0: poCombine else: poExclude
    subs.add s
  newPath(subs)

const MaxTracePoints* = 4096

proc fitTrace*(points: openArray[array[2, float64]]; tolerance: float64; closed: bool): (bool, Subpath) =
  ## Fits a freehand trace (Freeform Pen) with Bézier curves; false when too few points.
  let tol = if tolerance == tolerance and abs(tolerance) != Inf: clamp(tolerance, 0.1, 100.0) else: 2.0
  let window = max(2 * tol, 1.5)
  let minStep = min(tol * 0.25, 0.5)
  let stride = max((points.len + MaxTracePoints - 1) div MaxTracePoints, 1)
  let last = max(points.len - 1, 0)
  var lp: seq[P]
  for i, p in points:
    if (i mod stride != 0 and i != last) or p[0] != p[0] or p[1] != p[1] or abs(p[0]) >= 1e9 or abs(p[1]) >= 1e9: continue
    let q = (p[0], p[1])
    if lp.len > 0 and vlen(sub(q, lp[^1])) < minStep:
      if i == last and lp.len > 1: lp[^1] = q
    else: lp.add q
  if closed:
    while lp.len > 3 and vlen(sub(lp[^1], lp[0])) < minStep: lp.setLen(lp.len - 1)
    if lp.len >= 3: return (true, fitClosed(lp, tol, window))
    return (false, Subpath())
  if lp.len >= 2: (true, fitOpen(lp, tol, window)) else: (false, Subpath())
