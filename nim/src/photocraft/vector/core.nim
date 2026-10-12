## Vector rasterization: Bézier flattening, an exact-area scanline rasterizer with per-component
## winding and Photoshop path operations, polygon stroking (joins, caps, dashes), paints
## (solid/gradient) and compiled shapes and vector masks.

import std/[math, algorithm, options]
import ../doc

const DefaultTolerance* = 0.01  ## curve-to-chord distance in pixels

type
  P = (float64, float64)
  Polyline* = object
    pts*: seq[P]
    knot*: seq[bool]  ## true for knots of the source path (joins apply there)
    closed*: bool

# ---------------------------------------------------------------- flatten

proc push(pl: var Polyline; p: P; knot: bool) =
  if pl.pts.len > 0:
    let last = pl.pts[^1]
    if abs(last[0] - p[0]) < 1e-12 and abs(last[1] - p[1]) < 1e-12:
      if knot and pl.knot.len > 0: pl.knot[^1] = true
      return
  pl.pts.add p
  pl.knot.add knot

proc isLine(seg: array[4, Point]): bool =
  let (p0, c1, c2, p1) = (seg[0], seg[1], seg[2], seg[3])
  let (dx, dy) = (p1.x - p0.x, p1.y - p0.y)
  let len2 = dx * dx + dy * dy
  proc onChord(c: Point): bool =
    let vx = c.x - p0.x
    let vy = c.y - p0.y
    if len2 < 1e-24: return abs(vx) < 1e-9 and abs(vy) < 1e-9
    let t = (vx * dx + vy * dy) / len2
    let cr = abs(vx * dy - vy * dx) / sqrt(len2)
    cr < 1e-9 and t >= -1e-9 and t <= 1 + 1e-9
  onChord(c1) and onChord(c2)

proc pieces(seg: array[4, Point]; tol0: float64): int =
  let (p0, c1, c2, p1) = (seg[0], seg[1], seg[2], seg[3])
  let d1 = hypot(p0.x - 2 * c1.x + c2.x, p0.y - 2 * c1.y + c2.y)
  let d2 = hypot(c1.x - 2 * c2.x + p1.x, c1.y - 2 * c2.y + p1.y)
  let m = max(d1, d2)
  let size = hypot(c1.x - p0.x, c1.y - p0.y) + hypot(c2.x - c1.x, c2.y - c1.y) + hypot(p1.x - c2.x, p1.y - c2.y)
  let tol = max(min(tol0, size * 1e-3), 1e-4)
  let n = ceil(sqrt(0.75 * m / tol))
  if n == n and n != Inf: clamp(int(n), 1, 1 shl 16) else: 1

proc eval(seg: array[4, Point]; t: float64): P =
  let mt = 1 - t
  let (a, b, c, d) = (mt * mt * mt, 3 * mt * mt * t, 3 * mt * t * t, t * t * t)
  (a * seg[0].x + b * seg[1].x + c * seg[2].x + d * seg[3].x, a * seg[0].y + b * seg[1].y + c * seg[2].y + d * seg[3].y)

proc flattenSubpath*(s: Subpath; tol: float64): Polyline =
  ## Closed subpaths do not repeat the first point at the end.
  result.closed = s.closed
  if s.knots.len > 0: result.push((s.knots[0].anchor.x, s.knots[0].anchor.y), true)
  for seg in s.segments:
    if isLine(seg):
      result.push((seg[3].x, seg[3].y), true)
      continue
    let n = pieces(seg, tol)
    for j in 1 .. n: result.push(eval(seg, float64(j) / float64(n)), j == n)
  if s.closed and result.pts.len > 1:
    let (a, b) = (result.pts[0], result.pts[^1])
    if abs(a[0] - b[0]) < 1e-12 and abs(a[1] - b[1]) < 1e-12:
      result.pts.setLen(result.pts.len - 1)
      result.knot.setLen(result.knot.len - 1)
      result.knot[0] = true

proc flattenPath*(p: Path; tol: float64): seq[Polyline] =
  for s in p.subpaths: result.add flattenSubpath(s, tol)

proc validateShape*(path: Path; stroke: Option[ShapeStroke]): Option[string] =
  ## Validates before interactive rendering: finite coordinates and a bounded workload.
  var knots = 0
  for s in path.subpaths: knots += s.knots.len
  if knots > 4096 or path.subpaths.len > 4096: return some("shape path exceeds 4096 knots/subpaths")
  var points = 0
  var length = 0.0
  for sub in path.subpaths:
    for k in sub.knots:
      for p in [k.anchor, k.inCtrl, k.outCtrl]:
        if p.x != p.x or p.y != p.y or abs(p.x) > 1_000_000 or abs(p.y) > 1_000_000:
          return some("shape coordinates must be finite and within ±1000000 px")
    inc points
    for seg in sub.segments:
      points += (if isLine(seg): 1 else: pieces(seg, DefaultTolerance))
      for i in 0..2: length += hypot(seg[i + 1].x - seg[i].x, seg[i + 1].y - seg[i].y)
  if points > 100_000: return some("shape tessellation exceeds 100000 points")
  if stroke.isSome:
    let s = stroke.get
    let v = s.validate
    if v.isSome: return v
    if s.width > 0 and s.dashes.len > 0:
      var period = 0.0
      for d in s.dashes: period += float64(d)
      period *= float64(s.width)
      let transitions = (length / period + 2) * float64(s.dashes.len) + float64(points)
      if transitions != transitions or transitions > 100_000: return some("dash pattern is too dense for this path (100000 step budget)")
  none(string)

# ---------------------------------------------------------------- rasterizer

const Residue = 1e-6'f32

type
  Edge = object
    x0, y0, y1: float64
    dir: int32
    comp: int32
    dxdy: float64
  Component* = object
    op*: PathOp
    rule*: FillRule
  Rasterizer* = object
    edges: seq[Edge]
    comps: seq[Component]
    inverted: bool
    hasBounds: bool
    bounds: (float64, float64, float64, float64)
    unionOnly: bool
  BandEdge = object
    xa, xb, xm: float64
    idx: int
  RowState = object
    active: seq[int]
    cuts: seq[float64]
    band: seq[BandEdge]
    wind: seq[int32]
    acc: seq[float32]

proc newRasterizer*(inverted: bool): Rasterizer = Rasterizer(inverted: inverted, unionOnly: true)

proc isInverted*(r: Rasterizer): bool = r.inverted

proc addComponent*(r: var Rasterizer; polys: openArray[seq[P]]; op: PathOp; rule: FillRule) =
  ## Adds a component of closed polygons (open input closes implicitly).
  let comp = int32(r.comps.len)
  r.comps.add Component(op: op, rule: rule)
  if op != poCombine and comp != 0: r.unionOnly = false
  for poly in polys:
    let n = poly.len
    if n < 2: continue
    for i in 0 ..< n:
      let a = poly[i]
      let b = poly[(i + 1) mod n]
      if a[0] != a[0] or a[1] != a[1] or b[0] != b[0] or b[1] != b[1] or
         abs(a[0]) == Inf or abs(a[1]) == Inf or abs(b[0]) == Inf or abs(b[1]) == Inf: continue
      if not r.hasBounds:
        r.bounds = (min(a[0], b[0]), min(a[1], b[1]), max(a[0], b[0]), max(a[1], b[1]))
        r.hasBounds = true
      else:
        r.bounds = (min(r.bounds[0], min(a[0], b[0])), min(r.bounds[1], min(a[1], b[1])),
                    max(r.bounds[2], max(a[0], b[0])), max(r.bounds[3], max(a[1], b[1])))
      if a[1] == b[1]: continue
      let (p, q, dir) = if a[1] < b[1]: (a, b, 1'i32) else: (b, a, -1'i32)
      r.edges.add Edge(x0: p[0], y0: p[1], y1: q[1], dir: dir, comp: comp, dxdy: (q[0] - p[0]) / (q[1] - p[1]))
  r.edges.sort(proc (a, b: Edge): int = cmp(a.y0, b.y0))

proc pixelBounds*(r: Rasterizer): Option[Rect] =
  ## Pixel bounds of what may be covered (`none` = nothing, or everything when inverted).
  if r.inverted or not r.hasBounds: return none(Rect)
  template ci(v: float64): int = int(clamp(v, -2147483648.0, 2147483647.0))
  let rr = rect(ci(floor(r.bounds[0])), ci(floor(r.bounds[1])), satAdd(ci(ceil(r.bounds[2])), 1), satAdd(ci(ceil(r.bounds[3])), 1))
  if rr.isEmpty: none(Rect) else: some(rr)

proc compInside(r: Rasterizer; comp: int; w: int32): bool =
  if r.comps[comp].rule == frNonZero: w != 0 else: (w and 1) != 0

proc inside(r: Rasterizer; wind: seq[int32]; unionCount: int32): bool =
  if r.unionOnly: return (unionCount > 0) xor r.inverted
  var acc = false
  for i, c in r.comps:
    let v = if c.rule == frNonZero: wind[i] != 0 else: (wind[i] and 1) != 0
    let op = if i == 0: poCombine else: c.op
    acc = case op
      of poCombine, poJoin: acc or v
      of poSubtract: acc and not v
      of poIntersect: acc and v
      of poExclude: acc xor v
  acc xor r.inverted

proc draw(acc: var seq[float32]; x0: int; xa, ya, xb, yb, w: float64) =
  ## Adds `w` × the area right of the segment (inside one pixel row) to the cell accumulator.
  let width = float64(acc.len - 1)
  let left = float64(x0)
  var ua = xa - left
  var ub = xb - left
  let dy = yb - ya
  if dy <= 0: return
  if ua > ub: swap(ua, ub)
  let dx = ub - ua
  template hOf(u0, u1: float64): float64 = (if dx > 1e-12: dy * (u1 - u0) / dx else: dy)
  if ua < 0:
    let cut = min(ub, 0.0)
    acc[0] += float32(w * hOf(ua, cut))
    if ub <= 0: return
    ua = 0
  if ua >= width: return
  let e = min(ub, width)
  if dx <= 1e-12:
    let c = floor(ua)
    let f = ua - c
    let ci = int(c)
    acc[ci] += float32(w * dy * (1 - f))
    acc[ci + 1] += float32(w * dy * f)
    return
  var u = ua
  while u < e:
    let c = floor(u)
    let v = min(c + 1, e)
    let h = hOf(u, v)
    let m = 0.5 * (u + v) - c
    let ci = int(c)
    acc[ci] += float32(w * h * (1 - m))
    acc[ci + 1] += float32(w * h * m)
    if v <= u: break
    u = v

proc band(r: Rasterizer; a, b: float64; x0: int; st: var RowState; depth: int) =
  if b - a <= 1e-12: return
  st.band.setLen(0)
  let mid = 0.5 * (a + b)
  for i in st.active:
    let e = r.edges[i]
    if e.y0 <= mid and e.y1 >= mid:
      let xa = e.x0 + (a - e.y0) * e.dxdy
      let xb = e.x0 + (b - e.y0) * e.dxdy
      st.band.add BandEdge(xa: xa, xb: xb, xm: 0.5 * (xa + xb), idx: i)
  if st.band.len == 0:
    if r.inverted: st.acc[0] += float32(b - a)
    return
  st.band.sort(proc (p, q: BandEdge): int = cmp(p.xm, q.xm))
  if depth < 24:
    for k in 1 ..< st.band.len:
      let (p, q) = (st.band[k - 1], st.band[k])
      let top = p.xa - q.xa
      let bot = p.xb - q.xb
      if top > 1e-9 or bot > 1e-9:
        let t = if abs(top - bot) > 1e-15: top / (top - bot) else: 0.5
        var y = a + (b - a) * clamp(t, 0.0, 1.0)
        if y <= a + 1e-9 or y >= b - 1e-9: y = mid
        r.band(a, y, x0, st, depth + 1)
        r.band(y, b, x0, st, depth + 1)
        return
  for w in st.wind.mitems: w = 0
  var unionCount = 0'i32
  var inside = r.inverted
  if inside: st.acc[0] += float32(b - a)
  for k in 0 ..< st.band.len:
    let be = st.band[k]
    let e = r.edges[be.idx]
    let c = int(e.comp)
    if r.unionOnly:
      let was = r.compInside(c, st.wind[c])
      st.wind[c] += e.dir
      let now = r.compInside(c, st.wind[c])
      unionCount += int32(now) - int32(was)
    else:
      st.wind[c] += e.dir
    let now = r.inside(st.wind, unionCount)
    if now != inside:
      draw(st.acc, x0, be.xa, a, be.xb, b, if now: 1.0 else: -1.0)
      inside = now

proc renderInto*(r: Rasterizer; rc: Rect; o: var openArray[float32]) =
  ## Coverage of `rc` into `o` (`width × height` values), row-major.
  let w = rc.width
  if w <= 0: return
  var st = RowState(wind: newSeq[int32](max(r.comps.len, 1)), acc: newSeq[float32](w + 1))
  let top = float64(rc.y0)
  var next = 0
  while next < r.edges.len and r.edges[next].y0 < top: inc next
  for i in 0 ..< next:
    if r.edges[i].y1 > top: st.active.add i
  for row in 0 ..< rc.height:
    let y = rc.y0 + row
    let (ya, yb) = (float64(y), float64(y) + 1)
    var k = 0
    for i in st.active:
      if r.edges[i].y1 > ya:
        st.active[k] = i
        inc k
    st.active.setLen(k)
    while next < r.edges.len and r.edges[next].y0 < yb:
      if r.edges[next].y1 > ya: st.active.add next
      inc next
    for v in st.acc.mitems: v = 0
    if st.active.len == 0:
      let fill = if r.inverted: 1'f32 else: 0'f32
      for x in 0 ..< w: o[row * w + x] = fill
      continue
    st.cuts.setLen(0)
    st.cuts.add ya
    st.cuts.add yb
    for i in st.active:
      let e = r.edges[i]
      if e.y0 > ya and e.y0 < yb: st.cuts.add e.y0
      if e.y1 > ya and e.y1 < yb: st.cuts.add e.y1
    st.cuts.sort()
    var cuts: seq[float64]
    for c in st.cuts:
      if cuts.len == 0 or cuts[^1] != c: cuts.add c
    for ci in 0 ..< cuts.len - 1: r.band(cuts[ci], cuts[ci + 1], rc.x0, st, 0)
    var sum = 0'f32
    for x in 0 ..< w:
      sum += st.acc[x]
      o[row * w + x] = if sum >= Residue: min(sum, 1'f32) else: 0'f32

proc render*(r: Rasterizer; rc: Rect): seq[float32] =
  result = newSeq[float32](max(rc.width * rc.height, 0))
  if result.len > 0: r.renderInto(rc, result)

# ---------------------------------------------------------------- stroke

type StrokeStyle* = object
  width*: float64
  cap*: LineCap
  join*: LineJoin
  miterLimit*: float64
  dashes*: seq[float64]
  dashOffset*: float64

proc defaultStrokeStyle*(): StrokeStyle = StrokeStyle(width: 1, cap: lcapButt, join: ljMiter, miterLimit: 4)

proc vsub(a, b: P): P {.inline.} = (a[0] - b[0], a[1] - b[1])
proc vadd(a, b: P): P {.inline.} = (a[0] + b[0], a[1] + b[1])
proc vmul(a: P; k: float64): P {.inline.} = (a[0] * k, a[1] * k)
proc vlen(a: P): float64 {.inline.} = hypot(a[0], a[1])
proc vnorm(a: P): P {.inline.} =
  let l = vlen(a)
  if l > 0: (a[0] / l, a[1] / l) else: (0.0, 0.0)
proc perp(a: P): P {.inline.} = (a[1], -a[0])
proc cross(a, b: P): float64 {.inline.} = a[0] * b[1] - a[1] * b[0]
proc dot(a, b: P): float64 {.inline.} = a[0] * b[0] + a[1] * b[1]

proc signedArea(p: seq[P]): float64 =
  for i in 0 ..< p.len: result += cross(p[i], p[(i + 1) mod p.len])
  result *= 0.5

proc emit(o: var seq[seq[P]]; poly: seq[P]) =
  let a = signedArea(poly)
  if abs(a) < 1e-12: return
  if a < 0: o.add reversed(poly) else: o.add poly

proc arcStep(r, tol: float64): float64 =
  if r <= tol: return PI / 2
  clamp(2 * arccos(clamp(1 - tol / r, -1.0, 1.0)), 0.01, PI / 2)

proc areaRadius(r, step: float64): float64 = (if step < 1e-9: r else: r * sqrt(step / sin(step)))

proc circle(c: P; r, tol: float64): seq[P] =
  let n = max(int(ceil(TAU / arcStep(r, tol))), 8)
  let rr = areaRadius(r, TAU / float64(n))
  for i in 0 ..< n:
    let a = TAU * float64(i) / float64(n)
    result.add (c[0] + rr * cos(a), c[1] + rr * sin(a))

proc joinPiece(v, d1, d2: P; hw: float64; join: LineJoin; miterLimit, tol: float64; o: var seq[seq[P]]) =
  let c = cross(d1, d2)
  let d = dot(d1, d2)
  if abs(c) < 1e-12 and d > 0: return
  let s = if c > 0: 1.0 else: -1.0
  let n1 = vmul(perp(d1), hw * s)
  let n2 = vmul(perp(d2), hw * s)
  let (a, b) = (vadd(v, n1), vadd(v, n2))
  case join
  of ljBevel: o.emit(@[v, a, b])
  of ljMiter:
    let cosHalf = sqrt(max((1 + d) * 0.5, 0.0))
    if cosHalf < 1e-9 or 1 / cosHalf > miterLimit: o.emit(@[v, a, b])
    else:
      let m = vadd(v, vmul(vnorm(vadd(n1, n2)), hw / cosHalf))
      o.emit(@[v, a, m, b])
  of ljRound:
    let a0 = arctan2(n1[1], n1[0])
    var delta = arctan2(n2[1], n2[0]) - a0
    while delta > PI: delta -= TAU
    while delta < -PI: delta += TAU
    if abs(c) < 1e-12: delta = PI * (if cross(n1, d1) > 0: 1.0 else: -1.0)
    let a1 = a0 + delta
    let steps = max(int(ceil(abs(delta) / arcStep(hw, tol))), 1)
    let rr = areaRadius(hw, abs(delta) / float64(steps))
    var poly = @[v, a]
    for k in 1 ..< steps:
      let t = a0 + (a1 - a0) * float64(k) / float64(steps)
      poly.add (v[0] + rr * cos(t), v[1] + rr * sin(t))
    poly.add b
    o.emit(poly)

proc capPiece(p, dir: P; hw: float64; cap: LineCap; tol: float64; o: var seq[seq[P]]) =
  case cap
  of lcapButt: discard
  of lcapRound: o.emit(circle(p, hw, tol))
  of lcapSquare:
    let n = vmul(perp(dir), hw)
    let e = vmul(dir, hw)
    o.emit(@[vadd(p, n), vadd(vadd(p, n), e), vadd(vsub(p, n), e), vsub(p, n)])

proc strokeOne(pl: Polyline; style: StrokeStyle; tol: float64; o: var seq[seq[P]]) =
  let hw = style.width * 0.5
  var pts: seq[P]
  var knot: seq[bool]
  for i, p in pl.pts:
    if pts.len > 0 and vlen(vsub(p, pts[^1])) < 1e-9: continue
    pts.add p
    knot.add(if i < pl.knot.len: pl.knot[i] else: true)
  var closed = pl.closed
  if closed and pts.len > 1 and vlen(vsub(pts[0], pts[^1])) < 1e-9:
    pts.setLen(pts.len - 1)
    knot.setLen(knot.len - 1)
  if pts.len < 3: closed = false
  if pts.len == 0: return
  if pts.len == 1:
    let c = pts[0]
    case style.cap
    of lcapRound: o.emit(circle(c, hw, tol))
    of lcapSquare: o.emit(@[(c[0] - hw, c[1] - hw), (c[0] + hw, c[1] - hw), (c[0] + hw, c[1] + hw), (c[0] - hw, c[1] + hw)])
    of lcapButt: discard
    return
  let n = pts.len
  let segs = if closed: n else: n - 1
  template dir(i: int): P = vnorm(vsub(pts[(i + 1) mod n], pts[i]))
  template segLen(i: int): float64 = vlen(vsub(pts[(i + 1) mod n], pts[i]))
  let cosSmooth = cos(degToRad(25.0))
  proc smooth(j: int): bool =
    if j == 0 or (not closed and j == n - 1): return false
    let (i0, i1) = ((j + n - 1) mod n, j)
    let d = dot(dir(i0), dir(i1))
    if knot[j]: return d > 1 - 1e-12
    if d < cosSmooth: return false
    let tanHalf = sqrt(max((1 - d) / (1 + d), 0.0))
    hw * tanHalf < 0.5 * min(segLen(i0), segLen(i1))
  var left, right: seq[P]
  for i in 0 ..< segs:
    let a = pts[i]
    let b = pts[(i + 1) mod n]
    let nrm = vmul(perp(dir(i)), hw)
    if left.len == 0:
      left.add vadd(a, nrm)
      right.add vsub(a, nrm)
    let j = (i + 1) mod n
    if i + 1 < segs + int(not closed) and smooth(j):
      let n2 = vmul(perp(dir(j)), hw)
      let bis = vnorm(vadd(nrm, n2))
      let cosHalf = sqrt(max((1 + dot(dir(i), dir(j))) * 0.5, 1e-9))
      let off = vmul(bis, hw / cosHalf)
      left.add vadd(b, off)
      right.add vsub(b, off)
      continue
    left.add vadd(b, nrm)
    right.add vsub(b, nrm)
    var poly = left
    for k in countdown(right.high, 0): poly.add right[k]
    left.setLen(0)
    right.setLen(0)
    o.emit(poly)
    if closed or j != n - 1:
      let prev = pts[i]
      let v = pts[j]
      let next = pts[(j + 1) mod n]
      let d1 = vnorm(vsub(v, prev))
      let d2 = vnorm(vsub(next, v))
      let join = if style.join == ljRound: ljRound elif knot[j]: style.join else: ljMiter
      joinPiece(v, d1, d2, hw, join, if knot[j]: min(style.miterLimit, 500.0) else: Inf, tol, o)
  if not closed:
    capPiece(pts[0], vmul(vnorm(vsub(pts[1], pts[0])), -1), hw, style.cap, tol, o)
    capPiece(pts[n - 1], vnorm(vsub(pts[n - 1], pts[n - 2])), hw, style.cap, tol, o)

proc dashPolyline*(pl: Polyline; pattern: openArray[float64]; offset: float64): seq[Polyline] =
  ## Splits a polyline into dashes (open polylines). `pattern` alternates on/off lengths.
  if pattern.len == 0: return @[pl]
  if pattern.len > 32 or offset != offset or abs(offset) == Inf: return
  for d in pattern:
    if d != d or d < 0 or d == Inf: return
  for p in pl.pts:
    if p[0] != p[0] or p[1] != p[1]: return
  var pat = @pattern
  if pat.len mod 2 == 1: pat.add @pattern
  var total = 0.0
  for d in pat: total += d
  if total != total or total <= 1e-9 or pl.pts.len < 2: return
  var pts = pl.pts
  var knots = pl.knot
  knots.setLen(pts.len)
  for i in pl.knot.len ..< pts.len: knots[i] = true
  if pl.closed:
    pts.add pl.pts[0]
    knots.add true
  var idx = 0
  var left = pat[0]
  var skip = offset mod total
  if skip < 0: skip += total
  while skip > 0:
    if skip >= left:
      skip -= left
      idx = (idx + 1) mod pat.len
      left = pat[idx]
    else:
      left -= skip
      skip = 0
  var cur: Option[Polyline]
  if idx mod 2 == 0: cur = some(Polyline(pts: @[pts[0]], knot: @[true]))
  var guard = 0
  for wi in 0 ..< pts.len - 1:
    let (a, b) = (pts[wi], pts[wi + 1])
    let bKnot = knots[wi + 1]
    let seg = vlen(vsub(b, a))
    var pos = 0.0
    while seg - pos > 1e-12:
      inc guard
      if guard > 100_000: return @[]
      let step = min(left, seg - pos)
      if step > 0 and pos + step == pos: return @[]
      pos += step
      left -= step
      let atEnd = seg - pos <= 1e-12
      let p = if atEnd: b else: vadd(a, vmul(vsub(b, a), pos / seg))
      if cur.isSome:
        cur.get.pts.add p
        cur.get.knot.add(not atEnd or bKnot)
      if left <= 1e-12:
        idx = (idx + 1) mod pat.len
        left = pat[idx]
        if idx mod 2 == 1:
          if cur.isSome:
            result.add cur.get
            cur = none(Polyline)
        else: cur = some(Polyline(pts: @[p], knot: @[true]))
  if cur.isSome and cur.get.pts.len > 1: result.add cur.get
  if pl.closed and result.len > 0:
    let seam = pl.pts[0]
    let wraps = result[0].pts[0] == seam and result[^1].pts[^1] == seam
    if wraps and result.len == 1: result[0].closed = true
    elif wraps and result.len > 1:
      var tail = result.pop()
      for k in 1 ..< result[0].pts.len:
        tail.pts.add result[0].pts[k]
        tail.knot.add result[0].knot[k]
      result[0] = tail

proc strokePolygons*(lines: openArray[Polyline]; style: StrokeStyle; tol: float64): seq[seq[P]] =
  ## Stroke outline pieces (fill them with the non-zero rule).
  if style.width <= 0 or style.width != style.width or style.width == Inf or style.miterLimit != style.miterLimit or style.miterLimit < 1:
    return
  for pl in lines:
    var bad = false
    for p in pl.pts:
      if p[0] != p[0] or p[1] != p[1] or abs(p[0]) == Inf or abs(p[1]) == Inf: bad = true
    if bad: continue
    if style.dashes.len > 0:
      for d in dashPolyline(pl, style.dashes, style.dashOffset): strokeOne(d, style, tol, result)
    else: strokeOne(pl, style, tol, result)

# ---------------------------------------------------------------- path rasterizers

proc fillRasterizer*(path: Path; tol: float64): Rasterizer =
  ## The fill area: one component per joined subpath group, folded with the path operations.
  result = newRasterizer(path.inverted)
  for c in path.components:
    var polys: seq[seq[P]]
    for i in c: polys.add flattenSubpath(path.subpaths[i], tol).pts
    result.addComponent(polys, path.effectiveOp(c.a), path.fillRule)

proc strokeRasterizer*(path: Path; style: StrokeStyle; tol: float64): Rasterizer =
  let lines = flattenPath(path, max(min(tol * 4, max(style.width, 0.01) * 0.1), 1e-3))
  let polys = strokePolygons(lines, style, tol)
  result = newRasterizer(false)
  result.addComponent(polys, poCombine, frNonZero)

proc strokeStyle*(s: ShapeStroke): StrokeStyle =
  let w = float64(max(s.width, 0))
  result = StrokeStyle(width: w, cap: s.cap, join: s.join, miterLimit: float64(max(s.miterLimit, 1)),
                       dashOffset: float64(s.dashOffset) * w)
  for d in s.dashes: result.dashes.add float64(d) * w

proc pathCoverage*(path: Path; rc: Rect): seq[float32] = fillRasterizer(path, DefaultTolerance).render(rc)

type
  CompiledVectorMask* = object
    constant: bool
    value: float32
    raster: Rasterizer
    density: Option[float32]

proc compileVectorMask*(m: VectorMask): CompiledVectorMask =
  if not m.enabled: result = CompiledVectorMask(constant: true, value: 1)
  elif m.path.isEmpty: result = CompiledVectorMask(constant: true, value: (if m.path.inverted: 0 else: 1))
  else: result = CompiledVectorMask(raster: fillRasterizer(m.path, DefaultTolerance))
  if m.enabled and m.density < 1: result.density = some(clamp(m.density, 0'f32, 1'f32))

proc render*(m: CompiledVectorMask; rc: Rect): seq[float32] =
  if m.constant:
    result = newSeq[float32](max(rc.width * rc.height, 0))
    for v in result.mitems: v = m.value
  else: result = m.raster.render(rc)
  if m.density.isSome:
    let d = m.density.get
    for v in result.mitems: v = 1 - d * (1 - v)

proc vectorMaskValues*(m: VectorMask; rc: Rect): seq[float32] = compileVectorMask(m).render(rc)

proc tileBands(r: Rect): seq[Rect] =
  if r.isEmpty: return
  var y = divEuclid(r.y0, TileSize) * TileSize
  while y < r.y1:
    result.add rect(r.x0, max(y, r.y0), r.x1, min(y + TileSize, r.y1))
    y += TileSize

proc writeBandTiles(s: var Surface; band: Rect; data: seq[float32]; channels: int; keep: proc (px: openArray[float32]): bool) =
  ## Writes the tile-aligned pieces of a band buffer, skipping pieces `keep` rejects everywhere.
  let w = band.width
  var x = divEuclid(band.x0, TileSize) * TileSize
  while x < band.x1:
    let tr = rect(max(x, band.x0), band.y0, min(x + TileSize, band.x1), band.y1)
    let tw = tr.width
    var buf = newSeqOfCap[float32](tw * tr.height * channels)
    var any = false
    for row in 0 ..< tr.height:
      let o = (row * w + (tr.x0 - band.x0)) * channels
      for k in 0 ..< tw:
        if not any and keep(data.toOpenArray(o + k * channels, o + k * channels + channels - 1)): any = true
      for k in o ..< o + tw * channels: buf.add data[k]
    if any: s.writeRegion(tr, buf)
    x += TileSize

proc coverageSurface*(r: Rasterizer; format: PixelFormat; clip: Rect): Surface =
  ## A single-channel grayscale surface of the coverage over `clip`.
  let fmt = PixelFormat(mode: cmGrayscale, sample: format.sample, alpha: false)
  result = newSurface(fmt)
  let pb = r.pixelBounds
  let area = if r.isInverted: clip elif pb.isSome: pb.get.intersect(clip) else: EmptyRect
  for band in tileBands(area):
    let cov = r.render(band)
    writeBandTiles(result, band, cov, 1, proc (p: openArray[float32]): bool = p[0] > 0)

# ---------------------------------------------------------------- paint

type
  PaintKind* = enum pkNone, pkSolid, pkGradient
  Paint* = object
    kind*: PaintKind
    solid*: array[4, float32]
    stops*: seq[(float32, array[4, float32])]
    midpoints*: seq[float32]
    opacityStops*: seq[(float32, float32)]
    style*: GradientStyle
    layout*: (float32, float32, (float32, float32))  ## effective angle, scale, offset
    reverse*, dither*: bool
    frame*: (float64, float64, float64, float64)

proc gradientUnits*(angle, w, h: float64): (float64, float64) =
  ## (chord, diagonal) gradient lengths over a `w × h` frame at `angle` (radians).
  let (s, c) = (sin(angle), cos(angle))
  let ln = max(sqrt(pow(c * w, 2) + pow(s * h, 2)), 1.0)
  let chord = max(min(w / max(abs(c), 1e-6), h / max(abs(s), 1e-6)), 1.0)
  (chord, ln)

proc finiteF(v: float32): bool = v == v and abs(v) != Inf

proc gradientLayout*(style: GradientStyle; angle, scale: float32; offset: (float32, float32);
                     frame: (float64, float64, float64, float64)): (float32, float32, (float32, float32)) =
  ## Effective layout snapping Linear/Reflected end points to whole pixels, as Photoshop does.
  let unchanged = (angle, scale, offset)
  if style notin {gsLinear, gsReflected} or not angle.finiteF or not scale.finiteF or not offset[0].finiteF or not offset[1].finiteF:
    return unchanged
  let w = max(frame[2] - frame[0], 1.0)
  let h = max(frame[3] - frame[1], 1.0)
  let (cx, cy) = (frame[0] + w / 2 + float64(offset[0]) * w, frame[1] + h / 2 + float64(offset[1]) * h)
  let a = degToRad(float64(angle))
  let half = gradientUnits(a, w, h)[0] * float64(max(scale, 1e-3)) / 2
  let (dx, dy) = (cos(a) * half, -sin(a) * half)
  template snap(v: float64): float64 = floor(v + 1e-4)
  let e = (snap(cx + dx), snap(cy + dy))
  var start, mid: (float64, float64)
  if style == gsReflected:
    start = (cx, cy)
    mid = (cx, cy)
  else:
    start = (snap(cx - dx), snap(cy - dy))
    mid = ((start[0] + e[0]) / 2, (start[1] + e[1]) / 2)
  let v = (e[0] - start[0], e[1] - start[1])
  let ln = hypot(v[0], v[1])
  if ln < 0.5: return unchanged
  let a2 = arctan2(-v[1], v[0])
  let span = if style == gsReflected: 2 * ln else: ln
  let scale2 = span / gradientUnits(a2, w, h)[0]
  (float32(radToDeg(a2)), float32(scale2), (offset[0] + float32((mid[0] - cx) / w), offset[1] + float32((mid[1] - cy) / h)))

proc gradientT*(style: GradientStyle; angle, scale: float32; reverse: bool; offset: (float32, float32);
                frame: (float64, float64, float64, float64); x, y: float64): float32 =
  let w = max(frame[2] - frame[0], 1.0)
  let h = max(frame[3] - frame[1], 1.0)
  let cx = frame[0] + w / 2 + float64(offset[0]) * w
  let cy = frame[1] + h / 2 + float64(offset[1]) * h
  let a = degToRad(float64(angle))
  let (s, c) = (sin(a), cos(a))
  let (dx, dy) = (x - cx, y - cy)
  let along = dx * c - dy * s
  let across = dx * s + dy * c
  var (chord, ln) = gradientUnits(a, w, h)
  chord *= float64(max(scale, 1e-3))
  ln *= float64(max(scale, 1e-3))
  let corner = 0.5 * (c - s)
  var t = case style
    of gsLinear: (along - corner) / chord + 0.5
    of gsReflected: abs((along - corner) / (chord / 2))
    of gsRadial: hypot(dx, dy) / (ln / 2)
    of gsDiamond: (abs(along) + abs(across)) / (ln / 2)
    of gsAngle:
      (let v = (a - arctan2(-dy, dx)) / TAU; v - floor(v))
  t = clamp(t, 0.0, 1.0)
  float32(if reverse: 1 - t else: t)

proc midpointRemap*(u, m0: float32): float32 =
  let m = clamp(m0, 0.05'f32, 0.95'f32)
  if abs(m - 0.5) < 1e-6: u
  elif u <= m: 0.5 * u / m
  else: 0.5 + 0.5 * (u - m) / (1 - m)

proc sampleColor*(stops: openArray[(float32, array[4, float32])]; mids: openArray[float32]; t: float32): array[4, float32] =
  if stops.len == 0: return
  if t <= stops[0][0]: return stops[0][1]
  for i in 0 ..< stops.len - 1:
    let (a, b) = (stops[i], stops[i + 1])
    if t <= b[0]:
      let u = if b[0] > a[0]: (t - a[0]) / (b[0] - a[0]) else: 0'f32
      let k = midpointRemap(u, if i < mids.len: mids[i] else: 0.5'f32)
      for ch in 0..3: result[ch] = a[1][ch] + (b[1][ch] - a[1][ch]) * k
      return
  stops[^1][1]

proc sampleOpacity*(stops: openArray[(float32, float32)]; t: float32): float32 =
  if stops.len == 0: return 1
  if t <= stops[0][0]: return stops[0][1]
  for i in 0 ..< stops.len - 1:
    if t <= stops[i + 1][0]:
      let u = if stops[i + 1][0] > stops[i][0]: (t - stops[i][0]) / (stops[i + 1][0] - stops[i][0]) else: 0'f32
      return stops[i][1] + (stops[i + 1][1] - stops[i][1]) * u
  stops[^1][1]

proc ditherPx*(c: var array[4, float32]; x, y: int) =
  let n = (ditherNoise(x, y) - 0.5'f32) / 255
  for ch in 0..2: c[ch] = clamp(c[ch] + n, 0'f32, 1'f32)

proc paintFromFill*(f: Fill; bounds, canvas: (float64, float64, float64, float64)): Paint =
  proc rgba(c: Color): array[4, float32] =
    let v = c.toRgb
    [v[0], v[1], v[2], c.alpha]
  case f.kind
  of fkSolid: Paint(kind: pkSolid, solid: rgba(f.color))
  of fkGradient:
    let frame = if f.align: bounds else: canvas
    var stops: seq[(float32, array[4, float32])]
    for (p, c) in f.stops: stops.add (p, rgba(c))
    stops.sort(proc (a, b: (float32, array[4, float32])): int = cmp(a[0], b[0]))
    var op = f.opacityStops
    op.sort(proc (a, b: (float32, float32)): int = cmp(a[0], b[0]))
    Paint(kind: pkGradient, stops: stops, midpoints: f.midpoints, opacityStops: op, style: f.style,
          layout: gradientLayout(f.style, f.angle, f.scale, f.offset, frame), reverse: f.reverse,
          dither: f.dither, frame: frame)
  of fkPattern: Paint(kind: pkNone)

proc at*(p: Paint; x, y: float64): array[4, float32] =
  case p.kind
  of pkNone: discard
  of pkSolid: result = p.solid
  of pkGradient:
    let t = gradientT(p.style, p.layout[0], p.layout[1], p.reverse, p.layout[2], p.frame, x, y)
    result = sampleColor(p.stops, p.midpoints, t)
    if p.opacityStops.len > 0: result[3] *= sampleOpacity(p.opacityStops, t)
    if p.dither: ditherPx(result, int(floor(x)), int(floor(y)))

# ---------------------------------------------------------------- shapes

type CompiledShape* = object
  hasFill: bool
  fillR: Rasterizer
  fillPaint: Paint
  hasStroke: bool
  strokeR: Rasterizer
  strokePaint: Paint
  strokeOpacity: float32
  hasAlign: bool
  alignR: Rasterizer
  alignInside: bool
  bounds*: Option[Rect]

proc compileShape*(shape: ShapeLayer; tol: float64; canvas: Rect): CompiledShape =
  let pbo = shape.path.controlBounds
  let pb = if pbo.isSome: pbo.get else: (0.0, 0.0, 0.0, 0.0)
  let cb = (float64(canvas.x0), float64(canvas.y0), float64(canvas.x1), float64(canvas.y1))
  let area = fillRasterizer(shape.path, tol)
  if shape.fill.isSome:
    result.hasFill = true
    result.bounds = area.pixelBounds
    result.fillR = area
    result.fillPaint = paintFromFill(shape.fill.get, pb, cb)
  var allClosed = true
  for s in shape.path.subpaths:
    if not s.closed: allClosed = false
  if shape.stroke.isSome and shape.stroke.get.width > 0 and shape.stroke.get.opacity > 0:
    let s = shape.stroke.get
    var st = strokeStyle(s)
    if allClosed and s.align != saCenter:
      st.width *= 2
      result.hasAlign = true
      result.alignR = area
      result.alignInside = s.align == saInside
    let r = strokeRasterizer(shape.path, st, tol)
    let sb = r.pixelBounds
    if result.bounds.isSome and sb.isSome: result.bounds = some(result.bounds.get.union(sb.get))
    elif sb.isSome: result.bounds = sb
    result.hasStroke = true
    result.strokeR = r
    result.strokePaint = paintFromFill(s.paint, pb, cb)
    result.strokeOpacity = clamp(s.opacity, 0'f32, 1'f32)
  if shape.path.inverted and shape.fill.isSome:
    result.bounds = some(rect(int(low(int32)) div 4, int(low(int32)) div 4, int(high(int32)) div 4, int(high(int32)) div 4))

proc renderRgba*(cs: CompiledShape; rc: Rect): seq[array[4, float32]] =
  let n = max(rc.width * rc.height, 0)
  result = newSeq[array[4, float32]](n)
  let w = rc.width
  if cs.hasFill:
    let cov = cs.fillR.render(rc)
    for i in 0 ..< n:
      if cov[i] > 0:
        var p = cs.fillPaint.at(float64(rc.x0 + i mod w) + 0.5, float64(rc.y0 + i div w) + 0.5)
        p[3] *= cov[i]
        result[i] = p
  if cs.hasStroke:
    var cov = cs.strokeR.render(rc)
    if cs.hasAlign:
      let a = cs.alignR.render(rc)
      for i in 0 ..< n: cov[i] *= (if cs.alignInside: a[i] else: 1 - a[i])
    for i in 0 ..< n:
      if cov[i] <= 0: continue
      let s = cs.strokePaint.at(float64(rc.x0 + i mod w) + 0.5, float64(rc.y0 + i div w) + 0.5)
      let sa = s[3] * cov[i] * cs.strokeOpacity
      let da = result[i][3]
      let a = sa + da * (1 - sa)
      if a > 0:
        for k in 0..2: result[i][k] = (s[k] * sa + result[i][k] * da * (1 - sa)) / a
      result[i][3] = a

proc render*(cs: CompiledShape; format: PixelFormat; clip: Rect): Surface =
  result = newSurface(format)
  if cs.bounds.isNone: return
  let area = cs.bounds.get.intersect(clip)
  let ch = format.channels
  let alpha = format.alpha
  for band in tileBands(area):
    let rgba = cs.renderRgba(band)
    var vals = newSeq[float32](rgba.len * ch)
    var tmp = newSeq[float32](ch)
    for i, p in rgba:
      if p[3] > 0:
        discard fromRgbaInto(format, p, tmp)
        for k in 0 ..< ch: vals[i * ch + k] = tmp[k]
    writeBandTiles(result, band, vals, ch, proc (px: openArray[float32]): bool =
      if alpha: px[ch - 1] > 0
      else:
        var any = false
        for v in px:
          if v != 0: any = true
        any)

proc renderShape*(shape: ShapeLayer; format: PixelFormat; clip: Rect): Surface =
  ## A shape layer's appearance (fill, then stroke over it), clipped to `clip` (the canvas).
  compileShape(shape, DefaultTolerance, clip).render(format, clip)
