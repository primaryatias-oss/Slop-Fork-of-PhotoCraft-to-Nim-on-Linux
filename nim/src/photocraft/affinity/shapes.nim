## Parametric shapes (port of `affinity/src/shapes.rs`): Affinity stores the shape's parameters
## and its box, never the outline. Formulas were fitted to Affinity's own renders (the document
## thumbnails) of public files.

import std/[math, options]
import ./stream, ./types
import ../rmath

const
  Kappa = 0.5522847498307934 ## handle length of a quarter circle of radius 1
  MaxSides = 10_000'i64 ## vertices of a polygon or star
  FracPi2 = PI / 2.0

proc last(r: Reader; id: ObjId; tag: string): ptr SValue =
  ## The last value of a tag: a shape changed from one kind to another keeps a stale first block.
  if id < 0 or id >= r.s.objects.len: return nil
  let t = tagOf(tag)
  for i in countdown(r.s.objects[id].fields.high, 0):
    if r.s.objects[id].fields[i][0] == t: return unsafeAddr r.s.objects[id].fields[i][1]
  nil

proc num(r: Reader; id: ObjId; tag: string): Option[float64] =
  let v = last(r, id, tag)
  if v == nil: return
  case v.kind
  of svFloat: (if v.f == v.f and abs(v.f) != Inf: some(v.f) else: none(float64))
  of svUInt: some(float64(v.u))
  of svInt: some(float64(v.i))
  else: none(float64)

proc numOr(r: Reader; id: ObjId; tag: string; default: float64): float64 =
  let v = num(r, id, tag)
  if v.isSome: v.get else: default

proc fclamp(x, a, b: float64): float64 {.inline.} =
  ## Rust `f64::clamp` for `a <= b` (Rust panics otherwise; this returns `a`).
  if x < a: a elif x > b: b else: x

proc pow2(n: int): float64 =
  ## 2^n for exponents in the normal range.
  cast[float64](uint64(clamp(n, -1022, 1023) + 1023) shl 52)

proc addTurns(e0, start: float64): float64 =
  ## Rust's `while end < start { end += TAU }`, with the same rounding at every step, but
  ## fast-forwarded through runs where each addition rounds the same way: hostile angles such as
  ## 2e9 take hundreds of millions of steps. Where an addition no longer changes the value (Rust
  ## loops forever there) the value is returned as it is.
  var e = e0
  while e < start:
    let nxt = e + TAU
    if nxt == e: return e
    let a = abs(e)
    if a >= 1.0:
      var x: int
      discard frexp(e, x) # |e| in [2^(x-1), 2^x)
      let u = pow2(x - 53) # float spacing in that range
      let rem = TAU - floor(TAU / u) * u
      if rem != u / 2.0: # a tie rounds to even, which alternates the step; take it one at a time
        let r = nxt - e
        let hi = if e > 0.0: pow2(x) else: -pow2(x - 1)
        let n = min(floor((hi - TAU - e) / r), floor((start - e) / r)) - 2.0
        if n >= 1.0:
          e = e + n * r
          continue
    e = nxt
  e

proc sides(r: Reader; id: ObjId; tag: string; default: float64): int =
  let n = round(numOr(r, id, tag, default))
  int(fclamp(n, 3.0, float64(MaxSides)))

proc polygon(pts: openArray[AfPoint]): Option[AfSubPath] =
  if pts.len == 0: return
  var s = AfSubPath(start: pts[0], closed: true)
  for i in 1 ..< pts.len: s.segments.add [pts[i], pts[i], pts[i]]
  some(s)

proc rectangle*(x0, y0, x1, y1: float64): AfSubPath =
  AfSubPath(start: pt(x0, y0), closed: true, segments: @[
    [pt(x1, y0), pt(x1, y0), pt(x1, y0)], [pt(x1, y1), pt(x1, y1), pt(x1, y1)], [pt(x0, y1), pt(x0, y1), pt(x0, y1)]])

proc ellipse(cx, cy, rx, ry: float64): AfSubPath =
  let (kx, ky) = (Kappa * rx, Kappa * ry)
  AfSubPath(start: pt(cx + rx, cy), closed: true, segments: @[
    [pt(cx + rx, cy + ky), pt(cx + kx, cy + ry), pt(cx, cy + ry)],
    [pt(cx - kx, cy + ry), pt(cx - rx, cy + ky), pt(cx - rx, cy)],
    [pt(cx - rx, cy - ky), pt(cx - kx, cy - ry), pt(cx, cy - ry)],
    [pt(cx + kx, cy - ry), pt(cx + rx, cy - ky), pt(cx + rx, cy)]])

proc roundedRect(b: array[4, float64]; rx, ry: float64): AfSubPath =
  let (x0, y0, x1, y1) = (b[0], b[1], b[2], b[3])
  let (kx, ky) = ((1.0 - Kappa) * rx, (1.0 - Kappa) * ry)
  AfSubPath(start: pt(x0 + rx, y0), closed: true, segments: @[
    [pt(x1 - rx, y0), pt(x1 - rx, y0), pt(x1 - rx, y0)],
    [pt(x1 - kx, y0), pt(x1, y0 + ky), pt(x1, y0 + ry)],
    [pt(x1, y1 - ry), pt(x1, y1 - ry), pt(x1, y1 - ry)],
    [pt(x1, y1 - ky), pt(x1 - kx, y1), pt(x1 - rx, y1)],
    [pt(x0 + rx, y1), pt(x0 + rx, y1), pt(x0 + rx, y1)],
    [pt(x0 + kx, y1), pt(x0, y1 - ky), pt(x0, y1 - ry)],
    [pt(x0, y0 + ry), pt(x0, y0 + ry), pt(x0, y0 + ry)],
    [pt(x0, y0 + ky), pt(x0 + kx, y0), pt(x0 + rx, y0)]])

proc steps(v: float64): int =
  ## Rust `v as usize` for a value already clamped to 1..8 (NaN → 0).
  if v != v: 0 else: int(v)

proc arc(cx, cy, rx, ry, a0, a1: float64; outp: var seq[array[3, AfPoint]]) =
  ## Elliptical arc from angle `a0` to `a1` (radians, measured y-up as Affinity's pie angles are),
  ## as cubic segments of at most a quarter turn.
  let n = steps(fclamp(ceil(abs(a1 - a0) / FracPi2), 1.0, 8.0))
  let step = (a1 - a0) / float64(n)
  let k = 4.0 / 3.0 * tan(step / 4.0)
  template p(a: float64): AfPoint = pt(cx + rx * cos(a), cy - ry * sin(a))
  template d(a: float64): AfPoint = pt(-rx * sin(a), -ry * cos(a))
  for i in 0 ..< n:
    let (s, e) = (a0 + step * float64(i), a0 + step * float64(i + 1))
    let (ps, pe, ds, de) = (p(s), p(e), d(s), d(e))
    outp.add [pt(ps.x + k * ds.x, ps.y + k * ds.y), pt(pe.x - k * de.x, pe.y - k * de.y), pe]

proc pie(cx, cy, rx, ry, a0, a1, inner: float64): AfSubPath =
  result = AfSubPath(start: pt(cx + rx * cos(a0), cy - ry * sin(a0)), closed: true)
  arc(cx, cy, rx, ry, a0, a1, result.segments)
  if inner > 0.0:
    let p = pt(cx + inner * rx * cos(a1), cy - inner * ry * sin(a1))
    result.segments.add [p, p, p]
    arc(cx, cy, inner * rx, inner * ry, a1, a0, result.segments)
  else:
    let c = pt(cx, cy)
    result.segments.add [c, c, c]

proc cloud(cx, cy, rx, ry: float64; bubbles: int): AfSubPath =
  let n = clamp(bubbles, 3, 128) * 4
  var points = newSeq[AfPoint](n)
  for i in 0 ..< n:
    let a = TAU * float64(i) / float64(n) - FracPi2
    let l = 0.5 - 0.5 * cos(TAU * float64(i) / 4.0)
    let lobe = l * l
    let radius = 0.78 + 0.22 * lobe
    points[i] = pt(cx + rx * radius * cos(a), cy + ry * radius * sin(a))
  result = AfSubPath(start: points[0], closed: true)
  for i in 0 ..< n:
    let p0 = points[(i + n - 1) mod n]
    let p1 = points[i]
    let p2 = points[(i + 1) mod n]
    let p3 = points[(i + 2) mod n]
    result.segments.add [pt(p1.x + (p2.x - p0.x) / 6.0, p1.y + (p2.y - p0.y) / 6.0),
                         pt(p2.x - (p3.x - p1.x) / 6.0, p2.y - (p3.y - p1.y) / 6.0), p2]

proc heart(b: array[4, float64]): AfSubPath =
  let (x0, y0, x1, y1) = (b[0], b[1], b[2], b[3])
  let (w, h) = (x1 - x0, y1 - y0)
  template p(x, y: float64): AfPoint = pt(x0 + x * w, y0 + y * h)
  let start = p(0.5, 0.92)
  AfSubPath(start: start, closed: true, segments: @[
    [p(0.38, 0.85), p(0.04, 0.67), p(0.04, 0.37)],
    [p(0.04, 0.02), p(0.39, 0.00), p(0.5, 0.27)],
    [p(0.61, 0.00), p(0.96, 0.02), p(0.96, 0.37)],
    [p(0.96, 0.67), p(0.62, 0.85), start]])

proc cog(r: Reader; shape: ObjId; cx, cy, rx, ry: float64): AfSubPath =
  let teeth = sides(r, shape, "Teth", 12.0)
  let tooth = fclamp(numOr(r, shape, "TtSz", 0.3), 0.05, 0.9)
  let notch = fclamp(numOr(r, shape, "NtSz", 0.45), 0.05, 0.95)
  let tip = 1.0
  let root = fclamp(1.0 - 0.2 * tooth - 0.08 * notch, 0.55, 0.95)
  var pts = newSeqOfCap[AfPoint](teeth * 4)
  for i in 0 ..< teeth:
    let base = TAU * float64(i) / float64(teeth) - FracPi2
    for (offset, radius) in [(-0.5, root), (-0.32, tip), (0.32, tip), (0.5, root)]:
      let a = base + offset * TAU / float64(teeth)
      pts.add pt(cx + rx * radius * cos(a), cy + ry * radius * sin(a))
  let p = polygon(pts)
  if p.isSome: p.get else: rectangle(cx - rx, cy - ry, cx + rx, cy + ry)

proc calloutRect(r: Reader; shape: ObjId; b: array[4, float64]): AfSubPath =
  let (x0, y0, x1, y1) = (b[0], b[1], b[2], b[3])
  let (w, h) = (x1 - x0, y1 - y0)
  let tailH = fclamp(numOr(r, shape, "TlHg", 0.2), 0.0, 0.8) * h
  let tailW = fclamp(numOr(r, shape, "TlWd", 0.1), 0.02, 0.8) * w
  let tipX = x0 + fclamp(numOr(r, shape, "TlRP", 0.5), 0.0, 1.0) * w
  let baseX = x0 + fclamp(numOr(r, shape, "TlEP", 0.4), 0.0, 1.0) * w
  let a = fclamp(baseX - tailW / 2.0, x0, x1)
  let c = fclamp(baseX + tailW / 2.0, x0, x1)
  let bodyBottom = y1 - tailH
  let radius = rmax(rmin(w, bodyBottom - y0) * 0.12, 0.0)
  # The three contours overlap inside the fill; no internal edge is visible in a filled shape.
  # Join the tail to the rounded body in one closed outline to avoid a seam in stroked shapes.
  let start = pt(x0 + radius, y0)
  AfSubPath(start: start, closed: true, segments: @[
    [pt(x1 - radius, y0), pt(x1 - radius, y0), pt(x1 - radius, y0)],
    [pt(x1 - (1.0 - Kappa) * radius, y0), pt(x1, y0 + (1.0 - Kappa) * radius), pt(x1, y0 + radius)],
    [pt(x1, bodyBottom - radius), pt(x1, bodyBottom - radius), pt(x1, bodyBottom - radius)],
    [pt(x1, bodyBottom), pt(x1, bodyBottom), pt(x1 - radius, bodyBottom)],
    [pt(c + radius * 0.2, bodyBottom), pt(tipX, y1 - tailH * 0.15), pt(tipX, y1)],
    [pt(tipX, y1), pt(tipX, y1), pt(tipX, y1)],
    [pt(tipX, y1 - tailH * 0.15), pt(a - radius * 0.2, bodyBottom), pt(a, bodyBottom)],
    [pt(x0 + radius, bodyBottom), pt(x0, bodyBottom), pt(x0, bodyBottom - radius)],
    [pt(x0, y0 + radius), pt(x0, y0 + radius), pt(x0, y0 + radius)],
    [pt(x0, y0 + (1.0 - Kappa) * radius), pt(x0 + (1.0 - Kappa) * radius, y0), start]])

proc calloutEllipse(r: Reader; shape: ObjId; cx, cy, rx, ry: float64): AfSubPath =
  let tailH = fclamp(numOr(r, shape, "TlHg", 0.2), 0.0, 0.8)
  let pos = fclamp(numOr(r, shape, "TlEP", 0.25), 0.0, 1.0)
  let angle = numOr(r, shape, "TlAn", 0.0)
  let a0 = pos * TAU - angle
  let a1 = a0 + 0.35
  let start = pt(cx + rx * cos(a1), cy + ry * sin(a1))
  let e = pt(cx + rx * cos(a0), cy + ry * sin(a0))
  let tip = pt(cx + rx * cos(a0 + 0.18) * 1.08, cy + ry * sin(a0 + 0.18) * (1.0 + tailH))
  # A single ellipse-like outline with a short triangular tail at its lower edge.
  result = AfSubPath(start: start, closed: true)
  arc(cx, cy, rx, ry, a1, a0 + TAU, result.segments)
  result.segments.add [e, e, tip]
  result.segments.add [tip, tip, start]

proc arrow(r: Reader; shape: ObjId; b: array[4, float64]): AfSubPath =
  let (x0, y0, x1, y1) = (b[0], b[1], b[2], b[3])
  let h = y1 - y0
  let thick = fclamp(numOr(r, shape, "Thck", 0.35), 0.05, 0.9) * h
  let shaft = (y0 + y1 - thick) / 2.0
  let lower = shaft + thick
  let headStart = x1 - h * 0.45
  let p = polygon([pt(x0, shaft), pt(headStart, shaft), pt(headStart, y0), pt(x1, (y0 + y1) / 2.0),
                   pt(headStart, y1), pt(headStart, lower), pt(x0, lower)])
  if p.isSome: p.get else: rectangle(x0, shaft, x1, lower)

proc doubleStar(r: Reader; shape: ObjId; cx, cy, rx, ry: float64): AfSubPath =
  let n = clamp(sides(r, shape, "Pnts", 6.0) * 2, 8, 64)
  let inner = fclamp(numOr(r, shape, "IRad", 0.6), 0.05, 0.95)
  let outer = fclamp(numOr(r, shape, "PRad", 0.9), 0.2, 1.0)
  var pts: seq[AfPoint]
  for i in 0 ..< 2 * n:
    let a = -FracPi2 + PI * float64(i) / float64(n)
    let radius = if i mod 2 == 0: outer else: outer * inner
    pts.add pt(cx + rx * radius * cos(a), cy + ry * radius * sin(a))
  let p = polygon(pts)
  if p.isSome: p.get else: rectangle(cx - rx, cy - ry, cx + rx, cy + ry)

proc tear(r: Reader; shape: ObjId; b: array[4, float64]): AfSubPath =
  let (x0, y0, x1, y1) = (b[0], b[1], b[2], b[3])
  let (w, h) = (x1 - x0, y1 - y0)
  let bulge = fclamp(numOr(r, shape, "Ball", 0.25), 0.0, 0.5)
  let bend = fclamp(numOr(r, shape, "Bend", 0.0), -0.5, 0.5)
  let top = pt((x0 + x1) / 2.0, y0)
  let bottom = pt((x0 + x1) / 2.0 + bend * w * 0.25, y1)
  AfSubPath(start: top, closed: true, segments: @[
    [pt(x0 + w * 0.25, y0 + h * 0.42), pt(x0 + w * (0.5 - bulge), y1 - h * 0.03), bottom],
    [pt(x1 - w * (0.5 - bulge), y1 - h * 0.03), pt(x1 - w * 0.25, y0 + h * 0.42), top]])

proc crescent(cx, cy, rx, ry: float64): AfSubPath =
  result = AfSubPath(start: pt(cx, cy - ry), closed: true)
  arc(cx, cy, rx, ry, -FracPi2, -3.0 * FracPi2, result.segments)
  # The inner arc returns from the lower tip to the upper tip, leaving a crescent open to the right.
  let ix = cx + rx * 0.72
  let irx = rx * 0.86
  let iry = ry * 0.82
  let src = pt(ix, cy + iry)
  result.segments.add [src, src, src]
  arc(ix, cy, irx, iry, FracPi2, -FracPi2, result.segments)

proc segment(r: Reader; shape: ObjId; cx, cy, rx, ry: float64): AfSubPath =
  # Keep hostile but finite angles in a small range before constructing arc controls.
  let angle = remEuclidF(numOr(r, shape, "Angl", 0.0), TAU)
  let p0 = fclamp(numOr(r, shape, "Pos0", 0.25), 0.0, 1.0)
  let p1 = fclamp(numOr(r, shape, "Pos1", 0.75), 0.0, 1.0)
  let a0 = angle + (p0 - 0.5) * TAU
  var a1 = angle + (p1 - 0.5) * TAU
  while a1 < a0: a1 += TAU
  let start = pt(cx + rx * cos(a0), cy + ry * sin(a0))
  let e = pt(cx + rx * cos(a1), cy + ry * sin(a1))
  result = AfSubPath(start: start, closed: true)
  arc(cx, cy, rx, ry, a0, rmin(a1, a0 + TAU), result.segments)
  result.segments.add [e, e, start]

proc shape*(r: var Reader; shape: ObjId; b: array[4, float64]; world: AfAffine): Option[(AfPath, bool)] =
  ## Outline of `shape` (a `Shpe` object) in the node box `b` = (x0, y0, x1, y1).
  let (x0, y0, x1, y1) = (b[0], b[1], b[2], b[3])
  let (w, h) = (x1 - x0, y1 - y0)
  let (cx, cy, rx, ry) = ((x0 + x1) / 2.0, (y0 + y1) / 2.0, w / 2.0, h / 2.0)
  template onEllipse(a, f: float64): AfPoint = pt(cx + f * rx * cos(a), cy + f * ry * sin(a))
  let cls = r.s.class(shape)
  if cls.isNone: return
  let class = cls.get
  var sub: AfSubPath
  case $class
  of "ShNR", "ShRR":
    let ct = last(r, shape, "CTyp")
    let rounded = ct != nil and ct.kind == svArray and ct.arr.len > 0
    var radius = none(float64)
    let cr = last(r, shape, "ShCR")
    if cr != nil and cr.kind == svFloats and cr.floats.len > 0:
      let f = cr.floats[0]
      if f == f and abs(f) != Inf and f > 0.0: radius = some(f)
    if rounded and radius.isSome:
      let f = radius.get
      var other = false
      for t in ct.arr:
        if not (t.kind == svEnum and t.enumId in [0'u16, 4'u16]): other = true
      if other:
        r.warn("rectangles with straight, concave or other corner types (imported with round corners)")
      var crx, cry: float64
      if r.s.boolean(shape, "AbSz") == some(true):
        # An absolute radius is in document pixels, whatever the node's scale.
        let (a, b2, c, d) = (world.m[0], world.m[1], world.m[2], world.m[3])
        let (sx, sy) = (sqrt(a * a + b2 * b2), sqrt(c * c + d * d))
        if sx > 0.0 and sy > 0.0:
          crx = f / sx
          cry = f / sy
      else:
        crx = f * rmin(w, h)
        cry = crx
      sub = roundedRect(b, rmin(crx, w / 2.0), rmin(cry, h / 2.0))
    else:
      let p = polygon([pt(x0, y0), pt(x1, y0), pt(x1, y1), pt(x0, y1)])
      if p.isNone: return
      sub = p.get
  of "ShpE": sub = ellipse(cx, cy, rx, ry)
  of "ShPy":
    let n = sides(r, shape, "Side", 5.0)
    template a(i: int): float64 = -FracPi2 + TAU * float64(i) / float64(n)
    let sm = last(r, shape, "Smth")
    if sm != nil and sm.kind == svBool and sm.b:
      # Smoothed: tangent handles of length curv·4/3·tan(π/2n) along the inscribed ellipse.
      let k = numOr(r, shape, "Curv", 0.0) * 4.0 / 3.0 * tan(PI / (2.0 * float64(n)))
      template v(i: int): AfPoint = onEllipse(a(i), 1.0)
      template t(i: int): AfPoint = pt(-rx * sin(a(i)), ry * cos(a(i)))
      sub = AfSubPath(start: v(0), closed: true)
      for i in 0 ..< n:
        let (p, q, tp, tq) = (v(i), v(i + 1), t(i), t(i + 1))
        sub.segments.add [pt(p.x + k * tp.x, p.y + k * tp.y), pt(q.x - k * tq.x, q.y - k * tq.y), q]
    else:
      var pts: seq[AfPoint]
      for i in 0 ..< n: pts.add onEllipse(a(i), 1.0)
      let p = polygon(pts)
      if p.isNone: return
      sub = p.get
  of "ShSt":
    let n = sides(r, shape, "Pnts", 5.0)
    let q = fclamp(numOr(r, shape, "IRad", 0.382), 0.0, 1.0)
    for t in ["CrcI", "CrcO", "CrvL", "CrvR"]:
      let v = num(r, shape, t)
      if v.isSome and abs(v.get) > 1e-9:
        r.warn("stars with rounded or curved points (imported with sharp points)")
        break
    var pts: seq[AfPoint]
    for i in 0 ..< 2 * n:
      pts.add onEllipse(-FracPi2 + PI * float64(i) / float64(n), if i mod 2 == 0: 1.0 else: q)
    let p = polygon(pts)
    if p.isNone: return
    sub = p.get
  of "ShSS":
    let n = sides(r, shape, "Side", 5.0)
    let ri = fclamp(numOr(r, shape, "COut", 0.5), 0.0, 1.0)
    let step = PI / float64(n)
    let (l, hw, inner) = (cos(step), ri * sin(step), ri * cos(step))
    var pts: seq[AfPoint]
    for k in 0 ..< n:
      let phi = -FracPi2 + step + TAU * float64(k) / float64(n)
      let (c, s) = (cos(phi), sin(phi))
      for (u, v) in [(inner, -hw), (l, -hw), (l, hw), (inner, hw)]:
        pts.add pt(cx + rx * (u * c - v * s), cy + ry * (u * s + v * c))
    let p = polygon(pts)
    if p.isNone: return
    sub = p.get
  of "ShPi":
    let start = numOr(r, shape, "AngS", 0.0)
    let e = addTurns(numOr(r, shape, "AngE", TAU), start)
    let inner = fclamp(numOr(r, shape, "IRad", 0.0), 0.0, 1.0)
    sub = pie(cx, cy, rx, ry, start, rmin(e, start + TAU), inner)
  of "ShpT":
    let p = numOr(r, shape, "Pos ", 0.5)
    sub = polygon([pt(x0 + p * w, y0), pt(x1, y1), pt(x0, y1)]).get
  of "ShTz":
    let (l, rr) = (numOr(r, shape, "PosL", 0.25), numOr(r, shape, "PosR", 0.75))
    sub = polygon([pt(x0 + l * w, y0), pt(x0 + rr * w, y0), pt(x1, y1), pt(x0, y1)]).get
  of "ShCl": sub = cloud(cx, cy, rx, ry, sides(r, shape, "Bubl", 12.0))
  of "ShHt": sub = heart(b)
  of "ShCg": sub = cog(r, shape, cx, cy, rx, ry)
  of "ShCR": sub = calloutRect(r, shape, b)
  of "ShCE": sub = calloutEllipse(r, shape, cx, cy, rx, ry)
  of "ShDA": sub = arrow(r, shape, b)
  of "ShDS": sub = doubleStar(r, shape, cx, cy, rx, ry)
  of "ShTr": sub = tear(r, shape, b)
  of "ShCr": sub = crescent(cx, cy, rx, ry)
  of "ShpD": sub = polygon([pt(cx, y0), pt(x1, cy), pt(cx, y1), pt(x0, cy)]).get
  of "ShSg": sub = segment(r, shape, cx, cy, rx, ry)
  else:
    r.warn("an unrecognized parametric shape (imported as its bounding ellipse)")
    sub = ellipse(cx, cy, rx, ry)
  var path = AfPath(subpaths: @[sub])
  if $class == "ShCg":
    let hole = fclamp(numOr(r, shape, "Hole", 0.3), 0.0, 0.9)
    if hole > 0.0: path.subpaths.add ellipse(cx, cy, rx * hole, ry * hole)
    return some((path, true))
  some((path, false))

proc unit(src, dst: AfPoint): (AfPoint, float64) =
  let (dx, dy) = (dst.x - src.x, dst.y - src.y)
  let l = sqrt(dx * dx + dy * dy)
  if l > 0.0: (pt(dx / l, dy / l), l) else: (pt(0.0, 0.0), 0.0)

proc fillet*(pts: openArray[(AfPoint, float64)]; closed: bool): AfSubPath =
  ## A polyline with circular corners of the given radii (Affinity's live corners on curves).
  let n = pts.len
  template get(i: int; ok: var bool): AfPoint =
    var q = AfPoint()
    ok = false
    if closed:
      if n > 0:
        q = pts[floorMod(i, n)][0]
        ok = true
    elif i >= 0 and i < n:
      q = pts[i][0]
      ok = true
    q
  var corners = newSeqOfCap[(AfPoint, AfPoint, AfPoint, AfPoint)](n)
  for i in 0 ..< n:
    let (p, radius) = pts[i]
    var okA, okB: bool
    let a = get(i - 1, okA)
    let b = get(i + 1, okB)
    if not (okA and okB):
      corners.add (p, p, p, p)
      continue
    let (u1, l1) = unit(p, a)
    let (u2, l2) = unit(p, b)
    let cs = clamp(u1.x * u2.x + u1.y * u2.y, -1.0, 1.0)
    let phi = arccos(cs)
    if radius <= 0.0 or phi < 1e-6 or (PI - phi) < 1e-6:
      corners.add (p, p, p, p)
      continue
    let t = rmin(rmin(radius / tan(phi / 2.0), l1 / 2.0), l2 / 2.0)
    let r = t * tan(phi / 2.0)
    let k = 4.0 / 3.0 * tan((PI - phi) / 4.0) * r
    let s = pt(p.x + u1.x * t, p.y + u1.y * t)
    let e = pt(p.x + u2.x * t, p.y + u2.y * t)
    corners.add (s, pt(s.x - u1.x * k, s.y - u1.y * k), pt(e.x - u2.x * k, e.y - u2.y * k), e)
  if corners.len == 0: return AfSubPath(start: pt(0.0, 0.0), closed: closed)
  var cur = corners[0][0]
  let start = cur
  result = AfSubPath(start: start, closed: closed)
  for i, (s, c1, c2, e) in corners:
    if i > 0: result.segments.add [cur, s, s]
    if s != e: result.segments.add [c1, c2, e]
    cur = e
  if closed and cur != start: result.segments.add [cur, start, start]
