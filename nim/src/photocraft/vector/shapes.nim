## Live shapes as paths (rectangles with corner radii, ellipses, polygons and stars, lines) and
## non-affine maps of paths (Distort, Perspective, Warp) with adaptive subdivision.

import std/math
import ../doc

const Kappa* = 0.5522847498307934  ## cubic Bézier circle constant

proc smoothK(anchor, inc, outc: (float64, float64)): Knot =
  smoothKnot(point(anchor[0], anchor[1]), point(inc[0], inc[1]), point(outc[0], outc[1]))

proc rectPath*(x, y, w, h: float64): Path =
  newPath(@[polygon([(x, y), (x + w, y), (x + w, y + h), (x, y + h)])])

proc clampRadii*(w, h: float64; radii: array[4, float64]): array[4, float64] =
  var r: array[4, float64]
  for i in 0..3: r[i] = max(radii[i], 0)
  var f = 1.0
  for (a, b, side) in [(r[0], r[1], w), (r[1], r[2], h), (r[2], r[3], w), (r[3], r[0], h)]:
    if a + b > side and a + b > 0: f = min(f, side / (a + b))
  for i in 0..3: result[i] = r[i] * f

proc roundedRect*(x, y, w0, h0: float64; radii: array[4, float64]): Path =
  let (w, h) = (abs(w0), abs(h0))
  let rr = clampRadii(w, h, radii)
  let (tl, tr, br, bl) = (rr[0], rr[1], rr[2], rr[3])
  if tl <= 0 and tr <= 0 and br <= 0 and bl <= 0: return rectPath(x, y, w, h)
  let (x1, y1) = (x + w, y + h)
  var k: seq[Knot]
  proc pushArc(k: var seq[Knot]; start, e, cs, ce: (float64, float64); r: float64) =
    if r <= 0:
      k.add cornerKnot(start[0], start[1])
      return
    k.add Knot(anchor: point(start[0], start[1]), inCtrl: point(start[0], start[1]), outCtrl: point(cs[0], cs[1]))
    k.add Knot(anchor: point(e[0], e[1]), inCtrl: point(ce[0], ce[1]), outCtrl: point(e[0], e[1]))
  pushArc(k, (x1 - tr, y), (x1, y + tr), (x1 - tr + Kappa * tr, y), (x1, y + tr - Kappa * tr), tr)
  pushArc(k, (x1, y1 - br), (x1 - br, y1), (x1, y1 - br + Kappa * br), (x1 - br + Kappa * br, y1), br)
  pushArc(k, (x + bl, y1), (x, y1 - bl), (x + bl - Kappa * bl, y1), (x, y1 - bl + Kappa * bl), bl)
  pushArc(k, (x, y + tl), (x + tl, y), (x, y + tl - Kappa * tl), (x + tl - Kappa * tl, y), tl)
  # rotate left by n-1 = rotate right by one
  let lastK = k[^1]
  k.delete(k.high)
  k.insert(lastK, 0)
  var o: seq[Knot]
  for kn in k:
    if o.len > 0 and abs(o[^1].anchor.x - kn.anchor.x) < 1e-12 and abs(o[^1].anchor.y - kn.anchor.y) < 1e-12:
      o[^1].outCtrl = kn.outCtrl
      continue
    o.add kn
  if o.len > 1:
    let (f, l) = (o[0], o[^1])
    if abs(f.anchor.x - l.anchor.x) < 1e-12 and abs(f.anchor.y - l.anchor.y) < 1e-12:
      o[0].inCtrl = l.inCtrl
      o.setLen(o.len - 1)
  newPath(@[Subpath(closed: true, knots: o)])

proc ellipsePath*(x, y, w, h: float64): Path =
  let (rx, ry) = (w / 2, h / 2)
  let (cx, cy) = (x + rx, y + ry)
  let (kx, ky) = (rx * Kappa, ry * Kappa)
  newPath(@[Subpath(closed: true, knots: @[
    smoothK((cx, cy - ry), (cx - kx, cy - ry), (cx + kx, cy - ry)),
    smoothK((cx + rx, cy), (cx + rx, cy - ky), (cx + rx, cy + ky)),
    smoothK((cx, cy + ry), (cx + kx, cy + ry), (cx - kx, cy + ry)),
    smoothK((cx - rx, cy), (cx - rx, cy + ky), (cx - rx, cy - ky))])])

proc polygonPath*(x, y, w, h: float64; sides0: uint32; starRatio: float64): Path =
  let sides = int(clamp(sides0, 3'u32, 100'u32))
  let (rx, ry) = (w / 2, h / 2)
  let (cx, cy) = (x + rx, y + ry)
  let star = starRatio > 0 and starRatio < 1
  let count = if star: sides * 2 else: sides
  var pts: seq[(float64, float64)]
  for i in 0 ..< count:
    let a = -PI / 2 + TAU * float64(i) / float64(count)
    let k = if star and i mod 2 == 1: starRatio else: 1.0
    pts.add (cx + rx * k * cos(a), cy + ry * k * sin(a))
  newPath(@[polygon(pts)])

proc linePath*(a, b: (float64, float64); weight: float64): Path =
  let (dx, dy) = (b[0] - a[0], b[1] - a[1])
  let l = hypot(dx, dy)
  let hw = max(weight, 0) / 2
  let (nx, ny) = if l > 0: (-dy / l * hw, dx / l * hw) else: (0.0, hw)
  newPath(@[polygon([(a[0] + nx, a[1] + ny), (b[0] + nx, b[1] + ny), (b[0] - nx, b[1] - ny), (a[0] - nx, a[1] - ny)])])

proc livePath*(s: LiveShape): Path =
  case s.kind
  of lsRect: roundedRect(s.rect[0], s.rect[1], s.rect[2], s.rect[3], s.radii)
  of lsEllipse: ellipsePath(s.rect[0], s.rect[1], s.rect[2], s.rect[3])
  of lsPolygon: polygonPath(s.rect[0], s.rect[1], s.rect[2], s.rect[3], s.sides, s.starRatio)
  of lsLine: linePath((s.`from`[0], s.`from`[1]), (s.to[0], s.to[1]), s.weight)

# ---------------------------------------------------------------- maps

type PointMap* = proc (p: Point): Point

proc mapPoints*(path: Path; f: PointMap): Path =
  result = path
  for s in result.subpaths.mitems:
    for k in s.knots.mitems:
      k = Knot(anchor: f(k.anchor), inCtrl: f(k.inCtrl), outCtrl: f(k.outCtrl), smooth: k.smooth)

proc lerp(a, b: Point; t: float64): Point = point(a.x + (b.x - a.x) * t, a.y + (b.y - a.y) * t)
proc dist(a, b: Point): float64 = hypot(a.x - b.x, a.y - b.y)
proc evalSeg(s: array[4, Point]; t: float64): Point =
  let (ab, bc, cd) = (lerp(s[0], s[1], t), lerp(s[1], s[2], t), lerp(s[2], s[3], t))
  lerp(lerp(ab, bc, t), lerp(bc, cd, t), t)
proc halves(s: array[4, Point]): (array[4, Point], array[4, Point]) =
  let (ab, bc, cd) = (lerp(s[0], s[1], 0.5), lerp(s[1], s[2], 0.5), lerp(s[2], s[3], 0.5))
  let (abc, bcd) = (lerp(ab, bc, 0.5), lerp(bc, cd, 0.5))
  let m = lerp(abc, bcd, 0.5)
  ([s[0], ab, abc, m], [m, bcd, cd, s[3]])
proc offLine(p, a, b: Point): float64 =
  let l = dist(a, b)
  if l < 1e-12: dist(p, a) else: abs((b.x - a.x) * (a.y - p.y) - (a.x - p.x) * (b.y - a.y)) / l

const Probes = [0.25, 0.5, 0.75]

proc splitMapped(seg: array[4, Point]; f: PointMap; tol: float64; depth: int; o: var seq[array[4, Point]]) =
  let mapped = [f(seg[0]), f(seg[1]), f(seg[2]), f(seg[3])]
  var fits = true
  for t in Probes:
    if dist(f(evalSeg(seg, t)), evalSeg(mapped, t)) > tol: fits = false
  if fits or depth == 0:
    o.add mapped
    return
  let (a, b) = halves(seg)
  splitMapped(a, f, tol, depth - 1, o)
  splitMapped(b, f, tol, depth - 1, o)

proc mapSegment(seg: array[4, Point]; f: PointMap; tol: float64; depth: int): seq[array[4, Point]] =
  if seg[1] == seg[0] and seg[2] == seg[3]:
    let (a, b) = (f(seg[0]), f(seg[3]))
    var straight = true
    for t in Probes:
      if offLine(f(lerp(seg[0], seg[3], t)), a, b) > tol: straight = false
    if straight: return @[[a, a, b, b]]
    let line = [seg[0], lerp(seg[0], seg[3], 1 / 3), lerp(seg[0], seg[3], 2 / 3), seg[3]]
    splitMapped(line, f, tol, depth, result)
    return
  splitMapped(seg, f, tol, depth, result)

proc mapPath*(path: Path; f: PointMap; tol0: float64): Path =
  ## Maps a path through a non-affine point map, subdividing curves until they fit `tol`.
  let tol = if tol0 == tol0 and tol0 > 0 and tol0 != Inf: tol0 else: 0.1
  var segments = 0
  for s in path.subpaths: segments += s.knots.len
  var depth = 6
  while depth > 0 and segments * (1 shl depth) > (1 shl 20): dec depth
  result = path
  result.subpaths.setLen(0)
  for s in path.subpaths:
    let segs = s.segments
    if segs.len == 0:
      result.subpaths.add mapPoints(newPath(@[s]), f).subpaths[0]
      continue
    var pieces: seq[seq[array[4, Point]]]
    for seg in segs: pieces.add mapSegment(seg, f, tol, depth)
    var knots: seq[Knot]
    let n = s.knots.len
    for i, k in s.knots:
      let inc = if i > 0: i - 1 elif s.closed: n - 1 else: -1
      let inCtrl = if inc >= 0 and inc < pieces.len and pieces[inc].len > 0: pieces[inc][^1][2] else: f(k.inCtrl)
      let outCtrl = if i < pieces.len and pieces[i].len > 0: pieces[i][0][1] else: f(k.outCtrl)
      knots.add Knot(anchor: f(k.anchor), inCtrl: inCtrl, outCtrl: outCtrl, smooth: k.smooth)
      if i < pieces.len:
        let p = pieces[i]
        for w in 0 ..< p.len - 1:
          knots.add Knot(anchor: p[w + 1][0], inCtrl: p[w][2], outCtrl: p[w + 1][1], smooth: true)
    result.subpaths.add Subpath(closed: s.closed, knots: knots, op: s.op)
