## Edit › Perspective Warp (port of `perspective.rs`): planes (quads) drawn over the image in
## Layout mode, whose corners are then moved in Warp mode; the image follows a piecewise-homography
## map.
##
## Each plane maps its source quad onto its destination quad with a homography
## `M_i = H_dst ∘ H_src⁻¹` (through the unit square, so `(u, v)` are the plane's projective
## coordinates). For every edge shared by two planes the average of both planes' images of each
## edge point is the target and the difference is added, faded linearly across the plane, so the
## map is exactly continuous across shared edges, exactly the homography for a lone plane, and the
## identity when nothing moved. Points outside every plane use the nearest plane. Corners that
## coincide (within half a pixel) in the source are linked: they share one destination.

import std/[options, strutils, math]
import ../rmath
import ./transform

type
  Quad* = array[4, array[2, float64]]
  Plane* = object
    ## One plane: source quad (Layout mode) and destination quad (Warp mode), corners clockwise
    ## from top-left, document pixels.
    src*, dst*: Quad

  PlaneMap = object
    src, srcInv, dst: Homography
    quad: Quad
    shared: array[4, int] ## for each edge: the neighbouring plane sharing it, or -1

  PerspectiveMap* = object
    ## A prepared perspective warp (forward map, source → destination).
    planes: seq[PlaneMap]

  Straighten* = enum
    ## Straighten modes (Photoshop's Warp-mode buttons).
    stHorizontal, stVertical, stAuto

const
  LinkEps = 0.5
  Unit: Quad = [[0.0, 0.0], [1.0, 0.0], [1.0, 1.0], [0.0, 1.0]]

proc identityPlane*(q: Quad): Plane = Plane(src: q, dst: q)

proc unitToQuad(q: Quad): Option[Homography] = rectToQuad([0.0, 0.0, 1.0, 1.0], q)

proc linkedCorners*(planes: openArray[Plane]): seq[seq[(int, int)]] =
  ## Groups of linked corners: `(plane, corner)` pairs that coincide in the source.
  for pi, p in planes:
    for ci, c in p.src:
      var found = -1
      for gi, g in result:
        let (a, b) = g[0]
        let o = planes[a].src[b]
        if abs(o[0] - c[0]) <= LinkEps and abs(o[1] - c[1]) <= LinkEps:
          found = gi
          break
      if found >= 0: result[found].add (pi, int(ci))
      else: result.add @[(pi, int(ci))]

proc unify*(planes: var seq[Plane]) =
  ## Snaps linked corners to one source and one destination position (their means).
  for g in linkedCorners(planes):
    if g.len < 2: continue
    let n = float64(g.len)
    var s = [0.0, 0.0]
    var d = [0.0, 0.0]
    for (p, c) in g:
      s = [s[0] + planes[p].src[c][0] / n, s[1] + planes[p].src[c][1] / n]
      d = [d[0] + planes[p].dst[c][0] / n, d[1] + planes[p].dst[c][1] / n]
    for (p, c) in g:
      planes[p].src[c] = s
      planes[p].dst[c] = d

proc edgeUv(k: int; u, v: float64): (array[2, float64], float64) =
  ## Edge `k` of a plane runs from corner `k` to corner `k + 1`: (point on the edge in plane
  ## coordinates, weight of that edge's correction).
  case k
  of 0: ([u, 0.0], 1.0 - v)
  of 1: ([1.0, v], u)
  of 2: ([u, 1.0], v)
  else: ([0.0, v], 1.0 - u)

proc parseStraighten*(s: string): Option[Straighten] =
  case s.toLowerAscii
  of "h", "horizontal", "level": some(stHorizontal)
  of "v", "vertical": some(stVertical)
  of "auto", "both": some(stAuto)
  else: none(Straighten)

proc straighten*(planes: var seq[Plane]; mode: Straighten) =
  ## Makes the destination edges that are near-horizontal exactly horizontal and/or near-vertical
  ## exactly vertical (within 45°), moving linked corners together.
  unify(planes)
  let groups = linkedCorners(planes)
  proc groupOf(p, c: int): int =
    for gi, g in groups:
      if (p, c) in g: return gi
    0
  var pos = newSeq[array[2, float64]](groups.len)
  for gi, g in groups: pos[gi] = planes[g[0][0]].dst[g[0][1]]
  var edges: seq[(int, int)]
  for p in 0 ..< planes.len:
    for k in 0 .. 3: edges.add (groupOf(p, k), groupOf(p, (k + 1) mod 4))
  for (a, b) in edges:
    let (pa, pb) = (pos[a], pos[b])
    let (dx, dy) = (abs(pb[0] - pa[0]), abs(pb[1] - pa[1]))
    if dy <= dx and mode in {stHorizontal, stAuto}:
      let y = (pa[1] + pb[1]) / 2.0
      pos[a][1] = y
      pos[b][1] = y
    elif dx < dy and mode in {stVertical, stAuto}:
      let x = (pa[0] + pb[0]) / 2.0
      pos[a][0] = x
      pos[b][0] = x
  for gi, g in groups:
    for (pi, ci) in g: planes[pi].dst[ci] = pos[gi]

proc newPerspectiveMap*(planes0: openArray[Plane]): Option[PerspectiveMap] =
  ## Prepares the map. None if a quad is degenerate (or there are no planes).
  if planes0.len == 0: return none(PerspectiveMap)
  var planes = @planes0
  unify(planes)
  proc near(a, b: array[2, float64]): bool = abs(a[0] - b[0]) <= LinkEps and abs(a[1] - b[1]) <= LinkEps
  var m: PerspectiveMap
  for i, p in planes:
    let src = unitToQuad(p.src)
    if src.isNone: return none(PerspectiveMap)
    let srcInv = src.get.inverse
    if srcInv.isNone: return none(PerspectiveMap)
    let dst = unitToQuad(p.dst)
    if dst.isNone: return none(PerspectiveMap)
    var shared = [-1, -1, -1, -1]
    for k in 0 .. 3:
      let (a, b) = (p.src[k], p.src[(k + 1) mod 4])
      for j, o in planes:
        if j == i: continue
        var hit = false
        for mm in 0 .. 3:
          let (c, d) = (o.src[mm], o.src[(mm + 1) mod 4])
          if (near(a, c) and near(b, d)) or (near(a, d) and near(b, c)):
            hit = true
            break
        if hit:
          shared[k] = j
          break
    m.planes.add PlaneMap(src: src.get, srcInv: srcInv.get, dst: dst.get, quad: p.src, shared: shared)
  some(m)

proc distToQuad(q: Quad; p: array[2, float64]): float64 =
  result = F64Max
  for k in 0 .. 3:
    let (a, b) = (q[k], q[(k + 1) mod 4])
    let (dx, dy) = (b[0] - a[0], b[1] - a[1])
    let l2 = dx * dx + dy * dy
    let t = (if l2 > 0: clamp(((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / l2, 0.0, 1.0) else: 0.0)
    let ex = a[0] + t * dx - p[0]
    let ey = a[1] + t * dy - p[1]
    let d = sqrt(ex * ex + ey * ey)
    # `f64::min` ignores NaN.
    if not (d != d) and d < result: result = d

proc locate(m: PerspectiveMap; x, y: float64): (int, float64, float64) =
  ## The plane containing `p` (or the nearest one) and `p`'s plane coordinates.
  var best = (0, F64Max, 0.0, 0.0)
  for i, pm in m.planes:
    let (u, v) = pm.srcInv.apply(x, y)
    if u >= -1e-9 and u <= 1.0 + 1e-9 and v >= -1e-9 and v <= 1.0 + 1e-9: return (i, u, v)
    let d = distToQuad(pm.quad, [x, y])
    if d < best[1]: best = (i, d, u, v)
  (best[0], best[2], best[3])

proc map*(m: PerspectiveMap; x, y: float64): (float64, float64) =
  ## Maps a source point to the destination.
  let (i, u, v) = m.locate(x, y)
  let pm = m.planes[i]
  var (ox, oy) = pm.dst.apply(u, v)
  if ox.classify in {fcNan, fcInf, fcNegInf} or oy.classify in {fcNan, fcInf, fcNegInf}: return (x, y)
  let (cu, cv) = (clamp(u, 0.0, 1.0), clamp(v, 0.0, 1.0))
  for k in 0 .. 3:
    let j = pm.shared[k]
    if j < 0: continue
    let (e, w) = edgeUv(k, cu, cv)
    if w <= 0: continue
    let mine = pm.dst.apply(e[0], e[1])
    let (qx, qy) = pm.src.apply(e[0], e[1])
    let other = m.planes[j]
    let (ou, ov) = other.srcInv.apply(qx, qy)
    let theirs = other.dst.apply(ou, ov)
    let target = ((mine[0] + theirs[0]) / 2.0, (mine[1] + theirs[1]) / 2.0)
    ox += w * (target[0] - mine[0])
    oy += w * (target[1] - mine[1])
  (ox, oy)

proc isIdentity*(planes: openArray[Plane]): bool =
  ## Whether every plane maps onto itself (identity).
  for p in planes:
    for k in 0 .. 3:
      if not (abs(p.src[k][0] - p.dst[k][0]) < 1e-9 and abs(p.src[k][1] - p.dst[k][1]) < 1e-9): return false
  true

proc unitCorners*(): Quad =
  ## Corners of the unit square in plane order (for callers drawing plane grids).
  Unit

proc planePoint*(p: Plane; u, v: float64; dst: bool): array[2, float64] =
  ## A point of plane `p` at plane coordinates `(u, v)`, in source space.
  let q = (if dst: p.dst else: p.src)
  let h = unitToQuad(q)
  if h.isNone: return q[0]
  let (x, y) = h.get.apply(u, v)
  [x, y]
