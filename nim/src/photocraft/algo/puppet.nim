## Edit › Puppet Warp (port of `puppet.rs`): pins on a triangle mesh over the layer's opaque
## region, deformed as-rigidly-as-possible.
##
## * **Mesh.** The layer's coverage (alpha above a threshold) becomes a signed distance field,
##   offset by `expansion` to grow or shrink the region. Its outline is traced, simplified and
##   densified; a triangular lattice fills the inside; the mesh is the Delaunay triangulation of
##   those points with the outline's sides recovered, minus the triangles outside.
## * **Deformation.** The per-triangle ARAP energy of Liu et al. 2008 (the triangle form of
##   Sorkine & Alexa 2007) with cotangent weights, minimized by alternating a local step (best
##   rotation per triangle) and a global step (one sparse SPD solve per axis: a banded Cholesky
##   factor, or Jacobi-preconditioned conjugate gradients). Pins are stiff soft constraints on
##   the barycentric point under them. Modes change the local step: Rigid fits rotations,
##   Distort fits similarities (ASAP), Normal fits rotation × √scale. A pin's rotation fixes `R`
##   on the triangles around it. Mesh components without a pin stay where they are.
## * **Rendering.** Triangles are rasterized with `warp.warpTriangles`, back to front by the
##   depth of their nearest pin.

import std/[math, options, algorithm, tables, sets, json, strutils]
import ../[geom, raster, color]
import ./[util, transform, warp]

type
  PuppetMode* = enum
    pmRigid = "rigid", pmNormal = "normal", pmDistort = "distort"

  PuppetDensity* = enum
    pdFewer = "fewer", pdNormal = "normal", pdMore = "more"

  PuppetPin* = object
    ## A pin: where it was placed (`src`) and where it is now (`dst`), document pixels.
    src*, dst*: array[2, float64]
    rotate*: Option[float64] ## fixed rotation of the mesh around the pin (degrees); none = automatic
    depth*: int32            ## where the mesh overlaps itself, triangles nearest a higher pin draw on top

  PuppetWarp* = object
    ## A complete puppet warp (the command's params and a smart filter's stored data).
    pins*: seq[PuppetPin]
    mode*: PuppetMode
    density*: PuppetDensity
    expansion*: float64 ## grows (or with a negative value shrinks) the mesh beyond the opaque region, px

  PuppetMesh* = object
    ## A triangle mesh in document pixels.
    verts*: seq[array[2, float64]]
    tris*: seq[array[3, int]]
    spacing*: float64 ## grid spacing (px)

const
  AlphaMin = 0.05'f32       ## coverage threshold
  FieldBlocks = 1024.0      ## the outline field's longest side, in blocks
  MaxOutlinePoints = 20_000 ## most outline points the mesh takes
  NoneIdx = high(int)
  Iterations* = 40          ## local/global rounds used by the command
  MaxWarpPixels* = float64(1'u64 shl 30) ## the largest area (px) a puppet warp's result may span

proc parsePuppetMode*(s: string): Option[PuppetMode] =
  case s.toLowerAscii
  of "rigid": some(pmRigid)
  of "normal": some(pmNormal)
  of "distort": some(pmDistort)
  else: none(PuppetMode)

proc parsePuppetDensity*(s: string): Option[PuppetDensity] =
  case s.toLowerAscii
  of "fewer", "fewerpoints": some(pdFewer)
  of "normal": some(pdNormal)
  of "more", "morepoints": some(pdMore)
  else: none(PuppetDensity)

proc cells(d: PuppetDensity): float64 =
  case d
  of pdFewer: 12.0
  of pdNormal: 20.0
  of pdMore: 32.0

proc defaultPuppetWarp*(): PuppetWarp = PuppetWarp(mode: pmNormal, density: pdNormal, expansion: 2.0)

proc isIdentity*(w: PuppetWarp): bool =
  for p in w.pins:
    if p.src != p.dst: return false
    if p.rotate.isSome and remEuclidF(p.rotate.get, 360.0) != 0.0: return false
  true

proc dist2(a, b: array[2, float64]): float64 = (a[0] - b[0]) * (a[0] - b[0]) + (a[1] - b[1]) * (a[1] - b[1])

# ---------------------------------------------------------------- the outline field

type Field = object
  ## The puppet's region as a signed distance field over a block grid: negative inside the opaque
  ## area grown (or shrunk) by the expansion, positive outside, in px.
  x0, y0: float64
  g: float64 ## block size (px)
  bw, bh: int
  sd: seq[float32]
  expansion: float64

proc edt1d(f: openArray[float64]; o: var openArray[float64]; v: var seq[int]; z: var seq[float64]) =
  ## One-dimensional squared distance transform of the sampled function `f`.
  let n = f.len
  if n == 0: return
  var k = 0
  v[0] = 0
  z[0] = NegInf
  z[1] = Inf
  template meet(q, p: int): float64 =
    ((f[q] + float64(q) * float64(q)) - (f[p] + float64(p) * float64(p))) / (2.0 * (float64(q) - float64(p)))
  for q in 1 ..< n:
    var s = meet(q, v[k])
    while k > 0 and s <= z[k]:
      dec k
      s = meet(q, v[k])
    inc k
    v[k] = q
    z[k] = s
    z[k + 1] = Inf
  k = 0
  for q in 0 ..< n:
    let qf = float64(q)
    while z[k + 1] < qf: inc k
    let p = v[k]
    o[q] = (qf - float64(p)) * (qf - float64(p)) + f[p]

proc edt(mask: seq[bool]; site: bool; w, h: int): seq[float64] =
  ## Squared Euclidean distance (in blocks) from every block to the nearest block whose `mask`
  ## equals `site` (Felzenszwalb & Huttenlocher). About 1e20 when there is none.
  const Far = 1e20
  result = newSeq[float64](mask.len)
  for i, m in mask: result[i] = (if m == site: 0.0 else: Far)
  let m = max(w, h)
  var f = newSeq[float64](m)
  var o = newSeq[float64](m)
  var v = newSeq[int](m)
  var z = newSeq[float64](m + 1)
  for x in 0 ..< w:
    for y in 0 ..< h: f[y] = result[y * w + x]
    edt1d(f.toOpenArray(0, h - 1), o.toOpenArray(0, h - 1), v, z)
    for y in 0 ..< h: result[y * w + x] = o[y]
  for y in 0 ..< h:
    for x in 0 ..< w: f[x] = result[y * w + x]
    edt1d(f.toOpenArray(0, w - 1), result.toOpenArray(y * w, y * w + w - 1), v, z)

proc newField(src: Surface; bounds: Rect; expansion: float64): Field =
  let long = float64(max(bounds.width, bounds.height))
  let grow = ceil(rmax(expansion, 0.0))
  let g = asI32(rmax(ceil((long + 2.0 * grow + 8.0) / FieldBlocks), 1.0))
  let area = bounds.inflate(asI32(grow) + 2 * g)
  let bw = max((area.width + g - 1) div g, 1)
  let bh = max((area.height + g - 1) div g, 1)
  var cov = newSeq[bool](bw * bh)
  let fmt = src.format
  let n = fmt.channels
  if not fmt.alpha:
    for i in 0 ..< cov.len:
      let (x, y) = (area.x0 + (i mod bw) * g, area.y0 + (i div bw) * g)
      cov[i] = not rect(x, y, x + g, y + g).intersect(bounds).isEmpty
  else:
    for by in 0 ..< bh:
      let r = rect(area.x0, area.y0 + by * g, area.x0 + bw * g, area.y0 + (by + 1) * g).intersect(bounds)
      if r.isEmpty or not src.hasTilesIn(r): continue
      let row = src.readRegion(r)
      let rw = r.width
      for i in 0 ..< row.len div n:
        if row[i * n + n - 1] > AlphaMin:
          let x = r.x0 + i mod rw
          let bx = (x - area.x0) div g
          if bx >= 0 and bx < bw: cov[by * bw + bx] = true
  let outside = edt(cov, true, bw, bh)
  let inside = edt(cov, false, bw, bh)
  let gf = float64(g)
  var sd = newSeq[float32](cov.len)
  for i, c in cov: sd[i] = (if c: float32(-(sqrt(inside[i]) - 0.5) * gf) else: float32((sqrt(outside[i]) - 0.5) * gf))
  Field(x0: float64(area.x0), y0: float64(area.y0), g: gf, bw: bw, bh: bh, sd: sd, expansion: expansion)

proc at(f: Field; bx, by: int): float64 =
  ## The field at a block centre (exact, no interpolation).
  let i = by * f.bw + bx
  if i >= 0 and i < f.sd.len: float64(f.sd[i]) - f.expansion else: F64Max

proc value(f: Field; p: array[2, float64]): float64 =
  ## The field at `p` (bilinear between block centres; clamped to the grid).
  let fx = clamp((p[0] - f.x0) / f.g - 0.5, 0.0, float64(f.bw - 1))
  let fy = clamp((p[1] - f.y0) / f.g - 0.5, 0.0, float64(f.bh - 1))
  let (ix, iy) = (asUsize(floor(fx)), asUsize(floor(fy)))
  let (ix1, iy1) = (min(ix + 1, f.bw - 1), min(iy + 1, f.bh - 1))
  let (tx, ty) = (fx - float64(ix), fy - float64(iy))
  let top = f.at(ix, iy) * (1.0 - tx) + f.at(ix1, iy) * tx
  let bottom = f.at(ix, iy1) * (1.0 - tx) + f.at(ix1, iy1) * tx
  top * (1.0 - ty) + bottom * ty

proc outline(f: Field): seq[seq[array[2, float64]]] =
  ## The outline (where the field crosses zero) as closed loops, traced by marching squares over
  ## the block centres. The field's border blocks lie outside, so every loop closes.
  let (bw, bh) = (f.bw, f.bh)
  if bw < 2 or bh < 2: return @[]
  template inside(x, y: int): bool = f.at(x, y) < 0.0
  # A crossing is named by its block edge: 2·block for the edge to the right neighbour,
  # 2·block + 1 for the edge to the one below.
  template id(x, y: int; down: bool): int = 2 * (y * bw + x) + int(down)
  proc point(f: Field; id: int): array[2, float64] =
    let b = id div 2
    let (x, y) = (b mod f.bw, b div f.bw)
    let (x2, y2) = (if id mod 2 == 0: (x + 1, y) else: (x, y + 1))
    let (fa, fb) = (f.at(x, y), f.at(x2, y2))
    let t = (if abs(fa - fb) > 1e-12: clamp(fa / (fa - fb), 0.0, 1.0) else: 0.5)
    let (dx, dy) = (float64(x2 - x), float64(y2 - y))
    [f.x0 + (float64(x) + 0.5 + dx * t) * f.g, f.y0 + (float64(y) + 0.5 + dy * t) * f.g]
  var links = initTable[int, array[2, int]]()
  proc link(links: var Table[int, array[2, int]]; a, b: int) =
    for (p, q) in [(a, b), (b, a)]:
      var e = links.getOrDefault(p, [NoneIdx, NoneIdx])
      if e[0] == NoneIdx: e[0] = q else: e[1] = q
      links[p] = e
  for y in 0 ..< bh - 1:
    for x in 0 ..< bw - 1:
      let c = [inside(x, y), inside(x + 1, y), inside(x + 1, y + 1), inside(x, y + 1)]
      if c[1] == c[0] and c[2] == c[0] and c[3] == c[0]: continue
      # Top, right, bottom, left.
      let edges = [id(x, y, false), id(x + 1, y, true), id(x, y + 1, false), id(x, y, true)]
      let cross = [c[0] != c[1], c[1] != c[2], c[3] != c[2], c[0] != c[3]]
      var ks: seq[int]
      for k in 0 .. 3:
        if cross[k]: ks.add k
      if ks.len == 2: link(links, edges[ks[0]], edges[ks[1]])
      elif ks.len == 4:
        # A saddle: the centre decides which diagonal pair is joined.
        let centre = f.at(x, y) + f.at(x + 1, y) + f.at(x + 1, y + 1) + f.at(x, y + 1) < 0.0
        if c[0] == centre:
          link(links, edges[0], edges[1])
          link(links, edges[2], edges[3])
        else:
          link(links, edges[3], edges[0])
          link(links, edges[1], edges[2])
  var keys: seq[int]
  for k in links.keys: keys.add k
  keys.sort()
  var seen = initHashSet[int]()
  for start in keys:
    if start in seen: continue
    var lp: seq[array[2, float64]]
    var (prev, cur) = (NoneIdx, start)
    while true:
      seen.incl cur
      lp.add f.point(cur)
      if cur notin links: break
      let ab = links[cur]
      let next = (if ab[0] != prev: ab[0] else: ab[1])
      if next == NoneIdx or next in seen: break
      (prev, cur) = (cur, next)
    if lp.len >= 3: result.add lp

# ---------------------------------------------------------------- outline helpers

proc segDist(p, a, b: array[2, float64]): float64 =
  ## Distance from `p` to the segment `a`–`b`.
  let (dx, dy) = (b[0] - a[0], b[1] - a[1])
  let l2 = dx * dx + dy * dy
  let t = (if l2 > 0.0: clamp(((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / l2, 0.0, 1.0) else: 0.0)
  sqrt(dist2(p, [a[0] + dx * t, a[1] + dy * t]))

proc simplifyLoop(lp: seq[array[2, float64]]; tol: float64): seq[array[2, float64]] =
  ## Simplifies a closed loop (Douglas–Peucker, within `tol` px), which keeps its corners.
  let n = lp.len
  if n < 4: return lp
  # `max_by` keeps the last of equal maxima.
  var far = 1
  for a in 2 ..< n:
    if totalCmp(dist2(lp[0], lp[a]), dist2(lp[0], lp[far])) >= 0: far = a
  var keep = newSeq[bool](n)
  keep[0] = true
  keep[far] = true
  # Spans (a, b) of the loop; b == n stands for point 0.
  var stack = @[(0, far), (far, n)]
  while stack.len > 0:
    let (a, b) = stack.pop()
    let (pa, pb) = (lp[a], lp[b mod n])
    var best = (0.0, 0)
    for i in a + 1 ..< min(b, n):
      let d = segDist(lp[i], pa, pb)
      if d > best[0]: best = (d, i)
    if best[0] > tol:
      keep[best[1]] = true
      stack.add (a, best[1])
      stack.add (best[1], b)
  for i in 0 ..< n:
    if keep[i]: result.add lp[i]
  if result.len >= 3: return
  # A speck thinner than the tolerance: keep a few of its points.
  result.setLen(0)
  var i = 0
  while i < n:
    result.add lp[i]
    i += max(n div 4, 1)

proc lerp(a, b: array[2, float64]; t: float64): array[2, float64] = [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t]

proc densify(lp: seq[array[2, float64]]; step: float64): seq[array[2, float64]] =
  ## Splits the sides of a closed loop longer than `step` into equal parts.
  for k, a in lp:
    let b = lp[(k + 1) mod lp.len]
    let parts = asUsize(clamp(ceil(sqrt(dist2(a, b)) / step), 1.0, 1e4))
    for i in 0 ..< parts: result.add lerp(a, b, float64(i) / float64(parts))

proc orient(a, b, c: array[2, float64]): float64 = (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0])

proc inCircle(a, b, c, d: array[2, float64]): float64 =
  ## Positive when `d` lies inside the circumcircle of the positively oriented `a`, `b`, `c`.
  let (ax, ay) = (a[0] - d[0], a[1] - d[1])
  let (bx, by) = (b[0] - d[0], b[1] - d[1])
  let (cx, cy) = (c[0] - d[0], c[1] - d[1])
  (ax * ax + ay * ay) * (bx * cy - cx * by) - (bx * bx + by * by) * (ax * cy - cx * ay) + (cx * cx + cy * cy) * (ax * by - bx * ay)

# ---------------------------------------------------------------- Delaunay triangulation

type
  DTri = object
    ## Vertices (positively oriented) and the neighbour across the edge opposite each vertex.
    v, n: array[3, int]
    alive: bool

  Delaunay = object
    ## Incremental Delaunay triangulation (Bowyer–Watson), started from a triangle around the
    ## area. Vertices 0..3 are that triangle's.
    pts: seq[array[2, float64]]
    tris: seq[DTri]
    stamp: seq[uint32]
    pass: uint32
    last: int

proc newDelaunay(lo, hi: array[2, float64]): Delaunay =
  let (cx, cy) = (0.5 * (lo[0] + hi[0]), 0.5 * (lo[1] + hi[1]))
  let d = rmax(rmax(hi[0] - lo[0], hi[1] - lo[1]), 1.0) * 4.0
  var pts = @[[cx - 2.0 * d, cy - d], [cx + 2.0 * d, cy - d], [cx, cy + 2.0 * d]]
  if orient(pts[0], pts[1], pts[2]) < 0.0: swap(pts[1], pts[2])
  Delaunay(pts: pts, tris: @[DTri(v: [0, 1, 2], n: [NoneIdx, NoneIdx, NoneIdx], alive: true)], stamp: @[0'u32])

proc locate(dl: Delaunay; p: array[2, float64]): Option[int] =
  ## The triangle holding `p`: a visibility walk from the last one made.
  var t = -1
  if dl.last < dl.tris.len and dl.tris[dl.last].alive: t = dl.last
  else:
    for i, tri in dl.tris:
      if tri.alive:
        t = i
        break
    if t < 0: return none(int)
  for _ in 0 ..< dl.tris.len + 3:
    if t < 0 or t >= dl.tris.len: return none(int)
    let tri = dl.tris[t]
    var step = -1
    for k in 0 .. 2:
      if orient(dl.pts[tri.v[(k + 1) mod 3]], dl.pts[tri.v[(k + 2) mod 3]], p) < 0.0:
        step = k
        break
    if step < 0: return some(t)
    if tri.n[step] != NoneIdx: t = tri.n[step]
    else: return none(int)
  for i, tri in dl.tris:
    if not tri.alive: continue
    var ok = true
    for k in 0 .. 2:
      if not (orient(dl.pts[tri.v[(k + 1) mod 3]], dl.pts[tri.v[(k + 2) mod 3]], p) >= 0.0): ok = false
    if ok: return some(i)
  none(int)

proc insert(dl: var Delaunay; p: array[2, float64]): Option[int] =
  ## Inserts `p`; returns its vertex index (an existing vertex's when `p` coincides with it).
  let lt = dl.locate(p)
  if lt.isNone: return none(int)
  let t0 = lt.get
  for v in dl.tris[t0].v:
    if dist2(dl.pts[v], p) < 1e-12: return some(v)
  dl.pass = dl.pass + 1'u32
  let pass = dl.pass
  # The cavity: every triangle whose circumcircle holds `p`, grown from `t0`.
  var bad = @[t0]
  dl.stamp[t0] = pass
  var i = 0
  while i < bad.len:
    let t = bad[i]
    inc i
    for k in 0 .. 2:
      let nb = dl.tris[t].n[k]
      if nb == NoneIdx or dl.stamp[nb] == pass: continue
      let tv = dl.tris[nb].v
      if inCircle(dl.pts[tv[0]], dl.pts[tv[1]], dl.pts[tv[2]], p) > 0.0:
        dl.stamp[nb] = pass
        bad.add nb
  var rim: seq[(int, int, int)]
  for t in bad:
    let tri = dl.tris[t]
    for k in 0 .. 2:
      let nb = tri.n[k]
      if nb == NoneIdx or dl.stamp[nb] != pass: rim.add (tri.v[(k + 1) mod 3], tri.v[(k + 2) mod 3], nb)
    dl.tris[t].alive = false
  let pi = dl.pts.len
  dl.pts.add p
  let base = dl.tris.len
  for i, (a, b, o) in rim:
    dl.tris.add DTri(v: [a, b, pi], n: [NoneIdx, NoneIdx, o], alive: true)
    dl.stamp.add 0
    if o != NoneIdx and o < dl.tris.len:
      for k in 0 .. 2:
        let (x, y) = (dl.tris[o].v[(k + 1) mod 3], dl.tris[o].v[(k + 2) mod 3])
        if (x == b and y == a) or (x == a and y == b): dl.tris[o].n[k] = base + i
  for i, (a, b, _) in rim:
    var acrossB, acrossA = NoneIdx
    for j, r in rim:
      if r[0] == b:
        acrossB = base + j
        break
    for j, r in rim:
      if r[1] == a:
        acrossA = base + j
        break
    dl.tris[base + i].n[0] = acrossB
    dl.tris[base + i].n[1] = acrossA
  dl.last = base
  some(pi)

proc edges(dl: Delaunay): HashSet[(int, int)] =
  for t in dl.tris:
    if not t.alive: continue
    for k in 0 .. 2:
      let (a, b) = (t.v[k], t.v[(k + 1) mod 3])
      result.incl (min(a, b), max(a, b))

type Inside = object
  ## Even-odd point-in-polygon test against closed outline segments, bucketed by row.
  y0, row: float64
  rows: seq[seq[int]]

proc newInside(pts: seq[array[2, float64]]; segs: seq[(int, int)]; row: float64): Inside =
  var (y0, y1) = (F64Max, -F64Max)
  for (a, b) in segs:
    for y in [pts[a][1], pts[b][1]]:
      y0 = rmin(y0, y)
      y1 = rmax(y1, y)
  let n = (if y1 >= y0: asUsize(clamp(floor((y1 - y0) / row), 0.0, 1e5)) + 1 else: 0)
  var rows = newSeq[seq[int]](n)
  for i, (a, b) in segs:
    let (lo, hi) = (rmin(pts[a][1], pts[b][1]), rmax(pts[a][1], pts[b][1]))
    let (r0, r1) = (asUsize(floor((lo - y0) / row)), asUsize(floor((hi - y0) / row)))
    for r in r0 .. min(r1, n - 1): rows[r].add i
  Inside(y0: y0, row: row, rows: rows)

proc contains(ins: Inside; pts: seq[array[2, float64]]; segs: seq[(int, int)]; p: array[2, float64]): bool =
  if p[1] < ins.y0: return false
  let r = asUsize(floor((p[1] - ins.y0) / ins.row))
  if r >= ins.rows.len: return false
  for i in ins.rows[r]:
    let (a, b) = (pts[segs[i][0]], pts[segs[i][1]])
    if (a[1] > p[1]) != (b[1] > p[1]) and p[0] < a[0] + (p[1] - a[1]) / (b[1] - a[1]) * (b[0] - a[0]):
      result = not result

proc bandOrder(pos: seq[array[2, float64]]; tris: seq[array[3, int]]): seq[int] =
  ## The vertices `tris` use, in reverse Cuthill–McKee order (breadth first from the topmost vertex
  ## of each piece, lower degrees first), which keeps the solver's banded factor small.
  var adj = newSeq[seq[int]](pos.len)
  for t in tris:
    for k in 0 .. 2:
      let (a, b) = (t[k], t[(k + 1) mod 3])
      adj[a].add b
      adj[b].add a
  for a in adj.mitems:
    a.sort()
    var d: seq[int]
    for x in a:
      if d.len == 0 or d[^1] != x: d.add x
    a = d
  var starts: seq[int]
  for i in 0 ..< pos.len:
    if adj[i].len > 0: starts.add i
  starts.sort(proc(a, b: int): int =
    let c = totalCmp(pos[a][1], pos[b][1])
    if c != 0: c else: totalCmp(pos[a][0], pos[b][0]))
  var seen = newSeq[bool](pos.len)
  for s in starts:
    if seen[s]: continue
    seen[s] = true
    let first = result.len
    result.add s
    var i = first
    while i < result.len:
      let v = result[i]
      inc i
      var next: seq[int]
      for w in adj[v]:
        if not seen[w]: next.add w
      next.sort(proc(a, b: int): int = cmp(adj[a].len, adj[b].len))
      for w in next:
        seen[w] = true
        result.add w
  result.reverse()

proc buildMesh*(src: Surface; bounds: Rect; density: PuppetDensity; expansion: float64): PuppetMesh =
  ## Builds the mesh over the covered part of `src` inside `bounds`: the outline of the opaque
  ## region grown by `expansion`, simplified and split into sides no longer than the density's
  ## spacing, a triangular lattice inside, Delaunay-triangulated with the outline's sides
  ## recovered, minus the triangles outside the outline.
  if bounds.isEmpty: return PuppetMesh()
  let e = (if expansion == expansion and expansion - expansion == 0.0: clamp(expansion, -200.0, 200.0) else: 0.0)
  # The density counts steps across the region the mesh covers (grown by the expansion).
  let long = float64(max(bounds.width, bounds.height)) + 2.0 * rmax(e, 0.0)
  let spacing = rmax(long / density.cells, 2.0)
  let field = newField(src, bounds, e)
  let loops = field.outline
  let empty = PuppetMesh(spacing: spacing)
  if loops.len == 0: return empty
  var tol = rmax(0.5 * field.g, 0.5)
  var rings: seq[seq[array[2, float64]]]
  while true:
    rings.setLen(0)
    var total = 0
    for l in loops:
      rings.add densify(simplifyLoop(l, tol), spacing)
      total += rings[^1].len
    if total <= MaxOutlinePoints or tol > spacing: break
    tol *= 2.0
  let (lo, hi) = ([field.x0, field.y0], [field.x0 + float64(field.bw) * field.g, field.y0 + float64(field.bh) * field.g])
  var dl = newDelaunay(lo, hi)
  var segs: seq[(int, int)]
  for ring in rings:
    var idx: seq[int]
    for p in ring:
      let v = dl.insert(p)
      if v.isSome: idx.add v.get
    for k, a in idx:
      let b = idx[(k + 1) mod idx.len]
      if a != b: segs.add (a, b)
  # The inside: a triangular lattice, kept half a step clear of the outline.
  let rise = spacing * sqrt(3.0) / 2.0
  let rows = asUsize(ceil((hi[1] - lo[1]) / rise))
  let cols = asUsize(ceil((hi[0] - lo[0]) / spacing))
  for j in 0 .. rows:
    for i in 0 .. cols:
      let p = [lo[0] + (float64(i) + 0.5 * float64(j mod 2)) * spacing, lo[1] + float64(j) * rise]
      if field.value(p) < -0.5 * spacing: discard dl.insert(p)
  # Recover the outline's sides: split a side the triangulation lacks at its midpoint.
  var budget = segs.len * 2
  for _ in 0 ..< 8:
    let edges = dl.edges
    var next = newSeqOfCap[(int, int)](segs.len)
    var split = false
    for (a, b) in segs:
      if budget == 0 or (min(a, b), max(a, b)) in edges:
        next.add (a, b)
        continue
      let c = dl.insert(lerp(dl.pts[a], dl.pts[b], 0.5))
      if c.isSome and c.get != a and c.get != b:
        next.add (a, c.get)
        next.add (c.get, b)
        dec budget
        split = true
      else: next.add (a, b)
    segs = next
    if not split: break
  let inside = newInside(dl.pts, segs, spacing)
  var tris: seq[array[3, int]]
  for t in dl.tris:
    if not t.alive or t.v[0] < 3 or t.v[1] < 3 or t.v[2] < 3: continue
    let (a, b, c) = (dl.pts[t.v[0]], dl.pts[t.v[1]], dl.pts[t.v[2]])
    if orient(a, b, c) > 1e-9 and inside.contains(dl.pts, segs, [(a[0] + b[0] + c[0]) / 3.0, (a[1] + b[1] + c[1]) / 3.0]):
      tris.add t.v
  if tris.len == 0: return empty
  # Keep the vertices in use, numbered for a narrow band in the solver's matrix.
  let order = bandOrder(dl.pts, tris)
  var remap = newSeq[int](dl.pts.len)
  for r in remap.mitems: r = NoneIdx
  for nw, old in order: remap[old] = nw
  result = PuppetMesh(spacing: spacing)
  for i in order: result.verts.add dl.pts[i]
  for t in tris: result.tris.add [remap[t[0]], remap[t[1]], remap[t[2]]]

# ---------------------------------------------------------------- sparse solvers

type
  Csr = object
    ## A compressed sparse row matrix (symmetric, used by CG).
    rows, cols: seq[int]
    vals, diag: seq[float64]

  BandChol = object
    ## Banded Cholesky factor `A = L Lᵀ`; row `i` holds `L[i][i - p ..= i]`.
    n, p: int
    l: seq[float64]

proc csrFromTriplets(n: int; t0: seq[(int, int, float64)]): Csr =
  var t = t0
  t.sort(proc(a, b: (int, int, float64)): int = cmp((a[0], a[1]), (b[0], b[1])))
  var rows = newSeq[int](n + 1)
  var cols = newSeqOfCap[int](t.len)
  var vals = newSeqOfCap[float64](t.len)
  var last = (NoneIdx, NoneIdx)
  for (i, j, v) in t:
    if (i, j) == last:
      vals[^1] += v
      continue
    last = (i, j)
    cols.add j
    vals.add v
    rows[i + 1] = cols.len
  for i in 0 ..< n: rows[i + 1] = max(rows[i + 1], rows[i])
  var diag = newSeq[float64](n)
  for i in 0 ..< n:
    diag[i] = 1.0
    for k in rows[i] ..< rows[i + 1]:
      if cols[k] == i and vals[k] > 0.0: diag[i] = vals[k]
  Csr(rows: rows, cols: cols, vals: vals, diag: diag)

proc mul(a: Csr; x: openArray[float64]; o: var seq[float64]) =
  for i in 0 ..< o.len:
    var s = 0.0
    for k in a.rows[i] ..< a.rows[i + 1]: s += a.vals[k] * x[a.cols[k]]
    o[i] = s

proc solve(a: Csr; b: openArray[float64]; x: var seq[float64]) =
  ## Jacobi-preconditioned conjugate gradients, warm-started from `x`.
  let n = b.len
  var r = newSeq[float64](n)
  a.mul(x, r)
  for i in 0 ..< n: r[i] = b[i] - r[i]
  var bs = -0.0
  for v in b: bs += v * v
  let bn = rmax(sqrt(bs), 1e-30)
  var z = newSeq[float64](n)
  for i in 0 ..< n: z[i] = r[i] / a.diag[i]
  var p = z
  var rz = -0.0
  for i in 0 ..< n: rz += r[i] * z[i]
  var ap = newSeq[float64](n)
  for _ in 0 ..< clamp(4 * n, 50, 2000):
    var rr = -0.0
    for v in r: rr += v * v
    if sqrt(rr) <= 1e-12 * bn: break
    a.mul(p, ap)
    var pap = -0.0
    for i in 0 ..< n: pap += p[i] * ap[i]
    if abs(pap) < 1e-300: break
    let alpha = rz / pap
    for i in 0 ..< n:
      x[i] += alpha * p[i]
      r[i] -= alpha * ap[i]
    for i in 0 ..< n: z[i] = r[i] / a.diag[i]
    var rz2 = -0.0
    for i in 0 ..< n: rz2 += r[i] * z[i]
    let beta = rz2 / rz
    rz = rz2
    for i in 0 ..< n: p[i] = z[i] + beta * p[i]

proc newBandChol(a: Csr): Option[BandChol] =
  ## None when the band would be too large (or the matrix isn't positive definite).
  let n = a.diag.len
  var p = 0
  for i in 0 ..< n:
    for k in a.rows[i] ..< a.rows[i + 1]: p = max(p, abs(i - a.cols[k]))
  if (if n != 0 and p + 1 > high(int) div n: high(int) else: n * (p + 1)) > 40_000_000: return none(BandChol)
  let w = p + 1
  var l = newSeq[float64](n * w)
  for i in 0 ..< n:
    for k in a.rows[i] ..< a.rows[i + 1]:
      let j = a.cols[k]
      if j <= i: l[i * w + (j + p - i)] = a.vals[k]
  for i in 0 ..< n:
    let lo = max(i - p, 0)
    for j in lo .. i:
      let jlo = max(max(j - p, 0), lo)
      var s = l[i * w + (j + p - i)]
      for k in jlo ..< j: s -= l[i * w + (k + p - i)] * l[j * w + (k + p - j)]
      if i == j:
        if s <= 0.0 or not (s == s and s - s == 0.0): return none(BandChol)
        l[i * w + p] = sqrt(s)
      else:
        l[i * w + (j + p - i)] = s / l[j * w + p]
  some(BandChol(n: n, p: p, l: l))

proc solve(c: BandChol; b: openArray[float64]; x: var seq[float64]) =
  let (n, p, w) = (c.n, c.p, c.p + 1)
  var y = @b
  for i in 0 ..< n:
    var s = y[i]
    for k in max(i - p, 0) ..< i: s -= c.l[i * w + (k + p - i)] * y[k]
    y[i] = s / c.l[i * w + p]
  for i in countdown(n - 1, 0):
    var s = y[i]
    for k in i + 1 ..< min(i + p + 1, n): s -= c.l[k * w + (i + p - k)] * x[k]
    x[i] = s / c.l[i * w + p]

# ---------------------------------------------------------------- the ARAP solver

type
  Bound = object
    ## A pin bound to the mesh: barycentric weights on a triangle's vertices.
    verts: array[3, int]
    bary: array[3, float64]

  PuppetSolver* = object
    ## Prepared ARAP solver for one mesh and a set of pin source positions.
    mesh*: PuppetMesh
    w: seq[array[3, float64]] ## cotangent weight of each triangle's edge opposite corner k
    bound: seq[Bound]
    pinTris: seq[seq[int]]    ## triangles whose rotation a pin fixes
    anchored: seq[bool]       ## vertices of components without pins (held at rest)
    matrix: Csr
    chol: Option[BandChol]
    pinWeight: float64

proc cot(a, b, c: array[2, float64]): float64 =
  ## Cotangent of the angle at `a`.
  let (u, v) = ([b[0] - a[0], b[1] - a[1]], [c[0] - a[0], c[1] - a[1]])
  let cross = abs(u[0] * v[1] - u[1] * v[0])
  if cross < 1e-12: 0.0 else: (u[0] * v[0] + u[1] * v[1]) / cross

proc locateBound(mesh: PuppetMesh; p: array[2, float64]): Bound =
  for t in mesh.tris:
    let (a, b, c) = (mesh.verts[t[0]], mesh.verts[t[1]], mesh.verts[t[2]])
    let det = (b[0] - a[0]) * (c[1] - a[1]) - (c[0] - a[0]) * (b[1] - a[1])
    if abs(det) < 1e-12: continue
    let l1 = ((p[0] - a[0]) * (c[1] - a[1]) - (c[0] - a[0]) * (p[1] - a[1])) / det
    let l2 = ((b[0] - a[0]) * (p[1] - a[1]) - (p[0] - a[0]) * (b[1] - a[1])) / det
    let l0 = 1.0 - l1 - l2
    if l0 >= -1e-9 and l1 >= -1e-9 and l2 >= -1e-9: return Bound(verts: t, bary: [l0, l1, l2])
  # Outside the mesh: the nearest vertex.
  var i = 0
  for k in 1 ..< mesh.verts.len:
    if totalCmp(dist2(mesh.verts[k], p), dist2(mesh.verts[i], p)) < 0: i = k
  Bound(verts: [i, i, i], bary: [1.0, 0.0, 0.0])

proc newPuppetSolver*(mesh: PuppetMesh; pinSrc: openArray[array[2, float64]]; fixedRotation: openArray[bool]): PuppetSolver =
  ## Prepares the solver for pins placed at `pinSrc` (rotation-fixed pins listed in
  ## `fixedRotation`).
  let nv = mesh.verts.len
  var w = newSeq[array[3, float64]](mesh.tris.len)
  for i, t in mesh.tris:
    let (a, b, c) = (mesh.verts[t[0]], mesh.verts[t[1]], mesh.verts[t[2]])
    w[i] = [0.5 * cot(a, b, c), 0.5 * cot(b, c, a), 0.5 * cot(c, a, b)]
  # Without vertices there is nothing to bind a pin to; every pin loop zips `bound`.
  var bound: seq[Bound]
  if nv > 0:
    for p in pinSrc: bound.add locateBound(mesh, p)
  # Connected components (union-find over triangle edges).
  var parent = newSeq[int](nv)
  for i in 0 ..< nv: parent[i] = i
  proc find(p: var seq[int]; i0: int): int =
    var i = i0
    while p[i] != i:
      p[i] = p[p[i]]
      i = p[i]
    i
  for t in mesh.tris:
    for k in 1 .. 2:
      let (a, b) = (find(parent, t[0]), find(parent, t[k]))
      if a != b: parent[a] = b
  var pinnedRoot = newSeq[bool](nv)
  for b in bound:
    for v in b.verts: pinnedRoot[find(parent, v)] = true
  var anchored = newSeq[bool](nv)
  for v in 0 ..< nv: anchored[v] = not pinnedRoot[find(parent, v)]
  # Rotation-fixed pins: the triangles around the pin's nearest vertex.
  var pinTris = newSeq[seq[int]](bound.len)
  for k in 0 ..< min(bound.len, pinSrc.len):
    if not (k < fixedRotation.len and fixedRotation[k]): continue
    let b = bound[k]
    var v = b.verts[0]
    for q in 1 .. 2:
      if totalCmp(dist2(mesh.verts[b.verts[q]], pinSrc[k]), dist2(mesh.verts[v], pinSrc[k])) < 0: v = b.verts[q]
    for i, t in mesh.tris:
      if v in t: pinTris[k].add i
  var trip = newSeqOfCap[(int, int, float64)](mesh.tris.len * 12 + nv)
  var diagSum = 0.0
  for ti, t in mesh.tris:
    for k in 0 .. 2:
      let (i, j) = (t[(k + 1) mod 3], t[(k + 2) mod 3])
      let wk = w[ti][k]
      trip.add (i, i, wk)
      trip.add (j, j, wk)
      trip.add (i, j, -wk)
      trip.add (j, i, -wk)
      diagSum += 2.0 * wk
  let pinWeight = 1e4 * rmax(diagSum / float64(max(nv, 1)), 1.0)
  for b in bound:
    for a in 0 .. 2:
      for c in 0 .. 2: trip.add (b.verts[a], b.verts[c], pinWeight * b.bary[a] * b.bary[c])
  for v, an in anchored:
    if an: trip.add (v, v, pinWeight)
  let matrix = csrFromTriplets(nv, trip)
  PuppetSolver(mesh: mesh, w: w, bound: bound, pinTris: pinTris, anchored: anchored, matrix: matrix,
               chol: newBandChol(matrix), pinWeight: pinWeight)

proc energyWith(s: PuppetSolver; v: seq[array[2, float64]]; rots: seq[array[4, float64]]; pinDst: openArray[array[2, float64]]): float64 =
  ## The ARAP energy of `v` for the given per-triangle linear maps (plus the pin terms).
  let x = s.mesh.verts
  for ti, t in s.mesh.tris:
    let r = rots[ti]
    for k in 0 .. 2:
      let (i, j) = (t[(k + 1) mod 3], t[(k + 2) mod 3])
      let d = [v[i][0] - v[j][0], v[i][1] - v[j][1]]
      let ex = [x[i][0] - x[j][0], x[i][1] - x[j][1]]
      let re = [r[0] * ex[0] + r[1] * ex[1], r[2] * ex[0] + r[3] * ex[1]]
      result += s.w[ti][k] * ((d[0] - re[0]) * (d[0] - re[0]) + (d[1] - re[1]) * (d[1] - re[1]))
  for q in 0 ..< min(s.bound.len, pinDst.len):
    let b = s.bound[q]
    var p = [0.0, 0.0]
    for k in 0 .. 2: p = [p[0] + b.bary[k] * v[b.verts[k]][0], p[1] + b.bary[k] * v[b.verts[k]][1]]
    result += s.pinWeight * dist2(p, pinDst[q])

proc local(s: PuppetSolver; v: seq[array[2, float64]]; mode: PuppetMode; fixed: openArray[Option[float64]]): seq[array[4, float64]] =
  let x = s.mesh.verts
  result = newSeq[array[4, float64]](s.mesh.tris.len)
  for ti, t in s.mesh.tris:
    var sm = [0.0, 0.0, 0.0, 0.0]
    var norm = 0.0
    let wt = s.w[ti]
    for k in 0 .. 2:
      let (i, j) = (t[(k + 1) mod 3], t[(k + 2) mod 3])
      let d = [v[i][0] - v[j][0], v[i][1] - v[j][1]]
      let e = [x[i][0] - x[j][0], x[i][1] - x[j][1]]
      sm[0] += wt[k] * d[0] * e[0]
      sm[1] += wt[k] * d[0] * e[1]
      sm[2] += wt[k] * d[1] * e[0]
      sm[3] += wt[k] * d[1] * e[1]
      norm += wt[k] * (e[0] * e[0] + e[1] * e[1])
    let (c, sn) = (sm[0] + sm[3], sm[2] - sm[1])
    let th = arctan2(sn, c)
    var scale = (case mode
      of pmRigid: 1.0
      of pmNormal: sqrt(hypot(c, sn) / rmax(norm, 1e-12))
      of pmDistort: hypot(c, sn) / rmax(norm, 1e-12))
    if not (scale == scale and scale - scale == 0.0 and scale > 0.0): scale = 1.0
    let (sth, cth) = (sin(th), cos(th))
    result[ti] = [scale * cth, -scale * sth, scale * sth, scale * cth]
  for k, tris in s.pinTris:
    if k < fixed.len and fixed[k].isSome:
      let a = toRadians(fixed[k].get)
      let (sn, c) = (sin(a), cos(a))
      for t in tris: result[t] = [c, -sn, sn, c]

proc global(s: PuppetSolver; rots: seq[array[4, float64]]; pinDst: openArray[array[2, float64]]; v: var seq[array[2, float64]]) =
  let nv = v.len
  let x = s.mesh.verts
  var bx = newSeq[float64](nv)
  var by = newSeq[float64](nv)
  for ti, t in s.mesh.tris:
    let r = rots[ti]
    for k in 0 .. 2:
      let (i, j) = (t[(k + 1) mod 3], t[(k + 2) mod 3])
      let e = [x[i][0] - x[j][0], x[i][1] - x[j][1]]
      let re = [r[0] * e[0] + r[1] * e[1], r[2] * e[0] + r[3] * e[1]]
      let wk = s.w[ti][k]
      bx[i] += wk * re[0]
      by[i] += wk * re[1]
      bx[j] -= wk * re[0]
      by[j] -= wk * re[1]
  for q in 0 ..< min(s.bound.len, pinDst.len):
    let b = s.bound[q]
    for k in 0 .. 2:
      bx[b.verts[k]] += s.pinWeight * b.bary[k] * pinDst[q][0]
      by[b.verts[k]] += s.pinWeight * b.bary[k] * pinDst[q][1]
  for i, an in s.anchored:
    if an:
      bx[i] += s.pinWeight * x[i][0]
      by[i] += s.pinWeight * x[i][1]
  var sx = newSeq[float64](nv)
  var sy = newSeq[float64](nv)
  for i in 0 ..< nv:
    sx[i] = v[i][0]
    sy[i] = v[i][1]
  if s.chol.isSome:
    s.chol.get.solve(bx, sx)
    s.chol.get.solve(by, sy)
  else:
    s.matrix.solve(bx, sx)
    s.matrix.solve(by, sy)
  for i in 0 ..< nv: v[i] = [sx[i], sy[i]]

proc solve*(s: PuppetSolver; pinDst: openArray[array[2, float64]]; rotations: openArray[Option[float64]]; mode: PuppetMode;
            iterations: int; init = none(seq[array[2, float64]])): (seq[array[2, float64]], seq[float64]) =
  ## Deforms the mesh: `pinDst` (one per pin given to `newPuppetSolver`), optional fixed pin
  ## rotations (degrees), `iterations` local/global rounds, warm-started from `init` when given.
  ## Returns the deformed vertices and the rigid ARAP energy after each round.
  let nv = s.mesh.verts.len
  let warm = init.isSome and init.get.len == nv
  var v = (if warm: init.get else: s.mesh.verts)
  if nv == 0: return (v, @[])
  if not warm:
    # Start from R = I, or with rotated pins, each triangle turned by the inverse-distance
    # weighted angle of the rotated pins.
    var rotated: seq[(array[2, float64], float64)]
    for q in 0 ..< min(s.bound.len, rotations.len):
      if rotations[q].isSome:
        let b = s.bound[q]
        var p = [0.0, 0.0]
        for k in 0 .. 2: p = [p[0] + b.bary[k] * s.mesh.verts[b.verts[k]][0], p[1] + b.bary[k] * s.mesh.verts[b.verts[k]][1]]
        rotated.add (p, toRadians(rotations[q].get))
    var r = newSeq[array[4, float64]](s.mesh.tris.len)
    for ti, t in s.mesh.tris:
      if rotated.len == 0:
        r[ti] = [1.0, 0.0, 0.0, 1.0]
        continue
      var c = [0.0, 0.0]
      for i in t: c = [c[0] + s.mesh.verts[i][0] / 3.0, c[1] + s.mesh.verts[i][1] / 3.0]
      var (sw, sa) = (0.0, 0.0)
      for (p, a) in rotated:
        let wq = 1.0 / (dist2(p, c) + 1.0)
        sw += wq
        sa += wq * a
      let (sn, co) = (sin(sa / sw), cos(sa / sw))
      r[ti] = [co, -sn, sn, co]
    s.global(r, pinDst, v)
  var energies = newSeqOfCap[float64](iterations)
  for _ in 0 ..< iterations:
    let rots = s.local(v, mode, rotations)
    s.global(rots, pinDst, v)
    let rigid = s.local(v, pmRigid, rotations)
    energies.add s.energyWith(v, rigid, pinDst)
  (v, energies)

proc pinPoint*(s: PuppetSolver; v: openArray[array[2, float64]]; k: int): array[2, float64] =
  ## Where pin `k` sits on the deformed vertices `v` (its barycentric point).
  if k < 0 or k >= s.bound.len: return [NaN, NaN]
  let b = s.bound[k]
  result = [0.0, 0.0]
  for q in 0 .. 2: result = [result[0] + b.bary[q] * v[b.verts[q]][0], result[1] + b.bary[q] * v[b.verts[q]][1]]

proc energy*(s: PuppetSolver; v: seq[array[2, float64]]; pinDst: openArray[array[2, float64]]; rotations: openArray[Option[float64]]): float64 =
  ## Energy of the current state with the rigid best fit (for monitoring).
  s.energyWith(v, s.local(v, pmRigid, rotations), pinDst)

proc drawOrder*(s: PuppetSolver; depths: openArray[int32]; pinSrc: openArray[array[2, float64]]): seq[int] =
  ## Triangle draw order (back to front) for pins with depths.
  result = newSeq[int](s.mesh.tris.len)
  for i in 0 ..< result.len: result[i] = i
  var allZero = true
  for d in depths:
    if d != 0: allZero = false
  if allZero or pinSrc.len == 0: return
  var d = newSeq[int32](s.mesh.tris.len)
  for t in 0 ..< s.mesh.tris.len:
    var c = [0.0, 0.0]
    for i in s.mesh.tris[t]: c = [c[0] + s.mesh.verts[i][0] / 3.0, c[1] + s.mesh.verts[i][1] / 3.0]
    var k = 0
    for q in 1 ..< pinSrc.len:
      if totalCmp(dist2(pinSrc[q], c), dist2(pinSrc[k], c)) < 0: k = q
    d[t] = (if k < depths.len: depths[k] else: 0'i32)
  result.sort(proc(a, b: int): int = cmp(d[a], d[b]))

proc deform*(src: Surface; bounds: Rect; w: PuppetWarp; iterations: int): (PuppetSolver, seq[array[2, float64]], seq[int]) =
  ## The deformed mesh of a puppet warp: (solver, deformed vertices, draw order).
  let mesh = buildMesh(src, bounds, w.density, w.expansion)
  var pinSrc, dst: seq[array[2, float64]]
  var fixed: seq[bool]
  var rot: seq[Option[float64]]
  var depths: seq[int32]
  for p in w.pins:
    pinSrc.add p.src
    dst.add p.dst
    fixed.add p.rotate.isSome
    rot.add p.rotate
    depths.add p.depth
  let solver = newPuppetSolver(mesh, pinSrc, fixed)
  let (v, _) = solver.solve(dst, rot, w.mode, iterations)
  (solver, v, solver.drawOrder(depths, pinSrc))

proc render*(src: Surface; bounds: Rect; mesh: PuppetMesh; deformed: openArray[array[2, float64]]; order: openArray[int];
             interp: Interp): Surface =
  ## Rasterizes the mesh deformation of `src` (inside `bounds`): the deformed triangles only, so
  ## pixels outside the mesh are dropped.
  var verts: seq[(array[2, float64], array[2, float64])]
  for i in 0 ..< min(deformed.len, mesh.verts.len): verts.add (deformed[i], mesh.verts[i])
  var tris: seq[array[3, int]]
  for t in order: tris.add mesh.tris[t]
  warpTriangles(src, bounds, verts, tris, interp)

proc puppetWarp*(src: Surface; bounds: Rect; w: PuppetWarp; interp: Interp): Option[Surface] =
  ## Applies a puppet warp to `src` (content inside `bounds`). None when the deformed mesh would
  ## span more than `MaxWarpPixels`.
  let (solver, v, order) = deform(src, bounds, w, Iterations)
  # No triangles (an expansion that shrinks every pixel away): nothing to warp.
  if solver.mesh.tris.len == 0: return some(src)
  var (x0, y0, x1, y1) = (F64Max, F64Max, -F64Max, -F64Max)
  for p in v:
    (x0, y0, x1, y1) = (rmin(x0, p[0]), rmin(y0, p[1]), rmax(x1, p[0]), rmax(y1, p[1]))
  let area = (x1 - x0 + 1.0) * (y1 - y0 + 1.0)
  if area != area or area > MaxWarpPixels: return none(Surface)
  some(render(src, bounds, solver.mesh, v, order, interp))

# ---------------------------------------------------------------- JSON

proc num(n: JsonNode; name: string): float64 =
  case n.kind
  of JInt: float64(n.getBiggestInt)
  of JFloat: n.getFloat
  else: raise newException(ValueError, "invalid type for " & name & ": expected a number")

proc point(n: JsonNode; name: string): array[2, float64] =
  if n.kind != JArray or n.len != 2: raise newException(ValueError, name & " is [x, y]")
  [num(n[0], name), num(n[1], name)]

proc puppetWarpFromJson*(j: JsonNode): PuppetWarp =
  ## The warp from its JSON form (as the Rust build serialises it); raises `ValueError` on a
  ## malformed value.
  if j.kind != JObject: raise newException(ValueError, "a puppet warp is an object")
  result = defaultPuppetWarp()
  if j.hasKey("pins"):
    if j["pins"].kind != JArray: raise newException(ValueError, "pins must be an array")
    for p in j["pins"]:
      if p.kind != JObject or not p.hasKey("src") or not p.hasKey("dst"): raise newException(ValueError, "a pin is {src, dst}")
      var pin = PuppetPin(src: point(p["src"], "src"), dst: point(p["dst"], "dst"))
      if p.hasKey("rotate") and p["rotate"].kind != JNull: pin.rotate = some(num(p["rotate"], "rotate"))
      if p.hasKey("depth"):
        if p["depth"].kind != JInt or p["depth"].getBiggestInt < int64(low(int32)) or p["depth"].getBiggestInt > int64(high(int32)):
          raise newException(ValueError, "depth must be a whole number")
        pin.depth = int32(p["depth"].getBiggestInt)
      result.pins.add pin
  for (key, parse) in [("mode", 0), ("density", 1)]:
    if j.hasKey(key):
      let v = j[key]
      var ok = false
      if v.kind == JString:
        if parse == 0:
          for m in PuppetMode:
            if $m == v.getStr:
              result.mode = m
              ok = true
        else:
          for d in PuppetDensity:
            if $d == v.getStr:
              result.density = d
              ok = true
      if not ok: raise newException(ValueError, "unknown variant for " & key & ": " & $v)
  if j.hasKey("expansion"): result.expansion = num(j["expansion"], "expansion")

proc toJson*(w: PuppetWarp): JsonNode =
  var pins = newJArray()
  for p in w.pins:
    var o = %*{"src": p.src, "dst": p.dst}
    if p.rotate.isSome: o["rotate"] = %p.rotate.get
    if p.depth != 0: o["depth"] = %p.depth
    pins.add o
  %*{"pins": pins, "mode": $w.mode, "density": $w.density, "expansion": w.expansion}
