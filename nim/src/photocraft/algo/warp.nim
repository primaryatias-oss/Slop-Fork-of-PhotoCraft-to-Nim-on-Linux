## Mesh warping (Edit › Transform › Warp, smart-object placement with a warp, Liquify-style
## forward maps): the source rectangle is split into small cells whose forward-mapped vertices
## form triangles; each output pixel is sampled through the barycentric source position.
## Rust `photocraft_algo::warp`.

import std/math
import ../geom, ../color, ../raster
import ../geom/warp as gwarp
import transform, resample

const
  Cell = 4.0
  MaxCells = 512

type ForwardMap* = proc(x, y: float64): (float64, float64)

proc isFiniteF(v: float64): bool {.inline.} = classify(v) notin {fcInf, fcNegInf, fcNan}

proc warpTriangles*(src0: Surface; srcRect: Rect; verts: openArray[(array[2, float64], array[2, float64])];
                    tris: openArray[array[3, int]]; interp: Interp): Surface =
  ## Renders triangles whose vertices carry (destination, source) positions.
  var fmt = src0.format
  var src = src0
  if not fmt.alpha:
    fmt = pixelFormat(fmt.mode, fmt.sample, true)
    src = src0.convert(fmt)
  result = newSurface(fmt)
  if srcRect.isEmpty or tris.len == 0: return
  for v in verts:
    if not isFiniteF(v[0][0]) or not isFiniteF(v[0][1]): return
  for tri in tris:
    for i in tri:
      if i < 0 or i >= verts.len: return
  let lim = float64(1 shl 20)
  var (x0, y0, x1, y1) = (Inf, Inf, NegInf, NegInf)
  for tri in tris:
    for i in tri:
      let v = verts[i]
      x0 = min(x0, v[0][0]); y0 = min(y0, v[0][1]); x1 = max(x1, v[0][0]); y1 = max(y1, v[0][1])
  let dst = rect(asI32(floor(max(x0, -lim))) - 1, asI32(floor(max(y0, -lim))) - 1,
                 asI32(ceil(min(x1, lim))) + 1, asI32(ceil(min(y1, lim))) + 1)
  var tiles: seq[Rect]
  for tc in dst.tiles:
    let r = tc.rect.intersect(dst)
    if not r.isEmpty: tiles.add r
  if tiles.len == 0: return
  var bins = newSeq[seq[array[3, int]]](tiles.len)
  let ts = TileSize
  var tx0 = high(int)
  var ty0 = high(int)
  for t in tiles:
    tx0 = min(tx0, t.x0); ty0 = min(ty0, t.y0)
  tx0 = divEuclid(tx0, ts) * ts
  ty0 = divEuclid(ty0, ts) * ts
  var maxCol, maxRow = 0
  for t in tiles:
    maxCol = max(maxCol, (t.x0 - tx0) div ts)
    maxRow = max(maxRow, (t.y0 - ty0) div ts)
  let cols = maxCol + 1
  var lookup = newSeq[int](cols * (maxRow + 1))
  for v in lookup.mitems: v = -1
  for k, t in tiles:
    lookup[((t.y0 - ty0) div ts) * cols + (t.x0 - tx0) div ts] = k
  for tri in tris:
    var (bx0f, by0f, bx1f, by1f) = (Inf, Inf, NegInf, NegInf)
    for i in tri:
      let p = verts[i][0]
      bx0f = min(bx0f, p[0]); by0f = min(by0f, p[1]); bx1f = max(bx1f, p[0]); by1f = max(by1f, p[1])
    let (bx0, by0, bx1, by1) = (asI32(floor(bx0f)), asI32(floor(by0f)), asI32(ceil(bx1f)), asI32(ceil(by1f)))
    let (cx0, cy0) = (divEuclid(max(bx0, dst.x0) - tx0, ts), divEuclid(max(by0, dst.y0) - ty0, ts))
    let (cx1, cy1) = (divEuclid(min(bx1, dst.x1 - 1) - tx0, ts), divEuclid(min(by1, dst.y1 - 1) - ty0, ts))
    for cy in max(cy0, 0) .. cy1:
      for cx in max(cx0, 0) .. cx1:
        if cx >= cols: continue
        let g = cy * cols + cx
        if g >= 0 and g < lookup.len and lookup[g] >= 0:
          bins[lookup[g]].add tri
  let n = fmt.channels
  let a = n - 1
  for ti, t in tiles:
    let ttris = bins[ti]
    if ttris.len == 0: continue
    let w = t.width
    let h = t.height
    var uv = newSeq[array[2, float64]](w * h)
    for q in uv.mitems: q = [NaN, NaN]
    for tri in ttris:
      let (p0, s0) = verts[tri[0]]
      let (p1, s1) = verts[tri[1]]
      let (p2, s2) = verts[tri[2]]
      let det = (p1[0] - p0[0]) * (p2[1] - p0[1]) - (p2[0] - p0[0]) * (p1[1] - p0[1])
      if abs(det) < 1e-12 or det != det: continue
      let bx0 = asI32(max(floor(min(p0[0], min(p1[0], p2[0])) - 0.5), float64(t.x0)))
      let by0 = asI32(max(floor(min(p0[1], min(p1[1], p2[1])) - 0.5), float64(t.y0)))
      let bx1 = asI32(min(ceil(max(p0[0], max(p1[0], p2[0])) + 0.5), float64(t.x1)))
      let by1 = asI32(min(ceil(max(p0[1], max(p1[1], p2[1])) + 0.5), float64(t.y1)))
      let eps = -1e-9
      for y in by0 ..< by1:
        let py = float64(y) + 0.5
        for x in bx0 ..< bx1:
          let px = float64(x) + 0.5
          let l1 = ((px - p0[0]) * (p2[1] - p0[1]) - (p2[0] - p0[0]) * (py - p0[1])) / det
          let l2 = ((p1[0] - p0[0]) * (py - p0[1]) - (px - p0[0]) * (p1[1] - p0[1])) / det
          let l0 = 1.0 - l1 - l2
          if l0 < eps or l1 < eps or l2 < eps: continue
          let o = (y - t.y0) * w + (x - t.x0)
          uv[o] = [l0 * s0[0] + l1 * s1[0] + l2 * s2[0], l0 * s0[1] + l1 * s1[1] + l2 * s2[1]]
    var (fx0, fy0, fx1, fy1) = (Inf, Inf, NegInf, NegInf)
    for q in uv:
      if q[0] != q[0]: continue
      fx0 = min(fx0, q[0]); fy0 = min(fy0, q[1]); fx1 = max(fx1, q[0]); fy1 = max(fy1, q[1])
    if fx0 > fx1: continue
    let foot = rect(asI32(floor(fx0)) - 3, asI32(floor(fy0)) - 3, asI32(ceil(fx1)) + 3,
                    asI32(ceil(fy1)) + 3).intersect(srcRect)
    if foot.isEmpty or not src.hasTilesIn(foot): continue
    var px = src.readRegion(foot)
    var p = 0
    while p + n <= px.len:
      let al = px[p + a]
      for c in 0 ..< a: px[p + c] *= al
      p += n
    let fw = foot.width
    template at(x, y, c: int): float64 =
      (if x < foot.x0 or y < foot.y0 or x >= foot.x1 or y >= foot.y1: 0.0
       else: float64(px[((y - foot.y0) * fw + (x - foot.x0)) * n + c]))
    var outp = newSeq[float32](w * h * n)
    var any = false
    var acc: array[8, float64]
    for o, q in uv:
      if q[0] != q[0]: continue
      var (u, v) = (q[0] - 0.5, q[1] - 0.5)
      for c in 0 ..< n: acc[c] = 0
      case interp
      of ipNearest:
        let (ix, iy) = (asI32(floor(u + 0.5)), asI32(floor(v + 0.5)))
        for c in 0 ..< n: acc[c] = at(ix, iy, c)
      of ipBilinear:
        let (ix, iy) = (asI32(floor(u)), asI32(floor(v)))
        let (fx, fy) = (u - float64(ix), v - float64(iy))
        for (dy, wy) in [(0, 1.0 - fy), (1, fy)]:
          for (dx, wx) in [(0, 1.0 - fx), (1, fx)]:
            for c in 0 ..< n: acc[c] += at(ix + dx, iy + dy, c) * wx * wy
      of ipBicubic:
        if abs(u - round(u)) < 1e-7: u = round(u)
        if abs(v - round(v)) < 1e-7: v = round(v)
        let (ix, iy) = (asI32(floor(u)), asI32(floor(v)))
        let (wx, wy) = (catmullRom4(u - float64(ix)), catmullRom4(v - float64(iy)))
        for j in 0 ..< 4:
          for i in 0 ..< 4:
            let k = wx[i] * wy[j]
            if k == 0.0: continue
            for c in 0 ..< n: acc[c] += at(ix - 1 + i, iy - 1 + j, c) * k
      let al = clamp(acc[a], 0.0, 1.0)
      if al <= 1e-6: continue
      any = true
      let b = o * n
      for c in 0 ..< a: outp[b + c] = float32(clamp(acc[c] / al, 0.0, 1.0))
      outp[b + a] = float32(al)
    if any: result.writeRegion(t, outp)
  result.prune()

proc warpMeshSurface*(src: Surface; srcRect: Rect; f: ForwardMap; interp: Interp): Surface =
  ## Warps the content of `src` inside `srcRect` through the forward map `f` (source document
  ## coordinates → destination). Output has `src`'s format with alpha added.
  if srcRect.isEmpty:
    let fmt = src.format
    return newSurface(pixelFormat(fmt.mode, fmt.sample, true))
  let (sw, sh) = (float64(srcRect.width), float64(srcRect.height))
  let nx = clamp(asUsize(ceil(sw / Cell)), 1, MaxCells)
  let ny = clamp(asUsize(ceil(sh / Cell)), 1, MaxCells)
  var verts = newSeqOfCap[(array[2, float64], array[2, float64])]((nx + 1) * (ny + 1))
  for j in 0 .. ny:
    for i in 0 .. nx:
      let sx = float64(srcRect.x0) + sw * float64(i) / float64(nx)
      let sy = float64(srcRect.y0) + sh * float64(j) / float64(ny)
      let (dx, dy) = f(sx, sy)
      verts.add ([dx, dy], [sx, sy])
  let w1 = nx + 1
  var tris = newSeqOfCap[array[3, int]](nx * ny * 2)
  for j in 0 ..< ny:
    for i in 0 ..< nx:
      let a = j * w1 + i
      tris.add [a, a + 1, a + w1 + 1]
      tris.add [a, a + w1 + 1, a + w1]
  warpTriangles(src, srcRect, verts, tris, interp)

proc placeSource*(src: Surface; srcRect: Rect; t: Affine; warp: Option[Warp]): Surface =
  ## Places a smart object's source (its `srcRect` content) into the document: the warp first
  ## (in source space), then the affine `t`.
  let m = t.m
  let (a, b, c, d, e, f) = (m[0], m[1], m[2], m[3], m[4], m[5])
  if warp.isSome and not warp.get.isIdentity:
    let w = warp.get
    return warpMeshSurface(src, srcRect, proc(x, y: float64): (float64, float64) =
      let (u, v) = w.map(x, y)
      (a * u + c * v + e, b * u + d * v + f), ipBicubic)
  proc near(x, y: float64): bool = abs(x - y) < 1e-9
  if near(a, 1.0) and near(b, 0.0) and near(c, 0.0) and near(d, 1.0) and near(e, round(e)) and near(f, round(f)):
    return translateSurface(src, asI32(round(e)), asI32(round(f)))
  warpSurface(src, srcRect, Homography(m: [a, c, e, b, d, f, 0.0, 0.0, 1.0]), ipBicubic)

proc placeSourceProjective*(src: Surface; srcRect: Rect; h: Homography; warp: Option[Warp]): Surface =
  ## `placeSource` through a projective map `h` (source → document; Distort, Perspective).
  if warp.isSome and not warp.get.isIdentity:
    let w = warp.get
    return warpMeshSurface(src, srcRect, proc(x, y: float64): (float64, float64) =
      let (u, v) = w.map(x, y)
      h.apply(u, v), ipBicubic)
  warpSurface(src, srcRect, h, ipBicubic)

proc warpMeshGray*(s: Surface; f: ForwardMap; interp: Interp): Surface =
  ## Warps a single-channel surface (masks, selections) whose default is its outside value.
  let defPx = s.defaultPixel
  let default = if defPx.len > 0: defPx[0] else: 0'f32
  let fmt = s.format
  let src = s.contentBounds
  result = surfaceWithDefault(fmt, [default])
  if src.isEmpty: return
  let withAlpha = pixelFormat(fmt.mode, fmt.sample, true)
  var tmp = newSurface(withAlpha)
  let v = s.readRegion(src)
  var pairs = newSeq[float32](v.len * 2)
  for i, g in v:
    pairs[2 * i] = g
    pairs[2 * i + 1] = 1
  tmp.writeRegion(src, pairs)
  let w = warpMeshSurface(tmp, src, f, interp)
  var fillv = newSeq[float32](src.width * src.height)
  for x in fillv.mitems: x = default
  result.writeRegion(src, fillv)
  let b = w.contentBounds
  if not b.isEmpty:
    let px = w.readRegion(b)
    var flat = newSeq[float32](px.len div 2)
    for i in 0 ..< flat.len: flat[i] = px[2 * i] * px[2 * i + 1] + default * (1 - px[2 * i + 1])
    result.writeRegion(b, flat)
  result.prune()
