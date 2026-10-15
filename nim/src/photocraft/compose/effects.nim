## Layer styles: Drop/Inner Shadow, Outer/Inner Glow, Stroke, Color/Gradient/Pattern Overlay,
## Satin and Bevel & Emboss. Effect "maps" (coverage per effect) are built once per layer state
## over the layer's region from its shape, then composited in Photoshop's order: drop shadows and
## outer glows beneath, overlays/satin/inner glow/inner shadow inside the content, strokes, then
## bevels. Distances use exact Euclidean transforms (Felzenszwalb) or a 5×5 chamfer, bevels refine
## distances near sharp anti-aliased edges, blurs are box-of-box ("tent") kernels.

import std/[math, options]
import ../doc, ./buffer, ./psblend, ./gradient, ./pattern, ./adjust

const
  MaxReach* = 512'f32
  InsideEps = 0.5'f32 / 255
  EdgeMargin = 2'f32
  EdgeReach = 0.75'f32
  NormalRadius = 3
  EdgeLineMinSpread = 0.7'f32
  EdgeLineMaxOff = 0.1'f32
  FootRadius = 2
  ChamferInf = 1e20'f32
  Far = 1e9'f32
  BevelHEps* = 1e-5'f32

type
  Map = object
    w, h: int
    v: seq[float32]
  Metric = enum mEuclidean, mChamfer
  SharpEdge = object
    offset: float32
    normal: array[2, float32]
  EdgePixel = object
    at: int
    cov: float32
    edge: SharpEdge
  SharpEdges = object
    w, h: int
    slot: seq[int32]
    pixels: seq[EdgePixel]
  BevelPaint* = enum bpInner, bpOuter, bpBoth
  BevelGeom* = object
    paint*: BevelPaint
    width*, depth*: float32
    pillow*: bool
    chiselSoft*: float32
  FxMaps* = object
    rect*: Rect
    shape: Map
    per: seq[seq[Map]]
    bevelPaint: seq[BevelPaint]
    din, dout, vdout: Option[seq[float32]]
    outline: bool
    frames: seq[(float32, Rect)]
  TextureCtx* = object
    rect*: Rect
    anchor*: (float64, float64)
  VectorStroke* = object
    stroke*: Buffer
    mask*: Option[seq[float32]]

proc hasEffects*(l: Layer): bool =
  if not l.effects.enabled: return false
  for e in l.effects.items:
    if e.enabled: return true

proc margin*(l: Layer): int =
  var m = 0'f32
  for e in l.effects.items:
    if not e.enabled: continue
    let r = case e.kind
      of ekDropShadow, ekInnerShadow: e.shadow.distance + e.shadow.size
      of ekOuterGlow, ekInnerGlow: e.glow.size
      of ekStroke: e.stroke.size
      of ekSatin: e.satin.distance + e.satin.size
      of ekBevelEmboss: e.bevel.size + e.bevel.soften
      else: 0'f32
    if r == r and abs(r) != Inf: m = max(m, r)
  int(ceil(clamp(m, 0'f32, MaxReach))) + 2

proc mapsAreLocal*(l: Layer): bool =
  for e in l.effects.items:
    if not e.enabled: continue
    case e.kind
    of ekStroke:
      if e.stroke.paint.kind == fpGradient: return false
    of ekDropShadow, ekInnerShadow:
      if e.shadow.noise > 0: return false
    of ekOuterGlow, ekInnerGlow:
      if e.glow.technique == gtPrecise or e.glow.noise > 0: return false
    of ekBevelEmboss:
      if e.bevel.technique != btSmooth: return false
    else: discard
  true

proc effectOutline*(l: Layer): Option[Path] =
  ## The fill path a filled shape layer's effects follow (none for other layers).
  if l.content.kind != lkShape: return
  let sh = l.content.shape
  let strokeInside = sh.stroke.isNone or sh.stroke.get.width <= 0 or sh.stroke.get.align == saInside
  if sh.fill.isSome and strokeInside and not sh.path.isEmpty and not sh.path.inverted: some(sh.path) else: none(Path)

proc layerTextGamma*(l: Layer): float32 = (if l.content.kind == lkText: textGamma() else: 1'f32)

# ---------------------------------------------------------------- maps

proc newMap(w, h: int; fill = 0'f32): Map =
  result = Map(w: w, h: h, v: newSeq[float32](w * h))
  if fill != 0:
    for x in result.v.mitems: x = fill

proc get(m: Map; x, y: int): float32 {.inline.} =
  if x < 0 or y < 0 or x >= m.w or y >= m.h: 0'f32 else: m.v[y * m.w + x]

proc shifted(m: Map; dx, dy, outside: float32): Map =
  result = newMap(m.w, m.h)
  let (fx, fy) = (floor(dx), floor(dy))
  let (ax, ay) = (dx - fx, dy - fy)
  template s(x, y: int): float32 = (if x < 0 or y < 0 or x >= m.w or y >= m.h: outside else: m.v[y * m.w + x])
  for y in 0 ..< m.h:
    for x in 0 ..< m.w:
      let (sx, sy) = (x - int(fx), y - int(fy))
      let top = s(sx, sy) * (1 - ax) + s(sx - 1, sy) * ax
      let bot = s(sx, sy - 1) * (1 - ax) + s(sx - 1, sy - 1) * ax
      result.v[y * m.w + x] = top * (1 - ay) + bot * ay

proc invert(m: Map): Map =
  result = m
  for x in result.v.mitems: x = 1 - x

# ---------------------------------------------------------------- distance transforms

proc dt1(f: openArray[float32]; o: var openArray[float32]; v: var seq[int]; z: var seq[float32]; near: var openArray[int]) =
  ## 1-D squared Euclidean distance transform (Felzenszwalb–Huttenlocher), with nearest sites.
  let n = f.len
  var k = 0
  v[0] = 0
  z[0] = NegInf
  z[1] = Inf
  for q in 1 ..< n:
    while true:
      let p = v[k]
      let s = ((f[q] + float32(q * q)) - (f[p] + float32(p * p))) / (2 * float32(q) - 2 * float32(p))
      if s <= z[k] and k > 0:
        dec k
        continue
      if s <= z[k]:
        v[0] = q
        z[0] = NegInf
        z[1] = Inf
        break
      inc k
      v[k] = q
      z[k] = s
      z[k + 1] = Inf
      break
  k = 0
  for q in 0 ..< n:
    while z[k + 1] < float32(q): inc k
    let p = v[k]
    let d = float32(q) - float32(p)
    o[q] = d * d + f[p]
    near[q] = p

proc edtNearest(inside: seq[bool]; w, h: int): (seq[float32], seq[int]) =
  const InfD = 1e20'f32
  let total = w * h
  if total != inside.len or total == 0: return (newSeq[float32](inside.len), newSeq[int](inside.len))
  let n = max(w, h)
  var v = newSeq[int](n)
  var z = newSeq[float32](n + 1)
  var f = newSeq[float32](n)
  var o = newSeq[float32](n)
  var nr = newSeq[int](n)
  var gc = newSeq[float32](total)
  var nc = newSeq[int](total)
  for x in 0 ..< w:
    for y in 0 ..< h: f[y] = if inside[y * w + x]: 0'f32 else: InfD
    dt1(f.toOpenArray(0, h - 1), o, v, z, nr)
    for y in 0 ..< h:
      gc[x * h + y] = o[y]
      nc[x * h + y] = nr[y]
  var g = newSeq[float32](total)
  var nearest = newSeq[int](total)
  for y in 0 ..< h:
    for x in 0 ..< w: f[x] = gc[x * h + y]
    dt1(f.toOpenArray(0, w - 1), o, v, z, nr)
    for x in 0 ..< w:
      g[y * w + x] = sqrt(o[x])
      let px = nr[x]
      nearest[y * w + x] = nc[px * h + y] * w + px
  (g, nearest)

proc chamferFrom(d0: seq[float32]; w, h: int): (seq[float32], seq[int]) =
  const S2 = 1.4142135'f32
  const S5 = 2.236068'f32
  const Fwd = [(-1, 0, 1'f32), (-1, -1, S2), (0, -1, 1'f32), (1, -1, S2), (-1, -2, S5), (1, -2, S5), (-2, -1, S5), (2, -1, S5)]
  var d = d0
  var near = newSeq[int](w * h)
  for i in 0 ..< near.len: near[i] = i
  template relax(x, y, sign: int) =
    let i = y * w + x
    var best = d[i]
    var bn = near[i]
    for (ox, oy, wt) in Fwd:
      let (nx, ny) = (x + ox * sign, y + oy * sign)
      if nx < 0 or ny < 0 or nx >= w or ny >= h: continue
      let j = ny * w + nx
      let c = d[j] + wt
      if c < best:
        best = c
        bn = near[j]
    d[i] = best
    near[i] = bn
  for y in 0 ..< h:
    for x in 0 ..< w: relax(x, y, 1)
  for y in countdown(h - 1, 0):
    for x in countdown(w - 1, 0): relax(x, y, -1)
  (d, near)

proc covered(s: Map; inside: bool): seq[bool] =
  ## inside: a > eps; otherwise a <= eps.
  result = newSeq[bool](s.v.len)
  for i, a in s.v: result[i] = if inside: a > InsideEps else: a <= InsideEps

proc localCoverage(s: Map): Map =
  result = s
  for y in 0 ..< s.h:
    for x in 0 ..< s.w:
      let a = s.get(x, y)
      if a <= InsideEps: continue
      var mx = a
      for dy in -1..1:
        for dx in -1..1: mx = max(mx, s.get(x + dx, y + dy))
      result.v[y * s.w + x] = min(a / mx, 1'f32)

proc distOutsideBy(s: Map; m: Metric): seq[float32] =
  if m == mChamfer:
    var start = newSeq[float32](s.v.len)
    for i, a in s.v: start[i] = if a > InsideEps: 1 - min(a, 1'f32) else: ChamferInf
    let (d, _) = chamferFrom(start, s.w, s.h)
    result = newSeq[float32](s.v.len)
    for i, a in s.v: result[i] = if a > InsideEps: -0.5'f32 else: d[i] - 0.5'f32
    return
  let inside = covered(s, true)
  let (d, near) = edtNearest(inside, s.w, s.h)
  result = newSeq[float32](s.v.len)
  for i in 0 ..< s.v.len:
    result[i] = if inside[i]: -0.5'f32 else: d[i] - 0.5'f32 + (1 - min(s.v[near[i]], 1'f32))

proc distInsideBy(s: Map; m: Metric): seq[float32] =
  if m == mChamfer:
    let cov = localCoverage(s)
    var start = newSeq[float32](s.v.len)
    for i, a in s.v:
      let c = cov.v[i]
      start[i] = if a <= InsideEps: 0'f32 elif c < 1 - InsideEps: c else: ChamferInf
    let (d, _) = chamferFrom(start, s.w, s.h)
    result = newSeq[float32](s.v.len)
    for i, a in s.v: result[i] = if a <= InsideEps: -0.5'f32 else: d[i] - 0.5'f32
    return
  let outside = covered(s, false)
  let (d, _) = edtNearest(outside, s.w, s.h)
  result = newSeq[float32](s.v.len)
  for i in 0 ..< s.v.len: result[i] = if outside[i]: -0.5'f32 else: d[i] - 0.5'f32

# ---------------------------------------------------------------- sharp edges (bevels)

proc edgeOffset(n: array[2, float32]; a: float32): float32 =
  if a != a or abs(a) == Inf: return 0
  let (u, v) = (max(abs(n[0]), abs(n[1])), min(abs(n[0]), abs(n[1])))
  let (c0, inside) = if a > 0.5'f32: (1 - a, true) else: (a, false)
  let c = clamp(c0, 0'f32, 0.5'f32)
  let corner = if u > 0: v / (2 * u) else: 0'f32
  let r = if c >= corner: (0.5'f32 - c) * u else: 0.5'f32 * (u + v) - sqrt(2 * u * v * c)
  if inside: r else: -r

proc distance(e: SharpEdge; dx, dy: float32; inside: bool): Option[float32] =
  let (nx, ny) = (e.normal[0], e.normal[1])
  if nx == 0 and ny == 0: return none(float32)
  let signed = e.offset + dx * nx + dy * ny
  let across = if inside: signed else: -signed
  let along = abs(dx * -ny + dy * nx)
  if across > 0: some(hypot(across, max(along - EdgeReach, 0'f32))) else: none(float32)

proc edgeAt(s: SharpEdges; i: int): Option[SharpEdge] =
  if i < 0 or i >= s.slot.len: return none(SharpEdge)
  let k = int(s.slot[i]) - 1
  if k < 0 or k >= s.pixels.len: none(SharpEdge) else: some(s.pixels[k].edge)

proc edgeXY(s: SharpEdges; x, y: int): Option[SharpEdge] =
  if x < 0 or y < 0 or x >= s.w or y >= s.h: none(SharpEdge) else: s.edgeAt(y * s.w + x)

proc refineEdgeNormals(pixels: var seq[EdgePixel]; slot: seq[int32]; w, h: int) =
  var points = newSeq[array[2, float32]](pixels.len)
  for k, p in pixels:
    points[k] = [float32(p.at mod w) - p.edge.offset * p.edge.normal[0], float32(p.at div w) - p.edge.offset * p.edge.normal[1]]
  let before = pixels
  var near: seq[array[2, float32]]
  for k in 0 ..< pixels.len:
    let e = before[k].edge
    let (x, y) = (pixels[k].at mod w, pixels[k].at div w)
    var n = 0'f32
    var sum = [0'f32, 0]
    near.setLen(0)
    for dy in -NormalRadius .. NormalRadius:
      for dx in -NormalRadius .. NormalRadius:
        let (nx, ny) = (x + dx, y + dy)
        if dx * dx + dy * dy > NormalRadius * NormalRadius or nx < 0 or ny < 0 or nx >= w or ny >= h: continue
        let j = int(slot[ny * w + nx]) - 1
        if j < 0: continue
        let o = before[j].edge
        if o.normal[0] * e.normal[0] + o.normal[1] * e.normal[1] > 0.9'f32:
          near.add points[j]
          sum = [sum[0] + points[j][0], sum[1] + points[j][1]]
          n += 1
    if near.len < 3: continue
    let mean = [sum[0] / n, sum[1] / n]
    var sxx, sxy, syy: float32
    for p in near:
      let (dx, dy) = (p[0] - mean[0], p[1] - mean[1])
      sxx += dx * dx
      sxy += dx * dy
      syy += dy * dy
    let half = 0.5'f32 * (sxx + syy)
    let root = sqrt(0.25'f32 * (sxx - syy) * (sxx - syy) + sxy * sxy)
    let (along, across) = (sqrt(max((half + root) / n, 0)), sqrt(max((half - root) / n, 0)))
    if not (along > EdgeLineMinSpread and across < EdgeLineMaxOff): continue
    let angle = float32(0.5 * arctan2(float(2 * sxy), float(sxx - syy)))
    var normal: array[2, float32] = [-sin(angle), cos(angle)]
    if normal[0] * e.normal[0] + normal[1] * e.normal[1] < 0: normal = [-normal[0], -normal[1]]
    pixels[k].edge.normal = normal
    pixels[k].edge.offset = edgeOffset(normal, pixels[k].cov)

proc sharpEdges(s: Map): SharpEdges =
  let (w, h) = (s.w, s.h)
  result = SharpEdges(w: w, h: h)
  if w == 0 or h == 0 or s.v.len != w * h: return
  proc covAt(x, y: int): float32 =
    let a = s.v[y * w + x]
    if a <= InsideEps: return a
    var mx = a
    for dy in -1..1:
      for dx in -1..1: mx = max(mx, s.get(x + dx, y + dy))
    min(a / mx, 1'f32)
  proc covClamped(x, y: int): float32 = covAt(clamp(x, 0, w - 1), clamp(y, 0, h - 1))
  proc solid(x, y: int): bool =
    if x < 0 or y < 0 or x >= w or y >= h: return true
    let a = s.v[y * w + x]
    a != a or a > InsideEps
  for y in 0 ..< h:
    for x in 0 ..< w:
      if not solid(x, y): continue
      var allSolid = true
      for dy in -1..1:
        for dx in -1..1:
          if not solid(x + dx, y + dy): allSolid = false
      if allSolid: continue
      let i = y * w + x
      let c = covAt(x, y)
      if s.v[i] <= InsideEps or c >= 1 - InsideEps: continue
      var empty, full = false
      for dy in -1..1:
        for dx in -1..1:
          let (nx, ny) = (x + dx, y + dy)
          if (dx == 0 and dy == 0) or nx < 0 or ny < 0 or nx >= w or ny >= h: continue
          if s.get(nx, ny) <= InsideEps: empty = true
          if covAt(nx, ny) >= 1 - InsideEps: full = true
      if not (empty and full): continue
      template at(px, py: int): float32 = covClamped(px, py)
      let gx = (at(x + 1, y - 1) + 2 * at(x + 1, y) + at(x + 1, y + 1)) - (at(x - 1, y - 1) + 2 * at(x - 1, y) + at(x - 1, y + 1))
      let gy = (at(x - 1, y + 1) + 2 * at(x, y + 1) + at(x + 1, y + 1)) - (at(x - 1, y - 1) + 2 * at(x, y - 1) + at(x + 1, y - 1))
      let ln = hypot(gx, gy)
      var normal = [0'f32, 0]
      var nn = [1'f32, 0]
      if ln == ln and ln != Inf and ln > 1e-6'f32:
        normal = [gx / ln, gy / ln]
        nn = normal
      result.pixels.add EdgePixel(at: i, cov: c, edge: SharpEdge(offset: edgeOffset(nn, c), normal: normal))
  result.slot = newSeq[int32](w * h)
  for k, p in result.pixels: result.slot[p.at] = int32(k + 1)
  refineEdgeNormals(result.pixels, result.slot, w, h)

proc edgeDistance(sharp: SharpEdges; p, q: array[2, int]; inside: bool): Option[float32] =
  let (px, py) = (float32(p[0]), float32(p[1]))
  let eo = sharp.edgeXY(q[0], q[1])
  if eo.isNone: return
  let e = eo.get
  let d0 = e.distance(px - float32(q[0]), py - float32(q[1]), inside)
  if d0.isNone: return
  var best = d0.get
  let signed = e.offset + (px - float32(q[0])) * e.normal[0] + (py - float32(q[1])) * e.normal[1]
  let (fx, fy) = (int(round(px - signed * e.normal[0])), int(round(py - signed * e.normal[1])))
  for sy in max(fy - FootRadius, 0) .. min(fy + FootRadius, sharp.h - 1):
    for sx in max(fx - FootRadius, 0) .. min(fx + FootRadius, sharp.w - 1):
      let ne = sharp.edgeXY(sx, sy)
      if ne.isSome:
        let d = ne.get.distance(px - float32(sx), py - float32(sy), inside)
        if d.isSome: best = min(best, d.get)
  some(best)

proc bevelDistInside(s: Map; sharp: SharpEdges; reach: float32): seq[float32] =
  let empty = covered(s, false)
  let (d, near) = edtNearest(empty, s.w, s.h)
  result = newSeq[float32](s.v.len)
  for y in 0 ..< s.h:
    for x in 0 ..< s.w:
      let i = y * s.w + x
      if empty[i]:
        result[i] = -0.5
        continue
      let e = sharp.edgeAt(i)
      if e.isSome:
        result[i] = e.get.offset
        continue
      let plain = d[i] - 0.5'f32
      if d[i] > reach + EdgeMargin or sharp.pixels.len == 0:
        result[i] = plain
        continue
      let (ox, oy) = (near[i] mod s.w, near[i] div s.w)
      var best = -1
      var bq = (0, 0)
      for dy in -1..1:
        for dx in -1..1:
          let (qx, qy) = (ox + dx, oy + dy)
          if qx >= 0 and qy >= 0 and sharp.edgeXY(qx, qy).isSome:
            let dd = (qx - x) * (qx - x) + (qy - y) * (qy - y)
            if best < 0 or dd < best:
              best = dd
              bq = (qx, qy)
      if best >= 0:
        let r = edgeDistance(sharp, [x, y], [bq[0], bq[1]], true)
        result[i] = if r.isSome: min(r.get, plain) else: plain
      else: result[i] = plain

proc bevelDistOutside(s: Map; sharp: SharpEdges; reach: float32): seq[float32] =
  let inside = covered(s, true)
  let (d, near) = edtNearest(inside, s.w, s.h)
  result = newSeq[float32](s.v.len)
  for y in 0 ..< s.h:
    for x in 0 ..< s.w:
      let i = y * s.w + x
      let ei = sharp.edgeAt(i)
      if ei.isSome:
        result[i] = -ei.get.offset
        continue
      if inside[i]:
        result[i] = -0.5
        continue
      let q = near[i]
      let e = sharp.edgeAt(q)
      if e.isNone:
        result[i] = d[i] - (min(s.v[q], 1'f32) - 0.5'f32)
        continue
      let qp = [q mod s.w, q div s.w]
      let measured = if d[i] <= reach + EdgeMargin: edgeDistance(sharp, [x, y], qp, false)
                     else: e.get.distance(float32(x - qp[0]), float32(y - qp[1]), false)
      result[i] = if measured.isSome: measured.get else: d[i] - e.get.offset

# ---------------------------------------------------------------- blurs and curves

proc boxWidth(w: float32): float32 = (if w != w: 1'f32 else: clamp(w, 1'f32, MaxReach))

proc boxWeights(w0: float32): seq[float32] =
  let w = boxWidth(w0)
  let half = w / 2
  let r = int(max(ceil(half - 0.5'f32), 0))
  var sum = 0'f32
  for i in -r .. r:
    let x = max(min(float32(i) + 0.5'f32, half) - max(float32(i) - 0.5'f32, -half), 0'f32)
    result.add x
    sum += x
  for x in result.mitems: x /= sum

proc tentKernel*(w: float32): (int, seq[float32]) =
  let b = boxWeights(w)
  let n = b.len * 2 - 1
  var t = newSeq[float32](n)
  for i, x in b:
    for j, y in b: t[i + j] += x * y
  ((n - 1) div 2, t)

proc boxGeom(bw0: float32): (int, float64, float64) =
  let bw = boxWidth(bw0)
  let half = bw / 2
  let r = int(max(ceil(half - 0.5'f32), 0))
  let f = float64(clamp(half - (float32(r) - 0.5'f32), 0'f32, 1'f32))
  (r, f, 1.0 / float64(bw))

proc boxLine(src: openArray[float64]; x0: int; dst: var openArray[float64]; bw: float32) =
  let n = src.len
  let (r, f, norm) = boxGeom(bw)
  template at(i: int): float64 = (if i >= 0 and i < n: src[i] else: 0.0)
  if r == 0:
    for k in 0 ..< dst.len: dst[k] = at(x0 + k)
    return
  var inner = 0.0
  for i in x0 - (r - 1) .. x0 + (r - 1): inner += at(i)
  for k in 0 ..< dst.len:
    let x = x0 + k
    dst[k] = (inner + f * (at(x - r) + at(x + r))) * norm
    inner += at(x + r) - at(x - r + 1)

proc tentLine(src: openArray[float32]; dst: var openArray[float32]; bw: float32) =
  let (r, _, _) = boxGeom(bw)
  var s = newSeq[float64](src.len)
  for i, v in src: s[i] = float64(v)
  var mid = newSeq[float64](src.len + 2 * r)
  boxLine(s, -r, mid, bw)
  var o = newSeq[float64](src.len)
  boxLine(mid, r, o, bw)
  for i in 0 ..< dst.len: dst[i] = float32(o[i])

proc tent(m: var Map; w: float32) =
  if tentKernel(w)[0] == 0 or m.v.len == 0: return
  var a = newSeq[float32](m.v.len)
  for y in 0 ..< m.h: tentLine(m.v.toOpenArray(y * m.w, y * m.w + m.w - 1), a.toOpenArray(y * m.w, y * m.w + m.w - 1), w)
  var col = newSeq[float32](m.h)
  var o = newSeq[float32](m.h)
  for x in 0 ..< m.w:
    for y in 0 ..< m.h: col[y] = a[y * m.w + x]
    tentLine(col, o, w)
    for y in 0 ..< m.h: m.v[y * m.w + x] = o[y]

proc blur(m: var Map; size: float32) =
  if size == size and abs(size) != Inf: tent(m, size)

proc dilate(s: Map; r: float32): Map =
  if r <= 0: return s
  let d = distOutsideBy(s, mChamfer)
  result = s
  for i in 0 ..< result.v.len:
    result.v[i] = if d[i] < 0: min(result.v[i] + r, 1'f32) else: max(result.v[i], clamp(r + 0.5'f32 - d[i], 0'f32, 1'f32))

proc contourLut(c: Contour): Option[seq[float32]] =
  if c.kind == ckLinear: none(seq[float32]) else: some(contourCurveLut(c.points, c.corners))

proc lutAt(l: openArray[float32]; v: float32): float32 =
  let x = clamp(v, 0'f32, 1'f32) * float32(l.len - 1)
  let i = int(floor(x))
  let j = min(i + 1, l.len - 1)
  l[i] + (l[j] - l[i]) * (x - float32(i))

proc rangedLut*(contour: Contour; range0: float32): Option[seq[float32]] =
  let base = contourLut(contour)
  let rng = if range0 == range0 and abs(range0) != Inf: clamp(range0, 0.01'f32, 1'f32) else: 1'f32
  if rng >= 0.999'f32: return base
  var o = newSeq[float32](4096)
  for k in 0 ..< 4096:
    let v = min(float32(k) / 4095 / rng, 1'f32)
    o[k] = if base.isSome: lutAt(base.get, v) else: v
  some(o)

proc applyLut(m: Map; l: Option[seq[float32]]): Map =
  result = m
  if l.isSome:
    for x in result.v.mitems: x = lutAt(l.get, x)

proc applyContour(m: Map; c: Contour): Map = applyLut(m, contourLut(c))

proc hashNoise*(x, y: int64): float32 =
  var h = (cast[uint32](int32(x)) * 0x8da6b343'u32) xor (cast[uint32](int32(y)) * 0xd8163841'u32) xor 0x9e3779b9'u32
  h = h xor (h shr 15)
  h = h * 0x2c1b3c6d'u32
  h = h xor (h shr 12)
  float32(h and 0xffff) / 65536

proc noise(m: var Map; amount: float32; x0, y0: int) =
  if amount != amount or amount <= 0: return
  for y in 0 ..< m.h:
    for x in 0 ..< m.w:
      let i = y * m.w + x
      if m.v[i] <= 0: continue
      m.v[i] = clamp(m.v[i] + (hashNoise(int64(x0 + x), int64(y0 + y)) - 0.5'f32) * 2 * amount, 0'f32, 1'f32)

proc rgb3(c: Color): array[3, float32] = c.toRgb

proc offsetFor(angle, distance: float32): (float32, float32) =
  let a = degToRad(angle)
  (round(-cos(a) * distance), round(sin(a) * distance))

proc spreadSplit*(size, spread: float32): (float32, float32) = (round(size * spread), size * (1 - spread))

# ---------------------------------------------------------------- painting

proc sampleStops(stops: openArray[(float32, array[3, float32])]; t: float32): array[3, float32] =
  if stops.len == 0: return [t, t, t]
  if stops.len == 1: return stops[0][1]
  if t <= stops[0][0]: return stops[0][1]
  for i in 0 ..< stops.len - 1:
    if t <= stops[i + 1][0]:
      let k = if stops[i + 1][0] > stops[i][0]: (t - stops[i][0]) / (stops[i + 1][0] - stops[i][0]) else: 0'f32
      for c in 0..2: result[c] = stops[i][1][c] + (stops[i + 1][1][c] - stops[i][1][c]) * k
      return
  stops[^1][1]

type PreparedGradient = object
  color, opacity: seq[(float32, array[3, float32])]

proc prepare(g: Gradient): PreparedGradient =
  for (p, c) in g.stops: result.color.add (p, rgb3(c))
  for (p, a) in g.opacityStops: result.opacity.add (p, [a, a, a])

proc sample(g: PreparedGradient; t: float32): Px =
  let c = sampleStops(g.color, t)
  [c[0], c[1], c[2], (if g.opacity.len == 0: 1'f32 else: sampleStops(g.opacity, t)[0])]

proc sampleGradient*(g: Gradient; t: float32): Px = prepare(g).sample(t)

template paintWith(dst: var Buffer; m: Map; blend: BlendMode; opacity: float32; colorAt: untyped) =
  for i {.inject.} in 0 ..< dst.px.len:
    let a = m.v[i] * opacity
    if a <= 0: continue
    let c: Px = colorAt
    dst.px[i] = compositePs(blend, dst.px[i], [c[0], c[1], c[2], c[3] * a], 1)

proc paintColor(dst: var Buffer; m: Map; c: array[3, float32]; blend: BlendMode; opacity: float32) =
  paintWith(dst, m, blend, opacity, [c[0], c[1], c[2], 1'f32])

proc paintPattern(dst: var Buffer; m: Map; patterns: var PreparedPatterns; name, id: string; place: Placement; big: Rect; blend: BlendMode; opacity: float32) =
  let tile = patterns.get(id, name)
  if tile.isNil: return
  let w = big.width
  paintWith(dst, m, blend, opacity, (let (u, v) = place.map(float64(big.x0 + i mod w) + 0.5, float64(big.y0 + i div w) + 0.5); tile.sample(u, v)))

proc paintFx(dst: var Buffer; m: Map; p: FxPaint; shapeBounds: Rect; anchor: (float64, float64); big: Rect;
             blend: BlendMode; opacity: float32; patterns: var PreparedPatterns) =
  case p.kind
  of fpColor: paintColor(dst, m, rgb3(p.color), blend, opacity)
  of fpGradient:
    let g = p.gradient
    let prepared = prepare(g)
    let (angle, scale, offset) = gradientLayout(g.style, g.angle, g.scale, g.offset, shapeBounds)
    let w = big.width
    paintWith(dst, m, blend, opacity, prepared.sample(gradientT(g.style, angle, scale, g.reverse, offset, shapeBounds,
      float32(big.x0 + i mod w) + 0.5'f32, float32(big.y0 + i div w) + 0.5'f32)))
  of fpPattern:
    paintPattern(dst, m, patterns, p.name, p.id, anchored(anchor, true, (0'f32, 0'f32), p.scale, 0), big, blend, opacity)

proc glowGradientGain*(range0: float32): float32 =
  let r = if range0 == range0 and abs(range0) != Inf: clamp(range0, 0.01'f32, 1'f32) else: 1'f32
  1 / (r * r)

proc paintGlow(dst: var Buffer; m: Map; g: Glow; shapeBounds: Rect; anchor: (float64, float64); big: Rect; patterns: var PreparedPatterns) =
  if g.paint.kind != fpGradient:
    paintFx(dst, m, g.paint, shapeBounds, anchor, big, g.common.blend, g.common.opacity, patterns)
    return
  let prepared = prepare(g.paint.gradient)
  let gain = glowGradientGain(g.range)
  var strength = m
  for x in strength.v.mitems: x = min(x * gain, 1'f32)
  paintWith(dst, strength, g.common.blend, g.common.opacity, prepared.sample(1 - clamp(m.v[i], 0'f32, 1'f32)))

# ---------------------------------------------------------------- effect maps

proc shadowMap(shape: Map; s: Shadow; light: GlobalLight; inner: bool; origin: (int, int)): Map =
  let angle = if s.useGlobalLight: light.angle else: s.angle
  let (dx, dy) = offsetFor(angle, s.distance)
  let src = if inner: shape.invert else: shape
  var m = src.shifted(dx, dy, if inner: 1'f32 else: 0'f32)
  let (r, bw) = spreadSplit(s.size, s.spread)
  m = dilate(m, r)
  blur(m, bw)
  result = applyContour(m, s.contour)
  noise(result, s.noise, origin[0], origin[1])
  if inner:
    for i in 0 ..< result.v.len: result.v[i] *= shape.v[i]

proc glowMap(shape: Map; g: Glow; inner: bool; origin: (int, int)): Map =
  let src = if inner: (if g.source == gsEdge: shape.invert else: shape) else: shape
  var m: Map
  case g.technique
  of gtPrecise:
    let d = if inner: distInsideBy(shape, mEuclidean) else: distOutsideBy(src, mEuclidean)
    let solid = g.size * g.spread
    let soft = max(g.size - solid, 1e-3'f32)
    m = newMap(shape.w, shape.h)
    for i in 0 ..< m.v.len: m.v[i] = if d[i] <= solid: 1'f32 else: clamp(1 - (d[i] - solid) / soft, 0'f32, 1'f32)
    if inner and g.source == gsCenter: m = m.invert
  of gtSofter:
    let (r, bw) = spreadSplit(g.size, g.spread)
    m = dilate(src, r)
    blur(m, bw)
    if inner and g.source == gsCenter:
      var e = dilate(shape.invert, r)
      blur(e, bw)
      m = e.invert
  m = applyLut(m, rangedLut(g.contour, g.range))
  noise(m, g.noise, origin[0], origin[1])
  if inner:
    for i in 0 ..< m.v.len: m.v[i] *= shape.v[i]
  m

proc satinMap(shape: Map; s: Satin): Map =
  let (dx, dy) = offsetFor(s.angle, s.distance)
  var a = shape.shifted(dx, dy, 0)
  var b = shape.shifted(-dx, -dy, 0)
  tent(a, s.size)
  tent(b, s.size)
  var m = newMap(shape.w, shape.h)
  for i in 0 ..< m.v.len: m.v[i] = abs(a.v[i] - b.v[i])
  if s.invert: m = m.invert
  result = applyContour(m, s.contour)
  for i in 0 ..< result.v.len: result.v[i] *= shape.v[i]

proc bevelGeom*(b: Bevel): BevelGeom =
  let size = max(b.size, 1)
  let emboss = b.style in {bsEmboss, bsPillowEmboss}
  let paint = case b.style
    of bsOuterBevel: bpOuter
    of bsEmboss, bsPillowEmboss: bpBoth
    else: bpInner
  let width = if emboss: ceil(size / 2) else: size
  let smooth = b.technique == btSmooth
  BevelGeom(paint: paint, width: width, depth: b.depth * (if smooth: width else: size) * (if b.up: 1'f32 else: -1'f32),
            pillow: b.style == bsPillowEmboss, chiselSoft: (if b.technique == btChiselSoft: max(size / 4, 1) else: 0'f32))

proc bevelChiselH*(paint: BevelPaint; size, din, dout: float32): float32 =
  case paint
  of bpOuter: 1 - clamp(dout / size, 0'f32, 1'f32)
  of bpBoth:
    if din > 0: 0.5'f32 + 0.5'f32 * clamp(din / (size / 2), 0'f32, 1'f32) else: 0.5'f32 - 0.5'f32 * clamp(dout / (size / 2), 0'f32, 1'f32)
  of bpInner: clamp(din / size, 0'f32, 1'f32)

proc bevelHeight(shape: Map; b: Bevel; g: BevelGeom; tex: TextureCtx; patterns: var PreparedPatterns): Map =
  let size = max(b.size, 1)
  var h: Map
  if b.technique == btSmooth:
    h = shape
    tent(h, g.width)
  else:
    let sharp = sharpEdges(shape)
    let (din, dout) = (bevelDistInside(shape, sharp, size), bevelDistOutside(shape, sharp, size))
    h = newMap(shape.w, shape.h)
    for i in 0 ..< h.v.len: h.v[i] = bevelChiselH(g.paint, size, din[i], dout[i])
    if g.chiselSoft > 0: tent(h, g.chiselSoft)
  if b.contour.isSome:
    let l = contourLut(b.contour.get.contour)
    h = applyLut(h, if l.isSome: l else: some((var s = newSeq[float32](4096); for k in 0 ..< 4096: s[k] = float32(k) / 4095; s)))
  if b.texture.isSome:
    let t = b.texture.get
    let tile = patterns.get(t.id, t.name)
    if not tile.isNil:
      let unit = if abs(b.depth) > 1e-6'f32: max(abs(g.depth / b.depth), 1e-3'f32) else: max(g.width, 1)
      let place = anchored(tex.anchor, t.link, t.phase, t.scale, 0)
      let k = t.depth / unit
      for y in 0 ..< h.h:
        for x in 0 ..< h.w:
          let (u, v) = place.map(float64(tex.rect.x0 + x) + 0.5, float64(tex.rect.y0 + y) + 0.5)
          let p = tile.sample(u, v)
          let l = (0.299'f32 * p[0] + 0.587'f32 * p[1] + 0.114'f32 * p[2]) * p[3]
          h.v[y * h.w + x] += k * (if t.invert: 1 - l else: l)
  if b.soften >= 1: tent(h, b.soften)
  h

proc bevelMaps(shape: Map; b: Bevel; light: GlobalLight; tex: TextureCtx; patterns: var PreparedPatterns): (seq[Map], BevelPaint) =
  let g = bevelGeom(b)
  let hmap = bevelHeight(shape, b, g, tex, patterns)
  let (angle, altitude) = if b.useGlobalLight: (light.angle, light.altitude) else: (b.angle, b.altitude)
  let (sa, ca) = (sin(degToRad(angle)), cos(degToRad(angle)))
  let (se, ce) = (sin(degToRad(altitude)), cos(degToRad(altitude)))
  let lightV = [ca * ce, -sa * ce, se]
  let reachPast = g.paint == bpBoth
  proc onBevel(x, y: int): bool =
    if hmap.get(x, y) > BevelHEps: return true
    if reachPast:
      for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
        if hmap.get(x + dx, y + dy) > BevelHEps: return true
    false
  proc shadeInto(depth: float32; regionKind: int): (Map, Map) =
    var hi = newMap(shape.w, shape.h)
    var sh = newMap(shape.w, shape.h)
    for y in 0 ..< shape.h:
      for x in 0 ..< shape.w:
        let i = y * shape.w + x
        let gx = (hmap.get(x + 1, y) - hmap.get(x - 1, y)) * 0.5'f32 * depth
        let gy = (hmap.get(x, y + 1) - hmap.get(x, y - 1)) * 0.5'f32 * depth
        let ln = sqrt(gx * gx + gy * gy + 1)
        let shade = (-gx * lightV[0] - gy * lightV[1] + lightV[2]) / ln
        let region = if regionKind == 0: shape.v[i] else: float32(shape.v[i] < 1 - InsideEps and onBevel(x, y))
        let k = shade - se
        if k > 0: hi.v[i] = clamp(k / max(1 - se, 1e-3'f32), 0'f32, 1'f32) * region
        else: sh.v[i] = clamp(-k / max(se, 1e-3'f32), 0'f32, 1'f32) * region
    (applyContour(hi, b.glossContour), applyContour(sh, b.glossContour))
  case g.paint
  of bpInner, bpOuter:
    let (hi, sh) = shadeInto(g.depth, int(g.paint == bpOuter))
    (@[hi, sh], g.paint)
  of bpBoth:
    let (hi, sh) = shadeInto(g.depth, 0)
    let (ho, so) = shadeInto(if g.pillow: -g.depth else: g.depth, 1)
    proc mix(a, b2: Map): Map =
      result = newMap(a.w, a.h)
      for i in 0 ..< a.v.len:
        let s = shape.v[i]
        result.v[i] = (if s > InsideEps: a.v[i] else: 0'f32) + b2.v[i] * (1 - min(s, 1'f32))
    (@[mix(hi, ho), mix(sh, so)], g.paint)

proc strokeWidths*(st: StrokeFx): (float32, float32) =
  case st.position
  of spOutside: (0'f32, st.size)
  of spInside: (st.size, 0'f32)
  of spCenter: (st.size / 2, st.size / 2)

proc strokeFrameOf(shape: Map; outW: float32; rc: Rect): Rect =
  var (x0, y0, x1, y1) = (high(int), high(int), 0, 0)
  for y in 0 ..< shape.h:
    for x in 0 ..< shape.w:
      if shape.v[y * shape.w + x] > InsideEps:
        x0 = min(x0, x); y0 = min(y0, y); x1 = max(x1, x + 1); y1 = max(y1, y + 1)
  if x0 == high(int): return rc
  let grow = if outW == outW and abs(outW) != Inf: max(int(ceil(min(outW, MaxReach))) - 1, 0) else: 0
  rect(rc.x0 + x0, rc.y0 + y0, rc.x0 + x1, rc.y0 + y1).inflate(grow)

proc strokeFrame*(maps: FxMaps; st: StrokeFx): Option[Rect] =
  let ow = strokeWidths(st)[1]
  for (k, r) in maps.frames:
    if k == ow: return some(r)

proc bytes*(maps: FxMaps): int =
  var n = 1
  for p in maps.per: n += p.len
  n += int(maps.din.isSome) + int(maps.dout.isSome) + int(maps.vdout.isSome)
  n * maps.shape.v.len * 4

proc buildMaps*(layer: Layer; shape: seq[float32]; rc: Rect; light: GlobalLight; tex: TextureCtx;
                patterns: var PreparedPatterns): FxMaps =
  ## The effect maps of `layer` over `rc` from its shape coverage (row-major over `rc`).
  let (w, h) = (rc.width, rc.height)
  let sh = Map(w: w, h: h, v: shape)
  var items: seq[Effect]
  for e in layer.effects.items:
    if e.enabled: items.add e
  result = FxMaps(rect: rc, shape: sh, bevelPaint: newSeq[BevelPaint](items.len))
  for i, e in items:
    case e.kind
    of ekDropShadow: result.per.add @[shadowMap(sh, e.shadow, light, false, (rc.x0, rc.y0))]
    of ekInnerShadow: result.per.add @[shadowMap(sh, e.shadow, light, true, (rc.x0, rc.y0))]
    of ekOuterGlow: result.per.add @[glowMap(sh, e.glow, false, (rc.x0, rc.y0))]
    of ekInnerGlow: result.per.add @[glowMap(sh, e.glow, true, (rc.x0, rc.y0))]
    of ekSatin: result.per.add @[satinMap(sh, e.satin)]
    of ekBevelEmboss:
      let (maps, paint) = bevelMaps(sh, e.bevel, light, tex, patterns)
      result.bevelPaint[i] = paint
      result.per.add maps
    else: result.per.add newSeq[Map]()
  var hasStroke = false
  for e in items:
    if e.kind == ekStroke: hasStroke = true
  let vectorShape = layer.content.kind == lkShape
  if hasStroke:
    result.din = some(distInsideBy(sh, mChamfer))
    result.dout = some(distOutsideBy(sh, mChamfer))
  result.outline = effectOutline(layer).isSome
  if hasStroke and vectorShape and not result.outline: result.vdout = some(distOutsideBy(localCoverage(sh), mChamfer))
  for e in items:
    if e.kind == ekStroke and e.stroke.paint.kind == fpGradient:
      let ow = strokeWidths(e.stroke)[1]
      var seen = false
      for (k, _) in result.frames:
        if k == ow: seen = true
      if not seen: result.frames.add (ow, strokeFrameOf(sh, ow, rc))

proc cropVec(maps: FxMaps; v: seq[float32]; to: Rect; fill: float32): seq[float32] =
  let (w, h) = (to.width, to.height)
  result = newSeq[float32](w * h)
  if fill != 0:
    for x in result.mitems: x = fill
  let r = maps.rect
  let inter = r.intersect(to)
  if inter.isEmpty: return
  let sw = r.width
  let n = inter.width
  for y in inter.y0 ..< inter.y1:
    let s0 = (y - r.y0) * sw + (inter.x0 - r.x0)
    let d0 = (y - to.y0) * w + (inter.x0 - to.x0)
    for k in 0 ..< n: result[d0 + k] = v[s0 + k]

proc crop(maps: FxMaps; m: Map; to: Rect; fill: float32): Map =
  Map(w: to.width, h: to.height, v: maps.cropVec(m.v, to, fill))

proc knockOutExterior(m: var Map; shape: Map; fillOpacity: float32) =
  let seeThrough = 1 - clamp(fillOpacity, 0'f32, 1'f32)
  for i in 0 ..< m.v.len: m.v[i] *= 1 - shape.v[i] * seeThrough

proc outlineShare*(cov, l: float32): float32 =
  let outside = max(1 - clamp(cov, 0'f32, 1'f32), 0'f32)
  let rest = 1 - clamp(l, 0'f32, 1'f32)
  if rest <= 1e-6'f32: 1 else: min(outside / rest, 1'f32)

proc mixPremul(a, b: Px; k: float32): Px =
  let alpha = a[3] + (b[3] - a[3]) * k
  if alpha <= 0: return
  for c in 0..2: result[c] = (a[c] * a[3] + (b[c] * b[3] - a[c] * a[3]) * k) / alpha
  result[3] = alpha

proc compositeWithEffects*(layer: Layer; content: Buffer; backdrop: var Buffer; maps: FxMaps; layerBounds: Rect;
                           patterns: var PreparedPatterns; vstroke: Option[VectorStroke] = none(VectorStroke)) =
  ## Composites `content` (the layer's rendered pixels over `content.rect`, the effect region)
  ## with its effects onto `backdrop`, in Photoshop's effect order.
  let big = content.rect
  let (w, h) = (big.width, big.height)
  template kmask(i: int): float32 =
    (if vstroke.isSome and vstroke.get.mask.isSome and i < vstroke.get.mask.get.len: vstroke.get.mask.get[i] else: 1'f32)
  template unionA(i: int; a: float32): float32 =
    (if vstroke.isSome and i < vstroke.get.stroke.px.len: kmask(i) * (a + vstroke.get.stroke.px[i][3] * (1 - a)) else: a)
  let shapeless = not layer.advanced.transparencyShapes
  var shape: Map
  if shapeless: shape = maps.crop(maps.shape, big, 0)
  elif maps.outline:
    shape = maps.crop(maps.shape, big, 0)
    for i in 0 ..< shape.v.len: shape.v[i] = max(shape.v[i], unionA(i, content.px[i][3]))
  else:
    shape = newMap(w, h)
    for i in 0 ..< shape.v.len: shape.v[i] = unionA(i, content.px[i][3])
  let relative = shapeless or maps.outline or vstroke.isSome
  template fx(i, k: int): Map = maps.crop(maps.per[i][k], big, 0)
  let sb = layerBounds
  let anchor = if layer.effects.reference.isSome: layer.effects.reference.get else: (float64(sb.x0), float64(sb.y0))
  let rc = backdrop.rect
  let before = backdrop
  var work = transparentBuffer(big)
  for y in max(rc.y0, big.y0) ..< min(rc.y1, big.y1):
    for x in max(rc.x0, big.x0) ..< min(rc.x1, big.x1):
      work.px[(y - big.y0) * w + (x - big.x0)] = before.get(x, y)
  var items: seq[Effect]
  for e in layer.effects.items:
    if e.enabled: items.add e
  let vectorShape = layer.content.kind == lkShape
  for i in countdown(items.high, 0):
    let e = items[i]
    if e.kind == ekDropShadow:
      var m = fx(i, 0)
      if e.shadow.knocksOut: knockOutExterior(m, shape, layer.fillOpacity)
      paintColor(work, m, rgb3(e.shadow.color), e.shadow.common.blend, e.shadow.common.opacity)
  for i in countdown(items.high, 0):
    let e = items[i]
    if e.kind == ekOuterGlow:
      var m = fx(i, 0)
      knockOutExterior(m, shape, layer.fillOpacity)
      paintGlow(work, m, e.glow, sb, anchor, big, patterns)
  let fill = layer.fillOpacity
  let interiorGroup = layer.advanced.blendInterior and fill < 1
  let contentFill = if interiorGroup: 1'f32 else: fill
  template inside(a: float32): bool = a > InsideEps
  var lay = transparentBuffer(big)
  for i, p in content.px:
    let a = shape.v[i]
    let la = if not inside(a): 0'f32 elif relative: contentFill * min(kmask(i) * p[3] / a, 1'f32) else: contentFill
    lay.px[i] = [p[0], p[1], p[2], la]
  proc rel(m: Map): Map =
    result = m
    for i in 0 ..< m.v.len: result.v[i] = if shape.v[i] > InsideEps: min(m.v[i] / shape.v[i], 1'f32) else: 0'f32
  var full = newMap(w, h)
  for i in 0 ..< full.v.len: full.v[i] = if inside(shape.v[i]): 1'f32 else: 0'f32
  for i in countdown(items.high, 0):
    let e = items[i]
    if e.kind == ekPatternOverlay:
      paintPattern(lay, full, patterns, e.name, e.id, anchored(anchor, e.link, e.phase, e.scale, e.angle), big, e.common.blend, e.common.opacity)
  for i in countdown(items.high, 0):
    let e = items[i]
    if e.kind == ekGradientOverlay:
      paintFx(lay, full, FxPaint(kind: fpGradient, gradient: e.gradient), sb, anchor, big, e.common.blend, e.common.opacity, patterns)
  for i in countdown(items.high, 0):
    let e = items[i]
    if e.kind == ekColorOverlay: paintColor(lay, full, rgb3(e.color), e.common.blend, e.common.opacity)
  for i in countdown(items.high, 0):
    let e = items[i]
    if e.kind == ekSatin: paintColor(lay, rel(fx(i, 0)), rgb3(e.satin.color), e.satin.common.blend, e.satin.common.opacity)
  for i in countdown(items.high, 0):
    let e = items[i]
    if e.kind == ekInnerGlow: paintGlow(lay, rel(fx(i, 0)), e.glow, sb, anchor, big, patterns)
  if interiorGroup:
    for p in lay.px.mitems: p[3] *= max(fill, 0)
  for i in countdown(items.high, 0):
    let e = items[i]
    if e.kind == ekInnerShadow:
      paintColor(lay, rel(fx(i, 0)), rgb3(e.shadow.color), e.shadow.common.blend, e.shadow.common.opacity)
  if vstroke.isSome:
    let vs = vstroke.get
    for i in 0 ..< lay.px.len:
      let s = vs.stroke.px[i]
      let a = shape.v[i]
      if s[3] > 0 and inside(a):
        lay.px[i] = compositePs(bmNormal, lay.px[i], [s[0], s[1], s[2], fill * min(kmask(i) * s[3] / a, 1'f32)], 1)
  var strokes: seq[StrokeFx]
  for e in items:
    if e.kind == ekStroke: strokes.add e.stroke
  var din, dout: seq[float32]
  if strokes.len > 0 and maps.din.isSome and maps.dout.isSome:
    din = maps.cropVec(maps.din.get, big, 0)
    dout = maps.cropVec(maps.dout.get, big, Far)
  proc frame(st: StrokeFx): Rect =
    let f = maps.strokeFrame(st)
    if f.isSome: f.get else: sb
  for si in countdown(strokes.high, 0):
    let st = strokes[si]
    let inW = strokeWidths(st)[0]
    if inW <= 0 or din.len == 0: continue
    var m = newMap(w, h)
    for i in 0 ..< m.v.len: m.v[i] = if inside(shape.v[i]): clamp(inW + 0.5'f32 - din[i], 0'f32, 1'f32) else: 0'f32
    paintFx(lay, m, st.paint, frame(st), anchor, big, st.common.blend, st.common.opacity, patterns)
  for i in countdown(items.high, 0):
    let e = items[i]
    if e.kind == ekBevelEmboss and maps.bevelPaint[i] == bpInner:
      paintColor(lay, rel(fx(i, 0)), rgb3(e.bevel.highlightColor), e.bevel.highlight.blend, e.bevel.highlight.opacity)
      paintColor(lay, rel(fx(i, 1)), rgb3(e.bevel.shadowColor), e.bevel.shadow.blend, e.bevel.shadow.opacity)
  for i in 0 ..< lay.px.len: lay.px[i][3] *= min(shape.v[i], 1'f32)
  var anyOut = false
  for st in strokes:
    if strokeWidths(st)[1] > 0: anyOut = true
  if anyOut and dout.len > 0:
    let vd = if maps.vdout.isSome and vectorShape: maps.cropVec(maps.vdout.get, big, Far) else: dout
    let base = work
    var acc = newSeq[Px](w * h)
    var cover = newSeq[float32](w * h)
    for st in strokes:
      let outW = strokeWidths(st)[1]
      if outW <= 0: continue
      var share = newSeq[float32](w * h)
      for i in 0 ..< share.len:
        let band = clamp(outW + 0.5'f32 - vd[i], 0'f32, 1'f32)
        let k = if maps.outline: band * outlineShare(shape.v[i], lay.px[i][3])
                elif inside(shape.v[i]):
                  (if vectorShape: 0'f32
                   else:
                     let c = clamp(lay.px[i][3], 0'f32, 1'f32)
                     if c >= 1: 1'f32 else: clamp((1 - clamp(shape.v[i], 0'f32, 1'f32)) / (1 - c), 0'f32, 1'f32))
                else: band
        share[i] = k * st.common.opacity * (1 - cover[i])
        cover[i] += share[i]
      var blended = base
      var ones = newMap(w, h)
      for i in 0 ..< ones.v.len: ones.v[i] = if share[i] > 0: 1'f32 else: 0'f32
      paintFx(blended, ones, st.paint, frame(st), anchor, big, st.common.blend, 1, patterns)
      for i in 0 ..< acc.len:
        let s = share[i]
        if s > 0:
          let b = blended.px[i]
          for c in 0..2: acc[i][c] += s * b[c] * b[3]
          acc[i][3] += s * b[3]
    for i in 0 ..< work.px.len:
      let c = cover[i]
      if c <= 0: continue
      let p = work.px[i]
      let a = acc[i]
      let k = 1 - min(c, 1'f32)
      let alpha = p[3] * k + a[3]
      if alpha > 0:
        work.px[i] = [(p[0] * p[3] * k + a[0]) / alpha, (p[1] * p[3] * k + a[1]) / alpha, (p[2] * p[3] * k + a[2]) / alpha, min(alpha, 1'f32)]
  for i in countdown(items.high, 0):
    let e = items[i]
    if e.kind == ekBevelEmboss and maps.bevelPaint[i] == bpOuter:
      paintColor(work, fx(i, 0), rgb3(e.bevel.highlightColor), e.bevel.highlight.blend, e.bevel.highlight.opacity)
      paintColor(work, fx(i, 1), rgb3(e.bevel.shadowColor), e.bevel.shadow.blend, e.bevel.shadow.opacity)
  let mode = if layer.blend == bmPassThrough: bmNormal else: layer.blend
  let gamma = layerTextGamma(layer)
  for i in 0 ..< work.px.len:
    if lay.px[i][3] > 0: work.px[i] = compositeGamma(mode, work.px[i], lay.px[i], 1, gamma)
  for i in countdown(items.high, 0):
    let e = items[i]
    if e.kind == ekBevelEmboss and maps.bevelPaint[i] == bpBoth:
      paintColor(work, fx(i, 0), rgb3(e.bevel.highlightColor), e.bevel.highlight.blend, e.bevel.highlight.opacity)
      paintColor(work, fx(i, 1), rgb3(e.bevel.shadowColor), e.bevel.shadow.blend, e.bevel.shadow.opacity)
  let op = layer.opacity
  for y in rc.y0 ..< rc.y1:
    for x in rc.x0 ..< rc.x1:
      let i = (y - rc.y0) * rc.width + (x - rc.x0)
      let a = before.px[i]
      let b = if x >= big.x0 and x < big.x1 and y >= big.y0 and y < big.y1: work.px[(y - big.y0) * w + (x - big.x0)] else: a
      backdrop.px[i] = mixPremul(a, b, op)
