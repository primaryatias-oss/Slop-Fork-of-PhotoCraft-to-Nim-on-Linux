## Magnetic Lasso edge tracing: a selection border that snaps to the strongest edges near the
## pointer (port of `magnetic.rs`).
##
## Between two fastening points the border is the cheapest 8-connected pixel path, where pixels
## on a strong, well-localised edge are cheap and the rest are expensive: the "live wire" of
## Mortensen & Barrett, *Intelligent Scissors for Image Composition* (SIGGRAPH 1995). A link costs
## a weighted sum of an edge-strength term, an edge-centre term (non-maximum suppression across
## the edge, in place of the paper's Laplacian zero crossings) and a gradient-direction term that
## keeps the border running along the edge. The search stays in a corridor, the pixels within the
## detection width of the pointer's path, and a small pull towards that path keeps the border on
## the pointer where there is no edge. Points on an edge are placed at its sub-pixel centre.
##
## Pixels are read lazily in blocks through a caller-supplied fetch (a layer's pixels or the
## composite), so tracing on a 36 MP image reads only the area around the border.

import std/[math, options, tables, heapqueue]
import ../geom
import ./util

const
  Block = 128          ## side of the cached pixel blocks (px)
  MaxBlocks = 1024     ## blocks kept before the cache starts over
  Piece = 256.0        ## longest stretch of the pointer's path searched at once (px)
  MaxGuideLength* = 262_144.0 ## longest pointer path `trace` follows (px)
  WidthRange* = (1.0, 256.0)  ## detection width limits (px), as in Photoshop's options bar
  Dirs = [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]
    ## the 8 neighbours, in the order the search records them

# Link cost weights: Mortensen & Barrett's 0.43 (edge centre) / 0.43 (strength) / 0.14
# (direction), a cost per pixel of length and the pull towards the pointer's path. Module-level
# `let`s so float32 arithmetic on them is not folded in float64.
let
  WCentre = 0.43'f32
  WStrength = 0.43'f32
  WDirection = 0.14'f32
  WLength = 0.02'f32
  WGuide = 0.4'f32
  NeutralDirection = 2'f32 / 3'f32 ## the direction term where either pixel has no edge direction
  Sqrt2F = float32(sqrt(2.0))
  Pi32 = float32(PI)
  DirScale = 2'f32 / (3'f32 * Pi32)

type
  Settings* = object
    ## Detection settings: the Magnetic Lasso options bar.
    width*: float64    ## detection width (px): the border follows only edges this close to the path
    contrast*: float32 ## contrast (0..1): steps weaker than this, in every channel, are not edges

  Fetch* = proc(r: Rect): seq[array[4, uint8]]
    ## Reads straight-alpha RGBA8 pixels of a rectangle inside the image, row-major. A result of
    ## the wrong length reads as transparent.

  Tracer* = object
    ## Edge tracing over one image, caching the pixels it has read.
    bounds: Rect
    blocks: Table[(int, int), seq[array[4, uint8]]]

  Field = object
    ## Edge features of the pixels in an area.
    area: Rect
    w: int
    strength: seq[float32]       ## steepest channel's Sobel gradient, a step of height h reads h
    grad: seq[array[2, float32]] ## unit gradient of that channel (zero on flat pixels)
    ridge: seq[bool]             ## strongest across its edge (non-maximum suppression)
    offset: seq[array[2, float32]] ## from the pixel centre to the edge's sub-pixel centre

proc defaultSettings*(): Settings = Settings(width: 10.0, contrast: 0.1'f32)

proc isFinite(v: float64): bool {.inline.} = v == v and v - v == 0.0
proc isFinite(v: float32): bool {.inline.} = v == v and v - v == 0'f32

proc newSettings*(width: float64; contrast: float32): Settings =
  ## Settings clamped to their ranges (width 1..256 px, contrast 1..100 %); NaN takes the default.
  let d = defaultSettings()
  Settings(width: (if width.isFinite: clamp(width, WidthRange[0], WidthRange[1]) else: d.width),
           contrast: (if contrast.isFinite: clamp(contrast, 0.01'f32, 1'f32) else: d.contrast))

proc radius(s: Settings): float64 = s.width + 1.5
  ## The corridor's radius: the detection width plus room for an 8-connected path.

proc newTracer*(bounds: Rect): Tracer =
  ## A tracer for an image covering `bounds` (document pixels).
  Tracer(bounds: bounds)

proc bounds*(t: Tracer): Rect = t.bounds

proc cachedPixels*(t: Tracer): int =
  ## Pixels read so far (for tests and diagnostics).
  for v in t.blocks.values: result += v.len

# ---------------------------------------------------------------- geometry helpers

proc dist(a, b: array[2, float64]): float64 = hypot(a[0] - b[0], a[1] - b[1])

proc dist2Segment(p, a, b: array[2, float64]): float64 =
  let (vx, vy) = (b[0] - a[0], b[1] - a[1])
  let l2 = vx * vx + vy * vy
  let t = (if l2 > 0.0: clamp(((p[0] - a[0]) * vx + (p[1] - a[1]) * vy) / l2, 0.0, 1.0) else: 0.0)
  let (dx, dy) = (p[0] - (a[0] + t * vx), p[1] - (a[1] + t * vy))
  dx * dx + dy * dy

proc simplify*(p: openArray[array[2, float64]]; tol: float64): seq[array[2, float64]] =
  ## Ramer–Douglas–Peucker: drops points within `tol` of the line through their neighbours that
  ## stay, keeping both ends.
  let n = p.len
  if n < 3: return @p
  var keep = newSeq[bool](n)
  keep[0] = true
  keep[n - 1] = true
  let tol2 = tol * tol
  var stack = @[(0, n - 1)]
  while stack.len > 0:
    let (a, b) = stack.pop()
    if b <= a + 1: continue
    let (pa, pb) = (p[a], p[b])
    var far = (0.0, a)
    for k in a + 1 ..< b:
      let d2 = dist2Segment(p[k], pa, pb)
      if d2 > far[0]: far = (d2, k)
    if far[0] > tol2:
      keep[far[1]] = true
      stack.add (a, far[1])
      stack.add (far[1], b)
  for k in 0 ..< n:
    if keep[k]: result.add p[k]

proc length*(p: openArray[array[2, float64]]): float64 =
  ## Length of a polyline.
  result = -0.0
  for k in 1 ..< p.len: result += dist(p[k - 1], p[k])

proc split(g: seq[array[2, float64]]; piece: float64): seq[seq[array[2, float64]]] =
  ## The polyline `g` cut into pieces at most `piece` long; each piece starts where the last ended.
  if g.len == 0: return @[]
  var cur = @[g[0]]
  var acc = 0.0
  for k in 1 ..< g.len:
    var a = g[k - 1]
    let b = g[k]
    var seg = dist(a, b)
    while acc + seg > piece and seg > 0.0:
      let t = clamp((piece - acc) / seg, 0.0, 1.0)
      let m = [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t]
      cur.add m
      result.add cur
      cur = @[m]
      a = m
      seg = dist(a, b)
      acc = 0.0
    cur.add b
    acc += seg
  result.add cur

# ---------------------------------------------------------------- field

proc newField(area, ext: Rect; px: seq[array[4, uint8]]): Field =
  ## Features over `area` from the straight RGBA8 pixels `px` of `ext` (the area plus up to two
  ## pixels around it, inside the image; beyond it the edge pixels repeat).
  let (ew, eh) = (ext.width, ext.height)
  # Premultiplied channels: an opaque shape on transparency has an edge, transparent pixels of
  # different colours do not.
  var chs = newSeq[array[4, float32]](px.len)
  for i, p in px:
    let a = float32(p[3]) / 255'f32
    chs[i] = [float32(p[0]) / 255'f32 * a, float32(p[1]) / 255'f32 * a, float32(p[2]) / 255'f32 * a, a]
  template at(xq, yq: int): array[4, float32] =
    block:
      let xc = clamp(xq - ext.x0, 0, max(ew - 1, 0))
      let yc = clamp(yq - ext.y0, 0, max(eh - 1, 0))
      let k = yc * max(ew, 0) + xc
      (if k < chs.len: chs[k] else: [0'f32, 0, 0, 0])
  # Gradients over the area plus one ring (non-maximum suppression looks across it).
  let g1 = area.inflate(1).intersect(ext)
  let gw = g1.width
  var st = newSeq[float32](gw * g1.height)
  var gd = newSeq[array[2, float32]](st.len)
  for y in g1.y0 ..< g1.y1:
    for x in g1.x0 ..< g1.x1:
      let (nw, n, ne) = (at(x - 1, y - 1), at(x, y - 1), at(x + 1, y - 1))
      let (w, e) = (at(x - 1, y), at(x + 1, y))
      let (sw, s, se) = (at(x - 1, y + 1), at(x, y + 1), at(x + 1, y + 1))
      var best = (0'f32, [0'f32, 0'f32])
      for c in 0 .. 3:
        let gx = (ne[c] + 2'f32 * e[c] + se[c]) - (nw[c] + 2'f32 * w[c] + sw[c])
        let gy = (sw[c] + 2'f32 * s[c] + se[c]) - (nw[c] + 2'f32 * n[c] + ne[c])
        let m2 = gx * gx + gy * gy
        if m2 > best[0]: best = (m2, [gx, gy])
      let mag = sqrt(best[0])
      let i = (y - g1.y0) * gw + (x - g1.x0)
      if i < st.len:
        st[i] = rmin(mag / 4'f32, 1'f32)
        gd[i] = (if mag > 1e-6'f32: [best[1][0] / mag, best[1][1] / mag] else: [0'f32, 0'f32])
  template sAt(xq, yq: int): float32 =
    (if g1.contains(xq, yq) and (yq - g1.y0) * gw + (xq - g1.x0) < st.len: st[(yq - g1.y0) * gw + (xq - g1.x0)] else: 0'f32)
  let w = area.width
  let n = w * area.height
  result = Field(area: area, w: w, strength: newSeq[float32](n), grad: newSeq[array[2, float32]](n),
                 ridge: newSeq[bool](n), offset: newSeq[array[2, float32]](n))
  for y in area.y0 ..< area.y1:
    for x in area.x0 ..< area.x1:
      let s0 = sAt(x, y)
      let gi = (y - g1.y0) * gw + (x - g1.x0)
      let g = (if g1.contains(x, y) and gi < gd.len: gd[gi] else: [0'f32, 0'f32])
      let i = (y - area.y0) * w + (x - area.x0)
      result.strength[i] = s0
      result.grad[i] = g
      if g[0] == 0'f32 and g[1] == 0'f32: continue
      # The neighbours across the edge: the gradient rounded to one of 8 directions.
      proc q(v, o: float32): int = (if abs(v) >= 0.4142'f32 * abs(o): int(signum(v)) else: 0)
      let (dx, dy) = (q(g[0], g[1]), q(g[1], g[0]))
      let (sm, sp) = (sAt(x - dx, y - dy), sAt(x + dx, y + dy))
      if s0 > 0'f32 and s0 >= sm and s0 >= sp:
        result.ridge[i] = true
        # The peak of the parabola through the three strengths.
        let denom = sm - 2'f32 * s0 + sp
        let t = (if denom < -1e-6'f32: clamp(0.5'f32 * (sm - sp) / denom, -0.5'f32, 0.5'f32) else: 0'f32)
        result.offset[i] = [t * float32(dx), t * float32(dy)]

proc h(f: Field): int = f.strength.len div max(f.w, 1)

proc index(f: Field; x, y: int): Option[int] =
  if f.area.contains(x, y): some((y - f.area.y0) * f.w + (x - f.area.x0)) else: none(int)

proc point(f: Field; i, x, y: int; contrast: float32): array[2, float64] =
  ## Where the border runs through pixel `i` at (`x`, `y`): the edge's sub-pixel centre on an edge
  ## at least `contrast` strong, else the pixel centre.
  let onEdge = i < f.ridge.len and f.ridge[i] and f.strength[i] >= contrast
  let o = (if onEdge: f.offset[i] else: [0'f32, 0'f32])
  [float64(x) + 0.5 + float64(o[0]), float64(y) + 0.5 + float64(o[1])]

proc pointAt(f: Field; i: int; contrast: float32): array[2, float64] =
  let (x, y) = (i mod max(f.w, 1) + f.area.x0, i div max(f.w, 1) + f.area.y0)
  f.point(i, x, y, contrast)

# ---------------------------------------------------------------- tracer internals

proc clampPoint(t: Tracer; p: array[2, float64]): Option[array[2, float64]] =
  ## `p` kept inside the image, or none if it is not a finite point or the image is empty.
  let b = t.bounds
  if b.isEmpty or not p[0].isFinite or not p[1].isFinite: return none(array[2, float64])
  some([clamp(p[0], float64(b.x0), float64(b.x1)), clamp(p[1], float64(b.y0), float64(b.y1))])

proc pixel(t: Tracer; p: array[2, float64]): (int, int) =
  ## The pixel under a point (clamped to the image).
  let b = t.bounds
  proc c(v: float64; lo, hi: int): int =
    if v.isFinite: clamp(asI32(floor(v)), lo, max(hi - 1, lo)) else: lo
  (c(p[0], b.x0, b.x1), c(p[1], b.y0, b.y1))

proc satMul32(a, b: int): int = max(int(int32.low), min(int(int32.high), a * b))

proc pixels(t: var Tracer; fetch: Fetch; r: Rect): seq[array[4, uint8]] =
  ## Straight RGBA8 pixels of `r` (inside the image), through the block cache.
  let w = r.width
  result = newSeq[array[4, uint8]](w * r.height)
  if r.isEmpty: return
  if t.blocks.len > MaxBlocks: t.blocks.clear()
  let (bx0, by0) = (divEuclid(r.x0, Block), divEuclid(r.y0, Block))
  let (bx1, by1) = (divEuclid(r.x1 - 1, Block), divEuclid(r.y1 - 1, Block))
  for by in by0 .. by1:
    for bx in bx0 .. bx1:
      let br = rect(satMul32(bx, Block), satMul32(by, Block), satMul32(satAdd(bx, 1), Block),
                    satMul32(satAdd(by, 1), Block)).intersect(t.bounds)
      if br.isEmpty: continue
      let n = br.width * br.height
      if (bx, by) notin t.blocks:
        let v = (if fetch.isNil: @[] else: fetch(br))
        t.blocks[(bx, by)] = (if v.len == n: v else: newSeq[array[4, uint8]](n))
      let ov = br.intersect(r)
      let len = ov.width
      t.blocks.withValue((bx, by), blk):
        for y in ov.y0 ..< ov.y1:
          let src = (y - br.y0) * br.width + (ov.x0 - br.x0)
          let dst = (y - r.y0) * w + (ov.x0 - r.x0)
          if src + len <= blk[].len and dst + len <= result.len:
            for k in 0 ..< len: result[dst + k] = blk[][src + k]

proc field(t: var Tracer; fetch: Fetch; area: Rect): Field =
  ## Edge features over `area` (inside the image).
  # Sobel reads one ring beyond the gradient, and non-maximum suppression one beyond the area.
  let ext = area.inflate(2).intersect(t.bounds)
  let px = t.pixels(fetch, ext)
  newField(area, ext, px)

proc corridor(area: Rect; guide: openArray[array[2, float64]]; rad: float64): seq[float32] =
  ## Squared distance from each pixel centre of `area` to the polyline `guide`, infinite beyond
  ## `rad`: the corridor the border may use.
  let w = area.width
  result = newSeq[float32](w * area.height)
  for v in result.mitems: v = Inf.float32
  let r2 = rad * rad
  var segs = @guide
  if guide.len == 1: segs = @[guide[0], guide[0]]
  for k in 1 ..< segs.len:
    let (a, b) = (segs[k - 1], segs[k])
    proc lo(u, v: float64): int = asI32(floor(min(u, v) - rad))
    proc hi(u, v: float64): int = asI32(ceil(max(u, v) + rad)) + 1
    let r = rect(lo(a[0], b[0]), lo(a[1], b[1]), hi(a[0], b[0]), hi(a[1], b[1])).intersect(area)
    for y in r.y0 ..< r.y1:
      for x in r.x0 ..< r.x1:
        let d2 = dist2Segment([float64(x) + 0.5, float64(y) + 0.5], a, b)
        if d2 <= r2:
          let i = (y - area.y0) * w + (x - area.x0)
          if i < result.len: result[i] = rmin(result[i], float32(d2))

proc directionCost(tp, tq: array[2, float32]; dx, dy: int; len: float32): float32 =
  ## The gradient-direction term of a link from a pixel with edge tangent `tp` to its neighbour at
  ## (`dx`, `dy`) with tangent `tq`: 0 running straight along an edge, up to 1 cutting across it
  ## or turning sharply (Mortensen & Barrett's f_D).
  if (tp[0] == 0'f32 and tp[1] == 0'f32) or (tq[0] == 0'f32 and tq[1] == 0'f32): return NeutralDirection
  var l = [float32(dx) / len, float32(dy) / len]
  if tp[0] * l[0] + tp[1] * l[1] < 0'f32: l = [-l[0], -l[1]]
  let dp = clamp(tp[0] * l[0] + tp[1] * l[1], -1'f32, 1'f32)
  let dq = clamp(l[0] * tq[0] + l[1] * tq[1], -1'f32, 1'f32)
  DirScale * (arccos(dp) + arccos(dq))

proc search(f: Field; near: seq[float32]; rad2: float32; start, goal: int; contrast: float32): Option[seq[int]] =
  ## The cheapest 8-connected path from `start` to `goal` through the corridor `near` (squared
  ## distances to the guide, `rad2` at its rim), as field indices from `start` to `goal`.
  let (w, h) = (f.w, f.h)
  let n = w * h
  # Cost of entering each pixel, per pixel of length, and its edge tangent.
  var node = newSeq[float32](n)
  var tangent = newSeq[array[2, float32]](n)
  for i in 0 ..< n:
    node[i] = Inf.float32
    let d2 = (if i < near.len: near[i] else: Inf.float32)
    if not d2.isFinite: continue
    let s = (if i < f.strength.len: f.strength[i] else: 0'f32)
    let onEdge = s >= contrast and i < f.ridge.len and f.ridge[i]
    # No pull below the contrast, full pull from twice the contrast, a little more for stronger
    # edges still.
    let good = clamp((s - contrast) / rmax(contrast, 0.02'f32), 0'f32, 1'f32) * (0.6'f32 + 0.4'f32 * rmin(s, 1'f32))
    # Grows linearly away from the pointer's path, so the border hugs it where there is no edge.
    let pull = rmin(sqrt(d2 / rmax(rad2, 1'f32)), 1'f32)
    let centre = (if onEdge: 0'f32 else: 1'f32)
    node[i] = WCentre * centre + WStrength * (1'f32 - good) + WLength + WGuide * pull
    if s >= 0.5'f32 * contrast and i < f.grad.len:
      let g = f.grad[i]
      tangent[i] = [g[1], -g[0]]
  var dist = newSeq[float32](n)
  for d in dist.mitems: d = Inf.float32
  var `from` = newSeq[uint8](n)
  for v in `from`.mitems: v = high(uint8)
  if start < 0 or start >= n: return none(seq[int])
  dist[start] = 0
  var heap = initHeapQueue[(uint32, int)]()
  # Costs are finite and non-negative, so their bit patterns sort like the values.
  heap.push((cast[uint32](0'f32), start))
  while heap.len > 0:
    let (bits, i) = heap.pop()
    let d = cast[float32](bits)
    if d > (if i < dist.len: dist[i] else: Inf.float32): continue
    if i == goal: break
    let (x, y) = (i mod w, i div w)
    let tp = tangent[i]
    for k, (dx, dy) in Dirs:
      let (nx, ny) = (x + dx, y + dy)
      if nx < 0 or ny < 0 or nx >= w or ny >= h: continue
      let j = ny * w + nx
      let cost = node[j]
      if not cost.isFinite: continue
      let len = (if dx != 0 and dy != 0: Sqrt2F else: 1'f32)
      let tq = tangent[j]
      let nd = d + cost * len + WDirection * directionCost(tp, tq, dx, dy, len)
      if nd < dist[j]:
        dist[j] = nd
        `from`[j] = uint8(k)
        heap.push((cast[uint32](nd), j))
  if goal < 0 or goal >= n or not dist[goal].isFinite: return none(seq[int])
  var cells = @[goal]
  var cur = goal
  while cur != start and cells.len <= n:
    let k = int(`from`[cur])
    if k >= Dirs.len: return none(seq[int])
    let (dx, dy) = Dirs[k]
    let (x, y) = (cur mod w - dx, cur div w - dy)
    if x < 0 or y < 0: return none(seq[int])
    cur = y * w + x
    cells.add cur
  var r = newSeq[int](cells.len)
  for k in 0 ..< cells.len: r[k] = cells[cells.high - k]
  some(r)

# ---------------------------------------------------------------- public API

proc snap*(t: var Tracer; fetch: Fetch; p0: array[2, float64]; s0: Settings): Option[array[2, float64]] =
  ## Where a fastening point near `p` goes: the centre of the most prominent edge within the
  ## detection width, or `p` itself (kept inside the image) when no edge there reaches the
  ## contrast. None when `p` is not a finite point or the image is empty.
  let s = newSettings(s0.width, s0.contrast)
  let cp = t.clampPoint(p0)
  if cp.isNone: return none(array[2, float64])
  let p = cp.get
  let r = s.width
  let (px, py) = t.pixel(p)
  let ri = asI32(ceil(r)) + 1
  let area = rect(satSub(px, ri), satSub(py, ri), satAdd(px, ri + 1), satAdd(py, ri + 1)).intersect(t.bounds)
  let f = t.field(fetch, area)
  var best = none((float32, array[2, float64]))
  for y in area.y0 ..< area.y1:
    for x in area.x0 ..< area.x1:
      let ix = f.index(x, y)
      if ix.isNone: continue
      let i = ix.get
      let strength = f.strength[i]
      if strength < s.contrast or not f.ridge[i]: continue
      let d = dist([float64(x) + 0.5, float64(y) + 0.5], p)
      if d > r: continue
      # The strongest edge, a little in favour of the nearer one.
      let score = strength * (1'f32 - 0.25'f32 * float32(d / r))
      if best.isNone or score > best.get[0]:
        best = some((score, f.point(i, x, y, s.contrast)))
  some(if best.isSome: best.get[1] else: p)

proc tracePiece(t: var Tracer; fetch: Fetch; a, b: array[2, float64]; guide: seq[array[2, float64]]; s: Settings): seq[array[2, float64]] =
  ## One piece of `trace`: the cheapest path from `a` to `b` in the corridor around `guide`, from
  ## `a` to `b` inclusive.
  let rad = s.radius
  let (ax, ay) = t.pixel(a)
  let (bx, by) = t.pixel(b)
  var bbox = rect(min(ax, bx), min(ay, by), max(ax, bx) + 1, max(ay, by) + 1)
  for q in guide:
    let (x, y) = t.pixel(q)
    bbox = bbox.union(rect(x, y, x + 1, y + 1))
  let area = bbox.inflate(asI32(ceil(rad)) + 1).intersect(t.bounds)
  let f = t.field(fetch, area)
  var near = corridor(area, guide, rad)
  let (si, gi) = (f.index(ax, ay), f.index(bx, by))
  if si.isNone or gi.isNone: return @[a, b]
  let (start, goal) = (si.get, gi.get)
  for i in [start, goal]:
    if i < near.len: near[i] = 0
  let cells = search(f, near, float32(rad * rad), start, goal, s.contrast)
  if cells.isNone: return @[a, b]
  var pts: seq[array[2, float64]]
  for i in cells.get: pts.add f.pointAt(i, s.contrast)
  if pts.len <= 1: return @[a, b]
  pts[0] = a
  pts[^1] = b
  pts

proc trace*(t: var Tracer; fetch: Fetch; from0, to0: array[2, float64]; guide: openArray[array[2, float64]];
            s0: Settings): seq[array[2, float64]] =
  ## The border from `from` to `to` along the edges near `guide`, the pointer's path between them
  ## (`from` and `to` are its ends whatever it holds). It starts at `from` and ends at `to` exactly
  ## (both kept inside the image). Empty when either is not a finite point or the image is empty.
  let s = newSettings(s0.width, s0.contrast)
  let (fp, tp) = (t.clampPoint(from0), t.clampPoint(to0))
  if fp.isNone or tp.isNone: return @[]
  let (fromP, toP) = (fp.get, tp.get)
  var g0 = @[fromP]
  for q in guide:
    let c = t.clampPoint(q)
    if c.isSome: g0.add c.get
  g0.add toP
  # A coarse guide is enough for the corridor, and much cheaper to draw.
  let g = simplify(g0, 0.5)
  if length(g) > MaxGuideLength: return g
  let pieces = split(g, Piece)
  var o = @[fromP]
  var cur = fromP
  let last = max(pieces.len - 1, 0)
  for k, piece in pieces:
    var target = toP
    if k != last and piece.len > 0:
      let sn = t.snap(fetch, piece[^1], s)
      if sn.isSome: target = sn.get
    let part = t.tracePiece(fetch, cur, target, piece, s)
    for j in 1 ..< part.len: o.add part[j]
    cur = target
  # Within half a pixel: drops the staircase of pixel steps, keeps the curve of an edge.
  simplify(o, 0.5)
