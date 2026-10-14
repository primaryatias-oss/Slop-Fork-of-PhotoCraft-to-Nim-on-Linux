## Anti-aliased path rasterizer: exact signed-area accumulation (the "font-rs" technique), f32
## coverage, nonzero-ish fill (|winding| clamped to 1); curves are flattened to lines.

import std/math

type
  Xform* = array[6, float64]   ## affine `[a c e; b d f]` as [a, b, c, d, e, f]

  Coverage* = object
    width*, height*: int
    acc: seq[float32]

  BoundsSink* = object
    rect*: Option4

  Option4* = object
    some*: bool
    r*: array[4, float64]

const IdentityXform*: Xform = [1.0, 0.0, 0.0, 1.0, 0.0, 0.0]

proc apply*(m: Xform; x, y: float64): (float64, float64) {.inline.} =
  (m[0] * x + m[2] * y + m[4], m[1] * x + m[3] * y + m[5])

proc mul*(m, o: Xform): Xform =
  ## `m ∘ o` (apply `o` first).
  [m[0] * o[0] + m[2] * o[1], m[1] * o[0] + m[3] * o[1], m[0] * o[2] + m[2] * o[3],
   m[1] * o[2] + m[3] * o[3], m[0] * o[4] + m[2] * o[5] + m[4], m[1] * o[4] + m[3] * o[5] + m[5]]

proc maxScale*(m: Xform): float64 = max(sqrt(m[0] * m[0] + m[1] * m[1]), sqrt(m[2] * m[2] + m[3] * m[3]))

proc finite(x: float64): bool {.inline.} = x.classify notin {fcNan, fcInf, fcNegInf}

proc add*(b: var BoundsSink; p: (float64, float64)) =
  if not (p[0].finite and p[1].finite): return
  if not b.rect.some:
    b.rect = Option4(some: true, r: [p[0], p[1], p[0], p[1]])
  else:
    b.rect.r = [min(b.rect.r[0], p[0]), min(b.rect.r[1], p[1]), max(b.rect.r[2], p[0]), max(b.rect.r[3], p[1])]

proc line*(b: var BoundsSink; p0, p1: (float64, float64)) =
  b.add p0
  b.add p1

proc newCoverage*(width, height: int): Coverage =
  Coverage(width: width, height: height, acc: newSeq[float32](width * height + 2))

proc line*(c: var Coverage; p0, p1: (float64, float64)) =
  ## Adds a line segment (pixel coordinates, y down).
  let (w, h) = (float32(c.width), float32(c.height))
  if p0[1] == p1[1] or not (p0[0].finite and p0[1].finite and p1[0].finite and p1[1].finite): return
  let (dir, a, b) = if p0[1] < p1[1]: (1'f32, p0, p1) else: (-1'f32, p1, p0)
  let (x0, y0, x1, y1) = (float32(a[0]), float32(a[1]), float32(b[0]), float32(b[1]))
  if y1 <= 0 or y0 >= h: return
  let dxdy = (x1 - x0) / (y1 - y0)
  var x = x0
  let ys = max(y0, 0'f32)
  if y0 < 0: x -= y0 * dxdy
  let ye = min(y1, h)
  let stride = c.width
  var y = ys
  var row = int(floor(ys))
  while y < ye:
    let ynext = min(float32(row + 1), ye)
    let dy = ynext - y
    let xnext = x + dxdy * dy
    let d = dy * dir
    var (xa, xb) = if x < xnext: (x, xnext) else: (xnext, x)
    xa = clamp(xa, 0'f32, w)
    xb = clamp(xb, 0'f32, w)
    let ls = row * stride
    let x0f = floor(xa)
    let x0i = int(x0f)
    let x1c = ceil(xb)
    let x1i = int(x1c)
    if x1i <= x0i + 1:
      let xmf = 0.5'f32 * (xa + xb) - x0f
      c.acc[ls + x0i] += d - d * xmf
      c.acc[ls + x0i + 1] += d * xmf
    else:
      let s = 1'f32 / (xb - xa)
      let x0frac = xa - x0f
      let a0 = 0.5'f32 * s * (1 - x0frac) * (1 - x0frac)
      let x1frac = xb - x1c + 1
      let am = 0.5'f32 * s * x1frac * x1frac
      c.acc[ls + x0i] += d * a0
      if x1i == x0i + 2:
        c.acc[ls + x0i + 1] += d * (1 - a0 - am)
      else:
        let a1 = s * (1.5'f32 - x0frac)
        c.acc[ls + x0i + 1] += d * (a1 - a0)
        for xi in x0i + 2 ..< x1i - 1: c.acc[ls + xi] += d * s
        let a2 = a1 + float32(x1i - x0i - 3) * s
        c.acc[ls + x1i - 1] += d * (1 - a2 - am)
      c.acc[ls + x1i] += d * am
    x = xnext
    y = ynext
    inc row

proc finish*(c: Coverage): seq[float32] =
  ## Per-pixel coverage in 0..1.
  let n = c.width * c.height
  result = newSeq[float32](n)
  var sum = 0'f32
  for i in 0 ..< n:
    sum += c.acc[i]
    result[i] = min(abs(sum), 1'f32)

proc rectTo*[S](sink: var S; xf: Xform; x0, y0, x1, y1: float64) =
  ## Adds a transformed axis-aligned rectangle.
  let p = [xf.apply(x0, y0), xf.apply(x1, y0), xf.apply(x1, y1), xf.apply(x0, y1)]
  for i in 0 ..< 4: sink.line(p[i], p[(i + 1) mod 4])

type Pen*[S] = object
  ## Transforms font-space points (y up) and flattens curves into a line sink.
  sink*: ptr S
  xf*: Xform
  start, last: (float64, float64)
  tol: float64

proc newPen*[S](sink: var S; xf: Xform): Pen[S] =
  Pen[S](sink: addr sink, xf: xf, tol: 0.03)

proc to[S](p: var Pen[S]; q: (float64, float64)) =
  p.sink[].line(p.last, q)
  p.last = q

proc segments[S](p: Pen[S]; a, b, c: (float64, float64)): int =
  let dd = sqrt((a[0] - 2 * b[0] + c[0]) ^ 2 + (a[1] - 2 * b[1] + c[1]) ^ 2)
  clamp(int(ceil(sqrt(0.75 * dd / p.tol))), 1, 100)

proc close*[S](p: var Pen[S]) =
  if p.last != p.start: p.to(p.start)

proc moveTo*[S](p: var Pen[S]; x, y: float32) =
  p.close()
  let q = p.xf.apply(float64(x), float64(y))
  p.start = q
  p.last = q

proc lineTo*[S](p: var Pen[S]; x, y: float32) = p.to(p.xf.apply(float64(x), float64(y)))

proc quadTo*[S](p: var Pen[S]; cx, cy, x, y: float32) =
  let a = p.last
  let b = p.xf.apply(float64(cx), float64(cy))
  let c = p.xf.apply(float64(x), float64(y))
  let n = p.segments(a, b, c)
  for i in 1 .. n:
    let t = float64(i) / float64(n)
    let mt = 1 - t
    p.to((mt * mt * a[0] + 2 * mt * t * b[0] + t * t * c[0], mt * mt * a[1] + 2 * mt * t * b[1] + t * t * c[1]))

proc curveTo*[S](p: var Pen[S]; c0x, c0y, c1x, c1y, x, y: float32) =
  let a = p.last
  let b = p.xf.apply(float64(c0x), float64(c0y))
  let c = p.xf.apply(float64(c1x), float64(c1y))
  let d = p.xf.apply(float64(x), float64(y))
  let n = max(p.segments(a, b, c), p.segments(b, c, d))
  for i in 1 .. n:
    let t = float64(i) / float64(n)
    let mt = 1 - t
    let (k0, k1, k2, k3) = (mt * mt * mt, 3 * mt * mt * t, 3 * mt * t * t, t * t * t)
    p.to((k0 * a[0] + k1 * b[0] + k2 * c[0] + k3 * d[0], k0 * a[1] + k1 * b[1] + k2 * c[1] + k3 * d[1]))
