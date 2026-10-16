## Projective transforms (Free Transform, Distort, Perspective) and warping a surface through
## them with nearest / bilinear / bicubic interpolation. Rust `photocraft_algo::transform`.

import std/[math, options, json]
import ../geom, ../color, ../raster
import resample

type
  Homography* = object
    ## Row-major 3×3 matrix `[a b c; d e f; g h i]` mapping source → destination.
    m*: array[9, float64]

  Interp* = enum
    ipNearest = "nearest", ipBilinear = "bilinear", ipBicubic = "bicubic"

const IdentityHomography* = Homography(m: [1.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0])

proc homography*(m: array[9, float64]): Homography = Homography(m: m)
proc defaultOf*(_: typedesc[Interp]): Interp = ipBicubic

proc parseInterp*(s: string): Interp =
  case s
  of "nearest", "nearestNeighbor": ipNearest
  of "bilinear": ipBilinear
  else: ipBicubic

proc toJ*(i: Interp): JsonNode = newJString($i)

proc isIdentity*(h: Homography): bool =
  let m = h.m
  if abs(m[8]) < 1e-12: return false
  let w = m[8]
  abs(m[0] / w - 1.0) < 1e-9 and abs(m[4] / w - 1.0) < 1e-9 and abs(m[1] / w) < 1e-9 and
    abs(m[2] / w) < 1e-9 and abs(m[3] / w) < 1e-9 and abs(m[5] / w) < 1e-9 and abs(m[6] / w) < 1e-9 and
    abs(m[7] / w) < 1e-9

proc apply*(h: Homography; x, y: float64): (float64, float64) =
  let m = h.m
  let w = m[6] * x + m[7] * y + m[8]
  ((m[0] * x + m[1] * y + m[2]) / w, (m[3] * x + m[4] * y + m[5]) / w)

proc mul*(a, b: Homography): Homography =
  for i in 0 ..< 3:
    for j in 0 ..< 3:
      var s = -0.0 # Rust's `Iterator::sum` starts at -0.0
      for k in 0 ..< 3: s += a.m[i * 3 + k] * b.m[k * 3 + j]
      result.m[i * 3 + j] = s

proc inverse*(h: Homography): Option[Homography] =
  let m = h.m
  let c = [m[4] * m[8] - m[5] * m[7], m[2] * m[7] - m[1] * m[8], m[1] * m[5] - m[2] * m[4],
           m[5] * m[6] - m[3] * m[8], m[0] * m[8] - m[2] * m[6], m[2] * m[3] - m[0] * m[5],
           m[3] * m[7] - m[4] * m[6], m[1] * m[6] - m[0] * m[7], m[0] * m[4] - m[1] * m[3]]
  let det = m[0] * c[0] + m[1] * c[3] + m[2] * c[6]
  if abs(det) < 1e-12: return none(Homography)
  var r: Homography
  for i in 0 ..< 9: r.m[i] = c[i] / det
  some(r)

proc squareToQuad(q: array[4, array[2, float64]]): Option[Homography] =
  let (x0, y0, x1, y1, x2, y2, x3, y3) = (q[0][0], q[0][1], q[1][0], q[1][1], q[2][0], q[2][1], q[3][0], q[3][1])
  let (sx, sy) = (x0 - x1 + x2 - x3, y0 - y1 + y2 - y3)
  if abs(sx) < 1e-12 and abs(sy) < 1e-12:
    return some(Homography(m: [x1 - x0, x3 - x0, x0, y1 - y0, y3 - y0, y0, 0.0, 0.0, 1.0]))
  let (dx1, dx2, dy1, dy2) = (x1 - x2, x3 - x2, y1 - y2, y3 - y2)
  let den = dx1 * dy2 - dx2 * dy1
  if abs(den) < 1e-12: return none(Homography)
  let g = (sx * dy2 - dx2 * sy) / den
  let h = (dx1 * sy - sx * dy1) / den
  some(Homography(m: [x1 - x0 + g * x1, x3 - x0 + h * x3, x0, y1 - y0 + g * y1, y3 - y0 + h * y3, y0, g, h, 1.0]))

proc rectToQuad*(r: array[4, float64]; quad: array[4, array[2, float64]]): Option[Homography] =
  ## Maps the rectangle `[x0, y0, x1, y1]` onto `quad` (TL, TR, BR, BL).
  let (w, h) = (r[2] - r[0], r[3] - r[1])
  if w <= 0.0 or h <= 0.0: return none(Homography)
  let toUnit = Homography(m: [1.0 / w, 0.0, -r[0] / w, 0.0, 1.0 / h, -r[1] / h, 0.0, 0.0, 1.0])
  let s = squareToQuad(quad)
  if s.isNone: return none(Homography)
  some(s.get.mul(toUnit))

proc localScale*(h: Homography; x, y: float64): float64 =
  let e = 0.5
  let (a, b) = (h.apply(x - e, y), h.apply(x + e, y))
  let (c, d) = (h.apply(x, y - e), h.apply(x, y + e))
  let (jx, jy) = ((b[0] - a[0], b[1] - a[1]), (d[0] - c[0], d[1] - c[1]))
  sqrt(abs(jx[0] * jy[1] - jx[1] * jy[0]))

proc catmullRom4*(t: float64): array[4, float64] =
  let t2 = t * t
  let t3 = t2 * t
  [-0.5 * t3 + t2 - 0.5 * t, 1.5 * t3 - 2.5 * t2 + 1.0, -1.5 * t3 + 2.0 * t2 + 0.5 * t, 0.5 * t3 - 0.5 * t2]

proc convexQuad*(q: array[4, array[2, float64]]): bool =
  for p in q:
    for v in p:
      if v != v or v == Inf or v == NegInf: return false
  var area2 = 0.0
  for i in 0 ..< 4:
    let (a, b, c) = (q[i], q[(i + 1) mod 4], q[(i + 2) mod 4])
    let (e1, e2) = ([b[0] - a[0], b[1] - a[1]], [c[0] - b[0], c[1] - b[1]])
    let cross = e1[0] * e2[1] - e1[1] * e2[0]
    if cross != cross or cross <= 1e-3 * hypot(e1[0], e1[1]) * hypot(e2[0], e2[1]): return false
    area2 += a[0] * b[1] - b[0] * a[1]
  classify(area2) notin {fcInf, fcNegInf, fcNan} and area2 >= 2.0

proc quadSize*(q: array[4, array[2, float64]]): (float64, float64) =
  proc len(a, b: array[2, float64]): float64 = hypot(b[0] - a[0], b[1] - a[1])
  let (tl, tr, br, bl) = (q[0], q[1], q[2], q[3])
  ((len(tl, tr) + len(bl, br)) / 2.0, (len(tl, bl) + len(tr, br)) / 2.0)

proc quadToRect*(q: array[4, array[2, float64]]; w, h: float64): Option[Homography] =
  if not (convexQuad(q) and classify(w) notin {fcInf, fcNegInf, fcNan} and
          classify(h) notin {fcInf, fcNegInf, fcNan} and w > 0.0 and h > 0.0):
    return none(Homography)
  let r = rectToQuad([0.0, 0.0, w, h], q)
  if r.isNone: return none(Homography)
  r.get.inverse

proc isFiniteF(v: float64): bool {.inline.} = classify(v) notin {fcInf, fcNegInf, fcNan}

proc warpImpl(src0: Surface; srcRect0: Rect; h0: Homography; interp: Interp; clip: Option[Rect]): Surface =
  var fmt = src0.format
  var src = src0
  if not fmt.alpha:
    fmt = pixelFormat(fmt.mode, fmt.sample, true)
    src = src0.convert(fmt)
  result = newSurface(fmt)
  if srcRect0.isEmpty or (clip.isSome and clip.get.isEmpty): return
  var h = h0
  var srcRect = srcRect0
  var (cx, cy) = (float64(srcRect.x0 + srcRect.x1) / 2.0, float64(srcRect.y0 + srcRect.y1) / 2.0)
  if clip.isSome:
    let inv0 = h.inverse
    if inv0.isSome:
      let c = clip.get
      (cx, cy) = inv0.get.apply((float64(c.x0) + float64(c.x1)) / 2.0, (float64(c.y0) + float64(c.y1)) / 2.0)
  let scale = h.localScale(cx, cy)
  if interp != ipNearest and scale < 0.5 and scale > 0.0:
    let f = min(pow(2.0, ceil(log2(scale))), 1.0)
    src = resizeSurface(src, f, f, rsBicubic)
    h = h.mul(Homography(m: [1.0 / f, 0.0, 0.0, 0.0, 1.0 / f, 0.0, 0.0, 0.0, 1.0]))
    srcRect = scaledRect(srcRect, f, f)
  let invO = h.inverse
  if invO.isNone: return
  let inv = invO.get
  var corners: array[4, (float64, float64)]
  for i, (x, y) in [(srcRect.x0, srcRect.y0), (srcRect.x1, srcRect.y0), (srcRect.x1, srcRect.y1), (srcRect.x0, srcRect.y1)]:
    corners[i] = h.apply(float64(x), float64(y))
  let (sx0, sy0, sx1, sy1) = (float64(srcRect.x0) - 1.0, float64(srcRect.y0) - 1.0, float64(srcRect.x1) + 1.0, float64(srcRect.y1) + 1.0)
  var fwdW: array[4, float64]
  for i, (x, y) in [(sx0, sy0), (sx1, sy0), (sx1, sy1), (sx0, sy1)]:
    fwdW[i] = h.m[6] * x + h.m[7] * y + h.m[8]
  var side = 0.0
  for s in [1.0, -1.0]:
    var all = true
    for c in fwdW:
      if not (s * c > 0.0): all = false
    if all:
      side = s
      break
  var wmax = 0.0
  for c in fwdW: wmax = max(wmax, abs(c))
  let wLo = 1.0 / wmax
  var finite = true
  for c in corners:
    if not (isFiniteF(c[0]) and isFiniteF(c[1])): finite = false
  let lim = float64(1 shl 20)
  proc bounds(): Rect =
    var (mnx, mny, mxx, mxy) = (Inf, Inf, NegInf, NegInf)
    for c in corners:
      mnx = min(mnx, c[0]); mny = min(mny, c[1]); mxx = max(mxx, c[0]); mxy = max(mxy, c[1])
    rect(asI32(max(floor(mnx), -lim)) - 1, asI32(max(floor(mny), -lim)) - 1,
         asI32(min(ceil(mxx), lim)) + 1, asI32(min(ceil(mxy), lim)) + 1)
  var dst: Rect
  if clip.isSome:
    dst = (if finite and side != 0.0: bounds().intersect(clip.get) else: clip.get)
  elif finite: dst = bounds()
  else: return
  if dst.isEmpty: return
  let n = fmt.channels
  let a = n - 1
  for tc in dst.tiles:
    let t = tc.rect.intersect(dst)
    if t.isEmpty: continue
    let tcs = [(float64(t.x0), float64(t.y0)), (float64(t.x1), float64(t.y0)), (float64(t.x1), float64(t.y1)),
               (float64(t.x0), float64(t.y1))]
    var wc: array[4, float64]
    for i, p in tcs: wc[i] = inv.m[6] * p[0] + inv.m[7] * p[1] + inv.m[8]
    var poly: seq[(float64, float64)]
    if side != 0.0:
      var d: array[4, float64]
      for i in 0 ..< 4: d[i] = side * wc[i] - wLo
      for i in 0 ..< 4:
        let (p, dp) = (tcs[i], d[i])
        let (q, dq) = (tcs[(i + 1) mod 4], d[(i + 1) mod 4])
        if dp >= 0.0: poly.add p
        if (dp >= 0.0) != (dq >= 0.0):
          let k = dp / (dp - dq)
          poly.add (p[0] + (q[0] - p[0]) * k, p[1] + (q[1] - p[1]) * k)
    else:
      poly = @tcs
    if poly.len == 0: continue
    var (fx0, fy0, fx1, fy1) = (Inf, Inf, NegInf, NegInf)
    for p in poly:
      let f = inv.apply(p[0], p[1])
      fx0 = min(fx0, f[0]); fy0 = min(fy0, f[1]); fx1 = max(fx1, f[0]); fy1 = max(fy1, f[1])
    var straddles = false
    if side == 0.0:
      var le, ge = false
      for w in wc:
        if w <= 0.0: le = true
        if w >= 0.0: ge = true
      straddles = le and ge
    let foot = if straddles: srcRect
               else: rect(asI32(floor(fx0)) - 3, asI32(floor(fy0)) - 3, asI32(ceil(fx1)) + 3, asI32(ceil(fy1)) + 3).intersect(srcRect)
    if foot.isEmpty or not src.hasTilesIn(foot): continue
    var px = src.tryReadRegion(foot)
    var p = 0
    while p + n <= px.len:
      let al = px[p + a]
      for c in 0 ..< a: px[p + c] *= al
      p += n
    let fw = foot.width
    template at(x, y, c: int): float64 =
      (if x < foot.x0 or y < foot.y0 or x >= foot.x1 or y >= foot.y1: 0.0
       else: float64(px[((y - foot.y0) * fw + (x - foot.x0)) * n + c]))
    let w = t.width
    var outp = newSeq[float32](w * t.height * n)
    var any = false
    var acc: array[8, float64]
    for y in t.y0 ..< t.y1:
      for x in t.x0 ..< t.x1:
        var (u, v) = inv.apply(float64(x) + 0.5, float64(y) + 0.5)
        u -= 0.5
        v -= 0.5
        if u < float64(srcRect.x0) - 1.0 or v < float64(srcRect.y0) - 1.0 or u > float64(srcRect.x1) or
           v > float64(srcRect.y1) or u != u or v != v:
          continue
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
          let (ix, iy) = (asI32(floor(u)), asI32(floor(v)))
          let (wx, wy) = (catmullRom4(u - float64(ix)), catmullRom4(v - float64(iy)))
          for j in 0 ..< 4:
            for i in 0 ..< 4:
              let k = wx[i] * wy[j]
              for c in 0 ..< n: acc[c] += at(ix - 1 + i, iy - 1 + j, c) * k
        let al = clamp(acc[a], 0.0, 1.0)
        if al <= 0.0: continue
        any = true
        let o = ((y - t.y0) * w + (x - t.x0)) * n
        for c in 0 ..< a: outp[o + c] = float32(clamp(acc[c] / al, 0.0, 1.0))
        outp[o + a] = float32(al)
    if any: result.tryWriteRegion(t, outp)
  result.prune()

proc warpSurface*(src: Surface; srcRect: Rect; h: Homography; interp: Interp): Surface =
  ## Warps `src` (its `srcRect` content) through `h` (source → destination). The output has
  ## alpha; pixels outside the mapped area are transparent.
  warpImpl(src, srcRect, h, interp, none(Rect))

proc warpSurfaceClipped*(src: Surface; srcRect: Rect; h: Homography; interp: Interp; clip: Rect): Surface =
  ## `warpSurface` producing only the pixels inside `clip` (and safe for maps that send part of
  ## the source behind the horizon).
  warpImpl(src, srcRect, h, interp, some(clip))
