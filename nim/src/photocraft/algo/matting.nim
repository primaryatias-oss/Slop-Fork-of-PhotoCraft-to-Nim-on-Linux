## Edge refinement ("Select and Mask" / Refine Edge) and colour decontamination (port of
## `matting.rs`, `matting/brush.rs` and `matting/morphology.rs`).
##
## * Soft edges come from the **guided filter** (K. He, J. Sun and X. Tang, "Guided Image
##   Filtering", ECCV 2010 / PAMI 2013): filtering the binary selection with the image as guide
##   fits a local linear model `α ≈ aᵀI + b` in every window, so the mask takes on the image's
##   soft transitions inside a band of `radius` pixels around the selection edge. "Smart radius"
##   adapts the band per pixel to the transition width observed in the image.
## * Then, in Photoshop's order: Smooth, Feather (Gaussian, σ = feather / 2), Contrast and Shift
##   Edge (grey dilation / erosion).
## * **Decontaminate colours** replaces fringe colours by the normalised-convolution average of
##   nearby fully selected pixels, blended by `amount` and the fringe's transparency.
## * The **Refine Edge Brush** marks where refinement happens: added areas get coverage from the
##   colour-line model (A. Levin, D. Lischinski, Y. Weiss, CVPR 2006), restored ones keep the
##   original edge.
##
## Work is tiled: only tiles near the selection boundary are processed.

import std/[math, options, json, strutils]
import ../geom, ../color, ../raster
import ./util, ./segbase, ./selection, ./selection_distance

type
  RefineParams* = object
    ## Select and Mask parameters (Photoshop's units).
    radius*: float32     ## edge-detection band radius in pixels (0 = no edge detection)
    smartRadius*: bool   ## adapt the band to the edge's observed softness
    smooth*: float32     ## 0–100
    feather*: float32    ## pixels
    contrast*: float32   ## 0–100
    shiftEdge*: float32  ## −100–100 (% of `max(radius, 4 px)`)

  EdgeStroke* = object
    ## One Refine Edge Brush stroke, in document pixels.
    points*: seq[(float32, float32)] ## the stroke's path (a single point paints a dab)
    size*: float32                   ## brush diameter in pixels
    add*: bool                       ## add to the refinement area (true) or restore the original edge

  EdgeBrush* = object
    ## The painted area: signed coverage (`+1` refined, `−1` restored, `0` untouched) over `bbox`.
    bbox: Rect
    v: seq[int8] ## coverage × 127, row-major over `bbox`
    added: Rect  ## bounds of the added (positive) coverage
    window: int  ## box radius of the colour estimate (pixels)

  BrushBuf = object
    v: seq[float32]
    window: int

  Morphology* = proc(src: seq[float32]; w, h, r: int; grow: bool): seq[float32]

const
  Eps* = 1e-4'f32 ## guided-filter regulariser (colour guide, samples in 0–1)
  Tile = 256
  BrushSizeMin* = 1'f32    ## brush diameters the strokes may use
  BrushSizeMax* = 5000'f32
  MaxSweptArea* = float64(1'u64 shl 28)
    ## most pixels the strokes of one `EdgeBrush` may sweep (Σ length × size + size² per stroke)
  MaxGaussRadius = 1 shl 20

proc satI32(v: float32): int = asI32(float64(v))

proc asI64f(v: float32): int64 =
  ## Rust `v as i64`: saturating, NaN → 0.
  if v != v: 0'i64
  elif v >= 9.223372e18'f32: high(int64)
  elif v <= -9.223372e18'f32: low(int64)
  else: int64(v)

proc gfRadius(p: RefineParams): int = asUsize(float64(clamp(ceil(p.radius), 1'f32, 64'f32)))
proc smoothSigma(p: RefineParams): float32 = clamp(p.smooth, 0'f32, 100'f32) / 100'f32 * 4'f32
proc featherSigma(p: RefineParams): float32 = rmax(p.feather, 0'f32) / 2'f32
proc shiftPx(p: RefineParams): int =
  satI32(clamp(round(clamp(p.shiftEdge, -100'f32, 100'f32) / 100'f32 * rmax(p.radius, 4'f32)), -64'f32, 64'f32))
proc usesImage(p: RefineParams): bool = p.radius > 0'f32

proc halo*(p: RefineParams): int =
  ## Pixels read around an output pixel.
  var edge = 0
  if p.usesImage:
    let g = p.gfRadius
    edge = max(satI32(ceil(p.radius)), 2 * g + (if p.smartRadius: g + 1 else: 0))
  edge + satI32(ceil(3'f32 * p.smoothSigma)) + satI32(ceil(3'f32 * p.featherSigma)) + abs(p.shiftPx) + 2

proc spread*(p: RefineParams): int =
  ## How far the refined mask can extend beyond the original.
  (if p.usesImage: satI32(ceil(p.radius)) else: 0) + satI32(ceil(3'f32 * p.smoothSigma)) +
    satI32(ceil(3'f32 * p.featherSigma)) + max(p.shiftPx, 0) + 1

proc refineParamsFromJson*(j: JsonNode): RefineParams =
  ## The parameters from their JSON form (camelCase, missing fields 0 / false); raises
  ## `ValueError` on a malformed value.
  if j.kind != JObject: raise newException(ValueError, "Select and Mask parameters must be an object")
  proc num(n: JsonNode; k: string): float32 =
    case n.kind
    of JInt: float32(n.getBiggestInt)
    of JFloat: float32(n.getFloat)
    else: raise newException(ValueError, "invalid type for " & k & ": expected f32")
  for k, v in j:
    case k
    of "radius": result.radius = num(v, k)
    of "smartRadius":
      if v.kind != JBool: raise newException(ValueError, "invalid type for smartRadius: expected a boolean")
      result.smartRadius = v.getBool
    of "smooth": result.smooth = num(v, k)
    of "feather": result.feather = num(v, k)
    of "contrast": result.contrast = num(v, k)
    of "shiftEdge": result.shiftEdge = num(v, k)
    else: discard

proc toJ*(p: RefineParams): JsonNode =
  proc j32(v: float32): JsonNode = (if v.isFinite: newJFloat(parseFloat($v)) else: newJNull())
  %*{"radius": j32(p.radius), "smartRadius": p.smartRadius, "smooth": j32(p.smooth), "feather": j32(p.feather),
     "contrast": j32(p.contrast), "shiftEdge": j32(p.shiftEdge)}

# ---------------------------------------------------------------- filters

proc boxMean*(src: openArray[float32]; w, h, r: int): seq[float32] =
  ## Box mean with radius `r` (window clipped to the image, divided by the pixel count).
  if w <= 0 or h <= 0: return @[]
  var tmp = newSeq[float32](w * h)
  var pre = newSeq[float64](max(w, h) + 1)
  for y in 0 ..< h:
    for x in 0 ..< w: pre[x + 1] = pre[x] + float64(src[y * w + x])
    for x in 0 ..< w:
      let (lo, hi) = (max(x - r, 0), min(x + r + 1, w))
      tmp[y * w + x] = float32((pre[hi] - pre[lo]) / float64(hi - lo))
  result = newSeq[float32](w * h)
  var col = newSeq[float64]((h + 1) * w)
  for y in 0 ..< h:
    for x in 0 ..< w: col[(y + 1) * w + x] = col[y * w + x] + float64(tmp[y * w + x])
  for y in 0 ..< h:
    let (lo, hi) = (max(y - r, 0), min(y + r + 1, h))
    let n = float64(hi - lo)
    for x in 0 ..< w: result[y * w + x] = float32((col[hi * w + x] - col[lo * w + x]) / n)

proc guidedFilterGray*(guide, p: openArray[float32]; w, h, r: int; eps: float32): seq[float32] =
  ## Guided filter with a grayscale guide.
  let n = w * h
  let mi = boxMean(guide, w, h, r)
  let mp = boxMean(p, w, h, r)
  var ip, ii = newSeq[float32](n)
  for i in 0 ..< n:
    ip[i] = guide[i] * p[i]
    ii[i] = guide[i] * guide[i]
  let mip = boxMean(ip, w, h, r)
  let mii = boxMean(ii, w, h, r)
  var a, b = newSeq[float32](n)
  for i in 0 ..< n:
    let variance = mii[i] - mi[i] * mi[i]
    let cov = mip[i] - mi[i] * mp[i]
    a[i] = cov / (variance + eps)
    b[i] = mp[i] - a[i] * mi[i]
  let ma = boxMean(a, w, h, r)
  let mb = boxMean(b, w, h, r)
  result = newSeq[float32](n)
  for i in 0 ..< n: result[i] = ma[i] * guide[i] + mb[i]

proc guidedFilterColor*(guide: RgbImage; p: openArray[float32]; r: int; eps: float32): seq[float32] =
  ## Guided filter with a colour guide (He et al., Eq. 19–21: `a_k = (Σ_k + εU)⁻¹ cov_k(I, p)`).
  let (w, h) = (guide.w, guide.h)
  let n = w * h
  var chans: array[3, seq[float32]]
  for c in 0 .. 2:
    chans[c] = newSeq[float32](n)
    for i in 0 ..< n: chans[c][i] = guide.px[i][c]
  var m: array[3, seq[float32]]
  for c in 0 .. 2: m[c] = boxMean(chans[c], w, h, r)
  let mp = boxMean(p, w, h, r)
  var mip: array[3, seq[float32]]
  for c in 0 .. 2:
    var v = newSeq[float32](n)
    for i in 0 ..< n: v[i] = chans[c][i] * p[i]
    mip[c] = boxMean(v, w, h, r)
  const pairs = [(0, 0), (0, 1), (0, 2), (1, 1), (1, 2), (2, 2)]
  var mii: array[6, seq[float32]]
  for k, (a, b) in pairs:
    var v = newSeq[float32](n)
    for i in 0 ..< n: v[i] = chans[a][i] * chans[b][i]
    mii[k] = boxMean(v, w, h, r)
  var av: array[3, seq[float32]]
  for c in 0 .. 2: av[c] = newSeq[float32](n)
  var bv = newSeq[float32](n)
  let e = float64(eps)
  for i in 0 ..< n:
    let mu = [float64(m[0][i]), float64(m[1][i]), float64(m[2][i])]
    template s(k, a, b: int): float64 = float64(mii[k][i]) - mu[a] * mu[b]
    let (rr, rg, rb, gg, gb, bb) = (s(0, 0, 0) + e, s(1, 0, 1), s(2, 0, 2), s(3, 1, 1) + e, s(4, 1, 2), s(5, 2, 2) + e)
    let cov = [float64(mip[0][i]) - mu[0] * float64(mp[i]), float64(mip[1][i]) - mu[1] * float64(mp[i]),
               float64(mip[2][i]) - mu[2] * float64(mp[i])]
    # Inverse of the symmetric 3×3 matrix.
    let i00 = gg * bb - gb * gb
    let i01 = gb * rb - rg * bb
    let i02 = rg * gb - gg * rb
    let i11 = rr * bb - rb * rb
    let i12 = rb * rg - rr * gb
    let i22 = rr * gg - rg * rg
    let det = rr * i00 + rg * i01 + rb * i02
    var (a0, a1, a2) = (0.0, 0.0, 0.0)
    if not (abs(det) < 1e-30):
      a0 = (i00 * cov[0] + i01 * cov[1] + i02 * cov[2]) / det
      a1 = (i01 * cov[0] + i11 * cov[1] + i12 * cov[2]) / det
      a2 = (i02 * cov[0] + i12 * cov[1] + i22 * cov[2]) / det
    av[0][i] = float32(a0)
    av[1][i] = float32(a1)
    av[2][i] = float32(a2)
    bv[i] = float32(float64(mp[i]) - a0 * mu[0] - a1 * mu[1] - a2 * mu[2])
  var ma: array[3, seq[float32]]
  for c in 0 .. 2: ma[c] = boxMean(av[c], w, h, r)
  let mb = boxMean(bv, w, h, r)
  result = newSeq[float32](n)
  for i in 0 ..< n: result[i] = ma[0][i] * chans[0][i] + ma[1][i] * chans[1][i] + ma[2][i] * chans[2][i] + mb[i]

proc gaussianBlur*(src: openArray[float32]; w, h: int; sigma: float32): seq[float32] =
  ## Separable Gaussian blur (edge-clamped). Raises `ValueError` for absurd radii (σ above a
  ## third of a million pixels), which the Rust crate cannot allocate either.
  if sigma < 0.1'f32: return @src
  let r64 = asI64f(ceil(sigma * 3'f32))
  if r64 > MaxGaussRadius: raise newException(ValueError, "blur radius too large")
  let r = int(r64)
  var k = newSeq[float32](2 * r + 1)
  var s = -0.0'f32 # Rust's Iterator::sum starts at -0.0
  for i in -r .. r:
    k[i + r] = exp(-float32(i * i) / (2'f32 * sigma * sigma))
    s += k[i + r]
  for v in k.mitems: v = v / s
  var tmp = newSeq[float32](w * h)
  for y in 0 ..< h:
    for x in 0 ..< w:
      var a = 0'f32
      for i, kv in k:
        let xx = clamp(x + i - r, 0, w - 1)
        a += src[y * w + xx] * kv
      tmp[y * w + x] = a
  result = newSeq[float32](w * h)
  for y in 0 ..< h:
    for x in 0 ..< w:
      var a = 0'f32
      for i, kv in k:
        let yy = clamp(y + i - r, 0, h - 1)
        a += tmp[yy * w + x] * kv
      result[y * w + x] = a

proc runningExtreme(src: openArray[float32]; outp: var openArray[float32]; r: int; isMax: bool; g, hb: var seq[float32]) =
  ## Running max (or min) over a window of radius `r` (edge-clipped), van Herk / Gil–Werman.
  let n = src.len
  let k = 2 * r + 1
  template f(a, b: float32): float32 = (if isMax: rmax(a, b) else: rmin(a, b))
  let pad = (if isMax: NegInf.float32 else: Inf.float32)
  let m = ((n + 2 * r + k - 1) div k) * k
  g.setLen(m)
  hb.setLen(m)
  template p(j: int): float32 = (if j >= r and j < r + n: src[j - r] else: pad)
  for j in 0 ..< m:
    g[j] = (if j mod k == 0: p(j) else: f(g[j - 1], p(j)))
  for j in countdown(m - 1, 0):
    hb[j] = (if j mod k == k - 1 or j == m - 1: p(j) else: f(hb[j + 1], p(j)))
  for i in 0 ..< outp.len: outp[i] = f(hb[i], g[i + 2 * r])

proc maxSquare(src: openArray[float32]; w, h, r: int; isMax: bool): seq[float32] =
  ## Running max (or min) over a square window of radius `r` (edge-clipped), separable.
  var g, hb: seq[float32]
  var tmp = newSeq[float32](w * h)
  for y in 0 ..< h:
    runningExtreme(src.toOpenArray(y * w, (y + 1) * w - 1), tmp.toOpenArray(y * w, (y + 1) * w - 1), min(r, max(w - 1, 0)), isMax, g, hb)
  result = newSeq[float32](w * h)
  var col, res = newSeq[float32](h)
  for x in 0 ..< w:
    for y in 0 ..< h: col[y] = tmp[y * w + x]
    runningExtreme(col, res, min(r, max(h - 1, 0)), isMax, g, hb)
    for y in 0 ..< h: result[y * w + x] = res[y]

proc cross(src: seq[float32]; dst: var seq[float32]; w, h, d: int; grow: bool) =
  ## A radius-2d Manhattan ball is the union of five radius-d balls, centred here and d pixels in
  ## each axial direction (centres clamped at the canvas edge).
  template pick(a, b: float32): float32 = (if grow: rmax(a, b) else: rmin(a, b))
  for y in 0 ..< h:
    let up = max(y - d, 0) * w
    let down = min(y + d, h - 1) * w
    for x in 0 ..< w:
      let a = y * w
      dst[a + x] = pick(pick(pick(pick(src[a + x], src[a + max(x - d, 0)]), src[a + min(x + d, w - 1)]), src[up + x]), src[down + x])

proc repeated(src: seq[float32]; w, h, r: int; grow: bool): seq[float32] =
  var cur = src
  var nxt = cur
  template pick(a, b: float32): float32 = (if grow: rmax(a, b) else: rmin(a, b))
  for k in 0 ..< r:
    let square = k mod 2 == 0
    for y in 0 ..< h:
      for x in 0 ..< w:
        var v = cur[y * w + x]
        for dy in -1 .. 1:
          for dx in -1 .. 1:
            if (dx != 0 and dy != 0 and not square) or (dx == 0 and dy == 0): continue
            let (nx, ny) = (x + dx, y + dy)
            if nx >= 0 and ny >= 0 and nx < w and ny < h: v = pick(v, cur[ny * w + nx])
        nxt[y * w + x] = v
    swap(cur, nxt)
  cur

proc morph*(src: seq[float32]; w, h, r: int; grow: bool): seq[float32] =
  ## Grey dilation (`grow`) or erosion with the octagonal element of `r` alternating 3×3
  ## square/cross steps (exactly; masks without NaNs or negative zero take a fast path with the
  ## same result). Invalid sizes return an empty buffer.
  if w < 0 or h < 0 or w * h > src.len: return @[]
  let n = w * h
  if n == 0 or r <= 0: return src
  var special = false
  for i in 0 ..< n:
    let v = src[i]
    if v != v or (v == 0'f32 and signbit(v)):
      special = true
      break
  if r <= 2 or special: return repeated(src, w, h, r, grow)
  # ceil(r/2) unit squares plus floor(r/2) unit crosses, saturated at the image extent.
  let extent = max(w, h) - 1
  let square = min(r div 2 + r mod 2, extent)
  let diamond = min(r div 2, max(w + h - 2, 0))
  var cur = maxSquare(src.toOpenArray(0, n - 1), w, h, square, grow)
  if diamond > 0:
    var nxt = newSeq[float32](n)
    var built = 0
    var bits = 0
    while (diamond shr bits) > 0: inc bits
    for bit in countdown(bits - 1, 0):
      if built > 0:
        cross(cur, nxt, w, h, built, grow)
        swap(cur, nxt)
        built *= 2
      if ((diamond shr bit) and 1) != 0:
        cross(cur, nxt, w, h, 1, grow)
        swap(cur, nxt)
        built += 1
  result = cur
  for i in n ..< src.len: result.add src[i]

proc edgeWidth*(img: RgbImage; r: int): seq[float32] =
  ## Per-pixel transition width (pixels) of the guide's luminance: local range / local max
  ## gradient over a window of radius `r` (a sharp step gives ~2, a ramp of width L gives ~L).
  let (w, h) = (img.w, img.h)
  var y = newSeq[float32](w * h)
  for i, p in img.px: y[i] = 0.299'f32 * p[0] + 0.587'f32 * p[1] + 0.114'f32 * p[2]
  var g = newSeq[float32](w * h)
  for yy in 0 ..< h:
    for xx in 0 ..< w:
      let gx = (y[yy * w + min(xx + 1, w - 1)] - y[yy * w + max(xx - 1, 0)]) / 2'f32
      let gy = (y[min(yy + 1, h - 1) * w + xx] - y[max(yy - 1, 0) * w + xx]) / 2'f32
      g[yy * w + xx] = sqrt(gx * gx + gy * gy)
  let gm = maxSquare(g, w, h, r, true)
  let hi = maxSquare(y, w, h, r, true)
  let lo = maxSquare(y, w, h, r, false)
  result = newSeq[float32](w * h)
  for i in 0 ..< w * h: result[i] = (hi[i] - lo[i]) / rmax(gm[i], 1e-4'f32)

# ---------------------------------------------------------------- refine edge brush

proc capsule(p, a, b: (float32, float32); r: float32): float32 =
  ## Coverage (0–1) of a round brush of radius `r` swept from `a` to `b`, at pixel centre `p`.
  let (dx, dy) = (b[0] - a[0], b[1] - a[1])
  let len2 = dx * dx + dy * dy
  let t = (if len2 > 0'f32: clamp(((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / len2, 0'f32, 1'f32) else: 0'f32)
  let (cx, cy) = (a[0] + t * dx, a[1] + t * dy)
  let d = sqrt((p[0] - cx) * (p[0] - cx) + (p[1] - cy) * (p[1] - cy))
  clamp(r + 0.5'f32 - d, 0'f32, 1'f32)

proc dabBounds(p: (float32, float32); r: float32): Rect =
  ## The pixels a disc of radius `r` at `p` can touch, as a (possibly empty) rectangle.
  const lim = 1073741824'f32
  proc lo(v: float32): int = satI32(clamp(floor(v - r - 1'f32), -lim, lim))
  proc hi(v: float32): int = satI32(clamp(ceil(v + r + 1'f32), -lim, lim))
  rect(lo(p[0]), lo(p[1]), hi(p[0]), hi(p[1]))

proc sweptArea*(strokes: openArray[EdgeStroke]): float64 =
  ## The area `strokes` sweep (see `MaxSweptArea`); non-finite strokes count as infinite.
  result = -0.0
  for s in strokes:
    let size = float64(clamp(s.size, BrushSizeMin, BrushSizeMax))
    var len = -0.0
    for i in 1 ..< s.points.len:
      len += float64(hypot(s.points[i][0] - s.points[i - 1][0], s.points[i][1] - s.points[i - 1][1]))
    result += (if len.isFinite: len * size + size * size else: Inf)

proc segments(points: openArray[(float32, float32)]; r: float32): seq[((float32, float32), (float32, float32))] =
  ## The stroke's path as short segments (points closer than a quarter of the radius to the last
  ## kept one dropped, long segments split at most `r` long).
  if points.len == 0: return @[]
  let first = points[0]
  let minStep = rmax(r / 4'f32, 0.5'f32)
  let maxStep = rmax(r, 2'f32)
  var kept = @[first]
  for i in 1 ..< points.len:
    let p = points[i]
    let last = kept[^1]
    if hypot(p[0] - last[0], p[1] - last[1]) >= minStep: kept.add p
  if kept[^1] != points[^1]: kept.add points[^1]
  if kept.len == 1: return @[(first, first)]
  for i in 1 ..< kept.len:
    let (a, b) = (kept[i - 1], kept[i])
    let pieces = asUsize(float64(clamp(ceil(hypot(b[0] - a[0], b[1] - a[1]) / maxStep), 1'f32, 1e6'f32)))
    proc at(k: int): (float32, float32) =
      let t = float32(k) / float32(pieces)
      (a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t)
    for k in 0 ..< pieces: result.add (at(k), at(k + 1))

proc newEdgeBrush*(strokes: openArray[EdgeStroke]; canvas: Rect): Option[EdgeBrush] =
  ## Rasterises `strokes` (clipped to `canvas`). None when they paint nothing or sweep more than
  ## `MaxSweptArea`.
  if sweptArea(strokes) > MaxSweptArea: return none(EdgeBrush)
  var usable: seq[EdgeStroke]
  for s in strokes:
    var ok = s.size.isFinite and s.points.len > 0
    for p in s.points:
      if not (p[0].isFinite and p[1].isFinite): ok = false
    if ok: usable.add s
  proc strokeBounds(s: EdgeStroke): Rect =
    let r = clamp(s.size, BrushSizeMin, BrushSizeMax) / 2'f32
    var acc = EmptyRect
    for p in s.points: acc = acc.union(dabBounds(p, r))
    acc.intersect(canvas)
  var bbox = EmptyRect
  for s in usable: bbox = bbox.union(strokeBounds(s))
  if bbox.isEmpty: return none(EdgeBrush)
  let bw = bbox.width
  var v = newSeq[int8](bw * bbox.height)
  var cov: seq[float32]
  var maxAdd = 0'f32
  for s in usable:
    let sb = strokeBounds(s)
    if sb.isEmpty: continue
    let r = clamp(s.size, BrushSizeMin, BrushSizeMax) / 2'f32
    if s.add: maxAdd = rmax(maxAdd, 2'f32 * r)
    let sw = sb.width
    cov.setLen(0)
    cov.setLen(sw * sb.height)
    # The stroke's coverage: the most any of its segments gives (a single dab for one point).
    for (a, b) in segments(s.points, r):
      let seg = dabBounds(a, r).union(dabBounds(b, r)).intersect(sb)
      for y in seg.y0 ..< seg.y1:
        for x in seg.x0 ..< seg.x1:
          let c = capsule((float32(x) + 0.5'f32, float32(y) + 0.5'f32), a, b, r)
          let i = (y - sb.y0) * sw + (x - sb.x0)
          if i < cov.len and c > cov[i]: cov[i] = c
    let target = (if s.add: 1'f32 else: -1'f32)
    for y in sb.y0 ..< sb.y1:
      for x in sb.x0 ..< sb.x1:
        let ci = (y - sb.y0) * sw + (x - sb.x0)
        let c = (if ci < cov.len: cov[ci] else: 0'f32)
        if c <= 0'f32: continue
        let vi = (y - bbox.y0) * bw + (x - bbox.x0)
        if vi < v.len:
          let cur = float32(v[vi]) / 127'f32
          v[vi] = int8(clamp(round((cur + (target - cur) * c) * 127'f32), -127'f32, 127'f32))
  var anySet = false
  for c in v:
    if c != 0: anySet = true
  if not anySet: return none(EdgeBrush)
  var (x0, y0, x1, y1) = (high(int), high(int), low(int), low(int))
  for row in 0 ..< bbox.height:
    var (f, l) = (-1, -1)
    for x in 0 ..< bw:
      if v[row * bw + x] > 0:
        if f < 0: f = x
        l = x
    if f < 0: continue
    let y = bbox.y0 + row
    x0 = min(x0, bbox.x0 + f)
    x1 = max(x1, bbox.x0 + l + 1)
    y0 = min(y0, y)
    y1 = max(y1, y + 1)
  let added = (if x0 < x1: rect(x0, y0, x1, y1) else: EmptyRect)
  some(EdgeBrush(bbox: bbox, v: v, added: added, window: asUsize(float64(clamp(ceil(maxAdd), 4'f32, 64'f32)))))

proc bounds*(b: EdgeBrush): Rect = b.bbox ## where the strokes paint (added or restored)
proc addedBounds*(b: EdgeBrush): Rect = b.added ## where the strokes add to the refinement area
proc window*(b: EdgeBrush): int = b.window ## box radius of the colour estimate

proc at*(b: EdgeBrush; x, y: int): float32 =
  ## Signed coverage at document pixel `(x, y)` (zero outside the painted area).
  if not b.bbox.contains(x, y): return 0
  let i = (y - b.bbox.y0) * b.bbox.width + (x - b.bbox.x0)
  if i < b.v.len: float32(b.v[i]) / 127'f32 else: 0'f32

proc read*(b: EdgeBrush; r: Rect): seq[float32] =
  ## Signed coverage over `r` (row-major).
  for y in r.y0 ..< r.y1:
    for x in r.x0 ..< r.x1: result.add b.at(x, y)

proc estimate(img: RgbImage; m, brush: seq[float32]; w, h, window: int): seq[float32] =
  ## Coverage estimated from the image where `brush` adds: each pixel's colour projected on the
  ## line between the mean known foreground and background colours within `window` px (or
  ## `4 × window` where there are none that close).
  let n = w * h
  proc known(wantFg: bool): seq[float32] =
    result = newSeq[float32](n)
    for i in 0 ..< n:
      let mv = (if i < m.len: m[i] else: 0'f32)
      let bv = (if i < brush.len: brush[i] else: 0'f32)
      let ok = bv <= 0'f32 and (if wantFg: mv >= 0.98'f32 else: mv <= 0.02'f32)
      result[i] = (if ok: 1'f32 else: 0'f32)
  let (wf, wb) = (known(true), known(false))
  proc means(wt: seq[float32]; r: int): (seq[float32], array[3, seq[float32]]) =
    var sums: array[3, seq[float32]]
    for c in 0 .. 2:
      var v = newSeq[float32](n)
      for i in 0 ..< n: v[i] = img.px[i][c] * wt[i]
      sums[c] = boxMean(v, w, h, r)
    (boxMean(wt, w, h, r), sums)
  let near = [means(wf, window), means(wb, window)]
  let far = [means(wf, window * 4), means(wb, window * 4)]
  proc pick(t: (seq[float32], array[3, seq[float32]]); i: int): Option[array[3, float32]] =
    let d = t[0][i]
    if not (d >= 1e-4'f32): return none(array[3, float32])
    some([t[1][0][i] / d, t[1][1][i] / d, t[1][2][i] / d])
  proc mean(k, i: int): Option[array[3, float32]] =
    result = pick(near[k], i)
    if result.isNone: result = pick(far[k], i)
  result = m
  for i in 0 ..< min(result.len, n):
    if (if i < brush.len: brush[i] else: 0'f32) <= 0'f32: continue
    let (fm, bm) = (mean(0, i), mean(1, i))
    if fm.isNone or bm.isNone: continue
    let px = img.px[i]
    var (num, den) = (0'f32, 0'f32)
    for c in 0 .. 2:
      let (f, b) = (fm.get[c], bm.get[c])
      num += (px[c] - b) * (f - b)
      den += (f - b) * (f - b)
    if den > 1e-4'f32: result[i] = clamp(num / den, 0'f32, 1'f32)

# ---------------------------------------------------------------- refine

proc refineBufferWith(img: Option[RgbImage]; m: seq[float32]; w, h: int; p: RefineParams; brush: Option[BrushBuf];
                      morphology: Morphology): seq[float32] =
  var a = m
  if p.usesImage and img.isSome:
    let im = img.get
    var bin = newSeq[bool](w * h)
    for i in 0 ..< w * h: bin[i] = m[i] >= 0.5'f32
    var boundary = newSeq[bool](w * h)
    var any = false
    for y in 0 ..< h:
      for x in 0 ..< w:
        let i = y * w + x
        let v = bin[i]
        let diff = (x > 0 and bin[i - 1] != v) or (x + 1 < w and bin[i + 1] != v) or (y > 0 and bin[i - w] != v) or
                   (y + 1 < h and bin[i + w] != v)
        boundary[i] = diff
        any = any or diff
    if any:
      let dist = edt(boundary, w, h)
      let r = p.gfRadius
      let q = guidedFilterColor(im, m, r, Eps)
      let widths = (if p.smartRadius: edgeWidth(im, r) else: @[])
      for i in 0 ..< w * h:
        let reff = (if p.smartRadius: clamp(widths[i], 1.5'f32, rmax(p.radius, 1.5'f32)) else: p.radius)
        var wgt = clamp(reff + 1'f32 - dist[i], 0'f32, 1'f32)
        # Restored by the brush: the original edge.
        if brush.isSome and i < brush.get.v.len and brush.get.v[i] < 0'f32: wgt *= 1'f32 + brush.get.v[i]
        if wgt > 0'f32: a[i] = m[i] + (clamp(q[i], 0'f32, 1'f32) - m[i]) * wgt
  # Added by the brush: coverage estimated from the image.
  if brush.isSome and img.isSome:
    var adds = false
    for v in brush.get.v:
      if v > 0'f32: adds = true
    if adds:
      let est = estimate(img.get, m, brush.get.v, w, h, brush.get.window)
      for i in 0 ..< min(a.len, min(est.len, brush.get.v.len)):
        let bi = brush.get.v[i]
        if bi > 0'f32: a[i] += (est[i] - a[i]) * bi
  let ss = p.smoothSigma
  if ss >= 0.3'f32:
    a = gaussianBlur(a, w, h, ss)
    let gain = 1'f32 + ss
    for v in a.mitems: v = clamp((v - 0.5'f32) * gain + 0.5'f32, 0'f32, 1'f32)
  a = gaussianBlur(a, w, h, p.featherSigma)
  if p.contrast > 0'f32:
    let gain = 1'f32 / (1'f32 - 0.99'f32 * clamp(p.contrast, 0'f32, 100'f32) / 100'f32)
    for v in a.mitems: v = clamp((v - 0.5'f32) * gain + 0.5'f32, 0'f32, 1'f32)
  let d = p.shiftPx
  if d != 0: a = morphology(a, w, h, abs(d), d > 0)
  a

proc refineBuffer*(img: Option[RgbImage]; m: seq[float32]; w, h: int; p: RefineParams): seq[float32] =
  ## Refines a soft mask buffer `m` (`w × h`) with `img` as guide (required when `radius > 0`).
  if m.len < w * h: raise newException(ValueError, "mask buffer does not match its size")
  refineBufferWith(img, m, w, h, p, none(BrushBuf), morph)

proc refineMaskWith(sampler: Sampler; mask: proc(r: Rect): seq[float32]; content0, canvas: Rect; p: RefineParams;
                    brush: Option[EdgeBrush]; morphology: Morphology): Option[Region] =
  let content = content0.intersect(canvas)
  if content.isEmpty: return none(Region)
  let bbox = content.inflate(p.spread).union(if brush.isSome: brush.get.addedBounds else: EmptyRect).intersect(canvas)
  let halo = p.halo + (if brush.isSome: brush.get.window * 4 else: 0)
  let bw = bbox.width
  var outp = newSeq[uint8](bw * bbox.height)
  proc q(v: float32): uint8 = asU8(round(clamp(v, 0'f32, 1'f32) * 255'f32))
  var ty = bbox.y0
  while ty < bbox.y1:
    var tx = bbox.x0
    while tx < bbox.x1:
      let t = rect(tx, ty, min(tx + Tile, bbox.x1), min(ty + Tile, bbox.y1))
      tx += Tile
      let r = t.inflate(halo).intersect(canvas)
      let m = mask(r)
      let (rw, rh) = (r.width, r.height)
      if m.len < rw * rh: raise newException(ValueError, "mask reader returned a short buffer")
      let first = (if m.len > 0: m[0] else: 0'f32)
      let (cx, cy, cw, ch) = (t.x0 - r.x0, t.y0 - r.y0, t.width, t.height)
      var painted = none(seq[float32])
      if brush.isSome and not brush.get.bounds.intersect(r).isEmpty: painted = some(brush.get.read(r))
      var adds = false
      if painted.isSome:
        for v in painted.get:
          if v > 0'f32: adds = true
      var uniform = not adds and (first == 0'f32 or first == 1'f32)
      if uniform:
        for v in m:
          if v != first:
            uniform = false
            break
      var buf: seq[float32]
      if uniform:
        for yy in 0 ..< ch:
          let o = (t.y0 - bbox.y0 + yy) * bw + (t.x0 - bbox.x0)
          for xx in 0 ..< cw: outp[o + xx] = q(first)
        continue
      let img = (if p.usesImage or adds: some(sampler.rgb(r)) else: none(RgbImage))
      let bb = (if painted.isSome and brush.isSome: some(BrushBuf(v: painted.get, window: brush.get.window)) else: none(BrushBuf))
      buf = refineBufferWith(img, m, rw, rh, p, bb, morphology)
      for yy in 0 ..< ch:
        let o = (t.y0 - bbox.y0 + yy) * bw + (t.x0 - bbox.x0)
        for xx in 0 ..< cw: outp[o + xx] = q(buf[(cy + yy) * rw + cx + xx])
    ty += Tile
  trimRegion(Region(bbox: bbox, mask: outp))

proc refineMask*(sampler: Sampler; mask: proc(r: Rect): seq[float32]; content, canvas: Rect; p: RefineParams): Option[Region] =
  ## Refines a selection mask. `mask(r)` returns coverage over `r` (row-major); `content` bounds
  ## its non-zero pixels; the result is clipped to `canvas`. `sampler` supplies the guide image
  ## (only read when `radius > 0`). None when nothing remains selected.
  refineMaskWith(sampler, mask, content, canvas, p, none(EdgeBrush), morph)

proc refineMaskBrushed*(sampler: Sampler; mask: proc(r: Rect): seq[float32]; content, canvas: Rect; p: RefineParams;
                        brush: Option[EdgeBrush]): Option[Region] =
  ## `refineMask` with the Refine Edge Brush: where it adds, coverage comes from the image; where
  ## it restores, the original edge stays.
  refineMaskWith(sampler, mask, content, canvas, p, brush, morph)

proc regionReader*(reg: Region): proc(r: Rect): seq[float32] =
  ## A mask reader over a `Region` (zero outside its box).
  let rg = reg
  result = proc(r: Rect): seq[float32] =
    for y in r.y0 ..< r.y1:
      for x in r.x0 ..< r.x1: result.add rg.at(x, y)

proc surfaceReader*(s: Surface): proc(r: Rect): seq[float32] =
  ## A mask reader over a selection surface (channel 0).
  let surf = s
  result = proc(r: Rect): seq[float32] = maskFromSurface(some(surf), r)

proc regionSurface*(reg: Region): Surface =
  ## GRAY8 surface (default 0) holding a region's coverage (a selection or a layer mask).
  result = surfaceFromInterleaved(Gray8, reg.bbox, reg.mask)
  result.prune

proc tilesOf(area: Rect): seq[Rect] =
  var y = area.y0
  while y < area.y1:
    var x = area.x0
    while x < area.x1:
      result.add rect(x, y, min(x + Tile, area.x1), min(y + Tile, area.y1))
      x += Tile
    y += Tile

proc decontaminate*(src: Surface; alpha: Region; radius, amount0: float32): Surface =
  ## Decontaminates colours in the soft fringe of `alpha`: each fringe pixel moves towards the
  ## average colour of nearby fully selected pixels (normalised convolution with a box of radius
  ## `max(radius, 3)`, widened ×4 where no such pixel is near), by `amount`% scaled by its
  ## transparency. Works in the surface's own colour channels (any model / depth).
  let fmt = src.format
  let n = fmt.channels
  let nc = n - int(fmt.alpha)
  let rad = min(asUsize(float64(ceil(rmax(radius, 3'f32)))), 1 shl 16)
  let halo = rad * 4
  let amount = clamp(amount0, 0'f32, 100'f32) / 100'f32
  proc soft(a: float32): bool = a > 0.02'f32 and a < 0.98'f32
  result = src
  for t in tilesOf(alpha.bbox):
    var anySoft = false
    for y in t.y0 ..< t.y1:
      for x in t.x0 ..< t.x1:
        if soft(alpha.at(x, y)): anySoft = true
    if not anySoft: continue
    let r = t.inflate(halo)
    let (rw, rh) = (r.width, r.height)
    var px: seq[float32]
    src.readRegionInto(r, px)
    var al = newSeq[float32](rw * rh)
    for y in r.y0 ..< r.y1:
      for x in r.x0 ..< r.x1: al[(y - r.y0) * rw + (x - r.x0)] = alpha.at(x, y)
    var wts = newSeq[float32](rw * rh)
    for i in 0 ..< rw * rh: wts[i] = (if al[i] >= 0.98'f32: (if fmt.alpha: px[i * n + n - 1] else: 1'f32) else: 0'f32)
    let den = boxMean(wts, rw, rh, rad)
    let den2 = boxMean(wts, rw, rh, rad * 4)
    var nums: seq[(seq[float32], seq[float32])]
    for c in 0 ..< nc:
      var v = newSeq[float32](rw * rh)
      for i in 0 ..< rw * rh: v[i] = wts[i] * px[i * n + c]
      nums.add (boxMean(v, rw, rh, rad), boxMean(v, rw, rh, rad * 4))
    let (cx, cy, cw, ch) = (t.x0 - r.x0, t.y0 - r.y0, t.width, t.height)
    var outv = newSeqOfCap[float32](cw * ch * n)
    for yy in 0 ..< ch:
      for xx in 0 ..< cw:
        let i = (cy + yy) * rw + cx + xx
        let a = al[i]
        let k = (if soft(a): amount * clamp((1'f32 - a) * 4'f32, 0'f32, 1'f32) else: 0'f32)
        for c in 0 ..< n:
          let v = px[i * n + c]
          if c >= nc or k == 0'f32:
            outv.add v
            continue
          let f = (if den[i] > 1e-4'f32: nums[c][0][i] / den[i]
                   elif den2[i] > 1e-4'f32: nums[c][1][i] / den2[i]
                   else: v)
          outv.add v + (f - v) * k
    result.writeRegion(t, outv)

proc maskedCopy*(src: Surface; alpha: Region): Surface =
  ## The pixels of `src` with their alpha multiplied by `alpha` (zero outside its box); the
  ## result has an alpha channel even if `src` does not.
  let fmt = src.format
  let n = fmt.channels
  var outFmt = fmt
  outFmt.alpha = true
  let m = outFmt.channels
  result = newSurface(outFmt)
  for t in tilesOf(alpha.bbox):
    var px: seq[float32]
    src.readRegionInto(t, px)
    var outv = newSeqOfCap[float32](t.width * t.height * m)
    var any = false
    var i = 0
    for y in t.y0 ..< t.y1:
      for x in t.x0 ..< t.x1:
        let a = alpha.at(x, y)
        for c in 0 ..< m - 1: outv.add px[i * n + c]
        let sa = (if fmt.alpha: px[i * n + n - 1] else: 1'f32)
        outv.add sa * a
        any = any or sa * a > 0'f32
        inc i
    if any: result.writeRegion(t, outv)
  result.prune
