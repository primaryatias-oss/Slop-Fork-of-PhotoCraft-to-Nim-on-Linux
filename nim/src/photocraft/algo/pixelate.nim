## Pixelate: Color Halftone, Crystallize, Facet, Fragment, Mezzotint, Pointillize (port of
## `pixelate.rs`, `pixelate/facet.rs` and `pixelate/screen.rs`).
##
## Cell layouts are anchored at the reference bounds' origin and random choices hash document
## coordinates, so output is independent of tiling.

import std/[math, options]
import ../geom
import params, image, fxutil, noise, util
import ../rmath

# Run-time values, not `const`: Nim folds arithmetic on float32 constants in float64, which
# differs from Rust's f32 arithmetic (`1 - FracPi4` would come out one ulp off).
let
  Pi32 = float32(PI)
  FracPi4 = float32(PI / 4)
  Frac1Sqrt2 = float32(1 / sqrt(2.0))
  Sqrt2 = float32(sqrt(2.0))

# ---------------------------------------------------------------- halftone screen cache

type Screen = object
  ## Tile-local memoization of dot radii in rotated screen coordinates (a cell's radius doesn't
  ## depend on the output pixel).
  x0, y0, width, height: int
  radii: seq[float32]
  known: seq[bool]

proc newScreen(o: Rect; origin: (float32, float32); rotation: (float32, float32); cell: float32): Option[Screen] =
  if o.isEmpty or not cell.isFinite or cell < 1: return
  let (s, co) = rotation
  var (loX, loY, hiX, hiY) = (Inf.float32, Inf.float32, NegInf.float32, NegInf.float32)
  for y in [o.y0, o.y1 - 1]:
    for x in [o.x0, o.x1 - 1]:
      let (fx, fy) = (float32(x) + 0.5'f32 - origin[0], float32(y) + 0.5'f32 - origin[1])
      let (u, v) = ((fx * co + fy * s) / cell, (-fx * s + fy * co) / cell)
      if not u.isFinite or not v.isFinite or abs(u) > 4_000_000 or abs(v) > 4_000_000: return
      loX = rmin(loX, u)
      loY = rmin(loY, v)
      hiX = rmax(hiX, u)
      hiY = rmax(hiY, v)
  let (x0, y0) = (asI32(float64(floor(loX))) - 2, asI32(float64(floor(loY))) - 2)
  let width = asI32(float64(floor(hiX))) + 3 - x0
  let height = asI32(float64(floor(hiY))) + 3 - y0
  if width < 0 or height < 0 or width * height > 256 * 1024: return
  some(Screen(x0: x0, y0: y0, width: width, height: height, radii: newSeq[float32](width * height),
              known: newSeq[bool](width * height)))

template screenRadius(sc: var Screen; x, y: float32; compute: untyped): float32 =
  block:
    var slot = -1
    if x.isFinite and y.isFinite and abs(x) <= 4_000_001 and abs(y) <= 4_000_001:
      let (cx, cy) = (asI32(float64(x)) - sc.x0, asI32(float64(y)) - sc.y0)
      if cx >= 0 and cy >= 0 and cx < sc.width and cy < sc.height: slot = cy * sc.width + cx
    if slot >= 0:
      if not sc.known[slot]:
        sc.radii[slot] = compute
        sc.known[slot] = true
      sc.radii[slot]
    else: compute

# ---------------------------------------------------------------- Color Halftone

proc colorHalftone*(src: Image; o: Rect; ctx: Ctx; maxRadius: float32; angles: array[4, float32]): seq[float32] =
  ## Colour Halftone: each channel is screened at its own angle into dots whose area follows the
  ## channel's ink (1 − value in additive models).
  let n = src.ch
  let cc = ncol(ctx, n)
  let rmaxv = rmax(maxRadius, 1)
  # Cell side such that a full-size dot just covers the cell's corners.
  let cell = rmaxv * Sqrt2
  let sub = subtractive(ctx)
  let rgb = viaRgb(ctx)
  result = src.crop(o)
  let b = ctx.bounds
  let (ox, oy) = (float32(b.x0), float32(b.y0))
  var rot: seq[(float32, float32)]
  for c in 0 ..< max(cc, 3):
    let a = degToRad(angles[min(c, 3)])
    rot.add (sin(a), cos(a))
  let clip = b.intersect(src.rect)
  if clip.isEmpty: return
  proc value(x, y: float32; c: int): float32 =
    ## Channel value at a point averaged over five taps (for Lab, of the RGB conversion).
    let d = cell * 0.25'f32
    var acc = 0'f32
    var px: array[MaxC, float32]
    for (dx, dy) in [(0'f32, 0'f32), (-d, -d), (d, -d), (-d, d), (d, d)]:
      # Clamp to the bounds so cells straddling the edge read real pixels.
      let ix = clamp(asI32(float64(floor(x + dx))), clip.x0, clip.x1 - 1)
      let iy = clamp(asI32(float64(floor(y + dy))), clip.y0, clip.y1 - 1)
      if rgb:
        for k in 0 ..< n: px[k] = src.get(ix, iy, k)
        acc += rgba(ctx, px.toOpenArray(0, n - 1))[c]
      else:
        acc += src.get(ix, iy, c)
    acc / 5
  let w = o.width
  let chans = if rgb: 3 else: cc
  var screens: array[MaxC, Option[Screen]]
  for c in 0 ..< min(chans, MaxC):
    if c < rot.len: screens[c] = newScreen(o, (ox, oy), rot[c], cell)
  var vals: array[MaxC, float32]
  var i = 0
  while i * n + n <= result.len:
    let at = i * n
    let (x, y) = (o.x0 + i mod w, o.y0 + i div w)
    let (fx, fy) = (float32(x) + 0.5'f32 - ox, float32(y) + 0.5'f32 - oy)
    for c in 0 ..< chans:
      let (s, co) = rot[c]
      # Rotate into screen space.
      let (u, w2) = (fx * co + fy * s, -fx * s + fy * co)
      let (cu, cv) = (floor(u / cell), floor(w2 / cell))
      var cov = 0'f32
      for dj in -1 .. 1:
        for di in -1 .. 1:
          let (mu, mv) = ((cu + float32(di) + 0.5'f32) * cell, (cv + float32(dj) + 0.5'f32) * cell)
          let d = sqrt((u - mu) * (u - mu) + (w2 - mv) * (w2 - mv))
          if d >= rmaxv + 0.5'f32: continue
          # Back to document space to read the cell's value.
          let (dx, dy) = (mu * co - mv * s + ox, mu * s + mv * co + oy)
          template radius(): float32 =
            (block:
              let raw = clamp(value(dx, dy, c), 0'f32, 1'f32)
              let ink = if sub and not rgb: raw else: 1 - raw
              if ink <= FracPi4: cell * sqrt(ink / Pi32)
              else: cell * (0.5'f32 + (ink - FracPi4) / (1 - FracPi4) * (Frac1Sqrt2 - 0.5'f32)))
          let r =
            if c < MaxC and screens[c].isSome: screenRadius(screens[c].get, cu + float32(di), cv + float32(dj), radius())
            else: radius()
          # Tiny dots cannot cover more than their own area.
          cov = rmax(cov, rmin(clamp(r - d + 0.5'f32, 0'f32, 1'f32), Pi32 * r * r))
      vals[c] = if sub and not rgb: cov else: 1 - cov
    if rgb:
      let a = if ctx.alpha: result[at + n - 1] else: 1'f32
      setRgba(ctx, result.toOpenArray(at, at + n - 1), [vals[0], vals[1], vals[2], a])
    else:
      for c in 0 ..< cc: result[at + c] = vals[c]
    inc i

# ---------------------------------------------------------------- Crystallize

proc crystallize*(src: Image; o: Rect; ctx: Ctx; cellSize: float32; seed: uint32): seq[float32] =
  ## Crystallize: Voronoi cells around jittered points, each filled with the (premultiplied)
  ## average colour of its pixels.
  let n = src.ch
  let cell = rmax(cellSize, 2)
  let b = ctx.bounds
  let origin = (float32(b.x0), float32(b.y0))
  let c0 = (asI32(float64(floor(float32(o.x0 - b.x0) / cell))) - 2, asI32(float64(floor(float32(o.y0 - b.y0) / cell))) - 2)
  let c1 = (asI32(float64(floor(float32(o.x1 - b.x0) / cell))) + 2, asI32(float64(floor(float32(o.y1 - b.y0) / cell))) + 2)
  let (gw, gh) = (c1[0] - c0[0] + 1, c1[1] - c0[1] + 1)
  proc owner(x, y: int): (int, int) =
    let (fx, fy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
    let (cx, cy) = (asI32(float64(floor((fx - origin[0]) / cell))), asI32(float64(floor((fy - origin[1]) / cell))))
    var best = (F32Max, (cx, cy))
    for dy in -1 .. 1:
      for dx in -1 .. 1:
        let (px, py) = cellPoint(cx + dx, cy + dy, cell, seed, origin)
        let d = (px - fx) * (px - fx) + (py - fy) * (py - fy)
        if d < best[0]: best = (d, (cx + dx, cy + dy))
    best[1]
  var acc = newSeq[float32](gw * gh * (n + 1))
  let win = rect(b.x0 + asI32(float64(floor(float32(c0[0]) * cell))), b.y0 + asI32(float64(floor(float32(c0[1]) * cell))),
                 b.x0 + asI32(float64(ceil(float32(c1[0] + 1) * cell))), b.y0 + asI32(float64(ceil(float32(c1[1] + 1) * cell)))
                ).intersect(src.rect).intersect(b)
  var owners = newSeq[(int, int)](o.width * o.height)
  for y in win.y0 ..< win.y1:
    for x in win.x0 ..< win.x1:
      let (cx, cy) = owner(x, y)
      if o.contains(x, y): owners[(y - o.y0) * o.width + (x - o.x0)] = (cx, cy)
      if cx < c0[0] or cy < c0[1] or cx > c1[0] or cy > c1[1]: continue
      let k = ((cy - c0[1]) * gw + (cx - c0[0])) * (n + 1)
      let a = if ctx.alpha: src.get(x, y, n - 1) else: 1'f32
      for c in 0 ..< n:
        let s = src.get(x, y, c)
        acc[k + c] += (if ctx.alpha and c < n - 1: s * a else: s)
      acc[k + n] += 1
  # Pixels of `o` outside the bounds keep their own value; resolve owners for any missed.
  result = src.crop(o)
  var i = 0
  while i * n + n <= result.len:
    let (x, y) = xy(o, i)
    if b.contains(x, y):
      let (cx, cy) = if win.contains(x, y): owners[i] else: owner(x, y)
      if not (cx < c0[0] or cy < c0[1] or cx > c1[0] or cy > c1[1]):
        let k = ((cy - c0[1]) * gw + (cx - c0[0])) * (n + 1)
        let cnt = acc[k + n]
        if cnt > 0:
          for c in 0 ..< n: result[i * n + c] = acc[k + c] / cnt
          unpremulPx(result.toOpenArray(i * n, i * n + n - 1), ctx.alpha)
    inc i

# ---------------------------------------------------------------- Facet

proc facetDirect(src: Image; o: Rect; ctx: Ctx): seq[float32] =
  let n = src.ch
  let r = 2
  result = src.crop(o)
  var tmp: array[MaxC, float32]
  var i = 0
  while i * n + n <= result.len:
    let (x, y) = xy(o, i)
    var bestVar = F32Max
    var bestMean: array[MaxC, float32]
    for (qx, qy) in [(-r, -r), (0, -r), (-r, 0), (0, 0)]:
      var sum: array[MaxC, float32]
      var (s, s2) = (0'f32, 0'f32)
      for yy in y + qy .. y + qy + r:
        for xx in x + qx .. x + qx + r:
          for c in 0 ..< n:
            tmp[c] = src.get(xx, yy, c)
            sum[c] += tmp[c]
          let l = luma(ctx, tmp.toOpenArray(0, n - 1))
          s += l
          s2 += l * l
      let cnt = float32((r + 1) * (r + 1))
      let v = s2 / cnt - (s / cnt) * (s / cnt)
      if v < bestVar:
        for k in 0 ..< MaxC: sum[k] /= cnt
        bestVar = v
        bestMean = sum
    for c in 0 ..< n: result[i * n + c] = bestMean[c]
    inc i

type Quadrant = object
  variance: float32
  mean: array[MaxC, float32]

proc facetRun(src: Image; o: Rect; ctx: Ctx): Option[seq[float32]] =
  ## Each 3×3 quadrant is computed once and reused at its four output pixels (same sums as
  ## `facetDirect`, in the same order).
  let n = src.ch
  if n == 0 or n > MaxC or o.isEmpty: return
  let win = rect(o.x0 - 2, o.y0 - 2, o.x1 + 2, o.y1 + 2)
  if win.intersect(src.rect) != win: return
  let (w, h) = (win.width, win.height)
  if w * h > 1024 * 1024: return
  let stride = src.rect.width * n
  let offset = (win.x0 - src.rect.x0) * n
  template rowStart(y: int): int = (y - src.rect.y0) * stride + offset
  var lum = newSeqOfCap[float32](w * h)
  for y in win.y0 ..< win.y1:
    let s = rowStart(y)
    for x in 0 ..< w: lum.add luma(ctx, src.data.toOpenArray(s + x * n, s + x * n + n - 1))
  let qw = w - 2
  var quads: array[3, seq[Quadrant]]
  for line in quads.mitems: line = newSeq[Quadrant](qw)
  proc buildRow(y: int; line: var seq[Quadrant]) =
    for x in 0 ..< qw:
      var sum: array[MaxC, float32]
      var (s, s2) = (0'f32, 0'f32)
      for dy in 0 .. 2:
        let start = rowStart(win.y0 + y + dy) + x * n
        for k in 0 .. 2:
          for c in 0 ..< n: sum[c] += src.data[start + k * n + c]
          let l = lum[(y + dy) * w + x + k]
          s += l
          s2 += l * l
      for c in 0 ..< MaxC: sum[c] /= 9
      line[x] = Quadrant(variance: s2 / 9 - (s / 9) * (s / 9), mean: sum)
  buildRow(0, quads[0])
  buildRow(1, quads[1])
  let (width, height) = (o.width, o.height)
  var res = newSeqOfCap[float32](width * height * n)
  for y in 0 ..< height:
    buildRow(y + 2, quads[2])
    for x in 0 ..< width:
      var best = Quadrant(variance: F32Max)
      # Preserve quadrant order, including exact ties and non-finite variances.
      for q in [quads[0][x], quads[0][x + 2], quads[2][x], quads[2][x + 2]]:
        if q.variance < best.variance: best = q
      for c in 0 ..< n: res.add best.mean[c]
    let first = move quads[0]
    quads[0] = move quads[1]
    quads[1] = move quads[2]
    quads[2] = first
  some(res)

proc facet*(src: Image; o: Rect; ctx: Ctx): seq[float32] =
  ## Facet: a Kuwahara filter (radius 2): each pixel takes the mean of the least-varied of its
  ## four overlapping quadrants, clumping similar colours into flat facets while keeping edges.
  let r = facetRun(src, o, ctx)
  if r.isSome: r.get else: facetDirect(src, o, ctx)

# ---------------------------------------------------------------- Fragment, Mezzotint, Pointillize

proc fragment*(src: Image; o: Rect; ctx: Ctx): seq[float32] =
  ## Fragment: the average of four copies offset by 4 px (left, right, up, down).
  let n = src.ch
  result = newSeqOfCap[float32](o.width * o.height * n)
  var acc: array[MaxC, float32]
  for y in o.y0 ..< o.y1:
    for x in o.x0 ..< o.x1:
      for c in 0 ..< n: acc[c] = 0
      for (dx, dy) in [(-4, 0), (4, 0), (0, -4), (0, 4)]:
        let a = if ctx.alpha: src.get(x + dx, y + dy, n - 1) else: 1'f32
        for c in 0 ..< n:
          let s = src.get(x + dx, y + dy, c)
          acc[c] += (if ctx.alpha and c < n - 1: s * a else: s) * 0.25'f32
      unpremulPx(acc.toOpenArray(0, n - 1), ctx.alpha)
      for c in 0 ..< n: result.add acc[c]

proc mezzoThreshold(kind: MezzotintType; x, y: int; c, seed: uint32): float32 =
  ## Random threshold for Mezzotint pattern `kind` at a pixel.
  proc lines(len: int): float32 =
    let phase = asI32(float64(hash01(0, y, c + 40, seed) * float32(len)))
    hash01(divEuclid(x + phase, len), y, c, seed)
  proc strokes(len: int): float32 =
    let d = x - y
    let phase = asI32(float64(hash01(d, 0, c + 50, seed) * float32(len)))
    hash01(divEuclid(x + y + phase, len), d, c + 60, seed)
  case kind
  of mzFineDots: hash01(x, y, c, seed)
  of mzMediumDots: hash01(divEuclid(x, 2), divEuclid(y, 2), c, seed)
  of mzGrainyDots: 0.5'f32 * hash01(x, y, c, seed) + 0.5'f32 * hash01(divEuclid(x, 3), divEuclid(y, 3), c + 7, seed)
  of mzCoarseDots: hash01(divEuclid(x, 3), divEuclid(y, 3), c, seed)
  of mzShortLines: lines(6)
  of mzMediumLines: lines(14)
  of mzLongLines: lines(32)
  of mzShortStrokes: strokes(5)
  of mzMediumStrokes: strokes(10)
  of mzLongStrokes: strokes(20)

proc mezzotint*(src: Image; o: Rect; ctx: Ctx; kind: MezzotintType; seed: uint32): seq[float32] =
  ## Mezzotint: every colour channel is thresholded against a random pattern, giving pure channel
  ## values (fully saturated dots/lines in colour images).
  let n = src.ch
  let cc = ncol(ctx, n)
  let rgb = viaRgb(ctx)
  result = src.crop(o)
  var i = 0
  while i * n + n <= result.len:
    let (x, y) = xy(o, i)
    if rgb:
      var c = rgba(ctx, result.toOpenArray(i * n, i * n + n - 1))
      for k in 0 ..< 3: c[k] = (if c[k] > mezzoThreshold(kind, x, y, uint32(k), seed): 1 else: 0)
      setRgba(ctx, result.toOpenArray(i * n, i * n + n - 1), c)
    else:
      for k in 0 ..< cc:
        result[i * n + k] = (if result[i * n + k] > mezzoThreshold(kind, x, y, uint32(k), seed): 1 else: 0)
    inc i

proc pointillize*(src: Image; o: Rect; ctx: Ctx; cellSize: float32; seed: uint32; background: array[4, float32]): seq[float32] =
  ## Pointillize: randomly sized dots, coloured from the image at their centre, on a canvas of the
  ## background colour.
  let n = src.ch
  let cell = rmax(cellSize, 2)
  let b = ctx.bounds
  let origin = (float32(b.x0), float32(b.y0))
  let bg = native(ctx, background)
  result = src.crop(o)
  var dot: array[MaxC, float32]
  var i = 0
  while i * n + n <= result.len:
    let at = i * n
    let (x, y) = xy(o, i)
    let (fx, fy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
    let (cx, cy) = (asI32(float64(floor((fx - origin[0]) / cell))), asI32(float64(floor((fy - origin[1]) / cell))))
    # The dot whose edge the pixel is deepest inside wins.
    var best = none((float32, int, int, float32, float32))
    for dy in -1 .. 1:
      for dx in -1 .. 1:
        let (gx, gy) = (cx + dx, cy + dy)
        let (px0, py0) = cellPoint(gx, gy, cell, seed, origin)
        let rad = cell * (0.45'f32 + 0.35'f32 * hash01(gx, gy, 13, seed))
        let d = sqrt((px0 - fx) * (px0 - fx) + (py0 - fy) * (py0 - fy))
        let depth = rad - d
        if depth > -0.5'f32 and (best.isNone or depth > best.get[0]): best = some((depth, gx, gy, px0, py0))
    let aOrig = if ctx.alpha: result[at + n - 1] else: 1'f32
    # Canvas first, then the dot over it with an anti-aliased edge.
    var canvas: array[MaxC, float32]
    for c in 0 ..< n: canvas[c] = bg[c]
    if ctx.alpha: canvas[n - 1] = rmax(aOrig, rmin(bg[n - 1], 1))
    if best.isSome:
      let (depth, gx, gy, px0, py0) = best.get
      let (sx, sy) = (asI32(float64(floor(px0))), asI32(float64(floor(py0))))
      for c in 0 ..< n: dot[c] = src.get(sx, sy, c)
      # Slight per-dot brightness jitter, as in pointillist strokes.
      let j = (hash01(gx, gy, 17, seed) - 0.5'f32) * 0.08'f32
      let cc = ncol(ctx, n)
      let sub = subtractive(ctx)
      for c in 0 ..< cc: dot[c] = clamp(dot[c] + (if sub: -j else: j), 0'f32, 1'f32)
      let k = clamp(depth + 0.5'f32, 0'f32, 1'f32)
      for c in 0 ..< n: canvas[c] += (dot[c] - canvas[c]) * k
    for c in 0 ..< n: result[at + c] = canvas[c]
    inc i
