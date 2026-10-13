## Separable resampling of surfaces (Image Size, scaling transforms, reduced previews): Keys
## bicubic family (Photoshop's Bicubic / Smoother / Sharper / Automatic), bilinear, Lanczos-3,
## nearest; premultiplied, tile-banded, with the surface default outside its content.
## Rust `photocraft_algo::resample`.

import std/[math, options]
import ../geom, ../color, ../raster
import image

type
  Resample* = enum
    rsNearest = "nearest", rsBilinear = "bilinear", rsBicubic = "bicubic",
    rsBicubicSmoother = "bicubicSmoother", rsBicubicSharper = "bicubicSharper",
    rsBicubicAutomatic = "bicubicAutomatic", rsLanczos = "lanczos", rsPreserveDetails = "preserveDetails"

  KernelKind = enum kNearest, kBilinear, kKeys, kLanczos
  Kernel = object
    kind: KernelKind
    a, stretch: float64

  Pass = object
    sx, sy: float64
    kx, ky: Kernel

  Taps = object
    start: int
    weights: seq[float32]
    outside: float32

  Edge = object
    some: bool
    a, b: int

const
  Phases = 128.0
  ReadBudget = 1 shl 24

var preserveDetailsSharpen*: proc(s: Surface; amount, radius: float32): Surface = nil
  ## Unsharp mask applied after a Preserve Details enlargement. Installed by the filter module
  ## (which depends on this one); without it the enlargement is returned unsharpened.

proc defaultOf*(_: typedesc[Resample]): Resample = rsBicubic

proc kernel(k: KernelKind): Kernel = Kernel(kind: k)

proc keys(m: Resample; s0: float64): Kernel =
  let s = clamp(s0, 0.25, 1.0)
  var (a, stretch) = (-0.75, 1.0)
  case m
  of rsBicubicSmoother: (a, stretch) = (-0.625 - 0.5 * (1.0 - s), 1.15)
  of rsBicubicSharper: (a, stretch) = (-1.0 - 1.6 * (1.0 - s), 1.05)
  else: discard
  Kernel(kind: kKeys, a: a, stretch: stretch / s)

proc keysWeight(x0, a: float64): float64 =
  let x = abs(x0)
  if x < 1.0: (a + 2.0) * x * x * x - (a + 3.0) * x * x + 1.0
  elif x < 2.0: a * (x * x * x - 5.0 * x * x + 8.0 * x - 4.0)
  else: 0.0

proc lanczosWeight(x0: float64): float64 =
  let x = abs(x0)
  if x < 1e-9: 1.0
  elif x < 3.0:
    let px = PI * x
    3.0 * sin(px) * sin(px / 3.0) / (px * px)
  else: 0.0

proc plan(filter: Resample; sx, sy: float64): seq[Pass] =
  proc both(k: Kernel): seq[Pass] = @[Pass(sx: sx, sy: sy, kx: k, ky: k)]
  case filter
  of rsNearest: both(kernel(kNearest))
  of rsBilinear: both(kernel(kBilinear))
  of rsLanczos: both(kernel(kLanczos))
  of rsPreserveDetails:
    if sx > 1.0 or sy > 1.0: both(kernel(kLanczos)) else: plan(rsBicubicSharper, sx, sy)
  of rsBicubicAutomatic:
    if sx <= 1.0 and sy <= 1.0: plan(rsBicubicSharper, sx, sy)
    elif sx >= 1.0 and sy >= 1.0: plan(rsBicubicSmoother, sx, sy)
    elif sx < 1.0:
      @[Pass(sx: sx, sy: 1.0, kx: keys(rsBicubicSharper, sx), ky: kernel(kNearest)),
        Pass(sx: 1.0, sy: sy, kx: kernel(kNearest), ky: keys(rsBicubicSmoother, sy))]
    else:
      @[Pass(sx: 1.0, sy: sy, kx: kernel(kNearest), ky: keys(rsBicubicSharper, sy)),
        Pass(sx: sx, sy: 1.0, kx: keys(rsBicubicSmoother, sx), ky: kernel(kNearest))]
  of rsBicubic, rsBicubicSmoother, rsBicubicSharper:
    let k = keys(filter, if sy != 1.0: sy else: sx)
    if sx == 1.0 or sy == 1.0: both(k)
    elif sy < sx: @[Pass(sx: 1.0, sy: sy, kx: k, ky: k), Pass(sx: sx, sy: 1.0, kx: k, ky: k)]
    else: @[Pass(sx: sx, sy: 1.0, kx: k, ky: k), Pass(sx: 1.0, sy: sy, kx: k, ky: k)]

proc position(o: int; scale: float64; edge: Edge): float64 =
  var u = (float64(o) + 0.5) / scale - 0.5
  if edge.some: u = clamp(u, float64(edge.a), float64(max(edge.b, edge.a)))
  let j = floor(u)
  j + round((u - j) * Phases) / Phases

proc cover(o: int; scale: float64): (int, int) =
  let a = asI32(floor(float64(o) / scale + 1e-9))
  (a, asI32(max(floor(float64(o + 1) / scale + 1e-9), float64(a) + 1.0)))

proc weights(o: int; scale: float64; k: Kernel; edge: Edge): (int, seq[float64]) =
  case k.kind
  of kNearest:
    let u = (float64(o) + 0.5) / scale - 0.5
    (asI32(floor(u + 0.5)), @[1.0])
  of kBilinear:
    if scale < 0.5:
      let (a, b) = cover(o, scale)
      var w = newSeq[float64](max(b - a, 0))
      for i in 0 ..< w.len: w[i] = 1.0 / float64(b - a)
      (a, w)
    else:
      let u = position(o, scale, edge)
      let j = floor(u)
      (asI32(j), @[1.0 - (u - j), u - j])
  of kKeys:
    if scale < 0.25:
      let inner = 4.0 * scale
      var innerEdge = Edge()
      if edge.some:
        innerEdge = Edge(some: true, a: asI32(ceil(float64(edge.a) * inner - 1e-9)),
                         b: asI32(ceil(float64(edge.b + 1) * inner - 1e-9)) - 1)
      let (k0, kw) = weights(o, 0.25, k, innerEdge)
      proc clampK(x: int): int = (if innerEdge.some: clamp(x, innerEdge.a, max(innerEdge.b, innerEdge.a)) else: x)
      var boxes: seq[(int, int)]
      for i in 0 ..< kw.len: boxes.add cover(clampK(k0 + i), inner)
      var lo = 0
      var hi = 0
      if boxes.len > 0:
        lo = high(int)
        for b in boxes: lo = min(lo, b[0])
        hi = low(int)
        for b in boxes: hi = max(hi, b[1])
      else:
        hi = lo
      var w = newSeq[float64](max(hi - lo, 0))
      for i, (a, b) in boxes:
        for s in max(a - lo, 0) ..< min(b - lo, w.len):
          w[s] += kw[i] / float64(b - a)
      (lo, w)
    else:
      let u = position(o, scale, edge)
      let lo = asI32(floor(u - 2.0 * k.stretch))
      let hi = asI32(ceil(u + 2.0 * k.stretch))
      var w: seq[float64]
      var sum = 0.0
      for i in lo .. hi:
        let v = keysWeight((u - float64(i)) / k.stretch, k.a)
        w.add v
        sum += v
      for v in w.mitems: v = v / sum
      (lo, w)
  of kLanczos:
    let stretch = if scale < 1.0: 1.0 / scale else: 1.0
    var u = (float64(o) + 0.5) / scale - 0.5
    if edge.some: u = clamp(u, float64(edge.a), float64(max(edge.b, edge.a)))
    let lo = asI32(ceil(u - 3.0 * stretch))
    let hi = asI32(floor(u + 3.0 * stretch))
    var w: seq[float64]
    var sum = 0.0
    for i in lo .. hi:
      let v = lanczosWeight((float64(i) - u) / stretch)
      w.add v
      sum += v
    let kk = if abs(sum) > 1e-12: 1.0 / sum else: 1.0
    for v in w.mitems: v = v * kk
    (lo, w)

proc taps(dst0, dst1: int; scale: float64; kernel0: Kernel; edge: Edge; content: (int, int)): seq[Taps] =
  proc clampE(i: int): int = (if edge.some: clamp(i, edge.a, edge.b) else: i)
  let k = if scale == 1.0: kernel(kNearest) else: kernel0
  for o in dst0 ..< dst1:
    let (lo, w) = weights(o, scale, k, edge)
    let hi = lo + w.len - 1
    let start = clampE(lo)
    var folded = newSeq[float32](max(clampE(hi) - start + 1, 1))
    for i in lo .. hi:
      let slot = clampE(i) - start
      if slot >= 0 and slot < folded.len: folded[slot] += float32(w[i - lo])
    let (a, b) = (max(start, content[0]), min(start + folded.len - 1, content[1]))
    if a > b:
      var s = 0'f32
      for v in folded: s += v
      result.add Taps(start: content[0], outside: s)
      continue
    let (i0, i1) = (a - start, b - start + 1)
    var outside = 0'f32
    for i in 0 ..< i0: outside += folded[i]
    for i in i1 ..< folded.len: outside += folded[i]
    result.add Taps(start: a, weights: folded[i0 ..< i1], outside: outside)

proc scaledRect*(r: Rect; sx, sy: float64): Rect =
  if r.isEmpty: return EmptyRect
  rect(asI32(floor(float64(r.x0) * sx)), asI32(floor(float64(r.y0) * sy)),
       asI32(ceil(float64(r.x1) * sx)), asI32(ceil(float64(r.y1) * sy)))

proc horizontal(row: openArray[float32]; outp: var openArray[float32]; hx: openArray[Taps]; xLo: int;
                def: openArray[float32]; n: int) =
  ## One output row from one premultiplied source row (`row` starts at `xLo`).
  let pixels = row.len div max(n, 1)
  for ti in 0 ..< hx.len:
    let t = hx[ti]
    let d = ti * n
    if d + n > outp.len: break
    for c in 0 ..< n: outp[d + c] = 0
    for k, w in t.weights:
      let sx = t.start + k - xLo
      if sx >= 0 and sx < pixels:
        let sp = sx * n
        for c in 0 ..< n: outp[d + c] += row[sp + c] * w
    for c in 0 ..< n:
      outp[d + c] += (if c < def.len: def[c] else: 0'f32) * t.outside

proc resizePass(s: Surface; pass: Pass; canvas: Rect; hasCanvas: bool; budget: int): Surface =
  var edgeRect = EmptyRect
  var hasEdge = false
  if hasCanvas:
    edgeRect = s.contentBounds.union(canvas)
    hasEdge = not edgeRect.isEmpty
  let fmt = s.format
  result = surfaceWithDefault(fmt, s.defaultPixel)
  let src = s.contentBounds
  let dst = scaledRect(src, pass.sx, pass.sy)
  if dst.isEmpty: return
  let n = fmt.channels
  let alpha = fmt.alpha
  let area = if hasEdge: edgeRect else: src
  let ex = if hasEdge: Edge(some: true, a: edgeRect.x0, b: edgeRect.x1 - 1) else: Edge()
  let ey = if hasEdge: Edge(some: true, a: edgeRect.y0, b: edgeRect.y1 - 1) else: Edge()
  let hx = taps(dst.x0, dst.x1, pass.sx, pass.kx, ex, (area.x0, area.x1 - 1))
  let vy = taps(dst.y0, dst.y1, pass.sy, pass.ky, ey, (area.y0, area.y1 - 1))
  var xLo = high(int)
  for t in hx:
    if t.weights.len > 0: xLo = min(xLo, t.start)
  if xLo == high(int): xLo = area.x0
  var xHi = low(int)
  for t in hx: xHi = max(xHi, t.start + t.weights.len)
  if hx.len == 0: xHi = area.x0
  xHi = max(xHi, xLo)
  var def = s.defaultPixel
  premultiply(def, n, alpha)
  let dw = dst.width
  const Band = 64
  var defRow = newSeq[float32](dw * n)
  block:
    var row = newSeq[float32]((xHi - xLo) * n)
    for i in 0 ..< row.len: row[i] = def[i mod n]
    horizontal(row, defRow, hx, xLo, def, n)
  let rowsPerRead = clamp(budget div max(max(xHi - xLo, 1) * n, dw * n), 1, TileSize)
  var rows: seq[float32]
  var hrows: seq[float32]
  var b0 = 0
  while b0 < vy.len:
    let b1 = min(b0 + Band, vy.len)
    var yLo = high(int)
    for i in b0 ..< b1:
      if vy[i].weights.len > 0: yLo = min(yLo, vy[i].start)
    if yLo == high(int): yLo = area.y0
    var yHi = low(int)
    for i in b0 ..< b1: yHi = max(yHi, vy[i].start + vy[i].weights.len)
    if b1 <= b0: yHi = area.y0
    yHi = max(yHi, yLo)
    let rowLen = dw * n
    var res = newSeq[float32]((b1 - b0) * rowLen)
    var y = yLo
    while y < yHi:
      let y1 = min((divEuclid(y, TileSize) + 1) * TileSize, yHi)
      if s.hasTilesIn(rect(xLo, y, xHi, y1)):
        var c0 = y
        while c0 < y1:
          let c1 = min(c0 + rowsPerRead, y1)
          let rd = rect(xLo, c0, xHi, c1)
          s.readRegionInto(rd, rows)
          premultiply(rows, n, alpha)
          hrows.setLen((c1 - c0) * rowLen)
          let rw = max(rd.width * n, 1)
          for r in 0 ..< c1 - c0:
            if (r + 1) * rw > rows.len: break
            horizontal(rows.toOpenArray(r * rw, (r + 1) * rw - 1),
                       hrows.toOpenArray(r * rowLen, (r + 1) * rowLen - 1), hx, xLo, def, n)
          for r in 0 ..< c1 - c0:
            let yy = c0 + r
            for bi in b0 ..< b1:
              let t = vy[bi]
              let wi = yy - t.start
              if yy >= t.start and wi < t.weights.len:
                let w = t.weights[wi]
                let o = (bi - b0) * rowLen
                let h = r * rowLen
                for k in 0 ..< rowLen: res[o + k] += hrows[h + k] * w
          c0 = c1
      else:
        for bi in b0 ..< b1:
          let t = vy[bi]
          let a = min(max(y - t.start, 0), t.weights.len)
          let b = min(max(y1 - t.start, 0), t.weights.len)
          var w = 0'f32
          for i in a ..< b: w += t.weights[i]
          if w != 0:
            let o = (bi - b0) * rowLen
            for k in 0 ..< rowLen: res[o + k] += defRow[k] * w
      y = y1
    for bi in b0 ..< b1:
      let t = vy[bi]
      let o = (bi - b0) * rowLen
      for k in 0 ..< rowLen: res[o + k] += defRow[k] * t.outside
    if alpha:
      var p = n - 1
      while p < res.len:
        res[p] = clamp(res[p], 0'f32, 1'f32)
        p += n
    unpremultiply(res, n, alpha)
    result.writeRegion(rect(dst.x0, dst.y0 + b0, dst.x1, dst.y0 + b1), res)
    b0 = b1

proc resizeWithBudget(s: Surface; sx, sy: float64; filter: Resample; canvas: Rect; hasCanvas: bool;
                      budget: int): Surface =
  var cur = s
  var started = false
  var canvas = canvas
  for p in plan(filter, sx, sy):
    let next = resizePass(cur, p, canvas, hasCanvas, budget)
    if hasCanvas: canvas = scaledRect(canvas, p.sx, p.sy)
    cur = next
    started = true
  result = cur
  if filter == rsPreserveDetails and (sx > 1.0 or sy > 1.0) and not preserveDetailsSharpen.isNil:
    result = preserveDetailsSharpen(result, 30, 0.6'f32 * float32(max(sx, sy)))
  result.prune()
  discard started

proc resizeSurface*(s: Surface; sx, sy: float64; filter: Resample): Surface =
  resizeWithBudget(s, sx, sy, filter, EmptyRect, false, ReadBudget)

proc resizeSurfaceInCanvas*(s: Surface; sx, sy: float64; filter: Resample; canvas: Rect): Surface =
  resizeWithBudget(s, sx, sy, filter, canvas, true, ReadBudget)

proc resizeSurfaceBudget*(s: Surface; sx, sy: float64; filter: Resample; budget: int): Surface =
  ## Test hook: resize reading at most `budget` floats per source band.
  resizeWithBudget(s, sx, sy, filter, EmptyRect, false, budget)

proc translateSurface*(s: Surface; dx, dy: int): Surface = s.translated(dx, dy, s.contentBounds)

proc cropSurface*(s: Surface; keep: Rect): Surface =
  result = surfaceWithDefault(s.format, s.defaultPixel)
  let r = s.contentBounds.intersect(keep)
  if not r.isEmpty: result.writeInterleaved(r, s.toInterleaved(r))

proc parseResample*(s: string): Option[Resample] =
  for r in Resample:
    if $r == s: return some(r)
  none(Resample)
