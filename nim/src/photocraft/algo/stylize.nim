## Mosaic, Emboss, Find Edges, Solarize/Invert (per pixel), Desaturate (port of `stylize.rs`).

import std/math
import ../geom, ../color, ../raster
import params, image, util

proc fmtOf(ctx: Ctx): PixelFormat = pixelFormat(ctx.mode, stF32, ctx.alpha)

proc lumaOf(ctx: Ctx; px: openArray[float32]): float32 =
  let c = toRgba(fmtOf(ctx), px)
  0.299'f32 * c[0] + 0.587'f32 * c[1] + 0.114'f32 * c[2]

proc setGray(ctx: Ctx; px: var openArray[float32]; g: float32) =
  ## Replaces the colour with gray `g` in the pixel's own model (alpha kept).
  let a = if ctx.alpha: px[px.len - 1] else: 1'f32
  let v = fromRgba(fmtOf(ctx), [g, g, g, a])
  for i in 0 ..< px.len: px[i] = v[i]

template perPixel*(src: Image; o: Rect; ctx: Ctx; f: untyped): seq[float32] =
  ## Colour channels through `f` (of `it`; alpha untouched).
  block:
    let n = src.ch
    let cc = if ctx.alpha: n - 1 else: n
    var res = src.crop(o)
    var i = 0
    while i + n <= res.len:
      for c in 0 ..< cc:
        let it {.inject.} = res[i + c]
        res[i + c] = f
      i += n
    res

proc desaturate*(src: Image; o: Rect; ctx: Ctx): seq[float32] =
  let n = src.ch
  result = src.crop(o)
  var i = 0
  while i + n <= result.len:
    let g = lumaOf(ctx, result.toOpenArray(i, i + n - 1))
    setGray(ctx, result.toOpenArray(i, i + n - 1), g)
    i += n

proc mosaic*(src: Image; o: Rect; ctx: Ctx; cell: float32): seq[float32] =
  ## Mosaic cells anchored at the bounds' origin; each cell becomes its (premultiplied) average
  ## over the part inside the bounds.
  let n = src.ch
  let (width, height) = (o.width, o.height)
  if width == 0 or height == 0 or n == 0: return
  let cs = asI32(float64(round(rmax(cell, 1))))
  let b = ctx.bounds
  let rowStride = width * n
  result = newSeq[float32](rowStride * height)
  var acc = newSeq[float32](n)
  let cx0 = divEuclid(o.x0 - b.x0, cs)
  let cx1 = divEuclid(o.x1 - 1 - b.x0, cs)
  let cy0 = divEuclid(o.y0 - b.y0, cs)
  let cy1 = divEuclid(o.y1 - 1 - b.y0, cs)
  for cy in cy0 .. cy1:
    for cx in cx0 .. cx1:
      let cellRect = rect(b.x0 + cx * cs, b.y0 + cy * cs, b.x0 + (cx + 1) * cs, b.y0 + (cy + 1) * cs)
      let r = cellRect.intersect(b)
      for v in acc.mitems: v = 0
      var count = 0'f32
      for yy in r.y0 ..< r.y1:
        for xx in r.x0 ..< r.x1:
          let a = if ctx.alpha: src.get(xx, yy, n - 1) else: 1'f32
          for c in 0 ..< n:
            let s = src.get(xx, yy, c)
            acc[c] += (if ctx.alpha and c < n - 1: s * a else: s)
          count += 1
      if count > 0:
        for v in acc.mitems: v /= count
      if ctx.alpha:
        let a = acc[n - 1]
        for c in 0 ..< n - 1: acc[c] = (if a > 1e-7'f32: acc[c] / a else: 0)
      # The full cell clips the output, keeping partial cells beyond the averaging bounds.
      let wr = cellRect.intersect(o)
      for row in 0 ..< wr.height:
        let ry = wr.y0 - o.y0 + row
        for px in 0 ..< wr.width:
          let at = ry * rowStride + (wr.x0 - o.x0 + px) * n
          for c in 0 ..< n: result[at + c] = acc[c]

proc emboss*(src: Image; o: Rect; ctx: Ctx; angle, height, amount: float32): seq[float32] =
  ## Emboss: gray relief from luminance differences along the light angle.
  let n = src.ch
  let rad = degToRad(angle)
  let (s, c) = (sin(rad), cos(rad))
  let h = rmax(height, 1)
  let (dx, dy) = (asI32(float64(round(c * h))), asI32(float64(round(-s * h))))
  let k = amount / 100
  result = src.crop(o)
  var tmp = newSeq[float32](n)
  template lumAt(x, y: int; tmp: var seq[float32]): float32 =
    (for ci in 0 ..< n: tmp[ci] = src.get(x, y, ci); lumaOf(ctx, tmp))
  let w = o.width
  var i = 0
  while i * n + n <= result.len:
    let (x, y) = (o.x0 + i mod w, o.y0 + i div w)
    let g = 0.5'f32 + k * (lumAt(x + dx, y + dy, tmp) - lumAt(x - dx, y - dy, tmp))
    setGray(ctx, result.toOpenArray(i * n, i * n + n - 1), clamp(g, 0'f32, 1'f32))
    inc i

proc findEdges*(src: Image; o: Rect; ctx: Ctx): seq[float32] =
  ## Find Edges: Sobel magnitude per colour channel, dark edges on white.
  let n = src.ch
  let cc = if ctx.alpha: n - 1 else: n
  result = src.crop(o)
  let w = o.width
  var i = 0
  while i * n + n <= result.len:
    let (x, y) = (o.x0 + i mod w, o.y0 + i div w)
    for c in 0 ..< cc:
      template g(dx, dy: int): float32 = src.get(x + dx, y + dy, c)
      let gx = (g(1, -1) + 2 * g(1, 0) + g(1, 1)) - (g(-1, -1) + 2 * g(-1, 0) + g(-1, 1))
      let gy = (g(-1, 1) + 2 * g(0, 1) + g(1, 1)) - (g(-1, -1) + 2 * g(0, -1) + g(1, -1))
      result[i * n + c] = clamp(1 - sqrt(gx * gx + gy * gy) / 4, 0'f32, 1'f32)
    inc i
