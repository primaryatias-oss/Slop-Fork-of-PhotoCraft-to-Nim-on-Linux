## Helpers shared by the stylize/artistic filters: native ↔ RGBA conversion in the filter's
## colour model, luma, premultiplied windows, box and gaussian blurs on interleaved buffers,
## jittered cell points and smoothstep.

import std/math
import ../geom, ../color, ../raster, ./params, ./noise

const MaxC* = 8

proc fmt*(ctx: Ctx): PixelFormat {.inline.} = pixelFormat(ctx.mode, stF32, ctx.alpha)
proc ncol*(ctx: Ctx; n: int): int {.inline.} = (if ctx.alpha: n - 1 else: n)
proc subtractive*(ctx: Ctx): bool {.inline.} = ctx.mode == cmCmyk
proc viaRgb*(ctx: Ctx): bool {.inline.} = ctx.mode == cmLab

proc xy*(o: Rect; i: int): (int, int) {.inline.} =
  let w = max(o.width, 1)
  (o.x0 + i mod w, o.y0 + i div w)

proc rgba*(ctx: Ctx; px: openArray[float32]): array[4, float32] {.inline.} = toRgba(fmt(ctx), px)

proc setRgba*(ctx: Ctx; px: var openArray[float32]; c: array[4, float32]) =
  var tmp: array[MaxC, float32]
  let n = fromRgbaInto(fmt(ctx), c, tmp)
  for i in 0 ..< min(n, px.len): px[i] = tmp[i]

proc native*(ctx: Ctx; c: array[4, float32]): array[MaxC, float32] =
  discard fromRgbaInto(fmt(ctx), c, result)

proc luma*(ctx: Ctx; px: openArray[float32]): float32 {.inline.} =
  case ctx.mode
  of cmGrayscale, cmBitmap, cmDuotone, cmLab: px[0]
  of cmRgb: 0.299'f32 * px[0] + 0.587'f32 * px[1] + 0.114'f32 * px[2]
  else:
    let c = rgba(ctx, px)
    0.299'f32 * c[0] + 0.587'f32 * c[1] + 0.114'f32 * c[2]

proc premulWindow*(src: Image; win: Rect; alpha: bool): seq[float32] =
  let n = src.ch
  result = newSeqOfCap[float32](max(win.width * win.height * n, 0))
  for y in win.y0 ..< win.y1:
    for x in win.x0 ..< win.x1:
      let a = (if alpha: src.get(x, y, n - 1) else: 1'f32)
      for c in 0 ..< n:
        let s = src.get(x, y, c)
        result.add (if alpha and c < n - 1: s * a else: s)

proc unpremulPx*(px: var openArray[float32]; alpha: bool) =
  if not alpha or px.len == 0: return
  let n = px.len
  let a = px[n - 1]
  for i in 0 ..< n - 1: px[i] = (if a > 1e-7: px[i] / a else: 0)

proc boxRowsN(buf: var openArray[float32]; w, n, r: int; scratch: var seq[float32]) =
  if r == 0 or w == 0 or n == 0: return
  let norm = 1.0 / float64(2 * r + 1)
  let rowLen = w * n
  var row0 = 0
  while row0 + rowLen <= buf.len:
    scratch.setLen(rowLen)
    for i in 0 ..< rowLen: scratch[i] = buf[row0 + i]
    template at(x, c: int): float64 = float64(scratch[clamp(x, 0, w - 1) * n + c])
    for c in 0 ..< n:
      var acc = 0.0
      for x in -r .. r: acc += at(x, c)
      for x in 0 ..< w:
        buf[row0 + x * n + c] = float32(acc * norm)
        acc += at(x + r + 1, c) - at(x - r, c)
    row0 += rowLen

proc boxColsN(buf: var openArray[float32]; w, h, n, r: int; scratch: var seq[float32]) =
  if r == 0 or h == 0 or n == 0: return
  let norm = 1.0 / float64(2 * r + 1)
  for x in 0 ..< w:
    scratch.setLen(h * n)
    for y in 0 ..< h:
      for c in 0 ..< n: scratch[y * n + c] = buf[(y * w + x) * n + c]
    template at(y, c: int): float64 = float64(scratch[clamp(y, 0, h - 1) * n + c])
    for c in 0 ..< n:
      var acc = 0.0
      for y in -r .. r: acc += at(y, c)
      for y in 0 ..< h:
        buf[(y * w + x) * n + c] = float32(acc * norm)
        acc += at(y + r + 1, c) - at(y - r, c)

proc boxBlurN*(buf: var openArray[float32]; w, h, n, r: int) =
  ## Box blur of radius `r` on an interleaved `w × h × n` buffer (edges clamped).
  var scratch: seq[float32]
  boxRowsN(buf, w, n, r, scratch)
  boxColsN(buf, w, h, n, r, scratch)

const MaxBoxRadius* = 1 shl 16
  ## Box radii are capped here: far beyond any real blur, it keeps absurd sigmas (only reachable
  ## through hand-written params) from looping for hours. Results are Rust's below it.

proc gaussBoxRadii*(sigma: float32): array[3, int] =
  ## Radii of three box blurs approximating a gaussian of `sigma` (Kovesi 2010), with Rust's
  ## i32 arithmetic (saturating casts, NaN → 0) so odd sigmas give the same radii.
  let n = 3'f32
  let wIdeal = sqrt(12 * sigma * sigma / n + 1)
  var wl = asI32(floor(float64(wIdeal)))
  if wl mod 2 == 0: dec wl
  wl = max(wl, 1)
  let wu = wl + 2
  let sq = cast[int32](uint32((wl * wl) and 0xFFFF_FFFF))
  let m = asUsize(float64(clamp(round((12 * sigma * sigma - n * float32(sq) - 4 * n * float32(wl) - 3 * n) /
                                      (-4 * float32(wl) - 4)), 0'f32, n)))
  for i in 0 .. 2: result[i] = min((if i < m: (wl - 1) div 2 else: (wu - 1) div 2), MaxBoxRadius)
  if result == [0, 0, 0]: result[0] = 1

proc gaussBlurN*(buf: var openArray[float32]; w, h, n: int; sigma: float32) =
  if sigma < 0.3: return
  var scratch: seq[float32]
  for r in gaussBoxRadii(sigma): boxRowsN(buf, w, n, r, scratch)
  for r in gaussBoxRadii(sigma): boxColsN(buf, w, h, n, r, scratch)

proc cellPoint*(cx, cy: int; cell: float32; seed: uint32; origin: (float32, float32)): (float32, float32) {.inline.} =
  (origin[0] + (float32(cx) + 0.1'f32 + 0.8'f32 * hash01(cx, cy, 11, seed)) * cell,
   origin[1] + (float32(cy) + 0.1'f32 + 0.8'f32 * hash01(cx, cy, 12, seed)) * cell)

proc smoothstep*(e0, e1, x: float32): float32 {.inline.} =
  if e1 <= e0: return (if x >= e1: 1 else: 0)
  let t = clamp((x - e0) / (e1 - e0), 0'f32, 1'f32)
  t * t * (3 - 2 * t)

proc addSamplePremul*(src: Image; x, y: float32; clip: Rect; alpha: bool; acc: var openArray[float32]) =
  ## Adds the bilinear (premultiplied) sample at `(x, y)`, clamped to `clip`, into `acc`.
  if x.classify in {fcNan, fcInf, fcNegInf} or y.classify in {fcNan, fcInf, fcNegInf} or clip.isEmpty: return
  let n = src.ch
  let fx = clamp(x - 0.5'f32, float32(clip.x0), float32(clip.x1 - 1))
  let fy = clamp(y - 0.5'f32, float32(clip.y0), float32(clip.y1 - 1))
  let (x0, y0) = (int(floor(fx)), int(floor(fy)))
  let (x1, y1) = (min(x0 + 1, clip.x1 - 1), min(y0 + 1, clip.y1 - 1))
  let (ax, ay) = (fx - float32(x0), fy - float32(y0))
  let w = src.rect.width
  template idx(xx, yy: int): int = ((yy - src.rect.y0) * w + (xx - src.rect.x0)) * n
  for (i, k) in [(idx(x0, y0), (1 - ax) * (1 - ay)), (idx(x1, y0), ax * (1 - ay)), (idx(x0, y1), (1 - ax) * ay), (idx(x1, y1), ax * ay)]:
    if k <= 0: continue
    if i < 0 or i + n > src.data.len: continue
    let a = (if alpha: src.data[i + n - 1] * k else: k)
    if alpha:
      for c in 0 ..< n - 1: acc[c] += src.data[i + c] * a
      acc[n - 1] += a
    else:
      for c in 0 ..< n: acc[c] += src.data[i + c] * a
