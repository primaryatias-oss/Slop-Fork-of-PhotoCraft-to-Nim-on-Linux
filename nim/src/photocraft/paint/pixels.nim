## Native-channel pixel helpers shared by the stroke renderer and the retouch tools (the data
## types and per-pixel math of `paint/src/retouch.rs`).
##
## All pixel data here is the surface's *native* channels as normalised `float32` (any depth, any
## colour model); colour-space-specific work happens in the callers.

import ../geom, ../color, ../raster
import ./brush

type
  PxRegion* = object
    ## A rectangle of native-channel pixels (`rect.width × rect.height × ch` floats, row-major).
    ## (`retouch::Region` in the Rust crate.)
    rect*: Rect
    ch*: int
    data*: seq[float32]

  Footprint* = object
    ## One dab's footprint: coverage (hardness falloff × dab alpha) over its bounding rectangle.
    dab*: Dab
    index*: int          ## position of the dab in the stroke (0 = first)
    rect*: Rect
    cov*: seq[float32]   ## `rect.width × rect.height` coverage values in 0..1

proc newPxRegion*(rect: Rect; ch: int): PxRegion =
  ## A zeroed region.
  PxRegion(rect: rect, ch: ch, data: newSeq[float32](rect.width * rect.height * ch))

proc readPxRegion*(s: Surface; rect: Rect): PxRegion =
  ## Read `rect` from a surface.
  PxRegion(rect: rect, ch: s.channels, data: s.readRegion(rect))

proc width*(r: PxRegion): int {.inline.} = r.rect.width
proc height*(r: PxRegion): int {.inline.} = r.rect.height

proc index*(r: PxRegion; x, y: int): int {.inline.} =
  ## Pixel index of document coordinate `(x, y)` (must lie inside `rect`).
  (y - r.rect.y0) * r.width + (x - r.rect.x0)

proc offset*(r: PxRegion; x, y: int): int {.inline.} = r.index(x, y) * r.ch

template px*(r: PxRegion; x, y: int): untyped =
  ## The native channels of pixel `(x, y)` (an openArray view).
  (let o = r.offset(x, y); r.data.toOpenArray(o, o + r.ch - 1))

template pxMut*(r: var PxRegion; x, y: int): untyped =
  (let o = r.offset(x, y); r.data.toOpenArray(o, o + r.ch - 1))

proc pxSeq*(r: PxRegion; x, y: int): seq[float32] =
  let o = r.offset(x, y)
  r.data[o ..< o + r.ch]

proc crop*(r: PxRegion; c: Rect): PxRegion =
  ## Copy of the sub-rectangle `c` (clipped to this region).
  let cr = c.intersect(r.rect)
  result = newPxRegion(cr, r.ch)
  let (w, n) = (cr.width, r.ch)
  for y in cr.y0 ..< cr.y1:
    let s = r.index(cr.x0, y) * n
    let d = (y - cr.y0) * w * n
    for k in 0 ..< w * n: result.data[d + k] = r.data[s + k]

proc write*(r: PxRegion; s: var Surface) =
  ## Write this region into a surface.
  s.writeRegion(r.rect, r.data)

proc at*(f: Footprint; x, y: int): float32 {.inline.} =
  f.cov[(y - f.rect.y0) * f.rect.width + (x - f.rect.x0)]

proc alphaIndex*(fmt: PixelFormat): int {.inline.} =
  ## Index of the alpha channel, or -1 when the format has none.
  if fmt.alpha: fmt.mode.colorChannels else: -1

proc overNative*(fmt: PixelFormat; dst: var openArray[float32]; src: openArray[float32]; k: float32;
                 lockTransparency: bool) {.inline.} =
  ## Normal-mode "source over destination" at coverage `k`, in native channels (colour channels
  ## are channel-agnostic, so this works for RGB, Gray, CMYK, Lab… at any depth).
  let a = alphaIndex(fmt)
  if a >= 0:
    let (da, sa) = (dst[a], src[a] * k)
    let oa = sa + da * (1'f32 - sa)
    if oa > 0:
      for c in 0 ..< a: dst[c] = (src[c] * sa + dst[c] * da * (1'f32 - sa)) / oa
    if not lockTransparency: dst[a] = oa
  else:
    for c in 0 ..< dst.len: dst[c] += (src[c] - dst[c]) * k

proc mixPremultiplied*(dst: var openArray[float32]; src: openArray[float32]; a: int; k: float32) =
  ## `dst ← lerp(dst, src, k)` in premultiplied space (straight in, straight out; `a` = alpha
  ## index). Where the result is fully transparent the colour is mixed straight, so it stays
  ## defined. An out-of-range alpha index leaves `dst` untouched.
  if a < 0 or a >= dst.len or a >= src.len: return
  let (da, sa) = (dst[a], src[a])
  let oa = da + (sa - da) * k
  for c in 0 ..< min(dst.len, src.len):
    if c == a: continue
    dst[c] = if oa > 1e-6'f32: (dst[c] * da + (src[c] * sa - dst[c] * da) * k) / oa else: dst[c] + (src[c] - dst[c]) * k
  dst[a] = oa

proc premultiply*(px: var openArray[float32]; alpha: int) =
  ## Premultiply a straight pixel in place (no-op without alpha).
  if alpha < 0 or alpha >= px.len: return
  let pa = px[alpha]
  for c in 0 ..< px.len:
    if c != alpha: px[c] *= pa
