## Distortions (inverse mapping + bilinear resampling): Twirl, Pinch, Spherize, Wave, Ripple,
## Polar Coordinates, Displace, Shear, ZigZag (port of `distort.rs` and `distort2.rs`). All are
## centred on the reference bounds.

import std/[math, algorithm]
import ../geom
import params, image, util, noise, other

const
  Pi32 = float32(PI)
  Tau32 = float32(TAU)

template remap*(src: Image; o: Rect; ctx: Ctx; edge: Edge; f: untyped): seq[float32] =
  ## Samples `src` at `f` of every output pixel centre (`px`, `py` injected).
  block:
    let n = src.ch
    var res = newSeqOfCap[float32](o.width * o.height * n)
    var tmp = newSeq[float32](n)
    for yy in o.y0 ..< o.y1:
      for xx in o.x0 ..< o.x1:
        let px {.inject.} = float32(xx) + 0.5'f32
        let py {.inject.} = float32(yy) + 0.5'f32
        let (sx, sy) = f
        src.sample(sx, sy, edge, ctx.bounds, ctx.alpha, tmp)
        res.add tmp
    res

proc centre*(b: Rect): (float32, float32, float32) =
  let (w, h) = (float32(b.width), float32(b.height))
  (float32(b.x0) + w / 2, float32(b.y0) + h / 2, min(w, h) / 2)

proc twirl*(src: Image; o: Rect; ctx: Ctx; angle: float32): seq[float32] =
  let (cx, cy, rr) = centre(ctx.bounds)
  let a = degToRad(angle)
  proc f(x, y: float32): (float32, float32) =
    let (dx, dy) = (x - cx, y - cy)
    let r = sqrt(dx * dx + dy * dy)
    if r >= rr or rr <= 0: return (x, y)
    let t = a * (1 - r / rr)
    let (s, c) = (sin(t), cos(t))
    (cx + dx * c - dy * s, cy + dx * s + dy * c)
  remap(src, o, ctx, edRepeat, f(px, py))

proc pinch*(src: Image; o: Rect; ctx: Ctx; amount: float32): seq[float32] =
  let (cx, cy, rr) = centre(ctx.bounds)
  let a = clamp(amount / 100, -1'f32, 1'f32)
  proc f(x, y: float32): (float32, float32) =
    let (dx, dy) = (x - cx, y - cy)
    let r = sqrt(dx * dx + dy * dy)
    if r >= rr or r <= 0: return (x, y)
    let rs = rr * pow(r / rr, 1 - a * 0.5'f32)
    (cx + dx * rs / r, cy + dy * rs / r)
  remap(src, o, ctx, edRepeat, f(px, py))

proc sphere1d(rn, a: float32): float32 =
  ## rn in [0, 1): a positive amount magnifies the centre (samples closer in).
  let target = if a >= 0: arcsin(rn) * 2 / Pi32 else: sin(rn * float32(PI / 2))
  rn + abs(a) * (target - rn)

proc spherize*(src: Image; o: Rect; ctx: Ctx; amount: float32; mode: SpherizeMode): seq[float32] =
  let b = ctx.bounds
  let (cx, cy, rr) = centre(b)
  let (hx, hy) = (float32(b.width) / 2, float32(b.height) / 2)
  let a = clamp(amount / 100, -1'f32, 1'f32)
  proc f(x, y: float32): (float32, float32) =
    let (dx, dy) = (x - cx, y - cy)
    case mode
    of smNormal:
      let r = sqrt(dx * dx + dy * dy)
      if r >= rr or r <= 0: return (x, y)
      let rs = rr * sphere1d(r / rr, a)
      (cx + dx * rs / r, cy + dy * rs / r)
    of smHorizontalOnly:
      if abs(dx) >= hx: return (x, y)
      (cx + signum(dx) * hx * sphere1d(abs(dx) / hx, a), y)
    of smVerticalOnly:
      if abs(dy) >= hy: return (x, y)
      (x, cy + signum(dy) * hy * sphere1d(abs(dy) / hy, a))
  remap(src, o, ctx, edRepeat, f(px, py))

type WaveSpec* = object
  ## Wave generator settings.
  generators*: uint32
  wavelength*, amplitude*: (float32, float32)
  waveType*: WaveType
  undefined*: UndefinedAreas
  seed*: uint32

proc waveShape(t: WaveType; phase: float32): float32 =
  let p = remEuclidF(phase, Tau32) / Tau32
  case t
  of wtSine: sin(phase)
  of wtTriangle: 1 - 4 * abs(p - 0.5'f32)
  of wtSquare: (if p < 0.5: 1 else: -1)

proc wave*(src: Image; o: Rect; ctx: Ctx; w: WaveSpec): seq[float32] =
  let n = clamp(w.generators, 1'u32, 64'u32)
  proc lerp(ab: (float32, float32); t: float32): float32 = ab[0] + rmax(ab[1] - ab[0], 0) * t
  var gens: seq[(float32, float32, float32, float32)]
  for i in 0 ..< int(n):
    template r(k: int): float32 = hash01(i, k, 7, w.seed)
    gens.add (rmax(lerp(w.wavelength, r(1)), 1), lerp(w.amplitude, r(2)), r(3) * Tau32, r(4) * Tau32)
  let b = ctx.bounds
  proc f(x, y: float32): (float32, float32) =
    let (lx, ly) = (x - float32(b.x0), y - float32(b.y0))
    var (dx, dy) = (0'f32, 0'f32)
    for (len, amp, p1, p2) in gens:
      dx += amp * waveShape(w.waveType, Tau32 * ly / len + p1)
      dy += amp * waveShape(w.waveType, Tau32 * lx / len + p2)
    (x + dx / float32(n), y + dy / float32(n))
  remap(src, o, ctx, edgeOf(w.undefined), f(px, py))

proc ripple*(src: Image; o: Rect; ctx: Ctx; amount: float32; size: RippleSize): seq[float32] =
  let len = case size
    of rsSmall: 8'f32
    of rsMedium: 16'f32
    of rsLarge: 32'f32
  let amp = amount / 100 * len / 8
  let b = ctx.bounds
  proc f(x, y: float32): (float32, float32) =
    let (lx, ly) = (x - float32(b.x0), y - float32(b.y0))
    (x + amp * sin(Tau32 * ly / len), y + amp * sin(Tau32 * lx / len))
  remap(src, o, ctx, edRepeat, f(px, py))

proc polar*(src: Image; o: Rect; ctx: Ctx; mode: PolarMode): seq[float32] =
  let b = ctx.bounds
  let (w, h) = (float32(max(b.width, 1)), float32(max(b.height, 1)))
  let (cx, cy) = (float32(b.x0) + w / 2, float32(b.y0) + h / 2)
  let rmaxv = min(w, h) / 2
  proc f(x, y: float32): (float32, float32) =
    case mode
    of pmRectangularToPolar:
      # Output is polar: angle from 12 o'clock clockwise → source x, radius → source y.
      let (dx, dy) = (x - cx, y - cy)
      let theta = remEuclidF(arctan2(dx, -dy), Tau32)
      let r = sqrt(dx * dx + dy * dy)
      (float32(b.x0) + theta / Tau32 * w, float32(b.y0) + r / rmaxv * h)
    of pmPolarToRectangular:
      let theta = (x - float32(b.x0)) / w * Tau32
      let r = (y - float32(b.y0)) / h * rmaxv
      (cx + r * sin(theta), cy - r * cos(theta))
  remap(src, o, ctx, edTransparent, f(px, py))

# ---------------------------------------------------------------- Displace, Shear, ZigZag

const DisplacePx = 128'f32  ## max displacement at 100 % scale and full map contrast, px

proc mapSample(map: Image; u, v: float32; c: int): float32 =
  ## Bilinear read of channel `c` of a map at map-local coordinates (clamped).
  let r = map.rect
  let (w, h) = (r.width, r.height)
  let (fx, fy) = (u - 0.5'f32, v - 0.5'f32)
  let (x0f, y0f) = (floor(fx), floor(fy))
  let (ax, ay) = (fx - x0f, fy - y0f)
  template g(x, y: int): float32 = map.get(r.x0 + clamp(x, 0, w - 1), r.y0 + clamp(y, 0, h - 1), c)
  let (x0, y0) = (asI32(float64(x0f)), asI32(float64(y0f)))
  let top = g(x0, y0) + (g(x0 + 1, y0) - g(x0, y0)) * ax
  let bot = g(x0, y0 + 1) + (g(x0 + 1, y0 + 1) - g(x0, y0 + 1)) * ax
  top + (bot - top) * ay

proc displace*(src: Image; o: Rect; ctx: Ctx; scale: (float32, float32); stretch: bool; undefined: UndefinedAreas;
               map: ref Image): seq[float32] =
  ## Displace: the map's first channel shifts pixels horizontally and its second (or first, for
  ## one-channel maps) vertically; mid-gray = no shift. The map is stretched over the bounds or
  ## tiled from their top-left.
  if map.isNil or map[].rect.isEmpty: return src.crop(o)
  let b = ctx.bounds
  let (mw, mh) = (float32(map[].rect.width), float32(map[].rect.height))
  let (bw, bh) = (float32(max(b.width, 1)), float32(max(b.height, 1)))
  let vc = if map[].ch >= 3: 1 else: 0
  let (sx, sy) = (scale[0] / 100 * DisplacePx, scale[1] / 100 * DisplacePx)
  proc f(x, y: float32): (float32, float32) =
    let (lx, ly) = (x - float32(b.x0), y - float32(b.y0))
    let (u, v) = if stretch: (lx / bw * mw, ly / bh * mh) else: (remEuclidF(lx, mw), remEuclidF(ly, mh))
    let dx = (mapSample(map[], u, v, 0) - 0.5'f32) * 2 * sx
    let dy = (mapSample(map[], u, v, vc) - 0.5'f32) * 2 * sy
    (x + dx, y + dy)
  remap(src, o, ctx, edgeOf(undefined), f(px, py))

proc curveAt*(pts: openArray[array[2, float32]]; t: float32): float32 =
  ## Catmull–Rom interpolation through sorted `(t, value)` points (clamped ends).
  case pts.len
  of 0: 0
  of 1: pts[0][1]
  else:
    if t <= pts[0][0]: return pts[0][1]
    let last = pts.len - 1
    if t >= pts[last][0]: return pts[last][1]
    var i = 0
    for k in 0 ..< last:
      if t >= pts[k][0] and t <= pts[k + 1][0]:
        i = k
        break
    let p1 = pts[i]
    let p2 = pts[i + 1]
    let p0 = if i > 0: pts[i - 1] else: p1
    let p3 = if i + 2 <= last: pts[i + 2] else: p2
    let span = rmax(p2[0] - p1[0], 1e-6'f32)
    let s = (t - p1[0]) / span
    # Tangents scaled to the span (non-uniform Catmull–Rom).
    let m1 = if i > 0: (p2[1] - p0[1]) / rmax(p2[0] - p0[0], 1e-6'f32) * span else: p2[1] - p1[1]
    let m2 = if i + 2 <= last: (p3[1] - p1[1]) / rmax(p3[0] - p1[0], 1e-6'f32) * span else: p2[1] - p1[1]
    let (s2, s3) = (s * s, s * s * s)
    (2 * s3 - 3 * s2 + 1) * p1[1] + (s3 - 2 * s2 + s) * m1 + (-2 * s3 + 3 * s2) * p2[1] + (s3 - s2) * m2

proc shear*(src: Image; o: Rect; ctx: Ctx; points: openArray[array[2, float32]]; undefined: UndefinedAreas): seq[float32] =
  ## Shear: each row shifts horizontally by the curve through `points` (`[t, offset]`, t
  ## top→bottom, offset in half-widths).
  let b = ctx.bounds
  var sorted: seq[array[2, float32]]
  for p in points: sorted.add [clamp(p[0], 0'f32, 1'f32), clamp(p[1], -1'f32, 1'f32)]
  sorted.sort(proc(a, b: array[2, float32]): int = totalCmp(a[0], b[0]))
  var pts: seq[array[2, float32]]
  for p in sorted:
    if pts.len > 0 and abs(p[0] - pts[^1][0]) < 1e-6'f32: continue
    pts.add p
  let (bw, bh) = (float32(max(b.width, 1)), float32(max(b.height, 1)))
  var rows: seq[float32]
  for y in o.y0 ..< o.y1: rows.add curveAt(pts, (float32(y) + 0.5'f32 - float32(b.y0)) / bh) * bw / 2
  proc f(x, y: float32): (float32, float32) =
    let i = clamp(asI32(float64(y - 0.5'f32)) - o.y0, 0, rows.len - 1)
    (x - rows[i], y)
  remap(src, o, ctx, edgeOf(undefined), f(px, py))

proc zigzag*(src: Image; o: Rect; ctx: Ctx; amount, ridges: float32; style: ZigZagStyle): seq[float32] =
  ## ZigZag: radial ripples inside the bounds' inscribed circle — angular (around centre),
  ## radial (out from centre) or both (pond ripples).
  let (cx, cy, rr) = centre(ctx.bounds)
  let a = clamp(amount / 100, -1'f32, 1'f32)
  let k = rmax(clamp(ridges, 0'f32, 20'f32), 0.5'f32)
  proc f(x, y: float32): (float32, float32) =
    let (dx, dy) = (x - cx, y - cy)
    let r = sqrt(dx * dx + dy * dy)
    if r >= rr or r <= 0 or a == 0: return (x, y)
    let rn = r / rr
    let wv = sin(Tau32 * k * rn) * (1 - rn)
    let (radial, angular) = case style
      of zzOutFromCenter: (a * wv * rr / (4 * k), 0'f32)
      of zzAroundCenter: (0'f32, a * wv * Pi32 / (2 * k))
      of zzPondRipples: (a * wv * rr / (8 * k), a * wv * Pi32 / (4 * k))
    let th = arctan2(dy, dx) + angular
    let rs = rmax(r + radial, 0)
    (cx + rs * cos(th), cy + rs * sin(th))
  remap(src, o, ctx, edRepeat, f(px, py))
