## Procedural renderers of Filter › Render: Flame, Picture Frame and Tree (port of `render2.rs`).
##
## Each renderer turns its options into a list of anti-aliased vector primitives (`Prim`: tapered
## capsules and shaded ellipses, blended normally or additively). `composite` rasterizes the list
## over a surface tile by tile, in straight sRGB converted to and from the surface's own colour
## model and depth, mixed by the selection.
##
## The shapes are our own designs (L-system-like recursive branching for trees, turbulent flame
## strands, ornaments walked along the frame edge), built from observation of what the options
## do, not from any reference implementation.

import std/[math, options, algorithm, json, strutils]
import ../[geom, raster, color]
import ./util

# float32 constants that take part in arithmetic are `let`s so Nim does not fold them in float64.
let
  Pi32 = float32(PI)
  Tau32 = float32(TAU)
  FracPi2 = float32(PI / 2)
  FracPi3 = float32(PI / 3)
  FracPi4 = float32(PI / 4)

# ---------------------------------------------------------------- primitives and compositing

type
  ShapeKind* = enum skCapsule, skEllipse
  Shape* = object
    ## Geometry of a primitive (document pixel coordinates).
    case kind*: ShapeKind
    of skCapsule:
      a*, b*: (float32, float32) ## a segment from `a` to `b`
      ra*, rb*: float32          ## whose radius goes from `ra` to `rb`; colour `c0` → `c1` along it
    of skEllipse:
      c*: (float32, float32)     ## an ellipse rotated by `angle` radians
      rx*, ry*, angle*: float32  ## colour goes `c0` (centre) → `c1` (rim)

  Blend* = enum
    ## How a primitive combines with what is under it.
    bNormal  ## source over
    bAdd     ## additive light (glows)
    bLighten ## per-channel maximum of the premultiplied values

  Prim* = object
    ## One drawable primitive. Colours are straight sRGB RGBA (0–1).
    shape*: Shape
    c0*, c1*: array[4, float32]
    soft*: float32 ## edge softness in pixels (≥ 1 for anti-aliasing; larger for glows)
    blend*: Blend

proc `==`*(x, y: Shape): bool =
  if x.kind != y.kind: return false
  case x.kind
  of skCapsule: x.a == y.a and x.b == y.b and x.ra == y.ra and x.rb == y.rb
  of skEllipse: x.c == y.c and x.rx == y.rx and x.ry == y.ry and x.angle == y.angle

proc capsule*(a, b: (float32, float32); ra, rb: float32; c0, c1: array[4, float32]): Prim =
  Prim(shape: Shape(kind: skCapsule, a: a, b: b, ra: ra, rb: rb), c0: c0, c1: c1, soft: 1, blend: bNormal)

proc ellipse*(c: (float32, float32); rx, ry, angle: float32; c0, c1: array[4, float32]): Prim =
  Prim(shape: Shape(kind: skEllipse, c: c, rx: rx, ry: ry, angle: angle), c0: c0, c1: c1, soft: 1, blend: bNormal)

proc disc*(c: (float32, float32); r: float32; col: array[4, float32]): Prim = ellipse(c, r, r, 0, col, col)

proc withSoft(p: Prim; s: float32): Prim =
  result = p
  result.soft = s
proc additive(p: Prim): Prim =
  result = p
  result.blend = bAdd
proc lighten(p: Prim): Prim =
  result = p
  result.blend = bLighten

proc isFinite(v: float32): bool {.inline.} = v == v and v - v == 0'f32

proc bounds*(p: Prim): Rect =
  ## Pixel bounds this primitive can touch.
  let s = rmax(p.soft, 1'f32) + 1'f32
  var x0, y0, x1, y1: float32
  case p.shape.kind
  of skCapsule:
    let (a, b) = (p.shape.a, p.shape.b)
    let r = rmax(p.shape.ra, p.shape.rb) + s
    (x0, y0, x1, y1) = (rmin(a[0], b[0]) - r, rmin(a[1], b[1]) - r, rmax(a[0], b[0]) + r, rmax(a[1], b[1]) + r)
  of skEllipse:
    let (sn, cs) = (sin(p.shape.angle), cos(p.shape.angle))
    let (rx, ry, c) = (p.shape.rx, p.shape.ry, p.shape.c)
    let ex = sqrt((rx * cs) * (rx * cs) + (ry * sn) * (ry * sn)) + s
    let ey = sqrt((rx * sn) * (rx * sn) + (ry * cs) * (ry * cs)) + s
    (x0, y0, x1, y1) = (c[0] - ex, c[1] - ey, c[0] + ex, c[1] + ey)
  if not (x0.isFinite and y0.isFinite and x1.isFinite and y1.isFinite): return EmptyRect
  proc cl(v: float32): int = int(clamp(v, -1.0e8'f32, 1.0e8'f32))
  rect(cl(floor(x0)), cl(floor(y0)), cl(ceil(x1)), cl(ceil(y1)))

proc eval(p: Prim; x, y: float32): (float32, float32) {.inline.} =
  ## Coverage (0–1) and colour position (0–1) at a pixel centre.
  let soft = rmax(p.soft, 1'f32)
  case p.shape.kind
  of skCapsule:
    let (a, b, ra, rb) = (p.shape.a, p.shape.b, p.shape.ra, p.shape.rb)
    let (dx, dy) = (b[0] - a[0], b[1] - a[1])
    let l2 = dx * dx + dy * dy
    let t = (if l2 > 1e-9'f32: clamp(((x - a[0]) * dx + (y - a[1]) * dy) / l2, 0'f32, 1'f32) else: 0'f32)
    let (px, py) = (a[0] + dx * t - x, a[1] + dy * t - y)
    let d = sqrt(px * px + py * py)
    let r = ra + (rb - ra) * t
    # Hairlines thinner than a pixel fade instead of vanishing.
    let thin = clamp(2'f32 * r, 0'f32, 1'f32)
    (clamp((rmax(r, 0.5'f32) - d) / soft + 0.5'f32, 0'f32, 1'f32) * thin, t)
  of skEllipse:
    let (s, co) = (sin(p.shape.angle), cos(p.shape.angle))
    let (dx, dy) = (x - p.shape.c[0], y - p.shape.c[1])
    let (u, v) = (dx * co + dy * s, -dx * s + dy * co)
    let (rx, ry) = (rmax(p.shape.rx, 0.05'f32), rmax(p.shape.ry, 0.05'f32))
    let e = sqrt((u / rx) * (u / rx) + (v / ry) * (v / ry))
    let edge = (1'f32 - e) * rmin(rx, ry)
    let thin = clamp(2'f32 * rmin(rx, ry), 0'f32, 1'f32)
    (clamp(edge / soft + 0.5'f32, 0'f32, 1'f32) * thin, rmin(e, 1'f32))

proc lerp4(a, b: array[4, float32]; t: float32): array[4, float32] {.inline.} =
  [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t, a[2] + (b[2] - a[2]) * t, a[3] + (b[3] - a[3]) * t]

proc rasterTile(prims: openArray[Prim]; idx: openArray[uint32]; t: Rect): seq[array[4, float32]] =
  ## Rasterizes `prims` (in order) into a premultiplied RGBA buffer covering `t`.
  let w = t.width
  result = newSeq[array[4, float32]](w * t.height)
  for k in idx:
    let p = prims[k]
    let r = p.bounds.intersect(t)
    if r.isEmpty: continue
    for y in r.y0 ..< r.y1:
      let row = (y - t.y0) * w
      for x in r.x0 ..< r.x1:
        let (cov, ct) = p.eval(float32(x) + 0.5'f32, float32(y) + 0.5'f32)
        if cov <= 0: continue
        let c = (if p.c0 == p.c1: p.c0 else: lerp4(p.c0, p.c1, ct))
        let a = clamp(c[3] * cov, 0'f32, 1'f32)
        if a <= 0: continue
        let i = row + (x - t.x0)
        case p.blend
        of bAdd:
          result[i][0] += c[0] * a
          result[i][1] += c[1] * a
          result[i][2] += c[2] * a
          result[i][3] += a * (1'f32 - result[i][3])
        of bLighten:
          result[i][0] = rmax(result[i][0], c[0] * a)
          result[i][1] = rmax(result[i][1], c[1] * a)
          result[i][2] = rmax(result[i][2], c[2] * a)
          result[i][3] = rmax(result[i][3], a)
        of bNormal:
          let kk = 1'f32 - a
          result[i][0] = c[0] * a + result[i][0] * kk
          result[i][1] = c[1] * a + result[i][1] * kk
          result[i][2] = c[2] * a + result[i][2] * kk
          result[i][3] = a + result[i][3] * kk

const CTile = 128 ## size of the tiles `composite` works in

proc composite*(surface: var Surface; prims: openArray[Prim]; clip: Rect; selection: Option[Surface]): Rect =
  ## Draws `prims` over `surface` inside `clip`, mixed by the selection coverage (channel 0 of
  ## `selection`). Returns the bounds of what was drawn.
  result = EmptyRect
  var bnds = newSeq[Rect](prims.len)
  for k, p in prims: bnds[k] = p.bounds.intersect(clip)
  for b in bnds:
    if not b.isEmpty: result = (if result.isEmpty: b else: result.union(b))
  if result.isEmpty: return
  # Bin primitives into tiles (order preserved within a tile).
  let (tx0, ty0) = (divEuclid(result.x0, CTile), divEuclid(result.y0, CTile))
  let (tx1, ty1) = (divEuclid(result.x1 - 1, CTile), divEuclid(result.y1 - 1, CTile))
  let (nx, ny) = (tx1 - tx0 + 1, ty1 - ty0 + 1)
  var bins = newSeq[seq[uint32]](nx * ny)
  for k, b in bnds:
    if b.isEmpty: continue
    for ty in divEuclid(b.y0, CTile) .. divEuclid(b.y1 - 1, CTile):
      for tx in divEuclid(b.x0, CTile) .. divEuclid(b.x1 - 1, CTile):
        bins[(ty - ty0) * nx + (tx - tx0)].add uint32(k)
  let fmt = surface.format
  let n = fmt.channels
  var tmp: array[8, float32]
  for i, idx in bins:
    if idx.len == 0: continue
    let (tx, ty) = (tx0 + i mod nx, ty0 + i div nx)
    let t = rect(tx * CTile, ty * CTile, (tx + 1) * CTile, (ty + 1) * CTile).intersect(clip)
    if t.isEmpty: continue
    let paint = rasterTile(prims, idx, t)
    var data = surface.readRegion(t)
    var sel: seq[float32]
    var sn = 1
    if selection.isSome:
      sel = selection.get.readRegion(t)
      sn = selection.get.channels
    for q in 0 ..< data.len div n:
      let s = paint[q]
      let k = (if selection.isSome: clamp(sel[q * sn], 0'f32, 1'f32) else: 1'f32)
      let sa = clamp(s[3], 0'f32, 1'f32) * k
      if sa <= 0: continue
      let inv = 1'f32 / rmax(s[3], 1e-6'f32)
      let sc = [clamp(s[0] * inv, 0'f32, 1'f32), clamp(s[1] * inv, 0'f32, 1'f32), clamp(s[2] * inv, 0'f32, 1'f32)]
      let d = toRgba(fmt, data.toOpenArray(q * n, q * n + n - 1))
      let da = (if fmt.alpha: d[3] else: 1'f32)
      let oa = sa + da * (1'f32 - sa)
      template mix(sv, dv: float32): float32 = (if oa > 1e-7'f32: (sv * sa + dv * da * (1'f32 - sa)) / oa else: 0'f32)
      let o = [mix(sc[0], d[0]), mix(sc[1], d[1]), mix(sc[2], d[2]), oa]
      let m = fromRgbaInto(fmt, o, tmp)
      for c in 0 ..< min(m, n): data[q * n + c] = tmp[c]
    surface.writeRegion(t, data)

# ---------------------------------------------------------------- randomness

type RenderRng* = object
  ## Small deterministic PRNG (SplitMix64).
  state: uint64

proc newRenderRng*(seed: uint32; salt: uint64): RenderRng =
  RenderRng(state: (uint64(seed) shl 32) xor (salt * 0x9E37_79B9_7F4A_7C15'u64) xor 0x2545_F491_4F6C_DD1D'u64)

proc nextU64*(r: var RenderRng): uint64 =
  r.state += 0x9E37_79B9_7F4A_7C15'u64
  var z = r.state
  z = (z xor (z shr 30)) * 0xBF58_476D_1CE4_E5B9'u64
  z = (z xor (z shr 27)) * 0x94D0_49BB_1331_11EB'u64
  z xor (z shr 31)

proc f*(r: var RenderRng): float32 =
  ## Uniform in [0, 1).
  float32(r.nextU64() shr 40) / float32(1'u64 shl 24)

proc range*(r: var RenderRng; a, b: float32): float32 =
  ## Uniform in [a, b).
  a + (b - a) * r.f()

proc sym*(r: var RenderRng): float32 =
  ## Uniform in [-1, 1).
  r.f() * 2'f32 - 1'f32

proc asI64f(v: float32): int64 {.inline.} =
  if v != v: 0'i64
  elif v >= 9.2e18'f32: high(int64)
  elif v <= -9.2e18'f32: low(int64)
  else: int64(v)

proc latticeSym(i: int64; seed: uint64): float32 {.inline.} =
  var r = RenderRng(state: (cast[uint64](i) * 0xD6E8_FEB8_6659_FD93'u64) xor seed)
  r.sym()

proc noise1(x: float32; seed: uint64): float32 =
  ## Smooth 1-D value noise in [-1, 1].
  let fi = floor(x)
  let f = x - fi
  let i = asI64f(fi)
  let t = f * f * (3'f32 - 2'f32 * f)
  latticeSym(i, seed) + (latticeSym(i +% 1, seed) - latticeSym(i, seed)) * t

proc fbm1(x: float32; seed: uint64): float32 =
  ## Two octaves of `noise1`.
  (noise1(x, seed) + 0.5'f32 * noise1(x * 2.13'f32 + 7.1'f32, seed xor 0xA5A5'u64)) / 1.5'f32

# ---------------------------------------------------------------- polylines

type Spine = object
  ## A polyline parameterized by arc length.
  pts: seq[(float32, float32)]
  cum: seq[float32]

proc newSpine(pts: seq[(float32, float32)]): Spine =
  var cum = @[0'f32]
  for k in 1 ..< pts.len:
    let (dx, dy) = (pts[k][0] - pts[k - 1][0], pts[k][1] - pts[k - 1][1])
    cum.add cum[^1] + sqrt(dx * dx + dy * dy)
  Spine(pts: pts, cum: cum)

proc len(s: Spine): float32 = (if s.cum.len > 0: s.cum[^1] else: 0'f32)

proc at(sp: Spine; s0: float32): ((float32, float32), (float32, float32)) =
  ## Point and unit tangent at arc length `s`.
  let n = sp.pts.len
  if n == 0: return ((0'f32, 0'f32), (1'f32, 0'f32))
  if n == 1: return (sp.pts[0], (0'f32, -1'f32))
  let s = clamp(s0, 0'f32, sp.len)
  let (found, i) = binarySearchBy(sp.cum, proc(v: float32): int = (if v < s: -1 elif v > s: 1 elif v == s: 0 else: -1))
  let k = (if found: min(i, n - 2) else: min(max(i - 1, 0), n - 2))
  let (a, b) = (sp.pts[k], sp.pts[k + 1])
  let seg = rmax(sp.cum[k + 1] - sp.cum[k], 1e-6'f32)
  let t = clamp((s - sp.cum[k]) / seg, 0'f32, 1'f32)
  let (dx, dy) = (b[0] - a[0], b[1] - a[1])
  let l = rmax(sqrt(dx * dx + dy * dy), 1e-6'f32)
  ((a[0] + dx * t, a[1] + dy * t), (dx / l, dy / l))

# ---------------------------------------------------------------- flame

type
  FlameType* = enum
    ## Flame placement (Photoshop's six flame types).
    ftOneFlameAlongPath = "oneFlameAlongPath"                     ## one flame whose spine is the path
    ftMultipleFlamesAlongPath = "multipleFlamesAlongPath"         ## flames at `interval`, rising from the path's left
    ftMultipleFlamesPathDirections = "multipleFlamesPathDirections" ## pointing along the path's direction
    ftMultipleFlamesVariousLength = "multipleFlamesVariousLength" ## strongly varying lengths
    ftCandleLight = "candleLight"                                 ## a candle flame at the start of each subpath
    ftMultipleFlamesOneDirection = "multipleFlamesOneDirection"   ## all pointing at `angle`

  FlameStyle* = enum
    fsNormal = "normal", fsViolent = "violent", fsFlat = "flat"

  FlameShape* = enum
    fhParallel = "parallel", fhToCenter = "toCenter", fhSpread = "spread", fhOval = "oval", fhPointed = "pointed"

  FlameSpec* = object
    ## Flame options (Photoshop's Flame dialog, Basic + Advanced).
    flameType*: FlameType
    length*: float32               ## flame length in px (1–1000)
    randomizeLength*: bool
    width*: float32                ## flame width in px (1–1000)
    angle*: float32                ## direction for `multipleFlamesOneDirection` (degrees, 0 = up, clockwise)
    interval*: float32             ## spacing between flames along the path in px (1–1000)
    adjustIntervalForLoops*: bool  ## spread the flames evenly so a closed path loops seamlessly
    color*: Option[array[4, float32]] ## custom flame colour; none = natural flame colours
    turbulent*, jag*: float32      ## 0–100
    opacity*: float32              ## 0–100 %
    flameLines*: float32           ## flame lines (complexity) 1–100
    flameBottomAlignment*: float32 ## 0–100: how unevenly the lines start along the flame
    flameStyle*: FlameStyle
    flameShape*: FlameShape
    randomizeShapes*: bool
    seed*: uint32                  ## 0–100: arrangement seed
    quality*: uint32               ## 0 (draft) … 4 (fine): segments per flame line

proc defaultFlameSpec*(): FlameSpec =
  FlameSpec(flameType: ftOneFlameAlongPath, length: 150, width: 40, angle: 0, interval: 60, adjustIntervalForLoops: true,
            turbulent: 25, jag: 25, opacity: 75, flameLines: 20, flameBottomAlignment: 20, flameStyle: fsNormal,
            flameShape: fhParallel, quality: 2)

let FlameStops = [(0'f32, [1'f32, 0.98'f32, 0.85'f32]), (0.2'f32, [1'f32, 0.86'f32, 0.35'f32]),
                  (0.45'f32, [1'f32, 0.55'f32, 0.08'f32]), (0.75'f32, [0.9'f32, 0.25'f32, 0.02'f32]),
                  (1'f32, [0.6'f32, 0.08'f32, 0'f32])]

proc flameColour(t: float32; custom: Option[array[4, float32]]): array[4, float32] =
  ## Natural flame colour at position `t` along the flame (0 = base, 1 = tip).
  let a = pow(1'f32 - t, 0.7'f32) * 0.9'f32 + 0.1'f32 * (1'f32 - t)
  if custom.isSome:
    let c = custom.get
    # Hot near-white core → the colour → a darker rim.
    let core = [c[0] * 0.4'f32 + 0.6'f32, c[1] * 0.4'f32 + 0.6'f32, c[2] * 0.4'f32 + 0.6'f32]
    let rim = [c[0] * 0.6'f32, c[1] * 0.6'f32, c[2] * 0.6'f32]
    let k = clamp(t, 0'f32, 1'f32)
    var rgb: array[3, float32]
    if k < 0.35'f32:
      let u = k / 0.35'f32
      rgb = [core[0] + (c[0] - core[0]) * u, core[1] + (c[1] - core[1]) * u, core[2] + (c[2] - core[2]) * u]
    else:
      let u = (k - 0.35'f32) / 0.65'f32
      rgb = [c[0] + (rim[0] - c[0]) * u, c[1] + (rim[1] - c[1]) * u, c[2] + (rim[2] - c[2]) * u]
    [rgb[0], rgb[1], rgb[2], a * c[3]]
  else:
    let k = clamp(t, 0'f32, 1'f32)
    var rgb = FlameStops[4][1]
    for i in 1 ..< FlameStops.len:
      let (w0, w1) = (FlameStops[i - 1], FlameStops[i])
      if k <= w1[0]:
        let u = (k - w0[0]) / (w1[0] - w0[0])
        rgb = [w0[1][0] + (w1[1][0] - w0[1][0]) * u, w0[1][1] + (w1[1][1] - w0[1][1]) * u, w0[1][2] + (w1[1][2] - w0[1][2]) * u]
        break
    [rgb[0], rgb[1], rgb[2], a]

proc roundUsize(v: float32): int = asIdx(round(v))

proc drawFlame(o: var seq[Prim]; spine: Spine; width: float32; spec: FlameSpec; rng: var RenderRng) =
  ## One flame: its spine (base → tip), how wide it is, and its random stream.
  let len = spine.len
  if len < 1'f32: return
  let lines = roundUsize(clamp(spec.flameLines, 1'f32, 100'f32) * 0.6'f32 + 3'f32)
  let segs = min(max([6, 10, 16, 24, 36][int(min(spec.quality, 4'u32))], asIdx(len / 12'f32)), 160)
  var (turb, jag) = (clamp(spec.turbulent, 0'f32, 100'f32) / 100'f32, clamp(spec.jag, 0'f32, 100'f32) / 100'f32)
  case spec.flameStyle
  of fsViolent:
    turb = rmin(turb * 1.8'f32 + 0.2'f32, 2'f32)
    jag = rmin(jag * 1.8'f32 + 0.15'f32, 2'f32)
  of fsFlat:
    turb *= 0.35'f32
    jag *= 0.35'f32
  of fsNormal: discard
  let shape = (if spec.randomizeShapes: [fhParallel, fhToCenter, fhSpread, fhOval, fhPointed][int(rng.nextU64() mod 5)]
               else: spec.flameShape)
  let opacity = clamp(spec.opacity, 0'f32, 100'f32) / 100'f32
  let half = rmax(width, 1'f32) / 2'f32
  let seed = rng.nextU64()
  # The whole flame sways; each line adds its own flicker on top.
  proc sway(u: float32): float32 = turb * half * 1.6'f32 * fbm1(u * 1.7'f32, seed xor 0x5A5A'u64) * pow(u, 1.4'f32)
  # A soft additive body glow along the spine.
  for k in 0 ..< 6:
    let u = float32(k) / 6'f32 * 0.75'f32 + 0.05'f32
    let ((px, py), (dx, dy)) = spine.at(u * len)
    let off = sway(u)
    let c = flameColour(u, spec.color)
    let r = half * (1.25'f32 - u) + 2'f32
    o.add ellipse((px - dy * off, py + dx * off), r, r * 1.2'f32, arctan2(dy, dx), [c[0], c[1], c[2], 0.16'f32 * opacity],
                  [c[0], c[1] * 0.6'f32, c[2] * 0.3'f32, 0'f32]).withSoft(rmax(r, 2'f32)).additive()
  let lineR = clamp(half / float32(lines) * 2.2'f32, 0.8'f32, 10'f32)
  for li in 0 ..< lines:
    let lane = (if lines == 1: 0'f32 else: float32(li) / float32(lines - 1) * 2'f32 - 1'f32)
    let lseed = seed xor (uint64(li) * 0x51_7CC1_B727_220A'u64)
    let start = (clamp(spec.flameBottomAlignment, 0'f32, 100'f32) / 100'f32) * rng.f() * 0.4'f32
    # Outer lines are shorter, giving the flame its tongue shape.
    let reach = (1'f32 - 0.55'f32 * pow(abs(lane), 1.5'f32)) * rng.range(0.7'f32, 1'f32)
    let endU = clamp(start + reach * (1'f32 - start), start + 0.05'f32, 1'f32)
    let lr = lineR * rng.range(0.6'f32, 1.3'f32)
    var prev = none(((float32, float32), float32, float32))
    for k in 0 .. segs:
      let u = start + (endU - start) * float32(k) / float32(segs)
      let s = u * len
      let ((px, py), (dx, dy)) = spine.at(s)
      let (nx, ny) = (-dy, dx)
      let spread = (case shape
        of fhParallel: 1'f32 - 0.6'f32 * u
        of fhToCenter: 1'f32 - u
        of fhSpread: 1'f32 + u
        of fhOval: sin(Pi32 * (0.15'f32 + 0.85'f32 * u)) * 1.3'f32
        of fhPointed: (1'f32 - u) * (1'f32 - u))
      let flick = turb * half * 0.5'f32 * fbm1(s / (len * 0.2'f32 + 6'f32) + float32(li) * 0.37'f32, lseed) * u +
                  jag * half * 0.25'f32 * noise1(s / 5'f32, lseed xor 0x77'u64) * u
      let off = lane * half * spread + sway(u) + flick
      let p = (px + nx * off, py + ny * off)
      let r = lr * (1'f32 - u * 0.75'f32)
      # Lines fade out towards their own tip.
      let fadeT = clamp((u - start) / rmax(endU - start, 1e-3'f32), 0'f32, 1'f32)
      if prev.isSome:
        let (q, rq, uq) = prev.get
        # Fade in over the first tenth (a soft base) and out towards the line's tip.
        template fc(cq: array[4, float32]; uu, tq: float32): array[4, float32] =
          [cq[0], cq[1], cq[2], cq[3] * opacity * (1'f32 - tq * tq * tq) * sqrt(clamp((uu - start) / 0.08'f32, 0'f32, 1'f32))]
        o.add capsule(q, p, rq, r, fc(flameColour(uq, spec.color), uq, fadeT), fc(flameColour(u, spec.color), u, fadeT)).withSoft(
          1'f32 + lr * 0.6'f32).lighten()
      prev = some((p, r, u))

proc flame*(spec: FlameSpec; paths: openArray[seq[(float32, float32)]]): seq[Prim] =
  ## Builds flame primitives. `paths` are flattened polylines in document pixels.
  var rng = newRenderRng(spec.seed, 0xF1A3)
  let length = clamp(spec.length, 1'f32, 1000'f32)
  let width = clamp(spec.width, 1'f32, 1000'f32)
  let interval = clamp(spec.interval, 1'f32, 1000'f32)
  proc pickLen(rng: var RenderRng; v: float32): float32 =
    if spec.randomizeLength or v > 0: length * rng.range(1'f32 - rmax(v, 0.4'f32), 1'f32 + rmax(v, 0.4'f32) * 0.5'f32) else: length
  for path in paths:
    if path.len == 0: continue
    let spine = newSpine(path)
    let total = spine.len
    if spec.flameType == ftOneFlameAlongPath and path.len >= 2 and total >= 1'f32:
      drawFlame(result, spine, width, spec, rng)
    elif spec.flameType in {ftCandleLight, ftOneFlameAlongPath}:
      # A candle: a short teardrop rising straight up from the start.
      let base = path[0]
      let h = rmin(length, 400'f32)
      let candle = newSpine(@[base, (base[0], base[1] - h)])
      var c = spec
      c.flameShape = fhOval
      c.turbulent = spec.turbulent * 0.3'f32
      c.jag = spec.jag * 0.2'f32
      drawFlame(result, candle, rmax(width * 0.6'f32, 4'f32), c, rng)
      let core = flameColour(0.05'f32, spec.color)
      result.add ellipse((base[0], base[1] - h * 0.28'f32), rmax(width * 0.12'f32, 1.5'f32), h * 0.22'f32, 0,
                         [core[0], core[1], core[2], 0.9'f32 * clamp(spec.opacity, 0'f32, 100'f32) / 100'f32],
                         [core[0], core[1], core[2], 0'f32]).withSoft(2).additive()
    else:
      let closed = path.len > 2 and abs(path[0][0] - path[^1][0]) < 1'f32 and abs(path[0][1] - path[^1][1]) < 1'f32
      var count: int
      if spec.adjustIntervalForLoops and closed:
        let c = round(total / interval)
        count = (if c.isFinite and c > 1'f32: min(asIdx(c), 2048) else: 1)
      else:
        let c = floor(total / interval)
        count = (if c.isFinite and c >= 0'f32: min(asIdx(c), 2047) + 1 else: 1)
      let step = (if spec.adjustIntervalForLoops and closed: total / float32(count) else: interval)
      for k in 0 ..< count:
        if result.len >= 65536: break
        let s = float32(k) * step
        let ((x, y), (dx, dy)) = spine.at(s)
        var dir: (float32, float32)
        case spec.flameType
        of ftMultipleFlamesPathDirections: dir = (dx, dy)
        of ftMultipleFlamesOneDirection:
          let a = toRadians(spec.angle)
          dir = (sin(a), -cos(a))
        # Left of the path direction (up for a left-to-right path).
        else: dir = (dy, -dx)
        let v = (if spec.flameType == ftMultipleFlamesVariousLength: 0.7'f32 else: 0'f32)
        let l = pickLen(rng, v)
        let fl = newSpine(@[(x, y), (x + dir[0] * l, y + dir[1] * l)])
        drawFlame(result, fl, width, spec, rng)

proc defaultFlamePath*(canvas: Rect): seq[(float32, float32)] =
  ## The path Flame uses when the document has none: a gentle arc across the lower part of the
  ## canvas.
  let (w, h) = (float32(canvas.width), float32(canvas.height))
  for k in 0 .. 32:
    let t = float32(k) / 32'f32
    result.add (float32(canvas.x0) + w * (0.15'f32 + 0.7'f32 * t), float32(canvas.y0) + h * (0.8'f32 - 0.08'f32 * sin(Pi32 * t)))

# ---------------------------------------------------------------- tree

type
  Leaf = enum lfNone, lfRound, lfOval, lfNeedle, lfHeart, lfBlossom, lfLong, lfFrond
  Habit = enum
    hbBranching ## recursive branching (deciduous trees)
    hbConifer   ## a dominant stem with whorls of side branches
    hbPalm      ## a tall trunk crowned with fronds
    hbBamboo    ## several straight jointed stalks

  TreeKind = object
    name: string
    habit: Habit
    levels, children: uint32
    spread: float32 ## branch spread (degrees from the parent)
    ratio: float32  ## child / parent length ratio
    trunk: float32  ## trunk share of the total height
    droop: float32  ## downward bend per unit length at deep levels
    wiggle: float32 ## random bend
    thick: float32  ## trunk radius as a fraction of the height
    leaf: Leaf
    leafCol, bark: array[3, float32]
    accent: Option[array[3, float32]] ## fruit or blossom accent dots

proc tk(name: string; habit: Habit; levels, children: uint32; spread, ratio, trunk, droop, wiggle, thick: float32;
        leaf: Leaf; leafCol, bark: array[3, float32]; accent = none(array[3, float32])): TreeKind =
  TreeKind(name: name, habit: habit, levels: levels, children: children, spread: spread, ratio: ratio, trunk: trunk,
           droop: droop, wiggle: wiggle, thick: thick, leaf: leaf, leafCol: leafCol, bark: bark, accent: accent)

const
  Brown = [0.36'f32, 0.25, 0.16]
  GreyBark = [0.45'f32, 0.42, 0.38]
  Green = [0.24'f32, 0.52, 0.16]
  DarkGreen = [0.12'f32, 0.36, 0.14]

let Trees = [
  tk("Oak", hbBranching, 5, 3, 38, 0.72, 0.35, 0.05, 0.35, 0.035, lfRound, Green, Brown),
  tk("Maple", hbBranching, 5, 3, 34, 0.74, 0.32, 0, 0.25, 0.03, lfHeart, [0.3'f32, 0.55, 0.15], Brown),
  tk("Pine", hbConifer, 3, 2, 70, 0.5, 0.15, 0.25, 0.1, 0.025, lfNeedle, DarkGreen, Brown),
  tk("Palm", hbPalm, 1, 9, 0, 0, 0.85, 0, 0.15, 0.022, lfFrond, [0.25'f32, 0.5, 0.18], [0.55'f32, 0.45, 0.32]),
  tk("Birch", hbBranching, 5, 2, 28, 0.75, 0.4, 0.1, 0.3, 0.02, lfOval, [0.45'f32, 0.65, 0.2], [0.88'f32, 0.86, 0.82]),
  tk("Weeping Willow", hbBranching, 5, 3, 40, 0.78, 0.3, 0.9, 0.2, 0.035, lfLong, [0.45'f32, 0.62, 0.22], GreyBark),
  tk("Cherry Blossom", hbBranching, 5, 3, 42, 0.7, 0.3, 0, 0.45, 0.03, lfBlossom, [0.98'f32, 0.75, 0.82], [0.3'f32, 0.2, 0.17]),
  tk("Bamboo", hbBamboo, 1, 5, 6, 0, 1, 0, 0.05, 0.012, lfLong, [0.35'f32, 0.6, 0.2], [0.55'f32, 0.65, 0.3]),
  tk("Cypress", hbBranching, 5, 3, 10, 0.8, 0.1, -0.1, 0.1, 0.02, lfNeedle, DarkGreen, Brown),
  tk("Baobab", hbBranching, 3, 4, 50, 0.55, 0.65, 0, 0.3, 0.09, lfRound, [0.35'f32, 0.55, 0.2], [0.55'f32, 0.47, 0.4]),
  tk("Ginkgo", hbBranching, 4, 3, 30, 0.72, 0.35, 0, 0.2, 0.025, lfHeart, [0.75'f32, 0.75, 0.2], GreyBark),
  tk("Spruce", hbConifer, 3, 2, 80, 0.45, 0.1, 0.45, 0.08, 0.025, lfNeedle, [0.1'f32, 0.3, 0.2], Brown),
  tk("Bare Winter Tree", hbBranching, 6, 2, 34, 0.74, 0.3, 0.05, 0.35, 0.03, lfNone, Green, [0.3'f32, 0.25, 0.22]),
  tk("Apple", hbBranching, 5, 3, 46, 0.68, 0.3, 0.1, 0.4, 0.03, lfRound, Green, Brown, some([0.85'f32, 0.1, 0.1])),
  tk("Acacia", hbBranching, 4, 3, 55, 0.75, 0.45, -0.25, 0.25, 0.03, lfRound, [0.35'f32, 0.5, 0.18], Brown),
  tk("Poplar", hbBranching, 5, 3, 12, 0.8, 0.15, -0.15, 0.1, 0.025, lfOval, [0.3'f32, 0.55, 0.15], GreyBark),
  tk("Elm", hbBranching, 5, 2, 24, 0.8, 0.3, 0.15, 0.25, 0.032, lfOval, Green, Brown),
  tk("Magnolia", hbBranching, 4, 3, 40, 0.7, 0.3, 0, 0.3, 0.03, lfBlossom, [0.98'f32, 0.9, 0.92], [0.35'f32, 0.28, 0.25],
     some([0.85'f32, 0.5, 0.65])),
  tk("Banana", hbPalm, 1, 7, 0, 0, 0.6, 0, 0.05, 0.04, lfFrond, [0.45'f32, 0.7, 0.2], [0.5'f32, 0.6, 0.3]),
  tk("Bonsai", hbBranching, 4, 3, 60, 0.6, 0.45, 0.2, 0.7, 0.05, lfRound, DarkGreen, [0.4'f32, 0.3, 0.22]),
  tk("Jacaranda", hbBranching, 5, 3, 44, 0.72, 0.3, 0, 0.35, 0.03, lfBlossom, [0.6'f32, 0.45, 0.85], Brown),
  tk("Autumn Maple", hbBranching, 5, 3, 34, 0.74, 0.32, 0, 0.25, 0.03, lfHeart, [0.85'f32, 0.3, 0.1], Brown),
  tk("Golden Autumn", hbBranching, 5, 3, 36, 0.72, 0.33, 0.05, 0.3, 0.03, lfRound, [0.9'f32, 0.7, 0.15], Brown),
  tk("Olive", hbBranching, 4, 3, 48, 0.68, 0.35, 0.1, 0.6, 0.04, lfLong, [0.5'f32, 0.58, 0.42], GreyBark, some([0.25'f32, 0.28, 0.12])),
  tk("Fir", hbConifer, 3, 2, 75, 0.5, 0.12, 0.15, 0.05, 0.025, lfNeedle, [0.12'f32, 0.38, 0.22], Brown),
  tk("Cedar", hbConifer, 3, 3, 88, 0.6, 0.2, 0, 0.15, 0.03, lfNeedle, [0.2'f32, 0.4, 0.25], Brown),
  tk("Lemon", hbBranching, 4, 3, 45, 0.68, 0.3, 0.05, 0.35, 0.03, lfOval, DarkGreen, Brown, some([0.98'f32, 0.88, 0.15])),
  tk("Wisteria", hbBranching, 5, 3, 45, 0.72, 0.3, 0.7, 0.3, 0.03, lfBlossom, [0.7'f32, 0.55, 0.9], [0.35'f32, 0.3, 0.27]),
  tk("Young Sapling", hbBranching, 3, 2, 30, 0.7, 0.45, 0, 0.2, 0.015, lfOval, [0.4'f32, 0.7, 0.2], [0.45'f32, 0.35, 0.22]),
  tk("Dead Tree", hbBranching, 5, 2, 45, 0.68, 0.4, 0.1, 0.8, 0.035, lfNone, Green, [0.32'f32, 0.29, 0.26]),
  tk("Coconut Palm", hbPalm, 1, 11, 0, 0, 0.88, 0, 0.35, 0.018, lfFrond, [0.3'f32, 0.55, 0.15], [0.6'f32, 0.5, 0.36],
     some([0.4'f32, 0.3, 0.15])),
  tk("Shrub", hbBranching, 4, 4, 55, 0.7, 0.05, 0, 0.35, 0.02, lfRound, Green, Brown),
  tk("Plum Blossom", hbBranching, 5, 2, 48, 0.7, 0.3, 0.1, 0.6, 0.03, lfBlossom, [0.95'f32, 0.6, 0.7], [0.25'f32, 0.18, 0.15]),
  tk("Cloud Pine", hbBranching, 4, 3, 70, 0.65, 0.35, 0.2, 0.5, 0.035, lfNeedle, [0.15'f32, 0.4, 0.2], [0.35'f32, 0.27, 0.2])]

proc treeTypeNames*(): seq[string] =
  ## Names of the base tree types (1-based `baseTreeType` = index + 1).
  for t in Trees: result.add t.name

type
  TreeSpec* = object
    ## Tree options (Photoshop's Tree dialog).
    baseTreeType*: uint32       ## 1–34
    lightDirection*: uint32     ## 1 (light from the left) … 3 (above) … 5 (right)
    leavesAmount*: float32      ## 0–100
    leavesSize*: float32        ## 0–200 %
    branchesHeight*: float32    ## 50–300 %: how high the branching starts
    branchesThickness*: float32 ## 50–200 %
    defaultLeaves*: bool        ## use the type's leaf colour; otherwise `leavesColor`
    leavesColor*: array[4, float32]
    customBranchColor*: bool    ## use `branchesColor` instead of the type's bark
    branchesColor*: array[4, float32]
    flatShading*: bool          ## flat shading (no light variation)
    seed*: uint32
    x*, y*: float32             ## base position as a fraction of the canvas
    size*: float32              ## height as a fraction of the canvas height

proc defaultTreeSpec*(): TreeSpec =
  TreeSpec(baseTreeType: 1, lightDirection: 3, leavesAmount: 50, leavesSize: 100, branchesHeight: 100, branchesThickness: 100,
           defaultLeaves: true, leavesColor: [0.24'f32, 0.52, 0.16, 1], branchesColor: [0.36'f32, 0.25, 0.16, 1],
           x: 0.5, y: 0.95, size: 0.8)

type TreeCtx = object
  kind: TreeKind
  spec: TreeSpec
  rng: RenderRng
  branches: seq[Prim]
  leaves: seq[(float32, Prim)]
  bark, leaf: array[3, float32]
  light, centre: (float32, float32)
  leafPx: float32

proc shade(c: array[3, float32]; k: float32): array[4, float32] =
  [clamp(c[0] * k, 0'f32, 1'f32), clamp(c[1] * k, 0'f32, 1'f32), clamp(c[2] * k, 0'f32, 1'f32), 1'f32]

proc lightAt(t: var TreeCtx; p: (float32, float32)): float32 =
  ## Brightness of a leaf at `p` (lit side of the crown brighter).
  if t.spec.flatShading: return 1
  let (dx, dy) = (p[0] - t.centre[0], p[1] - t.centre[1])
  let l = rmax(sqrt(dx * dx + dy * dy), 1'f32)
  0.78'f32 + 0.32'f32 * ((dx * t.light[0] + dy * t.light[1]) / l) + 0.12'f32 * t.rng.sym()

proc addLeaf(t: var TreeCtx; p: (float32, float32); dir: float32) =
  let s = t.leafPx * t.rng.range(0.7'f32, 1.2'f32)
  let ang = dir + t.rng.sym() * 1.2'f32
  let k = t.lightAt(p)
  let col = t.leaf
  var prim: Prim
  case t.kind.leaf
  of lfNone: return
  of lfRound: prim = ellipse(p, s, s * 0.85'f32, ang, shade(col, k * 1.1'f32), shade(col, k * 0.8'f32))
  of lfOval: prim = ellipse(p, s * 1.2'f32, s * 0.55'f32, ang, shade(col, k * 1.1'f32), shade(col, k * 0.8'f32))
  of lfLong: prim = ellipse(p, s * 1.6'f32, s * 0.28'f32, ang, shade(col, k * 1.1'f32), shade(col, k * 0.8'f32))
  of lfHeart: prim = ellipse(p, s * 0.9'f32, s * 1.1'f32, ang, shade(col, k * 1.1'f32), shade(col, k * 0.75'f32))
  of lfNeedle:
    # A tuft of needles fanning out from the twig.
    let n = 7
    for j in 0 ..< n:
      let a = ang + (float32(j) / float32(n - 1) - 0.5'f32) * 2.4'f32 + t.rng.sym() * 0.15'f32
      let l = s * t.rng.range(1.1'f32, 1.7'f32)
      let q = (p[0] + cos(a) * l, p[1] + sin(a) * l)
      let kk = k + 0.1'f32 * t.rng.sym()
      t.leaves.add (kk, capsule(p, q, rmax(s * 0.1'f32, 0.45'f32), rmax(s * 0.05'f32, 0.3'f32), shade(col, kk * 0.85'f32),
                                shade(col, kk * 1.1'f32)))
    return
  of lfBlossom: prim = ellipse(p, s * 0.75'f32, s * 0.75'f32, 0, shade(col, k * 1.08'f32), shade(col, k * 0.85'f32))
  of lfFrond: prim = ellipse(p, s * 1.6'f32, s * 0.3'f32, ang, shade(col, k), shade(col, k * 0.8'f32))
  let order = k + t.rng.f() * 0.05'f32
  t.leaves.add (order, prim)
  if t.kind.accent.isSome and t.rng.f() < 0.12'f32:
    let acc = t.kind.accent.get
    let q = (p[0] + t.rng.sym() * s, p[1] + t.rng.sym() * s)
    let r = rmax(s * 0.45'f32, 1'f32)
    t.leaves.add (k + 1'f32, ellipse(q, r, r, 0, shade(acc, k * 1.15'f32), shade(acc, k * 0.75'f32)))

proc limb(t: var TreeCtx; p: (float32, float32); ang, len, r0, r1, depth01: float32): ((float32, float32), float32) =
  ## A curved branch from `p` heading `ang` (radians, 0 = right, y down). Returns its end point and
  ## end heading.
  let segs = clamp(asIdx(len / 6'f32), 2, 10)
  var (q, a) = (p, ang)
  let droop = t.kind.droop * depth01
  let col0 = shade(t.bark, 0.9'f32 + 0.2'f32 * depth01)
  for k in 0 ..< segs:
    let t0 = float32(k) / float32(segs)
    let t1 = float32(k + 1) / float32(segs)
    a += t.kind.wiggle * t.rng.sym() * 0.25'f32
    # Gravity pulls the heading towards straight down (π/2).
    let diff = arctan2(sin(FracPi2 - a), cos(FracPi2 - a))
    a += diff * droop * 0.12'f32
    let step = len / float32(segs)
    let nq = (q[0] + cos(a) * step, q[1] + sin(a) * step)
    let (ra, rb) = (r0 + (r1 - r0) * t0, r0 + (r1 - r0) * t1)
    t.branches.add capsule(q, nq, ra, rb, col0, col0)
    if depth01 > 0.99'f32 and t.kind.leaf != lfNone and k > 0 and
        t.rng.f() < clamp(t.spec.leavesAmount, 0'f32, 100'f32) / 100'f32 * 0.9'f32:
      t.addLeaf(nq, a)
    q = nq
  (q, a)

proc branch(t: var TreeCtx; p: (float32, float32); ang, len, r: float32; level: uint32; depth = 0) =
  let levels = t.kind.levels
  let depth01 = float32(level) / float32(max(levels, 1'u32))
  let rEnd = r * 0.62'f32
  let (endP, a) = t.limb(p, ang, len, r, rEnd, depth01)
  if level >= levels or depth > 64:
    let amount = clamp(t.spec.leavesAmount, 0'f32, 100'f32) / 100'f32
    if t.kind.leaf != lfNone:
      let n = roundUsize(amount * 9'f32)
      for _ in 0 ..< n:
        let d = len * 0.5'f32 * t.rng.f()
        let th = t.rng.f() * Tau32
        t.addLeaf((endP[0] + cos(th) * d, endP[1] + sin(th) * d), a)
    return
  let spread = toRadians(t.kind.spread)
  let n = t.kind.children
  for c in 0'u32 ..< n:
    let side = (if n == 1: 0'f32 else: float32(c) / float32(n - 1) * 2'f32 - 1'f32)
    let ca = a + side * spread + t.rng.sym() * spread * 0.35'f32
    let cl = len * t.kind.ratio * t.rng.range(0.8'f32, 1.1'f32)
    let cr = rEnd * t.rng.range(0.75'f32, 0.95'f32)
    t.branch(endP, ca, cl, cr, level + 1, depth + 1)

proc tree*(spec: TreeSpec; canvas: Rect): seq[Prim] =
  ## Builds tree primitives inside `canvas`.
  let kind = Trees[int(clamp(spec.baseTreeType, 1'u32, uint32(Trees.len)) - 1)]
  let h = float32(canvas.height) * clamp(spec.size, 0.05'f32, 2'f32)
  let base = (float32(canvas.x0) + float32(canvas.width) * spec.x, float32(canvas.y0) + float32(canvas.height) * spec.y)
  let la = toRadians(case clamp(spec.lightDirection, 1'u32, 5'u32)
    of 1: 180'f32
    of 2: 225'f32
    of 3: 270'f32
    of 4: 315'f32
    else: 0'f32)
  let leaf = (if spec.defaultLeaves: kind.leafCol else: [spec.leavesColor[0], spec.leavesColor[1], spec.leavesColor[2]])
  let bark = (if spec.customBranchColor: [spec.branchesColor[0], spec.branchesColor[1], spec.branchesColor[2]] else: kind.bark)
  let thick = clamp(spec.branchesThickness, 50'f32, 200'f32) / 100'f32
  let bh = clamp(spec.branchesHeight, 50'f32, 300'f32) / 100'f32
  var t = TreeCtx(kind: kind, spec: spec, rng: newRenderRng(spec.seed, 0x7EE'u64 + uint64(spec.baseTreeType)), bark: bark, leaf: leaf,
                  light: (cos(la), sin(la)), centre: (base[0], base[1] - h * 0.65'f32),
                  leafPx: rmax(h * 0.012'f32 * clamp(spec.leavesSize, 0'f32, 200'f32) / 100'f32, 0.6'f32))
  let up = -FracPi2
  let r0 = rmax(h * kind.thick * thick, 0.8'f32)
  case kind.habit
  of hbBranching:
    let trunk = rmin(h * kind.trunk * bh, h * 0.92'f32)
    let (top, a) = t.limb(base, up, rmax(trunk, 1'f32), r0, r0 * 0.75'f32, 0)
    # Crown length so the tree reaches about `h`.
    var geo = -0'f32
    for k in 0'u32 ..< kind.levels: geo += powi(kind.ratio, int(k))
    let first = (rmax(h - trunk, h * 0.08'f32) / rmax(geo, 0.5'f32)) * 1.05'f32
    let n = max(kind.children, 2'u32)
    for c in 0'u32 ..< n:
      let side = float32(c) / float32(n - 1) * 2'f32 - 1'f32
      let sp = toRadians(kind.spread) * 0.8'f32
      let ca = a + side * sp + t.rng.sym() * 0.15'f32
      let fl = first * t.rng.range(0.85'f32, 1.05'f32)
      t.branch(top, ca, fl, r0 * 0.7'f32, 1)
  of hbConifer:
    let (top, _) = t.limb(base, up, h, r0, r0 * 0.1'f32, 0)
    let start = rmin(h * kind.trunk * bh, h * 0.8'f32)
    let whorls = asIdx(14'f32 * clamp(sqrt(h / 300'f32), 0.6'f32, 2.5'f32))
    let spread = toRadians(kind.spread)
    for k in 0 ..< whorls:
      let f = float32(k) / float32(whorls)
      let y = base[1] - start - (h - start) * f
      let x = base[0] + (top[0] - base[0]) * ((base[1] - y) / h)
      let span = (h - start) * 0.42'f32 * pow(1'f32 - f, 0.9'f32) + h * 0.03'f32
      for side in [-1'f32, 1'f32]:
        let ang = up + side * spread + t.rng.sym() * 0.1'f32
        t.branch((x, y), ang, span, rmax(r0 * 0.35'f32 * (1'f32 - f), 0.6'f32), kind.levels)
        if kind.levels >= 2:
          let (s2, c2) = (sin(ang + side * 0.5'f32), cos(ang + side * 0.5'f32))
          let mid = (x + c2 * span * 0.5'f32, y + s2 * span * 0.5'f32)
          t.branch(mid, ang + side * 0.6'f32, span * 0.4'f32, rmax(r0 * 0.15'f32, 0.5'f32), kind.levels)
  of hbPalm:
    let trunk = rmax(h * kind.trunk * rmin(bh, 1.15'f32), 1'f32)
    let lean = t.rng.sym() * kind.wiggle
    let segs = 12
    var p = base
    for k in 0 ..< segs:
      let f = float32(k) / float32(segs)
      let a = up + lean * f
      let q = (p[0] + cos(a) * trunk / float32(segs), p[1] + sin(a) * trunk / float32(segs))
      let r = r0 * (1'f32 - 0.35'f32 * f)
      let ring = shade(bark, (if k mod 2 == 0: 1'f32 else: 0.85'f32))
      t.branches.add capsule(p, q, r, r * 0.97'f32, ring, ring)
      p = q
    let fronds = int(kind.children)
    let flen = h * 0.38'f32 * sqrt(clamp(spec.leavesSize, 10'f32, 200'f32) / 100'f32)
    for k in 0 ..< fronds:
      let a = up + (float32(k) / float32(fronds) - 0.5'f32) * Pi32 * 1.7'f32 + t.rng.sym() * 0.2'f32
      var q = p
      var ang = a
      let fsegs = 10
      let leaflets = roundUsize(clamp(spec.leavesAmount, 0'f32, 100'f32) / 100'f32 * 3'f32)
      for s in 0 ..< fsegs:
        # Fronds arch over and droop towards the ground.
        let d = FracPi2 - ang
        ang += 0.06'f32 * arctan2(sin(d), cos(d))
        let nq = (q[0] + cos(ang) * flen / float32(fsegs), q[1] + sin(ang) * flen / float32(fsegs))
        let k2 = t.lightAt(nq)
        t.leaves.add (k2 - 0.1'f32, capsule(q, nq, rmax(r0 * 0.15'f32, 0.6'f32), rmax(r0 * 0.1'f32, 0.4'f32),
                                            shade(t.leaf, k2 * 0.8'f32), shade(t.leaf, k2 * 0.8'f32)))
        for j in 0 ..< leaflets:
          let along = float32(j) / float32(leaflets)
          let at = (q[0] + (nq[0] - q[0]) * along, q[1] + (nq[1] - q[1]) * along)
          for side in [-1'f32, 1'f32]:
            let la2 = ang + side * 1.1'f32
            let ll = flen * 0.22'f32 * (1'f32 - float32(s) / float32(fsegs) * 0.7'f32)
            let tip = (at[0] + cos(la2) * ll, at[1] + sin(la2) * ll + ll * 0.25'f32)
            t.leaves.add (k2, capsule(at, tip, rmax(ll * 0.08'f32, 0.5'f32), 0.3'f32, shade(t.leaf, k2), shade(t.leaf, k2 * 0.9'f32)))
        q = nq
    if kind.accent.isSome:
      let acc = kind.accent.get
      for _ in 0 ..< 4:
        let c = (p[0] + t.rng.sym() * r0 * 2'f32, p[1] + t.rng.f() * r0 * 2'f32)
        t.leaves.add (2'f32, disc(c, r0 * 1.1'f32, shade(acc, 1)))
  of hbBamboo:
    let stalks = int(kind.children)
    for k in 0 ..< stalks:
      let x = base[0] + (float32(k) - float32(stalks - 1) / 2'f32) * r0 * 5'f32 + t.rng.sym() * r0
      let hh = h * t.rng.range(0.7'f32, 1'f32)
      let a = up + t.rng.sym() * toRadians(kind.spread) * 0.5'f32
      let joints = 8
      var p = (x, base[1])
      for j in 0 ..< joints:
        let q = (p[0] + cos(a) * hh / float32(joints), p[1] + sin(a) * hh / float32(joints))
        t.branches.add capsule(p, q, r0, r0, shade(bark, 1), shade(bark, 0.85'f32))
        t.branches.add capsule((q[0] - r0 * 1.1'f32, q[1]), (q[0] + r0 * 1.1'f32, q[1]), r0 * 0.25'f32, r0 * 0.25'f32,
                               shade(bark, 0.7'f32), shade(bark, 0.7'f32))
        if j >= joints div 3:
          let leaves = roundUsize(clamp(spec.leavesAmount, 0'f32, 100'f32) / 100'f32 * 4'f32)
          for _ in 0 ..< leaves:
            let side = (if t.rng.f() < 0.5'f32: -1'f32 else: 1'f32)
            let la2 = a + side * t.rng.range(0.6'f32, 1.4'f32)
            let ll = t.leafPx * 2.2'f32
            t.addLeaf((q[0] + cos(la2) * ll, q[1] + sin(la2) * ll), la2)
        p = q
  # Back leaves first: darker ones underneath, lit ones on top.
  t.leaves.sort(proc(a, b: (float32, Prim)): int = (if a[0] < b[0]: -1 elif a[0] > b[0]: 1 else: 0))
  result = t.branches
  for (_, p) in t.leaves: result.add p

# ---------------------------------------------------------------- picture frame

const FrameStyles* = ["vineWithFlowers", "vineWithLeaves", "ivy", "roses", "daisies", "berries", "bamboo", "waves",
                      "zigzag", "dots", "rope", "scallops", "doubleLine", "snowflakes", "stars", "hearts"]
  ## Picture frame designs (index = `frame` − 1).

type
  FrameSpec* = object
    ## Picture Frame options.
    frame*: uint32       ## 1-based design (see `FrameStyles`)
    margin*: float32     ## distance of the frame from the canvas edge, % of the shorter side (0–30)
    size*: float32       ## ornament size 1–100
    arrangement*: float32 ## ornament spacing / density 1–100 (higher = denser)
    lines*: uint32       ## number of parallel lines for line designs (1–5)
    thickness*: float32  ## line / vine thickness 1–100
    fade*: float32       ## 0–100 %: fade the ornaments towards the inside
    vineColor*, flowerColor*, leafColor*: array[4, float32]
    seed*: uint32

  Border = object
    ## The frame rectangle walked by arc length (clockwise from the top-left).
    x0, y0, w, h: float32

proc defaultFrameSpec*(): FrameSpec =
  FrameSpec(frame: 1, margin: 4, size: 50, arrangement: 50, lines: 1, thickness: 30, fade: 0,
            vineColor: [0.25'f32, 0.4, 0.15, 1], flowerColor: [0.9'f32, 0.3, 0.45, 1], leafColor: [0.3'f32, 0.6, 0.2, 1])

proc len(b: Border): float32 = 2'f32 * (b.w + b.h)

proc at(b: Border; s0: float32): ((float32, float32), (float32, float32), (float32, float32)) =
  ## Point, unit tangent and inward normal at arc length `s`.
  let s = remEuclidF(s0, b.len)
  let (w, h) = (b.w, b.h)
  if s < w: ((b.x0 + s, b.y0), (1'f32, 0'f32), (0'f32, 1'f32))
  elif s < w + h: ((b.x0 + w, b.y0 + s - w), (0'f32, 1'f32), (-1'f32, 0'f32))
  elif s < 2'f32 * w + h: ((b.x0 + w - (s - w - h), b.y0 + h), (-1'f32, 0'f32), (0'f32, -1'f32))
  else: ((b.x0, b.y0 + h - (s - 2'f32 * w - h)), (0'f32, -1'f32), (1'f32, 0'f32))

proc rgb(c: array[4, float32]): array[3, float32] = [c[0], c[1], c[2]]

proc flower(o: var seq[Prim]; c: (float32, float32); r: float32; petals: int; petal, centre: array[4, float32]; rot: float32) =
  for k in 0 ..< petals:
    let a = rot + float32(k) / float32(petals) * Tau32
    let p = (c[0] + cos(a) * r * 0.55'f32, c[1] + sin(a) * r * 0.55'f32)
    o.add ellipse(p, r * 0.5'f32, r * 0.26'f32, a, petal, shade(rgb(petal), 0.8'f32))
  o.add ellipse(c, r * 0.28'f32, r * 0.28'f32, 0, centre, shade(rgb(centre), 0.75'f32))

proc leaf(o: var seq[Prim]; p: (float32, float32); ang, len: float32; col: array[4, float32]) =
  let c = (p[0] + cos(ang) * len * 0.5'f32, p[1] + sin(ang) * len * 0.5'f32)
  o.add ellipse(c, len * 0.5'f32, len * 0.22'f32, ang, shade(rgb(col), 1.12'f32), shade(rgb(col), 0.8'f32))

proc star(o: var seq[Prim]; c: (float32, float32); r: float32; points: int; col: array[4, float32]; rot: float32) =
  for k in 0 ..< points:
    let a = rot + float32(k) / float32(points) * Tau32
    o.add capsule(c, (c[0] + cos(a) * r, c[1] + sin(a) * r), r * 0.28'f32, 0.4'f32, col, col)

proc fract(v: float32): float32 = v - trunc(v)

proc pictureFrame*(spec: FrameSpec; canvas: Rect): seq[Prim] =
  ## Builds picture-frame primitives inside `canvas`.
  var o: seq[Prim]
  let short = rmax(float32(min(canvas.width, canvas.height)), 8'f32)
  let margin = short * clamp(spec.margin, 0'f32, 30'f32) / 100'f32
  let unit = short * (0.008'f32 + clamp(spec.size, 1'f32, 100'f32) / 100'f32 * 0.05'f32)
  let thick = rmax(unit * 0.04'f32 + short * 0.00012'f32 * clamp(spec.thickness, 1'f32, 100'f32), 0.6'f32)
  let spacing = unit * (4'f32 - 3.2'f32 * clamp(spec.arrangement, 1'f32, 100'f32) / 100'f32)
  let inset = margin + unit
  let b = Border(x0: float32(canvas.x0) + inset, y0: float32(canvas.y0) + inset,
                 w: rmax(float32(canvas.width) - 2'f32 * inset, 1'f32), h: rmax(float32(canvas.height) - 2'f32 * inset, 1'f32))
  let total = b.len
  let count = asIdx(rmax(round(total / spacing), 4'f32))
  let step = total / float32(count)
  var rng = newRenderRng(spec.seed, 0xF8A3E'u64 + uint64(spec.frame))
  let (vine, fl, lf) = (spec.vineColor, spec.flowerColor, spec.leafColor)
  let style = FrameStyles[int(clamp(spec.frame, 1'u32, uint32(FrameStyles.len)) - 1)]
  let lines = int(clamp(spec.lines, 1'u32, 5'u32))
  let white = [1'f32, 1, 1, 1]
  let yellow = [0.98'f32, 0.82, 0.2, 1]
  proc waveOff(s, amp, wl: float32): float32 = amp * sin(s / wl * Tau32)
  case style
  of "vineWithFlowers", "vineWithLeaves", "ivy", "roses", "daisies", "berries":
    let amp = unit * 0.5'f32
    let wl = step * 2'f32
    # A wavy vine along the border.
    block:
      let n = asIdx(rmax(ceil(total / 3'f32), 8'f32))
      var prev = none((float32, float32))
      for k in 0 .. n:
        let s = float32(k) / float32(n) * total
        let (p, _, nn) = b.at(s)
        let off = amp * sin(s / wl * Tau32)
        let q = (p[0] + nn[0] * off, p[1] + nn[1] * off)
        if prev.isSome: o.add capsule(prev.get, q, thick, thick, vine, vine)
        prev = some(q)
    for k in 0 ..< count:
      let s = float32(k) * step + step * 0.5'f32
      let (p, t, nn) = b.at(s)
      let off = waveOff(s, amp, wl)
      let q = (p[0] + nn[0] * off, p[1] + nn[1] * off)
      let side = (if k mod 2 == 0: 1'f32 else: -1'f32)
      let ta = arctan2(t[1], t[0])
      let la = ta + side * 0.9'f32 + rng.sym() * 0.3'f32
      let ll = unit * rng.range(0.9'f32, 1.2'f32)
      case style
      of "vineWithFlowers":
        leaf(o, q, la, ll, lf)
        if k mod 2 == 0:
          let c = (q[0] + nn[0] * unit * 0.7'f32 * side, q[1] + nn[1] * unit * 0.7'f32 * side)
          flower(o, c, unit * 0.65'f32, 5, fl, yellow, rng.f() * 6.3'f32)
      of "vineWithLeaves":
        leaf(o, q, la, ll, lf)
        leaf(o, q, ta - side * 0.9'f32, ll * 0.8'f32, lf)
      of "ivy":
        for j in 0 ..< 3:
          let a = la + (float32(j) - 1'f32) * 0.7'f32
          leaf(o, q, a, ll * 0.75'f32, shade(rgb(lf), 0.75'f32 + 0.15'f32 * float32(j)))
      of "roses":
        leaf(o, q, la, ll, lf)
        if k mod 3 == 0:
          let r = unit * 0.7'f32
          for j, f in [1'f32, 0.75'f32, 0.5'f32, 0.28'f32]:
            let c = shade(rgb(fl), 0.75'f32 + 0.1'f32 * float32(j))
            o.add ellipse(q, r * f, r * f * 0.9'f32, rng.f() * 3'f32, c, shade(rgb(fl), 0.55'f32))
      of "daisies":
        leaf(o, q, la, ll * 0.8'f32, lf)
        if k mod 2 == 1: flower(o, q, unit * 0.6'f32, 12, white, yellow, rng.f())
      else:
        leaf(o, q, la, ll, lf)
        for _ in 0 ..< 3:
          let c = (q[0] + rng.sym() * unit * 0.4'f32, q[1] + rng.sym() * unit * 0.4'f32)
          o.add ellipse(c, unit * 0.2'f32, unit * 0.2'f32, 0, shade(rgb(fl), 1.2'f32), shade(rgb(fl), 0.6'f32))
  of "bamboo":
    for edge in 0 ..< 4:
      let (s0, l) = (case edge
        of 0: (0'f32, b.w)
        of 1: (b.w, b.h)
        of 2: (b.w + b.h, b.w)
        else: (2'f32 * b.w + b.h, b.h))
      let (a, _, _) = b.at(s0 + 0.01'f32)
      let (z, _, _) = b.at(s0 + l - 0.01'f32)
      let joints = asIdx(rmax(round(l / (spacing * 2'f32)), 1'f32))
      for j in 0 ..< joints:
        let f0 = float32(j) / float32(joints)
        let f1 = float32(j + 1) / float32(joints)
        let p0 = (a[0] + (z[0] - a[0]) * f0, a[1] + (z[1] - a[1]) * f0)
        let p1 = (a[0] + (z[0] - a[0]) * f1, a[1] + (z[1] - a[1]) * f1)
        o.add capsule(p0, p1, unit * 0.35'f32, unit * 0.35'f32, vine, shade(rgb(vine), 0.8'f32))
        o.add disc(p1, unit * 0.4'f32, shade(rgb(vine), 0.7'f32))
      # A few leaves per edge.
      for _ in 0 ..< max(joints div 2, 1):
        let f = rng.f()
        let p = (a[0] + (z[0] - a[0]) * f, a[1] + (z[1] - a[1]) * f)
        leaf(o, p, rng.f() * 6.3'f32, unit * 1.4'f32, lf)
  of "waves", "zigzag", "doubleLine", "rope":
    for li in 0 ..< lines:
      let off = (float32(li) - float32(lines - 1) / 2'f32) * thick * 4'f32
      let n = asIdx(rmax(ceil(total / 2'f32), 8'f32))
      var prev = none((float32, float32))
      for k in 0 .. n:
        let s = float32(k) / float32(n) * total
        let (p, _, nn) = b.at(s)
        let extra = (case style
          of "waves": waveOff(s + float32(li) * step * 0.25'f32, unit * 0.4'f32, step)
          of "zigzag":
            let ph = fract(s / step)
            unit * 0.4'f32 * (4'f32 * abs(ph - 0.5'f32) - 1'f32)
          else: 0'f32)
        let ov = off + extra
        let q = (p[0] + nn[0] * ov, p[1] + nn[1] * ov)
        if style == "rope":
          if k mod 2 == 0:
            let (_, t, _) = b.at(s)
            o.add ellipse(q, unit * 0.45'f32, unit * 0.2'f32, arctan2(t[1], t[0]) + 0.7'f32, shade(rgb(vine), 1.1'f32),
                          shade(rgb(vine), 0.65'f32))
        elif prev.isSome:
          o.add capsule(prev.get, q, thick, thick, vine, vine)
        prev = some(q)
      if style == "doubleLine":
        # A second rule inside each line and corner squares.
        let inner = Border(x0: b.x0 + unit * 0.6'f32 + off, y0: b.y0 + unit * 0.6'f32 + off,
                           w: b.w - 2'f32 * (unit * 0.6'f32 + off), h: b.h - 2'f32 * (unit * 0.6'f32 + off))
        let starts = [0'f32, inner.w, inner.w + inner.h, 2'f32 * inner.w + inner.h]
        let ends = [inner.w, inner.w + inner.h, 2'f32 * inner.w + inner.h, inner.len - 0.001'f32]
        for e in 0 ..< 4:
          let (p, _, _) = inner.at(starts[e])
          let (q, _, _) = inner.at(ends[e])
          o.add capsule(p, q, thick * 0.6'f32, thick * 0.6'f32, vine, vine)
    if style == "doubleLine":
      let corners = [0'f32, b.w, b.w + b.h, 2'f32 * b.w + b.h]
      for e in 0 ..< 4:
        let (p, _, _) = b.at(corners[e])
        o.add ellipse(p, unit * 0.45'f32, unit * 0.45'f32, FracPi4, fl, fl)
  of "dots", "scallops", "snowflakes", "stars", "hearts":
    for k in 0 ..< count:
      let s = float32(k) * step
      let (p, t, nn) = b.at(s)
      let ta = arctan2(t[1], t[0])
      case style
      of "dots":
        let r = unit * (if k mod 2 == 0: 0.4'f32 else: 0.22'f32)
        o.add ellipse(p, r, r, 0, shade(rgb(fl), 1.15'f32), shade(rgb(fl), 0.75'f32))
      of "scallops":
        let (c0, _, _) = b.at(s - step * 0.5'f32)
        let mid = ((c0[0] + p[0]) / 2'f32, (c0[1] + p[1]) / 2'f32)
        let n = 8
        for j in 0 ..< n:
          let a0 = Pi32 * float32(j) / float32(n)
          let a1 = Pi32 * float32(j + 1) / float32(n)
          let r = step * 0.5'f32
          template pt(aq: float32): (float32, float32) =
            (mid[0] - t[0] * r * cos(aq) - nn[0] * r * sin(aq), mid[1] - t[1] * r * cos(aq) - nn[1] * r * sin(aq))
          o.add capsule(pt(a0), pt(a1), thick, thick, vine, vine)
      of "snowflakes":
        let r = unit * 0.6'f32
        let c = [0.85'f32, 0.92, 1, 1]
        for j in 0 ..< 6:
          let a = ta + float32(j) * FracPi3
          let e = (p[0] + cos(a) * r, p[1] + sin(a) * r)
          o.add capsule(p, e, rmax(thick * 0.6'f32, 0.6'f32), rmax(thick * 0.4'f32, 0.5'f32), c, c)
          let m = (p[0] + cos(a) * r * 0.6'f32, p[1] + sin(a) * r * 0.6'f32)
          for sd in [-0.7'f32, 0.7'f32]:
            let b2 = (m[0] + cos(a + sd) * r * 0.3'f32, m[1] + sin(a + sd) * r * 0.3'f32)
            o.add capsule(m, b2, rmax(thick * 0.4'f32, 0.5'f32), 0.4'f32, c, c)
      of "stars": star(o, p, unit * 0.55'f32, 5, (if k mod 2 == 0: yellow else: fl), ta - FracPi2)
      else:
        # Heart: two lobes and a point towards the inside.
        let r = unit * 0.3'f32
        let (tx, ty) = t
        let l = (p[0] - tx * r * 0.8'f32 - nn[0] * r * 0.3'f32, p[1] - ty * r * 0.8'f32 - nn[1] * r * 0.3'f32)
        let rr = (p[0] + tx * r * 0.8'f32 - nn[0] * r * 0.3'f32, p[1] + ty * r * 0.8'f32 - nn[1] * r * 0.3'f32)
        let tip = (p[0] + nn[0] * r * 1.6'f32, p[1] + nn[1] * r * 1.6'f32)
        o.add capsule(l, tip, r, 0.6'f32, fl, fl)
        o.add capsule(rr, tip, r, 0.6'f32, fl, fl)
  else: discard
  # Fade: lower the opacity of ornaments that reach further inside.
  let fade = clamp(spec.fade, 0'f32, 100'f32) / 100'f32
  if fade > 0:
    let cx = float32(canvas.x0) + float32(canvas.width) / 2'f32
    let cy = float32(canvas.y0) + float32(canvas.height) / 2'f32
    for p in o.mitems:
      let c = (if p.shape.kind == skCapsule: p.shape.a else: p.shape.c)
      let edge = clamp(rmax(abs(c[0] - cx) / (b.w / 2'f32), abs(c[1] - cy) / (b.h / 2'f32)), 0'f32, 1'f32)
      let k = 1'f32 - fade * (1'f32 - edge) * 8'f32
      p.c0[3] *= clamp(k, 0'f32, 1'f32)
      p.c1[3] *= clamp(k, 0'f32, 1'f32)
  o

# ---------------------------------------------------------------- JSON options

proc num(n: JsonNode; name: string): float64 =
  case n.kind
  of JInt: float64(n.getBiggestInt)
  of JFloat: n.getFloat
  else: raise newException(ValueError, "invalid type for " & name & ": expected a number")

proc whole(n: JsonNode; name: string): uint32 =
  if n.kind != JInt or n.getBiggestInt < 0 or n.getBiggestInt > int64(high(uint32)):
    raise newException(ValueError, "invalid value for " & name & ": expected a whole number")
  uint32(n.getBiggestInt)

proc boolean(n: JsonNode; name: string): bool =
  if n.kind != JBool: raise newException(ValueError, "invalid type for " & name & ": expected a boolean")
  n.getBool

proc colour(n: JsonNode; name: string): array[4, float32] =
  if n.kind != JArray or n.len != 4: raise newException(ValueError, "invalid value for " & name & ": expected [r, g, b, a]")
  for i in 0 .. 3: result[i] = float32(num(n[i], name))

proc enumValue[T: enum](n: JsonNode; name: string): T =
  if n.kind == JString:
    for v in T:
      if $v == n.getStr: return v
  raise newException(ValueError, "unknown variant for " & name & ": " & $n)

proc flameSpecFromJson*(j: JsonNode): FlameSpec =
  ## The options from their JSON form (camelCase, missing fields take their defaults); raises
  ## `ValueError` on a malformed value.
  if j.kind != JObject: raise newException(ValueError, "Flame options must be an object")
  result = defaultFlameSpec()
  for k, v in j:
    case k
    of "flameType": result.flameType = enumValue[FlameType](v, k)
    of "length": result.length = float32(num(v, k))
    of "randomizeLength": result.randomizeLength = boolean(v, k)
    of "width": result.width = float32(num(v, k))
    of "angle": result.angle = float32(num(v, k))
    of "interval": result.interval = float32(num(v, k))
    of "adjustIntervalForLoops": result.adjustIntervalForLoops = boolean(v, k)
    of "color": result.color = (if v.kind == JNull: none(array[4, float32]) else: some(colour(v, k)))
    of "turbulent": result.turbulent = float32(num(v, k))
    of "jag": result.jag = float32(num(v, k))
    of "opacity": result.opacity = float32(num(v, k))
    of "flameLines": result.flameLines = float32(num(v, k))
    of "flameBottomAlignment": result.flameBottomAlignment = float32(num(v, k))
    of "flameStyle": result.flameStyle = enumValue[FlameStyle](v, k)
    of "flameShape": result.flameShape = enumValue[FlameShape](v, k)
    of "randomizeShapes": result.randomizeShapes = boolean(v, k)
    of "seed": result.seed = whole(v, k)
    of "quality": result.quality = whole(v, k)
    else: discard

proc treeSpecFromJson*(j: JsonNode): TreeSpec =
  if j.kind != JObject: raise newException(ValueError, "Tree options must be an object")
  result = defaultTreeSpec()
  for k, v in j:
    case k
    of "baseTreeType": result.baseTreeType = whole(v, k)
    of "lightDirection": result.lightDirection = whole(v, k)
    of "leavesAmount": result.leavesAmount = float32(num(v, k))
    of "leavesSize": result.leavesSize = float32(num(v, k))
    of "branchesHeight": result.branchesHeight = float32(num(v, k))
    of "branchesThickness": result.branchesThickness = float32(num(v, k))
    of "defaultLeaves": result.defaultLeaves = boolean(v, k)
    of "leavesColor": result.leavesColor = colour(v, k)
    of "customBranchColor": result.customBranchColor = boolean(v, k)
    of "branchesColor": result.branchesColor = colour(v, k)
    of "flatShading": result.flatShading = boolean(v, k)
    of "seed": result.seed = whole(v, k)
    of "x": result.x = float32(num(v, k))
    of "y": result.y = float32(num(v, k))
    of "size": result.size = float32(num(v, k))
    else: discard

proc frameSpecFromJson*(j: JsonNode): FrameSpec =
  if j.kind != JObject: raise newException(ValueError, "Picture Frame options must be an object")
  result = defaultFrameSpec()
  for k, v in j:
    case k
    of "frame": result.frame = whole(v, k)
    of "margin": result.margin = float32(num(v, k))
    of "size": result.size = float32(num(v, k))
    of "arrangement": result.arrangement = float32(num(v, k))
    of "lines": result.lines = whole(v, k)
    of "thickness": result.thickness = float32(num(v, k))
    of "fade": result.fade = float32(num(v, k))
    of "vineColor": result.vineColor = colour(v, k)
    of "flowerColor": result.flowerColor = colour(v, k)
    of "leafColor": result.leafColor = colour(v, k)
    of "seed": result.seed = whole(v, k)
    else: discard
