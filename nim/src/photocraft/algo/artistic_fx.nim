## Pixel work of the Filter Gallery (port of `artistic_fx.rs`).
##
## Every effect runs on an sRGB working copy of the tile's input window (straight colour; the
## source alpha is kept) and may read up to `effectReach` pixels around each output pixel, so a
## stack of effects reads the sum of their reaches. Noise, strokes and textures are functions of
## document coordinates, so results do not depend on the tiling.

import std/math
import ../geom, ./params, ./image, ./noise, ./util
from ./fxutil import boxBlurN, gaussBlurN, gaussBoxRadii, cellPoint, rgba, setRgba, MaxBoxRadius
from ./filters import wrapAddI32

type
  Px = array[3, float32]
  Win = object
    ## The working window: sRGB colour of every pixel of `r`.
    r: Rect
    w, h: int
    c: seq[Px]
  Weight = proc(p: Px): float32 {.nimcall.}

let
  Pi32 = float32(PI)
  Sqrt1_2 = 0.70710678118654752'f32

proc rad(v: float32): int {.inline.} =
  ## `v as usize` for a radius, capped like the box radii (`fxutil.MaxBoxRadius`).
  min(asUsize(float64(v)), MaxBoxRadius)

proc i32(v: float32): int32 {.inline.} = int32(asI32(float64(v)))

# ---------------------------------------------------------------- pixel helpers

proc smoothstep(e0, e1, x: float32): float32 {.inline.} =
  ## Hermite step from `e0` to `e1`; `e0 > e1` gives the falling step.
  if abs(e1 - e0) < 1e-9'f32:
    return (if x >= e1: 1'f32 else: 0'f32)
  let t = clamp((x - e0) / (e1 - e0), 0'f32, 1'f32)
  t * t * (3'f32 - 2'f32 * t)

proc lum(p: Px): float32 {.inline.} = 0.299'f32 * p[0] + 0.587'f32 * p[1] + 0.114'f32 * p[2]

proc mix(a, b: Px; t: float32): Px {.inline.} =
  [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t, a[2] + (b[2] - a[2]) * t]

template mapPx(p0: Px; body: untyped): Px =
  block:
    let q = p0
    var res: Px
    for k in 0 .. 2:
      let it {.inject.} = q[k]
      res[k] = body
    res

proc grey(v: float32): Px {.inline.} = [v, v, v]
proc rgb(c: array[4, float32]): Px {.inline.} = [c[0], c[1], c[2]]
proc clamp01(v: float32): float32 {.inline.} = clamp(v, 0'f32, 1'f32)
proc sq(v: float32): float32 {.inline.} = v * v

proc posterize(v, levels: float32): float32 {.inline.} =
  let l = rmax(levels - 1'f32, 1'f32)
  round(clamp01(v) * l) / l

proc saturate(p: Px; k: float32): Px {.inline.} =
  let l = lum(p)
  mapPx(p, l + (it - l) * k)

proc xy(win: Win; i: int): (int, int) {.inline.} = (win.r.x0 + i mod win.w, win.r.y0 + i div win.w)

proc lum(win: Win): seq[float32] =
  result = newSeq[float32](win.c.len)
  for i, p in win.c: result[i] = lum(p)

proc sample(win: Win; x, y: float32): Px =
  ## Bilinear sample at document coordinates (pixel centres at .5), clamped to the window.
  let fx = clamp(x - 0.5'f32 - float32(win.r.x0), 0'f32, float32(win.w - 1))
  let fy = clamp(y - 0.5'f32 - float32(win.r.y0), 0'f32, float32(win.h - 1))
  let (x0, y0) = (asUsize(float64(floor(fx))), asUsize(float64(floor(fy))))
  let (x0c, y0c) = (min(x0, win.w - 1), min(y0, win.h - 1))
  let (x1, y1) = (min(x0c + 1, win.w - 1), min(y0c + 1, win.h - 1))
  let (ax, ay) = (fx - float32(x0), fy - float32(y0))
  template g(xx, yy: int): Px = win.c[yy * win.w + xx]
  let top = mix(g(x0c, y0c), g(x1, y0c), ax)
  let bot = mix(g(x0c, y1), g(x1, y1), ax)
  mix(top, bot, ay)

proc at(win: Win; x, y: int): Px =
  ## The pixel nearest to document `(x, y)`, clamped to the window.
  let xx = clamp(x - win.r.x0, 0, win.w - 1)
  let yy = clamp(y - win.r.y0, 0, win.h - 1)
  win.c[yy * win.w + xx]

proc flat(c: seq[Px]): seq[float32] =
  result = newSeq[float32](c.len * 3)
  for i, p in c:
    for k in 0 .. 2: result[i * 3 + k] = p[k]

proc unflat(f: seq[float32]; c: var seq[Px]) =
  for i in 0 ..< c.len:
    for k in 0 .. 2: c[i][k] = f[i * 3 + k]

proc gs(sigma: float32): int =
  ## Pixels a Gaussian blur of `sigma` reads (three box passes per axis).
  if sigma < 0.3'f32: 0
  else:
    var s = 0
    for r in gaussBoxRadii(sigma): s += r
    wrapAddI32(s, 1)

proc blur1(p: var seq[float32]; w, h: int; sigma: float32) = gaussBlurN(p, w, h, 1, sigma)

proc blur3(c: var seq[Px]; w, h: int; sigma: float32) =
  if sigma < 0.3'f32: return
  var f = flat(c)
  gaussBlurN(f, w, h, 3, sigma)
  unflat(f, c)

proc sobel(p: seq[float32]; w, h: int): (seq[float32], seq[float32]) =
  ## Sobel gradient (per pixel, edges clamped) of a plane.
  template at(x, y: int): float32 = p[clamp(y, 0, h - 1) * w + clamp(x, 0, w - 1)]
  var gx = newSeq[float32](w * h)
  var gy = newSeq[float32](w * h)
  for y in 0 ..< h:
    for x in 0 ..< w:
      gx[y * w + x] = ((at(x + 1, y - 1) + 2'f32 * at(x + 1, y) + at(x + 1, y + 1)) -
                       (at(x - 1, y - 1) + 2'f32 * at(x - 1, y) + at(x - 1, y + 1))) / 8'f32
      gy[y * w + x] = ((at(x - 1, y + 1) + 2'f32 * at(x, y + 1) + at(x + 1, y + 1)) -
                       (at(x - 1, y - 1) + 2'f32 * at(x, y - 1) + at(x + 1, y - 1))) / 8'f32
  (gx, gy)

proc gradMag(p: seq[float32]; w, h: int): seq[float32] =
  let (gx, gy) = sobel(p, w, h)
  result = newSeq[float32](gx.len)
  for i in 0 ..< gx.len: result[i] = sqrt(gx[i] * gx[i] + gy[i] * gy[i])

proc lineBlur(buf: var seq[float32]; n, w, h: int; d: (int, int); r0: int) =
  ## Running-mean blur of radius `r` steps along direction `d` ((1,0), (0,1), (1,1) or (1,-1))
  ## of an interleaved `n`-channel buffer.
  let r = min(r0, MaxBoxRadius)
  if r == 0 or w == 0 or h == 0: return
  proc inside(x, y: int): bool = x >= 0 and y >= 0 and x < w and y < h
  let norm = 1.0 / float64(2 * r + 1)
  var idx: seq[int]
  var vals: seq[float64]
  for sy in 0 ..< h:
    for sx in 0 ..< w:
      if inside(sx - d[0], sy - d[1]): continue # not the start of a line
      idx.setLen(0)
      var (x, y) = (sx, sy)
      while inside(x, y):
        idx.add (y * w + x) * n
        x += d[0]
        y += d[1]
      let m = idx.len
      for c in 0 ..< n:
        vals.setLen(0)
        for o in idx: vals.add float64(buf[o + c])
        template at(k: int): float64 = vals[clamp(k, 0, m - 1)]
        var acc = 0.0
        for k in -r .. r: acc += at(k)
        for k, o in idx:
          buf[o + c] = float32(acc * norm)
          acc += at(k + r + 1) - at(k - r)

proc lineBlur3(c: var seq[Px]; w, h: int; d: (int, int); r: int) =
  if r == 0 or w == 0 or h == 0: return
  var f = flat(c)
  lineBlur(f, 3, w, h, d, r)
  unflat(f, c)

proc direction(name: string): (int, int) =
  case name
  of "horizontal": (1, 0)
  of "vertical": (0, 1)
  of "leftDiagonal": (1, 1)
  else: (1, -1) # rightDiagonal: bottom-left to top-right

proc kuwahara(win: var Win; r0: int) =
  ## Kuwahara smoothing (the flattest of four quadrant means): the painterly base of several
  ## Artistic filters. Box means make the cost independent of `r`.
  let r = min(r0, MaxBoxRadius)
  let h2 = max((r + 1) div 2, 1)
  let (w, h) = (win.w, win.h)
  var m = newSeqOfCap[float32](w * h * 5)
  for p in win.c:
    let l = lum(p)
    m.add p[0]
    m.add p[1]
    m.add p[2]
    m.add l
    m.add l * l
  boxBlurN(m, w, h, 5, h2)
  let o = h2
  for y in 0 ..< h:
    for x in 0 ..< w:
      var best = high(float32)
      var col: Px
      for (dx, dy) in [(-o, -o), (o, -o), (-o, o), (o, o)]:
        let i = (clamp(y + dy, 0, h - 1) * w + clamp(x + dx, 0, w - 1)) * 5
        let v = m[i + 4] - m[i + 3] * m[i + 3]
        if v < best:
          best = v
          col = [m[i], m[i + 1], m[i + 2]]
      win.c[y * w + x] = col

proc kuwaharaReach(r0: int): int =
  let r = min(r0, MaxBoxRadius)
  2 * max((r + 1) div 2, 1) + 1

proc unsharp(win: var Win; sigma, amount: float32) =
  if amount <= 0: return
  var b = win.c
  blur3(b, win.w, win.h, sigma)
  for i in 0 ..< win.c.len:
    let (p, q) = (win.c[i], b[i])
    win.c[i] = [p[0] + (p[0] - q[0]) * amount, p[1] + (p[1] - q[1]) * amount, p[2] + (p[2] - q[2]) * amount]

proc vnoise(x, y: float32; seed: uint32): float32 =
  ## Smooth value noise (0–1) on a unit lattice.
  let (ix, iy) = (i32(floor(x)), i32(floor(y)))
  let (tx, ty) = (x - float32(ix), y - float32(iy))
  let (sx, sy) = (tx * tx * (3'f32 - 2'f32 * tx), ty * ty * (3'f32 - 2'f32 * ty))
  template h(a, b: int32): float32 = hash01(a, b, 0'u32, seed)
  let top = h(ix, iy) + (h(ix +% 1, iy) - h(ix, iy)) * sx
  let bot = h(ix, iy +% 1) + (h(ix +% 1, iy +% 1) - h(ix, iy +% 1)) * sx
  top + (bot - top) * sy

proc fbm(x, y: float32; seed: uint32): float32 =
  ## Three-octave value noise (0–1).
  (vnoise(x, y, seed) * 0.57'f32 + vnoise(x * 2.03'f32, y * 2.03'f32, seed xor 0x51) * 0.29'f32 +
   vnoise(x * 4.1'f32, y * 4.1'f32, seed xor 0xa3) * 0.14'f32) / 1'f32

proc strokes(x, y: int; d: (int, int); len, wid: float32; seed: uint32): float32 =
  ## Stroke texture: value noise stretched `len` px along the direction `d` and `wid` px across.
  let (dx, dy) = (float32(d[0]), float32(d[1]))
  let l = rmax(sqrt(dx * dx + dy * dy), 1e-6'f32)
  let (ux, uy) = (dx / l, dy / l)
  let (x, y) = (float32(x), float32(y))
  let u = x * ux + y * uy
  let v = -x * uy + y * ux
  vnoise(u / rmax(len, 0.5'f32), v / rmax(wid, 0.3'f32), seed)

proc paintStroke(src: Win; x, y: int; d: (int, int); len0, wid0: float32; seed: uint32): (Px, float32) =
  ## Repaints in discrete strokes `len` px long and `wid` px wide along `d`: every stroke takes
  ## the colour of `src` at its middle, and neighbouring strokes start at staggered points, so the
  ## image reads as separate strokes rather than a smear. Also returns where the pixel lies along
  ## its stroke (0 at the start, 1 at the end). Reads up to `len / 2 + 1` px away.
  let (dx, dy) = (float32(d[0]), float32(d[1]))
  let l = rmax(sqrt(dx * dx + dy * dy), 1e-6'f32)
  let (ux, uy) = (dx / l, dy / l)
  let (len, wid) = (rmax(len0, 1'f32), rmax(wid0, 0.5'f32))
  let (fx, fy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
  let u = fx * ux + fy * uy
  let v = -fx * uy + fy * ux
  let lane = i32(floor(v / wid))
  let off = hash01(lane, 0'i32, 0'u32, seed) * len
  let t = remEuclidF((u + off) / len, 1'f32)
  # Back to the stroke's middle along the direction.
  let du = (0.5'f32 - t) * len
  (src.sample(fx + du * ux, fy + du * uy), t)

proc lightDir(name: string): (float32, float32) =
  ## Unit vector toward the light in image coordinates (y down).
  let s = Sqrt1_2
  case name
  of "bottom": (0'f32, 1'f32)
  of "bottomLeft": (-s, s)
  of "left": (-1'f32, 0'f32)
  of "topLeft": (-s, -s)
  of "topRight": (s, -s)
  of "right": (1'f32, 0'f32)
  of "bottomRight": (s, s)
  else: (0'f32, -1'f32)

proc shade(gx, gy: float32; light: (float32, float32); k: float32): float32 {.inline.} =
  ## Lambert shading factor (1 = flat) of a height field with gradient `(gx, gy)`.
  let (nx, ny, nz) = (-k * gx, -k * gy, 1'f32)
  let nl = sqrt(nx * nx + ny * ny + nz * nz)
  let (lx, ly, lz) = (light[0], light[1], 1'f32)
  let ll = sqrt(lx * lx + ly * ly + lz * lz)
  let d = (nx * lx + ny * ly + nz * lz) / (nl * ll)
  d / (lz / ll)

proc weave(x, y, p, noise: float32): float32 =
  ## Basket weave of threads `p` px wide with `noise` irregularity.
  let (cx, cy) = (int(i32(floor(x / p))), int(i32(floor(y / p))))
  let overH = floorMod(cx + cy, 2) == 0
  let (u, v) = (remEuclidF(x, p) / p, remEuclidF(y, p) / p)
  # A thread is a rounded ridge across its cell.
  let ridge = (if overH: sin(v * Pi32) else: sin(u * Pi32))
  let n = vnoise(x / (p * 0.7'f32), y / (p * 2'f32), 97)
  ridge * (1'f32 - noise) + n * noise

proc texture(kind: string; x0, y0, s: float32): float32 =
  ## Procedural textures (0–1 heights) at document coordinates; `s` scales them (1 = 100 %).
  let (x, y) = (x0 / s, y0 / s)
  case kind
  of "brick":
    let (bw, bh) = (24'f32, 12'f32)
    let row = floor(y / bh)
    let xo = x + (if asI32(float64(row)) mod 2 == 0: 0'f32 else: bw / 2'f32)
    let (u, v) = (remEuclidF(xo, bw), remEuclidF(y, bh))
    let d = rmin(rmin(u, bw - u), rmin(v, bh - v))
    smoothstep(0.6'f32, 2.2'f32, d) * (0.85'f32 + 0.15'f32 * vnoise(x / 3'f32, y / 3'f32, 91))
  of "burlap": weave(x, y, 7'f32, 0.35'f32)
  of "canvas": weave(x, y, 3.5'f32, 0.18'f32)
  of "sandstone": fbm(x / 7'f32, y / 7'f32, 93) * 0.7'f32 + hash01(i32(x), i32(y), 0'u32, 94'u32) * 0.3'f32
  of "blocks":
    let b = 18'f32
    let (u, v) = (remEuclidF(x, b) / b, remEuclidF(y, b) / b)
    rmin(rmin(rmin(u, 1'f32 - u), rmin(v, 1'f32 - v)) * 4'f32, 1'f32)
  of "frosted": fbm(x / 2.5'f32, y / 2.5'f32, 95)
  of "tinyLens":
    let p = 12'f32
    let (u, v) = ((remEuclidF(x, p) / p - 0.5'f32) * 2'f32, (remEuclidF(y, p) / p - 0.5'f32) * 2'f32)
    sqrt(rmax(1'f32 - (u * u + v * v), 0'f32))
  else: 0.5'f32

proc one(p: Px): float32 = 1'f32

proc texturizeBy(win: var Win; kind: string; scaling, relief: float32; light: string; invert: bool; weight: Weight) =
  ## `texturize` with the relief scaled per pixel by `weight` of its colour (0–1).
  if relief <= 0: return
  let s = clamp(scaling / 100'f32, 0.5'f32, 2'f32)
  let l = lightDir(light)
  let k = relief / 50'f32 * 3'f32
  let sign = (if invert: -1'f32 else: 1'f32)
  for i in 0 ..< win.c.len:
    let (x, y) = win.xy(i)
    let (fx, fy) = (float32(x), float32(y))
    let gx = (texture(kind, fx + 1'f32, fy, s) - texture(kind, fx - 1'f32, fy, s)) * 0.5'f32 * sign
    let gy = (texture(kind, fx, fy + 1'f32, s) - texture(kind, fx, fy - 1'f32, s)) * 0.5'f32 * sign
    let f = 1'f32 + (clamp(shade(gx, gy, l, k), 0'f32, 2'f32) - 1'f32) * weight(win.c[i])
    win.c[i] = mapPx(win.c[i], it * f)

proc texturize(win: var Win; kind: string; scaling, relief: float32; light: string; invert: bool) =
  ## Texturizer core: relief-shades the window with a procedural texture.
  texturizeBy(win, kind, scaling, relief, light, invert, one)

proc voronoi(x, y, cell: float32; seed: uint32): (float32, float32, (float32, float32)) =
  ## Closest and second-closest jittered cell points (distances and the closest point).
  let (cx, cy) = (i32(floor(x / cell)), i32(floor(y / cell)))
  var (d1, d2, p1) = (high(float32), high(float32), (x, y))
  for j in -1'i32 .. 1'i32:
    for i in -1'i32 .. 1'i32:
      let p = cellPoint(int(cx +% i), int(cy +% j), cell, seed, (0'f32, 0'f32))
      let d = sqrt(sq(p[0] - x) + sq(p[1] - y))
      if d < d1:
        d2 = d1
        d1 = d
        p1 = p
      elif d < d2:
        d2 = d
  (d1, d2, p1)

proc highlights(p: Px; l, area, intensity: float32): Px =
  ## Brightens highlights (Film Grain / Smudge Stick "Highlight Area" and "Intensity").
  if area <= 0: return p
  let m = smoothstep(1'f32 - area / 20'f32 * 0.75'f32, 1.02'f32, l) * (1'f32 - intensity / 10'f32 * 0.6'f32)
  mapPx(p, it + (1'f32 - it) * m)

proc normalize3(v: array[3, float32]): array[3, float32] =
  let l = rmax(sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]), 1e-9'f32)
  [v[0] / l, v[1] / l, v[2] / l]

# ---------------------------------------------------------------- reach

proc cutoutSigma(e: GalleryEffect): float32 =
  0.5'f32 + e.get("edgeSimplicity") * (1.1'f32 - 0.3'f32 * e.get("edgeFidelity"))
proc daubsRadius(e: GalleryEffect): int =
  let wide = e.choice("brushType") in ["wideSharp", "wideBlurry"]
  let r = e.get("brushSize") / 2'f32 * (if wide: 1.5'f32 else: 1'f32)
  rad(rmax(r, 1'f32))
proc plasticSigma(e: GalleryEffect): float32 =
  0.5'f32 + e.get("smoothness") * 0.5'f32 + (15'f32 - e.get("detail")) * 0.2'f32
proc watercolorRadius(e: GalleryEffect): int = 1 + rad((15'f32 - e.get("brushDetail")) / 2.5'f32)
proc basreliefSigma(e: GalleryEffect): float32 =
  0.3'f32 + e.get("smoothness") * 0.5'f32 + (15'f32 - e.get("detail")) * 0.12'f32
proc patchSize(e: GalleryEffect): float32 = 3'f32 + e.get("squareSize") * 1.5'f32
proc glassCell(e: GalleryEffect): float32 = e.get("cellSize") * 1.6'f32 + 1'f32

proc wsum(xs: varargs[int]): int =
  ## i32 sum with wrapping, as Rust's release build adds reaches.
  for x in xs: result = wrapAddI32(result, x)

proc ai(v: float32): int {.inline.} = asI32(float64(v))

proc effectReach*(e: GalleryEffect): int =
  ## Pixels an effect reads around an output pixel.
  template g(k: string): float32 = e.get(k)
  case e.filter
  of gfColoredPencil: 2
  of gfCutout: gs(cutoutSigma(e))
  of gfDryBrush, gfFresco: wsum(kuwaharaReach(1 + rad(g("brushSize"))), gs(1'f32), 2)
  of gfFilmGrain, gfDiffuseGlow, gfChalkCharcoal, gfReticulation, gfGrain, gfMosaicTiles, gfTexturizer: 0
  of gfRoughPastels: wsum(gs(0.6'f32 + 1.2'f32), ai((2'f32 + g("strokeLength")) / 2'f32), 3)
  of gfNeonGlow: gs(abs(g("glowSize")) * 0.6'f32 + 0.5'f32)
  of gfPaintDaubs: wsum(kuwaharaReach(daubsRadius(e)), gs(1.5'f32), 1)
  of gfPaletteKnife: wsum(kuwaharaReach(rad(rmax(g("strokeSize") / 3'f32, 1'f32))), gs(g("softness") * 0.5'f32))
  of gfPlasticWrap: wsum(gs(plasticSigma(e)), 2)
  of gfPosterEdges: wsum(gs(0.6'f32 + g("edgeThickness") * 0.6'f32), 2)
  of gfSmudgeStick: wsum(ai(g("strokeLength") * 1.5'f32), 2)
  of gfSponge: kuwaharaReach(1 + rad(g("brushSize")))
  of gfUnderpainting: gs(0.5'f32 + g("brushSize") * 0.4'f32)
  of gfWatercolor: wsum(kuwaharaReach(watercolorRadius(e)), 2)
  of gfAccentedEdges: wsum(gs(0.4'f32 + g("smoothness") * 0.3'f32), gs(g("edgeWidth") * 0.5'f32), 2)
  of gfAngledStrokes: wsum(gs(0.8'f32), ai(rmax(g("strokeLength"), 3'f32) / 2'f32), gs(1'f32), 3)
  of gfCrosshatch: wsum(ai(g("strokeLength") / 3'f32), gs(1'f32), 2)
  of gfDarkStrokes: 9
  of gfInkOutlines: wsum(ai(g("strokeLength") / 2'f32), 3)
  of gfSpatter: wsum(ai(ceil(g("sprayRadius"))), 2)
  of gfSprayedStrokes: wsum(ai(ceil(g("sprayRadius"))), ai(g("strokeLength") / 2'f32), 3)
  of gfSumiE: wsum(gs(g("strokeWidth") * 0.2'f32), ai(g("strokeWidth") / 2'f32), 2)
  of gfGlass: wsum(ai(ceil(g("distortion") * 2'f32)), 2)
  of gfOceanRipple: wsum(ai(ceil(g("rippleMagnitude") * 0.9'f32)), 2)
  of gfBasRelief: wsum(gs(basreliefSigma(e)), 2)
  of gfCharcoal: wsum(gs(1'f32), 2)
  of gfChrome: wsum(gs(1'f32 + g("smoothness") * 0.4'f32), 2)
  of gfConteCrayon: 0
  of gfGraphicPen: wsum(ai(g("strokeLength") / 2'f32), 2)
  of gfHalftonePattern: gs(0.5'f32 + g("size") * 0.3'f32)
  of gfNotePaper: wsum(gs(1'f32), gs(1.2'f32), 2)
  of gfPhotocopy: gs(1'f32 + g("detail") * 0.5'f32)
  of gfPlaster: wsum(gs(0.5'f32 + g("smoothness") * 0.8'f32), gs(1.5'f32), 2)
  of gfStamp: gs(0.3'f32 + g("smoothness") * 0.3'f32)
  of gfTornEdges: gs(1'f32)
  of gfWaterPaper: wsum(int(cast[int32](uint32((2 * ai(g("fiberLength") / 6'f32)) and 0xFFFF_FFFF))), 2)
  of gfGlowingEdges: wsum(gs(0.3'f32 + g("smoothness") * 0.25'f32), gs(g("edgeWidth") * 0.4'f32), 2)
  of gfCraquelure: 0
  of gfPatchwork: wsum(ai(ceil(patchSize(e))), 2)
  of gfStainedGlass: wsum(ai(ceil(glassCell(e) * 2.5'f32)), 2)

proc reach*(effects: openArray[GalleryEffect]): int =
  ## Total reach of a stack of effects.
  for e in effects: result = wrapAddI32(result, effectReach(e))

# ---------------------------------------------------------------- effects

proc pastelWeight(p: Px): float32 = 0.15'f32 + 0.85'f32 * (1'f32 - clamp01(lum(p)))

proc apply(e: GalleryEffect; win: var Win; ctx: Ctx) =
  template g(k: string): float32 = e.get(k)
  let (w, h) = (win.w, win.h)
  let (fg, bg) = (rgb(e.foreground), rgb(e.background))
  case e.filter
  # ---------------- Artistic ----------------
  of gfColoredPencil:
    let pw = g("pencilWidth")
    let pressure = g("strokePressure") / 15'f32
    let pb = g("paperBrightness")
    let paper = mapPx(bg, it * (0.7'f32 + 0.3'f32 * pb / 50'f32))
    let l = win.lum
    let e2 = gradMag(l, w, h)
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let d = 1'f32 - l[i]
      let h1 = strokes(x, y, (1, 1), 5'f32 + pw * 3'f32, 0.5'f32 + pw * 0.45'f32, 11)
      let h2 = strokes(x, y, (1, -1), 5'f32 + pw * 3'f32, 0.5'f32 + pw * 0.45'f32, 12)
      let hatch = rmax(h1, h2 * smoothstep(0.3'f32, 0.8'f32, d))
      let cov = clamp01(d * (0.55'f32 + pressure) + e2[i] * 3'f32 - (1'f32 - hatch) * 0.8'f32 + 0.25'f32)
      win.c[i] = mix(paper, mapPx(win.c[i], it * 0.88'f32), cov)
  of gfCutout:
    blur3(win.c, w, h, cutoutSigma(e))
    let levels = round(g("numberOfLevels"))
    for p in win.c.mitems:
      # Flat tones per luminance level, keeping each region's hue.
      let l = rmax(lum(p), 1e-4'f32)
      let k = posterize(l, levels) / l
      p = mapPx(p, posterize(clamp01(it * k), levels * 2'f32))
  of gfDryBrush, gfFresco:
    let fresco = e.filter == gfFresco
    kuwahara(win, 1 + rad(g("brushSize")))
    unsharp(win, 1'f32, (if fresco: 0.9'f32 else: 0.5'f32))
    let levels = 3'f32 + g("brushDetail")
    let amp = 0.02'f32 + (g("texture") - 1'f32) * 0.05'f32
    let l = (if fresco: gradMag(win.lum, w, h) else: @[])
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let n = (hash01(x, y, 0, 21) - 0.5'f32) * amp
      var p = mapPx(win.c[i], posterize(it, levels) * 0.6'f32 + it * 0.4'f32 + n)
      if fresco:
        let li = l[i]
        p = mapPx(p, pow(clamp01((it - 0.5'f32) * 1.3'f32 + 0.45'f32), 1.15'f32) * (1'f32 - clamp01(li * 2.5'f32) * 0.6'f32))
      win.c[i] = p
  of gfFilmGrain:
    let grain = g("grain") / 20'f32
    let (area, inten) = (g("highlightArea"), g("intensity"))
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let l = lum(win.c[i])
      let n = (hash01(x, y, 0, 31) - 0.5'f32) * grain * 0.5'f32 * (0.3'f32 + 2.8'f32 * l * (1'f32 - l))
      win.c[i] = highlights(mapPx(win.c[i], it + n), l, area, inten)
  of gfNeonGlow:
    let size = g("glowSize")
    let br = g("glowBrightness") / 50'f32
    let glow = rgb(e.color)
    let l = win.lum
    var m = l
    if not (size >= 0):
      for v in m.mitems: v = 1'f32 - v
    blur1(m, w, h, abs(size) * 0.6'f32 + 0.5'f32)
    for i in 0 ..< win.c.len:
      let base = mix(fg, bg, l[i])
      let k = clamp01(m[i] * 1.2'f32) * br * 2'f32
      for c in 0 .. 2:
        win.c[i][c] = 1'f32 - (1'f32 - base[c]) * (1'f32 - clamp01(glow[c] * k))
  of gfPaintDaubs:
    let ty = e.choice("brushType")
    kuwahara(win, daubsRadius(e))
    let sharp = g("sharpness") / 40'f32
    case ty
    of "wideBlurry": blur3(win.c, w, h, 1.5'f32)
    of "wideSharp": unsharp(win, 1.5'f32, sharp * 4'f32)
    else: unsharp(win, 1.2'f32, sharp * 2.5'f32)
    if ty in ["lightRough", "darkRough", "sparkle"]:
      let edges = (if ty == "sparkle": gradMag(win.lum, w, h) else: @[])
      for i in 0 ..< win.c.len:
        let (x, y) = win.xy(i)
        let n = fbm(float32(x) / 2'f32, float32(y) / 2'f32, 41) - 0.5'f32
        win.c[i] = (case ty
          of "lightRough": mapPx(win.c[i], it + rmax(n, 0'f32) * 0.35'f32)
          of "darkRough": mapPx(win.c[i], it * (1'f32 - rmax(n, 0'f32) * 0.7'f32))
          else:
            let k = clamp01(edges[i] * 4'f32) * smoothstep(0.75'f32, 0.95'f32, hash01(x, y, 0, 42)) * 0.8'f32
            mapPx(win.c[i], it + k))
  of gfPaletteKnife:
    kuwahara(win, rad(rmax(g("strokeSize") / 3'f32, 1'f32)))
    let levels = 2'f32 + g("strokeDetail") * 3'f32
    for p in win.c.mitems: p = mapPx(p, posterize(it, levels) * 0.5'f32 + it * 0.5'f32)
    blur3(win.c, w, h, g("softness") * 0.5'f32)
  of gfPlasticWrap:
    let hs = g("highlightStrength") / 20'f32
    var l = win.lum
    blur1(l, w, h, plasticSigma(e))
    let (gx, gy) = sobel(l, w, h)
    let lv = normalize3([-0.45'f32, -0.65'f32, 1'f32])
    let hv = normalize3([lv[0], lv[1], lv[2] + 1'f32])
    for i in 0 ..< win.c.len:
      let k = 14'f32
      let n = normalize3([-k * gx[i], -k * gy[i], 1'f32])
      let ndh = rmax(n[0] * hv[0] + n[1] * hv[1] + n[2] * hv[2], 0'f32)
      let tilt = 1'f32 - n[2]
      let spec = pow(ndh, 30'f32) * smoothstep(0.01'f32, 0.12'f32, tilt) + smoothstep(0.25'f32, 0.6'f32, tilt) * 0.25'f32
      win.c[i] = mapPx(win.c[i], it * 0.88'f32 + spec * hs * 1.3'f32)
  of gfPosterEdges:
    var l = win.lum
    blur1(l, w, h, 0.6'f32 + g("edgeThickness") * 0.6'f32)
    let edge = gradMag(l, w, h)
    let inten = g("edgeIntensity") / 10'f32
    let levels = g("posterization") + 2'f32
    for i in 0 ..< win.c.len:
      let ink = smoothstep(0.015'f32, 0.06'f32, edge[i] * (1'f32 + inten * 3'f32)) * (0.55'f32 + inten * 0.45'f32)
      win.c[i] = mapPx(win.c[i], posterize(it, levels) * (1'f32 - ink))
  of gfRoughPastels:
    # Chalk strokes up to the right; Stroke Detail keeps more of the image inside them.
    let len = g("strokeLength")
    let det = (g("strokeDetail") - 1'f32) / 19'f32
    if len >= 1:
      var soft = win.c
      blur3(soft, w, h, 0.6'f32 + (1'f32 - det) * 1.2'f32)
      let src = Win(r: win.r, w: w, h: h, c: soft)
      for i in 0 ..< win.c.len:
        let (x, y) = win.xy(i)
        let (c, t) = paintStroke(src, x, y, (1, -1), 2'f32 + len, 1.5'f32, 51)
        # A chalk stroke is a little lighter in its middle, grainy along its length.
        let n = (strokes(x, y, (1, -1), 2'f32 + len * 0.5'f32, 0.8'f32, 52) - 0.5'f32) * 0.12'f32 +
                (0.5'f32 - abs(t - 0.5'f32)) * 0.06'f32
        win.c[i] = mapPx(mix(c, win.c[i], det * 0.6'f32), it + n)
    # Bright areas are thick chalk; in dark ones it's scraped off to show the texture.
    texturizeBy(win, e.choice("texture"), g("scaling"), g("relief"), e.choice("light"), e.flag("invert"), pastelWeight)
  of gfSmudgeStick:
    lineBlur3(win.c, w, h, (1, 1), rad(g("strokeLength") * 1.5'f32))
    let (area, inten) = (g("highlightArea"), g("intensity"))
    for p in win.c.mitems:
      let l = lum(p)
      let q = mapPx(p, it * (0.7'f32 + 0.3'f32 * l))
      p = highlights(q, l, area, inten)
  of gfSponge:
    kuwahara(win, 1 + rad(g("brushSize")))
    let def = g("definition") / 25'f32
    let sc = 1.5'f32 + g("smoothness") * 0.5'f32
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let s = fbm(float32(x) / sc, float32(y) / sc, 61)
      let hole = smoothstep(0.58'f32, 0.75'f32, s)
      win.c[i] = mapPx(win.c[i], clamp01((it - 0.5'f32) * (1'f32 + def * 0.6'f32) + 0.5'f32) * (1'f32 - hole * def * 0.4'f32))
  of gfUnderpainting:
    let orig = win.c
    blur3(win.c, w, h, 0.5'f32 + g("brushSize") * 0.4'f32)
    for i in 0 ..< win.c.len: win.c[i] = saturate(mix(orig[i], win.c[i], 0.8'f32), 1.15'f32)
    let cov = g("textureCoverage") / 40'f32
    texturize(win, e.choice("texture"), g("scaling"), g("relief") * (0.5'f32 + cov * 1.5'f32), e.choice("light"), e.flag("invert"))
  of gfWatercolor:
    kuwahara(win, watercolorRadius(e))
    let sh = g("shadowIntensity") / 10'f32
    let edge = gradMag(win.lum, w, h)
    let amp = (g("texture") - 1'f32) * 0.04'f32
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let n = (fbm(float32(x) / 3'f32, float32(y) / 3'f32, 71) - 0.5'f32) * amp
      let k = 1'f32 - clamp01(edge[i] * (3'f32 + sh * 8'f32)) * 0.6'f32
      win.c[i] = mapPx(saturate(win.c[i], 1.2'f32), pow(clamp01(it * k + n), 1.1'f32 + sh * 0.6'f32))
  # ---------------- Brush Strokes ----------------
  of gfAccentedEdges:
    var l = win.lum
    blur1(l, w, h, 0.4'f32 + g("smoothness") * 0.3'f32)
    var edge = gradMag(l, w, h)
    blur1(edge, w, h, g("edgeWidth") * 0.5'f32)
    let eb = g("edgeBrightness") / 50'f32
    for i in 0 ..< win.c.len:
      let k = clamp01(edge[i] * 6'f32)
      win.c[i] =
        if eb >= 0.5'f32: mapPx(win.c[i], it + (1'f32 - it) * k * (eb - 0.5'f32) * 2'f32)
        else: mapPx(win.c[i], it * (1'f32 - k * (0.5'f32 - eb) * 2'f32))
  of gfAngledStrokes:
    # Light areas are painted in strokes going one way, dark areas the other way;
    # Direction Balance moves the split (0: all down-right, 100: all up-right).
    let len = rmax(g("strokeLength"), 3'f32)
    let bal = g("directionBalance") / 100'f32
    var soft = win.c
    blur3(soft, w, h, 0.8'f32)
    var l = newSeq[float32](soft.len)
    for i, p in soft: l[i] = lum(p)
    let src = Win(r: win.r, w: w, h: h, c: soft)
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let k = smoothstep(1'f32 - bal - 0.05'f32, 1'f32 - bal + 0.05'f32, l[i])
      let (down, _) = paintStroke(src, x, y, (1, 1), len, 2'f32, 83)
      let (up, _) = paintStroke(src, x, y, (1, -1), len, 2'f32, 84)
      let n = (strokes(x, y, (1, 1), len * 0.5'f32, 0.7'f32, 85) - 0.5'f32) * (1'f32 - k) +
              (strokes(x, y, (1, -1), len * 0.5'f32, 0.7'f32, 86) - 0.5'f32) * k
      win.c[i] = mapPx(mix(down, up, k), it + n * 0.1'f32)
    unsharp(win, 1'f32, 0.2'f32 + g("sharpness") / 10'f32 * 2.3'f32)
  of gfCrosshatch:
    let len = g("strokeLength")
    let r = rad(len / 3'f32)
    let st = g("strength")
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let n = (hash01(x, y, 0, 81) - 0.5'f32) * 0.12'f32 * st
      win.c[i] = mapPx(win.c[i], it + n)
    var a = win.c
    lineBlur3(a, w, h, (1, 1), r)
    lineBlur3(win.c, w, h, (1, -1), r)
    for i in 0 ..< win.c.len: win.c[i] = mix(a[i], win.c[i], 0.5'f32)
    unsharp(win, 1'f32, g("sharpness") / 20'f32 * 3'f32)
  of gfDarkStrokes:
    let (bal, bi, wi) = (g("balance") / 10'f32, g("blackIntensity") / 10'f32, g("whiteIntensity") / 10'f32)
    let l = win.lum
    var a = win.c
    lineBlur3(a, w, h, (1, 1), 3)
    lineBlur3(win.c, w, h, (1, -1), 8)
    let t = 0.25'f32 + bal * 0.5'f32
    for i in 0 ..< win.c.len:
      let d = mapPx(a[i], it * (1'f32 - bi * 0.85'f32))
      let li = mapPx(win.c[i], it + (1'f32 - it) * wi * 0.5'f32)
      win.c[i] = mix(d, li, smoothstep(t - 0.1'f32, t + 0.1'f32, l[i]))
  of gfInkOutlines:
    let (di, li) = (g("darkIntensity") / 50'f32, g("lightIntensity") / 50'f32)
    let l = win.lum
    let edge = gradMag(l, w, h)
    lineBlur3(win.c, w, h, (1, 1), rad(g("strokeLength") / 2'f32))
    for i in 0 ..< win.c.len:
      let ink = clamp01(edge[i] * 5'f32) * di * 1.5'f32
      let (lt, dk) = (smoothstep(0.5'f32, 1'f32, l[i]) * li, smoothstep(0.5'f32, 0'f32, l[i]) * di * 0.8'f32)
      win.c[i] = mapPx(win.c[i], (it + (1'f32 - it) * lt) * (1'f32 - dk) * (1'f32 - clamp01(ink)))
  of gfSpatter, gfSprayedStrokes:
    let radius = g("sprayRadius")
    let sc = (if e.filter == gfSpatter: 0.6'f32 + g("smoothness") * 0.25'f32 else: 1.2'f32)
    let src = Win(r: win.r, w: w, h: h, c: win.c)
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let (fx, fy) = (float32(x) / sc, float32(y) / sc)
      let dx = (vnoise(fx, fy, 91) - 0.5'f32) * 2'f32 * radius
      let dy = (vnoise(fx, fy, 92) - 0.5'f32) * 2'f32 * radius
      win.c[i] = src.sample(float32(x) + 0.5'f32 + dx, float32(y) + 0.5'f32 + dy)
    if e.filter == gfSprayedStrokes:
      lineBlur3(win.c, w, h, direction(e.choice("strokeDirection")), rad(g("strokeLength") / 2'f32))
  of gfSumiE:
    let sw = g("strokeWidth")
    let (pr, con) = (g("strokePressure") / 15'f32, g("contrast") / 40'f32)
    blur3(win.c, w, h, sw * 0.2'f32)
    lineBlur3(win.c, w, h, (1, 1), rad(sw / 2'f32))
    for p in win.c.mitems:
      let l = lum(p)
      let ink = pow(smoothstep(0.65'f32, 0.15'f32, l), 1'f32 - pr * 0.5'f32)
      p = mapPx(p, clamp01((it - 0.5'f32) * (1'f32 + con) + 0.5'f32) * (1'f32 - ink * 0.9'f32 * (0.5'f32 + con)))
  # ---------------- Distort ----------------
  of gfDiffuseGlow:
    let (gr, ga, ca) = (g("graininess") / 10'f32, g("glowAmount") / 20'f32, g("clearAmount") / 20'f32)
    let thr = 0.2'f32 + ca * 0.7'f32
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let l = lum(win.c[i])
      var m = smoothstep(thr - 0.3'f32, thr + 0.1'f32, l) * ga * 1.4'f32
      if hash01(x, y, 0, 101) < gr * 0.3'f32 * (m + 0.15'f32):
        m += 0.5'f32
      win.c[i] = mix(win.c[i], bg, clamp01(m))
  of gfGlass:
    let dist = g("distortion")
    let step = 0.5'f32 + g("smoothness") * 0.5'f32
    let kind = e.choice("texture")
    let s = clamp(g("scaling") / 100'f32, 0.5'f32, 2'f32)
    let sign = (if e.flag("invert"): -1'f32 else: 1'f32)
    let mx = dist * 2'f32
    let src = Win(r: win.r, w: w, h: h, c: win.c)
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let (fx, fy) = (float32(x), float32(y))
      let gx = texture(kind, fx + step, fy, s) - texture(kind, fx - step, fy, s)
      let gy = texture(kind, fx, fy + step, s) - texture(kind, fx, fy - step, s)
      let dx = clamp(gx * sign * dist * 3'f32, -mx, mx)
      let dy = clamp(gy * sign * dist * 3'f32, -mx, mx)
      win.c[i] = src.sample(fx + 0.5'f32 + dx, fy + 0.5'f32 + dy)
  of gfOceanRipple:
    let s = 2'f32 + g("rippleSize") * 1.2'f32
    let mag = g("rippleMagnitude") * 0.9'f32
    let src = Win(r: win.r, w: w, h: h, c: win.c)
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let (fx, fy) = (float32(x) / s, float32(y) / s)
      let dx = (vnoise(fx, fy, 111) - 0.5'f32) * 2'f32 * mag
      let dy = (vnoise(fx, fy, 112) - 0.5'f32) * 2'f32 * mag
      win.c[i] = src.sample(float32(x) + 0.5'f32 + dx, float32(y) + 0.5'f32 + dy)
  # ---------------- Sketch ----------------
  of gfBasRelief:
    var l = win.lum
    let orig = l
    blur1(l, w, h, basreliefSigma(e))
    let (gx, gy) = sobel(l, w, h)
    let lt = lightDir(e.choice("light"))
    let k = 3'f32 + g("detail") * 0.4'f32
    for i in 0 ..< win.c.len:
      let t = clamp01(orig[i] * 0.5'f32 + 0.25'f32 + k * -(gx[i] * lt[0] + gy[i] * lt[1]))
      win.c[i] = mix(fg, bg, t)
  of gfChalkCharcoal:
    let (ca, ch, pr) = (g("charcoalArea") / 20'f32, g("chalkArea") / 20'f32, g("strokePressure") / 5'f32)
    let mid = mix(fg, bg, 0.5'f32)
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let l = lum(win.c[i])
      let cn = strokes(x, y, (1, 1), 9'f32, 0.9'f32, 121) - 0.5'f32
      let kn = strokes(x, y, (1, -1), 9'f32, 0.9'f32, 122) - 0.5'f32
      let cc = smoothstep(0.25'f32, 0.6'f32, (1'f32 - l) * (0.4'f32 + ca * 1.2'f32) + cn * (0.6'f32 + pr * 0.4'f32) - 0.15'f32)
      let kc = smoothstep(0.25'f32, 0.6'f32, l * (0.4'f32 + ch * 1.2'f32) + kn * (0.6'f32 + pr * 0.4'f32) - 0.15'f32)
      win.c[i] = mix(mix(mid, fg, cc), bg, kc * (1'f32 - cc))
  of gfCharcoal:
    let (th, det, bal) = (g("charcoalThickness"), g("detail") / 5'f32, g("lightDarkBalance") / 100'f32)
    var l = win.lum
    blur1(l, w, h, 1'f32)
    let edge = gradMag(l, w, h)
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let sn = strokes(x, y, (1, 1), 8'f32 + th * 3'f32, 0.6'f32 + th * 0.5'f32, 131) - 0.5'f32
      let cov = smoothstep(1'f32 - bal - 0.2'f32, 1'f32 - bal + 0.2'f32, 1'f32 - l[i] + sn * 0.6'f32) +
                clamp01(edge[i] * det * 6'f32)
      win.c[i] = mix(bg, fg, clamp01(cov))
  of gfChrome:
    let (det, sm) = (g("detail") / 10'f32, g("smoothness") / 10'f32)
    var l = win.lum
    blur1(l, w, h, 1'f32 + sm * 4'f32)
    let edge = gradMag(l, w, h)
    for i in 0 ..< win.c.len:
      let v = 0.5'f32 + 0.5'f32 * sin((l[i] * (2.5'f32 + det * 3'f32) + edge[i] * 6'f32) * 2'f32 * Pi32)
      win.c[i] = grey(smoothstep(0.05'f32, 0.95'f32, v))
  of gfConteCrayon:
    let (fl, bl) = (g("foregroundLevel") / 15'f32, g("backgroundLevel") / 15'f32)
    let mid = mix(fg, bg, 0.5'f32)
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let l = lum(win.c[i])
      let sn = strokes(x, y, (1, -1), 7'f32, 1'f32, 141) - 0.5'f32
      let cf = clamp01((0.6'f32 - l) * 2'f32 * fl * 1.4'f32 + sn * 0.5'f32)
      let cb = clamp01((l - 0.4'f32) * 2'f32 * bl * 1.4'f32 - sn * 0.5'f32)
      win.c[i] = mix(mix(mid, fg, cf), bg, cb * (1'f32 - cf))
    texturize(win, e.choice("texture"), g("scaling"), g("relief"), e.choice("light"), e.flag("invert"))
  of gfGraphicPen:
    let r = rad(g("strokeLength") / 2'f32)
    let bal = g("lightDarkBalance") / 100'f32
    let l = win.lum
    var n = newSeq[float32](win.c.len)
    for i in 0 ..< n.len:
      let (x, y) = win.xy(i)
      n[i] = hash01(x, y, 0, 151)
    lineBlur(n, 1, w, h, direction(e.choice("strokeDirection")), r)
    let sd = 0.2887'f32 / sqrt(float32(2 * r + 1))
    for i in 0 ..< win.c.len:
      let z = (n[i] - 0.5'f32) / sd
      let t = l[i] + z * 0.16'f32
      win.c[i] = mix(bg, fg, smoothstep(bal + 0.04'f32, bal - 0.04'f32, t))
  of gfHalftonePattern:
    let size = g("size")
    let con = g("contrast") / 50'f32
    let cell = 3'f32 + size * 2'f32
    var l = win.lum
    blur1(l, w, h, 0.5'f32 + size * 0.3'f32)
    let b = ctx.bounds
    let (cx, cy) = (float32(b.x0 + b.x1) / 2'f32, float32(b.y0 + b.y1) / 2'f32)
    let ty = e.choice("patternType")
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let (fx, fy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
      let d = clamp01((1'f32 - l[i] - 0.5'f32) * (1'f32 + con * 3'f32) + 0.5'f32)
      let aa = 1'f32 / cell
      let ink = (case ty
        of "circle":
          let r = sqrt(sq(fx - cx) + sq(fy - cy)) / cell
          let q = abs((r - trunc(r)) - 0.5'f32) * 2'f32
          smoothstep(d + aa, d - aa, q)
        of "line":
          let fyc = fy / cell
          let q = abs((fyc - trunc(fyc)) - 0.5'f32) * 2'f32
          smoothstep(d + aa, d - aa, q)
        else:
          let (fxc, fyc) = (fx / cell, fy / cell)
          let (u, v) = ((fxc - trunc(fxc)) - 0.5'f32, (fyc - trunc(fyc)) - 0.5'f32)
          let q = sqrt(u * u + v * v) * 2'f32
          let rr = sqrt(d) * 1.15'f32
          smoothstep(rr + aa, rr - aa, q))
      win.c[i] = mix(bg, fg, ink)
  of gfNotePaper:
    let (bal, gr, rel) = (g("imageBalance") / 50'f32, g("graininess") / 20'f32, g("relief") / 25'f32)
    var l = win.lum
    blur1(l, w, h, 1'f32)
    var m = newSeq[float32](l.len)
    for i, v in l: m[i] = smoothstep(bal - 0.05'f32, bal + 0.05'f32, v)
    blur1(m, w, h, 1.2'f32)
    let (gx, gy) = sobel(m, w, h)
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let n = (hash01(x, y, 0, 161) - 0.5'f32) * gr * 0.3'f32
      let f = clamp(shade(gx[i], gy[i], lightDir("topLeft"), rel * 6'f32), 0'f32, 2'f32)
      win.c[i] = mapPx(mix(fg, bg, m[i]), (it + n) * f)
  of gfPhotocopy:
    let (det, dk) = (g("detail"), g("darkness") / 50'f32)
    let l = win.lum
    var b = l
    blur1(b, w, h, 1'f32 + det * 0.5'f32)
    for i in 0 ..< win.c.len:
      let hp = l[i] - b[i]
      let ink = smoothstep(0'f32, 0.06'f32, -hp * (1'f32 + dk * 8'f32)) + smoothstep(0.35'f32, 0.05'f32, l[i]) * dk * 1.5'f32
      win.c[i] = mix(bg, fg, clamp01(ink))
  of gfPlaster:
    let bal = g("imageBalance") / 50'f32
    var l = win.lum
    blur1(l, w, h, 0.5'f32 + g("smoothness") * 0.8'f32)
    var m = newSeq[float32](l.len)
    for i, v in l: m[i] = smoothstep(bal + 0.1'f32, bal - 0.1'f32, v)
    blur1(m, w, h, 1.5'f32)
    let (gx, gy) = sobel(m, w, h)
    let lt = lightDir(e.choice("light"))
    for i in 0 ..< win.c.len:
      let f = clamp(shade(gx[i], gy[i], lt, 6'f32), 0'f32, 2'f32)
      win.c[i] = mapPx(mix(bg, fg, m[i] * 0.6'f32), it * f)
  of gfReticulation:
    let (den, fl, bl) = (g("density") / 50'f32, g("foregroundLevel") / 50'f32, g("backgroundLevel") / 50'f32)
    let inkC = mix(bg, fg, 0.3'f32 + fl * 0.7'f32)
    let paper = mix(bg, fg, bl * 0.6'f32)
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let n = 0.5'f32 * hash01(x, y, 0, 171) + 0.5'f32 * vnoise(float32(x) / 1.8'f32, float32(y) / 1.8'f32, 172)
      let t = (1'f32 - lum(win.c[i])) + (n - 0.5'f32) * (0.4'f32 + den * 1.2'f32)
      win.c[i] = mix(paper, inkC, smoothstep(0.45'f32, 0.55'f32, t))
  of gfStamp:
    let bal = g("lightDarkBalance") / 50'f32
    var l = win.lum
    blur1(l, w, h, 0.3'f32 + g("smoothness") * 0.3'f32)
    for i in 0 ..< win.c.len:
      win.c[i] = mix(bg, fg, smoothstep(bal + 0.02'f32, bal - 0.02'f32, l[i]))
  of gfTornEdges:
    let (bal, sm, con) = (g("imageBalance") / 50'f32, g("smoothness"), g("contrast"))
    var l = win.lum
    blur1(l, w, h, 1'f32)
    let rough = (16'f32 - sm) / 15'f32 * 0.35'f32
    let aa = 0.25'f32 / con
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let t = l[i] + (fbm(float32(x) / 2'f32, float32(y) / 2'f32, 181) - 0.5'f32) * rough
      win.c[i] = mix(bg, fg, smoothstep(bal + aa, bal - aa, t))
  of gfWaterPaper:
    let fl = g("fiberLength")
    let (br, co) = (g("brightness") / 100'f32, g("contrast") / 100'f32)
    let r = rad(fl / 6'f32)
    lineBlur3(win.c, w, h, (1, 0), r)
    lineBlur3(win.c, w, h, (0, 1), r)
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let fib = rmax(strokes(x, y, (1, 0), fl, 1'f32, 191), strokes(x, y, (0, 1), fl, 1'f32, 192))
      win.c[i] = mapPx(win.c[i], (it * (0.85'f32 + 0.25'f32 * fib) - 0.5'f32) * (co * 1.6'f32 + 0.2'f32) + 0.5'f32 +
                                 (br - 0.5'f32) * 0.8'f32)
  # ---------------- Stylize ----------------
  of gfGlowingEdges:
    blur3(win.c, w, h, 0.3'f32 + g("smoothness") * 0.25'f32)
    var e3 = newSeq[Px](win.c.len)
    for c in 0 .. 2:
      var plane = newSeq[float32](win.c.len)
      for i, p in win.c: plane[i] = p[c]
      let gm = gradMag(plane, w, h)
      for i in 0 ..< e3.len: e3[i][c] = gm[i]
    blur3(e3, w, h, g("edgeWidth") * 0.4'f32)
    let k = 2'f32 + g("edgeBrightness") * 1.2'f32
    for i in 0 ..< win.c.len: win.c[i] = mapPx(e3[i], clamp01(it * k))
  # ---------------- Texture ----------------
  of gfCraquelure:
    let (sp, dep, bri) = (g("crackSpacing"), g("crackDepth") / 10'f32, g("crackBrightness") / 10'f32)
    let cell = sp * 1.3'f32 + 1'f32
    let width = 0.8'f32 + dep * 0.8'f32
    proc crack(x, y: float32): float32 =
      let (d1, d2, _) = voronoi(x, y, cell, 201)
      1'f32 - smoothstep(0'f32, width, d2 - d1)
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let (fx, fy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
      let c0 = crack(fx, fy)
      let (gx, gy) = (crack(fx + 1'f32, fy) - c0, crack(fx, fy + 1'f32) - c0)
      let f = clamp(shade(-gx, -gy, lightDir("topLeft"), dep * 2'f32), 0'f32, 2'f32)
      win.c[i] = mapPx(win.c[i], it * (0.55'f32 + bri * 0.5'f32) * (1'f32 - c0 * dep * 0.6'f32) * f)
  of gfGrain:
    let (it0, co) = (g("intensity") / 100'f32, g("contrast") / 100'f32)
    let ty = e.choice("grainType")
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      template hc(c: uint32): float32 = hash01(x, y, c, 211) - 0.5'f32
      let p = win.c[i]
      let q: Px = (case ty
        of "soft": [p[0] + hc(0) * it0 * 0.4'f32, p[1] + hc(1) * it0 * 0.4'f32, p[2] + hc(2) * it0 * 0.4'f32]
        of "sprinkles": (if hash01(x, y, 3, 211) < it0 * 0.15'f32: bg else: p)
        of "clumped":
          let nz = vnoise(float32(x) / 2'f32, float32(y) / 2'f32, 212)
          mapPx(p, it + (nz - 0.5'f32) * it0 * 1.2'f32)
        of "contrasty":
          let hv = hc(4)
          mapPx(p, (it - 0.5'f32) * (1'f32 + it0 * 2'f32) + 0.5'f32 + hv * it0 * 0.5'f32)
        of "enlarged":
          let (fx, fy) = (float32(x) / 3'f32, float32(y) / 3'f32)
          [p[0] + (vnoise(fx, fy, 213) - 0.5'f32) * it0, p[1] + (vnoise(fx, fy, 214) - 0.5'f32) * it0,
           p[2] + (vnoise(fx, fy, 215) - 0.5'f32) * it0]
        of "stippled":
          let st = (if lum(p) > hash01(x, y, 5, 211): bg else: fg)
          mix(p, st, 0.3'f32 + it0 * 0.7'f32)
        of "horizontal":
          let nz = vnoise(float32(x) / 25'f32, float32(y), 216)
          mapPx(p, it + (nz - 0.5'f32) * it0)
        of "vertical":
          let nz = vnoise(float32(x), float32(y) / 25'f32, 217)
          mapPx(p, it + (nz - 0.5'f32) * it0)
        of "speckle": (if hash01(x, y, 6, 211) < it0 * 0.1'f32: mix(p, fg, 0.85'f32) else: p)
        else: [p[0] + hc(0) * it0 * 0.8'f32, p[1] + hc(1) * it0 * 0.8'f32, p[2] + hc(2) * it0 * 0.8'f32])
      win.c[i] = mapPx(q, (it - 0.5'f32) * (0.5'f32 + co) + 0.5'f32)
  of gfMosaicTiles:
    let (ts, gw, lg) = (g("tileSize"), g("groutWidth"), g("lightenGrout") / 10'f32)
    proc edge(x, y: float32): float32 =
      let jx = x + (vnoise(x / ts * 0.7'f32, y / ts * 0.7'f32, 221) - 0.5'f32) * ts * 0.3'f32
      let jy = y + (vnoise(x / ts * 0.7'f32, y / ts * 0.7'f32, 222) - 0.5'f32) * ts * 0.3'f32
      let (u, v) = (remEuclidF(jx, ts), remEuclidF(jy, ts))
      rmin(rmin(u, ts - u), rmin(v, ts - v))
    proc hgt(xx, yy: float32): float32 = smoothstep(gw * 0.5'f32, gw * 0.5'f32 + 2.5'f32, edge(xx, yy))
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let (fx, fy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
      let de = edge(fx, fy)
      let grout = smoothstep(gw * 0.5'f32 + 0.7'f32, gw * 0.5'f32 - 0.3'f32, de)
      let s = (hgt(fx - 1'f32, fy - 1'f32) - hgt(fx + 1'f32, fy + 1'f32)) * 0.35'f32
      let l = lum(win.c[i])
      let gc = grey(clamp01(l * 0.5'f32 + lg * 0.45'f32))
      win.c[i] = mix(mapPx(win.c[i], it * (1'f32 + s)), gc, grout)
  of gfPatchwork:
    let size = patchSize(e)
    let rel = g("relief") / 25'f32
    let src = Win(r: win.r, w: w, h: h, c: win.c)
    proc hv(x, y: float32): float32 =
      let (bx, by) = (i32(floor(x / size)), i32(floor(y / size)))
      let (u, v) = (remEuclidF(x, size), remEuclidF(y, size))
      let bevel = smoothstep(0'f32, 1.5'f32, rmin(rmin(u, size - u), rmin(v, size - v)))
      hash01(bx, by, 0'u32, 231'u32) * bevel
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let (fx, fy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
      let (bx, by) = (floor(fx / size), floor(fy / size))
      let c = src.at(ai((bx + 0.5'f32) * size), ai((by + 0.5'f32) * size))
      let s = (hv(fx - 1'f32, fy - 1'f32) - hv(fx + 1'f32, fy + 1'f32)) * rel * 1.5'f32
      win.c[i] = mapPx(c, it * (1'f32 + s))
  of gfStainedGlass:
    let cell = glassCell(e)
    let bt = g("borderThickness")
    let li = g("lightIntensity") / 10'f32
    let b = ctx.bounds
    let (cx, cy) = (float32(b.x0 + b.x1) / 2'f32, float32(b.y0 + b.y1) / 2'f32)
    let half = rmax(float32(max(b.width, b.height)) / 2'f32, 1'f32)
    let src = Win(r: win.r, w: w, h: h, c: win.c)
    for i in 0 ..< win.c.len:
      let (x, y) = win.xy(i)
      let (fx, fy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
      let (d1, d2, p) = voronoi(fx, fy, cell, 241)
      let border = smoothstep(bt * 0.5'f32 + 0.6'f32, bt * 0.5'f32 - 0.4'f32, (d2 - d1) * 0.5'f32)
      let dn = rmin(sqrt(sq(fx - cx) + sq(fy - cy)) / half, 1'f32)
      let col = mapPx(src.at(ai(p[0]), ai(p[1])), it * (1'f32 + li * 0.6'f32 * (1'f32 - dn)))
      win.c[i] = mix(col, fg, border)
  of gfTexturizer:
    texturize(win, e.choice("texture"), g("scaling"), g("relief"), e.choice("light"), e.flag("invert"))

proc run*(effects: openArray[GalleryEffect]; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  ## Runs a stack of effects over `o` (the source covers `o` grown by `reach`).
  let n = src.ch
  let r = src.rect
  let (w, h) = (r.width, r.height)
  if effects.len == 0 or w == 0 or h == 0 or n == 0 or src.data.len < w * h * n:
    return src.crop(o)
  var win = Win(r: r, w: w, h: h, c: newSeq[Px](w * h))
  for i in 0 ..< w * h:
    win.c[i] = rgb(rgba(ctx, src.data.toOpenArray(i * n, i * n + n - 1)))
  for e in effects: apply(e, win, ctx)
  result = src.crop(o)
  let ow = max(o.width, 1)
  for i in 0 ..< result.len div n:
    let (x, y) = (o.x0 + i mod ow, o.y0 + i div ow)
    let a = (if ctx.alpha: result[i * n + n - 1] else: 1'f32)
    let c = win.at(x, y)
    setRgba(ctx, result.toOpenArray(i * n, i * n + n - 1), [clamp01(c[0]), clamp01(c[1]), clamp01(c[2]), a])
    if ctx.alpha: result[i * n + n - 1] = a
