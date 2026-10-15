## Render: Fibers, Lens Flare, Lighting Effects (port of `render.rs`).

import std/math
import ../geom, ../color
import params, image, fxutil, noise, util

proc valueNoise(x, y: float32; seed: uint32): float32 =
  ## Smooth value noise in [0, 1).
  let (ix, iy) = (asI32(float64(floor(x))), asI32(float64(floor(y))))
  let (tx, ty) = (x - float32(ix), y - float32(iy))
  let (sx, sy) = (tx * tx * (3 - 2 * tx), ty * ty * (3 - 2 * ty))
  template h(a, b: int): float32 = hash01(a, b, 0, seed)
  let top = h(ix, iy) + (h(ix + 1, iy) - h(ix, iy)) * sx
  let bot = h(ix, iy + 1) + (h(ix + 1, iy + 1) - h(ix, iy + 1)) * sx
  top + (bot - top) * sy

proc fibers*(src: Image; o: Rect; ctx: Ctx; variance, strength: float32; seed: uint32;
             colours: (array[4, float32], array[4, float32])): seq[float32] =
  ## Fibers: vertical fibrous streaks between the foreground and background colours. Variance
  ## sets how quickly the colour varies across fibres (and how short they are); strength
  ## stretches each fibre vertically.
  let n = src.ch
  let va = clamp(variance, 1'f32, 64'f32)
  let st = clamp(strength, 1'f32, 64'f32)
  let (fg, bg) = colours
  # Horizontal frequency rises with variance; vertical frequency falls with strength.
  let fx = 0.08'f32 + va / 64 * 0.6'f32
  let fy = fx / (2 + st * 1.5'f32) * (0.5'f32 + va / 32)
  result = src.crop(o)
  var i = 0
  while i * n + n <= result.len:
    let (x, y) = xy(o, i)
    let (xf, yf) = (float32(x), float32(y))
    var sum = 0'f32
    var amp = 1'f32
    var norm = 0'f32
    var (ax, ay) = (fx, fy)
    for oc in 0'u32 ..< 4'u32:
      sum += valueNoise(xf * ax, yf * ay, seed + oc * 977) * amp
      norm += amp
      amp *= 0.55'f32
      ax *= 2
      ay *= 2
    # Fine per-column grain gives the hairline texture.
    let grain = valueNoise(xf * 0.9'f32, yf * fy * 0.25'f32, seed xor 0x5bd1'u32) - 0.5'f32
    let t = clamp((sum / norm - 0.5'f32) * (1.6'f32 + va / 40) + 0.5'f32 + grain * 0.25'f32, 0'f32, 1'f32)
    let c = [fg[0] + (bg[0] - fg[0]) * t, fg[1] + (bg[1] - fg[1]) * t, fg[2] + (bg[2] - fg[2]) * t, 1'f32]
    setRgba(ctx, result.toOpenArray(i * n, i * n + n - 1), c)
    inc i

type Ghost = object
  ## One lens-flare ghost: position along the flare→centre axis, radius and tint.
  t, r: float32
  tint: array[3, float32]
  k: float32

proc lensFlare*(src: Image; o: Rect; ctx: Ctx; brightness: float32; centre: (float32, float32); lens: LensType): seq[float32] =
  ## Lens Flare: an additive (screen) glow, rays, halo ring and ghost discs along the line through
  ## the image centre. Lens types vary the elements.
  let n = src.ch
  let b = ctx.bounds
  let (bw, bh) = (float32(b.width), float32(b.height))
  let diag = rmax(hypot(bw, bh), 1)
  let (cx, cy) = (float32(b.x0) + centre[0] * bw, float32(b.y0) + centre[1] * bh)
  let (mx, my) = (float32(b.x0) + bw / 2, float32(b.y0) + bh / 2)
  let bright = clamp(brightness, 10'f32, 300'f32) / 100
  let s = diag / 1000
  let (core, glow, ringR, ringW, rays, streak) = case lens
    of ltZoom: (14'f32, 90'f32, 0'f32, 0'f32, 0.5'f32, 0'f32)
    of ltPrime35: (12'f32, 70'f32, 90'f32, 6'f32, 0.3'f32, 0'f32)
    of ltPrime105: (22'f32, 130'f32, 0'f32, 0'f32, 0.8'f32, 0'f32)
    of ltMoviePrime: (10'f32, 60'f32, 0'f32, 0'f32, 0.2'f32, 1'f32)
  let ghosts = case lens
    of ltZoom: @[Ghost(t: 0.35, r: 10, tint: [0.6'f32, 0.8, 1.0], k: 0.18), Ghost(t: 0.6, r: 26, tint: [0.5'f32, 1.0, 0.6], k: 0.10),
                 Ghost(t: 0.85, r: 6, tint: [1.0'f32, 0.7, 0.4], k: 0.25), Ghost(t: 1.25, r: 40, tint: [0.7'f32, 0.5, 1.0], k: 0.08),
                 Ghost(t: 1.5, r: 16, tint: [1.0'f32, 0.6, 0.5], k: 0.15), Ghost(t: 1.9, r: 70, tint: [0.5'f32, 0.7, 1.0], k: 0.06)]
    of ltPrime35: @[Ghost(t: 0.7, r: 18, tint: [0.6'f32, 1.0, 0.7], k: 0.12), Ghost(t: 1.4, r: 30, tint: [1.0'f32, 0.7, 0.5], k: 0.09)]
    of ltPrime105: @[Ghost(t: 0.5, r: 14, tint: [1.0'f32, 0.8, 0.5], k: 0.12), Ghost(t: 1.6, r: 50, tint: [0.6'f32, 0.7, 1.0], k: 0.07)]
    of ltMoviePrime: @[Ghost(t: 0.4, r: 8, tint: [0.5'f32, 0.7, 1.0], k: 0.2), Ghost(t: 1.0, r: 20, tint: [0.4'f32, 0.6, 1.0], k: 0.12),
                       Ghost(t: 1.6, r: 12, tint: [0.5'f32, 0.8, 1.0], k: 0.16)]
  result = src.crop(o)
  var i = 0
  while i * n + n <= result.len:
    let (x, y) = xy(o, i)
    let (fx, fy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
    let (dx, dy) = (fx - cx, fy - cy)
    let d = sqrt(dx * dx + dy * dy)
    var f: array[3, float32]
    # Hot core and wide glow (warm white).
    let q = d / (core * s)
    let g = exp(-(q * q)) + 0.35'f32 * exp(-d / (glow * s))
    for k, w in [1'f32, 0.95, 0.85]: f[k] += g * w
    # Rays: angular modulation decaying with distance.
    if rays > 0:
      let th = arctan2(dy, dx)
      let m = powi(sin(th * 12) * 0.5'f32 + 0.5'f32, 8) + powi(sin(th * 7 + 1.3'f32) * 0.5'f32 + 0.5'f32, 12) * 0.6'f32
      let rr = rays * m * exp(-d / (glow * 1.6'f32 * s))
      for v in f.mitems: v += rr * 0.6'f32
    # Anamorphic horizontal streak (movie prime).
    if streak > 0:
      let st = exp(-(abs(dy) / (2.5'f32 * s))) * exp(-(abs(dx) / (450 * s)))
      f[0] += st * 0.5'f32
      f[1] += st * 0.7'f32
      f[2] += st
    # Halo ring with a slight chromatic spread.
    if ringR > 0:
      for k, off in [1.04'f32, 1.0, 0.96]:
        let rd = (d - ringR * s * off) / (ringW * s)
        f[k] += 0.18'f32 * exp(-rd * rd)
    # Ghosts along the axis through the centre.
    for gh in ghosts:
      let (gx, gy) = (cx + (mx - cx) * gh.t * 2, cy + (my - cy) * gh.t * 2)
      let gd = sqrt((fx - gx) * (fx - gx) + (fy - gy) * (fy - gy)) / (gh.r * s)
      if gd < 1.2'f32:
        # Soft-edged disc, slightly brighter at the rim.
        let e = smoothstep(1, 0.85'f32, gd) * (0.6'f32 + 0.4'f32 * gd)
        for k in 0 ..< 3: f[k] += e * gh.k * gh.tint[k]
    var c = rgba(ctx, result.toOpenArray(i * n, i * n + n - 1))
    for k in 0 ..< 3:
      let a = rmax(f[k] * bright, 0)
      # Screen keeps highlights below white for display-referred values.
      c[k] = 1 - (1 - c[k]) * (1 - rmin(a, 1))
    setRgba(ctx, result.toOpenArray(i * n, i * n + n - 1), c)
    inc i

type LightingSpec* = object
  lights*: seq[Light]
  gloss*, metallic*, exposure*, ambience*: float32
  texture*: TextureChannel
  height*: float32
  whiteIsHigh*: bool

proc norm3(v: array[3, float32]): array[3, float32] =
  let m = rmax(sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]), 1e-9'f32)
  [v[0] / m, v[1] / m, v[2] / m]
proc dot3(a, b: array[3, float32]): float32 = a[0] * b[0] + a[1] * b[1] + a[2] * b[2]

proc lighting*(src: Image; o: Rect; ctx: Ctx; spec: LightingSpec): seq[float32] =
  ## Lighting Effects: Blinn–Phong shading of the image as a surface (bumped by a texture
  ## channel) under spot, point and infinite lights.
  let n = src.ch
  let b = ctx.bounds
  let (bw, bh) = (float32(b.width), float32(b.height))
  let short = rmax(min(bw, bh), 1)
  let hscale = clamp(spec.height, 0'f32, 100'f32) / 100 * 0.06'f32 * short
  let gl = clamp(spec.gloss, -100'f32, 100'f32)
  let shininess = pow(2'f32, 1 + (gl + 100) / 200 * 7)
  let specK = (gl + 100) / 200 * 0.7'f32
  let metal = (clamp(spec.metallic, -100'f32, 100'f32) + 100) / 200
  let expo = pow(2'f32, clamp(spec.exposure, -100'f32, 100'f32) / 50)
  let ambient = clamp(spec.ambience, -100'f32, 100'f32) / 100 * 0.5'f32
  var tmp: array[MaxC, float32]
  template tex(tx, ty: int): float32 =
    (block:
      let xx = clamp(tx, b.x0, b.x1 - 1)
      let yy = clamp(ty, b.y0, b.y1 - 1)
      for c in 0 ..< n: tmp[c] = src.get(xx, yy, c)
      let v = case spec.texture
        of tcNone: 0'f32
        of tcAlpha: (if ctx.alpha: tmp[n - 1] else: 1'f32)
        of tcLuminance: luma(ctx, tmp.toOpenArray(0, n - 1))
        of tcRed, tcGreen, tcBlue:
          let k = (if spec.texture == tcRed: 0 elif spec.texture == tcGreen: 1 else: 2)
          (if ctx.mode == cmRgb: tmp[k] else: rgba(ctx, tmp.toOpenArray(0, n - 1))[k])
      if spec.whiteIsHigh: v else: 1 - v)
  result = src.crop(o)
  var i = 0
  while i * n + n <= result.len:
    let (x, y) = xy(o, i)
    let (fx, fy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
    let nrm =
      if spec.texture == tcNone or hscale <= 0: [0'f32, 0, 1]
      else:
        let hx = (tex(x + 1, y) - tex(x - 1, y)) * 0.5'f32 * hscale
        let hy = (tex(x, y + 1) - tex(x, y - 1)) * 0.5'f32 * hscale
        norm3([-hx, -hy, 1'f32])
    var c = rgba(ctx, result.toOpenArray(i * n, i * n + n - 1))
    let base = [c[0], c[1], c[2]]
    var lit = [ambient, ambient, ambient]
    var spc: array[3, float32]
    for l in spec.lights:
      let lpos = [float32(b.x0) + l.x * bw, float32(b.y0) + l.y * bh, l.z * short]
      var ldir: array[3, float32]
      var atten: float32
      case l.kind
      of lkInfinite:
        let ar = degToRad(l.angle)
        let (s, co) = (sin(ar), cos(ar))
        let e = degToRad(clamp(l.elevation, 0'f32, 90'f32))
        ldir = norm3([co * cos(e), -s * cos(e), sin(e)])
        atten = 1
      of lkPoint:
        let v = [lpos[0] - fx, lpos[1] - fy, lpos[2]]
        let dist = sqrt(v[0] * v[0] + v[1] * v[1])
        let rch = rmax(l.radius * short, 1)
        ldir = norm3(v)
        let q = clamp(1 - dist / rch, 0'f32, 1'f32)
        atten = q * q
      of lkSpot:
        let v = [lpos[0] - fx, lpos[1] - fy, lpos[2]]
        let ld = norm3(v)
        let tgt = [float32(b.x0) + l.targetX * bw, float32(b.y0) + l.targetY * bh, 0'f32]
        let axis = norm3([tgt[0] - lpos[0], tgt[1] - lpos[1], tgt[2] - lpos[2]])
        let cosang = -dot3(ld, axis)
        let cone = clamp(l.cone, 1'f32, 89'f32)
        let outer = cos(degToRad(cone))
        let inner = cos(degToRad(cone * (clamp(l.hotspot, 0'f32, 100'f32) / 100)))
        ldir = ld
        atten = smoothstep(outer, rmax(inner, outer + 1e-4'f32), cosang)
      if atten <= 0: continue
      let inten = clamp(l.intensity, -100'f32, 100'f32) / 50 * atten
      let ndl = rmax(dot3(nrm, ldir), 0)
      let hvv = norm3([ldir[0], ldir[1], ldir[2] + 1])
      let s = pow(rmax(dot3(nrm, hvv), 0), shininess) * specK
      for k in 0 ..< 3:
        lit[k] += ndl * inten * l.color[k]
        # Metallic highlights take the surface colour; plastic ones the light's.
        let sc = l.color[k] * (1 - metal) + base[k] * metal
        spc[k] += s * rmax(inten, 0) * sc
    for k in 0 ..< 3: c[k] = rmax((base[k] * lit[k] + spc[k]) * expo, 0)
    setRgba(ctx, result.toOpenArray(i * n, i * n + n - 1), c)
    inc i
