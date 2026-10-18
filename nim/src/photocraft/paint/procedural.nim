## Procedural (generated, deterministic) texture patterns and sampled tips for the built-in presets
## (port of `paint/src/procedural.rs`). Everything here is original: seeded value noise and simple
## geometry, no bundled brush files.

import std/math
import ../geom, ../rmath
import ./brush, ./rng, ./tile

proc smooth(t: float32): float32 {.inline.} = t * t * (3'f32 - 2'f32 * t)

proc sq(v: float32): float32 {.inline.} = v * v

proc valueNoise(x, y: float32; period: int; salt: uint64): float32 =
  ## Tileable value noise with `period` lattice cells over the tile.
  let (x0, y0) = (floor(x), floor(y))
  let (tx, ty) = (smooth(x - x0), smooth(y - y0))
  let (xi, yi) = (asI32(float64(x0)), asI32(float64(y0)))
  template h(i, j: int): float32 = hash2(remEuclid(i, period), remEuclid(j, period), salt)
  let a = h(xi, yi) + (h(xi + 1, yi) - h(xi, yi)) * tx
  let b = h(xi, yi + 1) + (h(xi + 1, yi + 1) - h(xi, yi + 1)) * tx
  a + (b - a) * ty

proc fbm(x, y, size: float32; cells: int; octaves: uint32; salt: uint64): float32 =
  ## Tileable fractal noise in 0..1 (`cells` lattice cells across at the base octave).
  var (sum, amp, norm, c) = (0'f32, 1'f32, 0'f32, cells)
  for o in 0'u32 ..< octaves:
    sum += valueNoise(x / size * float32(c), y / size * float32(c), c, salt + uint64(o) * 7919) * amp
    norm += amp
    amp *= 0.5'f32
    c *= 2
  sum / norm

proc pattern*(style: PatternStyle; size, seed: uint32): PatternImage =
  ## Generate a seamless pattern tile.
  let n = int(clamp(size, 8'u32, 1024'u32))
  let s = float32(n)
  let salt = uint64(seed) * 0x0100_0000_01B3'u64 + 17
  var data = newSeq[float32](n * n)
  for y in 0 ..< n:
    for x in 0 ..< n:
      let (fx, fy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
      let v =
        case style
        of psNoise: fbm(fx, fy, s, 8, 4, salt)
        of psPaper:
          let base = fbm(fx, fy, s, 16, 4, salt)
          let grain = hash2(x, y, salt xor 0x55)
          clamp((base - 0.5'f32) * 2.2'f32 + 0.5'f32 + (grain - 0.5'f32) * 0.35'f32, 0'f32, 1'f32)
        of psCanvas:
          # Over/under weave: threads every 8 px, alternating which direction is on top.
          let period = 8'f32
          let (cx, cy) = (asI32(float64(floor(fx / period))), asI32(float64(floor(fy / period))))
          let (u, v) = (fract(fx / period), fract(fy / period))
          let warp = sin(float32(PI) * u)
          let weft = sin(float32(PI) * v)
          let top = if (cx + cy) mod 2 == 0: warp else: weft
          clamp(0.25'f32 + 0.65'f32 * top + (hash2(x, y, salt) - 0.5'f32) * 0.15'f32, 0'f32, 1'f32)
        of psDots:
          let period = 8'f32
          let (u, v) = (fract(fx / period) - 0.5'f32, fract(fy / period) - 0.5'f32)
          let d = sqrt(u * u + v * v)
          1'f32 - smooth(clamp((0.35'f32 - d) / 0.1'f32 + 0.5'f32, 0'f32, 1'f32))
      data[y * n + x] = v
  PatternImage(width: n, height: n, data: data)

const
  Pi32 = float32(PI)
  Tau32 = float32(TAU)

proc chalkTip*(n, seed: uint32): GrayTile =
  ## A chalk/charcoal tip: a disc with a ragged edge and grainy interior.
  let salt = uint64(seed) + 101
  let c = float32(n) / 2'f32
  grayTileFromFn(n, n, proc(x, y: uint32): float32 =
    let (dx, dy) = (float32(x) + 0.5'f32 - c, float32(y) + 0.5'f32 - c)
    let r = sqrt(dx * dx + dy * dy) / c
    let ang = arctan2(dy, dx)
    let edge = 0.8'f32 + 0.15'f32 * valueNoise((ang + Pi32) * 3'f32, 0.5'f32, 19, salt)
    let inside = clamp((edge - r) * 8'f32, 0'f32, 1'f32)
    let grain = hash2(int(x), int(y), salt)
    inside * (if grain < 0.2'f32: 0.2'f32 else: 0.7'f32 + 0.3'f32 * grain))

proc spatterTip*(n, seed, drops: uint32): GrayTile =
  ## A spatter tip: a cluster of round droplets of different sizes.
  let salt = uint64(seed) + 202
  let s = float32(n)
  var blobs: seq[(float32, float32, float32)]
  for i in 0'u32 ..< drops:
    let a = hash2(int(i), 0, salt) * Tau32
    let d = sqrt(hash2(int(i), 1, salt)) * 0.38'f32 * s
    let r = (0.03'f32 + 0.09'f32 * sq(hash2(int(i), 2, salt))) * s
    blobs.add (s / 2'f32 + cos(a) * d, s / 2'f32 + sin(a) * d, r)
  grayTileFromFn(n, n, proc(x, y: uint32): float32 =
    let (px, py) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
    var acc = 0'f32
    for (bx, by, r) in blobs:
      let d = sqrt(sq(px - bx) + sq(py - by))
      acc = rmax(acc, clamp(r + 0.5'f32 - d, 0'f32, 1'f32))
    acc)

proc bristleTip*(n, seed: uint32): GrayTile =
  ## A bristle tip: parallel hair streaks of varying density inside an ellipse.
  let salt = uint64(seed) + 303
  let c = float32(n) / 2'f32
  grayTileFromFn(n, n, proc(x, y: uint32): float32 =
    let (dx, dy) = ((float32(x) + 0.5'f32 - c) / c, (float32(y) + 0.5'f32 - c) / c)
    let r = sqrt(dx * dx + sq(dy * 1.8'f32))
    if r > 1: return 0'f32
    let hair = hash2(0, asI32(float64(float32(y) / 1.5'f32)), salt)
    let density = if hair < 0.25'f32: 0'f32 else: 0.5'f32 + 0.5'f32 * hair
    density * (1'f32 - sq(sq(r))))

proc charcoalTip*(n, seed: uint32): GrayTile =
  ## A charcoal tip: a squat ellipse of broken horizontal streaks with a gritty edge.
  let salt = uint64(seed) + 404
  let c = float32(n) / 2'f32
  grayTileFromFn(n, n, proc(x, y: uint32): float32 =
    let (dx, dy) = ((float32(x) + 0.5'f32 - c) / c, (float32(y) + 0.5'f32 - c) / c)
    let r = sqrt(dx * dx + sq(dy * 1.6'f32))
    let edge = 0.85'f32 + 0.15'f32 * valueNoise(dx * 6'f32 + 9'f32, dy * 6'f32 + 9'f32, 64, salt)
    if r > edge: return 0'f32
    # Streaks: per-row density modulated along x so they break up.
    let row = hash2(0, int(y), salt)
    let along = valueNoise(float32(x) / 6'f32, float32(y) / 1.5'f32, 1 shl 12, salt xor 0x77)
    let grit = hash2(int(x), int(y), salt xor 0x13)
    let v = (0.35'f32 + 0.65'f32 * row) * (0.4'f32 + 0.6'f32 * along) * (if grit < 0.15'f32: 0.2'f32 else: 1'f32)
    v * clamp((edge - r) * 6'f32, 0'f32, 1'f32))

proc leafTip*(n: uint32): GrayTile =
  ## A leaf: a pointed lens shape with a lighter midrib, stem towards -x.
  let c = float32(n) / 2'f32
  grayTileFromFn(n, n, proc(x, y: uint32): float32 =
    let (u, v) = ((float32(x) + 0.5'f32 - c) / c, (float32(y) + 0.5'f32 - c) / c)
    # Half-width of the blade at u: widest a little behind the middle, pointed at both ends.
    let t = clamp((u + 1'f32) / 2'f32, 0'f32, 1'f32)
    let half = 0.42'f32 * sin(Pi32 * t) * (1'f32 - 0.25'f32 * t)
    let inside = clamp((half - abs(v)) * c, 0'f32, 1'f32)
    let rib = 1'f32 - 0.45'f32 * (1'f32 - clamp(abs(v) * c / 1.2'f32, 0'f32, 1'f32))
    let veins = 1'f32 - 0.2'f32 * (1'f32 - clamp(abs(fract(u * 7'f32 - abs(v) * 5'f32) - 0.5'f32) * 8'f32, 0'f32, 1'f32))
    let stem = if u < -0.75'f32 and abs(v) * c < 0.9'f32: 1'f32 else: 0'f32
    rmax(inside * rib * veins, stem))

proc grassTip*(n, seed, blades: uint32): GrayTile =
  ## Grass: a tuft of tapered, slightly curved blades rising from the bottom edge.
  let salt = uint64(seed) + 505
  let s = float32(n)
  var bl: seq[(float32, float32, float32, float32)]
  for i in 0'u32 ..< blades:
    let base = (0.3'f32 + 0.4'f32 * hash2(int(i), 0, salt)) * s
    let height = (0.55'f32 + 0.42'f32 * hash2(int(i), 1, salt)) * s
    let lean = (hash2(int(i), 2, salt) - 0.5'f32) * 0.9'f32
    let width = (0.025'f32 + 0.025'f32 * hash2(int(i), 3, salt)) * s
    bl.add (base, height, lean, width)
  grayTileFromFn(n, n, proc(x, y: uint32): float32 =
    let (px, py) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
    let up = s - py
    var acc = 0'f32
    for (base, height, lean, width) in bl:
      if up < 0 or up > height: continue
      let t = up / height
      let cx = base + lean * height * t * t
      let w = width * (1'f32 - t) + 0.3'f32
      acc = rmax(acc, clamp(w - abs(px - cx), 0'f32, 1'f32))
    acc)

proc spongeTip*(n, seed: uint32): GrayTile =
  ## A sea sponge: a disc riddled with holes of different sizes.
  let salt = uint64(seed) + 606
  let c = float32(n) / 2'f32
  grayTileFromFn(n, n, proc(x, y: uint32): float32 =
    let (dx, dy) = (float32(x) + 0.5'f32 - c, float32(y) + 0.5'f32 - c)
    let r = sqrt(dx * dx + dy * dy) / c
    let edge = 0.78'f32 + 0.2'f32 * valueNoise((arctan2(dy, dx) + Pi32) * 2.5'f32, 0.5'f32, 16, salt)
    let disc = clamp((edge - r) * 6'f32, 0'f32, 1'f32)
    let pores = fbm(float32(x), float32(y), float32(n), 6, 3, salt)
    disc * clamp((pores - 0.42'f32) * 6'f32, 0'f32, 1'f32))

proc starTip*(n: uint32): GrayTile =
  ## A four-point sparkle with a soft glow.
  let c = float32(n) / 2'f32
  grayTileFromFn(n, n, proc(x, y: uint32): float32 =
    let (u, v) = (abs((float32(x) + 0.5'f32 - c) / c), abs((float32(y) + 0.5'f32 - c) / c))
    proc ray(a, b: float32): float32 =
      rmax((1'f32 - a) * (1'f32 - rmin(b / (0.08'f32 * (1'f32 - a) + 0.005'f32), 1'f32)), 0'f32)
    let glow = sq(rmax(1'f32 - sqrt(u * u + v * v) * 2.5'f32, 0'f32))
    rmin(rmax(rmax(ray(u, v), ray(v, u)), glow), 1'f32))

proc rakeTip*(n, seed, bristles: uint32): GrayTile =
  ## A rake: a row of separate round bristles of slightly different sizes.
  let salt = uint64(seed) + 707
  let s = float32(n)
  let k = max(bristles, 1'u32)
  var dots: seq[(float32, float32, float32)]
  for i in 0'u32 ..< k:
    let x = s * (float32(i) + 0.5'f32) / float32(k)
    let y = s / 2'f32 + (hash2(int(i), 0, salt) - 0.5'f32) * s * 0.08'f32
    let r = s / float32(k) * (0.22'f32 + 0.18'f32 * hash2(int(i), 1, salt))
    dots.add (x, y, r)
  grayTileFromFn(n, n, proc(x, y: uint32): float32 =
    let (px, py) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
    var acc = 0'f32
    for (dx, dy, r) in dots:
      let d = sqrt(sq(px - dx) + sq(py - dy))
      acc = rmax(acc, clamp(r + 0.5'f32 - d, 0'f32, 1'f32) * 0.9'f32)
    acc)
