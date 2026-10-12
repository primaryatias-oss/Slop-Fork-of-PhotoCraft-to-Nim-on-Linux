## Pattern tiles for fill layers and effects: premultiplied RGBA tiles sampled bilinearly with
## wrap-around, placed by a link anchor, phase, scale and angle.

import std/[math, tables]
import ../doc, ./buffer

type
  PatternTile* = ref object
    w*, h*: int
    px: seq[Px]   ## premultiplied
  Placement* = object
    origin*: (float64, float64)
    cs*: (float64, float64)
    invScale*: float64
  PreparedPatterns* = object
    patterns*: seq[Pattern]
    mode*: ColorMode
    cache: Table[int, PatternTile]

proc patternRgba*(p: Pattern; mode: ColorMode): seq[Px] =
  result = newSeq[Px](p.width * p.height)
  if result.len > 0: p.surface.readRgbaInto(p.rect, result)
  if mode in {cmGrayscale, cmBitmap, cmDuotone, cmCmyk} and p.surface.format.mode != mode:
    let fmt = PixelFormat(mode: mode, sample: stF32, alpha: true)
    var v: array[5, float32]
    for q in result.mitems:
      let n = fromRgbaInto(fmt, q, v)
      q = toRgba(fmt, v.toOpenArray(0, n - 1))

proc newPatternTile*(p: Pattern; mode = cmRgb): PatternTile =
  if p.isEmpty: return nil
  result = PatternTile(w: p.width, h: p.height, px: patternRgba(p, mode))
  for q in result.px.mitems:
    for c in 0..2: q[c] *= q[3]

proc at(t: PatternTile; x, y: int64): Px {.inline.} =
  let xi = int(((x mod t.w) + t.w) mod t.w)
  let yi = int(((y mod t.h) + t.h) mod t.h)
  t.px[yi * t.w + xi]

proc sample*(t: PatternTile; u0, v0: float64): Px =
  let (u, v) = (u0 - 0.5, v0 - 0.5)
  let (x0, y0) = (floor(u), floor(v))
  let (fx, fy) = (float32(u - x0), float32(v - y0))
  var acc: Px
  for (dy, wy) in [(0'i64, 1 - fy), (1'i64, fy)]:
    if wy == 0: continue
    for (dx, wx) in [(0'i64, 1 - fx), (1'i64, fx)]:
      if wx == 0: continue
      let p = t.at(int64(x0) + dx, int64(y0) + dy)
      let k = wx * wy
      for c in 0..3: acc[c] += p[c] * k
  if acc[3] <= 0: return
  [acc[0] / acc[3], acc[1] / acc[3], acc[2] / acc[3], min(acc[3], 1'f32)]

proc anchored*(anchor: (float64, float64); link: bool; phase: (float32, float32); scale, angle: float32): Placement =
  let base = if link: anchor else: (0.0, 0.0)
  let s = float64(scale)
  let a = degToRad(float64(angle))
  Placement(origin: (base[0] + float64(phase[0]), base[1] + float64(phase[1])), cs: (cos(a), sin(a)),
            invScale: if s == s and s != Inf and s > 1e-3: 1 / s else: 1.0)

proc placement*(frame: Rect; link: bool; phase: (float32, float32); scale, angle: float32): Placement =
  let anchor = if frame.isEmpty: (0.0, 0.0) else: (float64(frame.x0), float64(frame.y0))
  anchored(anchor, link, phase, scale, angle)

proc map*(p: Placement; x, y: float64): (float64, float64) {.inline.} =
  let (dx, dy) = (x - p.origin[0], y - p.origin[1])
  let (c, s) = p.cs
  ((dx * c - dy * s) * p.invScale, (dx * s + dy * c) * p.invScale)

proc renderPattern*(tile: PatternTile; place: Placement; rc: Rect): seq[Px] =
  result = newSeqOfCap[Px](max(rc.width * rc.height, 0))
  for y in rc.y0 ..< rc.y1:
    for x in rc.x0 ..< rc.x1:
      let (u, v) = place.map(float64(x) + 0.5, float64(y) + 0.5)
      result.add tile.sample(u, v)

proc newPreparedPatterns*(patterns: seq[Pattern]; mode = cmRgb): PreparedPatterns =
  PreparedPatterns(patterns: patterns, mode: mode)

proc get*(pp: var PreparedPatterns; id, name: string): PatternTile =
  ## The pattern's tile (cached for this render), nil when missing.
  let i = findPattern(pp.patterns, id, name)
  if i < 0: return nil
  if i notin pp.cache: pp.cache[i] = newPatternTile(pp.patterns[i], pp.mode)
  pp.cache[i]
