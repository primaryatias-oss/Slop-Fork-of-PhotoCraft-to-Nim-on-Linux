## Straight painting symmetry axes (port of `paint/src/symmetry.rs`). Geometry is independent of
## pixels and the UI.

import std/[math, options]
import ../rmath
import ./brush

const MaxCoordinate* = 1_000_000.0

type
  SymmetryMode* = enum
    symVertical = "vertical", symHorizontal = "horizontal", symDual = "dual", symDiagonal = "diagonal"

  PresetSymmetry* = object
    mode*: SymmetryMode
    center*: array[2, float64]
    rotation*: float64 ## clockwise degrees relative to the mode's initial direction (y down)

proc id*(m: SymmetryMode): string = $m

proc symmetryFromId*(id: string): Option[SymmetryMode] =
  for m in SymmetryMode:
    if $m == id: return some(m)

proc isFin(v: float64): bool {.inline.} = v == v and v != Inf and v != NegInf

proc newPresetSymmetry*(mode: SymmetryMode; center: array[2, float64]; rotation: float64): Option[PresetSymmetry] =
  if not isFin(rotation): return none(PresetSymmetry)
  for x in center:
    if not isFin(x) or abs(x) > MaxCoordinate: return none(PresetSymmetry)
  some(PresetSymmetry(mode: mode, center: center, rotation: remEuclidF(rotation, 360.0)))

proc directions*(s: PresetSymmetry): seq[array[2, float64]] =
  let initial = case s.mode
    of symHorizontal: 0.0
    of symDiagonal: 45.0
    else: 90.0
  let a = toRadians(initial + s.rotation)
  let first = [cos(a), sin(a)]
  if s.mode == symDual: @[first, [-first[1], first[0]]] else: @[first]

proc mirrorCount*(s: PresetSymmetry): int = (if s.mode == symDual: 3 else: 1)

proc reflectedPasses*(s: PresetSymmetry; points: openArray[StrokePoint]): seq[seq[StrokePoint]] =
  for d in s.directions:
    var copy: seq[StrokePoint]
    for p in points:
      let (dx, dy) = (p.x - s.center[0], p.y - s.center[1])
      let along = dx * d[0] + dy * d[1]
      var q = p
      q.x = s.center[0] + 2 * along * d[0] - dx
      q.y = s.center[1] + 2 * along * d[1] - dy
      copy.add q
    result.add copy
  if s.mode == symDual:
    var copy: seq[StrokePoint]
    for p in points:
      var q = p
      q.x = 2 * s.center[0] - p.x
      q.y = 2 * s.center[1] - p.y
      copy.add q
    result.add copy

proc segments*(s: PresetSymmetry; size: array[2, float64]): seq[array[2, array[2, float64]]] =
  ## Clip infinite axes to document bounds without assuming that their centre is inside.
  for d in s.directions:
    var (lo, hi) = (NegInf, Inf)
    var skip = false
    for k in 0 .. 1:
      let (origin, direction, extent) = (s.center[k], d[k], size[k])
      if abs(direction) < 1e-12:
        if origin < 0 or origin > extent:
          skip = true
          break
      else:
        let (a, b) = (-origin / direction, (extent - origin) / direction)
        lo = rmax(lo, rmin(a, b))
        hi = rmin(hi, rmax(a, b))
    if skip or not isFin(lo) or not isFin(hi) or hi < lo: continue
    result.add [[s.center[0] + lo * d[0], s.center[1] + lo * d[1]], [s.center[0] + hi * d[0], s.center[1] + hi * d[1]]]
