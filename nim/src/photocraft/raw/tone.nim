## The fitted tone curve and camera look types (from `raw/src/look.rs`; the fit itself is in
## `look.nim`, which needs the develop pipeline).

import std/[options, math]
import ./color, ./rutil

type
  ToneCurve* = object
    ## A monotone tone curve on linear values 0..1, piecewise linear between knots in the
    ## square-root domain. Always passes through (0, 0) and (1, 1).
    knots: seq[array[2, float64]] ## `(sqrt x, sqrt y)`, x strictly and y weakly increasing

  CameraLook* = object
    ## A colour rendering fitted to the camera's JPEG: use it through `DevelopOptions.cameraLook`.
    toXyz*: Mat3            ## white-balanced camera RGB → XYZ (D50), white mapping to D50 with Y = 1
    tone*: Option[ToneCurve] ## tone curve applied per channel to linear ProPhoto RGB after exposure
    deltaE*: float64         ## mean CIE76 ΔE against the camera JPEG on the fit grid, with this look
    deltaEFallback*: float64 ## the same with the neutral fallback (no calibration, no tone curve)

const MaxKnots* = 256 ## most knots a curve may have

proc isFin(v: float64): bool {.inline.} = v == v and v != Inf and v != NegInf

proc newToneCurve*(knots: openArray[array[2, float64]]): Option[ToneCurve] =
  ## A curve through linear-light knots `(x, y)`: 2 to `MaxKnots` finite points in 0..1, x
  ## strictly increasing, y not decreasing, starting at (0, 0) and ending at (1, 1).
  if knots.len < 2 or knots.len > MaxKnots: return
  for k in knots:
    for v in k:
      if not (isFin(v) and v >= 0.0 and v <= 1.0): return
  for i in 1 ..< knots.len:
    if not (knots[i][0] > knots[i - 1][0] and knots[i][1] >= knots[i - 1][1]): return
  if not (knots[0][0] == 0.0 and knots[0][1] == 0.0): return
  if not (knots[^1][0] == 1.0 and knots[^1][1] == 1.0): return
  var t = ToneCurve(knots: newSeq[array[2, float64]](knots.len))
  for i, k in knots: t.knots[i] = [sqrt(k[0]), sqrt(k[1])]
  some(t)

proc knots*(t: ToneCurve): seq[array[2, float64]] =
  ## The knots in linear light.
  for k in t.knots: result.add [k[0] * k[0], k[1] * k[1]]

proc `==`*(a, b: ToneCurve): bool = a.knots == b.knots

proc interp(k: openArray[array[2, float64]]; t: float64; frm, to: int): float64 =
  ## Piecewise-linear interpolation of coordinate `to` at `t` on coordinate `frm` (both
  ## increasing; the first segment that reaches `t` wins).
  let i = clamp(partitionPoint(k, it[frm] < t), 1, max(k.len - 1, 1))
  if i - 1 >= 0 and i < k.len:
    let (a, b) = (k[i - 1], k[i])
    let d = b[frm] - a[frm]
    if d <= 0.0: a[to] else: a[to] + (b[to] - a[to]) * clamp((t - a[frm]) / d, 0.0, 1.0)
  else: t

proc eval*(c: ToneCurve; x: float64): float64 =
  ## `f(x)` for linear `x` (clamped to 0..1).
  let t = if x != x: 0.0 else: sqrt(clamp(x, 0.0, 1.0))
  let s = interp(c.knots, t, 0, 1)
  s * s

proc inverseAndSlope*(c: ToneCurve; y: float64): (float64, float64) =
  ## `(x, f'(x))` for `x = f⁻¹(y)`, from one segment lookup: with `sx = sqrt x`, `sy = sqrt y`
  ## and segment slope `k = dsy/dsx`, `f'(x) = k · sy / sx`.
  let t = if y != y: 0.0 else: sqrt(clamp(y, 0.0, 1.0))
  let k = c.knots
  let i = clamp(partitionPoint(k, it[1] < t), 1, max(k.len - 1, 1))
  if not (i - 1 >= 0 and i < k.len): return (y, 1.0)
  let (a, b) = (k[i - 1], k[i])
  let (dx, dy) = (b[0] - a[0], b[1] - a[1])
  if dy <= 0.0 or dx <= 0.0: return (a[0] * a[0], 0.0)
  let sx = a[0] + dx * clamp((t - a[1]) / dy, 0.0, 1.0)
  let slope = dy / dx
  let d = if sx > 1e-9: slope * t / sx else: slope * slope
  (sx * sx, d)

proc inverse*(c: ToneCurve; y: float64): float64 =
  ## `f⁻¹(y)` (the lowest `x` where a flat part repeats `y`).
  c.inverseAndSlope(y)[0]

proc isUsable*(l: CameraLook): bool =
  ## Finite, bounded and invertible, with a positive white luminance.
  for r in l.toXyz:
    for v in r:
      if not (isFin(v) and abs(v) < 64.0): return false
  invert(l.toXyz).isSome and apply(l.toXyz, [1.0, 1.0, 1.0])[1] > 1e-3

proc note*(l: CameraLook): string =
  ## The note `develop` reports.
  "colour fitted to the camera's embedded JPEG (mean ΔE " & fixed(l.deltaE, 1) & " against it; " &
    fixed(l.deltaEFallback, 1) & " with the neutral fallback)"
