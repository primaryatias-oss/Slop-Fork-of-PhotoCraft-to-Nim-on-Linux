## Gradient geometry and sampling shared by fill layers, layer effects and the Gradient tool:
## Photoshop's chord/diagonal lengths, whole-pixel end-point snapping, midpoints, opacity stops,
## dither, and handle positions for on-canvas editing.

import std/[math, algorithm, options]
import ../doc, ../raster, ./buffer

proc rad(d: float32): float32 {.inline.} = d * float32(PI) / 180

proc gradientUnits*(angle, w, h: float32): (float32, float32) =
  ## (chord, diagonal) lengths over a `w × h` frame at `angle` degrees.
  let a = rad(angle)
  let (s, c) = (sin(a), cos(a))
  let ln = max(sqrt(pow(c * w, 2) + pow(s * h, 2)), 1'f32)
  let chord = max(min(w / max(abs(c), 1e-6), h / max(abs(s), 1e-6)), 1'f32)
  (chord, ln)

proc gradientT*(style: GradientStyle; angle, scale: float32; reverse: bool; offset: (float32, float32);
                bounds: Rect; x, y: float32): float32 =
  let w = float32(max(bounds.width, 1))
  let h = float32(max(bounds.height, 1))
  let cx = float32(bounds.x0) + w / 2 + offset[0] * w
  let cy = float32(bounds.y0) + h / 2 + offset[1] * h
  let a = rad(angle)
  let (s, c) = (sin(a), cos(a))
  let (dx, dy) = (x - cx, y - cy)
  let along = dx * c - dy * s
  let across = dx * s + dy * c
  var (chord, ln) = gradientUnits(angle, w, h)
  chord *= max(scale, 1e-3)
  ln *= max(scale, 1e-3)
  let corner = 0.5'f32 * (c - s)
  var t = case style
    of gsLinear: (along - corner) / chord + 0.5
    of gsReflected: abs((along - corner) / (chord / 2))
    of gsRadial: sqrt(dx * dx + dy * dy) / (ln / 2)
    of gsDiamond: (abs(along) + abs(across)) / (ln / 2)
    of gsAngle:
      (let v = (a - arctan2(-dy, dx)) / float32(TAU); v - floor(v))
  t = clamp(t, 0'f32, 1'f32)
  if reverse: 1 - t else: t

proc gradientLayout*(style: GradientStyle; angle, scale: float32; offset: (float32, float32); frame: Rect): (float32, float32, (float32, float32)) =
  ## Effective layout snapping Linear/Reflected end points to whole pixels (Photoshop).
  let unchanged = (angle, scale, offset)
  template fin(v: float32): bool = v == v and abs(v) != Inf
  if style notin {gsLinear, gsReflected} or not fin(angle) or not fin(scale) or not fin(offset[0]) or not fin(offset[1]):
    return unchanged
  let w = float64(max(frame.width, 1))
  let h = float64(max(frame.height, 1))
  let (cx, cy) = (float64(frame.x0) + w / 2 + float64(offset[0]) * w, float64(frame.y0) + h / 2 + float64(offset[1]) * h)
  proc chordOf(a: float64): float64 = max(min(w / max(abs(cos(a)), 1e-6), h / max(abs(sin(a)), 1e-6)), 1.0)
  let a = degToRad(float64(angle))
  let half = chordOf(a) * float64(max(scale, 1e-3)) / 2
  let (dx, dy) = (cos(a) * half, -sin(a) * half)
  template snap(v: float64): float64 = floor(v + 1e-4)
  let e = (snap(cx + dx), snap(cy + dy))
  var start, mid: (float64, float64)
  if style == gsReflected:
    start = (cx, cy); mid = (cx, cy)
  else:
    start = (snap(cx - dx), snap(cy - dy))
    mid = ((start[0] + e[0]) / 2, (start[1] + e[1]) / 2)
  let v = (e[0] - start[0], e[1] - start[1])
  let ln = hypot(v[0], v[1])
  if ln < 0.5: return unchanged
  let a2 = arctan2(-v[1], v[0])
  let span = if style == gsReflected: 2 * ln else: ln
  (float32(radToDeg(a2)), float32(span / chordOf(a2)), (offset[0] + float32((mid[0] - cx) / w), offset[1] + float32((mid[1] - cy) / h)))

proc midpointRemap*(u, m0: float32): float32 =
  let m = clamp(m0, 0.05'f32, 0.95'f32)
  if abs(m - 0.5) < 1e-6: u elif u <= m: 0.5 * u / m else: 0.5 + 0.5 * (u - m) / (1 - m)

type Ramp*[N: static int] = object
  color*: seq[(float32, array[N, float32])]
  mids*: seq[float32]
  opacity*: seq[(float32, float32)]

proc sample*[N: static int](r: Ramp[N]; t: float32): array[N, float32] =
  if r.color.len > 0:
    if t <= r.color[0][0]: result = r.color[0][1]
    else:
      result = r.color[^1][1]
      for i in 0 ..< r.color.len - 1:
        let (a, b) = (r.color[i], r.color[i + 1])
        if t <= b[0]:
          let u = if b[0] > a[0]: (t - a[0]) / (b[0] - a[0]) else: 0'f32
          let k = midpointRemap(u, if i < r.mids.len: r.mids[i] else: 0.5'f32)
          for c in 0 ..< N: result[c] = a[1][c] + (b[1][c] - a[1][c]) * k
          break
  if r.opacity.len > 0:
    var o = r.opacity[^1][1]
    if t <= r.opacity[0][0]: o = r.opacity[0][1]
    else:
      for i in 0 ..< r.opacity.len - 1:
        if t <= r.opacity[i + 1][0]:
          let u = if r.opacity[i + 1][0] > r.opacity[i][0]: (t - r.opacity[i][0]) / (r.opacity[i + 1][0] - r.opacity[i][0]) else: 0'f32
          o = r.opacity[i][1] + (r.opacity[i + 1][1] - r.opacity[i][1]) * u
          break
    result[N - 1] *= o

proc rampOf*(f: Fill): Ramp[4] =
  for (p, c) in f.stops:
    let rgb = c.toRgb
    result.color.add (p, [rgb[0], rgb[1], rgb[2], c.alpha])
  result.color.sort(proc (a, b: (float32, array[4, float32])): int = cmp(a[0], b[0]))
  result.mids = f.midpoints
  result.opacity = f.opacityStops
  result.opacity.sort(proc (a, b: (float32, float32)): int = cmp(a[0], b[0]))

proc ditherTo(c: var openArray[float32]; n: int; x, y: int; quantum: float32) =
  let d = (ditherNoise(x, y) - 0.5'f32) / 255
  for i in 0 ..< n:
    c[i] = clamp(c[i] + d, 0'f32, 1'f32)
    if quantum > 0: c[i] = floor(c[i] * quantum + 0.5) / quantum

proc renderGradient*(f: Fill; rc, frame: Rect; quantum: float32 = 0): seq[Px] =
  ## A gradient fill over `rc`, laid out in `frame` (`quantum` > 0 rounds dithered output).
  result = newSeq[Px](max(rc.width * rc.height, 0))
  if f.kind != fkGradient or rc.width <= 0: return
  let ramp = rampOf(f)
  let (angle, scale, offset) = gradientLayout(f.style, f.angle, f.scale, f.offset, frame)
  var i = 0
  for y in rc.y0 ..< rc.y1:
    for x in rc.x0 ..< rc.x1:
      var p = ramp.sample(gradientT(f.style, angle, scale, f.reverse, offset, frame, float32(x) + 0.5, float32(y) + 0.5))
      if f.dither: ditherTo(p, 3, x, y, quantum)
      result[i] = p
      inc i

proc renderCmykFill*(f: Fill; rc, frame: Rect; depth: SampleType): Option[Surface] =
  ## A fill rendered in native CMYK (ink values) for CMYK documents; `none` for patterns.
  var s = newSurface(PixelFormat(mode: cmCmyk, sample: depth, alpha: true))
  proc native(c: Color): array[5, float32] =
    let k = c.inMode(cmCmyk)
    [k.c[0], k.c[1], k.c[2], k.c[3], k.alpha]
  case f.kind
  of fkSolid: s.fillRect(rc, native(f.color))
  of fkGradient:
    var ramp: Ramp[5]
    for (p, c) in f.stops: ramp.color.add (p, native(c))
    ramp.color.sort(proc (a, b: (float32, array[5, float32])): int = cmp(a[0], b[0]))
    ramp.mids = f.midpoints
    ramp.opacity = f.opacityStops
    ramp.opacity.sort(proc (a, b: (float32, float32)): int = cmp(a[0], b[0]))
    let (angle, scale, offset) = gradientLayout(f.style, f.angle, f.scale, f.offset, frame)
    var data = newSeqOfCap[float32](rc.width * rc.height * 5)
    for y in rc.y0 ..< rc.y1:
      for x in rc.x0 ..< rc.x1:
        var p = ramp.sample(gradientT(f.style, angle, scale, f.reverse, offset, frame, float32(x) + 0.5, float32(y) + 0.5))
        if f.dither: ditherTo(p, 4, x, y, 0)
        data.add p
    s.writeRegion(rc, data)
  of fkPattern: return none(Surface)
  s.prune()
  some(s)

proc unitReach(style: GradientStyle; angle: float32; frame: Rect): float32 =
  let (w, h) = (float32(max(frame.width, 1)), float32(max(frame.height, 1)))
  let (chord, ln) = gradientUnits(angle, w, h)
  case style
  of gsLinear: chord
  of gsReflected: chord / 2
  else: ln / 2

proc handles*(style: GradientStyle; angle, scale: float32; offset: (float32, float32); frame: Rect): (array[2, float32], array[2, float32]) =
  ## On-canvas start/end handles of a gradient laid out in `frame`.
  let (w, h) = (float32(max(frame.width, 1)), float32(max(frame.height, 1)))
  let c: array[2, float32] = [float32(frame.x0) + w / 2 + offset[0] * w, float32(frame.y0) + h / 2 + offset[1] * h]
  let a = rad(angle)
  let dir: array[2, float32] = [cos(a), -sin(a)]
  let reach: float32 = unitReach(style, angle, frame) * max(scale, 1e-3'f32)
  if style == gsLinear:
    let hh: float32 = reach / 2
    result = ([c[0] - dir[0] * hh, c[1] - dir[1] * hh], [c[0] + dir[0] * hh, c[1] + dir[1] * hh])
  else: result = (c, [c[0] + dir[0] * reach, c[1] + dir[1] * reach])

proc fromHandles*(style: GradientStyle; frm, to: array[2, float32]; frame: Rect; fallbackAngle: float32): (float32, float32, (float32, float32)) =
  ## Angle, scale and offset for handles dragged to `frm` → `to`.
  let (dx, dy) = (to[0] - frm[0], to[1] - frm[1])
  var ln = hypot(dx, dy)
  let angle = if ln > 1e-4: arctan2(-dy, dx) * 180 / float32(PI) else: fallbackAngle
  ln = max(ln, 0.5)
  let c = if style == gsLinear: [(frm[0] + to[0]) / 2, (frm[1] + to[1]) / 2] else: frm
  let (w, h) = (float32(max(frame.width, 1)), float32(max(frame.height, 1)))
  (angle, ln / unitReach(style, angle, frame), ((c[0] - float32(frame.x0) - w / 2) / w, (c[1] - float32(frame.y0) - h / 2) / h))
