## From input points to dabs (port of `paint/src/dynamics.rs`): smoothing, brush pose, path
## walking (spacing, airbrush build-up) and the per-dab dynamics (shape, scattering, transfer,
## colour, texture depth).
##
## Everything is incremental (feed points in any chunking, get the same dabs) and deterministic:
## jitter comes from `rng` keyed by the stroke seed and the step/dab index.

import std/[math, options]
import ../geom, ../rmath
import ./brush, ./rng

const
  MaxStringPx* = 100.0      ## maximum pulled-string length at 100 % smoothing, in screen pixels
  SmoothingStepMs* = 16.0   ## stroke time over which exponential smoothing moves its fraction
  SpeedSpacingMs* = 8.0
    ## With Spacing unchecked, one dab per this many milliseconds of stroke time, so faster
    ## strokes space their dabs further apart.
  MaxGridSteps = 1e7        ## longest pixel-grid segment before falling back to distance spacing
  MinSpeedStep = 0.5        ## densest speed spacing, in pixels between dabs
  MaxAirbrushDabsPerSegment = 512 ## maximum airbrush catch-up dabs for one input segment
  MaxSegmentLen = 1e7
    ## Longest segment spaced by distance (Nim guard): the engine bounds coordinates to ±1e6, so
    ## only a non-finite or absurd point from a direct caller is longer, and stepping along it
    ## would never finish. Such a segment places no spacing steps.

proc lerpPt(a, b: StrokePoint; f: float64): StrokePoint {.inline.} =
  let ff = float32(f)
  StrokePoint(x: a.x + (b.x - a.x) * f, y: a.y + (b.y - a.y) * f,
              pressure: a.pressure + (b.pressure - a.pressure) * ff,
              tiltX: a.tiltX + (b.tiltX - a.tiltX) * ff, tiltY: a.tiltY + (b.tiltY - a.tiltY) * ff,
              rotation: a.rotation + (b.rotation - a.rotation) * ff, wheel: a.wheel + (b.wheel - a.wheel) * ff,
              time: a.time + (b.time - a.time) * f)

proc isFinite(v: float64): bool {.inline.} = v == v and v != Inf and v != NegInf

# ---------------------------------------------------------------- smoothing

type
  Smoother* = object
    ## Incremental stroke smoothing: exponential (with optional time-based catch-up) or pulled string.
    cfg: Smoothing
    zoom: float64
    pos: Option[StrokePoint]
    lastInput: Option[StrokePoint]

proc newSmoother*(cfg: Smoothing; zoom: float32): Smoother =
  ## `zoom` is the view zoom (only used with `adjustForZoom`).
  Smoother(cfg: cfg, zoom: float64(rmax(zoom, 0.01'f32)))

proc active(s: Smoother): bool = s.cfg.amount > 0

proc push*(s: var Smoother; p: StrokePoint; outp: var seq[StrokePoint]) =
  ## Feed one input point; appends the brush positions to paint through.
  let prevInput = s.lastInput
  s.lastInput = some(p)
  if not s.active:
    outp.add p
    return
  if s.pos.isNone:
    s.pos = some(p)
    outp.add p
    return
  let pos = s.pos.get
  let amount = float64(clamp(s.cfg.amount, 0'f32, 1'f32))
  if s.cfg.pulledString:
    var len = amount * MaxStringPx
    if s.cfg.adjustForZoom: len /= s.zoom
    let (dx, dy) = (p.x - pos.x, p.y - pos.y)
    let d = sqrt(dx * dx + dy * dy)
    if d > len:
      let k = (d - len) / d
      var np = p
      np.x = pos.x + dx * k
      np.y = pos.y + dy * k
      s.pos = some(np)
      outp.add np
    return
  # Exponential: every SmoothingStepMs of stroke time moves `a` of the way towards the pointer,
  # so the feel doesn't depend on how often the device reports. Input without timestamps takes
  # one step per point.
  let a = 1.0 - rmin(amount, 0.95)
  let dt = if prevInput.isSome: p.time - prevInput.get.time else: 0.0
  let still = prevInput.isSome and prevInput.get.x == p.x and prevInput.get.y == p.y
  if still and not s.cfg.catchUp:
    # Without Stroke Catch-Up the brush stops while the pointer pauses.
    return
  var iters = 1
  var step = a
  if dt > 0 and isFinite(dt):
    let steps = dt / SmoothingStepMs
    let it = clamp(ceil(steps), 1.0, 64.0)
    iters = int(it)
    step = 1.0 - pow(1.0 - a, rmin(steps, 64.0) / it)
  var cur = pos
  for _ in 0 ..< iters:
    var n = p
    n.x = cur.x + (p.x - cur.x) * step
    n.y = cur.y + (p.y - cur.y) * step
    cur = n
    outp.add cur
  s.pos = some(cur)

proc lagging*(s: Smoother): bool =
  ## Is Stroke Catch-Up still pulling the brush towards a pointer that has stopped?
  if s.active and s.cfg.catchUp and not s.cfg.pulledString and s.pos.isSome and s.lastInput.isSome:
    let (pos, last) = (s.pos.get, s.lastInput.get)
    abs(pos.x - last.x) + abs(pos.y - last.y) > 0.01
  else: false

proc smoothingString*(s: Smoother): Option[(array[2, float64], array[2, float64])] =
  ## The string between the brush and the pointer: `(brush, pointer)` while smoothing holds the
  ## brush back from the last input point, none when they coincide (or smoothing is off).
  if s.pos.isNone or s.lastInput.isNone: return
  let (pos, last) = (s.pos.get, s.lastInput.get)
  if s.active and (pos.x != last.x or pos.y != last.y):
    result = some(([pos.x, pos.y], [last.x, last.y]))

proc finish*(s: var Smoother; outp: var seq[StrokePoint]) =
  ## End of stroke: with Catch-Up On Stroke End the brush finishes at the last pointer position.
  if not s.active or not s.cfg.catchUpOnEnd: return
  if s.pos.isSome and s.lastInput.isSome:
    let (pos, last) = (s.pos.get, s.lastInput.get)
    if pos.x != last.x or pos.y != last.y:
      s.pos = some(last)
      outp.add last

proc smoothPoints*(cfg: Smoothing; zoom: float32; pts: openArray[StrokePoint]): seq[StrokePoint] =
  ## Smooth a whole point list with the brush's smoothing settings.
  var s = newSmoother(cfg, zoom)
  for p in pts: s.push(p, result)
  s.finish(result)

# ---------------------------------------------------------------- path walking

type
  StepInput* = object
    ## One spacing (or airbrush time) step along the path.
    point*: StrokePoint
    direction*: float32        ## visual (counter-clockwise, y-up) direction of travel in radians
    initialDirection*: float32
    step*: uint64              ## sequential step number (0 = first)

  PathWalker* = object
    ## Walks a polyline and emits steps every `stepLen(point)` pixels, plus airbrush steps in time.
    last: Option[StrokePoint]
    nextAt: float64            ## path distance from `last` to the next step
    pendingFirst: Option[StrokePoint]
    initialDir: Option[float32]
    dir: float32
    step: uint64
    interval: Option[float64]  ## airbrush interval in ms
    timeAcc: float64
    speed: Option[float64]     ## speed spacing: one step per this many ms of stroke time
    speedAcc: float64
    grid: Option[float64]      ## pixel-grid stepping: the dab-centre offset inside a pixel

  StepLen* = proc(p: StrokePoint): float64 {.closure.}

proc newPathWalker*(buildUpIntervalMs: Option[float64]): PathWalker =
  PathWalker(interval: buildUpIntervalMs)

proc pixelGrid*(w: PathWalker; offset: float64): PathWalker =
  ## Step pixel by pixel instead of by distance: one step per pixel along the segment's major axis,
  ## each at the pixel's dab centre (`offset` 0.5 for odd tip diameters, 0 for even ones), so a
  ## small aliased tip draws a clean 8-connected staircase.
  result = w
  result.grid = some(offset)

proc speedSpacing*(w: PathWalker; intervalMs: float64): PathWalker =
  ## Space steps by stroke time instead of distance: faster movement spreads them out.
  result = w
  result.speed = some(rmax(intervalMs, 0.1))

proc emit(w: var PathWalker; point: StrokePoint; outp: var seq[StepInput]) =
  outp.add StepInput(point: point, direction: w.dir,
                     initialDirection: (if w.initialDir.isSome: w.initialDir.get else: w.dir), step: w.step)
  w.step += 1

proc gridSteps(w: var PathWalker; a, p: StrokePoint; offset: float64; outp: var seq[StepInput]): bool =
  ## Emits the pixel steps from `a` to `p`. False, emitting nothing, when the span is not finite or
  ## implausibly long: distance spacing takes over.
  proc cell(v: float64): float64 = (if offset > 0: floor(v) else: round(v))
  let (x0, y0) = (cell(a.x), cell(a.y))
  let (dx, dy) = (cell(p.x) - x0, cell(p.y) - y0)
  let n = rmax(abs(dx), abs(dy))
  if not isFinite(n) or n > MaxGridSteps: return false
  let steps = uint64(n)
  for k in 1'u64 .. steps:
    let i = float64(k)
    var q = lerpPt(a, p, i / n)
    q.x = x0 + round(dx * i / n) + offset
    q.y = y0 + round(dy * i / n) + offset
    w.emit(q, outp)
  true

proc stepLen*(f: StepLen; p: StrokePoint): float64 = f(p)

proc push*[L](w: var PathWalker; p: StrokePoint; sl: L; outp: var seq[StepInput]) =
  ## Feed one point; `sl.stepLen(point)` gives the spacing at a point (a `StepLen` closure or a
  ## dab builder).
  mixin stepLen
  if w.last.isNone:
    w.last = some(p)
    w.pendingFirst = some(p)
    w.nextAt = sl.stepLen(p)
    return
  let a = w.last.get
  let (dx, dy) = (p.x - a.x, p.y - a.y)
  let len = sqrt(dx * dx + dy * dy)
  if len > 1e-9:
    w.dir = float32(arctan2(-dy, dx))
    if w.initialDir.isNone: w.initialDir = some(w.dir)
  if w.pendingFirst.isSome:
    let f = w.pendingFirst.get
    w.pendingFirst = none(StrokePoint)
    w.emit(f, outp)
  if len > 1e-9 and len <= MaxSegmentLen:
    # Speed spacing needs the segment's duration; without timestamps the segment is spaced by
    # distance instead.
    let dt = p.time - a.time
    if w.speed.isNone or not (dt > 0 and isFinite(dt)):
      if not (w.grid.isSome and w.gridSteps(a, p, w.grid.get, outp)):
        while w.nextAt <= len + 1e-9:
          let q = lerpPt(a, p, rmin(w.nextAt / len, 1.0))
          w.emit(q, outp)
          w.nextAt += rmax(sl.stepLen(q), 0.25)
        w.nextAt -= len
    else:
      let iv = w.speed.get
      w.speedAcc += dt
      let maxN = asUsize(rmax(ceil(len / MinSpeedStep), 1.0))
      var n = 0
      while w.speedAcc >= iv and n < maxN:
        w.speedAcc -= iv
        let f = clamp((dt - w.speedAcc) / dt, 0.0, 1.0)
        w.emit(lerpPt(a, p, f), outp)
        n += 1
      if n == maxN: w.speedAcc = remEuclidF(w.speedAcc, iv)
  if w.interval.isSome:
    let iv = w.interval.get
    let dt = p.time - a.time
    if dt > 0 and isFinite(dt):
      if dt > iv * float64(MaxAirbrushDabsPerSegment):
        # A stalled or hostile timestamp must not replay an arbitrarily long backlog.
        w.timeAcc = 0
        w.emit(p, outp)
        w.last = some(p)
        return
      w.timeAcc += dt
      var n = 0
      while w.timeAcc >= iv and n < MaxAirbrushDabsPerSegment:
        w.timeAcc -= iv
        let f = clamp((dt - w.timeAcc) / dt, 0.0, 1.0)
        w.emit(lerpPt(a, p, f), outp)
        n += 1
  w.last = some(p)

proc finish*(w: var PathWalker; outp: var seq[StepInput]) =
  if w.pendingFirst.isSome:
    let f = w.pendingFirst.get
    w.pendingFirst = none(StrokePoint)
    w.emit(f, outp)

proc hasInterval*(w: PathWalker): bool = w.interval.isSome

# ---------------------------------------------------------------- dynamics

proc controlValue*(c: Control; fadeSteps: uint32; s: StepInput): float32 =
  ## Scalar control value in 0..1.
  let p = s.point
  case c
  of ctlOff, ctlInitialDirection, ctlDirection: 1
  of ctlFade: 1'f32 - rmin(float32(s.step) / float32(max(fadeSteps, 1'u32)), 1'f32)
  of ctlPenPressure: clamp(p.pressure, 0'f32, 1'f32)
  # Upright pen = 1, flat pen = 0 (a mouse reports no tilt, so paints at full value).
  of ctlPenTilt: 1'f32 - clamp(hypot(p.tiltX, p.tiltY) / 90'f32, 0'f32, 1'f32)
  of ctlStylusWheel: clamp(p.wheel, 0'f32, 1'f32)
  of ctlRotation: remEuclidF(p.rotation, 360'f32) / 360'f32

proc controlAngle*(c: Control; fadeSteps: uint32; s: StepInput): float32 =
  ## Angle contributed by a control, in degrees (counter-clockwise).
  let p = s.point
  case c
  of ctlOff: 0
  of ctlFade: 360'f32 * rmin(float32(s.step) / float32(max(fadeSteps, 1'u32)), 1'f32)
  of ctlPenPressure: 360'f32 * clamp(p.pressure, 0'f32, 1'f32)
  of ctlPenTilt:
    if p.tiltX == 0 and p.tiltY == 0: 0'f32 else: toDegrees(arctan2(-p.tiltY, p.tiltX))
  of ctlStylusWheel: 360'f32 * clamp(p.wheel, 0'f32, 1'f32)
  of ctlRotation: p.rotation
  of ctlInitialDirection: toDegrees(s.initialDirection)
  of ctlDirection: toDegrees(s.direction)

proc scalar(d: Dynamic; s: StepInput; r: float32): float32 =
  ## A 0..1 multiplier from a dynamic: control lerps from `minimum` to 1, jitter reduces randomly,
  ## never below `minimum`.
  if not d.isActive: return 1
  let mn = clamp(d.minimum, 0'f32, 1'f32)
  let cv = controlValue(d.control, d.fadeSteps, s)
  let v = (mn + (1'f32 - mn) * cv) * (1'f32 - clamp(d.jitter, 0'f32, 1'f32) * r)
  rmax(v, mn)

proc rgbToHsv(c: array[3, float32]): array[3, float32] =
  let mx = rmax(rmax(c[0], c[1]), c[2])
  let mn = rmin(rmin(c[0], c[1]), c[2])
  let d = mx - mn
  let h0 =
    if d <= 0: 0'f32
    elif mx == c[0]: remEuclidF((c[1] - c[2]) / d, 6'f32)
    elif mx == c[1]: (c[2] - c[0]) / d + 2'f32
    else: (c[0] - c[1]) / d + 4'f32
  [h0 / 6'f32, (if mx > 0: d / mx else: 0'f32), mx]

proc hsvToRgb(h, s, v: float32): array[3, float32] =
  let h6 = remEuclidF(h, 1'f32) * 6'f32
  let i = floor(h6)
  let f = h6 - i
  let (p, q, t) = (v * (1'f32 - s), v * (1'f32 - s * f), v * (1'f32 - s * (1'f32 - f)))
  case asI32(float64(i))
  of 0: [v, t, p]
  of 1: [q, v, p]
  of 2: [p, v, t]
  of 3: [p, q, v]
  of 4: [t, p, v]
  else: [v, p, q]

proc dynamicColor*(b: BrushSettings; s: StepInput; seed, index: uint64): array[4, float32] =
  ## Colour Dynamics for one dab (or once per stroke, with `index = 0`).
  let cd = b.colorDynamics
  let (fg, bg) = (b.color, b.background)
  let base = if cd.fgBg.control != ctlOff: 1'f32 - controlValue(cd.fgBg.control, cd.fgBg.fadeSteps, s) else: 0'f32
  let t = clamp(base + (1'f32 - base) * clamp(cd.fgBg.jitter, 0'f32, 1'f32) * rand01(seed, index, StFgBg), 0'f32, 1'f32)
  let rgb = [fg[0] + (bg[0] - fg[0]) * t, fg[1] + (bg[1] - fg[1]) * t, fg[2] + (bg[2] - fg[2]) * t]
  var hsv = rgbToHsv(rgb)
  var (h, sat, v) = (hsv[0], hsv[1], hsv[2])
  h += randSigned(seed, index, StHue) * clamp(cd.hueJitter, 0'f32, 1'f32) * 0.5'f32
  sat = clamp(sat + randSigned(seed, index, StSat) * clamp(cd.saturationJitter, 0'f32, 1'f32), 0'f32, 1'f32)
  let pu = clamp(cd.purity, -1'f32, 1'f32)
  sat = if pu >= 0: sat + (1'f32 - sat) * pu else: sat * (1'f32 + pu)
  v = clamp(v + randSigned(seed, index, StBright) * clamp(cd.brightnessJitter, 0'f32, 1'f32), 0'f32, 1'f32)
  let o = hsvToRgb(h, sat, v)
  [o[0], o[1], o[2], fg[3] + (bg[3] - fg[3]) * t]

proc baseDiameter*(b: BrushSettings; s: StepInput): float32 =
  ## Base (pre-jitter) diameter at a step: size × size control (pressure etc.). Drives spacing.
  let sd = b.shapeDynamics
  var f = 1'f32
  if sd.enabled and sd.size.control != ctlOff:
    let mn = clamp(sd.size.minimum, 0'f32, 1'f32)
    f = mn + (1'f32 - mn) * controlValue(sd.size.control, sd.size.fadeSteps, s)
  elif b.pressureSize:
    f = clamp(s.point.pressure, 0'f32, 1'f32)
  b.size * f

type
  DabBuilder* = object
    ## Builds the primary dabs of a step (Shape Dynamics, Scattering, Transfer, Color, Texture depth).
    brush*: BrushSettings
    seed*: uint64
    strokeColor: Option[array[4, float32]] ## per-stroke colour (Color Dynamics, per tip off)
    nextIndex: uint64

  DualBuilder* = object
    ## Builds Dual Brush dabs (the second tip: size, spacing, scatter, count, flip).
    brush*: BrushSettings
    nextIndex: uint64

proc newDabBuilder*(brush: BrushSettings): DabBuilder = DabBuilder(brush: brush, seed: brush.seed)

proc stepLen*(db: DabBuilder; p: StrokePoint): float64 =
  let s = StepInput(point: p)
  let d = rmax(baseDiameter(db.brush, s), 1'f32)
  rmax(float64(d * rmax(db.brush.spacing, 0.01'f32)), 0.5)

proc build*(db: var DabBuilder; s: StepInput; outp: var seq[Dab]) =
  let seed = db.seed
  let count =
    if db.brush.scattering.enabled:
      let sc = db.brush.scattering
      let n = float32(clamp(sc.count, 1'u32, 16'u32))
      let k = scalar(sc.countJitter, s, rand01(seed, s.step, StCount))
      clamp(asU32(round(n * k)), 1'u32, 16'u32)
    else: 1'u32
  let baseD = baseDiameter(db.brush, s)
  for _ in 0'u32 ..< count:
    let i = db.nextIndex
    db.nextIndex += 1
    template b: untyped = db.brush
    template sd: untyped = db.brush.shapeDynamics
    template sc: untyped = db.brush.scattering
    template r(st: uint64): float32 = rand01(seed, i, st)
    # Shape.
    var diameter = baseD
    var angle = b.angle
    var roundness = clamp(b.roundness, 0.01'f32, 1'f32)
    var (fx, fy) = (b.flipX, b.flipY)
    if sd.enabled:
      if sd.size.jitter > 0:
        let mn = clamp(sd.size.minimum, 0'f32, 1'f32)
        diameter *= rmax(1'f32 - clamp(sd.size.jitter, 0'f32, 1'f32) * r(StSize), mn)
      angle += controlAngle(sd.angle.control, sd.angle.fadeSteps, s) +
        randSigned(seed, i, StAngle) * clamp(sd.angle.jitter, 0'f32, 1'f32) * 180'f32
      roundness = clamp(roundness * scalar(sd.roundness, s, r(StRoundness)), 0.01'f32, 1'f32)
      if sd.flipXJitter and r(StFlipX) < 0.5'f32: fx = not fx
      if sd.flipYJitter and r(StFlipY) < 0.5'f32: fy = not fy
    # Tilt Scale and Brush Projection (Shape Dynamics).
    var (projAngle, projScale) = (0'f32, 1'f32)
    if sd.enabled:
      let tilt = clamp(hypot(s.point.tiltX, s.point.tiltY), 0'f32, 90'f32)
      if sd.size.control == ctlPenTilt and sd.tiltScale > 0:
        let k = 1'f32 - clamp(sd.tiltScale, 0'f32, 2'f32) * 0.5'f32 * tilt / 90'f32
        roundness = clamp(roundness * k, 0.01'f32, 1'f32)
      if sd.brushProjection:
        angle += s.point.rotation
        if tilt > 0:
          projAngle = arctan2(-s.point.tiltY, s.point.tiltX)
          projScale = rmax(cos(toRadians(tilt)), 0.05'f32)
    let radius = rmax(diameter / 2'f32, 0.5'f32)
    # Scatter.
    var c = point(s.point.x, s.point.y)
    if sc.enabled and sc.scatter.jitter > 0:
      let cv = controlValue(sc.scatter.control, sc.scatter.fadeSteps, s)
      let amt = float64(sc.scatter.jitter * cv * radius)
      let (ox, oy) = (float64(randSigned(seed, i, StScatterX)) * amt, float64(randSigned(seed, i, StScatterY)) * amt)
      if sc.bothAxes:
        c.x += ox
        c.y += oy
      else:
        # Perpendicular to the direction of travel (y-down image space).
        let (dx, dy) = (float64(cos(s.direction)), -float64(sin(s.direction)))
        c.x += -dy * ox
        c.y += dx * ox
    # Transfer.
    template tr: untyped = db.brush.transfer
    var (opacity, flow) = (1'f32, b.flow)
    if tr.enabled:
      opacity = scalar(tr.opacity, s, r(StOpacity))
      flow *= scalar(tr.flow, s, r(StFlow))
    let (wet, mix) = if tr.enabled: (scalar(tr.wetness, s, r(StWet)), scalar(tr.mix, s, r(StMix))) else: (1'f32, 1'f32)
    if b.pressureOpacity and not (tr.enabled and tr.opacity.control != ctlOff):
      opacity *= clamp(s.point.pressure, 0'f32, 1'f32)
    # Colour.
    let color =
      if b.colorDynamics.enabled:
        if b.colorDynamics.perTip: dynamicColor(b, s, seed, i)
        else:
          if db.strokeColor.isNone: db.strokeColor = some(dynamicColor(b, s, seed, 0))
          db.strokeColor.get
      else: b.color
    # Texture depth (per tip).
    template tx: untyped = db.brush.texture
    let depth =
      if tx.enabled and tx.eachTip: clamp(tx.depth, 0'f32, 1'f32) * scalar(tx.depthJitter, s, r(StDepth))
      else: clamp(tx.depth, 0'f32, 1'f32)
    outp.add Dab(center: c, radius: radius, alpha: clamp(flow, 0'f32, 1'f32), angle: toRadians(angle),
                 roundness: roundness, flipX: fx, flipY: fy, opacity: clamp(opacity, 0'f32, 1'f32),
                 color: color, depth: depth, index: i, projAngle: projAngle, projScale: projScale,
                 wet: wet, mix: mix)

proc newDualBuilder*(brush: BrushSettings): DualBuilder = DualBuilder(brush: brush)

proc stepLen*(db: DualBuilder; p: StrokePoint): float64 =
  let d = db.brush.dualBrush
  rmax(float64(rmax(d.size, 1'f32) * rmax(d.spacing, 0.01'f32)), 0.5)

proc build*(db: var DualBuilder; s: StepInput; outp: var seq[Dab]) =
  template d: untyped = db.brush.dualBrush
  let seed = db.brush.seed xor 0xD0A1'u64
  for _ in 0'u32 ..< clamp(d.count, 1'u32, 16'u32):
    let i = db.nextIndex
    db.nextIndex += 1
    let radius = rmax(d.size / 2'f32, 0.5'f32)
    let amt = float64(d.scatter * radius)
    let (ox, oy) = (float64(randSigned(seed, i, StDualX)) * amt, float64(randSigned(seed, i, StDualY)) * amt)
    var c = point(s.point.x, s.point.y)
    if d.bothAxes:
      c.x += ox
      c.y += oy
    else:
      let (dx, dy) = (float64(cos(s.direction)), -float64(sin(s.direction)))
      c.x += -dy * ox
      c.y += dx * ox
    let flip = d.flip and rand01(seed, i, StDualFlip) < 0.5'f32
    outp.add Dab(center: c, radius: radius, alpha: 1, angle: toRadians(d.angle),
                 roundness: clamp(d.roundness, 0.01'f32, 1'f32), flipX: flip, opacity: 1,
                 color: [0'f32, 0, 0, 1], depth: 1, index: i, projScale: 1, wet: 1, mix: 1)

proc applyPose*(b: BrushSettings; p0: StrokePoint): StrokePoint =
  ## Apply Brush Pose to an input point.
  result = p0
  let pose = b.pose
  if pose.enabled:
    if pose.overrideTilt:
      result.tiltX = pose.tiltX
      result.tiltY = pose.tiltY
    if pose.overrideRotation: result.rotation = pose.rotation
    if pose.overridePressure: result.pressure = pose.pressure

type
  DabGenerator* = object
    ## Incremental dab generation for a whole brush (primary and optional dual dabs).
    smoother: Smoother
    walker: PathWalker
    dualWalker: Option[PathWalker]
    builder*: DabBuilder
    dual: Option[DualBuilder]

proc wantsTime*(g: DabGenerator): bool =
  ## Does the stroke change with time while the pointer is held still (airbrush Build-up, or
  ## smoothing still catching up)? The canvas then keeps feeding it the held point.
  g.walker.hasInterval or g.smoother.lagging

proc smoothingString*(g: DabGenerator): Option[(array[2, float64], array[2, float64])] =
  g.smoother.smoothingString

proc newDabGenerator*(brush0: BrushSettings; zoom: float32): DabGenerator =
  let brush = brush0.boundedForRender
  let interval = if brush.buildUp: some(1000.0 / float64(clamp(brush.buildUpRate, 0.1'f32, 1000'f32))) else: none(float64)
  let dualOn = brush.dualBrush.enabled
  var walker = if brush.spacingEnabled: newPathWalker(interval) else: newPathWalker(interval).speedSpacing(SpeedSpacingMs)
  # A small aliased tip whose dabs are at most a pixel apart steps pixel by pixel.
  if brush.aliased and brush.size * rmax(brush.spacing, 0.01'f32) <= 1:
    walker = walker.pixelGrid(float64(gridCenter(0, 0, brush.size)[0]))
  DabGenerator(smoother: newSmoother(brush.smoothing, zoom), walker: walker,
               dualWalker: (if dualOn: some(newPathWalker(none(float64))) else: none(PathWalker)),
               builder: newDabBuilder(brush),
               dual: (if dualOn: some(newDualBuilder(brush)) else: none(DualBuilder)))

proc walk(g: var DabGenerator; pts: openArray[StrokePoint]; dabs, dual: var seq[Dab]) =
  for p0 in pts:
    let p = applyPose(g.builder.brush, p0)
    var steps: seq[StepInput]
    g.walker.push(p, g.builder, steps)
    for s in steps: g.builder.build(s, dabs)
    if g.dualWalker.isSome and g.dual.isSome:
      var dsteps: seq[StepInput]
      g.dualWalker.get.push(p, g.dual.get, dsteps)
      for s in dsteps: g.dual.get.build(s, dual)

proc push*(g: var DabGenerator; pts: openArray[StrokePoint]; dabs, dual: var seq[Dab]) =
  ## Feed input points; appends the new primary and dual dabs.
  var sm: seq[StrokePoint]
  for p in pts: g.smoother.push(p, sm)
  g.walk(sm, dabs, dual)

proc finish*(g: var DabGenerator; dabs, dual: var seq[Dab]) =
  ## End of stroke.
  var sm: seq[StrokePoint]
  g.smoother.finish(sm)
  g.walk(sm, dabs, dual)
  var steps: seq[StepInput]
  g.walker.finish(steps)
  for s in steps: g.builder.build(s, dabs)
  if g.dualWalker.isSome and g.dual.isSome:
    var dsteps: seq[StepInput]
    g.dualWalker.get.finish(dsteps)
    for s in dsteps: g.dual.get.build(s, dual)

proc dabs*(stroke: Stroke): seq[Dab] =
  ## All primary dabs of a stroke (smoothing, pose, spacing, build-up and dynamics applied).
  var g = newDabGenerator(stroke.brush, 1)
  var dual: seq[Dab]
  g.push(stroke.points, result, dual)
  g.finish(result, dual)
