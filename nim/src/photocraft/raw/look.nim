## A per-file colour rendering ("camera look") fitted to the camera's own embedded JPEG (port of
## `raw/src/look.rs`), for raw files without colour calibration (NEF, ARW, CR2, RW2, ORF…), where
## the neutral fallback (camera channels treated as sRGB primaries, no tone curve) renders flat,
## grey and slightly green.
##
## Both the sensor data and the camera JPEG are reduced to a common grid of at most `ProxyEdge`
## cells on the long edge, over the whole (upright) frame. Each cell pairs the white-balanced
## camera RGB `c` (the white balance `developSensor` applies) with the JPEG's colour in linear
## ProPhoto RGB `y`. The model is the develop pipeline itself, `y ≈ f(M · c)` (M: 3×3, scaled so
## the balanced white keeps the exposure gain's luminance; f: one monotone tone curve applied per
## channel), fitted by alternating: `f` by binned (square-root domain) pool-adjacent-violators
## isotonic regression, `M` by weighted least squares of `f⁻¹(y)` on `c`, weighted so the residual
## approximates a CIE L* difference, ridge-pulled toward the neutral fallback, with Tukey
## biweights. The look is accepted only when its matrix is plausible and it lowers the held-out
## mean CIE76 ΔE to at most 85 % of the neutral fallback's.

import std/[options, math, algorithm]
import ../rmath
from ../geom import asUsize
import ./tiff, ./color, ./sensor, ./develop, ./tone, ./rutil

const ProxyEdge* = 256 ## long edge of the fit grid, in cells

type
  Reference* = object
    ## The camera's rendering of the picture: upright, interleaved 8-bit sRGB.
    width*, height*: uint32
    rgb*: seq[byte] ## `width * height * 3` samples

  LookRejected* = object of CatchableError
    ## Why no look was fitted (the neutral fallback stays).
    reason*: string

  Cell = object
    cam: array[3, float64]  ## white-balanced camera RGB, 0..1
    camClipped: bool        ## any sampled sensor value at or near clipping
    rgb: array[3, float64]  ## the JPEG's colour, linear ProPhoto RGB
    lum: float64            ## luminance (Y, D50) of `rgb`
    refClipped: float64     ## fraction of the cell's JPEG pixels with a channel at 250 or more
    edgeWeight: float64     ## weight from the JPEG's local contrast (edges count less)

  Sample = object
    ## A training / evaluation sample.
    cam, rgb: array[3, float64]
    lum, perceptual, weight: float64

  Lab = object
    ## CIE L*a*b* (D50) of linear ProPhoto RGB.
    toXyz: Mat3
    white: array[3, float64]

proc reject(why: string): ref LookRejected =
  (ref LookRejected)(reason: why, msg: "colour not fitted to the camera JPEG: " & why)

proc checkedMul(a, b: int): Option[int] =
  if a < 0 or b < 0: return
  if a != 0 and b > high(int) div a: return
  some(a * b)

proc isFin(v: float64): bool {.inline.} = v == v and abs(v) != Inf

proc srgbToLinear(v: float64): float64 =
  if v <= 0.04045: v / 12.92 else: pow((v + 0.055) / 1.055, 2.4)

proc sensorCell(s: Sensor; mult: array[3, float64]; x0, x1, y0, y1: int): Option[(array[3, float64], bool)] =
  ## Mean white-balanced camera RGB over `x0..x1 × y0..y1` (sampling at most about 8 × 8 Bayer
  ## quads), and whether any sample was near clipping.
  proc step(a, b: int): int = max((max(b - a, 0) + 15) div 16, 1) * 2
  let (sx, sy) = (step(x0, x1), step(y0, y1))
  var sum: array[3, float64]
  var n: array[3, uint32]
  var clipped = false
  template addSample(ch0: int; v0: float32) =
    let ch = ch0
    let v = float64(v0)
    let b = v * (if ch >= 0 and ch < 3: mult[ch] else: 1.0)
    clipped = clipped or v >= 0.98 or b >= 0.98
    if ch >= 0 and ch < 3:
      sum[ch] += clamp(b, 0.0, 1.0)
      n[ch] += 1
  var y = y0
  while y < y1:
    var x = x0
    while x < x1:
      for (dx, dy) in [(0, 0), (1, 0), (0, 1), (1, 1)]:
        let (px, py) = (x + dx, y + dy)
        if px >= x1 or py >= y1: continue
        if s.cfa.isSome and s.samples == 1:
          addSample(min(int(s.cfa.get.color(px, py)), 2), scaleValue(s, px, py, 0))
        else:
          for k in 0 .. 2: addSample(k, scaleValue(s, px, py, k))
      x += sx
    y += sy
  if n[0] > 0 and n[1] > 0 and n[2] > 0:
    result = some(([sum[0] / float64(n[0]), sum[1] / float64(n[1]), sum[2] / float64(n[2])], clipped))

proc referenceCell(r: Reference; lut: openArray[float64]; x0, x1, y0, y1: int): Option[(array[3, float64], float64, float64)] =
  ## Mean linear sRGB of a reference box, the fraction of its pixels with a clipped channel and an
  ## edge weight from its luminance spread.
  let rw = int(r.width)
  var sum: array[3, float64]
  var (l1, l2) = (0.0, 0.0)
  var (n, clipped) = (0, 0)
  for y in y0 ..< y1:
    let (a, b) = ((y * rw + x0) * 3, (y * rw + x1) * 3)
    if a > b or b > r.rgb.len: return
    var p = a
    while p + 3 <= b:
      var v: array[3, float64]
      for k in 0 .. 2:
        let byt = int(r.rgb[p + k])
        v[k] = if byt < lut.len: lut[byt] else: 0.0
        sum[k] += v[k]
      let l = 0.2126 * v[0] + 0.7152 * v[1] + 0.0722 * v[2]
      l1 += l
      l2 += l * l
      inc n
      if r.rgb[p] >= 250 or r.rgb[p + 1] >= 250 or r.rgb[p + 2] >= 250: inc clipped
      p += 3
  if n == 0: return
  let nf = float64(n)
  let mean = l1 / nf
  let sd = sqrt(rmax(l2 / nf - mean * mean, 0.0))
  let cv = sd / (mean + 0.01)
  let q = cv / 0.15
  some(([sum[0] / nf, sum[1] / nf, sum[2] / nf], float64(clipped) / nf, 1.0 / (1.0 + q * q)))

proc grid(s: Sensor; opts: DevelopOptions; r: Reference): (int, seq[Cell]) =
  ## The upright fit grid: (width in cells, cells row-major).
  let c = s.crop
  let inData = c.x >= 0 and c.width >= 0 and c.x <= high(int) - c.width and c.x + c.width <= s.width and
    c.y >= 0 and c.height >= 0 and c.y <= high(int) - c.height and c.y + c.height <= s.height
  var need = checkedMul(s.width, s.height)
  if need.isSome: need = checkedMul(need.get, s.samples)
  if c.isEmpty or not inData or need.isNone or s.data.len < need.get:
    raise reject("the crop lies outside the sensor data")
  if not (s.samples == 3 or (s.samples == 1 and s.cfa.isSome)): raise reject("unsupported sensor layout")
  let (rw, rh) = (int(r.width), int(r.height))
  var n3 = checkedMul(rw, rh)
  if n3.isSome: n3 = checkedMul(n3.get, 3)
  if rw < 32 or rh < 32 or n3 != some(r.rgb.len):
    raise reject("the reference image is too small or its buffer does not match its size")
  let swap = s.orientation in 5'u16 .. 8'u16
  let (uw, uh) = if swap: (c.height, c.width) else: (c.width, c.height)
  let aspect = (float64(rw) / float64(rh)) / (float64(uw) / float64(uh))
  if not (isFin(aspect) and abs(ln(aspect)) <= 0.03):
    raise reject("the reference (" & $rw & "x" & $rh & ") does not have the raw frame's shape (" & $uw & "x" & $uh & ")")
  # Cells at least 4 sensor pixels and 2 reference pixels across.
  let long = max(uw, uh)
  let edge = min(min(ProxyEdge, long div 4), max(rw, rh) div 2)
  let gwUp = max((uw * edge + long - 1) div long, 1)
  let ghUp = max((uh * edge + long - 1) div long, 1)
  if gwUp < 16 or ghUp < 16: raise reject("the image is too small to fit")
  var warnings: seq[string]
  var neutral: array[3, float64]
  try: neutral = resolveNeutral(s, opts, warnings)
  except RawError as e: raise reject(e.msg)
  let mult = [1.0 / neutral[0], 1.0 / neutral[1], 1.0 / neutral[2]]

  # Sensor-orientation grid.
  let (gw, gh) = if swap: (ghUp, gwUp) else: (gwUp, ghUp)
  var sensorCells: seq[Option[(array[3, float64], bool)]]
  for j in 0 ..< gh:
    let (y0, y1) = (c.y + j * c.height div gh, c.y + (j + 1) * c.height div gh)
    for i in 0 ..< gw:
      let (x0, x1) = (c.x + i * c.width div gw, c.x + (i + 1) * c.width div gw)
      sensorCells.add sensorCell(s, mult, x0, x1, y0, y1)

  # Reference cells, upright; then pair with the oriented sensor cells.
  var lut = newSeq[float64](256)
  for v in 0 .. 255: lut[v] = srgbToLinear(float64(v) / 255.0)
  let toPro = mul(xyzD50ToProphoto(), srgbToXyzD50())
  let proToXyz = rgbToXyz(Romm, D50Xy)
  var cells = newSeqOfCap[Cell](gwUp * ghUp)
  for j in 0 ..< ghUp:
    let (y0, y1) = (j * rh div ghUp, max((j + 1) * rh div ghUp, j * rh div ghUp + 1))
    for i in 0 ..< gwUp:
      let (x0, x1) = (i * rw div gwUp, max((i + 1) * rw div gwUp, i * rw div gwUp + 1))
      let rc = referenceCell(r, lut, x0, min(x1, rw), y0, min(y1, rh))
      let (sx, sy) = sourceXy(s.orientation, gw, gh, i, j)
      let si = sy * gw + sx
      if si < 0 or si >= sensorCells.len or sensorCells[si].isNone or rc.isNone:
        cells.add Cell(camClipped: true)
        continue
      let (cam, camClipped) = sensorCells[si].get
      let (srgb, refClipped, edgeWeight) = rc.get
      let rgb = apply(toPro, srgb)
      let lum = apply(proToXyz, rgb)[1]
      cells.add Cell(cam: cam, camClipped: camClipped, rgb: rgb, lum: lum, refClipped: refClipped, edgeWeight: edgeWeight)
  (gwUp, cells)

proc newLab(): Lab =
  let toXyz = rgbToXyz(Romm, D50Xy)
  Lab(white: apply(toXyz, [1.0, 1.0, 1.0]), toXyz: toXyz)

proc lab(l: Lab; rgb: array[3, float64]): array[3, float64] =
  let xyz = apply(l.toXyz, rgb)
  proc f(t: float64): float64 = (if t > 216.0 / 24389.0: rmath.cbrt(t) else: (24389.0 / 27.0 * t + 16.0) / 116.0)
  let (fx, fy, fz) = (f(xyz[0] / l.white[0]), f(xyz[1] / l.white[1]), f(xyz[2] / l.white[2]))
  [116.0 * fy - 16.0, 500.0 * (fx - fy), 200.0 * (fy - fz)]

proc deltaE(l: Lab; a, b: array[3, float64]): float64 =
  let (p, q) = (l.lab(a), l.lab(b))
  let (d0, d1, d2) = (p[0] - q[0], p[1] - q[1], p[2] - q[2])
  sqrt(d0 * d0 + d1 * d1 + d2 * d2)

proc labDeltaE*(a, b: array[3, float64]): float64 =
  ## CIE76 ΔE (D50) between two linear ProPhoto colours (tests).
  newLab().deltaE(a, b)

proc labOf*(rgb: array[3, float64]): array[3, float64] =
  ## CIE L*a*b* (D50) of a linear ProPhoto colour (tests).
  newLab().lab(rgb)

proc render(m: Mat3; tone: Option[ToneCurve]; cam: array[3, float64]): array[3, float64] =
  ## The develop pipeline's output for one sample (clipped to 0..1 like the 16-bit encode).
  let v = apply(m, cam)
  for k in 0 .. 2:
    let x = if isFin(v[k]): clamp(v[k], 0.0, 1.0) else: 0.0
    result[k] = if tone.isSome: tone.get.eval(x) else: x

const Chunk = 4096 ## samples per (Rust parallel) chunk of the least-squares accumulation

proc meanDeltaE(lab: Lab; samples: openArray[Sample]; m: Mat3; tone: Option[ToneCurve]): float64 =
  if samples.len == 0: return Inf
  var total = -0.0
  var lo = 0
  while lo < samples.len:
    var part = -0.0
    for i in lo ..< min(lo + Chunk, samples.len): part += lab.deltaE(render(m, tone, samples[i].cam), samples[i].rgb)
    total += part
    lo += Chunk
  total / float64(samples.len)

proc perceptual(lum: float64): float64 =
  ## Error-weight scale turning a linear difference near luminance `lum` into roughly L* units
  ## (dL*/dY = 116/3 · Y^(-2/3)).
  (116.0 / 3.0) * pow(rmax(lum, 0.0) + 0.008, -2.0 / 3.0)

const ProToXyzYRow: Mat3 = [[0.288040, 0.711874, 0.000086], [0.0, 0.0, 0.0], [0.0, 0.0, 0.0]]
  ## The Y row of linear ProPhoto → XYZ (other rows zero), for luminance.

proc plausible(rel: Mat3): bool =
  ## A plausible white-balanced camera → ProPhoto matrix (exposure removed): moderate diagonal and
  ## cross-talk, rows summing to about 1 (neutrals stay near neutral), not mirrored or collapsed.
  var entries = true
  for i in 0 .. 2:
    for j in 0 .. 2:
      let v = rel[i][j]
      let ok = if i == j: v >= 0.2 and v <= 3.0 else: abs(v) <= 2.0
      if not ok: entries = false
  let (a, b, c) = (rel[0][0], rel[0][1], rel[0][2])
  let (d, e, f) = (rel[1][0], rel[1][1], rel[1][2])
  let (g, h, i) = (rel[2][0], rel[2][1], rel[2][2])
  let det = a * (e * i - f * h) - b * (d * i - f * g) + c * (d * h - e * g)
  var sums = true
  for r in rel:
    let t = r[0] + r[1] + r[2]
    if not (t >= 0.6 and t <= 1.6): sums = false
  entries and sums and det > 0.05

proc correlation(p: openArray[(float64, float64)]): float64 =
  ## Pearson correlation of `(a, b)` pairs (NaN without spread).
  let n = float64(p.len)
  var (sa, sb) = (-0.0, -0.0)
  for v in p: sa += v[0]
  for v in p: sb += v[1]
  let (ma, mb) = (sa / n, sb / n)
  var (sab, saa, sbb) = (0.0, 0.0, 0.0)
  for (a, b) in p:
    sab += (a - ma) * (b - mb)
    saa += (a - ma) * (a - ma)
    sbb += (b - mb) * (b - mb)
  sab / sqrt(saa * sbb)

const CurveBins = 32 ## bins of the tone-curve fit, uniform in `sqrt(x)`

proc fitCurve(samples: openArray[Sample]; robust: openArray[float64]; m: Mat3): Option[ToneCurve] =
  ## Fits the tone curve to `(M · cam, rgb)` pairs of all channels, by weighted bin means in the
  ## square-root domain made monotone (pool adjacent violators).
  var bins: array[CurveBins, array[3, float64]] # weight, Σ w·sqrt(x), Σ w·sqrt(y)
  var total = 0.0
  for si in 0 ..< min(samples.len, robust.len):
    let s = samples[si]
    let x = apply(m, s.cam)
    let w = s.weight * robust[si]
    if w <= 0.0: continue
    for k in 0 .. 2:
      let (xv, yv) = (x[k], s.rgb[k])
      if not (xv > 1e-5 and xv < 1.0 and isFin(yv)): continue
      let (sx, sy) = (sqrt(xv), sqrt(clamp(yv, 0.0, 1.0)))
      let b = min(asUsize(sx * float64(CurveBins)), CurveBins - 1)
      bins[b][0] += w
      bins[b][1] += w * sx
      bins[b][2] += w * sy
      total += w
  if total != total or total <= 0.0: return
  # (weight, sqrt x, sqrt y) of the populated bins, then PAVA on y.
  var pts: seq[array[3, float64]]
  for b in bins:
    if b[0] > total * 2e-4: pts.add [b[0], b[1] / b[0], b[2] / b[0]]
  if pts.len < 4: return
  var blocks: seq[(float64, float64, int)] # (weight, mean y, count)
  for p in pts:
    blocks.add (p[0], p[2], 1)
    while blocks.len >= 2:
      let (b, a) = (blocks[^1], blocks[^2])
      if a[1] <= b[1]: break
      blocks.setLen(blocks.len - 2)
      let w = a[0] + b[0]
      blocks.add (w, (a[0] * a[1] + b[0] * b[1]) / w, a[2] + b[2])
  var i = 0
  for (_, y, n) in blocks:
    for q in i ..< min(i + n, pts.len): pts[q][2] = y
    i += n
  var knots = @[[0.0, 0.0]]
  for p in pts:
    let (x, y) = (p[1] * p[1], clamp(p[2] * p[2], 0.0, 1.0))
    let (px, py) = (knots[^1][0], knots[^1][1])
    if x > px + 1e-6 and x < 1.0 - 1e-6: knots.add [x, rmax(y, py)]
  knots.add [1.0, 1.0]
  newToneCurve(knots)

proc fitMatrix(samples: openArray[Sample]; robust: openArray[float64]; tone: ToneCurve; prior: Mat3; gain, ridge: float64): Mat3 =
  ## Weighted least squares for `M` from `f⁻¹(rgb)` on `cam`, ridge-pulled toward `prior`, then
  ## scaled so the balanced white keeps luminance `gain` (a matrix and a curve could otherwise
  ## trade exposure).
  type Normal = array[3, (Mat3, array[3, float64], float64)] # per row: normal matrix, rhs, level
  var parts: seq[Normal]
  var lo = 0
  while lo < samples.len:
    var acc: Normal
    for si in lo ..< min(lo + Chunk, min(samples.len, robust.len)):
      let s = samples[si]
      let r = robust[si]
      let c = s.cam
      let lv = (c[0] + c[1] + c[2]) / 3.0
      let level = lv * lv
      for i in 0 .. 2:
        let y = s.rgb[i]
        if not (y > 0.0 and y < 0.995): continue
        let (t, slope) = tone.inverseAndSlope(y)
        let q = slope * s.perceptual
        let w = s.weight * r * q * q
        if not (isFin(w) and w > 0.0): continue
        acc[i][2] += w * level
        for j in 0 .. 2:
          for k in 0 .. 2: acc[i][0][j][k] += w * c[j] * c[k]
          acc[i][1][j] += w * c[j] * t
    parts.add acc
    lo += Chunk
  var outM = prior
  for i in 0 .. 2:
    var a: Mat3
    var b: array[3, float64]
    var level = 0.0
    for p in parts:
      for j in 0 .. 2:
        for k in 0 .. 2: a[j][k] += p[i][0][j][k]
        b[j] += p[i][1][j]
      level += p[i][2]
    let lambda = ridge * ridge * level + 1e-12
    for j in 0 .. 2:
      a[j][j] += lambda
      b[j] += lambda * prior[i][j]
    let inv = invert(a)
    if inv.isSome:
      let x = apply(inv.get, b)
      if isFin(x[0]) and isFin(x[1]) and isFin(x[2]): outM[i] = x
  let whiteY = apply(ProToXyzYRow, apply(outM, [1.0, 1.0, 1.0]))[0]
  if isFin(whiteY) and whiteY > 1e-6: scale(outM, gain / whiteY) else: prior

proc updateRobust(samples: openArray[Sample]; robust: var seq[float64]; m: Mat3; tone: ToneCurve) =
  ## Tukey biweights from each sample's L*-scaled residual.
  var err = newSeq[float64](samples.len)
  for i, s in samples:
    let o = render(m, some(tone), s.cam)
    var acc = -0.0
    for k in 0 .. 2:
      let d = o[k] - s.rgb[k]
      acc += d * d
    err[i] = sqrt(acc) * s.perceptual
  var sorted: seq[float64]
  for v in err:
    if isFin(v): sorted.add v
  if sorted.len == 0: return
  sorted.sort(proc(a, b: float64): int = cmp(a, b))
  let median = sorted[sorted.len div 2]
  let c = 4.685 * rmax(1.4826 * median, 0.5)
  for i in 0 ..< min(robust.len, err.len):
    let u = err[i] / c
    robust[i] = if isFin(u) and u < 1.0: (let t = 1.0 - u * u; t * t) else: 0.0

proc fitModel(train: openArray[Sample]; fallback: Mat3; gain, ridge: float64): (Mat3, ToneCurve) =
  ## Alternates tone-curve and matrix fits with Tukey reweighting.
  var m = fallback
  var robust = newSeq[float64](train.len)
  for v in robust.mitems: v = 1.0
  var t = fitCurve(train, robust, m)
  if t.isNone: raise reject("no tone curve could be fitted")
  var tone = t.get
  for _ in 0 ..< 6:
    m = fitMatrix(train, robust, tone, fallback, gain, ridge)
    t = fitCurve(train, robust, m)
    if t.isNone: raise reject("no tone curve could be fitted")
    tone = t.get
    updateRobust(train, robust, m, tone)
  (m, tone)

const Ridges = [0.01, 0.05, 0.15] ## ridge strengths tried in turn (see `fitMatrix`)

proc fitCells(cells: openArray[Cell]; gw: int; baselineExposure: float64): CameraLook =
  let gain = pow(2.0, if isFin(baselineExposure): clamp(baselineExposure, -10.0, 10.0) else: 0.0)
  let fallback = scale(mul(xyzD50ToProphoto(), srgbToXyzD50()), gain)
  let lab = newLab()
  var train, test, all: seq[Sample]
  for k, c in cells:
    var finite = true
    for v in c.cam:
      if not isFin(v): finite = false
    for v in c.rgb:
      if not isFin(v): finite = false
    if not finite: continue
    let s = Sample(cam: c.cam, rgb: c.rgb, lum: c.lum, perceptual: perceptual(c.lum), weight: c.edgeWeight)
    all.add s
    let usable = not c.camClipped and c.refClipped < 0.01 and c.lum > 0.002 and c.cam[0] > 1e-4 and
      c.cam[1] > 1e-4 and c.cam[2] > 1e-4 and c.edgeWeight > 0.05
    if not usable: continue
    let (x, y) = (k mod max(gw, 1), k div max(gw, 1))
    if (x + 2 * y) mod 3 == 0: test.add s else: train.add s
  if train.len < 300 or test.len < 100:
    raise reject("too few usable cells (" & $train.len & " + " & $test.len & " held out)")

  # The reference must show this picture (its lightness follows the raw's) in colour.
  let usable = train & test
  var pairs = newSeq[(float64, float64)](usable.len)
  for i, s in usable:
    pairs[i] = (sqrt(rmax(apply(ProToXyzYRow, apply(fallback, s.cam))[0], 0.0)), sqrt(rmax(s.lum, 0.0)))
  let r = correlation(pairs)
  if r != r or r < 0.7:
    raise reject("the reference does not match the raw picture (lightness correlation " & fixed(r, 2) & ")")
  var coloured = 0
  for s in usable:
    let l = lab.lab(s.rgb)
    if hypot(l[1], l[2]) > 4.0: inc coloured
  if coloured * 50 < usable.len: raise reject("the reference is (nearly) colourless")

  # Weakest ridge first; a stronger pull toward the fallback when the matrix comes out implausible
  # (little colour in the scene, or a rendering a matrix can't follow).
  let heldFallback = meanDeltaE(lab, test, fallback, none(ToneCurve))
  var why = ""
  var found = false
  var m, rel: Mat3
  var tone: ToneCurve
  for ridge in Ridges:
    let (fm, ft) = fitModel(train, fallback, gain, ridge)
    let frel = scale(fm, 1.0 / gain)
    if not plausible(frel):
      why = "implausible matrix " & debugMat3(frel)
      continue
    let heldFit = meanDeltaE(lab, test, fm, some(ft))
    if heldFit != heldFit or heldFit > 0.85 * heldFallback:
      why = "the fit does not improve enough on the neutral fallback (held-out ΔE " & fixed(heldFit, 1) & " vs " &
        fixed(heldFallback, 1) & ")"
      continue
    (m, rel, tone) = (fm, frel, ft)
    found = true
    break
  if not found: raise reject(why)
  let deltaE = meanDeltaE(lab, all, m, some(tone))
  let deltaEFallback = meanDeltaE(lab, all, fallback, none(ToneCurve))
  let toXyz = mul(rgbToXyz(Romm, D50Xy), rel)
  result = CameraLook(toXyz: toXyz, tone: some(tone), deltaE: deltaE, deltaEFallback: deltaEFallback)
  if not result.isUsable: raise reject("the fitted matrix is not usable")

proc fitLook*(s: Sensor; opts: DevelopOptions; reference: Reference): CameraLook =
  ## Fits a `CameraLook` so that developing `s` with `opts` (white balance, baseline exposure)
  ## matches the camera's `reference` rendering of it. Raises `LookRejected`.
  let (gw, cells) = grid(s, opts, reference)
  fitCells(cells, gw, s.baselineExposure)
