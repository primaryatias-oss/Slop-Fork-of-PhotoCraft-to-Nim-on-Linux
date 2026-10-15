## Adjustment-layer math over straight-alpha RGBA buffers: Brightness/Contrast (modern and
## legacy), Levels, Curves (smooth and pencil, RGB/CMYK/Lab tone spaces), Exposure, Vibrance,
## Hue/Saturation with ranges, Color Balance, Black & White, Photo Filter, Channel Mixer, Color
## Lookup (3D LUTs), Invert, Posterize, Threshold, Gradient Map and Selective Color. The curve
## shapes were fitted to Photoshop ground truth in the Rust crate and are reproduced exactly.

import std/[math, algorithm]
import ../doc, ../color/convert, ../color/blend, ./buffer

const
  LutSize* = 4096
  LegacyPivot* = 128'f32 / 255'f32
  BalanceLevels = 0.39'f32
  VibranceGrey* = [0.288'f32, 0.712]
  VibranceBoost* = [0.5001'f32, 0.0887, 0.4374, 0.7573, -0.8113, 0.5154, 2.8026]

type
  TransferKind* = enum tkSrgb, tkGamma
  Transfer* = object
    kind*: TransferKind
    gamma*: float32
  Lut = seq[float32]

proc srgbTransfer*(): Transfer = Transfer(kind: tkSrgb)
proc gammaTransfer*(g: float32): Transfer = Transfer(kind: tkGamma, gamma: g)

proc transferForDocument*(mode: ColorMode; depth: SampleType): Transfer =
  if depth == stF32: return gammaTransfer(1)
  if mode in {cmGrayscale, cmDuotone, cmBitmap}: gammaTransfer(1.732) else: srgbTransfer()

proc forExposure*(t: Transfer): Transfer = (if t.kind == tkSrgb: gammaTransfer(2.2) else: t)

proc decode(t: Transfer; v: float32): float32 =
  if t.kind == tkSrgb: srgbToLinear(max(v, 0))
  elif t.gamma == 1: max(v, 0)
  else: pow(max(v, 0), t.gamma)

proc encode(t: Transfer; v: float32): float32 =
  if t.kind == tkSrgb: linearToSrgb(max(v, 0))
  elif t.gamma == 1: max(v, 0)
  else: pow(max(v, 0), 1 / t.gamma)

proc remE(a, b: float32): float32 =
  result = a mod b
  if result < 0: result += b

proc lut(table: Lut; v: float32): float32 {.inline.} =
  let x = clamp(v, 0'f32, 1'f32) * float32(table.len - 1)
  let i = int(floor(x))
  let j = min(i + 1, table.len - 1)
  let f = x - float32(i)
  table[i] * (1 - f) + table[j] * f

proc identityLut(): Lut =
  result = newSeq[float32](LutSize)
  for k in 0 ..< LutSize: result[k] = float32(k) / float32(LutSize - 1)

# ---------------------------------------------------------------- tone curves

proc levelsQ*(ch: LevelsChannel; v: float32; quantum: float32 = 0): float32 =
  ## Photoshop's Levels: input range, gamma (a p-norm blend fitted to Photoshop above 1),
  ## output range. `quantum` > 0 rounds like an integer document.
  let rng = max(ch.inWhite - ch.inBlack, 1e-6'f32)
  var u: float32
  if quantum >= 1 and quantum == quantum and quantum != Inf:
    let vq = round(v * quantum) / quantum
    u = floor(clamp((vq - ch.inBlack) / rng, 0'f32, 1'f32) * quantum + 0.5'f32 + 1e-3'f32) / quantum
  else: u = clamp((v - ch.inBlack) / rng, 0'f32, 1'f32)
  let g = max(ch.gamma, 0.01'f32)
  var t: float32
  if g > 1:
    let power = pow(u, 1 / g)
    let line = 0.93'f32 * pow(2'f32, g) * u
    if power <= 1e-6'f32 or line <= 1e-6'f32: t = min(power, line)
    else:
      const p = 10'f32
      t = pow(pow(power, -p) + pow(line, -p), -1 / p)
  else: t = pow(u, 1 / g)
  ch.outBlack + t * (ch.outWhite - ch.outBlack)

proc levelsFloat*(ch: LevelsChannel; v: float32): float32 =
  let t0 = (v - ch.inBlack) / max(ch.inWhite - ch.inBlack, 1e-6'f32)
  let t = if ch.gamma == 1: t0 else: (if t0 < 0: -1'f32 else: 1'f32) * pow(abs(t0), 1 / max(ch.gamma, 0.01'f32))
  ch.outBlack + t * (ch.outWhite - ch.outBlack)

proc curveLut*(points: openArray[CurvePoint]): Lut =
  ## Natural cubic spline through the points (Photoshop's smooth curve), sampled at 4096 levels.
  var pts: seq[(float32, float32)]
  for p in points: pts.add (p.input, p.output)
  pts.sort(proc (a, b: (float32, float32)): int = cmp(a[0], b[0]))
  var dd: seq[(float32, float32)]
  for p in pts:
    if dd.len == 0 or abs(dd[^1][0] - p[0]) >= 1e-6'f32: dd.add p
  pts = dd
  if pts.len < 2: return identityLut()
  let n = pts.len
  var m2 = newSeq[float64](n)
  if n > 2:
    var c = newSeq[float64](n)
    var d = newSeq[float64](n)
    for i in 1 ..< n - 1:
      let (h0, h1) = (float64(pts[i][0] - pts[i - 1][0]), float64(pts[i + 1][0] - pts[i][0]))
      let a = h0 / 6
      let b = (h0 + h1) / 3
      let cc = h1 / 6
      let r = float64(pts[i + 1][1] - pts[i][1]) / h1 - float64(pts[i][1] - pts[i - 1][1]) / h0
      let denom = b - a * c[i - 1]
      c[i] = cc / denom
      d[i] = (r - a * d[i - 1]) / denom
    for i in countdown(n - 2, 1): m2[i] = d[i] - c[i] * m2[i + 1]
  result = newSeq[float32](LutSize)
  var seg = 0
  for k in 0 ..< LutSize:
    let xv = float32(k) / float32(LutSize - 1)
    if xv <= pts[0][0]: result[k] = clamp(pts[0][1], 0'f32, 1'f32)
    elif xv >= pts[^1][0]: result[k] = clamp(pts[^1][1], 0'f32, 1'f32)
    else:
      while seg < n - 2 and xv > pts[seg + 1][0]: inc seg
      let (x0, y0) = (float64(pts[seg][0]), float64(pts[seg][1]))
      let (x1, y1) = (float64(pts[seg + 1][0]), float64(pts[seg + 1][1]))
      let h = x1 - x0
      let t = float64(xv)
      let a = (x1 - t) / h
      let b = (t - x0) / h
      let y = a * y0 + b * y1 + ((a * a * a - a) * m2[seg] + (b * b * b - b) * m2[seg + 1]) * h * h / 6
      result[k] = clamp(float32(y), 0'f32, 1'f32)

proc curveMapLut*(points: openArray[CurvePoint]): Lut =
  ## Pencil curves: straight lines between the points.
  var pts: seq[(float32, float32)]
  for p in points:
    if p.input == p.input and p.output == p.output: pts.add (p.input, p.output)
  pts.sort(proc (a, b: (float32, float32)): int = cmp(a[0], b[0]))
  var dd: seq[(float32, float32)]
  for p in pts:
    if dd.len == 0 or abs(dd[^1][0] - p[0]) >= 1e-6'f32: dd.add p
  pts = dd
  if pts.len == 0: return identityLut()
  result = newSeq[float32](LutSize)
  var seg = 0
  for k in 0 ..< LutSize:
    let xv = float32(k) / float32(LutSize - 1)
    if xv <= pts[0][0]: result[k] = clamp(pts[0][1], 0'f32, 1'f32)
    elif xv >= pts[^1][0]: result[k] = clamp(pts[^1][1], 0'f32, 1'f32)
    else:
      while seg + 1 < pts.len and pts[seg + 1][0] < xv: inc seg
      if seg + 1 >= pts.len:
        result[k] = clamp(pts[^1][1], 0'f32, 1'f32)
      else:
        let (x0, y0) = pts[seg]
        let (x1, y1) = pts[seg + 1]
        let t = if x1 > x0: (xv - x0) / (x1 - x0) else: 0'f32
        result[k] = clamp(y0 + (y1 - y0) * t, 0'f32, 1'f32)

proc curveLutMode*(points: openArray[CurvePoint]; pencil: bool): Lut =
  if pencil: curveMapLut(points) else: curveLut(points)

proc contourCurveLut*(points: openArray[CurvePoint]; corners: openArray[bool]): Lut =
  ## Contour curves: smooth segments split at corner points.
  var pts: seq[(CurvePoint, bool)]
  var anyCorner = false
  for i, p in points:
    let c = i < corners.len and corners[i]
    pts.add (p, c)
    if c: anyCorner = true
  if not anyCorner: return curveLut(points)
  pts.sort(proc (a, b: (CurvePoint, bool)): int = cmp(a[0].input, b[0].input))
  var dd: seq[(CurvePoint, bool)]
  for p in pts:
    if dd.len == 0 or abs(dd[^1][0].input - p[0].input) >= 1e-6'f32: dd.add p
  pts = dd
  var start = 0
  let last = pts.len - 1
  for i in 1 .. last:
    if not (pts[i][1] or i == last): continue
    var seg: seq[CurvePoint]
    for k in start .. i: seg.add pts[k][0]
    let frm = seg[0].input
    let l = curveLut(seg)
    if result.len == 0: result = l
    else:
      for k in 0 ..< LutSize:
        if float32(k) / float32(LutSize - 1) >= frm: result[k] = l[k]
    start = i
  if result.len == 0: result = curveLut(points)

proc adjustmentQuantum*(depth: SampleType): float32 =
  ## Steps per unit of an integer document's samples (0 for float documents).
  case depth
  of stU8: 255
  of stU16: 32768
  of stF32: 0

proc toneLutsQ*(adj: Adjustment; quantum: float32 = 0): array[4, Lut] =
  case adj.kind
  of akLevels:
    let ident = defaultLevelsChannel()
    let m = if adj.space == tsLab: ident else: adj.levelsMaster
    proc row(c: LevelsChannel): Lut =
      result = newSeq[float32](LutSize)
      for k in 0 ..< LutSize: result[k] = levelsQ(m, levelsQ(c, float32(k) / float32(LutSize - 1), quantum), quantum)
    [row(adj.levelsPerChannel[0]), row(adj.levelsPerChannel[1]), row(adj.levelsPerChannel[2]),
     row(if adj.space == tsCmyk: adj.levelsBlack else: ident)]
  of akCurves:
    let m = if adj.space == tsLab: curveLut([]) else: curveLutMode(adj.curvesMaster, adj.pencil)
    proc row(c: seq[CurvePoint]): Lut =
      result = curveLutMode(c, adj.pencil)
      for v in result.mitems: v = lut(m, v)
    [row(adj.curvesPerChannel[0]), row(adj.curvesPerChannel[1]), row(adj.curvesPerChannel[2]),
     row(if adj.space == tsCmyk: adj.curvesBlack else: @[])]
  else: [identityLut(), identityLut(), identityLut(), identityLut()]

proc toneLutsDepth*(adj: Adjustment; depth: SampleType; hasDepth: bool): array[4, Lut] =
  if adj.kind == akLevels and adj.space == tsRgb and hasDepth and depth == stF32:
    proc row(c: LevelsChannel): Lut =
      result = newSeq[float32](LutSize)
      for k in 0 ..< LutSize:
        result[k] = clamp(levelsFloat(adj.levelsMaster, levelsFloat(c, float32(k) / float32(LutSize - 1))), 0'f32, 1'f32)
    return [row(adj.levelsPerChannel[0]), row(adj.levelsPerChannel[1]), row(adj.levelsPerChannel[2]), identityLut()]
  toneLutsQ(adj, if hasDepth: adjustmentQuantum(depth) else: 0)

proc toneInSpace(space: ToneSpace; luts: array[4, Lut]; c: array[3, float32]): array[3, float32] =
  var before, after: array[3, float32]
  case space
  of tsCmyk:
    let ink = rgbToCmyk(c)
    var o: array[4, float32]
    for i in 0..3: o[i] = 1 - lut(luts[i], 1 - ink[i])
    if o == ink: return c
    before = cmykToRgb(ink)
    after = cmykToRgb(o)
  of tsLab:
    let l = srgbToLab(c)
    let n = [l[0] / 100, (l[1] + 128) / 255, (l[2] + 128) / 255]
    var o: array[3, float32]
    for i in 0..2: o[i] = lut(luts[i], n[i])
    if o == n: return c
    proc back(v: array[3, float32]): array[3, float32] = labToSrgb([v[0] * 100, v[1] * 255 - 128, v[2] * 255 - 128])
    before = back(n)
    after = back(o)
  of tsRgb:
    for i in 0..2: result[i] = lut(luts[i], c[i])
    return
  for i in 0..2: result[i] = clamp(c[i] + after[i] - before[i], 0'f32, 1'f32)

# ---------------------------------------------------------------- colour helpers

proc rgbToHsl*(c: array[3, float32]): (float32, float32, float32) =
  let mx = max(c[0], max(c[1], c[2]))
  let mn = min(c[0], min(c[1], c[2]))
  let l = (mx + mn) / 2
  if abs(mx - mn) < 1e-7'f32: return (0'f32, 0'f32, l)
  let d = mx - mn
  let s = if l > 0.5'f32: d / (2 - mx - mn) else: d / (mx + mn)
  let h = if mx == c[0]: remE((c[1] - c[2]) / d, 6)
          elif mx == c[1]: (c[2] - c[0]) / d + 2
          else: (c[0] - c[1]) / d + 4
  (h / 6, s, l)

proc hslToRgb*(h, s, l: float32): array[3, float32] =
  if s <= 0: return [l, l, l]
  let q = if l < 0.5'f32: l * (1 + s) else: l + s - l * s
  let p = 2 * l - q
  proc f(t0: float32): float32 =
    let t = remE(t0, 1)
    if t < 1'f32 / 6: p + (q - p) * 6 * t
    elif t < 0.5'f32: q
    elif t < 2'f32 / 3: p + (q - p) * (2'f32 / 3 - t) * 6
    else: p
  [f(h + 1'f32 / 3), f(h), f(h - 1'f32 / 3)]

proc hueRangeTables*(ranges: array[6, HueRange]): array[3, Lut] =
  for i in 0..2: result[i] = newSeq[float32](LutSize)
  for k in 0 ..< LutSize:
    let deg = float32(k) / float32(LutSize - 1) * 360
    var acc: array[3, float32]
    for r in ranges:
      if r.isNeutral: continue
      let w = r.weight(deg)
      acc[0] += w * r.hue
      acc[1] += w * r.saturation / 100
      acc[2] += w * r.lightness / 100
    for i in 0..2: result[i][k] = acc[i]

proc hueSaturation*(c: array[3, float32]; hue, s, l: float32; colorize: bool): array[3, float32] =
  var (hh, ss, ll) = rgbToHsl(c)
  if colorize:
    hh = remE(hue, 360) / 360
    ss = min(abs(s), 1)
  else:
    hh = remE(hh + hue / 360, 1)
    ss = clamp(ss * (1 + s), 0'f32, 1'f32)
  result = hslToRgb(hh, ss, ll)
  if l > 0:
    for v in result.mitems: v = v + (1 - v) * l
  elif l < 0:
    for v in result.mitems: v = v * (1 + l)

proc blackWhiteGray*(c: array[3, float32]; weights: array[6, float32]): float32 =
  let (r, g, b) = (c[0], c[1], c[2])
  let (mx, mn) = (max(r, max(g, b)), min(r, min(g, b)))
  let mid = r + g + b - mx - mn
  let primary = if r >= g and r >= b: 0 elif g >= b: 2 else: 4
  let secondary = if b <= r and b <= g: 1 elif r <= g: 3 else: 5
  clamp(mn + (mid - mn) * weights[secondary] / 100 + (mx - mid) * weights[primary] / 100, 0'f32, 1'f32)

proc bayer4*(x, y: int): float32 =
  const M = [0'f32, 8, 2, 10, 12, 4, 14, 6, 3, 11, 1, 9, 15, 7, 13, 5]
  (M[(y and 3) * 4 + (x and 3)] + 0.5'f32) / 16 - 0.5'f32

proc lut3dSample*(table: openArray[float32]; n: int; c: array[3, float32]; tetrahedral: bool): array[3, float32] =
  let m = float32(n - 1)
  var i0: array[3, int]
  var f: array[3, float32]
  for k in 0..2:
    let p = clamp(c[k], 0'f32, 1'f32) * m
    i0[k] = min(int(floor(p)), n - 2)
    f[k] = p - float32(i0[k])
  template at(dr, dg, db: int): array[3, float32] =
    let idx = (((i0[2] + db) * n + i0[1] + dg) * n + i0[0] + dr) * 3
    [table[idx], table[idx + 1], table[idx + 2]]
  proc mix(w: openArray[(float32, array[3, float32])]): array[3, float32] =
    for (a, v) in w:
      for k in 0..2: result[k] += a * v[k]
  let (fr, fg, fb) = (f[0], f[1], f[2])
  if tetrahedral:
    let (c000, c111) = (at(0, 0, 0), at(1, 1, 1))
    if fr > fg:
      if fg > fb: mix([(1 - fr, c000), (fr - fg, at(1, 0, 0)), (fg - fb, at(1, 1, 0)), (fb, c111)])
      elif fr > fb: mix([(1 - fr, c000), (fr - fb, at(1, 0, 0)), (fb - fg, at(1, 0, 1)), (fg, c111)])
      else: mix([(1 - fb, c000), (fb - fr, at(0, 0, 1)), (fr - fg, at(1, 0, 1)), (fg, c111)])
    elif fb > fg: mix([(1 - fb, c000), (fb - fg, at(0, 0, 1)), (fg - fr, at(0, 1, 1)), (fr, c111)])
    elif fb > fr: mix([(1 - fg, c000), (fg - fb, at(0, 1, 0)), (fb - fr, at(0, 1, 1)), (fr, c111)])
    else: mix([(1 - fg, c000), (fg - fr, at(0, 1, 0)), (fr - fb, at(1, 1, 0)), (fb, c111)])
  else:
    proc lerp3(a, b: array[3, float32]; t: float32): array[3, float32] =
      for k in 0..2: result[k] = a[k] + (b[k] - a[k]) * t
    let c00 = lerp3(at(0, 0, 0), at(1, 0, 0), fr)
    let c10 = lerp3(at(0, 1, 0), at(1, 1, 0), fr)
    let c01 = lerp3(at(0, 0, 1), at(1, 0, 1), fr)
    let c11 = lerp3(at(0, 1, 1), at(1, 1, 1), fr)
    lerp3(lerp3(c00, c10, fg), lerp3(c01, c11, fg), fb)

proc colorBalanceChannel*(tones: array[3, array[3, float32]]; preserveLuminosity: bool; ch: int; v0: float32): float32 =
  let (shadows, midtones, highlights) = (tones[0], tones[1], tones[2])
  template black(v, b: float32): float32 = max((v - b) / (1 - b), 0'f32)
  template white(v, w: float32): float32 = min(v / w, 1'f32)
  var v = clamp(v0, 0'f32, 1'f32)
  for (tone, sliders) in [(2, highlights), (0, shadows), (1, midtones)]:
    var a: array[3, float32]
    for k in 0..2: a[k] = clamp(sliders[k] / 100, -1'f32, 1'f32)
    if preserveLuminosity and tone != 1:
      let own = if ch < 3: a[ch] else: 0'f32
      let (lo, hi) = (min(a[0], min(a[1], a[2])), max(a[0], max(a[1], a[2])))
      v = if tone == 0: black(v, BalanceLevels * (hi - own)) else: white(v, 1 - BalanceLevels * (own - lo))
      continue
    for src in 0..2:
      let a = clamp(sliders[src] / 100, -1'f32, 1'f32)
      let own = src == ch
      if a == 0 or (not preserveLuminosity and not own): continue
      if tone == 1 and not preserveLuminosity: v = pow(v, pow(2'f32, -a))
      elif tone == 1: v = pow(v, pow(2'f32, (if own: -a else: a) / 2))
      elif tone == 0 and not preserveLuminosity:
        v = if a < 0: pow(black(v, BalanceLevels * -a), pow(2'f32, -a / 2)) else: pow(v, pow(2'f32, -a / 2))
      elif tone == 2 and not preserveLuminosity:
        v = if a > 0: pow(white(v, 1 - BalanceLevels * a), pow(2'f32, -a / 2)) else: pow(v, pow(2'f32, -a / 2))
  v

proc selectiveColorWeights*(c: array[3, float32]): array[9, float32] =
  let mx = max(c[0], max(c[1], c[2]))
  let mn = min(c[0], min(c[1], c[2]))
  let mid = c[0] + c[1] + c[2] - mx - mn
  template top(i: int): float32 = (if c[i] >= mx: mx - mid else: 0'f32)
  template bottom(i: int): float32 = (if c[i] <= mn: mid - mn else: 0'f32)
  [top(0), bottom(2), top(1), bottom(0), top(2), bottom(1), max((mn - 0.5'f32) * 2, 0'f32),
   clamp(1 - abs(mx - 0.5'f32) - abs(mn - 0.5'f32), 0'f32, 1'f32), max((0.5'f32 - mx) * 2, 0'f32)]

proc selectiveColor*(c: array[3, float32]; relative: bool; adj: array[9, array[4, float32]]): array[3, float32] =
  let w = selectiveColorWeights(c)
  var delta: array[3, float32]
  for r in 0 ..< 9:
    if w[r] <= 0: continue
    for i in 0..2:
      let ink = 1 - c[i]
      let a = (adj[r][i] + adj[r][3]) / 100
      delta[i] += (if relative: a * ink else: a) * w[r]
  for i in 0..2: result[i] = clamp(c[i] - delta[i], 0'f32, 1'f32)

proc photoFilterMatrix*(color: array[3, float32]; density: float32; normalizeY: bool): array[3, array[3, float32]] =
  let d = if density == density and abs(density) != Inf: clamp(density, 0'f32, 1'f32) else: 0'f32
  var lin: array[3, float32]
  for i in 0..2: lin[i] = srgbToLinear(clamp(color[i], 0'f32, 1'f32))
  let xyz = mat3Mul(SrgbToXyzD50, lin)
  var s: array[3, float32]
  for i in 0..2: s[i] = 1 - d + d * xyz[i] / D50f[i]
  if normalizeY and s[1] > 1e-6'f32:
    let y = s[1]
    for v in s.mitems: v /= y
  for r in 0..2:
    for c in 0..2:
      for k in 0..2: result[r][c] += XyzD50ToSrgb[r][k] * s[k] * SrgbToXyzD50[k][c]

proc hsvHue(c: array[3, float32]; mx, mn: float32): float32 =
  let d = mx - mn
  let h = if mx == c[0]: remE((c[1] - c[2]) / d, 6) elif mx == c[1]: (c[2] - c[0]) / d + 2 else: (c[0] - c[1]) / d + 4
  h * 60

proc vibranceSkin(h: float32): float32 =
  if h <= 30: 1 elif h < 45: (45 - h) / 15 elif h < 300: 0 else: (h - 300) / 60

proc vibranceLin*(c: array[3, float32]; v: float32): array[3, float32] =
  let mx = max(c[0], max(c[1], c[2]))
  let mn = min(c[0], min(c[1], c[2]))
  if mx <= 0 or mx - mn <= 1e-9'f32: return c
  let t = 1 - mn / mx
  let shape = t * (1 - t) * (2 - t + t * t) * mx * max(1 - mx, 0)
  var t2, v2: float32
  if v < 0:
    let a = -v
    t2 = t * (1 - a / 4) * (1 - a + a * t * (1 + t) / 2)
    v2 = mx - a * shape
  else:
    let w = vibranceSkin(hsvHue(c, mx, mn))
    let a = v * (1 - w) + 0.64'f32 * pow(v, 1.6'f32) * w
    let b = VibranceBoost
    let boost = (b[0] * a + b[1] * a * a) * pow(t, b[2]) * pow(1 - t, b[3]) * exp((b[4] + b[5] * a + b[6] * (1 - t)) * max(1 - mx, 0))
    t2 = min(t * (1 + boost), 1)
    v2 = mx + v / 4 * shape
  let m2 = v2 * (1 - t2)
  let k = (v2 - m2) / (mx - mn)
  for i in 0..2: result[i] = m2 + (c[i] - mn) * k

proc vibrancePx*(c: array[3, float32]; v, s: float32; transfer: Transfer; hi: float32): array[3, float32] =
  var l: array[3, float32]
  for i in 0..2: l[i] = transfer.decode(c[i])
  if v != 0: l = vibranceLin(l, v)
  if s != 0:
    let g = VibranceGrey[0] * l[0] + VibranceGrey[1] * l[1]
    for x in l.mitems: x = clamp(g + (1 + s) * (x - g), 0'f32, hi)
  for i in 0..2: result[i] = transfer.encode(clamp(l[i], 0'f32, hi))

proc modernBrightness*(v0, brightness: float32): float32 =
  if brightness == 0: return v0
  let s = pow(1.375'f32, brightness / 50)
  let p = if brightness >= 0: max(4.5'f32 - 0.013'f32 * brightness, 2'f32) else: 5 - 0.072'f32 * brightness
  let v = clamp(v0, 0'f32, 1'f32)
  clamp(s * v + (1 - s) * pow(v, p), 0'f32, 1'f32)

proc modernContrast*(v0, contrast: float32): float32 =
  if contrast == 0: return v0
  let k = contrast / 128
  let (e, mid) = (1 - k, 1 + k)
  proc half(t: float32): float32 =
    e * 0.5'f32 * (t * t * t - 2 * t * t + t) + (-2 * t * t * t + 3 * t * t) * 0.5'f32 + mid * 0.5'f32 * (t * t * t - t * t)
  let v = clamp(v0, 0'f32, 1'f32)
  if v <= 0.5'f32: half(v / 0.5'f32) else: 1 - half((1 - v) / 0.5'f32)

proc posterize*(v: float32; levels: uint32): float32 =
  let n = float32(clamp(levels, 2'u32, 255'u32))
  let x = round(clamp(v, 0'f32, 1'f32) * 255)
  let bin = min(floor(x * n / 256), n - 1)
  floor(bin * 255 / (n - 1)) / 255

proc gradientAt(stops: seq[(float32, array[3, float32])]; t: float32): array[3, float32] =
  if stops.len == 0: return [t, t, t]
  if stops.len == 1: return stops[0][1]
  if t <= stops[0][0]: return stops[0][1]
  for i in 0 ..< stops.len - 1:
    let (a, b) = (stops[i], stops[i + 1])
    if t <= b[0]:
      let k = if b[0] > a[0]: (t - a[0]) / (b[0] - a[0]) else: 0'f32
      for c in 0..2: result[c] = a[1][c] + (b[1][c] - a[1][c]) * k
      return
  stops[^1][1]

# ---------------------------------------------------------------- apply

template eachPx(buf: var Buffer; body: untyped) =
  ## Runs `body` for every non-transparent pixel with `c` (its RGB) in scope; `body` sets `o`.
  for p {.inject.} in buf.px.mitems:
    if p[3] <= 0: continue
    let c {.inject.} = [p[0], p[1], p[2]]
    var o {.inject.}: array[3, float32]
    body
    p[0] = o[0]; p[1] = o[1]; p[2] = o[2]

proc applyDepth*(adj: Adjustment; buf: var Buffer; transfer: Transfer; depth: SampleType; hasDepth = true) =
  ## Applies `adj` to every non-transparent pixel of `buf` in place.
  let f32doc = hasDepth and depth == stF32
  case adj.kind
  of akInvert:
    eachPx(buf):
      for i in 0..2: o[i] = 1 - c[i]
  of akThreshold:
    let t = round(adj.level * 255)
    eachPx(buf):
      let v = if round(rgbToGray(c) * 255) >= t: 1'f32 else: 0'f32
      o = [v, v, v]
  of akPosterize:
    eachPx(buf):
      for i in 0..2: o[i] = posterize(c[i], adj.levels)
  of akBrightnessContrast:
    if adj.legacy:
      let b = adj.brightness / 255
      let cc = clamp(adj.contrast, -100'f32, 99'f32)
      let k = if cc >= 0: 1 / (1 - cc / 100) else: 1 + cc / 100
      let (pre, post) = if cc >= 0: (b, 0'f32) else: (0'f32, b)
      eachPx(buf):
        for i in 0..2: o[i] = clamp((c[i] + pre - LegacyPivot) * k + LegacyPivot + post, 0'f32, 1'f32)
    else:
      let (b, cc) = (adj.brightness, adj.contrast)
      eachPx(buf):
        for i in 0..2: o[i] = clamp(modernContrast(modernBrightness(c[i], b), cc), 0'f32, 1'f32)
  of akExposure:
    let m = pow(2'f32, adj.exposure)
    let g = max(adj.gamma, 0.01'f32)
    let tr = transfer.forExposure
    let off = adj.offset
    eachPx(buf):
      for i in 0..2:
        if f32doc:
          let lin = c[i] * m + off
          o[i] = if g == 1: lin else: (if lin < 0: -1'f32 else: 1'f32) * pow(abs(lin), 1 / g)
        else:
          var lin = max(tr.decode(c[i]) * m + off, 0)
          if g != 1: lin = pow(lin, 1 / g)
          o[i] = clamp(tr.encode(lin), 0'f32, 1'f32)
  of akLevels, akCurves:
    if adj.kind == akLevels and adj.space == tsRgb and f32doc:
      let mi = adj.levelsMaster == defaultLevelsChannel()
      var ci: array[3, bool]
      for i in 0..2: ci[i] = adj.levelsPerChannel[i] == defaultLevelsChannel()
      if not mi or not (ci[0] and ci[1] and ci[2]):
        eachPx(buf):
          for i in 0..2:
            let v = if ci[i]: c[i] else: levelsFloat(adj.levelsPerChannel[i], c[i])
            o[i] = if mi: v else: levelsFloat(adj.levelsMaster, v)
      return
    let luts = toneLutsDepth(adj, depth, hasDepth)
    var identity: array[3, bool]
    if adj.kind == akCurves and adj.space == tsRgb and f32doc:
      for i in 0..2: identity[i] = isIdentityCurve(adj.curvesMaster) and isIdentityCurve(adj.curvesPerChannel[i])
    if adj.space == tsRgb:
      eachPx(buf):
        for i in 0..2: o[i] = (if identity[i]: c[i] else: lut(luts[i], c[i]))
    else:
      let sp = adj.space
      eachPx(buf):
        o = toneInSpace(sp, luts, c)
  of akHueSaturation:
    var useTable = false
    if not adj.colorize:
      for r in adj.ranges:
        if not r.isNeutral: useTable = true
    let table = if useTable: hueRangeTables(adj.ranges) else: [newSeq[float32](), newSeq[float32](), newSeq[float32]()]
    eachPx(buf):
      var dh, ds, dl: float32
      if useTable:
        let h0 = rgbToHsl(c)[0]
        let chroma = min((max(c[0], max(c[1], c[2])) - min(c[0], min(c[1], c[2]))) * 4, 1'f32)
        dh = lut(table[0], h0) * chroma
        ds = lut(table[1], h0) * chroma
        dl = lut(table[2], h0) * chroma
      o = hueSaturation(c, adj.hue + dh, clamp(adj.saturation / 100 + ds, -1'f32, 1'f32),
                        clamp(adj.lightness / 100 + dl, -1'f32, 1'f32), adj.colorize)
  of akVibrance:
    let (v, s) = (adj.vibrance / 100, adj.saturation / 100)
    let hi = if f32doc: Inf.float32 else: 1'f32
    eachPx(buf):
      o = vibrancePx(c, v, s, transfer, hi)
  of akChannelMixer:
    let mtx = adj.matrix
    eachPx(buf):
      for i in 0..2:
        let row = mtx[if adj.monochrome: 0 else: i]
        o[i] = clamp(row[0] * c[0] + row[1] * c[1] + row[2] * c[2] + row[3], 0'f32, 1'f32)
  of akPhotoFilter:
    let m = photoFilterMatrix(adj.filterColor, adj.density, adj.preserveLuminosity and f32doc)
    let setL = adj.preserveLuminosity and not f32doc
    eachPx(buf):
      var lin: array[3, float32]
      for i in 0..2: lin[i] = transfer.decode(c[i])
      var f = mat3Mul(m, lin)
      for v in f.mitems: v = transfer.encode(v)
      if setL: f = setLum(f, lum(c))
      for i in 0..2: o[i] = clamp(f[i], 0'f32, 1'f32)
  of akBlackWhite:
    eachPx(buf):
      let g = blackWhiteGray(c, adj.weights)
      if adj.tint.isSome:
        for i in 0..2: o[i] = clamp(g * adj.tint.get[i] * 2, 0'f32, 1'f32) * 0.5'f32 + g * 0.5'f32
      else: o = [g, g, g]
  of akGradientMap:
    let w = max(buf.rect.width, 1)
    for i, p in buf.px.mpairs:
      if p[3] <= 0: continue
      var t = rgbToGray([p[0], p[1], p[2]])
      if adj.reverse: t = 1 - t
      var o = gradientAt(adj.gmStops, t)
      if adj.dither:
        let d = bayer4(buf.rect.x0 + i mod w, buf.rect.y0 + i div w) / 255
        for v in o.mitems: v = clamp(v + d, 0'f32, 1'f32)
      p[0] = o[0]; p[1] = o[1]; p[2] = o[2]
  of akColorBalance:
    let tones = [adj.shadows, adj.midtones, adj.highlights]
    let pl = adj.preserveLuminosity
    eachPx(buf):
      for i in 0..2: o[i] = colorBalanceChannel(tones, pl, i, c[i])
  of akSelectiveColor:
    eachPx(buf):
      o = selectiveColor(c, adj.relative, adj.selective)
  of akColorLookup:
    let ln = lut3dLen(adj.lutSize)
    if adj.lut.isNil or ln.isNone or adj.lut.data.len < ln.get: return
    let n = int(adj.lutSize)
    let w = max(buf.rect.width, 1)
    for i, p in buf.px.mpairs:
      if p[3] <= 0: continue
      var o = lut3dSample(adj.lut.data, n, [p[0], p[1], p[2]], adj.tetrahedral)
      if adj.dither:
        let d = bayer4(buf.rect.x0 + i mod w, buf.rect.y0 + i div w) / 255
        for v in o.mitems: v = clamp(v + d, 0'f32, 1'f32)
      p[0] = o[0]; p[1] = o[1]; p[2] = o[2]
  of akUnsupported: discard

proc apply*(adj: Adjustment; buf: var Buffer) = applyDepth(adj, buf, srgbTransfer(), stU8, false)
