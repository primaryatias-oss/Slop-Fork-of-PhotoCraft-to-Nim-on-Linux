## Raw development (port of `raw/src/develop.rs`): linearize, subtract black, scale to white,
## white balance, clip highlights, demosaic, camera → XYZ (D50) → linear ProPhoto, exposure,
## gamma 1.8 encode to 16 bits, orientation.

import std/[options, math]
import ../rmath
import ./tiff, ./color, ./sensor, ./opcodes, ./demosaic, ./tone, ./rutil

type
  WbKind* = enum
    wbAsShot      ## the camera's as-shot white balance when the file records one, else automatic
    wbAuto        ## grey-world estimate from the image
    wbMultipliers ## explicit R, G, B multipliers applied to the camera channels

  WhiteBalance* = object
    ## White balance choice (the Rust enum; `multipliers` is used by `wbMultipliers` only).
    kind*: WbKind
    multipliers*: array[3, float64]

  DevelopOptions* = object
    ## Development settings. Start from `defaultDevelopOptions()` (Nim's zero value differs).
    demosaic*: Demosaic
    whiteBalance*: WhiteBalance
    exposure*: float64 ## exposure compensation in EV, added to the file's baseline exposure
    orient*: bool      ## apply the file's orientation (rotate / flip the pixels)
    limits*: RawLimits
    cameraLook*: Option[CameraLook]
      ## A colour rendering fitted to the camera's own JPEG (`fitLook`). When set it replaces the
      ## file's colour calibration (or the neutral fallback) and adds its tone curve.

  RawInfo* = object
    ## What was developed and how.
    format*: RawFormat
    make*, model*: Option[string]
    sensorWidth*, sensorHeight*: int
    cfa*: Option[string]
      ## CFA pattern at the crop origin: `"RGGB"` for Bayer; other patterns list their whole
      ## repeat row by row, rows separated by `/`; none for LinearRaw.
    wbMultipliers*: array[3, float64] ## white-balance multipliers used (normalized to a minimum of 1)
    orientation*: uint16
    baselineExposure*: float64

  Developed* = object
    ## A developed image: 16-bit RGB in ProPhoto RGB (ROMM primaries, D50, gamma 1.8).
    width*, height*: uint32
    rgb*: seq[uint16] ## interleaved RGB, `width * height * 3` samples
    info*: RawInfo
    warnings*: seq[string] ## notes about anything approximated or not applied

proc asShotWb*(): WhiteBalance = WhiteBalance(kind: wbAsShot)
proc autoWb*(): WhiteBalance = WhiteBalance(kind: wbAuto)
proc multipliersWb*(m: array[3, float64]): WhiteBalance = WhiteBalance(kind: wbMultipliers, multipliers: m)

proc defaultDevelopOptions*(): DevelopOptions =
  DevelopOptions(demosaic: DefaultDemosaic, whiteBalance: asShotWb(), exposure: 0.0, orient: true,
                 limits: defaultRawLimits(), cameraLook: none(CameraLook))

proc satSub(a, b: int): int {.inline.} = max(a - b, 0)

proc linearized(lin: seq[uint16]; raw: uint16): uint16 {.inline.} =
  if int(raw) < lin.len: lin[int(raw)] elif lin.len > 0: lin[^1] else: raw

proc scaleValue*(s: Sensor; x, y, sample: int): float32 =
  ## Linearization + black/white scaling for one sample (pre white balance), 0..1.
  let raw = s.data[(y * s.width + x) * s.samples + sample]
  let v = if s.linearization.isSome: float32(linearized(s.linearization.get, raw)) else: float32(raw)
  let black = s.black.at(satSub(x, s.active.x), satSub(y, s.active.y), sample, s.samples)
  let white = s.white[min(sample, 2)]
  let range = white - black
  if range != range or range < 1'f32: return 0'f32
  let n = (v - black) / range
  if n == n and abs(n) != Inf: n else: 0'f32

proc greyWorld(s: Sensor): array[3, float64] =
  ## Grey-world neutral (camera-space R, G, B) from unclipped mid-range samples.
  let c = s.crop
  let step = max(c.height div 512, 1)
  var sum: array[3, float64]
  var n: array[3, uint64]
  var r = 0
  while r < c.height:
    let y = c.y + r
    var rs: array[3, float64]
    var rn: array[3, uint64]
    for x in c.x ..< c.x + c.width:
      for k in 0 ..< min(s.samples, 3):
        let ch = if s.cfa.isSome: min(int(s.cfa.get.color(x, y)), 2) else: k
        let v = scaleValue(s, x, y, k)
        if v > 0.002'f32 and v < 0.9'f32:
          rs[ch] += float64(v)
          rn[ch] += 1
    for k in 0 .. 2:
      sum[k] += rs[k]
      n[k] += rn[k]
    r += step
  var mean: array[3, float64]
  for k in 0 .. 2: mean[k] = if n[k] > 0: sum[k] / float64(n[k]) else: 0.0
  for m in mean:
    if m != m or m <= 1e-6: return [1.0, 1.0, 1.0]
  var m = -F64Max
  for v in mean: m = rmax(m, v)
  [mean[0] / m, mean[1] / m, mean[2] / m]

proc normalize(n: array[3, float64]): Option[array[3, float64]] =
  var m = -F64Max
  for v in n: m = rmax(m, v)
  if not (m > 0.0): return
  for v in n:
    if not (v == v and abs(v) != Inf and v > 1e-4): return
  some([n[0] / m, n[1] / m, n[2] / m])

proc resolveNeutral*(s: Sensor; opts: DevelopOptions; warnings: var seq[string]): array[3, float64] =
  ## The camera-space neutral (max component 1) for `opts.whiteBalance`, as `developSensor` uses
  ## it (the same white balance `fitLook` balances with).
  # Monochrome data has no colour to balance.
  if s.isMonochrome: return [1.0, 1.0, 1.0]
  # White balance: camera-space neutral, max component 1.
  var asShot = s.color.asShotNeutral
  if asShot.isNone and s.color.asShotWhiteXy.isSome: asShot = s.color.xyToNeutral(s.color.asShotWhiteXy.get)
  if asShot.isNone and s.cameraWb.isSome:
    let m = s.cameraWb.get
    asShot = some([1.0 / m[0], 1.0 / m[1], 1.0 / m[2]])
  if asShot.isSome:
    let n = asShot.get # a temporary: the result must not alias the argument
    asShot = normalize(n)
  case opts.whiteBalance.kind
  of wbAsShot:
    if asShot.isSome: asShot.get
    else:
      warnings.add "no as-shot white balance in the file; estimated automatically (grey world)"
      greyWorld(s)
  of wbAuto: greyWorld(s)
  of wbMultipliers:
    let m = opts.whiteBalance.multipliers
    let r = normalize([1.0 / m[0], 1.0 / m[1], 1.0 / m[2]])
    if r.isNone: raise malformed("white-balance multipliers must be positive")
    r.get

proc gainRow(s: Sensor; y, x0, w, k: int): Option[seq[float32]] =
  ## Gain-map factors (DNG OpcodeList2 GainMap) for data row `y`, columns `x0..x0 + w`, sample
  ## `k`; none when no map touches the row.
  let a = s.active
  if s.gainMaps.len == 0 or y < a.y or a.width == 0 or a.height == 0: return
  let ay = y - a.y
  # Map points are placed in coordinates relative to the (active) image.
  let rv = (float64(ay) + 0.5) / float64(a.height)
  for m in s.gainMaps:
    let planesEnd = if m.plane > high(int) - m.planes: high(int) else: m.plane + m.planes
    let rowMultiple = if m.rowPitch == 0: ay - m.top == 0 else: (ay - m.top) mod m.rowPitch == 0
    if k < m.plane or k >= planesEnd or ay < m.top or ay >= m.bottom or not rowMultiple: continue
    let row = m.row(rv, k - m.plane)
    if result.isNone:
      var ones = newSeq[float32](w)
      for v in ones.mitems: v = 1'f32
      result = some(ones)
    for i in 0 ..< w:
      let x = x0 + i
      if x >= a.x and m.colPitch != 0 and m.covers(x - a.x, ay):
        result.get[i] *= m.at(row, (float64(x - a.x) + 0.5) / float64(a.width))

# ---------------------------------------------------------------- gamma

const GammaSteps = 4096

let
  gamma18 = 1.8'f32
  invGamma32 = 1'f32 / gamma18 ## Rust's f32 constant `1.0 / 1.8`
  twoOverGamma32 = 2'f32 / gamma18 ## Rust's f32 constant `2.0 / 1.8`

type GammaLut* = object
  ## Linear 0..1 → gamma-1.8 16-bit code. Tabulated over `s = sqrt(v)`, where the curve
  ## `s^(2/1.8)` is smooth down to 0, with linear interpolation (error below 0.2 code values).
  table: seq[float32]

proc tone(curve: openArray[array[2, float32]]; v: float32): float32 =
  ## `curve` at linear `v`: linear interpolation between the points in log₂ of the input, a line
  ## through 0 below the first point, the last output beyond the last.
  if curve.len == 0: return v
  let (first, last) = (curve[0], curve[^1])
  if v <= first[0]: return (if first[0] > 0'f32: v * first[1] / first[0] else: first[1])
  if v >= last[0]: return last[1]
  let i = clamp(partitionPoint(curve, it[0] <= v), 1, curve.len - 1)
  let (x0, y0, x1, y1) = (curve[i - 1][0], curve[i - 1][1], curve[i][0], curve[i][1])
  let f = (log2(v) - log2(x0)) / (log2(x1) - log2(x0))
  y0 + (y1 - y0) * f

proc gammaWithCurve*(curve: openArray[array[2, float32]]): GammaLut =
  ## The gamma-1.8 encoding after a tone curve (see `Sensor.toneCurve`; empty: none).
  result.table = newSeq[float32](GammaSteps + 2)
  for i in 0 .. GammaSteps + 1:
    let s = min(float32(i) / float32(GammaSteps), 1'f32)
    result.table[i] = pow(clamp(tone(curve, s * s), 0'f32, 1'f32), invGamma32) * 65535'f32

proc gammaWithTone(t: Option[ToneCurve]): GammaLut =
  ## The gamma-1.8 encoding after a fitted `ToneCurve` (none: plain gamma 1.8).
  result.table = newSeq[float32](GammaSteps + 2)
  for i in 0 .. GammaSteps + 1:
    let s = min(float64(i) / float64(GammaSteps), 1.0)
    result.table[i] = if t.isSome: float32(pow(t.get.eval(s * s), 1.0 / 1.8) * 65535.0)
                      else: pow(float32(s), twoOverGamma32) * 65535'f32

proc encode*(l: GammaLut; v0: float32): uint16 {.inline.} =
  let v = if v0 != v0: 0'f32 else: clamp(v0, 0'f32, 1'f32)
  let x = sqrt(v) * float32(GammaSteps)
  let i = min(asIdx(x), GammaSteps)
  let f = x - float32(i)
  let (a, b) = (l.table[i], l.table[i + 1])
  asU16(a + (b - a) * f + 0.5'f32)

# ---------------------------------------------------------------- orientation

proc sourceXy*(o: uint16; w, h, x2, y2: int): (int, int) =
  ## The source position in a `w × h` image of output pixel (`x2`, `y2`) after applying TIFF
  ## orientation `o` (output in range when the input is).
  let (wl, hl) = (satSub(w, 1), satSub(h, 1))
  case o
  of 2: (satSub(wl, x2), y2)
  of 3: (satSub(wl, x2), satSub(hl, y2))
  of 4: (x2, satSub(hl, y2))
  of 5: (y2, x2)
  of 6: (y2, satSub(hl, x2))
  of 7: (satSub(wl, y2), satSub(hl, x2))
  of 8: (satSub(wl, y2), x2)
  else: (x2, y2)

proc orient*(src: sink seq[uint16]; w, h: int; o: uint16): (seq[uint16], int, int) =
  ## Applies a TIFF orientation (1–8) to interleaved RGB.
  if o < 2 or o > 8: return (src, w, h)
  let swap = o >= 5
  let (ow, oh) = if swap: (h, w) else: (w, h)
  var outp = newSeq[uint16](src.len)
  for y2 in 0 ..< oh:
    for x2 in 0 ..< ow:
      let (x, y) = case o
        of 2: (w - 1 - x2, y2)
        of 3: (w - 1 - x2, h - 1 - y2)
        of 4: (x2, h - 1 - y2)
        of 5: (y2, x2)
        of 6: (y2, h - 1 - x2)
        of 7: (w - 1 - y2, h - 1 - x2)
        else: (w - 1 - y2, x2)
      let i = (y * w + x) * 3
      let d = (y2 * ow + x2) * 3
      outp[d] = src[i]
      outp[d + 1] = src[i + 1]
      outp[d + 2] = src[i + 2]
  (outp, ow, oh)

# ---------------------------------------------------------------- develop

proc developSensor*(s: Sensor; opts: DevelopOptions): Developed =
  ## Develops sensor data.
  let c = s.crop
  proc fits(a, b, lim: int): bool = a >= 0 and b >= 0 and a <= high(int) - b and a + b <= lim
  var need = 0
  let sizeOk = s.width >= 0 and s.height >= 0 and s.samples >= 0 and
    (s.height == 0 or s.width <= high(int) div s.height) and
    (s.samples == 0 or s.width * s.height <= high(int) div max(s.samples, 1))
  if sizeOk: need = s.width * s.height * s.samples
  if c.isEmpty or not fits(c.x, c.width, s.width) or not fits(c.y, c.height, s.height) or not sizeOk or s.data.len < need:
    raise malformed("crop lies outside the sensor data")
  if s.samples != 1 and s.samples != 3: raise unsupported($s.samples & " samples per pixel")
  # Working buffers: one f32 plane plus f32 RGB plus u16 RGB per pixel, and a green plane for
  # non-Bayer patterns.
  let bayer = s.cfa.isNone or s.cfa.get.isBayer
  opts.limits.check(uint64(c.width), uint64(c.height), (if bayer: 4'u64 + 12 + 6 else: 4'u64 + 4 + 12 + 6))
  var warnings = s.warnings

  let neutral = resolveNeutral(s, opts, warnings)
  let mult = [float32(1.0 / neutral[0]), float32(1.0 / neutral[1]), float32(1.0 / neutral[2])]

  # Balanced camera → XYZ D50: a look fitted to the camera's JPEG, the file's calibration, or the
  # neutral fallback. Monochrome data stays neutral grey: no camera look and no colour calibration.
  let mono = s.isMonochrome
  var look = none(CameraLook)
  if opts.cameraLook.isSome and opts.cameraLook.get.isUsable and not mono: look = opts.cameraLook
  if opts.cameraLook.isSome and look.isNone and not mono:
    warnings.add "the supplied camera look is not usable (non-finite or singular); ignored"
  var toXyz: Mat3
  if look.isSome:
    warnings.add look.get.note
    toXyz = look.get.toXyz
  else:
    let cal = s.color.balancedToXyzD50(neutral)
    if cal.isSome: toXyz = cal.get
    else:
      if s.format != rfDng and not mono:
        warnings.add "no colour calibration for this camera; camera colours are treated as sRGB primaries"
      toXyz = srgbToXyzD50()
  let gain = pow(2.0, s.baselineExposure + clamp(opts.exposure, -10.0, 10.0))
  let outM64 = scale(mul(xyzD50ToProphoto(), toXyz), gain)
  var outM: array[3, array[3, float32]]
  for i in 0 .. 2:
    for j in 0 .. 2: outM[i][j] = float32(outM64[i][j])

  # Neutral grey through `outM`: the gain alone (the matrix maps camera white to ProPhoto white,
  # up to rounding), so the three output channels stay exactly equal.
  let greyGain = float32(gain)
  let (w, h) = (c.width, c.height)
  var rgb: seq[float32]
  var pattern = none(string)
  if s.cfa.isSome:
    let cfa = s.cfa.get
    # Normalized, balanced, clipped CFA plane.
    var plane = newPadded(w, h)
    let stride = plane.stride
    # Fast path: black varies at most per 2×2 position, so black and gain repeat every `period`
    # columns (the CFA width, made even).
    let bl = s.black
    let periodic = bl.deltaH.len == 0 and bl.deltaV.len == 0 and bl.rows in 1 .. 2 and bl.cols in 1 .. 2 and cfa.width <= 16
    let period = if cfa.width mod 2 == 0: max(cfa.width, 2) else: cfa.width * 2
    var k = newSeq[(float32, float32)](max(period, 0))
    for r in 0 ..< h:
      let y = c.y + r
      let rowAt = (r + Pad) * stride + Pad
      if periodic and period > 0:
        # Per column of the period: black and the combined scale × white-balance gain.
        for p in 0 ..< period:
          let x = c.x + p
          let black = bl.at(satSub(x, s.active.x), satSub(y, s.active.y), 0, 1)
          let range = s.white[0] - black
          let g = if range >= 1'f32: mult[min(int(cfa.color(x, y)), 2)] / range else: 0'f32
          k[p] = (black, g)
        let src = y * s.width + c.x
        if s.linearization.isNone:
          for i in 0 ..< w:
            let (black, g) = k[i mod period]
            plane.data[rowAt + i] = clamp((float32(s.data[src + i]) - black) * g, 0'f32, 1'f32)
        else:
          let l = s.linearization.get
          for i in 0 ..< w:
            let (black, g) = k[i mod period]
            plane.data[rowAt + i] = clamp((float32(linearized(l, s.data[src + i])) - black) * g, 0'f32, 1'f32)
      else:
        for i in 0 ..< w:
          let x = c.x + i
          let ch = min(int(cfa.color(x, y)), 2)
          plane.data[rowAt + i] = clamp(scaleValue(s, x, y, 0) * mult[ch], 0'f32, 1'f32)
      # Lens-shading gain maps, after the white-balance clip so clipped highlights stay neutral.
      let g = gainRow(s, y, c.x, w, 0)
      if g.isSome:
        for i in 0 ..< w: plane.data[rowAt + i] = clamp(plane.data[rowAt + i] * g.get[i], 0'f32, 1'f32)
    plane.fillBorders()
    proc names(p: openArray[uint8]): string =
      for c in p: result.add ['R', 'G', 'B'][min(int(c), 2)]
    if bayer:
      let p = cfa.phase(c.x, c.y)
      pattern = some(names(p))
      rgb = demosaic(plane, p, opts.demosaic, toXyz)
    else:
      let pat = patternOf(cfa, c.x, c.y)
      var parts: seq[string]
      var i = 0
      let pw = max(pat.w, 1)
      while i < pat.colors.len:
        parts.add names(pat.colors.toOpenArray(i, min(i + pw, pat.colors.len) - 1))
        i += pw
      var joined = ""
      for q, part in parts:
        if q > 0: joined.add "/"
        joined.add part
      pattern = some(joined)
      rgb = demosaicCfa(plane, pat)
  else:
    rgb = newSeq[float32](w * h * 3)
    for r in 0 ..< h:
      let y = c.y + r
      let rowAt = r * w * 3
      for i in 0 ..< w:
        for k in 0 .. 2:
          # Monochrome: the one sample in all three channels.
          let sample = min(k, satSub(s.samples, 1))
          rgb[rowAt + i * 3 + k] = clamp(scaleValue(s, c.x + i, y, sample) * mult[k], 0'f32, 1'f32)
      for k in 0 ..< min(s.samples, 3):
        let g = gainRow(s, y, c.x, w, k)
        if g.isSome:
          for i in 0 ..< w:
            rgb[rowAt + i * 3 + k] = clamp(rgb[rowAt + i * 3 + k] * g.get[i], 0'f32, 1'f32)

  # Camera → ProPhoto, exposure, the look's tone curve, gamma 1.8, 16 bits.
  var outp = newSeq[uint16](w * h * 3)
  let lut = if look.isSome: gammaWithTone(look.get.tone) else: gammaWithCurve(s.toneCurve)
  for px in 0 ..< w * h:
    let o = px * 3
    if mono:
      let e = lut.encode(greyGain * rgb[o])
      outp[o] = e
      outp[o + 1] = e
      outp[o + 2] = e
      continue
    for k in 0 .. 2:
      outp[o + k] = lut.encode(outM[k][0] * rgb[o] + outM[k][1] * rgb[o + 1] + outM[k][2] * rgb[o + 2])
  rgb = @[]

  var (res, ow, oh) = if opts.orient: orient(move outp, w, h, s.orientation) else: (move outp, w, h)
  var nmax = F32Max
  for m in mult: nmax = rmin(nmax, m)
  nmax = rmax(nmax, 1e-6'f32)
  Developed(width: uint32(ow), height: uint32(oh), rgb: move res,
            info: RawInfo(format: s.format, make: s.make, model: s.model, sensorWidth: s.width,
                          sensorHeight: s.height, cfa: pattern,
                          wbMultipliers: [float64(mult[0] / nmax), float64(mult[1] / nmax), float64(mult[2] / nmax)],
                          orientation: s.orientation, baselineExposure: s.baselineExposure),
            warnings: warnings)
