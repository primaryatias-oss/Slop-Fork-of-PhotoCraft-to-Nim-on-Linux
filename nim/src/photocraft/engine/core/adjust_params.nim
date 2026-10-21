## Full parameter sets for the per-pixel adjustment kinds, shared by the destructive
## `image.adjustments.*` commands, `layer.newAdjustmentLayer.*` and `layer.setAdjustment` (port of
## `adjust_params.rs`).
##
## `adjFromParams` reads params over a base adjustment (missing keys keep the base's value, so a
## partial update only changes what it names) and `adjToParams` writes the complete set back, so
## `adjFromParams(kind, adjToParams(a), none, mode) == a`. Values out of range are clamped like
## Photoshop's fields; a key of the wrong type is an error.
##
## Units: Levels and Curves in 0–255 levels; percentages for the rest (as in Photoshop's dialogs);
## colours as `"#rrggbb"` or `[r, g, b]` in 0–1. Part of the `engine/core` unit: included, not
## imported.

const
  HueRanges* = ["reds", "yellows", "greens", "cyans", "blues", "magentas"]
    ## Hue/Saturation and Black & White colour-range keys (storage order).
  BwDefaults* = [40'f32, 60, 40, 60, 20, 80]
    ## Black & White default weights (percent) for `HueRanges`.
  PhotoFilters*: array[20, (string, string, array[3, uint8])] = [
    ("warming85", "Warming Filter (85)", [236'u8, 138, 0]),
    ("warmingLBA", "Warming Filter (LBA)", [250'u8, 150, 0]),
    ("warming81", "Warming Filter (81)", [235'u8, 177, 19]),
    ("cooling80", "Cooling Filter (80)", [0'u8, 109, 255]),
    ("coolingLBB", "Cooling Filter (LBB)", [0'u8, 93, 255]),
    ("cooling82", "Cooling Filter (82)", [0'u8, 181, 255]),
    ("red", "Red", [234'u8, 26, 26]),
    ("orange", "Orange", [243'u8, 132, 23]),
    ("yellow", "Yellow", [249'u8, 227, 28]),
    ("green", "Green", [25'u8, 201, 25]),
    ("cyan", "Cyan", [29'u8, 203, 234]),
    ("blue", "Blue", [29'u8, 53, 234]),
    ("violet", "Violet", [155'u8, 29, 234]),
    ("magenta", "Magenta", [227'u8, 24, 227]),
    ("sepia", "Sepia", [172'u8, 122, 51]),
    ("deepRed", "Deep Red", [255'u8, 0, 0]),
    ("deepBlue", "Deep Blue", [0'u8, 34, 205]),
    ("deepEmerald", "Deep Emerald", [0'u8, 140, 0]),
    ("deepYellow", "Deep Yellow", [255'u8, 213, 0]),
    ("underwater", "Underwater", [0'u8, 194, 177]),
  ]  ## Photo Filter presets `(id, label, colour)`: common photographic filter colours.
  SelectiveRanges* = ["reds", "yellows", "greens", "cyans", "blues", "magentas", "whites", "neutrals", "blacks"]
    ## Selective Color range keys in storage order (`adjust_cmds::RANGES`).
  MaxCurvePoints* = 19
    ## Most points a curve may have (Photoshop allows 16; the PSD record holds 19).
  MaxFittedPoints = 16
    ## Points Photoshop's "Edit points to modify the curve" places on a pencil-drawn curve at most.
  MaxSmoothPasses* = 10'f32
    ## Most Smooth passes one `smooth` param applies.
  CurveCoordinatePrecision = 100'f32
  CurvePointDuplicateDistance = 0.5'f32 / 255
  LevelsMinRange = 2'f32 / 255
    ## The smallest input range Levels allows (two levels), as the parser enforces.
  EyedropperCmd = "eyedropper"
  DefaultTint = [0.882'f32, 0.827, 0.702]
    ## Photoshop's default Black & White tint colour, a light sepia.

type
  CurvesEyedropper* = enum
    ## Fixed-target Curves / Levels eyedroppers.
    ceBlack, ceNeutralGray, ceWhite
  AdjReader = object
    ## Typed, clamped access to optional params.
    cmd: string
    p: JsonNode

proc lookupFromParams*(p: JsonNode; base: Option[Adjustment]): Adjustment
proc selectiveFromParams*(p: JsonNode; base: Option[Adjustment]): Adjustment

proc adjBad(cmdId, msg: string): ref EngineError = bad(cmdId, msg)

proc channelKeys*(space: ToneSpace): seq[string] =
  ## Per-channel keys of Levels/Curves in each tone space (after the composite).
  case space
  of tsRgb: @["red", "green", "blue"]
  of tsCmyk: @["cyan", "magenta", "yellow", "black"]
  of tsLab: @["lightness", "a", "b"]

proc defaultSpace*(mode: ColorMode): ToneSpace =
  ## The tone space new Levels/Curves get in a document of `mode`.
  case mode
  of cmCmyk: tsCmyk
  of cmLab: tsLab
  else: tsRgb

# ---------------------------------------------------------------- reader

proc get(r: AdjReader; key: string): JsonNode =
  ## `key`'s value; a null counts as missing.
  let v = r.p.pget(key)
  if v.isNull: nil else: v

proc numIn(cmdId, key: string; v: JsonNode; lo, hi: float32): float32 =
  let x = v.asF64
  if x.isNone or x.get != x.get or abs(x.get) == Inf: raise adjBad(cmdId, "`" & key & "` must be a number")
  clamp(float32(x.get), lo, hi)

proc num(r: AdjReader; key: string; default, lo, hi: float32): float32 =
  let v = r.get(key)
  if v.isNil: clamp(default, lo, hi) else: numIn(r.cmd, key, v, lo, hi)

proc boolean(r: AdjReader; key: string; default: bool): bool =
  let v = r.get(key)
  if v.isNil: default
  elif v.kind == JBool: v.bval
  else: raise adjBad(r.cmd, "`" & key & "` must be true or false")

proc numbers(r: AdjReader; key: string; n: int): Option[seq[float32]] =
  ## An array of exactly `n` numbers.
  let v = r.get(key)
  if v.isNil: return
  if not v.isArray or v.len != n: raise adjBad(r.cmd, "`" & key & "` must be an array of " & $n & " numbers")
  var s: seq[float32]
  for x in v.elems: s.add numIn(r.cmd, key, x, -1e6, 1e6)
  some(s)

proc hexByte(s: string): Option[uint8] =
  ## Rust `u8::from_str_radix(s, 16)` on a two-byte slice (a leading `+` is a sign).
  var t = s
  if t.len > 0 and t[0] == '+': t = t[1 .. ^1]
  if t.len == 0: return
  var v = 0
  for ch in t:
    let d = case ch
      of '0'..'9': ord(ch) - ord('0')
      of 'a'..'f': ord(ch) - ord('a') + 10
      of 'A'..'F': ord(ch) - ord('A') + 10
      else: return
    v = v * 16 + d
  if v > 255: return
  some(uint8(v))

proc colorOf(cmdId, key: string; v: JsonNode): array[3, float32] =
  let err = adjBad(cmdId, "`" & key & "` must be \"#rrggbb\" or [r, g, b] in 0..1")
  if v.kind == JString:
    var h = v.str
    var i = 0
    while i < h.len and h[i] == '#': inc i
    h = h[i .. ^1]
    proc b(h: string; i: int): Option[float32] =
      if i + 2 > h.len: return
      let x = hexByte(h[i ..< i + 2])
      if x.isNone: none(float32) else: some(float32(x.get) / 255)
    let (r, g, bb) = (b(h, 0), b(h, 2), b(h, 4))
    if (h.len == 6 or h.len == 8) and r.isSome and g.isSome and bb.isSome: return [r.get, g.get, bb.get]
    raise err
  if v.isArray and v.len >= 3:
    var c: array[3, float32]
    for i in 0 .. 2:
      let x = v.elems[i].asF64
      if x.isNone or x.get != x.get or abs(x.get) == Inf: raise err
      c[i] = clamp(float32(x.get), 0'f32, 1'f32)
    return c
  raise err

proc color(r: AdjReader; key: string; default: array[3, float32]): array[3, float32] =
  let v = r.get(key)
  if v.isNil: default else: colorOf(r.cmd, key, v)

proc obj(r: AdjReader; key: string): Option[AdjReader] =
  ## A channel object (`{"inBlack": …}`) or none.
  let v = r.get(key)
  if v.isNil: return
  if v.isObject: return some(AdjReader(cmd: r.cmd, p: v))
  raise adjBad(r.cmd, "`" & key & "` must be an object")

proc adjHex*(c: array[3, float32]): string =
  proc b(v: float32): string = toHex(int(asU8(round(clamp(v, 0'f32, 1'f32) * 255'f32))), 2).toLowerAscii
  "#" & b(c[0]) & b(c[1]) & b(c[2])

proc mergeMissing(a, b: JsonNode): JsonNode =
  ## Adds `b`'s keys that `a` lacks.
  result = a
  if a.isObject and b.isObject:
    for k, v in b.fields:
      if not a.hasKey(k): a[k] = v

proc mixerRow(v: seq[float32]): array[4, float32] =
  for i in 0 .. 3:
    result[i] = clamp(if i < v.len: v[i] else: 0'f32, -200'f32, 200'f32) / 100

proc fj(x: float32): JsonNode = %float64(x)
proc fj[N: static int](a: array[N, float32]): JsonNode =
  result = newJArray()
  for x in a: result.add %float64(x)

# ---------------------------------------------------------------- curves

proc identityCurve(): seq[CurvePoint] = @[CurvePoint(input: 0, output: 0), CurvePoint(input: 1, output: 1)]

proc curveLevel(v: float32): float32 =
  round(v * 255'f32 * CurveCoordinatePrecision) / CurveCoordinatePrecision

proc curveArr(c: seq[CurvePoint]): JsonNode =
  let c = if c.len >= 2: c else: identityCurve()
  result = newJArray()
  for p in c: result.add %*[float64(curveLevel(p.input)), float64(curveLevel(p.output))]

proc mapArr(c: seq[CurvePoint]): JsonNode =
  ## A pencil-mode curve as params: its `CurveMapLen` outputs in 0..255.
  result = newJArray()
  for o in curveMapOutputs(c): result.add %float64(curveLevel(o))

proc lutAt(lut: seq[float32]; x: float32): float32 =
  ## `lut` (evenly spaced over 0..=1) at `x`, linearly interpolated.
  if lut.len == 0: return x
  let last = lut.len - 1
  let f = clamp(x, 0'f32, 1'f32) * float32(last)
  let i = min(asIdx(floor(f)), last)
  let a = if i < lut.len: lut[i] else: x
  let b = lut[min(i + 1, last)]
  a + (b - a) * (f - float32(i))

proc pointsToMap*(points: seq[CurvePoint]): seq[CurvePoint] =
  ## A point curve drawn into a pencil-mode map (Photoshop switching to "Draw to modify the curve").
  let lut = curveLut(points)
  let last = float32(CurveMapLen - 1)
  var outs = newSeq[float32](CurveMapLen)
  for k in 0 ..< CurveMapLen: outs[k] = lutAt(lut, float32(k) / last)
  curveMap(outs)

proc totalGreaterEq(a, b: float32): bool =
  ## `a.total_cmp(&b) != Less` for the non-negative errors `map_to_points` compares (NaN last).
  if a != a: return true
  if b != b: return false
  a >= b

proc mapToPoints*(map: seq[CurvePoint]): seq[CurvePoint] =
  ## A pencil-mode map turned back into a point curve (Photoshop's "Edit points to modify the
  ## curve"): the endpoints, then the worst-fitting input until the spline is within one level of
  ## the map or `MaxFittedPoints` are placed.
  let target = curveMapOutputs(map)
  let last = CurveMapLen - 1
  proc at(target: seq[float32]; k, last: int): CurvePoint =
    CurvePoint(input: float32(k) / float32(last), output: if k < target.len: target[k] else: 0'f32)
  var pts = @[at(target, 0, last), at(target, last, last)]
  while pts.len < MaxFittedPoints:
    let lut = curveLut(pts)
    var worstK = -1
    var worstE = 0'f32
    for k in 0 ..< CurveMapLen:
      let t = if k < target.len: target[k] else: 0'f32
      let e = abs(lutAt(lut, float32(k) / float32(last)) - t)
      # `max_by` keeps the last of equal maxima.
      if worstK < 0 or totalGreaterEq(e, worstE):
        worstK = k
        worstE = e
    if worstK < 0 or not (worstE > 1'f32 / 255): break
    let p = at(target, worstK, last)
    var dup = false
    for q in pts:
      if abs(q.input - p.input) < CurvePointDuplicateDistance: dup = true
    if dup: break
    pts.add p
    pts.sort(proc(a, b: CurvePoint): int = cmp(a.input, b.input))
  pts

proc smoothCurveMap*(outputs: var seq[float32]) =
  ## One pass of Curves' Smooth button over pencil-mode outputs (any scale): each value becomes
  ## the mean of a symmetric window of up to three neighbours a side.
  const Radius = 3
  let src = outputs
  let n = src.len
  for k in 0 ..< outputs.len:
    let r = min(min(Radius, k), max(n - (k + 1), 0))
    if r == 0: continue
    var sum = -0'f32
    for i in k - r .. k + r: sum += src[i]
    outputs[k] = sum / float32(max(2 * r + 1, 1))

proc isIdentityMap(c: seq[CurvePoint]): bool =
  ## Whether a pencil-mode curve maps every level to itself.
  let last = float32(CurveMapLen - 1)
  let outs = curveMapOutputs(c)
  for k, o in outs:
    if not (abs(o - float32(k) / last) < 0.5'f32 / 255): return false
  true

proc parseCurve*(cmdId, key: string; v: JsonNode): seq[CurvePoint] =
  ## `[[in, out], …]` in 0..255: sorted by input, duplicates collapsed, 2..=19 points.
  let err = adjBad(cmdId, "`" & key & "` must be an array of 2..=" & $MaxCurvePoints & " [input, output] pairs in 0..255")
  if not v.isArray: raise err
  if v.len < 2 or v.len > 64: raise err
  var pts: seq[CurvePoint]
  for q in v.elems:
    if not q.isArray or q.len != 2: raise err
    let x = q.elems[0].asF64
    if x.isNone or x.get != x.get or abs(x.get) == Inf: raise err
    let y = q.elems[1].asF64
    if y.isNone or y.get != y.get or abs(y.get) == Inf: raise err
    pts.add CurvePoint(input: clamp(float32(x.get), 0'f32, 255'f32) / 255, output: clamp(float32(y.get), 0'f32, 255'f32) / 255)
  pts.sort(proc(a, b: CurvePoint): int = cmp(a.input, b.input))
  var kept: seq[CurvePoint]
  for p in pts:
    if kept.len > 0 and abs(p.input - kept[^1].input) < CurvePointDuplicateDistance: continue
    kept.add p
  if kept.len < 2 or kept.len > MaxCurvePoints: raise err
  kept

proc parseMap(cmdId, key: string; v: JsonNode): seq[CurvePoint] =
  ## A pencil-mode map param: exactly `CurveMapLen` outputs in 0..255.
  let err = adjBad(cmdId, "`" & key & "` must be " & $CurveMapLen & " output levels in 0..255 (a pencil-drawn map) or [input, output] pairs")
  if not v.isArray or v.len != CurveMapLen: raise err
  var outs: seq[float32]
  for x in v.elems:
    let f = x.asF64
    if f.isNone or f.get != f.get or abs(f.get) == Inf: raise err
    outs.add clamp(float32(f.get), 0'f32, 255'f32) / 255
  curveMap(outs)

proc parseCurveIn(cmdId, key: string; v: JsonNode; pencil: bool): seq[CurvePoint] =
  ## A Curves channel param in the adjustment's mode: `[[in, out], …]` points (drawn into a map in
  ## pencil mode) or a `CurveMapLen`-level map (fitted with points outside pencil mode).
  if v.isArray and v.len > 0 and v.elems[0].kind in {JInt, JFloat}:
    let m = parseMap(cmdId, key, v)
    if pencil: m else: mapToPoints(m)
  else:
    let pts = parseCurve(cmdId, key, v)
    if pencil: pointsToMap(pts) else: pts

# ---------------------------------------------------------------- kinds

proc hueSaturation(r: AdjReader; base: Option[Adjustment]): Adjustment =
  var (h, s, l, c) = (0'f32, 0'f32, 0'f32, false)
  var ranges = defaultHueRanges()
  if base.isSome and base.get.kind == akHueSaturation:
    let b = base.get
    (h, s, l, c) = (b.hue, b.saturation, b.lightness, b.colorize)
    ranges = b.ranges
  let colorize = r.boolean("colorize", c)
  # Colorize hue is 0..360, a shift is -180..180.
  let hue = if colorize: r.num("hue", remEuclidF(h, 360'f32), 0, 360) else: r.num("hue", h, -180, 180)
  # Turning Colorize on starts at 25% saturation unless one is given, like the dialog's checkbox.
  let s2 = if colorize and not c: rmax(s, 25'f32) else: s
  let saturation = r.num("saturation", s2, (if colorize: 0'f32 else: -100'f32), 100)
  let lightness = r.num("lightness", l, -100, 100)
  for i, key in HueRanges:
    let o = r.obj(key)
    if o.isNone: continue
    let o2 = o.get
    ranges[i].hue = o2.num("hue", ranges[i].hue, -180, 180)
    ranges[i].saturation = o2.num("saturation", ranges[i].saturation, -100, 100)
    ranges[i].lightness = o2.num("lightness", ranges[i].lightness, -100, 100)
    let b = o2.numbers("range", 4)
    if b.isSome:
      # Keep the four sliders ordered and within one turn of the first.
      ranges[i].bounds = canonicalBounds([b.get[0], b.get[1], b.get[2], b.get[3]])
  Adjustment(kind: akHueSaturation, hue: hue, saturation: saturation, lightness: lightness, colorize: colorize, ranges: ranges)

proc spaceOf(r: AdjReader; base: Option[ToneSpace]; mode: ColorMode): ToneSpace =
  ## Which tone space the params address: any channel key of a space picks it.
  proc has(r: AdjReader; keys: openArray[string]): bool =
    for k in keys:
      if not r.get(k).isNil: return true
  if r.has(["cyan", "magenta", "yellow", "black"]): tsCmyk
  elif r.has(["a", "b"]) or (r.has(["lightness"]) and not r.has(["red", "green", "blue"])): tsLab
  elif r.has(["red", "green", "blue"]): tsRgb
  elif base.isSome: base.get
  else: defaultSpace(mode)

proc readLevels(o: AdjReader; c: LevelsChannel): LevelsChannel =
  let inBlack = o.num("inBlack", c.inBlack * 255, 0, 253)
  let inWhite = rmax(o.num("inWhite", c.inWhite * 255, 2, 255), inBlack + 2)
  LevelsChannel(inBlack: inBlack / 255, inWhite: inWhite / 255, gamma: o.num("gamma", c.gamma, 0.01, 9.99),
                outBlack: o.num("outBlack", c.outBlack * 255, 0, 255) / 255,
                outWhite: o.num("outWhite", c.outWhite * 255, 0, 255) / 255)

proc levelsAdj(r: AdjReader; base: Option[Adjustment]; mode: ColorMode): Adjustment =
  var master = defaultLevelsChannel()
  var chans: array[4, LevelsChannel]
  for c in chans.mitems: c = defaultLevelsChannel()
  var baseSpace = none(ToneSpace)
  if base.isSome and base.get.kind == akLevels:
    let b = base.get
    master = b.levelsMaster
    chans = [b.levelsPerChannel[0], b.levelsPerChannel[1], b.levelsPerChannel[2], b.levelsBlack]
    baseSpace = some(b.space)
  let space = spaceOf(r, baseSpace, mode)
  if baseSpace.isSome and baseSpace.get != space:
    for c in chans.mitems: c = defaultLevelsChannel()
  if space == tsLab:
    # Top-level keys (and "gray") address lightness: Lab has no composite.
    chans[0] = readLevels(r, chans[0])
  else:
    master = readLevels(r, master)
    let g = r.obj("gray")
    if g.isSome: master = readLevels(g.get, master)
  for i, key in channelKeys(space):
    let o = r.obj(key)
    if o.isSome and i < chans.len: chans[i] = readLevels(o.get, chans[i])
  Adjustment(kind: akLevels, levelsMaster: master, levelsPerChannel: [chans[0], chans[1], chans[2]], space: space,
             levelsBlack: if space == tsCmyk: chans[3] else: defaultLevelsChannel())

proc curvesAdj(r: AdjReader; base: Option[Adjustment]; mode: ColorMode): Adjustment =
  var master = identityCurve()
  var chans = [identityCurve(), identityCurve(), identityCurve(), identityCurve()]
  var baseSpace = none(ToneSpace)
  var basePencil = false
  if base.isSome and base.get.kind == akCurves:
    let b = base.get
    master = b.curvesMaster
    chans = [b.curvesPerChannel[0], b.curvesPerChannel[1], b.curvesPerChannel[2], b.curvesBlack]
    baseSpace = some(b.space)
    basePencil = b.pencil
  let space = spaceOf(r, baseSpace, mode)
  if baseSpace.isSome and baseSpace.get != space:
    chans = [identityCurve(), identityCurve(), identityCurve(), identityCurve()]
  let pencil = r.boolean("pencil", basePencil)
  # Every stored curve in the adjustment's mode: maps are drawn from the spline when switching
  # to the pencil, fitted with points when switching back, and filled out to full maps.
  proc convert(c: seq[CurvePoint]; basePencil, pencil: bool): seq[CurvePoint] =
    if not basePencil and pencil: pointsToMap(c)
    elif basePencil and not pencil: mapToPoints(c)
    elif basePencil and pencil and c.len != CurveMapLen: curveMap(curveMapOutputs(c))
    else: c
  master = convert(master, basePencil, pencil)
  for c in chans.mitems: c = convert(c, basePencil, pencil)
  var composite = r.get("points")
  if composite.isNil: composite = r.get("gray")
  if not composite.isNil:
    let c = parseCurveIn(r.cmd, "points", composite, pencil)
    if space == tsLab: chans[0] = c else: master = c
  for i, key in channelKeys(space):
    let v = r.get(key)
    if not v.isNil and i < chans.len: chans[i] = parseCurveIn(r.cmd, key, v, pencil)
  let passes = int(round(r.num("smooth", 0, 0, MaxSmoothPasses)))
  if pencil:
    # Smoothed, then held at the params' precision so `adjToParams` round-trips exactly.
    proc level(o: float32): float32 = curveLevel(o) / 255
    proc smoothed(c: seq[CurvePoint]; passes: int): seq[CurvePoint] =
      var outs = curveMapOutputs(c)
      for _ in 0 ..< passes: smoothCurveMap(outs)
      for o in outs.mitems: o = level(o)
      curveMap(outs)
    master = smoothed(master, passes)
    for c in chans.mitems: c = smoothed(c, passes)
  let black = chans[3]
  let identity = if pencil: isIdentityMap(black) else: isIdentityCurve(black)
  Adjustment(kind: akCurves, curvesMaster: master, curvesPerChannel: [chans[0], chans[1], chans[2]], space: space,
             curvesBlack: if space == tsCmyk and not identity: black else: @[], pencil: pencil)

proc gradientStops(cmdId: string; v: JsonNode): seq[(float32, array[3, float32])] =
  ## `[[location 0..1, colour], …]` (colour as for `color`), sorted by location.
  let err = adjBad(cmdId, "`stops` must be an array of 2..=64 [location 0..1, colour] pairs")
  if not v.isArray or v.len < 2 or v.len > 64: raise err
  for s in v.elems:
    var t, c: JsonNode
    if s.isArray and s.len == 2: (t, c) = (s.elems[0], s.elems[1])
    elif s.isObject: (t, c) = (s.pget("location"), s.pget("color"))
    else: raise err
    let tf = if t.isNil: none(float64) else: t.asF64
    if tf.isNone or tf.get != tf.get or abs(tf.get) == Inf: raise err
    if c.isNil: raise err
    let col = colorOf(cmdId, "stops", c)
    result.add (clamp(float32(tf.get), 0'f32, 1'f32), col)
  result.sort(proc(a, b: (float32, array[3, float32])): int = cmp(a[0], b[0]))

proc adjFromParams*(kind: string; p: JsonNode; current: Option[Adjustment]; mode: ColorMode): Adjustment =
  ## Reads the adjustment of `kind` from `p` over `current` (or the kind's defaults when it is none
  ## or of another kind). `mode` picks the Levels/Curves channel space when neither the params nor
  ## the base name one.
  let cmdId = "adjustment `" & kind & "`"
  let p = if p.isNil: newJNull() else: p
  if not p.isObject and not p.isNull: raise adjBad(cmdId, "params must be an object")
  let r = AdjReader(cmd: cmdId, p: p)
  let base = current
  template baseIs(k: AdjustmentKind): bool = base.isSome and base.get.kind == k
  case kind
  of "invert": Adjustment(kind: akInvert)
  of "brightnessContrast":
    var (b0, c0, l0) = (0'f32, 0'f32, false)
    if baseIs(akBrightnessContrast): (b0, c0, l0) = (base.get.brightness, base.get.contrast, base.get.legacy)
    let legacy = r.boolean("legacy", l0)
    let (lo, hi) = if legacy: (-100'f32, 100'f32) else: (-50'f32, 100'f32)
    let brightness = r.num("brightness", b0, -150, 150)
    Adjustment(kind: akBrightnessContrast, brightness: brightness, contrast: r.num("contrast", c0, lo, hi), legacy: legacy)
  of "threshold":
    let l0 = if baseIs(akThreshold): base.get.level * 255 else: 128'f32
    Adjustment(kind: akThreshold, level: round(r.num("level", l0, 1, 255)) / 255)
  of "posterize":
    let l0 = if baseIs(akPosterize): float32(base.get.levels) else: 4'f32
    Adjustment(kind: akPosterize, levels: asU32(round(r.num("levels", l0, 2, 255))))
  of "exposure":
    var (e, o, g) = (0'f32, 0'f32, 1'f32)
    if baseIs(akExposure): (e, o, g) = (base.get.exposure, base.get.offset, base.get.gamma)
    let exposure = r.num("exposure", e, -20, 20)
    let offset = r.num("offset", o, -0.5, 0.5)
    Adjustment(kind: akExposure, exposure: exposure, offset: offset, gamma: r.num("gamma", g, 0.01, 9.99))
  of "vibrance":
    var (v, s) = (0'f32, 0'f32)
    if baseIs(akVibrance): (v, s) = (base.get.vibrance, base.get.saturation)
    let vibrance = r.num("vibrance", v, -100, 100)
    Adjustment(kind: akVibrance, vibrance: vibrance, saturation: r.num("saturation", s, -100, 100))
  of "hueSaturation": hueSaturation(r, base)
  of "levels": levelsAdj(r, base, mode)
  of "curves": curvesAdj(r, base, mode)
  of "colorBalance":
    var tones: array[3, array[3, float32]]
    var preserve = true
    if baseIs(akColorBalance):
      tones = [base.get.shadows, base.get.midtones, base.get.highlights]
      preserve = base.get.preserveLuminosity
    for i, key in ["shadows", "midtones", "highlights"]:
      let v = r.numbers(key, 3)
      if v.isSome:
        for k in 0 .. 2: tones[i][k] = clamp(v.get[k], -100'f32, 100'f32)
    Adjustment(kind: akColorBalance, shadows: tones[0], midtones: tones[1], highlights: tones[2],
               preserveLuminosity: r.boolean("preserveLuminosity", preserve))
  of "blackWhite":
    var weights = BwDefaults
    var tint0 = none(array[3, float32])
    if baseIs(akBlackWhite): (weights, tint0) = (base.get.weights, base.get.tint)
    for i, key in HueRanges: weights[i] = r.num(key, weights[i], -200, 300)
    let color = r.color("tintColor", if tint0.isSome: tint0.get else: DefaultTint)
    let tint = if r.boolean("tint", tint0.isSome): some(color) else: none(array[3, float32])
    Adjustment(kind: akBlackWhite, weights: weights, tint: tint)
  of "photoFilter":
    var (c0, d0, p0) = ([236'f32 / 255, 138'f32 / 255, 0'f32], 0.25'f32, true)
    if baseIs(akPhotoFilter): (c0, d0, p0) = (base.get.filterColor, base.get.density, base.get.preserveLuminosity)
    var preset = none(array[3, float32])
    let fv = p.pget("filter")
    if not fv.isNull:
      if fv.kind != JString: raise adjBad(cmdId, "`filter` must be a preset id")
      var found = false
      for f in PhotoFilters:
        if f[0] == fv.str:
          preset = some([float32(f[2][0]) / 255, float32(f[2][1]) / 255, float32(f[2][2]) / 255])
          found = true
          break
      if not found: raise adjBad(cmdId, "unknown filter `" & fv.str & "`")
    let color = r.color("color", if preset.isSome: preset.get else: c0)
    let density = r.num("density", d0 * 100, 0, 100) / 100
    Adjustment(kind: akPhotoFilter, filterColor: color, density: density,
               preserveLuminosity: r.boolean("preserveLuminosity", p0))
  of "channelMixer":
    var matrix = [[1'f32, 0, 0, 0], [0'f32, 1, 0, 0], [0'f32, 0, 1, 0]]
    var mono0 = false
    if baseIs(akChannelMixer): (matrix, mono0) = (base.get.matrix, base.get.monochrome)
    let monochrome = r.boolean("monochrome", mono0)
    if monochrome and not mono0 and p.pget("gray").isNil:
      # Turning Monochrome on starts from Photoshop's grey mix.
      matrix[0] = [0.4'f32, 0.4, 0.2, 0]
    for i, key in ["red", "green", "blue"]:
      let v = r.numbers(key, 4)
      if v.isSome: matrix[i] = mixerRow(v.get)
    let g = r.numbers("gray", 4)
    if g.isSome: matrix[0] = mixerRow(g.get)
    Adjustment(kind: akChannelMixer, matrix: matrix, monochrome: monochrome)
  of "gradientMap":
    var stops0 = @[(0'f32, [0'f32, 0, 0]), (1'f32, [1'f32, 1, 1])]
    var (rev0, dither0) = (false, false)
    if baseIs(akGradientMap): (stops0, rev0, dither0) = (base.get.gmStops, base.get.reverse, base.get.dither)
    let sv = p.pget("stops")
    let stops = if sv.isNull: stops0 else: gradientStops(cmdId, sv)
    let reverse = r.boolean("reverse", rev0)
    Adjustment(kind: akGradientMap, gmStops: stops, reverse: reverse, dither: r.boolean("dither", dither0))
  of "selectiveColor": selectiveFromParams(p, if baseIs(akSelectiveColor): base else: none(Adjustment))
  of "colorLookup": lookupFromParams(p, if baseIs(akColorLookup): base else: none(Adjustment))
  else: raise adjBad("adjustment", "unknown adjustment kind `" & kind & "`")

proc adjDefaultFor*(kind: string; mode: ColorMode): Adjustment =
  ## The neutral adjustment of `kind` (what "Reset to defaults" restores).
  adjFromParams(kind, newJObject(), none(Adjustment), mode)

proc levelsObj(c: LevelsChannel): JsonNode =
  proc q(v: float32): JsonNode = %float64(round(v * 255'f32 * 100'f32) / 100'f32)
  %*{"inBlack": q(c.inBlack), "gamma": float64(round(c.gamma * 1000'f32) / 1000'f32), "inWhite": q(c.inWhite),
     "outBlack": q(c.outBlack), "outWhite": q(c.outWhite)}

proc adjToParams*(a: Adjustment): JsonNode =
  ## The complete parameter set of `a` (the inverse of `adjFromParams`).
  case a.kind
  of akInvert, akUnsupported: newJObject()
  of akBrightnessContrast: %*{"brightness": fj(a.brightness), "contrast": fj(a.contrast), "legacy": a.legacy}
  of akThreshold: %*{"level": fj(round(a.level * 255'f32))}
  of akPosterize: %*{"levels": a.levels}
  of akExposure: %*{"exposure": fj(a.exposure), "offset": fj(a.offset), "gamma": fj(a.gamma)}
  of akVibrance: %*{"vibrance": fj(a.vibrance), "saturation": fj(a.saturation)}
  of akHueSaturation:
    var v = %*{"hue": fj(a.hue), "saturation": fj(a.saturation), "lightness": fj(a.lightness), "colorize": a.colorize}
    for i, key in HueRanges:
      let r = a.ranges[i]
      v[key] = %*{"hue": fj(r.hue), "saturation": fj(r.saturation), "lightness": fj(r.lightness), "range": fj(r.bounds)}
    v
  of akLevels:
    var v = levelsObj(a.levelsMaster)
    let chans = [a.levelsPerChannel[0], a.levelsPerChannel[1], a.levelsPerChannel[2], a.levelsBlack]
    for i, key in channelKeys(a.space): v[key] = levelsObj(chans[i])
    if a.space == tsLab:
      # No composite in Lab: the top-level keys are the lightness channel.
      v = mergeMissing(levelsObj(a.levelsPerChannel[0]), v)
    v
  of akCurves:
    proc arr(c: seq[CurvePoint]; pencil: bool): JsonNode = (if pencil: mapArr(c) else: curveArr(c))
    var v = %*{"points": arr(a.curvesMaster, a.pencil)}
    let ink = if a.curvesBlack.len >= 2: a.curvesBlack else: identityCurve()
    let chans = [a.curvesPerChannel[0], a.curvesPerChannel[1], a.curvesPerChannel[2], ink]
    for i, key in channelKeys(a.space): v[key] = arr(chans[i], a.pencil)
    if a.space == tsLab: v["points"] = arr(a.curvesPerChannel[0], a.pencil)
    if a.pencil: v["pencil"] = %true
    v
  of akColorBalance:
    %*{"shadows": fj(a.shadows), "midtones": fj(a.midtones), "highlights": fj(a.highlights),
       "preserveLuminosity": a.preserveLuminosity}
  of akBlackWhite:
    var v = %*{"tint": a.tint.isSome, "tintColor": adjHex(if a.tint.isSome: a.tint.get else: DefaultTint)}
    for i, key in HueRanges: v[key] = fj(a.weights[i])
    v
  of akPhotoFilter:
    %*{"color": adjHex(a.filterColor), "density": fj(round(a.density * 1000'f32) / 10'f32),
       "preserveLuminosity": a.preserveLuminosity}
  of akChannelMixer:
    proc row(r: array[4, float32]): JsonNode =
      var q: array[4, float32]
      for i in 0 .. 3: q[i] = round(r[i] * 1000'f32) / 10'f32
      fj(q)
    var v = %*{"red": row(a.matrix[0]), "green": row(a.matrix[1]), "blue": row(a.matrix[2]), "monochrome": a.monochrome}
    if a.monochrome: v["gray"] = row(a.matrix[0])
    v
  of akGradientMap:
    var stops = newJArray()
    for (t, c) in a.gmStops: stops.add %*[fj(t), adjHex(c)]
    %*{"stops": stops, "reverse": a.reverse, "dither": a.dither}
  of akSelectiveColor:
    var v = %*{"method": if a.relative: "relative" else: "absolute"}
    for i, key in SelectiveRanges: v[key] = fj(a.selective[i])
    v
  of akColorLookup:
    %*{"interpolation": if a.tetrahedral: "tetrahedral" else: "trilinear", "dither": a.dither}

# ---------------------------------------------------------------- eyedroppers

proc curveRoundTripKeepsAnchor(points: seq[CurvePoint]; input, output: float32): bool =
  ## Whether serializing and parsing `points` keeps every point and the requested anchor.
  var parsed: seq[CurvePoint]
  try: parsed = parseCurve("curves eyedropper", "curve", curveArr(points))
  except EngineError: return false
  if parsed.len != points.len: return false
  proc serialized(v: float32): float32 = curveLevel(v) / 255
  let (i, o) = (serialized(input), serialized(output))
  for p in parsed:
    if abs(p.input - i) < 1e-6'f32 and abs(p.output - o) < 1e-6'f32: return true
  false

proc moveCurveEndpoint(points: var seq[CurvePoint]; sample: float32; black: bool) =
  let sample = clamp(sample, 0'f32, 1'f32)
  var candidate = points
  if black:
    for p in candidate.mitems: p.input = sample + p.input * (1 - sample)
    if candidate.len > 0:
      candidate[0].input = sample
      candidate[0].output = 0
  else:
    for p in candidate.mitems: p.input *= sample
    if candidate.len > 0:
      candidate[^1].input = sample
      candidate[^1].output = 1
  # Scaling an endpoint toward the sample can collapse several points together: check that the
  # full curve survives the actual serializer and parser.
  let target = if black: 0'f32 else: 1'f32
  if curveRoundTripKeepsAnchor(candidate, sample, target):
    points = candidate
    return
  # Otherwise keep the user's points and make the two nearest the endpoint a target plateau.
  if black:
    var n = 0
    for i in countdown(points.len - 1, 0):
      if n == 2: break
      points[i].output = 0
      inc n
  else:
    for i in 0 ..< min(2, points.len): points[i].output = 1

proc anchorCurve(points: var seq[CurvePoint]; input, output: float32) =
  let (input, output) = (clamp(input, 0'f32, 1'f32), clamp(output, 0'f32, 1'f32))
  var candidate = points
  var found = -1
  for i, p in candidate:
    if abs(p.input - input) < CurvePointDuplicateDistance:
      found = i
      break
  if found >= 0:
    candidate[found].input = input
    candidate[found].output = output
  elif candidate.len < MaxCurvePoints:
    candidate.add CurvePoint(input: input, output: output)
  else:
    # The interior point nearest the input (the first of equal distances).
    var best = -1
    var bestD = 0'f32
    for i in 1 ..< candidate.len - 1:
      let d = abs(candidate[i].input - input)
      if best < 0 or d < bestD:
        best = i
        bestD = d
    if best >= 0: candidate[best] = CurvePoint(input: input, output: output)
  candidate.sort(proc(a, b: CurvePoint): int = cmp(a.input, b.input))
  if curveRoundTripKeepsAnchor(candidate, input, output):
    points = candidate
    return
  # A sample between two points half a level apart: make the bracketing pair a neutral plateau.
  if points.len == 0: return
  let last = points.len - 1
  var split = 0
  while split < points.len and points[split].input < input: inc split
  var lower = min(max(split - 1, 0), last)
  var upper = min(split, last)
  if lower == upper:
    if upper < last: inc upper
    else: lower = max(lower - 1, 0)
  points[lower].output = output
  points[upper].output = output

proc rgbLuma(rgb: array[3, float32]): float32 =
  ## Rec. 709 luma in the sampled display-RGB values.
  rgb[0] * 0.2126'f32 + rgb[1] * 0.7152'f32 + rgb[2] * 0.0722'f32

proc inverseMasterTarget(master: seq[CurvePoint]; target: float32): float32 =
  ## The master-curve input whose output is closest to `target`.
  if isIdentityCurve(master): return target
  let lut = curveLut(master)
  if lut.len < 2: return target
  let denominator = lut.len - 1
  var bestInput = target
  var bestError = Inf.float32
  for index, value in lut:
    let error = abs(value - target)
    if error < bestError:
      bestError = error
      bestInput = float32(index) / float32(denominator)
  bestInput

proc checkEyedropper(cmdId: string; picker: CurvesEyedropper; sample: array[3, float32]; mode: ColorMode) =
  for v in sample:
    if v != v or abs(v) == Inf: raise adjBad(cmdId, "sample components must be finite")
  if mode notin {cmRgb, cmGrayscale}: raise adjBad(cmdId, "only RGB and Grayscale documents are supported")
  if mode == cmGrayscale and picker == ceNeutralGray:
    raise adjBad(cmdId, "Neutral Gray is unavailable for Grayscale documents")

proc setLevelsInput(c: var LevelsChannel; sample: float32; black: bool) =
  if black:
    c.inBlack = clamp(sample, 0'f32, 1 - LevelsMinRange)
    c.inWhite = rmin(rmax(c.inWhite, c.inBlack + LevelsMinRange), 1'f32)
  else:
    c.inWhite = clamp(sample, LevelsMinRange, 1'f32)
    c.inBlack = rmax(rmin(c.inBlack, c.inWhite - LevelsMinRange), 0'f32)

proc setLevelsGamma(c: var LevelsChannel; sample, target: float32) =
  ## Sets the midtone gamma so `sample` maps to `target`, by bisection on the compositor's curve.
  let u = (sample - c.inBlack) / rmax(c.inWhite - c.inBlack, 1e-6'f32)
  let span = c.outWhite - c.outBlack
  let t = (target - c.outBlack) / span
  if not (u > 0 and u < 1) or abs(span) < 1e-6'f32 or not (t > 0 and t < 1): return
  var trial = c
  proc reached(trial: var LevelsChannel; gamma, sample, outBlack, span: float32): float32 =
    trial.gamma = gamma
    (levelsQ(trial, sample) - outBlack) / span
  var (lo, hi) = (ln(0.01'f32), ln(9.99'f32))
  for _ in 0 ..< 40:
    let mid = (lo + hi) / 2
    if reached(trial, exp(mid), sample, c.outBlack, span) < t: lo = mid
    else: hi = mid
  c.gamma = clamp(exp((lo + hi) / 2), 0.01'f32, 9.99'f32)

proc levelsEyedropper*(base: JsonNode; picker: CurvesEyedropper; sample: array[3, float32]; mode: ColorMode): Adjustment =
  ## Photoshop's Levels eyedroppers on the Levels in `base` (params), from a colour sample.
  const cmdId = "levels eyedropper"
  checkEyedropper(cmdId, picker, sample, mode)
  result = adjFromParams("levels", base, none(Adjustment), mode)
  if result.kind != akLevels: raise adjBad(cmdId, "expected a Levels adjustment")
  if result.space != tsRgb: raise adjBad(cmdId, "only RGB channel Levels are supported")
  var smp = sample
  for v in smp.mitems: v = clamp(v, 0'f32, 1'f32)
  if mode == cmGrayscale:
    setLevelsInput(result.levelsMaster, (smp[0] + smp[1] + smp[2]) / 3, picker == ceBlack)
    return
  case picker
  of ceBlack, ceWhite:
    for i in 0 .. 2: setLevelsInput(result.levelsPerChannel[i], smp[i], picker == ceBlack)
  of ceNeutralGray:
    let target = rgbLuma(smp)
    for i in 0 .. 2: setLevelsGamma(result.levelsPerChannel[i], smp[i], target)

proc curvesEyedropper*(base: JsonNode; picker: CurvesEyedropper; sample: array[3, float32]; mode: ColorMode): Adjustment =
  ## The Curves black, luma-preserving neutral-gray or white picker on a colour sample.
  checkEyedropper("curves eyedropper", picker, sample, mode)
  # The pickers place points: a pencil-drawn curve is first fitted with points.
  var b = if base.isNil: newJNull() else: base.copy
  if b.isObject:
    b.dropKey("smooth")
    let pv = b.pget("pencil")
    if not pv.isNil and pv.kind == JBool and pv.bval: b["pencil"] = %false
  result = adjFromParams("curves", b, none(Adjustment), mode)
  if result.kind != akCurves: raise adjBad("curves eyedropper", "expected a Curves adjustment")
  var smp = sample
  for v in smp.mitems: v = clamp(v, 0'f32, 1'f32)
  if mode == cmGrayscale:
    moveCurveEndpoint(result.curvesMaster, (smp[0] + smp[1] + smp[2]) / 3, picker == ceBlack)
    return
  case picker
  of ceBlack, ceWhite:
    let black = picker == ceBlack
    for i in 0 .. 2: moveCurveEndpoint(result.curvesPerChannel[i], smp[i], black)
    if result.curvesMaster.len > 0:
      if black: result.curvesMaster[0].output = 0
      else: result.curvesMaster[^1].output = 1
  of ceNeutralGray:
    let target = inverseMasterTarget(result.curvesMaster, rgbLuma(smp))
    for i in 0 .. 2: anchorCurve(result.curvesPerChannel[i], smp[i], target)

proc adjustmentEyedropper*(kind: string; base: JsonNode; picker: CurvesEyedropper; sample: array[3, float32]; mode: ColorMode): Adjustment =
  case kind
  of "levels": levelsEyedropper(base, picker, sample, mode)
  of "curves": curvesEyedropper(base, picker, sample, mode)
  else: raise adjBad(EyedropperCmd, "`eyedropper` applies to Levels and Curves only")

proc eyedropperSample(doc: Document; hide: Option[LayerId]; e: JsonNode): (CurvesEyedropper, array[3, float32]) =
  ## The picker and sample of an `eyedropper` param; `at` samples `doc` with layer `hide` hidden.
  const cmdId = EyedropperCmd
  let point = e.pget("point")
  let picker = if point.isNil or point.kind != JString: raise adjBad(cmdId, "`eyedropper.point` must be \"black\", \"gray\" or \"white\"")
               else:
                 case point.str
                 of "black": ceBlack
                 of "gray": ceNeutralGray
                 of "white": ceWhite
                 else: raise adjBad(cmdId, "`eyedropper.point` must be \"black\", \"gray\" or \"white\"")
  var sample: array[3, float32]
  let c = e.pget("color")
  let at = e.pget("at")
  if not c.isNil:
    var ok = c.isArray and c.len == 3
    if ok:
      for i in 0 .. 2:
        let f = c.elems[i].asF64
        if f.isNone: ok = false
        else: sample[i] = float32(f.get)
    if not ok: raise adjBad(cmdId, "`eyedropper.color` must be [r, g, b] in 0..1")
  elif not at.isNil:
    var xy: array[2, float64]
    var ok = at.isArray and at.len == 2
    if ok:
      for i in 0 .. 1:
        let f = at.elems[i].asF64
        if f.isNone or f.get != f.get or abs(f.get) == Inf: ok = false
        else: xy[i] = f.get
    if not ok: raise adjBad(cmdId, "`eyedropper.at` must be [x, y] in document pixels")
    let (x, y) = (xy[0], xy[1])
    let (w, h) = (float64(doc.size.width), float64(doc.size.height))
    if x < 0 or y < 0 or x >= w or y >= h: raise adjBad(cmdId, "`eyedropper.at` is outside the image")
    let size = sizeParam(cmdId, e)
    var px: array[4, float32]
    if hide.isSome:
      var below = doc
      let l = below.findLayer(hide.get)
      if not l.isNil: l.visible = false
      px = sampleColor(below, none(LayerId), x, y, size, slAll)
    else: px = sampleColor(doc, none(LayerId), x, y, size, slAll)
    if px[3] <= 0: raise adjBad(cmdId, "the sampled pixel is transparent")
    sample = [px[0], px[1], px[2]]
  else: raise adjBad(cmdId, "`eyedropper` needs `at` [x, y] or `color` [r, g, b]")
  (picker, sample)

proc eyedropperFromParams*(s: Session; kind: string; p: JsonNode): Option[Adjustment] =
  ## The `eyedropper` param of the Levels and Curves commands: `{"point":"black"|"gray"|"white",
  ## "at":[x,y]}` samples the merged composite, or `"color":[r,g,b]` gives the sample directly.
  let e = p.pget("eyedropper")
  if e.isNil: return
  let d = s.active
  if d.isNil: raise noDocument()
  let (picker, sample) = eyedropperSample(d.doc[], none(LayerId), e)
  var base = p.copy
  if base.isObject: base.dropKey("eyedropper")
  some(adjustmentEyedropper(kind, base, picker, sample, d.doc.mode))

proc adjUserKeys*(p: JsonNode): seq[string] =
  ## Params with the private keys (`layer`, `coalesce`, `channel`, `targetChannel`, `__…`) removed.
  if not p.isObject: return
  for k, _ in p.sortedPairs:
    if k notin ["layer", "coalesce", "channel", "targetChannel"] and not k.startsWith("__"): result.add k

proc layerEyedropper*(s: Session; id: LayerId; p: JsonNode): Option[Adjustment] =
  ## The `eyedropper` param of `layer.setAdjustment` on a Levels or Curves adjustment layer: `at`
  ## samples the image the layer adjusts (the composite with the layer hidden).
  let e = p.pget("eyedropper")
  if e.isNil: return
  let d = s.active
  if d.isNil: raise noDocument()
  let l = d.doc[].findLayer(id)
  if l.isNil: raise noLayer(id)
  if l.content.kind != lkAdjustment or l.content.adjustment.kind notin {akCurves, akLevels}:
    raise adjBad(EyedropperCmd, "`eyedropper` applies to Levels and Curves adjustment layers only")
  let adj = l.content.adjustment
  let kind = if adj.kind == akCurves: "curves" else: "levels"
  var rest = p.copy
  if rest.isObject: rest.dropKey("eyedropper")
  let current = if adjUserKeys(rest).len == 0: adj else: adjFromParams(kind, rest, some(adj), d.doc.mode)
  let (picker, sample) = eyedropperSample(d.doc[], some(id), e)
  some(adjustmentEyedropper(kind, adjToParams(current), picker, sample, d.doc.mode))
