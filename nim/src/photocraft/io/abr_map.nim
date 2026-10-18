## Photoshop brushes (`.abr`) → brush presets (port of `io/src/abr_map.rs`).
##
## `psd/abr` reads the file; this module maps its tips and `brushPreset` descriptors onto
## `BrushSettings` (Brush Tip Shape, Shape Dynamics, Scattering, Texture, Dual Brush, Color
## Dynamics, Transfer, Brush Pose, Noise, Wet Edges, Build-up, Smoothing and the captured tool
## options). Settings without an equivalent are listed in `AbrImport.warnings` instead of being
## dropped silently. Descriptor key names follow the publicly documented ABR v6+ layout.

import std/[options, sets, strutils, algorithm, math]
import ../raster, ../rmath, ../textutil
import ../psd/[abr, descriptor, patterns, io]
import ../paint/brush

const
  MaxTipEdge* = 2500'u32     ## largest sampled tip edge kept as is; bigger tips are downsampled
  MaxPatternEdge* = 1024'u32 ## largest texture pattern edge kept as is

type
  AbrImport* = object
    ## The presets read from one file plus everything that could not be mapped.
    presets*: seq[BrushPreset]
    warnings*: seq[string]
    version*: uint16 ## file version (1, 2, 6…)

# ---------------------------------------------------------------- descriptor helpers

proc num(d: Descriptor; k: string): Option[float64] =
  let v = d.get(k)
  if v.isNone: return
  let x = v.get
  let f = case x.kind
    of vkDouble: x.d
    of vkUnitFloat: x.uv
    of vkInteger: float64(x.i)
    of vkLargeInteger: float64(x.li)
    else: return
  if f == f and f != Inf and f != NegInf: result = some(f)

proc pct(d: Descriptor; k: string): Option[float32] =
  ## A percentage as a 0..1 fraction.
  let v = num(d, k)
  if v.isSome: result = some(float32(v.get / 100))

proc boolean(d: Descriptor; k: string): Option[bool] =
  let v = d.get(k)
  if v.isSome and v.get.kind == vkBoolean: result = some(v.get.b)

proc text(d: Descriptor; k: string): Option[string] =
  let v = d.get(k)
  if v.isSome and v.get.kind == vkText: result = some($v.get.text)

proc obj(d: Descriptor; k: string): Option[Descriptor] =
  let v = d.get(k)
  if v.isSome and v.get.kind in {vkDescriptor, vkGlobalObject}: result = some(v.get.desc)

proc enumValue(d: Descriptor; k: string): Option[string] =
  let v = d.get(k)
  if v.isSome and v.get.kind == vkEnumerated: result = some(lossyUtf8(v.get.enumValue.s.toOpenArrayByte(0, v.get.enumValue.s.high)))

proc maskMode(code: Option[string]): Option[MaskMode] =
  if code.isNone: return
  case code.get
  of "Mltp": some(mmMultiply)
  of "Sbtr": some(mmSubtract)
  of "Drkn": some(mmDarken)
  of "Ovrl": some(mmOverlay)
  of "CDdg": some(mmColorDodge)
  of "CBrn": some(mmColorBurn)
  of "linearBurn": some(mmLinearBurn)
  of "hardMix": some(mmHardMix)
  of "linearHeight": some(mmLinearHeight)
  of "height", "Hght": some(mmHeight)
  else: none(MaskMode)

const Known = ["Nm  ", "Brsh", "useTipDynamics", "flipX", "flipY", "brushProjection", "minimumDiameter",
  "minimumRoundness", "tiltScale", "szVr", "angleDynamics", "roundnessDynamics", "useScatter", "Spcn",
  "Cnt ", "bothAxes", "countDynamics", "scatterDynamics", "dualBrush", "brushGroup", "useTexture",
  "TxtC", "interpretation", "textureBlendMode", "textureDepth", "minimumDepth", "textureDepthDynamics",
  "Txtr", "textureScale", "InvT", "protectTexture", "textureBrightness", "textureContrast",
  "usePaintDynamics", "prVr", "opVr", "wtVr", "mxVr", "useColorDynamics", "clVr", "H   ", "Strt",
  "Brgh", "purity", "colorDynamicsPerTip", "Wtdg", "Nose", "Rpt ", "useLegacy", "useBrushPose",
  "overridePoseAngle", "overridePoseTiltX", "overridePoseTiltY", "overridePosePressure",
  "brushPosePressure", "brushPoseTiltX", "brushPoseTiltY", "brushPoseAngle", "toolOptions"]
  ## Preset-level keys this mapper understands (anything else is reported as unmapped).

proc q16(x: float32): uint16 {.inline.} = asU16(clamp(x, 0'f32, 1'f32) * 65535'f32 + 0.5'f32)

proc fit(w, h: uint32; v: seq[float32]; mx: uint32): (uint32, uint32, seq[float32]) =
  ## Box-downsample a `w × h` plane so neither side exceeds `mx`.
  if w <= mx and h <= mx: return (w, h, v)
  let k = rmax(float32(max(w, h)) / float32(mx), 1'f32)
  let nw = clamp(asU32(round(float32(w) / k)), 1'u32, mx)
  let nh = clamp(asU32(round(float32(h) / k)), 1'u32, mx)
  var outp = newSeq[float32](int(nw) * int(nh))
  for y in 0'u64 ..< uint64(nh):
    let y0 = int(y * h div nh)
    let y1 = min(max(int((y + 1) * h div nh), y0 + 1), int(h))
    for x in 0'u64 ..< uint64(nw):
      let x0 = int(x * w div nw)
      let x1 = min(max(int((x + 1) * w div nw), x0 + 1), int(w))
      var sum = 0'f32
      var n = 0
      for yy in y0 ..< y1:
        for xx in x0 ..< x1:
          let i = yy * int(w) + xx
          if i < v.len:
            sum += v[i]
            inc n
      outp[int(y) * int(nw) + int(x)] = if n > 0: sum / float32(n) else: 0'f32
  (nw, nh, outp)

proc patternTile(p: PsdPattern): Option[GrayTile] =
  ## A pattern as a grayscale texture tile (luminance; transparent areas count as white).
  let (w, h) = (p.width, p.height)
  if w <= 0 or h <= 0: return
  let n = w * h
  proc plane(c: int): Option[seq[float32]] =
    if c >= p.channels.len: return
    let ch = p.channels[c]
    var v: seq[float32]
    case p.depth
    of 8:
      for i in 0 ..< min(n, ch.len): v.add float32(ch[i]) / 255'f32
    of 16:
      for i in 0 ..< min(n, ch.len div 2): v.add float32(beU16(ch, 2 * i)) / 65535'f32
    of 32:
      for i in 0 ..< min(n, ch.len div 4): v.add clamp(cast[float32](beU32(ch, 4 * i)), 0'f32, 1'f32)
    else: return
    if v.len == n: result = some(v)
  var gray = newSeq[float32](n)
  case p.mode
  of 3:
    let (r, g, b) = (plane(0), plane(1), plane(2))
    if r.isNone or g.isNone or b.isNone: return
    for i in 0 ..< n: gray[i] = 0.299'f32 * r.get[i] + 0.587'f32 * g.get[i] + 0.114'f32 * b.get[i]
  of 2:
    if p.palette.isNone or p.channels.len == 0: return
    let pal = p.palette.get
    let idx = p.channels[0]
    for i in 0 ..< n:
      let k = int(if i < idx.len: idx[i] else: 0'u8) * 3
      proc c(o: int): float32 = float32(if k + o < pal.len: pal[k + o] else: 0'u8) / 255'f32
      gray[i] = 0.299'f32 * c(0) + 0.587'f32 * c(1) + 0.114'f32 * c(2)
  of 4:
    # CMYK planes are stored inverted (0 = full ink).
    let (k, c) = (plane(3), plane(0))
    if k.isNone or c.isNone: return
    for i in 0 ..< n: gray[i] = clamp(c.get[i] * k.get[i], 0'f32, 1'f32)
  else:
    let g = plane(0)
    if g.isNone: return
    gray = g.get
  let (fw, fh, fg) = fit(uint32(w), uint32(h), gray, MaxPatternEdge)
  var t = GrayTile(width: fw, height: fh, data: newSeq[uint16](fg.len))
  for i, x in fg: t.data[i] = q16(x)
  some(t)

# ---------------------------------------------------------------- mapper

type Mapper = object
  warnings: seq[string]   ## sorted, unique (a `BTreeSet` in the Rust crate)
  unknown: seq[string]

proc note(set: var seq[string]; s: string) =
  let i = set.lowerBound(s)
  if i >= set.len or set[i] != s: set.insert(s, i)

proc tip(m: var Mapper; s: AbrSample): (TipShape, float32) =
  ## A sampled tip (downsampled to `MaxTipEdge`) and its size (larger side).
  let (sw, sh) = (uint32(max(s.width, 0)), uint32(max(s.height, 0)))
  let (w, h, v) = fit(sw, sh, s.toUnit, MaxTipEdge)
  if (w, h) != (sw, sh): m.warnings.note("tips larger than " & $MaxTipEdge & " px were downsampled")
  var g = GrayTile(width: w, height: h, data: newSeq[uint16](v.len))
  for i, x in v: g.data[i] = q16(x)
  (TipShape(kind: tkSampled, sampled: g), float32(max(sw, sh)))

proc dynamic(m: var Mapper; d: Option[Descriptor]): Dynamic =
  if d.isNone: return defaultDynamic()
  let dd = d.get
  let control =
    case asI64(num(dd, "bVTy").get(0))
    of 1: ctlFade
    of 2: ctlPenPressure
    of 3: ctlPenTilt
    of 4: ctlStylusWheel
    of 5: ctlInitialDirection
    of 6: ctlDirection
    of 7:
      m.warnings.note("\"Initial Rotation\" control mapped to Rotation")
      ctlRotation
    of 8: ctlRotation
    else: ctlOff
  let fs = num(dd, "fStp")
  Dynamic(jitter: rmax(pct(dd, "jitter").get(0), 0'f32), control: control,
          fadeSteps: (if fs.isSome: uint32(clamp(fs.get, 1.0, 9999.0)) else: 25'u32),
          minimum: clamp(pct(dd, "Mnm ").get(0), 0'f32, 1'f32))

proc tipShape(m: var Mapper; f: AbrFile; t: Descriptor): Option[(TipShape, Option[float32])] =
  ## Tip-shape fields from a `computedBrush` / `sampledBrush` object; none when the sampled tip it
  ## references is missing.
  let sampled = t.classId.isId("sampledBrush") or t.get("sampledData").isSome
  if not sampled: return some((TipShape(kind: tkRound), none(float32)))
  let id = text(t, "sampledData").get("")
  var found = -1
  for i, s in f.samples:
    if s.id == id:
      found = i
      break
  if found < 0 and f.samples.len == 1: found = 0
  if found < 0: return none((TipShape, Option[float32]))
  let (tp, size) = m.tip(f.samples[found])
  some((tp, some(size)))

proc pattern(m: var Mapper; f: AbrFile; t: Descriptor): Pattern =
  let id = text(t, "Idnt").get("")
  let name = text(t, "Nm  ").get("")
  var found = -1
  if id.len > 0:
    for i, p in f.patterns:
      if p.id == id:
        found = i
        break
  if found < 0 and name.len > 0:
    for i, p in f.patterns:
      if p.name == name:
        found = i
        break
  let tile = if found >= 0: patternTile(f.patterns[found]) else: none(GrayTile)
  if tile.isSome: return Pattern(kind: pkTile, tile: tile.get)
  let label = if name.len == 0: id else: name
  m.warnings.note("texture pattern \"" & label & "\" is not embedded in the file; a procedural paper texture stands in")
  procedural(psPaper, 128, 1)

proc mixerOptions(m: var Mapper; to: Descriptor; mx: var MixerSettings) =
  ## Mixer Brush tool options captured with a preset (Wet, Load, Mix, Flow, Sample All Layers).
  ## Key names are best effort (Photoshop doesn't document them); several spellings are read.
  proc first(keys: openArray[string]): Option[float32] =
    for k in keys:
      let v = pct(to, k)
      if v.isSome: return v
  let (wet, load, mix) = (first(["wetness", "Wtns", "wet"]), first(["dryness", "load", "Ld  "]), first(["mix", "mixRatio", "Mx  "]))
  if wet.isNone and load.isNone and mix.isNone: return
  mx.wet = clamp(wet.get(mx.wet), 0'f32, 1'f32)
  mx.load = clamp(load.get(mx.load), 0'f32, 1'f32)
  mx.mix = clamp(mix.get(mx.mix), 0'f32, 1'f32)
  mx.flow = clamp(pct(to, "flow").get(mx.flow), 0'f32, 1'f32)
  for k in ["sampleMerged", "useAllLayers", "sampleAllLayers", "Mrgd"]:
    let v = boolean(to, k)
    if v.isSome:
      mx.sampleAllLayers = v.get
      break

proc maskBlendIsSet(to: Descriptor): bool =
  let m = enumValue(to, "Md  ")
  m.isSome and m.get != "Nrml"

proc preset(m: var Mapper; f: AbrFile; d: Descriptor): Option[BrushSettings] =
  for (k, _) in d.items:
    let key = lossyUtf8(k.s.toOpenArrayByte(0, k.s.high))
    if key notin Known: m.unknown.note(key)
  var b = defaultBrush()
  b.pressureSize = false
  b.spacing = 0.25
  let brsh = obj(d, "Brsh")
  if brsh.isSome:
    let t = brsh.get
    let ts = m.tipShape(f, t)
    if ts.isNone: return none(BrushSettings)
    let (tp, sampledSize) = ts.get
    b.tip = tp
    let dm = num(t, "Dmtr")
    b.size = clamp((if dm.isSome: float32(dm.get) elif sampledSize.isSome: sampledSize.get else: b.size), 1'f32, 5000'f32)
    let hd = pct(t, "Hrdn")
    if hd.isSome: b.hardness = clamp(hd.get, 0'f32, 1'f32)
    b.angle = float32(num(t, "Angl").get(0))
    b.roundness = clamp(pct(t, "Rndn").get(1), 0.01'f32, 1'f32)
    # `Intr` off = the Spacing checkbox is off: the pointer's speed sets the spacing.
    b.spacing = clamp(pct(t, "Spcn").get(0.25), 0.01'f32, 10'f32)
    b.spacingEnabled = boolean(t, "Intr") != some(false)
    b.flipX = boolean(t, "flipX").get(false)
    b.flipY = boolean(t, "flipY").get(false)
  # Shape Dynamics.
  if boolean(d, "useTipDynamics") == some(true):
    var size = m.dynamic(obj(d, "szVr"))
    size.minimum = clamp(pct(d, "minimumDiameter").get(size.minimum), 0'f32, 1'f32)
    var roundness = m.dynamic(obj(d, "roundnessDynamics"))
    roundness.minimum = clamp(pct(d, "minimumRoundness").get(roundness.minimum), 0'f32, 1'f32)
    b.shapeDynamics = ShapeDynamics(enabled: true, size: size, angle: m.dynamic(obj(d, "angleDynamics")),
                                    roundness: roundness, flipXJitter: boolean(d, "flipX").get(false),
                                    flipYJitter: boolean(d, "flipY").get(false),
                                    tiltScale: clamp(pct(d, "tiltScale").get(0), 0'f32, 2'f32),
                                    brushProjection: boolean(d, "brushProjection").get(false))
  # Scattering.
  if boolean(d, "useScatter") == some(true):
    b.scattering.enabled = true
    b.scattering.scatter = m.dynamic(obj(d, "scatterDynamics"))
    b.scattering.bothAxes = boolean(d, "bothAxes").get(false)
    let c = num(d, "Cnt ")
    b.scattering.count = if c.isSome: uint32(clamp(c.get, 1.0, 16.0)) else: 1'u32
    b.scattering.countJitter = m.dynamic(obj(d, "countDynamics"))
  # Texture.
  if boolean(d, "useTexture") == some(true):
    b.texture.enabled = true
    b.texture.eachTip = boolean(d, "TxtC").get(false)
    b.texture.invert = boolean(d, "InvT").get(false)
    b.texture.scale = clamp(pct(d, "textureScale").get(1), 0.01'f32, 10'f32)
    b.texture.brightness = float32(clamp(num(d, "textureBrightness").get(0) / 150, -1.0, 1.0))
    b.texture.contrast = float32(clamp(num(d, "textureContrast").get(0) / 50, -1.0, 2.0))
    b.texture.mode = maskMode(enumValue(d, "textureBlendMode")).get(mmMultiply)
    b.texture.depth = clamp(pct(d, "textureDepth").get(1), 0'f32, 1'f32)
    var dj = m.dynamic(obj(d, "textureDepthDynamics"))
    dj.minimum = clamp(pct(d, "minimumDepth").get(dj.minimum), 0'f32, 1'f32)
    b.texture.depthJitter = dj
    let p = obj(d, "Txtr")
    if p.isSome: b.texture.pattern = m.pattern(f, p.get)
  b.protectTexture = boolean(d, "protectTexture").get(false)
  # Dual Brush.
  let db0 = obj(d, "dualBrush")
  if db0.isSome and boolean(db0.get, "useDualBrush") == some(true):
    let db = db0.get
    var dual = defaultDualBrush()
    dual.enabled = true
    let t0 = obj(db, "Brsh")
    if t0.isSome:
      let t = t0.get
      let ts = m.tipShape(f, t)
      if ts.isSome:
        let (tp, sampledSize) = ts.get
        dual.tip = tp
        let dm = num(t, "Dmtr")
        dual.size = clamp((if dm.isSome: float32(dm.get) elif sampledSize.isSome: sampledSize.get else: dual.size), 1'f32, 5000'f32)
      else:
        m.warnings.note("a dual brush tip is missing from the file; a round tip stands in")
      dual.hardness = clamp(pct(t, "Hrdn").get(1), 0'f32, 1'f32)
      dual.roundness = clamp(pct(t, "Rndn").get(1), 0.01'f32, 1'f32)
      dual.angle = float32(num(t, "Angl").get(0))
      dual.spacing = clamp(pct(t, "Spcn").get(dual.spacing), 0.01'f32, 10'f32)
    dual.mode = maskMode(enumValue(db, "BlnM")).get(mmMultiply)
    dual.flip = boolean(db, "Flip").get(false)
    let sp = pct(db, "Spcn")
    if sp.isSome: dual.spacing = clamp(sp.get, 0.01'f32, 10'f32)
    dual.scatter = m.dynamic(obj(db, "scatterDynamics")).jitter
    dual.bothAxes = boolean(db, "bothAxes").get(false)
    let c = num(db, "Cnt ")
    dual.count = if c.isSome: uint32(clamp(c.get, 1.0, 16.0)) else: 1'u32
    b.dualBrush = dual
  # Color Dynamics.
  if boolean(d, "useColorDynamics") == some(true):
    b.colorDynamics.enabled = true
    b.colorDynamics.perTip = boolean(d, "colorDynamicsPerTip").get(true)
    b.colorDynamics.hueJitter = clamp(pct(d, "H   ").get(0), 0'f32, 1'f32)
    b.colorDynamics.saturationJitter = clamp(pct(d, "Strt").get(0), 0'f32, 1'f32)
    b.colorDynamics.brightnessJitter = clamp(pct(d, "Brgh").get(0), 0'f32, 1'f32)
    b.colorDynamics.purity = clamp(pct(d, "purity").get(0), -1'f32, 1'f32)
    b.colorDynamics.fgBg = m.dynamic(obj(d, "clVr"))
  # Transfer.
  if boolean(d, "usePaintDynamics") == some(true):
    b.transfer = Transfer(enabled: true, opacity: m.dynamic(obj(d, "opVr")), flow: m.dynamic(obj(d, "prVr")),
                          wetness: m.dynamic(obj(d, "wtVr")), mix: m.dynamic(obj(d, "mxVr")))
  # Brush Pose (Photoshop's tilt is ±100 %; ours is ±90°).
  if boolean(d, "useBrushPose") == some(true):
    b.pose = Pose(enabled: true,
                  tiltX: float32(clamp(num(d, "brushPoseTiltX").get(0) * 0.9, -90.0, 90.0)),
                  tiltY: float32(clamp(num(d, "brushPoseTiltY").get(0) * 0.9, -90.0, 90.0)),
                  rotation: float32(remEuclidF(num(d, "brushPoseAngle").get(0), 360.0)),
                  pressure: clamp(pct(d, "brushPosePressure").get(1), 0'f32, 1'f32),
                  overrideTilt: boolean(d, "overridePoseTiltX").get(false) or boolean(d, "overridePoseTiltY").get(false),
                  overrideRotation: boolean(d, "overridePoseAngle").get(false),
                  overridePressure: boolean(d, "overridePosePressure").get(false))
  b.noise = boolean(d, "Nose").get(false)
  b.wetEdges = boolean(d, "Wtdg").get(false)
  b.buildUp = boolean(d, "Rpt ").get(false)
  # Tool options captured with the preset ("Include tool settings").
  let to0 = obj(d, "toolOptions")
  if to0.isSome:
    let to = to0.get
    let op = pct(to, "Opct")
    if op.isSome: b.opacity = clamp(op.get, 0'f32, 1'f32)
    let fl = pct(to, "flow")
    if fl.isSome: b.flow = clamp(fl.get, 0'f32, 1'f32)
    b.pressureSize = boolean(to, "usePressureOverridesSize").get(false)
    b.pressureOpacity = boolean(to, "usePressureOverridesOpacity").get(false)
    if boolean(to, "smoothing").get(true):
      let v = pct(to, "smoothingValue")
      if v.isSome: b.smoothing.amount = clamp(v.get, 0'f32, 1'f32)
    let rm = boolean(to, "smoothingRadiusMode")
    if rm.isSome: b.smoothing.pulledString = rm.get
    let cu = boolean(to, "smoothingCatchup")
    if cu.isSome: b.smoothing.catchUp = cu.get
    let ce = boolean(to, "smoothingCatchupAtEnd")
    if ce.isSome: b.smoothing.catchUpOnEnd = ce.get
    let zc = boolean(to, "smoothingZoomCompensation")
    if zc.isSome: b.smoothing.adjustForZoom = zc.get
    m.mixerOptions(to, b.mixer)
    if maskBlendIsSet(to): m.warnings.note("tool blend modes stored with presets are not applied")
  some(b)

proc mapFileWith*(f: AbrFile; group: string; ctl: Interrupt): AbrImport =
  ## Map a parsed file, checking `ctl` before each brush (raises `Cancelled`).
  result = AbrImport(version: f.version, warnings: f.warnings)
  var m = Mapper()
  let total = float32(max(max(max(f.legacy.len, f.samples.len), f.presets.len), 1))
  proc step(i: int) =
    ctl.check()
    ctl.progress(float32(i) / total)
  if f.legacy.len > 0:
    for i, b in f.legacy:
      step(i)
      var base = defaultBrush()
      base.pressureSize = false
      base.spacing = if b.spacing == 0: 0.25'f32 else: float32(b.spacing) / 100'f32
      var brush = base
      var defaultName: string
      case b.tip.kind
      of ltComputed:
        brush.size = float32(max(b.tip.diameter, 1'u16))
        brush.hardness = clamp(float32(b.tip.hardness) / 100'f32, 0'f32, 1'f32)
        brush.angle = float32(b.tip.angle)
        brush.roundness = clamp(float32(b.tip.roundness) / 100'f32, 0.01'f32, 1'f32)
        defaultName = $b.tip.diameter & " px Round"
      of ltSampled:
        let (tp, size) = m.tip(b.tip.sample)
        brush.size = size
        brush.tip = tp
        brush.aliased = not b.antiAlias
        defaultName = "Sampled Brush " & $(i + 1)
      let name = if b.name.strip.len == 0: defaultName else: b.name.strip
      result.presets.add BrushPreset(name: name, brush: brush, group: group)
  elif f.presets.len == 0:
    # Tips without a settings section (early v6 files): one preset per tip.
    for i, s in f.samples:
      step(i)
      let (tp, size) = m.tip(s)
      var brush = defaultBrush()
      brush.pressureSize = false
      brush.spacing = 0.25
      brush.size = size
      brush.tip = tp
      result.presets.add BrushPreset(name: "Sampled Brush " & $(i + 1), brush: brush, group: group)
  else:
    for i, d in f.presets:
      step(i)
      let nm = text(d, "Nm  ")
      let name = if nm.isSome and nm.get.strip.len > 0: nm.get.strip else: "Brush " & $(i + 1)
      let b = m.preset(f, d)
      if b.isSome:
        # The file's folders nest inside `group`, as Photoshop nests them inside a folder named
        # after the imported file.
        let folder = if i < f.folders.len: f.folders[i] else: @[]
        result.presets.add BrushPreset(name: name, brush: b.get, group: group, folder: folder)
      else:
        m.warnings.note("\"" & name & "\": its sampled tip is missing from the file; skipped")
  result.warnings.add m.warnings
  if m.unknown.len > 0:
    result.warnings.add "settings without a PhotoCraft equivalent were ignored: " & m.unknown.join(", ")
  ctl.progress(1)

proc mapFile*(f: AbrFile; group: string): AbrImport =
  ## Map a parsed file (never cancelled).
  mapFileWith(f, group, NoInterrupt)

proc readAbrWith*(bytes: openArray[byte]; group: string; ctl: Interrupt): AbrImport =
  ## `readAbr` that checks `ctl` before each brush (and reports progress). Raises ValueError
  ## ("not a readable brush file (.abr): …", or "cancelled").
  var f: AbrFile
  try: f = parseAbr(bytes)
  except CatchableError as e: raise newException(ValueError, "not a readable brush file (.abr): " & e.msg)
  # Parsing is the small part; mapping (tip decoding) reports 10–100 %.
  ctl.progress(0.1)
  let inner = newInterrupt(proc(): bool = ctl.cancelled,
                           proc(p: float32) = ctl.progress(0.1'f32 + 0.9'f32 * p))
  try: result = mapFileWith(f, group, inner)
  except Cancelled: raise newException(ValueError, "cancelled")

proc readAbr*(bytes: openArray[byte]; group: string): AbrImport =
  ## Parse an `.abr` file into presets in group `group` (usually the file name). The file's own
  ## folders (Photoshop CC's preset hierarchy) become each preset's `folder` inside that group.
  readAbrWith(bytes, group, NoInterrupt)
