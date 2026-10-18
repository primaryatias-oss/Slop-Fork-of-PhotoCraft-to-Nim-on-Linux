## Photoshop brushes import (`io/src/abr_map/tests.rs`): v6 settings onto every brush section,
## missing patterns and tips, file folders, v1/v2 brushes, tips without settings, big tips,
## Tilt Scale / Projection / Mixer fields, garbage input, and sampled tips painting the brush
## colour.

import std/[unittest, options, sequtils, strutils]
import photocraft/[geom, color, raster]
import photocraft/psd/[abr, descriptor, patterns]
import photocraft/paint
import photocraft/io/abr_map

proc sample(id: string; w, h, depth: int): AbrSample =
  let bpp = depth div 8
  var data: seq[byte]
  for i in 0 ..< w * h:
    for _ in 0 ..< bpp: data.add(if i mod 2 == 0: 255'u8 else: 0'u8)
  AbrSample(id: id, width: w, height: h, depth: depth, data: data)

proc t(s: string): Value = vText(s)
proc u(unit: string; v: float64): Value = vUnit(unit, v)
proc prc(v: float64): Value = u("#Prc", v)
proc en(ty, v: string): Value = vEnum(ty, v)
proc vr(control: int32; jitter, minimum: float64): Value =
  vDesc(newDescriptor("brVr").with("bVTy", vInt(control)).with("fStp", vInt(40)).with("jitter", prc(jitter)).with("Mnm ", prc(minimum)))

proc sampledTip(id: string): Descriptor =
  newDescriptor("sampledBrush").with("Dmtr", u("#Pxl", 64)).with("Angl", u("#Ang", 30)).with("Rndn", prc(80))
    .with("Spcn", prc(40)).with("Intr", vBool(true)).with("flipX", vBool(true)).with("flipY", vBool(false))
    .with("sampledData", t(id))

proc fullPreset(): Descriptor =
  ## A preset touching every section.
  newDescriptor("brushPreset").with("Nm  ", t("Kitchen Sink")).with("Brsh", vDesc(sampledTip("$tip-1")))
    .with("useTipDynamics", vBool(true)).with("flipX", vBool(true)).with("flipY", vBool(false))
    .with("minimumDiameter", prc(25)).with("minimumRoundness", prc(10)).with("tiltScale", prc(0))
    .with("szVr", vr(2, 20, 0)).with("angleDynamics", vr(6, 10, 0)).with("roundnessDynamics", vr(3, 5, 0))
    .with("useScatter", vBool(true)).with("Cnt ", vInt(3)).with("bothAxes", vBool(true))
    .with("countDynamics", vr(0, 50, 0)).with("scatterDynamics", vr(1, 250, 0))
    .with("useTexture", vBool(true)).with("TxtC", vBool(true)).with("InvT", vBool(true))
    .with("textureScale", prc(150)).with("textureBrightness", vInt(-75)).with("textureContrast", vInt(25))
    .with("textureBlendMode", en("BlnM", "CBrn")).with("textureDepth", prc(60)).with("minimumDepth", prc(20))
    .with("textureDepthDynamics", vr(2, 30, 0))
    .with("Txtr", vDesc(newDescriptor("Ptrn").with("Nm  ", t("Grit")).with("Idnt", t("pat-uuid"))))
    .with("dualBrush", vDesc(newDescriptor("dualBrush").with("useDualBrush", vBool(true)).with("Flip", vBool(true))
      .with("Brsh", vDesc(newDescriptor("computedBrush").with("Dmtr", u("#Pxl", 12)).with("Hrdn", prc(50))))
      .with("BlnM", en("BlnM", "hardMix")).with("Spcn", prc(30)).with("Cnt ", vInt(2)).with("bothAxes", vBool(true))
      .with("scatterDynamics", vr(0, 120, 0))))
    .with("useColorDynamics", vBool(true)).with("clVr", vr(2, 40, 0)).with("H   ", prc(10)).with("Strt", prc(20))
    .with("Brgh", prc(30)).with("purity", prc(-50)).with("colorDynamicsPerTip", vBool(false))
    .with("usePaintDynamics", vBool(true)).with("opVr", vr(2, 0, 10)).with("prVr", vr(1, 15, 5)).with("wtVr", vr(0, 0, 0))
    .with("useBrushPose", vBool(true)).with("brushPoseTiltX", vInt(50)).with("brushPoseAngle", vInt(90))
    .with("overridePoseAngle", vBool(true)).with("Wtdg", vBool(true)).with("Nose", vBool(true)).with("Rpt ", vBool(true))
    .with("someFutureKey", vBool(true))
    .with("toolOptions", vDesc(newDescriptor("currentToolOptions").with("Opct", vInt(80)).with("flow", vInt(60))
      .with("smoothing", vBool(true)).with("smoothingValue", vInt(40)).with("usePressureOverridesSize", vBool(true))))

proc gritPattern(): PsdPattern =
  PsdPattern(mode: 1, width: 4, height: 2, name: "Grit", id: "pat-uuid", depth: 8,
             channels: @[@[0'u8, 64, 128, 255, 255, 128, 64, 0]])

suite "ABR import":
  test "v6 settings map onto brush sections":
    for sub in [1'u16, 2]:
      let computed = newDescriptor("brushPreset").with("Nm  ", t("Round 19")).with("Brsh",
        vDesc(newDescriptor("computedBrush").with("Dmtr", u("#Pxl", 19)).with("Hrdn", prc(0)).with("Spcn", prc(10))))
      let bytes = writeAbrV6(sub, [sample("$tip-1", 16, 8, 8)], [gritPattern()], [fullPreset(), computed], [], true)
      let imp = readAbr(bytes, "Test")
      check imp.presets.len == 2
      let p = imp.presets[0]
      check (p.name, p.group, p.builtin) == ("Kitchen Sink", "Test", false)
      let b = p.brush
      check b.tip.kind == tkSampled
      check (b.tip.sampled.width, b.tip.sampled.height) == (16'u32, 8'u32)
      check (b.tip.sampled.data[0], b.tip.sampled.data[1]) == (65535'u16, 0'u16)
      check (b.size, b.angle, b.roundness, b.spacing, b.flipX, b.flipY) == (64'f32, 30'f32, 0.8'f32, 0.4'f32, true, false)
      let sd = b.shapeDynamics
      check sd.enabled and sd.flipXJitter and not sd.flipYJitter
      check (sd.size.control, sd.size.jitter, sd.size.minimum) == (ctlPenPressure, 0.2'f32, 0.25'f32)
      check (sd.angle.control, sd.roundness.control, sd.roundness.minimum) == (ctlDirection, ctlPenTilt, 0.1'f32)
      let sc = b.scattering
      check sc.enabled and sc.bothAxes
      check (sc.count, sc.scatter.jitter, sc.scatter.control, sc.scatter.fadeSteps, sc.countJitter.jitter) ==
        (3'u32, 2.5'f32, ctlFade, 40'u32, 0.5'f32)
      let tx = b.texture
      check tx.enabled and tx.eachTip and tx.invert
      check (tx.scale, tx.brightness, tx.contrast, tx.mode, tx.depth) == (1.5'f32, -0.5'f32, 0.5'f32, mmColorBurn, 0.6'f32)
      check (tx.depthJitter.jitter, tx.depthJitter.minimum) == (0.3'f32, 0.2'f32)
      check tx.pattern.kind == pkTile
      check (tx.pattern.tile.width, tx.pattern.tile.height, tx.pattern.tile.data[3]) == (4'u32, 2'u32, 65535'u16)
      let db = b.dualBrush
      check db.enabled and db.flip and db.bothAxes and db.tip == TipShape(kind: tkRound)
      check (db.size, db.hardness, db.mode, db.spacing, db.count, db.scatter) == (12'f32, 0.5'f32, mmHardMix, 0.3'f32, 2'u32, 1.2'f32)
      let cd = b.colorDynamics
      check cd.enabled and not cd.perTip
      check (cd.fgBg.jitter, cd.hueJitter, cd.saturationJitter, cd.brightnessJitter, cd.purity) ==
        (0.4'f32, 0.1'f32, 0.2'f32, 0.3'f32, -0.5'f32)
      check b.transfer.enabled
      check (b.transfer.opacity.control, b.transfer.opacity.minimum) == (ctlPenPressure, 0.1'f32)
      check (b.transfer.flow.control, b.transfer.flow.jitter) == (ctlFade, 0.15'f32)
      check b.pose.enabled and b.pose.overrideRotation and not b.pose.overrideTilt
      check (b.pose.tiltX, b.pose.rotation) == (45'f32, 90'f32)
      check b.wetEdges and b.noise and b.buildUp
      check (b.opacity, b.flow, b.smoothing.amount, b.pressureSize) == (0.8'f32, 0.6'f32, 0.4'f32, true)
      check imp.warnings.anyIt("someFutureKey" in it)
      let r = imp.presets[1].brush
      check (r.tip == TipShape(kind: tkRound), r.size, r.hardness, r.spacing, r.pressureSize) == (true, 19'f32, 0'f32, 0.1'f32, false)

  test "a missing pattern and tip are reported":
    let orphan = newDescriptor("brushPreset").with("Nm  ", t("Orphan")).with("Brsh", vDesc(sampledTip("$nowhere")))
    let bytes = writeAbrV6(2, [sample("$tip-1", 4, 4, 16), sample("$tip-2", 4, 4, 8)], [], [fullPreset(), orphan], [], false)
    let imp = readAbr(bytes, "x")
    check imp.presets.len == 1
    check imp.presets[0].brush.texture.pattern.kind == pkProcedural
    check imp.warnings.anyIt("Grit" in it)
    check imp.warnings.anyIt("Orphan" in it)

  test "file folders nest inside the import group":
    proc named(n: string): Descriptor = newDescriptor("brushPreset").with("Nm  ", t(n))
    let orphan = newDescriptor("brushPreset").with("Nm  ", t("Orphan")).with("Brsh", vDesc(sampledTip("$nowhere")))
    let presets = [named("Top"), named("Pen"), orphan, named("Nib"), named("Chalk")]
    let folders = [newSeq[string](), @["Inks"], @["Inks"], @["Inks", "Fine"], @["Dry"]]
    let bytes = writeAbrV6(2, [sample("$a", 4, 4, 8), sample("$b", 4, 4, 8)], [], presets, folders, false)
    let imp = readAbr(bytes, "My Set")
    check imp.presets.mapIt((it.name, it.group, it.folder.join("/"))) ==
      @[("Top", "My Set", ""), ("Pen", "My Set", "Inks"), ("Nib", "My Set", "Inks/Fine"), ("Chalk", "My Set", "Dry")]
    let flat = readAbr(writeAbrV6(2, [], [], [named("A")], [], false), "Flat")
    check flat.presets.allIt(it.group == "Flat" and it.folder.len == 0)

  test "v1 and v2 brushes become presets":
    let brushes = @[
      LegacyBrush(name: "", spacing: 25, antiAlias: true,
                  tip: LegacyTip(kind: ltComputed, diameter: 13, hardness: 50, angle: 20, roundness: 60)),
      LegacyBrush(name: "Dune Grass", spacing: 0, antiAlias: false, tip: LegacyTip(kind: ltSampled, sample: sample("", 30, 10, 8)))]
    for version in [1'u16, 2]:
      let imp = readAbr(writeAbrV12(version, brushes, true), "Old")
      check imp.version == version
      check imp.presets.len == 2
      let c = imp.presets[0]
      check c.name == "13 px Round"
      check (c.brush.size, c.brush.hardness, c.brush.angle, c.brush.roundness, c.brush.spacing) == (13'f32, 0.5'f32, 20'f32, 0.6'f32, 0.25'f32)
      let s = imp.presets[1]
      check s.name == (if version == 2: "Dune Grass" else: "Sampled Brush 2")
      check (s.brush.size, s.brush.spacing, s.brush.aliased) == (30'f32, 0.25'f32, true)

  test "v6 without settings lists the tips":
    let bytes = writeAbrV6(1, [sample("$a", 8, 8, 8), sample("$b", 3, 9, 16)], [], [], [], true)
    check readAbr(bytes, "g").presets.mapIt(it.brush.size) == @[8'f32, 9]

  test "big tips are downsampled":
    let s = AbrSample(id: "$big", width: 3000, height: 1500, depth: 8, data: newSeqWith(3000 * 1500, 200'u8))
    let imp = readAbr(writeAbrV6(2, [s], [], [], [], true), "g")
    let tip = imp.presets[0].brush.tip
    check tip.kind == tkSampled
    check (tip.sampled.width, tip.sampled.height) == (MaxTipEdge, 1250'u32)
    check imp.presets[0].brush.size == 3000
    check imp.warnings.anyIt("downsampled" in it)

  test "tilt scale, projection, spacing and mixer fields map":
    let tip = newDescriptor("computedBrush").with("Dmtr", u("#Pxl", 30)).with("Spcn", prc(35)).with("Intr", vBool(false))
    let preset = newDescriptor("brushPreset").with("Nm  ", t("Wet Pen")).with("Brsh", vDesc(tip))
      .with("useTipDynamics", vBool(true)).with("szVr", vr(3, 0, 0)).with("tiltScale", prc(150))
      .with("brushProjection", vBool(true)).with("usePaintDynamics", vBool(true)).with("wtVr", vr(2, 30, 10))
      .with("mxVr", vr(1, 45, 0))
      .with("toolOptions", vDesc(newDescriptor("currentToolOptions").with("wetness", prc(80)).with("dryness", prc(20))
        .with("mix", prc(65)).with("flow", prc(70)).with("sampleMerged", vBool(true))))
    let plain = newDescriptor("brushPreset").with("Nm  ", t("Plain")).with("Brsh", vDesc(newDescriptor("computedBrush").with("Dmtr", u("#Pxl", 9))))
    let imp = readAbr(writeAbrV6(2, [], [], [preset, plain], [], true), "Mixer")
    let b = imp.presets[0].brush
    check not b.spacingEnabled and abs(b.spacing - 0.35) < 1e-6
    check b.shapeDynamics.brushProjection and abs(b.shapeDynamics.tiltScale - 1.5) < 1e-6 and b.shapeDynamics.size.control == ctlPenTilt
    check (b.transfer.wetness.control, b.transfer.wetness.jitter, b.transfer.wetness.minimum) == (ctlPenPressure, 0.3'f32, 0.1'f32)
    check (b.transfer.mix.control, b.transfer.mix.jitter) == (ctlFade, 0.45'f32)
    check (b.mixer.wet, b.mixer.load, b.mixer.mix, b.mixer.flow, b.mixer.sampleAllLayers) == (0.8'f32, 0.2'f32, 0.65'f32, 0.7'f32, true)
    check not imp.warnings.anyIt("Tilt Scale" in it or "Projection" in it or "Mixer" in it)
    let p = imp.presets[1].brush
    check p.spacingEnabled and not p.shapeDynamics.brushProjection and p.shapeDynamics.tiltScale == 0
    check p.mixer == defaultMixerSettings()
    check (p.transfer.wetness, p.transfer.mix) == (defaultDynamic(), defaultDynamic())

  test "garbage is an error":
    for bytes in [newSeq[byte](), cast[seq[byte]]("8BPS not a brush"), @[0'u8, 6, 0, 2]]:
      expect ValueError:
        discard readAbr(bytes, "g")

  test "a cancelled import fails with cancelled":
    let bytes = writeAbrV6(2, [sample("$a", 8, 8, 8)], [], [], [], true)
    try:
      discard readAbrWith(bytes, "g", newInterrupt(proc(): bool = true))
      check false
    except ValueError as e:
      check e.msg == "cancelled"

  test "a sampled tip paints the brush colour, never black":
    let preset = newDescriptor("brushPreset").with("Nm  ", t("Tip")).with("Brsh", vDesc(sampledTip("$tip-1")))
    let imp = readAbr(writeAbrV6(2, [sample("$tip-1", 8, 8, 8)], [], [preset], [], false), "Test")
    var b = imp.presets[0].brush
    b.pressureSize = false
    b.color = [0.8'f32, 0.1, 0.1, 1]
    var s = newSurface(pixelFormat(cmRgb, stF32, true))
    discard renderStroke(s, b, [strokePoint(30, 32.5, 1), strokePoint(34, 32.5, 1)], none(Surface), false, 1)
    var painted = 0
    for y in 0 ..< 64:
      for x in 0 ..< 64:
        let p = s.rgba(x, y)
        if p[3] > 0.5:
          inc painted
          check abs(p[0] - 0.8) < 0.05 and abs(p[1] - 0.1) < 0.05
    check painted > 0
    check painted < 64 * 64
