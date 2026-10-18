## Built-in brush presets (port of `paint/src/presets.rs`; our own designs, sampled tips and
## textures are generated procedurally).
##
## Grouped like Photoshop's default Brushes panel folders: General, Dry Media, Wet Media and
## Special Effects. Every preset is plain `BrushSettings` data, so it serialises, journals and can
## be edited like any user preset.

import std/[options, strutils]
import ./brush, ./procedural

const Groups* = ["General", "Dry Media", "Wet Media", "Special Effects"] ## preset groups, in panel order

proc preset(group, name: string; brush: BrushSettings): BrushPreset =
  BrushPreset(name: name, brush: brush, builtin: true, group: group)

proc pressure(): Dynamic = controlled(ctlPenPressure)
proc pressureMin(minimum: float32): Dynamic =
  result = pressure()
  result.minimum = minimum
proc sizeDyn(size: Dynamic): ShapeDynamics =
  result = defaultShapeDynamics()
  result.enabled = true
  result.size = size
proc transfer(opacity, flow: Dynamic): Transfer =
  result = defaultTransfer()
  result.enabled = true
  result.opacity = opacity
  result.flow = flow
proc paper(seed: uint32; depth: float32): Texture =
  result = defaultTexture()
  result.enabled = true
  result.pattern = procedural(psPaper, 128, seed)
  result.depth = depth
proc withPatternSize(t: Texture; n: uint32): Texture =
  result = t
  if result.pattern.kind == pkProcedural: result.pattern.size = n
proc dynamicWith(jitter = 0'f32; minimum = 0'f32; control = ctlOff; fadeSteps = 25'u32): Dynamic =
  Dynamic(jitter: jitter, control: control, fadeSteps: fadeSteps, minimum: minimum)
proc sampled(t: GrayTile): TipShape = TipShape(kind: tkSampled, sampled: t)

template edit(base: BrushSettings; body: untyped): BrushSettings =
  block:
    var it {.inject.} = base
    body
    it

proc general(base: BrushSettings): seq[BrushPreset] =
  template g(n: string; b: BrushSettings) = result.add preset("General", n, b)
  g("Hard Round", base.edit((it.size = 30; it.hardness = 1)))
  g("Soft Round", base.edit((it.size = 45; it.hardness = 0)))
  g("Hard Round Fine", base.edit((it.size = 5; it.hardness = 1; it.spacing = 0.1)))
  g("Soft Round Large", base.edit((it.size = 300; it.hardness = 0; it.spacing = 0.15)))
  g("Hard Round Pressure Size", base.edit((it.size = 30; it.hardness = 1; it.shapeDynamics = sizeDyn(pressure()))))
  g("Soft Round Pressure Size", base.edit((it.size = 45; it.hardness = 0; it.shapeDynamics = sizeDyn(pressure()))))
  g("Hard Round Pressure Opacity", base.edit((it.size = 30; it.hardness = 1; it.transfer = transfer(pressure(), defaultDynamic()))))
  g("Soft Round Pressure Opacity", base.edit((it.size = 45; it.hardness = 0; it.transfer = transfer(pressure(), defaultDynamic()))))
  g("Hard Round Pressure Opacity and Flow", base.edit((it.size = 30; it.hardness = 0.9; it.transfer = transfer(pressure(), pressureMin(0.2)))))
  g("Airbrush Soft", base.edit((it.size = 80; it.hardness = 0; it.flow = 0.1; it.spacing = 0.1; it.buildUp = true; it.buildUpRate = 25)))
  g("Airbrush Pressure Flow", base.edit((it.size = 120; it.hardness = 0; it.flow = 0.15; it.spacing = 0.1; it.buildUp = true;
                                          it.buildUpRate = 30; it.transfer = transfer(defaultDynamic(), pressure()))))
  g("Calligraphy Flat", base.edit((it.size = 28; it.hardness = 1; it.roundness = 0.2; it.angle = 45; it.spacing = 0.05)))

proc dryMedia(base: BrushSettings): seq[BrushPreset] =
  template g(n: string; b: BrushSettings) = result.add preset("Dry Media", n, b)
  g("Hard Pencil", base.edit((it.size = 3; it.hardness = 1; it.aliased = true; it.spacing = 0.1)))
  g("Soft Pencil", base.edit((it.size = 6; it.hardness = 0.6; it.spacing = 0.08; it.shapeDynamics = sizeDyn(pressureMin(0.4));
                              it.transfer = transfer(pressureMin(0.15), defaultDynamic());
                              it.texture = paper(21, 0.8); it.texture.scale = 0.5)))
  g("Chalk", base.edit((it.size = 40; it.tip = sampled(chalkTip(64, 7)); it.spacing = 0.2;
                        it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                        it.shapeDynamics.angle = jitterDyn(1); it.shapeDynamics.size = dynamicWith(jitter = 0.15);
                        it.texture = paper(3, 0.6))))
  g("Charcoal", base.edit((it.size = 45; it.tip = sampled(charcoalTip(64, 4)); it.spacing = 0.12;
                           it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                           it.shapeDynamics.size = pressureMin(0.5); it.shapeDynamics.angle = controlled(ctlDirection);
                           it.transfer = transfer(pressureMin(0.3), defaultDynamic());
                           it.texture = paper(5, 0.7); it.texture.mode = mmSubtract)))
  g("Pastel", base.edit((it.size = 50; it.tip = sampled(chalkTip(64, 31)); it.spacing = 0.15;
                         it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                         it.shapeDynamics.angle = jitterDyn(1); it.shapeDynamics.roundness = dynamicWith(jitter = 0.3, minimum = 0.5);
                         it.scattering = defaultScattering(); it.scattering.enabled = true; it.scattering.scatter = jitterDyn(0.25);
                         it.texture = defaultTexture(); it.texture.enabled = true; it.texture.pattern = procedural(psCanvas, 64, 2);
                         it.texture.depth = 0.55; it.texture.mode = mmSubtract)))
  g("Wax Crayon", base.edit((it.size = 22; it.hardness = 0.85; it.roundness = 0.7; it.spacing = 0.08;
                             it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                             it.shapeDynamics.angle = controlled(ctlDirection);
                             it.texture = paper(17, 0.9); it.texture.eachTip = true; it.texture.mode = mmHardMix;
                             it.texture.depthJitter = jitterDyn(0.3))))
  g("Dry Bristle", base.edit((it.size = 50; it.tip = sampled(bristleTip(64, 5)); it.spacing = 0.04;
                              it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                              it.shapeDynamics.angle = controlled(ctlDirection);
                              it.transfer = defaultTransfer(); it.transfer.enabled = true;
                              it.transfer.flow = dynamicWith(control = ctlFade, fadeSteps = 400, minimum = 0.1))))
  g("Rake", base.edit((it.size = 60; it.tip = sampled(rakeTip(64, 3, 7)); it.spacing = 0.03;
                       it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                       it.shapeDynamics.angle = controlled(ctlDirection);
                       it.transfer = transfer(pressureMin(0.25), defaultDynamic()))))

proc wetMedia(base: BrushSettings): seq[BrushPreset] =
  template g(n: string; b: BrushSettings) = result.add preset("Wet Media", n, b)
  g("Watercolor Wet Edges", base.edit((it.size = 60; it.hardness = 0.3; it.wetEdges = true;
                                       it.texture = paper(9, 0.35); it.texture.scale = 1.5;
                                       it.texture = it.texture.withPatternSize(256))))
  g("Watercolor Wash", base.edit((it.size = 140; it.hardness = 0; it.flow = 0.35; it.spacing = 0.12; it.wetEdges = true;
                                  it.shapeDynamics = sizeDyn(pressureMin(0.35));
                                  it.transfer = transfer(defaultDynamic(), dynamicWith(jitter = 0.2, control = ctlPenPressure));
                                  it.texture = paper(13, 0.25); it.texture.scale = 2;
                                  it.texture = it.texture.withPatternSize(256))))
  g("Ink Pen", base.edit((it.size = 8; it.hardness = 1; it.spacing = 0.05; it.shapeDynamics = sizeDyn(pressureMin(0.15));
                          it.smoothing = defaultSmoothing(); it.smoothing.amount = 0.35)))
  g("Oil Bristle", base.edit((it.size = 70; it.tip = sampled(bristleTip(64, 41)); it.spacing = 0.03;
                              it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                              it.shapeDynamics.angle = controlled(ctlDirection); it.shapeDynamics.size = pressureMin(0.6);
                              it.colorDynamics = defaultColorDynamics(); it.colorDynamics.enabled = true;
                              it.colorDynamics.perTip = false; it.colorDynamics.fgBg = jitterDyn(0.15);
                              it.colorDynamics.brightnessJitter = 0.05;
                              it.texture = defaultTexture(); it.texture.enabled = true;
                              it.texture.pattern = procedural(psCanvas, 64, 4); it.texture.depth = 0.3)))
  g("Gouache Flat", base.edit((it.size = 40; it.hardness = 0.9; it.roundness = 0.35; it.spacing = 0.05;
                               it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                               it.shapeDynamics.angle = controlled(ctlDirection);
                               it.transfer = transfer(defaultDynamic(), dynamicWith(control = ctlFade, fadeSteps = 600, minimum = 0.35)))))
  g("Ink Wash Sponge", base.edit((it.size = 70; it.tip = sampled(spongeTip(96, 8)); it.spacing = 0.3; it.flow = 0.6;
                                  it.wetEdges = true; it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                                  it.shapeDynamics.angle = jitterDyn(1); it.shapeDynamics.size = jitterDyn(0.2))))

proc specialEffects(base: BrushSettings): seq[BrushPreset] =
  template g(n: string; b: BrushSettings) = result.add preset("Special Effects", n, b)
  g("Spatter", base.edit((it.size = 60; it.tip = sampled(spatterTip(64, 11, 14)); it.spacing = 0.6;
                          it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                          it.shapeDynamics.size = dynamicWith(jitter = 0.6, minimum = 0.2); it.shapeDynamics.angle = jitterDyn(1);
                          it.shapeDynamics.flipXJitter = true; it.shapeDynamics.flipYJitter = true;
                          it.scattering = Scattering(enabled: true, scatter: jitterDyn(1.5), bothAxes: true, count: 2,
                                                     countJitter: jitterDyn(0.5)))))
  g("Stipple", base.edit((it.size = 40; it.tip = sampled(spatterTip(64, 77, 40)); it.spacing = 0.5;
                          it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                          it.shapeDynamics.angle = jitterDyn(1); it.shapeDynamics.flipXJitter = true;
                          it.scattering = defaultScattering(); it.scattering.enabled = true;
                          it.scattering.scatter = jitterDyn(0.6); it.scattering.bothAxes = true;
                          it.transfer = transfer(pressure(), defaultDynamic()))))
  g("Confetti", base.edit((it.size = 24; it.hardness = 1; it.roundness = 0.5; it.spacing = 1.2;
                           it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                           it.shapeDynamics.angle = jitterDyn(1); it.shapeDynamics.size = jitterDyn(0.5);
                           it.scattering = defaultScattering(); it.scattering.enabled = true;
                           it.scattering.scatter = jitterDyn(3); it.scattering.bothAxes = true; it.scattering.count = 3;
                           it.colorDynamics = defaultColorDynamics(); it.colorDynamics.enabled = true;
                           it.colorDynamics.perTip = true; it.colorDynamics.hueJitter = 1;
                           it.colorDynamics.saturationJitter = 0.2; it.colorDynamics.purity = 0.5)))
  g("Foliage", base.edit((it.size = 50; it.tip = sampled(leafTip(64)); it.spacing = 0.35;
                          it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                          it.shapeDynamics.size = dynamicWith(jitter = 0.5, minimum = 0.3); it.shapeDynamics.angle = jitterDyn(1);
                          it.shapeDynamics.roundness = dynamicWith(jitter = 0.4, minimum = 0.4); it.shapeDynamics.flipYJitter = true;
                          it.scattering = Scattering(enabled: true, scatter: jitterDyn(1.2), bothAxes: true, count: 2,
                                                     countJitter: jitterDyn(0.5));
                          it.colorDynamics = defaultColorDynamics(); it.colorDynamics.enabled = true;
                          it.colorDynamics.perTip = true; it.colorDynamics.fgBg = jitterDyn(0.6);
                          it.colorDynamics.hueJitter = 0.04; it.colorDynamics.brightnessJitter = 0.12)))
  g("Grass", base.edit((it.size = 70; it.tip = sampled(grassTip(96, 6, 11)); it.spacing = 0.25;
                        it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                        it.shapeDynamics.size = dynamicWith(jitter = 0.4, minimum = 0.4, control = ctlPenPressure);
                        it.shapeDynamics.angle = jitterDyn(0.06); it.shapeDynamics.flipXJitter = true;
                        it.scattering = defaultScattering(); it.scattering.enabled = true; it.scattering.scatter = jitterDyn(0.3);
                        it.colorDynamics = defaultColorDynamics(); it.colorDynamics.enabled = true;
                        it.colorDynamics.perTip = true; it.colorDynamics.fgBg = jitterDyn(0.7); it.colorDynamics.hueJitter = 0.03)))
  g("Sparkle", base.edit((it.size = 40; it.tip = sampled(starTip(64)); it.spacing = 1.5;
                          it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                          it.shapeDynamics.size = dynamicWith(jitter = 0.8, minimum = 0.15);
                          it.scattering = defaultScattering(); it.scattering.enabled = true;
                          it.scattering.scatter = jitterDyn(2.5); it.scattering.bothAxes = true;
                          it.transfer = transfer(jitterDyn(0.5), defaultDynamic()))))
  g("Sponge Texture", base.edit((it.size = 60; it.tip = sampled(spongeTip(64, 19)); it.spacing = 0.45;
                                 it.shapeDynamics = defaultShapeDynamics(); it.shapeDynamics.enabled = true;
                                 it.shapeDynamics.angle = jitterDyn(1);
                                 it.transfer = transfer(jitterDyn(0.3), defaultDynamic()))))
  g("Canvas Texture", base.edit((it.size = 50; it.hardness = 0.8; it.texture = defaultTexture(); it.texture.enabled = true;
                                 it.texture.pattern = procedural(psCanvas, 64, 1); it.texture.mode = mmSubtract;
                                 it.texture.depth = 0.7)))
  g("Halftone Dots", base.edit((it.size = 80; it.hardness = 0.5; it.texture = defaultTexture(); it.texture.enabled = true;
                                it.texture.pattern = procedural(psDots, 64, 1); it.texture.mode = mmHardMix;
                                it.texture.depth = 1)))
  g("Dual Grain", base.edit((it.size = 50; it.hardness = 0.6; it.noise = true;
                             it.dualBrush = defaultDualBrush(); it.dualBrush.enabled = true;
                             it.dualBrush.tip = sampled(spatterTip(48, 23, 20)); it.dualBrush.size = 30;
                             it.dualBrush.spacing = 0.3; it.dualBrush.scatter = 1; it.dualBrush.bothAxes = true;
                             it.dualBrush.count = 2; it.dualBrush.flip = true)))

proc builtin*(): seq[BrushPreset] =
  ## The built-in preset set, in display order (grouped by `Groups`).
  var base = defaultBrush()
  base.pressureSize = false
  base.spacing = 0.25
  result = general(base)
  result.add dryMedia(base)
  result.add wetMedia(base)
  result.add specialEffects(base)

proc find*(presets: openArray[BrushPreset]; name: string): Option[BrushPreset] =
  ## Find a preset by name (case-insensitive).
  for p in presets:
    if cmpIgnoreCase(p.name, name) == 0: return some(p)
