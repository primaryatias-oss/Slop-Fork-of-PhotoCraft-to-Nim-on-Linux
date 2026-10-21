## Preset state defaults and the preset-panel command table (`presets/mod.rs`). Part of the
## `engine/core` unit: included, not imported.

proc defaultPresetState*(): PresetState =
  PresetState(gradients: builtinGradients(), gradient: foregroundToBackground(), styles: builtinStyles(),
              shapes: builtinShapes(), patternGroups: builtinPatternGroups(), toolPresets: builtinToolPresets(),
              clone: defaultCloneSources(), swatches: builtinSwatches())

proc presetsSpecs*(): seq[CommandSpec] =
  ## Every preset-panel command.
  result = gradientPresetSpecs()
  result.add patternPresetSpecs()
  result.add stylePresetSpecs()
  result.add shapePresetSpecs()
  result.add toolPresetSpecs()
  result.add clonePresetSpecs()
