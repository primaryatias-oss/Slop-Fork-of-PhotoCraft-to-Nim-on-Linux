## Brush engine (port of `crates/paint`): Photoshop-grade brushes (computed and sampled tips, Shape
## Dynamics, Scattering, Texture, Dual Brush, Color Dynamics, Transfer, Brush Pose, Noise, Wet
## Edges, Build-up, Smoothing) plus the dab machinery shared by the retouching tools.
##
## Strokes are data (`Stroke`) so they can be recorded, replayed by automation, and tested
## deterministically: every jitter is a hash of the brush seed and the dab index (`rng`). Dabs
## composite with "build-up up to stroke opacity" semantics: within one stroke, coverage
## accumulates with flow towards each dab's opacity ceiling, and the stroke composites once at the
## stroke opacity (Photoshop's Opacity versus Flow).
##
## Module map: `brush` (the serde model), `dynamics` (points → dabs), `render` (dabs → stroke
## buffer → pixels), `presets` (built-ins), `mixer` (Mixer Brush), `replace` (Color Replacement),
## `retouch` (sequential/accumulating helpers for the retouch tools), `bg_erase` (Background
## Eraser), `outline` (cursor outlines), `symmetry` (painting symmetry), `procedural` (generated
## tips and patterns).

import paint/[rng, tile, brush, dynamics, procedural, pixels, render, retouch, mixer, replace,
              bg_erase, outline, symmetry, presets]
export rng, tile, brush, dynamics, procedural, pixels, render, retouch, mixer, replace, bg_erase,
       outline, symmetry, presets
