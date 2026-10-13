## Bounded hue/saturation distribution (port of `vectorscope.rs`). Input colours must already be
## in the chosen RGB space.

import std/math

const
  Hues* = 128
  Saturations* = 64

type Vectorscope* = object
  counts*: seq[uint32]
  samples*: uint64

proc newVectorscope*(): Vectorscope = Vectorscope(counts: newSeq[uint32](Hues * Saturations))

proc hueSaturation*(c: array[3, float32]): (float32, float32) =
  ## Hue as turns and saturation in 0..1; achromatic pixels have zero saturation.
  let (r, g, b) = (c[0], c[1], c[2])
  let high = max(max(r, g), b)
  let low = min(min(r, g), b)
  let delta = high - low
  if delta <= 1.1920929e-7'f32 or high <= 0: return (0'f32, 0'f32)
  let h = (if high == r: (g - b) / delta
           elif high == g: 2 + (b - r) / delta
           else: 4 + (r - g) / delta)
  var t = (h / 6) mod 1'f32
  if t < 0: t += 1
  (t, delta / high)

proc vectorscopeFromRgba*(pixels: openArray[array[4, float32]]): Vectorscope =
  result = newVectorscope()
  for p in pixels:
    if p[3] <= 0: continue
    var ok = true
    for v in p:
      if v.classify in {fcNan, fcInf, fcNegInf}: ok = false
    if not ok: continue
    let rgb = [clamp(p[0], 0'f32, 1'f32), clamp(p[1], 0'f32, 1'f32), clamp(p[2], 0'f32, 1'f32)]
    let (h, s) = hueSaturation(rgb)
    let hue = min(int(h * float32(Hues)), Hues - 1)
    let sat = min(int(round(s * float32(Saturations - 1))), Saturations - 1)
    let i = hue * Saturations + sat
    if i >= 0 and i < result.counts.len and result.counts[i] < high(uint32): inc result.counts[i]
    if result.samples < high(uint64): inc result.samples
