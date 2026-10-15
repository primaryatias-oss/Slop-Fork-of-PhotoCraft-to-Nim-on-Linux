## Bounded RGB histograms of straight, normalized float RGBA samples (port of `histogram.rs`).
##
## No colour conversion is performed: bins describe the supplied sample domain, not an assumed
## sRGB/ICC display space. 256 bins are a display resolution, not an input bit depth.

import std/math
import ../rmath

const Bins* = 256

type
  RgbHistogram* = object
    channels*: array[3, array[Bins, uint64]]
    samples*, transparent*, invalid*: uint64
    underflow*, overflow*: array[3, uint64]
      ## Finite samples outside 0..=1 are included in the edge bins and counted separately.
    shadows*, highlights*: array[3, uint64]
      ## Exact endpoints, before display-bin rounding. A near-white value is not clipped.

proc satInc(v: var uint64) {.inline.} =
  if v < high(uint64): inc v

proc finite(v: float32): bool {.inline.} = v.classify notin {fcNan, fcInf, fcNegInf}

proc clipping*(p: array[4, float32]): (uint8, uint8) =
  ## Clipped channels of one visible straight RGBA pixel as bit masks (bit 0=R, 1=G, 2=B):
  ## `(shadows, highlights)` at exact endpoints ≤0 / ≥1, not rounded edge bins. Transparent and
  ## non-finite pixels clip nothing.
  if p[3] <= 0 or not (p[0].finite and p[1].finite and p[2].finite and p[3].finite): return (0'u8, 0'u8)
  var shadows, highlights: uint8
  for c in 0 .. 2:
    if p[c] <= 0: shadows = shadows or uint8(1 shl c)
    if p[c] >= 1: highlights = highlights or uint8(1 shl c)
  (shadows, highlights)

proc fromRgba*(pixels: openArray[array[4, float32]]): RgbHistogram =
  ## Each visible pixel counts once, including partially transparent pixels. Non-finite pixels
  ## are rejected as a whole; transparent RGB cannot contribute phantom peaks.
  for pixel in pixels:
    if not pixel[3].finite:
      result.invalid.satInc
      continue
    if pixel[3] <= 0:
      result.transparent.satInc
      continue
    if not (pixel[0].finite and pixel[1].finite and pixel[2].finite):
      result.invalid.satInc
      continue
    result.samples.satInc
    let (low, high) = clipping(pixel)
    for c in 0 .. 2:
      let value = pixel[c]
      if (low and uint8(1 shl c)) != 0: result.shadows[c].satInc
      if (high and uint8(1 shl c)) != 0: result.highlights[c].satInc
      if value < 0: result.underflow[c].satInc
      elif value > 1: result.overflow[c].satInc
      # Match the preview encoder's nearest-bin rule, before it loses float precision.
      let bin = asIdx(clamp(value, 0'f32, 1'f32) * float32(Bins - 1) + 0.5'f32)
      if bin < Bins: result.channels[c][bin].satInc
