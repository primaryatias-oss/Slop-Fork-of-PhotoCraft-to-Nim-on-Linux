## PhotoCraft's image filters and imaging algorithms (port of `photocraft_algo`). Importing this
## module registers every ported filter with the dispatcher in `algo/filters` (`apply`,
## `outputArea`, `kernel`) and installs the resampler's Preserve Details sharpening.

import std/options
import geom, raster
import algo/[filters, basic_filters, blur_filters, fx_filters, resample]
export filters

# Preserve Details (enlargement) finishes with a light Unsharp Mask over the result, as in
# Rust's `resize_with_budget`; `resample` can't import the filter dispatcher itself.
resample.preserveDetailsSharpen = proc(s: Surface; amount, radius: float32): Surface =
  let dst = s.contentBounds
  var p = newFilterParams(fltUnsharpMask)
  p.amount = amount
  p.radius = radius
  p.threshold = 0
  let area = outputArea(p, dst, dst, none(Rect))
  apply(s, p, area, dst, none(Surface))
