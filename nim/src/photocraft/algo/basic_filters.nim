## Registers the kernels of the filter modules that need no planning or fast path: Add Noise,
## Median, Dust & Scratches, Custom, HSB/HSL, Color to Alpha, De-Interlace, NTSC Colors and the
## Stylize part 2 filters (Diffuse, Extrude, Tiles, Trace Contour, Wind).

import std/math
import ../geom
import filters, noise, other2, color_to_alpha, video, stylize2

registerKernel(fltAddNoise, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  addNoise(src, o, ctx, p.amount, p.distribution, p.monochromatic, p.seed))
registerKernel(fltMedian, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  median(src, o, p.radius))
registerKernel(fltDustAndScratches, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  median(src, o, p.radius, p.threshold))
registerKernel(fltCustom, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  custom(src, o, ctx, p.kernel, p.scale, p.offset))
registerKernel(fltHsbHsl, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  hsbHsl(src, o, ctx, p.input, p.output))
registerKernel(fltColorToAlpha, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  colorToAlpha(src, o, ctx, p.color, p.transparencyThreshold, p.opacityThreshold))
registerKernel(fltDeInterlace, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  deinterlace(src, o, ctx, p.eliminateEven, p.interpolate))
registerKernel(fltNtscColors, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  ntsc(src, o, ctx))
registerKernel(fltDiffuse, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  diffuse(src, o, ctx, p.diffuseMode, p.seed))
registerKernel(fltExtrude, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  extrude(src, o, ctx, ExtrudeSpec(kind: p.extrudeKind, size: p.size, depth: p.depth, levelBased: p.levelBased,
                                   solidFront: p.solidFront, maskIncomplete: p.maskIncomplete, seed: p.seed)))
registerHalo(fltExtrude, proc(p: FilterParams; bounds: Rect): Halo =
  let v = extrudeReach(p.depth) + clamp(p.size, 2, 255) * 3 + 1
  radiusHalo(int(ceil(max(v, 0))) + 1))
registerKernel(fltTiles, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  tiles(src, o, ctx, p.count, p.maxOffset, p.fill, (p.foreground, p.background), p.seed))
registerKernel(fltTraceContour, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  traceContour(src, o, ctx, p.level, p.upper))
registerKernel(fltWind, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  wind(src, o, ctx, p.windMethod, p.fromRight, p.seed))
registerHalo(fltWind, proc(p: FilterParams; bounds: Rect): Halo =
  radiusHalo(int(ceil(max(windReach(p.windMethod), 0))) + 1))
