## Registers the Other, Stylize, Distort, Pixelate, Render, Noise and Filter Gallery kernels.

import std/[options, math]
import ../geom
import filters, other, stylize, distort

registerKernel(fltMinimum, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  minMaxPreserve(src, o, p.radius, false, p.preserve))
registerKernel(fltMaximum, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  minMaxPreserve(src, o, p.radius, true, p.preserve))
registerKernel(fltOffset, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  offset(src, o, ctx, int(p.offsetH), int(p.offsetV), p.undefined))
registerKernel(fltMosaic, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  mosaic(src, o, ctx, p.cellSize))
registerKernel(fltEmboss, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  emboss(src, o, ctx, p.angle, p.height, p.amount))
registerKernel(fltFindEdges, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  findEdges(src, o, ctx))
registerKernel(fltSolarize, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  perPixel(src, o, ctx, (if it > 0.5'f32: 1 - it else: it)))
registerKernel(fltInvert, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  perPixel(src, o, ctx, 1 - it))
registerKernel(fltDesaturate, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  desaturate(src, o, ctx))
registerKernel(fltTwirl, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  twirl(src, o, ctx, p.angle))
registerKernel(fltPinch, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  pinch(src, o, ctx, p.amount))
registerKernel(fltSpherize, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  spherize(src, o, ctx, p.amount, p.spherizeMode))
registerKernel(fltWave, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  wave(src, o, ctx, WaveSpec(generators: p.generators, wavelength: (p.wavelengthMin, p.wavelengthMax),
       amplitude: (p.amplitudeMin, p.amplitudeMax), waveType: p.waveType, undefined: p.undefined, seed: p.seed)))
registerKernel(fltRipple, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  ripple(src, o, ctx, p.amount, p.rippleSize))
registerKernel(fltPolarCoordinates, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  polar(src, o, ctx, p.polarMode))
registerKernel(fltDisplace, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  displace(src, o, ctx, (p.horizontal, p.vertical), p.stretch, p.undefined, p.map))
registerKernel(fltShear, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  shear(src, o, ctx, p.points, p.undefined))
registerKernel(fltZigZag, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  zigzag(src, o, ctx, p.amount, p.ridges, p.zigzagStyle))
