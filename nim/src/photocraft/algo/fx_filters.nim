## Registers the Other, Stylize, Distort, Pixelate, Render, Noise and Filter Gallery kernels.

import std/[options, math]
import ../geom
import filters, other, stylize, distort, pixelate, oil, render, relight, denoise

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
registerKernel(fltColorHalftone, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  colorHalftone(src, o, ctx, p.maxRadius, p.angles))
registerKernel(fltCrystallize, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  crystallize(src, o, ctx, p.cellSize, p.seed))
registerKernel(fltFacet, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  facet(src, o, ctx))
registerKernel(fltFragment, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  fragment(src, o, ctx))
registerKernel(fltMezzotint, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  mezzotint(src, o, ctx, p.mezzotintKind, p.seed))
registerKernel(fltPointillize, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  pointillize(src, o, ctx, p.cellSize, p.seed, p.background))
registerKernel(fltOilPaint, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  oilPaint(src, o, ctx, OilSpec(stylization: p.stylization, cleanliness: p.cleanliness, scale: p.scale,
           bristleDetail: p.bristleDetail, lighting: p.lighting, angle: p.angle, shine: p.shine)))
registerHalo(fltOilPaint, proc(p: FilterParams; bounds: Rect): Halo = reachHalo(oil.reach(p.stylization, p.scale)))
registerKernel(fltFibers, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  fibers(src, o, ctx, p.variance, p.strength, p.seed, (p.foreground, p.background)))
registerKernel(fltLensFlare, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  lensFlare(src, o, ctx, p.brightness, (p.centerX, p.centerY), p.lens))
registerKernel(fltLightingEffects, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  lighting(src, o, ctx, LightingSpec(lights: p.lights, gloss: p.gloss, metallic: p.metallic, exposure: p.exposure,
           ambience: p.ambience, texture: p.texture, height: p.height, whiteIsHigh: p.whiteIsHigh)))
registerKernel(fltRelight, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  relight(src, o, ctx, RelightParams(angle: p.angle, elevation: p.elevation, intensity: p.intensity,
          ambient: p.ambient, warmth: p.warmth, softness: p.softness)))
registerHalo(fltRelight, proc(p: FilterParams; bounds: Rect): Halo = radiusHalo(haloRadius(p.softness, bounds)))
registerKernel(fltReduceNoise, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  reduceNoise(src, o, ctx, DenoiseSpec(strength: p.strength, preserve: p.preserveDetails, color: p.reduceColorNoise,
              sharpen: p.sharpenDetails, jpeg: p.removeJpegArtifact)))
registerHalo(fltReduceNoise, proc(p: FilterParams; bounds: Rect): Halo = reachHalo(denoise.reach()))
