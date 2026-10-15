## Registers the blur family with the filter dispatcher: Gaussian/Box (with the separable
## whole-area fast path), Motion (with the FFT fast path), Radial (with its sampling plan) and
## Surface blur, the sharpening filters, and the Blur and Blur Gallery filters of `blur2` and
## `gallery`.

import std/[options, math]
import ../geom, ../raster
import filters, blur, sharpen, blur2, gallery, radial_plan

registerFastPath(proc(surface: Surface; p: FilterParams; area, bounds: Rect; selection: Option[Surface];
                      extent: Option[Rect]; tile: int; ctl: Interrupt): Option[Option[Surface]] =
  let b = boxWidths(p)
  if b.isNone: return
  some(applyBoxBlur(surface, area, extent, selection, b.get, ctl)))
registerFastPath(motionApply)

registerKernel(fltGaussianBlur, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  gaussian(src, o, ctx, p.radius))
registerKernel(fltBoxBlur, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  boxed(src, o, ctx, p.radius))
registerKernel(fltMotionBlur, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  motion(src, o, ctx, p.angle, p.distance))
registerKernel(fltRadialBlur, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  radial(src, o, ctx, p.amount, p.`method`, p.radialQuality, (p.centerX, p.centerY)))
registerKernel(fltSurfaceBlur, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  surface(src, o, ctx, p.radius, p.threshold))

registerKernel(fltUnsharpMask, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  unsharp(src, o, ctx, p.amount, p.radius, p.threshold))
registerKernel(fltSmartSharpen, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  smart(src, o, ctx, p.amount, p.radius, p.reduceNoise))
registerKernel(fltHighPass, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  highPass(src, o, ctx, p.radius))
registerKernel(fltSmartBlur, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  smartBlur(src, o, ctx, p.radius, p.threshold, p.blurQuality, p.smartBlurMode))
registerKernel(fltLensBlur, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  lensBlur(src, o, ctx, LensSpec(radius: p.radius, blades: p.blades, curvature: p.curvature, rotation: p.rotation,
    depth: p.depthSource, focal: p.focalDistance, invert: p.invertDepth, brightness: p.brightness,
    threshold: p.threshold, noise: p.noise, distribution: p.distribution, mono: p.monochromatic, seed: p.seed,
    depthMap: p.depthMap)))
registerKernel(fltShapeBlur, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  shapeBlur(src, o, ctx, p.radius, p.shape))
registerHalo(fltShapeBlur, proc(p: FilterParams; bounds: Rect): Halo =
  reachHalo(shapeRadius(p.radius)))
registerPlan(fltShapeBlur, proc(p: FilterParams; bounds, area: Rect; ctl: Interrupt): Option[TilePlan] =
  # Shape rasterization depends only on the parameters, not on a tile's pixels.
  let prepared = newShapePrepared(p.radius, p.shape)
  some(TilePlan(strict: true, run: proc(src: Image; t: Rect; ctx: Ctx; ctl: Interrupt): Option[seq[float32]] =
    prepared.checkedRun(src, t, ctx))))

proc maxBlur(p: FilterParams): float32 =
  for pin in p.irisPins:
    if not pin.blur.isNaN: result = max(result, pin.blur)
  for pin in p.fieldPins:
    if not pin.blur.isNaN: result = max(result, pin.blur)

registerKernel(fltTiltShift, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  tiltShift(src, o, ctx, p.blur, (p.centerX, p.centerY), p.angle, p.focus, p.transition))
registerHalo(fltTiltShift, proc(p: FilterParams; bounds: Rect): Halo = reachHalo(gallery.reach(p.blur)))
registerKernel(fltIrisBlur, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  iris(src, o, ctx, p.irisPins))
registerHalo(fltIrisBlur, proc(p: FilterParams; bounds: Rect): Halo = reachHalo(gallery.reach(maxBlur(p))))
registerKernel(fltFieldBlur, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  field(src, o, ctx, p.fieldPins))
registerHalo(fltFieldBlur, proc(p: FilterParams; bounds: Rect): Halo = reachHalo(gallery.reach(maxBlur(p))))
registerKernel(fltSpinBlur, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  spin(src, o, ctx, p.spinPins))
registerKernel(fltPathBlur, proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  gallery.path(src, o, ctx, p.paths))
registerPlan(fltRadialBlur, proc(p: FilterParams; bounds, area: Rect; ctl: Interrupt): Option[TilePlan] =
  let plan = prepareRadial(bounds, p.amount, p.`method`, p.radialQuality, (p.centerX, p.centerY), some(area), ctl)
  if plan.isNone: return
  let pl = plan.get
  some(TilePlan(strict: false, run: proc(src: Image; t: Rect; ctx: Ctx; ctl: Interrupt): Option[seq[float32]] =
    pl.filter(src, t, ctx, ctl))))
