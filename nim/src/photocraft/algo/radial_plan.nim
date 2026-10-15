## Direct Radial Blur with immutable sampling tables shared by every output tile (port of
## `blur/radial.rs`). Pixel positions and the quality-dependent interval ceilings match the
## sampler in `blur.radial`; each bilinear tap is read once and the alpha arithmetic of every
## sample is kept, so the output is the same.

import std/[math, options]
import ../geom, ../raster
import params, image, blur, util

type RadialPlan* = object
  center: (float32, float32)
  extent: float32
  meth: RadialMethod
  tables: seq[(float32, float32)]   ## interval counts 1..maxIntervals, each with n+1 samples
  maxIntervals: int

proc prepareRadial*(bounds: Rect; amount: float32; meth: RadialMethod; quality: RadialQuality;
                    center: (float32, float32); area: Option[Rect]; ctl: Interrupt): Option[RadialPlan] =
  if not amount.isFinite or not center[0].isFinite or not center[1].isFinite or ctl.cancelled: return
  let amount = clamp(amount, 0'f32, 100'f32)
  let c = (float32(bounds.x0) + float32(bounds.width) * center[0], float32(bounds.y0) + float32(bounds.height) * center[1])
  if not c[0].isFinite or not c[1].isFinite: return
  let extent = if meth == rmSpin: degToRad(amount) else: amount / 200
  # Only prepare interval counts reachable in the requested output (corners are farthest).
  var path = Inf.float32
  if area.isSome:
    let a = area.get
    var radius = 0'f32
    for y in [a.y0, satSub(a.y1, 1)]:
      for x in [a.x0, satSub(a.x1, 1)]:
        let dx = float32(x) + 0.5'f32 - c[0]
        let dy = float32(y) + 0.5'f32 - c[1]
        let r = sqrt(dx * dx + dy * dy)
        if not r.isNaN: radius = max(radius, r)
    path = extent * radius
  let maxIntervals = radialIntervals(path, quality)
  var tables = newSeqOfCap[(float32, float32)](maxIntervals * (maxIntervals + 3) div 2)
  for n in 1 .. maxIntervals:
    if ctl.cancelled: return
    for i in 0 .. n:
      if meth == rmSpin:
        let t = (float32(i) / float32(n) - 0.5'f32) * extent
        tables.add (sin(t), cos(t))
      else:
        tables.add (0'f32, 1 - extent * float32(i) / float32(n))
  some(RadialPlan(center: c, extent: extent, meth: meth, tables: tables, maxIntervals: maxIntervals))

proc filter*(p: RadialPlan; src: Image; o: Rect; ctx: Ctx; ctl: Interrupt): Option[seq[float32]] =
  ## None on cancellation or invalid image storage. Sampling beyond `src.rect` is transparent.
  let n = src.ch
  if ctl.cancelled or n == 0 or src.rect.width * src.rect.height * n != src.data.len: return
  var res = newSeq[float32](o.width * o.height * n)
  if o.isEmpty: return some(res)
  let b = src.rect
  let w = b.width
  let alpha = ctx.alpha
  var sample = newSeq[float32](n)
  for ry in 0 ..< o.height:
    if ry mod 8 == 0 and ctl.cancelled: return
    let dy = float32(o.y0 + ry) + 0.5'f32 - p.center[1]
    for rx in 0 ..< o.width:
      let dx = float32(o.x0 + rx) + 0.5'f32 - p.center[0]
      let radius = sqrt(dx * dx + dy * dy)
      let cnt = clamp(asUsize(float64(ceil(p.extent * radius))), 1, p.maxIntervals)
      let start = (cnt - 1) * (cnt + 2) div 2
      let at = (ry * o.width + rx) * n
      for k in start ..< start + cnt + 1:
        let (s, c) = p.tables[k]
        let (x, y) =
          if p.meth == rmSpin: (p.center[0] + dx * c - dy * s, p.center[1] + dx * s + dy * c)
          else: (p.center[0] + dx * c, p.center[1] + dy * c)
        if not x.isFinite or not y.isFinite: continue
        let (fx, fy) = (x - 0.5'f32, y - 0.5'f32)
        let (x0f, y0f) = (floor(fx), floor(fy))
        let (ax, ay) = (fx - x0f, fy - y0f)
        let (x0, y0) = (asI32(float64(x0f)), asI32(float64(y0f)))
        for v in sample.mitems: v = 0
        for (tx, ty, weight) in [(x0, y0, (1 - ax) * (1 - ay)), (x0 + 1, y0, ax * (1 - ay)),
                                 (x0, y0 + 1, (1 - ax) * ay), (x0 + 1, y0 + 1, ax * ay)]:
          if weight <= 0: continue
          if tx < b.x0 or tx >= b.x1 or ty < b.y0 or ty >= b.y1: continue
          let pi = ((ty - b.y0) * w + (tx - b.x0)) * n
          if alpha:
            let a = src.data[pi + n - 1]
            for ch in 0 ..< n - 1: sample[ch] += src.data[pi + ch] * a * weight
            sample[n - 1] += a * weight
          else:
            for ch in 0 ..< n: sample[ch] += src.data[pi + ch] * weight
        # Keep the sampler's unpremultiply followed by the average's premultiply.
        let a = sample[n - 1]
        for ch in 0 ..< n:
          res[at + ch] += (if alpha and ch < n - 1: (if a > 0: sample[ch] / a else: sample[ch]) * a else: sample[ch])
      let recip = 1'f32 / float32(cnt + 1)
      for ch in 0 ..< n: res[at + ch] *= recip
      if alpha:
        let a = res[at + n - 1]
        for ch in 0 ..< n - 1: res[at + ch] = (if a > 1e-7: res[at + ch] / a else: 0)
  some(res)
