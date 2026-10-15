## Blur Gallery: Tilt-Shift, Iris, Field (spatially varying Gaussian blur), Spin (rotational blur
## inside ellipses) and Path (motion along paths) (port of `gallery.rs`).
##
## Spatially varying blur interpolates between a stack of Gaussian levels (σ = 0 and a geometric
## series up to the maximum), each computed with a radius-independent box cascade, so cost grows
## with the number of levels, not the blur size. (Rust's precomputed spin rotation tables give the
## same values as computing them per sample, which is what this port does.)

import std/math
import ../geom
import params, image, fxutil
import ../rmath

proc sigmaOf(blur: float32): float32 = clamp(blur, 0'f32, 500'f32) * 0.6'f32
proc fmax(a, b: float32): float32 = (if a.isNaN: b elif b.isNaN: a else: max(a, b))
proc fmin(a, b: float32): float32 = (if a.isNaN: b elif b.isNaN: a else: min(a, b))

proc reach*(blur: float32): float32 =
  ## Pixels read beyond an output tile for a maximum blur amount.
  3 * sigmaOf(blur) + 2

const Levels = 6

type SigmaProc = proc(x, y: float32): float32 {.closure.}

proc variableBlur(src: Image; o: Rect; ctx: Ctx; smax: float32; trimLevels: bool; sigma: SigmaProc): seq[float32] =
  ## Blurs `src` over `o` with a per-pixel σ (`sigma(x, y)`, ≤ `smax`).
  let n = src.ch
  if n == 0 or o.isEmpty: return
  if smax < 0.3'f32: return src.crop(o)
  let win = src.rect
  let (ww, wh) = (win.width, win.height)
  let (ow, oh) = (o.width, o.height)
  let rowLen = ww * n
  if rowLen <= 0: return
  let sourceLen = rowLen * wh
  let pixels = ow * oh
  let samples = pixels * n
  if trimLevels and src.data.len != sourceLen: return
  # Level σs: 0, then smax / 2^(Levels-1) … smax.
  var lv = @[0'f32]
  for k in 0 ..< Levels: lv.add smax / float32(1 shl (Levels - 1 - k))
  var sig = newSeq[float32](pixels)
  for i in 0 ..< pixels:
    let (x, y) = xy(o, i)
    sig[i] = clamp(sigma(float32(x) + 0.5'f32, float32(y) + 0.5'f32), 0'f32, smax)
  proc weight(k: int; s: float32): float32 =
    ## Hat-function weight of level k for σ s.
    let c = lv[k]
    if k > 0 and s >= lv[k - 1] and s <= c: (s - lv[k - 1]) / max(c - lv[k - 1], 1e-6'f32)
    elif k + 1 < lv.len and s >= c and s <= lv[k + 1]: (lv[k + 1] - s) / max(lv[k + 1] - c, 1e-6'f32)
    elif k + 1 == lv.len and s >= c: 1
    else: 0
  var p: seq[float32]
  if trimLevels:
    p = src.data
    premultiply(p, n, ctx.alpha)
  else:
    p = premulWindow(src, win, ctx.alpha)
  var acc = newSeq[float32](samples)
  var buf: seq[float32]
  for k, lvk in lv:
    var any = false
    for s in sig:
      if weight(k, s) > 0:
        any = true
        break
    if not any: continue
    var lw = ww
    var useBuf = false
    if k > 0:
      # Iris and Tilt-Shift opt in: each level needs its own box-cascade reach.
      let trim = trimLevels and win.intersect(o) == o
      var rch = 0
      if trim and lvk >= 0.3'f32:
        for r in gaussBoxRadii(lvk): rch += r
      rch = min(rch, int(high(int32)))
      lw = if trim: min(satAdd(o.x1, rch), win.x1) - win.x0 else: ww
      let lh = if trim: min(satAdd(o.y1, rch), win.y1) - win.y0 else: wh
      buf.setLen(0)
      if trim:
        for row in 0 ..< min(lh, wh):
          buf.add p.toOpenArray(row * rowLen, row * rowLen + lw * n - 1)
      else:
        buf.add p
      gaussBlurN(buf, lw, lh, n, lvk)
      useBuf = true
    for i, s in sig:
      let wk = weight(k, s)
      if wk <= 0: continue
      let (x, y) = xy(o, i)
      let at = ((y - win.y0) * lw + (x - win.x0)) * n
      if useBuf:
        if at >= 0 and at + n <= buf.len:
          for c in 0 ..< n: acc[i * n + c] += buf[at + c] * wk
      elif at >= 0 and at + n <= p.len:
        for c in 0 ..< n: acc[i * n + c] += p[at + c] * wk
  var i = 0
  while i + n <= acc.len:
    unpremulPx(acc.toOpenArray(i, i + n - 1), ctx.alpha)
    i += n
  acc

proc shortSide(b: Rect): float32 = max(float32(min(b.width, b.height)), 1)

proc tiltShift*(src: Image; o: Rect; ctx: Ctx; blur: float32; centre: (float32, float32);
                angle, focus, transition: float32): seq[float32] =
  ## Tilt-Shift: a sharp band through the centre at `angle`, blurring with distance beyond `focus`
  ## over `transition` (both fractions of the shorter side).
  let b = ctx.bounds
  let ss = shortSide(b)
  let (cx, cy) = (float32(b.x0) + centre[0] * float32(b.width), float32(b.y0) + centre[1] * float32(b.height))
  let rad = degToRad(angle)
  let (nx, ny) = (sin(rad), cos(rad))
  let smax = fmax(sigmaOf(blur), 0)
  let (f0, f1) = (fmax(focus, 0), fmax(focus, 0) + fmax(transition, 1e-3'f32))
  variableBlur(src, o, ctx, smax, true, proc(x, y: float32): float32 =
    let d = abs((x - cx) * nx + (y - cy) * ny) / ss
    smax * smoothstep(f0, f1, d))

proc pinDistance(x, y: float32; b: Rect; px, py, rx, ry, angle, m: float32): (float32, float32, float32) =
  ## Normalized (super)elliptical distance of a point from an iris/spin pin.
  let ss = shortSide(b)
  let (cx, cy) = (float32(b.x0) + px * float32(b.width), float32(b.y0) + py * float32(b.height))
  let angleDeg = if angle.classify notin {fcNan, fcInf, fcNegInf}: angle else: 0'f32
  let rad = degToRad(angleDeg)
  let (s, c) = (sin(rad), cos(rad))
  let (dx, dy) = (x - cx, y - cy)
  var (u, v) = (dx * c + dy * s, -dx * s + dy * c)
  u = u / (fmax(rx, 1e-3'f32) * ss)
  v = v / (fmax(ry, 1e-3'f32) * ss)
  let e = if abs(m - 2) < 1e-3'f32: sqrt(u * u + v * v) else: pow(pow(abs(u), m) + pow(abs(v), m), 1 / m)
  if e.classify in {fcNan, fcInf, fcNegInf}: (Inf.float32, 0'f32, 0'f32) else: (e, u, v)

type PreparedIris = object
  cx, cy, rx, ry, sin, cos, exponent, feather, blur: float32

proc prepareIris(p: IrisPin; b: Rect): PreparedIris =
  let ss = shortSide(b)
  let angle = if p.angle.classify notin {fcNan, fcInf, fcNegInf}: p.angle else: 0'f32
  let rad = degToRad(angle)
  PreparedIris(cx: float32(b.x0) + p.x * float32(b.width), cy: float32(b.y0) + p.y * float32(b.height),
               rx: fmax(p.radiusX, 1e-3'f32) * ss, ry: fmax(p.radiusY, 1e-3'f32) * ss, sin: sin(rad), cos: cos(rad),
               exponent: 2 + clamp(p.roundness, 0'f32, 100'f32) / 100 * 6, feather: clamp(p.feather, 0'f32, 0.999'f32),
               blur: sigmaOf(p.blur))

proc sigma(p: PreparedIris; x, y: float32): float32 =
  let (dx, dy) = (x - p.cx, y - p.cy)
  let (u, v) = ((dx * p.cos + dy * p.sin) / p.rx, (-dx * p.sin + dy * p.cos) / p.ry)
  # Outside the normalized bounding square the superellipse distance is at least one.
  var e =
    if abs(u) >= 1 or abs(v) >= 1: Inf.float32
    elif abs(p.exponent - 2) < 1e-3'f32: sqrt(u * u + v * v)
    else: pow(pow(abs(u), p.exponent) + pow(abs(v), p.exponent), 1 / p.exponent)
  if e.classify in {fcNan, fcInf, fcNegInf}: e = Inf.float32
  p.blur * smoothstep(p.feather, 1, e)

proc iris*(src: Image; o: Rect; ctx: Ctx; pins: openArray[IrisPin]): seq[float32] =
  ## Iris Blur: everything outside each pin's ellipse is blurred; the sharp core is `feather` of
  ## the ellipse. Sharpness from any pin wins.
  let b = ctx.bounds
  var smax = 0'f32
  for p in pins: smax = fmax(smax, sigmaOf(p.blur))
  if pins.len == 0 or smax < 0.3'f32: return src.crop(o)
  var prepared: seq[PreparedIris]
  for p in pins: prepared.add prepareIris(p, b)
  variableBlur(src, o, ctx, smax, true, proc(x, y: float32): float32 =
    result = F32Max
    for p in prepared: result = fmin(result, p.sigma(x, y)))

proc field*(src: Image; o: Rect; ctx: Ctx; pins: openArray[FieldPin]): seq[float32] =
  ## Field Blur: blur amounts at pins, interpolated by inverse squared distance.
  let b = ctx.bounds
  if pins.len == 0: return src.crop(o)
  var smax = 0'f32
  for p in pins: smax = fmax(smax, sigmaOf(p.blur))
  var pts: seq[(float32, float32, float32)]
  for p in pins: pts.add (float32(b.x0) + p.x * float32(b.width), float32(b.y0) + p.y * float32(b.height), sigmaOf(p.blur))
  variableBlur(src, o, ctx, smax, false, proc(x, y: float32): float32 =
    var (ws, s) = (0'f32, 0'f32)
    for (px, py, sg) in pts:
      let w = 1'f32 / ((x - px) * (x - px) + (y - py) * (y - py) + 1)
      ws += w
      s += w * sg
    s / ws)

proc spin*(src: Image; o: Rect; ctx: Ctx; pins: openArray[SpinPin]): seq[float32] =
  ## Spin Blur: content inside each ellipse is blurred along arcs about its centre (`blurAngle`
  ## degrees of rotation), feathered at the rim.
  let n = src.ch
  let b = ctx.bounds
  let ss = shortSide(b)
  result = src.crop(o)
  let clip = b.intersect(src.rect)
  if clip.isEmpty or n == 0: return
  var acc: array[MaxC, float32]
  var i = 0
  while i * n + n <= result.len:
    let at = i * n
    let (x, y) = xy(o, i)
    let (fx, fy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
    for p in pins:
      let (e, u, v) = pinDistance(fx, fy, b, p.x, p.y, p.radiusX, p.radiusY, p.angle, 2)
      if e.classify in {fcNan, fcInf, fcNegInf} or e >= 1: continue
      let theta = degToRad(clamp(p.blurAngle, 0'f32, 360'f32))
      let rpx = fmax(e * fmax(p.radiusX, p.radiusY) * ss, 0)
      let m = if rpx.classify notin {fcNan, fcInf, fcNegInf} and theta.classify notin {fcNan, fcInf, fcNegInf}:
                clamp(asUsize(float64(ceil(theta * rpx / 2))), 2, 64) else: 2
      let (cx, cy) = (float32(b.x0) + p.x * float32(b.width), float32(b.y0) + p.y * float32(b.height))
      let angleDeg = if p.angle.classify notin {fcNan, fcInf, fcNegInf}: p.angle else: 0'f32
      let ar = degToRad(angleDeg)
      let (sa, ca) = (sin(ar), cos(ar))
      for c in 0 ..< n: acc[c] = 0
      for j in 0 ..< m:
        let phi = theta * (float32(j) / float32(m - 1) - 0.5'f32)
        let (sp, cp) = (sin(phi), cos(phi))
        # Rotate in the ellipse's normalized space, then map back.
        let (ru, rv) = (u * cp - v * sp, u * sp + v * cp)
        let (lu, lv) = (ru * p.radiusX * ss, rv * p.radiusY * ss)
        let (sx, sy) = (cx + lu * ca - lv * sa, cy + lu * sa + lv * ca)
        addSamplePremul(src, sx, sy, clip, ctx.alpha, acc.toOpenArray(0, n - 1))
      for c in 0 ..< n: acc[c] /= float32(m)
      unpremulPx(acc.toOpenArray(0, n - 1), ctx.alpha)
      let k = 1 - smoothstep(0.85'f32, 1, e)
      for c in 0 ..< n: result[at + c] += (acc[c] - result[at + c]) * k
      break
    inc i

proc path*(src: Image; o: Rect; ctx: Ctx; paths: openArray[BlurPath]): seq[float32] =
  ## Path Blur: motion blur whose direction follows the nearest paths (inverse-distance weighted
  ## tangents) and whose length is the path speed, optionally tapering toward each path's end.
  let n = src.ch
  let b = ctx.bounds
  let (bw, bh) = (float32(b.width), float32(b.height))
  # Segments in document space: (ax, ay, bx, by, path index, t0, t1 along path).
  var segs: seq[(float32, float32, float32, float32, int, float32, float32)]
  for pi, p in paths:
    var pts: seq[(float32, float32)]
    for q in p.points: pts.add (float32(b.x0) + q[0] * bw, float32(b.y0) + q[1] * bh)
    var total = 0'f32
    for k in 0 ..< max(pts.len - 1, 0): total += hypot(pts[k + 1][0] - pts[k][0], pts[k + 1][1] - pts[k][1])
    total = fmax(total, 1e-3'f32)
    var run = 0'f32
    for k in 0 ..< max(pts.len - 1, 0):
      let l = hypot(pts[k + 1][0] - pts[k][0], pts[k + 1][1] - pts[k][1])
      segs.add (pts[k][0], pts[k][1], pts[k + 1][0], pts[k + 1][1], pi, run / total, (run + l) / total)
      run += l
  result = src.crop(o)
  let clip = b.intersect(src.rect)
  if segs.len == 0 or clip.isEmpty or n == 0: return
  var acc: array[MaxC, float32]
  var i = 0
  while i * n + n <= result.len:
    let at = i * n
    let (x, y) = xy(o, i)
    let (fx, fy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
    var (dx, dy, ln, ws) = (0'f32, 0'f32, 0'f32, 0'f32)
    for (ax, ay, bx, by, pi, t0, t1) in segs:
      let (sx, sy) = (bx - ax, by - ay)
      let l2 = fmax(sx * sx + sy * sy, 1e-6'f32)
      let t = clamp(((fx - ax) * sx + (fy - ay) * sy) / l2, 0'f32, 1'f32)
      let (ex, ey) = (fx - ax - sx * t, fy - ay - sy * t)
      let d2 = ex * ex + ey * ey
      let w = 1'f32 / (d2 + 1)
      let l = sqrt(l2)
      let along = t0 + (t1 - t0) * t
      dx += w * sx / l
      dy += w * sy / l
      ln += w * abs(paths[pi].speed) * (1 - clamp(paths[pi].taper, 0'f32, 100'f32) / 100 * along)
      ws += w
    let m = sqrt(dx * dx + dy * dy)
    let len = ln / ws
    if m < 1e-6'f32 or len < 0.5'f32:
      inc i
      continue
    let (ux, uy) = (dx / m, dy / m)
    # Samples ~1.5 px apart (bilinear fills the gaps), capped for very long blurs.
    let steps = clamp(asUsize(float64(ceil(len / 1.5'f32))) + 1, 2, 64)
    for c in 0 ..< n: acc[c] = 0
    for j in 0 ..< steps:
      let off = len * (float32(j) / float32(steps - 1) - 0.5'f32)
      addSamplePremul(src, fx + ux * off, fy + uy * off, clip, ctx.alpha, acc.toOpenArray(0, n - 1))
    for c in 0 ..< n: acc[c] /= float32(steps)
    unpremulPx(acc.toOpenArray(0, n - 1), ctx.alpha)
    for c in 0 ..< n: result[at + c] = acc[c]
    inc i
