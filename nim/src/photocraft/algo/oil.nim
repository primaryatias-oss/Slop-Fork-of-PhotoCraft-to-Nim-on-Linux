## Oil Paint: flow-guided smoothing plus a lit bristle relief (port of `oil.rs`).
##
## The structure tensor of the luminance (Brox et al.) gives a smooth stroke direction field, and a
## line integral convolution (Cabral & Leedom 1993) smears colour along it, as in flow-based
## abstraction (Kyprianidis & Döllner 2008). Convolving white noise along the same field produces
## the bristle streaks, which with the smoothed luminance form a height map shaded by a light.

import std/math
import ../geom, ../color
import params, image, fxutil, noise, util

type OilSpec* = object
  stylization*, cleanliness*, scale*, bristleDetail*: float32
  lighting*: bool
  angle*, shine*: float32

proc strokeLen(stylization: float32): float32 = 1.5'f32 + clamp(stylization, 0.1'f32, 10'f32) * 2
proc tensorSigma(scale: float32): float32 = 0.8'f32 + clamp(scale, 0.1'f32, 10'f32) * 0.6'f32

proc reach*(stylization, scale: float32): float32 =
  ## Pixels read beyond an output tile.
  strokeLen(stylization) + 3 * tensorSigma(scale) + 3 * 2.5'f32 + 4

proc oilPaint*(src: Image; o: Rect; ctx: Ctx; spec: OilSpec): seq[float32] =
  let n = src.ch
  let win = src.rect
  let (ww, wh) = (win.width, win.height)
  let alpha = ctx.alpha
  let cc = ncol(ctx, n)
  # 1. Premultiplied colour, cleaned (pre-smoothed) by `cleanliness`.
  var p = premulWindow(src, win, alpha)
  gaussBlurN(p, ww, wh, n, clamp(spec.cleanliness, 0'f32, 10'f32) * 0.25'f32)
  # 2. Luminance-like intensity for the structure tensor (channel mean works in every model).
  var lum = newSeq[float32](ww * wh)
  for i in 0 ..< ww * wh:
    var s = 0'f32
    for c in 0 ..< cc: s += p[i * n + c]
    lum[i] = s / float32(cc)
  # 3. Smoothed structure tensor (E, F, G).
  var efg = newSeq[float32](ww * wh * 3)
  for y in 1 ..< max(wh - 1, 1):
    for x in 1 ..< ww - 1:
      template l(dx, dy: int): float32 = lum[(y + dy) * ww + x + dx]
      let gx = (l(1, -1) + 2 * l(1, 0) + l(1, 1)) - (l(-1, -1) + 2 * l(-1, 0) + l(-1, 1))
      let gy = (l(-1, 1) + 2 * l(0, 1) + l(1, 1)) - (l(-1, -1) + 2 * l(0, -1) + l(1, -1))
      let at = (y * ww + x) * 3
      efg[at] = gx * gx
      efg[at + 1] = gx * gy
      efg[at + 2] = gy * gy
  gaussBlurN(efg, ww, wh, 3, tensorSigma(spec.scale))
  # 4. Stroke direction = minor eigenvector (along edges).
  let diag = float32(1 / sqrt(2.0))
  var tangent = newSeq[(float32, float32)](ww * wh)
  for i in 0 ..< ww * wh:
    let (e, f, g) = (efg[i * 3], efg[i * 3 + 1], efg[i * 3 + 2])
    let l1 = 0.5'f32 * (e + g + sqrt((e - g) * (e - g) + 4 * f * f))
    let (tx, ty) = (l1 - e, -f)
    let m = sqrt(tx * tx + ty * ty)
    # Near-isotropic structure has no stable direction: gentle diagonal strokes.
    tangent[i] = if m < 1e-6 or (l1 - (e + g - l1)) < 1e-6: (diag, diag) else: (tx / m, ty / m)
  # 5. Line integral convolution over `o` grown by 1 (for the relief gradient).
  let r1 = o.inflate(1).intersect(win)
  let (rw, rh) = (r1.width, r1.height)
  let len = strokeLen(spec.stylization)
  let h = rmax(len / 10, 1)
  let steps = asI32(float64(ceil(len / h)))
  var col = newSeq[float32](rw * rh * n)
  var bristle = newSeq[float32](rw * rh)
  template sample(x0f, y0f: float32; acc: var array[MaxC, float32]; k: float32) =
    let sx = clamp(x0f, 0'f32, float32(ww - 1))
    let sy = clamp(y0f, 0'f32, float32(wh - 1))
    let (x0, y0) = (asUsize(float64(floor(sx))), asUsize(float64(floor(sy))))
    let (x1, y1) = (min(x0 + 1, ww - 1), min(y0 + 1, wh - 1))
    let (ax, ay) = (sx - float32(x0), sy - float32(y0))
    let w00 = (1 - ax) * (1 - ay) * k
    let w10 = ax * (1 - ay) * k
    let w01 = (1 - ax) * ay * k
    let w11 = ax * ay * k
    let (i00, i10, i01, i11) = ((y0 * ww + x0) * n, (y0 * ww + x1) * n, (y1 * ww + x0) * n, (y1 * ww + x1) * n)
    for c in 0 ..< n: acc[c] += p[i00 + c] * w00 + p[i10 + c] * w10 + p[i01 + c] * w01 + p[i11 + c] * w11
  var acc: array[MaxC, float32]
  for ry in 0 ..< rh:
    for rx in 0 ..< rw:
      let (dx, dy) = (float32(r1.x0 - win.x0) + float32(rx), float32(r1.y0 - win.y0) + float32(ry))
      for c in 0 ..< n: acc[c] = 0
      var wsum = 0'f32
      var nsum = 0'f32
      sample(dx, dy, acc, 1)
      wsum += 1
      nsum += hash01(win.x0 + asI32(float64(dx)), win.y0 + asI32(float64(dy)), 71, 0)
      for dir in [1'f32, -1]:
        var (qx, qy) = (dx, dy)
        var (px, py) = (0'f32, 0'f32)
        for k in 1 .. steps:
          let ti = asUsize(float64(clamp(round(qy), 0'f32, float32(wh - 1)))) * ww +
                   asUsize(float64(clamp(round(qx), 0'f32, float32(ww - 1))))
          var (tx, ty) = tangent[ti]
          if k == 1:
            tx *= dir
            ty *= dir
          elif tx * px + ty * py < 0:
            tx = -tx
            ty = -ty
          px = tx
          py = ty
          qx += tx * h
          qy += ty * h
          let wk = 1 - float32(k) / float32(steps + 1)
          sample(qx, qy, acc, wk)
          wsum += wk
          nsum += wk * hash01(win.x0 + asI32(float64(round(qx))), win.y0 + asI32(float64(round(qy))), 71, 0)
      let at = (ry * rw + rx) * n
      for c in 0 ..< n: col[at + c] = acc[c] / wsum
      bristle[ry * rw + rx] = nsum / wsum - 0.5'f32
  # 6. Relief lighting.
  let ar = degToRad(spec.angle)
  let (ls, lc) = (sin(ar), cos(ar))
  let elev = degToRad(45'f32)
  let light = [lc * cos(elev), -ls * cos(elev), sin(elev)]
  let hv = [light[0], light[1], light[2] + 1]
  let hm = sqrt(hv[0] * hv[0] + hv[1] * hv[1] + hv[2] * hv[2])
  let half = [hv[0] / hm, hv[1] / hm, hv[2] / hm]
  let amp = clamp(spec.bristleDetail, 0'f32, 10'f32) / 10
  let shine = clamp(spec.shine, 0'f32, 10'f32) / 10
  let sub = ctx.mode == cmCmyk
  let lab = ctx.mode == cmLab
  proc height(rx, ry: int): float32 =
    let at = (ry * rw + rx) * n
    var l = 0'f32
    for c in 0 ..< cc: l += col[at + c]
    l = l / float32(cc)
    if sub: l = 1 - l
    l * 2 + bristle[ry * rw + rx] * amp * 6
  let flatSpec = pow(half[2], 40'f32)
  result = newSeqOfCap[float32](o.width * o.height * n)
  for y in o.y0 ..< o.y1:
    for x in o.x0 ..< o.x1:
      let (rx, ry) = (x - r1.x0, y - r1.y0)
      let at = (ry * rw + rx) * n
      var px: array[MaxC, float32]
      for c in 0 ..< n: px[c] = col[at + c]
      unpremulPx(px.toOpenArray(0, n - 1), alpha)
      # Painting restyles colour, not coverage: keep the layer's own alpha.
      if alpha: px[n - 1] = src.get(x, y, n - 1)
      if spec.lighting:
        let hx = height(min(rx + 1, rw - 1), ry) - height(max(rx - 1, 0), ry)
        let hy = height(rx, min(ry + 1, rh - 1)) - height(rx, max(ry - 1, 0))
        let nv = [-hx * 0.5'f32, -hy * 0.5'f32, 1'f32]
        let m = sqrt(nv[0] * nv[0] + nv[1] * nv[1] + 1)
        let ndl = (nv[0] * light[0] + nv[1] * light[1] + nv[2] * light[2]) / m
        let ndh = rmax((nv[0] * half[0] + nv[1] * half[1] + nv[2] * half[2]) / m, 0)
        let shade = 1 + (ndl - light[2]) * 1.2'f32
        let specV = shine * rmax(pow(ndh, 40'f32) - flatSpec, 0) * 0.8'f32
        let chans = if lab: 1 else: cc
        for c in 0 ..< chans: px[c] = (if sub: 1 - ((1 - px[c]) * shade + specV) else: px[c] * shade + specV)
      for c in 0 ..< n: result.add px[c]
