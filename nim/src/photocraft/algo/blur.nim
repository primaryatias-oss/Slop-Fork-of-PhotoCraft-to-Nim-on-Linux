## Blurs: Gaussian, box, motion, radial, surface (port of `blur.rs`, `blur/motion_apply.rs`,
## `blur/motion_fft.rs` and the box-blur whole-area path of `lib.rs`).

import std/[math, options, tables, cpuinfo, sequtils, algorithm, bitops, fenv]
import ../geom, ../color, ../raster
import params, image, util, fft, filters

const
  Strip* = 64            ## width in pixels of the column strips the vertical box passes work on
  ResultBudget = 256 shl 20

type StripData* = (Rect, seq[float32])   ## samples of a column strip of an output area

# ---------------------------------------------------------------- separable kernels

proc gaussianKernel*(sigma: float32): seq[float32] =
  ## Normalized Gaussian kernel with standard deviation `sigma` (radius 3σ).
  if sigma < 0.05'f32: return @[1'f32]
  let r = asI32(float64(ceil(sigma * 3)))
  for i in -r .. r: result.add exp(-float32(i * i) / (2 * sigma * sigma))
  var s = 0'f32
  for v in result: s += v
  for v in result.mitems: v = v / s

proc row*(img: Image; r: Rect): seq[float32] =
  ## Samples of the pixels of a one-pixel-high rectangle inside the image.
  let i = img.index(r.x0, r.y0)
  img.data[i ..< i + r.width * img.ch]

proc convSep*(src: Image; o: Rect; kx, ky: openArray[float32]; alpha: bool): seq[float32] =
  ## Separable convolution of `src` (premultiplied internally) for `o`; `src` must cover `o`
  ## grown by the kernel radii.
  let n = src.ch
  let (rx, ry) = (kx.len div 2, ky.len div 2)
  let (ow, oh) = (o.width, o.height)
  let th = oh + 2 * ry
  let win = rect(o.x0 - rx, o.y0 - ry, o.x1 + rx, o.y1 + ry)
  let ww = win.width
  var p = newSeq[float32](ww * win.height * n)
  for y in win.y0 ..< win.y1:
    for x in win.x0 ..< win.x1:
      let at = ((y - win.y0) * ww + (x - win.x0)) * n
      for c in 0 ..< n: p[at + c] = src.get(x, y, c)
  premultiply(p, n, alpha)
  # Horizontal pass: (ow × th).
  var tmp = newSeq[float32](ow * th * n)
  for ty in 0 ..< th:
    let rowAt = ty * ww * n
    let dstAt = ty * ow * n
    for ox in 0 ..< ow:
      for c in 0 ..< n:
        var acc = 0'f32
        for i, kv in kx: acc += p[rowAt + (ox + i) * n + c] * kv
        tmp[dstAt + ox * n + c] = acc
  # Vertical pass.
  result = newSeq[float32](ow * oh * n)
  for oy in 0 ..< oh:
    let d = oy * ow * n
    for i, kv in ky:
      let s = (oy + i) * ow * n
      for k in 0 ..< ow * n: result[d + k] += tmp[s + k] * kv
  unpremultiply(result, n, alpha)

# ---------------------------------------------------------------- box passes

proc boxesForGauss(sigma: float32; n: int): seq[int] =
  ## Box widths (odd) whose `n`-fold convolution has standard deviation `sigma`.
  let nf = float32(n)
  let wIdeal = sqrt(12 * sigma * sigma / nf + 1)
  var wl = asI32(float64(floor(wIdeal)))
  if wl mod 2 == 0: dec wl
  wl = max(wl, 1)
  let wu = wl + 2
  let mIdeal = (12 * sigma * sigma - nf * float32(wl * wl) - 4 * nf * float32(wl) - 3 * nf) / (-4 * float32(wl) - 4)
  let m = asUsize(float64(clamp(round(mIdeal), 0'f32, nf)))
  for i in 0 ..< n: result.add(if i < m: wl else: wu)

proc boxWidths*(p: FilterParams): Option[seq[int]] =
  ## The box widths (odd) a blur runs as running-sum passes along each axis, or none when it uses
  ## a direct kernel (small radii, where the kernel is quicker).
  case p.kind
  of fltGaussianBlur:
    if p.radius > 4: some(boxesForGauss(p.radius, 3)) else: none(seq[int])
  of fltBoxBlur:
    let r = asUsize(float64(round(max(p.radius, 0))))
    if r > 4: some(@[2 * r + 1]) else: none(seq[int])
  else: none(seq[int])

proc boxPass(src: openArray[float32]; dst: var openArray[float32]; w, r: int; acc: var seq[float32]) =
  ## One running-sum box pass of radius `r` along a line of `w`-float elements: element `i` of
  ## `dst` is the mean of elements `i .. i + 2r` of `src`, which is `2r` elements longer.
  if w == 0: return
  let norm = 1'f32 / float32(2 * r + 1)
  acc.setLen(0)
  acc.setLen(w)
  let srcLen = src.len div w
  for e in 0 ..< min(2 * r + 1, srcLen):
    for c in 0 ..< w: acc[c] += src[e * w + c]
  let dstLen = dst.len div w
  for i in 0 ..< dstLen:
    for c in 0 ..< w: dst[i * w + c] = acc[c] * norm
    let (add, sub) = (i + 2 * r + 1, i)
    if add < srcLen and sub < srcLen:
      for c in 0 ..< w: acc[c] += src[add * w + c] - src[sub * w + c]

type Scratch = object
  line, next, acc: seq[float32]

proc linePasses(dst: var openArray[float32]; w: int; radii: openArray[int]; s: var Scratch) =
  ## Box passes of the given radii in turn along `s.line` (`w`-float elements, as many as `dst`
  ## holds plus twice the radii's sum; consumed), ending in `dst`.
  if w == 0: return
  let ln = dst.len div w
  var tail = 0
  for r in radii: tail += r
  for k, r in radii:
    tail -= r
    if k + 1 == radii.len:
      boxPass(s.line, dst, w, r, s.acc)
    else:
      s.next.setLen((ln + 2 * tail) * w)
      boxPass(s.line, s.next, w, r, s.acc)
      swap(s.line, s.next)

proc strips(o: Rect): seq[Rect] =
  ## Column strips of `o` at multiples of `Strip`, so each lies inside one tile column.
  var x = divEuclid(o.x0, Strip) * Strip
  while x < o.x1:
    result.add rect(max(x, o.x0), o.y0, min(x + Strip, o.x1), o.y1)
    x += Strip

type RowReader* = proc(r: Rect; buf: var seq[float32]) {.closure.}

proc boxBlur*(src: Rect; read: RowReader; o: Rect; n: int; alpha: bool; boxes: openArray[int];
              ctl: Interrupt): Option[seq[StripData]] =
  ## Box blurs of the given (odd) widths, one after another along each axis, of the `n`-channel
  ## pixels of `src` extended beyond it by repeating its edge pixels (alpha-weighted when
  ## `alpha`). `read(row, buf)` fills `buf` with the samples of a one-pixel-high rectangle inside
  ## `src`. Running sums make the cost independent of the widths. Returns the straight results for
  ## `o` as column strips (none when cancelled).
  var radii: seq[int]
  for w in boxes: radii.add w div 2
  var reachSum = 0
  for r in radii: reachSum += r
  let reach = min(reachSum, int(high(int32)))
  let (ow, oh) = (o.width, o.height)
  let used = src.intersect(o.inflate(reach))
  var res: seq[StripData]
  if used.isEmpty or n == 0:
    # Nothing to read: transparent, as `Image.get` reads outside an image.
    for t in strips(o): res.add (t, newSeq[float32](t.width * oh * n))
    return some(res)
  var h = newSeq[float32](used.height * ow * n)
  let left = used.x0 - satSub(o.x0, reach)
  let right = satAdd(o.x1, reach) - used.x1
  var rowBuf: seq[float32]
  var s: Scratch
  for k in 0 ..< used.height:
    if ctl.cancelled: return none(seq[StripData])
    let y = used.y0 + k
    read(rect(used.x0, y, used.x1, y + 1), rowBuf)
    premultiply(rowBuf, n, alpha)
    if rowBuf.len < n: return none(seq[StripData])
    s.line.setLen(0)
    for _ in 0 ..< left: s.line.add rowBuf.toOpenArray(0, n - 1)
    s.line.add rowBuf
    for _ in 0 ..< right: s.line.add rowBuf.toOpenArray(rowBuf.len - n, rowBuf.len - 1)
    linePasses(h.toOpenArray(k * ow * n, (k + 1) * ow * n - 1), n, radii, s)
  # Vertical passes, strip by strip, the same way down the columns.
  let top = used.y0 - satSub(o.y0, reach)
  let bottom = satAdd(o.y1, reach) - used.y1
  let (rows, stride) = (used.height, ow * n)
  for t in strips(o):
    if ctl.cancelled: return none(seq[StripData])
    let (sx, w) = ((t.x0 - o.x0) * n, t.width * n)
    var sc: Scratch
    for _ in 0 ..< top: sc.line.add h.toOpenArray(sx, sx + w - 1)
    for y in 0 ..< rows: sc.line.add h.toOpenArray(y * stride + sx, y * stride + sx + w - 1)
    for _ in 0 ..< bottom: sc.line.add h.toOpenArray((rows - 1) * stride + sx, (rows - 1) * stride + sx + w - 1)
    var r = newSeq[float32](oh * w)
    linePasses(r, w, radii, sc)
    unpremultiply(r, n, alpha)
    res.add (t, r)
  if ctl.cancelled: none(seq[StripData]) else: some(res)

proc boxPasses(src: Image; o: Rect; alpha: bool; boxes: openArray[int]): seq[float32] =
  ## `boxBlur` of an image (edges repeated) into one interleaved buffer for `o`.
  let n = src.ch
  let ow = o.width * n
  result = newSeq[float32](ow * o.height)
  if result.len == 0: return
  let sp = unsafeAddr src
  let read: RowReader = proc(r: Rect; buf: var seq[float32]) = buf = sp[].row(r)
  let r = boxBlur(src.rect, read, o, n, alpha, boxes, NoInterrupt)
  if r.isNone: return
  for (t, data) in r.get:
    let (x0, w) = ((t.x0 - o.x0) * n, t.width * n)
    for y in 0 ..< o.height:
      for k in 0 ..< w: result[y * ow + x0 + k] = data[y * w + k]

proc gaussian*(src: Image; o: Rect; ctx: Ctx; radius: float32): seq[float32] =
  ## Exact kernel for small radii; beyond that three box passes approximate the Gaussian with cost
  ## independent of the radius (Kovesi, "Fast almost-Gaussian filtering", 2010).
  var p = newFilterParams(fltGaussianBlur)
  p.radius = radius
  let b = boxWidths(p)
  if b.isSome: boxPasses(src, o, ctx.alpha, b.get)
  else:
    let k = gaussianKernel(radius)
    convSep(src, o, k, k, ctx.alpha)

proc boxed*(src: Image; o: Rect; ctx: Ctx; radius: float32): seq[float32] =
  var p = newFilterParams(fltBoxBlur)
  p.radius = radius
  let b = boxWidths(p)
  if b.isSome: boxPasses(src, o, ctx.alpha, b.get)
  else:
    let r = asUsize(float64(round(max(radius, 0))))
    let k = newSeqWith(2 * r + 1, 1'f32 / float32(2 * r + 1))
    convSep(src, o, k, k, ctx.alpha)

# ---------------------------------------------------------------- sampled blurs

type OffsetsProc = proc(x, y: float32; pts: var seq[(float32, float32)]) {.closure.}

proc averageSamples(src: Image; o: Rect; ctx: Ctx; offsets: OffsetsProc): seq[float32] =
  let n = src.ch
  let (ow, oh) = (o.width, o.height)
  result = newSeq[float32](ow * oh * n)
  if ow == 0 or oh == 0 or n == 0: return
  var pts: seq[(float32, float32)]
  var tmp = newSeq[float32](n)
  var acc = newSeq[float32](n)
  for y in o.y0 ..< o.y1:
    let rowAt = (y - o.y0) * ow * n
    for x in o.x0 ..< o.x1:
      let (cx, cy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
      pts.setLen(0)
      offsets(cx, cy, pts)
      for a in acc.mitems: a = 0
      for (sx, sy) in pts:
        src.sample(sx, sy, edTransparent, src.rect, ctx.alpha, tmp)
        # Accumulate premultiplied.
        let a = if ctx.alpha: tmp[n - 1] else: 1'f32
        for c in 0 ..< n: acc[c] += (if ctx.alpha and c < n - 1: tmp[c] * a else: tmp[c])
      let k = 1'f32 / float32(max(pts.len, 1))
      let at = rowAt + (x - o.x0) * n
      for c in 0 ..< n: result[at + c] = acc[c] * k
      if ctx.alpha:
        let alpha = acc[n - 1] * k
        for c in 0 ..< n - 1: result[at + c] = (if alpha > 1e-7'f32: result[at + c] / alpha else: 0)

proc motionOffsets(angle, distance: float32): seq[(float32, float32)] =
  ## Sample offsets of a motion blur of `distance` at `angle`, relative to the pixel centre.
  let d = abs(distance)
  let rad = degToRad(angle)
  let (s, c) = (sin(rad), cos(rad))
  let steps = asI32(float64(ceil(d)))
  for i in 0 .. steps:
    let t = float32(i) / float32(steps) - 0.5'f32
    result.add (c * d * t, -s * d * t)

proc motion*(src: Image; o: Rect; ctx: Ctx; angle, distance: float32): seq[float32] =
  ## Motion blur: the mean of bilinear samples along a line centred on the pixel. The bilinear
  ## taps of the (pixel-independent) offsets are merged once into a fixed kernel, applied tap by
  ## tap to whole rows of a premultiplied window.
  if abs(distance) < 0.5'f32: return src.crop(o)
  let offs = motionOffsets(angle, distance)
  let k = 1'f32 / float32(offs.len)
  # Merged taps (dx, dy, weight), in the order first seen.
  var taps: seq[(int, int, float32)]
  var slot = initTable[(int, int), int]()
  for (ox, oy) in offs:
    let (x0f, y0f) = (floor(ox), floor(oy))
    let (ax, ay) = (ox - x0f, oy - y0f)
    let (x0, y0) = (asI32(float64(x0f)), asI32(float64(y0f)))
    for (dx, dy, w) in [(0, 0, (1 - ax) * (1 - ay)), (1, 0, ax * (1 - ay)), (0, 1, (1 - ax) * ay), (1, 1, ax * ay)]:
      if w <= 0: continue
      let key = (x0 + dx, y0 + dy)
      var i = slot.getOrDefault(key, -1)
      if i < 0:
        taps.add (key[0], key[1], 0'f32)
        i = taps.len - 1
        slot[key] = i
      taps[i][2] += w * k
  var reach = 0
  for t in taps: reach = max(reach, max(abs(t[0]), abs(t[1])))
  let n = src.ch
  let win = o.inflate(reach)
  let ww = win.width
  # Premultiplied window; outside the source reads as transparent, as the sampler does.
  var pw = newSeq[float32](ww * win.height * n)
  for y in win.y0 ..< win.y1:
    for x in win.x0 ..< win.x1:
      let i = ((y - win.y0) * ww + (x - win.x0)) * n
      let a = if ctx.alpha: src.get(x, y, n - 1) else: 1'f32
      for c in 0 ..< n:
        let v = src.get(x, y, c)
        pw[i + c] = (if ctx.alpha and c < n - 1: v * a else: v)
  let (ow, oh) = (o.width, o.height)
  result = newSeq[float32](ow * oh * n)
  let r = reach
  for oy in 0 ..< oh:
    let rowAt = oy * ow * n
    for (dx, dy, w) in taps:
      let start = ((oy + r + dy) * ww + (r + dx)) * n
      for k2 in 0 ..< ow * n: result[rowAt + k2] += w * pw[start + k2]
    if ctx.alpha:
      for px in 0 ..< ow:
        let at = rowAt + px * n
        let a = result[at + n - 1]
        for c in 0 ..< n - 1: result[at + c] = (if a > 1e-7'f32: result[at + c] / a else: 0)

proc radialIntervals*(pathLength: float32; quality: RadialQuality): int =
  let mx = case quality
    of rqDraft: 64
    of rqGood: 256
    of rqBest: 4096
  if pathLength.isNaN: return 1
  asUsize(float64(clamp(ceil(pathLength), 1'f32, float32(mx))))

proc radial*(src: Image; o: Rect; ctx: Ctx; amount: float32; meth: RadialMethod; quality: RadialQuality;
             center: (float32, float32)): seq[float32] =
  let b = ctx.bounds
  let (cx, cy) = (float32(b.x0) + float32(b.width) * center[0], float32(b.y0) + float32(b.height) * center[1])
  let amount = clamp(amount, 0'f32, 100'f32)
  averageSamples(src, o, ctx, proc(x, y: float32; pts: var seq[(float32, float32)]) =
    let (dx, dy) = (x - cx, y - cy)
    let r = sqrt(dx * dx + dy * dy)
    case meth
    of rmSpin:
      # Arc of `amount` degrees centred on the pixel.
      let arc = degToRad(amount)
      let n = radialIntervals(arc * r, quality)
      for i in 0 .. n:
        let t = (float32(i) / float32(n) - 0.5'f32) * arc
        let (s, c) = (sin(t), cos(t))
        pts.add (cx + dx * c - dy * s, cy + dx * s + dy * c)
    of rmZoom:
      # Samples along the ray, up to amount/2 % closer to the centre.
      let span = amount / 200
      let n = radialIntervals(span * r, quality)
      for i in 0 .. n:
        let k = 1 - span * float32(i) / float32(n)
        pts.add (cx + dx * k, cy + dy * k))

# ---------------------------------------------------------------- surface blur

proc surface8bit(src: Image; o: Rect; ctx: Ctx; r: int; t: float32; reach: int): Option[seq[float32]] =
  ## `surface` for 8-bit samples: a 256-bin histogram of each channel over the window slides along
  ## the row, so a pixel costs O(radius + levels) instead of O(radius²).
  let n = src.ch
  let cc = if ctx.alpha: n - 1 else: n
  let win = o.inflate(r)
  let (ww, wh) = (win.width, win.height)
  const Skip = 256'u16
  var lv = newSeq[uint16](ww * wh * cc)
  for y in win.y0 ..< win.y1:
    for x in win.x0 ..< win.x1:
      let i = ((y - win.y0) * ww + (x - win.x0)) * cc
      let skip = ctx.alpha and src.get(x, y, n - 1) <= 0
      for c in 0 ..< cc:
        let v = src.get(x, y, c)
        let k = round(v * 255)
        if not (k >= 0 and k <= 255) or cast[uint32](k / 255'f32) != cast[uint32](v): return none(seq[float32])
        lv[i + c] = (if skip: Skip else: uint16(k))
  let (ow, oh) = (o.width, o.height)
  let side = 2 * r + 1
  var res = newSeq[float32](ow * oh * n)
  var hist: array[257, uint32]
  for oy in 0 ..< oh:
    for c in 0 ..< cc:
      for v in hist.mitems: v = 0
      template col(wx: int; add: bool) =
        for wy in oy ..< oy + side:
          let kk = int(lv[(wy * ww + wx) * cc + c])
          if add: inc hist[kk] else: dec hist[kk]
      for wx in 0 ..< side: col(wx, true)
      for ox in 0 ..< ow:
        let v0 = src.get(o.x0 + ox, o.y0 + oy, c)
        let k0 = asUsize(float64(round(v0 * 255)))
        var acc, wsum = 0.0
        for k in max(k0 - reach, 0) ..< min(256, k0 + reach + 1):
          let cnt = hist[k]
          if cnt == 0: continue
          let v = float32(k) / 255'f32
          let w = max(1 - abs(v - v0) / t, 0'f32)
          acc += float64(v * w) * float64(cnt)
          wsum += float64(w) * float64(cnt)
        res[(oy * ow + ox) * n + c] = (if wsum > 0: float32(acc / wsum) else: v0)
        if ox + 1 < ow:
          col(ox, false)
          col(ox + side, true)
    if ctx.alpha:
      for ox in 0 ..< ow: res[(oy * ow + ox) * n + n - 1] = src.get(o.x0 + ox, o.y0 + oy, n - 1)
  some(res)

type RankSums = object
  ## Fenwick tree over value ranks, each node holding (count, Σv, Σv²) of its range.
  nodes: seq[array[3, float64]]

proc add(t: var RankSums; rank: int; sign, v: float64) =
  var i = rank + 1
  while i < t.nodes.len:
    t.nodes[i][0] += sign
    t.nodes[i][1] += sign * v
    t.nodes[i][2] += sign * v * v
    i += i and -i

proc prefix(t: RankSums; e: int): array[3, float64] =
  var i = min(e, max(t.nodes.len - 1, 0))
  while i > 0:
    result[0] += t.nodes[i][0]
    result[1] += t.nodes[i][1]
    result[2] += t.nodes[i][2]
    i -= i and -i

proc range(t: RankSums; s, e: int): array[3, float64] =
  let (a, b) = (t.prefix(s), t.prefix(e))
  [b[0] - a[0], b[1] - a[1], b[2] - a[2]]

proc partitionPoint*[T](a: openArray[T]; pred: proc(x: T): bool {.closure.}): int =
  ## Rust `slice::partition_point` (its binary search, step for step).
  var size = a.len
  if size == 0: return 0
  var base = 0
  while size > 1:
    let half = size div 2
    let mid = base + half
    if pred(a[mid]): base = mid
    size -= half
  base + (if pred(a[base]): 1 else: 0)

proc surfaceRanked(src: Image; o: Rect; ctx: Ctx; r: int; t: float32): Option[seq[float32]] =
  ## `surface` at any bit depth, with a Fenwick tree over the window's distinct values sliding
  ## along the row (the weight is linear in `v` on each side of the centre value). None when a
  ## sample isn't finite.
  let n = src.ch
  let cc = if ctx.alpha: n - 1 else: n
  let win = o.inflate(r)
  let (ww, wh) = (win.width, win.height)
  var vals = newSeqOfCap[float32](ww * wh * cc)
  var opaque = newSeqOfCap[bool](ww * wh)
  for y in win.y0 ..< win.y1:
    for x in win.x0 ..< win.x1:
      opaque.add(not (ctx.alpha and src.get(x, y, n - 1) <= 0))
      for c in 0 ..< cc:
        let v = src.get(x, y, c)
        if not v.isFinite: return none(seq[float32])
        vals.add(v + 0'f32)   # -0.0 → 0.0, so equal values share a rank
  var levels = vals
  levels.sort(proc(a, b: float32): int = totalCmp(a, b))
  var k = 0
  for i in 0 ..< levels.len:
    if i == 0 or levels[i] != levels[k - 1]:
      levels[k] = levels[i]
      inc k
  levels.setLen(k)
  var rank = newSeq[int](vals.len)
  for i, v in vals:
    let vv = v
    rank[i] = partitionPoint(levels, proc(l: float32): bool = l < vv)
  var tree = RankSums(nodes: newSeq[array[3, float64]](levels.len + 1))
  let (ow, oh) = (o.width, o.height)
  if ow == 0 or oh == 0: return some(newSeq[float32]())
  let side = 2 * r + 1
  let t64 = float64(t)
  var res = newSeq[float32](ow * oh * n)
  for oy in 0 ..< oh:
    for c in 0 ..< cc:
      template column(wx: int; sign: float64) =
        for wy in oy ..< oy + side:
          let p = wy * ww + wx
          if p < opaque.len and opaque[p] and p * cc + c < rank.len:
            tree.add(rank[p * cc + c], sign, float64(vals[p * cc + c]))
      for wx in 0 ..< side: column(wx, 1.0)
      for ox in 0 ..< ow:
        let v0 = src.get(o.x0 + ox, o.y0 + oy, c)
        # Ranks strictly within the threshold below v0 (and v0 itself), then above it.
        let lo = partitionPoint(levels, proc(l: float32): bool = 1 - abs(l - v0) / t <= 0 and l < v0)
        let mid = partitionPoint(levels, proc(l: float32): bool = l <= v0)
        let hi = partitionPoint(levels, proc(l: float32): bool = l <= v0 or 1 - abs(l - v0) / t > 0)
        let (below, above) = (tree.range(lo, mid), tree.range(mid, hi))
        let v0d = float64(v0)
        let wsum = ((t64 - v0d) * below[0] + below[1] + (t64 + v0d) * above[0] - above[1]) / t64
        let acc = ((t64 - v0d) * below[1] + below[2] + (t64 + v0d) * above[1] - above[2]) / t64
        res[(oy * ow + ox) * n + c] = (if below[0] + above[0] > 0 and wsum > 0: float32(acc / wsum) else: v0)
        if ox + 1 < ow:
          column(ox, -1.0)
          column(ox + side, 1.0)
      # Empty the tree for the next row and channel.
      for wx in ow - 1 ..< ow - 1 + side: column(wx, -1.0)
    if ctx.alpha:
      for ox in 0 ..< ow: res[(oy * ow + ox) * n + n - 1] = src.get(o.x0 + ox, o.y0 + oy, n - 1)
  some(res)

proc surfaceDirect(src: Image; o: Rect; ctx: Ctx; r: int; t: float32): seq[float32] =
  ## `surface` by summing the whole (2r + 1)² window of every pixel.
  let n = src.ch
  let cc = if ctx.alpha: n - 1 else: n
  result = newSeqOfCap[float32](o.width * o.height * n)
  for y in o.y0 ..< o.y1:
    for x in o.x0 ..< o.x1:
      for c in 0 ..< cc:
        let v0 = src.get(x, y, c)
        var acc, wsum = 0'f32
        for yy in y - r .. y + r:
          for xx in x - r .. x + r:
            if ctx.alpha and src.get(xx, yy, n - 1) <= 0: continue
            let v = src.get(xx, yy, c)
            let w = max(1 - abs(v - v0) / t, 0'f32)
            acc += v * w
            wsum += w
        result.add(if wsum > 0: acc / wsum else: v0)
      if ctx.alpha: result.add src.get(x, y, n - 1)

const RankedMinRadius = 10  ## radius from which `surfaceRanked` beats `surfaceDirect`

proc surface*(src: Image; o: Rect; ctx: Ctx; radius, threshold: float32): seq[float32] =
  let r = asI32(float64(round(max(radius, 0))))
  let t = (max(threshold, 1) / 255) * 2.5'f32
  # Levels within the threshold of the centre (the only ones with any weight), on each side.
  let reach = min(asUsize(float64(ceil(t * 255))), 255)
  let side = 2 * r + 1
  if side * side > 2 * reach + 1 + 4 * side:
    let res = surface8bit(src, o, ctx, r, t, reach)
    if res.isSome: return res.get
  if r >= RankedMinRadius:
    let res = surfaceRanked(src, o, ctx, r, t)
    if res.isSome: return res.get
  surfaceDirect(src, o, ctx, r, t)

# ---------------------------------------------------------------- box blur over a whole area

proc applyBoxBlur*(surface: Surface; area: Rect; extent: Option[Rect]; selection: Option[Surface];
                   boxes: openArray[int]; ctl: Interrupt): Option[Surface] =
  ## The separable box passes over a whole area (Box Blur, and Gaussian Blur beyond radius 4),
  ## band by band; none when cancelled.
  var o = surface
  let fmt = surface.format
  let n = fmt.channels
  var reachSum = 0
  for w in boxes: reachSum += w div 2
  let reach = min(reachSum, int(high(int32)))
  let grown = area.inflate(reach)
  var src = grown
  if extent.isSome:
    let e = grown.intersect(extent.get)
    if not e.isEmpty: src = e
  let sp = unsafeAddr surface
  let read: RowReader = proc(r: Rect; buf: var seq[float32]) = sp[].readRegionInto(r, buf)
  # Per output column: the horizontal results for every source row, plus the final samples.
  let columnBytes = (src.height + area.height) * n * sizeof(float32)
  let band = max(ResultBudget div max(columnBytes, 1), Strip) div Strip * Strip
  var bands: seq[Rect]
  var x = area.x0
  while x < area.x1:
    bands.add rect(x, area.y0, min(x + band, area.x1), area.y1)
    x += band
  for i, b in bands:
    let st = boxBlur(src, read, b, n, fmt.alpha, boxes, ctl)
    if st.isNone: return none(Surface)
    for (t, data0) in st.get:
      var data = data0
      if selection.isSome: mixSelection(data, t, selection.get, readImage(surface, t))
      o.writeRegion(t, data)
    ctl.progress(0.98'f32 * float32(i + 1) / float32(bands.len))
  o.prune()
  ctl.progress(1.0)
  some(o)

# ---------------------------------------------------------------- motion blur by FFT

const
  MaxPoints = 4 shl 20
  MaxSide = 8192
  MaxSample = 1e10'f32
  CoordinateLimit = 16_384
  MaxResidentBytes = 512 shl 20
  MinTile = 64
  MaxTile = 512

type
  Tap = object
    x, y: int
    weight: float64
  MotionPlan = object
    distance: float32
    width, height, tileWidth, tileHeight: int
    minX, maxX, minY, maxY: int
    kernel: seq[Complex64]
    taps: seq[Tap]
    rowForward, colForward, rowInverse, colInverse: FftPlan

proc ordinaryCoordinates(r: Rect): bool =
  for v in [r.x0, r.y0, r.x1, r.y1]:
    if v < -CoordinateLimit or v > CoordinateLimit: return false
  true

proc fftSize(needed: int): int =
  ## Smallest 2^a·3^b·5^c ≥ `needed` (0 when none fits `MaxSide`).
  if needed == 0 or needed > MaxSide: return 0
  var best = MaxSide
  var a = 1
  while a <= MaxSide:
    var b = a
    while b <= MaxSide:
      var c = b
      while c <= MaxSide:
        if c >= needed: best = min(best, c)
        c *= 5
      b *= 3
    a *= 2
  best

proc motionTaps(angle, distance: float32; clip: Option[(Rect, Rect)]): Option[seq[Tap]] =
  let distance = abs(distance)
  if not angle.isFinite or not (distance >= 64 and distance <= 2000): return
  let steps = asI32(float64(ceil(distance)))
  let rad = degToRad(angle)
  let (s, c) = (sin(rad), cos(rad))
  let norm = 1'f32 / float32(steps + 1)
  var merged: seq[(int, int, float32)]
  var slots = initTable[(int, int), int]()
  for i in 0 .. steps:
    let t = float32(i) / float32(steps) - 0.5'f32
    let (x, y) = (c * distance * t, -s * distance * t)
    let (ix, iy) = (asI32(float64(floor(x))), asI32(float64(floor(y))))
    let (fx, fy) = (x - float32(ix), y - float32(iy))
    for (tx, ty, w) in [(ix, iy, (1 - fx) * (1 - fy)), (ix + 1, iy, fx * (1 - fy)), (ix, iy + 1, (1 - fx) * fy),
                        (ix + 1, iy + 1, fx * fy)]:
      if w <= 0: continue
      var sl = slots.getOrDefault((tx, ty), -1)
      if sl < 0:
        merged.add (tx, ty, 0'f32)
        sl = merged.len - 1
        slots[(tx, ty)] = sl
      merged[sl][2] += w * norm
  var res: seq[Tap]
  for (x, y, w) in merged:
    if clip.isSome:
      let (source, o) = clip.get
      if o.x0 + x >= source.x1 or o.x1 - 1 + x < source.x0 or o.y0 + y >= source.y1 or o.y1 - 1 + y < source.y0:
        continue
    res.add Tap(x: x, y: y, weight: float64(w))
  some(res)

proc sampleRowPoint(src: Image; x, y: int; alpha: bool; taps: openArray[Tap]): Option[array[5, float32]] =
  ## One pixel of the row kernel, in its f32 order.
  if src.ch < 1 or src.ch > 5 or src.rect.width * src.rect.height * src.ch != src.data.len or
     not ordinaryCoordinates(src.rect): return
  var res: array[5, float32]
  for tap in taps:
    let (sx, sy) = (x + tap.x, y + tap.y)
    let a = if alpha: src.get(sx, sy, src.ch - 1) else: 1'f32
    let weight = float32(tap.weight)
    for c in 0 ..< src.ch:
      let sample = src.get(sx, sy, c)
      let pm = if alpha and c + 1 < src.ch: sample * a else: sample
      res[c] += weight * pm
  if alpha:
    let a = res[src.ch - 1]
    for c in 0 ..< src.ch - 1: res[c] = (if a > 1e-7'f32: res[c] / a else: 0)
  some(res)

proc spanOf(taps: openArray[Tap]): (int, int, int, int) =
  var (minX, maxX, minY, maxY) = (0, 0, 0, 0)
  for t in taps:
    minX = min(minX, t.x)
    maxX = max(maxX, t.x)
    minY = min(minY, t.y)
    maxY = max(maxY, t.y)
  (minX, maxX, minY, maxY)

proc worthwhile(angle, distance: float32; tile: int; area: Rect; rowTile, channels, workers: int): Option[bool] =
  ## Conservative crossover against the merged row kernel (as tuned for the Rust build).
  if workers < 4 or channels < 1 or channels > 5 or rowTile == 0: return some(false)
  let pairs = (channels + 1) div 2
  if not distance.isFinite or abs(distance) * float32(channels) < 1500'f32 * float32(pairs): return some(false)
  let tp = motionTaps(angle, distance, none((Rect, Rect)))
  if tp.isNone: return
  let taps = tp.get
  let (minX, maxX, minY, maxY) = spanOf(taps)
  let (sx, sy) = (maxX - minX, maxY - minY)
  let width = fftSize(tile + sx)
  let height = fftSize(tile + sy)
  if width == 0 or height == 0: return
  let points = width * height
  if points > MaxPoints: return some(false)
  let (ow, oh) = (area.width, area.height)
  let tiles = ((ow + tile - 1) div tile) * ((oh + tile - 1) div tile)
  let rowTiles = ((ow + rowTile - 1) div rowTile) * ((oh + rowTile - 1) div rowTile)
  let rowWorkers = min(workers, rowTiles)
  let source = (tile + sx) * (tile + sy) * channels * 4
  let res = tile * tile * channels
  let scratch = max(width, height) * 4
  let worker = max((points * 2 + scratch) * 16 + source + res * 8, source * 2)
  # Rust's `taps.capacity()` equals its length here (exact reservation, then filtered pushes).
  let resident = points * 16 + (width + height) * 256 + taps.len * 16
  let metadata = tiles * 32
  let available = MaxResidentBytes - resident - metadata
  if available < 0: return
  let memoryWorkers = available div (worker + res * 4)
  let fftWorkers = min(min(workers, memoryWorkers), tiles)
  let parallel = fftWorkers >= rowWorkers * 2 or (fftWorkers >= 4 and fftWorkers * 2 >= rowWorkers)
  let transformWork = points * (fastLog2(width) + fastLog2(height)) * tiles
  let rowWork = ow * oh * taps.len
  some(parallel and transformWork < rowWork)

proc transpose(src: openArray[Complex64]; dst: var openArray[Complex64]; width, height: int) =
  var by = 0
  while by < height:
    var bx = 0
    while bx < width:
      for y in by ..< min(by + 32, height):
        for x in 0 ..< min(width - bx, 32): dst[(bx + x) * height + y] = src[y * width + bx + x]
      bx += 32
    by += 32

proc transform(p: MotionPlan; data, transposed: var seq[Complex64]; scratch: var seq[Complex64]; inverse: bool) =
  let (rows, cols) = if inverse: (p.rowInverse, p.colInverse) else: (p.rowForward, p.colForward)
  for i in 0 ..< p.height: rows.process(data.toOpenArray(i * p.width, (i + 1) * p.width - 1), scratch)
  transpose(data, transposed, p.width, p.height)
  for i in 0 ..< p.width: cols.process(transposed.toOpenArray(i * p.height, (i + 1) * p.height - 1), scratch)
  transpose(transposed, data, p.height, p.width)

proc newMotionPlan(angle, distance: float32; tileWidth, tileHeight: int; clip: Option[(Rect, Rect)];
                   ctl: Interrupt): Option[MotionPlan] =
  if ctl.cancelled: return
  if tileWidth <= 0 or tileHeight <= 0 or tileWidth > 512 or tileHeight > 512: return
  let tp = motionTaps(angle, distance, clip)
  if tp.isNone: return
  let (minX, maxX, minY, maxY) = spanOf(tp.get)
  let width = fftSize(tileWidth + maxX - minX)
  let height = fftSize(tileHeight + maxY - minY)
  if width == 0 or height == 0 or width * height > MaxPoints: return
  var p = MotionPlan(distance: distance, width: width, height: height, tileWidth: tileWidth, tileHeight: tileHeight,
                     minX: minX, maxX: maxX, minY: minY, maxY: maxY, taps: tp.get,
                     rowForward: newFftPlan(width), colForward: newFftPlan(height),
                     rowInverse: newFftPlan(width, true), colInverse: newFftPlan(height, true))
  var kernel = newSeq[Complex64](width * height)
  for t in p.taps:
    # Convolution reverses the displacement of a correlation tap.
    let x = remEuclid(-t.x, width)
    let y = remEuclid(-t.y, height)
    kernel[y * width + x].re += t.weight
  var transposed = newSeq[Complex64](width * height)
  var scratch: seq[Complex64]
  p.transform(kernel, transposed, scratch, false)
  p.kernel = kernel
  some(p)

proc readRect(p: MotionPlan; o: Rect): Rect = rect(o.x0 + p.minX, o.y0 + p.minY, o.x1 + p.maxX, o.y1 + p.maxY)

proc filter(p: MotionPlan; src: Image; o: Rect; alpha: bool; ctl: Interrupt): Option[seq[float32]] =
  let (ow, oh) = (o.width, o.height)
  if o.isEmpty: return some(newSeq[float32]())
  if src.ch < 1 or src.ch > 5 or src.rect.width * src.rect.height * src.ch != src.data.len or ow > p.tileWidth or
     oh > p.tileHeight or not ordinaryCoordinates(src.rect) or not ordinaryCoordinates(o): return
  let ch = src.ch
  var scales: array[5, float64]
  var i = 0
  while i + ch <= src.data.len:
    for c in 0 ..< ch:
      let v = src.data[i + c]
      if not v.isFinite or abs(v) > MaxSample: return
    if alpha and not (src.data[i + ch - 1] >= 0 and src.data[i + ch - 1] <= 1): return
    let a = if alpha: src.data[i + ch - 1] else: 1'f32
    for c in 0 ..< ch:
      let v = if alpha and c + 1 < ch: src.data[i + c] * a else: src.data[i + c]
      scales[c] = max(scales[c], float64(abs(v)))
    i += ch
  var res = newSeq[float64](ow * oh * ch)
  let read = p.readRect(o)
  let used = read.intersect(src.rect)
  var allZero = true
  for c in 0 ..< ch:
    if scales[c] != 0: allZero = false
  if used.isEmpty or allZero: return some(newSeq[float32](res.len))
  let count = p.kernel.len
  var data = newSeq[Complex64](count)
  var transposed = newSeq[Complex64](count)
  var scratch: seq[Complex64]
  let norm = 1.0 / float64(count)
  let sw = src.rect.width
  var channel = 0
  while channel < ch:
    if ctl.cancelled: return
    for v in data.mitems: v = complex64(0, 0)
    for y in used.y0 ..< used.y1:
      let sourceStart = ((y - src.rect.y0) * sw + (used.x0 - src.rect.x0)) * ch
      let start = (y - read.y0) * p.width + (used.x0 - read.x0)
      for k in 0 ..< used.width:
        let px = sourceStart + k * ch
        let a = if alpha: src.data[px + ch - 1] else: 1'f32
        template sample(c: int): float64 =
          (if c < ch and scales[c] > 0:
            float64(if alpha and c + 1 < ch: src.data[px + c] * a else: src.data[px + c]) / scales[c]
          else: 0.0)
        data[start + k] = complex64(sample(channel), sample(channel + 1))
    p.transform(data, transposed, scratch, false)
    for k in 0 ..< count: data[k] = data[k] * p.kernel[k]
    p.transform(data, transposed, scratch, true)
    let (x0, y0) = (-p.minX, -p.minY)
    for y in 0 ..< oh:
      let start = (y + y0) * p.width + x0
      for x in 0 ..< ow:
        let v = data[start + x]
        res[(y * ow + x) * ch + channel] = v.re * norm * scales[channel]
        if channel + 1 < ch: res[(y * ow + x) * ch + channel + 1] = v.im * norm * scales[channel + 1]
    channel += 2
  if alpha:
    # The row kernel accumulates in f32: recompute very small alpha near its cutoff so a
    # different summation order cannot expose colour.
    let uncertainty = float64(ceil(abs(p.distance))) * float64(epsilon(float32)) * scales[ch - 1]
    for pi in 0 ..< ow * oh:
      let at = pi * ch
      var a = res[at + ch - 1]
      if abs(a) < 1e-14 * scales[ch - 1]: a = 0
      a = clamp(a, 0.0, 1.0)
      res[at + ch - 1] = a
      let af = float32(a)
      if a > 0 and (a <= 1.01e-7 + uncertainty or (af >= 0.99e-7'f32 and af <= 1.01e-7'f32)):
        let orig = sampleRowPoint(src, o.x0 + pi mod ow, o.y0 + pi div ow, alpha, p.taps)
        if orig.isNone: return
        for c in 0 ..< ch: res[at + c] = float64(orig.get[c])
        continue
      for c in 0 ..< ch - 1: res[at + c] = (if float32(a) > 1e-7'f32: res[at + c] / a else: 0.0)
  var output = newSeq[float32](res.len)
  for k, v in res: output[k] = float32(v)
  some(output)

proc validRect(r: Rect): bool =
  not r.isEmpty and r.width <= CoordinateLimit and r.height <= CoordinateLimit and ordinaryCoordinates(r)

proc motionApply*(surface: Surface; p: FilterParams; area, bounds: Rect; selection: Option[Surface];
                  extent: Option[Rect]; tile: int; ctl: Interrupt): Option[Option[Surface]] =
  ## Wide Motion Blur as overlap-save FFT convolution over bounded tiles: none declines (the row
  ## kernel runs instead), `some(none)` propagates cancellation.
  if p.kind != fltMotionBlur: return
  if ctl.cancelled: return some(none(Surface))
  if area.isEmpty: return some(some(surface))
  let fmt = surface.format
  let channels = fmt.channels
  if channels < 1 or channels > 5 or not validRect(area) or not validRect(bounds) or
     (extent.isSome and (not validRect(extent.get) or area.intersect(extent.get) != area)) or
     (selection.isSome and selection.get.channels == 0): return
  if area.width * area.height < 4096: return
  let rowTile = tile
  if rowTile < 0: return
  let workers = max(countProcessors(), 1)
  var t = clamp(tile, MinTile, MaxTile)
  while t > MinTile and (area.width < t or area.height < t): t = max(t div 2, MinTile)
  let ww = worthwhile(p.angle, p.distance, t, area, rowTile, channels, workers)
  if ww.isNone or not ww.get: return
  let plan = newMotionPlan(p.angle, p.distance, t, t, none((Rect, Rect)), ctl)
  if plan.isNone:
    return (if ctl.cancelled: some(none(Surface)) else: none(Option[Surface]))
  if ctl.cancelled: return some(none(Surface))
  var tiles: seq[(Rect, Rect)]
  var y = area.y0
  while y < area.y1:
    let y1 = min(y + t, area.y1)
    var x = area.x0
    while x < area.x1:
      let x1 = min(x + t, area.x1)
      let output = rect(x, y, x1, y1)
      let source = plan.get.readRect(output)
      if not validRect(source): return
      tiles.add (output, source)
      x = x1
    y = y1
  var o = surface
  var done = 0
  for (output, source) in tiles:
    if ctl.cancelled: return some(none(Surface))
    let src = if extent.isSome: readClamped(surface, source, extent.get) else: readImage(surface, source)
    var data = plan.get.filter(src, output, fmt.alpha, ctl)
    if data.isNone or data.get.len != output.width * output.height * channels:
      # Unsupported source values decline the whole specialization.
      return (if ctl.cancelled: some(none(Surface)) else: none(Option[Surface]))
    if selection.isSome: mixSelection(data.get, output, selection.get, src)
    o.writeRegion(output, data.get)
    inc done
    ctl.progress(0.98'f32 * float32(done) / float32(tiles.len))
  o.prune()
  ctl.progress(1.0)
  some(some(o))

proc motionFftFilter*(src: Image; o: Rect; alpha: bool; angle, distance: float32): Option[seq[float32]] =
  ## The FFT motion blur of one window (kernel clipped to the taps that can read `src`).
  if o.isEmpty: return some(newSeq[float32]())
  let plan = newMotionPlan(angle, distance, o.width, o.height, some((src.rect, o)), NoInterrupt)
  if plan.isNone: return
  plan.get.filter(src, o, alpha, NoInterrupt)
