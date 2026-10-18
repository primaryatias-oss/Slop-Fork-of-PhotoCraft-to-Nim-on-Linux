## SLIC superpixels (port of `segment/slic.rs`): R. Achanta et al., "SLIC Superpixels Compared to
## State-of-the-Art Superpixel Methods", IEEE PAMI 34(11), 2012: k-means in CIELAB + xy restricted
## to a 2S×2S window around each centre, centres seeded on a grid and moved to the lowest gradient
## in their 3×3 neighbourhood, then connectivity enforcement.

import std/math
import ../rmath, ../geom
import ./segbase

type
  Superpixels* = object
    ## A superpixel labelling.
    w*, h*: int
    labels*: seq[uint32] ## label per pixel, `0 ..< count`
    count*: int
    step*: float32       ## grid interval S used

proc rgbToLab*(c: array[3, float32]): array[3, float32] =
  ## sRGB (`0..=1`) to CIELAB (D65), L in `0..=100`.
  proc lin(v: float32): float32 = (if v <= 0.04045'f32: v / 12.92'f32 else: pow((v + 0.055'f32) / 1.055'f32, 2.4'f32))
  let (r, g, b) = (lin(c[0]), lin(c[1]), lin(c[2]))
  let x = (0.412_456_4'f32 * r + 0.357_576_1'f32 * g + 0.180_437_5'f32 * b) / 0.950_47'f32
  let y = 0.212_672_9'f32 * r + 0.715_152_2'f32 * g + 0.072_175'f32 * b
  let z = (0.019_333_9'f32 * r + 0.119_192'f32 * g + 0.950_304_1'f32 * b) / 1.088_83'f32
  proc f(t: float32): float32 = (if t > 0.008_856'f32: rmath.cbrt(t) else: 7.787'f32 * t + 16'f32 / 116'f32)
  let (fx, fy, fz) = (f(x), f(y), f(z))
  [116'f32 * fy - 16'f32, 500'f32 * (fx - fy), 200'f32 * (fy - fz)]

proc enforceConnectivity(labels: seq[uint32]; w, h, minSize: int): (seq[uint32], int) =
  ## Relabels 4-connected components; components smaller than `minSize` merge into the adjacent
  ## component found before them.
  let n = w * h
  var outp = newSeq[uint32](n)
  for v in outp.mitems: v = high(uint32)
  var next = 0'u32
  var comp: seq[int]
  for s in 0 ..< n:
    if outp[s] != high(uint32): continue
    let (sx, sy) = (s mod w, s div w)
    var adj = -1'i64
    if sx > 0 and outp[s - 1] != high(uint32): adj = int64(outp[s - 1])
    if sy > 0 and outp[s - w] != high(uint32): adj = int64(outp[s - w])
    comp.setLen(0)
    comp.add s
    outp[s] = next
    var k = 0
    while k < comp.len:
      let i = comp[k]
      inc k
      let (x, y) = (i mod w, i div w)
      for (ok, j) in [(x > 0, i - 1), (x + 1 < w, i + 1), (y > 0, i - w), (y + 1 < h, i + w)]:
        if ok and outp[j] == high(uint32) and labels[j] == labels[s]:
          outp[j] = next
          comp.add j
    if adj >= 0 and comp.len < minSize:
      for i in comp: outp[i] = uint32(adj)
    else: inc next
  (outp, int(next))

proc slicLab*(lab: seq[array[3, float32]]; w, h, target: int; compactness: float32; iters: int): Superpixels =
  ## `slic` on a CIELAB image.
  let n = w * h
  if n <= 0 or lab.len < n: return Superpixels(w: w, h: h, step: 1)
  let s = rmax(sqrt(float32(n) / float32(max(target, 1))), 1'f32)
  proc at(x, y: int): array[3, float32] = lab[min(y, h - 1) * w + min(x, w - 1)]
  proc grad(x, y: int): float32 =
    d2(at(max(x - 1, 0), y), at(x + 1, y)) + d2(at(x, max(y - 1, 0)), at(x, y + 1))
  # Seed centres: [L, a, b, x, y].
  var centers: seq[array[5, float32]]
  var y = s / 2'f32
  while y < float32(h):
    var x = s / 2'f32
    while x < float32(w):
      let (cx, cy) = (asIdx(x), asIdx(y))
      var (bx, by, bg) = (cx, cy, F32Max)
      for yy in max(cy - 1, 0) .. min(cy + 1, h - 1):
        for xx in max(cx - 1, 0) .. min(cx + 1, w - 1):
          let g = grad(xx, yy)
          if g < bg: (bx, by, bg) = (xx, yy, g)
      let c = lab[by * w + bx]
      centers.add [c[0], c[1], c[2], float32(bx), float32(by)]
      x += s
    y += s
  var labels = newSeq[uint32](n)
  for v in labels.mitems: v = high(uint32)
  var dist = newSeq[float32](n)
  let q = compactness / s
  let wxy = q * q
  let r = int(ceil(s))
  for _ in 0 ..< max(iters, 1):
    for v in dist.mitems: v = F32Max
    for k, c in centers:
      let (cx, cy) = (asI32(float64(round(c[3]))), asI32(float64(round(c[4]))))
      for yy in max(cy - r, 0) ..< min(cy + r + 1, h):
        for xx in max(cx - r, 0) ..< min(cx + r + 1, w):
          let i = yy * w + xx
          let dc = d2(lab[i], [c[0], c[1], c[2]])
          let (dx, dy) = (float32(xx) - c[3], float32(yy) - c[4])
          let d = dc + (dx * dx + dy * dy) * wxy
          if d < dist[i]:
            dist[i] = d
            labels[i] = uint32(k)
    var sums = newSeq[array[6, float64]](centers.len)
    for i, l in labels:
      if l == high(uint32): continue
      let p = lab[i]
      sums[l][0] += float64(p[0])
      sums[l][1] += float64(p[1])
      sums[l][2] += float64(p[2])
      sums[l][3] += float64(i mod w)
      sums[l][4] += float64(i div w)
      sums[l][5] += 1.0
    for k in 0 ..< centers.len:
      let sm = sums[k]
      if sm[5] > 0.0:
        for j in 0 .. 4: centers[k][j] = float32(sm[j] / sm[5])
  # Unreached pixels (possible at the far edges): the label of the pixel before.
  for i in 0 ..< n:
    if labels[i] == high(uint32): labels[i] = (if i > 0: labels[i - 1] else: 0'u32)
  let (lbl, count) = enforceConnectivity(labels, w, h, asUsize(float64((s * s) / 4'f32)))
  Superpixels(w: w, h: h, labels: lbl, count: count, step: s)

proc slic*(img: RgbImage; target: int; compactness: float32; iters: int): Superpixels =
  ## Segments `img` into about `target` superpixels. `compactness` is SLIC's `m` (10 is typical
  ## for CIELAB), `iters` the number of k-means iterations (10 in the paper).
  var lab = newSeq[array[3, float32]](img.px.len)
  for i, p in img.px: lab[i] = rgbToLab(p)
  slicLab(lab, img.w, img.h, target, compactness, iters)
