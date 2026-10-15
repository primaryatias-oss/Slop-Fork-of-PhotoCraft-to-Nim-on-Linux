## Auto-Blend pyramids and focus-stack fusion (ports of the Rust unit tests). Bit parity with Rust
## is checked by `tools/algocheck.py` (`tools/algocases/pyramid.json`, see PORTING.md).

import std/[unittest, options, math, sequtils]
import photocraft/algo/pyramid

proc checkerImg(w, h, cell, ch: int): seq[float32] =
  result = newSeq[float32](w * h * ch)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let v = (if ((x div cell) + (y div cell)) mod 2 == 0: 0.2'f32 else: 0.8'f32)
      for c in 0 ..< ch:
        result[(y * w + x) * ch + c] = (case c
          of 0: v
          of 1: v * 0.9'f32
          of 2: v * 0.5'f32
          else: 1'f32)

proc lumaOf(w, h, ch: int; px: seq[float32]): seq[float32] =
  for i in 0 ..< w * h:
    result.add (if ch >= 3: 0.2126'f32 * px[i * ch] + 0.7152'f32 * px[i * ch + 1] + 0.0722'f32 * px[i * ch + 2] else: px[i * ch])

proc blurImg(w, h, ch: int; px: seq[float32]; passes: int): seq[float32] =
  var planes = newSeq[seq[float32]](ch)
  for c in 0 ..< ch:
    for i in 0 ..< w * h: planes[c].add px[i * ch + c]
  for _ in 0 ..< passes:
    for p in planes.mitems: p = windowSum(w, h, p, binomial(2))
  for i in 0 ..< w * h * ch: result.add planes[i mod ch][i div ch]

proc rms(a, b: seq[float32]): float32 =
  var s = 0.0
  for i in 0 ..< a.len: s += float64((a[i] - b[i]) * (a[i] - b[i]))
  float32(sqrt(s / float64(a.len)))

proc fusion(levels = none(int); halo = 2'f32; guide = 2; radius = 1): StackFusion =
  StackFusion(energyRadius: radius, halo: halo, guide: guide, levels: levels)

suite "pyramids":
  test "laplacian round trip is exact":
    let (w, h) = (37, 23)
    var img = newSeq[float32](w * h * 2)
    for i in 0 ..< img.len: img[i] = float32(i * 37 mod 101) / 101'f32
    let pyr = laplacian(w, h, 2, img, 5)
    check pyr.len == 5
    let back = collapse(pyr, 2)
    for i in 0 ..< img.len: check abs(img[i] - back[i]) < 1e-4
  test "blend with one-hot weights reproduces each side":
    let (w, h) = (64, 32)
    let a = newSeqWith(w * h, 0.2'f32)
    let b = newSeqWith(w * h, 0.8'f32)
    var wa = newSeq[float32](w * h)
    for i in 0 ..< w * h: wa[i] = (if i mod w < 32: 1'f32 else: 0'f32)
    let wb = wa.mapIt(1'f32 - it)
    let o = blend(w, h, 1, [a, b], [wa, wb], autoLevels(w, h))
    check abs(o[5 * w + 2] - 0.2) < 1e-3 and abs(o[5 * w + 61] - 0.8) < 1e-3
    check o[5 * w + 31] > 0.3 and o[5 * w + 31] < 0.7
    expect ValueError: discard blend(w, h, 1, [a, b], [wa], 3)
  test "stack picks the sharp image; panorama splits the overlap":
    let (w, h) = (48, 48)
    proc checker(x, y: int): float32 = (if (x div 2 + y div 2) mod 2 == 0: 0.1'f32 else: 0.9'f32)
    var i0, i1 = newSeq[float32](w * h)
    for i in 0 ..< w * h:
      i0[i] = (if i mod w < 24: checker(i mod w, i div w) else: 0.5'f32)
      i1[i] = (if i mod w >= 24: checker(i mod w, i div w) else: 0.5'f32)
    let ones = newSeqWith(w * h, 1'f32)
    let wts = stackWeights(w, h, [i0, i1], [ones, ones])
    check wts[0][20 * w + 4] == 1 and wts[1][20 * w + 44] == 1
    let (pw, ph) = (60, 20)
    var pa, pb = newSeq[float32](pw * ph)
    for i in 0 ..< pw * ph:
      pa[i] = (if i mod pw < 40: 1'f32 else: 0'f32)
      pb[i] = (if i mod pw >= 20: 1'f32 else: 0'f32)
    let pws = panoramaWeights(pw, ph, [pa, pb])
    check pws[0][10 * pw + 5] == 1 and pws[1][10 * pw + 55] == 1
    var seam = -1
    for x in 0 ..< pw:
      if pws[1][10 * pw + x] == 1:
        seam = x
        break
    check seam in 25 .. 35
  test "stack levels, binomial window and window sum":
    check stackLevels(64, 64) == 2 and stackLevels(48, 100) == 1 and stackLevels(2070, 1378) == 6
    check stackLevels(8280, 5520) == 8 and stackLevels(100_000, 100_000) == 10
    check binomial(0) == @[1'f32] and binomial(1) == @[0.25'f32, 0.5, 0.25]
    let flat = newSeqWith(7 * 5, 0.3'f32)
    for v in windowSum(7, 5, flat, binomial(1)): check abs(v - 0.3) < 1e-6
    check windowSum(7, 5, flat, binomial(0)) == flat
  test "deviation and entropy measure local structure":
    let (w, h) = (20, 20)
    let (d0, e0) = deviationEntropy(w, h, newSeqWith(w * h, 0.5'f32))
    check d0.allIt(it == 0) and e0.allIt(it == 0)
    let (d, e) = deviationEntropy(w, h, lumaOf(w, h, 1, checkerImg(w, h, 1, 1)))
    check d[10 * w + 10] > 0.08 and e[10 * w + 10] > 0.6
  test "fusing identical images is a fixed point":
    let (w, h, ch) = (70, 50, 3)
    let im = checkerImg(w, h, 7, ch)
    let lm = lumaOf(w, h, ch, im)
    let ones = newSeqWith(w * h, 1'f32)
    let three = fusion(levels = some(3))
    for p in [three, fusion(levels = some(3), halo = 0), fusion(levels = some(3), guide = 1), fusion(levels = some(3), radius = 0)]:
      check rms(fuseStack(w, h, ch, [im, im, im], [lm, lm, lm], [ones, ones, ones], p), im) < 1e-5
    check rms(fuseStack(w, h, ch, [im], [lm], [ones], three), im) < 1e-5
    check fuseStack(w, h, ch, newSeq[seq[float32]](), newSeq[seq[float32]](), newSeq[seq[float32]](), three) == newSeq[float32](w * h * ch)
    check rms(fuseStack(w, h, ch, [im], [lm], [ones], fusion(levels = some(40))), im) < 1e-5
  test "fusion picks the sharp half from each image and ignores uncovered pixels":
    let (w, h, ch) = (96, 64, 4)
    let sharp = checkerImg(w, h, 6, ch)
    let soft = blurImg(w, h, ch, sharp, 6)
    var (a, b) = (sharp, sharp)
    for i in 0 ..< w * h:
      for c in 0 ..< ch:
        if i mod w >= w div 2: a[i * ch + c] = soft[i * ch + c] else: b[i * ch + c] = soft[i * ch + c]
    let (la, lb) = (lumaOf(w, h, ch, a), lumaOf(w, h, ch, b))
    let ones = newSeqWith(w * h, 1'f32)
    for p in [fusion(halo = 0), fusion(halo = 0, radius = 0), fusion(guide = 0)]:
      let o = fuseStack(w, h, ch, [a, b], [la, lb], [ones, ones], p)
      check rms(o, sharp) < 0.3 * rms(a, sharp)
      for i in 0 ..< w * h: check abs(o[i * ch + 3] - 1) < 1e-4
      let tol = (if p.halo > 0: 1e-3'f32 else: 0.1'f32)
      check abs(o[(20 * w + 10) * ch] - sharp[(20 * w + 10) * ch]) < tol
      check abs(o[(20 * w + 85) * ch + 1] - sharp[(20 * w + 85) * ch + 1]) < tol
    let old = blend(w, h, ch, [a, b], stackWeights(w, h, [la, lb], [ones, ones]), autoLevels(w, h))
    check rms(fuseStack(w, h, ch, [a, b], [la, lb], [ones, ones], defaultStackFusion()), sharp) <= rms(old, sharp)
    var ga = newSeq[float32](w * h * ch)
    var gb = newSeq[float32](w * h * ch)
    for i in 0 ..< w * h * ch:
      ga[i] = (if i mod ch == 3: 1'f32 else: 0.5'f32)
      gb[i] = (if (i div ch) mod w < w div 2: sharp[i] elif i mod ch == 3: 0'f32 else: 1'f32)
    var alphaB = newSeq[float32](w * h)
    for i in 0 ..< w * h: alphaB[i] = (if i mod w < w div 2: 1'f32 else: 0'f32)
    for p in [fusion(halo = 0), fusion(guide = 0)]:
      let o = fuseStack(w, h, ch, [ga, gb], [lumaOf(w, h, ch, ga), lumaOf(w, h, ch, gb)], [ones, alphaB], p)
      check abs(o[(30 * w + 12) * ch] - sharp[(30 * w + 12) * ch]) < 0.05
      for x in [60, 70, 90]:
        for c in 0 .. 2: check abs(o[(30 * w + x) * ch + c] - 0.5) < 0.03
  test "the residual rule takes the image that wins deviation and entropy":
    let (w, h, ch) = (40, 40, 3)
    let a = checkerImg(w, h, 2, ch)
    let b = newSeqWith(w * h * ch, 0.5'f32)
    let ones = newSeqWith(w * h, 1'f32)
    let one = fusion(levels = some(1), halo = 0)
    check rms(fuseStack(w, h, ch, [b, a], [lumaOf(w, h, ch, b), lumaOf(w, h, ch, a)], [ones, ones], one), a) < 1e-6
    var mean = newSeq[float32](a.len)
    for i in 0 ..< a.len: mean[i] = (a[i] + b[i]) / 2
    check rms(fuseStack(w, h, ch, [b, a], [lumaOf(w, h, ch, a), lumaOf(w, h, ch, a)], [ones, ones], one), mean) < 1e-6
    check abs(haloWeight(1e-4'f32 - 1e-8'f32, 3) - 1) < 1e-5 and haloWeight(1e-3, 2) > haloWeight(1e-4, 2)
