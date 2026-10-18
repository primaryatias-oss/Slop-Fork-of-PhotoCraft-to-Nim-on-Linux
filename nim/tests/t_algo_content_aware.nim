## Content-Aware Fill (`content_aware.rs` tests): cancellation and progress, in-place fills, the
## sampling window, the Auto sampling area, colour adaptation, exact copies, the Patch Tool's
## colour amount, variant geometry, plus no-crash checks. The module is included so its internals
## can be checked.

import std/[unittest, sets, sequtils]
include photocraft/algo/content_aware

proc stripes(w, h: int): (seq[float32], seq[bool]) =
  ## Vertical stripes (period 6) in grey, with a hole in the middle.
  var img = newSeq[float32](w * h)
  var hole = newSeq[bool](w * h)
  for i in 0 ..< w * h:
    img[i] = (if (i mod w) mod 6 < 3: 0.2'f32 else: 0.8'f32)
    hole[i] = abs(i mod w - w div 2) < 4 and abs(i div w - h div 2) < 4
  (img, hole)

proc noise(w, h: int; seed: uint64): seq[float32] =
  ## I.i.d. noise: every value occurs once, so a filled pixel equal to an image pixel was copied.
  var z = seed
  for _ in 0 ..< w * h * 3:
    z = z * 6364136223846793005'u64 + 1442695040888963407'u64
    result.add 0.2'f32 + 0.6'f32 * (float32(z shr 40) / float32(1'u64 shl 24))

proc noAdapt(): FillOptions =
  result = defaultFillOptions()
  result.colorAdaptation = AdaptNone

suite "content-aware fill":
  test "a cancelled fill stops; progress reaches one":
    let (w, h) = (60, 40)
    let (img, hole) = stripes(w, h)
    let source = newSeqWith(w * h, true)
    expect Cancelled:
      discard fillWith(w, h, 1, img, hole, source, defaultFillOptions(), newInterrupt(proc(): bool = true))
    var last = 0'f32
    var backwards = false
    let outp = fillWith(w, h, 1, img, hole, source, defaultFillOptions(), newInterrupt(proc(): bool = false, proc(f: float32) =
      if f < last: backwards = true
      last = f))
    check not backwards
    check last == 1'f32
    check outp == fill(w, h, 1, img, hole, source, defaultFillOptions())

  test "only the hole changes, with or without adaptations":
    let (w, h) = (90, 70)
    var rgb = noise(w, h, 4)
    for i in 0 ..< rgb.len: rgb[i] = rgb[i] * 0.5'f32 + 0.3'f32 * float32(((i div 3) mod w) mod 11 < 5)
    var hole, source = newSeq[bool](w * h)
    for i in 0 ..< w * h:
      hole[i] = i mod w in 30 ..< 52 and i div w in 25 ..< 44
      source[i] = i mod w < 75
    var mirrored = noAdapt()
    mirrored.mirror = true
    for opts in [defaultFillOptions(), mirrored]:
      let outp = fill(w, h, 3, rgb, hole, source, opts)
      for i in 0 ..< w * h:
        if not hole[i]:
          for c in 0 .. 2: check cast[uint32](outp[i * 3 + c]) == cast[uint32](rgb[i * 3 + c])
    let nowhere = newSeq[bool](w * h)
    check fill(w, h, 3, rgb, hole, nowhere, defaultFillOptions()) == membraneFill(w, h, 3, rgb, hole)

  test "auto sampling can be cancelled":
    let (w, h) = (200, 160)
    var img = noise(w, h, 5)
    for i in 0 ..< img.len: img[i] = img[i] * 0.1'f32 + (if (i div 3) mod w < 120: 0.3'f32 else: 0.6'f32)
    var hole = newSeq[bool](w * h)
    for i in 0 ..< w * h: hole[i] = i mod w in 80 ..< 110 and i div w in 60 ..< 90
    expect Cancelled:
      discard autoSamplingWith(w, h, 3, img, hole, newInterrupt(proc(): bool = true))
    check autoSamplingWith(w, h, 3, img, hole, NoInterrupt) == autoSampling(w, h, 3, img, hole)

  test "fills only the hole, with the known texture":
    let (w, h) = (48, 32)
    let (img, hole) = stripes(w, h)
    let outp = fill(w, h, 1, img, hole, newSeqWith(w * h, true), noAdapt())
    var (lo, hi) = (false, false)
    for i in 0 ..< w * h:
      if not hole[i]: check outp[i] == img[i]
      else:
        if outp[i] < 0.35: lo = true
        if outp[i] > 0.65: hi = true
    check lo and hi

  test "the sampling area restricts the sources":
    let (w, h) = (40, 20)
    var img = newSeq[float32](w * h)
    var hole, right = newSeq[bool](w * h)
    for i in 0 ..< w * h:
      img[i] = (if i mod w < 20: 0.1'f32 else: 0.9'f32)
      hole[i] = i mod w in 16 ..< 24 and i div w in 6 ..< 14
      right[i] = i mod w >= 24
    let outp = fill(w, h, 1, img, hole, right, noAdapt())
    var s = 0'f32
    for i in 0 ..< w * h:
      if hole[i]: s += outp[i]
    check s / 64 > 0.8

  test "adaptations run and stay deterministic":
    let (w, h) = (32, 24)
    let (img, hole) = stripes(w, h)
    var rgb: seq[float32]
    for v in img: rgb.add [v, v * 0.5'f32, 1'f32 - v]
    let opts = FillOptions(colorAdaptation: colorLevel("high"), rotations: rotationLevel("low"), scale: true, mirror: true, seed: 7)
    let a = fill(w, h, 3, rgb, hole, newSeqWith(w * h, true), opts)
    check a == fill(w, h, 3, rgb, hole, newSeqWith(w * h, true), opts)
    for v in a: check v.isFinite and v >= 0 and v <= 1

  test "colour adaptation follows a gradient":
    let (w, h) = (40, 20)
    var img = newSeq[float32](w * h)
    var hole = newSeq[bool](w * h)
    for i in 0 ..< w * h:
      img[i] = float32(i mod w) / float32(w)
      hole[i] = i mod w in 14 ..< 26 and i div w in 5 ..< 15
    var o = defaultFillOptions()
    o.colorAdaptation = colorLevel("veryHigh")
    let outp = fill(w, h, 1, img, hole, newSeqWith(w * h, true), o)
    var err = 0'f32
    for i in 0 ..< w * h:
      if hole[i]: err += abs(outp[i] - img[i])
    check err / 120 < 0.12

  test "the sampling window is Photoshop's":
    let canvas = rect(0, 0, 800, 800)
    proc sq(x0, y0, w, h: int): Rect = rect(x0, y0, x0 + w, y0 + h)
    check samplingWindow(sq(375, 375, 50, 50), canvas) == sq(300, 300, 200, 200)
    check samplingWindow(sq(350, 350, 100, 100), canvas) == sq(200, 200, 400, 400)
    check samplingWindow(sq(395, 395, 10, 10), canvas) == sq(300, 300, 200, 200)
    check samplingWindow(sq(370, 370, 60, 60), canvas) == sq(280, 280, 240, 240)
    check samplingWindow(sq(350, 385, 100, 30), canvas) == sq(258, 258, 283, 283)
    check samplingWindow(sq(300, 375, 200, 50), canvas) == sq(200, 200, 400, 400)
    check samplingWindow(sq(250, 350, 300, 100), canvas) == sq(53, 53, 693, 693)
    check samplingWindow(sq(10, 375, 50, 50), canvas) == sq(0, 300, 200, 200)
    check samplingWindow(sq(5, 5, 60, 60), canvas) == sq(0, 0, 240, 240)
    check samplingWindow(sq(40, 40, 50, 50), rect(0, 0, 150, 120)) == rect(0, 0, 150, 120)
    check samplingWindow(sq(100, 995, 1800, 10), rect(0, 0, 2000, 2000)) == sq(100, 400, 1800, 1200)
    check samplingWindow(EmptyRect, canvas) == EmptyRect

  test "auto sampling keeps what looks like the surroundings":
    let (w, h) = (200, 200)
    let tex = noise(w, h, 3)
    proc shade(x: int; band: bool): float32 =
      if band: (if x in 130 ..< 150: 0.55'f32 else: 0.35'f32)
      elif x < 135: 0.35'f32
      else: 0.55'f32
    for band in [false, true]:
      var img = newSeq[float32](w * h * 3)
      for i in 0 ..< img.len: img[i] = shade((i div 3) mod w, band) + (tex[i] - 0.5'f32) * 0.1'f32
      var hole = newSeq[bool](w * h)
      for i in 0 ..< w * h: hole[i] = i mod w in 75 ..< 125 and i div w in 75 ..< 125
      let area = autoSampling(w, h, 3, img, hole)
      proc frac(x0, x1: int): float32 =
        var (n, k) = (0, 0)
        for i in 0 ..< w * h:
          if i mod w in x0 ..< x1:
            if not hole[i]: inc n
            if area[i]: inc k
        float32(k) / float32(n)
      for i in 0 ..< w * h: check not (hole[i] and area[i])
      check frac(0, 125) > 0.95
      if band:
        check frac(133, 147) < 0.05
        check frac(155, 200) > 0.9
      else:
        check frac(140, 200) < 0.05

  test "without colour adaptation the fill copies pixels exactly":
    let (w, h) = (120, 120)
    let fine = noise(w, h, 9)
    var img = newSeq[float32](w * h * 3)
    for i in 0 ..< img.len:
      let (x, y, c) = (float32((i div 3) mod w), float32((i div 3) div w), i mod 3)
      img[i] = 0.5'f32 + 0.2'f32 * sin(x * 0.21'f32 + float32(c)) * cos(y * 0.17'f32) + 0.1'f32 * sin(x * 0.05'f32 - y * 0.08'f32) +
               (fine[i] - 0.5'f32) * 0.08'f32
    var hole = newSeq[bool](w * h)
    for i in 0 ..< w * h: hole[i] = i mod w in 40 ..< 80 and i div w in 40 ..< 80
    proc key(v: seq[float32]; i: int): array[3, int64] =
      for c in 0 .. 2: result[c] = int64(round(v[i * 3 + c] * 1e5'f32))
    var known = initHashSet[array[3, int64]]()
    for i in 0 ..< w * h:
      if not hole[i]: known.incl key(img, i)
    let outp = fill(w, h, 3, img, hole, newSeqWith(w * h, true), noAdapt())
    var exact = 0
    for i in 0 ..< w * h:
      if hole[i] and key(outp, i) in known: inc exact
    check float32(exact) > 0.2'f32 * 1600
    proc lap(v: seq[float32]; x, y: int): float32 =
      proc p(x, y: int): float32 = v[(y * w + x) * 3]
      abs(4'f32 * p(x, y) - p(x - 1, y) - p(x + 1, y) - p(x, y - 1) - p(x, y + 1))
    proc mean(inside: bool): float32 =
      var (s, n) = (0'f32, 0)
      for y in 1 ..< h - 1:
        for x in 1 ..< w - 1:
          if hole[y * w + x] == inside:
            s += lap(outp, x, y)
            inc n
      s / float32(n)
    check mean(true) > 0.6'f32 * mean(false)

  test "the Patch Tool's colour shifts up to about four hundredths per step":
    var frm, to: seq[float32]
    for i in 0 ..< 300: frm.add 0.7'f32 + 0.05'f32 * (float32(i mod 7) / 7'f32)
    for v in frm: to.add v - 0.4'f32
    var level = 0'f32
    for v in frm: level += v
    level /= 300
    proc shift(c: uint8): float32 =
      let gb = levelTransform(1, frm, to, colorAmount(c))
      var s = 0'f32
      for v in frm: s += gb[0][0] * v + gb[0][1]
      level - s / 300
    check shift(0) == 0'f32
    check abs(shift(3) - 0.12) < 0.01
    check abs(shift(10) - 0.4) < 0.01

  test "variant geometry":
    let img = newSeqWith(60, 0.5'f32)
    let ok = newSeqWith(60, true)
    var v = variant(10, 6, 1, img, ok, 90, 1, false, NoInterrupt)
    check (v.w, v.h) == (6, 10)
    v = variant(10, 6, 1, img, ok, 0, 0.8, true, NoInterrupt)
    check (v.w, v.h) == (8, 5)
    check v.ok.countIt(it) > 20

  test "hostile input does not crash":
    expect ValueError: discard fill(5, 5, 1, newSeq[float32](3), newSeq[bool](25), newSeq[bool](25), defaultFillOptions())
    expect ValueError: discard autoSampling(5, 5, 1, newSeq[float32](25), newSeq[bool](2))
    var o = defaultFillOptions()
    o.rotations = @[NaN.float32, 1e30'f32]
    let (img, hole) = stripes(20, 12)
    discard fill(20, 12, 1, img, hole, newSeqWith(240, true), o)
    o = defaultFillOptions()
    o.colorAdaptation = Adapt(gain: (5'f32, 0.1'f32), bias: -3)
    discard fill(20, 12, 1, img, hole, newSeqWith(240, true), o)
    check samplingWindow(rect(0, 0, high(int32), high(int32)), rect(0, 0, 10, 10)) == rect(0, 0, 10, 10)
    check levelTransform(0, @[], @[], AdaptNone).len == 0
