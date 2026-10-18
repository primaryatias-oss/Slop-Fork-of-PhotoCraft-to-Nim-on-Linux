## Select and Mask (`matting.rs`, `matting/brush.rs` and `matting/morphology.rs` tests): box mean
## and running extremes against naive versions, guided-filter matting, feather / contrast / shift,
## tiled refinement, decontamination, the Refine Edge Brush, exact octagonal morphology, plus
## no-crash checks. The module is included so its internals can be checked.

import std/unittest
import std/sequtils
include photocraft/algo/matting

proc blurredEdge(w, h: int; sigma: float32): (RgbImage, seq[float32]) =
  ## Horizontal blend from `a` (left) to `b` (right) across a Gaussian-blurred edge at x = 40.
  let (a, b) = ([0.9'f32, 0.8, 0.2], [0.1'f32, 0.2, 0.6])
  proc erf(x: float32): float32 =
    let t = 1'f32 / (1'f32 + 0.327_591_1'f32 * abs(x))
    let y = 1'f32 - (((((1.061_405_4'f32 * t - 1.453_152_1'f32) * t) + 1.421_413_7'f32) * t - 0.284_496_74'f32) * t + 0.254_829_6'f32) * t * exp(-x * x)
    if x >= 0: y else: -y
  var alpha = newSeq[float32](w)
  for x in 0 ..< w: alpha[x] = 0.5'f32 * (1'f32 - erf((float32(x) + 0.5'f32 - 40'f32) / (sigma * 1.4142135'f32)))
  var img = newRgbImage(w, h)
  for y in 0 ..< h:
    for x in 0 ..< w:
      for c in 0 .. 2: img.px[y * w + x][c] = a[c] * alpha[x] + b[c] * (1'f32 - alpha[x])
  (img, alpha)

suite "filters":
  test "max square matches the naive one":
    let (w, h) = (17, 11)
    var src = newSeq[float32](w * h)
    for i in 0 ..< w * h: src[i] = float32((i * 37) mod 23)
    for r in [1, 2, 5]:
      for isMax in [true, false]:
        let fast = maxSquare(src, w, h, r, isMax)
        for y in 0 ..< h:
          for x in 0 ..< w:
            var v = (if isMax: -F32Max else: F32Max)
            for yy in max(y - r, 0) ..< min(y + r + 1, h):
              for xx in max(x - r, 0) ..< min(x + r + 1, w):
                v = (if isMax: max(v, src[yy * w + xx]) else: min(v, src[yy * w + xx]))
            check fast[y * w + x] == v

  test "box mean matches the naive one":
    let (w, h) = (13, 9)
    var src = newSeq[float32](w * h)
    for i in 0 ..< w * h: src[i] = float32((i * 7) mod 11)
    let fast = boxMean(src, w, h, 2)
    for y in 0 ..< h:
      for x in 0 ..< w:
        var (s, n) = (0'f32, 0'f32)
        for yy in max(y - 2, 0) ..< min(y + 3, h):
          for xx in max(x - 2, 0) ..< min(x + 3, w):
            s += src[yy * w + xx]
            n += 1
        check abs(fast[y * w + x] - s / n) < 1e-4

suite "refine":
  test "guided-filter matting softens a blurred edge":
    let (w, h) = (80, 20)
    let (img, truth) = blurredEdge(w, h, 3)
    var m = newSeq[float32](w * h)
    for i in 0 ..< w * h: m[i] = (if i mod w < 40: 1'f32 else: 0'f32)
    let a = refineBuffer(some(img), m, w, h, RefineParams(radius: 10))
    let row = a[10 * w ..< 11 * w]
    var soft = 0
    for v in row:
      if v > 0.05 and v < 0.95: inc soft
    check soft >= 6
    for x in 0 ..< w:
      check abs(row[x] - truth[x]) < 0.12
      if x > 0: check row[x] <= row[x - 1] + 0.02
    check row[5] == 1'f32
    check row[75] == 0'f32
    let sharp = rgbFromFn(w, h, proc(x, y: int): array[3, float32] = (if x < 40: [0.9'f32, 0.8, 0.2] else: [0.1'f32, 0.2, 0.6]))
    let s = refineBuffer(some(sharp), m, w, h, RefineParams(radius: 10, smartRadius: true))
    soft = 0
    for v in s[10 * w ..< 11 * w]:
      if v > 0.05 and v < 0.95: inc soft
    check soft <= 2

  test "feather, contrast and shift":
    let (w, h) = (60, 10)
    var m = newSeq[float32](w * h)
    for i in 0 ..< w * h: m[i] = (if i mod w < 30: 1'f32 else: 0'f32)
    let f = refineBuffer(none(RgbImage), m, w, h, RefineParams(feather: 6))
    check f[5 * w + 29] > 0.5 and f[5 * w + 29] < 0.8 and f[5 * w + 31] > 0.1
    let c = refineBuffer(none(RgbImage), f, w, h, RefineParams(contrast: 100))
    check c[5 * w + 29] == 1'f32 and c[5 * w + 31] == 0'f32
    let grown = refineBuffer(none(RgbImage), m, w, h, RefineParams(shiftEdge: 100))
    check grown[5 * w + 33] == 1'f32
    check grown[5 * w + 34] == 0'f32
    let shrunk = refineBuffer(none(RgbImage), m, w, h, RefineParams(shiftEdge: -50))
    check shrunk[5 * w + 27] == 1'f32
    check shrunk[5 * w + 28] == 0'f32
    let sm = refineBuffer(none(RgbImage), m, w, h, RefineParams(smooth: 50))
    check sm[5 * w + 10] == 1'f32 and sm[5 * w + 50] == 0'f32

  test "refine-mask tiles match a single buffer":
    let (w, h) = (600, 300)
    let (img, _) = blurredEdge(w, h, 2)
    var mask = newSeq[uint8](w * h)
    for y in 0 ..< h:
      for x in 0 ..< w:
        let (dx, dy) = (float32(x) - 40, float32(y) - 150)
        mask[y * w + x] = (if dx * dx * 0.05'f32 + dy * dy < 120'f32 * 120 or x < 40: 255'u8 else: 0'u8)
    let reg = Region(bbox: rect(0, 0, w, h), mask: mask)
    let p = RefineParams(radius: 6, feather: 2, contrast: 10)
    let outp = refineMask(imageSampler(img), regionReader(reg), reg.bbox, reg.bbox, p).get
    var whole = newSeq[float32](w * h)
    for i, v in reg.mask: whole[i] = float32(v) / 255'f32
    let full = refineBuffer(some(img), whole, w, h, p)
    var maxd = 0'f32
    for y in 0 ..< h:
      for x in 0 ..< w: maxd = max(maxd, abs(outp.at(x, y) - full[y * w + x]))
    check maxd <= 1.5'f32 / 255

  test "decontamination and masked copies at any depth":
    for sample in [stU8, stU16, stF32]:
      let fmt = Rgba8.withSample(sample)
      var s = newSurface(fmt)
      s.fillRect(rect(0, 0, 10, 8), [1'f32, 0, 0, 1])
      s.fillRect(rect(10, 0, 20, 8), [0.5'f32, 0, 0.5, 1])
      s.fillRect(rect(11, 0, 20, 8), [0'f32, 0, 1, 1])
      var mask = newSeq[uint8](20 * 8)
      for i in 0 ..< mask.len: mask[i] = (if i mod 20 < 10: 255'u8 elif i mod 20 == 10: 128'u8 else: 0'u8)
      let a = Region(bbox: rect(0, 0, 20, 8), mask: mask)
      let d = decontaminate(s, a, 3, 100)
      let p = d.pixel(10, 4)
      check p[0] > 0.9 and p[2] < 0.1
      check d.pixel(15, 4) == s.pixel(15, 4)
      let c = maskedCopy(d, a)
      check c.format == fmt
      check abs(c.pixel(10, 4)[3] - 128'f32 / 255) < 0.01
      check c.pixel(15, 4)[3] == 0'f32
      check c.pixel(3, 3)[0] == 1'f32
    var g = newSurface(Gray8)
    g.fillRect(rect(0, 0, 4, 4), [0.5'f32])
    let c = maskedCopy(g, Region(bbox: rect(0, 0, 2, 4), mask: newSeqWith(8, 255'u8)))
    check c.format == GrayA8
    check c.pixel(3, 1)[1] == 0'f32
    check abs(c.pixel(1, 1)[0] - 0.5) < 0.01 and c.pixel(1, 1)[1] == 1'f32

suite "refine edge brush":
  test "picks up strands beyond the band and restores the edge":
    let (w, h) = (120, 60)
    let (fg, bg) = ([0.9'f32, 0.7, 0.2], [0.1'f32, 0.3, 0.8])
    proc strand(x: int): bool = x < 40 or (x in 60 ..< 84 and x mod 4 == 0)
    let img = rgbFromFn(w, h, proc(x, y: int): array[3, float32] = (if strand(x): fg else: bg))
    let canvas = rect(0, 0, w, h)
    var mask = newSeq[uint8](w * h)
    for i in 0 ..< w * h: mask[i] = (if i mod w < 40: 255'u8 else: 0'u8)
    let reg = Region(bbox: canvas, mask: mask)
    let p = RefineParams(radius: 4)
    proc refine(strokes: seq[EdgeStroke]): Region =
      refineMaskBrushed(imageSampler(img), regionReader(reg), reg.bbox, canvas, p, newEdgeBrush(strokes, canvas)).get
    check refine(@[]).at(64, 30) < 0.01
    let brushed = refine(@[EdgeStroke(points: @[(70'f32, 10'f32), (70'f32, 50'f32)], size: 30, add: true)])
    check brushed.at(64, 30) > 0.9 and brushed.at(72, 30) > 0.9
    check brushed.at(66, 30) < 0.1
    check brushed.at(20, 30) > 0.99 and brushed.at(100, 30) < 0.01
    let soft = RefineParams(radius: 4)
    let blurred = rgbFromFn(w, h, proc(x, y: int): array[3, float32] =
      let t = clamp((float32(x) - 36) / 8, 0'f32, 1'f32)
      [fg[0] * (1 - t) + bg[0] * t, fg[1] * (1 - t) + bg[1] * t, fg[2] * (1 - t) + bg[2] * t])
    let refined = refineMaskBrushed(imageSampler(blurred), regionReader(reg), reg.bbox, canvas, soft, none(EdgeBrush)).get
    check refined.at(41, 30) > 0.1
    let restore = @[EdgeStroke(points: @[(40'f32, 0'f32), (40'f32, 60'f32)], size: 20, add: false)]
    let restored = refineMaskBrushed(imageSampler(blurred), regionReader(reg), reg.bbox, canvas, soft, newEdgeBrush(restore, canvas)).get
    check restored.at(41, 30) < 0.01 and restored.at(38, 30) > 0.99

  test "strokes paint in order and clip to the canvas":
    let canvas = rect(0, 0, 100, 50)
    let add = EdgeStroke(points: @[(10'f32, 25'f32), (90'f32, 25'f32)], size: 10, add: true)
    var b = newEdgeBrush(@[add], canvas).get
    check b.at(50, 25) > 0.99 and b.at(50, 40) == 0'f32
    check b.window == 10
    let sub = EdgeStroke(points: @[(50'f32, 25'f32)], size: 20, add: false)
    b = newEdgeBrush(@[add, sub], canvas).get
    check b.at(50, 25) < -0.99 and b.at(20, 25) > 0.99
    let off = EdgeStroke(points: @[(-500'f32, -500'f32)], size: 10, add: true)
    let nan = EdgeStroke(points: @[(NaN.float32, 3'f32)], size: 10, add: true)
    let empty = EdgeStroke(size: 10, add: true)
    check newEdgeBrush(@[off, nan, empty], canvas).isNone

suite "morphology":
  test "every small binary mask matches repeated octagons":
    for (w, h) in [(3, 2), (2, 3), (4, 4)]:
      for bits in 0 ..< (1 shl (w * h)):
        var src = newSeq[float32](w * h)
        for i in 0 ..< w * h: src[i] = float32((bits shr i) and 1)
        for r in 0 .. 6:
          for grow in [false, true]: check morph(src, w, h, r, grow) == repeated(src, w, h, r, grow)

  test "grey and HDR values match at edges and every radius":
    var z = 3'u64
    for (w, h) in [(9, 7), (1, 13), (16, 1), (20, 11)]:
      for depth in [1'f32, 255, 4.5]:
        var src = newSeq[float32](w * h)
        for v in src.mitems:
          z = z * 6364136223846793005'u64 + 1442695040888963407'u64
          v = float32(z shr 40) / float32(1'u64 shl 24) * depth
        for r in 0 .. 24:
          for grow in [false, true]: check morph(src, w, h, r, grow) == repeated(src, w, h, r, grow)

  test "exceptional floats keep the old visitation order":
    let src = @[0'f32, -0'f32, NaN.float32, 1, -1, 0.5, Inf.float32, -0'f32, 2]
    for r in 1 .. 5:
      for grow in [false, true]:
        let (a, b) = (morph(src, 3, 3, r, grow), repeated(src, 3, 3, r, grow))
        for i in 0 ..< a.len: check cast[uint32](a[i]) == cast[uint32](b[i])

  test "invalid sizes and large radii are bounded":
    check morph(@[], high(int) div 2, 2, 3, true).len == 0
    check morph(@[0'f32], 2, 2, 3, true).len == 0
    check morph(@[], 0, 5, high(int), true).len == 0
    check morph(@[0'f32, 0.5, 1, 0.1], 2, 2, high(int), true) == @[1'f32, 1, 1, 1]
    check morph(@[0'f32, 0.5, 1, 0.1, 42], 2, 2, 5, true) == @[1'f32, 1, 1, 1, 42]
    var src = newSeq[float32](2 * 30001)
    for i in 0 ..< src.len: src[i] = float32(i mod 251)
    check morph(src, 2, 30001, high(int), true) == newSeqWith(src.len, 250'f32)
    check morph(src, 2, 30001, high(int), false) == newSeqWith(src.len, 0'f32)

suite "hostile input":
  test "matting does not crash":
    expect ValueError: discard refineBuffer(none(RgbImage), @[1'f32], 3, 3, RefineParams())
    expect ValueError: discard gaussianBlur(@[0.5'f32, 0.5], 2, 1, 1e30)
    discard refineBuffer(none(RgbImage), newSeq[float32](9), 3, 3, RefineParams(radius: NaN, smooth: Inf, shiftEdge: NaN))
    let img = rgbFromFn(9, 9, proc(x, y: int): array[3, float32] = [NaN.float32, 0, 1])
    discard refineBuffer(some(img), newSeqWith(81, 0.5'f32), 9, 9, RefineParams(radius: 1e9, smartRadius: true, contrast: 1e9))
    check refineMask(imageSampler(img), regionReader(Region(bbox: rect(0, 0, 9, 9), mask: newSeq[uint8](81))),
                     EmptyRect, rect(0, 0, 9, 9), RefineParams(radius: 3)).isNone
    check newEdgeBrush(@[EdgeStroke(points: @[(0'f32, 0'f32), (1e30'f32, 0'f32)], size: 5000, add: true)], rect(0, 0, 9, 9)).isNone
    discard sweptArea(@[EdgeStroke(points: @[(NaN.float32, 0'f32), (1'f32, 2'f32)], size: NaN, add: true)])
    discard edgeWidth(newRgbImage(0, 0), 3)
    discard boxMean(@[], 0, 0, 3)
