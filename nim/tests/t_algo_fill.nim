## Poisson blending, non-local completion, the Remove Tool, Perspective Warp, Crop and Straighten
## Photos and Red Eye (ports of the Rust unit tests). Bit parity with Rust is checked by
## `tools/algocheck.py` (see PORTING.md).

import std/[unittest, options, math, sequtils]
import photocraft/[raster]
import photocraft/algo/[poisson, nonlocal, remove, perspective, scancrop, redeye, transform]

proc disc(w, h: int; cx, cy, r: float32): seq[bool] =
  result = newSeq[bool](w * h)
  for i in 0 ..< w * h: result[i] = hypot(float32(i mod w) - cx, float32(i div w) - cy) < r

proc grid(w, h: int; f: proc(x, y: int): bool): seq[bool] =
  result = newSeq[bool](w * h)
  for i in 0 ..< w * h: result[i] = f(i mod w, i div w)

proc maxErr(a, b: seq[float32]): float32 =
  for i in 0 ..< a.len: result = max(result, abs(a[i] - b[i]))

suite "poisson":
  test "membrane reproduces a linear boundary":
    let (w, h) = (80, 60)
    let unknown = disc(w, h, 40, 30, 20)
    var truth = newSeq[float32](w * h)
    for i in 0 ..< w * h: truth[i] = 0.2'f32 + 0.005'f32 * float32(i mod w) + 0.003'f32 * float32(i div w)
    var v = newSeq[float32](w * h)
    for i in 0 ..< w * h: v[i] = (if unknown[i]: 0'f32 else: truth[i])
    solveMembrane(w, h, unknown, v)
    check maxErr(v, truth) < 2e-3
  test "multigrid matches the reference solve; known pixels untouched":
    let (w, h) = (96, 72)
    let unknown = disc(w, h, 45, 36, 28)
    var init = newSeq[float32](w * h)
    for i in 0 ..< w * h:
      init[i] = sin(float32(i mod w) * 0.21'f32) * 0.3'f32 + cos(float32(i div w) * 0.13'f32) * 0.2'f32 + 0.5'f32
    var fast = init
    solveMembrane(w, h, unknown, fast)
    var reference = init
    solveMembraneReference(w, h, unknown, reference, 1e-7, 100_000)
    check maxErr(fast, reference) < 2e-3
    for i in 0 ..< w * h:
      if not unknown[i]: check fast[i] == init[i]
  test "seamless clone takes the destination colour and the source texture":
    let (w, h, ch) = (64, 64, 3)
    let mask = disc(w, h, 32, 32, 20)
    var dst, src: seq[float32]
    for i in 0 ..< w * h:
      dst.add [0.7'f32, 0.3, 0.2]
      let t = (if (i mod w) div 4 mod 2 == 0: 0.05'f32 else: -0.05'f32)
      src.add [0.1'f32 + t, 0.2'f32 + t, 0.8'f32 + t]
    let o = seamlessClone(w, h, ch, src, dst, mask)
    var inside: seq[int]
    for i in 0 ..< w * h:
      if mask[i]: inside.add i
    for c in 0 ..< ch:
      var m = 0'f32
      for i in inside: m += o[i * ch + c]
      check abs(m / float32(inside.len) - dst[c]) < 0.02
    var (e, n) = (0'f32, 0)
    for i in inside:
      if mask[i + 1]:
        e += abs((o[(i + 1) * ch] - o[i * ch]) - (src[(i + 1) * ch] - src[i * ch]))
        inc n
    check e / float32(n) < 0.01
    check o[0 ..< 3] == dst[0 ..< 3]
  test "bad sizes raise instead of indexing out of range":
    var v = newSeq[float32](3)
    expect ValueError: solveMembrane(4, 4, newSeq[bool](16), v)
    expect ValueError: discard seamlessClone(2, 2, 1, @[0'f32], @[0'f32], @[true])

proc noise(i: int): float32 = float32((uint32(i) * 2_654_435_761'u32) shr 8) / float32(1'u32 shl 24)

proc grain(w: int; img: seq[float32]; m: seq[bool]): float32 =
  var (s, n) = (0'f32, 0)
  for i, on in m:
    if on and i mod w + 1 < w and i + w < img.len:
      s += abs(img[i + 1] - img[i]) + abs(img[i + w] - img[i])
      inc n
  s / float32(max(n, 1))

suite "non-local completion":
  test "a hole in texture next to a flat area of the same mean gets texture":
    let (w, h) = (96, 64)
    var img = newSeq[float32](w * h)
    for i in 0 ..< w * h: img[i] = (if i mod w < 48: 0.5'f32 else: 0.25'f32 + 0.5'f32 * noise(i))
    let hole = grid(w, h, proc(x, y: int): bool = x in 60 ..< 84 and y in 20 ..< 44)
    let o = completeWith(w, h, 1, img, hole, defaultParams(), NoInterrupt).get
    check grain(w, o, hole) > grain(w, img, hole) * 0.7
    for i in 0 ..< w * h:
      if not hole[i]: check o[i] == img[i]
  test "a hole in a gradient follows the gradient":
    let (w, h, ch) = (80, 60, 2)
    var img: seq[float32]
    for i in 0 ..< w * h: img.add [float32(i mod w) / float32(w), 1'f32]
    let hole = grid(w, h, proc(x, y: int): bool = x in 30 ..< 50 and y in 20 ..< 40)
    let o = completeWith(w, h, ch, img, hole, defaultParams(), NoInterrupt).get
    for i in 0 ..< w * h:
      if hole[i]: check abs(o[i * ch] - img[i * ch]) < 0.1
  test "levels follow the hole thickness":
    let (w, h) = (400, 400)
    let line = grid(w, h, proc(x, y: int): bool = y == 200 and x in 20 ..< 380)
    check levelCount(w, h, line, 3) == 1
    let d = grid(w, h, proc(x, y: int): bool = hypot(float32(x) - 200, float32(y) - 200) < 90)
    check levelCount(w, h, d, 3) == 5
  test "nothing to copy from and bad sizes":
    let img = newSeq[float32](144).mapIt(0.5'f32)
    check completeWith(12, 12, 1, img, newSeqWith(144, true), defaultParams(), NoInterrupt).isNone
    check completeWith(12, 12, 1, img, @[true, true, true], defaultParams(), NoInterrupt) == some(img)
    check completeWith(12, 12, 0, img, newSeqWith(144, true), defaultParams(), NoInterrupt) == some(img)
  test "cancelled is an error":
    let (w, h) = (64, 64)
    var img = newSeq[float32](w * h)
    for i in 0 ..< w * h: img[i] = noise(i)
    let hole = grid(w, h, proc(x, y: int): bool = x in 20 ..< 40 and y in 20 ..< 40)
    expect Cancelled: discard completeWith(w, h, 1, img, hole, defaultParams(), newInterrupt(proc(): bool = true))

suite "remove tool":
  test "a ring closes into a disc and an open line does not":
    let (w, h) = (40, 40)
    let ring = grid(w, h, proc(x, y: int): bool =
      let d = hypot(float32(x) - 20, float32(y) - 20)
      d >= 10 and d < 12)
    let closed = closeLoops(w, h, ring)
    check closed[20 * w + 20] and not closed[2 * w + 2]
    let line = grid(w, h, proc(x, y: int): bool = y == 20 and x in 5 ..< 35)
    check closeLoops(w, h, line) == line
  test "a loop open at one pixel encloses nothing":
    let (w, h) = (20, 20)
    var sq = grid(w, h, proc(x, y: int): bool = (x == 5 or x == 14 or y == 5 or y == 14) and x in 5 .. 14 and y in 5 .. 14)
    sq[5 * w + 9] = false
    check not closeLoops(w, h, sq)[10 * w + 10]
  test "dilate grows by the radius":
    let (w, h) = (11, 11)
    let dot = grid(w, h, proc(x, y: int): bool = x == 5 and y == 5)
    let d = dilate(w, h, dot, 2)
    check d.countIt(it) == 25
    check d[3 * w + 3] and d[7 * w + 7] and not d[2 * w + 5]
    check dilate(w, h, dot, 0) == dot
  test "radius measures the thickest part":
    let (w, h) = (30, 30)
    check holeRadius(w, h, grid(w, h, proc(x, y: int): bool = y == 10 and x in 5 ..< 25)) == 1
    check holeRadius(w, h, grid(w, h, proc(x, y: int): bool = x in 5 ..< 16 and y in 5 ..< 16)) == 6
    check holeRadius(w, h, newSeqWith(w * h, true)) == high(uint32)
    check holeRadius(w, h, [true]) == 0
  test "removes thin and wide holes from a gradient":
    let (w, h, ch) = (64, 48, 2)
    var truth: seq[float32]
    for i in 0 ..< w * h: truth.add [float32(i mod w) / float32(w), 1'f32]
    for hole in [grid(w, h, proc(x, y: int): bool = y == 24 and x in 10 ..< 54),
                 grid(w, h, proc(x, y: int): bool = x in 24 ..< 40 and y in 16 ..< 32)]:
      var img = truth
      for i, m in hole:
        if m: img[i * ch] = 1
      let o = removeWith(w, h, ch, img, hole, 7, NoInterrupt)
      for i, m in hole:
        if m: check abs(o[i * ch] - truth[i * ch]) < 0.12
        else: check o[i * ch ..< (i + 1) * ch] == img[i * ch ..< (i + 1) * ch]
  test "a long thin hole is filled in tiles":
    let (w, h) = (900, 700)
    var img = newSeq[float32](w * h)
    for i in 0 ..< w * h: img[i] = 0.3'f32 + 0.4'f32 * float32(((i mod w) div 7 + (i div w) div 5) mod 2)
    let hole = grid(w, h, proc(x, y: int): bool = abs(float32(x) - float32(y) * 1.2'f32 - 20) < 3 and y in 40 ..< 680)
    var holed = img
    for i, m in hole:
      if m: holed[i] = 1
    let o = removeWith(w, h, 1, holed, hole, 3, NoInterrupt)
    var ok = true
    for i, m in hole:
      if m: ok = ok and o[i] >= 0.29'f32 and o[i] <= 0.71'f32
      else: ok = ok and o[i] == holed[i]
    check ok
  test "hostile sizes return the input":
    let img = newSeqWith(12, 0.5'f32)
    check removeWith(2, 2, 3, img, @[true], 1, NoInterrupt) == img
    check removeWith(2, 2, 0, img, newSeqWith(4, true), 1, NoInterrupt) == img
    check removeWith(3, 2, 2, img, newSeq[bool](6), 1, NoInterrupt) == img
    check closeLoops(0, 0, newSeq[bool]()).len == 0
    check dilate(3, 3, [true], 1) == @[true]

proc close(a, b: (float64, float64); tol: float64): bool = abs(a[0] - b[0]) <= tol and abs(a[1] - b[1]) <= tol

suite "perspective warp":
  test "identity and a known homography":
    let q: Quad = [[10.0, 10.0], [90.0, 12.0], [85.0, 70.0], [12.0, 75.0]]
    let m = newPerspectiveMap([identityPlane(q)]).get
    for p in [(20.0, 20.0), (50.0, 40.0), (0.0, 0.0), (120.0, 90.0)]:
      check close(m.map(p[0], p[1]), p, 1e-9)
    let src: Quad = [[0.0, 0.0], [100.0, 0.0], [100.0, 100.0], [0.0, 100.0]]
    let dst: Quad = [[20.0, 0.0], [80.0, 0.0], [100.0, 100.0], [0.0, 100.0]]
    let m2 = newPerspectiveMap([Plane(src: src, dst: dst)]).get
    for k in 0 .. 3: check close(m2.map(src[k][0], src[k][1]), (dst[k][0], dst[k][1]), 1e-9)
    let hm = rectToQuad([0.0, 0.0, 100.0, 100.0], dst).get
    check close(m2.map(50, 50), hm.apply(50, 50), 1e-9)
    let c = m2.map(50, 50)
    check abs(c[0] - 50) < 1e-9 and c[1] < 50
  test "shared edges stay continuous":
    let a = Plane(src: [[0.0, 0.0], [50.0, 0.0], [50.0, 100.0], [0.0, 100.0]], dst: [[0.0, 10.0], [50.0, 0.0], [55.0, 100.0], [0.0, 90.0]])
    let b = Plane(src: [[50.2, 0.0], [100.0, 0.0], [100.0, 100.0], [50.0, 100.2]], dst: [[50.0, 0.0], [110.0, 20.0], [100.0, 80.0], [55.0, 100.0]])
    let m = newPerspectiveMap([a, b]).get
    for t in [0.05, 0.25, 0.5, 0.77, 0.95]:
      let (x, y) = (50.1 - 0.1 * t, 100.1 * t)
      check close(m.map(x - 1e-6, y), m.map(x + 1e-6, y), 1e-4)
    var zero: Quad
    check linkedCorners([identityPlane(zero)]).len == 1
  test "straighten levels and plumbs edges":
    var ps = @[Plane(src: [[0.0, 0.0], [100.0, 0.0], [100.0, 100.0], [0.0, 100.0]], dst: [[0.0, 3.0], [100.0, -2.0], [96.0, 100.0], [4.0, 104.0]])]
    straighten(ps, stVertical)
    check ps[0].dst[1][0] == ps[0].dst[2][0] and ps[0].dst[0][0] == ps[0].dst[3][0]
    check ps[0].dst[0][1] != ps[0].dst[1][1]
    straighten(ps, stAuto)
    check ps[0].dst[0][1] == ps[0].dst[1][1] and ps[0].dst[2][1] == ps[0].dst[3][1]
  test "degenerate planes give no map":
    check newPerspectiveMap(newSeq[Plane]()).isNone
    var dot: Quad = [[5.0, 5.0], [5.0, 5.0], [5.0, 5.0], [5.0, 5.0]]
    check newPerspectiveMap([identityPlane(dot)]).isNone
    check parseStraighten("Level") == some(stHorizontal) and parseStraighten("x").isNone

suite "crop and straighten photos":
  proc scan(w, h: int; photos: openArray[FoundPhoto]): seq[array[4, float32]] =
    result = newSeqWith(w * h, [0.97'f32, 0.97, 0.96, 1])
    for k, p in photos:
      let (s, c) = (sin(degToRad(p.angle)), cos(degToRad(p.angle)))
      for y in 0 ..< h:
        for x in 0 ..< w:
          let (dx, dy) = (float64(x) + 0.5 - p.center[0], float64(y) + 0.5 - p.center[1])
          let (u, v) = (c * dx + s * dy, -s * dx + c * dy)
          if abs(u) < p.width / 2 and abs(v) < p.height / 2:
            let t = float32(sin(u * 0.05) * cos(v * 0.07))
            result[y * w + x] = [0.3'f32 + 0.2'f32 * t, 0.2'f32 + 0.1'f32 * float32(k), 0.5'f32 - 0.2'f32 * t, 1]
  test "finds rotated photos in reading order":
    let truth = [FoundPhoto(center: [150.0, 120.0], width: 180, height: 120, angle: 7),
                 FoundPhoto(center: [450.0, 130.0], width: 160, height: 130, angle: -12),
                 FoundPhoto(center: [300.0, 330.0], width: 220, height: 110, angle: 3)]
    let (w, h) = (600, 440)
    let found = findPhotos(w, h, scan(w, h, truth))
    check found.len == 3
    if found.len == 3:
      for k in 0 .. 2:
        let (f, t) = (found[k], truth[k])
        check abs(f.angle - t.angle) < 1
        check abs(f.center[0] - t.center[0]) < 3 and abs(f.center[1] - t.center[1]) < 3
        check abs(f.width - t.width) < 6 and abs(f.height - t.height) < 6
      let c = found[0].corners
      check abs((c[0][0] + c[2][0]) / 2 - found[0].center[0]) < 1e-9
    check findPhotos(w, h, newSeqWith(w * h, [1'f32, 1, 1, 1])).len == 0
  test "tiny and mismatched scans find nothing":
    check findPhotos(4, 4, newSeq[array[4, float32]](16)).len == 0
    check findPhotos(100, 100, newSeq[array[4, float32]](10)).len == 0

suite "red eye":
  proc sample(w, h: int; f: proc(x, y: int): array[4, float32]): seq[array[4, float32]] =
    for y in 0 ..< h:
      for x in 0 ..< w: result.add f(x, y)
  proc inDisc(x, y: int; cx, cy, r: float32): bool = hypot(float32(x) - cx, float32(y) - cy) <= r
  proc portrait(x, y: int): array[4, float32] =
    if inDisc(x, y, 16, 16, 1.2): [1'f32, 1, 1, 1]
    elif inDisc(x, y, 16, 16, 10): [0.95'f32, 0.10, 0.10, 1]
    else: [0.38'f32, 0.28, 0.16, 1]
  proc luma(p: array[4, float32]): float32 = 0.2126'f32 * p[0] + 0.7152'f32 * p[1] + 0.0722'f32 * p[2]
  test "halo covers the erode and blur reach":
    check haloRadius() == 5
    check searchRadius(50) == 64 and searchRadius(0) == 8 and searchRadius(101) == searchRadius(100)
    check unclampedWindowSide(50).isSome and unclampedWindowSide(50).get < MaxWindow
    check unclampedWindowSide(10_000).isNone and unclampedWindowSide(NaN).isNone
  test "a red disc is neutralized and the iris is left alone":
    let w = 32
    var px = sample(w, w, portrait)
    let irisBefore = px[2 * w + 2]
    check redeye.apply(px, w, w, (16, 16), (0, 0), 50, 100) > 0
    let after = px[11 * w + 16]
    let neut = (0.10'f32 + 0.10'f32) / 2
    check abs(after[0] - after[1]) < 0.08 and abs(after[0] - after[2]) < 0.08
    check abs(after[0] - neut * 0.55'f32) < 0.08
    check luma(after) < luma([0.95'f32, 0.10, 0.10, 1]) - 0.05
    for c in 0 .. 3: check abs(px[2 * w + 2][c] - irisBefore[c]) < 1e-5
    let catchlight = px[16 * w + 16]
    check catchlight[0] >= 0.9 and catchlight[1] >= 0.9 and catchlight[2] >= 0.9
  test "gray pixels are not red eye":
    var px = newSeqWith(16 * 16, [0.4'f32, 0.4, 0.4, 1])
    check redeye.apply(px, 16, 16, (8, 8), (0, 0), 50, 50) == 0
    check px[8 * 16 + 8] == [0.4'f32, 0.4, 0.4, 1]
  test "tiles with the documented halo match an untiled run":
    let w = 128
    let src = sample(w, w, proc(x, y: int): array[4, float32] =
      if x < 64: [0.95'f32, 0.10, 0.10, 1] else: [0.38'f32, 0.28, 0.16, 1])
    var reference = src
    discard redeye.apply(reference, w, w, (60, 64), (0, 0), 50, 50)
    proc tiled(halo: int): seq[array[4, float32]] =
      result = src
      var y0 = 0
      while y0 < w:
        var x0 = 0
        while x0 < w:
          let (x1, y1) = (min(x0 + 64, w), min(y0 + 64, w))
          let (rx0, ry0, rx1, ry1) = (max(x0 - halo, 0), max(y0 - halo, 0), min(x1 + halo, w), min(y1 + halo, w))
          let (bw, bh) = (rx1 - rx0, ry1 - ry0)
          var buf = newSeq[array[4, float32]](bw * bh)
          for yy in 0 ..< bh:
            for xx in 0 ..< bw: buf[yy * bw + xx] = src[(ry0 + yy) * w + rx0 + xx]
          discard redeye.apply(buf, bw, bh, (60, 64), (rx0, ry0), 50, 50)
          for yy in y0 ..< y1:
            for xx in x0 ..< x1: result[yy * w + xx] = buf[(yy - ry0) * bw + xx - rx0]
          x0 += 64
        y0 += 64
    proc err(a, b: seq[array[4, float32]]): float32 =
      for i in 0 ..< a.len:
        for c in 0 .. 2: result = max(result, abs(a[i][c] - b[i][c]))
    let e = err(reference, tiled(haloRadius()))
    check e < 1e-4
    check err(reference, tiled(1)) > e + 1e-4
  test "empty or mismatched buffers change nothing":
    var empty: seq[array[4, float32]]
    check redeye.apply(empty, 0, 0, (0, 0), (0, 0), 50, 50) == 0
    var tiny = newSeqWith(4, [0.95'f32, 0.1, 0.1, 1])
    check redeye.apply(tiny, 8, 8, (1, 1), (0, 0), 50, 50) == 0
    check redeye.apply(tiny, -2, -2, (1, 1), (0, 0), 50, 50) == 0
