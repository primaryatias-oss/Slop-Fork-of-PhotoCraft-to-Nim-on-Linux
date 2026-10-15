## Content-Aware Scale (seam carving) and the Magnetic Lasso (ports of the Rust unit tests). Bit
## parity with Rust is checked by `tools/algocheck.py` (see PORTING.md).

import std/[unittest, options, math, sequtils]
import photocraft/[geom, raster]
import photocraft/algo/[seam, magnetic]

proc noise(len: int; levels, seed: uint32): seq[float32] =
  ## Noise quantised to `levels` values, so the energy map is full of exact ties.
  var s = seed or 1
  for _ in 0 ..< len:
    s = s xor (s shl 13)
    s = s xor (s shr 17)
    s = s xor (s shl 5)
    result.add float32(s mod levels) / float32(levels - 1)

proc fnv(v: seq[float32]): uint64 =
  result = 0xcbf2_9ce4_8422_2325'u64
  for f in v:
    let b = cast[uint32](f)
    for k in 0 .. 3: result = (result xor ((b shr (8 * k)) and 0xFF)) * 0x100_0000_01b3'u64

proc bar(w, h: int): seq[float32] =
  ## A flat grey image with a salient vertical bar at x = 10..14 (red/blue columns).
  result = newSeqWith(w * h * 3, 0.5'f32)
  for y in 0 ..< h:
    for x in 10 ..< 14:
      let c = (if x mod 2 == 0: [1'f32, 0, 0] else: [0'f32, 0, 1])
      for k in 0 .. 2: result[(y * w + x) * 3 + k] = c[k]

proc redColumns(w, h: int; img: seq[float32]): int =
  ## Columns that are pure red or pure blue top to bottom.
  for x in 0 ..< w:
    var all = true
    for y in 0 ..< h:
      let i = (y * w + x) * 3
      let (r, g, b) = (img[i], img[i + 1], img[i + 2])
      if not (g < 0.05 and ((r > 0.95 and b < 0.05) or (b > 0.95 and r < 0.05))): all = false
    if all: inc result

const NoProt = none(seq[float32])

suite "content-aware scale":
  test "output bits are pinned (the Rust build's hashes)":
    let cases = [(61, 47, 3, 2'u32, false, 40, 47), (61, 47, 3, 255'u32, false, 61, 30), (61, 47, 4, 4'u32, true, 35, 29),
                 (33, 20, 1, 3'u32, false, 80, 20), (33, 20, 3, 1000'u32, true, 50, 45), (48, 36, 4, 7'u32, false, 70, 18),
                 (5, 3, 1, 2'u32, false, 1, 1), (40, 40, 3, 2'u32, true, 39, 41)]
    const Pinned = [0x24ea_9353_02c2_7c58'u64, 0x5a6e_8554_a0cb_ebb1'u64, 0x8b18_d63f_396e_21c9'u64, 0x1db2_23ad_3e6a_a7c5'u64,
                    0x721d_6cc5_34e0_3fdf'u64, 0x961a_ea6d_e512_ec7d'u64, 0x4d25_767f_9dce_13f5'u64, 0x2157_9f86_27d6_b835'u64]
    for i, (w, h, ch, levels, prot, nw, nh) in cases:
      let img = noise(w * h * ch, levels, 7'u32 + uint32(i))
      let protect = (if prot: some(noise(w * h, 3, 99'u32 + uint32(i))) else: NoProt)
      let o = carve(w, h, ch, img, protect, nw, nh)
      check o.len == nw * nh * ch
      check fnv(o) == Pinned[i]
  test "the seam follows low energy":
    var e = newSeqWith(5 * 4, 9'f32)
    for y in 0 ..< 4: e[y * 5 + 2] = 0
    check findSeam(5, 4, e) == @[2, 2, 2, 2]
  test "shrinking keeps the salient bar; plain resampling squashes it":
    let (w, h) = (40, 12)
    let img = bar(w, h)
    let o = carve(w, h, 3, img, NoProt, 24, h)
    check o.len == 24 * h * 3 and redColumns(24, h, o) == 4
    check redColumns(24, h, resizeBilinear(w, h, 3, img, 24, h)) < 4
  test "the tall proxy shrink keeps full-resolution salient and protected pixels":
    let (w, h, ch, nw) = (512, 2048, 3, 384)
    var img = bar(w, h)
    var protect = newSeq[float32](w * h)
    for y in 0 ..< h:
      let i = (y * w + 256) * ch
      img[i] = 0.03
      img[i + 1] = 0.87
      img[i + 2] = 0.42
      protect[y * w + 256] = 1
    let o = carveWidthWith(w, h, ch, img, some(protect), nw, NoInterrupt)
    check o.len == nw * h * ch and redColumns(nw, h, o) == 4
    var ok = true
    for y in 0 ..< h:
      var found = false
      for x in 0 ..< nw:
        let i = (y * nw + x) * ch
        if o[i] == 0.03'f32 and o[i + 1] == 0.87'f32 and o[i + 2] == 0.42'f32: found = true
      ok = ok and found
    check ok
  test "cancellation":
    let img = newSeqWith(40 * 20 * 3, 0.5'f32)
    let yes = newInterrupt(proc(): bool = true)
    expect Cancelled: discard carveWith(40, 20, 3, img, NoProt, 30, 15, yes)
    check carveWith(40, 20, 3, img, NoProt, 30, 15, NoInterrupt) == carve(40, 20, 3, img, NoProt, 30, 15)
    expect Cancelled: discard carveWidthWith(128, 2048, 3, newSeqWith(128 * 2048 * 3, 0.5'f32), NoProt, 64, yes)
  test "enlarging duplicates flat areas":
    let o = carve(30, 8, 3, bar(30, 8), NoProt, 50, 8)
    check o.len == 50 * 8 * 3 and redColumns(50, 8, o) == 4
  test "height and protection":
    let (w, h) = (20, 30)
    var img = newSeqWith(w * h, 0.2'f32)
    for x in 0 ..< w: img[12 * w + x] = 1
    let o = carve(w, h, 1, img, NoProt, w, 18)
    check o.len == w * 18
    var rows = 0
    for y in 0 ..< 18:
      if (0 ..< w).toSeq.allIt(o[y * w + it] > 0.9): inc rows
    check rows == 1
    var protect = newSeq[float32](16 * 4)
    var marked = newSeqWith(16 * 4, 0.5'f32)
    for y in 0 ..< 4:
      protect[y * 16 + 3] = 1
      marked[y * 16 + 3] = 0.51
    let p = carve(16, 4, 1, marked, some(protect), 8, 4)
    for y in 0 ..< 4: check p[y * 8 ..< y * 8 + 8].anyIt(abs(it - 0.51'f32) < 1e-6)
  test "one-pixel lines enlarge by repeating":
    let col = @[0.1'f32, 0.2, 0.3, 0.4, 0.5]
    var rep4: seq[float32]
    for v in col: rep4.add [v, v, v, v]
    check carve(1, 5, 1, col, NoProt, 4, 5) == rep4
    check carve(5, 1, 1, col, NoProt, 5, 3) == col & col & col
    check carve(1, 1, 3, @[0.7'f32, 0.8, 0.9], NoProt, 3, 2) == cycle(@[0.7'f32, 0.8, 0.9], 6)
    for (w, h, nw, nh) in [(1, 5, 6, 10), (1, 17, 6, 34), (16, 1, 21, 2), (7, 1, 3, 8), (2, 1, 1, 8)]:
      check carve(w, h, 4, newSeqWith(w * h * 4, 0.5'f32), some(newSeqWith(w * h, 1'f32)), nw, nh).len == nw * nh * 4
  test "skin rule; malformed buffers are returned unchanged":
    check skinMask(2, 1, 3, [0.86'f32, 0.62, 0.5, 0.2, 0.4, 0.8]) == @[1'f32, 0]
    check carve(10, 10, 3, @[0.5'f32], NoProt, 5, 5) == @[0.5'f32]
    check energy(4, 4, 1, @[1'f32]).len == 0

# ---------------------------------------------------------------- magnetic lasso

proc image(f: proc(x, y: int): array[4, uint8]; read: ref int): Fetch =
  ## A procedural image as a fetch that counts the pixels it reads.
  result = proc(r: Rect): seq[array[4, uint8]] =
    read[] += r.width * r.height
    for y in r.y0 ..< r.y1:
      for x in r.x0 ..< r.x1: result.add f(x, y)

proc grey(v: uint8): array[4, uint8] = [v, v, v, 255'u8]

proc dist2Seg(p, a, b: array[2, float64]): float64 =
  let (vx, vy) = (b[0] - a[0], b[1] - a[1])
  let l2 = vx * vx + vy * vy
  let t = (if l2 > 0: clamp(((p[0] - a[0]) * vx + (p[1] - a[1]) * vy) / l2, 0.0, 1.0) else: 0.0)
  let (dx, dy) = (p[0] - (a[0] + t * vx), p[1] - (a[1] + t * vy))
  dx * dx + dy * dy

proc offPolyline(p: array[2, float64]; g: openArray[array[2, float64]]): float64 =
  result = Inf
  for k in 1 ..< g.len: result = min(result, sqrt(dist2Seg(p, g[k - 1], g[k])))

suite "magnetic lasso":
  test "snaps to the edge centre within the width":
    let read = new int
    let f = image(proc(x, y: int): array[4, uint8] = grey(if x < 20: 0 else: 255), read)
    var t = newTracer(rect(0, 0, 64, 32))
    let p = t.snap(f, [14.3, 10.5], newSettings(10, 0.1)).get
    check abs(p[0] - 20) < 0.01 and abs(p[1] - 10.5) < 0.01
    check abs(t.snap(f, [27.0, 3.2], newSettings(10, 0.1)).get[0] - 20) < 0.01
    check t.snap(f, [14.3, 10.5], newSettings(3, 0.1)) == some([14.3, 10.5])
  test "contrast sets which steps are edges":
    let read = new int
    let f = image(proc(x, y: int): array[4, uint8] = grey(if x < 20: 100 else: 113), read)
    var t = newTracer(rect(0, 0, 64, 32))
    check t.snap(f, [14.3, 10.5], newSettings(10, 0.10)) == some([14.3, 10.5])
    check abs(t.snap(f, [14.3, 10.5], newSettings(10, 0.03)).get[0] - 20) < 0.01
  test "snaps to the most prominent edge":
    let read = new int
    let f = image(proc(x, y: int): array[4, uint8] = grey([0'u8, 40, 255][int(x >= 12) + int(x >= 18)]), read)
    var t = newTracer(rect(0, 0, 40, 20))
    check abs(t.snap(f, [13.0, 10.0], newSettings(10, 0.1)).get[0] - 18) < 0.01
  test "trace follows a curved edge":
    let read = new int
    let f = image(proc(x, y: int): array[4, uint8] =
      grey(if hypot(float64(x) + 0.5 - 50, float64(y) + 0.5 - 50) < 30: 255 else: 0), read)
    var t = newTracer(rect(0, 0, 100, 100))
    let s = defaultSettings()
    let a = t.snap(f, [50.0, 21.0], s).get
    let b = t.snap(f, [79.0, 50.0], s).get
    let path = t.trace(f, a, b, [], s)
    check path.len > 4 and path[0] == a and path[^1] == b
    for p in path: check abs(hypot(p[0] - 50, p[1] - 50) - 30) < 1
  test "without edges the border follows the pointer":
    let read = new int
    let f = image(proc(x, y: int): array[4, uint8] = grey(128), read)
    var t = newTracer(rect(0, 0, 120, 80))
    let s = defaultSettings()
    let (a, b) = ([10.0, 40.0], [110.0, 40.0])
    var guide: seq[array[2, float64]]
    for i in 1 .. 9: guide.add [10.0 + float64(i) * 10.0, 40.0 - 15.0 * sin(float64(i) * PI / 10.0)]
    let g = @[a] & guide & @[b]
    let path = t.trace(f, a, b, guide, s)
    for p in path: check offPolyline(p, g) < 2
    check offPolyline([60.0, 25.0], path) < 2
    let path2 = t.trace(f, [10.0, 10.0], [100.0, 75.0], [[30.0, 70.0]], s)
    for p in path2: check offPolyline(p, [[10.0, 10.0], [30.0, 70.0], [100.0, 75.0]]) <= s.width
    check t.trace(f, [10.0, 10.0], [110.0, 73.0], [], s) == @[[10.0, 10.0], [110.0, 73.0]]
  test "the corridor keeps the border off edges the pointer never went near":
    let read = new int
    let f = image(proc(x, y: int): array[4, uint8] = grey([60'u8, 100, 255][int(y >= 20) + int(y >= 40)]), read)
    var t = newTracer(rect(0, 0, 120, 60))
    let path = t.trace(f, [10.0, 22.0], [110.0, 18.0], [[60.0, 23.0]], newSettings(10, 0.1))
    for p in path:
      check abs(p[1] - 20) <= 2 + 1e-9
      if p[0] > 25 and p[0] < 95: check abs(p[1] - 20) < 0.01
  test "long guides are traced in pieces along the edge":
    let read = new int
    let f = image(proc(x, y: int): array[4, uint8] = grey(if y < 30: 20 else: 200), read)
    var t = newTracer(rect(0, 0, 1000, 60))
    var guide: seq[array[2, float64]]
    for i in 1 ..< 20: guide.add [float64(i) * 50, (if i mod 2 == 0: 26.0 else: 34.0)]
    let path = t.trace(f, [5.0, 30.0], [995.0, 30.0], guide, defaultSettings())
    for p in path: check abs(p[1] - 30) < 0.01
    check abs(length(path) - 990) < 1 and path.len < 20
  test "reads only the pixels around the border":
    let read = new int
    let f = image(proc(x, y: int): array[4, uint8] = grey(if x < 3000: 0 else: 255), read)
    var t = newTracer(rect(0, 0, 6000, 4000))
    let s = defaultSettings()
    let a = t.snap(f, [2996.0, 2000.0], s).get
    let b = t.snap(f, [3004.0, 2100.0], s).get
    let path = t.trace(f, a, b, [], s)
    for p in path: check abs(p[0] - 3000) < 0.01
    let first = read[]
    check first <= 6 * 128 * 128 and t.cachedPixels == first
    discard t.trace(f, a, b, [], s)
    check read[] == first
  test "hostile input never panics":
    let read = new int
    let f = image(proc(x, y: int): array[4, uint8] = grey(uint8((x xor y) and 255)), read)
    let s = defaultSettings()
    var empty = newTracer(EmptyRect)
    check empty.snap(f, [1.0, 1.0], s).isNone and empty.trace(f, [1.0, 1.0], [5.0, 5.0], [], s).len == 0
    var t = newTracer(rect(0, 0, 50, 40))
    check t.snap(f, [NaN, 1.0], s).isNone and t.trace(f, [Inf, 1.0], [5.0, 5.0], [], s).len == 0
    let path = t.trace(f, [-1e300, -1e9], [1e9, 1e300], [[NaN, 3.0], [1e12, -5.0]], s)
    for p in path: check p[0] >= 0 and p[0] <= 50 and p[1] >= 0 and p[1] <= 40
    for bad in [Settings(width: NaN, contrast: NaN), Settings(width: 1e300, contrast: -4), Settings(width: -3, contrast: 1e30)]:
      check t.snap(f, [20.0, 20.0], bad).isSome
      check t.trace(f, [2.0, 2.0], [45.0, 30.0], [[10.0, 35.0]], bad).len > 0
    let badFetch: Fetch = proc(r: Rect): seq[array[4, uint8]] = newSeqWith(3, [255'u8, 255, 255, 255])
    var t2 = newTracer(rect(0, 0, 300, 300))
    check t2.snap(badFetch, [150.0, 150.0], s) == some([150.0, 150.0])
    let p2 = t2.trace(badFetch, [10.0, 10.0], [290.0, 280.0], [], s)
    check p2[0] == [10.0, 10.0] and p2[^1] == [290.0, 280.0]
    check t2.trace(badFetch, [7.0, 7.0], [7.0, 7.0], [], s) == @[[7.0, 7.0], [7.0, 7.0]]
  test "simplify and length":
    check simplify([[0.0, 0.0], [1.0, 0.1], [2.0, 0.0], [3.0, 5.0]], 0.3) == @[[0.0, 0.0], [2.0, 0.0], [3.0, 5.0]]
    check abs(length([[0.0, 0.0], [600.0, 0.0], [600.0, 100.0]]) - 700) < 1e-9
