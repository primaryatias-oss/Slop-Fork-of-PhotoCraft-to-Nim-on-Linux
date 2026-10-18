## Smart selection (`segment/tests.rs`, `segment/sky.rs` tests): max-flow against a reference,
## grid cuts, GMMs, SLIC, GrabCut / Object Selection, Quick Selection (including Photoshop's
## measured behaviour), Select Subject, Focus Area, Sky, and no-crash checks.

import std/[unittest, options, math, deques]
import photocraft/geom
import photocraft/algo/[util, segment, grabcut, slic, quick, sky, subject, focus, matting, selection]

proc edmondsKarp(cap: var seq[seq[float64]]): float64 =
  ## Reference max flow (node 0 = s, n−1 = t).
  let n = cap.len
  while true:
    var prev = newSeq[int](n)
    for v in prev.mitems: v = -1
    prev[0] = 0
    var q = [0].toDeque
    while q.len > 0:
      let u = q.popFirst
      for v in 0 ..< n:
        if prev[v] == -1 and cap[u][v] > 1e-9:
          prev[v] = u
          q.addLast v
    if prev[n - 1] == -1: return result
    var b = F64Max
    var v = n - 1
    while v != 0:
      b = min(b, cap[prev[v]][v])
      v = prev[v]
    v = n - 1
    while v != 0:
      cap[prev[v]][v] -= b
      cap[v][prev[v]] += b
      v = prev[v]
    result += b

proc discScene(w, h: int; cx, cy, r: float32; seed: uint64): (RgbImage, seq[bool]) =
  ## A textured disc on a noisy background: (image, ground-truth mask).
  var rng = newSegRng(seed)
  var img = newRgbImage(w, h)
  var truth = newSeq[bool](w * h)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let (dx, dy) = (float32(x) + 0.5'f32 - cx, float32(y) + 0.5'f32 - cy)
      let inside = dx * dx + dy * dy <= r * r
      truth[y * w + x] = inside
      let n = 0.05'f32 * rng.normal
      var c: array[3, float32]
      if inside:
        let t = clamp(sin(float32(x + y) * 0.35'f32) * 0.5'f32 + 0.5'f32, 0'f32, 1'f32)
        c = [0.85'f32 + 0.05'f32 * t + n, 0.2'f32 + 0.3'f32 * t + n, 0.15'f32 + n]
      else: c = [0.25'f32 + n, 0.45'f32 + n, 0.7'f32 + n]
      for k in 0 .. 2: img.px[y * w + x][k] = clamp(c[k], 0'f32, 1'f32)
  (img, truth)

proc iou(region: Region; truth: seq[bool]; w, h: int): float32 =
  var (inter, uni) = (0, 0)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let a = region.at(x, y) >= 0.5'f32
      let b = truth[y * w + x]
      if a and b: inc inter
      if a or b: inc uni
  float32(inter) / float32(max(uni, 1))

proc twoRegions(w, h: int): RgbImage =
  var rng = newSegRng(5)
  result = newRgbImage(w, h)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let n = 0.04'f32 * rng.normal
      let t = (sin(float32(x) * 0.5'f32) * cos(float32(y) * 0.3'f32)) * 0.05'f32
      let c = (if x < 60: [0.8'f32 + t + n, 0.5'f32 + n, 0.3'f32 + t] else: [0.2'f32 + n, 0.35'f32 + t, 0.6'f32 + n])
      for k in 0 .. 2: result.px[y * w + x][k] = clamp(c[k], 0'f32, 1'f32)

const QsRed = [200'f32 / 255, 60'f32 / 255, 60'f32 / 255]
const QsBlue = [60'f32 / 255, 60'f32 / 255, 200'f32 / 255]

proc qsScene(w, h: int; bg, fg: array[3, float32]; r: (int, int, int, int)): RgbImage =
  result = newRgbImage(w, h)
  for y in 0 ..< h:
    for x in 0 ..< w: result.px[y * w + x] = (if x >= r[0] and x < r[2] and y >= r[1] and y < r[3]: fg else: bg)

proc qsSquare(w, side: int; fg, bg: array[3, float32]): RgbImage =
  let x0 = (w - side) div 2
  qsScene(w, w, bg, fg, (x0, x0, x0 + side, x0 + side))

proc qs(img: RgbImage; pts: seq[(float32, float32)]; size: float32): Region =
  quickSelect(imageSampler(img), rect(0, 0, img.w, img.h), pts, size).get

proc count(r: Region): int =
  for v in r.mask:
    if v >= 128: inc result

proc insideOutside(r: Region; q: (int, int, int, int)): (int, int) =
  for y in r.bbox.y0 ..< r.bbox.y1:
    for x in r.bbox.x0 ..< r.bbox.x1:
      if r.at(x, y) >= 0.5'f32:
        if x >= q[0] and x < q[2] and y >= q[1] and y < q[3]: inc result[0] else: inc result[1]

proc landscape(w, h: int): RgbImage =
  ## Blue gradient sky over textured ground, with a dark round tree crown on a trunk.
  let horizon = h * 3 div 5
  result = newRgbImage(w, h)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let (cx, cy, rr) = (float32(w) * 0.7'f32, float32(h) * 0.35'f32, float32(h) * 0.18'f32)
      let inCrown = sqrt((float32(x) - cx) * (float32(x) - cx) + (float32(y) - cy) * (float32(y) - cy)) < rr
      let inTrunk = abs(float32(x) - cx) < 3'f32 and float32(y) > cy and y < horizon
      let n = (float32((x * 7919 + y * 104_729) mod 97) / 97'f32 - 0.5'f32) * 0.25'f32
      result.px[y * w + x] =
        if inCrown: [0.1'f32 + n * 0.5'f32, 0.35'f32 + n, 0.1'f32 + n * 0.5'f32]
        elif inTrunk: [0.3'f32 + n * 0.3'f32, 0.2'f32, 0.1'f32]
        elif y < horizon:
          let t = float32(y) / float32(horizon)
          [0.25'f32 + 0.4'f32 * t, 0.5'f32 + 0.3'f32 * t, 0.9'f32 + 0.05'f32 * t]
        else: [0.35'f32 + n, 0.3'f32 + n, 0.15'f32 + n * 0.5'f32]

suite "max-flow":
  test "tiny graph":
    var g = initGraph(2, 1)
    g.addTweights(0, 3, 2)
    g.addTweights(1, 2, 3)
    g.addEdge(0, 1, 1, 0)
    check g.maxflow == 5.0

  test "CLRS network":
    var g = initGraph(4, 5)
    g.addTweights(0, 16, 0)
    g.addTweights(1, 13, 0)
    g.addTweights(2, 0, 20)
    g.addTweights(3, 0, 4)
    g.addEdge(0, 1, 10, 4)
    g.addEdge(0, 2, 12, 0)
    g.addEdge(2, 1, 9, 0)
    g.addEdge(1, 3, 14, 0)
    g.addEdge(3, 2, 7, 0)
    check g.maxflow == 23.0
    check g.inSource(0) and g.inSource(1) and g.inSource(3)
    check not g.inSource(2)
    check g.residualOutOfSourceSide == 0'f32

  test "can be cancelled and otherwise cuts the same":
    for (w, h) in [(120, 100), (20, 15)]:
      var g = initGraph(w * h, w * h * 2)
      var z = 7'u64
      proc next(): float32 =
        z = z * 6364136223846793005'u64 + 1442695040888963407'u64
        float32(z shr 40) / float32(1'u64 shl 24)
      for i in 0 ..< w * h:
        let (a, b) = (next() * 2, next() * 2)
        g.addTweights(i, a, b)
        if i mod w + 1 < w:
          let (c, d) = (next(), next())
          g.addEdge(i, i + 1, c, d)
        if i + w < w * h:
          let (c, d) = (next(), next())
          g.addEdge(i, i + w, c, d)
      var (plain, asked) = (g, g)
      let flow = plain.maxflow
      check asked.maxflowWith(proc(): bool = false) == some(flow)
      for i in 0 ..< w * h: check plain.inSource(i) == asked.inSource(i)
      if w * h > 4096: check g.maxflowWith(proc(): bool = true).isNone

  test "matches the reference on random graphs":
    var rng = newSegRng(7)
    for trial in 0 ..< 60:
      let n = 3 + trial mod 12
      var g = initGraph(n, n * 3)
      var cap = newSeq[seq[float64]](n + 2)
      for k in 0 ..< cap.len: cap[k] = newSeq[float64](n + 2)
      var edges: seq[(int, int, float32, float32)]
      for i in 0 ..< n:
        var (s, t) = (round(rng.f32 * 10), round(rng.f32 * 10))
        if rng.f32 < 0.4'f32: s = 0
        if rng.f32 < 0.4'f32: t = 0
        g.addTweights(i, s, t)
        cap[0][i + 1] += float64(s)
        cap[i + 1][n + 1] += float64(t)
      for i in 0 ..< n:
        for j in i + 1 ..< n:
          if rng.f32 < 0.35'f32:
            let (a, b) = (round(rng.f32 * 8), round(rng.f32 * 8))
            g.addEdge(i, j, a, b)
            cap[i + 1][j + 1] += float64(a)
            cap[j + 1][i + 1] += float64(b)
            edges.add (i, j, a, b)
      let orig = cap
      let reference = edmondsKarp(cap)
      check abs(g.maxflow - reference) < 1e-3
      var cut = 0.0
      for i in 0 ..< n: cut += (if g.inSource(i): orig[i + 1][n + 1] else: orig[0][i + 1])
      for (i, j, a, b) in edges:
        if g.inSource(i) and not g.inSource(j): cut += float64(a)
        if g.inSource(j) and not g.inSource(i): cut += float64(b)
      check abs(cut - reference) < 1e-3

  test "push/relabel and Boykov–Kolmogorov agree":
    var g = initGraph(5000, 15000)
    var rng = newSegRng(9)
    for i in 0 ..< 5000:
      let (s, t) = (rng.f32 * 2 - 0.6, rng.f32 * 2 - 0.6)
      g.addTweights(i, rmax(s, 0'f32), rmax(t, 0'f32))
    for i in 0 ..< 5000:
      for _ in 0 ..< 3:
        let j = int(rng.nextU64 mod 5000)
        if j != i:
          let (a, b) = (rng.f32, rng.f32)
          g.addEdge(i, j, a, b)
    var (pr, bk) = (g, g)
    check abs(pr.maxflowPushRelabel - bk.maxflowBk) < 1e-2

suite "grid cut, GMM, SLIC":
  test "grid cut separates two halves":
    let img = rgbFromFn(20, 10, proc(x, y: int): array[3, float32] = (if x < 10: [0.9'f32, 0.1, 0.1] else: [0.1'f32, 0.1, 0.9]))
    var fixed = newSeq[uint8](200)
    fixed[5 * 20 + 1] = HardFg
    fixed[5 * 20 + 18] = HardBg
    let zeros = newSeq[float32](200)
    let cut = gridCut(img, zeros, zeros, fixed, 50, contrastBeta(img))
    for y in 0 ..< 10:
      for x in 0 ..< 20: check cut[y * 20 + x] == (x < 10)

  test "the optimized cut is the reference cut":
    let (img, _) = discScene(90, 70, 45, 35, 20, 3)
    var fixed = newSeq[uint8](90 * 70)
    for i in 0 ..< fixed.len:
      if i mod 90 < 3: fixed[i] = HardBg
      elif i mod 90 in 40 .. 50 and i div 90 in 30 .. 40: fixed[i] = HardFg
    var cf, cb = newSeq[float32](fixed.len)
    for i, p in img.px:
      cf[i] = 1 - p[0]
      cb[i] = p[0]
    let wf = proc(d2: float32): float32 = 2'f32 * exp(-d2 * 30'f32)
    check gridCutWith(img, cf, cb, fixed, wf) == gridCutReference(img, cf, cb, fixed, wf)

  test "GMM recovers synthetic clusters":
    var rng = newSegRng(3)
    let truth = [([0.2'f32, 0.3, 0.8], 0.03'f32, 0.5'f32), ([0.8'f32, 0.2, 0.1], 0.05'f32, 0.3'f32), ([0.5'f32, 0.9, 0.4], 0.02'f32, 0.2'f32)]
    var samples: seq[array[3, float32]]
    for (mean, sd, w) in truth:
      for _ in 0 ..< int(w * 6000):
        var s: array[3, float32]
        for k in 0 .. 2: s[k] = mean[k] + sd * rng.normal
        samples.add s
    let g = fit(samples, 3, DefaultReg).get
    check g.comps.len == 3
    for (mean, sd, w) in truth:
      var best = 0
      for i in 1 ..< g.comps.len:
        if d2(g.comps[i].mean, mean) < d2(g.comps[best].mean, mean): best = i
      let c = g.comps[best]
      check sqrt(d2(c.mean, mean)) < 0.01
      check abs(c.weight - w) < 0.03
      check abs(sqrt(c.cov[0][0]) - sd) < 0.01
    check g.negLog([0.2'f32, 0.3, 0.8]) < g.negLog([0.5'f32, 0.5, 0.5])
    check g.refit(samples).get.comps.len == 3

  test "SLIC counts, compactness and edges":
    var rng = newSegRng(11)
    let (w, h) = (120, 120)
    var img = rgbFromFn(w, h, proc(x, y: int): array[3, float32] = (if x < 60: [0.8'f32, 0.3, 0.2] else: [0.2'f32, 0.5, 0.8]))
    for p in img.px.mitems:
      for k in 0 .. 2: p[k] = clamp(p[k] + 0.02'f32 * rng.normal, 0'f32, 1'f32)
    let sp = slic(img, 64, 10, 10)
    check sp.count in 40 .. 90
    var bbox = newSeq[(int, int, int, int)](sp.count)
    for b in bbox.mitems: b = (high(int), high(int), 0, 0)
    var left, total = newSeq[int](sp.count)
    for y in 0 ..< h:
      for x in 0 ..< w:
        let l = int(sp.labels[y * w + x])
        bbox[l] = (min(bbox[l][0], x), min(bbox[l][1], y), max(bbox[l][2], x), max(bbox[l][3], y))
        inc total[l]
        if x < 60: inc left[l]
    let s = sp.step
    for l, b in bbox:
      check total[l] > 0
      check float32(b[2] - b[0] + 1) <= 3'f32 * s and float32(b[3] - b[1] + 1) <= 3'f32 * s
      check float32(max(left[l], total[l] - left[l])) / float32(total[l]) > 0.95'f32

  test "the sampler's downsampling matches the image's":
    let img = rgbFromFn(37, 29, proc(x, y: int): array[3, float32] = [float32(x) / 37, float32(y) / 29, float32((x * y) mod 7) / 7])
    let a = imageSampler(img, (5, 3)).rgbScaled(rect(5, 3, 42, 32), 4)
    let b = img.downsample(4)
    check (a.w, a.h) == (b.w, b.h)
    for i in 0 ..< a.px.len:
      for c in 0 .. 2: check abs(a.px[i][c] - b.px[i][c]) < 1e-5

  test "clean mask drops islands and fills holes":
    let (w, h) = (40, 40)
    var m = newSeq[bool](w * h)
    for y in 5 ..< 30:
      for x in 5 ..< 30: m[y * w + x] = true
    m[15 * w + 15] = false
    m[36 * w + 36] = true
    let c = cleanMask(m, w, h, 0.1, 0.05)
    check c[15 * w + 15]
    check not c[36 * w + 36]

suite "GrabCut and Object Selection":
  test "GrabCut trimap on a disc":
    let (img, truth) = discScene(120, 100, 60, 50, 30, 1)
    var tri = newSeq[uint8](img.w * img.h)
    for y in 12 ..< 88:
      for x in 22 ..< 98: tri[y * img.w + x] = PrFg
    check grabcut(img, tri, 8).isSome
    var (inter, uni) = (0, 0)
    for i, t in tri:
      let a = t == PrFg or t == Fg
      if a and truth[i]: inc inter
      if a or truth[i]: inc uni
    check float32(inter) / float32(uni) > 0.95

  test "Object Selection at full resolution":
    let (img, truth) = discScene(200, 160, 100, 80, 50, 2)
    let reg = objectSelect(imageSampler(img), rect(0, 0, 200, 160), rect(40, 20, 160, 140), 1_000_000).get
    check iou(reg, truth, 200, 160) > 0.95

  test "Object Selection downsampled with band refinement":
    let (img, truth) = discScene(640, 480, 330, 250, 150, 3)
    let reg = objectSelect(imageSampler(img), rect(0, 0, 640, 480), rect(160, 80, 500, 420), 20_000).get
    check iou(reg, truth, 640, 480) > 0.95
    var farWrong = 0
    for y in 0 ..< 480:
      for x in 0 ..< 640:
        let (dx, dy) = (float32(x) + 0.5'f32 - 330, float32(y) + 0.5'f32 - 250)
        let d = sqrt(dx * dx + dy * dy) - 150
        if abs(d) > 2 and (reg.at(x, y) >= 0.5'f32) != truth[y * 640 + x]: inc farWrong
    check farWrong < 300

suite "Quick Selection":
  test "points far off the canvas select nothing":
    let img = twoRegions(140, 100)
    for p in [(3e9'f32, 0'f32), (0'f32, 3e9'f32), (F32Max, F32Max)]:
      check quickSelect(imageSampler(img), rect(0, 0, 140, 100), @[p], 8).isNone

  test "grows without leaking":
    let img = twoRegions(140, 100)
    let reg = quickSelect(imageSampler(img), rect(0, 0, 140, 100), @[(20'f32, 50'f32), (35'f32, 50'f32)], 8).get
    var inside = 0
    for y in 0 ..< 100:
      for x in 0 ..< 140:
        let v = reg.at(x, y)
        if x >= 62: check v < 0.5
        if x < 58 and v >= 0.5: inc inside
    check float32(inside) > 0.97'f32 * 58 * 100

  test "stops at a thin line":
    var rng = newSegRng(9)
    var img = newRgbImage(120, 80)
    for y in 0 ..< 80:
      for x in 0 ..< 120:
        let n = 0.03'f32 * rng.normal
        img.px[y * 120 + x] = (if x == 60 or x == 61: [0.05'f32, 0.05, 0.05] else: [0.6'f32 + n, 0.6'f32 + n, 0.55'f32 + n])
    let reg = quickSelect(imageSampler(img), rect(0, 0, 120, 80), @[(30'f32, 40'f32)], 6).get
    check reg.at(10, 10) >= 0.5 and reg.at(50, 70) >= 0.5
    for y in 0 ..< 80:
      for x in 63 ..< 120: check reg.at(x, y) < 0.5

  test "a drag selects around itself, not a big object":
    let (img, truth) = discScene(600, 500, 300, 250, 180, 4)
    let reg = quickSelect(imageSampler(img), rect(0, 0, 600, 500), @[(260'f32, 250'f32), (340'f32, 250'f32)], 30).get
    check count(reg) in 3000 ..< 9000
    check reg.at(300, 250) >= 0.5 and reg.at(262, 250) >= 0.5
    check iou(reg, truth, 600, 500) < 0.2

  test "works at full or exactly half resolution":
    check workingStep(512, 512) == 1
    check workingStep(513, 512) == 2
    check workingStep(520, 300) == 1
    check workingStep(900, 300) == 2
    check workingStep(5000, 3500) == 2
    let r = qs(qsSquare(640, 200, QsRed, QsBlue), @[(320'f32, 320'f32)], 30)
    var row: seq[uint8]
    for x in 216 ..< 223: row.add asU8(round(r.at(x, 320) * 255))
    check row == @[0'u8, 0, 0, 64, 191, 255, 255]

  test "a click in a flat area selects about the brush":
    check count(qs(qsScene(400, 400, QsRed, QsRed, (0, 0, 0, 0)), @[(200'f32, 200'f32)], 30)) in 700 ..< 1600

  test "a click takes an object only while it is small enough":
    var (i, o) = insideOutside(qs(qsSquare(500, 220, QsRed, QsBlue), @[(250'f32, 250'f32)], 30), (140, 140, 360, 360))
    check i >= 220 * 220 - 10 and o == 0
    check count(qs(qsSquare(500, 240, QsRed, QsBlue), @[(250'f32, 250'f32)], 30)) < 2000
    let (lo, hi) = ([125'f32 / 255, 110'f32 / 255, 110'f32 / 255], [110'f32 / 255, 110'f32 / 255, 125'f32 / 255])
    (i, o) = insideOutside(qs(qsSquare(500, 94, lo, hi), @[(250'f32, 250'f32)], 30), (203, 203, 297, 297))
    check i >= 94 * 94 - 10 and o == 0
    check count(qs(qsSquare(500, 120, lo, hi), @[(250'f32, 250'f32)], 30)) < 2000

  test "a narrow bar end to end, but only the end near it":
    let img = qsScene(500, 500, QsBlue, QsRed, (100, 50, 180, 450))
    var (i, o) = insideOutside(qs(img, @[(140'f32, 250'f32)], 30), (100, 50, 180, 450))
    check i >= 80 * 400 - 10 and o == 0
    (i, o) = insideOutside(qs(img, @[(140'f32, 420'f32)], 30), (100, 380, 180, 450))
    check i > 2500 and o == 0

  test "a short drag takes what a click does not":
    let img = qsSquare(500, 300, QsRed, QsBlue)
    check count(qs(img, @[(250'f32, 250'f32)], 30)) < 2000
    let (i, o) = insideOutside(qs(img, @[(220'f32, 250'f32), (280'f32, 250'f32)], 30), (100, 100, 400, 400))
    check i >= 300 * 300 - 10 and o == 0

  test "on a large document the window grows to the object":
    let img = qsScene(1000, 600, QsBlue, QsRed, (20, 260, 980, 340))
    let (i, o) = insideOutside(qs(img, @[(500.5'f32, 300.5'f32)], 30), (20, 260, 980, 340))
    check i >= 960 * 80 * 98 div 100 and o < 2000

suite "Subject, Focus Area, Sky":
  test "Select Subject finds a centred object":
    let (img, truth) = discScene(200, 150, 100, 75, 40, 6)
    check iou(selectSubject(imageSampler(img), rect(0, 0, 200, 150)).get, truth, 200, 150) > 0.9

  test "saliency highlights the object":
    let (img, truth) = discScene(120, 90, 60, 45, 22, 8)
    let sal = saliency(img)
    var (fi, fn, bi, bn) = (0'f32, 0'f32, 0'f32, 0'f32)
    for i, s in sal:
      if truth[i]:
        fi += s
        fn += 1
      else:
        bi += s
        bn += 1
    check fi / fn > 0.6 and bi / bn < 0.25

  test "Focus Area selects the sharp half":
    var rng = newSegRng(12)
    let (w, h) = (160, 100)
    var noise = newSeq[float32](w * h)
    for v in noise.mitems: v = rng.f32
    let blurred = matting.boxMean(noise, w, h, 4)
    var img = newRgbImage(w, h)
    for y in 0 ..< h:
      for x in 0 ..< w:
        let v = (if x < 80: noise[y * w + x] else: blurred[y * w + x])
        img.px[y * w + x] = [v, v * 0.8'f32, 0.3'f32]
    let reg = focusArea(imageSampler(img), rect(0, 0, w, h), 0.5, 0).get
    var (l, r) = (0, 0)
    for y in 0 ..< h:
      for x in 0 ..< w:
        if reg.at(x, y) >= 0.5:
          if x < 80: inc l else: inc r
    check float32(l) > 0.85'f32 * 80 * float32(h)
    check float32(r) < 0.1'f32 * 80 * float32(h)

  test "finds the sky and not the ground or the tree":
    let img = landscape(160, 100)
    let r = selectSky(imageSampler(img), rect(0, 0, 160, 100), defaultSkyParams()).get
    check r.at(10, 5) > 0.9
    check r.at(80, 50) > 0.9
    check r.at(10, 80) < 0.1
    check r.at(112, 35) < 0.1
    check r.bbox.y1 <= 64

  test "no sky in a ground-only image":
    var img = newRgbImage(64, 64)
    for y in 0 ..< 64:
      for x in 0 ..< 64:
        let n = (float32((x * 7919 + y * 104_729) mod 97) / 97'f32 - 0.5'f32) * 0.3'f32
        img.px[y * 64 + x] = [0.35'f32 + n, 0.3'f32 + n, 0.12'f32]
    check selectSky(imageSampler(img), rect(0, 0, 64, 64), defaultSkyParams()).isNone

  test "an overcast sky is found":
    let img = rgbFromFn(64, 64, proc(x, y: int): array[3, float32] = (if y < 30: [0.85'f32, 0.86, 0.88] else: [0.2'f32, 0.3, 0.1]))
    let r = selectSky(imageSampler(img), rect(0, 0, 64, 64), defaultSkyParams()).get
    check r.at(30, 10) > 0.9 and r.at(30, 50) < 0.1

  test "the colour prior ranks sky over foliage":
    check colourPrior([0.4'f32, 0.6, 0.95]) > 0.8
    check colourPrior([0.15'f32, 0.4, 0.15]) < 0.1
    check colourPrior([0.9'f32, 0.9, 0.92]) > 0.8

suite "hostile input":
  test "segmentation does not crash":
    let empty = newRgbImage(0, 0)
    check gridCut(empty, @[], @[], @[], 50, 1).len == 0
    check gridCut(newRgbImage(5, 5), @[1'f32], @[1'f32], @[0'u8], 50, 1).len == 0
    check fit(newSeq[array[3, float32]](), 3, DefaultReg).isNone
    discard fit(@[[NaN.float32, 0, 0], [Inf.float32, 1, 1]], 4, -1)
    check slic(empty, 10, 10, 10).count == 0
    discard slic(rgbFromFn(3, 2, proc(x, y: int): array[3, float32] = [NaN.float32, 0, 1]), 0, NaN, 0)
    var tri = newSeq[uint8](4)
    check grabcut(newRgbImage(2, 2), tri, 3).isNone
    let img = twoRegions(30, 20)
    check objectSelect(imageSampler(img), rect(0, 0, 30, 20), rect(5, 5, 6, 6), 1000).isNone
    discard quickSelect(imageSampler(img), rect(0, 0, 30, 20), @[(NaN.float32, NaN.float32)], NaN)
    discard quickSelect(imageSampler(img), rect(0, 0, 30, 20), @[(5'f32, 5'f32)], 1e30)
    check selectSky(imageSampler(img), rect(0, 0, 2, 2), defaultSkyParams()).isNone
    check selectSubject(imageSampler(img), rect(0, 0, 3, 30)).isNone
    discard focusArea(imageSampler(img), rect(0, 0, 30, 20), NaN, NaN)
    check otsu(@[]) > 0
    check cleanMask(newSeq[bool](4), 2, 2, 0.5, 0.5) == newSeq[bool](4)
