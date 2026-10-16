## Photomerge and Auto-Align: Harris/BRIEF features and model fitting (`features.rs` tests),
## panorama alignment, photometric correction, seams and blending (`panorama.rs` tests) and direct
## registration (`direct.rs` tests), plus no-crash checks on hostile input.

import std/[unittest, options, math, sequtils]
import photocraft/algo/[util, transform, features, panorama, direct]

# ---------------------------------------------------------------- features

proc ftTexture(w, h: int): seq[float32] =
  ## Deterministic textured test image: blobs of varying brightness.
  var rng = newFRng(42)
  result = newSeqWith(w * h, 0.3'f32)
  for _ in 0 ..< 140:
    let (cx, cy) = (rng.unit * float64(w), rng.unit * float64(h))
    let r = 3.0 + rng.unit * 9.0
    let v = float32(rng.unit)
    let sq = rng.unit > 0.5
    for y in 0 ..< h:
      for x in 0 ..< w:
        let (dx, dy) = (float64(x) - cx, float64(y) - cy)
        let inside = (if sq: abs(dx) < r and abs(dy) < r * 0.6 else: hypot(dx, dy) < r)
        if inside: result[y * w + x] = v

proc ftWarp(w, hgt: int; img: seq[float32]; inv: Homography): seq[float32] =
  ## Resample `img` (destination → source lookup with `inv`), bilinear, edge-clamped.
  result = newSeqWith(w * hgt, 0.3'f32)
  for y in 0 ..< hgt:
    for x in 0 ..< w:
      let (sx, sy) = inv.apply(float64(x), float64(y))
      if sx < 0.0 or sy < 0.0 or sx >= float64(w - 1) or sy >= float64(hgt - 1): continue
      let (x0, y0) = (int(floor(sx)), int(floor(sy)))
      let (tx, ty) = (float32(sx - float64(x0)), float32(sy - float64(y0)))
      template p(xx, yy: int): float32 = img[yy * w + xx]
      let top = p(x0, y0) + (p(x0 + 1, y0) - p(x0, y0)) * tx
      let bot = p(x0, y0 + 1) + (p(x0 + 1, y0 + 1) - p(x0, y0 + 1)) * tx
      result[y * w + x] = top + (bot - top) * ty

suite "features":
  test "harris finds square corners":
    let (w, h) = (80, 80)
    var img = newSeq[float32](w * h)
    for y in 30 ..< 50:
      for x in 30 ..< 50: img[y * w + x] = 1
    let c = harris(w, h, img, 10, 5'f32, none(seq[bool]))
    check c.len >= 4
    for (x, y) in [(30'f32, 30'f32), (49'f32, 49'f32)]:
      check c.anyIt(abs(it.x - x) <= 2 and abs(it.y - y) <= 2)

  test "fits are exact on clean data":
    let src = @[[0.0, 0.0], [10.0, 0.0], [10.0, 10.0], [0.0, 10.0], [5.0, 3.0]]
    let truth = Homography(m: [1.1, 0.05, 3.0, -0.02, 0.95, -2.0, 0.001, 0.0005, 1.0])
    var dst: seq[array[2, float64]]
    for p in src:
      let (x, y) = truth.apply(p[0], p[1])
      dst.add [x, y]
    let h = fit(mHomography, src, dst).get
    for p in src:
      let (a, b) = (h.apply(p[0], p[1]), truth.apply(p[0], p[1]))
      check abs(a[0] - b[0]) < 1e-6 and abs(a[1] - b[1]) < 1e-6
    let sim = Homography(m: [0.9, -0.2, 4.0, 0.2, 0.9, 1.0, 0.0, 0.0, 1.0])
    dst.setLen(0)
    for p in src:
      let (x, y) = sim.apply(p[0], p[1])
      dst.add [x, y]
    let hs = fit(mSimilarity, src, dst).get
    for i in 0 ..< 9: check abs(hs.m[i] - sim.m[i]) < 1e-9

  test "ransac rejects outliers":
    var src, dst: seq[array[2, float64]]
    for i in 0 ..< 40:
      let p = [float64(i * 7 mod 50), float64(i * 13 mod 50)]
      src.add p
      dst.add(if i mod 4 == 0: [p[0] * 3.0 + 40.0, -p[1]] else: [p[0] + 12.0, p[1] - 5.0])
    let (h, inl) = ransac(mTranslation, src, dst, 100, 1.0, 1).get
    check inl.len == 30
    check abs(h.m[2] - 12.0) < 1e-9 and abs(h.m[5] + 5.0) < 1e-9

  test "registers translation and similarity":
    let (w, h) = (192, 160)
    let reference = ftTexture(w, h)
    # The moving image is the reference shifted by (+9, −6): registration maps it back.
    let shift = Homography(m: [1.0, 0.0, 9.0, 0.0, 1.0, -6.0, 0.0, 0.0, 1.0])
    var moving = ftWarp(w, h, reference, shift.inverse.get)
    let (m, n) = features.register(w, h, reference, moving, none(seq[bool]), none(seq[bool]), mTranslation).get
    check n >= 6
    check abs(m.m[2] + 9.0) < 1.0 and abs(m.m[5] - 6.0) < 1.0
    # Rotated 4° and scaled 1.03 about the centre.
    let (c, s) = (cos(degToRad(4.0)) * 1.03, sin(degToRad(4.0)) * 1.03)
    let (cx, cy) = (float64(w) / 2.0, float64(h) / 2.0)
    let sim = Homography(m: [c, -s, cx - c * cx + s * cy, s, c, cy - s * cx - c * cy, 0.0, 0.0, 1.0])
    moving = ftWarp(w, h, reference, sim.inverse.get)
    let (m2, _) = features.register(w, h, reference, moving, none(seq[bool]), none(seq[bool]), mSimilarity).get
    for p in [[40.0, 40.0], [150.0, 120.0], [96.0, 80.0]]:
      let (x, y) = sim.apply(p[0], p[1])
      let (bx, by) = m2.apply(x, y)
      check abs(bx - p[0]) < 1.5 and abs(by - p[1]) < 1.5

  test "descriptors are deterministic":
    let (w, h) = (96, 96)
    let img = ftTexture(w, h)
    let c = harris(w, h, img, 50, 6'f32, none(seq[bool]))
    check describe(w, h, img, c, false) == describe(w, h, img, c, false)
    check hamming([0'u64, 0, 0, 0], [1'u64, 3, 0, high(uint64)]) == 67

  test "hostile input does not crash":
    let img = ftTexture(64, 64)
    check harris(64, 64, img, 50, 6'f32, some(newSeq[bool](3))).len == 0
    check harris(64, 640, img, 50, 6'f32, none(seq[bool])).len == 0
    check harris(-5, 64, img, 50, 6'f32, none(seq[bool])).len == 0
    check harris(64, 64, img, -1, NaN.float32, none(seq[bool])).len <= 1 # like Rust's max = 0
    let c = harris(64, 64, img, 50, 4'f32, none(seq[bool]))
    check describe(640, 64, img, c, false).len == c.len
    check describe(64, 64, img, @[Corner(x: 1e30, y: NaN, score: 1)], false).len == 1
    check matchDescriptors(@[], @[[1'u64, 2, 3, 4]], 0.8).len == 0
    let nanPts = @[[NaN, 1.0], [Inf, -Inf], [0.0, 0.0], [1.0, 1.0], [2.0, 5.0]]
    for m in [mTranslation, mSimilarity, mHomography]:
      discard fit(m, nanPts, nanPts)
      discard ransac(m, nanPts, nanPts[0 .. 2], 20, 1.0, 3)
      discard ransac(m, @[], @[], 20, 1.0, 3)
    check features.register(10, 10, img, img, none(seq[bool]), none(seq[bool]), mSimilarity).isNone

# ---------------------------------------------------------------- panorama

proc scene(w, h: int; seed: uint64): seq[float32] =
  ## Smooth random texture: blobs over a gradient (bounded detail so Harris finds corners).
  var rng = newRng(seed)
  result = newSeq[float32](w * h)
  for i in 0 ..< w * h: result[i] = 0.25'f32 + 0.2'f32 * (float32(i mod w) / float32(w))
  for _ in 0 ..< max(w * h div 900, 40):
    let cx = float64(rng.next mod uint64(w))
    let cy = float64(rng.next mod uint64(h))
    let r = 3.0 + float64(rng.next mod 9)
    let v = float32(rng.next mod 1000) / 1000'f32
    let sq = rng.next mod 2 == 0
    let (x0, x1) = (int(max(cx - r, 0.0)), min(int(cx + r) + 1, w))
    let (y0, y1) = (int(max(cy - r, 0.0)), min(int(cy + r) + 1, h))
    for y in y0 ..< y1:
      for x in x0 ..< x1:
        let (dx, dy) = (float64(x) - cx, float64(y) - cy)
        if (if sq: abs(dx) < r and abs(dy) < r * 0.6 else: hypot(dx, dy) < r): result[y * w + x] = v

proc view(sc: seq[float32]; sw, sh, tw, th: int; map: proc(x, y: float64): (float64, float64)): seq[float32] =
  ## Crops a `tw × th` view of a scene through `map` (view px → scene px).
  for i in 0 ..< tw * th:
    let (x, y) = map(float64(i mod tw), float64(i div tw))
    result.add(if x < 0.0 or y < 0.0 or x > float64(sw - 1) or y > float64(sh - 1): 0'f32
               else: bilinear(sc, sw, sh, 1, 0, float32(x), float32(y)))

suite "panorama":
  test "recovers translations and homographies":
    let (sw, sh) = (640, 300)
    let sc = scene(sw, sh, 3)
    let (tw, th) = (260, 220)
    let offs = [(20.0, 30.0), (190.0, 40.0), (360.0, 25.0)]
    var feats: seq[ImageFeatures]
    for (ox, oy) in offs:
      let (ox, oy) = (ox, oy)
      let im = view(sc, sw, sh, tw, th, proc(x, y: float64): (float64, float64) = (x + ox, y + oy))
      feats.add detect(tw, th, im, none(seq[bool]), 700)
    let matches = matchPairs(feats)
    check matches.len >= 2
    for layout in [lyReposition, lyCollage, lyPerspective]:
      let a = align(feats, matches, AlignOptions(layout: layout, reference: some(1))).get
      check a.reference == 1
      for i, p in a.placements:
        let (u, v) = p.get.forward(50.0, 60.0)
        let (eu, ev) = (50.0 + offs[i][0] - offs[1][0], 60.0 + offs[i][1] - offs[1][1])
        check abs(u - eu) < 1.0 and abs(v - ev) < 1.0
      check a.rms < 1.0
    # A projective view is recovered by the perspective layout.
    let hq = Homography(m: [1.05, 0.03, 180.0, -0.02, 1.0, 35.0, 0.0001, -0.00005, 1.0])
    let im2 = view(sc, sw, sh, tw, th, proc(x, y: float64): (float64, float64) = hq.apply(x, y))
    let feats2 = @[feats[0], detect(tw, th, im2, none(seq[bool]), 700)]
    let m2 = matchPairs(feats2)
    let a = align(feats2, m2, AlignOptions(layout: lyPerspective, reference: some(0))).get
    let p = a.placements[1].get
    for (x, y) in [(20.0, 30.0), (70.0, 180.0), (40.0, 100.0)]:
      let (sx, sy) = hq.apply(x, y)
      let (u, v) = p.forward(x, y)
      check abs(u - (sx - 20.0)) < 1.5 and abs(v - (sy - 30.0)) < 1.5

  test "rotating camera focal and cylindrical layout":
    let f = 300.0
    let (sw, sh) = (1400, 500)
    let sc = scene(sw, sh, 9)
    let (tw, th) = (320, 240)
    var feats: seq[ImageFeatures]
    for deg in [-25.0, 0.0, 25.0]:
      let deg = deg
      let im = view(sc, sw, sh, tw, th, proc(x, y: float64): (float64, float64) =
        let (dx, dy) = (x - float64(tw) / 2.0, y - float64(th) / 2.0)
        let t = arctan2(dx, f) + degToRad(deg)
        let hh = dy / hypot(dx, f)
        (float64(sw) / 2.0 + f * t, float64(sh) / 2.0 + f * hh))
      feats.add detect(tw, th, im, none(seq[bool]), 700)
    let matches = matchPairs(feats)
    check matches.countIt(it.b == it.a + 1) == 2
    var est: seq[float64]
    for m in matches:
      let e = focalFromHomography(m.homography, [160.0, 120.0], [160.0, 120.0])
      if e.isSome: est.add e.get
    check est.anyIt(abs(it - f) / f < 0.15)
    let a = align(feats, matches, AlignOptions(layout: lyCylindrical, reference: some(1))).get
    check abs(a.focal - f) / f < 0.1
    let p = a.placements[2].get
    let (u, _) = p.forward(160.0, 120.0)
    let expect = 160.0 + a.focal * degToRad(25.0)
    check abs(u - expect) < 4.0
    check a.rms < 1.5
    let (u2, v2) = p.forward(40.0, 200.0)
    let (x, y) = p.inverse(u2, v2).get
    check abs(x - 40.0) < 1e-6 and abs(y - 200.0) < 1e-6

  test "placement round trips all projections":
    for projection in [prPlane, prCylinder, prSphere]:
      let p = Placement(projection: projection, focal: 400.0, center: [200.0, 150.0], k1: 0.05,
                        h: Homography(m: [0.98, -0.1, 30.0, 0.1, 0.98, -12.0, 0.0, 0.0, 1.0]))
      for (x, y) in [(0.0, 0.0), (390.0, 20.0), (123.0, 280.0)]:
        let (u, v) = p.forward(x, y)
        let (bx, by) = p.inverse(u, v).get
        check abs(bx - x) < 1e-6 and abs(by - y) < 1e-6
      let s = p.scaled(2.0)
      let (u, v) = p.forward(100.0, 50.0)
      let (u2, v2) = s.forward(200.0, 100.0)
      check abs(u2 - 2.0 * u) < 1e-6 and abs(v2 - 2.0 * v) < 1e-6

  test "photometric recovers gain and vignette":
    let (w, h) = (200, 100)
    proc base(x, y: int): float32 = 0.3'f32 + 0.2'f32 * abs(sin(float32(x) * 0.05'f32) * cos(float32(y) * 0.07'f32))
    var imgs: seq[RoiImage]
    var pls: seq[Placement]
    for (x0, g) in [(0, 1.0'f32), (100, 1.3'f32)]:
      var px: seq[float32]
      for y in 0 ..< h:
        for x in 0 ..< w: px.add base(x + x0, y) * g
      imgs.add RoiImage(x0: x0, y0: 0, w: w, h: h, ch: 1, px: px, alpha: newSeqWith(w * h, 1'f32))
      pls.add Placement(projection: prPlane, focal: 200.0, center: [100.0, 50.0], k1: 0.0,
                        h: Homography(m: [1.0, 0.0, 100.0 + float64(x0), 0.0, 1.0, 50.0, 0.0, 0.0, 1.0]))
    let ph = photometric(imgs, some(pls), @[(w, h), (w, h)], true, false)
    check abs(ph.gains[1][0] / ph.gains[0][0] - 1.3'f32) < 0.03'f32
    let pv = photometric(imgs, some(pls), @[(w, h), (w, h)], true, true)
    check abs(pv.vignette[0]) < 0.1

  test "seams avoid disagreement and multiband is seamless":
    let (w, h) = (120, 40)
    var luma = @[newSeqWith(w * h, 0.5'f32), newSeqWith(w * h, 0.5'f32)]
    var cover = @[newSeq[bool](w * h), newSeq[bool](w * h)]
    for i in 0 ..< w * h:
      cover[0][i] = i mod w < 80
      cover[1][i] = i mod w >= 40
    for y in 0 ..< h:
      for x in 50 ..< 58: luma[1][y * w + x] = 1
    let labels = seamLabels(w, h, luma, cover, [0, 1])
    for y in 0 ..< h:
      let owner = toSeq(50 ..< 58).mapIt(labels[y * w + it])
      check owner.allIt(it == owner[0])
    check labels[5] == 0
    check labels[w - 1] == 1
    proc mk(x0: int; v: float32): RoiImage =
      RoiImage(x0: x0, y0: 0, w: 80, h: h, ch: 1, px: newSeqWith(80 * h, v), alpha: newSeqWith(80 * h, 1'f32))
    let imgs = @[mk(0, 0.3), mk(40, 0.7)]
    var wts: seq[seq[float32]]
    for k in 0 .. 1:
      var wt = newSeq[float32](80 * h)
      for i in 0 ..< 80 * h:
        let x = i mod 80 + imgs[k].x0
        wt[i] = (if (x < 60) == (k == 0): 1'f32 else: 0'f32)
      wts.add wt
    let (outp, cov) = multiband(w, h, imgs, wts, 4)
    let row = toSeq(0 ..< w).mapIt(outp[20 * w + it])
    check abs(row[2] - 0.3'f32) < 0.02'f32 and abs(row[w - 3] - 0.7'f32) < 0.02'f32
    var maxJump = 0'f32
    for i in 1 ..< row.len: maxJump = max(maxJump, abs(row[i] - row[i - 1]))
    check maxJump < 0.1'f32
    check cov.allIt(it == 1'f32)

  test "hostile input does not crash":
    let feats = @[detect(10, 10, newSeq[float32](3), none(seq[bool]), 100), ImageFeatures(w: 5, h: 5)]
    check matchPairs(feats).len == 0
    var lopsided = detect(64, 64, ftTexture(64, 64), none(seq[bool]), 100)
    lopsided.points.setLen(1)
    check matchPairs(@[lopsided, detect(64, 64, ftTexture(64, 64), none(seq[bool]), 100)]).len == 0
    check align(feats, @[PairMatch(a: 7, b: 9)], AlignOptions(layout: lyAuto)).isNone
    check align(@[], @[], AlignOptions(layout: lySpherical, focal: some(-1.0), reference: some(-3))).isNone
    discard focalFromHomography(Homography(m: [NaN, 0, 0, 0, 0, 0, 0, 0, 0]), [0.0, 0.0], [Inf, 0.0])
    let bad = RoiImage(x0: 3, y0: -2, w: 50, h: 40, ch: 3, px: newSeq[float32](7), alpha: newSeq[float32](2))
    discard photometric(@[bad, bad], some(newSeq[Placement]()), @[], true, true)
    check seamLabels(10, 10, @[newSeq[float32](3)], @[newSeq[bool](3)], [0, 5, -1]).allIt(it == -1)
    discard multiband(30, 20, @[bad], @[newSeq[float32](1)], 99)
    discard multiband(0, 0, @[], @[], 0)
    discard multiband(-3, 5, @[bad], @[newSeq[float32](1)], 4)
    discard multiband(30, 20, @[RoiImage(w: 2, h: 2, ch: -1)], @[newSeq[float32](4)], 4)
    discard photometric(@[RoiImage(w: 2, h: 2, ch: -1)], none(seq[Placement]), @[], true, true)
    check seamLabels(-4, 3, @[], @[], [0]).len == 0
    let p = Placement(projection: prSphere, focal: 0.0, center: [NaN, 0.0], k1: 1e300, h: Homography(m: [0.0, 0, 0, 0, 0, 0, 0, 0, 0]))
    discard p.forward(1.0, 2.0)
    discard p.inverse(Inf, NaN)
    discard p.bounds(-4.0, NaN)

# ---------------------------------------------------------------- direct

proc drTexture(w, h: int; seed: uint32): seq[float32] =
  ## A smooth random texture: a sum of a few sinusoids, so sub-pixel resampling is faithful.
  let s = float32(seed)
  for i in 0 ..< w * h:
    let (x, y) = (float32(i mod w), float32(i div w))
    let v = sin(x * 0.21'f32 + s) * cos(y * 0.17'f32 - s) + sin(x * 0.071'f32 + y * 0.093'f32 + s * 0.5'f32) * 0.7'f32 +
            cos((x * 0.33'f32 - y * 0.29'f32) * 1.3'f32 + s) * 0.4'f32
    result.add 0.5'f32 + 0.2'f32 * v

proc plane(w, h: int; luma: seq[float32]): Plane = Plane(w: w, h: h, luma: luma)

proc withPose(dx = 0.0; dy = 0.0; scale = 1.0; rot = 0.0): Pose =
  Pose(dx: dx, dy: dy, scale: scale, rot: rot, aspect: 1.0)

suite "direct":
  test "pose matrix is the identity at the identity and shifts in pixels":
    check PoseIdentity.matrix(640, 480) == IdentityHomography
    var (x, y) = withPose(dx = 0.1, dy = -0.05).matrix(200, 100).apply(50.0, 20.0)
    check abs(x - 70.0) < 1e-9 and abs(y - 15.0) < 1e-9
    let p = withPose(scale = 1.5, rot = 0.3)
    (x, y) = p.matrix(200, 100).apply(100.0, 50.0)
    check abs(x - 100.0) < 1e-9 and abs(y - 50.0) < 1e-9
    check fromVec(p.toVec) == p
    check free(moTranslation).countIt(it) == 2
    check free(moHomography).allIt(it)

  test "warp by the identity is exact and marks coverage":
    let (w, h) = (40, 30)
    let src = drTexture(w, h, 1)
    var (o, v) = warp(plane(w, h, src), PoseIdentity)
    for i in 0 ..< src.len: check abs(o[i] - src[i]) < 1e-6
    check v.allIt(it)
    (o, v) = warp(plane(w, h, src), withPose(dx = 10.0 / float64(w)))
    for y in 0 ..< h: check not v[y * w] and not v[y * w + 9] and v[y * w + 10]
    var valid = newSeq[bool](w * h)
    for i in 0 ..< w * h: valid[i] = i mod w < 20
    (o, v) = warp(Plane(w: w, h: h, luma: src, valid: some(valid)), PoseIdentity)
    check v[5 * w + 10] and not v[5 * w + 30]

  test "cost is zero at the truth and the sentinel without overlap":
    let (w, h) = (96, 72)
    let src = drTexture(w, h, 2)
    let l = levels(plane(w, h, src))
    check cost(l[0], l[0], PoseIdentity)[0] < 1e-9
    check cost(l[0], l[0], withPose(dx = 2.0))[0] == 1e9
    let lb = levels(plane(w, h, src.mapIt(it + 0.2'f32)))
    check cost(l[0], lb[0], PoseIdentity)[0] < 1e-6
    check cost(l[0], lb[0], withPose(dx = 0.02))[0] > 1e-3

  test "correlation is exposure independent and low for unrelated pictures":
    let (w, h) = (96, 72)
    let src = drTexture(w, h, 2)
    let l = levels(plane(w, h, src))
    let doubled = src.mapIt(it * 2'f32)
    let ld = levels(plane(w, h, doubled))
    let (rms, _, corr) = cost(l[0], ld[0], PoseIdentity)
    check rms > 0.05 and corr > 0.9999
    let other = levels(plane(w, h, drTexture(w, h, 7)))
    check abs(cost(l[0], other[0], PoseIdentity)[2]) < 0.3
    let flat = levels(plane(w, h, newSeqWith(w * h, 0.5'f32)))
    check cost(l[0], flat[0], PoseIdentity)[2] == 0.0
    let r = direct.register(plane(w, h, src), plane(w, h, doubled), moSimilarity, PoseIdentity, 0)
    check r.correlation > 0.999

  test "pyramid stops above 64 rows":
    var l = levels(plane(40, 300, newSeqWith(40 * 300, 0.25'f32)))
    check l.mapIt((it.w, it.h)) == @[(40, 300), (20, 150), (10, 75), (5, 38)]
    check l.allIt(it.luma.allIt(abs(it - 0.25'f32) < 1e-6))
    var valid = newSeq[bool](40 * 300)
    for i in 0 ..< valid.len: valid[i] = i mod 40 < 20
    l = levels(Plane(w: 40, h: 300, luma: newSeqWith(40 * 300, 0.25'f32), valid: some(valid)))
    let v = l[2].valid.get
    check v[10 * 10 + 2] and not v[10 * 10 + 8]

  test "nelder-mead finds the minimum of a bowl":
    let f = proc(x: seq[float64]): float64 = (x[0] - 0.3) ^ 2 + 2.0 * (x[1] + 0.2) ^ 2
    var r = nelderMead(f, @[0.0, 0.0], @[-1.0, -1.0], @[1.0, 1.0], @[0.1, 0.1], @[1e-6, 1e-6])
    check abs(r[0] - 0.3) < 1e-4 and abs(r[1] + 0.2) < 1e-4
    r = nelderMead(f, @[0.0, 0.0], @[-1.0, -1.0], @[0.1, 1.0], @[0.1, 0.1], @[1e-6, 1e-6])
    check abs(r[0] - 0.1) < 1e-6
    check nelderMead(f, @[], @[], @[], @[], @[]).len == 0
    let (step, tol) = levelSteps(@[0, 2], 3, 8000, 6000)
    check abs(step[0] * 8000.0 - 8.0) < 1e-9 and abs(tol[1] * 4000.0 - 0.8) < 1e-9

  test "registers a similarity to a tenth of a pixel":
    let (w, h) = (320, 240)
    let reference = drTexture(w, h, 3)
    let truth = withPose(dx = 3.3 / float64(w), dy = -2.1 / float64(h), scale = 1.012, rot = degToRad(0.6))
    let m = truth.matrix(w, h)
    var target: seq[float32]
    for i in 0 ..< w * h:
      let (x, y) = m.apply(float64(i mod w), float64(i div w))
      target.add direct.sample(reference, w, h, clamp(x, 0.0, float64(w - 1)), clamp(y, 0.0, float64(h - 1)))
    for (motion, coarsen) in [(moSimilarity, 0), (moSimilarity, 1), (moHomography, 0)]:
      let r = direct.register(plane(w, h, reference), plane(w, h, target), motion, PoseIdentity, coarsen)
      var corner = 0.0
      for (px, py) in [(0.0, 0.0), (float64(w), float64(h))]:
        let (tx, ty) = truth.matrix(w, h).apply(px, py)
        let (rx, ry) = r.h.apply(px, py)
        corner = max(corner, hypot(tx - rx, ty - ry))
      check corner < 0.1 * float64(1 shl coarsen) + 0.05
      let searched = (w shr coarsen) * (h shr coarsen)
      check r.rms < 0.01 and r.overlap > searched div 2
    let t = withPose(dx = 4.0 / float64(w), dy = 3.0 / float64(h))
    target.setLen(0)
    for i in 0 ..< w * h:
      target.add direct.sample(reference, w, h, clamp(float64(i mod w) + 4.0, 0.0, float64(w - 1)),
                               clamp(float64(i div w) + 3.0, 0.0, float64(h - 1)))
    let r = direct.register(plane(w, h, reference), plane(w, h, target), moTranslation, PoseIdentity, 0)
    check abs((r.pose.dx - t.dx) * float64(w)) < 0.1 and abs((r.pose.dy - t.dy) * float64(h)) < 0.1
    check r.pose.scale == 1.0 and r.pose.rot == 0.0

  test "coverage keeps uncovered pixels out of the fit":
    let (w, h) = (320, 240)
    let reference = drTexture(w, h, 4)
    var target: seq[float32]
    for i in 0 ..< w * h:
      target.add direct.sample(reference, w, h, clamp(float64(i mod w) + 2.5, 0.0, float64(w - 1)), float64(i div w))
    var valid = newSeq[bool](w * h)
    for i in 0 ..< w * h:
      valid[i] = i mod w < w div 2
      if not valid[i]: target[i] = float32((i * 7919) mod 1000) / 1000'f32
    let r = direct.register(plane(w, h, reference), Plane(w: w, h: h, luma: target, valid: some(valid)), moTranslation,
                            PoseIdentity, 0)
    check abs(r.pose.dx * float64(w) - 2.5) < 0.1
    check r.overlap < w * h div 2 + w and r.overlap > w * h div 3

  test "hostile input raises or degrades, never crashes":
    expect ValueError:
      discard direct.register(Plane(w: 10, h: 10, luma: newSeq[float32](3)), plane(1, 1, @[0'f32]), moSimilarity, PoseIdentity, 0)
    expect ValueError:
      discard warp(Plane(w: 2, h: 2, luma: newSeq[float32](4), valid: some(newSeq[bool](1))), PoseIdentity)
    let r = direct.register(plane(0, 0, @[]), plane(0, 0, @[]), moHomography, PoseIdentity, -4)
    check r.rms == 1e9 and r.overlap == 0
    let src = drTexture(20, 20, 1)
    discard direct.register(plane(20, 20, src), plane(20, 20, src), moHomography,
                            Pose(dx: NaN, dy: Inf, scale: 0.0, rot: 1e300, aspect: -1.0), 100)
    discard warp(plane(20, 20, src), Pose(scale: 0.0))
    discard warp(plane(20, 20, src), Pose(dx: 1e300, scale: 1.0, aspect: 1.0, px: NaN))
