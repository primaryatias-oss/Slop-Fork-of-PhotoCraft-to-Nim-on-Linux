## Hole filling (`inpaint.rs` tests): completion, the onion peel against a plain scan, the
## source index, full-patch distances, proximity match, synthesis, plus no-crash checks. The
## module is included so its internals can be checked as the Rust tests do.

import std/unittest
include photocraft/algo/inpaint

proc texture(w, h: int): seq[float32] =
  ## Smooth gradient plus deterministic fine noise.
  var rng = newIRng(7)
  for i in 0 ..< w * h:
    let (x, y) = (float32(i mod w), float32(i div w))
    let n = float32(rng.next mod 1000) / 1000'f32 * 0.04'f32 - 0.02'f32
    result.add [0.3'f32 + 0.004'f32 * x + n, 0.5'f32 + 0.002'f32 * y + n, 0.4'f32 + n]

proc disc(w, h: int; cx, cy, r: float32): seq[bool] =
  for i in 0 ..< w * h: result.add hypot(float32(i mod w) - cx, float32(i div w) - cy) < r

proc holeRmse(a, b: seq[float32]; hole: seq[bool]; ch: int): float32 =
  var (e, n) = (0'f32, 0)
  for i, hh in hole:
    if hh:
      for c in 0 ..< ch:
        let d = a[i * ch + c] - b[i * ch + c]
        e += d * d
        inc n
  sqrt(e / float32(n))

proc busy(w, h, ch: int; seed: uint64): seq[float32] =
  ## Structure, fine noise and flat patches (so that some matches tie exactly).
  var rng = newIRng(seed)
  for i in 0 ..< w * h:
    let (x, y) = (i mod w, i div w)
    let flat = (x div 9 + y div 7) mod 5 == 0
    let n = float32(rng.next mod 1000) / 1000'f32
    for c in 0 ..< ch:
      result.add(if flat: 0.25'f32 + 0.1'f32 * float32(c)
                 else: min(0.2'f32 + 0.004'f32 * float32(x) + 0.1'f32 * float32((x + 2 * y + c) mod 6) + 0.05'f32 * n, 1'f32))

proc bitsOf(v: seq[float32]): seq[uint32] =
  for x in v: result.add cast[uint32](x)

proc roles(w, h: int; hole: seq[bool]): seq[Px] =
  ## A hole, a band that is context only, a corner that is no image.
  for i in 0 ..< w * h:
    let (x, y) = (i mod w, i div w)
    result.add(if hole[i]: pxHole
               elif x < 4 and y < 6: pxVoid
               elif x >= w * 3 div 4 and x < w * 3 div 4 + 3: pxKnown
               else: pxSource)

proc onionPeelPlain(w, h, ch: int; r: int; img: var seq[float32]; px: seq[Px]; sources: seq[(int32, int32)]) =
  ## The onion peel as first written: every source in order, every known pixel summed.
  var known = newSeq[bool](w * h)
  for i in 0 ..< w * h: known[i] = px[i] in {pxKnown, pxSource}
  while true:
    var front: seq[int]
    for i in 0 ..< w * h:
      let (x, y) = (i mod w, i div w)
      if known[i] or px[i] != pxHole: continue
      var edge = false
      for dy in -1 .. 1:
        for dx in -1 .. 1:
          let (nx, ny) = (x + dx, y + dy)
          if nx >= 0 and ny >= 0 and nx < w and ny < h and known[ny * w + nx]: edge = true
      if edge: front.add i
    if front.len == 0: break
    var vals: seq[seq[float32]]
    for i in front:
      let (x, y) = (i mod w, i div w)
      var best = (Inf.float32, (0'i32, 0'i32))
      for (sx, sy) in sources:
        var (e, n) = (0'f32, 0)
        for dy in -r .. r:
          for dx in -r .. r:
            let (tx, ty) = (x + dx, y + dy)
            if tx < 0 or ty < 0 or tx >= w or ty >= h or not known[ty * w + tx]: continue
            let (ti, si) = ((ty * w + tx) * ch, ((int(sy) + dy) * w + int(sx) + dx) * ch)
            for c in 0 ..< ch:
              let d = img[ti + c] - img[si + c]
              e += d * d
            inc n
        if n > 0 and e / float32(n) < best[0]: best = (e / float32(n), (sx, sy))
      let si = (int(best[1][1]) * w + int(best[1][0])) * ch
      vals.add img[si ..< si + ch]
    for k, i in front:
      for c in 0 ..< ch: img[i * ch + c] = vals[k][c]
      known[i] = true

suite "inpaint":
  test "completion fills a blemish from texture":
    let (w, h, ch) = (96, 96, 3)
    let clean = texture(w, h)
    let hole = disc(w, h, 48, 48, 9)
    var dirty = clean
    for i, hh in hole:
      if hh:
        for c in 0 ..< ch: dirty[i * ch + c] = 0.02
    let outp = complete(w, h, ch, dirty, hole, defaultCompleteParams()).get
    let before = holeRmse(dirty, clean, hole, ch)
    let after = holeRmse(outp, clean, hole, ch)
    check after < 0.06 and after < before * 0.15
    for i in 0 ..< outp.len:
      if not hole[i div ch]: check outp[i] == dirty[i]
    check outp == complete(w, h, ch, dirty, hole, defaultCompleteParams()).get

  test "completion continues structure":
    let (w, h, ch) = (80, 80, 1)
    var clean, dirty: seq[float32]
    for i in 0 ..< w * h: clean.add(if (i mod w div 4) mod 2 == 0: 0.2'f32 else: 0.8'f32)
    let hole = disc(w, h, 40, 40, 7)
    for i in 0 ..< w * h: dirty.add(if hole[i]: 0.5'f32 else: clean[i])
    let outp = complete(w, h, ch, dirty, hole, defaultCompleteParams()).get
    check holeRmse(outp, clean, hole, ch) < 0.15

  test "proximity finds the matching offset":
    let (w, h, ch) = (80, 60, 1)
    var img: seq[float32]
    for i in 0 ..< w * h: img.add sin(float32(i mod w) * 6.2831855'f32 / 10'f32) * 0.4'f32 + 0.5'f32
    let hole = disc(w, h, 40, 30, 5)
    let (dx, dy) = bestOffset(w, h, ch, img, hole, 3, 20).get
    check floorMod(dx, 10) == 0
    check (dx, dy) != (0'i32, 0'i32)

  test "proximity search clamps hostile radii":
    let (w, h, ch) = (24, 20, 1)
    var img: seq[float32]
    for i in 0 ..< w * h: img.add float32(i mod w) / float32(w)
    let hole = disc(w, h, 12, 10, 2)
    let r = bestOffset(w, h, ch, img, hole, high(int), int(high(int32)))
    if r.isSome: check abs(r.get[0]) <= int32(w) and abs(r.get[1]) <= int32(h)
    check bestOffset(w, h, ch, img, hole, 2, -1).isNone
    check bestOffset(w, h, ch, img, hole, 2, 0).isNone

  test "the onion peel picks exactly what the plain scan picks":
    let (w, h, ch, r) = (48, 40, 3, 3)
    var img = texture(w, h)
    for y in 0 ..< h:
      for x in 6 ..< w div 2:
        for c in 0 ..< ch: img[(y * w + x) * ch + c] = img[(y * w + x mod 6) * ch + c]
    for hole in [disc(w, h, 20, 18, 7.5), disc(w, h, 34, 22, 5)]:
      var px: seq[Px]
      for b in hole: px.add(if b: pxHole else: pxSource)
      let near = integral(w, h, hole)
      var sources: seq[(int32, int32)]
      for y in r ..< h - r:
        for x in r ..< w - r:
          if windowCount(near, w, h, x - r, y - r, x + r + 1, y + r + 1) == 0: sources.add (int32(x), int32(y))
      var (fast, plain) = (img, img)
      onionPeel(w, h, ch, int32(r), fast, px, holeBounds(w, h, px).get, sources, NoInterrupt)
      onionPeelPlain(w, h, ch, r, plain, px, sources)
      check bitsOf(fast) == bitsOf(plain)
      check fast != img

  test "the onion peel takes the plain scan's sources":
    for (w, h, ch, seed) in [(44, 36, 1, 1'u64), (50, 40, 3, 2'u64), (40, 40, 4, 3'u64)]:
      var img = busy(w, h, ch, seed)
      if seed == 3:
        for v in img.mitems: v = round(v * 3'f32) / 3'f32
      let hole = disc(w, h, float32(w) * 0.45'f32, float32(h) * 0.5'f32, 7.5)
      let px = roles(w, h, hole)
      let valid = validSources(w, h, 3, px, NoInterrupt).get
      let sources = newSources(valid, w).all(valid, w)
      var (a, b) = (img, img)
      onionPeel(w, h, ch, 3, a, px, holeBounds(w, h, px).get, sources, NoInterrupt)
      onionPeelPlain(w, h, ch, 3, b, px, sources)
      check bitsOf(a) == bitsOf(b)

  test "least sum is where the quotient crosses":
    var rng = newIRng(5)
    for i in 0 ..< 4000:
      let n = float32(1'u64 + rng.next mod 49)
      let v = (case i mod 4
        of 0: 0'f32
        of 1: float32(rng.next mod 1000) * 1e-9'f32
        else: float32(rng.next mod 1_000_000) / 1_000_000'f32)
      for strict in [true, false]:
        template holds(e: float32): bool = (if strict: e / n > v else: e / n >= v)
        let e = leastSum(v, n, strict)
        check holds(e)
        check e == 0'f32 or not holds(nextDown(e))
    check leastSum(Inf.float32, 4, true) == Inf.float32

  test "valid sources are the wholly copyable patches":
    let (w, h, r) = (37, 29, 2)
    var rng = newIRng(9)
    var px: seq[Px]
    for _ in 0 ..< w * h:
      px.add (case rng.next mod 23
        of 0: pxHole
        of 1: pxVoid
        of 2: pxKnown
        else: pxSource)
    let valid = validSources(w, h, r, px, NoInterrupt).get
    var source: seq[bool]
    for k in px: source.add k == pxSource
    let ss = integral(w, h, source)
    let full = uint32((2 * r + 1) * (2 * r + 1))
    for y in 0 ..< h:
      for x in 0 ..< w:
        let inside = x >= r and y >= r and x + r < w and y + r < h
        let want = inside and windowCount(ss, w, h, x - r, y - r, x + r + 1, y + r + 1) == full
        check valid[y * w + x] == want
    let sources = newSources(valid, w)
    let list = sources.all(valid, w)
    check list.len > 10 and list.len == sources.len
    for k, p in list: check sources.nth(k, valid, w) == p
    let (w2, h2) = (300, 9)
    var wide: seq[bool]
    for i in 0 ..< w2 * h2: wide.add (i mod w2) mod 7 == 3 and (i mod w2) div 64 != 2 and i div w2 != 4
    let s2 = newSources(wide, w2)
    for k, p in s2.all(wide, w2): check s2.nth(k, wide, w2) == p

  test "full patches measure as clipped ones":
    let (w, h, ch, r) = (46, 38, 4, 3)
    let img = busy(w, h, ch, 4)
    let hole = disc(w, h, 20, 18, 6)
    let px = roles(w, h, hole)
    let hb = holeBounds(w, h, px).get
    let tb = hb.grow(r, w, h)
    let valid = validSources(w, h, r, px, NoInterrupt).get
    var picks: seq[(int32, int32)]
    let all = newSources(valid, w).all(valid, w)
    var q = 0
    while q < all.len:
      picks.add all[q]
      q += 37
    check picks.len > 5
    for adapt in [AdaptNone, Adapt(gain: (0.8'f32, 1.25'f32), bias: 0.08)]:
      for keep in [false, true]:
        var s = Solver(w: w, h: h, ch: ch, r: int32(r), img: img, px: px, valid: valid, targets: targetsAround(w, r, px, hb, tb),
                       holeBox: hb, tbox: tb, adapt: adapt, ctl: NoInterrupt)
        s.prepare(keep)
        var nnf = newSeq[(int32, int32)](tb.area)
        for v in nnf.mitems: v = picks[0]
        var cost = newSeq[float32](tb.area)
        for c in cost.mitems: c = Inf.float32
        s.costs(nnf, cost)
        var checked = 0
        for k in 0 ..< tb.area:
          if not s.targets[k] or not s.full[k]: continue
          let t = (int32(tb.x0 + k mod tb.width), int32(tb.y0 + k div tb.width))
          for src in picks:
            if adapt.isNone:
              let exact = s.distClipped(t, src, Inf.float32)
              check cast[uint32](s.distFull(t, src, Inf.float32)) == cast[uint32](exact)
              for cutoff in [exact * 0.5'f32, exact, exact * 2'f32]:
                check (s.distFull(t, src, cutoff) < cutoff) == (s.distClipped(t, src, cutoff) < cutoff)
            else:
              var m1, m2: array[4, array[5, float32]]
              s.momentsFull(k, t, src, m1)
              check s.moments(t, src, m2) == (2 * r + 1) * (2 * r + 1)
              for c in 0 ..< 4:
                for j in 0 ..< 5: check cast[uint32](m1[c][j]) == cast[uint32](m2[c][j])
              check cast[uint32](s.distAdaptedFull(k, t, src)) == cast[uint32](s.distAdapted(t, src))
            inc checked
        check checked > 100

  test "owned completion fills in place or gives the pixels back":
    let (w, h, ch) = (64, 52, 3)
    let img = busy(w, h, ch, 6)
    let hole = disc(w, h, 30, 26, 8)
    let px = roles(w, h, hole)
    let p = defaultCompleteParams()
    let borrowed = completeMasked(w, h, ch, img, px, p, NoInterrupt).get
    var owned = img
    check completeMaskedOwned(w, h, ch, owned, px, p, NoInterrupt)
    check bitsOf(owned) == bitsOf(borrowed)
    var known = px
    for k in known.mitems:
      if k == pxSource: k = pxKnown
    check completeMasked(w, h, ch, img, known, p, NoInterrupt).isNone
    var back = img
    check not completeMaskedOwned(w, h, ch, back, known, p, NoInterrupt)
    check bitsOf(back) == bitsOf(img)
    var c = img
    expect Cancelled:
      discard completeMaskedOwned(w, h, ch, c, px, p, newInterrupt(proc(): bool = true))

  test "synthesis fills the hole with plausible values":
    let (w, h, ch) = (64, 64, 3)
    let clean = texture(w, h)
    let hole = disc(w, h, 32, 32, 6)
    let outp = synthesize(w, h, ch, clean, hole, 8, 3)
    check holeRmse(outp, clean, hole, ch) < 0.15
    for v in outp: check v.isFinite

  test "hostile input does not crash":
    let img = texture(20, 20)
    let hole = disc(20, 20, 10, 10, 3)
    expect ValueError: discard complete(20, 21, 3, img, hole, defaultCompleteParams())
    expect ValueError: discard complete(20, 20, 3, img[0 ..< 30], hole, defaultCompleteParams())
    var p = defaultCompleteParams()
    p.patchRadius = high(int)
    check complete(20, 20, 3, img, hole, p).isNone
    p = defaultCompleteParams()
    p.adapt = Adapt(gain: (5'f32, 0.1'f32), bias: -1)
    p.emIters = 1
    p.pmIters = 0
    check complete(20, 20, 3, img, hole, p).isSome
    var nanImg = img
    for i in 0 ..< nanImg.len:
      if i mod 7 == 0: nanImg[i] = NaN
    discard complete(20, 20, 3, nanImg, hole, defaultCompleteParams())
    check complete(20, 20, 0, @[], hole, defaultCompleteParams()).get.len == 0
    check bestOffset(20, 20, 3, img[0 ..< 5], hole, 2, 5).isNone
    check synthesize(20, 20, 3, img, hole[0 ..< 3], 8, 1) == img
    discard synthesize(20, 20, 3, nanImg, hole, high(int), 1)
    discard synthesize(20, 20, 3, img, hole, -5, 1)
