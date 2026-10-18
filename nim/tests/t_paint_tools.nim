## Brush engine modules (the tests of `paint/src/{rng,tile,procedural,mixer,retouch,replace,
## bg_erase,outline,symmetry,presets}.rs`), plus no-crash checks on hostile input.

import std/[unittest, sequtils, json, math, options, algorithm]
import photocraft/[geom, color, raster, serde, rmath]
import photocraft/paint

const
  Rgba8 = PixelFormat(mode: cmRgb, sample: stU8, alpha: true)
  Rgba16 = PixelFormat(mode: cmRgb, sample: stU16, alpha: true)
  Rgba32 = PixelFormat(mode: cmRgb, sample: stF32, alpha: true)
  Graya8 = PixelFormat(mode: cmGrayscale, sample: stU8, alpha: true)
  Gray8 = PixelFormat(mode: cmGrayscale, sample: stU8, alpha: false)
  Cmyka8 = PixelFormat(mode: cmCmyk, sample: stU8, alpha: true)

template tweak(base, body: untyped): untyped =
  block:
    var it {.inject.} = base
    body
    it

proc stroke(points: openArray[(float64, float64)]; size, hardness: float32): Stroke =
  result = Stroke(brush: defaultBrush().tweak((it.size = size; it.hardness = hardness; it.spacing = 0.25;
                                               it.pressureSize = false)))
  for (x, y) in points: result.points.add strokePoint(x, y, 1)

suite "paint: rng and tiles":
  test "rng is deterministic and uniform-ish":
    check rand01(7, 3, 1) == rand01(7, 3, 1)
    check rand01(7, 3, 1) != rand01(8, 3, 1)
    check rand01(7, 3, 1) != rand01(7, 3, 2)
    var mean = 0'f32
    for i in 0'u64 ..< 10000: mean += rand01(1, i, 0)
    mean /= 10000
    check abs(mean - 0.5) < 0.02
    for i in 0'u64 ..< 10000:
      let v = rand01(2, i, 5)
      check v >= 0 and v < 1

  test "base64 round trip":
    for n in 0 ..< 10:
      let b = toSeq(0 ..< n).mapIt(byte((it * 37 + 5) and 0xFF))
      check b64Decode(b64Encode(b)).get == b
    check b64Decode("abc").isNone
    check b64Decode("ab!=").isNone

  test "tile serde round trip and the float form":
    let t = grayTileFromFn(5, 3, proc(x, y: uint32): float32 = float32(x + y) / 6)
    check fromJson(parseJson($toJ(t)), GrayTile) == t
    check fromJson(parseJson("""{"width":2,"height":1,"data":[0.0,1.0]}"""), GrayTile).data == @[0'u16, 65535]
    expect SerdeError:
      discard fromJson(parseJson("""{"width":2,"height":2,"data":[0.0,1.0]}"""), GrayTile)
    expect SerdeError:
      discard fromJson(parseJson("""{"width":0,"height":0,"data":[]}"""), GrayTile)
    expect SerdeError:
      discard fromJson(parseJson("""{"width":4294967295,"height":4294967295,"data":"AAAA"}"""), GrayTile)

  test "previews downscale deterministically and tolerate bad tiles":
    let t = grayTileFromFn(1000, 400, proc(x, y: uint32): float32 = (if x < 500: 1'f32 else: 0'f32))
    let p = t.preview(96)
    check p.isValid and p.width <= 96 and p.height <= 96
    check p == t.preview(96)
    check p.get(0, 0) > 0.99 and p.get(p.width - 1, 0) < 0.01
    check p.data.allIt(it mod 257 == 0)
    check grayTileFromFn(5, 3, proc(x, y: uint32): float32 = 0.5).preview(96).width == 5
    check GrayTile(width: 4, height: 4, data: @[1'u16]).preview(96).isValid
    check GrayTile(width: 0, height: 0).preview(0).isValid

  test "stored tiles compare by key and serialise loaded as sampled":
    let full = grayTileFromFn(300, 200, proc(x, y: uint32): float32 = float32((x + y) mod 7) / 7)
    let r = StoredTile(key: "abc", width: 300, height: 200, preview: full.preview(PreviewSide))
    var loaded = r
    loaded.full = new(GrayTileRef)
    loaded.full[] = full
    check TipShape(kind: tkStored, stored: r) == TipShape(kind: tkStored, stored: loaded)
    check TipShape(kind: tkStored, stored: r) != TipShape(kind: tkSampled, sampled: full)
    check r.bitmap.width == r.preview.width
    check loaded.bitmap == full
    let j = toJ(TipShape(kind: tkStored, stored: r))
    check j["stored"]["key"].getStr == "abc"
    check not j["stored"].hasKey("full")
    var back: TipShape
    fromJ(j, back)
    check back == TipShape(kind: tkStored, stored: r)
    check $toJ(TipShape(kind: tkStored, stored: loaded)) == $toJ(TipShape(kind: tkSampled, sampled: full))
    check $toJ(Pattern(kind: pkStored, stored: loaded)) == $toJ(Pattern(kind: pkTile, tile: full))
    check toJ(Pattern(kind: pkStored, stored: r))["stored"]["width"].getInt == 300

  test "mips average":
    let t = grayTileFromFn(4, 4, proc(x, y: uint32): float32 = (if x < 2: 1'f32 else: 0'f32))
    let m = newMips(t)
    check m.levels.len == 3
    check abs(m.levels[2].v[0] - 0.5) < 1e-3
    check m.levelFor(4) == 0
    check m.levelFor(1) == 2

suite "paint: procedural":
  test "patterns are seamless and in range":
    for st in PatternStyle:
      let p = pattern(st, 64, 3)
      check p.data.allIt(it >= 0 and it <= 1)
      var dev = 0'f32
      for v in p.data: dev += abs(v - 0.5)
      check dev / float32(p.data.len) > 0.02
      check p.data == pattern(st, 64, 3).data
    let p = pattern(psNoise, 64, 1)
    var seam = 0'f32
    for y in 0 ..< 64: seam += abs(p.data[y * 64] - p.data[y * 64 + 63])
    check seam / 64 < 0.08

  test "tips have paint":
    for t in [chalkTip(48, 1), spatterTip(48, 1, 12), bristleTip(48, 1), charcoalTip(48, 1), leafTip(48),
              grassTip(48, 1, 9), spongeTip(48, 1), starTip(48), rakeTip(48, 1, 7)]:
      check t.isValid
      check t.toF32.foldl(a + b, 0'f32) > 48
      check t.get(0, 0) == 0

  test "degenerate sizes don't crash":
    for t in [chalkTip(0, 1), spatterTip(1, 1, 0), grassTip(2, 3, 0), rakeTip(1, 1, 0)]: discard t.isValid
    check pattern(psDots, 0, 0).width == 8
    check pattern(psCanvas, high(uint32), high(uint32)).width == 1024

suite "paint: mixer":
  proc mstroke(y: float64): Stroke =
    Stroke(brush: defaultBrush().tweak((it.size = 8; it.hardness = 1; it.spacing = 0.25; it.pressureSize = false)),
           points: @[strokePoint(4, y, 1), strokePoint(196, y, 1)])

  test "a low load runs dry and a full load does not":
    for (load, dry) in [(0'f32, true), (1'f32, false)]:
      var s = newSurface(Rgba8)
      var st = MixerState()
      st.load([1'f32, 0, 0, 1])
      discard applyMixerStroke(s, none(Surface), mstroke(10), MixerSettings(wet: 0, load: load, mix: 0, flow: 1), st,
                               none(Surface), false)
      let (start, finish) = (s.rgba(10, 10)[3], s.rgba(190, 10)[3])
      check start > 0.95
      if dry:
        check finish < start * 0.6
        check st.amount < 0.2
      else:
        check finish > 0.95
        check abs(st.amount - 1) < 1e-6

  test "a wet brush picks up and smears canvas colour":
    var s = newSurface(Rgba8)
    s.fillRect(rect(0, 0, 100, 20), [0'f32, 0, 1, 1])
    s.fillRect(rect(100, 0, 200, 20), [1'f32, 1, 1, 1])
    var st = MixerState()
    st.load([1'f32, 0, 0, 1])
    discard applyMixerStroke(s, none(Surface), mstroke(10), MixerSettings(wet: 1, load: 0.3, mix: 0.9, flow: 1), st,
                             none(Surface), false)
    let c = s.rgba(115, 10)
    check c[2] > c[1] + 0.05 and c[1] < 0.95
    check st.pickup.get[2] > 0.2
    st.clean()
    check st.pickup.isNone

  test "a dry, empty brush paints nothing":
    var s = newSurface(Rgba8)
    var st = MixerState()
    let dmg = applyMixerStroke(s, none(Surface), mstroke(10), defaultMixerSettings().tweak((it.wet = 0)), st,
                               none(Surface), false)
    check not dmg.isEmpty
    check s.rgba(50, 10)[3] == 0

  test "Transfer › Wetness scales pickup per dab":
    proc run(pressure: float32): Option[array[4, float32]] =
      var s = newSurface(Rgba8)
      s.fillRect(rect(0, 0, 200, 20), [0'f32, 0, 1, 1])
      var st = MixerState()
      st.load([1'f32, 0, 0, 1])
      var k = mstroke(10)
      k.brush.transfer = defaultTransfer().tweak((it.enabled = true; it.wetness = controlled(ctlPenPressure)))
      for p in k.points.mitems: p.pressure = pressure
      discard applyMixerStroke(s, none(Surface), k, MixerSettings(wet: 1, load: 1, mix: 1, flow: 1), st, none(Surface), false)
      st.pickup
    check run(0).isNone
    check run(1).get[2] > 0.2

  test "mixer state serde":
    var st = MixerState(amount: 0.5)
    st.load([0.1'f32, 0.2, 0.3, 1])
    let back = fromJson(parseJson($toJ(st)), MixerState)
    check back == st
    check fromJson(parseJson("{}"), MixerState) == MixerState()

suite "paint: retouch":
  test "coverage matches the apply-stroke bounds":
    let st = stroke([(10.0, 10.0), (40.0, 12.0)], 8, 1)
    let (r, cov) = strokeCoverage(st)
    check r.contains(10, 10) and r.contains(40, 12)
    check abs(cov[(10 - r.y0) * r.width + (25 - r.x0)] - 1) < 1e-6
    check cov.allIt(it >= 0 and it <= 1)

  test "a coverage composite copies exactly at full hardness":
    for fmt in [Rgba8, Rgba16, Rgba32, Cmyka8, Graya8]:
      var s = newSurface(fmt)
      let n = fmt.channels
      var fill = newSeqWith(n, 0.2'f32)
      fill[^1] = 1
      s.fillRect(rect(0, 0, 64, 64), fill)
      let st = stroke([(20.0, 20.0)], 10, 1)
      let (r, cov) = strokeCoverage(st)
      var paint = newPxRegion(r, n)
      for i in 0 ..< paint.data.len: paint.data[i] = (if fmt.alpha and i mod n == n - 1: 1'f32 else: 0.8'f32)
      discard applyCoverage(s, r, cov, 1, none(Surface), false, paint, bmNormal)
      let p = s.pixel(20, 20)
      check p[0 ..< n - 1].allIt(abs(it - 0.8) < 1e-2) and p[n - 1] == 1
      check abs(s.pixel(40, 40)[0] - 0.2) < 1e-2

  test "a dab stroke runs sequentially and respects the selection":
    var s = newSurface(Gray8)
    var sel = newSurface(Gray8)
    sel.fillRect(rect(0, 0, 30, 100), [1'f32])
    let st = stroke([(10.0, 10.0), (50.0, 10.0)], 6, 1)
    var seen: seq[int]
    discard applyDabStroke(s, st, some(sel), false, 2, proc(work: var PxRegion; fp: Footprint) =
      seen.add fp.index
      for y in fp.rect.y0 ..< fp.rect.y1:
        for x in fp.rect.x0 ..< fp.rect.x1:
          if fp.at(x, y) > 0.5: work.data[work.offset(x, y)] += 0.1)
    for i in 1 ..< seen.len: check seen[i] == seen[i - 1] + 1
    check s.pixel(20, 10)[0] > 0.1
    check s.pixel(40, 10)[0] == 0

  test "smudge drags colour along the stroke":
    for fmt in [Rgba8, Rgba16, Rgba32]:
      var s = newSurface(fmt)
      s.fillRect(rect(0, 0, 100, 40), [1'f32, 1, 1, 1])
      s.fillRect(rect(0, 0, 30, 40), [1'f32, 0, 0, 1])
      let st = stroke([(20.0, 20.0), (70.0, 20.0)], 12, 1)
      var sm = newSmudge(0.8, none(seq[float32]), 12)
      discard applyDabStroke(s, st, none(Surface), false, 0, proc(w: var PxRegion; fp: Footprint) = sm.dab(w, fp))
      check s.pixel(35, 20)[1] < 0.5
      check s.pixel(35, 20)[1] < s.pixel(60, 20)[1]
      check s.pixel(35, 35)[1] == 1
      var s2 = newSurface(fmt)
      s2.fillRect(rect(0, 0, 100, 40), [1'f32, 1, 1, 1])
      var fg = newSmudge(0.8, some(@[0'f32, 0, 1, 1]), 12)
      discard applyDabStroke(s2, st, none(Surface), false, 0, proc(w: var PxRegion; fp: Footprint) = fg.dab(w, fp))
      check s2.pixel(22, 20)[0] < 0.5

  test "smudging into transparency leaves no dark fringe":
    for fmt in [Rgba8, Rgba16, Rgba32]:
      var s = newSurface(fmt)
      s.fillRect(rect(0, 0, 30, 40), [1'f32, 0.8, 0, 1])
      let st = stroke([(20.0, 20.0), (70.0, 20.0)], 12, 0.5)
      var sm = newSmudge(0.8, none(seq[float32]), 12).withAlpha(3)
      discard applyDabStroke(s, st, none(Surface), false, 0, proc(w: var PxRegion; fp: Footprint) = sm.dab(w, fp))
      var smudged = 0
      for x in 30 ..< 80:
        for y in 14 ..< 27:
          let p = s.rgba(x, y)
          if p[3] > 0.02:
            inc smudged
            check p[0] > 0.97 and abs(p[1] - 0.8) < 0.03
      check smudged > 50
      var t = newSurface(fmt)
      t.fillRect(rect(0, 0, 30, 40), [1'f32, 0.8, 0, 1])
      var st2 = st
      st2.brush.opacity = 0.5
      var sm2 = newSmudge(0.8, none(seq[float32]), 12).withAlpha(3)
      discard applyDabStroke(t, st2, none(Surface), false, 0, proc(w: var PxRegion; fp: Footprint) = sm2.dab(w, fp))
      let p = t.rgba(34, 20)
      check p[3] > 0.02 and p[0] > 0.97

  test "a premultiplied mix ignores the colour of transparent pixels":
    var d = @[0'f32, 0, 0, 0]
    mixPremultiplied(d, [1'f32, 0.5, 0, 1], 3, 0.25)
    check d == @[1'f32, 0.5, 0, 0.25]
    d = @[0.2'f32, 0.2, 0.2, 1]
    mixPremultiplied(d, [0'f32, 0, 0, 0], 3, 1)
    check d[3] == 0
    d = @[0.5'f32, 0.5]
    mixPremultiplied(d, [1'f32, 1], 7, 0.5)
    check d == @[0.5'f32, 0.5]

  test "the footprint stays inside the full-size ring":
    let radius = 10.5'f32
    for hardness in [1'f32, 0.5, 0]:
      let dab = roundDab(point(32, 32), radius, 1)
      let fp = footprint(dab, 0, hardness)
      var painted = 0
      var farthest = 0.0
      for y in fp.rect.y0 ..< fp.rect.y1:
        for x in fp.rect.x0 ..< fp.rect.x1:
          let dist = sqrt((float64(x) + 0.5 - dab.center.x) ^ 2 + (float64(y) + 0.5 - dab.center.y) ^ 2)
          if fp.at(x, y) <= 0: continue
          inc painted
          farthest = max(farthest, dist)
          check dist < float64(radius) + 0.5
      check painted > 0
      if hardness == 1: check farthest > float64(radius)

suite "paint: Color Replacement":
  proc setup(fmt: PixelFormat): Surface =
    result = newSurface(fmt)
    result.fillRect(rect(0, 0, 40, 40), fromRgba(fmt, [0.2'f32, 0.6, 0.2, 1]))
    result.fillRect(rect(40, 0, 80, 40), fromRgba(fmt, [0.1'f32, 0.1, 0.5, 1]))
  proc rstroke(): Stroke =
    Stroke(brush: defaultBrush().tweak((it.size = 30; it.hardness = 1; it.spacing = 0.25; it.pressureSize = false)),
           points: @[strokePoint(20, 20, 1), strokePoint(60, 20, 1)])

  test "modes follow the non-separable definitions":
    let green = [0.2'f32, 0.6, 0.2]
    for mode in ReplaceMode:
      var s = setup(Rgba32)
      let rs = defaultReplaceSettings().tweak((it.mode = mode; it.sampling = smOnce; it.limits = lmDiscontiguous;
                                               it.tolerance = 0.1; it.color = [1'f32, 0, 0, 1]))
      discard applyColorReplacement(s, rstroke(), rs, none(Surface), false)
      let c = s.rgba(20, 20)
      case mode
      of rpHue, rpColor:
        check abs(lum([c[0], c[1], c[2]]) - lum(green)) < 1e-3
        check c[0] > c[1]
      of rpSaturation: check abs(lum([c[0], c[1], c[2]]) - lum(green)) < 1e-3 and c[1] > c[0]
      of rpLuminosity: check abs(lum([c[0], c[1], c[2]]) - lum([1'f32, 0, 0])) < 1e-3
      let b = s.rgba(60, 20)
      check abs(b[2] - 0.5) < 1e-3 and abs(b[0] - 0.1) < 1e-3

  test "continuous sampling follows the cursor; the swatch limits":
    var s = setup(Rgba8)
    let rs = defaultReplaceSettings().tweak((it.mode = rpColor; it.sampling = smContinuous; it.limits = lmContiguous;
                                             it.tolerance = 0.1; it.color = [1'f32, 0, 0, 1]))
    discard applyColorReplacement(s, rstroke(), rs, none(Surface), false)
    check s.rgba(20, 20)[0] > 0.3 and s.rgba(70, 20)[0] > s.rgba(70, 20)[1]
    var s2 = setup(Rgba8)
    discard applyColorReplacement(s2, rstroke(), rs.tweak((it.sampling = smBackgroundSwatch; it.background = [1'f32, 1, 1, 1])),
                                  none(Surface), false)
    check s2.rgba(20, 20) == setup(Rgba8).rgba(20, 20)

  test "contiguous stops at disconnected matches":
    var s = newSurface(Rgba32)
    s.fillRect(rect(0, 0, 60, 40), [0.2'f32, 0.6, 0.2, 1])
    s.fillRect(rect(30, 0, 34, 40), [0'f32, 0, 1, 1])
    var st = rstroke()
    st.points = @[strokePoint(20, 20, 1)]
    for (limits, islandChanged) in [(lmContiguous, false), (lmDiscontiguous, true)]:
      var t = s
      let rs = defaultReplaceSettings().tweak((it.mode = rpColor; it.sampling = smOnce; it.limits = limits;
                                               it.tolerance = 0.05; it.color = [1'f32, 0, 0, 1]))
      discard applyColorReplacement(t, st, rs, none(Surface), false)
      check (abs(t.rgba(34, 20)[0] - 0.2) > 0.05) == islandChanged
      check t.rgba(20, 20)[0] > 0.3

suite "paint: Background Eraser":
  const Green = [0.2'f32, 0.6, 0.2, 1]
  const Blue = [0.1'f32, 0.1, 0.5, 1]
  proc setup(fmt: PixelFormat): Surface =
    result = newSurface(fmt)
    result.fillRect(rect(0, 0, 40, 40), fromRgba(fmt, Green))
    result.fillRect(rect(40, 0, 80, 40), fromRgba(fmt, Blue))
  proc bstroke(points: openArray[(float64, float64)]): Stroke = stroke(points, 30, 1)
  proc alpha(s: Surface; x, y: int): float32 = s.rgba(x, y)[3]

  test "once erases only the sampled colour in every format":
    for mode in [cmRgb, cmCmyk, cmGrayscale]:
      for sample in [stU8, stU16, stF32]:
        let fmt = pixelFormat(mode, sample, true)
        var s = setup(fmt)
        let bs = defaultBgEraseSettings().tweak((it.sampling = smOnce; it.limits = lmDiscontiguous; it.tolerance = 0.05))
        let d = applyBackgroundEraser(s, bstroke([(20.0, 20.0), (60.0, 20.0)]), bs, none(Surface))
        check not d.isEmpty
        check alpha(s, 20, 20) == 0
        check alpha(s, 45, 20) == 1
        check alpha(s, 20, 39) == 1

  test "continuous follows the hotspot; the swatch matches the background":
    var s = setup(Rgba8)
    let bs = defaultBgEraseSettings().tweak((it.sampling = smContinuous; it.limits = lmDiscontiguous; it.tolerance = 0.05))
    discard applyBackgroundEraser(s, bstroke([(20.0, 20.0), (60.0, 20.0)]), bs, none(Surface))
    check alpha(s, 20, 20) == 0
    check alpha(s, 60, 20) == 0
    var t = setup(Rgba8)
    discard applyBackgroundEraser(t, bstroke([(30.0, 20.0), (50.0, 20.0)]),
                                  bs.tweak((it.sampling = smBackgroundSwatch; it.background = Blue)), none(Surface))
    check alpha(t, 30, 20) == 1
    check alpha(t, 50, 20) == 0

  test "contiguous and protect foreground":
    var s = newSurface(Rgba32)
    s.fillRect(rect(0, 0, 60, 40), Green)
    s.fillRect(rect(26, 0, 30, 40), Blue)
    for (limits, islandErased) in [(lmContiguous, false), (lmFindEdges, false), (lmDiscontiguous, true)]:
      var t = s
      let bs = defaultBgEraseSettings().tweak((it.sampling = smOnce; it.limits = limits; it.tolerance = 0.05))
      discard applyBackgroundEraser(t, bstroke([(20.0, 20.0)]), bs, none(Surface))
      check (alpha(t, 31, 20) == 0) == islandErased
      check alpha(t, 20, 20) == 0
    var t = s
    let bs = defaultBgEraseSettings().tweak((it.sampling = smOnce; it.tolerance = 0.05; it.protectForeground = true;
                                             it.foreground = Green))
    discard applyBackgroundEraser(t, bstroke([(20.0, 20.0)]), bs, none(Surface))
    check t == s

  test "the selection limits; degenerate inputs":
    var s = setup(Rgba8)
    var sel = newSurface(Gray8)
    sel.fillRect(rect(0, 0, 20, 40), [1'f32])
    let bs = defaultBgEraseSettings().tweak((it.sampling = smOnce; it.limits = lmDiscontiguous; it.tolerance = 0.05))
    discard applyBackgroundEraser(s, bstroke([(20.0, 20.0)]), bs, some(sel))
    check alpha(s, 15, 20) == 0
    check alpha(s, 25, 20) == 1
    var t = newSurface(Rgba8)
    check applyBackgroundEraser(t, bstroke([(5.0, 5.0)]), bs, none(Surface)).width > 0
    check t.contentBounds == EmptyRect
    var u = setup(Rgba8)
    discard applyBackgroundEraser(u, bstroke([(20.0, 20.0)]), bs.tweak((it.tolerance = NaN)), none(Surface))
    check alpha(u, 20, 20) == 0
    var g = newSurface(Gray8)
    check applyBackgroundEraser(g, bstroke([(5.0, 5.0)]), bs, none(Surface)).isEmpty

suite "paint: outline, symmetry, presets":
  proc maxRadius(rings: seq[Ring]): float32 =
    for r in rings:
      for p in r: result = max(result, hypot(p[0], p[1]))

  test "round outlines sit on the half coverage contour":
    for hardness in [0'f32, 0.25, 0.5, 0.75, 1]:
      let b = defaultBrush().tweak((it.size = 40; it.angle = 33; it.roundness = 0.4; it.hardness = hardness))
      check abs(maxRadius(tipOutline(b, false).get) - halfCoverageRadius(1, hardness)) < 1e-4
      check abs(maxRadius(tipOutline(b, true).get) - 1) < 1e-4

  test "sampled outlines stay inside the tip; blank tips have none":
    let disc = grayTileFromFn(32, 32, proc(x, y: uint32): float32 =
      let (dx, dy) = (float32(x) - 15.5'f32, float32(y) - 15.5'f32)
      clamp(1'f32 - sqrt(dx * dx + dy * dy) / 16, 0'f32, 1'f32))
    let rings = tipOutline(defaultBrush().tweak((it.size = 20; it.tip = TipShape(kind: tkSampled, sampled: disc))), false).get
    check rings.allIt(it.len >= 3)
    for r in rings:
      for p in r: check hypot(p[0], p[1]) <= 1.001
    let blank = grayTileFromFn(8, 8, proc(x, y: uint32): float32 = 0)
    check tipOutline(defaultBrush().tweak((it.size = 20; it.tip = TipShape(kind: tkSampled, sampled: blank))), false).isNone

  test "hostile settings get no outline":
    for size in [NaN.float32, Inf, 0, -1]:
      check tipOutline(defaultBrush().tweak((it.size = size; it.angle = 33; it.roundness = 0.5)), false).isNone
    for angle in [NaN.float32, Inf]:
      check tipOutline(defaultBrush().tweak((it.size = 40; it.angle = angle; it.roundness = 0.5)), false).isNone

  test "modes reflect about their centre and keep stylus data":
    var p = defaultStrokePoint()
    p.x = 12
    p.y = 25
    p.pressure = 0.4
    p.tiltX = 18
    for (mode, expected) in [(symVertical, [28.0, 25.0]), (symHorizontal, [12.0, 35.0]), (symDiagonal, [15.0, 22.0])]:
      let r = newPresetSymmetry(mode, [20.0, 30.0], 0).get.reflectedPasses([p])[0][0]
      check abs(r.x - expected[0]) < 1e-9 and abs(r.y - expected[1]) < 1e-9
      check (r.pressure, r.tiltX) == (p.pressure, p.tiltX)
    let copies = newPresetSymmetry(symDual, [20.0, 30.0], 0).get.reflectedPasses([p])
    check copies.len == 3
    for (c, e) in zip(copies, [[28.0, 25.0], [12.0, 35.0], [28.0, 35.0]]):
      check abs(c[0].x - e[0]) < 1e-9 and abs(c[0].y - e[1]) < 1e-9

  test "rotated axes clip on rectangles and accept half-pixel centres":
    for size in [[100.0, 100.0], [101.0, 53.0]]:
      for mode in SymmetryMode:
        let s = newPresetSymmetry(mode, [size[0] / 2, size[1] / 2], 0).get
        let segs = s.segments(size)
        check segs.len == (if mode == symDual: 2 else: 1)
        for seg in segs:
          for q in seg: check q[0] >= -1e-9 and q[0] <= size[0] + 1e-9 and q[1] >= -1e-9 and q[1] <= size[1] + 1e-9
        if mode == symDiagonal:
          let d = s.directions[0]
          check abs(d[0] - d[1]) < 1e-9
    let p = newPresetSymmetry(symVertical, [20.0, 30.0], 90).get.reflectedPasses([strokePoint(12, 25, 1)])[0][0]
    check abs(p.x - 12) < 1e-9 and abs(p.y - 35) < 1e-9
    check newPresetSymmetry(symDual, [NaN, 0.0], 0).isNone
    check newPresetSymmetry(symDual, [0.0, 0.0], Inf).isNone
    check newPresetSymmetry(symDual, [MaxCoordinate + 1, 0.0], 0).isNone
    check newPresetSymmetry(symHorizontal, [20.0, -5.0], 0).get.segments([100.0, 60.0]).len == 0
    check symmetryFromId("dual") == some(symDual) and symmetryFromId("x").isNone

  test "built-ins are grouped and unique":
    let v = builtin()
    check v.len in 30 .. 40
    var names = v.mapIt(it.name)
    names.sort()
    check names.deduplicate(isSorted = true).len == v.len
    for g in Groups: check v.countIt(it.group == g) >= 6
    check v.allIt(it.builtin and it.group in Groups)
    let order = v.mapIt(Groups.find(it.group))
    for i in 1 ..< order.len: check order[i - 1] <= order[i]
    check v.find("Hard Round Pressure Size").get.brush.shapeDynamics.size.control == ctlPenPressure
    check v.find("HARD ROUND").isSome

suite "paint: never crash on hostile input":
  test "malformed brush JSON raises a catchable error":
    for s in ["""{"size": "big"}""", """{"tip": "sampled"}""", """{"tip": {"nope": 1}}""",
              """{"texture": {"pattern": {"procedural": {"style": "dots"}}}}""", """{"mode": "Nope"}""",
              """{"tip": {"sampled": {"width": 2, "height": 1, "data": "AAD//w="}}}""", "[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39]",
              """{"seed": -1}""", """{"scattering": {"count": 1.5}}"""]:
      expect CatchableError:
        discard brushFromJson(parseJson(s))
    check brushFromJson(parseJson("[3, 0.5]")).hardness == 0.5

  test "absurd strokes render or no-op without panicking":
    var s = newSurface(Rgba8)
    for b in [defaultBrush().tweak((it.size = NaN)), defaultBrush().tweak((it.size = -3; it.spacing = NaN)),
              defaultBrush().tweak((it.roundness = NaN; it.angle = Inf; it.hardness = NaN)),
              defaultBrush().tweak((it.buildUp = true; it.buildUpRate = NaN)),
              defaultBrush().tweak((it.scattering.enabled = true; it.scattering.count = high(uint32))),
              defaultBrush().tweak((it.dualBrush.enabled = true; it.dualBrush.count = 0; it.dualBrush.size = NaN)),
              defaultBrush().tweak((it.texture.enabled = true; it.texture.scale = NaN;
                                    it.texture.pattern = Pattern(kind: pkTile, tile: GrayTile(width: 3, height: 3)))),
              defaultBrush().tweak((it.tip = TipShape(kind: tkSampled, sampled: GrayTile(width: 5, height: 5, data: @[1'u16]))))]:
      for pts in [@[strokePoint(NaN, 3, 1)], @[strokePoint(Inf, NegInf, 1), strokePoint(5, 5, NaN)],
                  @[strokePoint(5, 5, 1), strokePoint(9, 9, 1)], @[strokePoint(5, 5, 1), strokePoint(1e300, 5, 1)]]:
        discard renderStroke(s, b, pts, none(Surface), false, 1)
        discard dabs(Stroke(brush: b, points: pts))
    check renderStroke(s, defaultBrush(), [], none(Surface), false, 1) == EmptyRect

  test "hostile retouch inputs stay safe":
    var s = newSurface(Rgba8)
    let st = stroke([(5.0, 5.0), (30.0, 9.0)], 10, 1)
    var sm = newSmudge(NaN, some(newSeq[float32]()), Inf)
    discard applyDabStroke(s, st, none(Surface), false, 3, proc(w: var PxRegion; fp: Footprint) = sm.dab(w, fp))
    var st2 = MixerState(amount: NaN)
    st2.load([NaN.float32, 0, 0, 1])
    discard applyMixerStroke(s, none(Surface), st, MixerSettings(wet: NaN, load: Inf, mix: -1, flow: 2), st2, none(Surface), true)
    discard applyColorReplacement(s, st, defaultReplaceSettings().tweak((it.tolerance = NaN)), none(Surface), false)
    var noAlpha = newSurface(Gray8)
    check applyBackgroundEraser(noAlpha, st, defaultBgEraseSettings(), none(Surface)).isEmpty
    check applyDabStroke(s, Stroke(brush: defaultBrush()), none(Surface), false, 0, proc(w: var PxRegion; fp: Footprint) = discard) == EmptyRect
