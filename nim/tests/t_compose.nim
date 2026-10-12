## Compositor tests (ported from crates/compose/src/tests.rs): blending, masks, clipping,
## groups, adjustments, fills, layer styles, Blend If, tiling and reduced renders.
import std/[unittest, math, options]
import photocraft/[doc, compose]
import photocraft/color/blend as genericBlend

const E = 2'f32 / 255

proc close4(a, b: Px): bool =
  for i in 0 .. 3:
    if abs(a[i] - b[i]) > E: return false
  true

proc docWhite(w, h: int): Document =
  withBackground("t", size(w, h), cmRgb, stU8, White)

proc solidLayer(name: string; r: Rect; c: Px; fmt = Rgba8): Layer =
  result = rasterLayer(name, fmt)
  result.content.raster.fillRect(r, c)

proc px(d: Document; x, y: int): Px = render(d, rectXywh(x, y, 1, 1)).px[0]

proc fxDoc(effects: seq[Effect]): Document =
  result = docWhite(40, 40)
  var l = solidLayer("sq", rect(10, 10, 30, 30), [1'f32, 0, 0, 1])
  l.effects.items = effects
  result.layers.add l

proc strokeFx(size: float32; position: StrokePosition): Effect =
  result = newEffect(ekStroke)
  result.stroke = StrokeFx(common: fxCommon(bmNormal, 1), size: size, position: position,
                           paint: FxPaint(kind: fpColor, color: rgb(0, 0, 1), scale: 1))

proc shadow(distance, angle: float32): Shadow =
  Shadow(common: fxCommon(bmNormal, 1), color: Black, angle: angle, useGlobalLight: false,
         distance: distance, spread: 1, size: 0, contour: Contour(kind: ckLinear), knocksOut: true)

proc shadowFx(kind: EffectKind; s: Shadow): Effect =
  result = newEffect(kind)
  result.shadow = s

suite "compose":
  test "background, normal layer, opacity × fill, hidden layers":
    var d = docWhite(8, 8)
    check close4(px(d, 3, 3), [1'f32, 1, 1, 1])
    check close4(px(d, 20, 3), [0'f32, 0, 0, 0])
    d.layers.add solidLayer("red", rect(0, 0, 4, 8), [1'f32, 0, 0, 1])
    check close4(px(d, 1, 1), [1'f32, 0, 0, 1])
    check close4(px(d, 5, 1), [1'f32, 1, 1, 1])
    var d2 = docWhite(4, 4)
    var l = solidLayer("k", rect(0, 0, 4, 4), [0'f32, 0, 0, 1])
    l.opacity = 0.5
    l.fillOpacity = 0.5
    d2.layers.add l
    check abs(px(d2, 0, 0)[0] - 0.75) <= E
    d2.layers[1].visible = false
    check close4(px(d2, 0, 0), [1'f32, 1, 1, 1])

  test "every blend mode matches the reference on opaque pixels":
    let backdrop = [0.6'f32, 0.3, 0.2, 1]
    let source = [0.2'f32, 0.7, 0.5, 1]
    for mode in LayerModes:
      if mode == bmDissolve: continue
      var d = newDocument("b", size(2, 2), cmRgb, stF32)
      let bottom = solidLayer("b", rect(0, 0, 2, 2), backdrop, Rgba32f)
      var top = solidLayer("t", rect(0, 0, 2, 2), source, Rgba32f)
      top.blend = mode
      d.layers = @[bottom, top]
      let got = px(d, 0, 0)
      var want: array[3, float32]
      withBlendSpace(false, true):
        want = blendRgbPs(mode, [0.6'f32, 0.3, 0.2], [0.2'f32, 0.7, 0.5])
      for i in 0 .. 2:
        check abs(got[i] - want[i]) < 1e-5
      check abs(got[3] - 1) < 1e-6

  test "layer masks, disabled masks and feathering":
    var d = docWhite(4, 4)
    var l = solidLayer("k", rect(0, 0, 4, 4), [0'f32, 0, 0, 1])
    var m = revealAllMask()
    m.surface.fillRect(rect(0, 0, 2, 4), [0'f32])
    l.mask = some(m)
    d.layers.add l
    check close4(px(d, 0, 0), [1'f32, 1, 1, 1])
    check close4(px(d, 3, 0), [0'f32, 0, 0, 1])
    var d2 = docWhite(2, 2)
    var l2 = solidLayer("k", rect(0, 0, 2, 2), [0'f32, 0, 0, 1])
    var m2 = hideAllMask()
    m2.enabled = false
    l2.mask = some(m2)
    d2.layers.add l2
    check close4(px(d2, 0, 0), [0'f32, 0, 0, 1])
    var d3 = docWhite(64, 8)
    var l3 = solidLayer("k", rect(0, 0, 64, 8), [0'f32, 0, 0, 1])
    var m3 = hideAllMask()
    m3.surface.fillRect(rect(0, 0, 32, 8), [1'f32])
    l3.mask = some(m3)
    d3.layers.add l3
    check px(d3, 31, 4)[0] < 0.01 and px(d3, 32, 4)[0] > 0.99
    var m4 = d3.layers[1].mask.get
    m4.feather = 8
    d3.layers[1].mask = some(m4)
    let (a, b) = (px(d3, 31, 4)[0], px(d3, 32, 4)[0])
    check a > 0.3 and a < 0.5 and b > 0.5 and b < 0.7
    check px(d3, 4, 4)[0] < 0.01 and px(d3, 60, 4)[0] > 0.99

  test "clipping masks":
    var d = docWhite(8, 8)
    d.layers.add solidLayer("base", rect(0, 0, 4, 8), [0'f32, 0, 1, 1])
    var clip = solidLayer("clip", rect(0, 0, 8, 8), [1'f32, 0, 0, 1])
    clip.clipped = true
    d.layers.add clip
    check close4(px(d, 1, 1), [1'f32, 0, 0, 1])
    check close4(px(d, 6, 1), [1'f32, 1, 1, 1])
    d.layers[1].visible = false
    check close4(px(d, 1, 1), [1'f32, 1, 1, 1])

  test "isolated vs pass-through groups, group opacity":
    proc make(blend: BlendMode): Px =
      var d = docWhite(2, 2)
      d.layers[0].content.raster.fillRect(rect(0, 0, 2, 2), [0.5'f32, 0.5, 0.5, 1])
      var m = solidLayer("m", rect(0, 0, 2, 2), [0.5'f32, 0.5, 0.5, 1])
      m.blend = bmMultiply
      var g = groupLayer("g", @[m])
      g.blend = blend
      d.layers.add g
      px(d, 0, 0)
    check abs(make(bmPassThrough)[0] - 0.25) <= E
    check abs(make(bmNormal)[0] - 0.5) <= E
    proc makeFill(fill: float32): Px =
      var d = docWhite(2, 2)
      var g = groupLayer("g", @[newLayer("inv", adjustmentContent(newAdjustment(akInvert)))])
      g.blend = bmPassThrough
      g.fillOpacity = fill
      d.layers.add g
      px(d, 0, 0)
    check close4(makeFill(1), [0'f32, 0, 0, 1])
    check close4(makeFill(0.4), [1'f32, 1, 1, 1])
    var d = docWhite(2, 2)
    var g = groupLayer("g", @[solidLayer("a", rect(0, 0, 2, 2), [0'f32, 0, 0, 1]),
                              solidLayer("b", rect(0, 0, 2, 2), [0'f32, 0, 0, 1])])
    g.blend = bmNormal
    g.opacity = 0.5
    d.layers.add g
    check abs(px(d, 0, 0)[0] - 0.5) <= E
    var t = newDocument("coverage", size(2, 2), cmRgb, stF32)
    var hdrGroup = groupLayer("half", @[solidLayer("HDR red", t.bounds, [2'f32, 0, 0, 1], Rgba32f)])
    hdrGroup.opacity = 0.5
    t.layers.add hdrGroup
    check close4(px(t, 0, 0), [2'f32, 0, 0, 0.5])

  test "adjustment layers: invert, opacity + mask, clipped":
    var d = docWhite(2, 2)
    d.layers[0].content.raster.fillRect(rect(0, 0, 2, 2), [0.2'f32, 0.4, 0.6, 1])
    d.layers.add newLayer("inv", adjustmentContent(newAdjustment(akInvert)))
    check close4(px(d, 0, 0), [0.8'f32, 0.6, 0.4, 1])
    var d2 = docWhite(4, 1)
    d2.layers[0].content.raster.fillRect(rect(0, 0, 4, 1), [0'f32, 0, 0, 1])
    var adj = newLayer("inv", adjustmentContent(newAdjustment(akInvert)))
    adj.opacity = 0.5
    var m = revealAllMask()
    m.surface.fillRect(rect(2, 0, 4, 1), [0'f32])
    adj.mask = some(m)
    d2.layers.add adj
    check abs(px(d2, 0, 0)[0] - 0.5) <= E
    check abs(px(d2, 3, 0)[0]) <= E
    var d3 = docWhite(4, 1)
    d3.layers.add solidLayer("base", rect(0, 0, 2, 1), [0'f32, 0, 0, 1])
    var a3 = newLayer("inv", adjustmentContent(newAdjustment(akInvert)))
    a3.clipped = true
    d3.layers.add a3
    check close4(px(d3, 0, 0), [1'f32, 1, 1, 1])
    check close4(px(d3, 3, 0), [1'f32, 1, 1, 1])

  test "solid and gradient fill layers":
    var d = docWhite(10, 1)
    d.layers.add newLayer("fill", fillContent(solidFill(rgb(0, 0, 1))))
    check close4(px(d, 5, 0), [0'f32, 0, 1, 1])
    var d2 = docWhite(10, 1)
    d2.layers.add newLayer("g", fillContent(gradientFill(@[(0'f32, Black), (1'f32, White)], 0, 1, gsLinear, false)))
    let full = flatten(d2)
    check full.px[0][0] < full.px[9][0]
    check render(d2, rect(9, 0, 10, 1)).px[0] == full.px[9]

  test "dissolve coverage matches opacity and is deterministic":
    var d = newDocument("d", size(64, 64), cmRgb, stU8)
    var l = solidLayer("k", rect(0, 0, 64, 64), [0'f32, 0, 0, 1])
    l.blend = bmDissolve
    l.opacity = 0.3
    d.layers.add l
    let b = flatten(d)
    var covered = 0
    for p in b.px:
      if p[3] > 0.5: inc covered
    check abs(covered / b.px.len - 0.3) < 0.05
    check flatten(d) == b

  test "rendering is tile independent":
    var d = docWhite(300, 300)
    var l = solidLayer("a", rect(20, 20, 280, 280), [0.1'f32, 0.5, 0.9, 0.7])
    l.blend = bmOverlay
    d.layers.add l
    let full = flatten(d)
    let sub = render(d, rect(250, 250, 270, 262))
    var same = true
    for y in 250 ..< 262:
      for x in 250 ..< 270:
        if full.get(x, y) != sub.get(x, y): same = false
    check same

  test "CMYK documents render through the CMYK profile":
    var d = newDocument("c", size(2, 2), cmCmyk, stU8)
    var l = rasterLayer("k", d.pixelFormat)
    l.content.raster.fillRect(rect(0, 0, 2, 2), [0'f32, 0, 0, 1, 1])
    d.layers.add l
    let p = px(d, 0, 0)
    check p[0] < 0.3 and abs(p[0] - p[1]) < 0.05 and abs(p[1] - p[2]) < 0.05 and p[3] == 1

  test "strokes: outside, inside, center, zero fill, enable switches":
    let d = fxDoc(@[strokeFx(3, spOutside)])
    check close4(px(d, 8, 20), [0'f32, 0, 1, 1])
    check close4(px(d, 7, 20), [0'f32, 0, 1, 1])
    check close4(px(d, 5, 20), [1'f32, 1, 1, 1])
    check close4(px(d, 20, 20), [1'f32, 0, 0, 1])
    for (depth, fmt) in [(stU8, Rgba8), (stU16, Rgba16), (stF32, Rgba32f)]:
      var dz = withBackground("t", size(40, 40), cmRgb, depth, White)
      var l = solidLayer("sq", rect(10, 10, 30, 30), [1'f32, 0, 0, 1], fmt)
      l.effects.items = @[strokeFx(3, spOutside)]
      l.fillOpacity = 0
      dz.layers.add l
      check close4(px(dz, 20, 20), [1'f32, 1, 1, 1])
      check close4(px(dz, 11, 20), [1'f32, 1, 1, 1])
      check close4(px(dz, 8, 20), [0'f32, 0, 1, 1])
      dz.layers[1].fillOpacity = 0.5
      check close4(px(dz, 20, 20), [1'f32, 0.5, 0.5, 1])
    let di = fxDoc(@[strokeFx(2, spInside)])
    check close4(px(di, 10, 20), [0'f32, 0, 1, 1])
    check close4(px(di, 11, 20), [0'f32, 0, 1, 1])
    check close4(px(di, 13, 20), [1'f32, 0, 0, 1])
    check close4(px(di, 9, 20), [1'f32, 1, 1, 1])
    let dc = fxDoc(@[strokeFx(4, spCenter)])
    check close4(px(dc, 8, 20), [0'f32, 0, 1, 1])
    check close4(px(dc, 11, 20), [0'f32, 0, 1, 1])
    check close4(px(dc, 13, 20), [1'f32, 0, 0, 1])
    var off = fxDoc(@[strokeFx(3, spOutside)])
    off.layers[1].effects.enabled = false
    check close4(px(off, 8, 20), [1'f32, 1, 1, 1])
    var off2 = fxDoc(@[strokeFx(3, spOutside)])
    off2.layers[1].effects.items[0].stroke.common.enabled = false
    check close4(px(off2, 8, 20), [1'f32, 1, 1, 1])

  test "color overlay ignores fill opacity but not opacity":
    var e = newEffect(ekColorOverlay)
    e.common = fxCommon(bmNormal, 1)
    e.color = rgb(0, 1, 0)
    var d = fxDoc(@[e])
    d.layers[1].fillOpacity = 0
    check close4(px(d, 20, 20), [0'f32, 1, 0, 1])
    d.layers[1].opacity = 0.5
    check close4(px(d, 20, 20), [0.5'f32, 1, 0.5, 1])

  test "drop shadows: direction, global light, knockout, softness":
    let d = fxDoc(@[shadowFx(ekDropShadow, shadow(5, 90))])
    check close4(px(d, 20, 32), [0'f32, 0, 0, 1])
    check close4(px(d, 20, 8), [1'f32, 1, 1, 1])
    let d2 = fxDoc(@[shadowFx(ekDropShadow, shadow(5, 0))])
    check close4(px(d2, 8, 20), [0'f32, 0, 0, 1])
    check close4(px(d2, 32, 20), [1'f32, 1, 1, 1])
    var s = shadow(5, 0)
    s.useGlobalLight = true
    var d3 = fxDoc(@[shadowFx(ekDropShadow, s)])
    d3.globalLight.angle = 90
    check close4(px(d3, 20, 32), [0'f32, 0, 0, 1])
    d3.layers[1].fillOpacity = 0
    check close4(px(d3, 20, 28), [1'f32, 1, 1, 1])
    d3.layers[1].effects.items[0].shadow.knocksOut = false
    check close4(px(d3, 20, 28), [0'f32, 0, 0, 1])
    var soft = shadow(0, 90)
    soft.spread = 0
    soft.size = 6
    let d4 = fxDoc(@[shadowFx(ekDropShadow, soft)])
    let edge = px(d4, 30, 20)
    check edge[0] < 0.8 and edge[0] > 0.2
    check px(d4, 39, 20)[0] > 0.99

  test "inner shadow stays inside, group effects follow children's effects":
    var s = shadow(4, 90)
    s.knocksOut = false
    let d = fxDoc(@[shadowFx(ekInnerShadow, s)])
    check close4(px(d, 20, 11), [0'f32, 0, 0, 1])
    check close4(px(d, 20, 25), [1'f32, 0, 0, 1])
    var dg = docWhite(48, 48)
    var sq = solidLayer("sq", rect(14, 14, 34, 34), [1'f32, 0, 0, 1])
    sq.effects.items = @[strokeFx(6, spOutside)]
    var grp = groupLayer("g", @[sq])
    grp.effects.items = @[shadowFx(ekDropShadow, shadow(2, 90))]
    dg.layers.add grp
    check close4(px(dg, 9, 24), [0'f32, 0, 1, 1])
    check close4(px(dg, 24, 38), [0'f32, 0, 1, 1])
    check close4(px(dg, 24, 41), [0'f32, 0, 0, 1])
    check close4(px(dg, 24, 43), [1'f32, 1, 1, 1])

  test "effects render identically in tiles":
    var d = docWhite(300, 300)
    var l = solidLayer("sq", rect(40, 40, 260, 260), [1'f32, 0, 0, 1])
    var s = shadow(6, 45)
    s.spread = 0
    s.size = 8
    l.effects.items = @[shadowFx(ekDropShadow, s), strokeFx(4, spOutside)]
    d.layers.add l
    let full = flatten(d)
    let one = renderTiled(d, d.bounds, 1000)
    var same = true
    for i in 0 ..< full.px.len:
      for c in 0 .. 3:
        if abs(full.px[i][c] - one.px[i][c]) > 1e-5: same = false
    check same

  test "channel restrictions keep the backdrop":
    var d = docWhite(2, 2)
    var l = solidLayer("k", rect(0, 0, 2, 2), [0'f32, 0, 0, 1])
    l.excludedChannels = 0b010 # green off
    d.layers.add l
    check close4(px(d, 0, 0), [0'f32, 1, 0, 1])

  test "blend if: this layer and underlying layer":
    var d = docWhite(2, 1)
    d.layers[0].content.raster.fillRect(rect(0, 0, 1, 1), [0'f32, 0, 0, 1])
    var l = solidLayer("dark", rect(0, 0, 2, 1), [0.1'f32, 0.1, 0.1, 1])
    l.blendIf.set(0, [BlendRange(black: [0'u8, 0], white: [255'u8, 255]),
                      BlendRange(black: [128'u8, 128], white: [255'u8, 255])])
    d.layers.add l
    # underlying gray must be ≥ 128: hidden over black, shown over white
    check close4(px(d, 0, 0), [0'f32, 0, 0, 1])
    check close4(px(d, 1, 0), [0.1'f32, 0.1, 0.1, 1])

  test "reduced renders, damage and thumbnails":
    var d = docWhite(64, 32)
    d.layers.add solidLayer("a", rect(0, 0, 32, 32), [1'f32, 0, 0, 1])
    let r = renderReduced(d, 16, 8)
    check r.rect == rect(0, 0, 16, 8)
    check close4(r.px[0], [1'f32, 0, 0, 1])
    check close4(r.px[15], [1'f32, 1, 1, 1])
    let dmg = renderReducedDamage(d, 16, 8, rect(30, 0, 34, 4))
    check dmg.rect == rect(7, 0, 9, 1)
    for y in dmg.rect.y0 ..< dmg.rect.y1:
      for x in dmg.rect.x0 ..< dmg.rect.x1:
        check dmg.get(x, y) == r.get(x, y)
    let t = thumbnail(d, 16)
    check t.width == 16 and t.height == 8
    var rows: seq[Rect]
    renderBands(d, d.bounds, 1, proc(b: Buffer) = rows.add b.rect)
    check rows == @[d.bounds]

  test "flatten to surface and bounds helpers":
    var d = docWhite(8, 8)
    d.layers.add solidLayer("a", rect(2, 2, 4, 4), [1'f32, 0, 0, 1])
    let s = flattenToSurface(d, Rgba8)
    check s.pixel(3, 3) == @[1'f32, 0, 0, 1]
    check s.pixel(0, 0) == @[1'f32, 1, 1, 1]
    check layerBounds(d.layers[1], d.bounds) == rect(2, 2, 4, 4)
    check compositeBounds(d.layers[1], d.bounds) == some(rect(2, 2, 4, 4))
    check changeBounds(newLayer("inv", adjustmentContent(newAdjustment(akInvert))), d.bounds).isNone
    check not occludesBelow(d.layers[1], cmRgb)
    check occludesBelow(newLayer("f", fillContent(solidFill(rgb(1, 0, 0)))), cmRgb)

proc glow(technique: GlowTechnique; source: GlowSource): Glow =
  Glow(common: fxCommon(bmNormal, 1), paint: FxPaint(kind: fpColor, color: rgb(0, 1, 0), scale: 1),
       technique: technique, spread: 0, size: 4, contour: Contour(kind: ckLinear), range: 0.5, source: source)

proc glowFx(kind: EffectKind; g: Glow): Effect =
  result = newEffect(kind)
  result.glow = g

suite "compose effects":
  test "outer and inner glow regions, precise centre glow":
    let d = fxDoc(@[glowFx(ekOuterGlow, glow(gtPrecise, gsEdge))])
    let near = px(d, 9, 20)
    check near[1] > 0.7 and near[0] < 0.3
    check close4(px(d, 3, 20), [1'f32, 1, 1, 1])
    check close4(px(d, 20, 20), [1'f32, 0, 0, 1])
    let d2 = fxDoc(@[glowFx(ekInnerGlow, glow(gtSofter, gsEdge))])
    check px(d2, 10, 20)[1] > 0.3
    check close4(px(d2, 20, 20), [1'f32, 0, 0, 1])
    check close4(px(d2, 5, 20), [1'f32, 1, 1, 1])
    let d3 = fxDoc(@[glowFx(ekInnerGlow, glow(gtPrecise, gsCenter))])
    let (center, edge) = (px(d3, 20, 20), px(d3, 10, 20))
    check center[1] > 0.9 and center[0] < 0.1
    check edge[0] > center[0] and edge[1] < center[1]

  test "glow gradients follow the shape":
    var g = glow(gtSofter, gsEdge)
    var grad = defaultGradient()
    grad.stops = @[(0'f32, rgb(1, 0, 0)), (1'f32, rgb(0, 0, 1))]
    g.paint = FxPaint(kind: fpGradient, gradient: grad, scale: 1)
    g.size = 8
    let d = fxDoc(@[glowFx(ekOuterGlow, g)])
    let near = px(d, 9, 20)
    for p in [px(d, 30, 20), px(d, 20, 9), px(d, 20, 30)]:
      for c in 0 .. 3: check abs(p[c] - near[c]) < 1 / 255
    check near[0] > 0.8 and near[2] < 0.2 and near[1] < 0.1
    let o = px(d, 5, 20)
    check o[2] > o[0] and o[1] < 0.9

  test "gradient overlay follows angle and reverse":
    var g = defaultGradient()
    g.stops = @[(0'f32, Black), (1'f32, White)]
    g.angle = 0
    var e = newEffect(ekGradientOverlay)
    e.common = fxCommon(bmNormal, 1)
    e.gradient = g
    let d = fxDoc(@[e])
    check px(d, 10, 20)[0] < 0.1 and px(d, 29, 20)[0] > 0.9
    e.gradient.reverse = true
    let d2 = fxDoc(@[e])
    check px(d2, 10, 20)[0] > 0.9

  test "satin and bevel stay inside the shape, lit from the top":
    var satin = newEffect(ekSatin)
    satin.satin = Satin(common: fxCommon(bmMultiply, 1), color: Black, angle: 19, distance: 4, size: 4,
                        contour: Contour(kind: ckLinear), antiAlias: true)
    var bevel = newEffect(ekBevelEmboss)
    bevel.bevel = Bevel(enabled: true, style: bsInnerBevel, technique: btSmooth, depth: 1, up: true, size: 4,
                        angle: 90, altitude: 30, glossContour: Contour(kind: ckLinear),
                        highlight: fxCommon(bmScreen, 0.75), highlightColor: White,
                        shadow: fxCommon(bmMultiply, 0.75), shadowColor: Black)
    let d = fxDoc(@[satin, bevel])
    for (x, y) in [(5, 5), (35, 20), (20, 35)]:
      check close4(px(d, x, y), [1'f32, 1, 1, 1])
    check px(d, 20, 11)[1] > px(d, 20, 28)[1]

  test "clipped layer effects are clipped, group effects use the group's shape":
    var d = docWhite(40, 40)
    d.layers.add solidLayer("base", rect(10, 10, 30, 30), [1'f32, 0, 0, 1])
    var c = solidLayer("clip", rect(20, 10, 40, 30), [0'f32, 1, 0, 1])
    c.clipped = true
    c.effects.items.add strokeFx(3, spOutside)
    d.layers.add c
    check close4(px(d, 18, 20), [0'f32, 0, 1, 1])
    check close4(px(d, 20, 8), [1'f32, 1, 1, 1])
    var d2 = docWhite(40, 40)
    var g = groupLayer("g", @[solidLayer("a", rect(10, 10, 20, 30), [1'f32, 0, 0, 1]),
                              solidLayer("b", rect(20, 10, 30, 30), [1'f32, 0, 0, 1])])
    g.effects.items.add strokeFx(2, spOutside)
    d2.layers.add g
    check close4(px(d2, 9, 20), [0'f32, 0, 1, 1])
    check close4(px(d2, 20, 20), [1'f32, 0, 0, 1])

  test "effect maps are cached and invalidated by pixel changes":
    discard purgeEffectCache()
    var d = fxDoc(@[strokeFx(3, spOutside)])
    check close4(px(d, 8, 20), [0'f32, 0, 1, 1])
    check effectCacheBytes() > 0
    d.layers[1].content.raster.fillRect(rect(10, 10, 30, 30), [0'f32, 0, 0, 0])
    d.layers[1].content.raster.fillRect(rect(20, 10, 30, 30), [1'f32, 0, 0, 1])
    check close4(px(d, 8, 20), [1'f32, 1, 1, 1])
    check close4(px(d, 18, 20), [0'f32, 0, 1, 1])
