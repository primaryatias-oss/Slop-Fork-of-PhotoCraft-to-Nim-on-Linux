## Document import/export tests (ported from crates/io/tests: roundtrip.rs, modes.rs, the shared
## generators in common/mod.rs, and pieces of flat.rs / psd_structure.rs).
import std/[unittest, options, strutils, sets, tables, math]
import photocraft/[geom, color, raster, doc, psd, io, codecs]
import photocraft/io/[effects_map, vector_map, linked, common]

# ---------------------------------------------------------------- generators (common/mod.rs)

proc g(v: int): float32 = float32(v) / 255

proc pattern(fmt: PixelFormat; r: Rect; seed: int; alpha: bool): Surface =
  result = newSurface(fmt)
  let n = fmt.channels
  var vals: seq[float32]
  for y in r.y0 ..< r.y1:
    for x in r.x0 ..< r.x1:
      for c in 0 ..< n:
        let v = if c == n - 1 and fmt.alpha:
                  (if alpha: floorMod((x + y) * 37 + seed, 256) else: 255)
                else: floorMod(x * 13 + y * 7 + seed * 31 + c * 50, 256)
        vals.add float32(v) / 255
  result.writeRegion(r, vals)
  result.prune()

proc maskSurface(sample: SampleType; r: Rect; default: float32; seed: int): Surface =
  result = surfaceWithDefault(pixelFormat(cmGrayscale, sample, false), [default])
  var vals: seq[float32]
  for i in 0 ..< r.width * r.height: vals.add float32((i * 29 + seed) mod 256) / 255
  result.writeRegion(r, vals)
  result.prune()

proc fillColor(mode: ColorMode): Color =
  case mode
  of cmCmyk: Color(mode: cmCmyk, c: [0.2'f32, 0.4, 0.6, 0.1], alpha: 1)
  of cmGrayscale: gray(0.25)
  else: rgb(1, 0.5, 0.25)

type Features = object
  groups, masks, adjustments, fills, allBlends, extras: bool

const
  AllFeatures = Features(groups: true, masks: true, adjustments: true, fills: true, allBlends: true, extras: true)
  PixelFeatures = Features(groups: true, masks: true, allBlends: true)

proc rasterOf(name: string; fmt: PixelFormat; r: Rect; seed: int; alpha: bool): Layer =
  newLayer(name, rasterContent(pattern(fmt, r, seed, alpha)))

proc toneSpace(mode: ColorMode): ToneSpace =
  case mode
  of cmCmyk: tsCmyk
  of cmLab: tsLab
  else: tsRgb

proc genDoc(mode: ColorMode; depth: SampleType; f: Features): Document =
  result = newDocument("gen", size(24, 16), mode, depth)
  let fmt = result.pixelFormat
  var bg = rasterOf("Background", fmt, result.bounds, 1, false)
  bg.locks.transparency = true
  bg.locks.position = true
  result.layers.add bg
  var neg = rasterOf("Negative \u{1F600}", fmt, rect(-5, -3, 7, 6), 2, true)
  neg.opacity = g(200)
  neg.fillOpacity = g(128)
  neg.blend = bmMultiply
  neg.label = lcBlue
  neg.advanced.blendInterior = true
  neg.advanced.transparencyShapes = false
  result.layers.add neg
  var hidden = rasterOf("Hidden", fmt, rect(3, 3, 9, 9), 3, true)
  hidden.visible = false
  result.layers.add hidden
  var clipped = rasterOf("Clipped", fmt, rect(0, 0, 10, 10), 4, true)
  clipped.clipped = true
  clipped.blend = bmScreen
  result.layers.add clipped
  result.layers.add newLayer("Empty", rasterContent(newSurface(fmt)))
  if f.masks:
    var m = rasterOf("Masked", fmt, rect(2, 2, 14, 12), 5, true)
    m.mask = some(doc.LayerMask(surface: maskSurface(depth, rect(4, 4, 10, 9), 1, 1), enabled: true, linked: true,
                                density: 1, feather: 0))
    m.advanced.layerMaskHidesEffects = true
    m.advanced.vectorMaskHidesEffects = true
    result.layers.add m
    var m2 = rasterOf("Masked2", fmt, rect(-2, 1, 6, 5), 6, true)
    m2.mask = some(doc.LayerMask(surface: maskSurface(depth, rect(-1, 0, 3, 4), 0, 2), enabled: false, linked: false,
                                 density: g(128), feather: 2.5))
    result.layers.add m2
  if f.groups:
    let innerChild = rasterOf("Inner child", fmt, rect(1, 1, 5, 5), 7, true)
    var inner = groupLayer("Inner", @[innerChild])
    inner.blend = bmMultiply
    inner.opacity = g(180)
    inner.content.group.expanded = false
    inner.advanced.knockout = koShallow
    let a = rasterOf("Outer child", fmt, rect(6, 2, 12, 8), 8, true)
    var outer = groupLayer("Outer", @[a, inner])
    outer.locks.position = true
    outer.label = lcRed
    outer.advanced.knockout = koDeep
    outer.advanced.blendClipped = false
    result.layers.add outer
    result.layers.add groupLayer("Empty group", @[])
  if f.allBlends:
    for i, m in LayerModes:
      if m == bmDissolve: continue
      var l = rasterOf("blend " & m.label, fmt, rectXywh(i mod 20, i mod 12, 3, 2), 10 + i, true)
      l.blend = m
      result.layers.add l
  if f.adjustments:
    var adjs: seq[Adjustment]
    adjs.add newAdjustment(akInvert)
    var t = newAdjustment(akThreshold); t.level = g(100); adjs.add t
    var p = newAdjustment(akPosterize); p.levels = 5; adjs.add p
    var bc = newAdjustment(akBrightnessContrast); bc.brightness = 20; bc.contrast = -10; adjs.add bc
    var hs = newAdjustment(akHueSaturation); hs.hue = 30; hs.saturation = -20; hs.lightness = 5; adjs.add hs
    var ex = newAdjustment(akExposure); ex.exposure = 0.5; ex.offset = 0; ex.gamma = 1; adjs.add ex
    var lv = newAdjustment(akLevels)
    lv.levelsMaster = LevelsChannel(inBlack: g(10), inWhite: g(240), gamma: 1.2, outBlack: 0, outWhite: 1)
    lv.space = toneSpace(mode)
    adjs.add lv
    var cv = newAdjustment(akCurves)
    cv.curvesMaster = @[CurvePoint(input: 0, output: 0), CurvePoint(input: g(128), output: g(150)), CurvePoint(input: 1, output: 1)]
    cv.space = toneSpace(mode)
    adjs.add cv
    var un = newAdjustment(akUnsupported); un.psdKey = "selc"; un.raw = @[0'u8, 1, 0, 0]; adjs.add un
    if mode != cmGrayscale:
      var vb = newAdjustment(akVibrance); vb.vibrance = 40; vb.saturation = -10; adjs.add vb
      var cb = newAdjustment(akColorBalance)
      cb.shadows = [10'f32, 0, -5]; cb.midtones = [0'f32, 20, 0]; cb.highlights = [-30'f32, 0, 15]
      cb.preserveLuminosity = true
      adjs.add cb
      var bw = newAdjustment(akBlackWhite); bw.weights = [50'f32, 60, 40, 60, 20, 70]; bw.tint = some([1'f32, 0.8, 0.6]); adjs.add bw
      var pf = newAdjustment(akPhotoFilter)
      pf.filterColor = [1'f32, float32(32768) / 65535, 0]; pf.density = 0.25; pf.preserveLuminosity = false
      adjs.add pf
      var cm = newAdjustment(akChannelMixer)
      cm.matrix = [[0.8'f32, 0.2, 0, 0], [0'f32, 1, 0, 0.1], [0.1'f32, 0, 0.9, 0]]
      adjs.add cm
    for i, a in adjs:
      var l = newLayer("adj " & $i, adjustmentContent(a))
      l.opacity = g(if i mod 2 == 0: 255 else: 128)
      result.layers.add l
  if f.fills:
    var solid = newLayer("Solid fill", fillContent(solidFill(fillColor(mode))))
    solid.opacity = g(64)
    result.layers.add solid
  if f.extras:
    result.resolutionDpi = 300
    result.iccProfile = shared(@[1'u8, 2, 3, 4])
    result.metadata.xmp = some("<x:xmpmeta/>")
    result.metadata.exif = shared(cast[seq[byte]]("MM\0*\0\0\0\x08"))
    result.metadata.psdResources.add PsdResource(id: 4000, name: "", data: shared(@[9'u8, 9, 9]))
    result.guides.horizontal.add 5.5
    result.guides.vertical.add 12.0
    result.channels.add newAlphaChannel("Selection", maskSurface(depth, result.bounds, 0, 3))

proc pruned(s: Surface): Surface =
  result = s
  result.prune()

proc surfaceEq(a0, b0: Surface; ctx: string) =
  let (a, b) = (pruned(a0), pruned(b0))
  check a.format == b.format
  check a.defaultPixel == b.defaultPixel
  check a.contentBounds == b.contentBounds
  let r = a.contentBounds.union(b.contentBounds)
  if not r.isEmpty and a.toInterleaved(r) != b.toInterleaved(r):
    checkpoint ctx & ": pixels differ"
    fail()

proc colorClose(a, b: Color; ctx: string) =
  check a.mode == b.mode
  for i in 0 ..< 4:
    if abs(a.c[i] - b.c[i]) >= 1e-4:
      checkpoint ctx & ": color"
      fail()

proc layersEq(a, b: Layer; path: string) =
  let ctx = path & "/" & a.name
  checkpoint ctx
  check a.name == b.name
  check a.visible == b.visible
  check a.locks == b.locks
  check a.blend == b.blend
  check a.opacity == b.opacity
  check a.fillOpacity == b.fillOpacity
  check a.clipped == b.clipped
  check a.label == b.label
  check a.advanced == b.advanced
  check a.excludedChannels == b.excludedChannels
  check a.blendIf == b.blendIf
  check a.effects.psdRaw == b.effects.psdRaw
  check a.effects.enabled == b.effects.enabled
  if a.psdId.isSome: check a.psdId == b.psdId
  for blk in a.psdBlocks:
    if blk notin b.psdBlocks:
      checkpoint ctx & ": preserved block " & blk.key & " missing"
      fail()
  if a.fillCache.isSome:
    check b.fillCache.isSome
    if b.fillCache.isSome: surfaceEq(a.fillCache.get.surface, b.fillCache.get.surface, ctx & ": fill cache")
  check a.mask.isSome == b.mask.isSome
  if a.mask.isSome and b.mask.isSome:
    let (x, y) = (a.mask.get, b.mask.get)
    check x.enabled == y.enabled
    check x.linked == y.linked
    check x.density == y.density
    check x.feather == y.feather
    surfaceEq(x.surface, y.surface, ctx & ": mask")
  check a.content.kind == b.content.kind
  if a.content.kind != b.content.kind: return
  case a.content.kind
  of lkRaster: surfaceEq(a.content.raster, b.content.raster, ctx)
  of lkGroup:
    check a.content.group.expanded == b.content.group.expanded
    check a.content.group.children.len == b.content.group.children.len
    for i in 0 ..< min(a.content.group.children.len, b.content.group.children.len):
      layersEq(a.content.group.children[i], b.content.group.children[i], ctx)
  of lkAdjustment: check a.content.adjustment == b.content.adjustment
  of lkFill:
    if a.content.fill.kind == fkSolid and b.content.fill.kind == fkSolid:
      colorClose(a.content.fill.color, b.content.fill.color, ctx)
    else: check a.content.fill == b.content.fill
  of lkText:
    check a.content.text.text == b.content.text.text
    surfaceEq(a.content.text.cache.get, b.content.text.cache.get, ctx)
  of lkShape: surfaceEq(a.content.shape.cache.get, b.content.shape.cache.get, ctx)
  of lkSmart: surfaceEq(a.content.smart.cache.get, b.content.smart.cache.get, ctx)

proc docsEq(a, b: Document) =
  check a.size == b.size
  check a.mode == b.mode
  check a.depth == b.depth
  check abs(a.resolutionDpi - b.resolutionDpi) < 1e-3
  check a.iccProfile == b.iccProfile
  check a.metadata == b.metadata
  check a.guides == b.guides
  check a.channels.len == b.channels.len
  for i in 0 ..< min(a.channels.len, b.channels.len):
    check a.channels[i].name == b.channels[i].name
    surfaceEq(a.channels[i].surface, b.channels[i].surface, "channel " & a.channels[i].name)
  check a.layers.len == b.layers.len
  for i in 0 ..< min(a.layers.len, b.layers.len): layersEq(a.layers[i], b.layers[i], "")

proc roundtrip(d: Document): Document =
  let r = exportDoc(d, "x.psd")
  let file = parsePsd(r.bytes)
  check file.toBytes == r.bytes
  importDoc("x.psd", r.bytes).document

proc findRec(f: PsdFile; name: string): LayerRecord =
  for r in f.layers:
    if r.name == name: return r

proc blockOf(r: LayerRecord; key: string): Option[seq[byte]] =
  let b = r.findBlock(key)
  if b.isSome: some(b.get.data) else: none(seq[byte])

# ---------------------------------------------------------------- roundtrip.rs

suite "io: PSD round trips":
  for (mode, depth, feats) in [(cmRgb, stU8, AllFeatures), (cmRgb, stU16, AllFeatures), (cmRgb, stF32, AllFeatures),
                              (cmGrayscale, stU8, AllFeatures), (cmGrayscale, stU16, AllFeatures),
                              (cmGrayscale, stF32, AllFeatures), (cmCmyk, stU8, AllFeatures),
                              (cmCmyk, stU16, AllFeatures), (cmLab, stU8, AllFeatures), (cmLab, stU16, AllFeatures),
                              (cmRgb, stU8, PixelFeatures), (cmGrayscale, stU16, PixelFeatures),
                              (cmCmyk, stU8, PixelFeatures)]:
    test "round trip " & $mode & " " & $depth & (if feats.extras: " all" else: " pixels"):
      let d = genDoc(mode, depth, feats)
      docsEq(d, roundtrip(d))

  test "psb round trip":
    let d = genDoc(cmRgb, stU8, AllFeatures)
    let r = exportDoc(d, "x.psb")
    check r.bytes[5] == 2
    docsEq(d, importDoc("x.psb", r.bytes).document)

  test "force psb option":
    var o = defaultExportOptions()
    o.forcePsb = true
    check exportDoc(genDoc(cmRgb, stU8, PixelFeatures), "x.psd", o).bytes[5] == 2

  test "import sets name":
    let r = exportDoc(genDoc(cmRgb, stU8, PixelFeatures), "psd")
    check importDoc("hello.psd", r.bytes).document.name == "hello.psd"

  test "empty document":
    let d = newDocument("e", size(4, 3), cmRgb, stU8)
    let back = roundtrip(d)
    check back.layers.len <= 1
    check back.size == d.size

  test "zero-sized psd is rejected":
    let good = exportDoc(genDoc(cmRgb, stU8, PixelFeatures), "x.psd").bytes
    for (a, b) in [(14, 18), (18, 22), (14, 22)]:
      var bytes = good
      for i in a ..< b: bytes[i] = 0
      expect IoFailure:
        discard importDoc("z.psd", bytes)
      try: discard importDoc("z.psd", bytes)
      except IoFailure as e: check "at least 1x1" in e.msg
    expect IoFailure:
      discard exportDoc(newDocument("e", size(0, 3), cmRgb, stU8), "x.psd")

  test "round trip reaches a fixed point":
    let d = genDoc(cmRgb, stU16, AllFeatures)
    let a = exportDoc(d, "a.psd").bytes
    let b = exportDoc(importDoc("a.psd", a).document, "a.psd").bytes
    let c = exportDoc(importDoc("b.psd", b).document, "a.psd").bytes
    check b == c
    check parsePsd(a).layers == parsePsd(b).layers

  test "raster layer blocks preserved":
    var d = genDoc(cmRgb, stU8, PixelFeatures)
    let blocks = @[PsdBlock(key: "vmsk", data: shared(newSeq[byte](12))), PsdBlock(key: "clbl", data: shared(@[0'u8, 0, 0, 0])),
                   PsdBlock(key: "Zzzz", data: shared(@[1'u8]))]
    d.layers[1].psdBlocks = blocks
    d.layers[1].advanced = defaultAdvancedBlending()
    d.layers[1].advanced.blendClipped = false
    d.layers[1].vectorMask = vectorMaskFromBlock(newSeq[byte](12), d.size.width, d.size.height)
    let back = roundtrip(d)
    var expected = blocks
    expected[2].data = shared(@[1'u8, 0])
    check back.layers[1].psdBlocks == expected
    check roundtrip(back).layers[1].psdBlocks == expected
    check back.layers[1].vectorMask == d.layers[1].vectorMask

  test "psd ids are kept and deduplicated":
    var d = genDoc(cmRgb, stU8, PixelFeatures)
    d.layers[0].psdId = some(500'u32)
    d.layers[1].psdId = some(500'u32)
    d.layers[2].psdId = some(3'u32)
    let back = roundtrip(d)
    check back.layers[0].psdId == some(500'u32)
    check back.layers[1].psdId != some(500'u32)
    check back.layers[2].psdId == some(3'u32)
    var ids: seq[uint32]
    for (_, _, l) in back.walk:
      if l.psdId.isSome: ids.add l.psdId.get
    check ids.toHashSet.len == ids.len

  test "edited adjustment regenerates block, unedited keeps raw":
    var d = genDoc(cmRgb, stU8, PixelFeatures)
    var l = newLayer("lv", adjustmentContent(newAdjustment(akLevels)))
    var t = d
    t.layers.add l
    var raw = findRec(documentToPsd(t), "lv").blockOf("levl").get
    raw[2 + 5 * 10 + 1] = 7
    l.psdBlocks = @[PsdBlock(key: "levl", data: shared(raw))]
    d.layers.add l
    check findRec(documentToPsd(d), "lv").blockOf("levl").get == raw
    d.layers[^1].content.adjustment.levelsMaster.inBlack = 10 / 255
    let data = findRec(documentToPsd(d), "lv").blockOf("levl").get
    check data != raw
    check data[3] == 10

  test "gradient and pattern fills round trip":
    var d = genDoc(cmRgb, stU8, PixelFeatures)
    d.layers.add newLayer("grad", fillContent(gradientFill(@[(0'f32, rgb(1, 0, 0)), (1'f32, rgb(0, 0, 1))], 30, 1, gsDiamond, true)))
    var pf = patternFill("Dots", "", 0.5)
    d.layers.add newLayer("pat", fillContent(pf))
    docsEq(d, roundtrip(d))

  test "psd raw resources and global blocks preserved":
    var d = genDoc(cmRgb, stU8, PixelFeatures)
    d.metadata.psdResources.add PsdResource(id: 4001, name: "named", data: shared(@[1'u8]))
    d.metadata.psdGlobalBlocks.add PsdGlobalBlock(signature: "8BIM", key: "Patt", data: shared(@[7'u8, 7, 7, 7, 7, 7]))
    d.metadata.psdGlobalBlocks.add PsdGlobalBlock(signature: "8B64", key: "lnk2", data: shared(@[1'u8, 2]))
    check roundtrip(d).metadata == d.metadata

  test "documentToPsd direct api":
    let d = genDoc(cmRgb, stU8, AllFeatures)
    let f = documentToPsd(d)
    check f.header.width == 24
    docsEq(d, psdToDocument(f)[0])

  test "big document goes psb with warning":
    let d = newDocument("big", size(30_001, 1), cmGrayscale, stU8)
    let (f, w) = documentToPsdWith(d, defaultPsdExportOptions())
    check f.header.version == vPsb
    var found = false
    for x in w:
      if "PSB" in x: found = true
    check found

  test "effects round trip typed and raw":
    var d = genDoc(cmRgb, stU8, PixelFeatures)
    d.globalLight = GlobalLight(angle: 45, altitude: 25)
    var s1 = newEffect(ekStroke)
    s1.stroke = StrokeFx(common: fxCommon(bmNormal, 1), size: 3, position: spCenter, paint: FxPaint(kind: fpColor, color: rgb(0, 1, 0), scale: 1))
    var s2 = newEffect(ekStroke)
    s2.stroke = StrokeFx(common: fxCommon(bmNormal, 0.5), size: 6, position: spOutside, paint: FxPaint(kind: fpColor, color: rgb(0, 0, 1), scale: 1))
    d.layers[1].effects.items = @[defaultDropShadow(), s1, s2]
    let back = roundtrip(d)
    check back.globalLight == d.globalLight
    let fx = back.layers[1].effects
    check fx.items.len == 3
    check not fx.psdRaw.isNil
    let raw = fx.psdRaw.data
    check findRec(documentToPsd(back), back.layers[1].name).blockOf("lfx2").get == raw
    var edited = back
    edited.layers[1].effects.items[0].shadow.distance = 20
    let data = findRec(documentToPsd(edited), back.layers[1].name).blockOf("lfx2").get
    check data != raw
    let parsed = parseLfx2(data)
    check parsed.isSome and parsed.get[1][0].kind == ekDropShadow and parsed.get[1][0].shadow.distance == 20

  test "group effects use lfxs":
    var d = genDoc(cmRgb, stU8, PixelFeatures)
    var n = -1
    for i, l in d.layers:
      if l.isGroup:
        n = i
        break
    var co = newEffect(ekColorOverlay)
    co.common = fxCommon(bmNormal, 1)
    co.color = rgb(1, 0, 0)
    d.layers[n].effects.items.add co
    let f = documentToPsd(d)
    var rec: LayerRecord
    for r in f.layers:
      if r.name == d.layers[n].name and r.sectionType.isFolder: rec = r
    check rec.blockOf("lfxs").isSome and rec.blockOf("lfx2").isNone
    check psdToDocument(f)[0].layers[n].effects.items.len == 1

  test "clearing effects drops stale blocks":
    var d = genDoc(cmRgb, stU8, PixelFeatures)
    d.layers[1].effects.items.add defaultDropShadow()
    let back = roundtrip(d)
    var cleared = back
    cleared.layers[1].effects = newEffects()
    check findRec(documentToPsd(cleared), back.layers[1].name).blockOf("lfx2").isNone

  test "link groups and effects reference round trip":
    var d = newDocument("l", size(8, 8), cmRgb, stU8)
    let fmt = d.pixelFormat
    var a = rasterLayer("a", fmt)
    a.content.raster.fillRect(rect(0, 0, 4, 4), [1'f32, 0, 0, 1])
    a.linkGroup = some(7'u64)
    a.effects.reference = some((-201.0, 47.5))
    var b = rasterLayer("b", fmt)
    b.linkGroup = some(7'u64)
    var inner = rasterLayer("inner", fmt)
    inner.linkGroup = some(3'u64)
    d.layers = @[a, b, groupLayer("g", @[inner])]
    let back = roundtrip(d)
    check back.layers[0].linkGroup == some(7'u64)
    check back.layers[1].linkGroup == some(7'u64)
    check back.layers[2].linkGroup.isNone
    check back.layers[2].content.group.children[0].linkGroup == some(3'u64)
    check back.layers[0].effects.reference == some((-201.0, 47.5))
    check back.layers[1].effects.reference.isNone
    proc res(bytes: seq[byte]): Option[seq[byte]] =
      let r = parsePsd(bytes).resource(1026)
      if r.isSome: some(r.get.data) else: none(seq[byte])
    check res(exportDoc(back, "x.psd").bytes) == res(exportDoc(d, "x.psd").bytes)
    check res(exportDoc(d, "x.psd").bytes).get == @[0'u8, 7, 0, 7, 0, 0, 0, 3, 0, 0]
    var big = d
    big.layers[0].linkGroup = some(1'u64 shl 40)
    big.layers[1].linkGroup = some(1'u64 shl 40)
    check res(exportDoc(big, "x.psd").bytes).get == @[0'u8, 1, 0, 1, 0, 0, 0, 2, 0, 0]
    var none0 = d
    none0.layers[0].linkGroup = none(uint64)
    none0.layers[1].linkGroup = none(uint64)
    none0.layers[2].content.group.children[0].linkGroup = none(uint64)
    check res(exportDoc(none0, "x.psd").bytes).isNone

  test "channel restrictions round trip as brst":
    var d = newDocument("c", size(8, 8), cmRgb, stU8)
    var a = rasterLayer("a", d.pixelFormat)
    a.content.raster.fillRect(rect(0, 0, 4, 4), [1'f32, 0, 0, 1])
    a.excludedChannels = 0b110
    d.layers = @[a, rasterLayer("b", d.pixelFormat)]
    let f = documentToPsd(d)
    check findRec(f, "a").blockOf("brst").get == @[0'u8, 0, 0, 1, 0, 0, 0, 2]
    check findRec(f, "b").blockOf("brst").isNone
    let back = roundtrip(d)
    check back.layers[0].excludedChannels == 0b110
    check back.layers[1].excludedChannels == 0
    var cleared = back
    cleared.layers[0].excludedChannels = 0
    check findRec(documentToPsd(cleared), "a").blockOf("brst").isNone

  test "advanced blending round trips as tagged blocks":
    for depth in [stU8, stU16, stF32]:
      var d = newDocument("a", size(8, 8), cmRgb, depth)
      var a = rasterLayer("a", d.pixelFormat)
      a.content.raster.fillRect(rect(0, 0, 4, 4), [1'f32, 0, 0, 1])
      let adv = AdvancedBlending(knockout: koDeep, blendInterior: true, blendClipped: false, transparencyShapes: false,
                                 layerMaskHidesEffects: true, vectorMaskHidesEffects: true)
      a.advanced = adv
      var gl = groupLayer("g", @[rasterLayer("c", d.pixelFormat)])
      gl.advanced.knockout = koShallow
      gl.fillOpacity = 128 / 255
      d.layers = @[a, rasterLayer("b", d.pixelFormat), gl]
      let f = documentToPsd(d)
      let ra = findRec(f, "a")
      for (k, v) in [("knko", 2'u8), ("infx", 1'u8), ("clbl", 0'u8), ("tsly", 0'u8), ("lmgm", 1'u8), ("vmgm", 1'u8)]:
        check ra.blockOf(k).get == @[v, 0, 0, 0]
      for k in ["knko", "infx", "clbl", "tsly", "lmgm", "vmgm"]:
        check findRec(f, "b").blockOf(k).isNone
      check findRec(f, "g").blockOf("knko").get == @[1'u8, 0, 0, 0]
      let back = roundtrip(d)
      check back.layers[0].advanced == adv
      check back.layers[1].advanced.isDefault
      check back.layers[2].advanced.knockout == koShallow
      check back.layers[2].fillOpacity == 128'f32 / 255
      var reset = back
      reset.layers[0].advanced = defaultAdvancedBlending()
      let rr = findRec(documentToPsd(reset), "a")
      check rr.blockOf("knko").get == @[0'u8, 0, 0, 0]
      check rr.blockOf("clbl").get == @[1'u8, 0, 0, 0]
      check roundtrip(reset).layers[0].advanced.isDefault

  test "blend if round trips as blending ranges":
    var d = newDocument("b", size(8, 8), cmRgb, stU8)
    var a = rasterLayer("a", d.pixelFormat)
    a.content.raster.fillRect(rect(0, 0, 4, 4), [1'f32, 1, 1, 1])
    var bi = BlendIf()
    bi.set(0, [doc.BlendRange(black: [0'u8, 0], white: [200'u8, 230]), FullRange])
    bi.set(2, [FullRange, doc.BlendRange(black: [64'u8, 64], white: [255'u8, 255])])
    a.blendIf = bi
    var gl = groupLayer("g", @[rasterLayer("c", d.pixelFormat)])
    gl.blendIf.set(1, [doc.BlendRange(black: [1'u8, 2], white: [3'u8, 4]), FullRange])
    d.layers = @[a, rasterLayer("b", d.pixelFormat), gl]
    let f = documentToPsd(d)
    let full = @[0'u8, 0, 255, 255, 0, 0, 255, 255]
    check findRec(f, "a").blendingRanges.data == @[0'u8, 0, 200, 230, 0, 0, 255, 255] & full &
      @[0'u8, 0, 255, 255, 64, 64, 255, 255] & full
    check findRec(f, "b").blendingRanges == fullBlendingRanges(3)
    let back = roundtrip(d)
    check back.layers[0].blendIf == bi
    check back.layers[1].blendIf.isDefault
    check back.layers[2].blendIf.get(1)[0] == doc.BlendRange(black: [1'u8, 2], white: [3'u8, 4])

# ---------------------------------------------------------------- formats

suite "io: flat formats and facade":
  test "png round trip keeps pixels and transparency":
    var d = newDocument("p", size(5, 4), cmRgb, stU8)
    var l = rasterLayer("Layer 0", d.pixelFormat)
    l.content.raster.fillRect(rect(1, 1, 4, 3), [1'f32, 0.5, 0, 1])
    d.layers.add l
    let r = exportDoc(d, "x.png")
    let back = importDoc("x.png", r.bytes).document
    check back.layers.len == 1
    check back.layers[0].surface.pixel(2, 2) == @[1'f32, 128 / 255, 0, 1]
    check back.layers[0].surface.pixel(0, 0)[3] == 0

  test "opaque export opens as a locked background":
    var d = withBackground("b", size(3, 3), cmRgb, stU8, rgb(0.2, 0.4, 0.6))
    let back = importDoc("x.png", exportDoc(d, "x.png").bytes).document
    check back.layers[0].name == "Background"
    check back.layers[0].locks.transparency

  test "unknown export extension fails":
    expect IoFailure:
      discard exportDoc(newDocument("e", size(2, 2), cmRgb, stU8), "x.nope")

  test "pdn and affinity export are refused":
    let d = newDocument("e", size(2, 2), cmRgb, stU8)
    for ext in ["pdn", "afphoto"]:
      expect IoFailure:
        discard exportDoc(d, ext)

  test "pcraft through the facade":
    let d = genDoc(cmRgb, stU8, PixelFeatures)
    let back = importDoc("x.pcraft", exportDoc(d, "pcraft").bytes).document
    check back.layers.len == d.layers.len

  test "tiff with layers reopens layered":
    var o = defaultExportOptions()
    o.tiffLayers = true
    let d = genDoc(cmRgb, stU8, PixelFeatures)
    let r = exportDoc(d, "x.tif", o)
    let back = importDoc("x.tif", r.bytes).document
    check back.layers.len == d.layers.len
    for i in 0 ..< d.layers.len: check back.layers[i].name == d.layers[i].name

  test "multichannel round trip":
    var d = newDocument("m", size(6, 5), cmMultichannel, stU8)
    var a = newAlphaChannel("Cyan", maskSurface(stU8, d.bounds, 0, 4))
    a.spot = some((Black, 0'f32))
    d.channels.add a
    let back = roundtrip(d)
    check back.mode == cmMultichannel
    check back.channels.len == 1
    check back.channels[0].name == "Cyan"
    surfaceEq(back.channels[0].surface, d.channels[0].surface, "channel")

  test "lossy utf8 and extensions":
    check lossyUtf8([byte('a'), 0xFF, byte('b')]) == "a�b"
    check lossyUtf8(cast[seq[byte]]("é")) == "é"
    check hasExtension("dir/a.PSD", "psd")
    check not hasExtension(".psd", "psd")
    check extension("Foo.TIF") == "tif"

  test "linked files encode and parse":
    let f = LinkedFile(uuid: "abc", fileName: "pic.png", bytes: @[0x89'u8, byte('P'), byte('N'), byte('G'), 1, 2])
    let enc = encodeLinkedFile(f)
    check enc.len mod 4 == 0
    let back = parseLinkedFiles(enc)
    check back == @[f]
    check blockUuids(enc) == @["abc"]
    let copy = copyItem(enc, "abc", "xyz")
    check copy.isSome and parseLinkedFiles(copy.get)[0].uuid == "xyz"
    check rebuildBlock(enc, proc(u: string): bool = false, []).get.len == 0
    check rebuildBlock(enc, proc(u: string): bool = true, []).isNone

  test "export never panics on odd documents":
    var d = newDocument("odd", size(3, 2), cmRgb, stU8)
    d.layers.add groupLayer("g", @[])
    d.layers.add newLayer("adj", adjustmentContent(newAdjustment(akColorLookup)))
    for ext in ["psd", "psb", "png", "tif", "jpg", "pcraft", "bmp", "tga", "webp", "exr", "qoi"]:
      discard exportDoc(d, ext)
