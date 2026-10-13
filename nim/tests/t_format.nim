## Native format tests (ported from crates/format/tests): doc → .pcraft → doc equality in every
## mode and depth, zip and directory bundles, incremental saves, ids, previews, corruption,
## limits, and crash-recovery autosaves.
import std/[unittest, os, options, json, strutils, tables]
import photocraft/[doc, format, zstd, zlib, raster]

type Rng = object
  s: uint64
proc next(r: var Rng): uint64 =
  r.s = r.s + 0x9E37_79B9_7F4A_7C15'u64
  var z = r.s
  z = (z xor (z shr 30)) * 0xBF58_476D_1CE4_E5B9'u64
  z = (z xor (z shr 27)) * 0x94D0_49BB_1331_11EB'u64
  z xor (z shr 31)
proc f(r: var Rng): float32 = float32(r.next shr 40) / float32(1'u64 shl 24)

proc scribble(s: var Surface; seed: uint64; hdr: bool) =
  var r = Rng(s: seed)
  let ch = s.channels
  for (x0, y0) in [(-12, -7), (0, 0), (250, 250), (600, 30)]:
    for dy in 0 ..< 9:
      for dx in 0 ..< 11:
        var px = newSeq[float32](ch)
        for c in 0 ..< ch: px[c] = (if hdr: r.f * 4 - 1 else: r.f)
        s.writePixel(x0 + dx, y0 + dy, px)

proc blob(seed: uint8; n: int): SharedBytes =
  result = SharedBytes()
  for i in 0 ..< n: result.data.add byte((i * 31 + int(seed)) and 0xff)

proc richDoc(mode: ColorMode; depth: SampleType): Document =
  let hdr = depth == stF32
  result = withBackground("Rich", size(300, 280), mode, depth, White)
  result.resolutionDpi = 300
  result.iccProfile = blob(1, 500)
  let pf = result.pixelFormat
  var paint = rasterLayer("Paint", pf)
  scribble(paint.content.raster, 1, hdr)
  paint.blend = bmMultiply
  paint.opacity = 0.75
  paint.fillOpacity = 0.5
  paint.clipped = true
  paint.label = lcViolet
  paint.locks.pixels = true
  var mask = hideAllMask()
  scribble(mask.surface, 2, false)
  mask.density = 0.8
  mask.feather = 3.5
  paint.mask = some(mask)
  var co = newEffect(ekColorOverlay)
  co.color = rgb(1, 0.2, 0.1)
  paint.effects.items.add co
  paint.effects.psdRaw = blob(3, 64)
  paint.psdBlocks.add PsdBlock(key: "vmsk", data: blob(4, 40))
  paint.psdId = some(42'u32)
  var inner = rasterLayer("Inner", pf)
  scribble(inner.content.raster, 6, hdr)
  var adj = newLayer("Hue/Sat", adjustmentContent(newAdjustment(akHueSaturation)))
  adj.visible = false
  var group = groupLayer("Group", @[groupLayer("Nested", @[inner]), adj])
  group.content.group.expanded = false
  var fill = newLayer("Fill", fillContent(solidFill(rgba(0.1, 0.2, 0.3, 0.9))))
  var fc = newSurface(pf)
  scribble(fc, 7, false)
  fill.fillCache = some(FillCache(fill: fill.content.fill, surface: fc))
  let grad = newLayer("Gradient", fillContent(gradientFill(@[(0'f32, Black), (1'f32, White)], 45, 1.5, gsLinear, true)))
  var cache = newSurface(pf)
  scribble(cache, 8, false)
  var tl = TextLayer(text: "Hello ✓ world", fontFamily: "Inter", sizePt: 24, color: rgb(0.2, 0.4, 0.6),
                     transform: translateAffine(10.5, 20.25), cache: some(cache), psdRaw: blob(9, 100),
                     shape: TextShape(kind: tsBox, x: 1, y: 2, width: 100, height: 50),
                     orientation: orVertical, antialias: aaCrisp,
                     warp: some(TextWarp(style: "warpArc", value: 25)))
  var cs = defaultCharStyle()
  cs.kerning = kOff
  tl.runs = @[TextRun(len: 1, style: cs), TextRun(len: 14, style: defaultCharStyle())]
  tl.paragraphs = @[ParagraphRun(len: 15, style: defaultParagraphStyle())]
  let text = newLayer("Type", LayerContent(kind: lkText, text: tl))
  var stroke = defaultShapeStroke()
  stroke.width = 5.5
  stroke.dashes = @[0'f32, 2, 4, 1]
  let shape = newLayer("Shape", shapeContent(ShapeLayer(path: newPath(@[polygon([(10.0, 10.0), (40.0, 10.0), (25.0, 40.0)])]),
                                                        fill: some(solidFill(rgb(0, 1, 0))), stroke: some(stroke))))
  var so = SmartObject(contentsId: freshContentsId(), source: SmartSource(kind: ssEmbedded, fileName: "a.png", bytes: blob(10, 300)),
                       transform: translateAffine(3, 4), filtersEnabled: true)
  let smart = newLayer("Smart", LayerContent(kind: lkSmart, smart: so))
  result.layers.add @[paint, group, fill, grad, text, shape, smart]
  var ch = newAlphaChannel("Alpha 1", newSurface(pixelFormat(cmGrayscale, depth, false)))
  scribble(ch.surface, 11, false)
  result.channels.add ch
  result.guides.horizontal = @[10.5, 20.0]
  result.metadata.xmp = some("<x:xmpmeta/>")
  result.metadata.exif = blob(12, 50)
  result.metadata.psdResources.add PsdResource(id: 1005, name: "", data: blob(13, 16))
  result.metadata.psdGlobalBlocks.add PsdGlobalBlock(signature: "8BIM", key: "Patt", data: blob(14, 8))
  var pat = newSurface(Rgba8)
  scribble(pat, 15, false)
  result.patterns.add Pattern(id: "p1", name: "Dots", width: 8, height: 8, surface: pat)
  result.layerComps.add LayerComp(id: 1, name: "A", applyVisibility: true,
                                  states: @[CompLayerState(layer: paint.id, visible: some(false))])

proc tempDir(name: string): string =
  result = getTempDir() / ("pcraft-nim-" & name & "-" & $getCurrentProcessId())
  removeDir(result)
  createDir(result)

suite "format":
  test "zip round trip in every mode and depth":
    for mode in [cmRgb, cmGrayscale, cmCmyk, cmLab]:
      for depth in [stU8, stU16, stF32]:
        let d = richDoc(mode, depth)
        let bytes = saveToBytes(d)
        check isPcraft(bytes)
        let back = loadFromBytes(bytes)
        check back == d

  test "directory bundles, save_path, incremental saves and garbage collection":
    let d = richDoc(cmRgb, stU8)
    let dir = tempDir("dir")
    let w = newPcraftWriter()
    let s1 = w.saveDir(d, dir / "a.pcraft")
    check fileExists(dir / "a.pcraft" / "manifest.json")
    check loadPath(dir / "a.pcraft") == d
    check s1.tilesWritten == s1.tilesTotal
    var d2 = d
    d2.layers[1].content.raster.writePixel(1, 1, [0.5'f32, 0.5, 0.5, 1])
    let s2 = w.saveDir(d2, dir / "a.pcraft")
    check s2.tilesWritten == 1
    check s2.objectsRemoved == 1
    check loadPath(dir / "a.pcraft") == d2
    discard newPcraftWriter().savePath(d, dir / "b.pcraft")
    check fileExists(dir / "b.pcraft")
    check loadPath(dir / "b.pcraft") == d
    let (_, z1) = w.saveZip(d)
    let (_, z2) = w.saveZip(d)
    check z1.tilesWritten == z1.tilesTotal and z2.tilesReused == z2.tilesTotal
    removeDir(dir)

  test "identical tiles are stored once; ids are preserved or fresh":
    var d = newDocument("Same", size(1024, 1024), cmRgb, stU8)
    var l = rasterLayer("Solid", Rgba8)
    l.content.raster.fillRect(rect(0, 0, 1024, 1024), [1'f32, 0, 0, 1])
    d.layers.add l
    let (bytes, st) = newPcraftWriter().saveZip(d)
    check st.tilesTotal == 1
    let back = loadFromBytes(bytes)
    check back.layers[0].id == l.id
    var fresh = defaultLoadOptions()
    fresh.preserveIds = false
    check loadFromBytes(bytes, fresh).layers[0].id != l.id

  test "previews are embedded and readable":
    var img = newRgba8Image(4, 3)
    for v in img.pixels.mitems: v = 200
    let bytes = saveToBytes(richDoc(cmRgb, stU8), SaveOptions(thumbnail: some(img), composite: some(img)))
    let t = readThumbnail(bytes)
    check t.isSome and t.get.len > 8 and t.get[1] == byte('P')

  test "corruption, truncation and limits fail cleanly":
    let bytes = saveToBytes(richDoc(cmRgb, stU8))
    for n in [0, 10, 100, bytes.len div 2, bytes.len - 1]:
      expect CatchableError: discard loadFromBytes(bytes[0 ..< n])
    var flips = 0
    for i in countup(0, bytes.len - 1, max(bytes.len div 97, 1)):
      var b = bytes
      b[i] = b[i] xor 0x40
      try:
        if loadFromBytes(b) != richDoc(cmRgb, stU8): inc flips
      except CatchableError: inc flips
    check flips > 0
    # a newer format is refused
    var z = newZipWriter()
    z.add("manifest.json", toBytes("""{"format_version": 99, "generator": "x", "document": {}}"""))
    try:
      discard loadFromBytes(z.finish)
      check false
    except FormatError as e:
      check e.kind == feTooNew
    # deep JSON is refused without recursion
    var z2 = newZipWriter()
    z2.add("manifest.json", toBytes(repeat("[", 10000)))
    expect FormatError: discard loadFromBytes(z2.finish)
    var small = defaultLoadOptions()
    small.maxManifestBytes = 100
    expect FormatError: discard loadFromBytes(bytes, small)
    var tiny = defaultLoadOptions()
    tiny.maxTotalBytes = 1000
    expect FormatError: discard loadFromBytes(bytes, tiny)

  test "non-finite values are refused at save":
    var d = richDoc(cmRgb, stU8)
    d.layers[1].opacity = NaN
    try:
      discard saveToBytes(d)
      check false
    except FormatError as e:
      check e.kind == feNonFinite
      check "opacity" in e.path

  test "autosave then recover, discard":
    let dir = tempDir("recovery")
    let a = newAutosaver(dir, "doc 1")
    let d = richDoc(cmRgb, stU8)
    check a.requestChecked(d, 3, some("/tmp/x.psd"))[0]
    let entries = listRecovery(dir)
    check entries.len == 1
    check entries[0].info.revision == 3 and entries[0].info.key == "doc_1"
    check recover(entries[0]) == d
    discardRecovery(dir, entries[0])
    check listRecovery(dir).len == 0
    writeFile(dir / "junk.json", "{not json")
    check listRecovery(dir).len == 0
    removeDir(dir)
