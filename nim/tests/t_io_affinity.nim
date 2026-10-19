## Affinity documents through `io` (`io/tests/affinity.rs`, `affinity_artboards.rs`): native import
## (layers, artboards, shapes, paints, masks) and the embedded preview only as a warned fallback
## that keeps its pixels and alpha; never written back.

import std/[unittest, options, strutils, random]
import photocraft/[geom, color, doc, io, codecs, raster, zlib]
import photocraft/affinity/synth

proc be32(v: uint32): seq[byte] = @[byte(v shr 24), byte((v shr 16) and 0xFF), byte((v shr 8) and 0xFF), byte(v and 0xFF)]
proc le32(v: uint32): seq[byte] = @[byte(v and 0xFF), byte((v shr 8) and 0xFF), byte((v shr 16) and 0xFF), byte(v shr 24)]
proc bs(s: string): seq[byte] =
  for c in s: result.add byte(c)
proc put(b: var seq[byte]; at: int; v: openArray[byte]) =
  for i, x in v: b[at + i] = x
proc splice(b: var seq[byte]; at: int; v: openArray[byte]) =
  b = b[0 ..< at] & @v & b[at ..< b.len]

proc png(): seq[byte] =
  let img = imageFromRaw(2, 1, clRgba, isU8, @[220'u8, 40, 60, 255, 0, 0, 0, 0])
  encode(img, fmtPng)

proc chunk(kind: string; data: openArray[byte]): seq[byte] =
  let tagged = bs(kind) & @data
  be32(uint32(data.len)) & tagged & be32(crc32(tagged))

proc file(png: seq[byte]): seq[byte] =
  ## A synthetic record envelope, not an Affinity document writer (there is no object graph).
  result = newSeq[byte](72)
  result.put(0, bs("\x00\xffKA"))
  result.put(4, [12'u8, 0])
  result.put(8, bs("nsrP"))
  result.put(12, bs("#Inf"))
  result.put(24, [72'u8, 0, 0, 0, 0, 0, 0, 0])
  result.put(64, bs("Prot"))
  result.add bs("\xff\xff\xff\xffThmb")
  result.add le32(1)
  result.add le32(uint32(png.len + 13))
  result.add le32(29)
  result.add le32(0)
  result.add le32(uint32(png.len))
  result.add 1
  result.add png

proc err(name: string; b: seq[byte]): string =
  try:
    discard importDoc(name, b)
    ""
  except IoFailure as e: e.msg

proc withChunk(encoded: seq[byte]; c: seq[byte]): seq[byte] =
  result = encoded
  result.splice(result.len - 12, c)

suite "affinity preview fallback":
  test "the preview is named and warns at its actual size":
    let r = importDoc("art.af", file(png()))
    check (r.document.size.width, r.document.size.height) == (2, 1)
    check r.document.layers.len == 1
    check r.document.layers[0].name == "Affinity preview"
    check r.sourceReadOnly and r.previewOnly
    check "2×1" in r.warnings[0]
    check "native Affinity document could not be read" in r.warnings[0] and "no layers" in r.warnings[0]
    let outp = exportDoc(r.document, "copy.png")
    check decode(outp.bytes).data == @[220'u8, 40, 60, 255, 0, 0, 0, 0]
    let native = exportDoc(r.document, "copy.pcraft")
    let back = importDoc("copy.pcraft", native.bytes)
    check back.document.size == r.document.size
    check not back.sourceReadOnly and not back.previewOnly
    for ext in ["af", "afphoto", "afdesign", "afpub"]:
      try:
        discard exportDoc(r.document, ext)
        check false
      except IoFailure as e: check "Affinity export" in e.msg

  test "magic wins over the name and renamed legacy files are handled":
    check importDoc("renamed.png", file(png())).sourceReadOnly
    check importDoc("unknown", file(png())).sourceReadOnly
    check err("fake.af", png()) != ""
    # Affinity 1 and 2 (container versions 8 to 11) store the same preview record.
    var legacy = file(png())
    legacy.put(4, [10'u8, 0])
    check importDoc("legacy.afphoto", legacy).previewOnly
    legacy.put(4, [13'u8, 0])
    check "unknown container version" in err("future.af", legacy)

  test "the preview decoder keeps 16-bit gray and alpha":
    var data: seq[byte]
    for v in [12345'u16, 65535, 0, 0]: data.add [byte(v and 0xFF), byte(v shr 8)]
    let img = imageFromRaw(2, 1, clGrayA, isU16, data)
    let r = importDoc("gray.af", file(encode(img, fmtPng)))
    check r.document.depth == stU16
    check r.document.mode == cmGrayscale
    let decoded = decode(exportDoc(r.document, "copy.png").bytes)
    check decoded.sample == isU16
    check decoded.data == img.data

  test "previewless files fail with a reason":
    var b = file(png())
    b.put(24, newSeq[byte](8))
    check "no embedded preview" in err("no-preview.af", b)

  test "corruption and every truncation fail without a document":
    let b = file(png())
    for e in 0 ..< b.len: check err("bad.af", b[0 ..< e]) != ""
    var badCrc = b
    badCrc[130] = badCrc[130] xor 1
    check err("bad.af", badCrc) != ""

  test "CRC failures after IDAT cannot pass as partial PNGs":
    var badIend = file(png())
    badIend[^1] = badIend[^1] xor 1
    check "PNG chunk checksum" in err("bad-iend.af", badIend)
    let tagged = bs("tEXtnote\0preview")
    let c = be32(uint32(tagged.len - 4)) & tagged & be32(crc32(tagged))
    var encoded = withChunk(png(), c)
    discard importDoc("valid-ancillary.af", file(encoded))
    encoded[encoded.len - 12 - 1] = encoded[encoded.len - 12 - 1] xor 1
    check "PNG chunk checksum" in err("bad-ancillary.af", file(encoded))

  test "compressed metadata and APNG fail before reaching the decoder":
    for kind in ["zTXt", "iTXt", "iCCP", "acTL", "fcTL", "fdAT"]:
      let tagged = bs(kind) & bs("preview\0\0\x78\x9c")
      let c = be32(uint32(tagged.len - 4)) & tagged & be32(crc32(tagged))
      let e = err("unsupported.af", file(withChunk(png(), c)))
      check "unsupported Affinity file" in e
      check "PNG metadata" in e or "animated PNG" in e

  test "many compressed text chunks cannot bypass the memory budget":
    var x = newSeq[byte](1024 * 1024)
    for v in x.mitems: v = byte('x')
    let compressed = zlibCompress(x, 9)
    check compressed.len < 2048
    let tagged = bs("zTXtnote\0\0") & compressed
    let c = be32(uint32(tagged.len - 4)) & tagged & be32(crc32(tagged))
    var many: seq[byte]
    for _ in 0 ..< 1024: many.add c
    check "compressed PNG metadata" in err("text-bomb.af", file(withChunk(png(), many)))

  test "invalid critical chunks after IDAT fail":
    let original = png()
    for trailing in [chunk("IHDR", original[16 ..< 29]), chunk("PLTE", [0'u8, 0, 0]), chunk("ABCD", [])]:
      check "Affinity file" in err("bad-order.af", file(withChunk(original, trailing)))
    let interrupted = chunk("tEXt", bs("note\0preview")) & chunk("IDAT", [])
    check "non-contiguous PNG image data" in err("split-idat.af", file(withChunk(original, interrupted)))

  test "mutated previews never crash":
    var rng = initRand(0xAF5)
    for _ in 0 ..< 128:
      var b = file(png())
      for _ in 0 ..< rng.rand(15):
        let i = rng.rand(299)
        if i < b.len: b[i] = byte(rng.rand(255))
      discard err("mutated.af", b)

# ---------------------------------------------------------------- native documents

proc rgbaF(r, g, b, a: float32): F =
  var v: seq[byte]
  for x in [r, g, b, a]: v.add le32(cast[uint32](x))
  FStruct(v)

proc solid(id: uint32; c: F): F =
  FDef(id, @[tag("FDsc")], @[(tag("FDeF"), FDef(id + 1, @[tag("FilS")],
    @[(tag("Colr"), FDef(id + 2, @[tag("RGBA")], @[(tag("_col"), c)]))]))])

proc node(x, y: float64): seq[byte] =
  for v in [x, y]:
    let u = cast[uint64](v)
    for k in 0 ..< 8: result.add byte((u shr (8 * k)) and 0xFF)
  result.add [1'u8, 0]

proc square(s, o: float64): seq[seq[byte]] =
  @[node(o, o), node(o + s, o), node(o + s, o + s), node(o, o + s), node(o, o)]

proc nativeDocument(m: Method): seq[byte] =
  ## A version-12 document at 144 ppi, 200×100: an artboard "Board" moved by (20, 10) holding a
  ## red ring (two subpaths, filled even-odd) with a 4 px blue stroke.
  let ring = FDef(20, @[tag("PCrv")], @[
    (tag("Desc"), FStr("Ring")),
    (tag("Crvs"), FObj(tag("PCvD"), @[(tag("Data"), FPos(@[FU8(0), FU32(2), FBool(true), FRecords(18, square(60.0, 0.0)),
                                                         FBool(true), FRecords(18, square(20.0, 20.0))]))])),
    (tag("BFFl"), FShared(@[solid(21, rgbaF(1, 0, 0, 1))])),
    (tag("LIFl"), FShared(@[solid(24, rgbaF(0, 0, 1, 1))])),
    (tag("LILn"), FShared(@[FDef(27, @[tag("LDsc")], @[(tag("LDeL"), FDef(28, @[tag("LSty")], @[(tag("Wght"), FF64(4.0))]))])])),
  ])
  let board = FDef(10, @[tag("ShpN")], @[
    (tag("Desc"), FStr("Board")),
    (tag("ABEn"), FBool(true)),
    (tag("Shpe"), FDef(11, @[tag("ShNR")], @[])),
    (tag("ShpB"), FF64s([0.0, 0.0, 120.0, 80.0])),
    (tag("Xfrm"), FF64s([1.0, 0.0, 20.0, 0.0, 1.0, 10.0])),
    (tag("BFFl"), FShared(@[solid(12, rgbaF(1, 1, 1, 1))])),
    (tag("Chld"), FShared(@[ring])),
  ])
  let spread = FDef(2, @[tag("Sprd")], @[(tag("SprB"), FF64s([0.0, 0.0, 200.0, 100.0])), (tag("SprT"), FBool(true)),
                                         (tag("Chld"), FShared(@[board]))])
  let d = stream(@[
    (tag("UVCn"), FObj(tag("UVCn"), @[(tag("UPPI"), FF64(144.0))])),
    (tag("DocR"), FDef(1, @[tag("DocN")], @[(tag("Chld"), FShared(@[spread]))])),
  ])
  container([("doc.dat", d, m)], png(), true)

suite "affinity native import":
  test "artboards, shapes and paints import as editable layers":
    for m in [mStored, mZlib, mZstd]:
      let r = importDoc("native.af", nativeDocument(m))
      check r.sourceReadOnly and not r.previewOnly
      let d = r.document
      check (d.size.width, d.size.height, d.resolutionDpi) == (200, 100, 144'f32)
      let board = d.layers[0]
      check board.name == "Board"
      check board.content.kind == lkGroup
      let ab = board.content.group.artboard.get
      check (ab.rect.x0, ab.rect.y0, ab.rect.x1, ab.rect.y1) == (20, 10, 140, 90)
      let ring = board.content.group.children[0]
      check ring.name == "Ring"
      check ring.content.kind == lkShape
      let sh = ring.content.shape
      check sh.path.subpaths.len == 2
      check sh.path.subpaths[0].knots.len == 4 # the closing duplicate knot is merged
      check sh.path.subpaths[0].knots[0].anchor == point(20.0, 10.0)
      check sh.stroke.get.width == 4'f32
      # The cache shows the ring: red on it, empty in its hole.
      let cache = sh.cache.get
      check cache.toInterleaved(rect(30, 20, 31, 21)) == @[255'u8, 0, 0, 255]
      check cache.toInterleaved(rect(50, 40, 51, 41))[3] == 0 # even-odd hole
      # Saved as a native PhotoCraft document and opened again.
      let saved = exportDoc(d, "copy.pcraft")
      check importDoc("copy.pcraft", saved.bytes).document.layers.len == d.layers.len
      discard exportDoc(d, "copy.psd")
      expect IoFailure: discard exportDoc(d, "copy.af")

  test "a damaged native document falls back to its preview with the reason":
    var bytes = nativeDocument(mZlib)
    bytes[80] = bytes[80] xor 0xFF
    let r = importDoc("broken.af", bytes)
    check r.previewOnly
    check "could not be read (damaged Affinity file" in r.warnings[0]

  test "mutated native documents never crash":
    var rng = initRand(0xAF6)
    for k in 0 ..< 256:
      var b = nativeDocument(if k mod 2 == 0: mStored else: mZstd)
      for _ in 0 ..< 1 + rng.rand(14): b[rng.rand(2047) mod b.len] = byte(rng.rand(255))
      discard err("mutated.af", b)

# ---------------------------------------------------------------- artboards (support/current_affinity.rs)

type Marker = enum mkLegacy, mkCurrent, mkOtherProperties

proc redSolid(id: uint32): F = solid(id, rgbaF(1, 0, 0, 1))

proc board(id: uint32; x: float64; marker: Marker; curve: bool): F =
  let child = FDef(id + 10, @[tag("ShpN")], @[
    (tag("Desc"), FStr("Overflow")),
    (tag("Shpe"), FDef(id + 11, @[tag("ShNR")], @[])),
    (tag("ShpB"), FF64s([-4.0, -4.0, 20.0, 20.0])),
    (tag("BFFl"), FShared(@[redSolid(id + 12)])),
  ])
  var fields = @[
    (tag("Desc"), FStr("Board " & $id)),
    (tag("Shpe"), FDef(id + 1, @[tag("ShNR")], @[])),
    (tag("ShpB"), FF64s([0.0, 0.0, 16.0, 16.0])),
    (tag("Xfrm"), FF64s([1.0, 0.0, x, 0.0, 1.0, 0.0])),
    (tag("Chld"), FShared(@[child])),
  ]
  case marker
  of mkLegacy: fields.add (tag("ABEn"), FBool(true))
  of mkCurrent, mkOtherProperties:
    fields.add (tag("ABEn"), FBool(false))
    fields.add (tag("phrp"), FDef(id + 2, @[tag(if marker == mkCurrent: "aprp" else: "prps")], @[]))
  FDef(id, @[tag(if curve: "PCrv" else: "ShpN")], fields)

proc boardsDocument(marker: Marker; curve: bool; m: Method): seq[byte] =
  let spread = FDef(2, @[tag("Sprd")], @[
    (tag("SprB"), FF64s([0.0, 0.0, 40.0, 16.0])),
    (tag("SprT"), FBool(true)),
    (tag("Chld"), FShared(@[board(100, 0.0, marker, curve), board(200, 24.0, marker, curve)])),
  ])
  let s = stream(@[
    (tag("UVCn"), FObj(tag("UVCn"), @[(tag("UPPI"), FF64(144.0))])),
    (tag("DocR"), FDef(1, @[tag("DocN")], @[(tag("Chld"), FShared(@[spread]))])),
  ])
  container([("doc.dat", s, m)])

proc boards(d: Document): seq[(string, Rect)] =
  for (_, name, a) in d.artboards: result.add (name, a.rect)

proc pixels(d: Document): seq[byte] = decode(exportDoc(d, "boards.png").bytes).data

suite "affinity artboards":
  test "current property artboards keep bounds, clipping and round trips":
    for marker in [mkLegacy, mkCurrent]:
      for m in [mStored, mZlib, mZstd]:
        let loaded = importDoc("synthetic.af", boardsDocument(marker, false, m))
        check not loaded.previewOnly and loaded.sourceReadOnly
        let d = loaded.document
        check boards(d) == @[("Board 100", rect(0, 0, 16, 16)), ("Board 200", rect(24, 0, 40, 16))]
        let expected = pixels(d)
        check expected.len == 40 * 16 * 4
        var clipped = true
        for i in 0 ..< expected.len div 4:
          let x = i mod 40
          let want = if x >= 16 and x < 24: [0'u8, 0, 0, 0] else: [255'u8, 0, 0, 255]
          if expected[4 * i ..< 4 * i + 4] != @want: clipped = false
        check clipped # overflow must be clipped at the board edges
        for name in ["copy.pcraft", "copy.psd"]:
          let reopened = importDoc(name, exportDoc(d, name).bytes).document
          check boards(reopened) == boards(d)
          check pixels(reopened) == expected

  test "current properties mark converted curve artboards too":
    let loaded = importDoc("curves.af", boardsDocument(mkCurrent, true, mStored))
    check loaded.document.artboards.len == 2
    var found = false
    for w in loaded.warnings:
      if "converted artboard outline" in w: found = true
    check found

  test "other properties do not turn ordinary shapes into artboards":
    check not importDoc("shapes.af", boardsDocument(mkOtherProperties, false, mStored)).document.hasArtboards
