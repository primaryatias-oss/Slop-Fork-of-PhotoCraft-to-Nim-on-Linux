## Affinity reader on synthetic inputs (`affinity/src/tests.rs`): the indexed preview, the archive,
## the object stream and the document model. Every failure must be an `AffinityError`; anything
## else (an IndexDefect, an overflow) fails the test.

import std/[unittest, options, random, strutils]
import photocraft/affinity
import photocraft/affinity/synth
import photocraft/zlib

proc be32(v: uint32): seq[byte] = @[byte(v shr 24), byte((v shr 16) and 0xFF), byte((v shr 8) and 0xFF), byte(v and 0xFF)]
proc le32(v: uint32): seq[byte] = @[byte(v and 0xFF), byte((v shr 8) and 0xFF), byte((v shr 16) and 0xFF), byte(v shr 24)]
proc le64(v: uint64): seq[byte] =
  for k in 0 ..< 8: result.add byte((v shr (8 * k)) and 0xFF)
proc bytesOf(s: string): seq[byte] =
  for c in s: result.add byte(c)
proc put(b: var seq[byte]; at: int; v: openArray[byte]) =
  for i, x in v: b[at + i] = x
proc splice(b: var seq[byte]; at: int; v: openArray[byte]) =
  b = b[0 ..< at] & @v & b[at ..< b.len]

proc fixture(): seq[byte] =
  var png = bytesOf("\x89PNG\r\n\x1a\n")
  png.add be32(13)
  png.add bytesOf("IHDR")
  png.add be32(2)
  png.add be32(1)
  png.add [8'u8, 6, 0, 0, 0]
  png.add be32(crc32(png[12 .. ^1]))
  png.add [0'u8, 0, 0, 0]
  png.add bytesOf("IDAT")
  png.add be32(crc32(bytesOf("IDAT")))
  png.add [0'u8, 0, 0, 0]
  png.add bytesOf("IEND")
  png.add be32(crc32(bytesOf("IEND")))
  var b = newSeq[byte](72)
  b.put(0, bytesOf(Magic))
  b.put(4, [12'u8, 0])
  b.put(8, bytesOf("nsrP"))
  b.put(12, bytesOf("#Inf"))
  b.put(24, le64(72))
  b.put(64, bytesOf("Prot"))
  b.add bytesOf("\xff\xff\xff\xffThmb")
  b.add le32(1)
  b.add le32(uint32(png.len + 13))
  b.add le32(29)
  b.add le32(0)
  b.add le32(uint32(png.len))
  b.add 1
  b.add png
  b

proc previewOk(b: openArray[byte]): bool =
  try:
    discard preview(b)
    true
  except AffinityError: false

proc previewErr(b: openArray[byte]): ref AffinityError =
  try: discard preview(b)
  except AffinityError as e: return e
  nil

suite "affinity preview":
  test "indexed preview and header":
    let b = fixture()
    check header(b).version == 12
    let p = preview(b)
    check (p.width, p.height) == (2'u32, 1'u32)
    check p.png(b) == b[101 .. ^1]

  test "every truncation is rejected":
    let b = fixture()
    for e in 0 ..< b.len: check not previewOk(b[0 ..< e])

  test "rejects assets, versions, offsets, lengths and bombs":
    let b = fixture()
    for (offset, value) in [(4, le64(13)), (8, bytesOf("urBR")), (24, le64(high(uint64))), (24, le64(1)),
                            (24, le64(0)), (84, le32(high(uint32))), (88, le32(30)), (96, le32(high(uint32))),
                            (117, be32(5000))]:
      var broken = b
      broken.put(offset, value)
      check not previewOk(broken)
    # Versions 8 to 11 store the same record (checked on public Affinity 1 and 2 files).
    for version in 8 .. 11:
      var legacy = b
      legacy.put(4, [byte(version), 0])
      check previewOk(legacy)
    # Flags 4 and 8 occur in real files; the low two bits mark other container variants.
    for (flags, ok) in [(4, true), (8, true), (1, false), (2, false)]:
      var variant = b
      variant.put(6, [byte(flags), 0])
      check previewOk(variant) == ok
    var old = b
    old.put(4, [7'u8, 0])
    check previewErr(old).kind == afUnsupported

  test "an unindexed PNG or resource is never used":
    var b = fixture()
    b.put(24, le64(0))
    check not previewOk(b)
    b.put(24, le64(72))
    b.put(76, bytesOf("Meta"))
    check not previewOk(b)

  test "every chunk checksum including the end is checked":
    let b = fixture()
    for at in [130, b.len - 1]:
      var damaged = b
      damaged[at] = damaged[at] xor 1
      let e = previewErr(damaged)
      check e.kind == afMalformed and e.detail == "PNG chunk checksum"
    var ancillary = @[0'u8, 0, 0, 0] & bytesOf("tEXt") & be32(crc32(bytesOf("tEXt")))
    var with = b
    let at = b.len - 12
    with.splice(at, ancillary)
    let pngLength = uint32(with.len - 101)
    with.put(84, le32(pngLength + 13))
    with.put(96, le32(pngLength))
    check previewOk(with)
    with[at + 8] = with[at + 8] xor 1
    check previewErr(with).detail == "PNG chunk checksum"

  test "compressed metadata and animation are not preview support":
    for kind in ["zTXt", "iTXt", "iCCP", "acTL", "fcTL", "fdAT"]:
      let extra = @[0'u8, 0, 0, 0] & bytesOf(kind) & be32(crc32(bytesOf(kind)))
      var b = fixture()
      let at = b.len - 12
      b.splice(at, extra)
      let length = uint32(b.len - 101)
      b.put(84, le32(length + 13))
      b.put(96, le32(length))
      check previewErr(b).kind == afUnsupported

  test "critical chunks follow a single complete PNG layout":
    proc withChunks(kinds: openArray[string]): seq[byte] =
      let original = fixture()
      var b = original[0 ..< 134] # file, record and complete first IHDR
      for kind in kinds:
        b.add [0'u8, 0, 0, 0]
        b.add bytesOf(kind)
        b.add be32(crc32(bytesOf(kind)))
      let length = uint32(b.len - 101)
      b.put(84, le32(length + 13))
      b.put(96, le32(length))
      b
    check previewOk(withChunks(["IDAT", "IDAT", "IEND"]))
    # An otherwise valid unknown ancillary chunk with its reserved bit set is accepted.
    check previewOk(withChunks(["abct", "IDAT", "IEND"]))
    for kinds in [@["IEND"], @["IDAT", "IHDR", "IEND"], @["PLTE", "PLTE", "IDAT", "IEND"], @["IDAT", "PLTE", "IEND"],
                  @["IDAT", "tEXt", "IDAT", "IEND"], @["ABCD", "IDAT", "IEND"], @["IDAT", "IEND", "tEXt"]]:
      check not previewOk(withChunks(kinds))

  test "arbitrary input and mutations never panic":
    var rng = initRand(0xAF)
    for _ in 0 ..< 256:
      var b = newSeq[byte](rng.rand(2047))
      for v in b.mitems: v = byte(rng.rand(255))
      try: discard header(b)
      except AffinityError: discard
      discard previewOk(b)
    for _ in 0 ..< 256:
      var b = fixture()
      for _ in 0 ..< rng.rand(31):
        let i = rng.rand(199)
        if i < b.len: b[i] = byte(rng.rand(255))
      discard previewOk(b)

proc doc(): seq[byte] =
  stream(@[
    (tag("Desc"), FStr("Document")),
    (tag("Visi"), FBool(true)),
    (tag("Xfrm"), FF64s([1.0, 0.0, 5.0, 0.0, 1.0, -2.0])),
    (tag("Colr"), FDef(7, @[tag("RGBA")], @[(tag("_col"), FStruct(newSeq[byte](16)))])),
    (tag("Same"), FRef(7)),
    (tag("Null"), FNull()),
    (tag("Kids"), FShared(@[FDef(8, @[tag("Grup"), tag("Node")], @[(tag("Desc"), FStr("g"))]), FRef(8)])),
    (tag("Crvs"), FObj(tag("PCvD"), @[(tag("Data"), FPos(@[FU8(0), FU32(1), FBool(true),
                                                           FRecords(18, @[newSeq[byte](18), newSeq[byte](18), newSeq[byte](18), newSeq[byte](18)])]))])),
    (tag("Enum"), FEnum(2, 1)),
    (tag("Tile"), FEntry("d/3")),
    (tag("Stat"), FU8s([4'u8, 0, 2])),
  ])

proc parseErr(b: openArray[byte]): ref AffinityError =
  try: discard parseStream(b)
  except AffinityError as e: return e
  nil

proc readErr(a: var Archive; name: string): ref AffinityError =
  try: discard a.read(name)
  except AffinityError as e: return e
  nil

proc fatOf(file: seq[byte]): int =
  var v = 0'u64
  for k in countdown(7, 0): v = (v shl 8) or uint64(file[16 + k])
  int(v)

suite "affinity archive and stream":
  test "every method round-trips with checksums":
    let d = doc()
    for m in [mStored, mZlib, mZstd]:
      let file = container([("doc.dat", d, m), ("d/3", bytesOf("tile"), m)])
      var a = openArchive(file, defaultLimits())
      check a.header.version == 12
      check a.read("doc.dat") == d
      check a.read("d/3") == bytesOf("tile")
      check a.extracted == d.len + 4
      check readErr(a, "missing") != nil

  test "the stream parses into shared objects and links":
    let s = parseStream(doc())
    let root = s.objects[s.root]
    check root.class == tagOf("Pers")
    check root.get(tagOf("Desc")) == some(SValue(kind: svStr, s: "Document"))
    check root.get(tagOf("Xfrm")) == some(SValue(kind: svFloats, floats: @[1.0, 0.0, 5.0, 0.0, 1.0, -2.0]))
    let a = root.get(tagOf("Colr")).get
    let b = root.get(tagOf("Same")).get
    check a.kind == svObj and a.obj >= 0 and a == b
    check s.objects[a.obj].class == tagOf("RGBA")
    check s.objects[a.obj].kind == okShared
    let kids = root.get(tagOf("Kids")).get
    check kids.arr[0] == kids.arr[1]
    check s.objects[kids.arr[0].obj].chain == @[tagOf("Grup"), tagOf("Node")]
    check root.get(tagOf("Enum")) == some(SValue(kind: svEnum, enumId: 2, enumVersion: 1))
    check root.get(tagOf("Tile")) == some(SValue(kind: svEntry, s: "d/3"))
    check root.get(tagOf("Null")) == some(SValue(kind: svObj, obj: -1))

  test "checksums, sizes and budgets are enforced":
    let d = doc()
    let file = container([("doc.dat", d, mZstd)])
    # Flip a byte inside the compressed payload, past the "#Fil" tag at offset 72.
    var broken = file
    broken[90] = broken[90] xor 0x55
    var a = openArchive(broken, defaultLimits())
    check readErr(a, "doc.dat") != nil
    var b = openArchive(file, Limits(maxEntry: d.len - 1, maxTotal: high(int)))
    check readErr(b, "doc.dat").kind == afLimit
    var c = openArchive(file, Limits(maxEntry: high(int), maxTotal: d.len + 1))
    discard c.read("doc.dat")
    check readErr(c, "doc.dat").kind == afLimit

  test "a declared size smaller than the data is rejected, not truncated":
    let big = newSeq[byte](1 shl 20)
    var bigData = big
    for v in bigData.mitems: v = 7
    var file = container([("doc.dat", bigData, mZlib)])
    # Rewrite the declared size in the table to 1 KiB: decoding must stop and fail.
    let sizeAt = fatOf(file) + 59 + 5 + 8
    file.put(sizeAt, le64(1024))
    var a = openArchive(file, defaultLimits())
    check readErr(a, "doc.dat").msg == "damaged Affinity file: archive entry size mismatch"

  test "libraries and cyclic tables are rejected":
    let d = doc()
    var file = container([("doc.dat", d, mStored)])
    var library = file
    library.put(8, bytesOf("rAsA"))
    try:
      discard openArchive(library, defaultLimits())
      check false
    except AffinityError as e: check e.kind == afUnsupported
    let fat = fatOf(file)
    file.put(fat + 4, le64(uint64(fat)))
    try:
      discard openArchive(file, defaultLimits())
      check false
    except AffinityError as e: check e.detail == "allocation tables form a cycle"

  test "hostile streams fail cleanly":
    var deep: seq[(Tag, F)]
    for _ in 0 ..< 2000: deep = @[(tag("Chld"), FObj(tag("Grup"), deep))]
    let e1 = parseErr(stream(deep))
    check e1.kind == afLimit and e1.detail == "objects nested too deeply"
    # An array claiming four billion elements in a few bytes.
    let huge = stream(@[(tag("Big_"), FRaw([0x81'u8, byte('_'), byte('g'), byte('i'), byte('B'), 0xff, 0xff, 0xff, 0xff, 0]))])
    check parseErr(huge).detail == "array longer than its data"
    check parseErr(stream(@[(tag("Link"), FRef(99))])) != nil
    check parseErr(stream(@[(tag("A___"), FDef(1, @[tag("RGBA")], @[])), (tag("B___"), FDef(1, @[tag("RGBA")], @[]))])) != nil
    check parseErr(stream(@[(tag("X___"), FRaw([0x0b'u8, 1, 2, 3, 4]))])).kind == afUnsupported
    let d = doc()
    for e in 0 ..< d.len: check parseErr(d[0 ..< e]) != nil

  test "a small compressed entry cannot make the stream allocate gigabytes":
    proc field(code: uint8; n: uint32; data: int): F =
      var f = @[0x80'u8 or code]
      f.add le32(uint32(tag("Big_")))
      f.add le32(n)
      f.setLen(f.len + data)
      FRaw(f)
    let bools = 1'u32 shl 27
    let bytes = uint32(MaxValues) + 1
    for f in [field(0x29, bools, int(bools div 8)), field(0x01, bytes, int(bytes))]:
      let d = stream(@[(tag("Big_"), f)])
      let file = container([("doc.dat", d, mZstd)])
      check file.len < d.len div 100
      var a = openArchive(file, defaultLimits())
      let entry = a.read("doc.dat")
      check parseErr(entry).msg == "Affinity file exceeds a safety limit: too many values in the document stream"

  test "mutated containers and streams never panic":
    var rng = initRand(0xAFF)
    let d = doc()
    for _ in 0 ..< 512:
      var edits: seq[(int, byte)]
      for _ in 0 ..< 1 + rng.rand(22): edits.add (rng.rand(4095), byte(rng.rand(255)))
      for m in [mStored, mZlib, mZstd]:
        var file = container([("doc.dat", d, m)])
        for (i, v) in edits: file[i mod file.len] = v
        try:
          var a = openArchive(file, defaultLimits())
          let bytes = a.read("doc.dat")
          discard parseStream(bytes)
        except AffinityError: discard
      var s = d
      for (i, v) in edits: s[i mod s.len] = v
      discard parseErr(s)

# ---------------------------------------------------------------- the document model

proc rgbaF(r, g, b, a: float32): F =
  var v: seq[byte]
  for x in [r, g, b, a]: v.add le32(cast[uint32](x))
  FStruct(v)

proc solid(id: uint32; c: F): F =
  FDef(id, @[tag("FDsc")], @[(tag("FDeF"), FDef(id + 1, @[tag("FilS")],
    @[(tag("Colr"), FDef(id + 2, @[tag("RGBA")], @[(tag("_col"), c)]))]))])

proc node(x, y: float64; role = 0'u8): seq[byte] =
  result = le64(cast[uint64](x)) & le64(cast[uint64](y))
  result.add [1'u8, role]

proc square(s, o: float64): seq[seq[byte]] =
  @[node(o, o), node(o + s, o), node(o + s, o + s), node(o, o + s), node(o, o)]

proc nativeDoc*(m: Method; extra: seq[F] = @[]): seq[byte] =
  ## A version-12 document at 144 ppi, 200×100: an artboard "Board" moved by (20, 10) holding a
  ## red ring (two subpaths, filled even-odd) with a 4 px blue stroke (`io/tests/affinity.rs`).
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
    (tag("Chld"), FShared(@[ring] & extra)),
  ])
  let spread = FDef(2, @[tag("Sprd")], @[(tag("SprB"), FF64s([0.0, 0.0, 200.0, 100.0])), (tag("SprT"), FBool(true)),
                                         (tag("Chld"), FShared(@[board]))])
  let d = stream(@[
    (tag("UVCn"), FObj(tag("UVCn"), @[(tag("UPPI"), FF64(144.0))])),
    (tag("DocR"), FDef(1, @[tag("DocN")], @[(tag("Chld"), FShared(@[spread]))])),
  ])
  container([("doc.dat", d, m)], fixture()[101 .. ^1], true)

suite "affinity model":
  test "artboards, shapes and paints":
    for m in [mStored, mZlib, mZstd]:
      let d = read(nativeDoc(m))
      check d.dpi == 144.0
      check d.spreads.len == 1
      check d.spreads[0].bounds == AfRect(x0: 0, y0: 0, x1: 200, y1: 100)
      check d.spreads[0].transparent
      let board = d.spreads[0].nodes[0]
      check board.name == "Board"
      check board.kind.tag == nkArtboard
      check board.kind.rect == AfRect(x0: 20, y0: 10, x1: 140, y1: 90)
      check board.kind.background.len == 1 and board.kind.background[0].color == rgb(1, 1, 1, 1)
      let ring = board.children[0]
      check ring.name == "Ring" and ring.kind.tag == nkShape
      check ring.kind.evenOdd
      check ring.kind.path.subpaths.len == 2
      check ring.kind.path.subpaths[0].start == pt(20, 10)
      check ring.kind.path.subpaths[0].segments.len == 4
      check ring.kind.fills[0].color == rgb(1, 0, 0, 1)
      check ring.kind.strokes.len == 1 and ring.kind.strokes[0].width == 4.0
      check ring.kind.strokes[0].paint.color == rgb(0, 0, 1, 1)
      check d.warnings.len == 0

  test "unknown layers, blends and effects are reported":
    let extra = @[
      FDef(40, @[tag("Grup")], @[(tag("Desc"), FStr("G")), (tag("Blnd"), FEnum(99, 0)),
                                 (tag("FiEf"), FShared(@[FDef(41, @[tag("Shdw")], @[])])),
                                 (tag("Chld"), FShared(@[FDef(42, @[tag("XYRA")], @[]), FDef(43, @[tag("XYRA")], @[])]))]),
      FDef(44, @[tag("ShpN")], @[(tag("Shpe"), FDef(45, @[tag("ShHt")], @[])), (tag("ShpB"), FF64s([0.0, 0.0, 10.0, 10.0]))]),
    ]
    let d = read(nativeDoc(mStored, extra))
    check "adjustment layers (2×)" in d.warnings
    check "an unknown blend mode (imported as Normal)" in d.warnings
    check "layer effects (shadows, glows, outlines, bevels…)" in d.warnings
    let g = d.spreads[0].nodes[0].children[1]
    check g.blend == abNormal and g.children.len == 2 and g.children[0].kind.tag == nkUnsupported
    let heart = d.spreads[0].nodes[0].children[2]
    check heart.kind.tag == nkShape and heart.kind.path.subpaths[0].segments.len == 4
    check heart.name == ""

  test "a layer that contains itself is cut":
    let loop = FDef(50, @[tag("Grup")], @[(tag("Chld"), FShared(@[FRef(50)]))])
    let d = read(nativeDoc(mStored, @[loop]))
    check "a layer that contains itself" in d.warnings
    check d.spreads[0].nodes[0].children[1].children.len == 0

  test "missing pages and roots are errors":
    let noRoot = container([("doc.dat", stream(@[(tag("Desc"), FStr("x"))]), mStored)])
    try:
      discard read(noRoot)
      check false
    except AffinityError as e: check e.detail == "document has no root node"
    let noPages = container([("doc.dat", stream(@[(tag("DocR"), FDef(1, @[tag("DocN")], @[]))]), mStored)])
    try:
      discard read(noPages)
      check false
    except AffinityError as e: check e.detail == "document has no pages"

  test "mutated native documents never panic":
    var rng = initRand(0xAF3)
    for k in 0 ..< 256:
      var b = nativeDoc(if k mod 2 == 0: mStored else: mZstd)
      for _ in 0 ..< 1 + rng.rand(14):
        b[rng.rand(2047) mod b.len] = byte(rng.rand(255))
      try: discard read(b)
      except AffinityError: discard
