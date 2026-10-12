## PSD model tests: builder round trips, byte-stable re-serialization of the psd-tools corpus,
## and graceful failure on corrupt input.
import std/[unittest, os, options, strutils]
import photocraft/psd, photocraft/zlib

const Corpus = "../corpus/psd-tools-src/tests/psd_files"

suite "psd":
  test "builder writes a file that reads back byte-stable":
    var b = newPsdBuilder(2, 1)
    b.pushLayer newLayerSpec("Red", 0, 0, 2, 1, rgba8(@[255'u8, 0, 0, 255, 255, 0, 0, 128]))
    b.composite rgba8(@[255'u8, 0, 0, 255, 255, 0, 0, 128])
    let bytes = b.toBytes
    let f = parsePsd(bytes)
    check f.toBytes == bytes
    check f.layers[0].name == "Red"
    check f.layerRgba8(0).data[7] == 128
    check f.compositeRgba8().data[0 .. 3] == @[255'u8, 0, 0, 255]

  test "every compression round trips at 8 and 16 bits with groups and masks":
    for c in AllCompressions:
      for depth in [8, 16]:
        for v in [vPsd, vPsb]:
          var b = newPsdBuilder(5, 3)
          b.depth = depth
          b.version = v
          b.compression = c
          var px = if depth == 8: rgba8(newSeq[byte](60)) else: rgba16(newSeq[uint16](60))
          for i in 0 ..< 60:
            if depth == 8: px.d8[i] = byte(i * 4) else: px.d16[i] = uint16(i * 1000)
          b.beginGroup newGroupSpec("G")
          var spec = newLayerSpec("L ü", 1, 0, 5, 3, px)
          spec.mask = some(MaskSpec(rect: rectXywh(0, 0, 2, 2), data: @[0'u8, 64, 128, 255], defaultColor: 255))
          b.pushLayer spec
          b.endGroup()
          b.composite px
          let bytes = b.toBytes
          let f = parsePsd(bytes)
          check f.toBytes == bytes
          check f.layers.len == 3
          check f.layers[1].name == "L ü"
          let tree = f.layerTree
          check tree.len == 1 and tree[0].group and tree[0].children.len == 1
          let m = f.layerUserMask(1)
          check m.isSome and m.get.data == @[0'u8, 64, 128, 255]
          let rgba = f.layerRgba8(1)
          check rgba.width == 5

  test "descriptor round trip":
    let d = newDescriptor("Lefx").with("Scl ", vUnit("#Prc", 100)).with("masterFXSwitch", vBool(true))
      .with("list", vList(@[vInt(3), vText("hi"), vEnum("BlnM", "Nrml")]))
    let bytes = versioned(d).toBytes
    let back = versionedFromBytes(bytes)
    check back.descriptor == d
    check back.toBytes == bytes
    check back.descriptor.getText("list").isNone
    check $back.descriptor.getList("list").get[1].text == "hi"

  test "psd-tools corpus re-serializes byte for byte":
    if not dirExists(Corpus): skip()
    else:
      var ok, failed, rejected = 0
      for path in walkFiles(Corpus / "*.ps[db]"):
        let bytes = readFile(path).toBytes
        try:
          let f = parsePsd(bytes)
          if f.toBytes == bytes: inc ok
          else:
            inc failed
            echo "  not byte-stable: ", path.extractFilename
          for i in 0 ..< f.layerCount:
            try: discard f.layerRgba8(i)
            except PsdError: discard
          try: discard f.compositeRgba8()
          except PsdError: discard
        except PsdError as e:
          inc rejected
          echo "  rejected ", path.extractFilename, ": ", e.msg
      echo "  corpus: ", ok, " byte-stable, ", failed, " differ, ", rejected, " rejected"
      check failed == 0

  test "truncated and corrupt input raises PsdError":
    var b = newPsdBuilder(4, 4)
    b.pushLayer newLayerSpec("A", 0, 0, 4, 4, rgba8(newSeq[byte](64)))
    let bytes = b.toBytes
    for cut in countup(0, bytes.len - 1, 3):
      try: discard parsePsd(bytes[0 ..< cut])
      except PsdError: discard
    var flipped = bytes
    for i in countup(26, flipped.len - 1, 7):
      flipped[i] = flipped[i] xor 0x5A
      try:
        let f = parsePsd(flipped)
        for k in 0 ..< f.layerCount:
          try: discard f.layerRgba8(k)
          except PsdError: discard
      except PsdError: discard

suite "psd presets":
  test "aco, ase and shc round trip":
    let sw = @[AcoSwatch(name: "Red", color: acoFromRecord(acoRgb, [65535'u16, 0, 0, 0])),
               AcoSwatch(name: "Ink", color: AcoColor(kind: ackCmyk, space: acoCmyk, v: [100'u16, 200, 300, 400])),
               AcoSwatch(name: "Lab", color: acoFromRecord(acoLab, [5000'u16, cast[uint16](-300'i16), 200, 0]))]
    let a = parseAco(writeAco(sw))
    check a.hasNames and a.swatches == sw
    check a.swatches[2].color.labA == -300
    let ase = AseFile(entries: @[AseEntry(swatch: AseSwatch(name: "R", color: AseColor(kind: ascRgb, c: @[1'f32, 0, 0]))),
      AseEntry(isGroup: true, name: "G", swatches: @[AseSwatch(name: "K", color: AseColor(kind: ascGray, c: @[0.5'f32]), kind: akSpot)])])
    let back = parseAse(writeAse(ase))
    check back.swatchCount == 2 and back.entries[1].swatches[0].kind == akSpot
    check back.entries[0].swatch.color.c == @[1'f32, 0, 0]
    let shc = @[ShcContour(name: "Cone", points: @[ShcPoint(input: 0, output: 0, continuous: false), ShcPoint(input: 255, output: 128, continuous: true)])]
    check parseShc(writeShc(shc)) == shc
    expect PsdError: discard parseAco(@[0'u8, 9])
    expect PsdError: discard parseAse(@[byte('A'), byte('S')])

  test "grd, patterns and abr round trip":
    let g = @[GrdGradient(name: "BW", stops: @[GrdStop(location: 0, midpoint: 0.5, color: GrdColor(kind: gcForeground)),
                                         GrdStop(location: 1, midpoint: 0.5, color: GrdColor(kind: gcRgb, v: [1'f32, 0.5, 0, 0]))],
                          opacity: @[(0'f32, 1'f32, 0.5'f32)])]
    let gb = parseGrd(writeGrd(g))
    check gb.len == 1 and gb[0].name == "BW" and gb[0].stops.len == 2
    check abs(gb[0].stops[1].color.v[1] - 0.5) < 1e-6
    var plane = newSeq[byte](12)
    for i in 0 ..< 12: plane[i] = byte(i * 20)
    let pat = PsdPattern(mode: 1, width: 4, height: 3, name: "P", id: "abc", depth: 8, channels: @[plane], alpha: some(plane))
    let pats = parsePatFile(writePatFile([pat]))
    check pats.len == 1 and pats[0].channels[0] == plane and pats[0].alpha.get == plane
    check parsePatternBlock(writePatternBlock([pat]))[0].name == "P"
    let s = AbrSample(id: "u1", width: 4, height: 3, depth: 8, data: plane)
    let v6 = parseAbr(writeAbrV6(2, [s], [pat], [newDescriptor("brushPreset").with("Nm  ", vText("Tip"))],
                                 [@["Folder", "Sub"]]))
    check v6.samples.len == 1 and v6.samples[0].data == plane and v6.patterns.len == 1
    check v6.presets.len == 1 and v6.folders == @[@["Folder", "Sub"]]
    let legacy = @[LegacyBrush(spacing: 25, antiAlias: true, tip: LegacyTip(kind: ltComputed, diameter: 9, hardness: 100, angle: -5, roundness: 80)),
                   LegacyBrush(name: "S", spacing: 10, antiAlias: true, tip: LegacyTip(kind: ltSampled, sample: s))]
    let v2 = parseAbr(writeAbrV12(2, legacy, true))
    check v2.legacy.len == 2 and v2.legacy[0].tip.angle == -5 and v2.legacy[1].tip.sample.data == plane
    expect PsdError: discard parseAbr(@[0'u8, 3])

  test "layered TIFF image source data round trips (big-endian)":
    var b = newPsdBuilder(3, 2)
    b.pushLayer newLayerSpec("A", 0, 0, 3, 2, rgba8(newSeq[byte](24)))
    let f = b.build
    let (bytes, _) = fromPsd(f).toBytes
    let (isd, _) = parseImageSourceData(bytes)
    check isd.layerInfo.get.layers[0].name == "A"
    check toBytes(isd)[0] == bytes
