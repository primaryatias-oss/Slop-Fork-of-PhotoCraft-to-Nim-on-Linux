## PSD type layers: `TySh`/EngineData/`Txt2` ⇄ `TextLayer` and named style sheets (ports of the
## Rust `photocraft_text` PSD tests; byte-level parity with Rust is checked by the oracle).

import std/[unittest, options, json, algorithm]
import photocraft/[doc, color, geom, raster]
import photocraft/psd/descriptor
import photocraft/text
import photocraft/text/[psd_text, psd_styles, engine_data]

proc point(text: string; size: float32): TextLayer =
  result = defaultTextLayer()
  result.text = text
  result.fontFamily = "Inter"
  result.sizePt = size

proc styled(text: string; style: CharStyle): TextLayer =
  result = defaultTextLayer()
  result.text = text
  result.runs = @[TextRun(len: text.len, style: style)]

proc runsOf(text: string; runs: openArray[(int, CharStyle)]): TextLayer =
  result = defaultTextLayer()
  result.text = text
  for (n, s) in runs: result.runs.add TextRun(len: n, style: s)
  result.syncSummary()

suite "PSD type layers":
  test "hostile text index is ignored, not sized":
    let t = runsOf("AB", [(2, defaultCharStyle())])
    let tysh = buildTysh(t, 72, none(array[4, float32]))
    check textIndex(setTextIndex(tysh, 0).get) == some(0'i32)
    check textIndex(setTextIndex(tysh, MaxTextIndex).get) == some(MaxTextIndex)
    for hostile in [MaxTextIndex + 1, high(int32)]:
      check textIndex(setTextIndex(tysh, hostile).get).isNone
      let o = parseTxt2(buildTxt2([(hostile, t)], none(seq[byte]))).get
      check o.path(["1", "1"]).asArray.get.len == 0
    let o = parseTxt2(buildTxt2([(MaxTextIndex, t)], none(seq[byte]))).get
    check o.path(["1", "1"]).asArray.get.len == int(MaxTextIndex) + 1

  test "fill is enabled in runs and new default styles":
    var t = point("AB", 100)
    var a = defaultCharStyle()
    a.color = rgba(0.2, 0.4, 0.6, 1)
    var b = defaultCharStyle()
    b.fauxBold = true
    b.sizePt = 100
    t.runs = @[TextRun(len: 1, style: a), TextRun(len: 1, style: b)]
    for shape in [TextShape(kind: tsPoint), TextShape(kind: tsBox, width: 300, height: 200)]:
      t.shape = shape
      let tysh = parseTysh(buildTysh(t, 72, none(array[4, float32]))).get
      let data = engineData(tysh.text).get
      let runs = data.path(["EngineDict", "StyleRun", "RunArray"]).asArray.get
      check runs.len == 2
      for run in runs:
        check run.path(["StyleSheet", "StyleSheetData", "FillFlag"]).asBool == some(true)
      for key in ["ResourceDict", "DocumentResources"]:
        let sheets = data.path([key, "StyleSheetSet"]).asArray.get
        check sheets[0].path(["StyleSheetData", "FillFlag"]).asBool == some(true)
      let back = textLayerFromTysh(writeTysh(tysh), 72).get
      check back.text == t.text
      check back.runs.len == t.runs.len
      for i in 0 ..< back.runs.len:
        check back.runs[i].style.color == t.runs[i].style.color
        check back.runs[i].style.sizePt == t.runs[i].style.sizePt
        check back.runs[i].style.fauxBold == t.runs[i].style.fauxBold

  test "manual kerning round trips in Photoshop's form":
    let s = defaultCharStyle()
    let cases = @[
      @[(1, kOff, 100'f32), (1, kOff, -50'f32), (2, kMetrics, 0'f32)],
      @[(1, kMetrics, 0'f32), (1, kOff, 0'f32), (2, kMetrics, 0'f32)],
      @[(4, kOff, 0'f32)],
      @[(2, kMetrics, 0'f32), (1, kOff, 25'f32), (1, kMetrics, 0'f32)],
      @[(3, kMetrics, 0'f32), (1, kOff, 300'f32)]]
    proc per(t: TextLayer): seq[(Kerning, float32)] =
      for r in t.charRuns:
        for _ in 0 ..< r.len: result.add (r.style.kerning, r.style.kern)
    for runs in cases:
      var rs: seq[(int, CharStyle)]
      for (n, k, v) in runs:
        var st = s
        st.kerning = k
        st.kern = v
        rs.add (n, st)
      let t = runsOf("AVAT", rs)
      let back = textLayerFromTysh(buildTysh(t, 72, none(array[4, float32])), 72).get
      var want = per(t)
      # The last character's mode has no EngineData slot (Metrics unless it has a manual kern).
      if want[^1][1] == 0: want[^1][0] = kMetrics
      check per(back) == want

  test "antialias, OpenType toggles and warp round trip":
    var style = defaultCharStyle()
    style.fontFamily = "Inter"
    style.sizePt = 20
    style.features = @[FontFeature(tag: "swsh", value: 1), FontFeature(tag: "frac", value: 1)]
    style.discretionaryLigatures = true
    for aa in [aaWindows, aaWindowsLcd, aaCrisp, aaNone]:
      var t = styled("1/2 Swash", style)
      t.antialias = aa
      t.warp = some(TextWarp(style: "warpFlag", value: -35, horizontalDistortion: 10, verticalDistortion: 0, horizontal: false))
      let back = textLayerFromTysh(buildTysh(t, 72, none(array[4, float32])), 72).get
      check back.antialias == aa
      check back.warp == t.warp
      let st = back.charRuns[0].style
      check st.discretionaryLigatures
      var tags: seq[string]
      for f in st.features: tags.add f.tag
      tags.sort()
      check tags == @["frac", "swsh"]

  test "legacy point origin imports and round trips on canvas (#1469)":
    var src = defaultCharStyle()
    src.fontFamily = "Inter"
    src.sizePt = 120
    var tysh = parseTysh(buildTysh(styled("Name", src), 72, none(array[4, float32]))).get
    tysh.transform = Affine(m: [4.1667, 0.0, 0.0, 4.1667, -32375.0, -32887.5])
    let ink = [8050.0, 8160.0, 8220.0, 8300.0]
    proc rect(cls: string): Value =
      vDesc(newDescriptor(cls).with("Left", vUnit("#Pnt", ink[0])).with("Top ", vUnit("#Pnt", ink[1]))
        .with("Rght", vUnit("#Pnt", ink[2])).with("Btom", vUnit("#Pnt", ink[3])))
    var items: seq[(Id, Value)]
    for it in tysh.text.items:
      if it[0].s notin ["bounds", "boundingBox"]: items.add it
    tysh.text.items = items
    tysh.text.items.add (id("bounds"), rect("bounds"))
    tysh.text.items.add (id("boundingBox"), rect("boundingBox"))
    let imported = textLayerFromTysh(writeTysh(tysh), 72).get
    let expected = [4.1667, 0.0, 0.0, 4.1667, 1166.935, 1112.772]
    for i in 0 ..< 6: check abs(imported.transform.m[i] - expected[i]) < 0.001
    let (_, rendered) = newTextEngine().render(imported, 72, PixelFormat(mode: cmRgb, sample: stU8, alpha: true))
    let placed = rendered.surface.contentBounds
    check not placed.isEmpty
    check placed.x0 in 1000 ..< 3000 and placed.y0 in 500 ..< 2000
    let rt = textLayerFromTysh(buildTysh(imported, 72, some([8050'f32, 8160, 8220, 8300])), 72).get
    for i in 0 ..< 6: check abs(rt.transform.m[i] - expected[i]) < 0.001

  test "engine data template without EngineDict":
    let t = styled("Hi", defaultCharStyle())
    for tmpl in [parseEngineData(cast[seq[byte]]("<< /ResourceDict << >> >>")), edDict(), edInt(3)]:
      let e = buildEngineData(t, some(tmpl), 72)
      check e.path(["EngineDict", "Editor", "Text"]) == some(edString("Hi\r"))

  test "translate moves only the translation":
    var t = point("Hello!", 26)
    t.transform = Affine(m: [1.5, 0.0, 0.0, 1.5, 80.0, 340.0])
    let tysh = buildTysh(t, 72, some([0'f32, -20, 300, 4]))
    let moved = translateTysh(tysh, -8, -279).get
    let back = parseTysh(moved).get
    check back.transform.m == [1.5, 0.0, 0.0, 1.5, 72.0, 61.0]
    check moved.len == tysh.len
    check moved[0 ..< 34] == tysh[0 ..< 34] and moved[50 .. ^1] == tysh[50 .. ^1]
    check parseTysh(tysh).get.bounds == back.bounds
    check translateTysh(tysh[0 ..< 40], 1, 1).isNone

  test "Txt2 carries the optical mode and keeps an unchanged blk byte for byte":
    var s = defaultCharStyle()
    s.kerning = kOptical
    var t = runsOf("AV\nTo", [(2, s), (3, defaultCharStyle())])
    check needsTxt2([(0'i32, t)])
    let blk = buildTxt2([(0'i32, t)], none(seq[byte]))
    var plain = t
    plain.runs = @[TextRun(len: 5, style: defaultCharStyle())]
    let tysh = buildTysh(plain, 72, none(array[4, float32]))
    var back = textLayerFromTysh(tysh, 72).get
    applyTxt2(back, tysh, parseTxt2(blk).get)
    check back.charRuns[0].style.kerning == kOptical
    check back.charRuns[^1].style.kerning == kMetrics
    # Same layers again: the previous blk comes back unchanged.
    check buildTxt2([(0'i32, t)], some(blk)) == blk
    check not needsTxt2([(0'i32, plain)])

  test "malformed input never panics":
    check parseTysh([]).isNone
    check textLayerFromTysh(newSeq[byte](60), 72).isNone
    check not engineTextIsBlank(cast[seq[byte]]("junk"))
    check parseTxt2(cast[seq[byte]]("/0 << /1 [ ")).isNone
    let tysh = buildTysh(point("Hello", 12), 72, none(array[4, float32]))
    for cut in 0 ..< tysh.len:
      let part = tysh[0 ..< cut]
      discard textLayerFromTysh(part, 72)
      discard readStyleSheets(part, 72)
      discard textIndex(part)
      discard setTextIndex(part, 3)
    var junk = edDict(@[("1", edDict(@[("1", edArray(@[edInt(4), edDict(@[("0", edString("x"))])]))]))])
    var t = point("x", 12)
    applyTxt2(t, tysh, junk)
    discard buildTxt2([(1'i32, t)], some(writeBare(junk.items)))

suite "PSD style sheets":
  test "named sheets round trip":
    var styles = defaultTextStyles()
    var emph = defaultCharStyle()
    emph.sizePt = 30
    emph.underline = true
    styles.character.add CharacterStyleDef(id: 4, name: "Emphasis", attrs: diffAttrs(emph, defaultCharStyle()))
    var centered = defaultParagraphStyle()
    centered.align = taCenter
    styles.paragraph.add ParagraphStyleDef(id: 2, name: "Centered", paraAttrs: diffAttrs(centered, defaultParagraphStyle()),
                                           charAttrs: newJObject())
    var base = defaultCharStyle()
    base.fontFamily = DefaultFamily
    let e = styles.resolveChar(some(2'u32), some(4'u32), DefaultFamily)
    var layer = defaultTextLayer()
    layer.text = "plain bold"
    layer.runs = @[TextRun(len: 6, style: base), TextRun(len: 4, style: e)]
    layer.paragraphs = @[ParagraphRun(len: 10, style: styles.resolvePara(some(2'u32)))]
    layer.syncSummary()
    let tysh = buildTysh(layer, 72, none(array[4, float32]))
    check not hasNamedSheets(tysh)
    let o = writeStyleSheets(tysh, layer, styles, 72).get
    check hasNamedSheets(o)
    let r = readStyleSheets(o, 72).get
    check r.charSheets.len == 1
    check r.charSheets[0][1] == "Emphasis"
    check r.charSheets[0][2].sizePt == 30 and r.charSheets[0][2].underline
    check r.paraSheets[0][1] == "Centered"
    check r.paraSheets[0][2].align == taCenter
    check r.runChar == @[none(int), some(1)]
    check r.runPara == @[some(1)]
    let back = textLayerFromTysh(o, 72).get
    check back.text == layer.text
    check back.charRuns[1].style.sizePt == 30
    let plain = writeStyleSheets(o, layer, defaultTextStyles(), 72).get
    check not hasNamedSheets(plain)
    for x in readStyleSheets(plain, 72).get.runChar: check x.isNone

  test "malformed input":
    check readStyleSheets([], 72).isNone
    check readStyleSheets(newSeq[byte](60), 72).isNone
    check not hasNamedSheets(cast[seq[byte]]("junk"))
    check writeStyleSheets([1'u8, 2, 3], defaultTextLayer(), defaultTextStyles(), 72).isNone
