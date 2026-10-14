## Font reading, shaping and segmentation. Expected values come from the Rust build's libraries
## (skrifa metrics, HarfRust shaping) on the bundled fonts, via `tools/oracle`.

import std/[unittest, unicode, os, options]
import photocraft/font/[sfnt, shape]
import photocraft/text/[uprops, segment]

const fontsDir = currentSourcePath().parentDir.parentDir.parentDir / "assets" / "fonts"

proc face(name: string): Face =
  loadFace(newFontBlob(cast[seq[byte]](readFile(fontsDir / name))), 0)

proc cps(s: string): seq[int] =
  for r in s.runes: result.add int(r)

proc shaped(f: Face; s: string; feats: seq[ShapeFeature] = @[]): seq[array[3, int]] =
  for g in f.shape(cps(s), ShapeOptions(script: scLatin, features: feats)):
    result.add [g.gid, g.cluster, int(g.xAdvance)]

suite "font":
  test "Inter metrics and names match skrifa":
    let f = face("Inter-Regular.ttf")
    check f.unitsPerEm == 2048
    check f.numGlyphs == 2937
    check f.ascent == 1984
    check f.descent == 494
    check f.familyName == "Inter"
    check f.postscriptName.get("") == "Inter-Regular"
    check f.mappings.len == 2852

  test "JetBrains Mono metrics":
    let f = face("JetBrainsMono-Regular.ttf")
    check f.unitsPerEm == 1000
    check f.ascent == 1020
    check f.descent == 300
    check f.familyName == "JetBrains Mono"

  test "kerning, contextual alternates and composition match HarfRust":
    let f = face("Inter-Regular.ttf")
    check f.shaped("AVA Te") == @[[2, 0, 1273], [456, 1, 1273], [2, 2, 1413], [1777, 3, 576],
                                   [411, 4, 1162], [614, 5, 1194]]
    check f.shaped("é -> 1:00") == @[[618, 0, 1194], [1777, 2, 576], [1805, 3, 1954],
      [1777, 5, 576], [1342, 6, 833], [1506, 7, 590], [1341, 8, 1292], [1341, 9, 1292]]

  test "kern off restores plain advances":
    let f = face("Inter-Regular.ttf")
    check f.shaped("AV", @[("kern", 0)]) == @[[2, 0, 1413], [456, 1, 1413]]

  test "monospace ligature components keep their cells":
    let f = face("JetBrainsMono-Regular.ttf")
    check f.shaped("!= ->") == @[[1751, 0, 600], [872, 1, 600], [821, 2, 600], [1751, 3, 600],
                                  [841, 4, 600]]

  test "empty text and missing glyphs shape without errors":
    let f = face("Inter-Regular.ttf")
    check f.shape(newSeq[int](), ShapeOptions(script: scLatin)).len == 0
    let g = f.shape(@[0x10FFFF, 0x41], ShapeOptions(script: scLatin))
    check g.len == 2
    check g[0].gid == 0

suite "segmentation":
  test "line breaks after spaces, not inside words or before punctuation":
    let b = lineBreaks(cps("Hello, world! (a-b) 12.5%"))
    check not b[1]        # e|l
    check not b[5]        # o|,
    check b[7]            # ' '|w
    check not b[12]       # d|!
    check b[14]           # ' '|(
    check not b[15]       # (|a
    check b[17]           # -|b (break after hyphen)
    check not b[21]       # 1|2 numbers stay together
    check not b[24]       # 5|%
  test "ideographs break between characters, not before closing punctuation":
    let b = lineBreaks(cps("漢字。かな"))
    check b[1]
    check not b[2]        # before 。
    check b[3]
  test "grapheme clusters keep combining marks and emoji sequences":
    let g = graphemeBoundaries([0x65, 0x301, 0x61, 0x1F468, 0x200D, 0x1F469, 0x62, 0x0D, 0x0A])
    check g == @[true, false, true, true, false, false, true, true, false]

suite "unicode properties":
  test "categories, scripts, bidi and normalization":
    check generalCategory(0x41) == gcLu
    check script(0x0627).isoCode == "Arab"
    check bidiClass(0x05D0) == bcR
    check mirror(0x28) == 0x29
    check compose(0x65, 0x301) == 0xE9
    check decompose(0xE9) == (0x65, 0x301)
    check compose(0x1100, 0x1161) == 0xAC00
    check joiningType(0x0628) == 3
