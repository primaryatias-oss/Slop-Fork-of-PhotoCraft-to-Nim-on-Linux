## Type layout against the Rust text engine (expected values from `tools/oracle layout`, bundled
## fonts only).

import std/[unittest, options, math]
import photocraft/doc
import photocraft/text/[fonts, layout, bidi]

proc near(a, b: float32): bool = abs(a - b) <= 1e-3'f32 * max(1'f32, abs(b))

proc layer(text: string; style: CharStyle; para = none(ParagraphStyle); shape = TextShape(kind: tsPoint)): TextLayer =
  result = defaultTextLayer()
  result.text = text
  result.runs = @[TextRun(len: text.len, style: style)]
  if para.isSome: result.paragraphs = @[ParagraphRun(len: text.len, style: para.get)]
  result.shape = shape

proc inter(size: float32): CharStyle =
  result = defaultCharStyle()
  result.fontFamily = "Inter"
  result.sizePt = size

let db = newFontDb()

suite "text layout":
  test "point text matches the Rust layout":
    var ly: Layouter
    let l = ly.layout(db, layer("Hello, World! AVA Te office", inter(24)), 72)
    check l.faces.len == 1
    check near(l.faces[0].sizePx, 24)
    check l.lines.len == 1
    check l.lines[0].range == (0, 27)
    check near(l.lines[0].x1, 304.13672)
    check near(l.lines[0].ascent, 23.25)
    check near(l.lines[0].descent, 5.7890625)
    check l.glyphs[0].id == 161
    check near(l.glyphs[1].x, 17.835938)
    check near(l.glyphs[3].x, 37.640625)

  test "justified box text with indents matches the Rust layout":
    var ly: Layouter
    var p = defaultParagraphStyle()
    p.align = taJustifyLeft
    p.firstLineIndentPt = 12
    p.startIndentPt = 4
    p.endIndentPt = 6
    let text = "The quick brown fox jumps over the lazy dog. Pack my box with five dozen liquor jugs. Sphinx of black quartz, judge my vow."
    let l = ly.layout(db, layer(text, inter(14), some(p), TextShape(kind: tsBox, width: 180, height: 400)), 72)
    let want = [(0, 20, 10.185547'f32, 16'f32, 151.1875'f32), (20, 45, 26.985548, 4, 166.25098),
                (45, 67, 43.78555, 4, 148.49121), (67, 93, 60.58555, 4, 170.64648),
                (93, 119, 77.38555, 4, 172.29395), (119, 123, 94.185555, 4, 34.194336)]
    check l.lines.len == want.len
    for i, w in want:
      check l.lines[i].range == (w[0], w[1])
      check near(l.lines[i].baseline, w[2])
      check near(l.lines[i].x0, w[3])
      check near(l.lines[i].x1, w[4])

  test "empty and degenerate layers lay out without errors":
    var ly: Layouter
    var t = defaultTextLayer()
    let l = ly.layout(db, t, 72)
    check l.lines.len == 1
    t.text = "\n\n"
    check ly.layout(db, t, 0).lines.len == 3
    t.text = "x"
    t.shape = TextShape(kind: tsBox, width: -5, height: 0)
    discard ly.layout(db, t, 72)

  test "carets and hit testing":
    var ly: Layouter
    let l = ly.layout(db, layer("ab cd", inter(20)), 72)
    check l.hitTest(-100, 0) == 0
    check l.hitTest(1000, 0) == 5
    let (x, _, _) = l.caret(5)
    check near(x, l.lines[0].x1)
    check wordBoundary("hello world", 2, true) == 5
    check charIndex("héllo", 3) == 2

suite "bidi":
  test "levels for mixed text":
    var r: BidiResolver
    r.resolve(@[0x61, 0x20, 0x5D0, 0x5D1, 0x20, 0x31])
    check r.baseLevel == 0
    check r.levels == @[0'u8, 0, 1, 1, 1, 2]
