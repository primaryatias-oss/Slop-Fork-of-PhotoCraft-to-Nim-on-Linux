## Layout of a text layer's model into positioned glyphs (text-space pixels, y down, first baseline
## of point text at y = 0) — port of `photocraft_text::layout`.
##
## Each paragraph is shaped and line-broken by `para` (the parley port); paragraphs are then
## stacked with Photoshop's rules: baseline-to-baseline distance = the largest leading on the line
## (auto leading = paragraph factor × size), space before/after, indents, and point-text alignment
## around the anchor. Vertical type is laid out in line space and turned 90° clockwise into text
## space: CJK characters stay upright (VORG / vmtx / ideographic em box, `vert` alternates), other
## text runs rotated, Photoshop's default.

import std/[options, unicode, strutils, algorithm, math, sequtils]
import ../doc
import ../font/[sfnt, shape]
import fonts, para, optical, cjk, craft_fonts, served, uprops
import ../rmath

const
  SyntheticSmallCapsScale = 0.7'f32
  DefaultTabStopPt* = 36'f32  ## Photoshop's default tab stops: every half inch
  ForcedLineBreak* = 3        ## U+0003, a line break inside a paragraph (Shift+Return in PSD type)
  Lrm = "‎"
  Rlm = "‏"

type
  SmallCapsMode = enum scNone, scOpenType, scSynthetic

  VClass* = enum vcRotate = 0, vcUpright = 1, vcAlternateOrRotate = 2

  GlyphOrient* = enum goHorizontal, goUpright, goRotated

  GlyphFace* = object
    face*: FaceRec
    coords*: seq[int16]
    sizePx*: float32
    embolden*: bool       ## synthetic bold (face lacks the weight)
    skewDeg*: float32     ## synthetic oblique angle (face lacks italics)

  PlacedGlyph* = object
    face*: int
    id*: int
    x*, y*: float32       ## pen position, text space px (y = baseline before baseline shift)
    style*: int           ## character run index (into `TextLayout.styles`)
    orient*: GlyphOrient

  DecorationRect* = object
    x0*, y0*, x1*, y1*: float32
    style*: int

  LineInfo* = object
    range*: (int, int)    ## byte range in the layer text (without the paragraph break)
    baseline*: float32
    x0*, x1*: float32
    ascent*, descent*: float32
    paragraph*: int

  ClusterInfo* = object
    range*: (int, int)
    x*, advance*: float32
    line*: int
    rtl*: bool

  TextLayout* = object
    faces*: seq[GlyphFace]
    glyphs*: seq[PlacedGlyph]
    decorations*: seq[DecorationRect]
    lines*: seq[LineInfo]
    clusters*: seq[ClusterInfo]
    styles*: seq[CharStyle]
    pxPerPt*: float32
    vertical*: bool

  Layouter* = object
    optical: OpticalCache

proc smallCapsMode(caps: Caps; faceHasSmcp: bool): SmallCapsMode =
  if caps == capsSmallCaps: (if faceHasSmcp: scOpenType else: scSynthetic) else: scNone

proc verticalClass*(u: int): VClass =
  ## How a character is set in vertical type (a simplification of UAX #50).
  if u in 0x2014 .. 0x2016 or u in [0x2025, 0x2026, 0x2329, 0x232A] or u in 0x3008 .. 0x3011 or
     u in 0x3014 .. 0x301F or u in [0x3030, 0x30A0, 0x30FC] or u in 0xFE59 .. 0xFE5E or
     u in [0xFF08, 0xFF09, 0xFF0D] or u in 0xFF1A .. 0xFF1E or u in [0xFF3B, 0xFF3D, 0xFF3F] or
     u in 0xFF5B .. 0xFF60 or u == 0xFFE3:
    return vcAlternateOrRotate
  if u in 0xFF61 .. 0xFFDC: return vcRotate
  let upright = classify(u).isSome or u in [0xA7, 0xA9, 0xAE, 0xB1, 0xD7, 0xF7] or u in 0xBC .. 0xBE or
    u in 0x2E80 .. 0x2EFF or u in 0xFE10 .. 0xFE1F or u in 0xFE30 .. 0xFE4F or u in 0x1F000 .. 0x1FAFF
  if upright: vcUpright else: vcRotate

# ---------------------------------------------------------------- the layout result

proc toText*(l: TextLayout; x, y: float32): (float32, float32) =
  ## Line space → text space (90° clockwise for vertical type).
  if l.vertical: (-y, x) else: (x, y)

proc toLine*(l: TextLayout; x, y: float32): (float32, float32) =
  if l.vertical: (y, -x) else: (x, y)

proc lineBounds*(l: TextLayout): Option[array[4, float32]] =
  ## Logical bounds of all lines in line space.
  for ln in l.lines:
    let r = [ln.x0, ln.baseline - ln.ascent, ln.x1, ln.baseline + ln.descent]
    if result.isNone: result = some(r)
    else:
      let a = result.get
      result = some([min(a[0], r[0]), min(a[1], r[1]), max(a[2], r[2]), max(a[3], r[3])])

proc bounds*(l: TextLayout): Option[array[4, float32]] =
  ## Logical bounds (x0, y0, x1, y1) of all lines, text space.
  let b = l.lineBounds
  if b.isNone: return
  let v = b.get
  some(if l.vertical: [-v[3], v[0], -v[1], v[2]] else: v)

proc closestLine(l: TextLayout; y: float32): int =
  result = -1
  var best = Inf.float32
  for i, ln in l.lines:
    let d = if y < ln.baseline - ln.ascent: ln.baseline - ln.ascent - y
            elif y > ln.baseline + ln.descent: y - ln.baseline - ln.descent
            else: 0'f32
    if d < best:
      best = d
      result = i

proc hitTestXOnLine(l: TextLayout; li: int; x: float32): int =
  if li < 0 or li >= l.lines.len: return 0
  var bestD = F32Max
  var bestOff = l.lines[li].range[1]
  for c in l.clusters:
    if c.line != li: continue
    let mid = c.x + c.advance / 2
    let (before, after) = if c.rtl: (c.range[1], c.range[0]) else: (c.range[0], c.range[1])
    let (d, off) = if x < mid: (abs(x - c.x), before) else: (abs(x - c.x - c.advance), after)
    if d < bestD:
      bestD = d
      bestOff = off
  bestOff

proc hitTestLine*(l: TextLayout; x, y: float32): int =
  ## Byte offset of the caret nearest to a line-space point.
  let li = l.closestLine(y)
  if li < 0: 0 else: l.hitTestXOnLine(li, x)

proc hitTest*(l: TextLayout; x, y: float32): int =
  ## Byte offset of the caret nearest to a text-space point.
  let (lx, ly) = l.toLine(x, y)
  l.hitTestLine(lx, ly)

proc caret*(l: TextLayout; offset: int): (float32, float32, float32) =
  ## Caret for a byte offset: (x, top, bottom) in line space.
  for c in l.clusters:
    if c.range[0] == offset and c.line < l.lines.len:
      let ln = l.lines[c.line]
      return ((if c.rtl: c.x + c.advance else: c.x), ln.baseline - ln.ascent, ln.baseline + ln.descent)
  var li = max(l.lines.len - 1, 0)
  for i, ln in l.lines:
    if offset >= ln.range[0] and offset <= ln.range[1]:
      li = i
      break
  if li >= l.lines.len: return (0'f32, 0'f32, 0'f32)
  let ln = l.lines[li]
  var x = ln.x1
  for c in l.clusters:
    if c.line == li and c.range[1] == offset:
      x = if c.rtl: c.x else: c.x + c.advance
      break
  (x, ln.baseline - ln.ascent, ln.baseline + ln.descent)

proc caretSegment*(l: TextLayout; offset: int): array[2, (float32, float32)] =
  let (x, top, bottom) = l.caret(offset)
  [l.toText(x, top), l.toText(x, bottom)]

proc byteIndex*(text: string; index: int): int =
  ## Byte offset of character `index` (the text length past the end).
  var i = 0
  var n = 0
  while i < text.len:
    if n == index: return i
    i += text.runeLenAt(i)
    inc n
  text.len

proc charIndex*(text: string; byte: int): int =
  ## Character index of byte `byte`, floored to a char boundary.
  var b = min(max(byte, 0), text.len)
  while b > 0 and b < text.len and (uint8(text[b]) and 0xC0) == 0x80: dec b
  var i = 0
  while i < b:
    i += text.runeLenAt(i)
    inc result

proc lineIndex*(l: TextLayout; byte: int): int =
  ## Line containing a byte offset (at a shared wrap boundary, the following line).
  for i in countdown(l.lines.len - 1, 0):
    if byte >= l.lines[i].range[0] and byte <= l.lines[i].range[1]: return i
  max(l.lines.len - 1, 0)

proc hitChar*(l: TextLayout; text: string; x, y: float32): (int, int) =
  ## Nearest caret to a text-space point: character index and the line it sits on.
  let b = l.hitTest(x, y)
  let (_, ly) = l.toLine(x, y)
  let cl = l.closestLine(ly)
  (charIndex(text, b), if cl >= 0: cl else: l.lineIndex(b))

proc textPointInside*(l: TextLayout; x, y, slop: float32): bool =
  let s = if slop.classify in {fcNormal, fcZero, fcSubnormal, fcNegZero}: max(slop, 0) else: 0'f32
  let b = l.bounds
  b.isSome and x >= b.get[0] - s and x <= b.get[2] + s and y >= b.get[1] - s and y <= b.get[3] + s

proc isAlnum(r: Rune): bool = r.isAlpha or generalCategory(int(r)) in {gcNd, gcNl, gcNo}

proc wordBoundary*(text: string; idx: int; forward: bool): int =
  ## Character index of the word boundary before (or after) `idx`; a word is alphanumerics.
  let chars = toSeq(text.runes)
  var i = min(idx, chars.len)
  if forward:
    while i < chars.len and not chars[i].isAlnum: inc i
    while i < chars.len and chars[i].isAlnum: inc i
  else:
    while i > 0 and not chars[i - 1].isAlnum: dec i
    while i > 0 and chars[i - 1].isAlnum: dec i
  i

proc lineStep*(l: TextLayout; text: string; idx: int; x: float32; dir: int): int =
  ## Caret on the neighbouring line keeping `x` (line space); past the ends, the text's ends.
  let n = text.runeLen
  let i = min(idx, n)
  if l.lines.len == 0: return i
  let cur = l.lineIndex(byteIndex(text, i))
  if dir < 0:
    if cur == 0: return 0
    return charIndex(text, l.hitTestXOnLine(cur - 1, x))
  if cur + 1 >= l.lines.len: return n
  charIndex(text, l.hitTestXOnLine(cur + 1, x))

proc lineEdge*(l: TextLayout; text: string; idx: int; atEnd: bool): int =
  let n = text.runeLen
  let i = min(idx, n)
  if l.lines.len == 0: return i
  let ln = l.lines[l.lineIndex(byteIndex(text, i))]
  charIndex(text, if atEnd: ln.range[1] else: ln.range[0])

# ---------------------------------------------------------------- helpers

proc splitParagraphs*(text: string): seq[(int, int)] =
  ## Byte ranges of paragraphs (each with its `\r`, `\n` or `\r\n`); text ending with a break
  ## yields a final empty paragraph.
  var start = 0
  var i = 0
  while i < text.len:
    case text[i]
    of '\r':
      let e = if i + 1 < text.len and text[i + 1] == '\n': i + 2 else: i + 1
      result.add (start, e)
      start = e
      i = e
    of '\n':
      result.add (start, i + 1)
      start = i + 1
      inc i
    else: inc i
  result.add (start, text.len)

proc stripBreak(text: string; r: (int, int)): int =
  var e = r[1]
  while e > r[0] and text[e - 1] in {'\r', '\n'}: dec e
  e

proc quoted(s: string): string = s.multiReplace(("\\", ""), ("\"", ""))

proc nextTabStop(pos, interval: float32): float32 =
  if interval > 0 and pos.classify notin {fcInf, fcNegInf, fcNan}:
    floor((pos + 1e-3'f32) / interval) * interval + interval
  else: pos

proc featureList(st: CharStyle; sc: SmallCapsMode): seq[ShapeFeature] =
  if st.kerning != kMetrics or st.kern != 0: result.add ("kern", 0)
  if not st.ligatures:
    result.add ("liga", 0)
    result.add ("clig", 0)
  if st.discretionaryLigatures: result.add ("dlig", 1)
  if sc == scOpenType: result.add ("smcp", 1)
  for f in st.features:
    var ascii = true
    for c in f.tag:
      if ord(c) > 127: ascii = false
    if f.tag.len == 4 and ascii: result.add (f.tag, int(f.value))

proc variationList(st: CharStyle): seq[(string, float32)] =
  var hasOpsz = false
  for v in st.variations:
    if v.axis == "opsz": hasOpsz = true
    if v.axis.len == 4 and v.value.classify in {fcNormal, fcZero, fcSubnormal, fcNegZero}:
      result.add (v.axis, v.value)
  if st.sizePt.classify in {fcNormal, fcZero, fcSubnormal, fcNegZero} and not hasOpsz:
    result.add ("opsz", max(st.sizePt, 0.01'f32))

proc validLocale(s: string): string =
  ## A BCP 47-looking tag, or "" (parley drops tags it can't parse).
  if s.len == 0 or s.len > 35: return ""
  for c in s:
    if not (c.isAlphaNumeric or c in {'-', '_'}): return ""
  s.replace('_', '-')

proc styleProps(st: CharStyle; k: float32; fallback: seq[string]; idx: int; sc: SmallCapsMode): seq[StyleProp] =
  let px = max(st.sizePt * k, 0.01'f32)
  var fam: seq[FamilyRef]
  if st.fontFamily.len > 0: fam.add FamilyRef(name: quoted(st.fontFamily))
  let fb = if isSerifFamily(st.fontFamily): minchoFirst(fallback) else: fallback
  for f in fb: fam.add FamilyRef(name: quoted(f))
  fam.add FamilyRef(name: "sans-serif", generic: true)
  @[StyleProp(kind: pkFamilies, families: fam), StyleProp(kind: pkSize, value: px),
    StyleProp(kind: pkWeight, value: float32(clamp(int(st.weight), 1, 1000))),
    StyleProp(kind: pkItalic, flag: st.italic),
    StyleProp(kind: pkFeatures, features: featureList(st, sc)),
    StyleProp(kind: pkVariations, variations: variationList(st)),
    StyleProp(kind: pkLetterSpacing, value: st.tracking / 1000 * px),
    StyleProp(kind: pkUnderline, flag: st.underline), StyleProp(kind: pkStrikethrough, flag: st.strikethrough),
    StyleProp(kind: pkBrush, brush: StyleBrush(run: idx, vclass: 0)),
    StyleProp(kind: pkLocale, locale: validLocale(st.language.get("")))]

proc syntheticBold*(matcherSays: bool; requested: uint16; faceWeight: float32): bool =
  ## Only a bold request (≥ 600) on a face that isn't bold (≤ 500) is emboldened.
  matcherSays and requested >= 600 and faceWeight <= 500

proc isClosing(u: int): bool =
  u in [0x3001, 0x3002, 0xFF0C, 0xFF0E, 0x300D, 0x300F, 0xFF09, 0x3015, 0x3011, 0x3009, 0x300B, 0x3019, 0x3017, 0xFF3D, 0xFF5D]

proc isOpening(u: int): bool =
  u in [0x300C, 0x300E, 0xFF08, 0x3014, 0x3010, 0x3008, 0x300A, 0x3018, 0x3016, 0xFF3B, 0xFF5B]

proc squeezePunctuation*(text: string; clusters: var openArray[ClusterInfo]; glyphs: var openArray[PlacedGlyph]): float32 =
  ## Japanese punctuation squeeze (JIS X 4051 style) for one line; returns the length removed.
  proc firstCp(c: ClusterInfo): int =
    if c.range[0] < c.range[1] and c.range[1] <= text.len: int(text.runeAt(c.range[0])) else: -1
  proc lastCp(c: ClusterInfo): int =
    if c.range[0] < c.range[1] and c.range[1] <= text.len:
      var i = c.range[1] - 1
      while i > c.range[0] and (uint8(text[i]) and 0xC0) == 0x80: dec i
      int(text.runeAt(i))
    else: -1
  var shifts = newSeq[float32](clusters.len)
  var total = 0'f32
  for i in 1 ..< clusters.len:
    let prev = clusters[i - 1]
    let cur = clusters[i]
    if prev.rtl or cur.rtl: continue
    let a = lastCp(prev)
    let b = firstCp(cur)
    if a < 0 or b < 0: continue
    let cut = if isClosing(a) and (isOpening(b) or isClosing(b)): prev.advance * 0.5'f32
              elif isOpening(a) and isOpening(b): cur.advance * 0.5'f32
              else: 0'f32
    if cut.classify in {fcNormal, fcSubnormal} and cut > 0: total += cut
    shifts[i] = total
  if total <= 0: return 0
  var starts: seq[float32]
  for c in clusters: starts.add c.x
  for i in 0 ..< clusters.len: clusters[i].x -= shifts[i]
  for g in glyphs.mitems:
    var idx = 0
    for k in countdown(starts.len - 1, 0):
      if starts[k] <= g.x + 1e-3'f32:
        idx = k
        break
    g.x -= shifts[idx]
  total

# Vertical metrics of a font instance: VORG, vhea/vmtx, else the ideographic em box.
type VMetrics = object
  face: Option[Face]
  scale, emOrigin: float32

proc newVMetrics(r: FaceRec; size: float32): VMetrics =
  result.face = r.tryLoad
  if result.face.isSome:
    let f = result.face.get
    let upem = max(float32(f.unitsPerEm), 1)
    let asc = f.ascent
    let desc = f.descent
    let ratio = if asc + desc > 0: clamp(asc / (asc + desc), 0'f32, 1'f32) else: 0.88'f32
    result.scale = size / upem
    result.emOrigin = size * ratio
  else:
    result.emOrigin = size * 0.88'f32

proc cmap(v: VMetrics; u: int): int =
  if v.face.isNone: return 0
  try: v.face.get.glyphIndex(u) except FontError: 0

proc origin(v: VMetrics; gid: int): float32 =
  ## Distance from the top of an upright glyph's cell to its baseline (px).
  if v.face.isNone: return v.emOrigin
  let f = v.face.get
  let vo = f.verticalOrigin(gid)
  if vo.isSome: return vo.get * v.scale
  let tsb = f.topSideBearing(gid)
  if tsb.isNone: return v.emOrigin
  var ymax = NegInf.float32
  try:
    for c in f.outline(gid):
      case c.verb
      of pvMove, pvLine: ymax = max(ymax, c.pts[2][1])
      of pvQuad: ymax = max(ymax, max(c.pts[0][1], c.pts[2][1]))
      of pvCubic: ymax = max(ymax, max(c.pts[0][1], max(c.pts[1][1], c.pts[2][1])))
      of pvClose: discard
  except FontError: return v.emOrigin
  if ymax == NegInf.float32: return v.emOrigin
  let r = (ymax + tsb.get) * v.scale
  if r.classify in {fcInf, fcNegInf, fcNan}: v.emOrigin else: r

# skrifa's FreeType-compatible 16.16 metric scaling (glyph bounds of TrueType fonts).
proc fixedScaled(size: float32; upem: int; value: int): float32 =
  if upem <= 0: return float32(value)
  let a = uint64(abs(int32(size * 64)))
  let b = uint64(upem)
  let s = int64(((a shl 16) + (b shr 1)) div b)
  let v = int64(value)
  let su = uint64(abs(s))
  let au = uint64(abs(v))
  let r = int64((su * au + 32) div 64)
  let signed = if (s < 0) != (v < 0): -r else: r
  float32(int32(signed)) * (1'f32 / 65536)

proc firstAscent(l: ParaLayout; line: int): Option[float32] =
  ## Height of the lowercase ascender ('d') of the tallest run on the line.
  var best = none(float32)
  for item in 0 ..< l.lineItemCount(line):
    let rd = l.runs[l.itemRun(line, item)]
    let f = rd.face.tryLoad
    if f.isNone: continue
    let face = f.get
    var h = none(float32)
    let g = (try: face.glyphIndex(ord('d')) except FontError: 0)
    if g != 0:
      if face.hasTable("CFF ") or face.hasTable("CFF2"):
        let bb = face.glyphBounds(g)
        if bb.isSome: h = some(bb.get[3] * rd.fontSize / float32(face.unitsPerEm))
      else:
        let e = face.glyfExtents(g)
        if e.isSome: h = some(fixedScaled(rd.fontSize, face.unitsPerEm, e.get[1]))
    if h.isNone:
      let s = rd.fontSize / float32(max(face.unitsPerEm, 1))
      h = if face.os2Version >= 2: some(float32(face.capHeight) * s) else: some(face.ascent * s * 0.75'f32)
    if best.isNone or h.get > best.get: best = h
  best

# ---------------------------------------------------------------- layout

type KernSlot = object
  run: int                 ## line item index
  start: int               ## byte offset in the paragraph text
  first, last: int         ## glyph ids, -1 when none
  face: FaceRec
  coords: seq[int16]
  size: float32
  rtl, blank, upright, tab: bool
  advance: float32

type TabWidth = object
  advance, scale, width: float32

proc tabWidths(l: ParaLayout; tabs: seq[int]; styles: seq[CharStyle]; indent, firstIndent, interval: float32): seq[TabWidth] =
  for _ in tabs: result.add TabWidth(advance: interval, scale: 1, width: interval)
  for li in 0 ..< l.lineCount:
    var pos = indent + (if li == 0: firstIndent else: 0'f32)
    var seen: seq[int]
    for gr in l.glyphRuns(li):
      if gr.runIndex in seen: continue
      seen.add gr.runIndex
      let st = if gr.styleIndex < l.styles.len: l.styles[gr.styleIndex].brush.run else: -1
      let scale = if st >= 0 and st < styles.len and styles[st].horizontalScale > 0: styles[st].horizontalScale else: 1'f32
      for c in l.visualClusters(li, gr.runIndex):
        let k = tabs.find(c.textStart)
        if k >= 0:
          let stop = nextTabStop(pos, interval)
          result[k] = TabWidth(advance: c.advance, scale: scale, width: stop - pos)
          pos = stop
        else:
          pos += c.advance * scale

proc layout*(ly: var Layouter; fonts: FontDb; t: TextLayer; dpi: float32): TextLayout =
  let k = if dpi > 0: dpi / 72 else: 1'f32
  let runs = t.charRuns
  let paras = t.paragraphRuns
  let vertical = t.orientation == orVertical
  var output = TextLayout(pxPerPt: k, vertical: vertical)
  # Resolve families (PostScript names from PSDs, unknown families).
  for r in runs:
    var s = r.style
    if s.postscriptName.isSome:
      let ps = s.postscriptName.get
      var present = false
      for f in fonts.faces(s.fontFamily):
        if f.postscriptName == some(ps): present = true
      if not present:
        let f = fonts.resolvePostscript(ps)
        if f.exact or (not fonts.hasFamily(s.fontFamily) and fonts.hasFamily(f.family)):
          s.fontFamily = f.family
          s.weight = f.weight
          s.italic = f.italic
    if not fonts.hasFamily(s.fontFamily): request(s.fontFamily)
    fonts.selectNamedFace(s)
    output.styles.add s
  var runStarts: seq[int]
  var acc = 0
  for r in runs:
    runStarts.add acc
    acc += r.len
  proc styleAt(off: int): int =
    result = 0
    for i in countdown(runStarts.len - 1, 0):
      if runStarts[i] <= off: return i
  var paraStarts: seq[int]
  acc = 0
  for p in paras:
    paraStarts.add acc
    acc += p.len
  proc paraStyleAt(off: int): ParagraphStyle =
    var i = 0
    for j in countdown(paraStarts.len - 1, 0):
      if paraStarts[j] <= off:
        i = j
        break
    paras[i].style
  let text = t.text
  let isBox = t.shape.kind == tsBox
  let boxRect = if isBox: (t.shape.x, t.shape.y, t.shape.width, t.shape.height) else: (0'f32, 0'f32, 0'f32, 0'f32)
  var prevBaseline = none(float32)
  var pendingSpace = 0'f32
  var stop = false
  let smallCaps = block:
    var v: seq[SmallCapsMode]
    for st in output.styles:
      v.add smallCapsMode(st.caps, fonts.selectedFaceHasFeature(st.fontFamily, st.weight, st.italic, "smcp"))
    v
  for pi, prange in splitParagraphs(text):
    if stop: break
    let ps = paraStyleAt(prange[0])
    let contentEnd = stripBreak(text, prange)
    let content = text[prange[0] ..< contentEnd]
    let prefix = case ps.direction
      of tdAuto: ""
      of tdLtr: Lrm
      of tdRtl: Rlm
    var ptext = prefix
    var syntheticSmallCaps: seq[((int, int), float32)]
    var tabs: seq[int]
    let tabPx = DefaultTabStopPt * k
    var i = 0
    while i < content.len:
      let r = content.runeAt(i)
      let ch = int(r)
      let n = r.size
      if ch == ForcedLineBreak:
        ptext.add '\n'
      elif ch == 9:
        tabs.add ptext.len
        ptext.add ' '
      else:
        let si = styleAt(prange[0] + i)
        let style = output.styles[si]
        var done = false
        if style.caps == capsAllCaps or smallCaps[si] == scSynthetic:
          let up = $r.toUpper
          if up.len == n:
            ptext.add up
            if style.caps == capsSmallCaps and r.isLower:
              let e = ptext.len
              syntheticSmallCaps.add ((e - up.len, e), style.sizePt * k * SyntheticSmallCapsScale)
            done = true
        if not done: ptext.add $r
      i += n
    let si0 = styleAt(prange[0])
    let firstPx = output.styles[si0].sizePt * k
    requestForText(ptext)
    let fallback = fonts.fallbackStack()
    proc build(tabSpacing: seq[float32]): ParaLayout =
      var b = initParaBuilder(ptext)
      for p in styleProps(output.styles[si0], k, fallback, si0, smallCaps[si0]): b.pushDefault(p)
      for ri, st in output.styles:
        let rs = runStarts[ri]
        let a = max(rs, prange[0])
        let z = min(rs + runs[ri].len, contentEnd)
        if a >= z: continue
        let r0 = a - prange[0] + prefix.len
        let r1 = z - prange[0] + prefix.len
        for p in styleProps(st, k, fallback, ri, smallCaps[ri]): b.push(p, r0, r1)
        if vertical:
          let piece = ptext[r0 ..< r1]
          var seg = none((int, VClass))
          proc flush(b: var ParaBuilder; frm, to: int; cls: VClass) =
            if cls == vcRotate or frm >= to: return
            var feats = featureList(st, smallCaps[ri])
            feats.add ("vert", 1)
            b.push(StyleProp(kind: pkFeatures, features: feats), r0 + frm, r0 + to)
            b.push(StyleProp(kind: pkBrush, brush: StyleBrush(run: ri, vclass: ord(cls))), r0 + frm, r0 + to)
          var j = 0
          while j < piece.len:
            let cls = verticalClass(int(piece.runeAt(j)))
            if seg.isSome:
              if seg.get[1] != cls:
                flush(b, seg.get[0], j, seg.get[1])
                seg = some((j, cls))
            else: seg = some((j, cls))
            j += piece.runeLenAt(j)
          if seg.isSome: flush(b, seg.get[0], piece.len, seg.get[1])
      for (rng, size) in syntheticSmallCaps: b.push(StyleProp(kind: pkSize, value: size), rng[0], rng[1])
      for ti, at in tabs:
        if ti < tabSpacing.len: b.push(StyleProp(kind: pkLetterSpacing, value: tabSpacing[ti]), at, at + 1)
      b.build(fonts)
    let indentStart = ps.startIndentPt * k
    let indentEnd = ps.endIndentPt * k
    let firstIndent = ps.firstLineIndentPt * k
    let (lineOrigin, lineLen) = if vertical: (boxRect[1], boxRect[3]) else: (boxRect[0], boxRect[2])
    let avail = if isBox: some(max(lineLen - indentStart - indentEnd, 1'f32)) else: none(float32)
    proc breakLines(pl: var ParaLayout) =
      if firstIndent != 0: pl.setTextIndent(firstIndent)
      pl.breakAllLines(avail)
    let wide = if isBox: tabPx else: 0'f32
    var pl = build(newSeqWith(tabs.len, wide))
    breakLines(pl)
    if isBox and tabs.len > 0:
      let first = tabWidths(pl, tabs, output.styles, indentStart, firstIndent, tabPx)
      var want: seq[float32]
      for t2 in first: want.add t2.width
      for _ in 0 ..< 4:
        var spacing: seq[float32]
        for ti, t2 in first: spacing.add want[ti] / t2.scale - (t2.advance - tabPx)
        var nxt = build(spacing)
        breakLines(nxt)
        var got: seq[float32]
        for t2 in tabWidths(nxt, tabs, output.styles, indentStart, firstIndent, tabPx): got.add t2.width
        var same = true
        for ti in 0 ..< min(got.len, want.len):
          if abs(got[ti] - want[ti]) >= 0.01'f32: same = false
        if same:
          pl = nxt
          break
        want = got
    let alignment = if isBox:
        case ps.align
        of taLeft: alLeft
        of taCenter: alCenter
        of taRight: alRight
        else: alJustify
      else: alLeft
    pl.align(alignment, not isBox)
    if prevBaseline.isSome: pendingSpace += ps.spaceBeforePt * k
    let nlines = pl.lineCount
    for li in 0 ..< nlines:
      let line = pl.line(li)
      let m = line.metrics
      let gruns = pl.glyphRuns(li)
      var leading = 0'f32
      for gr in gruns:
        let st = output.styles[pl.styles[gr.styleIndex].brush.run]
        let px = pl.runs[gr.run].fontSize
        leading = max(leading, if st.leadingPt.isSome: st.leadingPt.get * k else: ps.autoLeading * px)
      if leading == 0:
        let st = output.styles[styleAt(prange[0])]
        leading = if st.leadingPt.isSome: st.leadingPt.get * k else: ps.autoLeading * st.sizePt * k
      var colHalf = 0'f32
      for gr in gruns: colHalf = max(colHalf, pl.runs[gr.run].fontSize * 0.5'f32)
      if colHalf <= 0: colHalf = firstPx * 0.5'f32
      let (ascent, descent) = if vertical: (colHalf, colHalf)
                              elif m.ascent > 0 or m.descent > 0: (m.ascent, m.descent)
                              else: (firstPx * 0.8'f32, firstPx * 0.2'f32)
      let baseline = if prevBaseline.isNone:
          if isBox and vertical: -(boxRect[0] + boxRect[2]) + colHalf
          elif isBox: boxRect[1] + pl.firstAscent(li).get(ascent)
          else: 0'f32
        else: prevBaseline.get + leading + pendingSpace
      pendingSpace = 0
      let overflow = if vertical: baseline + descent > -boxRect[0] + 0.5'f32 else: baseline + descent > boxRect[1] + boxRect[3] + 0.5'f32
      if isBox and overflow:
        stop = true
        break
      prevBaseline = some(baseline)
      let lastLine = li + 1 == nlines
      let adv = m.advance - m.trailingWhitespace
      proc mapOff(o: int): int = min(prange[0] + max(o - prefix.len, 0), contentEnd)
      # Kerning slots per line item, in visual cluster order.
      var slots: seq[KernSlot]
      var lineRuns: seq[int]
      for gr in gruns:
        if gr.runIndex in lineRuns: continue
        lineRuns.add gr.runIndex
        let stIdx = pl.styles[gr.styleIndex].brush.run
        let hs = if stIdx < output.styles.len and output.styles[stIdx].horizontalScale > 0: output.styles[stIdx].horizontalScale else: 1'f32
        let rd = pl.runs[gr.run]
        for c in pl.visualClusters(li, gr.runIndex):
          let firstG = if c.glyphs.len > 0: c.glyphs[0].id else: -1
          let lastG = if c.glyphs.len > 0: c.glyphs[^1].id else: firstG
          slots.add KernSlot(run: gr.runIndex, start: c.textStart, first: firstG, last: lastG, face: rd.face,
                             coords: rd.coords, size: rd.fontSize, rtl: c.rtl,
                             blank: firstG < 0 or c.spaceOrNbsp or c.textEnd <= prefix.len,
                             upright: vertical and pl.styles[c.styleIndex].brush.vclass != ord(vcRotate),
                             advance: c.advance * hs, tab: c.textStart in tabs)
      var kernPx = newSeq[float32](slots.len)
      for j in 0 ..< max(slots.len - 1, 0):
        let a = slots[j]
        let b = slots[j + 1]
        if a.start < prefix.len: continue
        let st = output.styles[styleAt(mapOff(a.start))]
        let nx = output.styles[styleAt(mapOff(b.start))]
        var units = if st.kern.classify in {fcNormal, fcZero, fcSubnormal, fcNegZero}: st.kern else: 0'f32
        if st.kerning == kOptical and nx.kerning == kOptical and st.kern == 0 and nx.kern == 0 and
           not a.rtl and not b.rtl and not a.blank and not b.blank and not a.upright and not b.upright and
           a.face == b.face and a.coords == b.coords and abs(a.size - b.size) < 1e-3'f32 and a.last >= 0 and b.first >= 0:
          let f = a.face.tryLoad
          if f.isSome:
            let kk = ly.optical.pair(a.face.blobId, f.get, a.face.index, a.coords, a.last, b.first)
            if kk.isSome: units += kk.get + sizeAdjust(st.sizePt)
        kernPx[j] = units / 1000 * a.size
      # Tabs: the kerning after a tab takes the pen to the next stop.
      var pos = indentStart + (if li == 0: firstIndent else: 0'f32)
      var trailingTabs = 0'f32
      for j, s in slots:
        if s.tab:
          kernPx[j] = nextTabStop(pos, tabPx) - pos - s.advance
          var allBlank = true
          for n in j + 1 ..< slots.len:
            if not slots[n].blank: allBlank = false
          if allBlank: trailingTabs += kernPx[j]
        pos += s.advance + kernPx[j]
      var lineKern = 0'f32
      for x in kernPx: lineKern += x
      lineKern -= trailingTabs
      # Per line item: glyph → slot, glyphs emitted, first slot whose kerning isn't applied yet.
      type Cursor = object
        run: int
        g2s: seq[int]
        emitted, nextSlot: int
      var cursors: seq[Cursor]
      for gr in gruns:
        var found = false
        for c in cursors:
          if c.run == gr.runIndex: found = true
        if found: continue
        var base = 0
        for si, s in slots:
          if s.run == gr.runIndex:
            base = si
            break
        var g2s: seq[int]
        for ci, c in pl.visualClusters(li, gr.runIndex):
          for _ in c.glyphs: g2s.add base + ci
        cursors.add Cursor(run: gr.runIndex, g2s: g2s, nextSlot: base)
      let kernAlign = if isBox:
          case ps.align
          of taCenter: lineKern / 2
          of taRight: lineKern
          else: 0'f32
        else: 0'f32
      let dx = if isBox:
          let base = lineOrigin + indentStart - kernAlign
          let slack = avail.get(0) - adv - lineKern
          base + (if lastLine:
            case ps.align
            of taJustifyCenter: slack * 0.5'f32 - m.offset
            of taJustifyRight: slack - m.offset
            else: 0'f32
          else: 0'f32)
        else:
          let target = case ps.align
            of taCenter, taJustifyCenter: -(adv + lineKern) / 2
            of taRight, taJustifyRight: -(adv + lineKern)
            else: indentStart
          target - m.offset
      let justifyAll = isBox and lastLine and ps.align == taJustifyAll
      let lineIndex = output.lines.len
      let g0 = output.glyphs.len
      let c0 = output.clusters.len
      type VGlyph = object
        upright: bool
        advance, origin, centre: float32
      var vinfo: seq[VGlyph]
      var extra = 0'f32
      var seenRuns: seq[int]
      for gr in gruns:
        let rd = pl.runs[gr.run]
        let si = pl.styles[gr.styleIndex].brush.run
        let st = output.styles[si]
        let hs = if st.horizontalScale > 0: st.horizontalScale else: 1'f32
        let face = output.faces.len
        output.faces.add GlyphFace(face: rd.face, coords: rd.coords, sizePx: rd.fontSize,
                                   embolden: syntheticBold(rd.synthesis.embolden, st.weight, rd.attrsWeight),
                                   skewDeg: rd.synthesis.skew)
        let runX0 = dx + gr.offset + extra
        if gr.runIndex notin seenRuns:
          seenRuns.add gr.runIndex
          var cx = runX0
          var base = 0
          for si2, s in slots:
            if s.run == gr.runIndex:
              base = si2
              break
          for ci, c in pl.visualClusters(li, gr.runIndex):
            let a = c.advance * hs + (if base + ci < kernPx.len: kernPx[base + ci] else: 0'f32)
            if c.textEnd > prefix.len or prefix.len == 0:
              output.clusters.add ClusterInfo(range: (mapOff(c.textStart), mapOff(c.textEnd)), x: cx, advance: a,
                                              line: lineIndex, rtl: c.rtl)
            cx += a
        let rm = rd.metrics
        let vm = if vertical: some(newVMetrics(rd.face, rd.fontSize)) else: none(VMetrics)
        let cls = pl.styles[gr.styleIndex].brush.vclass
        var plain: seq[int]
        if vm.isSome and cls == 2:
          let (ts, te) = pl.itemTextRange(li, gr.runIndex)
          var j = ts
          while j < te and j < ptext.len:
            let g = vm.get.cmap(int(ptext.runeAt(j)))
            if g != 0: plain.add g
            j += ptext.runeLenAt(j)
        var pen = gr.offset
        var cur = -1
        for ci2, c in cursors:
          if c.run == gr.runIndex: cur = ci2
        for g in gr.glyphs:
          if cur >= 0:
            if cursors[cur].emitted < cursors[cur].g2s.len:
              let slot = cursors[cur].g2s[cursors[cur].emitted]
              while cursors[cur].nextSlot < slot:
                if cursors[cur].nextSlot < kernPx.len: extra += kernPx[cursors[cur].nextSlot]
                inc cursors[cur].nextSlot
            inc cursors[cur].emitted
          output.glyphs.add PlacedGlyph(face: face, id: g.id, x: dx + pen + g.x + extra, y: baseline + g.y,
                                        style: si, orient: goHorizontal)
          if vm.isSome:
            let upright = cls == 1 or (cls == 2 and g.id notin plain)
            vinfo.add VGlyph(upright: upright, advance: g.advance * hs,
                             origin: (if upright: vm.get.origin(g.id) else: 0'f32),
                             centre: (rm.ascent - rm.descent) * 0.5'f32)
          extra += g.advance * (hs - 1)
          pen += g.advance
        if cur >= 0 and cursors[cur].emitted >= cursors[cur].g2s.len:
          var e = cursors[cur].nextSlot
          for s2 in countdown(slots.len - 1, 0):
            if slots[s2].run == cursors[cur].run:
              e = s2 + 1
              break
          while cursors[cur].nextSlot < e:
            if cursors[cur].nextSlot < kernPx.len: extra += kernPx[cursors[cur].nextSlot]
            inc cursors[cur].nextSlot
        let runX1 = dx + gr.offset + gr.advance + extra
        let shift = st.baselineShiftPt * k
        let ust = pl.styles[gr.styleIndex]
        if vertical:
          let half = rd.fontSize * 0.5'f32
          if ust.underline:
            let v1 = baseline - half
            output.decorations.add DecorationRect(x0: -v1, y0: runX0, x1: -(v1 - max(rm.underlineSize, 1)), y1: runX1, style: si)
          if ust.strikethrough:
            let sz = max(rm.strikethroughSize, 1)
            output.decorations.add DecorationRect(x0: -(baseline + sz * 0.5'f32), y0: runX0, x1: -(baseline - sz * 0.5'f32), y1: runX1, style: si)
        elif ust.underline:
          let y0 = baseline - rm.underlineOffset - shift
          output.decorations.add DecorationRect(x0: runX0, y0: y0, x1: runX1, y1: y0 + max(rm.underlineSize, 1), style: si)
        if not vertical and ust.strikethrough:
          let y0 = baseline - rm.strikethroughOffset - shift
          output.decorations.add DecorationRect(x0: runX0, y0: y0, x1: runX1, y1: y0 + max(rm.strikethroughSize, 1), style: si)
      if vertical:
        extra -= squeezePunctuation(text, output.clusters.toOpenArray(c0, output.clusters.len - 1),
                                    output.glyphs.toOpenArray(g0, output.glyphs.len - 1))
      if justifyAll:
        let slack = avail.get(0) - (adv + extra)
        let n = output.clusters.len - c0
        if n > 1 and slack > 0:
          let step = slack / float32(n - 1)
          var starts: seq[float32]
          for c in output.clusters[c0 .. ^1]: starts.add c.x
          for i2 in c0 ..< output.clusters.len: output.clusters[i2].x += step * float32(i2 - c0)
          for gi in g0 ..< output.glyphs.len:
            var idx = 0
            for s2 in countdown(starts.len - 1, 0):
              if starts[s2] <= output.glyphs[gi].x + 1e-3'f32:
                idx = s2
                break
            output.glyphs[gi].x += step * float32(idx)
          extra += slack
      if vertical:
        for gi in g0 ..< output.glyphs.len:
          let v = vinfo[gi - g0]
          let (u, cross) = (output.glyphs[gi].x, output.glyphs[gi].y)
          if v.upright:
            output.glyphs[gi].x = -baseline - v.advance * 0.5'f32
            output.glyphs[gi].y = u + v.origin
            output.glyphs[gi].orient = goUpright
          else:
            output.glyphs[gi].x = -(cross + v.centre)
            output.glyphs[gi].y = u
            output.glyphs[gi].orient = goRotated
      let x0 = dx + m.offset
      output.lines.add LineInfo(range: (mapOff(line.textStart), min(mapOff(line.textEnd), contentEnd)),
                                baseline: baseline, x0: x0, x1: x0 + adv + extra - trailingTabs,
                                ascent: ascent, descent: descent, paragraph: pi)
    pendingSpace += ps.spaceAfterPt * k
  output
