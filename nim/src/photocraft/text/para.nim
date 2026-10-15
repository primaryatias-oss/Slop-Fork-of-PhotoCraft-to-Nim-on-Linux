## Paragraph layout: the parts of parley the type engine uses, ported. Text with ranged styles is
## analyzed (line break opportunities, scripts, bidi levels), itemized and shaped run by run with
## per-cluster font fallback, then broken into lines and aligned. Positions are in pixels with y
## down; glyph runs come out per line in visual order.

import std/[options, unicode, algorithm, math]
import ../font/[sfnt, shape]
import uprops, segment, bidi, fonts
import ../rmath

type
  FamilyRef* = object
    name*: string
    generic*: bool          ## a CSS generic family ("sans-serif") rather than a named one

  StyleBrush* = object
    run*: int               ## character run index (into the type layer's styles)
    vclass*: int            ## vertical-type class (0 in horizontal type)

  ParaStyle* = object
    families*: seq[FamilyRef]
    size*: float32
    weight*: float32
    italic*: bool
    features*: seq[ShapeFeature]
    variations*: seq[(string, float32)]
    letterSpacing*: float32
    wordSpacing*: float32
    underline*, strikethrough*: bool
    brush*: StyleBrush
    locale*: string

  PropKind* = enum pkFamilies, pkSize, pkWeight, pkItalic, pkFeatures, pkVariations, pkLetterSpacing,
                   pkUnderline, pkStrikethrough, pkBrush, pkLocale
  StyleProp* = object
    case kind*: PropKind
    of pkFamilies: families*: seq[FamilyRef]
    of pkSize, pkWeight, pkLetterSpacing: value*: float32
    of pkItalic, pkUnderline, pkStrikethrough: flag*: bool
    of pkFeatures: features*: seq[ShapeFeature]
    of pkVariations: variations*: seq[(string, float32)]
    of pkBrush: brush*: StyleBrush
    of pkLocale: locale*: string

  ParaBuilder* = object
    text*: string
    root: ParaStyle
    props: seq[(StyleProp, int, int)]

  Boundary = enum bNone, bWord, bLine, bMandatory

  CharInfo = object
    cp: int
    byteOff: int
    boundary: Boundary
    script: Script
    styleIndex: int
    contributes, control, emoji, forceNormalize, variationSelector: bool

  RunMetrics* = object
    ascent*, descent*, leading*: float32
    underlineOffset*, underlineSize*: float32
    strikethroughOffset*, strikethroughSize*: float32
    lineHeight*: float32
    xHeight*, capHeight*: Option[float32]

  Glyph* = object
    id*: int
    styleIndex*: int
    x*, y*, advance*: float32

  Cluster = object
    boundary: Boundary
    sourceChar: int
    ligStart, ligComponent: bool
    styleIndex: int
    glyphLen: int            ## -1: one glyph stored inline (`glyphOffset` is its id)
    textLen: int
    glyphOffset: int
    textOffset: int
    advance: float32

  RunData* = object
    face*: FaceRec
    fontSize*: float32
    attrsWeight*: float32    ## the requested weight the face was matched for
    synthesis*: Synthesis
    coords*: seq[int16]
    textStart, textEnd: int
    bidiLevel*: int
    clusterStart, clusterEnd: int
    glyphStart: int
    metrics*: RunMetrics
    wordSpacing, letterSpacing: float32
    advance: float32

  BreakReason = enum brNone, brRegular, brExplicit, brEmergency

  LineItem = object
    run: int
    bidiLevel: int
    advance: float32
    isWhitespace, hasTrailingWhitespace: bool
    textStart, textEnd: int
    clusterStart, clusterEnd: int

  LineMetrics* = object
    ascent*, descent*, leading*, lineHeight*, baseline*, offset*, advance*, trailingWhitespace*: float32
    inlineMin*, inlineMax*, blockMin*, blockMax*: float32

  LineData = object
    textStart, textEnd: int
    itemStart, itemEnd: int
    metrics: LineMetrics
    breakReason: BreakReason
    maxAdvance: float32
    numSpaces: int
    indent: float32

  Alignment* = enum alStart, alEnd, alLeft, alCenter, alRight, alJustify

  ParaLayout* = object
    text*: string
    baseLevel*: int
    textLen: int
    styles*: seq[ParaStyle]
    runs*: seq[RunData]
    items: seq[int]            ## run indices in logical order
    clusters: seq[Cluster]
    glyphs: seq[Glyph]
    lines: seq[LineData]
    lineItems: seq[LineItem]
    layoutMaxAdvance: float32
    width*, fullWidth*, height*: float32
    alignedJustified: bool
    indentAmount: float32

# ---------------------------------------------------------------- styles

proc apply(s: var ParaStyle; p: StyleProp) =
  case p.kind
  of pkFamilies: s.families = p.families
  of pkSize: s.size = p.value
  of pkWeight: s.weight = p.value
  of pkItalic: s.italic = p.flag
  of pkFeatures: s.features = p.features
  of pkVariations: s.variations = p.variations
  of pkLetterSpacing: s.letterSpacing = p.value
  of pkUnderline: s.underline = p.flag
  of pkStrikethrough: s.strikethrough = p.flag
  of pkBrush: s.brush = p.brush
  of pkLocale: s.locale = p.locale

proc defaultParaStyle*(): ParaStyle =
  ParaStyle(families: @[FamilyRef(name: "sans-serif", generic: true)], size: 16, weight: 400)

proc initParaBuilder*(text: string): ParaBuilder =
  ParaBuilder(text: text, root: defaultParaStyle())

proc pushDefault*(b: var ParaBuilder; p: StyleProp) = b.root.apply(p)

proc push*(b: var ParaBuilder; p: StyleProp; start, stop: int) =
  b.props.add (p, min(max(start, 0), b.text.len), min(max(stop, 0), b.text.len))

proc resolveStyles(b: ParaBuilder): (seq[ParaStyle], seq[(int, int, int)]) =
  ## Style table and runs (byte start, end, style index), merging equal neighbours (parley's
  ## `RangedStyleBuilder::finish`).
  let n = b.text.len
  if n == 0: return (@[b.root], @[(0, 0, 0)])
  # Byte offsets where any range starts or ends split the text into pieces of one style.
  var cuts = @[0, n]
  for (_, s, e) in b.props:
    if s < e:
      cuts.add s
      cuts.add e
  cuts.sort()
  var pieces: seq[(int, int, ParaStyle)]
  for i in 0 ..< cuts.len - 1:
    let (s, e) = (cuts[i], cuts[i + 1])
    if s >= e: continue
    var st = b.root
    for (p, ps, pe) in b.props:
      if ps <= s and pe >= e and ps < pe: st.apply(p)
    pieces.add (s, e, st)
  var styles: seq[ParaStyle]
  var runs: seq[(int, int, int)]
  for (s, e, st) in pieces:
    if styles.len > 0 and styles[^1] == st:
      runs[^1][1] = e
    else:
      styles.add st
      runs.add (s, e, styles.len - 1)
  (styles, runs)

# ---------------------------------------------------------------- analysis

proc isVariationSelector(u: int): bool =
  u in 0xFE00 .. 0xFE0F or u in 0xE0100 .. 0xE01EF or u in 0x180B .. 0x180D or u == 0x180F

proc contributesToShaping(gc: GeneralCategory; sc: Script): bool =
  if gc in {gcCc, gcZl, gcZp}: return false
  not (gc == gcCf and sc != scInherited)

proc analyze(text: string; styleRuns: seq[(int, int, int)]; infos: var seq[CharInfo];
             levels: var seq[uint8]; baseLevel: var int) =
  let t = if text.len == 0: " " else: text
  var cps: seq[int]
  var offs: seq[int]
  var i = 0
  for r in t.runes:
    cps.add int(r)
    offs.add i
    i += r.size
  let lb = lineBreaks(cps)
  infos.setLen(0)
  var mandatoryNext = false
  var needsBidi = false
  for k, u in cps:
    var b = if k > 0 and lb[k]: bLine else: bNone
    if mandatoryNext: b = bMandatory
    let gc = generalCategory(u)
    let sc = script(u)
    let gb = graphemeBreak(u)
    let vs = isVariationSelector(u)
    let force = (gb == gbExtend and u != 0x200C and not vs) or gb == gbSpacingMark
    needsBidi = needsBidi or needsBidiResolution(bidiClass(u))
    infos.add CharInfo(cp: u, byteOff: offs[k], boundary: b, script: sc, contributes: contributesToShaping(gc, sc),
                       control: gc == gcCc, emoji: isExtendedPictographic(u), forceNormalize: force,
                       variationSelector: vs)
    mandatoryNext = isMandatoryBreak(u)
  # Style index per character.
  var ci = 0
  for (s, e, si) in styleRuns:
    while ci < infos.len and infos[ci].byteOff < e:
      infos[ci].styleIndex = si
      inc ci
  if needsBidi:
    var r: BidiResolver
    r.resolve(cps)
    levels = r.levels
    baseLevel = r.baseLevel
  else:
    levels = newSeq[uint8](cps.len)
    baseLevel = 0

# ---------------------------------------------------------------- fonts

type
  SelectedFont = object
    face: FaceRec
    synthesis: Synthesis
    attrsWeight: float32

proc sameFont(a, b: SelectedFont): bool =
  a.face == b.face and a.synthesis == b.synthesis

proc stackFamilies(db: FontDb; families: seq[FamilyRef]): seq[int] =
  for f in families:
    if f.generic:
      for i in db.genericFamilies(f.name):
        if i notin result: result.add i
    else:
      let i = db.familyIndex(f.name)
      if i >= 0 and i notin result: result.add i

proc mapRatio(face: Face; chars: openArray[int]; contributes: openArray[bool]; control: openArray[bool];
              mapLenIn: int; hasContributing: bool): float32 =
  ## parley's `Mapper::map`: share of the cluster's shaping characters the font covers.
  let mapLen = mapLenIn
  if mapLen == 0: return 1
  var mapped = 0
  for k, c in chars:
    let g = (try: face.glyphIndex(c) except FontError: 0)
    if not contributes[k]:
      if not hasContributing: inc mapped
    elif g != 0: inc mapped
  float32(mapped) / float32(mapLen)

proc selectFont(db: FontDb; style: ParaStyle; cluster: openArray[CharInfo]): Option[SelectedFont] =
  ## The first font in the style's stack that covers the whole cluster (as typed, composed or
  ## decomposed); else the one covering the most of it; else the first font of the stack.
  var chars: seq[int]
  var contributes, control: seq[bool]
  var mapLen = 0
  var force = false
  for c in cluster:
    chars.add c.cp
    let contrib = c.contributes and not (c.emoji and c.variationSelector)
    contributes.add contrib
    control.add c.control
    if contrib: inc mapLen
    force = force or c.forceNormalize
  let hasContributing = mapLen > 0
  # Alternative forms: NFC of a pair, NFD of a single character.
  var nfc: seq[int]
  if chars.len == 2:
    let c = compose(chars[0], chars[1])
    if c != 0: nfc = @[c]
  var nfd: seq[int]
  if chars.len == 1:
    let (a, b) = decompose(chars[0])
    if a != 0 and b != 0: nfd = @[a, b]
  proc formRatio(face: Face; form: seq[int]): float32 =
    var contrib, ctl: seq[bool]
    var n = 0
    for c in form:
      let gc = generalCategory(c)
      let ok = contributesToShaping(gc, script(c))
      contrib.add ok
      ctl.add gc == gcCc
      if gc != gcCc: inc n
    var has = false
    for x in contrib: has = has or x
    mapRatio(face, form, contrib, ctl, max(n, 1), has)
  let width = 1'f32
  let fstyle = if style.italic: fsItalic else: fsNormal
  var best: Option[SelectedFont]
  var bestRatio = 0'f32
  for fi in stackFamilies(db, style.families):
    let faces = db.familyFaces(fi)
    let idx = matchFace(faces, width, fstyle, 0, style.weight)
    if idx < 0: continue
    let rec = faces[idx]
    let face = rec.tryLoad
    if face.isNone: continue
    let sel = SelectedFont(face: rec, synthesis: rec.synthesis(width, fstyle, 0, style.weight),
                           attrsWeight: style.weight)
    let prev = bestRatio
    var r: float32
    var complete = false
    if force and nfc.len > 0:
      r = formRatio(face.get, nfc)
      if r > bestRatio:
        bestRatio = r
        if r >= 1: complete = true
    if not complete:
      r = mapRatio(face.get, chars, contributes, control, max(mapLen, 1), hasContributing)
      if r > bestRatio:
        bestRatio = r
        if r >= 1: complete = true
    if not complete and nfd.len > 0:
      r = formRatio(face.get, nfd)
      if r > bestRatio:
        bestRatio = r
        if r >= 1: complete = true
      if not complete and not force and nfc.len > 0:
        r = formRatio(face.get, nfc)
        if r > bestRatio:
          bestRatio = r
          if r >= 1: complete = true
    if complete: return some(sel)
    if bestRatio > prev: best = some(sel)
    elif best.isNone: best = some(sel)
  best

proc normalizedCoords(rec: FaceRec; syn: Synthesis; variations: seq[(string, float32)]): seq[int16] =
  ## Normalized variation coordinates (F2Dot14) for the face's axes (no `avar` mapping).
  if rec.axes.len == 0: return
  var any = false
  result = newSeq[int16](rec.axes.len)
  for i, a in rec.axes:
    var v = a.defaultValue
    for (t, x) in syn.vars:
      if t == a.tag: v = x
    for (t, x) in variations:
      if t == a.tag: v = x
    v = clamp(v, a.minValue, a.maxValue)
    var n = 0'f32
    if v < a.defaultValue and a.defaultValue > a.minValue: n = (v - a.defaultValue) / (a.defaultValue - a.minValue)
    elif v > a.defaultValue and a.maxValue > a.defaultValue: n = (v - a.defaultValue) / (a.maxValue - a.defaultValue)
    result[i] = int16(round(n * 16384))
    if result[i] != 0: any = true
  if not any: result.setLen(0)

proc runMetrics(face: Face; size: float32): RunMetrics =
  ## skrifa `Metrics` at `size` turned into parley's run metrics (HarfBuzz defaults for missing
  ## decorations).
  let upem = float32(max(face.unitsPerEm, 1))
  let s = size / upem
  let ascent = face.ascent * s
  let descent = -face.descent * s     # skrifa's sign: negative below the baseline
  let leading = face.lineGap * s
  if face.hasTable("post"):
    result.underlineOffset = float32(face.underlinePosition) * s
    result.underlineSize = float32(face.underlineThickness) * s
  else:
    result.underlineOffset = upem / 18
    result.underlineSize = upem / 18
  if face.os2Version >= 0 and face.hasTable("OS/2"):
    result.strikethroughOffset = float32(face.strikeoutPosition) * s
    result.strikethroughSize = float32(face.strikeoutSize) * s
  else:
    result.strikethroughOffset = ascent / 2
    result.strikethroughSize = upem / 18
  result.ascent = ascent
  result.descent = -descent
  result.leading = leading
  result.lineHeight = (ascent - descent + leading) * 1
  if face.os2Version >= 2:
    result.xHeight = some(float32(face.xHeight) * s)
    result.capHeight = some(float32(face.capHeight) * s)

# ---------------------------------------------------------------- shaping

proc isWhitespaceKind(c: int): int =
  ## 0 none, 1 space, 2 no-break space, 3 tab, 4 newline (parley `Whitespace`).
  case c
  of 0x20: 1
  of 0xA0: 2
  of 0x09: 3
  of 0x0A, 0x0D, 0x2028, 0x2029: 4
  else: 0

proc pushCluster(clusters: var seq[Cluster]; info: CharInfo; ch: (int, int); glyphOffset: int; advance: float32;
                 totalGlyphs: int; kind: int; inline: int) =
  ## kind: 0 regular, 1 ligature start, 2 ligature component, 3 newline.
  let glyphLen = totalGlyphs - glyphOffset
  var c = Cluster(boundary: info.boundary, sourceChar: ch[1], styleIndex: info.styleIndex,
                  textLen: Rune(ch[1]).size, textOffset: ch[0], advance: advance,
                  ligStart: kind == 1, ligComponent: kind == 2)
  case kind
  of 2:
    c.glyphLen = 0
    c.glyphOffset = 0
  of 3:
    c.glyphLen = 0
    c.glyphOffset = 0
    c.advance = 0
  else:
    if inline >= 0:
      c.glyphLen = -1
      c.glyphOffset = inline
    else:
      c.glyphLen = glyphLen
      c.glyphOffset = glyphOffset
  clusters.add c

proc processClusters(l: var ParaLayout; rtl: bool; scale: float32; glyphs: seq[HbGlyph];
                     infos: openArray[CharInfo]; chars: seq[(int, int)]): float32 =
  ## parley `process_clusters`: HarfRust glyphs (visual order) → clusters (in visual order here;
  ## the caller reverses them for right-to-left runs) and glyphs.
  var it = 0
  var order = chars
  if rtl: order.reverse()
  template nextChar(): (int, int) =
    (let v = order[min(it, order.len - 1)]; inc it; v)
  var clusterStartChar = nextChar()
  var totalGlyphs = 0
  var clusterGlyphOffset = 0
  var clusterId = glyphs[0].cluster
  var info = infos[clusterId]
  var runAdvance = 0'f32
  var clusterAdvance = 0'f32
  var pending: Option[Glyph]
  var lastClusterId = if rtl: infos.len else: 0
  proc numComponents(next, cur, last: int): int = (if rtl: last - cur else: next - cur)
  for g in glyphs:
    if clusterId != g.cluster:
      runAdvance += clusterAdvance
      let nc = numComponents(g.cluster, clusterId, lastClusterId)
      clusterAdvance /= float32(max(nc, 1))
      let isNewline = isWhitespaceKind(clusterStartChar[1]) == 4
      let kind = if nc > 1: 1 elif isNewline: 3 else: 0
      var inline = -1
      if kind == 0:
        if pending.isSome:
          inline = pending.get.id
          pending = none(Glyph)
      else:
        if pending.isSome:
          l.glyphs.add pending.get
          inc totalGlyphs
          pending = none(Glyph)
      pushCluster(l.clusters, info, clusterStartChar, clusterGlyphOffset, clusterAdvance, totalGlyphs, kind, inline)
      clusterGlyphOffset = totalGlyphs
      if nc > 1:
        for k in 1 ..< nc:
          clusterStartChar = nextChar()
          if isWhitespaceKind(clusterStartChar[1]) == 1: break
          let ci = if rtl: clusterId + nc - k else: clusterId + k
          pushCluster(l.clusters, infos[min(ci, infos.len - 1)], clusterStartChar, clusterGlyphOffset,
                      clusterAdvance, totalGlyphs, 2, -1)
      clusterStartChar = nextChar()
      clusterAdvance = 0
      lastClusterId = clusterId
      clusterId = g.cluster
      info = infos[min(clusterId, infos.len - 1)]
      pending = none(Glyph)
    let glyph = Glyph(id: g.gid, styleIndex: info.styleIndex, x: float32(g.xOffset) * scale,
                      y: -float32(g.yOffset) * scale, advance: float32(g.xAdvance) * scale)
    clusterAdvance += glyph.advance
    if pending.isSome:
      l.glyphs.add pending.get
      inc totalGlyphs
      pending = none(Glyph)
    if totalGlyphs == clusterGlyphOffset and glyph.x == 0 and glyph.y == 0:
      pending = some(glyph)
    else:
      l.glyphs.add glyph
      inc totalGlyphs
  # The last cluster.
  let nextId = if rtl: 0 else: infos.len
  let nc = numComponents(nextId, clusterId, lastClusterId)
  if nc > 1:
    if pending.isSome:
      l.glyphs.add pending.get
      inc totalGlyphs
      pending = none(Glyph)
    let ligAdvance = clusterAdvance / float32(nc)
    pushCluster(l.clusters, info, clusterStartChar, clusterGlyphOffset, ligAdvance, totalGlyphs, 1, -1)
    clusterGlyphOffset = totalGlyphs
    var k = 1
    while it < order.len:
      let ch = nextChar()
      if isWhitespaceKind(ch[1]) == 1: break
      let ci = if rtl: clusterId + nc - k else: clusterId + k
      pushCluster(l.clusters, infos[min(ci, infos.len - 1)], ch, clusterGlyphOffset, ligAdvance, totalGlyphs, 2, -1)
      inc k
  else:
    let isNewline = isWhitespaceKind(clusterStartChar[1]) == 4
    var inline = -1
    if not isNewline:
      if totalGlyphs == clusterGlyphOffset and pending.isSome:
        inline = pending.get.id
        pending = none(Glyph)
    else:
      if pending.isSome:
        l.glyphs.add pending.get
        inc totalGlyphs
        pending = none(Glyph)
    pushCluster(l.clusters, info, clusterStartChar, clusterGlyphOffset, clusterAdvance, totalGlyphs,
                (if isNewline: 3 else: 0), inline)
  runAdvance += clusterAdvance
  runAdvance

proc pushRun(l: var ParaLayout; sel: SelectedFont; face: Face; size: float32; glyphs: seq[HbGlyph];
             level: int; style: ParaStyle; segText: seq[(int, int)]; infos: openArray[CharInfo];
             textStart, textEnd: int) =
  var run = RunData(face: sel.face, fontSize: size, attrsWeight: sel.attrsWeight, synthesis: sel.synthesis,
                    coords: normalizedCoords(sel.face, sel.synthesis, style.variations),
                    textStart: textStart, textEnd: textEnd, bidiLevel: level, glyphStart: l.glyphs.len,
                    metrics: runMetrics(face, size), wordSpacing: style.wordSpacing,
                    letterSpacing: style.letterSpacing)
  if glyphs.len == 0: return
  let scale = size / float32(max(face.unitsPerEm, 1))
  let start = l.clusters.len
  run.clusterStart = start
  let rtl = (level and 1) == 1
  run.advance = l.processClusters(rtl, scale, glyphs, infos, segText)
  if rtl: l.clusters.reverse(start, l.clusters.len - 1)
  run.clusterEnd = l.clusters.len
  if run.clusterEnd > run.clusterStart:
    l.runs.add run
    l.items.add l.runs.len - 1

proc shapeItem(l: var ParaLayout; db: FontDb; text: string; infos: seq[CharInfo]; charStart, charEnd: int;
               level: int; sc: Script; style: ParaStyle) =
  if charStart >= charEnd: return
  # Grapheme clusters of the item.
  var cps: seq[int]
  for k in charStart ..< charEnd: cps.add infos[k].cp
  let gb = graphemeBoundaries(cps)
  var clusters: seq[(int, int)]   # char ranges (absolute)
  var s = charStart
  for k in charStart + 1 ..< charEnd:
    if gb[k - charStart]:
      clusters.add (s, k)
      s = k
  clusters.add (s, charEnd)
  var ci = 0
  let styles = l.styles
  proc pick(c: (int, int)): Option[SelectedFont] =
    let st = styles[infos[c[0]].styleIndex]
    selectFont(db, st, infos.toOpenArray(c[0], c[1] - 1))
  var current = pick(clusters[0])
  while current.isSome and ci < clusters.len:
    let font = current.get
    let segStart = clusters[ci][0]
    var segEnd = clusters[ci][1]
    current = none(SelectedFont)
    inc ci
    while ci < clusters.len:
      let nf = pick(clusters[ci])
      if nf.isNone:
        inc ci
        continue
      if not sameFont(nf.get, font):
        current = nf
        break
      segEnd = clusters[ci][1]
      inc ci
    let face = font.face.tryLoad
    if face.isNone: continue
    var seg: seq[int]
    var segChars: seq[(int, int)]
    let segByte0 = infos[segStart].byteOff
    for k in segStart ..< segEnd:
      seg.add infos[k].cp
      segChars.add (infos[k].byteOff - segByte0, infos[k].cp)
    var lang = style.locale
    let dash = lang.find('-')
    if dash >= 0: lang = lang[0 ..< dash]
    let glyphs = face.get.shape(seg, ShapeOptions(script: sc, language: lang, rtl: (level and 1) == 1,
                                                  features: style.features))
    let textEnd = if segEnd < infos.len: infos[segEnd].byteOff else: text.len
    l.pushRun(font, face.get, style.size, glyphs, level, style, segChars,
              infos.toOpenArray(segStart, segEnd - 1), segByte0, textEnd)

proc withSize(s: ParaStyle; size: float32): ParaStyle =
  result = s
  result.size = size

proc shapeText(l: var ParaLayout; db: FontDb; text: string; infos: seq[CharInfo]; levels: seq[uint8]) =
  if infos.len == 0 or l.styles.len == 0: return
  var style = l.styles[infos[0].styleIndex]
  var itemStyle = infos[0].styleIndex
  var itemSize = style.size
  var itemLevel = if levels.len > 0: int(levels[0]) else: 0
  var itemScript = scLatin
  for inf in infos:
    if inf.script.isRealScript:
      itemScript = inf.script
      break
  var itemLocale = style.locale
  var itemVariations = style.variations
  var itemFeatures = style.features
  var itemLetter = style.letterSpacing
  var itemWord = style.wordSpacing
  var startChar = 0
  proc near(a, b: float32): bool = abs(a - b) <= 1e-6'f32 * max(1'f32, max(abs(a), abs(b)))
  for k, inf in infos:
    var brk = false
    var sc = inf.script
    if not sc.isRealScript: sc = itemScript
    let level = if k < levels.len: int(levels[k]) else: 0
    if itemStyle != inf.styleIndex:
      itemStyle = inf.styleIndex
      style = l.styles[inf.styleIndex]
      if not near(style.size, itemSize) or style.locale != itemLocale or style.variations != itemVariations or
         style.features != itemFeatures or not near(style.letterSpacing, itemLetter) or
         not near(style.wordSpacing, itemWord):
        brk = true
    if level != itemLevel or sc != itemScript: brk = true
    if brk and k > startChar:
      l.shapeItem(db, text, infos, startChar, k, itemLevel, itemScript, l.styles[infos[startChar].styleIndex].withSize(itemSize))
      itemSize = style.size
      itemLevel = level
      itemScript = sc
      itemLocale = style.locale
      itemVariations = style.variations
      itemFeatures = style.features
      itemLetter = style.letterSpacing
      itemWord = style.wordSpacing
      startChar = k
  if startChar < infos.len:
    l.shapeItem(db, text, infos, startChar, infos.len, itemLevel, itemScript, l.styles[infos[startChar].styleIndex].withSize(itemSize))


proc finishSpacing(l: var ParaLayout) =
  ## Letter and word spacing after each cluster (parley `LayoutData::finish`).
  proc nearlyZero(x: float32): bool = abs(x) < 1e-6'f32
  for run in l.runs:
    if nearlyZero(run.wordSpacing) and nearlyZero(run.letterSpacing): continue
    for ci in run.clusterStart ..< run.clusterEnd:
      var spacing = run.letterSpacing
      let ws = isWhitespaceKind(l.clusters[ci].sourceChar)
      if not nearlyZero(run.wordSpacing) and ws in {1, 2}: spacing += run.wordSpacing
      if not nearlyZero(spacing):
        l.clusters[ci].advance += spacing
        if l.clusters[ci].glyphLen > 0:
          let last = run.glyphStart + l.clusters[ci].glyphOffset + l.clusters[ci].glyphLen - 1
          if last < l.glyphs.len: l.glyphs[last].advance += spacing

proc build*(b: ParaBuilder; db: FontDb): ParaLayout =
  ## Analyzes, itemizes and shapes the text (lines come from `breakAllLines`).
  let (styles, runs) = b.resolveStyles()
  result.text = b.text
  result.styles = styles
  result.textLen = b.text.len
  var infos: seq[CharInfo]
  var levels: seq[uint8]
  var base = 0
  analyze(b.text, runs, infos, levels, base)
  result.baseLevel = base
  let t = if b.text.len == 0: " " else: b.text
  result.shapeText(db, t, infos, levels)
  result.finishSpacing()

# ---------------------------------------------------------------- line breaking

type
  LineState = object
    x: float32
    itemStart, itemEnd: int
    clusterStart, clusterEnd: int
    numSpaces: int
    runningLineHeight: float32

  PrevBoundary = object
    itemIdx, clusterIdx: int
    state: LineState

  Breaker = object
    itemIdx, clusterIdx: int
    lineY: float64
    lineMaxAdvance: float32
    line: LineState
    prevBoundary: Option[PrevBoundary]
    done: bool
    lines: seq[LineData]
    lineItems: seq[LineItem]

proc isSpaceOrNbsp(c: Cluster): bool = isWhitespaceKind(c.sourceChar) in {1, 2}

proc runCluster(l: ParaLayout; ri, ci: int): Cluster = l.clusters[ci]

proc clusterTextRange(l: ParaLayout; run: RunData; c: Cluster): (int, int) =
  let s = run.textStart + c.textOffset
  (s, s + c.textLen)

proc commitLine(l: ParaLayout; br: var Breaker; maxAdvance: float32; reason: BreakReason; indent: float32) =
  br.line.clusterEnd = min(br.line.clusterEnd, l.clusters.len)
  br.line.itemEnd = min(br.line.itemEnd, l.items.len)
  let startItem = br.lineItems.len
  var firstRun = 0
  var lastRun = 0
  if br.line.itemEnd > br.line.itemStart:
    firstRun = 0
    lastRun = br.line.itemEnd - br.line.itemStart - 1
  var committed = false
  for i in br.line.itemStart ..< br.line.itemEnd:
    let rd = l.runs[l.items[i]]
    var cs = rd.clusterStart
    var ce = rd.clusterEnd
    let rel = i - br.line.itemStart
    if rel == firstRun: cs = br.line.clusterStart
    if rel == lastRun: ce = br.line.clusterEnd
    if cs >= rd.clusterEnd: continue
    committed = true
    var ts, te: int
    if rd.clusterEnd > rd.clusterStart:
      ts = l.clusterTextRange(rd, l.clusters[cs])[0]
      te = l.clusterTextRange(rd, l.clusters[rd.clusterStart + max(ce - rd.clusterStart - 1, 0)])[1]
    br.lineItems.add LineItem(run: l.items[i], bidiLevel: rd.bidiLevel, clusterStart: cs, clusterEnd: ce,
                              textStart: ts, textEnd: te)
  var numSpaces = br.line.numSpaces
  if reason == brRegular and br.line.clusterStart < br.line.clusterEnd and
     l.clusters[br.line.clusterEnd - 1].isSpaceOrNbsp:
    numSpaces = max(numSpaces - 1, 0)
  br.lines.add LineData(itemStart: startItem, itemEnd: br.lineItems.len, maxAdvance: maxAdvance,
                        breakReason: reason, numSpaces: numSpaces, indent: indent,
                        metrics: LineMetrics(advance: br.line.x))
  br.line.numSpaces = 0
  if committed: br.line.clusterStart = br.line.clusterEnd
  br.line.itemStart = max(br.line.itemEnd - 1, 0)

proc computeWhitespace(l: ParaLayout; it: var LineItem) =
  it.isWhitespace = true
  if (it.bidiLevel and 1) != 0:
    for ci in it.clusterStart ..< it.clusterEnd:
      if isWhiteSpace(l.clusters[ci].sourceChar): it.hasTrailingWhitespace = true
      else:
        it.isWhitespace = false
        break
  else:
    for ci in countdown(it.clusterEnd - 1, it.clusterStart):
      if isWhiteSpace(l.clusters[ci].sourceChar): it.hasTrailingWhitespace = true
      else:
        it.isWhitespace = false
        break

proc reorderLineItems(items: var openArray[LineItem]) =
  var maxLevel = 0
  var lowestOdd = 255
  for it in items:
    maxLevel = max(maxLevel, it.bidiLevel)
    if (it.bidiLevel and 1) != 0: lowestOdd = min(lowestOdd, it.bidiLevel)
  for level in countdown(maxLevel, lowestOdd):
    var i = 0
    while i < items.len:
      if items[i].bidiLevel >= level:
        var e = i + 1
        while e < items.len and items[e].bidiLevel >= level: inc e
        var j = i
        var k = e - 1
        while j < k:
          swap(items[j], items[k])
          inc j
          dec k
        i = e
      inc i

proc finishLine(l: ParaLayout; br: var Breaker; idx: int; lineHeight: float32) =
  let prevMetrics = if idx > 0: some(br.lines[idx - 1].metrics) else: none(LineMetrics)
  var line = br.lines[idx]
  line.metrics.ascent = 0
  line.metrics.descent = 0
  line.metrics.leading = 0
  line.metrics.offset = 0
  line.metrics.lineHeight = lineHeight
  line.textStart = high(int)
  line.textEnd = 0
  if line.itemEnd <= line.itemStart:
    line.textStart = l.textLen
    line.textEnd = l.textLen
  var haveMetrics = false
  var needsReorder = false
  for k in countdown(line.itemEnd - 1, line.itemStart):
    var it = br.lineItems[k]
    l.computeWhitespace(it)
    line.textEnd = max(line.textEnd, it.textEnd)
    line.textStart = min(line.textStart, it.textStart)
    if it.bidiLevel != 0: needsReorder = true
    var adv = 0'f32
    for ci in it.clusterStart ..< it.clusterEnd: adv += l.clusters[ci].advance
    it.advance = adv
    br.lineItems[k] = it
    if not haveMetrics and it.isWhitespace: continue
    let rm = l.runs[it.run].metrics
    line.metrics.ascent = max(line.metrics.ascent, rm.ascent)
    line.metrics.descent = max(line.metrics.descent, rm.descent)
    haveMetrics = true
  if needsReorder and line.itemEnd - line.itemStart > 1:
    reorderLineItems(br.lineItems.toOpenArray(line.itemStart, line.itemEnd - 1))
  var trailing = 0'f32
  if line.itemEnd > line.itemStart:
    let it = if l.baseLevel mod 2 == 1: br.lineItems[line.itemStart] else: br.lineItems[line.itemEnd - 1]
    if it.hasTrailingWhitespace:
      if (it.bidiLevel and 1) != 0:
        for ci in it.clusterStart ..< it.clusterEnd:
          if isWhitespaceKind(l.clusters[ci].sourceChar) == 0: break
          trailing += l.clusters[ci].advance
      else:
        for ci in countdown(it.clusterEnd - 1, it.clusterStart):
          if isWhitespaceKind(l.clusters[ci].sourceChar) == 0: break
          trailing += l.clusters[ci].advance
  line.metrics.trailingWhitespace = trailing
  if not haveMetrics:
    if line.itemEnd > line.itemStart:
      let rm = l.runs[br.lineItems[line.itemStart].run].metrics
      line.metrics.ascent = rm.ascent
      line.metrics.descent = rm.descent
    elif prevMetrics.isSome:
      line.metrics = prevMetrics.get
      var ri = -1
      for i in countdown(l.runs.len - 1, 0):
        if l.runs[i].textEnd > l.runs[i].textStart:
          ri = i
          break
      if ri >= 0:
        let at = br.lineItems.len
        br.lineItems.add LineItem(run: ri, clusterStart: l.runs[ri].clusterEnd, clusterEnd: l.runs[ri].clusterEnd,
                                  textStart: l.runs[ri].textEnd, textEnd: l.runs[ri].textEnd)
        line.itemStart = at
        line.itemEnd = at + 1
  line.metrics.leading = line.metrics.lineHeight - (line.metrics.ascent + line.metrics.descent)
  let above = line.metrics.leading * 0.5'f32
  let below = line.metrics.leading * 0.5'f32
  let y = br.lineY
  line.metrics.baseline = line.metrics.ascent + above + float32(y)
  line.metrics.blockMin = line.metrics.baseline - line.metrics.ascent - max(above, 0)
  line.metrics.blockMax = line.metrics.baseline + line.metrics.descent + max(below, 0)
  line.metrics.inlineMin = 0
  line.metrics.inlineMax = br.lineMaxAdvance
  br.lines[idx] = line

proc startNewLine(l: ParaLayout; br: var Breaker; reason: BreakReason; maxAdvance, indent: float32) =
  l.commitLine(br, maxAdvance, reason, indent)
  let h = br.line.runningLineHeight
  br.line.x = 0
  br.line.runningLineHeight = 0
  br.prevBoundary = none(PrevBoundary)
  l.finishLine(br, br.lines.len - 1, h)
  br.lineY += float64(h)

proc breakNext(l: ParaLayout; br: var Breaker): bool =
  if br.done: return false
  let maxAdv0 = if l.textLen == 0: F32Max else: br.lineMaxAdvance
  let indent = if br.lines.len == 0: l.indentAmount else: 0'f32
  let maxAdvance = maxAdv0 - indent
  while br.itemIdx < l.items.len:
    let rd = l.runs[l.items[br.itemIdx]]
    let lineHeight = rd.metrics.lineHeight
    while br.clusterIdx < rd.clusterEnd:
      let c = l.clusters[br.clusterIdx]
      let ws = isWhitespaceKind(c.sourceChar)
      let isNewline = ws == 4
      let isSpace = ws in {1, 2}
      if c.boundary == bLine:
        if not c.ligComponent and br.line.x != 0:
          br.prevBoundary = some(PrevBoundary(itemIdx: br.itemIdx, clusterIdx: br.clusterIdx, state: br.line))
      elif isNewline:
        br.line.itemEnd = br.itemIdx + 1
        br.line.clusterEnd = br.clusterIdx + 1
        inc br.clusterIdx
        br.line.runningLineHeight = max(br.line.runningLineHeight, lineHeight)
        l.startNewLine(br, brExplicit, maxAdvance, indent)
        return true
      var advance = c.advance
      if c.ligStart:
        while br.clusterIdx + 1 < rd.clusterEnd and l.clusters[br.clusterIdx + 1].ligComponent:
          advance += l.clusters[br.clusterIdx + 1].advance
          inc br.clusterIdx
      let nextX = br.line.x + advance
      if nextX <= maxAdvance:
        br.line.itemEnd = br.itemIdx + 1
        br.line.clusterEnd = br.clusterIdx + 1
        inc br.clusterIdx
        br.line.x = nextX
        br.line.runningLineHeight = max(br.line.runningLineHeight, lineHeight)
        if isSpace: inc br.line.numSpaces
      elif isSpace:
        br.line.itemEnd = br.itemIdx + 1
        br.line.clusterEnd = br.clusterIdx + 1
        inc br.clusterIdx
        br.line.x = nextX
        br.line.runningLineHeight = max(br.line.runningLineHeight, lineHeight)
        l.startNewLine(br, brRegular, maxAdvance, indent)
        return true
      elif br.prevBoundary.isSome:
        let p = br.prevBoundary.get
        br.prevBoundary = none(PrevBoundary)
        br.itemIdx = p.itemIdx
        br.clusterIdx = p.clusterIdx
        br.line = p.state
        l.startNewLine(br, brRegular, maxAdvance, indent)
        return true
      else:
        br.line.itemEnd = br.itemIdx + 1
        br.line.clusterEnd = br.clusterIdx + 1
        inc br.clusterIdx
        br.line.x = nextX
        br.line.runningLineHeight = max(br.line.runningLineHeight, lineHeight)
    inc br.itemIdx
  if br.line.itemEnd == 0: br.line.itemEnd = 1
  br.done = true
  l.startNewLine(br, brNone, maxAdvance, indent)
  true

proc setTextIndent*(l: var ParaLayout; amount: float32) = l.indentAmount = amount

proc unjustify(l: var ParaLayout)

proc breakAllLines*(l: var ParaLayout; maxAdvance: Option[float32]) =
  ## Breaks the paragraph into lines no longer than `maxAdvance` (unbounded when none).
  l.unjustify()
  var br = Breaker(lineMaxAdvance: maxAdvance.get(F32Max))
  while l.breakNext(br): discard
  if l.textLen == 0 and br.lineItems.len > 0:
    br.lineItems[0].textStart = 0
    br.lineItems[0].textEnd = 0
    br.lineItems[0].clusterStart = 0
    br.lineItems[0].clusterEnd = 0
  var width, full = 0'f32
  var height = 0'f64
  for line in br.lines:
    let lineMax = line.metrics.inlineMin + line.metrics.advance + max(line.indent, 0)
    full = max(full, lineMax)
    width = max(width, lineMax - line.metrics.trailingWhitespace)
    height += float64(line.metrics.lineHeight)
  if br.lineMaxAdvance >= F32Max:
    for line in br.lines.mitems:
      if line.metrics.inlineMax >= F32Max: line.metrics.inlineMax = width
  if br.lines.len > 0 and br.lines[^1].itemEnd <= br.lines[^1].itemStart:
    height -= float64(br.lines[^1].metrics.lineHeight)
  l.width = width
  l.fullWidth = full
  l.height = float32(height)
  l.layoutMaxAdvance = br.lineMaxAdvance
  l.lines = br.lines
  l.lineItems = br.lineItems

# ---------------------------------------------------------------- alignment

proc alignImpl(l: var ParaLayout; alignment: Alignment; alignWhenOverflowing: bool; undo: bool) =
  let rtl = (l.baseLevel and 1) == 1
  for li in 0 ..< l.lines.len:
    var line = l.lines[li]
    let indent = line.indent
    line.metrics.offset = if rtl: -line.metrics.trailingWhitespace else: indent
    let lineWidth = line.metrics.inlineMax - line.metrics.inlineMin
    let free = lineWidth - indent - line.metrics.advance + line.metrics.trailingWhitespace
    if not alignWhenOverflowing and free <= 0:
      if rtl: line.metrics.offset += free
      l.lines[li] = line
      continue
    case alignment
    of alLeft: discard
    of alStart: (if rtl: line.metrics.offset += free)
    of alEnd: (if not rtl: line.metrics.offset += free)
    of alRight: line.metrics.offset += free
    of alCenter: line.metrics.offset += free * 0.5'f32
    of alJustify:
      if free > 0:
        if line.breakReason in {brNone, brExplicit} or line.numSpaces == 0:
          if rtl: line.metrics.offset += free
        else:
          let adjustment = free / float32(line.numSpaces) * (if undo: -1'f32 else: 1'f32)
          var applied = 0
          var order: seq[int]
          for k in line.itemStart ..< line.itemEnd: order.add k
          if rtl: order.reverse()
          for k in order:
            let it = l.lineItems[k]
            var cs: seq[int]
            for ci in it.clusterStart ..< it.clusterEnd: cs.add ci
            if (it.bidiLevel and 1) != 0: cs.reverse()
            for ci in cs:
              if applied == line.numSpaces: break
              if l.clusters[ci].isSpaceOrNbsp:
                l.clusters[ci].advance += adjustment
                inc applied
    l.lines[li] = line

proc unjustify(l: var ParaLayout) =
  if l.alignedJustified:
    l.alignImpl(alJustify, false, true)
    l.alignedJustified = false

proc align*(l: var ParaLayout; alignment: Alignment; alignWhenOverflowing: bool) =
  l.unjustify()
  l.alignedJustified = alignment == alJustify
  l.alignImpl(alignment, alignWhenOverflowing, false)

# ---------------------------------------------------------------- reading the layout

type
  ClusterView* = object
    textStart*, textEnd*: int
    advance*: float32
    rtl*: bool
    spaceOrNbsp*: bool
    styleIndex*: int
    glyphs*: seq[Glyph]

  GlyphRunView* = object
    runIndex*: int        ## index of the run among the line's items (parley `Run::index`)
    run*: int             ## index into `ParaLayout.runs`
    styleIndex*: int
    offset*, advance*: float32
    glyphs*: seq[Glyph]

  LineView* = object
    metrics*: LineMetrics
    textStart*, textEnd*: int
    breakReason*: int

proc lineCount*(l: ParaLayout): int = l.lines.len

proc line*(l: ParaLayout; i: int): LineView =
  let d = l.lines[i]
  LineView(metrics: d.metrics, textStart: d.textStart, textEnd: d.textEnd, breakReason: ord(d.breakReason))

proc clusterGlyphs(l: ParaLayout; run: RunData; c: Cluster): seq[Glyph] =
  if c.glyphLen == -1:
    result.add Glyph(id: c.glyphOffset, styleIndex: c.styleIndex, advance: c.advance)
  else:
    let s = run.glyphStart + c.glyphOffset
    for k in s ..< min(s + c.glyphLen, l.glyphs.len): result.add l.glyphs[k]

proc lineItemCount*(l: ParaLayout; line: int): int = l.lines[line].itemEnd - l.lines[line].itemStart

proc itemRun*(l: ParaLayout; line, item: int): int = l.lineItems[l.lines[line].itemStart + item].run
  ## Index into `runs` of a line's item.

proc visualClusters*(l: ParaLayout; line, item: int): seq[ClusterView] =
  ## The clusters of a line item (a run's part on that line) in visual order.
  let it = l.lineItems[l.lines[line].itemStart + item]
  let run = l.runs[it.run]
  let rtl = (run.bidiLevel and 1) != 0
  var idx: seq[int]
  for ci in it.clusterStart ..< it.clusterEnd: idx.add ci
  if rtl: idx.reverse()
  for ci in idx:
    let c = l.clusters[ci]
    let (s, e) = l.clusterTextRange(run, c)
    result.add ClusterView(textStart: s, textEnd: e, advance: c.advance, rtl: rtl, spaceOrNbsp: c.isSpaceOrNbsp,
                           styleIndex: c.styleIndex, glyphs: l.clusterGlyphs(run, c))

proc itemTextRange*(l: ParaLayout; line, item: int): (int, int) =
  let it = l.lineItems[l.lines[line].itemStart + item]
  (it.textStart, it.textEnd)

proc glyphRuns*(l: ParaLayout; line: int): seq[GlyphRunView] =
  ## The line's glyph runs (a run split where the glyph style changes), positioned (parley
  ## `Line::items`).
  let d = l.lines[line]
  var offset = 0'f32
  for k in d.itemStart ..< d.itemEnd:
    let rel = k - d.itemStart
    var glyphs: seq[Glyph]
    for c in l.visualClusters(line, rel):
      glyphs.add c.glyphs
    var i = 0
    while i < glyphs.len:
      let si = glyphs[i].styleIndex
      var adv = 0'f32
      var j = i
      while j < glyphs.len and glyphs[j].styleIndex == si:
        adv += glyphs[j].advance
        inc j
      result.add GlyphRunView(runIndex: rel, run: l.lineItems[k].run, styleIndex: si,
                              offset: offset + d.metrics.inlineMin + d.metrics.offset, advance: adv,
                              glyphs: glyphs[i ..< j])
      offset += adv
      i = j

proc runFace*(l: ParaLayout; run: int): FaceRec = l.runs[run].face
