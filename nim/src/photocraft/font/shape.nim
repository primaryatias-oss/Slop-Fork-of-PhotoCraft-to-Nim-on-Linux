## Text shaping: Unicode characters → positioned glyphs for one font, script, language and
## direction. Follows HarfBuzz's default shaper (what HarfRust runs for Latin, Greek, Cyrillic,
## CJK and most other scripts in the Rust build): Unicode properties and grapheme clusters,
## normalization against the font's cmap, GSUB, advances and space fallbacks, GPOS (or the legacy
## `kern` table), mark zeroing and attachment resolution, default-ignorable hiding.
##
## Output glyphs are in visual order (left to right), positions in font units.

import std/[strutils, algorithm, options, sequtils]
import sfnt, otl
import ../text/uprops

export HbGlyph

type
  ShapeFeature* = tuple[tag: string, value: int]
  ShapeOptions* = object
    script*: Script
    language*: string          ## BCP 47 tag or its primary subtag; "" for none
    rtl*: bool
    features*: seq[ShapeFeature]   ## user features (override the defaults: "kern" 0 turns kerning off)

const
  spNone = 0'u8
  spEm = 1'u8
  spEm2 = 2'u8
  spEm3 = 3'u8
  spEm4 = 4'u8
  spEm5 = 5'u8
  spEm6 = 6'u8
  spEm16 = 16'u8
  sp4Em18 = 17'u8
  spSpace = 18'u8
  spFigure = 19'u8
  spPunctuation = 20'u8
  spNarrow = 21'u8

proc spaceFallbackType(u: int): uint8 =
  case u
  of 0x0020, 0x00A0: spSpace
  of 0x2000, 0x2002: spEm2
  of 0x2001, 0x2003, 0x3000: spEm
  of 0x2004: spEm3
  of 0x2005: spEm4
  of 0x2006: spEm6
  of 0x2007: spFigure
  of 0x2008: spPunctuation
  of 0x2009: spEm5
  of 0x200A: spEm16
  of 0x202F: spNarrow
  of 0x205F: sp4Em18
  else: spNone

# ---------------------------------------------------------------- OpenType tags

proc otScriptTags*(s: Script): seq[string] =
  ## OpenType script tags to try for a Unicode script, newest first (HarfBuzz's mapping).
  let iso = isoCode(s)
  case iso
  of "Zyyy", "Zinh", "Zzzz": return @[]
  of "Hira", "Kana": return @["kana"]
  of "Laoo": return @["lao "]
  of "Yiii": return @["yi  "]
  of "Nkoo": return @["nko "]
  of "Vaii": return @["vai "]
  of "Zmth": return @["math"]
  of "Deva": return @["dev3", "dev2", "deva"]
  of "Beng": return @["bng3", "bng2", "beng"]
  of "Guru": return @["gur3", "gur2", "guru"]
  of "Gujr": return @["gjr3", "gjr2", "gujr"]
  of "Orya": return @["ory3", "ory2", "orya"]
  of "Taml": return @["tml3", "tml2", "taml"]
  of "Telu": return @["tel3", "tel2", "telu"]
  of "Knda": return @["knd3", "knd2", "knda"]
  of "Mlym": return @["mlm3", "mlm2", "mlym"]
  of "Mymr": return @["mym2", "mymr"]
  else: return @[iso.toLowerAscii]

const languageTags = [
  ("af", "AFK "), ("am", "AMH "), ("ar", "ARA "), ("az", "AZE "), ("be", "BEL "), ("bg", "BGR "),
  ("bn", "BEN "), ("bs", "BOS "), ("ca", "CAT "), ("cs", "CSY "), ("cy", "WEL "), ("da", "DAN "),
  ("de", "DEU "), ("el", "ELL "), ("en", "ENG "), ("es", "ESP "), ("et", "ETI "), ("eu", "EUQ "),
  ("fa", "FAR "), ("fi", "FIN "), ("fr", "FRA "), ("ga", "IRI "), ("gl", "GAL "), ("gu", "GUJ "),
  ("he", "IWR "), ("iw", "IWR "), ("hi", "HIN "), ("hr", "HRV "), ("hu", "HUN "), ("hy", "HYE "),
  ("id", "IND "), ("is", "ISL "), ("it", "ITA "), ("ja", "JAN "), ("ka", "KAT "), ("kk", "KAZ "),
  ("km", "KHM "), ("kn", "KAN "), ("ko", "KOR "), ("ky", "KIR "), ("lo", "LAO "), ("lt", "LTH "),
  ("lv", "LVI "), ("mk", "MKD "), ("ml", "MAL "), ("mn", "MNG "), ("mr", "MAR "), ("ms", "MLY "),
  ("mt", "MTS "), ("my", "BRM "), ("nb", "NOR "), ("ne", "NEP "), ("nl", "NLD "), ("nn", "NYN "),
  ("no", "NOR "), ("pa", "PAN "), ("pl", "PLK "), ("ps", "PAS "), ("pt", "PTG "), ("ro", "ROM "),
  ("ru", "RUS "), ("si", "SNH "), ("sk", "SKY "), ("sl", "SLV "), ("sq", "SQI "), ("sr", "SRB "),
  ("sv", "SVE "), ("sw", "SWK "), ("ta", "TAM "), ("te", "TEL "), ("th", "THA "), ("tr", "TRK "),
  ("uk", "UKR "), ("ur", "URD "), ("uz", "UZB "), ("vi", "VIT "), ("zh", "ZHS "), ("zu", "ZUL ")]

proc otLanguageTags*(lang: string): seq[string] =
  ## OpenType language system tags for a BCP 47 language (primary subtag; Chinese by region/script).
  let l = lang.toLowerAscii.replace('_', '-')
  let primary = l.split('-')[0]
  if primary == "zh":
    if "hant" in l or "-tw" in l: return @["ZHT ", "ZHH "]
    if "-hk" in l or "-mo" in l: return @["ZHH ", "ZHT "]
    return @["ZHS "]
  for (k, v) in languageTags:
    if k == primary: return @[v]
  @[]

proc horizontalDirectionRtl(s: Script): int =
  ## 1 for right-to-left scripts, 0 left-to-right, -1 undetermined.
  case isoCode(s)
  of "Arab", "Hebr", "Syrc", "Thaa", "Cprt", "Khar", "Phnx", "Nkoo", "Lydi", "Avst", "Armi", "Phli",
     "Prti", "Sarb", "Orkh", "Samr", "Mand", "Merc", "Mero", "Mani", "Mend", "Nbat", "Narb", "Palm",
     "Phlp", "Hatr", "Adlm", "Rohg", "Sogo", "Sogd", "Elym", "Chrs", "Yezi", "Ougr": 1
  of "Hung", "Ital", "Runr", "Zyyy", "Zinh", "Zzzz": -1
  else: 0

# ---------------------------------------------------------------- Unicode properties

proc modifiedCombiningClass(u: int): uint8 =
  if u == 0x1A60 or u == 0x0FC6: return 254
  if u == 0x0F39: return 127
  let c = combiningClass(u)
  uint8(case c
    of 10: 22
    of 11: 15
    of 12: 16
    of 13: 17
    of 14: 23
    of 15: 18
    of 16: 19
    of 17: 20
    of 18: 21
    of 19: 14
    of 20: 24
    of 21: 12
    of 22: 25
    of 23: 13
    of 24: 10
    of 25: 11
    of 27: 28
    of 28: 29
    of 29: 30
    of 30: 31
    of 31: 32
    of 32: 33
    of 33: 27
    of 84, 91: 0
    of 103: 3
    of 130: 132
    of 132: 131
    else: c)

proc initProps(g: var HbGlyph) =
  let u = g.cp
  let gc = generalCategory(u)
  g.gc = uint8(ord(gc))
  g.uflags = 0
  g.ccc = 0
  if u >= 0x80:
    if isDefaultIgnorable(u):
      g.uflags = g.uflags or ufIgnorable
      if u == 0x200C: g.uflags = g.uflags or ufZwnj
      elif u == 0x200D: g.uflags = g.uflags or ufZwj
      elif u in 0x180B .. 0x180D or u == 0x180F or u in 0xE0020 .. 0xE007F or u == 0x034F:
        g.uflags = g.uflags or ufHidden
    if gc.isMark:
      g.uflags = g.uflags or ufContinuation
      g.ccc = modifiedCombiningClass(u)

proc isUnicodeMark(g: HbGlyph): bool = GeneralCategory(g.gc).isMark

# ---------------------------------------------------------------- normalization

type Norm = object
  f: Face
  input: seq[HbGlyph]
  idx: int
  output: seq[HbGlyph]

proc glyph(n: Norm; u: int): int =
  ## Nominal glyph, 0 when the font lacks it.
  try: n.f.glyphIndex(u) except FontError: 0

proc nextChar(n: var Norm; gid: int) =
  var g = n.input[n.idx]
  g.gid = gid
  n.output.add g
  inc n.idx

proc outputChar(n: var Norm; u, gid: int) =
  var g = n.input[n.idx]
  g.cp = u
  g.gid = gid
  g.initProps()
  n.output.add g

proc decomposeRec(n: var Norm; shortest: bool; ab: int; depth = 0): int =
  let (a, b) = decompose(ab)
  if a == 0 or depth > 16: return 0
  let bGlyph = if b != 0: n.glyph(b) else: 0
  if b != 0 and bGlyph == 0: return 0
  let aGlyph = n.glyph(a)
  let ret = n.decomposeRec(shortest, a, depth + 1)
  if ret > 0:
    if b != 0:
      n.outputChar(b, bGlyph)
      return ret + 1
    return ret
  if aGlyph != 0:
    n.outputChar(a, aGlyph)
    if b != 0:
      n.outputChar(b, bGlyph)
      return 2
    return 1
  0

proc decomposeCurrent(n: var Norm; shortest: bool) =
  let u = n.input[n.idx].cp
  let g = n.glyph(u)
  if shortest and g != 0:
    n.nextChar(g)
    return
  if n.decomposeRec(shortest, u) > 0:
    inc n.idx
    return
  if not shortest and g != 0:
    n.nextChar(g)
    return
  if GeneralCategory(n.input[n.idx].gc) == gcZs:
    let st = spaceFallbackType(u)
    if st != spNone:
      let sg = n.glyph(0x20)
      if sg != 0:
        n.input[n.idx].space = st
        n.nextChar(sg)
        return
  if u == 0x2011:
    let o = n.glyph(0x2010)
    if o != 0:
      n.nextChar(o)
      return
  n.nextChar(g)

type
  ComposeFn = proc(a, b: int): int {.closure.}
  ReorderFn = proc(buf: var seq[HbGlyph]; start, stop: int) {.closure.}

proc normalize(f: Face; buf: var seq[HbGlyph]; composeFn: ComposeFn; reorder: ReorderFn; recompose = true) =
  ## `recompose` false is HarfBuzz's NONE mode (Hangul): map, decompose only what is missing.
  ## HarfBuzz's "composed diacritics" normalization: decompose what the font lacks (and clusters
  ## with marks), reorder marks by combining class, recompose where the font has the result.
  if buf.len == 0: return
  var n = Norm(f: f, input: move buf)
  let count = n.input.len
  var allSimple = true
  while true:
    var stop = n.idx + 1
    while stop < count and not n.input[stop].isUnicodeMark: inc stop
    if stop < count: dec stop
    # Simple clusters: map directly while the font has the characters.
    while n.idx < stop:
      let g = n.glyph(n.input[n.idx].cp)
      if g == 0: break
      n.nextChar(g)
    while n.idx < stop: n.decomposeCurrent(true)
    if n.idx >= count: break
    allSimple = false
    stop = n.idx + 1
    while stop < count and n.input[stop].isUnicodeMark: inc stop
    while n.idx < stop: n.decomposeCurrent(not recompose)
    if n.idx >= count: break
  buf = move n.output
  if allSimple: return
  if not recompose:
    # Reordering still happens in NONE mode; recomposition doesn't.
    discard
  # Reorder marks (stable sort by combining class within each run of marks).
  var i = 0
  while i < buf.len:
    if buf[i].ccc == 0:
      inc i
      continue
    var e = i + 1
    while e < buf.len and buf[e].ccc != 0: inc e
    if e - i <= 32:
      var run = buf[i ..< e]
      run.sort(proc(a, b: HbGlyph): int = cmp(a.ccc, b.ccc), SortOrder.Ascending)
      for k in 0 ..< run.len: buf[i + k] = run[k]
      if not reorder.isNil: reorder(buf, i, e)
    i = e + 1
  if not recompose: return
  # Recompose.
  var outp: seq[HbGlyph]
  var starter = 0
  outp.add buf[0]
  var k = 1
  while k < buf.len:
    let cur = buf[k]
    if cur.isUnicodeMark and (starter == outp.len - 1 or outp[^1].ccc < cur.ccc):
      let composed = composeFn(outp[starter].cp, cur.cp)
      if composed != 0:
        let g = (try: f.glyphIndex(composed) except FontError: 0)
        if g != 0:
          # Merge clusters, drop the mark, rewrite the starter.
          let cl = min(outp[starter].cluster, cur.cluster)
          for j in starter ..< outp.len: outp[j].cluster = cl
          outp[starter].cp = composed
          outp[starter].gid = g
          outp[starter].initProps()
          outp[starter].cluster = cl
          inc k
          continue
      elif starter < outp.len - 1 and outp[^1].ccc > cur.ccc:
        starter = outp.len
    outp.add cur
    if outp[^1].ccc == 0: starter = outp.len - 1
    inc k
  buf = outp

# ---------------------------------------------------------------- clusters

proc mergeClusters(buf: var seq[HbGlyph]; start, stop: int) =
  if stop - start < 2: return
  var cl = buf[start].cluster
  for i in start + 1 ..< stop: cl = min(cl, buf[i].cluster)
  var s = start
  var e = stop
  if cl != buf[e - 1].cluster:
    while e < buf.len and buf[e - 1].cluster == buf[e].cluster: inc e
  if cl != buf[s].cluster:
    while s > 0 and buf[s - 1].cluster == buf[s].cluster: dec s
  for i in s ..< e: buf[i].cluster = cl

proc graphemeEnd(buf: seq[HbGlyph]; start: int): int =
  result = start + 1
  while result < buf.len and (buf[result].uflags and ufContinuation) != 0: inc result

proc setContinuations(buf: var seq[HbGlyph]) =
  var i = 0
  while i < buf.len:
    let u = buf[i].cp
    let gc = GeneralCategory(buf[i].gc)
    if u < 0x80 or gc in {gcLl, gcLu, gcLt, gcLo, gcZs}:
      inc i
      continue
    if gc == gcSk and u in 0x1F3FB .. 0x1F3FF:
      buf[i].uflags = buf[i].uflags or ufContinuation
    elif i > 0 and u in 0x1F1E6 .. 0x1F1FF:
      if buf[i - 1].cp in 0x1F1E6 .. 0x1F1FF and (buf[i - 1].uflags and ufContinuation) == 0:
        buf[i].uflags = buf[i].uflags or ufContinuation
    elif (buf[i].uflags and ufZwj) != 0:
      buf[i].uflags = buf[i].uflags or ufContinuation
      if i + 1 < buf.len and isExtendedPictographic(buf[i + 1].cp):
        buf[i + 1].initProps()
        buf[i + 1].uflags = buf[i + 1].uflags or ufContinuation
        inc i
    elif u in 0xFF9E .. 0xFF9F or u in 0xE0020 .. 0xE007F:
      buf[i].uflags = buf[i].uflags or ufContinuation
    inc i

proc formClusters(buf: var seq[HbGlyph]) =
  var i = 0
  while i < buf.len:
    let e = graphemeEnd(buf, i)
    mergeClusters(buf, i, e)
    i = e

proc reverseGraphemes(buf: var seq[HbGlyph]) =
  var groups: seq[seq[HbGlyph]]
  var i = 0
  while i < buf.len:
    let e = graphemeEnd(buf, i)
    groups.add buf[i ..< e]
    i = e
  buf.setLen(0)
  for k in countdown(groups.len - 1, 0): buf.add groups[k]

# ---------------------------------------------------------------- shapers

type
  ShaperKind = enum shDefault, shHebrew, shThai, shArabic, shHangul

  FeatureInfo = object
    tag: string
    value: int
    global: bool
    manualZwj: bool
    stage: array[2, int]
    order: int

  PreprocessFn = proc(f: Face; buf: var seq[HbGlyph]) {.nimcall.}

  Plan = object
    kind: ShaperKind
    rtl: bool
    script: Script
    scriptTags, langTags: seq[string]
    gsubStages: seq[seq[StageLookup]]
    pauses: seq[Pause]
    gposLookups: seq[StageLookup]
    masks: seq[(string, uint32)]
    globalMask: uint32
    kernOn, hasGposMark, applyGpos, applyKern, zeroMarks: bool
    adjustMarkPositioningWhenZeroing, fallbackMarkPositioning: bool
    recompose: bool
    composeFn: ComposeFn
    reorder: ReorderFn
    preprocess: PreprocessFn

  MapBuilder = object
    features: seq[FeatureInfo]
    stage: array[2, int]
    pauses: seq[Pause]

proc addFeature(m: var MapBuilder; tag: string; value: int; global = true; manualZwj = false) =
  m.features.add FeatureInfo(tag: tag, value: value, global: global, manualZwj: manualZwj, stage: m.stage,
                             order: m.features.len)

proc hasFeatureRequest(m: MapBuilder; tag: string): bool =
  for f in m.features:
    if f.tag == tag: return true
  false

proc addGsubPause(m: var MapBuilder; p: Pause = nil) =
  while m.pauses.len < m.stage[0]: m.pauses.add nil
  m.pauses.add p
  inc m.stage[0]

proc categorize(script: Script; gsubScript: string): ShaperKind =
  case isoCode(script)
  of "Hebr": shHebrew
  of "Thai", "Laoo": shThai
  of "Hang": shHangul
  of "Arab": shArabic
  of "Syrc": (if gsubScript != "DFLT": shArabic else: shDefault)
  else: shDefault

const hebrewDageshForms = [0xFB30, 0xFB31, 0xFB32, 0xFB33, 0xFB34, 0xFB35, 0xFB36, 0x0000, 0xFB38,
  0xFB39, 0xFB3A, 0xFB3B, 0xFB3C, 0x0000, 0xFB3E, 0x0000, 0xFB40, 0xFB41, 0x0000, 0xFB43, 0xFB44,
  0x0000, 0xFB46, 0xFB47, 0xFB48, 0xFB49, 0xFB4A]

proc hebrewCompose(hasGposMark: bool): ComposeFn =
  result = proc(a, b: int): int =
    let c = compose(a, b)
    if c != 0 or hasGposMark: return c
    case b
    of 0x05B4: (if a == 0x05D9: 0xFB1D else: 0)
    of 0x05B7: (if a == 0x05D9: 0xFB1F elif a == 0x05D0: 0xFB2E else: 0)
    of 0x05B8: (if a == 0x05D0: 0xFB2F else: 0)
    of 0x05B9: (if a == 0x05D5: 0xFB4B else: 0)
    of 0x05BC:
      if a in 0x05D0 .. 0x05EA: hebrewDageshForms[a - 0x05D0]
      elif a == 0xFB2A: 0xFB2C
      elif a == 0xFB2B: 0xFB2D
      else: 0
    of 0x05BF:
      case a
      of 0x05D1: 0xFB4C
      of 0x05DB: 0xFB4D
      of 0x05E4: 0xFB4E
      else: 0
    of 0x05C1: (if a == 0x05E9: 0xFB2A elif a == 0xFB49: 0xFB2C else: 0)
    of 0x05C2: (if a == 0x05E9: 0xFB2B elif a == 0xFB49: 0xFB2D else: 0)
    else: 0

proc hebrewReorder(buf: var seq[HbGlyph]; start, stop: int) =
  ## Patah/qamats + sheva/hiriq + meteg/below: put the meteg before the vowel (HarfBuzz).
  let ccc17 = modifiedCombiningClass(0x05B7)   # patah
  let ccc18 = modifiedCombiningClass(0x05B8)   # qamats
  let ccc10 = modifiedCombiningClass(0x05B0)   # sheva
  let ccc14 = modifiedCombiningClass(0x05B4)   # hiriq
  let ccc22 = modifiedCombiningClass(0x05BD)   # meteg
  for i in start + 2 ..< stop:
    let c0 = buf[i - 2].ccc
    let c1 = buf[i - 1].ccc
    let c2 = buf[i].ccc
    if (c0 == ccc17 or c0 == ccc18) and (c1 == ccc10 or c1 == ccc14) and (c2 == ccc22 or c2 == 220'u8):
      mergeClusters(buf, i - 1, i + 1)
      swap(buf[i - 1], buf[i])
      break

const arabicModifierMarks = [0x0654, 0x0655, 0x0658, 0x06DC, 0x06E3, 0x06E7, 0x06E8, 0x08CA, 0x08CB,
  0x08CD, 0x08CE, 0x08CF, 0x08D3, 0x08F3]

proc arabicReorder(buf: var seq[HbGlyph]; start0, stop: int) =
  ## Moves "modifier combining marks" (hamza, small high letters) to the front of the mark run.
  var start = start0
  var i = start
  for cc in [220'u8, 230'u8]:
    while i < stop and buf[i].ccc < cc: inc i
    if i == stop: break
    if buf[i].ccc > cc: continue
    var j = i
    while j < stop and buf[j].ccc == cc and buf[j].cp in arabicModifierMarks: inc j
    if i == j: continue
    mergeClusters(buf, start, j)
    let temp = buf[i ..< j]
    for k in countdown(i - start - 1, 0): buf[k + start + j - i] = buf[k + start]
    for k in 0 ..< temp.len: buf[start + k] = temp[k]
    let newStart = start + j - i
    let newCc = if cc == 220: modifiedCombiningClass(0x05BD) else: modifiedCombiningClass(0xFB1E)
    while start < newStart:
      buf[start].ccc = newCc
      inc start
    i = j

# Arabic joining state machine (HarfBuzz): per state and joining type, (previous action, current
# action, next state). Actions: 0 isol, 1 fina, 2 fin2, 3 fin3, 4 medi, 5 med2, 6 init, 7 none.
const arabicStates: array[7, array[6, (int, int, int)]] = [
  [(7, 7, 0), (7, 0, 2), (7, 0, 1), (7, 0, 2), (7, 0, 1), (7, 0, 6)],
  [(7, 7, 0), (7, 0, 2), (7, 0, 1), (7, 0, 2), (7, 2, 5), (7, 0, 6)],
  [(7, 7, 0), (7, 0, 2), (6, 1, 1), (6, 1, 3), (6, 1, 4), (6, 1, 6)],
  [(7, 7, 0), (7, 0, 2), (4, 1, 1), (4, 1, 3), (4, 1, 4), (4, 1, 6)],
  [(7, 7, 0), (7, 0, 2), (5, 0, 1), (5, 0, 2), (5, 2, 5), (5, 0, 6)],
  [(7, 7, 0), (7, 0, 2), (0, 0, 1), (0, 0, 2), (0, 2, 5), (0, 0, 6)],
  [(7, 7, 0), (7, 0, 2), (7, 0, 1), (7, 0, 2), (7, 3, 5), (7, 0, 6)]]
const arabicFeatures = ["isol", "fina", "fin2", "fin3", "medi", "med2", "init"]

proc arabicJoiningType(g: HbGlyph): int =
  let j = joiningType(g.cp)
  if j != 0 or g.cp < 0x80: return j
  0

proc arabicSetupMasks(p: Plan; buf: var seq[HbGlyph]) =
  var actions = newSeq[int](buf.len)
  var prev = -1
  var state = 0
  for i in 0 ..< buf.len:
    let t = arabicJoiningType(buf[i])
    if t == 6:
      actions[i] = 7
      continue
    let e = arabicStates[state][t]
    if e[0] != 7 and prev >= 0: actions[prev] = e[0]
    actions[i] = e[1]
    prev = i
    state = e[2]
  if isoCode(p.script) == "Mong":
    for i in 1 ..< buf.len:
      if buf[i].cp in 0x180B .. 0x180D or buf[i].cp == 0x180F: actions[i] = actions[i - 1]
  var maskArray: array[8, uint32]
  for k, t in arabicFeatures:
    for (tag, m) in p.masks:
      if tag == t: maskArray[k] = m
  for i in 0 ..< buf.len: buf[i].mask = buf[i].mask or maskArray[actions[i]]

proc thaiPreprocess(f: Face; buf: var seq[HbGlyph]) =
  ## Decomposes SARA AM into NIKHAHIT + SARA AA and moves the NIKHAHIT before any above-base marks.
  proc isAboveBase(u: int): bool =
    let v = u and not 0x80
    v in 0x0E34 .. 0x0E37 or v in 0x0E47 .. 0x0E4E or v == 0x0E31 or v == 0x0E3B
  var outp: seq[HbGlyph]
  for g in buf:
    if (g.cp and not 0x80) != 0x0E33:
      outp.add g
      continue
    var nik = g
    nik.cp = g.cp - 0x0E33 + 0x0E4D
    nik.gc = uint8(ord(gcMn))
    var aa = g
    aa.cp = g.cp - 1
    outp.add nik
    outp.add aa
    let e = outp.len
    var start = e - 2
    while start > 0 and isAboveBase(outp[start - 1].cp): dec start
    if start + 2 < e:
      mergeClusters(outp, start, e)
      let t = outp[e - 2]
      for i in countdown(e - start - 3, 0): outp[i + start + 1] = outp[i + start]
      outp[start] = t
    if start != 0: mergeClusters(outp, start - 1, e)
  buf = outp

proc hangulPreprocess(f: Face; buf: var seq[HbGlyph]) =
  ## Composes L V (T) jamo into syllables the font has; decomposes syllables it lacks (HarfBuzz).
  ## Jamo that stay separate get their ljmo/vjmo/tjmo feature in `space` (1–3) for the masks.
  proc has(u: int): bool = (try: f.glyphIndex(u) != 0 except FontError: false)
  proc isL(u: int): bool = u in 0x1100 .. 0x115F or u in 0xA960 .. 0xA97C
  proc isV(u: int): bool = u in 0x1160 .. 0x11A7 or u in 0xD7B0 .. 0xD7C6
  proc isT(u: int): bool = u in 0x11A8 .. 0x11FF or u in 0xD7CB .. 0xD7FB
  const (lBase, vBase, tBase, lCount, vCount, tCount, sBase) = (0x1100, 0x1161, 0x11A7, 19, 21, 28, 0xAC00)
  const nCount = vCount * tCount
  var outp: seq[HbGlyph]
  var i = 0
  while i < buf.len:
    let u = buf[i].cp
    if isL(u) and i + 1 < buf.len and isV(buf[i + 1].cp):
      let v = buf[i + 1].cp
      var t = 0
      if i + 2 < buf.len and isT(buf[i + 2].cp): t = buf[i + 2].cp
      let n = if t != 0: 3 else: 2
      if u in lBase ..< lBase + lCount and v in vBase ..< vBase + vCount and (t == 0 or t in tBase + 1 ..< tBase + tCount):
        let s = sBase + (u - lBase) * nCount + (v - vBase) * tCount + (if t != 0: t - tBase else: 0)
        if has(s):
          var g = buf[i]
          g.cp = s
          g.cluster = buf[i ..< i + n].mapIt(it.cluster).min
          outp.add g
          i += n
          continue
      let start = outp.len
      for k in 0 ..< n:
        var g = buf[i + k]
        g.space = uint8(k + 1) + 100
        outp.add g
      mergeClusters(outp, start, outp.len)
      i += n
      continue
    if u in sBase ..< sBase + lCount * nCount:
      let hasS = has(u)
      let li = (u - sBase) div nCount
      let ni = (u - sBase) mod nCount
      let vi = ni div tCount
      let ti = ni mod tCount
      if ti == 0 and i + 1 < buf.len and buf[i + 1].cp in tBase + 1 ..< tBase + tCount:
        let ns = u + buf[i + 1].cp - tBase
        if has(ns):
          var g = buf[i]
          g.cp = ns
          g.cluster = min(buf[i].cluster, buf[i + 1].cluster)
          outp.add g
          i += 2
          continue
      if not hasS or (ti == 0 and i + 1 < buf.len and isT(buf[i + 1].cp)):
        let parts = [lBase + li, vBase + vi, tBase + ti]
        if has(parts[0]) and has(parts[1]) and (ti == 0 or has(parts[2])):
          let start = outp.len
          for k in 0 ..< (if ti != 0: 3 else: 2):
            var g = buf[i]
            g.cp = parts[k]
            g.initProps()
            g.space = uint8(k + 1) + 100
            outp.add g
          inc i
          if hasS and ti == 0 and i < buf.len:
            var g = buf[i]
            g.space = 103
            outp.add g
            inc i
          mergeClusters(outp, start, outp.len)
          continue
    outp.add buf[i]
    inc i
  buf = outp

proc buildPlan(f: Face; opts: ShapeOptions): Plan =
  result.rtl = opts.rtl
  result.script = opts.script
  result.scriptTags = otScriptTags(opts.script)
  result.langTags = otLanguageTags(opts.language)
  let gsubScript = f.chosenScript(true, result.scriptTags)
  result.kind = categorize(opts.script, gsubScript)
  var m: MapBuilder
  m.addFeature("rvrn", 1)
  m.addGsubPause()
  if opts.rtl:
    m.addFeature("rtla", 1)
    m.addFeature("rtlm", 1, global = false)
  else:
    m.addFeature("ltra", 1)
    m.addFeature("ltrm", 1)
  for t in ["frac", "numr", "dnom"]: m.addFeature(t, 1, global = false)
  case result.kind
  of shArabic:
    m.addFeature("stch", 1)
    m.addGsubPause()
    m.addFeature("ccmp", 1, manualZwj = true)
    m.addFeature("locl", 1, manualZwj = true)
    m.addGsubPause()
    for t in arabicFeatures:
      m.addFeature(t, 1, global = false, manualZwj = true)
      m.addGsubPause()
    m.addGsubPause()
    m.addFeature("rlig", 1, manualZwj = true)
    if isoCode(opts.script) == "Arab": m.addGsubPause()
    m.addFeature("calt", 1, manualZwj = true)
    if not m.hasFeatureRequest("rclt"): m.addGsubPause()
    for t in ["liga", "clig", "mset"]: m.addFeature(t, 1, manualZwj = true)
  of shHangul:
    for t in ["ljmo", "vjmo", "tjmo"]: m.addFeature(t, 1, global = false)
  else: discard
  for t in ["abvm", "blwm", "ccmp", "locl", "mark", "mkmk", "rlig", "calt", "clig", "curs", "dist",
            "kern", "liga", "rclt"]:
    m.addFeature(t, 1)
  for (t, v) in opts.features: m.addFeature(t, v)
  if result.kind == shHangul: m.addFeature("calt", 0)
  # Compile: merge duplicate tags (a later request wins), allocate mask bits, gather lookups.
  var feats = m.features
  feats.sort(proc(a, b: FeatureInfo): int =
    result = cmp(a.tag, b.tag)
    if result == 0: result = cmp(a.order, b.order))
  var merged: seq[FeatureInfo]
  for fi in feats:
    if merged.len > 0 and merged[^1].tag == fi.tag:
      var j = merged[^1]
      if fi.global:
        j.global = true
        j.value = fi.value
      else:
        j.global = false
        j.value = max(j.value, fi.value)
      j.stage = [min(j.stage[0], fi.stage[0]), min(j.stage[1], fi.stage[1])]
      merged[^1] = j
    else:
      merged.add fi
  let nStages = m.stage[0] + 1
  var gsubFound = newSeq[seq[StageLookup]](nStages)
  var gposFound: seq[StageLookup]
  var nextBit = 0
  for fi in merged:
    if fi.tag == "kern": result.kernOn = fi.value != 0
    if fi.value == 0: continue
    if nextBit >= 31: continue
    let bit = 1'u32 shl nextBit
    inc nextBit
    result.masks.add (fi.tag, bit)
    if fi.global: result.globalMask = result.globalMask or bit
    let value = if fi.global: fi.value else: 1
    for l in f.featureLookupIndices(true, result.scriptTags, result.langTags, fi.tag):
      gsubFound[min(fi.stage[0], nStages - 1)].add (l, value, bit, not fi.manualZwj)
    for l in f.featureLookupIndices(false, result.scriptTags, result.langTags, fi.tag):
      gposFound.add (l, value, bit, not fi.manualZwj)
    if fi.tag == "mark" and f.featureLookupIndices(false, result.scriptTags, result.langTags, "mark").len > 0:
      result.hasGposMark = true
  for l in f.requiredFeatureLookups(true, result.scriptTags, result.langTags):
    gsubFound[^1].add (l, 1, result.globalMask, true)
  for l in f.requiredFeatureLookups(false, result.scriptTags, result.langTags):
    gposFound.add (l, 1, result.globalMask, true)
  proc finish(v: seq[StageLookup]): seq[StageLookup] =
    var s = v
    s.sort(proc(a, b: StageLookup): int = cmp(a.lookup, b.lookup))
    for x in s:
      if result.len > 0 and result[^1].lookup == x.lookup:
        result[^1].mask = result[^1].mask or x.mask
        result[^1].autoZwj = result[^1].autoZwj and x.autoZwj
      else: result.add x
  for st in gsubFound: result.gsubStages.add finish(st)
  result.gposLookups = finish(gposFound)
  result.pauses = m.pauses
  # Positioning strategy (HarfBuzz `hb_ot_shape_planner_t::compile`).
  var disableGpos = false
  if result.kind == shHebrew:
    disableGpos = f.chosenScript(false, result.scriptTags) != "hebr"
  result.applyGpos = f.hasGpos and not disableGpos
  let hasGposKern = result.applyGpos and
    f.featureLookupIndices(false, result.scriptTags, result.langTags, "kern").len > 0 and result.kernOn
  let fallbackPosition = result.kind != shThai
  result.applyKern = (not hasGposKern or not result.applyGpos) and f.hasKern and fallbackPosition
  result.zeroMarks = result.kind != shHangul
  result.adjustMarkPositioningWhenZeroing = not result.applyGpos
  result.fallbackMarkPositioning = result.adjustMarkPositioningWhenZeroing and fallbackPosition
  result.recompose = result.kind != shHangul
  result.composeFn = proc(a, b: int): int = compose(a, b)
  case result.kind
  of shHebrew:
    result.composeFn = hebrewCompose(result.hasGposMark)
    result.reorder = hebrewReorder
  of shArabic: result.reorder = arabicReorder
  of shThai: result.preprocess = thaiPreprocess
  of shHangul: result.preprocess = hangulPreprocess
  else: discard

proc mask(p: Plan; tag: string): uint32 =
  for (t, m) in p.masks:
    if t == tag: return m
  0

# ---------------------------------------------------------------- fallback mark positioning

const
  cccAttachedBelowLeft = 200'u8
  cccAttachedBelow = 202'u8
  cccAttachedAbove = 214'u8
  cccAttachedAboveRight = 216'u8
  cccBelowLeft = 218'u8
  cccBelow = 220'u8
  cccBelowRight = 222'u8
  cccAboveLeft = 228'u8
  cccAbove = 230'u8
  cccAboveRight = 232'u8
  cccDoubleBelow = 233'u8
  cccDoubleAbove = 234'u8

proc mcc(c: int): uint8 = modifiedCombiningClass(
  case c   # a code point whose combining class is `c`, to reuse the mapping
  of 10: 0x05B0
  of 11: 0x05B1
  of 12: 0x05B2
  of 13: 0x05B3
  of 14: 0x05B4
  of 15: 0x05B5
  of 16: 0x05B6
  of 17: 0x05B7
  of 18: 0x05B8
  of 19: 0x05B9
  of 20: 0x05BB
  of 21: 0x05BC
  of 22: 0x05BD
  of 23: 0x05BF
  of 24: 0x05C1
  of 25: 0x05C2
  of 26: 0xFB1E
  of 27: 0x064B
  of 28: 0x064C
  of 29: 0x064D
  of 30: 0x064E
  of 31: 0x064F
  of 32: 0x0650
  of 33: 0x0651
  of 34: 0x0652
  of 35: 0x0670
  of 36: 0x0711
  of 103: 0x0E38
  of 107: 0x0E48
  of 118: 0x0EB8
  of 122: 0x0EC8
  of 129: 0x0F71
  of 130: 0x0F72
  of 132: 0x0F74
  else: 0)

proc recategorize(u: int; class: uint8): uint8 =
  var class = class
  if class >= 200: return class
  if (u and not 0xFF) == 0x0E00:
    if class == 0:
      case u
      of 0x0E31, 0x0E34, 0x0E35, 0x0E36, 0x0E37, 0x0E47, 0x0E4C, 0x0E4D, 0x0E4E: class = cccAboveRight
      of 0x0EB1, 0x0EB4, 0x0EB5, 0x0EB6, 0x0EB7, 0x0EBB, 0x0ECC, 0x0ECD: class = cccAbove
      of 0x0EBC: class = cccBelow
      else: discard
    elif u == 0x0E3A: class = cccBelowRight
  if class == mcc(10) or class == mcc(11) or class == mcc(12) or class == mcc(13) or class == mcc(14) or
     class == mcc(15) or class == mcc(16) or class == mcc(17) or class == mcc(18) or class == mcc(20) or
     class == mcc(22): return cccBelow
  if class == mcc(23): return cccAttachedAbove
  if class == mcc(24): return cccAboveRight
  if class == mcc(25) or class == mcc(19): return cccAboveLeft
  if class == mcc(26): return cccAbove
  if class == mcc(21): return class
  if class == mcc(27) or class == mcc(28) or class == mcc(30) or class == mcc(31) or class == mcc(33) or
     class == mcc(34) or class == mcc(35) or class == mcc(36): return cccAbove
  if class == mcc(29) or class == mcc(32): return cccBelow
  if class == mcc(103): return cccBelowRight
  if class == mcc(107): return cccAboveRight
  if class == mcc(118): return cccBelow
  if class == mcc(122): return cccAbove
  if class == mcc(129): return cccBelow
  if class == mcc(130): return cccAbove
  if class == mcc(132): return cccBelow
  class

type Extents = object
  xBearing, yBearing, width, height: int

proc extents(f: Face; gid: int): (bool, Extents) =
  let e = f.glyfExtents(gid)
  if e.isNone: return (false, Extents())
  let (a, b, c, d) = e.get
  (true, Extents(xBearing: a, yBearing: b, width: c, height: d))

proc zeroMarkAdvances(buf: var seq[HbGlyph]; start, stop: int; adjust: bool) =
  for i in start ..< stop:
    if GeneralCategory(buf[i].gc) == gcMn:
      if adjust:
        buf[i].xOffset -= buf[i].xAdvance
        buf[i].yOffset -= buf[i].yAdvance
      buf[i].xAdvance = 0
      buf[i].yAdvance = 0

proc positionMark(f: Face; rtl: bool; g: var HbGlyph; base: var Extents; cls: uint8) =
  let (ok, m) = f.extents(g.gid)
  if not ok: return
  let yGap = f.unitsPerEm div 16
  g.xOffset = 0
  g.yOffset = 0
  case cls
  of cccDoubleBelow, cccDoubleAbove:
    g.xOffset = int32(base.xBearing + (if not rtl: base.width else: 0) - m.width div 2 - m.xBearing)
  of cccAttachedBelowLeft, cccBelowLeft, cccAboveLeft:
    g.xOffset = int32(base.xBearing - m.xBearing)
  of cccAttachedAboveRight, cccBelowRight, cccAboveRight:
    g.xOffset = int32(base.xBearing + base.width - m.width - m.xBearing)
  else:
    g.xOffset = int32(base.xBearing + (base.width - m.width) div 2 - m.xBearing)
  let attached = cls in {cccAttachedBelowLeft, cccAttachedBelow, cccAttachedAbove, cccAttachedAboveRight}
  case cls
  of cccDoubleBelow, cccBelowLeft, cccBelow, cccBelowRight, cccAttachedBelowLeft, cccAttachedBelow:
    if not attached: base.height -= yGap
    g.yOffset = int32(base.yBearing + base.height - m.yBearing)
    if (yGap > 0) == (g.yOffset > 0):
      base.height -= int(g.yOffset)
      g.yOffset = 0
    base.height += m.height
  of cccDoubleAbove, cccAboveLeft, cccAbove, cccAboveRight, cccAttachedAbove, cccAttachedAboveRight:
    if not attached:
      base.yBearing += yGap
      base.height -= yGap
    g.yOffset = int32(base.yBearing - (m.yBearing + m.height))
    if (yGap > 0) != (g.yOffset > 0):
      let correction = -int(g.yOffset) div 2
      base.yBearing += correction
      base.height -= correction
      g.yOffset += int32(correction)
    base.yBearing -= m.height
    base.height += m.height
  else: discard

proc positionAroundBase(f: Face; p: Plan; buf: var seq[HbGlyph]; base, stop: int; adjust: bool) =
  let (ok, be) = f.extents(buf[base].gid)
  if not ok:
    zeroMarkAdvances(buf, base + 1, stop, adjust)
    return
  var baseExt = be
  baseExt.yBearing += int(buf[base].yOffset)
  baseExt.xBearing = 0
  baseExt.width = int(f.advance(buf[base].gid))
  let ligIdB = buf[base].ligProps shr 5
  let numComps = (if (buf[base].props and gpLigature) != 0 and (buf[base].ligProps and 0x10) != 0: buf[base].ligProps and 0x0F else: 1)
  var xOff = 0
  var yOff = 0
  if not p.rtl:
    xOff -= int(buf[base].xAdvance)
    yOff -= int(buf[base].yAdvance)
  var lastComp = -1
  var lastClass = 255'u8
  var compExt = baseExt
  var clusterExt = baseExt
  for i in base + 1 ..< stop:
    if buf[i].ccc != 0:
      if numComps > 1:
        let thisLigId = buf[i].ligProps shr 5
        let thisComp0 = (if (buf[i].ligProps and 0x10) != 0: 0 else: buf[i].ligProps and 0x0F)
        var thisComp = thisComp0 - 1
        if ligIdB == 0 or ligIdB != thisLigId or thisComp >= numComps: thisComp = numComps - 1
        if lastComp != thisComp:
          lastComp = thisComp
          lastClass = 255
          compExt = baseExt
          let ltr = not p.rtl
          compExt.xBearing += ((if ltr: thisComp else: numComps - 1 - thisComp) * compExt.width) div numComps
          compExt.width = compExt.width div numComps
      let cls = buf[i].ccc
      if lastClass != cls:
        lastClass = cls
        clusterExt = compExt
      positionMark(f, p.rtl, buf[i], clusterExt, cls)
      buf[i].xAdvance = 0
      buf[i].yAdvance = 0
      buf[i].xOffset += int32(xOff)
      buf[i].yOffset += int32(yOff)
    else:
      if not p.rtl:
        xOff -= int(buf[i].xAdvance)
        yOff -= int(buf[i].yAdvance)
      else:
        xOff += int(buf[i].xAdvance)
        yOff += int(buf[i].yAdvance)

proc isUM(g: HbGlyph): bool = GeneralCategory(g.gc).isMark

proc fallbackCluster(f: Face; p: Plan; buf: var seq[HbGlyph]; start, stop: int; adjust: bool) =
  if stop - start < 2: return
  var i = start
  while i < stop:
    if not buf[i].isUM:
      var j = i + 1
      while j < stop and ((buf[j].uflags and (ufHidden or ufIgnorable)) != 0 or buf[j].isUM): inc j
      positionAroundBase(f, p, buf, i, j, adjust)
      i = j - 1
    inc i

proc fallbackPositionMarks(f: Face; p: Plan; buf: var seq[HbGlyph]; adjust: bool) =
  var start = 0
  for i in 1 ..< buf.len:
    if not buf[i].isUM and (buf[i].uflags and (ufHidden or ufIgnorable)) == 0:
      fallbackCluster(f, p, buf, start, i, adjust)
      start = i
  fallbackCluster(f, p, buf, start, buf.len, adjust)

# ---------------------------------------------------------------- the pipeline

proc shape*(f: Face; text: openArray[int]; opts: ShapeOptions): seq[HbGlyph] =
  ## Shapes `text` (code points; clusters are their indices) with one font.
  var buf = newSeq[HbGlyph](text.len)
  for i, u in text:
    buf[i].cp = u
    buf[i].cluster = i
    buf[i].initProps()
  if buf.len == 0: return
  let plan = buildPlan(f, opts)
  for g in buf.mitems: g.mask = plan.globalMask
  setContinuations(buf)
  formClusters(buf)
  # Native direction of the script (HarfBuzz `ensure_native_direction`).
  var rtl = opts.rtl
  let targetRtl = opts.rtl
  var hor = horizontalDirectionRtl(opts.script)
  if hor == 1 and not rtl:
    var foundNumber, foundLetter, foundRi = false
    for g in buf:
      let gc = GeneralCategory(g.gc)
      if gc == gcNd: foundNumber = true
      elif gc.isLetter:
        foundLetter = true
        break
      elif g.cp in 0x1F1E6 .. 0x1F1FF: foundRi = true
    if (foundNumber or foundRi) and not foundLetter: hor = 0
  if hor != -1 and (hor == 1) != rtl:
    reverseGraphemes(buf)
    rtl = not rtl
  if not plan.preprocess.isNil: plan.preprocess(f, buf)
  # Mirroring for right-to-left text.
  if targetRtl:
    let rtlm = plan.mask("rtlm")
    for g in buf.mitems:
      let m = mirror(g.cp)
      if m != 0 and (try: f.glyphIndex(m) != 0 except FontError: false):
        g.cp = m
      else:
        g.mask = g.mask or rtlm
  normalize(f, buf, plan.composeFn, plan.reorder, plan.recompose)
  # Fractions: numerator / denominator forms around U+2044.
  let fracMask = plan.mask("frac")
  let numrMask = plan.mask("numr")
  let dnomMask = plan.mask("dnom")
  if fracMask != 0 or (numrMask != 0 and dnomMask != 0):
    var i = 0
    while i < buf.len:
      if buf[i].cp == 0x2044:
        var s = i
        while s > 0 and GeneralCategory(buf[s - 1].gc) == gcNd: dec s
        var e = i + 1
        while e < buf.len and GeneralCategory(buf[e].gc) == gcNd: inc e
        if s == i or e == i + 1:
          inc i
          continue
        let (pre, post) = if not rtl: (numrMask or fracMask, fracMask or dnomMask)
                          else: (fracMask or dnomMask, numrMask or fracMask)
        for k in s ..< i: buf[k].mask = buf[k].mask or pre
        buf[i].mask = buf[i].mask or fracMask
        for k in i + 1 ..< e: buf[k].mask = buf[k].mask or post
        i = e
      else: inc i
  case plan.kind
  of shArabic: arabicSetupMasks(plan, buf)
  of shHangul:
    let jm = [plan.mask("ljmo"), plan.mask("vjmo"), plan.mask("tjmo")]
    for g in buf.mitems:
      if g.space in 101'u8 .. 103'u8:
        g.mask = g.mask or jm[int(g.space) - 101]
        g.space = 0
  else: discard
  if plan.fallbackMarkPositioning:
    for g in buf.mitems:
      if GeneralCategory(g.gc) == gcMn: g.ccc = recategorize(g.cp, g.ccc)
  # GSUB.
  f.substituteStart(buf)
  f.substitute(buf, plan.gsubStages, plan.pauses, rtl)
  # Advances and space fallbacks.
  let upem = max(f.unitsPerEm, 1)
  for g in buf.mitems:
    g.xAdvance = int32(f.advance(g.gid))
    g.yAdvance = 0
    g.xOffset = 0
    g.yOffset = 0
    if GeneralCategory(g.gc) != gcZs or (g.props and gpLigated) != 0: continue
    case g.space
    of spEm, spEm2, spEm3, spEm4, spEm5, spEm6, spEm16:
      g.xAdvance = int32((upem + int(g.space) div 2) div int(g.space))
    of sp4Em18: g.xAdvance = int32(upem * 4 div 18)
    of spFigure:
      for d in ord('0') .. ord('9'):
        let dg = (try: f.glyphIndex(d) except FontError: 0)
        if dg != 0:
          g.xAdvance = int32(f.advance(dg))
          break
    of spPunctuation:
      var pg = (try: f.glyphIndex(ord('.')) except FontError: 0)
      if pg == 0: pg = (try: f.glyphIndex(ord(',')) except FontError: 0)
      if pg != 0: g.xAdvance = int32(f.advance(pg))
    of spNarrow: g.xAdvance = g.xAdvance div 2
    else: discard
  # GPOS, or the legacy kern table.
  var attachments = false
  if plan.applyGpos: attachments = f.position(buf, plan.gposLookups, rtl)
  if plan.applyKern and plan.kernOn:
    let kernMask = plan.mask("kern")
    var i = 0
    while i + 1 < buf.len:
      if (buf[i].props and gpMark) != 0 or (buf[i].mask and kernMask) == 0:
        inc i
        continue
      var j = i + 1
      while j < buf.len and ((buf[j].props and gpMark) != 0 or (buf[j].uflags and ufIgnorable) != 0): inc j
      if j >= buf.len: break
      let k = f.kernPair(buf[i].gid, buf[j].gid)
      if k != 0:
        let k1 = int32(k shr 1)
        let k2 = int32(k) - k1
        buf[i].xAdvance += k1
        buf[j].xAdvance += k2
        buf[j].xOffset += k2
      i = j
  # Marks have no advance (by GDEF), after positioning.
  let adjustOffsets = plan.adjustMarkPositioningWhenZeroing and not rtl
  if plan.zeroMarks:
    for g in buf.mitems:
      if (g.props and gpMark) != 0:
        if adjustOffsets:
          g.xOffset -= g.xAdvance
          g.yOffset -= g.yAdvance
        g.xAdvance = 0
        g.yAdvance = 0
  for g in buf.mitems:
    if (g.uflags and ufIgnorable) != 0:
      g.xAdvance = 0
      g.yAdvance = 0
      g.xOffset = 0
  if attachments: finishOffsets(buf, rtl)
  if plan.fallbackMarkPositioning: fallbackPositionMarks(f, Plan(rtl: rtl), buf, adjustOffsets)
  if rtl: buf.reverse()
  # Hide default ignorables behind the space glyph (or drop them without one).
  var hasIgnorable = false
  for g in buf:
    if (g.uflags and ufIgnorable) != 0: hasIgnorable = true
  if hasIgnorable:
    let space = (try: f.glyphIndex(0x20) except FontError: 0)
    if space != 0:
      for g in buf.mitems:
        if (g.uflags and ufIgnorable) != 0: g.gid = space
    else:
      var kept: seq[HbGlyph]
      for g in buf:
        if (g.uflags and ufIgnorable) == 0: kept.add g
      buf = kept
  buf
