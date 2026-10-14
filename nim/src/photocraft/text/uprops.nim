## Unicode character properties for shaping, segmentation and bidi (the data ICU4X and HarfRust
## carry in the Rust build), backed by the generated tables in `ucd_data`.

import std/tables
import ucd_data

type
  GeneralCategory* = enum
    gcCn, gcLu, gcLl, gcLt, gcLm, gcLo, gcMn, gcMc, gcMe, gcNd, gcNl, gcNo, gcPc, gcPd, gcPs, gcPe,
    gcPi, gcPf, gcPo, gcSm, gcSc, gcSk, gcSo, gcZs, gcZl, gcZp, gcCc, gcCf, gcCs, gcCo
  LineBreak* = enum
    lbXX, lbBK, lbCR, lbLF, lbCM, lbNL, lbSG, lbWJ, lbZW, lbGL, lbSP, lbZWJ, lbB2, lbBA, lbBB, lbHY,
    lbCB, lbCL, lbCP, lbEX, lbIN, lbNS, lbOP, lbQU, lbIS, lbNU, lbPO, lbPR, lbSY, lbAI, lbAL, lbCJ,
    lbEB, lbEM, lbH2, lbH3, lbHL, lbID, lbJL, lbJV, lbJT, lbRI, lbSA
  GraphemeBreak* = enum
    gbOther, gbCR, gbLF, gbControl, gbExtend, gbZWJ, gbRegionalIndicator, gbPrepend, gbSpacingMark,
    gbL, gbV, gbT, gbLV, gbLVT
  BidiClass* = enum
    bcL, bcR, bcAL, bcEN, bcES, bcET, bcAN, bcCS, bcNSM, bcBN, bcB, bcS, bcWS, bcON, bcLRE, bcLRO,
    bcRLE, bcRLO, bcPDF, bcLRI, bcRLI, bcFSI, bcPDI
  EastAsianWidth* = enum eaN, eaA, eaF, eaH, eaNa, eaW
  BracketType* = enum btNone, btOpen, btClose
  Script* = distinct uint8   ## index into `ucd_data.scNames`

const
  scUnknown* = Script(0)
  scCommon* = Script(1)
  scInherited* = Script(2)
  scLatin* = Script(3)

static:
  for i, n in gcNames: doAssert $GeneralCategory(i) == "gc" & n
  for i, n in lbNames: doAssert $LineBreak(i) == "lb" & n
  for i, n in bcNames: doAssert $BidiClass(i) == "bc" & n
  for i, n in eaNames: doAssert $EastAsianWidth(i) == "ea" & n
  doAssert gcbNames.len == ord(high(GraphemeBreak)) + 1
  doAssert scNames[3] == "Latin"

proc `==`*(a, b: Script): bool {.borrow.}
proc `$`*(s: Script): string = scNames[int(s)]

proc mapValue(t: openArray[uint32]; cp: int): int {.inline.} =
  ## Value of the last entry whose start is <= cp.
  if cp < 0 or cp > 0x10FFFF: return int(t[0] and 0xFF)
  var lo = 0
  var hi = t.len - 1
  let key = uint32(cp)
  while lo < hi:
    let mid = (lo + hi + 1) div 2
    if (t[mid] shr 8) <= key: lo = mid else: hi = mid - 1
  int(t[lo] and 0xFF)

proc inRanges(t: openArray[uint32]; cp: int): bool {.inline.} =
  ## `t` holds inclusive (lo, hi) pairs, sorted.
  var lo = 0
  var hi = t.len div 2 - 1
  let c = uint32(cp)
  while lo <= hi:
    let mid = (lo + hi) div 2
    if c < t[mid * 2]: hi = mid - 1
    elif c > t[mid * 2 + 1]: lo = mid + 1
    else: return true
  false

proc generalCategory*(cp: int): GeneralCategory = GeneralCategory(mapValue(gcTable, cp))
proc lineBreak*(cp: int): LineBreak = LineBreak(mapValue(lbTable, cp))
proc graphemeBreak*(cp: int): GraphemeBreak = GraphemeBreak(mapValue(gcbTable, cp))
proc bidiClass*(cp: int): BidiClass = BidiClass(mapValue(bcTable, cp))
proc eastAsianWidth*(cp: int): EastAsianWidth = EastAsianWidth(mapValue(eaTable, cp))
proc bracketType*(cp: int): BracketType = BracketType(mapValue(bptTable, cp))
proc script*(cp: int): Script = Script(mapValue(scTable, cp))
proc isExtendedPictographic*(cp: int): bool = inRanges(extendedPictographic, cp)
proc isDefaultIgnorable*(cp: int): bool = inRanges(defaultIgnorable, cp)
proc isEmojiPresentation*(cp: int): bool = inRanges(emojiPresentation, cp)

proc joiningType*(cp: int): int =
  ## Arabic joining type: 0 U, 1 L, 2 R, 3 D (join-causing too), 4 Alaph, 5 Dalath/Rish, 6 T.
  let v = mapValue(jtTable, cp)
  if v == 7: 3 else: v

proc isoCode*(s: Script): string = scCodes[int(s)]
  ## ISO 15924 code ("Latn").

proc scriptFromCode*(code: string): Script =
  for i, c in scCodes:
    if c == code: return Script(i)
  scUnknown

proc isRealScript*(s: Script): bool = s != scCommon and s != scUnknown and s != scInherited

proc isMark*(gc: GeneralCategory): bool = gc in {gcMn, gcMc, gcMe}
proc isLetter*(gc: GeneralCategory): bool = gc in {gcLu, gcLl, gcLt, gcLm, gcLo}

proc mirror*(cp: int): int =
  ## Bidi mirroring glyph, 0 when there is none.
  var lo = 0
  var hi = bmgPairs.len div 2 - 1
  let c = uint32(cp)
  while lo <= hi:
    let mid = (lo + hi) div 2
    let k = bmgPairs[mid * 2]
    if c < k: hi = mid - 1
    elif c > k: lo = mid + 1
    else: return int(bmgPairs[mid * 2 + 1])
  0

proc combiningClass*(cp: int): int =
  var lo = 0
  var hi = cccRanges.len div 3 - 1
  let c = uint32(cp)
  while lo <= hi:
    let mid = (lo + hi) div 2
    if c < cccRanges[mid * 3]: hi = mid - 1
    elif c > cccRanges[mid * 3 + 1]: lo = mid + 1
    else: return int(cccRanges[mid * 3 + 2])
  0

const
  HangulSBase = 0xAC00
  HangulLBase = 0x1100
  HangulVBase = 0x1161
  HangulTBase = 0x11A7
  HangulLCount = 19
  HangulVCount = 21
  HangulTCount = 28
  HangulNCount = HangulVCount * HangulTCount
  HangulSCount = HangulLCount * HangulNCount

proc decompose*(cp: int): (int, int) =
  ## Canonical decomposition, one level: (a, b), b = 0 for a singleton; (0, 0) when none.
  if cp >= HangulSBase and cp < HangulSBase + HangulSCount:
    let s = cp - HangulSBase
    let t = s mod HangulTCount
    if t != 0: return (cp - t, HangulTBase + t)
    return (HangulLBase + s div HangulNCount, HangulVBase + (s mod HangulNCount) div HangulTCount)
  var lo = 0
  var hi = decompositions.len div 3 - 1
  let c = uint32(cp)
  while lo <= hi:
    let mid = (lo + hi) div 2
    let k = decompositions[mid * 3]
    if c < k: hi = mid - 1
    elif c > k: lo = mid + 1
    else: return (int(decompositions[mid * 3 + 1]), int(decompositions[mid * 3 + 2]))
  (0, 0)

var compositionTable: Table[(int, int), int]
var compositionReady = false

proc buildCompositions() =
  var i = 0
  while i < decompositions.len:
    let cp = int(decompositions[i])
    let b = int(decompositions[i + 2])
    if b != 0 and not inRanges(compositionExclusions, cp):
      compositionTable[(int(decompositions[i + 1]), b)] = cp
    i += 3
  compositionReady = true

proc compose*(a, b: int): int =
  ## Canonical (primary) composition of a pair, 0 when none.
  if a >= HangulLBase and a < HangulLBase + HangulLCount and b >= HangulVBase and b < HangulVBase + HangulVCount:
    return HangulSBase + ((a - HangulLBase) * HangulVCount + (b - HangulVBase)) * HangulTCount
  if a >= HangulSBase and a < HangulSBase + HangulSCount and (a - HangulSBase) mod HangulTCount == 0 and
     b > HangulTBase and b < HangulTBase + HangulTCount:
    return a + (b - HangulTBase)
  if not compositionReady:
    {.cast(gcSafe).}: buildCompositions()
  {.cast(gcSafe).}:
    compositionTable.getOrDefault((a, b), 0)

proc isWhiteSpace*(cp: int): bool =
  ## Rust `char::is_whitespace` (White_Space property).
  cp in 0x09 .. 0x0D or cp == 0x20 or cp == 0x85 or cp == 0xA0 or cp == 0x1680 or
    cp in 0x2000 .. 0x200A or cp == 0x2028 or cp == 0x2029 or cp == 0x202F or cp == 0x205F or cp == 0x3000
