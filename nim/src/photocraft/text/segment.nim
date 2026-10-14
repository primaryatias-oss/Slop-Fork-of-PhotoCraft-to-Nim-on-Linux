## Text segmentation: line break opportunities (UAX #14, strict, as ICU4X's line segmenter with
## the "normal" word option) and extended grapheme clusters (UAX #29). The Rust build gets these
## from `icu_segmenter`; complex-script dictionaries (Thai, Lao, Khmer, Burmese) are not
## available here, so those scripts break like ordinary letters (UAX #14's SA → AL fallback).

import uprops

proc resolvedClass(cp: int): LineBreak =
  ## LB1: AI, SG, XX → AL; SA → CM (marks) or AL; CJ → NS (strict).
  let c = lineBreak(cp)
  case c
  of lbAI, lbSG, lbXX: lbAL
  of lbSA:
    if generalCategory(cp) in {gcMn, gcMc}: lbCM else: lbAL
  of lbCJ: lbNS
  else: c

proc isWideEa(cp: int): bool = eastAsianWidth(cp) in {eaF, eaW, eaH}

proc lineBreaks*(cps: openArray[int]): seq[bool] =
  ## `result[i]` is true when a line may break before `cps[i]` (i ≥ 1). Mandatory breaks (after
  ## BK, CR, LF, NL) are included; callers that track them separately can still tell them apart
  ## by the character before.
  let n = cps.len
  result = newSeq[bool](n)
  if n == 0: return
  var orig = newSeq[LineBreak](n)
  var cls = newSeq[LineBreak](n)
  var attached = newSeq[bool](n)
  for i in 0 ..< n:
    orig[i] = resolvedClass(cps[i])
    cls[i] = orig[i]
  # LB9 / LB10: combining marks and ZWJ take their base's class; stray ones become AL.
  for i in 0 ..< n:
    if orig[i] in {lbCM, lbZWJ}:
      if i > 0 and cls[i - 1] notin {lbBK, lbCR, lbLF, lbNL, lbSP, lbZW} and
         (orig[i - 1] notin {lbBK, lbCR, lbLF, lbNL, lbSP, lbZW}):
        cls[i] = cls[i - 1]
        attached[i] = true
      else:
        cls[i] = lbAL
  proc lastNonSpace(i: int): int =
    ## Index of the last non-SP character before position `i` (effective classes), -1 if none.
    var j = i - 1
    while j >= 0 and cls[j] == lbSP: dec j
    j
  var riCount = 0   # regional indicators in the run ending just before position i
  for i in 1 ..< n:
    let b = cls[i - 1]
    let a = cls[i]
    let ob = orig[i - 1]
    let oa = orig[i]
    if not attached[i - 1]:
      if ob == lbRI: inc riCount else: riCount = 0
    var brk: bool
    block decide:
      # LB4, LB5
      if ob == lbBK: (brk = true; break decide)
      if ob == lbCR and oa == lbLF: (brk = false; break decide)
      if ob in {lbCR, lbLF, lbNL}: (brk = true; break decide)
      # LB6, LB7
      if oa in {lbBK, lbCR, lbLF, lbNL}: (brk = false; break decide)
      if oa in {lbSP, lbZW}: (brk = false; break decide)
      # LB8: ZW SP* ÷
      let ln = lastNonSpace(i)
      if ln >= 0 and orig[ln] == lbZW: (brk = true; break decide)
      # LB8a, LB9
      if ob == lbZWJ: (brk = false; break decide)
      if attached[i]: (brk = false; break decide)
      # LB11, LB12, LB12a
      if a == lbWJ or b == lbWJ: (brk = false; break decide)
      if b == lbGL: (brk = false; break decide)
      if a == lbGL and b notin {lbSP, lbBA, lbHY}: (brk = false; break decide)
      # LB13
      if a in {lbCL, lbCP, lbEX, lbIS, lbSY}: (brk = false; break decide)
      # LB14–LB17 (SP* sequences)
      let lb = if ln >= 0: cls[ln] else: lbXX
      if lb == lbOP: (brk = false; break decide)
      if a == lbOP and lb == lbQU: (brk = false; break decide)
      if a == lbNS and lb in {lbCL, lbCP}: (brk = false; break decide)
      if a == lbB2 and lb == lbB2: (brk = false; break decide)
      # LB18
      if b == lbSP: (brk = true; break decide)
      # LB19, LB20
      if a == lbQU or b == lbQU: (brk = false; break decide)
      if a == lbCB or b == lbCB: (brk = true; break decide)
      # LB21, LB21a, LB21b
      if a in {lbBA, lbHY, lbNS} or b == lbBB: (brk = false; break decide)
      if b in {lbHY, lbBA}:
        var j = i - 1
        while j > 0 and attached[j]: dec j
        if j >= 1 and cls[j - 1] == lbHL: (brk = false; break decide)
      if b == lbSY and a == lbHL: (brk = false; break decide)
      # LB22
      if a == lbIN: (brk = false; break decide)
      # LB23, LB23a, LB24
      if b in {lbAL, lbHL} and a == lbNU: (brk = false; break decide)
      if b == lbNU and a in {lbAL, lbHL}: (brk = false; break decide)
      if b == lbPR and a in {lbID, lbEB, lbEM}: (brk = false; break decide)
      if b in {lbID, lbEB, lbEM} and a == lbPO: (brk = false; break decide)
      if b in {lbPR, lbPO} and a in {lbAL, lbHL}: (brk = false; break decide)
      if b in {lbAL, lbHL} and a in {lbPR, lbPO}: (brk = false; break decide)
      # LB25 (pair form)
      if (b in {lbCL, lbCP, lbNU} and a in {lbPO, lbPR}) or (b in {lbPO, lbPR} and a in {lbOP, lbNU}) or
         (b in {lbHY, lbIS, lbNU, lbSY} and a == lbNU):
        brk = false
        break decide
      # LB26, LB27
      if b == lbJL and a in {lbJL, lbJV, lbH2, lbH3}: (brk = false; break decide)
      if b in {lbJV, lbH2} and a in {lbJV, lbJT}: (brk = false; break decide)
      if b in {lbJT, lbH3} and a == lbJT: (brk = false; break decide)
      if b in {lbJL, lbJV, lbJT, lbH2, lbH3} and a == lbPO: (brk = false; break decide)
      if b == lbPR and a in {lbJL, lbJV, lbJT, lbH2, lbH3}: (brk = false; break decide)
      # LB28, LB29
      if b in {lbAL, lbHL} and a in {lbAL, lbHL}: (brk = false; break decide)
      if b == lbIS and a in {lbAL, lbHL}: (brk = false; break decide)
      # LB30
      if b in {lbAL, lbHL, lbNU} and a == lbOP and not isWideEa(cps[i]): (brk = false; break decide)
      if b == lbCP and a in {lbAL, lbHL, lbNU} and not isWideEa(cps[i - 1]): (brk = false; break decide)
      # LB30a: regional indicator pairs
      if b == lbRI and a == lbRI and riCount mod 2 == 1: (brk = false; break decide)
      # LB30b
      if b == lbEB and a == lbEM: (brk = false; break decide)
      if a == lbEM and isExtendedPictographic(cps[i - 1]) and generalCategory(cps[i - 1]) == gcCn:
        brk = false
        break decide
      brk = true
    result[i] = brk

proc isMandatoryBreak*(cp: int): bool =
  ## A character after which the line must break (BK, CR, LF, NL).
  lineBreak(cp) in {lbBK, lbCR, lbLF, lbNL}

proc graphemeBoundaries*(cps: openArray[int]): seq[bool] =
  ## `result[i]` is true when a grapheme cluster starts at `cps[i]` (always true for i = 0).
  let n = cps.len
  result = newSeq[bool](n)
  if n == 0: return
  result[0] = true
  var prev = graphemeBreak(cps[0])
  var riRun = (if prev == gbRegionalIndicator: 1 else: 0)   # consecutive RIs ending at i - 1
  # GB11 state: inside "ExtPict Extend*" (and whether the last was ZWJ).
  var emojiSeq = isExtendedPictographic(cps[0])
  for i in 1 ..< n:
    let cur = graphemeBreak(cps[i])
    var brk: bool
    block decide:
      if prev == gbCR and cur == gbLF: (brk = false; break decide)
      if prev in {gbControl, gbCR, gbLF}: (brk = true; break decide)
      if cur in {gbControl, gbCR, gbLF}: (brk = true; break decide)
      if prev == gbL and cur in {gbL, gbV, gbLV, gbLVT}: (brk = false; break decide)
      if prev in {gbLV, gbV} and cur in {gbV, gbT}: (brk = false; break decide)
      if prev in {gbLVT, gbT} and cur == gbT: (brk = false; break decide)
      if cur in {gbExtend, gbZWJ}: (brk = false; break decide)
      if cur == gbSpacingMark: (brk = false; break decide)
      if prev == gbPrepend: (brk = false; break decide)
      if prev == gbZWJ and emojiSeq and isExtendedPictographic(cps[i]): (brk = false; break decide)
      if prev == gbRegionalIndicator and cur == gbRegionalIndicator and riRun mod 2 == 1:
        brk = false
        break decide
      brk = true
    result[i] = brk
    # Track GB11 and GB12/13 state.
    if isExtendedPictographic(cps[i]): emojiSeq = true
    elif cur == gbExtend and emojiSeq: discard
    elif cur == gbZWJ and emojiSeq: discard
    else: emojiSeq = false
    if cur == gbRegionalIndicator: inc riRun else: riRun = 0
    prev = cur
