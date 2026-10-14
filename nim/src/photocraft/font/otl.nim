## OpenType Layout (GSUB/GPOS) application, following HarfBuzz's semantics (the engine parley
## uses through HarfRust in the Rust build): glyph properties from GDEF, lookup flags and mark
## filtering sets, ligature ids/components for mark attachment, context and chaining-context
## lookups with nested lookup application, mark/cursive attachment chains resolved at the end.
##
## The buffer holds glyphs in logical order; `shape.nim` drives the pipeline around it.

import std/[options, tables, algorithm]
import sfnt

const
  gpBase* = 0x02
  gpLigature* = 0x04
  gpMark* = 0x08
  gpSubstituted* = 0x10
  gpLigated* = 0x20
  gpMultiplied* = 0x40
  gpPreserve = gpSubstituted or gpLigated or gpMultiplied

  ufIgnorable* = 0x01'u16   ## Default_Ignorable_Code_Point
  ufHidden* = 0x02'u16      ## ignorable that GSUB must still see (CGJ, Mongolian FVS, tags)
  ufZwj* = 0x04'u16
  ufZwnj* = 0x08'u16
  ufContinuation* = 0x10'u16

  attachMark* = 1'u8
  attachCursive* = 2'u8

  IsLigBase = 0x10
  MaxNesting = 64
  MaxContext = 64

  lfRightToLeft = 0x0001
  lfIgnoreFlags = 0x000E
  lfIgnoreMarks = 0x0008
  lfUseMarkFilteringSet = 0x0010
  lfMarkAttachmentType = 0xFF00

type
  HbGlyph* = object
    cp*: int               ## Unicode code point the glyph came from (after normalization)
    gid*: int
    cluster*: int
    gc*: uint8             ## `uprops.GeneralCategory` ordinal
    ccc*: uint8            ## canonical combining class
    uflags*: uint16
    space*: uint8          ## space fallback type (`shape.nim`)
    props*: int            ## glyph props: GDEF class bits, mark attachment class << 8, substitution flags
    ligProps*: int
    xAdvance*, yAdvance*, xOffset*, yOffset*: int32
    attachChain*: int
    attachType*: uint8
    mask*: uint32          ## features that apply to this glyph (one bit per feature)

  Table0 = object
    ok: bool
    base, scripts, features, lookups, variations: int

  LookupInfo = object
    kind, props: int
    subs: seq[int]

  OtlData = ref object of RootObj
    gsub, gpos: Table0
    gdef, glyphClassDef, markAttachClassDef, markGlyphSets: int
    hasGlyphClasses*: bool
    gsubLookups, gposLookups: Table[int, LookupInfo]

  Ctx = object
    f: Face
    d: OtlData
    gsub: bool
    buf: seq[HbGlyph]
    idx: int
    lookupProps: int
    lookupMask: uint32
    autoZwj: bool
    featureValue: int
    nesting: int
    rtl: bool
    serial: int
    hasAttachment: bool

# ---------------------------------------------------------------- byte access

proc u16(f: Face; at: int): int {.inline.} =
  if at < 0 or at + 2 > f.blob.data.len: raise newException(FontError, "layout table is truncated")
  (int(f.blob.data[at]) shl 8) or int(f.blob.data[at + 1])

proc i16(f: Face; at: int): int =
  let v = f.u16(at)
  if v >= 0x8000: v - 0x10000 else: v

proc u32(f: Face; at: int): int = (f.u16(at) shl 16) or f.u16(at + 2)

proc tag(f: Face; at: int): string =
  if at < 0 or at + 4 > f.blob.data.len: raise newException(FontError, "layout table is truncated")
  result = newString(4)
  for i in 0 ..< 4: result[i] = char(f.blob.data[at + i])

# ---------------------------------------------------------------- tables

proc header(f: Face; t: string): Table0 =
  let tb = f.table(t)
  if tb.isNone: return
  let b = tb.get[0]
  try:
    result = Table0(ok: true, base: b, scripts: b + f.u16(b + 4), features: b + f.u16(b + 6),
                    lookups: b + f.u16(b + 8))
    if f.u16(b + 2) >= 1 and f.u32(b + 10) != 0: result.variations = b + f.u32(b + 10)
  except FontError: result = Table0()

proc data(f: Face): OtlData =
  if not f.layoutCache.isNil and f.layoutCache of OtlData: return OtlData(f.layoutCache)
  let d = OtlData(gsub: f.header("GSUB"), gpos: f.header("GPOS"))
  let g = f.table("GDEF")
  if g.isSome:
    try:
      let b = g.get[0]
      d.gdef = b
      let gc = f.u16(b + 4)
      if gc != 0:
        d.glyphClassDef = b + gc
        d.hasGlyphClasses = true
      let mac = f.u16(b + 10)
      if mac != 0: d.markAttachClassDef = b + mac
      if f.u16(b) == 1 and f.u16(b + 2) >= 2:
        let ms = f.u16(b + 12)
        if ms != 0: d.markGlyphSets = b + ms
    except FontError: discard
  f.layoutCache = d
  d

proc hasGsub*(f: Face): bool = f.data.gsub.ok
proc hasGpos*(f: Face): bool = f.data.gpos.ok
proc hasGlyphClasses*(f: Face): bool = f.data.hasGlyphClasses

proc coverageIndex(f: Face; at, gid: int): int =
  ## Index of `gid` in the coverage table at `at`, -1 when absent.
  let fmt = f.u16(at)
  if fmt == 1:
    var lo = 0
    var hi = f.u16(at + 2) - 1
    while lo <= hi:
      let mid = (lo + hi) div 2
      let g = f.u16(at + 4 + mid * 2)
      if g == gid: return mid
      if g < gid: lo = mid + 1 else: hi = mid - 1
  elif fmt == 2:
    var lo = 0
    var hi = f.u16(at + 2) - 1
    while lo <= hi:
      let mid = (lo + hi) div 2
      let r = at + 4 + mid * 6
      if gid < f.u16(r): hi = mid - 1
      elif gid > f.u16(r + 2): lo = mid + 1
      else: return f.u16(r + 4) + gid - f.u16(r)
  -1

proc classOf(f: Face; at, gid: int): int =
  if at == 0: return 0
  let fmt = f.u16(at)
  if fmt == 1:
    let start = f.u16(at + 2)
    let n = f.u16(at + 4)
    if gid >= start and gid < start + n: return f.u16(at + 6 + (gid - start) * 2)
  elif fmt == 2:
    var lo = 0
    var hi = f.u16(at + 2) - 1
    while lo <= hi:
      let mid = (lo + hi) div 2
      let r = at + 4 + mid * 6
      if gid < f.u16(r): hi = mid - 1
      elif gid > f.u16(r + 2): lo = mid + 1
      else: return f.u16(r + 4)
  0

proc gdefClass*(f: Face; gid: int): int =
  ## GDEF glyph class: 1 base, 2 ligature, 3 mark, 4 component; 0 unknown.
  let d = f.data
  if d.glyphClassDef == 0: return 0
  try: f.classOf(d.glyphClassDef, gid) except FontError: 0

proc glyphProps*(f: Face; gid: int): int =
  ## HarfBuzz glyph props for a glyph from GDEF.
  let d = f.data
  try:
    case f.classOf(d.glyphClassDef, gid)
    of 1: gpBase
    of 2: gpLigature
    of 3: gpMark or (f.classOf(d.markAttachClassDef, gid) shl 8)
    else: 0
  except FontError: 0

proc markSetCovers(f: Face; setIndex, gid: int): bool =
  let d = f.data
  if d.markGlyphSets == 0: return false
  try:
    if setIndex >= f.u16(d.markGlyphSets + 2): return false
    f.coverageIndex(d.markGlyphSets + f.u32(d.markGlyphSets + 4 + setIndex * 4), gid) >= 0
  except FontError: false

# ---------------------------------------------------------------- feature selection

proc chooseLangSys(f: Face; t: Table0; scriptTags, langTags: openArray[string]): (int, string) =
  ## The LangSys table offset for the first script tag present (then DFLT, dflt, latn) and the
  ## first language present (then 'dflt', then the default LangSys), and the chosen script tag.
  if not t.ok: return (0, "")
  try:
    let ns = f.u16(t.scripts)
    var scriptAt = 0
    var chosen = ""
    var tries = @scriptTags
    tries.add ["DFLT", "dflt", "latn"]
    for want in tries:
      for i in 0 ..< ns:
        if f.tag(t.scripts + 2 + i * 6) == want:
          scriptAt = t.scripts + f.u16(t.scripts + 2 + i * 6 + 4)
          chosen = want
          break
      if scriptAt != 0: break
    if scriptAt == 0: return (0, "")
    let nl = f.u16(scriptAt + 2)
    var langs = @langTags
    langs.add "dflt"
    for want in langs:
      for i in 0 ..< nl:
        if f.tag(scriptAt + 4 + i * 6) == want:
          return (scriptAt + f.u16(scriptAt + 4 + i * 6 + 4), chosen)
    let d = f.u16(scriptAt)
    if d != 0: return (scriptAt + d, chosen)
    (0, chosen)
  except FontError: (0, "")

proc chosenScript*(f: Face; gsub: bool; scriptTags: openArray[string]): string =
  let d = f.data
  f.chooseLangSys(if gsub: d.gsub else: d.gpos, scriptTags, [])[1]

proc featureIndex(f: Face; t: Table0; langSys: int; feature: string): int =
  ## Index of `feature` among the LangSys's features, -1 when absent.
  if langSys == 0: return -1
  try:
    let total = f.u16(t.features)
    let n = f.u16(langSys + 4)
    for i in 0 ..< n:
      let fi = f.u16(langSys + 6 + i * 2)
      if fi < total and f.tag(t.features + 2 + fi * 6) == feature: return fi
  except FontError: discard
  -1

proc hasFeature*(f: Face; table, feature: string; scriptTags: openArray[string] = []; langTags: openArray[string] = []): bool =
  ## Whether the chosen script/language system of GSUB or GPOS has `feature`; with no script tags,
  ## whether any feature record has that tag.
  let d = f.data
  let t = if table == "GSUB": d.gsub else: d.gpos
  if not t.ok: return false
  if scriptTags.len == 0:
    try:
      let n = f.u16(t.features)
      for i in 0 ..< n:
        if f.tag(t.features + 2 + i * 6) == feature: return true
    except FontError: discard
    return false
  let (ls, _) = f.chooseLangSys(t, scriptTags, langTags)
  f.featureIndex(t, ls, feature) >= 0

proc featureLookups(f: Face; t: Table0; fi: int): seq[int] =
  try:
    let fat = t.features + f.u16(t.features + 2 + fi * 6 + 4)
    for k in 0 ..< f.u16(fat + 2): result.add f.u16(fat + 4 + k * 2)
  except FontError: discard

type StageLookup* = tuple[lookup, value: int, mask: uint32, autoZwj: bool]

proc featureLookupIndices*(f: Face; gsub: bool; scriptTags, langTags: openArray[string]; feature: string): seq[int] =
  ## Lookup indices of `feature` in the chosen script/language system (empty when absent).
  let d = f.data
  let t = if gsub: d.gsub else: d.gpos
  let (ls, _) = f.chooseLangSys(t, scriptTags, langTags)
  let fi = f.featureIndex(t, ls, feature)
  if fi >= 0: result = f.featureLookups(t, fi)

proc requiredFeatureLookups*(f: Face; gsub: bool; scriptTags, langTags: openArray[string]): seq[int] =
  let d = f.data
  let t = if gsub: d.gsub else: d.gpos
  let (ls, _) = f.chooseLangSys(t, scriptTags, langTags)
  if ls == 0: return
  try:
    let r = f.u16(ls + 2)
    if r != 0xFFFF and r < f.u16(t.features): result = f.featureLookups(t, r)
  except FontError: discard

# ---------------------------------------------------------------- lookups

proc lookupInfo(c: var Ctx; index: int): LookupInfo =
  let cache = if c.gsub: addr c.d.gsubLookups else: addr c.d.gposLookups
  if index in cache[]: return cache[][index]
  let f = c.f
  let t = if c.gsub: c.d.gsub else: c.d.gpos
  try:
    if index < f.u16(t.lookups):
      let lt = t.lookups + f.u16(t.lookups + 2 + index * 2)
      var kind = f.u16(lt)
      let flag = f.u16(lt + 2)
      let count = f.u16(lt + 4)
      var subs: seq[int]
      for i in 0 ..< count: subs.add lt + f.u16(lt + 6 + i * 2)
      var props = flag
      if (flag and lfUseMarkFilteringSet) != 0: props = props or (f.u16(lt + 6 + count * 2) shl 16)
      let ext = if c.gsub: 7 else: 9
      if kind == ext:
        var resolved: seq[int]
        for s in subs:
          if f.u16(s) == 1:
            kind = f.u16(s + 2)
            resolved.add s + f.u32(s + 4)
        subs = resolved
      result = LookupInfo(kind: kind, props: props, subs: subs)
  except FontError: result = LookupInfo()
  cache[][index] = result

# ---------------------------------------------------------------- glyph helpers

proc ligId(g: HbGlyph): int {.inline.} = g.ligProps shr 5
proc ligatedInternal(g: HbGlyph): bool {.inline.} = (g.ligProps and IsLigBase) != 0
proc ligComp(g: HbGlyph): int {.inline.} = (if g.ligatedInternal: 0 else: g.ligProps and 0x0F)
proc ligNumComps(g: HbGlyph): int {.inline.} =
  if (g.props and gpLigature) != 0 and g.ligatedInternal: g.ligProps and 0x0F else: 1
proc setLigForLigature(g: var HbGlyph; id, n: int) {.inline.} = g.ligProps = (id shl 5) or IsLigBase or (n and 0x0F)
proc setLigForMark(g: var HbGlyph; id, comp: int) {.inline.} = g.ligProps = (id shl 5) or (comp and 0x0F)
proc isMark(g: HbGlyph): bool {.inline.} = (g.props and gpMark) != 0
proc isBaseGlyph(g: HbGlyph): bool {.inline.} = (g.props and gpBase) != 0
proc isLigature(g: HbGlyph): bool {.inline.} = (g.props and gpLigature) != 0
proc multiplied(g: HbGlyph): bool {.inline.} = (g.props and gpMultiplied) != 0

proc allocateLigId(c: var Ctx): int =
  for _ in 0 ..< 2:
    result = c.serial and 7
    inc c.serial
    if result != 0: return

proc checkGlyphProperty(c: Ctx; g: HbGlyph; lookupProps: int): bool =
  let gp = g.props
  if (gp and lookupProps and lfIgnoreFlags) != 0: return false
  if (gp and gpMark) != 0:
    if (lookupProps and lfUseMarkFilteringSet) != 0: return c.f.markSetCovers(lookupProps shr 16, g.gid)
    if (lookupProps and lfMarkAttachmentType) != 0:
      return (lookupProps and lfMarkAttachmentType) == (gp and lfMarkAttachmentType)
  true

proc setGlyphClass(c: var Ctx; g: var HbGlyph; gid: int; classGuess = 0; ligature = false; component = false) =
  var props = g.props or gpSubstituted
  if ligature: props = (props or gpLigated) and not gpMultiplied
  if component: props = props or gpMultiplied
  if c.d.hasGlyphClasses: props = (props and gpPreserve) or c.f.glyphProps(gid)
  elif classGuess != 0: props = (props and gpPreserve) or classGuess
  g.props = props
  g.gid = gid

proc mergeClusters(c: var Ctx; start, stop: int) =
  var s = start
  var e = stop
  if e - s < 2 or s < 0 or e > c.buf.len: return
  var cl = c.buf[s].cluster
  for i in s + 1 ..< e: cl = min(cl, c.buf[i].cluster)
  if cl != c.buf[e - 1].cluster:
    while e < c.buf.len and c.buf[e - 1].cluster == c.buf[e].cluster: inc e
  if cl != c.buf[s].cluster:
    while s > 0 and c.buf[s - 1].cluster == c.buf[s].cluster: dec s
  for i in s ..< e: c.buf[i].cluster = cl

# ---------------------------------------------------------------- skipping iterator

type
  Skip = enum skNo, skMaybe, skYes
  MatchRes = enum mrMatch, mrNotMatch, mrSkip
  Matcher = proc(gid: int; k: int): bool {.closure.}

proc maySkip(c: Ctx; g: HbGlyph; lookupProps: int; ignoreHidden, ignoreZwj: bool): Skip =
  if not c.checkGlyphProperty(g, lookupProps): return skYes
  if (g.uflags and ufIgnorable) != 0 and (ignoreZwj or (g.uflags and ufZwj) == 0) and
     (ignoreHidden or (g.uflags and ufHidden) == 0): return skMaybe
  skNo

proc matchGlyph(c: Ctx; i: int; lookupProps: int; m: Matcher; k: int; mask = high(uint32);
                context = false): MatchRes =
  let g = c.buf[i]
  let s = c.maySkip(g, lookupProps, not c.gsub, not c.gsub or c.autoZwj or context)
  if s == skYes: return mrSkip
  if (g.mask and mask) == 0:
    if s == skNo: return mrNotMatch
    return mrSkip
  if m.isNil:
    if s == skNo: return mrMatch
    return mrSkip
  if m(g.gid, k): return mrMatch
  if s == skNo: return mrNotMatch
  mrSkip

proc skipNext(c: Ctx; start: int; lookupProps: int; m: Matcher; k: int; mask = high(uint32);
              context = false): int =
  ## Next matching position after `start`, -1 when a glyph fails to match or the buffer ends.
  var i = start + 1
  while i < c.buf.len:
    case c.matchGlyph(i, lookupProps, m, k, mask, context)
    of mrMatch: return i
    of mrNotMatch: return -1
    of mrSkip: inc i
  -1

proc skipPrev(c: Ctx; start: int; lookupProps: int; m: Matcher; k: int; context = false): int =
  var i = start - 1
  while i >= 0:
    case c.matchGlyph(i, lookupProps, m, k, high(uint32), context)
    of mrMatch: return i
    of mrNotMatch: return -1
    of mrSkip: dec i
  -1

proc matchInput(c: var Ctx; count: int; m: Matcher; positions: var seq[int]; matchEnd: var int;
                totalComponents: var int): bool =
  ## Matches the `count - 1` glyphs after the current one (`m(gid, k)` for k in 0 ..< count - 1).
  positions.setLen(0)
  positions.add c.idx
  let first = c.buf[c.idx]
  let firstLigId = first.ligId
  let firstLigComp = first.ligComp
  totalComponents = first.ligNumComps
  var p = c.idx
  for k in 0 ..< count - 1:
    p = c.skipNext(p, c.lookupProps, m, k, c.lookupMask)
    if p < 0: return false
    let g = c.buf[p]
    let thisLigId = g.ligId
    let thisLigComp = g.ligComp
    if firstLigId != 0 and firstLigComp != 0:
      if firstLigId != thisLigId or firstLigComp != thisLigComp: return false
    elif thisLigId != 0 and thisLigComp != 0 and thisLigId != firstLigId:
      return false
    totalComponents += g.ligNumComps
    positions.add p
  matchEnd = p + 1
  true

proc matchBacktrack(c: Ctx; count: int; m: Matcher): bool =
  var p = c.idx
  for k in 0 ..< count:
    p = c.skipPrev(p, c.lookupProps, m, k, context = true)
    if p < 0: return false
  true

proc matchLookahead(c: Ctx; count: int; m: Matcher; startIndex: int): bool =
  var p = startIndex - 1
  for k in 0 ..< count:
    p = c.skipNext(p, c.lookupProps, m, k, context = true)
    if p < 0: return false
  true

# ---------------------------------------------------------------- application

proc applyLookupAt(c: var Ctx; lookupIndex: int): bool

proc recurse(c: var Ctx; lookupIndex: int): bool =
  if c.nesting >= MaxNesting: return false
  let saved = (c.lookupProps, c.featureValue)
  inc c.nesting
  result = c.applyLookupAt(lookupIndex)
  dec c.nesting
  (c.lookupProps, c.featureValue) = saved

proc applyLookupRecords(c: var Ctx; positions: var seq[int]; records: seq[(int, int)]; matchEnd: int) =
  var count = positions.len
  var endPos = matchEnd
  for (seqIndex, lookupIndex) in records:
    if seqIndex >= count: continue
    let idx = seqIndex
    let origLen = c.buf.len
    if positions[idx] >= c.buf.len: continue
    c.idx = positions[idx]
    if not c.recurse(lookupIndex): continue
    var delta = c.buf.len - origLen
    if delta == 0: continue
    endPos += delta
    if endPos < positions[idx]:
      delta += positions[idx] - endPos
      endPos = positions[idx]
    var next = idx + 1
    if delta > 0:
      if delta + count > MaxContext: break
    else:
      delta = max(delta, next - count)
      next -= delta
    # Shift the positions after `next` by `delta` slots.
    let tail = positions[next ..< count]
    positions.setLen(max(count + delta, 0))
    for j, v in tail:
      if next + delta + j < positions.len: positions[next + delta + j] = v
    next += delta
    count += delta
    for j in idx + 1 ..< min(next, positions.len): positions[j] = positions[j - 1] + 1
    for j in next ..< min(count, positions.len): positions[j] += delta
  c.idx = endPos

proc recordsAt(f: Face; at, n: int): seq[(int, int)] =
  for k in 0 ..< n: result.add (f.u16(at + k * 4), f.u16(at + k * 4 + 2))

proc contextApply(c: var Ctx; inputCount: int; inputM: Matcher; backCount: int; backM: Matcher;
                  aheadCount: int; aheadM: Matcher; records: seq[(int, int)]): bool =
  var positions: seq[int]
  var matchEnd = 0
  var comps = 0
  if not c.matchInput(inputCount, inputM, positions, matchEnd, comps): return false
  if backCount > 0 and not c.matchBacktrack(backCount, backM): return false
  if aheadCount > 0 and not c.matchLookahead(aheadCount, aheadM, matchEnd): return false
  c.applyLookupRecords(positions, records, matchEnd)
  true

proc contextual(c: var Ctx; sub: int; chained: bool): bool =
  ## Context (GSUB 5 / GPOS 7) and chaining context (GSUB 6 / GPOS 8), formats 1–3.
  let f = c.f
  let g = c.buf[c.idx].gid
  let fmt = f.u16(sub)
  if not chained:
    case fmt
    of 1:
      let ci = f.coverageIndex(sub + f.u16(sub + 2), g)
      if ci < 0 or ci >= f.u16(sub + 4): return false
      let rs = sub + f.u16(sub + 6 + ci * 2)
      for r in 0 ..< f.u16(rs):
        let rule = rs + f.u16(rs + 2 + r * 2)
        let gc = f.u16(rule)
        let lc = f.u16(rule + 2)
        if gc == 0: continue
        let m: Matcher = proc(gid, k: int): bool = f.u16(rule + 4 + k * 2) == gid
        if c.contextApply(gc, m, 0, nil, 0, nil, f.recordsAt(rule + 4 + (gc - 1) * 2, lc)): return true
    of 2:
      if f.coverageIndex(sub + f.u16(sub + 2), g) < 0: return false
      let cd = sub + f.u16(sub + 4)
      let cls = f.classOf(cd, g)
      if cls >= f.u16(sub + 6): return false
      let off = f.u16(sub + 8 + cls * 2)
      if off == 0: return false
      let rs = sub + off
      for r in 0 ..< f.u16(rs):
        let rule = rs + f.u16(rs + 2 + r * 2)
        let gc = f.u16(rule)
        let lc = f.u16(rule + 2)
        if gc == 0: continue
        let m: Matcher = proc(gid, k: int): bool = f.classOf(cd, gid) == f.u16(rule + 4 + k * 2)
        if c.contextApply(gc, m, 0, nil, 0, nil, f.recordsAt(rule + 4 + (gc - 1) * 2, lc)): return true
    of 3:
      let gc = f.u16(sub + 2)
      let lc = f.u16(sub + 4)
      if gc == 0 or f.coverageIndex(sub + f.u16(sub + 6), g) < 0: return false
      let m: Matcher = proc(gid, k: int): bool = f.coverageIndex(sub + f.u16(sub + 6 + (k + 1) * 2), gid) >= 0
      return c.contextApply(gc, m, 0, nil, 0, nil, f.recordsAt(sub + 6 + gc * 2, lc))
    else: discard
    return false
  case fmt
  of 1:
    let ci = f.coverageIndex(sub + f.u16(sub + 2), g)
    if ci < 0 or ci >= f.u16(sub + 4): return false
    let rs = sub + f.u16(sub + 6 + ci * 2)
    for r in 0 ..< f.u16(rs):
      var p = rs + f.u16(rs + 2 + r * 2)
      let bc = f.u16(p)
      let back = p + 2
      p += 2 + bc * 2
      let ic = f.u16(p)
      let input = p + 2
      if ic == 0: continue
      p += 2 + (ic - 1) * 2
      let lac = f.u16(p)
      let ahead = p + 2
      p += 2 + lac * 2
      let lc = f.u16(p)
      let mi: Matcher = proc(gid, k: int): bool = f.u16(input + k * 2) == gid
      let mb: Matcher = proc(gid, k: int): bool = f.u16(back + k * 2) == gid
      let ma: Matcher = proc(gid, k: int): bool = f.u16(ahead + k * 2) == gid
      if c.contextApply(ic, mi, bc, mb, lac, ma, f.recordsAt(p + 2, lc)): return true
  of 2:
    if f.coverageIndex(sub + f.u16(sub + 2), g) < 0: return false
    let bcd = sub + f.u16(sub + 4)
    let icd = sub + f.u16(sub + 6)
    let lcd = sub + f.u16(sub + 8)
    let cls = f.classOf(icd, g)
    if cls >= f.u16(sub + 10): return false
    let off = f.u16(sub + 12 + cls * 2)
    if off == 0: return false
    let rs = sub + off
    for r in 0 ..< f.u16(rs):
      var p = rs + f.u16(rs + 2 + r * 2)
      let bc = f.u16(p)
      let back = p + 2
      p += 2 + bc * 2
      let ic = f.u16(p)
      let input = p + 2
      if ic == 0: continue
      p += 2 + (ic - 1) * 2
      let lac = f.u16(p)
      let ahead = p + 2
      p += 2 + lac * 2
      let lc = f.u16(p)
      let mi: Matcher = proc(gid, k: int): bool = f.classOf(icd, gid) == f.u16(input + k * 2)
      let mb: Matcher = proc(gid, k: int): bool = f.classOf(bcd, gid) == f.u16(back + k * 2)
      let ma: Matcher = proc(gid, k: int): bool = f.classOf(lcd, gid) == f.u16(ahead + k * 2)
      if c.contextApply(ic, mi, bc, mb, lac, ma, f.recordsAt(p + 2, lc)): return true
  of 3:
    var p = sub + 2
    let bc = f.u16(p)
    let back = p + 2
    p += 2 + bc * 2
    let ic = f.u16(p)
    let input = p + 2
    p += 2 + ic * 2
    let lac = f.u16(p)
    let ahead = p + 2
    p += 2 + lac * 2
    let lc = f.u16(p)
    if ic == 0 or f.coverageIndex(sub + f.u16(input), g) < 0: return false
    let mi: Matcher = proc(gid, k: int): bool = f.coverageIndex(sub + f.u16(input + (k + 1) * 2), gid) >= 0
    let mb: Matcher = proc(gid, k: int): bool = f.coverageIndex(sub + f.u16(back + k * 2), gid) >= 0
    let ma: Matcher = proc(gid, k: int): bool = f.coverageIndex(sub + f.u16(ahead + k * 2), gid) >= 0
    return c.contextApply(ic, mi, bc, mb, lac, ma, f.recordsAt(p + 2, lc))
  else: discard
  false

# ---------------------------------------------------------------- GSUB

proc replaceGlyph(c: var Ctx; gid: int) =
  c.setGlyphClass(c.buf[c.idx], gid)
  inc c.idx

proc deleteGlyph(c: var Ctx) =
  ## Removes the current glyph, merging its cluster into a neighbour (HarfBuzz `delete_glyph`).
  let i = c.idx
  let cl = c.buf[i].cluster
  let survives = (i + 1 < c.buf.len and c.buf[i + 1].cluster == cl) or (i > 0 and c.buf[i - 1].cluster == cl)
  if not survives:
    if i > 0:
      if cl < c.buf[i - 1].cluster:
        let old = c.buf[i - 1].cluster
        var j = i
        while j > 0 and c.buf[j - 1].cluster == old:
          c.buf[j - 1].cluster = cl
          dec j
    elif i + 1 < c.buf.len:
      c.mergeClusters(i, i + 2)
  c.buf.delete(i)

proc ligate(c: var Ctx; positions: seq[int]; matchEnd: int; ligGlyph: int; totalComponents: int) =
  c.mergeClusters(c.idx, matchEnd)
  let count = positions.len
  var isBaseLigature = c.buf[positions[0]].isBaseGlyph
  var isMarkLigature = c.buf[positions[0]].isMark
  for i in 1 ..< count:
    if not c.buf[positions[i]].isMark:
      isBaseLigature = false
      isMarkLigature = false
      break
  let isLig = not isBaseLigature and not isMarkLigature
  let klass = if isLig: gpLigature else: 0
  let ligIdNew = if isLig: c.allocateLigId() else: 0
  let first = positions[0]
  var lastLigId = c.buf[first].ligId
  var lastNumComponents = c.buf[first].ligNumComps
  var componentsSoFar = lastNumComponents
  if isLig:
    c.buf[first].setLigForLigature(ligIdNew, totalComponents)
    if c.buf[first].gc == 6'u8:   # Mn → Lo
      c.buf[first].gc = 5'u8
  c.setGlyphClass(c.buf[first], ligGlyph, klass, ligature = true)
  # Walk the components: marks in between are re-attached; the components are removed.
  var removed = 0
  var j = first + 1
  for k in 1 ..< count:
    let target = positions[k] - removed
    while j < target:
      if isLig:
        var thisComp = c.buf[j].ligComp
        if thisComp == 0: thisComp = lastNumComponents
        let newComp = componentsSoFar - lastNumComponents + min(thisComp, lastNumComponents)
        c.buf[j].setLigForMark(ligIdNew, newComp)
      inc j
    lastLigId = c.buf[j].ligId
    lastNumComponents = c.buf[j].ligNumComps
    componentsSoFar += lastNumComponents
    c.buf.delete(j)
    inc removed
  if not isMarkLigature and lastLigId != 0:
    var i = j
    while i < c.buf.len:
      if c.buf[i].ligId != lastLigId: break
      let thisComp = c.buf[i].ligComp
      if thisComp == 0: break
      c.buf[i].setLigForMark(ligIdNew, componentsSoFar - lastNumComponents + min(thisComp, lastNumComponents))
      inc i
  c.idx = j

proc applyGsub(c: var Ctx; kind, sub: int): bool =
  let f = c.f
  let g = c.buf[c.idx].gid
  let fmt = f.u16(sub)
  case kind
  of 1:
    let ci = f.coverageIndex(sub + f.u16(sub + 2), g)
    if ci < 0: return false
    if fmt == 1: c.replaceGlyph((g + f.i16(sub + 4)) and 0xFFFF)
    elif fmt == 2:
      if ci >= f.u16(sub + 4): return false
      c.replaceGlyph(f.u16(sub + 6 + ci * 2))
    else: return false
    true
  of 2:
    let ci = f.coverageIndex(sub + f.u16(sub + 2), g)
    if ci < 0 or ci >= f.u16(sub + 4): return false
    let s = sub + f.u16(sub + 6 + ci * 2)
    let n = f.u16(s)
    if n == 1:
      c.replaceGlyph(f.u16(s + 2))
      return true
    if n == 0:
      c.deleteGlyph()
      return true
    let cur = c.buf[c.idx]
    let klass = if cur.isLigature: gpBase else: 0
    let lid = cur.ligId
    var repl: seq[HbGlyph]
    for k in 0 ..< n:
      var x = cur
      if lid == 0: x.setLigForMark(0, k)
      c.setGlyphClass(x, f.u16(s + 2 + k * 2), klass, component = true)
      repl.add x
    c.buf[c.idx .. c.idx] = repl
    c.idx += n
    true
  of 3:
    let ci = f.coverageIndex(sub + f.u16(sub + 2), g)
    if ci < 0 or ci >= f.u16(sub + 4): return false
    let s = sub + f.u16(sub + 6 + ci * 2)
    let n = f.u16(s)
    let alt = c.featureValue - 1
    if alt < 0 or alt >= n: return false
    c.replaceGlyph(f.u16(s + 2 + alt * 2))
    true
  of 4:
    let ci = f.coverageIndex(sub + f.u16(sub + 2), g)
    if ci < 0 or ci >= f.u16(sub + 4): return false
    let ls = sub + f.u16(sub + 6 + ci * 2)
    for k in 0 ..< f.u16(ls):
      let lig = ls + f.u16(ls + 2 + k * 2)
      let cc = f.u16(lig + 2)
      if cc == 0: continue
      if cc == 1:
        c.replaceGlyph(f.u16(lig))
        return true
      let m: Matcher = proc(gid, k: int): bool = f.u16(lig + 4 + k * 2) == gid
      var positions: seq[int]
      var matchEnd, comps: int
      if not c.matchInput(cc, m, positions, matchEnd, comps): continue
      c.ligate(positions, matchEnd, f.u16(lig), comps)
      return true
    false
  of 5: c.contextual(sub, false)
  of 6: c.contextual(sub, true)
  of 8:
    # Reverse chaining contextual single (applied from the end; see `applyLookupString`).
    if fmt != 1: return false
    let ci = f.coverageIndex(sub + f.u16(sub + 2), g)
    if ci < 0: return false
    var p = sub + 4
    let bc = f.u16(p)
    let back = p + 2
    p += 2 + bc * 2
    let lac = f.u16(p)
    let ahead = p + 2
    p += 2 + lac * 2
    let n = f.u16(p)
    if ci >= n: return false
    let mb: Matcher = proc(gid, k: int): bool = f.coverageIndex(sub + f.u16(back + k * 2), gid) >= 0
    let ma: Matcher = proc(gid, k: int): bool = f.coverageIndex(sub + f.u16(ahead + k * 2), gid) >= 0
    if bc > 0 and not c.matchBacktrack(bc, mb): return false
    if lac > 0 and not c.matchLookahead(lac, ma, c.idx + 1): return false
    c.setGlyphClass(c.buf[c.idx], f.u16(p + 2 + ci * 2))
    true
  else: false

# ---------------------------------------------------------------- GPOS

proc valueRecord(f: Face; at, fmt: int; g: var HbGlyph): bool =
  ## Applies a value record (horizontal). Returns whether it changed anything.
  var p = at
  if (fmt and 1) != 0:
    let v = f.i16(p)
    g.xOffset += int32(v)
    result = result or v != 0
    p += 2
  if (fmt and 2) != 0:
    let v = f.i16(p)
    g.yOffset += int32(v)
    result = result or v != 0
    p += 2
  if (fmt and 4) != 0:
    let v = f.i16(p)
    g.xAdvance += int32(v)
    result = result or v != 0
    p += 2

proc valueSize(fmt: int): int =
  var n = 0
  for bit in 0 ..< 8:
    if (fmt and (1 shl bit)) != 0: inc n
  n * 2

proc anchorAt(f: Face; at: int): (int, int) = (f.i16(at + 2), f.i16(at + 4))

proc markApply(c: var Ctx; markArray, markIndex, glyphIndexInAnchors, classCount: int;
               anchorsRowAt: proc(row, cls: int): int; basePos: int): bool =
  let f = c.f
  let rec = markArray + 2 + markIndex * 4
  let markClass = f.u16(rec)
  if markClass >= classCount: return false
  let baseAnchor = anchorsRowAt(glyphIndexInAnchors, markClass)
  if baseAnchor == 0: return false
  let (mx, my) = f.anchorAt(markArray + f.u16(rec + 2))
  let (bx, by) = f.anchorAt(baseAnchor)
  c.buf[c.idx].xOffset = int32(bx - mx)
  c.buf[c.idx].yOffset = int32(by - my)
  c.buf[c.idx].attachType = attachMark
  c.buf[c.idx].attachChain = basePos - c.idx
  c.hasAttachment = true
  inc c.idx
  true

proc acceptBase(c: Ctx; i: int): bool =
  ## HarfBuzz `MarkBasePosFormat1::accept`: only the first glyph of a multiple substitution.
  let g = c.buf[i]
  not g.multiplied or g.ligComp == 0 or i == 0 or c.buf[i - 1].isMark or not c.buf[i - 1].multiplied or
    g.ligId != c.buf[i - 1].ligId or g.ligComp != c.buf[i - 1].ligComp + 1

proc findBase(c: Ctx; checkAccept: bool; baseCov: int): int =
  var j = c.idx
  while j > 0:
    let r = c.matchGlyph(j - 1, lfIgnoreMarks, nil, 0)
    if r == mrMatch:
      if checkAccept and not c.acceptBase(j - 1) and c.f.coverageIndex(baseCov, c.buf[j - 1].gid) < 0:
        discard
      else:
        return j - 1
    dec j
  -1

proc reverseCursiveMinorOffset(c: var Ctx; i, newParent: int; depth = 0) =
  let chain = c.buf[i].attachChain
  let typ = c.buf[i].attachType
  if chain == 0 or (typ and attachCursive) == 0 or depth > MaxNesting: return
  c.buf[i].attachChain = 0
  let j = i + chain
  if j == newParent or j < 0 or j >= c.buf.len: return
  c.reverseCursiveMinorOffset(j, newParent, depth + 1)
  c.buf[j].yOffset = -c.buf[i].yOffset
  c.buf[j].attachChain = -chain
  c.buf[j].attachType = typ

proc applyGpos(c: var Ctx; kind, sub: int): bool =
  let f = c.f
  let g = c.buf[c.idx].gid
  let fmt = f.u16(sub)
  case kind
  of 1:
    let ci = f.coverageIndex(sub + f.u16(sub + 2), g)
    if ci < 0: return false
    let vf = f.u16(sub + 4)
    if fmt == 1: discard f.valueRecord(sub + 6, vf, c.buf[c.idx])
    elif fmt == 2:
      if ci >= f.u16(sub + 6): return false
      discard f.valueRecord(sub + 8 + ci * valueSize(vf), vf, c.buf[c.idx])
    else: return false
    inc c.idx
    true
  of 2:
    let ci = f.coverageIndex(sub + f.u16(sub + 2), g)
    if ci < 0: return false
    var pos = c.skipNext(c.idx, c.lookupProps, nil, 0, c.lookupMask)
    if pos < 0: return false
    let g2 = c.buf[pos].gid
    let vf1 = f.u16(sub + 4)
    let vf2 = f.u16(sub + 6)
    let (s1, s2) = (valueSize(vf1), valueSize(vf2))
    if fmt == 1:
      if ci >= f.u16(sub + 8): return false
      let ps = sub + f.u16(sub + 10 + ci * 2)
      let n = f.u16(ps)
      let recSize = 2 + s1 + s2
      var lo = 0
      var hi = n - 1
      while lo <= hi:
        let mid = (lo + hi) div 2
        let r = ps + 2 + mid * recSize
        let sg = f.u16(r)
        if sg == g2:
          discard f.valueRecord(r + 2, vf1, c.buf[c.idx])
          discard f.valueRecord(r + 2 + s1, vf2, c.buf[pos])
          if s2 != 0: inc pos
          c.idx = pos
          return true
        if sg < g2: lo = mid + 1 else: hi = mid - 1
      return false
    elif fmt == 2:
      let c1 = f.classOf(sub + f.u16(sub + 8), g)
      let c2 = f.classOf(sub + f.u16(sub + 10), g2)
      let n1 = f.u16(sub + 12)
      let n2 = f.u16(sub + 14)
      if c1 >= n1 or c2 >= n2: return false
      let r = sub + 16 + (c1 * n2 + c2) * (s1 + s2)
      discard f.valueRecord(r, vf1, c.buf[c.idx])
      discard f.valueRecord(r + s1, vf2, c.buf[pos])
      if s2 != 0: inc pos
      c.idx = pos
      return true
    false
  of 3:
    if fmt != 1: return false
    let cov = sub + f.u16(sub + 2)
    let ci = f.coverageIndex(cov, g)
    if ci < 0 or ci >= f.u16(sub + 4): return false
    let entry = f.u16(sub + 6 + ci * 4)
    if entry == 0: return false
    let i = c.skipPrev(c.idx, c.lookupProps, nil, 0)
    if i < 0: return false
    let pi = f.coverageIndex(cov, c.buf[i].gid)
    if pi < 0 or pi >= f.u16(sub + 4): return false
    let exitOff = f.u16(sub + 6 + pi * 4 + 2)
    if exitOff == 0: return false
    let j = c.idx
    let (exitX, exitY) = f.anchorAt(sub + exitOff)
    let (entryX, entryY) = f.anchorAt(sub + entry)
    if not c.rtl:
      c.buf[i].xAdvance = int32(exitX) + c.buf[i].xOffset
      let d = int32(entryX) + c.buf[j].xOffset
      c.buf[j].xAdvance -= d
      c.buf[j].xOffset -= d
    else:
      let d = int32(exitX) + c.buf[i].xOffset
      c.buf[i].xAdvance -= d
      c.buf[i].xOffset -= d
      c.buf[j].xAdvance = int32(entryX) + c.buf[j].xOffset
    var child = i
    var parent = j
    var yOff = entryY - exitY
    if (c.lookupProps and lfRightToLeft) == 0:
      swap(child, parent)
      yOff = -yOff
    c.reverseCursiveMinorOffset(child, parent)
    c.buf[child].attachType = attachCursive
    c.buf[child].attachChain = parent - child
    c.hasAttachment = true
    c.buf[child].yOffset = int32(yOff)
    if c.buf[parent].attachChain == -c.buf[child].attachChain:
      c.buf[parent].attachChain = 0
      c.buf[parent].yOffset = 0
    inc c.idx
    true
  of 4:
    let mi = f.coverageIndex(sub + f.u16(sub + 2), g)
    if mi < 0: return false
    let baseCov = sub + f.u16(sub + 4)
    let b = c.findBase(true, baseCov)
    if b < 0: return false
    let bi = f.coverageIndex(baseCov, c.buf[b].gid)
    if bi < 0: return false
    let classCount = f.u16(sub + 6)
    let baseArray = sub + f.u16(sub + 10)
    let rowAt = proc(row, cls: int): int =
      let o = f.u16(baseArray + 2 + (row * classCount + cls) * 2)
      if o == 0: 0 else: baseArray + o
    c.markApply(sub + f.u16(sub + 8), mi, bi, classCount, rowAt, b)
  of 5:
    let mi = f.coverageIndex(sub + f.u16(sub + 2), g)
    if mi < 0: return false
    let b = c.findBase(false, 0)
    if b < 0: return false
    let li = f.coverageIndex(sub + f.u16(sub + 4), c.buf[b].gid)
    if li < 0: return false
    let classCount = f.u16(sub + 6)
    let ligArray = sub + f.u16(sub + 10)
    let attach = ligArray + f.u16(ligArray + 2 + li * 2)
    let compCount = f.u16(attach)
    if compCount == 0: return false
    let ligIdB = c.buf[b].ligId
    let markId = c.buf[c.idx].ligId
    let markComp = c.buf[c.idx].ligComp
    let compIndex = if ligIdB != 0 and ligIdB == markId and markComp > 0: min(compCount, markComp) - 1
                    else: compCount - 1
    let rowAt = proc(row, cls: int): int =
      let o = f.u16(attach + 2 + (row * classCount + cls) * 2)
      if o == 0: 0 else: attach + o
    c.markApply(sub + f.u16(sub + 8), mi, compIndex, classCount, rowAt, b)
  of 6:
    let mi = f.coverageIndex(sub + f.u16(sub + 2), g)
    if mi < 0: return false
    let j = c.skipPrev(c.idx, c.lookupProps and not lfIgnoreFlags, nil, 0)
    if j < 0 or not c.buf[j].isMark: return false
    let id1 = c.buf[c.idx].ligId
    let id2 = c.buf[j].ligId
    let comp1 = c.buf[c.idx].ligComp
    let comp2 = c.buf[j].ligComp
    let good = if id1 == id2: id1 == 0 or comp1 == comp2
               else: (id1 > 0 and comp1 == 0) or (id2 > 0 and comp2 == 0)
    if not good: return false
    let m2 = f.coverageIndex(sub + f.u16(sub + 4), c.buf[j].gid)
    if m2 < 0: return false
    let classCount = f.u16(sub + 6)
    let mark2Array = sub + f.u16(sub + 10)
    let rowAt = proc(row, cls: int): int =
      let o = f.u16(mark2Array + 2 + (row * classCount + cls) * 2)
      if o == 0: 0 else: mark2Array + o
    c.markApply(sub + f.u16(sub + 8), mi, m2, classCount, rowAt, j)
  of 7: c.contextual(sub, false)
  of 8: c.contextual(sub, true)
  else: false

proc applyLookupAt(c: var Ctx; lookupIndex: int): bool =
  ## Applies one lookup's subtables at `c.idx` (first that applies wins).
  let info = c.lookupInfo(lookupIndex)
  c.lookupProps = info.props
  for s in info.subs:
    var applied = false
    try:
      applied = if c.gsub: c.applyGsub(info.kind, s) else: c.applyGpos(info.kind, s)
    except FontError: applied = false
    if applied: return true
  false

proc applyLookupString(c: var Ctx; lookupIndex, value: int; mask: uint32; autoZwj: bool) =
  let info = c.lookupInfo(lookupIndex)
  if info.subs.len == 0: return
  c.featureValue = value
  c.lookupMask = mask
  c.autoZwj = autoZwj
  if c.gsub and info.kind == 8:
    var i = c.buf.len - 1
    while i >= 0:
      c.idx = i
      c.lookupProps = info.props
      if (c.buf[i].mask and mask) != 0 and c.checkGlyphProperty(c.buf[i], info.props):
        discard c.applyLookupAt(lookupIndex)
      dec i
    return
  c.idx = 0
  var guard = 0
  while c.idx < c.buf.len:
    inc guard
    if guard > 1_000_000: break
    c.lookupProps = info.props
    let before = c.idx
    let lenBefore = c.buf.len
    if (c.buf[c.idx].mask and mask) != 0 and c.checkGlyphProperty(c.buf[c.idx], info.props) and
       c.applyLookupAt(lookupIndex):
      if c.idx <= before and c.buf.len >= lenBefore: c.idx = before + 1   # always make progress
    else:
      inc c.idx

# ---------------------------------------------------------------- public entry points

proc substituteStart*(f: Face; buf: var seq[HbGlyph]) =
  ## GDEF glyph classes on every glyph (or a guess from the Unicode category without GDEF).
  let d = f.data
  for g in buf.mitems:
    g.ligProps = 0
    if d.hasGlyphClasses: g.props = f.glyphProps(g.gid)
    else: g.props = if g.gc == 6'u8 and (g.uflags and ufIgnorable) == 0: gpMark else: gpBase

type Pause* = proc(buf: var seq[HbGlyph]) {.closure.}

proc substitute*(f: Face; buf: var seq[HbGlyph]; stages: seq[seq[StageLookup]]; pauses: seq[Pause]; rtl: bool) =
  ## Applies GSUB lookups (each stage's lookups in order, then that stage's pause) to the buffer.
  let d = f.data
  var c = Ctx(f: f, d: d, gsub: true, buf: move buf, rtl: rtl, serial: 1)
  for si, stage in stages:
    if d.gsub.ok:
      for (li, v, m, z) in stage: c.applyLookupString(li, v, m, z)
    if si < pauses.len and not pauses[si].isNil: pauses[si](c.buf)
  buf = move c.buf

proc position*(f: Face; buf: var seq[HbGlyph]; lookups: seq[StageLookup]; rtl: bool): bool =
  ## Applies GPOS lookups. Returns whether any attachment (mark/cursive) was recorded.
  let d = f.data
  if not d.gpos.ok: return false
  for g in buf.mitems:
    g.attachChain = 0
    g.attachType = 0
  var c = Ctx(f: f, d: d, gsub: false, buf: move buf, rtl: rtl)
  for (li, v, m, z) in lookups: c.applyLookupString(li, v, m, z)
  result = c.hasAttachment
  buf = move c.buf

proc propagate(buf: var seq[HbGlyph]; i: int; rtl: bool; nesting: int) =
  let chain = buf[i].attachChain
  let typ = buf[i].attachType
  if chain == 0: return
  buf[i].attachChain = 0
  let j = i + chain
  if j < 0 or j >= buf.len or nesting <= 0: return
  propagate(buf, j, rtl, nesting - 1)
  if (typ and attachCursive) != 0:
    buf[i].yOffset += buf[j].yOffset
  else:
    buf[i].xOffset += buf[j].xOffset
    buf[i].yOffset += buf[j].yOffset
    if j < i:
      if not rtl:
        for k in j ..< i:
          buf[i].xOffset -= buf[k].xAdvance
          buf[i].yOffset -= buf[k].yAdvance
      else:
        for k in j + 1 .. i:
          buf[i].xOffset += buf[k].xAdvance
          buf[i].yOffset += buf[k].yAdvance

proc finishOffsets*(buf: var seq[HbGlyph]; rtl: bool) =
  ## Resolves attachment chains (HarfBuzz `position_finish_offsets`).
  for i in 0 ..< buf.len: propagate(buf, i, rtl, MaxNesting)

proc kernPair*(f: Face; left, right: int): int =
  ## The legacy `kern` table (format 0, horizontal) adjustment for a glyph pair, in font units.
  let t = f.table("kern")
  if t.isNone: return 0
  try:
    let b = t.get[0]
    if f.u16(b) != 0: return 0
    var p = b + 4
    for _ in 0 ..< f.u16(b + 2):
      let len = f.u16(p + 2)
      let coverage = f.u16(p + 4)
      if (coverage shr 8) == 0 and (coverage and 1) != 0 and (coverage and 4) == 0:
        let n = f.u16(p + 6)
        var lo = 0
        var hi = n - 1
        let key = (left shl 16) or right
        while lo <= hi:
          let mid = (lo + hi) div 2
          let r = p + 14 + mid * 6
          let k = (f.u16(r) shl 16) or f.u16(r + 2)
          if k == key: return f.i16(r + 4)
          if k < key: lo = mid + 1 else: hi = mid - 1
      if len <= 0: break
      p += len
  except FontError: discard
  0

proc hasKern*(f: Face): bool = f.hasTable("kern")
