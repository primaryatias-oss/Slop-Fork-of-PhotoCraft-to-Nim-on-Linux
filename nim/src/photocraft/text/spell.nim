## Spelling checker for Edit › Check Spelling (port of `photocraft_text::spell`).
##
## The dictionary is SCOWL size 50 (American English, ~72k words; see
## `assets/dict/LICENSE-SCOWL.txt`), gzip-compressed into the binary and decoded on first use.
## Words are grouped by SCOWL level (10 = most common … 50), which ranks suggestions.
##
## Case rules follow common spell checkers: a lowercase dictionary word may be written
## capitalised or in all caps; a capitalised entry (proper noun) must keep its capital. Trailing
## possessive `'s` is ignored, typographic apostrophes count as `'`, and tokens containing digits
## are skipped.

import std/[tables, sets, strutils, unicode, algorithm, locks, sequtils]
import ../zlib
import uprops

const WordsGz = staticRead("../../../../assets/dict/en_US-scowl-50.txt.gz")

type
  Dictionary* = ref object
    ## A word list with frequency levels.
    words: Table[string, uint8]              ## word (as listed) → SCOWL level
    byLower: Table[string, seq[string]]      ## lowercase form → listed forms
    byLen: seq[seq[(string, seq[Rune], uint8)]]  ## lowercase words grouped by char length

  Misspelling* = object
    ## A misspelled word in a string (byte range in the checked text).
    start*, `end`*: int
    word*: string

proc isAlphabetic(r: Rune): bool =
  let gc = generalCategory(int(r))
  gc.isLetter or gc == gcNl
proc isNumeric(r: Rune): bool = generalCategory(int(r)) in {gcNd, gcNl, gcNo}
proc isUpperR(r: Rune): bool = generalCategory(int(r)) == gcLu or (r != r.toLower and r.toUpper == r)
proc lowerStr(s: string): string =
  for r in s.runes: result.add r.toLower
proc upperStr(s: string): string =
  for r in s.runes: result.add r.toUpper

proc fromList*(list: string): Dictionary =
  ## Builds a dictionary from one word per line; `#NN` lines set the level of the words after.
  result = Dictionary()
  var level = 50'u8
  for line in list.splitLines:
    let w = line.strip(chars = {' ', '\t', '\r', '\n', '\v', '\f'})
    if w.startsWith('#'):
      try: level = uint8(parseInt(w[1 .. ^1]))
      except ValueError: discard
      continue
    if w.len == 0 or result.words.hasKey(w): continue
    result.words[w] = level
    result.byLower.mgetOrPut(lowerStr(w), @[]).add w
  for lw, forms in result.byLower:
    let n = lw.runeLen
    if result.byLen.len <= n: result.byLen.setLen(n + 1)
    var lvl = 50'u8
    var any = false
    for f in forms:
      if result.words.hasKey(f):
        let v = result.words[f]
        if not any or v < lvl: lvl = v
        any = true
    result.byLen[n].add (lw, toSeq(lw.runes), lvl)
  # Keys are unique, so ordering by the word orders the whole tuple.
  for v in result.byLen.mitems: v.sort(proc(x, y: (string, seq[Rune], uint8)): int = cmp(x[0], y[0]))

var englishDict: Dictionary
var englishLock: Lock
initLock(englishLock)

proc english*(): Dictionary =
  ## The bundled English dictionary (decoded once).
  withLock englishLock:
    if englishDict.isNil:
      var s = ""
      try: s = toStr(gzipDecompress(toBytes(WordsGz)))
      except CatchableError: discard
      englishDict = fromList(s)
    result = englishDict

proc len*(d: Dictionary): int = d.words.len
proc isEmpty*(d: Dictionary): bool = d.words.len == 0

proc normalize(w: string): string =
  ## Typographic apostrophes → `'`.
  w.multiReplace(("’", "'"), ("‘", "'"), ("ʼ", "'"))

proc capitalize(w: string): string =
  if w.len == 0: return ""
  let r = w.runeAt(0)
  r.toUpper.toUTF8 & w[r.size .. ^1]

proc check*(d: Dictionary; word: string; user: HashSet[string]): bool =
  ## Whether `word` is spelled correctly (also accepting `user` words, compared without case).
  var w = normalize(word)
  if w.endsWith("'s") and w.len > 2: w = w[0 ..< w.len - 2]
  w = w.strip(chars = {'\''})
  var hasAlpha = false
  for r in w.runes:
    if r.isNumeric: return true
    if r.isAlphabetic: hasAlpha = true
  if w.len == 0 or not hasAlpha: return true
  let lower = lowerStr(w)
  if lower in user or d.words.hasKey(w): return true
  if not d.byLower.hasKey(lower): return false
  var allCaps = true
  for r in w.runes:
    if r.isAlphabetic and not r.isUpperR: allCaps = false
  var firstCap = false
  var i = 0
  for r in w.runes:
    if i == 0: firstCap = r.isUpperR
    elif r.isUpperR: firstCap = false
    inc i
  for f in d.byLower[lower]:
    var fLower = true
    for r in f.runes:
      if r.isUpperR: fLower = false
    # "the" → "The"/"THE"; "Paris" → "PARIS"; "NASA" stays as listed (exact match above).
    if allCaps or (fLower and firstCap) or (firstCap and capitalize(f) == w): return true
  false

type Rows = object
  prev2, prev, cur: seq[int]

proc distance(a, b: openArray[Rune]; max: int; rows: var Rows): int =
  ## Damerau–Levenshtein (optimal string alignment) distance, or -1 when above `max`.
  let (n, m) = (a.len, b.len)
  if abs(n - m) > max: return -1
  rows.prev2.setLen(0)
  rows.prev2.setLen(m + 1)
  rows.prev.setLen(m + 1)
  for j in 0 .. m: rows.prev[j] = j
  rows.cur.setLen(0)
  rows.cur.setLen(m + 1)
  for i in 1 .. n:
    rows.cur[0] = i
    var rowMin = rows.cur[0]
    for j in 1 .. m:
      let cost = if a[i - 1] != b[j - 1]: 1 else: 0
      var v = min(min(rows.prev[j] + 1, rows.cur[j - 1] + 1), rows.prev[j - 1] + cost)
      if i > 1 and j > 1 and a[i - 1] == b[j - 2] and a[i - 2] == b[j - 1]:
        v = min(v, rows.prev2[j - 2] + 1)
      rows.cur[j] = v
      rowMin = min(rowMin, v)
    if rowMin > max: return -1
    swap(rows.prev2, rows.prev)
    swap(rows.prev, rows.cur)
  if rows.prev[m] <= max: rows.prev[m] else: -1

proc cmpRunes(a, b: seq[Rune]): int =
  for i in 0 ..< min(a.len, b.len):
    if a[i] != b[i]: return cmp(int(a[i]), int(b[i]))
  cmp(a.len, b.len)

proc suggest*(d: Dictionary; word: string; max: int): seq[string] =
  ## Up to `max` suggestions for a misspelled word, nearest first (then most common), with the
  ## word's case pattern applied.
  let w = normalize(word)
  let lower = lowerStr(w)
  let lc = toSeq(lower.runes)
  let n = lc.len
  let maxD = if n <= 4: 1 else: 2
  # Same letters in another order (transposed typing) ranks first among equal distances.
  var sorted = lc
  sorted.sort(proc(x, y: Rune): int = cmp(int(x), int(y)))
  var found: seq[(int, bool, uint8, string)]
  var rows: Rows
  for ln in max(n - maxD, 0) .. n + maxD:
    if ln >= d.byLen.len: continue
    for (cand, cc, lvl) in d.byLen[ln]:
      let dist = distance(lc, cc, maxD, rows)
      if dist >= 0:
        var v = cc
        v.sort(proc(x, y: Rune): int = cmp(int(x), int(y)))
        found.add (dist, cmpRunes(v, sorted) != 0, lvl, cand)
  found.sort()
  var alphaCount = 0
  var allCaps = true
  for r in w.runes:
    if r.isAlphabetic:
      inc alphaCount
      if not r.isUpperR: allCaps = false
  allCaps = allCaps and alphaCount > 1
  let firstCap = w.len > 0 and w.runeAt(0).isUpperR
  for (_, _, _, cand) in found:
    # Common (lowercase) forms before proper nouns: "brwon" → "brown", then "Brown".
    var forms = d.byLower.getOrDefault(cand)
    proc hasUpper(f: string): bool =
      for r in f.runes:
        if r.isUpperR: return true
    forms.sort(proc(x, y: string): int = cmp(hasUpper(x), hasUpper(y)))
    for form in forms:
      let s = if allCaps: upperStr(form) elif firstCap: capitalize(form) else: form
      if s notin result and s != w: result.add s
    if result.len >= max: break
  if result.len > max: result.setLen(max)

proc words*(text: string): seq[(int, int)] =
  ## Byte ranges of the words in `text`: letters/digits with inner apostrophes (hyphens split).
  var chars: seq[(int, Rune)]
  var b = 0
  while b < text.len:
    let n = max(1, runeLenAt(text, b))
    chars.add (b, runeAt(text, b))
    b += n
  var start = -1
  for i, (b, c) in chars:
    let apos = int(c) in [ord('\''), 0x2019, 0x02BC]
    let innerApos = apos and start >= 0 and i + 1 < chars.len and chars[i + 1][1].isAlphabetic
    if c.isAlphabetic or c.isNumeric or innerApos:
      if start < 0: start = b
    elif start >= 0:
      result.add (start, b)
      start = -1
  if start >= 0: result.add (start, text.len)

proc misspellings*(d: Dictionary; text: string; user: HashSet[string]): seq[Misspelling] =
  ## Misspelled words of `text` in order.
  for (a, b) in words(text):
    if not d.check(text[a ..< b], user): result.add Misspelling(start: a, `end`: b, word: text[a ..< b])
