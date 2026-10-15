## UI fallback fonts for the non-CJK scripts the bundled Inter / JetBrains Mono don't draw
## (Thai, Arabic, Hebrew, the Indic scripts, ...): which characters belong to which script, and
## which installed font files to try for it (port of `photocraft_text::scripts`).
##
## Candidates come from the file names in the platform font directories (the same directories
## `fonts.FontDb` scans), listed once per process and only when first asked for. Noto fonts are
## matched by their family-derived file names (`NotoSansThai-Regular.ttf`,
## `NotoSansThai[wght].ttf`), platform fonts by their known file names. A file name is only a
## hint: callers check that a face really maps the character (`coveringFace`). CJK has its own,
## locale-ordered lists (`cjk`).

import std/[options, os, strutils, unicode, algorithm, locks]
import ../font/sfnt
import fonts

type FallbackScript* = enum
  ## A script that needs a UI fallback font.
  fsThai, fsLao, fsKhmer, fsMyanmar, fsTibetan, fsArabic, fsHebrew, fsArmenian, fsGeorgian,
  fsEthiopic, fsDevanagari, fsBengali, fsGurmukhi, fsGujarati, fsOriya, fsTamil, fsTelugu,
  fsKannada, fsMalayalam, fsSinhala

const FileNames: array[FallbackScript, seq[string]] = [
  # Lowercase, without extension, spaces, underscores or style suffix; Noto first, then Windows,
  # macOS and other Linux families.
  @["notosansthai", "notosansthailooped", "notosansthaiui", "notoserifthai", "leelawui", "leelawad", "tahoma",
    "thonburi", "sukhumvitset", "ayuthaya", "loma", "garuda", "waree", "kinnari", "norasi"],
  @["notosanslao", "notosanslaoui", "notoseriflao", "leelawui", "laosangammn", "laomn", "phetsarathot", "saysetthaot"],
  @["notosanskhmer", "notosanskhmerui", "notoserifkhmer", "leelawui", "daunpenh", "khmersangammn", "khmermn", "khmeros",
    "khmerossys"],
  @["notosansmyanmar", "notosansmyanmarui", "notoserifmyanmar", "mmrtext", "myanmarsangammn", "myanmarmn", "padauk"],
  @["notoseriftibetan", "himalaya", "kailasa", "jomolhari", "tibetanmachineuni"],
  @["notosansarabic", "notosansarabicui", "notonaskharabic", "notonaskharabicui", "notokufiarabic", "segoeui",
    "tahoma", "geezapro", "sfarabic", "dejavusans", "kacstone", "kacstbook", "amiri", "arial"],
  @["notosanshebrew", "notoserifhebrew", "segoeui", "arialhb", "sfhebrew", "dejavusans", "arial", "freesans", "culmus"],
  @["notosansarmenian", "notoserifarmenian", "segoeui", "sylfaen", "mshtakan", "dejavusans", "freesans"],
  @["notosansgeorgian", "notoserifgeorgian", "segoeui", "sylfaen", "dejavusans", "freesans"],
  @["notosansethiopic", "notoserifethiopic", "ebrima", "kefa", "abyssinicasil"],
  @["notosansdevanagari", "notosansdevanagariui", "notoserifdevanagari", "nirmala", "mangal", "kohinoor",
    "kohinoordevanagari", "itfdevanagari", "devanagarimt", "devanagarisangammn", "lohitdevanagari",
    "lohit-devanagari", "gargi", "freesans"],
  @["notosansbengali", "notosansbengaliui", "notoserifbengali", "nirmala", "vrinda", "kohinoorbangla",
    "banglasangammn", "banglamn", "lohitbengali", "lohit-bengali", "lohit-assamese"],
  @["notosansgurmukhi", "notosansgurmukhiui", "notoserifgurmukhi", "nirmala", "raavi", "gurmukhisangammn",
    "gurmukhimn", "lohit-gurmukhi", "lohit-punjabi"],
  @["notosansgujarati", "notosansgujaratiui", "notoserifgujarati", "nirmala", "shruti", "kohinoorgujarati",
    "gujaratisangammn", "gujaratimt", "lohit-gujarati"],
  @["notosansoriya", "notosansoriyaui", "notoseriforiya", "nirmala", "kalinga", "oriyasangammn", "oriyamn",
    "lohit-odia", "lohit-oriya"],
  @["notosanstamil", "notosanstamilui", "notoseriftamil", "nirmala", "latha", "tamilsangammn", "tamilmn",
    "lohit-tamil"],
  @["notosanstelugu", "notosansteluguui", "notoseriftelugu", "nirmala", "gautami", "kohinoortelugu",
    "telugusangammn", "telugumn", "lohit-telugu"],
  @["notosanskannada", "notosanskannadaui", "notoserifkannada", "nirmala", "tunga", "kannadasangammn",
    "kannadamn", "lohit-kannada"],
  @["notosansmalayalam", "notosansmalayalamui", "notoserifmalayalam", "nirmala", "kartika", "malayalamsangammn",
    "malayalammn", "lohit-malayalam"],
  @["notosanssinhala", "notosanssinhalaui", "notoserifsinhala", "nirmala", "iskpota", "sinhalasangammn",
    "sinhalamn", "lklug"]]

proc classify*(c: int): Option[FallbackScript] =
  ## The script of a character that needs a non-CJK fallback font.
  case c
  of 0x0E00 .. 0x0E7F: some(fsThai)
  of 0x0E80 .. 0x0EFF: some(fsLao)
  of 0x1780 .. 0x17FF, 0x19E0 .. 0x19FF: some(fsKhmer)
  of 0x1000 .. 0x109F, 0xA9E0 .. 0xA9FF, 0xAA60 .. 0xAA7F: some(fsMyanmar)
  of 0x0F00 .. 0x0FFF: some(fsTibetan)
  of 0x0600 .. 0x06FF, 0x0750 .. 0x077F, 0x0870 .. 0x08FF, 0xFB50 .. 0xFDFF, 0xFE70 .. 0xFEFF: some(fsArabic)
  of 0x0590 .. 0x05FF, 0xFB1D .. 0xFB4F: some(fsHebrew)
  of 0x0530 .. 0x058F, 0xFB13 .. 0xFB17: some(fsArmenian)
  of 0x10A0 .. 0x10FF, 0x1C90 .. 0x1CBF, 0x2D00 .. 0x2D2F: some(fsGeorgian)
  of 0x1200 .. 0x139F, 0x2D80 .. 0x2DDF, 0xAB00 .. 0xAB2F: some(fsEthiopic)
  of 0x0900 .. 0x097F, 0xA8E0 .. 0xA8FF: some(fsDevanagari)
  of 0x0980 .. 0x09FF: some(fsBengali)
  of 0x0A00 .. 0x0A7F: some(fsGurmukhi)
  of 0x0A80 .. 0x0AFF: some(fsGujarati)
  of 0x0B00 .. 0x0B7F: some(fsOriya)
  of 0x0B80 .. 0x0BFF: some(fsTamil)
  of 0x0C00 .. 0x0C7F: some(fsTelugu)
  of 0x0C80 .. 0x0CFF: some(fsKannada)
  of 0x0D00 .. 0x0D7F: some(fsMalayalam)
  of 0x0D80 .. 0x0DFF: some(fsSinhala)
  else: none(FallbackScript)

proc lowerStr(s: string): string =
  for r in s.runes: result.add r.toLower

proc fileKey(path: string): Option[(string, string, int)] =
  ## `(name, stem, style rank)` of a font file: `FileNames` form, the file stem, and 0 for the
  ## regular face, 1 for a variable font, 2 for other weights, 3 for bold, italic and condensed
  ## faces. Non-font files give none.
  let (_, stem, ext) = splitFile(path)
  if ext.len < 2 or ext[1 .. ^1].toLowerAscii notin ["ttf", "otf", "ttc", "otc"] or stem.len == 0: return
  proc squash(s: string): string = lowerStr(s.multiReplace((" ", ""), ("_", "")))
  proc style(s0: string): int =
    let s = s0.toLowerAscii
    if s.len == 0 or s == "-regular": 0
    elif s.startsWith('['): 1
    else:
      for w in ["bold", "italic", "oblique", "condensed", "narrow", "black", "heavy", "thin", "light"]:
        if w in s: return 3
      2
  var cut = stem.len
  for i, ch in stem:
    if ch in {'-', '['}:
      cut = i
      break
  some((squash(stem[0 ..< cut]), stem, style(stem[cut .. ^1])))

proc cmpPath*(a, b: string): int =
  ## Rust `Path` ordering: component by component.
  let (x, y) = (a.split('/'), b.split('/'))
  var (i, j) = (0, 0)
  var xs, ys: seq[string]
  for c in x:
    if c.len > 0 and c != ".": xs.add c
  for c in y:
    if c.len > 0 and c != ".": ys.add c
  let ra = a.startsWith('/')
  let rb = b.startsWith('/')
  if ra != rb: return (if ra: -1 else: 1)
  while i < xs.len and j < ys.len:
    let c = cmp(xs[i], ys[j])
    if c != 0: return c
    inc i
    inc j
  cmp(xs.len, ys.len)

proc candidates*(script: FallbackScript; files: openArray[string]): seq[string] =
  ## Font files among `files` that may cover `script`, most preferred first (at most 16).
  const MaxCandidates = 16
  let names = FileNames[script]
  var ranked: seq[(int, int, string)]
  for p in files:
    let k = fileKey(p)
    if k.isNone: continue
    let (base, stem, style) = k.get
    # A name with a hyphen ("lohit-tamil") matches the whole stem.
    let full = lowerStr(stem).multiReplace((" ", ""), ("_", ""))
    var rank = -1
    for i, n in names:
      if n == base or n == full:
        rank = i
        break
    if rank < 0: continue
    ranked.add (rank, (if names[rank] == full: 0 else: style), p)
  ranked.sort(proc(a, b: (int, int, string)): int =
    if a[0] != b[0]: cmp(a[0], b[0]) elif a[1] != b[1]: cmp(a[1], b[1]) else: cmpPath(a[2], b[2]))
  for i in 0 ..< min(ranked.len, MaxCandidates): result.add ranked[i][2]

var sysFiles: seq[string]
var sysListed = false
var sysLock: Lock
initLock(sysLock)

proc systemFontFiles*(): seq[string] =
  ## Every font file in the platform font directories, listed on first use and cached for the
  ## process (names only; nothing is opened).
  withLock sysLock:
    if not sysListed:
      for d in systemFontDirs(): collectFontFiles(d, 0, sysFiles)
      sysFiles.sort(cmpPath)
      var k = 0
      for i in 0 ..< sysFiles.len:
        if i == 0 or sysFiles[i] != sysFiles[k - 1]:
          sysFiles[k] = sysFiles[i]
          inc k
      sysFiles.setLen(k)
      sysListed = true
    result = sysFiles

proc systemCandidates*(script: FallbackScript): seq[string] =
  ## Installed font files that may cover `script`, most preferred first.
  candidates(script, systemFontFiles())

proc coveringFace*(bytes: seq[byte]; c: int): Option[int] =
  ## Index of the first face in `bytes` (a font or a collection) whose character map has `c`;
  ## none for fonts without it and for data that doesn't parse.
  try:
    let faces = loadFaces(bytes)
    for i, f in faces:
      if f.glyphIndex(c) != 0: return some(i)
  except CatchableError: discard
  none(int)
