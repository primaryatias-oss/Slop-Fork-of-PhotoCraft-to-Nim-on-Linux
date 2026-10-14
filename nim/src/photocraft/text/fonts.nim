## Font database (port of `photocraft_text::fonts` and the parts of fontique it relies on):
## bundled fonts (always available), optional system fonts from a directory scan (no fontconfig),
## user font data, TrueType collections, CSS-style face matching with synthesis (faux bold /
## oblique), and PostScript-name lookup for PSD import.

import std/[tables, options, strutils, algorithm, os, unicode, math, sequtils]
import ../font/[sfnt, otl]
import ../doc
import cjk, craft_fonts, served

const
  DefaultFamily* = "Inter"           ## family used when a style names no family or an unknown one
  MonoFamily* = "JetBrains Mono"     ## bundled monospace family
  MaxFontFileBytes = 256 * 1024 * 1024

const bundledFontData = [
  ("Inter-Regular.ttf", staticRead("../../../../assets/fonts/Inter-Regular.ttf")),
  ("Inter-Medium.ttf", staticRead("../../../../assets/fonts/Inter-Medium.ttf")),
  ("Inter-SemiBold.ttf", staticRead("../../../../assets/fonts/Inter-SemiBold.ttf")),
  ("JetBrainsMono-Regular.ttf", staticRead("../../../../assets/fonts/JetBrainsMono-Regular.ttf"))]

proc bundledFont*(name: string): seq[byte] =
  ## One of the fonts shipped with PhotoCraft (OFL; licences in `assets/fonts`), empty if unknown.
  for (n, d) in bundledFontData:
    if n == name: return cast[seq[byte]](d)

const FallbackCandidates = ["Noto Sans", "Segoe UI", "DejaVu Sans", "Geeza Pro", "Arial Hebrew",
  "Noto Sans Arabic", "Noto Sans Hebrew"]
const ThaiFamilies* = ["Leelawadee UI", "Leelawadee", "Tahoma", "Thonburi", "Sukhumvit Set", "Noto Sans Thai",
  "Noto Sans Thai Looped", "Noto Sans Thai UI"]
const KhmerFamilies* = ["Khmer Sangam MN", "Khmer MN", "Khmer UI", "Noto Sans Khmer"]
const LaoFamilies* = ["Lao Sangam MN", "Lao MN", "Lao UI", "Noto Sans Lao", "Noto Sans Lao Looped"]
const MyanmarFamilies* = ["Myanmar Sangam MN", "Myanmar MN", "Myanmar Text", "Noto Sans Myanmar"]
const FallbackLast = ["Arial Unicode MS", "Apple Color Emoji", "Segoe UI Emoji", "Noto Color Emoji"]

proc fallbackCandidates*(order: array[4, CjkScript]): seq[string] =
  ## Every fallback family candidate, CJK ordered for `order`.
  result = @FallbackCandidates
  result.add ThaiFamilies
  result.add KhmerFamilies
  result.add LaoFamilies
  result.add MyanmarFamilies
  for s in order:
    if s == Japanese: result.add japaneseFamilies()
    result.add families(s)
  result.add FallbackLast

var fontGeneration: int
proc generation*(): int = fontGeneration
  ## Changes whenever font data is registered in any `FontDb` (font menus compare it).

type
  FontStyleKind* = enum fsNormal, fsItalic, fsOblique

  FaceRec* = ref object
    ## One face in the database. Its full data is read on first use.
    family*: string
    path*: string
    blob: FontBlob
    index*: int
    weight*: float32
    width*: float32           ## ratio, 1.0 normal
    style*: FontStyleKind
    obliqueAngle*: float32
    axes*: seq[Axis]
    meta: Face                ## names, metrics and attributes (possibly from a partial read)
    face: Face
    loadFailed: bool

  FamilyRec = object
    name: string
    faces: seq[FaceRec]

  FaceInfo* = object
    ## One face in the database (for font menus).
    family*: string
    style*: string              ## typographic subfamily (name 17) or legacy subfamily (name 2)
    postscriptName*: Option[string]
    weight*: float32
    italic*: bool
    axes*: seq[(string, float32, float32, float32)]   ## (tag, min, default, max)

  ResolvedFont* = object
    ## Result of a PostScript-name lookup.
    family*: string
    weight*: uint16
    italic*: bool
    exact*: bool   ## the exact face was found (false: a heuristic family guess)

  Synthesis* = object
    ## Suggested synthesis for a face and requested attributes (fontique `Synthesis`).
    vars*: seq[(string, float32)]
    embolden*: bool
    skew*: float32     ## degrees, 0 for none

  FontDb* = ref object
    families: seq[FamilyRec]
    nameIndex: Table[string, int]        ## lowercased name or alias → family
    generics: Table[string, seq[int]]
    systemLoaded: bool
    psCache: Table[string, Option[ResolvedFont]]
    fallbacks: seq[string]
    faceCache: Table[string, seq[FaceInfo]]
    faceAliases: Table[(string, int), string]
    nextAlias: int

proc isHiddenFamily*(name: string): bool = name.startsWith(".")
  ## A platform-private family (macOS UI faces start with '.'): never listed or picked by default.

proc nameKey(s: string): string = unicode.toLower(s)

# ---------------------------------------------------------------- reading faces

proc be32(b: openArray[byte]; at: int): int =
  (int(b[at]) shl 24) or (int(b[at + 1]) shl 16) or (int(b[at + 2]) shl 8) or int(b[at + 3])
proc be16(b: openArray[byte]; at: int): int = (int(b[at]) shl 8) or int(b[at + 1])

proc readAt(f: File; pos, len: int): seq[byte] =
  if len <= 0 or len > MaxFontFileBytes: return
  result = newSeq[byte](len)
  f.setFilePos(pos)
  if f.readBytes(result, 0, len) != len: result.setLen(0)

const metaTables = ["head", "maxp", "hhea", "OS/2", "post", "name", "cmap", "fvar", "vhea"]

proc sparseFace(f: File; fileSize, dirOff: int): Face =
  ## A face holding only the tables needed for names, metrics and matching (no outlines).
  let hdr = f.readAt(dirOff, 12)
  if hdr.len < 12: raise newException(FontError, "truncated font")
  let numTables = be16(hdr, 4)
  let dir = f.readAt(dirOff + 12, numTables * 16)
  if dir.len < numTables * 16: raise newException(FontError, "truncated font")
  var picked: seq[(string, seq[byte])]
  var hasOutlines = false
  for i in 0 ..< numTables:
    let r = i * 16
    var tg = newString(4)
    for k in 0 ..< 4: tg[k] = char(dir[r + k])
    let off = be32(dir, r + 8)
    let len = be32(dir, r + 12)
    if tg in ["glyf", "CFF ", "CFF2"]: hasOutlines = true
    if tg in metaTables and off >= 0 and len >= 0 and off + len <= fileSize:
      picked.add (tg, f.readAt(off, len))
  if hasOutlines: picked.add ("glyf", newSeq[byte]())
  var buf = newSeq[byte](12 + picked.len * 16)
  for k in 0 ..< 4: buf[k] = hdr[k]
  buf[4] = byte(picked.len shr 8)
  buf[5] = byte(picked.len and 0xFF)
  for i, (tg, data) in picked:
    let off = buf.len
    let r = 12 + i * 16
    for k in 0 ..< 4: buf[r + k] = byte(tg[k])
    for k, v in [off shr 24, off shr 16, off shr 8, off]: buf[r + 8 + k] = byte(v and 0xFF)
    for k, v in [data.len shr 24, data.len shr 16, data.len shr 8, data.len]: buf[r + 12 + k] = byte(v and 0xFF)
    buf.add data
    while buf.len mod 4 != 0: buf.add 0
  loadFace(newFontBlob(buf), 0)

proc scanFile(path: string): seq[(int, Face)] =
  ## (face index, metadata face) for every face of a font file; empty when unreadable.
  var f: File
  if not open(f, path, fmRead): return
  defer: f.close()
  try:
    let size = int(f.getFileSize)
    if size < 12 or size > MaxFontFileBytes: return
    let head = f.readAt(0, 12)
    if head.len < 12: return
    if head[0] == byte('t') and head[1] == byte('t') and head[2] == byte('c') and head[3] == byte('f'):
      let n = be32(head, 8)
      if n <= 0 or n > 4096: return
      let offs = f.readAt(12, n * 4)
      if offs.len < n * 4: return
      for i in 0 ..< n:
        try: result.add (i, sparseFace(f, size, be32(offs, i * 4)))
        except FontError, IOError, ValueError: discard
    else:
      try: result.add (0, sparseFace(f, size, 0))
      except FontError, IOError, ValueError: discard
  except IOError, OSError: discard

proc allNames(f: Face; id: int): seq[string] =
  ## fontique `all_names`: the US English (else Macintosh English, else first) name, then the rest.
  var recs: seq[NameRecord]
  for r in f.names:
    if r.nameId == id: recs.add r
  var best = -1
  var bestRank = -1
  for i, r in recs:
    if r.language == 0x409:
      best = i
      break
    let rank = if r.language == 0: 2 elif i == 0: 1 else: -1
    if rank > bestRank:
      bestRank = rank
      best = i
  if best >= 0 and recs[best].value.len > 0: result.add recs[best].value
  for i, r in recs:
    if i != best and r.value.len > 0: result.add r.value

proc widthRatio(widthClass: int): float32 =
  case widthClass
  of 0, 1: 0.5
  of 2: 0.625
  of 3: 0.75
  of 4: 0.875
  of 5: 1.0
  of 6: 1.125
  of 7: 1.25
  of 8: 1.5
  else: 2.0

proc newFaceRec(meta: Face; path: string; blob: FontBlob; index: int): FaceRec =
  result = FaceRec(path: path, blob: blob, index: index, meta: meta, axes: meta.axes)
  if meta.hasTable("OS/2"):
    result.width = widthRatio(meta.widthClass)
    if (meta.fsSelection and 1) != 0: result.style = fsItalic
    elif (meta.fsSelection and 0x200) != 0:
      result.style = fsOblique
      result.obliqueAngle = meta.italicAngle
    result.weight = float32(meta.weightClass)
  else:
    result.width = 1
    result.style = if (meta.macStyle and 2) != 0: fsItalic else: fsNormal
    result.weight = if (meta.macStyle and 1) != 0: 700 else: 400

proc load*(r: FaceRec): Face =
  ## The full face (outlines, layout tables), read on first use. Raises `FontError`.
  if not r.face.isNil: return r.face
  if r.loadFailed: raise newException(FontError, "font file could not be read")
  try:
    if r.blob.data.len == 0:
      if r.path.len == 0: raise newException(FontError, "no font data")
      let size = getFileSize(r.path)
      if size > MaxFontFileBytes: raise newException(FontError, "font file is too large")
      r.blob = newFontBlob(cast[seq[byte]](readFile(r.path)))
    r.face = loadFace(r.blob, r.index)
  except IOError, OSError:
    r.loadFailed = true
    raise newException(FontError, "font file could not be read: " & r.path)
  except FontError:
    r.loadFailed = true
    raise
  r.face

proc tryLoad*(r: FaceRec): Option[Face] =
  try: some(r.load) except FontError: none(Face)

proc blobId*(r: FaceRec): uint64 =
  ## Identity of the face's font data (stable while the database lives).
  if r.blob.data.len == 0: discard r.tryLoad
  r.blob.id

# ---------------------------------------------------------------- registration

proc addFace(db: FontDb; meta: Face; path: string; blob: FontBlob; index: int; familyOverride = ""): string =
  if not meta.hasCharmap: return ""
  if meta.postscriptName.isNone and familyOverride.len == 0: return ""
  var names = allNames(meta, 16)
  if names.len == 0: names = allNames(meta, 1)
  if familyOverride.len > 0: names = @[familyOverride]
  if names.len == 0: return ""
  let rec = newFaceRec(meta, path, blob, index)
  let key = nameKey(names[0])
  var fi = db.nameIndex.getOrDefault(key, -1)
  if fi < 0 or db.families[fi].name.nameKey != key and familyOverride.len > 0:
    fi = db.families.len
    db.families.add FamilyRec(name: names[0])
    db.nameIndex[key] = fi
  rec.family = db.families[fi].name
  db.families[fi].faces.add rec
  for alias in names[1 .. ^1]:
    let k = nameKey(alias)
    if k notin db.nameIndex: db.nameIndex[k] = fi
  db.families[fi].name

proc refreshGenerics(db: FontDb) =
  let inter = db.nameIndex.getOrDefault(nameKey(DefaultFamily), -1)
  if inter >= 0:
    for g in ["sans-serif", "serif", "system-ui", "ui-sans-serif"]:
      if db.generics.getOrDefault(g).len == 0: db.generics[g] = @[inter]
  let mono = db.nameIndex.getOrDefault(nameKey(MonoFamily), -1)
  if mono >= 0:
    for g in ["monospace", "ui-monospace"]:
      if db.generics.getOrDefault(g).len == 0: db.generics[g] = @[mono]
  db.fallbacks.setLen(0)
  for f in fallbackCandidates(uiScriptOrder()):
    if nameKey(f) in db.nameIndex: db.fallbacks.add f
  for f in scriptFallbackFamilies():
    if nameKey(f) in db.nameIndex and f notin db.fallbacks: db.fallbacks.add f

proc registerBlob(db: FontDb; blob: FontBlob): seq[string] =
  var n = 1
  try: n = faceCount(blob.data) except FontError: n = 0
  for i in 0 ..< n:
    try:
      let meta = loadFace(blob, i)
      let fam = db.addFace(meta, "", blob, i)
      if fam.len > 0 and fam notin result: result.add fam
    except FontError: discard
  result.sort(proc(a, b: string): int = cmp((a.isHiddenFamily, unicode.toLower(a)), (b.isHiddenFamily, unicode.toLower(b))))
  db.psCache.clear()
  db.faceCache.clear()
  db.refreshGenerics()
  inc fontGeneration

proc registerFontData*(db: FontDb; bytes: seq[byte]): seq[string] =
  ## Registers font data (TTF/OTF, or every face of a TTC/OTC). Returns the family names added.
  if bytes.len == 0 or bytes.len > MaxFontFileBytes: return
  db.registerBlob(newFontBlob(bytes))

proc newFontDb*(): FontDb =
  ## Bundled fonts only (deterministic: tests and headless use).
  result = FontDb()
  for (_, data) in bundledFontData: discard result.registerFontData(cast[seq[byte]](data))
  for f in craftFontList:
    if f.isJapanese: discard result.registerFontData(f.bytes)
  result.refreshGenerics()

proc systemFontDirs*(): seq[string] =
  let home = getEnv("HOME")
  when defined(macosx):
    result = @["/System/Library/Fonts", "/Library/Fonts"]
    if home.len > 0: result.add home / "Library/Fonts"
  elif defined(windows):
    result = @[getEnv("WINDIR", "C:\\Windows") / "Fonts"]
    let l = getEnv("LOCALAPPDATA")
    if l.len > 0: result.add l / "Microsoft\\Windows\\Fonts"
  else:
    result = @["/usr/share/fonts", "/usr/local/share/fonts", "/run/host/fonts", "/run/host/user-fonts"]
    let x = getEnv("XDG_DATA_HOME")
    if x.len > 0: result.add x / "fonts"
    if home.len > 0:
      result.add home / ".local/share/fonts"
      result.add home / ".fonts"

proc collectFontFiles*(dir: string; depth: int; output: var seq[string]) =
  if not dirExists(dir): return
  try:
    for kind, p in walkDir(dir):
      if kind in {pcDir, pcLinkToDir}:
        if depth < 8: collectFontFiles(p, depth + 1, output)
      elif p.splitFile.ext.toLowerAscii in [".ttf", ".otf", ".ttc", ".otc"]:
        output.add p
  except OSError: discard

proc loadSystemFonts*(db: FontDb) =
  ## Scans the platform font directories (once). Full font data is read when a face is used.
  if db.systemLoaded: return
  db.systemLoaded = true
  var files: seq[string]
  for d in systemFontDirs(): collectFontFiles(d, 0, files)
  for path in files:
    for (i, meta) in scanFile(path): discard db.addFace(meta, path, FontBlob(), i)
  db.psCache.clear()
  db.faceCache.clear()
  db.refreshGenerics()
  inc fontGeneration

proc newFontDbWithSystemFonts*(): FontDb =
  ## Bundled fonts plus the fonts installed on this machine.
  result = newFontDb()
  result.loadSystemFonts()

# ---------------------------------------------------------------- queries

proc familyIndex*(db: FontDb; name: string): int = db.nameIndex.getOrDefault(nameKey(name), -1)
proc hasFamily*(db: FontDb; name: string): bool = db.familyIndex(name) >= 0

proc familyNames*(db: FontDb): seq[string] =
  ## All family names, sorted (private '.' families hidden).
  for f in db.families:
    if not f.name.isHiddenFamily and f.name notin result: result.add f.name
  result.sort(proc(a, b: string): int = cmp(unicode.toLower(a), unicode.toLower(b)))

proc familyFaces*(db: FontDb; index: int): seq[FaceRec] =
  if index >= 0 and index < db.families.len: db.families[index].faces else: @[]

proc genericFamilies*(db: FontDb; generic: string): seq[int] = db.generics.getOrDefault(generic)

proc fallbackStack*(db: FontDb): seq[string] =
  ## Families tried after the requested one: the bundled default, then installed coverage fonts.
  result = @[DefaultFamily]
  result.add db.fallbacks

proc matchFace*(faces: seq[FaceRec]; width: float32; style: FontStyleKind; obliqueAngle: float32;
                weight: float32): int =
  ## CSS font matching (fontique `match_font`): width, then style, then weight. -1 when empty.
  const Threshold = 14'f32
  if faces.len == 0: return -1
  if faces.len == 1: return 0
  var set: seq[int]
  for i in 0 ..< faces.len: set.add i
  proc w(i: int): int = int(faces[i].width * 100)
  let wantW = int(width * 100)
  var useW = w(set[0])
  var exact = false
  for i in set:
    if w(i) == wantW: exact = true
  if exact: useW = wantW
  else:
    var below, above = -1
    for i in set:
      if w(i) < wantW and (below < 0 or w(i) > w(below)): below = i
      if w(i) > wantW and (above < 0 or w(i) < w(above)): above = i
    if wantW <= 100:
      if below >= 0: useW = w(below) elif above >= 0: useW = w(above)
    else:
      if above >= 0: useW = w(above) elif below >= 0: useW = w(below)
  var kept: seq[int]
  for i in set:
    if w(i) == useW: kept.add i
  set = kept
  # Style.
  proc angle(i: int): float32 = (if faces[i].obliqueAngle != 0: faces[i].obliqueAngle else: Threshold)
  proc same(i: int; s: FontStyleKind; a: float32): bool =
    faces[i].style == s and (s != fsOblique or angle(i) == a)
  var useStyle = style
  var useAngle = if style == fsOblique: (if obliqueAngle != 0: obliqueAngle else: Threshold) else: 0'f32
  var any = false
  for i in set:
    if same(i, useStyle, useAngle): any = true
  if not any:
    proc pickOblique(filter: proc(a: float32): bool; smallest: bool): int =
      result = -1
      for i in set:
        if faces[i].style == fsOblique and filter(angle(i)):
          if result < 0 or (if smallest: angle(i) < angle(result) else: angle(i) > angle(result)): result = i
    if style == fsItalic:
      var p = pickOblique(proc(a: float32): bool = a >= Threshold, true)
      if p < 0: p = pickOblique(proc(a: float32): bool = a > 0 and a < Threshold, false)
      if p < 0: p = pickOblique(proc(a: float32): bool = a < 0, false)
      if p >= 0: (useStyle = fsOblique; useAngle = angle(p))
      else: (useStyle = faces[set[0]].style; useAngle = angle(set[0]))
    elif style == fsOblique:
      let a = useAngle
      var p = -1
      if a >= Threshold:
        p = pickOblique(proc(x: float32): bool = x >= a, true)
        if p < 0: p = pickOblique(proc(x: float32): bool = x > 0 and x < a, false)
      elif a >= 0:
        p = pickOblique(proc(x: float32): bool = x > 0 and x < a, false)
        if p < 0: p = pickOblique(proc(x: float32): bool = x >= a, true)
      elif a > -Threshold:
        p = pickOblique(proc(x: float32): bool = x < 0 and x > a, true)
        if p < 0: p = pickOblique(proc(x: float32): bool = x <= a, false)
      else:
        p = pickOblique(proc(x: float32): bool = x <= a, false)
        if p < 0: p = pickOblique(proc(x: float32): bool = x < 0 and x > a, true)
      if p >= 0: (useStyle = fsOblique; useAngle = angle(p))
      else:
        # Synthesized from an upright face (fontique always asks with synthesis on).
        var hasNormal = false
        for i in set:
          if faces[i].style == fsNormal: hasNormal = true
        useStyle = if hasNormal: fsNormal else: faces[set[0]].style
        useAngle = angle(set[0])
    else:
      var p = pickOblique(proc(x: float32): bool = x >= 0, true)
      if p >= 0: (useStyle = fsOblique; useAngle = angle(p))
      else:
        var it = -1
        for i in set:
          if faces[i].style == fsItalic and it < 0: it = i
        if it >= 0: useStyle = fsItalic
        else:
          p = pickOblique(proc(x: float32): bool = x < 0, false)
          if p >= 0: (useStyle = fsOblique; useAngle = angle(p))
          else: (useStyle = faces[set[0]].style; useAngle = angle(set[0]))
  kept.setLen(0)
  for i in set:
    if faces[i].style == useStyle and (useStyle != fsOblique or angle(i) == useAngle): kept.add i
  set = kept
  if set.len == 0: return -1
  # Weight.
  for i in set:
    if faces[i].weight == weight: return i
  proc best(filter: proc(x: float32): bool; smallest: bool): int =
    result = -1
    for i in set:
      if filter(faces[i].weight):
        if result < 0 or (if smallest: faces[i].weight < faces[result].weight else: faces[i].weight > faces[result].weight):
          result = i
  var r = -1
  if weight >= 400 and weight <= 500:
    r = best(proc(x: float32): bool = x >= weight and x <= 500, true)
    if r < 0: r = best(proc(x: float32): bool = x < weight, false)
    if r < 0: r = best(proc(x: float32): bool = x > 500, true)
  elif weight < 400:
    r = best(proc(x: float32): bool = x <= weight, false)
    if r < 0: r = best(proc(x: float32): bool = x > weight, true)
  else:
    r = best(proc(x: float32): bool = x >= weight, true)
    if r < 0: r = best(proc(x: float32): bool = x < weight, false)
  r

proc synthesis*(r: FaceRec; width: float32; style: FontStyleKind; obliqueAngle: float32; weight: float32): Synthesis =
  ## What to synthesize to draw `r` with the requested attributes (fontique `FontInfo::synthesis`).
  proc hasAxis(t: string): bool =
    for a in r.axes:
      if a.tag == t: return true
    false
  if hasAxis("wdth") and r.width != width: result.vars.add ("wdth", width * 100)
  if r.weight != weight:
    if hasAxis("wght"): result.vars.add ("wght", weight)
    elif weight > r.weight: result.embolden = true
  let sameStyle = r.style == style and (style != fsOblique or r.obliqueAngle == obliqueAngle)
  if not sameStyle and r.style == fsNormal:
    case style
    of fsItalic:
      if hasAxis("ital"): result.vars.add ("ital", 1'f32)
      elif hasAxis("slnt"): result.vars.add ("slnt", 14'f32)
      else: result.skew = 14
    of fsOblique:
      let deg = if obliqueAngle != 0: obliqueAngle else: 14'f32
      if hasAxis("slnt"): result.vars.add ("slnt", deg)
      elif hasAxis("ital") and deg > 0: result.vars.add ("ital", 1'f32)
      else: result.skew = float32(int(deg))
    of fsNormal: discard

proc selectFace*(db: FontDb; family: string; weight: float32; italic: bool): Option[FaceRec] =
  let fi = db.familyIndex(family)
  if fi < 0: return
  let faces = db.families[fi].faces
  let i = matchFace(faces, 1, (if italic: fsItalic else: fsNormal), 0, weight)
  if i >= 0: some(faces[i]) else: none(FaceRec)

proc selectedFaceHasFeature*(db: FontDb; family: string; weight: uint16; italic: bool; feature: string): bool =
  ## Whether the face matched for a character style has GSUB `feature` (checked on that face).
  let fam = if family.len == 0 or not db.hasFamily(family): DefaultFamily else: family
  let r = db.selectFace(fam, float32(clamp(int(weight), 1, 1000)), italic)
  if r.isNone: return false
  let f = r.get.tryLoad
  if f.isNone: return false
  f.get.hasFeature("GSUB", feature)

proc fallbackStyle(weight: float32; italic: bool): string =
  let name = case int(round(weight))
    of low(int) .. 150: "Thin"
    of 151 .. 250: "ExtraLight"
    of 251 .. 350: "Light"
    of 351 .. 450: "Regular"
    of 451 .. 550: "Medium"
    of 551 .. 650: "SemiBold"
    of 651 .. 750: "Bold"
    of 751 .. 850: "ExtraBold"
    else: "Black"
  if italic: (if name == "Regular": "Italic" else: name & " Italic") else: name

proc faces*(db: FontDb; family: string): seq[FaceInfo] =
  ## Faces of a family, keeping each face's own names even when they share CSS attributes.
  if family in db.faceCache: return db.faceCache[family]
  let fi = db.familyIndex(family)
  if fi < 0: return
  for r in db.families[fi].faces:
    let italic = r.style != fsNormal
    var style = r.meta.name(17).get("")
    if style.len == 0: style = r.meta.name(2).get("")
    if style.len == 0: style = fallbackStyle(r.weight, italic)
    var axes: seq[(string, float32, float32, float32)]
    for a in r.axes: axes.add (a.tag, a.minValue, a.defaultValue, a.maxValue)
    let ps = r.meta.postscriptName
    result.add FaceInfo(family: db.families[fi].name, style: style,
                        postscriptName: (if ps.isSome and ps.get.len > 0: ps else: none(string)),
                        weight: r.weight, italic: italic, axes: axes)
  db.faceCache[family] = result

proc namedFace*(db: FontDb; family, style: string): Option[FaceInfo] =
  for f in db.faces(family):
    if cmpIgnoreCase(f.style, style) == 0: return some(f)

proc selectNamedFace*(db: FontDb; style: var CharStyle) =
  ## Picks the exact face a style names (PostScript name, or style name + attributes) through a
  ## private alias family, so CSS matching can't substitute a sibling with the same attributes.
  let family = style.fontFamily
  let fs = db.faces(family)
  var index = -1
  if style.postscriptName.isSome:
    for i, f in fs:
      if f.postscriptName == style.postscriptName:
        index = i
        break
  if index < 0 and style.fontStyle.len > 0:
    for i, f in fs:
      if cmpIgnoreCase(f.style, style.fontStyle) == 0 and uint16(round(f.weight)) == style.weight and
         f.italic == style.italic:
        index = i
        break
  if index < 0: return
  let face = fs[index]
  let key = (family, index)
  var alias = db.faceAliases.getOrDefault(key, "")
  if alias.len == 0:
    let fi = db.familyIndex(family)
    if fi < 0 or index >= db.families[fi].faces.len: return
    let src = db.families[fi].faces[index]
    alias = ".PhotoCraft-face-" & $fi & "-" & $index
    let rec = FaceRec(family: alias, path: src.path, blob: src.blob, index: src.index, weight: src.weight,
                      width: src.width, style: src.style, obliqueAngle: src.obliqueAngle, axes: src.axes,
                      meta: src.meta, face: src.face)
    let nf = db.families.len
    db.families.add FamilyRec(name: alias, faces: @[rec])
    db.nameIndex[nameKey(alias)] = nf
    db.faceAliases[key] = alias
  style.fontFamily = alias
  style.weight = uint16(round(face.weight))
  style.italic = face.italic

proc guessFromPostscript*(ps: string): ResolvedFont =
  ## Heuristic PostScript-name split: `MyriadPro-BoldIt` → ("Myriad Pro", 700, italic).
  let dash = ps.find('-')
  var fam = if dash >= 0: ps[0 ..< dash] else: ps
  let styl = if dash >= 0: ps[dash + 1 .. ^1] else: ""
  let pro = fam
  for suffix in ["MT", "PS", "Std", "Pro"]:
    while fam.endsWith(suffix): fam = fam[0 ..< fam.len - suffix.len]
  let suffix = if pro.endsWith("Pro"): " Pro" elif pro.endsWith("Std"): " Std" else: ""
  var family = ""
  let chars = toSeq(fam.runes)
  for i, c in chars:
    if i > 0 and c.isUpper and (chars[i - 1].isLower or (i + 1 < chars.len and chars[i + 1].isLower and chars[i - 1].isUpper)):
      family.add ' '
    family.add $c
  family.add suffix
  let s = styl.toLowerAscii
  let weight: uint16 =
    if "thin" in s or "hairline" in s: 100
    elif "extralight" in s or "ultralight" in s: 200
    elif "light" in s: 300
    elif "medium" in s: 500
    elif "semibold" in s or "demibold" in s or "demi" in s: 600
    elif "extrabold" in s or "ultrabold" in s or "heavy" in s: 800
    elif "black" in s: 900
    elif "bold" in s: 700
    else: 400
  let italic = "italic" in s or s.endsWith("it") or "oblique" in s
  ResolvedFont(family: family.strip, weight: weight, italic: italic, exact: false)

proc findExact(db: FontDb; ps, familyGuess: string): Option[ResolvedFont] =
  let firstWord = familyGuess.split(' ')[0].toLowerAscii
  var candidates = db.familyNames
  candidates.sort(proc(a, b: string): int =
    cmp(not a.toLowerAscii.replace(" ", "").startsWith(firstWord), not b.toLowerAscii.replace(" ", "").startsWith(firstWord)),
    SortOrder.Ascending)
  for fam in candidates:
    for face in db.faces(fam):
      if face.postscriptName.isNone: continue
      let name = face.postscriptName.get
      let r = ResolvedFont(family: face.family, weight: uint16(round(face.weight)), italic: face.italic, exact: true)
      db.psCache[name] = some(r)
      if name == ps: return some(r)

proc resolvePostscript*(db: FontDb; ps: string): ResolvedFont =
  ## A face by PostScript name (as PSD files store it): exact match by name table, else a
  ## heuristic family guess (kept only when the family exists here).
  let c = db.psCache.getOrDefault(ps, none(ResolvedFont))
  if c.isSome: return c.get
  let guess = guessFromPostscript(ps)
  let exact = db.findExact(ps, guess.family)
  result = if exact.isSome: exact.get else:
    var g = guess
    if not db.hasFamily(g.family):
      for f in db.familyNames:
        if cmpIgnoreCase(f.replace(" ", ""), g.family.replace(" ", "")) == 0:
          g.family = f
          break
    g
  db.psCache[ps] = some(result)

proc localizedName*(english: string; names: seq[(string, Option[string])]): Option[string] =
  ## The first family name in a non-English language that differs from `english`.
  for (name, lang) in names:
    if lang.isNone or lang.get.len == 0: continue
    let primary = lang.get.split({'-', '_'})[0]
    if cmpIgnoreCase(primary, "en") == 0: continue
    if name.strip.len == 0 or name == english: continue
    return some(name)

proc localizedFamilyNames*(db: FontDb): Table[string, string] =
  ## Each family's localized (non-English) name, for font menus with English names off.
  for family in db.familyNames:
    let fi = db.familyIndex(family)
    if fi < 0 or db.families[fi].faces.len == 0: continue
    let meta = db.families[fi].faces[0].meta
    var names = meta.localizedNames(16)
    if names.len == 0: names = meta.localizedNames(1)
    let l = localizedName(family, names)
    if l.isSome: result[family] = l.get

proc faceCountOf*(bytes: openArray[byte]): int =
  ## Number of faces in a font file (1 for TTF/OTF, n for TTC/OTC, 0 if unreadable).
  try: faceCount(bytes) except FontError: 0
