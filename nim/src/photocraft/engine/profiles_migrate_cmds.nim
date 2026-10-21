## `edit.installedProfiles` (port of `installed_profiles.rs`) and Edit › Presets › Migrate Presets
## (`migrate_cmds.rs`).
##
## Installed profiles: ICC profiles installed in the operating system, as Photoshop lists them in
## Print › Printer Profile, View › Proof Setup › Custom and Edit › Convert to Profile, by their
## description; the commands take the file's path.
##
## Migrate Presets merges a presets file (another install's preferences, or an exported preset
## library) into the current library, appending groups the library doesn't already have (by name).

import std/[json, options, os, strutils, algorithm, times]
from std/unicode import validateUtf8
import ./core
import ../cms
import ../sjson

# ---------------------------------------------------------------- installed_profiles.rs

type InstalledProfile* = object
  path*: string ## the file, as the profile commands take it
  description*: string
  class*: ProfileClass
  colorSpace*: ColorSpace

proc isDestination*(p: InstalledProfile): bool =
  ## Whether a document can be assigned, converted or proofed to it (device and colour space
  ## profiles in RGB, CMYK or Gray; device links, abstract and named colour profiles are not).
  p.class.kind in {pcInput, pcDisplay, pcOutput, pcColorSpace} and p.colorSpace.kind in {csRgb, csCmyk, csGray}

const
  MaxScanDepth = 3            ## folders scanned at most this deep
  MaxScanFiles = 1000         ## at most this many files are parsed per scan
  MaxProfileBytes = 32 shl 20 ## larger files are not profiles anyone prints with
  FreshSeconds = 10.0         ## how long a scan is reused

proc profileDirs*(): seq[string] =
  ## The operating system's colour profile folders.
  var home = getEnv("HOME")
  if home.len == 0: home = getEnv("USERPROFILE")
  when defined(windows):
    let root = (if existsEnv("SystemRoot"): getEnv("SystemRoot") else: r"C:\Windows")
    result.add root / r"System32\spool\drivers\color"
  elif defined(macosx):
    result.add "/System/Library/ColorSync/Profiles"
    result.add "/Library/ColorSync/Profiles"
    if existsEnv("HOME") or existsEnv("USERPROFILE"): result.add home / "Library/ColorSync/Profiles"
  else:
    result.add "/usr/share/color/icc"
    result.add "/usr/local/share/color/icc"
    if existsEnv("HOME") or existsEnv("USERPROFILE"):
      result.add home / ".local/share/icc"
      result.add home / ".color/icc"

proc collect(dir: string; depth: int; files: var seq[string]) =
  if depth > MaxScanDepth or files.len >= MaxScanFiles: return
  try:
    for kind, path in walkDir(dir):
      if files.len >= MaxScanFiles: return
      if kind in {pcDir, pcLinkToDir}: collect(path, depth + 1, files)
      else:
        let ext = splitFile(path).ext
        if ext.len > 1 and (cmpIgnoreCase(ext[1 .. ^1], "icc") == 0 or cmpIgnoreCase(ext[1 .. ^1], "icm") == 0):
          files.add path
  except OSError: discard

proc readOne(path: string): Option[InstalledProfile] =
  try:
    if getFileSize(path) > MaxProfileBytes: return
    let bytes = readFile(path)
    let p = parseProfile(bytes.toOpenArrayByte(0, bytes.high))
    let d = strip(p.description)
    let description = (if d.len == 0: splitFile(path).name else: d)
    some(InstalledProfile(path: path, description: description, class: p.class, colorSpace: p.colorSpace))
  except CatchableError: none(InstalledProfile)

proc scan*(dirs: seq[string]): seq[InstalledProfile] =
  ## Every profile under `dirs` that parses, sorted by description; unreadable or malformed files
  ## are skipped.
  var files: seq[string]
  for d in dirs: collect(d, 0, files)
  for f in files:
    let p = readOne(f)
    if p.isSome: result.add p.get
  result.sort(proc(a, b: InstalledProfile): int =
    result = cmp(toLowerAscii(a.description), toLowerAscii(b.description))
    if result == 0: result = cmp(a.path, b.path))
  var dedup: seq[InstalledProfile]
  for p in result:
    if dedup.len == 0 or dedup[^1].path != p.path: dedup.add p
  result = dedup

var profileCache {.threadvar.}: Option[(float64, seq[InstalledProfile])]

proc installed*(): seq[InstalledProfile] =
  ## The installed profiles (cached for `FreshSeconds`). Empty on the web.
  when defined(js) or defined(wasm32): return
  else:
    let now = epochTime()
    if profileCache.isSome and now - profileCache.get[0] < FreshSeconds: return profileCache.get[1]
    result = scan(profileDirs())
    profileCache = some((now, result))

proc classId(c: ProfileClass): string =
  case c.kind
  of pcInput: "input"
  of pcDisplay: "display"
  of pcOutput: "output"
  of pcDeviceLink: "deviceLink"
  of pcColorSpace: "colorSpace"
  of pcAbstract: "abstract"
  of pcNamedColor: "namedColor"
  of pcUnknown: "unknown"

proc spaceId(c: ColorSpace): string =
  ## `format!("{c:?}").to_lowercase()`.
  case c.kind
  of csXyz: "xyz"
  of csLab: "lab"
  of csLuv: "luv"
  of csYCbCr: "ycbcr"
  of csYxy: "yxy"
  of csRgb: "rgb"
  of csGray: "gray"
  of csHsv: "hsv"
  of csHls: "hls"
  of csCmyk: "cmyk"
  of csCmy: "cmy"
  of csColor: "color(" & $c.n & ")"
  of csUnknown: "unknown(" & $c.raw & ")"

proc installedProfilesCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "edit.installedProfiles"
  const classes = ["any", "input", "display", "output", "deviceLink", "colorSpace", "abstract", "namedColor"]
  var class = "any"
  let cv = p.pget("class")
  if not cv.isNull:
    if cv.kind == JString and cv.str in classes: class = cv.str
    else: raise bad(cmdId, "class is any|input|display|output|deviceLink|colorSpace|abstract|namedColor, not " & jsonText(cv))
  var space = "any"
  let sv = p.pget("colorSpace")
  if not sv.isNull:
    if sv.kind == JString and sv.str in ["any", "rgb", "cmyk", "gray"]: space = sv.str
    else: raise bad(cmdId, "colorSpace is any|rgb|cmyk|gray, not " & jsonText(sv))
  var profiles = newJArray()
  for i in installed():
    if class != "any" and classId(i.class) != class: continue
    if space != "any" and spaceId(i.colorSpace) != space: continue
    profiles.add %*{"path": i.path, "description": i.description, "class": classId(i.class), "colorSpace": spaceId(i.colorSpace)}
  %*{"profiles": profiles, "dirs": profileDirs()}

proc installedProfilesSpecs*(): seq[CommandSpec] =
  @[cmd("edit.installedProfiles", "Installed Profiles", [], "",
        """{"class":"any|input|display|output|deviceLink|colorSpace|abstract|namedColor"="any","colorSpace":"any|rgb|cmyk|gray"="any"} → {profiles: [{path, description, class, colorSpace}], dirs} (the OS profile folders; pass a path as any command's "profile")""",
        enabledProc(""), runProc(installedProfilesCmd(s, p)), journal = false)]

# ---------------------------------------------------------------- migrate_cmds.rs

proc mergeGroups[T](cur: var seq[PresetGroup[T]]; incoming: seq[PresetGroup[T]]): int =
  ## Append incoming groups whose name the library doesn't already have; returns how many were added.
  for g in incoming:
    var have = false
    for e in cur:
      if e.name == g.name:
        have = true
        break
    if not have:
      cur.add g
      inc result

proc groupsOf[T](v: JsonNode; key: string): seq[PresetGroup[T]] =
  ## `v[key]` as groups, empty when absent or malformed.
  let g = v.pget(key)
  if g.isNil: return
  try: fromJ(g, result)
  except CatchableError: result = @[]

proc migrate(s: var Session; p: JsonNode): JsonNode =
  let pv = p.pget("path").asStr
  if pv.isNone: raise bad("edit.presets.migratePresets", "need `path` to a presets or preferences file")
  let path = pv.get
  var text: string
  try:
    let bytes = readBytesRust(path)
    text = newString(bytes.len)
    if bytes.len > 0: copyMem(addr text[0], unsafeAddr bytes[0], bytes.len)
    if validateUtf8(text) >= 0: raise newException(OSError, "stream did not contain valid UTF-8")
  except OSError as e: raise other("read `" & path & "`: " & e.msg)
  var root: JsonNode
  try: root = parseSerdeJson(text)
  except SjsonError as e: raise other("`" & path & "` is not JSON: " & e.msg)
  # Accept a full preferences file (has a `presets` section) or a bare presets object.
  let v = (if not root.pget("presets").isNil: root.pget("presets") else: root)
  let g = mergeGroups(s.presets.gradients, groupsOf[GradientPreset](v, "gradients"))
  let st = mergeGroups(s.presets.styles, groupsOf[StylePreset](v, "styles"))
  let sh = mergeGroups(s.presets.shapes, groupsOf[ShapePreset](v, "shapes"))
  let pt = mergeGroups(s.presets.patternGroups, groupsOf[string](v, "pattern_groups"))
  # Tool presets are a flat list, matched by name.
  var tp = 0
  let arr = v.pget("tool_presets")
  if not arr.isNil:
    var incoming: seq[ToolPreset]
    var ok = true
    try: fromJ(arr, incoming)
    except CatchableError: ok = false
    if ok:
      for t in incoming:
        var have = false
        for e in s.presets.toolPresets:
          if e.name == t.name:
            have = true
            break
        if not have:
          s.presets.toolPresets.add t
          inc tp
  let total = g + st + sh + pt + tp
  if total > 0: s.presetsChanged()
  %*{"migrated": total, "added": {"gradients": g, "styles": st, "shapes": sh, "patternGroups": pt, "toolPresets": tp}}

proc migrateSpecs*(): seq[CommandSpec] =
  @[cmd("edit.presets.migratePresets", "Migrate Presets", ["Edit", "Presets"], "",
        """{path} → {migrated, added:{gradients,styles,shapes,patternGroups,toolPresets}}: merge a presets/preferences file into the library (appends groups by name)""",
        enabledProc(""), runProc(migrate(s, p)), journal = false)]
