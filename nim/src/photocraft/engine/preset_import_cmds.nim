## Importing Photoshop preset files (port of `preset_import_cmds.rs`).
##
## - `brush.presets.importAbr` adds the presets of an `.abr` file (v1, v2, v6+) to the brush
##   library as one group named after the file, with the file's own folders nested inside it
##   as Photoshop does (Brushes panel › Import Brushes…, Preset Manager). Parsing lives in
##   `psd/abr`, the mapping onto `BrushSettings` in `io/abr_map`.
## - `gradient.presets.importGrd` adds the gradients of a `.grd` file (version 5) to the
##   Gradients panel as one group.

import std/[json, options, os, strutils, unicode, algorithm, sets, math]
import ./core
import ../paint/tile as ptile
import ../paint/presets as ppresets
import ../paint/brush as pbrush
import ../rmath
import ../psd/grd
import ../io/abr_map
from ../format import NulPathMsg
when not defined(js): import std/posix

proc alwaysE(s: Session): string = ""

proc trimU*(s: string): string = unicode.strip(s)
  ## Rust's `str::trim` (Unicode white space).

const MaxFileBytes* = 512'u64 shl 20 ## largest accepted file (Photoshop's biggest bundled sets are well under this)

proc fileLen(path: string): uint64 =
  ## `std::fs::metadata(path)?.len()`, raising `OSError` with Rust's message.
  when defined(js) or defined(wasm32):
    raise newException(OSError, "no file system")
  else:
    if '\0' in path: raise newException(OSError, NulPathMsg)
    var st: Stat
    if stat(path.cstring, st) != 0: raise newException(OSError, osErrorText(osLastError()))
    uint64(st.st_size)

proc fileBytes*(p: JsonNode; cmdId, what: string): (seq[byte], string) =
  ## File bytes from `data` (base64) or `path`, plus a default group name.
  let d = p.pget("data")
  if not d.isNil:
    if d.kind != JString: raise bad(cmdId, "`data` must be a base64 string")
    if uint64(d.str.len) > MaxFileBytes div 3 * 4 + 4: raise bad(cmdId, "`data` is too large")
    let bytes = b64Decode(d.str)
    if bytes.isNone: raise bad(cmdId, "`data` is not valid base64")
    return (bytes.get, "")
  let pv = p.pget("path").asStr
  if pv.isNone or trimU(pv.get).len == 0: raise bad(cmdId, "give `path` (" & what & ") or `data` (base64)")
  let path = pv.get
  var len: uint64
  try: len = fileLen(path)
  except OSError as e: raise other(path & ": " & e.msg)
  if len > MaxFileBytes: raise bad(cmdId, path & ": file too large (" & $len & " bytes)")
  let bytes = readFileOr(path)
  (bytes, splitFile(path).name)

proc free(n: string; taken: openArray[string]): bool =
  for t in taken:
    if t.len == n.len and t.toLowerAscii == n.toLowerAscii: return false
  true

proc unique(name: string; taken: openArray[string]): string =
  ## A name not used by any preset in `taken` (case-insensitive): `name`, `name 2`, `name 3`…
  if free(name, taken): return name
  var i = 2
  while true:
    let n = name & " " & $i
    if free(n, taken): return n
    inc i

proc groupParam(p: JsonNode; cmdId, stem, fallback: string): string =
  let v = p.pget("group")
  if not v.isNil:
    let g = v.asStr
    if g.isNone or trimU(g.get).len == 0: raise bad(cmdId, "`group` must be a non-empty string")
    return trimU(g.get)
  if stem.len > 0: stem else: fallback

proc addAbrPresets(s: var Session; p: JsonNode; group: string; imp: AbrImport): JsonNode =
  # Re-importing a file replaces its group instead of duplicating it.
  if p.pget("replace").asBool.get(true):
    var kept: seq[BrushPreset]
    for x in s.tools.presets:
      if x.builtin or x.group != group: kept.add x
    s.tools.presets = kept
  var taken: seq[string]
  for x in s.tools.presets: taken.add x.name
  var names: seq[string]
  for preset0 in imp.presets:
    var preset = preset0
    preset.name = unique(preset.name, taken)
    taken.add preset.name
    names.add preset.name
    s.tools.presets.add preset
  s.brushPresetsChanged()
  if p.pget("select").asBool.get(false) and names.len > 0:
    let pr = ppresets.find(s.tools.presets, names[0])
    if pr.isSome:
      var b = pr.get.brush.pickedOver(s.tools.brush)
      let why = s.loadBrushTips(b)
      if why.len > 0: raise bad("brush.presets.importAbr", why)
      s.tools.brush = b
  # Distinct folder paths the file brought (its Photoshop folders, nested inside `group`).
  var folders = initHashSet[seq[string]]()
  for x in s.tools.presets:
    if not x.builtin and x.group == group:
      for d in 1 .. x.folder.len: folders.incl x.folder[0 ..< d]
  var imported = newJArray()
  for n in names: imported.add %n
  %*{"group": group, "imported": imported, "count": names.len, "folders": folders.len, "version": imp.version,
     "warnings": imp.warnings}

proc importAbr(s: var Session; p: JsonNode): JsonNode =
  const C = "brush.presets.importAbr"
  let params = p
  runJob(s, "Import Brushes", false,
    proc(ctx: JobCtx): (string, AbrImport) =
      ctx.progress(0.0, "Importing brushes")
      let (bytes, stem) = fileBytes(params, C, ".abr file")
      let group = groupParam(params, C, stem, "Imported Brushes")
      ctx.check()
      var imp: AbrImport
      try: imp = readAbrWith(bytes, group, ctx.interrupt(0.05, 1.0, "Importing brushes"))
      except ValueError as e:
        if ctx.cancelled: raise cancelledErr()
        raise bad(C, e.msg)
      (group, imp),
    proc(s: var Session; r: (string, AbrImport)): JsonNode = addAbrPresets(s, params, r[0], r[1]))

# ---------------------------------------------------------------- gradients (.grd)

proc hsbToRgbRaw(c: array[3, float32]): array[3, float32] =
  let h = remEuclidF(c[0], 360'f32) / 60'f32
  let ch = c[2] * c[1]
  let x = ch * (1'f32 - abs(h mod 2'f32 - 1'f32))
  let hi = (if h != h: 0'u32 elif h >= 4294967295'f32: high(uint32) elif h <= 0: 0'u32 else: uint32(h))
  let (r, g, b) = (case hi
                   of 0: (ch, x, 0'f32)
                   of 1: (x, ch, 0'f32)
                   of 2: (0'f32, ch, x)
                   of 3: (0'f32, x, ch)
                   of 4: (x, 0'f32, ch)
                   else: (ch, 0'f32, x))
  let m = c[2] - ch
  [r + m, g + m, b + m]

proc labToRgbRaw(c: array[3, float32]): array[3, float32] =
  ## CIE Lab (D50, as Photoshop stores it) to sRGB through XYZ with Bradford adaptation to D65.
  let (l, a, b) = (c[0], c[1], c[2])
  let fy = (l + 16'f32) / 116'f32
  let (fx, fz) = (fy + a / 500'f32, fy - b / 200'f32)
  proc inv(t: float32): float32 =
    if t > 6'f32 / 29'f32: t * t * t
    else:
      let k = 6'f32 / 29'f32
      3'f32 * (k * k) * (t - 4'f32 / 29'f32)
  let (x, y, z) = (0.9642'f32 * inv(fx), inv(fy), 0.8249'f32 * inv(fz))
  # XYZ (D50) → linear sRGB (D65), Bradford-adapted matrix.
  let r = 3.1339'f32 * x - 1.6169'f32 * y - 0.4906'f32 * z
  let g = -0.9788'f32 * x + 1.9161'f32 * y + 0.0335'f32 * z
  let bb = 0.0719'f32 * x - 0.2290'f32 * y + 1.4052'f32 * z
  proc gamma(c0: float32): float32 =
    let c = clamp(c0, 0'f32, 1'f32)
    if c <= 0.0031308'f32: 12.92'f32 * c else: 1.055'f32 * pow(c, 1'f32 / 2.4'f32) - 0.055'f32
  [gamma(r), gamma(g), gamma(bb)]

proc stopColor(c: GrdColor): StopColor =
  proc cl(c: array[3, float32]): StopColor =
    var v = c
    for x in v.mitems: x = (if x == x and abs(x) != Inf: clamp(x, 0'f32, 1'f32) else: 0'f32)
    StopColor(kind: scRgb, rgb: v)
  case c.kind
  of gcForeground: StopColor(kind: scForeground)
  of gcBackground: StopColor(kind: scBackground)
  of gcRgb: cl([c.v[0], c.v[1], c.v[2]])
  of gcHsb: cl(hsbToRgbRaw([c.v[0], c.v[1], c.v[2]]))
  # Naive (profile-free) ink → RGB, like an uncalibrated preview.
  of gcCmyk: cl([(1'f32 - c.v[0]) * (1'f32 - c.v[3]), (1'f32 - c.v[1]) * (1'f32 - c.v[3]), (1'f32 - c.v[2]) * (1'f32 - c.v[3])])
  of gcLab: cl(labToRgbRaw([c.v[0], c.v[1], c.v[2]]))
  of gcGray: cl([1'f32 - c.v[0], 1'f32 - c.v[0], 1'f32 - c.v[0]])

proc importGrd(s: var Session; p: JsonNode): JsonNode =
  const C = "gradient.presets.importGrd"
  let (bytes, stem) = fileBytes(p, C, ".grd file")
  let group = groupParam(p, C, stem, "Imported Gradients")
  var grads: seq[GrdGradient]
  try: grads = parseGrd(bytes)
  except CatchableError as e: raise bad(C, "not a readable gradient file (.grd): " & e.msg)
  var warnings: seq[string]
  var items: seq[GradientPreset]
  var noise = 0
  var midpoints = false
  for i, g in grads:
    if g.noise or g.stops.len < 2:
      inc noise
      continue
    for st in g.stops:
      if abs(st.midpoint - 0.5'f32) > 0.01'f32: midpoints = true
    for o in g.opacity:
      if abs(o[2] - 0.5'f32) > 0.01'f32: midpoints = true
    var stops: seq[(float32, StopColor)]
    for st in g.stops: stops.add (st.location, stopColor(st.color))
    stops.sort(proc(a, b: (float32, StopColor)): int = cmp(a[0], b[0]))
    var opacity: seq[(float32, float32)]
    for o in g.opacity: opacity.add (o[0], o[1])
    opacity.sort(proc(a, b: (float32, float32)): int = cmp(a[0], b[0]))
    var allOpaque = true
    for o in opacity:
      if not (o[1] >= 1'f32): allOpaque = false
    if allOpaque: opacity.setLen(0)
    let name = (if trimU(g.name).len == 0: "Gradient " & $(i + 1) else: trimU(g.name))
    items.add GradientPreset(name: name, stops: stops, opacity: opacity)
  if noise > 0: warnings.add $noise & " noise gradients are not supported and were skipped"
  if midpoints: warnings.add "colour and opacity midpoints other than 50 % are drawn at 50 %"
  if items.len == 0: raise bad(C, "the file holds no gradients PhotoCraft can use (noise gradients only)")
  var names = newJArray()
  for g in items: names.add %g.name
  if p.pget("replace").asBool.get(true):
    var kept: seq[PresetGroup[GradientPreset]]
    for g in s.presets.gradients:
      if g.name != group: kept.add g
    s.presets.gradients = kept
  s.presets.gradients.add newGroup(group, items)
  s.presetsChanged()
  %*{"group": group, "imported": names, "count": items.len, "warnings": warnings}

proc presetImportSpecs*(): seq[CommandSpec] =
  @[cmd("brush.presets.importAbr", "Import Brushes…", [], "",
        """{"path":".abr file"?,"data":base64 bytes?,"group":string?=file name,"replace":bool=true (replace a group of the same name),"select":bool=false (make the first imported preset current)} → {group, imported, count, folders (nested folders kept from the file), version, warnings}""",
        enabledProc(alwaysE(s)), runProc(importAbr(s, p))),
    cmd("gradient.presets.importGrd", "Import Gradients…", [], "",
        """{"path":".grd file"?,"data":base64 bytes?,"group":string?=file name,"replace":bool=true (replace a group of the same name)}""",
        enabledProc(alwaysE(s)), runProc(importGrd(s, p)))]
