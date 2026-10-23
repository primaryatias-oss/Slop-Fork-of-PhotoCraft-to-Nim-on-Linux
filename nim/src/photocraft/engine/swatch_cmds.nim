## Swatches panel commands (port of `swatch_cmds.rs` and the colour half of `presets/swatches.rs`):
## add, delete, rename, reorder and group swatches, set the foreground or background colour from
## one, restore the defaults, and import or export Photoshop `.aco` and Adobe Swatch Exchange
## `.ase` files.
##
## A swatch keeps its colour in the model it was made in (RGB, HSB, CMYK, Lab, grayscale, or a
## colour-book entry kept verbatim); turning it into the RGB the foreground colour holds goes
## through the CMS (`SwatchConv`). Swatches are addressed by name (unique across the library),
## optionally narrowed by `group`, or by `index` (position across all groups, in panel order).
## Like the other preset commands they change the preferences document, not a document's history.

import std/[json, options, os, strutils, unicode, math, algorithm, tables, sets]
import ./core
import ./preset_import_cmds
import ../cms
import ../color/convert as cconvert
import ../psd/swatches as psw
import ../paint/tile as ptile
import ../rmath

const
  MaxSwatches* = 100_000 ## most swatches the library holds (imports beyond it are refused)
  MaxGroups* = 10_000    ## most groups the library holds
  MaxName* = 255         ## longest swatch or group name kept (characters)

proc clip(s: string): string = runeSubStr(s, 0, MaxName)

proc total(groups: openArray[PresetGroup[Swatch]]): int =
  for g in groups: result += g.items.len

proc capLibrary*(groups: var seq[PresetGroup[Swatch]]) =
  ## Trim a hand-edited or imported library to the limits (never refuse to start over it).
  if groups.len > MaxGroups: groups.setLen(MaxGroups)
  var left = MaxSwatches
  for g in groups.mitems:
    if g.items.len > left: g.items.setLen(left)
    left -= g.items.len
    g.name = clip(g.name)
    for s in g.items.mitems: s.name = clip(s.name)

proc alwaysE(s: Session): string = ""
proc hasSwatchesE(s: Session): string = (if total(s.presets.swatches) > 0: "" else: "the Swatches panel is empty")
proc hasGroupsE(s: Session): string = (if s.presets.swatches.len == 0: "there are no swatch groups" else: "")

# ---------------------------------------------------------------- colours

proc finite(v: float32): float32 = (if v == v and abs(v) != Inf: v else: 0'f32)
proc unit(v: float32): float32 = clamp(finite(v), 0'f32, 1'f32)

proc model*(c: SwatchColor): string = $c.kind

proc displayValues*(c: SwatchColor): seq[float64] =
  ## Components in the units the Color Picker shows: RGB 0..255, HSB degrees and percent, CMYK
  ## and gray ink percent, Lab as is.
  proc r(v: float32; k: float64): float64 = round(float64(finite(v)) * k * 1000.0) / 1000.0
  case c.kind
  of swRgb:
    for v in c.v3: result.add r(v, 255.0)
  of swHsb: result = @[r(c.v3[0], 1.0), r(c.v3[1], 100.0), r(c.v3[2], 100.0)]
  of swCmyk:
    for v in c.v4: result.add r(v, 100.0)
  of swLab:
    for v in c.v3: result.add r(v, 1.0)
  of swGray: result = @[r(c.gray, 100.0)]
  of swBook:
    result.add float64(c.space)
    for v in c.values: result.add float64(v)

proc toJson*(c: SwatchColor): JsonNode =
  var vals = newJArray()
  for v in c.displayValues: vals.add %v
  %*{"model": c.model, "values": vals}

proc hexDigitsOnly(s: string): bool =
  for ch in s:
    if ch notin HexDigits: return false
  true

proc swatchColorParam*(v: JsonNode): SwatchColor =
  ## Parse a command's colour: `"#rrggbb"` (RGB working space), or
  ## `{"model":"rgb|hsb|cmyk|lab|gray","values":[…]}` in `displayValues` units. Raises
  ## `ValueError` with the message.
  if v.kind == JString:
    var h = trimU(v.str)
    while h.startsWith("#"): h = h[1 .. ^1]
    if h.len != 6 or not hexDigitsOnly(h):
      raise newException(ValueError, "bad colour `" & v.str & "` (\"#rrggbb\" or {\"model\",\"values\"})")
    proc b(i: int): float32 = float32(parseHexInt(h[i ..< i + 2])) / 255'f32
    return SwatchColor(kind: swRgb, v3: [b(0), b(2), b(4)])
  let model = v.pget("model").asStr
  if model.isNone: raise newException(ValueError, "a colour needs `model` (rgb|hsb|cmyk|lab|gray) and `values`")
  let arr = v.pget("values")
  if not arr.isArray: raise newException(ValueError, "a colour needs `values`")
  var vals: seq[float32]
  for x in arr.elems:
    let f = x.asF64
    if f.isNone or not (f.get == f.get and abs(f.get) != Inf):
      raise newException(ValueError, "colour values must be finite numbers")
    vals.add float32(f.get)
  proc need(n: int) =
    if vals.len != n:
      raise newException(ValueError, "a " & model.get & " colour has " & $n & " values, got " & $vals.len)
  proc pct(v: float32): float32 = clamp(v / 100'f32, 0'f32, 1'f32)
  case model.get
  of "rgb":
    need(3)
    SwatchColor(kind: swRgb, v3: [clamp(vals[0] / 255'f32, 0'f32, 1'f32), clamp(vals[1] / 255'f32, 0'f32, 1'f32),
                                  clamp(vals[2] / 255'f32, 0'f32, 1'f32)])
  of "hsb":
    need(3)
    SwatchColor(kind: swHsb, v3: [remEuclidF(vals[0], 360'f32), pct(vals[1]), pct(vals[2])])
  of "cmyk":
    need(4)
    SwatchColor(kind: swCmyk, v4: [pct(vals[0]), pct(vals[1]), pct(vals[2]), pct(vals[3])])
  of "lab":
    need(3)
    SwatchColor(kind: swLab, v3: [clamp(vals[0], 0'f32, 100'f32), clamp(vals[1], -128'f32, 127'f32),
                                  clamp(vals[2], -128'f32, 127'f32)])
  of "gray":
    need(1)
    SwatchColor(kind: swGray, gray: pct(vals[0]))
  else: raise newException(ValueError, "unknown colour model `" & model.get & "` (rgb|hsb|cmyk|lab|gray)")

type SwatchConv* = object
  ## Converts swatch colours to the RGB working space through the CMS (relative colorimetric +
  ## black point compensation, Photoshop's default). Build one per library or colour-settings
  ## change; evaluating is cheap.
  cmyk, lab, gray: Option[Transform]

proc working(s: Session; mode: ColorMode): Profile =
  let c = s.color.settings
  let spec = (case mode
              of cmCmyk: c.workingCmyk
              of cmGrayscale: c.workingGray
              else: c.workingRgb)
  try:
    let p = resolveProfile(spec, none(Document), some(mode))
    if p.colorSpace == modeSpace(mode): return p
  except CatchableError: discard
  workingProfile(mode)

proc newSwatchConv*(s: Session): SwatchConv =
  ## Conversions into the session's RGB working space from its CMYK and gray working spaces.
  let rgb = working(s, cmRgb)
  let opts = defaultTransformOptions()
  proc link(src: Profile): Option[Transform] =
    try: some(cached(src, rgb, opts))
    except CatchableError: none(Transform)
  SwatchConv(cmyk: link(working(s, cmCmyk)), lab: link(bLabD50.profile), gray: link(working(s, cmGrayscale)))

proc rgb*(conv: SwatchConv; c: SwatchColor): array[3, float32] =
  ## The colour in the RGB working space (what the foreground colour holds), 0..1. Colour-book
  ## entries, which have no colour of their own here, are mid gray.
  proc eval(t: Option[Transform]; input: openArray[float32]): Option[array[3, float32]] =
    if t.isNone: return
    var o: array[16, float32]
    t.get.eval(input, o)
    some([unit(o[0]), unit(o[1]), unit(o[2])])
  case c.kind
  of swRgb: [unit(c.v3[0]), unit(c.v3[1]), unit(c.v3[2])]
  of swHsb: hsbToRgb(c.v3)
  of swCmyk:
    let v = [unit(c.v4[0]), unit(c.v4[1]), unit(c.v4[2]), unit(c.v4[3])]
    eval(conv.cmyk, v).get(cconvert.cmykToRgb(v))
  of swLab:
    let (l, a, b) = (c.v3[0], c.v3[1], c.v3[2])
    # ICC v4 Lab encoding (the Lab D50 profile's device values).
    let enc = [unit(finite(l) / 100'f32), unit((finite(a) + 128'f32) / 255'f32), unit((finite(b) + 128'f32) / 255'f32)]
    eval(conv.lab, enc).get(cconvert.labToSrgb([finite(l), finite(a), finite(b)]))
  of swGray:
    let level = 1'f32 - unit(c.gray)
    eval(conv.gray, [level]).get([level, level, level])
  of swBook: [0.5'f32, 0.5, 0.5]

# ---------------------------------------------------------------- params and lookup

proc nameParam(p: JsonNode; k, cmdId: string): Option[string] =
  let v = p.pget(k)
  if v.isNull: return
  if v.kind == JString and trimU(v.str).len > 0: return some(clip(trimU(v.str)))
  raise bad(cmdId, "`" & k & "` must be a non-empty string")

proc locate(s: Session; p: JsonNode; cmdId: string): (int, int) =
  ## The swatch a command names: `"swatch"` (name, narrowed by `"group"`) or `"index"`.
  let groups = s.presets.swatches
  let name = strParam(p, "swatch")
  if name.isSome:
    let at = findPreset(groups, name.get, strParam(p, "group"))
    if at.isNone: raise bad(cmdId, "no swatch \"" & name.get & "\" (see swatches.list)")
    return at.get
  let iv = p.pget("index")
  if not iv.isNil:
    let i = iv.asU64
    if i.isNone: raise bad(cmdId, "`index` must be a non-negative integer")
    var left = i.get
    for gi, g in groups:
      if left < uint64(g.items.len): return (gi, int(left))
      left -= uint64(g.items.len)
    raise bad(cmdId, "no swatch at index " & $i.get & " (the panel has " & $total(groups) & ")")
  raise bad(cmdId, "give `swatch` (a name) or `index`")

proc indexByName(groups: openArray[PresetGroup[Swatch]]; group: Option[string]): Table[string, (int, int)] =
  ## Name → position of the first swatch with that name (optionally only in `group`).
  for gi, g in groups:
    if group.isSome and g.name != group.get: continue
    for ii, sw in g.items:
      if sw.name notin result: result[sw.name] = (gi, ii)

proc groupOf(s: Session; name, cmdId: string): int =
  for i, g in s.presets.swatches:
    if g.name == name: return i
  raise bad(cmdId, "no swatch group \"" & name & "\"")

proc swatchJson(conv: SwatchConv; sw: Swatch): JsonNode =
  result = sw.color.toJson
  result["name"] = %sw.name
  result["hex"] = %swatchHex(conv.rgb(sw.color))
  if sw.kind != skProcess: result["kind"] = %($sw.kind)

proc parseSwatchKind(k: string): Option[SwatchKind] =
  case k
  of "process", "normal": some(skProcess)
  of "global": some(skGlobal)
  of "spot": some(skSpot)
  else: none(SwatchKind)

# ---------------------------------------------------------------- commands

proc listSwatches(s: var Session; p: JsonNode): JsonNode =
  let conv = newSwatchConv(s)
  var groups = newJArray()
  for g in s.presets.swatches:
    var items = newJArray()
    for sw in g.items: items.add swatchJson(conv, sw)
    groups.add %*{"name": g.name, "swatches": items}
  %*{"groups": groups, "count": total(s.presets.swatches)}

proc addSwatch(s: var Session; p: JsonNode): JsonNode =
  const C = "swatches.add"
  let cv = p.pget("color")
  var color: SwatchColor
  if cv.isNull:
    let f = s.tools.foreground
    proc u(v: float32): float32 = (if v == v and abs(v) != Inf: clamp(v, 0'f32, 1'f32) else: 0'f32)
    color = SwatchColor(kind: swRgb, v3: [u(f[0]), u(f[1]), u(f[2])])
  else:
    try: color = swatchColorParam(cv)
    except ValueError as e: raise bad(C, e.msg)
  let k = strParam(p, "kind")
  var kind = skProcess
  if k.isSome:
    let pk = parseSwatchKind(k.get)
    if pk.isNone: raise bad(C, "unknown kind `" & k.get & "` (process|global|spot)")
    kind = pk.get
  if total(s.presets.swatches) >= MaxSwatches: raise bad(C, "the Swatches panel is full (" & $MaxSwatches & " swatches)")
  let group = nameParam(p, "group", C)
  let explicit = nameParam(p, "name", C)
  var name: string
  if explicit.isSome: name = uniqueName(s.presets.swatches, explicit.get)
  else:
    # Photoshop's default: "Color Swatch 1", "Color Swatch 2"…
    var taken = initHashSet[string]()
    for g in s.presets.swatches:
      for it in g.items: taken.incl it.name
    var i = 1
    while true:
      name = "Color Swatch " & $i
      if name notin taken: break
      inc i
  var gi = 0
  if group.isSome:
    gi = -1
    for i, x in s.presets.swatches:
      if x.name == group.get:
        gi = i
        break
    if gi < 0:
      if s.presets.swatches.len >= MaxGroups: raise bad(C, "too many swatch groups")
      s.presets.swatches.add newGroup(group.get, newSeq[Swatch]())
      gi = s.presets.swatches.high
  elif s.presets.swatches.len == 0: s.presets.swatches.add newGroup("Swatches", newSeq[Swatch]())
  let iv = p.pget("index")
  var at = s.presets.swatches[gi].items.len
  if not iv.isNull:
    let i = iv.asU64
    if i.isNone: raise bad(C, "`index` must be a non-negative integer")
    at = int(min(i.get, uint64(at)))
  s.presets.swatches[gi].items.insert(Swatch(name: name, color: color, kind: kind), at)
  let gname = s.presets.swatches[gi].name
  s.presetsChanged()
  %*{"name": name, "group": gname, "index": at}

proc deleteSwatch(s: var Session; p: JsonNode): JsonNode =
  const C = "swatches.delete"
  var targets: seq[(int, int)]
  let sv = p.pget("swatch")
  if sv.isArray:
    let ix = indexByName(s.presets.swatches, strParam(p, "group"))
    for n in sv.elems:
      if n.kind != JString: raise bad(C, "`swatch` names must be strings")
      if n.str notin ix: raise bad(C, "no swatch \"" & n.str & "\" (see swatches.list)")
      targets.add ix[n.str]
  else: targets.add locate(s, p, C)
  # Remove from the back so earlier positions stay valid; duplicates count once.
  targets.sort()
  var uniq: seq[(int, int)]
  for t in targets:
    if uniq.len == 0 or uniq[^1] != t: uniq.add t
  var names: seq[string]
  for k in countdown(uniq.high, 0):
    let (gi, ii) = uniq[k]
    if gi < s.presets.swatches.len and ii < s.presets.swatches[gi].items.len:
      names.add s.presets.swatches[gi].items[ii].name
      s.presets.swatches[gi].items.delete(ii)
  names.reverse()
  s.presetsChanged()
  %*{"deleted": names}

proc renameSwatch(s: var Session; p: JsonNode): JsonNode =
  const C = "swatches.rename"
  let (gi, ii) = locate(s, p, C)
  let to = nameParam(p, "name", C)
  if to.isNone: raise bad(C, "missing `name`")
  if s.presets.swatches[gi].items[ii].name != to.get and findPreset(s.presets.swatches, to.get, none(string)).isSome:
    raise bad(C, "a swatch named \"" & to.get & "\" already exists")
  s.presets.swatches[gi].items[ii].name = to.get
  s.presetsChanged()
  %*{"name": to.get}

proc moveSwatch(s: var Session; p: JsonNode): JsonNode =
  const C = "swatches.move"
  let (gi, ii) = locate(s, p, C)
  let t = nameParam(p, "to", C)
  let ti = (if t.isSome: groupOf(s, t.get, C) else: gi)
  let pv = p.pget("position")
  var at = none(uint64)
  if not pv.isNull:
    let i = pv.asU64
    if i.isNone: raise bad(C, "`position` must be a non-negative integer")
    at = i
  let item = s.presets.swatches[gi].items[ii]
  s.presets.swatches[gi].items.delete(ii)
  let n = s.presets.swatches[ti].items.len
  let pos = int(min(at.get(uint64(n)), uint64(n)))
  s.presets.swatches[ti].items.insert(item, pos)
  let gname = s.presets.swatches[ti].name
  s.presetsChanged()
  %*{"group": gname, "position": pos}

proc newSwatchGroup(s: var Session; p: JsonNode): JsonNode =
  const C = "swatches.newGroup"
  if s.presets.swatches.len >= MaxGroups: raise bad(C, "too many swatch groups")
  let base = nameParam(p, "name", C).get("Group")
  # Swatches to move into the new group (Photoshop: New Group from the selected swatches).
  var moving: seq[(int, int)]
  let a = p.pget("swatches")
  if not a.isNil:
    if not a.isArray: raise bad(C, "`swatches` must be a list of names")
    let ix = indexByName(s.presets.swatches, none(string))
    for n in a.elems:
      if n.kind != JString: raise bad(C, "`swatches` names must be strings")
      if n.str notin ix: raise bad(C, "no swatch \"" & n.str & "\"")
      moving.add ix[n.str]
  moving.sort()
  var uniq: seq[(int, int)]
  for t in moving:
    if uniq.len == 0 or uniq[^1] != t: uniq.add t
  let name = uniqueGroup(s.presets.swatches, base)
  var items: seq[Swatch]
  for k in countdown(uniq.high, 0):
    let (gi, ii) = uniq[k]
    items.add s.presets.swatches[gi].items[ii]
    s.presets.swatches[gi].items.delete(ii)
  items.reverse()
  let n = items.len
  s.presets.swatches.add newGroup(name, items)
  s.presetsChanged()
  %*{"group": name, "moved": n}

proc renameSwatchGroup(s: var Session; p: JsonNode): JsonNode =
  const C = "swatches.renameGroup"
  let g = nameParam(p, "group", C)
  if g.isNone: raise bad(C, "missing `group`")
  let to = nameParam(p, "name", C)
  if to.isNone: raise bad(C, "missing `name`")
  let i = groupOf(s, g.get, C)
  if g.get != to.get:
    for x in s.presets.swatches:
      if x.name == to.get: raise bad(C, "a group named \"" & to.get & "\" already exists")
  s.presets.swatches[i].name = to.get
  s.presetsChanged()
  %*{"group": to.get}

proc deleteSwatchGroup(s: var Session; p: JsonNode): JsonNode =
  const C = "swatches.deleteGroup"
  let g = nameParam(p, "group", C)
  if g.isNone: raise bad(C, "missing `group`")
  let kv = p.pget("keepSwatches")
  var keep = false
  if not kv.isNull:
    let b = kv.asBool
    if b.isNone: raise bad(C, "`keepSwatches` must be true or false")
    keep = b.get
  let i = groupOf(s, g.get, C)
  if keep and s.presets.swatches.len < 2: raise bad(C, "there is no other group to keep the swatches in")
  let removed = s.presets.swatches[i]
  s.presets.swatches.delete(i)
  let n = removed.items.len
  if keep:
    # Into the group above (or the new first group), like Photoshop's "Delete Group Only".
    let into = min(max(i - 1, 0), max(s.presets.swatches.len - 1, 0))
    if into < s.presets.swatches.len: s.presets.swatches[into].items.add removed.items
  s.presetsChanged()
  result = %*{"group": g.get}
  result[if keep: "kept" else: "deleted"] = %n

proc useSwatch(s: var Session; p: JsonNode): JsonNode =
  const C = "swatches.use"
  let (gi, ii) = locate(s, p, C)
  let target = strParam(p, "target").get("foreground")
  let sw = s.presets.swatches[gi].items[ii]
  let rgb = newSwatchConv(s).rgb(sw.color)
  let c = [rgb[0], rgb[1], rgb[2], 1'f32]
  case target
  of "foreground": s.tools.foreground = c
  of "background": s.tools.background = c
  else: raise bad(C, "unknown target `" & target & "` (foreground|background)")
  %*{"swatch": sw.name, "target": target, "hex": swatchHex(rgb)}

proc resetSwatches(s: var Session; p: JsonNode): JsonNode =
  const C = "swatches.reset"
  let av = p.pget("append")
  var append = false
  if not av.isNull:
    let b = av.asBool
    if b.isNone: raise bad(C, "`append` must be true or false")
    append = b.get
  var b = builtinSwatches()
  if append:
    if total(s.presets.swatches) + total(b) > MaxSwatches or s.presets.swatches.len + b.len > MaxGroups:
      raise bad(C, "the Swatches panel is full")
    for g in b.mitems:
      g.name = uniqueGroup(s.presets.swatches, g.name)
      for sw in g.items.mitems: sw.name = uniqueName(s.presets.swatches, sw.name)
      s.presets.swatches.add g
  else: s.presets.swatches = b
  s.presetsChanged()
  %*{"groups": s.presets.swatches.len, "count": total(s.presets.swatches)}

# ---------------------------------------------------------------- files

type SwatchFormat* = enum sfAco = "aco", sfAse = "ase"

proc parseSwatchFormat(s: string): Option[SwatchFormat] =
  var t = s
  while t.startsWith("."): t = t[1 .. ^1]
  case t.toLowerAscii
  of "aco": some(sfAco)
  of "ase": some(sfAse)
  else: none(SwatchFormat)

proc extFormat(path: Option[string]): Option[SwatchFormat] =
  if path.isNone: return
  let (_, name, ext) = splitFile(path.get)
  if ext.len < 2 or name.len == 0: return
  parseSwatchFormat(ext[1 .. ^1])

proc q16(v: float32): uint16 = uint16(round(unit(v) * 65535'f32))

proc fromAco(c: AcoColor): SwatchColor =
  proc f(v: uint16): float32 = float32(v) / 65535'f32
  case c.kind
  of ackRgb: SwatchColor(kind: swRgb, v3: [f(c.v[0]), f(c.v[1]), f(c.v[2])])
  of ackHsb: SwatchColor(kind: swHsb, v3: [f(c.v[0]) * 360'f32, f(c.v[1]), f(c.v[2])])
  of ackCmyk: SwatchColor(kind: swCmyk, v4: [f(c.v[0]), f(c.v[1]), f(c.v[2]), f(c.v[3])])
  of ackLab: SwatchColor(kind: swLab, v3: [float32(min(c.v[0], 10000'u16)) / 100'f32, float32(c.labA) / 100'f32,
                                         float32(c.labB) / 100'f32])
  of ackGray: SwatchColor(kind: swGray, gray: 1'f32 - float32(min(c.v[0], 10000'u16)) / 10000'f32)
  of ackOther: SwatchColor(kind: swBook, space: c.space, values: c.v)

proc toAco(c: SwatchColor): AcoColor =
  case c.kind
  of swRgb: AcoColor(kind: ackRgb, space: acoRgb, v: [q16(c.v3[0]), q16(c.v3[1]), q16(c.v3[2]), 0])
  of swHsb:
    let h = (if c.v3[0] == c.v3[0] and abs(c.v3[0]) != Inf: remEuclidF(c.v3[0], 360'f32) else: 0'f32)
    AcoColor(kind: ackHsb, space: acoHsb, v: [uint16(min(round(h / 360'f32 * 65535'f32), 65535'f32)), q16(c.v3[1]), q16(c.v3[2]), 0])
  of swCmyk: AcoColor(kind: ackCmyk, space: acoCmyk, v: [q16(c.v4[0]), q16(c.v4[1]), q16(c.v4[2]), q16(c.v4[3])])
  of swLab:
    proc ab(v: float32): int16 =
      if v == v and abs(v) != Inf: int16(clamp(round(v * 100'f32), -12800'f32, 12700'f32)) else: 0'i16
    let l = (if c.v3[0] == c.v3[0] and abs(c.v3[0]) != Inf: c.v3[0] else: 0'f32)
    AcoColor(kind: ackLab, space: acoLab, v: [uint16(clamp(round(l * 100'f32), 0'f32, 10000'f32)),
                                              cast[uint16](ab(c.v3[1])), cast[uint16](ab(c.v3[2])), 0])
  of swGray: AcoColor(kind: ackGray, space: acoGray, v: [uint16(round((1'f32 - unit(c.gray)) * 10000'f32)), 0, 0, 0])
  of swBook: AcoColor(kind: ackOther, space: c.space, v: c.values)

proc fromAse(c: AseColor): Option[SwatchColor] =
  case c.kind
  of ascRgb: some(SwatchColor(kind: swRgb, v3: [c.c[0], c.c[1], c.c[2]]))
  of ascCmyk: some(SwatchColor(kind: swCmyk, v4: [c.c[0], c.c[1], c.c[2], c.c[3]]))
  # L is stored as a fraction (L*/100); a few writers store L* itself, which can only be told
  # apart above 1.
  of ascLab: some(SwatchColor(kind: swLab, v3: [(if c.c[0] > 1'f32: c.c[0] else: c.c[0] * 100'f32), c.c[1], c.c[2]]))
  of ascGray: some(SwatchColor(kind: swGray, gray: 1'f32 - c.c[0]))
  of ascOther: none(SwatchColor)

proc toAse(c: SwatchColor): Option[AseColor] =
  case c.kind
  of swRgb: some(AseColor(kind: ascRgb, c: @[c.v3[0], c.v3[1], c.v3[2]]))
  # ASE has no HSB; HSB is a view of RGB, so this is exact up to float rounding.
  of swHsb:
    let v = hsbToRgb(c.v3)
    some(AseColor(kind: ascRgb, c: @[v[0], v[1], v[2]]))
  of swCmyk: some(AseColor(kind: ascCmyk, c: @[c.v4[0], c.v4[1], c.v4[2], c.v4[3]]))
  of swLab: some(AseColor(kind: ascLab, c: @[c.v3[0] / 100'f32, c.v3[1], c.v3[2]]))
  of swGray: some(AseColor(kind: ascGray, c: @[1'f32 - c.gray]))
  of swBook: none(AseColor)

proc kindFromAse(k: AseKind): SwatchKind =
  case k
  of akGlobal: skGlobal
  of akSpot: skSpot
  of akProcess: skProcess

proc kindToAse(k: SwatchKind): AseKind =
  case k
  of skGlobal: akGlobal
  of skSpot: akSpot
  of skProcess: akProcess

proc readSwatchFile*(bytes: openArray[byte]; format: SwatchFormat; group: string): (seq[PresetGroup[Swatch]], seq[string]) =
  ## Parse a swatch file into groups (`group` names the group for ungrouped colours). Raises
  ## `ValueError` with the message.
  var warnings: seq[string]
  var outp: seq[PresetGroup[Swatch]]
  proc fallback(i: int; name: string): string =
    if trimU(name).len == 0: "Swatch " & $(i + 1) else: clip(trimU(name))
  case format
  of sfAco:
    var f: AcoFile
    try: f = parseAco(bytes)
    except CatchableError as e: raise newException(ValueError, "not a readable .aco swatch file: " & e.msg)
    var books = 0
    for sw in f.swatches:
      if sw.color.kind == ackOther: inc books
    if books > 0:
      warnings.add $books & " swatches use a colour book or colour space PhotoCraft can't show; they are kept (shown gray) and saved back unchanged"
    var items: seq[Swatch]
    for i, sw in f.swatches: items.add Swatch(name: fallback(i, sw.name), color: fromAco(sw.color), kind: skProcess)
    outp.add newGroup(group, items)
  of sfAse:
    var f: AseFile
    try: f = parseAse(bytes)
    except CatchableError as e: raise newException(ValueError, "not a readable .ase swatch file: " & e.msg)
    var skipped = 0
    var n = 0
    proc conv(sw: AseSwatch): Option[Swatch] =
      inc n
      let c = fromAse(sw.color)
      if c.isSome: return some(Swatch(name: fallback(n - 1, sw.name), color: c.get, kind: kindFromAse(sw.kind)))
      inc skipped
    var loose: seq[Swatch]
    for e in f.entries:
      if not e.isGroup:
        let c = conv(e.swatch)
        if c.isSome: loose.add c.get
      else:
        var items: seq[Swatch]
        for sw in e.swatches:
          let c = conv(sw)
          if c.isSome: items.add c.get
        let name = (if trimU(e.name).len == 0: group else: clip(trimU(e.name)))
        outp.add newGroup(name, items)
    if loose.len > 0 or outp.len == 0: outp.insert(newGroup(group, loose), 0)
    if skipped > 0: warnings.add $skipped & " swatches in an unknown colour model were skipped"
  if total(outp) == 0: raise newException(ValueError, "the file holds no swatches")
  (outp, warnings)

proc writeSwatchFile*(groups: openArray[PresetGroup[Swatch]]; format: SwatchFormat): (seq[byte], int, seq[string]) =
  ## Encode groups as a swatch file: the bytes, the number of swatches written and warnings about
  ## what the format cannot hold. Raises `ValueError` with the message.
  var warnings: seq[string]
  case format
  of sfAco:
    var swatches: seq[AcoSwatch]
    var special = false
    for g in groups:
      for sw in g.items:
        swatches.add AcoSwatch(name: sw.name, color: toAco(sw.color))
        if sw.kind != skProcess: special = true
    if groups.len > 1: warnings.add ".aco files have no groups; the swatches were saved as one list"
    if special: warnings.add ".aco files don't mark spot or global colours; save as .ase to keep them"
    var bytes: seq[byte]
    try: bytes = writeAco(swatches)
    except CatchableError as e: raise newException(ValueError, e.msg)
    (bytes, swatches.len, warnings)
  of sfAse:
    var books = 0
    var entries: seq[AseEntry]
    var n = 0
    for g in groups:
      var swatches: seq[AseSwatch]
      for sw in g.items:
        let c = toAse(sw.color)
        if c.isSome: swatches.add AseSwatch(name: sw.name, color: c.get, kind: kindToAse(sw.kind))
        else: inc books
      n += swatches.len
      entries.add AseEntry(isGroup: true, name: g.name, swatches: swatches)
    if books > 0:
      warnings.add $books & " colour-book swatches can't be stored in an .ase file and were left out (save as .aco to keep them)"
    var bytes: seq[byte]
    try: bytes = writeAse(AseFile(entries: entries))
    except CatchableError as e: raise newException(ValueError, e.msg)
    (bytes, n, warnings)

proc importSwatches(s: var Session; p: JsonNode): JsonNode =
  const C = "swatches.import"
  let (bytes, stem) = fileBytes(p, C, ".aco or .ase file")
  var format: SwatchFormat
  let fv = strParam(p, "format")
  if fv.isSome:
    let f = parseSwatchFormat(fv.get)
    if f.isNone: raise bad(C, "unknown format `" & fv.get & "` (aco|ase)")
    format = f.get
  elif bytes.len >= 4 and bytes[0 ..< 4] == @[byte('A'), byte('S'), byte('E'), byte('F')]: format = sfAse
  elif bytes.len >= 2 and bytes[0] == 0 and bytes[1] in {1'u8, 2'u8}: format = sfAco
  else:
    let f = extFormat(strParam(p, "path"))
    if f.isNone: raise bad(C, "not an .aco or .ase swatch file")
    format = f.get
  let mode = strParam(p, "mode").get("append")
  let replace = case mode
    of "append": false
    of "replace": true
    else: raise bad(C, "unknown mode `" & mode & "` (append|replace)")
  let g = nameParam(p, "group", C)
  let group = (if g.isSome: g.get elif trimU(stem).len > 0: clip(trimU(stem)) else: "Imported Swatches")
  var groups: seq[PresetGroup[Swatch]]
  var warnings: seq[string]
  try: (groups, warnings) = readSwatchFile(bytes, format, group)
  except ValueError as e: raise bad(C, e.msg)
  var lib = (if replace: newSeq[PresetGroup[Swatch]]() else: s.presets.swatches)
  if total(lib) + total(groups) > MaxSwatches or lib.len + groups.len > MaxGroups:
    raise bad(C, "too many swatches: the panel holds at most " & $MaxSwatches & " in " & $MaxGroups & " groups")
  let count = total(groups)
  var names = newJArray()
  # Names stay unique across the library (agents address swatches by name).
  var next = initTable[string, int]()
  var taken = initHashSet[string]()
  for lg in lib:
    for it in lg.items: taken.incl it.name
  for g0 in groups:
    var g = g0
    g.name = uniqueGroup(lib, g.name)
    for sw in g.items.mitems:
      if sw.name in taken:
        # Resume numbering per base name, so a file of identical names stays linear.
        var i = next.getOrDefault(sw.name, 2)
        while (sw.name & " " & $i) in taken: inc i
        next[sw.name] = i + 1
        sw.name = sw.name & " " & $i
      taken.incl sw.name
    names.add %g.name
    lib.add g
  s.presets.swatches = lib
  s.presetsChanged()
  %*{"format": $format, "mode": mode, "groups": names, "count": count, "warnings": warnings}

proc exportSwatches(s: var Session; p: JsonNode): JsonNode =
  const C = "swatches.export"
  let path = strParam(p, "path")
  var format = sfAco
  let fv = strParam(p, "format")
  if fv.isSome:
    let f = parseSwatchFormat(fv.get)
    if f.isNone: raise bad(C, "unknown format `" & fv.get & "` (aco|ase)")
    format = f.get
  else: format = extFormat(path).get(sfAco)
  let g = nameParam(p, "group", C)
  var groups = (if g.isSome: @[s.presets.swatches[groupOf(s, g.get, C)]] else: s.presets.swatches)
  # Only some swatches (Photoshop: Export Selected Swatches).
  let a = p.pget("swatches")
  if not a.isNil:
    if not a.isArray: raise bad(C, "`swatches` must be a list of names")
    var want = initHashSet[string]()
    let ix = indexByName(groups, none(string))
    for n in a.elems:
      if n.kind != JString: raise bad(C, "`swatches` names must be strings")
      if n.str notin ix: raise bad(C, "no swatch \"" & n.str & "\"")
      want.incl n.str
    var kept: seq[PresetGroup[Swatch]]
    for g in groups.mitems:
      var items: seq[Swatch]
      for sw in g.items:
        if sw.name in want: items.add sw
      g.items = items
      if g.items.len > 0: kept.add g
    groups = kept
  if total(groups) == 0: raise bad(C, "no swatches to export")
  var bytes: seq[byte]
  var count: int
  var warnings: seq[string]
  try: (bytes, count, warnings) = writeSwatchFile(groups, format)
  except ValueError as e: raise bad(C, e.msg)
  result = %*{"format": $format, "count": count, "bytes": bytes.len, "warnings": warnings}
  if path.isSome:
    writeFileAtomic(path.get, bytes)
    result["path"] = %path.get
  else: result["data"] = %b64Encode(bytes)

proc swatchSpecs*(): seq[CommandSpec] =
  const
    Color = """"#rrggbb"|{"model":"rgb|hsb|cmyk|lab|gray","values":[rgb 0..255 | hue°,s%,b% | c%,m%,y%,k% | L,a,b | k%]}"""
    Target = """"swatch":name (or "index":n across all groups),"group":name? (narrows the name lookup)"""
  @[cmd("swatches.list", "Swatches", [], "",
        """{} → {groups:[{name,swatches:[{name,model,values,hex,kind?}]}],count}. values are in Color Picker units; hex is the colour in the RGB working space.""",
        enabledProc(alwaysE(s)), runProc(listSwatches(s, p)), journal = false),
    cmd("swatches.add", "New Swatch…", [], "",
        """{"color":""" & Color & """?=foreground,"name":str?="Color Swatch N","group":name?=first (created when missing),"index":n?=end,"kind":"process|global|spot"="process"} → {name,group,index}""",
        enabledProc(alwaysE(s)), runProc(addSwatch(s, p))),
    cmd("swatches.delete", "Delete Swatch", [], "",
        "{" & Target & """; "swatch" may be a list of names} → {deleted:[names]}""",
        enabledProc(hasSwatchesE(s)), runProc(deleteSwatch(s, p))),
    cmd("swatches.rename", "Rename Swatch…", [], "", "{" & Target & ""","name":str} → {name}""",
        enabledProc(hasSwatchesE(s)), runProc(renameSwatch(s, p))),
    cmd("swatches.move", "Move Swatch", [], "",
        "{" & Target & ""","to":group?=its own group,"position":n?=end} → {group,position}""",
        enabledProc(hasSwatchesE(s)), runProc(moveSwatch(s, p))),
    cmd("swatches.newGroup", "New Swatch Group…", [], "",
        """{"name":str="Group" (made unique),"swatches":[names]? (moved into the new group)} → {group,moved}""",
        enabledProc(alwaysE(s)), runProc(newSwatchGroup(s, p))),
    cmd("swatches.renameGroup", "Rename Swatch Group…", [], "", """{"group":name,"name":str} → {group}""",
        enabledProc(hasGroupsE(s)), runProc(renameSwatchGroup(s, p))),
    cmd("swatches.deleteGroup", "Delete Swatch Group", [], "",
        """{"group":name,"keepSwatches":bool=false (move its swatches into the group above)} → {group,deleted|kept}""",
        enabledProc(hasGroupsE(s)), runProc(deleteSwatchGroup(s, p))),
    cmd("swatches.use", "Use Swatch", [], "",
        "{" & Target & ""","target":"foreground|background"="foreground"} → {swatch,target,hex}. CMYK, Lab and gray swatches are converted to the RGB working space with colour management.""",
        enabledProc(hasSwatchesE(s)), runProc(useSwatch(s, p))),
    cmd("swatches.reset", "Reset Swatches", [], "",
        """{"append":bool=false (add the defaults instead of replacing the library)} → {groups,count}""",
        enabledProc(alwaysE(s)), runProc(resetSwatches(s, p))),
    cmd("swatches.import", "Import Swatches…", [], "",
        """{"path":".aco or .ase file"?,"data":base64 bytes?,"format":"aco|ase"? (detected),"mode":"append|replace"="append","group":str?=file name (the group for ungrouped colours)} → {format,mode,groups,count,warnings}""",
        enabledProc(alwaysE(s)), runProc(importSwatches(s, p))),
    cmd("swatches.export", "Export Swatches…", [], "",
        """{"format":"aco|ase"?=from path, else aco,"path":file? (else the bytes come back as base64 `data`),"group":name? (only that group),"swatches":[names]? (only these)} → {format,count,bytes,warnings,path|data}""",
        enabledProc(hasSwatchesE(s)), runProc(exportSwatches(s, p)))]
