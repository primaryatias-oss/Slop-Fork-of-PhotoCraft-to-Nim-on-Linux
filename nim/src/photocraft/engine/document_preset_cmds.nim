## Named New Document configurations and File › New › Recent (port of the commands of
## `document_preset_cmds.rs`; its `file.new` validation lives in the engine core). Selecting a
## preset only returns settings: document creation stays in `file.new`.

import std/[json, options, strutils, unicode, math]
import ./core

const
  MaxDocumentPresets = 256
  MaxRecentDocuments* = 20 ## File › New › Recent keeps this many settings, like Photoshop's Recent tab

proc serdeCamel*(T: typedesc[DocumentSettings]): bool = true

proc toJ*(d: DocumentSettings): JsonNode =
  ## serde's form: `backgroundColor` is left out when unset.
  result = %*{"width": d.width, "height": d.height, "resolution": d.resolution, "mode": d.mode, "depth": d.depth,
              "background": d.background}
  if d.backgroundColor.isSome: result["backgroundColor"] = toJ(d.backgroundColor.get)
  result["unit"] = %d.unit
  result["resolutionUnit"] = %d.resolutionUnit

proc toJ*(p: DocumentPreset): JsonNode = %*{"name": p.name, "settings": toJ(p.settings)}

proc validate(d: DocumentSettings): string =
  ## Why the settings can't create a document ("" when they can).
  if d.width < 1 or d.width > MaxDimension or d.height < 1 or d.height > MaxDimension:
    return "Width and height must be within 1..=" & $MaxDimension & " pixels"
  if d.resolution != d.resolution or abs(d.resolution) == Inf or d.resolution < 1 or d.resolution > float32(MaxResolution):
    return "Resolution must be within 1..=" & $int(MaxResolution) & " ppi"
  if colorModeParam(d.mode).isNone or sampleTypeParam(uint64(d.depth)).isNone:
    return "Use RGB, Grayscale, CMYK or Lab at 8, 16 or 32 bits"
  if d.background notin ["white", "black", "transparent", "backgroundColor"] and parseHex(d.background).isNone:
    return "Unknown background choice or invalid colour"
  if d.background == "backgroundColor" and d.backgroundColor.isNone:
    return "A saved Background Color needs its colour value"
  if d.backgroundColor.isSome:
    for v in d.backgroundColor.get:
      if v != v or abs(v) == Inf or v < 0 or v > 1: return "Background colour components must be within 0..1"
  if d.unit notin ["px", "in", "cm", "mm", "pt", "pica"] or d.resolutionUnit notin ["in", "cm"]:
    return "Unknown dimension or resolution display unit"
  ""

proc commandParams*(d: DocumentSettings): JsonNode =
  ## Creation params deliberately exclude both the preset name and display-only units.
  result = %*{"width": d.width, "height": d.height, "resolution": d.resolution, "mode": d.mode, "depth": d.depth,
              "background": d.background}
  if d.backgroundColor.isSome: result["backgroundColor"] = toJ(d.backgroundColor.get)

proc settingsFromParams(s: Session; p: JsonNode; cmdId: string): DocumentSettings =
  if not p.isObject: raise bad(cmdId, "`settings` must be an object")
  var fields = newJObject()
  for k, v in p.pairs: fields[k] = v
  for (k, v) in [("resolution", %72.0), ("mode", %"rgb"), ("depth", %8), ("background", %"white"), ("unit", %"px"),
                 ("resolutionUnit", %"in")]:
    if not fields.hasKey(k): fields[k] = v
  # Like file.new, accept pixel counts sent as floats by a UI, but refuse invalid input rather than
  # saving a different configuration.
  for key in ["width", "height"]:
    let n = fields.pget(key).asF64
    if n.isNone or n.get != n.get or abs(n.get) == Inf or n.get < 1 or n.get > float64(MaxDimension):
      raise bad(cmdId, "`" & key & "` must be within 1..=" & $MaxDimension & " pixels")
    fields[key] = %uint32(round(n.get))
  if fields.pget("mode").asStr == some("grayscale"): fields["mode"] = %"gray"
  if fields.pget("background").asStr == some("backgroundColor"):
    let c = backgroundColorParam(fields, s.tools.background, cmdId)
    fields["backgroundColor"] = %*[c[0], c[1], c[2]]
  try:
    withSerdeStrict: fromJ(fields, result)
  except SerdeError as e: raise bad(cmdId, "invalid settings: " & e.msg)
  let why = validate(result)
  if why.len > 0: raise bad(cmdId, why)

proc checkedName(name: string): Option[string] =
  let n = unicode.strip(name)
  if n.len == 0 or n.runeLen > 255: none(string) else: some(n)

proc loadDocumentPresets*(value: JsonNode): seq[DocumentPreset] =
  ## Read entries independently so a damaged preset cannot discard other presets or preferences.
  if not value.isArray: return
  for k, entry in value.elems:
    if k >= MaxDocumentPresets: break
    var preset: DocumentPreset
    try: fromJ(entry, preset)
    except CatchableError: continue
    let name = checkedName(preset.name)
    if name.isNone or validate(preset.settings).len > 0: continue
    var dup = false
    for p in result:
      if cmpIgnoreCase(p.name, name.get) == 0: dup = true
    if dup: continue
    preset.name = name.get
    if preset.settings.mode == "grayscale": preset.settings.mode = "gray"
    result.add preset

proc loadRecentDocuments*(value: JsonNode): seq[DocumentSettings] =
  ## Read the Recent list like `loadDocumentPresets`: a damaged entry is dropped, newest first.
  if not value.isArray: return
  for entry in value.elems:
    var settings: DocumentSettings
    try: fromJ(entry, settings)
    except CatchableError: continue
    if settings.mode == "grayscale": settings.mode = "gray"
    if validate(settings).len > 0: continue
    var dup = false
    for r in result:
      if r.commandParams == settings.commandParams: dup = true
    if dup: continue
    result.add settings
    if result.len >= MaxRecentDocuments: break

proc addRecent(s: var Session; p: JsonNode): JsonNode =
  ## Record settings a document was just created with: first in Recent, an equal older entry
  ## removed, the oldest dropped past the cap.
  const cmdId = "document.recent.add"
  let sv = p.pget("settings")
  let settings = settingsFromParams(s, (if sv.isNil: newJNull() else: sv), cmdId)
  var kept = @[settings]
  for r in s.presets.recentDocuments:
    if r.commandParams != settings.commandParams: kept.add r
  if kept.len > MaxRecentDocuments: kept.setLen(MaxRecentDocuments)
  s.presets.recentDocuments = kept
  s.presetsChanged()
  var a = newJArray()
  for r in s.presets.recentDocuments: a.add toJ(r)
  %*{"recent": a}

proc savePreset(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "document.presets.save"
  let name = checkedName(reqStr(p, "name", cmdId))
  if name.isNone: raise bad(cmdId, "Preset names must contain 1–255 characters after trimming")
  for pr in s.presets.documents:
    if cmpIgnoreCase(pr.name, name.get) == 0:
      raise bad(cmdId, "A preset named \"" & name.get & "\" already exists; choose another name")
  if s.presets.documents.len >= MaxDocumentPresets:
    raise bad(cmdId, "At most " & $MaxDocumentPresets & " document presets can be saved")
  let sv = p.pget("settings")
  let settings = settingsFromParams(s, (if sv.isNil: newJNull() else: sv), cmdId)
  let preset = DocumentPreset(name: name.get, settings: settings)
  result = %*{"preset": toJ(preset)}
  s.presets.documents.add preset
  s.presetsChanged()

proc findDocPreset(s: Session; name: string): int =
  for i, pr in s.presets.documents:
    if cmpIgnoreCase(pr.name, name) == 0: return i
  -1

proc getPreset(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "document.presets.get"
  let i = findDocPreset(s, reqStr(p, "name", cmdId))
  if i < 0: raise bad(cmdId, "No such document preset")
  let preset = s.presets.documents[i]
  %*{"preset": toJ(preset), "params": preset.settings.commandParams}

proc deletePreset(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "document.presets.delete"
  let name = reqStr(p, "name", cmdId)
  let i = findDocPreset(s, name)
  if i < 0: raise bad(cmdId, "No such document preset")
  s.presets.documents.delete(i)
  s.presetsChanged()
  %*{"deleted": name}

proc listPresets(s: Session): JsonNode =
  var a = newJArray()
  for pr in s.presets.documents: a.add toJ(pr)
  %*{"presets": a}

proc listRecent(s: Session): JsonNode =
  var a = newJArray()
  for r in s.presets.recentDocuments: a.add toJ(r)
  %*{"recent": a}

proc documentPresetSpecs*(): seq[CommandSpec] =
  @[cmd("document.presets.list", "List Document Presets", [], "", "{}", enabledProc(""), runProc(listPresets(s)), journal = false),
    cmd("document.presets.get", "Get Document Preset", [], "",
        """{"name":str} -> {preset,params} (params for file.new; no document is created)""", enabledProc(""),
        runProc(getPreset(s, p)), journal = false),
    cmd("document.presets.save", "Save Document Preset", [], "",
        """{"name":str (trimmed, ASCII case-insensitive unique, 1..255 characters),"settings":{"width":px,"height":px,"resolution":ppi=72,"mode":"rgb|gray|cmyk|lab"="rgb","depth":8|16|32=8,"background":"white|black|transparent|backgroundColor|#rrggbb"="white","backgroundColor":[r,g,b]?,"unit":"px|in|cm|mm|pt|pica"="px","resolutionUnit":"in|cm"="in"}}""",
        enabledProc(""), runProc(savePreset(s, p))),
    cmd("document.presets.delete", "Delete Document Preset", [], "", """{"name":str}""", enabledProc(""),
        runProc(deletePreset(s, p))),
    cmd("document.recent.list", "List Recent Document Settings", [], "",
        "{} -> {recent:[settings]} (newest first; each settings.command params minus unit/resolutionUnit go to file.new)",
        enabledProc(""), runProc(listRecent(s)), journal = false),
    cmd("document.recent.add", "Add Recent Document Settings", [], "",
        """{"settings":{same fields as document.presets.save}} (File › New records each document it creates; newest first, max 20, duplicates collapsed)""",
        enabledProc(""), runProc(addRecent(s, p)), journal = false)]
