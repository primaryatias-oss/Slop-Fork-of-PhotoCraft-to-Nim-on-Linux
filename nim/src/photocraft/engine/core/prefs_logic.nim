## Preferences logic (the behaviour half of `prefs.rs`; the data is `prefs_model.nim`): dotted
## paths into the JSON form, validation of one value, set / reset with serde's errors, shortcut
## spelling, and the session's view of it all (`prefsView`, `setPref`, `applyPrefs`). The
## `prefs.*` commands live in `prefs_cmds.nim`. Part of the `engine/core` unit: included, not
## imported.

type PrefsError* = object of ValueError

proc prefsErr(msg: string): ref PrefsError = newException(PrefsError, msg)

const
  PrefSections* = [("general", "General"), ("interface", "Interface"), ("workspace", "Workspace"), ("tools", "Tools"),
                   ("historyLog", "History Log"), ("fileHandling", "File Handling"), ("export", "Export"),
                   ("performance", "Performance"), ("scratchDisks", "Scratch Disks"), ("cursors", "Cursors"),
                   ("transparencyAndGamut", "Transparency & Gamut"), ("unitsAndRulers", "Units & Rulers"),
                   ("guidesGridAndSlices", "Guides, Grid & Slices"), ("plugIns", "Plug-ins"), ("type", "Type"),
                   ("enhancedControls", "Enhanced Controls"), ("rawDefaults", "Camera Raw Defaults"),
                   ("integrations", "Integrations")]
    ## Preferences dialog sections in Photoshop's order: (id, title).
  HiddenUntilImplemented* = [
    "general.colorPicker", "general.animatedZoom", "general.zoomResizesWindows", "workspace.autoCollapseIconPanels",
    "workspace.autoShowHiddenPanels", "workspace.enableFloatingDocumentWindowDocking", "workspace.enableNarrowOptionsBar",
    "fileHandling.saveInBackground", "fileHandling.maximizePsdCompatibility", "performance.cacheLevels",
    "performance.effectCacheMb", "performance.legacyCompositing", "scratchDisks.disks", "unitsAndRulers.columnWidth",
    "unitsAndRulers.gutter", "plugIns.showExtensionPanels", "plugIns.allowScriptsToConnect", "plugIns.generatorEnabled",
    "type.missingGlyphProtection", "type.textEngine", "type.recentFonts", "enhancedControls.scrubbySliderAcceleration",
    "enhancedControls.touchGestures", "rawDefaults.colorSpace", "rawDefaults.bitDepth", "rawDefaults.resolution",
    "rawDefaults.sharpenFor", "rawDefaults.openAsSmartObject", "rawDefaults.applyAutoTone",
    "integrations.allowAgentControl", "integrations.controlPort"]
    ## Preferences that nothing reads yet, so Edit › Preferences doesn't show them.
  LinuxOnlyPrefs* = ["performance.linuxDisplayServer"]
  TemporaryTools* = [("tools.temporary.hand", "Hand Tool (hold)", "Space"),
                     ("tools.temporary.zoomIn", "Zoom In (hold, drag to scrub)", "Cmd+Space"),
                     ("tools.temporary.zoomOut", "Zoom Out (hold)", "Alt+Space"),
                     ("tools.temporary.erase", "Erase with Brush or Pencil (hold)", "`")]
    ## Hold-and-release gestures: bound like shortcuts but held rather than pressed.
  PressureCurveMaxPoints* = 16

proc isHiddenPref*(path: string): bool =
  path in HiddenUntilImplemented or (not defined(linux) and path in LinuxOnlyPrefs)

proc enumNames(T: typedesc[enum]): seq[string] =
  for v in T: result.add $v

proc prefChoices*(path: string): Option[seq[string]] =
  ## Choices of an enumerated preference (dotted path, e.g. `"cursors.painting"`).
  case path
  of "general.colorPicker": some(enumNames(ColorPickerKind))
  of "general.imageInterpolation": some(enumNames(Interpolation))
  of "interface.theme": some(enumNames(Theme))
  of "interface.appearanceMode": some(enumNames(AppearanceMode))
  of "interface.darkTheme": some(enumNames(DarkTheme))
  of "interface.lightTheme": some(enumNames(LightTheme))
  of "interface.canvasColor": some(enumNames(CanvasColor))
  of "interface.canvasBorder": some(enumNames(CanvasBorder))
  of "interface.uiScale": some(enumNames(UiScale))
  of "interface.uiFontSize": some(enumNames(UiFontSize))
  of "historyLog.destination": some(enumNames(LogDestination))
  of "historyLog.detail": some(enumNames(LogDetail))
  of "fileHandling.imagePreviews", "fileHandling.maximizePsdCompatibility": some(enumNames(Ask))
  of "export.quickExportFormat": some(enumNames(QuickExportFormat))
  of "export.quickExportLocation": some(enumNames(ExportLocation))
  of "export.metadata": some(enumNames(ExportMetadata))
  of "cursors.painting": some(enumNames(PaintingCursor))
  of "cursors.other": some(enumNames(OtherCursor))
  of "tools.rightClickWithPaintingTools": some(enumNames(RightClickPaint))
  of "transparencyAndGamut.gridSize": some(enumNames(CheckerSize))
  of "transparencyAndGamut.gridColors": some(enumNames(CheckerColors))
  of "unitsAndRulers.rulers", "guidesGridAndSlices.gridUnit": some(enumNames(Unit))
  of "unitsAndRulers.typeUnits": some(enumNames(TypeUnit))
  of "unitsAndRulers.pointSize": some(enumNames(PointSize))
  of "guidesGridAndSlices.guideStyle", "guidesGridAndSlices.gridStyle": some(enumNames(LineStyle))
  of "type.textEngine": some(enumNames(TextEngine))
  of "type.fontPreview": some(enumNames(FontPreview))
  of "rawDefaults.colorSpace": some(enumNames(RawColorSpace))
  of "rawDefaults.bitDepth": some(enumNames(RawDepth))
  of "rawDefaults.sharpenFor": some(enumNames(RawSharpen))
  of "performance.gpuBackend": some(enumNames(GpuBackend))
  of "performance.linuxDisplayServer": some(enumNames(LinuxDisplayServer))
  of "performance.renderingMode": some(enumNames(RenderingMode))
  else: none(seq[string])

proc prefRange*(path: string): Option[(float64, float64)] =
  ## Valid range of a numeric preference.
  case path
  of "fileHandling.autosaveMinutes": some((1.0, 240.0))
  of "fileHandling.recentFileCount": some((0.0, 100.0))
  of "fileHandling.rasterizeSvgGroupsDeeperThan": some((0.0, float64(MaxGroupDepth)))
  of "interface.notificationDurationSeconds": some((1.0, 120.0))
  of "export.jpegQuality", "export.webpQuality": some((1.0, 100.0))
  of "performance.memoryUsageMb": some((256.0, 1_048_576.0))
  of "performance.historyStates": some((1.0, 1000.0))
  of "performance.cacheLevels": some((1.0, 8.0))
  of "performance.cacheTileSize": some((256.0, 16384.0))
  of "performance.effectCacheMb": some((16.0, 65536.0))
  of "transparencyAndGamut.gamutWarningOpacity": some((1.0, 100.0))
  of "unitsAndRulers.columnWidth", "unitsAndRulers.gutter": some((0.0, 10_000.0))
  of "unitsAndRulers.printResolution", "unitsAndRulers.screenResolution", "rawDefaults.resolution": some((1.0, 10_000.0))
  of "guidesGridAndSlices.gridlineEvery": some((0.001, 10_000.0))
  of "guidesGridAndSlices.subdivisions": some((1.0, 100.0))
  of "type.recentFonts": some((0.0, 50.0))
  of "integrations.controlPort": some((0.0, 65535.0))
  else: none((float64, float64))

proc isColorPref*(path: string): bool =
  ## Is this preference a `#rrggbb` colour?
  if prefChoices(path).isSome or path.startsWith("shortcuts.") or path.startsWith("menus."): return false
  let i = path.rfind('.')
  let leaf = (if i >= 0: path[i + 1 .. ^1] else: path)
  leaf in ["guideColor", "smartGuideColor", "gridColor", "sliceColor", "canvasCustomColor", "brushPreviewColor",
           "gamutWarningColor", "customLight", "customDark"]

proc prefHex*(s: string): Option[array[3, uint8]] =
  ## `#rrggbb` (surrounding space and any number of `#` allowed).
  var t = unicode.strip(s)
  while t.startsWith("#"): t = t[1 .. ^1]
  if t.len != 6: return
  var outp: array[3, uint8]
  for i in 0..2:
    var part = t[2 * i ..< 2 * i + 2]
    if part.startsWith("+"): part = part[1 .. ^1]
    if part.len == 0: return
    var v = 0
    for ch in part:
      let d = (case ch
               of '0'..'9': ord(ch) - ord('0')
               of 'a'..'f': ord(ch) - ord('a') + 10
               of 'A'..'F': ord(ch) - ord('A') + 10
               else: -1)
      if d < 0: return
      v = v * 16 + d
    outp[i] = uint8(v)
  some(outp)

# ---------------------------------------------------------------- JSON paths

proc keyed(path: string): Option[(string, string)] =
  ## Map keys that are command ids (which contain dots): `shortcuts.<id>`, `menus.colors.<id>`.
  for prefix in ["shortcuts.", "menus.colors."]:
    if path.startsWith(prefix): return some((prefix[0 ..< prefix.len - 1], path[prefix.len .. ^1]))

proc getPath*(root: JsonNode; path: string): JsonNode =
  ## The value at a dotted path (`""` = everything), or nil.
  if root.isNil: return nil
  if path.len == 0: return root
  let k = keyed(path)
  if k.isSome:
    let m = getPath(root, k.get[0])
    return m.pget(k.get[1])
  result = root
  for part in path.split('.'):
    result = result.pget(part)
    if result.isNil: return nil

proc setPath(root: JsonNode; path: string; value: JsonNode) =
  ## Set `value` at `path` (creating keys only inside free-form maps such as `shortcuts`).
  let k = keyed(path)
  if k.isSome:
    var cur = root
    for part in k.get[0].split('.'):
      cur = cur.pget(part)
      if cur.isNil: raise prefsErr("unknown preference `" & path & "`")
    if cur.kind != JObject: raise prefsErr("`" & k.get[0] & "` is not a map")
    cur[k.get[1]] = value
    return
  var keys: seq[string]
  for part in path.split('.'):
    if part.len > 0: keys.add part
  if keys.len == 0: raise prefsErr("empty path")
  let last = keys[^1]
  let parents = keys[0 ..< keys.len - 1]
  var cur = root
  for part in parents:
    cur = cur.pget(part)
    if cur.isNil: raise prefsErr("unknown preference `" & path & "`")
  let freeMap = (parents.len > 0 and parents[0] == "shortcuts") or
                (parents.len == 2 and parents[0] == "menus" and parents[1] == "colors")
  if cur.kind != JObject: raise prefsErr("`" & path & "` is not inside a section")
  if not cur.fields.hasKey(last) and not freeMap: raise prefsErr("unknown preference `" & path & "`")
  cur[last] = value

proc capitalizeKey(s: string): string =
  if s.len == 0: return s
  let f = s[0]
  if f in Letters:
    let rest = s[1 .. ^1]
    var allDigits = rest.len > 0
    for c in rest:
      if c notin Digits: allDigits = false
    # Function keys stay upper-case (F1…F24); named keys are Title case.
    if f in {'f', 'F'} and allDigits: "F" & rest
    else: $f.toUpperAscii & rest.toLowerAscii
  else: s

proc normalizeShortcut*(s0: string): Option[string] =
  ## Canonical spelling of a shortcut: modifiers in `Cmd+Ctrl+Alt+Shift` order, key last (single
  ## characters upper-cased). None if it has no key or an unknown modifier.
  let s = unicode.strip(s0)
  if s.len == 0: return
  var (cmd, ctrl, alt, shift) = (false, false, false, false)
  var key = none(string)
  # A trailing "+" key is spelled "Cmd++".
  var body = s
  var plusKey = false
  if s.endsWith("++") or s == "+":
    while body.endsWith("+"): body.setLen(body.len - 1)
    plusKey = true
  for part in body.split('+'):
    if part.len == 0: continue
    case part.toLowerAscii
    of "cmd", "command", "⌘", "meta", "super": cmd = true
    of "ctrl", "control", "⌃": ctrl = true
    of "alt", "option", "opt", "⌥": alt = true
    of "shift", "⇧": shift = true
    else:
      if key.isSome: return
      key = some(if part.runeLen == 1: unicode.toUpper(part) else: capitalizeKey(part))
  if plusKey:
    if key.isSome: return
    key = some("+")
  if key.isNone: return
  var parts: seq[string]
  if cmd: parts.add "Cmd"
  if ctrl: parts.add "Ctrl"
  if alt: parts.add "Alt"
  if shift: parts.add "Shift"
  var r = parts.join("+")
  if r.len > 0: r.add '+'
  r.add key.get
  some(r)

proc checkValue(path: string; v: JsonNode) =
  ## Validate one value for `path` before it is stored (choices, ranges, colours).
  if path == "interface.favoriteThemes":
    if not v.isArray: raise prefsErr("`interface.favoriteThemes` must be a list of theme ids")
    if v.len > enumNames(Theme).len: raise prefsErr("`interface.favoriteThemes` contains too many themes")
    var seen: seq[string]
    for item in v.elems:
      let t = item.asStr
      if t.isNone or t.get notin enumNames(Theme): raise prefsErr("unknown favorite theme `" & jsonText(item) & "`")
      if t.get in seen: raise prefsErr("duplicate favorite theme `" & t.get & "`")
      seen.add t.get
  if path == "performance.renderingMode" and v.isNull: return # legacy policy, resolved from useGpu and gpuBackend
  let c = prefChoices(path)
  if c.isSome:
    let s = v.asStr
    if s.isNone: raise prefsErr("`" & path & "` must be one of " & c.get.join("|"))
    if s.get notin c.get: raise prefsErr("`" & path & "` must be one of " & c.get.join("|") & " (got `" & s.get & "`)")
  if path == "interface.language":
    var ok = false
    let s = v.asStr
    if s.isSome and s.get.len > 0 and s.get.len <= 16:
      ok = true
      for ch in s.get:
        if ch notin Letters + Digits + {'-'}: ok = false
    if not ok: raise prefsErr("`interface.language` must be `auto` or a language code such as `en` or `ja`")
  let r = prefRange(path)
  if r.isSome:
    let x = v.asF64
    if x.isNone: raise prefsErr("`" & path & "` must be a number")
    if not (x.get >= r.get[0] and x.get <= r.get[1]):
      raise prefsErr("`" & path & "` must be within " & displayF64(r.get[0]) & ".." & displayF64(r.get[1]) &
                     " (got " & displayF64(x.get) & ")")
  if isColorPref(path):
    let s = v.asStr
    if s.isNone or prefHex(s.get).isNone: raise prefsErr("`" & path & "` must be a #rrggbb colour")
  if path == "tools.pressureCurve":
    if not v.isArray: raise prefsErr("`tools.pressureCurve` must be a list of [input, output] points")
    if v.len < 2 or v.len > PressureCurveMaxPoints:
      raise prefsErr("`tools.pressureCurve` needs 2..=" & $PressureCurveMaxPoints & " points (got " & $v.len & ")")
    for p in v.elems:
      var ok = p.isArray and p.len == 2
      if ok:
        for x in p.elems:
          let f = x.asF64
          if f.isNone or not (f.get >= 0 and f.get <= 1): ok = false
      if not ok: raise prefsErr("`tools.pressureCurve` points are [input, output] pairs within 0..1 (got " & jsonText(p) & ")")
  if path.startsWith("shortcuts."):
    let sc = path["shortcuts.".len .. ^1]
    let s = v.asStr
    if s.isNone: raise prefsErr("shortcut for `" & sc & "` must be a string")
    if s.get.len > 0 and normalizeShortcut(s.get).isNone:
      raise prefsErr("`" & s.get & "` is not a valid shortcut (e.g. Cmd+Shift+N, F7)")

proc prefsFromJson(root: JsonNode; path: string; prefix = true): Preferences =
  ## `serde_json::from_value` of a whole preferences tree (serde's message on a bad value).
  result = defaultPrefs()
  try:
    withSerdeStrict: fromJ(root, result)
  except SerdeError as e:
    raise prefsErr(if prefix: "invalid value for `" & path & "`: " & e.msg else: e.msg)

proc getValue*(p: Preferences; path: string): JsonNode =
  ## Read a value by dotted path (`""` = everything); nil when there is none.
  getPath(toJ(p), path)

proc setValue*(p: var Preferences; path: string; value0: JsonNode) =
  ## Set a value by dotted path, validating it. Unknown paths and wrong types raise `PrefsError`.
  var value = value0
  if path.startsWith("shortcuts."):
    if value.kind == JString and value.str.len > 0:
      # Normalise shortcut spelling so conflicts compare equal.
      let n = normalizeShortcut(value.str)
      if n.isNone: raise prefsErr("`" & value.str & "` is not a valid shortcut")
      value = newJString(n.get)
    elif value.kind == JNull:
      # null = back to the default.
      p.shortcuts.del(path["shortcuts.".len .. ^1])
      return
  checkValue(path, value)
  let root = toJ(p)
  setPath(root, path, value)
  p = prefsFromJson(root, path)

proc resetPath*(p: var Preferences; path: Option[string]) =
  ## Reset one section (or one dotted path), or everything with none.
  if path.isNone or path.get.len == 0:
    p = defaultPrefs()
    return
  # These maps have no stored defaults; removing an override restores the fallback.
  let k = keyed(path.get)
  if k.isSome and k.get[0] == "shortcuts":
    p.shortcuts.del(k.get[1])
    return
  if k.isSome and k.get[0] == "menus.colors":
    p.menus.colors.del(k.get[1])
    return
  let def = defaultPrefs().getValue(path.get)
  if def.isNil: raise prefsErr("unknown preference `" & path.get & "`")
  if path.get == "shortcuts":
    p.shortcuts.clear()
    return
  let root = toJ(p)
  setPath(root, path.get, def)
  p = prefsFromJson(root, path.get, prefix = false)

proc shortcut*(p: Preferences; id: string; default: Option[string]): Option[string] =
  ## The shortcut in effect for a command: the user's override, else its default.
  if p.shortcuts.hasKey(id):
    let s = p.shortcuts[id]
    if s.len == 0: none(string) else: some(s)
  else: default

proc shortcutConflicts*(bindings: openArray[(string, string)]): seq[(string, seq[string])] =
  ## Shortcuts bound to more than one command: (shortcut, ids), sorted by shortcut.
  var by = initOrderedTable[string, seq[string]]()
  for (id, sc) in bindings:
    let n = normalizeShortcut(sc)
    if n.isSome: by.mgetOrPut(n.get, @[]).add id
  var keys: seq[string]
  for k in by.keys: keys.add k
  keys.sort(cmp[string])
  for k in keys:
    if by[k].len > 1: result.add (k, by[k])

proc fmtDecimals*(v: float64; decimals: int): string =
  ## Rounds `v` to `decimals` places and drops trailing zeros.
  let p = pow(10.0, float64(decimals))
  var r = round(v * p) / p
  if r == 0: r = 0
  var s = formatFloat(r, ffDecimal, decimals)
  if '.' notin s: return s
  while s.endsWith("0"): s.setLen(s.len - 1)
  if s.endsWith("."): s.setLen(s.len - 1)
  s

# ---------------------------------------------------------------- colour settings as serde data

proc serdeCamel*(T: typedesc[ColorSettings]): bool = true
proc defaultOf*(T: typedesc[ColorSettings]): ColorSettings = defaultColorSettings()

# ---------------------------------------------------------------- the session's preferences


proc applyPrefs*(s: var Session) =
  ## Push engine-relevant preferences into live state: every document's history limit and the
  ## layer-effect cache budget.
  let perf = s.prefs.values.performance
  for st in s.docs.mitems:
    st.history.maxStates = max(int(perf.historyStates), 1)
    st.history.maxBytes = int(perf.memoryUsageMb) * (1 shl 20)
    discard st.history.trim(st.doc[])
  setEffectCacheBudget(int(perf.effectCacheMb) shl 20)

proc editPrefs*(s: var Session; f: proc(p: var Preferences)) =
  ## Change preferences (bumps the revision) and apply the engine-side ones.
  inc s.prefs.rev
  f(s.prefs.values)
  s.applyPrefs()

proc prefsView*(s: Session): JsonNode =
  ## Full JSON view used by `prefs.get`: preferences plus `colorSettings`.
  result = toJ(s.prefs.values)
  result["colorSettings"] = toJ(s.color.settings)

proc syncTheme(next: var Preferences) =
  ## Old `interface.theme` clients still select a visible theme.
  case next.`interface`.theme
  of thPro, thProMedium, thStudio, thSolarizedDark, thAdwaitaDark, thBreezeDark:
    next.`interface`.darkTheme = (case next.`interface`.theme
                                  of thPro: dtPro
                                  of thProMedium: dtProMedium
                                  of thStudio: dtStudio
                                  of thSolarizedDark: dtSolarizedDark
                                  of thAdwaitaDark: dtAdwaitaDark
                                  else: dtBreezeDark)
    next.`interface`.appearanceMode = amDark
  of thStudioLight, thProLightGray, thProLight, thClassic, thAdwaita, thBreezeLight:
    next.`interface`.lightTheme = (case next.`interface`.theme
                                   of thStudioLight: ltStudioLight
                                   of thProLightGray: ltProLightGray
                                   of thProLight: ltProLight
                                   of thClassic: ltClassic
                                   of thAdwaita: ltAdwaita
                                   else: ltBreezeLight)
    next.`interface`.appearanceMode = amLight

proc setPref*(s: var Session; path: string; value: JsonNode) =
  ## Set one dotted path, routing `colorSettings.*` to the colour state. Raises `PrefsError`.
  if path.startsWith("colorSettings"):
    var cur = toJ(s.color.settings)
    var rest = path["colorSettings".len .. ^1]
    while rest.startsWith("."): rest = rest[1 .. ^1]
    if rest.len == 0: cur = value
    else: setPath(cur, rest, value)
    var next = defaultColorSettings()
    try:
      withSerdeStrict: fromJ(cur, next)
    except SerdeError as e: raise prefsErr("invalid value for `" & path & "`: " & e.msg)
    let why = validateSettings(next)
    if why.len > 0: raise prefsErr(why)
    s.color.settings = next
    inc s.prefs.rev
    return
  var next = s.prefs.values
  var section = path.len == 0 or path in ["shortcuts", "menus", "toolbar"]
  for (id, _) in PrefSections:
    if id == path: section = true
  if section:
    # Whole section (or everything): merge the object key by key so each value is checked.
    if value.isNil or value.kind != JObject: raise prefsErr("`" & path & "` needs an object")
    for k, v in value.sortedPairs:
      let p = if path.len == 0: k else: path & "." & k
      if path.len == 0 and k == "colorSettings":
        s.setPref("colorSettings", v)
        continue
      let cur = next.getValue(p)
      if v.kind == JObject and not p.startsWith("shortcuts") and not p.endsWith(".colors") and not cur.isNil and
          cur.kind == JObject:
        for k2, v2 in v.sortedPairs: next.setValue(p & "." & k2, v2)
      else: next.setValue(p, v)
  else: next.setValue(path, value)
  if path == "interface.theme" or next.`interface`.theme != s.prefs.values.`interface`.theme: syncTheme(next)
  inc s.prefs.rev
  s.prefs.values = next

proc loadPrefsJson*(s: var Session; text: string) =
  ## Restore preferences saved by the app. Missing keys keep their defaults and unknown keys are
  ## ignored, so files from older and newer versions load. Raises `PrefsError`.
  var v: JsonNode
  try: v = parseJson(text)
  except CatchableError as e: raise prefsErr("preferences: " & e.msg)
  if v.isObject:
    let iface = v.pget("interface")
    if iface.isObject and not iface.fields.hasKey("appearanceMode"):
      let t = iface.pget("theme").asStr
      if t.isSome and t.get in enumNames(Theme):
        if t.get in ["pro", "proMedium", "studio", "solarizedDark", "adwaitaDark", "breezeDark"]:
          iface["appearanceMode"] = %"dark"
          iface["darkTheme"] = %t.get
        else:
          iface["appearanceMode"] = %"light"
          iface["lightTheme"] = %t.get
  var color, presetsJ: JsonNode
  if v.isObject:
    color = v.pget("colorSettings")
    presetsJ = v.pget("presets")
    v.dropKey("colorSettings")
    v.dropKey("presets")
  var prefs = defaultPrefs()
  try:
    withSerdeStrict: fromJ(v, prefs)
  except SerdeError as e: raise prefsErr("preferences: " & e.msg)
  if not color.isNil:
    var c = defaultColorSettings()
    try:
      withSerdeStrict: fromJ(color, c)
    except SerdeError: c = defaultColorSettings()
    s.color.settings = c
    setTextGamma(c.blendTextGamma)
  inc s.prefs.rev
  s.prefs.values = prefs
  s.applyPrefs()
