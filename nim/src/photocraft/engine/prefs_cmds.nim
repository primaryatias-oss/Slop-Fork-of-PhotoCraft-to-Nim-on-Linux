## Preference commands (the command half of `prefs.rs`, plus `kys.rs`): `prefs.get` / `prefs.set`
## / `prefs.reset`, the `edit.preferences.<section>` items, Edit › Keyboard Shortcuts (with
## Photoshop `.kys` sets), Edit › Menus and Edit › Toolbar.
##
## `.kys` files are read with `std/xmlparser`; a malformed file's error wording differs from the
## Rust build's XML parser (roxmltree).

import std/[json, options, strutils, unicode, tables, algorithm, xmlparser, xmltree, streams, strtabs]
import ./core
import ../compose/psblend

proc alwaysE(s: Session): string = ""

proc prefsGet(s: var Session; p: JsonNode): JsonNode =
  let path = p.pget("path").asStr.get("")
  result = getPath(s.prefsView, path)
  if result.isNil: raise bad("prefs.get", "unknown preference `" & path & "`")

proc prefsSet(s: var Session; p: JsonNode): JsonNode =
  const C = "prefs.set"
  var changes: seq[(string, JsonNode)]
  let path = p.pget("path").asStr
  if path.isSome:
    let v = p.pget("value")
    if v.isNil: raise bad(C, "missing `value`")
    changes.add (path.get, v)
  let m = p.pget("values")
  if m.isObject:
    for k, v in m.sortedPairs: changes.add (k, v)
  if changes.len == 0: raise bad(C, "give `path` and `value`, or `values: {path: value, …}`")
  # All or nothing: validate against a scratch copy first.
  let (savedPrefs, savedColor) = (s.prefs.values, s.color.settings)
  for (path, v) in changes:
    try: s.setPref(path, v)
    except PrefsError as e:
      inc s.prefs.rev
      s.prefs.values = savedPrefs
      s.color.settings = savedColor
      raise bad(C, e.msg)
  # `colorSettings.blendTextGamma` lives in the compositor too, as `edit.colorSettings` sets it.
  setTextGamma(s.color.settings.blendTextGamma)
  s.applyPrefs()
  let view = s.prefsView
  result = newJObject()
  for (k, _) in changes:
    let v = getPath(view, k)
    result[k] = (if v.isNil: newJNull() else: v)

proc prefsReset(s: var Session; p: JsonNode): JsonNode =
  const C = "prefs.reset"
  var pv = p.pget("path")
  if pv.isNil: pv = p.pget("section")
  let path = pv.asStr
  if path.isSome and path.get == "colorSettings": s.color.settings = defaultColorSettings()
  elif path.isSome and path.get.startsWith("colorSettings."):
    # One colour setting: take the default from the colour settings and route it like `prefs.set`.
    let key = path.get["colorSettings.".len .. ^1]
    let def = getPath(toJ(defaultColorSettings()), key)
    if def.isNil: raise bad(C, "unknown preference `" & path.get & "`")
    try: s.setPref(path.get, def)
    except PrefsError as e: raise bad(C, e.msg)
  elif path.isNone:
    s.color.settings = defaultColorSettings()
    s.editPrefs(proc(pr: var Preferences) = pr.resetPath(none(string)))
  else:
    var next = s.prefs.values
    try: next.resetPath(path)
    except PrefsError as e: raise bad(C, e.msg)
    s.editPrefs(proc(pr: var Preferences) = pr = next)
  inc s.prefs.rev
  setTextGamma(s.color.settings.blendTextGamma)
  s.applyPrefs()
  if path.isSome and (path.get.startsWith("shortcuts.") or path.get.startsWith("menus.colors.")):
    # The removed override is absent, so reading its old path would report an error.
    newJNull()
  else: prefsGet(s, %*{"path": path.get("")})

proc preferencesSection(s: var Session; section: string): JsonNode =
  ## `edit.preferences.<section>`: the section's values (the GUI opens the dialog on it instead).
  var title = "General"
  for (id, t) in PrefSections:
    if id == section: title = t
  let values = s.prefs.values.getValue(section)
  %*{"section": section, "title": title, "values": (if values.isNil: newJNull() else: values)}

# ---------------------------------------------------------------- .kys shortcut sets (kys.rs)

const KysMaxBytes = 4 shl 20 ## a set is a few tens of kilobytes; anything far larger is not one

type
  KysCommand = object
    name: string
    shortcuts: seq[string]
  KysSet = object
    name: Option[string]
    commands: seq[KysCommand]
    toolKeys: int
  KysPlan = object
    set: OrderedTable[string, string] ## command id → normalised shortcut (sorted by id)
    unknown, unreadable: seq[string]
    alternates: int
  KysCandidate = object
    id, label: string
    default: Option[string]

proc localTag(n: XmlNode): string =
  let t = n.tag
  let i = t.find(':')
  if i >= 0: t[i + 1 .. ^1] else: t

proc parseKys(text0: string): KysSet =
  ## Parse a `.kys` file (or Photoshop's live `Keyboard Shortcuts.psp`, the same XML). Rows
  ## without a name or a shortcut are skipped. Raises `ValueError`.
  if text0.len > KysMaxBytes: raise newException(ValueError, "not a keyboard shortcut set (.kys): larger than 4 MiB")
  var text = text0
  while text.startsWith("\xEF\xBB\xBF"): text = text[3 .. ^1]
  var errors: seq[string]
  var root: XmlNode
  try: root = parseXml(newStringStream(text), "kys", errors)
  except CatchableError as e: raise newException(ValueError, "not a keyboard shortcut set (.kys): " & e.msg)
  if errors.len > 0: raise newException(ValueError, "not a keyboard shortcut set (.kys): " & errors[0])
  if root.isNil or root.kind != xnElement or root.localTag != "photoshop-keyboard-shortcuts":
    raise newException(ValueError, "not a keyboard shortcut set (.kys): no <photoshop-keyboard-shortcuts> root")
  if not root.attrs.isNil and root.attrs.hasKey("filename"):
    let f = root.attrs["filename"]
    var cut = -1
    for i, c in f:
      if c in {'=', '/'}: cut = i
    let n = unicode.strip(if cut >= 0: f[cut + 1 .. ^1] else: f)
    if n.len > 0: result.name = some(n)
  for node in root:
    if node.kind != xnElement: continue
    case node.localTag
    of "command":
      if node.attrs.isNil or not node.attrs.hasKey("name"): continue
      let name = unicode.strip(node.attrs["name"])
      if name.len == 0: continue
      var shortcuts: seq[string]
      for c in node:
        if c.kind == xnElement and c.localTag == "shortcut":
          let t = unicode.strip(c.innerText)
          if t.len > 0: shortcuts.add t
      if shortcuts.len > 0: result.commands.add KysCommand(name: name, shortcuts: shortcuts)
    of "tool":
      if unicode.strip(node.innerText).len > 0: inc result.toolKeys
    else: discard

proc kysKey(label: string): string =
  ## Photoshop's label and ours compared loosely: case, a trailing ellipsis and surrounding space
  ## do not count.
  var l = unicode.strip(label).replace("...", "…")
  while l.endsWith("…"): l.setLen(l.len - "…".len)
  unicode.toLower(unicode.strip(l))

proc commandCandidates(): seq[KysCandidate] =
  ## The engine's own candidates: every command in a menu, in command order.
  for c in commandSpecs():
    if c.menu.len > 0: result.add KysCandidate(id: c.id, label: c.label, default: c.shortcut)

proc planKys(candidates: seq[KysCandidate]; set: KysSet): KysPlan =
  var byLabel = initTable[string, seq[(string, Option[string])]]()
  for c in candidates:
    let k = kysKey(c.label)
    var entry = byLabel.getOrDefault(k)
    var dup = false
    for (id, _) in entry:
      if id == c.id: dup = true
    if not dup:
      entry.add (c.id, c.default)
      byLabel[k] = entry
  var picked: seq[(string, string)]
  for row in set.commands:
    let k = kysKey(row.name)
    if k notin byLabel:
      result.unknown.add row.name
      continue
    let cands = byLabel[k]
    let sc = normalizeShortcut(row.shortcuts[0])
    if sc.isNone:
      result.unreadable.add row.name
      continue
    result.alternates += max(row.shortcuts.len - 1, 0)
    var imported: seq[string]
    for s in row.shortcuts:
      let n = normalizeShortcut(s)
      if n.isSome: imported.add n.get
    var pick = -1
    for i, (_, d) in cands:
      if d.isSome:
        let n = normalizeShortcut(d.get)
        if n.isSome and n.get in imported:
          pick = i
          break
    if pick < 0:
      for i, (_, d) in cands:
        if d.isSome:
          pick = i
          break
    if pick < 0 and cands.len > 0: pick = 0
    if pick >= 0:
      let (id, default) = cands[pick]
      # A row that lists the command's own default among its keys keeps the default.
      let d = (if default.isSome: normalizeShortcut(default.get) else: none(string))
      picked.add (id, (if d.isSome and d.get in imported: d.get else: sc.get))
  var tbl = initOrderedTable[string, string]()
  for (id, sc) in picked: tbl[id] = sc
  tbl.sort(proc(a, b: (string, string)): int = cmp(a[0], b[0]))
  result.set = tbl

# ---------------------------------------------------------------- Keyboard Shortcuts

proc bindable(): seq[(string, Option[string])] =
  ## Every bindable id with its default: commands, then the temporary tools.
  for c in commandSpecs(): result.add (c.id, c.shortcut)
  for t in TemporaryTools: result.add (t[0], some(t[2]))

proc keyboardShortcuts(s: var Session; p0: JsonNode): JsonNode =
  const C = "edit.keyboardShortcuts"
  var p = p0
  let r = p.pget("reset")
  if not r.isNil:
    if r.kind == JBool and r.bval: s.editPrefs(proc(pr: var Preferences) = pr.shortcuts.clear())
    elif r.kind == JArray:
      let ids = r
      s.editPrefs(proc(pr: var Preferences) =
        for id in ids.elems:
          if id.kind == JString: pr.shortcuts.del(id.str))
  # A Photoshop `.kys` set (`importKys`, its XML text) becomes `set` entries, matched by label; a
  # key equal to the command's default restores the default, and keys given in `set` as well win
  # over the file's.
  var importInfo: JsonNode = nil
  let xml = p.pget("importKys")
  if not xml.isNil:
    if xml.kind != JString: raise bad(C, "`importKys` is the text of a .kys file")
    var set: KysSet
    try: set = parseKys(xml.str)
    except ValueError as e: raise bad(C, e.msg)
    let plan = planKys(commandCandidates(), set)
    var merged = newJObject()
    for id, sc in plan.set:
      var default = none(string)
      for c in commandSpecs():
        if c.id == id:
          if c.shortcut.isSome: default = normalizeShortcut(c.shortcut.get)
          break
      merged[id] = (if default.isSome and default.get == sc: newJNull() else: %sc)
    let given = p.pget("set")
    if given.isObject:
      for k, v in given.pairs: merged[k] = v
    var planSet = newJObject()
    for id, sc in plan.set: planSet[id] = %sc
    importInfo = %*{"name": (if set.name.isSome: %set.name.get else: newJNull()), "imported": plan.set.len,
                    "set": planSet, "unknown": plan.unknown, "unreadable": plan.unreadable,
                    "alternates": plan.alternates, "toolKeys": set.toolKeys}
    p = p.copy
    p["set"] = merged
  let m = p.pget("set")
  if m.isObject:
    var next = s.prefs.values
    let all = bindable()
    let allowUnknown = p.pget("allowUnknown").asBool.get(false)
    for id, v in m.sortedPairs:
      var known = false
      for (b, _) in all:
        if b == id:
          known = true
          break
      if not known and not allowUnknown: raise bad(C, "unknown command `" & id & "`")
      try: next.setValue("shortcuts." & id, v)
      except PrefsError as e: raise bad(C, e.msg)
    # Like Photoshop, a shortcut moved to a new command is taken away from the old one.
    if p.pget("removeConflicts").asBool.get(true):
      for id, v in m.sortedPairs:
        let sc = (if v.kind == JString: normalizeShortcut(v.str) else: none(string))
        if sc.isNone: continue
        for (c, def) in all:
          # Never strip a command that this same call is assigning: two entries for one key are
          # a clash inside the call, which the returned conflicts list reports (#719).
          if c == id or m.fields.hasKey(c): continue
          let cur = next.shortcut(c, def)
          if cur.isSome and normalizeShortcut(cur.get) == sc: next.shortcuts[c] = ""
    next.shortcuts.sortByKey()
    s.editPrefs(proc(pr: var Preferences) = pr = next)
  let prefs = s.prefs.values
  let filter = (let f = p.pget("filter").asStr; if f.isSome: some(f.get.toLowerAscii) else: none(string))
  let listAll = p.pget("list").asBool.get(false)
  var list = newJArray()
  proc consider(id, label: string; menu: seq[string]; def: Option[string]; hold: bool) =
    if filter.isSome and not (id.toLowerAscii.contains(filter.get) or label.toLowerAscii.contains(filter.get)): return
    if not (listAll or filter.isSome or prefs.shortcuts.hasKey(id)): return
    let sc = prefs.shortcut(id, def)
    var v = %*{"id": id, "label": label, "menu": menu, "default": (if def.isSome: %def.get else: newJNull()),
               "shortcut": (if sc.isSome: %sc.get else: newJNull())}
    if hold: v["hold"] = %true
    list.add v
  for c in commandSpecs(): consider(c.id, c.label, c.menu, c.shortcut, false)
  for t in TemporaryTools: consider(t[0], t[1], @["Tools", "Temporary"], some(t[2]), true)
  var bindings: seq[(string, string)]
  for (id, def) in bindable():
    let sc = prefs.shortcut(id, def)
    if sc.isSome: bindings.add (id, sc.get)
  var conflicts = newJArray()
  for (sc, ids) in shortcutConflicts(bindings): conflicts.add %*{"shortcut": sc, "commands": ids}
  result = %*{"overrides": toJ(prefs.shortcuts), "commands": list, "conflicts": conflicts}
  if not importInfo.isNil: result["import"] = importInfo

proc menusCmd(s: var Session; p: JsonNode): JsonNode =
  ## Edit › Menus: hide/show items and give them colours.
  proc ids(k: string): seq[string] =
    let a = p.pget(k)
    if a.isArray:
      for x in a.elems:
        if x.kind == JString: result.add x.str
  const Colors = ["none", "red", "orange", "yellow", "green", "blue", "violet", "gray"]
  let cm = p.pget("color")
  if cm.isObject:
    for _, v in cm.sortedPairs:
      if v.kind == JString and v.str notin Colors:
        raise bad("edit.menus", "unknown colour `" & v.str & "` (" & Colors.join("|") & ")")
  let (hide, show) = (ids("hide"), ids("show"))
  let reset = p.pget("reset").asBool == some(true)
  s.editPrefs(proc(pr: var Preferences) =
    if reset: pr.menus = MenuCustomization()
    for id in hide:
      if id notin pr.menus.hidden: pr.menus.hidden.add id
    var kept: seq[string]
    for h in pr.menus.hidden:
      if h notin show: kept.add h
    pr.menus.hidden = kept
    if cm.isObject:
      for id, v in cm.sortedPairs:
        if v.kind == JString and v.str != "none": pr.menus.colors[id] = v.str
        else: pr.menus.colors.del(id)
      pr.menus.colors.sortByKey())
  toJ(s.prefs.values.menus)

proc toolbarCmd(s: var Session; p: JsonNode): JsonNode =
  ## Edit › Toolbar: hide tools and reorder the toolbar.
  proc ids(k: string): Option[seq[string]] =
    let a = p.pget(k)
    if not a.isArray: return
    var r: seq[string]
    for x in a.elems:
      if x.kind == JString: r.add x.str
    some(r)
  let reset = p.pget("reset").asBool == some(true)
  let (hidden, order) = (ids("hidden"), ids("order"))
  s.editPrefs(proc(pr: var Preferences) =
    if reset: pr.toolbar = ToolbarCustomization()
    if hidden.isSome: pr.toolbar.hidden = hidden.get
    if order.isSome: pr.toolbar.order = order.get)
  toJ(s.prefs.values.toolbar)

proc section(id, label, shortcut: string): CommandSpec =
  let sec = id[id.rfind('.') + 1 .. ^1]
  cmd(id, label, ["Edit", "Preferences"], shortcut, "{}", enabledProc(alwaysE(s)),
      runProc(preferencesSection(s, sec)), journal = false)

proc prefsSpecs*(): seq[CommandSpec] =
  @[cmd("prefs.get", "Get Preferences", [], "",
        """{"path":"section.key"?=everything (e.g. "performance.historyStates", "colorSettings.workingRgb")}""",
        enabledProc(alwaysE(s)), runProc(prefsGet(s, p)), journal = false),
    cmd("prefs.set", "Set Preferences", [], "",
        """{"path":"section.key","value":json} or {"values":{"section.key":json,…}} (validated; all or nothing)""",
        enabledProc(alwaysE(s)), runProc(prefsSet(s, p)), journal = false),
    cmd("prefs.reset", "Reset Preferences", [], "", """{"path":"section|section.key"?=everything}""",
        enabledProc(alwaysE(s)), runProc(prefsReset(s, p)), journal = false),
    # Photoshop: ⌘K opens Preferences › General (⌘, is Layer › Hide Layers).
    section("edit.preferences.general", "General…", "Cmd+K"),
    section("edit.preferences.interface", "Interface…", ""),
    section("edit.preferences.workspace", "Workspace…", ""),
    section("edit.preferences.tools", "Tools…", ""),
    section("edit.preferences.historyLog", "History Log…", ""),
    section("edit.preferences.fileHandling", "File Handling…", ""),
    section("edit.preferences.export", "Export…", ""),
    section("edit.preferences.performance", "Performance…", ""),
    section("edit.preferences.scratchDisks", "Scratch Disks…", ""),
    section("edit.preferences.cursors", "Cursors…", ""),
    section("edit.preferences.transparencyAndGamut", "Transparency & Gamut…", ""),
    section("edit.preferences.unitsAndRulers", "Units & Rulers…", ""),
    section("edit.preferences.guidesGridAndSlices", "Guides, Grid & Slices…", ""),
    section("edit.preferences.plugIns", "Plug-ins…", ""),
    section("edit.preferences.type", "Type…", ""),
    section("edit.preferences.enhancedControls", "Enhanced Controls…", ""),
    section("edit.preferences.rawDefaults", "Camera Raw…", ""),
    section("edit.preferences.integrations", "Integrations…", ""),
    cmd("edit.keyboardShortcuts", "Keyboard Shortcuts…", ["Edit"], "Cmd+Alt+Shift+K",
        """{"set":{"<command id>|tools.temporary.hand|zoomIn|zoomOut":"Cmd+Shift+X"|""(remove)|null(default)}?,"reset":true|["<id>",…]?,"removeConflicts":bool=true,"filter":str?,"list":bool=false,"importKys":"<.kys XML>"?}""",
        enabledProc(alwaysE(s)), runProc(keyboardShortcuts(s, p))),
    cmd("edit.menus", "Menus…", ["Edit"], "Cmd+Alt+Shift+M",
        """{"hide":["<id>",…]?,"show":["<id>",…]?,"color":{"<id>":"red|orange|yellow|green|blue|violet|gray|none"}?,"reset":bool=false}""",
        enabledProc(alwaysE(s)), runProc(menusCmd(s, p))),
    cmd("edit.toolbar", "Toolbar…", ["Edit"], "", """{"hidden":["<tool>",…]?,"order":["<tool>",…]?,"reset":bool=false}""",
        enabledProc(alwaysE(s)), runProc(toolbarCmd(s, p)))]
