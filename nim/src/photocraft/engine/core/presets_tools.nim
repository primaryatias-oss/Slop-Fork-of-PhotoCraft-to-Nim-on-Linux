## Window › Tool Presets (`presets/tools.rs`): a tool plus its options under a name. A preset's
## `options` are opaque JSON the frontend writes and reads back; the engine understands `"brush"`
## (applied like `tools.setBrush`) and `"foreground"` (Include Color). Crop presets are tool presets
## of the `"crop"` tool. Part of the `engine/core` unit: included, not imported.

const
  MaxToolPresets* = 1000 ## saving more is refused; a larger preferences file loads the first
  MaxPresetName* = 255   ## `brush_preset_cmds::MAX_NAME`

proc checkedPresetName(name, cmdId: string): string =
  ## A preset name: trimmed, 1–255 characters.
  let n = unicode.strip(name)
  if n.len == 0: raise bad(cmdId, "the preset name is empty")
  if n.runeLen > MaxPresetName: raise bad(cmdId, "preset names are at most " & $MaxPresetName & " characters")
  n

proc loadToolPresets*(value: JsonNode): seq[ToolPreset] =
  ## Read saved tool presets entry by entry, so a damaged entry is skipped without discarding the
  ## others.
  if not value.isArray: return
  for entry in value.elems:
    if result.len >= MaxToolPresets: break
    if not entry.isObject: continue
    let name = entry.pget("name").asStr
    let tool = entry.pget("tool").asStr
    if name.isNone or tool.isNone: continue
    var n: string
    try: n = checkedPresetName(name.get, "")
    except EngineError: continue
    if unicode.strip(tool.get).len == 0 or result.anyIt(it.name == n): continue
    var options = entry.pget("options")
    if not options.isObject: options = newJObject()
    result.add ToolPreset(name: n, tool: tool.get, options: options.copy)

proc normTool(t: string): string =
  result = t.toLowerAscii.multiReplace((" ", ""), ("_", ""), ("-", ""))
  while result.endsWith("tool"): result.setLen(result.len - 4)

proc builtinToolPresets*(): seq[ToolPreset] =
  ## A few starting presets (our own).
  proc brush(size, hardness: float32): JsonNode = %*{"brush": {"size": size, "hardness": hardness}}
  @[
    ToolPreset(name: "Soft Round 100 px", tool: "brush", options: brush(100, 0)),
    ToolPreset(name: "Hard Round 12 px", tool: "brush", options: brush(12, 1)),
    ToolPreset(name: "Soft Eraser 60 px", tool: "eraser", options: brush(60, 0)),
    ToolPreset(name: "Radial Gradient", tool: "gradient", options: %*{"toolOptions": {"gradient_style": "radial"}}),
    ToolPreset(name: "Clone Soft 40 px", tool: "cloneStamp", options: brush(40, 0.2)),
  ]

proc toolPresetsList(s: var Session; p: JsonNode): JsonNode =
  let filter = strParam(p, "tool")
  var items = newJArray()
  for t in s.presets.toolPresets:
    if filter.isNone or normTool(t.tool) == normTool(filter.get): items.add %*{"name": t.name, "tool": t.tool}
  %*{"presets": items}

proc toolPresetsNew(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "tool.presets.new"
  let tool = reqStr(p, "tool", cmdId)
  var options = (if p.pget("options").isNil: newJObject() else: p.pget("options").copy)
  if not options.isObject: raise bad(cmdId, "`options` must be an object")
  # Brush-based tools carry the current brush unless the caller sent one.
  const brushy = ["brush", "pencil", "eraser", "clonestamp", "healing", "spothealing", "historybrush", "blur", "sharpen",
                  "smudge", "dodge", "burn", "sponge", "mixerbrush", "colorreplacement"]
  if options.pget("brush").isNil and normTool(tool) in brushy: options["brush"] = toJ(s.tools.brush)
  if p.getBoolOr("includeColor", false): options["foreground"] = %s.tools.foreground
  if s.presets.toolPresets.len >= MaxToolPresets:
    raise bad(cmdId, "at most " & $MaxToolPresets & " tool presets can be saved; delete some first")
  var base: string
  let n = p.pget("name")
  if n.isNull: base = tool
  elif n.kind == JString:
    base = (if unicode.strip(n.str).len == 0: tool else: checkedPresetName(n.str, cmdId))
  else: raise bad(cmdId, "`name` must be a string")
  let name = uniqueName([newGroup("", s.presets.toolPresets)], base)
  s.presets.toolPresets.add ToolPreset(name: name, tool: tool, options: options)
  s.presetsChanged()
  %*{"name": name, "tool": tool}

proc toolPresetsSelect(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "tool.presets.select"
  let name = reqStr(p, "preset", cmdId)
  var found = -1
  for i, t in s.presets.toolPresets:
    if t.name == name:
      found = i
      break
  if found < 0: raise bad(cmdId, "no tool preset \"" & name & "\" (see tool.presets.list)")
  let t = s.presets.toolPresets[found]
  let b = t.options.pget("brush")
  if b.isObject: discard callCommand(s, "tools.setBrush", %*{"brush": b})
  let c = t.options.pget("foreground")
  if c.isArray and c.len >= 3:
    var v: seq[float32]
    for x in c.elems: v.add float32(x.asF64.get(0.0))
    s.tools.foreground = [v[0], v[1], v[2], (if v.len > 3: v[3] else: 1'f32)]
  %*{"name": t.name, "tool": t.tool, "options": t.options}

proc toolPresetsEdit(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "tool.presets.edit"
  let action = reqStr(p, "action", cmdId)
  if action notin ["rename", "delete"]: raise bad(cmdId, "tool presets support rename and delete")
  if action == "rename": discard checkedPresetName(reqStr(p, "name", cmdId), cmdId)
  var groups = @[newGroup("", s.presets.toolPresets)]
  try: result = editGroups(groups, action, p, cmdId)
  finally: s.presets.toolPresets = groups[0].items
  s.presetsChanged()

proc toolPresetsReset(s: var Session): JsonNode =
  s.presets.toolPresets = builtinToolPresets()
  s.presetsChanged()
  %*{"presets": s.presets.toolPresets.len}

proc toolPresetSpecs*(): seq[CommandSpec] =
  @[
    cmd("tool.presets.list", "Tool Presets", [], "", "{\"tool\":name? (Current Tool Only)} → {presets:[{name,tool}]}",
        presetAlwaysE, runProc(toolPresetsList(s, p)), journal = false),
    cmd("tool.presets.new", "New Tool Preset…", [], "",
        "{\"name\":str?=tool (at most 255 characters; a taken name gets \" 2\", \" 3\"…),\"tool\":name (\"crop\" for Crop presets),\"options\":{…}? (opaque; \"brush\" defaults to the current brush for painting tools),\"includeColor\":bool=false}",
        presetAlwaysE, runProc(toolPresetsNew(s, p))),
    cmd("tool.presets.select", "Select Tool Preset", [], "",
        "{\"preset\":name} → {name,tool,options} (applies \"brush\" and \"foreground\"; the shell switches tool and options)",
        presetAlwaysE, runProc(toolPresetsSelect(s, p))),
    cmd("tool.presets.edit", "Edit Tool Presets", [], "",
        "{\"action\":\"rename|delete\",\"preset\":name|[names],\"name\":str (rename)}", presetAlwaysE,
        runProc(toolPresetsEdit(s, p))),
    cmd("tool.presets.reset", "Reset Tool Presets", [], "", "{}", presetAlwaysE, runProc(toolPresetsReset(s))),
  ]
