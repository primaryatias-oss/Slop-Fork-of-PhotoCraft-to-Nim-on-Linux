## Window › Patterns (`presets/patterns.rs`): folders over the pattern library (`pattern.*` owns the
## patterns themselves), the selected pattern, and Pattern Fill layers made from it. Groups hold
## pattern ids; library patterns in no group are shown in a trailing "Patterns" group; ids whose
## pattern was deleted are dropped. Part of the `engine/core` unit: included, not imported.

const UngroupedPatterns = "Patterns"

proc builtinPatternGroups*(): seq[PresetGroup[string]] =
  ## Built-in folders for the built-in procedural patterns.
  let lib = builtinPatterns()
  proc ids(names: openArray[string]): seq[string] =
    for n in names:
      for p in lib:
        if p.name == n:
          result.add p.id
          break
  @[newGroup("Geometric", ids(["Checkerboard", "Diagonal Lines", "Dots", "Grid"])),
    newGroup("Textures", ids(["Bricks", "Weave", "Paper Noise"]))]

proc syncPatternGroups*(s: var Session) =
  ## Bring the groups in line with the library.
  var lib: seq[string]
  for p in s.patterns.items: lib.add p.id
  for g in s.presets.patternGroups.mitems: g.items.keepItIf(it in lib)
  var loose: seq[string]
  for id in lib:
    if not s.presets.patternGroups.anyIt(id in it.items): loose.add id
  if loose.len > 0:
    let gi = groupIndex(s.presets.patternGroups, some(UngroupedPatterns))
    s.presets.patternGroups[gi].items.add loose

proc patternPresetsList(s: var Session): JsonNode =
  syncPatternGroups(s)
  var groups = newJArray()
  for g in s.presets.patternGroups:
    var pats = newJArray()
    for id in g.items:
      for p in s.patterns.items:
        if p.id == id:
          pats.add %*{"id": p.id, "name": p.displayName, "width": p.width, "height": p.height}
          break
    groups.add %*{"name": g.name, "patterns": pats}
  let cur = currentPresetPattern(s)
  %*{"groups": groups, "current": (if cur.isSome: %cur.get.id else: newJNull())}

proc patternKey(s: Session; p: JsonNode; cmdId: string): DocPattern =
  let key = strParam(p, "pattern").get("")
  let r = resolvePattern(s, key)
  if r.isNone: raise bad(cmdId, "no pattern \"" & key & "\" (see pattern.presets.list)")
  r.get

proc activePatternFill(s: Session): Option[LayerId] =
  let d = s.active
  if d.isNil or d.activeLayer.isNone: return
  let l = d.doc[].findLayer(d.activeLayer.get)
  if not l.isNil and l[].content.kind == lkFill and l[].content.fill.kind == fkPattern: some(d.activeLayer.get)
  else: none(LayerId)

proc patternPresetsSelect(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "pattern.presets.select"
  let pat = patternKey(s, p, cmdId)
  s.presets.pattern = some(pat.id)
  inc s.presets.rev
  result = %*{"pattern": pat.id, "name": pat.displayName}
  let id = activePatternFill(s)
  if p.getBoolOr("applyToLayer", true) and id.isSome:
    let lid = id.get
    s.editDo("Change Pattern Fill"):
      ensureInDoc(doc, pat)
      let l = doc.layerPtr(lid)
      if not l.isNil and l[].content.kind == lkFill and l[].content.fill.kind == fkPattern:
        l[].content.fill.name = pat.name
        l[].content.fill.id = pat.id
    result["layer"] = %uint64(lid)

proc patternPresetsApply(s: var Session; p: JsonNode): JsonNode =
  let pat = patternKey(s, p, "pattern.presets.apply")
  var q = (if p.isObject: p.copy else: newJObject())
  q["pattern"] = %pat.id
  callCommand(s, "layer.newFillLayer.pattern", q)

proc patternPresetsNew(s: var Session; p: JsonNode): JsonNode =
  let r = callCommand(s, "edit.definePattern", (if p.isNil: newJNull() else: p.copy))
  let id = r.pget("pattern").asStr.get("")
  syncPatternGroups(s)
  let g = strParam(p, "group")
  if g.isSome:
    for gr in s.presets.patternGroups.mitems: gr.items.keepItIf(it != id)
    let gi = groupIndex(s.presets.patternGroups, g)
    s.presets.patternGroups[gi].items.add id
  s.presetsChanged()
  %*{"pattern": id}

proc patternPresetsEdit(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "pattern.presets.edit"
  syncPatternGroups(s)
  let action = reqStr(p, "action", cmdId)
  # Rename/delete act on the library pattern (by id or name).
  case action
  of "rename":
    result = callCommand(s, "pattern.rename", %*{"pattern": reqStr(p, "preset", cmdId), "name": reqStr(p, "name", cmdId)})
  of "delete":
    var keys: seq[string]
    let pv = p.pget("preset")
    if pv.isArray:
      for x in pv.elems:
        if x.kind == JString: keys.add x.str
    else: keys = @[reqStr(p, "preset", cmdId)]
    for k in keys: discard callCommand(s, "pattern.delete", %*{"pattern": k})
    result = %*{"deleted": keys.len}
  of "deleteGroup":
    # Deleting a folder deletes its patterns, as in Photoshop.
    let g = reqStr(p, "group", cmdId)
    var i = -1
    for k, x in s.presets.patternGroups:
      if x.name == g:
        i = k
        break
    if i < 0: raise bad(cmdId, "no group \"" & g & "\"")
    let gone = s.presets.patternGroups[i]
    s.presets.patternGroups.delete(i)
    s.patterns.items.keepItIf(it.id notin gone.items)
    result = %*{"group": g, "deleted": gone.items.len}
  of "move":
    let key = reqStr(p, "preset", cmdId)
    let pat = resolvePattern(s, key)
    if pat.isNone: raise bad(cmdId, "no pattern \"" & key & "\"")
    var q = p.copy
    q["preset"] = %pat.get.id
    result = editGroups(s.presets.patternGroups, "move", q, cmdId)
  else: result = editGroups(s.presets.patternGroups, action, p, cmdId)
  syncPatternGroups(s)
  s.presetsChanged()

proc patternPresetSpecs*(): seq[CommandSpec] =
  @[
    cmd("pattern.presets.list", "Pattern Presets", [], "", "{} → {groups:[{name,patterns:[{id,name,width,height}]}],current}",
        presetAlwaysE, runProc(patternPresetsList(s)), journal = false),
    cmd("pattern.presets.select", "Select Pattern", [], "",
        "{\"pattern\":id|name,\"applyToLayer\":bool=true (also changes a selected Pattern Fill layer)} (Fill and new Pattern Fill layers default to it)",
        presetAlwaysE, runProc(patternPresetsSelect(s, p))),
    cmd("pattern.presets.apply", "New Pattern Fill Layer from Preset", [], "",
        "{\"pattern\":id|name?=selected,\"scale\":1..1000=100,\"angle\":deg=0} → {layer}", presetHasDocE,
        runProc(patternPresetsApply(s, p))),
    cmd("pattern.presets.new", "New Pattern Preset", [], "",
        "{\"name\":str?,\"group\":name?,\"rect\":[x0,y0,x1,y1]?} (Define Pattern from the selection/canvas into a group)",
        presetHasDocE, runProc(patternPresetsNew(s, p))),
    cmd("pattern.presets.edit", "Edit Pattern Presets", [], "", GroupEditParams, presetAlwaysE,
        runProc(patternPresetsEdit(s, p))),
  ]
