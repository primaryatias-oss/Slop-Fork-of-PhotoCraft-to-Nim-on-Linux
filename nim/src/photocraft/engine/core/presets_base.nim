## Preset panels (`presets/mod.rs`): Window › Gradients, Patterns, Styles, Shapes, Tool Presets,
## Clone Source and Swatches. Photoshop keeps presets in named groups (folders) that the panels show
## as thumbnail grids or lists; the engine owns that data so every frontend sees and edits the same
## sets through `<kind>.presets.*` commands. The built-in groups are our own (clean-room). Part of
## the `engine/core` unit: included, not imported.

proc newGroup*[T](name: string; items: seq[T]): PresetGroup[T] = PresetGroup[T](name: name, items: items)

proc itemName(x: string): string = x
proc itemName[T: object](x: T): string = x.name
proc setItemName(x: var string; n: string) = x = n
proc setItemName[T: object](x: var T; n: string) = x.name = n

proc presetsChanged*(s: var Session) =
  ## Record a preset change: bumps the panel revision (the preferences document persists it).
  inc s.presets.rev
  prefsTouch(s)

proc presetAlwaysE(s: Session): string = ""
proc presetHasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")
proc presetHasLayerE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if d.activeLayer.isNone or d.doc[].findLayer(d.activeLayer.get).isNil: return "no active layer"
  ""

proc strParam*(p: JsonNode; k: string): Option[string] =
  ## A trimmed, non-empty string param.
  let v = p.pget(k).asStr
  if v.isNone: return
  let t = unicode.strip(v.get)
  if t.len > 0: some(t) else: none(string)

proc reqStr*(p: JsonNode; k, cmdId: string): string =
  let v = strParam(p, k)
  if v.isNone: raise bad(cmdId, "missing `" & k & "`")
  v.get

proc findPreset*[T](groups: openArray[PresetGroup[T]]; name: string; group: Option[string]): Option[(int, int)] =
  ## Locate a preset by name (optionally within `group`): (group index, item index).
  for gi, g in groups:
    if group.isSome and g.name != group.get: continue
    for ii, it in g.items:
      if itemName(it) == name: return some((gi, ii))

proc nameTaken[T](groups: openArray[PresetGroup[T]]; n: string): bool =
  for g in groups:
    for it in g.items:
      if itemName(it) == n: return true

proc groupTaken[T](groups: openArray[PresetGroup[T]]; n: string): bool =
  for g in groups:
    if g.name == n: return true

proc uniqueName*[T](groups: openArray[PresetGroup[T]]; base: string): string =
  ## `base`, or `base 2`, `base 3`… so names stay unique across all groups.
  if not nameTaken(groups, base): return base
  var i = 2
  while true:
    let n = base & " " & $i
    if not nameTaken(groups, n): return n
    inc i

proc groupIndex*[T](groups: var seq[PresetGroup[T]]; group: Option[string]): int =
  ## Index of `group` (created at the end when missing), or the first group (created as
  ## "Presets" when there is none).
  if group.isSome:
    for i, g in groups:
      if g.name == group.get: return i
    groups.add newGroup[T](group.get, @[])
    return groups.len - 1
  if groups.len == 0: groups.add newGroup[T]("Presets", @[])
  0

proc uniqueGroup*[T](groups: openArray[PresetGroup[T]]; base: string): string =
  if not groupTaken(groups, base): return base
  var i = 1
  while true:
    let n = base & " " & $i
    if not groupTaken(groups, n): return n
    inc i

proc editGroups*[T](groups: var seq[PresetGroup[T]]; action: string; p: JsonNode; cmdId: string): JsonNode =
  ## Generic group/item edits shared by every preset kind: `rename`, `delete`, `move`,
  ## `newGroup`, `renameGroup`, `deleteGroup`.
  let group = strParam(p, "group")
  case action
  of "rename":
    let name = reqStr(p, "preset", cmdId)
    let to = reqStr(p, "name", cmdId)
    let at = findPreset(groups, name, group)
    if at.isNone: raise bad(cmdId, "no preset \"" & name & "\"")
    if name != to and findPreset(groups, to, none(string)).isSome:
      raise bad(cmdId, "a preset named \"" & to & "\" already exists")
    setItemName(groups[at.get[0]].items[at.get[1]], to)
    %*{"name": to}
  of "delete":
    var names: seq[string]
    let pv = p.pget("preset")
    if pv.isArray:
      for x in pv.elems:
        if x.kind == JString: names.add x.str
    else: names = @[reqStr(p, "preset", cmdId)]
    # All or nothing: a missing name deletes none of the others.
    for n in names:
      if findPreset(groups, n, group).isNone: raise bad(cmdId, "no preset \"" & n & "\"")
    names.sort(cmp[string])
    names = deduplicate(names, isSorted = true)
    for n in names:
      let at = findPreset(groups, n, group)
      if at.isSome and at.get[0] < groups.len and at.get[1] < groups[at.get[0]].items.len:
        groups[at.get[0]].items.delete(at.get[1])
    %*{"deleted": names.len}
  of "move":
    let name = reqStr(p, "preset", cmdId)
    let to = reqStr(p, "to", cmdId)
    let at = findPreset(groups, name, group)
    if at.isNone: raise bad(cmdId, "no preset \"" & name & "\"")
    let item = groups[at.get[0]].items[at.get[1]]
    groups[at.get[0]].items.delete(at.get[1])
    let ti = groupIndex(groups, some(to))
    let idx = p.pget("index").asU64
    let n = groups[ti].items.len
    let pos = (if idx.isSome: int(min(idx.get, uint64(n))) else: n)
    groups[ti].items.insert(item, pos)
    %*{"group": to}
  of "newGroup":
    let base = strParam(p, "name").get("Group")
    let name = uniqueGroup(groups, base)
    groups.add newGroup[T](name, @[])
    %*{"group": name}
  of "renameGroup":
    let g = reqStr(p, "group", cmdId)
    let to = reqStr(p, "name", cmdId)
    var i = -1
    for k, x in groups:
      if x.name == g:
        i = k
        break
    if i < 0: raise bad(cmdId, "no group \"" & g & "\"")
    groups[i].name = to
    %*{"group": to}
  of "deleteGroup":
    let g = reqStr(p, "group", cmdId)
    var i = -1
    for k, x in groups:
      if x.name == g:
        i = k
        break
    if i < 0: raise bad(cmdId, "no group \"" & g & "\"")
    let removed = groups[i]
    groups.delete(i)
    %*{"group": g, "deleted": removed.items.len}
  else: raise bad(cmdId, "unknown action " & action)

const GroupEditParams* = "{\"action\":\"rename|delete|move|newGroup|renameGroup|deleteGroup\",\"preset\":name|[names] (rename/delete/move),\"group\":name? (narrows the lookup; the group for renameGroup/deleteGroup),\"name\":str (rename: new name; newGroup/renameGroup: group name),\"to\":group (move),\"index\":n? (move)}"

proc callCommand*(s: var Session; id: string; p: JsonNode): JsonNode =
  ## Run another command's implementation from inside a command (checks `enabled`, but doesn't
  ## journal it: the outer command is what an action replays).
  let spec = findSpec(id)
  if spec.isNil: raise unknownCommand(id)
  let why = spec[].enabled(s)
  if why.len > 0: raise disabledErr(id, why)
  spec[].run(s, p)
