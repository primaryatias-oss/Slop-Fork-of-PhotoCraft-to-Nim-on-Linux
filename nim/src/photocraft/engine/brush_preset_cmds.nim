## Organising brush presets (port of `brush_preset_cmds.rs`): the Brushes panel's drag and drop and
## context menus. Rename, move a preset within or between groups and folders, move, rename and
## delete whole groups and folders.
##
## Groups are the panel's top-level folders (`BrushPreset.group`); a group can hold nested folders
## (`BrushPreset.folder`, a path inside the group). The group commands take an optional `folder`
## path to act on a nested folder instead of the whole group. The library order is the panel
## order: groups appear in order of their first preset, and a group's folders and presets in order
## of their first preset too. A built-in that is renamed or moved to another group becomes the
## user's preset.

import std/[json, options, strutils, unicode]
import ./core
import ../paint
import ../psd/abr

const
  MaxBrushPresetName* = 255             ## longest preset or group name accepted
  MaxFolderDepth* = AbrMaxFolderDepth   ## deepest folder path accepted inside a group

proc textParam(p: JsonNode; k, cmdId: string): string =
  let v = p.pget(k).asStr
  let t = (if v.isSome: unicode.strip(v.get) else: "")
  if t.len == 0: raise bad(cmdId, "missing `" & k & "`")
  if t.runeLen > MaxBrushPresetName: raise bad(cmdId, "`" & k & "` is longer than " & $MaxBrushPresetName & " characters")
  t

proc groupParam(p: JsonNode; k, cmdId: string): Option[string] =
  ## A group name param: may be empty (the ungrouped presets), never absurdly long.
  let v = p.pget(k)
  if v.isNull: return none(string)
  if v.kind == JString and v.str.runeLen <= MaxBrushPresetName: return some(unicode.strip(v.str))
  raise bad(cmdId, "`" & k & "` must be a group name (string, at most " & $MaxBrushPresetName & " characters)")

proc folderParam(p: JsonNode; cmdId: string): Option[seq[string]] =
  ## A folder path param (`["Outer", "Inner"]`, inside the group): none when absent or null.
  let v = p.pget("folder")
  if v.isNull: return none(seq[string])
  template err(): untyped =
    bad(cmdId, "`folder` must be a list of at most " & $MaxFolderDepth & " folder names (non-empty strings of at most " &
        $MaxBrushPresetName & " characters)")
  if v.kind != JArray or v.len > MaxFolderDepth: raise err()
  var f: seq[string]
  for e in v.elems:
    if e.kind != JString: raise err()
    let n = unicode.strip(e.str)
    if n.len == 0 or n.runeLen > MaxBrushPresetName: raise err()
    f.add n
  some(f)

proc startsWith(a, prefix: seq[string]): bool =
  if prefix.len > a.len: return false
  for i, x in prefix:
    if a[i] != x: return false
  true

proc inFolder(x: BrushPreset; g: string; f: seq[string]): bool =
  ## Is `x` in folder `f` of group `g`, directly or below it?
  x.group == g and x.folder.startsWith(f)

proc subfolderOrder*(presets: seq[BrushPreset]; g: string; parent: seq[string]): seq[string] =
  ## The subfolders directly inside folder `parent` of group `g`, in panel order.
  for x in presets:
    if inFolder(x, g, parent) and x.folder.len > parent.len:
      let n = x.folder[parent.len]
      if n notin result: result.add n

proc indexOfPreset(presets: seq[BrushPreset]; name: string): int =
  for i, x in presets:
    if cmpIgnoreCase(x.name, name) == 0: return i
  -1

proc findPreset(s: Session; p: JsonNode; k, cmdId: string): int =
  let name = textParam(p, k, cmdId)
  result = indexOfPreset(s.tools.presets, name)
  if result < 0: raise bad(cmdId, "no brush preset named `" & name & "`")

proc groupOrder*(presets: seq[BrushPreset]): seq[string] =
  ## Groups in panel order (first appearance).
  for p in presets:
    if p.group notin result: result.add p.group

proc positionInGroup(presets: seq[BrushPreset]; i: int): int =
  if i >= presets.len: return 0
  for k in 0 ..< i:
    if presets[k].group == presets[i].group: inc result

proc indexParam(p: JsonNode; cmdId: string): Option[uint64] =
  let v = p.pget("index")
  if v.isNull: return none(uint64)
  let n = v.asU64
  if n.isNone: raise bad(cmdId, "`index` must be a non-negative integer")
  n

proc renamePreset(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "brush.presets.rename"
  let i = findPreset(s, p, "name", cmdId)
  let newName = textParam(p, "newName", cmdId)
  let j = indexOfPreset(s.tools.presets, newName)
  if j >= 0 and j != i: raise bad(cmdId, "a brush preset named `" & newName & "` already exists")
  if s.tools.presets[i].name != newName:
    let wasCurrent = s.tools.currentPreset.isSome and cmpIgnoreCase(s.tools.currentPreset.get, s.tools.presets[i].name) == 0
    s.tools.presets[i].name = newName
    s.tools.presets[i].builtin = false
    if wasCurrent: s.tools.currentPreset = some(newName)
    s.brushPresetsChanged()
  %*{"name": newName}

proc movePreset(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "brush.presets.move"
  let i = findPreset(s, p, "name", cmdId)
  var before = -1
  if not p.pget("before").isNull:
    before = findPreset(s, p, "before", cmdId)
    if before == i: raise bad(cmdId, "`before` names the preset being moved")
  let index = indexParam(p, cmdId)
  let folder = folderParam(p, cmdId)
  let group = groupParam(p, "group", cmdId)
  var lib = s.tools.presets
  let moving = lib[i]
  let target =
    if group.isSome: group.get
    elif before >= 0: lib[before].group
    else: moving.group
  # The folder: as given, else `before`'s, else its own when it stays in its group.
  let targetFolder =
    if folder.isSome: folder.get
    elif before >= 0: lib[before].folder
    elif moving.group == target: moving.folder
    else: newSeq[string]()
  if before >= 0:
    if lib[before].group != target: raise bad(cmdId, "`before` is not in group `" & target & "`")
    if lib[before].folder != targetFolder: raise bad(cmdId, "`before` is not in that folder")
  let beforeName = (if before >= 0: some(lib[before].name) else: none(string))
  var pr = lib[i]
  lib.delete(i)
  if pr.group != target or pr.folder != targetFolder:
    pr.group = target
    pr.folder = targetFolder
    # A built-in moved elsewhere is the user's now (built-ins regenerate in their own group).
    pr.builtin = false
  var members: seq[int]
  for k, x in lib:
    if x.group == target: members.add k
  # Appended: after the folder's last preset (or the group's, for a new folder).
  var last = -1
  for k in countdown(lib.high, 0):
    if inFolder(lib[k], target, targetFolder):
      last = k
      break
  if last < 0 and members.len > 0: last = members[^1]
  let ending = (if last >= 0: last + 1 else: lib.len)
  var pos: int
  if beforeName.isSome:
    let k = indexOfPreset(lib, beforeName.get)
    pos = (if k >= 0: k else: lib.len)
  elif index.isSome:
    let n = index.get
    pos = (if n < uint64(members.len): members[int(n)] elif members.len > 0: members[^1] + 1 else: lib.len)
  else: pos = ending
  pos = min(pos, lib.len)
  let name = pr.name
  lib.insert(pr, pos)
  let at = positionInGroup(lib, pos)
  s.tools.presets = lib
  s.brushPresetsChanged()
  %*{"name": name, "group": target, "folder": targetFolder, "index": at}

proc groupBlock(s: Session; g, cmdId: string) =
  for x in s.tools.presets:
    if x.group == g: return
  raise bad(cmdId, "no brush preset group named `" & g & "`")

proc targetFolder(s: Session; p: JsonNode; cmdId: string): (string, seq[string]) =
  ## The group and (possibly empty) folder path a group command acts on; both must exist.
  let g = groupParam(p, "group", cmdId)
  if g.isNone: raise bad(cmdId, "missing `group`")
  groupBlock(s, g.get, cmdId)
  let f = folderParam(p, cmdId).get(@[])
  var found = false
  for x in s.tools.presets:
    if inFolder(x, g.get, f):
      found = true
      break
  if not found: raise bad(cmdId, "no folder `" & f.join("/") & "` in brush preset group `" & g.get & "`")
  (g.get, f)

proc moveFolder(s: var Session; p: JsonNode; g: string; f: seq[string]; cmdId: string): JsonNode =
  ## Move folder `f` (non-empty) of group `g` among its sibling folders, as a block.
  if f.len == 0: raise bad(cmdId, "missing `folder`")
  let leaf = f[^1]
  let parent = f[0 ..< f.len - 1]
  let siblings = subfolderOrder(s.tools.presets, g, parent)
  var before = none(string)
  let bv = p.pget("before")
  if not bv.isNull:
    if bv.kind == JString and bv.str == leaf: raise bad(cmdId, "`before` names the folder being moved")
    elif bv.kind == JString and bv.str in siblings: before = some(bv.str)
    else: raise bad(cmdId, "`before` must name a folder next to the one being moved")
  let index = indexParam(p, cmdId)
  let lib = s.tools.presets
  # Where the block sits among the other presets (its place when nothing else is in the parent).
  var start = 0
  for k, x in lib:
    if inFolder(x, g, f):
      start = k
      break
  var blk, rest: seq[BrushPreset]
  for x in lib:
    if inFolder(x, g, f): blk.add x else: rest.add x
  proc sibling(name: string): seq[string] = parent & @[name]
  proc first(name: string; rest: seq[BrushPreset]): int =
    let sf = sibling(name)
    for k, x in rest:
      if inFolder(x, g, sf): return k
    rest.len
  var others: seq[string]
  for n in siblings:
    if n != leaf: others.add n
  proc parentEnd(rest: seq[BrushPreset]): int =
    for k in countdown(rest.high, 0):
      if inFolder(rest[k], g, parent): return k + 1
    start
  let pos =
    if before.isSome: first(before.get, rest)
    elif index.isSome: (if index.get < uint64(others.len): first(others[int(index.get)], rest) else: parentEnd(rest))
    else: parentEnd(rest)
  let cut = min(pos, rest.len)
  var outp = rest[0 ..< cut]
  outp.add blk
  outp.add rest[cut .. ^1]
  s.tools.presets = outp
  s.brushPresetsChanged()
  let at = max(subfolderOrder(s.tools.presets, g, parent).find(leaf), 0)
  %*{"group": g, "folder": f, "index": at}

proc moveGroup(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "brush.presets.moveGroup"
  let (g, f) = targetFolder(s, p, cmdId)
  if f.len > 0: return moveFolder(s, p, g, f, cmdId)
  let before = groupParam(p, "before", cmdId)
  if before.isSome:
    groupBlock(s, before.get, cmdId)
    if before.get == g: raise bad(cmdId, "`before` names the group being moved")
  let index = indexParam(p, cmdId)
  var blk, rest: seq[BrushPreset]
  for x in s.tools.presets:
    if x.group == g: blk.add x else: rest.add x
  let order = groupOrder(rest)
  proc first(name: string): int =
    for k, x in rest:
      if x.group == name: return k
    rest.len
  let pos =
    if before.isSome: first(before.get)
    elif index.isSome: (if index.get < uint64(order.len): first(order[int(index.get)]) else: rest.len)
    else: rest.len
  let cut = min(pos, rest.len)
  var outp = rest[0 ..< cut]
  outp.add blk
  outp.add rest[cut .. ^1]
  s.tools.presets = outp
  s.brushPresetsChanged()
  let at = max(groupOrder(s.tools.presets).find(g), 0)
  %*{"group": g, "index": at}

proc renameGroup(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "brush.presets.renameGroup"
  let (g, f) = targetFolder(s, p, cmdId)
  let newName = textParam(p, "newName", cmdId)
  if f.len > 0:
    let leaf = f[^1]
    let parent = f[0 ..< f.len - 1]
    if newName != leaf:
      if newName in subfolderOrder(s.tools.presets, g, parent):
        raise bad(cmdId, "a folder named `" & newName & "` already exists there")
      let depth = parent.len
      for x in s.tools.presets.mitems:
        if inFolder(x, g, f):
          if depth < x.folder.len: x.folder[depth] = newName
          x.builtin = false
      s.brushPresetsChanged()
    return %*{"group": g, "folder": parent & @[newName]}
  if newName != g:
    for x in s.tools.presets:
      if x.group == newName: raise bad(cmdId, "a group named `" & newName & "` already exists")
    for x in s.tools.presets.mitems:
      if x.group == g:
        x.group = newName
        x.builtin = false
    s.brushPresetsChanged()
  %*{"group": newName}

proc deleteGroup(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "brush.presets.deleteGroup"
  let (g, f) = targetFolder(s, p, cmdId)
  let before = s.tools.presets.len
  var kept: seq[BrushPreset]
  for x in s.tools.presets:
    if not inFolder(x, g, f): kept.add x
  s.tools.presets = kept
  if s.tools.currentPreset.isSome and indexOfPreset(s.tools.presets, s.tools.currentPreset.get) < 0:
    s.tools.currentPreset = none(string)
  s.brushPresetsChanged()
  %*{"deleted": before - s.tools.presets.len, "count": s.tools.presets.len}

proc brushPresetSpecs*(): seq[CommandSpec] =
  @[cmd("brush.presets.rename", "Rename Brush", [], "", """{"name":string,"newName":string} → {name}""", enabledProc(""),
        runProc(renamePreset(s, p))),
    cmd("brush.presets.move", "Move Brush", [], "",
        """{"name":string,"group":string?=its group ("" = ungrouped),"folder":[name]? (nested folder names in the group, outermost first; [] = the group itself)=before's folder, else its own if it stays in its group, else [],"before":name? (preset to land before, in that folder) | "index":n? (position in the group)=end of the folder} → {name, group, folder, index}""",
        enabledProc(""), runProc(movePreset(s, p))),
    cmd("brush.presets.moveGroup", "Move Brush Group", [], "",
        """{"group":string,"folder":[name]? (move this nested folder among its sibling folders instead),"before":group? (or sibling folder name) | "index":n? (group or sibling folder position)=end} → {group, folder?, index}""",
        enabledProc(""), runProc(moveGroup(s, p))),
    cmd("brush.presets.renameGroup", "Rename Brush Group", [], "",
        """{"group":string,"folder":[name]? (rename this nested folder instead),"newName":string} → {group, folder?}""",
        enabledProc(""), runProc(renameGroup(s, p))),
    cmd("brush.presets.deleteGroup", "Delete Brush Group", [], "",
        """{"group":string,"folder":[name]? (delete this nested folder instead)} → {deleted, count}""",
        enabledProc(""), runProc(deleteGroup(s, p)))]
