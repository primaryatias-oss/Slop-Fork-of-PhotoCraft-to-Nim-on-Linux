## Window › Actions, as engine commands (port of `actions_cmds.rs`): `actions.record` / `stop` /
## `play` / `list` / `get` / `delete` / `move` / `rename` / `toggle`.
##
## The list lives on the session so the panel, the CLI, the control channel and MCP share one copy.
## Recording copies replayable journal entries into an action; a replayable command's journal
## entry replaces the entries of the commands it ran on its own behalf, so they are recorded once.
## Playback runs those steps with `Session.execute` and stops at the first error, leaving one
## history step per step that ran. Steps switched off with `actions.toggle` are skipped by
## playback, Batch and droplets. An untrusted session's `authorize` is called for every nested step.

import std/[json, options, strutils, unicode, algorithm, tables, math, sequtils]
import ../rmath
import ./core

proc alwaysE(s: Session): string = ""

proc newAction*(name: string; steps: seq[(string, JsonNode)] = @[]): Action = Action(name: name, steps: steps)

proc stepEnabled*(a: Action; step: int): bool = step notin a.disabled

proc touch(st: var ActionState) =
  if st.rev < high(uint64): inc st.rev

proc num(v: JsonNode): Option[int64] =
  ## `as_i64`, else a finite float rounded.
  if v.isNil: return
  if v.kind == JInt: return some(int64(v.num))
  let f = v.asF64
  if f.isSome and f.get == f.get and abs(f.get) != Inf: some(asI64(round(f.get))) else: none(int64)

proc resolveAction*(list: seq[Action]; p: JsonNode; cmdId: string): int =
  let v = p.pget("action")
  if v.isNil: raise bad(cmdId, "needs \"action\" (a name or an index)")
  let i = num(v)
  if i.isSome:
    if i.get < 0: raise bad(cmdId, "action index is negative")
    if i.get >= int64(list.len): raise bad(cmdId, "no action at index " & $i.get)
    return int(i.get)
  if v.kind == JString:
    if v.str.len == 0: raise bad(cmdId, "action name is empty")
    for k, a in list:
      if a.name == v.str: return k
    raise bad(cmdId, "no action named `" & v.str & "`")
  raise bad(cmdId, "\"action\" must be a name or an index")

proc fromStep(p: JsonNode; len: int): int =
  let v = p.pget("from")
  if v.isNull: return 0
  let i = num(v)
  if i.isNone: raise bad("actions.play", "\"from\" must be a step index")
  if i.get < 0: raise bad("actions.play", "\"from\" is negative")
  if i.get > int64(len): raise bad("actions.play", "\"from\" " & $i.get & " is past the last step (" & $len & ")")
  int(i.get)

proc playbackPlan*(s: Session; p: JsonNode): (Action, int) =
  ## Shared validation for headless and shell playback. Does not start playback.
  if s.actions.playing > 0 and s.actions.playbackStack.len == 0:
    raise bad("actions.play", "an action is already playing")
  let idx = resolveAction(s.actions.list, p, "actions.play")
  let action = s.actions.list[idx]
  if action.name in s.actions.playbackStack or (s.actions.recording.isSome and s.actions.recording.get[1] == idx):
    raise bad("actions.play", "recursive action call to `" & action.name & "`")
  if s.actions.playbackStack.len >= 16: raise bad("actions.play", "action nesting exceeds 16 levels")
  (action, fromStep(p, action.steps.len))

proc calledActions*(list: seq[Action]; steps: var seq[(string, JsonNode)]): seq[Action] =
  ## The actions that `steps` call through Play Action steps, followed transitively, in list order,
  ## so a droplet can run without the Actions panel that made it. Index references become names.
  proc targets(list: seq[Action]; steps: var seq[(string, JsonNode)]; depth: int; queue: var seq[(int, int)]) =
    for st in steps.mitems:
      if st[0] != "actions.play": continue
      var i: int
      try: i = resolveAction(list, st[1], "actions.play")
      except EngineError: continue
      if st[1].isObject:
        st[1] = st[1].copy
        st[1]["action"] = %list[i].name
      queue.add (i, depth)
  var queue: seq[(int, int)]
  targets(list, steps, 1, queue)
  var found = initOrderedTable[int, Action]()
  while queue.len > 0:
    let (i, depth) = queue.pop()
    # Playback stops at 16 levels, so deeper calls never run.
    if depth > 16 or i in found: continue
    if i < 0 or i >= list.len: continue
    var action = list[i]
    targets(list, action.steps, depth + 1, queue)
    found[i] = action
  var keys: seq[int]
  for k in found.keys: keys.add k
  keys.sort()
  for k in keys: result.add found[k]

proc pendingSteps*(s: Session; action: int): seq[(string, JsonNode)] =
  ## Replayable journal entries not yet committed to this action while recording.
  let start = (if s.actions.recording.isSome and s.actions.recording.get[1] == action: s.actions.recording.get[0]
               else: s.journal.len)
  for k in start ..< s.journal.len:
    if isReplayable(s.journal[k][0]): result.add s.journal[k]

proc flushRecording(s: var Session) =
  if s.actions.recording.isNone: return
  let idx = s.actions.recording.get[1]
  if idx >= s.actions.list.len: raise bad("actions.stop", "the action being recorded no longer exists")
  let steps = pendingSteps(s, idx)
  s.actions.recording = some((s.journal.len, idx))
  if steps.len > 0:
    s.actions.list[idx].steps.add steps
    s.actions.touch()

proc notPlaying(s: Session; cmdId: string) =
  if s.actions.playing > 0: raise bad(cmdId, "cannot edit actions during playback")

proc listActions(s: var Session): JsonNode =
  var actions = newJArray()
  for a in s.actions.list: actions.add %*{"name": a.name, "steps": a.steps.len}
  var recording = newJNull()
  if s.actions.recording.isSome:
    let i = s.actions.recording.get[1]
    if i < s.actions.list.len: recording = %s.actions.list[i].name
  %*{"actions": actions, "recording": recording}

proc getAction(s: var Session; p: JsonNode): JsonNode =
  let idx = resolveAction(s.actions.list, p, "actions.get")
  let a = s.actions.list[idx]
  var steps = newJArray()
  for (id, params) in a.steps: steps.add %*[id, params]
  %*{"name": a.name, "steps": steps, "disabled": a.disabled}

proc recordAction(s: var Session; p: JsonNode): JsonNode =
  const C = "actions.record"
  notPlaying(s, C)
  if s.actions.recording.isSome: raise bad(C, "already recording; stop first")
  var idx: int
  if not p.pget("action").isNil: idx = resolveAction(s.actions.list, p, C)
  else:
    var name: string
    let v = p.pget("name")
    if v.isNull:
      var n = s.actions.list.len + 1
      while true:
        var taken = false
        for a in s.actions.list:
          if a.name == "Action " & $n: taken = true
        if not taken: break
        inc n
      name = "Action " & $n
    elif v.kind == JString:
      if v.str.len == 0: raise bad(C, "\"name\" is empty")
      name = v.str
    else: raise bad(C, "\"name\" must be a string")
    for a in s.actions.list:
      if a.name == name: raise bad(C, "an action with this name already exists")
    s.actions.list.add newAction(name)
    s.actions.touch()
    idx = s.actions.list.high
  s.actions.recording = some((s.journal.len, idx))
  %*{"action": (if idx < s.actions.list.len: s.actions.list[idx].name else: ""), "index": idx}

proc stopRecording(s: var Session): JsonNode =
  notPlaying(s, "actions.stop")
  if s.actions.recording.isNone: raise bad("actions.stop", "not recording")
  let idx = s.actions.recording.get[1]
  flushRecording(s)
  s.actions.recording = none((int, int))
  let a = s.actions.list[idx]
  %*{"action": a.name, "steps": a.steps.len}

proc nestedFailure*(id: string; value: JsonNode): Option[string] =
  ## Nested playback returns a structured failure even though the command itself succeeded.
  if id != "actions.play": return
  let failure = value.pget("failed")
  if failure.isNil: return
  let step = failure.pget("step").asU64.get(0)
  some("action `" & value.pget("action").asStr.get("") & "` step " & $(if step == high(uint64): step else: step + 1) &
       " failed: " & failure.pget("error").asStr.get("unknown error"))

proc runRecorded(s: var Session; action: Action; frm: int): (uint64, JsonNode) =
  var ran = 0'u64
  var failed: JsonNode = nil
  for i, (id, params) in action.steps:
    if not action.stepEnabled(i) or i < frm: continue
    if not s.authorize.isNil:
      try: s.authorize(id, params)
      except CatchableError as e:
        failed = %*{"step": i, "id": id, "error": e.msg}
        break
    try:
      let value = s.execute(id, params.copy)
      let nf = nestedFailure(id, value)
      if nf.isSome:
        failed = %*{"step": i, "id": id, "error": nf.get}
        break
      inc ran
    except CatchableError as e:
      failed = %*{"step": i, "id": id, "error": e.msg}
      break
  (ran, failed)

proc beginPlayback*(s: var Session; action: Action): Option[(int, int)] =
  ## Suspend recording while a called action executes: keep its commands in the journal for
  ## diagnostics, but record just the named call in the parent action.
  flushRecording(s)
  result = s.actions.recording
  s.actions.recording = none((int, int))
  if s.actions.playing < high(uint8): inc s.actions.playing
  s.actions.playbackStack.add action.name

proc finishPlayback*(s: var Session; recording: Option[(int, int)]; action: Action; frm: int) =
  if s.actions.playbackStack.len > 0: s.actions.playbackStack.setLen(s.actions.playbackStack.len - 1)
  if s.actions.playing > 0: dec s.actions.playing
  if recording.isSome:
    s.actions.recording = some((s.journal.len, recording.get[1]))
    var params = %*{"action": action.name}
    if frm > 0: params["from"] = %frm
    s.journal.add ("actions.play", params)

proc playAction(s: var Session; p: JsonNode): JsonNode =
  let (action, frm) = playbackPlan(s, p)
  let recording = beginPlayback(s, action)
  let (ran, failed) = runRecorded(s, action, frm)
  finishPlayback(s, recording, action, frm)
  if failed.isNil: %*{"action": action.name, "ran": ran}
  else: %*{"action": action.name, "ran": ran, "failed": failed}

proc movedIndex*(index, frm, to: int): int =
  ## Follow a selected/recording row when a list item is moved.
  if index == frm: to
  elif frm < index and index <= to: index - 1
  elif to <= index and index < frm: index + 1
  else: index

proc sortedUnique(v: seq[int]): seq[int] =
  result = v
  result.sort()
  var k = 0
  for i, x in result:
    if i == 0 or x != result[k - 1]:
      result[k] = x
      inc k
  result.setLen(k)

proc moveItem(s: var Session; p: JsonNode): JsonNode =
  const C = "actions.move"
  notPlaying(s, C)
  let idx = resolveAction(s.actions.list, p, C)
  proc index(key: string): int =
    let v = p.pget(key).asU64
    if v.isNone or v.get > uint64(high(int)): raise bad(C, key & " must be a non-negative integer")
    int(v.get)
  let to = index("to")
  if not p.pget("step").isNil:
    let step = index("step")
    let len = s.actions.list[idx].steps.len + pendingSteps(s, idx).len
    if step >= len or to >= len: raise bad(C, "step or destination is out of range")
    if s.actions.recording.isSome and s.actions.recording.get[1] == idx: flushRecording(s)
    let moved = s.actions.list[idx].steps[step]
    s.actions.list[idx].steps.delete(step)
    s.actions.list[idx].steps.insert(moved, to)
    var d: seq[int]
    for i in s.actions.list[idx].disabled: d.add movedIndex(i, step, to)
    s.actions.list[idx].disabled = sortedUnique(d)
    result = %*{"action": s.actions.list[idx].name, "step": to}
    s.actions.touch()
  else:
    if to >= s.actions.list.len: raise bad(C, "destination is out of range")
    let moved = s.actions.list[idx]
    s.actions.list.delete(idx)
    result = %*{"action": moved.name, "index": to}
    s.actions.list.insert(moved, to)
    if s.actions.recording.isSome:
      let (start, rec) = s.actions.recording.get
      s.actions.recording = some((start, movedIndex(rec, idx, to)))
    s.actions.touch()

proc deleteAction(s: var Session; p: JsonNode): JsonNode =
  const C = "actions.delete"
  notPlaying(s, C)
  let idx = resolveAction(s.actions.list, p, C)
  let sv = p.pget("step")
  if not sv.isNil:
    let v = sv.asU64
    if v.isNone or v.get > uint64(high(int)): raise bad(C, "step must be a non-negative integer")
    let step = int(v.get)
    let len = s.actions.list[idx].steps.len + pendingSteps(s, idx).len
    if step >= len: raise bad(C, "step is out of range")
    # Commit pending entries before editing the action.
    if s.actions.recording.isSome and s.actions.recording.get[1] == idx: flushRecording(s)
    s.actions.list[idx].steps.delete(step)
    var d: seq[int]
    for i in s.actions.list[idx].disabled:
      if i != step: d.add(if i > step: i - 1 else: i)
    s.actions.list[idx].disabled = sortedUnique(d)
    result = %*{"action": s.actions.list[idx].name, "deletedStep": step, "steps": s.actions.list[idx].steps.len}
    s.actions.touch()
    return
  if s.actions.recording.isSome: raise bad(C, "stop recording before deleting an action")
  let name = s.actions.list[idx].name
  s.actions.list.delete(idx)
  s.actions.touch()
  %*{"deleted": name}

proc toggleAction(s: var Session; p: JsonNode): JsonNode =
  ## The Actions panel check marks: one step on or off, or every step.
  const C = "actions.toggle"
  notPlaying(s, C)
  let idx = resolveAction(s.actions.list, p, C)
  var enabled = none(bool)
  let ev = p.pget("enabled")
  if not ev.isNull:
    if ev.kind != JBool: raise bad(C, "\"enabled\" must be true or false")
    enabled = some(ev.bval)
  var step = none(int)
  let sv = p.pget("step")
  if not sv.isNull:
    let v = sv.asU64
    if v.isNone or v.get > uint64(high(int)): raise bad(C, "step must be a non-negative integer")
    step = some(int(v.get))
  let len = s.actions.list[idx].steps.len + pendingSteps(s, idx).len
  if step.isSome and step.get >= len: raise bad(C, "step is out of range")
  if s.actions.recording.isSome and s.actions.recording.get[1] == idx: flushRecording(s)
  template a: untyped = s.actions.list[idx]
  if step.isSome:
    if enabled.get(not a.stepEnabled(step.get)):
      a.disabled.keepItIf(it != step.get)
    else:
      a.disabled = sortedUnique(a.disabled & @[step.get])
  elif enabled.get(a.disabled.len > 0): a.disabled.setLen(0)
  else:
    var all: seq[int]
    for i in 0 ..< a.steps.len: all.add i
    a.disabled = all
  result = %*{"action": a.name, "disabled": a.disabled}
  s.actions.touch()

const PlayShortcutPrefix* = "actions.play:" ## shortcut override that plays a named action

proc renameAction(s: var Session; p: JsonNode): JsonNode =
  ## Rename an action in place. Nested `actions.play` steps that call it by name, and its
  ## function-key binding, follow the new name.
  const C = "actions.rename"
  notPlaying(s, C)
  let idx = resolveAction(s.actions.list, p, C)
  let v = p.pget("name")
  if v.isNil: raise bad(C, "needs \"name\"")
  if v.kind != JString: raise bad(C, "\"name\" must be a string")
  if unicode.strip(v.str).len == 0: raise bad(C, "\"name\" is empty")
  let name = v.str
  let old = s.actions.list[idx].name
  if old == name: return %*{"action": name, "index": idx}
  for a in s.actions.list:
    if a.name == name: raise bad(C, "an action with this name already exists")
  let pending = (if s.actions.recording.isSome: s.actions.recording.get[0] else: s.journal.len)
  for a in s.actions.list.mitems:
    for st in a.steps.mitems:
      if st[0] == "actions.play" and st[1].pget("action").asStr == some(old):
        st[1] = st[1].copy
        st[1]["action"] = %name
  for k in pending ..< s.journal.len:
    if s.journal[k][0] == "actions.play" and s.journal[k][1].pget("action").asStr == some(old):
      s.journal[k][1] = s.journal[k][1].copy
      s.journal[k][1]["action"] = %name
  s.actions.list[idx].name = name
  s.actions.touch()
  let (frm, to) = (PlayShortcutPrefix & old, PlayShortcutPrefix & name)
  if s.prefs.values.shortcuts.hasKey(frm):
    s.editPrefs(proc(pr: var Preferences) =
      if pr.shortcuts.hasKey(frm):
        let key = pr.shortcuts[frm]
        pr.shortcuts.del(frm)
        pr.shortcuts[to] = key
        pr.shortcuts.sortByKey())
  %*{"action": name, "index": idx}

proc actionsFromJson*(v: JsonNode): seq[Action] =
  ## `serde_json::from_value::<Vec<Action>>` with serde's messages (raises `ValueError`).
  proc fail(msg: string) = raise newException(ValueError, msg)
  if v.kind != JArray: fail(serdeInvalidType(v, "a sequence").msg)
  for e in v.elems:
    var a: Action
    case e.kind
    of JObject:
      let n = e.pget("name")
      if n.isNil: fail("missing field `name`")
      if n.kind != JString: fail(serdeInvalidType(n, "a string").msg)
      a.name = n.str
      let st = e.pget("steps")
      if not st.isNil:
        if st.kind != JArray: fail(serdeInvalidType(st, "a sequence").msg)
        for x in st.elems:
          if x.kind != JArray: fail(serdeInvalidType(x, "a tuple of size 2").msg)
          if x.len < 2: fail("invalid length " & $x.len & ", expected a tuple of size 2")
          if x.len > 2: fail("invalid length " & $x.len & ", expected fewer elements in array")
          if x.elems[0].kind != JString: fail(serdeInvalidType(x.elems[0], "a string").msg)
          a.steps.add (x.elems[0].str, x.elems[1])
      let d = e.pget("disabled")
      if not d.isNil:
        if d.kind != JArray: fail(serdeInvalidType(d, "a sequence").msg)
        var ds: seq[int]
        for x in d.elems:
          if x.kind == JInt:
            if x.num < 0: fail("invalid value: integer `" & $x.num & "`, expected usize")
            ds.add int(x.num)
          else: fail(serdeInvalidType(x, "usize").msg)
        a.disabled = sortedUnique(ds)
    else: fail(serdeInvalidType(e, "struct Action").msg)
    result.add a

proc actionsSpecs*(): seq[CommandSpec] =
  @[cmd("actions.move", "Move Action or Step", [], "",
        """{"action":name|index, "step":index?, "to":index}. Move a step within its action, or the action itself. to is the final zero-based index. Pending recorded steps can be moved.""",
        enabledProc(alwaysE(s)), runProc(moveItem(s, p)), journal = false),
    cmd("actions.list", "List Actions", [], "", "{} → {actions:[{name, steps:count}], recording:name|null}",
        enabledProc(alwaysE(s)), runProc(listActions(s)), journal = false),
    cmd("actions.get", "Get Action", [], "",
        """{"action":name|index} → {name, steps:[[id, params]…], disabled:[step…]} (steps is the shape file.automate.batch and droplets take; disabled lists the 0-based steps playback skips)""",
        enabledProc(alwaysE(s)), runProc(getAction(s, p)), journal = false),
    cmd("actions.record", "Record Action", [], "",
        """{"name":str? (new action, default "Action N")} or {"action":name|index} (append) → {action, index}""",
        enabledProc(alwaysE(s)), runProc(recordAction(s, p)), journal = false),
    cmd("actions.stop", "Stop Recording", [], "",
        "{} → {action, steps:count} (steps recorded since actions.record, queries and action-editing commands omitted)",
        enabledProc(alwaysE(s)), runProc(stopRecording(s)), journal = false),
    cmd("actions.play", "Play Action", [], "",
        """{"action":name|index, "from":step?} → {action, ran, failed?:{step, id, error}}. step and from are 0-based; steps switched off with actions.toggle are skipped. Stops at the first error (the command still returns ok, with failed set) and leaves one history step per step that ran. Nested calls are supported up to 16 levels; cycles are rejected. Recording stores a named call rather than its expanded commands. Each step is checked with Session::authorize when one is installed.""",
        enabledProc(alwaysE(s)), runProc(playAction(s, p)), journal = false),
    cmd("actions.delete", "Delete Action", [], "",
        """{"action":name|index, "step":index?} → {deleted:name} or {action, deletedStep, steps:count}. step is 0-based, including pending recorded steps. Whole-action deletion is refused while recording.""",
        enabledProc(alwaysE(s)), runProc(deleteAction(s, p)), journal = false),
    cmd("actions.toggle", "Toggle Action Steps", [], "",
        """{"action":name|index, "step":index?, "enabled":bool?} → {action, disabled:[step…]}. Switch one 0-based step (including pending recorded steps) on or off for playback, Batch and droplets; omitted enabled flips it. Without step, switch every step: on unless all already are, or as enabled says.""",
        enabledProc(alwaysE(s)), runProc(toggleAction(s, p)), journal = false),
    cmd("actions.rename", "Rename Action", [], "",
        """{"action":name|index, "name":str} → {action, index}. The name must not be empty or whitespace-only and must be unique. Nested Play Action steps and the action's function key follow the rename.""",
        enabledProc(alwaysE(s)), runProc(renameAction(s, p)), journal = false)]
