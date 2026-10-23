## EXR parts (port of `exr_cmds.rs`): what a multi-part or channel-grouped EXR file holds, and
## opening the chosen parts/groups as the layers of one document (a Maya/Arnold render writes one
## part per AOV). The query command feeds the File › Open EXR Parts… dialog, the CLI, MCP and the
## control channel alike.

import std/[json, options, strutils]
import ./core
import ./file_cmds
import ../io/exr_parts as xparts
import ../io/common as iocommon

proc strParamReq(p: JsonNode; key, cmdId: string): string =
  let v = p.pget(key).asStr
  if v.isNone or v.get.len == 0: raise bad(cmdId, "missing \"" & key & "\"")
  v.get

proc entriesJson(path: string): JsonNode =
  let bytes = readFileOr(path)
  var entries: seq[Entry]
  try: entries = xparts.entries(bytes)
  except EngineError: raise
  except CatchableError as e: raise other(path & ": " & e.msg)
  result = newJArray()
  for e in entries:
    let kind = (case e.kind
                of ekPart: "part"
                of ekGroup: "group"
                of ekCryptomatte: "cryptomatte")
    result.add %*{"kind": kind, "name": e.name, "view": (if e.view.isSome: %e.view.get else: newJNull()),
                  "width": e.width, "height": e.height, "channels": e.channels,
                  "objects": (if e.objects.isSome: %e.objects.get else: newJNull())}

proc selected(p: JsonNode; prefix: string): seq[string] =
  ## Collects `part/<name>` and `group/<name>` boolean params into the selection names.
  if not p.isObject: return
  for k, v in p.sortedPairs:
    if k.startsWith(prefix) and v.asBool == some(true): result.add k[prefix.len .. ^1]

proc openExrParts(s: var Session; p: JsonNode): JsonNode =
  const C = "file.openExrParts"
  let path = strParamReq(p, "path", C)
  let view = p.pget("view").asStr.get("both")
  if view notin ["both", "left", "right"]:
    raise bad(C, "view is \"" & view & "\"; it is \"both\", \"left\" or \"right\"")
  let sel = Selection(parts: selected(p, "part/"), groups: selected(p, "group/"), view: view,
                      precomp: p.pget("precomp").asBool.get(false))
  let bytes = readFileOr(path)
  var r: ImportResult
  try: r = importExrParts(stem(path), bytes, sel)
  except EngineError: raise
  except CatchableError as e: raise other(path & ": " & e.msg)
  let (i, color) = s.openDocument(r.document, some(path))
  %*{"document": i, "color": color, "warnings": r.warnings}

proc exrSpecs*(): seq[CommandSpec] =
  @[cmd("file.exrParts", "EXR Parts Info", [], "", """{"path":str (the EXR file)}""", enabledProc(nativeE(s)),
        runProc(entriesJson(strParamReq(p, "path", "file.exrParts")))),
    cmd("file.openExrParts", "Open EXR Parts…", ["File"], "",
        """{"path":str (the EXR file),"view":"both|left|right"="both","part/<name>":bool (import this part as a layer),"group/<name>":bool (import this channel group as a layer),"precomp":bool=false (add the additive beauty precomp from the AOVs)}""",
        enabledProc(nativeE(s)), runProc(openExrParts(s, p)))]
