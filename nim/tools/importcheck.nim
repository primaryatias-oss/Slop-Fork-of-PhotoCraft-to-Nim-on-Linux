## Compares `io.importDoc` with the Rust oracle's `import` (nim/PORTING.md): for each file, the
## error or the warnings, and the saved `.pcraft` manifests (the whole document model, tiles by
## content hash) with the fresh ids left out. `importcheck ORACLE FILE...`; exits 1 on a difference.
## With `IMPORTCHECK_NO_TEXT_CACHE` set, the rendered pixels of type layers are left out too.

import std/[os, osproc, strutils, json, tables]
import photocraft/[io, format]

const Volatile = ["id", "contents_id", "generator"]

let noTextCache = existsEnv("IMPORTCHECK_NO_TEXT_CACHE")

proc strip(n: JsonNode): JsonNode =
  case n.kind
  of JObject:
    result = newJObject()
    let isText = noTextCache and n.hasKey("kind") and n["kind"].kind == JString and n["kind"].getStr == "text"
    for k, v in n.fields:
      if k notin Volatile and not (isText and k == "cache"): result[k] = strip(v)
  of JArray:
    result = newJArray()
    for v in n.elems: result.add strip(v)
  else: result = n

proc firstDiff(a, b: JsonNode; path: string): string =
  if a.kind != b.kind: return path & ": " & $a.kind & " vs " & $b.kind
  case a.kind
  of JObject:
    for k, v in a.fields:
      if not b.hasKey(k): return path & "." & k & ": missing in nim"
      let d = firstDiff(v, b[k], path & "." & k)
      if d.len > 0: return d
    for k, _ in b.fields:
      if not a.hasKey(k): return path & "." & k & ": missing in rust"
  of JArray:
    if a.len != b.len: return path & ": " & $a.len & " vs " & $b.len & " items"
    for i in 0 ..< a.len:
      let d = firstDiff(a[i], b[i], path & "[" & $i & "]")
      if d.len > 0: return d
  else:
    if a != b: return path & ": " & ($a)[0 ..< min(($a).len, 200)] & " vs " & ($b)[0 ..< min(($b).len, 200)]
  ""

proc bytesOf(path: string): seq[byte] =
  let t = readFile(path)
  result = newSeq[byte](t.len)
  for i, c in t: result[i] = byte(c)

when isMainModule:
  let args = commandLineParams()
  let oracle = args[0]
  let tmp = getTempDir() / "importcheck.pcraft"
  var same, diff = 0
  for path in args[1 .. ^1]:
    removeFile(tmp)
    let (rs, _) = execCmdEx(quoteShell(oracle) & " import " & quoteShell(path) & " " & quoteShell(tmp))
    var rustWarn: seq[string]
    var rustErr = ""
    for l in rs.splitLines:
      if l.startsWith("warning: "): rustWarn.add l[9 .. ^1]
      elif l.startsWith("ERR "): rustErr = l[4 .. ^1]
    if rustErr.len == 0 and not fileExists(tmp):
      echo "SKIP ", path, " (the Rust import panicked)"
      continue
    var nimErr = ""
    var r: ImportResult
    try: r = importDoc(path, bytesOf(path))
    except CatchableError as e: nimErr = e.msg
    var problem = ""
    if rustErr.len > 0 or nimErr.len > 0:
      if rustErr != nimErr: problem = "error: rust " & rustErr & " | nim " & nimErr
    elif rustWarn != r.warnings:
      problem = "warnings: rust " & rustWarn.join(" / ") & " | nim " & r.warnings.join(" / ")
    elif r.sourceReadOnly != true:
      problem = "not read-only"
    else:
      let a = strip(readManifest(bytesOf(tmp)))
      let b = strip(readManifest(saveToBytes(r.document)))
      problem = firstDiff(a, b, "")
    if problem.len == 0: inc same
    else:
      inc diff
      if diff <= 10 or existsEnv("IMPORTCHECK_ALL"): echo "DIFF ", path, "\n  ", problem
  echo same, " identical, ", diff, " different"
  quit(if diff > 0: 1 else: 0)
