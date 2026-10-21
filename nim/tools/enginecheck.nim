## Compares the Nim engine with the Rust oracle's `engine.script` (nim/PORTING.md): each script
## (a JSON list of `{"cmd", "params"}` steps) runs in a fresh session on both sides; the step
## results and errors must match, and so must the open documents' `.pcraft` manifests (tiles by
## content hash). `enginecheck ORACLE SCRIPT.json...`; exits 1 on a difference.
## `enginecheck --specs ORACLE` compares the registry (ids, labels, menus, shortcuts, params).

import std/[os, osproc, strutils, json, tables, math]
import photocraft/[engine, format, zstd]

const Volatile = ["generator"]

proc strip(n: JsonNode): JsonNode =
  case n.kind
  of JObject:
    result = newJObject()
    for k, v in n.fields:
      if k notin Volatile: result[k] = strip(v)
  of JArray:
    result = newJArray()
    for v in n.elems: result.add strip(v)
  else: result = n

proc sameNumber(a, b: JsonNode): bool =
  ## Numbers match when equal as f32 (Rust prints f32 fields in their shortest f32 form).
  let x = if a.kind == JInt: float64(a.num) else: a.fnum
  let y = if b.kind == JInt: float64(b.num) else: b.fnum
  x == y or float32(x) == float32(y) or (x != x and y != y)

proc firstDiff(a, b: JsonNode; path: string): string =
  if a.kind in {JInt, JFloat} and b.kind in {JInt, JFloat}:
    return (if sameNumber(a, b): "" else: path & ": " & $a & " vs " & $b)
  if a.kind != b.kind: return path & ": " & $a.kind & " vs " & $b.kind & " (" & ($a)[0 ..< min(($a).len, 120)] & " | " & ($b)[0 ..< min(($b).len, 120)] & ")"
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
    if a != b: return path & ": " & ($a)[0 ..< min(($a).len, 300)] & " vs " & ($b)[0 ..< min(($b).len, 300)]
  ""

proc expandBlobs(n: JsonNode; bundle: seq[byte]; depth = 0): JsonNode =
  ## Embedded smart object sources are `.pcraft` bundles whose bytes differ between the builds
  ## (generator, zip layout): compare their manifests instead of their hashes.
  case n.kind
  of JObject:
    result = newJObject()
    for k, v in n.fields:
      if k == "blob" and v.kind == JString and n{"kind"}.getStr == "embedded" and depth < 8:
        try:
          let raw = zstdDecompress(newZipSource(bundle).get("blobs/" & v.getStr & ".zst", 1 shl 30))
          if raw.len > 2 and raw[0] == byte('P') and raw[1] == byte('K'):
            result[k] = expandBlobs(strip(readManifest(raw)), raw, depth + 1)
            continue
        except CatchableError: discard
      result[k] = expandBlobs(v, bundle, depth)
  of JArray:
    result = newJArray()
    for v in n.elems: result.add expandBlobs(v, bundle, depth)
  else: result = n

proc bytesOf(path: string): seq[byte] =
  let t = readFile(path)
  result = newSeq[byte](t.len)
  for i, c in t: result[i] = byte(c)

proc runNim(steps: JsonNode): (JsonNode, seq[Document]) =
  resetIdsForTesting()
  clearSourceCache()
  discard purgeStackCache()
  var s = newSession()
  var log = newJArray()
  for step in steps.elems:
    let cmd = step{"cmd"}.getStr("")
    let params = if step.hasKey("params"): step["params"] else: newJNull()
    try: log.add %*{"ok": s.execute(cmd, params)}
    except EngineError as e: log.add %*{"err": e.msg}
    except CatchableError as e: log.add %*{"err": "UNCONVERTED " & e.msg}
    except Defect as e: log.add %*{"panic": true, "nim": e.msg}
  var docs: seq[Document]
  for d in s.documents: docs.add d.doc[]
  (log, docs)

proc compareSpecs(oracle: string): int =
  let (rs, _) = execCmdEx(quoteShell(oracle) & " engine.specs")
  var rust = initTable[string, JsonNode]()
  var order: seq[string]
  for l in rs.splitLines:
    if l.len == 0 or l[0] != '{': continue
    let j = parseJson(l)
    if not rust.hasKey(j["id"].getStr):
      rust[j["id"].getStr] = j
      order.add j["id"].getStr
  var bad = 0
  var seen = 0
  var nimOrder: seq[string]
  for c in commandSpecs():
    nimOrder.add c.id
    if not rust.hasKey(c.id):
      echo "EXTRA ", c.id
      inc bad
      continue
    inc seen
    var menu = newJArray()
    for m in c.menu: menu.add %m
    let mine = %*{"id": c.id, "label": c.label, "menu": menu,
                  "shortcut": (if c.shortcut.isSome: %c.shortcut.get else: newJNull()), "params": c.params,
                  "journal": c.journal}
    let d = firstDiff(rust[c.id], mine, c.id)
    if d.len > 0:
      echo "DIFF ", d
      inc bad
  # Ported commands keep the Rust order.
  var ri = 0
  for id in nimOrder:
    let k = order.find(id)
    if k < ri:
      echo "ORDER ", id
      inc bad
    ri = max(ri, k)
  echo seen, " of ", order.len, " commands ported, ", bad, " problems"
  bad

when isMainModule:
  let args = commandLineParams()
  if args.len >= 2 and args[0] == "--specs":
    quit(if compareSpecs(args[1]) > 0: 1 else: 0)
  let oracle = args[0]
  let tmp = getTempDir() / "enginecheck"
  var same, diff = 0
  for path in args[1 .. ^1]:
    removeDir(tmp)
    discard execCmdEx(quoteShell(oracle) & " engine.script " & quoteShell(path) & " " & quoteShell(tmp))
    if not fileExists(tmp / "log.json"):
      echo "SKIP ", path, " (the oracle failed)"
      continue
    let rustLog = parseFile(tmp / "log.json")
    let steps = parseFile(path)
    let (nimLog, docs) = runNim(steps)
    var problem = ""
    for i in 0 ..< max(rustLog.len, nimLog.len):
      if i >= rustLog.len or i >= nimLog.len:
        problem = "step count"
        break
      let d = firstDiff(rustLog[i], nimLog[i], "")
      if d.len > 0:
        problem = "step " & $i & " (" & steps[i]{"cmd"}.getStr & ")" & d
        break
    if problem.len == 0:
      var k = 0
      while fileExists(tmp / ("doc" & $k & ".pcraft")): inc k
      if k != docs.len: problem = "documents: rust " & $k & " nim " & $docs.len
      else:
        for i in 0 ..< k:
          let ra = bytesOf(tmp / ("doc" & $i & ".pcraft"))
          let rb = saveToBytes(docs[i])
          let a = expandBlobs(strip(readManifest(ra)), ra)
          let b = expandBlobs(strip(readManifest(rb)), rb)
          let d = firstDiff(a, b, "doc" & $i)
          if d.len > 0:
            problem = d
            break
    if problem.len == 0: inc same
    else:
      inc diff
      if diff <= 20 or existsEnv("ENGINECHECK_ALL"): echo "DIFF ", path, "\n  ", problem
  echo same, " identical, ", diff, " different"
  quit(if diff > 0: 1 else: 0)
