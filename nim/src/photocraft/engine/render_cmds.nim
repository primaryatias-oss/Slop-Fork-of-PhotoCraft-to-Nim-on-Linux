## Filter › Render › Flame…, Picture Frame… and Tree… (port of `render_cmds.rs`): procedural
## renderers (`algo/render2`). They draw onto the active pixel layer (or a new layer with
## `newLayer: true`, or when the active layer has no pixels), inside the selection, in any colour
## mode and depth, as one undo step. Results report what was drawn (`layer`, `bounds`,
## `primitives`) so agents can verify them without screenshots.

import std/[json, options, math, strutils]
import ./core
import ../vector, ../rmath
import ../algo/render2 as arender2

# serde's derived forms of the specs: camelCase keys, missing fields take their defaults.
proc serdeCamel*(T: typedesc[FlameSpec]): bool = true
proc serdeCamel*(T: typedesc[TreeSpec]): bool = true
proc serdeCamel*(T: typedesc[FrameSpec]): bool = true

proc hasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")

proc parseColour(v: JsonNode): Option[array[4, float32]] =
  ## `[r, g, b(, a)]` (0–1) or `#rrggbb` → `[r, g, b, a]`.
  if v.isArray and v.len >= 3:
    proc c(j: int; d: float64): float32 = float32(if j < v.len: v.elems[j].asF64.get(d) else: d)
    return some([c(0, 0), c(1, 0), c(2, 0), c(3, 1)])
  if not v.isNil and v.kind == JString and v.str.len == 7 and v.str.startsWith("#"):
    proc ch(j: int): float32 =
      var t = v.str[j ..< j + 2]
      if t.startsWith("+"): t = t[1 .. ^1]
      if t.len == 0: return 0
      var n = 0
      for c in t:
        let d = case c
          of '0'..'9': ord(c) - ord('0')
          of 'a'..'f': ord(c) - ord('a') + 10
          of 'A'..'F': ord(c) - ord('A') + 10
          else: -1
        if d < 0: return 0
        n = n * 16 + d
      float32(n) / 255'f32
    return some([ch(1), ch(3), ch(5), 1'f32])
  none(array[4, float32])

proc normalized(p: JsonNode; colours: openArray[string]): JsonNode =
  ## Params as an object with the named colour keys normalized to arrays (invalid ones dropped).
  result = newJObject()
  if p.isObject:
    for k, v in p.pairs: result[k] = v
  for k in ["layer", "newLayer", "path"]:
    if result.hasKey(k): result.delete(k)
  for k in colours:
    if result.hasKey(k):
      let v = result[k]
      result.delete(k)
      let c = parseColour(v)
      if c.isSome: result[k] = %*[c.get[0], c.get[1], c.get[2], c.get[3]]

proc wholeNumbers(m: JsonNode; keys: openArray[string]) =
  ## Integer fields given as JSON floats round to the nearest integer (dialogs send every number as
  ## a float); a negative or too-large result is then still refused by the spec's parser.
  for k in keys:
    let v = m.pget(k)
    if not v.isNil and v.kind == JFloat and v.fnum == v.fnum and abs(v.fnum) != Inf:
      m[k] = %asI64(round(v.fnum))

proc specFrom[T](m: JsonNode; default: T): T =
  result = default
  try:
    withSerdeStrict: fromJ(m, result)
  except SerdeError as e: raise other("invalid parameters: " & e.msg)

proc squash(s: string): string = s.toLowerAscii.multiReplace((" ", ""), ("-", ""), ("_", ""))

proc indexParam(m: JsonNode; k: string; names: openArray[string]) =
  ## Accepts a 1-based number or a name (camelCase) for an enumerated option.
  let v = m.pget(k)
  if not v.isNil and v.kind == JString:
    let lower = squash(v.str)
    var i = 1
    for j, n in names:
      if squash(n) == lower:
        i = j + 1
        break
    m[k] = %i

proc flamePaths(s: Session; p: JsonNode): Option[seq[seq[(float32, float32)]]] =
  ## Flattened polylines of the path Flame follows: `path` (a saved path name, `"work"`), else the
  ## work path, else the first saved path. None when the document has no path.
  let d = s.active
  if d.isNil: return
  let doc = d.doc
  var path = none(Path)
  let n = p.pget("path").asStr
  if n.isSome and n.get.len > 0 and cmpIgnoreCase(n.get, "work") != 0:
    for sp in doc.paths:
      if sp.name == n.get:
        path = some(sp.path)
        break
  else:
    if doc.workPath.isSome: path = doc.workPath
    elif doc.paths.len > 0: path = some(doc.paths[0].path)
  if path.isNone: return
  var polys: seq[seq[(float32, float32)]]
  for pl in flattenPath(path.get, 0.5):
    var v: seq[(float32, float32)]
    for (x, y) in pl.pts: v.add (float32(x), float32(y))
    if pl.closed and v.len > 0: v.add v[0]
    if v.len > 0: polys.add v
  if polys.len > 0: some(polys) else: none(seq[seq[(float32, float32)]])

proc draw(s: var Session; label: string; p: JsonNode; prims: seq[Prim]; extra: JsonNode): JsonNode =
  ## Draws `prims` onto the target layer as one undo step.
  let wantNew = p.pget("newLayer").asBool.get(false)
  let tv = p.pget("layer").asU64
  let target = (if tv.isSome: some(LayerId(tv.get)) else: none(LayerId))
  let count = prims.len
  let (layer, created, used) = s.editWith(label, proc(doc: var Document; active: var Option[LayerId]): (LayerId, bool, Rect) =
    let id = (if target.isSome: target else: active)
    let l = (if id.isSome: doc.findLayer(id.get) else: nil)
    let raster = not l.isNil and l[].content.kind == lkRaster
    var lid: LayerId
    var created = false
    if wantNew or not raster:
      var name = label
      while name.endsWith("…"): name.setLen(name.len - "…".len)
      let nl = newLayer(name, LayerContent(kind: lkRaster, raster: newSurface(doc.pixelFormat)))
      lid = doc.insertAbove(active, nl)
      active = some(lid)
      created = true
    else:
      if id.isNone: raise other("no active layer")
      lid = id.get
    let selection = doc.selection
    var clip = doc.bounds
    if selection.isSome: clip = clip.intersect(selection.get.contentBounds)
    let surf = paintSurface(doc, lid, newJNull())
    let used = arender2.composite(surf[], prims, clip, selection)
    surf[].prune()
    (lid, created, used))
  result = %*{"layer": uint64(layer), "newLayer": created,
              "bounds": (if used.isEmpty: newJNull() else: %*[used.x0, used.y0, used.x1, used.y1]), "primitives": count}
  if extra.isObject:
    for k, v in extra.pairs: result[k] = v

proc canvasOf(s: Session): Rect =
  if s.active.isNil: raise noDocument()
  s.active.doc[].bounds

proc runFlame(s: var Session; p: JsonNode): JsonNode =
  var m = normalized(p, ["color"])
  if not m.pget("useCustomColor").asBool.get(m.hasKey("color")):
    if m.hasKey("color"): m.delete("color")
  if m.hasKey("useCustomColor"): m.delete("useCustomColor")
  let q = m.pget("quality")
  if not q.isNil and q.kind in {JInt, JFloat}:
    m["quality"] = %uint32(clamp(q.asF64.get(2.0), 0.0, 4.0))
  elif not q.isNil and q.kind == JString:
    let i = ["draft", "low", "medium", "high", "fine"].find(q.str)
    m["quality"] = %(if i < 0: 2 else: i) # unknown names take medium
  wholeNumbers(m, ["seed"])
  let spec = specFrom(m, defaultFlameSpec())
  let found = flamePaths(s, p)
  let usedPath = found.isSome
  var paths: seq[seq[(float32, float32)]]
  if found.isSome: paths = found.get
  else:
    let c = (if s.active.isNil: rect(0, 0, 1, 1) else: s.active.doc[].bounds)
    if spec.flameType == ftCandleLight:
      paths = @[@[(float32(c.x0) + float32(c.width) * 0.5'f32, float32(c.y0) + float32(c.height) * 0.75'f32)]]
    else: paths = @[defaultFlamePath(c)]
  let prims = flame(spec, paths)
  draw(s, "Flame…", p, prims, %*{"usedPath": usedPath})

proc runTree(s: var Session; p: JsonNode): JsonNode =
  var m = normalized(p, ["leavesColor", "branchesColor"])
  indexParam(m, "baseTreeType", treeTypeNames())
  if m.hasKey("leavesColor") and not m.hasKey("defaultLeaves"): m["defaultLeaves"] = %false
  if m.hasKey("branchesColor") and not m.hasKey("customBranchColor"): m["customBranchColor"] = %true
  wholeNumbers(m, ["baseTreeType", "lightDirection", "seed"])
  let spec = specFrom(m, defaultTreeSpec())
  let prims = tree(spec, canvasOf(s))
  let name = treeTypeNames()[int(clamp(spec.baseTreeType, 1'u32, 34'u32)) - 1]
  draw(s, "Tree…", p, prims, %*{"treeType": name})

proc runFrame(s: var Session; p: JsonNode): JsonNode =
  var m = normalized(p, ["vineColor", "flowerColor", "leafColor"])
  indexParam(m, "frame", FrameStyles)
  wholeNumbers(m, ["frame", "lines", "seed"])
  let spec = specFrom(m, defaultFrameSpec())
  let prims = pictureFrame(spec, canvasOf(s))
  let name = FrameStyles[int(clamp(spec.frame, 1'u32, uint32(FrameStyles.len))) - 1]
  draw(s, "Picture Frame…", p, prims, %*{"frame": name})

proc renderSpecs*(): seq[CommandSpec] =
  @[cmd("filter.render.flame", "Flame…", ["Filter", "Render"], "",
        """{"flameType":"oneFlameAlongPath|multipleFlamesAlongPath|multipleFlamesPathDirections|multipleFlamesVariousLength|candleLight|multipleFlamesOneDirection","length":1..1000=150,"randomizeLength":bool,"width":1..1000=40,"angle":-180..180=0,"interval":1..1000=60,"adjustIntervalForLoops":bool=true,"useCustomColor":bool,"color":color,"turbulent":0..100=25,"jag":0..100=25,"opacity":0..100=75,"flameLines":1..100=20,"flameBottomAlignment":0..100=20,"flameStyle":"normal|violent|flat","flameShape":"parallel|toCenter|spread|oval|pointed","randomizeShapes":bool,"seed":u32=0,"quality":"draft|low|medium|high|fine","path":text,"newLayer":bool} → {layer,newLayer,bounds,primitives,usedPath}""",
        hasDocE, runProc(runFlame(s, p))),
    cmd("filter.render.pictureFrame", "Picture Frame…", ["Filter", "Render"], "",
        """{"frame":"vineWithFlowers|vineWithLeaves|ivy|roses|daisies|berries|bamboo|waves|zigzag|dots|rope|scallops|doubleLine|snowflakes|stars|hearts","margin":0..30=4,"size":1..100=50,"arrangement":1..100=50,"lines":1..5=1,"thickness":1..100=30,"fade":0..100=0,"vineColor":color,"flowerColor":color,"leafColor":color,"seed":u32=0,"newLayer":bool} → {layer,newLayer,bounds,primitives,frame}""",
        hasDocE, runProc(runFrame(s, p))),
    cmd("filter.render.tree", "Tree…", ["Filter", "Render"], "",
        """{"baseTreeType":1..34=1,"lightDirection":1..5=3,"leavesAmount":0..100=50,"leavesSize":0..200=100,"branchesHeight":50..300=100,"branchesThickness":50..200=100,"defaultLeaves":bool=true,"leavesColor":color,"customBranchColor":bool,"branchesColor":color,"flatShading":bool,"seed":u32=0,"x":0..1=0.5,"y":0..1=0.95,"size":0.05..2=0.8,"newLayer":bool} → {layer,newLayer,bounds,primitives,treeType}""",
        hasDocE, runProc(runTree(s, p)))]
