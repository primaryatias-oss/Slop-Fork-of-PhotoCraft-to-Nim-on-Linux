## Patterns: Edit › Define Pattern, the pattern library (`pattern.*`), Layer › New Fill Layer ›
## Pattern, Edit › Fill with a pattern and brush textures from library patterns (port of
## `pattern_cmds.rs`). The library is app state on the session; documents keep a copy of every
## pattern their layers use. Part of the `engine/core` unit: included, not imported.

proc hash01(x, y, seed: uint32): float32 =
  var h = (x * 0x8da6_b343'u32) xor (y * 0xd816_3841'u32) xor (seed * 0xcb1a_b31f'u32)
  h = h xor (h shr 13)
  h = h * 0x5bd1_e995'u32
  h = h xor (h shr 15)
  float32(h and 0xffff) / 65535'f32

proc procedural(name: string; w, h: uint32; f: proc(x, y: uint32): array[3, float32]): DocPattern =
  let fmt = PixelFormat(mode: cmRgb, sample: stU8, alpha: false)
  var s = newSurface(fmt)
  var px = newSeqOfCap[float32](int(w * h * 3))
  for y in 0'u32 ..< h:
    for x in 0'u32 ..< w:
      for v in f(x, y): px.add v
  # Quantise through the 8-bit surface so ids are stable.
  s.writeRegion(rect(0, 0, int(w), int(h)), px)
  newPattern(name, s, int(w), int(h))

proc builtinPatterns*(): seq[DocPattern] =
  ## Built-in patterns, generated procedurally (our own, CC0), so the library is never empty.
  proc g(v: float32): array[3, float32] = [v, v, v]
  result.add procedural("Checkerboard", 16, 16, proc(x, y: uint32): array[3, float32] =
    g(if (x div 8 + y div 8) mod 2 == 0: 0.8'f32 else: 1'f32))
  result.add procedural("Diagonal Lines", 8, 8, proc(x, y: uint32): array[3, float32] =
    g(if (x + y) mod 8 < 2: 0.2'f32 else: 1'f32))
  result.add procedural("Dots", 12, 12, proc(x, y: uint32): array[3, float32] =
    let dx = float32(x) + 0.5'f32 - 6'f32
    let dy = float32(y) + 0.5'f32 - 6'f32
    g(clamp(sqrt(dx * dx + dy * dy) - 3'f32, 0'f32, 1'f32)))
  result.add procedural("Grid", 16, 16, proc(x, y: uint32): array[3, float32] =
    g(if x == 0 or y == 0: 0.55'f32 else: 1'f32))
  result.add procedural("Bricks", 32, 16, proc(x, y: uint32): array[3, float32] =
    let row = y div 8
    let xo = (x + (if row mod 2 == 1: 8'u32 else: 0'u32)) mod 16
    let mortar = y mod 8 == 7 or xo == 15
    let n = hash01(x, y, 7) * 0.08'f32
    if mortar: [0.78'f32 + n, 0.76'f32 + n, 0.72'f32 + n] else: [0.62'f32 + n, 0.27'f32 + n * 0.5'f32, 0.2'f32])
  result.add procedural("Weave", 16, 16, proc(x, y: uint32): array[3, float32] =
    let over = ((x div 4) + (y div 4)) mod 2 == 0
    let t = if over: float32(y mod 4) / 3'f32 else: float32(x mod 4) / 3'f32
    let v = 0.55'f32 + 0.35'f32 * (1'f32 - abs(2'f32 * t - 1'f32))
    [v, v * 0.92'f32, v * 0.8'f32])
  result.add procedural("Paper Noise", 64, 64, proc(x, y: uint32): array[3, float32] =
    # Two octaves of smoothed value noise (wraps at 64 px).
    proc oct(cell, seed: uint32): float32 =
      let cx = x div cell
      let cy = y div cell
      let fx = float32(x mod cell) / float32(cell)
      let fy = float32(y mod cell) / float32(cell)
      let n = 64'u32 div cell
      proc v(i, j: uint32): float32 = hash01(i mod n, j mod n, seed)
      proc sm(t: float32): float32 = t * t * (3'f32 - 2'f32 * t)
      let sx = sm(fx)
      let sy = sm(fy)
      let a = v(cx, cy) + (v(cx + 1, cy) - v(cx, cy)) * sx
      let b = v(cx, cy + 1) + (v(cx + 1, cy + 1) - v(cx, cy + 1)) * sx
      a + (b - a) * sy
    g(0.78'f32 + 0.14'f32 * oct(8, 1) + 0.08'f32 * oct(2, 2)))

proc newPatternLibrary*(): PatternLibrary = PatternLibrary(items: builtinPatterns())

proc hasPatternE(s: Session): string =
  if s.patterns.items.len == 0 and (s.active.isNil or s.active.doc.patterns.len == 0): "no patterns" else: ""

proc currentPresetPattern*(s: Session): Option[DocPattern] =
  ## The Patterns panel's selection, if it is still in the library.
  if s.presets.pattern.isNone: return
  for p in s.patterns.items:
    if p.id == s.presets.pattern.get: return some(p)

proc resolvePattern*(s: Session; key: string): Option[DocPattern] =
  ## Finds a pattern by id or name: the active document's first, then the library. An empty key
  ## picks the Patterns panel's selection, else the first library pattern.
  if key.len == 0:
    let c = currentPresetPattern(s)
    if c.isSome: return c
    if s.patterns.items.len > 0: return some(s.patterns.items[0])
    return
  let d = s.active
  if not d.isNil:
    let i = findPattern(d.doc.patterns, key, key)
    if i >= 0: return some(d.doc.patterns[i])
  let j = findPattern(s.patterns.items, key, key)
  if j >= 0: some(s.patterns.items[j]) else: none(DocPattern)

proc resolvePatternParam*(s: Session; cmdId: string; p: JsonNode): DocPattern =
  let key = p.getStrOr("pattern", "")
  let r = resolvePattern(s, key)
  if r.isNone: raise bad(cmdId, "no pattern \"" & key & "\" (see pattern.list)")
  r.get

proc ensureInDoc*(doc: var Document; pat: DocPattern) =
  ## Copies `pat` into the document's patterns unless it is already there (by id).
  for p in doc.patterns:
    if p.id == pat.id: return
  doc.patterns.add pat

proc resolveEffect*(s: Session; fx: Effect): (Effect, Option[DocPattern]) =
  ## Resolves the pattern of a Pattern Overlay built from params (its `name` holds the key).
  if fx.kind != ekPatternOverlay: return (fx, none(DocPattern))
  let key = if fx.id.len == 0: fx.name else: fx.id
  let pat = resolvePattern(s, key)
  if pat.isNone: raise bad("layer.layerStyle.patternOverlay", "no pattern \"" & key & "\" (see pattern.list)")
  var e = fx
  e.name = pat.get.name
  e.id = pat.get.id
  (e, pat)

proc patternInfo(p: DocPattern; inDoc, inLibrary: bool): JsonNode =
  let f = p.surface.format
  %*{"id": p.id, "name": patternDisplayName(p.name), "storedName": p.name, "width": p.width, "height": p.height,
     "mode": $f.mode, "depth": $f.sample, "alpha": f.alpha, "inDocument": inDoc, "inLibrary": inLibrary}

proc patternList(s: var Session; p: JsonNode): JsonNode =
  let docPats = if s.active.isNil: @[] else: s.active.doc.patterns
  result = newJArray()
  for q in s.patterns.items:
    var inDoc = false
    for d in docPats:
      if d.id == q.id: inDoc = true
    result.add patternInfo(q, inDoc, true)
  for d in docPats:
    var inLib = false
    for q in s.patterns.items:
      if q.id == d.id: inLib = true
    if not inLib: result.add patternInfo(d, true, false)

proc compositeSurface(doc: Document; r: Rect): Surface =
  ## The visible composite over `r` as a surface in the document's pixel format (with alpha),
  ## at the origin.
  let buf = render(doc, r)
  let df = doc.pixelFormat
  let fmt = PixelFormat(mode: df.mode, sample: df.sample, alpha: true)
  let n = fmt.channels
  var px = newSeq[float32](buf.px.len * n)
  for i, q in buf.px: discard fromRgbaInto(fmt, q, px.toOpenArray(i * n, i * n + n - 1))
  result = newSurface(fmt)
  result.writeRegion(rect(0, 0, r.width, r.height), px)

proc fileStemRust*(name: string): string =
  ## `Path::file_stem` of a file name: the last component without its extension.
  var base = name
  while base.len > 1 and base.endsWith("/"): base.setLen(base.len - 1)
  let slash = base.rfind('/')
  if slash >= 0: base = base[slash + 1 .. ^1]
  if base in ["", ".", ".."]: return ""
  let dot = base.rfind('.')
  if dot <= 0: base else: base[0 ..< dot]

proc defaultPatternName*(s: Session): string =
  ## The name Define Pattern gives a pattern without one: the document's name without its
  ## extension ("Pattern" without a document), numbered past the library's names.
  let docName = if s.active.isNil: "" else: s.active.doc.name
  var base = fileStemRust(docName)
  if base.len == 0: base = "Pattern"
  proc taken(n: string): bool =
    for q in s.patterns.items:
      if q.name == n: return true
  if not taken(base): return base
  var i = 2
  while true:
    let n = base & " " & $i
    if not taken(n): return n
    inc i

proc definePattern(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "edit.definePattern"
  let st = s.activeOrRaise()
  let doc = st.doc
  var r: Rect
  let a = p.pget("rect")
  if a.isArray and a.len == 4:
    var v: array[4, int]
    for i in 0 ..< 4: v[i] = asI32(round(a.elems[i].asF64.get(0.0)))
    r = rect(v[0], v[1], v[2], v[3])
  elif doc.selection.isSome: r = doc.selection.get.contentBounds
  else: r = doc[].bounds
  r = r.intersect(doc[].bounds)
  if r.isEmpty: raise bad(cmdId, "the area is empty")
  if r.width > 4000 or r.height > 4000: raise bad(cmdId, "patterns are limited to 4000 × 4000 px")
  var surf = compositeSurface(doc[], r)
  # Opaque patterns drop their alpha channel (as Photoshop does).
  let fmt = surf.format
  let px = surf.readRegion(rect(0, 0, r.width, r.height))
  let n = fmt.channels
  var opaque = true
  for i in 0 ..< px.len div n:
    if not (px[i * n + n - 1] >= 1): opaque = false
  if opaque: surf = surf.convert(PixelFormat(mode: fmt.mode, sample: fmt.sample, alpha: false))
  let nm = p.pget("name").asStr
  let name = if nm.isSome and nm.get.strip.len > 0: nm.get else: defaultPatternName(s)
  let pat = newPattern(name, surf, r.width, r.height)
  let id = pat.id
  let outp = patternInfo(pat, false, true)
  var have = false
  for q in s.patterns.items:
    if q.id == id: have = true
  if not have: s.patterns.items.add pat
  %*{"pattern": id, "info": outp}

proc libraryIndex(s: Session; cmdId: string; p: JsonNode): int =
  let key = p.pget("pattern").asStr
  if key.isNone: raise bad(cmdId, "missing `pattern` (id or name)")
  let j = findPattern(s.patterns.items, key.get, key.get)
  if j < 0: raise bad(cmdId, "no library pattern \"" & key.get & "\"")
  let found = s.patterns.items[j].id
  for i, q in s.patterns.items:
    if q.id == found: return i
  0

proc renamePattern(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "pattern.rename"
  let nm = p.pget("name").asStr
  if nm.isNone or nm.get.strip.len == 0: raise bad(cmdId, "missing `name`")
  let name = nm.get
  let i = libraryIndex(s, cmdId, p)
  let id = s.patterns.items[i].id
  s.patterns.items[i].name = name
  # A copy stored in the document is renamed too (a document edit, so undoable).
  let d = s.active
  var inDoc = false
  if not d.isNil:
    for q in d.doc.patterns:
      if q.id == id: inDoc = true
  if inDoc:
    s.editDo("Rename Pattern"):
      for q in doc.patterns.mitems:
        if q.id == id: q.name = name
  %*{"pattern": id, "name": name}

proc deletePattern(s: var Session; p: JsonNode): JsonNode =
  let i = libraryIndex(s, "pattern.delete", p)
  let pat = s.patterns.items[i]
  s.patterns.items.delete(i)
  %*{"deleted": pat.id}

proc importPatterns(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "pattern.import"
  let path = p.pget("path").asStr
  if path.isNone: raise bad(cmdId, "missing `path` (.pat file)")
  let bytes = readFileOr(path.get)
  var pats: seq[DocPattern]
  try: pats = readPat(bytes)
  except CatchableError as e: raise other(e.msg)
  var ids = newJArray()
  for pat in pats:
    ids.add %pat.id
    var found = false
    for q in s.patterns.items.mitems:
      if q.id == pat.id:
        q = pat
        found = true
        break
    if not found: s.patterns.items.add pat
  %*{"imported": ids}

proc exportPatterns(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "pattern.export"
  let path = p.pget("path").asStr
  if path.isNone: raise bad(cmdId, "missing `path`")
  var pats: seq[DocPattern]
  let keys = p.pget("patterns")
  if keys.isArray:
    for k in keys.elems:
      if k.kind != JString: continue
      let r = resolvePattern(s, k.str)
      if r.isNone: raise bad(cmdId, "no pattern \"" & k.str & "\"")
      pats.add r.get
  else: pats = s.patterns.items
  var bytes: seq[byte]
  try: bytes = writePat(pats)
  except CatchableError as e: raise other(e.msg)
  writeFileAtomic(path.get, bytes)
  %*{"path": path.get, "count": pats.len, "bytes": bytes.len}

proc patternPlacement*(p: JsonNode): (float32, float32, bool, (float32, float32)) =
  proc num(k: string; d: float64): float32 = float32(p.getF64Or(k, d))
  var phase = (0'f32, 0'f32)
  let a = p.pget("phase")
  if a.isArray:
    phase = (float32(if a.len > 0: a.elems[0].asF64.get(0.0) else: 0.0),
             float32(if a.len > 1: a.elems[1].asF64.get(0.0) else: 0.0))
  (clamp(num("scale", 100.0) / 100'f32, 0.01'f32, 10'f32), num("angle", 0.0), p.getBoolOr("link", true), phase)

proc insertPatternFillLayer*(doc: var Document; active: var Option[LayerId]; pat: DocPattern; p: JsonNode): LayerId =
  ## The edit of `layer.newFillLayer.pattern`.
  let (scale, angle, link, phase) = patternPlacement(p)
  ensureInDoc(doc, pat)
  let fill = Fill(kind: fkPattern, name: pat.name, scale: scale, id: pat.id, angle: angle, link: link, phase: phase)
  var l = newLayer(doc.nextLayerName("Pattern Fill"), fillContent(fill))
  maskNewLayer(doc, l, p, "layer.newFillLayer.pattern")
  result = doc.insertAbove(active, l)
  active = some(result)

proc newPatternFillLayer(s: var Session; p: JsonNode): JsonNode =
  let pat = resolvePatternParam(s, "layer.newFillLayer.pattern", p)
  let id = s.editGet("New Pattern Fill Layer", LayerId):
    insertPatternFillLayer(doc, active, pat, p)
  %*{"layer": id, "pattern": pat.id}

proc brushTexture(s: var Session; p: JsonNode): JsonNode =
  let pat = resolvePatternParam(s, "brush.texturePattern", p)
  # Brush textures are grey: luminance of the composite over white.
  var px = newSeq[Rgba](pat.width * pat.height)
  if px.len > 0: pat.surface.readRgbaInto(pat.rect, px)
  var lum = newSeq[float32](px.len)
  for i, q in px:
    let l = 0.299'f32 * q[0] + 0.587'f32 * q[1] + 0.114'f32 * q[2]
    lum[i] = l * q[3] + (1'f32 - q[3])
  let tile = grayTileFromF32(uint32(pat.width), uint32(pat.height), lum)
  s.tools.brush.texture.pattern = BrushPattern(kind: pkTile, tile: tile)
  s.tools.brush.texture.enabled = p.getBoolOr("enabled", true)
  let sc = p.pget("scale").asF64
  if sc.isSome: s.tools.brush.texture.scale = clamp(float32(sc.get) / 100'f32, 0.01'f32, 10'f32)
  %*{"pattern": pat.id}

proc averagePatternColor*(p: DocPattern): Color =
  ## A swatch colour for UIs (average of the tile), straight RGB.
  var px = newSeq[Rgba](p.width * p.height)
  if px.len > 0: p.surface.readRgbaInto(p.rect, px)
  let n = float32(max(px.len, 1))
  var s: array[3, float32]
  for q in px:
    s = [s[0] + q[0], s[1] + q[1], s[2] + q[2]]
  rgb(s[0] / n, s[1] / n, s[2] / n)

proc patternSpecs*(): seq[CommandSpec] =
  @[
    cmd("edit.definePattern", "Define Pattern…", ["Edit"], "",
        """{"name":text,"rect":[x0,y0,x1,y1]?} → {"pattern":id} (name: default the document's name numbered past the library's names; rect: default the selection bounds or else the canvas; added to the library from the visible composite)""",
        hasDocE, definePattern),
    cmd("pattern.list", "Patterns", [], "", "{} → [{id,name,width,height,mode,depth,inDocument,inLibrary}]", always,
        patternList, journal = false),
    cmd("pattern.rename", "Rename Pattern", [], "", """{"pattern":id|name,"name":str}""", hasPatternE, renamePattern),
    cmd("pattern.delete", "Delete Pattern", [], "", """{"pattern":id|name} (library only; documents keep their copy)""",
        hasPatternE, deletePattern),
    cmd("pattern.import", "Import Patterns…", [], "", """{"path":".pat file"}""", always, importPatterns),
    cmd("pattern.export", "Export Patterns…", [], "", """{"path":".pat file","patterns":[id|name]? (default: the whole library)}""",
        hasPatternE, exportPatterns),
    cmd("layer.newFillLayer.pattern", "Pattern…", ["Layer", "New Fill Layer"], "",
        """{"pattern":id|name?=first library pattern,"scale":1..1000=100,"angle":deg=0,"link":bool=true,"phase":[x,y]?,"path":"work"|saved path name|{…path}? (the active path: becomes the vector mask; else the selection is the layer mask)}""",
        hasDocE, newPatternFillLayer),
    cmd("brush.texturePattern", "Brush Texture Pattern", [], "",
        """{"pattern":id|name,"scale":1..1000?,"enabled":bool=true} (sets the session brush's Texture to that pattern)""",
        hasPatternE, brushTexture),
  ]
