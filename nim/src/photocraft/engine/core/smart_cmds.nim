## Smart Objects and Smart Filters (`smart_cmds.rs`, `smart_cmds/stack_cache.rs`,
## `smart_cmds/unpack.rs`, `link_watch.rs`): Layer › Smart Objects, Layer › Smart Filter and
## Filter › Convert for Smart Filters. Part of the `engine/core` unit: included, not imported.
##
## A smart object keeps its source (embedded file bytes, or a linked path) plus a transform and a
## stack of smart filters; its cache is the rendered appearance the compositors draw:
##
##   source bytes ──decode + composite (cached by content hash)──► source image
##                ──transform (exact shift, or bicubic warp)──────► placed (document pixels)
##                ──each visible smart filter, blended────────────► filtered
##                ──filter mask mix (unfiltered ↔ filtered)───────► cache
##
## The placed stage and the result after each filter are cached too (the stage cache), so a
## re-render starts from the deepest stage it shares with an earlier one.

const PlacedKeys = ["SoLd", "PlLd", "SoLE"]
  ## PSD placed-layer keys that describe the smart object's source; stale once we change it.

# ---------------------------------------------------------------- source encoding / decoding

proc encodeSource*(doc: Document): seq[byte] =
  ## Embedded smart-object contents: an in-memory `.pcraft` bundle of `doc`.
  try: saveToBytes(doc)
  except EngineError: raise
  except AllocationError: raise
  except CatchableError as e: raise other("can't encode smart object contents: " & e.msg)

proc decodeSource*(fileName: string; bytes: openArray[byte]): Document =
  ## Smart-object contents (a `.pcraft` bundle, PSD/PSB, or any flat image format).
  try: fileImport(fileName, bytes)
  except EngineError as e: raise other("can't read smart object contents \"" & fileName & "\": " & e.msg)

proc readSourceFile(path: string): Option[seq[byte]] =
  if path.len == 0: return
  try: some(readBytesRust(path))
  except OSError: none(seq[byte])

proc bytesOf(b: SharedBytes): SharedBytes = (if b.isNil: SharedBytes() else: b)

proc embeddedSourceBytes(meta: Metadata; src: SmartSource): Option[(string, SharedBytes)] =
  ## Memory-only resolution also covers Photoshop's embedded files addressed by a `Linked` UUID.
  case src.kind
  of ssEmbedded: some((src.fileName, bytesOf(src.bytes)))
  of ssLinked:
    let f = findLinkedFile(meta, src.path)
    if f.isSome: some((f.get.fileName, SharedBytes(data: f.get.bytes))) else: none((string, SharedBytes))

proc sourceBytes*(meta: Metadata; src: SmartSource): Option[(string, SharedBytes)] =
  ## The source file of a smart object: embedded bytes, the PSD's embedded linked-layer data (PSD
  ## import keeps the placed layer's uuid as the path), or a linked file on disk.
  result = embeddedSourceBytes(meta, src)
  if result.isNone and src.kind == ssLinked:
    let b = readSourceFile(src.path)
    if b.isSome: result = some((baseName(src.path), SharedBytes(data: b.get)))

proc authorizedSourceBytes(s: Session; cmdId: string; meta: Metadata; src: SmartSource): Option[(string, SharedBytes)] =
  ## The actual path is stored in the document, not in the command's params: recheck it before
  ## reading, reusing embedded PSD bytes rather than parsing their global blocks a second time.
  let e = embeddedSourceBytes(meta, src)
  if e.isSome: return e
  if src.kind != ssLinked: return
  if not s.authorize.isNil: s.authorize(cmdId, %*{"path": src.path})
  let b = readSourceFile(src.path)
  if b.isSome: some((baseName(src.path), SharedBytes(data: b.get))) else: none((string, SharedBytes))

proc authorizeSource(s: Session; cmdId: string; doc: Document; src: SmartSource) =
  ## Recheck a source's actual path before an untrusted command reads it. Embedded PSD UUIDs
  ## never reach the filesystem.
  if not s.authorize.isNil and src.kind == ssLinked and embeddedSourceBytes(doc.metadata, src).isNone:
    s.authorize(cmdId, %*{"path": src.path})

# ---------------------------------------------------------------- decoded source cache

type
  SourceImage* = object
    ## A decoded, composited source in its own pixel space (`bounds` starts at the origin).
    surface*: Surface
    bounds*: Rect
  SourceKey = (array[32, byte], PixelFormat)
  SourceEntry = object
    key: SourceKey
    image: SourceImage
    decodes: uint32

var sourceCache: seq[SourceEntry]
  ## Decoded source composites by content hash, most recent last.
const
  SourceCacheEntries = 32
  SourceCacheBytes = 1 shl 30

proc cacheKey(bytes: openArray[byte]; fmt: PixelFormat): SourceKey = (blake3(bytes), fmt)

proc cacheGet(key: SourceKey): Option[SourceImage] =
  for i, e in sourceCache:
    if e.key == key:
      let entry = sourceCache[i]
      sourceCache.delete(i)
      sourceCache.add entry
      return some(entry.image)

proc cachePut(key: SourceKey; image: SourceImage) =
  var decodes = 1'u32
  for i, e in sourceCache:
    if e.key == key:
      decodes = e.decodes + 1
      sourceCache.delete(i)
      break
  sourceCache.add SourceEntry(key: key, image: image, decodes: decodes)
  proc size(e: SourceEntry): int = e.image.bounds.width * e.image.bounds.height * e.key[1].channels * 4
  proc total(): int =
    for e in sourceCache: result += size(e)
  while sourceCache.len > SourceCacheEntries or (sourceCache.len > 1 and total() > SourceCacheBytes):
    sourceCache.delete(0)

proc sourceDecodeCount*(bytes: openArray[byte]; fmt: PixelFormat): uint32 =
  ## How many times these contents were decoded at this pixel format (0 = never / evicted).
  let key = cacheKey(bytes, fmt)
  for e in sourceCache:
    if e.key == key: return e.decodes

proc clearSourceCache*() =
  ## Forget every decoded source (a fresh process, for cross-checks: a decode reserves ids).
  sourceCache.setLen(0)

proc bufferToSurface(buf: Buffer; fmt: PixelFormat): Surface =
  let n = fmt.channels
  var data = newSeq[float32](buf.px.len * n)
  var tmp: array[8, float32]
  for i, p in buf.px:
    discard fromRgbaInto(fmt, p, tmp)
    for c in 0 ..< n: data[i * n + c] = tmp[c]
  result = newSurface(fmt)
  if not buf.rect.isEmpty: result.writeRegion(buf.rect, data)
  result.prune()

proc isSvg*(bytes: openArray[byte]): bool =
  ## Does this look like an SVG document (plain XML; `.svgz` is recognised by its extension)?
  var head = newString(min(bytes.len, 4096))
  for i in 0 ..< head.len: head[i] = char(bytes[i])
  var t = head
  while t.startsWith("\xEF\xBB\xBF"): t = t[3 .. ^1]
  t = t.strip(trailing = false)
  if t.startsWith("<svg"): return true
  (t.startsWith("<?xml") or t.startsWith("<!DOCTYPE") or t.startsWith("<!--")) and t.contains("<svg")

proc svgSourceImage*(bytes: openArray[byte]; fmt: PixelFormat; scale: float64): SourceImage =
  ## An SVG source rendered at `scale` times its own size. SVG rendering is not ported yet.
  raise other("can't render the SVG smart object: this format is not available in this build yet")

proc vectorScale(sm: SmartObject): float64 =
  ## The larger axis scale of the transform in 1/16 steps, between 1/16 and 64.
  let m = sm.transform.m
  let k = max(sqrt(m[0] * m[0] + m[1] * m[1]), sqrt(m[2] * m[2] + m[3] * m[3]))
  if k != k or abs(k) == Inf: return 1.0
  clamp(ceil(k * 16.0) / 16.0, 1.0 / 16.0, 64.0)

proc sourceImage*(fileName: string; bytes: openArray[byte]; fmt: PixelFormat): SourceImage =
  ## The composited source image of `bytes` in pixel format `fmt` (decoded once, then cached).
  let key = cacheKey(bytes, fmt)
  let hit = cacheGet(key)
  if hit.isSome: return hit.get
  let doc = decodeSource(fileName, bytes)
  let buf = flatten(doc)
  result = SourceImage(surface: bufferToSurface(buf, fmt), bounds: doc.bounds)
  cachePut(key, result)

proc stackImage*(fileName: string; bytes: openArray[byte]; fmt: PixelFormat; mode: StackMode): SourceImage =
  ## Layer › Smart Objects › Stack Mode: the source's layers (or, when it holds a single group, the
  ## group's layers) combined per pixel with `mode`.
  var key = cacheKey(bytes, fmt)
  var h = newBlake3()
  h.update(key[0])
  let tag = "stack:" & $mode
  h.update(tag.toOpenArrayByte(0, tag.high))
  key[0] = h.finish
  let hit = cacheGet(key)
  if hit.isSome: return hit.get
  let doc = decodeSource(fileName, bytes)
  let layers = (if doc.layers.len == 1 and doc.layers[0].isGroup: doc.layers[0].content.group.children
                else: doc.layers)
  let bounds = doc.bounds
  var frames: seq[seq[array[4, float32]]]
  for l in layers:
    if not l.visible: continue
    var one = l
    one.blend = bmNormal
    one.clipped = false
    frames.add renderLayer(one, bounds).px
  if frames.len == 0: raise other("the smart object's contents have no visible layers")
  let stat: astack.Stat = case mode
    of smEntropy: astack.stEntropy
    of smKurtosis: astack.stKurtosis
    of smMaximum: astack.stMaximum
    of smMean: astack.stMean
    of smMedian: astack.stMedian
    of smMinimum: astack.stMinimum
    of smRange: astack.stRange
    of smSkewness: astack.stSkewness
    of smStandardDeviation: astack.stStandardDeviation
    of smSummation: astack.stSummation
    of smVariance: astack.stVariance
  let buf = Buffer(rect: bounds, px: astack.combine(frames, stat))
  result = SourceImage(surface: bufferToSurface(buf, fmt), bounds: bounds)
  cachePut(key, result)

# ---------------------------------------------------------------- stage cache (stack_cache.rs)

type
  StageKey = array[32, byte]
  Stage = object
    surface: Surface
    extent: Rect ## where filters repeat edge pixels: the canvas ∪ the placed content
  StageEntry = object
    key: StageKey
    stage: Stage
    bytes: int

var stageCache: seq[StageEntry] ## most recently used last
const
  StageEntries = 64
  StageBudget* = 768 shl 20 ## accounted tile bytes; a stage larger than this alone isn't kept

proc hashText(h: var Blake3; t: string) = h.update(t.toOpenArrayByte(0, t.high))
proc hashF64(h: var Blake3; v: float64) =
  var b: array[8, byte]
  let u = cast[uint64](v)
  for i in 0 ..< 8: b[i] = byte((u shr (8 * i)) and 0xFF)
  h.update(b)

proc placedKey(bytes: openArray[byte]; fmt: PixelFormat; sm: SmartObject; canvas: Rect): StageKey =
  ## The first stage: placing `bytes` through the transform, warp or perspective.
  var h = newBlake3()
  h.update(bytes)
  for v in sm.transform.m: h.hashF64(v)
  h.hashText($fmt.mode & "|" & $fmt.sample & "|" & $fmt.alpha & "|" & $toJ(sm.warp) & "|")
  if sm.perspective.isSome:
    for v in sm.perspective.get: h.hashF64(v)
  h.hashText("|" & (if sm.stackMode.isSome: $sm.stackMode.get else: "none") & "|" & $canvas.x0 & "," &
             $canvas.y0 & "," & $canvas.x1 & "," & $canvas.y1)
  h.finish

proc filterKey(below: StageKey; f: SmartFilter): StageKey =
  ## The stage after applying `f` to stage `below`.
  var h = newBlake3()
  h.update(below)
  h.hashText(f.command & "\0" & jsonText(f.params) & "\0" & $f.blend & "\0")
  h.hashF64(float64(f.opacity))
  h.finish

proc surfaceBytes(s: Surface): int =
  # Tiles shared with the document (or another stage) are charged anyway: the cache is what keeps
  # them alive once the document moves on.
  result = s.defaultBytes.len
  for c in s.sortedCoords: result = result +% s.tile(c).bytes.len

proc stageGet(key: StageKey): Option[Stage] =
  for i, e in stageCache:
    if e.key == key:
      let entry = stageCache[i]
      stageCache.delete(i)
      stageCache.add entry
      return some(entry.stage)

proc stagePut(key: StageKey; stage: Stage) =
  let bytes = surfaceBytes(stage.surface)
  if bytes > StageBudget: return
  stageCache.keepItIf(it.key != key)
  stageCache.add StageEntry(key: key, stage: stage, bytes: bytes)
  proc total(): int =
    for e in stageCache: result += e.bytes
  while stageCache.len > StageEntries or total() > StageBudget: stageCache.delete(0)

proc stackCacheBytes*(): int =
  ## Bytes held by rendered smart filter stages.
  for e in stageCache: result += e.bytes

proc purgeStackCache*(): int =
  ## Drop every rendered smart filter stage (Edit › Purge › All). Returns the bytes released.
  result = stackCacheBytes()
  stageCache.setLen(0)

# ---------------------------------------------------------------- rendering

proc blendSurfaces(base, top: Surface; mode: BlendMode; opacity: float32): Surface =
  ## `top` blended over `base` with `mode` at `opacity` (a smart filter's blending options).
  let fmt = base.format
  let area = base.contentBounds.union(top.contentBounds)
  result = base
  if area.isEmpty: return
  let n = fmt.channels
  let b = base.readRegion(area)
  let t = top.readRegion(area)
  var o = newSeq[float32](b.len)
  var tmp: array[8, float32]
  for i in 0 ..< b.len div n:
    let rgba = composite(mode, toRgba(fmt, b.toOpenArray(i * n, i * n + n - 1)),
                         toRgba(fmt, t.toOpenArray(i * n, i * n + n - 1)), opacity)
    discard fromRgbaInto(fmt, rgba, tmp)
    for c in 0 ..< n: o[i * n + c] = tmp[c]
  result.writeRegion(area, o)
  result.prune()

proc maskMix(base, filtered: Surface; mask: LayerMask): Surface =
  ## Mixes `filtered` over `base` through the filter mask (premultiplied, any channel layout).
  let fmt = base.format
  let area = base.contentBounds.union(filtered.contentBounds)
  result = filtered
  if area.isEmpty: return
  let n = fmt.channels
  let b = base.readRegion(area)
  let f = filtered.readRegion(area)
  var k: seq[float32]
  mask.valuesInto(area, k)
  var o = newSeq[float32](b.len)
  for i in 0 ..< b.len div n:
    let m = clamp((if i < k.len: k[i] else: 1'f32), 0'f32, 1'f32)
    let at = i * n
    if not fmt.alpha:
      for c in 0 ..< n: o[at + c] = b[at + c] + (f[at + c] - b[at + c]) * m
      continue
    let a = n - 1
    let oa = b[at + a] + (f[at + a] - b[at + a]) * m
    o[at + a] = oa
    for c in 0 ..< a:
      let pm = b[at + c] * b[at + a] + (f[at + c] * f[at + a] - b[at + c] * b[at + a]) * m
      o[at + c] = (if oa > 0: clamp(pm / oa, 0'f32, 1'f32) else: 0'f32)
  result.writeRegion(area, o)
  result.prune()

proc applySmartFilter(f: SmartFilter; cur: Surface; canvas, extent: Rect): Surface =
  ## One visible smart filter over `cur`, blended with its blend mode and opacity. Unknown ids
  ## (e.g. Photoshop filters we don't implement) leave the pixels alone.
  let o = applyFilterToSurface(f.command, f.params, cur, canvas, none(Surface), extent)
  if o.isNone: return cur
  if f.blend == bmNormal and f.opacity >= 1: o.get
  else: blendSurfaces(cur, o.get, f.blend, clamp(f.opacity, 0'f32, 1'f32))

proc applySmartFilters*(placed: Surface; sm: SmartObject; canvas: Rect): Surface =
  ## Runs the smart filter stack on the placed (unfiltered) content.
  if not sm.filtersEnabled or not sm.smartFilters.anyIt(it.visible): return placed
  var cur = placed
  # Edge pixels repeat at the canvas ∪ placed-content edge, as for layer filters.
  let extent = canvas.union(placed.contentBounds)
  for f in sm.smartFilters:
    if f.visible: cur = applySmartFilter(f, cur, canvas, extent)
  if sm.filterMask.isSome and sm.filterMask.get.enabled: maskMix(placed, cur, sm.filterMask.get) else: cur

proc placeSmart(name: string; bytes: openArray[byte]; fmt: PixelFormat; sm: SmartObject): Surface =
  ## The source placed in document pixels, before any smart filter.
  # A vector source is rendered at the placement's scale, and the placement is divided by that
  # scale so the rendered pixels land where the source units would.
  if sm.stackMode.isNone and sm.warp.isNone and isSvg(bytes):
    let k = vectorScale(sm)
    let img = svgSourceImage(bytes, fmt, k)
    if sm.perspective.isSome:
      let p = sm.perspective.get
      let h = homography([p[0] / k, p[1] / k, p[2], p[3] / k, p[4] / k, p[5], p[6] / k, p[7] / k, p[8]])
      return placeSourceProjective(img.surface, img.bounds, h, none(Warp))
    let m = sm.transform.m
    return placeSource(img.surface, img.bounds, Affine(m: [m[0] / k, m[1] / k, m[2] / k, m[3] / k, m[4], m[5]]),
                       none(Warp))
  let img = (if sm.stackMode.isSome: stackImage(name, bytes, fmt, sm.stackMode.get) else: sourceImage(name, bytes, fmt))
  # Through the warp (source space) and the transform in one pass; whole-pixel moves are exact.
  if sm.perspective.isSome: placeSourceProjective(img.surface, img.bounds, homography(sm.perspective.get), sm.warp)
  else: placeSource(img.surface, img.bounds, sm.transform, sm.warp)

proc renderIn(doc: Document; sm: SmartObject; ctx: JobCtx): Option[Surface] =
  ## Renders a smart object from its source, starting from the deepest stage of the stack already
  ## rendered. `none` when the source is unavailable (callers keep the existing cache).
  let sb = sourceBytes(doc.metadata, sm.source)
  if sb.isNone: return
  let (name, bytes) = sb.get
  let fmt = doc.pixelFormat
  let canvas = doc.bounds
  let pk = placedKey(bytes.data, fmt, sm, canvas)
  proc placed(): Stage =
    let hit = stageGet(pk)
    if hit.isSome: return hit.get
    let surface = placeSmart(name, bytes.data, fmt, sm)
    # Edge pixels repeat at the canvas ∪ placed-content edge, as for layer filters.
    result = Stage(extent: canvas.union(surface.contentBounds), surface: surface)
    stagePut(pk, result)
  var filters: seq[SmartFilter]
  for f in sm.smartFilters:
    if sm.filtersEnabled and f.visible: filters.add f
  if filters.len == 0: return some(placed().surface)
  var keys: seq[StageKey]
  var key = pk
  for f in filters:
    key = filterKey(key, f)
    keys.add key
  var done = 0
  var cur: Stage
  var found = false
  for i in countdown(keys.high, 0):
    let hit = stageGet(keys[i])
    if hit.isSome:
      done = i + 1
      cur = hit.get
      found = true
      break
  if not found: cur = placed()
  let n = float32(filters.len)
  for i in done ..< filters.len:
    ctx.check()
    ctx.progress(float32(i) / n, "")
    cur = Stage(surface: applySmartFilter(filters[i], cur.surface, canvas, cur.extent), extent: cur.extent)
    stagePut(keys[i], cur)
  ctx.progress(1.0, "")
  if sm.filterMask.isSome and sm.filterMask.get.enabled: some(maskMix(placed().surface, cur.surface, sm.filterMask.get))
  else: some(cur.surface)

proc render*(doc: Document; sm: SmartObject): Option[Surface] =
  ## Renders a smart object from its source; `none` when the source is unavailable.
  renderIn(doc, sm, newJobCtx())

proc renderBelowFilter*(doc: Document; sm: SmartObject; index: int): Option[Surface] =
  ## The smart object's pixels below smart filter `index` (the input that filter edits).
  var below = sm
  if index < below.smartFilters.len: below.smartFilters.setLen(max(index, 0))
  below.filterMask = none(LayerMask)
  render(doc, below)

proc refreshLayerIn(doc: Document; l: var Layer; ctx: JobCtx): bool =
  if l.content.kind != lkSmart: return false
  let px = renderIn(doc, l.content.smart, ctx)
  if px.isNone: return false
  l.content.smart.cache = px
  true

proc refreshSmartLayer*(doc: Document; l: var Layer): bool {.discardable.} =
  ## Re-renders smart layer `l` (which lives in `doc`) in place. False if its source is
  ## unavailable (the cache is then left alone).
  refreshLayerIn(doc, l, newJobCtx())

proc refreshIn(doc: var Document; id: LayerId; ctx: JobCtx): bool =
  let lp = doc.layer(id)
  if lp.isNone: raise noLayer(id)
  var l = lp.get
  if l.content.kind != lkSmart: raise other("the layer is not a smart object")
  result = refreshLayerIn(doc, l, ctx)
  if result:
    let dst = doc.layerPtr(id)
    if not dst.isNil: dst[].content = l.content

proc smartRefresh*(doc: var Document; id: LayerId): bool =
  ## Re-renders the smart object `id` of `doc` (`smart_cmds::refresh`).
  refreshIn(doc, id, newJobCtx())

proc refreshOrFailIn(doc: var Document; id: LayerId; ctx: JobCtx) =
  if not refreshIn(doc, id, ctx):
    raise other("the smart object's contents are unavailable (missing linked file?), so it can't be re-rendered")

proc refreshOrFail(doc: var Document; id: LayerId) = refreshOrFailIn(doc, id, newJobCtx())

proc smartRef(doc: Document; id: LayerId): ptr SmartObject =
  let l = doc.findLayer(id)
  if l.isNil: raise noLayer(id)
  if l[].content.kind != lkSmart: raise other("the layer is not a smart object")
  addr l[].content.smart

proc smartPtr(doc: var Document; id: LayerId): ptr SmartObject =
  let l = doc.layerPtr(id)
  if l.isNil: raise noLayer(id)
  if l[].content.kind != lkSmart: raise other("the layer is not a smart object")
  addr l[].content.smart

proc maskFromSelection(sel: Surface): LayerMask =
  LayerMask(surface: sel, enabled: true, linked: true, density: 1, feather: 0)

proc addSmartFilter*(doc: var Document; id: LayerId; sf: SmartFilter; sel: Option[Surface]) =
  ## Appends a smart filter (from a `filter.*` command) and re-renders. A selection becomes the
  ## filter mask, as in Photoshop. Without a renderable source the filter is applied to the cached
  ## pixels (and still recorded).
  let canvas = doc.bounds
  let renderable = sourceBytes(doc.metadata, smartRef(doc, id)[].source).isSome
  let sm = smartPtr(doc, id)
  if renderable:
    if sel.isSome and sm[].filterMask.isNone: sm[].filterMask = some(maskFromSelection(sel.get))
    sm[].smartFilters.add sf
    refreshOrFail(doc, id)
    return
  var bounds = canvas
  if sel.isSome and not sel.get.contentBounds.isEmpty: bounds = sel.get.contentBounds
  if sm[].cache.isNone: raise other("smart object has no pixels")
  let o = applyFilterToSurface(sf.command, sf.params, sm[].cache.get, bounds, sel, canvas)
  if o.isNone: raise other("unknown filter " & sf.command)
  sm[].cache = o
  sm[].smartFilters.add sf

proc snapAffine*(a: Affine): Affine =
  ## Rounds float noise off a composed transform (scale 0.1 then 10 should be exactly 1).
  for i, v in a.m: result.m[i] = (if abs(v - round(v)) < 1e-9: round(v) else: v)

proc affineHomography*(a: Affine): Homography =
  let m = a.m
  homography([m[0], m[2], m[4], m[1], m[3], m[5], 0.0, 0.0, 1.0])

proc smartPlacement*(sm: SmartObject): Homography =
  ## Where the source pixels land in the document: the projective map or the affine transform.
  if sm.perspective.isSome: homography(sm.perspective.get) else: affineHomography(sm.transform)

proc setPlacement*(sm: var SmartObject; h: Homography) =
  ## An affine map is stored exactly in `transform` (no `perspective`); a projective one in
  ## `perspective`, with `transform` its affine approximation at the source origin.
  let m = h.m
  var n = m
  if abs(m[8]) > 1e-12:
    for i in 0 ..< 9: n[i] = m[i] / m[8]
  if abs(n[6]) < 1e-12 and abs(n[7]) < 1e-12:
    sm.transform = snapAffine(Affine(m: [n[0], n[3], n[1], n[4], n[2], n[5]]))
    sm.perspective = none(array[9, float64])
    return
  let hh = homography(n)
  let o = hh.apply(0.0, 0.0)
  let x = hh.apply(1.0, 0.0)
  let y = hh.apply(0.0, 1.0)
  sm.transform = Affine(m: [x[0] - o[0], x[1] - o[1], y[0] - o[0], y[1] - o[1], o[0], o[1]])
  sm.perspective = some(n)

proc transformPlacement*(sm: var SmartObject; a: Affine) =
  ## Composes `a` (document → document) onto the smart object's placement.
  if sm.perspective.isNone: sm.transform = snapAffine(a.mul(sm.transform))
  else: setPlacement(sm, affineHomography(a).mul(smartPlacement(sm)))

proc shiftSmart*(sm: var SmartObject; dx, dy: int) =
  ## Moves a smart object by whole pixels without re-rendering.
  transformPlacement(sm, translateAffine(float64(dx), float64(dy)))
  if sm.cache.isSome: sm.cache = some(shiftSurface(sm.cache.get, dx, dy))
  if sm.filterMask.isSome: sm.filterMask.get.surface = shiftSurface(sm.filterMask.get.surface, dx, dy)

proc detachPsd(l: var Layer) =
  ## Forget PSD placed-layer data that no longer describes the smart object (its source changed).
  if l.content.kind == lkSmart: l.content.smart.psdRaw = nil
  l.psdBlocks.keepItIf(it.key notin PlacedKeys)

# ---------------------------------------------------------------- conversion

proc shiftLayer(l: var Layer; dx, dy: int) =
  ## Moves a layer (any kind) by whole pixels without re-rendering anything.
  if dx == 0 and dy == 0: return
  let a = translateAffine(float64(dx), float64(dy))
  if l.mask.isSome: l.mask.get.surface = translateSurface(l.mask.get.surface, dx, dy)
  if l.vectorMask.isSome: l.vectorMask.get.path = l.vectorMask.get.path.transform(a)
  if l.fillCache.isSome: l.fillCache.get.surface = translateSurface(l.fillCache.get.surface, dx, dy)
  case l.content.kind
  of lkRaster: l.content.raster = translateSurface(l.content.raster, dx, dy)
  of lkText:
    l.content.text.transform = a.mul(l.content.text.transform)
    if l.content.text.cache.isSome: l.content.text.cache = some(translateSurface(l.content.text.cache.get, dx, dy))
  of lkShape:
    transformShape(l.content.shape, a)
    if l.content.shape.cache.isSome: l.content.shape.cache = some(translateSurface(l.content.shape.cache.get, dx, dy))
  of lkSmart: shiftSmart(l.content.smart, dx, dy)
  of lkGroup:
    for c in l.content.group.children.mitems: shiftLayer(c, dx, dy)
  else: discard

proc subtreeBounds(l: Layer; canvas: Rect): Rect =
  case l.content.kind
  of lkGroup:
    result = EmptyRect
    for c in l.content.group.children: result = result.union(subtreeBounds(c, canvas))
  of lkAdjustment, lkFill: result = canvas
  else: result = (if l.hasSurface: l.surface.contentBounds else: EmptyRect)

proc anyLayer(l: Layer; f: proc(l: Layer): bool): bool =
  if f(l): return true
  if l.content.kind == lkGroup:
    for c in l.content.group.children:
      if anyLayer(c, f): return true

proc layersToSmart*(doc: Document; name: string; layers: seq[Layer]): Layer =
  ## A smart-object layer named `name` whose nested document holds `layers` (bottom to top) at its
  ## top level, as Photoshop does when several layers are converted: no wrapper group. The nested
  ## document is cropped to the rendered bounds and the cache is the exact composite.
  let canvas = doc.bounds
  var sub = newDocument(name & ".pcraft", doc.size, doc.mode, doc.depth)
  sub.resolutionDpi = doc.resolutionDpi
  sub.iccProfile = doc.iccProfile
  sub.globalLight = doc.globalLight
  # Pattern fills and pattern layer effects reference document-level patterns by ID.
  sub.patterns = doc.patterns
  if layers.anyIt(anyLayer(it, proc(x: Layer): bool = x.content.kind == lkSmart)):
    # Nested PSD placed layers find their embedded files here.
    sub.metadata.psdGlobalBlocks = doc.metadata.psdGlobalBlocks
  let hasFx = layers.anyIt(anyLayer(it, proc(x: Layer): bool = x.effects.enabled and x.effects.items.len > 0))
  var region = canvas
  for l in layers: region = region.union(subtreeBounds(l, canvas))
  region = region.inflate(if hasFx: 256 else: 0)
  sub.layers = layers
  let buf = compose.render(sub, region)
  let w = region.width
  var (bx0, by0, bx1, by1) = (high(int), high(int), low(int), low(int))
  if w > 0:
    for i, p in buf.px:
      if p[3] > 0:
        let x = region.x0 + i mod w
        let y = region.y0 + i div w
        bx0 = min(bx0, x)
        by0 = min(by0, y)
        bx1 = max(bx1, x + 1)
        by1 = max(by1, y + 1)
  if bx1 <= bx0 or by1 <= by0: raise other("the layer is empty, so there is nothing to convert")
  let b = rect(bx0, by0, bx1, by1)
  let fmt = doc.pixelFormat
  var cache = bufferToSurface(buf, fmt)
  cache = cropSurface(cache, b)
  cache.prune()
  for l in sub.layers.mitems: shiftLayer(l, -b.x0, -b.y0)
  sub.size = Size(width: b.width, height: b.height)
  let bytes = encodeSource(sub)
  # Seed the decode cache with the composite we already have.
  cachePut(cacheKey(bytes, fmt), SourceImage(surface: translateSurface(cache, -b.x0, -b.y0), bounds: sub.bounds))
  let source = SmartSource(kind: ssEmbedded, fileName: sub.name, bytes: SharedBytes(data: bytes))
  newLayer(name, smartContent(newSmartObject(source, translateAffine(float64(b.x0), float64(b.y0)), some(cache))))

proc layerToSmart*(doc: Document; l: Layer): Layer =
  ## The smart-object layer replacing `l`: the layer (with its mask, effects and fill opacity)
  ## becomes a nested document cropped to its rendered bounds; visibility, opacity, blend mode,
  ## clipping and label stay on the smart layer.
  let background = l.name == "Background" and l.locks.position
  var inner = l
  inner.visible = true
  inner.opacity = 1
  inner.clipped = false
  inner.locks = Locks()
  if not (inner.isGroup and inner.blend == bmPassThrough): inner.blend = bmNormal
  if background: inner.name = "Layer 0"
  result = layersToSmart(doc, l.name, @[inner])
  if background: result.name = "Layer 0"
  result.visible = l.visible
  result.opacity = l.opacity
  result.blend = (if l.blend == bmPassThrough: bmNormal else: l.blend)
  result.clipped = l.clipped
  result.label = l.label
  if not background: result.locks = l.locks

proc convertSmart(s: var Session; p: JsonNode): JsonNode =
  let explicit = p.pget("layer").asU64
  let ids = (if explicit.isSome: @[LayerId(explicit.get)] else: selected(s))
  let newId = s.editWith("Convert to Smart Object", proc(doc: var Document; active: var Option[LayerId]): LayerId =
    let roots = topLevel(doc, ids)
    if roots.len == 0: raise other("no layer selected")
    let top = roots[^1]
    let lo = doc.layer(top)
    if lo.isNone: raise noLayer(top)
    var so: Layer
    if roots.len == 1: so = layerToSmart(doc, lo.get)
    else:
      var children: seq[Layer]
      for id in roots:
        let c = doc.layer(id)
        if c.isNone: raise noLayer(id)
        children.add c.get
      so = layersToSmart(doc, lo.get.name, children)
    let nid = so.id
    # As with Group Layers, place the result in the top selected root's parent. Insert before
    # removal so paths shifting in other parents cannot discard unselected content.
    discard doc.insertAbove(some(top), so)
    for id in roots:
      if doc.remove(id).isNone: raise noLayer(id)
    active = some(nid)
    nid)
  reselect(s, @[newId], some(newId))
  %*{"layer": uint64(newId)}

# ---------------------------------------------------------------- contents

proc viaCopy(s: var Session; p: JsonNode): JsonNode =
  let id = layerParam(s, p)
  s.editWith("New Smart Object via Copy", proc(doc: var Document; active: var Option[LayerId]): JsonNode =
    let src = doc.layer(id)
    if src.isNone: raise noLayer(id)
    var copy = src.get.duplicate
    copy.name = doc.copyName(src.get.name)
    if copy.content.kind != lkSmart: raise other("the layer is not a smart object")
    # Independent contents: resolve to embedded bytes and drop the shared PSD uuid.
    copy.content.smart.contentsId = freshContentsId()
    let sb = sourceBytes(doc.metadata, copy.content.smart.source)
    if sb.isSome: copy.content.smart.source = SmartSource(kind: ssEmbedded, fileName: sb.get[0], bytes: sb.get[1])
    detachPsd(copy)
    let nid = doc.insertAbove(some(id), copy)
    active = some(nid)
    %*{"layer": uint64(nid)})

proc pathParam(cmdId: string; p: JsonNode): string =
  let v = p.pget("path").asStr
  if v.isNone or v.get.len == 0: raise bad(cmdId, "pass `path`")
  v.get

proc instances(doc: Document; contentsId: SmartContentsId): seq[LayerId] =
  ## Instances sharing contents in this document, including those inside groups.
  for (_, _, l) in doc.walk:
    if l.content.kind == lkSmart and l.content.smart.contentsId == contentsId: result.add l.id

proc setSharedSource(s: var Session; id: LayerId; label: string; keepPsd: bool; src: SmartSource): JsonNode =
  ## Swaps shared contents, keeping each instance's transform and filters, and re-renders.
  let (parent, contentsId) = s.editWith(label, proc(doc: var Document; active: var Option[LayerId]): (DocId, SmartContentsId) =
    let cid = smartRef(doc, id)[].contentsId
    for inst in instances(doc, cid):
      smartPtr(doc, inst)[].source = src
      if not keepPsd:
        let l = doc.layerPtr(inst)
        if l.isNil: raise noLayer(inst)
        detachPsd(l[])
      refreshOrFail(doc, inst)
    (doc.id, cid))
  # An old editor must not overwrite newly replaced/relinked contents on close. Keep its document
  # open, but the next Edit Contents opens the replacement source.
  s.smartLinks.keepItIf(it.parent != parent or it.contentsId != contentsId)
  %*{"layer": uint64(id)}

proc setSource(s: var Session; p: JsonNode; label: string; keepPsd: bool;
               make: proc(meta: Metadata; src: SmartSource): SmartSource): JsonNode =
  let id = layerParam(s, p)
  let st = s.active
  if st.isNil: raise noDocument()
  let src = make(st.doc[].metadata, smartRef(st.doc[], id)[].source)
  setSharedSource(s, id, label, keepPsd, src)

proc replaceContentsBytes*(s: var Session; id: LayerId; fileName: string; bytes: seq[byte]): JsonNode =
  ## Replace Contents from a file's name and bytes: what the desktop and web file pickers hand over.
  discard decodeSource(fileName, bytes) # fail before touching the document
  setSharedSource(s, id, "Replace Contents", false,
                  SmartSource(kind: ssEmbedded, fileName: fileName, bytes: SharedBytes(data: bytes)))

proc replaceContents(s: var Session; p: JsonNode): JsonNode =
  let path = pathParam("layer.smartObjects.replaceContents", p)
  var bytes: seq[byte]
  try: bytes = readBytesRust(path)
  except OSError as e: raise other("can't read " & path & ": " & e.msg)
  let id = layerParam(s, p)
  replaceContentsBytes(s, id, baseName(path), bytes)

proc contentsOf*(doc: Document; id: LayerId): (string, SharedBytes) =
  ## The file name and bytes of smart object `id`'s contents.
  let sb = sourceBytes(doc.metadata, smartRef(doc, id)[].source)
  if sb.isNone: raise other("the smart object's contents are unavailable")
  sb.get

proc exportContents(s: var Session; p: JsonNode): JsonNode =
  let path = pathParam("layer.smartObjects.exportContents", p)
  let id = layerParam(s, p)
  let st = s.active
  if st.isNil: raise noDocument()
  let (name, bytes) = contentsOf(st.doc[], id)
  writeFileAtomic(path, bytes.data)
  %*{"path": path, "fileName": name, "bytes": bytes.data.len}

proc editContents(s: var Session; p: JsonNode): JsonNode =
  let id = layerParam(s, p)
  let st = s.active
  if st.isNil: raise noDocument()
  let parent = st.doc.id
  let contentsId = smartRef(st.doc[], id)[].contentsId
  # Already open for editing: switch to that document rather than opening another copy.
  for link in s.smartLinks:
    if link.parent == parent and link.contentsId == contentsId:
      for index, d in s.docs:
        if d.doc.id == link.child:
          discard s.setActive(index)
          return %*{"document": index, "parentLayer": uint64(id)}
      break
  let source = smartRef(st.doc[], id)[].source
  let sb = authorizedSourceBytes(s, "layer.smartObjects.editContents", st.doc.metadata, source)
  if sb.isNone: raise other("the smart object's contents are unavailable")
  let (name, bytes) = sb.get
  var child = decodeSource(name, bytes.data)
  # Bundles keep their document id; each open copy needs its own.
  child.id = freshDocId()
  child.name = name
  let index = s.addDocument(child)
  # Admission may replace an ID already owned by another open document.
  let childId = s.docs[index].doc.id
  s.smartLinks.keepItIf(it.child != childId)
  s.smartLinks.add SmartLink(child: childId, parent: parent, layer: id, contentsId: contentsId)
  %*{"document": index, "parentLayer": uint64(id)}

proc commitChild*(s: var Session; index: int): bool =
  ## Writes an Edit Contents document back into all its parent instances (one undoable step in
  ## the parent) and marks it saved. False if `index` isn't an Edit Contents document.
  if index < 0 or index >= s.docs.len: return false
  let child = s.docs[index].doc[]
  var link: SmartLink
  var linked = false
  for l in s.smartLinks:
    if l.child == child.id:
      link = l
      linked = true
      break
  if not linked: return false
  var parent = -1
  for i, d in s.docs:
    if d.doc.id == link.parent:
      parent = i
      break
  if parent < 0: raise other("the smart object's document was closed")
  let bytes = SharedBytes(data: encodeSource(child))
  let prev = s.activeIdx
  s.activeIdx = some(parent)
  let cid = link.contentsId
  let childName = child.name
  try:
    s.edit("Edit Contents", proc(doc: var Document; active: var Option[LayerId]) =
      let ids = instances(doc, cid)
      if ids.len == 0: raise other("the smart object's instances were removed")
      for id in ids:
        let sm = smartPtr(doc, id)
        let name = (if sm[].source.kind == ssEmbedded: sm[].source.fileName else: childName)
        let dot = name.rfind('.')
        let stem = (if dot >= 0: name[0 ..< dot] else: name)
        sm[].source = SmartSource(kind: ssEmbedded, fileName: stem & ".pcraft", bytes: bytes)
        let l = doc.layerPtr(id)
        if l.isNil: raise noLayer(id)
        detachPsd(l[])
        refreshOrFail(doc, id))
  finally:
    s.activeIdx = prev
  if index < s.docs.len: s.docs[index].savedRevision = s.docs[index].revision
  true

proc onCloseDocument(s: var Session; index: int) =
  ## Called before a document closes: an edited Edit Contents document updates its parent.
  if index < 0 or index >= s.docs.len: return
  let id = s.docs[index].doc.id
  if s.docs[index].isDirty and s.smartLinks.anyIt(it.child == id):
    try: discard commitChild(s, index)
    except CatchableError: discard
  s.smartLinks.keepItIf(it.child != id and it.parent != id)

proc soIsChildE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if s.smartLinks.anyIt(it.child == d.doc.id): "" else: "this document isn't smart object contents"

proc updateAll(s: var Session): JsonNode =
  let st = s.active
  if st.isNil: raise noDocument()
  var ids: seq[LayerId]
  for (_, _, l) in st.doc[].walk:
    if l.content.kind == lkSmart and l.content.smart.source.kind == ssLinked: ids.add l.id
  var updated: seq[uint64]
  s.edit("Update All Modified Content", proc(doc: var Document; active: var Option[LayerId]) =
    for id in ids:
      if smartRefresh(doc, id): updated.add uint64(id))
  %*{"updated": updated}

# ---------------------------------------------------------------- smart filter editing

proc editFilters(s: var Session; p: JsonNode; label: string; f: proc(sm: var SmartObject)): JsonNode =
  ## Edits the smart filters of a smart object and re-renders it. The edit itself is instant, so
  ## its errors (a bad index, say) are reported at once; the re-render is the job.
  let id = layerParam(s, p)
  let st = s.active
  if st.isNil: raise noDocument()
  var edited = smartRef(st.doc[], id)[]
  f(edited)
  let e = edited
  editJob(s, label,
    proc(doc: var Document; active: var Option[LayerId]; ctx: JobCtx): bool =
      smartPtr(doc, id)[] = e
      refreshOrFailIn(doc, id, ctx)
      true,
    proc(r: bool): JsonNode = %*{"layer": uint64(id)})

proc filterIndex(cmdId: string; p: JsonNode; sm: SmartObject): int =
  let n = sm.smartFilters.len
  if n == 0: raise other("the smart object has no smart filters")
  let v = p.pget("index")
  if v.isNil: return n - 1
  let i = v.asU64
  if i.isNone or i.get >= uint64(n): raise bad(cmdId, "`index` must be 0.." & $n & " (0 = bottom filter)")
  int(i.get)

proc setFilterVisible(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "layer.smartFilter.setVisible"
  editFilters(s, p, "Smart Filter Visibility", proc(sm: var SmartObject) =
    let i = filterIndex(cmdId, p, sm)
    sm.smartFilters[i].visible = p.pget("visible").asBool.get(not sm.smartFilters[i].visible))

proc setFilterParams(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "layer.smartFilter.setParams"
  let newParams = p.pget("params")
  if newParams.isNil: raise bad(cmdId, "pass `params` (merged into the filter's parameters)")
  editFilters(s, p, "Edit Smart Filter", proc(sm: var SmartObject) =
    let i = filterIndex(cmdId, p, sm)
    if sm.smartFilters[i].command == UnsupportedFilter:
      raise bad(cmdId, "this smart filter isn't implemented in PhotoCraft: it is kept as is (it can be hidden, moved or deleted)")
    let old = sm.smartFilters[i].params
    if not old.isNil and old.kind == JObject and newParams.kind == JObject:
      let merged = old.copy
      for k, v in newParams.fields: merged[k] = v.copy
      sm.smartFilters[i].params = merged
    else: sm.smartFilters[i].params = newParams.copy)

proc blendingOptions(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "layer.smartFilter.blendingOptions"
  var blend = none(BlendMode)
  let b = p.pget("blend").asStr
  if b.isSome:
    let m = blendFromStr(b.get)
    if m.isNone or m.get == bmPassThrough: raise bad(cmdId, "unknown blend mode `" & b.get & "`")
    blend = m
  var opacity = none(float32)
  let o = p.pget("opacity").asF64
  if o.isSome: opacity = some(float32(if o.get > 1.0: o.get / 100.0 else: o.get))
  editFilters(s, p, "Smart Filter Blending Options", proc(sm: var SmartObject) =
    let i = filterIndex(cmdId, p, sm)
    if blend.isSome: sm.smartFilters[i].blend = blend.get
    if opacity.isSome: sm.smartFilters[i].opacity = clamp(opacity.get, 0'f32, 1'f32))

proc deleteFilter(s: var Session; p: JsonNode): JsonNode =
  editFilters(s, p, "Delete Smart Filter", proc(sm: var SmartObject) =
    let i = filterIndex("layer.smartFilter.delete", p, sm)
    sm.smartFilters.delete(i))

proc moveFilter(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "layer.smartFilter.move"
  editFilters(s, p, "Move Smart Filter", proc(sm: var SmartObject) =
    let i = filterIndex(cmdId, p, sm)
    let n = sm.smartFilters.len
    let to = p.pget("to").asU64
    if to.isNone or to.get >= uint64(n): raise bad(cmdId, "`to` must be 0.." & $n)
    let f = sm.smartFilters[i]
    sm.smartFilters.delete(i)
    sm.smartFilters.insert(f, int(to.get)))

# ---------------------------------------------------------------- Convert to Layers (unpack.rs)

proc embedNested(meta: Metadata; l: var Layer) =
  ## Nested PSD placed layers find their files in the contents' own global blocks, which stay
  ## behind: embed those sources so the nested smart objects keep rendering in the document.
  if l.content.kind == lkSmart and l.content.smart.source.kind == ssLinked:
    let f = findLinkedFile(meta, l.content.smart.source.path)
    if f.isSome:
      l.content.smart.source = SmartSource(kind: ssEmbedded, fileName: f.get.fileName, bytes: SharedBytes(data: f.get.bytes))
      detachPsd(l)
  if l.content.kind == lkGroup:
    for c in l.content.group.children.mitems: embedNested(meta, c)

proc unpackContents(s: Session; doc: Document; sm: SmartObject; intent: Intent; bpc: bool): seq[Layer] =
  ## The smart object's contents as layers in `doc`'s colour model, profile and depth, still in
  ## source pixel coordinates. A Background becomes a normal layer.
  let sb = authorizedSourceBytes(s, "layer.smartObjects.convertToLayers", doc.metadata, sm.source)
  if sb.isNone: raise other("the smart object's contents are unavailable (missing linked file?)")
  let (name, bytes) = sb.get
  var src = decodeSource(name, bytes.data)
  let mode = doc.pixelFormat.mode
  if src.pixelFormat.mode != mode or src.iccProfile != doc.iccProfile:
    convertDocument(src, documentProfile(doc), intent, bpc)
    if src.pixelFormat.mode != mode:
      raise other("can't convert the smart object's contents to the document's " & $doc.mode & " mode")
  if src.depth != doc.depth: convertLayersDepth(src.layers, doc.depth)
  result = move src.layers
  for l in result.mitems:
    if isBackground(l):
      l.name = "Layer 0"
      l.locks = Locks()
    embedNested(src.metadata, l)

proc released(l: var Layer; f: proc(l: var Layer)) =
  ## Runs `f` with every lock and mask link in `l`'s tree released, so a transform moves all of
  ## it, then restores them.
  var saved: seq[(Locks, Option[bool], Option[bool])]
  var one = @[move l]
  forEachLayer(one, proc(x: var Layer) =
    saved.add (x.locks, (if x.mask.isSome: some(x.mask.get.linked) else: none(bool)),
               (if x.vectorMask.isSome: some(x.vectorMask.get.linked) else: none(bool)))
    x.locks = Locks()
    if x.mask.isSome: x.mask.get.linked = true
    if x.vectorMask.isSome: x.vectorMask.get.linked = true)
  try: f(one[0])
  finally:
    # A transform keeps the tree's shape, so the walk meets the layers in the same order.
    var k = 0
    forEachLayer(one, proc(x: var Layer) =
      if k >= saved.len: return
      let (locks, mask, vector) = saved[k]
      inc k
      x.locks = locks
      if x.mask.isSome and mask.isSome: x.mask.get.linked = mask.get
      if x.vectorMask.isSome and vector.isSome: x.vectorMask.get.linked = vector.get)
    l = move one[0]

proc unpackPlace(s: Session; doc: Document; sm: SmartObject; layers: var seq[Layer]) =
  ## Moves unpacked layers from source pixels to where the smart object placed them: an exact
  ## shift for whole-pixel moves, else its transform.
  if sm.warp.isSome and not sm.warp.get.isIdentity:
    raise other("a warped smart object can't be converted to layers; rasterize it instead (Layer › Smart Objects › Rasterize)")
  let m = sm.transform.m
  proc whole(v: float64): bool = abs(v - round(v)) < 1e-9
  if sm.perspective.isNone and abs(m[0] - 1.0) < 1e-9 and abs(m[1]) < 1e-9 and abs(m[2]) < 1e-9 and
      abs(m[3] - 1.0) < 1e-9 and whole(m[4]) and whole(m[5]):
    # Saturating casts: a placement that far out moves the layers off any canvas either way.
    for l in layers.mitems: shiftLayer(l, asI32(round(m[4])), asI32(round(m[5])))
    return
  # Unlike a whole-pixel shift, this path re-renders nested smart objects from their sources.
  # Check every read before transforming anything.
  if not s.authorize.isNil:
    var pending: seq[Layer] = layers
    while pending.len > 0:
      let l = pending.pop()
      if l.content.kind == lkSmart:
        authorizeSource(s, "layer.smartObjects.convertToLayers", doc, l.content.smart.source)
      if l.content.kind == lkGroup:
        for c in l.content.group.children: pending.add c
  let h = smartPlacement(sm)
  let affine = (if sm.perspective.isNone: some(sm.transform) else: none(Affine))
  for l in layers.mitems:
    released(l, proc(x: var Layer) = transformLayer(none(Surface), Locks(), x, h, affine, ipBicubic))
    refreshRendered(doc, l)

proc foldable(so, c: Layer): bool =
  ## Whether the smart layer's own properties fold into its one unpacked layer `c` without
  ## changing the look.
  so.mask.isNone and so.vectorMask.isNone and (not so.effects.enabled or so.effects.items.len == 0) and
    so.fillOpacity >= 1 and so.blendIf.isDefault and so.excludedChannels == 0 and not c.clipped and
    c.blend in {bmNormal, bmPassThrough} and c.blendIf.isDefault and c.excludedChannels == 0

proc unpacked(so: Layer; layers: seq[Layer]): Layer =
  ## What replaces smart layer `so`: its one unpacked layer with `so`'s properties folded in, or a
  ## group named after it that carries them (isolated like the smart object, never pass-through).
  let blend = (if so.blend == bmPassThrough: bmNormal else: so.blend)
  if layers.len == 1 and foldable(so, layers[0]):
    result = layers[0]
    result.name = so.name
    result.visible = result.visible and so.visible
    result.opacity = result.opacity * so.opacity
    result.blend = blend
    result.clipped = so.clipped
    result.locks = so.locks
    result.label = so.label
    result.linkGroup = so.linkGroup
    return
  result = so
  result.id = freshLayerId()
  result.psdId = none(uint32)
  result.blend = blend
  result.content = groupContent(layers)
  detachPsd(result)

proc convertToLayers(s: var Session; p: JsonNode): JsonNode =
  let id = layerParam(s, p)
  let intent = s.color.settings.settingsIntent
  let bpc = s.color.settings.bpc
  let st = s.active
  if st.isNil: raise noDocument()
  let docRef = st.doc
  let soPtr = docRef[].findLayer(id)
  if soPtr.isNil: raise noLayer(id)
  let sm = smartRef(docRef[], id)[]
  var layers = unpackContents(s, docRef[], sm, intent, bpc)
  unpackPlace(s, docRef[], sm, layers)
  let discarded = sm.smartFilters.len
  let o = unpacked(soPtr[], layers)
  let newId = o.id
  let group = o.isGroup
  s.edit("Convert to Layers", proc(doc: var Document; active: var Option[LayerId]) =
    let path = doc.pathOf(id)
    if path.isNone: raise noLayer(id)
    let lp = doc.layerAtPtr(path.get)
    if lp.isNil: raise noLayer(id)
    lp[] = o
    active = some(newId))
  %*{"layer": uint64(newId), "group": group, "discardedSmartFilters": discarded}

# ---------------------------------------------------------------- linked files (link_watch.rs)

proc stamp(path: string): Option[Stamp] =
  ## A file's size and modification time.
  when defined(js) or defined(wasm32): none(Stamp)
  else:
    var st: Stat
    if stat(path.cstring, st) != 0: return
    if int64(st.st_mtim.tv_sec) < 0: return
    some((uint64(st.st_size), int64(st.st_mtim.tv_sec), int64(st.st_mtim.tv_nsec)))

proc linkedFiles*(d: Document): seq[(string, seq[uint64])] =
  ## The files of `d`'s linked smart objects, with the layers showing each (sorted by path).
  var byFile = initTable[string, seq[uint64]]()
  for (_, _, l) in d.walk:
    if l.content.kind == lkSmart and l.content.smart.source.kind == ssLinked:
      byFile.mgetOrPut(l.content.smart.source.path, @[]).add uint64(l.id)
  var keys = toSeq(byFile.keys)
  keys.sort(cmp[string])
  for k in keys: result.add (k, byFile[k])

proc applyLinkStamps(s: var Session; doc: uint64; now: seq[(string, seq[uint64], Option[Stamp])]): JsonNode =
  ## Compares the stamps of document `doc`'s linked files with the ones last seen and updates the
  ## layers of the changed ones. Returns `{updated: [layer ids]}`.
  let st = s.active
  # The document went to the back meanwhile: it's looked at when it's in front again.
  if st.isNil or uint64(st.doc.id) != doc: return %*{"updated": []}
  # This document's last stamps; those of closed documents are dropped.
  var open = initHashSet[uint64]()
  for d in s.docs: open.incl uint64(d.doc.id)
  var before = initTable[string, Stamp]()
  var kept = initTable[(uint64, string), Stamp]()
  for k, v in s.linkStamps:
    if k[0] == doc: before[k[1]] = v
    elif k[0] in open: kept[k] = v
  s.linkStamps = kept
  var changed: seq[(string, seq[uint64])]
  for (path, ids, now) in now:
    let old = before.getOrDefault(path, (0'u64, -1'i64, 0'i64))
    let hasOld = path in before
    if now.isNone:
      # Not there right now (an app saving by delete and rename): keep the last stamp.
      if hasOld: s.linkStamps[(doc, path)] = old
    else:
      if hasOld and old != now.get: changed.add (path, ids)
      s.linkStamps[(doc, path)] = now.get
  var updated: seq[uint64]
  for (path, layerIds) in changed:
    let ids = layerIds
    let file = path
    try:
      s.edit("Update Modified Content", proc(d: var Document; active: var Option[LayerId]) =
        for id in ids:
          if not smartRefresh(d, LayerId(id)): raise other(file & " can't be read"))
      updated.add ids
    except CatchableError:
      # Not readable (still being written, or not a format smart objects read): the edit left
      # nothing behind, and the file is tried again once it changes again.
      discard
  %*{"updated": updated}

proc updateChanged(s: var Session; p: JsonNode): JsonNode =
  ## `layer.smartObjects.updateChanged`: stamp the active document's linked files now and update
  ## the layers of the ones that changed.
  let st = s.active
  if st.isNil: raise noDocument()
  let doc = uint64(st.doc.id)
  var now: seq[(string, seq[uint64], Option[Stamp])]
  for (path, ids) in linkedFiles(st.doc[]): now.add (path, ids, stamp(path))
  applyLinkStamps(s, doc, now)

proc startLinkScan*(s: var Session) =
  ## Stamps the active document's linked files (the Rust build does this on a worker thread).
  if s.linkScan.isSome: return
  let st = s.active
  if st.isNil: return
  let files = linkedFiles(st.doc[])
  if files.len == 0: return
  var stamps: seq[(string, seq[uint64], Option[Stamp])]
  for (path, ids) in files: stamps.add (path, ids, stamp(path))
  s.linkScan = some((uint64(st.doc.id), stamps))

proc pollLinkScan*(s: var Session): Option[JsonNode] =
  ## Applies a finished look (`none` while there is none).
  if s.linkScan.isNone: return
  let (doc, stamps) = s.linkScan.get
  s.linkScan = none(LinkScan)
  some(applyLinkStamps(s, doc, stamps))

# ---------------------------------------------------------------- enablement

proc soActiveSmart(s: Session): (ptr SmartObject, string) =
  let d = s.active
  if d.isNil: return (nil, "no document open")
  if d.activeLayer.isNone: return (nil, "no active layer")
  let l = d.doc[].findLayer(d.activeLayer.get)
  if l.isNil: return (nil, "no active layer")
  if l[].content.kind != lkSmart:
    return (nil, "the active layer is " & l[].content.article & " " & l[].content.kindName & " layer, not a smart object")
  (addr l[].content.smart, "")

proc soHasSmartE(s: Session): string = soActiveSmart(s)[1]
proc soHasLinkedE(s: Session): string =
  let (sm, why) = soActiveSmart(s)
  if sm.isNil: return why
  if sm[].source.kind == ssLinked: "" else: "the smart object is embedded"
proc soHasFiltersE(s: Session): string =
  let (sm, why) = soActiveSmart(s)
  if sm.isNil: return why
  if sm[].smartFilters.len == 0: "the smart object has no smart filters" else: ""
proc soHasFilterMaskE(s: Session): string =
  let (sm, why) = soActiveSmart(s)
  if sm.isNil: return why
  if sm[].filterMask.isNone: "the smart object has no filter mask" else: ""
proc soConvertibleE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if d.activeLayer.isNone: return "no active layer"
  if d.doc[].findLayer(d.activeLayer.get).isNil: "no active layer" else: ""
proc soNotSmartE(s: Session): string =
  let why = soConvertibleE(s)
  if why.len > 0: return why
  if soActiveSmart(s)[1].len == 0: "the layer is already a smart object" else: ""
proc soHasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")

proc smartSpecs*(): seq[CommandSpec] =
  const so = ["Layer", "Smart Objects"]
  const sf = ["Layer", "Smart Filter"]
  result = @[
    cmd("layer.smartObjects.convertToSmartObject", "Convert to Smart Object", so, "",
        "{\"layer\":id?} (default: combine selected layers)", soConvertibleE, runProc(convertSmart(s, p))),
    cmd("filter.convertForSmartFilters", "Convert for Smart Filters", ["Filter"], "",
        "{\"layer\":id?} (default: combine selected layers)", soNotSmartE, runProc(convertSmart(s, p))),
    cmd("layer.smartObjects.newSmartObjectViaCopy", "New Smart Object via Copy", so, "", "{\"layer\":id?}", soHasSmartE,
        runProc(viaCopy(s, p))),
    cmd("layer.smartObjects.rasterize", "Rasterize", so, "", "{\"layer\":id?}", soHasSmartE,
        runProc(s.execute("layer.rasterize.smartObject", if p.isNil: newJNull() else: p.copy))),
    cmd("layer.smartObjects.editContents", "Edit Contents", so, "",
        "{\"layer\":id?} → opens the contents as a new document; saving (layer.smartObjects.saveContents) or closing it updates the smart object",
        soHasSmartE, runProc(editContents(s, p))),
    cmd("layer.smartObjects.saveContents", "Save Contents", [], "", "{} (in an Edit Contents document)", soIsChildE,
        runProc((
          let i = s.activeIndex
          if i.isNone: raise noDocument()
          %*{"updated": commitChild(s, i.get)}))),
    cmd("layer.smartObjects.replaceContents", "Replace Contents…", so, "", "{\"layer\":id?,\"path\":str}", soHasSmartE,
        runProc(replaceContents(s, p))),
    cmd("layer.smartObjects.exportContents", "Export Contents…", so, "", "{\"layer\":id?,\"path\":str}", soHasSmartE,
        runProc(exportContents(s, p))),
    cmd("layer.smartObjects.relinkToFile", "Relink to File…", so, "", "{\"layer\":id?,\"path\":str}", soHasSmartE,
        runProc((
          let path = pathParam("layer.smartObjects.relinkToFile", p)
          setSource(s, p, "Relink to File", false,
                    proc(meta: Metadata; src: SmartSource): SmartSource = SmartSource(kind: ssLinked, path: path))))),
    cmd("layer.smartObjects.updateModifiedContent", "Update Modified Content", so, "", "{\"layer\":id?}", soHasLinkedE,
        runProc((
          let id = layerParam(s, p)
          s.edit("Update Modified Content", proc(doc: var Document; active: var Option[LayerId]) = refreshOrFail(doc, id))
          %*{"layer": uint64(id)}))),
    cmd("layer.smartObjects.updateChanged", "Update Changed Linked Content", [], "",
        "{} — re-render the active document's linked smart objects whose files changed on disk since they were last seen (size or modification time); the desktop app does this by itself every two seconds, as Photoshop does while a document is open. Files seen for the first time are only remembered. One Update Modified Content undo step per file → {updated: [layer ids]}",
        soHasDocE, runProc(updateChanged(s, p))),
    cmd("layer.smartObjects.updateAllModifiedContent", "Update All Modified Content", so, "", "{}", soHasDocE,
        runProc(updateAll(s))),
    cmd("layer.smartObjects.convertToLayers", "Convert to Layers", so, "",
        "{\"layer\":id?} (contents unpacked at the placement: one layer, or a group named after the smart object; smart filters are discarded)",
        soHasSmartE, runProc(convertToLayers(s, p))),
    cmd("layer.smartObjects.convertToEmbedded", "Convert to Embedded", so, "", "{\"layer\":id?}", soHasLinkedE,
        runProc(setSource(s, p, "Convert to Embedded", true, proc(meta: Metadata; src: SmartSource): SmartSource =
          let sb = sourceBytes(meta, src)
          if sb.isNone: raise other("the linked file can't be read")
          SmartSource(kind: ssEmbedded, fileName: sb.get[0], bytes: sb.get[1])))),
    cmd("layer.smartObjects.convertToLinked", "Convert to Linked…", so, "",
        "{\"layer\":id?,\"path\":str} (writes the contents there)", soHasSmartE,
        runProc((
          let path = pathParam("layer.smartObjects.convertToLinked", p)
          setSource(s, p, "Convert to Linked", false, proc(meta: Metadata; src: SmartSource): SmartSource =
            let sb = sourceBytes(meta, src)
            if sb.isNone: raise other("the smart object's contents are unavailable")
            writeFileAtomic(path, sb.get[1].data)
            SmartSource(kind: ssLinked, path: path))))),
    # Smart filters
    cmd("layer.smartFilter.disableSmartFilters", "Disable Smart Filters", sf, "",
        "{\"layer\":id?,\"enabled\":bool? (default: toggle)}", soHasFiltersE,
        runProc(editFilters(s, p, "Disable Smart Filters", proc(sm: var SmartObject) =
          sm.filtersEnabled = p.pget("enabled").asBool.get(not sm.filtersEnabled)))),
    cmd("layer.smartFilter.deleteFilterMask", "Delete Filter Mask", sf, "", "{\"layer\":id?}", soHasFilterMaskE,
        runProc(editFilters(s, p, "Delete Filter Mask", proc(sm: var SmartObject) = sm.filterMask = none(LayerMask)))),
    cmd("layer.smartFilter.disableFilterMask", "Disable Filter Mask", sf, "",
        "{\"layer\":id?,\"enabled\":bool? (default: toggle)}", soHasFilterMaskE,
        runProc(editFilters(s, p, "Disable Filter Mask", proc(sm: var SmartObject) =
          if sm.filterMask.isSome:
            sm.filterMask.get.enabled = p.pget("enabled").asBool.get(not sm.filterMask.get.enabled)))),
    cmd("layer.smartFilter.blendingOptions", "Blending Options…", sf, "",
        "{\"layer\":id?,\"index\":u32? (0 = bottom; default top),\"blend\":str?,\"opacity\":0..1?}", soHasFiltersE,
        runProc(blendingOptions(s, p))),
    cmd("layer.smartFilter.clearSmartFilters", "Clear Smart Filters", sf, "", "{\"layer\":id?}", soHasFiltersE,
        runProc(editFilters(s, p, "Clear Smart Filters", proc(sm: var SmartObject) =
          sm.smartFilters.setLen(0)
          sm.filterMask = none(LayerMask)))),
    cmd("layer.smartFilter.setVisible", "Show/Hide Smart Filter", [], "",
        "{\"layer\":id?,\"index\":u32?,\"visible\":bool? (default: toggle)}", soHasFiltersE,
        runProc(setFilterVisible(s, p))),
    cmd("layer.smartFilter.setParams", "Edit Smart Filter", [], "",
        "{\"layer\":id?,\"index\":u32?,\"params\":{…} (merged)}", soHasFiltersE, runProc(setFilterParams(s, p))),
    cmd("layer.smartFilter.delete", "Delete Smart Filter", [], "", "{\"layer\":id?,\"index\":u32?}", soHasFiltersE,
        runProc(deleteFilter(s, p))),
    cmd("layer.smartFilter.move", "Move Smart Filter", [], "", "{\"layer\":id?,\"index\":u32?,\"to\":u32}",
        soHasFiltersE, runProc(moveFilter(s, p))),
  ]
