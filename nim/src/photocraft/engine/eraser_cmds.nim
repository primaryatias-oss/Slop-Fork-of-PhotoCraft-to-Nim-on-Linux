## The Eraser group's colour-aware tools (port of `eraser_cmds.rs`): Magic Eraser
## (`paint.magicEraser`, a wand-style region erased to transparency) and Background Eraser
## (`paint.backgroundEraser`, a brush that erases colours similar to the one under its hotspot).
##
## Like Photoshop, both turn a Background layer into a normal layer ("Layer 0") first, since the
## Background can't hold transparency. The Magic Eraser paints the background colour on other
## layers with locked transparency; the Background Eraser refuses them. Both respect the
## selection, work in the layer's own colour model and depth, and are one undo step each.

import std/[json, options, math]
import ./core, ./selection_cmds
import ../paint, ../raster
import ../algo/selection as sel
import ../algo/erase

proc eflag(p: JsonNode; k: string; d: bool): bool = p.pget(k).asBool.get(d)

proc number(p: JsonNode; cmdId, k: string; d, lo, hi: float64): float64 =
  ## A finite number param clamped to `lo..hi`, `d` when absent; non-numbers are an error.
  let v = p.pget(k)
  if v.isNull: return d
  let f = v.asF64
  if f.isNone or f.get != f.get or abs(f.get) == Inf: raise bad(cmdId, "`" & k & "` must be a number")
  clamp(f.get, lo, hi)

proc enumParam[T](p: JsonNode; cmdId, k: string; d: T; allowed: string): T =
  let v = p.pget(k)
  if v.isNull: return d
  result = d
  try:
    withSerdeStrict: fromJ(v, result)
  except SerdeError: raise bad(cmdId, "`" & k & "` must be one of " & allowed)

proc hasPixelLayerE(s: Session): string =
  ## The active layer must hold pixels (the erasers don't work on type, shapes, smart objects…).
  let d = s.active
  if d.isNil: return "no document open"
  if d.activeLayer.isNone: return "no active layer"
  let l = d.doc[].findLayer(d.activeLayer.get)
  if l.isNil: return "no active layer"
  if l[].content.kind == lkRaster: ""
  else: "active layer is " & l[].content.article & " " & l[].content.kindName & " layer, not a pixel layer"

proc activePixelLayer(s: Session; cmdId: string): LayerId =
  let why = hasPixelLayerE(s)
  if why.len > 0: raise bad(cmdId, why)
  s.active.activeLayer.get

proc eraserIsBackground(l: Layer): bool =
  l.name == "Background" and l.locks.transparency and l.locks.position and l.content.kind == lkRaster

proc unlockBackground(l: var Layer) =
  ## Photoshop converts the Background to a normal layer before erasing to transparency.
  if eraserIsBackground(l):
    l.name = "Layer 0"
    l.locks.transparency = false
    l.locks.position = false

proc pixelsLocked(doc: Document; l: Layer): ref EngineError =
  let locks = doc.effectiveLocks(l.id)
  if locks.pixels or locks.all:
    other("Could not complete your request because the layer \"" & l.name & "\" is locked")
  else: nil

proc eraseDamage(s: var Session; dmg: Rect; erased: bool): JsonNode =
  let st = s.active
  if not st.isNil and not dmg.isEmpty: st.lastDamage = some(dmg)
  %*{"erased": erased, "damage": [dmg.x0, dmg.y0, dmg.width, dmg.height]}

proc magicEraser(s: var Session; p: JsonNode): JsonNode =
  ## `paint.magicEraser`: erase the wand region around `(x, y)`.
  const cmdId = "paint.magicEraser"
  proc coord(k: string): float64 =
    let v = p.pget(k).asF64
    if v.isNone or v.get != v.get or abs(v.get) == Inf or not (abs(v.get) < 1e9):
      raise bad(cmdId, "missing or invalid `" & k & "`")
    v.get
  let x = asI32(floor(coord("x")))
  let y = asI32(floor(coord("y")))
  let tolerance = float32(number(p, cmdId, "tolerance", 32, 0, 255))
  let opacity = float32(number(p, cmdId, "opacity", 100, 1, 100)) / 100
  let antiAlias = eflag(p, "antiAlias", true)
  let contiguous = eflag(p, "contiguous", true)
  let sampleAll = eflag(p, "sampleAllLayers", false)
  let id = activePixelLayer(s, cmdId)
  block:
    let d = s.active
    if d.isNil: raise noDocument()
    let l = d.doc[].findLayer(id)
    if not l.isNil:
      let e = pixelsLocked(d.doc[], l[])
      if not e.isNil: raise e
    # Clicking off the canvas erases nothing (and records no history step).
    if not d.doc[].bounds.contains(x, y): return eraseDamage(s, EmptyRect, false)
  let (area, img) = sampleRgba8(s, sampleAll)
  let region = sel.wandRegion(img, area, (x, y), tolerance, contiguous, antiAlias)
  if region.isNone: return eraseDamage(s, EmptyRect, false)
  let bg = s.tools.background
  let r = region.get
  let dmg = s.editWith("Magic Eraser", proc(doc: var Document; active: var Option[LayerId]): Rect =
    let selection = doc.selection
    let lp = doc.layerPtr(id)
    if lp.isNil: raise noLayer(id)
    unlockBackground(lp[])
    let lock = doc.effectiveLocks(id).transparency
    let l = doc.layerPtr(id)
    if l.isNil: raise noLayer(id)
    let surf = surfaceMut(l[])
    if surf.isNil: raise bad(cmdId, "not a pixel layer")
    let fmt = surf[].format
    # Locked transparency: paint the background colour instead (Photoshop).
    let lockColor = (if lock: some(fromRgba(fmt, [bg[0], bg[1], bg[2], 1'f32])) else: none(seq[float32]))
    magicErase(surf[], r, opacity, selection, lockColor))
  eraseDamage(s, dmg, true)

proc backgroundEraser(s: var Session; p: JsonNode): JsonNode =
  ## `paint.backgroundEraser`: a stroke erasing colours similar to the sampled one.
  const cmdId = "paint.backgroundEraser"
  let pts = parsePoints(p, cmdId)
  for q in pts:
    if q.x != q.x or q.y != q.y or abs(q.x) == Inf or abs(q.y) == Inf or abs(q.x) > 1e7 or abs(q.y) > 1e7:
      raise bad(cmdId, "`points` must be finite canvas coordinates")
  let brush = resolveBrush(s, p, cmdId)
  var bs = BgEraseSettings(
    sampling: enumParam(p, cmdId, "sampling", smContinuous, "continuous|once|backgroundSwatch"),
    limits: enumParam(p, cmdId, "limits", lmContiguous, "discontiguous|contiguous|findEdges"),
    tolerance: float32(number(p, cmdId, "tolerance", 50, 0, 100)) / 100,
    protectForeground: eflag(p, "protectForegroundColor", false),
    foreground: s.tools.foreground,
    background: s.tools.background)
  let id = activePixelLayer(s, cmdId)
  block:
    let d = s.active
    if d.isNil: raise noDocument()
    let l = d.doc[].findLayer(id)
    if l.isNil: raise noLayer(id)
    let e = pixelsLocked(d.doc[], l[])
    if not e.isNil: raise e
    if d.doc[].effectiveLocks(id).transparency and not eraserIsBackground(l[]):
      raise other("Could not use the Background Eraser because the transparency of layer \"" & l[].name & "\" is locked")
  let stroke = Stroke(brush: brush, points: pts)
  let settings = bs
  let dmg = s.editWith("Background Eraser", proc(doc: var Document; active: var Option[LayerId]): Rect =
    let selection = doc.selection
    let l = doc.layerPtr(id)
    if l.isNil: raise noLayer(id)
    unlockBackground(l[])
    let surf = surfaceMut(l[])
    if surf.isNil: raise bad(cmdId, "not a pixel layer")
    applyBackgroundEraser(surf[], stroke, settings, selection))
  eraseDamage(s, dmg, true)

proc eraserSpecs*(): seq[CommandSpec] =
  @[
    cmd("paint.magicEraser", "Magic Eraser", [], "",
        "{\"x\":px,\"y\":px,\"tolerance\":0..255=32,\"antiAlias\":bool=true,\"contiguous\":bool=true,\"sampleAllLayers\":bool=false,\"opacity\":1..100=100}",
        hasPixelLayerE, runProc(magicEraser(s, p))),
    cmd("paint.backgroundEraser", "Background Eraser", [], "",
        "{\"points\":[[x,y,pressure?],…],\"size\":px?,\"hardness\":0..1?,\"brush\":{…}?,\"preset\":name?,\"sampling\":\"continuous|once|backgroundSwatch\"=\"continuous\",\"limits\":\"discontiguous|contiguous|findEdges\"=\"contiguous\",\"tolerance\":0..100=50,\"protectForegroundColor\":bool=false,\"seed\":u64?}",
        hasPixelLayerE, runProc(backgroundEraser(s, p))),
  ]
