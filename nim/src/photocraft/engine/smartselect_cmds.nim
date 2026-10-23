## Smart selection commands (port of `smartselect_cmds.rs`): Quick Selection, Object Selection,
## Select Subject, Select and Mask (Refine Edge) and Focus Area. The algorithms live in
## `algo/quick`, `algo/grabcut`, `algo/subject`, `algo/focus` and `algo/matting`; these commands
## pick the pixels to analyse (active layer or composite), run them, and store the result as the
## selection (or a mask / new layer).
##
## Rust keeps Select Subject's last result per session (a cache keyed by the sampled pixels); the
## port recomputes it, which gives the same region.

import std/[json, options, math, strutils]
import ./core
import ../compose
import ../algo/segbase
import ../algo/selection as asel
import ../algo/quick as aquick
import ../algo/grabcut as agrabcut
import ../algo/subject as asubject
import ../algo/focus as afocus
import ../algo/matting

proc hasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")

proc hasSelectionOrMaskE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if d.doc.selection.isSome: return ""
  if d.activeLayer.isSome:
    let l = d.doc[].findLayer(d.activeLayer.get)
    if not l.isNil and l[].mask.isSome: return ""
  "no selection or layer mask"

proc f(p: JsonNode; k: string; d: float32): float32 =
  let v = p.pget(k).asF64
  if v.isSome: float32(v.get) else: d

proc b(p: JsonNode; k: string; d: bool): bool = p.pget(k).asBool.get(d)

proc mode(p: JsonNode; default: string): SelectionMode =
  let m = p.pget("mode").asStr.get(default)
  parseSelectionMode(if m == "new": "replace" else: m)

proc hasPixels(l: Layer): bool =
  ## Rust's `Layer::surface().is_some()`.
  case l.content.kind
  of lkRaster: true
  of lkText: l.content.text.cache.isSome
  of lkShape: l.content.shape.cache.isSome
  of lkSmart: l.content.smart.cache.isSome
  else: false

proc docSampler*(doc: ptr Document; layer: Option[LayerId]; allLayers: bool): Sampler =
  ## A sampler over `layer`'s pixels in `doc` (or the composite of all visible layers with
  ## `allLayers`, or when the layer has no pixels). `doc` must outlive the sampler.
  let l = (if layer.isSome: doc[].findLayer(layer.get) else: nil)
  if not allLayers and not l.isNil and hasPixels(l[]): surfaceSampler(l[].surface)
  else: Sampler(rgba: proc(r: Rect): seq[Rgba] = compose.render(doc[], r).px)

proc docSampler*(doc: ref Document; layer: Option[LayerId]; allLayers: bool): Sampler =
  ## `docSampler` over a shared document (kept alive by the sampler).
  let held = doc
  let l = (if layer.isSome: held[].findLayer(layer.get) else: nil)
  if not allLayers and not l.isNil and hasPixels(l[]): surfaceSampler(l[].surface)
  else: Sampler(rgba: proc(r: Rect): seq[Rgba] = compose.render(held[], r).px)

proc applyRegion(s: var Session; label: string; region: Option[Region]; m: SelectionMode): JsonNode =
  ## Stores `region` combined with the current selection by `m` as one history step. Finding
  ## nothing never destroys work: the selection (and history) is left alone and the caller gets
  ## `changed: false`, whatever the mode.
  let d = s.active
  if d.isNil: raise noDocument()
  if region.isNone: return %*{"selected": d.doc.selection.isSome, "changed": false}
  let bb = region.get.bbox
  let selected = s.editWith(label, proc(doc: var Document; active: var Option[LayerId]): bool =
    doc.selection = combineRegion(doc.selection, region, m)
    doc.selection.isSome)
  %*{"selected": selected, "changed": true, "bounds": [bb.x0, bb.y0, int32(bb.width), int32(bb.height)]}

proc points(p: JsonNode): seq[(float32, float32)] =
  let a = p.pget("points")
  if not a.isArray: return
  for q in a.elems:
    if not q.isArray or q.len < 2: continue
    let (x, y) = (q.elems[0].asF64, q.elems[1].asF64)
    if x.isSome and y.isSome: result.add (float32(x.get), float32(y.get))

const Enhance = RefineParams(radius: 4, smartRadius: true, smooth: 15, feather: 0, contrast: 25, shiftEdge: 0)
  ## Auto-Enhance for Quick Selection: a light edge refinement that flows the edge towards image
  ## edges and removes blockiness.

proc quickSelection(s: var Session; p: JsonNode): JsonNode =
  let pts = points(p)
  if pts.len == 0: raise bad("select.quick", "needs at least one point")
  let size = clamp(f(p, "size", 30), 1'f32, 5000'f32)
  let m = case p.pget("mode").asStr.get("add")
    of "subtract": smSubtract
    of "replace", "new": smReplace
    else: smAdd
  let enhance = b(p, "enhanceEdge", false)
  let d = s.active
  if d.isNil: raise noDocument()
  let smp = docSampler(d.doc, d.activeLayer, b(p, "sampleAllLayers", false))
  let canvas = d.doc[].bounds
  var region = quickSelect(smp, canvas, pts, size)
  if region.isSome and enhance:
    region = refineMask(smp, regionReader(region.get), region.get.bbox, canvas, Enhance)
  applyRegion(s, "Quick Selection", region, m)

proc rectParam(p: JsonNode): Option[Rect] =
  let a = p.pget("rect")
  if not a.isArray: return
  var v: seq[float64]
  for x in a.elems:
    let n = x.asF64
    if n.isSome: v.add n.get
  if v.len < 4: return
  # Negative sizes (dragging up/left) are normalised.
  let (x0, x1) = (min(v[0], v[0] + v[2]), max(v[0], v[0] + v[2]))
  let (y0, y1) = (min(v[1], v[1] + v[3]), max(v[1], v[1] + v[3]))
  some(rect(asI32(floor(x0)), asI32(floor(y0)), asI32(ceil(x1)), asI32(ceil(y1))))

proc objectSelection(s: var Session; p: JsonNode): JsonNode =
  let r = rectParam(p)
  if r.isNone: raise bad("select.object", "needs \"rect\": [x, y, w, h]")
  let d = s.active
  if d.isNil: raise noDocument()
  let smp = docSampler(d.doc, d.activeLayer, b(p, "sampleAllLayers", false))
  let region = objectSelect(smp, d.doc[].bounds, r.get, 160_000)
  applyRegion(s, "Object Selection", region, mode(p, "replace"))

proc selectSubjectCmd(s: var Session; p: JsonNode): JsonNode =
  let d = s.active
  if d.isNil: raise noDocument()
  let smp = docSampler(d.doc, d.activeLayer, b(p, "sampleAllLayers", true))
  let region = selectSubject(smp, d.doc[].bounds)
  applyRegion(s, "Select Subject", region, mode(p, "replace"))

proc focusAreaCmd(s: var Session; p: JsonNode): JsonNode =
  let (range, noise) = (clamp(f(p, "range", 0.5), 0'f32, 1'f32), clamp(f(p, "noise", 0), 0'f32, 1'f32))
  let d = s.active
  if d.isNil: raise noDocument()
  let smp = docSampler(d.doc, d.activeLayer, b(p, "sampleAllLayers", true))
  let region = focusArea(smp, d.doc[].bounds, range, noise)
  applyRegion(s, "Focus Area", region, mode(p, "replace"))

proc refineParams(p: JsonNode): RefineParams =
  RefineParams(radius: clamp(f(p, "radius", 0), 0'f32, 250'f32), smartRadius: b(p, "smartRadius", false),
               smooth: clamp(f(p, "smooth", 0), 0'f32, 100'f32), feather: clamp(f(p, "feather", 0), 0'f32, 1000'f32),
               contrast: clamp(f(p, "contrast", 0), 0'f32, 100'f32),
               shiftEdge: clamp(f(p, "shiftEdge", 0), -100'f32, 100'f32))

const
  MaxStrokes* = 4096 ## most Refine Edge Brush strokes one Select and Mask takes
  MaxStrokePoints* = 100_000 ## most points across them

proc refineStrokes(p: JsonNode): seq[EdgeStroke] =
  ## The Refine Edge Brush strokes of `refineStrokes`:
  ## `[{"points":[[x,y],…],"size":px,"mode":"add|subtract"}]`, in painting order.
  const C = "select.refineEdge"
  let v = p.pget("refineStrokes")
  if v.isNull: return
  if not v.isArray: raise bad(C, "refineStrokes must be an array of strokes")
  if v.len > MaxStrokes: raise bad(C, "refineStrokes holds more than " & $MaxStrokes & " strokes")
  var total = 0
  for stroke in v.elems:
    if not stroke.isObject:
      raise bad(C, "each refineStrokes entry must be {\"points\":[[x,y],…],\"size\":px,\"mode\":\"add|subtract\"}")
    let pts = stroke.pget("points")
    if not pts.isArray or pts.len == 0: raise bad(C, "each stroke needs a non-empty points array")
    total += pts.len
    if total > MaxStrokePoints: raise bad(C, "refineStrokes holds more than " & $MaxStrokePoints & " points")
    var points: seq[(float32, float32)]
    for pt in pts.elems:
      var ok = pt.isArray and pt.len == 2
      var x, y: float64
      if ok:
        let (a, c) = (pt.elems[0].asF64, pt.elems[1].asF64)
        ok = a.isSome and c.isSome
        if ok:
          (x, y) = (a.get, c.get)
          proc fin(v: float64): bool = v == v and abs(v) != Inf and abs(v) <= 1e7
          ok = fin(x) and fin(y)
      if not ok: raise bad(C, "stroke points must be [x,y] pairs of finite numbers")
      points.add (float32(x), float32(y))
    var size = 30'f32
    let sv = stroke.pget("size")
    if not sv.isNil:
      let z = sv.asF64
      if z.isNone or not (z.get == z.get and abs(z.get) != Inf) or z.get < float64(BrushSizeMin) or z.get > float64(BrushSizeMax):
        raise bad(C, "stroke size must be " & $int(BrushSizeMin) & ".." & $int(BrushSizeMax) & " px")
      size = float32(z.get)
    let mv = stroke.pget("mode")
    var add = true
    if not mv.isNil:
      let m = mv.asStr
      if m.isSome and m.get == "add": add = true
      elif m.isSome and m.get == "subtract": add = false
      else: raise bad(C, "stroke mode must be add|subtract")
    result.add EdgeStroke(points: points, size: size, add: add)
  if sweptArea(result) > MaxSweptArea: raise bad(C, "refineStrokes paint too large an area")

proc refineEdge(s: var Session; p: JsonNode): JsonNode =
  ## Select and Mask: refines the selection edge, then outputs to the selection, a layer mask on
  ## the active layer, a new layer or a new document (each with or without a mask).
  ## Decontaminating colours needs new pixels, so it upgrades `selection` / `layerMask` output to
  ## `newLayerWithMask` (as Photoshop does). New-layer outputs hide the source layer; new-document
  ## outputs leave the source document untouched.
  let params = refineParams(p)
  let strokes = refineStrokes(p)
  let decont = b(p, "decontaminate", false)
  let amount = clamp(f(p, "amount", 100), 0'f32, 100'f32)
  let d = s.active
  if d.isNil: raise noDocument()
  var active = d.activeLayer
  if active.isSome and d.doc[].findLayer(active.get).isNil: active = none(LayerId)
  let activeL = (if active.isSome: d.doc[].findLayer(active.get) else: nil)
  let defaultOutput = (if d.doc.selection.isNone and not activeL.isNil and activeL[].mask.isSome: "layerMask"
                       else: "selection")
  var output = p.pget("output").asStr.get(defaultOutput)
  if output notin ["selection", "layerMask", "newLayer", "newLayerWithMask", "newDocument", "newDocumentWithMask"]:
    raise bad("select.refineEdge", "output must be selection|layerMask|newLayer|newLayerWithMask|newDocument|newDocumentWithMask")
  if decont and output in ["selection", "layerMask"]: output = "newLayerWithMask"
  var region = none(Region)
  block sample:
    let smp = docSampler(d.doc, d.activeLayer, b(p, "sampleAllLayers", false))
    var sel: Surface
    if d.doc.selection.isSome: sel = d.doc.selection.get
    elif not activeL.isNil and activeL[].mask.isSome: sel = activeL[].mask.get.surface
    else: break sample
    let brush = newEdgeBrush(strokes, d.doc[].bounds)
    region = refineMaskBrushed(smp, surfaceReader(sel), sel.contentBounds, d.doc[].bounds, params, brush)
  case output
  of "selection":
    let selected = s.editWith("Select and Mask", proc(doc: var Document; act: var Option[LayerId]): bool =
      doc.selection = (if region.isSome: some(regionSurface(region.get)) else: none(Surface))
      doc.selection.isSome)
    return %*{"output": output, "selected": selected}
  of "layerMask":
    if active.isNone: raise other("no active layer")
    let id = active.get
    s.edit("Select and Mask", proc(doc: var Document; act: var Option[LayerId]) =
      let surface = (if region.isSome: regionSurface(region.get) else: newSurface(Gray8))
      let l = doc.findLayer(id)
      if l.isNil: raise noLayer(id)
      var m = revealAllMask()
      m.surface = surface
      l[].mask = some(m)
      doc.selection = none(Surface))
    return %*{"output": output, "layer": uint64(id), "selected": false}
  else:
    if active.isNone: raise other("no active layer")
    let id = active.get
    let srcLayer = d.doc[].findLayer(id)
    if srcLayer.isNil: raise noLayer(id)
    if not hasPixels(srcLayer[]):
      raise other("the active layer is " & srcLayer[].content.article & " " & srcLayer[].content.kindName &
                  " layer without pixels")
    let src = srcLayer[].surface
    let name = d.doc[].copyName(srcLayer[].name)
    let reg = region.get(Region(bbox: EmptyRect))
    let pixels = if decont: decontaminate(src, reg, params.radius, amount) else: src
    let withMask = output in ["newLayerWithMask", "newDocumentWithMask"]
    var content: Surface
    var mask = none(LayerMask)
    if withMask:
      content = pixels
      var m = revealAllMask()
      m.surface = regionSurface(reg)
      mask = some(m)
    else: content = maskedCopy(pixels, reg)
    if output.startsWith("newDocument"):
      # Same canvas, colour mode, depth and profile, so the pixels need no conversion.
      var doc = newDocument("Untitled", d.doc.size, d.doc.mode, d.doc.depth)
      doc.resolutionDpi = d.doc.resolutionDpi
      doc.iccProfile = d.doc.iccProfile
      doc.colorTable = d.doc.colorTable
      doc.duotone = d.doc.duotone
      var layer = newLayer(srcLayer[].name, LayerContent(kind: lkRaster, raster: content))
      layer.mask = mask
      let layerId = layer.id
      doc.layers.add layer
      let i = s.addDocument(doc)
      return %*{"output": output, "document": i, "layer": uint64(layerId), "selected": false}
    let newId = s.editWith("Select and Mask", proc(doc: var Document; act: var Option[LayerId]): LayerId =
      var layer = newLayer(name, LayerContent(kind: lkRaster, raster: content))
      layer.mask = mask
      let nid = doc.insertAbove(some(id), layer)
      let l = doc.findLayer(id)
      if not l.isNil: l[].visible = false
      doc.selection = none(Surface)
      act = some(nid)
      nid)
    return %*{"output": output, "layer": uint64(newId), "selected": false}

proc smartSelectSpecs*(): seq[CommandSpec] =
  @[cmd("select.quick", "Quick Selection", [], "",
        """{"points":[[x,y],…],"size":px=30,"mode":"add|subtract|replace"="add","sampleAllLayers":bool=false,"enhanceEdge":bool=false}""",
        enabledProc(hasDocE(s)), runProc(quickSelection(s, p))),
    cmd("select.object", "Object Selection", [], "",
        """{"rect":[x,y,w,h],"mode":"replace|add|subtract|intersect"="replace","sampleAllLayers":bool=false}""",
        enabledProc(hasDocE(s)), runProc(objectSelection(s, p))),
    cmd("select.subject", "Subject", ["Select"], "",
        """{"sampleAllLayers":bool=true,"mode":"replace|add|subtract|intersect"="replace"}""",
        enabledProc(hasDocE(s)), runProc(selectSubjectCmd(s, p))),
    cmd("select.refineEdge", "Refine Edge", [], "",
        """{"radius":px=0,"smartRadius":bool=false,"smooth":0..100=0,"feather":px=0,"contrast":0..100=0,"shiftEdge":-100..100=0,"decontaminate":bool=false,"amount":0..100=100,"output":"selection|layerMask|newLayer|newLayerWithMask|newDocument|newDocumentWithMask"="selection","sampleAllLayers":bool=false,"refineStrokes":[{"points":[[x,y],…],"size":px=30,"mode":"add|subtract"="add"},…]=[]} (refineStrokes: the Refine Edge Brush, in painting order: add refines the painted area from the image, subtract keeps the original edge there)""",
        enabledProc(hasSelectionOrMaskE(s)), runProc(refineEdge(s, p))),
    cmd("select.focusArea", "Focus Area…", ["Select"], "",
        """{"range":0..1=0.5,"noise":0..1=0,"sampleAllLayers":bool=true,"mode":"replace|add|subtract|intersect"="replace"}""",
        enabledProc(hasDocE(s)), runProc(focusAreaCmd(s, p)))]

# ---------------------------------------------------------------- Remove Background

const
  RemoveBackgroundCmd* = "layer.removeBackground"
  Refine = RefineParams(radius: 2, smartRadius: true, smooth: 10, feather: 0.5, contrast: 10, shiftEdge: 0)
    ## Edge refinement after Select Subject: a narrow smart radius and a little smoothing, so soft
    ## edges and hair get partial coverage instead of a hard cut.

proc cutoutCheck(doc: Document; l: Layer): string =
  ## Remove Background needs an unlocked pixel layer ("" = fine).
  if l.content.kind != lkRaster:
    return "the layer is " & l.content.article & " " & l.content.kindName & " layer, not a pixel layer"
  let locks = doc.effectiveLocks(l.id)
  if locks.pixels or locks.all:
    return "Could not complete your request because the layer \"" & l.name & "\" is locked"
  ""

proc cutoutE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  let (l, why) = activeLayerOf(s)
  if l.isNil: return why
  cutoutCheck(d.doc[], l[])

proc removeBackground(s: var Session; p: JsonNode): JsonNode =
  ## Remove Background, Photoshop's Quick Action for a pixel layer (Properties › Quick Actions):
  ## Select Subject with a light edge refinement, turned into a layer mask, in one history step.
  ## Like Photoshop it is non-destructive (the pixels stay; the mask hides the background), turns
  ## the Background layer into a normal layer first and has no menu item.
  let id = layerParam(s, p)
  let d = s.active
  if d.isNil: raise noDocument()
  let l = d.doc[].findLayer(id)
  if l.isNil: raise noLayer(id)
  let why = cutoutCheck(d.doc[], l[])
  if why.len > 0: raise other(why)
  let sampleAll = p.pget("sampleAllLayers").asBool.get(false)
  let refine = p.pget("refine").asBool.get(true)
  editJob(s, "Remove Background",
    proc(doc: var Document; active: var Option[LayerId]; ctx: JobCtx): array[4, int32] =
      ctx.progress(0.1, "Finding subject")
      let smp = docSampler(addr doc, some(id), sampleAll)
      let found = selectSubject(smp, doc.bounds)
      if found.isNone: raise other("no subject found")
      var region = found.get
      if ctx.cancelled: raise cancelledErr()
      if refine:
        ctx.progress(0.6, "Refining edge")
        let refined = refineMask(smp, regionReader(region), region.bbox, doc.bounds, Refine)
        if refined.isSome: region = refined.get
        if ctx.cancelled: raise cancelledErr()
      backgroundToLayerForMask(doc, id)
      let tl = doc.findLayer(id)
      if tl.isNil: raise noLayer(id)
      var m = revealAllMask()
      m.surface = regionSurface(region)
      tl[].mask = some(m)
      doc.selection = none(Surface)
      let bb = region.bbox
      [int32(bb.x0), int32(bb.y0), int32(bb.width), int32(bb.height)],
    proc(bounds: array[4, int32]): JsonNode = %*{"layer": uint64(id), "bounds": bounds})

proc cutoutSpecs*(): seq[CommandSpec] =
  ## Remove Background (no menu path: a Properties Quick Action in Photoshop).
  @[cmd(RemoveBackgroundCmd, "Remove Background", [], "",
        """{"layer":id?,"sampleAllLayers":bool=false,"refine":bool=true} → {layer,bounds} (adds a layer mask from Select Subject; the Background becomes a normal layer)""",
        enabledProc(cutoutE(s)), runProc(removeBackground(s, p)))]
