## Canvas-wide geometry: Image Rotation turns and flips, and moving every non-pixel mark (paths,
## guides, notes, slices, vector masks, gradient angles…) through an affine map (port of
## `canvas_geom.rs`). Part of the `engine/core` unit: included, not imported.

const GeomEps = 1e-9

proc mapPt(a: Affine; p: array[2, float64]): array[2, float64] =
  let q = a.apply(point(p[0], p[1]))
  [q.x, q.y]

proc mapBox(a: Affine; b: array[4, float64]): array[4, float64] =
  ## Bounding box of the four mapped corners of `[x0, y0, x1, y1]`.
  result = [F64Max, F64Max, -F64Max, -F64Max]
  for p in [[b[0], b[1]], [b[2], b[1]], [b[2], b[3]], [b[0], b[3]]]:
    let q = mapPt(a, p)
    result = [rmin(result[0], q[0]), rmin(result[1], q[1]), rmax(result[2], q[0]), rmax(result[3], q[1])]

proc mapRect*(a: Affine; r: Rect): Rect =
  if r.isEmpty: return r
  let b = mapBox(a, [float64(r.x0), float64(r.y0), float64(r.x1), float64(r.y1)])
  proc i(v: float64): int = asI32(clamp(round(v), float64(low(int32) div 2), float64(high(int32) div 2)))
  rect(i(b[0]), i(b[1]), i(b[2]), i(b[3]))

proc mapAngle(a: Affine; deg: float32): float32 =
  ## A gradient angle (degrees, counter-clockwise on screen) carried through `a`'s linear part.
  let r = toRadians(float64(deg))
  let s = sin(r)
  let c = cos(r)
  let dx = c
  let dy = -s
  let nx = a.m[0] * dx + a.m[2] * dy
  let ny = a.m[1] * dx + a.m[3] * dy
  if abs(nx) < GeomEps and abs(ny) < GeomEps: return deg
  var o = toDegrees(arctan2(-ny, nx))
  # Keep whole-degree angles whole.
  if abs(o - round(o)) < 1e-6: o = round(o)
  float32(if o <= -180.0: o + 360.0 else: o)

proc mapFill(a: Affine; f: var Fill) =
  if f.kind == fkGradient:
    f.angle = mapAngle(a, f.angle)
    let ox = float64(f.offset[0])
    let oy = float64(f.offset[1])
    f.offset = (float32(a.m[0] * ox + a.m[2] * oy), float32(a.m[1] * ox + a.m[3] * oy))

proc retagText(dpi: float32; t: var TextLayer) =
  ## Rebuilds a type layer's PSD `TySh` block for its current transform, keeping its pixels.
  if t.psdRaw.isNil: return
  let layout = shared().layout(t, dpi)
  t.psdRaw = shared(buildTysh(t, dpi, layout.bounds))

proc transformLayerGeometry*(l: var Layer; a: Affine; content: bool; dpi: float32) =
  ## Applies `a` to one layer's (and its children's) non-pixel geometry. With `content` false
  ## only the parts Free Transform doesn't move are touched.
  if l.vectorMask.isSome and (content or not l.vectorMask.get.linked):
    l.vectorMask.get.path = l.vectorMask.get.path.transform(a)
  if l.effects.reference.isSome:
    let (x, y) = l.effects.reference.get
    let p = mapPt(a, [x, y])
    l.effects.reference = some((p[0], p[1]))
  if not content:
    # Free Transform doesn't warp fill caches: drop it, the fill re-renders from its model.
    l.fillCache = none(FillCache)
  elif l.fillCache.isSome:
    mapFill(a, l.fillCache.get.fill)
  case l.content.kind
  of lkFill: mapFill(a, l.content.fill)
  of lkShape:
    if l.content.shape.fill.isSome: mapFill(a, l.content.shape.fill.get)
    if content: transformShape(l.content.shape, a)
  of lkText:
    if content:
      l.content.text.transform = a.mul(l.content.text.transform)
      retagText(dpi, l.content.text)
  of lkSmart:
    if content: transformPlacement(l.content.smart, a)
  of lkGroup:
    if l.content.group.artboard.isSome:
      l.content.group.artboard.get.rect = mapRect(a, l.content.group.artboard.get.rect)
    for c in l.content.group.children.mitems: transformLayerGeometry(c, a, content, dpi)
  else: discard

proc transformDocMarks*(doc: var Document; a: Affine) =
  ## Document-level geometry: paths, notes, Count/Ruler marks, slices and guides.
  for p in doc.paths.mitems: p.path = p.path.transform(a)
  if doc.workPath.isSome: doc.workPath = some(doc.workPath.get.transform(a))
  for n in doc.notes.mitems:
    n.position = mapPt(a, n.position)
    n.popup = mapBox(a, n.popup)
  for g in doc.measurement.countGroups.mitems:
    for p in g.points.mitems: p = mapPt(a, p)
  if doc.measurement.ruler.isSome:
    template r: untyped = doc.measurement.ruler.get
    r.start = mapPt(a, r.start)
    r.`end` = mapPt(a, r.`end`)
    if r.protractor.isSome: r.protractor = some(mapPt(a, r.protractor.get))
  for s in doc.slices.list.mitems: s.rect = mapRect(a, s.rect)
  let (m0, m1, m2, m3, tx, ty) = (a.m[0], a.m[1], a.m[2], a.m[3], a.m[4], a.m[5])
  template g: untyped = doc.guides
  if abs(m1) < GeomEps and abs(m2) < GeomEps:
    for y in g.horizontal.mitems: y = m3 * y + ty
    for x in g.vertical.mitems: x = m0 * x + tx
  elif abs(m0) < GeomEps and abs(m3) < GeomEps:
    # A horizontal guide at y becomes a vertical one at x' = m2·y + tx, and vice versa.
    var v, h: seq[float64]
    for y in g.horizontal: v.add m2 * y + tx
    for x in g.vertical: h.add m1 * x + ty
    g.horizontal = h
    g.vertical = v
  else:
    g.horizontal.setLen(0)
    g.vertical.setLen(0)

proc transformGeometry*(doc: var Document; a: Affine) =
  ## Applies `a` to all non-pixel geometry of the document.
  let dpi = doc.resolutionDpi
  for l in doc.layers.mitems: transformLayerGeometry(l, a, true, dpi)
  transformDocMarks(doc, a)

type Refresh* = enum rfShapes, rfAll

proc refreshRec(snapshot: Document; layers: var seq[Layer]; what: Refresh) =
  for l in layers.mitems:
    case l.content.kind
    of lkGroup: refreshRec(snapshot, l.content.group.children, what)
    of lkShape: refreshShape(snapshot, l.content.shape)
    of lkText:
      if what == rfAll: refreshText(snapshot, l.content.text)
    of lkSmart:
      if what == rfAll:
        try: discard refreshSmartLayer(snapshot, l)
        except CatchableError: discard
    else: discard

proc refreshDoc*(doc: var Document; what: Refresh) =
  ## Re-renders shapes (and with `rfAll` type and smart objects) after the geometry moved.
  let snapshot = doc
  refreshRec(snapshot, doc.layers, what)

type Turn* = enum tFlipHorizontal, tFlipVertical, tRotate180, tCw90, tCcw90

proc pixelMap*(t: Turn; w, h: int): proc(x, y: int): (int, int) =
  result = proc(x, y: int): (int, int) =
    case t
    of tFlipHorizontal: (w - 1 - x, y)
    of tFlipVertical: (x, h - 1 - y)
    of tRotate180: (w - 1 - x, h - 1 - y)
    of tCw90: (h - 1 - y, x)
    of tCcw90: (y, w - 1 - x)

proc affine*(t: Turn; w, h: float64): Affine =
  case t
  of tFlipHorizontal: Affine(m: [-1.0, 0.0, 0.0, 1.0, w, 0.0])
  of tFlipVertical: Affine(m: [1.0, 0.0, 0.0, -1.0, 0.0, h])
  of tRotate180: Affine(m: [-1.0, 0.0, 0.0, -1.0, w, h])
  of tCw90: Affine(m: [0.0, 1.0, -1.0, 0.0, h, 0.0])
  of tCcw90: Affine(m: [0.0, -1.0, 1.0, 0.0, 0.0, w])

proc swaps(t: Turn): bool = t in {tCw90, tCcw90}

proc remapLayers(layers: var seq[Layer]; map: proc(x, y: int): (int, int)) =
  for l in layers.mitems:
    if l.mask.isSome: l.mask.get.surface = remapSurface(l.mask.get.surface, map)
    if l.fillCache.isSome: l.fillCache.get.surface = remapSurface(l.fillCache.get.surface, map)
    case l.content.kind
    of lkRaster: l.content.raster = remapSurface(l.content.raster, map)
    of lkText:
      if l.content.text.cache.isSome: l.content.text.cache = some(remapSurface(l.content.text.cache.get, map))
    of lkShape:
      if l.content.shape.cache.isSome: l.content.shape.cache = some(remapSurface(l.content.shape.cache.get, map))
    of lkSmart:
      if l.content.smart.cache.isSome: l.content.smart.cache = some(remapSurface(l.content.smart.cache.get, map))
      if l.content.smart.filterMask.isSome:
        l.content.smart.filterMask.get.surface = remapSurface(l.content.smart.filterMask.get.surface, map)
    of lkGroup: remapLayers(l.content.group.children, map)
    else: discard

proc remapPixels(doc: var Document; map: proc(x, y: int): (int, int)) =
  ## Every pixel surface a canvas turn moves.
  remapLayers(doc.layers, map)
  for ch in doc.channels.mitems: ch.surface = remapSurface(ch.surface, map)
  if doc.quickMask.isSome: doc.quickMask.get.surface = remapSurface(doc.quickMask.get.surface, map)
  if doc.selection.isSome: doc.selection = some(remapSurface(doc.selection.get, map))

proc turnCanvas*(doc: var Document; turn: Turn) =
  ## Turns or flips the whole canvas: pixels remap exactly; vector geometry follows the map.
  let (w, h) = (doc.size.width, doc.size.height)
  remapPixels(doc, turn.pixelMap(w, h))
  let a = turn.affine(float64(doc.size.width), float64(doc.size.height))
  if turn.swaps: doc.size = size(doc.size.height, doc.size.width)
  transformGeometry(doc, a)
