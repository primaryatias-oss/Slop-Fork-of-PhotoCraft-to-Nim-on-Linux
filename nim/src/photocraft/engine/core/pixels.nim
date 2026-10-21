## Destructive pixel operations on layers (Image › Adjustments, flips, fills): port of
## `pixels.rs` and `allocation.rs`. Part of the `engine/core` unit: included, not imported.

# ---------------------------------------------------------------- allocation.rs

proc allocError(purpose, detail: string): ref EngineError =
  other("not enough memory for " & purpose & " (" & detail & "); the document was not changed")

proc checkCapacity*(len, elemSize: int; purpose: string) =
  ## `allocation::capacity`: refuse a buffer whose byte size overflows or can't exist.
  if len < 0 or (elemSize > 0 and len > high(int) div elemSize):
    raise allocError(purpose, "requested size overflowed")
  let bytes = len * elemSize
  if bytes > high(int) div 2:
    raise allocError(purpose, "not enough memory for " & $bytes & " bytes")

proc allocFilled*[T](len: int; value: T; purpose: string): seq[T] =
  checkCapacity(len, sizeof(T), purpose)
  result = newSeq[T](len)
  if value != default(T):
    for v in result.mitems: v = value

proc allocCheckpoint*(purpose: string) = discard

proc allocReadRegion*(surface: Surface; area: Rect; purpose: string): seq[float32] =
  let w = area.width
  let h = area.height
  let ch = surface.channels
  if h > 0 and w > high(int) div h: raise allocError(purpose, "requested size overflowed")
  if ch > 0 and w * h > high(int) div ch: raise allocError(purpose, "requested size overflowed")
  checkCapacity(w * h * ch, sizeof(float32), purpose)
  surface.readRegionInto(area, result)

# ---------------------------------------------------------------- compositing

proc compositeLayers*(solo: Document; format: PixelFormat; background: Option[array[3, float32]]): Surface =
  ## Composite `solo`'s layers inside their visible composite bounds (effects included) into a
  ## tiled surface, band by band.
  let canvas = solo.bounds
  var area = EmptyRect
  var any = false
  for layer in solo.layers:
    if not layer.visible: continue
    let b0 = compositeBounds(layer, canvas)
    let b = if b0.isSome: b0.get else: canvas
    if b.isEmpty: continue
    area = if any: area.union(b) else: b
    any = true
  area = area.intersect(canvas)
  var surface = newSurface(format)
  if area.isEmpty: return surface
  let channels = format.channels
  var data: seq[float32]
  renderBands(solo, area, 0, proc(band0: Buffer) =
    var band = band0
    if background.isSome:
      let color = background.get
      for pixel in band.px.mitems:
        let alpha = pixel[3]
        for channel in 0 ..< 3:
          pixel[channel] = pixel[channel] * alpha + color[channel] * (1 - alpha)
        pixel[3] = 1
    data.setLen(band.px.len * channels)
    for i, pixel in band.px:
      discard fromRgbaInto(format, pixel, data.toOpenArray(i * channels, i * channels + channels - 1))
    surface.writeRegion(band.rect, data))
  surface.prune()
  surface

# ---------------------------------------------------------------- adjustments

proc adjusted(s: Surface; r: Rect; adj: Adjustment; selection: Option[Surface]; mode: ColorMode): seq[float32] =
  ## `s`'s pixels over `r` (untouched ones included) with `adj` applied through the selection.
  let fmt = s.format
  let n = fmt.channels
  let raw = s.readRegion(r)
  var buf = Buffer(rect: r, px: newSeq[Px](raw.len div max(n, 1)))
  for i in 0 ..< buf.px.len: buf.px[i] = toRgba(fmt, raw.toOpenArray(i * n, i * n + n - 1))
  let orig = buf
  # 32-bit documents get the float behaviour of an adjustment layer there; integer depths keep
  # the unrounded, clipped curves.
  applyDepth(adj, buf, transferForDocument(mode, fmt.sample), fmt.sample, fmt.sample == stF32)
  let w = r.width
  result = newSeqOfCap[float32](raw.len)
  var enc: array[8, float32]
  for i in 0 ..< buf.px.len:
    let a = buf.px[i]
    let o = orig.px[i]
    let k = if selection.isSome: selection.get.sampleChannel(r.x0 + i mod w, r.y0 + i div w, 0) else: 1'f32
    var mixed: array[4, float32]
    for c in 0 ..< 4: mixed[c] = o[c] + (a[c] - o[c]) * k
    let m = fromRgbaInto(fmt, mixed, enc)
    for c in 0 ..< m: result.add enc[c]

proc adjustSurface*(s: var Surface; adj: Adjustment; selection: Option[Surface]; mode: ColorMode) =
  ## Applies `adj` to a surface (any model and depth, via straight RGBA) through the selection,
  ## tile by tile (the result equals one pass over the whole region).
  let r = s.contentBounds
  if r.isEmpty: return
  let fmt = s.format
  var done: seq[(TileCoord, Tile)]
  for (tc, tile) in s.takeTiles(r):
    var one = newSurface(fmt)
    one.putTiles([(tc, tile)])
    let tr = tc.rect.intersect(r)
    let outp = adjusted(one, tr, adj, selection, mode)
    one.writeRegion(tr, outp)
    for t in one.takeTiles(tc.rect): done.add t
  s.putTiles(done)

proc adjustMask*(s: var Surface; adj: Adjustment; selection: Option[Surface]) =
  ## Applies `adj` to a layer mask through the selection. Without a selection the default pixel
  ## changes too (Invert turns a reveal-all mask into a hide-all one).
  let mode = cmGrayscale
  if selection.isSome:
    let r = selection.get.contentBounds
    if not r.isEmpty:
      let outp = adjusted(s, r, adj, selection, mode)
      s.writeRegion(r, outp)
      s.prune()
    return
  let fmt = s.format
  let unit = rectXywh(0, 0, 1, 1)
  var outp = surfaceWithDefault(fmt, adjusted(surfaceWithDefault(fmt, s.defaultPixel), unit, adj, none(Surface), mode))
  let r = s.tileBounds
  if not r.isEmpty:
    outp.writeRegion(r, adjusted(s, r, adj, none(Surface), mode))
  outp.prune()
  s = outp

# ---------------------------------------------------------------- fills (tile edits)

type
  TileMask* = object
    ## `fill_cmds::Mask`: a selection over one tile (its default pixel where it has no tile).
    bytes: seq[byte]
    fmt: PixelFormat
    origin: Rect

proc maskOf*(sel: Surface; tc: TileCoord): TileMask =
  let t = sel.tile(tc)
  TileMask(bytes: (if t.isNil: sel.defaultBytes else: t.data), fmt: sel.format, origin: tc.rect)

proc at*(m: TileMask; x, y: int): float32 =
  let bpp = m.fmt.bytesPerPixel
  # A default-pixel mask is one pixel long: every position reads it.
  let i = if m.bytes.len == bpp: 0 else: ((y - m.origin.y0) * TileSize + (x - m.origin.x0)) * bpp
  if i < 0 or i + bpp > m.bytes.len: 0'f32 else: readSample(m.bytes.toOpenArray(i, i + bpp - 1), m.fmt.sample, 0)

proc full*(m: TileMask; r: Rect): bool =
  for y in r.y0 ..< r.y1:
    for x in r.x0 ..< r.x1:
      if m.at(x, y) < 1: return false
  true

type
  TileEditKind = enum tePut, teDrop
  TileEdit = object
    kind: TileEditKind
    tile: Tile

proc editTiles(s: var Surface; area: Rect; selection: Option[Surface]; purpose: string;
               whole: proc(full: bool): Option[TileEdit];
               pixel: proc(k: float32; px: var seq[float32]): bool) =
  ## Edits `s` over `area` tile by tile. `whole` may handle a tile lying wholly inside `area`
  ## and fully selected in one go; otherwise `pixel` edits each pixel (decoded channel values in
  ## and out; false = untouched). A tile is copied only when one of its pixels changes.
  let fmt = s.format
  let n = fmt.channels
  let bpp = fmt.bytesPerPixel
  var coords: seq[TileCoord]
  for tc in area.tiles: coords.add tc
  var blank: Tile
  for tc in coords:
    if s.tile(tc).isNil:
      blank = s.defaultTile
      break
  var edits: seq[(TileCoord, Tile, bool)]  # (coord, tile, drop)
  var px = newSeq[float32](n)
  var enc = newSeq[byte](bpp)
  for tc in coords:
    let tr = tc.rect.intersect(area)
    let hasMask = selection.isSome
    let mask = if hasMask: maskOf(selection.get, tc) else: TileMask()
    let full = tr == tc.rect and (not hasMask or mask.full(tr))
    let current = s.tile(tc)
    let w = whole(full)
    if w.isSome:
      case w.get.kind
      of tePut: edits.add (tc, w.get.tile, false)
      of teDrop:
        if not current.isNil: edits.add (tc, Tile(nil), true)
      continue
    let src = if current.isNil: blank else: current
    if src.isNil: continue
    var outp: Tile
    for y in tr.y0 ..< tr.y1:
      let row = (y - tc.ty * TileSize) * TileSize
      for x in tr.x0 ..< tr.x1:
        let i = (row + (x - tc.tx * TileSize)) * bpp
        if i < 0 or i + bpp > src.data.len: continue
        for c in 0 ..< n: px[c] = readSample(src.data.toOpenArray(i, i + bpp - 1), fmt.sample, c)
        if not pixel((if hasMask: mask.at(x, y) else: 1'f32), px): continue
        encodePixel(fmt, px, enc)
        var same = true
        for k in 0 ..< bpp:
          if enc[k] != src.data[i + k]:
            same = false
            break
        if same: continue
        if outp.isNil: outp = src.clone
        for k in 0 ..< bpp: outp.data[i + k] = enc[k]
    if not outp.isNil: edits.add (tc, outp, false)
  for (tc, t, drop) in edits:
    if drop: discard s.takeTiles(tc.rect)
    else: s.putTiles([(tc, t)])

proc fillPixel(fmt: PixelFormat; px: var seq[float32]; color: array[4, float32]; k: float32; lockTransparency: bool) =
  let d = toRgba(fmt, px)
  let sa = color[3] * k
  let oa = sa + d[3] * (1 - sa)
  var o: array[4, float32]
  if oa > 0:
    for c in 0 ..< 3: o[c] = (color[c] * sa + d[c] * d[3] * (1 - sa)) / oa
  o[3] = if lockTransparency: d[3] else: oa
  var enc: array[8, float32]
  let m = fromRgbaInto(fmt, o, enc)
  for i in 0 ..< min(px.len, m): px[i] = enc[i]

proc tryFillSurface*(s: var Surface; area: Rect; color: array[4, float32]; selection: Option[Surface]; lockTransparency: bool) =
  ## Fill (respecting selection coverage) with a straight RGBA colour.
  let fmt = s.format
  let opaque = color[3] == 1 and (not lockTransparency or not fmt.alpha) and
    (selection.isNone or selection.get.format.sample != stF32)
  var solid: Tile
  if opaque:
    var enc: array[8, float32]
    let n = fromRgbaInto(fmt, [color[0], color[1], color[2], 1'f32], enc)
    solid = s.solidTile(enc.toOpenArray(0, n - 1))
  editTiles(s, area, selection, "filling pixels",
    proc(full: bool): Option[TileEdit] =
      if full and not solid.isNil: some(TileEdit(kind: tePut, tile: solid)) else: none(TileEdit),
    proc(k: float32; px: var seq[float32]): bool =
      if k <= 0: return false
      fillPixel(fmt, px, color, k, lockTransparency)
      true)

proc tryClearSurface*(s: var Surface; area: Rect; selection: Option[Surface]) =
  ## Clear pixels (make transparent) within the selection.
  let fmt = s.format
  let n = fmt.channels
  if not fmt.alpha: return
  var emptyDefault = true
  for b in s.defaultBytes:
    if b != 0: emptyDefault = false
  editTiles(s, area, selection, "erasing pixels",
    proc(full: bool): Option[TileEdit] =
      if full and emptyDefault: some(TileEdit(kind: teDrop)) else: none(TileEdit),
    proc(k: float32; px: var seq[float32]): bool =
      if n < 1: return false
      px[n - 1] = px[n - 1] * (1 - k)
      # Fully cleared pixels become the empty pixel.
      if px[n - 1] <= 0:
        for v in px.mitems: v = 0
      true)
  s.prune()

proc remapSurface*(s: Surface; map: proc(x, y: int): (int, int)): Surface =
  ## Remaps a surface through a pixel-coordinate mapping (flips, rotations).
  let r = s.contentBounds
  result = surfaceWithDefault(s.format, s.defaultPixel)
  let n = s.channels
  let raw = s.readRegion(r)
  let w = r.width
  for i in 0 ..< raw.len div max(n, 1):
    let (nx, ny) = map(r.x0 + i mod w, r.y0 + i div w)
    result.writePixel(nx, ny, raw.toOpenArray(i * n, i * n + n - 1))

proc mergeDown*(docBounds: Rect; lower, upper: Layer; format: PixelFormat): Layer =
  ## Merges `upper` onto `lower` into a pixel layer (Layer › Merge Down).
  var tmp = newDocument("merge", docBounds.size, format.mode, format.sample)
  tmp.layers = @[lower, upper]
  let s = compositeLayers(tmp, format, none(array[3, float32]))
  result = newLayer(lower.name, rasterContent(s))
  result.id = lower.id
  result.blend = lower.blend
  result.opacity = 1
  # Merge Down replaces the lower layer, so keep its editing restrictions.
  result.locks = lower.locks
