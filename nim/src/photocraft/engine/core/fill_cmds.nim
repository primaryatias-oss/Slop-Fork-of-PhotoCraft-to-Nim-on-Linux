## Edit › Fill… (`edit.fill`): Photoshop's Fill dialog as one command (port of `fill_cmds.rs`).
## Contents: Foreground / Background / Color / Content-Aware / Pattern / History / Black / 50%
## Gray / White, blended with `mode` at `opacity`, optionally preserving the target's
## transparency. Part of the `engine/core` unit: included, not imported.

const
  FillCmd = "edit.fill"
  FillContents* = ["foreground", "background", "color", "contentAware", "pattern", "history", "black", "gray", "white"]

proc fillBad(msg: string): ref EngineError = bad(FillCmd, msg)

type
  FillSourceKind = enum fsColor, fsPixels
  FillSource = object
    ## What a pixel of the filled area gets before blending.
    case kind: FillSourceKind
    of fsColor: color: array[4, float32]
    of fsPixels:
      rect: Rect
      px: seq[array[4, float32]]

proc at(src: FillSource; x, y: int): array[4, float32] =
  case src.kind
  of fsColor: src.color
  of fsPixels:
    if not src.rect.contains(x, y): return
    let i = (y - src.rect.y0) * src.rect.width + (x - src.rect.x0)
    if i < src.px.len: src.px[i] else: [0'f32, 0, 0, 0]

proc fillContents(p: JsonNode): string =
  let v = p.pget("contents")
  if v.isNull: return "color"
  if v.kind == JString:
    for k in FillContents:
      if cmpIgnoreCase(k, v.str) == 0 and k.len == v.str.len: return k
    raise fillBad("unknown contents `" & v.str & "` (" & FillContents.join("|") & ")")
  raise fillBad("`contents` must be a string, not " & jsonText(v))

proc fillMode(p: JsonNode): BlendMode =
  let v = p.pget("mode")
  if v.isNull: return bmNormal
  if v.kind == JString:
    let m = blendFromStr(v.str)
    if m.isNone or m.get == bmPassThrough: raise fillBad("unknown blend mode `" & v.str & "`")
    return m.get
  raise fillBad("`mode` must be a blend mode name, not " & jsonText(v))

proc fillOpacity(p: JsonNode): float32 =
  let v = p.pget("opacity")
  if v.isNull: return 1
  let o = v.asF64
  if o.isNone or o.get != o.get or abs(o.get) == Inf: raise fillBad("`opacity` must be a number 0..100, not " & jsonText(v))
  float32(clamp(o.get, 0.0, 100.0) / 100.0)

proc fillFlag(p: JsonNode; k: string; d: bool): bool =
  let v = p.pget(k)
  if v.isNull: return d
  if v.kind == JBool: return v.bval
  raise fillBad("`" & k & "` must be true or false, not " & jsonText(v))

type FillBlend = object
  mode: BlendMode
  opacity: float32
  keepAlpha: bool  ## leave every pixel's alpha as it was
  restore: bool    ## Normal mode replaces the pixels, transparency included (History)

proc lerpPremul(d, c: array[4, float32]; k: float32): array[4, float32] =
  ## `d` moved towards `c` by `k`, in premultiplied space (straight RGBA in and out).
  let a = d[3] * (1 - k) + c[3] * k
  if a <= 0: return
  for i in 0 ..< 3: result[i] = (d[i] * d[3] * (1 - k) + c[i] * c[3] * k) / a
  result[3] = a

proc blendTile(bytes: var seq[byte]; fmt: PixelFormat; origin, tr: Rect; source: FillSource; hasMask: bool;
               mask: TileMask; b: FillBlend) =
  ## Blends into one tile's encoded pixels over `tr`.
  let n = fmt.channels
  let bpp = fmt.bytesPerPixel
  let blend = if b.mode == bmDissolve: bmNormal else: b.mode
  var px = newSeq[float32](n)
  var enc: array[8, float32]
  # An opaque colour at full coverage in Normal mode just replaces the colour (and the alpha
  # unless it is kept): copy its encoded bytes.
  var opaque = false
  var opaqueBytes: seq[byte]
  var keep = bpp
  if source.kind == fsColor and source.color[3] >= 1 and b.mode == bmNormal:
    let m = fromRgbaInto(fmt, source.color, enc)
    opaqueBytes = newSeq[byte](bpp)
    encodePixel(fmt, enc.toOpenArray(0, min(m, n) - 1), opaqueBytes)
    keep = if b.keepAlpha and fmt.alpha: bpp - bpp div max(n, 1) else: bpp
    opaque = true
  var outb = newSeq[byte](bpp)
  for y in tr.y0 ..< tr.y1:
    let row = (y - origin.y0) * TileSize
    for x in tr.x0 ..< tr.x1:
      var k = (if hasMask: mask.at(x, y) else: 1'f32) * b.opacity
      if k <= 0: continue
      let i = (row + (x - origin.x0)) * bpp
      if i < 0 or i + bpp > bytes.len: continue
      if k >= 1 and opaque:
        for j in 0 ..< keep: bytes[i + j] = opaqueBytes[j]
        continue
      for c in 0 ..< n: px[c] = readSample(bytes.toOpenArray(i, i + bpp - 1), fmt.sample, c)
      let d = toRgba(fmt, px)
      var c = source.at(x, y)
      if b.mode == bmDissolve:
        # Dissolve: each pixel is either fully painted or untouched, with the coverage as odds.
        k = if ditherNoise(x, y) < c[3] * k: 1'f32 else: 0'f32
        c[3] = 1
        if k <= 0: continue
      var o = if b.restore and blend == bmNormal: lerpPremul(d, c, k) else: composite(blend, d, c, k)
      if b.keepAlpha or not fmt.alpha: o[3] = d[3]
      let m = fromRgbaInto(fmt, o, enc)
      encodePixel(fmt, enc.toOpenArray(0, min(m, n) - 1), outb)
      for j in 0 ..< bpp: bytes[i + j] = outb[j]

proc blendInto(surf: var Surface; area: Rect; source: FillSource; sel: Option[Surface]; b: FillBlend) =
  ## Blends `source` into `surf` over `area`: per pixel, the selection coverage times the
  ## opacity, in the blend mode; tile by tile on the encoded pixels.
  let fmt = surf.format
  var solid: Tile
  if source.kind == fsColor and source.color[3] >= 1 and b.mode == bmNormal and b.opacity >= 1 and
      (not b.keepAlpha or not fmt.alpha):
    var enc: array[8, float32]
    let n = fromRgbaInto(fmt, source.color, enc)
    solid = surf.solidTile(enc.toOpenArray(0, n - 1))
  var done: seq[(TileCoord, Tile)]
  for (tc, tile) in surf.takeTiles(area):
    let tr = tc.rect.intersect(area)
    let hasMask = sel.isSome
    let mask = if hasMask: maskOf(sel.get, tc) else: TileMask()
    if not solid.isNil and tr == tc.rect and (not hasMask or mask.full(tr)):
      done.add (tc, solid)
      continue
    var t = tile.clone
    blendTile(t.data, fmt, tc.rect, tr, source, hasMask, mask, b)
    done.add (tc, t)
  surf.putTiles(done)

proc readRgbaChecked(src: Surface; area: Rect; purpose: string): seq[array[4, float32]] =
  let w = area.width
  let h = area.height
  if h > 0 and w > high(int) div h:
    raise other("not enough memory for " & purpose & " (requested size overflowed); the document was not changed")
  result = allocFilled(w * h, [0'f32, 0, 0, 0], purpose)
  if result.len > 0: src.readRgbaInto(area, result)

proc compositeOver*(surf: var Surface; src: Surface; area: Rect; limit: Option[Surface]) =
  ## `src` over `surf` across `area` (Normal, 100 %), weighted by `limit` when given.
  let source = FillSource(kind: fsPixels, rect: area, px: readRgbaChecked(src, area, "pasting pixels"))
  blendInto(surf, area, source, limit, FillBlend(mode: bmNormal, opacity: 1))

proc fillColor*(surf: var Surface; area: Rect; color: array[4, float32]; sel: Option[Surface]) =
  ## `area` of `surf` filled with an opaque `color` (Normal, 100 %), weighted by `sel`.
  blendInto(surf, area, FillSource(kind: fsColor, color: color), sel, FillBlend(mode: bmNormal, opacity: 1))

proc blendColorMask*(surf: var Surface; area: Rect; color: array[4, float32]; coverage: Surface; mode: BlendMode;
                     opacity: float32; preserveTransparency: bool) =
  ## Edit › Fill's compositing for Edit › Stroke.
  blendInto(surf, area, FillSource(kind: fsColor, color: color), some(coverage),
            FillBlend(mode: mode, opacity: opacity, keepAlpha: preserveTransparency))

proc samplingWindowFor*(hole, canvas: Rect): Rect =
  ## Photoshop's Content-Aware sampling window around the selection bounds `hole`.
  caware.samplingWindow(hole.intersect(canvas), canvas)

proc windowSize*(window: Rect): (int, int) =
  ## The size of a Content-Aware sampling window, or an error for an absurd one.
  let w = window.width
  let h = window.height
  if uint64(w) * uint64(h) > 400_000_000'u64: raise other("the selection is too large for Content-Aware fill")
  (w, h)

proc readWindow*(surf: Surface; r: Rect; cancelled: proc(): bool): Option[seq[float32]] =
  ## `readRegion` of `r` in bands, asking `cancelled` before each (none once it says so).
  const Band = 64
  let n = surf.channels
  let w = r.width
  let h = r.height
  var outp = newSeq[float32](w * h * n)
  if outp.len == 0: return some(outp)
  var buf: seq[float32]
  var y = 0
  while y < h:
    if cancelled(): return none(seq[float32])
    let rows = min(Band, h - y)
    surf.readRegionInto(rect(r.x0, r.y0 + y, r.x1, r.y0 + y + rows), buf)
    if buf.len == rows * w * n: copyMem(addr outp[y * w * n], addr buf[0], buf.len * sizeof(float32))
    y += rows
  if cancelled(): none(seq[float32]) else: some(outp)

proc readWindowCover*(surf: Surface; r: Rect; cancelled: proc(): bool): Option[seq[bool]] =
  ## Which pixels of `r` a selection (or mask) covers at all: channel 0 above zero.
  let n = max(surf.channels, 1)
  let all = readWindow(surf, r, cancelled)
  if all.isNone: return none(seq[bool])
  var outp = newSeq[bool](r.width * r.height)
  for i in 0 ..< outp.len: outp[i] = all.get[i * n] > 0
  some(outp)

proc readWindowMask*(surf: Surface; r: Rect; cancelled: proc(): bool): Option[seq[float32]] =
  ## Channel 0 of `readWindow`.
  let n = max(surf.channels, 1)
  let all = readWindow(surf, r, cancelled)
  if all.isNone or n == 1: return all
  var outp = newSeq[float32](all.get.len div n)
  for i in 0 ..< outp.len: outp[i] = all.get[i * n]
  some(outp)

proc contentAwareSource(surf, sel: Surface; canvas: Rect; colorAdaptation: bool; seed: uint64): FillSource =
  ## The selection filled from its surroundings, as straight RGBA over the sampling window.
  let holeBounds = sel.contentBounds.intersect(canvas)
  if holeBounds.isEmpty: raise other("the selection is outside the canvas")
  let window = samplingWindowFor(holeBounds, canvas)
  let fmt = surf.format
  let n = fmt.channels
  let (w, h) = windowSize(window)
  let img = surf.readRegion(window)
  var hole = newSeq[bool](w * h)
  var allHole = true
  for i in 0 ..< hole.len:
    hole[i] = sel.sampleChannel(window.x0 + i mod w, window.y0 + i div w, 0) > 0
    if not hole[i]: allHole = false
  if allHole: raise other("Content-Aware fill needs pixels outside the selection to sample from")
  var source = newSeq[bool](w * h)
  for v in source.mitems: v = true
  let opts = caware.FillOptions(colorAdaptation: colorLevel(if colorAdaptation: "default" else: "none"),
                                rotations: rotationLevel("none"), scale: false, mirror: false, seed: seed)
  if img.len != w * h * n: raise other("could not read the layer pixels")
  let filled = caware.fill(w, h, n, img, hole, source, opts)
  var px = newSeq[array[4, float32]](filled.len div max(n, 1))
  for i in 0 ..< px.len: px[i] = toRgba(fmt, filled.toOpenArray(i * n, i * n + n - 1))
  FillSource(kind: fsPixels, rect: window, px: px)

proc fillCmd*(s: var Session; p: JsonNode): JsonNode =
  ## Edit › Fill.
  let contents = fillContents(p)
  let mode = fillMode(p)
  let opacity = fillOpacity(p)
  let preserve = fillFlag(p, "preserveTransparency", false)
  let channel = isChannelTarget(p)
  let id = if channel: none(LayerId) else: some(layerParam(s, p))
  let st = s.activeOrRaise()
  let docBounds = st.doc[].bounds
  let sel = st.doc.selection
  var area = if sel.isSome: sel.get.contentBounds else: docBounds
  var outp = %*{"contents": contents}
  let restore = contents == "history"
  var source: FillSource
  case contents
  of "foreground": source = FillSource(kind: fsColor, color: s.tools.foreground)
  of "background": source = FillSource(kind: fsColor, color: s.tools.background)
  of "color": source = FillSource(kind: fsColor, color: colorParam(p, "color", s.tools.foreground))
  of "black": source = FillSource(kind: fsColor, color: [0'f32, 0, 0, 1])
  of "gray":
    let g = 128'f32 / 255'f32
    source = FillSource(kind: fsColor, color: [g, g, g, 1'f32])
  of "white": source = FillSource(kind: fsColor, color: [1'f32, 1, 1, 1])
  of "pattern":
    let pat = resolvePatternParam(s, FillCmd, p)
    let (scale, angle, _, phase) = patternPlacement(p)
    let tile = newPatternTile(pat)
    if tile.isNil: raise fillBad("the pattern is empty")
    # Photoshop's Fill uses the canvas origin.
    let place = placement(EmptyRect, false, phase, scale, angle)
    outp["pattern"] = %pat.id
    source = FillSource(kind: fsPixels, rect: area, px: renderPattern(tile, place, area))
  of "history":
    var explicit = none(int)
    let v = p.pget("state")
    if not v.isNull:
      let u = v.asU64
      if u.isNone: raise fillBad("`state` must be a history state index, not " & jsonText(v))
      explicit = some(int(min(u.get, uint64(high(int)))))
    let past = st.history.pastLen
    var sourceDoc: DocRef
    if explicit.isSome and explicit.get == past: sourceDoc = st.doc
    elif explicit.isSome:
      let d = st.history.state(explicit.get)
      if d.isNone: raise fillBad("no history state " & $explicit.get & " (0..=" & $past & ")")
      sourceDoc = d.get
    else:
      # The History Brush's default source: the oldest state still held.
      let d = st.history.state(0)
      sourceDoc = if d.isSome: d.get else: st.doc
    var src: Surface
    try: src = readTargetSurface(sourceDoc[], id, p)
    except EngineError: raise other("the target did not exist (as pixels, a mask or a channel) in that history state")
    source = FillSource(kind: fsPixels, rect: area, px: readRgbaChecked(src, area, "reading Fill's history source"))
  of "contentAware":
    if sel.isNone: raise other("Content-Aware fill needs a selection")
    if channel or isMaskTarget(p): raise other("Content-Aware fill works on layer pixels only")
    let adapt = fillFlag(p, "colorAdaptation", true)
    if id.isNone: raise other("no active layer")
    let l = st.doc[].findLayer(id.get)
    if l.isNil or not l[].hasSurface: raise other("Content-Aware fill needs a pixel layer")
    let seed = p.pget("seed").asU64.get(1)
    source = contentAwareSource(l[].surface, sel.get, docBounds, adapt, seed)
  else: raise fillBad("unknown contents `" & contents & "`")
  area = area.intersect(docBounds)
  if area.isEmpty: return outp
  s.editDo("Fill"):
    let sel = doc.selection
    let (surf, lock) = targetSurface(doc, id, p)
    blendInto(surf[], area, source, sel, FillBlend(mode: mode, opacity: opacity, keepAlpha: preserve or lock, restore: restore))
  outp
