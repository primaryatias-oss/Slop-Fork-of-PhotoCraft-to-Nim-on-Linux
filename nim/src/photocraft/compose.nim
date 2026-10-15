## CPU compositor: the reference renderer. Flattens a document's layer tree into straight-alpha
## float RGBA with Photoshop's semantics: blend modes (Photoshop-exact variants), opacity and
## Fill opacity, clipping masks, pass-through groups, adjustment layers, fill layers, masks
## (pixel, vector, feathered), layer styles, Blend If, channel restrictions, knockouts and the
## advanced blending options, artboards and multichannel spot inks.
##
## Rendering is tiled (256 px) so working memory stays bounded; effect maps are built once per
## render per layer state and kept in a byte-budgeted cache across renders.

import std/[math, options, tables, hashes, deques, os]
import ./doc, ./vector, ./color/convert
import ./compose/[buffer, adjust, psblend, gradient, pattern, masks, effects, shape_split]
export buffer, adjust, psblend, gradient, pattern, masks, effects, shape_split

const
  RenderTile* = 256
  BandPixels* = 8'i64 shl 20
  ProxyThumbnailPixels* = 16'i64 shl 20
  DefaultFxCacheBudget* = 768 shl 20

type
  Scope = object
    root, isolated: bool
  VmEntry = object
    mask: VectorMask
    compiled: CompiledVectorMask
  Ctx = ref object
    canvas: Rect
    transfer: Transfer
    light: GlobalLight
    patterns: PreparedPatterns
    mode: ColorMode
    depth: SampleType
    vectorMasks: Table[int, VmEntry]
    fxMaps: Table[Hash, (ref FxMaps, bool)]
    fxClip: Option[Rect]
    doc: ptr Document
  FxEntry = object
    maps: ref FxMaps
    pin: Layer
    bytes: int
  KoState = object
    active: bool
    shape: seq[float32]
    before: Buffer
    knocked: Option[Buffer]
    opacity: float32

const
  ScopeRoot = Scope(root: true, isolated: false)
  ScopeIsolated = Scope(root: false, isolated: true)

proc passThrough(s: Scope): Scope = Scope(root: false, isolated: s.isolated)

var fxCache {.threadvar.}: Table[Hash, FxEntry]
var fxOrder {.threadvar.}: Deque[Hash]
var fxBytes {.threadvar.}: int
var fxBudget = DefaultFxCacheBudget
var iccRefs {.threadvar.}: seq[(SharedBytes, IccRef)]

# ---------------------------------------------------------------- forward declarations

proc compositeStackAt(layers: openArray[Layer]; backdrop: var Buffer; cx: Ctx; scope: Scope)
proc compositeLayer(layer: Layer; clipped: openArray[Layer]; backdrop: var Buffer; cx: Ctx;
                    ko: Option[Buffer]; scope: Scope)
proc compositeAtop(layer: Layer; base: var Buffer; cx: Ctx)
proc renderContent(layer: Layer; rc: Rect; cx: Ctx): Option[Buffer]
proc layerBounds*(layer: Layer; canvas: Rect): Rect

# ---------------------------------------------------------------- context

proc cmykSpace*(doc: Document): CmykSpace =
  ## The document's own CMYK space (nil: not CMYK, no profile, or the built-in one).
  if doc.mode != cmCmyk or doc.iccProfile.isNil or doc.iccProfile.data.len == 0: return nil
  for (k, r) in iccRefs:
    if cast[pointer](k) == cast[pointer](doc.iccProfile): return cmykSpaceFor(r)
  var r: IccRef
  new r
  r[] = doc.iccProfile.data
  if iccRefs.len >= 16: iccRefs.delete(0)
  iccRefs.add (doc.iccProfile, r)
  cmykSpaceFor(r)

proc ctxForDoc(doc: Document): Ctx =
  Ctx(doc: unsafeAddr doc, canvas: doc.bounds, transfer: transferForDocument(doc.mode, doc.depth),
      light: doc.globalLight, patterns: newPreparedPatterns(doc.patterns, doc.pixelFormat.mode),
      mode: doc.mode, depth: doc.depth)

proc setFxClip(cx: Ctx; rc: Rect) =
  if cx.fxClip != some(rc):
    cx.fxClip = some(rc)
    var drop: seq[Hash]
    for k, v in cx.fxMaps:
      if v[1]: drop.add k
    for k in drop: cx.fxMaps.del k

proc vectorMaskValues(cx: Ctx; vm: VectorMask; rc: Rect): seq[float32] =
  let key = cast[int](unsafeAddr vm)
  cx.vectorMasks.withValue(key, e):
    if e.mask == vm: return e.compiled.render(rc)
  let c = compileVectorMask(vm)
  cx.vectorMasks[key] = VmEntry(mask: vm, compiled: c)
  c.render(rc)

# ---------------------------------------------------------------- small helpers

proc mix(a, b: Px; k: float32): Px {.inline.} =
  let alpha = a[3] + (b[3] - a[3]) * k
  if alpha <= 0: return
  for c in 0 .. 2: result[c] = clamp((a[c] * a[3] + (b[c] * b[3] - a[c] * a[3]) * k) / alpha, 0'f32, 1'f32)
  result[3] = min(alpha, 1'f32)

proc quantize(b: var Buffer; q: float32) =
  for p in b.px.mitems:
    for v in p.mitems: v = floor(v * q + 0.5'f32) / q

proc dissolveNoise(x, y: int): float32 {.inline.} =
  var h = (cast[uint32](int32(x)) * 0x8da6_b343'u32) xor (cast[uint32](int32(y)) * 0xd816_3841'u32) xor 0x9e37_79b9'u32
  h = h xor (h shr 15)
  h = h * 0x2c1b_3c6d'u32
  h = h xor (h shr 12)
  float32(h and 0xffff) / 65536

proc maskK(m: Option[seq[float32]]; i: int): float32 {.inline.} =
  if m.isSome: m.get[i] else: 1

proc ones(n: int): seq[float32] =
  result = newSeq[float32](max(n, 0))
  for v in result.mitems: v = 1

proc hasEnabledMask(l: Layer): bool = l.mask.isSome and l.mask.get.enabled
proc hasEnabledVectorMask(l: Layer): bool = l.vectorMask.isSome and l.vectorMask.get.enabled

proc surfaceAlphaDefaultTransparent(s: Surface): bool =
  let d = s.defaultPixel
  s.format.alpha and d.len > 0 and d[^1] <= 0

proc textGamma*(layer: Layer): float32 =
  ## The blending gamma for the layer's content (text blends at the text gamma).
  if layer.content.kind == lkText: textGamma() else: 1

# ---------------------------------------------------------------- advanced blending

proc hidesEffects(layer: Layer): bool =
  let a = layer.advanced
  hasEffects(layer) and ((a.layerMaskHidesEffects and hasEnabledMask(layer)) or
                         (a.vectorMaskHidesEffects and hasEnabledVectorMask(layer)))

proc stripHidingMasks(layer: Layer): Layer =
  result = layer
  if result.advanced.layerMaskHidesEffects: result.mask = none(LayerMask)
  if result.advanced.vectorMaskHidesEffects: result.vectorMask = none(VectorMask)

proc clipsIndividually(layer: Layer; clipped: openArray[Layer]): bool =
  if layer.advanced.blendClipped or layer.content.kind == lkAdjustment: return false
  for c in clipped:
    if c.visible: return true

proc advancedActive*(layer: Layer; clipped: openArray[Layer]): bool =
  ## Whether any non-default advanced blending option changes how the layer renders.
  let a = layer.advanced
  if a.isDefault: return false
  let fx = hasEffects(layer)
  a.knockout != koNone or clipsIndividually(layer, clipped) or (a.blendInterior and fx) or
    (not a.transparencyShapes and fx) or hidesEffects(layer)

proc shapeless*(layer: Layer): bool =
  ## Transparency Shapes Layer off: the layer's footprint is its whole canvas.
  not layer.advanced.transparencyShapes and (layer.advanced.knockout != koNone or hasEffects(layer))

proc isBackground*(l: Layer): bool =
  l.name == "Background" and l.locks.transparency and not l.clipped and l.content.kind == lkRaster

proc needsFloor(layers: openArray[Layer]): bool =
  for l in layers:
    if l.visible and not l.clipped and l.advanced.knockout == koShallow: return true

# ---------------------------------------------------------------- multichannel

proc inks*(doc: Document; rc: Rect): Buffer =
  ## The spot channels of a Multichannel document mixed as inks over white paper.
  result = filledBuffer(rc, [1'f32, 1, 1, 1])
  var vals: seq[float32]
  for ch in doc.channels:
    if ch.spot.isNone: continue
    if ch.surface.contentBounds.intersect(rc).isEmpty: continue
    let (ink, solidity) = ch.spot.get
    let c = ink.toRgb
    let s = clamp(solidity, 0'f32, 1'f32)
    let n = max(ch.surface.channels, 1)
    ch.surface.readRegionInto(rc, vals)
    for i in 0 ..< result.px.len:
      if i * n >= vals.len: break
      let v = clamp(vals[i * n], 0'f32, 1'f32)
      if v <= 0: continue
      let keep = 1 - s * v
      for j in 0 .. 2:
        result.px[i][j] = keep * result.px[i][j] * (1 - v + v * c[j]) + s * v * c[j]

proc backdropFor(doc: Document; rc: Rect): Buffer =
  if doc.mode == cmMultichannel: inks(doc, rc) else: transparentBuffer(rc)

proc deepTarget(cx: Ctx; rc: Rect): Buffer =
  if cx.doc.isNil: return transparentBuffer(rc)
  result = backdropFor(cx.doc[], rc)
  if cx.doc[].layers.len > 0:
    template bg: untyped = cx.doc[].layers[0]
    if bg.visible and isBackground(bg):
      compositeLayer(bg, [], result, cx, none(Buffer), ScopeRoot)

proc knockoutTarget(layer: Layer; floor: Option[Buffer]; rc: Rect; cx: Ctx; scope: Scope): Option[Buffer] =
  proc deep(rc: Rect; cx: Ctx; scope: Scope): Buffer =
    if scope.isolated: transparentBuffer(rc) else: deepTarget(cx, rc)
  case layer.advanced.knockout
  of koNone: none(Buffer)
  of koShallow: (if floor.isSome: floor else: some(deep(rc, cx, scope)))
  of koDeep: some(deep(rc, cx, scope))

# ---------------------------------------------------------------- masks and content

proc maskVals(layer: Layer; rc: Rect; cx: Ctx): Option[seq[float32]] =
  if hasFeather(layer):
    let s = combinedMask(layer, cx.canvas)
    if s.isSome:
      var v: seq[float32]
      s.get.readRegionInto(rc, v)
      return some(v)
  var vector: Option[seq[float32]]
  if layer.vectorMask.isSome: vector = some(cx.vectorMaskValues(layer.vectorMask.get, rc))
  if layer.mask.isNone: return vector
  var v: seq[float32]
  layer.mask.get.valuesInto(rc, v)
  if vector.isSome:
    for i in 0 ..< min(v.len, vector.get.len): v[i] *= vector.get[i]
  some(v)

proc paintBounds*(layer: Layer): Option[Rect] =
  ## A shape layer's path control bounds (where gradient/pattern effects are framed).
  if layer.content.kind != lkShape: return none(Rect)
  let cb = layer.content.shape.path.controlBounds
  if cb.isNone: return none(Rect)
  let (x0, y0, x1, y1) = cb.get
  let r = rect(int(floor(x0)), int(floor(y0)), int(floor(x1)), int(floor(y1)))
  if r.isEmpty: none(Rect) else: some(r)

proc fillFrame*(layer: Layer; canvas: Rect): Rect =
  ## The frame a fill layer's gradient or pattern is laid out in: the canvas, narrowed to the
  ## revealed part of a hide-all mask and to the vector mask's bounds.
  result = canvas
  if layer.content.kind == lkFill and layer.content.fill.kind == fkGradient and not layer.content.fill.align:
    return
  if layer.mask.isSome and layer.mask.get.enabled:
    let d = layer.mask.get.surface.defaultPixel
    if d.len > 0 and d[0] <= 0:
      let b = layer.mask.get.surface.contentBounds.intersect(canvas)
      if not b.isEmpty: result = b
  if layer.vectorMask.isSome and layer.vectorMask.get.enabled and not layer.vectorMask.get.path.inverted:
    let cb = layer.vectorMask.get.path.controlBounds
    if cb.isSome:
      let (x0, y0, x1, y1) = cb.get
      let b = rect(int(floor(x0)), int(floor(y0)), int(ceil(x1)), int(ceil(y1))).intersect(result)
      if not b.isEmpty: result = b

proc renderFill(f: Fill; rc, canvas: Rect; patterns: var PreparedPatterns; quantum: float32): Buffer =
  case f.kind
  of fkSolid:
    let rgb = f.color.toRgb
    filledBuffer(rc, [rgb[0], rgb[1], rgb[2], f.color.alpha])
  of fkGradient:
    Buffer(rect: rc, px: renderGradient(f, rc, canvas, quantum))
  of fkPattern:
    let tile = patterns.get(f.id, f.name)
    if tile.isNil: transparentBuffer(rc)
    else: Buffer(rect: rc, px: renderPattern(tile, placement(canvas, f.link, f.phase, f.scale, f.angle), rc))

proc renderFillContent*(layer: Layer; f: Fill; canvas: Rect; patterns: seq[Pattern]): Buffer =
  ## A fill layer's fill over `canvas` (laid out in its fill frame), without masks.
  var prepared = newPreparedPatterns(patterns)
  renderFill(f, canvas, fillFrame(layer, canvas), prepared, 0)

proc compositeStack(layers: openArray[Layer]; backdrop: var Buffer; cx: Ctx) =
  compositeStackAt(layers, backdrop, cx, ScopeIsolated)

proc renderContent(layer: Layer; rc: Rect; cx: Ctx): Option[Buffer] =
  var buf: Buffer
  case layer.content.kind
  of lkGroup:
    buf = transparentBuffer(rc)
    compositeStack(layer.content.group.children, buf, cx)
  of lkFill:
    template f: untyped = layer.content.fill
    if layer.fillCache.isSome and layer.fillCache.get.fill == f:
      buf = surfaceToBuffer(layer.fillCache.get.surface, rc)
    elif cx.mode == cmCmyk:
      let frame = fillFrame(layer, cx.canvas)
      let solid = f.kind == fkSolid
      let s = renderCmykFill(f, (if solid: rect(0, 0, 1, 1) else: rc), frame, cx.depth)
      if s.isSome:
        buf = (if solid: filledBuffer(rc, s.get.rgba(0, 0)) else: surfaceToBuffer(s.get, rc))
      else:
        buf = renderFill(f, rc, frame, cx.patterns, adjustmentQuantum(cx.depth))
    else:
      buf = renderFill(f, rc, fillFrame(layer, cx.canvas), cx.patterns, adjustmentQuantum(cx.depth))
  of lkAdjustment:
    return none(Buffer)
  else:
    buf = (if layer.hasSurface: surfaceToBuffer(layer.surface, rc) else: transparentBuffer(rc))
  let m = maskVals(layer, rc, cx)
  if m.isSome:
    for i in 0 ..< min(buf.px.len, m.get.len): buf.px[i][3] *= m.get[i]
  some(buf)

proc effectShape(layer: Layer; rc: Rect; cx: Ctx): seq[float32] =
  let n = rc.width * rc.height
  if not layer.advanced.transparencyShapes:
    let m = maskVals(layer, rc, cx)
    return (if m.isSome: m.get else: ones(n))
  let outline = effectOutline(layer)
  if outline.isNone or n == 0:
    let c = renderContent(layer, rc, cx)
    if c.isNone: return newSeq[float32](n)
    result = newSeq[float32](n)
    for i in 0 ..< n: result[i] = c.get.px[i][3]
    return
  var a = newSeq[float32](n)
  if layer.hasSurface:
    let b = surfaceToBuffer(layer.surface, rc)
    for i in 0 ..< n: a[i] = b.px[i][3]
  let cov = pathCoverage(outline.get, rc)
  let mv = maskVals(layer, rc, cx)
  let (w, h) = (rc.width, rc.height)
  result = newSeq[float32](n)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = y * w + x
      var v: float32
      if cov[i] >= 1 - 1e-3'f32:
        v = 1
      else:
        var f = 0'f32
        for yy in max(y - 1, 0) ..< min(y + 2, h):
          for xx in max(x - 1, 0) ..< min(x + 2, w):
            f = max(f, a[yy * w + xx])
        v = clamp(a[i] + (1 - min(f, 1'f32)) * cov[i], 0'f32, 1'f32)
      result[i] = v * maskK(mv, i)

# ---------------------------------------------------------------- bounds

proc effectSourceBounds*(layer: Layer; canvas: Rect): Rect
proc shapelessStrokeBounds(layer: Layer): Option[Rect]

proc layerBounds*(layer: Layer; canvas: Rect): Rect =
  ## Bounds of the layer's visible content (with a shape's path when it carries effects).
  if shapeless(layer): return canvas
  case layer.content.kind
  of lkGroup:
    if layer.content.group.artboard.isSome: return layer.content.group.artboard.get.rect
    result = EmptyRect
    for c in layer.content.group.children:
      if not c.visible: continue
      let b = layerBounds(c, canvas)
      if b.isEmpty: continue
      result = (if result.isEmpty: b else: result.union(b))
  of lkFill:
    result = canvas
  else:
    let b = (if layer.hasSurface: layer.surface.contentBounds else: EmptyRect)
    let p = (if effectOutline(layer).isSome and hasEffects(layer): paintBounds(layer) else: none(Rect))
    if p.isSome: result = (if b.isEmpty: p.get else: b.union(p.get))
    else: result = b

proc effectSourceBounds*(layer: Layer; canvas: Rect): Rect =
  ## Bounds of the pixels the layer's effects are computed from (children's effects included).
  if layer.content.kind == lkGroup and layer.content.group.artboard.isNone and not shapeless(layer):
    result = EmptyRect
    for c in layer.content.group.children:
      if not c.visible: continue
      let m = (if hasEffects(c): margin(c) else: 0)
      var b = effectSourceBounds(c, canvas)
      if not b.isEmpty: b = b.inflate(m)
      if b.isEmpty: continue
      result = (if result.isEmpty: b else: result.union(b))
  else:
    result = layerBounds(layer, canvas)

proc transparentOutside*(layer: Layer): bool =
  ## Whether the layer is transparent outside its content (no opaque default pixel).
  case layer.content.kind
  of lkGroup:
    for c in layer.content.group.children:
      if c.visible and not transparentOutside(c): return false
    true
  of lkFill, lkAdjustment: true
  else:
    not layer.hasSurface or surfaceAlphaDefaultTransparent(layer.surface)

proc emptyIn(layer: Layer; rc: Rect): bool =
  if hasEffects(layer):
    let canvas = rect(int(low(int32)) div 4, int(low(int32)) div 4, int(high(int32)) div 4, int(high(int32)) div 4)
    let sb = shapelessStrokeBounds(layer)
    let bounds = (if sb.isSome: sb.get else: effectSourceBounds(layer, canvas))
    return transparentOutside(layer) and bounds.inflate(margin(layer)).intersect(rc).isEmpty
  case layer.content.kind
  of lkRaster, lkText, lkShape, lkSmart:
    if not layer.hasSurface: return true
    let s = layer.surface
    not s.hasTilesIn(rc) and surfaceAlphaDefaultTransparent(s)
  else: false

proc shapelessStrokeBounds(layer: Layer): Option[Rect] =
  if layer.advanced.transparencyShapes or layer.advanced.knockout != koNone or
      layer.advanced.blendInterior or layer.clipped or not layer.blendIf.isDefault or
      hasEnabledMask(layer) or hasEnabledVectorMask(layer) or not transparentOutside(layer) or
      not mapsAreLocal(layer):
    return none(Rect)
  if not layer.effects.enabled: return none(Rect)
  var anyOn = false
  for e in layer.effects.items:
    if e.enabled:
      anyOn = true
      if e.kind != ekStroke: return none(Rect)
  if not anyOn: return none(Rect)
  case layer.content.kind
  of lkRaster, lkText, lkShape, lkSmart:
    let b = (if layer.hasSurface: layer.surface.contentBounds else: EmptyRect)
    let p = (if effectOutline(layer).isSome: paintBounds(layer) else: none(Rect))
    if p.isSome: some(if b.isEmpty: p.get else: b.union(p.get))
    else: some(b)
  else: none(Rect)

proc compositeBounds*(layer: Layer; canvas: Rect): Option[Rect] =
  ## The canvas area the layer can change when composited; none when it may touch everything.
  if not transparentOutside(layer): return none(Rect)
  var own: Rect
  case layer.content.kind
  of lkGroup:
    if layer.content.group.artboard.isNone:
      own = EmptyRect
      for c in layer.content.group.children:
        if not c.visible: continue
        let b = compositeBounds(c, canvas)
        if b.isNone: return none(Rect)
        if not b.get.isEmpty: own = (if own.isEmpty: b.get else: own.union(b.get))
    else:
      own = layerBounds(layer, canvas)
  of lkAdjustment: own = EmptyRect
  else: own = layerBounds(layer, canvas)
  if own.isEmpty: return some(EmptyRect)
  let m = (if hasEffects(layer): margin(layer) else: 0)
  some(own.inflate(m).intersect(canvas))

proc reachesBelow(l: Layer): bool =
  case l.content.kind
  of lkAdjustment: not l.clipped
  of lkGroup:
    if l.blend != bmPassThrough: return false
    for c in l.content.group.children:
      if reachesBelow(c): return true
    false
  else: false

proc changeBounds*(layer: Layer; canvas: Rect): Option[Rect] =
  ## Like `compositeBounds`, but none for layers that change what is below them.
  if layer.content.kind == lkAdjustment or reachesBelow(layer): return none(Rect)
  compositeBounds(layer, canvas)

proc blendIfActive*(layer: Layer; mode: ColorMode): bool =
  mode in {cmRgb, cmGrayscale, cmDuotone} and not layer.blendIf.isDefault

proc occludesBelow*(layer: Layer; mode: ColorMode): bool =
  ## An opaque, plain Normal fill layer hides everything beneath it.
  if layer.content.kind != lkFill: return false
  template f: untyped = layer.content.fill
  var opaque: bool
  case f.kind
  of fkSolid: opaque = f.color.alpha >= 1
  of fkGradient:
    opaque = f.stops.len > 0
    for (_, c) in f.stops:
      if c.alpha < 1: opaque = false
    for (_, o) in f.opacityStops:
      if o < 1: opaque = false
  of fkPattern: opaque = false
  let cached = layer.fillCache.isSome and layer.fillCache.get.fill == f
  opaque and not cached and layer.visible and not layer.clipped and layer.blend == bmNormal and
    layer.opacity >= 1 and layer.fillOpacity >= 1 and not hasEnabledMask(layer) and
    not hasEnabledVectorMask(layer) and not hasEffects(layer) and layer.excludedChannels == 0 and
    not blendIfActive(layer, mode)

proc channelWeights*(layer: Layer; mode: ColorMode): Option[array[3, float32]] =
  ## Per-channel weights from the Advanced Blending channel checkboxes; none when all are on.
  let x = layer.excludedChannels
  if x == 0: return none(array[3, float32])
  proc keep(x: uint32; bit: int): float32 = (if (x and (1'u32 shl bit)) != 0: 0 else: 1)
  var w: array[3, float32]
  case mode
  of cmRgb: w = [keep(x, 0), keep(x, 1), keep(x, 2)]
  of cmGrayscale, cmDuotone: w = [keep(x, 0), keep(x, 0), keep(x, 0)]
  else: return none(array[3, float32])
  if w == [1'f32, 1, 1]: none(array[3, float32]) else: some(w)

proc restoreChannels(outp: var Buffer; before: Buffer; w: array[3, float32]) =
  for i in 0 ..< outp.px.len:
    for c in 0 .. 2:
      outp.px[i][c] = before.px[i][c] + (outp.px[i][c] - before.px[i][c]) * w[c]

proc blendIfWeight(layer: Layer; mode: ColorMode; this, under: Option[Px]): float32 =
  result = 1
  let sides = [this, under]
  for side in 0 .. 1:
    if sides[side].isNone: continue
    let p = sides[side].get
    template v(c: int): float32 = clamp(p[c], 0'f32, 1'f32) * 255
    if mode == cmRgb:
      let gray = 0.299'f32 * v(0) + 0.587'f32 * v(1) + 0.114'f32 * v(2)
      result *= layer.blendIf.get(0)[side].weight(gray)
      for c in 0 .. 2: result *= layer.blendIf.get(c + 1)[side].weight(v(c))
    else:
      result *= layer.blendIf.get(0)[side].weight(v(0)) * layer.blendIf.get(1)[side].weight(v(0))
    if result <= 0: return 0

proc applyBlendIf(layer: Layer; before: Buffer; outp: var Buffer; cx: Ctx) =
  let own = (if layer.content.kind == lkAdjustment: none(Buffer) else: renderContent(layer, outp.rect, cx))
  for i in 0 ..< outp.px.len:
    let p = outp.px[i]
    let b = before.px[i]
    var this: Option[Px]
    if own.isSome:
      if own.get.px[i][3] > 0: this = some(own.get.px[i])
    else: this = some(p)
    let under = (if b[3] > 0: some(b) else: none(Px))
    let k = blendIfWeight(layer, cx.mode, this, under)
    if k >= 1: continue
    let a = b[3] + (p[3] - b[3]) * k
    if a > 0:
      var r: Px
      for c in 0 .. 2: r[c] = clamp((b[c] * b[3] + (p[c] * p[3] - b[c] * b[3]) * k) / a, 0'f32, 1'f32)
      r[3] = a
      outp.px[i] = r
    else:
      outp.px[i] = [0'f32, 0, 0, 0]

# ---------------------------------------------------------------- blending buffers

proc blendIntoG(backdrop: var Buffer; src: Buffer; mode: BlendMode; opacity, gamma: float32) =
  let rc = backdrop.rect
  let w = max(rc.width, 1)
  for i in 0 ..< backdrop.px.len:
    var s = src.px[i]
    if s[3] <= 0: continue
    if mode == bmDissolve:
      let (x, y) = (rc.x0 + i mod w, rc.y0 + i div w)
      s[3] = (if dissolveNoise(x, y) < s[3] * opacity: 1 else: 0)
      backdrop.px[i] = compositePs(bmNormal, backdrop.px[i], s, 1)
      continue
    backdrop.px[i] = compositeGamma(mode, backdrop.px[i], s, opacity, gamma)

proc blendInto(backdrop: var Buffer; src: Buffer; mode: BlendMode; opacity: float32) =
  blendIntoG(backdrop, src, mode, opacity, 1)

proc blendIntoFill(backdrop: var Buffer; src: Buffer; mode: BlendMode; opacity, fill, gamma: float32) =
  if fill >= 1 or not fillIsSpecial(mode):
    blendIntoG(backdrop, src, mode, opacity * fill, gamma)
    return
  for i in 0 ..< backdrop.px.len:
    if src.px[i][3] > 0:
      backdrop.px[i] = compositeFill(mode, backdrop.px[i], src.px[i], opacity, fill, gamma)

# ---------------------------------------------------------------- knockouts and clipping

proc knockoutShape(layer: Layer; rc: Rect; cx: Ctx): seq[float32] =
  let n = rc.width * rc.height
  proc masks(layer: Layer; rc: Rect; cx: Ctx; n: int): seq[float32] =
    let m = maskVals(layer, rc, cx)
    if m.isSome: m.get else: ones(n)
  if not layer.advanced.transparencyShapes: return masks(layer, rc, cx, n)
  let c = renderContent(layer, rc, cx)
  if c.isNone: return masks(layer, rc, cx, n)
  result = newSeq[float32](c.get.px.len)
  for i, p in c.get.px: result[i] = p[3]

proc knockoutBegin(layer: Layer; backdrop: var Buffer; target: Buffer; clipAlpha: bool; cx: Ctx): KoState =
  result.shape = knockoutShape(layer, backdrop.rect, cx)
  if target.rect != backdrop.rect or result.shape.len != backdrop.px.len: return
  result.active = true
  result.before = backdrop
  for i in 0 ..< backdrop.px.len:
    let k = result.shape[i]
    if k > 0:
      let a = backdrop.px[i][3]
      backdrop.px[i] = mix(backdrop.px[i], target.px[i], min(k, 1'f32))
      if clipAlpha: backdrop.px[i][3] = min(backdrop.px[i][3], a)
  result.opacity = layer.opacity
  if layer.opacity < 1: result.knocked = some(backdrop)

proc knockoutEnd(st: KoState; backdrop: var Buffer) =
  if not st.active or st.knocked.isNone: return
  let rest = 1 - clamp(st.opacity, 0'f32, 1'f32)
  template knocked: untyped = st.knocked.get
  for i in 0 ..< backdrop.px.len:
    if st.shape[i] <= 0: continue
    let p = backdrop.px[i]
    let b = st.before.px[i]
    let k = knocked.px[i]
    var pm = [p[0] * p[3], p[1] * p[3], p[2] * p[3], p[3]]
    for c in 0 .. 2: pm[c] += rest * (b[c] * b[3] - k[c] * k[3])
    pm[3] += rest * (b[3] - k[3])
    let a = clamp(pm[3], 0'f32, 1'f32)
    backdrop.px[i] = (if a > 0: [clamp(pm[0] / a, 0'f32, 1'f32), clamp(pm[1] / a, 0'f32, 1'f32),
                                  clamp(pm[2] / a, 0'f32, 1'f32), a]
                      else: [0'f32, 0, 0, 0])

template knockout(layer: Layer; backdrop: var Buffer; target: Buffer; clipAlpha: bool; cx: Ctx; draw: untyped) =
  let koState = knockoutBegin(layer, backdrop, target, clipAlpha, cx)
  draw
  knockoutEnd(koState, backdrop)

proc compositeClipped(clipped: openArray[Layer]; base: var Buffer; cx: Ctx) =
  var anyKo = false
  for c in clipped:
    if c.visible and c.advanced.knockout != koNone: anyKo = true
  let floor = (if anyKo: some(base) else: none(Buffer))
  for c in clipped:
    if not c.visible: continue
    if floor.isSome and c.advanced.knockout != koNone:
      knockout(c, base, floor.get, true, cx):
        compositeAtop(c, base, cx)
    else:
      compositeAtop(c, base, cx)

proc compositeClippedIndividually(layer: Layer; clipped: openArray[Layer]; backdrop: var Buffer; cx: Ctx;
                                  ko: Option[Buffer]; scope: Scope) =
  let rc = backdrop.rect
  let n = backdrop.px.len
  var anyShallow = false
  for c in clipped:
    if c.visible and c.advanced.knockout == koShallow: anyShallow = true
  let floor = (if anyShallow: some(backdrop) else: none(Buffer))
  compositeLayer(layer, [], backdrop, cx, ko, scope)
  let op = clamp(layer.opacity, 0'f32, 1'f32)
  var clip: seq[float32]
  let content = renderContent(layer, rc, cx)
  if content.isSome:
    clip = newSeq[float32](content.get.px.len)
    for i, p in content.get.px: clip[i] = p[3] * op
  else:
    let m = maskVals(layer, rc, cx)
    clip = (if m.isSome: m.get else: ones(n))
    for v in clip.mitems: v *= op
  if clip.len != n: return
  for c in clipped:
    if not c.visible: continue
    let before = backdrop
    let t = knockoutTarget(c, floor, rc, cx, Scope(root: false, isolated: scope.isolated))
    compositeLayer(c, [], backdrop, cx, t, scope)
    for i in 0 ..< n:
      let k = clip[i]
      if k < 1: backdrop.px[i] = mix(before.px[i], backdrop.px[i], max(k, 0'f32))

proc hidingMask(layer: Layer; rc: Rect; cx: Ctx): Option[seq[float32]] =
  if not hidesEffects(layer): return none(seq[float32])
  let a = layer.advanced
  var only = newLayer(layer.name, rasterContent(newSurface(Rgba32f)))
  only.id = layer.id
  if a.layerMaskHidesEffects: only.mask = layer.mask
  if a.vectorMaskHidesEffects: only.vectorMask = layer.vectorMask
  maskVals(only, rc, cx)

proc applyHidingMask(outp: var Buffer; before: Buffer; mask: seq[float32]) =
  for i in 0 ..< min(outp.px.len, mask.len):
    let k = mask[i]
    if k < 1: outp.px[i] = mix(before.px[i], outp.px[i], max(k, 0'f32))

# ---------------------------------------------------------------- effect maps

proc surfaceFp(s: Surface): Hash =
  var h: Hash = hash(ord(s.format.mode)) !& hash(ord(s.format.sample)) !& hash(s.format.alpha)
  for v in s.defaultPixel: h = h !& hash(cast[uint32](v))
  var acc = uint64(s.tileCount)
  for c, t in s.tiles:
    var x = uint64(cast[uint](t)) xor (uint64(cast[uint32](int32(c.tx))) shl 40) xor (uint64(cast[uint32](int32(c.ty))) shl 8)
    x = x * 0x9e37_79b9_7f4a_7c15'u64
    acc = acc + (x xor (x shr 29))
  !$(h !& hash(acc))

proc layerIdentity(layer: Layer; h: var Hash) =
  h = h !& hash(uint64(layer.id)) !& hash(cast[uint32](layer.fillOpacity))
  case layer.content.kind
  of lkGroup:
    for c in layer.content.group.children:
      h = h !& hash(c.visible) !& hash(cast[uint32](c.opacity)) !& hash(c.clipped) !& hash(ord(c.blend))
      layerIdentity(c, h)
    h = h !& hash($toJ(layer.content.group.artboard))
  of lkFill:
    h = h !& hash($toJ(layer.content.fill))
    if layer.fillCache.isSome and layer.fillCache.get.fill == layer.content.fill:
      h = h !& surfaceFp(layer.fillCache.get.surface)
  of lkAdjustment:
    h = h !& hash($toJ(layer.content.adjustment))
  else:
    h = h !& (if layer.hasSurface: surfaceFp(layer.surface) else: 0)
  if layer.mask.isSome:
    template m: untyped = layer.mask.get
    h = h !& surfaceFp(m.surface) !& hash(m.enabled) !& hash(cast[uint32](m.density)) !& hash(cast[uint32](m.feather))
  if layer.vectorMask.isSome: h = h !& hash($toJ(layer.vectorMask.get))
  h = h !& hash($toJ(layer.effects)) !& 0xfe

proc setEffectCacheBudget*(bytes: int) = fxBudget = max(bytes, 1 shl 20)
proc effectCacheBudget*(): int = fxBudget
proc effectCacheBytes*(): int = fxBytes

proc purgeEffectCache*(): int =
  ## Drops every cached effect map; returns the bytes freed.
  result = fxBytes
  fxCache.clear()
  fxOrder.clear()
  fxBytes = 0

proc textureCtx(layer: Layer; region: Rect; cx: Ctx): TextureCtx =
  let pb = paintBounds(layer)
  let sb = (if pb.isSome: pb.get else: layerBounds(layer, cx.canvas))
  TextureCtx(rect: region, anchor: layer.effects.reference.get((float64(sb.x0), float64(sb.y0))))

proc effectRegion(layer: Layer; rc: Rect; cx: Ctx): (Rect, bool) =
  let m = margin(layer)
  let region = effectSourceBounds(layer, cx.canvas).inflate(m).intersect(cx.canvas.inflate(m))
  if cx.fxClip.isSome:
    let c = cx.fxClip.get
    if c.containsRect(rc) and shapeless(layer) and mapsAreLocal(layer):
      let part = region.intersect(c.inflate(satAdd(m, m)))
      return (part, part != region)
  (region, false)

proc buildEffectMaps(layer: Layer; region: Rect; cx: Ctx): ref FxMaps =
  let shape = (if region.isEmpty: newSeq[float32]() else: effectShape(layer, region, cx))
  new result
  result[] = buildMaps(layer, shape, region, cx.light, textureCtx(layer, region, cx), cx.patterns)

proc cachedEffectMaps(layer: Layer; region: Rect; key: Hash; cx: Ctx): ref FxMaps =
  fxCache.withValue(key, e):
    return e.maps
  result = buildEffectMaps(layer, region, cx)
  let bytes = result[].bytes
  fxCache[key] = FxEntry(maps: result, pin: layer, bytes: bytes)
  fxOrder.addLast key
  fxBytes += bytes
  while fxBytes > fxBudget and fxOrder.len > 1:
    let old = fxOrder.popFirst
    if old == key:
      fxOrder.addLast old
      continue
    fxCache.withValue(old, e):
      fxBytes = max(fxBytes - e.bytes, 0)
    fxCache.del old

proc effectMaps(layer: Layer; rc: Rect; cx: Ctx): ref FxMaps =
  let (region, clipped) = effectRegion(layer, rc, cx)
  if existsEnv("PHOTOCRAFT_FX_NOCACHE"): return buildEffectMaps(layer, region, cx)
  var h: Hash = 0
  layerIdentity(layer, h)
  h = h !& hash((region.x0, region.y0, region.x1, region.y1)) !&
      hash(cast[uint32](cx.light.angle)) !& hash(cast[uint32](cx.light.altitude))
  let key = !$h
  cx.fxMaps.withValue(key, e):
    return e[0]
  result = cachedEffectMaps(layer, region, key, cx)
  cx.fxMaps[key] = (result, clipped)

# ---------------------------------------------------------------- shapes split into fill + stroke

proc splitParts(layer: Layer; rc: Rect; cx: Ctx): Option[(Buffer, Buffer, Option[seq[float32]])] =
  if layer.content.kind != lkShape or layer.content.shape.stroke.isNone: return
  let parts = splitShape(layer.content.shape, cx.canvas, cx.depth)
  if parts.isNone: return
  some((surfaceToBuffer(parts.get[0], rc), surfaceToBuffer(parts.get[1], rc), maskVals(layer, rc, cx)))

proc shapeParts(layer: Layer; clipped: openArray[Layer]; rc: Rect; cx: Ctx): Option[(Buffer, Buffer, Option[seq[float32]])] =
  var anyVisible = false
  for c in clipped:
    if c.visible: anyVisible = true
  if not anyVisible: return
  splitParts(layer, rc, cx)

# ---------------------------------------------------------------- compositing

proc compositeLayerEffects(layer: Layer; clipped: openArray[Layer]; backdrop: var Buffer; cx: Ctx) =
  let rc = backdrop.rect
  var content: Buffer
  var vstroke: Option[VectorStroke]
  let parts = splitParts(layer, rc, cx)
  if parts.isSome:
    content = parts.get[0]
    vstroke = some(VectorStroke(stroke: parts.get[1], mask: parts.get[2]))
  else:
    let c = renderContent(layer, rc, cx)
    if c.isNone: return
    content = c.get
  compositeClipped(clipped, content, cx)
  let maps = effectMaps(layer, rc, cx)
  let pb = paintBounds(layer)
  compositeWithEffects(layer, content, backdrop, maps[], (if pb.isSome: pb.get else: layerBounds(layer, cx.canvas)),
                       cx.patterns, vstroke)

proc compositeLayerPlain(layer: Layer; clipped: openArray[Layer]; backdrop: var Buffer; cx: Ctx; scope: Scope) =
  let rc = backdrop.rect
  if (clipped.len == 0 or shapelessStrokeBounds(layer).isNone) and emptyIn(layer, rc): return
  let opacity = layer.opacity * layer.fillOpacity
  if layer.content.kind == lkGroup and layer.blend == bmPassThrough and layer.fillOpacity >= 1 and
      not hasEffects(layer):
    let needsMix = opacity < 1 or layer.mask.isSome or layer.vectorMask.isSome
    var hasClipped = false
    for c in clipped:
      if c.visible: hasClipped = true
    var before: Buffer
    if needsMix or hasClipped: before = backdrop
    compositeStackAt(layer.content.group.children, backdrop, cx, scope.passThrough)
    if needsMix:
      let mv = maskVals(layer, rc, cx)
      for i in 0 ..< backdrop.px.len:
        let k = opacity * maskK(mv, i)
        let a = before.px[i]
        let b = backdrop.px[i]
        let wa = a[3] * (1 - k)
        let wb = b[3] * k
        let alpha = wa + wb
        if alpha > 0:
          var r: Px
          for c in 0 .. 2: r[c] = (a[c] * wa + b[c] * wb) / alpha
          r[3] = alpha
          backdrop.px[i] = r
        else:
          backdrop.px[i] = [0'f32, 0, 0, 0]
    if hasClipped:
      let iso = renderContent(layer, rc, cx)
      if iso.isSome:
        var clippedIso = iso.get
        compositeClipped(clipped, clippedIso, cx)
        var without = before
        blendInto(without, iso.get, bmNormal, opacity)
        var with = before
        blendInto(with, clippedIso, bmNormal, opacity)
        for i in 0 ..< backdrop.px.len:
          let p = backdrop.px[i]
          let (w, wo) = (with.px[i], without.px[i])
          var pm = [p[0] * p[3], p[1] * p[3], p[2] * p[3], p[3]]
          for c in 0 .. 2: pm[c] += w[c] * w[3] - wo[c] * wo[3]
          pm[3] += w[3] - wo[3]
          let a = clamp(pm[3], 0'f32, 1'f32)
          backdrop.px[i] = (if a > 0: [clamp(pm[0] / a, 0'f32, 1'f32), clamp(pm[1] / a, 0'f32, 1'f32),
                                        clamp(pm[2] / a, 0'f32, 1'f32), a]
                            else: [0'f32, 0, 0, 0])
    return
  if layer.content.kind == lkAdjustment:
    var adjusted = backdrop
    applyDepth(layer.content.adjustment, adjusted, cx.transfer, cx.depth)
    compositeClipped(clipped, adjusted, cx)
    let mv = maskVals(layer, rc, cx)
    for i in 0 ..< backdrop.px.len:
      let k = opacity * maskK(mv, i)
      if k <= 0: continue
      let b = backdrop.px[i]
      let a = adjusted.px[i]
      let bl = blendRgbPs(layer.blend, [b[0], b[1], b[2]], [a[0], a[1], a[2]])
      backdrop.px[i] = [b[0] + (bl[0] - b[0]) * k, b[1] + (bl[1] - b[1]) * k, b[2] + (bl[2] - b[2]) * k, b[3]]
    return
  if hasEffects(layer):
    let hide = hidingMask(layer, rc, cx)
    var before: Buffer
    if hide.isSome: before = backdrop
    if hidesEffects(layer):
      compositeLayerEffects(stripHidingMasks(layer), clipped, backdrop, cx)
    else:
      compositeLayerEffects(layer, clipped, backdrop, cx)
    if hide.isSome: applyHidingMask(backdrop, before, hide.get)
    return
  let sp = shapeParts(layer, clipped, rc, cx)
  if sp.isSome:
    var content = sp.get[0]
    compositeClipped(clipped, content, cx)
    for i in 0 ..< content.px.len:
      content.px[i] = compositePs(bmNormal, content.px[i], sp.get[1].px[i], 1)
      content.px[i][3] *= maskK(sp.get[2], i)
    blendIntoFill(backdrop, content, layer.blend, layer.opacity, layer.fillOpacity, 1)
    return
  let c = renderContent(layer, rc, cx)
  if c.isNone: return
  var content = c.get
  compositeClipped(clipped, content, cx)
  blendIntoFill(backdrop, content, layer.blend, layer.opacity, layer.fillOpacity, textGamma(layer))

proc compositeArtboard(layer: Layer; ab: Artboard; clipped: openArray[Layer]; backdrop: var Buffer; cx: Ctx; scope: Scope) =
  let board = ab.rect.intersect(backdrop.rect)
  if board.isEmpty: return
  let (bw, w) = (board.width, backdrop.rect.width)
  var sub = transparentBuffer(board)
  for y in board.y0 ..< board.y1:
    let o = (y - backdrop.rect.y0) * w + (board.x0 - backdrop.rect.x0)
    let so = (y - board.y0) * bw
    for i in 0 ..< bw: sub.px[so + i] = backdrop.px[o + i]
  let bg = ab.background.rgba
  if bg.isSome: blendInto(sub, filledBuffer(board, bg.get), bmNormal, 1)
  compositeLayerPlain(layer, clipped, sub, cx, scope)
  for y in board.y0 ..< board.y1:
    let o = (y - backdrop.rect.y0) * w + (board.x0 - backdrop.rect.x0)
    let so = (y - board.y0) * bw
    for i in 0 ..< bw: backdrop.px[o + i] = sub.px[so + i]

proc compositeLayerAny(layer: Layer; clipped: openArray[Layer]; backdrop: var Buffer; cx: Ctx; scope: Scope) =
  if layer.content.kind == lkGroup and layer.content.group.artboard.isSome:
    compositeArtboard(layer, layer.content.group.artboard.get, clipped, backdrop, cx, scope)
  else:
    compositeLayerPlain(layer, clipped, backdrop, cx, scope)

proc compositeLayer(layer: Layer; clipped: openArray[Layer]; backdrop: var Buffer; cx: Ctx;
                    ko: Option[Buffer]; scope: Scope) =
  if clipsIndividually(layer, clipped):
    compositeClippedIndividually(layer, clipped, backdrop, cx, ko, scope)
    return
  let w = channelWeights(layer, cx.mode)
  let blendIf = blendIfActive(layer, cx.mode)
  var before: Buffer
  if w.isSome or blendIf: before = backdrop
  if ko.isSome:
    knockout(layer, backdrop, ko.get, false, cx):
      compositeLayerAny(layer, clipped, backdrop, cx, scope)
  else:
    compositeLayerAny(layer, clipped, backdrop, cx, scope)
  if w.isSome: restoreChannels(backdrop, before, w.get)
  if blendIf: applyBlendIf(layer, before, backdrop, cx)

proc compositeAtopAny(layer: Layer; base: var Buffer; cx: Ctx) =
  let rc = base.rect
  if layer.content.kind == lkAdjustment:
    var adjusted = base
    applyDepth(layer.content.adjustment, adjusted, cx.transfer, cx.depth)
    let mv = maskVals(layer, rc, cx)
    for i in 0 ..< base.px.len:
      let k = layer.opacity * layer.fillOpacity * maskK(mv, i)
      let p = base.px[i]
      let a = adjusted.px[i]
      let bl = blendRgbPs(layer.blend, [p[0], p[1], p[2]], [a[0], a[1], a[2]])
      for c in 0 .. 2: base.px[i][c] = p[c] + (bl[c] - p[c]) * k
    return
  if hasEffects(layer):
    let hide = hidingMask(layer, rc, cx)
    proc run(src: Layer; base: var Buffer; hide: Option[seq[float32]]; cx: Ctx) =
      var content: Buffer
      var vstroke: Option[VectorStroke]
      let parts = splitParts(src, rc, cx)
      if parts.isSome:
        content = parts.get[0]
        vstroke = some(VectorStroke(stroke: parts.get[1], mask: parts.get[2]))
      else:
        let c = renderContent(src, rc, cx)
        if c.isNone: return
        content = c.get
      var opaque = Buffer(rect: rc, px: newSeq[Px](base.px.len))
      for i, p in base.px: opaque.px[i] = [p[0], p[1], p[2], 1'f32]
      let maps = effectMaps(src, rc, cx)
      let pb = paintBounds(src)
      compositeWithEffects(src, content, opaque, maps[], (if pb.isSome: pb.get else: layerBounds(src, cx.canvas)),
                           cx.patterns, vstroke)
      if hide.isSome:
        var plain = Buffer(rect: rc, px: newSeq[Px](base.px.len))
        for i, p in base.px: plain.px[i] = [p[0], p[1], p[2], 1'f32]
        applyHidingMask(opaque, plain, hide.get)
      for i in 0 ..< base.px.len:
        if base.px[i][3] > 0:
          let o = opaque.px[i]
          base.px[i] = [o[0], o[1], o[2], base.px[i][3]]
    if hidesEffects(layer): run(stripHidingMasks(layer), base, hide, cx)
    else: run(layer, base, hide, cx)
    return
  let c = renderContent(layer, rc, cx)
  if c.isNone: return
  let opacity = layer.opacity * layer.fillOpacity
  let gamma = textGamma(layer)
  for i in 0 ..< base.px.len:
    let alpha = base.px[i][3]
    if alpha <= 0: continue
    let p = base.px[i]
    let r = compositeGamma(layer.blend, [p[0], p[1], p[2], 1'f32], c.get.px[i], opacity, gamma)
    base.px[i] = [r[0], r[1], r[2], alpha]

proc compositeAtop(layer: Layer; base: var Buffer; cx: Ctx) =
  let w = channelWeights(layer, cx.mode)
  let blendIf = blendIfActive(layer, cx.mode)
  var before: Buffer
  if w.isSome or blendIf: before = base
  compositeAtopAny(layer, base, cx)
  if w.isSome: restoreChannels(base, before, w.get)
  if blendIf: applyBlendIf(layer, before, base, cx)

proc compositeStackAt(layers: openArray[Layer]; backdrop: var Buffer; cx: Ctx; scope: Scope) =
  let floor = (if not scope.root and needsFloor(layers): some(backdrop) else: none(Buffer))
  var i = 0
  while i < layers.len:
    var j = i + 1
    while j < layers.len and layers[j].clipped and not layers[i].clipped: inc j
    if layers[i].visible:
      let ko = knockoutTarget(layers[i], floor, backdrop.rect, cx, scope)
      compositeLayer(layers[i], layers.toOpenArray(i + 1, j - 1), backdrop, cx, ko, scope)
      if layers[i].content.kind == lkAdjustment:
        let q = adjustmentQuantum(cx.depth)
        if q > 0: quantize(backdrop, q)
    i = max(j, i + 1)

# ---------------------------------------------------------------- rendering

proc renderTiledWith(doc: Document; rc: Rect; tile: int; cx: Ctx): Buffer =
  let tile = max(tile, 1)
  let lab = doc.mode == cmLab
  let hdr = doc.depth == stF32
  let cmyk = cmykSpace(doc)
  if rc.isEmpty: return transparentBuffer(rc)
  cx.setFxClip(rc)
  if rc.width <= tile and rc.height <= tile:
    withCmykSpace(cmyk):
      result = backdropFor(doc, rc)
      withBlendSpace(lab, hdr):
        compositeStackAt(doc.layers, result, cx, ScopeRoot)
    return
  result = transparentBuffer(rc)
  let w = rc.width
  var y = rc.y0
  while y < rc.y1:
    let y1 = min(satAdd(y, tile), rc.y1)
    var x = rc.x0
    while x < rc.x1:
      let x1 = min(satAdd(x, tile), rc.x1)
      let t = rect(x, y, x1, y1)
      var part: Buffer
      withCmykSpace(cmyk):
        part = backdropFor(doc, t)
        withBlendSpace(lab, hdr):
          compositeStackAt(doc.layers, part, cx, ScopeRoot)
      let pw = t.width
      for row in 0 ..< t.height:
        let o = (t.y0 - rc.y0 + row) * w + (t.x0 - rc.x0)
        for k in 0 ..< pw: result.px[o + k] = part.px[row * pw + k]
      x = x1
    y = y1

proc renderTiled*(doc: Document; rc: Rect; tile: int): Buffer =
  ## Composites `rc` of the document in `tile`-sized pieces.
  renderTiledWith(doc, rc, tile, ctxForDoc(doc))

proc render*(doc: Document; rc: Rect): Buffer =
  ## Composites the document over `rc` (straight-alpha float RGBA).
  renderTiled(doc, rc, RenderTile)

proc bandRowsFor*(width: int; requested: int): int =
  ## Rows per band: `requested`, or about `BandPixels` pixels, rounded up to whole tiles.
  let want = (if requested > 0: int64(requested) else: BandPixels div int64(max(width, 1)))
  let tiles = (want + RenderTile - 1) div RenderTile
  int(clamp(tiles, 1'i64, 1'i64 shl 16) * RenderTile)

proc renderBands*(doc: Document; rc: Rect; bandRows: int; sink: proc(b: Buffer)) =
  ## Composites `rc` top to bottom in horizontal bands, handing each to `sink` (memory stays
  ## bounded by one band). Exceptions raised by `sink` stop the render and propagate.
  if rc.isEmpty: return
  let cx = ctxForDoc(doc)
  let rows = bandRowsFor(rc.width, bandRows)
  var y = rc.y0
  while y < rc.y1:
    let y1 = min(satAdd(y, rows), rc.y1)
    sink(renderTiledWith(doc, rect(rc.x0, y, rc.x1, y1), RenderTile, cx))
    y = y1

proc flattenToSurface*(doc: Document; fmt: PixelFormat; background = none(array[3, float32])): Surface =
  ## The composite as a surface in `fmt`, optionally over an opaque background colour.
  var s = newSurface(fmt)
  let n = fmt.channels
  var data: seq[float32]
  renderBands(doc, doc.bounds, 0, proc(band0: Buffer) =
    let band = (if background.isSome: band0.overBackground(background.get) else: band0)
    data.setLen(band.px.len * n)
    for i, p in band.px:
      discard fromRgbaInto(fmt, p, data.toOpenArray(i * n, i * n + n - 1))
    s.writeRegion(band.rect, data))
  s.prune()
  s

proc flatten*(doc: Document): Buffer =
  ## The whole canvas composited.
  render(doc, doc.bounds)

proc renderLayer*(layer: Layer; rc: Rect): Buffer =
  ## One layer composited alone over transparency (sRGB, 32-bit maths, no patterns).
  result = transparentBuffer(rc)
  let cx = Ctx(canvas: rc, transfer: srgbTransfer(), light: defaultGlobalLight(),
               patterns: newPreparedPatterns(@[]), mode: cmRgb, depth: stF32)
  compositeStackAt([layer], result, cx, ScopeRoot)

proc layerShape*(doc: Document; layer: Layer; rc: Rect): seq[float32] =
  ## The coverage layer styles are computed from (shape, masks, Transparency Shapes Layer).
  effectShape(layer, rc, ctxForDoc(doc))

proc strokeFrame*(doc: Document; layer: Layer; st: StrokeFx): Option[Rect] =
  ## The frame a gradient Stroke effect is laid out in.
  if st.paint.kind != fpGradient or not hasEffects(layer): return none(Rect)
  let cx = ctxForDoc(doc)
  effectMaps(layer, doc.bounds, cx)[].strokeFrame(st)

# ---------------------------------------------------------------- proxies and reduced renders

proc downsample*(s: Surface; k: int): Surface =
  ## Nearest-neighbour shrink by an integer factor (the top-left pixel of each k×k cell).
  result = surfaceWithDefault(s.format, s.defaultPixel)
  let b = s.contentBounds
  if b.isEmpty or k <= 1:
    if k <= 1: return s
    return
  let k = min(k, 1 shl 20)
  let bpp = s.format.bytesPerPixel
  let (x0, x1) = (divEuclid(b.x0, k), divEuclid(b.x1, k) + int(remEuclid(b.x1, k) != 0))
  let (y0, y1) = (divEuclid(b.y0, k), divEuclid(b.y1, k) + int(remEuclid(b.y1, k) != 0))
  let w = max(x1 - x0, 0)
  var row = newSeq[byte](w * bpp)
  for oy in y0 ..< y1:
    let src = s.toInterleaved(rect(x0 * k, oy * k, x1 * k, oy * k + 1))
    for i in 0 ..< w:
      for j in 0 ..< bpp: row[i * bpp + j] = src[i * k * bpp + j]
    result.writeInterleaved(rect(x0, oy, x1, oy + 1), row)

proc shrinkLayer(l: var Layer; k: int) =
  if l.mask.isSome:
    var m = l.mask.get
    m.surface = downsample(m.surface, k)
    l.mask = some(m)
  if l.fillCache.isSome:
    var fc = l.fillCache.get
    fc.surface = downsample(fc.surface, k)
    l.fillCache = some(fc)
  case l.content.kind
  of lkRaster: l.content.raster = downsample(l.content.raster, k)
  of lkGroup:
    if l.content.group.artboard.isSome:
      var ab = l.content.group.artboard.get
      proc edge(v, k: int): int = divEuclid(v, k) + int(remEuclid(v, k) != 0)
      ab.rect = rect(edge(ab.rect.x0, k), edge(ab.rect.y0, k), edge(ab.rect.x1, k), edge(ab.rect.y1, k))
      l.content.group.artboard = some(ab)
    for c in l.content.group.children.mitems: shrinkLayer(c, k)
  of lkText:
    if l.content.text.cache.isSome: l.content.text.cache = some(downsample(l.content.text.cache.get, k))
  of lkShape:
    if l.content.shape.cache.isSome: l.content.shape.cache = some(downsample(l.content.shape.cache.get, k))
  of lkSmart:
    if l.content.smart.cache.isSome: l.content.smart.cache = some(downsample(l.content.smart.cache.get, k))
  of lkAdjustment, lkFill: discard

proc proxyDocument*(doc: Document; k: int): Document =
  ## A copy of the document shrunk by `k` (for fast thumbnails of huge documents).
  result = doc
  if k <= 1: return
  result.size = size((doc.size.width + k - 1) div k, (doc.size.height + k - 1) div k)
  for l in result.layers.mitems: shrinkLayer(l, k)
  for c in result.channels.mitems: c.surface = downsample(c.surface, k)
  if result.selection.isSome: result.selection = some(downsample(result.selection.get, k))

proc proxyFaithful*(doc: Document): bool =
  ## Whether a proxy renders like the original (no effects or vector masks to rescale).
  for l in doc.allLayers:
    if hasEffects(l) or hasEnabledVectorMask(l): return false
  true

proc renderReducedInBands(doc: Document; b: Rect; w0, h0: int; damage: Option[Rect]; bandRows: int): Buffer =
  let (fw, fh) = (b.width, b.height)
  let (w, h) = (clamp(w0, 1, max(b.width, 1)), clamp(h0, 1, max(b.height, 1)))
  let full = rectXywh(0, 0, w, h)
  if fw == 0 or fh == 0:
    return transparentBuffer(if damage.isSome: EmptyRect else: full)
  proc span(t, f, n: int): (int, int) = (t * f div n, min(max((t + 1) * f div n, t * f div n + 1), f))
  proc cell(x, f, n: int): int = max((x + 1) * n - 1, 0) div f
  var outR = full
  if damage.isSome:
    let d = damage.get.intersect(b)
    if d.isEmpty: return transparentBuffer(EmptyRect)
    let (x0, x1) = (d.x0 - b.x0, d.x1 - b.x0)
    let (y0, y1) = (d.y0 - b.y0, d.y1 - b.y0)
    outR = rect(cell(x0, fw, w), cell(y0, fh, h), cell(x1 - 1, fw, w) + 1, cell(y1 - 1, fh, h) + 1)
  let (ox0, oy0, ow, oh) = (outR.x0, outR.y0, outR.width, outR.height)
  let (sx0, sx1) = (span(ox0, fw, w)[0], span(ox0 + ow - 1, fw, w)[1])
  let (sy0, sy1) = (span(oy0, fh, h)[0], span(oy0 + oh - 1, fh, h)[1])
  let src = rect(b.x0 + sx0, b.y0 + sy0, b.x0 + sx1, b.y0 + sy1)
  if w == fw and h == fh:
    result = render(doc, src)
    result.rect = outR
    return
  proc mapOf(f, n: int): seq[int] =
    result = newSeq[int](f)
    for t in 0 ..< n:
      let (a, z) = span(t, f, n)
      for i in a ..< z: result[i] = t
  let cols = mapOf(fw, w)
  let rows = mapOf(fh, h)
  var acc = newSeq[Px](ow * oh)
  renderBands(doc, src, bandRows, proc(band: Buffer) =
    let (by0, by1) = (band.rect.y0 - b.y0, band.rect.y1 - b.y0)
    let (t0, t1) = (rows[by0], rows[by1 - 1] + 1)
    let sw = sx1 - sx0
    for t in t0 ..< t1:
      let (a, z) = span(t, fh, h)
      let orow = (t - oy0) * ow
      for y in max(a, by0) ..< min(z, by1):
        let base = (y - by0) * sw
        for i in 0 ..< sw:
          let p = band.px[base + i]
          let o = orow + cols[sx0 + i] - ox0
          for c in 0 .. 2: acc[o][c] += p[c] * p[3]
          acc[o][3] += p[3])
  for i in 0 ..< acc.len:
    let ((x0, x1), (y0, y1)) = (span(ox0 + i mod ow, fw, w), span(oy0 + i div ow, fh, h))
    let n = float32(max((y1 - y0) * (x1 - x0), 1))
    let a = acc[i][3]
    acc[i] = (if a > 0: [acc[i][0] / a, acc[i][1] / a, acc[i][2] / a, a / n] else: [0'f32, 0, 0, 0])
  Buffer(rect: outR, px: acc)

proc renderReduced*(doc: Document; w, h: int): Buffer =
  ## The whole canvas box-filtered down to `w × h`.
  renderReducedInBands(doc, doc.bounds, w, h, none(Rect), 0)

proc renderReducedRect*(doc: Document; rc: Rect; w, h: int): Buffer =
  renderReducedInBands(doc, rc, w, h, none(Rect), 0)

proc renderReducedDamage*(doc: Document; w, h: int; damage: Rect): Buffer =
  ## Only the cells of a `w × h` reduction that `damage` (canvas pixels) touches.
  renderReducedInBands(doc, doc.bounds, w, h, some(damage), 0)

proc thumbnailBuffer*(doc: Document; maxSide: int): Buffer =
  let b = doc.bounds
  let longest = max(max(b.width, b.height), 1)
  let scale = min(float32(maxSide) / float32(longest), 1'f32)
  let w = max(int(round(float32(b.width) * scale)), 1)
  let h = max(int(round(float32(b.height) * scale)), 1)
  let k = longest div (max(maxSide, 1) * 2)
  if k >= 2 and doc.size.area > ProxyThumbnailPixels and proxyFaithful(doc):
    renderReduced(proxyDocument(doc, k), w, h)
  else:
    renderReduced(doc, w, h)

proc thumbnail*(doc: Document; maxSide: int): Rgba8Image =
  ## A thumbnail whose longest side is at most `maxSide`.
  thumbnailBuffer(doc, maxSide).toRgba8
