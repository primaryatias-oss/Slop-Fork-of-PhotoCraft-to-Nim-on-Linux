## Combined layer masks: the pixel mask times the rasterized vector mask, with Mask Properties ›
## Feather applied as a Gaussian (three box blurs), cached per mask state.

import std/[math, hashes, tables]
import ../doc, ../vector

const
  MaskFormat = PixelFormat(mode: cmGrayscale, sample: stU16, alpha: false)
  CacheCapacity = 64

var maskCache {.threadvar.}: Table[Hash, Surface]
var maskOrder {.threadvar.}: seq[Hash]

proc featherSigma*(px: float32): float32 =
  if px == px and abs(px) != Inf: clamp(px, 0'f32, 1000'f32) else: 0'f32

proc hasFeather*(l: Layer): bool =
  (l.mask.isSome and l.mask.get.enabled and featherSigma(l.mask.get.feather) > 0) or
    (l.vectorMask.isSome and l.vectorMask.get.enabled and featherSigma(l.vectorMask.get.feather) > 0)

proc blurLine(line: var openArray[float32]; scratch: var seq[float32]; r: int) =
  let n = line.len
  if n == 0: return
  for i in 0 ..< n: scratch[i] = line[i]
  template get(i: int): float32 = scratch[clamp(i, 0, n - 1)]
  var acc = 0'f32
  for i in -r .. r: acc += get(i)
  let cnt = float32(2 * r + 1)
  for i in 0 ..< n:
    line[i] = acc / cnt
    acc += get(i + r + 1) - get(i - r)

proc gaussian*(v: var seq[float32]; w, h: int; sigma: float32) =
  ## Three box blurs approximating a Gaussian of `sigma` (separable, clamped edges).
  if sigma <= 0 or w == 0 or h == 0 or v.len != w * h: return
  let ideal = sqrt(12 * sigma * sigma / 3 + 1)
  let r = ((int(floor(ideal)) or 1) - 1) div 2
  if r == 0: return
  var scratch = newSeq[float32](max(w, h))
  var line = newSeq[float32](max(w, h))
  for y in 0 ..< h:
    for _ in 0..2: blurLine(v.toOpenArray(y * w, y * w + w - 1), scratch, r)
  for x in 0 ..< w:
    for y in 0 ..< h: line[y] = v[y * w + x]
    for _ in 0..2: blurLine(line.toOpenArray(0, h - 1), scratch, r)
    for y in 0 ..< h: v[y * w + x] = line[y]

proc maskKey(l: Layer; canvas: Rect): Hash =
  var h: Hash = 0
  h = h !& hash(uint64(l.id))
  h = h !& hash((canvas.x0, canvas.y0, canvas.x1, canvas.y1))
  if l.vectorMask.isSome: h = h !& hash($toJ(l.vectorMask.get.path)) !& hash((l.vectorMask.get.enabled, l.vectorMask.get.density, l.vectorMask.get.feather))
  if l.mask.isSome:
    let m = l.mask.get
    h = h !& hash((m.enabled, m.density, m.feather))
    for c, t in m.surface.tiles: h = h !& hash((c.tx, c.ty, cast[uint](t)))
    h = h !& hash(m.surface.defaultPixel)
  !$h

proc buildMask(l: Layer; canvas: Rect): Surface =
  let vm = if l.vectorMask.isSome and l.vectorMask.get.enabled: l.vectorMask else: none(VectorMask)
  let pixel = if l.mask.isSome and l.mask.get.enabled: l.mask else: none(LayerMask)
  let sv = if vm.isSome: featherSigma(vm.get.feather) else: 0'f32
  let sp = if pixel.isSome: featherSigma(pixel.get.feather) else: 0'f32
  let compiled = if vm.isSome: some(compileVectorMask(vm.get)) else: none(CompiledVectorMask)
  let far = rectXywh(canvas.x0 - 1_000_000, canvas.y0 - 1_000_000, 1, 1)
  let vOut = if compiled.isSome: compiled.get.render(far)[0] else: 1'f32
  var pDef = 1'f32
  if pixel.isSome:
    let dp = pixel.get.surface.defaultPixel
    let d = if dp.len > 0: dp[0] else: 1'f32
    pDef = 1 - pixel.get.density * (1 - d)
  proc grow(r: Rect; sigma: float32): Rect =
    let m = int(ceil(sigma * 3)) + 2
    if r.isEmpty: r else: rect(r.x0 - m, r.y0 - m, r.x1 + m, r.y1 + m)
  var area = EmptyRect
  if vm.isSome:
    let cb = vm.get.path.controlBounds
    if cb.isSome:
      let (x0, y0, x1, y1) = cb.get
      area = grow(rect(int(floor(x0)) - 2, int(floor(y0)) - 2, int(ceil(x1)) + 2, int(ceil(y1)) + 2), sv)
  if pixel.isSome and vOut > 0:
    let b = grow(pixel.get.surface.contentBounds, sp)
    area = if area.isEmpty: b elif b.isEmpty: area else: area.union(b)
  area = area.intersect(canvas)
  result = surfaceWithDefault(MaskFormat, [pDef * vOut])
  if not area.isEmpty:
    let (w, h) = (area.width, area.height)
    var v = if compiled.isSome: compiled.get.render(area) else: (var s = newSeq[float32](w * h); for x in s.mitems: x = 1; s)
    gaussian(v, w, h, sv)
    if pixel.isSome:
      var pm: seq[float32]
      pixel.get.valuesInto(area, pm)
      gaussian(pm, w, h, sp)
      for i in 0 ..< v.len: v[i] *= pm[i]
    result.writeRegion(area, v)

proc combinedMask*(l: Layer; canvas: Rect): Option[Surface] =
  ## The layer's feathered/vector mask over the canvas (cached), or `none` when neither applies.
  if not ((l.vectorMask.isSome and l.vectorMask.get.enabled) or hasFeather(l)): return none(Surface)
  let k = maskKey(l, canvas)
  if k in maskCache: return some(maskCache[k])
  let s = buildMask(l, canvas)
  if maskOrder.len >= CacheCapacity:
    maskCache.del maskOrder[0]
    maskOrder.delete(0)
  maskCache[k] = s
  maskOrder.add k
  some(s)

proc purgeMaskCache*(): int =
  result = maskCache.len
  maskCache.clear()
  maskOrder.setLen(0)
