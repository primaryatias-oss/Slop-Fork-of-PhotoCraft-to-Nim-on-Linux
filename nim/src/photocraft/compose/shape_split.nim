## Splits a stroked shape layer into its fill and stroke parts (each rendered alone) so clipped
## layers and layer styles can treat them separately, the way Photoshop does. When the layer
## carries a cached render (e.g. read from a PSD), the stroke part is refitted so fill + stroke
## reproduces the cache exactly. Results are cached (32 entries, least recently used evicted).

import std/[tables, hashes, options]
import ../doc, ../vector

type Entry = object
  parts: (Surface, Surface)
  tick: uint64
  pin: Option[Surface] ## keeps the cache tiles alive so their identity stays unique

const Capacity = 32

var splitCache {.threadvar.}: Table[Hash, Entry]
var splitTick {.threadvar.}: uint64

proc surfaceIdentity(s: Surface): Hash =
  result = hash(ord(s.format.mode)) !& hash(ord(s.format.sample)) !& hash(s.format.alpha)
  for v in s.defaultPixel: result = result !& hash(cast[uint32](v))
  for (c, t) in s.sortedTiles:
    result = result !& hash(c.tx) !& hash(c.ty) !& hash(cast[int](t))

proc fitStroke(fill: Surface; stroke: var Surface; cache: Surface; canvas: Rect) =
  let (cb, sb) = (cache.contentBounds, stroke.contentBounds)
  let r = (if cb.isEmpty: sb elif sb.isEmpty: cb else: cb.union(sb)).intersect(canvas)
  if r.isEmpty: return
  let n = r.width * r.height
  var f = newSeq[Rgba](n)
  var s = newSeq[Rgba](n)
  var c = newSeq[Rgba](n)
  fill.readRgbaInto(r, f)
  stroke.readRgbaInto(r, s)
  cache.readRgbaInto(r, c)
  var outp = newSeqOfCap[float32](n * 4)
  let isFloat = stroke.format.sample == stF32
  for i in 0 ..< n:
    let (fp, sp, cp) = (f[i], s[i], c[i])
    var p = sp
    if fp[3] < 1 - 1e-3:
      let a = clamp((cp[3] - fp[3]) / (1 - fp[3]), 0'f32, 1'f32)
      if sp[3] <= 0:
        p = [cp[0], cp[1], cp[2], a]
      elif a > 1e-3:
        let k = fp[3] * (1 - a)
        for j in 0 .. 2:
          let v = (cp[3] * cp[j] - k * fp[j]) / a
          p[j] = (if isFloat: v else: clamp(v, 0'f32, 1'f32))
        p[3] = a
      else:
        p = [sp[0], sp[1], sp[2], a]
    for v in p: outp.add v
  stroke.writeRegion(r, outp)

proc splitShape*(sh: ShapeLayer; canvas: Rect; depth: SampleType): Option[(Surface, Surface)] =
  ## `(fill only, stroke only)` renders of a stroked shape over `canvas`; none without a stroke.
  if sh.stroke.isNone: return none((Surface, Surface))
  var h: Hash = hash((canvas.x0, canvas.y0, canvas.x1, canvas.y1)) !& hash(ord(depth))
  h = h !& hash($toJ(sh.path)) !& hash($toJ(sh.fill)) !& hash($toJ(sh.stroke)) !& hash($toJ(sh.live))
  if sh.cache.isSome: h = h !& surfaceIdentity(sh.cache.get)
  let key = !$h
  inc splitTick
  splitCache.withValue(key, e):
    e.tick = splitTick
    return some(e.parts)
  let fmt = Rgba8.withSample(depth)
  var fillOnly = sh
  fillOnly.stroke = none(ShapeStroke)
  fillOnly.cache = none(Surface)
  var strokeOnly = sh
  strokeOnly.fill = none(Fill)
  strokeOnly.cache = none(Surface)
  var parts = (renderShape(fillOnly, fmt, canvas), renderShape(strokeOnly, fmt, canvas))
  if sh.cache.isSome: fitStroke(parts[0], parts[1], sh.cache.get, canvas)
  if splitCache.len >= Capacity:
    var oldest: Hash
    var best = high(uint64)
    for k, e in splitCache:
      if e.tick < best: (best, oldest) = (e.tick, k)
    splitCache.del oldest
  splitCache[key] = Entry(parts: parts, tick: splitTick, pin: sh.cache)
  some(parts)

proc purgeShapeSplitCache*() = splitCache.clear()
