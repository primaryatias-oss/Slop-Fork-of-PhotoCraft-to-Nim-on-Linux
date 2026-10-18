## Color Replacement (port of `paint/src/replace.rs`): recolour pixels similar to a sampled colour
## using a non-separable blend (Hue, Saturation, Color or Luminosity) with the foreground colour.

import std/[options, math]
import ../geom, ../color, ../raster, ../rmath, ../serde
import ./brush, ./retouch

type
  ReplaceMode* = enum
    rpHue = "hue", rpSaturation = "saturation", rpColor = "color", rpLuminosity = "luminosity"

  Sampling* = enum
    smContinuous = "continuous"         ## resample under the brush centre at every dab
    smOnce = "once"                     ## sample once, where the stroke starts
    smBackgroundSwatch = "backgroundSwatch" ## replace only colours near the background swatch

  Limits* = enum
    lmDiscontiguous = "discontiguous" ## matching pixels anywhere under the brush
    lmContiguous = "contiguous"       ## matching pixels connected to the brush centre
    lmFindEdges = "findEdges"         ## contiguous, and stop at edges (no soft tolerance falloff)

  ReplaceSettings* = object
    mode*: ReplaceMode
    sampling*: Sampling
    limits*: Limits
    tolerance*: float32           ## 0..1 (max per-channel RGB distance)
    antiAlias*: bool
    color*: array[4, float32]     ## replacement (foreground) colour, straight RGBA
    background*: array[4, float32] ## background swatch for `smBackgroundSwatch`

proc defaultReplaceSettings*(): ReplaceSettings =
  ReplaceSettings(mode: rpColor, sampling: smContinuous, limits: lmContiguous, tolerance: 0.3,
                  antiAlias: true, color: [1'f32, 0, 0, 1], background: [1'f32, 1, 1, 1])

proc defaultOf*(_: typedesc[ReplaceSettings]): ReplaceSettings = defaultReplaceSettings()
proc serdeCamel*(_: typedesc[ReplaceSettings]): bool = true

proc dist(a: array[4, float32]; b: array[3, float32]): float32 =
  rmax(rmax(abs(a[0] - b[0]), abs(a[1] - b[1])), abs(a[2] - b[2]))

proc keepConnected*(weight: var seq[float32]; w, h, sx, sy: int) =
  ## Zeroes the weights not 4-connected to `(sx, sy)` through non-zero weights.
  var keep = newSeq[bool](w * h)
  let si = sy * w + sx
  if si >= 0 and si < weight.len and weight[si] > 0:
    var stack = @[si]
    keep[si] = true
    while stack.len > 0:
      let i = stack.pop()
      let (x, y) = (i mod w, i div w)
      template nb(j: int) =
        if not keep[j] and weight[j] > 0:
          keep[j] = true
          stack.add j
      if x > 0: nb(i - 1)
      if x + 1 < w: nb(i + 1)
      if y > 0: nb(i - w)
      if y + 1 < h: nb(i + w)
  for i in 0 ..< min(weight.len, keep.len):
    if not keep[i]: weight[i] = 0

proc colorReplacementEffect*(pre: Surface; rs: ReplaceSettings): DabEffect =
  ## One Color Replacement dab's edit of a working copy, matching against `pre` (the target before
  ## the stroke). Shared by `applyColorReplacement` and live previews that run the dabs as they
  ## come, so both recolour the same pixels.
  let fmt = pre.format
  let n = fmt.channels
  let mode = case rs.mode
    of rpHue: bmHue
    of rpSaturation: bmSaturation
    of rpColor: bmColor
    of rpLuminosity: bmLuminosity
  let fg = [rs.color[0], rs.color[1], rs.color[2]]
  let tol = clamp(rs.tolerance, 0'f32, 1'f32)
  let soft = rs.antiAlias and rs.limits != lmFindEdges
  var once: Option[array[3, float32]]
  proc matchW(d: float32): float32 =
    if d > tol: 0'f32
    elif soft and tol > 0: clamp((tol - d) / (tol * 0.25'f32), 0'f32, 1'f32) # soft over the last quarter
    else: 1'f32
  result = proc(work: var PxRegion; fp: Footprint) =
    let (cx, cy) = (asI32(floor(fp.dab.center.x)), asI32(floor(fp.dab.center.y)))
    let sampled =
      case rs.sampling
      of smContinuous:
        let c = pre.rgba(cx, cy)
        [c[0], c[1], c[2]]
      of smOnce:
        if once.isNone:
          let c = pre.rgba(cx, cy)
          once = some([c[0], c[1], c[2]])
        once.get
      of smBackgroundSwatch: [rs.background[0], rs.background[1], rs.background[2]]
    let r = fp.rect.intersect(work.rect)
    if r.isEmpty: return
    let (w, h) = (r.width, r.height)
    # Per-pixel match weight, against the pre-stroke colours, so pixels recoloured by earlier dabs
    # of this stroke still count as the sampled colour.
    var weight = newSeq[float32](w * h)
    for y in r.y0 ..< r.y1:
      for x in r.x0 ..< r.x1:
        if fp.at(x, y) <= 0: continue
        let c = pre.rgba(x, y)
        if c[3] <= 0: continue
        weight[(y - r.y0) * w + (x - r.x0)] = matchW(dist(c, sampled))
    if rs.limits != lmDiscontiguous:
      # Keep only the region connected to the centre (4-connected flood fill).
      keepConnected(weight, w, h, clamp(cx, r.x0, r.x1 - 1) - r.x0, clamp(cy, r.y0, r.y1 - 1) - r.y0)
    var enc: array[8, float32]
    for y in r.y0 ..< r.y1:
      for x in r.x0 ..< r.x1:
        let k = fp.at(x, y) * weight[(y - r.y0) * w + (x - r.x0)]
        if k <= 0: continue
        let o = work.offset(x, y)
        let c = toRgba(fmt, work.data.toOpenArray(o, o + n - 1))
        let b = blendRgb(mode, [c[0], c[1], c[2]], fg)
        let oc = [c[0] + (b[0] - c[0]) * k, c[1] + (b[1] - c[1]) * k, c[2] + (b[2] - c[2]) * k, c[3]]
        discard fromRgbaInto(fmt, oc, enc)
        for ch in 0 ..< n: work.data[o + ch] = enc[ch]

proc applyColorReplacement*(target: var Surface; stroke: Stroke; rs: ReplaceSettings;
                            selection: Option[Surface]; lockTransparency: bool): Rect =
  ## Replace colours along a stroke. Returns the damaged rectangle.
  let effect = colorReplacementEffect(target, rs)
  applyDabStroke(target, stroke, selection, lockTransparency, 0, effect)
