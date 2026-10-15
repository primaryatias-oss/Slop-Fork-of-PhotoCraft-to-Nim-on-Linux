## Paint helpers: bucket fill and the gradient tool (port of `algo/paint.rs`).

import std/[math, options]
import ../geom, ../color, ../raster, ../rmath
import ../color/blend
import ./selection

type
  GradientShape* = enum
    ## Gradient tool shape.
    gsLinear = "linear", gsRadial = "radial", gsAngle = "angle", gsReflected = "reflected", gsDiamond = "diamond"

  Stop4* = (float32, array[4, float32])

let Tau32 = float32(TAU)

proc toolGradientT*(shape: GradientShape; frm, to: (float32, float32); x, y: float32): float32 =
  ## Gradient tool parameter `t` for a drag from `frm` to `to`.
  let (dx, dy) = (to[0] - frm[0], to[1] - frm[1])
  let len = rmax(sqrt(dx * dx + dy * dy), 1e-6'f32)
  let (ux, uy) = (dx / len, dy / len)
  let (px, py) = (x - frm[0], y - frm[1])
  let along = px * ux + py * uy
  let across = -px * uy + py * ux
  let t = case shape
    of gsLinear: along / len
    of gsRadial: sqrt(px * px + py * py) / len
    of gsReflected: abs(along) / len
    of gsDiamond: (abs(along) + abs(across)) / len
    of gsAngle: remEuclidF(arctan2(across, along) / Tau32, 1'f32)
  clamp(t, 0'f32, 1'f32)

proc sampleStops*(stops: openArray[Stop4]; t: float32): array[4, float32] =
  ## Samples RGBA stops (sorted by position) at `t`.
  if stops.len == 0: return
  if stops.len == 1: return stops[0][1]
  if t <= stops[0][0]: return stops[0][1]
  for i in 0 ..< stops.len - 1:
    let (a, b) = (stops[i], stops[i + 1])
    if t <= b[0]:
      let k = (if b[0] > a[0]: (t - a[0]) / (b[0] - a[0]) else: 0'f32)
      for c in 0 .. 3: result[c] = a[1][c] + (b[1][c] - a[1][c]) * k
      return
  stops[^1][1]

proc compositeArea(s: var Surface; area: Rect; mode: BlendMode; coverage: proc(x, y: int): float32;
                   color: proc(x, y: int): array[4, float32]; lockTransparency: bool) =
  ## Composites `color(x, y)` over `area` of the surface with per-pixel coverage, tile by tile;
  ## tiles with zero coverage are left untouched (their copy-on-write storage survives).
  let fmt = s.format
  let n = fmt.channels
  var done: seq[(Rect, seq[float32])]
  for tc in area.tiles:
    let r = tc.rect.intersect(area)
    if r.isEmpty: continue
    let w = r.width
    var any = false
    var region: seq[float32]
    var enc: array[8, float32]
    for y in r.y0 ..< r.y1:
      for x in r.x0 ..< r.x1:
        let k = coverage(x, y)
        if not (k > 0): continue
        if not any:
          region = s.readRegion(r)
          any = true
        let i = ((y - r.y0) * w + (x - r.x0)) * n
        let d = toRgba(fmt, region.toOpenArray(i, i + n - 1))
        var o = blend.composite(mode, d, color(x, y), k)
        if lockTransparency: o[3] = d[3]
        discard fromRgbaInto(fmt, o, enc)
        for c in 0 ..< n: region[i + c] = enc[c]
    if any: done.add (r, region)
  for (r, region) in done: s.writeRegion(r, region)

proc selCov(selection: Option[Surface]; x, y: int): float32 {.inline.} =
  if selection.isNone: 1'f32 else: selection.get.sampleChannel(x, y, 0)

proc paintGradient*(s: var Surface; area: Rect; frm, to: (float32, float32); shape: GradientShape;
                    stops: seq[Stop4]; reverse: bool; opacity: float32; mode: BlendMode; dither: bool;
                    selection: Option[Surface]; lockTransparency: bool) =
  ## Gradient tool over `area` (the selection bounds or canvas).
  compositeArea(s, area, mode,
    proc(x, y: int): float32 = opacity * selCov(selection, x, y),
    proc(x, y: int): array[4, float32] =
      let t = toolGradientT(shape, frm, to, float32(x) + 0.5'f32, float32(y) + 0.5'f32)
      var c = sampleStops(stops, (if reverse: 1'f32 - t else: t))
      if dither:
        # One quantisation step of monochromatic noise breaks 8-bit banding without speckle.
        let nz = (ditherNoise(x, y) - 0.5'f32) / 255'f32
        for ch in 0 .. 2: c[ch] = clamp(c[ch] + nz, 0'f32, 1'f32)
      c,
    lockTransparency)

proc bucketFillRegionSrc*(s: var Surface; region: Region; opacity: float32; selection: Option[Surface];
                          lockTransparency: bool; src: proc(x, y: int): array[4, float32]) =
  ## Paints a bucket region sampled before the edit, so its boundary can come from the visible
  ## composite while only `s` receives the colour or pattern. Selection coverage still clips it.
  compositeArea(s, region.bbox, bmNormal,
    proc(x, y: int): float32 = region.at(x, y) * opacity * selCov(selection, x, y), src, lockTransparency)

proc bucketFillSrc*(s: var Surface; area: Rect; seed: (int, int); tolerance: float32; contiguous, antiAlias: bool;
                    opacity: float32; selection: Option[Surface]; lockTransparency: bool;
                    src: proc(x, y: int): array[4, float32]): bool =
  ## Like `bucketFill` but the fill colour comes from `src(x, y)` (e.g. a pattern sampled at the
  ## canvas position).
  if not area.contains(seed[0], seed[1]): return false
  let region = block:
    let img = rgba8Image(s, area)
    wandRegion(img, area, seed, tolerance, contiguous, antiAlias)
  if region.isNone: return false
  bucketFillRegionSrc(s, region.get, opacity, selection, lockTransparency, src)
  true

proc bucketFill*(s: var Surface; area: Rect; seed: (int, int); tolerance: float32; contiguous, antiAlias: bool;
                 color: array[4, float32]; opacity: float32; selection: Option[Surface]; lockTransparency: bool): bool =
  ## Paint bucket: fills the region similar to the seed pixel (on the layer's own pixels) within
  ## `area`, respecting the selection.
  bucketFillSrc(s, area, seed, tolerance, contiguous, antiAlias, opacity, selection, lockTransparency,
                proc(x, y: int): array[4, float32] = color)
