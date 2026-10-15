## Running filters (port of the `photocraft_algo` crate root, `lib.rs`): halos, output areas and
## the tiled `apply` with selection mixing.
##
## Every filter is depth-agnostic (normalized `f32` samples in the surface's own colour model),
## tile aware (`halo` declares how far outside an output tile it reads, so results don't depend on
## the tiling) and selection aware (output is mixed with the original by the selection's
## coverage).
##
## Filter modules plug in through the registry below rather than a central `case`: each module
## registers its kernels (and, where needed, a halo, a whole-area fast path or a per-run tile
## plan) when it is imported. `photocraft/algo` imports them all.

import std/[options, math]
import ../geom, ../color, ../raster
import params
export params

type
  KernelProc* = proc(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] {.nimcall.}
    ## A filter's tile kernel: the filtered samples of `o` (interleaved, `src.ch` channels) from
    ## `src` (`o` grown by the halo, or the whole reference area for global filters).
  HaloProc* = proc(p: FilterParams; bounds: Rect): Halo {.nimcall.}
    ## A filter's halo when it depends on the filter module's own helpers.
  FastPathProc* = proc(surface: Surface; p: FilterParams; area, bounds: Rect; selection: Option[Surface];
                       extent: Option[Rect]; tile: int; ctl: Interrupt): Option[Option[Surface]] {.nimcall.}
    ## A whole-area implementation (separable box blur, motion blur): `none` when it doesn't
    ## handle these parameters, else the result (`some(none)` when cancelled).
  TileRunner* = proc(src: Image; t: Rect; ctx: Ctx; ctl: Interrupt): Option[seq[float32]] {.closure.}
  TilePlan* = object
    ## Work prepared once per `apply` (sample tables, rasterized shapes) and run per tile.
    run*: TileRunner
    strict*: bool
      ## A tile the plan can't produce fails the whole run (`apply` returns none); otherwise the
      ## tile falls back to the kernel unless the run was cancelled.
  PlanProc* = proc(p: FilterParams; bounds, area: Rect; ctl: Interrupt): Option[TilePlan] {.nimcall.}

  FilterUnavailable* = object of CatchableError
    ## The filter's module is not part of this build.
  FilterTooLarge* = object of CatchableError
    ## The halo would make a tile's source window absurdly large (hand-written params far outside
    ## the documented ranges); Rust runs out of memory there.

var
  kernels*: array[FilterKind, KernelProc]
  halos*: array[FilterKind, HaloProc]
  planners*: array[FilterKind, PlanProc]
  fastPaths*: seq[FastPathProc]

proc registerKernel*(k: FilterKind; f: KernelProc) = kernels[k] = f
proc registerHalo*(k: FilterKind; f: HaloProc) = halos[k] = f
proc registerPlan*(k: FilterKind; f: PlanProc) = planners[k] = f
proc registerFastPath*(f: FastPathProc) = fastPaths.add f

const
  Tile* = 256
  ResultBudget = 256 shl 20
  MaxWindowSamples = 1 shl 28 ## samples in one tile's source window (1 GiB of f32)

proc ceilI(v: float32): int =
  ## `f32::ceil() as i32` (saturating, NaN → 0).
  let c = ceil(v)
  if c != c: 0
  elif c >= 2147483647'f32: high(int32).int
  elif c <= -2147483648'f32: low(int32).int
  else: int(c)

proc wrapAddI32*(a, b: int): int =
  ## `a + b` in i32 with wrapping, as the Rust release build computes halo radii.
  int(cast[int32](uint32((a + b) and 0xFFFF_FFFF)))

proc fmax0(v: float32): float32 = (if v.isNaN: 0'f32 else: max(v, 0))

proc reachHalo*(v: float32): Halo =
  ## `Halo::Radius(v.max(0.0).ceil() as i32 + 1)`.
  radiusHalo(wrapAddI32(ceilI(fmax0(v)), 1))

proc halo*(p: FilterParams): Halo =
  ## How far outside an output tile the filter reads.
  proc g(sigma: float32): Halo = radiusHalo(wrapAddI32(ceilI(fmax0(sigma) * 3), 1))
  proc r(v: float32): Halo = reachHalo(v)
  case p.kind
  of fltGaussianBlur, fltHighPass, fltUnsharpMask, fltSmartSharpen: g(p.radius)
  of fltBoxBlur, fltSurfaceBlur, fltMedian, fltDustAndScratches, fltMinimum, fltMaximum: r(p.radius)
  of fltMotionBlur: radiusHalo(wrapAddI32(ceilI(abs(p.distance) / 2), 2))
  of fltMosaic: radiusHalo(wrapAddI32(ceilI(if p.cellSize.isNaN: 1'f32 else: max(p.cellSize, 1)), 1))
  of fltEmboss: radiusHalo(wrapAddI32(ceilI(abs(p.height)), 2))
  of fltFindEdges: radiusHalo(1)
  of fltAddNoise, fltSolarize, fltInvert, fltDesaturate: radiusHalo(0)
  of fltRadialBlur, fltOffset, fltTwirl, fltPinch, fltSpherize, fltWave, fltRipple, fltPolarCoordinates: BoundsHalo
  of fltColorHalftone: r(p.maxRadius * 3)
  of fltCrystallize: r((if p.cellSize.isNaN: 1'f32 else: max(p.cellSize, 1)) * 3)
  of fltPointillize: r((if p.cellSize.isNaN: 1'f32 else: max(p.cellSize, 1)) * 2)
  of fltFacet: r(3)
  of fltFragment: r(4)
  of fltMezzotint, fltHsbHsl, fltColorToAlpha, fltNtscColors: radiusHalo(0)
  of fltDiffuse: r(1)
  of fltTiles: BoundsHalo
  of fltTraceContour: r(1)
  of fltDisplace, fltShear, fltZigZag, fltSpinBlur: BoundsHalo
  of fltFibers, fltLensFlare: radiusHalo(0)
  of fltLightingEffects: r(1)
  of fltSmartBlur, fltLensBlur: r(p.radius)
  of fltPathBlur:
    var m = 0'f32
    for path in p.paths: m = max(m, abs(path.speed))
    r(m / 2 + 1)
  of fltCustom: r(2)
  of fltDeInterlace: r(1)
  of fltRelight:
    if p.intensity == 0: radiusHalo(0)
    elif halos[fltRelight] != nil: halos[fltRelight](p, EmptyRect)
    else: radiusHalo(0)
  of fltExtrude, fltOilPaint, fltWind, fltReduceNoise, fltShapeBlur, fltTiltShift, fltIrisBlur, fltFieldBlur,
     fltFilterGallery:
    if halos[p.kind] != nil: halos[p.kind](p, EmptyRect) else: radiusHalo(0)

proc haloFor*(p: FilterParams; bounds: Rect): Halo =
  ## The halo for a run over `bounds` (Relight's softness scales with the image).
  if p.kind == fltRelight and p.intensity != 0 and halos[fltRelight] != nil: halos[fltRelight](p, bounds)
  else: p.halo

proc makesTransparency*(p: FilterParams): bool = p.kind == fltColorToAlpha
proc isGlobal*(p: FilterParams): bool = p.halo == BoundsHalo
proc isAvailable*(k: FilterKind): bool = kernels[k] != nil

proc kernel*(p: FilterParams; src: Image; o: Rect; ctx: Ctx): seq[float32] =
  ## Runs the filter's tile kernel. Raises `FilterUnavailable` for a filter not in this build.
  let k = kernels[p.kind]
  if k == nil: raise newException(FilterUnavailable, p.label & " is not available in this build")
  k(p, src, o, ctx)

proc outputArea*(p: FilterParams; content, bounds: Rect; selectionBounds = none(Rect)): Rect =
  ## The area a filter changes: the content grown by the halo (the whole bounds for global
  ## filters), clipped to the selection.
  let h = p.haloFor(bounds)
  result =
    if h.bounds: bounds
    elif content.isEmpty: EmptyRect
    else: content.inflate(h.radius)
  if selectionBounds.isSome: result = result.intersect(selectionBounds.get)

proc autoTile(p: FilterParams; bounds: Rect): int =
  let h = p.haloFor(bounds)
  if h.bounds: Tile else: min(max(Tile, (2 * h.radius + 63) div 64 * 64), 2048)

proc mixSelection*(data: var seq[float32]; t: Rect; sel: Surface; orig: Image) =
  ## Blends filtered samples with the original by the selection's coverage.
  let w = t.width
  let ch = max(orig.ch, 1)
  var i = 0
  while i * ch + ch <= data.len:
    let (x, y) = (t.x0 + i mod w, t.y0 + i div w)
    let k = clamp(sel.pixel(x, y)[0], 0'f32, 1'f32)
    if k < 1:
      for c in 0 ..< ch:
        let o = orig.get(x, y, c)
        data[i * ch + c] = o + (data[i * ch + c] - o) * k
    inc i

proc applyTiledWith*(surface: Surface; p: FilterParams; area, bounds: Rect; selection: Option[Surface];
                     tile: int; extent: Option[Rect]; ctl: Interrupt; usePlans = true): Option[Surface] =
  ## Filters `area` of `surface` tile by tile; none when cancelled (or a strict plan fails).
  if ctl.cancelled: return none(Surface)
  var o = surface
  if area.isEmpty: return some(o)
  for fp in fastPaths:
    let r = fp(surface, p, area, bounds, selection, extent, tile, ctl)
    if r.isSome: return r.get
  let tile = if p.kind == fltShapeBlur: clamp(tile, 1, 2048) else: max(tile, 1)
  let fmt = surface.format
  let ctx = Ctx(bounds: bounds, mode: fmt.mode, alpha: fmt.alpha)
  var plan = none(TilePlan)
  if usePlans and planners[p.kind] != nil:
    plan = planners[p.kind](p, bounds, area, ctl)
  if ctl.cancelled: return none(Surface)
  let h = p.haloFor(bounds)
  var shared = none(Image)
  if h.bounds: shared = some(readImage(surface, bounds.union(area)))
  var tiles: seq[Rect]
  var y = area.y0
  while y < area.y1:
    var x = area.x0
    while x < area.x1:
      tiles.add rect(x, y, min(x + tile, area.x1), min(y + tile, area.y1))
      x += tile
    y += tile
  let total = max(tiles.len, 1)
  var finished = 0
  let perTile = tile * tile * fmt.channels * sizeof(float32)
  let group = max(ResultBudget div max(perTile, 1), 1)
  var start = 0
  while start < tiles.len:
    if ctl.cancelled: return none(Surface)
    var results: seq[(Rect, seq[float32])]
    for ti in start ..< min(start + group, tiles.len):
      let t = tiles[ti]
      if ctl.cancelled:
        results.add (t, newSeq[float32]())
        continue
      if shared.isNone:
        let win = t.inflate(h.radius)
        if win.width * win.height * fmt.channels > MaxWindowSamples:
          raise newException(FilterTooLarge, p.label & " reads too far around each pixel (" & $h.radius & " px)")
      let src =
        if shared.isSome: shared.get
        elif extent.isSome: readClamped(surface, t.inflate(h.radius), extent.get)
        else: readImage(surface, t.inflate(h.radius))
      var data: seq[float32]
      var ok = true
      if plan.isSome:
        let r = plan.get.run(src, t, ctx, ctl)
        if r.isSome: data = r.get
        elif plan.get.strict: ok = false
        elif ctl.cancelled: ok = false
        else: data = kernel(p, src, t, ctx)
      else:
        data = kernel(p, src, t, ctx)
      if not ok:
        results.add (t, newSeq[float32]())
        continue
      if selection.isSome: mixSelection(data, t, selection.get, src)
      inc finished
      ctl.progress(0.98'f32 * float32(finished) / float32(total))
      results.add (t, data)
    if ctl.cancelled: return none(Surface)
    for (t, data) in results:
      if data.len != t.width * t.height * fmt.channels:
        # A strict plan rejected a tile (Shape Blur geometry or allocation): keep the input.
        if plan.isSome and plan.get.strict: return none(Surface)
    for (t, data) in results: o.writeRegion(t, data)
    start += group
  o.prune()
  ctl.progress(1.0)
  some(o)

proc applyTiled*(surface: Surface; p: FilterParams; area, bounds: Rect; selection = none(Surface); tile: int;
                 extent = none(Rect)): Surface =
  ## As `applyTiledWith` without cancellation; the input comes back unchanged when a strict plan
  ## fails.
  let r = applyTiledWith(surface, p, area, bounds, selection, tile, extent, NoInterrupt)
  if r.isSome: r.get else: surface

proc apply*(surface: Surface; p: FilterParams; area, bounds: Rect; selection = none(Surface)): Surface =
  ## Runs `p` over `area` (see `outputArea`), reading outside `area` up to the halo.
  applyTiled(surface, p, area, bounds, selection, autoTile(p, bounds), none(Rect))

proc applyIn*(surface: Surface; p: FilterParams; area, bounds: Rect; selection: Option[Surface]; extent: Rect): Surface =
  ## As `apply`, reading only inside `extent` (pixels outside it repeat its edge).
  applyTiled(surface, p, area.intersect(extent), bounds, selection, autoTile(p, bounds), some(extent))

proc applyInWith*(surface: Surface; p: FilterParams; area, bounds: Rect; selection: Option[Surface]; extent: Rect;
                  ctl: Interrupt): Option[Surface] =
  ## `applyIn` with cancellation and progress; none when cancelled.
  applyTiledWith(surface, p, area.intersect(extent), bounds, selection, autoTile(p, bounds), some(extent), ctl)
