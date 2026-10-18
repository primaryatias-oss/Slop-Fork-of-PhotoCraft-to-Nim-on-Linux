## Dab rasterisation and stroke compositing (port of `paint/src/render.rs`).
##
## A stroke paints into a sparse, tiled *stroke buffer* (coverage plus, with Color Dynamics, a
## premultiplied colour), exactly like a private stroke layer: dabs build up with flow up to their
## opacity ceiling, stroke-level masks (Dual Brush, Texture) apply when the buffer is composited,
## and the whole buffer composites onto the pre-stroke pixels at the stroke opacity. Because the
## buffer is sparse and compositing always starts from the pre-stroke pixels, strokes can be fed in
## chunks and re-composited incrementally for interactive painting (see `StrokeRenderer`).

import std/[math, options, tables, sets, algorithm]
import ../geom, ../color, ../raster, ../rmath
import ./brush, ./tile, ./rng, ./dynamics, ./procedural, ./pixels

const
  DitherSalt = 0x0000_6469_7468_6572'u64 ## salt of the 8-bit dither's per-pixel noise
  CovTile* = 64                           ## edge of a stroke-buffer tile
  SmallTipRadius = 3'f32
    ## Computed tips with a smaller radius (in pixels) are rasterised by area, not at pixel centres.
  Subpixel = [-0.375'f32, -0.125'f32, 0.125'f32, 0.375'f32]
    ## Sub-pixel sample offsets (a 4×4 grid) for the area coverage of small tips.
  NoiseSalt = 0x006E_6F69_7365'u64

proc dither8(px: var openArray[float32]; x, y: int) {.inline.} =
  ## Before a stroke's result is quantised to 8 bits, offset every channel of the pixel by the
  ## same deterministic noise of under half a level, so fractional values round up or down in
  ## proportion (a soft, low-flow stroke's fine gradient survives as grain instead of bands).
  let d = (hash2(x, y, DitherSalt) - 0.5'f32) / 255'f32
  for v in px.mitems: v += d

proc smoothstep(e0, e1, x: float32): float32 =
  let t = clamp((x - e0) / (e1 - e0), 0'f32, 1'f32)
  t * t * (3'f32 - 2'f32 * t)

proc wetFactor(rn: float32): float32 {.inline.} =
  ## Wet Edges: how much paint stays at a pixel whose nearest dab reaches it at `rn` (0 at a dab's
  ## centre, 1 at its rim). The interior keeps half the paint, the rim all of it.
  0.5'f32 + 0.5'f32 * smoothstep(0.5'f32, 1'f32, rn)

proc maskCombine*(mode: MaskMode; v, t, d: float32): float32 =
  ## Combine brush coverage `v` with a mask value `t` (texture/dual tip) at `depth`. Every mode
  ## maps `v = 0` to 0 (a mask never adds paint where the brush has none).
  if v <= 0: return 0
  template lerp(a, b: float32): float32 = a + (b - a) * d
  let r =
    case mode
    of mmMultiply: v * (1'f32 - d + d * t)
    of mmSubtract: rmax(v - d * (1'f32 - t), 0'f32)
    of mmDarken: rmin(v, 1'f32 - d + d * t)
    of mmOverlay:
      let o = if v < 0.5'f32: 2'f32 * v * t else: 1'f32 - 2'f32 * (1'f32 - v) * (1'f32 - t)
      lerp(v, o)
    of mmColorDodge: lerp(v, rmin(v / rmax(1'f32 - t, 1e-3'f32), 1'f32))
    of mmColorBurn: lerp(v, 1'f32 - rmin((1'f32 - v) / rmax(t, 1e-3'f32), 1'f32))
    of mmLinearBurn: lerp(v, rmax(v + t - 1'f32, 0'f32))
    of mmHardMix: lerp(v, (if v + t > 1: 1'f32 else: 0'f32))
    of mmLinearHeight: v * clamp((t - (1'f32 - d * v)) * 4'f32 + 0.5'f32, 0'f32, 1'f32)
    of mmHeight: (if t >= 1'f32 - d * v: v else: 0'f32)
  clamp(r, 0'f32, 1'f32)

proc isFin(v: float32): bool {.inline.} = v == v and v != Inf and v != NegInf

# ---------------------------------------------------------------- brush context

type
  BrushContext* = object
    ## A stroke-ready brush: the settings plus prepared tip mips and texture (shared between
    ## copies, like the Rust crate's `Arc`s).
    brush*: BrushSettings
    tip: ref Mips
    dualTip: ref Mips
    texture: ref PatternImage

proc preparePattern(b: BrushSettings): ref PatternImage =
  let t = b.texture
  if not t.enabled: return nil
  new(result)
  if t.pattern.kind == pkProcedural:
    result[] = pattern(t.pattern.style, t.pattern.size, t.pattern.seed)
  else:
    let g = t.pattern.bitmap
    if g.isNone or not g.get.isValid: return nil
    result[] = PatternImage(width: int(g.get.width), height: int(g.get.height), data: g.get.toF32)
  let (br, ct) = (clamp(t.brightness, -1'f32, 1'f32), clamp(t.contrast, -1'f32, 1'f32))
  for v in result.data.mitems:
    let x = clamp((v - 0.5'f32) * (1'f32 + ct) + 0.5'f32 + br, 0'f32, 1'f32)
    v = if t.invert: 1'f32 - x else: x

proc rectAround*(d: Dab; reach: float32): Rect =
  ## The pixel rectangle `reach` (plus 1 px of anti-aliasing slack) around a dab's centre. The
  ## casts and the arithmetic saturate, so a far or non-finite centre or reach gives a (possibly
  ## empty) rectangle instead of an overflow.
  let rr = satAdd(asI32(float64(ceil(reach))), 1)
  let (cx, cy) = (asI32(floor(d.center.x)), asI32(floor(d.center.y)))
  rect(satSub(cx, rr), satSub(cy, rr), satAdd(satAdd(cx, rr), 1), satAdd(satAdd(cy, rr), 1))

proc newBrushContext*(brush0: BrushSettings): BrushContext =
  let brush = brush0.boundedForRender
  proc mips(t: TipShape): ref Mips =
    let g = t.bitmap
    if g.isSome and g.get.isValid:
      new(result)
      result[] = newMips(g.get)
  result = BrushContext(tip: mips(brush.tip),
                        dualTip: (if brush.dualBrush.enabled: mips(brush.dualBrush.tip) else: nil),
                        texture: preparePattern(brush), brush: brush)

proc textureAt*(ctx: BrushContext; x, y: int): float32 {.inline.} =
  ## Texture value at a document pixel (bilinear, tiled).
  let p = ctx.texture
  if p.isNil: return 1
  let s = rmax(ctx.brush.texture.scale, 0.01'f32)
  if s == 1:
    # Pixel centres land exactly on texels at 100 %.
    return p.data[remEuclid(y, p.height) * p.width + remEuclid(x, p.width)]
  let (fx, fy) = ((float32(x) + 0.5'f32) / s - 0.5'f32, (float32(y) + 0.5'f32) / s - 0.5'f32)
  let (x0, y0) = (floor(fx), floor(fy))
  let (tx, ty) = (fx - x0, fy - y0)
  let a = p[].sampleWrap(x0, y0) + (p[].sampleWrap(x0 + 1, y0) - p[].sampleWrap(x0, y0)) * tx
  let b = p[].sampleWrap(x0, y0 + 1) + (p[].sampleWrap(x0 + 1, y0 + 1) - p[].sampleWrap(x0, y0 + 1)) * tx
  a + (b - a) * ty

proc dabRect*(ctx: BrushContext; d: Dab; dual: bool): Rect =
  ## Bounding rectangle of a dab (rotated sampled tips need the corner reach).
  let sampled = if dual: not ctx.dualTip.isNil else: not ctx.tip.isNil
  let reach = if sampled: d.radius * float32(sqrt(2'f32)) else: d.radius
  rectAround(d, reach)

proc rasterizeRound(d: Dab; rect: Rect; outp: var seq[float32]; hardness: float32; aliased: bool) =
  ## Ordinary computed tips at angle 0 (the zero-angle transform only changes signs; squaring
  ## gives the same radial distance), kept apart from the feature loop.
  let (cx, cy) = if aliased: gridCenter(d.center.x, d.center.y, 2'f32 * d.radius) else: (float32(d.center.x), float32(d.center.y))
  let ro = rmin(rmax(d.roundness, 0.5'f32 / d.radius), 1'f32)
  let rm = rmax(d.radius * ro, 0.5'f32)
  let w = rect.width
  let reach2 = (rm + 0.5'f32) * (rm + 0.5'f32)
  let inner = rm * clamp(hardness, 0'f32, 1'f32)
  let rows = if w > 0: outp.len div w else: 0
  for yy in 0 ..< rows:
    let dy = float32(rect.y0 + yy) + 0.5'f32 - cy
    let row = yy * w
    if aliased:
      for xx in 0 ..< w:
        let dx = float32(rect.x0 + xx) + 0.5'f32 - cx
        let d2 = (dx * ro) * (dx * ro) + dy * dy
        outp[row + xx] = if d2 < reach2 and sqrt(d2) <= rm: d.alpha else: 0'f32
    elif rm - inner < 1e-3'f32:
      for xx in 0 ..< w:
        let dx = float32(rect.x0 + xx) + 0.5'f32 - cx
        let d2 = (dx * ro) * (dx * ro) + dy * dy
        let edge = rmin(clamp(rm + 0.5'f32 - sqrt(d2), 0'f32, 1'f32), 1'f32)
        outp[row + xx] = if d2 >= reach2 or edge <= 0: 0'f32 else: edge * d.alpha
    else:
      for xx in 0 ..< w:
        let dx = float32(rect.x0 + xx) + 0.5'f32 - cx
        let d2 = (dx * ro) * (dx * ro) + dy * dy
        let dd = sqrt(d2)
        let edge = rmin(clamp(rm + 0.5'f32 - dd, 0'f32, 1'f32), 1'f32)
        let t = clamp((dd - inner) / (rm - inner), 0'f32, 1'f32)
        let s = 1'f32 - t * t
        let s2 = s * s
        let falloff = if dd <= inner: 1'f32 else: s2 * s2
        let val = edge * falloff
        outp[row + xx] = if d2 >= reach2 or val <= 0: 0'f32 else: val * d.alpha

proc rasterizeInto(ctx: BrushContext; d: Dab; dual: bool; rect: Rect; outp: var seq[float32];
                   depth: var seq[float32]; wantDepth: bool; scalarOnly = false) =
  ## `rasterize`; with `wantDepth`, Wet Edges is left out of `outp` and each painted pixel's
  ## depth inside the dab (`1 − rn`) goes to `depth`, for the stroke to apply it. `scalarOnly`
  ## skips the computed-tip fast path (the tests compare the two bit for bit).
  let (w, h) = (rect.width, rect.height)
  outp.setLen(0)
  outp.setLen(w * h)
  if wantDepth:
    depth.setLen(0)
    depth.setLen(w * h)
  template b: untyped = ctx.brush
  let hardness = if dual: b.dualBrush.hardness else: b.hardness
  let mips = if dual: ctx.dualTip else: ctx.tip
  let aliased = b.aliased and not dual
  let wet = b.wetEdges and not dual and not wantDepth
  let noise = b.noise and not dual
  let texTip = not dual and b.texture.enabled and b.texture.eachTip and not ctx.texture.isNil
  let (cx, cy) = if aliased: gridCenter(d.center.x, d.center.y, 2'f32 * d.radius) else: (float32(d.center.x), float32(d.center.y))
  let (sn, cs) = (sin(d.angle), cos(d.angle))
  let (fx, fy) = ((if d.flipX: -1'f32 else: 1'f32), (if d.flipY: -1'f32 else: 1'f32))
  # Brush Projection: stretch the sampling coordinates along the tilt direction, which
  # foreshortens the tip there by `projScale`.
  let proj = not dual and d.projScale < 0.999'f32
  let (pax, pay, pk) =
    if proj: (cos(d.projAngle), sin(d.projAngle), 1'f32 / rmax(d.projScale, 0.05'f32) - 1'f32)
    else: (0'f32, 0'f32, 0'f32)
  let r = d.radius
  let ro = rmin(rmax(d.roundness, 0.5'f32 / r), 1'f32)
  let rm = rmax(r * ro, 0.5'f32)
  let reach2 = (rm + 0.5'f32) * (rm + 0.5'f32)
  # Sampled-tip mapping.
  var (level, invScale, tw, th) = (0, 1'f32, 1'f32, 1'f32)
  if not mips.isNil:
    let l0 = mips.levels[0]
    let big = float32(max(l0.w, l0.h))
    level = mips[].levelFor(2'f32 * r * rmax(ro, 0.25'f32))
    invScale = big / (2'f32 * r)
    tw = float32(l0.w)
    th = float32(l0.h)
  # A pixel offset from the dab centre in tip space: to y-up, project, rotate by -angle, flip.
  template toTip(dx0, dy0: float32): (float32, float32) =
    var ux = dx0
    var uy = -dy0
    if proj:
      let t = (ux * pax + uy * pay) * pk
      ux += t * pax
      uy += t * pay
      ((ux * cs + uy * sn) * fx, (-ux * sn + uy * cs) * fy)
    else:
      ((ux * cs + uy * sn) * fx, (-ux * sn + uy * cs) * fy)
  # A tip a few pixels across takes each pixel's covered area (a 4×4 grid inside it).
  let small = mips.isNil and not aliased and rm < SmallTipRadius
  let smallReach2 = (rm + 0.75'f32) * (rm + 0.75'f32)
  if not scalarOnly and not (not mips.isNil or small or (aliased and b.square) or noise or texTip or wet) and
      not wantDepth and not proj and d.angle == 0 and isFin(d.radius) and isFin(d.roundness) and
      isFin(rm) and isFin(cx) and isFin(cy) and isFin(hardness) and isFin(d.alpha) and w > 0:
    rasterizeRound(d, rect, outp, hardness, aliased)
    return
  for yy in 0 ..< h:
    let y = rect.y0 + yy
    let dy = float32(y) + 0.5'f32 - cy
    for xx in 0 ..< w:
      let x = rect.x0 + xx
      let dx = float32(x) + 0.5'f32 - cx
      let (u, v) = toTip(dx, dy)
      var val, rn: float32
      if aliased and b.square:
        # Block: the whole-pixel square, hard (the Eraser's Block mode).
        if abs(dx) >= r or abs(dy) >= r: continue
        (val, rn) = (1'f32, 0'f32)
      elif mips.isNil and small:
        let d2 = (u * ro) * (u * ro) + v * v
        if d2 >= smallReach2: continue
        var sum = 0'f32
        for sy in Subpixel:
          for sx in Subpixel:
            let (su, sv) = toTip(dx + sx, dy + sy)
            let sd = sqrt((su * ro) * (su * ro) + sv * sv)
            # Each sample anti-aliases over its own quarter pixel, so the covered area changes
            # smoothly with the dab's position.
            let inside = clamp((rm - sd) * 4'f32 + 0.5'f32, 0'f32, 1'f32)
            if inside > 0: sum += inside * tipFalloff(rmin(sd, rm), rm, hardness)
        (val, rn) = (sum / 16'f32, sqrt(d2) / rm)
      elif mips.isNil:
        let d2 = (u * ro) * (u * ro) + v * v
        if d2 >= reach2: continue
        let dd = sqrt(d2)
        val = if aliased: (if dd <= rm: 1'f32 else: 0'f32) else: dabCoverage(dd, rm, hardness)
        rn = dd / rm
      else:
        let (px, py) = (u * invScale + tw / 2'f32, -(v / ro) * invScale + th / 2'f32)
        if px < -1 or py < -1 or px > tw + 1 or py > th + 1: continue
        let s = mips[].sampleClamped(level, px / tw, py / th)
        val = if aliased: (if s >= 0.5'f32: 1'f32 else: 0'f32) else: s
        rn = 1'f32 - s
      if val <= 0: continue
      if noise and val < 1:
        let n = hash2(x, y, NoiseSalt)
        let hard = if n < val: 1'f32 else: 0'f32
        val += (hard - val) * 0.7'f32
      if texTip: val = maskCombine(b.texture.mode, val, ctx.textureAt(x, y), d.depth)
      if wet: val *= wetFactor(rn)
      if wantDepth and yy * w + xx < depth.len: depth[yy * w + xx] = clamp(1'f32 - rn, 0'f32, 1'f32)
      outp[yy * w + xx] = val * d.alpha

proc rasterize*(ctx: BrushContext; d: Dab; dual: bool; rect: Rect; outp: var seq[float32]) =
  ## Rasterise a dab over `rect` into `outp` (tip shape × noise × per-tip texture × wet edges ×
  ## flow). Wet Edges shapes this one dab; a stroke applies it to its accumulated coverage instead.
  var none: seq[float32]
  ctx.rasterizeInto(d, dual, rect, outp, none, false)

proc footprint*(ctx: BrushContext; d: Dab; index: int): Footprint =
  ## One dab's dense footprint (for the sequential retouch tools).
  let rect = ctx.dabRect(d, false)
  var cov: seq[float32]
  ctx.rasterize(d, false, rect, cov)
  Footprint(dab: d, index: index, rect: rect, cov: cov)

proc strokeMaskWet(ctx: BrushContext; c: float32; hasDepth: bool; depth: float32; hasDual: bool;
                   dual: float32; x, y: int): float32 {.inline.} =
  ## Stroke-level masks applied to accumulated coverage `c` at a pixel, with Wet Edges from the
  ## stroke's depth there.
  template b: untyped = ctx.brush
  var m = if hasDepth: c * wetFactor(1'f32 - depth) else: c
  if hasDual: m = maskCombine(b.dualBrush.mode, m, dual, 1)
  if not ctx.texture.isNil and not b.texture.eachTip:
    m = maskCombine(b.texture.mode, m, ctx.textureAt(x, y), clamp(b.texture.depth, 0'f32, 1'f32))
  m

proc strokeMask*(ctx: BrushContext; c: float32; dual: Option[float32]; x, y: int): float32 {.inline.} =
  ## Stroke-level masks applied to accumulated coverage `c` at a pixel.
  ctx.strokeMaskWet(c, false, 0, dual.isSome, (if dual.isSome: dual.get else: 0'f32), x, y)

# ---------------------------------------------------------------- sparse stroke buffer

type
  CovTileData = object
    cov: seq[float32]
    depth: seq[float32] ## Wet Edges: the deepest any dab reaches each pixel (empty without)
    col: seq[float32]   ## premultiplied native colour channels (`nc` per pixel), empty without

  CoverageMap* = object
    ## A sparse tiled coverage (+ optional colour) buffer.
    tiles: Table[(int, int), CovTileData]
    nc: int
    bounds: Rect
    dirty: HashSet[(int, int)]

const CovArea = CovTile * CovTile

proc newCoverageMap*(colorChannels: int): CoverageMap = CoverageMap(nc: colorChannels)
proc bounds*(m: CoverageMap): Rect = m.bounds

proc tileRect(tx, ty: int): Rect {.inline.} = rect(tx * CovTile, ty * CovTile, (tx + 1) * CovTile, (ty + 1) * CovTile)

proc get*(m: CoverageMap; x, y: int): float32 {.inline.} =
  let (tx, ty) = (divEuclid(x, CovTile), divEuclid(y, CovTile))
  m.tiles.withValue((tx, ty), t):
    return t.cov[(y - ty * CovTile) * CovTile + (x - tx * CovTile)]
  0

proc depthAt(m: CoverageMap; x, y: int; depth: var float32): bool {.inline.} =
  ## Wet Edges depth at a pixel; false when the stroke has none.
  let (tx, ty) = (divEuclid(x, CovTile), divEuclid(y, CovTile))
  m.tiles.withValue((tx, ty), t):
    let i = (y - ty * CovTile) * CovTile + (x - tx * CovTile)
    if i < t.depth.len:
      depth = t.depth[i]
      return true
  false

proc accumulate*(m: var CoverageMap; rect: Rect; vals: openArray[float32]; ceil: float32;
                 color: openArray[float32]; hasColor: bool; depth: openArray[float32]; hasDepth: bool) =
  ## Accumulate dab values over `rect`: flow builds up towards the dab's opacity ceiling
  ## (`c ← c + v·(ceil − c)`). `color` = native colour for per-dab colour. `depth` (Wet Edges, one
  ## value per pixel of `rect`) keeps the deepest any dab reaches each pixel.
  if rect.isEmpty: return
  m.bounds = m.bounds.union(rect)
  let w = rect.width
  let nc = m.nc
  let (t0x, t0y) = (divEuclid(rect.x0, CovTile), divEuclid(rect.y0, CovTile))
  let (t1x, t1y) = (divEuclid(rect.x1 - 1, CovTile), divEuclid(rect.y1 - 1, CovTile))
  let wantCol = hasColor and nc > 0
  for ty in t0y .. t1y:
    for tx in t0x .. t1x:
      let tr = tileRect(tx, ty).intersect(rect)
      if tr.isEmpty: continue
      # Skip tiles this dab doesn't touch.
      var any = false
      block scan:
        for y in tr.y0 ..< tr.y1:
          let row = (y - rect.y0) * w
          for x in tr.x0 ..< tr.x1:
            if vals[row + x - rect.x0] > 0:
              any = true
              break scan
      if not any: continue
      let key = (tx, ty)
      if not m.tiles.hasKey(key):
        m.tiles[key] = CovTileData(cov: newSeq[float32](CovArea),
                                   col: (if wantCol: newSeq[float32](CovArea * nc) else: @[]))
      m.dirty.incl key
      m.tiles.withValue(key, tile):
        if wantCol and tile.col.len == 0: tile.col = newSeq[float32](CovArea * nc)
        if hasDepth and tile.depth.len == 0: tile.depth = newSeq[float32](CovArea)
        for y in tr.y0 ..< tr.y1:
          let row = (y - rect.y0) * w
          let trow = (y - ty * CovTile) * CovTile
          for x in tr.x0 ..< tr.x1:
            let v = vals[row + x - rect.x0]
            if v <= 0: continue
            let ti = trow + x - tx * CovTile
            let di = row + x - rect.x0
            if hasDepth and di < depth.len and ti < tile.depth.len:
              tile.depth[ti] = rmax(tile.depth[ti], depth[di])
            let c = tile.cov[ti]
            let nv = if ceil > c: c + v * (ceil - c) else: c
            if nv <= c: continue
            if wantCol:
              let k = if c < 1: (nv - c) / (1'f32 - c) else: 0'f32
              let o = ti * nc
              for i in 0 ..< nc: tile.col[o + i] = color[i] * k + tile.col[o + i] * (1'f32 - k)
            tile.cov[ti] = nv

proc takeDirty(m: var CoverageMap): seq[(int, int)] =
  for k in m.dirty: result.add k
  m.dirty.clear()
  result.sort()

proc unionMax(m: var CoverageMap; other: CoverageMap) =
  ## Union two passes of the same brush stroke without applying opacity twice where they overlap.
  ## The stronger coverage wins, including its per-dab colour when present.
  m.bounds = m.bounds.union(other.bounds)
  for key, source in other.tiles.pairs:
    if not m.tiles.hasKey(key):
      m.tiles[key] = source
      m.dirty.incl key
      continue
    let nc = m.nc
    m.tiles.withValue(key, target):
      if target.depth.len == 0: target.depth = source.depth
      else:
        for i in 0 ..< min(target.depth.len, source.depth.len): target.depth[i] = rmax(target.depth[i], source.depth[i])
      for index in 0 ..< min(target.cov.len, source.cov.len):
        if source.cov[index] > target.cov[index]:
          target.cov[index] = source.cov[index]
          let start = index * nc
          if start + nc <= target.col.len and start + nc <= source.col.len:
            for c in 0 ..< nc: target.col[start + c] = source.col[start + c]
    m.dirty.incl key

# ---------------------------------------------------------------- stroke renderer

type
  StrokeRenderer* = object
    ## Incremental stroke rendering: feed points, composite dirty tiles from the pre-stroke pixels.
    ctx*: BrushContext
    generator: DabGenerator
    cov: CoverageMap
    hasDual: bool
    dual: CoverageMap
    fmt: Option[PixelFormat]
    perDabColor: bool
    dabsDone: int
    scratch: seq[float32]
    depthScratch: seq[float32] ## Wet Edges depth of the dab being accumulated
    dabBuf, dualBuf: seq[Dab]
    recording: bool
    allDabs: seq[Dab]

proc newStrokeRenderer*(brush: BrushSettings; fmt: Option[PixelFormat]; zoom: float32): StrokeRenderer =
  ## `fmt` = target pixel format (needed for per-dab colour); `zoom` for smoothing.
  let perDabColor = brush.colorDynamics.enabled and not brush.erase and fmt.isSome
  let nc = if perDabColor: fmt.get.mode.colorChannels else: 0
  StrokeRenderer(ctx: newBrushContext(brush), generator: newDabGenerator(brush, zoom),
                 cov: newCoverageMap(nc), hasDual: brush.dualBrush.enabled, dual: newCoverageMap(0),
                 fmt: fmt, perDabColor: perDabColor)

proc recordDabs*(r: StrokeRenderer): StrokeRenderer =
  ## Keep a copy of every primary dab (for tests and sequential tools).
  result = r
  result.recording = true

proc dabs*(r: StrokeRenderer): seq[Dab] = r.allDabs
proc dabCount*(r: StrokeRenderer): int = r.dabsDone
proc bounds*(r: StrokeRenderer): Rect = r.cov.bounds ## union of the dab rectangles so far

proc rasterPending(r: var StrokeRenderer) =
  let wet = r.ctx.brush.wetEdges
  var native: array[8, float32]
  for d in r.dabBuf:
    let rect = r.ctx.dabRect(d, false)
    # Wet Edges acts on the whole stroke: the dab records how deep it reaches each pixel and the
    # stroke darkens its own rim when composited, not every dab's.
    r.ctx.rasterizeInto(d, false, rect, r.scratch, r.depthScratch, wet)
    var nc = 0
    let hasCol = r.perDabColor and r.fmt.isSome
    if hasCol:
      discard fromRgbaInto(r.fmt.get, d.color, native)
      nc = r.fmt.get.mode.colorChannels
    r.cov.accumulate(rect, r.scratch, d.opacity, native.toOpenArray(0, max(nc, 1) - 1), hasCol,
                     r.depthScratch, wet)
    # Bounds track the full dab rectangle (even fully transparent parts), like the damage.
    r.cov.bounds = r.cov.bounds.union(rect)
  if r.hasDual:
    for d in r.dualBuf:
      let rect = r.ctx.dabRect(d, true)
      r.ctx.rasterize(d, true, rect, r.scratch)
      r.dual.accumulate(rect, r.scratch, 1, [], false, [], false)
  r.dabsDone += r.dabBuf.len
  if r.recording: r.allDabs.add r.dabBuf
  r.dabBuf.setLen(0)
  r.dualBuf.setLen(0)

proc wantsTime*(r: StrokeRenderer): bool = r.generator.wantsTime

proc smoothingString*(r: StrokeRenderer): Option[(array[2, float64], array[2, float64])] =
  ## The string between the brush and the pointer while smoothing lags.
  r.generator.smoothingString

proc push*(r: var StrokeRenderer; pts: openArray[StrokePoint]) =
  ## Feed input points (any chunking gives the same result).
  r.generator.push(pts, r.dabBuf, r.dualBuf)
  r.rasterPending()

proc finish*(r: var StrokeRenderer) =
  ## End of stroke (smoothing catch-up, a lone first point).
  r.generator.finish(r.dabBuf, r.dualBuf)
  r.rasterPending()

proc tailDabs*(r: StrokeRenderer): seq[Dab] =
  ## The primary dabs finishing the stroke now would add, without finishing it.
  var g = r.generator
  var duals: seq[Dab]
  g.finish(result, duals)

proc tailPreview*(r: StrokeRenderer): Option[StrokeRenderer] =
  ## What finishing the stroke now would add, for live previews: a renderer holding copies of the
  ## coverage tiles the tail touches with the tail rendered in, so its `composite` draws exactly
  ## those tiles as `finish` would leave them. None when finishing adds nothing.
  var g = r.generator
  var dabs, duals: seq[Dab]
  g.finish(dabs, duals)
  if dabs.len == 0 and duals.len == 0: return none(StrokeRenderer)
  var keys = initHashSet[(int, int)]()
  proc addKeys(rr: Rect) =
    if rr.isEmpty: return
    for ty in divEuclid(rr.y0, CovTile) .. divEuclid(rr.y1 - 1, CovTile):
      for tx in divEuclid(rr.x0, CovTile) .. divEuclid(rr.x1 - 1, CovTile): keys.incl (tx, ty)
  for d in dabs: addKeys(r.ctx.dabRect(d, false))
  for d in duals: addKeys(r.ctx.dabRect(d, true))
  proc subset(m: CoverageMap): CoverageMap =
    result = CoverageMap(nc: m.nc, bounds: m.bounds)
    for k in keys:
      m.tiles.withValue(k, t): result.tiles[k] = t
  var t = StrokeRenderer(ctx: r.ctx, generator: g, cov: subset(r.cov), hasDual: r.hasDual,
                         dual: (if r.hasDual: subset(r.dual) else: newCoverageMap(0)), fmt: r.fmt,
                         perDabColor: r.perDabColor, dabsDone: r.dabsDone, dabBuf: dabs, dualBuf: duals)
  t.rasterPending()
  # Tiles whose coverage the tail doesn't change still composite the same: redraw them all.
  for k in t.cov.tiles.keys: t.cov.dirty.incl k
  some(t)

proc markDirty*(r: var StrokeRenderer; rr: Rect) =
  ## Mark the coverage tiles over `rr` for the next `composite` (e.g. to redraw over a preview).
  if rr.isEmpty: return
  for ty in divEuclid(rr.y0, CovTile) .. divEuclid(rr.y1 - 1, CovTile):
    for tx in divEuclid(rr.x0, CovTile) .. divEuclid(rr.x1 - 1, CovTile):
      if r.cov.tiles.hasKey((tx, ty)): r.cov.dirty.incl (tx, ty)

proc takeDirtyRect*(r: var StrokeRenderer): Rect =
  ## The coverage tiles touched since the last call (or `composite`), as one rectangle clipped to
  ## the stroke bounds, and forget them.
  result = EmptyRect
  for (tx, ty) in r.cov.takeDirty(): result = result.union(tileRect(tx, ty))
  r.dual.dirty.clear()
  result = result.intersect(r.bounds)

proc coverageAt*(r: StrokeRenderer; x, y: int): float32 =
  ## Final stroke coverage at a pixel (stroke-level masks applied, before opacity/selection).
  let c = r.cov.get(x, y)
  if c <= 0: return 0
  var dp = 0'f32
  let hasDepth = r.cov.depthAt(x, y, dp)
  r.ctx.strokeMaskWet(c, hasDepth, dp, r.hasDual, (if r.hasDual: r.dual.get(x, y) else: 0'f32), x, y)

proc coverageIn*(r: StrokeRenderer; rr: Rect): seq[float32] =
  ## Final coverage over `rr`, one value per pixel in rows.
  for y in rr.y0 ..< rr.y1:
    for x in rr.x0 ..< rr.x1: result.add r.coverageAt(x, y)

proc denseCoverage*(r: StrokeRenderer): (Rect, seq[float32]) =
  ## Dense final coverage over the stroke bounds.
  let b = r.bounds
  let w = b.width
  var outp = newSeq[float32](w * b.height)
  for key, t in r.cov.tiles.pairs:
    let (tx, ty) = key
    let tr = tileRect(tx, ty).intersect(b)
    for y in tr.y0 ..< tr.y1:
      for x in tr.x0 ..< tr.x1:
        let ti = (y - ty * CovTile) * CovTile + (x - tx * CovTile)
        let c = t.cov[ti]
        if c > 0:
          let hasDepth = ti < t.depth.len
          outp[(y - b.y0) * w + (x - b.x0)] = r.ctx.strokeMaskWet(c, hasDepth, (if hasDepth: t.depth[ti] else: 0'f32),
            r.hasDual, (if r.hasDual: r.dual.get(x, y) else: 0'f32), x, y)
  (b, outp)

proc composite*(r: var StrokeRenderer; pre: Surface; target: var Surface; selection: Option[Surface];
                lockTransparency, all: bool): Rect =
  ## Composite the stroke buffer onto `target`, reading original pixels from `pre` (the
  ## pre-stroke surface; pass a copy of the target taken before the first `push`). With
  ## `all = false` only tiles touched since the last composite are processed (interactive use).
  ## Returns the damaged rectangle.
  var keys: seq[(int, int)]
  if all:
    r.cov.dirty.clear()
    for k in r.cov.tiles.keys: keys.add k
    keys.sort()
  else:
    keys = r.cov.takeDirty()
  r.dual.dirty.clear()
  let bounds = r.bounds
  let fmt = target.format
  let n = fmt.channels
  let aIdx = alphaIndex(fmt)
  let nc = fmt.mode.colorChannels
  template b: untyped = r.ctx.brush
  let opacity = clamp(b.opacity, 0'f32, 1'f32)
  var src: array[8, float32]
  discard fromRgbaInto(fmt, b.color, src)
  if aIdx >= 0: src[aIdx] = b.color[3]
  # 8-bit targets: the pixels the stroke changes are dithered as they are quantised, so soft,
  # low-flow strokes don't band.
  let dither = fmt.sample == stU8
  var dmg = EmptyRect
  var region: seq[float32]
  var enc: array[8, float32]
  for key in keys:
    if not r.cov.tiles.hasKey(key): continue
    let (tx, ty) = key
    let tr = tileRect(tx, ty).intersect(bounds)
    if tr.isEmpty: continue
    pre.readRegionInto(tr, region)
    var selCh = 0
    var selV: seq[float32]
    if selection.isSome:
      selCh = selection.get.channels
      selV = selection.get.readRegion(tr)
    let w = tr.width
    r.cov.tiles.withValue(key, tile):
      for y in tr.y0 ..< tr.y1:
        for x in tr.x0 ..< tr.x1:
          let ti = (y - ty * CovTile) * CovTile + (x - tx * CovTile)
          let c = tile.cov[ti]
          if c <= 0: continue
          let i = (y - tr.y0) * w + (x - tr.x0)
          let hasDepth = ti < tile.depth.len
          let m = r.ctx.strokeMaskWet(c, hasDepth, (if hasDepth: tile.depth[ti] else: 0'f32), r.hasDual,
                                      (if r.hasDual: r.dual.get(x, y) else: 0'f32), x, y)
          let s = if selection.isSome: selV[i * selCh] else: 1'f32
          var k = rmin(m * opacity * s, 1'f32)
          if k <= 0: continue
          if b.mode == bmDissolve:
            k = if hash2(x, y, b.seed) < k: 1'f32 else: 0'f32
            if k == 0: continue
          template px: untyped = region.toOpenArray(i * n, (i + 1) * n - 1)
          if b.erase:
            if aIdx >= 0 and not lockTransparency: region[i * n + aIdx] *= 1'f32 - k
            if dither: dither8(px, x, y)
            continue
          if lockTransparency and aIdx >= 0 and region[i * n + aIdx] <= 0: continue
          if r.perDabColor and tile.col.len > 0:
            for ch in 0 ..< nc: src[ch] = tile.col[ti * nc + ch] / c
          if b.mode in {bmNormal, bmDissolve}:
            overNative(fmt, px, src.toOpenArray(0, n - 1), k, lockTransparency)
          else:
            let d = toRgba(fmt, px)
            let sr = toRgba(fmt, src.toOpenArray(0, n - 1))
            var o = color.composite(b.mode, d, sr, k)
            if lockTransparency: o[3] = d[3]
            discard fromRgbaInto(fmt, o, enc)
            for ch in 0 ..< n: region[i * n + ch] = enc[ch]
          if dither: dither8(px, x, y)
    target.writeRegion(tr, region)
    dmg = dmg.union(tr)
  if all: bounds else: dmg

proc compositeUnionMany*(r: StrokeRenderer; others: openArray[StrokeRenderer]; pre: Surface;
                         target: var Surface; selection: Option[Surface]; lockTransparency: bool): Rect =
  ## Combine all symmetry passes before applying brush opacity and selection once.
  var merged = r
  for other in others:
    merged.cov.unionMax(other.cov)
    if merged.hasDual and other.hasDual: merged.dual.unionMax(other.dual)
  merged.composite(pre, target, selection, lockTransparency, true)

proc compositeUnion*(r: StrokeRenderer; other: StrokeRenderer; pre: Surface; target: var Surface;
                     selection: Option[Surface]; lockTransparency: bool): Rect =
  ## Composite the union of this stroke and a mirrored pass as one stroke, keeping overlapping
  ## dabs on a symmetry axis under one opacity ceiling.
  r.compositeUnionMany([other], pre, target, selection, lockTransparency)

proc renderStroke*(target: var Surface; brush: BrushSettings; points: openArray[StrokePoint];
                   selection: Option[Surface]; lockTransparency: bool; zoom: float32): Rect =
  ## Render a whole stroke onto `target` (one-shot). Returns the damaged rectangle.
  if points.len == 0: return EmptyRect
  let pre = target
  var r = newStrokeRenderer(brush, some(target.format), zoom)
  r.push(points)
  r.finish()
  r.composite(pre, target, selection, lockTransparency, true)

proc applyStroke*(target: var Surface; stroke: Stroke; selection: Option[Surface]; lockTransparency: bool): Rect =
  ## Rasterize a stroke onto `target`, optionally limited by a selection (grayscale coverage
  ## surface). Returns the damaged rectangle.
  renderStroke(target, stroke.brush, stroke.points, selection, lockTransparency, 1)
