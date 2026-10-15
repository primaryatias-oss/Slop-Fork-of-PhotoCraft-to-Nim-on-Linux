## Filter ▸ Stylize (part 2): Diffuse, Extrude, Tiles, Trace Contour and Wind (port of
## `stylize2.rs`, `stylize2/extrude.rs` and `stylize2/wind.rs`). Extrude's blocks and Wind's
## streaks have a swept (scatter) implementation that is bit-identical to the per-pixel search
## and is used whenever its scratch buffers stay bounded.

import std/[math, options]
import ../geom, ./params, ./fxutil, ./noise
import ../rmath

proc asI32(v: float32): int {.inline.} =
  ## Rust's saturating `f32 as i32` (NaN → 0).
  if v.isNaN: 0
  elif v >= 2147483647'f32: int(high(int32))
  elif v <= -2147483648'f32: int(low(int32))
  else: int(v)

proc signum(v: float32): float32 {.inline.} =
  if v.isNaN: v else: copySign(1'f32, v)

proc readPx(src: Image; x, y: int; buf: var openArray[float32]) {.inline.} =
  for c in 0 ..< buf.len: buf[c] = src.get(x, y, c)

proc fits32(v: int): bool {.inline.} = v >= int(low(int32)) and v <= int(high(int32))

# ---------------------------------------------------------------- Diffuse

const Ring = [(1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1), (0, -1), (1, -1)]

proc diffuse*(src: Image; o: Rect; ctx: Ctx; mode: DiffuseMode; seed: uint32): seq[float32] =
  let n = src.ch
  result = src.crop(o)
  if n == 0 or n > MaxC: return
  var nb: array[MaxC, float32]
  for i in 0 ..< result.len div n:
    let (x, y) = xy(o, i)
    let b = i * n
    case mode
    of dmAnisotropic:
      let me = luma(ctx, result.toOpenArray(b, b + n - 1))
      var best = (F32Max, (0, 0))
      let start = int(hash01(x, y, 3, seed) * 8)
      for k in 0 ..< 8:
        let (dx, dy) = Ring[(start + k) mod 8]
        readPx(src, x + dx, y + dy, nb.toOpenArray(0, n - 1))
        let d = abs(luma(ctx, nb.toOpenArray(0, n - 1)) - me)
        if d < best[0]: best = (d, (dx, dy))
      let (dx, dy) = best[1]
      readPx(src, x + dx, y + dy, nb.toOpenArray(0, n - 1))
      for c in 0 ..< n: result[b + c] = 0.5'f32 * (result[b + c] + nb[c])
    else:
      let dx = int(hash01(x, y, 1, seed) * 3) - 1
      let dy = int(hash01(x, y, 2, seed) * 3) - 1
      readPx(src, x + dx, y + dy, nb.toOpenArray(0, n - 1))
      if ctx.alpha and nb[n - 1] <= 0 and result[b + n - 1] > 0 and mode != dmNormal: continue
      let take = (case mode
        of dmDarkenOnly: luma(ctx, nb.toOpenArray(0, n - 1)) < luma(ctx, result.toOpenArray(b, b + n - 1))
        of dmLightenOnly: luma(ctx, nb.toOpenArray(0, n - 1)) > luma(ctx, result.toOpenArray(b, b + n - 1))
        else: true)
      if take:
        for c in 0 ..< n: result[b + c] = nb[c]

# ---------------------------------------------------------------- Extrude

type ExtrudeSpec* = object
  kind*: ExtrudeType
  size*, depth*: float32
  levelBased*, solidFront*, maskIncomplete*: bool
  seed*: uint32

proc extrudeReach*(depth: float32): float32 =
  ## How far (pixels) a block's front face moves from its cell.
  clamp(depth, 0'f32, 255'f32) * 0.25'f32

proc interval(p, m, o, half: float32): Option[(float32, float32)] {.inline.} =
  if abs(o) < 1e-6'f32:
    return (if abs(p - m) <= half: some((0'f32, 1'f32)) else: none((float32, float32)))
  let (a, b) = ((p - m - half) / o, (p - m + half) / o)
  let (lo, hi) = (max(min(a, b), 0'f32), min(max(a, b), 1'f32))
  if lo <= hi: some((lo, hi)) else: none((float32, float32))

type CellInfo = tuple[col: array[MaxC, float32], h: float32]

proc cellInfo(src: Image; ctx: Ctx; spec: ExtrudeSpec; b: Rect; cs, gx, gy: int): CellInfo =
  let n = src.ch
  let r = rect(b.x0 + gx * cs, b.y0 + gy * cs, b.x0 + (gx + 1) * cs, b.y0 + (gy + 1) * cs).intersect(b)
  var acc: array[MaxC, float32]
  var cnt = 0'f32
  let step = max(cs div 8, 1)
  var yy = r.y0
  while yy < r.y1:
    var xx = r.x0
    while xx < r.x1:
      let a = (if ctx.alpha: src.get(xx, yy, n - 1) else: 1'f32)
      for c in 0 ..< n:
        let s = src.get(xx, yy, c)
        acc[c] += (if ctx.alpha and c < n - 1: s * a else: s)
      cnt += 1
      xx += step
    yy += step
  if cnt > 0:
    for c in 0 ..< n: acc[c] /= cnt
  unpremulPx(acc.toOpenArray(0, n - 1), ctx.alpha)
  let h = (if spec.levelBased: clamp(luma(ctx, acc.toOpenArray(0, n - 1)), 0'f32, 1'f32)
           else: hash01(gx, gy, 21, spec.seed))
  (acc, h)

proc shadePx(ctx: Ctx; n: int; px: var openArray[float32]; col: openArray[float32]; k: float32) =
  let cc = ncol(ctx, n)
  let sub = subtractive(ctx)
  for c in 0 ..< cc:
    px[c] = clamp((if sub: 1 - (1 - col[c]) * k else: col[c] * k), 0'f32, 1'f32)
  if ctx.alpha: px[n - 1] = col[n - 1]

proc extrudeDirect*(src: Image; o: Rect; ctx: Ctx; spec: ExtrudeSpec): seq[float32] =
  ## The per-pixel search (reference implementation of Extrude).
  let n = src.ch
  result = src.crop(o)
  if n == 0 or n > MaxC: return
  let cs = int(round(clamp(spec.size, 2'f32, 255'f32)))
  let b = ctx.bounds
  let (bcx, bcy) = (float32(b.x0 + b.x1) / 2, float32(b.y0 + b.y1) / 2)
  let halfDiag = max(hypot(float32(b.width), float32(b.height)) / 2, 1'f32)
  let reach = extrudeReach(spec.depth)
  template cellRect(gx, gy: int): Rect = rect(b.x0 + gx * cs, b.y0 + gy * cs, b.x0 + (gx + 1) * cs, b.y0 + (gy + 1) * cs)
  template incomplete(r: Rect): bool = r.intersect(b) != r
  var cache = newSeqOfCap[((int, int), CellInfo)](64)
  proc info(cache: var seq[((int, int), CellInfo)]; gx, gy: int): CellInfo =
    for e in cache:
      if e[0] == (gx, gy): return e[1]
    result = cellInfo(src, ctx, spec, b, cs, gx, gy)
    if cache.len >= 64: cache.delete(0)
    cache.add(((gx, gy), result))
  let w = o.width
  for i in 0 ..< result.len div n:
    let (x, y) = (o.x0 + i mod w, o.y0 + i div w)
    if not b.contains(x, y): continue
    if i mod w == 0: cache.setLen(0)
    let base = i * n
    let (fx, fy) = (float32(x) + 0.5'f32, float32(y) + 0.5'f32)
    let (gx, gy) = (divEuclid(x - b.x0, cs), divEuclid(y - b.y0, cs))
    case spec.kind
    of etPyramids:
      let r = cellRect(gx, gy)
      if spec.maskIncomplete and incomplete(r): continue
      let (col, h) = info(cache, gx, gy)
      let (u, v) = (fx - (float32(r.x0) + float32(cs) / 2), fy - (float32(r.y0) + float32(cs) / 2))
      let hgt = h * clamp(spec.depth, 1'f32, 255'f32) / 255 * float32(cs)
      let s = float32(cs) / 2
      let (nx, ny) = (if abs(u) >= abs(v): (signum(u) * hgt, 0'f32) else: (0'f32, signum(v) * hgt))
      let norm = sqrt(nx * nx + ny * ny + s * s)
      let (lx, ly, lz) = (-0.5'f32, -0.5'f32, 0.70710677'f32)
      let lambert = max((nx * lx + ny * ly + s * lz) / norm, 0'f32)
      shadePx(ctx, n, result.toOpenArray(base, base + n - 1), col.toOpenArray(0, n - 1), 0.35'f32 + 0.9'f32 * lambert)
    of etBlocks:
      let k = int(ceil(reach / float32(cs))) + 1
      var found = false
      var best: (float32, int, int, int)
      for dy in -k .. k:
        for dx in -k .. k:
          let (cx, cy) = (gx + dx, gy + dy)
          let r = cellRect(cx, cy)
          if r.x1 <= b.x0 or r.y1 <= b.y0 or r.x0 >= b.x1 or r.y0 >= b.y1: continue
          if spec.maskIncomplete and incomplete(r): continue
          let h = info(cache, cx, cy).h
          if found and best[0] >= h: continue
          let (mx, my) = (float32(r.x0) + float32(cs) / 2, float32(r.y0) + float32(cs) / 2)
          let off = reach * h / halfDiag
          let (ox, oy) = ((mx - bcx) * off, (my - bcy) * off)
          let half = float32(cs) / 2
          let ixo = interval(fx, mx, ox, half)
          let iyo = interval(fy, my, oy, half)
          if ixo.isNone or iyo.isNone: continue
          let (ix, iy) = (ixo.get, iyo.get)
          let (lo, hi) = (max(ix[0], iy[0]), min(ix[1], iy[1]))
          if lo > hi: continue
          let face = (if hi >= 1: 0
                      elif ix[1] < iy[1]: (if ox > 0: 1 else: 2)
                      elif oy > 0: 3
                      else: 4)
          best = (h, cx, cy, face)
          found = true
      if not found: continue
      let (h, cx, cy, face) = best
      let r = cellRect(cx, cy)
      let (mx, my) = (float32(r.x0) + float32(cs) / 2, float32(r.y0) + float32(cs) / 2)
      let off = reach * h / halfDiag
      let col = info(cache, cx, cy).col
      if face == 0:
        if spec.solidFront:
          shadePx(ctx, n, result.toOpenArray(base, base + n - 1), col.toOpenArray(0, n - 1), 1)
        else:
          let sx = clamp(asI32(floor(fx - (mx - bcx) * off)), r.x0, r.x1 - 1)
          let sy = clamp(asI32(floor(fy - (my - bcy) * off)), r.y0, r.y1 - 1)
          for c in 0 ..< n: result[base + c] = src.get(sx, sy, c)
      else:
        let kk = (case face
          of 1: 0.85'f32
          of 2: 0.55'f32
          of 3: 0.95'f32
          else: 0.45'f32)
        shadePx(ctx, n, result.toOpenArray(base, base + n - 1), col.toOpenArray(0, n - 1), kk)

const
  ExtrudeMaxPixels = 4_194_304
  ExtrudeMaxCells = 262_144'i64
  ExtrudeMaxCoord = 1 shl 22

proc extrudeBlocks*(src: Image; o: Rect; ctx: Ctx; spec: ExtrudeSpec): Option[seq[float32]] =
  ## Swept Blocks: each cell paints the pixels its front face and sides cover; none when the
  ## geometry is unsupported or the scratch would be too large.
  if spec.kind != etBlocks or src.ch == 0 or src.ch > MaxC or spec.size.classify in {fcNan, fcInf, fcNegInf} or
      spec.depth.classify in {fcNan, fcInf, fcNegInf}:
    return none(seq[float32])
  let b = ctx.bounds
  for v in [b.x0, b.y0, b.x1, b.y1, o.x0, o.y0, o.x1, o.y1]:
    if abs(v) > ExtrudeMaxCoord: return none(seq[float32])
  let n = src.ch
  let w = o.width
  let count = w * o.height
  if count > ExtrudeMaxPixels: return none(seq[float32])
  var heights = newSeq[float32](count)
  for v in heights.mitems: v = NegInf
  var res = src.crop(o)
  let area = o.intersect(b)
  if area.isEmpty: return some(res)
  let cs = int(round(clamp(spec.size, 2'f32, 255'f32)))
  let reach = extrudeReach(spec.depth)
  let k = int(ceil(reach / float32(cs))) + 1
  let (gx0, gy0) = (divEuclid(area.x0 - b.x0, cs), divEuclid(area.y0 - b.y0, cs))
  let (gx1, gy1) = (divEuclid(area.x1 - 1 - b.x0, cs), divEuclid(area.y1 - 1 - b.y0, cs))
  let (cx0, cy0) = (max(gx0 - k, 0), max(gy0 - k, 0))
  let (cx1, cy1) = (min(gx1 + k, (b.width - 1) div cs), min(gy1 + k, (b.height - 1) div cs))
  if int64(cx1 - cx0 + 1) * int64(cy1 - cy0 + 1) > ExtrudeMaxCells: return none(seq[float32])
  let (bcx, bcy) = (float32(b.x0 + b.x1) / 2, float32(b.y0 + b.y1) / 2)
  let halfDiag = max(hypot(float32(b.width), float32(b.height)) / 2, 1'f32)
  let half = float32(cs) / 2
  let cc = ncol(ctx, n)
  let sub = subtractive(ctx)
  for cy in cy0 .. cy1:
    for cx in cx0 .. cx1:
      let r = rect(b.x0 + cx * cs, b.y0 + cy * cs, b.x0 + (cx + 1) * cs, b.y0 + (cy + 1) * cs)
      let clipped = r.intersect(b)
      if spec.maskIncomplete and clipped != r: continue
      var col: array[MaxC, float32]
      var cnt = 0'f32
      let step = max(cs div 8, 1)
      var yy = clipped.y0
      while yy < clipped.y1:
        var xx = clipped.x0
        while xx < clipped.x1:
          let a = (if ctx.alpha: src.get(xx, yy, n - 1) else: 1'f32)
          for c in 0 ..< n:
            let s = src.get(xx, yy, c)
            col[c] += (if ctx.alpha and c < n - 1: s * a else: s)
          cnt += 1
          xx += step
        yy += step
      if cnt > 0:
        for c in 0 ..< n: col[c] /= cnt
      unpremulPx(col.toOpenArray(0, n - 1), ctx.alpha)
      let h = (if spec.levelBased: clamp(luma(ctx, col.toOpenArray(0, n - 1)), 0'f32, 1'f32)
               else: hash01(cx, cy, 21, spec.seed))
      if h.classify in {fcNan, fcInf, fcNegInf}: return none(seq[float32])
      let (mx, my) = (float32(r.x0) + half, float32(r.y0) + half)
      let off = reach * h / halfDiag
      let (ox, oy) = ((mx - bcx) * off, (my - bcy) * off)
      let sweep = rect(asI32(floor(min(mx, mx + ox) - half)) - 2, asI32(floor(min(my, my + oy) - half)) - 2,
                       asI32(ceil(max(mx, mx + ox) + half)) + 2, asI32(ceil(max(my, my + oy) + half)) + 2).intersect(area)
      for y in sweep.y0 ..< sweep.y1:
        let gy = divEuclid(y - b.y0, cs)
        if abs(cy - gy) > k: continue
        let fy = float32(y) + 0.5'f32
        let iyo = interval(fy, my, oy, half)
        if iyo.isNone: continue
        let iy = iyo.get
        for x in sweep.x0 ..< sweep.x1:
          if abs(cx - divEuclid(x - b.x0, cs)) > k: continue
          let i = (y - o.y0) * w + (x - o.x0)
          if i < 0 or i >= heights.len: return none(seq[float32])
          if heights[i] >= h: continue
          let fx = float32(x) + 0.5'f32
          let ixo = interval(fx, mx, ox, half)
          if ixo.isNone: continue
          let ix = ixo.get
          let (lo, hi) = (max(ix[0], iy[0]), min(ix[1], iy[1]))
          if lo > hi: continue
          heights[i] = h
          let face = (if hi >= 1: 0
                      elif ix[1] < iy[1]: (if ox > 0: 1 else: 2)
                      elif oy > 0: 3
                      else: 4)
          let pb = i * n
          if pb + n > res.len: return none(seq[float32])
          if face == 0 and not spec.solidFront:
            let sx = asI32(floor(fx - (mx - bcx) * off))
            let sy = asI32(floor(fy - (my - bcy) * off))
            readPx(src, clamp(sx, r.x0, r.x1 - 1), clamp(sy, r.y0, r.y1 - 1), res.toOpenArray(pb, pb + n - 1))
          else:
            let shade = (case face
              of 0: 1'f32
              of 1: 0.85'f32
              of 2: 0.55'f32
              of 3: 0.95'f32
              else: 0.45'f32)
            for c in 0 ..< cc:
              res[pb + c] = clamp((if sub: 1 - (1 - col[c]) * shade else: col[c] * shade), 0'f32, 1'f32)
            if ctx.alpha: res[pb + n - 1] = col[n - 1]
  some(res)

proc extrude*(src: Image; o: Rect; ctx: Ctx; spec: ExtrudeSpec): seq[float32] =
  let fast = extrudeBlocks(src, o, ctx, spec)
  if fast.isSome: fast.get else: extrudeDirect(src, o, ctx, spec)

# ---------------------------------------------------------------- Tiles

proc tiles*(src: Image; o: Rect; ctx: Ctx; count: uint32; maxOffset: float32; fill: TileFill;
            colours: (array[4, float32], array[4, float32]); seed: uint32): seq[float32] =
  let n = src.ch
  let b = ctx.bounds
  let ts = max(float32(min(b.width, b.height)) / float32(clamp(count, 1'u32, 99'u32)), 1'f32)
  let m = clamp(maxOffset, 0'f32, 99'f32) / 100 * ts
  let fg = native(ctx, colours[0])
  let bg = native(ctx, colours[1])
  let cc = ncol(ctx, n)
  result = src.crop(o)
  if n == 0 or n > MaxC: return
  let w = o.width
  template offset(gx, gy: int): (float32, float32) =
    ((hash01(gx, gy, 31, seed) * 2 - 1) * m, (hash01(gx, gy, 32, seed) * 2 - 1) * m)
  for i in 0 ..< result.len div n:
    let (x, y) = (o.x0 + i mod w, o.y0 + i div w)
    if not b.contains(x, y): continue
    let base = i * n
    let (fx, fy) = (float32(x) + 0.5'f32 - float32(b.x0), float32(y) + 0.5'f32 - float32(b.y0))
    let (gx, gy) = (asI32(floor(fx / ts)), asI32(floor(fy / ts)))
    var hit = false
    var hitOrder = (0, 0)
    var hox, hoy: float32
    for dy in -1 .. 1:
      for dx in -1 .. 1:
        let (cx, cy) = (gx + dx, gy + dy)
        let (ox, oy) = offset(cx, cy)
        let (u, v) = (fx - ox - float32(cx) * ts, fy - oy - float32(cy) * ts)
        if u >= 0 and v >= 0 and u < ts and v < ts and float32(cx) * ts < float32(b.width) and
            float32(cy) * ts < float32(b.height) and cx >= 0 and cy >= 0:
          let order = (cy, cx)
          if not hit or order > hitOrder:
            hit = true
            hitOrder = order
            (hox, hoy) = (ox, oy)
    if hit:
      let sx = clamp(asI32(floor(float32(x) + 0.5'f32 - hox)), b.x0, b.x1 - 1)
      let sy = clamp(asI32(floor(float32(y) + 0.5'f32 - hoy)), b.y0, b.y1 - 1)
      for c in 0 ..< n: result[base + c] = src.get(sx, sy, c)
    else:
      case fill
      of tfBackground:
        for c in 0 ..< n: result[base + c] = bg[c]
      of tfForeground:
        for c in 0 ..< n: result[base + c] = fg[c]
      of tfInverse:
        for c in 0 ..< cc: result[base + c] = 1 - result[base + c]
      of tfUnaltered: discard

# ---------------------------------------------------------------- Trace Contour

proc traceContour*(src: Image; o: Rect; ctx: Ctx; level: float32; upper: bool): seq[float32] =
  let n = src.ch
  let cc = ncol(ctx, n)
  let sub = subtractive(ctx)
  let bounds = ctx.bounds.intersect(src.rect)
  let area = (if bounds.isEmpty: src.rect else: bounds)
  let t = clamp(level, 0'f32, 255'f32) / 255
  template bright(xx, yy, c: int): float32 =
    (let v = src.getEdge(xx, yy, c, edRepeat, area); if sub: 1 - v else: v)
  result = src.crop(o)
  if n == 0: return
  for i in 0 ..< result.len div n:
    let (x, y) = xy(o, i)
    for c in 0 ..< cc:
      let me = bright(x, y, c) > t
      var line = false
      if me == upper:
        for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
          if (bright(x + dx, y + dy, c) > t) != me:
            line = true
            break
      let paper = (if line: 0'f32 else: 1'f32)
      result[i * n + c] = (if sub: 1 - paper else: paper)

# ---------------------------------------------------------------- Wind

proc windReach*(m: WindMethod): float32 =
  case m
  of wmWind: 16
  of wmBlast: 40
  of wmStagger: 12

proc windDirect*(src: Image; o: Rect; ctx: Ctx; meth: WindMethod; fromRight: bool; seed: uint32): seq[float32] =
  ## The per-pixel search (reference implementation of Wind).
  let n = src.ch
  let lmax = int(windReach(meth))
  let d = (if fromRight: -1 else: 1)
  result = src.crop(o)
  if n == 0 or n > MaxC: return
  var tmp, tmp2: array[MaxC, float32]
  template lum(xx, yy: int; buf: var array[MaxC, float32]): float32 =
    readPx(src, xx, yy, buf.toOpenArray(0, n - 1))
    luma(ctx, buf.toOpenArray(0, n - 1))
  let w = o.width
  for i in 0 ..< result.len div n:
    let (x, y) = (o.x0 + i mod w, o.y0 + i div w)
    let base = i * n
    if meth == wmStagger:
      let segLen = 8 + int(hash01(0, y, 41, seed) * 32)
      let phase = int(hash01(1, y, 42, seed) * float32(segLen))
      let seg = divEuclid(x * d + phase, segLen)
      let hs = hash01(seg, y, 43, seed)
      let shift = int(hs * hs * float32(lmax))
      readPx(src, x - d * shift, y, tmp.toOpenArray(0, n - 1))
      for c in 0 ..< n: result[base + c] = tmp[c]
      continue
    let strength = (if meth == wmBlast: 8'f32 else: 4'f32)
    var best = (0'f32, x)
    for k in 1 .. lmax:
      let s = x - d * k
      let hs = hash01(s, y, 44, seed)
      let len = max(hs * hs * float32(lmax), 1'f32)
      if float32(k) > len: continue
      let l1 = lum(s, y, tmp)
      let l2 = lum(s - d, y, tmp2)
      let edge = min(abs(l1 - l2) * strength, 1'f32)
      let wgt = edge * (1 - float32(k) / (len + 1))
      if wgt > best[0]: best = (wgt, s)
    if best[0] > 0:
      readPx(src, best[1], y, tmp.toOpenArray(0, n - 1))
      for c in 0 ..< n: result[base + c] += (tmp[c] - result[base + c]) * best[0]

const WindMaxRow = 1_048_576

proc windStreaks*(src: Image; o: Rect; ctx: Ctx; meth: WindMethod; fromRight: bool; seed: uint32): Option[seq[float32]] =
  ## Wind/Blast by scattering each source edge's streak; none for Stagger or oversized rows.
  if meth == wmStagger or src.ch == 0 or src.ch > MaxC: return none(seq[float32])
  let n = src.ch
  let w = o.width
  if w == 0 or w > WindMaxRow or o.isEmpty: return none(seq[float32])
  let lmax = int(windReach(meth))
  let d = (if fromRight: -1 else: 1)
  let start = (if fromRight: o.x1 + (lmax - 1) else: o.x0 - lmax)
  if not fits32(start) or not fits32(start - d): return none(seq[float32])
  let sources = w + lmax
  var best = newSeq[(float32, int)](w)
  var res = src.crop(o)
  var tmp: array[MaxC, float32]
  let strength = (if meth == wmBlast: 8'f32 else: 4'f32)
  for y in o.y0 ..< o.y1:
    for e in best.mitems: e = (0'f32, lmax + 1)
    readPx(src, start - d, y, tmp.toOpenArray(0, n - 1))
    var prev = luma(ctx, tmp.toOpenArray(0, n - 1))
    for step in 0 ..< sources:
      let s = start + d * step
      if not fits32(s): return none(seq[float32])
      readPx(src, s, y, tmp.toOpenArray(0, n - 1))
      let lum = luma(ctx, tmp.toOpenArray(0, n - 1))
      let edge = min(abs(lum - prev) * strength, 1'f32)
      prev = lum
      if edge <= 0: continue
      let hs = hash01(s, y, 44, seed)
      let len = max(hs * hs * float32(lmax), 1'f32)
      for k in 1 .. min(asI32(floor(len)), lmax):
        let x = s + d * k
        if not fits32(x): return none(seq[float32])
        if x < o.x0 or x >= o.x1: continue
        let weight = edge * (1 - float32(k) / (len + 1))
        let hi = x - o.x0
        if weight > best[hi][0] or (weight == best[hi][0] and k < best[hi][1]):
          best[hi] = (weight, k)
    let row = (y - o.y0) * w * n
    for x in 0 ..< w:
      let (weight, k) = best[x]
      if weight > 0:
        let sx = o.x0 + x - d * k
        if not fits32(sx): return none(seq[float32])
        readPx(src, sx, y, tmp.toOpenArray(0, n - 1))
        let pb = row + x * n
        for c in 0 ..< n: res[pb + c] += (tmp[c] - res[pb + c]) * weight
  some(res)

proc wind*(src: Image; o: Rect; ctx: Ctx; meth: WindMethod; fromRight: bool; seed: uint32): seq[float32] =
  let fast = windStreaks(src, o, ctx, meth, fromRight, seed)
  if fast.isSome: fast.get else: windDirect(src, o, ctx, meth, fromRight, seed)
