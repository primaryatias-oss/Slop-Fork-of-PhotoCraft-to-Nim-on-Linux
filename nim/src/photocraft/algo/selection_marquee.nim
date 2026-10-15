## Marquee shapes built straight into tiles, and the tile-wise combination of two selections
## (port of `selection/marquee.rs`).
##
## Both produce exactly the bytes the per-pixel versions did (an ellipse written with
## `writePixel`, a combination read and written through whole-canvas `f32` buffers), but work a
## tile at a time, keep only the tiles that hold something (as `prune` would leave them) and share
## one tile for every fully selected one.

import std/[math, options, tables]
import ../geom, ../color, ../raster

const TS = TileSize

proc encode8(v: float32): uint8 =
  ## The byte a GRAY8 surface stores for `v`.
  var b = [0'u8]
  writeSample(b, stU8, 0, v)
  b[0]

proc decode8(b: uint8): float32 =
  ## The value a GRAY8 surface reads for `b`.
  readSample([b], stU8, 0)

type
  Ellipse = object
    ## An ellipse inscribed in a rectangle, tested in `f32` exactly as the Elliptical Marquee
    ## always has (so its anti-aliased edge keeps the same bytes).
    cx, cy, rx, ry: float32
  RowSpan = object
    ## One row of the ellipse: nothing outside `lo..hi`, fully selected in `fullLo..fullHi`, and
    ## the edge pixels in between need the exact coverage test.
    lo, fullLo, fullHi, hi: int

proc newEllipse(r: Rect): Ellipse =
  Ellipse(cx: float32(r.x0 + r.x1) / 2'f32, cy: float32(r.y0 + r.y1) / 2'f32,
          rx: float32(r.width) / 2'f32, ry: float32(r.height) / 2'f32)

proc inside(e: Ellipse; x, y: float32): bool {.inline.} =
  let (dx, dy) = ((x - e.cx) / e.rx, (y - e.cy) / e.ry)
  dx * dx + dy * dy <= 1'f32

proc full(e: Ellipse; x, y: int; aa: bool): bool {.inline.} =
  ## Whether every point of pixel `(x, y)` that the coverage samples is inside: its four corners
  ## (anti-aliased) or its centre.
  let (fx, fy) = (float32(x), float32(y))
  if not aa: return e.inside(fx + 0.5'f32, fy + 0.5'f32)
  e.inside(fx, fy) and e.inside(fx + 1, fy) and e.inside(fx, fy + 1) and e.inside(fx + 1, fy + 1)

proc coverage(e: Ellipse; x, y: int; aa: bool): int {.inline.} =
  ## Coverage of pixel `(x, y)` in sixteenths: 16 when `full`, else (anti-aliased) the number of
  ## its 4×4 sub-samples inside.
  if e.full(x, y, aa): return 16
  if not aa: return 0
  let (fx, fy) = (float32(x), float32(y))
  for i in 0 ..< 16:
    if e.inside(fx + (float32(i mod 4) + 0.5'f32) / 4'f32, fy + (float32(i div 4) + 0.5'f32) / 4'f32): inc result

proc rowSpan(e: Ellipse; y: int; cut: Rect; aa: bool): RowSpan =
  ## Bounds row `y` of `e` within `cut.x0..cut.x1` from the exact (f64) ellipse, widened by a
  ## margin that covers the f32 tests' rounding, then checked with the exact tests; any doubt
  ## evaluates the whole row.
  let exact = RowSpan(lo: cut.x0, fullLo: cut.x1, fullHi: cut.x1, hi: cut.x1)
  let (cx, cy, rx, ry) = (float64(e.cx), float64(e.cy), float64(e.rx), float64(e.ry))
  # Every point the tests sample in this row lies in `y..y + 1`.
  let (ya, yb) = (float64(y), float64(y) + 1.0)
  let near = (if cy < ya: ya - cy else: max(cy - yb, 0.0))
  let far = max(abs(ya - cy), abs(yb - cy))
  proc half(d: float64): float64 =
    let t = 1.0 - (d / ry) * (d / ry)
    if t >= 0: rx * sqrt(t) else: -1.0
  let hn = half(near)
  let outer = (if hn != hn: 0.0 else: max(hn, 0.0))
  let inner = half(far)
  # Near the top and bottom a rounding error δ in the test moves the edge by up to rx·√δ.
  let m = 2.0 + rx * 1e-3
  proc fin(v: float64): bool = v.classify notin {fcNan, fcInf, fcNegInf}
  if not (fin(outer) and fin(inner) and fin(m)): return exact
  proc px(v: float64): int = int(clamp(v, float64(cut.x0), float64(cut.x1)))
  let lo = px(floor(cx - outer - m))
  let hi = px(floor(cx + outer + m) + 1.0)
  var (fullLo, fullHi) = (px(ceil(cx - inner + m)), px(floor(cx + inner - m)))
  if inner < 0 or fullLo >= fullHi: (fullLo, fullHi) = (hi, hi)
  proc empty(x: int): bool = e.coverage(x, y, aa) == 0
  let ok = (lo <= cut.x0 or empty(lo - 1)) and (hi >= cut.x1 or empty(hi)) and
           (fullLo >= fullHi or (e.full(fullLo, y, aa) and e.full(fullHi - 1, y, aa)))
  if ok: RowSpan(lo: lo, fullLo: fullLo, fullHi: fullHi, hi: hi) else: exact

proc ellipseSurface*(r, clip: Rect; antiAlias: bool): Surface =
  ## The Elliptical Marquee's shape: the ellipse inscribed in `r`, cut to `clip`, as a GRAY8
  ## selection. Anti-aliased edge pixels hold the fraction of their 4×4 sub-samples inside;
  ## without anti-aliasing a pixel is selected when its centre is inside.
  result = newSurface(Gray8)
  let cut = r.intersect(clip)
  if cut.isEmpty: return
  let e = newEllipse(r)
  var enc: array[17, uint8]
  for n in 0 .. 16: enc[n] = encode8(float32(n) / 16'f32)
  let one = encode8(1)
  var rows = newSeq[RowSpan](cut.height)
  for i in 0 ..< cut.height: rows[i] = rowSpan(e, cut.y0 + i, cut, antiAlias)
  let full = result.solidTile([1'f32])
  let blank = result.solidTile([0'f32])
  var built: seq[(TileCoord, Tile)]
  for tc in cut.tiles:
    let rect = tc.rect
    let tr = rect.intersect(cut)
    if tr == rect:
      var allFull = true
      for y in tr.y0 ..< tr.y1:
        let sp = rows[y - cut.y0]
        if not (sp.fullLo <= tr.x0 and sp.fullHi >= tr.x1):
          allFull = false
          break
      if allFull:
        built.add (tc, full)
        continue
    var tile: Tile = nil
    for y in tr.y0 ..< tr.y1:
      let sp = rows[y - cut.y0]
      let row = (y - rect.y0) * TS
      template at(x: int): int = row + (x - rect.x0)
      let (a, b) = (max(sp.fullLo, tr.x0), min(sp.fullHi, tr.x1))
      if a < b:
        if tile.isNil: tile = blank.clone
        for x in a ..< b: tile.data[at(x)] = one
      for (s0, s1) in [(max(sp.lo, tr.x0), min(sp.fullLo, tr.x1)), (max(sp.fullHi, tr.x0), min(sp.hi, tr.x1))]:
        for x in s0 ..< s1:
          let n = e.coverage(x, y, antiAlias)
          if n > 0:
            if tile.isNil: tile = blank.clone
            tile.data[at(x)] = enc[n]
    if not tile.isNil: built.add (tc, tile)
  result.putTiles(built)

proc combineSurfaces*(a, b: Surface; area: Rect; f: proc(x, y: float32): float32): Surface =
  ## `f(a, b)` per pixel over `area` for two selections, as a GRAY8 selection holding nothing
  ## outside `area`. GRAY8 inputs (every selection) go tile by tile through a table of all
  ## 256×256 byte pairs; a tile `f` leaves unchanged stays shared with its input.
  result = newSurface(Gray8)
  if area.isEmpty: return
  if a.format != Gray8 or b.format != Gray8:
    let (ra, rb) = (a.readRegion(area), b.readRegion(area))
    var data = newSeq[float32](min(ra.len, rb.len))
    for i in 0 ..< data.len: data[i] = f(ra[i], rb[i])
    if data.len == area.width * area.height:
      result.writeRegion(area, data)
      result.prune()
    return
  var lut = newSeq[uint8](65536)
  for i in 0 .. 65535: lut[i] = encode8(f(decode8(uint8(i shr 8)), decode8(uint8(i and 0xFF))))
  template pair(x, y: uint8): uint8 = lut[(int(x) shl 8) or int(y)]
  let da = (if a.defaultBytes.len > 0: a.defaultBytes[0] else: 0'u8)
  let db = (if b.defaultBytes.len > 0: b.defaultBytes[0] else: 0'u8)
  # Whether `f` passes one side through where the other side has no tile (e.g. Add, Subtract).
  var keepsA = true
  var keepsB = true
  for v in 0'u8 .. 255'u8:
    if pair(v, db) != v: keepsA = false
    if pair(da, v) != v: keepsB = false
  let (full, blank) = (result.solidTile([1'f32]), result.solidTile([0'f32]))
  var built: seq[(TileCoord, Tile)]
  for tc in area.tiles:
    let rect = tc.rect
    let tr = rect.intersect(area)
    let ta = a.tiles.getOrDefault(tc)
    let tb = b.tiles.getOrDefault(tc)
    if tr == rect:
      var pass: Tile = nil
      if not ta.isNil and tb.isNil and keepsA: pass = ta
      elif ta.isNil and not tb.isNil and keepsB: pass = tb
      elif ta.isNil and tb.isNil and pair(da, db) == 0: continue
      if not pass.isNil:
        var any = false
        for v in pass.data:
          if v != 0:
            any = true
            break
        if any: built.add (tc, pass)
        continue
    var tile = blank.clone
    var (lo, hi) = ((if tr == rect: 255'u8 else: 0'u8), 0'u8)
    let (x0, x1) = (tr.x0 - rect.x0, tr.x1 - rect.x0)
    for ly in tr.y0 - rect.y0 ..< tr.y1 - rect.y0:
      let (s, e) = (ly * TS + x0, ly * TS + x1)
      for i in s ..< e:
        let x = (if not ta.isNil and e <= ta.data.len: ta.data[i] else: da)
        let y = (if not tb.isNil and e <= tb.data.len: tb.data[i] else: db)
        let d = pair(x, y)
        tile.data[i] = d
        lo = min(lo, d)
        hi = max(hi, d)
    if hi > 0: built.add (tc, (if lo == 255'u8: full else: tile))
  result.putTiles(built)
