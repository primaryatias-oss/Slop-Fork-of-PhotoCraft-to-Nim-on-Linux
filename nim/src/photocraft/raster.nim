## Sparse, copy-on-write tiled surfaces in any `PixelFormat`.
##
## A `Surface` is an infinite plane of 256×256 tiles. Missing tiles read as the surface's
## default pixel (transparent for layers, white for "reveal all" masks). Tiles are shared refs,
## so copying a surface is O(tiles) and mutation copies only the touched tiles (`isUniqueRef`
## decides), which makes undo snapshots, background jobs and autosave cheap.
##
## Port of `crates/raster`.

import std/[tables, algorithm, sets, math]
import geom, color

type
  AllocationError* = object of CatchableError ## A surface allocation could not be represented.

  Cancelled* = object of CatchableError ## An operation stopped because it was cancelled.

  Interrupt* = object
    ## Cancellation check plus progress sink for one long operation. Cheap to copy.
    cancelFn*: proc(): bool {.closure.}
    progressFn*: proc(f: float32) {.closure.}

  Tile* = ref object
    ## Pixel storage for one tile: `TileSize² × bytesPerPixel`, row-major, interleaved channels.
    data*: seq[byte]

  Surface* = object
    format*: PixelFormat
    defPx: seq[byte]
    tiles*: Table[TileCoord, Tile]

  Rgba8Image* = object
    ## A simple owned RGBA8 image, used for display and thumbnails.
    width*, height*: int
    pixels*: seq[uint8]

  Rgba* = array[4, float32]

const NoInterrupt* = Interrupt()

# ---------------------------------------------------------------- interrupt

proc newInterrupt*(cancel: proc(): bool; progress: proc(f: float32) = nil): Interrupt =
  Interrupt(cancelFn: cancel, progressFn: progress)

proc cancelled*(i: Interrupt): bool {.inline.} = (not i.cancelFn.isNil) and i.cancelFn()

proc progress*(i: Interrupt; fraction: float32) =
  ## Report progress (`0..1`; values outside are clamped, NaN is ignored).
  if not i.progressFn.isNil and classify(fraction) notin {fcNan, fcInf, fcNegInf}:
    i.progressFn(clamp(fraction, 0.0'f32, 1.0'f32))

proc check*(i: Interrupt) =
  ## Raises `Cancelled` once cancelled.
  if i.cancelled: raise newException(Cancelled, "cancelled")

# ---------------------------------------------------------------- tiles

const TilePixels* = TileSize * TileSize
const MaxTileBytes = TilePixels * 64

proc filledTile(format: PixelFormat; pixel: openArray[byte]): Tile =
  let bpp = format.bytesPerPixel
  let bytes = TilePixels * bpp
  if bytes > MaxTileBytes: raise newException(AllocationError, "requested surface allocation is too large")
  result = Tile(data: newSeq[byte](bytes))
  var nonzero = false
  for b in pixel:
    if b != 0: nonzero = true
  if nonzero:
    for i in 0 ..< TilePixels:
      for k in 0 ..< bpp: result.data[i * bpp + k] = pixel[k]

proc clone*(t: Tile): Tile = Tile(data: t.data)
proc bytes*(t: Tile): lent seq[byte] = t.data

# ---------------------------------------------------------------- pixel encoding

proc encodePixel*(format: PixelFormat; px: openArray[float32]; outp: var openArray[byte]) =
  ## Encode normalised floats into one pixel's bytes.
  for i in 0 ..< min(px.len, format.channels): writeSample(outp, format.sample, i, px[i])

proc decodePixel*(format: PixelFormat; bytes: openArray[byte]): seq[float32] =
  result = newSeq[float32](format.channels)
  for i in 0 ..< format.channels: result[i] = readSample(bytes, format.sample, i)

proc toRgba*(format: PixelFormat; px: openArray[float32]): Rgba =
  let n = format.mode.colorChannels
  let a = if format.alpha: px[n] else: 1.0'f32
  var rgb: array[3, float32]
  case format.mode
  of cmGrayscale, cmBitmap, cmDuotone: rgb = [px[0], px[0], px[0]]
  of cmCmyk: rgb = cmykToRgb([px[0], px[1], px[2], px[3]])
  of cmLab: rgb = labToSrgb([px[0] * 100.0'f32, px[1] * 255.0'f32 - 128.0'f32, px[2] * 255.0'f32 - 128.0'f32])
  else: rgb = [px[0], px[1], px[2]]
  [rgb[0], rgb[1], rgb[2], a]

proc fromRgbaInto*(format: PixelFormat; rgba: Rgba; outp: var openArray[float32]): int =
  ## Writes `format.channels` values into `outp` and returns how many were written.
  let rgb = [rgba[0], rgba[1], rgba[2]]
  var n: int
  case format.mode
  of cmGrayscale, cmBitmap, cmDuotone:
    outp[0] = rgbToGray(rgb)
    n = 1
  of cmCmyk:
    let c = rgbToCmyk(rgb)
    for i in 0..3: outp[i] = c[i]
    n = 4
  of cmLab:
    let l = srgbToLab(rgb)
    outp[0] = l[0] / 100.0'f32
    outp[1] = (l[1] + 128.0'f32) / 255.0'f32
    outp[2] = (l[2] + 128.0'f32) / 255.0'f32
    n = 3
  else:
    for i in 0..2: outp[i] = rgb[i]
    n = 3
  if format.alpha:
    outp[n] = rgba[3]
    n + 1
  else: n

proc fromRgba*(format: PixelFormat; rgba: Rgba): seq[float32] =
  var buf: array[8, float32]
  let n = fromRgbaInto(format, rgba, buf)
  result = newSeq[float32](n)
  for i in 0 ..< n: result[i] = buf[i]

proc convertPixel*(src, dst: PixelFormat; px: openArray[float32]): seq[float32] =
  ## Convert one pixel between formats via sRGB RGBA.
  if src.mode == dst.mode:
    let n = src.mode.colorChannels
    for i in 0 ..< n: result.add px[i]
    if dst.alpha: result.add(if src.alpha: px[n] else: 1.0'f32)
    return
  fromRgba(dst, toRgba(src, px))

proc isHdr*(sample: SampleType): bool = sample == stF32

proc newRgba8Image*(width, height: int): Rgba8Image =
  Rgba8Image(width: width, height: height, pixels: newSeq[uint8](width * height * 4))

# ---------------------------------------------------------------- surface

proc newSurface*(format: PixelFormat): Surface =
  ## Transparent/zero surface.
  Surface(format: format, defPx: newSeq[byte](format.bytesPerPixel), tiles: initTable[TileCoord, Tile]())

proc surfaceWithDefault*(format: PixelFormat; pixel: openArray[float32]): Surface =
  ## Surface whose untouched pixels read as `pixel` (normalised channel values).
  result = newSurface(format)
  encodePixel(format, pixel, result.defPx)

proc `==`*(a, b: Surface): bool =
  if a.format != b.format or a.defPx != b.defPx or a.tiles.len != b.tiles.len: return false
  for c, t in a.tiles:
    if not b.tiles.hasKey(c): return false
    let u = b.tiles[c]
    if t != u and t.data != u.data: return false
  true

proc channels*(s: Surface): int {.inline.} = s.format.channels
proc tileCount*(s: Surface): int = s.tiles.len
proc tile*(s: Surface; c: TileCoord): Tile = s.tiles.getOrDefault(c)
proc defaultPixel*(s: Surface): seq[float32] = decodePixel(s.format, s.defPx)
proc defaultBytes*(s: Surface): lent seq[byte] = s.defPx

proc sortedCoords*(s: Surface): seq[TileCoord] =
  ## Tile coordinates in (tx, ty) order (deterministic iteration).
  for c in s.tiles.keys: result.add c
  result.sort(proc(a, b: TileCoord): int = cmp((a.tx, a.ty), (b.tx, b.ty)))

iterator sortedTiles*(s: Surface): (TileCoord, Tile) =
  for c in s.sortedCoords: yield (c, s.tiles[c])

proc tileMut*(s: var Surface; c: TileCoord): Tile =
  ## Mutable access to a tile, allocating it (filled with the default pixel) or un-sharing it.
  if not s.tiles.hasKey(c):
    s.tiles[c] = filledTile(s.format, s.defPx)
  elif not isUniqueRef(s.tiles[c]):
    s.tiles[c] = s.tiles[c].clone
  s.tiles[c]

proc tryTileMut*(s: var Surface; c: TileCoord): Tile = s.tileMut(c)

proc defaultTile*(s: Surface): Tile = filledTile(s.format, s.defPx)

proc solidTile*(s: Surface; px: openArray[float32]): Tile =
  ## A tile of this surface's format filled with `px`; share it across coordinates.
  var enc = newSeq[byte](s.format.bytesPerPixel)
  encodePixel(s.format, px, enc)
  filledTile(s.format, enc)

proc tileBounds*(s: Surface): Rect =
  ## Coarse bounds: union of allocated tile rects.
  result = EmptyRect
  for c in s.tiles.keys: result = result.union(c.rect)

proc pixelEq(data: seq[byte]; off: int; dp: seq[byte]): bool {.inline.} =
  for k in 0 ..< dp.len:
    if data[off + k] != dp[k]: return false
  true

proc contentBounds*(s: Surface): Rect =
  ## Exact bounds of pixels that differ from the default pixel.
  let bpp = s.format.bytesPerPixel
  if s.tiles.len == 0: return EmptyRect
  var gx0, gy0 = high(int)
  var gx1, gy1 = low(int)
  for c in s.tiles.keys:
    gx0 = min(gx0, c.tx); gy0 = min(gy0, c.ty); gx1 = max(gx1, c.tx); gy1 = max(gy1, c.ty)
  var order = s.sortedCoords
  order.sort(proc(a, b: TileCoord): int =
    cmp(min(min(a.tx - gx0, gx1 - a.tx), min(a.ty - gy0, gy1 - a.ty)), min(min(b.tx - gx0, gx1 - b.tx), min(b.ty - gy0, gy1 - b.ty))))
  let dp = s.defPx
  result = EmptyRect
  for c in order:
    let t = s.tiles[c]
    let origin = c.rect
    if not result.isEmpty and result.containsRect(origin): continue
    var x0, y0 = high(int)
    var x1, y1 = 0
    for y in 0 ..< TileSize:
      let row = y * TileSize * bpp
      var first = -1
      for x in 0 ..< TileSize:
        if not pixelEq(t.data, row + x * bpp, dp):
          first = x
          break
      if first < 0: continue
      var last = first
      for x in countdown(TileSize - 1, first):
        if not pixelEq(t.data, row + x * bpp, dp):
          last = x
          break
      x0 = min(x0, first)
      x1 = max(x1, last + 1)
      y0 = min(y0, y)
      y1 = y + 1
    if x0 != high(int):
      result = result.union(rect(x0, y0, x1, y1).translate(origin.x0, origin.y0))

proc prune*(s: var Surface) =
  ## Drop tiles that are entirely the default pixel.
  let dp = s.defPx
  let bpp = dp.len
  var drop: seq[TileCoord]
  for c, t in s.tiles:
    var any = false
    for i in 0 ..< TilePixels:
      if not pixelEq(t.data, i * bpp, dp):
        any = true
        break
    if not any: drop.add c
  for c in drop: s.tiles.del c

proc locate(s: Surface; x, y: int): (TileCoord, int) {.inline.} =
  let c = tileContaining(x, y)
  let lx = remEuclid(x, TileSize)
  let ly = remEuclid(y, TileSize)
  (c, (ly * TileSize + lx) * s.format.channels)

proc readPixel*(s: Surface; x, y: int; outp: var openArray[float32]) =
  let (c, base) = s.locate(x, y)
  let n = min(s.channels, outp.len)
  let t = s.tiles.getOrDefault(c)
  if t.isNil:
    for i in 0 ..< n: outp[i] = readSample(s.defPx, s.format.sample, i)
  else:
    for i in 0 ..< n: outp[i] = readSample(t.data, s.format.sample, base + i)

proc pixel*(s: Surface; x, y: int): seq[float32] =
  ## Read one pixel as normalised channel values (`channels` floats).
  result = newSeq[float32](s.channels)
  s.readPixel(x, y, result)

proc writePixel*(s: var Surface; x, y: int; px: openArray[float32]) =
  let (c, base) = s.locate(x, y)
  let n = min(s.channels, px.len)
  let t = s.tileMut(c)
  for i in 0 ..< n: writeSample(t.data, s.format.sample, base + i, px[i])

proc sampleChannel*(s: Surface; x, y, c: int): float32 {.inline.} =
  ## One channel of one pixel, without allocating.
  let (tc, base) = s.locate(x, y)
  let t = s.tiles.getOrDefault(tc)
  if t.isNil: readSample(s.defPx, s.format.sample, c) else: readSample(t.data, s.format.sample, base + c)

proc sampleRowStrided*(s: Surface; y, x0, step0, c: int; outp: var openArray[float32]) =
  ## Channel `c` at `(x0 + i·step, y)` for `i in 0..outp.len`, one tile lookup per run.
  let step = max(step0, 1)
  let dp = readSample(s.defPx, s.format.sample, c)
  let n = s.format.channels
  let ty = divEuclid(y, TileSize)
  let ly = remEuclid(y, TileSize)
  var i = 0
  while i < outp.len:
    let x = x0 + i * step
    let tx = divEuclid(x, TileSize)
    let tileEnd = (tx + 1) * TileSize
    let count = max((tileEnd - x + step - 1) div step, 1)
    let e = min(i + count, outp.len)
    let t = s.tiles.getOrDefault(tileCoord(tx, ty))
    if t.isNil:
      for k in i ..< e: outp[k] = dp
    else:
      for k in i ..< e:
        let lx = x + (k - i) * step - tx * TileSize
        outp[k] = readSample(t.data, s.format.sample, (ly * TileSize + lx) * n + c)
    i = e

proc readRegionInto*(s: Surface; r: Rect; outp: var seq[float32]) =
  ## `readRegion` into a reusable buffer (resized to fit).
  let n = s.channels
  let w = r.width
  outp.setLen(0)
  outp.setLen(w * r.height * n)
  let dp = s.defaultPixel
  for tc in r.tiles:
    let tr = tc.rect.intersect(r)
    let t = s.tiles.getOrDefault(tc)
    let span = tr.width * n
    for y in tr.y0 ..< tr.y1:
      let o = ((y - r.y0) * w + (tr.x0 - r.x0)) * n
      if t.isNil:
        for px in 0 ..< tr.width:
          for k in 0 ..< n: outp[o + px * n + k] = dp[k]
      else:
        let base = ((y - tc.ty * TileSize) * TileSize + (tr.x0 - tc.tx * TileSize)) * n
        case s.format.sample
        of stU8:
          for i in 0 ..< span: outp[o + i] = float32(t.data[base + i]) / 255.0'f32
        else:
          for i in 0 ..< span: outp[o + i] = readSample(t.data, s.format.sample, base + i)

proc readRegion*(s: Surface; r: Rect): seq[float32] =
  ## Read a rectangle as interleaved normalised floats (`w*h*channels`).
  s.readRegionInto(r, result)

proc tryReadRegion*(s: Surface; r: Rect): seq[float32] =
  let len = int64(r.width) * int64(r.height) * int64(s.channels)
  if len > int64(high(int32)) * 4: raise newException(AllocationError, "requested surface allocation is too large")
  s.readRegion(r)

proc readRgbaInto*(s: Surface; r: Rect; outp: var openArray[Rgba]) =
  ## Reads `r` as straight RGBA floats (converting from the surface's model) into `outp`.
  let w = r.width
  let fmt = s.format
  let n = fmt.channels
  let dp = toRgba(fmt, s.defaultPixel)
  var px: array[8, float32]
  for tc in r.tiles:
    let tr = tc.rect.intersect(r)
    let t = s.tiles.getOrDefault(tc)
    for y in tr.y0 ..< tr.y1:
      let o = (y - r.y0) * w + (tr.x0 - r.x0)
      let cnt = tr.width
      if t.isNil:
        for i in 0 ..< cnt: outp[o + i] = dp
        continue
      let base = ((y - tc.ty * TileSize) * TileSize + (tr.x0 - tc.tx * TileSize)) * n
      if fmt.mode == cmRgb and fmt.sample == stU8 and fmt.alpha:
        for i in 0 ..< cnt:
          let b = base + i * 4
          outp[o + i] = [float32(t.data[b]) / 255.0'f32, float32(t.data[b + 1]) / 255.0'f32,
                         float32(t.data[b + 2]) / 255.0'f32, float32(t.data[b + 3]) / 255.0'f32]
      elif fmt.mode == cmGrayscale and fmt.sample == stU8 and fmt.alpha:
        for i in 0 ..< cnt:
          let b = base + i * 2
          let g = float32(t.data[b]) / 255.0'f32
          outp[o + i] = [g, g, g, float32(t.data[b + 1]) / 255.0'f32]
      elif fmt.mode == cmCmyk and fmt.sample == stU8:
        var prevKey: array[4, uint8]
        var prevRgb: array[3, float32]
        var havePrev = false
        for i in 0 ..< cnt:
          let b = base + i * n
          let ink = [t.data[b], t.data[b + 1], t.data[b + 2], t.data[b + 3]]
          if not (havePrev and prevKey == ink):
            prevRgb = cmykToRgb([float32(ink[0]) / 255.0'f32, float32(ink[1]) / 255.0'f32, float32(ink[2]) / 255.0'f32, float32(ink[3]) / 255.0'f32])
            prevKey = ink
            havePrev = true
          let a = if n > 4: float32(t.data[b + 4]) / 255.0'f32 else: 1.0'f32
          outp[o + i] = [prevRgb[0], prevRgb[1], prevRgb[2], a]
      else:
        for i in 0 ..< cnt:
          for c in 0 ..< n: px[c] = readSample(t.data, fmt.sample, base + i * n + c)
          outp[o + i] = toRgba(fmt, px.toOpenArray(0, n - 1))

proc readRgba8Into*(s: Surface; r: Rect; outp: var openArray[array[4, uint8]]) =
  ## Reads `r` as straight 8-bit RGBA into `outp` (`w*h` entries).
  let w = r.width
  let fmt = s.format
  let rgba8 = fmt.mode == cmRgb and fmt.sample == stU8 and fmt.alpha
  let dp = toRgba(fmt, s.defaultPixel)
  let dp8 = [q8(dp[0]), q8(dp[1]), q8(dp[2]), q8(dp[3])]
  var row: seq[Rgba]
  for tc in r.tiles:
    let tr = tc.rect.intersect(r)
    let t = s.tiles.getOrDefault(tc)
    for y in tr.y0 ..< tr.y1:
      let o = (y - r.y0) * w + (tr.x0 - r.x0)
      let cnt = tr.width
      if t.isNil:
        for i in 0 ..< cnt: outp[o + i] = dp8
      elif rgba8:
        let base = ((y - tc.ty * TileSize) * TileSize + (tr.x0 - tc.tx * TileSize)) * 4
        for i in 0 ..< cnt:
          let b = base + i * 4
          outp[o + i] = [t.data[b], t.data[b + 1], t.data[b + 2], t.data[b + 3]]
      else:
        row.setLen(cnt)
        s.readRgbaInto(rect(tr.x0, y, tr.x1, y + 1), row)
        for i in 0 ..< cnt: outp[o + i] = [q8(row[i][0]), q8(row[i][1]), q8(row[i][2]), q8(row[i][3])]

proc hasTilesIn*(s: Surface; r: Rect): bool =
  ## `true` if any allocated tile intersects `r`.
  for tc in r.tiles:
    if s.tiles.hasKey(tc): return true
  false

proc writeRegion*(s: var Surface; r: Rect; data: openArray[float32]) =
  ## Write interleaved normalised floats into a rectangle.
  let n = s.channels
  let w = r.width
  if data.len != w * r.height * n: raise newException(AllocationError, "region data length mismatch")
  let sample = s.format.sample
  for tc in r.tiles:
    let tr = tc.rect.intersect(r)
    let t = s.tileMut(tc)
    let span = tr.width * n
    for y in tr.y0 ..< tr.y1:
      let o = ((y - r.y0) * w + (tr.x0 - r.x0)) * n
      let base = ((y - tc.ty * TileSize) * TileSize + (tr.x0 - tc.tx * TileSize)) * n
      case sample
      of stU8:
        for i in 0 ..< span: t.data[base + i] = uint8(clamp(data[o + i], 0.0'f32, 1.0'f32) * 255.0'f32 + 0.5'f32)
      else:
        for i in 0 ..< span: writeSample(t.data, sample, base + i, data[o + i])

proc tryWriteRegion*(s: var Surface; r: Rect; data: openArray[float32]) = s.writeRegion(r, data)

proc takeTiles*(s: var Surface; r: Rect): seq[(TileCoord, Tile)] =
  ## Take the tiles covering `r` out of the surface (missing ones filled with the default pixel).
  var blank: Tile
  for c in r.tiles:
    var t: Tile
    if s.tiles.pop(c, t): result.add (c, t)
    else:
      if blank.isNil: blank = s.defaultTile
      result.add (c, blank)

proc putTiles*(s: var Surface; tiles: openArray[(TileCoord, Tile)]) =
  for (c, t) in tiles: s.tiles[c] = t

proc fillRect*(s: var Surface; r: Rect; px: openArray[float32]) =
  ## Fill a rectangle with one pixel value. Fully covered tiles share one tile.
  var enc = newSeq[byte](s.format.bytesPerPixel)
  encodePixel(s.format, px, enc)
  let bpp = enc.len
  var full: Tile
  for tc in r.tiles:
    let tr = tc.rect.intersect(r)
    if tr == tc.rect:
      if full.isNil: full = filledTile(s.format, enc)
      s.tiles[tc] = full
      continue
    let t = s.tileMut(tc)
    for y in tr.y0 ..< tr.y1:
      let ly = y - tc.ty * TileSize
      let lx0 = tr.x0 - tc.tx * TileSize
      let lx1 = tr.x1 - tc.tx * TileSize
      for x in lx0 ..< lx1:
        let b = (ly * TileSize + x) * bpp
        for k in 0 ..< bpp: t.data[b + k] = enc[k]

proc writeInterleaved*(s: var Surface; r: Rect; bytes: openArray[byte]) =
  ## Write interleaved encoded bytes (same format as the surface) into `r`.
  let bpp = s.format.bytesPerPixel
  let w = r.width
  if bytes.len != w * r.height * bpp: raise newException(AllocationError, "interleaved length mismatch")
  for tc in r.tiles:
    let tr = tc.rect.intersect(r)
    let t = s.tileMut(tc)
    let span = tr.width * bpp
    for y in tr.y0 ..< tr.y1:
      let src = ((y - r.y0) * w + (tr.x0 - r.x0)) * bpp
      let dst = ((y - tc.ty * TileSize) * TileSize + (tr.x0 - tc.tx * TileSize)) * bpp
      if span > 0: copyMem(addr t.data[dst], unsafeAddr bytes[src], span)

proc surfaceFromInterleaved*(format: PixelFormat; r: Rect; bytes: openArray[byte]): Surface =
  ## Build from interleaved *encoded* bytes of `format` covering `r`.
  result = newSurface(format)
  result.writeInterleaved(r, bytes)

proc toInterleaved*(s: Surface; r: Rect): seq[byte] =
  ## Read `r` as interleaved encoded bytes in the surface's format.
  let bpp = s.format.bytesPerPixel
  let w = r.width
  result = newSeq[byte](w * r.height * bpp)
  for tc in r.tiles:
    let tr = tc.rect.intersect(r)
    let span = tr.width * bpp
    let t = s.tiles.getOrDefault(tc)
    for y in tr.y0 ..< tr.y1:
      let dst = ((y - r.y0) * w + (tr.x0 - r.x0)) * bpp
      if t.isNil:
        for p in 0 ..< tr.width:
          for k in 0 ..< bpp: result[dst + p * bpp + k] = s.defPx[k]
      else:
        let src = ((y - tc.ty * TileSize) * TileSize + (tr.x0 - tc.tx * TileSize)) * bpp
        if span > 0: copyMem(addr result[dst], unsafeAddr t.data[src], span)

proc tryToInterleaved*(s: Surface; r: Rect): seq[byte] =
  let len = int64(r.width) * int64(r.height) * int64(s.format.bytesPerPixel)
  if len > int64(high(int32)) * 4: raise newException(AllocationError, "requested surface allocation is too large")
  s.toInterleaved(r)

proc translated*(s: Surface; dx, dy: int; content: Rect): Surface =
  ## The surface moved by whole pixels; moves by whole tiles share the tiles themselves.
  ## `content` must hold every pixel that isn't the default one.
  result = Surface(format: s.format, defPx: s.defPx, tiles: initTable[TileCoord, Tile]())
  var sources: seq[TileCoord]
  for c in s.sortedCoords:
    if not c.rect.intersect(content).isEmpty: sources.add c
  if dx mod TileSize == 0 and dy mod TileSize == 0:
    let tx = dx div TileSize
    let ty = dy div TileSize
    for c in sources: result.tiles[tileCoord(satAdd(c.tx, tx), satAdd(c.ty, ty))] = s.tiles[c]
    return
  let bpp = s.format.bytesPerPixel
  let moved = content.translate(dx, dy)
  var targets = initHashSet[TileCoord]()
  for c in sources:
    for t in c.rect.translate(dx, dy).intersect(moved).tiles: targets.incl t
  for dc in targets:
    let tile = filledTile(s.format, s.defPx)
    let dr = dc.rect
    let sr = dr.translate(-dx, -dy)
    for sc in sr.intersect(content).tiles:
      let st = s.tiles.getOrDefault(sc)
      if st.isNil: continue
      let part = sc.rect.intersect(sr)
      let span = part.width * bpp
      for y in part.y0 ..< part.y1:
        let srcI = (y - sc.ty * TileSize) * TileSize + (part.x0 - sc.tx * TileSize)
        let dstI = (y + dy - dr.y0) * TileSize + (part.x0 + dx - dr.x0)
        if srcI < 0 or dstI < 0: continue
        let src = srcI * bpp
        let dst = dstI * bpp
        if dst + span <= tile.data.len and src + span <= st.data.len and span > 0:
          copyMem(addr tile.data[dst], unsafeAddr st.data[src], span)
    result.tiles[dc] = tile

proc convert*(s: Surface; to: PixelFormat): Surface =
  ## Convert to another pixel format (depth and/or model).
  result = surfaceWithDefault(to, convertPixel(s.format, to, s.defaultPixel))
  let fromN = s.channels
  let toN = to.channels
  var src = newSeq[float32](fromN)
  for c, t in s.tiles:
    let dst = result.tileMut(c)
    for i in 0 ..< TilePixels:
      for k in 0 ..< fromN: src[k] = readSample(t.data, s.format.sample, i * fromN + k)
      let px = convertPixel(s.format, to, src)
      for k in 0 ..< min(px.len, toN): writeSample(dst.data, to.sample, i * toN + k, px[k])

proc tryConvert*(s: Surface; to: PixelFormat): Surface = s.convert(to)

proc rgba*(s: Surface; x, y: int): Rgba =
  ## Composite-friendly RGBA read (converts from the surface's model).
  toRgba(s.format, s.pixel(x, y))
