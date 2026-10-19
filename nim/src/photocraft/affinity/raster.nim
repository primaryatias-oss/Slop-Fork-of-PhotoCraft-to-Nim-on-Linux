## Pixel layers (`Rstr`) and placed images (`ImgN`), port of `affinity/src/raster.rs`. A placed
## image keeps its original file (`DyBm.Bckg` → archive entry `c/N`), which is imported as is;
## other pixels are planar tiles of 256 bytes × 256 rows per channel, each its own archive entry.

import std/[options, tables, math]
import ./container, ./stream, ./types

const
  MaxPixels = 64'u64 shl 20 ## pixels decoded per layer (the content's bounding box, not the canvas)
  Tile = 256
  TileBytes = Tile * Tile

proc u32Of(v: Option[int64]): uint32 =
  if v.isSome and v.get >= 0 and v.get <= int64(high(uint32)): uint32(v.get) else: 0'u32

proc original(r: var Reader; name: string): seq[byte] =
  ## The original image file: entry `c/N` is a stream whose root (`Blck`) holds it as `Data`.
  let bytes = r.archive.read(name)
  let s = parseStream(bytes)
  if s.root < 0 or s.root >= s.objects.len: raise malformed("embedded image")
  let v = s.objects[s.root].get(tagOf("Data"))
  if v.isSome and v.get.kind == svBlob and v.get.bytes.len > 0: return v.get.bytes
  raise malformed("embedded image without data")

proc tagN(prefix: string; c: int): string = prefix & $char(ord('0') + c)

proc satMul(a, b: int): int =
  if a == 0 or b == 0: 0
  elif a > high(int) div b: high(int)
  else: a * b

proc usizeOf(v: Option[int64]): Option[int] =
  if v.isSome and v.get >= 0: some(int(v.get)) else: none(int)

template copyTile(outp: var seq[byte]; row: int; bx0, py0, bx1, py1, ox, oy: int; at: untyped) =
  ## Copies the overlap of a tile at byte column `ox`, row `oy` into the cropped plane; `at`
  ## reads the tile sample at (`tx`, `ty`).
  for y in max(oy, py0) ..< min(oy + Tile, py1):
    for x in max(ox, bx0) ..< min(ox + Tile, bx1):
      let i = (y - py0) * row + (x - bx0)
      if i >= 0 and i < outp.len:
        let tx {.inject.} = x - ox
        let ty {.inject.} = y - oy
        outp[i] = at

proc plane(r: var Reader; bitmap: ObjId; c, bps: int; crop: (int, int, int, int);
           cache: var Table[string, seq[byte]]): seq[byte] =
  ## One channel of level 0, cropped to (x0, y0, w, h) pixels; `bps` bytes per sample.
  let (x0, y0, w, h) = crop
  let width = usizeOf(r.s.intOf(bitmap, "BmpW")).get(0)
  let height = usizeOf(r.s.intOf(bitmap, "BmpH")).get(0)
  let tw = usizeOf(r.s.intOf(bitmap, tagN("TWi", c))).get((width * bps + Tile - 1) div Tile)
  let th = usizeOf(r.s.intOf(bitmap, tagN("THi", c))).get((height + Tile - 1) div Tile)
  var states: seq[uint8]
  let st = r.s.field(bitmap, tagN("Sta", c))
  if st.isSome and st.get.kind == svArray:
    for v in st.get.arr:
      states.add(if v.kind == svUInt and v.u <= 255: uint8(v.u) else: 255'u8)
  let tiles = r.s.children(bitmap, tagN("Idx", c))
  let row = w * bps
  result = newSeq[byte](row * h)
  let (bx0, bx1) = (x0 * bps, (x0 + w) * bps)
  let (py0, py1) = (y0, y0 + h)
  var next = 0
  let twm = max(tw, 1)
  for t in 0 ..< min(states.len, satMul(tw, th)):
    let state = states[t]
    let (ty, tx) = (t div twm, t mod twm)
    var fill = -1
    case state
    of 0, 1: discard
    of 2: fill = 0xFF
    of 4:
      if next >= tiles.len: raise malformed("missing pixel tile")
      let blck = tiles[next]
      inc next
      let (ox, oy) = (tx * Tile, ty * Tile)
      if ox >= bx1 or ox + Tile <= bx0 or oy >= py1 or oy + Tile <= py0: continue
      let name = r.s.entry(blck, "Data")
      if name.isNone: raise malformed("pixel tile without data")
      if name.get notin cache:
        var data = r.archive.read(name.get)
        # Older files wrap a tile in a one-field stream: header, then a 64 KiB blob.
        if data.len == TileBytes + 22 and data[0] == 0x00 and data[1] == 0xFF and data[2] == byte('K') and data[3] == byte('S'):
          data = data[21 ..< 21 + TileBytes]
        if data.len != TileBytes: raise malformed("pixel tile size")
        cache[name.get] = data
      let tile = addr cache[name.get]
      copyTile(result, row, bx0, py0, bx1, py1, ox, oy, tile[][ty * Tile + tx])
      continue
    of 3: r.warn("32-bit pixel layers")
    of 5: r.warn("pixel layers drawn from an embedded image (left empty)")
    else: raise malformed("unknown pixel tile state")
    if fill >= 0:
      let v = byte(fill)
      copyTile(result, row, bx0, py0, bx1, py1, tx * Tile, ty * Tile, v)

proc roundU8(c: float64): uint8 =
  ## Rust `(c.clamp(0.0, 1.0) * 255.0).round() as u8`.
  let v = round(clamp(c, 0.0, 1.0) * 255.0)
  if v != v or v <= 0.0: 0'u8 elif v >= 255.0: 255'u8 else: uint8(v)

proc decode(r: var Reader; bitmap: ObjId; crop: (uint32, uint32, uint32, uint32); world: AfAffine): Option[AfImage] =
  ## Decodes the tiles of a bitmap inside `crop` (pixels), placed by `world` (bitmap pixels to
  ## document).
  let (x0, y0, x1, y1) = crop
  if x1 <= x0 or y1 <= y0: return
  let (w, h) = (x1 - x0, y1 - y0)
  if uint64(w) * uint64(h) > MaxPixels:
    r.warn("pixel layers larger than 64 megapixels")
    return
  let format = r.s.enumeration(bitmap, "Frmt")
  var channels, bps: int
  case (if format.isSome: int(format.get[0]) else: -1)
  of 0: (channels, bps) = (4, 1)
  of 1: (channels, bps) = (4, 2)
  of 6: (channels, bps) = (1, 1)
  of 7: (channels, bps) = (1, 2)
  of 4:
    r.warn("CMYK pixel layers (converted to RGB without the document's colour profile)")
    (channels, bps) = (5, 1)
  else:
    r.warn("pixel layers in grey, Lab or 32-bit formats")
    return
  var planes: seq[seq[byte]]
  var cache: Table[string, seq[byte]]
  for c in 1 .. channels:
    planes.add plane(r, bitmap, c, bps, (int(x0), int(y0), int(w), int(h)), cache)
  let n = int(w) * int(h)
  var rgba = newSeq[byte](n * 4)
  template sample(p: seq[byte]; i: int): float64 =
    if bps == 2:
      let lo = if 2 * i < p.len: p[2 * i] else: 0'u8
      let hi = if 2 * i + 1 < p.len: p[2 * i + 1] else: 0'u8
      float64(uint16(lo) or (uint16(hi) shl 8)) / 65535.0
    else:
      float64(if i < p.len: p[i] else: 0'u8) / 255.0
  var v = newSeq[float64](channels)
  for i in 0 ..< n:
    for c in 0 ..< channels: v[c] = sample(planes[c], i)
    var px: array[4, float64]
    if channels == 1: px = [v[0], v[0], v[0], 1.0]
    elif channels == 5:
      let k = 1.0 - v[3]
      px = [(1.0 - v[0]) * k, (1.0 - v[1]) * k, (1.0 - v[2]) * k, v[4]]
    else: px = [v[0], v[1], v[2], v[3]]
    for c in 0 .. 3: rgba[4 * i + c] = roundU8(px[c])
  let transform = AfAffine(m: [1.0, 0.0, 0.0, 1.0, float64(x0), float64(y0)]).then(world)
  some(AfImage(width: w, height: h, pixels: AfPixels(kind: pxRgba8, data: rgba), transform: transform))

proc nodeImage*(r: var Reader; id: ObjId; world: AfAffine): Option[AfImage] =
  let bm = r.s.child(id, "Bitm")
  if bm.isNone:
    r.warn("pixel layers without pixels")
    return
  let bitmap = bm.get
  if not r.s.isClass(bitmap, "DyBm"):
    r.warn("pixel layers stored in an unknown way")
    return
  let width = u32Of(r.s.intOf(bitmap, "BmpW"))
  let height = u32Of(r.s.intOf(bitmap, "BmpH"))
  if width == 0 or height == 0: return
  # A placed image: the original file, byte for byte.
  let hasTiles = r.s.hasField(bitmap, "Sta1")
  let name = r.s.entry(bitmap, "Bckg")
  if name.isSome and name.get.len > 0 and (r.s.isClass(id, "ImgN") or not hasTiles):
    try:
      let bytes = original(r, name.get)
      return some(AfImage(width: width, height: height, pixels: AfPixels(kind: pxEncoded, data: bytes), transform: world))
    except AffinityError as e:
      if e.kind == afLimit: raise
      r.warn("an embedded image file that could not be read (its cached pixels were used)")
  # The content's bounding box, so a mostly empty canvas-sized layer stays small.
  var crop = (0'u32, 0'u32, width, height)
  let br = r.s.field(id, "BitR")
  if br.isSome and br.get.kind == svInts and br.get.ints.len == 4 and br.get.ints[0] > int64(low(int32)) + 1:
    let v = br.get.ints
    crop = (uint32(clamp(v[0], 0'i64, int64(width))), uint32(clamp(v[1], 0'i64, int64(height))),
            uint32(clamp(v[2], 0'i64, int64(width))), uint32(clamp(v[3], 0'i64, int64(height))))
  if r.s.hasField(bitmap, "Bckg") and hasTiles and not r.s.isClass(id, "ImgN"):
    r.warn("edited placed images use their stored pixels")
  decode(r, bitmap, crop, world)

proc mask*(r: var Reader; id: ObjId; world: AfAffine): Option[AfImage] =
  ## A mask layer (`MRst`): one channel of coverage as grey pixels (white shows, black hides).
  let bm = r.s.child(id, "Bitm")
  if bm.isNone: return
  let bitmap = bm.get
  let width = u32Of(r.s.intOf(bitmap, "BmpW"))
  let height = u32Of(r.s.intOf(bitmap, "BmpH"))
  let f = r.s.enumeration(bitmap, "Frmt")
  if width == 0 or height == 0 or not (f.isSome and f.get[0] in [6'u16, 7'u16]):
    r.warn("pixel masks in an unknown format (imported without them)")
    return
  decode(r, bitmap, (0'u32, 0'u32, width, height), world)

proc embedded*(r: var Reader; id: ObjId; world: AfAffine): Option[AfImage] =
  ## An embedded document or symbol instance (`EmbN`), as the picture of it Affinity caches.
  let e = r.s.child(id, "Bitm")
  if e.isNone: return
  let cache = r.s.child(e.get, "Cach")
  if cache.isNone: return
  let bbox = r.s.floatsN(e.get, "ChBB", 4)
  if bbox.isNone: return
  let width = u32Of(r.s.intOf(cache.get, "BmpW"))
  let height = u32Of(r.s.intOf(cache.get, "BmpH"))
  if width == 0 or height == 0: return
  let (x0, y0, x1, y1) = (bbox.get[0], bbox.get[1], bbox.get[2], bbox.get[3])
  # The instance's origin is the centre of the embedded content (checked against the
  # thumbnails of documents with embedded documents and SVG files).
  let (cx, cy) = ((x0 + x1) / 2.0, (y0 + y1) / 2.0)
  let place = AfAffine(m: [(x1 - x0) / float64(width), 0.0, 0.0, (y1 - y0) / float64(height), x0 - cx, y0 - cy]).then(world)
  decode(r, cache.get, (0'u32, 0'u32, width, height), place)
