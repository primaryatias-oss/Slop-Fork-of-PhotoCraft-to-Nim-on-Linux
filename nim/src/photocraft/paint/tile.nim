## Grayscale bitmaps for sampled tips and texture patterns, with a compact serde form (port of
## `paint/src/tile.rs`).
##
## Values are 16-bit (0..65535 ↔ 0..1) so presets stay small; JSON carries them as base64 of the
## little-endian samples (`"data": "…"`), and also accepts a plain array of 0..1 floats.

import std/[math, json]
import ../rmath, ../serde

type
  GrayTile* = object
    ## A grayscale bitmap, row-major, 1 = full.
    width*, height*: uint32
    data*: seq[uint16]

  GrayTileRef* = ref GrayTile

  StoredTile* = object
    ## A tip or texture bitmap kept outside the brush settings by its owner and loaded only when
    ## the brush is used: the preset store keeps imported tips on disk, so a library of big sampled
    ## brushes costs a few small previews in memory.
    ##
    ## `preview` is a small copy (`GrayTile.preview`) for thumbnails and stroke previews; `full` is
    ## the bitmap once the owner has loaded it (shared with the owner's cache, never serialised).
    ## Two stored tiles are equal when their key and size are (the key names the content).
    key*: string
    width*, height*: uint32
    preview*: GrayTile
    full*: GrayTileRef

  MipLevel* = object
    w*, h*: int
    v*: seq[float32]

  Mips* = object
    ## A float mip pyramid of a `GrayTile` for sampling at any scale without aliasing.
    levels*: seq[MipLevel] ## level 0 = full resolution

  PatternImage* = object
    ## A tiling float pattern (texture), sampled with wrap-around.
    width*, height*: int
    data*: seq[float32]

const PreviewSide* = 96'u32
  ## Larger side of the previews kept for tips that are loaded on demand (`StoredTile`).

proc q16(v: float32): uint16 {.inline.} = asU16(clamp(v, 0'f32, 1'f32) * 65535'f32 + 0.5'f32)

proc grayTileFromFn*(width, height: uint32; f: proc(x, y: uint32): float32): GrayTile =
  ## Build from a function returning 0..1 values.
  result = GrayTile(width: width, height: height)
  result.data = newSeqOfCap[uint16](int(width) * int(height))
  for y in 0'u32 ..< height:
    for x in 0'u32 ..< width:
      result.data.add q16(f(x, y))

proc grayTileFromF32*(width, height: uint32; v: openArray[float32]): GrayTile =
  ## Build from 0..1 floats (`width × height`); raises ValueError on a size mismatch.
  if v.len != int(width) * int(height): raise newException(ValueError, "GrayTile size mismatch")
  result = GrayTile(width: width, height: height, data: newSeq[uint16](v.len))
  for i, x in v: result.data[i] = q16(x)

proc get*(t: GrayTile; x, y: uint32): float32 {.inline.} =
  ## The value at a pixel (0 outside the data).
  let i = int(y) * int(t.width) + int(x)
  if i < t.data.len: float32(t.data[i]) / 65535'f32 else: 0'f32

proc toF32*(t: GrayTile): seq[float32] =
  result = newSeq[float32](t.data.len)
  for i, v in t.data: result[i] = float32(v) / 65535'f32

proc isValid*(t: GrayTile): bool =
  t.width > 0 and t.height > 0 and t.data.len == int(t.width) * int(t.height)

proc divCeil(a, b: int): int {.inline.} = (a + b - 1) div b

proc preview*(t: GrayTile; maxSide: uint32): GrayTile =
  ## A box-filtered copy whose larger side is at most `maxSide` (the whole tile when it is already
  ## that small), quantised to 8-bit levels so it stores compactly. Deterministic. An invalid tile
  ## gives 1×1.
  proc q8(v: uint32): uint16 = uint16((v + 128) div 257 * 257)
  if not t.isValid: return GrayTile(width: 1, height: 1, data: @[0'u16])
  let (w, h) = (int(t.width), int(t.height))
  let mx = int(max(maxSide, 1'u32))
  let f = max(divCeil(max(w, h), mx), 1)
  let (nw, nh) = (divCeil(w, f), divCeil(h, f))
  var sum = newSeq[uint64](nw * nh)
  var cnt = newSeq[uint32](nw * nh)
  for y in 0 ..< h:
    let base = (y div f) * nw
    for x in 0 ..< w:
      let j = base + x div f
      if j < sum.len:
        sum[j] += uint64(t.data[y * w + x])
        cnt[j] += 1
  result = GrayTile(width: uint32(nw), height: uint32(nh), data: newSeq[uint16](nw * nh))
  for i in 0 ..< sum.len:
    result.data[i] = q8(if cnt[i] > 0: uint32(sum[i] div uint64(cnt[i])) else: 0'u32)

proc bitmap*(s: StoredTile): lent GrayTile =
  ## The full bitmap when loaded, else the preview.
  if s.full.isNil: return s.preview
  return s.full[]

proc isLoaded*(s: StoredTile): bool = not s.full.isNil

proc `==`*(a, b: StoredTile): bool = a.key == b.key and a.width == b.width and a.height == b.height

# ---------------------------------------------------------------- base64

const B64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

proc b64Encode*(b: openArray[byte]): string =
  ## Standard base64 with padding (the tile serde form; commands taking file bytes).
  result = newStringOfCap((b.len + 2) div 3 * 4)
  var i = 0
  while i < b.len:
    let c1 = if i + 1 < b.len: uint32(b[i + 1]) else: 0'u32
    let c2 = if i + 2 < b.len: uint32(b[i + 2]) else: 0'u32
    let n = (uint32(b[i]) shl 16) or (c1 shl 8) or c2
    result.add B64[int((n shr 18) and 63)]
    result.add B64[int((n shr 12) and 63)]
    result.add(if i + 1 < b.len: B64[int((n shr 6) and 63)] else: '=')
    result.add(if i + 2 < b.len: B64[int(n and 63)] else: '=')
    i += 3

proc b64Decode*(s: string): Option[seq[byte]] =
  ## Decode standard base64 (whitespace ignored); none on invalid input.
  proc val(c: char): int =
    case c
    of 'A'..'Z': ord(c) - ord('A')
    of 'a'..'z': ord(c) - ord('a') + 26
    of '0'..'9': ord(c) - ord('0') + 52
    of '+': 62
    of '/': 63
    else: -1
  var t = newStringOfCap(s.len)
  for c in s:
    if c notin {' ', '\t', '\n', '\r', '\f'}: t.add c
  if t.len mod 4 != 0: return none(seq[byte])
  var outp = newSeqOfCap[byte](t.len div 4 * 3)
  var k = 0
  while k < t.len:
    var pad = 0
    for j in countdown(k + 3, k):
      if t[j] == '=': inc pad
      else: break
    var n = 0'u32
    for j in 0 ..< 4:
      var d = 0
      if j < 4 - pad:
        d = val(t[k + j])
        if d < 0: return none(seq[byte])
      n = (n shl 6) or uint32(d)
    outp.add byte((n shr 16) and 0xFF)
    if pad < 2: outp.add byte((n shr 8) and 0xFF)
    if pad < 1: outp.add byte(n and 0xFF)
    k += 4
  some(outp)

# ---------------------------------------------------------------- serde

proc toJ*(t: GrayTile): JsonNode =
  var bytes = newSeq[byte](t.data.len * 2)
  for i, v in t.data:
    bytes[2 * i] = byte(v and 0xFF)
    bytes[2 * i + 1] = byte(v shr 8)
  result = %*{"width": t.width, "height": t.height, "data": b64Encode(bytes)}

proc requireField(n: JsonNode; key: string): JsonNode =
  result = n.getOrDefault(key)
  if result.isNil: raise serdeError("missing field `" & key & "`")

proc fromJ*(n: JsonNode; t: var GrayTile) =
  if n.kind != JObject: raise serdeError("expected a tile object")
  var w, h: uint32
  fromJ(requireField(n, "width"), w)
  fromJ(requireField(n, "height"), h)
  let d = requireField(n, "data")
  var data: seq[uint16]
  case d.kind
  of JString:
    let b = b64Decode(d.getStr)
    if b.isNone: raise serdeError("invalid base64 in tile data")
    let bytes = b.get
    data = newSeq[uint16](bytes.len div 2)
    for i in 0 ..< data.len: data[i] = uint16(bytes[2 * i]) or (uint16(bytes[2 * i + 1]) shl 8)
  of JArray:
    data = newSeq[uint16](d.len)
    for i, e in d.elems:
      var f: float32
      if e.kind notin {JInt, JFloat}: raise serdeError("data did not match any variant")
      fromJ(e, f)
      data[i] = q16(f)
  else: raise serdeError("data did not match any variant")
  let n64 = uint64(w) * uint64(h)
  if n64 == 0 or uint64(data.len) != n64:
    raise serdeError("tile data has " & $data.len & " samples, expected " & $w & "×" & $h)
  t = GrayTile(width: w, height: h, data: data)

proc toJ*(s: StoredTile): JsonNode =
  %*{"key": s.key, "width": s.width, "height": s.height, "preview": toJ(s.preview)}

proc fromJ*(n: JsonNode; s: var StoredTile) =
  if n.kind != JObject: raise serdeError("expected a stored tile object")
  var r = StoredTile()
  fromJ(requireField(n, "key"), r.key)
  fromJ(requireField(n, "width"), r.width)
  fromJ(requireField(n, "height"), r.height)
  fromJ(requireField(n, "preview"), r.preview)
  s = r

# ---------------------------------------------------------------- sampling

proc newMips*(t: GrayTile): Mips =
  result.levels.add MipLevel(w: int(t.width), h: int(t.height), v: t.toF32)
  while true:
    let last = result.levels[^1]
    if last.w <= 1 and last.h <= 1: break
    let (w, h) = (last.w, last.h)
    let (nw, nh) = (divCeil(w, 2), divCeil(h, 2))
    var nv = newSeq[float32](nw * nh)
    for y in 0 ..< nh:
      for x in 0 ..< nw:
        var s = 0'f32
        var c = 0'f32
        for dy in 0 .. 1:
          for dx in 0 .. 1:
            let (sx, sy) = (x * 2 + dx, y * 2 + dy)
            if sx < w and sy < h:
              s += last.v[sy * w + sx]
              c += 1
        nv[y * nw + x] = s / c
    result.levels.add MipLevel(w: nw, h: nh, v: nv)

proc levelFor*(m: Mips; px: float32): int =
  ## Mip level for drawing the tile at `px` pixels across its larger side.
  let l0 = m.levels[0]
  let ratio = rmax(float32(max(l0.w, l0.h)) / rmax(px, 1'f32), 1'f32)
  min(asIdx(floor(log2(ratio))), m.levels.len - 1)

proc sampleClamped*(m: Mips; level: int; u, v: float32): float32 {.inline.} =
  ## Bilinear sample at normalised `u, v` in 0..1 (outside = 0).
  let L = m.levels[level]
  let fx = u * float32(L.w) - 0.5'f32
  let fy = v * float32(L.h) - 0.5'f32
  let (x0f, y0f) = (floor(fx), floor(fy))
  let (tx, ty) = (fx - x0f, fy - y0f)
  let (x0, y0) = (asI64(x0f), asI64(y0f))
  template get(x, y: int64): float32 =
    (if x < 0 or y < 0 or x >= int64(L.w) or y >= int64(L.h): 0'f32 else: L.v[int(y) * L.w + int(x)])
  let a = get(x0, y0) + (get(x0 + 1, y0) - get(x0, y0)) * tx
  let b = get(x0, y0 + 1) + (get(x0 + 1, y0 + 1) - get(x0, y0 + 1)) * tx
  a + (b - a) * ty

proc sampleWrap*(p: PatternImage; x, y: float32): float32 {.inline.} =
  let xi = int(floorMod(asI64(floor(x)), int64(p.width)))
  let yi = int(floorMod(asI64(floor(y)), int64(p.height)))
  p.data[yi * p.width + xi]
