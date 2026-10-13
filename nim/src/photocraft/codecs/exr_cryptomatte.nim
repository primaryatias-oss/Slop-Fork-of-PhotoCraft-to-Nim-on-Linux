## Cryptomatte layers of an EXR (specification 1.2): object/material ID streams with coverage,
## their manifests, and the ID hash. Rust `codecs/exr_cryptomatte.rs` (listing; per-pixel
## decoding is not ported yet).

import std/[options, json, strutils, algorithm]
import image, exr

type
  CryptomatteLayer* = object
    part*: int
    name*, key*: string
    conversion*: Option[string]
    manifest*: seq[(float32, string)]
    sidecarManifest*: Option[string]
    channels*: seq[string]

  StreamChannel* = object
    stream*: string
    index*: uint32
    coverage*: bool

proc nameOf*(l: CryptomatteLayer; id: float32): Option[string] =
  for (i, n) in l.manifest:
    if i == id: return some(n)

proc splitStream*(channel: string): Option[StreamChannel] =
  let dot = channel.rfind('.')
  if dot < 0: return
  let stream = channel[0 ..< dot]
  let coverage = case channel[dot + 1 .. ^1]
    of "red", "r": false
    of "green", "g": true
    else: return
  var digits = 0
  while digits < stream.len and stream[stream.len - 1 - digits] in Digits: inc digits
  if digits == 0 or digits == stream.len: return
  try:
    let index = parseUInt(stream[stream.len - digits .. ^1])
    if index > uint(high(uint32)): return
    some(StreamChannel(stream: stream, index: uint32(index), coverage: coverage))
  except ValueError: none(StreamChannel)

proc rotl(x: uint32; r: int): uint32 {.inline.} = (x shl r) or (x shr (32 - r))

proc murmur3x32(data: string): uint32 =
  const C1 = 0xcc9e2d51'u32
  const C2 = 0x1b873593'u32
  var h = 0'u32
  let nblocks = data.len div 4
  for i in 0 ..< nblocks:
    var k = uint32(ord(data[4 * i])) or (uint32(ord(data[4 * i + 1])) shl 8) or
            (uint32(ord(data[4 * i + 2])) shl 16) or (uint32(ord(data[4 * i + 3])) shl 24)
    k = rotl(k * C1, 15) * C2
    h = h xor k
    h = rotl(h, 13) * 5 + 0xe6546b64'u32
  let rest = data.len - nblocks * 4
  if rest > 0:
    var k = 0'u32
    for i in 0 ..< rest: k = k or (uint32(ord(data[nblocks * 4 + i])) shl (8 * i))
    k = rotl(k * C1, 15) * C2
    h = h xor k
  h = h xor uint32(data.len)
  h = h xor (h shr 16)
  h = h * 0x85ebca6b'u32
  h = h xor (h shr 13)
  h = h * 0xc2b2ae35'u32
  h xor (h shr 16)

proc cryptomatteId*(name: string): float32 =
  ## The float ID Cryptomatte assigns to an object name (MurmurHash3 with the exponent fixed).
  var bits = murmur3x32(name)
  let exp = (bits shr 23) and 0xff
  if exp == 0 or exp == 255: bits = bits xor (1'u32 shl 23)
  cast[float32](bits)

proc cryptomatteKey*(name: string): string =
  toHex(cast[uint32](cryptomatteId(name))).toLowerAscii[0 ..< 7]

proc cryptomattePreviewColor*(id: float32): array[3, float32] =
  let bits = cast[uint32](id)
  [0'f32, cast[float32](bits shl 8), cast[float32](bits shl 16)]

proc attribute(attrs: seq[(string, string)]; key, field: string): Option[string] =
  let full = "cryptomatte/" & key & "/" & field
  for (n, v) in attrs:
    if n == full: return some(v)

proc parseManifest(text: string): seq[(float32, string)] =
  var node: JsonNode
  try: node = parseJson(text)
  except CatchableError: return
  if node.kind != JObject: return
  for name, hex in node.pairs:
    if hex.kind != JString: continue
    var h = hex.getStr
    if h.startsWith("0x"): h = h[2 .. ^1]
    try:
      let bits = parseHexInt(h)
      if bits < 0 or bits > int(high(uint32)): continue
      result.add (cast[float32](uint32(bits)), name)
    except ValueError: discard

proc layerStreams(p: ExrPart; name: string): seq[string] =
  proc normalize(s: string): string = s.toLowerAscii.multiReplace(("_", ""), ("-", ""))
  let want = normalize(name)
  var found: seq[(uint32, string)]
  for c in p.channels:
    let s = splitStream(c.name)
    if s.isNone or s.get.coverage: continue
    var base = s.get.stream
    while base.len > 0 and base[^1] in Digits: base.setLen(base.len - 1)
    if normalize(base) == want: found.add (s.get.index, s.get.stream)
  found.sort()
  var last: (uint32, string)
  for i, f in found:
    if i == 0 or f != last: result.add f[1]
    last = f

proc cryptomatteKeys(attrs: seq[(string, string)]): seq[string] =
  for (n, _) in attrs:
    if not n.startsWith("cryptomatte/"): continue
    let rest = n["cryptomatte/".len .. ^1]
    let slash = rest.find('/')
    if slash <= 0: continue
    let key = rest[0 ..< slash]
    if key notin result: result.add key

proc cryptomatteLayers*(b: openArray[byte]; limits: Limits): seq[CryptomatteLayer] =
  ## Every Cryptomatte layer of the file, by part then name. Raises `CodecError`.
  const ManifestCap = 16 shl 20
  let parts = exrInfo(b)
  for p in parts:
    let w = p.dataWindow[2] - p.dataWindow[0] + 1
    let h = p.dataWindow[3] - p.dataWindow[1] + 1
    limits.checkBytes(w, h, 8)
    for key in cryptomatteKeys(p.attributes):
      let name = attribute(p.attributes, key, "name")
      if name.isNone: continue
      let channels = layerStreams(p, name.get)
      if channels.len == 0: continue
      let m = attribute(p.attributes, key, "manifest")
      let manifest = if m.isSome and m.get.len <= ManifestCap: parseManifest(m.get) else: @[]
      result.add CryptomatteLayer(part: p.index, name: name.get, key: key,
                                  conversion: attribute(p.attributes, key, "conversion"), manifest: manifest,
                                  sidecarManifest: attribute(p.attributes, key, "manif_file"), channels: channels)
  result.sort(proc(a, b: CryptomatteLayer): int =
    result = cmp(a.part, b.part)
    if result == 0: result = cmp(a.name, b.name))
