## The native `.pcraft` format: a zip (or a directory) holding `manifest.json` (the document
## tree, serde-compatible with the Rust writer), content-addressed zstd-compressed tiles and blobs
## (`tiles/<blake3>.zst`, `blobs/<blake3>.zst`), and optional PNG previews. Saves are atomic
## (temp file + rename); a writer remembers tile hashes and compressed objects between saves so
## incremental saves only compress what changed. Also: crash-recovery autosaves.
##
## Port of crates/format. Deviations: single-threaded (autosaves run synchronously in
## `request`), and tile bytes are written in the host byte order (little-endian hosts only).

import std/[json, options, tables, sets, os, strutils, algorithm, times, hashes]
import ./geom, ./geom/warp, ./color, ./raster, ./doc, ./serde, ./zlib, ./blake3, ./zstd, ./codecs

const
  FormatVersion* = 1'u32
  Extension* = "pcraft"
  MaxCanvasSide* = 300_000
  MaxCanvasPixels* = int64(MaxCanvasSide) * int64(MaxCanvasSide)
  DefaultReverifyBudget* = 16'i64 * 1024 * 1024
  Generator = "photocraft-format 0.7.0 (nim)"
  ManifestName = "manifest.json"
  ThumbName = "thumb.png"
  CompositeName = "composite/preview.png"
  MaxManifestDepth = 3 * (MaxGroupDepth + 1) + 64
  ReadChunk* = 64 shl 20

type
  FormatErrorKind* = enum
    feCorrupt, feUnsupported, feTooNew, feLimitExceeded, feNonFinite, feIo, feJson
  FormatError* = object of CatchableError
    kind*: FormatErrorKind
    found*, supported*: uint32
    path*: string

  SaveOptions* = object
    thumbnail*: Option[Rgba8Image]
    composite*: Option[Rgba8Image]

  LoadOptions* = object
    maxManifestBytes*: int
    maxBlobBytes*: int
    maxTotalBytes*: int64
    preserveIds*: bool
    maxCanvasSide*: int
    maxCanvasPixels*: int64

  SaveStats* = object
    tilesTotal*, tilesWritten*, tilesReused*: int
    blobsTotal*, blobsWritten*: int
    objectsRemoved*: int
    manifestBytes*: int

  ByteSource* = ref object of RootObj
    ## Where a bundle's entries come from (a zip in memory or a directory).
  ZipSource* = ref object of ByteSource
    zip: ZipReader
  DirSource* = ref object of ByteSource
    root*: string

  ZipEntry* = object
    name*: string
    meth: uint16
    crc: uint32
    compressed, uncompressed, dataOffset: int
  ZipReader* = object
    bytes: seq[byte]
    entries*: seq[ZipEntry]
  ZipWriter* = object
    outp: seq[byte]
    central: seq[byte]
    count: int

  ObjectKind = enum okTile, okBlob
  Obj = object
    kind: ObjectKind
    tile: Tile
    blob: SharedBytes
  Signature = object
    len: int64
    mtime: int64
    inode: int64
  Verified = object
    sig: Signature
    checkedAt: uint64
  PcraftWriter* = ref object
    ## Saves documents, reusing tile hashes and compressed objects between saves.
    hashCache: Table[pointer, (Tile, string)]
    compressedCache: Table[string, seq[byte]]
    verifiedDir: string
    generation: uint64
    verified: Table[string, Verified]
    reverifyBudget: int64

  Prepared = object
    manifest: string
    objects: OrderedTable[string, Obj]
    previews: seq[(string, seq[byte])]
    stats: SaveStats

  Collect = object
    tiles: Table[string, Tile]
    blobs: Table[string, SharedBytes]
    used: Table[pointer, (Tile, string)]

proc fmtError(kind: FormatErrorKind; msg: string): ref FormatError =
  let m = (case kind
    of feCorrupt: "corrupt .pcraft bundle: " & msg
    of feUnsupported: "unsupported .pcraft feature: " & msg
    of feLimitExceeded: "limit exceeded: " & msg
    of feIo: "I/O: " & msg
    of feJson: "manifest JSON: " & msg
    else: msg)
  (ref FormatError)(kind: kind, msg: m)

proc corrupt(msg: string): ref FormatError = fmtError(feCorrupt, msg)
proc unsupported(msg: string): ref FormatError = fmtError(feUnsupported, msg)
proc limit(msg: string): ref FormatError = fmtError(feLimitExceeded, msg)

proc tooNew(found, supported: uint32): ref FormatError =
  (ref FormatError)(kind: feTooNew, found: found, supported: supported,
    msg: "bundle written by a newer version (format " & $found & ", this build reads up to " & $supported & ")")

proc nonFiniteError(path: string): ref FormatError =
  (ref FormatError)(kind: feNonFinite, path: path,
    msg: "`" & path & "` is NaN or infinite, which a .pcraft manifest cannot hold; reset that value and save again")

proc defaultLoadOptions*(): LoadOptions =
  LoadOptions(maxManifestBytes: 256 shl 20, maxBlobBytes: 1 shl 30, maxTotalBytes: 16'i64 shl 30,
              preserveIds: true, maxCanvasSide: MaxCanvasSide, maxCanvasPixels: MaxCanvasPixels)

# ---------------------------------------------------------------- hex and hashes

proc hex(bytes: openArray[byte]): string =
  const digits = "0123456789abcdef"
  result = newStringOfCap(bytes.len * 2)
  for b in bytes:
    result.add digits[int(b shr 4)]
    result.add digits[int(b and 15)]

proc unhex(s: string): seq[byte] =
  if s.len mod 2 != 0: raise corrupt("bad hex `" & s & "`")
  result = newSeq[byte](s.len div 2)
  for i in 0 ..< result.len:
    let pair = s[2 * i .. 2 * i + 1]
    if not (pair[0] in HexDigits and pair[1] in HexDigits): raise corrupt("bad hex `" & s & "`")
    result[i] = byte(parseHexInt(pair))

proc key4(s: string): string =
  let b = unhex(s)
  if b.len != 4: raise corrupt("bad 4-byte key `" & s & "`")
  for c in b: result.add char(c)

proc keyHex(k: string): string =
  var b: seq[byte]
  for c in k: b.add byte(c)
  hex(b)

proc isValidHash(h: string): bool =
  if h.len != 64: return false
  for c in h:
    if c notin {'0' .. '9', 'a' .. 'f'}: return false
  true

proc hashBytes(b: openArray[byte]): string = blake3Hex(b)

proc tilePath(h: string): string = "tiles/" & h & ".zst"
proc blobPath(h: string): string = "blobs/" & h & ".zst"
proc tileLen(f: PixelFormat): int = TileSize * TileSize * f.bytesPerPixel

# ---------------------------------------------------------------- zip

const
  LocalSig = 0x0403_4b50'u32
  CentralSig = 0x0201_4b50'u32
  EocdSig = 0x0605_4b50'u32

proc putU16(o: var seq[byte]; v: uint16) =
  o.add byte(v and 0xff)
  o.add byte(v shr 8)

proc putU32(o: var seq[byte]; v: uint32) =
  for i in 0 .. 3: o.add byte((v shr (8 * i)) and 0xff)

proc newZipWriter*(): ZipWriter = ZipWriter()

proc add*(z: var ZipWriter; name: string; data: openArray[byte]) =
  ## Adds a stored (uncompressed) entry.
  if z.count >= 65535: raise unsupported("more than 65535 zip entries (ZIP64 not supported)")
  if data.len > int(high(uint32)): raise unsupported("zip entry over 4 GiB")
  if z.outp.len > int(high(uint32)): raise unsupported("zip over 4 GiB")
  let offset = uint32(z.outp.len)
  let size = uint32(data.len)
  let crc = crc32(data)
  template header(o: var seq[byte]) =
    o.putU16(20)
    o.putU16(0x0800)
    o.putU16(0)
    o.putU16(0)
    o.putU16(0x21)
    o.putU32(crc)
    o.putU32(size)
    o.putU32(size)
    o.putU16(uint16(name.len))
  z.outp.putU32(LocalSig)
  header(z.outp)
  z.outp.putU16(0)
  for c in name: z.outp.add byte(c)
  z.outp.add data
  z.central.putU32(CentralSig)
  z.central.putU16(20)
  header(z.central)
  for _ in 0 ..< 8: z.central.add 0'u8
  z.central.putU32(0)
  z.central.putU32(offset)
  for c in name: z.central.add byte(c)
  inc z.count

proc finish*(z: var ZipWriter): seq[byte] =
  if z.outp.len > int(high(uint32)): raise unsupported("zip over 4 GiB")
  let cdOffset = uint32(z.outp.len)
  let cdSize = uint32(z.central.len)
  result = move z.outp
  result.add z.central
  result.putU32(EocdSig)
  for _ in 0 ..< 4: result.add 0'u8
  result.putU16(uint16(z.count))
  result.putU16(uint16(z.count))
  result.putU32(cdSize)
  result.putU32(cdOffset)
  result.putU16(0)

proc u16At(b: openArray[byte]; o: int): int =
  if o < 0 or o + 2 > b.len: raise corrupt("truncated zip")
  int(b[o]) or (int(b[o + 1]) shl 8)

proc u32At(b: openArray[byte]; o: int): int =
  if o < 0 or o + 4 > b.len: raise corrupt("truncated zip")
  int(b[o]) or (int(b[o + 1]) shl 8) or (int(b[o + 2]) shl 16) or (int(b[o + 3]) shl 24)

proc newZipReader*(bytes: seq[byte]): ZipReader =
  if bytes.len < 22: raise corrupt("not a zip archive (too short)")
  let lo = max(bytes.len - 22 - 65535, 0)
  var eocd = -1
  for i in countdown(bytes.len - 22, lo):
    if uint32(u32At(bytes, i)) == EocdSig:
      eocd = i
      break
  if eocd < 0: raise corrupt("zip end-of-central-directory not found")
  let count = u16At(bytes, eocd + 10)
  let cdSize = u32At(bytes, eocd + 12)
  let cdOff = u32At(bytes, eocd + 16)
  let cdEnd = cdOff + cdSize
  if cdEnd > eocd: raise corrupt("zip central directory out of range")
  result.bytes = bytes
  var p = cdOff
  for _ in 0 ..< count:
    let headerEnd = p + 46
    if headerEnd > cdEnd: raise corrupt("truncated zip central directory entry")
    if uint32(u32At(bytes, p)) != CentralSig: raise corrupt("bad zip central directory entry")
    let flags = u16At(bytes, p + 8)
    let meth = u16At(bytes, p + 10)
    let crc = uint32(u32At(bytes, p + 16))
    let compressed = u32At(bytes, p + 20)
    let uncompressed = u32At(bytes, p + 24)
    let nlen = u16At(bytes, p + 28)
    let elen = u16At(bytes, p + 30)
    let clen = u16At(bytes, p + 32)
    let local = u32At(bytes, p + 42)
    let nameEnd = headerEnd + nlen
    if nameEnd > cdEnd: raise corrupt("zip entry name out of range")
    var name = newString(nlen)
    for i in 0 ..< nlen: name[i] = char(bytes[headerEnd + i])
    if (flags and 1) != 0: raise unsupported("encrypted zip entries")
    let entryEnd = nameEnd + elen + clen
    if entryEnd > cdEnd: raise corrupt("zip central directory entry out of range")
    if uint32(u32At(bytes, local)) != LocalSig: raise corrupt("bad zip local header")
    let lnlen = u16At(bytes, local + 26)
    let lelen = u16At(bytes, local + 28)
    let dataOffset = local + 30 + lnlen + lelen
    if dataOffset + compressed > bytes.len: raise corrupt("zip entry `" & name & "` out of range")
    result.entries.add ZipEntry(name: name, meth: uint16(meth), crc: crc, compressed: compressed,
                                uncompressed: uncompressed, dataOffset: dataOffset)
    p = entryEnd

proc find*(z: ZipReader; name: string): int =
  for i, e in z.entries:
    if e.name == name: return i
  -1

proc read*(z: ZipReader; e: ZipEntry; max: int): seq[byte] =
  if e.uncompressed > max:
    raise limit("zip entry `" & e.name & "` is " & $e.uncompressed & " bytes (max " & $max & ")")
  template raw: untyped = z.bytes.toOpenArray(e.dataOffset, e.dataOffset + e.compressed - 1)
  case e.meth
  of 0: result = @raw
  of 8:
    try: result = inflateRaw(raw, max + 1)
    except CatchableError as err: raise corrupt("zip entry `" & e.name & "`: " & err.msg)
  else: raise unsupported("zip compression method " & $e.meth & " for `" & e.name & "`")
  if result.len != e.uncompressed or result.len > max: raise corrupt("zip entry `" & e.name & "` has wrong size")
  if crc32(result) != e.crc: raise corrupt("zip entry `" & e.name & "` failed its CRC check")

proc readByName*(z: ZipReader; name: string; max: int): seq[byte] =
  let i = z.find(name)
  if i < 0: raise corrupt("missing `" & name & "`")
  z.read(z.entries[i], max)

# ---------------------------------------------------------------- byte sources

method get*(s: ByteSource; path: string; max: int): seq[byte] {.base.} =
  raise unsupported("no byte source")

method get*(s: ZipSource; path: string; max: int): seq[byte] = s.zip.readByName(path, max)

const NulPathMsg* = "file name contained an unexpected NUL byte"
  ## Rust's `std::io` error for a path with a NUL byte. Nim's file procs pass paths to C as
  ## NUL-terminated strings, so such a path would silently name a different (truncated) file:
  ## every file access on a caller-supplied path checks for it first.

proc readFileBytes*(path: string; max = high(int)): seq[byte] =
  ## The whole file, refusing files over `max` bytes.
  if '\0' in path: raise fmtError(feIo, path & ": " & NulPathMsg)
  var f: File
  if not open(f, path, fmRead): raise fmtError(feIo, "cannot open " & path)
  defer: f.close()
  let size = f.getFileSize
  if size > max: raise limit("`" & path & "` exceeds " & $max & " bytes")
  result = newSeq[byte](size)
  if size > 0 and f.readBytes(result, 0, size) != size: raise fmtError(feIo, "short read on " & path)

method get*(s: DirSource; path: string; max: int): seq[byte] =
  let p = s.root / path
  if not fileExists(p):
    if dirExists(p): raise corrupt("`" & path & "` is not a regular file")
    raise corrupt("missing `" & path & "`")
  try: readFileBytes(p, max)
  except FormatError as e:
    if e.kind == feLimitExceeded: raise limit("`" & path & "` exceeds " & $max & " bytes")
    raise

proc newZipSource*(bytes: seq[byte]): ZipSource = ZipSource(zip: newZipReader(bytes))

# ---------------------------------------------------------------- atomic writes

var tempCounter {.threadvar.}: int

proc tempName*(file: string): string =
  inc tempCounter
  var stem = ""
  for i, r in file:
    if i >= 64: break
    stem.add r
  "." & stem & "." & $getCurrentProcessId() & "-" & $tempCounter & ".pcsave.tmp"

proc atomicWrite*(path: string; data: openArray[byte]; progress: proc(f: float32) = nil) =
  ## Writes `data` to a temporary file next to `path`, flushes it and renames it over `path`, so a
  ## crash or full disk never leaves a half-written file (the original stays unchanged).
  const unchanged = "the original file was not changed"
  var target = path
  try:
    if '\0' notin path and symlinkExists(path): target = expandSymlink(path)
  except OSError: discard
  let (dir0, name, ext) = splitFile(target)
  if name.len == 0 and ext.len == 0: raise fmtError(feIo, path & ": not a file path")
  let dir = (if dir0.len == 0: "." else: dir0)
  if '\0' in dir: raise fmtError(feIo, "cannot create the folder " & dir & ": " & NulPathMsg)
  if '\0' in name or '\0' in ext: raise fmtError(feIo, "cannot save " & path & ": " & NulPathMsg & "; " & unchanged)
  try: createDir(dir)
  except OSError as e: raise fmtError(feIo, "cannot create the folder " & dir & ": " & e.msg)
  var tmp = ""
  for _ in 0 ..< 8:
    let candidate = dir / tempName(name & ext)
    if fileExists(candidate): continue
    var f: File
    if not open(f, candidate, fmWrite):
      raise fmtError(feIo, "cannot save " & path & ": the folder " & dir & " is not writable; " & unchanged)
    try:
      const chunk = 8 shl 20
      var written = 0
      while written < data.len:
        let n = min(chunk, data.len - written)
        if f.writeBytes(data, written, n) != n:
          raise fmtError(feIo, "cannot save " & path & ": the disk is full; " & unchanged)
        written += n
        if progress != nil: progress(float32(written) / float32(max(data.len, 1)))
      f.flushFile()
    except IOError as e:
      f.close()
      try: removeFile(candidate) except OSError: discard
      raise fmtError(feIo, "cannot save " & path & ": " & e.msg & "; " & unchanged)
    except FormatError:
      f.close()
      try: removeFile(candidate) except OSError: discard
      raise
    f.close()
    tmp = candidate
    break
  if tmp.len == 0: raise fmtError(feIo, dir & ": could not create a temporary file")
  if fileExists(target):
    try: setFilePermissions(tmp, getFilePermissions(target)) except OSError: discard
  try: moveFile(tmp, target)
  except OSError as e:
    try: removeFile(tmp) except OSError: discard
    raise fmtError(feIo, "cannot save " & path & ": " & e.msg & "; " & unchanged)

# ---------------------------------------------------------------- non-finite check

proc nonFinitePath[T](x: T; path: string): string =
  ## The (serde-style) path of the first NaN or infinite float in `x`, or "".
  mixin nonFinitePath
  when T is SomeFloat:
    if x != x or x == Inf or x == NegInf: return path.strip(chars = {'.'}, trailing = false)
  elif T is Surface or T is SharedBytes or T is SharedF32 or T is JsonNode or T is Tile or T is ref:
    discard
  elif T is Option:
    if x.isSome: return nonFinitePath(x.get, path)
  elif T is seq or T is array:
    for i, v in x:
      let r = nonFinitePath(v, path & "[" & $i & "]")
      if r.len > 0: return r
  elif T is tuple:
    var i = 0
    for v in x.fields:
      let r = nonFinitePath(v, path & "[" & $i & "]")
      if r.len > 0: return r
      inc i
  elif T is object:
    for name, v in x.fieldPairs:
      let r = nonFinitePath(v, path & "." & snakeCase(name))
      if r.len > 0: return r
  ""

# ---------------------------------------------------------------- manifest writing

proc checkManifestDepth(json: string) =
  var depth = 0
  var inStr, escaped = false
  for b in json:
    if inStr:
      if escaped: escaped = false
      elif b == '\\': escaped = true
      elif b == '"': inStr = false
      continue
    case b
    of '"': inStr = true
    of '[', '{':
      inc depth
      if depth > MaxManifestDepth:
        raise limit("manifest JSON nested deeper than " & $MaxManifestDepth & " levels")
    of ']', '}': depth = max(depth - 1, 0)
    else: discard

proc sinkTile(c: var Collect; t: Tile; prev: Table[pointer, (Tile, string)]): string =
  let key = cast[pointer](t)
  var h: string
  if key in c.used: h = c.used[key][1]
  elif key in prev and cast[pointer](prev[key][0]) == key: h = prev[key][1]
  else: h = hashBytes(t.data)
  c.used[key] = (t, h)
  if h notin c.tiles: c.tiles[h] = t
  h

proc sinkBlob(c: var Collect; b: SharedBytes): string =
  let data = (if b.isNil: newSeq[byte]() else: b.data)
  result = hashBytes(data)
  if result notin c.blobs: c.blobs[result] = (if b.isNil: SharedBytes(data: @[]) else: b)

proc optBlob(c: var Collect; b: SharedBytes): JsonNode =
  if b.isNil: newJNull() else: newJString(c.sinkBlob(b))

proc surfaceM(c: var Collect; s: Surface; prev: Table[pointer, (Tile, string)]): JsonNode =
  let f = s.format
  var dp = newSeq[byte](f.bytesPerPixel)
  encodePixel(f, s.defaultPixel, dp)
  var tiles = newJArray()
  for (coord, t) in s.sortedTiles:
    tiles.add %*{"tx": coord.tx, "ty": coord.ty, "hash": c.sinkTile(t, prev)}
  %*{"format": toJ(f), "default": hex(dp), "tiles": tiles}

proc optSurfaceM(c: var Collect; s: Option[Surface]; prev: Table[pointer, (Tile, string)]): JsonNode =
  if s.isSome: c.surfaceM(s.get, prev) else: newJNull()

proc maskM(c: var Collect; m: LayerMask; prev: Table[pointer, (Tile, string)]): JsonNode =
  %*{"surface": c.surfaceM(m.surface, prev), "enabled": m.enabled, "linked": m.linked,
     "density": toJ(m.density), "feather": toJ(m.feather)}

proc effectsM(c: var Collect; e: Effects): JsonNode =
  %*{"enabled": e.enabled, "items": toJ(e.items), "psd_raw": c.optBlob(e.psdRaw), "reference": toJ(e.reference)}

proc layerM(c: var Collect; l: Layer; prev: Table[pointer, (Tile, string)]): JsonNode =
  var content: JsonNode
  case l.content.kind
  of lkRaster:
    content = %*{"kind": "raster", "surface": c.surfaceM(l.content.raster, prev)}
  of lkGroup:
    var kids = newJArray()
    for ch in l.content.group.children: kids.add c.layerM(ch, prev)
    content = %*{"kind": "group", "children": kids, "expanded": l.content.group.expanded,
                 "artboard": toJ(l.content.group.artboard)}
  of lkAdjustment:
    content = %*{"kind": "adjustment", "adjustment": toJ(l.content.adjustment)}
  of lkFill:
    content = %*{"kind": "fill", "fill": toJ(l.content.fill)}
  of lkText:
    template t: untyped = l.content.text
    content = %*{"kind": "text", "text": t.text, "font_family": t.fontFamily, "size_pt": toJ(t.sizePt),
                 "color": toJ(t.color), "transform": toJ(t.transform), "cache": c.optSurfaceM(t.cache, prev),
                 "psd_raw": c.optBlob(t.psdRaw), "runs": toJ(t.runs), "paragraphs": toJ(t.paragraphs),
                 "shape": toJ(t.shape), "orientation": toJ(t.orientation), "antialias": toJ(t.antialias),
                 "warp": toJ(t.warp)}
  of lkShape:
    template s: untyped = l.content.shape
    content = %*{"kind": "shape", "fill": toJ(s.fill), "cache": c.optSurfaceM(s.cache, prev),
                 "psd_raw": c.optBlob(s.psdRaw), "path": toJ(s.path), "stroke": toJ(s.stroke), "live": toJ(s.live)}
  of lkSmart:
    template s: untyped = l.content.smart
    let source = (if s.source.kind == ssEmbedded:
                    %*{"kind": "embedded", "file_name": s.source.fileName, "blob": c.sinkBlob(s.source.bytes)}
                  else: %*{"kind": "linked", "path": s.source.path})
    content = %*{"kind": "smart", "contents_id": uint64(s.contentsId), "source": source,
                 "transform": toJ(s.transform), "smart_filters": toJ(s.smartFilters),
                 "cache": c.optSurfaceM(s.cache, prev), "psd_raw": c.optBlob(s.psdRaw),
                 "filters_enabled": s.filtersEnabled,
                 "filter_mask": (if s.filterMask.isSome: c.maskM(s.filterMask.get, prev) else: newJNull()),
                 "warp": toJ(s.warp), "stack_mode": toJ(s.stackMode)}
    if s.perspective.isSome: content["perspective"] = toJ(s.perspective.get)
  var psdBlocks = newJArray()
  for b in l.psdBlocks: psdBlocks.add %*[keyHex(b.key), c.sinkBlob(b.data)]
  result = %*{"id": uint64(l.id), "name": l.name, "visible": l.visible, "locks": toJ(l.locks),
              "blend": toJ(l.blend), "opacity": toJ(l.opacity), "fill_opacity": toJ(l.fillOpacity),
              "clipped": l.clipped, "mask": (if l.mask.isSome: c.maskM(l.mask.get, prev) else: newJNull()),
              "vector_mask": toJ(l.vectorMask), "effects": c.effectsM(l.effects), "label": toJ(l.label),
              "content": content, "psd_blocks": psdBlocks, "psd_id": toJ(l.psdId),
              "fill_cache": (if l.fillCache.isSome:
                               %*{"fill": toJ(l.fillCache.get.fill), "surface": c.surfaceM(l.fillCache.get.surface, prev)}
                             else: newJNull()),
              "link_group": toJ(l.linkGroup)}
  if l.excludedChannels != 0: result["excluded_channels"] = toJ(l.excludedChannels)
  if not l.blendIf.isDefault: result["blend_if"] = toJ(l.blendIf)
  if not l.advanced.isDefault: result["advanced"] = toJ(l.advanced)
  if l.video.isSome:
    template v: untyped = l.video.get
    var frames = newJArray()
    for f in v.frames: frames.add c.surfaceM(f, prev)
    var vm = %*{"frames": frames}
    if v.originalFrames.isSome:
      var orig = newJArray()
      for f in v.originalFrames.get: orig.add c.surfaceM(f, prev)
      vm["original_frames"] = orig
    vm["source"] = toJ(v.source)
    vm["fps"] = toJ(v.fps)
    vm["show_altered"] = %v.showAltered
    result["video"] = vm
  else:
    result["video"] = newJNull()

proc channelM(c: var Collect; ch: AlphaChannel; prev: Table[pointer, (Tile, string)]): JsonNode =
  %*{"name": ch.name, "surface": c.surfaceM(ch.surface, prev), "spot": toJ(ch.spot), "color": toJ(ch.color),
     "opacity": toJ(ch.opacity), "indicates": toJ(ch.indicates)}

proc compM(c: var Collect; comp: LayerComp): JsonNode =
  var states = newJArray()
  for s in comp.states:
    var st = %*{"layer": uint64(s.layer), "visible": toJ(s.visible), "position": toJ(s.position)}
    if s.appearance.isSome:
      let a = s.appearance.get
      st["appearance"] = %*{"blend": toJ(a.blend), "opacity": toJ(a.opacity), "fill_opacity": toJ(a.fillOpacity),
                            "effects": c.effectsM(a.effects)}
    else: st["appearance"] = newJNull()
    states.add st
  %*{"id": comp.id, "name": comp.name, "comment": comp.comment, "apply_visibility": comp.applyVisibility,
     "apply_position": comp.applyPosition, "apply_appearance": comp.applyAppearance, "states": states}

proc docM(c: var Collect; d: Document; prev: Table[pointer, (Tile, string)]): JsonNode =
  var layers = newJArray()
  for l in d.layers: layers.add c.layerM(l, prev)
  var channels = newJArray()
  for ch in d.channels: channels.add c.channelM(ch, prev)
  var resources = newJArray()
  for r in d.metadata.psdResources: resources.add %*[r.id, r.name, c.sinkBlob(r.data)]
  var globals = newJArray()
  for g in d.metadata.psdGlobalBlocks: globals.add %*[keyHex(g.signature), keyHex(g.key), c.sinkBlob(g.data)]
  var md = %*{"xmp": toJ(d.metadata.xmp), "exif": c.optBlob(d.metadata.exif)}
  if d.metadata.text.len > 0: md["text"] = toJ(d.metadata.text)
  md["psd_resources"] = resources
  md["psd_global_blocks"] = globals
  var paths = newJArray()
  for p in d.paths: paths.add %*{"name": p.name, "path": toJ(p.path), "psd_raw": c.optBlob(p.psdRaw)}
  var patterns = newJArray()
  for p in d.patterns:
    patterns.add %*{"id": p.id, "name": p.name, "width": p.width, "height": p.height, "surface": c.surfaceM(p.surface, prev)}
  var comps = newJArray()
  for comp in d.layerComps: comps.add c.compM(comp)
  result = %*{"id": uint64(d.id), "name": d.name, "size": toJ(d.size), "resolution_dpi": toJ(d.resolutionDpi),
              "mode": toJ(d.mode), "depth": toJ(d.depth), "icc_profile": c.optBlob(d.iccProfile),
              "layers": layers, "channels": channels, "guides": toJ(d.guides),
              "selection": c.optSurfaceM(d.selection, prev), "metadata": md, "global_light": toJ(d.globalLight),
              "paths": paths, "work_path": toJ(d.workPath), "clipping_path": toJ(d.clippingPath),
              "quick_mask": (if d.quickMask.isSome: c.channelM(d.quickMask.get, prev) else: newJNull())}
  if d.quickMaskUnselected: result["quick_mask_unselected"] = %true
  result["patterns"] = patterns
  result["color_table"] = toJ(d.colorTable)
  result["duotone"] = toJ(d.duotone)
  result["layer_comps"] = comps
  result["last_applied_comp"] = toJ(d.lastAppliedComp)
  result["last_document_state"] = (if d.lastDocumentState.isSome: c.compM(d.lastDocumentState.get) else: newJNull())
  result["variables"] = toJ(d.variables)
  result["timeline"] = toJ(d.timeline)
  result["measurement"] = toJ(d.measurement)
  result["notes"] = toJ(d.notes)
  result["text_styles"] = toJ(d.textStyles)
  if not d.slices.isEmpty: result["slices"] = toJ(d.slices)

proc checkNesting*(layers: openArray[Layer]) =
  proc tooDeepAt(layers: openArray[Layer]; depth: int): bool =
    if depth > MaxGroupDepth and layers.len > 0: return true
    for l in layers:
      if l.content.kind == lkGroup and tooDeepAt(l.content.group.children, depth + 1): return true
  if tooDeepAt(layers, 0): raise limit("layer groups nested deeper than " & $MaxGroupDepth)

proc png(img: Rgba8Image): seq[byte] =
  try:
    encode(imageFromRaw(img.width, img.height, clRgba, isU8, img.pixels), fmtPng)
  except CatchableError as e: raise unsupported("preview: " & e.msg)

proc newPcraftWriter*(): PcraftWriter = PcraftWriter(reverifyBudget: DefaultReverifyBudget)

proc setReverifyBudget*(w: PcraftWriter; bytes: int64) = w.reverifyBudget = bytes

proc prepare(w: PcraftWriter; d: Document; opts: SaveOptions): Prepared =
  checkNesting(d.layers)
  let bad = nonFinitePath(d, "document")
  if bad.len > 0: raise nonFiniteError(bad)
  var c: Collect
  let document = c.docM(d, w.hashCache)
  w.hashCache = c.used
  if opts.thumbnail.isSome: result.previews.add (ThumbName, png(opts.thumbnail.get))
  if opts.composite.isSome: result.previews.add (CompositeName, png(opts.composite.get))
  let manifest = %*{"format_version": FormatVersion, "generator": Generator, "document": document,
                    "thumbnail": (if opts.thumbnail.isSome: %ThumbName else: newJNull()),
                    "composite": (if opts.composite.isSome: %CompositeName else: newJNull())}
  result.manifest = manifest.pretty(2)
  checkManifestDepth(result.manifest)
  result.stats = SaveStats(tilesTotal: c.tiles.len, blobsTotal: c.blobs.len, manifestBytes: result.manifest.len)
  var keys: seq[(string, Obj)]
  for h, t in c.tiles: keys.add (tilePath(h), Obj(kind: okTile, tile: t))
  for h, b in c.blobs: keys.add (blobPath(h), Obj(kind: okBlob, blob: b))
  keys.sort(proc(a, b: (string, Obj)): int = cmp(a[0], b[0]))
  for (k, o) in keys: result.objects[k] = o

proc rawPtr(o: Obj): ptr seq[byte] =
  if o.kind == okTile: addr o.tile.data else: addr o.blob.data

template rawBytes(o: Obj): untyped = o.rawPtr[]

proc compressed(o: Obj): seq[byte] = zstdCompress(o.rawBytes)

proc decompress(data: openArray[byte]; expected: int; what: string): seq[byte] =
  try: result = zstdDecompress(data, expected + 1)
  except CatchableError as e: raise corrupt(what & ": zstd: " & e.msg)
  if result.len != expected: raise corrupt(what & ": expected " & $expected & " bytes, got " & $result.len)

proc toBytes(s: string): seq[byte] =
  result = newSeq[byte](s.len)
  for i, ch in s: result[i] = byte(ch)

proc saveZip*(w: PcraftWriter; d: Document; opts = SaveOptions()): (seq[byte], SaveStats) =
  ## The document as `.pcraft` zip bytes, with save statistics.
  let p = w.prepare(d, opts)
  var stats = p.stats
  var z = newZipWriter()
  z.add(ManifestName, p.manifest.toBytes)
  for (name, data) in p.previews: z.add(name, data)
  var next: Table[string, seq[byte]]
  for path, obj in p.objects:
    var data: seq[byte]
    if path in w.compressedCache:
      if obj.kind == okTile: inc stats.tilesReused
      data = w.compressedCache[path]
    else:
      if obj.kind == okTile: inc stats.tilesWritten else: inc stats.blobsWritten
      data = obj.compressed
    z.add(path, data)
    next[path] = data
  w.compressedCache = next
  (z.finish, stats)

proc signatureOf(path: string): Option[Signature] =
  try:
    let info = getFileInfo(path, followSymlink = true)
    if info.kind != pcFile: return none(Signature)
    some(Signature(len: info.size, mtime: info.lastWriteTime.toUnix * 1_000_000_000 + info.lastWriteTime.nanosecond,
                   inode: int64(info.id.file)))
  except OSError: none(Signature)

proc existingObjectIsValid(path: string; o: Obj; what: string): (bool, Option[Signature]) =
  if not fileExists(path): return (false, none(Signature))
  let sig = signatureOf(path)
  let expectedLen = o.rawBytes.len
  let maxLen = expectedLen + expectedLen div 8 + 4096
  if sig.isNone or sig.get.len > maxLen: return (false, none(Signature))
  var data: seq[byte]
  try: data = readFileBytes(path, maxLen)
  except CatchableError: return (false, none(Signature))
  try:
    let actual = decompress(data, expectedLen, what)
    (hashBytes(actual) == hashBytes(o.rawBytes), sig)
  except CatchableError: (false, sig)

proc listObjects(dir: string): HashSet[string] =
  for sub in ["tiles", "blobs"]:
    if not dirExists(dir / sub): continue
    for kind, f in walkDir(dir / sub, relative = true):
      if kind == pcFile and f.endsWith(".zst"): result.incl sub & "/" & f

proc rollingRecheck(w: PcraftWriter; objects: OrderedTable[string, Obj]): HashSet[string] =
  if w.reverifyBudget <= 0: return
  var candidates: seq[(string, Verified)]
  for path, v in w.verified:
    if path in objects: candidates.add (path, v)
  candidates.sort(proc(a, b: (string, Verified)): int =
    result = cmp(a[1].checkedAt, b[1].checkedAt)
    if result == 0: result = cmp(a[0], b[0]))
  var spent = 0'i64
  for (path, v) in candidates:
    let next = spent + v.sig.len
    if result.len > 0 and next > w.reverifyBudget: break
    spent = next
    result.incl path

proc saveDir*(w: PcraftWriter; d: Document; dir: string; opts = SaveOptions()): SaveStats =
  ## Saves as a bundle directory, writing only objects that are missing or fail verification
  ## and removing ones the document no longer uses.
  let p = w.prepare(d, opts)
  result = p.stats
  for sub in ["tiles", "blobs", "composite"]:
    try: createDir(dir / sub)
    except OSError as e: raise fmtError(feIo, e.msg)
  let canonical = absolutePath(dir)
  if w.verifiedDir != canonical:
    w.verifiedDir = canonical
    w.verified.clear()
    w.generation = 0
  let existing = listObjects(dir)
  inc w.generation
  let recheck = w.rollingRecheck(p.objects)
  for path, obj in p.objects:
    let objectPath = dir / path
    let sig = signatureOf(objectPath)
    let alreadyVerified = path notin recheck and sig.isSome and path in w.verified and w.verified[path].sig == sig.get
    if path in existing and alreadyVerified:
      if obj.kind == okTile: inc result.tilesReused
      continue
    if path in existing:
      let (valid, s) = existingObjectIsValid(objectPath, obj, path)
      if valid:
        if s.isSome: w.verified[path] = Verified(sig: s.get, checkedAt: w.generation)
        if obj.kind == okTile: inc result.tilesReused
        continue
    if obj.kind == okTile: inc result.tilesWritten else: inc result.blobsWritten
    atomicWrite(objectPath, obj.compressed)
    let written = signatureOf(objectPath)
    if written.isSome: w.verified[path] = Verified(sig: written.get, checkedAt: w.generation)
    else: w.verified.del path
  for (name, data) in p.previews: atomicWrite(dir / name, data)
  for stale in [ThumbName, CompositeName]:
    var present = false
    for (n, _) in p.previews:
      if n == stale: present = true
    if not present:
      try: removeFile(dir / stale) except OSError: discard
  atomicWrite(dir / ManifestName, p.manifest.toBytes)
  for path in existing:
    if path notin p.objects:
      try:
        removeFile(dir / path)
        inc result.objectsRemoved
      except OSError: discard
      w.verified.del path

proc savePath*(w: PcraftWriter; d: Document; path: string; opts = SaveOptions()): SaveStats =
  ## A directory (or a path ending in a separator) saves as a bundle folder, anything else as a zip.
  if '\0' in path: raise fmtError(feIo, path & ": " & NulPathMsg)
  if dirExists(path) or path.endsWith('/') or path.endsWith('\\'):
    w.saveDir(d, path, opts)
  else:
    let (bytes, stats) = w.saveZip(d, opts)
    atomicWrite(path, bytes)
    stats

proc saveToBytes*(d: Document; opts = SaveOptions()): seq[byte] =
  newPcraftWriter().saveZip(d, opts)[0]

# ---------------------------------------------------------------- manifest reading

type
  Loader = object
    src: ByteSource
    opts: LoadOptions
    total: int64
    blobs: Table[string, SharedBytes]
    preserveIds: bool
    idMap: Table[uint64, LayerId]
    contentsIdMap: Table[uint64, SmartContentsId]
    legacyLinked: Table[string, SmartContentsId]
    tiles: Table[(SampleType, string), Tile]

proc account(ld: var Loader; n: int) =
  ld.total += n
  if ld.total > ld.opts.maxTotalBytes: raise limit("bundle expands beyond " & $ld.opts.maxTotalBytes & " bytes")

proc fetchTile(ld: var Loader; hash: string; len: int): seq[byte] =
  if not isValidHash(hash): raise corrupt("invalid tile hash `" & hash & "`")
  ld.account(len)
  let path = tilePath(hash)
  let z = ld.src.get(path, len + len div 8 + 4096)
  result = decompress(z, len, path)
  if hashBytes(result) != hash: raise corrupt(path & ": content does not match its hash")

proc fetchBlob(ld: var Loader; hash: string): SharedBytes =
  if hash in ld.blobs: return ld.blobs[hash]
  if not isValidHash(hash): raise corrupt("invalid blob hash `" & hash & "`")
  let path = blobPath(hash)
  let z = ld.src.get(path, ld.opts.maxBlobBytes)
  var data: seq[byte]
  try: data = zstdDecompress(z, ld.opts.maxBlobBytes + 1)
  except CatchableError as e: raise corrupt(path & ": zstd: " & e.msg)
  if data.len > ld.opts.maxBlobBytes: raise limit(path & " exceeds max_blob_bytes")
  ld.account(data.len)
  if hashBytes(data) != hash: raise corrupt(path & ": content does not match its hash")
  result = SharedBytes(data: data)
  ld.blobs[hash] = result

template field(n: JsonNode; key: string): JsonNode =
  (if n.kind == JObject and n.hasKey(key): n[key] else: newJNull())

proc req(n: JsonNode; key: string): JsonNode =
  if n.kind != JObject or not n.hasKey(key): raise fmtError(feJson, "missing field `" & key & "`")
  n[key]

proc parse[T](n: JsonNode; x: var T) =
  try: fromJ(n, x)
  except SerdeError as e: raise fmtError(feJson, e.msg)
  except ValueError as e: raise fmtError(feJson, e.msg)

proc get[T](n: JsonNode; key: string; _: typedesc[T]): T =
  parse(n.req(key), result)

proc getOr[T](n: JsonNode; key: string; default: T): T =
  let v = n.field(key)
  if v.kind == JNull: default
  else:
    var r = default
    parse(v, r)
    r

proc optBlob(ld: var Loader; n: JsonNode): SharedBytes =
  if n.kind == JNull: nil
  elif n.kind == JString: ld.fetchBlob(n.getStr)
  else: raise fmtError(feJson, "expected a blob hash")

proc surface(ld: var Loader; m: JsonNode): Surface =
  let f = m.get("format", PixelFormat)
  let dp = unhex(m.get("default", string))
  if dp.len != f.bytesPerPixel: raise corrupt("default pixel has wrong size for its format")
  result = surfaceWithDefault(f, decodePixel(f, dp))
  let len = tileLen(f)
  let tiles = m.req("tiles")
  if tiles.kind != JArray: raise fmtError(feJson, "tiles: expected an array")
  for t in tiles:
    let tx = t.get("tx", int32)
    let ty = t.get("ty", int32)
    let hash = t.get("hash", string)
    proc fits(i: int32): bool = int64(i) * TileSize >= int64(low(int32)) and (int64(i) + 1) * TileSize <= int64(high(int32))
    if not fits(tx) or not fits(ty): raise corrupt("tile (" & $tx & ", " & $ty & ") is outside the coordinate range")
    let c = tileCoord(tx, ty)
    if c in result.tiles: raise corrupt("duplicate tile (" & $tx & ", " & $ty & ")")
    let key = (f.sample, hash)
    if key in ld.tiles:
      let tile = ld.tiles[key]
      if tile.data.len != len: raise corrupt("tile " & hash & " is referenced with different sizes")
      result.tiles[c] = tile
      continue
    let tile = Tile(data: ld.fetchTile(hash, len))
    result.tiles[c] = tile
    ld.tiles[key] = tile

proc optSurface(ld: var Loader; n: JsonNode): Option[Surface] =
  if n.kind == JNull: none(Surface) else: some(ld.surface(n))

proc mask(ld: var Loader; m: JsonNode): LayerMask =
  LayerMask(surface: ld.surface(m.req("surface")), enabled: m.get("enabled", bool), linked: m.get("linked", bool),
            density: m.get("density", float32), feather: m.get("feather", float32))

proc effects(ld: var Loader; m: JsonNode): Effects =
  Effects(enabled: m.get("enabled", bool), items: m.get("items", seq[Effect]), psdRaw: ld.optBlob(m.field("psd_raw")),
          reference: m.getOr("reference", none((float64, float64))))

proc layerId(ld: var Loader; raw: uint64): LayerId =
  result = (if ld.preserveIds: LayerId(raw) else: freshLayerId())
  ld.idMap[raw] = result

proc contentsId(ld: var Loader; raw: Option[uint64]; source: JsonNode): SmartContentsId =
  if raw.isSome:
    if raw.get notin ld.contentsIdMap:
      ld.contentsIdMap[raw.get] = (if ld.preserveIds: SmartContentsId(raw.get) else: freshContentsId())
    return ld.contentsIdMap[raw.get]
  if source.field("kind").getStr == "linked":
    let path = source.field("path").getStr
    if path.len > 0:
      if path notin ld.legacyLinked: ld.legacyLinked[path] = freshContentsId()
      return ld.legacyLinked[path]
  freshContentsId()

proc tooDeep(): ref FormatError = limit("layer groups nested deeper than " & $MaxGroupDepth)

proc layer(ld: var Loader; m: JsonNode; depth: int): Layer =
  if depth > MaxGroupDepth: raise tooDeep()
  let content = m.req("content")
  let kind = content.get("kind", string)
  var lc: LayerContent
  case kind
  of "raster": lc = rasterContent(ld.surface(content.req("surface")))
  of "group":
    var kids: seq[Layer]
    let arr = content.req("children")
    if arr.kind != JArray: raise fmtError(feJson, "children: expected an array")
    for ch in arr: kids.add ld.layer(ch, depth + 1)
    lc = LayerContent(kind: lkGroup, group: Group(children: kids, expanded: content.get("expanded", bool),
                                                  artboard: content.getOr("artboard", none(Artboard))))
  of "adjustment": lc = LayerContent(kind: lkAdjustment, adjustment: content.get("adjustment", Adjustment))
  of "fill": lc = LayerContent(kind: lkFill, fill: content.get("fill", Fill))
  of "text":
    var t = TextLayer(text: content.get("text", string), fontFamily: content.get("font_family", string),
                      sizePt: content.get("size_pt", float32), color: content.get("color", Color),
                      transform: content.get("transform", Affine))
    t.cache = ld.optSurface(content.field("cache"))
    t.psdRaw = ld.optBlob(content.field("psd_raw"))
    t.runs = content.getOr("runs", newSeq[TextRun]())
    t.paragraphs = content.getOr("paragraphs", newSeq[ParagraphRun]())
    t.shape = content.getOr("shape", t.shape)
    t.orientation = content.getOr("orientation", t.orientation)
    t.antialias = content.getOr("antialias", t.antialias)
    t.warp = content.getOr("warp", none(TextWarp))
    lc = LayerContent(kind: lkText, text: t)
  of "shape":
    var s = ShapeLayer(fill: content.getOr("fill", none(Fill)))
    s.cache = ld.optSurface(content.field("cache"))
    s.psdRaw = ld.optBlob(content.field("psd_raw"))
    s.path = content.getOr("path", s.path)
    s.stroke = content.getOr("stroke", none(ShapeStroke))
    s.live = content.getOr("live", none(LiveShape))
    lc = LayerContent(kind: lkShape, shape: s)
  of "smart":
    let source = content.req("source")
    var so = SmartObject(contentsId: ld.contentsId(content.getOr("contents_id", none(uint64)), source),
                         transform: content.get("transform", Affine),
                         smartFilters: content.get("smart_filters", seq[SmartFilter]))
    case source.get("kind", string)
    of "embedded":
      so.source = SmartSource(kind: ssEmbedded, fileName: source.get("file_name", string),
                              bytes: ld.fetchBlob(source.get("blob", string)))
    of "linked": so.source = SmartSource(kind: ssLinked, path: source.get("path", string))
    else: raise fmtError(feJson, "unknown smart object source kind")
    so.cache = ld.optSurface(content.field("cache"))
    so.psdRaw = ld.optBlob(content.field("psd_raw"))
    so.filtersEnabled = content.getOr("filters_enabled", true)
    let fm = content.field("filter_mask")
    if fm.kind != JNull: so.filterMask = some(ld.mask(fm))
    so.warp = content.getOr("warp", none(Warp))
    if so.warp.isSome and so.warp.get.mesh.isSome and not so.warp.get.mesh.get.isValid: so.warp = none(Warp)
    so.stackMode = content.getOr("stack_mode", none(StackMode))
    let persp = content.getOr("perspective", none(array[9, float64]))
    if persp.isSome:
      var ok = true
      for v in persp.get:
        if v != v or v == Inf or v == NegInf: ok = false
      if ok: so.perspective = persp
    lc = LayerContent(kind: lkSmart, smart: so)
  else: raise fmtError(feJson, "unknown layer content kind `" & kind & "`")
  # Built directly (not `newLayer`): its id comes from the file, so none is minted here.
  result = Layer(name: m.get("name", string), visible: true, blend: bmNormal, opacity: 1, fillOpacity: 1,
                 effects: newEffects(), label: lcNone, content: lc, advanced: defaultAdvancedBlending())
  let mm = m.field("mask")
  if mm.kind != JNull: result.mask = some(ld.mask(mm))
  let fc = m.field("fill_cache")
  if fc.kind != JNull:
    result.fillCache = some(FillCache(fill: fc.get("fill", Fill), surface: ld.surface(fc.req("surface"))))
  let blocks = m.req("psd_blocks")
  if blocks.kind != JArray: raise fmtError(feJson, "psd_blocks: expected an array")
  for b in blocks:
    if b.kind != JArray or b.len != 2: raise fmtError(feJson, "psd_blocks: expected [key, hash] pairs")
    result.psdBlocks.add PsdBlock(key: key4(b[0].getStr), data: ld.fetchBlob(b[1].getStr))
  result.id = ld.layerId(m.get("id", uint64))
  result.visible = m.get("visible", bool)
  result.locks = m.get("locks", Locks)
  result.blend = m.get("blend", BlendMode)
  result.opacity = m.get("opacity", float32)
  result.fillOpacity = m.get("fill_opacity", float32)
  result.clipped = m.get("clipped", bool)
  result.vectorMask = m.getOr("vector_mask", none(VectorMask))
  result.effects = ld.effects(m.req("effects"))
  result.label = m.get("label", LabelColor)
  result.psdId = m.getOr("psd_id", none(uint32))
  result.linkGroup = m.getOr("link_group", none(uint64))
  result.excludedChannels = m.getOr("excluded_channels", 0'u32)
  result.blendIf = m.getOr("blend_if", BlendIf())
  result.advanced = m.getOr("advanced", defaultAdvancedBlending())
  let v = m.field("video")
  if v.kind != JNull:
    var vd = VideoData(source: v.get("source", VideoSource), fps: v.get("fps", float32),
                       showAltered: v.getOr("show_altered", false))
    for f in v.req("frames"): vd.frames.add ld.surface(f)
    let orig = v.field("original_frames")
    if orig.kind == JArray:
      if orig.len != vd.frames.len: raise corrupt("video original frame count differs from edited frame count")
      var frames: seq[Surface]
      for f in orig: frames.add ld.surface(f)
      vd.originalFrames = some(frames)
    result.video = some(vd)

proc channel(ld: var Loader; c: JsonNode): AlphaChannel =
  AlphaChannel(name: c.get("name", string), surface: ld.surface(c.req("surface")),
               spot: c.getOr("spot", none((Color, float32))), color: c.getOr("color", AlphaDefaultColor),
               opacity: c.getOr("opacity", 0.5'f32), indicates: c.getOr("indicates", ciMaskedAreas))

proc comp(ld: var Loader; m: JsonNode): LayerComp =
  result = LayerComp(id: m.get("id", uint32), name: m.get("name", string), comment: m.getOr("comment", ""),
                     applyVisibility: m.get("apply_visibility", bool), applyPosition: m.get("apply_position", bool),
                     applyAppearance: m.get("apply_appearance", bool))
  for s in m.field("states"):
    let raw = s.get("layer", uint64)
    let layer = (if raw in ld.idMap: ld.idMap[raw] else: LayerId(high(uint64) - min(raw, high(uint64) div 2)))
    var st = CompLayerState(layer: layer, visible: s.getOr("visible", none(bool)),
                            position: s.getOr("position", none((int, int))))
    let a = s.field("appearance")
    if a.kind != JNull:
      st.appearance = some(CompAppearance(blend: a.get("blend", BlendMode), opacity: a.get("opacity", float32),
                                          fillOpacity: a.get("fill_opacity", float32), effects: ld.effects(a.req("effects"))))
    result.states.add st

proc reserveIdsThrough(maxId: uint64): bool =
  let probe = uint64(freshLayerId())
  if maxId < probe: return true
  if maxId - probe > 10_000_000: return false
  ensureIdsAbove(maxId)
  true

proc maxIds(layers: JsonNode; depth: int; m: var uint64) =
  if layers.kind != JArray: return
  if depth > MaxGroupDepth and layers.len > 0: raise tooDeep()
  for l in layers:
    let id = l.field("id")
    if id.kind == JInt: m = max(m, uint64(id.getBiggestInt))
    let c = l.field("content")
    case c.field("kind").getStr
    of "group": maxIds(c.field("children"), depth + 1, m)
    of "smart":
      let ci = c.field("contents_id")
      if ci.kind == JInt: m = max(m, uint64(ci.getBiggestInt))
    else: discard

proc document(ld: var Loader; m: JsonNode): Document =
  if ld.preserveIds:
    var mx = m.get("id", uint64)
    maxIds(m.field("layers"), 0, mx)
    ld.preserveIds = reserveIdsThrough(mx)
  result.name = m.get("name", string)
  result.size = m.get("size", Size)
  result.resolutionDpi = m.get("resolution_dpi", float32)
  result.mode = m.get("mode", ColorMode)
  result.depth = m.get("depth", SampleType)
  result.iccProfile = ld.optBlob(m.field("icc_profile"))
  for l in m.req("layers"): result.layers.add ld.layer(l, 0)
  for c in m.req("channels"): result.channels.add ld.channel(c)
  result.guides = m.get("guides", Guides)
  result.selection = ld.optSurface(m.field("selection"))
  let md = m.req("metadata")
  result.metadata.xmp = md.getOr("xmp", none(string))
  result.metadata.exif = ld.optBlob(md.field("exif"))
  result.metadata.text = md.getOr("text", newSeq[(string, string)]())
  for r in md.field("psd_resources"):
    if r.kind != JArray or r.len != 3: raise fmtError(feJson, "psd_resources: expected [id, name, hash]")
    var id: uint16
    parse(r[0], id)
    result.metadata.psdResources.add PsdResource(id: id, name: r[1].getStr, data: ld.fetchBlob(r[2].getStr))
  for g in md.field("psd_global_blocks"):
    if g.kind != JArray or g.len != 3: raise fmtError(feJson, "psd_global_blocks: expected [sig, key, hash]")
    result.metadata.psdGlobalBlocks.add PsdGlobalBlock(signature: key4(g[0].getStr), key: key4(g[1].getStr),
                                                        data: ld.fetchBlob(g[2].getStr))
  result.globalLight = m.get("global_light", GlobalLight)
  for p in m.field("paths"):
    result.paths.add NamedPath(name: p.get("name", string), path: p.get("path", Path), psdRaw: ld.optBlob(p.field("psd_raw")))
  result.workPath = m.getOr("work_path", none(Path))
  result.clippingPath = m.getOr("clipping_path", none(ClippingPath))
  let qm = m.field("quick_mask")
  if qm.kind != JNull: result.quickMask = some(ld.channel(qm))
  result.quickMaskUnselected = m.getOr("quick_mask_unselected", false)
  for p in m.field("patterns"):
    result.patterns.add Pattern(id: p.get("id", string), name: p.get("name", string), width: p.get("width", int),
                                height: p.get("height", int), surface: ld.surface(p.req("surface")))
  result.colorTable = m.getOr("color_table", none(ColorTable))
  result.duotone = m.getOr("duotone", none(Duotone))
  result.variables = m.getOr("variables", result.variables)
  result.timeline = m.getOr("timeline", none(Timeline))
  for c in m.field("layer_comps"): result.layerComps.add ld.comp(c)
  result.lastAppliedComp = m.getOr("last_applied_comp", none(uint32))
  let lds = m.field("last_document_state")
  if lds.kind != JNull: result.lastDocumentState = some(ld.comp(lds))
  result.measurement = m.getOr("measurement", result.measurement)
  result.notes = m.getOr("notes", result.notes)
  result.textStyles = m.getOr("text_styles", result.textStyles)
  result.slices = m.getOr("slices", result.slices)
  for sl in result.slices.list.mitems:
    if sl.layer.isSome:
      let raw = uint64(sl.layer.get)
      sl.layer = (if raw in ld.idMap: some(ld.idMap[raw]) else: none(LayerId))
      if sl.layer.isNone and sl.origin == soLayer: sl.origin = soUser
  result.id = (if ld.preserveIds: DocId(m.get("id", uint64)) else: freshDocId())

proc migrate(v: JsonNode): uint32 =
  let fv = v.field("format_version")
  if fv.kind != JInt: raise corrupt("manifest has no format_version")
  let found = fv.getBiggestInt
  if found < 0 or found > int64(high(uint32)): raise corrupt("format_version out of range")
  if found == 0: raise corrupt("format_version 0 is invalid")
  if uint32(found) > FormatVersion: raise tooNew(uint32(found), FormatVersion)
  # No migration steps exist yet (format 1 is the only version).
  uint32(found)

proc readManifestJson(src: ByteSource; opts: LoadOptions): JsonNode =
  let raw = src.get(ManifestName, opts.maxManifestBytes)
  var s = newString(raw.len)
  for i, b in raw: s[i] = char(b)
  checkManifestDepth(s)
  try: result = parseJson(s)
  except CatchableError as e: raise fmtError(feJson, e.msg)
  discard migrate(result)

proc admitCanvas(size: Size; opts: LoadOptions) =
  let (w, h) = (size.width, size.height)
  if w <= 0 or h <= 0: raise corrupt("canvas size " & $w & "x" & $h & " is empty")
  if w > opts.maxCanvasSide or h > opts.maxCanvasSide:
    raise limit("canvas " & $w & "x" & $h & " exceeds " & $opts.maxCanvasSide & " pixels per side")
  if size.area > opts.maxCanvasPixels:
    raise limit("canvas " & $w & "x" & $h & " exceeds " & $opts.maxCanvasPixels & " pixels")

proc load*(src: ByteSource; opts = defaultLoadOptions()): Document =
  let m = readManifestJson(src, opts)
  let d = m.req("document")
  admitCanvas(d.get("size", Size), opts)
  var ld = Loader(src: src, opts: opts, preserveIds: opts.preserveIds)
  ld.document(d)

proc isPcraft*(bytes: openArray[byte]): bool =
  ## Whether `bytes` look like a `.pcraft` zip (a zip with `manifest.json`).
  if bytes.len < 4 or bytes[0] != byte('P') or bytes[1] != byte('K') or bytes[2] != 3 or bytes[3] != 4: return false
  if bytes.len >= 30:
    let nameLen = int(bytes[26]) or (int(bytes[27]) shl 8)
    if 30 + nameLen <= bytes.len and nameLen == ManifestName.len:
      var same = true
      for i in 0 ..< nameLen:
        if char(bytes[30 + i]) != ManifestName[i]: same = false
      if same: return true
  try: newZipReader(@bytes).find(ManifestName) >= 0
  except CatchableError: false

proc loadFromBytes*(bytes: seq[byte]; opts = defaultLoadOptions()): Document =
  load(newZipSource(bytes), opts)

proc loadPath*(path: string; opts = defaultLoadOptions()): Document =
  ## Loads a `.pcraft` zip file or bundle directory.
  if '\0' in path: raise fmtError(feIo, path & ": " & NulPathMsg)
  if dirExists(path): load(DirSource(root: path), opts)
  else: loadFromBytes(readFileBytes(path), opts)

proc readManifest*(bytes: seq[byte]): JsonNode =
  ## The (migrated) manifest JSON of a `.pcraft` zip.
  readManifestJson(newZipSource(bytes), defaultLoadOptions())

proc readThumbnail*(bytes: seq[byte]): Option[seq[byte]] =
  let src = newZipSource(bytes)
  let m = readManifestJson(src, defaultLoadOptions())
  let t = m.field("thumbnail")
  if t.kind == JString: some(src.get(t.getStr, 64 shl 20)) else: none(seq[byte])

# ---------------------------------------------------------------- crash recovery

type
  RecoveryInfo* = object
    key*: string
    documentName*: string
    originalPath*: Option[string]
    savedAt*: uint64
    revision*: uint64
  RecoveryEntry* = object
    info*: RecoveryInfo
    bundle*: string
  Autosaver* = ref object
    ## Autosaves one document into `<dir>/<key>.pcraft/` plus a `<key>.json` sidecar.
    dir*, key*: string
    writer: PcraftWriter
    last: Option[(bool, SaveStats, string)]
    completed: seq[(uint64, bool, string)]
    stopped: bool
  RecoveryStore* = ref object
    dir*: string
    session: string
    savers: Table[uint64, Autosaver]
    adopted: Table[uint64, string]

proc sanitize(key: string): string =
  for c in key: result.add (if c in {'a' .. 'z', 'A' .. 'Z', '0' .. '9', '-', '_'}: c else: '_')
  if result.len == 0: result = "untitled"

proc newAutosaver*(dir, key: string): Autosaver =
  Autosaver(dir: dir, key: sanitize(key), writer: newPcraftWriter())

proc bundlePath*(a: Autosaver): string = a.dir / (a.key & ".pcraft")

proc requestChecked*(a: Autosaver; snapshot: Document; revision: uint64; originalPath = none(string);
                     opts = SaveOptions()): (bool, string) =
  ## Saves the snapshot now (the Nim port has no background thread); returns (ok, error).
  if a.stopped: return (false, "autosave worker is stopped")
  let info = RecoveryInfo(key: a.key, documentName: snapshot.name, originalPath: originalPath,
                          savedAt: uint64(max(getTime().toUnix, 0)), revision: revision)
  try:
    createDir(a.bundlePath)
    let stats = a.writer.saveDir(snapshot, a.bundlePath, opts)
    atomicWrite(a.dir / (a.key & ".json"), (toJ(info).pretty(2)).toBytes)
    a.last = some((true, stats, ""))
    a.completed.add (revision, true, "")
    (true, "")
  except CatchableError as e:
    a.last = some((false, SaveStats(), e.msg))
    a.completed.add (revision, false, e.msg)
    (true, "")

proc request*(a: Autosaver; snapshot: Document; revision: uint64; originalPath = none(string); opts = SaveOptions()) =
  discard a.requestChecked(snapshot, revision, originalPath, opts)

proc takeCompleted*(a: Autosaver): seq[(uint64, bool, string)] =
  result = move a.completed
  a.completed = @[]

proc lastResult*(a: Autosaver): Option[(bool, SaveStats, string)] = a.last

proc removeEntry(dir, key: string) =
  let bundle = dir / (key & ".pcraft")
  try:
    if dirExists(bundle): removeDir(bundle)
    let sidecar = dir / (key & ".json")
    if fileExists(sidecar): removeFile(sidecar)
  except OSError as e: raise fmtError(feIo, e.msg)

proc discardAutosave*(a: Autosaver) =
  a.stopped = true
  removeEntry(a.dir, a.key)

proc listRecovery*(dir: string): seq[RecoveryEntry] =
  ## Recoverable autosaves in `dir`, newest first.
  if not dirExists(dir): return
  for kind, p in walkDir(dir):
    if kind != pcFile or not p.endsWith(".json"): continue
    var info: RecoveryInfo
    try:
      let j = parseFile(p)
      fromJ(j, info)
    except CatchableError: continue
    if sanitize(info.key) != info.key or splitFile(p).name != info.key: continue
    let bundle = dir / (info.key & ".pcraft")
    if fileExists(bundle / ManifestName): result.add RecoveryEntry(info: info, bundle: bundle)
  result.sort(proc(a, b: RecoveryEntry): int =
    result = cmp(b.info.savedAt, a.info.savedAt)
    if result == 0: result = cmp(a.info.key, b.info.key))

proc recover*(e: RecoveryEntry): Document = loadPath(e.bundle)

proc discardRecovery*(dir: string; e: RecoveryEntry) = removeEntry(dir, e.info.key)

proc newRecoveryStore*(dir: string): RecoveryStore =
  RecoveryStore(dir: dir, session: toHex(getTime().toUnix) & "_" & $getCurrentProcessId())

proc recoverAll*(s: RecoveryStore): seq[(RecoveryEntry, Document)] =
  for e in listRecovery(s.dir):
    try: result.add (e, recover(e))
    except CatchableError: discard

proc adopt*(s: RecoveryStore; docId: uint64; key: string) = s.adopted[docId] = sanitize(key)

proc autosave*(s: RecoveryStore; d: Document; revision: uint64; originalPath = none(string)): (bool, string) =
  let id = uint64(d.id)
  if id notin s.savers:
    let key = (if id in s.adopted: s.adopted[id] else: "doc-" & s.session & "-" & $id)
    s.savers[id] = newAutosaver(s.dir, key)
  s.savers[id].requestChecked(d, revision, originalPath)

proc discardDoc*(s: RecoveryStore; docId: uint64) =
  if docId in s.savers:
    s.savers[docId].discardAutosave()
    s.savers.del docId
  if docId in s.adopted:
    removeEntry(s.dir, s.adopted[docId])
    s.adopted.del docId
