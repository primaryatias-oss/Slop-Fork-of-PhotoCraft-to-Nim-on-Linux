## Document → PSD/PSB (Rust `psd_export.rs`, plus the Multichannel half of
## `multichannel_map.rs`).
##
## Every layer becomes a record with its pixels (or rendered content for fills, text, shapes and
## smart objects), masks, blending options and the tagged blocks that describe its live content;
## blocks imported from a PSD and not regenerated are written back verbatim. The merged image is
## the CPU compositor's result. Smart-object sources are embedded as linked-file entries. Type
## layers write their `TySh` (generated for layers created here, style sheets regenerated from the
## document's styles) and the document's `Txt2` is regenerated from the type layers.

import std/[options, tables, sets, math, algorithm, unicode, strutils, sequtils]
import ../geom, ../color, ../raster, ../doc, ../psd, ../compose, ../format, ../blake3
import ../codecs/meta
import ../compose/gradient as cgradient
import ../color/convert as cconvert
import ../algo/[transform, warp]
import common, pixels, blocks, adjust_map, effects_map, channel_map, vector_map, smart_map, comps_map,
       pattern_map, slices_map, text_styles_map, annotations_map, linked
import psd_import
import ../text/psd_text

const
  PsdMaxFileSize = 2'u64 * 1024 * 1024 * 1024
  PsdEstimateFixedOverhead = 1024'u64 * 1024
  PsdEstimateLayerOverhead = 64'u64 * 1024
  MaxNesting = 8
  MergedWeight = 2.0'f32
  MergedRendered = 0.8'f32
  LinkKeys = ["lnk2", "lnk3", "lnkD", "lnkE"]
  FxKeys = ["FEid", "FXid"]

type
  PsdExportOptions* = object
    forcePsb*, mergedMatte*: bool
    mergedCompression*: Compression

  Source = object
    uuid: string
    size: (float64, float64)
    dpi: float64

  SourceResult = object
    ok: bool
    src: Source
    err: string

  Composite = object
    present: bool
    surface: Surface
    bounds: Rect

  SmartOut = object
    depth: int
    globals: seq[PsdGlobalBlock]
    known, used: HashSet[string]
    pruneOk: bool
    add: seq[LinkedFile]
    converted: Table[(uint64, string), SourceResult]
    placedUsed: HashSet[string]
    placedOwners: Table[string, LayerId]
    unplacedOwners: Table[uint64, LayerId]
    linkedContents: Table[string, SmartContentsId]
    linkedCopies: Table[(string, uint64), PsdGlobalBlock]
    docs: Table[string, Document]
    composites: Table[string, Composite]
    oldFx: seq[FilterEffectsItem]
    keepFx: HashSet[string]
    newFx: seq[FilterEffectsItem]

  Raw = seq[(string, seq[byte])]

  Ex = object
    fmt, maskFmt: PixelFormat
    dpi: float32
    textStyles: TextStyles
    cc: int
    cmyk: bool
    version: Version
    nextId: uint32
    layerIds: Table[LayerId, uint32]
    textIndex: Table[LayerId, int32]
    canvas: Rect
    records: seq[LayerRecord]
    warnings: seq[string]
    guides: Guides
    hasComps: bool
    comps: seq[LayerComp]
    lastComp: Option[LayerComp]
    smart: SmartOut
    patterns: seq[Pattern]

var importHook*: proc(name: string; bytes: seq[byte]): Document = nil
  ## Full-format import (installed by the io facade) used to measure non-PSD smart-object sources.

proc defaultPsdExportOptions*(): PsdExportOptions =
  PsdExportOptions(forcePsb: false, mergedMatte: true, mergedCompression: cRle)

proc q255(v: float32): uint8 = uint8(round(clamp(v, 0'f32, 1'f32) * 255))

proc asU32(v: float64): uint32 =
  if v != v or v <= 0: 0'u32
  elif v >= 4294967295.0: high(uint32)
  else: uint32(v)

proc fmtDebug(f: PixelFormat): string =
  "PixelFormat { mode: " & $f.mode & ", sample: " & $f.sample & ", alpha: " & $f.alpha & " }"

proc beBytes32(v: int32): seq[byte] =
  let u = cast[uint32](v)
  @[byte(u shr 24), byte((u shr 16) and 0xff), byte((u shr 8) and 0xff), byte(u and 0xff)]

proc putBeU32(o: var seq[byte]; v: uint32) =
  for s in [24, 16, 8, 0]: o.add byte((v shr s) and 0xff)

proc putBeU16(o: var seq[byte]; v: uint16) =
  o.add byte(v shr 8)
  o.add byte(v and 0xff)

proc putBeF64(o: var seq[byte]; v: float64) =
  let u = cast[uint64](v)
  for s in [56, 48, 40, 32, 24, 16, 8, 0]: o.add byte((u shr s) and 0xff)

proc withSpan*(ctl: Interrupt; lo, hi: float32): Interrupt =
  ## An interrupt reporting progress inside `[lo, hi]` of `ctl`'s range.
  newInterrupt(proc(): bool = ctl.cancelled,
               proc(p: float32) = ctl.progress(lo + (hi - lo) * clamp(p, 0'f32, 1'f32)))

# ---------------------------------------------------------------- names and resources

proc legacyName*(s: string): seq[byte] =
  for r in s.runes:
    if result.len >= 255: break
    let c = int(r)
    result.add (if c < 128 and c >= 32 and c != 127: byte(c) else: byte('?'))

proc unicodeNamesResource*(names: openArray[string]): seq[byte] =
  for n in names:
    var units = utf16Units(n)
    units.add 0
    result.putBeU32(uint32(units.len))
    for u in units: result.putBeU16(u)

proc pascalNamesResource*(names: openArray[string]): seq[byte] =
  for n in names:
    let b = legacyName(n)
    result.add byte(b.len)
    result.add b

proc guidesResource(d: Document): seq[byte] =
  result.putBeU32(1)
  result.putBeU32(576)
  result.putBeU32(576)
  result.putBeU32(uint32(d.guides.horizontal.len + d.guides.vertical.len))
  for (list, dir) in [(d.guides.vertical, 0'u8), (d.guides.horizontal, 1'u8)]:
    for p in list:
      result.putBeU32(cast[uint32](int32(asI32(round(p * 32.0)))))
      result.add dir

proc linkGroupResource(layers: openArray[Layer]): Option[seq[byte]] =
  var per: seq[Option[uint64]]
  proc walk(layers: openArray[Layer]; o: var seq[Option[uint64]]) =
    for l in layers:
      if l.content.kind == lkGroup:
        o.add none(uint64)
        walk(l.content.group.children, o)
      o.add l.linkGroup
  walk(layers, per)
  var anySome = false
  var fits = true
  for g in per:
    if g.isSome:
      anySome = true
      if g.get < 1 or g.get > 65535: fits = false
  if not anySome: return none(seq[byte])
  var seen: seq[uint64]
  var o: seq[byte]
  for g in per:
    var id = 0'u16
    if g.isSome:
      if fits: id = uint16(g.get)
      else:
        var i = seen.find(g.get)
        if i < 0:
          seen.add g.get
          i = seen.len - 1
        id = uint16(min(i + 1, 65535))
    o.putBeU16(id)
  some(o)

proc smartKeys(data: SharedBytes): seq[string] =
  if not data.isNil and data.data.len >= 4:
    let k = cast[string](data.data[0 ..< 4])
    if k == "plcL": return @["PlLd"]
    if k == "soLD": return @["SoLd", "SoLE"]
  @["SoLd", "PlLd", "SoLE"]

proc effectsUnchanged(l: Layer): Option[bool] =
  var src: seq[byte]
  var found = false
  for b in l.psdBlocks:
    if b.key == "lmfx":
      src = b.data.data
      found = true
      break
  if not found:
    if l.effects.psdRaw.isNil: return none(bool)
    src = l.effects.psdRaw.data
  let p = parseLfx2(src)
  if p.isNone: return none(bool)
  some(p.get[0] == l.effects.enabled and p.get[1] == l.effects.items)

proc maskParameters(user, vector: Option[(float32, float32)]): Option[MaskParameters] =
  proc density(v: Option[(float32, float32)]): Option[uint8] =
    if v.isSome and v.get[0] < 1: some(q255(v.get[0])) else: none(uint8)
  proc feather(v: Option[(float32, float32)]): Option[float64] =
    if v.isSome and v.get[1] != 0: some(float64(v.get[1])) else: none(float64)
  var p = MaskParameters(userDensity: density(user), userFeather: feather(user),
                         vectorDensity: density(vector), vectorFeather: feather(vector))
  let flags = uint8(ord(p.userDensity.isSome)) or (uint8(ord(p.userFeather.isSome)) shl 1) or
              (uint8(ord(p.vectorDensity.isSome)) shl 2) or (uint8(ord(p.vectorFeather.isSome)) shl 3)
  if flags == 0: return none(MaskParameters)
  p.flags = flags
  some(p)

proc upsert(raw: var Raw; key: string; data: seq[byte]; before: Option[string]) =
  for e in raw.mitems:
    if e[0] == key:
      e[1] = data
      return
  if before.isSome:
    for i, e in raw:
      if e[0] == before.get:
        raw.insert((key, data), i)
        return
  raw.add (key, data)

proc rawHas(raw: Raw; key: string): bool =
  for e in raw:
    if e[0] == key: return true

proc rawPos(raw: Raw; key: string): int =
  for i, e in raw:
    if e[0] == key: return i
  -1

proc toPsdRect(r: Rect): PsdRect = PsdRect(top: int32(r.y0), left: int32(r.x0), bottom: int32(r.y1), right: int32(r.x1))

proc psdMode(m: ColorMode): PsdColorMode =
  case m
  of cmGrayscale: pcmGrayscale
  of cmCmyk: pcmCmyk
  of cmLab: pcmLab
  else: pcmRgb

proc contentsUuid(contentsId: SmartContentsId; bytes: openArray[byte]): string =
  var h = newBlake3()
  var le: array[8, byte]
  let v = uint64(contentsId)
  for i in 0 ..< 8: le[i] = byte((v shr (8 * i)) and 0xff)
  h.update(le)
  h.update(bytes)
  uuidFrom(h.finish())

proc paddedFx(fx: FilterEffects): seq[byte] =
  result = fx.toBytes
  while result.len mod 4 != 0: result.add 0

# ---------------------------------------------------------------- size estimate

proc estimateEncodedSize(rawBytes, rows, structural: uint64): Option[uint64] =
  # Saturation stands in for Rust's checked arithmetic: anything this large is over the limit.
  let lim = high(uint64) div 4
  if rawBytes > lim or rows > lim div 8 or structural > lim: return none(uint64)
  some(rawBytes + (rawBytes + 63) div 64 + rows * 8 + structural)

proc addPlaneEstimate(rawBytes, rows: var uint64; width, height, channels, bps: uint64): bool =
  let lim = high(uint64) div 16
  if width > 0 and height > lim div width: return false
  let wh = width * height
  if channels > 0 and wh > lim div channels: return false
  let plane = wh * channels
  if bps > 0 and plane > lim div bps: return false
  rawBytes += plane * bps
  rows += height * channels
  rawBytes < lim and rows < lim

proc surfaceDims(s: Surface): (uint64, uint64) =
  let b = s.contentBounds
  (uint64(b.width), uint64(b.height))

proc estimatePsdSize*(d: Document): Option[uint64] =
  var rawBytes, rows = 0'u64
  var structural = PsdEstimateFixedOverhead
  let width = uint64(max(d.size.width, 0))
  let height = uint64(max(d.size.height, 0))
  if d.mode == cmMultichannel:
    let channels = uint64(clamp(d.channels.len, 1, 56))
    let sb = uint64(if d.depth == stF32: stU16.bytes else: d.depth.bytes)
    if not addPlaneEstimate(rawBytes, rows, width, height, channels, sb): return none(uint64)
  else:
    let format = d.pixelFormat
    let colorChannels = uint64(format.mode.colorChannels)
    let bps = uint64(format.sample.bytes)
    let layerChannels = colorChannels + 1
    for (_, _, layer) in d.walk:
      structural += PsdEstimateLayerOverhead + uint64(layer.name.len)
      for b in layer.psdBlocks:
        if not b.data.isNil: structural += uint64(b.data.data.len)
      if layer.content.kind == lkFill:
        var (fw, fh) = (width, height)
        if layer.fillCache.isSome and layer.fillCache.get.fill == layer.content.fill:
          let (cw, ch) = surfaceDims(layer.fillCache.get.surface)
          fw = max(fw, cw)
          fh = max(fh, ch)
        if not addPlaneEstimate(rawBytes, rows, fw, fh, layerChannels, bps): return none(uint64)
      elif layer.hasSurface:
        let (sw, sh) = surfaceDims(layer.surface)
        if not addPlaneEstimate(rawBytes, rows, sw, sh, layerChannels, bps): return none(uint64)
      if layer.mask.isSome:
        let (mw, mh) = surfaceDims(layer.mask.get.surface)
        if not addPlaneEstimate(rawBytes, rows, mw, mh, 1, bps): return none(uint64)
      if layer.content.kind == lkSmart and layer.content.smart.source.kind == ssEmbedded and
         not layer.content.smart.source.bytes.isNil:
        structural += uint64(layer.content.smart.source.bytes.data.len)
    let maxExtra = max(56 - (format.mode.colorChannels + 1), 0)
    var extraChannels = min(d.channels.len, maxExtra)
    if d.quickMask.isSome and extraChannels < maxExtra: inc extraChannels
    let mergedChannels = colorChannels + 1 + uint64(extraChannels)
    if not addPlaneEstimate(rawBytes, rows, width, height, mergedChannels, bps): return none(uint64)
    for i in 0 ..< min(extraChannels, d.channels.len):
      structural += uint64(d.channels[i].name.len)
  if not d.iccProfile.isNil: structural += uint64(d.iccProfile.data.len)
  if d.metadata.xmp.isSome: structural += uint64(d.metadata.xmp.get.len)
  if not d.metadata.exif.isNil: structural += uint64(d.metadata.exif.data.len)
  for r in d.metadata.psdResources:
    structural += uint64(r.name.len) + (if r.data.isNil: 0'u64 else: uint64(r.data.data.len))
  for b in d.metadata.psdGlobalBlocks:
    if not b.data.isNil: structural += uint64(b.data.data.len)
  estimateEncodedSize(rawBytes, rows, structural)

proc psdSizeExceedsLimit*(d: Document): bool =
  let e = estimatePsdSize(d)
  e.isNone or e.get > PsdMaxFileSize

proc psdVersion*(d: Document; forcePsb, sizeExceedsLimit: bool): Version =
  if forcePsb or d.size.width > 30_000 or d.size.height > 30_000 or sizeExceedsLimit: vPsb else: vPsd

# ---------------------------------------------------------------- smart objects

proc newSmartOut(d: Document; depth: int): SmartOut =
  result = SmartOut(depth: depth, globals: d.metadata.psdGlobalBlocks, pruneOk: true)
  for b in result.globals:
    if b.key in LinkKeys and not b.data.isNil:
      for u in blockUuids(b.data.data): result.known.incl u
  for b in result.globals:
    if b.key in FxKeys and not b.data.isNil:
      try:
        let fx = parseFilterEffects(b.data.data)
        result.oldFx.add fx.items
      except CatchableError: discard
  var layers = d.walk
  layers.sort(proc(a, b: (LayerPath, int, Layer)): int =
    result = cmp(a[2].psdId.isNone, b[2].psdId.isNone)
    if result == 0: result = cmp(uint64(a[2].id), uint64(b[2].id)))
  for (_, _, layer) in layers:
    if layer.content.kind != lkSmart: continue
    let sm = layer.content.smart
    if sm.source.kind != ssLinked or sm.psdRaw.isNil: continue
    let placed = parseSold(sm.psdRaw.data)
    if placed.isNone or placed.get.idnt != sm.source.path: continue
    let path = sm.source.path
    if path notin result.linkedContents: result.linkedContents[path] = sm.contentsId
    if placed.get.placed.len == 0:
      if uint64(sm.contentsId) notin result.unplacedOwners: result.unplacedOwners[uint64(sm.contentsId)] = layer.id
    elif placed.get.placed notin result.placedOwners:
      result.placedOwners[placed.get.placed] = layer.id

proc finish(s: SmartOut; blocks: seq[PsdGlobalBlock]): seq[PsdGlobalBlock] =
  proc keep(u: string): bool = not s.pruneOk or u in s.used
  var added = false
  var fxPlaced = false
  for b in blocks:
    if b.key in LinkKeys:
      let add = if not added and b.key == "lnk2": s.add else: @[]
      if b.key == "lnk2": added = true
      let rebuilt = rebuildBlock(if b.data.isNil: @[] else: b.data.data, keep, add)
      if rebuilt.isNone: result.add b
      elif rebuilt.get.len == 0: discard
      else: result.add PsdGlobalBlock(signature: b.signature, key: b.key, data: shared(rebuilt.get))
    elif b.key in FxKeys:
      var fx: FilterEffects
      try: fx = parseFilterEffects(if b.data.isNil: @[] else: b.data.data)
      except CatchableError:
        result.add b
        continue
      let before = fx.items.len
      fx.items.keepItIf(it.id in s.keepFx)
      let fresh = not fxPlaced and s.newFx.len > 0
      if fresh:
        fx.items.add s.newFx
        fxPlaced = true
      if fx.items.len == before and not fresh: result.add b
      elif fx.items.len > 0:
        result.add PsdGlobalBlock(signature: b.signature, key: b.key, data: shared(paddedFx(fx)))
    else:
      result.add b
  if not added and s.add.len > 0:
    var data: seq[byte]
    for f in s.add: data.add encodeLinkedFile(f)
    result.add PsdGlobalBlock(signature: "8BIM", key: "lnk2", data: shared(data))
  var copyKeys: seq[(string, uint64)]
  for k in s.linkedCopies.keys: copyKeys.add k
  copyKeys.sort(proc(a, b: (string, uint64)): int =
    result = cmp(a[0], b[0])
    if result == 0: result = cmp(a[1], b[1]))
  for k in copyKeys:
    let c = s.linkedCopies[k]
    var merged = false
    for b in result.mitems:
      if b.signature == c.signature and b.key == c.key:
        b.data = shared((if b.data.isNil: @[] else: b.data.data) & c.data.data)
        merged = true
        break
    if not merged: result.add c
  if not fxPlaced and s.newFx.len > 0:
    let fx = FilterEffects(version: 3, items: s.newFx)
    result.add PsdGlobalBlock(signature: "8BIM", key: "FEid", data: shared(paddedFx(fx)))
    var hasFmsk = false
    for b in result:
      if b.key == "FMsk": hasFmsk = true
    if not hasFmsk:
      result.add PsdGlobalBlock(signature: "8BIM", key: "FMsk",
                                data: shared(@[0'u8, 0, 255, 255, 0, 0, 0, 0, 0, 0, 0, 50]))

proc sourceGeometry(name: string; bytes: seq[byte]): Option[((float64, float64), float64)] =
  if bytes.len >= 4 and cast[string](bytes[0 ..< 4]) == "8BPS":
    if bytes.len < 22: return none(((float64, float64), float64))
    let h = float64(beU32(bytes, 14))
    let w = float64(beU32(bytes, 18))
    return some(((w, h), 72.0))
  if importHook.isNil: return none(((float64, float64), float64))
  try:
    let d = importHook(name, bytes)
    some(((float64(d.size.width), float64(d.size.height)), float64(d.resolutionDpi)))
  except CatchableError: none(((float64, float64), float64))

proc sizeFromLayer(sm: SmartObject): (float64, float64) =
  if sm.cache.isNone: return (1.0, 1.0)
  let r = sm.cache.get.contentBounds
  let inv = sm.transform.inverse
  if r.isEmpty or inv.isNone: return (1.0, 1.0)
  let m = inv.get.m
  var (w, h) = (1.0, 1.0)
  for (x, y) in [(r.x0, r.y0), (r.x1, r.y0), (r.x1, r.y1), (r.x0, r.y1)]:
    let (fx, fy) = (float64(x), float64(y))
    w = max(w, round(m[0] * fx + m[2] * fy + m[4]))
    h = max(h, round(m[1] * fx + m[3] * fy + m[5]))
  (w, h)

proc documentToPsdNested(d: Document; opts: PsdExportOptions; depth: int; ctl: Interrupt): (PsdFile, seq[string])

proc convertSource(ex: var Ex; contentsId: SmartContentsId; fileName: string; bytes: seq[byte]): SourceResult =
  var name: string
  var data: seq[byte]
  var size: (float64, float64)
  var dpi: float64
  var nested = none(Document)
  if isPcraft(bytes):
    if ex.smart.depth >= MaxNesting:
      return SourceResult(err: "smart objects nest more than " & $MaxNesting & " deep")
    var sub: Document
    try: sub = loadFromBytes(bytes)
    except CatchableError as e: return SourceResult(err: "its contents can't be read: " & e.msg)
    var o = defaultPsdExportOptions()
    o.forcePsb = true
    let (file, warnings) = documentToPsdNested(sub, o, ex.smart.depth + 1, NoInterrupt)
    try: data = file.toBytes
    except CatchableError as e: return SourceResult(err: "its contents can't be written: " & e.msg)
    let dot = fileName.rfind('.')
    let stem = if dot >= 0: fileName[0 ..< dot] else: fileName
    for w in warnings: ex.warnings.add "smart object " & stem & ": " & w
    size = (float64(sub.size.width), float64(sub.size.height))
    name = stem & ".psb"
    dpi = float64(sub.resolutionDpi)
    nested = some(sub)
  else:
    let g = sourceGeometry(fileName, bytes)
    if g.isSome: (size, dpi) = g.get
    else: (size, dpi) = ((0.0, 0.0), float64(ex.dpi))
    name = fileName
    data = bytes
  let uuid = contentsUuid(contentsId, data)
  if nested.isSome: ex.smart.docs[uuid] = nested.get
  var known = uuid in ex.smart.known
  for f in ex.smart.add:
    if f.uuid == uuid: known = true
  if not known: ex.smart.add.add LinkedFile(uuid: uuid, fileName: name, bytes: data)
  SourceResult(ok: true, src: Source(uuid: uuid, size: size, dpi: dpi))

proc embed(ex: var Ex; contentsId: SmartContentsId; fileName: string; bytes: seq[byte]): SourceResult =
  let key = (uint64(contentsId), blake3Hex(bytes))
  if key in ex.smart.converted: return ex.smart.converted[key]
  result = ex.convertSource(contentsId, fileName, bytes)
  ex.smart.converted[key] = result

proc smartSource(ex: var Ex; sm: SmartObject; tmpl: Option[Placed]): SourceResult =
  case sm.source.kind
  of ssLinked:
    let path = sm.source.path
    if path in ex.smart.known or (tmpl.isSome and tmpl.get.idnt == path):
      var stored = none((float64, float64))
      if tmpl.isSome and tmpl.get.idnt == path: stored = storedSize(tmpl.get.descriptor)
      var geometry = none(((float64, float64), float64))
      if stored.isSome: geometry = some((stored.get, float64(ex.dpi)))
      else:
        let f = findLinkedFile(ex.smart.globals, path)
        if f.isSome: geometry = sourceGeometry(f.get.fileName, f.get.bytes)
      let (size, dpi) = if geometry.isSome: geometry.get else: (sizeFromLayer(sm), float64(ex.dpi))
      if path notin ex.smart.linkedContents: ex.smart.linkedContents[path] = sm.contentsId
      let original = ex.smart.linkedContents[path]
      var uuid: string
      if path.len == 0 or original == sm.contentsId: uuid = path
      else:
        uuid = contentsUuid(sm.contentsId, cast[seq[byte]](path))
        let key = (path, uint64(sm.contentsId))
        if key notin ex.smart.linkedCopies:
          var copy = none(PsdGlobalBlock)
          for b in ex.smart.globals:
            if b.key in LinkKeys and not b.data.isNil:
              let c = copyItem(b.data.data, path, uuid)
              if c.isSome:
                copy = some(PsdGlobalBlock(signature: b.signature, key: b.key, data: shared(c.get)))
                break
          if copy.isNone:
            return SourceResult(err: "the linked source " & path & " has no readable record to copy independently")
          ex.smart.linkedCopies[key] = copy.get
      SourceResult(ok: true, src: Source(uuid: uuid, size: size, dpi: dpi))
    else:
      when defined(js) or defined(emscripten):
        SourceResult(err: "the linked file " & path & " can't be read here")
      else:
        var bytes: seq[byte]
        try: bytes = cast[seq[byte]](readFile(path))
        except CatchableError as e:
          return SourceResult(err: "the linked file " & path & " can't be read: " & e.msg)
        var name = path
        let cut = max(path.rfind('/'), path.rfind('\\'))
        if cut >= 0: name = path[cut + 1 .. ^1]
        ex.warnings.add "the linked smart object " & name & " was embedded (PSD export keeps no external links)"
        ex.embed(sm.contentsId, name, bytes)
  of ssEmbedded:
    ex.embed(sm.contentsId, sm.source.fileName, if sm.source.bytes.isNil: @[] else: sm.source.bytes.data)

proc sourceComposite(ex: var Ex; uuid: string): Composite =
  if uuid in ex.smart.composites: return ex.smart.composites[uuid]
  var sub = none(Document)
  if uuid in ex.smart.docs: sub = some(ex.smart.docs[uuid])
  else:
    var file = none(LinkedFile)
    for f in ex.smart.add:
      if f.uuid == uuid:
        file = some(f)
        break
    if file.isNone:
      var all = ex.smart.globals
      for _, c in ex.smart.linkedCopies: all.add c
      file = findLinkedFile(all, uuid)
    if file.isSome and not importHook.isNil:
      try: sub = some(importHook(file.get.fileName, file.get.bytes))
      except CatchableError: discard
  var c = Composite()
  if sub.isSome:
    let d = sub.get
    var s = newSurface(ex.fmt)
    let fmt = ex.fmt
    try:
      renderBands(d, d.bounds, 0, proc(band: Buffer) =
        var vals = newSeqOfCap[float32](band.px.len * fmt.channels)
        var v: array[5, float32]
        for p in band.px:
          let n = fromRgbaInto(fmt, p, v)
          for i in 0 ..< n: vals.add v[i]
        s.writeRegion(band.rect, vals))
    except CatchableError: discard
    s.prune()
    c = Composite(present: true, surface: s, bounds: d.bounds)
  ex.smart.composites[uuid] = c
  c

proc placedUnchanged(ex: Ex; t: Placed; data: seq[byte]; sm: SmartObject; stack: FilterStack): bool =
  let tstack = if t.stack.isSome: t.stack.get else: defaultFilterStack()
  if t.transform != sm.transform or parsePlacedWarp("SoLd", data) != sm.warp or tstack != stack: return false
  if stack.filters.len == 0: return true
  for i in ex.smart.oldFx:
    if i.id == t.placed:
      try: return maskFromItem(i, ex.maskFmt.sample, stack) == sm.filterMask
      except CatchableError: return false
  sm.filterMask.isNone

proc smartBlocks(ex: var Ex; l: Layer; sm: SmartObject; tmpl: Option[Placed]; raw: var Raw) =
  let sr = ex.smartSource(sm, tmpl)
  if not sr.ok:
    raw.keepItIf(it[0] notin ["SoLd", "PlLd", "SoLE"])
    ex.warnings.add "layer \"" & l.name & "\": smart object written as pixels (" & sr.err & ")"
    return
  let src = sr.src
  ex.smart.used.incl src.uuid
  let stack = FilterStack(enabled: sm.filtersEnabled, filters: sm.smartFilters,
                          maskEnabled: sm.filterMask.isNone or sm.filterMask.get.enabled,
                          maskLinked: sm.filterMask.isSome and sm.filterMask.get.linked)
  let sameSource = if tmpl.isSome and tmpl.get.idnt == src.uuid: tmpl else: none(Placed)
  let unchanged = sameSource.isSome and not sm.psdRaw.isNil and
                  ex.placedUnchanged(sameSource.get, sm.psdRaw.data, sm, stack)
  var placed: string
  block choose:
    if sameSource.isSome:
      let t = sameSource.get
      if t.placed.len == 0 and unchanged and uint64(sm.contentsId) in ex.smart.unplacedOwners and
         ex.smart.unplacedOwners[uint64(sm.contentsId)] == l.id:
        placed = ""
        break choose
      if t.placed in ex.smart.placedOwners and ex.smart.placedOwners[t.placed] == l.id and
         not ex.smart.placedUsed.containsOrIncl(t.placed):
        placed = t.placed
        break choose
    let seed = src.uuid & ":" & $uint64(l.id)
    var suffix = 0'u64
    while true:
      let s = if suffix == 0: seed else: seed & ":" & $suffix
      let id = uuidFrom(cast[seq[byte]](s))
      if id notin ex.smart.placedOwners and not ex.smart.placedUsed.containsOrIncl(id):
        placed = id
        break
      inc suffix
  if sameSource.isSome and not sm.psdRaw.isNil and sameSource.get.placed == placed and unchanged:
    let keys = if raw.rawHas("SoLd"): @["SoLd"] else: @["SoLE", "SoLd"]
    var done = false
    for k in keys:
      let i = raw.rawPos(k)
      if i >= 0:
        raw[i][1] = sm.psdRaw.data
        done = true
        break
    if not done: raw.add ("SoLd", sm.psdRaw.data)
    if stack.filters.len > 0: ex.smart.keepFx.incl sameSource.get.placed
    return
  var size = src.size
  if sameSource.isSome:
    let st = storedSize(sameSource.get.descriptor)
    if st.isSome: size = st.get
  if not (size[0] > 0 and size[1] > 0): size = sizeFromLayer(sm)
  var warnings: seq[string]
  let fx = if stack.filters.len > 0: some(filterFx(stack, l.name, warnings)) else: none(Descriptor)
  let spec = PlacedSpec(idnt: src.uuid, placed: placed, transform: sm.transform, perspective: sm.perspective,
                        size: size, dpi: src.dpi, warp: sm.warp, filterFx: fx)
  let sold = soldBytes(if sameSource.isSome: some(sameSource.get.descriptor) else: none(Descriptor), spec, warnings)
  let plld = plldBytes(spec, warnings)
  var dedup: seq[string]
  for w in warnings:
    if dedup.len == 0 or dedup[^1] != w: dedup.add w
  let key = if sameSource.isSome and raw.rawHas("SoLE") and not raw.rawHas("SoLd"): "SoLE" else: "SoLd"
  raw.keepItIf(it[0] == key or it[0] notin ["SoLd", "SoLE"])
  upsert(raw, "PlLd", plld, some(key))
  upsert(raw, key, sold, none(string))
  if stack.filters.len > 0:
    let bounds = if sm.cache.isSome: sm.cache.get.contentBounds.union(ex.canvas) else: ex.canvas
    var item = none(FilterEffectsItem)
    if sm.stackMode.isNone:
      let c = ex.sourceComposite(src.uuid)
      if c.present:
        try:
          let unfiltered =
            if sm.perspective.isSome: placeSourceProjective(c.surface, c.bounds, homography(sm.perspective.get), sm.warp)
            else: placeSource(c.surface, c.bounds, sm.transform, sm.warp)
          item = feidItem(placed, unfiltered, sm.filterMask, bounds, ex.fmt)
        except AllocationError as e:
          dedup.add "smart filter mask was not re-rendered for PSD export (" & e.msg & ")"
    if item.isSome: ex.smart.newFx.add item.get
    elif sm.filterMask.isSome:
      ex.warnings.add "layer \"" & l.name & "\": the smart filter mask was not written to PSD"
  for w in dedup:
    ex.warnings.add (if w.startsWith("layer "): w else: "layer \"" & l.name & "\": " & w)

# ---------------------------------------------------------------- records

proc compression(ex: Ex): Compression = (if ex.fmt.sample == stF32: cZipPrediction else: cRle)

proc encode(ex: Ex; id: int16; plane: openArray[byte]; w, h: int): ChannelData =
  let depth = psdDepth(ex.fmt.sample)
  let c = if w == 0 or h == 0: cRaw else: ex.compression
  try: return encodeChannel(id, c, plane, w, h, depth, ex.version)
  except CatchableError: discard
  try: return encodeChannel(id, cRaw, plane, w, h, depth, ex.version)
  except CatchableError: discard
  ChannelData(id: id, compression: some(cRaw), data: @plane)

proc emptyChannels(ex: Ex): seq[ChannelData] =
  for id in -1 ..< ex.cc: result.add ex.encode(int16(id), [], 0, 0)

proc pixelChannels(ex: var Ex; s0: Surface; name: string): (PsdRect, seq[ChannelData]) =
  var s = s0
  if s0.format != ex.fmt:
    ex.warnings.add "layer \"" & name & "\": pixels converted from " & fmtDebug(s0.format) & " to " & fmtDebug(ex.fmt)
    s = s0.convert(ex.fmt)
  let r = s.contentBounds
  if r.isEmpty: return (PsdRect(), ex.emptyChannels)
  var bytes = s.toInterleaved(r)
  if ex.fmt.mode == cmLab and ex.fmt.sample == stU16: lab16Chroma(bytes, ex.cc + 1, false)
  var invert = newSeq[bool](ex.cc)
  for v in invert.mitems: v = ex.cmyk
  invert.add false
  let planes = deinterleave(bytes, ex.cc + 1, ex.fmt.sample, invert)
  let (w, h) = (r.width, r.height)
  var ch = @[ex.encode(-1, planes[ex.cc], w, h)]
  for c in 0 ..< ex.cc: ch.add ex.encode(int16(c), planes[c], w, h)
  (toPsdRect(r), ch)

proc mask(ex: Ex; m: doc.LayerMask; vector: Option[(float32, float32)]): (MaskData, ChannelData) =
  let s = if m.surface.format != ex.maskFmt: m.surface.convert(ex.maskFmt) else: m.surface
  let dp = s.defaultPixel
  let default = if dp.len > 0: dp[0] else: 0'f32
  let r = s.contentBounds
  let (w, h) = (r.width, r.height)
  let plane = if r.isEmpty: newSeq[byte]() else: deinterleave(s.toInterleaved(r), 1, ex.maskFmt.sample, [false])[0]
  var flags = 0'u8
  if not m.linked: flags = flags or mfRelative
  if not m.enabled: flags = flags or mfDisabled
  let parameters = maskParameters(some((m.density, m.feather)), vector)
  if parameters.isSome: flags = flags or mfParameters
  let pm = psd.LayerMask(rect: (if r.isEmpty: PsdRect() else: toPsdRect(r)), defaultColor: q255(default), flags: flags,
                         trailing: (if parameters.isNone: @[0'u8, 0] else: @[]), parameters: parameters,
                         realFirst: true)
  (MaskData(kind: mdMask, mask: pm), ex.encode(-2, plane, w, h))

proc freshId(ex: var Ex): uint32 =
  inc ex.nextId
  ex.nextId

proc layerIdFor(ex: var Ex; l: Layer): uint32 =
  if l.id in ex.layerIds: ex.layerIds[l.id] else: ex.freshId

proc record(ex: var Ex; l: Layer; extra: seq[TaggedBlock]; pixels: Option[Surface]): LayerRecord =
  let id = ex.layerIdFor(l)
  var (rect, channels) = if pixels.isSome: ex.pixelChannels(pixels.get, l.name) else: (PsdRect(), ex.emptyChannels)
  var md = MaskData(kind: mdNone)
  let vector = if l.vectorMask.isSome: some((l.vectorMask.get.density, l.vectorMask.get.feather))
               else: none((float32, float32))
  if l.mask.isSome:
    let (m, ch) = ex.mask(l.mask.get, vector)
    md = m
    channels.add ch
  else:
    let p = maskParameters(none((float32, float32)), vector)
    if p.isSome:
      md = MaskData(kind: mdMask, mask: psd.LayerMask(rect: PsdRect(), defaultColor: 255, flags: mfParameters,
                                                      parameters: p, realFirst: true))
  var flags = LayerFlags(0)
  flags.setHidden(not l.visible)
  flags.setBit(lfTransparencyProtected, l.locks.transparency)
  var blks = @[unicodeNameBlock(l.name), layerIdBlock(id)]
  let lspf = lspfFromLocks(l.locks)
  if lspf != 0: blks.add protectionBlock(lspf)
  if l.label != lcNone: blks.add sheetColorBlock(labelIndex(l.label))
  blks.add fillOpacityBlock(q255(l.fillOpacity))
  let fx = l.effects
  for effect in fx.items:
    var modes: seq[BlendMode]
    case effect.kind
    of ekDropShadow, ekInnerShadow: modes = @[effect.shadow.common.blend]
    of ekOuterGlow, ekInnerGlow: modes = @[effect.glow.common.blend]
    of ekStroke: modes = @[effect.stroke.common.blend]
    of ekColorOverlay, ekGradientOverlay, ekPatternOverlay: modes = @[effect.common.blend]
    of ekSatin: modes = @[effect.satin.common.blend]
    of ekBevelEmboss: modes = @[effect.bevel.highlight.blend, effect.bevel.shadow.blend]
    for mode in modes:
      if not mode.hasPsdEquivalent:
        ex.warnings.add "layer \"" & l.name & "\", " & effect.label & ": " & mode.label &
          " has no PSD equivalent and was written as Normal; save as .pcraft to preserve it"
  var lfx2 = none(seq[byte])
  let unchanged = effectsUnchanged(l)
  if (unchanged.isNone or unchanged.get) and not fx.psdRaw.isNil: lfx2 = some(fx.psdRaw.data)
  elif fx.items.len > 0: lfx2 = some(writeLfx2(fx.enabled, fx.items))
  if lfx2.isSome: blks.add newBlock((if l.isGroup: "lfxs" else: "lfx2"), lfx2.get)
  blks.add extra
  for b in blks.mitems:
    if b.data.len mod 2 == 1 and b.padding.isNone: b.data.add 0
  if not l.blend.hasPsdEquivalent:
    ex.warnings.add "layer \"" & l.name & "\": " & l.blend.label &
      " has no PSD equivalent and was written as Normal; save as .pcraft to preserve it"
  LayerRecord(rect: rect, channels: channels, blendMode: l.blend.psdKey, opacity: q255(l.opacity),
              clipping: uint8(ord(l.clipped)), flags: flags, mask: md,
              blendingRanges: rangesFromBlendIf(l.blendIf, ex.cc), legacyName: legacyName(l.name), blocks: blks)

proc setPrincipal(raw: var Raw; keys: openArray[string]; data: SharedBytes) =
  if data.isNil: return
  for k in keys:
    let i = raw.rawPos(k)
    if i >= 0:
      raw[i][1] = data.data
      return
  raw.add (keys[0], data.data)

proc fillRule(raw: var Raw; regenerated: var Raw; f: Fill) =
  var keep = false
  for (k, d) in raw:
    if k in ["SoCo", "GdFl", "PtFl"]:
      let p = parseFill(k, d)
      if p.isSome and p.get == f: keep = true
  if not keep:
    raw.keepItIf(it[0] notin ["SoCo", "GdFl", "PtFl"])
    var (key, data) = writeFill(f)
    while data.len mod 4 != 0: data.add 0
    regenerated.add (key, data)

proc vectorMaskBlock(ex: Ex; l: Layer; raw: var Raw) =
  let (w, h) = (ex.canvas.width, ex.canvas.height)
  var pos = -1
  for i, (k, _) in raw:
    if k == "vsms" or k == "vmsk":
      pos = i
      break
  if l.vectorMask.isNone:
    if pos >= 0: raw.keepItIf(it[0] != "vsms" and it[0] != "vmsk")
    return
  let vm = l.vectorMask.get
  if pos >= 0 and vectorMaskBlockMatches(raw[pos][1], vm, w, h): return
  let data = vectorMaskBytes(vm, w, h)
  if pos >= 0: raw[pos][1] = data
  else: raw.add ("vmsk", data)

proc contentBlocks(ex: var Ex; l: Layer): seq[TaggedBlock] =
  var raw: Raw
  for b in l.psdBlocks:
    if b.key notin Regenerated: raw.add (b.key, (if b.data.isNil: @[] else: b.data.data))
  if effectsUnchanged(l) == some(false): raw.keepItIf(it[0] != "lmfx")
  var regenerated: Raw
  case l.content.kind
  of lkAdjustment:
    let a = l.content.adjustment
    let channels =
      case ex.fmt.mode
      of cmRgb: chRgb
      of cmGrayscale: chGray
      of cmCmyk: chCmyk
      of cmLab: chLab
      else: chOther
    var cged = none(seq[byte])
    for (k, d) in raw:
      if k == "CgEd":
        cged = some(d)
        break
    var keep = false
    for (k, d) in raw:
      let legacyPhfl = k == "phfl" and d.len >= 2 and d[0] == 0 and d[1] == 2 and d.len mod 4 != 0
      if not legacyPhfl and k in AdjustmentKeys and adjust_map.parse(k, d, cged, channels) == a:
        keep = true
        break
    if not keep:
      raw.keepItIf(it[0] notin AdjustmentKeys and it[0] != "CgEd")
      let w = adjust_map.write(a)
      if w.len == 0:
        ex.warnings.add "layer \"" & l.name & "\": " & a.label & " adjustment is not yet written to PSD"
      if a.kind == akColorLookup and a.tetrahedral:
        ex.warnings.add "layer \"" & l.name &
          "\": Color Lookup tetrahedral interpolation is saved as trilinear (PSD has no interpolation setting)"
      regenerated.add w
  of lkFill: fillRule(raw, regenerated, l.content.fill)
  of lkShape:
    let sh = l.content.shape
    if sh.fill.isSome: fillRule(raw, regenerated, sh.fill.get)
    shapeBlocks(sh, raw, ex.canvas.width, ex.canvas.height, ex.dpi)
  of lkText:
    let t = l.content.text
    # Text layers without PSD data (created here) get a generated TySh.
    let generated = if t.psdRaw.isNil: shared(buildTysh(t, ex.dpi, none(array[4, float32]))) else: nil
    let src = if t.psdRaw.isNil: generated else: t.psdRaw
    # Character/paragraph style sheets from the document's styles.
    var styled: SharedBytes = nil
    if not src.isNil:
      let s2 = exportTysh(src.data, t, ex.textStyles, ex.dpi)
      if s2.isSome: styled = shared(s2.get)
    # `TextIndex` names the layer's object in the document's `Txt2` (where the auto-kern mode
    # lives); left at 0, every type layer would claim object 0 (#1348). An unchanged preserved
    # TySh stays byte-for-byte, but duplicate or out-of-range file numbers are renumbered.
    let idx = ex.textIndex.getOrDefault(l.id, 0'i32)
    let rewrite =
      if not styled.isNil: styled
      elif not generated.isNil: generated
      elif not src.isNil and textIndex(src.data) != some(idx): src
      else: nil
    var fresh: SharedBytes = nil
    if not rewrite.isNil:
      let f2 = setTextIndex(rewrite.data, idx)
      if f2.isSome: fresh = shared(f2.get)
    setPrincipal(raw, ["TySh"], if not fresh.isNil: fresh elif not styled.isNil: styled else: src)
    if not raw.rawHas("TySh"):
      ex.warnings.add "layer \"" & l.name & "\": text layer written as pixels (no TySh data)"
  of lkSmart:
    let sm = l.content.smart
    let parsed = if sm.psdRaw.isNil: none(Placed) else: parseSold(sm.psdRaw.data)
    if not sm.psdRaw.isNil and parsed.isNone:
      ex.smart.pruneOk = false
      setPrincipal(raw, smartKeys(sm.psdRaw), sm.psdRaw)
    else:
      ex.smartBlocks(l, sm, parsed, raw)
  of lkRaster, lkGroup: discard
  if l.effects.reference.isSome:
    var data: seq[byte]
    data.putBeF64(l.effects.reference.get[0])
    data.putBeF64(l.effects.reference.get[1])
    let i = raw.rawPos("fxrp")
    if i >= 0: raw[i][1] = data else: raw.add ("fxrp", data)
  else:
    raw.keepItIf(it[0] != "fxrp")
  let brst = brstData(l.excludedChannels)
  if brst.isSome:
    let i = raw.rawPos("brst")
    if i >= 0: raw[i][1] = brst.get else: raw.add ("brst", brst.get)
  else:
    raw.keepItIf(it[0] != "brst")
  putAdvanced(l.advanced, raw)
  if l.content.kind != lkShape: ex.vectorMaskBlock(l, raw)
  artboardBlock(ex.guides, l, raw)
  if ex.hasComps:
    let id = if l.id in ex.layerIds: ex.layerIds[l.id] else: 0'u32
    setCmls(raw, writeCmls(ex.comps, ex.lastComp, l, id))
  for (k, d) in regenerated: result.add newBlock(k, d)
  for (k, d) in raw: result.add newBlock(k, d)

proc fillPixels(ex: Ex; l: Layer; f: Fill): Surface =
  if l.fillCache.isSome and l.fillCache.get.fill == f: return l.fillCache.get.surface
  if ex.cmyk:
    let s = renderCmykFill(f, ex.canvas, fillFrame(l, ex.canvas), ex.fmt.sample)
    if s.isSome: return s.get
  let buf = renderFillContent(l, f, ex.canvas, ex.patterns)
  result = newSurface(ex.fmt)
  var vals = newSeqOfCap[float32](buf.px.len * ex.fmt.channels)
  for p in buf.px: vals.add fromRgba(ex.fmt, p)
  result.writeRegion(ex.canvas, vals)
  result.prune()

proc emit(ex: var Ex; layers: openArray[Layer]; tick: proc(): bool): bool =
  ## Appends records bottom-to-top; false once `tick` reports cancellation.
  for l in layers:
    let extra = ex.contentBlocks(l)
    case l.content.kind
    of lkGroup:
      let id = ex.freshId
      var divider = defaultLayerRecord()
      divider.channels = ex.emptyChannels
      divider.blendingRanges = fullBlendingRanges(ex.cc)
      divider.legacyName = cast[seq[byte]]("</Layer group>")
      divider.blocks = @[unicodeNameBlock("</Layer group>"), layerIdBlock(id), sectionDividerBlock(stBoundingDivider)]
      ex.records.add divider
      if not ex.emit(l.content.group.children, tick): return false
      let kind = if l.content.group.expanded: stOpenFolder else: stClosedFolder
      let lsct = sectionDividerBlock(kind, some(l.blend.psdKey))
      ex.records.add ex.record(l, @[lsct] & extra, none(Surface))
    of lkRaster: ex.records.add ex.record(l, extra, some(l.content.raster))
    of lkAdjustment: ex.records.add ex.record(l, extra, none(Surface))
    of lkFill:
      let px = ex.fillPixels(l, l.content.fill)
      ex.records.add ex.record(l, extra, some(px))
    of lkText: ex.records.add ex.record(l, extra, l.content.text.cache)
    of lkShape: ex.records.add ex.record(l, extra, l.content.shape.cache)
    of lkSmart: ex.records.add ex.record(l, extra, l.content.smart.cache)
    if not tick(): return false
  true

# ---------------------------------------------------------------- merged image

type CancelRender = object of CatchableError

proc mergedPlanes(d: Document; fmt: PixelFormat; cmyk, matte: bool; ctl: Interrupt): (seq[byte], bool, bool) =
  let sample = fmt.sample
  let cc = fmt.mode.colorChannels
  let bps = sample.bytes
  let n = int(d.size.area)
  let plane = n * bps
  var planes = newSeq[byte](plane * (cc + 1))
  var hasAlpha, translucent = false
  let white = fromRgba(fmt, [1'f32, 1, 1, 1])
  let canvas = d.bounds
  let w = canvas.width
  let space = cmykSpace(d)
  let lab16 = fmt.mode == cmLab and sample == stU16
  let rows = float32(max(canvas.height, 1))
  var outp = newSeq[seq[byte]](cc + 1)
  try:
    renderBands(d, canvas, 0, proc(band: Buffer) =
      if ctl.cancelled: raise newException(CancelRender, "cancelled")
      for p in band.px:
        let a = p[3]
        case sample
        of stU8:
          if q255(a) < 255: hasAlpha = true
        of stU16:
          if uint16(clamp(a, 0'f32, 1'f32) * 65535 + 0.5) != high(uint16): hasAlpha = true
        of stF32:
          if a != 1: hasAlpha = true
        if a < 1: translucent = true
      let start = (band.rect.y0 - canvas.y0) * w
      for c in 0 .. cc: outp[c].setLen(0)
      var v: array[5, float32]
      withCmykSpace(space):
        for p in band.px:
          discard fromRgbaInto(fmt, p, v)
          for c in 0 .. cc:
            let m = if c < cc and matte: matte(v[c], v[cc], white[c]) else: v[c]
            var x = if cmyk and c < cc: 1 - m else: m
            if lab16 and (c == 1 or c == 2): x = x * (Lab16ChromaMax / 65535)
            encodeBe(x, sample, outp[c])
      for c in 0 .. cc:
        let o = c * plane + start * bps
        if outp[c].len > 0 and o + outp[c].len <= planes.len:
          copyMem(addr planes[o], unsafeAddr outp[c][0], outp[c].len)
      ctl.progress(float32(band.rect.y1 - canvas.y0) / rows))
  except CancelRender: discard
  (planes, hasAlpha, translucent)

# ---------------------------------------------------------------- multichannel

proc multichannelToPsd(d: Document; forcePsb: bool): (PsdFile, seq[string]) =
  var warnings: seq[string]
  var sample = d.depth
  if sample == stF32:
    warnings.add "32-bit Multichannel written as 16 bits/channel (32-bit Multichannel isn't supported)"
    sample = stU16
  if d.layers.len > 0:
    warnings.add "Multichannel documents have no layers: the layers were not written"
  let canvas = d.bounds
  let channels = d.channels[0 ..< min(d.channels.len, 56)]
  if d.channels.len > 56: warnings.add "only 56 channels fit in PSD; the rest were dropped"
  var planes: seq[byte]
  if channels.len == 0:
    for _ in 0 ..< canvas.width * canvas.height: encodeBe(1, sample, planes)
  for ch in channels:
    let vals = ch.surface.readRegion(canvas)
    let n = ch.surface.channels
    var i = 0
    while i + n <= vals.len:
      encodeBe(1 - vals[i], sample, planes)
      i += n
  let big = d.size.width > 30_000 or d.size.height > 30_000
  let exceeds = not forcePsb and not big and psdSizeExceedsLimit(d)
  let version = psdVersion(d, forcePsb, exceeds)
  if big and not forcePsb: warnings.add "document exceeds 30000 px; written as PSB"
  elif exceeds and not forcePsb: warnings.add "estimated encoded size exceeds 2 GB; written as PSB"
  let header = newHeader(version, d.size.width, d.size.height, max(channels.len, 1), psdDepth(sample), pcmMultichannel)
  var imageData: ImageData
  try: imageData = encodeImageData(cRle, planes, header)
  except CatchableError:
    try: imageData = encodeImageData(cRaw, planes, header)
    except CatchableError: imageData = ImageData(compression: cRaw, data: planes)
  var resources = @[newResource(ridResolutionInfo, resolutionFromDpi(float64(d.resolutionDpi)).toBytes)]
  if channels.len > 0:
    var names: seq[string]
    for c in channels: names.add c.name
    resources.add newResource(1006, pascalNamesResource(names))
    resources.add newResource(1045, unicodeNamesResource(names))
    resources.add newResource(DisplayInfo, displayInfo(channels))
  if d.metadata.xmp.isSome: resources.add newResource(ridXmp, cast[seq[byte]](d.metadata.xmp.get))
  resources.add versionInfoResource(true)
  let file = PsdFile(header: header, colorModeData: colorModeDataForDepth(header.depth), resources: resources,
                     layerInfoPlacement: LayerInfoPlacement(kind: lpSection), imageData: imageData)
  (file, warnings)

# ---------------------------------------------------------------- document

proc documentToPsdNested(d: Document; opts: PsdExportOptions; depth: int; ctl: Interrupt): (PsdFile, seq[string]) =
  if d.mode == cmMultichannel: return multichannelToPsd(d, opts.forcePsb)
  let fmt = d.pixelFormat
  let sample = fmt.sample
  let cc = fmt.mode.colorChannels
  let big = d.size.width > 30_000 or d.size.height > 30_000
  let exceeds = not opts.forcePsb and not big and psdSizeExceedsLimit(d)
  let version = psdVersion(d, opts.forcePsb, exceeds)
  var ex = Ex(fmt: fmt, dpi: d.resolutionDpi, textStyles: d.textStyles, maskFmt: pixelFormat(cmGrayscale, sample, false),
              cc: cc, cmyk: fmt.mode == cmCmyk, version: version, canvas: d.bounds, guides: d.guides,
              smart: newSmartOut(d, depth), patterns: d.patterns)
  if not compsUnchanged(d):
    ex.hasComps = true
    ex.comps = d.layerComps
    ex.lastComp = d.lastDocumentState
  if big and not opts.forcePsb: ex.warnings.add "document exceeds 30000 px; written as PSB"
  elif exceeds and not opts.forcePsb: ex.warnings.add "estimated encoded size exceeds 2 GB; written as PSB"
  if fmt.mode != d.mode: ex.warnings.add $d.mode & " document written as " & $fmt.mode
  let walk = d.walk
  block:
    var used = initHashSet[uint32]()
    for (_, _, l) in walk:
      if l.psdId.isSome and not used.containsOrIncl(l.psdId.get):
        ex.layerIds[l.id] = l.psdId.get
    var next = 1'u32
    for (_, _, l) in walk:
      if l.id notin ex.layerIds:
        while next in used: inc next
        used.incl next
        ex.layerIds[l.id] = next
    for u in used: ex.nextId = max(ex.nextId, u)
    var usedTxt = initHashSet[int32]()
    var nextTxt = 0'i32
    for (_, _, l) in walk:
      if l.content.kind != lkText: continue
      let kept = if l.content.text.psdRaw.isNil: none(int32) else: textIndex(l.content.text.psdRaw.data)
      var index: int32
      if kept.isSome and not usedTxt.containsOrIncl(kept.get): index = kept.get
      else:
        while nextTxt in usedTxt: inc nextTxt
        usedTxt.incl nextTxt
        index = nextTxt
      ex.textIndex[l.id] = index
  let records = float32(walk.len)
  let units = records + MergedWeight
  let layersShare = records / units
  var done = 0'f32
  let tick = proc(): bool =
    done += 1
    ctl.progress(done / units)
    not ctl.cancelled
  let space = cmykSpace(d)
  withCmykSpace(space):
    discard ex.emit(d.layers, tick)
  let mergedEnd = layersShare + (1 - layersShare) * MergedRendered
  var (planes, hasAlpha, translucent) = mergedPlanes(d, fmt, fmt.mode == cmCmyk, opts.mergedMatte,
                                                     withSpan(ctl, layersShare, mergedEnd))
  if not hasAlpha and translucent and opts.mergedMatte:
    planes = mergedPlanes(d, fmt, fmt.mode == cmCmyk, false, newInterrupt(proc(): bool = ctl.cancelled))[0]
  let n = int(d.size.area)
  if not hasAlpha: planes.setLen(min(planes.len, cc * n * sample.bytes))
  let canvas = d.bounds
  let maxExtra = 56 - cc - ord(hasAlpha)
  if d.channels.len > maxExtra:
    ex.warnings.add "only " & $maxExtra & " alpha channels fit in PSD; the rest were dropped"
  var extra = d.channels[0 ..< min(d.channels.len, maxExtra)]
  var quickMaskId = none(uint16)
  if d.quickMask.isSome:
    if extra.len < maxExtra:
      extra.add d.quickMask.get
      quickMaskId = some(uint16(cc + ord(hasAlpha) + extra.len - 1))
    else:
      ex.warnings.add "Quick Mask was dropped because the PSD channel limit was reached"
  for a in extra:
    let s = if a.surface.format != ex.maskFmt: a.surface.convert(ex.maskFmt) else: a.surface
    planes.add deinterleave(s.toInterleaved(canvas), 1, sample, [false])[0]
  let channels = cc + ord(hasAlpha) + extra.len
  let header = newHeader(version, d.size.width, d.size.height, channels, psdDepth(sample), psdMode(fmt.mode))
  let mcomp = if sample == stF32: cRaw else: opts.mergedCompression
  var imageData: ImageData
  try: imageData = encodeImageData(mcomp, planes, header)
  except CatchableError:
    try: imageData = encodeImageData(cRaw, planes, header)
    except CatchableError: imageData = ImageData(compression: cRaw, data: planes)
  ctl.progress(1.0)
  var resources = @[
    newResource(ridResolutionInfo, resolutionFromDpi(float64(d.resolutionDpi)).toBytes),
    newResource(ridGlobalAngle, beBytes32(int32(asI32(round(float64(d.globalLight.angle)))))),
    newResource(ridGlobalAltitude, beBytes32(int32(asI32(round(float64(d.globalLight.altitude))))))]
  if not d.iccProfile.isNil: resources.add newResource(ridIccProfile, d.iccProfile.data)
  if d.guides.horizontal.len > 0 or d.guides.vertical.len > 0: resources.add newResource(1032, guidesResource(d))
  if extra.len > 0:
    var names: seq[string]
    for a in extra: names.add a.name
    resources.add newResource(1006, pascalNamesResource(names))
    resources.add newResource(1045, unicodeNamesResource(names))
    resources.add newResource(DisplayInfo, displayInfo(extra))
  if quickMaskId.isSome:
    var data: seq[byte]
    data.putBeU16(quickMaskId.get)
    data.add 0
    resources.add newResource(QuickMaskInfo, data)
  block:
    let (w, h) = (d.size.width, d.size.height)
    let maxPaths = int(SavedPathsLast - SavedPathsFirst + 1)
    if d.paths.len > maxPaths:
      ex.warnings.add "only " & $maxPaths & " saved paths fit in PSD; the rest were dropped"
    for i in 0 ..< min(d.paths.len, maxPaths):
      let p = d.paths[i]
      var data: seq[byte]
      if not p.psdRaw.isNil and pathFromResource(p.psdRaw.data, w, h) == some(p.path): data = p.psdRaw.data
      else: data = pathToRecords(p.path, w, h).toBytes
      var r = newResource(SavedPathsFirst + uint16(i), data)
      r.name = legacyName(p.name)
      resources.add r
    if d.workPath.isSome:
      resources.add newResource(WorkPath, pathToRecords(d.workPath.get, w, h).toBytes)
    var rawName = none(string)
    for r in d.metadata.psdResources:
      if r.id == ClippingPathId:
        let dd = if r.data.isNil: @[] else: r.data.data
        let nn = if dd.len > 0: int(dd[0]) else: 0
        rawName = some(if 1 + nn <= dd.len: lossyUtf8(dd.toOpenArray(1, nn)) else: "")
        break
    if d.clippingPath.isSome and rawName != some(d.clippingPath.get.name):
      let c = d.clippingPath.get
      let name = legacyName(c.name)
      var data = @[byte(name.len)]
      data.add name
      data.putBeU32(asU32(float64(max(c.flatness, 0'f32)) * 65536.0))
      resources.add newResource(ClippingPathId, data)
  let groups = linkGroupResource(d.layers)
  if groups.isSome: resources.add newResource(ridLayerGroupInfo, groups.get)
  let ppi = if d.resolutionDpi > 0: some((d.resolutionDpi, d.resolutionDpi)) else: none((float32, float32))
  if d.metadata.xmp.isSome:
    resources.add newResource(ridXmp, cast[seq[byte]](exportXmp(d.metadata.xmp.get, ppi)))
  if not d.metadata.exif.isNil:
    resources.add newResource(ridExif, exportExif(d.metadata.exif.data, ppi))
  var globalBlocks: seq[TaggedBlock]
  let preserved = ex.smart.finish(annotations_map.exportBlocks(d, exportGlobalBlocks(d)))
  # The type layers' `Txt2` is regenerated (the auto-kern modes live there, and a preserved block
  # goes stale as text is edited): one object per `TextIndex`. The block it replaces is handed
  # over so a kept object keeps everything the file held beyond its text and style runs. A fresh
  # document of ordinary (Metrics) text writes none: readers reject the empty stub (#3514).
  var textLayers: seq[(int32, TextLayer)]
  for (_, _, l) in d.walk:
    if l.content.kind == lkText: textLayers.add (ex.textIndex.getOrDefault(l.id, 0'i32), l.content.text)
  var previous = none(seq[byte])
  if textLayers.len > 0:
    for b in preserved:
      if b.key == "Txt2":
        previous = some(if b.data.isNil: newSeq[byte]() else: b.data.data)
        break
  let txt2 =
    if previous.isSome or needsTxt2(textLayers): some(buildTxt2(textLayers, previous))
    else: none(seq[byte])
  for b in preserved:
    if txt2.isSome and b.key == "Txt2": continue
    if ex.records.len > 0 and b.key in ["Lr16", "Lr32", "Layr"]: continue
    let data = if b.data.isNil: newSeq[byte]() else: b.data.data
    var tb = newBlock(b.key, data)
    tb.signature = b.signature
    tb.padding = some(newSeq[byte]((4 - data.len mod 4) mod 4))
    globalBlocks.add tb
  if txt2.isSome:
    var tb = newBlock("Txt2", txt2.get)
    tb.signature = "8BIM"
    tb.padding = some(newSeq[byte]((4 - txt2.get.len mod 4) mod 4))
    globalBlocks.add tb
  let compsResource = if ex.hasComps: some(writeCompsResource(d)) else: none(Option[seq[byte]])
  let (slicesResource, slicesWarning) = slices_map.exportResource(d, ex.layerIds)
  if slicesWarning.isSome: ex.warnings.add slicesWarning.get
  for r in d.metadata.psdResources:
    if r.id == LayerComps and compsResource.isSome: continue
    if r.id == SlicesId and slicesResource.isSome: continue
    if not keepResource(d, r.id): continue
    let data = if r.data.isNil: newSeq[byte]() else: r.data.data
    if r.id == ClippingPathId:
      let nn = if data.len > 0: int(data[0]) else: 0
      let rawName = if 1 + nn <= data.len: lossyUtf8(data.toOpenArray(1, nn)) else: ""
      if d.clippingPath.isNone or d.clippingPath.get.name != rawName: continue
    var res = newResource(r.id, data)
    res.name = legacyName(r.name)
    resources.add res
  let scale = freshScaleResource(d)
  if scale.isSome: resources.add newResource(MeasurementScaleId, scale.get)
  if compsResource.isSome and compsResource.get.isSome: resources.add newResource(LayerComps, compsResource.get.get)
  if slicesResource.isSome and slicesResource.get.isSome: resources.add newResource(SlicesId, slicesResource.get.get)
  resources.add versionInfoResource(true)
  let hasLayers = ex.records.len > 0
  var placement = LayerInfoPlacement(kind: lpSection)
  if hasLayers and sample == stU16:
    placement = LayerInfoPlacement(kind: lpGlobalBlock, index: 0, signature: "8BIM", key: "Lr16")
  elif hasLayers and sample == stF32:
    placement = LayerInfoPlacement(kind: lpGlobalBlock, index: 0, signature: "8BIM", key: "Lr32")
  var layerInfo = none(LayerInfo)
  if hasLayers:
    var info = LayerInfo(mergedAlpha: hasAlpha, layers: move ex.records)
    try: info.padTo(version, 4)
    except CatchableError: info.padding = none(seq[byte])
    layerInfo = some(info)
  let file = PsdFile(header: header, colorModeData: colorModeDataForDepth(header.depth), resources: resources,
                     layerInfo: layerInfo, layerInfoPlacement: placement,
                     globalLayerMask: (if hasLayers or globalBlocks.len > 0: some(GlobalLayerMask()) else: none(GlobalLayerMask)),
                     globalBlocks: globalBlocks, imageData: imageData)
  (file, ex.warnings)

proc documentToPsdWith*(d: Document; opts: PsdExportOptions): (PsdFile, seq[string]) =
  documentToPsdNested(d, opts, 0, NoInterrupt)

proc documentToPsdInterruptible*(d: Document; opts: PsdExportOptions; ctl: Interrupt): (PsdFile, seq[string]) =
  documentToPsdNested(d, opts, 0, ctl)

proc documentToPsd*(d: Document): PsdFile = documentToPsdWith(d, defaultPsdExportOptions())[0]
