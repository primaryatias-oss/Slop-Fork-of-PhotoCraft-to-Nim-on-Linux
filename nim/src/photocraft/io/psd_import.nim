## PSD/PSB → document (Rust `psd_import.rs`).
##
## Layers, groups, masks, adjustments, fills, shapes, smart objects and type map to document
## content; everything PhotoCraft does not interpret (unknown tagged blocks, resources, global
## blocks) is kept verbatim so a PSD save can write it back. Text layers are read from the `TySh`
## block's plain text and transform until the type engine is ported (see PORTING.md).

import std/[options, tables, strutils, sequtils]
import ../geom, ../color, ../raster, ../doc, ../psd
import common, pixels, blocks, adjust_map, effects_map, channel_map, vector_map, smart_map,
       comps_map, pattern_map, slices_map, text_styles_map, annotations_map, multichannel_map
from ../vector import renderShape

const
  MappedResources* = [ridGlobalAngle, ridGlobalAltitude, ridResolutionInfo, ridIccProfile, ridXmp, ridExif,
    ridVersionInfo, ridThumbnail, ridThumbnailPs4, ridLayerState, ridLayerGroupInfo,
    1032'u16,   # grid and guides
    1045'u16,   # unicode alpha names
    1006'u16,   # pascal alpha names
    1069'u16,   # layer selection ids
    1072'u16,   # layer group(s) enabled id
    DisplayInfo, DisplayInfoOld, QuickMaskInfo]
  Regenerated* = ["luni", "lyid", "lsct", "lsdk", "iOpa", "lspf", "lclr", "lfx2", "lfxs"]
    ## Layer blocks rebuilt from the document on PSD save rather than kept verbatim.

type
  Ctx = object
    file: ptr PsdFile
    fmt, maskFmt: PixelFormat
    cc: int
    cmyk: bool
    warnings: seq[string]
    dpi: float32
    filterEffects: seq[FilterEffectsItem]
    smartContents: Table[string, SmartContentsId]
    ctl: Interrupt
    done, total: int

template records(cx: Ctx): untyped = cx.file[].layerInfo.get.layers

proc docMode(m: PsdColorMode): Option[ColorMode] =
  case uint16(m)
  of 0: some(cmBitmap)
  of 1: some(cmGrayscale)
  of 2: some(cmIndexed)
  of 3: some(cmRgb)
  of 4: some(cmCmyk)
  of 7: some(cmMultichannel)
  of 8: some(cmDuotone)
  of 9: some(cmLab)
  else: none(ColorMode)

proc selectedRealMask(rec: LayerRecord): Option[RealMask] =
  if not rec.hasChannel(ChannelRealUserMask): return
  let m = rec.layerMask
  if m.isNone: return
  m.get.real

proc blockData(rec: LayerRecord; key: string): Option[seq[byte]] =
  let i = rec.blockIndex(key)
  if i >= 0: some(rec.blocks[i].data) else: none(seq[byte])

proc hasBlock(rec: LayerRecord; key: string): bool = rec.blockIndex(key) >= 0

proc firstKey(rec: LayerRecord; keys: openArray[string]): Option[string] =
  for k in keys:
    if rec.hasBlock(k): return some(k)

proc softShapeFill(rec: LayerRecord; hasVector: bool; fillKey: Option[string]): Option[(Fill, bool)] =
  if not hasVector or rec.hasBlock("vstk") or rec.hasBlock("vscg"): return
  if fillKey.isNone: return
  let data = rec.blockData(fillKey.get)
  if data.isNone: return
  let fill = parseFill(fillKey.get, data.get)
  if fill.isNone: return
  let m = rec.layerMask
  var soft = false
  if m.isSome and m.get.parameters.isSome:
    let p = m.get.parameters.get
    soft = (p.vectorDensity.isSome and p.vectorDensity.get < 255) or
           (p.vectorFeather.isSome and p.vectorFeather.get > 0)
  if soft or fill.get.kind == fkPattern: some((fill.get, soft)) else: none((Fill, bool))

proc warn(cx: var Ctx; s: string) = cx.warnings.add s

proc channelPlane(cx: var Ctx; rec: LayerRecord; id: int16; name: string): Option[seq[byte]] =
  if not rec.hasChannel(id): return
  try:
    some(rec.decodeChannel(id, cx.file[].header.depth, cx.file[].header.version))
  except PsdError as e:
    cx.warn("layer \"" & name & "\": channel " & $id & " could not be decoded (" & e.msg & "); treated as empty")
    none(seq[byte])

proc recordSurface(cx: var Ctx; rec: LayerRecord; name: string): Surface =
  let r = rec.rect
  if r.isEmpty: return newSurface(cx.fmt)
  var (w, h) = (0, 0)
  try: (w, h) = r.size
  except PsdError: return newSurface(cx.fmt)
  let s = cx.fmt.sample
  var ids: seq[int16]
  for c in 0 ..< cx.cc: ids.add int16(c)
  ids.add ChannelTransparency
  var planes: seq[seq[byte]]
  var present: seq[bool]
  for id in ids:
    let p = cx.channelPlane(rec, id, name)
    present.add p.isSome
    planes.add (if p.isSome: p.get else: @[])
  for i, id in ids:
    if not present[i] and rec.hasChannel(id): return newSurface(cx.fmt)
  if not present.anyIt(it):
    cx.warn("layer \"" & name & "\": no channel data for its " & $w & "x" & $h & " bounds; treated as empty")
    return newSurface(cx.fmt)
  var fill = newSeq[seq[byte]]()
  for _ in 0 ..< cx.cc: fill.add zeroSample(s)
  fill.add maxSample(s)
  var invert = newSeq[bool]()
  for _ in 0 ..< cx.cc: invert.add cx.cmyk
  invert.add false
  var bytes = interleave(planes, present, fill, w * h, s, invert)
  if cx.fmt.mode == cmLab and s == stU16:
    lab16Chroma(bytes, cx.cc + 1, true)
  result = surfaceFromInterleaved(cx.fmt, rect(int(r.left), int(r.top), int(r.right), int(r.bottom)), bytes)
  result.prune()

proc recordMask(cx: var Ctx; rec: LayerRecord; name: string): Option[doc.LayerMask] =
  let mo = rec.layerMask
  if mo.isNone: return
  let m = mo.get
  var id: int16
  var mrect: PsdRect
  var default, flags: uint8
  let real = selectedRealMask(rec)
  if real.isSome:
    (id, mrect, default, flags) = (ChannelRealUserMask, real.get.rect, real.get.background, real.get.flags)
  else:
    if not rec.hasChannel(ChannelUserMask): return
    (id, mrect, default, flags) = (ChannelUserMask, m.rect, m.defaultColor, m.flags)
  var surface = surfaceWithDefault(cx.maskFmt, [float32(default) / 255])
  if not mrect.isEmpty:
    var ok = true
    var (w, h) = (0, 0)
    try: (w, h) = mrect.size
    except PsdError: ok = false
    if ok:
      let plane = cx.channelPlane(rec, id, name)
      if plane.isSome:
        let s = cx.maskFmt.sample
        let bytes = interleave([plane.get], [true], [zeroSample(s)], w * h, s, [false])
        surface.writeInterleaved(rect(int(mrect.left), int(mrect.top), int(mrect.right), int(mrect.bottom)), bytes)
        surface.prune()
  let params = m.parameters
  var density = 1'f32
  var feather = 0'f32
  if params.isSome:
    if params.get.userDensity.isSome: density = float32(params.get.userDensity.get) / 255
    if params.get.userFeather.isSome: feather = float32(params.get.userFeather.get)
  some(doc.LayerMask(surface: surface, enabled: (flags and 2) == 0, linked: (flags and 1) == 0,
                     density: density, feather: feather))

proc filterMask(cx: var Ctx; placed: string; stack: FilterStack; name: string): Option[doc.LayerMask] =
  for item in cx.filterEffects:
    if item.id == placed:
      try:
        return maskFromItem(item, cx.maskFmt.sample, stack)
      except ValueError, PsdError:
        cx.warn("layer \"" & name & "\": " & getCurrentExceptionMsg() & "; the filter mask was ignored")
        return none(doc.LayerMask)
  none(doc.LayerMask)

proc beF64(b: openArray[byte]; at: int): float64 =
  var u = 0'u64
  for i in 0 ..< 8: u = (u shl 8) or uint64(b[at + i])
  cast[float64](u)

proc applyCommon(cx: var Ctx; l: var Layer; rec: LayerRecord; blendOverride: Option[FourCC]) =
  l.visible = rec.isVisible
  l.opacity = float32(rec.opacity) / 255
  l.fillOpacity = float32(rec.fillOpacity) / 255
  l.clipped = rec.clipping != 0
  let key = if blendOverride.isSome: blendOverride.get else: rec.blendMode
  let b = blendFromPsdKey(key)
  if b.isSome: l.blend = b.get
  else:
    cx.warn("layer \"" & l.name & "\": unknown blend mode " & escapeJson(key) & "; using Normal")
    l.blend = bmNormal
  let lspf = rec.blockData("lspf")
  if lspf.isSome and lspf.get.len >= 4:
    l.locks = locksFromLspf(beU32(lspf.get, 0))
  if rec.flags.has(lfTransparencyProtected):
    l.locks.transparency = true
  let lclr = rec.blockData("lclr")
  if lclr.isSome and lclr.get.len >= 2:
    l.label = labelFromIndex(beU16(lclr.get, 0))
  let single = block:
    let a = rec.blockData("lfx2")
    if a.isSome: a else: rec.blockData("lfxs")
  let lmfx = rec.blockData("lmfx")
  let fx = if lmfx.isSome: lmfx else: single
  if fx.isSome:
    var enabled: bool
    var items: seq[Effect]
    let parsed = parseLfx2(fx.get)
    if parsed.isSome: (enabled, items) = parsed.get
    else: enabled = effectsEnabled(fx.get)
    l.effects = Effects(enabled: enabled, items: items,
                        psdRaw: (if single.isSome: shared(single.get) else: nil))
  else:
    let lrfx = rec.blockData("lrFX")
    if lrfx.isSome and not rec.sectionType.isFolder:
      let parsed = parseLrfx(lrfx.get)
      if parsed.isSome:
        l.effects = Effects(enabled: parsed.get[0], items: parsed.get[1])
  let fxrp = rec.blockData("fxrp")
  if fxrp.isSome and fxrp.get.len >= 16:
    l.effects.reference = some((beF64(fxrp.get, 0), beF64(fxrp.get, 8)))
  let brst = rec.blockData("brst")
  if brst.isSome: l.excludedChannels = parseBrst(brst.get)
  let r = rec
  l.advanced = advancedFromBlocks(proc(k: string): Option[seq[byte]] = r.blockData(k))
  l.blendIf = blendIfFromRanges(rec.blendingRanges)
  l.psdId = rec.layerId
  l.mask = cx.recordMask(rec, l.name)

proc preservedBlocks(rec: LayerRecord): seq[PsdBlock] =
  for b in rec.blocks:
    if b.key notin Regenerated: result.add PsdBlock(key: b.key, data: shared(b.data))

proc applyVectorMask(cx: var Ctx; l: var Layer; rec: LayerRecord) =
  let b = block:
    let a = rec.blockData("vsms")
    if a.isSome: a else: rec.blockData("vmsk")
  if b.isNone: return
  var vm = vectorMaskFromBlock(b.get, cx.file[].header.width, cx.file[].header.height)
  if vm.isNone: return
  let m = rec.layerMask
  if m.isSome:
    let p = m.get.parameters
    if p.isSome:
      vm.get.density = (if p.get.vectorDensity.isSome: float32(p.get.vectorDensity.get) / 255 else: 1'f32)
      vm.get.feather = (if p.get.vectorFeather.isSome: float32(p.get.vectorFeather.get) else: 0'f32)
    if (m.get.flags and 8) != 0 and selectedRealMask(rec).isNone:
      l.mask = none(doc.LayerMask)
  l.vectorMask = vm

proc engineTextIsBlank(text: string): bool = text.strip.len == 0

proc layerFromRecord(cx: var Ctx; rec: LayerRecord): Layer =
  let name = rec.name
  let fillKey = rec.firstKey(["SoCo", "GdFl", "PtFl"])
  let vectorKey = rec.firstKey(["vsms", "vmsk"])
  let adjKey = rec.firstKey(AdjustmentKeys)
  let smartKey = rec.firstKey(["SoLd", "PlLd", "SoLE"])
  let blocks = preservedBlocks(rec)
  proc principal(k: string): SharedBytes =
    for b in blocks:
      if b.key == k: return b.data
    nil
  var fillCache = none(FillCache)
  var content: LayerContent
  if adjKey.isSome:
    let k = adjKey.get
    let data = rec.blockData(k).get(@[])
    let channels =
      case cx.fmt.mode
      of cmRgb: chRgb
      of cmGrayscale: chGray
      of cmCmyk: chCmyk
      of cmLab: chLab
      else: chOther
    let adj = adjust_map.parse(k, data, rec.blockData("CgEd"), channels)
    if adj.kind == akUnsupported:
      let why = unreadableReason(k, data)
      let whyText = if why.isSome: " (" & why.get & ")" else: ""
      cx.warn("layer \"" & name & "\": its " & adjust_map.label(k) & " settings could not be read" & whyText &
              "; the layer is kept as saved but has no effect")
    content = adjustmentContent(adj)
  elif rec.hasBlock("TySh"):
    let data = rec.blockData("TySh").get(@[])
    var t = defaultTextLayer()
    let parsed = parseTysh(data)
    if parsed.isSome: (t.text, t.transform) = parsed.get
    t.syncSummary()
    let cache = cx.recordSurface(rec, name)
    let drawn = not cache.contentBounds.isEmpty or engineTextIsBlank(t.text)
    t.cache = (if drawn: some(cache) else: none(Surface))
    t.psdRaw = principal("TySh")
    content = textContent(t)
  elif smartKey.isSome:
    let k = smartKey.get
    let kd = rec.blockData(k)
    var (id, transform) = ("", IdentityAffine)
    if kd.isSome: (id, transform) = parseSmart(k, kd.get)
    var contentsId: SmartContentsId
    if id.len == 0: contentsId = freshContentsId()
    else:
      if id notin cx.smartContents: cx.smartContents[id] = freshContentsId()
      contentsId = cx.smartContents[id]
    var placed = none(Placed)
    block:
      let a = rec.blockData("SoLd")
      let sold = if a.isSome: a else: rec.blockData("SoLE")
      if sold.isSome: placed = parseSold(sold.get)
    let stack = if placed.isSome and placed.get.stack.isSome: placed.get.stack.get else: defaultFilterStack()
    var fmask = none(doc.LayerMask)
    if placed.isSome and stack.filters.len > 0:
      fmask = cx.filterMask(placed.get.placed, stack, name)
    content = smartContent(SmartObject(
      contentsId: contentsId,
      source: SmartSource(kind: ssLinked, path: id),
      transform: transform,
      smartFilters: stack.filters,
      cache: some(cx.recordSurface(rec, name)),
      psdRaw: principal(k),
      filtersEnabled: stack.enabled,
      filterMask: fmask,
      warp: (if kd.isSome: parsePlacedWarp(k, kd.get) else: none(Warp)),
      perspective: (if kd.isSome: parseSmartPerspective(k, kd.get) else: none(array[9, float64]))))
  else:
    let soft = softShapeFill(rec, vectorKey.isSome, fillKey)
    if soft.isSome:
      let (f, isSoft) = soft.get
      if not isSoft and f.kind == fkPattern and not rec.rect.isEmpty:
        fillCache = some(FillCache(fill: f, surface: cx.recordSurface(rec, name)))
      content = fillContent(f)
    elif (vectorKey.isSome and (fillKey.isSome or rec.hasBlock("vstk"))) or rec.hasBlock("vscg"):
      var fill = none(Fill)
      if fillKey.isSome:
        let d = rec.blockData(fillKey.get)
        if d.isSome: fill = parseFill(fillKey.get, d.get)
      var sh = ShapeLayer(fill: fill, cache: some(cx.recordSurface(rec, name)),
                          psdRaw: (if vectorKey.isSome: principal(vectorKey.get) else: nil))
      let r = rec
      shapeFromBlocks(sh, proc(k: string): Option[seq[byte]] = r.blockData(k),
                      cx.file[].header.width, cx.file[].header.height, cx.dpi)
      var badRect = rec.rect.isEmpty
      if not badRect:
        try: discard rec.rect.size
        except PsdError: badRect = true
      if badRect and (sh.path.subpaths.len > 0 or sh.path.inverted):
        let canvas = rect(0, 0, cx.file[].header.width, cx.file[].header.height)
        sh.cache = some(renderShape(sh, cx.fmt, canvas))
      content = shapeContent(sh)
    elif fillKey.isSome:
      let k = fillKey.get
      let d = rec.blockData(k)
      let f = if d.isSome: parseFill(k, d.get) else: none(Fill)
      if f.isSome:
        if not rec.rect.isEmpty:
          fillCache = some(FillCache(fill: f.get, surface: cx.recordSurface(rec, name)))
        content = fillContent(f.get)
      else:
        cx.warn("layer \"" & name & "\": unreadable " & k & " fill; imported as pixels")
        content = rasterContent(cx.recordSurface(rec, name))
    else:
      content = rasterContent(cx.recordSurface(rec, name))
  let rendered = content.kind in {lkShape, lkText, lkSmart}
  result = newLayer(name, content)
  result.psdBlocks = blocks
  result.fillCache = fillCache
  cx.applyCommon(result, rec, none(FourCC))
  let lm = rec.layerMask
  if rendered and selectedRealMask(rec).isNone and lm.isSome and (lm.get.flags and 8) != 0:
    result.mask = none(doc.LayerMask)
  if result.content.kind != lkShape:
    cx.applyVectorMask(result, rec)

proc build(cx: var Ctx; nodes: openArray[LayerNode]; depth: int): seq[Layer] =
  for n in nodes:
    if cx.ctl.cancelled: break
    if not n.group:
      let l = cx.layerFromRecord(cx.records[n.index])
      inc cx.done
      cx.ctl.progress(0.1'f32 + 0.85'f32 * float32(cx.done) / float32(max(cx.total, 1)))
      result.add l
    else:
      let rec = cx.records[n.index]
      var children: seq[Layer]
      if depth >= MaxGroupDepth:
        cx.warn("group `" & rec.name & "` nests deeper than " & $MaxGroupDepth &
                " groups; its contents were not imported")
      else:
        children = cx.build(n.children, depth + 1)
      let sd = rec.sectionDivider
      let expanded = sd.isNone or sd.get.kind != stClosedFolder
      var artboard = none(Artboard)
      for k in ArtboardKeys:
        let d = rec.blockData(k)
        if d.isSome:
          artboard = parseArtboard(d.get)
          break
      var l = newLayer(rec.name, LayerContent(kind: lkGroup,
                       group: Group(children: children, expanded: expanded, artboard: artboard)))
      l.psdBlocks = preservedBlocks(rec)
      cx.applyCommon(l, rec, (if sd.isSome: sd.get.blendMode else: none(FourCC)))
      cx.applyVectorMask(l, rec)
      result.add l

proc applyLinkGroups(nodes: openArray[LayerNode]; layers: var seq[Layer]; ids: openArray[uint16]; depth = 0) =
  for i in 0 ..< min(nodes.len, layers.len):
    let n = nodes[i]
    if n.group and depth < MaxGroupDepth and layers[i].content.kind == lkGroup:
      applyLinkGroups(n.children, layers[i].content.group.children, ids, depth + 1)
    layers[i].linkGroup =
      if n.index < ids.len and ids[n.index] != 0: some(uint64(ids[n.index])) else: none(uint64)

proc groupDepth*(file: PsdFile): int =
  var open = 0
  for rec in file.layers:
    let st = rec.sectionType
    if st == stBoundingDivider:
      inc open
      result = max(result, open)
    elif st.isFolder:
      open = max(open - 1, 0)

proc checkGroupDepth*(file: PsdFile) =
  if groupDepth(file) > MaxGroupDepth:
    raise ioError(ieUnsupported, "layer groups nested deeper than " & $MaxGroupDepth)

proc unicodeNames*(data: openArray[byte]): seq[string] =
  var at = 0
  while at + 4 <= data.len:
    let n = int(beU32(data, at))
    at += 4
    if n < 0 or n > (data.len - at) div 2: break
    var units = newSeq[uint16](n)
    for i in 0 ..< n: units[i] = beU16(data, at + 2 * i)
    var s = fromUtf16(units)
    while s.len > 0 and s[^1] == '\0': s.setLen(s.len - 1)
    result.add s
    at += n * 2

proc parseGuides(data: openArray[byte]; d: var Document) =
  if data.len < 16: return
  let count = int(beU32(data, 12))
  for i in 0 ..< count:
    let at = 16 + i * 5
    if at + 5 > data.len: break
    let pos = float64(cast[int32](beU32(data, at))) / 32
    if data[at + 4] == 1: d.guides.horizontal.add pos
    else: d.guides.vertical.add pos

proc isSavedPath(id: uint16): bool = id >= SavedPathsFirst and id <= SavedPathsLast

proc layerGroupIds(file: PsdFile): Option[seq[uint16]] =
  let r = file.resource(ridLayerGroupInfo)
  if r.isNone: return
  var ids: seq[uint16]
  for i in 0 ..< r.get.data.len div 2: ids.add beU16(r.get.data, 2 * i)
  some(ids)

proc psdToDocumentWith*(file: PsdFile; ctl: Interrupt): Option[(Document, seq[string])] =
  ## Maps a parsed PSD to a document; `none` when `ctl` cancels.
  let h = file.header
  var warnings: seq[string]
  var mode = cmRgb
  let dm = docMode(h.colorMode)
  if dm.isSome: mode = dm.get
  else: warnings.add "unknown color mode " & $uint16(h.colorMode) & "; importing as RGB"
  let layered = mode in {cmGrayscale, cmRgb, cmCmyk, cmLab} and h.depth != 1
  let multichannel = mode == cmMultichannel and h.depth != 1
  let depth = if layered or multichannel: sampleForDepth(h.depth) else: stU8
  var d = newDocument("Untitled", Size(width: h.width, height: h.height), mode, depth)
  for r in file.resources:
    let id = r.id
    if id == ridResolutionInfo:
      try:
        let ri = parseResolution(r.data)
        proc ppi(res: float64; unit: uint16): float64 = (if unit == 2: res * 2.54 else: res)
        let (x, y) = (ppi(ri.hRes, ri.hResUnit), ppi(ri.vRes, ri.vResUnit))
        d.resolutionDpi = float32(x)
        let w = unequalResolutionWarning(x, y)
        if w.isSome: warnings.add w.get
      except PsdError: discard
    elif id == ridIccProfile: d.iccProfile = shared(r.data)
    elif id == ridXmp: d.metadata.xmp = some(lossyUtf8(r.data))
    elif id == ridExif: d.metadata.exif = shared(r.data)
    elif id == 1032: parseGuides(r.data, d)
    elif id == ridGlobalAngle or id == ridGlobalAltitude:
      if r.data.len >= 4:
        let v = float32(cast[int32](beU32(r.data, 0)))
        if id == ridGlobalAngle: d.globalLight.angle = v else: d.globalLight.altitude = v
    elif isSavedPath(id):
      let path = pathFromResource(r.data, h.width, h.height)
      if path.isSome:
        d.paths.add NamedPath(name: lossyUtf8(r.name), path: path.get, psdRaw: shared(r.data))
      else:
        d.metadata.psdResources.add PsdResource(id: id, name: lossyUtf8(r.name), data: shared(r.data))
    elif id == WorkPath:
      d.workPath = pathFromResource(r.data, h.width, h.height)
    elif id == ClippingPathId:
      let n = if r.data.len > 0: int(r.data[0]) else: 0
      let name = if 1 + n <= r.data.len: lossyUtf8(r.data.toOpenArray(1, n)) else: ""
      d.clippingPath = some(ClippingPath(name: name, flatness: 0))
      d.metadata.psdResources.add PsdResource(id: id, name: lossyUtf8(r.name), data: shared(r.data))
    elif id in MappedResources: discard
    else:
      d.metadata.psdResources.add PsdResource(id: id, name: lossyUtf8(r.name), data: shared(r.data))
  for b in file.globalBlocks:
    d.metadata.psdGlobalBlocks.add PsdGlobalBlock(signature: b.signature, key: b.key, data: shared(b.data))
  let unreadable = file.unreadableLayerBlock
  if unreadable.isSome:
    warnings.add "the layers (" & unreadable.get[0].key & " block) could not be read (" & unreadable.get[1] &
      "); only the merged image was opened. Saving as PSD replaces the unreadable layer data"
  d.patterns = fromGlobalBlocks(d)
  d.notes = notesFromBlocks(d)
  if annoUnreadable(d):
    warnings.add "the notes (Anno block) could not be read; they are kept unchanged on PSD save unless a note is added"
  let scale = rawScale(d)
  if scale.isSome: d.measurement.scale = scale.get
  let fmt = d.pixelFormat
  let cc = fmt.mode.colorChannels
  var filePtr = unsafeAddr file
  var cx = Ctx(file: filePtr, fmt: fmt, maskFmt: pixelFormat(cmGrayscale, depth, false), cc: cc,
               cmyk: fmt.mode == cmCmyk, warnings: warnings, dpi: d.resolutionDpi, ctl: ctl,
               total: file.layerCount)
  for b in file.globalBlocks:
    if b.key notin ["FEid", "FXid"]: continue
    if ctl.cancelled: return none((Document, seq[string]))
    try:
      let fx = parseFilterEffects(b.data)
      cx.filterEffects.add fx.items
    except PsdError as e:
      cx.warn("smart filter masks (" & b.key & ") could not be read: " & e.msg)
  if ctl.cancelled: return none((Document, seq[string]))
  let (w, hh) = (h.width, h.height)
  let n = w * hh
  let canvas = rect(0, 0, h.width, h.height)
  let mergedAlpha = file.mergedHasAlpha
  let extra = h.channels > cc + ord(mergedAlpha)
  var merged = none(seq[byte])
  var mergedTried = false
  if not layered or file.layerCount == 0 or extra:
    mergedTried = true
    var m: seq[byte]
    var ok = true
    try: m = file.decodeMerged
    except PsdError as e:
      ok = false
      if ctl.cancelled: return none((Document, seq[string]))
      ctl.progress(0.1)
      cx.warn("merged image could not be decoded: " & e.msg)
    if ok:
      if ctl.cancelled: return none((Document, seq[string]))
      ctl.progress(0.1)
      merged = some(m)
  if multichannel:
    if merged.isSome:
      let r1045 = file.resource(1045)
      let names = if r1045.isSome: unicodeNames(r1045.get.data) else: @[]
      importMultichannel(file, merged.get, d, names, cx.warnings)
    if file.layerCount > 0:
      cx.warn("Multichannel documents have no layers: " & $file.layerCount & " layer records were not imported")
  elif layered:
    let tree = file.layerTree(MaxGroupDepth)
    d.layers = cx.build(tree, 0)
    let groups = layerGroupIds(file)
    if groups.isSome: applyLinkGroups(tree, d.layers, groups.get)
    if d.layers.len == 0 and merged.isSome:
      let all = merged.get
      let plane = h.rowBytes * hh
      let alphaIdx = if mergedAlpha: cc else: -1
      var planes: seq[seq[byte]]
      var present: seq[bool]
      for c in 0 ..< cc + 1:
        let idx = if c < cc: c else: alphaIdx
        if idx >= 0 and (idx + 1) * plane <= all.len:
          planes.add all[idx * plane ..< (idx + 1) * plane]
          present.add true
        else:
          planes.add @[]
          present.add false
      var fill: seq[seq[byte]]
      for _ in 0 ..< cc: fill.add zeroSample(depth)
      fill.add maxSample(depth)
      var inv: seq[bool]
      for _ in 0 ..< cc: inv.add cx.cmyk
      inv.add false
      var bytes = interleave(planes, present, fill, n, depth, inv)
      if fmt.mode == cmLab and depth == stU16:
        lab16Chroma(bytes, cc + 1, true)
      var s = surfaceFromInterleaved(fmt, canvas, bytes)
      if alphaIdx >= 0:
        let white = fromRgba(fmt, [1'f32, 1, 1, 1])
        var vals: seq[float32]
        var y = canvas.y0
        while y < canvas.y1:
          let band = rect(canvas.x0, y, canvas.x1, min(y + TileSize, canvas.y1))
          s.readRegionInto(band, vals)
          var p = 0
          while p + cc < vals.len:
            let a = vals[p + cc]
            for c in 0 ..< cc:
              vals[p + c] = (if a <= 0: 0'f32 else: unmatte(vals[p + c], a, white[c]))
            p += cc + 1
          s.writeRegion(band, vals)
          y = band.y1
      s.prune()
      var bg = newLayer("Background", rasterContent(s))
      if alphaIdx < 0:
        bg.locks.transparency = true
        bg.locks.position = true
      d.layers.add bg
  else:
    if file.layerCount > 0:
      cx.warn($h.colorMode & " documents are imported flattened: " & $file.layerCount &
              " layer records were not imported")
    else:
      cx.warn($h.colorMode & " " & $h.depth & "-bit document converted to " & $fmt.mode & " 8-bit for editing")
    if h.colorMode == pcmIndexed and file.colorModeData.len >= 768:
      let m = file.colorModeData
      var colors: seq[array[3, uint8]]
      for i in 0 ..< 256: colors.add [m[i], m[256 + i], m[512 + i]]
      d.colorTable = some(ColorTable(colors: colors))
    elif h.colorMode == pcmDuotone and file.colorModeData.len > 0:
      d.duotone = some(Duotone(inks: @[newDuotoneInk("Black", [0'f32, 0, 0])], psdRaw: some(file.colorModeData)))
      cx.warn("duotone inks are not interpreted (shown as grayscale)")
    elif file.colorModeData.len > 0:
      cx.warn("color mode data is not preserved")
    var img: Option[RgbaImage]
    try: img = some(file.compositeRgba8)
    except PsdError:
      if merged.isSome:
        try:
          let all = merged.get
          let plane = h.rowBytes * hh
          var data = newSeq[byte](n * 4)
          for i in 0 ..< data.len: data[i] = 255
          for c in 0 ..< min(3, h.channels):
            if (c + 1) * plane > all.len: raise newException(PsdError, "plane out of range")
            let p = planeToU8(all.toOpenArray(c * plane, (c + 1) * plane - 1), h.depth, w, hh)
            for i in 0 ..< min(n, p.len): data[i * 4 + c] = p[i]
          img = some(RgbaImage(left: 0, top: 0, width: h.width, height: h.height, data: data))
        except PsdError: discard
    if img.isSome:
      var s = newSurface(fmt)
      var vals: seq[float32]
      let data = img.get.data
      var i = 0
      while i + 3 < data.len:
        vals.add fromRgba(fmt, [float32(data[i]) / 255, float32(data[i + 1]) / 255,
                                float32(data[i + 2]) / 255, float32(data[i + 3]) / 255])
        i += 4
      s.writeRegion(canvas, vals)
      s.prune()
      d.layers.add newLayer("Background", rasterContent(s))
  if layered and merged.isSome:
    let all = merged.get
    let first = cc + ord(mergedAlpha)
    let plane = h.rowBytes * hh
    let r1045 = file.resource(1045)
    let names = if r1045.isSome: unicodeNames(r1045.get.data) else: @[]
    var k = 0
    for idx in first ..< h.channels:
      if (idx + 1) * plane > all.len: break
      var s = newSurface(cx.maskFmt)
      s.writeInterleaved(canvas, interleave([all[idx * plane ..< (idx + 1) * plane]], [true], [zeroSample(depth)],
                                            n, depth, [false]))
      s.prune()
      let name = if k < names.len: names[k] else: "Alpha " & $(k + 1)
      d.channels.add newAlphaChannel(name, s)
      inc k
    let di = file.resource(DisplayInfo)
    if di.isSome: applyDisplayInfo(di.get.data, true, d.channels)
    else:
      let old = file.resource(DisplayInfoOld)
      if old.isSome: applyDisplayInfo(old.get.data, false, d.channels)
    let qm = file.resource(QuickMaskInfo)
    if qm.isSome and qm.get.data.len >= 2:
      let k = int(beU16(qm.get.data, 0)) - first
      if k >= 0 and k < d.channels.len:
        d.quickMask = some(d.channels[k])
        d.channels.delete(k)
  var rawComps = none(seq[byte])
  for r in d.metadata.psdResources:
    if r.id == LayerComps:
      rawComps = some(r.data.data)
      break
  let (comps, lastApplied, lastState, compWarnings) = decodeComps(rawComps, d)
  d.layerComps = comps
  d.lastAppliedComp = lastApplied
  d.lastDocumentState = lastState
  cx.warnings.add compWarnings
  importSlices(d)
  cx.warnings.add importStyles(d)
  if ctl.cancelled: return none((Document, seq[string]))
  ctl.progress(1.0)
  discard mergedTried
  some((d, cx.warnings))

proc psdToDocument*(file: PsdFile): (Document, seq[string]) =
  let r = psdToDocumentWith(file, NoInterrupt)
  if r.isSome: r.get
  else: (newDocument("Untitled", Size(width: 1, height: 1), cmRgb, stU8), newSeq[string]())
