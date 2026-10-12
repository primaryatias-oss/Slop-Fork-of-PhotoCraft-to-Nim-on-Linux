## `PsdBuilder`: constructs PSD/PSB files from pixel buffers. The builder never composites:
## supply the merged image with `composite`; otherwise a white placeholder is written and
## resource 1057 records `hasRealMergedData = false`.

import std/options
import ./io, ./model

type
  PixelKind* = enum pkRgba, pkGrayA, pkCmyka
  PixelData* = object
    ## Interleaved pixels with alpha. CMYK values are ink amounts (0 = no ink); the builder
    ## stores them inverted as Photoshop does. Exactly one of `d8`/`d16` is used (by `depth`).
    kind*: PixelKind
    depth*: int
    d8*: seq[byte]
    d16*: seq[uint16]
  MaskSpec* = object
    rect*: PsdRect
    data*: seq[byte]
    defaultColor*: uint8
    disabled*: bool
  LayerSpec* = object
    name*: string
    left*, top*: int32
    width*, height*: int
    pixels*: PixelData
    blendMode*: FourCC
    opacity*: uint8
    fillOpacity*: Option[uint8]
    visible*, clipping*: bool
    mask*: Option[MaskSpec]
    extraBlocks*: seq[TaggedBlock]
  GroupSpec* = object
    name*: string
    blendMode*: FourCC
    opacity*: uint8
    visible*, open*: bool
  EntryKind = enum ekLayer, ekGroupStart, ekGroupEnd
  Entry = object
    kind: EntryKind
    layer: LayerSpec
    group: GroupSpec
  PsdBuilder* = object
    width*, height*: int
    colorMode*: PsdColorMode
    depth*: int
    version*: Version
    compression*: Compression
    entries: seq[Entry]
    openGroups: seq[GroupSpec]
    compositePx: Option[PixelData]
    resources*: seq[ImageResource]

proc rgba8*(d: seq[byte]): PixelData = PixelData(kind: pkRgba, depth: 8, d8: d)
proc rgba16*(d: seq[uint16]): PixelData = PixelData(kind: pkRgba, depth: 16, d16: d)
proc grayA8*(d: seq[byte]): PixelData = PixelData(kind: pkGrayA, depth: 8, d8: d)
proc grayA16*(d: seq[uint16]): PixelData = PixelData(kind: pkGrayA, depth: 16, d16: d)
proc cmyka8*(d: seq[byte]): PixelData = PixelData(kind: pkCmyka, depth: 8, d8: d)
proc cmyka16*(d: seq[uint16]): PixelData = PixelData(kind: pkCmyka, depth: 16, d16: d)

proc mode(p: PixelData): PsdColorMode =
  case p.kind
  of pkRgba: pcmRgb
  of pkGrayA: pcmGrayscale
  of pkCmyka: pcmCmyk

proc spp(p: PixelData): int =
  case p.kind
  of pkRgba: 4
  of pkGrayA: 2
  of pkCmyka: 5

proc len(p: PixelData): int = (if p.depth == 16: p.d16.len else: p.d8.len)

proc plane(p: PixelData; c, n: int): seq[byte] =
  let spp = p.spp
  let invert = p.kind == pkCmyka and c < 4
  if p.depth == 16:
    result = newSeqOfCap[byte](2 * n)
    for i in 0 ..< n:
      let s = p.d16[i * spp + c]
      result.putU16(if invert: 65535'u16 - s else: s)
  else:
    result = newSeq[byte](n)
    for i in 0 ..< n:
      let s = p.d8[i * spp + c]
      result[i] = if invert: 255'u8 - s else: s

proc alphaIsOpaque(p: PixelData; n: int): bool =
  let spp = p.spp
  for i in 0 ..< n:
    if p.depth == 16:
      if p.d16[i * spp + spp - 1] != 65535: return false
    elif p.d8[i * spp + spp - 1] != 255: return false
  true

proc compositePlane(p: PixelData; c, n: int): seq[byte] =
  ## Photoshop stores transparent RGB composites against white.
  if p.kind != pkRgba or c >= 3: return p.plane(c, n)
  if p.depth == 16:
    for i in 0 ..< n:
      let (col, a) = (uint32(p.d16[4 * i + c]), uint32(p.d16[4 * i + 3]))
      let v = if a == 0: uint16(col) else: uint16((col * a + 32767) div 65535 + 65535 - a)
      result.putU16(v)
  else:
    result = newSeq[byte](n)
    for i in 0 ..< n:
      let (col, a) = (uint32(p.d8[4 * i + c]), uint32(p.d8[4 * i + 3]))
      result[i] = if a == 0: byte(col) else: byte((col * a + 127) div 255 + 255 - a)

proc newLayerSpec*(name: string; left, top: int32; width, height: int; pixels: PixelData): LayerSpec =
  LayerSpec(name: name, left: left, top: top, width: width, height: height, pixels: pixels,
            blendMode: "norm", opacity: 255, visible: true)

proc newGroupSpec*(name: string): GroupSpec =
  GroupSpec(name: name, blendMode: "pass", opacity: 255, visible: true, open: true)

proc newPsdBuilder*(width, height: int): PsdBuilder =
  PsdBuilder(width: width, height: height, colorMode: pcmRgb, depth: 8, version: vPsd, compression: cRle)

proc setResolution*(b: var PsdBuilder; dpi: float64) =
  b.resources.add newResource(ridResolutionInfo, resolutionFromDpi(dpi).toBytes)
proc setIccProfile*(b: var PsdBuilder; icc: seq[byte]) = b.resources.add newResource(ridIccProfile, icc)
proc pushLayer*(b: var PsdBuilder; spec: LayerSpec) = b.entries.add Entry(kind: ekLayer, layer: spec)
proc beginGroup*(b: var PsdBuilder; spec: GroupSpec) =
  b.entries.add Entry(kind: ekGroupStart)
  b.openGroups.add spec
proc endGroup*(b: var PsdBuilder) =
  if b.openGroups.len == 0: raise invalid("end_group without begin_group")
  b.entries.add Entry(kind: ekGroupEnd, group: b.openGroups.pop())
proc composite*(b: var PsdBuilder; p: PixelData) = b.compositePx = some(p)

proc colorChannels(b: PsdBuilder): int =
  if b.colorMode == pcmRgb: 3
  elif b.colorMode == pcmGrayscale: 1
  elif b.colorMode == pcmCmyk: 4
  else: raise unsupported("builder color mode " & $b.colorMode)

proc checkPixels(b: PsdBuilder; p: PixelData; n: int) =
  if p.mode != b.colorMode or p.depth != b.depth:
    raise invalid("pixel format " & $p.mode & "/" & $p.depth & " does not match document " & $b.colorMode & "/" & $b.depth)
  if p.len != n * p.spp:
    raise invalid("pixel buffer has " & $p.len & " samples, expected " & $(n * p.spp))

proc channel(b: PsdBuilder; id: int16; plane: openArray[byte]; w, h: int): ChannelData =
  encodeChannel(id, b.compression, plane, w, h, b.depth, b.version)

proc emptyChannels(b: PsdBuilder; cc: int): seq[ChannelData] =
  result.add b.channel(-1, [], 0, 0)
  for c in 0 ..< cc: result.add b.channel(int16(c), [], 0, 0)

proc layerRecord(b: PsdBuilder; spec: LayerSpec; cc: int; id: uint32): LayerRecord =
  let (w, h) = (spec.width, spec.height)
  let n = w * h
  b.checkPixels(spec.pixels, n)
  result = defaultLayerRecord()
  result.channels.add b.channel(-1, spec.pixels.plane(cc, n), w, h)
  for c in 0 ..< cc: result.channels.add b.channel(int16(c), spec.pixels.plane(c, n), w, h)
  if spec.mask.isSome:
    let m = spec.mask.get
    let (mw, mh) = m.rect.size
    if m.data.len != mw * mh: raise invalid("mask buffer size mismatch")
    var plane: seq[byte]
    if b.depth == 16:
      for v in m.data: plane.putU16(uint16(v) * 257)
    else: plane = m.data
    result.channels.add b.channel(-2, plane, mw, mh)
    result.mask = MaskData(kind: mdMask, mask: newLayerMask(m.rect, m.defaultColor, if m.disabled: mfDisabled else: 0'u8))
  result.flags.setHidden(not spec.visible)
  result.blocks = @[unicodeNameBlock(spec.name), layerIdBlock(id)]
  if spec.fillOpacity.isSome: result.blocks.add fillOpacityBlock(spec.fillOpacity.get)
  result.blocks.add spec.extraBlocks
  result.rect = rectXywh(spec.left, spec.top, spec.width, spec.height)
  result.blendMode = spec.blendMode
  result.opacity = spec.opacity
  result.clipping = if spec.clipping: 1 else: 0
  result.blendingRanges = fullBlendingRanges(cc)
  result.legacyName = encodeLegacyName(spec.name)

proc build*(b: PsdBuilder): PsdFile =
  if b.openGroups.len > 0: raise invalid("unclosed group")
  if b.depth notin [8, 16]: raise unsupported("builder depth " & $b.depth)
  let cc = b.colorChannels
  var layers: seq[LayerRecord]
  var id = 1'u32
  for e in b.entries:
    case e.kind
    of ekLayer: layers.add b.layerRecord(e.layer, cc, id)
    of ekGroupStart:
      var r = defaultLayerRecord()
      r.channels = b.emptyChannels(cc)
      r.blendingRanges = fullBlendingRanges(cc)
      r.legacyName = encodeLegacyName("</Layer group>")
      r.blocks = @[unicodeNameBlock("</Layer group>"), layerIdBlock(id), sectionDividerBlock(stBoundingDivider)]
      layers.add r
    of ekGroupEnd:
      let g = e.group
      var r = defaultLayerRecord()
      r.channels = b.emptyChannels(cc)
      r.blendMode = g.blendMode
      r.opacity = g.opacity
      r.flags.setHidden(not g.visible)
      r.blendingRanges = fullBlendingRanges(cc)
      r.legacyName = encodeLegacyName(g.name)
      r.blocks = @[unicodeNameBlock(g.name), layerIdBlock(id),
                   sectionDividerBlock(if g.open: stOpenFolder else: stClosedFolder, some(g.blendMode))]
      layers.add r
    inc id
  let n = b.width * b.height
  var real = false
  var withAlpha = false
  var planes: seq[byte]
  if b.compositePx.isSome:
    let p = b.compositePx.get
    b.checkPixels(p, n)
    withAlpha = not p.alphaIsOpaque(n)
    for c in 0 ..< cc + int(withAlpha):
      planes.add(if withAlpha: p.compositePlane(c, n) else: p.plane(c, n))
    real = true
  else:
    planes = newSeq[byte](n * (b.depth div 8) * cc)
    for v in planes.mitems: v = 0xFF
  let header = newHeader(b.version, b.width, b.height, cc + int(withAlpha), b.depth, b.colorMode)
  header.validate()
  result.header = header
  result.imageData = encodeImageData(b.compression, planes, header)
  result.resources = b.resources
  result.resources.add versionInfoResource(real)
  let hasLayers = layers.len > 0
  if hasLayers:
    result.layerInfo = some(LayerInfo(mergedAlpha: withAlpha, layers: layers))
    result.globalLayerMask = some(GlobalLayerMask())
  result.layerInfoPlacement =
    if hasLayers and b.depth == 16: LayerInfoPlacement(kind: lpGlobalBlock, index: 0, signature: "8BIM", key: "Lr16")
    else: LayerInfoPlacement(kind: lpSection)

proc toBytes*(b: PsdBuilder): seq[byte] = b.build().toBytes
