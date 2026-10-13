## Photoshop layer data embedded in TIFF files (layered TIFF), per Adobe's Photoshop TIFF
## Technical Notes: tag 37724 `ImageSourceData` ("Adobe Photoshop Document Data Block\0" then
## tagged blocks, `Layr`/`Lr16`/`Lr32` holding the layer info and `LMsk` the global mask) and tag
## 34377 (image resources).
##
## Either byte order is read and written: Intel-order (little-endian, `MIB8`) data goes through
## the field-by-field transcoder (`transcode`); blocks of unknown layout are kept as stored.

import std/options
import ./io, ./compression, ./model, ./transcode
export transcode.ByteOrder, transcode.isLittle

const
  TiffSignature* = "Adobe Photoshop Document Data Block\0"
  TiffSignaturePsb* = "Adobe Photoshop Document Data V0002\0"

type
  ImageSourceData* = object
    version*: Version
    byteOrder*: ByteOrder
    layerInfo*: Option[LayerInfo]
    layerInfoKey*: FourCC
    globalLayerMask*: Option[GlobalLayerMask]
    globalBlocks*: seq[TaggedBlock]
    trailing*: seq[byte]

proc layerInfoKey*(depth: int): string =
  case depth
  of 16: "Lr16"
  of 32: "Lr32"
  else: "Layr"

proc defaultImageSourceData*(): ImageSourceData =
  ImageSourceData(version: vPsd, byteOrder: boBig, layerInfoKey: "Layr")

proc blockOrder(rest: openArray[byte]): ByteOrder =
  if rest.len < 4: return boBig
  let s = toFourCC(rest)
  case s
  of "8BIM", "8B64": boBig
  of "MIB8", "46B8": boLittle
  else: raise badSignature("8BIM or MIB8 (Photoshop image source data block)", s)

proc startsWith(data: openArray[byte]; s: string): bool =
  if data.len < s.len: return false
  for i, c in s:
    if data[i] != byte(c): return false
  true

proc parseImageSourceData*(data: openArray[byte]): (ImageSourceData, seq[string]) =
  var isd = defaultImageSourceData()
  if data.startsWith(TiffSignature): isd.version = vPsd
  elif data.startsWith(TiffSignaturePsb): isd.version = vPsb
  else: raise invalid("not Photoshop image source data (missing \"Adobe Photoshop Document Data Block\")")
  template rest: untyped = data.toOpenArray(36, data.len - 1)
  isd.byteOrder = blockOrder(rest)
  var warnings: seq[string]
  var big: seq[byte]
  if isd.byteOrder == boLittle:
    (big, warnings) = transcodeBlocks(rest, isd.version, boLittle, boBig)
  else:
    big = @rest
  var r = newReader(big)
  var (blocks, trailing) = readBlocks(r, isd.version)
  for b in blocks.mitems:
    if b.padding.isSome and b.padding.get.len <= 3:
      var allZero = true
      for x in b.padding.get:
        if x != 0: allZero = false
      if allZero: b.padding = none(seq[byte])
  for i, b in blocks:
    if b.key in ["Layr", "Lr16", "Lr32"]:
      isd.layerInfo = some(readLayerInfoBody(b.data, isd.version))
      isd.layerInfoKey = b.key
      blocks.delete(i)
      break
  for i, b in blocks:
    if b.key == "LMsk":
      isd.globalLayerMask = some(GlobalLayerMask(data: b.data))
      blocks.delete(i)
      break
  isd.globalBlocks = blocks
  isd.trailing = trailing
  (isd, warnings)

proc paddedBlock(key: string; data: seq[byte]): TaggedBlock =
  TaggedBlock(signature: "8BIM", key: key, data: data, padding: some(newSeq[byte]((4 - data.len mod 4) mod 4)))

proc toBytes*(isd: ImageSourceData; order = boBig): (seq[byte], seq[string]) =
  var blocks: seq[TaggedBlock]
  var warnings: seq[string]
  if isd.layerInfo.isSome:
    var info = isd.layerInfo.get
    info.padTo(isd.version, 4)
    var data: seq[byte]
    writeLayerInfoBody(info, data, isd.version)
    blocks.add paddedBlock(isd.layerInfoKey, data)
  if isd.globalLayerMask.isSome: blocks.add paddedBlock("LMsk", isd.globalLayerMask.get.data)
  for b0 in isd.globalBlocks:
    var b = b0
    # Padded to 4 like the others; the signature stays (a foreign-order marker included).
    if b.padding.isNone: b.padding = some(newSeq[byte]((4 - b.data.len mod 4) mod 4))
    blocks.add b
  var body: seq[byte]
  writeBlocks(body, blocks, isd.version, 2)
  body.put(isd.trailing)
  if order == boLittle: (body, warnings) = transcodeBlocks(body, isd.version, boBig, boLittle)
  var o: seq[byte]
  o.put(if isd.version.isPsb: TiffSignaturePsb else: TiffSignature)
  o.put(body)
  (o, warnings)

proc fromPsd*(f: PsdFile): ImageSourceData =
  ImageSourceData(version: f.header.version, byteOrder: boBig, layerInfo: f.layerInfo,
    layerInfoKey: (if f.layerInfoPlacement.kind == lpGlobalBlock: f.layerInfoPlacement.key else: layerInfoKey(f.header.depth)),
    globalLayerMask: f.globalLayerMask, globalBlocks: f.globalBlocks, trailing: f.layerMaskTrailing)

proc intoPsd*(isd: ImageSourceData; header: Header; resources: seq[ImageResource]; imageData: ImageData): PsdFile =
  result.header = header
  result.resources = resources
  result.layerInfo = isd.layerInfo
  result.layerInfoPlacement =
    if isd.layerInfoKey in ["Lr16", "Lr32"] and isd.layerInfo.isSome:
      LayerInfoPlacement(kind: lpGlobalBlock, index: 0, signature: "8BIM", key: isd.layerInfoKey)
    else: LayerInfoPlacement(kind: lpSection)
  result.globalLayerMask = isd.globalLayerMask
  result.globalBlocks = isd.globalBlocks
  result.layerMaskTrailing = isd.trailing
  result.imageData = imageData

proc resourcesFromBytes*(data0: openArray[byte]): (seq[ImageResource], seq[string]) =
  var warnings: seq[string]
  var data: seq[byte]
  if blockOrder(data0) == boLittle: (data, warnings) = transcodeResources(data0, boLittle, boBig)
  else: data = @data0
  var r = newReader(data)
  var res: seq[ImageResource]
  while not r.isEmpty:
    if r.remaining < 12:
      var zeros = true
      for i in r.pos ..< r.size:
        if r.at(i) != 0: zeros = false
      if zeros: break
    var s = r.sub(uint64(r.remaining))
    var one: seq[ImageResource]
    # read exactly one resource
    let before = s.pos
    discard before
    let sig = s.fourcc()
    if sig notin ["8BIM", "MeSa", "AgHg", "PHUT", "DCSR"]: raise badSignature("8BIM resource", sig)
    let id = s.u16()
    let name = readPascal(s, 2)
    let ln = s.u32()
    let d = s.bytesU64(uint64(ln))
    if ln mod 2 == 1 and not s.isEmpty: s.skip(1)
    one.add ImageResource(signature: sig, id: id, name: name, data: d)
    res.add one
    r.pos += s.pos
  (res, warnings)

proc resourcesToBytes*(resources: openArray[ImageResource]): seq[byte] =
  for x in resources: x.write(result)
