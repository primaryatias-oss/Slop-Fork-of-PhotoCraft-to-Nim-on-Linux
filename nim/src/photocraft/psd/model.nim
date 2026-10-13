## The PSD/PSB file model: header, image resources, tagged blocks, layer records and the merged
## image, with a byte-stable parser and writer. Unknown blocks and resources are kept raw, channel
## data stays in its stored encoding, and padding is preserved, so unmodified files round-trip
## byte for byte.

import std/[options, strutils, math]
import ./io, ./compression, ./descriptor
export compression

const
  MaxDimension* = 300_000
  MaxChannels* = 56
  MaxLayerChannels* = 64
  ChannelTransparency* = -1'i16
  ChannelUserMask* = -2'i16
  ChannelRealUserMask* = -3'i16

# ---------------------------------------------------------------- header

type
  PsdColorMode* = distinct uint16
  Header* = object
    version*: Version
    reserved*: array[6, byte]
    channels*: int
    height*, width*: int
    depth*: int
    colorMode*: PsdColorMode

const
  pcmBitmap* = PsdColorMode(0)
  pcmGrayscale* = PsdColorMode(1)
  pcmIndexed* = PsdColorMode(2)
  pcmRgb* = PsdColorMode(3)
  pcmCmyk* = PsdColorMode(4)
  pcmMultichannel* = PsdColorMode(7)
  pcmDuotone* = PsdColorMode(8)
  pcmLab* = PsdColorMode(9)

proc `==`*(a, b: PsdColorMode): bool {.borrow.}
proc `$`*(m: PsdColorMode): string =
  case uint16(m)
  of 0: "Bitmap"
  of 1: "Grayscale"
  of 2: "Indexed"
  of 3: "Rgb"
  of 4: "Cmyk"
  of 7: "Multichannel"
  of 8: "Duotone"
  of 9: "Lab"
  else: "Unknown(" & $uint16(m) & ")"

proc colorChannels*(m: PsdColorMode): Option[int] =
  case uint16(m)
  of 0, 1, 2, 8: some(1)
  of 3, 9: some(3)
  of 4: some(4)
  else: none(int)

proc newHeader*(version: Version; width, height, channels, depth: int; mode: PsdColorMode): Header =
  Header(version: version, width: width, height: height, channels: channels, depth: depth, colorMode: mode)

proc rowBytes*(h: Header): int = rowBytes(h.width, h.depth)

proc validate*(h: Header) =
  if h.channels < 1 or h.channels > MaxChannels:
    raise invalid("channel count " & $h.channels & " out of range 1..=56")
  if h.width < 1 or h.height < 1:
    raise invalid("image dimensions " & $h.width & "x" & $h.height & " must be at least 1x1")
  if h.width > MaxDimension or h.height > MaxDimension: raise limitExceeded("image dimensions exceed 300000")
  if h.depth notin [1, 8, 16, 32]: raise invalid("unsupported bit depth " & $h.depth)

proc readHeader*(r: var Reader): Header =
  let sig = r.fourcc()
  if sig != "8BPS": raise badSignature("8BPS", sig)
  let v = r.u16()
  case v
  of 1: result.version = vPsd
  of 2: result.version = vPsb
  else: raise psdError(peUnsupportedVersion, $v)
  for i in 0..5: result.reserved[i] = r.u8()
  result.channels = int(r.u16())
  result.height = int(r.u32())
  result.width = int(r.u32())
  result.depth = int(r.u16())
  result.colorMode = PsdColorMode(r.u16())
  result.validate()

proc write*(h: Header; o: var seq[byte]) =
  o.put("8BPS")
  o.putU16(uint16(ord(h.version)))
  o.put(h.reserved)
  o.putU16(uint16(h.channels))
  o.putU32(uint32(h.height))
  o.putU32(uint32(h.width))
  o.putU16(uint16(h.depth))
  o.putU16(uint16(h.colorMode))

# ---------------------------------------------------------------- blend keys

const PsdBlendKeys* = ["pass", "norm", "diss", "dark", "mul ", "idiv", "lbrn", "dkCl", "lite", "scrn",
  "div ", "lddg", "lgCl", "over", "sLit", "hLit", "vLit", "lLit", "pLit", "hMix", "diff", "smud",
  "fsub", "fdiv", "hue ", "sat ", "colr", "lum "]
  ## Photoshop blend mode keys (pass-through first, then the 27 layer modes in menu order).

# ---------------------------------------------------------------- image resources

const
  ridResolutionInfo* = 1005'u16
  ridLayerState* = 1024'u16
  ridWorkPath* = 1025'u16
  ridLayerGroupInfo* = 1026'u16
  ridThumbnailPs4* = 1033'u16
  ridThumbnail* = 1036'u16
  ridGlobalAngle* = 1037'u16
  ridIccProfile* = 1039'u16
  ridGlobalAltitude* = 1049'u16
  ridSlices* = 1050'u16
  ridVersionInfo* = 1057'u16
  ridExif* = 1058'u16
  ridXmp* = 1060'u16

type
  ImageResource* = object
    signature*: FourCC
    id*: uint16
    name*: seq[byte]
    data*: seq[byte]
  ResolutionInfo* = object
    hResFixed*: uint32
    hResUnit*, widthUnit*: uint16
    vResFixed*: uint32
    vResUnit*, heightUnit*: uint16
  VersionInfo* = object
    version*: uint32
    hasRealMergedData*: bool
    writer*, reader*: UnicodeString
    fileVersion*: uint32

proc newResource*(id: uint16; data: seq[byte]): ImageResource =
  ImageResource(signature: "8BIM", id: id, data: data)

proc resolutionFromDpi*(dpi: float64): ResolutionInfo =
  let f = uint32(clamp(round(dpi * 65536.0), 0.0, float64(high(uint32))))
  ResolutionInfo(hResFixed: f, hResUnit: 1, widthUnit: 1, vResFixed: f, vResUnit: 1, heightUnit: 1)

proc hRes*(r: ResolutionInfo): float64 = float64(r.hResFixed) / 65536.0
proc vRes*(r: ResolutionInfo): float64 = float64(r.vResFixed) / 65536.0

proc parseResolution*(b: openArray[byte]): ResolutionInfo =
  var r = newReader(b)
  ResolutionInfo(hResFixed: r.u32(), hResUnit: r.u16(), widthUnit: r.u16(), vResFixed: r.u32(),
                 vResUnit: r.u16(), heightUnit: r.u16())

proc toBytes*(x: ResolutionInfo): seq[byte] =
  result.putU32(x.hResFixed); result.putU16(x.hResUnit); result.putU16(x.widthUnit)
  result.putU32(x.vResFixed); result.putU16(x.vResUnit); result.putU16(x.heightUnit)

proc parseVersionInfo*(b: openArray[byte]): VersionInfo =
  var r = newReader(b)
  result.version = r.u32()
  result.hasRealMergedData = r.u8() != 0
  result.writer = readUstr(r)
  result.reader = readUstr(r)
  result.fileVersion = r.u32()

proc toBytes*(x: VersionInfo): seq[byte] =
  result.putU32(x.version)
  result.putU8(if x.hasRealMergedData: 1 else: 0)
  x.writer.write(result)
  x.reader.write(result)
  result.putU32(x.fileVersion)

proc versionInfoResource*(hasRealMergedData: bool): ImageResource =
  newResource(ridVersionInfo, VersionInfo(version: 1, hasRealMergedData: hasRealMergedData,
    writer: ustrNul("Adobe Photoshop"), reader: ustrNul("Adobe Photoshop CS6"), fileVersion: 1).toBytes)

proc readResource(r: var Reader): ImageResource =
  result.signature = r.fourcc()
  if result.signature notin ["8BIM", "MeSa", "AgHg", "PHUT", "DCSR"]:
    raise badSignature("8BIM resource", result.signature)
  result.id = r.u16()
  result.name = readPascal(r, 2)
  let ln = r.u32()
  result.data = r.bytesU64(uint64(ln))
  if ln mod 2 == 1: r.skip(1)

proc write*(x: ImageResource; o: var seq[byte]) =
  o.put(x.signature)
  o.putU16(x.id)
  writePascal(o, x.name, 2)
  o.putLen(uint64(x.data.len), false)
  o.put(x.data)
  if x.data.len mod 2 == 1: o.putU8(0)

proc readResourceSection*(r: var Reader): seq[ImageResource] =
  let ln = r.u32()
  var s = r.sub(uint64(ln))
  while not s.isEmpty: result.add readResource(s)

proc writeResourceSection*(o: var seq[byte]; res: openArray[ImageResource]) =
  let at = o.beginLen(false)
  for x in res: x.write(o)
  o.endLen(at, false)

# ---------------------------------------------------------------- tagged blocks

const PsbLongKeys* = ["LMsk", "Lr16", "Lr32", "Layr", "Mt16", "Mt32", "Mtrn", "Alph", "FMsk", "lnk2",
  "FEid", "FXid", "PxSD", "lnk3", "lnkE", "pths", "extd", "extn", "FELS", "cinf", "artd"]
  ## Keys whose length field is 8 bytes in PSB files (the spec's list plus observed ones).

type
  TaggedBlock* = object
    signature*: FourCC   ## `8BIM` or `8B64`
    key*: FourCC
    data*: seq[byte]
    padding*: Option[seq[byte]]  ## `none` = default zero padding (4 for global, 2 for layer blocks)
  SectionType* = distinct uint32
  SectionDivider* = object
    kind*: SectionType
    blendMode*: Option[FourCC]
    subType*: Option[uint32]

const
  stOther* = SectionType(0)
  stOpenFolder* = SectionType(1)
  stClosedFolder* = SectionType(2)
  stBoundingDivider* = SectionType(3)

proc `==`*(a, b: SectionType): bool {.borrow.}
proc isFolder*(s: SectionType): bool = s == stOpenFolder or s == stClosedFolder

proc usesLongLength*(v: Version; key: string): bool = v.isPsb and key in PsbLongKeys

proc newBlock*(key: string; data: seq[byte]): TaggedBlock = TaggedBlock(signature: "8BIM", key: key, data: data)

proc isForeignOrderSignature*(b: string): bool = b.startsWith("MIB8") or b.startsWith("46B8")
proc isForeignOrder*(b: TaggedBlock): bool = isForeignOrderSignature(b.signature)

proc defaultPadding(ln, alignment: int): int = (alignment - ln mod alignment) mod alignment

proc unicodeNameBlock*(name: string): TaggedBlock =
  var d: seq[byte]
  ustr(name).write(d)
  while d.len mod 4 != 0: d.add 0
  newBlock("luni", d)

proc sectionDividerBlock*(kind: SectionType; blendMode = none(FourCC); subType = none(uint32)): TaggedBlock =
  var d: seq[byte]
  d.putU32(uint32(kind))
  if blendMode.isSome:
    d.put("8BIM")
    d.put(blendMode.get)
    if subType.isSome: d.putU32(subType.get)
  newBlock("lsct", d)

proc layerIdBlock*(id: uint32): TaggedBlock =
  var d: seq[byte]
  d.putU32(id)
  newBlock("lyid", d)

proc flagBlock*(key: string; v: uint8): TaggedBlock = newBlock(key, @[v, 0, 0, 0])
proc fillOpacityBlock*(v: uint8): TaggedBlock = flagBlock("iOpa", v)
proc protectionBlock*(flags: uint32): TaggedBlock =
  var d: seq[byte]
  d.putU32(flags)
  newBlock("lspf", d)
proc sheetColorBlock*(c: uint16): TaggedBlock =
  var d: seq[byte]
  d.putU16(c)
  d.add [0'u8, 0, 0, 0, 0, 0]
  newBlock("lclr", d)

proc parseSection*(d: openArray[byte]): SectionDivider =
  var r = newReader(d)
  result.kind = SectionType(r.u32())
  if r.remaining >= 8:
    let sig = r.fourcc()
    if sig != "8BIM": raise badSignature("8BIM", sig)
    result.blendMode = some(r.fourcc())
    if r.remaining >= 4: result.subType = some(r.u32())

proc isSig(r: Reader; at: int): bool =
  ## Whether a block signature starts at absolute position `at`.
  if at + 4 > r.size: return false
  var s = newString(4)
  for i in 0..3: s[i] = char(r.at(at + i))
  s == "8BIM" or s == "8B64" or isForeignOrderSignature(s)

type ConsumeFn* = proc (index: int; signature, key: FourCC; data: openArray[byte]; padding: Option[seq[byte]]): bool

proc readBlocksWith*(r: var Reader; version: Version; alignment: int; consume: ConsumeFn): (seq[TaggedBlock], seq[byte]) =
  ## Reads tagged blocks until the reader is exhausted. Bytes that do not form a block are
  ## returned as trailing data. `consume` may claim a block (returning true) before it is copied.
  var blocks: seq[TaggedBlock]
  while true:
    if r.remaining < 12 or not r.isSig(r.pos):
      let trailing = r.peekRest()
      r.pos = r.size
      return (blocks, trailing)
    let signature = r.fourcc()
    let key = r.fourcc()
    let ln = r.lenField(usesLongLength(version, key))
    if ln > uint64(r.remaining): raise eof(r.pos, int(min(ln - uint64(r.remaining), uint64(high(int)))))
    let dataAt = r.pos
    let dataLen = int(ln)
    r.pos += dataLen
    let restLen = r.remaining
    var pad = -1
    for k in 0..3:
      if k == restLen or (k < restLen and r.isSig(r.pos + k)):
        pad = k
        break
    let def = defaultPadding(dataLen, alignment)
    if pad < 0:
      var zeros = def <= restLen
      if zeros:
        for k in 0 ..< def:
          if r.at(r.pos + k) != 0: zeros = false
      pad = if zeros: def else: 0
    let padBytes = r.bytes(pad)
    var allZero = true
    for b in padBytes:
      if b != 0: allZero = false
    let padding = if padBytes.len == def and allZero: none(seq[byte]) else: some(padBytes)
    let claimed = consume != nil and consume(blocks.len, signature, key,
      toOpenArray(r.dataPtr, dataAt, dataAt + dataLen - 1), padding)
    if not claimed:
      var data = newSeq[byte](dataLen)
      if dataLen > 0: copyMem(addr data[0], addr r.dataPtr[dataAt], dataLen)
      blocks.add TaggedBlock(signature: signature, key: key, data: data, padding: padding)

proc readBlocks*(r: var Reader; version: Version): (seq[TaggedBlock], seq[byte]) =
  readBlocksWith(r, version, 2, nil)

proc write*(b: TaggedBlock; o: var seq[byte]; version: Version; alignment: int) =
  o.put(b.signature)
  o.put(b.key)
  o.putLen(uint64(b.data.len), usesLongLength(version, b.key))
  o.put(b.data)
  if b.padding.isSome: o.put(b.padding.get)
  else:
    for _ in 0 ..< defaultPadding(b.data.len, alignment): o.add 0

proc writeBlocks*(o: var seq[byte]; blocks: openArray[TaggedBlock]; version: Version; alignment: int) =
  for b in blocks: b.write(o, version, alignment)

proc encodedLen*(b: TaggedBlock; version: Version): int =
  let l = if usesLongLength(version, b.key): 8 else: 4
  let p = if b.padding.isSome: b.padding.get.len else: defaultPadding(b.data.len, 2)
  8 + l + b.data.len + p

# ---------------------------------------------------------------- layers

type
  PsdRect* = object
    top*, left*, bottom*, right*: int32
  LayerFlags* = distinct uint8
  MaskParameters* = object
    flags*: uint8
    userDensity*: Option[uint8]
    userFeather*: Option[float64]
    vectorDensity*: Option[uint8]
    vectorFeather*: Option[float64]
  RealMask* = object
    flags*, background*: uint8
    rect*: PsdRect
  LayerMask* = object
    rect*: PsdRect
    defaultColor*, flags*: uint8
    parameters*: Option[MaskParameters]
    real*: Option[RealMask]
    trailing*: seq[byte]
    realFirst*: bool
  MaskDataKind* = enum mdNone, mdMask, mdRaw
  MaskData* = object
    case kind*: MaskDataKind
    of mdNone: discard
    of mdMask: mask*: LayerMask
    of mdRaw: raw*: seq[byte]
  BlendingRanges* = object
    data*: seq[byte]
  BlendRange* = object
    source*, dest*: array[4, uint8]
  ChannelData* = object
    id*: int16
    compression*: Option[Compression]  ## `none` when the stored length was 0
    data*: seq[byte]
  LayerRecord* = object
    rect*: PsdRect
    channels*: seq[ChannelData]
    blendMode*: FourCC
    opacity*, clipping*: uint8
    flags*: LayerFlags
    filler*: uint8
    mask*: MaskData
    blendingRanges*: BlendingRanges
    legacyName*: seq[byte]  ## Pascal name bytes (padded to 4 in the file); see `name`
    blocks*: seq[TaggedBlock]
    extraTrailing*: seq[byte]
  LayerInfo* = object
    mergedAlpha*: bool
    layers*: seq[LayerRecord]
    padding*: Option[seq[byte]]

const
  lfTransparencyProtected* = 1'u8
  lfHidden* = 2'u8
  lfObsolete* = 4'u8
  lfBit4Useful* = 8'u8
  lfPixelDataIrrelevant* = 16'u8
  mfRelative* = 1'u8
  mfDisabled* = 2'u8
  mfInvert* = 4'u8
  mfParameters* = 16'u8

proc `==`*(a, b: LayerFlags): bool {.borrow.}
proc has*(f: LayerFlags; bit: uint8): bool = (uint8(f) and bit) != 0
proc hidden*(f: LayerFlags): bool = f.has(lfHidden)
proc setBit*(f: var LayerFlags; bit: uint8; on: bool) =
  f = LayerFlags(if on: uint8(f) or bit else: uint8(f) and not bit)
proc setHidden*(f: var LayerFlags; v: bool) = f.setBit(lfHidden, v)

proc `==`*(a, b: MaskData): bool =
  if a.kind != b.kind: return false
  case a.kind
  of mdNone: true
  of mdMask: a.mask == b.mask
  of mdRaw: a.raw == b.raw

proc rectXywh*(left, top: int32; width, height: int): PsdRect =
  let w = int32(min(width, high(int32)))
  let h = int32(min(height, high(int32)))
  PsdRect(top: top, left: left, bottom: int32(clamp(int64(top) + h, int64(low(int32)), int64(high(int32)))),
          right: int32(clamp(int64(left) + w, int64(low(int32)), int64(high(int32)))))
proc width*(r: PsdRect): int64 = int64(r.right) - int64(r.left)
proc height*(r: PsdRect): int64 = int64(r.bottom) - int64(r.top)
proc isEmpty*(r: PsdRect): bool = r.width <= 0 or r.height <= 0
proc size*(r: PsdRect): (int, int) =
  let (w, h) = (r.width, r.height)
  if w < 0 or h < 0: raise invalid("rectangle has negative size")
  if w > MaxDimension or h > MaxDimension: raise limitExceeded("layer dimensions exceed 300000")
  (int(w), int(h))
proc readRect(r: var Reader): PsdRect =
  result.top = r.i32(); result.left = r.i32(); result.bottom = r.i32(); result.right = r.i32()
proc write(x: PsdRect; o: var seq[byte]) =
  o.putI32(x.top); o.putI32(x.left); o.putI32(x.bottom); o.putI32(x.right)

proc newLayerMask*(rect: PsdRect; defaultColor, flags: uint8): LayerMask =
  LayerMask(rect: rect, defaultColor: defaultColor, flags: flags and not mfParameters, trailing: @[0'u8, 0], realFirst: true)
proc disabled*(m: LayerMask): bool = (m.flags and mfDisabled) != 0

proc maskParameterBytes*(pf: uint8): int =
  int((pf and 1) != 0) + 8 * int((pf and 2) != 0) + int((pf and 4) != 0) + 8 * int((pf and 8) != 0)

proc realBeforeParameters*(d: openArray[byte]): bool =
  ## Whether a mask record with parameters stores the real-mask fields first (Photoshop's order).
  template fits(pfAt: int): bool =
    pfAt < d.len and (let mb = maskParameterBytes(d[pfAt]); d.len >= pfAt + 1 + mb and d.len <= pfAt + 4 + mb)
  if fits(18): return false
  if not fits(36): return false
  template knownFlags(pf: uint8): bool = (pf and 0xF0) == 0
  template realBgOk(at: int): bool = at + 1 < d.len and (d[at + 1] == 0 or d[at + 1] == 255)
  let photoshop = realBgOk(18) and 36 < d.len and knownFlags(d[36])
  var spec = false
  if 18 < d.len:
    let pf = d[18]
    let realAt = 19 + maskParameterBytes(pf)
    spec = knownFlags(pf) and d.len >= realAt + 18 and d.len <= realAt + 21 and realBgOk(realAt)
  photoshop or not spec

proc parseLayerMask(d: openArray[byte]): LayerMask =
  var r = newReader(d)
  result.rect = readRect(r)
  result.defaultColor = r.u8()
  result.flags = r.u8()
  template readReal(): Option[RealMask] =
    if r.remaining >= 18:
      let f = r.u8()
      let bg = r.u8()
      some(RealMask(flags: f, background: bg, rect: readRect(r)))
    else: none(RealMask)
  let hasParams = (result.flags and mfParameters) != 0
  let realFirst = hasParams and realBeforeParameters(d)
  if realFirst: result.real = readReal()
  if hasParams:
    var p = MaskParameters(flags: r.u8())
    if (p.flags and 1) != 0: p.userDensity = some(r.u8())
    if (p.flags and 2) != 0: p.userFeather = some(r.f64())
    if (p.flags and 4) != 0: p.vectorDensity = some(r.u8())
    if (p.flags and 8) != 0: p.vectorFeather = some(r.f64())
    result.parameters = some(p)
  if not realFirst: result.real = readReal()
  result.realFirst = realFirst or result.real.isNone or result.parameters.isNone
  result.trailing = r.peekRest()

proc write(m: LayerMask; o: var seq[byte]) =
  m.rect.write(o)
  o.putU8(m.defaultColor)
  o.putU8(m.flags)
  template writeReal() =
    if m.real.isSome:
      o.putU8(m.real.get.flags)
      o.putU8(m.real.get.background)
      m.real.get.rect.write(o)
  if m.realFirst: writeReal()
  if m.parameters.isSome:
    let p = m.parameters.get
    o.putU8(p.flags)
    if p.userDensity.isSome: o.putU8(p.userDensity.get)
    if p.userFeather.isSome: o.putF64(p.userFeather.get)
    if p.vectorDensity.isSome: o.putU8(p.vectorDensity.get)
    if p.vectorFeather.isSome: o.putF64(p.vectorFeather.get)
  if not m.realFirst: writeReal()
  o.put(m.trailing)

proc fullBlendingRanges*(channels: int): BlendingRanges =
  for _ in 0 .. channels: result.data.add [0'u8, 0, 255, 255, 0, 0, 255, 255]

proc ranges*(b: BlendingRanges): seq[BlendRange] =
  var i = 0
  while i + 8 <= b.data.len:
    var x: BlendRange
    for k in 0..3:
      x.source[k] = b.data[i + k]
      x.dest[k] = b.data[i + 4 + k]
    result.add x
    i += 8

proc encodeChannel*(id: int16; c: Compression; decoded: openArray[byte]; width, height, depth: int; version: Version): ChannelData =
  let l = PlaneLayout(planes: 1, width: width, height: height, depth: depth, version: version)
  ChannelData(id: id, compression: some(c), data: encodePlanes(c, decoded, l))

proc decode*(ch: ChannelData; width, height, depth: int; version: Version): seq[byte] =
  let l = PlaneLayout(planes: 1, width: width, height: height, depth: depth, version: version)
  if ch.compression.isSome: decodePlanes(ch.compression.get, ch.data, l)
  elif l.decodedLen == 0: @[]
  else: raise invalid("channel has no data")

proc storedLen(ch: ChannelData): uint64 =
  if ch.compression.isSome: 2'u64 + uint64(ch.data.len) else: 0

proc defaultLayerRecord*(): LayerRecord =
  LayerRecord(blendMode: "norm", opacity: 255, mask: MaskData(kind: mdNone))

proc unicodeName*(b: TaggedBlock): Option[string] =
  if b.key != "luni": return
  try:
    var r = newReader(b.data)
    some($readUstr(r))
  except PsdError: none(string)

proc name*(l: LayerRecord): string =
  ## Layer name, preferring the Unicode `luni` block over the legacy Pascal name.
  for b in l.blocks:
    if b.key == "luni":
      let n = b.unicodeName
      if n.isSome: return n.get
  decodeLegacyName(l.legacyName)

proc blockIndex*(l: LayerRecord; key: string): int =
  for i, b in l.blocks:
    if b.key == key: return i
  -1

proc findBlock*(l: LayerRecord; key: string): Option[TaggedBlock] =
  let i = l.blockIndex(key)
  if i >= 0: some(l.blocks[i]) else: none(TaggedBlock)

proc sectionDivider*(l: LayerRecord): Option[SectionDivider] =
  for key in ["lsct", "lsdk"]:
    let b = l.findBlock(key)
    if b.isSome:
      try: return some(parseSection(b.get.data))
      except PsdError: discard
  none(SectionDivider)

proc sectionType*(l: LayerRecord): SectionType =
  let s = l.sectionDivider
  if s.isSome: s.get.kind else: stOther

proc isVisible*(l: LayerRecord): bool = not l.flags.hidden

proc blockU32(l: LayerRecord; key: string): Option[uint32] =
  let b = l.findBlock(key)
  if b.isNone or b.get.data.len < 4: return
  some(beU32(b.get.data, 0))

proc blockU8(l: LayerRecord; key: string): Option[uint8] =
  let b = l.findBlock(key)
  if b.isNone or b.get.data.len < 1: return
  some(b.get.data[0])

proc layerId*(l: LayerRecord): Option[uint32] = l.blockU32("lyid")
proc fillOpacity*(l: LayerRecord): uint8 =
  let v = l.blockU8("iOpa")
  if v.isSome: v.get else: 255
proc protection*(l: LayerRecord): uint32 =
  let v = l.blockU32("lspf")
  if v.isSome: v.get else: 0
proc flagBlockValue*(l: LayerRecord; key: string): Option[bool] =
  let v = l.blockU8(key)
  if v.isSome: some(v.get != 0) else: none(bool)
proc knockout*(l: LayerRecord): uint8 =
  let v = l.blockU8("knko")
  if v.isSome: v.get else: 0
proc sheetColor*(l: LayerRecord): uint16 =
  let b = l.findBlock("lclr")
  if b.isSome and b.get.data.len >= 2: beU16(b.get.data, 0) else: 0

proc layerMask*(l: LayerRecord): Option[LayerMask] =
  if l.mask.kind == mdMask: some(l.mask.mask) else: none(LayerMask)

proc channelIndex*(l: LayerRecord; id: int16): int =
  for i, c in l.channels:
    if c.id == id: return i
  -1

proc hasChannel*(l: LayerRecord; id: int16): bool = l.channelIndex(id) >= 0

proc channelRect*(l: LayerRecord; id: int16): PsdRect =
  if l.mask.kind == mdMask:
    if id == ChannelUserMask: return l.mask.mask.rect
    if id == ChannelRealUserMask:
      return (if l.mask.mask.real.isSome: l.mask.mask.real.get.rect else: l.mask.mask.rect)
  l.rect

proc decodeChannel*(l: LayerRecord; id: int16; depth: int; version: Version): seq[byte] =
  let i = l.channelIndex(id)
  if i < 0: raise invalid("layer has no channel " & $id)
  let (w, h) = l.channelRect(id).size
  l.channels[i].decode(w, h, depth, version)

proc readLayerRecord(r: var Reader; version: Version): (LayerRecord, seq[uint64]) =
  var rec = defaultLayerRecord()
  rec.rect = readRect(r)
  let nch = int(r.u16())
  if nch > MaxLayerChannels: raise limitExceeded("too many channels in layer")
  var lens: seq[uint64]
  for _ in 0 ..< nch:
    let id = r.i16()
    let ln = r.lenField(version.isPsb)
    rec.channels.add ChannelData(id: id)
    lens.add ln
  let sig = r.fourcc()
  if sig != "8BIM": raise badSignature("8BIM (layer blend mode)", sig)
  rec.blendMode = r.fourcc()
  rec.opacity = r.u8()
  rec.clipping = r.u8()
  rec.flags = LayerFlags(r.u8())
  rec.filler = r.u8()
  let extraLen = r.u32()
  var x = r.sub(uint64(extraLen))
  let maskLen = x.u32()
  let maskBytes = x.bytesU64(uint64(maskLen))
  if maskBytes.len == 0: rec.mask = MaskData(kind: mdNone)
  else:
    try: rec.mask = MaskData(kind: mdMask, mask: parseLayerMask(maskBytes))
    except PsdError: rec.mask = MaskData(kind: mdRaw, raw: maskBytes)
  let brLen = x.u32()
  rec.blendingRanges = BlendingRanges(data: x.bytesU64(uint64(brLen)))
  rec.legacyName = readPascal(x, 4)
  let (blocks, trailing) = readBlocks(x, version)
  rec.blocks = blocks
  rec.extraTrailing = trailing
  (rec, lens)

proc write(l: LayerRecord; o: var seq[byte]; version: Version) =
  l.rect.write(o)
  if l.channels.len > 65535: raise limitExceeded("too many channels")
  o.putU16(uint16(l.channels.len))
  for c in l.channels:
    o.putI16(c.id)
    o.putLen(c.storedLen, version.isPsb)
  o.put("8BIM")
  o.put(if l.blendMode.len == 4: l.blendMode else: "norm")
  o.putU8(l.opacity)
  o.putU8(l.clipping)
  o.putU8(uint8(l.flags))
  o.putU8(l.filler)
  let at = o.beginLen(false)
  let m = o.beginLen(false)
  case l.mask.kind
  of mdNone: discard
  of mdMask: l.mask.mask.write(o)
  of mdRaw: o.put(l.mask.raw)
  o.endLen(m, false)
  o.putLen(uint64(l.blendingRanges.data.len), false)
  o.put(l.blendingRanges.data)
  writePascal(o, l.legacyName, 4)
  writeBlocks(o, l.blocks, version, 2)
  o.put(l.extraTrailing)
  o.endLen(at, false)

proc readLayerInfoBody*(data: openArray[byte]; version: Version): LayerInfo =
  var r = newReader(data)
  let count = int(r.i16())
  result.mergedAlpha = count < 0
  let n = abs(count)
  r.checkCount(uint64(n), 34)
  var allLens: seq[seq[uint64]]
  for _ in 0 ..< n:
    let (rec, lens) = readLayerRecord(r, version)
    result.layers.add rec
    allLens.add lens
  for li in 0 ..< result.layers.len:
    for ci in 0 ..< result.layers[li].channels.len:
      let ln = allLens[li][ci]
      if ln == 0: continue
      if ln == 1: raise invalid("channel data length 1")
      result.layers[li].channels[ci].compression = some(Compression(code: r.u16()))
      result.layers[li].channels[ci].data = r.bytesU64(ln - 2)
  let bodyLen = r.pos
  let rest = r.peekRest()
  var allZero = true
  for b in rest:
    if b != 0: allZero = false
  result.padding = if rest.len == bodyLen mod 2 and allZero: none(seq[byte]) else: some(rest)

proc writeLayerInfoBody*(info: LayerInfo; o: var seq[byte]; version: Version) =
  let start = o.len
  if info.layers.len > 32767: raise limitExceeded("more than 32767 layers")
  let n = int16(info.layers.len)
  o.putI16(if info.mergedAlpha: -n else: n)
  for l in info.layers: l.write(o, version)
  for l in info.layers:
    for c in l.channels:
      if c.compression.isSome:
        o.putU16(c.compression.get.code)
        o.put(c.data)
  if info.padding.isSome: o.put(info.padding.get)
  elif (o.len - start) mod 2 == 1: o.putU8(0)

proc unpaddedLen*(info: LayerInfo; version: Version): uint64 =
  var records: seq[byte]
  records.putI16(0)
  for l in info.layers: l.write(records, version)
  result = uint64(records.len)
  for l in info.layers:
    for c in l.channels:
      if c.compression.isSome: result += 2'u64 + uint64(c.data.len)

proc padTo*(info: var LayerInfo; version: Version; align: uint64) =
  ## Pads the body to a multiple of `align` (Photoshop uses 4).
  let a = max(align, 1'u64)
  let ln = info.unpaddedLen(version)
  info.padding = some(newSeq[byte](int((a - ln mod a) mod a)))

# ---------------------------------------------------------------- merged image

type ImageData* = object
  compression*: Compression
  data*: seq[byte]   ## encoded bytes of all channels (to the end of the file)

proc mergedLayout*(h: Header): PlaneLayout =
  PlaneLayout(planes: h.channels, width: h.width, height: h.height, depth: h.depth, version: h.version)

proc encodeImageData*(c: Compression; decoded: openArray[byte]; h: Header): ImageData =
  ImageData(compression: c, data: encodePlanes(c, decoded, mergedLayout(h)))

proc decode*(img: ImageData; h: Header): seq[byte] = decodePlanes(img.compression, img.data, mergedLayout(h))

# ---------------------------------------------------------------- file

type
  PlacementKind* = enum lpSection, lpGlobalBlock
  LayerInfoPlacement* = object
    case kind*: PlacementKind
    of lpSection: discard
    of lpGlobalBlock:
      index*: int
      signature*, key*: FourCC
      padding*: Option[seq[byte]]
  GlobalLayerMask* = object
    data*: seq[byte]
  PsdFile* = object
    header*: Header
    colorModeData*: seq[byte]
    resources*: seq[ImageResource]
    layerInfo*: Option[LayerInfo]
    layerInfoPlacement*: LayerInfoPlacement
    globalLayerMask*: Option[GlobalLayerMask]
    globalBlocks*: seq[TaggedBlock]
    layerMaskTrailing*: seq[byte]
    imageData*: ImageData

proc `==`*(a, b: LayerInfoPlacement): bool =
  if a.kind != b.kind: return false
  case a.kind
  of lpSection: true
  of lpGlobalBlock: a.index == b.index and a.signature == b.signature and a.key == b.key and a.padding == b.padding

proc parsePsd*(data: openArray[byte]): PsdFile =
  ## Parses a PSD or PSB file. Raises `PsdError` on malformed input.
  var r = newReader(data)
  var f: PsdFile
  f.header = readHeader(r)
  let psb = f.header.version.isPsb
  let cmdLen = r.u32()
  f.colorModeData = r.bytesU64(uint64(cmdLen))
  f.resources = readResourceSection(r)
  let lmLen = r.lenField(psb)
  var s = r.sub(lmLen)
  f.layerInfoPlacement = LayerInfoPlacement(kind: lpSection)
  if not s.isEmpty:
    let liLen = s.lenField(psb)
    if liLen > 0:
      var li = s.sub(liLen)
      f.layerInfo = some(readLayerInfoBody(li.restView, f.header.version))
    if s.remaining >= 4:
      let n = s.u32()
      f.globalLayerMask = some(GlobalLayerMask(data: s.bytesU64(uint64(n))))
      var candidateSeen = f.layerInfo.isSome
      var found: Option[LayerInfo]
      var placement = LayerInfoPlacement(kind: lpSection)
      let version = f.header.version
      let consume: ConsumeFn = proc (index: int; signature, key: FourCC; d: openArray[byte]; padding: Option[seq[byte]]): bool =
        if candidateSeen or key notin ["Lr16", "Lr32", "Layr"]: return false
        candidateSeen = true
        try:
          found = some(readLayerInfoBody(d, version))
        except PsdError:
          return false
        placement = LayerInfoPlacement(kind: lpGlobalBlock, index: index, signature: signature, key: key, padding: padding)
        true
      let (blocks, trailing) = readBlocksWith(s, version, 4, consume)
      if found.isSome:
        f.layerInfo = found
        f.layerInfoPlacement = placement
      f.globalBlocks = blocks
      f.layerMaskTrailing = trailing
    else:
      f.layerMaskTrailing = s.peekRest()
  f.imageData.compression = Compression(code: r.u16())
  validatePlanes(f.imageData.compression, r.restView, mergedLayout(f.header))
  f.imageData.data = r.peekRest()
  f

proc toBytes*(f: PsdFile): seq[byte] =
  ## Serializes the file.
  let v = f.header.version
  let psb = v.isPsb
  var o: seq[byte]
  f.header.write(o)
  o.putLen(uint64(f.colorModeData.len), false)
  o.put(f.colorModeData)
  writeResourceSection(o, f.resources)
  let inBlock = f.layerInfoPlacement.kind == lpGlobalBlock
  let sectionEmpty = f.layerInfo.isNone and f.globalLayerMask.isNone and f.globalBlocks.len == 0 and f.layerMaskTrailing.len == 0
  let at = o.beginLen(psb)
  if not sectionEmpty:
    let li = o.beginLen(psb)
    if f.layerInfo.isSome and not inBlock: writeLayerInfoBody(f.layerInfo.get, o, v)
    o.endLen(li, psb)
    let needMask = f.globalLayerMask.isSome or f.globalBlocks.len > 0 or (inBlock and f.layerInfo.isSome)
    if needMask:
      let gm = if f.globalLayerMask.isSome: f.globalLayerMask.get.data else: @[]
      o.putLen(uint64(gm.len), false)
      o.put(gm)
    if f.layerInfo.isSome and inBlock:
      let p = f.layerInfoPlacement
      let split = min(p.index, f.globalBlocks.len)
      writeBlocks(o, f.globalBlocks.toOpenArray(0, split - 1), v, 4)
      o.put(p.signature)
      o.put(p.key)
      let long = usesLongLength(v, p.key)
      let at2 = o.beginLen(long)
      let start = o.len
      writeLayerInfoBody(f.layerInfo.get, o, v)
      let ln = o.len - start
      o.endLen(at2, long)
      if p.padding.isSome: o.put(p.padding.get)
      else:
        for _ in 0 ..< (4 - ln mod 4) mod 4: o.add 0
      writeBlocks(o, f.globalBlocks.toOpenArray(split, f.globalBlocks.len - 1), v, 4)
    else:
      writeBlocks(o, f.globalBlocks, v, 4)
    o.put(f.layerMaskTrailing)
  o.endLen(at, psb)
  o.putU16(f.imageData.compression.code)
  o.put(f.imageData.data)
  o

proc layers*(f: PsdFile): seq[LayerRecord] =
  if f.layerInfo.isSome: f.layerInfo.get.layers else: @[]

proc layerCount*(f: PsdFile): int =
  if f.layerInfo.isSome: f.layerInfo.get.layers.len else: 0

proc resource*(f: PsdFile; id: uint16): Option[ImageResource] =
  for x in f.resources:
    if x.id == id: return some(x)

proc globalBlock*(f: PsdFile; key: string): Option[TaggedBlock] =
  for b in f.globalBlocks:
    if b.key == key: return some(b)

proc iccProfile*(f: PsdFile): Option[seq[byte]] =
  let r = f.resource(ridIccProfile)
  if r.isSome: some(r.get.data) else: none(seq[byte])

proc resolution*(f: PsdFile): Option[ResolutionInfo] =
  let r = f.resource(ridResolutionInfo)
  if r.isNone: return
  try: some(parseResolution(r.get.data))
  except PsdError: none(ResolutionInfo)

proc hasRealMergedData*(f: PsdFile): Option[bool] =
  let r = f.resource(ridVersionInfo)
  if r.isNone: return
  try: some(parseVersionInfo(r.get.data).hasRealMergedData)
  except PsdError: none(bool)

proc unreadableLayerBlock*(f: PsdFile): Option[(TaggedBlock, string)] =
  ## The global `Lr16`/`Lr32`/`Layr` block whose layers could not be parsed, with the error.
  if f.layerInfo.isSome: return
  for b in f.globalBlocks:
    if b.key in ["Lr16", "Lr32", "Layr"]:
      try: discard readLayerInfoBody(b.data, f.header.version)
      except PsdError as e: return some((b, e.msg))
      return

proc decodeMerged*(f: PsdFile): seq[byte] = f.imageData.decode(f.header)

proc mergedHasAlpha*(f: PsdFile): bool =
  let color = f.header.colorMode.colorChannels
  if color.isNone or f.header.channels <= color.get: return false
  if f.layerInfo.isSome: f.layerInfo.get.mergedAlpha else: true

proc validate*(f: PsdFile) =
  f.header.validate()
  if f.header.version == vPsd and (f.header.width > 30_000 or f.header.height > 30_000):
    raise limitExceeded("PSD dimensions exceed 30000; use PSB")
  if f.header.colorMode == pcmIndexed and f.colorModeData.len != 768:
    raise invalid("indexed color mode requires a 768-byte palette")

# ---------------------------------------------------------------- layer tree

type
  LayerNode* = object
    index*: int                ## record index (the folder record for groups)
    group*: bool
    divider*: int              ## bounding divider index, -1 when none
    children*: seq[LayerNode]  ## bottom-to-top

proc count*(n: LayerNode): int =
  result = 1
  for c in n.children: result += c.count

proc layerTree*(f: PsdFile; maxDepth = high(int)): seq[LayerNode] =
  ## Nested layer tree from section dividers, in file order (bottom-to-top). Built iteratively;
  ## malformed nesting is tolerated (unmatched folders become empty groups, unclosed dividers are
  ## dropped and their children hoisted). Groups nested `maxDepth` deep keep no children.
  var stack: seq[(int, seq[LayerNode])] = @[(-1, newSeq[LayerNode]())]
  template push(node: LayerNode) =
    if stack.len <= (if maxDepth == high(int): high(int) else: maxDepth + 1):
      stack[^1][1].add node
  let ls = f.layers
  for i, rec in ls:
    let st = rec.sectionType
    if st == stBoundingDivider: stack.add (i, newSeq[LayerNode]())
    elif st.isFolder:
      var divider = -1
      var children: seq[LayerNode]
      if stack.len > 1:
        let top = stack.pop()
        divider = top[0]
        children = top[1]
      push(LayerNode(index: i, group: true, divider: divider, children: children))
    else: push(LayerNode(index: i, divider: -1))
  while stack.len > 1:
    let top = stack.pop()
    stack[^1][1].add top[1]
  stack[0][1]

# ---------------------------------------------------------------- pixels

proc samplesU16*(b: openArray[byte]): seq[uint16] =
  result = newSeq[uint16](b.len div 2)
  for i in 0 ..< result.len: result[i] = beU16(b, 2 * i)

proc samplesF32*(b: openArray[byte]): seq[float32] =
  result = newSeq[float32](b.len div 4)
  for i in 0 ..< result.len: result[i] = cast[float32](beU32(b, 4 * i))

proc u16ToBytes*(s: openArray[uint16]): seq[byte] =
  for v in s: result.putU16(v)

proc f32ToBytes*(s: openArray[float32]): seq[byte] =
  for v in s: result.putU32(cast[uint32](v))

proc unpackBits*(b: openArray[byte]; width, height: int): seq[byte] =
  let rb = (width + 7) div 8
  result = newSeqOfCap[byte](width * height)
  for y in 0 ..< height:
    for x in 0 ..< width:
      let i = y * rb + x div 8
      let v = if i < b.len: b[i] else: 0'u8
      result.add (v shr (7 - (x mod 8))) and 1

proc planeToU8*(b: openArray[byte]; depth, width, height: int): seq[byte] =
  case depth
  of 1:
    result = unpackBits(b, width, height)
    for v in result.mitems: v = (if v == 1: 0'u8 else: 255'u8)
  of 8: result = @b
  of 16:
    result = newSeq[byte](b.len div 2)
    for i in 0 ..< result.len: result[i] = byte((uint32(beU16(b, 2 * i)) * 255 + 32767) div 65535)
  of 32:
    result = newSeq[byte](b.len div 4)
    for i in 0 ..< result.len:
      let v = cast[float32](beU32(b, 4 * i))
      result[i] = if v != v: 0'u8 else: byte(round(clamp(v, 0'f32, 1'f32) * 255))
  else: raise unsupported("depth " & $depth)

type
  RgbaImage* = object
    left*, top*: int32
    width*, height*: int
    data*: seq[byte]
  GrayImage* = object
    left*, top*: int32
    width*, height*: int
    data*: seq[byte]
    defaultValue*: uint8

proc colorChannelCount(mode: PsdColorMode): int =
  case uint16(mode)
  of 3: 3
  of 4: 4
  of 0, 1, 2, 8: 1
  else: raise unsupported("rgba8 for color mode " & $mode)

proc interleave(mode: PsdColorMode; color: seq[seq[byte]]; alpha: Option[seq[byte]]; palette: openArray[byte]; n: int): seq[byte] =
  if color.len != colorChannelCount(mode): raise invalid("channel size mismatch")
  for p in color:
    if p.len != n: raise invalid("channel size mismatch")
  if alpha.isSome and alpha.get.len != n: raise invalid("channel size mismatch")
  if uint64(n) * 4 > MaxDecodedBytes: raise limitExceeded("RGBA output exceeds MAX_DECODED_BYTES")
  result = newSeq[byte](n * 4)
  for i in 0 ..< n:
    var r, g, b: byte
    case uint16(mode)
    of 3: (r, g, b) = (color[0][i], color[1][i], color[2][i])
    of 1, 0, 8: (r, g, b) = (color[0][i], color[0][i], color[0][i])
    of 2:
      let idx = int(color[0][i])
      template p(k: int): byte = (if k * 256 + idx < palette.len: palette[k * 256 + idx] else: 0'u8)
      (r, g, b) = (p(0), p(1), p(2))
    of 4:
      let k = uint32(color[3][i])
      template f(c: byte): byte = byte((uint32(c) * k + 127) div 255)
      (r, g, b) = (f(color[0][i]), f(color[1][i]), f(color[2][i]))
    else: raise unsupported("rgba8 for color mode " & $mode)
    result[4 * i] = r
    result[4 * i + 1] = g
    result[4 * i + 2] = b
    result[4 * i + 3] = if alpha.isSome: alpha.get[i] else: 255

proc unmatteToU8(color, alpha: openArray[byte]; depth: int): seq[byte] =
  template convert(c, a: float32): byte =
    (let straight = (if a > 0: (c + a - 1) / a else: c); byte(round(clamp(straight, 0'f32, 1'f32) * 255)))
  case depth
  of 16:
    result = newSeq[byte](min(color.len, alpha.len) div 2)
    for i in 0 ..< result.len:
      result[i] = convert(float32(beU16(color, 2 * i)) / 65535, float32(beU16(alpha, 2 * i)) / 65535)
  of 32:
    result = newSeq[byte](min(color.len, alpha.len) div 4)
    for i in 0 ..< result.len:
      result[i] = convert(cast[float32](beU32(color, 4 * i)), cast[float32](beU32(alpha, 4 * i)))
  else: raise unsupported("RGB composite depth " & $depth)

proc layerRgba8*(f: PsdFile; index: int): RgbaImage =
  ## Layer pixels as RGBA8 (transparency as alpha; the user mask is not applied).
  let rec = f.layerInfo.get.layers[index]
  let (w, h) = rec.rect.size
  let n = w * h
  if uint64(n) * 4 > MaxDecodedBytes: raise limitExceeded("RGBA output exceeds MAX_DECODED_BYTES")
  let depth = f.header.depth
  let mode = f.header.colorMode
  let cc = colorChannelCount(mode)
  var color: seq[seq[byte]]
  for id in 0 ..< cc:
    var plane: seq[byte]
    if rec.hasChannel(int16(id)):
      let bytes = rec.decodeChannel(int16(id), depth, f.header.version)
      plane = if depth == 8: bytes else: planeToU8(bytes, depth, w, h)
    else:
      plane = newSeq[byte](n)
      if mode == pcmCmyk:
        for v in plane.mitems: v = 255
    if plane.len != n: raise invalid("channel size mismatch")
    color.add plane
  var alpha: Option[seq[byte]]
  if rec.hasChannel(ChannelTransparency):
    let bytes = rec.decodeChannel(ChannelTransparency, depth, f.header.version)
    alpha = some(if depth == 8: bytes else: planeToU8(bytes, depth, w, h))
  RgbaImage(left: rec.rect.left, top: rec.rect.top, width: w, height: h, data: interleave(mode, color, alpha, [], n))

proc layerUserMask*(f: PsdFile; index: int): Option[GrayImage] =
  ## The user mask (channel -3 if present, else -2) as 8-bit gray.
  let rec = f.layerInfo.get.layers[index]
  let id = if rec.hasChannel(ChannelRealUserMask): ChannelRealUserMask
           elif rec.hasChannel(ChannelUserMask): ChannelUserMask
           else: return none(GrayImage)
  let rect = rec.channelRect(id)
  let (w, h) = rect.size
  let data = planeToU8(rec.decodeChannel(id, f.header.depth, f.header.version), f.header.depth, w, h)
  var dv = 0'u8
  let m = rec.layerMask
  if m.isSome:
    dv = if id == ChannelRealUserMask and m.get.real.isSome: m.get.real.get.background else: m.get.defaultColor
  some(GrayImage(left: rect.left, top: rect.top, width: w, height: h, data: data, defaultValue: dv))

proc compositeRgba8*(f: PsdFile): RgbaImage =
  ## The merged composite as RGBA8 (RGB, gray, CMYK, indexed, duotone, bitmap; all depths).
  let h = f.header
  h.validate()
  let (w, hh) = (h.width, h.height)
  let n = w * hh
  let cc = colorChannelCount(h.colorMode)
  if h.channels < cc: raise invalid("fewer channels than the color mode requires")
  let all = f.decodeMerged()
  let plane = h.rowBytes * hh
  let hasAlpha = f.mergedHasAlpha
  if hasAlpha and (cc + 1) * plane > all.len: raise invalid("alpha channel size mismatch")
  proc get(i: int): seq[byte] =
    if (i + 1) * plane > all.len: raise invalid("channel size mismatch")
    if h.depth == 8: all[i * plane ..< (i + 1) * plane]
    else: planeToU8(all.toOpenArray(i * plane, (i + 1) * plane - 1), h.depth, w, hh)
  var color: seq[seq[byte]]
  for i in 0 ..< cc:
    if h.colorMode == pcmRgb and h.depth != 8 and hasAlpha:
      if (i + 1) * plane > all.len: raise invalid("channel size mismatch")
      color.add unmatteToU8(all.toOpenArray(i * plane, (i + 1) * plane - 1),
                            all.toOpenArray(cc * plane, (cc + 1) * plane - 1), h.depth)
    else: color.add get(i)
  let alpha = if hasAlpha: some(get(cc)) else: none(seq[byte])
  var data = interleave(h.colorMode, color, alpha, f.colorModeData, n)
  if h.colorMode == pcmRgb and h.depth == 8 and hasAlpha:
    for p in 0 ..< n:
      let a = uint32(data[4 * p + 3])
      if a == 0: continue
      for c in 0..2:
        let v = uint32(data[4 * p + c])
        let s = (if v + a > 255: v + a - 255 else: 0'u32) * 255 + a div 2
        data[4 * p + c] = byte(min(s div a, 255'u32))
  RgbaImage(left: 0, top: 0, width: w, height: hh, data: data)

# ---------------------------------------------------------------- shmd metadata

type MetadataItem* = object
  signature*, key*: FourCC
  copy*: bool
  reserved*: array[3, byte]
  data*: seq[byte]

proc newMetadataItem*(key: string; data: seq[byte]): MetadataItem =
  result = MetadataItem(signature: "8BIM", key: key, data: data)
  if result.data.len mod 2 == 1: result.data.add 0

proc parseShmd*(data: openArray[byte]): seq[MetadataItem] =
  var r = newReader(data)
  let count = r.u32()
  r.checkCount(uint64(count), 16)
  for _ in 0'u32 ..< count:
    var it: MetadataItem
    it.signature = r.fourcc()
    it.key = r.fourcc()
    it.copy = r.u8() != 0
    for k in 0..2: it.reserved[k] = r.u8()
    it.data = r.bytesU64(uint64(r.u32()))
    result.add it
  if not r.isEmpty: raise invalid("trailing bytes after metadata items")

proc writeShmd*(items: openArray[MetadataItem]): seq[byte] =
  result.putU32(uint32(items.len))
  for it in items:
    result.put(it.signature)
    result.put(it.key)
    result.putU8(if it.copy: 1 else: 0)
    result.put(it.reserved)
    result.putU32(uint32(it.data.len))
    result.put(it.data)
