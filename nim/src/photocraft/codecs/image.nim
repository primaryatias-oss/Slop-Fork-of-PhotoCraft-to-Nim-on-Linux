## The codecs' own flat raster image type, formats, options and errors.
##
## Samples are stored interleaved (`RGBARGBA…`), row-major, top-to-bottom, in native
## endianness. Integer samples are unsigned and normalized to `[0, max]`; float samples are
## nominally `[0, 1]` but may exceed it (HDR). Port of `crates/codecs/src/{image,format,options,error}.rs`.

import std/[options, strutils, math]
import ../cms/lut3d

type
  Format* = enum
    fmtPng = "PNG", fmtJpeg = "JPEG", fmtTiff = "TIFF", fmtWebP = "WebP", fmtGif = "GIF",
    fmtBmp = "BMP", fmtTga = "TGA", fmtIco = "ICO", fmtPnm = "Netpbm", fmtQoi = "QOI",
    fmtOpenExr = "OpenEXR", fmtHdr = "Radiance HDR", fmtAvif = "AVIF", fmtHeif = "HEIF", fmtFits = "FITS"

  CodecErrorKind* = enum ceUnknownFormat, ceUnsupported, ceMalformed, ceLimitExceeded, ceEncode, ceInvalidImage
  CodecError* = object of CatchableError
    kind*: CodecErrorKind
    format*: Format

  ChannelLayout* = enum
    ## Alpha, when present, is always the last channel and is straight (not premultiplied).
    clGray = "Gray", clGrayA = "GrayA", clRgb = "Rgb", clRgba = "Rgba", clCmyk = "Cmyk", clCmykA = "CmykA"

  ImgSample* = enum
    isU8 = "U8", isU16 = "U16", isF16 = "F16", isF32 = "F32"

  Metadata* = object
    exif*: Option[seq[byte]] ## raw EXIF in TIFF structure (no `Exif\0\0` prefix)
    xmp*: Option[string]
    dpi*: Option[(float32, float32)]
    text*: seq[(string, string)]
    photoshopResources*: Option[seq[byte]] ## TIFF tag 34377
    photoshopLayers*: Option[seq[byte]]    ## TIFF tag 37724

  DecodeWarningKind* = enum dwMoreFrames, dwMorePages, dwMoreParts, dwTruncated, dwDeepFlattened
  DecodeWarning* = object
    kind*: DecodeWarningKind
    total*: Option[int]
    format*: Format
    maxSamples*: int

  Image* = object
    width*, height*: int
    layout*: ChannelLayout
    sample*: ImgSample
    data*: seq[byte]
    icc*: Option[seq[byte]]
    meta*: Metadata
    warnings*: seq[DecodeWarning]

  Limits* = object
    maxWidth*, maxHeight*: int
    maxPixels*: int64
    maxAlloc*: int64

  DecodeOptions* = object
    limits*: Limits
    keepOrientation*: bool

  PngCompression* = enum pcNone, pcFast, pcDefault, pcBest
  TiffCompression* = enum tcNone, tcLzw, tcDeflate, tcPackBits
  ExrCompression* = enum ecNone, ecRle, ecZip1, ecZip16, ecPiz

  EncodeOptions* = object
    jpegQuality*: int
    jpegChromaSubsampling*: bool
    pngCompression*: PngCompression
    pngInterlaced*: bool
    webpLossless*: bool
    webpQuality*: int
    tiffCompression*: TiffCompression
    tiffBigtiff*: bool
    exrCompression*: ExrCompression
    embedIcc*: bool
    embedMetadata*: bool

  FormatCaps* = object
    read*, write*: bool
    depths*: seq[ImgSample]
    layouts*: seq[ChannelLayout]
    alpha*, icc*, exif*, xmp*, dpi*, text*, animation*, lossy*: bool

proc codecError*(kind: CodecErrorKind; format: Format; msg: string): ref CodecError =
  let m = case kind
    of ceUnknownFormat: "unrecognized image format"
    of ceUnsupported: $format & ": " & msg
    of ceMalformed: $format & ": malformed data: " & msg
    of ceLimitExceeded: "limit exceeded: " & msg
    of ceEncode: $format & ": encode failed: " & msg
    of ceInvalidImage: "invalid image: " & msg
  result = newException(CodecError, m)
  result.kind = kind
  result.format = format

proc malformed*(f: Format; msg: string): ref CodecError = codecError(ceMalformed, f, msg)
proc unsupported*(f: Format; msg: string): ref CodecError = codecError(ceUnsupported, f, msg)
proc encodeErr*(f: Format; msg: string): ref CodecError = codecError(ceEncode, f, msg)
proc invalidImage*(msg: string): ref CodecError = codecError(ceInvalidImage, fmtPng, msg)
proc limitExceeded*(msg: string): ref CodecError = codecError(ceLimitExceeded, fmtPng, msg)

# ---------------------------------------------------------------- layout / sample

proc channels*(l: ChannelLayout): int =
  case l
  of clGray: 1
  of clGrayA: 2
  of clRgb: 3
  of clRgba, clCmyk: 4
  of clCmykA: 5
proc hasAlpha*(l: ChannelLayout): bool = l in {clGrayA, clRgba, clCmykA}
proc colorChannels*(l: ChannelLayout): int = l.channels - (if l.hasAlpha: 1 else: 0)
proc isGray*(l: ChannelLayout): bool = l in {clGray, clGrayA}
proc isRgb*(l: ChannelLayout): bool = l in {clRgb, clRgba}
proc isCmyk*(l: ChannelLayout): bool = l in {clCmyk, clCmykA}
proc withAlpha*(l: ChannelLayout): ChannelLayout =
  case l
  of clGray, clGrayA: clGrayA
  of clRgb, clRgba: clRgba
  of clCmyk, clCmykA: clCmykA
proc withoutAlpha*(l: ChannelLayout): ChannelLayout =
  case l
  of clGray, clGrayA: clGray
  of clRgb, clRgba: clRgb
  of clCmyk, clCmykA: clCmyk
proc sameModel*(a, b: ChannelLayout): bool =
  (a.isGray and b.isGray) or (a.isRgb and b.isRgb) or (a.isCmyk and b.isCmyk)

proc bytes*(s: ImgSample): int =
  case s
  of isU8: 1
  of isU16, isF16: 2
  of isF32: 4
proc isFloat*(s: ImgSample): bool = s in {isF16, isF32}

proc isEmpty*(m: Metadata): bool =
  m.exif.isNone and m.xmp.isNone and m.dpi.isNone and m.text.len == 0 and m.photoshopResources.isNone and m.photoshopLayers.isNone

proc `$`*(w: DecodeWarning): string =
  case w.kind
  of dwMoreFrames:
    if w.total.isSome: "only the first of " & $w.total.get & " frames was imported"
    else: "only the first frame of the animation was imported"
  of dwMorePages:
    if w.total.isSome: "only the first of " & $w.total.get & " pages was imported"
    else: "only the first page of the file was imported"
  of dwMoreParts:
    if w.total.isSome: "only one of " & $w.total.get & " EXR parts was imported"
    else: "only one EXR part of the file was imported"
  of dwTruncated: $w.format & " data ends early (the file is truncated or damaged); part of the image is missing"
  of dwDeepFlattened: $w.format & " deep data was composited into a flat image"

# ---------------------------------------------------------------- limits

proc defaultLimits*(): Limits =
  Limits(maxWidth: 1 shl 18, maxHeight: 1 shl 18, maxPixels: 1'i64 shl 30, maxAlloc: 8'i64 shl 30)

proc noLimits*(): Limits = Limits(maxWidth: high(int32), maxHeight: high(int32), maxPixels: high(int64), maxAlloc: high(int64))

proc checkBytes*(l: Limits; width, height: int; bytesPerPixel: int) =
  if width <= 0 or height <= 0: raise invalidImage("zero-sized image " & $width & "x" & $height)
  if width > l.maxWidth or height > l.maxHeight:
    raise limitExceeded("dimensions " & $width & "x" & $height & " exceed max " & $l.maxWidth & "x" & $l.maxHeight)
  let pixels = int64(width) * int64(height)
  if pixels > l.maxPixels: raise limitExceeded($pixels & " pixels exceed max " & $l.maxPixels)
  let b = if bytesPerPixel > 0 and pixels > high(int64) div bytesPerPixel: high(int64) else: pixels * bytesPerPixel
  if b > l.maxAlloc: raise limitExceeded($b & " bytes exceed max_alloc " & $l.maxAlloc)

proc check*(l: Limits; width, height: int; layout: ChannelLayout; sample: ImgSample) =
  l.checkBytes(width, height, layout.channels * sample.bytes)

proc defaultDecodeOptions*(): DecodeOptions = DecodeOptions(limits: defaultLimits())

proc defaultEncodeOptions*(): EncodeOptions =
  EncodeOptions(jpegQuality: 90, jpegChromaSubsampling: true, pngCompression: pcDefault, webpLossless: true,
                webpQuality: 80, tiffCompression: tcDeflate, exrCompression: ecZip16, embedIcc: true, embedMetadata: true)

# ---------------------------------------------------------------- sample helpers

proc rdU16(d: openArray[byte]; i: int): uint16 {.inline.} = cast[ptr uint16](unsafeAddr d[2 * i])[]
proc rdF32(d: openArray[byte]; i: int): float32 {.inline.} = cast[ptr float32](unsafeAddr d[4 * i])[]

proc readNormalized*(data: openArray[byte]; sample: ImgSample; i: int): float32 {.inline.} =
  case sample
  of isU8: float32(data[i]) / 255.0'f32
  of isU16: float32(rdU16(data, i)) / 65535.0'f32
  of isF16: f16ToF32(rdU16(data, i))
  of isF32: rdF32(data, i)

proc toNormalized*(data: openArray[byte]; sample: ImgSample): seq[float32] =
  let n = data.len div sample.bytes
  result = newSeq[float32](n)
  case sample
  of isU8:
    for i in 0 ..< n: result[i] = float32(data[i]) / 255.0'f32
  else:
    for i in 0 ..< n: result[i] = readNormalized(data, sample, i)

proc clamp01(v: float32): float32 {.inline.} =
  if v != v: 0.0'f32 else: clamp(v, 0.0'f32, 1.0'f32)

proc quantize*(values: openArray[float32]; sample: ImgSample): seq[byte] =
  result = newSeq[byte](values.len * sample.bytes)
  case sample
  of isU8:
    for i, v in values: result[i] = uint8(round(clamp01(v) * 255.0'f32))
  of isU16:
    for i, v in values: cast[ptr uint16](addr result[2 * i])[] = uint16(round(clamp01(v) * 65535.0'f32))
  of isF16:
    for i, v in values: cast[ptr uint16](addr result[2 * i])[] = f32ToF16(v)
  of isF32:
    for i, v in values: cast[ptr float32](addr result[4 * i])[] = v

const Luma: array[3, float32] = [0.2126'f32, 0.7152, 0.0722]

proc convertLayout*(src: openArray[float32]; src0, to: ChannelLayout): seq[float32] =
  ## Convert normalized interleaved samples between layouts (naive, not colour-managed).
  if src0 == to: return @src
  let fc = src0.channels
  let tc = to.channels
  let n = src.len div fc
  result = newSeqOfCap[float32](n * tc)
  for p in 0 ..< n:
    let b = p * fc
    let alpha = if src0.hasAlpha: src[b + fc - 1] else: 1.0'f32
    if src0.sameModel(to):
      for k in 0 ..< src0.colorChannels: result.add src[b + k]
    else:
      var rgb: array[3, float32]
      case src0
      of clGray, clGrayA: rgb = [src[b], src[b], src[b]]
      of clRgb, clRgba: rgb = [src[b], src[b + 1], src[b + 2]]
      of clCmyk, clCmykA:
        let k = src[b + 3]
        rgb = [(1 - src[b]) * (1 - k), (1 - src[b + 1]) * (1 - k), (1 - src[b + 2]) * (1 - k)]
      case to
      of clGray, clGrayA: result.add rgb[0] * Luma[0] + rgb[1] * Luma[1] + rgb[2] * Luma[2]
      of clRgb, clRgba: result.add rgb
      of clCmyk, clCmykA:
        let k = 1.0'f32 - max(rgb[0], max(rgb[1], rgb[2]))
        if k >= 1.0: result.add [0.0'f32, 0, 0, 1]
        else:
          let d = 1.0'f32 - k
          result.add [(1 - rgb[0] - k) / d, (1 - rgb[1] - k) / d, (1 - rgb[2] - k) / d, k]
    if to.hasAlpha: result.add alpha

# ---------------------------------------------------------------- image

proc byteLen(width, height: int; layout: ChannelLayout; sample: ImgSample): int =
  if width < 0 or height < 0: raise invalidImage("negative image size")
  let n = int64(width) * int64(height) * layout.channels * sample.bytes
  if n > int64(high(int)) div 2: raise invalidImage("image size overflows")
  int(n)

proc newImage*(width, height: int; layout: ChannelLayout; sample: ImgSample): Image =
  ## A zero-filled image.
  Image(width: width, height: height, layout: layout, sample: sample, data: newSeq[byte](byteLen(width, height, layout, sample)))

proc imageFromRaw*(width, height: int; layout: ChannelLayout; sample: ImgSample; data: seq[byte]): Image =
  let expected = byteLen(width, height, layout, sample)
  if data.len != expected:
    raise invalidImage("buffer has " & $data.len & " bytes, expected " & $expected & " for " & $width & "x" & $height & " " & $layout & " " & $sample)
  Image(width: width, height: height, layout: layout, sample: sample, data: data)

proc imageFromNormalized*(width, height: int; layout: ChannelLayout; sample: ImgSample; values: openArray[float32]): Image =
  imageFromRaw(width, height, layout, sample, quantize(values, sample))

proc pixelCount*(i: Image): int = i.width * i.height
proc sampleCount*(i: Image): int = i.pixelCount * i.layout.channels
proc sampleNormalized*(i: Image; index: int): float32 = readNormalized(i.data, i.sample, index)
proc get*(i: Image; x, y, c: int): float32 = i.sampleNormalized((y * i.width + x) * i.layout.channels + c)
proc toNormalized*(i: Image): seq[float32] = toNormalized(i.data, i.sample)

proc toU16Samples*(i: Image): seq[uint16] =
  result = newSeq[uint16](i.data.len div 2)
  for k in 0 ..< result.len: result[k] = rdU16(i.data, k)

proc toF32Samples*(i: Image): seq[float32] =
  result = newSeq[float32](i.data.len div 4)
  for k in 0 ..< result.len: result[k] = rdF32(i.data, k)

proc hasTranslucency*(i: Image): bool =
  if not i.layout.hasAlpha: return false
  let ch = i.layout.channels
  for p in 0 ..< i.pixelCount:
    if i.sampleNormalized(p * ch + ch - 1) < 1.0: return true
  false

proc hasOutOfRange*(i: Image): bool =
  if not i.sample.isFloat: return false
  for k in 0 ..< i.sampleCount:
    let v = i.sampleNormalized(k)
    if not (v >= 0.0 and v <= 1.0): return true
  false

proc convert*(i: Image; layout: ChannelLayout; sample: ImgSample): Image =
  ## Convert to another layout and/or sample type. The ICC profile is kept only if the colour
  ## model is unchanged. Metadata is kept.
  if layout == i.layout and sample == i.sample: return i
  var data: seq[byte]
  if sample == i.sample and not i.layout.hasAlpha and layout == i.layout.withAlpha:
    let bps = sample.bytes
    let src = i.layout.channels * bps
    var one = newSeq[byte](bps)
    case sample
    of isU8: one[0] = 255
    of isU16: one[0] = 255; one[1] = 255
    of isF16: cast[ptr uint16](addr one[0])[] = f32ToF16(1.0)
    of isF32: cast[ptr float32](addr one[0])[] = 1.0'f32
    data = newSeqOfCap[byte](i.data.len div src * (src + bps))
    for p in 0 ..< i.data.len div src:
      for k in 0 ..< src: data.add i.data[p * src + k]
      data.add one
  elif layout == i.layout:
    data = quantize(i.toNormalized, sample)
  else:
    data = quantize(convertLayout(i.toNormalized, i.layout, layout), sample)
  Image(width: i.width, height: i.height, layout: layout, sample: sample, data: data,
        icc: (if layout.sameModel(i.layout): i.icc else: none(seq[byte])), meta: i.meta, warnings: i.warnings)

proc toRgba8*(i: Image): seq[byte] = i.convert(clRgba, isU8).data
proc toRgbaF32*(i: Image): seq[float32] = convertLayout(i.toNormalized, i.layout, clRgba)

# ---------------------------------------------------------------- formats

const AllFormats* = [fmtPng, fmtJpeg, fmtTiff, fmtWebP, fmtGif, fmtBmp, fmtTga, fmtIco, fmtPnm, fmtQoi, fmtOpenExr, fmtHdr, fmtAvif, fmtHeif, fmtFits]

proc name*(f: Format): string = $f

proc extensions*(f: Format): seq[string] =
  case f
  of fmtPng: @["png", "apng"]
  of fmtJpeg: @["jpg", "jpeg", "jpe", "jfif"]
  of fmtTiff: @["tif", "tiff"]
  of fmtWebP: @["webp"]
  of fmtGif: @["gif"]
  of fmtBmp: @["bmp", "dib"]
  of fmtTga: @["tga", "icb", "vda", "vst"]
  of fmtIco: @["ico"]
  of fmtPnm: @["pnm", "pbm", "pgm", "ppm", "pam", "pfm"]
  of fmtQoi: @["qoi"]
  of fmtOpenExr: @["exr"]
  of fmtHdr: @["hdr"]
  of fmtAvif: @["avif"]
  of fmtHeif: @["heic", "heif", "hif"]
  of fmtFits: @["fits", "fit", "fts"]

proc mimeType*(f: Format): string =
  case f
  of fmtPng: "image/png"
  of fmtJpeg: "image/jpeg"
  of fmtTiff: "image/tiff"
  of fmtWebP: "image/webp"
  of fmtGif: "image/gif"
  of fmtBmp: "image/bmp"
  of fmtTga: "image/x-tga"
  of fmtIco: "image/x-icon"
  of fmtPnm: "image/x-portable-anymap"
  of fmtQoi: "image/qoi"
  of fmtOpenExr: "image/x-exr"
  of fmtHdr: "image/vnd.radiance"
  of fmtAvif: "image/avif"
  of fmtHeif: "image/heif"
  of fmtFits: "image/fits"

proc maxDimensions*(f: Format): Option[(int, int)] =
  case f
  of fmtJpeg, fmtGif: some((65535, 65535))
  of fmtWebP: some((16384, 16384))
  of fmtIco: some((256, 256))
  else: none((int, int))

proc caps*(f: Format): FormatCaps =
  ## Capability table: what the format holds and what this build reads/writes.
  result = FormatCaps(read: true, write: true, depths: @[isU8], layouts: @[clRgb, clRgba], alpha: true)
  const RgbGray = @[clGray, clGrayA, clRgb, clRgba]
  const AllLayouts = @[clGray, clGrayA, clRgb, clRgba, clCmyk, clCmykA]
  case f
  of fmtPng:
    result.depths = @[isU8, isU16]; result.layouts = RgbGray
    result.icc = true; result.exif = true; result.xmp = true; result.dpi = true; result.text = true; result.animation = true
  of fmtJpeg:
    result.layouts = @[clGray, clRgb, clCmyk]; result.alpha = false
    result.icc = true; result.exif = true; result.xmp = true; result.dpi = true; result.lossy = true
  of fmtTiff:
    result.depths = @[isU8, isU16, isF32]; result.layouts = AllLayouts
    result.icc = true; result.xmp = true; result.dpi = true; result.text = true
  of fmtWebP:
    result.icc = true; result.exif = true; result.xmp = true; result.animation = true
  of fmtGif:
    result.layouts = @[clRgba]; result.animation = true; result.lossy = true
  of fmtBmp, fmtQoi: discard
  of fmtTga: result.layouts = RgbGray
  of fmtIco: result.layouts = @[clRgba]
  of fmtPnm:
    result.depths = @[isU8, isU16, isF32]; result.layouts = AllLayouts
  of fmtOpenExr:
    result.depths = @[isF16, isF32]; result.layouts = RgbGray
  of fmtHdr:
    result.depths = @[isF32]; result.layouts = @[clRgb]; result.alpha = false; result.lossy = true
  of fmtAvif:
    result.read = false; result.write = false; result.lossy = true
  of fmtHeif:
    result.read = false; result.write = false; result.depths = @[isU8, isU16]
    result.icc = true; result.exif = true; result.xmp = true; result.lossy = true
  of fmtFits:
    result.depths = @[isU8, isU16, isF32]; result.layouts = @[clGray, clRgb]; result.alpha = false; result.text = true

proc isEnabled*(f: Format): bool =
  let c = caps(f)
  c.read or c.write

proc startsWith(b: openArray[byte]; s: string): bool =
  if b.len < s.len: return false
  for i, c in s:
    if b[i] != byte(c): return false
  true

proc ftypHas(b: openArray[byte]; brands: openArray[string]): bool =
  let size = (int(b[0]) shl 24) or (int(b[1]) shl 16) or (int(b[2]) shl 8) or int(b[3])
  let e = clamp(size, 12, min(b.len, 64))
  var k = 0
  var p = 8
  while p + 4 <= e:
    if k != 1:
      var s = ""
      for j in 0..3: s.add char(b[p + j])
      if s in brands: return true
    inc k
    p += 4
  false

proc looksLikeTga(b: openArray[byte]): bool =
  if b.len < 18: return false
  if b.len >= 26:
    let foot = "TRUEVISION-XFILE.\0"
    var ok = true
    for i, c in foot:
      if b[b.len - 18 + i] != byte(c): ok = false
    if ok: return true
  let cmapType = b[1]
  let imgType = b[2]
  let cmapLen = int(b[5]) or (int(b[6]) shl 8)
  let cmapDepth = b[7]
  let w = int(b[12]) or (int(b[13]) shl 8)
  let h = int(b[14]) or (int(b[15]) shl 8)
  let depth = b[16]
  let desc = b[17]
  let typeOk = imgType in {1'u8, 2, 3, 9, 10, 11}
  let cmapOk = case cmapType
    of 0: cmapLen == 0 and cmapDepth == 0 and imgType notin {1'u8, 9}
    of 1: imgType in {1'u8, 9} and cmapLen > 0 and cmapDepth in {15'u8, 16, 24, 32}
    else: false
  let depthOk = case imgType
    of 1, 9: depth == 8
    of 3, 11: depth in {8'u8, 16}
    else: depth in {15'u8, 16, 24, 32}
  typeOk and cmapOk and depthOk and w > 0 and h > 0 and (desc and 0xC0) == 0

proc detect*(b: openArray[byte]): Option[Format] =
  ## Identify a format from its leading bytes (magic numbers).
  if b.startsWith("\x89PNG\r\n\x1a\n"): return some(fmtPng)
  if b.len >= 3 and b[0] == 0xFF and b[1] == 0xD8 and b[2] == 0xFF: return some(fmtJpeg)
  if b.startsWith("II*\0") or b.startsWith("MM\0*") or b.startsWith("II+\0") or b.startsWith("MM\0+"): return some(fmtTiff)
  if b.len >= 12 and b.startsWith("RIFF") and b[8] == byte('W') and b[9] == byte('E') and b[10] == byte('B') and b[11] == byte('P'):
    return some(fmtWebP)
  if b.startsWith("GIF87a") or b.startsWith("GIF89a"): return some(fmtGif)
  if b.startsWith("qoif"): return some(fmtQoi)
  if b.len >= 4 and b[0] == 0x76 and b[1] == 0x2F and b[2] == 0x31 and b[3] == 0x01: return some(fmtOpenExr)
  if b.startsWith("#?RADIANCE") or b.startsWith("#?RGBE"): return some(fmtHdr)
  if b.len >= 12 and b[4] == byte('f') and b[5] == byte('t') and b[6] == byte('y') and b[7] == byte('p'):
    if ftypHas(b, ["avif", "avis"]): return some(fmtAvif)
    if ftypHas(b, ["heic", "heix", "heim", "heis", "hevc", "hevx", "mif1", "msf1", "miaf"]): return some(fmtHeif)
  if b.len >= 14 and b.startsWith("BM"): return some(fmtBmp)
  if b.len >= 6 and b[0] == 0 and b[1] == 0 and b[2] == 1 and b[3] == 0 and (b[4] != 0 or b[5] != 0): return some(fmtIco)
  if b.len >= 3 and b[0] == byte('P') and char(b[1]) in {'1'..'7', 'F', 'f'} and char(b[2]) in Whitespace: return some(fmtPnm)
  if b.startsWith("SIMPLE  ="): return some(fmtFits)
  if looksLikeTga(b): return some(fmtTga)
  none(Format)

proc fromExtension*(ext: string): Option[Format] =
  ## Look up a format by extension (case-insensitive, with or without a dot, or a path).
  var e = ext
  let cut = max(e.rfind('.'), max(e.rfind('/'), e.rfind('\\')))
  if cut >= 0: e = e[cut + 1 .. ^1]
  e = e.toLowerAscii
  for f in AllFormats:
    if e in f.extensions: return some(f)
  none(Format)
