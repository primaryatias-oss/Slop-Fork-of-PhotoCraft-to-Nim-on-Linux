## `photocraft/codecs`: symmetric, depth- and metadata-preserving codecs for flat raster
## image formats, implemented in pure Nim (the Rust crate wraps third-party crates; with only
## the Nim standard library every format is implemented here from its specification).
##
## * Bit depth, ICC profiles and basic metadata (EXIF, XMP, DPI, text) are preserved where the
##   format allows. `caps` declares per-format capabilities and `fidelityWarnings` says what an
##   export will lose.
## * The API is I/O free (bytes in, bytes out). Decoders enforce `Limits` before allocating
##   and raise `CodecError`, never crash, on malformed input.
##
## Port of `crates/codecs`.

import std/[options, strutils]
import codecs/[image, meta, png, jpeg, simple, tiff, webp, exr]
export image, meta

type
  FidelityWarningKind* = enum
    fwWriteUnsupported, fwDepthReduced, fwRangeClipped, fwAlphaDiscarded, fwAlphaBinarized,
    fwCmykConverted, fwColorToGray, fwPaletteQuantized, fwLossyCompression, fwIccDropped,
    fwExifDropped, fwXmpDropped, fwDpiDropped, fwTextDropped, fwMetadataTooLarge, fwDimensionsExceeded
  FidelityWarning* = object
    kind*: FidelityWarningKind
    format*: Format
    src*, dst*: ImgSample
    toLayout*: ChannelLayout
    what*: string
    bytes*: int
    maxW*, maxH*: int

  Plan* = object
    layout*: ChannelLayout
    sample*: ImgSample

proc isFatal*(w: FidelityWarning): bool = w.kind in {fwWriteUnsupported, fwDimensionsExceeded}

proc depthName(s: ImgSample): string =
  case s
  of isU8: "8-bit"
  of isU16: "16-bit"
  of isF16: "16-bit float"
  of isF32: "32-bit float"

proc `$`*(w: FidelityWarning): string =
  case w.kind
  of fwWriteUnsupported: $w.format & " cannot be written in this build"
  of fwDepthReduced: depthName(w.src) & " will be reduced to " & depthName(w.dst)
  of fwRangeClipped: "HDR values outside 0..1 will be clipped"
  of fwAlphaDiscarded: "alpha will be discarded"
  of fwAlphaBinarized: "alpha will be reduced to on/off transparency"
  of fwCmykConverted: "CMYK will be converted to " & $w.toLayout & " (not colour-managed)"
  of fwColorToGray: "colour will be converted to grayscale"
  of fwPaletteQuantized: "colours will be quantized to a 256-colour palette"
  of fwLossyCompression: "lossy compression"
  of fwIccDropped: "ICC profile not supported; it will be dropped"
  of fwExifDropped: "EXIF not supported; it will be dropped"
  of fwXmpDropped: "XMP not supported; it will be dropped"
  of fwDpiDropped: "resolution (DPI) not supported; it will be dropped"
  of fwTextDropped: "text metadata not supported; it will be dropped"
  of fwMetadataTooLarge: w.what & " (" & $w.bytes & " bytes) is too large for the format; it will be dropped"
  of fwDimensionsExceeded: "image exceeds the format maximum of " & $w.maxW & "x" & $w.maxH

proc pickSample(src: ImgSample; depths: seq[ImgSample]): ImgSample =
  if src in depths: return src
  let pref = case src
    of isU8: @[isU16, isF32, isF16]
    of isU16: @[isF32, isU8, isF16]
    of isF16: @[isF32, isU16, isU8]
    of isF32: @[isF16, isU16, isU8]
  for s in pref:
    if s in depths: return s
  depths[0]

proc pickLayout(src: ChannelLayout; layouts: seq[ChannelLayout]; alpha: bool): ChannelLayout =
  var cand = src
  if not alpha: cand = cand.withoutAlpha
  if cand in layouts: return cand
  if cand.isCmyk or cand.isGray: cand = if cand.hasAlpha: clRgba else: clRgb
  if cand in layouts: return cand
  if cand.withAlpha in layouts: return cand.withAlpha
  if cand.withoutAlpha in layouts: return cand.withoutAlpha
  layouts[0]

proc plan*(img: Image; format: Format; opts: EncodeOptions): Plan =
  let c = caps(format)
  if format == fmtPnm and img.sample.isFloat:
    return Plan(layout: pickLayout(img.layout, @[clGray, clRgb], false), sample: isF32)
  Plan(layout: pickLayout(img.layout, c.layouts, c.alpha), sample: pickSample(img.sample, c.depths))

proc fidelityWarnings*(img: Image; format: Format; opts = defaultEncodeOptions()): seq[FidelityWarning] =
  ## What will be lost or altered when writing `img` as `format`.
  let c = caps(format)
  if not c.write: return @[FidelityWarning(kind: fwWriteUnsupported, format: format)]
  let md = format.maxDimensions
  if md.isSome and (img.width > md.get[0] or img.height > md.get[1]):
    result.add FidelityWarning(kind: fwDimensionsExceeded, maxW: md.get[0], maxH: md.get[1])
  let p = plan(img, format, opts)
  let (sl, ss) = (img.layout, img.sample)
  let lossyDepth = not (ss == p.sample or ss == isU8 or (ss == isU16 and p.sample == isF32) or (ss == isF16 and p.sample == isF32))
  if lossyDepth: result.add FidelityWarning(kind: fwDepthReduced, src: ss, dst: p.sample)
  if ss.isFloat and not p.sample.isFloat and img.hasOutOfRange: result.add FidelityWarning(kind: fwRangeClipped)
  if sl.hasAlpha and not p.layout.hasAlpha and img.hasTranslucency: result.add FidelityWarning(kind: fwAlphaDiscarded)
  if format == fmtGif and p.layout.hasAlpha and img.hasTranslucency: result.add FidelityWarning(kind: fwAlphaBinarized)
  if sl.isCmyk and not p.layout.isCmyk: result.add FidelityWarning(kind: fwCmykConverted, toLayout: p.layout)
  if not sl.isGray and p.layout.isGray: result.add FidelityWarning(kind: fwColorToGray)
  if format == fmtGif: result.add FidelityWarning(kind: fwPaletteQuantized)
  elif c.lossy or (format == fmtWebP and not opts.webpLossless): result.add FidelityWarning(kind: fwLossyCompression)
  if img.icc.isSome and opts.embedIcc and (not c.icc or not sl.sameModel(p.layout)): result.add FidelityWarning(kind: fwIccDropped)
  if opts.embedMetadata:
    let m = img.meta
    if m.exif.isSome and not c.exif: result.add FidelityWarning(kind: fwExifDropped)
    if m.xmp.isSome and not c.xmp: result.add FidelityWarning(kind: fwXmpDropped)
    if format == fmtJpeg:
      if m.exif.isSome and exifSegment(m.exif.get, none((float32, float32))).isNone:
        result.add FidelityWarning(kind: fwMetadataTooLarge, what: "EXIF", bytes: m.exif.get.len)
      if m.xmp.isSome and xmpSegment(m.xmp.get, none((float32, float32))).isNone:
        result.add FidelityWarning(kind: fwMetadataTooLarge, what: "XMP", bytes: m.xmp.get.len)
    if m.dpi.isSome and not c.dpi: result.add FidelityWarning(kind: fwDpiDropped)
    if m.text.len > 0 and not c.text: result.add FidelityWarning(kind: fwTextDropped)

proc finishDecode(img: Image; o: int; opts: DecodeOptions): Image =
  opts.limits.check(img.width, img.height, img.layout, img.sample)
  if opts.keepOrientation: return img
  if o >= 5 and o <= 8: opts.limits.check(img.height, img.width, img.layout, img.sample)
  img.oriented(o)

proc decodeAs*(format: Format; bytes: openArray[byte]; opts = defaultDecodeOptions()): Image =
  ## Decode as a specific format. Raises `CodecError`.
  if not caps(format).read: raise unsupported(format, "decoding is not available for this format")
  let l = opts.limits
  var img = case format
    of fmtPng: decodePng(bytes, l)
    of fmtJpeg: decodeJpeg(bytes, l)
    of fmtTiff: decodeTiff(bytes, l)
    of fmtWebP: decodeWebp(bytes, l)
    of fmtBmp: decodeBmp(bytes, l)
    of fmtGif: decodeGif(bytes, l)
    of fmtTga: decodeTga(bytes, l)
    of fmtIco: decodeIco(bytes, l)
    of fmtPnm: decodePnm(bytes, l)
    of fmtQoi: decodeQoi(bytes, l)
    of fmtHdr: decodeHdr(bytes, l)
    of fmtOpenExr: decodeExr(bytes, l)
    of fmtFits: decodeFits(bytes, l)
    of fmtAvif, fmtHeif: raise unsupported(format, "decoding is not available in this build")
  if img.meta.dpi.isNone and format != fmtTiff:
    img.meta.dpi = resolutionFromMetadata(img.meta.exif, img.meta.xmp)
  let o = case format
    of fmtTiff: tiffOrientation(bytes)
    else: (if img.meta.exif.isSome: exifOrientation(img.meta.exif.get) else: 1)
  finishDecode(img, o, opts)

proc decode*(bytes: openArray[byte]; opts = defaultDecodeOptions()): Image =
  ## Detect the format and decode. Raises `CodecError`.
  let f = detect(bytes)
  if f.isNone: raise codecError(ceUnknownFormat, fmtPng, "")
  decodeAs(f.get, bytes, opts)

proc encode*(img: Image; format: Format; opts = defaultEncodeOptions()): seq[byte] =
  ## Encode `img` as `format`, converting as `fidelityWarnings` reports. Raises `CodecError`.
  if not caps(format).write: raise unsupported(format, "encoding is not available in this build")
  if img.width == 0 or img.height == 0: raise invalidImage("cannot encode an empty image")
  let md = format.maxDimensions
  if md.isSome and (img.width > md.get[0] or img.height > md.get[1]):
    raise encodeErr(format, "dimensions exceed " & $md.get[0] & "x" & $md.get[1])
  let p = plan(img, format, opts)
  case format
  of fmtPng: encodePng(img, p.layout, p.sample, opts)
  of fmtJpeg: encodeJpeg(img, p.layout, opts)
  of fmtTiff: encodeTiff(img, p.layout, p.sample, opts)
  of fmtWebP: encodeWebp(img, p.layout, opts)
  of fmtBmp: encodeBmp(img, p.layout)
  of fmtGif: encodeGif(img)
  of fmtTga: encodeTga(img, p.layout)
  of fmtIco: encodeIco(img)
  of fmtPnm: encodePnm(img, p.layout, p.sample)
  of fmtQoi: encodeQoi(img, p.layout)
  of fmtHdr: encodeHdr(img)
  of fmtOpenExr: encodeExr(img, p.layout, p.sample, opts)
  of fmtFits: encodeFits(img, p.layout, p.sample)
  of fmtAvif, fmtHeif: raise unsupported(format, "encoding is not available in this build")
