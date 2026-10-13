## Layered TIFF export: Photoshop's layer data (tag 37724) and image resources (tag 34377) next
## to the flat composite, so Photoshop reopens the layers (Rust `tiff_layers.rs`; the import
## half is `flat.importLayered`).

import std/options
import ../color, ../raster, ../doc, ../psd, ../codecs
import common, pixels, flat, psd_export

const
  TiffModelDone = 0.4'f32
  TiffLayersDone = 0.68'f32
  TiffCompositeDone = 0.72'f32

proc wouldWriteLayers*(d: Document): bool =
  ## False when the document is a lone locked background (a plain TIFF says it all).
  not (singleLayer(d).isSome and d.layers.len > 0 and d.layers[0].locks.transparency)

proc layeredMode(d: Document): bool =
  d.pixelFormat.mode in {cmGrayscale, cmRgb, cmCmyk} and d.mode != cmMultichannel and d.layers.len > 0

proc interleavePlanes(planes: openArray[byte]; w, h, channels, colorChannels: int; sample: ImgSample;
                      invertColor: bool): seq[byte] =
  let n = w * h
  let bps = sample.bytes
  let plane = n * bps
  if planes.len < plane * channels:
    raise ioError(ieUnsupported, "the merged image is shorter than its header says")
  result = tryBuffer(n * channels, bps)
  for i in 0 ..< n:
    for c in 0 ..< channels:
      let at = c * plane + i * bps
      let invert = invertColor and c < colorChannels
      case bps
      of 1: result.add (if invert: 255'u8 - planes[at] else: planes[at])
      of 2:
        var v = (uint16(planes[at]) shl 8) or uint16(planes[at + 1])
        if invert: v = 65535'u16 - v
        result.add cast[array[2, byte]](v)
      of 4:
        var u = 0'u32
        for k in 0 ..< 4: u = (u shl 8) or uint32(planes[at + k])
        var v = cast[float32](u)
        if invert: v = 1 - v
        result.add cast[array[4, byte]](v)
      else: raise ioError(ieUnsupported, $bps & "-byte samples")

proc exportLayered*(d: Document; opts: ExportOptions; ctl: Interrupt): ExportResult =
  if not layeredMode(d):
    result = exportFlatWith(d, fmtTiff, opts, ctl)
    if d.layers.len > 0:
      result.warnings.add $d.pixelFormat.mode & " documents are saved as a flat TIFF: their layers are not kept"
    return
  var warnings: seq[string]
  let big = d.size.width > 30_000 or d.size.height > 30_000
  let psdOpts = PsdExportOptions(forcePsb: opts.forcePsb or big, mergedMatte: false, mergedCompression: cRaw)
  let (file, w0) = documentToPsdInterruptible(d, psdOpts, withSpan(ctl, 0, TiffModelDone))
  if ctl.cancelled: raise ioError(ieCancelled)
  warnings.add w0
  let order = if writesLittleEndian(): boLittle else: boBig
  var layers: seq[byte]
  var w1: seq[string]
  try: (layers, w1) = fromPsd(file).toBytes(order)
  except PsdError as e: raise ioError(iePsd, e.msg)
  if ctl.cancelled: raise ioError(ieCancelled)
  ctl.progress(TiffLayersDone)
  for x in w1: warnings.add "layer data: " & x
  var kept: seq[ImageResource]
  for r in file.resources:
    if r.id != ridIccProfile and r.id != ridXmp: kept.add r
  var resources: seq[byte]
  try: resources = resourcesToBytes(kept)
  except PsdError as e: raise ioError(iePsd, e.msg)
  let fmt = d.pixelFormat
  let cc = fmt.mode.colorChannels
  let hasAlpha = file.layerInfo.isSome and file.layerInfo.get.mergedAlpha
  let channels = cc + ord(hasAlpha)
  let extra = max(file.header.channels - channels, 0)
  if extra > 0:
    warnings.add $extra & " alpha channel(s) are not written to TIFF (one transparency channel only)"
  let sample = csampleOf(fmt.sample)
  let (w, h) = (d.size.width, d.size.height)
  let data = interleavePlanes(file.imageData.data, w, h, channels, cc, sample, fmt.mode == cmCmyk)
  if ctl.cancelled: raise ioError(ieCancelled)
  ctl.progress(TiffCompositeDone)
  var img: Image
  try: img = imageFromRaw(w, h, layoutFor(fmt.mode, hasAlpha), sample, data)
  except CodecError as e: raise ioError(ieCodec, e.msg)
  img.icc = if d.iccProfile.isNil: none(seq[byte]) else: some(d.iccProfile.data)
  img.meta = codecs.Metadata(xmp: (if opts.xmp == xeAll: d.metadata.xmp else: none(string)),
                             text: (if opts.xmp == xeAll: d.metadata.text else: @[]),
                             dpi: some((d.resolutionDpi, d.resolutionDpi)),
                             photoshopResources: some(resources), photoshopLayers: some(layers))
  encodeImage(img, fmtTiff, opts, warnings)

proc stripForeignOrderBlocks*(file: var PsdFile): seq[string] =
  ## Drops tagged blocks kept in little-endian order from a TIFF (PSD can't hold them).
  var warnings: seq[string]
  proc strip(blocks: var seq[TaggedBlock]; owner: string) =
    var kept: seq[TaggedBlock]
    for b in blocks:
      if b.isForeignOrder:
        warnings.add owner & ": " & b.key & " block (" & $b.data.len &
          " bytes) kept from a little-endian TIFF cannot be written to PSD and was dropped"
      else: kept.add b
    blocks = kept
  if file.layerInfo.isSome:
    for l in file.layerInfo.get.layers.mitems:
      let name = l.name
      strip(l.blocks, "layer \"" & name & "\"")
  strip(file.globalBlocks, "document")
  warnings
