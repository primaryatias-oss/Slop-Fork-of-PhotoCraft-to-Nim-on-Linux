## Document import/export across formats (Rust `photocraft_io` lib.rs): `.pcraft`, PSD/PSB,
## layered TIFF, multi-part EXR and every flat codec format. PDN, OpenRaster, SVG, camera raw
## and Affinity are dispatched to their modules as those are ported (see PORTING.md); until
## then they report "unsupported".

import std/[options, strutils]
import geom, color, raster, doc, psd, compose, format, codecs
import io/[common, pixels, psd_import, psd_export, psd_thumbnail, flat, tiff_layers]
export common, psd_import, psd_export, flat, tiff_layers

const
  PcraftThumbnailDone = 0.15'f32
  PcraftPreviewsDone = 0.4'f32
  PsdModelDone = 0.9'f32

type FormatHook* = object
  ## A format module plugged into `import`/`export` (OpenRaster, PDN, SVG, camera raw, Affinity).
  detect*: proc(name: string; bytes: openArray[byte]): bool
  importer*: proc(name: string; bytes: seq[byte]; ctl: Interrupt; maxGroupDepth: int): ImportResult
  exportExts*: seq[string]
  exporter*: proc(d: Document; ext: string; opts: ExportOptions; ctl: Interrupt): ExportResult

var formatHooks*: seq[FormatHook]
  ## Checked in order before the flat codecs (after .pcraft and PSD).

proc isPsd*(bytes: openArray[byte]): bool =
  bytes.len >= 4 and bytes[0] == byte('8') and bytes[1] == byte('B') and bytes[2] == byte('P') and bytes[3] == byte('S')

proc check(ctl: Interrupt) =
  if ctl.cancelled: raise ioError(ieCancelled)

proc importStages(name: string; bytes: openArray[byte]; ctl: Interrupt; maxGroupDepth: int): ImportResult =
  if hasExtension(name, format.Extension) or isPcraft(bytes):
    try: return ImportResult(document: loadFromBytes(@bytes))
    except FormatError as e: raise ioError(iePcraft, e.msg)
  if isPsd(bytes):
    var file: PsdFile
    try: file = parsePsd(bytes)
    except PsdError as e: raise ioError(iePsd, e.msg)
    checkGroupDepth(file)
    check(ctl)
    ctl.progress(0.05)
    let r = psdToDocumentWith(file, ctl)
    if r.isNone: raise ioError(ieCancelled)
    var (d, warnings) = r.get
    d.name = name
    return ImportResult(document: d, warnings: warnings)
  for h in formatHooks:
    if not h.detect.isNil and not h.importer.isNil and h.detect(name, bytes):
      return h.importer(name, @bytes, ctl, maxGroupDepth)
  if hasExtension(name, "ora") or hasExtension(name, "pdn") or hasExtension(name, "svg") or
     hasExtension(name, "svgz") or hasExtension(name, "afphoto") or hasExtension(name, "afdesign"):
    raise ioError(ieUnsupported, "this format is not available in this build yet")
  importFlat(name, bytes)

proc importWithDepth*(name: string; bytes: openArray[byte]; maxGroupDepth: int; ctl: Interrupt): ImportResult =
  check(ctl)
  result = importStages(name, bytes, ctl, maxGroupDepth)
  check(ctl)
  ctl.progress(1.0)

proc importDoc*(name: string; bytes: openArray[byte]): ImportResult =
  ## Opens any supported file. Raises `IoFailure`.
  importWithDepth(name, bytes, MaxGroupDepth, NoInterrupt)

proc importWith*(name: string; bytes: openArray[byte]; ctl: Interrupt): ImportResult =
  importWithDepth(name, bytes, MaxGroupDepth, ctl)

proc exportStages(d: Document; nameOrExt: string; opts: ExportOptions; ctl: Interrupt): ExportResult =
  let ext = extension(nameOrExt)
  if ext == "pdn":
    raise ioError(ieUnsupported, "PDN is import-only; save as .pcraft to preserve all layers and blend modes")
  if ext in ["afphoto", "afdesign", "afpub"]:
    raise ioError(ieUnsupported, "Affinity export is not implemented; save a new PSD, PNG or .pcraft copy")
  for h in formatHooks:
    if ext in h.exportExts and not h.exporter.isNil: return h.exporter(d, ext, opts, ctl)
  if ext == format.Extension:
    let thumb = thumbnail(d, 256)
    check(ctl)
    ctl.progress(PcraftThumbnailDone)
    let composite = thumbnail(d, 1024)
    check(ctl)
    ctl.progress(PcraftPreviewsDone)
    try:
      return ExportResult(bytes: saveToBytes(d, SaveOptions(thumbnail: some(thumb), composite: some(composite))))
    except FormatError as e: raise ioError(iePcraft, e.msg)
  if ext == "psd" or ext == "psb":
    var o = defaultPsdExportOptions()
    o.forcePsb = opts.forcePsb or ext == "psb"
    var (file, warnings) = documentToPsdInterruptible(d, o, withSpan(ctl, 0, PsdModelDone))
    check(ctl)
    if d.metadata.text.len > 0:
      warnings.add "free-form text metadata is not supported by PSD/PSB; it will be dropped"
    warnings.add stripForeignOrderBlocks(file)
    if opts.imagePreviews:
      try:
        let r = thumbnailResource(d)
        var at = file.resources.len
        for i, x in file.resources:
          if x.id == ridVersionInfo:
            at = i
            break
        file.resources.insert(r, at)
      except IoFailure as e:
        warnings.add "image preview not saved: " & e.msg
    try:
      file.header.validate()
      return ExportResult(bytes: file.toBytes, warnings: warnings)
    except PsdError as e: raise ioError(iePsd, e.msg)
  let f = fromExtension(ext)
  if f.isNone: raise ioError(ieUnknownFormat, nameOrExt)
  if f.get == fmtTiff and opts.tiffLayers and wouldWriteLayers(d): return exportLayered(d, opts, ctl)
  exportFlatWith(d, f.get, opts, ctl)

proc exportWith*(d: Document; nameOrExt: string; opts: ExportOptions; ctl: Interrupt): ExportResult =
  check(ctl)
  result = exportStages(d, nameOrExt, opts, ctl)
  check(ctl)
  ctl.progress(1.0)

proc exportDoc*(d: Document; nameOrExt: string; opts = defaultExportOptions()): ExportResult =
  ## Saves `d` in the format named by an extension or file name. Raises `IoFailure`.
  exportWith(d, nameOrExt, opts, NoInterrupt)

proc mergedComposite*(file: PsdFile): seq[array[4, float32]] =
  ## A PSD's merged image as straight RGBA floats (Photoshop's "maximize compatibility" data).
  let h = file.header
  try: h.validate()
  except PsdError as e: raise ioError(iePsd, e.msg)
  if h.colorMode == pcmRgb and file.mergedHasAlpha:
    var raw: seq[byte]
    try: raw = file.decodeMerged
    except PsdError as e: raise ioError(iePsd, e.msg)
    let plane = raw.len div max(h.channels, 1)
    if plane == 0: raise ioError(iePsd, "invalid data: empty RGB composite")
    var planes: seq[seq[float32]]
    for k in 0 ..< min(4, raw.len div plane):
      let bytes = raw[k * plane ..< (k + 1) * plane]
      case h.depth
      of 32: planes.add samplesF32(bytes)
      of 16:
        var v: seq[float32]
        for x in samplesU16(bytes): v.add float32(x) / 65535
        planes.add v
      else:
        var v: seq[float32]
        try:
          for x in planeToU8(bytes, h.depth, h.width, h.height): v.add float32(x) / 255
        except PsdError as e: raise ioError(iePsd, e.msg)
        planes.add v
    if planes.len != 4: raise ioError(iePsd, "invalid data: missing RGB composite channels")
    let n = min(min(planes[0].len, planes[1].len), min(planes[2].len, planes[3].len))
    for i in 0 ..< n:
      var px = [planes[0][i], planes[1][i], planes[2][i], planes[3][i]]
      for c in 0 ..< 3:
        px[c] = unmatte(px[c], px[3], 1)
        if h.depth != 32: px[c] = clamp(px[c], 0'f32, 1'f32)
      result.add px
    return
  if h.depth != 32 and h.colorMode != pcmLab and h.colorMode != pcmCmyk:
    var ok = true
    var img: RgbaImage
    try: img = file.compositeRgba8
    except PsdError: ok = false
    if ok:
      let um = file.mergedHasAlpha
      var i = 0
      while i + 3 < img.data.len:
        var v = [float32(img.data[i]) / 255, float32(img.data[i + 1]) / 255, float32(img.data[i + 2]) / 255,
                 float32(img.data[i + 3]) / 255]
        if um:
          for c in 0 ..< 3: v[c] = clamp(unmatte(v[c], v[3], 1), 0'f32, 1'f32)
        result.add v
        i += 4
      return
  var flatFile = file
  flatFile.layerInfo = some(LayerInfo(mergedAlpha: file.mergedHasAlpha))
  let (d, _) = psdToDocument(flatFile)
  if d.layers.len == 0 and d.mode == cmMultichannel: return flatten(d).px
  if d.layers.len == 0 or not d.layers[0].hasSurface: raise ioError(ieUnsupported, "no merged image")
  surfaceToBuffer(d.layers[0].surface, d.bounds).px

psd_export.importHook = proc(name: string; bytes: seq[byte]): Document = importDoc(name, bytes).document
