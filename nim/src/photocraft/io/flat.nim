## Single-image formats (PNG, JPEG, TIFF, WebP, …) ↔ documents (Rust `flat.rs`): import as one
## layer; export the composite with colour management, mattes and format-specific conversions.

import std/[options, math, strutils, sequtils]
import ../geom, ../color, ../raster, ../doc, ../compose, ../cms, ../codecs, ../psd
import ../color/convert as cconvert
import common, psd_import

const BandBytes = 32 shl 20

proc cErr(e: ref CodecError): ref IoFailure = ioError(ieCodec, e.msg)

proc tryBuffer*(pixels, bytesPerPixel: int): seq[byte] =
  ## An empty buffer with room for `pixels × bytesPerPixel` bytes; raises when that is absurd.
  if pixels < 0 or bytesPerPixel < 0 or (bytesPerPixel > 0 and pixels > high(int) div bytesPerPixel):
    raise ioError(ieUnsupported, "image too large")
  let len = pixels * bytesPerPixel
  if len > (1 shl 40): raise ioError(ieUnsupported, "not enough memory for a " & $(len shr 20) & " MB image")
  newSeqOfCap[byte](len)

proc imageSurface*(img: Image; targetLayout: ChannelLayout; csample: ImgSample): Surface =
  let mode = case targetLayout
    of clGray, clGrayA: cmGrayscale
    of clCmyk, clCmykA: cmCmyk
    else: cmRgb
  let depth = case csample
    of isU8: stU8
    of isU16: stU16
    else: stF32
  let fmt = pixelFormat(mode, depth, true)
  let (w, h) = (img.width, img.height)
  result = newSurface(fmt)
  if img.layout == targetLayout and img.sample == csample:
    result.writeInterleaved(rect(0, 0, w, h), img.data)
  else:
    let row = img.data.len div max(h, 1)
    let band = max(BandBytes div max(row, 1), 1)
    let chunk = max(row, 1) * band
    var i = 0
    var at = 0
    while at < img.data.len:
      let e = min(at + chunk, img.data.len)
      let n = (e - at) div max(row, 1)
      let part = imageFromRaw(w, n, img.layout, img.sample, img.data[at ..< e]).convert(targetLayout, csample)
      let y0 = i * band
      result.writeInterleaved(rect(0, y0, w, y0 + n), part.data)
      at = e
      inc i

proc warningText(w: DecodeWarning): string = $w

proc imageToDocument*(name: string; img: Image): ImportResult =
  var warnings: seq[string]
  for w in img.warnings: warnings.add warningText(w)
  let (mode, targetLayout) = case img.layout
    of clGray, clGrayA: (cmGrayscale, clGrayA)
    of clRgb, clRgba: (cmRgb, clRgba)
    of clCmyk, clCmykA: (cmCmyk, clCmykA)
  var depth = stU8
  var csample = isU8
  case img.sample
  of isU8: discard
  of isU16: (depth, csample) = (stU16, isU16)
  of isF16:
    warnings.add "16-bit float samples are stored as 32-bit float"
    (depth, csample) = (stF32, isF32)
  of isF32: (depth, csample) = (stF32, isF32)
  var d = newDocument(name, size(img.width, img.height), mode, depth)
  var s = imageSurface(img, targetLayout, csample)
  s.prune()
  if img.layout.hasAlpha:
    d.layers.add newLayer("Layer 0", rasterContent(s))
  else:
    var bg = newLayer("Background", rasterContent(s))
    bg.locks.transparency = true
    bg.locks.position = true
    d.layers.add bg
  if img.icc.isSome: d.iccProfile = shared(img.icc.get)
  if img.meta.exif.isSome: d.metadata.exif = shared(img.meta.exif.get)
  d.metadata.xmp = img.meta.xmp
  d.metadata.text = img.meta.text
  if img.meta.dpi.isSome:
    let (x, y) = img.meta.dpi.get
    d.resolutionDpi = x
    let w = unequalResolutionWarning(float64(x), float64(y))
    if w.isSome: warnings.add w.get
  ImportResult(document: d, warnings: warnings)

proc setResource(resources: var seq[ImageResource]; id: uint16; data: Option[seq[byte]]) =
  resources.keepItIf(it.id != id)
  if data.isSome: resources.add newResource(id, data.get)

proc planarBigEndian(img: Image; colorChannels: int; invertColor: bool): seq[byte] =
  let n = img.width * img.height
  let ch = img.layout.channels
  let bps = img.sample.bytes
  result = tryBuffer(n * ch, bps)
  result.setLen(n * ch * bps)
  for c in 0 ..< ch:
    let invert = invertColor and c < colorChannels
    for i in 0 ..< n:
      let src = (i * ch + c) * bps
      let dst = c * n * bps + i * bps
      if src + bps > img.data.len: break
      case bps
      of 1:
        let v = img.data[src]
        result[dst] = (if invert: 255'u8 - v else: v)
      of 2:
        var v: uint16
        copyMem(addr v, unsafeAddr img.data[src], 2)
        if invert: v = 65535'u16 - v
        result[dst] = byte(v shr 8)
        result[dst + 1] = byte(v and 0xff)
      of 4:
        var v: float32
        copyMem(addr v, unsafeAddr img.data[src], 4)
        if invert: v = 1 - v
        let u = cast[uint32](v)
        for k in 0 ..< 4: result[dst + k] = byte((u shr (24 - 8 * k)) and 0xff)
      else: raise ioError(ieUnsupported, $bps & "-byte samples")

proc importLayered*(name: string; img0: Image; layers: seq[byte]): ImportResult =
  ## A TIFF carrying Photoshop layer data (tag 37724): its layers over the TIFF's composite.
  var warnings: seq[string]
  var isd: ImageSourceData
  var w: seq[string]
  try: (isd, w) = parseImageSourceData(layers)
  except CatchableError as e:
    result = imageToDocument(name, img0)
    result.warnings.add "the PSD layer data could not be read (" & e.msg & "); opened flattened"
    return
  for x in w: warnings.add "layer data: " & x
  var resources: seq[ImageResource]
  let pr = img0.meta.photoshopResources
  if pr.isSome and pr.get.len > 0:
    try:
      let (r, rw) = resourcesFromBytes(pr.get)
      resources = r
      for x in rw: warnings.add "image resources: " & x
    except CatchableError as e:
      warnings.add "the PSD image resources could not be read: " & e.msg
  setResource(resources, ridIccProfile, img0.icc)
  if img0.meta.dpi.isSome and img0.meta.dpi.get[0] > 0:
    let (x, y) = img0.meta.dpi.get
    var ri = resolutionFromDpi(float64(x))
    ri.vResFixed = resolutionFromDpi(float64(y)).vResFixed
    setResource(resources, ridResolutionInfo, some(ri.toBytes))
  if img0.meta.xmp.isSome: setResource(resources, ridXmp, some(cast[seq[byte]](img0.meta.xmp.get)))
  if img0.meta.exif.isSome: setResource(resources, ridExif, some(img0.meta.exif.get))
  var img = img0
  if img.sample == isF16:
    warnings.add "16-bit float samples are stored as 32-bit float"
    img = img.convert(img.layout, isF32)
  let (mode, colorChannels) = case img.layout
    of clGray, clGrayA: (pcmGrayscale, 1)
    of clRgb, clRgba: (pcmRgb, 3)
    of clCmyk, clCmykA: (pcmCmyk, 4)
  let depth = case img.sample
    of isU8: 8
    of isU16: 16
    of isF16, isF32: 32
  let header = newHeader(isd.version, img.width, img.height, img.layout.channels, depth, mode)
  let data = planarBigEndian(img, colorChannels, mode == pcmCmyk)
  let file = isd.intoPsd(header, resources, ImageData(compression: cRaw, data: data))
  checkGroupDepth(file)
  var (d, dw) = psdToDocument(file)
  warnings.add dw
  d.name = name
  if d.iccProfile.isNil and img.icc.isSome: d.iccProfile = shared(img.icc.get)
  d.metadata.text = img.meta.text
  ImportResult(document: d, warnings: warnings)

proc tgaAlphaToChannel(d: var Document) =
  let canvas = d.bounds
  let depth = d.depth
  if d.layers.len == 0 or d.layers[0].content.kind != lkRaster: return
  template s: untyped = d.layers[0].content.raster
  let ch = s.format.channels
  if not s.format.alpha or ch < 2: return
  var alpha = newSurface(pixelFormat(cmGrayscale, depth, false))
  let rows = max(BandBytes div (max(canvas.width, 1) * ch * 4), 1)
  var y = canvas.y0
  while y < canvas.y1:
    let r = rect(canvas.x0, y, canvas.x1, min(satAdd(y, rows), canvas.y1))
    var px = s.readRegion(r)
    var a = newSeq[float32](px.len div ch)
    for i in 0 ..< a.len:
      a[i] = px[i * ch + ch - 1]
      px[i * ch + ch - 1] = 1
    s.writeRegion(r, px)
    alpha.writeRegion(r, a)
    y = r.y1
  s.prune()
  alpha.prune()
  d.layers[0].name = "Background"
  d.layers[0].locks.transparency = true
  d.layers[0].locks.position = true
  d.channels.add newAlphaChannel("Alpha 1", alpha)

proc tgaAlphaChannelToTransparency*(d: var Document) =
  ## Turns a TGA's alpha channel (imported as "Alpha 1") back into layer transparency.
  let canvas = d.bounds
  if d.layers.len != 1 or d.channels.len != 1: return
  if d.layers[0].content.kind != lkRaster: return
  template s: untyped = d.layers[0].content.raster
  let ch = s.format.channels
  if not s.format.alpha or ch < 2 or d.channels[0].spot.isSome: return
  let rows = max(BandBytes div (max(canvas.width, 1) * ch * 4), 1)
  var y = canvas.y0
  while y < canvas.y1:
    let r = rect(canvas.x0, y, canvas.x1, min(satAdd(y, rows), canvas.y1))
    var px = s.readRegion(r)
    let a = d.channels[0].surface.readRegion(r)
    let ac = d.channels[0].surface.channels
    for i in 0 ..< min(px.len div ch, a.len div max(ac, 1)):
      px[i * ch + ch - 1] = a[i * ac]
    s.writeRegion(r, px)
    y = r.y1
  s.prune()
  d.layers[0].name = "Layer 0"
  d.layers[0].locks.transparency = false
  d.layers[0].locks.position = false
  d.channels.setLen(0)

proc importTiffPage*(name: string; bytes: openArray[byte]; page = -1): ImportResult =
  ## A TIFF page (`-1`: the default page) as a document; Photoshop layer data opens layered.
  var keep = defaultDecodeOptions()
  keep.keepOrientation = true
  var img: Image
  var orientation: int
  try:
    if page < 0:
      img = decode(bytes, keep)
      orientation = tiffOrientation(bytes)
    else:
      img = decodeTiffPage(bytes, page, keep)
      orientation = tiffPageOrientation(bytes, page)
  except CodecError as e: raise cErr(e)
  if img.meta.photoshopLayers.isSome and img.meta.photoshopLayers.get.len > 0:
    result = importLayered(name, img, img.meta.photoshopLayers.get)
    if orientation != 1:
      result.warnings.add "the TIFF's orientation tag was ignored so the layers stay aligned with the image"
    return
  try: img = img.oriented(orientation)
  except CodecError as e: raise cErr(e)
  imageToDocument(name, img)

proc importFlat*(name: string; bytes: openArray[byte]): ImportResult =
  let fmt = detect(bytes)
  if fmt == some(fmtTiff): return importTiffPage(name, bytes)
  var img: Image
  try: img = decode(bytes)
  except CodecError as e: raise cErr(e)
  result = imageToDocument(name, img)
  if fmt == some(fmtTga) and img.layout == clRgba: tgaAlphaToChannel(result.document)
  template d: untyped = result.document
  if d.iccProfile.isNil and (fmt == some(fmtOpenExr) or fmt == some(fmtHdr)):
    case d.mode
    of cmRgb: d.iccProfile = shared(bLinearSrgb.profile.toBytes)
    of cmGrayscale: d.iccProfile = shared(linearGray().toBytes)
    else: discard

proc singleLayer*(d: Document): Option[Surface] =
  ## The one raster layer whose pixels are exactly the composite, when there is one.
  if d.layers.len != 1: return
  let l = d.layers[0]
  let ok = l.visible and l.opacity >= 1 and l.fillOpacity >= 1 and l.mask.isNone and
    (l.vectorMask.isNone or not l.vectorMask.get.enabled) and l.effects.items.len == 0 and
    l.effects.psdRaw.isNil and l.blend in {bmNormal, bmPassThrough} and
    channelWeights(l, d.mode).isNone and not blendIfActive(l, d.mode)
  if ok and l.content.kind == lkRaster and l.content.raster.format == d.pixelFormat:
    return some(l.content.raster)

proc layoutFor*(mode: ColorMode; alpha: bool): ChannelLayout =
  case mode
  of cmGrayscale: (if alpha: clGrayA else: clGray)
  of cmCmyk: (if alpha: clCmykA else: clCmyk)
  else: (if alpha: clRgba else: clRgb)

proc csampleOf*(s: SampleType): ImgSample =
  case s
  of stU8: isU8
  of stU16: isU16
  of stF32: isF32

proc dropAlpha(data: var seq[byte]; color, alpha: int) =
  let stride = color + alpha
  let n = data.len div stride
  for i in 0 ..< n:
    if i * color != i * stride:
      moveMem(addr data[i * color], addr data[i * stride], color)
  data.setLen(n * color)

proc opaqueSurface(s: Surface; r: Rect): bool =
  let fmt = s.format
  let (ch, bps) = (fmt.channels, fmt.sample.bytes)
  let a = ch - 1
  var default = newSeq[byte](ch * bps)
  encodePixel(fmt, s.defaultPixel, default)
  for tc in r.tiles:
    let tr = tc.rect.intersect(r)
    let t = s.tile(tc)
    if t.isNil:
      if readSample(default, fmt.sample, a) < 1: return false
      continue
    let tr0 = tc.rect
    for y in tr.y0 ..< tr.y1:
      let row = ((y - tr0.y0) * TileSize + (tr.x0 - tr0.x0)) * ch * bps
      for x in 0 ..< tr.width:
        let o = row + x * ch * bps
        if readSample(t.data.toOpenArray(o, o + ch * bps - 1), fmt.sample, a) < 1: return false
  true

proc documentToImage*(d: Document; warnings: var seq[string]): Image =
  ## The composite as a codec image: a lone raster layer's pixels as they are, otherwise the
  ## flattened render (RGB or gray; CMYK and Lab as sRGB).
  let (w, h) = (d.size.width, d.size.height)
  let canvas = d.bounds
  let fmt = d.pixelFormat
  let n = w * h
  let native = if fmt.mode != cmLab: singleLayer(d) else: none(Surface)
  var icc = if d.iccProfile.isNil: none(seq[byte]) else: some(d.iccProfile.data)
  if native.isSome:
    let s = native.get
    let ch = fmt.channels
    let opaque = opaqueSurface(s, canvas)
    let layout = layoutFor(fmt.mode, not opaque)
    let bps = fmt.sample.bytes
    let keep = if opaque: (ch - 1) * bps else: ch * bps
    var data = tryBuffer(n, keep)
    let rows = max(BandBytes div max(w * ch * bps, 1), 1)
    var y = 0
    while y < h:
      let y1 = min(satAdd(y, rows), h)
      let band = s.toInterleaved(rect(0, y, w, y1))
      if opaque:
        var p = 0
        while p + ch * bps <= band.len:
          data.add band.toOpenArray(p, p + keep - 1)
          p += ch * bps
      else:
        data.add band
      y = y1
    try: result = imageFromRaw(w, h, layout, csampleOf(fmt.sample), data)
    except CodecError as e: raise cErr(e)
  else:
    warnings.add $d.layerCount & " layer(s) flattened; layers, masks and blend modes are not kept"
    let gray = fmt.mode == cmGrayscale
    if fmt.mode == cmCmyk or fmt.mode == cmLab:
      warnings.add $fmt.mode & " composite written as sRGB RGB (colour-managed conversion)"
      icc = some(bSrgb.profile.toBytes)
    let cs = csampleOf(fmt.sample)
    let colors = if gray: 1 else: 3
    var data = tryBuffer(n, (colors + 1) * cs.bytes)
    var opaque = true
    renderBands(d, canvas, 0, proc(band: Buffer) =
      for p in band.px:
        if not (p[3] >= 1): opaque = false
      proc put(v0: float32) =
        let v = if v0 != v0: 0'f32 else: v0
        case cs
        of isU8: data.add uint8(round(clamp(v, 0'f32, 1'f32) * 255))
        of isU16:
          let u = uint16(round(clamp(v, 0'f32, 1'f32) * 65535))
          data.add cast[array[2, byte]](u)
        of isF16, isF32: data.add cast[array[4, byte]](v)
      for p in band.px:
        if gray: put(rgbToGray([p[0], p[1], p[2]]))
        else:
          put(p[0]); put(p[1]); put(p[2])
        put(p[3]))
    if opaque: dropAlpha(data, colors * cs.bytes, cs.bytes)
    let layout = layoutFor(if gray: cmGrayscale else: cmRgb, not opaque)
    try: result = imageFromRaw(w, h, layout, cs, data)
    except CodecError as e: raise cErr(e)
  result.icc = icc
  result.meta = codecs.Metadata(exif: (if d.metadata.exif.isNil: none(seq[byte]) else: some(d.metadata.exif.data)),
                                xmp: d.metadata.xmp, text: d.metadata.text,
                                dpi: some((d.resolutionDpi, d.resolutionDpi)))

proc compositeShare(format: Format): float32 =
  case format
  of fmtTiff: 0.3
  of fmtJpeg: 0.55
  else: 0.7

proc fidelityText(w: FidelityWarning): string = $w

proc encodeImage*(img: Image; format: Format; opts: ExportOptions; warnings0: seq[string]): ExportResult =
  var warnings = warnings0
  for w in fidelityWarnings(img, format, opts.encode):
    if w.isFatal: raise ioError(ieUnsupported, fidelityText(w))
    warnings.add fidelityText(w)
  try: ExportResult(bytes: encode(img, format, opts.encode), warnings: warnings)
  except CodecError as e: raise cErr(e)

proc mapBands(img: Image; layout: ChannelLayout; sample: ImgSample; f: proc(v: seq[float32]): seq[float32]): Image =
  let (w, h) = (img.width, img.height)
  let row = img.data.len div max(h, 1)
  let band = max(row, 1) * max(BandBytes div max(row, 1), 1)
  var data = tryBuffer(img.pixelCount, layout.channels * sample.bytes)
  var at = 0
  try:
    while at < img.data.len:
      let e = min(at + band, img.data.len)
      let n = (e - at) div max(row, 1)
      let vals = f(imageFromRaw(w, n, img.layout, img.sample, img.data[at ..< e]).toNormalized)
      data.add imageFromNormalized(w, n, layout, sample, vals).data
      at = e
    result = imageFromRaw(w, h, layout, sample, data)
  except CodecError as e: raise cErr(e)
  result.icc = img.icc
  result.meta = img.meta

proc matteOverWhite(img: Image): Image =
  let layout = img.layout
  let white = if layout.isCmyk: 0'f32 else: 1'f32
  let ch = layout.channels
  let cc = layout.colorChannels
  mapBands(img, layout.withoutAlpha, img.sample, proc(vals: seq[float32]): seq[float32] =
    result = newSeqOfCap[float32](vals.len div ch * cc)
    var p = 0
    while p + ch <= vals.len:
      let a = clamp(vals[p + ch - 1], 0'f32, 1'f32)
      for c in 0 ..< ch - 1: result.add vals[p + c] * a + white * (1 - a)
      p += ch)

proc parsedOr(icc: Option[seq[byte]]; space: ColorSpaceKind; fallback: Profile): Profile =
  if icc.isSome:
    try:
      let p = parseProfile(icc.get)
      if p.colorSpace.kind == space: return p
    except CatchableError: discard
  fallback

proc convertFrom(img: Image; src, dst: Profile; intent: Intent; bpc: bool; sample: ImgSample): Option[Image] =
  if sameColors(src, dst): return none(Image)
  var t: Transform
  try: t = newTransform(src, dst, intent, bpc)
  except CatchableError as e: raise ioError(ieUnsupported, e.msg)
  let stride = img.layout.channels
  var o = mapBands(img, img.layout, sample, proc(v: seq[float32]): seq[float32] =
    result = v
    t.apply(result, stride))
  o.icc = none(seq[byte])
  some(o)

proc convertRgb(img: Image; dst: Profile; intent: Intent; bpc: bool; sample: ImgSample): Option[Image] =
  if not img.layout.isRgb: return none(Image)
  convertFrom(img, parsedOr(img.icc, csRgb, bSrgb.profile), dst, intent, bpc, sample)

proc convertGray(img: Image; dst: Profile; sample: ImgSample): Option[Image] =
  if img.layout notin {clGray, clGrayA}: return none(Image)
  convertFrom(img, parsedOr(img.icc, csGray, bSGray.profile), dst, RelativeColorimetric, false, sample)

proc cmykImageToSrgb(img: Image): Image =
  let src = parsedOr(img.icc, csCmyk, bCoatedCmyk.profile)
  let dst = bSrgb.profile
  var t: Transform
  try: t = newTransform(src, dst, RelativeColorimetric, true)
  except CatchableError as e: raise ioError(ieUnsupported, e.msg)
  let alpha = img.layout.hasAlpha
  let ss = (if alpha: 5 else: 4)
  let ds = (if alpha: 4 else: 3)
  let (w, h) = (img.width, img.height)
  let layout = if alpha: clRgba else: clRgb
  let sample = if img.sample == isF16: isF32 else: img.sample
  let row = w
  let srcBpp = img.layout.channels * img.sample.bytes
  let bandRows = clamp(BandBytes div max(row * ss * 4, 1), 1, max(h, 1))
  var o = newSeqOfCap[float32](img.pixelCount * ds)
  var idx = 0
  try:
    while idx < h:
      let n = min(bandRows, h - idx)
      let a = idx * row * srcBpp
      let b = (idx + n) * row * srcBpp
      if b > img.data.len: raise ioError(ieUnsupported, "CMYK image data is shorter than its dimensions")
      idx += n
      let vals = imageFromRaw(w, n, img.layout, img.sample, img.data[a ..< b]).toNormalized
      var dstBand = newSeq[float32](n * row * ds)
      t.convertF32(vals, ss, dstBand, ds, true)
      o.add dstBand
    result = imageFromNormalized(w, h, layout, sample, o)
  except CodecError as e: raise cErr(e)
  result.icc = some(dst.toBytes)
  result.meta = img.meta

proc tgaDefaultBits*(d: Document): TgaBits =
  for c in d.channels:
    if c.spot.isNone: return tb32
  tb24

proc tgaWithAlphaChannel(d: Document; img0: Image; channel: AlphaChannel; warnings: var seq[string]): Image =
  var img = img0
  if img.layout.hasAlpha:
    warnings.add "transparency composited over white; the alpha channel is the Targa's alpha"
    img = matteOverWhite(img)
  let (w, h) = (img.width, img.height)
  let canvas = d.bounds
  if canvas.width != w or canvas.height != h:
    raise ioError(ieUnsupported, "the composite and the alpha channel differ in size")
  let sample = img.sample
  let row = img.data.len div max(h, 1)
  let band = max(BandBytes div max(row, 1), 1)
  let stride = max(channel.surface.channels, 1)
  var data = tryBuffer(img.pixelCount, clRgba.channels * sample.bytes)
  var y0 = canvas.y0
  var at = 0
  try:
    while at < img.data.len:
      let e = min(at + max(row, 1) * band, img.data.len)
      let n = (e - at) div max(row, 1)
      let y1 = satAdd(y0, n)
      let alpha = channel.surface.readRegion(rect(canvas.x0, y0, canvas.x1, y1))
      let rgb = imageFromRaw(w, n, img.layout, sample, img.data[at ..< e]).toNormalized
      var vals = newSeqOfCap[float32](rgb.len div 3 * 4)
      var i = 0
      while i * 3 + 2 < rgb.len and i * stride < alpha.len:
        vals.add rgb[i * 3]; vals.add rgb[i * 3 + 1]; vals.add rgb[i * 3 + 2]
        vals.add alpha[i * stride]
        inc i
      data.add imageFromNormalized(w, n, clRgba, sample, vals).data
      y0 = y1
      at = e
    result = imageFromRaw(w, h, clRgba, sample, data)
  except CodecError as e: raise cErr(e)
  result.icc = img.icc
  result.meta = img.meta

proc tgaImage(d: Document; img: Image; bits0: Option[TgaBits]; warnings: var seq[string]): Image =
  if not img.layout.isRgb:
    if bits0.isSome:
      warnings.add $bits0.get.bits & "-bit Targa applies to RGB images; the grayscale image is written as it is"
    return img
  var alphas: seq[AlphaChannel]
  for c in d.channels:
    if c.spot.isNone: alphas.add c
  let bits = if bits0.isSome: bits0.get
             elif alphas.len > 0 or img.layout.hasAlpha: tb32
             else: tb24
  case bits
  of tb24:
    if alphas.len > 0:
      var names: seq[string]
      for c in alphas: names.add "\"" & c.name & "\""
      warnings.add "a 24-bit Targa has no alpha; not saved: " & names.join(", ")
    if not img.layout.hasAlpha: return img
    warnings.add "transparency composited over white; a 24-bit Targa has no alpha"
    matteOverWhite(img)
  of tb32:
    if alphas.len == 1: return tgaWithAlphaChannel(d, img, alphas[0], warnings)
    if alphas.len > 0:
      warnings.add "Targa holds one alpha channel; with several, none was written (as in Photoshop)"
    if img.layout.hasAlpha: return img
    mapBands(img, clRgba, img.sample, proc(v: seq[float32]): seq[float32] =
      result = newSeqOfCap[float32](v.len div 3 * 4)
      var i = 0
      while i + 2 < v.len:
        result.add v[i]; result.add v[i + 1]; result.add v[i + 2]; result.add 1'f32
        i += 3)

proc exportFlatWith*(d: Document; format: Format; opts: ExportOptions; ctl: Interrupt): ExportResult

proc exportModeSpecific(d: Document; format: Format; opts: ExportOptions): Option[ExportResult] =
  case d.mode
  of cmIndexed:
    if format != fmtPng: return
    if d.colorTable.isNone or d.colorTable.get.colors.len == 0 or d.colorTable.get.colors.len > 256: return
    let table = d.colorTable.get
    var idx = tryBuffer(int(d.size.area), 1)
    renderBands(d, d.bounds, 0, proc(band: Buffer) =
      for p in band.px:
        if table.transparent.isSome and p[3] < 0.5'f32: idx.add table.transparent.get
        else: idx.add uint8(table.nearest([p[0], p[1], p[2]])))
    let dpi = if opts.encode.embedMetadata: some((d.resolutionDpi, d.resolutionDpi)) else: none((float32, float32))
    var bytes: seq[byte]
    try: bytes = encodePngIndexed(d.size.width, d.size.height, idx, table.colors, table.transparent, dpi)
    except CodecError as e: raise cErr(e)
    var warnings = @["written as an 8-bit palette PNG (" & $table.colors.len & " colours)"]
    if opts.encode.embedMetadata and opts.xmp == xeAll and d.metadata.text.len > 0:
      warnings.add "text metadata is not supported by the palette PNG exporter; it will be dropped"
    some(ExportResult(bytes: bytes, warnings: warnings))
  of cmDuotone:
    if d.duotone.isNone: return
    let du = d.duotone.get
    var shown = d
    shown.layers.add newLayer("Duotone", adjustmentContent(du.displayAdjustment))
    shown.mode = cmRgb
    shown.iccProfile = nil
    shown.duotone = none(Duotone)
    var r = exportFlatWith(shown, format, opts, NoInterrupt)
    var kept: seq[string]
    for w in r.warnings:
      if "flattened" notin w: kept.add w
    r.warnings = kept
    r.warnings.add "Duotone (" & $du.inks.len & " inks) written as RGB"
    some(r)
  else: none(ExportResult)

proc exportFlatWith*(d: Document; format: Format; opts: ExportOptions; ctl: Interrupt): ExportResult =
  let special = exportModeSpecific(d, format, opts)
  if special.isSome: return special.get
  var warnings: seq[string]
  var img = documentToImage(d, warnings)
  if ctl.cancelled: raise ioError(ieCancelled)
  ctl.progress(compositeShare(format))
  if opts.xmp == xeNone:
    img.meta.xmp = none(string)
    img.meta.text.setLen(0)
  if img.layout.hasAlpha and not format.caps.alpha:
    img = matteOverWhite(img)
    warnings.add "transparency composited over white for " & format.debugName
  var cmykOk = false
  for l in format.caps.layouts:
    if l.isCmyk: cmykOk = true
  if img.layout.isCmyk and not cmykOk:
    img = cmykImageToSrgb(img)
    warnings.add "CMYK converted to sRGB for " & format.debugName & " through the document's colour profile"
  if format == fmtOpenExr or format == fmtHdr:
    let linear = convertRgb(img, bLinearSrgb.profile, RelativeColorimetric, false, isF32)
    if linear.isSome: img = linear.get
    else:
      let lg = convertGray(img, linearGray(), isF32)
      if lg.isSome: img = lg.get
  elif not format.caps.icc:
    let srgb = convertRgb(img, bSrgb.profile, Perceptual, true, img.sample)
    if srgb.isSome:
      img = srgb.get
      warnings.add "colours converted to sRGB; " & format.debugName & " can't embed the document's colour profile"
  if format == fmtTga: img = tgaImage(d, img, opts.tgaBits, warnings)
  encodeImage(img, format, opts, warnings)

proc exportFlat*(d: Document; format: Format; opts: ExportOptions): ExportResult =
  exportFlatWith(d, format, opts, NoInterrupt)
