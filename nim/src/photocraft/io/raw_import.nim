## Camera raw files via `photocraft/raw` (port of `io/src/raw.rs`): developed to a 16-bit RGB
## document in ProPhoto RGB (the embedded profile is the built-in ProPhoto-compatible profile),
## or, for raw variants not decoded yet, the camera's embedded JPEG preview with a warning.
##
## Files without colour calibration (everything but DNG) get their colour from the camera's own
## embedded JPEG: the preview is decoded here and handed to `fitLook`, which fits a matrix and
## tone curve and keeps the neutral fallback when the fit is not clearly better.

import std/[options, strutils, math]
import ../doc, ../codecs, ../cms
import ../raw as praw
import ../algo/camera_raw
import ./common, ./flat

proc isRaw*(bytes: openArray[byte]): bool =
  ## `true` if `bytes` are a camera raw file `photocraft/raw` recognises.
  praw.isRaw(bytes)

proc limits(): RawLimits =
  ## The decode limits shared with the flat codecs.
  let l = defaultLimits()
  RawLimits(maxWidth: uint32(l.maxWidth), maxHeight: uint32(l.maxHeight), maxPixels: uint64(l.maxPixels),
            maxAlloc: uint64(l.maxAlloc))

proc rawErr(e: ref RawError): ref IoFailure = ioError(ieRaw, e.msg)

proc uprightPreview(raw, jpeg: openArray[byte]): Image =
  ## A raw file's embedded JPEG preview, turned upright. Its own EXIF orientation wins when it
  ## has one; otherwise the raw's IFD0 orientation applies (TIFF-based raws record it there).
  var opts = defaultDecodeOptions()
  opts.keepOrientation = true
  var img: Image
  try: img = decodeAs(fmtJpeg, jpeg, opts)
  except CodecError as e: raise ioError(ieCodec, e.msg)
  let own = if img.meta.exif.isSome: exifOrientation(img.meta.exif.get) else: 1
  let o = if own != 1: own else: exifOrientation(raw)
  try: img.oriented(o)
  except CodecError as e: raise ioError(ieCodec, e.msg)

type
  RawTuning* = object
    ## Camera Raw's Temperature, Tint and Exposure, applied while developing the sensor data (the
    ## open-time Camera Raw dialog) instead of to the developed RGB pixels.
    temperature*: float32 ## −100…100, as `CameraRaw.temperature`
    tint*: float32        ## −100…100, as `CameraRaw.tint`
    exposure*: float32    ## EV, as `CameraRaw.exposure`

proc cameraName(make, model: Option[string]): string =
  ## "Canon" + "Canon EOS 80D" and "NIKON CORPORATION" + "NIKON D3200" read as the model alone.
  if make.isSome and model.isSome:
    let words = make.get.splitWhitespace
    if words.len > 0 and model.get.toLowerAscii.startsWith(words[0].toLowerAscii): return model.get
  var parts: seq[string]
  if make.isSome: parts.add make.get
  if model.isSome: parts.add model.get
  parts.join(" ")

proc cameraNameForTest*(make, model: Option[string]): string = cameraName(make, model)

proc wbGains(temperature, tint: float32): array[3, float64] =
  ## Camera Raw's white-balance gains for Temperature / Tint (−100…100), as the filter applies.
  let g = whiteBalanceGains(clamp(temperature, -100'f32, 100'f32), clamp(tint, -100'f32, 100'f32))
  [float64(g[0]), float64(g[1]), float64(g[2])]

proc developedDocument(name, format: string; dev: Developed; summary: string): ImportResult =
  ## A developed raw as a 16-bit ProPhoto document, with a note saying how it was developed.
  var bytes = newSeq[byte](dev.rgb.len * 2)
  for i, v in dev.rgb:
    bytes[2 * i] = byte(v and 0xFF)
    bytes[2 * i + 1] = byte(v shr 8)
  var img: Image
  try: img = imageFromRaw(int(dev.width), int(dev.height), clRgb, isU16, bytes)
  except CodecError as e: raise ioError(ieCodec, e.msg)
  result = imageToDocument(name, img)
  result.document.iccProfile = SharedBytes(data: bProPhotoCompat.profile.toBytes)
  let camera = cameraName(dev.info.make, dev.info.model)
  result.warnings.add format & (if camera.len == 0: "" else: " from " & camera) & " " & summary & " into 16-bit " & OutputSpace
  for w in dev.warnings: result.warnings.add w

const FitPreviewEdge = 2 * ProxyEdge
  ## Smallest preview long edge preferred for the colour fit (the fit grid wants about two pixels
  ## per cell).

proc fitPreviewLook*(raw: openArray[byte]; sensor: Sensor; opts: DevelopOptions): CameraLook =
  ## Fits the colour of a decoded raw to the camera's embedded JPEG: picks the smallest preview at
  ## least `FitPreviewEdge` pixels long (else the largest), decodes it, turns it upright and
  ## converts it to sRGB when it carries another ICC profile. Raises `ValueError` saying why there
  ## is no fit.
  let previews = embeddedPreviews(raw)
  proc long(p: Preview): uint32 = max(p.width, p.height)
  var pick = -1
  for i, p in previews:
    if long(p) >= uint32(FitPreviewEdge) and (pick < 0 or long(p) < long(previews[pick])): pick = i
  if pick < 0:
    for i, p in previews:
      if pick < 0 or long(p) >= long(previews[pick]): pick = i # the last of the largest
  if pick < 0: raise newException(ValueError, "no embedded JPEG preview")
  let p = previews[pick]
  var img: Image
  try: img = uprightPreview(raw, raw.toOpenArray(p.offset, p.offset + p.len - 1))
  except IoFailure as e: raise newException(ValueError, "the embedded preview does not decode: " & e.msg)
  if img.layout != clRgb or img.sample != isU8: raise newException(ValueError, "the embedded preview is not 8-bit RGB")
  var rgb = img.data
  if img.icc.isSome:
    let srgb = bSrgb.profile
    var src: Profile
    try: src = parseProfile(img.icc.get)
    except CatchableError as e: raise newException(ValueError, "the preview's ICC profile does not parse: " & e.msg)
    if not src.sameColors(srgb):
      var t: Transform
      try: t = newTransform(src, srgb, RelativeColorimetric, false)
      except CatchableError as e: raise newException(ValueError, e.msg)
      var outp = newSeq[byte](rgb.len)
      t.convertU8(rgb, 3, outp, 3, false)
      rgb = outp
  try: fitLook(sensor, opts, Reference(width: uint32(img.width), height: uint32(img.height), rgb: rgb))
  except LookRejected as e: raise newException(ValueError, e.msg)

proc developFitted(bytes: openArray[byte]; opts: DevelopOptions): Developed =
  ## Decodes and develops; a file without colour calibration gets a look fitted to its preview.
  let sensor = decode(bytes, opts.limits)
  var o = opts
  if o.cameraLook.isNone and sensor.color.calibrations.len == 0 and not sensor.isMonochrome:
    try: o.cameraLook = some(fitPreviewLook(bytes, sensor, o))
    except ValueError: discard
  developSensor(sensor, o)

const DevelopedNote* = "developed with default settings"
  ## In the import note of a raw developed from its sensor data with the default settings (not of
  ## a preview fallback): what the open-time Camera Raw dialog looks for.

proc formatName(bytes: openArray[byte]): string =
  let f = identify(bytes)
  if f.isSome: $f.get else: "camera raw"

proc signedFixed(v: float32; prec: int): string =
  ## Rust `{:+.N}`.
  if v != v: return "NaN"
  if v == Inf: return "+inf"
  if v == NegInf: return "-inf"
  let s = formatFloat(float64(v), ffDecimal, prec)
  if s.startsWith("-"): s else: "+" & s

proc importRawWith*(name: string; bytes: openArray[byte]; opts: DevelopOptions): ImportResult =
  ## Develops a raw file with explicit settings. Without `opts.cameraLook`, a file without colour
  ## calibration gets one fitted to its embedded JPEG when that clearly helps.
  let format = formatName(bytes)
  try:
    let dev = developFitted(bytes, opts)
    let summary = DevelopedNote & " (" & opts.demosaic.id & " demosaic, as-shot white balance)"
    return developedDocument(name, format, dev, summary)
  except RawError as e:
    if e.kind != reUnsupported: raise rawErr(e)
    let p = embeddedPreview(bytes)
    if p.isNone: raise rawErr(e)
    let img = uprightPreview(bytes, bytes.toOpenArray(p.get.offset, p.get.offset + p.get.len - 1))
    result = imageToDocument(name, img)
    result.warnings.insert(format & ": " & e.detail & " is not supported yet; opened the camera's embedded " &
      $p.get.width & "x" & $p.get.height & " JPEG preview instead (8-bit, not the raw sensor data)", 0)

proc importRaw*(name: string; bytes: openArray[byte]): ImportResult =
  ## Develops a raw file with the default settings.
  var o = defaultDevelopOptions()
  o.limits = limits()
  importRawWith(name, bytes, o)

proc importRawTuned*(name: string; bytes: openArray[byte]; tuning: RawTuning): (ImportResult, bool) =
  ## Develops a raw file with `tuning` applied at the raw stage: Exposure as develop exposure,
  ## Temperature / Tint as gains on the camera's as-shot white-balance multipliers. The second
  ## value is false when the file has no as-shot white balance: Temperature / Tint were then not
  ## applied, and the caller keeps them as RGB adjustments.
  let format = formatName(bytes)
  let exposure = float64(tuning.exposure)
  if not (exposure == exposure and exposure >= -10.0 and exposure <= 10.0):
    raise ioError(ieRaw, "malformed camera raw: exposure " & rustFloat(tuning.exposure) & " EV is out of range")
  var sensor: Sensor
  try: sensor = decode(bytes, limits())
  except RawError as e: raise rawErr(e)
  var opts = defaultDevelopOptions()
  opts.limits = limits()
  # The camera rendering of a normal import, fitted at the camera's default WB and exposure before
  # applying the user controls, so the fit cannot cancel their adjustments.
  if sensor.color.calibrations.len == 0:
    try: opts.cameraLook = some(fitPreviewLook(bytes, sensor, opts))
    except ValueError: discard
  opts.exposure = exposure
  var wbApplied = false
  if sensor.cameraWb.isSome:
    let m = sensor.cameraWb.get
    let g = wbGains(tuning.temperature, tuning.tint)
    opts.whiteBalance = multipliersWb([m[0] * g[0], m[1] * g[1], m[2] * g[2]])
    wbApplied = true
  var dev: Developed
  try: dev = developSensor(sensor, opts)
  except RawError as e: raise rawErr(e)
  let wb = if wbApplied: "as-shot white balance, temperature " & signedFixed(tuning.temperature, 0) & ", tint " &
                         signedFixed(tuning.tint, 0)
           else: "estimated white balance"
  let summary = "developed in Camera Raw (exposure " & signedFixed(tuning.exposure, 2) & " EV, " & wb & ")"
  (developedDocument(name, format, dev, summary), wbApplied)
