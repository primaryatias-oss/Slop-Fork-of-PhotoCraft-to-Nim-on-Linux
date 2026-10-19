## Digital Negative (port of `raw/src/dng.rs`; Adobe DNG Specification 1.7): raw IFD selection,
## CFA description, linearization, black/white levels, active area, default crop, colour
## calibration and white balance.

import std/[options, math, strutils]
import ./tiff, ./color, ./sensor, ./opcodes

const
  PhotometricCfa* = 32803'u32
  PhotometricLinearRaw* = 34892'u32

proc rawIfd(t: Tiff; ifds: openArray[Ifd]): Option[Ifd] =
  ## The main raw image: NewSubFileType 0 with CFA or LinearRaw data, largest first (the last of
  ## equally large ones, like `Iterator::max_by_key`).
  var best = 0'u64
  for i in ifds:
    if t.tagUint(i, TagNewSubfileType).get(0) != 0: continue
    let p = t.tagUint(i, TagPhotometric)
    if p != some(PhotometricCfa) and p != some(PhotometricLinearRaw): continue
    let px = uint64(t.tagUint(i, TagImageWidth).get(0)) * uint64(t.tagUint(i, TagImageLength).get(0))
    if result.isNone or px >= best:
      best = px
      result = some(i)

proc matrix(t: Tiff; ifd: Ifd; tg: uint16): Option[Mat3] =
  let v = t.tagFloats(ifd, tg)
  if v.len == 9: result = fromSlice(v)

proc triple(t: Tiff; ifd: Ifd; tg: uint16): Option[array[3, float64]] =
  let v = t.tagFloats(ifd, tg)
  if v.len != 3: return
  for x in v:
    if not (x == x and x != Inf and x != NegInf and x > 0.0): return
  some([v[0], v[1], v[2]])

proc invertible(m: Option[Mat3]): Option[Mat3] =
  if m.isSome and invert(m.get).isSome: m else: none(Mat3)

proc colorInfo*(t: Tiff; ifd0: Ifd): ColorInfo =
  ## Colour calibration from IFD0.
  var cal: seq[Calibration]
  for (cm, cc, fm, ill, defaultT) in [
      (TagColorMatrix1, TagCameraCalibration1, TagForwardMatrix1, TagCalibrationIlluminant1, 2856.0),
      (TagColorMatrix2, TagCameraCalibration2, TagForwardMatrix2, TagCalibrationIlluminant2, 6504.0)]:
    let colorMatrix = matrix(t, ifd0, cm)
    if colorMatrix.isNone or invert(colorMatrix.get).isNone: continue
    let code = t.tagUint(ifd0, ill)
    let temp = if code.isSome: illuminantTemperature(code.get) else: none(float64)
    cal.add Calibration(temperature: temp.get(defaultT), colorMatrix: colorMatrix.get,
                        forwardMatrix: invertible(matrix(t, ifd0, fm)),
                        cameraCalibration: invertible(matrix(t, ifd0, cc)).get(Identity))
  sortCalibrations(cal)
  if cal.len == 2 and cal[0].forwardMatrix.isSome != cal[1].forwardMatrix.isSome:
    for c in cal.mitems: c.forwardMatrix = none(Mat3)
  var whiteXy = none(array[2, float64])
  let v = t.tagFloats(ifd0, TagAsShotWhiteXy)
  if v.len == 2 and v[0] > 0.0 and v[0] < 1.0 and v[1] > 0.0 and v[1] < 1.0: whiteXy = some([v[0], v[1]])
  ColorInfo(calibrations: cal, analogBalance: triple(t, ifd0, TagAnalogBalance),
            asShotNeutral: triple(t, ifd0, TagAsShotNeutral), asShotWhiteXy: whiteXy)

proc debugList(v: openArray[uint32]): string =
  ## Rust `{:?}` of a `Vec<u32>`.
  result = "["
  for i, x in v:
    if i > 0: result.add ", "
    result.add $x
  result.add "]"

proc cfa(t: Tiff; ifd: Ifd; active: RawRect): Cfa =
  ## CFA pattern of a DNG raw IFD, anchored at the active area origin.
  let dim = t.tagUints(ifd, TagCfaRepeatPatternDim)
  let (rows, cols) = if dim.len == 2: (int(dim[0]), int(dim[1])) else: (2, 2)
  let pattern = t.tagUints(ifd, TagCfaPattern)
  if rows == 0 or cols == 0 or rows > 16 or cols > 16 or pattern.len != rows * cols:
    raise malformed("bad CFA pattern")
  var planes = t.tagUints(ifd, TagCfaPlaneColor)
  if planes.len == 0: planes = @[0'u32, 1, 2]
  if planes != @[0'u32, 1, 2]:
    raise unsupported("CFA plane colours " & debugList(planes) & " (only red, green, blue)")
  if t.tagUint(ifd, TagCfaLayout).get(1) != 1: raise unsupported("non-rectangular CFA layout")
  var colors: seq[uint8]
  for code in pattern:
    if code > 2: raise unsupported("CFA colours other than red, green and blue")
    colors.add uint8(code)
  result = Cfa(width: cols, height: rows, colors: colors, originX: active.x, originY: active.y)
  if not result.isBayer: raise unsupported($rows & "x" & $cols & " non-Bayer CFA (e.g. X-Trans)")

proc rectFrom(t: Tiff; ifd: Ifd; width, height: int): RawRect =
  let full = rawRect(0, 0, width, height)
  let v = t.tagUints(ifd, TagActiveArea)
  if v.len == 4 and v[2] > v[0] and v[3] > v[1]:
    rawRect(int(v[1]), int(v[0]), int(v[3] - v[1]), int(v[2] - v[0])).intersect(full)
  else: full

proc cropValue(value: float64; name: string): int =
  if not (value == value and value != Inf) or value < 0.0: raise malformed("invalid DNG " & name)
  let rounded = round(value)
  if rounded >= 18446744073709551616.0: raise malformed("DNG " & name & " is not representable")
  # Values past `int` (the Rust `usize` is unsigned) can never fit inside an active area: they
  # saturate and fail the bounds check below, as the Rust checked additions do.
  if rounded >= 9.2233720368547758e18: high(int) else: int(rounded)

proc cropPair(t: Tiff; raw: Ifd; tg: uint16; default: array[2, float64]): Option[array[2, float64]] =
  if not raw.has(tg): return some(default)
  let v = t.tagFloats(raw, tg)
  if v.len == 2: result = some([v[0], v[1]])

proc checkedAdd(a, b: int): Option[int] =
  if a > high(int) - b: none(int) else: some(a + b)

proc decodeDng*(t: Tiff; limits: RawLimits): Sensor =
  ## Decodes a DNG file's raw image and metadata.
  let ifds = t.allIfds
  let ifd00 = t.ifdAt(t.firstIfd, 0)
  if ifd00.isNone: raise malformed("DNG has no IFD0")
  let ifd0 = ifd00.get
  let raw0 = rawIfd(t, ifds)
  if raw0.isNone: raise malformed("DNG has no raw image")
  let raw = raw0.get
  var warnings: seq[string]
  let photometric = t.tagUint(raw, TagPhotometric).get(0)
  var plane = readPlane(t, raw, limits, jlFlat)
  let samples = plane.samples
  let isCfa = photometric == PhotometricCfa
  if isCfa and samples != 1: raise unsupported("CFA data with " & $samples & " samples per pixel")
  # LinearRaw: three colour samples, or one for a monochrome camera or scanner (DNG 1.7,
  # ColorPlanes 1: Leica Monochrom, VueScan film scans).
  if not isCfa and samples != 1 and samples != 3:
    raise unsupported("LinearRaw data with " & $samples & " samples per pixel")
  let active = rectFrom(t, raw, plane.width, plane.height)
  if active.isEmpty: raise malformed("empty active area")
  let cfaV = if isCfa: some(cfa(t, raw, active)) else: none(Cfa)

  # Default crop, relative to the active area. Each tag has its own default (DNG 1.7.1.0,
  # DefaultCropOrigin and DefaultCropSize): the origin (0, 0), the size the whole image.
  var crop = active
  if raw.has(TagDefaultCropOrigin) or raw.has(TagDefaultCropSize):
    # A present tag must hold two values; an absent one takes its default.
    template pair(tg: uint16; default: array[2, float64]): Option[array[2, float64]] = cropPair(t, raw, tg, default)
    let origin = pair(TagDefaultCropOrigin, [0.0, 0.0])
    let size = pair(TagDefaultCropSize, [float64(plane.width), float64(plane.height)])
    if origin.isSome and size.isSome:
      let ox = cropValue(origin.get[0], "DefaultCropOrigin")
      let oy = cropValue(origin.get[1], "DefaultCropOrigin")
      let cw = cropValue(size.get[0], "DefaultCropSize")
      let ch = cropValue(size.get[1], "DefaultCropSize")
      let activeRight = checkedAdd(active.x, active.width)
      if activeRight.isNone: raise malformed("DNG active-area right edge overflow")
      let activeBottom = checkedAdd(active.y, active.height)
      if activeBottom.isNone: raise malformed("DNG active-area bottom edge overflow")
      var candidate = none(RawRect)
      let (x, y) = (checkedAdd(active.x, ox), checkedAdd(active.y, oy))
      if x.isSome and y.isSome:
        let (right, bottom) = (checkedAdd(x.get, cw), checkedAdd(y.get, ch))
        if right.isSome and bottom.isSome and x.get >= active.x and y.get >= active.y and
            right.get <= activeRight.get and bottom.get <= activeBottom.get:
          candidate = some(rawRect(x.get, y.get, cw, ch))
      if candidate.isSome and not candidate.get.isEmpty: crop = candidate.get
      else: warnings.add "out-of-range or empty DNG default crop; active area used"
    else:
      warnings.add "incomplete DNG default crop; active area used"
  let scale = t.tagFloats(raw, TagDefaultScale)
  if scale.len == 2 and abs(scale[0] - scale[1]) > 1e-6:
    warnings.add "non-square pixels (DefaultScale) are not resampled"
  # Opcode lists: OpcodeList2 gain maps (lens shading) are applied; everything else is reported.
  var gainMaps: seq[GainMap]
  for (tg, what) in [(TagOpcodeList1, 1), (TagOpcodeList2, 2), (TagOpcodeList3, 3)]:
    let e = raw.get(tg)
    if e.isNone: continue
    let r = t.rawRange(e.get)
    if r.isNone: continue
    let (maps, skipped) = parseOpcodes(t.data.toOpenArray(r.get[0], r.get[0] + r.get[1] - 1))
    if what == 2: gainMaps = maps
    elif maps.len > 0: warnings.add "DNG OpcodeList" & $what & ": GainMap is not applied"
    if skipped.len > 0: warnings.add "DNG OpcodeList" & $what & ": " & skipped.join(", ") & " not applied"

  # DNG 1.6 ProfileGainTableMap: the local tone mapping that phones (Samsung Expert RAW) rely on
  # for their default look. Without it the rendering is darker in the shadows.
  if ifd0.has(TagProfileGainTableMap) or raw.has(TagProfileGainTableMap):
    warnings.add "DNG ProfileGainTableMap (local tone mapping) is not applied"

  let mx = float32((1'u32 shl plane.bits) - 1)
  var linearization = none(seq[uint16])
  let le = raw.get(TagLinearizationTable)
  if le.isSome:
    let v = t.uints(le.get)
    if v.len > 0 and v.len <= 65536:
      var l = newSeq[uint16](v.len)
      for i, x in v: l[i] = uint16(min(x, 65535'u32))
      linearization = some(l)
  var whiteDefault = mx
  if linearization.isSome:
    var m = 0'u16
    for x in linearization.get: m = max(m, x)
    whiteDefault = float32(m)
  let wl = t.tagFloats(raw, TagWhiteLevel)
  var white: array[3, float32]
  for s in 0 .. 2:
    let v = if s < wl.len: some(wl[s]) elif wl.len > 0: some(wl[0]) else: none(float64)
    let f = if v.isSome: float32(v.get) else: 0'f32
    white[s] = if v.isSome and f > 0'f32: f else: whiteDefault

  let dim = t.tagUints(raw, TagBlackLevelRepeatDim)
  let (rows, cols) = if dim.len == 2 and dim[0] in 1'u32 .. 16'u32 and dim[1] in 1'u32 .. 16'u32:
                       (int(dim[0]), int(dim[1])) else: (1, 1)
  let bl = t.tagFloats(raw, TagBlackLevel)
  var black: BlackLevels
  if bl.len == rows * cols * samples:
    black = BlackLevels(rows: rows, cols: cols, values: newSeq[float32](bl.len))
    for i, v in bl: black.values[i] = float32(v)
  else:
    black = uniformBlack(if bl.len > 0: float32(bl[0]) else: 0'f32)
  let dh = t.tagFloats(raw, TagBlackLevelDeltaH)
  if dh.len == active.width:
    black.deltaH = newSeq[float32](dh.len)
    for i, v in dh: black.deltaH[i] = float32(v)
  let dv = t.tagFloats(raw, TagBlackLevelDeltaV)
  if dv.len == active.height:
    black.deltaV = newSeq[float32](dv.len)
    for i, v in dv: black.deltaV[i] = float32(v)

  let colorI = colorInfo(t, ifd0)
  # A monochrome image has no colour to calibrate: it develops as neutral grey.
  if colorI.calibrations.len == 0 and (isCfa or samples != 1):
    warnings.add "the DNG has no colour matrix; colours are approximate"
  let be = t.tagFloats(ifd0, TagBaselineExposure)
  let beo = t.tagFloats(raw, TagBaselineExposureOffset)
  let baselineExposure = (if be.len > 0: be[0] else: 0.0) + (if beo.len > 0: beo[0] else: 0.0)
  var model = t.tagAscii(ifd0, TagUniqueCameraModel)
  if model.isNone: model = t.tagAscii(ifd0, TagModel)
  Sensor(format: rfDng, make: t.tagAscii(ifd0, TagMake), model: model, width: plane.width,
         height: plane.height, samples: samples, data: move plane.data, cfa: cfaV,
         linearization: linearization, black: black, white: white, active: active, crop: crop,
         color: colorI, cameraWb: none(array[3, float64]),
         orientation: orientation(t.tagUint(ifd0, TagOrientation)),
         baselineExposure: (if baselineExposure == baselineExposure and abs(baselineExposure) != Inf:
                              clamp(baselineExposure, -10.0, 10.0) else: 0.0),
         gainMaps: gainMaps, warnings: warnings)
