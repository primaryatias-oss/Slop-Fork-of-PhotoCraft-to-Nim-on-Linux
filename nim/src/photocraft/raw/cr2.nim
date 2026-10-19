## Canon CR2 (port of `raw/src/cr2.rs`).
##
## CR2 is a TIFF file whose header carries `CR`, the major and minor version and the offset of the
## raw IFD (IFD3) at bytes 8–15. The raw IFD stores one lossless JPEG whose decoded sample sequence
## fills the sensor image in vertical slices: the private tag 0xC640 gives the slice count, slice
## width and last slice width. sRAW/mRAW (subsampled YCbCr) is not supported.
##
## Sensor geometry and the as-shot white balance come from the Canon maker note (tag 0x00E0
## SensorInfo and 0x4001 ColorData, whose as-shot RGGB levels sit at a version-dependent position;
## candidates are validated before use). The black level is measured on the masked border; the
## white level is the clipping point found in the data. The CFA row phase is measured from the
## data (`measuredRedRow`), falling back on a table of Canon model IDs whose data starts with a
## green/blue row.

import std/[options, strutils]
import ./tiff, ./sensor, ./ljpeg

const
  CanonSensorInfo = 0x00E0'u16
  CanonColorData = 0x4001'u16
  CanonModelId = 0x0010'u16
  GreenFirstModels = [0x8000_0218'u32, 0x8000_0250'u32, 0x8000_0270'u32, 0x8000_0286'u32, 0x8000_0327'u32]
    ## Canon model IDs (MakerNote 0x0010) whose raw data starts with a green/blue row: the EOS 5D
    ## Mark II, 7D, 550D, 600D and 1200D. Only consulted when the measurement is inconclusive.

proc satAdd(a, b: int): int {.inline.} = (if a > high(int) - b: high(int) else: a + b)

proc measuredRedRow*(data: openArray[uint16]; width: int; area: RawRect; black, white: float32): Option[int] =
  ## Which data rows hold red (0 = even, 1 = odd), measured over `area`; none when the two cues
  ## disagree or there is too little unclipped signal. Samples every other row pair; red is in
  ## the even columns.
  var diff, upper, lower: array[2, float64]
  var n: array[2, uint64]
  let (lo, hi) = (black + 1'f32, black + (white - black) * 0.98'f32)
  let xEnd = max(min(satAdd(area.x, area.width), width) - 1, 0)
  let yEnd = max(satAdd(area.y, area.height) - 1, 0)
  template rowOk(yy: int): bool =
    width > 0 and yy >= 0 and yy <= high(int) div width - 1 and (yy + 1) * width <= data.len
  var y = satAdd(area.y, area.y and 1)
  while y < yEnd:
    if not rowOk(y) or not rowOk(y + 1): break
    let (r0, r1) = (y * width, (y + 1) * width)
    var x = area.x
    while x < xEnd:
      if x >= width or x + 1 >= width: break
      let (a, b) = (float32(data[r0 + x]), float32(data[r1 + x + 1]))
      if a < lo or b < lo or a > hi or b > hi:
        inc x
        continue
      let p = x and 1
      diff[p] += float64(abs(a - b))
      upper[p] += float64(a - black)
      lower[p] += float64(b - black)
      n[p] += 1
      inc x
    y = satAdd(y, 4)
  if n[0] < 256 or n[1] < 256: return
  let d = [diff[0] / float64(n[0]), diff[1] / float64(n[1])]
  var rel: array[2, float64]
  for p in 0 .. 1: rel[p] = abs(upper[p] - lower[p]) / max(upper[p] + lower[p], 1.0)
  if d[0] < 0.8 * d[1] and rel[0] < rel[1]: some(1)
  elif d[1] < 0.8 * d[0] and rel[1] < rel[0]: some(0)
  else: none(int)

proc makerNote*(t: Tiff; ifds: openArray[Ifd]): Option[Ifd] =
  ## The Canon maker note IFD, if present.
  for i in ifds:
    let e = i.get(TagMakerNote)
    if e.isSome: return t.ifdAt(e.get.at, 0)

proc wordAt(t: Tiff; e: Entry; bytes, off: int): Option[float64] =
  let at = off * 2 + e.at
  if at + 2 <= e.at + bytes:
    let v = t.u16At(at)
    if v.isSome: result = some(float64(v.get))

proc asShotWb(t: Tiff; mn: Ifd): Option[array[3, float64]] =
  ## As-shot white-balance multipliers (R, G, B) from ColorData.
  let e0 = mn.get(CanonColorData)
  if e0.isNone: return
  let e = e0.get
  # The entry is UNDEFINED: its bytes are a table of 16-bit words (file byte order), and the
  # offsets below count words, not bytes. Reads stay inside the entry's own byte count.
  let bytes = e.unit * int(e.count)
  template word(off: int): Option[float64] = wordAt(t, e, bytes, off)
  # Word offsets of WB_RGGBLevelsAsShot used by the ColorData versions.
  for off in [0x3F, 0x47, 0x19, 0x22]:
    let (r, g1, g2, b) = (word(off), word(off + 1), word(off + 2), word(off + 3))
    if r.isNone or g1.isNone or g2.isNone or b.isNone: continue
    let g = (g1.get + g2.get) / 2.0
    let (rg, bg) = (r.get / g, b.get / g)
    let plausible = g >= 256.0 and g < 16384.0 and abs(g1.get - g2.get) <= g * 0.05 and rg >= 0.25 and rg < 8.0 and
      bg >= 0.25 and bg < 8.0
    if plausible: return some([rg, 1.0, bg])

proc clipLevel*(data: openArray[uint16]; precision: int): float32 =
  ## The white (clipping) level: the largest value when a meaningful share of samples piles up at
  ## it, else the full range of the declared precision.
  let full = float32((1'u32 shl min(precision, 16)) - 1)
  let step = max(data.len div 2_000_000, 1)
  var mx = 0'u16
  var i = 0
  while i < data.len:
    mx = max(mx, data[i])
    i += step
  if mx == 0: return full
  var near = 0
  i = 0
  while i < data.len:
    if uint32(data[i]) + uint32(mx div 256) >= uint32(mx): inc near
    i += step
  let sampled = (data.len + step - 1) div step
  # At least 0.05 % of samples at the top: that is the clipping point. Back off slightly so the
  # clipped plateau maps to white.
  if near * 2000 >= sampled: float32(mx) * 0.995'f32 else: full

proc maskedBlack*(data: openArray[uint16]; width: int; area: RawRect): Option[array[4, float32]] =
  ## Mean of a region per 2×2 position.
  if area.isEmpty or area.width < 2 or area.height < 2: return
  var sum, n: array[4, uint64]
  for y in area.y ..< area.y + area.height:
    for x in area.x ..< area.x + area.width:
      let i = y * width + x
      if i < 0 or i >= data.len: return
      let k = (y and 1) * 2 + (x and 1)
      sum[k] += uint64(data[i])
      n[k] += 1
  for k in 0 .. 3:
    if n[k] == 0: return
  some([float32(sum[0]) / float32(n[0]), float32(sum[1]) / float32(n[1]), float32(sum[2]) / float32(n[2]),
        float32(sum[3]) / float32(n[3])])

proc realignBlack*(b: BlackLevels; active: RawRect): BlackLevels =
  ## Black levels measured per data-parity 2×2 position, re-indexed from the active origin.
  if b.rows != 2 or b.cols != 2 or b.values.len != 4: return b
  result = b
  for k in 0 .. 3:
    let (ax, ay) = (k and 1, k shr 1)
    let (x, y) = ((active.x + ax) and 1, (active.y + ay) and 1)
    result.values[k] = b.values[y * 2 + x]

proc decodeCr2*(t: Tiff; limits: RawLimits): Sensor =
  let ifds = t.allIfds
  let ifd0 = t.ifdAt(t.firstIfd, 0)
  if ifd0.isNone: raise malformed("CR2 has no IFD0")
  let rawOff = int(t.u32At(12).get(0))
  let raw0 = t.ifdAt(rawOff, 0)
  if raw0.isNone: raise malformed("CR2 raw IFD is missing")
  let raw = raw0.get
  let off0 = t.tagUint(raw, TagStripOffsets)
  if off0.isNone: raise malformed("CR2 raw data offset missing")
  let off = int(off0.get)
  let l0 = t.tagUint(raw, TagStripByteCounts)
  let len = if l0.isSome: int(l0.get) else: max(t.data.len - off, 0)
  var srcLen: int
  if t.inRange(off, len): srcLen = len
  elif off <= t.data.len: srcLen = t.data.len - off
  else: raise malformed("CR2 raw data lies outside the file")
  template src: untyped = t.data.toOpenArray(off, off + srcLen - 1)
  var frame: LjFrame
  try: frame = ljFrame(src)
  except RawError as e:
    if e.kind == reUnsupported and "subsampled" in e.detail: raise unsupported("Canon sRAW / mRAW")
    raise
  let total = frame.samples
  let slices = t.tagUints(raw, TagCr2Slice)
  var width: int
  var widths: seq[int]
  if slices.len == 3 and slices[0] < 64 and slices[1] > 0 and slices[2] > 0:
    let (n, w, last) = (int(slices[0]), int(slices[1]), int(slices[2]))
    for _ in 0 ..< n: widths.add w
    widths.add last
    width = n * w + last
  else:
    width = frame.width * frame.components
    widths = @[width]
  if width == 0 or total mod width != 0: raise malformed("CR2 slices do not match the JPEG frame")
  let height = total div width
  limits.check(uint64(width), uint64(height), 2)
  let (_, samples) = decodeLj(src, total)
  # De-slice.
  var data = newSeq[uint16](width * height)
  var pos = 0
  var x0 = 0
  for sw in widths:
    for y in 0 ..< height:
      if pos + sw > samples.len: raise malformed("CR2 slice data is short")
      for k in 0 ..< sw: data[y * width + x0 + k] = samples[pos + k]
      pos += sw
    x0 += sw
  var warnings: seq[string]
  let mn = makerNote(t, ifds)
  let full = rawRect(0, 0, width, height)
  # SensorInfo: [1] width, [2] height, [5] left, [6] top, [7] right, [8] bottom border (inclusive).
  let info = if mn.isSome: t.tagUints(mn.get, CanonSensorInfo) else: @[]
  var active = full
  if info.len >= 9:
    let (l, tp, r, b) = (info[5], info[6], info[7], info[8])
    if r > l and b > tp: active = rawRect(int(l), int(tp), int(r - l + 1), int(b - tp + 1)).intersect(full)
  if active.isEmpty: active = full
  # Black: the masked columns left of the image (skipping a few edge columns). Only the outer
  # half: the columns next to the image area can be partly exposed.
  let blackM = if active.x >= 16: maskedBlack(data, width, rawRect(4, active.y, active.x div 2 - 4, active.height))
               else: none(array[4, float32])
  var black: BlackLevels
  if blackM.isSome: black = BlackLevels(rows: 2, cols: 2, values: @(blackM.get))
  else:
    warnings.add "no masked sensor area; black level assumed to be 0"
    black = uniformBlack(0)
  let white = clipLevel(data, frame.precision)
  # Red is in the even data columns; the red rows are measured.
  var sumB = -0'f32
  for v in black.values: sumB += v
  let meanBlack = sumB / float32(max(black.values.len, 1))
  var redRow: int
  let m = measuredRedRow(data, width, active, meanBlack, white)
  if m.isSome: redRow = m.get
  else:
    let id = if mn.isSome: t.tagUint(mn.get, CanonModelId) else: none(uint32)
    let known = id.isSome and id.get in GreenFirstModels
    if not known: warnings.add "CR2 colour filter phase could not be measured; assumed red/green in the first row"
    redRow = ord(known)
  let colors = if redRow == 0: @[0'u8, 1, 1, 2] else: @[1'u8, 2, 0, 1]
  let cameraWb = if mn.isSome: asShotWb(t, mn.get) else: none(array[3, float64])
  # The black-level values index from the active area origin, but the masked measurement used
  # data parity: realign them to active-area parity.
  let measured = black
  black = realignBlack(measured, active)
  Sensor(format: rfCr2, make: t.tagAscii(ifd0.get, TagMake), model: t.tagAscii(ifd0.get, TagModel),
         width: width, height: height, samples: 1, data: data,
         cfa: some(Cfa(width: 2, height: 2, colors: colors)), black: black, white: [white, white, white],
         active: active, crop: active, cameraWb: cameraWb,
         orientation: orientation(t.tagUint(ifd0.get, TagOrientation)), warnings: warnings)
