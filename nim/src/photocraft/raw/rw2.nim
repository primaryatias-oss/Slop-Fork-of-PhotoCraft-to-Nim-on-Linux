## Panasonic / Leica RW2 with the bit-packed sensor layout (port of `raw/src/rw2.rs`; PanasonicRaw
## RawFormat 5, written by recent Lumix bodies).
##
## IFD0 holds Panasonic's own tags (ExifTool's PanasonicRaw table): SensorWidth/Height (2, 3),
## Sensor{Top,Left,Bottom,Right}Border (4–7), CFAPattern (9), BitsPerSample (0x0A),
## LinearityLimit{Red,Green,Blue} (0x0E–0x10), BlackLevel{Red,Green,Blue} (0x1C–0x1E),
## WB{Red,Green,Blue}Level (0x24–0x26), RawFormat (0x2D) and RawDataOffset (0x118).
##
## The packing (observed): from RawDataOffset the data is stored in pages of 0x4000 bytes, each
## split at 0x1FF8 with its second part first; the logical stream is a sequence of 16-byte blocks,
## each a little-endian 128-bit number holding 10 12-bit (or 9 14-bit) pixels from the least
## significant bit up. Older RawFormats store vendor-compressed data, which is not decoded.

import std/options
import ./tiff, ./sensor

const
  SensorWidth = 0x0002'u16
  SensorHeight = 0x0003'u16
  SensorTopBorder = 0x0004'u16
  SensorLeftBorder = 0x0005'u16
  SensorBottomBorder = 0x0006'u16
  SensorRightBorder = 0x0007'u16
  Rw2CfaPattern = 0x0009'u16
  Rw2BitsPerSample = 0x000A'u16
  LinearityLimit = [0x000E'u16, 0x000F, 0x0010]
  Rw2BlackLevel = [0x001C'u16, 0x001D, 0x001E]
  WbLevel = [0x0024'u16, 0x0025, 0x0026]
  Rw2RawFormat = 0x002D'u16
  RawDataOffset = 0x0118'u16
  Page = 0x4000 ## page size of the stored stream (observed)
  Split = 0x1FF8 ## split point within a page

proc physical*(i: int): int {.inline.} =
  ## Physical offset (relative to RawDataOffset) of logical byte `i`.
  let page = i - i mod Page
  page + (i mod Page + Split) mod Page

proc pixelsPerBlock*(bits: int): Option[int] =
  case bits
  of 12: some(10)
  of 14: some(9)
  else: none(int)

proc unpackBlock*(b: openArray[byte]; bits: int; outp: var openArray[uint16]) =
  ## Unpacks one 16-byte block of `bits`-bit pixels into `outp`.
  for i in 0 ..< outp.len:
    let at = bits * i
    if at + bits > 128: break
    var v = 0'u32
    for k in 0 ..< bits:
      let p = at + k
      v = v or (((uint32(b[p shr 3]) shr (p and 7)) and 1) shl k)
    outp[i] = uint16(v)

proc tagLevel(t: Tiff; ifd: Ifd; tg: uint16): Option[float64] =
  let v = t.tagUint(ifd, tg)
  if v.isSome: result = some(float64(v.get))

proc decodeRw2*(t: Tiff; limits: RawLimits): Sensor =
  let ifd0 = t.ifdAt(t.firstIfd, 0)
  if ifd0.isNone: raise malformed("RW2 has no IFD0")
  let ifd = ifd0.get
  let rawFormat = t.tagUint(ifd, Rw2RawFormat)
  if rawFormat != some(5'u32):
    let f = if rawFormat.isSome: $rawFormat.get else: "unknown"
    raise unsupported("Panasonic RW2 raw format " & f & " (vendor-compressed) is not decoded yet")
  let w0 = t.tagUint(ifd, SensorWidth)
  if w0.isNone: raise malformed("RW2 has no sensor width")
  let h0 = t.tagUint(ifd, SensorHeight)
  if h0.isNone: raise malformed("RW2 has no sensor height")
  let (width, height) = (int(w0.get), int(h0.get))
  let bits = int(t.tagUint(ifd, Rw2BitsPerSample).get(12))
  let ppb0 = pixelsPerBlock(bits)
  if ppb0.isNone: raise unsupported("Panasonic RW2 with " & $bits & "-bit samples")
  let ppb = ppb0.get
  if width mod ppb != 0: raise unsupported("Panasonic RW2 " & $width & " pixels wide (not whole " & $ppb & "-pixel blocks)")
  limits.check(uint64(width), uint64(height), 2)
  let pattern = case t.tagUint(ifd, Rw2CfaPattern).get(0)
    of 1: "RGGB"
    of 2: "GRBG"
    of 3: "GBRG"
    of 4: "BGGR"
    else: raise unsupported("Panasonic RW2 without a CFA pattern")
  let cfa = bayer(pattern).get
  var off = t.tagUint(ifd, RawDataOffset)
  if off.isNone: off = t.tagUint(ifd, TagStripOffsets)
  if off.isNone: raise malformed("RW2 has no raw data offset")
  let offset = int(off.get)
  if offset > t.data.len: raise malformed("RW2 raw data lies outside the file")
  let srcLen = t.data.len - offset
  let rowBytes = width div ppb * 16
  let need = rowBytes * height
  # Whole pages are stored; tolerate a short final page (its missing bytes read as 0).
  if srcLen < max(need - Page, 0): raise malformed("RW2 raw data is truncated")
  var data = newSeq[uint16](width * height)
  var b: array[16, byte]
  var px = newSeq[uint16](ppb)
  for r in 0 ..< height:
    let at = r * rowBytes
    for k in 0 ..< width div ppb:
      # SPLIT and PAGE are multiples of 8, so each 8-byte half is contiguous.
      let l = at + k * 16
      for half in 0 .. 1:
        let p = physical(l + 8 * half)
        let ok = p + 8 <= srcLen
        for j in 0 .. 7: b[8 * half + j] = (if ok: t.data[offset + p + j] else: 0'u8)
      unpackBlock(b, bits, px)
      for j in 0 ..< ppb: data[r * width + k * ppb + j] = px[j]
  let full = rawRect(0, 0, width, height)
  template level(tg: uint16): Option[float64] = tagLevel(t, ifd, tg)
  var warnings: seq[string]
  var black: BlackLevels
  let (br, bg, bb) = (level(Rw2BlackLevel[0]), level(Rw2BlackLevel[1]), level(Rw2BlackLevel[2]))
  if br.isSome and bg.isSome and bb.isSome:
    let rgb = [br.get, bg.get, bb.get]
    let ph = cfa.phase(0, 0)
    black = BlackLevels(rows: 2, cols: 2, values: @[float32(rgb[min(int(ph[0]), 2)]), float32(rgb[min(int(ph[1]), 2)]),
                                                     float32(rgb[min(int(ph[2]), 2)]), float32(rgb[min(int(ph[3]), 2)])])
  else:
    warnings.add "RW2: black level not recorded; assumed 0"
    black = uniformBlack(0)
  let mx = float32((1'u32 shl bits) - 1)
  var white: array[3, float32]
  for k in 0 .. 2:
    let v = level(LinearityLimit[k])
    white[k] = if v.isSome and float32(v.get) > 0 and float32(v.get) <= mx: float32(v.get) else: mx
  var cameraWb = none(array[3, float64])
  let (wr, wg, wb) = (level(WbLevel[0]), level(WbLevel[1]), level(WbLevel[2]))
  if wr.isSome and wg.isSome and wb.isSome and wg.get > 0:
    let m = [wr.get / wg.get, 1.0, wb.get / wg.get]
    if m[0] >= 0.25 and m[0] < 8 and m[2] >= 0.25 and m[2] < 8: cameraWb = some(m)
  var crop = full
  let (l, tp, r, bt) = (t.tagUint(ifd, SensorLeftBorder), t.tagUint(ifd, SensorTopBorder),
                        t.tagUint(ifd, SensorRightBorder), t.tagUint(ifd, SensorBottomBorder))
  if l.isSome and tp.isSome and r.isSome and bt.isSome and r.get > l.get and bt.get > tp.get:
    crop = rawRect(int(l.get), int(tp.get), int(r.get - l.get), int(bt.get - tp.get)).intersect(full)
  if crop.isEmpty: crop = full
  Sensor(format: rfRw2, make: t.tagAscii(ifd, TagMake), model: t.tagAscii(ifd, TagModel), width: width,
         height: height, samples: 1, data: data, cfa: some(cfa), black: black, white: white, active: full,
         crop: crop, cameraWb: cameraWb, orientation: orientation(t.tagUint(ifd, TagOrientation)),
         warnings: warnings)
