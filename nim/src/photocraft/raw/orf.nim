## Olympus ORF with uncompressed sensor data (port of `raw/src/orf.rs`; older Four Thirds bodies
## such as the E-1 and E-400).
##
## The sensor data is IFD0's strips, 16-bit samples whose top ValidBits bits are significant. The
## CFA comes from the EXIF CFAPattern. Levels come from the publicly documented Olympus maker-note
## ImageProcessing tags (ExifTool's Olympus table): WB_RBLevels (0x0100), WB_GLevel (0x011F),
## BlackLevel2 (0x0600), ValidBits (0x0611) and CropLeft / CropTop / CropWidth / CropHeight
## (0x0612–0x0615). Later bodies store Olympus' own compressed data, which is not decoded.

import std/options
import ./tiff, ./sensor, ./tiffep, ./cr2

const
  CameraSettings = 0x2020'u16
  PreviewImageStart = 0x0101'u16
  PreviewImageLength = 0x0102'u16
  ImageProcessing = 0x2040'u16
  WbRbLevels = 0x0100'u16
  WbGLevel = 0x011F'u16
  BlackLevel2 = 0x0600'u16
  ValidBits = 0x0611'u16
  CropTags = [0x0612'u16, 0x0613, 0x0614, 0x0615]

proc startsWith(t: Tiff; at: int; s: string): bool =
  if not t.inRange(at, s.len): return false
  for k, c in s:
    if t.data[at + k] != byte(c): return false
  true

proc makerSubIfd(t: Tiff; ifds: openArray[Ifd]; sub: uint16): Option[(Tiff, Ifd, int)] =
  ## A sub-IFD of the Olympus maker note, with the TIFF view its offsets are relative to and that
  ## view's absolute position in the file. New-style notes (`OLYMPUS\0` + byte order) are relative
  ## to the note; old-style ones (`OLYMP\0`) to the file.
  var e = none(Entry)
  for i in ifds:
    let x = i.get(TagMakerNote)
    if x.isSome:
      e = x
      break
  if e.isNone or not t.inRange(e.get.at, 12): return
  let at = e.get.at
  var view: Tiff
  var main: Ifd
  var base: int
  if t.startsWith(at, "OLYMPUS\0"):
    var le: bool
    if t.startsWith(at + 8, "II"): le = true
    elif t.startsWith(at + 8, "MM"): le = false
    else: return
    view = Tiff(data: t.data[at .. ^1], le: le, firstIfd: 12)
    let m = view.ifdAt(12, 0)
    if m.isNone: return
    (main, base) = (m.get, at)
  elif t.startsWith(at, "OLYMP\0"):
    view = t
    let m = t.ifdAt(at + 8, 0)
    if m.isNone: return
    (main, base) = (m.get, 0)
  else: return
  let ip = main.get(sub)
  if ip.isNone: return
  var off: int
  if ip.get.typ in [4'u16, 13]:
    let v = view.uint(ip.get, 0)
    if v.isNone: return
    off = int(v.get)
  else: off = ip.get.at
  let ifd = view.ifdAt(off, 0)
  if ifd.isNone: return
  some((view, ifd.get, base))

proc orfPreviewRange*(t: Tiff): Option[(int, int)] =
  ## Absolute offset and length of the camera's preview JPEG (maker note CameraSettings
  ## PreviewImageStart / PreviewImageLength).
  let s = makerSubIfd(t, t.allIfds, CameraSettings)
  if s.isNone: return
  let (view, cs, base) = s.get
  let start = view.tagUint(cs, PreviewImageStart)
  let len = view.tagUint(cs, PreviewImageLength)
  if start.isNone or len.isNone: return
  some((base + int(start.get), int(len.get)))

proc decodeOrf*(t: Tiff; limits: RawLimits): Sensor =
  let ifds = t.allIfds
  let raw0 = t.ifdAt(t.firstIfd, 0)
  if raw0.isNone: raise malformed("ORF has no IFD0")
  let raw = raw0.get
  let width = uint64(t.tagUint(raw, TagImageWidth).get(0))
  let height = uint64(t.tagUint(raw, TagImageLength).get(0))
  # Every ORF, compressed or not, declares its strip byte counts. Missing or unreadable counts mean
  # IFD0 is cut short, not the compressed variant.
  let counts = t.tagUints(raw, TagStripByteCounts)
  if counts.len == 0: raise malformed("ORF IFD0 has no readable strip byte counts")
  var stored = 0'u64
  for c in counts: stored += uint64(c)
  let need = if width != 0 and height > high(uint64) div width: high(uint64) else: width * height
  let need2 = if need > high(uint64) div 2: high(uint64) else: need * 2
  if t.tagUint(raw, TagCompression).get(1) != 1 or stored < need2:
    raise unsupported("Olympus compressed (or packed) ORF is not decoded")
  let plane = readPlane(t, raw, limits, jlFlat)
  if plane.samples != 1: raise unsupported("ORF with " & $plane.samples & " samples per pixel")
  let cfa = tiffEpCfa(t, raw, ifds)
  if cfa.isNone: raise unsupported("ORF: CFA pattern not recorded")
  if not cfa.get.isBayer: raise unsupported("ORF: non-Bayer CFA")
  let full = rawRect(0, 0, plane.width, plane.height)
  var warnings: seq[string]
  let ip = makerSubIfd(t, ifds, ImageProcessing)
  template floats(tg: uint16): seq[float64] = (if ip.isSome: ip.get[0].tagFloats(ip.get[1], tg) else: @[])
  var black: BlackLevels
  let byPos = rggbByPosition(cfa.get, floats(BlackLevel2))
  if byPos.isSome: black = BlackLevels(rows: 2, cols: 2, values: @(byPos.get))
  else:
    warnings.add "ORF: black level not recorded; assumed 0"
    black = uniformBlack(0)
  var data = plane.data
  var white: float32
  let vb = floats(ValidBits)
  if vb.len > 0 and vb[0] >= 8 and vb[0] <= 16:
    let bits = int(vb[0])
    let top = (1'u32 shl bits) - 1
    # Observed: the E-1 / E-400 store the valid bits left-justified in the 16-bit container.
    var over = false
    for v in data:
      if uint32(v) > top:
        over = true
        break
    if over:
      let shift = 16 - bits
      for v in data.mitems: v = v shr shift
    white = float32(top)
  else: white = clipLevel(data, plane.bits)
  let gl = floats(WbGLevel)
  let g = if gl.len > 0 and gl[0] > 0: gl[0] else: 256.0
  var cameraWb = none(array[3, float64])
  let rb = floats(WbRbLevels)
  if rb.len >= 2:
    let m = [rb[0] / g, 1.0, rb[1] / g]
    if m[0] >= 0.25 and m[0] < 8 and m[2] >= 0.25 and m[2] < 8: cameraWb = some(m)
  var crop = full
  var cv: array[4, Option[float64]]
  for k in 0 .. 3:
    let f = floats(CropTags[k])
    if f.len > 0: cv[k] = some(f[0])
  if cv[0].isSome and cv[1].isSome and cv[2].isSome and cv[3].isSome:
    var ok = true
    for k in 0 .. 3:
      if not (cv[k].get >= 0 and cv[k].get < 1e6): ok = false
    if ok:
      let r = rawRect(int(cv[0].get), int(cv[1].get), int(cv[2].get), int(cv[3].get)).intersect(full)
      crop = if r.isEmpty: full else: r
  var make, model: Option[string]
  for i in ifds:
    if make.isNone: make = t.tagAscii(i, TagMake)
    if model.isNone: model = t.tagAscii(i, TagModel)
  Sensor(format: rfOrf, make: make, model: model, width: plane.width, height: plane.height, samples: 1,
         data: data, cfa: cfa, black: black, white: [white, white, white], active: full, crop: crop,
         cameraWb: cameraWb, orientation: orientation(t.tagUint(raw, TagOrientation)), warnings: warnings)
