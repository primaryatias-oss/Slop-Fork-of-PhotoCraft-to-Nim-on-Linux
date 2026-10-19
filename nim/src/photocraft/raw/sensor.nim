## Undeveloped sensor data and the shared strip/tile reader (port of `raw/src/sensor.rs`).

import std/[options, strutils]
import ../zlib
import ../codecs/image as cimage
import ../codecs/jpeg as cjpeg
import ./tiff, ./color, ./ljpeg, ./opcodes

type
  RawFormat* = enum
    ## Camera raw container families.
    rfDng = "DNG", rfCr2 = "CR2", rfCr3 = "CR3", rfNef = "NEF", rfArw = "ARW", rfPef = "PEF",
    rfOrf = "ORF", rfRw2 = "RW2", rfRaf = "RAF", rfTiffEp = "TIFF/EP raw"

  RawRect* = object
    ## A rectangle in sensor-data pixels (`photocraft_raw::Rect`).
    x*, y*, width*, height*: int

  Cfa* = object
    ## Colour of one CFA cell: 0 = red, 1 = green, 2 = blue.
    width*, height*: int
    colors*: seq[uint8]      ## row-major colours, `width * height` entries
    originX*, originY*: int  ## data coordinates of the pattern's top-left cell

  BlackLevels* = object
    ## Black levels (DNG BlackLevel / BlackLevelRepeatDim / BlackLevelDeltaH/V).
    rows*, cols*: int
    values*: seq[float32] ## `rows * cols * samples` values, sample fastest
    deltaH*: seq[float32] ## per column of the active area
    deltaV*: seq[float32] ## per row of the active area

  Sensor* = object
    ## Undeveloped sensor data with everything needed to develop it.
    format*: RawFormat
    make*, model*: Option[string]
    width*, height*: int        ## data size in pixels
    samples*: int               ## 1 for CFA data, 3 for LinearRaw (1 for monochrome LinearRaw)
    data*: seq[uint16]          ## row-major interleaved samples, `width * height * samples`
    cfa*: Option[Cfa]           ## the CFA pattern (CFA data only)
    linearization*: Option[seq[uint16]] ## maps stored values to linear values
    black*: BlackLevels
    white*: array[3, float32]   ## white (saturation) level per sample
    active*: RawRect            ## sensor area holding image data (DNG ActiveArea)
    crop*: RawRect              ## the area to output (ActiveArea ∩ DefaultCrop)
    color*: ColorInfo
    cameraWb*: Option[array[3, float64]] ## white-balance multipliers from the camera (non-DNG)
    orientation*: uint16        ## TIFF orientation (1–8)
    baselineExposure*: float64  ## EV baked into the default rendering (DNG BaselineExposure)
    gainMaps*: seq[GainMap]     ## DNG OpcodeList2 gain maps (lens shading)
    toneCurve*: seq[array[2, float32]]
      ## The camera profile's default tone curve: increasing `[input, output]` pairs in linear
      ## ProPhoto, applied per channel after the colour conversion and exposure; empty: none.
    warnings*: seq[string]

  Plane* = object
    ## Raw samples read from one IFD.
    width*, height*, samples*: int
    bits*: int
    data*: seq[uint16]

  Segment* = object
    x*, y*: int
    w*, h*: int          ## encoded width and height (tile size, or image width × rows per strip)
    offset*, len*: int

  JpegLayout* = enum
    ## How a lossless-JPEG tile's samples map onto the tile.
    jlFlat  ## the decoded sample sequence fills the tile row by row (DNG)
    jlQuads ## a 4-component frame of half the tile's size: each JPEG pixel holds a 2×2 block

const JpegXl = 52546'u32 ## DNG 1.7 JPEG XL compression

proc rawRect*(x, y, width, height: int): RawRect = RawRect(x: x, y: y, width: width, height: height)

proc satAdd(a, b: int): int {.inline.} = (if a > high(int) - b: high(int) else: a + b)

proc intersect*(a, o: RawRect): RawRect =
  ## The overlap of two rectangles (may be empty).
  let x0 = max(a.x, o.x)
  let y0 = max(a.y, o.y)
  let x1 = min(satAdd(a.x, a.width), satAdd(o.x, o.width))
  let y1 = min(satAdd(a.y, a.height), satAdd(o.y, o.height))
  rawRect(x0, y0, max(x1 - x0, 0), max(y1 - y0, 0))

proc isEmpty*(r: RawRect): bool = r.width == 0 or r.height == 0

proc bayer*(pattern: string): Option[Cfa] =
  ## A 2×2 pattern from a string such as `"RGGB"`, anchored at (0, 0).
  var colors: seq[uint8]
  for c in pattern:
    case c
    of 'R': colors.add 0
    of 'G': colors.add 1
    of 'B': colors.add 2
    else: return none(Cfa)
  if colors.len == 4: result = some(Cfa(width: 2, height: 2, colors: colors))

proc color*(c: Cfa; x, y: int): uint8 {.inline.} =
  ## Colour at data coordinates.
  if c.width <= 0 or c.height <= 0: return 1
  let cx = (x + c.width - c.originX mod c.width) mod c.width
  let cy = (y + c.height - c.originY mod c.height) mod c.height
  let i = cy * c.width + cx
  if i >= 0 and i < c.colors.len: c.colors[i] else: 1'u8

proc phase*(c: Cfa; x0, y0: int): array[4, uint8] =
  ## The 2×2 colours seen from data coordinates (`x0`, `y0`), row-major.
  [c.color(x0, y0), c.color(x0 + 1, y0), c.color(x0, y0 + 1), c.color(x0 + 1, y0 + 1)]

proc isBayer*(c: Cfa): bool =
  ## Whether the pattern is Bayer: a 2×2 period holding one red, one blue and two diagonal greens.
  if c.width == 0 or c.height == 0 or c.width > 16 or c.height > 16: return false
  let p = c.phase(c.originX, c.originY)
  var periodic = true
  for y in 0 ..< c.height:
    for x in 0 ..< c.width:
      if c.color(c.originX + x, c.originY + y) != p[(y mod 2) * 2 + x mod 2]: periodic = false
  let greensOnDiagonal =
    (p[0] == 1 and p[3] == 1 and p[1] != 1 and p[2] != 1 and int(p[1]) + int(p[2]) == 2) or
    (p[1] == 1 and p[2] == 1 and p[0] != 1 and p[3] != 1 and int(p[0]) + int(p[3]) == 2)
  periodic and greensOnDiagonal

proc uniformBlack*(v: float32): BlackLevels = BlackLevels(rows: 1, cols: 1, values: @[v])

proc at*(b: BlackLevels; ax, ay, s, spp: int): float32 {.inline.} =
  ## Black level at active-area coordinates for sample `s` of `spp`.
  let r = ay mod max(b.rows, 1)
  let c = ax mod max(b.cols, 1)
  let i = (r * max(b.cols, 1) + c) * spp + s
  let base = if i >= 0 and i < b.values.len: b.values[i] elif b.values.len > 0: b.values[0] else: 0'f32
  base + (if ax < b.deltaH.len: b.deltaH[ax] else: 0'f32) + (if ay < b.deltaV.len: b.deltaV[ay] else: 0'f32)

proc isUniform*(b: BlackLevels): bool =
  ## Whether there is a single black value for everything.
  for v in b.deltaH:
    if v != 0: return false
  for v in b.deltaV:
    if v != 0: return false
  for i in 1 ..< b.values.len:
    if b.values[i - 1] != b.values[i]: return false
  true

proc isMonochrome*(s: Sensor): bool =
  ## One-sample LinearRaw: develops to neutral grey with no white balance or colour calibration.
  s.samples == 1 and s.cfa.isNone

proc divCeil(a, b: int): int {.inline.} = (a + b - 1) div b

proc segments*(t: Tiff; ifd: Ifd; width, height: int): seq[Segment] =
  let tw0 = t.tagUint(ifd, TagTileWidth)
  let th0 = t.tagUint(ifd, TagTileLength)
  if tw0.isSome and th0.isSome:
    let (tw, th) = (int(tw0.get), int(th0.get))
    if tw == 0 or th == 0 or tw > (1 shl 16) or th > (1 shl 16): raise malformed("bad tile size")
    let across = divCeil(width, tw)
    let down = divCeil(height, th)
    let offsets = t.tagUints(ifd, TagTileOffsets)
    let counts = t.tagUints(ifd, TagTileByteCounts)
    let n = across * down
    if offsets.len < n or counts.len < n: raise malformed("missing tile offsets")
    for i in 0 ..< n:
      result.add Segment(x: (i mod across) * tw, y: (i div across) * th, w: tw, h: th,
                         offset: int(offsets[i]), len: int(counts[i]))
  else:
    let r = t.tagUint(ifd, TagRowsPerStrip)
    let rps = if r.isSome and r.get > 0: min(int(r.get), height) else: height
    let n = divCeil(height, max(rps, 1))
    let offsets = t.tagUints(ifd, TagStripOffsets)
    var counts = t.tagUints(ifd, TagStripByteCounts)
    if offsets.len < n: raise malformed("missing strip offsets")
    if counts.len < n:
      if n == 1: counts = @[uint32(max(t.data.len - int(offsets[0]), 0) and 0xFFFF_FFFF)]
      else: raise malformed("missing strip byte counts")
    for i in 0 ..< n:
      result.add Segment(x: 0, y: i * rps, w: width, h: rps, offset: int(offsets[i]), len: int(counts[i]))

proc undelta(v: seq[uint16]; plane, rowSamples, rows: int; bits: int): seq[uint16] =
  ## Reverses TIFF predictor 2 (horizontal differencing) on interleaved samples: each sample
  ## gains its channel's predecessor in the row, wrapping at the sample's container.
  let mask = if bits <= 8: 0xFF'u16 else: 0xFFFF'u16
  result = v
  let rs = max(rowSamples, 1)
  var r = 0
  var start = 0
  while start < result.len and r < rows:
    let stop = min(start + rs, result.len)
    for i in start + plane ..< stop: result[i] = (result[i] + result[i - plane]) and mask
    start = stop
    inc r

proc unpack*(src: openArray[byte]; w, rows, samples, bits: int; le: bool): seq[uint16] =
  ## Unpacks uncompressed samples: 8-bit, 16-bit in the file's byte order, or MSB-first
  ## bit-packed rows padded to a byte (TIFF 6.0). 9–15-bit data stored in 16-bit containers is
  ## recognised by its byte count.
  let n = w * rows * samples
  if bits == 16 or (bits > 8 and src.len >= n * 2):
    if src.len < n * 2: raise malformed("uncompressed raw data is truncated")
    result = newSeq[uint16](n)
    for i in 0 ..< n:
      let (a, b) = (uint16(src[2 * i]), uint16(src[2 * i + 1]))
      result[i] = if le: a or (b shl 8) else: (a shl 8) or b
    return
  if bits == 8:
    if src.len < n: raise malformed("uncompressed raw data is truncated")
    result = newSeq[uint16](n)
    for i in 0 ..< n: result[i] = uint16(src[i])
    return
  let rowBytes = divCeil(w * samples * bits, 8)
  if src.len < rowBytes * rows: raise malformed("uncompressed raw data is truncated")
  result = newSeqOfCap[uint16](n)
  let maskBits = (1'u32 shl bits) - 1
  for r in 0 ..< rows:
    var p = r * rowBytes
    let stop = (r + 1) * rowBytes
    var acc = 0'u32
    var have = 0
    for _ in 0 ..< w * samples:
      while have < bits:
        acc = (acc shl 8) or (if p < stop: uint32(src[p]) else: 0'u32)
        if p < stop: inc p
        have += 8
      have -= bits
      result.add uint16((acc shr have) and maskBits)

proc decodeSegment(t: Tiff; ifd: Ifd; s: Segment; height, samples, bits: int; compression: uint32;
                   limits: RawLimits; layout: JpegLayout): seq[uint16] =
  let rows = min(s.h, height - s.y)
  if not t.inRange(s.offset, s.len): raise malformed("raw data lies outside the file")
  template src: untyped = t.data.toOpenArray(s.offset, s.offset + s.len - 1)
  # Every branch below sizes at least one buffer from the segment's own geometry, which the file
  # declares independently of the image: charge the segment itself before decoding it.
  limits.check(uint64(s.w), uint64(s.h), uint64(samples) * 2)
  case compression
  of 1: result = unpack(src, s.w, rows, samples, bits, t.le)
  of 8:
    # Adobe DNG compression 8: zlib streams (RFC 1950). The output is capped at the tile's size
    # in whole bytes per sample; `unpack` then reads it bit-packed or in 16-bit containers.
    let need = s.w * rows * samples * divCeil(bits, 8)
    var outp: seq[byte]
    try: outp = zlibDecompressUpTo(src, need)
    except ZlibError: raise malformed("deflate tile does not decompress")
    let v = unpack(outp, s.w, rows, samples, bits, t.le)
    # The TIFF predictor runs after unpacking, per row, per sample plane.
    let pred = t.tagUint(ifd, TagPredictor).get(1)
    case pred
    of 1: result = v
    of 2: result = undelta(v, samples, s.w * samples, rows, bits)
    of 3: raise unsupported("floating-point predictor (compression 8, predictor 3)")
    else: raise malformed("unknown predictor " & $pred)
  of JpegXl:
    # The Nim port has no JPEG XL decoder (like the Rust browser build).
    raise unsupported("JPEG XL-compressed DNG (DNG 1.7) is not supported by this build")
  of 34892:
    # Adobe DNG compression 34892: baseline JPEG tiles (lossy, 8-bit). A CFA part decodes to
    # gray samples, a 3-sample LinearRaw part holds YCbCr converted to RGB.
    if samples != 1 and samples != 3:
      raise unsupported("lossy DNG with " & $samples & " samples per pixel (1 or 3 are supported)")
    var img: cimage.Image
    try:
      let fr = cjpeg.scanMetadata(src).frame
      if fr.isNone: raise malformed("lossy JPEG tile has no header")
      let (fw, fh, _) = fr.get
      if (fw, fh) != (s.w, rows):
        raise malformed("lossy JPEG tile is " & $fw & "x" & $fh & ", expected " & $s.w & "x" & $rows)
      img = cjpeg.decodeJpeg(src, cimage.defaultLimits())
    except CodecError as e: raise malformed("lossy JPEG tile: " & e.msg)
    let need = s.w * rows * samples
    var bytes: seq[byte]
    if samples == 1 and img.layout == clGray: bytes = img.data
    elif samples == 3 and img.layout == clRgb: bytes = img.data
    elif samples == 1 and img.layout == clRgb:
      bytes = newSeq[byte](img.width * img.height)
      for i in 0 ..< bytes.len:
        let y = 0.299 * float64(img.data[3 * i]) + 0.587 * float64(img.data[3 * i + 1]) + 0.114 * float64(img.data[3 * i + 2])
        bytes[i] = byte(min(max(int(y + 0.5), 0), 255))
    else: bytes = img.data
    if img.sample != isU8 or bytes.len != need:
      raise malformed("lossy JPEG tile holds " & $bytes.len & " samples, expected " & $need)
    # The 8-bit samples widen to the document's 16-bit range (0xFF -> 0xFFFF).
    result = newSeq[uint16](need)
    for i, b in bytes: result[i] = uint16(b) * 257
  else:
    # Compression 7 (lossless JPEG): the frame the entropy stream declares is capped by the
    # budget, not only by the segment, and the `jlQuads` scatter buffer comes out of the same
    # budget. `maxAlloc` counts bytes, a sample is two.
    let need = s.w * rows * samples
    let scatter = if layout == jlQuads and samples == 1: uint64(s.w) * uint64(s.h) * 2 else: 0'u64
    let budget = int(min((if limits.maxAlloc > scatter: limits.maxAlloc - scatter else: 0'u64) div 2, uint64(high(int))))
    let geom = s.w * s.h * samples * 4
    let (f, v) = decodeLj(src, min(geom, budget))
    if v.len < need: raise malformed("lossless JPEG tile is smaller than its tile")
    if layout == jlQuads and samples == 1 and f.components == 4 and f.width * 2 == s.w and f.height * 2 == s.h:
      result = newSeq[uint16](s.w * s.h)
      for i in 0 ..< v.len div 4:
        let at = 2 * (i div f.width) * s.w + 2 * (i mod f.width)
        result[at] = v[4 * i]
        result[at + 1] = v[4 * i + 1]
        result[at + s.w] = v[4 * i + 2]
        result[at + s.w + 1] = v[4 * i + 3]
      return
    result = v

proc readPlane*(t: Tiff; ifd: Ifd; limits: RawLimits; layout: JpegLayout): Plane =
  ## Reads the (uncompressed, deflate, lossless-JPEG or lossy-JPEG) image of a raw IFD.
  let w0 = t.tagUint(ifd, TagImageWidth)
  if w0.isNone: raise malformed("raw image has no width")
  let h0 = t.tagUint(ifd, TagImageLength)
  if h0.isNone: raise malformed("raw image has no height")
  let (width, height) = (int(w0.get), int(h0.get))
  let samples = int(t.tagUint(ifd, TagSamplesPerPixel).get(1))
  let bits = int(t.tagUint(ifd, TagBitsPerSample).get(1))
  let compression = t.tagUint(ifd, TagCompression).get(1)
  if samples notin 1 .. 4: raise unsupported($samples & " samples per pixel")
  if t.tagUint(ifd, TagSampleFormat) == some(3'u32): raise unsupported("floating-point raw data")
  if bits notin 1 .. 16: raise unsupported($bits & "-bit raw samples")
  if samples > 1 and t.tagUint(ifd, TagPlanarConfiguration) == some(2'u32): raise unsupported("planar raw data")
  case compression
  of 1, 7, 8, 34892, JpegXl: discard
  of 34713: raise unsupported("Nikon compressed NEF")
  of 32767: raise unsupported("Sony compressed ARW")
  of 65535: raise unsupported("Pentax compressed PEF")
  else: raise unsupported("raw compression scheme " & $compression)
  limits.check(uint64(width), uint64(height), uint64(samples) * 2)
  let segs = segments(t, ifd, width, height)
  # Segments decode in order (the Rust crate decodes them in parallel and reports the first
  # failing segment's error, the same one).
  var decoded: seq[seq[uint16]]
  for s in segs: decoded.add decodeSegment(t, ifd, s, height, samples, bits, compression, limits, layout)
  var data = newSeq[uint16](width * height * samples)
  for k, s in segs:
    let d = decoded[k]
    let rows = min(s.h, height - s.y)
    let cols = min(s.w, width - s.x)
    for r in 0 ..< rows:
      let src = r * s.w * samples
      let at = ((s.y + r) * width + s.x) * samples
      for i in 0 ..< cols * samples: data[at + i] = d[src + i]
  Plane(width: width, height: height, samples: samples, bits: bits, data: data)
