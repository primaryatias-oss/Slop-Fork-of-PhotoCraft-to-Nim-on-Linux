## `photocraft/raw`: a clean-room camera raw decoder and developer (port of the Rust
## `photocraft-raw` crate).
##
## Implemented only from public specifications and papers: TIFF 6.0, TIFF/EP (ISO 12234-2), the
## Adobe DNG Specification 1.7, ITU-T T.81 (lossless JPEG, process 14), the published structure of
## Canon's CR2 container, published descriptions of Sony's cRAW code, public maker-note and RAF tag
## tables, Nikon Huffman tables recovered by black-box analysis of CC0 samples, observation of
## sample files, and the demosaicing papers cited in `raw/demosaic`.
##
## * `identify` / `isRaw` recognise raw files from their bytes.
## * `decode` reads the undeveloped `Sensor` data and metadata.
## * `develop` turns it into 16-bit RGB in ProPhoto RGB (ROMM primaries, D50 white, gamma 1.8).
## * `embeddedPreview` finds the camera's full-size JPEG preview; `embeddedPreviews` lists every
##   preview.
## * `fitLook` fits a per-file colour matrix and tone curve (`CameraLook`) to the camera's own
##   (decoded) JPEG; pass it in `DevelopOptions.cameraLook`.
##
## Decoded: DNG (uncompressed, deflate and lossless-JPEG, lossy-JPEG tiles; strips and tiles, CFA
## and LinearRaw), CR2, uncompressed or lossless-JPEG TIFF/EP raws, Nikon compressed NEF, Sony
## compressed ARW (cRAW), Panasonic RW2, uncompressed Olympus ORF and uncompressed Fujifilm RAF
## (Bayer and X-Trans). JPEG XL-compressed DNG tiles (DNG 1.7) and everything else raise
## `RawError` with kind `reUnsupported`. The module does no I/O and never panics on hostile input:
## sizes are checked against `RawLimits` before allocating.

import std/[options, strutils]
import ./raw/[tiff, color, sensor, opcodes, ljpeg, demosaic, tone, develop, look, preview, dng, cr2,
              tiffep, rw2, orf, raf]

export tiff.RawError, tiff.RawErrorKind, tiff.RawLimits, tiff.defaultRawLimits, tiff.check
export color.Mat3, color.Calibration, color.ColorInfo
export sensor
export opcodes.GainMap
export demosaic.Demosaic, demosaic.DefaultDemosaic, demosaic.id, demosaic.demosaicFromId
export tone
export develop.WbKind, develop.WhiteBalance, develop.DevelopOptions, develop.RawInfo, develop.Developed,
  develop.asShotWb, develop.autoWb, develop.multipliersWb, develop.defaultDevelopOptions, develop.developSensor
export look.Reference, look.LookRejected, look.ProxyEdge, look.fitLook
export preview.Preview, preview.embeddedPreview, preview.embeddedPreviews

const
  OutputSpace* = "ProPhoto RGB (ROMM primaries, D50, gamma 1.8)" ## the output colour space of `develop`
  Extensions* = ["dng", "cr2", "cr3", "nef", "nrw", "arw", "srf", "sr2", "pef", "orf", "rw2", "raf"]
    ## file extensions of the raw formats this module recognises

proc name*(f: RawFormat): string = $f

proc makeFormat(make: Option[string]): RawFormat =
  let m = make.get("").toUpperAscii
  if m.startsWith("NIKON"): rfNef
  elif m.startsWith("SONY"): rfArw
  elif m.startsWith("PENTAX") or m.startsWith("RICOH"): rfPef
  elif m.startsWith("OLYMPUS") or m.startsWith("OM DIGITAL"): rfOrf
  else: rfTiffEp

proc bytesAre(b: openArray[byte]; at: int; s: string): bool =
  if at < 0 or at + s.len > b.len: return false
  for i, c in s:
    if b[at + i] != byte(c): return false
  true

proc identify*(bytes: openArray[byte]): Option[RawFormat] =
  ## Recognises a camera raw file from its bytes (cheap: header and IFDs only).
  if bytesAre(bytes, 4, "ftypcrx "): return some(rfCr3)
  if raf.hasMagic(bytes): return some(rfRaf)
  let tt = newTiff(bytes)
  if tt.isNone: return
  let t = tt.get
  if bytesAre(bytes, 0, "IIRO") or bytesAre(bytes, 0, "IIRS") or bytesAre(bytes, 0, "MMOR"): return some(rfOrf)
  if bytesAre(bytes, 0, "IIU\0"): return some(rfRw2)
  if bytesAre(bytes, 8, "CR\x02"): return some(rfCr2)
  let ifds = t.allIfds
  for i in ifds:
    if i.has(TagDngVersion): return some(rfDng)
  for i in ifds:
    let p = t.tagUint(i, TagPhotometric)
    if p == some(PhotometricCfa) or p == some(PhotometricLinearRaw):
      var make = none(string)
      for j in ifds:
        make = t.tagAscii(j, TagMake)
        if make.isSome: break
      return some(makeFormat(make))
  if isSonyNonCfa(t, ifds): return some(rfArw)

proc isRaw*(bytes: openArray[byte]): bool =
  ## `true` if `bytes` look like a camera raw file.
  identify(bytes).isSome

proc decode*(bytes: openArray[byte]; limits = defaultRawLimits()): Sensor =
  ## Reads the undeveloped sensor data and metadata. Raises `RawError`.
  let format = identify(bytes)
  if format.isNone: raise notRaw()
  case format.get
  of rfCr3: raise unsupported("Canon CR3 (ISO BMFF / CRX) is not decoded yet")
  of rfRaf: decodeRaf(bytes, limits)
  else:
    let tt = newTiff(bytes)
    if tt.isNone: raise notRaw()
    let t = tt.get
    case format.get
    of rfDng: decodeDng(t, limits)
    of rfCr2: decodeCr2(t, limits)
    of rfRw2: decodeRw2(t, limits)
    of rfOrf: decodeOrf(t, limits)
    else: decodeTiffEp(t, format.get, limits)

proc decodeLosslessJpeg*(bytes: openArray[byte]; maxSamples: int): (int, int, int, seq[uint16]) =
  ## Decodes a lossless JPEG (T.81 process 14) stream into `(width, height, components,
  ## samples)`; at most `maxSamples` samples are allocated. Exposed for fuzzing and tools.
  let (f, v) = decodeLj(bytes, maxSamples)
  (f.width, f.height, f.components, v)

proc dumpStructure*(bytes: openArray[byte]): string =
  ## Lists the TIFF structure of a raw file (debugging aid).
  tiff.dump(bytes)

proc develop*(bytes: openArray[byte]; opts = defaultDevelopOptions()): Developed =
  ## Decodes and develops a raw file. Raises `RawError`.
  developSensor(decode(bytes, opts.limits), opts)
