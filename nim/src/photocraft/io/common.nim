## Shared io types: errors, import/export results and options (Rust `photocraft_io` lib.rs).

import std/[options, strutils, math]
import ../doc, ../codecs/image, ../textutil
export lossyUtf8

type
  IoErrorKind* = enum
    iePdn = "PDN", ieOra = "OpenRaster", iePsd = "PSD", ieCodec = "codec",
    ieUnknownFormat = "unknown file format", ieUnsupported = "unsupported", iePcraft = "pcraft",
    ieRaw = "raw", ieSvg = "SVG", ieCancelled = "cancelled"
  IoFailure* = object of CatchableError
    kind*: IoErrorKind

  ImportResult* = object
    document*: Document
    warnings*: seq[string]
    sourceReadOnly*, previewOnly*: bool

  ExportResult* = object
    bytes*: seq[byte]
    warnings*: seq[string]

  XmpEmbed* = enum xeAll, xeNone
  TgaBits* = enum tb24 = 24, tb32 = 32

  ExportOptions* = object
    encode*: EncodeOptions
    forcePsb*, tiffLayers*: bool
    xmp*: XmpEmbed
    tgaBits*: Option[TgaBits]
    imagePreviews*: bool

proc ioError*(kind: IoErrorKind; msg = ""): ref IoFailure =
  ## Builds an error whose message matches the Rust `Display` (`"PSD: …"`, `"unsupported: …"`).
  let text =
    case kind
    of ieCancelled: "cancelled"
    of ieRaw: msg
    else: $kind & ": " & msg
  (ref IoFailure)(kind: kind, msg: text)

proc defaultExportOptions*(): ExportOptions =
  ExportOptions(encode: defaultEncodeOptions(), xmp: xeAll, imagePreviews: true)

proc tgaBitsFrom*(bits: uint64): Option[TgaBits] =
  case bits
  of 24: some(tb24)
  of 32: some(tb32)
  else: none(TgaBits)

proc bits*(b: TgaBits): uint8 = uint8(ord(b))

proc fmt2(v: float64): string = formatFloat(v, ffDecimal, 2)

proc unequalResolutionWarning*(x, y: float64): Option[string] =
  if abs(x - y) >= 0.01:
    some("the vertical resolution (" & fmt2(y) & " ppi) differs from the horizontal (" & fmt2(x) &
         " ppi); the document keeps " & fmt2(x) & " ppi for both")
  else: none(string)

proc hasExtension*(name, expected: string): bool =
  ## True when the file name (after any path) has a non-empty base and the given extension.
  var base = name
  let cut = max(base.rfind('/'), base.rfind('\\'))
  if cut >= 0: base = base[cut + 1 .. ^1]
  let dot = base.rfind('.')
  dot > 0 and cmpIgnoreCase(base[dot + 1 .. ^1], expected) == 0

proc extension*(nameOrExt: string): string =
  ## Lower-cased last component after `.`, `/` or `\` (the whole string when there is none).
  let cut = max(nameOrExt.rfind('.'), max(nameOrExt.rfind('/'), nameOrExt.rfind('\\')))
  (if cut >= 0: nameOrExt[cut + 1 .. ^1] else: nameOrExt).toLowerAscii


proc plainDecimal(s: string): string =
  ## `1.5e-07` → `0.00000015`, `1e+20` → `100000000000000000000` (Rust prints floats this way).
  let e = s.find({'e', 'E'})
  if e < 0: return s
  var mant = s[0 ..< e]
  let exp = parseInt(s[e + 1 .. ^1])
  var neg = false
  if mant.startsWith("-"):
    neg = true
    mant = mant[1 .. ^1]
  let dot = mant.find('.')
  var digits = mant.replace(".", "")
  var point = (if dot < 0: mant.len else: dot) + exp
  while digits.len > 1 and digits[^1] == '0' and digits.len > point: digits.setLen(digits.len - 1)
  if point <= 0: result = "0." & repeat('0', -point) & digits
  elif point >= digits.len: result = digits & repeat('0', point - digits.len)
  else: result = digits[0 ..< point] & "." & digits[point .. ^1]
  if neg: result = "-" & result

proc rustFloat*(v: float32): string =
  ## Rust's `Display` for `f32`: shortest round-trip digits, no exponent, no `.0` on integers.
  if v != v: return "NaN"
  if v == Inf: return "inf"
  if v == NegInf: return "-inf"
  result = plainDecimal($v)
  if result.endsWith(".0"): result.setLen(result.len - 2)

proc rustFloat*(v: float64): string =
  ## Rust's `Display` for `f64`.
  if v != v: return "NaN"
  if v == Inf: return "inf"
  if v == NegInf: return "-inf"
  result = plainDecimal($v)
  if result.endsWith(".0"): result.setLen(result.len - 2)
