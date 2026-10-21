## Units and lengths of the preferences (the `Unit` / `PointSize` part of `prefs.rs`). Part of
## the `engine/core` unit: included, not imported.

type
  Unit* = enum
    ## Length units for rulers, the Info panel and size dialogs.
    unPixels = "pixels", unInches = "inches", unCentimeters = "cm", unMillimeters = "mm",
    unPoints = "points", unPicas = "picas", unPercent = "percent"
  PointSize* = enum psPostScript = "postScript", psTraditional = "traditional"

proc perInch*(p: PointSize): float64 = (if p == psPostScript: 72.0 else: 72.27)

proc fromPx*(u: Unit; px, dpi0, extent, ppi: float64): float64 =
  ## Document pixels → this unit.
  let dpi = rmax(dpi0, 1e-6)
  case u
  of unPixels: px
  of unInches: px / dpi
  of unCentimeters: px / dpi * 2.54
  of unMillimeters: px / dpi * 25.4
  of unPoints: px / dpi * ppi
  of unPicas: px / dpi * ppi / 12.0
  of unPercent: px / rmax(extent, 1e-6) * 100.0

proc toPx*(u: Unit; v, dpi0, extent, ppi: float64): float64 =
  ## This unit → document pixels.
  let dpi = rmax(dpi0, 1e-6)
  case u
  of unPixels: v
  of unInches: v * dpi
  of unCentimeters: v / 2.54 * dpi
  of unMillimeters: v / 25.4 * dpi
  of unPoints: v / rmax(ppi, 1e-6) * dpi
  of unPicas: v * 12.0 / rmax(ppi, 1e-6) * dpi
  of unPercent: v * extent / 100.0

proc unitFromSuffix*(suffix: string): Option[Unit] =
  ## The unit a typed suffix names, in any case.
  case suffix.toLowerAscii
  of "px", "pixel", "pixels": some(unPixels)
  of "in", "inch", "inches", "\"": some(unInches)
  of "cm", "centimeter", "centimeters", "centimetre", "centimetres": some(unCentimeters)
  of "mm", "millimeter", "millimeters", "millimetre", "millimetres": some(unMillimeters)
  of "pt", "point", "points": some(unPoints)
  of "pc", "pica", "picas": some(unPicas)
  of "%", "percent": some(unPercent)
  else: none(Unit)

proc rustParseF64*(s: string): Option[float64] =
  ## Rust's `str::parse::<f64>` for plain decimal numbers (sign, digits, one point, exponent).
  if s.len == 0: return
  var i = 0
  if s[i] in {'+', '-'}: inc i
  var digits = 0
  while i < s.len and s[i] in Digits:
    inc i
    inc digits
  if i < s.len and s[i] == '.':
    inc i
    while i < s.len and s[i] in Digits:
      inc i
      inc digits
  if digits == 0: return
  if i < s.len and s[i] in {'e', 'E'}:
    inc i
    if i < s.len and s[i] in {'+', '-'}: inc i
    var ed = 0
    while i < s.len and s[i] in Digits:
      inc i
      inc ed
    if ed == 0: return
  if i != s.len: return
  try: some(parseFloat(s))
  except ValueError: none(float64)

proc parseLength*(text: string): Option[(float64, Option[Unit])] =
  ## A typed length: a number with an optional unit suffix ("12", "2.5 cm", "50%").
  let t = text.strip
  var e = 0
  while e < t.len and t[e] in Digits + {'.', ',', '-', '+'}: inc e
  let number = t[0 ..< e].strip.replace(',', '.')
  let v = rustParseF64(number)
  if v.isNone or v.get != v.get or abs(v.get) == Inf: return
  let suffix = t[e .. ^1].strip
  var unit = none(Unit)
  if suffix.len > 0:
    unit = unitFromSuffix(suffix)
    if unit.isNone: return
  some((v.get, unit))

proc prefsParseHex*(s: string): Option[array[3, uint8]] =
  ## `prefs::parse_hex`: "#rrggbb" (or without the `#`).
  var t = s.strip
  while t.startsWith("#"): t = t[1 .. ^1]
  if t.len != 6: return
  var outp: array[3, uint8]
  for i in 0 ..< 3:
    try: outp[i] = uint8(parseHexInt(t[i * 2 .. i * 2 + 1]))
    except ValueError: return
    if t[i * 2] notin HexDigits or t[i * 2 + 1] notin HexDigits: return
  some(outp)
