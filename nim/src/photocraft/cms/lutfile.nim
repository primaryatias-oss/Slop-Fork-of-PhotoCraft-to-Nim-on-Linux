## Creative 3D LUT files for Image › Adjustments › Color Lookup: `.cube` (Adobe/Resolve), `.3dl`
## (Autodesk Lustre / Flame) and `.look` (SpeedGrade XML), plus a few generated (CC0) looks.
##
## Every table is normalised to `LutFile`: `size`³ RGB triplets in 0..1, **red varying fastest**.

import std/[strutils, math, options, strformat]

type
  LutFile* = object
    title*: string
    size*: int
    data*: seq[float32] ## `size`³ × 3 values, red fastest.
    domainMin*, domainMax*: array[3, float32]

  LutError* = object of CatchableError

const MaxLutSize* = 129

proc lutErr(msg: string): ref LutError = newException(LutError, msg)

proc checkedDataLen(size: int): (int, int) =
  if size < 2 or size > MaxLutSize: raise lutErr("unsupported LUT size " & $size)
  (size * size * size, size * size * size * 3)

proc lutFromFn*(title: string; size: int; f: proc(c: array[3, float32]): array[3, float32]): LutFile =
  ## Samples `f` on an `size`³ grid.
  let m = float32(max(size, 2) - 1)
  result = LutFile(title: title, size: size, domainMin: [0.0'f32, 0, 0], domainMax: [1.0'f32, 1, 1])
  result.data = newSeqOfCap[float32](size * size * size * 3)
  for b in 0 ..< size:
    for g in 0 ..< size:
      for r in 0 ..< size:
        let o = f([float32(r) / m, float32(g) / m, float32(b) / m])
        for v in o: result.data.add clamp(v, 0.0'f32, 1.0'f32)

proc identityLut*(size: int): LutFile = lutFromFn("Identity", size, proc(c: array[3, float32]): array[3, float32] = c)

proc domainIsDefault*(l: LutFile): bool =
  l.domainMin == [0.0'f32, 0, 0] and l.domainMax == [1.0'f32, 1, 1]

proc check(l: LutFile): LutFile =
  let (entries, dataLen) = checkedDataLen(l.size)
  if l.data.len != dataLen: raise lutErr(&"expected {entries} entries, found {l.data.len div 3}")
  for v in l.data:
    if classify(v) in {fcNan, fcInf, fcNegInf}: raise lutErr("non-finite LUT value")
  l

proc parseF32(s: string; v: var float32): bool =
  try:
    v = float32(parseFloat(s))
    true
  except ValueError: false

proc parseCube*(text: string): LutFile =
  ## Adobe / Resolve `.cube`. 1D LUTs (`LUT_1D_SIZE`) are expanded to a 33³ table.
  var title = ""
  var size3, size1 = 0
  var dmin = [0.0'f32, 0, 0]
  var dmax = [1.0'f32, 1, 1]
  var rows: seq[float32]
  for raw in text.splitLines:
    let line = raw.split('#')[0].strip
    if line.len == 0: continue
    let parts = line.splitWhitespace
    let key = parts[0]
    proc nums(): array[3, float32] =
      var v: seq[float32]
      for s in parts[1..^1]:
        var f: float32
        if parseF32(s, f): v.add f
      if v.len != 3: raise lutErr("bad line `" & line & "`")
      [v[0], v[1], v[2]]
    case key
    of "TITLE": title = line[5..^1].strip.strip(chars = {'"'})
    of "LUT_3D_SIZE":
      try: size3 = parseInt(parts[1])
      except CatchableError: raise lutErr("bad LUT_3D_SIZE")
    of "LUT_1D_SIZE":
      try: size1 = parseInt(parts[1])
      except CatchableError: raise lutErr("bad LUT_1D_SIZE")
    of "DOMAIN_MIN": dmin = nums()
    of "DOMAIN_MAX": dmax = nums()
    of "LUT_3D_INPUT_RANGE", "LUT_1D_INPUT_RANGE":
      var v: seq[float32]
      for s in parts[1..^1]:
        var f: float32
        if parseF32(s, f): v.add f
      if v.len == 2:
        dmin = [v[0], v[0], v[0]]
        dmax = [v[1], v[1], v[1]]
    else:
      if key[0] in {'0'..'9', '-', '+', '.'}:
        var v: seq[float32]
        var f: float32
        if not parseF32(key, f): raise lutErr("bad number in `" & line & "`")
        v.add f
        for s in parts[1..^1]:
          if parseF32(s, f): v.add f
        if v.len != 3: raise lutErr("bad data line `" & line & "`")
        rows.add v
  var domainOk = true
  for k in 0..2:
    if not (dmin[k] < dmax[k] and classify(dmin[k]) notin {fcNan, fcInf, fcNegInf} and classify(dmax[k]) notin {fcNan, fcInf, fcNegInf}):
      domainOk = false
  if (size3 > 0 or size1 >= 2) and not domainOk:
    raise lutErr("DOMAIN_MIN must lie below DOMAIN_MAX")
  if size3 > 0:
    return LutFile(title: title, size: size3, data: rows, domainMin: dmin, domainMax: dmax).check
  if size1 >= 2:
    if rows.len != size1 * 3: raise lutErr("no LUT_3D_SIZE in .cube file")
    let rowsC = rows
    proc curve(ch: int; v: float32): float32 =
      let t = (v - dmin[ch]) / (dmax[ch] - dmin[ch])
      let x = (if classify(t) notin {fcNan, fcInf, fcNegInf}: clamp(t, 0.0'f32, 1.0'f32) else: 0.0'f32) * float32(size1 - 1)
      let i = min(int(floor(x)), size1 - 2)
      let f = x - float32(i)
      rowsC[i * 3 + ch] * (1.0'f32 - f) + rowsC[(i + 1) * 3 + ch] * f
    var l = lutFromFn(title, 33, proc(c: array[3, float32]): array[3, float32] = [curve(0, c[0]), curve(1, c[1]), curve(2, c[2])])
    l.title = title
    return l.check
  raise lutErr("no LUT_3D_SIZE in .cube file")

proc parse3dl*(text: string): LutFile =
  ## Lustre `.3dl`: integer RGB rows with **blue** varying fastest; bit depth inferred.
  var rows: seq[array[3, float64]]
  for raw in text.splitLines:
    let line = raw.split('#')[0].strip
    if line.len == 0 or line[0].isAlphaAscii: continue
    var v: seq[float64]
    for s in line.splitWhitespace:
      try: v.add parseFloat(s)
      except ValueError: discard
    if v.len == 3: rows.add [v[0], v[1], v[2]]
  let n = int(round(cbrt(float64(rows.len))))
  if n < 2 or n * n * n != rows.len: raise lutErr($rows.len & " rows is not a cube")
  var mx = 0.0
  for r in rows:
    for v in r: mx = max(mx, v)
  var scale = 1.0
  if mx > 1.0:
    scale = mx
    for s in [1023.0, 4095.0, 16383.0, 65535.0]:
      if mx <= s:
        scale = s
        break
  var data = newSeq[float32](n * n * n * 3)
  for i, row in rows:
    let r = i div (n * n)
    let g = (i div n) mod n
    let b = i mod n
    let at = ((b * n + g) * n + r) * 3
    for k in 0..2: data[at + k] = float32(row[k] / scale)
  LutFile(size: n, data: data, domainMin: [0.0'f32, 0, 0], domainMax: [1.0'f32, 1, 1]).check

proc parseLook*(text: string): LutFile =
  ## SpeedGrade `.look`: `<size>` and a hex `<data>` string of little-endian float32 triplets.
  proc tag(name: string): Option[string] =
    let open = "<" & name & ">"
    let s = text.find(open)
    if s < 0: return none(string)
    let start = s + open.len
    let e = text.find("</" & name & ">", start)
    if e < 0: return none(string)
    some(text[start ..< e].strip.strip(chars = {'"'}).strip)
  let sz = tag("size")
  var size = 0
  try: size = parseInt(sz.get(""))
  except ValueError: raise lutErr("no <size> in .look")
  let (entries, _) = checkedDataLen(size)
  let dataT = tag("data")
  if dataT.isNone: raise lutErr("no <data> in .look")
  var hex = ""
  for c in dataT.get:
    if c in HexDigits: hex.add c
  var bytes: seq[byte]
  for k in 0 ..< hex.len div 2: bytes.add byte(parseHexInt(hex[2 * k .. 2 * k + 1]))
  var floats: seq[float32]
  for k in 0 ..< bytes.len div 4:
    let u = uint32(bytes[4 * k]) or (uint32(bytes[4 * k + 1]) shl 8) or (uint32(bytes[4 * k + 2]) shl 16) or (uint32(bytes[4 * k + 3]) shl 24)
    floats.add cast[float32](u)
  var data: seq[float32]
  if floats.len == entries * 4:
    for k in 0 ..< entries:
      data.add floats[4 * k]; data.add floats[4 * k + 1]; data.add floats[4 * k + 2]
  else: data = floats
  LutFile(title: tag("title").get(""), size: size, data: data, domainMin: [0.0'f32, 0, 0], domainMax: [1.0'f32, 1, 1]).check

proc parseLutFile*(fileName: string; bytes: openArray[byte]): LutFile =
  ## Parses a LUT, picking the format from `fileName`'s extension (falls back to sniffing).
  let lower = fileName.toLowerAscii
  var text = newString(bytes.len)
  for i, b in bytes: text[i] = char(b)
  if lower.endsWith(".3dl"): parse3dl(text)
  elif lower.endsWith(".look") or text.strip(trailing = false).startsWith("<"): parseLook(text)
  else: parseCube(text)

proc writeCube*(l: LutFile): string =
  ## Writes a `.cube` file (what Photoshop embeds in a Color Lookup layer).
  if l.title.len > 0: result.add "TITLE \"" & l.title.replace("\"", "'") & "\"\n"
  result.add "LUT_3D_SIZE " & $l.size & "\n"
  for k in 0 ..< l.data.len div 3:
    result.add formatFloat(l.data[3 * k], ffDecimal, 6) & " " & formatFloat(l.data[3 * k + 1], ffDecimal, 6) & " " &
      formatFloat(l.data[3 * k + 2], ffDecimal, 6) & "\n"

proc luma(c: array[3, float32]): float32 = 0.299'f32 * c[0] + 0.587'f32 * c[1] + 0.114'f32 * c[2]
proc smooth(x0: float32): float32 =
  let x = clamp(x0, 0.0'f32, 1.0'f32)
  x * x * (3.0'f32 - 2.0'f32 * x)

const BuiltinLooks*: array[8, (string, string)] = [
  ("warm", "Warm Filter"), ("cool", "Cool Filter"), ("tealOrange", "Teal & Orange"),
  ("bleachBypass", "Bleach Bypass"), ("fadedFilm", "Faded Film"), ("dayForNight", "Day for Night"),
  ("monoContrast", "Mono High Contrast"), ("crossProcess", "Cross Process")]

proc builtinLook*(id: string): Option[LutFile] =
  ## A built-in look by id (33³), or none.
  var label = ""
  for b in BuiltinLooks:
    if b[0] == id: label = b[1]
  if label.len == 0: return none(LutFile)
  proc look(c: array[3, float32]): array[3, float32] =
    case id
    of "warm": [c[0] * 0.92'f32 + 0.08'f32, c[1] * 0.97'f32 + 0.02'f32, c[2] * 0.85'f32]
    of "cool": [c[0] * 0.86'f32, c[1] * 0.97'f32 + 0.01'f32, c[2] * 0.9'f32 + 0.1'f32]
    of "tealOrange":
      let l = luma(c)
      let t = smooth(l)
      let teal = [0.0'f32, 0.5, 0.55]
      let orange = [1.0'f32, 0.62, 0.3]
      var r: array[3, float32]
      for k in 0..2:
        let tint = teal[k] * (1.0'f32 - t) + orange[k] * t
        r[k] = c[k] * 0.75'f32 + min(tint * l * 1.1'f32, 1.0'f32) * 0.25'f32 + (smooth(c[k]) - c[k]) * 0.3'f32
      r
    of "bleachBypass":
      let l = luma(c)
      let s = smooth(l)
      var r: array[3, float32]
      for k in 0..2: r[k] = (c[k] * 0.45'f32 + l * 0.55'f32) * 0.5'f32 + s * 0.5'f32
      r
    of "fadedFilm":
      let l = luma(c)
      let tint = [1.0'f32, 0.96, 0.88]
      var r: array[3, float32]
      for k in 0..2: r[k] = (0.08'f32 + (c[k] * 0.8'f32 + l * 0.2'f32) * 0.84'f32) * tint[k] + 0.02'f32
      r
    of "dayForNight":
      let l = luma(c)
      [l * 0.35'f32, l * 0.45'f32 + c[1] * 0.05'f32, l * 0.65'f32 + c[2] * 0.15'f32]
    of "monoContrast":
      let v = smooth(smooth(luma(c)))
      [v, v, v]
    else: [smooth(c[0]) * 1.05'f32, c[1] * 1.1'f32 - 0.03'f32, c[2] * 0.7'f32 + 0.15'f32]
  let f = proc(c: array[3, float32]): array[3, float32] = look(c)
  some(lutFromFn(label, 33, f))
