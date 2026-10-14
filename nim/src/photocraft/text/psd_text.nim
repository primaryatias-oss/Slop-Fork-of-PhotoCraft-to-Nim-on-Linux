## PSD type layers: the `TySh` block (transform + text descriptor with `EngineData` + warp) ⇄ the
## typed `TextLayer` model (port of `photocraft_text::psd`).
##
## Units: EngineData sizes are in text-space units, which the `TySh` transform maps to document
## pixels; Photoshop shows them as points at the document resolution, so
## `size_pt = FontSize × 72 / dpi` (and likewise leading, tracking-independent lengths, indents).
## Box bounds stay in text-space pixels.
##
## Writing starts from the layer's original `TySh` (when present) and patches only the text, runs,
## paragraphs, fonts, shape and transform, so Photoshop-only settings survive an edit.

import std/[options, strutils, unicode, math]
import ../geom, ../color, ../doc
import ../psd/descriptor
import engine_data, fonts, uprops

type TySh* = object
  ## Parsed `TySh` contents.
  transform*: Affine
  text*: Descriptor
  warp*: Option[Descriptor]
  bounds*: array[4, int32]   ## the four trailing i32 values (left, top, right, bottom)

const MaxTextIndex* = 0xFFFF'i32
  ## The highest text-object number honoured here (`TextIndex`, and the slot array `buildTxt2`
  ## sizes). PSD text objects are numbered densely from 0; a crafted file can name object
  ## `i32::MAX`, and sizing the array by it would make every save allocate gigabytes.

const OpenTypeKeys* = [
  ## EngineData style keys for the Type › OpenType toggles and the OpenType feature each one turns
  ## on (the standard and discretionary ligature keys map to dedicated `CharStyle` fields).
  ("ContextualLigatures", "calt"), ("Swash", "swsh"), ("OldStyle", "onum"),
  ("StylisticAlternates", "salt"), ("Titling", "titl"), ("Ornaments", "ornm"),
  ("Ordinals", "ordn"), ("Fractions", "frac")]

# ---------------------------------------------------------------- byte helpers

proc beF64(d: openArray[byte]; at: int): Option[float64] =
  if at < 0 or at + 8 > d.len: return
  var u: uint64
  for i in 0 ..< 8: u = (u shl 8) or uint64(d[at + i])
  some(cast[float64](u))

proc beI32(d: openArray[byte]; at: int): Option[int32] =
  if at < 0 or at + 4 > d.len: return
  var u: uint32
  for i in 0 ..< 4: u = (u shl 8) or uint32(d[at + i])
  some(cast[int32](u))

proc putU16(o: var seq[byte]; v: uint16) =
  o.add byte(v shr 8)
  o.add byte(v and 0xFF)

proc putF64(o: var seq[byte]; v: float64) =
  let u = cast[uint64](v)
  for s in countdown(56, 0, 8): o.add byte((u shr s) and 0xFF)

proc putI32(o: var seq[byte]; v: int32) =
  let u = cast[uint32](v)
  for s in countdown(24, 0, 8): o.add byte((u shr s) and 0xFF)

proc saturatingI32(v: float32): int32 =
  ## Rust `f32 as i32`: truncates, saturates, NaN → 0.
  if v != v: 0'i32
  elif v >= 2147483647'f32: high(int32)
  elif v <= -2147483648'f32: low(int32)
  else: int32(v)

proc saturatingI64(v: float64): int64 =
  if v != v: 0'i64
  elif v >= 9.223372036854775807e18: high(int64)
  elif v <= -9.223372036854775808e18: low(int64)
  else: int64(v)

proc toIndex(v: Option[int64]; default = 0): int =
  ## `i64 as usize` used as an index: negative values can't index anything.
  if v.isNone: default
  elif v.get < 0: high(int)
  else: int(v.get)

proc utf16Len(r: Rune): int = (if int(r) >= 0x10000: 2 else: 1)

proc utf16Count(s: string): int =
  for r in s.runes: result += utf16Len(r)

# ---------------------------------------------------------------- TySh

proc parseTysh*(data: openArray[byte]): Option[TySh] =
  ## Parses a `TySh` block.
  var m: array[6, float64]
  for i in 0 ..< 6:
    let v = beF64(data, 2 + i * 8)
    if v.isNone: return
    m[i] = v.get
  if data.len < 52: return
  var text: VersionedDescriptor
  var used: int
  try: (text, used) = parseVersionedPrefix(data.toOpenArray(52, data.len - 1))
  except CatchableError: return
  let restAt = 52 + used
  var warp = none(Descriptor)
  var wused = 0
  if restAt + 2 <= data.len:
    try:
      let (w, n) = parseVersionedPrefix(data.toOpenArray(restAt + 2, data.len - 1))
      warp = some(w.descriptor)
      wused = n + 2
    except CatchableError: discard
  let tail = restAt + wused
  var bounds: array[4, int32]
  for i in 0 ..< 4:
    let x = beI32(data, tail + i * 4)
    if x.isSome: bounds[i] = x.get
  some(TySh(transform: Affine(m: m), text: text.descriptor, warp: warp, bounds: bounds))

proc translateTysh*(tysh: openArray[byte]; dx, dy: float64): Option[seq[byte]] =
  ## `tysh` moved by (`dx`, `dy`): only the transform's translation changes. None when the block
  ## is too short.
  var o = @tysh
  for (at, d) in [(34, dx), (42, dy)]:
    let v = beF64(o, at)
    if v.isNone: return
    var b: seq[byte]
    b.putF64(v.get + d)
    for i in 0 ..< 8: o[at + i] = b[i]
  some(o)

proc enumv(t, v: string): Value = vEnum(t, v)

proc defaultWarp(): Descriptor =
  newDescriptor("warp")
    .with("warpStyle", enumv("warpStyle", "warpNone"))
    .with("warpValue", vDouble(0.0))
    .with("warpPerspective", vDouble(0.0))
    .with("warpPerspectiveOther", vDouble(0.0))
    .with("warpRotate", enumv("Ornt", "Hrzn"))

proc writeTysh*(t: TySh): seq[byte] =
  ## Serializes a `TySh` block.
  result.putU16(1)
  for x in t.transform.m: result.putF64(x)
  result.putU16(50)
  result.add versioned(t.text).toBytes
  result.putU16(1)
  result.add versioned(if t.warp.isSome: t.warp.get else: defaultWarp()).toBytes
  for b in t.bounds: result.putI32(b)

proc enumValue(d: Descriptor; key: string): Option[string] =
  let p = d.getPtr(key)
  if p != nil and p.kind == vkEnumerated: some(p.enumValue.s) else: none(string)

proc dnum(d: Descriptor; key: string): Option[float64] =
  let p = d.getPtr(key)
  if p == nil: return
  case p.kind
  of vkDouble: some(p.d)
  of vkUnitFloat: some(p.uv)
  of vkInteger: some(float64(p.i))
  else: none(float64)

proc engineData*(text: Descriptor): Option[EdValue] =
  ## The EngineData of a text descriptor.
  let p = text.getPtr("EngineData")
  if p != nil and p.kind == vkRawData: tryParseEngineData(p.raw) else: none(EdValue)

proc isBlank(s: string): bool =
  for r in s.runes:
    if not isWhiteSpace(int(r)): return false
  true

proc engineTextIsBlank*(tysh: openArray[byte]): bool =
  ## Whether the text engine of a `TySh` block has nothing to draw: its EngineData text
  ## (`EngineDict/Editor/Text`, what Photoshop lays out) is present and only whitespace. Some
  ## Photoshop layers carry their characters only in the descriptor's `Txt ` and render nothing.
  let t = parseTysh(tysh)
  if t.isNone: return false
  let e = engineData(t.get.text)
  if e.isNone: return false
  let s = e.get.path(["EngineDict", "Editor", "Text"]).asStr
  s.isSome and isBlank(s.get)

proc parseTxt2*(data: openArray[byte]): Option[EdValue] =
  ## Parses the document's `Txt2` block (Photoshop's text engine data for all type layers): a bare
  ## sequence of `/key value` pairs (no enclosing `<< >>`).
  var wrapped = newSeqOfCap[byte](data.len + 4)
  wrapped.add [byte('<'), byte('<')]
  wrapped.add data
  wrapped.add [byte('>'), byte('>')]
  let v = tryParseEngineData(wrapped)
  if v.isSome and v.get.kind == ekDict: v else: none(EdValue)

proc arrF*(v: Option[EdValue]): seq[float64] =
  if v.isNone or v.get.kind != ekArray: return
  for x in v.get.elems:
    let f = x.asF64
    if f.isSome: result.add f.get

proc utf16ToByteLengths*(text: string; lens: openArray[float64]): seq[int] =
  ## Converts UTF-16 run lengths over `text` into UTF-8 byte lengths.
  var pos = 0
  for l in lens:
    var units =
      if l != l or l <= 0: 0
      elif l >= 9.2e18: high(int)
      else: int(l)
    var bytes = 0
    while units > 0:
      if pos >= text.len: break
      let n = max(1, runeLenAt(text, pos))
      let r = runeAt(text, pos)
      units = max(0, units - utf16Len(r))
      bytes += n
      pos += n
    result.add bytes

proc textIndexOf(text: Descriptor): Option[int32] =
  let p = text.getPtr("TextIndex")
  if p != nil and p.kind == vkInteger: some(p.i) else: none(int32)

proc applyTxt2*(layer: var TextLayer; tysh: openArray[byte]; txt2: EdValue) =
  ## Applies the automatic kerning modes that Photoshop keeps only in `Txt2` (EngineData writes
  ## Optical and "0" alike as `AutoKerning true`) to `layer`, read from `tysh`. Only when the
  ## layer's `TextIndex` entry still holds the layer's text (an edit elsewhere leaves `Txt2`
  ## stale).
  ##
  ## Observed layout: `Txt2 /1` is the list of text objects; an object's `/0` is its text model
  ## with `/0` the text (`\r` breaks, trailing `\r`) and `/6 /0` its style runs, each
  ## `<< /0 << /0 << … /6 <<style>> >> >> /1 utf16-length >>`; style key `/11` is the auto-kern
  ## mode of the pair after each character: 0 manual, 1 metrics, 2 optical.
  let t = parseTysh(tysh)
  if t.isNone: return
  let ti = textIndexOf(t.get.text)
  if ti.isNone or ti.get < 0: return
  let index = int(ti.get)
  let objs = txt2.path(["1", "1"]).asArray
  if objs.isNone or index >= objs.get.len: return
  let model = objs.get[index].get("0")
  let text = model.get("0").asStr
  if text.isNone: return
  var stripped = text.get
  if stripped.endsWith('\r'): stripped.setLen(stripped.len - 1)
  if stripped.replace('\r', '\n') != layer.text: return
  let styleRuns = model.path(["6", "0"]).asArray
  if styleRuns.isNone: return
  var lensF: seq[float64]
  var modes: seq[Option[int64]]
  for r in styleRuns.get:
    lensF.add r.get("1").asF64.get(0.0)
    modes.add r.path(["0", "0", "6", "11"]).asI64
  let lens = utf16ToByteLengths(text.get, lensF)
  var segs: seq[(int, Option[int64])]
  for i in 0 ..< min(lens.len, modes.len):
    if lens[i] > 0: segs.add (lens[i], modes[i])
  let runs = layer.charRuns
  var res: seq[TextRun]
  var si = 0
  var segEnd = if segs.len > 0: segs[0][0] else: high(int)
  var at = 0
  for r in runs:
    let stop = at + r.len
    while at < stop:
      while at >= segEnd and si + 1 < segs.len:
        inc si
        segEnd = if segEnd > high(int) - segs[si][0]: high(int) else: segEnd + segs[si][0]
      let pieceEnd = if at < segEnd: min(stop, segEnd) else: stop
      var style = r.style
      if si < segs.len and segs[si][1].isSome and at < segEnd:
        case segs[si][1].get
        of 0: style.kerning = kOff
        of 1: style.kerning = kMetrics
        of 2: style.kerning = kOptical
        else: discard
      let ln = pieceEnd - at
      if res.len > 0 and res[^1].style == style: res[^1].len += ln
      else: res.add TextRun(len: ln, style: style)
      at = pieceEnd
  layer.runs = res
  layer.syncSummary()

# ---------------------------------------------------------------- reading

proc lookup(run: EdValue; base: Option[EdValue]; key: string): Option[EdValue] =
  result = run.get(key)
  if result.isNone and base.isSome: result = base.get.get(key)

proc charStyle*(base: Option[EdValue]; d: EdValue; fonts: openArray[string]; k: float32): CharStyle =
  template g(key: string): Option[EdValue] = lookup(d, base, key)
  template num(key: string): Option[float64] = g(key).asF64
  template flag(key: string): Option[bool] = g(key).asBool
  result = defaultCharStyle()
  let fi = g("Font").asI64
  if fi.isSome and fi.get >= 0 and fi.get < int64(fonts.len):
    let ps = fonts[int(fi.get)]
    let r = guessFromPostscript(ps)
    result.fontFamily = r.family
    result.weight = r.weight
    result.italic = r.italic
    result.postscriptName = some(ps)
  let fs = num("FontSize")
  if fs.isSome: result.sizePt = float32(fs.get) * k
  if flag("AutoLeading") == some(false):
    let v = num("Leading")
    if v.isSome: result.leadingPt = some(float32(v.get) * k)
  result.tracking = float32(num("Tracking").get(0.0))
  result.baselineShiftPt = float32(num("BaselineShift").get(0.0)) * k
  result.horizontalScale = float32(num("HorizontalScale").get(1.0))
  result.verticalScale = float32(num("VerticalScale").get(1.0))
  result.fauxBold = flag("FauxBold").get(false)
  result.fauxItalic = flag("FauxItalic").get(false)
  result.underline = flag("Underline").get(false)
  result.strikethrough = flag("Strikethrough").get(false)
  result.ligatures = flag("Ligatures").get(true)
  result.discretionaryLigatures = flag("DLigatures").get(false)
  for (key, tag) in OpenTypeKeys:
    if flag(key) == some(true): result.features.add FontFeature(tag: tag, value: 1)
  result.kerning = if flag("AutoKerning") == some(false): kOff else: kMetrics
  let kv = num("Kerning")
  result.kern = if kv.isSome and kv.get.classify notin {fcNan, fcInf, fcNegInf}: float32(kv.get) else: 0'f32
  result.caps =
    case g("FontCaps").asI64.get(-1)
    of 1: capsSmallCaps
    of 2: capsAllCaps
    else: capsNormal
  let c = g("FillColor")
  if c.isSome:
    let v = arrF(c.get.get("Values"))
    let a = float32(if v.len > 0: v[0] else: 1.0)
    result.color =
      case v.len
      of 5: Color(mode: cmCmyk, c: [float32(v[1]), float32(v[2]), float32(v[3]), float32(v[4])], alpha: a)
      of 2: Color(mode: cmGrayscale, c: [float32(v[1]), 0'f32, 0, 0], alpha: a)
      of 4: rgba(float32(v[1]), float32(v[2]), float32(v[3]), a)
      else: Black

proc paraStyle*(base: Option[EdValue]; d: EdValue; k: float32): ParagraphStyle =
  template num(key: string): Option[float64] = lookup(d, base, key).asF64
  result = defaultParagraphStyle()
  let j = num("Justification")
  result.align =
    if j.isNone: taLeft
    else:
      case saturatingI64(j.get)
      of 1: taRight
      of 2: taCenter
      of 3: taJustifyLeft
      of 4: taJustifyRight
      of 5: taJustifyCenter
      of 6: taJustifyAll
      else: taLeft
  result.firstLineIndentPt = float32(num("FirstLineIndent").get(0.0)) * k
  result.startIndentPt = float32(num("StartIndent").get(0.0)) * k
  result.endIndentPt = float32(num("EndIndent").get(0.0)) * k
  result.spaceBeforePt = float32(num("SpaceBefore").get(0.0)) * k
  result.spaceAfterPt = float32(num("SpaceAfter").get(0.0)) * k
  result.autoLeading = float32(num("AutoLeading").get(1.2))
  result.hyphenate = lookup(d, base, "AutoHyphenate").asBool.get(false)

proc kernUnits*(v: float32): int64 =
  ## Manual kerning as Photoshop stores it: a whole number of 1/1000 em (its UI range is ±1000;
  ## anything non-finite writes 0).
  if v.classify in {fcNan, fcInf, fcNegInf}: 0'i64
  else: int64(clamp(round(v), -10_000'f32, 10_000'f32))

proc manual(s: CharStyle): bool = s.kerning == kOff or kernUnits(s.kern) != 0

proc sameButKerning(a, b: CharStyle): bool =
  ## Whether two styles differ at most in their kerning.
  var x = a
  x.kerning = b.kerning
  x.kern = b.kern
  x == b

proc kerningFromPairs(text: string; runs: seq[TextRun]; incoming: openArray[(int, bool, float32)]): seq[TextRun] =
  ## Reads EngineData's kerning fields back into the model. `incoming` is per EngineData style
  ## run: (UTF-8 length in the engine text, `AutoKerning`, `Kerning`).
  ##
  ## Kerning in EngineData, as observed in Photoshop 2026: a manual kern k (1/1000 em) after
  ## character c is written on character c + 1 as `AutoKerning false` + `Kerning k`; everything
  ## else is `true` + 0. Photoshop's reader takes `Kerning k` on character j as the kern after
  ## j - 1 (making j - 1 manual), and `AutoKerning false` on j as "character j - 2 is manually
  ## kerned". The automatic mode itself (Metrics or Optical) lives in `Txt2` (see `applyTxt2`).
  if text.len == 0 or runs.len == 0: return runs
  var starts: seq[int]
  var p = 0
  while p < text.len:
    starts.add p
    p += max(1, runeLenAt(text, p))
  starts.add text.len
  let n = starts.len - 1
  var kern = newSeq[float32](n)
  var off = newSeq[bool](n)
  var si = 0
  var segEnd = if incoming.len > 0: incoming[0][0] else: 0
  for j, b in starts:
    while b >= segEnd and si < incoming.len:
      inc si
      let add = if si < incoming.len: incoming[si][0] else: 0
      segEnd = if segEnd > high(int) - add: high(int) else: segEnd + add
    if si >= incoming.len: break
    let (_, auto, k) = incoming[si]
    if k != 0 and j >= 1 and j - 1 < n:
      kern[j - 1] = k
      off[j - 1] = true
    if not auto and j >= 2 and j - 2 < n:
      off[j - 2] = true
  var ri = 0
  var runEnd = runs[0].len
  for c in 0 ..< n:
    let b = starts[c]
    let ln = starts[c + 1] - b
    while b >= runEnd and ri + 1 < runs.len:
      inc ri
      runEnd = if runEnd > high(int) - runs[ri].len: high(int) else: runEnd + runs[ri].len
    let r = runs[ri]
    let k = kern[c]
    let kerning = if off[c]: kOff else: kMetrics
    if result.len > 0 and result[^1].style.kern == k and result[^1].style.kerning == kerning and
        sameButKerning(result[^1].style, r.style):
      result[^1].len += ln
    else:
      var style = r.style
      style.kerning = kerning
      style.kern = k
      result.add TextRun(len: ln, style: style)

proc fontNames(res: Option[EdValue]): seq[string] =
  let a = res.get("FontSet").asArray
  if a.isNone: return
  for f in a.get: result.add f.get("Name").asStr.get("")

proc sheetAt(a: Option[seq[EdValue]]; i: int): Option[EdValue] =
  if a.isNone or a.get.len == 0: return
  if i >= 0 and i < a.get.len: some(a.get[i]) else: some(a.get[0])

proc applyEngineData(layer: var TextLayer; e: EdValue; txt: Option[string]; k: float32) =
  let edText = e.path(["EngineDict", "Editor", "Text"]).asStr.get("")
  # Photoshop always ends the engine text with a paragraph break; the descriptor text doesn't.
  let text =
    if txt.isSome: txt.get
    elif edText.endsWith('\r'): edText[0 ..< edText.len - 1]
    else: edText
  layer.text = text.replace('\r', '\n')
  var res = e.get("ResourceDict")
  if res.isNone: res = e.get("DocumentResources")
  let fonts = fontNames(res)
  let defaultSheet = toIndex(res.get("TheNormalStyleSheet").asI64)
  let baseStyle = sheetAt(res.get("StyleSheetSet").asArray, defaultSheet).get("StyleSheetData")
  let defaultParaSheet = toIndex(res.get("TheNormalParagraphSheet").asI64)
  let basePara = sheetAt(res.get("ParagraphSheetSet").asArray, defaultParaSheet).get("Properties")

  # Character runs.
  let srun = e.path(["EngineDict", "StyleRun"])
  var sdata: seq[EdValue]
  let sa = srun.get("RunArray").asArray
  if sa.isSome:
    for x in sa.get:
      let d = x.path(["StyleSheet", "StyleSheetData"])
      if d.isSome: sdata.add d.get
  let slens = utf16ToByteLengths(edText, arrF(srun.get("RunLengthArray")))
  var incoming: seq[(int, bool, float32)]
  layer.runs = @[]
  for i in 0 ..< min(sdata.len, slens.len):
    let d = sdata[i]
    let auto = lookup(d, baseStyle, "AutoKerning").asBool != some(false)
    let kv = lookup(d, baseStyle, "Kerning").asF64
    let kern = if kv.isSome and kv.get.classify notin {fcNan, fcInf, fcNegInf}: float32(kv.get) else: 0'f32
    incoming.add (slens[i], auto, kern)
    layer.runs.add TextRun(len: slens[i], style: charStyle(baseStyle, d, fonts, k))
  layer.runs = kerningFromPairs(layer.text, layer.charRuns, incoming)

  # Paragraph runs.
  let prun = e.path(["EngineDict", "ParagraphRun"])
  var pdata: seq[EdValue]
  let pa = prun.get("RunArray").asArray
  if pa.isSome:
    for x in pa.get:
      let d = x.path(["ParagraphSheet", "Properties"])
      if d.isSome: pdata.add d.get
  let plens = utf16ToByteLengths(edText, arrF(prun.get("RunLengthArray")))
  layer.paragraphs = @[]
  for i in 0 ..< min(pdata.len, plens.len):
    layer.paragraphs.add ParagraphRun(len: plens[i], style: paraStyle(basePara, pdata[i], k))

  # Shape.
  let children = e.path(["EngineDict", "Rendered", "Shapes", "Children"]).asArray
  if children.isSome and children.get.len > 0:
    let sh = children.get[0]
    if sh.get("ShapeType").asI64 == some(1'i64):
      let b = arrF(sh.path(["Cookie", "Photoshop", "BoxBounds"]))
      if b.len == 4:
        layer.shape = TextShape(kind: tsBox, x: float32(b[0]), y: float32(b[1]),
                                width: float32(b[2] - b[0]), height: float32(b[3] - b[1]))

proc isFinite(v: float64): bool = v.classify notin {fcNan, fcInf, fcNegInf}

proc descriptorRect(text: Descriptor; key: string): Option[array[4, float64]] =
  let p = text.getPtr(key)
  if p == nil or p.kind != vkDescriptor: return
  let r = p.desc
  let (l, t, rt, b) = (dnum(r, "Left"), dnum(r, "Top "), dnum(r, "Rght"), dnum(r, "Btom"))
  if l.isNone or t.isNone or rt.isNone or b.isNone: return
  let bounds = [l.get, t.get, rt.get, b.get]
  for v in bounds:
    if not v.isFinite: return
  if bounds[2] > bounds[0] and bounds[3] > bounds[1]: some(bounds) else: none(array[4, float64])

proc legacyPointOrigin(t: TySh): Option[array[2, float64]] =
  ## The local origin to fold into a legacy point-text transform, if present: legacy files use a
  ## zero PointBase plus a very large `boundingBox` origin, with an equally remote transform
  ## translation that cancels it out.
  let e = engineData(t.text)
  if e.isNone: return
  let children = e.get.path(["EngineDict", "Rendered", "Shapes", "Children"]).asArray
  if children.isNone or children.get.len == 0: return
  let pb = children.get[0].path(["Cookie", "Photoshop", "PointBase"]).asArray
  if pb.isNone or pb.get.len != 2: return
  if pb.get[0].asF64 != some(0.0) or pb.get[1].asF64 != some(0.0): return
  let bounds = descriptorRect(t.text, "boundingBox")
  if bounds.isNone: return
  let bb = bounds.get
  let m = t.transform.m
  for v in [m[0], m[1], m[2], m[3], m[4], m[5], bb[0], bb[1]]:
    if not v.isFinite: return
  let shifted = [m[0] * bb[0] + m[2] * bb[1] + m[4], m[1] * bb[0] + m[3] * bb[1] + m[5]]
  let originDistance = max(abs(m[4]), abs(m[5]))
  let shiftedDistance = max(abs(shifted[0]), abs(shifted[1]))
  if max(abs(bb[0]), abs(bb[1])) > 1024.0 and originDistance > 4096.0 and shiftedDistance < originDistance:
    some([bb[0], bb[1]])
  else: none(array[2, float64])

proc textLayerFromTysh*(data: openArray[byte]; dpi: float32): Option[TextLayer] =
  ## Builds a `TextLayer` (model, text and transform; no cache, no `psdRaw`) from `TySh` data.
  let tp = parseTysh(data)
  if tp.isNone: return
  let t = tp.get
  var layer = defaultTextLayer()
  layer.transform = t.transform
  let tx = t.text.getPtr("Txt ")
  let txt = if tx != nil and tx.kind == vkText: some($tx.text) else: none(string)
  layer.orientation = if enumValue(t.text, "Ornt") == some("Vrtc"): orVertical else: orHorizontal
  layer.antialias =
    case enumValue(t.text, "AntA").get("")
    of "Anno", "antiAliasNone": aaNone
    of "antiAliasSharp", "AnSh": aaSharp
    of "AnCr", "antiAliasCrisp": aaCrisp
    of "AnSt", "antiAliasStrong": aaStrong
    of "antiAliasPlatformGray": aaWindows
    of "antiAliasPlatformLCD": aaWindowsLcd
    else: aaSmooth
  if t.warp.isSome:
    let w = t.warp.get
    let style = enumValue(w, "warpStyle").get("warpNone")
    if style != "warpNone":
      layer.warp = some(TextWarp(style: style, value: float32(dnum(w, "warpValue").get(0.0)),
                                 horizontalDistortion: float32(dnum(w, "warpPerspective").get(0.0)),
                                 verticalDistortion: float32(dnum(w, "warpPerspectiveOther").get(0.0)),
                                 horizontal: enumValue(w, "warpRotate") != some("Vrtc")))
  let k = 72'f32 / (if dpi > 0: dpi else: 72'f32)
  let e = engineData(t.text)
  if e.isSome:
    # Without an `Ornt` key, the engine data's writing direction says it (2 = vertical).
    if enumValue(t.text, "Ornt").isNone and
        e.get.path(["EngineDict", "Rendered", "Shapes", "WritingDirection"]).asF64 == some(2.0):
      layer.orientation = orVertical
    applyEngineData(layer, e.get, txt, k)
  else:
    layer.text = txt.get("").replace('\r', '\n')
  # Some legacy PSDs store point text at a large local origin even though PointBase is zero; move
  # the imported layout to the descriptor's ink origin so editing and writing agree (#1469).
  let o = legacyPointOrigin(t)
  if o.isSome:
    let (x, y) = (o.get[0], o.get[1])
    let m = layer.transform.m
    layer.transform.m = [m[0], m[1], m[2], m[3], m[0] * x + m[2] * y + m[4], m[1] * x + m[3] * y + m[5]]
  layer.syncSummary()
  some(layer)

# ---------------------------------------------------------------- writing

proc real(v: float32): EdValue = edReal(float64(v))

proc pairRuns(edText: string; runs: seq[TextRun]): seq[(int, (bool, int64), int)] =
  ## EngineData style runs for `edText` (the layer text with `\r` breaks and the trailing `\r`):
  ## (model run index, `(AutoKerning, Kerning)`, UTF-16 length), split where those change.
  var owner: seq[(int, int)]
  var ri = 0
  var runEnd = if runs.len > 0: runs[0].len else: 0
  var b = 0
  while b < edText.len:
    while b >= runEnd and ri + 1 < runs.len:
      inc ri
      runEnd = if runEnd > high(int) - runs[ri].len: high(int) else: runEnd + runs[ri].len
    let n = max(1, runeLenAt(edText, b))
    owner.add (ri, utf16Len(runeAt(edText, b)))
    b += n
  proc style(i: int): Option[CharStyle] =
    if i >= 0 and i < owner.len and owner[i][0] < runs.len: some(runs[owner[i][0]].style) else: none(CharStyle)
  for j, (ri, n) in owner:
    let ps = style(j - 1)
    let k = if j >= 1 and ps.isSome: kernUnits(ps.get.kern) else: 0'i64
    # `false` marks character j - 2 manual: written when it is and its own kern (on j - 1)
    # doesn't say so already; harmless (and Photoshop's own form) next to a kern on j.
    let off =
      if j >= 2:
        let s = style(j - 2)
        s.isSome and manual(s.get) and (kernUnits(s.get.kern) == 0 or k != 0)
      else: k != 0
    let fields = (not off, k)
    if result.len > 0 and result[^1][0] == ri and result[^1][1] == fields: result[^1][2] += n
    else: result.add (ri, fields, n)

proc postscriptFor*(s: CharStyle): string =
  ## PostScript name for a style (the PSD `FontSet` stores these).
  if s.postscriptName.isSome: return s.postscriptName.get
  let fam = if s.fontFamily.len == 0: DefaultFamily else: s.fontFamily
  let w =
    case s.weight
    of 0 .. 150: "Thin"
    of 151 .. 250: "ExtraLight"
    of 251 .. 350: "Light"
    of 351 .. 450: "Regular"
    of 451 .. 550: "Medium"
    of 551 .. 650: "SemiBold"
    of 651 .. 750: "Bold"
    of 751 .. 850: "ExtraBold"
    else: "Black"
  let style = if s.italic: (if w == "Regular": "Italic" else: w & "Italic") else: w
  fam.replace(" ", "") & "-" & style

proc styleSheetData*(s: CharStyle; font: int; k: float32): EdValue =
  let c = s.color
  var values: seq[EdValue]
  case c.mode
  of cmCmyk: values = @[real(c.alpha), real(c.c[0]), real(c.c[1]), real(c.c[2]), real(c.c[3])]
  of cmGrayscale: values = @[real(c.alpha), real(c.c[0])]
  else:
    let rgb = c.toRgb
    values = @[real(c.alpha), real(rgb[0]), real(rgb[1]), real(rgb[2])]
  let colorType = case c.mode
    of cmCmyk: 2'i64
    of cmGrayscale: 0'i64
    else: 1'i64
  let caps = case s.caps
    of capsNormal: 0'i64
    of capsSmallCaps: 1'i64
    of capsAllCaps: 2'i64
  result = edDict(@[
    ("Font", edInt(int64(font))),
    ("FontSize", real(s.sizePt / k)),
    ("FauxBold", edBool(s.fauxBold)),
    ("FauxItalic", edBool(s.fauxItalic)),
    ("AutoLeading", edBool(s.leadingPt.isNone)),
    ("Leading", real(s.leadingPt.get(0'f32) / k)),
    ("HorizontalScale", real(s.horizontalScale)),
    ("VerticalScale", real(s.verticalScale)),
    ("Tracking", edInt(saturatingI64(float64(round(s.tracking))))),
    ("AutoKerning", edBool(s.kerning != kOff)),
    ("Kerning", edInt(kernUnits(s.kern))),
    ("BaselineShift", real(s.baselineShiftPt / k)),
    ("FontCaps", edInt(caps)),
    ("Underline", edBool(s.underline)),
    ("Strikethrough", edBool(s.strikethrough)),
    ("Ligatures", edBool(s.ligatures)),
    ("DLigatures", edBool(s.discretionaryLigatures)),
    # Photopea lays out edited text without painting it unless its fill is enabled.
    ("FillFlag", edBool(true)),
    ("FillColor", edDict(@[("Type", edInt(colorType)), ("Values", edArray(values))]))])
  for (key, tag) in OpenTypeKeys:
    var on = false
    for f in s.features:
      if f.tag == tag and f.value > 0: on = true
    if on: result.items.add (key, edBool(true))

proc paragraphProperties*(p: ParagraphStyle; k: float32): EdValue =
  let j = case p.align
    of taLeft: 0'i64
    of taRight: 1
    of taCenter: 2
    of taJustifyLeft: 3
    of taJustifyRight: 4
    of taJustifyCenter: 5
    of taJustifyAll: 6
  edDict(@[
    ("Justification", edInt(j)),
    ("FirstLineIndent", real(p.firstLineIndentPt / k)),
    ("StartIndent", real(p.startIndentPt / k)),
    ("EndIndent", real(p.endIndentPt / k)),
    ("SpaceBefore", real(p.spaceBeforePt / k)),
    ("SpaceAfter", real(p.spaceAfterPt / k)),
    ("AutoHyphenate", edBool(p.hyphenate)),
    ("AutoLeading", real(p.autoLeading)),
    ("LeadingType", edInt(0))])

proc fontEntry*(name: string): EdValue =
  edDict(@[("Name", edString(name)), ("Script", edInt(0)), ("FontType", edInt(1)), ("Synthetic", edInt(0))])

proc txt2Mode(s: CharStyle): int64 =
  ## The `Txt2` auto-kern mode of a style: 0 manual, 1 metrics, 2 optical.
  if manual(s): 0 elif s.kerning == kOptical: 2 else: 1

proc txt2ModeRuns(layer: TextLayer): seq[(int64, int)] =
  ## `Txt2` style runs for `layer`: `(mode, UTF-16 length)` over the engine text (`\r` breaks and
  ## the trailing `\r`, which takes the last run's mode), split where the auto-kern mode changes.
  let runs = layer.charRuns
  let edText = layer.text.replace('\n', '\r') & "\r"
  var ri = 0
  var runEnd = if runs.len > 0: runs[0].len else: 0
  var b = 0
  while b < edText.len:
    while b >= runEnd and ri + 1 < runs.len:
      inc ri
      runEnd = if runEnd > high(int) - runs[ri].len: high(int) else: runEnd + runs[ri].len
    let mode = if ri < runs.len: txt2Mode(runs[ri].style) else: 1'i64
    let u = utf16Len(runeAt(edText, b))
    if result.len > 0 and result[^1][0] == mode: result[^1][1] += u
    else: result.add (mode, u)
    b += max(1, runeLenAt(edText, b))

proc needsTxt2*(objects: openArray[(int32, TextLayer)]): bool =
  ## Whether the document `Txt2` block is needed for `objects`: only a non-Metrics auto-kern mode
  ## (`Optical`) is exclusive to `Txt2`, so a fresh document of ordinary Metrics text has nothing
  ## to put in the block (writing an empty stub makes some readers fail, #3514).
  for (_, l) in objects:
    for r in l.charRuns:
      if r.style.kerning == kOptical: return true
  false

proc withModes(runs: openArray[EdValue]; modes: openArray[(int64, int)]): Option[seq[EdValue]] =
  ## The file's own `Txt2` style runs `runs` with the auto-kern modes `modes` set at
  ## `/0 /0 /6 /11`, keeping every other style key. A run a mode change falls inside is split
  ## there. None when the runs don't cover the text or lack a style dictionary.
  var lens: seq[int]
  for r in runs:
    let l = r.get("1").asI64
    if l.isNone or l.get < 0: return
    lens.add int(l.get)
  var total = 0
  for m in modes:
    if total > high(int) - m[1]: return
    total += m[1]
  var sum = 0
  for l in lens:
    if sum > high(int) - l: return
    sum += l
  if sum != total: return
  if modes.len == 0: return
  var res: seq[EdValue]
  var mi = 0
  var modeEnd = modes[0][1]
  var at = 0
  for idx, run in runs:
    let ln = lens[idx]
    if ln == 0:
      res.add run
      continue
    let stop = at + ln
    while at < stop:
      while at >= modeEnd and mi + 1 < modes.len:
        inc mi
        modeEnd = if modeEnd > high(int) - modes[mi][1]: high(int) else: modeEnd + modes[mi][1]
      let pieceEnd = min(stop, modeEnd)
      if pieceEnd <= at: return
      var piece = run
      if pieceEnd - at != ln: piece.set("1", edInt(int64(pieceEnd - at)))
      let l0 = piece.getMut("0")
      if l0 == nil: return
      let l1 = l0[].getMut("0")
      if l1 == nil: return
      let style = l1[].getMut("6")
      if style == nil or style[].kind != ekDict: return
      let mode = modes[mi][0]
      # Keep the file's value (and its number type) when the mode is the same.
      if style[].get("11").asI64 != some(mode): style[].set("11", edInt(mode))
      res.add piece
      at = pieceEnd
  some(res)

proc buildTxt2*(objects: openArray[(int32, TextLayer)]; previous: Option[seq[byte]]): seq[byte] =
  ## The document's `Txt2` block for `objects`: `(TextIndex, layer)` pairs, one text object each
  ## at the slot its `TextIndex` names (unused slots are empty dictionaries), carrying the
  ## automatic kerning mode of every pair (`/11`: 0 manual, 1 metrics, 2 optical). Numbers outside
  ## `0..MaxTextIndex` are skipped.
  ##
  ## `previous` is the block this replaces (the file's own). A kept object (text unchanged) keeps
  ## everything the file held beyond its model's text and style runs, and its own style runs keep
  ## every style key (only the auto-kern mode is set). Everything else the file's block held is
  ## kept, and a block whose content comes out unchanged is returned byte for byte (#2374).
  let prev = if previous.isSome: parseTxt2(previous.get) else: none(EdValue)
  let prevObjects = prev.path(["1", "1"]).asArray.get(@[])
  var slots = prevObjects
  for (index, layer) in objects:
    if index < 0 or index > MaxTextIndex: continue
    let text = layer.text.replace('\n', '\r') & "\r"
    let modes = txt2ModeRuns(layer)
    let at = int(index)
    var kept = none(EdValue)
    if at < prevObjects.len and prevObjects[at].get("0").get("0").asStr == some(text):
      kept = some(prevObjects[at])
    var obj = kept.get(edDict())
    var model = obj.get("0").get(edDict())
    if model.kind != ekDict: model = edDict()
    var fileRuns = none(seq[EdValue])
    if kept.isSome:
      let rs = model.path(["6", "0"]).asArray
      if rs.isSome: fileRuns = withModes(rs.get, modes)
    var runs: seq[EdValue]
    if fileRuns.isSome: runs = fileRuns.get
    else:
      for (mode, ln) in modes:
        # `<< /0 << /0 << /0 () /6 << /0 0 /11 mode >> >> >> /1 len >>`.
        let style = edDict(@[("0", edInt(0)), ("11", edInt(mode))])
        let data = edDict(@[("0", edString("")), ("6", style)])
        runs.add edDict(@[("0", edDict(@[("0", data)])), ("1", edInt(int64(ln)))])
    model.set("0", edString(text))
    let sr = model.getMut("6")
    if sr != nil and sr[].kind == ekDict: sr[].set("0", edArray(runs))
    else: model.set("6", edDict(@[("0", edArray(runs))]))
    obj.set("0", model)
    while slots.len <= at: slots.add edDict()
    slots[at] = obj
  # The outer dictionary keeps whatever the block held (`/98` version, block extras, …), and `/1`
  # whatever it holds beside the text objects.
  var outer = prev.get(edDict(@[("98", edDict(@[("0", edInt(14))])), ("0", edDict())]))
  let objs = outer.getMut("1")
  if objs != nil and objs[].kind == ekDict: objs[].set("1", edArray(slots))
  else: outer.set("1", edDict(@[("1", edArray(slots))]))
  # Nothing changed: the file's own bytes (writing them back would re-encode strings and numbers).
  if previous.isSome and prev.isSome and prev.get == outer: return previous.get
  if outer.kind == ekDict: writeBare(outer.items) else: writeBare([])

proc textIndex*(tysh: openArray[byte]): Option[int32] =
  ## The layer's text-object number in the document's `Txt2` block, when its `TySh` names one
  ## within `0..MaxTextIndex`.
  let t = parseTysh(tysh)
  if t.isNone: return
  let i = textIndexOf(t.get.text)
  if i.isSome and i.get >= 0 and i.get <= MaxTextIndex: i else: none(int32)

proc setDescItem(d: var Descriptor; key: string; v: Value) =
  for i in 0 ..< d.items.len:
    if d.items[i][0].s == key:
      d.items[i][1] = v
      return
  d.items.add (id(key), v)

proc setTextIndex*(tysh: openArray[byte]; index: int32): Option[seq[byte]] =
  ## Rewrites a `TySh`'s `TextIndex`. None when `tysh` doesn't parse.
  var t = parseTysh(tysh)
  if t.isNone: return
  var ty = t.get
  ty.text.setDescItem("TextIndex", vInt(index))
  some(writeTysh(ty))

proc buildEngineData*(layer: TextLayer; tmpl: Option[EdValue]; dpi: float32): EdValue =
  ## Updates (or creates) an EngineData tree for `layer`.
  let k = 72'f32 / (if dpi > 0: dpi else: 72'f32)
  let fresh = edDict(@[("EngineDict", edDict()), ("ResourceDict", edDict())])
  # The template comes from the PSD: it may not be a dictionary or may lack `EngineDict`.
  var e = if tmpl.isSome and tmpl.get.kind == ekDict: tmpl.get else: fresh
  let ed0 = e.getPtr("EngineDict")
  if ed0 == nil or ed0[].kind != ekDict: e.set("EngineDict", edDict())
  let runs = layer.charRuns
  let paras = layer.paragraphRuns
  let text = layer.text.replace('\n', '\r')
  let edText = text & "\r"

  # Fonts: keep the template's font list and append new PostScript names.
  var fonts: seq[string]
  let fs = e.path(["ResourceDict", "FontSet"]).asArray
  if fs.isSome:
    for f in fs.get: fonts.add f.get("Name").asStr.get("")
  proc fontIndex(fonts: var seq[string]; ps: string): int =
    let i = fonts.find(ps)
    if i >= 0: return i
    fonts.add ps
    fonts.len - 1
  var sruns, slens: seq[EdValue]
  for (ri, pair, n) in pairRuns(edText, runs):
    if ri >= runs.len: continue
    let fi = fonts.fontIndex(postscriptFor(runs[ri].style))
    var data = styleSheetData(runs[ri].style, fi, k)
    data.set("AutoKerning", edBool(pair[0]))
    data.set("Kerning", edInt(pair[1]))
    sruns.add edDict(@[("StyleSheet", edDict(@[("StyleSheetData", data)]))])
    slens.add edInt(int64(n))
  var pruns, plens: seq[EdValue]
  var at = 0
  for i, p in paras:
    let a = min(at, text.len)
    let piece = text[a ..< min(at + p.len, text.len)]
    at += p.len
    var n = utf16Count(piece)
    if i + 1 == paras.len: inc n
    pruns.add edDict(@[("ParagraphSheet", edDict(@[("DefaultStyleSheet", edInt(0)),
                                                   ("Properties", paragraphProperties(p.style, k))]))])
    plens.add edInt(int64(n))
  var fontset: seq[EdValue]
  for f in fonts: fontset.add fontEntry(f)

  let dict = e.getMut("EngineDict")
  if dict == nil: return fresh
  var editor = dict[].get("Editor").get(edDict())
  editor.set("Text", edString(edText))
  dict[].set("Editor", editor)
  var pr = dict[].get("ParagraphRun").get(edDict())
  pr.set("RunArray", edArray(pruns))
  pr.set("RunLengthArray", edArray(plens))
  if not pr.has("IsJoinable"): pr.set("IsJoinable", edInt(1))
  dict[].set("ParagraphRun", pr)
  var sr = dict[].get("StyleRun").get(edDict())
  sr.set("RunArray", edArray(sruns))
  sr.set("RunLengthArray", edArray(slens))
  if not sr.has("IsJoinable"): sr.set("IsJoinable", edInt(2))
  dict[].set("StyleRun", sr)
  let aa = case layer.antialias
    of aaNone: 0'i64
    of aaSharp: 1
    of aaCrisp: 2
    of aaStrong: 3
    of aaSmooth: 4
    # EngineData has no platform modes; the `AntA` descriptor carries the exact value.
    of aaWindows, aaWindowsLcd: 1
  dict[].set("AntiAlias", edInt(aa))
  if not dict[].has("UseFractionalGlyphWidths"): dict[].set("UseFractionalGlyphWidths", edBool(true))
  let shapeType = if layer.shape.kind == tsBox: 1'i64 else: 0'i64
  var photoshop = edDict(@[("ShapeType", edInt(shapeType))])
  if layer.shape.kind == tsBox:
    let s = layer.shape
    photoshop.set("BoxBounds", edArray(@[real(s.x), real(s.y), real(s.x + s.width), real(s.y + s.height)]))
  else:
    photoshop.set("PointBase", edArray(@[real(0), real(0)]))
  photoshop.set("Base", edDict(@[
    ("ShapeType", edInt(shapeType)),
    ("TransformPoint0", edArray(@[real(1), real(0)])),
    ("TransformPoint1", edArray(@[real(0), real(1)])),
    ("TransformPoint2", edArray(@[real(0), real(0)]))]))
  # EngineData writing direction and procession, as Photoshop writes vertical type: 2 and 1
  # (horizontal: 0 and 0). The descriptor's `Ornt` agrees.
  let writing = if layer.orientation == orVertical: 2'i64 else: 0'i64
  let child = edDict(@[
    ("ShapeType", edInt(shapeType)),
    ("Procession", edInt(if writing == 2: 1 else: 0)),
    ("Lines", edDict(@[("WritingDirection", edInt(writing)), ("Children", edArray())])),
    ("Cookie", edDict(@[("Photoshop", photoshop)]))])
  dict[].set("Rendered", edDict(@[
    ("Version", edInt(1)),
    ("Shapes", edDict(@[("WritingDirection", edInt(writing)), ("Children", edArray(@[child]))]))]))

  for key in ["ResourceDict", "DocumentResources"]:
    if key == "DocumentResources" and not e.has(key):
      let rd = e.get("ResourceDict").get(edDict())
      e.set(key, rd)
      continue
    let rd = e.getMut(key)
    if rd == nil: continue
    rd[].set("FontSet", edArray(fontset))
    if not rd[].has("StyleSheetSet"):
      var base = defaultCharStyle()
      if runs.len > 0: base = runs[0].style
      rd[].set("StyleSheetSet", edArray(@[edDict(@[("Name", edString("Normal RGB")),
                                                   ("StyleSheetData", styleSheetData(base, 0, k))])]))
    if not rd[].has("ParagraphSheetSet"):
      rd[].set("ParagraphSheetSet", edArray(@[edDict(@[
        ("Name", edString("Normal RGB")), ("DefaultStyleSheet", edInt(0)),
        ("Properties", paragraphProperties(defaultParagraphStyle(), k))])]))
    for k2 in ["TheNormalStyleSheet", "TheNormalParagraphSheet"]:
      if not rd[].has(k2): rd[].set(k2, edInt(0))
  e

proc buildTysh*(layer: TextLayer; dpi: float32; ink: Option[array[4, float32]]): seq[byte] =
  ## Builds the `TySh` block for `layer`, patching its current `psdRaw` when present. `ink` is the
  ## layer's rendered bounds in text space (for `bounds`/`boundingBox`).
  let old = if layer.psdRaw.isNil: none(TySh) else: parseTysh(layer.psdRaw.data)
  let tmpl = if old.isSome: engineData(old.get.text) else: none(EdValue)
  let e = buildEngineData(layer, tmpl, dpi)
  var text = if old.isSome: old.get.text else: newDescriptor("TxLr")
  text.setDescItem("Txt ", vText(layer.text.replace('\n', '\r')))
  if text.getPtr("textGridding") == nil: text.setDescItem("textGridding", enumv("textGridding", "None"))
  text.setDescItem("Ornt", enumv("Ornt", if layer.orientation == orVertical: "Vrtc" else: "Hrzn"))
  let aa = case layer.antialias
    of aaNone: "Anno"
    of aaSharp: "antiAliasSharp"
    of aaCrisp: "AnCr"
    of aaStrong: "AnSt"
    of aaSmooth: "AnSm"
    of aaWindows: "antiAliasPlatformGray"
    of aaWindowsLcd: "antiAliasPlatformLCD"
  text.setDescItem("AntA", enumv("Annt", aa))
  var lrtb: array[4, float32]
  if layer.shape.kind == tsBox:
    let s = layer.shape
    lrtb = [s.x, s.y, s.x + s.width, s.y + s.height]
  elif ink.isSome: lrtb = ink.get
  let (l, t, r, b) = (lrtb[0], lrtb[1], lrtb[2], lrtb[3])
  proc rect(cls: string): Value =
    vDesc(newDescriptor(cls)
      .with("Left", vUnit("#Pnt", float64(l)))
      .with("Top ", vUnit("#Pnt", float64(t)))
      .with("Rght", vUnit("#Pnt", float64(r)))
      .with("Btom", vUnit("#Pnt", float64(b))))
  text.setDescItem("bounds", rect("bounds"))
  text.setDescItem("boundingBox", rect("boundingBox"))
  if text.getPtr("TextIndex") == nil: text.setDescItem("TextIndex", vInt(0))
  text.setDescItem("EngineData", Value(kind: vkRawData, raw: writeEngineData(e)))
  var warp = none(Descriptor)
  if layer.warp.isSome:
    let w = layer.warp.get
    warp = some(newDescriptor("warp")
      .with("warpStyle", enumv("warpStyle", w.style))
      .with("warpValue", vDouble(float64(w.value)))
      .with("warpPerspective", vDouble(float64(w.horizontalDistortion)))
      .with("warpPerspectiveOther", vDouble(float64(w.verticalDistortion)))
      .with("warpRotate", enumv("Ornt", if w.horizontal: "Hrzn" else: "Vrtc")))
  elif old.isSome and old.get.warp.isSome and enumValue(old.get.warp.get, "warpStyle") == some("warpNone"):
    warp = old.get.warp
  let bounds = [saturatingI32(floor(l)), saturatingI32(floor(t)), saturatingI32(ceil(r)), saturatingI32(ceil(b))]
  writeTysh(TySh(transform: layer.transform, text: text, warp: warp, bounds: bounds))
