## Opening chosen parts, channel groups (AOVs) and a beauty precomp of a multi-part OpenEXR
## as layers (Rust `exr_parts.rs`).

import std/[options, strutils]
import ../geom, ../color, ../raster, ../doc, ../cms, ../codecs
import ../codecs/[exr, exr_cryptomatte]
import common, flat

type
  EntryKind* = enum ekPart, ekGroup, ekCryptomatte
  Entry* = object
    kind*: EntryKind
    name*: string
    view*: Option[string]
    width*, height*, channels*: int
    objects*: Option[int]

  Selection* = object
    parts*, groups*: seq[string]
    view*: string
    precomp*: bool

  Group = object
    prefix: string
    part: int
    channels: seq[string]
    alpha: Option[string]

const
  PrecompRequired = ["directdiffuse", "indirectdiffuse", "directspecular", "indirectspecular", "emission"]
  PrecompOptional = ["transmission", "sss", "volume"]

proc cErr(e: ref CodecError): ref IoFailure = ioError(ieCodec, e.msg)

proc normalize(name: string): string =
  for c in name:
    if c in {'a'..'z', 'A'..'Z', '0'..'9'}: result.add c.toLowerAscii

proc channelBase(name: string): string =
  let d = name.rfind('.')
  if d >= 0: name[d + 1 .. ^1] else: name

proc channelBaseSplit(name: string): Option[(string, string)] =
  let d = name.rfind('.')
  if d <= 0: return
  some((name[0 ..< d], name[d + 1 .. ^1]))

proc partSize(p: ExrPart): (int, int) =
  (p.dataWindow[2] - p.dataWindow[0] + 1, p.dataWindow[3] - p.dataWindow[1] + 1)

proc info(bytes: openArray[byte]): seq[ExrPart] =
  try: exrInfo(bytes)
  except CodecError as e: raise cErr(e)

proc groups(bytes: openArray[byte]): seq[Group] =
  for part in info(bytes):
    if part.deep: continue
    var found: seq[Group]
    for c in part.channels:
      let split = channelBaseSplit(c.name)
      if split.isNone: continue
      let (prefix, base) = split.get
      if prefix.len > 0 and prefix[^1] in Digits: continue
      var gi = -1
      for i, g in found:
        if g.prefix == prefix: gi = i
      if gi < 0:
        found.add Group(prefix: prefix, part: part.index)
        gi = found.len - 1
      case base
      of "R", "G", "B": found[gi].channels.add c.name
      of "A": found[gi].alpha = some(c.name)
      else: discard
    for g in found:
      if g.channels.len == 3: result.add g

proc entries*(bytes: openArray[byte]): seq[Entry] =
  ## The parts, AOV groups and Cryptomatte layers a file offers.
  let parts = info(bytes)
  var allDeep = true
  for p in parts:
    if not p.deep: allDeep = false
  if allDeep:
    raise ioError(ieUnsupported, "this EXR holds only deep parts; it opens through the deep decoder")
  for part in parts:
    if part.deep: continue
    var n = 0
    for c in part.channels:
      if channelBase(c.name) in ["R", "G", "B", "A", "Y"]: inc n
    let (w, h) = partSize(part)
    result.add Entry(kind: ekPart, name: (if part.name.isSome: part.name.get else: "part " & $part.index),
                     view: part.view, width: w, height: h, channels: n)
  for g in groups(bytes):
    result.add Entry(kind: ekGroup, name: g.prefix, channels: 3 + ord(g.alpha.isSome))
  var layers: seq[CryptomatteLayer]
  try: layers = cryptomatteLayers(bytes, defaultLimits())
  except CodecError as e: raise cErr(e)
  for l in layers:
    result.add Entry(kind: ekCryptomatte, name: l.name, channels: l.channels.len, objects: some(l.manifest.len))

proc layerSurface(img: Image; mx: var (int, int)): (Surface, ColorMode) =
  let (layout, mode) = if img.layout in {clGray, clGrayA}: (clGrayA, cmGrayscale) else: (clRgba, cmRgb)
  mx[0] = max(mx[0], img.width)
  mx[1] = max(mx[1], img.height)
  (imageSurface(img.convert(layout, isF32), layout, isF32), mode)

proc precompLayer(bytes: openArray[byte]; allGroups: seq[Group]; limits: Limits; mx: var (int, int);
                  warnings: var seq[string]): Layer =
  var have: seq[string]
  for g in allGroups: have.add normalize(g.prefix)
  var missing: seq[string]
  for r in PrecompRequired:
    if r notin have: missing.add r
  if missing.len > 0:
    raise ioError(ieUnsupported, "the AOV set is incomplete for a beauty precomp; missing: " & missing.join(", "))
  var sumRgb, alpha: seq[float32]
  var size = (0, 0)
  for g in allGroups:
    let n = normalize(g.prefix)
    if n notin PrecompRequired and n notin PrecompOptional: continue
    var names = g.channels
    if g.alpha.isSome: names.add g.alpha.get
    var img: Image
    try: img = decodeExrChannels(bytes, g.part, names, limits)
    except CodecError as e: raise cErr(e)
    let rgba = img.convert(clRgba, isF32)
    let (w, h) = (rgba.width, rgba.height)
    if size == (0, 0):
      size = (w, h)
      sumRgb = newSeq[float32](w * h * 3)
      alpha = newSeq[float32](w * h)
    elif size != (w, h):
      raise ioError(ieUnsupported, "the AOV groups differ in size; a beauty precomp needs one common extent")
    let px = rgba.toF32Samples
    for p in 0 ..< min(w * h, px.len div 4):
      sumRgb[p * 3] += px[p * 4]
      sumRgb[p * 3 + 1] += px[p * 4 + 1]
      sumRgb[p * 3 + 2] += px[p * 4 + 2]
      alpha[p] = max(alpha[p], px[p * 4 + 3])
  let (w, h) = size
  if w == 0 or h == 0: raise ioError(ieUnsupported, "the AOV groups are empty")
  var o = newSeq[float32](w * h * 4)
  for p in 0 ..< w * h:
    let a = alpha[p]
    if a > 0:
      o[p * 4] = sumRgb[p * 3] / a
      o[p * 4 + 1] = sumRgb[p * 3 + 1] / a
      o[p * 4 + 2] = sumRgb[p * 3 + 2] / a
    o[p * 4 + 3] = a
  var img: Image
  try: img = imageFromNormalized(w, h, clRgba, isF32, o)
  except CodecError as e: raise cErr(e)
  mx[0] = max(mx[0], w)
  mx[1] = max(mx[1], h)
  let (surface, _) = layerSurface(img, mx)
  warnings.add "beauty (precomp) is the additive sum of the file's AOVs; alpha is the maximum coverage"
  newLayer("beauty (precomp)", rasterContent(surface))

proc importExrParts*(name: string; bytes: openArray[byte]; sel: Selection): ImportResult =
  ## The chosen parts and groups (and optionally a beauty precomp) as 32-bit layers.
  let limits = defaultLimits()
  let parts = info(bytes)
  if parts.len == 0: raise ioError(ieUnsupported, "the EXR has no parts")
  let allGroups = groups(bytes)
  proc viewPasses(view: Option[string]): bool =
    view.isNone or cmpIgnoreCase(sel.view, "both") == 0 or cmpIgnoreCase(view.get, sel.view) == 0
  proc viewDebug(v: Option[string]): string =
    if v.isSome: "Some(" & escapeJson(v.get) & ")" else: "None"
  var chosenParts: seq[ExrPart]
  for want in sel.parts:
    var found = -1
    for i, p in parts:
      if not p.deep and p.name == some(want):
        found = i
        break
    if found < 0: raise ioError(ieUnsupported, "no flat part named " & want)
    let p = parts[found]
    if not viewPasses(p.view):
      raise ioError(ieUnsupported, "the part " & want & " is of the " & viewDebug(p.view) & " view, the filter is " & sel.view)
    var dup = false
    for c in chosenParts:
      if c.index == p.index: dup = true
    if not dup: chosenParts.add p
  var chosenGroups: seq[Group]
  for want in sel.groups:
    var gi = -1
    for i, g in allGroups:
      if g.prefix == want:
        gi = i
        break
    if gi < 0: raise ioError(ieUnsupported, "no channel group named " & want)
    let g = allGroups[gi]
    let partView = if g.part < parts.len: parts[g.part].view else: none(string)
    if not viewPasses(partView):
      raise ioError(ieUnsupported, "the group " & want & " is of the " & viewDebug(partView) & " view, the filter is " & sel.view)
    var dup = false
    for c in chosenGroups:
      if c.prefix == g.prefix: dup = true
    if not dup: chosenGroups.add g
  if chosenParts.len == 0 and chosenGroups.len == 0 and not sel.precomp:
    raise ioError(ieUnsupported, "no parts or groups selected")
  var layers: seq[Layer]
  var warnings: seq[string]
  var grayOnly = true
  var mx = (1, 1)
  var f16Seen = false
  for part in chosenParts:
    var img: Image
    try: img = decodeExrPart(bytes, part.index, limits)
    except CodecError as e: raise cErr(e)
    if img.sample == isF16: f16Seen = true
    let (surface, mode) = layerSurface(img, mx)
    grayOnly = grayOnly and mode == cmGrayscale
    layers.add newLayer((if part.name.isSome: part.name.get else: "part " & $part.index), rasterContent(surface))
  for g in chosenGroups:
    var names = g.channels
    if g.alpha.isSome: names.add g.alpha.get
    var img: Image
    try: img = decodeExrChannels(bytes, g.part, names, limits)
    except CodecError as e: raise cErr(e)
    if img.sample == isF16: f16Seen = true
    let (surface, mode) = layerSurface(img, mx)
    grayOnly = grayOnly and mode == cmGrayscale
    layers.add newLayer(g.prefix, rasterContent(surface))
  if sel.precomp:
    layers.add precompLayer(bytes, allGroups, limits, mx, warnings)
    grayOnly = false
  if layers.len == 0: raise ioError(ieUnsupported, "nothing imported")
  if f16Seen: warnings.add "16-bit float samples are stored as 32-bit float"
  let mode = if grayOnly: cmGrayscale else: cmRgb
  var d = newDocument(name, size(mx[0], mx[1]), mode, stF32)
  d.layers = layers
  if mode == cmRgb: d.iccProfile = shared(bLinearSrgb.profile.toBytes)
  ImportResult(document: d, warnings: warnings)
