## Image › Mode › Multichannel, and conversions out of it (`multichannel_cmds.rs`). Part of the
## `engine/core` unit: included, not imported (Image › Mode calls `convertFromMultichannel`).
##
## A Multichannel document has no layers: its image is a set of ink channels, kept as spot channels
## in `Document.channels` (value = ink density, 1 = solid; the compositor prints them over white).
## Like Photoshop the conversion flattens and turns each colour channel into an ink: RGB → Cyan,
## Magenta, Yellow (density = 1 − value); CMYK → Cyan, Magenta, Yellow, Black; Lab → Alpha 1–3;
## Grayscale → Black; Duotone → one channel per ink. Existing alpha channels stay alpha channels.
## Converting back reinterprets the channel data when the count matches the target (3 → RGB / Lab,
## 4 → CMYK, 1 → Grayscale); otherwise the printed look is converted.

const
  InkCyan = Color(mode: cmRgb, c: [0.0'f32, 0.682, 0.937, 0], alpha: 1)
  InkMagenta = Color(mode: cmRgb, c: [0.925'f32, 0.0, 0.549, 0], alpha: 1)
  InkYellow = Color(mode: cmRgb, c: [1.0'f32, 0.949, 0.0, 0], alpha: 1)
  InkBlack = Color(mode: cmRgb, c: [0.137'f32, 0.122, 0.125, 0], alpha: 1)
  MultichannelSources = [cmRgb, cmCmyk, cmLab, cmGrayscale, cmDuotone]

proc multichannelE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if d.doc.mode in MultichannelSources: ""
  else: "Multichannel needs an RGB, CMYK, Lab, Grayscale or Duotone image (the document is " & $d.doc.mode & ")"

proc flattenedPlanes(doc: Document): seq[seq[float32]] =
  ## Native colour planes of the flattened image (opaque over white): one per colour channel of the
  ## document's pixel format. A lone plain layer is read exactly; anything else goes through the
  ## compositor.
  let fmt = doc.pixelFormat
  let cc = fmt.mode.colorChannels
  let canvas = doc.bounds
  let n = canvas.width * canvas.height
  let white = fromRgba(fmt, [1'f32, 1, 1, 1])
  result = newSeq[seq[float32]](cc)
  for pl in result.mitems: pl = newSeqOfCap[float32](n)
  var plain = false
  if doc.layers.len == 1:
    let l = doc.layers[0]
    plain = l.visible and l.opacity >= 1 and l.fillOpacity >= 1 and l.mask.isNone and l.vectorMask.isNone and
            not hasEffects(l) and l.content.kind == lkRaster and l.content.raster.format.mode == fmt.mode
  if plain:
    let s = doc.layers[0].content.raster
    let ch = s.channels
    let alpha = s.format.alpha
    let data = s.readRegion(canvas)
    var i = 0
    while i + ch <= data.len:
      let a = (if alpha: data[i + ch - 1] else: 1'f32)
      for c in 0 ..< cc: result[c].add data[i + c] * a + white[c] * (1 - a)
      i += ch
  else:
    let buf = flatten(doc).overBackground([1'f32, 1, 1])
    for p in buf.px:
      let v = fromRgba(fmt, p)
      for c in 0 ..< cc: result[c].add v[c]

proc channelSurface(doc: Document; vals: seq[float32]): Surface =
  result = newSurface(pixelFormat(cmGrayscale, doc.depth, false))
  var clamped = newSeq[float32](vals.len)
  for i, v in vals: clamped[i] = clamp(v, 0'f32, 1'f32)
  result.writeRegion(doc.bounds, clamped)
  result.prune()

proc inksOf(doc: Document): seq[(string, Color, seq[float32])] =
  ## The ink channels (name, display colour, densities) of the flattened document.
  let planes = flattenedPlanes(doc)
  proc inv(p: seq[float32]): seq[float32] =
    result = newSeq[float32](p.len)
    for i, v in p: result[i] = 1 - v
  case doc.mode
  of cmRgb:
    for i, (n, c) in [("Cyan", InkCyan), ("Magenta", InkMagenta), ("Yellow", InkYellow)]:
      if i < planes.len: result.add (n, c, inv(planes[i]))
  of cmCmyk:
    for i, (n, c) in [("Cyan", InkCyan), ("Magenta", InkMagenta), ("Yellow", InkYellow), ("Black", InkBlack)]:
      if i < planes.len: result.add (n, c, planes[i])
  of cmLab:
    for k, p in planes: result.add ("Alpha " & $(k + 1), Black, inv(p))
  else:
    if doc.mode == cmDuotone and doc.duotone.isSome and doc.duotone.get.inks.len > 0:
      for ink in doc.duotone.get.inks:
        var d = newSeq[float32](planes[0].len)
        for i, g in planes[0]: d[i] = ink.density(1 - g)
        result.add (ink.name, rgb(ink.color[0], ink.color[1], ink.color[2]), d)
    else: result.add ("Black", Black, inv(planes[0]))

proc toMultichannel(s: var Session; p: JsonNode): JsonNode =
  let names = s.editWith("Multichannel", proc(doc: var Document; active: var Option[LayerId]): seq[string] =
    let inks = inksOf(doc)
    var channels: seq[AlphaChannel]
    for (name, ink, vals) in inks:
      var ch = newAlphaChannel(name, channelSurface(doc, vals))
      ch.spot = some((ink, 0'f32))
      channels.add ch
    # Existing alpha / spot channels follow the inks, converted to the document depth.
    let fmt = pixelFormat(cmGrayscale, doc.depth, false)
    for ch0 in doc.channels:
      var ch = ch0
      if ch.surface.format != fmt: ch.surface = ch.surface.convert(fmt)
      channels.add ch
    doc.channels = channels
    doc.layers.setLen(0)
    active = none(LayerId)
    doc.mode = cmMultichannel
    doc.quickMask = none(AlphaChannel)
    doc.colorTable = none(ColorTable)
    doc.duotone = none(Duotone)
    doc.iccProfile = nil
    for (n, _, _) in inks: result.add n)
  %*{"mode": "multichannel", "channels": names}

proc convertFromMultichannel*(s: var Session; mode: ColorMode; p: JsonNode): JsonNode =
  ## Image › Mode › RGB / CMYK / Lab / Grayscale from a Multichannel document. Profiles are not
  ## involved: Multichannel has none.
  if mode notin {cmRgb, cmCmyk, cmLab, cmGrayscale}:
    raise other("a Multichannel document can't be converted to " & $mode)
  let how = s.editWith("Mode Change", proc(doc: var Document; active: var Option[LayerId]): string =
    var inks, others: seq[AlphaChannel]
    for c in doc.channels:
      if c.spot.isSome: inks.add c else: others.add c
    doc.channels.setLen(0)
    let canvas = doc.bounds
    let n = canvas.width * canvas.height
    let reinterpret = (mode in {cmRgb, cmLab} and inks.len == 3) or (mode == cmCmyk and inks.len == 4) or
                      (mode == cmGrayscale and inks.len == 1)
    # The printed look, rendered before the inks leave the document.
    var shown: Buffer
    if not reinterpret:
      var d = doc
      d.channels = inks
      shown = flatten(d)
    doc.mode = mode
    let fmt = doc.pixelFormat
    let cc = fmt.mode.colorChannels
    var data = newSeq[float32](n * (cc + 1))
    if not reinterpret:
      for i, q in shown.px:
        if i >= n: break
        let v = fromRgba(fmt, [q[0], q[1], q[2], 1'f32])
        for c in 0 .. cc: data[i * (cc + 1) + c] = v[c]
    else:
      for k, ch in inks:
        let vals = ch.surface.readRegion(canvas)
        let step = ch.surface.channels
        var i = 0
        while (i + 1) * step <= vals.len and i < n:
          let v = vals[i * step]
          # CMYK plates are ink amounts already; other modes store light.
          data[i * (cc + 1) + k] = (if mode == cmCmyk: v else: 1 - v)
          inc i
      for i in 0 ..< n: data[i * (cc + 1) + cc] = 1
    var surf = newSurface(fmt)
    surf.writeRegion(canvas, data)
    surf.prune()
    var bg = newLayer("Background", LayerContent(kind: lkRaster, raster: surf))
    bg.locks.transparency = true
    bg.locks.position = true
    active = some(bg.id)
    doc.layers = @[bg]
    doc.channels = others
    (if reinterpret: "channels" else: "appearance"))
  %*{"mode": toLowerAscii($mode), "from": "multichannel", "converted": how}

proc inkNames*(doc: Document): seq[string] =
  ## Names of the ink (spot) channels that make up a Multichannel image.
  for c in doc.channels:
    if c.spot.isSome: result.add c.name

proc multichannelSpecs*(): seq[CommandSpec] =
  @[cmd("image.mode.multichannel", "Multichannel", ["Image", "Mode"], "",
        """{} (flattens; RGB → Cyan/Magenta/Yellow, CMYK → Cyan/Magenta/Yellow/Black, Lab → Alpha 1–3, Grayscale → Black, Duotone → its inks)""",
        multichannelE, runProc(toMultichannel(s, p)))]
