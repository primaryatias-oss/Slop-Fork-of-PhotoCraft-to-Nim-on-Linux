## Image menu remnants (port of `mode_cmds.rs`): Image Rotation › Arbitrary, and the palette / ink
## modes — Mode › Indexed Color, Color Table, Bitmap and Duotone.
##
## Indexed, Bitmap and Duotone documents keep expanded pixels (RGB 8-bit / Gray 8-bit / Gray):
## Indexed and Bitmap quantise the flattened image in place and record the palette or the 1-bit
## look; Duotone keeps the gray pixels and the inks, which display and flat export render
## through. Like Photoshop the conversions flatten (Indexed, Bitmap) and require a grayscale
## source (Bitmap, Duotone). Part of the `engine/core` unit: included, not imported.

proc forEachLayer*(layers: var seq[Layer]; f: proc(l: var Layer))

proc mdNum(p: JsonNode; key: string; default: float32): float32 =
  let v = p.pget(key).asF64
  if v.isSome: float32(v.get) else: default

proc modeIs(s: Session; modes: set[ColorMode]; what: string): string =
  let d = s.active
  if d.isNil: return "no document open"
  if d.doc.mode in modes: "" else: what & " (the document is " & $d.doc.mode & ")"

# ---------------------------------------------------------------- Image Rotation › Arbitrary

proc rotatedSize*(size: Size; deg: float64): Size =
  ## Canvas size after rotating `size` by `deg` (the canvas grows to fit, as in Photoshop).
  let (s, c) = (sin(degToRad(deg)), cos(degToRad(deg)))
  let (w, h) = (float64(size.width), float64(size.height))
  let nw = max(ceil(w * abs(c) + h * abs(s) - 1e-6), 1.0)
  let nh = max(ceil(w * abs(s) + h * abs(c) - 1e-6), 1.0)
  proc satU32(f: float64): int =
    if f != f or f <= 0: 0 elif f >= 4294967295.0: int(high(uint32)) else: int(f)
  Size(width: satU32(nw), height: satU32(nh))

proc withReleasedMovementLocks(l: var Layer; f: proc(l: var Layer)) =
  ## Runs `f` with movement locks (`position` and `all`) released on `l` and its descendants, so a
  ## canvas-wide rotation moves every layer, then restores each lock.
  var saved: seq[Locks]
  var one = @[l]
  forEachLayer(one, proc(x: var Layer) =
    saved.add x.locks
    x.locks.position = false
    x.locks.all = false)
  l = one[0]
  try: f(l)
  finally:
    var i = 0
    one = @[l]
    forEachLayer(one, proc(x: var Layer) =
      if i < saved.len: x.locks = saved[i]
      inc i)
    l = one[0]

proc rotationAffine(old, new: Size; deg: float64): Affine =
  ## Clockwise on screen (y down): x' = c·x − s·y, y' = s·x + c·y about the centres.
  let (sn, cs) = (sin(degToRad(deg)), cos(degToRad(deg)))
  let (cx, cy) = (float64(old.width) / 2, float64(old.height) / 2)
  let (nx, ny) = (float64(new.width) / 2, float64(new.height) / 2)
  Affine(m: [cs, sn, -sn, cs, nx - (cs * cx - sn * cy), ny - (sn * cx + cs * cy)])

proc rotateArbitrary*(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "image.rotation.arbitrary"
  let angle = float64(mdNum(p, "angle", 0))
  if angle != angle or abs(angle) == Inf or abs(angle) > 3600: raise bad(cmdId, "angle out of range")
  let dir = p.getStrOr("direction", "cw")
  let deg = case dir
    of "cw": angle
    of "ccw": -angle
    else: raise bad(cmdId, "direction `" & dir & "` (cw|ccw)")
  if abs(remEuclidF(deg, 360.0)) < 1e-9:
    let d = s.active
    return %*{"width": (if d.isNil: newJNull() else: %d.doc.size.width),
              "height": (if d.isNil: newJNull() else: %d.doc.size.height)}
  let bg = s.tools.background
  # Color Sampler points turn with the canvas (the same map the edit applies to the geometry).
  block:
    let old = s.activeOrRaise().doc.size
    s.setSamplerMap(rotationAffine(old, rotatedSize(old, deg), deg))
  let interp = parseInterp(p.getStrOr("interpolation", "bicubic"))
  let size = s.editGet("Rotate Canvas", Size):
    let old = doc.size
    let new = rotatedSize(old, deg)
    let a = rotationAffine(old, new, deg)
    let h = Homography(m: [a.m[0], a.m[2], a.m[4], a.m[1], a.m[3], a.m[5], 0.0, 0.0, 1.0])
    doc.size = new
    let canvas = doc.bounds
    let fmt = doc.pixelFormat
    let dpi = doc.resolutionDpi
    for l in doc.layers.mitems:
      let background = l.name == "Background" and l.locks.position and l.content.kind == lkRaster
      let surf = if background: l.surfaceMut else: nil
      if not surf.isNil:
        # The Background stays a Background: rotated pixels over the background colour.
        let src = surf[].contentBounds
        let rotated = warpSurfaceE(surf[], src, h, interp)
        var base = newSurface(fmt)
        let fill = fromRgba(fmt, bg)
        let count = canvas.width * canvas.height
        var data = newSeqOfCap[float32](count * fill.len)
        for _ in 0 ..< count: data.add fill
        base.writeRegion(canvas, data)
        compositeOver(base, rotated)
        base.prune()
        surf[] = base
        if l.mask.isSome: l.mask.get.surface = warpGray(l.mask.get.surface, h, interp)
      else:
        # Rotating the whole image moves locked layers too, including inside groups.
        withReleasedMovementLocks(l, proc(x: var Layer) =
          transformLayer(none(Surface), Locks(), x, h, some(a), interp))
      # What Free Transform leaves alone: unlinked vector masks, gradient angles, artboards…
      transformLayerGeometry(l, a, false, dpi)
    transformDocMarks(doc, a)
    for ch in doc.channels.mitems: ch.surface = warpGray(ch.surface, h, interp)
    if doc.quickMask.isSome: doc.quickMask.get.surface = warpGray(doc.quickMask.get.surface, h, interp)
    if doc.selection.isSome:
      let ws = warpGray(doc.selection.get, h, ipBilinear)
      doc.selection = if ws.contentBounds.isEmpty: none(Surface) else: some(ws)
    # Type, shapes and smart objects re-render from their new geometry.
    refreshDoc(doc, rfAll)
    doc.guides = Guides()
    new
  %*{"width": size.width, "height": size.height}

# ---------------------------------------------------------------- flatten helpers

proc modeComposite(doc: Document; opaque: bool): seq[array[4, float32]] =
  ## The flattened image as straight RGBA (over white when `opaque`).
  let buf = flatten(doc)
  if opaque: buf.overBackground([1'f32, 1, 1]).px else: buf.px

proc singleLayer(doc: var Document; active: var Option[LayerId]; mode: ColorMode; px: openArray[array[4, float32]];
                 name: string; background: bool) =
  ## Replace the document's layers by one 8-bit layer holding `px` in `mode`'s storage format.
  doc.mode = mode
  doc.depth = stU8
  let fmt = doc.pixelFormat
  var pixel = newSeq[float32](fmt.channels)
  var data = newSeqOfCap[float32](px.len * fmt.channels)
  for q in px:
    discard fromRgbaInto(fmt, q, pixel)
    data.add pixel
  var l = rasterLayer(name, fmt)
  if background:
    l.locks.transparency = true
    l.locks.position = true
  let surf = l.surfaceMut
  if not surf.isNil: surf[].writeRegion(doc.bounds, data)
  active = some(l.id)
  doc.layers = @[l]
  for ch in doc.channels.mitems:
    ch.surface = ch.surface.convert(PixelFormat(mode: cmGrayscale, sample: stU8, alpha: ch.surface.format.alpha))
  doc.quickMask = none(AlphaChannel)
  doc.colorTable = none(ColorTable)
  doc.duotone = none(Duotone)

# ---------------------------------------------------------------- Indexed Color / Color Table

proc parseForced(s: string): Option[Forced] =
  case s
  of "none": some(fcNone)
  of "blackWhite", "blackAndWhite": some(fcBlackWhite)
  of "primaries": some(fcPrimaries)
  of "web": some(fcWeb)
  else: none(Forced)

proc parseDither(s: string): Option[Dither] =
  case s
  of "none": some(dtNone)
  of "diffusion": some(dtDiffusion)
  of "pattern": some(dtPattern)
  of "noise": some(dtNoise)
  else: none(Dither)

proc indexedColor(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "image.mode.indexedColor"
  let kind = paletteKindFromId(p.getStrOr("palette", "selective"))
  if kind.isNone: raise bad(cmdId, "palette: exact|systemMac|systemWindows|web|uniform|perceptual|selective|adaptive")
  let forced = parseForced(p.getStrOr("forced", "blackWhite"))
  if forced.isNone: raise bad(cmdId, "forced: none|blackWhite|primaries|web")
  let dither = parseDither(p.getStrOr("dither", "diffusion"))
  if dither.isNone: raise bad(cmdId, "dither: none|diffusion|pattern|noise")
  var colors = 256
  let cv = p.pget("colors")
  if not cv.isNil:
    let v = cv.asF64
    if v.isNone: raise bad(cmdId, "colors must be an integer from 2 to 256")
    if v.get != v.get or abs(v.get) == Inf or v.get != trunc(v.get) or v.get < 2 or v.get > 256:
      raise bad(cmdId, "colors must be an integer from 2 to 256")
    colors = int(v.get)
  let amount = clamp(mdNum(p, "amount", 75), 0'f32, 100'f32) / 100
  let transparency = p.getBoolOr("transparency", true)
  let r = s.editGet("Indexed Color", (int, Option[int])):
    var px = modeComposite(doc, not transparency)
    var hasClear = false
    if transparency:
      for q in px:
        if q[3] < 0.5:
          hasClear = true
          break
    var pal: seq[Rgb8]
    try: pal = buildPalette(px, kind.get, (if hasClear: max(max(colors - 1, 0), 2) else: colors), forced.get)
    except QuantizeError as e: raise bad(cmdId, e.msg)
    var transparent = none(int)
    if hasClear and pal.len < 256:
      pal.add [255'u8, 255, 255]
      transparent = some(pal.len - 1)
    let w = int(doc.size.width)
    discard quantize(px, w, pal, dither.get, amount, transparent)
    singleLayer(doc, active, cmIndexed, px, (if transparent.isSome: "Index" else: "Background"), transparent.isNone)
    doc.colorTable = some(ColorTable(colors: pal,
      transparent: if transparent.isSome: some(uint8(transparent.get)) else: none(uint8)))
    (pal.len, transparent)
  %*{"colors": r[0], "transparentIndex": (if r[1].isSome: %r[1].get else: newJNull())}

proc hex3(c: array[3, uint8]): string =
  "#" & toHex(int(c[0]), 2).toLowerAscii & toHex(int(c[1]), 2).toLowerAscii & toHex(int(c[2]), 2).toLowerAscii

proc parseColor8(v: JsonNode): Option[array[3, uint8]] =
  if v.isNil: return
  if v.kind == JString:
    var h = v.str
    var i = 0
    while i < h.len and h[i] == '#': inc i
    h = h[i .. ^1]
    var c: array[3, uint8]
    for k in 0 .. 2:
      if 2 * k + 2 > h.len: return
      let b = hexByte(h[2 * k ..< 2 * k + 2])
      if b.isNone: return
      c[k] = b.get
    return some(c)
  if v.isArray and v.len >= 3:
    var c: array[3, uint8]
    for k in 0 .. 2:
      let x = v.elems[k].asF64
      if x.isNone: return
      c[k] = uint8(clamp(round(x.get), 0.0, 255.0))
    return some(c)

proc presetTable*(name: string): Option[seq[array[3, uint8]]] =
  ## Built-in Color Table presets.
  proc ramp(f: proc(t: float32): array[3, float32]): seq[array[3, uint8]] =
    for i in 0 ..< 256:
      let c = f(float32(i) / 255)
      var o: array[3, uint8]
      for k in 0 .. 2: o[k] = asU8(round(clamp(c[k], 0'f32, 1'f32) * 255))
      result.add o
  case name
  of "grayscale": some(ramp(proc(t: float32): array[3, float32] = [t, t, t]))
  of "blackBody":
    # Black → red → yellow → white, the incandescence scale.
    some(ramp(proc(t: float32): array[3, float32] =
      [min(t * 3, 1'f32), clamp(t * 3 - 1, 0'f32, 1'f32), clamp(t * 3 - 2, 0'f32, 1'f32)]))
  of "spectrum":
    some(ramp(proc(t: float32): array[3, float32] =
      let h = t * 300 / 60
      let x = 1 - abs(h mod 2'f32 - 1)
      case asI32(h)
      of 0: [1'f32, x, 0]
      of 1: [x, 1'f32, 0]
      of 2: [0'f32, 1, x]
      of 3: [0'f32, x, 1]
      else: [x, 0'f32, 1]))
  of "systemMac", "system": some(macPalette())
  of "systemWindows": some(windowsPalette())
  of "web": some(webPalette())
  else: none(seq[array[3, uint8]])

proc colorTableCmd(s: var Session; p: JsonNode): JsonNode =
  ## Image › Mode › Color Table: replace entries (pixels keep their index, so their colour follows
  ## the table) and set the transparent entry. No changes: report the table.
  const cmdId = "image.mode.colorTable"
  let st = s.activeOrRaise()
  if st.doc.colorTable.isNone: raise other("the document has no color table")
  let cur = st.doc.colorTable.get
  var table = cur
  let tname = p.getStrOr("table", "custom")
  if tname != "custom":
    let t = presetTable(tname)
    if t.isNone: raise bad(cmdId, "unknown table `" & tname & "`")
    table.colors = t.get
  let cv = p.pget("colors")
  if cv.isArray:
    var c: seq[array[3, uint8]]
    for v in cv.elems:
      let x = parseColor8(v)
      if x.isNone: raise bad(cmdId, "colors: [\"#rrggbb\" | [r, g, b], …]")
      c.add x.get
    if c.len == 0 or c.len > 256: raise bad(cmdId, "a color table has 1 to 256 entries")
    table.colors = c
  let ev = p.pget("entries")
  if ev.isObject:
    for k, v in ev.sortedPairs:
      var i: int
      try:
        if k.len == 0 or not k.allCharsInSet(Digits): raise newException(ValueError, "")
        i = parseInt(k)
      except ValueError: raise bad(cmdId, "entry index `" & k & "`")
      let c = parseColor8(v)
      if c.isNone: raise bad(cmdId, "entry " & $i & ": bad colour")
      if i >= table.colors.len:
        raise bad(cmdId, "entry " & $i & " is past the table (" & $table.colors.len & " entries)")
      table.colors[i] = c.get
  let tv = p.pget("transparent")
  if not tv.isNil:
    if tv.kind == JNull: table.transparent = none(uint8)
    else:
      let u = tv.asU64
      if u.isSome and u.get < uint64(table.colors.len): table.transparent = some(uint8(u.get))
  proc report(t: ColorTable): JsonNode =
    var cs = newJArray()
    for c in t.colors: cs.add %hex3(c)
    %*{"colors": cs, "transparent": (if t.transparent.isSome: %t.transparent.get else: newJNull())}
  if table == cur: return report(cur)
  let outp = report(table)
  s.editDo("Color Table"):
    let old = cur
    for l in doc.layers.mitems:
      let surf = l.surfaceMut
      if surf.isNil: continue
      let r = surf[].contentBounds
      if r.isEmpty: continue
      let fmt = surf[].format
      let n = fmt.channels
      var data = surf[].readRegion(r)
      var enc: array[8, float32]
      var k = 0
      while k + n <= data.len:
        let c = toRgba(fmt, data.toOpenArray(k, k + n - 1))
        let rgb = [c[0], c[1], c[2]]
        var i: int
        var skip = false
        if c[3] <= 0:
          if old.transparent.isNone: skip = true
          else:
            i = int(old.transparent.get)
            if i >= old.colors.len or old.rgb(i) != rgb: skip = true
        else: i = old.nearest(rgb)
        if not skip:
          let alpha = if table.transparent == some(uint8(i)): 0'f32 elif c[3] <= 0: 1'f32 else: c[3]
          let nc = if i < table.colors.len: table.rgb(i) else: table.rgb(table.nearest([c[0], c[1], c[2]]))
          let m = fromRgbaInto(fmt, [nc[0], nc[1], nc[2], alpha], enc)
          for j in 0 ..< m: data[k + j] = enc[j]
        k += n
      surf[].writeRegion(r, data)
      surf[].prune()
    doc.colorTable = some(table)
  outp

# ---------------------------------------------------------------- Bitmap

proc bitmapCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "image.mode.bitmap"
  let dpi = max(s.activeOrRaise().doc.resolutionDpi, 1'f32)
  var meth: BitmapMethod
  let m = p.getStrOr("method", "diffusion")
  case m
  of "threshold", "threshold50": meth = BitmapMethod(kind: bmThreshold)
  of "pattern": meth = BitmapMethod(kind: bmPattern)
  of "diffusion": meth = BitmapMethod(kind: bmDiffusion)
  of "halftone":
    let freq = clamp(mdNum(p, "frequency", 53), 1'f32, 999'f32)
    let shape = halftoneShapeFromId(p.getStrOr("shape", "round"))
    if shape.isNone: raise bad(cmdId, "shape: round|ellipse|line|square|diamond|cross")
    meth = BitmapMethod(kind: bmHalftone, cell: max(dpi / freq, 2'f32), angle: mdNum(p, "angle", 45), shape: shape.get)
  else: raise bad(cmdId, "method `" & m & "` (threshold|pattern|diffusion|halftone)")
  s.editDo("Bitmap"):
    let px = modeComposite(doc, true)
    var gray = newSeq[float32](px.len)
    for i, q in px: gray[i] = rgbToGray([q[0], q[1], q[2]])
    toBitmap(gray, int(doc.size.width), meth)
    var outp = newSeq[array[4, float32]](gray.len)
    for i, g in gray: outp[i] = [g, g, g, 1'f32]
    singleLayer(doc, active, cmBitmap, outp, "Background", true)
  jnull()

# ---------------------------------------------------------------- Duotone

proc defaultInks(n: int): seq[DuotoneInk] =
  ## Default inks per type: black first, then warm brown, gold and slate blue.
  let all = [("Black", [0'f32, 0, 0]), ("Warm Brown", [0.62'f32, 0.38, 0.18]), ("Gold", [0.86'f32, 0.68, 0.18]),
             ("Slate Blue", [0.27'f32, 0.38, 0.58])]
  for i in 0 ..< min(n, all.len): result.add newDuotoneInk(all[i][0], all[i][1])

proc parseInks(cmdId: string; v: JsonNode; n: int): seq[DuotoneInk] =
  if not v.isArray: raise bad(cmdId, "inks: [{\"name\",\"color\":\"#rrggbb\",\"curve\":[[in,out],…] (0..100)}, …]")
  let defaults = defaultInks(min(max(n, v.len), 4))
  for i in 0 ..< min(v.len, 4):
    let ink = v.elems[i]
    var d = if i < defaults.len: defaults[i] else: newDuotoneInk("Ink " & $(i + 1), [0'f32, 0, 0])
    let name = ink.pget("name")
    if not name.isNil and name.kind == JString: d.name = name.str
    let c = parseColor8(ink.pget("color"))
    if c.isSome: d.color = [float32(c.get[0]) / 255, float32(c.get[1]) / 255, float32(c.get[2]) / 255]
    let pts = ink.pget("curve")
    if pts.isArray:
      var curve: seq[CurvePoint]
      for q in pts.elems:
        let a = jIdx(q, 0).asF64
        let b = jIdx(q, 1).asF64
        if a.isSome and b.isSome: curve.add CurvePoint(input: float32(a.get) / 100, output: float32(b.get) / 100)
      curve.sort(proc(a, b: CurvePoint): int = cmp(a.input, b.input))
      if curve.len >= 2: d.curve = curve
    result.add d

proc duotoneCmd(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "image.mode.duotone"
  let t = p.getStrOr("type", "duotone")
  let n = case t
    of "monotone": 1
    of "duotone": 2
    of "tritone": 3
    of "quadtone": 4
    else: raise bad(cmdId, "type `" & t & "` (monotone|duotone|tritone|quadtone)")
  let iv = p.pget("inks")
  var inks = if iv.isNil: defaultInks(n) else: parseInks(cmdId, iv, n)
  if inks.len < n:
    let d = defaultInks(n)
    for i in inks.len ..< d.len: inks.add d[i]
  if inks.len > n: inks.setLen(n)
  var names = newJArray()
  for i in inks: names.add %i.name
  s.editDo("Duotone"):
    doc.mode = cmDuotone
    doc.duotone = some(Duotone(inks: inks, psdRaw: none(seq[byte])))
    doc.colorTable = none(ColorTable)
  %*{"inks": names}

proc modeSpecs*(): seq[CommandSpec] =
  @[
    cmd("image.rotation.arbitrary", "Arbitrary…", ["Image", "Image Rotation"], "",
        """{"angle":-359.99..359.99=0,"direction":"cw|ccw"="cw","interpolation":"bicubic|bilinear|nearest"="bicubic"}""",
        hasDocE, rotateArbitrary),
    cmd("image.mode.indexedColor", "Indexed Color…", ["Image", "Mode"], "",
        """{"palette":"selective|perceptual|adaptive|exact|systemMac|systemWindows|web|uniform"="selective","colors":2..256=256,"forced":"blackWhite|none|primaries|web"="blackWhite","transparency":bool=true,"dither":"diffusion|none|pattern|noise"="diffusion","amount":0..100=75} (flattens)""",
        proc(s: Session): string = modeIs(s, {cmRgb, cmGrayscale, cmIndexed, cmDuotone, cmCmyk, cmLab}, "Indexed Color needs an RGB or Grayscale image"),
        indexedColor),
    cmd("image.mode.colorTable", "Color Table…", ["Image", "Mode"], "",
        """{"table":"custom|blackBody|grayscale|spectrum|systemMac|systemWindows|web"="custom","colors":json,"entries":json,"transparent":json} (colors: ["#rrggbb", …]; entries: {"index": "#rrggbb"}; transparent: index|null)""",
        proc(s: Session): string = modeIs(s, {cmIndexed}, "Color Table needs an Indexed Color image"), colorTableCmd),
    cmd("image.mode.bitmap", "Bitmap…", ["Image", "Mode"], "",
        """{"method":"diffusion|threshold|pattern|halftone"="diffusion","frequency":1..999=53,"angle":-180..180=45,"shape":"round|ellipse|line|square|diamond|cross"="round"} (halftone frequency in lines/inch; flattens)""",
        proc(s: Session): string = modeIs(s, {cmGrayscale}, "Bitmap needs a Grayscale image (Image › Mode › Grayscale first)"),
        bitmapCmd),
    cmd("image.mode.duotone", "Duotone…", ["Image", "Mode"], "",
        """{"type":"duotone|monotone|tritone|quadtone"="duotone","inks":json} (inks: [{"name","color":"#rrggbb","curve":[[in,out],…] in 0..100}, …])""",
        proc(s: Session): string = modeIs(s, {cmGrayscale, cmDuotone}, "Duotone needs a Grayscale image (Image › Mode › Grayscale first)"),
        duotoneCmd),
  ]

proc duotoneDisplayLayer*(d: Duotone): Layer =
  ## The gradient-map layer that renders Duotone inks over the gray composite.
  newLayer("Duotone", adjustmentContent(d.displayAdjustment))

proc displayDocument*(doc: Document): Option[Document] =
  ## A copy of `doc` that displays like the printed result (Duotone inks), or none when the
  ## document displays as stored.
  if doc.mode != cmDuotone or doc.duotone.isNone: return
  var outp = doc
  outp.layers.add duotoneDisplayLayer(doc.duotone.get)
  some(outp)
