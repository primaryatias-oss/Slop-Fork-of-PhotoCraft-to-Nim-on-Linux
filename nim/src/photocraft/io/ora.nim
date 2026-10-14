## OpenRaster (`.ora`, Krita/MyPaint/GIMP): a zip of PNG layers plus `stack.xml` (Rust `ora.rs`).
## Import maps `stack`/`layer` elements to groups and raster layers with SVG/Krita composite
## operations; export writes every layer as pixels with the merged image and a thumbnail.

import std/[options, strutils, math, xmlparser, xmltree, strtabs, unicode, streams]
import ../geom, ../color, ../raster, ../doc, ../compose, ../format, ../codecs
import common

const
  Mimetype* = "image/openraster"
  MaxPixelBytes* = 1 shl 30
  MaxSide* = 300_000
  MaxNodes* = 8192
  MaxStackXml = 16 shl 20
  MaxEntry = 512 shl 20
  ThumbnailSide = 256

type
  Warnings* = object
    general: seq[string]
    perLayer: seq[(string, seq[string])]

  NodeKind = enum nkRaster, nkGroup
  OraNode = object
    layer: Layer
    case kind: NodeKind
    of nkRaster:
      image: int     ## index into the decoded images, -1 when none
      x, y: int
    of nkGroup:
      children: seq[OraNode]

  OraReader = object
    zip: ZipReader
    ctl: Interrupt
    nodes, budget: int
    warnings: Warnings
    images: seq[Image]

proc invalid(msg: string): ref IoFailure = ioError(ieOra, msg)

proc zipMimetype*(bytes: openArray[byte]): Option[string] =
  template u16at(o: int): int =
    (if o + 2 <= bytes.len: int(bytes[o]) or (int(bytes[o + 1]) shl 8) else: -1)
  template u32at(o: int): int64 =
    (if o + 4 > bytes.len: -1'i64
     else: int64(bytes[o]) or (int64(bytes[o + 1]) shl 8) or (int64(bytes[o + 2]) shl 16) or (int64(bytes[o + 3]) shl 24))
  if u32at(0) != 0x04034b50 or u16at(8) != 0: return
  let size = u32at(18)
  let nameLen = u16at(26)
  let extraLen = u16at(28)
  if size < 0 or nameLen < 0 or extraLen < 0 or size > 256: return
  if 30 + nameLen > bytes.len: return
  var name = ""
  for i in 0 ..< nameLen: name.add char(bytes[30 + i])
  if name != "mimetype": return
  let start = 30 + nameLen + extraLen
  if start + int(size) > bytes.len: return
  var s = ""
  for i in 0 ..< int(size): s.add char(bytes[start + i])
  some(s)

proc isOra*(bytes: openArray[byte]): bool = zipMimetype(bytes) == some(Mimetype)

proc kritaBlend*(id: string): Option[(BlendMode, bool)] =
  case id
  of "normal": some((bmNormal, false))
  of "multiply": some((bmMultiply, false))
  of "screen": some((bmScreen, false))
  of "overlay": some((bmOverlay, false))
  of "darken": some((bmDarken, false))
  of "lighten": some((bmLighten, false))
  of "dodge": some((bmColorDodge, false))
  of "burn": some((bmColorBurn, false))
  of "hard_light": some((bmHardLight, false))
  of "soft_light": some((bmSoftLight, false))
  of "soft_light_svg": some((bmSoftLight, true))
  of "diff": some((bmDifference, false))
  of "exclusion": some((bmExclusion, false))
  of "linear_burn": some((bmLinearBurn, false))
  of "linear_dodge", "add": some((bmLinearDodge, false))
  of "subtract": some((bmSubtract, false))
  of "divide": some((bmDivide, false))
  of "vivid_light": some((bmVividLight, false))
  of "linear light": some((bmLinearLight, false))
  of "pin_light": some((bmPinLight, false))
  of "hard_mix_photoshop": some((bmHardMix, false))
  of "hard mix": some((bmHardMix, true))
  of "darker color": some((bmDarkerColor, false))
  of "lighter color": some((bmLighterColor, false))
  of "hue": some((bmHue, false))
  of "saturation": some((bmSaturation, false))
  of "color": some((bmColor, false))
  of "luminize": some((bmLuminosity, false))
  of "dissolve": some((bmDissolve, false))
  of "reflect": some((bmReflect, false))
  of "glow": some((bmGlow, false))
  of "negation": some((bmNegation, false))
  of "xor": some((bmXor, true))
  else: none((BlendMode, bool))

proc kritaId*(mode: BlendMode): Option[string] =
  case mode
  of bmNormal, bmPassThrough: some("normal")
  of bmDissolve: some("dissolve")
  of bmDarken: some("darken")
  of bmMultiply: some("multiply")
  of bmColorBurn: some("burn")
  of bmLinearBurn: some("linear_burn")
  of bmDarkerColor: some("darker color")
  of bmLighten: some("lighten")
  of bmScreen: some("screen")
  of bmColorDodge: some("dodge")
  of bmLinearDodge: some("linear_dodge")
  of bmLighterColor: some("lighter color")
  of bmOverlay: some("overlay")
  of bmSoftLight: some("soft_light")
  of bmHardLight: some("hard_light")
  of bmVividLight: some("vivid_light")
  of bmLinearLight: some("linear light")
  of bmPinLight: some("pin_light")
  of bmHardMix: some("hard_mix_photoshop")
  of bmDifference: some("diff")
  of bmExclusion: some("exclusion")
  of bmSubtract: some("subtract")
  of bmDivide: some("divide")
  of bmHue: some("hue")
  of bmSaturation: some("saturation")
  of bmColor: some("color")
  of bmLuminosity: some("luminize")
  of bmReflect: some("reflect")
  of bmGlow: some("glow")
  of bmNegation: some("negation")
  else: none(string)

const SvgOps: array[15, (string, BlendMode)] = [
  ("svg:src-over", bmNormal), ("svg:multiply", bmMultiply), ("svg:screen", bmScreen),
  ("svg:overlay", bmOverlay), ("svg:darken", bmDarken), ("svg:lighten", bmLighten),
  ("svg:color-dodge", bmColorDodge), ("svg:color-burn", bmColorBurn), ("svg:hard-light", bmHardLight),
  ("svg:soft-light", bmSoftLight), ("svg:difference", bmDifference), ("svg:color", bmColor),
  ("svg:luminosity", bmLuminosity), ("svg:hue", bmHue), ("svg:saturation", bmSaturation)]

proc note*(w: var Warnings; message: string) =
  if message notin w.general: w.general.add message

proc layer*(w: var Warnings; layer, message: string) =
  for e in w.perLayer.mitems:
    if e[0] == message:
      if layer notin e[1]: e[1].add layer
      return
  w.perLayer.add (message, @[layer])

proc finish*(w: Warnings): seq[string] =
  result = w.general
  for (message, layers) in w.perLayer:
    var shown: seq[string]
    for i in 0 ..< min(layers.len, 5): shown.add "\"" & layers[i] & "\""
    let more = max(layers.len - 5, 0)
    let tail = if more > 0: " and " & $more & " more" else: ""
    result.add message & " (" & shown.join(", ") & tail & ")"

proc compositeOp(op, layer: string; warnings: var Warnings): BlendMode =
  for (name, mode) in SvgOps:
    if name == op:
      if op == "svg:soft-light":
        warnings.note("svg:soft-light (the W3C curve) opens as Photoshop's Soft Light, which differs slightly")
      return mode
  if op == "svg:plus": return bmLinearDodge
  if op == "svg:exclusion": return bmExclusion
  if op.startsWith("krita:"):
    let k = kritaBlend(op[6 .. ^1])
    if k.isSome:
      if k.get[1]: warnings.note("Krita's " & op & " blend mode is approximated by " & k.get[0].label)
      return k.get[0]
  warnings.layer(layer, "composite operation " & op & " is not supported; it opens as Normal")
  bmNormal

proc localName*(n: XmlNode): string =
  ## The tag without its namespace prefix (`svg:rect` → `rect`).
  let t = n.tag
  let c = t.find(':')
  if c >= 0: t[c + 1 .. ^1] else: t

proc attr(n: XmlNode; name: string): Option[string] =
  if n.attrs.isNil or not n.attrs.hasKey(name): none(string) else: some(n.attrs[name])

proc num(n: XmlNode; name: string; default: float64): float64 =
  let a = n.attr(name)
  if a.isNone: return default
  try:
    let v = parseFloat(a.get.strip)
    if classify(v) in {fcInf, fcNegInf, fcNan}: raise newException(ValueError, "")
    v
  except ValueError: raise invalid("invalid " & name & " attribute `" & a.get & "`")

proc intAttr(n: XmlNode; name: string): int =
  let v = round(num(n, name, 0))
  if abs(v) > float64(MaxSide) * 4: raise invalid(name & " offset " & rustFloat(v) & " is out of range")
  int(v)

proc flag(n: XmlNode; name: string): bool =
  let a = n.attr(name)
  a.isSome and a.get.strip in ["true", "1"]

proc props(n: XmlNode; layer: var Layer; index: int; warnings: var Warnings) =
  let nm = n.attr("name")
  layer.name = if nm.isSome and nm.get.strip.len > 0: nm.get else: "Layer " & $index
  let vis = n.attr("visibility")
  layer.visible = vis.isNone or vis.get.strip != "hidden"
  layer.opacity = float32(clamp(num(n, "opacity", 1), 0.0, 1.0))
  let op = n.attr("composite-op")
  layer.blend = compositeOp(if op.isSome: op.get.strip else: "svg:src-over", layer.name, warnings)
  if flag(n, "edit-locked"): layer.locks.all = true
  if flag(n, "alpha-preserve"): layer.locks.transparency = true

proc png(r: var OraReader; src, layer: string): int =
  let i = r.zip.find(src)
  if i < 0:
    r.warnings.layer(layer, "the layer's image is missing from the archive; the layer opens empty")
    return -1
  var data: seq[byte]
  try: data = r.zip.read(r.zip.entries[i], MaxEntry)
  except CatchableError as e: raise invalid(src & ": " & e.msg)
  var opts = defaultDecodeOptions()
  opts.limits.maxWidth = MaxSide
  opts.limits.maxHeight = MaxSide
  opts.limits.maxAlloc = int64(r.budget)
  var image: Image
  try: image = decode(data, opts)
  except CodecError as e: raise invalid(src & ": " & e.msg)
  let rgba16 = image.width * image.height * 8
  if rgba16 > r.budget: raise invalid("decoded layers exceed the 1 GiB limit")
  r.budget -= rgba16
  if detect(data) != some(fmtPng):
    r.warnings.note("some layer images are not PNG, as OpenRaster expects; they were read anyway")
  r.images.add image
  r.images.len - 1

proc stack(r: var OraReader; s: XmlNode; depth: int): seq[OraNode] =
  if depth > MaxGroupDepth:
    raise ioError(ieUnsupported, "layer groups nested deeper than " & $MaxGroupDepth)
  for child in s:
    if child.kind != xnElement: continue
    if r.ctl.cancelled: raise ioError(ieCancelled)
    inc r.nodes
    if r.nodes > MaxNodes: raise invalid("more than " & $MaxNodes & " layers")
    let index = r.nodes
    case child.localName
    of "layer":
      var layer = rasterLayer("", pixelFormat(cmRgb, stU8, true))
      props(child, layer, index, r.warnings)
      let (x, y) = (intAttr(child, "x"), intAttr(child, "y"))
      var image = -1
      let src = child.attr("src")
      if src.isSome: image = r.png(src.get.strip, layer.name)
      else: r.warnings.layer(layer.name, "the layer has no image; it opens empty")
      result.add OraNode(kind: nkRaster, layer: layer, image: image, x: x, y: y)
    of "stack":
      var layer = groupLayer("", @[])
      props(child, layer, index, r.warnings)
      let iso = child.attr("isolation")
      let isolate = iso.isSome and iso.get.strip == "isolate"
      if not isolate and layer.blend == bmNormal: layer.blend = bmPassThrough
      let children = r.stack(child, depth + 1)
      result.add OraNode(kind: nkGroup, layer: layer, children: children)
    else:
      let nm = child.attr("name")
      let name = if nm.isSome: nm.get else: child.localName
      r.warnings.layer(name, "OpenRaster <" & child.localName & "> elements are not supported and were skipped")

proc documentDepth(images: openArray[Image]): SampleType =
  result = stU8
  for i in images:
    if result == stF32 or i.sample in {isF32, isF16}: result = stF32
    elif result == stU16 or i.sample == isU16: result = stU16

proc csampleOf(depth: SampleType): ImgSample =
  case depth
  of stU8: isU8
  of stU16: isU16
  of stF32: isF32

proc build(nodes: seq[OraNode]; images: seq[Image]; fmt: PixelFormat; ctl: Interrupt): seq[Layer] =
  for i in countdown(nodes.len - 1, 0):
    if ctl.cancelled: raise ioError(ieCancelled)
    let node = nodes[i]
    var layer = node.layer
    case node.kind
    of nkRaster:
      var surface = newSurface(fmt)
      if node.image >= 0 and node.image < images.len:
        let image = images[node.image]
        let (w, h) = (image.width, image.height)
        let px = image.convert(clRgba, csampleOf(fmt.sample))
        let r = rect(node.x, node.y, satAdd(node.x, w), satAdd(node.y, h))
        if r.width != w or r.height != h or px.data.len != w * h * fmt.bytesPerPixel:
          raise invalid("layer image does not fit its position")
        surface.writeInterleaved(r, px.data)
        surface.prune()
      layer.content = rasterContent(surface)
    of nkGroup:
      layer.content.group.children = build(node.children, images, fmt, ctl)
    result.add layer

proc importOra*(name: string; bytes: openArray[byte]; ctl: Interrupt): ImportResult =
  var zip: ZipReader
  try: zip = newZipReader(@bytes)
  except CatchableError as e: raise invalid(e.msg)
  var mime, xmlBytes: seq[byte]
  try: mime = zip.readByName("mimetype", 256)
  except CatchableError as e: raise invalid(e.msg)
  if cast[string](mime).strip != Mimetype: raise invalid("the archive's mimetype is not image/openraster")
  try: xmlBytes = zip.readByName("stack.xml", MaxStackXml)
  except CatchableError as e: raise invalid(e.msg)
  let xml = cast[string](xmlBytes)
  if validateUtf8(xml) >= 0: raise invalid("stack.xml is not UTF-8")
  var errors: seq[string]
  var root: XmlNode
  try: root = parseXml(newStringStream(xml), "stack.xml", errors)
  except CatchableError as e: raise invalid("stack.xml: " & e.msg)
  if errors.len > 0: raise invalid("stack.xml: " & errors[0])
  if root.isNil or root.kind != xnElement or root.localName != "image": raise invalid("stack.xml has no <image> root")
  proc side(a: string): int =
    let v = num(root, a, 0)
    if not (v >= 1 and v <= float64(MaxSide)) or v != floor(v):
      raise invalid("image " & a & " " & rustFloat(v) & " must be a whole number of pixels from 1 to " & $MaxSide)
    int(v)
  let (w, h) = (side("w"), side("h"))
  if w * h * 8 > MaxPixelBytes: raise invalid("the canvas exceeds the 1 GiB pixel limit")
  var rootStack: XmlNode = nil
  for c in root:
    if c.kind == xnElement and c.localName == "stack":
      rootStack = c
      break
  if rootStack.isNil: raise invalid("stack.xml has no root <stack>")
  var r = OraReader(zip: zip, ctl: ctl, budget: MaxPixelBytes)
  let nodes = r.stack(rootStack, 1)
  ctl.progress(0.8)
  let depth = documentDepth(r.images)
  if depth == stF32: r.warnings.note("float layer images open as a 32-bit float document")
  let fmt = pixelFormat(cmRgb, depth, true)
  var d = newDocument(name, size(w, h), cmRgb, depth)
  d.layers = build(nodes, r.images, fmt, ctl)
  let xres = num(root, "xres", 72)
  let yres = num(root, "yres", xres)
  if xres > 0 and xres < 1.0e6:
    d.resolutionDpi = float32(xres)
    let uw = unequalResolutionWarning(xres, yres)
    if uw.isSome: r.warnings.general.add uw.get
  if d.layers.len == 0: r.warnings.note("the OpenRaster file has no layers")
  ImportResult(document: d, warnings: r.warnings.finish)

# ---------------------------------------------------------------- export

proc escapeXml(s: string): string =
  for r in s.runes:
    let c = int(r)
    case c
    of ord('&'): result.add "&amp;"
    of ord('<'): result.add "&lt;"
    of ord('>'): result.add "&gt;"
    of ord('"'): result.add "&quot;"
    of ord('\''): result.add "&apos;"
    of 9: result.add "&#9;"
    of 10: result.add "&#10;"
    else:
      if c < 32 or (c >= 127 and c < 160): discard
      else: result.add $r

proc opName(mode: BlendMode): Option[string] =
  case mode
  of bmPassThrough: return some("svg:src-over")
  of bmLinearDodge: return some("svg:plus")
  else: discard
  for (n, m) in SvgOps:
    if m == mode: return some(n)
  let k = kritaId(mode)
  if k.isSome: some("krita:" & k.get) else: none(string)

proc encodeRgba(px: openArray[Px]; sample: ImgSample): seq[byte] =
  for p in px:
    for v in p:
      if sample == isU16:
        let u = uint16(clamp(v, 0'f32, 1'f32) * 65535 + 0.5)
        result.add cast[array[2, byte]](u)
      else:
        result.add uint8(clamp(v, 0'f32, 1'f32) * 255 + 0.5)

proc pngBytes(w, h: int; px: openArray[Px]; sample: ImgSample): seq[byte] =
  var opts = defaultEncodeOptions()
  opts.embedIcc = false
  try: encode(imageFromRaw(w, h, clRgba, sample, encodeRgba(px, sample)), fmtPng, opts)
  except CodecError as e: raise ioError(ieCodec, e.msg)

proc cropBuf(r: Rect; px0: seq[Px]): (Rect, seq[Px]) =
  var px = px0
  let w = r.width
  if w == 0: return (EmptyRect, @[])
  var (x0, y0, x1, y1) = (high(int), high(int), 0, 0)
  for i in 0 ..< px.len:
    if px[i][3] <= 0:
      px[i] = [0'f32, 0, 0, 0]
      continue
    let (x, y) = (i mod w, i div w)
    x0 = min(x0, x); y0 = min(y0, y); x1 = max(x1, x + 1); y1 = max(y1, y + 1)
  if x0 >= x1: return (EmptyRect, @[])
  if x0 == 0 and y0 == 0 and x1 == w and y1 == px.len div w: return (r, px)
  var o: seq[Px]
  for y in y0 ..< y1:
    for x in x0 ..< x1: o.add px[y * w + x]
  (rect(r.x0 + x0, r.y0 + y0, r.x0 + x1, r.y0 + y1), o)

type OraWriter = object
  zip: ZipWriter
  xml: string
  next: int
  sample: ImgSample
  warnings: Warnings
  canvas: Rect

proc common(wr: var OraWriter; layer: Layer): string =
  let opacity = if layer.content.kind == lkGroup: clamp(layer.opacity * layer.fillOpacity, 0'f32, 1'f32)
                else: clamp(layer.opacity, 0'f32, 1'f32)
  result = "name=\"" & escapeXml(layer.name) & "\" visibility=\"" & (if layer.visible: "visible" else: "hidden") &
           "\" opacity=\"" & rustFloat(opacity) & "\""
  var op = opName(layer.blend)
  if op.isNone:
    wr.warnings.layer(layer.name, "the " & layer.blend.label & " blend mode has no OpenRaster equivalent; it was saved as Normal")
    op = some("svg:src-over")
  result.add " composite-op=\"" & op.get & "\""
  if layer.locks.all or layer.locks.pixels: result.add " edit-locked=\"true\""
  if layer.locks.transparency: result.add " alpha-preserve=\"true\""
  if layer.clipped: wr.warnings.layer(layer.name, "OpenRaster has no clipping masks; these layers were saved unclipped")
  if layer.effects.items.len > 0 or not layer.effects.psdRaw.isNil:
    wr.warnings.layer(layer.name, "layer styles are not saved to OpenRaster")
  if layer.vectorMask.isSome and layer.vectorMask.get.enabled:
    wr.warnings.layer(layer.name, "vector masks are not saved to OpenRaster")
  if layer.fillOpacity < 1:
    if layer.content.kind == lkGroup: wr.warnings.layer(layer.name, "fill opacity was combined into the layer opacity")
    else: wr.warnings.layer(layer.name, "fill opacity was applied to the saved layer pixels")

proc pixels(wr: var OraWriter; layer: Layer): (Rect, seq[Px]) =
  var r: Rect
  var buf: seq[Px]
  if layer.content.kind == lkRaster:
    r = layer.content.raster.contentBounds
    buf = surfaceToBuffer(layer.content.raster, r).px
  else:
    var solo = layer
    solo.visible = true
    solo.opacity = 1
    solo.fillOpacity = 1
    solo.blend = bmNormal
    solo.clipped = false
    solo.mask = none(doc.LayerMask)
    solo.effects = newEffects()
    solo.effects.enabled = false
    let cb = compositeBounds(solo, wr.canvas)
    r = (if cb.isSome: cb.get else: wr.canvas).intersect(wr.canvas)
    wr.warnings.layer(layer.name, layer.content.kindName.toLowerAscii & " layers are saved to OpenRaster as pixels")
    buf = renderLayer(solo, r).px
  if layer.mask.isSome and layer.mask.get.enabled:
    let m = layer.mask.get
    if m.feather > 0: wr.warnings.layer(layer.name, "layer mask feathering is not applied in OpenRaster")
    wr.warnings.layer(layer.name, "layer masks were applied to the layer pixels")
    var values: seq[float32]
    m.valuesInto(r, values)
    for i in 0 ..< min(buf.len, values.len): buf[i][3] *= values[i]
  if layer.fillOpacity < 1:
    for p in buf.mitems: p[3] *= clamp(layer.fillOpacity, 0'f32, 1'f32)
  cropBuf(r, buf)

proc writeLayers(wr: var OraWriter; layers: openArray[Layer]; depth: int) =
  if depth > MaxGroupDepth: raise ioError(ieUnsupported, "layer groups nested deeper than " & $MaxGroupDepth)
  for i in countdown(layers.len - 1, 0):
    let layer = layers[i]
    case layer.content.kind
    of lkGroup:
      let attrs = wr.common(layer)
      let isolation = if layer.blend == bmPassThrough: "auto" else: "isolate"
      if layer.mask.isSome and layer.mask.get.enabled:
        wr.warnings.layer(layer.name, "group masks are not saved to OpenRaster")
      wr.xml.add "<stack " & attrs & " isolation=\"" & isolation & "\">"
      wr.writeLayers(layer.content.group.children, depth + 1)
      wr.xml.add "</stack>"
    of lkAdjustment:
      wr.warnings.layer(layer.name, "adjustment layers cannot be saved to OpenRaster and were left out (the merged image keeps their look)")
    else:
      let attrs = wr.common(layer)
      var (r, px) = wr.pixels(layer)
      if r.isEmpty: (r, px) = (rectXywh(0, 0, 1, 1), @[[0'f32, 0, 0, 0]])
      let src = "data/layer" & $wr.next & ".png"
      inc wr.next
      try: wr.zip.add(src, pngBytes(r.width, r.height, px, wr.sample))
      except FormatError as e: raise ioError(iePcraft, e.msg)
      wr.xml.add "<layer " & attrs & " src=\"" & src & "\" x=\"" & $r.x0 & "\" y=\"" & $r.y0 & "\"/>"

proc fmtRes(v: float32): string = rustFloat(max(round(v), 1'f32))

proc exportOra*(d: Document): ExportResult =
  if d.mode notin {cmRgb, cmGrayscale}:
    raise ioError(ieUnsupported, "OpenRaster holds RGB layers; convert the document to RGB or Grayscale first")
  let sample = if d.depth == stU8: isU8 else: isU16
  var wr = OraWriter(zip: newZipWriter(), sample: sample, canvas: d.bounds)
  try: wr.zip.add("mimetype", cast[seq[byte]](Mimetype))
  except FormatError as e: raise ioError(iePcraft, e.msg)
  if d.depth == stF32: wr.warnings.note("32-bit float layers are saved as 16-bit PNGs")
  wr.xml.add "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<image version=\"0.0.6\" w=\"" & $d.size.width & "\" h=\"" &
    $d.size.height & "\" xres=\"" & fmtRes(d.resolutionDpi) & "\" yres=\"" & fmtRes(d.resolutionDpi) & "\"><stack>"
  wr.writeLayers(d.layers, 1)
  wr.xml.add "</stack></image>\n"
  try:
    wr.zip.add("stack.xml", cast[seq[byte]](wr.xml))
    let merged = flatten(d)
    wr.zip.add("mergedimage.png", pngBytes(d.size.width, d.size.height, merged.px, sample))
    let thumb = thumbnail(d, ThumbnailSide)
    wr.zip.add("Thumbnails/thumbnail.png",
               encode(imageFromRaw(thumb.width, thumb.height, clRgba, isU8, thumb.pixels), fmtPng, defaultEncodeOptions()))
  except FormatError as e: raise ioError(iePcraft, e.msg)
  except CodecError as e: raise ioError(ieCodec, e.msg)
  if d.channels.len > 0: wr.warnings.note("alpha channels are not saved to OpenRaster")
  if d.metadata.text.len > 0 or d.metadata.xmp.isSome or not d.metadata.exif.isNil:
    wr.warnings.note("metadata (XMP, EXIF, text) is not saved to OpenRaster")
  if not d.iccProfile.isNil: wr.warnings.note("the colour profile is not saved to OpenRaster; readers assume sRGB")
  if d.mode == cmGrayscale: wr.warnings.note("grayscale layers are saved as RGB")
  ExportResult(bytes: wr.zip.finish, warnings: wr.warnings.finish)
