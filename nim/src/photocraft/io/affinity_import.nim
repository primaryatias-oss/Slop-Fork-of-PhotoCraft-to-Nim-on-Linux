## Affinity documents (`.af`, `.afdesign`, `.afphoto`, `.afpub`), port of `io/src/affinity.rs`: the
## native layers, curves, shapes, artboards, text, images and masks `photocraft/affinity` reads,
## mapped to PhotoCraft layers (groups, shape layers, type layers, embedded smart objects, pixel
## layers, layer and vector masks). What could not be mapped is listed in the warnings. A file
## whose native data can't be read opens as its embedded preview, with a warning saying why.

import std/[options, strutils, math]
import ../geom, ../color, ../raster, ../doc, ../codecs, ../vector, ../rmath
import ../compose/gradient
import ../affinity
import ../affinity/container as afcontainer, ../affinity/model as afmodel, ../affinity/preview as afpreview
import ./common, ./flat, ./text_import

const
  Extensions* = ["af", "afphoto", "afdesign", "afpub"] ## extensions recognized as Affinity documents
  MaxPixels = 64'u64 shl 20 ## pixels an image or mask is resampled into, after cropping to the canvas
  MaxSide = 300_000.0 ## canvas side in pixels

proc isAffinity*(bytes: openArray[byte]): bool = afcontainer.isAffinity(bytes)

proc hasExtension*(name: string): bool =
  ## Whether the file name (after any path) ends in one of `Extensions`.
  var base = name
  let cut = max(base.rfind('/'), base.rfind('\\'))
  if cut >= 0: base = base[cut + 1 .. ^1]
  let dot = base.rfind('.')
  if dot < 0: return false
  let ext = base[dot + 1 .. ^1]
  for e in Extensions:
    if cmpIgnoreCase(ext, e) == 0: return true
  false

type Builder = object
  doc: Document
  warnings: seq[string]
  format: PixelFormat ## always RGBA8; spreads are placed side by side
  canvas: Rect

proc toAffine(a: AfAffine): Affine = Affine(m: a.m)
proc toPoint(p: AfPoint): Point = point(p.x, p.y)

proc asI32(v: float64): int =
  ## Rust `v as i32`: saturating, NaN → 0.
  if v != v: 0 elif v >= 2147483647.0: 2147483647 elif v <= -2147483648.0: -2147483648 else: int(v)

proc asU32(v: float64): int =
  if v != v or v <= 0.0: 0 elif v >= 4294967295.0: 4294967295 else: int(v)

proc warn(b: var Builder; w: string) =
  if w notin b.warnings: b.warnings.add w

proc newBuilder(name: string; d: AfDocument): Builder =
  # Spreads (Publisher pages) side by side, 64 px apart.
  var w = 0.0
  var h = 0.0
  for i, s in d.spreads:
    w += rmax(s.bounds.x1 - s.bounds.x0, 1.0) + (if i > 0: 64.0 else: 0.0)
    h = rmax(h, rmax(s.bounds.y1 - s.bounds.y0, 1.0))
  if not (w == w and abs(w) != Inf and h == h and abs(h) != Inf) or w > MaxSide or h > MaxSide:
    raise ioError(ieUnsupported, "the Affinity document is larger than 300 000 pixels on a side")
  let sz = size(asU32(rmax(ceil(w), 1.0)), asU32(rmax(ceil(h), 1.0)))
  var doc = newDocument(name, sz, cmRgb, stU8)
  doc.resolutionDpi = float32(d.dpi)
  Builder(doc: doc, format: Rgba8, canvas: rect(0, 0, sz.width, sz.height))

proc bbox(b: Builder; m: Affine; r: AfRect): Rect =
  ## Integer bounding box of `r` mapped by `m`, clipped to the canvas.
  var (x0, y0, x1, y1) = (Inf, Inf, -Inf, -Inf)
  for (x, y) in [(r.x0, r.y0), (r.x1, r.y0), (r.x1, r.y1), (r.x0, r.y1)]:
    let p = m.apply(point(x, y))
    x0 = rmin(x0, p.x)
    y0 = rmin(y0, p.y)
    x1 = rmax(x1, p.x)
    y1 = rmax(y1, p.y)
  template c(v: float64; lo, hi: int): int = asI32(clamp(v, float64(lo), float64(hi)))
  rect(c(floor(x0), b.canvas.x0, b.canvas.x1), c(floor(y0), b.canvas.y0, b.canvas.y1),
       c(ceil(x1), b.canvas.x0, b.canvas.x1), c(ceil(y1), b.canvas.y0, b.canvas.y1))

proc blend(b: var Builder; v: AfBlend): BlendMode =
  case v
  of abNormal: bmNormal
  of abPassThrough: bmPassThrough
  of abDarken: bmDarken
  of abDarkerColor: bmDarkerColor
  of abMultiply: bmMultiply
  of abColorBurn: bmColorBurn
  of abLighten: bmLighten
  of abLighterColor: bmLighterColor
  of abScreen: bmScreen
  of abColorDodge: bmColorDodge
  of abAdd: bmLinearDodge
  of abOverlay: bmOverlay
  of abSoftLight: bmSoftLight
  of abHardLight: bmHardLight
  of abVividLight: bmVividLight
  of abPinLight: bmPinLight
  of abLinearLight: bmLinearLight
  of abHardMix: bmHardMix
  of abDifference: bmDifference
  of abExclusion: bmExclusion
  of abSubtract: bmSubtract
  of abHue: bmHue
  of abSaturation: bmSaturation
  of abLuminosity: bmLuminosity
  of abColor: bmColor
  of abAverage, abNegation, abReflect, abGlow, abErase:
    b.warn("the Average, Negation, Reflect, Glow and Erase blend modes import as Normal")
    bmNormal

proc color(c: AfColor): Color =
  template f(v: float64): float32 = float32(clamp(v, 0.0, 1.0))
  case c.kind
  of acRgb: rgba(f(c.r), f(c.g), f(c.b), f(c.a))
  of acCmyk:
    let rgb = Color(mode: cmCmyk, c: [f(c.c), f(c.mg), f(c.y), f(c.k)], alpha: f(c.a)).toRgb()
    rgba(rgb[0], rgb[1], rgb[2], f(c.a))
  of acGray: rgba(f(c.v), f(c.v), f(c.v), f(c.a))
  of acLab:
    let lab = Color(mode: cmLab, c: [f(c.l / 100.0), f((c.la + 128.0) / 255.0), f((c.lb + 128.0) / 255.0), 0'f32], alpha: f(c.a))
    let rgb = lab.toRgb()
    rgba(rgb[0], rgb[1], rgb[2], f(c.a))

proc defaultName(k: AfKind): string =
  case k.tag
  of nkLayer: "Layer"
  of nkGroup: "Group"
  of nkArtboard: "Artboard"
  of nkShape: "Shape"
  of nkText: "Text"
  of nkImage: "Image"
  of nkUnsupported: "Group"

proc base(b: var Builder; n: AfNode; content: LayerContent): Layer =
  result = newLayer(if n.name.len == 0: defaultName(n.kind) else: n.name, content)
  result.visible = n.visible
  result.opacity = float32(n.opacity)
  result.blend = b.blend(n.blend)
  if n.locked: result.locks = Locks(all: true)

proc group(b: var Builder; n: AfNode; children: seq[Layer]): Layer =
  # Pass-through and Normal (isolated) groups map one to one.
  b.base(n, LayerContent(kind: lkGroup, group: Group(children: children, expanded: false)))

proc path(b: Builder; p: AfPath; toDoc: Affine; evenOdd: bool): Path =
  let m = toDoc
  var subpaths: seq[Subpath]
  for s in p.subpaths:
    # Knots: each anchor with the control points of the segments arriving and leaving.
    var knots = newSeqOfCap[Knot](s.segments.len + 1)
    let start = m.apply(toPoint(s.start))
    knots.add cornerKnot(start.x, start.y)
    for seg in s.segments:
      knots[^1].outCtrl = m.apply(toPoint(seg[0]))
      let e = m.apply(toPoint(seg[2]))
      var k = cornerKnot(e.x, e.y)
      k.inCtrl = m.apply(toPoint(seg[1]))
      knots.add k
    # A closed subpath that ends where it starts: the last knot is the first.
    if s.closed and knots.len > 1:
      let (f, l) = (knots[0], knots[^1])
      if abs(f.anchor.x - l.anchor.x) < 1e-9 and abs(f.anchor.y - l.anchor.y) < 1e-9:
        discard knots.pop()
        knots[0].inCtrl = l.inCtrl
    subpaths.add Subpath(closed: s.closed, knots: knots)
  result = newPath(subpaths)
  if evenOdd:
    # One component filled even-odd, so inner subpaths cut holes as in Affinity.
    result.fillRule = frEvenOdd
    for i in 1 ..< result.subpaths.len: result.subpaths[i].op = poJoin

proc fill(b: var Builder; p: AfPaint; frame: Rect; toDoc: Affine): Option[Fill] =
  case p.kind
  of apNone: none(Fill)
  of apSolid: some(solidFill(color(p.color)))
  of apGradient:
    let g = p.gradient
    let style = case g.kind
      of agLinear: gsLinear
      of agRadial: gsRadial
      of agConical: gsAngle
    let m = toDoc.mul(toAffine(g.transform))
    let (o, e, v) = (m.apply(point(0, 0)), m.apply(point(1, 0)), m.apply(point(0, 1)))
    let (u, w) = (hypot(e.x - o.x, e.y - o.y), hypot(v.x - o.x, v.y - o.y))
    if style == gsRadial and u > 0.0 and abs(w / u - 1.0) > 0.01:
      b.warn("elliptical gradients import as circular ones")
    let (angle, scale, offset) = fromHandles(style, [float32(o.x), float32(o.y)], [float32(e.x), float32(e.y)], frame, 0'f32)
    var f = Fill(kind: fkGradient, angle: angle, scale: scale, style: style, offset: offset, align: true)
    for s in g.stops:
      var c = color(s.color)
      c.alpha = 1
      f.stops.add (float32(s.offset), c)
      f.opacityStops.add (float32(s.offset), float32(s.color.alpha))
    for i in 0 ..< max(g.stops.len - 1, 0): f.midpoints.add float32(g.stops[i].halfPoint)
    some(f)

proc shape(b: var Builder; path: Path; fills: seq[AfPaint]; strokes: seq[AfStroke]; toDoc: Affine): ShapeLayer =
  let cb = path.controlBounds
  let frame = if cb.isSome:
      let (x0, y0, x1, y1) = cb.get
      rect(asI32(floor(x0)), asI32(floor(y0)), asI32(ceil(x1)), asI32(ceil(y1)))
    else: b.canvas
  if fills.len > 1 or strokes.len > 1: b.warn("only one fill and one stroke per shape import")
  let fl = if fills.len > 0: b.fill(fills[0], frame, toDoc) else: none(Fill)
  var stroke = none(ShapeStroke)
  if strokes.len > 0:
    let s = strokes[0]
    let paint = b.fill(s.paint, frame, toDoc)
    if paint.isSome:
      if s.behind: b.warn("strokes drawn behind their fill import in front of it")
      var st = ShapeStroke(width: float32(s.width), paint: paint.get, opacity: 1, miterLimit: float32(s.miterLimit))
      st.align = case s.align
        of strokeCenter: saCenter
        of strokeInside: saInside
        of strokeOutside: saOutside
      st.cap = case s.cap
        of capButt: lcapButt
        of capRound: lcapRound
        of capSquare: lcapSquare
      st.join = case s.join
        of joinMiter: ljMiter
        of joinRound: ljRound
        of joinBevel: ljBevel
      # In multiples of the width, as Photoshop stores them.
      if s.dash.isSome:
        for v in s.dash.get[0]: st.dashes.add float32(v / rmax(s.width, 1e-9))
        st.dashOffset = float32(s.dash.get[1] / rmax(s.width, 1e-9))
      stroke = some(st)
  result = ShapeLayer(path: path, fill: fl, stroke: stroke)
  result.cache = some(renderShape(result, b.format, b.canvas))

proc text(b: Builder; t: AfText; toDoc: Affine): TextLayer =
  let origin = if t.frame.isSome: point(t.frame.get.x0, t.frame.get.y0) else: toPoint(t.anchor)
  let m = toDoc.mul(toAffine(t.transform)).mul(translateAffine(origin.x, origin.y))
  let k = rmax(sqrt(abs(m.determinant)), 1e-9)
  let transform = m.mul(scaleAffine(1.0 / k))
  let dpi = float64(max(b.doc.resolutionDpi, 1'f32))
  var txt = ""
  var runs: seq[TextRun]
  for r in t.runs:
    # Affinity separates paragraphs with U+2029 and lines with U+2028.
    let s = r.text.replace(" ", "\r").replace(" ", "\r")
    var style = defaultCharStyle()
    style.fontFamily = r.family
    style.postscriptName = if r.postscript.len > 0: some(r.postscript) else: none(string)
    style.weight = uint16(clamp(r.weight, 100'i64, 900'i64))
    style.italic = r.italic
    style.sizePt = float32(r.size * k * 72.0 / dpi)
    style.color = if r.fill.kind == apSolid: color(r.fill.color) else: Black
    style.tracking = float32(r.tracking * 1000.0)
    style.leadingPt = if r.leading.isSome: some(float32(r.leading.get * k * 72.0 / dpi)) else: none(float32)
    txt.add s
    runs.add TextRun(len: s.len, style: style)
  let align = case t.align
    of alLeft: taLeft
    of alCenter: taCenter
    of alRight: taRight
    of alJustify: taJustifyLeft
  result = defaultTextLayer()
  result.text = txt
  result.fontFamily = if runs.len > 0: runs[0].style.fontFamily else: ""
  result.sizePt = if runs.len > 0: runs[0].style.sizePt else: 12'f32
  result.color = if runs.len > 0: runs[0].style.color else: Black
  result.transform = transform
  result.runs = runs
  var ps = defaultParagraphStyle()
  ps.align = align
  result.paragraphs = @[ParagraphRun(len: txt.len, style: ps)]
  if t.frame.isSome:
    let f = t.frame.get
    result.shape = TextShape(kind: tsBox, x: 0, y: 0, width: float32((f.x1 - f.x0) * k), height: float32((f.y1 - f.y0) * k))

proc pixels(b: var Builder; img: AfImage): Option[(int, int, seq[byte])] =
  ## Straight RGBA8 pixels of an image (decoding an embedded file), with its size.
  case img.pixels.kind
  of pxRgba8: some((int(img.width), int(img.height), img.pixels.data))
  of pxEncoded:
    var opts = defaultDecodeOptions()
    opts.limits = codecs.Limits(maxWidth: 65_535, maxHeight: 65_535, maxPixels: int64(MaxPixels), maxAlloc: 512'i64 shl 20)
    try:
      let d = decode(img.pixels.data, opts)
      some((d.width, d.height, d.toRgba8))
    except CodecError:
      b.warn("an embedded image that could not be decoded was left out")
      none((int, int, seq[byte]))

proc resample(b: var Builder; w, h: int; src: openArray[byte]; m: Affine; channels: int): Option[Surface] =
  ## Bilinear resampling of `src` (`w`×`h`, `channels` 4 = straight RGBA8 or 1 = grey) mapped by
  ## `m` (source pixels to canvas pixels) into a surface; exact copy when `m` is an integer
  ## translation. Pixels outside the source are transparent (grey: 0, hidden).
  let format = if channels == 1: Gray8 else: b.format
  var outp = if channels == 1: surfaceWithDefault(format, [0'f32]) else: newSurface(format)
  let r = b.bbox(m, AfRect(x0: 0, y0: 0, x1: float64(w), y1: float64(h)))
  if r.width == 0 or r.height == 0: return some(outp)
  if uint64(r.width) * uint64(r.height) > MaxPixels:
    b.warn("images larger than 64 megapixels on the canvas were left out")
    return none(Surface)
  let (a, bb, c, d, e, f) = (m.m[0], m.m[1], m.m[2], m.m[3], m.m[4], m.m[5])
  let exact = a == 1.0 and d == 1.0 and bb == 0.0 and c == 0.0 and fract(e) == 0.0 and fract(f) == 0.0
  let inv = m.inverse
  if inv.isNone: return none(Surface)
  let (rw, rh) = (r.width, r.height)
  var buf = newSeq[byte](rw * rh * channels)
  template at(x, y: int64; ch: int): float64 =
    (if x < 0 or y < 0 or x >= int64(w) or y >= int64(h): 0.0
     else:
       let i = (int(y) * w + int(x)) * channels + ch
       float64(if i < src.len: src[i] else: 0'u8))
  template toU8(v: float64): byte =
    (let q = clamp(round(v), 0.0, 255.0); if q != q: 0'u8 else: byte(q))
  for row in 0 ..< rh:
    for col in 0 ..< rw:
      let o = (row * rw + col) * channels
      let p = inv.get.apply(point(float64(r.x0) + float64(col) + 0.5, float64(r.y0) + float64(row) + 0.5))
      if exact:
        let (x, y) = (asI64(floor(p.x)), asI64(floor(p.y)))
        for ch in 0 ..< channels: buf[o + ch] = byte(at(x, y, ch))
        continue
      let (fx, fy) = (p.x - 0.5, p.y - 0.5)
      let (x0, y0) = (asI64(floor(fx)), asI64(floor(fy)))
      let (tx, ty) = (fx - float64(x0), fy - float64(y0))
      let weights = [((x0, y0), (1.0 - tx) * (1.0 - ty)), ((x0 + 1, y0), tx * (1.0 - ty)),
                     ((x0, y0 + 1), (1.0 - tx) * ty), ((x0 + 1, y0 + 1), tx * ty)]
      if channels == 1:
        var s = -0.0
        for (xy, wgt) in weights: s += at(xy[0], xy[1], 0) * wgt
        buf[o] = toU8(s)
      else:
        # Interpolate premultiplied colour so transparent pixels don't bleed.
        var alpha = -0.0
        for (xy, wgt) in weights: alpha += at(xy[0], xy[1], 3) * wgt
        for ch in 0 ..< 3:
          var pm = -0.0
          for (xy, wgt) in weights: pm += at(xy[0], xy[1], ch) * at(xy[0], xy[1], 3) / 255.0 * wgt
          buf[o + ch] = if alpha > 0.0: toU8(pm * 255.0 / alpha) else: 0'u8
        buf[o + 3] = toU8(alpha)
  outp.writeInterleaved(r, buf)
  outp.prune()
  some(outp)

proc image(b: var Builder; img: AfImage; toDoc: Affine): Option[LayerContent] =
  let px = b.pixels(img)
  if px.isNone: return
  let (w, h, rgba) = px.get
  # The pixel grid maps onto the Affinity image's own size, whatever the stored file's.
  let m = toDoc.mul(toAffine(img.transform)).mul(Affine(m: [float64(img.width) / float64(max(w, 1)), 0.0, 0.0,
                                                            float64(img.height) / float64(max(h, 1)), 0.0, 0.0]))
  let surface = b.resample(w, h, rgba, m, 4)
  if surface.isNone: return
  case img.pixels.kind
  of pxEncoded:
    # A placed image keeps its file, as a smart object placed by the same transform.
    let src = SmartSource(kind: ssEmbedded, fileName: "Affinity image", bytes: SharedBytes(data: img.pixels.data))
    some(smartContent(newSmartObject(src, m, surface)))
  of pxRgba8: some(rasterContent(surface.get))

proc mask(b: var Builder; img: AfImage; toDoc: Affine): Option[Surface] =
  let px = b.pixels(img)
  if px.isNone: return
  let (w, h, rgba) = px.get
  var grey = newSeq[byte](rgba.len div 4)
  for i in 0 ..< grey.len: grey[i] = rgba[4 * i]
  b.resample(w, h, grey, toDoc.mul(toAffine(img.transform)), 1)

proc layer(b: var Builder; n: AfNode; toDoc: Affine; top: bool): Option[Layer] =
  ## One Affinity node and its subtree; `toDoc` maps Affinity document pixels to the canvas.
  var children: seq[Layer]
  for c in n.children:
    let l = b.layer(c, toDoc, false)
    if l.isSome: children.add l.get
  var l: Layer
  case n.kind.tag
  of nkLayer, nkGroup: l = b.group(n, children)
  of nkUnsupported:
    if children.len == 0: return
    l = b.group(n, children)
  of nkShape:
    let p = b.path(n.kind.path, toDoc, n.kind.evenOdd)
    let sh = b.shape(p, n.kind.fills, n.kind.strokes, toDoc)
    if children.len == 0: l = b.base(n, shapeContent(sh))
    else:
      # A shape clips its children: a group masked by the outline, the shape at its bottom.
      var own = newLayer(if n.name.len == 0: "Shape" else: n.name, shapeContent(sh))
      own.opacity = 1
      l = b.group(n, @[own] & children)
      l.vectorMask = some(VectorMask(path: p, enabled: true, linked: true, density: 1, feather: 0))
      if n.kind.strokes.len > 0: b.warn("strokes of shapes that clip other layers are clipped to the shape")
  of nkArtboard:
    let r = b.bbox(toDoc, n.kind.rect)
    var bg = ArtboardBackground(kind: abTransparent, color: White)
    if n.kind.background.len > 0:
      let p = n.kind.background[0]
      if p.kind == apSolid:
        let c = color(p.color)
        if c.alpha > 0: bg = ArtboardBackground(kind: abCustom, color: c)
      else: b.warn("artboard backgrounds other than a solid colour import as transparent")
    l = b.group(n, children)
    if top:
      l.content.group.artboard = some(Artboard(rect: r, background: bg))
    else:
      # PhotoCraft artboards are top-level groups: a nested one clips by a vector mask.
      let p = newPath(@[polygon([(float64(r.x0), float64(r.y0)), (float64(r.x1), float64(r.y0)),
                                 (float64(r.x1), float64(r.y1)), (float64(r.x0), float64(r.y1))])])
      l.vectorMask = some(VectorMask(path: p, enabled: true, linked: true, density: 1, feather: 0))
      b.warn("artboards inside groups import as masked groups")
  of nkText:
    let t = b.text(n.kind.text, toDoc)
    let own = b.base(n, textContent(t))
    l = if children.len == 0: own else: b.group(n, @[own] & children)
  of nkImage:
    let content = b.image(n.kind.image, toDoc)
    if content.isSome: l = b.base(n, content.get)
    elif children.len == 0: return
    else: l = b.group(n, children)
  if n.mask.isSome:
    l.vectorMask = some(VectorMask(path: b.path(n.mask.get, toDoc, false), enabled: true, linked: true, density: 1, feather: 0))
  if n.pixelMask.isSome:
    let s = b.mask(n.pixelMask.get, toDoc)
    if s.isSome: l.mask = some(LayerMask(surface: s.get, enabled: true, linked: true, density: 1, feather: 0))
  some(l)

proc hasArtboards(layers: seq[Layer]): bool =
  for l in layers:
    if l.content.kind == lkGroup and l.content.group.artboard.isSome: return true
  false

proc build(b: var Builder; d: AfDocument) =
  var pages = d.spreads.len > 1
  for s in d.spreads:
    if s.pages.len > 1: pages = true
  var x = 0.0
  var layers: seq[Layer]
  for i, spread in d.spreads:
    let toDoc = translateAffine(x - spread.bounds.x0, -spread.bounds.y0)
    var children: seq[Layer]
    for n in spread.nodes:
      let l = b.layer(n, toDoc, true)
      if l.isSome: children.add l.get
    if pages:
      # Each page becomes an artboard holding the art drawn on it.
      let rects = if spread.pages.len == 0: @[spread.bounds] else: spread.pages
      let r = b.bbox(toDoc, rects[0])
      var g = newLayer("Page " & $(i + 1), LayerContent(kind: lkGroup, group: Group(children: children, expanded: true)))
      g.content.group.artboard = some(Artboard(rect: r, background: ArtboardBackground(kind: abWhite, color: White)))
      if rects.len > 1: b.warn("facing pages import as one artboard per spread")
      layers.add g
    else:
      layers.add children
    x += rmax(spread.bounds.x1 - spread.bounds.x0, 1.0) + 64.0
  if d.spreads.len > 0 and not d.spreads[0].transparent and not hasArtboards(layers):
    # An opaque page: a white background below the art.
    var bg = surfaceWithDefault(b.format, [1'f32, 1, 1, 1])
    bg.prune()
    var l = newLayer("Background", rasterContent(bg))
    l.locks.transparency = true
    layers.insert(l, 0)
  b.doc.layers = layers

proc previewDocument(name: string; bytes: openArray[byte]; native: string): ImportResult =
  ## The embedded preview, for files whose native document can't be read.
  var p: afpreview.Preview
  try: p = afpreview.preview(bytes)
  except AffinityError as e:
    raise ioError(ieUnsupported, native & "; its embedded preview can't be read either (" & e.msg & ")")
  var opts = defaultDecodeOptions()
  opts.limits = codecs.Limits(maxWidth: 4096, maxHeight: 4096, maxPixels: 4096 * 4096, maxAlloc: 128'i64 shl 20)
  var img: Image
  try: img = decode(bytes.toOpenArray(p.pngStart, p.pngStart + p.pngLen - 1), opts)
  except CodecError as e: raise ioError(ieCodec, e.msg)
  result = imageToDocument(name, img)
  if result.document.layers.len > 0:
    result.document.layers[0].name = "Affinity preview"
    result.document.layers[0].locks = Locks()
  result.sourceReadOnly = true
  result.previewOnly = true
  result.warnings.insert("The native Affinity document could not be read (" & native & "). Opened only its embedded " &
    $p.width & "×" & $p.height & " PNG preview, which may be smaller than the document: no layers, vectors, text or " &
    "pages. Save a new copy; the Affinity source cannot be saved back. Export PSD or PNG from Affinity for " &
    "full-resolution artwork.", 0)

proc importAffinity*(name: string; bytes: openArray[byte]): ImportResult =
  ## Imports an Affinity document; a file whose native data can't be read opens as its preview.
  var d: AfDocument
  try: d = afmodel.read(bytes)
  except AffinityError as e: return previewDocument(name, bytes, e.msg)
  var b = newBuilder(name, d)
  b.build(d)
  for w in d.warnings: result.warnings.add "Affinity: not imported or approximated: " & w & "."
  for w in b.warnings: result.warnings.add "Affinity: " & w & "."
  # Type layers draw from their model, within the import's raster bound: the sizes and frames
  # come from an untrusted file.
  text_import.prepare(b.doc)
  # Save never writes back over the Affinity file.
  result.document = move b.doc
  result.sourceReadOnly = true
