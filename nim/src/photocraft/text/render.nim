## Rasterizes a `TextLayout` into a document-space surface of any pixel format (port of
## `photocraft_text::render`): faux bold by offset copies, faux italic shear, warp, per-colour
## coverage composited in premultiplied form.

import std/[options, math]
import ../geom, ../color, ../raster as rst, ../doc
import ../font/sfnt
import fonts, layout, raster, warp

const
  FauxItalicDeg* = 12'f32       ## faux-italic slant in degrees (Photoshop-like)
  FauxBoldRadius* = 0.018'f32   ## faux-bold dilation radius as a fraction of the font size
  MaxPixels = 256'i64 * 1024 * 1024

type
  Rendered* = object
    surface*: Surface
    rect*: Rect

  WarpSink[S] = object
    inner: ptr S
    warp: TextWarper
    post: Xform

  PathElKind* = enum peMove, peLine, peQuad, peCurve, peClose
  PathEl* = object
    kind*: PathElKind
    pts*: array[3, array[2, float64]]   ## used points first (control points, then end point)

proc line[S](w: var WarpSink[S]; p0, p1: (float64, float64)) =
  let len = sqrt((p1[0] - p0[0]) ^ 2 + (p1[1] - p0[1]) ^ 2)
  let n = clamp(int(ceil(len / w.warp.maxSegment)), 1, 256)
  let warp = w.warp
  let post = w.post
  template map(t: float64): (float64, float64) =
    (let (x, y) = warp.apply(p0[0] + (p1[0] - p0[0]) * t, p0[1] + (p1[1] - p0[1]) * t); post.apply(x, y))
  var a = map(0.0)
  for i in 1 .. n:
    let b = map(float64(i) / float64(n))
    w.inner[].line(a, b)
    a = b

proc drawOutline[S](pen: var Pen[S]; o: Outline) =
  for c in o:
    case c.verb
    of pvMove: pen.moveTo(c.pts[2][0], c.pts[2][1])
    of pvLine: pen.lineTo(c.pts[2][0], c.pts[2][1])
    of pvQuad: pen.quadTo(c.pts[0][0], c.pts[0][1], c.pts[2][0], c.pts[2][1])
    of pvCubic: pen.curveTo(c.pts[0][0], c.pts[0][1], c.pts[1][0], c.pts[1][1], c.pts[2][0], c.pts[2][1])
    of pvClose: pen.close()
  pen.close()

proc glyphXform(l: TextLayout; g: PlacedGlyph; faceSkewDeg: float32): Xform =
  ## Font units (scaled to px, y up) → text space: scales, faux italic, baseline shift, orientation.
  if g.style < 0 or g.style >= l.styles.len: return [1.0, 0.0, 0.0, -1.0, float64(g.x), float64(g.y)]
  let st = l.styles[g.style]
  let hs = float64(if st.horizontalScale > 0: st.horizontalScale else: 1'f32)
  let vs = float64(if st.verticalScale > 0: st.verticalScale else: 1'f32)
  let skewDeg = (if st.fauxItalic: FauxItalicDeg else: 0'f32) + faceSkewDeg
  let skew = tan(degToRad(float64(skewDeg))) * vs
  let shift = float64(st.baselineShiftPt * l.pxPerPt)
  let (x, y) = (float64(g.x), float64(g.y))
  case g.orient
  of goHorizontal: [hs, 0.0, skew, -vs, x, y - shift]
  of goUpright: [hs, 0.0, skew, -vs, x + shift, y]
  of goRotated: mul([0.0, 1.0, -1.0, 0.0, x, y], [hs, 0.0, skew, -vs, 0.0, -shift])

proc glyphOutline(l: TextLayout; g: PlacedGlyph): Option[Outline] =
  if g.face < 0 or g.face >= l.faces.len: return
  let gf = l.faces[g.face]
  let f = gf.face.tryLoad
  if f.isNone: return
  if g.id < 0 or g.id >= f.get.numGlyphs: return
  some(f.get.scaledOutline(g.id, gf.sizePx))

proc draw[S](l: TextLayout; transform: Xform; sink: var S; onlyColor: Option[Color]; wp: Option[TextWarper]) =
  for g in l.glyphs:
    if g.style < 0 or g.style >= l.styles.len: continue
    let st = l.styles[g.style]
    if onlyColor.isSome and onlyColor.get != st.color: continue
    let o = l.glyphOutline(g)
    if o.isNone: continue
    let face = l.faces[g.face]
    let glyph = glyphXform(l, g, face.skewDeg)
    let bold = st.fauxBold or face.embolden
    let r = float64(face.sizePx * FauxBoldRadius)
    let offsets: seq[(float64, float64)] =
      if bold: @[(-1.0, 0.0), (1.0, 0.0), (0.0, -1.0), (0.0, 1.0), (0.7, 0.7), (-0.7, -0.7), (0.7, -0.7), (-0.7, 0.7)]
      else: @[(0.0, 0.0)]
    for (ox, oy) in offsets:
      let local = mul([1.0, 0.0, 0.0, 1.0, ox * r, oy * r], glyph)
      if wp.isSome:
        var ws = WarpSink[S](inner: addr sink, warp: wp.get, post: transform)
        var pen = newPen(ws, local)
        pen.drawOutline(o.get)
      else:
        var pen = newPen(sink, mul(transform, local))
        pen.drawOutline(o.get)
  for d in l.decorations:
    if d.style < 0 or d.style >= l.styles.len: continue
    if onlyColor.isSome and onlyColor.get != l.styles[d.style].color: continue
    if wp.isSome:
      var ws = WarpSink[S](inner: addr sink, warp: wp.get, post: transform)
      rectTo(ws, IdentityXform, float64(d.x0), float64(d.y0), float64(d.x1), float64(d.y1))
    else:
      rectTo(sink, transform, float64(d.x0), float64(d.y0), float64(d.x1), float64(d.y1))

proc layoutWarp*(l: TextLayout; w: Option[TextWarp]): Option[TextWarper] =
  ## The warp to apply to `l` (none when absent, `warpNone` or flat).
  if w.isNone: return
  let b = l.bounds
  if b.isNone: return
  newTextWarper(w.get, b.get)

proc colorIn(format: PixelFormat; c: Color): seq[float32] =
  let n = format.mode.colorChannels
  if c.mode == format.mode:
    for i in 0 ..< n: result.add c.c[i]
  else:
    let rgb = c.toRgb
    var f = format
    f.alpha = false
    result = fromRgba(f, [rgb[0], rgb[1], rgb[2], 1'f32])
    result.setLen(n)

proc rectFromBounds(b: array[4, float64]): Rect =
  for v in b:
    if v.classify in {fcNan, fcInf, fcNegInf}: return EmptyRect
  let mn = float64(low(int32))
  let mx = float64(high(int32))
  let x0 = clamp(floor(b[0]) - 1, mn, mx)
  let y0 = clamp(floor(b[1]) - 1, mn, mx)
  let x1 = clamp(ceil(b[2]) + 1, mn, mx)
  let y1 = clamp(ceil(b[3]) + 1, mn, mx)
  if x1 <= x0 or y1 <= y0 or x1 - x0 > float64(MaxPixels) or y1 - y0 > float64(MaxPixels): return EmptyRect
  rect(int(x0), int(y0), int(x1), int(y1))

proc inkRectWarped*(l: TextLayout; transform: Affine; wp: Option[TextWarper]): Rect =
  var b: BoundsSink
  draw(l, transform.m, b, none(Color), wp)
  if b.rect.some: rectFromBounds(b.rect.r) else: EmptyRect

proc inkRect*(l: TextLayout; transform: Affine): Rect =
  ## Document-space bounds of the drawn text (integer pixel rectangle).
  inkRectWarped(l, transform, none(TextWarper))

proc rasterizeWarped*(l: TextLayout; transform: Affine; format: PixelFormat; antialias: AntiAlias;
                      wp: Option[TextWarper]): Rendered =
  ## Rasterizes `l` through `transform` (text space → document pixels); the result has alpha and
  ## colour in `format`'s model and depth.
  var fmt = format
  fmt.alpha = true
  let r = inkRectWarped(l, transform, wp)
  let (w, h) = (r.width, r.height)
  result.surface = newSurface(fmt)
  if w == 0 or h == 0 or int64(w) * int64(h) > MaxPixels:
    result.rect = rect(0, 0, 0, 0)
    return
  let n = fmt.mode.colorChannels
  let stride = n + 1
  var acc = newSeq[float32](w * h * stride)
  let xf = mul([1.0, 0.0, 0.0, 1.0, -float64(r.x0), -float64(r.y0)], transform.m)
  var colors: seq[Color]
  for st in l.styles:
    if st.color notin colors: colors.add st.color
  for c in colors:
    var cov = newCoverage(w, h)
    draw(l, xf, cov, some(c), wp)
    let cv = cov.finish()
    let comps = colorIn(fmt, c)
    let a = clamp(c.alpha, 0'f32, 1'f32)
    for i, v0 in cv:
      let v = if antialias == aaNone: (if v0 >= 0.5'f32: 1'f32 else: 0'f32) else: v0
      let s = v * a
      if s <= 0: continue
      let keep = 1 - s
      let base = i * stride
      for j in 0 ..< n: acc[base + j] = comps[j] * s + acc[base + j] * keep
      acc[base + n] = s + acc[base + n] * keep
  for i in 0 ..< w * h:
    let a = acc[i * stride + n]
    if a > 0:
      for j in 0 ..< n: acc[i * stride + j] = clamp(acc[i * stride + j] / a, 0'f32, 1'f32)
  result.surface.writeRegion(r, acc)
  result.surface.prune()
  result.rect = r

proc rasterize*(l: TextLayout; transform: Affine; format: PixelFormat; antialias: AntiAlias): Rendered =
  rasterizeWarped(l, transform, format, antialias, none(TextWarper))

type Recorder = object
  glyph, post: Xform
  warp: Option[TextWarper]
  output: seq[PathEl]

proc map(r: Recorder; x, y: float32): array[2, float64] =
  var (tx, ty) = r.glyph.apply(float64(x), float64(y))
  if r.warp.isSome: (tx, ty) = r.warp.get.apply(tx, ty)
  let (dx, dy) = r.post.apply(tx, ty)
  [dx, dy]

proc outlines*(l: TextLayout; transform: Affine; wp: Option[TextWarper]): seq[seq[PathEl]] =
  ## Glyph outlines in document space, one element list per glyph (Create Work Path, Convert to
  ## Shape). Faux bold isn't applied; faux italic and scaling are.
  for g in l.glyphs:
    let o = l.glyphOutline(g)
    if o.isNone: continue
    let face = l.faces[g.face]
    var rec = Recorder(glyph: glyphXform(l, g, face.skewDeg), post: transform.m, warp: wp)
    for c in o.get:
      case c.verb
      of pvMove: rec.output.add PathEl(kind: peMove, pts: [rec.map(c.pts[2][0], c.pts[2][1]), [0.0, 0.0], [0.0, 0.0]])
      of pvLine: rec.output.add PathEl(kind: peLine, pts: [rec.map(c.pts[2][0], c.pts[2][1]), [0.0, 0.0], [0.0, 0.0]])
      of pvQuad: rec.output.add PathEl(kind: peQuad, pts: [rec.map(c.pts[0][0], c.pts[0][1]), rec.map(c.pts[2][0], c.pts[2][1]), [0.0, 0.0]])
      of pvCubic: rec.output.add PathEl(kind: peCurve, pts: [rec.map(c.pts[0][0], c.pts[0][1]), rec.map(c.pts[1][0], c.pts[1][1]), rec.map(c.pts[2][0], c.pts[2][1])])
      of pvClose: rec.output.add PathEl(kind: peClose)
    if rec.output.len > 0: result.add rec.output
