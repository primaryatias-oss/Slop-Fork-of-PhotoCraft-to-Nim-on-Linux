## Live gradients (port of `gradient_fill_cmds.rs`): the Gradient tool's non-destructive mode
## (Photoshop 2023+). A drag makes a Gradient Fill layer above the active layer, masked by the
## selection, whose start and end points, colour stops, opacity stops and midpoints stay editable
## on canvas afterwards. The classic destructive mode is `paint.gradient`.
##
## A live gradient is laid out on the canvas ("Align with layer" off), so its handles stay put
## when its mask changes. Its start/end points are stored as the fill's angle, scale and centre
## offset (`compose/gradient`), which is what a PSD Gradient Fill holds, and it renders the same
## pixels as painting the same drag with `paint.gradient`.
##
## Every edit is one command, so one history step per gesture: the UI previews a drag with
## `applySet` and commits it once on release.

import std/[json, options, strutils, algorithm, math]
import ./core
import ../compose
import ../compose/gradient as gf
import ../rmath

const
  GradientCreateCmd* = "gradient.fill.create"
  GradientSetCmd* = "gradient.fill.set"
  GradientStopCmd* = "gradient.fill.stop"
  GradientGetCmd* = "gradient.fill.get"
  MaxCoord = 1.0e7'f32 ## largest coordinate accepted for a start/end point (pixels)
  ScaleRange = (0.1'f32, 1.5'f32)
    ## Scale range of a gradient fill (a fraction): Photoshop allows 10–150 % (the PSD stores a
    ## percentage). A handle drag past it clamps, so the far handle stops where the scale does.

proc isFin32(v: float32): bool {.inline.} = v == v and abs(v) != Inf

proc f32Of(n: JsonNode): Option[float32] =
  ## `v.as_f64().map(|v| v as f32)`.
  let x = n.asF64
  if x.isSome: some(float32(x.get)) else: none(float32)

proc num(p: JsonNode; k: string): Option[float32] =
  let v = p.pget(k).f32Of
  if v.isSome and isFin32(v.get): v else: none(float32)

proc point(cmdId: string; p: JsonNode; k: string): Option[array[2, float32]] =
  let v = p.pget(k)
  if v.isNil: return
  if not v.isArray or v.len != 2: raise bad(cmdId, "`" & k & "` is [x, y]")
  proc c(i: int): Option[float32] =
    let x = v.elems[i].f32Of
    if x.isSome and isFin32(x.get) and abs(x.get) <= MaxCoord: x else: none(float32)
  let (x, y) = (c(0), c(1))
  if x.isNone or y.isNone: raise bad(cmdId, "`" & k & "` needs finite coordinates within ±10000000")
  some([x.get, y.get])

proc styleParam(cmdId: string; p: JsonNode): Option[GradientStyle] =
  let v = p.pget("style")
  if v.isNull: return
  if v.kind != JString: raise bad(cmdId, "`style` is a string")
  case v.str.toLowerAscii
  of "linear": some(gsLinear)
  of "radial": some(gsRadial)
  of "angle": some(gsAngle)
  of "reflected": some(gsReflected)
  of "diamond": some(gsDiamond)
  else: raise bad(cmdId, "unknown style `" & v.str.toLowerAscii & "` (linear|radial|angle|reflected|diamond)")

proc styleName*(s: GradientStyle): string =
  case s
  of gsLinear: "linear"
  of gsRadial: "radial"
  of gsAngle: "angle"
  of gsReflected: "reflected"
  of gsDiamond: "diamond"

proc hexByte(s: string): Option[uint8] =
  ## `u8::from_str_radix(s, 16)`: an optional `+`, then hex digits.
  var t = s
  if t.startsWith("+"): t = t[1 .. ^1]
  if t.len == 0: return
  var v = 0
  for ch in t:
    let d = case ch
      of '0'..'9': ord(ch) - ord('0')
      of 'a'..'f': ord(ch) - ord('a') + 10
      of 'A'..'F': ord(ch) - ord('A') + 10
      else: -1
    if d < 0: return
    v = v * 16 + d
    if v > 255: return
  some(uint8(v))

proc isCharBoundary(s: string; i: int): bool =
  i <= 0 or i >= s.len or (ord(s[i]) and 0xC0) != 0x80

proc parseColor(v: JsonNode; fg, bg: array[4, float32]): Option[Color] =
  ## A stop colour: `"#rrggbb"`, `"#rrggbbaa"`, `[r, g, b, a?]` (0..1), `"foreground"` or
  ## `"background"`.
  var c: array[4, float32]
  if v.isNil: return
  if v.kind == JString and v.str == "foreground": c = fg
  elif v.kind == JString and v.str == "background": c = bg
  elif v.kind == JString:
    var h = v.str
    while h.startsWith("#"): h = h[1 .. ^1]
    proc b(i: int): Option[float32] =
      if i + 2 > h.len or not h.isCharBoundary(i) or not h.isCharBoundary(i + 2): return
      let x = hexByte(h[i ..< i + 2])
      if x.isSome: some(float32(x.get) / 255'f32) else: none(float32)
    case h.len
    of 6:
      let (r, g, bb) = (b(0), b(2), b(4))
      if r.isNone or g.isNone or bb.isNone: return
      c = [r.get, g.get, bb.get, 1'f32]
    of 8:
      let (r, g, bb, a) = (b(0), b(2), b(4), b(6))
      if r.isNone or g.isNone or bb.isNone or a.isNone: return
      c = [r.get, g.get, bb.get, a.get]
    else: return
  elif v.kind == JArray and v.len in 3..4:
    proc f(i: int): Option[float32] =
      let x = v.elems[i].f32Of
      if x.isSome and isFin32(x.get): x else: none(float32)
    for i in 0 ..< v.len:
      let x = f(i)
      if x.isNone: return
      c[i] = x.get
    if v.len == 3: c[3] = 1'f32
  else: return
  for x in c.mitems: x = clamp(x, 0'f32, 1'f32)
  some(rgba(c[0], c[1], c[2], c[3]))

proc hex(c: Color): string =
  let rgb = c.toRgb
  proc q(v: float32): int = int(round(clamp(v, 0'f32, 1'f32) * 255'f32))
  result = "#" & toHex(q(rgb[0]), 2).toLowerAscii & toHex(q(rgb[1]), 2).toLowerAscii & toHex(q(rgb[2]), 2).toLowerAscii
  if c.alpha < 1'f32: result.add toHex(q(c.alpha), 2).toLowerAscii

proc presetParam(s: Session; p: JsonNode; cmdId: string): GradientPreset =
  ## The gradient a `gradient.fill.create` uses: a preset (`"gradient"`), explicit `"stops"`,
  ## else the current gradient (Gradients panel); explicit `"transparency"` stops replace its own.
  let name = p.pget("gradient").asStr
  if name.isSome:
    for g in s.presets.gradients:
      for it in g.items:
        if it.name.len == name.get.len and it.name.toLowerAscii == name.get.toLowerAscii:
          return it.withTransparency(p, cmdId)
    raise bad(cmdId, "no gradient preset \"" & name.get & "\" (see gradient.presets.list)")
  if not p.pget("stops").isNil: return gradientFromParams("Custom", p, cmdId)
  s.presets.gradient.withTransparency(p, cmdId)

proc gradientOf(s: Session; id: LayerId; cmdId: string): (Fill, Rect) =
  ## The fill and frame of gradient fill layer `id`.
  let d = s.active
  if d.isNil: raise noDocument()
  let l = d.doc[].findLayer(id)
  if l.isNil: raise noLayer(id)
  if l[].content.kind == lkFill and l[].content.fill.kind == fkGradient:
    return (l[].content.fill, fillFrame(l[], d.doc[].bounds))
  raise bad(cmdId, "layer \"" & l[].name & "\" is not a gradient fill layer")

proc hasGradientFillE(s: Session): string =
  ## Whether the active layer is a gradient fill layer (the commands' `enabled`).
  let (l, why) = activeLayerOf(s)
  if l.isNil: return why
  if l[].content.kind == lkFill and l[].content.fill.kind == fkGradient: ""
  else: "the active layer is not a gradient fill layer"

proc hasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")

proc clampScale(v: float32): float32 = clamp(v, ScaleRange[0], ScaleRange[1])

proc frameWithAlign(layer: Layer; f: Fill; canvas: Rect): Rect =
  ## The frame a gradient fill of `layer` (with `align`) is laid out in.
  var l = layer
  l.content = LayerContent(kind: lkFill, fill: f)
  fillFrame(l, canvas)

proc applySet*(layer: Layer; f: Fill; canvas: Rect; p: JsonNode; fg, bg: array[4, float32]): Fill =
  ## Applies `gradient.fill.set` params to a gradient fill (`layer` gives its frame; `fg`/`bg`
  ## resolve stop colours). Pure, so the canvas previews a handle drag with exactly what the
  ## command will commit.
  const C = GradientSetCmd
  if f.kind != fkGradient: raise bad(C, "not a gradient fill")
  var o = f
  let oldFrame = frameWithAlign(layer, f, canvas)
  let oldHandles = gf.handles(o.style, o.angle, o.scale, o.offset, oldFrame)
  let av = p.pget("align")
  if not av.isNil:
    let b = av.asBool
    if b.isNone: raise bad(C, "`align` is a boolean")
    o.align = b.get
  let newStyle = styleParam(C, p)
  var explicitGeometry = false
  for k in ["angle", "scale", "offset"]:
    if not p.pget(k).isNil: explicitGeometry = true
  let a = p.pget("angle")
  if not a.isNil:
    let v = a.f32Of
    if v.isNone or not isFin32(v.get): raise bad(C, "`angle` is a number (degrees)")
    o.angle = remEuclidF(v.get, 360'f32)
    if o.angle > 180'f32: o.angle -= 360'f32
  if not p.pget("scale").isNil:
    let v = num(p, "scale")
    if v.isNone or not (v.get > 0'f32): raise bad(C, "`scale` is a positive percentage")
    o.scale = clampScale(v.get / 100'f32)
  let ov = p.pget("offset")
  if not ov.isNil:
    if not ov.isArray or ov.len != 2: raise bad(C, "`offset` is [x%, y%]")
    proc c(i: int): Option[float32] =
      let x = ov.elems[i].f32Of
      if x.isSome and isFin32(x.get) and abs(x.get) <= 1.0e5'f32: x else: none(float32)
    let (x, y) = (c(0), c(1))
    if x.isNone or y.isNone: raise bad(C, "`offset` needs finite percentages")
    o.offset = (x.get / 100'f32, y.get / 100'f32)
  let rv = p.pget("reverse")
  if not rv.isNil:
    let b = rv.asBool
    if b.isNone: raise bad(C, "`reverse` is a boolean")
    o.reverse = b.get
  let dv = p.pget("dither")
  if not dv.isNil:
    let b = dv.asBool
    if b.isNone: raise bad(C, "`dither` is a boolean")
    o.dither = b.get
  let frm = point(C, p, "from")
  let to = point(C, p, "to")
  let frame = block:
    let tmp = Fill(kind: fkGradient, angle: o.angle, scale: o.scale, style: o.style, reverse: o.reverse,
                   offset: o.offset, dither: o.dither, align: o.align)
    frameWithAlign(layer, tmp, canvas)
  # A style change alone keeps the handles where they are (they are what the user placed).
  let styleChanged = newStyle.isSome and newStyle.get != o.style
  if newStyle.isSome: o.style = newStyle.get
  if frm.isSome or to.isSome or (styleChanged and not explicitGeometry):
    let cur = if frm.isSome or to.isSome: gf.handles(o.style, o.angle, o.scale, o.offset, frame) else: oldHandles
    let (f0, t0) = (frm.get(cur[0]), to.get(cur[1]))
    let (an, sc, off) = gf.fromHandles(o.style, f0, t0, frame, o.angle)
    o.angle = an
    o.scale = clampScale(sc)
    o.offset = off
    # Past the scale range the dragged handle stops; the other one stays where it is (#1243).
    # (Linear centres on the midpoint, so a clamped scale would otherwise move both ends.)
    var anchor = none((array[2, float32], bool))
    if frm.isSome and to.isNone: anchor = some((t0, false))
    elif frm.isNone and to.isSome: anchor = some((f0, true))
    if anchor.isSome and o.scale != sc:
      let (pin, isStart) = anchor.get
      let (f1, t1) = gf.handles(o.style, o.angle, o.scale, o.offset, frame)
      let at = if isStart: f1 else: t1
      let (dx, dy) = (pin[0] - at[0], pin[1] - at[1])
      let (w, h) = (float32(max(frame.width, 1)), float32(max(frame.height, 1)))
      o.offset = (o.offset[0] + dx / w, o.offset[1] + dy / h)
  let sv = p.pget("stops")
  if not sv.isNil:
    if not sv.isArray: raise bad(C, "`stops` is [[location, colour], …]")
    var nw: seq[(float32, Color)]
    for st in sv.elems:
      if not st.isArray or st.len != 2: raise bad(C, "a stop is [location 0..1, colour]")
      let t = st.elems[0].f32Of
      if t.isNone or not isFin32(t.get): raise bad(C, "stop location is a number")
      let c = parseColor(st.elems[1], fg, bg)
      if c.isNone: raise bad(C, "stop colour is \"#rrggbb\", [r,g,b,a], \"foreground\" or \"background\"")
      nw.add (clamp(t.get, 0'f32, 1'f32), c.get)
    if nw.len < 2: raise bad(C, "a gradient needs at least 2 colour stops")
    if nw.len > 1024: raise bad(C, "too many colour stops (max 1024)")
    nw.sort(proc(a, b: (float32, Color)): int = cmp(a[0], b[0]))
    if nw.len != o.stops.len: o.midpoints.setLen(0)
    o.stops = nw
  let tr = transparencyParam(p, C)
  if tr.isSome: o.opacityStops = tr.get
  let mv = p.pget("midpoints")
  if not mv.isNil:
    if not mv.isArray: raise bad(C, "`midpoints` is [0..1, …] (one per segment)")
    var mids: seq[float32]
    for m in mv.elems:
      let x = m.f32Of
      if x.isNone or not isFin32(x.get): raise bad(C, "midpoints are numbers")
      mids.add clamp(x.get, 0.05'f32, 0.95'f32)
    let segs = max(o.stops.len - 1, 0)
    if mids.len > segs: raise bad(C, $mids.len & " midpoints for " & $segs & " segments")
    o.midpoints = mids
  o

proc newGradientLayer*(s: Session; doc: Document; p: JsonNode): Layer =
  ## The Gradient Fill layer `gradient.fill.create` adds to `doc` for `p` (also the canvas's live
  ## preview of a Gradient tool drag): laid out on the canvas, so the handles land exactly where
  ## the drag was, and masked by the selection (Photoshop keeps the selection, as a mask).
  const C = GradientCreateCmd
  let frm = point(C, p, "from")
  if frm.isNone: raise bad(C, "missing `from` [x, y]")
  let to = point(C, p, "to")
  if to.isNone: raise bad(C, "missing `to` [x, y]")
  let style = styleParam(C, p).get(gsLinear)
  let g = presetParam(s, p, C)
  let (stops, opacityStops) = g.fillStops(s.tools.foreground, s.tools.background)
  let reverse = p.pget("reverse").asBool.get(false)
  let dither = p.pget("dither").asBool.get(true)
  let opacity = clamp(num(p, "opacity").get(100'f32), 0'f32, 100'f32) / 100'f32
  var blend = bmNormal
  let m = p.pget("mode").asStr
  if m.isSome and m.get.len > 0:
    let b = blendFromStr(m.get)
    if b.isNone: raise bad(C, "unknown blend mode `" & m.get & "`")
    blend = b.get
  let canvas = doc.bounds
  if canvas.isEmpty: raise bad(C, "the document is empty")
  let (angle, scale, offset) = gf.fromHandles(style, frm.get, to.get, canvas, 0'f32)
  let fill = Fill(kind: fkGradient, stops: stops, angle: angle, scale: clampScale(scale), style: style,
                  reverse: reverse, opacityStops: opacityStops, offset: offset, dither: dither,
                  align: false).inMode(doc.mode)
  result = newLayer(doc.nextLayerName("Gradient Fill"), LayerContent(kind: lkFill, fill: fill))
  result.opacity = opacity
  result.blend = blend
  result.mask = selectionMask(doc)

proc create(s: var Session; p: JsonNode): JsonNode =
  let d = s.active
  if d.isNil: raise noDocument()
  let layer = newGradientLayer(s, d.doc[], p)
  let id = s.editWith("New Gradient Fill Layer", proc(doc: var Document; active: var Option[LayerId]): LayerId =
    result = doc.insertAbove(active, layer)
    active = some(result))
  %*{"layer": uint64(id)}

proc describeFill*(f: Fill; frame: Rect): JsonNode =
  ## A gradient fill layer's settings as JSON (`gradient.fill.get`).
  if f.kind != fkGradient: return newJNull()
  let (frm, to) = gf.handles(f.style, f.angle, f.scale, f.offset, frame)
  var stops = newJArray()
  for (t, c) in f.stops: stops.add %*[t, hex(c)]
  var tr = newJArray()
  for (t, a) in f.opacityStops: tr.add %*[t, a * 100'f32]
  %*{"style": styleName(f.style), "angle": f.angle, "scale": f.scale * 100'f32,
     "offset": [f.offset[0] * 100'f32, f.offset[1] * 100'f32], "reverse": f.reverse, "dither": f.dither,
     "align": f.align, "from": frm, "to": to, "stops": stops, "transparency": tr, "midpoints": f.midpoints}

proc describe(s: Session; id: LayerId): JsonNode =
  let (f, frame) = gradientOf(s, id, GradientGetCmd)
  result = describeFill(f, frame)
  result["layer"] = %uint64(id)

proc replaceFill(s: var Session; id: LayerId; label: string; nw: Fill) =
  let d = s.active
  if d.isNil: raise noDocument()
  checkPixelsUnlocked(d.doc[], id)
  s.edit(label, proc(doc: var Document; active: var Option[LayerId]) =
    let mode = doc.mode
    let l = doc.findLayer(id)
    if l.isNil: raise noLayer(id)
    l[].content = LayerContent(kind: lkFill, fill: nw.inMode(mode)))

proc setGradient(s: var Session; p: JsonNode): JsonNode =
  let id = layerParam(s, p)
  let (fill, _) = gradientOf(s, id, GradientSetCmd)
  let (fg, bg) = (s.tools.foreground, s.tools.background)
  let d = s.active
  if d.isNil: raise noDocument()
  let l = d.doc[].findLayer(id)
  if l.isNil: raise noLayer(id)
  let layer = l[]
  let nw = applySet(layer, fill, d.doc[].bounds, p, fg, bg)
  if nw != fill: replaceFill(s, id, "Edit Gradient Fill", nw)
  describe(s, id)

type Paired = (float32, Color, float32)

proc paired(stops: seq[(float32, Color)]; mids: seq[float32]): seq[Paired] =
  ## Sorted colour stops paired with the midpoint of the segment that follows each.
  for i, (t, c) in stops: result.add (t, c, (if i < mids.len: mids[i] else: 0.5'f32))
  result.sort(proc(a, b: Paired): int = cmp(a[0], b[0]))

proc unpair(v0: seq[Paired]): (seq[(float32, Color)], seq[float32]) =
  var v = v0
  v.sort(proc(a, b: Paired): int = cmp(a[0], b[0]))
  var mids: seq[float32]
  for i in 0 ..< max(v.len - 1, 0): mids.add v[i][2]
  var allHalf = true
  for m in mids:
    if not (abs(m - 0.5'f32) < 1e-6'f32): allHalf = false
  if allHalf: mids.setLen(0)
  var st: seq[(float32, Color)]
  for (t, c, _) in v: st.add (t, c)
  (st, mids)

proc rampColorAlpha(f: Fill; t: float32): float32 =
  ## The colour stops' own alpha at `t` (without the opacity stops).
  if f.kind != fkGradient: return 1'f32
  let bare = Fill(kind: fkGradient, stops: f.stops, angle: 0, scale: 1, style: gsLinear, midpoints: f.midpoints,
                  align: true)
  max(rampOf(bare).sample(t)[3], 1e-6'f32)

proc applyStop*(f: Fill; p: JsonNode; fg, bg: array[4, float32]): Fill =
  ## Applies a `gradient.fill.stop` edit to a gradient fill. Pure (see `applySet`).
  const C = GradientStopCmd
  if f.kind != fkGradient: raise bad(C, "not a gradient fill")
  var o = f
  let action = p.pget("action").asStr
  if action.isNone: raise bad(C, "missing `action` (add|move|delete|color|opacity|midpoint)")
  let kind = p.pget("kind").asStr.get("color")
  let opacityKind = case kind
    of "color": false
    of "opacity": true
    else: raise bad(C, "unknown kind `" & kind & "` (color|opacity)")
  # The `index` param, checked against the `len` entries it can address (`what`, e.g. "colour
  # stop" or "segment") before any arithmetic on it: an index is untrusted (#958).
  proc index(len: int; what: string): int =
    let i = p.pget("index").asU64
    if i.isNone: raise bad(C, "missing `index` (a stop number from 0)")
    if i.get >= uint64(len): raise bad(C, "no " & what & " " & $i.get & " (there are " & $len & ")")
    int(i.get)
  proc location(): float32 =
    let v = num(p, "location")
    if v.isNone: raise bad(C, "missing `location` (0..1)")
    clamp(v.get, 0'f32, 1'f32)
  let ramp = rampOf(f)
  if opacityKind:
    # Opacity stops: an empty list means "opaque" (two 100 % stops when first edited).
    var os = o.opacityStops
    if os.len == 0: os = @[(0'f32, 1'f32), (1'f32, 1'f32)]
    os.sort(proc(a, b: (float32, float32)): int = cmp(a[0], b[0]))
    proc opacity(): float32 =
      let v = num(p, "opacity")
      if v.isNone: raise bad(C, "missing `opacity` (0..100)")
      clamp(v.get / 100'f32, 0'f32, 1'f32)
    case action.get
    of "add":
      let t = location()
      let v = num(p, "opacity")
      let a = if v.isSome: clamp(v.get / 100'f32, 0'f32, 1'f32) else: ramp.sample(t)[3] / rampColorAlpha(f, t)
      os.add (t, clamp(a, 0'f32, 1'f32))
    of "move":
      let i = index(os.len, "opacity stop")
      os[i][0] = location()
    of "delete":
      let i = index(os.len, "opacity stop")
      if os.len <= 2: raise bad(C, "a gradient keeps at least 2 opacity stops")
      os.delete(i)
    of "opacity":
      let i = index(os.len, "opacity stop")
      os[i][1] = opacity()
    else: raise bad(C, "unknown action `" & action.get & "` for opacity stops (add|move|delete|opacity)")
    os.sort(proc(a, b: (float32, float32)): int = cmp(a[0], b[0]))
    if os.len > 1024: raise bad(C, "too many opacity stops (max 1024)")
    o.opacityStops = os
    return o
  var v = paired(o.stops, o.midpoints)
  case action.get
  of "add":
    let t = location()
    # A new stop takes the colour the gradient has there (Photoshop), unless given.
    let cv = p.pget("color")
    var c: Color
    if not cv.isNil:
      let pc = parseColor(cv, fg, bg)
      if pc.isNone: raise bad(C, "bad `color`")
      c = pc.get
    else:
      let sm = ramp.sample(t)
      c = rgba(sm[0], sm[1], sm[2], rampColorAlpha(f, t))
    # Splitting a segment resets its midpoint.
    for k in countdown(v.high, 0):
      if v[k][0] <= t:
        v[k][2] = 0.5'f32
        break
    v.add (t, c, 0.5'f32)
    if v.len > 1024: raise bad(C, "too many colour stops (max 1024)")
  of "move":
    let i = index(v.len, "colour stop")
    v[i][0] = location()
  of "delete":
    let i = index(v.len, "colour stop")
    if v.len <= 2: raise bad(C, "a gradient keeps at least 2 colour stops")
    v.delete(i)
  of "color":
    let i = index(v.len, "colour stop")
    let c = parseColor(p.pget("color"), fg, bg)
    if c.isNone: raise bad(C, "missing or bad `color`")
    v[i][1] = c.get
  of "midpoint":
    # Segment `i` runs from stop `i` to stop `i + 1`, so there is one fewer than stops.
    let i = index(max(v.len - 1, 0), "segment")
    let m = num(p, "location")
    if m.isNone: raise bad(C, "missing `location` (0..1 of the segment)")
    v[i][2] = clamp(m.get, 0.05'f32, 0.95'f32)
  else: raise bad(C, "unknown action `" & action.get & "` (add|move|delete|color|midpoint)")
  let (st, mids) = unpair(v)
  o.stops = st
  o.midpoints = mids
  o

proc stopGradient(s: var Session; p: JsonNode): JsonNode =
  let id = layerParam(s, p)
  let (fill, _) = gradientOf(s, id, GradientStopCmd)
  let nw = applyStop(fill, p, s.tools.foreground, s.tools.background)
  if nw != fill: replaceFill(s, id, "Edit Gradient Fill", nw)
  describe(s, id)

proc getGradient(s: var Session; p: JsonNode): JsonNode =
  let id = layerParam(s, p)
  describe(s, id)

proc gradientFillSpecs*(): seq[CommandSpec] =
  @[cmd(GradientCreateCmd, "Gradient (Live)", [], "",
        """{"from":[x,y],"to":[x,y],"style":"linear|radial|angle|reflected|diamond"="linear","gradient":preset name?,"stops":[[t,"#rrggbb"|"foreground"|"background"],…]?,"transparency":[[t,0..100],…]? (default: the current gradient),"reverse":bool=false,"dither":bool=true,"opacity":0..100=100,"mode":"normal|multiply|…"="normal"} → {"layer":id}; a Gradient Fill layer above the active layer, masked by the selection""",
        enabledProc(hasDocE(s)), runProc(create(s, p))),
    cmd(GradientSetCmd, "Edit Gradient Fill", [], "",
        """{"layer":id?,"from":[x,y]?,"to":[x,y]?,"style":"linear|radial|angle|reflected|diamond"?,"angle":deg?,"scale":%?,"offset":[x%,y%]?,"reverse":bool?,"dither":bool?,"align":bool?,"stops":[[t,"#rrggbb"|"#rrggbbaa"|[r,g,b,a]|"foreground"|"background"],…]?,"transparency":[[t,0..100],…]?,"midpoints":[0.05..0.95 per segment]?} → the gradient (see gradient.fill.get)""",
        enabledProc(hasGradientFillE(s)), runProc(setGradient(s, p))),
    cmd(GradientStopCmd, "Edit Gradient Stop", [], "",
        """{"layer":id?,"action":"add|move|delete|color|opacity|midpoint","kind":"color|opacity"="color","index":stop (midpoint: segment) number,"location":0..1 (midpoint: of the segment),"color":"#rrggbb"|[r,g,b,a]|"foreground"|"background" (add: default the colour there),"opacity":0..100} → the gradient""",
        enabledProc(hasGradientFillE(s)), runProc(stopGradient(s, p))),
    cmd(GradientGetCmd, "Gradient Fill Settings", [], "",
        """{"layer":id?} → {"style","angle","scale":%,"offset":[%,%],"reverse","dither","align","from":[x,y],"to":[x,y],"stops":[[t,"#hex"]],"transparency":[[t,%]],"midpoints":[…]}""",
        enabledProc(hasGradientFillE(s)), runProc(getGradient(s, p)), journal = false)]
