## Select menu and selection tools (port of `selection_cmds.rs`): Magic Wand, Color Range, Modify
## (expand/contract/border/smooth/feather), Grow, Similar, Lasso. Selections are grayscale
## coverage surfaces clipped to the canvas.

import std/[json, options, math, strutils]
import ./core
import ../compose, ../cms, ../rmath, ../rfmt
import ../algo/selection as sel

proc hasSelectionE(s: Session): string =
  let d = s.active
  if d.isNil or d.doc.selection.isNone: "no selection" else: ""

proc fNum(p: JsonNode; k: string; d: float32): float32 =
  let v = p.pget(k).asF64
  if v.isSome: float32(v.get) else: d

proc selMode(p: JsonNode): SelectionMode = parseSelectionMode(p.getStrOr("mode", "replace"))

proc pointInSelection(doc: Document; x, y: int): bool =
  ## A Color Range replacement is scoped to the selection that was active when it opened.
  doc.selection.isNone or doc.selection.get.sampleChannel(x, y, 0) > 0

proc scopeColorRange(mask: var seq[float32]; doc: Document; area: Rect; m: SelectionMode) =
  ## Explicit add/subtract/intersect keep their normal selection-combine semantics.
  if m != smReplace or doc.selection.isNone: return
  let previous = maskFromSurface(doc.selection, area)
  for i in 0 ..< min(mask.len, previous.len): mask[i] = rmin(mask[i], previous[i])

proc samplePixels(s: Session; allLayers: bool): (Rect, seq[array[4, float32]]) =
  ## RGBA pixels the tools sample: the composite, or the active layer.
  let d = s.activeOrRaise()
  let area = d.doc[].bounds
  var layer: ptr Layer = nil
  if d.activeLayer.isSome: layer = d.doc[].findLayer(d.activeLayer.get)
  if not allLayers and not layer.isNil and layer[].hasSurface:
    var px = newSeq[array[4, float32]](area.width * area.height)
    layer[].surface.readRgbaInto(area, px)
    return (area, px)
  (area, render(d.doc[], area).px)

proc setSelection(s: var Session; label: string; area: Rect; mask: seq[float32]; m: SelectionMode): JsonNode =
  let selected = s.editGet(label, bool):
    doc.selection = combine(doc.selection, mask, area, m)
    doc.selection.isSome
  %*{"selected": selected}

proc currentMask(doc: Document): (Rect, seq[float32]) =
  let area = doc.bounds
  (area, maskFromSurface(doc.selection, area))

proc shownGray(s: Session): Option[(Rect, seq[array[4, uint8]])] =
  ## Grayscale the canvas is actually showing, as opaque RGBA, when that is not the colour
  ## composite: a gray layer-mask view, or — with every colour channel hidden — the first visible
  ## alpha, Quick Mask, or the active layer's mask.
  let st = s.active
  if st.isNil: return
  let doc = st.doc
  let area = doc[].bounds
  let v = st.channelView
  let colorsOff = v.visibleColors(colorCount(doc[])) == 0
  let shown = v.shownLayerMask(doc[])
  var surf: Surface
  if shown.isSome and shown.get[1] == mvGray: surf = shown.get[0].surface
  elif colorsOff:
    var found = -1
    for i in 0 ..< doc.channels.len:
      if v.alphaShown(i):
        found = i
        break
    if found >= 0: surf = doc.channels[found].surface
    elif doc.quickMask.isSome and not v.quickMaskHidden: surf = doc.quickMask.get.surface
    elif shown.isSome: surf = shown.get[0].surface
    else:
      if st.activeLayer.isNone: return
      let l = doc[].findLayer(st.activeLayer.get)
      if l.isNil or l.mask.isNone: return
      surf = l.mask.get.surface
  else: return
  some((area, rgba8Image(surf, area)))

proc sampleRgba8*(s: Session; allLayers: bool): (Rect, seq[array[4, uint8]]) =
  ## 8-bit RGBA of the active layer (or the composite with `allLayers`) over the canvas.
  let d = s.activeOrRaise()
  let area = d.doc[].bounds
  var layer: ptr Layer = nil
  if d.activeLayer.isSome: layer = d.doc[].findLayer(d.activeLayer.get)
  if not allLayers and not layer.isNil and layer[].hasSurface: return (area, rgba8Image(layer[].surface, area))
  let px = render(d.doc[], area).px
  var outp = newSeq[array[4, uint8]](px.len)
  for i, q in px:
    for c in 0 .. 3: outp[i][c] = asU8(clamp(q[c], 0'f32, 1'f32) * 255 + 0.5'f32)
  (area, outp)

proc magicWand(s: var Session; p: JsonNode): JsonNode =
  let x = asI32(floor(fNum(p, "x", 0)))
  let y = asI32(floor(fNum(p, "y", 0)))
  let g = shownGray(s)
  let (area, img) = if g.isSome: g.get else: sampleRgba8(s, p.getBoolOr("sampleAllLayers", false))
  let region = wandRegion(img, area, (x, y), fNum(p, "tolerance", 32), p.getBoolOr("contiguous", true),
                          p.getBoolOr("antiAlias", true))
  let m = selMode(p)
  let selected = s.editGet("Magic Wand", bool):
    doc.selection = combineRegion(doc.selection, region, m)
    doc.selection.isSome
  %*{"selected": selected}

proc parseColor3(p: JsonNode): array[3, float32] =
  let v = p.pget("color")
  if v.isArray and v.len >= 3:
    for i in 0 .. 2:
      let f = v.elems[i].asF64
      result[i] = if f.isSome: float32(f.get) else: 0'f32
  elif not v.isNil and v.kind == JString:
    var h = v.str
    var i = 0
    while i < h.len and h[i] == '#': inc i
    h = h[i .. ^1]
    for k in 0 .. 2:
      let j = 2 * k
      if j + 2 <= h.len:
        let b = hexByte(h[j ..< j + 2])
        if b.isSome: result[k] = float32(b.get) / 255

proc pointText(pt: JsonNode): string = jsonText(pt)

proc parsePoint(pt: JsonNode): Option[(float64, float64)] =
  if not pt.isArray or pt.len != 2: return
  let x = pt.elems[0].asF64
  let y = pt.elems[1].asF64
  if x.isNone or y.isNone: return
  if x.get != x.get or abs(x.get) == Inf or y.get != y.get or abs(y.get) == Inf: return
  some((x.get, y.get))

proc eyedropperSamples(p: JsonNode; key: string; area: Rect; px: seq[array[4, float32]];
                       allowed: proc(x, y: int): bool; badFn: proc(msg: string): ref EngineError): seq[RangeSample] =
  ## Eyedropper samples from `key` (`[[x, y], …]` in document pixels): the colour under each point
  ## and where it was picked (area-local, for Localized Color Clusters).
  let pts = p.pget(key)
  if pts.isNil: return
  if not pts.isArray: raise badFn("`" & key & "` must be [[x, y], …]")
  let w = area.width
  for pt in pts.elems:
    let xy = parsePoint(pt)
    if xy.isNone: raise badFn("bad point " & pointText(pt) & " in `" & key & "` (want [x, y])")
    let (x, y) = xy.get
    let (xi, yi) = (asI32(floor(x)), asI32(floor(y)))
    var q = none(array[4, float32])
    if area.contains(xi, yi):
      let i = (yi - area.y0) * w + (xi - area.x0)
      if i >= 0 and i < px.len: q = some(px[i])
    if q.isNone:
      raise badFn("point [" & displayFloat(x) & ", " & displayFloat(y) & "] in `" & key & "` is outside the canvas")
    if not allowed(xi, yi):
      raise badFn("point [" & displayFloat(x) & ", " & displayFloat(y) & "] in `" & key & "` is outside the active selection")
    result.add RangeSample(color: [q.get[0], q.get[1], q.get[2]], at: some((float32(xi - area.x0), float32(yi - area.y0))))

proc labTransform(s: Session): Transform =
  ## Composite RGB (the compositor's output profile for the document) → CIE Lab D50.
  let d = s.activeOrRaise()
  let src = compositeProfile(d.doc[])
  try: cached(src, bLabD50.profile, defaultTransformOptions())
  except CatchableError as e: raise other("colour management: " & e.msg)

proc decodeLab(v: array[3, float32]): array[3, float32] =
  ## The Lab profile's normalised encoding (ICC v4) → L 0–100, a and b −128–127.
  [v[0] * 100, v[1] * 255 - 128, v[2] * 255 - 128]

proc colourToLab(t: Transform; rgb: array[3, float32]): array[3, float32] =
  var outp: array[3, float32]
  let inp = [clamp(rgb[0], 0'f32, 1'f32), clamp(rgb[1], 0'f32, 1'f32), clamp(rgb[2], 0'f32, 1'f32)]
  t.eval(inp, outp)
  decodeLab(outp)

proc pixelsToLab(t: Transform; px: seq[array[4, float32]]): seq[array[4, float32]] =
  ## RGBA pixels → Lab + alpha (L, a, b, alpha).
  var src = newSeq[float32](px.len * 4)
  for i, q in px:
    src[4 * i] = clamp(q[0], 0'f32, 1'f32)
    src[4 * i + 1] = clamp(q[1], 0'f32, 1'f32)
    src[4 * i + 2] = clamp(q[2], 0'f32, 1'f32)
    src[4 * i + 3] = q[3]
  var dst = newSeq[float32](px.len * 4)
  t.convertF32(src, 4, dst, 4, true)
  result = newSeq[array[4, float32]](px.len)
  for i in 0 ..< px.len:
    let lab = decodeLab([dst[4 * i], dst[4 * i + 1], dst[4 * i + 2]])
    result[i] = [lab[0], lab[1], lab[2], dst[4 * i + 3]]

type ColorRangeSamples* = object
  ## Prepared Sampled Colors query shared by the command and its interactive preview.
  toLab: Transform
  range: LabRange
  grid: bool
  fuzziness: float32
  localized: Option[float32]
  plusAt: seq[(float32, float32)]

proc crBad(msg: string): ref EngineError = bad("select.colorRange", msg)

proc withSamples(s: Session; p: JsonNode; area: Rect; plus, minus: seq[RangeSample]): ColorRangeSamples =
  proc fuzz(p: JsonNode; d: float32): float32 = rmax(fNum(p, "fuzziness", d), 0'f32)
  # The order the eyedropper clicks came in ("+" a point, "-" a subtracted point).
  var order: seq[bool]
  let ov = p.pget("order")
  if ov.isNil:
    for _ in plus: order.add true
    for _ in minus: order.add false
  else:
    if not ov.isArray: raise crBad("`order` must be a list of \"+\" and \"-\"")
    for v in ov.elems:
      if v.kind == JString and v.str == "+": order.add true
      elif v.kind == JString and v.str == "-": order.add false
      else: raise crBad("bad entry " & jsonText(v) & " in `order` (want \"+\" or \"-\")")
    var np, nm = 0
    for o in order:
      if o: inc np else: inc nm
    if np != plus.len or nm != minus.len:
      raise crBad("`order` must list each of `points` (\"+\") and `subtractPoints` (\"-\") once")
  var fixed: seq[array[3, float32]]
  let cs = p.pget("colors")
  if not cs.isNil:
    if not cs.isArray: raise crBad("`colors` must be a list of colours")
    for c in cs.elems: fixed.add parseColor3(%*{"color": c})
  if not p.pget("color").isNil: fixed.add parseColor3(p)
  # Localized Color Clusters: Range is a percentage of the canvas's longer side.
  var plusAt: seq[(float32, float32)]
  for x in plus:
    if x.at.isSome: plusAt.add x.at.get
  var localized = none(float32)
  if p.getBoolOr("localized", false):
    if plusAt.len == 0: raise crBad("localized clusters need eyedropper `points`")
    localized = some(clamp(fNum(p, "range", 100), 0'f32, 100'f32) / 100 * float32(max(area.width, area.height)))
  let toLab = labTransform(s)
  # 8-bit documents are compared on the 8-bit Lab grid, samples and pixels alike.
  let d = s.active
  let grid = not d.isNil and d.doc.depth == stU8
  proc q(c: array[3, float32]; grid: bool): array[3, float32] = (if grid: quantizeLab8(c) else: c)
  proc toLabQ(c: array[3, float32]): array[3, float32] = q(colourToLab(toLab, c), grid)
  proc foreground(): LabRange =
    let f = s.tools.foreground
    LabRange.point(toLabQ([f[0], f[1], f[2]]))
  var fixedLab: seq[array[3, float32]]
  for c in fixed: fixedLab.add toLabQ(c)
  var range = LabRange.spanning(fixedLab)
  var (pi, mi) = (0, 0)
  for add in order:
    if add:
      if pi < plus.len:
        let c = toLabQ(plus[pi].color)
        inc pi
        range = some(if range.isNone: LabRange.point(c) else: range.get.grow(c))
    elif mi < minus.len:
      let c = toLabQ(minus[mi].color)
      inc mi
      range = some((if range.isSome: range.get else: foreground()).subtract(c, fuzz(p, 40)))
  let r = if range.isSome: range.get else: foreground()
  ColorRangeSamples(toLab: toLab, range: r, grid: grid, fuzziness: fuzz(p, 40), localized: localized, plusAt: plusAt)

proc newColorRangeSamples*(s: Session; p: JsonNode): ColorRangeSamples =
  ## Prepare the query without a full-resolution composite: point samples use the same
  ## compositor and profile as the final selection.
  let d = s.activeOrRaise()
  let doc = d.doc
  let area = doc[].bounds
  proc sample(key: string): seq[RangeSample] =
    let pts = p.pget(key)
    if pts.isNil: return
    if not pts.isArray: raise crBad("`" & key & "` must be [[x, y], …]")
    for point in pts.elems:
      let q = parsePoint(point)
      if q.isNone: raise crBad("bad point " & jsonText(point) & " in `" & key & "` (want [x, y])")
      let (x, y) = (asI32(floor(q.get[0])), asI32(floor(q.get[1])))
      if not area.contains(x, y): raise crBad("point [" & $x & ", " & $y & "] in `" & key & "` is outside the canvas")
      if selMode(p) == smReplace and not pointInSelection(doc[], x, y):
        raise crBad("point [" & $x & ", " & $y & "] in `" & key & "` is outside the active selection")
      let r = rectXywh(x, y, 1, 1)
      var pixel = none(array[4, float32])
      if not p.getBoolOr("sampleAllLayers", true) and d.activeLayer.isSome:
        let l = doc[].findLayer(d.activeLayer.get)
        if not l.isNil and l[].hasSurface:
          var px: array[1, array[4, float32]]
          l[].surface.readRgbaInto(r, px)
          pixel = some(px[0])
      let px = if pixel.isSome: pixel.get
               else:
                 let b = render(doc[], r).px
                 if b.len > 0: b[0] else: [0'f32, 0, 0, 0]
      result.add RangeSample(color: [px[0], px[1], px[2]], at: some((float32(x - area.x0), float32(y - area.y0))))
  let plus = sample("points")
  let minus = sample("subtractPoints")
  withSamples(s, p, area, plus, minus)

proc coverage*(c: ColorRangeSamples; px: seq[array[4, float32]]; width: int; scale: float32; origin: array[2, float32]): seq[float32] =
  ## Coverage of a composite rectangle. `scale` is the original document pixels represented by
  ## one input pixel; `origin` is in original document pixels (for a visible canvas crop).
  var lab = pixelsToLab(c.toLab, px)
  if c.grid:
    for p in lab.mitems:
      let q = quantizeLab8([p[0], p[1], p[2]])
      p = [q[0], q[1], q[2], p[3]]
  let scale = if scale == scale and abs(scale) != Inf and scale > 0: scale else: 1'f32
  var points: seq[(float32, float32)]
  for (x, y) in c.plusAt: points.add ((x - origin[0]) / scale, (y - origin[1]) / scale)
  let loc = if c.localized.isSome: some((c.localized.get / scale, points)) else: none((float32, seq[(float32, float32)]))
  colorRangeLab(lab, width, c.range, c.fuzziness, c.grid, loc)

proc colorRangeCmd(s: var Session; p: JsonNode): JsonNode =
  let sv = p.pget("select")
  let preset = if sv.isNil: "sampledColors"
               elif sv.kind == JString: sv.str
               else: raise crBad("`select` must be a string")
  let invert = p.getBoolOr("invert", false)
  let selectionMode = selMode(p)
  proc fuzz(p: JsonNode; d: float32): float32 = rmax(fNum(p, "fuzziness", d), 0'f32)
  var area: Rect
  var mask: seq[float32]
  case preset
  of "sampledColors":
    let (a, px) = samplePixels(s, p.getBoolOr("sampleAllLayers", true))
    area = a
    let w = a.width
    # The dialog's eyedroppers: colours picked on the image, at their positions.
    let doc = s.activeOrRaise().doc
    let allowed = proc(x, y: int): bool = selectionMode != smReplace or pointInSelection(doc[], x, y)
    let plus = eyedropperSamples(p, "points", a, px, allowed, crBad)
    let minus = eyedropperSamples(p, "subtractPoints", a, px, allowed, crBad)
    let query = withSamples(s, p, a, plus, minus)
    mask = query.coverage(px, w, 1, [0'f32, 0])
  of "reds", "yellows", "greens", "cyans", "blues", "magentas":
    let center = case preset
      of "reds": 0'f32
      of "yellows": 60'f32
      of "greens": 120'f32
      of "cyans": 180'f32
      of "blues": 240'f32
      else: 300'f32
    let (a, px) = samplePixels(s, p.getBoolOr("sampleAllLayers", true))
    (area, mask) = (a, hueRange(px, center))
  of "highlights", "midtones", "shadows":
    # Range in levels (Highlights / Shadows: one value, Midtones: [low, high]); Fuzziness is a
    # percentage of the tonal scale.
    proc level(v: JsonNode): Option[float32] =
      let x = v.asF64
      if x.isSome and x.get >= 0 and x.get <= 255: some(float32(x.get)) else: none(float32)
    let r = p.pget("tonalRange")
    var (lo, hi) = (0'f32, 0'f32)
    if r.isNil:
      (lo, hi) = case preset
        of "shadows": (0'f32, 65'f32)
        of "highlights": (190'f32, 255'f32)
        else: (105'f32, 150'f32)
    elif preset == "shadows":
      let l = level(r)
      if l.isNone: raise crBad("tonalRange: want a level 0..255 (got " & jsonText(r) & ")")
      (lo, hi) = (0'f32, l.get)
    elif preset == "highlights":
      let l = level(r)
      if l.isNone: raise crBad("tonalRange: want a level 0..255 (got " & jsonText(r) & ")")
      (lo, hi) = (l.get, 255'f32)
    else:
      var ok = false
      if r.isArray and r.len == 2:
        let l = level(r.elems[0])
        let h = level(r.elems[1])
        if l.isSome and h.isSome and l.get <= h.get:
          (lo, hi) = (l.get, h.get)
          ok = true
      if not ok: raise crBad("tonalRange: want [low, high] levels 0..255 (got " & jsonText(r) & ")")
    let (a, px) = samplePixels(s, p.getBoolOr("sampleAllLayers", true))
    (area, mask) = (a, toneRange(px, lo, hi, min(fuzz(p, 20), 100'f32) / 100 * 255))
  of "outOfGamut":
    # Pixels the current proof setup can't reproduce, as the gamut warning shows them.
    let d = s.activeOrRaise()
    let pv = s.color.proof(d.doc.id)
    let (m, _) = gamutMask(d.doc[], pv.setup, pv.gamutThreshold)
    area = d.doc[].bounds
    mask = newSeq[float32](m.len)
    for i, v in m: mask[i] = float32(v) / 255
  else:
    raise crBad("unknown select `" & preset & "` (sampledColors, reds, yellows, greens, cyans, blues, magentas, highlights, midtones, shadows, outOfGamut)")
  if invert:
    for v in mask.mitems: v = 1 - v
  # Invert the colour result first, then constrain it to the original selection.
  scopeColorRange(mask, s.activeOrRaise().doc[], area, selectionMode)
  setSelection(s, "Color Range", area, mask, selectionMode)

const MaxModifyPixels = 1 shl 30
  ## The largest padded mask Modify works on; beyond it the edit is refused.

proc touchesEdge(m: seq[float32]; w, h: int): bool =
  ## Whether any pixel on the outermost rows or columns of `m` (`w`×`h`) is selected.
  proc sel(m: seq[float32]; w, x, y: int): bool =
    let i = y * w + x
    i >= 0 and i < m.len and m[i] > 0
  for x in 0 ..< w:
    if sel(m, w, x, 0) or sel(m, w, x, max(h - 1, 0)): return true
  for y in 0 ..< h:
    if sel(m, w, 0, y) or sel(m, w, max(w - 1, 0), y): return true
  false

proc padMask(m: seq[float32]; w, h, pad: int; outsideEmpty: bool): seq[float32] =
  ## `m` with a `pad`-pixel margin: unselected when `outsideEmpty`, else the nearest edge pixel
  ## repeated (the selection continues past the canvas).
  let pw = w + 2 * pad
  result = newSeq[float32](pw * (h + 2 * pad))
  if w == 0 or h == 0: return
  for py in 0 ..< h + 2 * pad:
    if outsideEmpty and not (py >= pad and py < pad + h): continue
    let y = min(max(py - pad, 0), h - 1)
    if (y + 1) * w > m.len: continue
    let o = py * pw
    for x in 0 ..< w: result[o + pad + x] = m[y * w + x]
    if not outsideEmpty:
      for x in 0 ..< pad:
        result[o + x] = m[y * w]
        result[o + pad + w + x] = m[y * w + w - 1]

proc cropMask(m: seq[float32]; pw, pad, w, h: int): seq[float32] =
  ## The `w`×`h` centre of a mask padded by `padMask`.
  result = newSeqOfCap[float32](w * h)
  let pw = max(pw, 1)
  for row in pad ..< pad + h:
    let o = row * pw + pad
    if o + w <= m.len:
      for x in 0 ..< w: result.add m[o + x]

proc modify(s: var Session; p: JsonNode; op: string): JsonNode =
  # The dialogs' ranges (Photoshop's limits); anything else is refused, never clamped silently.
  let (mn, mx) = case op
    of "border": (1'f32, 200'f32)
    of "feather": (0.1'f32, 1000'f32)
    else: (1'f32, 500'f32)
  let r = fNum(p, "radius", 1)
  if not (r >= mn and r <= mx):
    raise bad("select.modify." & op, "radius must be a number in " & displayFloat(mn) & ".." & displayFloat(mx))
  # "Apply effect at canvas bounds": the canvas edge is a selection edge when on. Border always
  # follows the canvas edge.
  let atBounds = op == "border" or p.getBoolOr("applyAtCanvasBounds", false)
  let (area, m) = currentMask(s.activeOrRaise().doc[])
  let (w, h) = (area.width, area.height)
  # Feather reaches 3σ = 3 r; the others reach r. A margin past that keeps the canvas edge out of
  # reach; a selection clear of the canvas edge needs none.
  let reach = if op == "feather": r * 3 else: r
  let pad = if touchesEdge(m, w, h): int(ceil(reach)) + 2 else: 0
  let (pw, ph) = (w + 2 * pad, h + 2 * pad)
  if pw > 0 and ph > MaxModifyPixels div max(pw, 1) or pw * ph > MaxModifyPixels:
    raise bad("select.modify." & op, "the selection is too large to modify")
  # Unpadded (the usual case) the mask is used as it is.
  let padded = if pad == 0: m else: padMask(m, w, h, pad, atBounds)
  var outp = case op
    of "expand": expand(padded, pw, ph, r)
    of "contract": contract(padded, pw, ph, r)
    of "border": border(padded, pw, ph, r)
    of "smooth": smooth(padded, pw, ph, r)
    else: feather(padded, pw, ph, r)
  if pad != 0: outp = cropMask(outp, pw, pad, w, h)
  let label = case op
    of "expand": "Expand Selection"
    of "contract": "Contract Selection"
    of "border": "Border Selection"
    of "smooth": "Smooth Selection"
    else: "Feather Selection"
  setSelection(s, label, area, outp, smReplace)

proc growSimilar(s: var Session; p: JsonNode; contiguous: bool): JsonNode =
  ## Grow (contiguous) / Similar (anywhere): pixels whose colour lies within the selected pixels'
  ## colour range widened by the tolerance.
  let tol = fNum(p, "tolerance", 32) / 255
  let (area, px) = samplePixels(s, p.getBoolOr("sampleAllLayers", false))
  let (_, m) = currentMask(s.activeOrRaise().doc[])
  var lo = [float32.high, float32.high, float32.high, float32.high]
  var hi = [float32.low, float32.low, float32.low, float32.low]
  for i in 0 ..< min(px.len, m.len):
    if m[i] >= 0.5:
      for c in 0 .. 3:
        lo[c] = rmin(lo[c], px[i][c])
        hi[c] = rmax(hi[c], px[i][c])
  if lo[0] > hi[0]: raise other("selection is empty")
  proc inside(q: array[4, float32]; lo, hi: array[4, float32]; tol: float32): bool =
    for c in 0 .. 3:
      if not (q[c] >= lo[c] - tol - 1e-6'f32 and q[c] <= hi[c] + tol + 1e-6'f32): return false
    true
  let (w, h) = (area.width, area.height)
  var outp = newSeq[float32](m.len)
  for i, v in m: outp[i] = if v >= 0.5: 1'f32 else: 0'f32
  if contiguous:
    var stack: seq[int]
    for i in 0 ..< outp.len:
      if outp[i] > 0: stack.add i
    while stack.len > 0:
      let i = stack.pop()
      let (x, y) = (i mod w, i div w)
      for j in [(if x > 0: i - 1 else: -1), (if x + 1 < w: i + 1 else: -1), (if y > 0: i - w else: -1), (if y + 1 < h: i + w else: -1)]:
        if j >= 0 and outp[j] == 0 and inside(px[j], lo, hi, tol):
          outp[j] = 1
          stack.add j
  else:
    for i in 0 ..< min(outp.len, px.len):
      if inside(px[i], lo, hi, tol): outp[i] = 1
  setSelection(s, if contiguous: "Grow" else: "Similar", area, outp, smReplace)

proc polygonSelection*(s: var Session; cmdId, label: string; pts: seq[(float32, float32)]; p: JsonNode): JsonNode =
  ## Selects the polygon `pts` with the lasso tools' `mode`, `antiAlias` and `feather` params, as
  ## one history step called `label`.
  let fth = fNum(p, "feather", 0)
  if not (fth >= 0 and fth <= 1000): raise bad(cmdId, "feather must be a number in 0..1000")
  let area = s.activeOrRaise().doc[].bounds
  var mask = polygon(pts, area, p.getBoolOr("antiAlias", true))
  if fth > 0: mask = feather(mask, area.width, area.height, fth)
  setSelection(s, label, area, mask, selMode(p))

proc lasso(s: var Session; p: JsonNode): JsonNode =
  var pts: seq[(float32, float32)]
  let a = p.pget("points")
  if a.isArray:
    for q in a.elems:
      let x = (if q.isArray and q.len > 0: q.elems[0] else: nil).asF64
      let y = (if q.isArray and q.len > 1: q.elems[1] else: nil).asF64
      if x.isSome and y.isSome: pts.add (float32(x.get), float32(y.get))
  if pts.len < 3: raise bad("select.lasso", "needs at least 3 points")
  polygonSelection(s, "select.lasso", "Lasso", pts, p)

proc modifySpec(op, label, params: string; ): CommandSpec =
  cmd("select.modify." & op, label, ["Select", "Modify"], "", params, hasSelectionE, runProc(modify(s, p, op)))

proc selectionSpecs*(): seq[CommandSpec] =
  @[
    cmd("select.magicWand", "Magic Wand", [], "",
        """{"x":px,"y":px,"tolerance":0..255=32,"contiguous":bool=true,"antiAlias":bool=true,"sampleAllLayers":bool=false,"mode":"replace|add|subtract|intersect"="replace"} (samples the visible alpha, Quick Mask, or layer mask when that is the picture)""",
        hasDocE, magicWand),
    cmd("select.colorRange", "Color Range…", ["Select"], "",
        """{"select":"sampledColors|reds|yellows|greens|cyans|blues|magentas|highlights|midtones|shadows|outOfGamut"="sampledColors","color":"#rrggbb"=foreground,"colors":["#rrggbb",…]?,"points":[[x,y],…]? (eyedropper samples),"subtractPoints":[[x,y],…]? (minus eyedropper),"order":["+"|"-",…]? (click order of points and subtractPoints; default all points first),"fuzziness":0..200=40 (tones: 0..100 %=20),"localized":bool=false,"range":0..100=100 (% of the longer side),"tonalRange":level|[lo,hi] (shadows 65, highlights 190, midtones [105,150]),"invert":bool=false,"sampleAllLayers":bool=true,"mode":"replace|add|subtract|intersect"="replace"}""",
        hasDocE, colorRangeCmd),
    modifySpec("border", "Border…", """{"radius":1..200=1}"""),
    modifySpec("smooth", "Smooth…", """{"radius":1..500=1,"applyAtCanvasBounds":bool=false}"""),
    modifySpec("expand", "Expand…", """{"radius":1..500=1,"applyAtCanvasBounds":bool=false}"""),
    modifySpec("contract", "Contract…", """{"radius":1..500=1,"applyAtCanvasBounds":bool=false}"""),
    modifySpec("feather", "Feather…", """{"radius":0.1..1000=1,"applyAtCanvasBounds":bool=false}"""),
    cmd("select.grow", "Grow", ["Select"], "", """{"tolerance":0..255=32,"sampleAllLayers":bool=false}""", hasSelectionE,
        runProc(growSimilar(s, p, true))),
    cmd("select.similar", "Similar", ["Select"], "", """{"tolerance":0..255=32,"sampleAllLayers":bool=false}""", hasSelectionE,
        runProc(growSimilar(s, p, false))),
    cmd("select.lasso", "Lasso", [], "",
        """{"points":[[x,y],…],"mode":"replace|add|subtract|intersect"="replace","antiAlias":bool=true,"feather":px=0}""",
        hasDocE, lasso),
  ]
