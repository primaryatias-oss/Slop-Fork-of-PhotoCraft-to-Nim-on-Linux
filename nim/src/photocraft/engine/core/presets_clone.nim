## Window › Clone Source (`presets/clone_source.rs`): five clone source slots for Clone Stamp and
## Healing Brush, each with a sample point, an offset, W/H scale, rotation and flips, plus the
## overlay options. The transform maps the destination around the anchor onto the source around
## the sample point: a 200 % width paints the source twice as wide, a 30° rotation paints it turned
## 30° counter-clockwise on screen. Part of the `engine/core` unit: included, not imported.

const CloneSlots* = 5

proc defaultCloneSource*(): CloneSource = CloneSource(scale: [100.0, 100.0])
proc defaultCloneOverlay*(): CloneOverlay =
  CloneOverlay(show: true, opacity: 100, clipped: true, autoHide: true, invert: false, blend: "normal")
proc defaultCloneSources*(): CloneSources =
  for i in 0 ..< CloneSlots: result.slots[i] = defaultCloneSource()
  result.overlay = defaultCloneOverlay()

proc activeSlot*(c: CloneSources): CloneSource = c.slots[min(c.active, CloneSlots - 1)]
proc activeSlotIndex(c: CloneSources): int = min(c.active, CloneSlots - 1)

proc isIdentity*(c: CloneSource): bool =
  abs(c.scale[0] - 100.0) < 1e-9 and abs(c.scale[1] - 100.0) < 1e-9 and abs(remEuclidF(c.rotation, 360.0)) < 1e-9 and
    not c.flipH and not c.flipV

proc offset*(c: CloneSource): Option[array[2, float64]] =
  ## The panel's Offset X/Y: source − anchor (once a stroke paired them).
  if c.source.isNone or c.anchor.isNone: return
  some([c.source.get[0] - c.anchor.get[0], c.source.get[1] - c.anchor.get[1]])

proc toJson(c: CloneSource; i: int): JsonNode =
  proc opt(v: Option[array[2, float64]]): JsonNode = (if v.isSome: %v.get else: newJNull())
  %*{"index": i, "source": opt(c.source), "anchor": opt(c.anchor), "offset": opt(c.offset),
     "layer": (if c.layer.isSome: %c.layer.get else: newJNull()), "width": c.scale[0], "height": c.scale[1],
     "linked": c.linked, "rotation": c.rotation, "flipH": c.flipH, "flipV": c.flipV}

proc toJson(o: CloneOverlay): JsonNode =
  %*{"show": o.show, "opacity": o.opacity, "clipped": o.clipped, "auto_hide": o.autoHide, "invert": o.invert, "blend": o.blend}

proc isTranslation*(m: CloneMapping): bool =
  abs(m.m[0] - 1.0) < 1e-12 and abs(m.m[1]) < 1e-12 and abs(m.m[2]) < 1e-12 and abs(m.m[3] - 1.0) < 1e-12

proc offset*(m: CloneMapping): (int, int) =
  ## Integer translation (identity transform).
  (asI32(round(m.source[0] - m.anchor[0])), asI32(round(m.source[1] - m.anchor[1])))

proc map*(m: CloneMapping; x, y: float64): (float64, float64) =
  let dx = x - m.anchor[0]
  let dy = y - m.anchor[1]
  (m.source[0] + m.m[0] * dx + m.m[1] * dy, m.source[1] + m.m[2] * dx + m.m[3] * dy)

proc sourceRect*(m: CloneMapping; dst: Rect): Rect =
  ## Source pixels needed to resample `dst` (with a one-pixel apron for bilinear).
  let pts = [m.map(float64(dst.x0), float64(dst.y0)), m.map(float64(dst.x1), float64(dst.y0)),
             m.map(float64(dst.x0), float64(dst.y1)), m.map(float64(dst.x1), float64(dst.y1))]
  var (x0, y0, x1, y1) = (F64Max, F64Max, -F64Max, -F64Max)
  for (x, y) in pts:
    x0 = rmin(x0, x)
    y0 = rmin(y0, y)
    x1 = rmax(x1, x)
    y1 = rmax(y1, y)
  # Input-derived coordinates can be arbitrarily large: clamp to the i32 range before casting
  # and apply the apron with saturating arithmetic.
  proc apron(v: float64; pad: int): int =
    let c = clamp(v, float64(low(int32)), float64(high(int32)))
    satAdd(asI32(c), pad)
  rect(apron(floor(x0), -2), apron(floor(y0), -2), apron(ceil(x1), 2), apron(ceil(y1), 2))

proc transformMatrix*(scale: array[2, float64]; rotationDeg: float64; flipH, flipV: bool): array[4, float64] =
  ## Matrix taking destination offsets to source offsets for a slot's W/H, rotation and flips.
  let sx = rmax(scale[0] / 100.0, 1e-3) * (if flipH: -1.0 else: 1.0)
  let sy = rmax(scale[1] / 100.0, 1e-3) * (if flipV: -1.0 else: 1.0)
  # Forward (source → destination): rotate after scaling. Rotation is counter-clockwise on
  # screen, which is clockwise in y-down maths.
  let a = toRadians(-rotationDeg)
  let (s, c) = (sin(a), cos(a))
  let f = [c * sx, -s * sy, s * sx, c * sy]
  let det = f[0] * f[3] - f[1] * f[2]
  [f[3] / det, -f[1] / det, -f[2] / det, f[0] / det]

proc clonePt(p: JsonNode; k: string): Option[(float64, float64)] =
  let a = p.pget(k)
  if not a.isArray or a.len < 2: return
  let (x, y) = (a.elems[0].asF64, a.elems[1].asF64)
  if x.isNone or y.isNone: return
  some((x.get, y.get))

proc resolveClone*(s: Session; p: JsonNode; first: (float64, float64); cmdId: string): (CloneMapping, Option[array[2, float64]]) =
  ## A clone stroke's mapping from its params and the active slot, and the anchor an aligned
  ## stroke from the slot stores.
  let aligned = p.getBoolOr("aligned", true)
  let slot = s.presets.clone.activeSlot
  let explicitTf = ["scale", "rotation", "flipH", "flipV"].anyIt(not p.pget(it).isNil)
  var m: array[4, float64]
  if explicitTf:
    var sc = [100.0, 100.0]
    let a = p.pget("scale")
    if a.isArray:
      sc = [(if a.len > 0: a.elems[0].asF64.get(100.0) else: 100.0), (if a.len > 1: a.elems[1].asF64.get(100.0) else: 100.0)]
    m = transformMatrix(sc, p.pget("rotation").asF64.get(0.0), p.getBoolOr("flipH", false), p.getBoolOr("flipV", false))
  else: m = transformMatrix(slot.scale, slot.rotation, slot.flipH, slot.flipV)
  let anchorParam = clonePt(p, "anchor")
  let off = clonePt(p, "offset")
  if off.isSome:
    let a = anchorParam.get(first)
    return (CloneMapping(source: (a[0] + off.get[0], a[1] + off.get[1]), anchor: a, m: m), none(array[2, float64]))
  let src = clonePt(p, "source")
  if src.isSome:
    return (CloneMapping(source: src.get, anchor: anchorParam.get(first), m: m), none(array[2, float64]))
  if slot.source.isNone:
    raise bad(cmdId, "missing `source` ([x,y]) or `offset` ([dx,dy]); or set one with cloneSource.set")
  let anchor = (if aligned and slot.anchor.isSome: (slot.anchor.get[0], slot.anchor.get[1]) else: first)
  (CloneMapping(source: (slot.source.get[0], slot.source.get[1]), anchor: anchor, m: m),
   (if aligned: some([anchor[0], anchor[1]]) else: none(array[2, float64])))

proc cloneMapping*(s: var Session; p: JsonNode; first: (float64, float64); cmdId: string): CloneMapping =
  ## `resolveClone`, remembering the anchor of an aligned stroke in the slot.
  let (m, anchor) = resolveClone(s, p, first, cmdId)
  if anchor.isSome: s.presets.clone.slots[s.presets.clone.activeSlotIndex].anchor = anchor
  m

proc overlayBlend*(mode: string; src, dst: array[4, float32]): array[4, float32] =
  ## The Clone Source overlay's blend mode applied to one pixel (unknown modes draw as Normal).
  result = src
  if mode notin ["darken", "lighten", "difference"]: return
  let a = clamp(dst[3], 0'f32, 1'f32)
  for i in 0 ..< 3:
    let (sv, dv) = (src[i], dst[i])
    let f = (case mode
             of "darken": min(sv, dv)
             of "lighten": max(sv, dv)
             else: abs(sv - dv))
    result[i] = sv + (f - sv) * a

proc resampleClone*(src: PxRegion; dst: Rect; m: CloneMapping; alpha: Option[int]): PxRegion =
  ## Bilinear resample of `src` (covering `sourceRect`) onto `dst`, interpolating premultiplied by
  ## the alpha channel when there is one. Outside `src` is transparent.
  let n = src.ch
  result = newPxRegion(dst, n)
  let w = dst.width
  var q: array[4, seq[float32]]
  for k in 0 ..< 4: q[k] = newSeq[float32](n)
  proc fetch(x, y: int; buf: var seq[float32]) =
    if x < src.rect.x0 or y < src.rect.y0 or x >= src.rect.x1 or y >= src.rect.y1:
      for v in buf.mitems: v = 0
      return
    let at = ((y - src.rect.y0) * src.rect.width + (x - src.rect.x0)) * n
    for c in 0 ..< n: buf[c] = src.data[at + c]
    if alpha.isSome:
      let av = buf[alpha.get]
      for c in 0 ..< n:
        if c != alpha.get: buf[c] *= av
  for i in 0 ..< dst.width * dst.height:
    let x = dst.x0 + i mod w
    let y = dst.y0 + i div w
    let (sx, sy) = m.map(float64(x) + 0.5, float64(y) + 0.5)
    let fx = sx - 0.5
    let fy = sy - 0.5
    let x0 = asI32(floor(fx))
    let y0 = asI32(floor(fy))
    let tx = float32(fx - float64(x0))
    let ty = float32(fy - float64(y0))
    fetch(x0, y0, q[0])
    fetch(x0 + 1, y0, q[1])
    fetch(x0, y0 + 1, q[2])
    fetch(x0 + 1, y0 + 1, q[3])
    let o = i * n
    for c in 0 ..< n:
      let top = q[0][c] + (q[1][c] - q[0][c]) * tx
      let bot = q[2][c] + (q[3][c] - q[2][c]) * tx
      result.data[o + c] = top + (bot - top) * ty
    if alpha.isSome:
      let av = result.data[o + alpha.get]
      for c in 0 ..< n:
        if c != alpha.get: result.data[o + c] = (if av > 1e-6'f32: result.data[o + c] / av else: 0'f32)

# ---------------------------------------------------------------- commands

proc cloneIndex(s: Session; p: JsonNode; cmdId: string): int =
  let v = p.pget("index").asU64
  if v.isNone: return s.presets.clone.active
  if v.get < uint64(CloneSlots): return int(v.get)
  raise bad(cmdId, "index " & $v.get & " out of range (0..5)")

proc cloneList(s: var Session): JsonNode =
  let c = s.presets.clone
  var sources = newJArray()
  for i, x in c.slots: sources.add x.toJson(i)
  %*{"active": c.active, "sources": sources, "overlay": c.overlay.toJson}

proc cloneSelect(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "cloneSource.select"
  let v = p.pget("index").asU64
  if v.isNone: raise bad(cmdId, "missing `index`")
  if v.get >= uint64(CloneSlots): raise bad(cmdId, "index " & $v.get & " out of range (0..5)")
  let i = int(v.get)
  s.presets.clone.active = i
  s.presets.clone.activeSlot.toJson(i)

proc cloneSet(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "cloneSource.set"
  let i = cloneIndex(s, p, cmdId)
  let st = s.active
  let activeLayer = (if st.isNil or st.activeLayer.isNone: none(uint64) else: some(uint64(st.activeLayer.get)))
  template slot: untyped = s.presets.clone.slots[i]
  let src = clonePt(p, "source")
  if src.isSome:
    # A new sample point re-pairs with the next stroke.
    slot.source = some([src.get[0], src.get[1]])
    slot.anchor = none(array[2, float64])
    let l = p.pget("layer").asU64
    slot.layer = (if l.isSome: l else: activeLayer)
  let off = clonePt(p, "offset")
  if off.isSome:
    # Offset X/Y fields: keep the anchor (or the source as anchor) and move the source.
    let a = (if slot.anchor.isSome: slot.anchor elif slot.source.isSome: slot.source else: none(array[2, float64]))
    if a.isNone: raise bad(cmdId, "set a `source` before an offset")
    slot.anchor = a
    slot.source = some([a.get[0] + off.get[0], a.get[1] + off.get[1]])
  let sc = p.pget("scale")
  if sc.isArray:
    slot.scale = [(if sc.len > 0: sc.elems[0].asF64.get(100.0) else: 100.0), (if sc.len > 1: sc.elems[1].asF64.get(100.0) else: 100.0)]
  let lk = p.pget("linked").asBool
  if lk.isSome: slot.linked = lk.get
  let before = slot.scale
  let w = p.pget("width").asF64
  let h = p.pget("height").asF64
  if w.isSome: slot.scale[0] = w.get
  if h.isSome: slot.scale[1] = h.get
  # Linked W/H keep their ratio: the field that was not given follows the one that was.
  if slot.linked:
    if w.isSome and h.isNone and abs(before[0]) > 1e-9: slot.scale[1] = before[1] * w.get / before[0]
    elif w.isNone and h.isSome and abs(before[1]) > 1e-9: slot.scale[0] = before[0] * h.get / before[1]
  if slot.scale.anyIt(not (abs(it) >= 1.0 and abs(it) <= 1000.0)):
    slot.scale = [clamp(slot.scale[0], 1.0, 1000.0), clamp(slot.scale[1], 1.0, 1000.0)]
  let r = p.pget("rotation").asF64
  if r.isSome: slot.rotation = remEuclidF(r.get + 180.0, 360.0) - 180.0
  let fh = p.pget("flipH").asBool
  if fh.isSome: slot.flipH = fh.get
  let fv = p.pget("flipV").asBool
  if fv.isSome: slot.flipV = fv.get
  if p.pget("clear").asBool == some(true): slot = defaultCloneSource()
  if p.getBoolOr("select", true): s.presets.clone.active = i
  s.presets.clone.slots[i].toJson(i)

proc cloneResetTransform(s: var Session; p: JsonNode): JsonNode =
  let i = cloneIndex(s, p, "cloneSource.resetTransform")
  s.presets.clone.slots[i].scale = [100.0, 100.0]
  s.presets.clone.slots[i].rotation = 0
  s.presets.clone.slots[i].flipH = false
  s.presets.clone.slots[i].flipV = false
  s.presets.clone.slots[i].toJson(i)

proc cloneOverlayCmd(s: var Session; p: JsonNode): JsonNode =
  template o: untyped = s.presets.clone.overlay
  let show = p.pget("show").asBool
  if show.isSome: o.show = show.get
  let op = p.pget("opacity").asF64
  if op.isSome: o.opacity = clamp(float32(op.get), 0'f32, 100'f32)
  let cl = p.pget("clipped").asBool
  if cl.isSome: o.clipped = cl.get
  let ah = p.pget("autoHide").asBool
  if ah.isSome: o.autoHide = ah.get
  let inv = p.pget("invert").asBool
  if inv.isSome: o.invert = inv.get
  let m = p.pget("blend").asStr
  if m.isSome:
    if m.get notin ["normal", "darken", "lighten", "difference"]:
      raise bad("cloneSource.overlay", "unknown overlay mode `" & m.get & "` (normal|darken|lighten|difference)")
    o.blend = m.get
  o.toJson

proc clonePresetSpecs*(): seq[CommandSpec] =
  @[
    cmd("cloneSource.list", "Clone Sources", [], "",
        "{} → {active,sources:[{index,source,anchor,offset,layer,width,height,linked,rotation,flipH,flipV}],overlay}",
        presetAlwaysE, runProc(cloneList(s)), journal = false),
    cmd("cloneSource.select", "Select Clone Source", [], "", "{\"index\":0..4}", presetAlwaysE, runProc(cloneSelect(s, p))),
    cmd("cloneSource.set", "Set Clone Source", [], "",
        "{\"index\":0..4?=active,\"source\":[x,y]? (⌥-click; re-pairs with the next stroke),\"offset\":[dx,dy]?,\"width\":%?,\"height\":%?|\"scale\":[w%,h%]?,\"linked\":bool? (W/H link: a lone width or height scales the other by the same factor),\"rotation\":deg?,\"flipH\":bool?,\"flipV\":bool?,\"layer\":id?,\"clear\":bool?,\"select\":bool=true}",
        presetAlwaysE, runProc(cloneSet(s, p))),
    cmd("cloneSource.resetTransform", "Reset Transform", [], "", "{\"index\":0..4?=active}", presetAlwaysE,
        runProc(cloneResetTransform(s, p))),
    cmd("cloneSource.overlay", "Clone Source Overlay", [], "",
        "{\"show\":bool?,\"opacity\":0..100?,\"clipped\":bool?,\"autoHide\":bool?,\"invert\":bool?,\"blend\":\"normal|darken|lighten|difference\"?}",
        presetAlwaysE, runProc(cloneOverlayCmd(s, p))),
  ]
