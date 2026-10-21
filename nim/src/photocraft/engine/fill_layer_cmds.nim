## Editing fill layers (ports of `solid_fill_cmds.rs` and `pattern_fill_cmds.rs`): the Solid Color
## picker and the Pattern Fill dialog (pattern, Angle, Scale, Link with Layer, Snap to Origin)
## commit one undoable content edit each, independent of the layer's masks.

import std/[json, options, strutils]
import ./core
import ../rfmt

const
  SolidSetCmd* = "layer.fill.solidColor.set"
  PatternSetCmd* = "layer.fill.pattern.set"
  PatternCreateCmd* = "layer.newFillLayer.pattern"
  PatternScaleRange* = (1.0, 1000.0) ## Photoshop's Scale range in the Pattern Fill dialog (percent)
  MaxPhase = 1.0e7                   ## largest phase (origin offset) accepted, in pixels

proc isFin(v: float64): bool {.inline.} = v == v and abs(v) != Inf

proc docE(s: Session): string = (if s.active.isNil: "no document open" else: "")

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

proc colorParam*(value: JsonNode; original: Color; mode: ColorMode): Color =
  ## Parse an RGB picker value or a full colour in its stored model. A three-component RGB value
  ## preserves alpha; opening the picker must not make a translucent fill opaque.
  template sbad(msg: string): untyped = bad(SolidSetCmd, msg)
  case value.kind
  of JString:
    let h = (if value.str.startsWith("#"): value.str[1 .. ^1] else: value.str)
    proc channel(i: int): Option[float32] =
      if i + 2 > h.len: return
      let b = hexByte(h[i ..< i + 2])
      if b.isSome: some(float32(b.get) / 255'f32) else: none(float32)
    if h.len != 6 and h.len != 8: raise sbad("color must be #rrggbb or #rrggbbaa")
    let (r, g, b) = (channel(0), channel(2), channel(4))
    if r.isNone or g.isNone or b.isNone: raise sbad("invalid hexadecimal color")
    var alpha = original.alpha
    if h.len == 8:
      let a = channel(6)
      if a.isNone: raise sbad("invalid alpha")
      alpha = a.get
    result = rgba(r.get, g.get, b.get, alpha).inMode(mode)
  of JArray:
    if value.len < 3 or value.len > 4: raise sbad("color must be hexadecimal RGB, [r,g,b,a?], or a stored Color")
    proc channel(i: int): Option[float32] =
      let v = value.elems[i].asF64
      if v.isSome and isFin(v.get) and v.get >= 0 and v.get <= 1: some(float32(v.get)) else: none(float32)
    let (r, g, b) = (channel(0), channel(1), channel(2))
    if r.isNone or g.isNone or b.isNone: raise sbad("RGB components must be finite numbers within 0..1")
    var alpha = original.alpha
    if value.len == 4:
      let a = channel(3)
      if a.isNone: raise sbad("alpha must be a finite number within 0..1")
      alpha = a.get
    result = rgba(r.get, g.get, b.get, alpha).inMode(mode)
  of JObject:
    try: fromJ(value, result)
    except CatchableError: raise sbad("invalid stored Color")
  else: raise sbad("color must be hexadecimal RGB, [r,g,b,a?], or a stored Color")
  for v in result.c:
    if v != v or abs(v) == Inf: raise sbad("color must be finite and alpha within 0..1")
  if result.alpha != result.alpha or not (result.alpha >= 0 and result.alpha <= 1):
    raise sbad("color must be finite and alpha within 0..1")

proc checkTarget(s: Session; p: JsonNode; cmdId: string): ptr DocState =
  if not p.isObject or (not p.pget("layer").isNil and p.pget("layer").asU64.isNone):
    raise bad(cmdId, "params must be an object and layer an unsigned id")
  result = s.active
  if result.isNil: raise noDocument()
  let dv = p.pget("document")
  if not dv.isNil and dv.asU64 != some(uint64(result.doc.id)):
    raise bad(cmdId, "the target document is no longer active")

proc solidSet(s: var Session; p: JsonNode): JsonNode =
  let st = checkTarget(s, p, SolidSetCmd)
  let id = layerParam(s, p)
  let layer = st.doc[].findLayer(id)
  if layer.isNil: raise noLayer(id)
  if layer[].content.kind != lkFill or layer[].content.fill.kind != fkSolid:
    raise bad(SolidSetCmd, "the target must be a Solid Color Fill layer")
  let original = layer[].content.fill.color
  let cv = p.pget("color")
  if cv.isNil: raise bad(SolidSetCmd, "color is required")
  let color = colorParam(cv, original, st.doc.mode)
  if color != original:
    checkPixelsUnlocked(st.doc[], id)
    if st.doc[].effectiveLocks(id).transparency and color.alpha != original.alpha:
      raise bad(SolidSetCmd, "the layer's transparency is locked")
    s.edit("Solid Color", proc(doc: var Document; active: var Option[LayerId]) =
      let l = doc.findLayer(id)
      if l.isNil: raise noLayer(id)
      l[].content = LayerContent(kind: lkFill, fill: Fill(kind: fkSolid, color: color)))
  %*{"layer": uint64(id), "color": toJ(color)}

proc edited(s: Session; base: Fill; p: JsonNode): (Fill, Option[DocPattern]) =
  ## The pattern fill `p` asks for, each param optional and defaulting to `base`. Returns the new
  ## fill and the pattern to copy into the document (when `pattern` was given).
  if base.kind != fkPattern: raise bad(PatternSetCmd, "the target must be a Pattern Fill layer")
  proc num(k: string; lo, hi: float64): Option[float32] =
    let v = p.pget(k)
    if v.isNull: return none(float32)
    let x = v.asF64
    if x.isSome and isFin(x.get) and x.get >= lo and x.get <= hi: return some(float32(x.get))
    raise bad(PatternSetCmd, "`" & k & "` must be a number within " & displayF64(lo) & ".." & displayF64(hi))
  var fill = base
  var pat = none(DocPattern)
  let pv = p.pget("pattern")
  if not pv.isNull:
    if pv.kind != JString or pv.str.len == 0: raise bad(PatternSetCmd, "`pattern` is a pattern id or name")
    let r = resolvePattern(s, pv.str)
    if r.isNone: raise bad(PatternSetCmd, "no pattern \"" & pv.str & "\" (see pattern.list)")
    pat = r
  let sc = num("scale", PatternScaleRange[0], PatternScaleRange[1])
  if sc.isSome: fill.scale = sc.get / 100'f32
  let an = num("angle", -180, 180)
  if an.isSome: fill.angle = an.get
  let lv = p.pget("link")
  if not lv.isNull:
    let b = lv.asBool
    if b.isNone: raise bad(PatternSetCmd, "`link` is true or false")
    fill.link = b.get
  let ph = p.pget("phase")
  if not ph.isNull:
    if not ph.isArray or ph.len != 2: raise bad(PatternSetCmd, "`phase` is [x, y]")
    proc c(i: int): Option[float32] =
      let v = ph.elems[i].asF64
      if v.isSome and isFin(v.get) and abs(v.get) <= MaxPhase: some(float32(v.get)) else: none(float32)
    let (x, y) = (c(0), c(1))
    if x.isNone or y.isNone: raise bad(PatternSetCmd, "`phase` needs finite coordinates within ±" & displayF64(MaxPhase))
    fill.phase = (x.get, y.get)
  if pat.isSome:
    fill.name = pat.get.name
    fill.id = pat.get.id
  (fill, pat)

proc patternTarget(s: Session; p: JsonNode): (LayerId, Fill, Fill, Option[DocPattern]) =
  ## The target layer and the fill `p` gives it, checked against the active document.
  let st = checkTarget(s, p, PatternSetCmd)
  let id = layerParam(s, p)
  let layer = st.doc[].findLayer(id)
  if layer.isNil: raise noLayer(id)
  if layer[].content.kind != lkFill or layer[].content.fill.kind != fkPattern:
    raise bad(PatternSetCmd, "the target must be a Pattern Fill layer")
  let original = layer[].content.fill
  let (fill, pat) = edited(s, original, p)
  (id, original, fill, pat)

proc patternInfo(layer: LayerId; fill: Fill): JsonNode =
  if fill.kind != fkPattern: return %*{"layer": uint64(layer)}
  %*{"layer": uint64(layer), "pattern": fill.id, "scale": fill.scale * 100'f32, "angle": fill.angle, "link": fill.link,
     "phase": [fill.phase[0], fill.phase[1]]}

proc patternSet(s: var Session; p: JsonNode): JsonNode =
  let (id, original, fill, pat) = patternTarget(s, p)
  if fill != original:
    let st = s.active
    if st.isNil: raise noDocument()
    checkPixelsUnlocked(st.doc[], id)
    s.edit("Pattern Fill", proc(doc: var Document; active: var Option[LayerId]) =
      if pat.isSome: ensureInDoc(doc, pat.get)
      let l = doc.findLayer(id)
      if l.isNil: raise noLayer(id)
      l[].content = LayerContent(kind: lkFill, fill: fill))
  patternInfo(id, fill)

proc patternFillPreview*(s: Session; cmdId: string; p: JsonNode): Document =
  ## The active document as `layer.fill.pattern.set` or `layer.newFillLayer.pattern` with `p`
  ## would leave it, without a history step: the Pattern Fill dialog's live preview.
  let st = s.active
  if st.isNil: raise noDocument()
  result = st.doc[]
  case cmdId
  of PatternSetCmd:
    let (id, _, fill, pat) = patternTarget(s, p)
    if pat.isSome: ensureInDoc(result, pat.get)
    let l = result.findLayer(id)
    if l.isNil: raise noLayer(id)
    l[].content = LayerContent(kind: lkFill, fill: fill)
  of PatternCreateCmd:
    let pat = resolvePatternParam(s, PatternCreateCmd, p)
    var active = st.activeLayer
    discard insertPatternFillLayer(result, active, pat, p)
  else: raise bad(cmdId, "no Pattern Fill preview for this command")

proc solidFillSpecs*(): seq[CommandSpec] =
  @[cmd(SolidSetCmd, "Solid Color…", [], "",
        """{"layer":id? (default active),"document":id? (reject a stale dialog),"color":"#rrggbb"|"#rrggbbaa"|[r,g,b,a?]|{mode,c:[f32;4],alpha:f32}}; RGB values without alpha preserve it; stored Color preserves its model and precision""",
        docE, runProc(solidSet(s, p)))]

proc patternFillSpecs*(): seq[CommandSpec] =
  @[cmd(PatternSetCmd, "Pattern Fill…", [], "",
        """{"layer":id? (default active; a Pattern Fill layer),"document":id? (reject a stale dialog),"pattern":id|name?,"scale":1..1000?,"angle":-180..180?,"link":bool?,"phase":[x,y]? (Snap to Origin: [0,0])}; omitted values keep the layer's""",
        docE, runProc(patternSet(s, p)))]
