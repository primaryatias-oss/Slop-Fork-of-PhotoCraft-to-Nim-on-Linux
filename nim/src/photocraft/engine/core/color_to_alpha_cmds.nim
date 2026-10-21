## Filter › Other › Color to Alpha (`filter.other.colorToAlpha`, port of
## `color_to_alpha_cmds.rs`): turns a chosen colour into transparency, unmixing it from partly
## covered pixels so antialiased edges keep their shape over any new background.
##
## It runs through `runFilter` like every other filter, which turns a Background layer into a
## normal layer first, as the erasers do. A layer whose transparency is locked is refused rather
## than silently getting its old alpha back. Part of the `engine/core` unit: included, not
## imported.

proc ctaBad(msg: string): ref EngineError = bad(ColorToAlphaId, msg)

proc unitVal(v, default: float32): float32 =
  ## A colour channel value: finite and clamped to 0–1.
  if v == v and abs(v) != Inf: clamp(v, 0'f32, 1'f32) else: default

proc colorToAlphaParams*(p: JsonNode): FilterParams =
  ## Algorithm parameters (lenient: out-of-range values are clamped, unreadable ones fall back to
  ## the defaults). The command validates strictly before it gets here.
  let c = colorParam(p, "color", [1'f32, 1, 1, 1])
  proc num(p: JsonNode; k: string; d: float32): float32 =
    let v = p.pget(k).asF64
    if v.isSome: unitVal(float32(v.get), d) else: d
  result = newFilterParams(fltColorToAlpha)
  result.color = [unitVal(c[0], 1), unitVal(c[1], 1), unitVal(c[2], 1), 1'f32]
  result.transparencyThreshold = num(p, "transparencyThreshold", 0)
  result.opacityThreshold = num(p, "opacityThreshold", 1)

proc ctaThreshold(p: JsonNode; key: string) =
  let v = p.pget(key)
  if v.isNil: return
  let x = v.asF64
  if x.isNone: raise ctaBad("`" & key & "` must be a number in 0..1")
  if not (x.get == x.get and abs(x.get) != Inf and x.get >= 0 and x.get <= 1): raise ctaBad("`" & key & "` must be in 0..1")

proc ctaValidate(p: JsonNode) =
  ## Rejects params the lenient `colorToAlphaParams` would otherwise quietly replace.
  if not p.isObject: raise ctaBad("params must be a JSON object")
  let c = p.pget("color")
  if not c.isNil:
    if c.kind == JString:
      if parseHex(c.str).isNone: raise ctaBad("`color` must be \"#rrggbb\", not " & debugStr(c.str))
    elif c.isArray and c.len >= 3 and c.len <= 4:
      for v in c.elems:
        let x = v.asF64
        if x.isNone or not (x.get == x.get and abs(x.get) != Inf and x.get >= 0 and x.get <= 1):
          raise ctaBad("`color` as an array must hold 3 or 4 numbers in 0..1")
    else: raise ctaBad("`color` must be \"#rrggbb\" or [r, g, b] in 0..1")
  ctaThreshold(p, "transparencyThreshold")
  ctaThreshold(p, "opacityThreshold")

proc ctaEnabled(s: Session): string =
  ## A pixel layer (or smart object) in an RGB or Grayscale document whose pixels and
  ## transparency may change. The Background counts: it becomes a normal layer when run.
  if editsChannel(s): return "Color to Alpha works on layer pixels, not on a channel or Quick Mask"
  let why = hasFilterableLayerE(s)
  if why.len > 0: return why
  let d = s.active
  if d.isNil: return "no document open"
  if d.doc.mode notin {cmRgb, cmGrayscale}:
    return "Color to Alpha needs an RGB or Grayscale document (this one is " & $d.doc.mode & ")"
  let (l, lwhy) = activeLayerOf(s)
  if l.isNil: return lwhy
  let locks = d.doc[].effectiveLocks(l.id)
  if locks.pixels or locks.all: return "the layer \"" & l.name & "\" is locked"
  if locks.transparency and not isBackground(l[]): return "the transparency of layer \"" & l.name & "\" is locked"
  ""

proc colorToAlphaSpecs*(): seq[CommandSpec] =
  @[cmd(ColorToAlphaId, "Color to Alpha…", ["Filter", "Other"], "",
        """{"color":color=#ffffff,"transparencyThreshold":0..1=0,"opacityThreshold":0..1=1}""", ctaEnabled, runProc do:
      ctaValidate(p)
      runFilter(s, ColorToAlphaId, p))]
