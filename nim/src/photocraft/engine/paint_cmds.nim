## Paint Bucket and the Gradient tool (port of `paint_cmds.rs`).

import std/[json, options, math, strutils]
import ./core, ./selection_cmds
import ../compose/pattern as cpattern
import ../algo/paint as apaint
import ../algo/selection as asel

proc fparam(p: JsonNode; k: string; d: float32): float32 =
  let v = p.pget(k).asF64
  if v.isSome: float32(v.get) else: d

proc bparam(p: JsonNode; k: string; d: bool): bool = p.pget(k).asBool.get(d)

proc hexChannel(h: string; i: int): Option[float32] =
  ## `u8::from_str_radix(&h[i..i+2], 16)` as 0..1.
  if i + 2 > h.len: return
  var t = h[i ..< i + 2]
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
  some(float32(v) / 255'f32)

proc colorValue(v: JsonNode; d: array[4, float32]): array[4, float32] =
  if v.isNil: return d
  case v.kind
  of JArray:
    if v.len < 3: return d
    var c: seq[float32]
    for x in v.elems: c.add float32(x.asF64.get(0.0))
    [c[0], c[1], c[2], (if c.len > 3: c[3] else: 1'f32)]
  of JString:
    var h = v.str
    while h.startsWith("#"): h = h[1 .. ^1]
    let (r, g, b) = (hexChannel(h, 0), hexChannel(h, 2), hexChannel(h, 4))
    if r.isSome and g.isSome and b.isSome: [r.get, g.get, b.get, hexChannel(h, 6).get(1'f32)]
    else: d
  else: d

proc pointParam(p: JsonNode; k: string): Option[(float32, float32)] =
  let a = p.pget(k)
  if not a.isArray or a.len < 2: return
  let (x, y) = (a.elems[0].asF64, a.elems[1].asF64)
  if x.isSome and y.isSome: some((float32(x.get), float32(y.get))) else: none((float32, float32))

proc bucket(s: var Session; p: JsonNode): JsonNode =
  let fg = s.tools.foreground
  let c = colorValue(p.pget("color"), fg)
  let (x, y) = (asI32(float64(floor(fparam(p, "x", 0)))), asI32(float64(floor(fparam(p, "y", 0)))))
  let (tol, contiguous, aa, opacity) = (fparam(p, "tolerance", 32), bparam(p, "contiguous", true), bparam(p, "antiAlias", true),
                                        fparam(p, "opacity", 100) / 100'f32)
  let all = bparam(p, "sampleAllLayers", false) and targetOf(p).kind == tgPixels
  # Other visible layers can define the region, but the fill still writes only to the target.
  var region = none(asel.Region)
  if all:
    let (area, img) = sampleRgba8(s, true)
    region = wandRegion(img, area, (x, y), tol, contiguous, aa)
  # Fill source: foreground colour (default) or a pattern (the Paint Bucket "Fill" dropdown).
  let sv = (if not p.pget("contents").isNil: p.pget("contents") else: p.pget("source"))
  let source = sv.asStr.get("foreground")
  var tile: PatternTile = nil
  var place: Placement
  if source == "pattern":
    let pat = resolvePatternParam(s, "paint.bucket", p)
    tile = newPatternTile(pat)
    if tile.isNil: raise bad("paint.bucket", "the pattern is empty")
    let (scale, angle, _, phase) = patternPlacement(p)
    place = cpattern.placement(EmptyRect, false, phase, scale, angle)
  let filled = s.editWith("Paint Bucket", proc(doc: var Document; active: var Option[LayerId]): bool =
    let area = doc.bounds
    let sel = doc.selection
    let (surf, lock) = targetSurface(doc, active, p)
    var ok: bool
    if all and region.isNone: ok = false
    elif not tile.isNil:
      # Render the pattern over the canvas once, then sample it at each filled pixel.
      let rendered = renderPattern(tile, place, area)
      let w = area.width
      let src = proc(px, py: int): array[4, float32] = rendered[(py - area.y0) * w + (px - area.x0)]
      if region.isSome:
        bucketFillRegionSrc(surf[], region.get, opacity, sel, lock, src)
        ok = true
      else: ok = bucketFillSrc(surf[], area, (x, y), tol, contiguous, aa, opacity, sel, lock, src)
    elif region.isSome:
      bucketFillRegionSrc(surf[], region.get, opacity, sel, lock, proc(px, py: int): array[4, float32] = c)
      ok = true
    else: ok = bucketFill(surf[], area, (x, y), tol, contiguous, aa, c, opacity, sel, lock)
    surf[].prune()
    ok)
  %*{"filled": filled}

proc gradient(s: var Session; p: JsonNode): JsonNode =
  let frm = pointParam(p, "from")
  if frm.isNone: raise bad("paint.gradient", "missing from")
  let to = pointParam(p, "to")
  if to.isNone: raise bad("paint.gradient", "missing to")
  let shape = case p.pget("style").asStr.get("linear")
    of "radial": GradientShape.gsRadial
    of "angle": GradientShape.gsAngle
    of "reflected": GradientShape.gsReflected
    of "diamond": GradientShape.gsDiamond
    else: GradientShape.gsLinear
  let (fg, bg) = (s.tools.foreground, s.tools.background)
  # A preset (`gradient`), explicit `stops`, or the current gradient (Gradients panel); the legacy
  # `colors` list spaces its colours evenly. Explicit `transparency` applies to all.
  var stops: seq[Stop4]
  let ts = toolStops(s, p)
  if ts.isSome: stops = ts.get
  else:
    var colors: seq[array[4, float32]]
    let cv = p.pget("colors")
    if cv.isArray:
      for v in cv.elems: colors.add colorValue(v, fg)
    else: colors = @[fg, bg]
    let n = colors.len
    var cs: GradientStops
    for i, c in colors: cs.add ((if n > 1: float32(i) / float32(n - 1) else: 0'f32), c)
    let tr = transparencyParam(p, "paint.gradient")
    stops = applyOpacity(cs, (if tr.isSome: tr.get else: @[]))
  let reverse = bparam(p, "reverse", false)
  let opacity = fparam(p, "opacity", 100) / 100'f32
  let m = p.pget("mode").asStr
  let blend = (if m.isSome and blendFromStr(m.get).isSome: blendFromStr(m.get).get else: bmNormal)
  let dither = bparam(p, "dither", true) # Photoshop dithers gradients by default (reduces banding).
  let (f, t) = (frm.get, to.get)
  s.edit("Gradient", proc(doc: var Document; active: var Option[LayerId]) =
    let sel = doc.selection
    var area = doc.bounds
    if sel.isSome:
      let b = sel.get.contentBounds
      if not b.isEmpty: area = b
    area = area.intersect(doc.bounds)
    let (surf, lock) = targetSurface(doc, active, p)
    paintGradient(surf[], area, f, t, shape, stops, reverse, opacity, blend, dither, sel, lock))
  newJNull()

proc paintSpecs*(): seq[CommandSpec] =
  @[cmd("paint.bucket", "Paint Bucket", [], "",
        """{"x":px,"y":px,"tolerance":0..255=32,"contiguous":bool=true,"antiAlias":bool=true,"sampleAllLayers":bool=false,"contents":"foreground|pattern"="foreground","color":"#rrggbb"=foreground,"pattern":id|name (contents=pattern),"scale":%=100,"angle":deg,"opacity":1..100=100,"target":"pixels"|"mask"|"quickMask"|{"channel":i}=Channels panel target}""",
        hasPaintableE, runProc(bucket(s, p))),
    cmd("paint.gradient", "Gradient", [], "",
        """{"from":[x,y],"to":[x,y],"style":"linear|radial|angle|reflected|diamond"="linear","colors":["#rrggbb",…]? (evenly spaced),"gradient":preset name?,"stops":[[t,"#rrggbb"|"foreground"|"background"],…]?,"transparency":[[t,0..100],…]? (default: the current gradient, see gradient.presets.select),"reverse":bool=false,"dither":bool=true,"opacity":1..100=100,"mode":"normal|multiply|…"="normal","target":"pixels"|"mask"|"quickMask"|{"channel":i}=Channels panel target}""",
        hasPaintableE, runProc(gradient(s, p)))]
