## Window › Gradients (`presets/gradients.rs`): gradient presets in groups, the Gradient tool's
## current gradient and Gradient Fill layers made from presets. A preset has colour stops (a fixed
## colour, or the live foreground/background colour) and transparency stops, both at `0..=1`.
## Midpoints are always 50 %. Built-in colours are our own. Part of the `engine/core` unit.

type
  GradientStops* = seq[(float32, array[4, float32])] ## RGBA stops `(location, colour)` to paint
  GradientFillStops* = (seq[(float32, Color)], seq[(float32, float32)])

const
  GradientSelectCmd = "gradient.presets.select"
  GradientStopCmd* = "gradient.presets.stop"

proc stopFg(): StopColor = StopColor(kind: scForeground)
proc stopBg(): StopColor = StopColor(kind: scBackground)
proc stopRgb(c: array[3, float32]): StopColor = StopColor(kind: scRgb, rgb: c)

proc gradHex(s: string): array[3, float32] =
  var h = s
  while h.startsWith("#"): h = h[1 .. ^1]
  for i in 0 ..< 3:
    let part = (if i * 2 + 2 <= h.len: h[i * 2 ..< i * 2 + 2] else: "")
    var v = 0.0'f32
    if part.len == 2 and part.allCharsInSet(HexDigits): v = float32(parseHexInt(part)) / 255'f32
    result[i] = v

proc gradHexOf(c: array[3, float32]): string =
  proc q(v: float32): int = int(round(clamp(v, 0'f32, 1'f32) * 255'f32))
  "#" & toHex(q(c[0]), 2).toLowerAscii & toHex(q(c[1]), 2).toLowerAscii & toHex(q(c[2]), 2).toLowerAscii

proc lerp4(a, b: array[4, float32]; t: float32): array[4, float32] =
  for i in 0 ..< 4: result[i] = a[i] + (b[i] - a[i]) * t

proc sampleStops[V](stops: openArray[(float32, V)]; t: float32; mix: proc(a, b: V; u: float32): V): Option[V] =
  ## Piecewise-linear value of sorted `(t, v)` stops at `t` (clamped at the ends).
  if stops.len == 0: return
  if t <= stops[0][0]: return some(stops[0][1])
  for i in 0 ..< stops.len - 1:
    let (t0, a) = stops[i]
    let (t1, b) = stops[i + 1]
    if t <= t1:
      let u = (if t1 > t0: (t - t0) / (t1 - t0) else: 1'f32)
      return some(mix(a, b, u))
  some(stops[^1][1])

proc sortByLocation[V](s: var seq[(float32, V)]) =
  s.sort(proc(a, b: (float32, V)): int = totalCmp(a[0], b[0]))

proc newGradientPreset*(name: string; colors: openArray[string]): GradientPreset =
  let n = max(colors.len, 2) - 1
  result.name = name
  for i, c in colors: result.stops.add (float32(i) / float32(n), stopRgb(gradHex(c)))

proc foregroundToBackground*(): GradientPreset =
  GradientPreset(name: "Foreground to Background", stops: @[(0'f32, stopFg()), (1'f32, stopBg())])

proc applyOpacity*(cs: GradientStops; opacity: openArray[(float32, float32)]): GradientStops =
  ## Sorted RGBA colour stops with transparency stops `(location, opacity 0..1)` multiplied into
  ## their alpha. Where both vary, extra samples keep the product close to per-pixel interpolation.
  var os: seq[(float32, float32)]
  for (t, a) in opacity: os.add (clamp(t, 0'f32, 1'f32), clamp(a, 0'f32, 1'f32))
  sortByLocation(os)
  if os.len == 0 or os.allIt(abs(it[1] - 1'f32) < 1e-6'f32): return cs
  var ts: seq[float32]
  for s in cs: ts.add s[0]
  for s in os: ts.add s[0]
  ts.add 0'f32
  ts.add 1'f32
  ts.sort(totalCmp)
  var dd: seq[float32]
  for t in ts:
    if dd.len > 0 and abs(dd[^1] - t) < 1e-6'f32: continue
    dd.add t
  var all: seq[float32]
  for i in 0 ..< dd.len - 1:
    for k in 0 ..< 4: all.add dd[i] + (dd[i + 1] - dd[i]) * float32(k) / 4'f32
  all.add 1'f32
  for t in all:
    var c = sampleStops(cs, t, lerp4).get([0'f32, 0, 0, 0])
    c[3] *= sampleStops(os, t, proc(a, b: float32; u: float32): float32 = a + (b - a) * u).get(1'f32)
    result.add (t, c)

proc resolve*(g: GradientPreset; fg, bg: array[4, float32]): GradientStops =
  ## RGBA stops for painting: colour and transparency stops merged.
  proc toRgba(c: StopColor): array[4, float32] =
    case c.kind
    of scForeground: [fg[0], fg[1], fg[2], 1'f32]
    of scBackground: [bg[0], bg[1], bg[2], 1'f32]
    of scRgb: [c.rgb[0], c.rgb[1], c.rgb[2], 1'f32]
  var cs: GradientStops
  for (t, c) in g.stops: cs.add (clamp(t, 0'f32, 1'f32), toRgba(c))
  sortByLocation(cs)
  if cs.len == 0: cs = @[(0'f32, [fg[0], fg[1], fg[2], 1'f32]), (1'f32, [bg[0], bg[1], bg[2], 1'f32])]
  applyOpacity(cs, g.opacity)

proc fillStops*(g: GradientPreset; fg, bg: array[4, float32]): GradientFillStops =
  ## Colour stops (live colours resolved now, sorted, opaque) and opacity stops kept apart, as an
  ## editable Gradient Fill layer holds them.
  proc toColor(c: StopColor): Color =
    case c.kind
    of scForeground: rgb(fg[0], fg[1], fg[2])
    of scBackground: rgb(bg[0], bg[1], bg[2])
    of scRgb: rgb(c.rgb[0], c.rgb[1], c.rgb[2])
  var cs: seq[(float32, Color)]
  for (t, c) in g.stops: cs.add (clamp(t, 0'f32, 1'f32), toColor(c))
  sortByLocation(cs)
  if cs.len == 0: cs = @[(0'f32, toColor(stopFg())), (1'f32, toColor(stopBg()))]
  var os: seq[(float32, float32)]
  for (t, a) in g.opacity: os.add (clamp(t, 0'f32, 1'f32), clamp(a, 0'f32, 1'f32))
  sortByLocation(os)
  if os.allIt(abs(it[1] - 1'f32) < 1e-6'f32): os.setLen(0)
  (cs, os)

proc docStops*(g: GradientPreset; fg, bg: array[4, float32]): seq[(float32, Color)] =
  ## Stops for a Gradient Fill layer / overlay (live colours resolved now, like Photoshop).
  for (t, c) in g.resolve(fg, bg): result.add (t, rgba(c[0], c[1], c[2], c[3]))

proc toJson*(g: GradientPreset): JsonNode =
  var stops = newJArray()
  for (t, c) in g.stops:
    case c.kind
    of scForeground: stops.add %*[t, "foreground"]
    of scBackground: stops.add %*[t, "background"]
    of scRgb: stops.add %*[t, gradHexOf(c.rgb)]
  var opacity = newJArray()
  for (t, a) in g.opacity: opacity.add %*[t, round(a * 100'f32)]
  %*{"name": g.name, "stops": stops, "transparency": opacity}

proc isHexColor(h: string): bool =
  var s = h
  while s.startsWith("#"): s = s[1 .. ^1]
  s.len == 6 and s.allCharsInSet(HexDigits)

proc finiteF32(n: JsonNode): Option[float32] =
  ## A JSON number as a finite `f32`.
  let x = n.asF64
  if x.isSome:
    let y = float32(x.get)
    if y == y and abs(y) != Inf: return some(y)

proc transparencyParam*(p: JsonNode; cmdId: string): Option[seq[(float32, float32)]] =
  ## Explicit `"transparency":[[location 0..1, opacity 0..100], …]` as sorted `(location,
  ## opacity 0..1)` stops; none when `p` has none.
  let v = p.pget("transparency")
  if v.isNil: return
  if not v.isArray: raise bad(cmdId, "`transparency` is [[location, opacity %], …]")
  if v.len > 1024: raise bad(cmdId, "too many opacity stops (max 1024)")
  var stops: seq[(float32, float32)]
  for s in v.elems:
    if not s.isArray or s.len != 2: raise bad(cmdId, "an opacity stop is [location 0..1, opacity 0..100]")
    let (t, o) = (finiteF32(s.elems[0]), finiteF32(s.elems[1]))
    if t.isNone or o.isNone: raise bad(cmdId, "opacity stops are numbers")
    stops.add (clamp(t.get, 0'f32, 1'f32), clamp(o.get / 100'f32, 0'f32, 1'f32))
  sortByLocation(stops)
  some(stops)

proc withTransparency*(g: GradientPreset; p: JsonNode; cmdId: string): GradientPreset =
  result = g
  let o = transparencyParam(p, cmdId)
  if o.isSome: result.opacity = o.get

proc gradientFromParams*(name: string; p: JsonNode; cmdId: string): GradientPreset =
  ## Parse `{"stops":[[t,"#hex"|"foreground"|"background"],…],"transparency":[[t,0..100],…]}`.
  let arr = p.pget("stops")
  if not arr.isArray: raise bad(cmdId, "missing `stops`")
  var stops: seq[(float32, StopColor)]
  for i, s in arr.elems:
    var t = none(float32)
    var c = none(string)
    if s.isArray and s.len == 2:
      let f = s.elems[0].asF64
      if f.isSome: t = some(float32(f.get))
      c = s.elems[1].asStr
    elif s.kind == JString:
      t = some(if arr.len > 1: float32(i) / float32(arr.len - 1) else: 0'f32)
      c = some(s.str)
    if t.isNone or c.isNone: raise bad(cmdId, "stops are [location, \"#rrggbb\"|\"foreground\"|\"background\"]")
    let col = (case c.get
               of "foreground": stopFg()
               of "background": stopBg()
               else:
                 if isHexColor(c.get): stopRgb(gradHex(c.get))
                 else: raise bad(cmdId, "bad stop colour `" & c.get & "`"))
    stops.add (clamp(t.get, 0'f32, 1'f32), col)
  if stops.len < 2: raise bad(cmdId, "a gradient needs at least 2 colour stops")
  sortByLocation(stops)
  GradientPreset(name: name, stops: stops).withTransparency(p, cmdId)

proc builtinGradients*(): seq[PresetGroup[GradientPreset]] =
  ## Built-in groups, laid out like Photoshop's default Gradients panel. The colours are our own.
  proc fam(prefix: string; sets: openArray[seq[string]]): seq[GradientPreset] =
    for i, c in sets: result.add newGradientPreset(prefix & " " & align($(i + 1), 2, '0'), c)
  let fgTransparent = GradientPreset(name: "Foreground to Transparent", stops: @[(0'f32, stopFg()), (1'f32, stopFg())],
                                     opacity: @[(0'f32, 1'f32), (1'f32, 0'f32)])
  let rainbow = newGradientPreset("Rainbow", ["#ec1313", "#ec6d13", "#ecc813", "#b6ec13", "#5bec13", "#13ec25", "#13ec80",
                                              "#13ecda", "#13a4ec", "#1349ec", "#3713ec", "#9213ec", "#ec13ec"])
  var fade = rainbow
  fade.name = "Rainbow Fade"
  fade.opacity = @[(0'f32, 0'f32), (0.125'f32, 1'f32), (0.875'f32, 1'f32), (1'f32, 0'f32)]
  @[
    newGroup("Basics", @[foregroundToBackground(), fgTransparent, newGradientPreset("Black, White", ["#000000", "#ffffff"])]),
    newGroup("Blues", fam("Blue", [@["#0b3d91", "#4fa3e0"], @["#1c2a5a", "#3f6fd8", "#a8d4ff"], @["#00b4d8", "#03045e"],
                                   @["#5ec8f2", "#e6f6ff"], @["#253b80", "#20b2aa"], @["#89c2ff", "#1d4ed8", "#0a1a40"]])),
    newGroup("Purples", fam("Purple", [@["#3c096c", "#c77dff"], @["#5a189a", "#e0aaff"], @["#240046", "#7b2cbf", "#f3d9ff"],
                                       @["#9d4edd", "#4361ee"], @["#6a0572", "#ab83a1"], @["#2d0b59", "#ff6fd8"]])),
    newGroup("Pinks", fam("Pink", [@["#ff4d8d", "#ffd1e1"], @["#c9184a", "#ff8fab"], @["#ff70a6", "#ffd670"],
                                   @["#8e2c5a", "#f7a1c4", "#fff0f5"], @["#ff5fa2", "#a855f7"], @["#ffc2d6", "#ff2e63"]])),
    newGroup("Reds", fam("Red", [@["#7f0000", "#ff3b30"], @["#d00000", "#ffba08"], @["#370617", "#9d0208", "#f48c06"],
                                 @["#e5383b", "#f5f3f4"], @["#a4161a", "#0b090a"], @["#ff595e", "#ffca3a"]])),
    newGroup("Oranges", fam("Orange", [@["#ff6d00", "#ffd60a"], @["#e85d04", "#ffba08"], @["#9c3d0b", "#ff9e40", "#fff1d6"],
                                       @["#ff7b00", "#ff0054"], @["#fb8500", "#023047"], @["#ffb703", "#fb5607"]])),
    newGroup("Greens", fam("Green", [@["#004b23", "#70e000"], @["#2d6a4f", "#b7e4c7"], @["#1b4332", "#40916c", "#d8f3dc"],
                                     @["#38b000", "#ccff33"], @["#0b6e4f", "#08a0a8"], @["#606c38", "#dda15e"]])),
    newGroup("Browns", fam("Brown", [@["#3e2412", "#a47148"], @["#6f4518", "#e6ccb2"], @["#2b1708", "#7f5539", "#ddb892"],
                                     @["#8b5e34", "#ffd8a8"], @["#582f0e", "#b6ad90"], @["#4a3728", "#c08552", "#f3e9dc"]])),
    newGroup("Grays", fam("Gray", [@["#212529", "#f8f9fa"], @["#495057", "#dee2e6"], @["#000000", "#6c757d", "#ffffff"],
                                   @["#343a40", "#adb5bd", "#343a40"], @["#8d99ae", "#2b2d42"], @["#e9ecef", "#ced4da", "#868e96"]])),
    newGroup("Rainbows", @[
      rainbow,
      newGradientPreset("Pastel Rainbow", ["#f4afaf", "#f4cbaf", "#f4e8af", "#e2f4af", "#c6f4af", "#aff4b4", "#aff4d1",
                                           "#aff4ee", "#afddf4", "#afc0f4", "#baaff4", "#d7aff4", "#f4aff4"]),
      newGradientPreset("Neon Rainbow", ["#ff0000", "#ff6a00", "#ffd400", "#bfff00", "#55ff00", "#00ff15", "#00ff80",
                                         "#00ffea", "#00aaff", "#0040ff", "#2a00ff", "#9500ff", "#ff00ff"]),
      newGradientPreset("Oil Slick", ["#0b0f1a", "#1b2a4a", "#0f6f6f", "#1fae8a", "#5ac8a8", "#7a5cff", "#b13bd6", "#e04a8f",
                                      "#f2a65a", "#3a2a6b", "#0b0f1a"]),
      fade]),
  ]

# ---------------------------------------------------------------- commands

proc toolStops*(s: Session; p: JsonNode): Option[GradientStops] =
  ## Gradient for the Gradient tool: `"gradient": preset name`, else explicit `"stops"`, else the
  ## current gradient; explicit `"transparency"` stops replace its own. None when the caller gave
  ## `colors` (the legacy evenly spaced form `paint.gradient` handles itself).
  const cmdId = "paint.gradient"
  let colors = p.pget("colors")
  if colors.isArray and colors.len > 0: return
  var g: GradientPreset
  let name = strParam(p, "gradient")
  if name.isSome:
    let at = findPreset(s.presets.gradients, name.get, none(string))
    if at.isNone: raise bad(cmdId, "no gradient preset \"" & name.get & "\"")
    g = s.presets.gradients[at.get[0]].items[at.get[1]].withTransparency(p, cmdId)
  elif not p.pget("stops").isNil: g = gradientFromParams("Custom", p, cmdId)
  else: g = s.presets.gradient.withTransparency(p, cmdId)
  some(g.resolve(s.tools.foreground, s.tools.background))

proc gradientPresetParam(s: Session; p: JsonNode; cmdId: string): GradientPreset =
  ## The preset named by `"preset"` (else the current gradient), or explicit `"stops"`.
  let name = strParam(p, "preset")
  if name.isSome:
    let at = findPreset(s.presets.gradients, name.get, strParam(p, "group"))
    if at.isNone: raise bad(cmdId, "no gradient preset \"" & name.get & "\" (see gradient.presets.list)")
    return s.presets.gradients[at.get[0]].items[at.get[1]]
  if not p.pget("stops").isNil: return gradientFromParams(strParam(p, "name").get("Custom"), p, cmdId)
  s.presets.gradient

proc gradientList(s: var Session): JsonNode =
  var groups = newJArray()
  for g in s.presets.gradients:
    var items = newJArray()
    for it in g.items: items.add it.toJson
    groups.add %*{"name": g.name, "presets": items}
  %*{"groups": groups, "current": s.presets.gradient.toJson}

proc activeGradientFill(s: Session): Option[LayerId] =
  let d = s.active
  if d.isNil or d.activeLayer.isNone: return
  let l = d.doc[].findLayer(d.activeLayer.get)
  if not l.isNil and l[].content.kind == lkFill and l[].content.fill.kind == fkGradient: some(d.activeLayer.get)
  else: none(LayerId)

proc gradientSelect(s: var Session; p: JsonNode): JsonNode =
  let g = gradientPresetParam(s, p, GradientSelectCmd)
  s.presets.gradient = g
  s.presetsChanged()
  result = %*{"current": g.toJson}
  # Clicking a preset with a Gradient Fill layer selected changes that layer (Photoshop).
  let id = activeGradientFill(s)
  if p.getBoolOr("applyToLayer", true) and id.isSome:
    let (stopsNew, opacityNew) = g.fillStops(s.tools.foreground, s.tools.background)
    let lid = id.get
    s.editDo("Change Gradient Fill"):
      let mode = doc.mode
      let l = doc.layerPtr(lid)
      if not l.isNil and l[].content.kind == lkFill and l[].content.fill.kind == fkGradient:
        l[].content.fill.stops = @[]
        for (t, c) in stopsNew: l[].content.fill.stops.add (t, c.inMode(mode))
        l[].content.fill.opacityStops = opacityNew
        l[].content.fill.midpoints.setLen(0)
    result["layer"] = %uint64(lid)

proc stopColorParam(v: JsonNode; cmdId: string): StopColor =
  ## A preset stop colour: `"#rrggbb"`, `"foreground"` or `"background"`.
  let s = v.asStr
  if s.isNone: raise bad(cmdId, "a stop colour is \"#rrggbb\", \"foreground\" or \"background\"")
  case s.get
  of "foreground": stopFg()
  of "background": stopBg()
  else:
    if isHexColor(s.get): stopRgb(gradHex(s.get))
    else: raise bad(cmdId, "bad stop colour `" & s.get & "`")

proc applyPresetStop*(g: GradientPreset; p: JsonNode; fg, bg: array[4, float32]; cmdId: string): GradientPreset =
  ## Applies a `gradient.presets.stop` edit to the Gradient tool's current gradient (pure, so the
  ## editor previews a drag with exactly what the command commits).
  result = g
  let action = p.pget("action").asStr
  if action.isNone: raise bad(cmdId, "missing `action` (add|move|delete|color|opacity)")
  if action.get == "midpoint":
    raise bad(cmdId, "preset gradients keep midpoints at the centre (edit a Gradient Fill layer to move them)")
  let kind = p.pget("kind").asStr.get("color")
  var opacityKind = false
  case kind
  of "color": opacityKind = false
  of "opacity": opacityKind = true
  else: raise bad(cmdId, "unknown kind `" & kind & "` (color|opacity)")
  proc index(): int =
    let v = p.pget("index").asU64
    if v.isNone: raise bad(cmdId, "missing `index` (a stop number from 0)")
    if v.get > uint64(high(int)): raise bad(cmdId, "missing `index` (a stop number from 0)")
    int(v.get)
  proc location(): float32 =
    let v = p.pget("location").asF64
    if v.isSome:
      let f = float32(v.get)
      if f == f and abs(f) != Inf: return clamp(f, 0'f32, 1'f32)
    raise bad(cmdId, "missing `location` (0..1)")
  let ramp = g.resolve(fg, bg)
  if opacityKind:
    # Opacity stops: an empty list means "opaque" (two 100 % stops when first edited).
    var os = (if result.opacity.len == 0: @[(0'f32, 1'f32), (1'f32, 1'f32)] else: result.opacity)
    sortByLocation(os)
    proc opacity(): float32 =
      let v = p.pget("opacity").asF64
      if v.isNone: raise bad(cmdId, "missing `opacity` (0..100)")
      clamp(float32(v.get) / 100'f32, 0'f32, 1'f32)
    case action.get
    of "add":
      let t = location()
      let a = (if not p.pget("opacity").isNil: opacity()
               else: (let c = sampleStops(ramp, t, lerp4); if c.isSome: c.get[3] else: 1'f32))
      os.add (t, a)
    of "move":
      let i = index()
      let t = location()
      if i >= os.len: raise bad(cmdId, "no opacity stop " & $i)
      os[i][0] = t
    of "delete":
      let i = index()
      if i >= os.len: raise bad(cmdId, "no opacity stop " & $i)
      if os.len <= 2: raise bad(cmdId, "a gradient keeps at least 2 opacity stops")
      os.delete(i)
    of "opacity":
      let i = index()
      let a = opacity()
      if i >= os.len: raise bad(cmdId, "no opacity stop " & $i)
      os[i][1] = a
    else: raise bad(cmdId, "unknown action `" & action.get & "` for opacity stops (add|move|delete|opacity)")
    sortByLocation(os)
    if os.len > 1024: raise bad(cmdId, "too many opacity stops (max 1024)")
    # Fully opaque again: store as no opacity stops.
    if os.allIt(abs(it[1] - 1'f32) < 1e-6'f32): os.setLen(0)
    result.opacity = os
    return
  var stops = result.stops
  sortByLocation(stops)
  case action.get
  of "add":
    let t = location()
    # A new stop takes the colour the gradient has there (Photoshop), unless given.
    var c: StopColor
    if not p.pget("color").isNil: c = stopColorParam(p.pget("color"), cmdId)
    else:
      let sc = sampleStops(ramp, t, lerp4)
      c = (if sc.isSome: stopRgb([sc.get[0], sc.get[1], sc.get[2]]) else: stopRgb([0'f32, 0, 0]))
    stops.add (t, c)
    if stops.len > 1024: raise bad(cmdId, "too many colour stops (max 1024)")
  of "move":
    let i = index()
    let t = location()
    if i >= stops.len: raise bad(cmdId, "no colour stop " & $i)
    stops[i][0] = t
  of "delete":
    let i = index()
    if i >= stops.len: raise bad(cmdId, "no colour stop " & $i)
    if stops.len <= 2: raise bad(cmdId, "a gradient keeps at least 2 colour stops")
    stops.delete(i)
  of "color":
    let i = index()
    let cv = p.pget("color")
    if cv.isNil: raise bad(cmdId, "missing `color`")
    let c = stopColorParam(cv, cmdId)
    if i >= stops.len: raise bad(cmdId, "no colour stop " & $i)
    stops[i][1] = c
  else: raise bad(cmdId, "unknown action `" & action.get & "` (add|move|delete|color)")
  sortByLocation(stops)
  result.stops = stops

proc gradientStop(s: var Session; p: JsonNode): JsonNode =
  let g = applyPresetStop(s.presets.gradient, p, s.tools.foreground, s.tools.background, GradientStopCmd)
  s.presets.gradient = g
  s.presetsChanged()
  %*{"current": s.presets.gradient.toJson}

proc gradientApply(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "gradient.presets.apply"
  let g = gradientPresetParam(s, p, cmdId)
  let stops = g.docStops(s.tools.foreground, s.tools.background)
  let angle = float32(p.pget("angle").asF64.get(90.0))
  let scale = clamp(float32(p.pget("scale").asF64.get(100.0)) / 100'f32, 0.1'f32, 1.5'f32)
  let style = gradientStyle(p.pget("style").asStr.get("linear"))
  let reverse = p.getBoolOr("reverse", false)
  let id = s.editWith("New Gradient Fill Layer", proc(doc: var Document; active: var Option[LayerId]): LayerId =
    let fill = gradientFill(stops, angle, scale, style, reverse).inMode(doc.mode)
    result = doc.insertAbove(active, newLayer(doc.nextLayerName("Gradient Fill"), fillContent(fill)))
    active = some(result))
  %*{"layer": uint64(id), "preset": g.name}

proc gradientNew(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "gradient.presets.new"
  var g = (if not p.pget("stops").isNil: gradientFromParams("", p, cmdId) else: s.presets.gradient)
  g.name = uniqueName(s.presets.gradients, strParam(p, "name").get("Custom"))
  let gi = groupIndex(s.presets.gradients, strParam(p, "group"))
  s.presets.gradients[gi].items.add g
  let group = s.presets.gradients[gi].name
  s.presetsChanged()
  %*{"name": g.name, "group": group}

proc gradientEdit(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "gradient.presets.edit"
  let action = reqStr(p, "action", cmdId)
  result = editGroups(s.presets.gradients, action, p, cmdId)
  s.presetsChanged()

proc gradientReset(s: var Session; p: JsonNode): JsonNode =
  var b = builtinGradients()
  if p.getBoolOr("append", false):
    for g in b.mitems: g.name = uniqueGroup(s.presets.gradients, g.name)
    s.presets.gradients.add b
  else: s.presets.gradients = b
  s.presetsChanged()
  %*{"groups": s.presets.gradients.len}

proc gradientPresetSpecs*(): seq[CommandSpec] =
  const stops = "\"stops\":[[t 0..1,\"#rrggbb\"|\"foreground\"|\"background\"],…],\"transparency\":[[t,opacity 0..100],…]?"
  @[
    cmd("gradient.presets.list", "Gradient Presets", [], "", "{} → {groups:[{name,presets:[{name,stops,transparency}]}],current}",
        presetAlwaysE, runProc(gradientList(s)), journal = false),
    cmd(GradientSelectCmd, "Select Gradient", [], "",
        "{\"preset\":name|" & stops & ",\"group\":name?,\"applyToLayer\":bool=true (also recolours a selected Gradient Fill layer)} → {current,layer?}. The Gradient tool paints with it.",
        presetAlwaysE, runProc(gradientSelect(s, p))),
    cmd(GradientStopCmd, "Edit Gradient Stop", [], "",
        "{\"action\":\"add|move|delete|color|opacity\",\"kind\":\"color|opacity\"=\"color\",\"index\":stop number,\"location\":0..1,\"color\":\"#rrggbb\"|\"foreground\"|\"background\" (add: default the colour there),\"opacity\":0..100} → {\"current\"}. Edits the Gradient tool's current gradient (no layer, midpoints stay centred).",
        presetAlwaysE, runProc(gradientStop(s, p))),
    cmd("gradient.presets.apply", "New Gradient Fill Layer from Preset", [], "",
        "{\"preset\":name?=current|" & stops & ",\"angle\":deg=90,\"style\":\"linear|radial|angle|reflected|diamond\"=\"linear\",\"scale\":10..150=100,\"reverse\":bool} → {layer}",
        presetHasDocE, runProc(gradientApply(s, p))),
    cmd("gradient.presets.new", "New Gradient Preset", [], "",
        "{\"name\":str=\"Custom\",\"group\":name?=first," & stops & " (default: the current gradient)}",
        presetAlwaysE, runProc(gradientNew(s, p))),
    cmd("gradient.presets.edit", "Edit Gradient Presets", [], "", GroupEditParams, presetAlwaysE,
        runProc(gradientEdit(s, p))),
    cmd("gradient.presets.reset", "Restore Default Gradients", [], "", "{\"append\":bool=false}", presetAlwaysE,
        runProc(gradientReset(s, p))),
  ]
