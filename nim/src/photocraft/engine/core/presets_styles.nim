## Window › Styles (`presets/styles.rs`): layer effects (plus optional blending options) in groups,
## applied to the selected layers. The built-in styles were designed here. Part of the
## `engine/core` unit: included, not imported.

proc styleFromSpec(name: string; fx: JsonNode; fill: Option[float32]): StylePreset =
  result = StylePreset(name: name, fillOpacity: fill)
  if fx.isArray:
    for e in fx.elems:
      if not e.isArray or e.len == 0 or e.elems[0].kind != JString: continue
      let params = (if e.len > 1: e.elems[1] else: newJNull())
      let effect = effectFromParams(e.elems[0].str, params)
      if effect.isSome: result.effects.add effect.get

proc toJson*(st: StylePreset): JsonNode =
  var fx = newJArray()
  for e in st.effects: fx.add %e.label
  %*{"name": st.name, "effects": fx, "blend": (if st.blend.isSome: %st.blend.get.label else: newJNull()),
     "fillOpacity": (if st.fillOpacity.isSome: %round(st.fillOpacity.get * 100'f32) else: newJNull())}

proc builtinStyles*(): seq[PresetGroup[StylePreset]] =
  template s(name: string; fx: untyped; fill: Option[float32]): StylePreset = styleFromSpec(name, %*fx, fill)
  let none32 = none(float32)
  @[
    newGroup("Basics", @[
      styleFromSpec("Default Style (None)", newJArray(), none32),
      s("Drop Shadow", [["dropShadow", {"distance": 6, "size": 8, "opacity": 60}]], none32),
      s("Soft Shadow", [["dropShadow", {"distance": 2, "size": 16, "opacity": 45}]], none32),
      s("Black Stroke", [["stroke", {"size": 3, "color": "#000000", "position": "outside"}]], none32),
      s("White Stroke", [["stroke", {"size": 3, "color": "#ffffff", "position": "outside"}]], none32),
      s("Inner Shadow", [["innerShadow", {"distance": 4, "size": 6, "opacity": 55}]], none32),
      s("Outer Glow", [["outerGlow", {"size": 14, "color": "#ffe9a8", "opacity": 80}]], none32),
      s("Emboss", [["bevelEmboss", {"style": "emboss", "size": 4, "depth": 120}]], none32)]),
    newGroup("Text Effects", @[
      s("Neon Blue", [["colorOverlay", {"color": "#d8f4ff"}], ["innerGlow", {"color": "#3fb6ff", "size": 6, "opacity": 90, "blend": "normal"}],
                      ["outerGlow", {"color": "#1e90ff", "size": 22, "opacity": 90}]], none32),
      s("Neon Pink", [["colorOverlay", {"color": "#ffe1f3"}], ["innerGlow", {"color": "#ff4fb4", "size": 6, "opacity": 90, "blend": "normal"}],
                      ["outerGlow", {"color": "#ff2d95", "size": 22, "opacity": 90}]], none32),
      s("Chrome", [["gradientOverlay", {"from": "#3a3f47", "to": "#f4f6f8", "angle": 90}],
                   ["bevelEmboss", {"style": "inner", "size": 6, "depth": 250, "soften": 1}],
                   ["stroke", {"size": 1, "color": "#2a2d33", "position": "outside"}]], none32),
      s("Gold", [["gradientOverlay", {"from": "#8a5a12", "to": "#ffe08a", "angle": 90}], ["bevelEmboss", {"style": "inner", "size": 5, "depth": 180}],
                 ["dropShadow", {"distance": 3, "size": 5, "opacity": 50}]], none32),
      s("Comic Outline", [["colorOverlay", {"color": "#ffd23f"}], ["stroke", {"size": 4, "color": "#111111", "position": "outside"}],
                          ["dropShadow", {"distance": 6, "size": 0, "opacity": 100, "angle": 135}]], none32),
      s("Letterpress", [["colorOverlay", {"color": "#5b6168"}], ["innerShadow", {"distance": 2, "size": 2, "opacity": 70}],
                        ["dropShadow", {"distance": 1, "size": 0, "color": "#ffffff", "opacity": 60, "blend": "screen", "angle": 90}]], none32),
      s("Hollow", [["stroke", {"size": 2, "color": "#222222", "position": "inside"}]], some(0'f32))]),
    newGroup("Buttons", @[
      s("Glass", [["gradientOverlay", {"from": "#2b6cb0", "to": "#90cdf4", "angle": 90}],
                  ["innerGlow", {"color": "#ffffff", "size": 6, "opacity": 45, "blend": "screen"}],
                  ["bevelEmboss", {"style": "inner", "size": 8, "depth": 100, "soften": 4}],
                  ["dropShadow", {"distance": 3, "size": 8, "opacity": 40}]], none32),
      s("Gel Green", [["gradientOverlay", {"from": "#1f7a3a", "to": "#7be495", "angle": 90}], ["innerShadow", {"distance": 2, "size": 5, "opacity": 35}],
                      ["bevelEmboss", {"style": "inner", "size": 10, "depth": 150, "soften": 6}]], none32),
      s("Pressed", [["colorOverlay", {"color": "#d7dbe0"}], ["innerShadow", {"distance": 3, "size": 6, "opacity": 60}],
                    ["stroke", {"size": 1, "color": "#9aa1a9", "position": "inside"}]], none32),
      s("Pill Red", [["gradientOverlay", {"from": "#9b1c1c", "to": "#f87171", "angle": 90}], ["bevelEmboss", {"style": "pillow", "size": 6, "depth": 120}],
                     ["dropShadow", {"distance": 2, "size": 4, "opacity": 45}]], none32),
      s("Flat Shadow", [["colorOverlay", {"color": "#3b82f6"}], ["dropShadow", {"distance": 5, "size": 0, "opacity": 35, "angle": 90}]], none32),
      s("Satin Plum", [["colorOverlay", {"color": "#7e3a8c"}], ["satin", {"color": "#1b0420", "opacity": 55, "size": 18, "distance": 14}],
                       ["bevelEmboss", {"style": "inner", "size": 4, "depth": 80}]], none32)]),
  ]

proc findStyle*(s: Session; p: JsonNode; cmdId: string): StylePreset =
  let name = reqStr(p, "preset", cmdId)
  let at = findPreset(s.presets.styles, name, strParam(p, "group"))
  if at.isNone: raise bad(cmdId, "no style \"" & name & "\" (see style.presets.list)")
  s.presets.styles[at.get[0]].items[at.get[1]]

proc stylePresetsList(s: var Session): JsonNode =
  var groups = newJArray()
  for g in s.presets.styles:
    var items = newJArray()
    for it in g.items: items.add it.toJson
    groups.add %*{"name": g.name, "presets": items}
  %*{"groups": groups}

proc styleTargets(s: Session; p: JsonNode): seq[LayerId] =
  let a = p.pget("layers")
  if a.isArray:
    for v in a.elems:
      let u = v.asU64
      if u.isSome: result.add LayerId(u.get)
    return
  let id = p.pget("layer").asU64
  if id.isSome: return @[LayerId(id.get)]
  selected(s)

proc stylePresetsApply(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "style.presets.apply"
  let style = findStyle(s, p, cmdId)
  let add = p.getBoolOr("add", false)
  let ids = styleTargets(s, p)
  if ids.len == 0: raise bad(cmdId, "no target layers")
  var effects: seq[Effect]
  var pats: seq[DocPattern]
  for fx in style.effects:
    let (e, pat) = resolveEffect(s, fx)
    effects.add e
    if pat.isSome: pats.add pat.get
  s.editDo("Apply Style: " & style.name):
    for pat in pats: ensureInDoc(doc, pat)
    let mode = doc.mode
    var modeEffects: seq[Effect]
    for e in effects: modeEffects.add e.inMode(mode)
    for id in ids:
      let l = doc.layerPtr(id)
      if l.isNil: raise noLayer(id)
      if l[].name == "Background" and l[].locks.transparency and l[].locks.position and l[].content.kind == lkRaster:
        l[].name = "Layer 0"
        l[].locks.transparency = false
        l[].locks.position = false
      if add: l[].effects.items.add modeEffects
      else:
        l[].effects.items = modeEffects
        l[].effects.psdRaw = nil
      l[].effects.psdRaw = nil
      l[].effects.enabled = true
      if not add or style.blend.isSome:
        if style.blend.isSome: l[].blend = style.blend.get
        if style.fillOpacity.isSome: l[].fillOpacity = style.fillOpacity.get
        elif not add: l[].fillOpacity = 1
  var arr = newJArray()
  for id in ids: arr.add %uint64(id)
  %*{"layers": arr, "style": style.name}

proc stylePresetsNew(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "style.presets.new"
  let id = layerParam(s, p)
  let d = s.active
  if d.isNil: raise noDocument()
  let lp = d.doc[].findLayer(id)
  if lp.isNil: raise noLayer(id)
  let l = lp[]
  let withFx = p.getBoolOr("includeEffects", true)
  let withBlend = p.getBoolOr("includeBlending", true)
  if not withFx and not withBlend: raise bad(cmdId, "include effects and/or blending options")
  var listed = none(seq[Effect])
  let list = p.pget("effects")
  if list.isArray:
    var fx: seq[Effect]
    for e in list.elems:
      let kind = (if e.isArray and e.len > 0: e.elems[0].asStr else: none(string))
      if kind.isNone: raise bad(cmdId, "each effect is [kind, params]")
      let params = (if e.len > 1: e.elems[1] else: newJNull())
      let effect = effectFromParams(kind.get, params)
      if effect.isNone: raise bad(cmdId, "unknown effect " & kind.get)
      fx.add effect.get
    listed = some(fx)
  var style = StylePreset(name: uniqueName(s.presets.styles, strParam(p, "name").get("Style")))
  style.effects = (if listed.isSome: listed.get elif withFx: l.effects.items else: @[])
  let b = p.pget("blend").asStr
  let bm = (if b.isSome: blendFromStr(b.get) else: none(BlendMode))
  style.blend = (if bm.isSome: bm elif withBlend: some(l.blend) else: none(BlendMode))
  let fo = p.pget("fillOpacity").asF64
  style.fillOpacity = (if fo.isSome: some(float32(clamp(fo.get / 100.0, 0.0, 1.0)))
                       elif withBlend: some(l.fillOpacity) else: none(float32))
  let gi = groupIndex(s.presets.styles, strParam(p, "group"))
  result = %*{"name": style.name, "group": s.presets.styles[gi].name, "effects": style.effects.len}
  s.presets.styles[gi].items.add style
  s.presetsChanged()

proc stylePresetsEdit(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "style.presets.edit"
  let action = reqStr(p, "action", cmdId)
  result = editGroups(s.presets.styles, action, p, cmdId)
  s.presetsChanged()

proc stylePresetsReset(s: var Session; p: JsonNode): JsonNode =
  var b = builtinStyles()
  if p.getBoolOr("append", false):
    for g in b.mitems: g.name = uniqueGroup(s.presets.styles, g.name)
    s.presets.styles.add b
  else: s.presets.styles = b
  s.presetsChanged()
  %*{"groups": s.presets.styles.len}

proc stylePresetSpecs*(): seq[CommandSpec] =
  @[
    cmd("style.presets.list", "Style Presets", [], "", "{} → {groups:[{name,presets:[{name,effects,blend,fillOpacity}]}]}",
        presetAlwaysE, runProc(stylePresetsList(s)), journal = false),
    cmd("style.presets.apply", "Apply Style", [], "",
        "{\"preset\":name,\"group\":name?,\"layers\":[ids]?|\"layer\":id? (default: the selected layers),\"add\":bool=false (⇧-click: add to the existing effects)} → {layers,style}",
        presetHasLayerE, runProc(stylePresetsApply(s, p))),
    cmd("style.presets.new", "New Style…", [], "",
        "{\"name\":str=\"Style\",\"group\":name?,\"layer\":id?,\"includeEffects\":bool=true,\"includeBlending\":bool=true,\"effects\":[[kind, params]]? (explicit list, e.g. the Layer Style dialog's pending state; overrides includeEffects),\"blend\":str?,\"fillOpacity\":0..100? (override the layer's blending options)}",
        presetHasLayerE, runProc(stylePresetsNew(s, p))),
    cmd("style.presets.edit", "Edit Style Presets", [], "", GroupEditParams, presetAlwaysE, runProc(stylePresetsEdit(s, p))),
    cmd("style.presets.reset", "Restore Default Styles", [], "", "{\"append\":bool=false}", presetAlwaysE,
        runProc(stylePresetsReset(s, p))),
  ]
