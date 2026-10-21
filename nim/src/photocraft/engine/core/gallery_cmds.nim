## Filter › Filter Gallery… (`filter.filterGallery`) and one command per gallery filter
## (`filter.gallery.<key>`, not in menus) (port of `gallery_cmds.rs`).
##
## `filter.filterGallery {effects:[{filter, params, visible}]}` applies a stack of effect layers
## in order (the gallery dialog's effect list, bottom to top). Like every filter it goes through
## `runFilter`. The foreground/background colours the Sketch filters read are recorded as
## explicit params, so smart filters re-render identically. Part of the `engine/core` unit:
## included, not imported.

proc effectFromJson*(filter: GalleryFilter; p, fallback: JsonNode): GalleryEffect =
  ## One effect from its params object (`fallback` supplies colours not given per effect).
  result = newGalleryEffect(filter)
  for prm in filter.params:
    let v = p.pget(prm.key)
    if v.isNil: continue
    if prm.kind == gpChoice and v.kind == JString: discard result.setChoice(prm.key, v.str)
    elif v.kind == JBool: result.set(prm.key, if v.bval: 1'f32 else: 0'f32)
    else:
      let n = v.asF64
      if n.isSome: result.set(prm.key, float32(n.get))
  proc pick(p, fallback: JsonNode; k: string): Option[array[4, float32]] =
    result = fxColour(p.pget(k))
    if result.isNone: result = fxColour(fallback.pget(k))
  let fg = pick(p, fallback, "foreground")
  if fg.isSome: result.foreground = fg.get
  let bg = pick(p, fallback, "background")
  if bg.isSome: result.background = bg.get
  let gc = pick(p, fallback, "glowColor")
  if gc.isSome: result.color = gc.get

proc effectsFromJson*(p: JsonNode): seq[GalleryEffect] =
  ## The effect stack of a `filter.filterGallery` params object; hidden entries are skipped.
  ## Raises `ValueError` with the problem for a malformed stack.
  let list = p.pget("effects")
  if not list.isArray: return
  if list.len > 32: raise newException(ValueError, "too many effects in filterGallery stack (max 32)")
  for k, item in list.elems:
    let vis = item.pget("visible")
    if not vis.isNil and vis.kind == JBool and not vis.bval: continue
    let key = item.pget("filter")
    if key.isNil or key.kind != JString: raise newException(ValueError, "effect " & $k & ": missing `filter`")
    let f = galleryFromKey(key.str)
    if f.isNone: raise newException(ValueError, "effect " & $k & ": unknown gallery filter `" & key.str & "`")
    let pv = item.pget("params")
    let params = if pv.isObject: pv else: item
    result.add effectFromJson(f.get, params, p)

proc galleryParamsFor*(id: string; p: JsonNode): Option[FilterParams] =
  ## Algorithm parameters for the ids this module owns.
  var fp = newFilterParams(fltFilterGallery)
  if id == "filter.filterGallery":
    try: fp.effects = effectsFromJson(p)
    except ValueError: fp.effects = @[]
    return some(fp)
  const prefix = "filter.gallery."
  if not id.startsWith(prefix): return
  let f = galleryFromKey(id[prefix.len .. ^1])
  if f.isNone: return
  fp.effects = @[effectFromJson(f.get, p, newJNull())]
  some(fp)

proc checkChoices(f: GalleryFilter; p: JsonNode): string =
  for prm in f.params:
    let v = p.pget(prm.key)
    if v.isNil or prm.kind != gpChoice: continue
    var ok = false
    case v.kind
    of JString:
      for n in prm.choices:
        if n.toLowerAscii == v.str.toLowerAscii: ok = true
    of JInt, JFloat: ok = true  # a choice index, clamped as the dialog stores it
    else: discard
    if not ok: return "`" & prm.key & "` must be one of " & prm.choices.join("|")

proc galleryValidateParams*(id: string; p: JsonNode) =
  ## An unknown choice name or a wrong-typed choice is a bad-params error, for one gallery filter
  ## or each effect of a `filter.filterGallery` stack.
  if id == "filter.filterGallery":
    let list = p.pget("effects")
    if list.isArray:
      for k, item in list.elems:
        let key = item.pget("filter")
        if key.isNil or key.kind != JString: continue
        let f = galleryFromKey(key.str)
        if f.isNone: continue
        let pv = item.pget("params")
        let params = if pv.isObject: pv else: item
        let m = checkChoices(f.get, params)
        if m.len > 0: raise bad(id, "effect " & $k & ": " & m)
    return
  const prefix = "filter.gallery."
  if not id.startsWith(prefix): return
  let f = galleryFromKey(id[prefix.len .. ^1])
  if f.isNone: return
  let m = checkChoices(f.get, p)
  if m.len > 0: raise bad(id, m)

proc galleryUsesColours*(id: string): bool =
  ## Whether the command reads the foreground/background colours.
  if id == "filter.filterGallery": return true
  const prefix = "filter.gallery."
  if not id.startsWith(prefix): return false
  let f = galleryFromKey(id[prefix.len .. ^1])
  f.isSome and f.get.usesColours

proc galleryCatalogue*(): JsonNode =
  ## The gallery catalogue for dialogs and agents: categories, filters and parameter notation.
  var filters = newJArray()
  for f in GalleryFilter:
    filters.add %*{"key": f.key, "name": f.name, "category": f.category, "command": f.commandId, "params": f.paramsDoc}
  var cats = newJArray()
  for c in GalleryCategories: cats.add %c
  %*{"categories": cats, "filters": filters}

proc runGallery(s: var Session; p: JsonNode): JsonNode =
  let lv = p.pget("list")
  if not lv.isNil and lv.kind == JBool and lv.bval: return galleryCatalogue()
  var effects: seq[GalleryEffect]
  try: effects = effectsFromJson(p)
  except ValueError as e: raise other(e.msg)
  if effects.len == 0:
    raise other("filter.filterGallery needs `effects`: [{\"filter\": \"<key>\", \"params\": {...}}] (pass {\"list\": true} for the catalogue)")
  # Normalize the recorded params (journal / smart filter) to the canonical form.
  var q = if p.isObject: p.copy else: newJObject()
  q.dropKey("list")
  runFilter(s, "filter.filterGallery", q)

proc gallerySpecs*(): seq[CommandSpec] =
  result = @[cmd("filter.filterGallery", "Filter Gallery…", ["Filter"], "",
                 """{"effects":json,"foreground":json,"background":json,"list":bool}""", hasFilterableLayerE, runGallery)]
  for f in GalleryFilter:
    result.add filterSpec(f.commandId, f.name, [], f.paramsDoc)
