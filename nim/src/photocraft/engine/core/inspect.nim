## JSON views of engine state for automation, tests and debugging (port of `inspect.rs`). Part
## of the `engine/core` unit: included, not imported.

proc optJ[T](o: Option[T]): JsonNode =
  if o.isSome: %o.get else: newJNull()

proc inspectSession*(s: Session): JsonNode =
  var docs = newJArray()
  for i, d in s.documents:
    docs.add %*{"index": i, "name": d.doc.name, "path": optJ(d.path), "width": d.doc.size.width,
                "height": d.doc.size.height, "dirty": d.isDirty, "revision": d.revision}
  %*{"active": optJ(s.activeIndex), "documents": docs, "foreground": s.tools.foreground,
     "background": s.tools.background}

proc layerSel(l: Layer; selected: openArray[LayerId]): JsonNode =
  var bounds = newJNull()
  if l.hasSurface:
    let r = l.surface.contentBounds
    bounds = %*[r.x0, r.y0, r.width, r.height]
  result = %*{
    "id": l.id, "name": l.name, "kind": l.content.kindName, "visible": l.visible, "labelColor": l.label.id,
    "opacity": l.opacity, "fill": l.fillOpacity, "blend": l.blend.label, "clipped": l.clipped,
    "hasMask": l.mask.isSome, "hasVectorMask": l.vectorMask.isSome,
    "vectorMaskEnabled": (if l.vectorMask.isSome: %l.vectorMask.get.enabled else: newJNull()),
    "bounds": bounds, "selected": l.id in selected,
    "linkGroup": (if l.linkGroup.isSome: %l.linkGroup.get else: newJNull())}
  case l.content.kind
  of lkGroup:
    var children = newJArray()
    for i in countdown(l.content.group.children.len - 1, 0):
      children.add layerSel(l.content.group.children[i], selected)
    result["children"] = children
    result["expanded"] = %l.content.group.expanded
    if l.content.group.artboard.isSome:
      let a = l.content.group.artboard.get
      result["artboard"] = %*{"rect": [a.rect.x0, a.rect.y0, a.rect.width, a.rect.height],
                              "background": $a.background.kind, "preset": a.preset}
  of lkAdjustment: result["adjustment"] = toJ(l.content.adjustment)
  of lkSmart:
    let sm = l.content.smart
    var filters = newJArray()
    for f in sm.smartFilters:
      filters.add %*{"command": f.command, "params": (if f.params.isNil: newJNull() else: f.params),
                     "visible": f.visible, "opacity": f.opacity, "blend": f.blend.label}
    result["smartFilters"] = filters
    result["smartFiltersEnabled"] = %sm.filtersEnabled
    result["smartSource"] = case sm.source.kind
      of ssEmbedded: %*{"kind": "embedded", "fileName": sm.source.fileName}
      of ssLinked: %*{"kind": "linked", "path": sm.source.path}
  of lkText:
    result["text"] = %*{"text": l.content.text.text, "font": l.content.text.fontFamily, "sizePt": l.content.text.sizePt}
  else: discard
  # Advanced Blending › Channels (only when some channel is left out).
  if l.excludedChannels != 0:
    var ch = newJArray()
    for i in 0 ..< 4: ch.add %((l.excludedChannels and (1'u32 shl i)) == 0)
    result["channels"] = ch
  if not l.advanced.isDefault: result["advancedBlending"] = toJ(l.advanced)
  if not l.blendIf.isDefault:
    var ranges = newJArray()
    for i, r in l.blendIf.ranges:
      if r[0].isFull and r[1].isFull: continue
      ranges.add %*{"channel": i, "thisLayer": r[0].toBytes, "underlying": r[1].toBytes}
    result["blendIf"] = ranges
  if l.effects.items.len > 0:
    var items = newJArray()
    for e in l.effects.items: items.add %*{"kind": e.label, "enabled": e.enabled}
    result["effects"] = %*{"enabled": l.effects.enabled, "items": items}
  if l.video.isSome:
    let vid = l.video.get
    let source = if vid.source.kind == vsBlank: %"blank" else: %*{"file": vid.source.path}
    result["video"] = %*{"frames": vid.frames.len, "fps": vid.fps, "showAltered": vid.showAltered, "source": source}

proc inspectLayer*(l: Layer): JsonNode = layerSel(l, [])

proc inspectDocument*(d: DocState): JsonNode =
  let doc = d.doc
  let selected = d.selectedLayers
  var layers = newJArray()
  for i in countdown(doc.layers.len - 1, 0): layers.add layerSel(doc.layers[i], selected)
  var history = newJArray()
  for e in d.history.entries: history.add %e
  var snaps = newJArray()
  for snap in d.snapshots: snaps.add %*{"id": snap.id, "name": snap.name}
  var comps = newJArray()
  for c in doc.layerComps: comps.add %*{"id": c.id, "name": c.name}
  var notes = newJArray()
  for n in doc.notes: notes.add %*{"author": n.author, "text": n.text, "position": n.position}
  let ruler = if doc.measurement.ruler.isSome:
      let r = doc.measurement.ruler.get
      %*{"start": r.start, "end": r.`end`, "protractor": (if r.protractor.isSome: %r.protractor.get else: newJNull())}
    else: newJNull()
  let selBounds = if doc.selection.isSome:
      let r = doc.selection.get.contentBounds
      %*[r.x0, r.y0, r.width, r.height]
    else: newJNull()
  var variables = newJNull()
  if not doc.variables.isEmpty:
    var defs = newJArray()
    for vd in doc.variables.defs: defs.add %*{"name": vd.name, "layer": vd.layer}
    var sets = newJArray()
    for s in doc.variables.dataSets: sets.add %s.name
    variables = %*{"defs": defs, "dataSets": sets, "active": optJ(doc.variables.active)}
  %*{
    "name": doc.name, "path": optJ(d.path), "width": doc.size.width, "height": doc.size.height,
    "mode": $doc.mode, "depth": doc.depth.bits, "resolution": doc.resolutionDpi,
    "activeLayer": optJ(d.activeLayer), "selectedLayers": idsJson(selected), "isolatedLayers": idsJson(d.isolatedLayers),
    "hasSelection": doc.selection.isSome, "selectionBounds": selBounds, "layers": layers, "history": history,
    "snapshots": snaps, "canUndo": d.history.canUndo, "canRedo": d.history.canRedo, "revision": d.revision,
    "channels": channelsJson(d), "quickMask": doc.quickMask.isSome,
    "symmetry": (if d.symmetry.isSome: symmetryInspect(d.symmetry.get) else: newJNull()),
    "layerMaskView": maskViewJson(d), "layerComps": comps, "lastAppliedComp": optJ(doc.lastAppliedComp),
    "measurement": {"scale": doc.measurement.scale.describe, "ruler": ruler, "count": doc.measurement.countTotal,
                    "countGroups": doc.measurement.countGroups.len},
    "notes": notes,
    "timeline": (if doc.timeline.isSome: %*{"fps": doc.timeline.get.fps, "duration": doc.timeline.get.duration,
                                            "current": doc.timeline.get.current} else: newJNull()),
    "variables": variables}
