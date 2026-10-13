## Layer comps (resource 1065 + per-layer `shmd`/`cmls` settings) and artboards (`artb`/`artd`/`abdd`).

import std/[options, math]
import ../geom, ../color, ../doc, ../psd, ./blocks

const
  LayerComps* = 1065'u16
  ArtboardKeys* = ["artb", "artd", "abdd"]

proc obj(class: string): Descriptor =
  result = newDescriptor(class)
  result.name = UnicodeString(units: @[0'u16])

proc padEven(v: var seq[byte]) =
  if v.len mod 2 == 1: v.add 0'u8

proc intOf(d: Descriptor; key: string): Option[int64] =
  let p = d.getPtr(key)
  if p == nil: return none(int64)
  case p.kind
  of vkInteger: some(int64(p.i))
  of vkLargeInteger: some(p.li)
  of vkDouble: some(int64(round(p.d)))
  of vkUnitFloat: some(int64(round(p.uv)))
  else: none(int64)

proc parseArtboard*(data: openArray[byte]): Option[Artboard] =
  let d = parsePrefixVersioned(data)
  if d.isNone: return none(Artboard)
  let r = getD(d.get, "artboardRect")
  if r.isNone: return none(Artboard)
  template g(k: string): Option[float64] = num(r.get.get(k))
  let (l, t, rr, b) = (g("Left"), g("Top "), g("Rght"), g("Btom"))
  if l.isNone or t.isNone or rr.isNone or b.isNone: return none(Artboard)
  let rc = rect(int(round(l.get)), int(round(t.get)), int(round(rr.get)), int(round(b.get)))
  var bg: ArtboardBackground
  case intOf(d.get, "artboardBackgroundType").get(1)
  of 2: bg = ArtboardBackground(kind: abBlack)
  of 3: bg = ArtboardBackground(kind: abTransparent)
  of 4:
    let c = getD(d.get, "Clr ")
    bg = ArtboardBackground(kind: abCustom, color: (if c.isSome: colorFromDesc(c.get).get(White) else: White))
  else: bg = ArtboardBackground(kind: abWhite)
  some(Artboard(rect: rc, background: bg, preset: textOf(d.get, "artboardPresetName")))

proc writeArtboard*(a: Artboard; guides: openArray[int32]): seq[byte] =
  template f(v: int): Value = vDouble(float64(v))
  let r = newDescriptor("classFloatRect").with("Top ", f(a.rect.y0)).with("Left", f(a.rect.x0)).with("Btom", f(a.rect.y1)).with("Rght", f(a.rect.x1))
  let color = (case a.background.kind
    of abCustom: a.background.color
    of abBlack: Black
    else: White)
  var gl: seq[Value]
  for g in guides: gl.add vInt(g)
  let d = newDescriptor("artboard").with("artboardRect", vDesc(r)).with("guideIndeces", vList(gl))
    .with("artboardPresetName", Value(kind: vkText, text: ustrNul(a.preset))).with("Clr ", vDesc(colorToDesc(color)))
    .with("artboardBackgroundType", vInt(a.background.psdType))
  result = versioned(d).toBytes
  padEven(result)

proc guidesIn*(guides: Guides; r: Rect): seq[int32] =
  var i = 0'i32
  for x in guides.vertical:
    if x > float64(r.x0) and x < float64(r.x1): result.add i
    inc i
  for y in guides.horizontal:
    if y > float64(r.y0) and y < float64(r.y1): result.add i
    inc i

proc artboardBlock*(guides: Guides; l: Layer; raw: var seq[(string, seq[byte])]) =
  var pos = -1
  for i, (k, _) in raw:
    if k in ArtboardKeys:
      pos = i
      break
  let a = l.artboard
  if a.isNone:
    if pos >= 0:
      var kept: seq[(string, seq[byte])]
      for e in raw:
        if e[0] notin ArtboardKeys: kept.add e
      raw = kept
    return
  if pos >= 0 and parseArtboard(raw[pos][1]) == a: return
  let data = writeArtboard(a.get, guidesIn(guides, a.get.rect))
  if pos >= 0: raw[pos] = ("artb", data) else: raw.add ("artb", data)

type Setting = object
  comps: seq[uint32]
  visible: Option[bool]
  offset: Option[(int64, int64)]

proc parseCmls(data: openArray[byte]): Option[seq[Setting]] =
  let d = parsePrefixVersioned(data)
  if d.isNone: return none(seq[Setting])
  let items = d.get.getList("layerSettings")
  if items.isNone: return none(seq[Setting])
  var outp: seq[Setting]
  for it in items.get:
    if it.kind != vkDescriptor: continue
    var s = Setting()
    let cl = it.desc.getList("compList")
    if cl.isSome:
      for v in cl.get:
        if v.kind == vkInteger: s.comps.add cast[uint32](v.i)
    s.visible = it.desc.getBool("enab")
    let o = getD(it.desc, "Ofst")
    if o.isSome:
      let (h, v) = (intOf(o.get, "Hrzn"), intOf(o.get, "Vrtc"))
      if h.isSome and v.isSome: s.offset = some((h.get, v.get))
    outp.add s
  some(outp)

proc layerCmls(l: Layer): Option[seq[byte]] =
  for b in l.psdBlocks:
    if b.key == "shmd":
      try:
        for it in parseShmd(b.data.data):
          if it.key == "cmls": return some(it.data)
      except CatchableError: return none(seq[byte])
  none(seq[byte])

proc offsetPosition(p: (int, int); d: (int64, int64)): Option[(int, int)] =
  let (x, y) = (int64(p[0]) + d[0], int64(p[1]) + d[1])
  if x < int64(low(int32)) or x > int64(high(int32)) or y < int64(low(int32)) or y > int64(high(int32)): none((int, int))
  else: some((int(x), int(y)))

proc decodeComps*(resource: Option[seq[byte]]; d: Document): (seq[LayerComp], Option[uint32], Option[LayerComp], seq[string]) =
  var warnings: seq[string]
  if resource.isNone: return (@[], none(uint32), none(LayerComp), warnings)
  let dd = parsePrefixVersioned(resource.get)
  if dd.isNone: return (@[], none(uint32), none(LayerComp), warnings)
  var comps: seq[LayerComp]
  let list = dd.get.getList("list")
  if list.isSome:
    for it in list.get:
      if it.kind != vkDescriptor: continue
      let cid = intOf(it.desc, "compID")
      if cid.isNone: continue
      var comp = LayerComp(id: uint32(cid.get and 0xffff_ffff), name: textOf(it.desc, "Nm  "), comment: textOf(it.desc, "comment"))
      comp.setCapturedInfo(int32(intOf(it.desc, "capturedInfo").get(0)))
      comps.add comp
  var lastApplied = none(uint32)
  let la = intOf(dd.get, "lastAppliedComp")
  if la.isSome:
    for c in comps:
      if int64(c.id) == la.get: lastApplied = some(c.id)
  var last = LayerComp(id: 0, name: "Last Document State", applyVisibility: true, applyPosition: true)
  for (_, _, l) in d.walk:
    let cm = layerCmls(l)
    if cm.isNone: continue
    let settings = parseCmls(cm.get)
    if settings.isNone: continue
    let here = layerPosition(l)
    for s in settings.get:
      if s.visible.isNone and s.offset.isNone: continue
      var st = CompLayerState(layer: l.id, visible: s.visible)
      if here.isSome and s.offset.isSome:
        st.position = offsetPosition(here.get, s.offset.get)
        if st.position.isNone:
          warnings.add "layer \"" & l.name & "\": layer comp position offset (" & $s.offset.get[0] & ", " & $s.offset.get[1] &
                       ") is out of range; that position was not imported"
      for cid in s.comps:
        if cid == 0: last.states.add st
        else:
          for c in comps.mitems:
            if c.id == cid:
              c.states.add st
              break
  (comps, lastApplied, (if last.states.len > 0: some(last) else: none(LayerComp)), warnings)

proc compsFromPsd*(resource: Option[seq[byte]]; d: Document): (seq[LayerComp], Option[uint32], Option[LayerComp]) =
  let (c, a, l, _) = decodeComps(resource, d)
  (c, a, l)

proc writeCompsResource*(d: Document): Option[seq[byte]] =
  if d.layerComps.len == 0: return none(seq[byte])
  var list: seq[Value]
  for c in d.layerComps:
    var x = newDescriptor("Comp").with("Nm  ", Value(kind: vkText, text: ustrNul(c.name)))
    if c.comment.len > 0: x = x.with("comment", Value(kind: vkText, text: ustrNul(c.comment)))
    list.add vDesc(x.with("compID", vInt(cast[int32](c.id))).with("capturedInfo", vInt(c.capturedInfo)))
  var x = newDescriptor("CompList").with("list", vList(list))
  if d.lastAppliedComp.isSome: x = x.with("lastAppliedComp", vInt(cast[int32](d.lastAppliedComp.get)))
  some(versioned(x).toBytes)

proc settingValue(ids: openArray[uint32]; st: Option[CompLayerState]; here: Option[(int, int)]): Value =
  var s = obj("null")
  if st.isSome:
    if st.get.visible.isSome: s = s.with("enab", vBool(st.get.visible.get))
    if st.get.position.isSome and here.isSome:
      let (dx, dy) = (st.get.position.get[0] - here.get[0], st.get.position.get[1] - here.get[1])
      if dx >= int(low(int32)) and dx <= int(high(int32)) and dy >= int(low(int32)) and dy <= int(high(int32)):
        s = s.with("Ofst", vDesc(obj("null").with("Hrzn", vInt(int32(dx))).with("Vrtc", vInt(int32(dy)))))
  var l: seq[Value]
  for i in ids: l.add vInt(cast[int32](i))
  vDesc(s.with("compList", vList(l)))

proc writeCmls*(comps: openArray[LayerComp]; last: Option[LayerComp]; l: Layer; psdId: uint32): Option[seq[byte]] =
  if comps.len == 0: return none(seq[byte])
  let here = layerPosition(l)
  var settings: seq[Value]
  for c in comps: settings.add settingValue([c.id], c.state(l.id), here)
  settings.add settingValue([0'u32], (if last.isSome: last.get.state(l.id) else: none(CompLayerState)), here)
  let origin = obj("null").with("Hrzn", vDouble(0)).with("Vrtc", vDouble(0))
  let d = newDescriptor("null").with("origFXRefPoint", vDesc(origin)).with("LyrI", vInt(cast[int32](psdId)))
    .with("layerSettings", vList(settings))
  some(versioned(d).toBytes)

proc rawResource*(d: Document; id: uint16): Option[seq[byte]] =
  for r in d.metadata.psdResources:
    if r.id == id: return some(r.data.data)
  none(seq[byte])

proc compsUnchanged*(d: Document): bool =
  let (comps, lastApplied, last) = compsFromPsd(rawResource(d, LayerComps), d)
  comps == d.layerComps and lastApplied == d.lastAppliedComp and last == d.lastDocumentState

proc setCmls*(raw: var seq[(string, seq[byte])]; cmls: Option[seq[byte]]) =
  var pos = -1
  for i, (k, _) in raw:
    if k == "shmd":
      pos = i
      break
  var items: seq[MetadataItem]
  if pos >= 0:
    try: items = parseShmd(raw[pos][1])
    except CatchableError: items = @[]
  var ci = -1
  for i, it in items:
    if it.key == "cmls":
      ci = i
      break
  if ci >= 0 and cmls.isSome:
    var data = cmls.get
    padEven(data)
    items[ci].data = data
  elif ci >= 0: items.delete(ci)
  elif cmls.isSome: items.insert(newMetadataItem("cmls", cmls.get), 0)
  else: return
  if pos >= 0 and items.len == 0: raw.delete(pos)
  elif pos >= 0: raw[pos][1] = writeShmd(items)
  elif items.len > 0: raw.add ("shmd", writeShmd(items))
