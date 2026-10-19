## Reading the document model (port of `affinity/src/model.rs`): spreads, artboards, layers,
## groups, curves, shapes, text and images, with transforms composed into document space.
## Anything else is reported by `AfDocument.warnings` instead of being dropped silently.

import std/[options, sets]
import ./container, ./stream, ./types, ./geometry, ./paint, ./raster, ./text, ./shapes
import ../rmath

const
  MaxTreeDepth = 128 ## deepest layer nesting followed; deeper content is reported, not imported
  MaxNodes = 500_000 ## total layer nodes imported

proc blendFromEnum(id, version: uint16): Option[AfBlend] =
  ## The enum (id, version) pairs observed in a document whose layers are named after their mode.
  let b = case id
    of 0: abNormal
    of 1: (if version == 0: abDarken else: return)
    of 2: (if version == 1: abDarkerColor elif version == 0: abMultiply else: return)
    of 3: (if version == 0: abColorBurn else: return)
    of 4: (if version == 0: abLighten else: return)
    of 6: (if version == 1: abLighterColor elif version == 0: abColorDodge else: return)
    of 5: (if version == 0: abScreen else: return)
    of 7: (if version == 0: abAdd else: return)
    of 8: (if version == 0: abOverlay else: return)
    of 9: (if version == 0: abSoftLight else: return)
    of 10: (if version == 0: abHardLight else: return)
    of 11: (if version == 0: abVividLight else: return)
    of 12: (if version == 0: abPinLight else: return)
    of 15: (if version == 1: abLinearLight elif version == 0: abExclusion else: return)
    of 13: (if version == 0: abHardMix else: return)
    of 14: (if version == 0: abDifference else: return)
    of 16: (if version == 0: abSubtract else: return)
    of 17: (if version == 0: abHue else: return)
    of 18: (if version == 0: abSaturation else: return)
    of 19: (if version == 0: abLuminosity else: return)
    of 20: (if version == 0: abColor else: return)
    of 21: (if version == 0: abAverage else: return)
    of 22: (if version == 0: abNegation else: return)
    of 23: (if version == 0: abReflect else: return)
    of 24: (if version == 0: abGlow else: return)
    of 25: (if version == 0: abErase else: return)
    else: return
  some(b)

proc isContainer(class: Tag): bool = class == tagOf("Scop") or class == tagOf("Grup")

proc pages(r: Reader; spread: ObjId): seq[AfRect] =
  ## Page rectangles: `SpMd.PagR[].rctp` (Publisher 2, Affinity 3) or `SprB` split into `PagC`
  ## equal pages (Publisher 1 facing spreads).
  let md = r.s.child(spread, "SpMd")
  if md.isSome:
    var v: seq[AfRect]
    for p in r.s.children(md.get, "PagR"):
      let f = r.s.floatsN(p, "rctp", 4)
      if f.isSome:
        let rc = rect(f.get)
        if rc.x1 > rc.x0 and rc.y1 > rc.y0: v.add rc
    if v.len > 0: return v
  let b = r.s.floatsN(spread, "SprB", 4)
  let n = r.s.intOf(spread, "PagC")
  if b.isSome and n.isSome and n.get >= 2 and n.get <= 16:
    let bb = rect(b.get)
    let w = (bb.x1 - bb.x0) / float64(n.get)
    for i in 0 ..< int(n.get):
      result.add AfRect(x0: bb.x0 + w * float64(i), y0: bb.y0, x1: bb.x0 + w * float64(i + 1), y1: bb.y1)

proc artboard(r: var Reader; id: ObjId; world: AfAffine): AfKind =
  let sb = r.s.floatsN(id, "ShpB", 4)
  let b = if sb.isSome: rect(sb.get) else: AfRect()
  var corners = [pt(b.x0, b.y0), pt(b.x1, b.y0), pt(b.x1, b.y1), pt(b.x0, b.y1)]
  for c in corners.mitems: c = world.apply(c)
  let (a, b2, c, d) = (world.m[0], world.m[1], world.m[2], world.m[3])
  if abs(b2) > 1e-9 or abs(c) > 1e-9 or a <= 0.0 or d <= 0.0:
    r.warn("a rotated or flipped artboard (imported at its bounding box)")
  var rc = AfRect(x0: Inf, y0: Inf, x1: -Inf, y1: -Inf)
  for p in corners:
    rc.x0 = rmin(rc.x0, p.x)
    rc.y0 = rmin(rc.y0, p.y)
    rc.x1 = rmax(rc.x1, p.x)
    rc.y1 = rmax(rc.y1, p.y)
  let background = fills(r, id, world, "BFFl")
  AfKind(tag: nkArtboard, rect: rc, background: background)

proc kind(r: var Reader; id: ObjId; class: Tag; world, parent: AfAffine): AfKind =
  case $class
  of "Scop": AfKind(tag: nkLayer)
  of "Grup": AfKind(tag: nkGroup)
  of "PCrv", "ShpN", "Comp", "SNEN", "SNRR", "ShRN":
    # Current .af boards carry artboard properties (aprp) in phrp; older documents use ABEn.
    # Converted curve boards keep those properties too. Other property classes also use phrp,
    # so the field alone is not a marker.
    let phrp = r.s.child(id, "phrp")
    if r.s.boolean(id, "ABEn") == some(true) or (phrp.isSome and r.s.isClass(phrp.get, "aprp")):
      if class != tagOf("ShpN"):
        r.warn("a converted artboard outline (imported at its bounding box)")
      return artboard(r, id, world)
    let o = outline(r, id, class, world)
    if o.isNone:
      r.warn("a " & describe(class) & " that could not be read")
      return AfKind(tag: nkUnsupported)
    let (local, evenOdd) = o.get
    let (fl, st) = nodePaint(r, id, world)
    AfKind(tag: nkShape, path: local.transformed(world), fills: fl, strokes: st, evenOdd: evenOdd)
  of "TxtA", "TxtF", "TxtC":
    let t = text.read(r, id, world)
    if t.isSome: AfKind(tag: nkText, text: t.get)
    else:
      r.warn("text whose story could not be read (master-page text, or an unknown layout)")
      AfKind(tag: nkUnsupported)
  of "ImgN", "Rstr":
    let img = nodeImage(r, id, world)
    if img.isSome: AfKind(tag: nkImage, image: img.get) else: AfKind(tag: nkUnsupported)
  of "EmbN":
    let img = embedded(r, id, world)
    if img.isSome:
      r.warn("embedded documents and symbols (imported as pictures of them)")
      AfKind(tag: nkImage, image: img.get)
    else:
      r.warn("embedded documents and symbols without a cached picture")
      AfKind(tag: nkUnsupported)
  of "FRst":
    # A fill layer paints its whole bitmap area; its gradient lives in page space.
    let b = r.s.child(id, "Bitm")
    template size(t: string): Option[float64] =
      (if b.isSome:
        (let v = r.s.intOf(b.get, t); if v.isSome and float64(v.get) > 0.0: some(float64(v.get)) else: none(float64))
      else: none(float64))
    let (w, h) = (size("BmpW"), size("BmpH"))
    if w.isSome and h.isSome:
      let local = AfPath(subpaths: @[rectangle(0.0, 0.0, w.get, h.get)])
      let fl = fills(r, id, parent, "BFFl")
      AfKind(tag: nkShape, path: local.transformed(world), fills: fl, evenOdd: false)
    else:
      r.warn("fill layers without an area")
      AfKind(tag: nkUnsupported)
  else:
    r.warn(describe(class) & " layers")
    AfKind(tag: nkUnsupported)

proc attachedMasks(r: var Reader; id: ObjId; world: AfAffine): (Option[AfPath], Option[AfImage]) =
  ## Vector shapes attached to a node (`AdCh`) mask it: it shows only inside their outlines; an
  ## attached mask layer is a pixel mask. Adjustments attached the same way are reported.
  var mask = none(AfPath)
  var pixels = none(AfImage)
  for a in r.s.children(id, "AdCh"):
    let class = r.s.class(a).get(Tag(0))
    if r.s.boolean(a, "Visi") == some(false): continue
    if class == tagOf("PCrv") or class == tagOf("ShpN") or class == tagOf("Comp"):
      let local = r.xfrm(a).then(world)
      let o = outline(r, a, class, local)
      if o.isSome:
        var m = mask.get(AfPath())
        for s in o.get[0].transformed(local).subpaths: m.subpaths.add s
        mask = some(m)
    elif class == tagOf("MRst"):
      let local = r.xfrm(a).then(world)
      let m = raster.mask(r, a, local)
      if m.isSome:
        if pixels.isNone: pixels = m
        else: r.warn("several pixel masks on one layer (only the first is used)")
    else:
      r.warn(describe(class) & " layers attached to other layers (imported without them)")
  (mask, pixels)

proc node(r: var Reader; id: ObjId; parent: AfAffine; depth: int): Option[AfNode]

proc children(r: var Reader; parent: ObjId; world: AfAffine; depth: int): (seq[AfNode], Option[AfImage]) =
  ## The children of `parent`, and the pixel mask a mask layer among them puts on the parent.
  var o: seq[AfNode]
  var mask = none(AfImage)
  for c in r.s.children(parent, "Chld"):
    if r.s.isClass(c, "MRst"):
      # A mask layer inside a group or layer masks that container.
      if r.s.boolean(c, "Visi") == some(false): continue
      let local = r.xfrm(c)
      let m = raster.mask(r, c, local.then(world))
      if m.isSome:
        if mask.isNone: mask = m
        else: r.warn("several pixel masks on one layer (only the first is used)")
      continue
    let n = node(r, c, world, depth + 1)
    if n.isSome: o.add n.get
  (o, mask)

proc node(r: var Reader; id: ObjId; parent: AfAffine; depth: int): Option[AfNode] =
  if depth > MaxTreeDepth:
    r.warn("layers nested deeper than 128 levels")
    return
  inc r.nodes
  if r.nodes > MaxNodes: raise limit("too many layers")
  # A node that contains itself (a shared object linked back into its own subtree).
  if r.active.containsOrIncl(id):
    r.warn("a layer that contains itself")
    return
  let world = r.xfrm(id).then(parent)
  let class = r.s.class(id).get(Tag(0))
  let name = r.s.str(id, "Desc").get("")
  let visible = r.s.boolean(id, "Visi").get(true)
  let e = r.s.boolean(id, "Edtb")
  let locked = e.isSome and not e.get
  let op = r.s.f64(id, "Opac")
  let opacity = if op.isSome: clamp(op.get, 0.0, 1.0) else: 1.0
  var blend = abNormal
  let be = r.s.enumeration(id, "Blnd")
  if be.isNone:
    if r.s.boolean(id, "PasT") != some(false) and isContainer(class): blend = abPassThrough
  else:
    let b = blendFromEnum(be.get[0], be.get[1])
    if b.isSome: blend = b.get
    else: r.warn("an unknown blend mode (imported as Normal)")
  var k: AfKind
  if not world.isFinite:
    r.warn("a layer with an invalid transform")
    k = AfKind(tag: nkUnsupported)
  else:
    k = kind(r, id, class, world, parent)
  # A compound's children are its operands, already merged into its outline.
  var kids: seq[AfNode]
  var childMask = none(AfImage)
  if class != tagOf("Comp"): (kids, childMask) = children(r, id, world, depth)
  let (mask, attached) = attachedMasks(r, id, world)
  let pixelMask = if attached.isSome: attached else: childMask
  r.active.excl id
  for fx in r.s.children(id, "FiEf"):
    if r.s.boolean(fx, "Enab") != some(false):
      r.warn("layer effects (shadows, glows, outlines, bevels…)")
      break
  some(AfNode(class: class, name: name, visible: visible, locked: locked, opacity: opacity, blend: blend,
              kind: k, mask: mask, pixelMask: pixelMask, children: kids))

proc document*(r: var Reader): AfDocument =
  let root = r.s.root
  r.dpi = 72.0
  let uv = r.s.child(root, "UVCn")
  if uv.isSome:
    let d = r.s.f64(uv.get, "UPPI")
    if d.isSome and d.get > 0.0 and d.get < 100_000.0: r.dpi = d.get
  var savedBy = none(string)
  let ver = r.s.child(root, "NVer")
  if ver.isSome:
    let v = ver.get
    let product = r.s.str(v, "Prod").get("Affinity")
    let (a, b, c) = (r.s.intOf(v, "Majr"), r.s.intOf(v, "Minr"), r.s.intOf(v, "Bild"))
    savedBy = some(if a.isSome and b.isSome and c.isSome: product & " " & $a.get & "." & $b.get & "." & $c.get else: product)
  let docR = r.s.child(root, "DocR")
  if docR.isNone: raise malformed("document has no root node")
  let doc = docR.get
  var spreads: seq[AfSpread]
  for spread in r.s.children(doc, "Chld"):
    if not r.s.isClass(spread, "Sprd"):
      r.warn("an unknown kind of page")
      continue
    let pg = pages(r, spread)
    var bounds = AfRect()
    let sb = r.s.floatsN(spread, "SprB", 4)
    if sb.isSome: bounds = rect(sb.get)
    elif pg.len > 0:
      bounds = pg[0]
      for i in 1 ..< pg.len: bounds = union(bounds, pg[i])
    else:
      let ds = r.s.floatsN(doc, "DfSz", 2)
      if ds.isSome and ds.get[0] > 0.0 and ds.get[1] > 0.0:
        bounds = AfRect(x0: 0.0, y0: 0.0, x1: ds.get[0], y1: ds.get[1])
    let transparent = r.s.boolean(spread, "SprT").get(false)
    let (nodes, mask) = children(r, spread, Identity, 0)
    if mask.isSome: r.warn("mask layers directly on a page (imported without them)")
    spreads.add AfSpread(bounds: bounds, pages: pg, transparent: transparent, nodes: nodes)
  if spreads.len == 0: raise malformed("document has no pages")
  if r.s.children(doc, "MpCh").len > 0:
    r.warn("master pages (their content is not placed on the pages that use them)")
  AfDocument(dpi: r.dpi, spreads: spreads, savedBy: savedBy, warnings: r.takeWarnings())

proc read*(bytes: openArray[byte]; limits = defaultLimits()): AfDocument =
  ## Reads a document: archive, `doc.dat` and the model. Raises `AffinityError`.
  var archive = openArchive(bytes, limits)
  let doc = archive.read("doc.dat")
  var r = newReader(parseStream(doc), archive)
  r.document()
