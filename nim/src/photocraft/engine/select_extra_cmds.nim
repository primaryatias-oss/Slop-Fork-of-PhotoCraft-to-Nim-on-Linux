## Select › Sky, Select › Isolate Layers and Select › Transform Selection (port of
## `select_extra_cmds.rs`).
##
## * `select.sky` runs the classical sky segmentation of `algo/sky` on the composite (or the
##   active layer) and combines it with the selection.
## * `select.isolateLayers` toggles the Layers panel's isolation filter (`DocState.isolatedLayers`,
##   view state).
## * `select.transformSelection` transforms the selection outline/mask only (not pixels): a quad, an
##   affine matrix, scale / rotate / skew / move about a reference point, or a warp.

import std/[json, options, math]
import ./core
import ../compose
import ../algo/sky as asky
import ../algo/segbase
import ../algo/selection as asel
import ../algo/transform as atransform
import ../algo/warp as awarp
import ../geom/warp as gwarp

proc hasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")
proc hasSelectionE(s: Session): string = (if s.active.isNil or s.active.doc.selection.isNone: "no selection" else: "")
proc hasLayerE(s: Session): string = (if s.active.isNil or s.active.activeLayer.isNone: "no active layer" else: "")

proc fnum(p: JsonNode; k: string; d: float64): float64 = p.pget(k).asF64.get(d)

proc hasOwnSurface(l: Layer): bool =
  case l.content.kind
  of lkRaster: true
  of lkText: l.content.text.cache.isSome
  of lkShape: l.content.shape.cache.isSome
  of lkSmart: l.content.smart.cache.isSome
  else: false

# ---------------------------------------------------------------- Sky

proc selectSky(s: var Session; p: JsonNode): JsonNode =
  let d = s.active
  if d.isNil: raise noDocument()
  let doc = d.doc
  let canvas = doc[].bounds
  let params = SkyParams(threshold: float32(fnum(p, "threshold", 50)), softness: float32(fnum(p, "softness", 50)))
  let all = p.pget("sampleAllLayers").asBool.get(true)
  let l = (if d.activeLayer.isSome: doc[].findLayer(d.activeLayer.get) else: nil)
  let sampler =
    if not all and not l.isNil and hasOwnSurface(l[]): surfaceSampler(l[].surface)
    else: Sampler(rgba: proc(r: Rect): seq[Rgba] = compose.render(doc[], r).px)
  let region = selectSky(sampler, canvas, params)
  let m0 = p.pget("mode").asStr.get("replace")
  let m = parseSelectionMode(if m0 == "new": "replace" else: m0)
  if region.isNone:
    # Photoshop reports "No sky was detected" and leaves the selection alone.
    return %*{"selected": doc.selection.isSome, "changed": false, "coverage": 0.0, "message": "No sky was detected"}
  var total = 0.0
  for v in region.get.mask: total += float64(v) / 255.0
  let coverage = total / max(float64(canvas.width) * float64(canvas.height), 1.0)
  let b = region.get.bbox
  let rg = region
  let selected = s.editWith("Select Sky", proc(doc: var Document; active: var Option[LayerId]): bool =
    doc.selection = combineRegion(doc.selection, rg, m)
    doc.selection.isSome)
  %*{"selected": selected, "changed": true, "coverage": coverage, "bounds": [b.x0, b.y0, b.width, b.height]}

# ---------------------------------------------------------------- Isolate Layers

proc isolateLayers(s: var Session; p: JsonNode): JsonNode =
  let st = s.active
  if st.isNil: raise noDocument()
  let on = p.pget("on").asBool.get(st.isolatedLayers.len == 0)
  st.isolatedLayers = (if on: st[].selectedOrdered else: newSeq[LayerId]())
  inc st.revision
  var ids: seq[uint64]
  for l in st.isolatedLayers: ids.add uint64(l)
  %*{"isolated": st.isolatedLayers.len > 0, "layers": ids}

proc isolationShows*(doc: Document; isolated: seq[LayerId]; id: LayerId): bool =
  ## Whether a Layers panel row for `id` is listed under isolation (ancestors of an isolated layer
  ## stay visible so it can be shown in place).
  if isolated.len == 0 or id in isolated: return true
  proc contains(l: Layer; depth: int): bool =
    if l.content.kind != lkGroup or depth > 512: return false
    for c in l.content.group.children:
      if c.id in isolated or contains(c, depth + 1): return true
  let l = doc.findLayer(id)
  not l.isNil and contains(l[], 0)

# ---------------------------------------------------------------- Transform Selection

proc referencePoint(p: JsonNode; r: array[4, float64]): array[2, float64] =
  let (x0, y0, x1, y1) = (r[0], r[1], r[2], r[3])
  let (cx, cy) = ((x0 + x1) / 2.0, (y0 + y1) / 2.0)
  let v = p.pget("reference")
  if v.isArray and v.len >= 2: return [v.elems[0].asF64.get(cx), v.elems[1].asF64.get(cy)]
  if not v.isNil and v.kind == JString:
    return case v.str
      of "topLeft": [x0, y0]
      of "top": [cx, y0]
      of "topRight": [x1, y0]
      of "left": [x0, cy]
      of "right": [x1, cy]
      of "bottomLeft": [x0, y1]
      of "bottom": [cx, y1]
      of "bottomRight": [x1, y1]
      else: [cx, cy]
  [cx, cy]

proc quadParam(p: JsonNode): Option[array[4, array[2, float64]]] =
  var a = p.pget("quad")
  if a.isNil: a = p.pget("corners")
  if not a.isArray or a.len != 4: return
  var q: array[4, array[2, float64]]
  for i, v in a.elems:
    if not v.isArray or v.len < 2: return
    let (x, y) = (v.elems[0].asF64, v.elems[1].asF64)
    if x.isNone or y.isNone: return
    q[i] = [x.get, y.get]
  some(q)

proc homog(m: array[9, float64]): Homography = Homography(m: m)

proc affineParams(p: JsonNode; r: array[4, float64]): Homography =
  ## The homography of the scale / rotate / skew / move params about the reference point.
  let rp = referencePoint(p, r)
  let (px, py) = (rp[0], rp[1])
  let (sx, sy) = (fnum(p, "scaleX", 100) / 100, fnum(p, "scaleY", 100) / 100)
  let a = degToRad(fnum(p, "rotate", 0))
  let (kx, ky) = (tan(degToRad(fnum(p, "skewX", 0))), tan(degToRad(fnum(p, "skewY", 0))))
  let (dx, dy) = (fnum(p, "dx", fnum(p, "x", 0)), fnum(p, "dy", fnum(p, "y", 0)))
  proc t(x, y: float64): Homography = homog([1.0, 0, x, 0, 1, y, 0, 0, 1])
  let (sa, ca) = (sin(a), cos(a))
  let rot = homog([ca, -sa, 0, sa, ca, 0, 0, 0, 1])
  let skew = homog([1.0, kx, 0, ky, 1, 0, 0, 0, 1])
  let scale = homog([sx, 0, 0, 0, sy, 0, 0, 0, 1])
  t(px + dx, py + dy).mul(rot).mul(skew).mul(scale).mul(t(-px, -py))

proc selectionBoundsJson(s: Session): JsonNode =
  let d = s.active
  if d.isNil or d.doc.selection.isNone: return newJNull()
  let r = d.doc.selection.get.contentBounds
  %*[r.x0, r.y0, r.width, r.height]

proc transformSelection(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "select.transformSelection"
  let d = s.active
  if d.isNil: raise noDocument()
  if d.doc.selection.isNone: raise other("no selection")
  let b = d.doc.selection.get.contentBounds
  var r = [float64(b.x0), float64(b.y0), float64(b.x1), float64(b.y1)]
  let rv = p.pget("rect")
  if rv.isArray and rv.len == 4:
    for i in 0 ..< 4: r[i] = rv.elems[i].asF64.get(0.0)
  let interp = parseInterp(p.pget("interpolation").asStr.get("bilinear"))
  var isWarp = false
  for k in ["warp", "style", "mesh", "grid"]:
    if not p.pget(k).isNil: isWarp = true
  if isWarp:
    let rr = rect(asI32(floor(r[0])), asI32(floor(r[1])), asI32(ceil(r[2])), asI32(ceil(r[3])))
    let w = warpFromParams(cmdId, p, [float64(rr.x0), float64(rr.y0), float64(rr.x1), float64(rr.y1)])
    if w.isIdentity: return %*{"changed": false}
    let selected = s.editWith("Transform Selection", proc(doc: var Document; active: var Option[LayerId]): bool =
      if doc.selection.isSome:
        let warped = warpMeshGray(doc.selection.get, proc(x, y: float64): (float64, float64) = w.map(x, y), interp)
        doc.selection = (if warped.contentBounds.isEmpty: none(Surface) else: some(warped))
      doc.selection.isSome)
    return %*{"changed": true, "selected": selected, "bounds": selectionBoundsJson(s)}
  var h: Homography
  let q = quadParam(p)
  let mv = p.pget("matrix")
  if q.isSome:
    let hh = rectToQuad(r, q.get)
    if hh.isNone: raise bad(cmdId, "degenerate quad")
    h = hh.get
  elif mv.isArray:
    var v: seq[float64]
    for e in mv.elems:
      let x = e.asF64
      if x.isSome: v.add x.get
    if v.len != 6: raise bad(cmdId, "matrix must be [a, b, c, d, e, f]")
    h = homog([v[0], v[2], v[4], v[1], v[3], v[5], 0, 0, 1])
  else: h = affineParams(p, r)
  if h.inverse.isNone: raise bad(cmdId, "the transform collapses the selection")
  var identity = true
  for i in 0 ..< 9:
    if not (abs(h.m[i] - IdentityHomography.m[i]) < 1e-12): identity = false
  if identity: return %*{"changed": false, "selected": true}
  let hm = h
  let selected = s.editWith("Transform Selection", proc(doc: var Document; active: var Option[LayerId]): bool =
    if doc.selection.isSome:
      let warped = warpGray(doc.selection.get, hm, interp)
      doc.selection = (if warped.contentBounds.isEmpty: none(Surface) else: some(warped))
    doc.selection.isSome)
  %*{"changed": true, "selected": selected, "bounds": selectionBoundsJson(s)}

proc selectExtraSpecs*(): seq[CommandSpec] =
  @[cmd("select.sky", "Sky", ["Select"], "",
        """{"mode":"replace|add|subtract|intersect"="replace","sampleAllLayers":bool=true,"threshold":0..100=50 (higher = stricter),"softness":0..100=50 (edge softness)} → {selected, changed, coverage (0–1 of the canvas), bounds [x,y,w,h]}""",
        hasDocE, runProc(selectSky(s, p))),
    cmd("select.isolateLayers", "Isolate Layers", ["Select"], "",
        """{"on":bool? (default: toggle)} — Layers panel lists only the selected layers → {isolated, layers}""",
        hasLayerE, runProc(isolateLayers(s, p)), journal = false),
    cmd("select.transformSelection", "Transform Selection", ["Select"], "",
        """{"rect":[x0,y0,x1,y1]? (frame; default = selection bounds),"quad"|"corners":[[x,y]×4]? (where the frame's corners go: distort/perspective),"matrix":[a,b,c,d,e,f]?,"scaleX":%=100,"scaleY":%=100,"rotate":deg=0,"skewX":deg=0,"skewY":deg=0,"dx":px=0,"dy":px=0,"reference":"center|topLeft|top|topRight|left|right|bottomLeft|bottom|bottomRight"|[x,y]="center","style"|"mesh"|"grid"|"warp":… (warp, as edit.transform.warp),"interpolation":"bilinear|bicubic|nearest"="bilinear"}""",
        hasSelectionE, runProc(transformSelection(s, p)))]
