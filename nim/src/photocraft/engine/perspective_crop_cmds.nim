## The Perspective Crop tool's commit (port of `perspective_crop_cmds.rs`): `image.perspectiveCrop`.
##
## Four corners (TL, TR, BR, BL in document px) mark a quad that is a rectangle seen in perspective
## (a photographed document, a facade). The whole document is mapped through the homography taking
## that quad onto an upright W x H rectangle, which becomes the new canvas, in one undo step: every
## raster layer, layer mask, alpha channel and the Quick Mask is resampled through the map, the
## Background stays a Background over the background colour where the quad reaches past the image,
## and pixels outside the quad are deleted.
##
## Type, shape, fill and smart object layers are rasterized first: a projective map has no editable
## equivalent for them. Vector masks, paths, notes, slices and guides follow the affine map closest
## to the homography (exact when the quad is a parallelogram).

import std/[json, options, math]
import ./core
import ../rfmt
import ../algo/transform as atransform

const
  CropCmd = "image.perspectiveCrop"
  MaxSide = 300_000         ## the largest output side (the `file.new`, Canvas Size and Crop limit)
  MaxCoord = 1e7            ## corner coordinates beyond this are refused
  MaxResolution = 10_000.0  ## the highest resolution (ppi) the crop may set

type Quad = array[4, array[2, float64]]

proc cbad(msg: string): ref EngineError = bad(CropCmd, msg)

proc isFin(v: float64): bool {.inline.} = v == v and abs(v) != Inf

proc hasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")

proc corners(p: JsonNode): Quad =
  ## `corners`: four `[x, y]` points, TL, TR, BR, BL, forming a convex quad clockwise on screen.
  const shape = "`corners` must be four [x, y] points: top-left, top-right, bottom-right, bottom-left"
  let list = p.pget("corners")
  if not list.isArray or list.len != 4: raise cbad(shape)
  for i, c in list.elems:
    if not c.isArray or c.len != 2: raise cbad(shape)
    let (x, y) = (c.elems[0].asF64, c.elems[1].asF64)
    if x.isNone or y.isNone: raise cbad(shape)
    if not (isFin(x.get) and isFin(y.get)) or abs(x.get) > MaxCoord or abs(y.get) > MaxCoord:
      raise cbad("corner [" & displayF64(x.get) & ", " & displayF64(y.get) & "] must be finite and within ±" &
                 displayF64(MaxCoord) & " px")
    result[i] = [x.get, y.get]
  if not convexQuad(result):
    raise cbad("the corners must form a convex quadrilateral at least 1 px in area, in order top-left, top-right, bottom-right, bottom-left (clockwise on screen)")

proc outSize(p: JsonNode; q: Quad): (int, int) =
  ## Output size: `width`/`height` in whole px; one alone keeps the quad's proportions; neither
  ## takes the quad's mean side lengths (`quadSize`).
  proc side(key: string): Option[float64] =
    let v = p.pget(key)
    if v.isNull: return none(float64)
    let n = v.asF64
    if n.isSome and isFin(n.get) and round(n.get) >= 1 and round(n.get) <= float64(MaxSide): return some(round(n.get))
    raise cbad("`" & key & "` = " & jsonText(v) & " must be a number of pixels from 1 to " & $MaxSide)
  let (qw, qh) = quadSize(q)
  let (sw, sh) = (side("width"), side("height"))
  let (w, h) =
    if sw.isSome and sh.isSome: (sw.get, sh.get)
    elif sw.isSome: (sw.get, sw.get * qh / qw)
    elif sh.isSome: (sh.get * qw / qh, sh.get)
    else: (qw, qh)
  proc px(v0: float64): int =
    let v = max(round(v0), 1.0)
    if not isFin(v) or v > float64(MaxSide):
      raise cbad("the output would be " & displayF64(v) & " px on a side; the limit is " & $MaxSide)
    int(v)
  let pw = px(w)
  (pw, px(h))

proc resolution(p: JsonNode): Option[float32] =
  let v = p.pget("resolution")
  if v.isNull: return none(float32)
  let r = v.asF64
  if r.isSome and isFin(r.get) and r.get >= 1 and r.get <= MaxResolution: return some(float32(r.get))
  raise cbad("`resolution` = " & jsonText(v) & " must be a number of pixels per inch from 1 to " & displayF64(MaxResolution))

proc interpolation(p: JsonNode): Interp =
  let v = p.pget("interpolation")
  if v.isNull: return ipBicubic
  if v.kind == JString and v.str in ["nearest", "nearestNeighbor", "bilinear", "bicubic"]: return parseInterp(v.str)
  raise cbad("`interpolation` = " & jsonText(v) & " must be \"bicubic\", \"bilinear\" or \"nearest\"")

proc nearestAffine(q: Quad; w, h: float64): Affine =
  ## The affine map closest to the crop for geometry that can't take a homography (vector masks,
  ## paths, notes, guides): the quad's centroid goes to the output's centre and its mean horizontal
  ## and vertical edges to the output's sides. Exact when the quad is a parallelogram.
  let (tl, tr, br, bl) = (q[0], q[1], q[2], q[3])
  let u = [(tr[0] - tl[0] + br[0] - bl[0]) / 2.0, (tr[1] - tl[1] + br[1] - bl[1]) / 2.0]
  let v = [(bl[0] - tl[0] + br[0] - tr[0]) / 2.0, (bl[1] - tl[1] + br[1] - tr[1]) / 2.0]
  let det = u[0] * v[1] - v[0] * u[1]
  if not isFin(det) or abs(det) < 1e-12: return IdentityAffine
  let (m00, m01, m10, m11) = (w * v[1] / det, -w * v[0] / det, -h * u[1] / det, h * u[0] / det)
  let c = [(tl[0] + tr[0] + br[0] + bl[0]) / 4.0, (tl[1] + tr[1] + br[1] + bl[1]) / 4.0]
  Affine(m: [m00, m10, m01, m11, w / 2.0 - (m00 * c[0] + m01 * c[1]), h / 2.0 - (m10 * c[0] + m11 * c[1])])

proc cropWarpGray(s: Surface; h: Homography; interp: Interp; clip: Rect): Surface =
  ## A one-channel surface (mask, alpha channel, Quick Mask) through `h`, kept inside `clip`; what
  ## the map uncovers takes the surface's default (a mask's reveal or hide).
  const what = "a perspective crop of a mask or channel"
  let dp = s.defaultPixel
  let default = (if dp.len > 0: dp[0] else: 0'f32)
  let fmt = s.format
  result = surfaceWithDefault(fmt, [default])
  let src = s.contentBounds
  if src.isEmpty: return
  # Content with alpha 1 over a transparent outside, so the edges blend into the default.
  let v = allocReadRegion(s, src, what)
  if v.len > high(int) div 2: raise cbad("the mask is too large")
  var lifted = allocFilled(v.len * 2, 1'f32, what)
  for i, g in v: lifted[i * 2] = g
  var tmp = newSurface(pixelFormat(fmt.mode, fmt.sample, true))
  tmp.tryWriteRegion(src, lifted)
  let warped = warpSurfaceClipped(tmp, src, h, interp, clip)
  let b = warped.contentBounds.intersect(clip)
  if not b.isEmpty:
    let px = allocReadRegion(warped, b, what)
    var flat = newSeq[float32](px.len div 2)
    for i in 0 ..< flat.len:
      let (g, a) = (px[i * 2], px[i * 2 + 1])
      flat[i] = g * a + default * (1 - a)
    result.tryWriteRegion(b, flat)
  result.prune()

proc warpLayers(layers: var seq[Layer]; h: Homography; interp: Interp; clip: Rect; bg: seq[float32]) =
  ## Maps every layer's pixels and masks through `h` into `clip`.
  for l in layers.mitems:
    allocCheckpoint("a perspective crop")
    let background = isBackground(l)
    case l.content.kind
    of lkGroup: warpLayers(l.content.group.children, h, interp, clip, bg)
    of lkRaster:
      let surf = l.content.raster
      let warped = warpSurfaceClipped(surf, surf.contentBounds, h, interp, clip)
      if background:
        # The Background stays opaque: the mapped pixels over the background colour.
        var base = newSurface(surf.format)
        base.fillRect(clip, bg)
        compositeOver(base, warped)
        base.prune()
        l.content.raster = base
      else: l.content.raster = warped
    else: discard # adjustment layers have no pixels; the rest was rasterized before
    if l.mask.isSome:
      l.mask.get.surface = cropWarpGray(l.mask.get.surface, h, interp, clip)
    l.fillCache = none(FillCache)

proc applyCrop(doc: var Document; q: Quad; h: Homography; size: (int, int); interp: Interp; bg: array[4, float32]) =
  let (w, ht) = size
  let clip = rectXywh(0, 0, w, ht)
  let bgPx = fromRgba(doc.pixelFormat, bg)
  warpLayers(doc.layers, h, interp, clip, bgPx)
  for ch in doc.channels.mitems: ch.surface = cropWarpGray(ch.surface, h, interp, clip)
  if doc.quickMask.isSome: doc.quickMask.get.surface = cropWarpGray(doc.quickMask.get.surface, h, interp, clip)
  doc.selection = none(Surface)
  transformGeometry(doc, nearestAffine(q, float64(w), float64(ht)))
  doc.size = size(w, ht)
  refreshDoc(doc, rfShapes)

proc perspectiveCrop(s: var Session; p: JsonNode): JsonNode =
  let q = corners(p)
  let (w, h) = outSize(p, q)
  let res = resolution(p)
  let interp = interpolation(p)
  let d = s.active
  if d.isNil: raise noDocument()
  checkResampleBudget(CropCmd, d.doc[], w, h, "")
  var rasterize = false
  for (_, _, l) in d.doc[].walk:
    if l.content.kind in {lkText, lkShape, lkFill, lkSmart}: rasterize = true
  let map = quadToRect(q, float64(w), float64(h))
  if map.isNone: raise cbad("the corners don't define a perspective")
  let bg = s.tools.background
  discard compound(s, "Perspective Crop", proc(s: var Session): bool =
    if rasterize: discard rasterizeAll(s)
    s.edit("Perspective Crop", proc(doc: var Document; active: var Option[LayerId]) =
      applyCrop(doc, q, map.get, (w, h), interp, bg)
      if res.isSome: doc.resolutionDpi = res.get)
    true)
  let st = s.active
  let dpi = (if st.isNil: newJNull() else: %st.doc.resolutionDpi)
  var cs = newJArray()
  for c in q: cs.add %*[c[0], c[1]]
  %*{"width": w, "height": h, "resolution": dpi, "corners": cs}

proc perspectiveCropSpecs*(): seq[CommandSpec] =
  @[cmd(CropCmd, "Perspective Crop", [], "",
        """{"corners":[[x,y],[x,y],[x,y],[x,y]] (top-left, top-right, bottom-right, bottom-left; a convex quad, document px),"width":px,"height":px,"resolution":ppi,"interpolation":"bicubic|bilinear|nearest"="bicubic"} → {width,height,resolution,corners} (the quad is mapped onto an upright width x height canvas, all layers, one undo step; no width/height: the quad's mean side lengths, one alone keeps its proportions; type, shape, fill and smart object layers are rasterized)""",
        hasDocE, runProc(perspectiveCrop(s, p)))]
