## Filter › Vanishing Point (port of `vanishing.rs`): perspective planes, pasting onto a plane and
## cloning in perspective.
##
## A plane is a quad the user aligns with a flat surface in the photo. Its two vanishing points
## (where opposite edges meet) give the camera's focal length for a centred principal point,
## `f² = −(v₁ − c)·(v₂ − c)` (R. Hartley, A. Zisserman, *Multiple View Geometry*, 2nd ed., §8.8),
## which lifts the quad to 3-D: each corner is back-projected onto the plane through the
## vanishing directions' normal. That gives the plane's metric aspect (so pasted images keep
## their proportions) and lets perpendicular planes be torn off an edge by rotating about it in
## 3-D and projecting back.
##
## Pasting maps the image's rectangle into the plane's metric coordinates and warps it with the
## composed homography, clipped to the plane. Clone stamping copies, for each destination dab, the
## pixels at the same metric offset from the source point, so texture shrinks with distance as it
## does in the photo.

import std/[math, options]
import ../[geom, raster, color]
import ./[util, transform]

type
  V3 = array[3, float64]
  Quad2 = array[4, array[2, float64]]

  VpPlane* = object
    ## A perspective plane: corners in image px, clockwise from the plane's top-left.
    corners*: Quad2

  Edge* = enum
    ## Plane edges for tearing off a perpendicular plane.
    edTop = "top", edRight = "right", edBottom = "bottom", edLeft = "left"

  Lifted* = object
    ## The camera and a plane lifted to 3-D.
    focal*: float64
    center*: array[2, float64]
    pts*: array[4, V3] ## 3-D corners (camera coordinates, plane at unit distance along its normal)

  PlaneCheck* = enum
    ## Plane validity: Photoshop draws a valid plane's grid blue, a doubtful one yellow and an
    ## invalid one red.
    pcValid = "valid", pcQuestionable = "questionable", pcInvalid = "invalid"

  Scene* = object
    ## A scene: planes plus the shared focal length.
    planes*: seq[VpPlane]
    focal*: float64
    center*: array[2, float64]

proc sub(a, b: V3): V3 = [a[0] - b[0], a[1] - b[1], a[2] - b[2]]
proc add(a, b: V3): V3 = [a[0] + b[0], a[1] + b[1], a[2] + b[2]]
proc mul(a: V3; k: float64): V3 = [a[0] * k, a[1] * k, a[2] * k]
proc dot(a, b: V3): float64 = a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
proc cross(a, b: V3): V3 = [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]]
proc norm(a: V3): V3 = mul(a, 1.0 / rmax(sqrt(dot(a, a)), 1e-300))

proc meet(a, b, c, d: array[2, float64]): V3 =
  ## Intersection of lines (a, b) and (c, d) in homogeneous coordinates.
  cross(cross([a[0], a[1], 1.0], [b[0], b[1], 1.0]), cross([c[0], c[1], 1.0], [d[0], d[1], 1.0]))

proc isFinite(v: float64): bool {.inline.} = v == v and v - v == 0.0

proc homography*(p: VpPlane): Option[Homography] =
  ## Unit square (u, v) → image.
  rectToQuad([0.0, 0.0, 1.0, 1.0], p.corners)

proc focal*(p: VpPlane; c: array[2, float64]): Option[float64] =
  ## Focal length (px) implied by the plane's two vanishing points, if both are finite and
  ## consistent with a camera centred at `c`.
  let q = p.corners
  let v1 = meet(q[0], q[1], q[3], q[2])
  let v2 = meet(q[0], q[3], q[1], q[2])
  if abs(v1[2]) < 1e-9 or abs(v2[2]) < 1e-9: return none(float64)
  let (a, b) = ([v1[0] / v1[2] - c[0], v1[1] / v1[2] - c[1]], [v2[0] / v2[2] - c[0], v2[1] / v2[2] - c[1]])
  let f2 = -(a[0] * b[0] + a[1] * b[1])
  if f2 > 1.0: some(sqrt(f2)) else: none(float64)

proc lift*(p: VpPlane; f: float64; c: array[2, float64]): Lifted =
  ## Lifts the plane to 3-D with focal `f` (camera at the origin looking along +z).
  proc ray(q: array[2, float64]): V3 = [(q[0] - c[0]) / f, (q[1] - c[1]) / f, 1.0]
  let q = p.corners
  # Vanishing directions (or the edge directions when they're parallel in the image).
  proc dir(a, b, cc, d: array[2, float64]): V3 =
    let v = meet(a, b, cc, d)
    if abs(v[2]) < 1e-9 * rmax(abs(v[0]) + abs(v[1]), 1.0): norm([v[0], v[1], 0.0])
    else: norm(ray([v[0] / v[2], v[1] / v[2]]))
  let d1 = dir(q[0], q[1], q[3], q[2])
  let d2 = dir(q[0], q[3], q[1], q[2])
  var n = norm(cross(d1, d2))
  if n[2] < 0.0: n = mul(n, -1.0)
  result = Lifted(focal: f, center: c)
  for i in 0 .. 3:
    let r = ray(q[i])
    let lam = 1.0 / rmax(abs(dot(n, r)), 1e-9)
    result.pts[i] = mul(r, lam)

proc turns(p: VpPlane): Option[array[4, float64]] =
  ## Turn of each corner (`sin` of the angle between consecutive edges, positive when the corners
  ## run clockwise on screen); none when an edge is shorter than a pixel or a corner is not
  ## finite.
  let q = p.corners
  for c in q:
    for v in c:
      if not v.isFinite or abs(v) > 1e9: return none(array[4, float64])
  proc e(i: int): array[2, float64] = [q[(i + 1) mod 4][0] - q[i][0], q[(i + 1) mod 4][1] - q[i][1]]
  var o: array[4, float64]
  for i in 0 .. 3:
    let (a, b) = (e(i), e((i + 1) mod 4))
    let (la, lb) = (hypot(a[0], a[1]), hypot(b[0], b[1]))
    if la < 1.0 or lb < 1.0: return none(array[4, float64])
    o[i] = (a[0] * b[1] - a[1] * b[0]) / (la * lb)
  some(o)

proc check*(p: VpPlane; center: array[2, float64]): PlaneCheck =
  ## How usable the plane is: invalid for a quad that isn't convex (self-intersecting, a corner
  ## pushed inside, a near-straight or near-zero corner), questionable for a convex quad whose
  ## perspective can't come from a camera centred at `center` (or with a corner sharper than
  ## 10°), else valid.
  let tt = p.turns
  if tt.isNone: return pcInvalid
  let t = tt.get
  var allPos, allNeg = true
  for v in t:
    if not (v > 0.0): allPos = false
    if not (v < 0.0): allNeg = false
  var mn = F64Max
  for v in t: mn = rmin(mn, abs(v))
  if not (allPos or allNeg) or mn < sin(toRadians(1.0)) or p.homography.isNone: return pcInvalid
  if mn < sin(toRadians(10.0)): return pcQuestionable
  # Both vanishing points finite but no real focal length fits them.
  let q = p.corners
  let (v1, v2) = (meet(q[0], q[1], q[3], q[2]), meet(q[0], q[3], q[1], q[2]))
  proc finite(v: V3): bool = abs(v[2]) > 1e-9 * rmax(abs(v[0]) + abs(v[1]), 1.0)
  if finite(v1) and finite(v2) and p.focal(center).isNone: return pcQuestionable
  pcValid

proc normalized*(p: VpPlane): VpPlane =
  ## The same quad with its corners clockwise on screen, starting at the top-left one (the order
  ## `paste` and `tearOff` expect), however they were clicked.
  var q = p.corners
  let t = p.turns
  if t.isSome:
    var s = -0.0
    for v in t.get: s += v
    if s < 0.0:
      # Build the permutation in a temporary: an array literal assigned to `q` writes into `q`
      # while it is still being read.
      let r = [q[0], q[3], q[2], q[1]]
      q = r
  var first = 0
  for i in 1 .. 3:
    if totalCmp(q[i][0] + q[i][1], q[first][0] + q[first][1]) < 0: first = i
  VpPlane(corners: [q[first], q[(first + 1) mod 4], q[(first + 2) mod 4], q[(first + 3) mod 4]])

proc project*(l: Lifted; x: V3): Option[array[2, float64]] =
  if x[2] > 1e-9: some([l.center[0] + l.focal * x[0] / x[2], l.center[1] + l.focal * x[1] / x[2]])
  else: none(array[2, float64])

proc lengths*(l: Lifted): (float64, float64) =
  ## Metric side lengths (u along the top edge, v along the left edge).
  proc ln(a, b: V3): float64 = sqrt(dot(sub(a, b), sub(a, b)))
  ((ln(l.pts[0], l.pts[1]) + ln(l.pts[3], l.pts[2])) / 2.0, (ln(l.pts[0], l.pts[3]) + ln(l.pts[1], l.pts[2])) / 2.0)

proc newScene*(first: VpPlane; center: array[2, float64]; focal: Option[float64]; fallback: float64): Scene =
  ## Focal length from the first plane (or `fallback` px when it is fronto-parallel).
  var f = fallback
  if focal.isSome and focal.get > 1.0: f = focal.get
  else:
    let pf = first.focal(center)
    if pf.isSome: f = pf.get
  Scene(planes: @[first], focal: f, center: center)

proc lifted*(s: Scene; i: int): Lifted =
  ## Plane `i` lifted to 3-D (raises `ValueError` for a plane that does not exist).
  if i < 0 or i >= s.planes.len: raise newException(ValueError, "no plane " & $i)
  s.planes[i].lift(s.focal, s.center)

proc tearOff*(s: var Scene; frm: int; edge: Edge; angle, depth: float64): Option[int] =
  ## Tears a plane off edge `edge` of plane `frm`, turned by `angle` degrees about that edge
  ## (90 = perpendicular), extending `depth` times the parent's adjacent side. Returns its index.
  if frm < 0 or frm >= s.planes.len: return none(int)
  let lf = s.lifted(frm)
  let p = lf.pts
  # Edge endpoints (a → b, clockwise) and the parent's inward direction.
  let (ia, ib, ic) = (case edge
    of edTop: (0, 1, 3)
    of edRight: (1, 2, 0)
    of edBottom: (2, 3, 1)
    of edLeft: (3, 0, 2))
  let (a, b) = (p[ia], p[ib])
  let axis = norm(sub(b, a))
  let inward = sub(p[ic], p[ia])
  let len = sqrt(dot(inward, inward)) * rmax(depth, 0.05)
  # The new plane continues outward (−inward), turned about the edge by 180° − angle; of the two
  # turning directions pick the one that faces the camera with the parent in front of it.
  let outw = mul(norm(inward), -1.0)
  let th = toRadians(180.0 - angle)
  var best = none((float64, Quad2))
  for sgn in [1.0, -1.0]:
    let (sn, c) = (sin(sgn * th), cos(sgn * th))
    let rot = add(add(mul(outw, c), mul(cross(axis, outw), sn)), mul(axis, dot(axis, outw) * (1.0 - c)))
    let (a2, b2) = (add(a, mul(rot, len)), add(b, mul(rot, len)))
    let (pa, pb, pa2, pb2) = (lf.project(a), lf.project(b), lf.project(a2), lf.project(b2))
    if pa.isNone or pb.isNone or pa2.isNone or pb2.isNone: continue
    var n2 = norm(cross(sub(b, a), sub(a2, a)))
    if dot(n2, a) > 0.0: n2 = mul(n2, -1.0)
    let score = dot(n2, inward)
    if best.isNone or score > best.get[0]: best = some((score, [pa.get, pb.get, pa2.get, pb2.get]))
  if best.isNone: return none(int)
  let q = best.get[1]
  let (pa, pb, pa2, pb2) = (q[0], q[1], q[2], q[3])
  # Keep corners clockwise from the new plane's top-left (shared edge first or last).
  let corners = (case edge
    of edTop: [pa2, pb2, pb, pa]
    of edRight: [pa, pa2, pb2, pb]
    of edBottom: [pb, pa, pa2, pb2]
    of edLeft: [pb2, pb, pa, pa2])
  s.planes.add VpPlane(corners: corners)
  some(s.planes.len - 1)

proc frame*(s: Scene; i: int): Option[(Homography, float64, float64)] =
  ## Plane-metric frame of plane `i`: (homography unit square → image, metric width, height).
  if i < 0 or i >= s.planes.len: return none((Homography, float64, float64))
  let h = s.planes[i].homography
  if h.isNone: return none((Homography, float64, float64))
  let (lu, lv) = s.lifted(i).lengths
  some((h.get, lu, lv))

proc planeAt*(s: Scene; p: array[2, float64]): Option[int] =
  ## The plane whose quad contains image point `p`.
  for i in countdown(s.planes.len - 1, 0):
    let h = s.planes[i].homography
    if h.isNone: continue
    let inv = h.get.inverse
    if inv.isNone: continue
    let (u, v) = inv.get.apply(p[0], p[1])
    if u >= 0.0 and u <= 1.0 and v >= 0.0 and v <= 1.0: return some(i)
  none(int)

proc quadMask(h: Homography; area: Rect): seq[float32] =
  ## Coverage mask (0/1 surface values) of a plane quad over `area`.
  result = newSeq[float32](area.width * area.height)
  let inv = h.inverse
  if inv.isNone: return
  let w = area.width
  for i in 0 ..< result.len:
    let (x, y) = (float64(area.x0) + float64(i mod w) + 0.5, float64(area.y0) + float64(i div w) + 0.5)
    let (u, v) = inv.get.apply(x, y)
    if u >= 0.0 and u <= 1.0 and v >= 0.0 and v <= 1.0: result[i] = 1

proc paste*(scene: Scene; i: int; src: Surface; srcRect: Rect; at: array[2, float64]; width: float64): Option[Surface] =
  ## Pastes `src` (content in `srcRect`) onto plane `i` with its top-left at plane coordinates
  ## `at` (0..1) and its width `width` (fraction of the plane's width), aspect preserved in the
  ## plane's metric. Returns the warped image clipped to the plane (transparent elsewhere). Raises
  ## `AllocationError` when the warp would be too large.
  let fr = scene.frame(i)
  if fr.isNone: return none(Surface)
  let (h, lu, lv) = fr.get
  let (sw, sh) = (float64(srcRect.width), float64(srcRect.height))
  if sw <= 0.0 or sh <= 0.0: return none(Surface)
  # Source px → plane (u, v): u spans `width`, v keeps the metric aspect.
  let su = width / sw
  let sv = width * lu / lv / sw
  let toPlane = homography([su, 0.0, at[0] - float64(srcRect.x0) * su, 0.0, sv, at[1] - float64(srcRect.y0) * sv, 0.0, 0.0, 1.0])
  let full = h.mul(toPlane)
  var o = warpSurface(src, srcRect, full, ipBicubic)
  # Clip to the plane.
  let b = o.contentBounds
  if b.isEmpty: return some(o)
  let mask = quadMask(h, b)
  let n = o.format.channels
  var data = o.readRegion(b)
  for k in 0 ..< mask.len: data[k * n + n - 1] *= mask[k]
  o.writeRegion(b, data)
  o.prune()
  some(o)

proc sample(s: Surface; x, y: float64; n: int): seq[float32] =
  let (x0, y0) = (asI32(floor(x)), asI32(floor(y)))
  let (tx, ty) = (float32(x - float64(x0)), float32(y - float64(y0)))
  let r = s.readRegion(rect(x0, y0, x0 + 2, y0 + 2))
  result = newSeq[float32](n)
  for c in 0 ..< n:
    result[c] = (r[c] * (1'f32 - tx) + r[n + c] * tx) * (1'f32 - ty) + (r[2 * n + c] * (1'f32 - tx) + r[3 * n + c] * tx) * ty

proc cloneStroke*(scene: Scene; surf: var Surface; source: array[2, float64]; points: openArray[array[2, float64]];
                  size, hardness: float64; opacity: float32): int =
  ## Perspective clone stamp: one stroke of dabs at image points `points` on the destination
  ## plane, copying from `source` (image px) with the offset fixed in plane-metric units at the
  ## first dab. `size` is the brush diameter in px at the first dab; `hardness` 0..=1. Returns the
  ## number of dabs that changed pixels.
  if points.len == 0: return 0
  let first = points[0]
  let (ps, pd) = (scene.planeAt(source), scene.planeAt(first))
  if ps.isNone or pd.isNone: return 0
  let (fs, fd) = (scene.frame(ps.get), scene.frame(pd.get))
  if fs.isNone or fd.isNone: return 0
  let (hs, lus, lvs) = fs.get
  let (hd, lud, lvd) = fd.get
  let (hsInv, hdInv) = (hs.inverse, hd.inverse)
  if hsInv.isNone or hdInv.isNone: return 0
  proc metric(inv: Homography; lu, lv: float64; p: array[2, float64]): array[2, float64] =
    let (u, v) = inv.apply(p[0], p[1])
    [u * lu, v * lv]
  let ms = metric(hsInv.get, lus, lvs, source)
  let md0 = metric(hdInv.get, lud, lvd, first)
  let offset = [ms[0] - md0[0], ms[1] - md0[1]]
  # Brush radius in metric units (from the image size at the first dab).
  let probe = metric(hdInv.get, lud, lvd, [first[0] + size / 2.0, first[1]])
  let rM = rmax(hypot(probe[0] - md0[0], probe[1] - md0[1]), 1e-9)
  let srcCopy = surf
  let n = surf.format.channels
  for p in points:
    let mc = metric(hdInv.get, lud, lvd, p)
    # Image-space footprint: map a metric circle's bounding points.
    var bb = [F64Max, F64Max, -F64Max, -F64Max]
    for k in 0 ..< 16:
      let a = float64(k) / 16.0 * TAU
      let (u, v) = ((mc[0] + rM * cos(a)) / lud, (mc[1] + rM * sin(a)) / lvd)
      let (x, y) = hd.apply(u, v)
      bb = [rmin(bb[0], x), rmin(bb[1], y), rmax(bb[2], x), rmax(bb[3], y)]
    let area = rect(asI32(floor(bb[0])), asI32(floor(bb[1])), satAdd(asI32(ceil(bb[2])), 1), satAdd(asI32(ceil(bb[3])), 1))
    if area.isEmpty or area.width > 4096 or area.height > 4096: continue
    var data = surf.readRegion(area)
    let w = area.width
    var changed = false
    for i in 0 ..< data.len div n:
      let (x, y) = (float64(area.x0) + float64(i mod w) + 0.5, float64(area.y0) + float64(i div w) + 0.5)
      let m = metric(hdInv.get, lud, lvd, [x, y])
      let d = hypot(m[0] - mc[0], m[1] - mc[1]) / rM
      if d >= 1.0: continue
      let hard = clamp(hardness, 0.0, 0.99)
      let fall = (if d <= hard: 1.0 else: 1.0 - (d - hard) / (1.0 - hard))
      let k = float32(fall * fall * (3.0 - 2.0 * fall)) * opacity
      # Source pixel at the same metric offset.
      let sm = [m[0] + offset[0], m[1] + offset[1]]
      let (sx, sy) = hs.apply(sm[0] / lus, sm[1] / lvs)
      let s = sample(srcCopy, sx - 0.5, sy - 0.5, n)
      for c in 0 ..< n: data[i * n + c] += (s[c] - data[i * n + c]) * k
      changed = true
    if changed:
      surf.writeRegion(area, data)
      inc result
