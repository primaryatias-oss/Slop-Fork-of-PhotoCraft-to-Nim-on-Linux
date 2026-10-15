## Filter › Adaptive Wide Angle (port of `wideangle.rs`): straighten lines that a wide-angle or
## fisheye lens bends.
##
## The user marks constraints by their two end points; the lens model (equidistant fisheye
## `r = f·θ`, rectilinear perspective `r = f·tan θ`, or a full 2:1 equirectangular sphere) turns
## them into rays, and the scene line between them is the great circle through both rays, which is
## sampled back into the image as the curve the user sees. A content-preserving mesh warp then
## makes every constrained curve straight (and horizontal or vertical when asked) while keeping
## each mesh triangle as similar as possible to its original shape (T. Igarashi et al., *As-Rigid-
## As-Possible Shape Manipulation*, SIGGRAPH 2005, as used by R. Carroll, M. Agrawala, A. Agarwala,
## *Optimizing Content-Preserving Projections for Wide-Angle Images*, SIGGRAPH 2009). The linear
## least-squares problems are solved with conjugate gradients; the result is rendered by textured
## triangles (`warp.warpTriangles`).

import std/[math, options, json]
import ../[geom, raster]
import ./[util, transform, warp]

type
  WideModel* = enum
    ## Lens projection of the source image.
    wmAuto = "auto", wmFisheye = "fisheye", wmPerspective = "perspective", wmFullSpherical = "fullSpherical"

  Orientation* = enum
    ## Constraint orientation.
    orFree = "free", orHorizontal = "horizontal", orVertical = "vertical"

  Constraint* = object
    ## A constraint: the end points of a scene line (image px).
    a*, b*: array[2, float64]
    orientation*: Orientation

  WideAngle* = object
    ## Adaptive Wide Angle settings.
    model*: WideModel
    focalLength*: float64 ## mm (35 mm frame after the crop factor)
    cropFactor*: float64
    scale*: float64       ## output scale in percent
    constraints*: seq[Constraint]

  Camera* = object
    ## Camera model over an image frame.
    model*: WideModel
    f*: float64 ## focal length in px
    c*, size*: array[2, float64]

  WideMesh* = object
    ## The solved warp: a grid over the frame with each vertex's output position.
    nx*, ny*: int
    verts*: seq[(array[2, float64], array[2, float64])] ## (output, source) per vertex, row-major
    curves*: seq[seq[array[2, float64]]] ## each constraint's curve in the source (for overlays)
    residual*: float64 ## max deviation from straight of the constrained curves after the warp (px)

proc parseWideModel*(s: string): Option[WideModel] =
  case s
  of "auto": some(wmAuto)
  of "fisheye": some(wmFisheye)
  of "perspective": some(wmPerspective)
  of "fullSpherical", "spherical": some(wmFullSpherical)
  else: none(WideModel)

proc defaultWideAngle*(): WideAngle = WideAngle(model: wmAuto, focalLength: 0, cropFactor: 1, scale: 100)

proc dot(a, b: array[3, float64]): float64 = a[0] * b[0] + a[1] * b[1] + a[2] * b[2]
proc norm(v: array[3, float64]): array[3, float64] =
  let l = rmax(sqrt(dot(v, v)), 1e-300)
  [v[0] / l, v[1] / l, v[2] / l]

proc newCamera*(p: WideAngle; frame: Rect): Camera =
  ## Resolves `Auto` and the focal length: 35 mm-equivalent `focalMm × crop` over a 36 mm long
  ## side; `0` picks a default (fisheye 12 mm, perspective 24 mm).
  let (w, h) = (float64(max(frame.width, 1)), float64(max(frame.height, 1)))
  var f35 = p.focalLength * rmax(p.cropFactor, 0.1)
  var model = p.model
  if model == wmAuto:
    model = (if abs(w / h - 2.0) < 0.01: wmFullSpherical
             elif f35 > 0.0 and f35 < 17.0: wmFisheye
             else: wmPerspective)
  if not (f35 > 0.0): f35 = (if model == wmFisheye: 12.0 else: 24.0)
  Camera(model: model, f: f35 / 36.0 * max(w, h), c: [float64(frame.x0 + frame.x1) / 2.0, float64(frame.y0 + frame.y1) / 2.0],
         size: [w, h])

proc ray*(cam: Camera; x, y: float64): array[3, float64] =
  ## Image px → unit ray (x right, y down, z forward).
  let (dx, dy) = (x - cam.c[0], y - cam.c[1])
  var v: array[3, float64]
  case cam.model
  of wmFullSpherical:
    let lon = dx / cam.size[0] * TAU
    let lat = dy / cam.size[1] * PI
    v = [sin(lon) * cos(lat), sin(lat), cos(lon) * cos(lat)]
  of wmFisheye:
    let r = hypot(dx, dy)
    let th = r / cam.f
    v = (if r < 1e-12: [0.0, 0.0, 1.0] else: [sin(th) * dx / r, sin(th) * dy / r, cos(th)])
  else: v = [dx, dy, cam.f]
  norm(v)

proc pixel*(cam: Camera; d: array[3, float64]): Option[array[2, float64]] =
  ## Unit ray → image px.
  case cam.model
  of wmFullSpherical:
    let lon = arctan2(d[0], d[2])
    let lat = arcsin(clamp(d[1], -1.0, 1.0))
    some([cam.c[0] + lon / TAU * cam.size[0], cam.c[1] + lat / PI * cam.size[1]])
  of wmFisheye:
    let th = arccos(clamp(d[2], -1.0, 1.0))
    let s = hypot(d[0], d[1])
    if s < 1e-12: return some(cam.c)
    some([cam.c[0] + cam.f * th * d[0] / s, cam.c[1] + cam.f * th * d[1] / s])
  else:
    if d[2] > 1e-6: some([cam.c[0] + cam.f * d[0] / d[2], cam.c[1] + cam.f * d[1] / d[2]]) else: none(array[2, float64])

proc arc*(cam: Camera; a, b: array[2, float64]; n: int): seq[array[2, float64]] =
  ## `n + 1` image points along the scene line (great circle) from `a` to `b`.
  let (da, db) = (cam.ray(a[0], a[1]), cam.ray(b[0], b[1]))
  let omega = arccos(clamp(dot(da, db), -1.0, 1.0))
  for k in 0 .. n:
    let t = float64(k) / float64(n)
    var d: array[3, float64]
    if omega < 1e-9: d = da
    else:
      let (sa, sb) = (sin((1.0 - t) * omega) / sin(omega), sin(t * omega) / sin(omega))
      d = norm([sa * da[0] + sb * db[0], sa * da[1] + sb * db[1], sa * da[2] + sb * db[2]])
    let p = cam.pixel(d)
    if p.isSome: result.add p.get

proc triangles*(m: WideMesh): seq[array[3, int]] =
  let w1 = m.nx + 1
  for j in 0 ..< m.ny:
    for i in 0 ..< m.nx:
      let a = j * w1 + i
      result.add [a, a + 1, a + w1 + 1]
      result.add [a, a + w1 + 1, a + w1]

proc map*(m: WideMesh; frame: Rect; p: array[2, float64]): array[2, float64] =
  ## Output position of source point `p` (bilinear in its grid cell).
  let (cw, ch) = (float64(frame.width) / float64(m.nx), float64(frame.height) / float64(m.ny))
  let gx = clamp((p[0] - float64(frame.x0)) / cw, 0.0, float64(m.nx) - 1e-9)
  let gy = clamp((p[1] - float64(frame.y0)) / ch, 0.0, float64(m.ny) - 1e-9)
  let (i, j) = (asUsize(floor(gx)), asUsize(floor(gy)))
  let (tx, ty) = (gx - float64(i), gy - float64(j))
  let w1 = m.nx + 1
  template v(ii, jj: int): array[2, float64] = m.verts[jj * w1 + ii][0]
  let (a, b, c, d) = (v(i, j), v(i + 1, j), v(i, j + 1), v(i + 1, j + 1))
  [(a[0] * (1.0 - tx) + b[0] * tx) * (1.0 - ty) + (c[0] * (1.0 - tx) + d[0] * tx) * ty,
   (a[1] * (1.0 - tx) + b[1] * tx) * (1.0 - ty) + (c[1] * (1.0 - tx) + d[1] * tx) * ty]

proc solve*(p: WideAngle; frame: Rect): WideMesh =
  ## Solves the content-preserving warp for `p` over `frame`.
  let cam = newCamera(p, frame)
  let (fw, fh) = (float64(max(frame.width, 1)), float64(max(frame.height, 1)))
  let cells = 48.0
  let cell = max(fw, fh) / cells
  let nx = clamp(asUsize(round(fw / cell)), 2, 96)
  let ny = clamp(asUsize(round(fh / cell)), 2, 96)
  let (cw, ch) = (fw / float64(nx), fh / float64(ny))
  let w1 = nx + 1
  let nv = w1 * (ny + 1)
  var src = newSeq[array[2, float64]](nv)
  for k in 0 ..< nv: src[k] = [float64(frame.x0) + float64(k mod w1) * cw, float64(frame.y0) + float64(k div w1) * ch]
  var curves: seq[seq[array[2, float64]]]
  for c in p.constraints: curves.add cam.arc(c.a, c.b, 24)
  var x = newSeq[float64](2 * nv)
  for k, s in src:
    x[2 * k] = s[0]
    x[2 * k + 1] = s[1]
  # Bilinear weights of a source point in the grid.
  proc weights(q: array[2, float64]): array[4, (int, float64)] =
    let gx = clamp((q[0] - float64(frame.x0)) / cw, 0.0, float64(nx) - 1e-9)
    let gy = clamp((q[1] - float64(frame.y0)) / ch, 0.0, float64(ny) - 1e-9)
    let (i, j) = (asUsize(floor(gx)), asUsize(floor(gy)))
    let (tx, ty) = (gx - float64(i), gy - float64(j))
    let a = j * w1 + i
    [(a, (1.0 - tx) * (1.0 - ty)), (a + 1, tx * (1.0 - ty)), (a + w1, (1.0 - tx) * ty), (a + w1 + 1, tx * ty)]
  var tris: seq[array[3, int]]
  for j in 0 ..< ny:
    for i in 0 ..< nx:
      let a = j * w1 + i
      tris.add [a, a + 1, a + w1 + 1]
      tris.add [a, a + w1 + 1, a + w1]
      # Both diagonals so the shape term is symmetric.
      tris.add [a, a + 1, a + w1]
      tris.add [a + 1, a + w1 + 1, a + w1]
  var active: seq[int]
  for k in 0 ..< curves.len:
    if curves[k].len >= 3: active.add k
  if active.len > 0:
    var normals = newSeq[array[2, float64]](active.len)
    for nn in normals.mitems: nn = [0.0, 1.0]
    for round in 0 ..< 4:
      var ls = newSparseLs(2 * nv)
      # Shape: v0 = v1 + u (v2 − v1) + v R90 (v2 − v1), with (u, v) from the source.
      for t in tris:
        for rot in 0 .. 2:
          let (i0, i1, i2) = (t[rot], t[(rot + 1) mod 3], t[(rot + 2) mod 3])
          let (p0, p1, p2) = (src[i0], src[i1], src[i2])
          let e = [p2[0] - p1[0], p2[1] - p1[1]]
          let l2 = e[0] * e[0] + e[1] * e[1]
          let d = [p0[0] - p1[0], p0[1] - p1[1]]
          let u = (d[0] * e[0] + d[1] * e[1]) / l2
          let v = (d[0] * -e[1] + d[1] * e[0]) / l2
          ls.add(@[(2 * i0, 1.0), (2 * i1, -1.0 + u), (2 * i2, -u), (2 * i2 + 1, v), (2 * i1 + 1, -v)], 0.0, 1.0)
          ls.add(@[(2 * i0 + 1, 1.0), (2 * i1 + 1, -1.0 + u), (2 * i2 + 1, -u), (2 * i2, -v), (2 * i1, v)], 0.0, 1.0)
      # Weak anchoring to the source (fixes the global similarity).
      for k, s in src:
        ls.add(@[(2 * k, 1.0)], s[0], 0.02)
        ls.add(@[(2 * k + 1, 1.0)], s[1], 0.02)
      # Lines.
      for ci, k in active:
        let c = curves[k]
        let n = (case p.constraints[k].orientation
          of orHorizontal: [0.0, 1.0]
          of orVertical: [1.0, 0.0]
          of orFree: normals[ci])
        let w0 = weights(c[0])
        for qi in 1 ..< c.len:
          let wq = weights(c[qi])
          var row = newSeqOfCap[(int, float64)](16)
          for (i, w) in wq:
            row.add (2 * i, n[0] * w)
            row.add (2 * i + 1, n[1] * w)
          for (i, w) in w0:
            row.add (2 * i, -n[0] * w)
            row.add (2 * i + 1, -n[1] * w)
          ls.add(row, 0.0, 10.0)
      x = ls.solve(x, 400 + 200 * round)
      # Re-estimate free normals from the current end points.
      proc pos(q: array[2, float64]): array[2, float64] =
        result = [0.0, 0.0]
        for (i, w) in weights(q): result = [result[0] + w * x[2 * i], result[1] + w * x[2 * i + 1]]
      var changed = false
      for ci, k in active:
        let c = curves[k]
        let (a, b) = (pos(c[0]), pos(c[^1]))
        let d = [b[0] - a[0], b[1] - a[1]]
        let l = rmax(hypot(d[0], d[1]), 1e-9)
        let nn = [-d[1] / l, d[0] / l]
        if abs(nn[0] - normals[ci][0]) + abs(nn[1] - normals[ci][1]) > 1e-3: changed = true
        normals[ci] = nn
      var allFixed = true
      for k in active:
        if p.constraints[k].orientation == orFree: allFixed = false
      if round == 0 and allFixed: break
      if round > 0 and not changed: break
  # Scale about the centre.
  let s = clamp(p.scale / 100.0, 0.1, 10.0)
  let c = cam.c
  var verts = newSeq[(array[2, float64], array[2, float64])](nv)
  for k in 0 ..< nv: verts[k] = ([c[0] + (x[2 * k] - c[0]) * s, c[1] + (x[2 * k + 1] - c[1]) * s], src[k])
  result = WideMesh(nx: nx, ny: ny, verts: verts, curves: curves, residual: 0)
  # Straightness residual: distance of curve samples from the chord of their mapped ends.
  var worst = 0.0
  for cv in result.curves:
    if cv.len < 3: continue
    var pts: seq[array[2, float64]]
    for q in cv: pts.add result.map(frame, q)
    let (a, b) = (pts[0], pts[^1])
    let d = [b[0] - a[0], b[1] - a[1]]
    let l = rmax(hypot(d[0], d[1]), 1e-9)
    for q in pts: worst = rmax(worst, abs((q[0] - a[0]) * d[1] - (q[1] - a[1]) * d[0]) / l)
  result.residual = worst

proc apply*(src: Surface; frame: Rect; p: WideAngle; interp: Interp): Surface =
  ## Applies Adaptive Wide Angle to `src` over `frame` (pixels outside the warped image become
  ## transparent).
  let mesh = solve(p, frame)
  warpTriangles(src, frame, mesh.verts, mesh.triangles, interp)

# ---------------------------------------------------------------- JSON options

proc num(n: JsonNode; name: string): float64 =
  case n.kind
  of JInt: float64(n.getBiggestInt)
  of JFloat: n.getFloat
  else: raise newException(ValueError, "invalid type for " & name & ": expected a number")

proc point(n: JsonNode; name: string): array[2, float64] =
  if n.kind != JArray or n.len != 2: raise newException(ValueError, name & " is [x, y]")
  [num(n[0], name), num(n[1], name)]

proc wideAngleFromJson*(j: JsonNode): WideAngle =
  ## The settings from their JSON form (camelCase, missing fields take their defaults); raises
  ## `ValueError` on a malformed value.
  if j.kind != JObject: raise newException(ValueError, "Adaptive Wide Angle options must be an object")
  result = defaultWideAngle()
  for k, v in j:
    case k
    of "model":
      var found = false
      if v.kind == JString:
        for m in WideModel:
          if $m == v.getStr:
            result.model = m
            found = true
      if not found: raise newException(ValueError, "unknown variant for model: " & $v)
    of "focalLength": result.focalLength = num(v, k)
    of "cropFactor": result.cropFactor = num(v, k)
    of "scale": result.scale = num(v, k)
    of "constraints":
      if v.kind != JArray: raise newException(ValueError, "constraints must be an array")
      for c in v:
        if c.kind != JObject or not c.hasKey("a") or not c.hasKey("b"):
          raise newException(ValueError, "a constraint is {a: [x, y], b: [x, y]}")
        var o = orFree
        if c.hasKey("orientation"):
          var found = false
          if c["orientation"].kind == JString:
            for q in Orientation:
              if $q == c["orientation"].getStr:
                o = q
                found = true
          if not found: raise newException(ValueError, "unknown orientation " & $c["orientation"])
        result.constraints.add Constraint(a: point(c["a"], "a"), b: point(c["b"], "b"), orientation: o)
    else: discard
