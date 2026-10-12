## Warp geometry shared by Edit › Transform › Warp, smart-object warps and Warp Text.
##
## Two kinds of warp, both forward maps from a source box to document space:
## - **Styles** (`WarpStyle`, Photoshop's fifteen presets plus bend and horizontal/vertical
##   distortion). Each is a single bicubic Bezier patch whose control grid follows the bend.
##   Every style is the identity at bend 0 and continuous in the bend.
## - **Custom meshes** (`BezierMesh`): a grid of bicubic Bezier patches. Split warps add patch
##   rows/columns by exact de Casteljau subdivision, so splitting never changes the shape.
##
## Pure geometry: resampling lives in `photocraft/algo/warp`. Port of `crates/geom/src/warp.rs`.

import std/[math, options, strutils, json]

type
  WarpStyle* = enum
    ## Warp style: none, a custom mesh, or one of Photoshop's presets (menu order).
    wsNone = "none", wsCustom = "custom", wsArc = "arc", wsArcLower = "arcLower",
    wsArcUpper = "arcUpper", wsArch = "arch", wsBulge = "bulge", wsShellLower = "shellLower",
    wsShellUpper = "shellUpper", wsFlag = "flag", wsWave = "wave", wsFish = "fish",
    wsRise = "rise", wsFisheye = "fisheye", wsInflate = "inflate", wsSqueeze = "squeeze",
    wsTwist = "twist"

  P2* = array[2, float64]
  Row4 = array[4, P2]
  Grid = array[4, Row4] ## A single bicubic patch's control points `[row][column]`, box units.

  StyleWarp* = object
    ## A preset style prepared for one box: its control grid and the box it bends.
    grid: Grid
    o: (float64, float64)
    size: (float64, float64)

  BezierMesh* = object
    ## A grid of bicubic Bezier patches over the unit square of a source box.
    ##
    ## `us`/`vs` are the patch boundaries in normalised coordinates (`0..1`, increasing, first 0
    ## and last 1). Control points are a `(3·(us.len-1)+1) × (3·(vs.len-1)+1)` grid in
    ## row-major order, in output (document) coordinates.
    us*, vs*: seq[float64]
    points*: seq[P2]

  Warp* = object
    ## A complete warp: what Photoshop's `warp` descriptor stores, plus the custom mesh.
    style*: WarpStyle
    bend*: float64        ## Bend in percent (-100..100), presets only.
    hDistort*: float64
    vDistort*: float64
    vertical*: bool       ## Warp along the vertical axis.
    bounds*: array[4, float64] ## The source box `[x0, y0, x1, y1]`.
    mesh*: Option[BezierMesh]  ## Control mesh for `wsCustom` (output coordinates).

const Presets*: array[15, WarpStyle] = [wsArc, wsArcLower, wsArcUpper, wsArch, wsBulge,
  wsShellLower, wsShellUpper, wsFlag, wsWave, wsFish, wsRise, wsFisheye, wsInflate, wsSqueeze, wsTwist]

proc psdName*(w: WarpStyle): string =
  ## PSD `warpStyle` enum value (`warpArc`, `warpCustom`, `warpNone`…).
  let id = $w
  "warp" & id[0].toUpperAscii & id[1..^1]

proc id*(w: WarpStyle): string = $w

proc label*(w: WarpStyle): string =
  case w
  of wsNone: "None"
  of wsCustom: "Custom"
  of wsArc: "Arc"
  of wsArcLower: "Arc Lower"
  of wsArcUpper: "Arc Upper"
  of wsArch: "Arch"
  of wsBulge: "Bulge"
  of wsShellLower: "Shell Lower"
  of wsShellUpper: "Shell Upper"
  of wsFlag: "Flag"
  of wsWave: "Wave"
  of wsFish: "Fish"
  of wsRise: "Rise"
  of wsFisheye: "Fisheye"
  of wsInflate: "Inflate"
  of wsSqueeze: "Squeeze"
  of wsTwist: "Twist"

proc parseWarpStyle*(s: string): Option[WarpStyle] =
  ## Parses a short id, a PSD name or a label (case-insensitive, spaces ignored).
  var k = ""
  for c in s:
    if not c.isSpaceAscii: k.add c.toLowerAscii
  for w in WarpStyle:
    if w.id.toLowerAscii == k or w.psdName.toLowerAscii == k: return some(w)
  none(WarpStyle)

proc isPreset*(w: WarpStyle): bool = w notin {wsNone, wsCustom}

# ---- preset grids ----

proc restGrid(): Grid =
  for j in 0..3:
    for i in 0..3:
      result[j][i] = [float64(i) / 3.0, float64(j) / 3.0]

proc edgeArc(y, b: float64; up: bool; h: float64): Row4 =
  ## A horizontal edge at height `y` bent into a circular arc of `|b|·π` keeping its chord.
  if abs(b) < 1e-9:
    for i in 0..3: result[i] = [float64(i) / 3.0, y]
    return
  let th = abs(b) * PI
  let k = 4.0 / 3.0 * tan(th / 4.0) * 0.5 / sin(th / 2.0)
  let s = if (b > 0.0) == up: -1.0 else: 1.0
  let c = cos(th / 2.0)
  let n = sin(th / 2.0)
  let dy = s * k * n / h
  [[0.0, y], [k * c, y + dy], [1.0 - k * c, y + dy], [1.0, y]]

proc arcGrid(b, h: float64): Grid =
  ## Arc: every row is a circular arc of `|b|·π` around one centre.
  if abs(b) < 1e-9: return restGrid()
  let up = b > 0.0
  let th = abs(b) * PI
  let r = 0.5 / sin(th / 2.0)
  let k = 4.0 / 3.0 * tan(th / 4.0)
  let s = if up: -1.0 else: 1.0
  let cy = if up: h + r * cos(th / 2.0) else: -r * cos(th / 2.0)
  for j in 0..3:
    let v = float64(j) / 3.0
    let big = r + h * (if up: 1.0 - v else: v)
    proc at(a: float64): P2 = [0.5 + big * sin(a), cy + s * big * cos(a)]
    proc tangent(a: float64): P2 = [cos(a), -s * sin(a)]
    let p0 = at(-th / 2.0)
    let p3 = at(th / 2.0)
    let t0 = tangent(-th / 2.0)
    let t1 = tangent(th / 2.0)
    let l = k * big
    let pts = [p0, [p0[0] + l * t0[0], p0[1] + l * t0[1]], [p3[0] - l * t1[0], p3[1] - l * t1[1]], p3]
    for i in 0..3: result[j][i] = [pts[i][0], pts[i][1] / h]

proc blendRows(top, bottom: Row4): Grid =
  for j in 0..3:
    let t = float64(j) / 3.0
    for i in 0..3:
      result[j][i] = [top[i][0] + (bottom[i][0] - top[i][0]) * t, top[i][1] + (bottom[i][1] - top[i][1]) * t]

proc presetGrid(style: WarpStyle; b, hd0, vd0: float64; vertical: bool; aspect: float64): Grid =
  ## Photoshop's warp presets as Bezier control grids (measured from renders, clean-room).
  let h = if vertical: 1.0 / aspect else: aspect
  let (hd, vd) = if vertical: (vd0, hd0) else: (hd0, vd0)
  var g = restGrid()
  case style
  of wsArc: g = arcGrid(b, h)
  of wsArch:
    for j in 0..3: g[j] = edgeArc(float64(j) / 3.0, b, true, h)
  of wsBulge: g = blendRows(edgeArc(0.0, b, true, h), edgeArc(1.0, b, false, h))
  of wsArcLower: g = blendRows(g[0], edgeArc(1.0, b, false, h))
  of wsArcUpper: g = blendRows(edgeArc(0.0, b, true, h), g[3])
  of wsShellLower:
    let a = arcGrid(-b, h)
    g[3] = a[3]
    g[2][0] = a[2][0]
    g[2][3] = a[2][3]
  of wsShellUpper:
    let a = arcGrid(b, h)
    g[0] = a[0]
    g[1][0] = a[1][0]
    g[1][3] = a[1][3]
  of wsFlag:
    for j in 0..3:
      g[j][1][1] -= 2.0 * b
      g[j][2][1] += 2.0 * b
  of wsWave:
    for j in 1..2:
      g[j][1][1] += 4.0 / 3.0 * b
      g[j][2][1] -= 4.0 / 3.0 * b
  of wsFish:
    let fs = [-2.0, -2.0 / 3.0, 2.0 / 3.0, 2.0]
    for j in 0..3:
      g[j][1][1] += fs[j] * b
      g[j][2][1] -= fs[j] * b
  of wsRise:
    for j in 0..3:
      g[j][0][1] += 2.0 * b
      g[j][1][1] += 2.0 * b
  of wsFisheye:
    for (j, i, dx, dy) in [(1, 1, -1.0, -1.0), (1, 2, 1.0, -1.0), (2, 1, -1.0, 1.0), (2, 2, 1.0, 1.0)]:
      g[j][i][0] += dx * 2.0 / 3.0 * b
      g[j][i][1] += dy * 2.0 / 3.0 * b
  of wsInflate, wsSqueeze:
    let s = if style == wsInflate: 1.0 else: -1.0
    for i in 1..2: g[0][i][1] -= b / 3.0
    for i in 1..2: g[3][i][1] += b / 3.0
    for j in 1..2:
      g[j][0][0] -= s * b / 3.0
      g[j][3][0] += s * b / 3.0
    for (j, i, dx, dy) in [(1, 1, -1.0, -1.0), (1, 2, 1.0, -1.0), (2, 1, -1.0, 1.0), (2, 2, 1.0, 1.0)]:
      g[j][i][0] += s * dx * b / 9.0
      g[j][i][1] += dy * b / 9.0
  of wsTwist:
    let a = abs(b)
    if b >= 0.0:
      g[1][1][0] += a
      g[1][2][1] += a
      g[2][1][1] -= a
      g[2][2][0] -= a
    else:
      g[1][1][1] += a
      g[1][2][0] -= a
      g[2][1][0] += a
      g[2][2][1] -= a
  of wsNone, wsCustom: discard
  # Vertical distortion: top and bottom rows scale about their end points' midpoint.
  if vd != 0.0:
    proc moved(row: Row4; s: float64): Row4 =
      let m = [(row[0][0] + row[3][0]) / 2.0, (row[0][1] + row[3][1]) / 2.0]
      for i in 0..3:
        result[i] = [m[0] + (row[i][0] - m[0]) * s - row[i][0], m[1] + (row[i][1] - m[1]) * s - row[i][1]]
    let d0 = moved(g[0], 1.0 - vd)
    let d3 = moved(g[3], 1.0 + vd)
    for j in 0..3:
      let t = float64(j) / 3.0
      for i in 0..3:
        g[j][i][0] += d0[i][0] + (d3[i][0] - d0[i][0]) * t
        g[j][i][1] += d0[i][1] + (d3[i][1] - d0[i][1]) * t
  # Horizontal distortion: each column scales about its end points' midpoint.
  if hd != 0.0:
    for i in 0..3:
      let s = 1.0 + hd * (2.0 * float64(i) / 3.0 - 1.0)
      let m = [(g[0][i][0] + g[3][i][0]) / 2.0, (g[0][i][1] + g[3][i][1]) / 2.0]
      for j in 0..3:
        g[j][i] = [m[0] + (g[j][i][0] - m[0]) * s, m[1] + (g[j][i][1] - m[1]) * s]
  if vertical:
    var t: Grid
    for j in 0..3:
      for i in 0..3:
        t[j][i] = [g[i][j][1], g[i][j][0]]
    g = t
  g

proc bernstein(t: float64): array[4, float64] =
  let s = 1.0 - t
  [s * s * s, 3.0 * s * s * t, 3.0 * s * t * t, t * t * t]

proc lerp(a, b: P2; t: float64): P2 = [a[0] + (b[0] - a[0]) * t, a[1] + (b[1] - a[1]) * t]

proc splitCubic(p: Row4; t: float64): array[7, P2] =
  ## De Casteljau split of a cubic at `t`: 7 points (left 0..3, right 3..6).
  let a = lerp(p[0], p[1], t)
  let b = lerp(p[1], p[2], t)
  let c = lerp(p[2], p[3], t)
  let d = lerp(a, b, t)
  let e = lerp(b, c, t)
  let f = lerp(d, e, t)
  [p[0], a, d, f, e, c, p[3]]

const BernInv: array[4, array[4, float64]] = [[1.0, 0.0, 0.0, 0.0],
  [-5.0 / 6.0, 3.0, -1.5, 1.0 / 3.0], [1.0 / 3.0, -1.5, 3.0, -5.0 / 6.0], [0.0, 0.0, 0.0, 1.0]]

# ---- StyleWarp ----

proc newStyleWarp*(style: WarpStyle; bend, hDistort, vDistort: float64; vertical: bool;
                   bounds: array[4, float64]): Option[StyleWarp] =
  ## Prepares `style` with bend / distortions in percent (-100..100) for `[x0, y0, x1, y1]`.
  ## None for non-presets, an all-zero warp or an empty box.
  if not style.isPreset: return none(StyleWarp)
  let (x0, y0, x1, y1) = (bounds[0], bounds[1], bounds[2], bounds[3])
  let hw = (x1 - x0) / 2.0
  let hh = (y1 - y0) / 2.0
  if not (hw > 0.0 and hh > 0.0): return none(StyleWarp)
  proc pct(v: float64): float64 =
    if classify(v) in {fcNan, fcInf, fcNegInf}: 0.0 else: clamp(v / 100.0, -1.0, 1.0)
  let b = pct(bend)
  let hd = pct(hDistort)
  let vd = pct(vDistort)
  if b == 0.0 and hd == 0.0 and vd == 0.0: return none(StyleWarp)
  some StyleWarp(grid: presetGrid(style, b, hd, vd, vertical, hh / hw), o: (x0, y0), size: (2.0 * hw, 2.0 * hh))

proc maxSegment*(w: StyleWarp): float64 = max(min(w.size[0], w.size[1]) / 32.0, 0.5)

proc apply*(w: StyleWarp; x, y: float64): (float64, float64) =
  let u = (x - w.o[0]) / w.size[0]
  let v = (y - w.o[1]) / w.size[1]
  let bu = bernstein(u)
  let bv = bernstein(v)
  var px, py = 0.0
  for j in 0..3:
    for i in 0..3:
      px += bu[i] * bv[j] * w.grid[j][i][0]
      py += bu[i] * bv[j] * w.grid[j][i][1]
  (w.o[0] + px * w.size[0], w.o[1] + py * w.size[1])

proc mesh*(w: StyleWarp): BezierMesh =
  ## The style as one Bezier patch over its box.
  result.us = @[0.0, 1.0]
  result.vs = @[0.0, 1.0]
  for j in 0..3:
    for i in 0..3:
      result.points.add [w.o[0] + w.grid[j][i][0] * w.size[0], w.o[1] + w.grid[j][i][1] * w.size[1]]

# ---- BezierMesh ----

proc fit*(f: proc(s, t: float64): P2; us, vs: seq[float64]): BezierMesh =
  ## Fits a mesh with patch boundaries `us`/`vs` to the map `f` by interpolating it at the
  ## patch's thirds. Exact for bicubic maps (affine maps included).
  let pc = us.len - 1
  let pr = vs.len - 1
  let nx = 3 * pc + 1
  let ny = 3 * pr + 1
  var points = newSeq[P2](nx * ny)
  for pj in 0 ..< pr:
    for pi in 0 ..< pc:
      var s: array[4, array[4, P2]]
      for j in 0..3:
        for i in 0..3:
          let u = us[pi] + (us[pi + 1] - us[pi]) * float64(i) / 3.0
          let t = vs[pj] + (vs[pj + 1] - vs[pj]) * float64(j) / 3.0
          s[j][i] = f(u, t)
      for j in 0..3:
        for i in 0..3:
          var acc: P2
          for a in 0..3:
            for b in 0..3:
              let k = BernInv[j][a] * BernInv[i][b]
              acc[0] += k * s[a][b][0]
              acc[1] += k * s[a][b][1]
          points[(3 * pj + j) * nx + 3 * pi + i] = acc
  BezierMesh(us: us, vs: vs, points: points)

proc identityMesh*(bounds: array[4, float64]; cols, rows: int): BezierMesh =
  ## The identity mesh over `bounds` with `cols × rows` patches.
  let c = clamp(cols, 1, 64)
  let r = clamp(rows, 1, 64)
  var us, vs: seq[float64]
  for i in 0..c: us.add float64(i) / float64(c)
  for i in 0..r: vs.add float64(i) / float64(r)
  fit(proc(s, t: float64): P2 = [bounds[0] + s * (bounds[2] - bounds[0]), bounds[1] + t * (bounds[3] - bounds[1])], us, vs)

proc nx*(m: BezierMesh): int = 3 * (m.us.len - 1) + 1
proc ny*(m: BezierMesh): int = 3 * (m.vs.len - 1) + 1
proc point*(m: BezierMesh; i, j: int): P2 = m.points[j * m.nx + i]

proc isValid*(m: BezierMesh): bool =
  ## Checks the invariants (knots and point count); malformed data from files is rejected.
  proc knotsOk(k: seq[float64]): bool =
    if k.len < 2 or k[0] != 0.0 or k[^1] != 1.0: return false
    for i in 1 ..< k.len:
      if not (k[i] > k[i - 1]): return false
    true
  if not (knotsOk(m.us) and knotsOk(m.vs)): return false
  if m.points.len != m.nx * m.ny: return false
  for p in m.points:
    if classify(p[0]) in {fcNan, fcInf, fcNegInf} or classify(p[1]) in {fcNan, fcInf, fcNegInf}: return false
  true

proc locate(knots: seq[float64]; s0: float64): (int, float64) =
  let n = knots.len - 1
  let s = clamp(s0, 0.0, 1.0)
  var i = 0
  while i + 1 < n and s > knots[i + 1]: inc i
  let w = knots[i + 1] - knots[i]
  (i, if w > 0.0: (s - knots[i]) / w else: 0.0)

proc eval*(m: BezierMesh; s, t: float64): P2 =
  ## Evaluates the surface at normalised `(s, t)`.
  let (pi, a) = locate(m.us, s)
  let (pj, b) = locate(m.vs, t)
  let ba = bernstein(a)
  let bb = bernstein(b)
  let nx = m.nx
  for j in 0..3:
    for i in 0..3:
      let p = m.points[(3 * pj + j) * nx + 3 * pi + i]
      let k = ba[i] * bb[j]
      result[0] += p[0] * k
      result[1] += p[1] * k

proc transpose(m: var BezierMesh) =
  let nx = m.nx
  let ny = m.ny
  var pts = newSeq[P2](nx * ny)
  for j in 0 ..< ny:
    for i in 0 ..< nx:
      pts[i * ny + j] = m.points[j * nx + i]
  m.points = pts
  swap(m.us, m.vs)

proc splitU*(m: var BezierMesh; s: float64): bool =
  ## Splits the patch column containing `s` at `s`. Exact: the surface is unchanged.
  let (pi, a) = locate(m.us, s)
  if not (a >= 0.01 and a <= 0.99): return false
  let nx = m.nx
  let ny = m.ny
  var pts = newSeqOfCap[P2]((nx + 3) * ny)
  for j in 0 ..< ny:
    let base = j * nx
    for i in 0 ..< 3 * pi: pts.add m.points[base + i]
    let seg = [m.points[base + 3 * pi], m.points[base + 3 * pi + 1], m.points[base + 3 * pi + 2], m.points[base + 3 * pi + 3]]
    for p in splitCubic(seg, a): pts.add p
    for i in 3 * pi + 4 ..< nx: pts.add m.points[base + i]
  m.points = pts
  m.us.insert(clamp(s, 0.0, 1.0), pi + 1)
  true

proc splitV*(m: var BezierMesh; t: float64): bool =
  m.transpose()
  result = m.splitU(t)
  m.transpose()

proc removeSplitU*(m: var BezierMesh; k: int): bool =
  ## Removes interior vertical boundary `k` (1..us.len-2), merging its two patch columns.
  if k == 0 or k + 1 >= m.us.len: return false
  let a = (m.us[k] - m.us[k - 1]) / (m.us[k + 1] - m.us[k - 1])
  let nx = m.nx
  let ny = m.ny
  var pts = newSeqOfCap[P2]((nx - 3) * ny)
  let c0 = 3 * (k - 1)
  for j in 0 ..< ny:
    let base = j * nx
    let p0 = m.points[base + c0]
    let q1 = m.points[base + c0 + 1]
    let r2 = m.points[base + c0 + 5]
    let p3 = m.points[base + c0 + 6]
    let p1 = [(q1[0] - (1.0 - a) * p0[0]) / a, (q1[1] - (1.0 - a) * p0[1]) / a]
    let p2 = [(r2[0] - a * p3[0]) / (1.0 - a), (r2[1] - a * p3[1]) / (1.0 - a)]
    for i in 0 ..< c0: pts.add m.points[base + i]
    pts.add p0
    pts.add p1
    pts.add p2
    pts.add p3
    for i in c0 + 7 ..< nx: pts.add m.points[base + i]
  m.points = pts
  m.us.delete(k)
  true

proc removeSplitV*(m: var BezierMesh; k: int): bool =
  m.transpose()
  result = m.removeSplitU(k)
  m.transpose()

proc mapPoints*(m: BezierMesh; f: proc(p: P2): P2): BezierMesh =
  ## Applies `f` to every control point (exact for affine maps).
  result = BezierMesh(us: m.us, vs: m.vs)
  for p in m.points: result.points.add f(p)

proc paramAt*(m: BezierMesh; p: P2): (float64, float64) =
  ## Normalised `(s, t)` whose surface point is nearest `p` (coarse search, then Newton steps).
  const n = 48
  var best = (0.5, 0.5, Inf)
  for j in 0..n:
    for i in 0..n:
      let s = float64(i) / n
      let t = float64(j) / n
      let q = m.eval(s, t)
      let d = (q[0] - p[0]) ^ 2 + (q[1] - p[1]) ^ 2
      if d < best[2]: best = (s, t, d)
  var s = best[0]
  var t = best[1]
  for _ in 0 ..< 12:
    const h = 1e-5
    let q = m.eval(s, t)
    let qs = m.eval(s + h, t)
    let qt = m.eval(s, t + h)
    let a = (qs[0] - q[0]) / h
    let b = (qt[0] - q[0]) / h
    let c = (qs[1] - q[1]) / h
    let d = (qt[1] - q[1]) / h
    let det = a * d - b * c
    if abs(det) < 1e-12: break
    let ex = p[0] - q[0]
    let ey = p[1] - q[1]
    s = clamp(s + (d * ex - b * ey) / det, 0.0, 1.0)
    t = clamp(t + (a * ey - c * ex) / det, 0.0, 1.0)
  (s, t)

proc smoothLines(m: var BezierMesh; i0, i1a, j0, j1a: int) =
  ## Makes the tangent equal on both sides of every interior grid line in the given window.
  let nx = m.nx
  let i1 = min(i1a, nx)
  let j1 = min(j1a, m.ny)
  var triples: seq[(int, int, int, float64, float64)]
  for a in 1 ..< m.us.len - 1:
    if 3 * a >= i0 and 3 * a < i1:
      for j in j0 ..< j1:
        triples.add (j * nx + 3 * a - 1, j * nx + 3 * a, j * nx + 3 * a + 1, m.us[a] - m.us[a - 1], m.us[a + 1] - m.us[a])
  for b in 1 ..< m.vs.len - 1:
    if 3 * b >= j0 and 3 * b < j1:
      for i in i0 ..< i1:
        triples.add ((3 * b - 1) * nx + i, 3 * b * nx + i, (3 * b + 1) * nx + i, m.vs[b] - m.vs[b - 1], m.vs[b + 1] - m.vs[b])
  for (l, c, r, hl, hr) in triples:
    let pl = m.points[l]
    let pc = m.points[c]
    let pr = m.points[r]
    let tan = [((pc[0] - pl[0]) / hl + (pr[0] - pc[0]) / hr) / 2.0, ((pc[1] - pl[1]) / hl + (pr[1] - pc[1]) / hr) / 2.0]
    m.points[l] = [pc[0] - tan[0] * hl, pc[1] - tan[1] * hl]
    m.points[r] = [pc[0] + tan[0] * hr, pc[1] + tan[1] * hr]

proc finite2(d: P2): bool = classify(d[0]) notin {fcNan, fcInf, fcNegInf} and classify(d[1]) notin {fcNan, fcInf, fcNegInf}

proc pull*(m: var BezierMesh; base: seq[P2]; s, t: float64; d: P2) =
  ## Pulls the surface point at normalised `(s, t)` by `d` (dragging inside a patch).
  ## `base` is the control grid at the start of the drag. The surrounding grid follows with a
  ## fading share; the four inner points of the grabbed patch close the gap exactly.
  const BorderZone = 0.12
  const Reach = 1.8
  if base.len != m.points.len or not finite2(d): return
  m.points = base
  let cols = m.us.len - 1
  let rows = m.vs.len - 1
  let (pi, a) = locate(m.us, s)
  let (pj, b) = locate(m.vs, t)
  let ba = bernstein(a)
  let bb = bernstein(b)
  let nx = m.nx
  proc anchor(ai, aj: int): P2 =
    let idx = 3 * aj * nx + 3 * ai
    if idx >= 0 and idx < base.len: base[idx] else: [0.0, 0.0]
  let at0 = m.eval(s, t)
  let target = [at0[0] + d[0], at0[1] + d[1]]
  var size = 1.0
  for k in 0..1:
    let vals = [anchor(pi, pj)[k], anchor(pi + 1, pj)[k], anchor(pi, pj + 1)[k], anchor(pi + 1, pj + 1)[k]]
    var lo = Inf
    var hi = -Inf
    for v in vals:
      lo = min(lo, v)
      hi = max(hi, v)
    size = max(size, hi - lo)
  let radius = Reach * size
  let innerW = (ba[1] + ba[2]) * (bb[1] + bb[2])
  let interior = clamp(innerW / 0.5625, 0.0, 1.0)
  let toBorder = min(min(s, 1.0 - s), min(t, 1.0 - t))
  let nearBorder = clamp(1.0 - toBorder / BorderZone, 0.0, 1.0)
  proc meshCorner(ai, aj: int): bool = (ai == 0 or ai == cols) and (aj == 0 or aj == rows)
  proc reach(ai, aj: int; wa, wb: float64): float64 =
    let border = ai == 0 or ai == cols or aj == 0 or aj == rows
    if meshCorner(ai, aj): 0.0 else: wa * wb * (if border: nearBorder else: 1.0)
  let corner = [
    (pi, pj, reach(pi, pj, ba[0] + ba[1], bb[0] + bb[1])),
    (pi + 1, pj, reach(pi + 1, pj, ba[2] + ba[3], bb[0] + bb[1])),
    (pi, pj + 1, reach(pi, pj + 1, ba[0] + ba[1], bb[2] + bb[3])),
    (pi + 1, pj + 1, reach(pi + 1, pj + 1, ba[2] + ba[3], bb[2] + bb[3]))]
  var share = newSeq[float64]((cols + 1) * (rows + 1))
  for aj in 0..rows:
    for ai in 0..cols:
      let p = anchor(ai, aj)
      let x = clamp(hypot(p[0] - at0[0], p[1] - at0[1]) / radius, 0.0, 1.0)
      let fall = 0.5 * (1.0 + cos(PI * x))
      let border = ai == 0 or ai == cols or aj == 0 or aj == rows
      let edge = if meshCorner(ai, aj): 0.0 elif border: nearBorder else: 1.0
      share[aj * (cols + 1) + ai] = fall * edge
  for (ai, aj, w) in corner:
    let idx = aj * (cols + 1) + ai
    share[idx] = max(share[idx], (1.0 - interior) * w)
  for j in 0 ..< m.ny:
    for i in 0 ..< nx:
      let ai = min((i + 1) div 3, cols)
      let aj = min((j + 1) div 3, rows)
      let k = share[aj * (cols + 1) + ai]
      m.points[j * nx + i][0] += d[0] * k
      m.points[j * nx + i][1] += d[1] * k
  var norm = 0.0
  for j in [1, 2]:
    for i in [1, 2]:
      norm += (ba[i] * bb[j]) ^ 2
  var cornerNorm = 0.0
  for c in corner: cornerNorm += c[2] * c[2]
  let innerPart = if cornerNorm > 1e-12: min(innerW / 0.15, 1.0) else: 1.0
  var edge: seq[(int, float64)]
  for k in [1, 2]:
    if pi == 0: edge.add(((3 * pj + k) * nx + 3 * pi, ba[0] * bb[k]))
    if pi + 1 == cols: edge.add(((3 * pj + k) * nx + 3 * pi + 3, ba[3] * bb[k]))
    if pj == 0: edge.add((3 * pj * nx + 3 * pi + k, ba[k] * bb[0]))
    if pj + 1 == rows: edge.add(((3 * pj + 3) * nx + 3 * pi + k, ba[k] * bb[3]))
  var edgeNorm = 0.0
  for e in edge: edgeNorm += e[1] * e[1]
  let edgePart = if edgeNorm > 1e-12: nearBorder else: 0.0
  let innerShare = (1.0 - edgePart) * innerPart
  let cornerShare = (1.0 - edgePart) * (1.0 - innerPart)
  for _ in 0 ..< 60:
    let now = m.eval(s, t)
    let rem = [target[0] - now[0], target[1] - now[1]]
    if hypot(rem[0], rem[1]) < 1e-9: break
    if innerShare > 0.0 and norm > 0.0:
      for j in [1, 2]:
        for i in [1, 2]:
          let k = ba[i] * bb[j] / norm * innerShare
          let idx = (3 * pj + j) * nx + 3 * pi + i
          m.points[idx][0] += rem[0] * k
          m.points[idx][1] += rem[1] * k
    if edgePart > 0.0:
      for (idx, w) in edge:
        if idx >= 0 and idx < m.points.len:
          let k = w / edgeNorm * edgePart
          m.points[idx][0] += rem[0] * k
          m.points[idx][1] += rem[1] * k
    if cornerShare > 0.0 and cornerNorm > 0.0:
      for (ai, aj, w) in corner:
        let k = w / cornerNorm * cornerShare
        for dj in -1..1:
          for di in -1..1:
            let i = 3 * ai + di
            let j = 3 * aj + dj
            if i >= 0 and j >= 0 and i < nx and j < m.ny:
              m.points[j * nx + i][0] += rem[0] * k
              m.points[j * nx + i][1] += rem[1] * k
    for _ in 0..2:
      m.smoothLines(max(3 * pi - 3, 0), 3 * pi + 7, max(3 * pj - 3, 0), 3 * pj + 7)

proc dragPoint*(m: var BezierMesh; base: seq[P2]; index: int; d: P2) =
  ## Drags control point `index` by `d` from the grid `base` it had when the drag began,
  ## keeping the surface smooth across the grid lines.
  if base.len != m.points.len or index < 0 or index >= base.len or not finite2(d): return
  m.points = base
  let nx = m.nx
  let ny = m.ny
  let i = index mod nx
  let j = index div nx
  template shift(ii, jj: int; dd: P2) =
    let si = ii
    let sj = jj
    if si >= 0 and sj >= 0 and si < nx and sj < ny:
      m.points[sj * nx + si][0] += dd[0]
      m.points[sj * nx + si][1] += dd[1]
  proc span(knots: seq[float64]; k: int): Option[(float64, float64)] =
    if k > 0 and k + 1 < knots.len: some((knots[k] - knots[k - 1], knots[k + 1] - knots[k]))
    else: none((float64, float64))
  let ri = i mod 3
  let rj = j mod 3
  if ri == 0 and rj == 0:
    for dj in -1..1:
      for di in -1..1:
        shift(i + di, j + dj, d)
  elif ri == 0:
    let r = rj
    for di in -1..1: shift(i + di, j, d)
    let sp = span(m.vs, j div 3 + (if r == 2: 1 else: 0))
    if sp.isSome:
      let (hl, hr) = sp.get
      let (other, k) = if r == 1: (j - 2, hl / hr) else: (j + 2, hr / hl)
      for di in -1..1: shift(i + di, other, [-d[0] * k, -d[1] * k])
  elif rj == 0:
    let r = ri
    for dj in -1..1: shift(i, j + dj, d)
    let sp = span(m.us, i div 3 + (if r == 2: 1 else: 0))
    if sp.isSome:
      let (hl, hr) = sp.get
      let (other, k) = if r == 1: (i - 2, hl / hr) else: (i + 2, hr / hl)
      for dj in -1..1: shift(other, j + dj, [-d[0] * k, -d[1] * k])
  else:
    shift(i, j, d)

proc controlBounds*(m: BezierMesh): array[4, float64] =
  ## Bounding box of the control points (the surface lies inside it).
  result = [Inf, Inf, -Inf, -Inf]
  for p in m.points:
    result = [min(result[0], p[0]), min(result[1], p[1]), max(result[2], p[0]), max(result[3], p[1])]

# ---- Warp ----

proc warpNone*(bounds: array[4, float64]): Warp =
  Warp(style: wsNone, bounds: bounds)

proc warpPreset*(style: WarpStyle; bend: float64; bounds: array[4, float64]): Warp =
  Warp(style: style, bend: bend, bounds: bounds)

proc warpCustom*(mesh: BezierMesh; bounds: array[4, float64]): Warp =
  Warp(style: wsCustom, mesh: some(mesh), bounds: bounds)

proc styleWarp(w: Warp): Option[StyleWarp] =
  newStyleWarp(w.style, w.bend, w.hDistort, w.vDistort, w.vertical, w.bounds)

proc boxPoint(w: Warp; s, t: float64): P2 =
  let b = w.bounds
  [b[0] + s * (b[2] - b[0]), b[1] + t * (b[3] - b[1])]

proc isIdentity*(w: Warp): bool =
  ## Whether the warp maps every point of its box onto itself (within `1e-9` px).
  case w.style
  of wsNone: true
  of wsCustom:
    if w.mesh.isNone: return true
    let m = w.mesh.get
    let id = fit(proc(s, t: float64): P2 = w.boxPoint(s, t), m.us, m.vs)
    if id.points.len != m.points.len: return false
    for i in 0 ..< m.points.len:
      if abs(m.points[i][0] - id.points[i][0]) >= 1e-9 or abs(m.points[i][1] - id.points[i][1]) >= 1e-9:
        return false
    true
  else: w.styleWarp.isNone

proc map*(w: Warp; x, y: float64): (float64, float64) =
  ## Maps a source point (inside the box) to output space.
  case w.style
  of wsNone: (x, y)
  of wsCustom:
    if w.mesh.isSome:
      let b = w.bounds
      let ww = b[2] - b[0]
      let hh = b[3] - b[1]
      if ww <= 0.0 or hh <= 0.0: return (x, y)
      let p = w.mesh.get.eval((x - b[0]) / ww, (y - b[1]) / hh)
      (p[0], p[1])
    else: (x, y)
  else:
    let sw = w.styleWarp
    if sw.isSome: sw.get.apply(x, y) else: (x, y)

proc toMesh*(w: Warp; cols, rows: int): BezierMesh =
  ## The warp as a control mesh (a preset is its own Bezier patch split into `cols × rows`).
  if w.style == wsCustom and w.mesh.isSome: return w.mesh.get
  let c = clamp(cols, 1, 64)
  let r = clamp(rows, 1, 64)
  let sw = w.styleWarp
  if sw.isSome:
    var m = sw.get.mesh
    for i in 1 ..< c: discard m.splitU(float64(i) / float64(c))
    for j in 1 ..< r: discard m.splitV(float64(j) / float64(r))
    return m
  var us, vs: seq[float64]
  for i in 0..c: us.add float64(i) / float64(c)
  for i in 0..r: vs.add float64(i) / float64(r)
  fit(proc(s, t: float64): P2 =
    let p = w.boxPoint(s, t)
    let (x, y) = w.map(p[0], p[1])
    [x, y], us, vs)

proc thenAffine*(w: Warp; m: array[6, float64]): Warp =
  ## The same warp followed by the affine map `[a, b, c, d, e, f]`, as a custom mesh.
  let mesh = w.toMesh(4, 4).mapPoints(proc(p: P2): P2 = [m[0] * p[0] + m[2] * p[1] + m[4], m[1] * p[0] + m[3] * p[1] + m[5]])
  warpCustom(mesh, w.bounds)

proc outputBounds*(w: Warp): array[4, float64] =
  ## Approximate output bounds (sampled).
  const n = 32
  result = [Inf, Inf, -Inf, -Inf]
  for j in 0..n:
    for i in 0..n:
      let p = w.boxPoint(float64(i) / n, float64(j) / n)
      let (x, y) = w.map(p[0], p[1])
      result = [min(result[0], x), min(result[1], y), max(result[2], x), max(result[3], y)]

# ---- JSON (serde-compatible: camelCase fields, style ids) ----

proc toJson*(m: BezierMesh): JsonNode =
  var pts = newJArray()
  for p in m.points: pts.add %*[p[0], p[1]]
  %*{"us": m.us, "vs": m.vs, "points": pts}

proc meshFromJson*(n: JsonNode): Option[BezierMesh] =
  if n.isNil or n.kind != JObject: return none(BezierMesh)
  var m: BezierMesh
  for k in ["us", "vs", "points"]:
    if not n.hasKey(k) or n[k].kind != JArray: return none(BezierMesh)
  for v in n["us"]: m.us.add v.getFloat
  for v in n["vs"]: m.vs.add v.getFloat
  for p in n["points"]:
    if p.kind != JArray or p.len != 2: return none(BezierMesh)
    m.points.add [p[0].getFloat, p[1].getFloat]
  some(m)

proc toJson*(w: Warp): JsonNode =
  result = %*{"style": $w.style, "bend": w.bend, "hDistort": w.hDistort, "vDistort": w.vDistort,
              "vertical": w.vertical, "bounds": w.bounds}
  result["mesh"] = if w.mesh.isSome: w.mesh.get.toJson else: newJNull()

proc warpFromJson*(n: JsonNode): Option[Warp] =
  if n.isNil or n.kind != JObject: return none(Warp)
  let st = parseWarpStyle(n{"style"}.getStr("none"))
  if st.isNone: return none(Warp)
  var w = Warp(style: st.get)
  w.bend = n{"bend"}.getFloat(0.0)
  w.hDistort = n{"hDistort"}.getFloat(0.0)
  w.vDistort = n{"vDistort"}.getFloat(0.0)
  w.vertical = n{"vertical"}.getBool(false)
  let b = n{"bounds"}
  if b.isNil or b.kind != JArray or b.len != 4: return none(Warp)
  for i in 0..3: w.bounds[i] = b[i].getFloat
  if n.hasKey("mesh") and n["mesh"].kind == JObject:
    w.mesh = meshFromJson(n["mesh"])
  some(w)
