## Filter › Liquify (port of `liquify.rs`): a dense displacement field edited by brush strokes,
## then used to resample the layer once.
##
## The field stores, for every node of a regular grid (one node per `cell` document pixels), the
## offset `d` such that output pixel `p` shows the source at `p + d(p)` (an inverse map, so
## applying it never leaves holes). Strokes are plain data (`LiquifyStroke`: tool, brush and
## points), applied in order by `applyStroke`; replaying the same strokes on the same bounds gives
## the same field bit for bit, which is what makes the filter a replayable command and a smart
## filter.
##
## Every tool is written as "where does the new output at `p` come from in the current output":
## `q = M(p)` (Forward Warp: `p - kΔ`, Twirl: a rotation about the brush centre, Pucker/Bloat: a
## scaling about it), so the new field is `d'(p) = q + d(q) - p`, sampled bilinearly from the
## field as it was before the dab. Reconstruct scales `d` toward zero and Smooth relaxes it toward
## its local mean. Freeze/Thaw paint a mask that scales every tool's strength down.
##
## `applyLiquify` upsamples the field with Catmull-Rom (bicubic) per pixel and samples the source
## (premultiplied, bicubic), tile by tile; tiles where the field is zero are copied untouched.
## `ProxyImage.render` is the cheap preview path: bilinear field and bilinear source on a
## downsampled RGBA copy, re-rendered only where a dab changed things.

import std/[math, options, algorithm, json]
import ../[geom, raster, color]
import ./util

type
  LiquifyTool* = enum
    ## The Liquify tools (Photoshop's left tool strip, minus Hand/Zoom/Face).
    ltForwardWarp = "forwardWarp"
    ltReconstruct = "reconstruct"
    ltSmooth = "smooth"
    ltTwirlCw = "twirlCw"
    ltTwirlCcw = "twirlCcw"
    ltPucker = "pucker"
    ltBloat = "bloat"
    ltPushLeft = "pushLeft"
    ltFreeze = "freeze"
    ltThaw = "thaw"
    ltLassoMask = "lassoMask"
    ltReconstructAll = "reconstructAll" ## the Reconstruct… button: scales the whole (unfrozen) field toward zero
    ltFreezeAll = "freezeAll"           ## Mask Options › Mask All
    ltThawAll = "thawAll"               ## Mask None
    ltInvertFreeze = "invertFreeze"     ## Invert All

  LiquifyStroke* = object
    ## One brush stroke: the tool, its brush options and the points it passed through (`[x, y]`
    ## or `[x, y, pressure 0..1]`, document pixels).
    tool*: LiquifyTool
    size*: float64     ## brush diameter (px)
    density*: float64  ## how far the full effect reaches toward the brush edge, 0..100
    pressure*: float64 ## overall strength, 0..100
    rate*: float64     ## speed of the stationary tools, 0..100
    points*: seq[seq[float64]]
    amount*: Option[float64]
      ## `reconstructAll` only: how much to restore, 0..100. `lassoMask`: 1.0/none freezes the
      ## polygon, 0.0 thaws it.

  LiquifyField* = object
    ## The displacement field plus the freeze mask, over `bounds` (document pixels).
    bounds*: Rect
    cell*: float64             ## document pixels per field node
    w*, h*: int
    d*: seq[array[2, float32]] ## inverse displacement per node, row-major
    freeze*: seq[float32]      ## freeze mask per node (0 = free, 1 = frozen)

  ProxyImage* = object
    ## A downsampled, premultiplied RGBA copy of a layer for the interactive preview.
    bounds*: Rect  ## document region the proxy covers (the field bounds)
    scale*: float64 ## document pixels per proxy pixel
    w*, h*: int
    px*: seq[array[4, float32]] ## premultiplied RGBA, row-major

const
  AllTools* = [ltForwardWarp, ltReconstruct, ltSmooth, ltTwirlCw, ltTwirlCcw, ltPucker, ltBloat, ltPushLeft,
               ltFreeze, ltThaw, ltLassoMask]
  MaxFullResSourcePixels = 8 * 1024 * 1024

proc label*(t: LiquifyTool): string =
  case t
  of ltForwardWarp: "Forward Warp"
  of ltReconstruct: "Reconstruct"
  of ltSmooth: "Smooth"
  of ltTwirlCw: "Twirl Clockwise"
  of ltTwirlCcw: "Twirl Counterclockwise"
  of ltPucker: "Pucker"
  of ltBloat: "Bloat"
  of ltPushLeft: "Push Left"
  of ltFreeze: "Freeze Mask"
  of ltThaw: "Thaw Mask"
  of ltLassoMask: "Freeze Lasso"
  of ltReconstructAll: "Reconstruct All"
  of ltFreezeAll: "Mask All"
  of ltThawAll: "Mask None"
  of ltInvertFreeze: "Invert Mask"

proc isGlobal*(t: LiquifyTool): bool = t in {ltReconstructAll, ltFreezeAll, ltThawAll, ltInvertFreeze}
  ## Whole-field operations (no brush, no points).
proc isStationary*(t: LiquifyTool): bool = t notin {ltForwardWarp, ltPushLeft, ltLassoMask}
  ## Tools that act while the brush is held still (each recorded point is a dab).

proc isFinite(v: float64): bool {.inline.} = v == v and v - v == 0.0
proc isFinite(v: float32): bool {.inline.} = v == v and v - v == 0'f32

proc asI64(v: float64): int {.inline.} =
  ## Rust `v as isize`: saturating, NaN → 0.
  if v != v: 0
  elif v >= 9.2e18: high(int)
  elif v <= -9.2e18: low(int)
  else: int(v)

proc satAddU(a, b: int): int {.inline.} = (if a > high(int) - b: high(int) else: a + b)

proc newLiquifyStroke*(tool: LiquifyTool; size: float64): LiquifyStroke =
  LiquifyStroke(tool: tool, size: size, density: 50, pressure: 100, rate: 80)

proc validate*(s: LiquifyStroke): Option[string] =
  ## Validates the stroke (finite numbers, 2 or 3 per point): none when valid, else the error.
  if not (s.size.isFinite and s.size >= 1.0 and s.size <= 15000.0):
    return some("brush size " & $s.size & " out of range 1..15000")
  for p in s.points:
    if p.len notin 2 .. 3: return some("stroke points are [x, y] or [x, y, pressure]")
    for v in p:
      if not v.isFinite: return some("stroke points are [x, y] or [x, y, pressure]")
  none(string)

proc parseTool*(s: string): Option[LiquifyTool] =
  for t in LiquifyTool:
    if $t == s: return some(t)
  none(LiquifyTool)

proc strokeFromJson*(j: JsonNode): LiquifyStroke =
  ## A stroke from its JSON form (camelCase fields; missing fields take their defaults). Raises
  ## `ValueError` on a malformed value.
  if j.kind != JObject: raise newException(ValueError, "a Liquify stroke is a JSON object")
  result = newLiquifyStroke(ltForwardWarp, 100)
  proc num(n: JsonNode; name: string): float64 =
    case n.kind
    of JInt: float64(n.getBiggestInt)
    of JFloat: n.getFloat
    else: raise newException(ValueError, "Liquify stroke field " & name & " must be a number")
  if j.hasKey("tool"):
    let t = (if j["tool"].kind == JString: parseTool(j["tool"].getStr) else: none(LiquifyTool))
    if t.isNone: raise newException(ValueError, "unknown Liquify tool " & $j["tool"])
    result.tool = t.get
  if j.hasKey("size"): result.size = num(j["size"], "size")
  if j.hasKey("density"): result.density = num(j["density"], "density")
  if j.hasKey("pressure"): result.pressure = num(j["pressure"], "pressure")
  if j.hasKey("rate"): result.rate = num(j["rate"], "rate")
  if j.hasKey("points"):
    if j["points"].kind != JArray: raise newException(ValueError, "Liquify stroke points must be an array")
    for p in j["points"]:
      if p.kind != JArray: raise newException(ValueError, "a Liquify stroke point is an array of numbers")
      var v: seq[float64]
      for x in p: v.add num(x, "points")
      result.points.add v
  if j.hasKey("amount") and j["amount"].kind != JNull: result.amount = some(num(j["amount"], "amount"))

proc toJson*(s: LiquifyStroke): JsonNode =
  result = %*{"tool": $s.tool, "size": s.size, "density": s.density, "pressure": s.pressure, "rate": s.rate,
              "points": s.points}
  result["amount"] = (if s.amount.isSome: %s.amount.get else: newJNull())

# ---------------------------------------------------------------- field

proc autoCell*(bounds: Rect): float64 =
  ## The default field resolution for `bounds`: 2 px per node up to 4 MP, 4 px beyond.
  if bounds.size.area <= 4_200_000: 2.0 else: 4.0

proc newLiquifyField*(bounds: Rect; cell0: float64): LiquifyField =
  let cell = clamp(cell0, 1.0, 64.0)
  let w = asUsize(ceil(float64(bounds.width) / cell)) + 2
  let h = asUsize(ceil(float64(bounds.height) / cell)) + 2
  LiquifyField(bounds: bounds, cell: cell, w: w, h: h, d: newSeq[array[2, float32]](w * h), freeze: newSeq[float32](w * h))

proc nodePos*(f: LiquifyField; i, j: int): array[2, float64] {.inline.} =
  ## Document position of node (i, j) (node 0 sits on the first pixel centre).
  [float64(f.bounds.x0) + 0.5 + float64(i) * f.cell, float64(f.bounds.y0) + 0.5 + float64(j) * f.cell]

proc toGrid(f: LiquifyField; x, y: float64): (float64, float64) {.inline.} =
  ((x - float64(f.bounds.x0) - 0.5) / f.cell, (y - float64(f.bounds.y0) - 0.5) / f.cell)

proc isIdentity*(f: LiquifyField): bool =
  for v in f.d:
    if not (v[0] == 0'f32 and v[1] == 0'f32): return false
  true

proc maxDisplacement*(f: LiquifyField): float32 =
  ## Largest displacement magnitude (px).
  result = 0'f32
  for v in f.d: result = rmax(result, hypot(v[0], v[1]))

proc bilinear(d: openArray[array[2, float32]]; w, h: int; gx0, gy0: float64): array[2, float64] =
  if not (gx0 > -1.0 and gy0 > -1.0 and gx0 < float64(w) and gy0 < float64(h)): return [0.0, 0.0]
  let gx = clamp(gx0, 0.0, float64(w - 1))
  let gy = clamp(gy0, 0.0, float64(h - 1))
  let (i, j) = (min(int(gx), max(w - 2, 0)), min(int(gy), max(h - 2, 0)))
  let (fx, fy) = (gx - float64(i), gy - float64(j))
  let i1 = min(i + 1, w - 1)
  let j1 = min(j + 1, h - 1)
  let (a, b, c, e) = (d[j * w + i], d[j * w + i1], d[j1 * w + i], d[j1 * w + i1])
  for k in 0 .. 1:
    result[k] = (float64(a[k]) * (1.0 - fx) + float64(b[k]) * fx) * (1.0 - fy) +
                (float64(c[k]) * (1.0 - fx) + float64(e[k]) * fx) * fy

proc sample*(f: LiquifyField; x, y: float64): array[2, float64] =
  ## Bilinear displacement at a document point (zero outside the field).
  let (gx, gy) = f.toGrid(x, y)
  bilinear(f.d, f.w, f.h, gx, gy)

proc freezeAt*(f: LiquifyField; x, y: float64): float32 =
  ## Freeze value at a document point (bilinear).
  let (gx0, gy0) = f.toGrid(x, y)
  let gx = clamp(gx0, 0.0, float64(f.w - 1))
  let gy = clamp(gy0, 0.0, float64(f.h - 1))
  let (i, j) = (min(asUsize(gx), f.w - 2), min(asUsize(gy), f.h - 2))
  let (fx, fy) = (float32(gx - float64(i)), float32(gy - float64(j)))
  template at(ii, jj: int): float32 = f.freeze[jj * f.w + ii]
  (at(i, j) * (1'f32 - fx) + at(i + 1, j) * fx) * (1'f32 - fy) + (at(i, j + 1) * (1'f32 - fx) + at(i + 1, j + 1) * fx) * fy

proc falloff*(t20, density0: float64): float64 =
  ## A dab's strength at squared distance `t2` (in brush radii, 0..1) for Density `density`
  ## (0..1): 1 at the centre, down to 0 at the rim with a flat slope at every density.
  let t2 = clamp(t20, 0.0, 1.0)
  let density = (if density0 != density0: 0.5 else: clamp(density0, 0.0, 1.0))
  # Feathered: (1 - t²)³. Dense: (1 - t⁴)², flat in the middle.
  let feathered = powi(1.0 - t2, 3)
  let dense = powi(1.0 - t2 * t2, 2)
  feathered + (dense - feathered) * density

proc catmullRom(t: float64): array[4, float64] =
  let t2 = t * t
  let t3 = t2 * t
  [-0.5 * t3 + t2 - 0.5 * t, 1.5 * t3 - 2.5 * t2 + 1.0, -1.5 * t3 + 2.0 * t2 + 0.5 * t, 0.5 * t3 - 0.5 * t2]

proc applyLasso(f: var LiquifyField; s: LiquifyStroke): Rect =
  ## Lasso: sets the freeze mask to `amount` (1.0 when none; 0.0 thaws) at every field node inside
  ## the closed polygon in `s.points` (even-odd rule; non-finite points are skipped). Returns the
  ## document rectangle the mask changed in.
  var poly: seq[array[2, float64]]
  for p in s.points:
    if p.len >= 2 and p[0].isFinite and p[1].isFinite: poly.add [p[0], p[1]]
  if poly.len < 3 or f.w == 0 or f.h == 0: return EmptyRect
  var (px0, py0, px1, py1) = (Inf, Inf, NegInf, NegInf)
  for p in poly:
    px0 = min(px0, p[0])
    py0 = min(py0, p[1])
    px1 = max(px1, p[0])
    py1 = max(py1, p[1])
  let (_, gy0) = f.toGrid(px0, py0)
  let (_, gy1) = f.toGrid(px1, py1)
  let lastRow = float64(max(f.h - 1, 0))
  let lastCol = float64(max(f.w - 1, 0))
  let (j0, j1) = (rmax(ceil(gy0), 0.0), rmin(floor(gy1), lastRow))
  if j1 < j0: return EmptyRect
  let v = (if s.amount.get(1.0) > 0.5: 1'f32 else: 0'f32)
  var xs: seq[float64]
  for j in asUsize(j0) .. asUsize(j1):
    let y = f.nodePos(0, j)[1]
    xs.setLen(0)
    var prev = poly[^1]
    for a in poly:
      if (a[1] > y) != (prev[1] > y):
        xs.add a[0] + (y - a[1]) * (prev[0] - a[0]) / (prev[1] - a[1])
      prev = a
    xs.sort(proc(a, b: float64): int = totalCmp(a, b))
    # Even-odd: nodes with xs[2k] <= x < xs[2k + 1] are inside.
    var k = 0
    while k + 1 < xs.len:
      let (ga, _) = f.toGrid(xs[k], y)
      let (gb, _) = f.toGrid(xs[k + 1], y)
      k += 2
      let (i0, i1) = (rmax(ceil(ga), 0.0), rmin(ceil(gb) - 1.0, lastCol))
      if i1 < i0: continue
      let row = j * f.w
      let (a, b) = (row + asUsize(i0), row + asUsize(i1))
      if b < f.freeze.len:
        for q in a .. b: f.freeze[q] = v
  rect(asI32(floor(px0)), asI32(floor(py0)), asI32(ceil(px1)), asI32(ceil(py1))).intersect(f.bounds)

proc reachableSteps(f: LiquifyField; a, b: array[3, float64]; steps: int; r: float64; stationary: bool): Option[(int, int)] =
  ## The steps `k0..=k1` of a segment walked in `steps` whose dab can touch a field node, or none
  ## when none can.
  if f.w == 0 or f.h == 0: return none((int, int))
  let pad = r + 2.0 * f.cell
  let lo = [float64(f.bounds.x0) + 0.5 - pad, float64(f.bounds.y0) + 0.5 - pad]
  let hi = [lo[0] + float64(f.w - 1) * f.cell + 2.0 * pad, lo[1] + float64(f.h - 1) * f.cell + 2.0 * pad]
  let (n, o) = (float64(steps), (if stationary: 0.0 else: 1.0))
  var (k0, k1) = (1.0, n)
  for ax in 0 .. 1:
    let d = b[ax] - a[ax]
    if d == 0.0:
      if a[ax] != a[ax] or a[ax] < lo[ax] or a[ax] > hi[ax]: return none((int, int))
      continue
    let (u0, u1) = ((lo[ax] - a[ax]) / d, (hi[ax] - a[ax]) / d)
    k0 = rmax(k0, floor(rmin(u0, u1) * n + o) - 1.0)
    k1 = rmin(k1, ceil(rmax(u0, u1) * n + o) + 1.0)
  # NaN: a segment whose length overflowed.
  if k0 != k0 or k1 != k1 or k0 > k1: return none((int, int))
  some((asUsize(k0), asUsize(k1)))

proc dab(f: var LiquifyField; s: LiquifyStroke; c, delta: array[2, float64]; pointPressure: float64): Rect =
  ## One dab at `c` with brush motion `delta`. Returns the document rect it affected.
  if s.tool == ltLassoMask: return EmptyRect # the lasso is polygon-only; never a brush dab
  let r = rmax(s.size / 2.0, 0.5)
  let strength = clamp(s.pressure / 100.0, 0.0, 1.0) * pointPressure
  let rate = clamp(s.rate / 100.0, 0.0, 1.0)
  let density = clamp(s.density / 100.0, 0.0, 1.0)
  if strength <= 0.0: return EmptyRect
  let (gx0, gy0) = f.toGrid(c[0] - r, c[1] - r)
  let (gx1, gy1) = f.toGrid(c[0] + r, c[1] + r)
  let i0 = asUsize(rmax(floor(gx0), 0.0))
  let j0 = asUsize(rmax(floor(gy0), 0.0))
  let i1 = min(asI64(rmax(ceil(gx1), -1.0)), f.w - 1)
  let j1 = min(asI64(rmax(ceil(gy1), -1.0)), f.h - 1)
  if i1 < i0 or j1 < j0: return EmptyRect
  # Snapshot of the affected nodes plus a margin (sampling reads around them).
  let m = satAddU(asUsize(ceil((rmax(abs(delta[0]), abs(delta[1])) + r * 0.2) / f.cell)), 2)
  let (si0, sj0) = (max(i0 - m, 0), max(j0 - m, 0))
  let (si1, sj1) = (min(satAddU(i1, m), f.w - 1), min(satAddU(j1, m), f.h - 1))
  let sw = si1 - si0 + 1
  let sh = sj1 - sj0 + 1
  var snap = newSeqOfCap[array[2, float32]](sw * sh)
  for j in sj0 .. sj1:
    for i in si0 .. si1: snap.add f.d[j * f.w + i]
  var writes: seq[(int, array[2, float32], float32)]
  let invCell = 1.0 / f.cell
  for j in j0 .. j1:
    for i in i0 .. i1:
      let p = f.nodePos(i, j)
      let (dx, dy) = (p[0] - c[0], p[1] - c[1])
      let t2 = (dx * dx + dy * dy) / (r * r)
      if t2 >= 1.0: continue
      let fall = falloff(t2, density)
      let idx = j * f.w + i
      let kRaw = strength * fall
      if s.tool == ltFreeze or s.tool == ltThaw:
        let fr = f.freeze[idx]
        let v = (if s.tool == ltFreeze: rmin(float64(fr) + kRaw, 1.0) else: rmax(float64(fr) - kRaw, 0.0))
        writes.add (idx, f.d[idx], float32(v))
        continue
      let k = kRaw * (1.0 - float64(f.freeze[idx]))
      if k <= 0.0: continue
      let cur = f.d[idx]
      var nv: array[2, float32]
      case s.tool
      of ltReconstruct:
        let a = float32(1.0 - k * (0.1 + 0.4 * rate))
        nv = [cur[0] * a, cur[1] * a]
      of ltSmooth:
        let (gx, gy) = (i - si0, j - sj0)
        template at(x, y: int): array[2, float32] = snap[clamp(y, 0, sh - 1) * sw + clamp(x, 0, sw - 1)]
        var mean = [0'f32, 0'f32]
        for (ox, oy) in [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (1, -1), (-1, 1), (1, 1)]:
          let v = at(gx + ox, gy + oy)
          mean[0] += v[0] / 8'f32
          mean[1] += v[1] / 8'f32
        let a = float32(k * (0.1 + 0.4 * rate))
        nv = [cur[0] + (mean[0] - cur[0]) * a, cur[1] + (mean[1] - cur[1]) * a]
      else:
        var q: array[2, float64]
        case s.tool
        of ltForwardWarp: q = [p[0] - k * delta[0], p[1] - k * delta[1]]
        # Left of the stroke direction (y down): (dx, dy) → (dy, -dx).
        of ltPushLeft: q = [p[0] - k * delta[1], p[1] + k * delta[0]]
        of ltTwirlCw, ltTwirlCcw:
          let th = k * rate * 0.12 * (if s.tool == ltTwirlCw: 1.0 else: -1.0)
          # Content turns by +th, so sample at the point turned by -th.
          let (sn, cs) = (sin(-th), cos(-th))
          q = [c[0] + dx * cs - dy * sn, c[1] + dx * sn + dy * cs]
        of ltPucker:
          let a = 1.0 + k * rate * 0.06
          q = [c[0] + dx * a, c[1] + dy * a]
        else:
          let a = 1.0 - k * rate * 0.06
          q = [c[0] + dx * a, c[1] + dy * a]
        let (qx, qy) = ((q[0] - p[0]) * invCell + float64(i), (q[1] - p[1]) * invCell + float64(j))
        let (lx, ly) = (qx - float64(si0), qy - float64(sj0))
        let dq = (if lx >= 0.0 and ly >= 0.0 and lx <= float64(sw - 1) and ly <= float64(sh - 1): bilinear(snap, sw, sh, lx, ly)
                  else: bilinear(f.d, f.w, f.h, qx, qy)) # outside the snapshot nothing changed: the live field
        nv = [float32(q[0] + dq[0] - p[0]), float32(q[1] + dq[1] - p[1])]
      writes.add (idx, nv, f.freeze[idx])
  for (idx, d, fr) in writes:
    f.d[idx] = d
    f.freeze[idx] = fr
  let pad = asI32(ceil(r)) + 2
  rect(satSub(asI32(floor(c[0])), pad), satSub(asI32(floor(c[1])), pad), satAdd(asI32(ceil(c[0])), pad),
       satAdd(asI32(ceil(c[1])), pad)).intersect(f.bounds)

proc strokeBegin*(f: var LiquifyField; s: LiquifyStroke; p: array[3, float64]): Rect =
  ## Starts a stroke at `p` (`[x, y, pressure]`): stationary tools dab once there.
  if s.tool.isStationary and not s.tool.isGlobal: f.dab(s, [p[0], p[1]], [0.0, 0.0], clamp(p[2], 0.0, 1.0))
  else: EmptyRect

proc strokeSegment*(f: var LiquifyField; s: LiquifyStroke; a, b: array[3, float64]): Rect =
  ## Continues a stroke from `a` to `b`: dabs every fifth of the brush radius (a stationary tool
  ## held still dabs once at `b`).
  result = EmptyRect
  let (pa, pb) = (clamp(a[2], 0.0, 1.0), clamp(b[2], 0.0, 1.0))
  let len = hypot(b[0] - a[0], b[1] - a[1])
  if len == 0.0:
    return (if s.tool.isStationary: f.dab(s, [b[0], b[1]], [0.0, 0.0], pb) else: result)
  let r = rmax(s.size / 2.0, 0.5)
  let spacing = rmax(r * 0.2, 0.5)
  let steps = asUsize(rmax(ceil(len / spacing), 1.0))
  # Walk only the steps whose dab can reach a field node.
  let reach = f.reachableSteps(a, b, steps, r, s.tool.isStationary)
  if reach.isNone: return
  let (k0, k1) = reach.get
  for k in k0 .. k1:
    let t0 = float64(k - 1) / float64(steps)
    let t1 = float64(k) / float64(steps)
    let c = [a[0] + (b[0] - a[0]) * t1, a[1] + (b[1] - a[1]) * t1]
    let delta = [(b[0] - a[0]) * (t1 - t0), (b[1] - a[1]) * (t1 - t0)]
    let pr = pa + (pb - pa) * t1
    # Forward Warp centres the dab where the brush was (it drags what is under it).
    let centre = (if s.tool.isStationary: c else: [c[0] - delta[0], c[1] - delta[1]])
    result = result.union(f.dab(s, centre, delta, pr))

proc applyStroke*(f: var LiquifyField; s: LiquifyStroke): Rect =
  ## Applies one stroke. Returns the document rectangle whose output changed.
  case s.tool
  of ltLassoMask: return f.applyLasso(s)
  of ltReconstructAll:
    let k = float32(clamp(s.amount.get(100.0) / 100.0, 0.0, 1.0))
    for i in 0 ..< f.d.len:
      let m = 1'f32 - k * (1'f32 - f.freeze[i])
      f.d[i][0] *= m
      f.d[i][1] *= m
    return f.bounds
  of ltFreezeAll:
    for v in f.freeze.mitems: v = 1
  of ltThawAll:
    for v in f.freeze.mitems: v = 0
  of ltInvertFreeze:
    for v in f.freeze.mitems: v = 1'f32 - v
  else: discard
  if s.tool.isGlobal: return f.bounds
  var pts: seq[array[3, float64]]
  for p in s.points:
    if p.len >= 2: pts.add [p[0], p[1], (if p.len > 2: p[2] else: 1.0)]
  if pts.len == 0: return EmptyRect
  result = f.strokeBegin(s, pts[0])
  for k in 1 ..< pts.len: result = result.union(f.strokeSegment(s, pts[k - 1], pts[k]))

proc fromStrokes*(bounds: Rect; cell: float64; strokes: openArray[LiquifyStroke]): LiquifyField =
  ## Replays strokes on a fresh field.
  result = newLiquifyField(bounds, cell)
  for s in strokes: discard result.applyStroke(s)

proc sampleCubic(f: LiquifyField; gx, gy: float64; wx, wy: array[4, float64]): array[2, float64] =
  ## Catmull-Rom (bicubic) displacement at grid coordinates.
  let (ix, iy) = (asI64(floor(gx)), asI64(floor(gy)))
  for jj in 0 .. 3:
    let y = clamp(iy - 1 + jj, 0, f.h - 1)
    for ii in 0 .. 3:
      let x = clamp(ix - 1 + ii, 0, f.w - 1)
      let k = wx[ii] * wy[jj]
      let v = f.d[y * f.w + x]
      result[0] += float64(v[0]) * k
      result[1] += float64(v[1]) * k

proc touches(f: LiquifyField; r: Rect): bool =
  ## Whether any node influencing document rect `r` is displaced.
  let (gx0, gy0) = f.toGrid(float64(r.x0), float64(r.y0))
  let (gx1, gy1) = f.toGrid(float64(r.x1), float64(r.y1))
  let i0 = clamp(asI64(floor(gx0)) - 2, 0, f.w - 1)
  let j0 = clamp(asI64(floor(gy0)) - 2, 0, f.h - 1)
  let i1 = clamp(asI64(ceil(gx1)) + 2, 0, f.w - 1)
  let j1 = clamp(asI64(ceil(gy1)) + 2, 0, f.h - 1)
  for j in j0 .. j1:
    for i in i0 .. i1:
      let v = f.d[j * f.w + i]
      if v[0] != 0'f32 or v[1] != 0'f32: return true
  false

proc applyLiquify*(src: Surface; field: LiquifyField): Surface =
  ## Resamples `src` through the field: inside `field.bounds` each pixel shows the source at
  ## `p + d(p)` (bicubic field, bicubic premultiplied source, edge pixels repeat past the field
  ## bounds); everything outside the bounds is kept. Same format as `src`.
  let fmt = src.format
  let n = fmt.channels
  let alpha = fmt.alpha
  let b = field.bounds
  result = src
  if field.isIdentity or b.isEmpty or field.d.len < field.w * field.h or field.w < 1 or field.h < 1: return
  let maxd = asI32(ceil(float64(field.maxDisplacement))) + 3
  for tc in b.tiles:
    let t = tc.rect.intersect(b)
    if t.isEmpty or not field.touches(t): continue
    let w = t.width
    let h = t.height
    # Field per pixel (bicubic), rows share their y weights.
    var disp = newSeq[array[2, float64]](w * h)
    var (fx0, fy0, fx1, fy1) = (F64Max, F64Max, -F64Max, -F64Max)
    var moved = false
    for y in 0 ..< h:
      let py = float64(t.y0) + float64(y) + 0.5
      let gy = (py - float64(b.y0) - 0.5) / field.cell
      let wy = catmullRom(gy - floor(gy))
      for x in 0 ..< w:
        let px = float64(t.x0) + float64(x) + 0.5
        let gx = (px - float64(b.x0) - 0.5) / field.cell
        let wx = catmullRom(gx - floor(gx))
        let d = field.sampleCubic(gx, gy, wx, wy)
        if d[0] != 0.0 or d[1] != 0.0: moved = true
        disp[y * w + x] = d
        let (sx, sy) = (px + d[0], py + d[1])
        fx0 = rmin(fx0, sx)
        fy0 = rmin(fy0, sy)
        fx1 = rmax(fx1, sx)
        fy1 = rmax(fy1, sy)
    if not moved: continue
    let foot = rect(satSub(asI32(floor(fx0)), 3), satSub(asI32(floor(fy0)), 3), satAdd(asI32(ceil(fx1)), 3),
                    satAdd(asI32(ceil(fy1)), 3)).intersect(t.inflate(maxd)).intersect(b)
    if foot.isEmpty: continue
    var px = src.readRegion(foot)
    if alpha:
      for k in 0 ..< px.len div n:
        let al = px[k * n + n - 1]
        for c in 0 ..< n - 1: px[k * n + c] *= al
    let fw = foot.width
    template at(xq, yq: int): int =
      ((clamp(yq, foot.y0, foot.y1 - 1) - foot.y0) * fw + (clamp(xq, foot.x0, foot.x1 - 1) - foot.x0)) * n
    proc snapT(v: float64): float64 = (if abs(v - round(v)) < 1e-7: round(v) else: v)
    var outp = newSeq[float32](w * h * n)
    var acc: array[8, float64]
    for y in 0 ..< h:
      for x in 0 ..< w:
        let d = disp[y * w + x]
        let u = snapT(float64(t.x0) + float64(x) + d[0])
        let v = snapT(float64(t.y0) + float64(y) + d[1])
        let (ix, iy) = (asI32(floor(u)), asI32(floor(v)))
        let (wx, wy) = (catmullRom(u - float64(ix)), catmullRom(v - float64(iy)))
        for c in 0 ..< n: acc[c] = 0
        for j in 0 .. 3:
          for i in 0 .. 3:
            let k = wx[i] * wy[j]
            if k == 0.0: continue
            let o = at(ix - 1 + i, iy - 1 + j)
            for c in 0 ..< min(n, 8): acc[c] += float64(px[o + c]) * k
        let o = (y * w + x) * n
        if alpha:
          let al = clamp(acc[n - 1], 0.0, 1.0)
          outp[o + n - 1] = float32(al)
          for c in 0 ..< n - 1: outp[o + c] = (if al > 0.0: float32(clamp(acc[c] / al, 0.0, 1.0)) else: 0'f32)
        else:
          for c in 0 ..< n: outp[o + c] = float32(clamp(acc[c], 0.0, 1.0))
    result.writeRegion(t, outp)
  if alpha: result.prune()

# ---------------------------------------------------------------- preview proxy

proc newProxyImage*(src: Surface; bounds: Rect; maxSide: int): ProxyImage =
  ## Box-downsamples `src` over `bounds` so the longer side is at most `maxSide`.
  let side = float64(max(max(bounds.width, bounds.height), 1))
  let scale = ceil(rmax(side / float64(max(maxSide, 1)), 1.0))
  let w = max(asUsize(ceil(float64(bounds.width) / scale)), 1)
  let h = max(asUsize(ceil(float64(bounds.height) / scale)), 1)
  let k = asI32(scale)
  var px = newSeq[array[4, float32]](w * h)
  let fmt = src.format
  for y in 0 ..< h:
    let r = rect(bounds.x0, bounds.y0 + y * k, bounds.x0 + w * k, min(bounds.y0 + (y + 1) * k, bounds.y1))
    var buf = newSeq[Rgba](r.width * r.height)
    src.readRgbaInto(r, buf)
    let rw = r.width
    for x in 0 ..< w:
      var acc = [0'f32, 0, 0, 0]
      var cnt = 0'f32
      for yy in 0 ..< r.height:
        for xx in x * k ..< min((x + 1) * k, rw):
          let p = buf[yy * rw + xx]
          let a = (if fmt.alpha: p[3] else: 1'f32)
          acc[0] += p[0] * a
          acc[1] += p[1] * a
          acc[2] += p[2] * a
          acc[3] += a
          cnt += 1
      if cnt > 0: px[y * w + x] = [acc[0] / cnt, acc[1] / cnt, acc[2] / cnt, acc[3] / cnt]
  ProxyImage(bounds: bounds, scale: scale, w: w, h: h, px: px)

proc toU8(v: float32): uint8 {.inline.} = asU8(clamp(v, 0'f32, 1'f32) * 255'f32 + 0.5'f32)

proc render*(p: ProxyImage; field: LiquifyField; r: array[4, int]; outp: var openArray[array[4, uint8]]) =
  ## Renders proxy pixels `r` (proxy coordinates) through the field into `outp` (premultiplied
  ## RGBA8, `w × h`): bilinear field, bilinear source.
  let (x0, y0, x1, y1) = (min(r[0], p.w), min(r[1], p.h), min(r[2], p.w), min(r[3], p.h))
  if x0 >= x1 or y0 >= y1 or outp.len < y1 * p.w or p.px.len < p.w * p.h: return
  let s = p.scale
  let (bx, by) = (float64(p.bounds.x0), float64(p.bounds.y0))
  for y in y0 ..< y1:
    let py = by + (float64(y) + 0.5) * s
    for x in x0 ..< x1:
      let px = bx + (float64(x) + 0.5) * s
      let d = field.sample(px, py)
      let (u, v) = ((px + d[0] - bx) / s - 0.5, (py + d[1] - by) / s - 0.5)
      let (iu, iv) = (floor(u), floor(v))
      let (fu, fv) = (float32(u - iu), float32(v - iv))
      template at(xq, yq: float64): array[4, float32] =
        p.px[asUsize(clamp(yq, 0.0, float64(p.h - 1))) * p.w + asUsize(clamp(xq, 0.0, float64(p.w - 1)))]
      let (a, b, c, e) = (at(iu, iv), at(iu + 1.0, iv), at(iu, iv + 1.0), at(iu + 1.0, iv + 1.0))
      var o: array[4, uint8]
      for k in 0 .. 3:
        let v = (a[k] * (1'f32 - fu) + b[k] * fu) * (1'f32 - fv) + (c[k] * (1'f32 - fu) + e[k] * fu) * fv
        o[k] = toU8(v)
      outp[y * p.w + x] = o

proc renderFullResolution*(src: Surface; field: LiquifyField; bounds0, region0: Rect; outp: var openArray[array[4, uint8]];
                           sourceScratch: var seq[array[4, float32]]): bool =
  ## Renders a document-pixel-resolution preview for `region` into the corresponding part of
  ## `outp`, whose dimensions cover `bounds`. Source reads are limited to the dirty region plus the
  ## displacement range that can affect it. `sourceScratch` is reused by the caller between
  ## updates.
  let bounds = bounds0.intersect(field.bounds)
  let region = region0.intersect(bounds)
  let (width, height) = (bounds.width, bounds.height)
  if region.isEmpty or outp.len != width * height or width == 0 or height == 0: return false
  # Bilinear field interpolation is a convex combination of its surrounding nodes. The extrema of
  # those nodes therefore bound every source sample in this output rectangle.
  var (minDx, minDy, maxDx, maxDy) = (Inf.float32, Inf.float32, NegInf.float32, NegInf.float32)
  let (gx0, gy0) = field.toGrid(float64(region.x0) + 0.5, float64(region.y0) + 0.5)
  let (gx1, gy1) = field.toGrid(float64(region.x1) - 0.5, float64(region.y1) - 0.5)
  let (i0, j0) = (asUsize(rmax(floor(gx0), 0.0)), asUsize(rmax(floor(gy0), 0.0)))
  let (i1, j1) = (asUsize(rmax(ceil(gx1), 0.0)), asUsize(rmax(ceil(gy1), 0.0)))
  for j in j0 .. min(j1, max(field.h - 1, 0)):
    for i in i0 .. min(i1, max(field.w - 1, 0)):
      let idx = j * field.w + i
      if idx >= field.d.len: continue
      let v = field.d[idx]
      minDx = rmin(minDx, v[0])
      minDy = rmin(minDy, v[1])
      maxDx = rmax(maxDx, v[0])
      maxDy = rmax(maxDy, v[1])
  if not (minDx.isFinite and minDy.isFinite and maxDx.isFinite and maxDy.isFinite): return false
  # Source coordinates are pixel centres. Add one sample on each side for bilinear interpolation,
  # then clamp to the document just as the regular proxy renderer does.
  let x0 = floor(float64(region.x0) + float64(minDx)) - 1.0
  let y0 = floor(float64(region.y0) + float64(minDy)) - 1.0
  let x1 = ceil(float64(region.x1 - 1) + float64(maxDx)) + 2.0
  let y1 = ceil(float64(region.y1 - 1) + float64(maxDy)) + 2.0
  let canvas = field.bounds
  proc cl(v: float64; lo, hi: int): int = asI32(min(max(v, float64(lo)), float64(hi)))
  let srcRect = rect(cl(x0, canvas.x0, canvas.x1), cl(y0, canvas.y0, canvas.y1), cl(x1, canvas.x0, canvas.x1),
                     cl(y1, canvas.y0, canvas.y1))
  if srcRect.isEmpty: return false
  let (sw, sh) = (srcRect.width, srcRect.height)
  let sourceLen = sw * sh
  if sourceLen > MaxFullResSourcePixels: return false
  sourceScratch.setLen(sourceLen)
  src.readRgbaInto(srcRect, sourceScratch)
  if not src.format.alpha:
    for p in sourceScratch.mitems: p[3] = 1
  for p in sourceScratch.mitems:
    let a = p[3]
    p[0] *= a
    p[1] *= a
    p[2] *= a
  let ox0 = region.x0 - bounds.x0
  let ox1 = region.x1 - bounds.x0
  let oy0 = region.y0 - bounds.y0
  let oy1 = region.y1 - bounds.y0
  for row in oy0 ..< oy1:
    let y = bounds.y0 + row
    for col in ox0 ..< ox1:
      let x = bounds.x0 + col
      let p = [float64(x) + 0.5, float64(y) + 0.5]
      let d = field.sample(p[0], p[1])
      let fx = clamp(p[0] + d[0] - 0.5, float64(canvas.x0), float64(canvas.x1 - 1))
      let fy = clamp(p[1] + d[1] - 0.5, float64(canvas.y0), float64(canvas.y1 - 1))
      let (ix, iy) = (floor(fx), floor(fy))
      let (tx, ty) = (float32(fx - ix), float32(fy - iy))
      let sx0 = clamp(asI32(ix), srcRect.x0, srcRect.x1 - 1)
      let sy0 = clamp(asI32(iy), srcRect.y0, srcRect.y1 - 1)
      let sx1 = min(sx0 + 1, srcRect.x1 - 1)
      let sy1 = min(sy0 + 1, srcRect.y1 - 1)
      template at(sx, sy: int): array[4, float32] =
        sourceScratch[(sy - srcRect.y0) * sw + (sx - srcRect.x0)]
      let (a, b, c, e) = (at(sx0, sy0), at(sx1, sy0), at(sx0, sy1), at(sx1, sy1))
      var o: array[4, uint8]
      for k in 0 .. 3:
        o[k] = toU8((a[k] * (1'f32 - tx) + b[k] * tx) * (1'f32 - ty) + (c[k] * (1'f32 - tx) + e[k] * tx) * ty)
      outp[row * width + col] = o
  true

proc proxyRect*(p: ProxyImage; r: Rect): array[4, int] =
  ## The proxy pixel rectangle covering document rect `r`.
  let s = p.scale
  proc f(v, o: int): float64 = rmax(float64(v - o) / s, 0.0)
  [asUsize(floor(f(r.x0, p.bounds.x0))), asUsize(floor(f(r.y0, p.bounds.y0))),
   min(asUsize(ceil(f(r.x1, p.bounds.x0))) + 1, p.w), min(asUsize(ceil(f(r.y1, p.bounds.y0))) + 1, p.h)]
