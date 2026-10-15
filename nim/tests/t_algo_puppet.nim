## Puppet Warp (ports of the Rust unit tests). Bit parity with Rust is checked by
## `tools/algocheck.py` (`tools/algocases/puppet.json`, see PORTING.md).

import std/[unittest, options, math, json]
import photocraft/[geom, raster, color]
import photocraft/algo/[puppet, transform]

proc blob(st: SampleType): Surface =
  ## An L shape: a vertical bar and a foot.
  result = newSurface(pixelFormat(cmRgb, st, true))
  for y in 10 ..< 70:
    for x in 20 ..< 36: result.writePixel(x, y, [float32((x + y) mod 7) / 6'f32, 0.5'f32, float32(y) / 80'f32, 1'f32])
  for y in 56 ..< 70:
    for x in 36 ..< 70: result.writePixel(x, y, [0.2'f32, float32((x * 3) mod 5) / 4'f32, 0.8'f32, 1'f32])

proc orient(a, b, c: array[2, float64]): float64 = (b[0] - a[0]) * (c[1] - a[1]) - (b[1] - a[1]) * (c[0] - a[0])
proc dist2(a, b: array[2, float64]): float64 = (a[0] - b[0]) * (a[0] - b[0]) + (a[1] - b[1]) * (a[1] - b[1])

proc inMesh(m: PuppetMesh; p: array[2, float64]): bool =
  for t in m.tris:
    let (a, b, c) = (m.verts[t[0]], m.verts[t[1]], m.verts[t[2]])
    let (d0, d1, d2) = (orient(a, b, p), orient(b, c, p), orient(c, a, p))
    if (d0 >= 0 and d1 >= 0 and d2 >= 0) or (d0 <= 0 and d1 <= 0 and d2 <= 0): return true

proc warp(pins: seq[PuppetPin]; mode = pmNormal; density = pdNormal; expansion = 2.0): PuppetWarp =
  PuppetWarp(pins: pins, mode: mode, density: density, expansion: expansion)

suite "puppet warp":
  test "the mesh covers the shape only":
    let s = blob(stU8)
    let b = s.contentBounds
    let m = buildMesh(s, b, pdNormal, 2)
    check m.tris.len > 20
    proc inside(p: array[2, float64]): bool =
      for t in m.tris:
        var (x0, x1, y0, y1) = (Inf, NegInf, Inf, NegInf)
        for i in t:
          x0 = min(x0, m.verts[i][0])
          x1 = max(x1, m.verts[i][0])
          y0 = min(y0, m.verts[i][1])
          y1 = max(y1, m.verts[i][1])
        if p[0] >= x0 and p[0] <= x1 and p[1] >= y0 and p[1] <= y1: return true
    check inside([28.0, 30.0]) and inside([60.0, 62.0]) and not inside([60.0, 20.0])
    check buildMesh(s, b, pdMore, 2).tris.len > m.tris.len and m.tris.len > buildMesh(s, b, pdFewer, 2).tris.len
  test "the mesh follows the outline":
    var s = newSurface(pixelFormat(cmRgb, stU8, true))
    for y in 40 ..< 160:
      for x in 40 ..< 160:
        if hypot(float64(x) + 0.5 - 100, float64(y) + 0.5 - 100) <= 60: s.writePixel(x, y, [0.3'f32, 0.6, 0.9, 1])
    let m = buildMesh(s, s.contentBounds, pdNormal, 2)
    var onRim = 0
    for v in m.verts:
      let r = hypot(v[0] - 100, v[1] - 100)
      check r <= 63
      if r > 61: inc onRim
    check onRim >= 40
    var area = 0.0
    for t in m.tris: area += abs(orient(m.verts[t[0]], m.verts[t[1]], m.verts[t[2]])) / 2
    check abs(area / (PI * 62 * 62) - 1) < 0.03
    let l = blob(stU8)
    let ml = buildMesh(l, l.contentBounds, pdFewer, 0)
    var allIn = true
    for x in 20 ..< 36:
      for y in 10 ..< 70: allIn = allIn and inMesh(ml, [float64(x) + 0.5, float64(y) + 0.5])
    for x in 36 ..< 70:
      for y in 56 ..< 70: allIn = allIn and inMesh(ml, [float64(x) + 0.5, float64(y) + 0.5])
    check allIn and not inMesh(ml, [40.0, 50.0]) and not inMesh(ml, [60.0, 20.0])
  test "no pins or unmoved pins are the identity":
    for st in [stU8, stU16, stF32]:
      let s = blob(st)
      let b = s.contentBounds
      for pins in [newSeq[PuppetPin](), @[PuppetPin(src: [28.0, 20.0], dst: [28.0, 20.0]), PuppetPin(src: [60.0, 62.0], dst: [60.0, 62.0])]]:
        let o = puppetWarp(s, b, warp(pins), ipBicubic).get
        let (x, y) = (s.readRegion(b), o.readRegion(b))
        var worst = 0'f32
        for i in 0 ..< x.len: worst = max(worst, abs(x[i] - y[i]))
        check worst <= 1 / 255 + 1e-6
  test "an expansion that empties the mesh warps nothing":
    let solver = newPuppetSolver(PuppetMesh(), [[10.0, 10.0]], [true])
    check solver.mesh.verts.len == 0
    let s = blob(stU8)
    let b = s.contentBounds
    let w = warp(@[PuppetPin(src: [28.0, 30.0], dst: [38.0, 25.0])], expansion = -200)
    check buildMesh(s, b, w.density, w.expansion).tris.len == 0
    check puppetWarp(s, b, w, ipBilinear).get.readRegion(b) == s.readRegion(b)
  test "a pin dragged absurdly far is refused before rendering":
    let s = blob(stU8)
    let b = s.contentBounds
    proc pin(dst: array[2, float64]): PuppetWarp =
      warp(@[PuppetPin(src: [28.0, 20.0], dst: [28.0, 20.0]), PuppetPin(src: [60.0, 62.0], dst: dst)])
    check puppetWarp(s, b, pin([1048576.0, 1048576.0]), ipBilinear).isNone
    check puppetWarp(s, b, pin([-1e7, 62.0]), ipBilinear).isNone
    check not puppetWarp(s, b, pin([70.0, 66.0]), ipBilinear).get.contentBounds.isEmpty
  test "a single pin translates rigidly; a fixed rotation turns the mesh":
    let s = blob(stU8)
    let b = s.contentBounds
    var w = warp(@[PuppetPin(src: [28.0, 30.0], dst: [38.0, 25.0])], mode = pmRigid)
    let (solver, v, _) = deform(s, b, w, Iterations)
    for i in 0 ..< v.len:
      check abs(v[i][0] - solver.mesh.verts[i][0] - 10) < 1e-3 and abs(v[i][1] - solver.mesh.verts[i][1] + 5) < 1e-3
    let o = puppetWarp(s, b, w, ipBilinear).get
    check o.contentBounds == b.translate(10, -5) and o.pixel(30, 20) == s.pixel(20, 25)
    w.pins = @[PuppetPin(src: [28.0, 30.0], dst: [28.0, 30.0], rotate: some(90.0))]
    let (solver2, v2, _) = deform(s, b, w, Iterations)
    var i = 0
    for k in 1 ..< v2.len:
      if solver2.mesh.verts[k][0] >= solver2.mesh.verts[i][0]: i = k
    let (x, d) = (solver2.mesh.verts[i], v2[i])
    let (dx, dy) = (x[0] - 28, x[1] - 30)
    check abs(d[0] - (28 - dy)) < 1 and abs(d[1] - (30 + dx)) < 1
  test "ARAP energy decreases and the modes differ":
    let s = blob(stU8)
    let mesh = buildMesh(s, s.contentBounds, pdNormal, 2)
    let src = [[28.0, 14.0], [64.0, 62.0], [28.0, 62.0]]
    let dst = [[10.0, 30.0], [70.0, 50.0], [28.0, 62.0]]
    let solver = newPuppetSolver(mesh, src, [false, false, false])
    let rot = [none(float64), none(float64), none(float64)]
    let (v, e) = solver.solve(dst, rot, pmRigid, 30)
    check e.len == 30
    let start = solver.energy(solver.solve(dst, rot, pmRigid, 0)[0], dst, rot)
    check e[0] <= start * (1 + 1e-9)
    for k in 1 ..< e.len: check e[k] <= e[k - 1] * (1 + 1e-6) + 1e-9
    check e[29] < start
    for k in 0 .. 2: check dist2(solver.pinPoint(v, k), dst[k]) < 0.25
    check solver.solve(dst, rot, pmDistort, 30)[0] != v
  test "depth orders triangles":
    let s = blob(stU8)
    let mesh = buildMesh(s, s.contentBounds, pdFewer, 2)
    let pins = [[28.0, 14.0], [64.0, 62.0]]
    let solver = newPuppetSolver(mesh, pins, [false, false])
    let order = solver.drawOrder([1'i32, 0], pins)
    proc near0(t: int): bool =
      var c = [0.0, 0.0]
      for i in solver.mesh.tris[t]: c = [c[0] + solver.mesh.verts[i][0] / 3, c[1] + solver.mesh.verts[i][1] / 3]
      dist2(c, pins[0]) < dist2(c, pins[1])
    var k = 0
    while k < order.len and not near0(order[k]): inc k
    check k < order.len
    for i in 0 ..< order.len: check near0(order[i]) == (i >= k)
  test "warps round-trip through JSON":
    let w = warp(@[PuppetPin(src: [1.0, 2.0], dst: [3.0, 4.0], rotate: some(30.0), depth: 2)], pmDistort, pdMore, 5)
    check puppetWarpFromJson(w.toJson) == w
    check puppetWarpFromJson(parseJson("{}")) == defaultPuppetWarp()
    expect ValueError: discard puppetWarpFromJson(parseJson("""{"mode": "melt"}"""))
    expect ValueError: discard puppetWarpFromJson(parseJson("""{"pins": [{"src": [1, 2]}]}"""))
