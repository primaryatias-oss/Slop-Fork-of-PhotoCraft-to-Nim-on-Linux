## Selection algorithms on coverage masks (`w × h` row-major `f32` in `0..=1` over an area):
## magic wand / flood regions, colour range, modify (expand, contract, border, smooth, feather),
## anti-aliased lasso, and combining with an existing selection (port of `selection.rs`).

import std/[math, options, algorithm, tables]
import ../geom, ../color, ../raster, ../rmath
import ./util, ./selection_distance, ./selection_blur, ./selection_marquee

export edt, ellipseSurface, combineSurfaces

type
  SelectionMode* = enum
    ## How a new selection combines with the current one.
    smReplace = "replace", smAdd = "add", smSubtract = "subtract", smIntersect = "intersect"

  Region* = object
    ## A selected region: 8-bit coverage over `bbox` (row-major), the bounding box of the pixels
    ## touched (including the anti-aliased fringe).
    bbox*: Rect
    mask*: seq[uint8]

  RangeSample* = object
    ## One Color Range sample (Sampled Colors): an eyedropper colour, and where it was picked
    ## (needed for Localized Color Clusters; area-local pixel coordinates).
    color*: array[3, float32]
    at*: Option[(float32, float32)]

  LabRange* = object
    ## The colours of Color Range › Sampled Colors: the Lab box spanned by the eyedropper samples.
    ## One sample is a box of size zero. CIE Lab (D50): L 0–100, a and b about −128–127.
    lo*, hi*: array[3, float32]

proc parseSelectionMode*(s: string): SelectionMode =
  ## Parses `replace|add|subtract|intersect` (unknown → replace).
  case s
  of "add": smAdd
  of "subtract": smSubtract
  of "intersect": smIntersect
  else: smReplace

proc finite(v: float32): bool {.inline.} = v.classify notin {fcNan, fcInf, fcNegInf}

proc maskFromSurface*(s: Option[Surface]; area: Rect): seq[float32] =
  ## Reads a selection surface over `area` (no selection → all zero).
  let n = area.width * area.height
  if s.isNone: return newSeq[float32](n)
  let sf = s.get
  if sf.format == Gray8 and not area.isEmpty:
    # Selections are GRAY8: copy tile bytes through a table.
    let w = area.width
    var lut: array[256, float32]
    for i in 0 .. 255: lut[i] = float32(i) / 255'f32
    let dp = (let d = sf.defaultPixel; if d.len > 0: d[0] else: 0'f32)
    result = newSeq[float32](n)
    for yy in 0 ..< area.height:
      let y = area.y0 + yy
      var x = area.x0
      while x < area.x1:
        let tc = TileCoord.containing(x, y)
        let x1 = min(tc.rect.x1, area.x1)
        let t = sf.tiles.getOrDefault(tc)
        let base = (y - tc.ty * TileSize) * TileSize + (x - tc.tx * TileSize)
        let span = x1 - x
        let dst = yy * w + (x - area.x0)
        if not t.isNil and base + span <= t.data.len:
          for i in 0 ..< span: result[dst + i] = lut[t.data[base + i]]
        else:
          for i in 0 ..< span: result[dst + i] = dp
        x = x1
    return
  var v: seq[float32]
  sf.readRegionInto(area, v)
  let ch = sf.channels
  if ch > 1:
    result = newSeq[float32](v.len div ch)
    for i in 0 ..< result.len: result[i] = v[i * ch]
  else: result = v

proc coverage8(v: float32): uint8 {.inline.} =
  ## 8-bit selection value of a coverage (clamped and rounded, as GRAY8 surfaces store it; NaN → 0).
  asU8(clamp(v, 0'f32, 1'f32) * 255'f32 + 0.5'f32)

proc surfaceFromMasks(area: Rect; newM: openArray[float32]; prev: Option[seq[float32]];
                      f: proc(p, v: float32): float32): Surface =
  ## Builds a GRAY8 selection surface over `area` whose coverage at each sample is `f(prev, new)`,
  ## `prev` reading 0 without a `prev` mask and missing samples reading 0. Only the tiles that
  ## hold something are kept, and fully selected tiles share one tile.
  result = newSurface(Gray8)
  if area.isEmpty: return
  let w = area.width
  let full = result.solidTile([1'f32])
  template at(m: openArray[float32]; i: int): float32 = (if i < m.len: m[i] else: 0'f32)
  var built: seq[(TileCoord, Tile)]
  let blanks = result.takeTiles(area)
  for (tc, blank) in blanks:
    let tr = tc.rect.intersect(area)
    let (lx, span) = (tr.x0 - area.x0, tr.width)
    var tile = blank.clone
    var (lo, hi) = ((if tr == tc.rect: 255'u8 else: 0'u8), 0'u8)
    for y in tr.y0 ..< tr.y1:
      let o = (y - tc.ty * TileSize) * TileSize + (tr.x0 - tc.tx * TileSize)
      if o + span > tile.data.len: continue
      let i0 = (y - area.y0) * w + lx
      for k in 0 ..< span:
        let i = i0 + k
        let p = (if prev.isSome: at(prev.get, i) else: 0'f32)
        let d = coverage8(f(p, at(newM, i)))
        tile.data[o + k] = d
        lo = min(lo, d)
        hi = max(hi, d)
    if hi > 0: built.add (tc, (if lo == 255'u8: full else: tile))
  result.putTiles(built)

proc maskToSurface*(m: openArray[float32]; area: Rect): Surface =
  ## Builds a GRAY8 selection surface from a mask over `area` (missing samples are 0).
  surfaceFromMasks(area, m, none(seq[float32]), proc(p, v: float32): float32 = v)

proc q8(v: float32): float32 = float32(coverage8(v)) / 255'f32

proc combine*(old: Option[Surface]; newM: openArray[float32]; area: Rect; mode: SelectionMode): Option[Surface] =
  ## Combines `new` with `old` by `mode`. None when nothing is selected.
  if area.isEmpty: return none(Surface)
  # Quantize to the 8-bit grid selections are stored on, so combining a mask with itself is
  # exact. Below half an 8-bit step is nothing.
  let s =
    case mode
    of smReplace: surfaceFromMasks(area, newM, none(seq[float32]), proc(p, v: float32): float32 = v)
    else:
      let prev = some(maskFromSurface(old, area))
      case mode
      of smAdd: surfaceFromMasks(area, newM, prev, proc(p, v: float32): float32 = rmax(p, q8(v)))
      of smSubtract: surfaceFromMasks(area, newM, prev, proc(p, v: float32): float32 = rmax(p - q8(v), 0'f32))
      else: surfaceFromMasks(area, newM, prev, proc(p, v: float32): float32 = rmin(p, q8(v)))
  if s.tileCount > 0: some(s) else: none(Surface)

proc close(a, b: array[4, float32]; tol: float32): bool =
  if a[3] == 0 and b[3] == 0: return true
  for c in 0 .. 3:
    if not (abs(a[c] - b[c]) * 255'f32 <= tol + 1e-3'f32): return false
  true

proc antialias(m: var seq[float32]; w, h: int) =
  ## Softens hard mask edges: edge pixels become the 3×3 average.
  let src = m
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = y * w + x
      var sum = 0'f32
      var cnt = 0'f32
      var edge = false
      for dy in -1 .. 1:
        for dx in -1 .. 1:
          let (xx, yy) = (x + dx, y + dy)
          let v = (if xx < 0 or yy < 0 or xx >= w or yy >= h: src[i] else: src[yy * w + xx])
          edge = edge or v != src[i]
          sum += v
          cnt += 1
      if edge:
        # Keep selected pixels mostly selected, add a soft fringe.
        m[i] = (if src[i] > 0.5'f32: 0.5'f32 + 0.5'f32 * sum / cnt else: 0.5'f32 * sum / cnt)

proc magicWand*(px: openArray[array[4, float32]]; area: Rect; seed: (int, int); tolerance: float32;
                contiguous, antiAlias: bool): seq[float32] =
  ## Pixels similar to the seed pixel (per-channel difference ≤ `tolerance` levels, alpha
  ## included; fully transparent pixels match regardless of hidden RGB). `contiguous` limits to
  ## the 4-connected region around the seed. `px` covers `area`.
  let (w, h) = (area.width, area.height)
  result = newSeq[float32](w * h)
  if not area.contains(seed[0], seed[1]) or px.len < w * h: return
  let si = (seed[1] - area.y0) * w + (seed[0] - area.x0)
  let target = px[si]
  if contiguous:
    var stack = @[si]
    result[si] = 1
    while stack.len > 0:
      let i = stack.pop()
      let (x, y) = (i mod w, i div w)
      template visit(j: int) =
        if result[j] == 0 and close(px[j], target, tolerance):
          result[j] = 1
          stack.add j
      if x > 0: visit(i - 1)
      if x + 1 < w: visit(i + 1)
      if y > 0: visit(i - w)
      if y + 1 < h: visit(i + w)
  else:
    for i in 0 ..< w * h:
      if close(px[i], target, tolerance): result[i] = 1
  if antiAlias: antialias(result, w, h)

proc rgba8Image*(s: Surface; area: Rect): seq[array[4, uint8]] =
  ## Straight RGBA8 copy of `area` of a surface.
  let (w, h) = (area.width, area.height)
  result = newSeq[array[4, uint8]](w * h)
  if w == 0 or h == 0: return
  s.readRgba8Into(area, result)

proc at*(r: Region; x, y: int): float32 =
  ## Coverage (0–1) at document pixel `(x, y)`; zero outside the box.
  if not r.bbox.contains(x, y): return 0
  let i = (y - r.bbox.y0) * r.bbox.width + (x - r.bbox.x0)
  if i < r.mask.len: float32(r.mask[i]) / 255'f32 else: 0'f32

proc wandRegion*(img: openArray[array[4, uint8]]; area: Rect; seed: (int, int); tolerance: float32;
                 contiguous, antiAlias: bool): Option[Region] =
  ## Magic wand on 8-bit pixels: a scanline flood fill (or a global match) that only allocates a
  ## byte mark per pixel, then anti-aliases just the edges inside the touched bounding box.
  ## Matches `magicWand` for 8-bit sources. None if the seed is outside `area`.
  if not area.contains(seed[0], seed[1]): return none(Region)
  let (w, h) = (area.width, area.height)
  if img.len < w * h: return none(Region)
  let target = img[(seed[1] - area.y0) * w + (seed[0] - area.x0)]
  let tol = asI32(float64(rmax(floor(tolerance + 1e-3'f32), 0'f32)))
  # Copying a selection can leave RGB under zero alpha. Those invisible colours must not split a
  # transparent region into pieces, as if the copied source were still visible.
  proc similar(p: array[4, uint8]): bool =
    if target[3] == 0 and p[3] == 0: return true
    for c in 0 .. 3:
      if abs(int(p[c]) - int(target[c])) > tol: return false
    true
  var marks = newSeq[uint8](w * h)
  var (bx0, by0, bx1, by1) = (high(int), high(int), 0, 0)
  if contiguous:
    # Span fill: fill a run, then scan the rows above and below it for new seeds.
    var stack = @[(seed[0] - area.x0, seed[1] - area.y0)]
    while stack.len > 0:
      let (x, y) = stack.pop()
      let row = y * w
      if marks[row + x] != 0: continue
      var (l, r) = (x, x)
      while l > 0 and marks[row + l - 1] == 0 and similar(img[row + l - 1]): dec l
      while r + 1 < w and marks[row + r + 1] == 0 and similar(img[row + r + 1]): inc r
      for k in row + l .. row + r: marks[k] = 255
      (bx0, bx1, by0, by1) = (min(bx0, l), max(bx1, r), min(by0, y), max(by1, y))
      for ny in [y - 1, y + 1]:
        if ny < 0 or ny >= h: continue
        let nrow = ny * w
        var inRun = false
        for nx in l .. r:
          let ok = marks[nrow + nx] == 0 and similar(img[nrow + nx])
          if ok and not inRun: stack.add (nx, ny)
          inRun = ok
  else:
    for y in 0 ..< h:
      var a = -1
      var b = -1
      for x in 0 ..< w:
        if similar(img[y * w + x]):
          marks[y * w + x] = 255
          if a < 0: a = x
          b = x
      if a >= 0: (bx0, bx1, by0, by1) = (min(bx0, a), max(bx1, b), min(by0, y), max(by1, y))
  if bx0 == high(int): return none(Region)
  # One pixel of fringe for anti-aliasing, clamped to the area.
  let pad = (if antiAlias: 1 else: 0)
  let (x0, y0) = (max(bx0 - pad, 0), max(by0 - pad, 0))
  let (x1, y1) = (min(bx1 + 1 + pad, w), min(by1 + 1 + pad, h))
  let bw = x1 - x0
  template at(x, y: int): uint8 = marks[y * w + x]
  var mask = newSeq[uint8](bw * (y1 - y0))
  for yy in 0 ..< y1 - y0:
    let y = y0 + yy
    for xx in 0 ..< bw:
      let x = x0 + xx
      let v = at(x, y)
      var o = v
      if antiAlias:
        # Same rule as `antialias`: edge pixels blend with their 3×3 neighbourhood; neighbours
        # outside the area count as the pixel itself.
        var sum = 0'u32
        var edge = false
        for dy in -1 .. 1:
          for dx in -1 .. 1:
            let (nx, ny) = (x + dx, y + dy)
            let n = (if nx < 0 or ny < 0 or nx >= w or ny >= h: v else: at(nx, ny))
            edge = edge or n != v
            sum += uint32(n)
        if edge:
          let avg = float32(sum) / (9'f32 * 255'f32)
          let f = (if v > 127: 0.5'f32 + 0.5'f32 * avg else: 0.5'f32 * avg)
          o = asU8(round(f * 255'f32))
      mask[yy * bw + xx] = o
  let bbox = rect(area.x0 + x0, area.y0 + y0, area.x0 + x1, area.y0 + y1)
  some(Region(bbox: bbox, mask: mask))

proc combineRegion*(old: Option[Surface]; newR: Option[Region]; mode: SelectionMode): Option[Surface] =
  ## Combines a `Region` with the current selection, touching only the region's box (outside it
  ## the new mask is zero). None when nothing remains selected.
  let nr = (if newR.isSome: newR.get else: Region(bbox: EmptyRect))
  let b = nr.bbox
  proc prevU8(s: Surface): seq[uint8] =
    var v: seq[float32]
    s.readRegionInto(b, v)
    let ch = max(s.channels, 1)
    result = newSeq[uint8](v.len div ch)
    for i in 0 ..< result.len: result[i] = asU8(round(clamp(v[i * ch], 0'f32, 1'f32) * 255'f32))
  proc fromBytes(bytes: openArray[uint8]): Option[Surface] =
    var any = false
    for v in bytes:
      if v != 0:
        any = true
        break
    if b.isEmpty or not any: return none(Surface)
    var s = surfaceFromInterleaved(Gray8, b, bytes)
    s.prune()
    some(s)
  proc nonempty(s: Surface): Option[Surface] = (if s.contentBounds.isEmpty: none(Surface) else: some(s))
  if mode == smReplace or (mode == smAdd and old.isNone): return fromBytes(nr.mask)
  if old.isNone: return none(Surface)
  let o = old.get
  if mode == smIntersect:
    let p = prevU8(o)
    var m = newSeq[uint8](min(p.len, nr.mask.len))
    for i in 0 ..< m.len: m[i] = min(p[i], nr.mask[i])
    return fromBytes(m)
  if b.isEmpty: return nonempty(o)
  let p = prevU8(o)
  var outp = newSeq[float32](min(p.len, nr.mask.len))
  for i in 0 ..< outp.len:
    let (a, n) = (p[i], nr.mask[i])
    outp[i] = float32(if mode == smAdd: max(a, n) else: (if a > n: a - n else: 0'u8)) / 255'f32
  var s = o
  s.writeRegion(b, outp)
  s.prune()
  nonempty(s)

proc colorRange*(px: openArray[array[4, float32]]; color: array[3, float32]; fuzziness: float32): seq[float32] =
  ## Colour Range: coverage falls off linearly with the per-channel colour distance; `fuzziness`
  ## in levels (0–200).
  let f = rmax(fuzziness, 0'f32) + 1'f32
  result = newSeq[float32](px.len)
  for i, p in px:
    if p[3] <= 0: continue
    var d = 0'f32
    for c in 0 .. 2: d = rmax(d, abs(p[c] - color[c]) * 255'f32)
    result[i] = clamp(1'f32 - d / f, 0'f32, 1'f32)

proc colorRangeSamples*(px: openArray[array[4, float32]]; w: int; samples: openArray[RangeSample];
                        fuzziness: float32; localized: Option[float32]): seq[float32] =
  ## Color Range › Sampled Colors with several samples: each pixel takes its best match over the
  ## samples, each judged like `colorRange`. With `localized` (Localized Color Clusters) a sample
  ## also fades with distance from where it was picked, reaching 0 at `localized` pixels.
  let f = rmax(fuzziness, 0'f32) + 1'f32
  let ww = max(w, 1)
  result = newSeq[float32](px.len)
  for i, p in px:
    if p[3] <= 0: continue
    let (x, y) = (float32(i mod ww), float32(i div ww))
    var best = 0'f32
    for s in samples:
      var d = 0'f32
      for c in 0 .. 2: d = rmax(d, abs(p[c] - s.color[c]) * 255'f32)
      var k = clamp(1'f32 - d / f, 0'f32, 1'f32)
      if localized.isSome and s.at.isSome:
        let (sx, sy) = s.at.get
        let dist = sqrt((x - sx) * (x - sx) + (y - sy) * (y - sy))
        k *= clamp(1'f32 - dist / rmax(localized.get, 1'f32), 0'f32, 1'f32)
      best = rmax(best, k)
    result[i] = best

proc point*(_: typedesc[LabRange]; lab: array[3, float32]): LabRange =
  ## The box around one colour.
  LabRange(lo: lab, hi: lab)

proc grow*(r: LabRange; lab: array[3, float32]): LabRange =
  ## "Add to Sample": the box grows to take `lab` in.
  for k in 0 .. 2:
    result.lo[k] = rmin(r.lo[k], lab[k])
    result.hi[k] = rmax(r.hi[k], lab[k])

proc spanning*(_: typedesc[LabRange]; colors: openArray[array[3, float32]]): Option[LabRange] =
  ## The box spanning `colors`, none when there are none.
  if colors.len == 0: return none(LabRange)
  var r = LabRange.point(colors[0])
  for c in colors: r = r.grow(c)
  some(r)

proc subtract*(self: LabRange; lab: array[3, float32]; fuzziness: float32): LabRange =
  ## "Subtract from Sample" the way Photoshop does it: it only reshapes the box, each Lab axis on
  ## its own, with `w` the axis's width at `fuzziness` (L: fuzziness/2.55 units, a and b: a third
  ## of fuzziness·256/255) and q = w/5. Inside the box the nearer edge moves onto the subtracted
  ## value (the upper one on a tie); on an edge or outside it by less than w, that edge moves in to
  ## the first of m + q, …, m + 5q beyond it; further out nothing changes.
  let fz = (if fuzziness.finite: rmax(fuzziness, 0'f32) else: 0'f32)
  result = self
  for k in 0 .. 2:
    let m = lab[k]
    # Measured: the lightness step has no 256/255 factor, the a/b steps do.
    let w = (if k == 0: fz / 2.55'f32 else: fz * 256'f32 / 255'f32 / 3'f32)
    let (lo, hi) = (result.lo[k], result.hi[k])
    if not m.finite: continue
    if m > lo and m < hi:
      if m - lo < hi - m: result.lo[k] = m
      else: result.hi[k] = m
      continue
    let q = w / 5'f32
    if q <= 0: continue
    let d = (if m <= lo: lo - m else: m - hi)
    let n = floor(d / q) + 1'f32
    if n > 5: continue
    if m <= lo: result.lo[k] = rmin(m + n * q, hi)
    else: result.hi[k] = rmax(m - n * q, lo)

proc coverage(lab: array[3, float32]; range: LabRange; fuzziness: float32; grid8: bool): float32 =
  proc outside(v, lo, hi: float32): float32 = rmax(rmax(lo - v, v - hi), 0'f32)
  if fuzziness.isNaN or fuzziness <= 0:
    let d = rmax(rmax(outside(lab[0], range.lo[0], range.hi[0]) * 2.55'f32, outside(lab[1], range.lo[1], range.hi[1])),
                 outside(lab[2], range.lo[2], range.hi[2]))
    return (if d < 0.5'f32: 1'f32 else: 0'f32)
  proc ab(v, lo, hi: float32): float32 =
    if not grid8: outside(v, lo, hi)
    elif v > hi: v - hi + 0.5'f32
    elif v < lo: rmax(lo - v - 0.5'f32, 0'f32)
    else: 0'f32
  let dl = outside(lab[0], range.lo[0], range.hi[0]) * 2.55'f32
  let da = 3'f32 * ab(lab[1], range.lo[1], range.hi[1])
  let db = 3'f32 * ab(lab[2], range.lo[2], range.hi[2])
  let t = rmax(rmax(dl, da), db) / (fuzziness * 256'f32 / 255'f32)
  let v =
    if t <= 0.5'f32: 1'f32 - 2'f32 * t * t
    elif t < 1: 2'f32 * (1'f32 - t) * (1'f32 - t)
    else: 0'f32
  clamp(floor(v * 256'f32), 0'f32, 255'f32) / 255'f32

proc labRangeCoverage*(lab: array[3, float32]; range: LabRange; fuzziness: float32): float32 =
  ## How much a Lab colour belongs to `range` at `fuzziness` (0–200), the way Photoshop's
  ## Select › Color Range › Sampled Colors decides it (reconstructed by observation): per axis the
  ## distance outside the box in 8-bit Lab units (L × 2.55, a and b weighted ×3), the coverage the
  ## falloff of the largest, a quadratic B-spline of width fuzziness × 256/255 quantised to 256
  ## levels. Fuzziness 0 keeps only colours equal to the samples.
  coverage(lab, range, fuzziness, false)

proc labRangeCoverage8bit*(lab: array[3, float32]; range: LabRange; fuzziness: float32): float32 =
  ## `labRangeCoverage` for 8-bit documents, compared on the 8-bit Lab grid: a and b steps above
  ## the range count half a unit more, below it half a unit less.
  coverage(lab, range, fuzziness, true)

proc quantizeLab8*(lab: array[3, float32]): array[3, float32] =
  ## A Lab colour on Photoshop's 8-bit Lab grid (L in 1/2.55 steps, a and b whole units).
  [round(lab[0] * 2.55'f32) / 2.55'f32, round(lab[1]), round(lab[2])]

proc colorRangeLab*(lab: openArray[array[4, float32]]; w: int; range: LabRange; fuzziness: float32; grid8: bool;
                    localized: Option[(float32, seq[(float32, float32)])]): seq[float32] =
  ## Color Range › Sampled Colors over Lab pixels (`lab[i]` = L, a, b, alpha; `w` the row width):
  ## each pixel's `labRangeCoverage` (`labRangeCoverage8bit` with `grid8`), 0 where transparent.
  ## With `localized` (a radius in pixels and the sample positions) coverage also fades with the
  ## distance to the nearest sample, reaching 0 at the radius.
  let w = max(w, 1)
  let loc = (if localized.isSome and localized.get[1].len > 0: localized else: none((float32, seq[(float32, float32)])))
  result = newSeq[float32](lab.len)
  for i, p in lab:
    let (x, y) = (i mod w, i div w)
    let alpha = p[3]
    if alpha.isNaN or alpha <= 0: continue
    var k = coverage([p[0], p[1], p[2]], range, fuzziness, grid8)
    if loc.isSome and k > 0:
      let (radius, at) = loc.get
      let (px, py) = (float32(x), float32(y))
      var near = 0'f32
      for (sx, sy) in at:
        near = rmax(near, clamp(1'f32 - sqrt((px - sx) * (px - sx) + (py - sy) * (py - sy)) / rmax(radius, 1'f32), 0'f32, 1'f32))
      k *= near
    result[i] = k

proc hueChroma(c: array[3, float32]): (float32, float32) =
  ## Hue (degrees, 0..360) and chroma (max − min) of an RGB colour.
  let (r, g, b) = (c[0], c[1], c[2])
  let mx = rmax(rmax(r, g), b)
  let mn = rmin(rmin(r, g), b)
  let d = mx - mn
  if d <= 0: return (0'f32, 0'f32)
  let h =
    if mx == r: remEuclidF((g - b) / d, 6'f32)
    elif mx == g: (b - r) / d + 2'f32
    else: (r - g) / d + 4'f32
  (h * 60'f32, d)

proc hueRange*(px: openArray[array[4, float32]]; center: float32): seq[float32] =
  ## Color Range › Select: Reds, Yellows, Greens, Cyans, Blues or Magentas (`center` = 0, 60, …,
  ## 300°): full at the family's hue, none 60° away, times the chroma.
  result = newSeq[float32](px.len)
  for i, p in px:
    if p[3] <= 0: continue
    let (h, chroma) = hueChroma([clamp(p[0], 0'f32, 1'f32), clamp(p[1], 0'f32, 1'f32), clamp(p[2], 0'f32, 1'f32)])
    var dh = remEuclidF(h - center, 360'f32)
    dh = rmin(dh, 360'f32 - dh)
    result[i] = clamp(clamp(1'f32 - dh / 60'f32, 0'f32, 1'f32) * chroma, 0'f32, 1'f32)

proc toneRange*(px: openArray[array[4, float32]]; lo, hi, falloff: float32): seq[float32] =
  ## Color Range › Select: Highlights, Midtones or Shadows. Pixels whose luminance (Rec. 601,
  ## 0..255) lies in `lo..=hi` are fully selected; outside, selection falls off linearly to 0 over
  ## `falloff` levels.
  let ramp = rmax(falloff, 1e-3'f32)
  result = newSeq[float32](px.len)
  for i, p in px:
    if p[3] <= 0: continue
    let l = clamp(0.299'f32 * p[0] + 0.587'f32 * p[1] + 0.114'f32 * p[2], 0'f32, 1'f32) * 255'f32
    let o = (if l < lo: lo - l elif l > hi: l - hi else: 0'f32)
    result[i] = clamp(1'f32 - o / ramp, 0'f32, 1'f32)

proc expand*(m: openArray[float32]; w, h: int; r: float32): seq[float32] =
  ## Grows the selection by `r` pixels.
  var inside = newSeq[bool](m.len)
  for i, v in m: inside[i] = v >= 0.5'f32
  let d = edt(inside, w, h)
  # A pixel whose centre is `d` away from the nearest selected centre is covered when d ≤ r
  # (fractional radii give partial coverage).
  result = newSeq[float32](min(m.len, d.len))
  for i in 0 ..< result.len: result[i] = rmax(m[i], clamp(r + 1'f32 - d[i], 0'f32, 1'f32))

proc contract*(m: openArray[float32]; w, h: int; r: float32): seq[float32] =
  ## Shrinks the selection by `r` pixels.
  var inv = newSeq[float32](m.len)
  for i, v in m: inv[i] = 1'f32 - v
  result = expand(inv, w, h, r)
  for v in result.mitems: v = 1'f32 - v

proc border*(m: openArray[float32]; w, h: int; r: float32): seq[float32] =
  ## Select › Modify › Border: a soft band `2r` wide centred on the selection edge, as in
  ## Photoshop: a pixel `D` away (centre to centre) from the nearest pixel on the other side of
  ## the edge is selected `1 − (D − 1) / r`.
  var inside = newSeq[bool](m.len)
  var outside = newSeq[bool](m.len)
  for i, v in m:
    inside[i] = v >= 0.5'f32
    outside[i] = not inside[i]
  let (toInside, toOutside) = (edt(inside, w, h), edt(outside, w, h))
  if r <= 0 or toInside.len != m.len or toOutside.len != m.len: return newSeq[float32](m.len)
  result = newSeq[float32](m.len)
  for i in 0 ..< m.len:
    let d = (if inside[i]: toOutside[i] else: toInside[i])
    result[i] = clamp(1'f32 - (d - 1'f32) / r, 0'f32, 1'f32)

proc smooth*(m: openArray[float32]; w, h: int; r: float32): seq[float32] =
  ## Smooth: removes specks and rounds corners (box average of radius `round(r)`, edges
  ## replicated, then re-thresholded).
  selection_blur.smooth(m, w, h, r)

proc feather*(m: openArray[float32]; w, h: int; radius: float32): seq[float32] =
  ## Feather: Gaussian blur of the mask with sigma = radius, as in Photoshop (kernel truncated at
  ## 3σ, zero beyond the canvas).
  selection_blur.feather(m, w, h, radius)

proc polygon*(points: openArray[(float32, float32)]; area: Rect; antiAlias: bool): seq[float32] =
  ## Anti-aliased polygon (even-odd) coverage over `area`: exact horizontal coverage, 4 sub-rows
  ## per pixel row (or 1 when not anti-aliased).
  let (w, h) = (area.width, area.height)
  result = newSeq[float32](w * h)
  if points.len < 3: return
  let sub = (if antiAlias: 4 else: 1)
  var xs: seq[float32]
  for py in 0 ..< h:
    for s in 0 ..< sub:
      let y = float32(area.y0) + float32(py) + (float32(s) + 0.5'f32) / float32(sub)
      xs.setLen(0)
      for i in 0 ..< points.len:
        let (a, b) = (points[i], points[(i + 1) mod points.len])
        if (a[1] <= y and b[1] > y) or (b[1] <= y and a[1] > y):
          xs.add a[0] + (y - a[1]) / (b[1] - a[1]) * (b[0] - a[0])
      xs.sort(proc(a, b: float32): int = totalCmp(a, b))
      var k = 0
      while k + 1 < xs.len:
        let (x0, x1) = (rmax(xs[k] - float32(area.x0), 0'f32), rmin(xs[k + 1] - float32(area.x0), float32(w)))
        k += 2
        if not (x1 > x0): continue
        let (c0, c1) = (asIdx(floor(x0)), min(asIdx(ceil(x1)), w))
        for cx in c0 ..< c1:
          let cov =
            if antiAlias: rmin(float32(cx + 1), x1) - rmax(float32(cx), x0)
            else:
              let c = float32(cx) + 0.5'f32
              (if c >= x0 and c < x1: 1'f32 else: 0'f32)
          result[py * w + cx] += rmax(cov, 0'f32) / float32(sub)
  for v in result.mitems: v = rmin(v, 1'f32)
