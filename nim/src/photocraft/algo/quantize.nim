## Colour reduction for Image › Mode › Indexed Color and Bitmap: palette construction (exact,
## system, web, uniform, perceptual, selective, adaptive), forced colours, and dithering (none,
## Floyd–Steinberg diffusion, ordered pattern, noise), plus 1-bit conversion methods (port of
## `quantize.rs` and `quantize/nearest.rs`).
##
## Buffers are straight RGBA in 0..=1, row-major. Median cut follows Heckbert (1982); k-means
## refinement is Lloyd's algorithm; error diffusion is Floyd & Steinberg (1976).

import std/[math, algorithm, tables, options]
import ../rmath, ../color/convert
import ./util

type
  Rgb8* = array[3, uint8]

  PaletteKind* = enum
    pkExact = "Exact"                 ## every colour of the image (fails above 256)
    pkSystemMac = "SystemMac"
    pkSystemWindows = "SystemWindows"
    pkWeb = "Web"
    pkUniform = "Uniform"
    pkPerceptual = "Perceptual"       ## Lab median cut + k-means (perceptually weighted)
    pkSelective = "Selective"         ## perceptual, with entries near web-safe colours snapped to them
    pkAdaptive = "Adaptive"           ## RGB median cut weighted by frequency

  Forced* = enum
    fcNone = "None", fcBlackWhite = "BlackWhite", fcPrimaries = "Primaries", fcWeb = "Web"

  Dither* = enum
    dtNone = "None", dtDiffusion = "Diffusion", dtPattern = "Pattern", dtNoise = "Noise"

  HalftoneShape* = enum
    hsRound = "Round", hsEllipse = "Ellipse", hsLine = "Line", hsSquare = "Square", hsDiamond = "Diamond",
    hsCross = "Cross"

  BitmapMethodKind* = enum bmThreshold = "Threshold", bmPattern = "Pattern", bmDiffusion = "Diffusion",
                           bmHalftone = "Halftone"
  BitmapMethod* = object
    ## Image › Mode › Bitmap methods. Halftone screen: cell size in pixels, angle in degrees, dot shape.
    case kind*: BitmapMethodKind
    of bmHalftone:
      cell*, angle*: float32
      shape*: HalftoneShape
    else: discard

  QuantizeError* = object of CatchableError

proc paletteKindFromId*(s: string): Option[PaletteKind] =
  case s
  of "exact": some(pkExact)
  of "system", "systemMac", "systemMacOs": some(pkSystemMac)
  of "systemWindows": some(pkSystemWindows)
  of "web": some(pkWeb)
  of "uniform": some(pkUniform)
  of "perceptual", "localPerceptual": some(pkPerceptual)
  of "selective", "localSelective": some(pkSelective)
  of "adaptive", "localAdaptive": some(pkAdaptive)
  else: none(PaletteKind)

proc halftoneShapeFromId*(s: string): Option[HalftoneShape] =
  case s
  of "round": some(hsRound)
  of "ellipse": some(hsEllipse)
  of "line": some(hsLine)
  of "square": some(hsSquare)
  of "diamond": some(hsDiamond)
  of "cross": some(hsCross)
  else: none(HalftoneShape)

# ---------------------------------------------------------------- exact nearest colour search

type
  Entry = object
    point: array[3, float32]
    index, axis: int
  Nearest = object
    ## Exact nearest colour search. Balanced median splits bound recursion to O(log n). Small
    ## palettes use a linear scan; no colour buckets, sampling or approximate distances.
    entries: seq[Entry]
    tree: bool

proc distance(a, b: array[3, float32]): float32 {.inline.} =
  (a[0] - b[0]) * (a[0] - b[0]) + (a[1] - b[1]) * (a[1] - b[1]) + (a[2] - b[2]) * (a[2] - b[2])

proc finite3(p: array[3, float32]): bool {.inline.} =
  for v in p:
    if v.classify in {fcNan, fcInf, fcNegInf}: return false
  true

proc partition(e: var seq[Entry]; a, b: int; depth = 0) =
  ## Median split of `e[a ..< b]` along its widest axis (the pivot is the element of rank `len/2`
  ## in (coordinate, index) order; which side the others land on is all the search relies on).
  let n = b - a
  if n <= 8 or depth > 64: return
  var lo = [F32Max, F32Max, F32Max]
  var hi = [-F32Max, -F32Max, -F32Max]
  for k in a ..< b:
    for c in 0 .. 2:
      lo[c] = rmin(lo[c], e[k].point[c])
      hi[c] = rmax(hi[c], e[k].point[c])
  # `max_by`: the last of equal maxima.
  var axis = 0
  for c in 1 .. 2:
    if totalCmp(hi[c] - lo[c], hi[axis] - lo[axis]) >= 0: axis = c
  var part = e[a ..< b]
  part.sort(proc(x, y: Entry): int =
    let t = totalCmp(x.point[axis], y.point[axis])
    if t != 0: t else: cmp(x.index, y.index))
  for k in 0 ..< n: e[a + k] = part[k]
  let mid = a + n div 2
  e[mid].axis = axis
  partition(e, a, mid, depth + 1)
  partition(e, mid + 1, b, depth + 1)

proc newNearest(points: openArray[array[3, float32]]): Nearest =
  for i, p in points: result.entries.add Entry(point: p, index: i)
  result.tree = result.entries.len > 64
  if result.tree:
    for en in result.entries:
      if not finite3(en.point):
        result.tree = false
        break
  if result.tree: partition(result.entries, 0, result.entries.len)

proc consider(en: Entry; point: array[3, float32]; best: var (float32, int)) {.inline.} =
  let d = distance(en.point, point)
  # Original palette order breaks ties, independent of the tree's traversal order.
  if d < best[0] or (d == best[0] and en.index < best[1]): best = (d, en.index)

proc search(e: seq[Entry]; a, b: int; point: array[3, float32]; best: var (float32, int)) =
  let n = b - a
  if n <= 8:
    for k in a ..< b: consider(e[k], point, best)
    return
  let mid = a + n div 2
  let pivot = e[mid]
  consider(pivot, point, best)
  let delta = point[pivot.axis] - pivot.point[pivot.axis]
  let (nearA, nearB, farA, farB) = (if delta <= 0: (a, mid, mid + 1, b) else: (mid + 1, b, a, mid))
  if nearB > nearA: search(e, nearA, nearB, point, best)
  # The splitting plane is a lower bound, using the same f32 arithmetic as `distance`.
  if farB > farA and delta * delta <= best[0]: search(e, farA, farB, point, best)

proc find(n: Nearest; point: array[3, float32]): int =
  var best = (F32Max, 0)
  if not n.tree:
    for en in n.entries:
      let d = distance(en.point, point)
      if d < best[0]: best = (d, en.index)
  elif finite3(point):
    search(n.entries, 0, n.entries.len, point, best)
  best[1]

# ---------------------------------------------------------------- palettes

proc to8(v: float32): uint8 = asU8(round(clamp(v, 0'f32, 1'f32) * 255'f32))
proc rgb8(p: array[4, float32]): Rgb8 = [to8(p[0]), to8(p[1]), to8(p[2])]

proc webPalette*(): seq[Rgb8] =
  ## The 216-colour web-safe cube (0, 51, …, 255 per channel).
  for r in 0'u8 ..< 6'u8:
    for g in 0'u8 ..< 6'u8:
      for b in 0'u8 ..< 6'u8: result.add [r * 51, g * 51, b * 51]

proc macPalette*(): seq[Rgb8] =
  ## Classic Mac OS system palette: the web cube plus 10-step red, green, blue and gray ramps.
  result = webPalette()
  # Ramps use the values between cube steps (17-unit spacing, skipping multiples of 51).
  var steps: seq[uint8]
  for k in 1'u8 .. 15'u8:
    if (k * 17) mod 51 != 0: steps.add k * 17
  for s in steps.reversed: result.add [s, 0'u8, 0'u8]
  for s in steps.reversed: result.add [0'u8, s, 0'u8]
  for s in steps.reversed: result.add [0'u8, 0'u8, s]
  for s in steps.reversed: result.add [s, s, s]
  if result.len > 256: result.setLen(256)

proc windowsPalette*(): seq[Rgb8] =
  ## Windows default palette: the 20 static system colours plus the web cube and a gray ramp.
  result = @[[0'u8, 0, 0], [128'u8, 0, 0], [0'u8, 128, 0], [128'u8, 128, 0], [0'u8, 0, 128], [128'u8, 0, 128],
             [0'u8, 128, 128], [192'u8, 192, 192], [192'u8, 220, 192], [166'u8, 202, 240], [255'u8, 251, 240],
             [160'u8, 160, 164], [128'u8, 128, 128], [255'u8, 0, 0], [0'u8, 255, 0], [255'u8, 255, 0], [0'u8, 0, 255],
             [255'u8, 0, 255], [0'u8, 255, 255], [255'u8, 255, 255]]
  for c in webPalette():
    if c notin result: result.add c
  var g = 8'u8
  while result.len < 256:
    if [g, g, g] notin result: result.add [g, g, g]
    g = g + 12 # wraps like Rust's `wrapping_add`

proc uniformPalette*(n: int): seq[Rgb8] =
  ## Up to `n` colours on an equal-step RGB grid.
  let k = max(asIdx(floor(rmath.cbrt(float32(n)))), 2)
  proc lv(i: int): uint8 = asU8(round(float32(i) / float32(k - 1) * 255'f32))
  for r in 0 ..< k:
    for g in 0 ..< k:
      for b in 0 ..< k: result.add [lv(r), lv(g), lv(b)]

proc forcedColors(f: Forced): seq[Rgb8] =
  case f
  of fcNone: @[]
  of fcBlackWhite: @[[0'u8, 0, 0], [255'u8, 255, 255]]
  of fcPrimaries: @[[0'u8, 0, 0], [255'u8, 255, 255], [255'u8, 0, 0], [0'u8, 255, 0], [0'u8, 0, 255], [0'u8, 255, 255],
                    [255'u8, 0, 255], [255'u8, 255, 0]]
  of fcWeb: webPalette()

proc histogram(px: openArray[array[4, float32]]): seq[(Rgb8, uint32)] =
  ## Unique 8-bit colours with counts (opaque pixels only), sorted.
  var m = initTable[Rgb8, uint32]()
  for p in px:
    if p[3] >= 0.5'f32: m.mgetOrPut(rgb8(p), 0) += 1
  for k, v in m: result.add (k, v)
  # Rust sorts the (colour, count) tuples lexicographically (colours are unique).
  result.sort(proc(a, b: (Rgb8, uint32)): int =
    for k in 0 .. 2:
      if a[0][k] != b[0][k]: return cmp(a[0][k], b[0][k])
    cmp(a[1], b[1]))

proc medianCut(pts: seq[(array[3, float32], uint32)]; n: int): seq[array[3, float32]] =
  ## Median cut over `pts` (feature vectors + weights) into at most `n` boxes; returns box means.
  if pts.len == 0 or n <= 0: return
  var boxes = @[pts]
  while boxes.len < n:
    # Split the box with the largest weighted extent (the first of equal scores).
    var (bi, axis, score) = (-1, 0, -1'f32)
    for i, b in boxes:
      if b.len <= 1: continue
      var best = (0, -1'f32)
      for a in 0 .. 2:
        var (lo, hi) = (F32Max, -F32Max)
        for p in b:
          lo = rmin(lo, p[0][a])
          hi = rmax(hi, p[0][a])
        if hi - lo > best[1]: best = (a, hi - lo)
      var w = 0'f32
      for p in b: w += float32(p[1])
      let s = best[1] * sqrt(w)
      if s > score: (bi, axis, score) = (i, best[0], s)
    if bi < 0 or not (score > 0): break
    # `swap_remove`: the last box takes the removed one's place.
    var b = boxes[bi]
    boxes[bi] = boxes[^1]
    boxes.setLen(boxes.len - 1)
    let ax = axis
    b.sort(proc(p, q: (array[3, float32], uint32)): int = totalCmp(p[0][ax], q[0][ax]))
    var total = 0'u64
    for p in b: total += uint64(p[1])
    var acc = 0'u64
    var cut = 1
    for i, p in b:
      acc += uint64(p[1])
      if acc * 2 >= total:
        cut = clamp(i + 1, 1, b.len - 1)
        break
    boxes.add b[0 ..< cut]
    boxes.add b[cut .. ^1]
  for b in boxes:
    var w = 0.0
    for p in b: w += float64(p[1])
    var m: array[3, float32]
    for a in 0 .. 2:
      var s = 0.0
      for p in b: s += float64(p[0][a]) * float64(p[1])
      m[a] = float32(s / w)
    result.add m

proc kmeans(pts: seq[(array[3, float32], uint32)]; centers: var seq[array[3, float32]]; iters: int) =
  ## Lloyd refinement of `centers` over weighted points.
  if centers.len == 0: return # no room left (the forced colours fill the palette)
  for _ in 0 ..< iters:
    let lookup = newNearest(centers)
    var sum = newSeq[array[3, float64]](centers.len)
    var cnt = newSeq[float64](centers.len)
    for (p, w) in pts:
      let i = lookup.find(p)
      for a in 0 .. 2: sum[i][a] += float64(p[a]) * float64(w)
      cnt[i] += float64(w)
    for i in 0 ..< centers.len:
      if cnt[i] > 0:
        for a in 0 .. 2: centers[i][a] = float32(sum[i][a] / cnt[i])

proc labToRgb8(lab: array[3, float32]): Rgb8 =
  let c = labToSrgb(lab)
  [to8(c[0]), to8(c[1]), to8(c[2])]

proc buildPalette*(px: openArray[array[4, float32]]; kind: PaletteKind; colors0: int; forced: Forced): seq[Rgb8] =
  ## Builds the palette for `px`. `colors` is the requested count (2..=256) for the computed kinds;
  ## forced colours are included first and count toward it. Raises `QuantizeError` when Exact
  ## meets more than 256 colours.
  let colors = clamp(colors0, 2, 256)
  result = forcedColors(forced)
  let room = max(colors - result.len, 0)
  # Fixed palettes, or computed palettes already filled by forced colours, need no scan.
  # Exact still counts every colour to enforce its 256-colour limit.
  let hist = (if kind == pkExact or (room > 0 and kind in {pkAdaptive, pkPerceptual, pkSelective}): histogram(px)
              else: @[])
  var computed: seq[Rgb8]
  case kind
  of pkExact:
    if hist.len > 256:
      raise newException(QuantizeError, "the image has " & $hist.len & " colours; Exact needs 256 or fewer")
    for h in hist: computed.add h[0]
  of pkSystemMac: computed = macPalette()
  of pkSystemWindows: computed = windowsPalette()
  of pkWeb: computed = webPalette()
  of pkUniform: computed = uniformPalette(max(room, 8))
  of pkAdaptive:
    if hist.len <= room:
      for h in hist: computed.add h[0]
    else:
      var pts: seq[(array[3, float32], uint32)]
      for (c, n) in hist: pts.add ([float32(c[0]), float32(c[1]), float32(c[2])], n)
      var centers = medianCut(pts, room)
      kmeans(pts, centers, 2)
      for c in centers:
        computed.add [asU8(clamp(round(c[0]), 0'f32, 255'f32)), asU8(clamp(round(c[1]), 0'f32, 255'f32)),
                      asU8(clamp(round(c[2]), 0'f32, 255'f32))]
  of pkPerceptual, pkSelective:
    if hist.len <= room:
      for h in hist: computed.add h[0]
    else:
      var pts: seq[(array[3, float32], uint32)]
      for (c, n) in hist:
        pts.add (srgbToLab([float32(c[0]) / 255'f32, float32(c[1]) / 255'f32, float32(c[2]) / 255'f32]), n)
      var centers = medianCut(pts, room)
      kmeans(pts, centers, 4)
      for c in centers: computed.add labToRgb8(c)
      if kind == pkSelective:
        # Favour web-safe colours: snap entries within ~10 levels of one.
        for c in computed.mitems:
          var snap: Rgb8
          var near = true
          for k in 0 .. 2:
            snap[k] = asU8(round(float32(c[k]) / 51'f32) * 51'f32)
            if abs(int(c[k]) - int(snap[k])) > 10: near = false
          if near: c = snap
  for c in computed:
    if result.len >= 256: break
    if c notin result: result.add c
  if kind in {pkAdaptive, pkPerceptual, pkSelective, pkUniform}:
    let keep = max(colors, forcedColors(forced).len)
    if result.len > keep: result.setLen(keep)
  if result.len == 0: result.add [0'u8, 0, 0]

# ---------------------------------------------------------------- mapping and dithering

proc bayer8*(x, y: int): float32 =
  ## 8×8 Bayer threshold in -0.5..0.5.
  const M = [0'u8, 32, 8, 40, 2, 34, 10, 42, 48, 16, 56, 24, 50, 18, 58, 26, 12, 44, 4, 36, 14, 46, 6, 38, 60, 28,
             52, 20, 62, 30, 54, 22, 3, 35, 11, 43, 1, 33, 9, 41, 51, 19, 59, 27, 49, 17, 57, 25, 15, 47, 7, 39, 13,
             45, 5, 37, 63, 31, 55, 23, 61, 29, 53, 21]
  (float32(M[(y mod 8) * 8 + x mod 8]) + 0.5'f32) / 64'f32 - 0.5'f32

proc hash01(x, y: int; seed: uint32): float32 =
  var h = (uint32(x and 0xFFFF_FFFF) * 0x8da6_b343'u32) xor (uint32(y and 0xFFFF_FFFF) * 0xd816_3841'u32) xor
          (seed * 0xcb1a_b31f'u32)
  h = h xor (h shr 13)
  h = h * 0x5bd1_e995'u32
  h = h xor (h shr 15)
  float32(h and 0xffff) / 65535'f32

proc quantizeWith(px: var openArray[array[4, float32]]; w: int; pal: openArray[Rgb8]; dither: Dither; amount: float32;
                  transparent: Option[int]; nearest: proc(c: array[3, float32]): int): seq[uint8] =
  let h = (if w > 0: px.len div w else: 0)
  result = newSeq[uint8](px.len)
  if pal.len == 0: return
  # Typical palette spacing sets the ordered/noise dither amplitude.
  let spread = rmin(1'f32 / rmax(rmath.cbrt(float32(pal.len)), 1'f32), 0.5'f32) * clamp(amount, 0'f32, 1'f32)
  template entry(k: int): array[3, float32] =
    [float32(pal[k][0]) / 255'f32, float32(pal[k][1]) / 255'f32, float32(pal[k][2]) / 255'f32]
  if dither != dtDiffusion:
    # None / Pattern / Noise dither have no cross-pixel dependency.
    for i in 0 ..< (if w > 0: h * w else: 0):
      let (x, y) = (i mod w, i div w)
      let pv = px[i]
      if pv[3] < 0.5'f32:
        if transparent.isSome:
          result[i] = uint8(transparent.get and 0xFF)
          px[i] = [0'f32, 0, 0, 0]
          continue
        if pv[3] <= 0: continue
      var c = [pv[0], pv[1], pv[2]]
      case dither
      of dtPattern:
        for q in 0 .. 2: c[q] = c[q] + bayer8(x, y) * spread
      of dtNoise:
        for q in 0 .. 2: c[q] = c[q] + (hash01(x, y, 7) - 0.5'f32) * spread
      else: discard
      for q in 0 .. 2: c[q] = clamp(c[q], 0'f32, 1'f32)
      let k = nearest(c)
      result[i] = uint8(k and 0xFF)
      let e = entry(k)
      px[i] = [e[0], e[1], e[2], (if transparent.isSome: 1'f32 else: pv[3])]
    return
  var err = newSeq[array[3, float32]](2 * (w + 2))
  let a = clamp(amount, 0'f32, 1'f32)
  for y in 0 ..< h:
    # Rows ping-pong: `cur` is row y, `next` row y + 1 (offset by one for x = -1).
    for k in 0 ..< w + 2:
      err[k] = err[w + 2 + k]
      err[w + 2 + k] = [0'f32, 0, 0]
    for x in 0 ..< w:
      let i = y * w + x
      let p = px[i]
      if p[3] < 0.5'f32:
        if transparent.isSome:
          result[i] = uint8(transparent.get and 0xFF)
          px[i] = [0'f32, 0, 0, 0]
          continue
        if p[3] <= 0: continue
      var c = [p[0], p[1], p[2]]
      for k in 0 .. 2: c[k] += err[x + 1][k]
      for q in 0 .. 2: c[q] = clamp(c[q], 0'f32, 1'f32)
      let k = nearest(c)
      result[i] = uint8(k and 0xFF)
      let e = entry(k)
      var d: array[3, float32]
      for q in 0 .. 2: d[q] = (c[q] - e[q]) * a
      let nx = w + 2
      for q in 0 .. 2:
        err[x + 2][q] += d[q] * 7'f32 / 16'f32
        err[nx + x][q] += d[q] * 3'f32 / 16'f32
        err[nx + x + 1][q] += d[q] * 5'f32 / 16'f32
        err[nx + x + 2][q] += d[q] / 16'f32
      px[i] = [e[0], e[1], e[2], (if transparent.isSome: 1'f32 else: p[3])]

proc quantize*(px: var openArray[array[4, float32]]; w: int; pal: openArray[Rgb8]; dither: Dither; amount: float32;
               transparent: Option[int]): seq[uint8] =
  ## Maps every opaque pixel to its palette entry (in place) with the chosen dither; returns the
  ## index per pixel. Pixels with alpha < ½ become index `transparent` (and alpha 0) when given;
  ## otherwise alpha is kept. `amount` (0..=1) scales diffusion / pattern / noise strength.
  var pts: seq[array[3, float32]]
  for c in pal: pts.add [float32(c[0]), float32(c[1]), float32(c[2])]
  let lookup = newNearest(pts)
  quantizeWith(px, w, pal, dither, amount, transparent,
               proc(c: array[3, float32]): int = lookup.find([c[0] * 255'f32, c[1] * 255'f32, c[2] * 255'f32]))

proc spot(shape: HalftoneShape; u, v: float32): float32 =
  ## Spot function in 0..=1 for a position (u, v) in -1..1 within a halftone cell.
  let s = case shape
    of hsRound: 1'f32 - (u * u + v * v) / 2'f32
    of hsEllipse: 1'f32 - (u * u + 0.6'f32 * v * v) / 1.6'f32
    of hsLine: 1'f32 - abs(v)
    of hsSquare: 1'f32 - rmax(abs(u), abs(v))
    of hsDiamond: 1'f32 - (abs(u) + abs(v)) / 2'f32
    of hsCross: 1'f32 - rmin(abs(u), abs(v))
  clamp(s, 0'f32, 1'f32)

proc toBitmap*(gray: var openArray[float32]; w: int; m: BitmapMethod) =
  ## Converts gray values (0..=1, row-major `w` wide) to 0/1 in place.
  let h = (if w > 0: gray.len div w else: 0)
  case m.kind
  of bmThreshold:
    for v in gray.mitems: v = (if v >= 0.5'f32: 1'f32 else: 0'f32)
  of bmPattern:
    if w <= 0: return
    for i in 0 ..< gray.len: gray[i] = (if gray[i] > bayer8(i mod w, i div w) + 0.5'f32: 1'f32 else: 0'f32)
  of bmDiffusion:
    var buf = @gray
    for y in 0 ..< h:
      for x in 0 ..< w:
        let i = y * w + x
        let old = buf[i]
        let nw = (if old >= 0.5'f32: 1'f32 else: 0'f32)
        let e = old - nw
        gray[i] = nw
        if x + 1 < w: buf[i + 1] += e * 7'f32 / 16'f32
        if y + 1 < h:
          if x > 0: buf[i + w - 1] += e * 3'f32 / 16'f32
          buf[i + w] += e * 5'f32 / 16'f32
          if x + 1 < w: buf[i + w + 1] += e / 16'f32
  of bmHalftone:
    if w <= 0: return
    let cell = rmax(m.cell, 2'f32)
    let ar = toRadians(m.angle)
    let (s, c) = (sin(ar), cos(ar))
    # Rank the spot function over the cell so coverage is proportional to the tone.
    var table = newSeq[float32](32 * 32)
    for k in 0 ..< table.len:
      table[k] = spot(m.shape, (float32(k mod 32) + 0.5'f32) / 16'f32 - 1'f32, (float32(k div 32) + 0.5'f32) / 16'f32 - 1'f32)
    table.sort(proc(a, b: float32): int = totalCmp(a, b))
    proc cdf(x: float32): float32 =
      var (lo, hi) = (0, table.len)
      while lo < hi:
        let mid = (lo + hi) div 2
        if table[mid] < x: lo = mid + 1 else: hi = mid
      float32(lo) / float32(table.len)
    for i in 0 ..< gray.len:
      let (x, y) = (float32(i mod w) + 0.5'f32, float32(i div w) + 0.5'f32)
      let (rx, ry) = (x * c + y * s, -x * s + y * c)
      let u = remEuclidF(rx / cell, 1'f32) * 2'f32 - 1'f32
      let vv = remEuclidF(ry / cell, 1'f32) * 2'f32 - 1'f32
      # Ink (black) covers where the spot function exceeds the gray level.
      gray[i] = (if cdf(spot(m.shape, u, vv)) >= gray[i]: 0'f32 else: 1'f32)
