## Bayer demosaicing (port of `raw/src/demosaic.rs`).
##
## * **Bilinear**: each missing colour is the mean of the same-colour neighbours in the 3×3 window.
## * **Malvar–He–Cutler**: H. S. Malvar, L.-W. He, R. Cutler, "High-quality linear interpolation
##   for demosaicing of Bayer-pattern color images", ICASSP 2004: bilinear plus a
##   gradient-correction term (5×5 kernels).
## * **AHD**: K. Hirakawa, T. W. Parks, "Adaptive homogeneity-directed demosaicing algorithm",
##   IEEE Trans. Image Processing 14(3), 2005: horizontally and vertically interpolated
##   candidates, chosen per pixel by their local homogeneity in CIELAB.
##
## Input is a `Padded` plane of normalized CFA samples (mirror-reflected borders, which keep the
## CFA parity, so every kernel reads in bounds); `phase` gives the colours (0 = R, 1 = G, 2 = B) of
## the 2×2 cell at the image origin, row major. Output is interleaved RGB.
##
## Other periodic CFAs (Fujifilm X-Trans' 6×6 pattern) go through `demosaicCfa`, a
## pattern-agnostic edge-directed interpolation:
##
## 1. Green at non-green sites: per axis (horizontal, vertical), the mean of the green neighbours
##    on that axis, weighted by the inverse square of the local gradient along it (mean absolute
##    difference of same-colour sample pairs on the axis within a 5×5 window), as in
##    gradient-weighted interpolation (e.g. W. Lu, Y.-P. Tan, "Color filter array demosaicking:
##    new method and performance measures", IEEE TIP 12(10), 2003).
## 2. Red and blue everywhere from colour differences: the green estimate plus the weighted mean
##    of (sample − green) at the same-colour sites of the 5×5 window, weighted by inverse squared
##    distance and by green similarity (so differences are not carried across edges).
##
## The pattern breaks under mirror padding when its period is not 2, so this path reads only
## interior samples and drops taps outside the image.

import std/[options, strutils, math]
import ../rmath
import ./color, ./sensor

type
  Demosaic* = enum
    ## Demosaicing algorithm. The Rust default is `dmAhd` (see `DefaultDemosaic`).
    dmBilinear ## fastest; soft, with colour fringes on fine detail
    dmMhc      ## Malvar–He–Cutler gradient-corrected linear interpolation: fast and sharp
    dmAhd      ## adaptive homogeneity-directed (Hirakawa–Parks): best edges, slower

const DefaultDemosaic* = dmAhd

proc id*(d: Demosaic): string =
  case d
  of dmBilinear: "bilinear"
  of dmMhc: "mhc"
  of dmAhd: "ahd"

proc demosaicFromId*(s: string): Option[Demosaic] =
  case s.toLowerAscii
  of "bilinear", "fast": some(dmBilinear)
  of "mhc", "malvar": some(dmMhc)
  of "ahd", "best": some(dmAhd)
  else: none(Demosaic)

const Pad* = 6 ## border width of a `Padded` plane (even, so the CFA parity is unchanged)

proc reflect(i0, n: int): int {.inline.} =
  ## Mirror-reflects `i` into `0..n` (n ≥ 1); clamps when `n` is tiny.
  if n <= 1: return 0
  var i = i0
  if i < 0: i = -i
  if i >= n: i = 2 * (n - 1) - i
  clamp(i, 0, n - 1)

type
  Padded* = object
    ## A `w × h` plane stored with `Pad` extra pixels on every side.
    data*: seq[float32]
    w*, h*, stride*: int

proc newPadded*(w, h: int): Padded =
  let stride = w + 2 * Pad
  Padded(data: newSeq[float32](stride * (h + 2 * Pad)), w: w, h: h, stride: stride)

proc fillBorders*(p: var Padded) =
  ## Fills the borders by mirror reflection of the interior.
  let (w, h, s) = (p.w, p.h, p.stride)
  if w == 0 or h == 0: return
  for y in Pad ..< Pad + h:
    for px in 0 ..< s:
      if px >= Pad and px < Pad + w: continue
      p.data[y * s + px] = p.data[y * s + Pad + reflect(px - Pad, w)]
  for py in 0 ..< h + 2 * Pad:
    if py >= Pad and py < Pad + h: continue
    let src = Pad + reflect(py - Pad, h)
    for k in 0 ..< s: p.data[py * s + k] = p.data[src * s + k]

proc paddedFromPlane*(plane: openArray[float32]; w, h: int): Padded =
  ## From an unpadded plane (tests and small inputs).
  result = newPadded(w, h)
  for y in 0 ..< h:
    let at = (y + Pad) * result.stride + Pad
    for x in 0 ..< w: result.data[at + x] = plane[y * w + x]
  result.fillBorders()

type Tap = (int, float32)

proc kernels(phase: array[4, uint8]; meth: Demosaic; stride: int): array[4, array[3, seq[Tap]]] =
  ## Per 2×2 position, per output colour: kernel taps as (offset, weight).
  proc color(x, y: int): uint8 = phase[(y and 1) * 2 + (x and 1)]
  let s = stride
  for pos in 0 .. 3:
    let (px, py) = (pos and 1, pos shr 1)
    let own = color(px, py)
    for ci in 0 .. 2:
      let c = uint8(ci)
      var taps: seq[(int, int, float32)]
      if c == own:
        taps = @[(0, 0, 1'f32)]
      elif meth == dmBilinear:
        var t: seq[(int, int)]
        for dy in -1 .. 1:
          for dx in -1 .. 1:
            if color(px + dx, py + dy) == c: t.add (dx, dy)
        let w = 1'f32 / float32(max(t.len, 1))
        for (dx, dy) in t: taps.add (dx, dy, w)
      elif c == 1:
        # Malvar–He–Cutler (weights / 8): G at R or B.
        taps = @[(0, 0, 4'f32), (-1, 0, 2'f32), (1, 0, 2'f32), (0, -1, 2'f32), (0, 1, 2'f32), (-2, 0, -1'f32),
                 (2, 0, -1'f32), (0, -2, -1'f32), (0, 2, -1'f32)]
      elif own == 1:
        # R or B at G: from the row neighbours or the column neighbours.
        let (ax, ay) = if color(px + 1, py) == c: (1, 0) else: (0, 1)
        taps = @[(0, 0, 5'f32), (-ax, -ay, 4'f32), (ax, ay, 4'f32), (-1, -1, -1'f32), (1, -1, -1'f32),
                 (-1, 1, -1'f32), (1, 1, -1'f32), (-2 * ax, -2 * ay, -1'f32), (2 * ax, 2 * ay, -1'f32),
                 (-2 * ay, -2 * ax, 0.5'f32), (2 * ay, 2 * ax, 0.5'f32)]
      else:
        # R at B or B at R.
        taps = @[(0, 0, 6'f32), (-1, -1, 2'f32), (1, -1, 2'f32), (-1, 1, 2'f32), (1, 1, 2'f32), (-2, 0, -1.5'f32),
                 (2, 0, -1.5'f32), (0, -2, -1.5'f32), (0, 2, -1.5'f32)]
      let norm = if meth == dmBilinear or c == own: 1'f32 else: 0.125'f32
      for (dx, dy, w) in taps: result[pos][ci].add (dy * s + dx, w * norm)

# ---------------------------------------------------------------- AHD

const
  LabfSteps = 1 shl 14
  AhdBand = 32 ## output rows per AHD band
  M = 3        ## extra band rows above and below the output rows

let
  labfRange = 2'f32
  labfScale = 8192'f32 ## LABF_STEPS / LABF_RANGE
  lab116 = 116'f32
  labOff = 16'f32 / lab116
  labEps = 0.008856'f32
  labSlope = 7.787'f32

proc labfExact*(t: float32): float32 =
  if t > labEps: rmath.cbrt(t) else: labSlope * t + labOff

type LabF* = object
  ## CIE f(t) for L*a*b*, tabulated on 0..2 with linear interpolation.
  table: seq[float32]

proc newLabF*(): LabF =
  result.table = newSeq[float32](LabfSteps + 2)
  for i in 0 .. LabfSteps + 1:
    result.table[i] = labfExact(float32(i) * labfRange / float32(LabfSteps))

proc lookup*(l: LabF; t: float32): float32 {.inline.} =
  if not (t >= 0'f32 and t < labfRange): return labfExact(rmax(t, 0'f32))
  let x = t * labfScale
  let i = min(asIdx(x), LabfSteps)
  let a = l.table[i]
  a + (l.table[i + 1] - a) * (x - float32(i))

proc labAt(l: seq[float32]; k: int): (float32, float32, float32) {.inline.} = (l[k * 3], l[k * 3 + 1], l[k * 3 + 2])

proc dist(a, c: (float32, float32, float32)): (float32, float32) {.inline.} =
  (abs(a[0] - c[0]), (a[1] - c[1]) * (a[1] - c[1]) + (a[2] - c[2]) * (a[2] - c[2]))

proc ahd(p: Padded; phase: array[4, uint8]; toXyz: Mat3; outp: var seq[float32]) =
  let (w, s) = (p.w, p.stride)
  template d(i: int): float32 = p.data[i]
  # Colour at padded coordinates (Pad is even, so the phase is unchanged).
  template color(px, py: int): uint8 = phase[(py and 1) * 2 + (px and 1)]
  let white = [0.9642'f32, 1'f32, 0.8249'f32]
  var m: array[3, array[3, float32]]
  for i in 0 .. 2:
    for j in 0 .. 2: m[i][j] = float32(toXyz[i][j]) / white[i]
  let labf = newLabF()
  let totalRows = outp.len div (w * 3)
  var band = 0
  while band * AhdBand < totalRows:
    let rows = min(AhdBand, totalRows - band * AhdBand)
    let n = rows + 2 * M
    # Band row b ↔ padded row py(b); padded columns are used as is.
    template py(b: int): int = band * AhdBand + Pad - M + b
    # 1. Green interpolated horizontally (0) and vertically (1), columns 2..s-2.
    var g = [newSeq[float32](n * s), newSeq[float32](n * s)]
    for b in 0 ..< n:
      let y = py(b)
      for x in 2 ..< s - 2:
        let i = y * s + x
        let c = d(i)
        let k = b * s + x
        if color(x, y) == 1:
          g[0][k] = c
          g[1][k] = c
          continue
        let (l, r) = (d(i - 1), d(i + 1))
        let gh = (l + r) * 0.5'f32 + (2'f32 * c - d(i - 2) - d(i + 2)) * 0.25'f32
        g[0][k] = clamp(gh, rmin(l, r), rmax(l, r))
        let (u, dn) = (d(i - s), d(i + s))
        let gv = (u + dn) * 0.5'f32 + (2'f32 * c - d(i - 2 * s) - d(i + 2 * s)) * 0.25'f32
        g[1][k] = clamp(gv, rmin(u, dn), rmax(u, dn))
    # 2. RGB candidates and their Lab, band rows 1..n-1, columns 3..s-3.
    var rgb = [newSeq[float32](n * s * 3), newSeq[float32](n * s * 3)]
    var lab = [newSeq[float32](n * s * 3), newSeq[float32](n * s * 3)]
    for dir in 0 .. 1:
      # Colour difference (sample − green) at band row b, column x.
      template diff(b, x: int): float32 = d(py(b) * s + x) - g[dir][(b) * s + x]
      for b in 1 ..< n - 1:
        let y = py(b)
        for x in 3 ..< s - 3:
          let own = color(x, y)
          let gv = g[dir][b * s + x]
          var px = [gv, gv, gv]
          if own == 1:
            let hc = min(int(color(x + 1, y)), 2)
            px[hc] = gv + 0.5'f32 * (diff(b, x - 1) + diff(b, x + 1))
            px[2 - hc] = gv + 0.5'f32 * (diff(b - 1, x) + diff(b + 1, x))
          else:
            let o = min(int(own), 2)
            px[o] = d(y * s + x)
            px[2 - o] = gv + 0.25'f32 * (diff(b - 1, x - 1) + diff(b - 1, x + 1) + diff(b + 1, x - 1) + diff(b + 1, x + 1))
          for k in 0 .. 2: px[k] = clamp(px[k], 0'f32, 1'f32)
          let i = (b * s + x) * 3
          rgb[dir][i] = px[0]
          rgb[dir][i + 1] = px[1]
          rgb[dir][i + 2] = px[2]
          var xyz: array[3, float32]
          for k in 0 .. 2: xyz[k] = m[k][0] * px[0] + m[k][1] * px[1] + m[k][2] * px[2]
          let (fx, fy, fz) = (labf.lookup(xyz[0]), labf.lookup(xyz[1]), labf.lookup(xyz[2]))
          lab[dir][i] = lab116 * fy - 16'f32
          lab[dir][i + 1] = 500'f32 * (fx - fy)
          lab[dir][i + 2] = 200'f32 * (fy - fz)
    # 3. Homogeneity maps, band rows 2..n-2, columns 4..s-4.
    var hom = [newSeq[uint8](n * s), newSeq[uint8](n * s)]
    for b in 2 ..< n - 2:
      for x in 4 ..< s - 4:
        let k = b * s + x
        let nb = [k - 1, k + 1, k - s, k + s]
        let (ch, cv) = (labAt(lab[0], k), labAt(lab[1], k))
        var dh, dv: array[4, (float32, float32)]
        for q in 0 .. 3:
          dh[q] = dist(ch, labAt(lab[0], nb[q]))
          dv[q] = dist(cv, labAt(lab[1], nb[q]))
        # The horizontal candidate judged along the row, the vertical along the column.
        let epsL = rmin(rmax(dh[0][0], dh[1][0]), rmax(dv[2][0], dv[3][0]))
        let epsC = rmin(rmax(dh[0][1], dh[1][1]), rmax(dv[2][1], dv[3][1]))
        var ch0, cv0 = 0'u8
        for q in 0 .. 3:
          if dh[q][0] <= epsL and dh[q][1] <= epsC: inc ch0
          if dv[q][0] <= epsL and dv[q][1] <= epsC: inc cv0
        hom[0][k] = ch0
        hom[1][k] = cv0
    # 4. Per pixel, the candidate with the larger 3×3 homogeneity sum.
    for r in 0 ..< rows:
      let b = r + M
      for x in 0 ..< w:
        let px = x + Pad
        var sh, sv = 0'u32
        for bb in b - 1 .. b + 1:
          for xx in px - 1 .. px + 1:
            sh += uint32(hom[0][bb * s + xx])
            sv += uint32(hom[1][bb * s + xx])
        let i = (b * s + px) * 3
        let o = ((band * AhdBand + r) * w + x) * 3
        for k in 0 .. 2:
          outp[o + k] = if sh > sv: rgb[0][i + k]
                        elif sh < sv: rgb[1][i + k]
                        else: 0.5'f32 * (rgb[0][i + k] + rgb[1][i + k])
    inc band

proc demosaic*(p: Padded; phase: array[4, uint8]; meth: Demosaic; toXyz: Mat3): seq[float32] =
  ## Demosaics a padded CFA plane into interleaved RGB (`w × h × 3`).
  let (w, h, s) = (p.w, p.h, p.stride)
  result = newSeq[float32](w * h * 3)
  if w == 0 or h == 0 or p.data.len < s * (h + 2 * Pad): return
  if meth == dmAhd:
    ahd(p, phase, toXyz, result)
    return
  let ks = kernels(phase, meth, s)
  template d(i: int): float32 = p.data[i]
  for y in 0 ..< h:
    let base = (y + Pad) * s + Pad
    for x in 0 ..< w:
      let pos = (y and 1) * 2 + (x and 1)
      let i = base + x
      let o = (y * w + x) * 3
      if meth == dmBilinear:
        # Direct Bayer bilinear (the kernels' result without the tap loops).
        let own = min(int(phase[pos]), 2)
        let (nn, so, e, wv) = (d(i - s), d(i + s), d(i + 1), d(i - 1))
        if own == 1:
          let hc = min(int(phase[(y and 1) * 2 + ((x + 1) and 1)]), 2)
          result[o + 1] = d(i)
          result[o + hc] = (e + wv) * 0.5'f32
          result[o + 2 - hc] = (nn + so) * 0.5'f32
        else:
          result[o + own] = d(i)
          result[o + 1] = (nn + so + e + wv) * 0.25'f32
          result[o + 2 - own] = (d(i - s - 1) + d(i - s + 1) + d(i + s - 1) + d(i + s + 1)) * 0.25'f32
        continue
      for c in 0 .. 2:
        var v = 0'f32
        for (off, wt) in ks[pos][c]: v += wt * d(i + off)
        result[o + c] = clamp(v, 0'f32, 1'f32)

proc bilinearKernelValue*(p: Padded; phase: array[4, uint8]; x, y, c: int): float32 =
  ## The generic bilinear kernel evaluation at (`x`, `y`) (tests compare the fast path to it).
  let ks = kernels(phase, dmBilinear, p.stride)
  let i = (y + Pad) * p.stride + Pad + x
  for (off, wt) in ks[(y and 1) * 2 + (x and 1)][c]: result += wt * p.data[i + off]

# ---------------------------------------------------------------- generic CFA

type
  Pattern* = object
    ## A CFA repeat as seen from the plane's origin: colour of plane pixel (x, y) is
    ## `colors[(y mod h) * w + x mod w]`.
    w*, h*: int
    colors*: seq[uint8]

proc patternOf*(cfa: Cfa; x0, y0: int): Pattern =
  ## The repeat of `cfa` starting at data coordinates (`x0`, `y0`).
  let (w, h) = (clamp(cfa.width, 1, 16), clamp(cfa.height, 1, 16))
  result = Pattern(w: w, h: h, colors: newSeq[uint8](w * h))
  for i in 0 ..< w * h: result.colors[i] = min(cfa.color(x0 + i mod w, y0 + i div w), 2'u8)

proc at*(p: Pattern; x, y: int): uint8 =
  let (w, h) = (max(p.w, 1), max(p.h, 1))
  let i = floorMod(y, h) * w + floorMod(x, w)
  if i >= 0 and i < p.colors.len: p.colors[i] else: 1'u8

const Radius = 2 ## tap radius of the generic demosaic (a 5×5 window)

type
  Off = (int, int)
  Axis = object
    ## One interpolation axis at a non-green site.
    near: seq[Off]                 ## green neighbours at distance 1 on the axis
    pairList: seq[(Off, Off, float32)] ## same-colour sample pairs along the axis (a, b, 1 / distance)
  Taps = object
    ## The taps of one CFA phase.
    own: uint8
    axes: array[2, Axis]
    greens: seq[Off]               ## greens of the 3×3 (or 5×5) window, when no axis has one
    sites: array[3, seq[(Off, float32)]] ## per colour, its window sites with weight 1 / distance²

proc phaseTaps(pat: Pattern; px, py: int): Taps =
  proc col(dx, dy: int): uint8 = pat.at(px + dx, py + dy)
  var window: seq[Off]
  for dy in -Radius .. Radius:
    for dx in -Radius .. Radius: window.add (dx, dy)
  for ai, (ax, ay) in [(1, 0), (0, 1)]:
    var a: Axis
    for (dx, dy) in [(ax, ay), (-ax, -ay)]:
      if col(dx, dy) == 1: a.near.add (dx, dy)
    for (dx, dy) in window:
      # Pairs on the axis line or the lines next to it.
      if (ax == 1 and abs(dy) > 1) or (ay == 1 and abs(dx) > 1): continue
      for k in 1 .. 2:
        let (bx, by) = (dx + k * ax, dy + k * ay)
        if abs(bx) <= Radius and abs(by) <= Radius and col(dx, dy) == col(bx, by):
          a.pairList.add ((dx, dy), (bx, by), 1'f32 / float32(k))
    result.axes[ai] = a
  proc ring(r: int): seq[Off] =
    for (dx, dy) in window:
      if abs(dx) <= r and abs(dy) <= r and (dx, dy) != (0, 0) and col(dx, dy) == 1: result.add (dx, dy)
  result.greens = ring(1)
  if result.greens.len == 0: result.greens = ring(Radius)
  for c in 0'u8 .. 2'u8:
    for (dx, dy) in window:
      if (dx, dy) != (0, 0) and col(dx, dy) == c: result.sites[int(c)].add ((dx, dy), 1'f32 / float32(dx * dx + dy * dy))
  result.own = col(0, 0)

let
  gradEps = 1'f32 / 256'f32 ## regularizes the inverse-gradient weights (about 1/256 of full scale)
  diffEps = 1'f32 / 64'f32  ## regularizes the green-similarity weights of the colour differences

proc sampleAt(plane: openArray[float32]; stride, pad, w, h, x, y: int; o: Off; v: var float32): bool {.inline.} =
  ## Sample at (x + dx, y + dy); false outside the image.
  let (xx, yy) = (x + o[0], y + o[1])
  if xx < 0 or yy < 0 or xx >= w or yy >= h: return false
  let i = (yy + pad) * stride + pad + xx
  if i < 0 or i >= plane.len: return false
  v = plane[i]
  true

proc demosaicCfa*(p: Padded; pat: Pattern): seq[float32] =
  ## Demosaics a padded plane with any periodic CFA (see the module docs) into interleaved RGB.
  ## There is one algorithm for every `Demosaic` choice: without the edge-directed weights it is
  ## no faster and leaves zipper artefacts along every edge of an X-Trans image.
  let (w, h, s) = (p.w, p.h, p.stride)
  result = newSeq[float32](w * h * 3)
  if w == 0 or h == 0 or pat.w == 0 or pat.h == 0 or pat.colors.len < pat.w * pat.h or p.data.len < s * (h + 2 * Pad):
    return
  var taps: seq[Taps]
  for i in 0 ..< pat.w * pat.h: taps.add phaseTaps(pat, i mod pat.w, i div pat.w)
  template raw(x, y: int; o: Off; v: untyped): bool = sampleAt(p.data, s, Pad, w, h, x, y, o, v)

  # 1. Green everywhere.
  var green = newSeq[float32](w * h)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let ti = (y mod pat.h) * pat.w + x mod pat.w
      template t: untyped = taps[ti]
      var v = 0'f32
      discard raw(x, y, (0, 0), v)
      if t.own == 1:
        green[y * w + x] = v
        continue
      var num, den = 0'f32
      for a in t.axes:
        var sum = 0'f32
        var n = 0'u32
        for o in a.near:
          var q: float32
          if raw(x, y, o, q):
            sum += q
            inc n
        if n == 0: continue
        var grad, m = 0'f32
        for (oa, ob, k) in a.pairList:
          var va, vb: float32
          if raw(x, y, oa, va) and raw(x, y, ob, vb):
            grad += abs(va - vb) * k
            m += 1'f32
        let gr = if m > 0'f32: grad / m else: 0'f32
        let wt = 1'f32 / ((gradEps + gr) * (gradEps + gr))
        num += wt * sum / float32(n)
        den += wt
      if den > 0'f32:
        green[y * w + x] = num / den
      else:
        var sum = 0'f32
        var n = 0'u32
        for o in t.greens:
          var q: float32
          if raw(x, y, o, q):
            sum += q
            inc n
        green[y * w + x] = if n > 0: sum / float32(n) else: v

  # 2. Red and blue from colour differences.
  for y in 0 ..< h:
    for x in 0 ..< w:
      let ti = (y mod pat.h) * pat.w + x mod pat.w
      template t: untyped = taps[ti]
      let g = green[y * w + x]
      var v = 0'f32
      discard raw(x, y, (0, 0), v)
      let o = (y * w + x) * 3
      result[o + 1] = g
      for c in [0, 2]:
        if int(t.own) == c:
          result[o + c] = v
          continue
        var num, den = 0'f32
        for (off, base) in t.sites[c]:
          var vq, gq: float32
          if raw(x, y, off, vq) and sampleAt(green, w, 0, w, h, x, y, off, gq):
            let wt = base / (diffEps + abs(gq - g))
            num += wt * (vq - gq)
            den += wt
        result[o + c] = if den > 0'f32: g + num / den else: g
      for k in 0 .. 2: result[o + k] = clamp(result[o + k], 0'f32, 1'f32)
