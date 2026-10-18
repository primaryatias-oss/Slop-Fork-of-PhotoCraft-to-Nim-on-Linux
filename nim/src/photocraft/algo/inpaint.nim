## Hole filling for Content-Aware Fill and Spot Healing: patch-based completion, proximity match
## and texture synthesis (port of `inpaint.rs` and `inpaint/proximity_integral.rs`).
##
## * `complete`, content-aware fill: the multi-scale "space-time completion" EM scheme of
##   Y. Wexler, E. Shechtman, M. Irani, *Space-Time Completion of Video*, IEEE TPAMI 2007, with the
##   nearest-neighbour field computed by C. Barnes, E. Shechtman, A. Finkelstein, D. B. Goldman,
##   *PatchMatch*, SIGGRAPH 2009 (random init, propagation, exponentially shrinking random search).
##   Each EM iteration finds, for every patch overlapping the hole, its most similar source patch,
##   then re-estimates hole pixels by a similarity-weighted vote of the overlapping matches. Coarse
##   to fine over an image pyramid: the coarsest level starts by filling the hole from its edge
##   inwards, ring by ring ("onion peel"), each pixel from the patch that best matches what is
##   known around it; every finer level starts from the coarser level's matches scaled up and
##   voted with the finer level's own pixels. With colour adaptation the copies come with a gain
##   and bias (S. Darabi et al., *Image Melding*, SIGGRAPH 2012), within limits (`Adapt`).
## * `bestOffset`, proximity match: the single displacement whose surrounding ring best matches
##   the ring around the hole (sum of squared differences), i.e. an automatic Healing Brush source.
## * `synthesize`, create texture: patch-based texture synthesis in the style of A. Efros,
##   W. Freeman, *Image Quilting for Texture Synthesis and Transfer*, SIGGRAPH 2001.
##
## All functions take interleaved `w × h × ch` normalised floats (any colour model/depth) and a
## `w × h` hole mask, and return a full buffer equal to the input outside the hole. They are
## deterministic (fixed-seed hash RNG); the Rust crate's parallel passes give the same results
## as these sequential ones.

import std/[math, options, algorithm]
import ../raster
import ./util, ./poisson

const SourceMomentsMax = 1 shl 20 ## largest level (in cells) whose source patches' moments are kept
const SourceBlock = 64             ## columns per block of `Sources`' index
const MomCh = 8                    ## most channels whose moments the colour adaptation fits
const Lanes = 4                    ## sources `knownSseLanes` sums at once
const NoneU32 = high(uint32)

type
  IRng* = object
    ## SplitMix64 (the inpainting variant: the seed is mixed with the increment first).
    state: uint64

  Adapt* = object
    ## How far a copied patch may be adjusted to its target, per channel: `gain · source + bias`,
    ## the gain within `gain` and the bias within `±bias` (Image Melding's photometric transform).
    gain*: (float32, float32)
    bias*: float32

  CompleteParams* = object
    ## Parameters for `complete`.
    patchRadius*: int ## patches are `2r+1` square
    emIters*: int     ## EM iterations at the coarsest level (finer levels use fewer)
    minEmIters*: int
    pmIters*: int     ## PatchMatch passes per EM iteration
    refineExtent*: int
      ## levels where the hole is wider than this only refine the scaled-up match field
    adapt*: Adapt     ## colour adaptation of the copies
    seed*: uint64

  Px* = enum
    ## What a pixel is to `completeMasked`.
    pxVoid   ## not part of the image: ignored
    pxKnown  ## known, part of the context the fill must agree with, but not copied from
    pxSource ## known and may be copied from
    pxHole   ## to be filled

  Level = object
    w, h: int
    img: seq[float32]
    px: seq[Px]

  Bounds* = object
    ## The half-open box `[x0, x1) × [y0, y1)` of a level's grid.
    x0*, y0*, x1*, y1*: int

  Sources* = object
    ## The valid source centres in row-major order, indexed by counts per row and per block of
    ## `SourceBlock` columns (so the `k`-th is found without listing them all).
    rowStart: seq[int]
    blockStart: seq[uint32]
    blocks: int

  Coarser = object
    w, h: int
    img: seq[float32]
    field: seq[(int32, int32)]
    tbox: Bounds

  Solver* = object
    ## Per-level PatchMatch/EM state (exported for tests).
    w*, h*, ch*: int
    r*: int32
    img*: seq[float32]
    px*: seq[Px]
    valid*: seq[bool]
    holeBox*, tbox*: Bounds
    targets*: seq[bool]
    full*: seq[bool]
    tmom: seq[array[2, float32]]
    smom: seq[array[2, float32]]
    adapt*: Adapt
    ctl: Interrupt

  Best = object
    v: float32
    j: int
    worse, equal: float32

  HoleIntegral = object
    x0, y0: int
    w, h: int
    data: seq[uint32]

const AdaptNone* = Adapt(gain: (1'f32, 1'f32), bias: 0'f32) ## copies are taken as they are

proc isNone*(a: Adapt): bool = a.gain == (1'f32, 1'f32) and a.bias == 0'f32

proc defaultCompleteParams*(): CompleteParams =
  CompleteParams(patchRadius: 3, emIters: 10, minEmIters: 3, pmIters: 2, refineExtent: 192, adapt: AdaptNone, seed: 1)

# ---------------------------------------------------------------- rng, bounds

proc newIRng*(seed: uint64): IRng = IRng(state: seed xor 0x9E37_79B9_7F4A_7C15'u64)

proc next*(r: var IRng): uint64 =
  r.state += 0x9E37_79B9_7F4A_7C15'u64
  var z = r.state
  z = (z xor (z shr 30)) * 0xBF58_476D_1CE4_E5B9'u64
  z = (z xor (z shr 27)) * 0x94D0_49BB_1331_11EB'u64
  z xor (z shr 31)

proc skip*(r: var IRng; n: uint64) =
  ## Skip `n` numbers (the same state as after `n` calls of `next`).
  r.state += n * 0x9E37_79B9_7F4A_7C15'u64

proc range*(r: var IRng; lo, hi: int32): int32 =
  ## Uniform integer in `lo..=hi`.
  if hi <= lo: return lo
  lo + int32(r.next mod uint64(int64(hi) - int64(lo) + 1))

proc grow(b: Bounds; r, w, h: int): Bounds =
  Bounds(x0: max(b.x0 - r, 0), y0: max(b.y0 - r, 0), x1: min(b.x1 + r, w), y1: min(b.y1 + r, h))

proc half(b: Bounds): Bounds = Bounds(x0: b.x0 div 2, y0: b.y0 div 2, x1: (b.x1 + 1) div 2, y1: (b.y1 + 1) div 2)
proc width*(b: Bounds): int = max(b.x1 - b.x0, 0)
proc height*(b: Bounds): int = max(b.y1 - b.y0, 0)
proc area*(b: Bounds): int = b.width * b.height

# ---------------------------------------------------------------- f32 helpers

proc nextUp*(v: float32): float32 =
  ## Rust's `f32::next_up`.
  let bits = cast[uint32](v)
  if v != v or bits == 0x7F80_0000'u32: return v
  let a = bits and 0x7FFF_FFFF'u32
  cast[float32](if a == 0: 1'u32 elif bits == a: bits + 1 else: bits - 1)

proc nextDown*(v: float32): float32 =
  ## Rust's `f32::next_down`.
  let bits = cast[uint32](v)
  if v != v or bits == 0xFF80_0000'u32: return v
  let a = bits and 0x7FFF_FFFF'u32
  cast[float32](if a == 0: 0x8000_0001'u32 elif bits == a: bits - 1 else: bits + 1)

# ---------------------------------------------------------------- pyramid, tables

proc downsample(l: Level; ch: int): Level =
  ## Half-size level: a cell is a hole if any of its pixels is; else it averages its non-void
  ## pixels, and may be copied from only if all of them may.
  let (w, h) = ((l.w + 1) div 2, (l.h + 1) div 2)
  result = Level(w: w, h: h, img: newSeq[float32](w * h * ch), px: newSeq[Px](w * h))
  for y in 0 ..< h:
    for x in 0 ..< w:
      var n = 0'f32
      let o = (y * w + x) * ch
      var (hole, allSource, any) = (false, true, false)
      for (dx, dy) in [(0, 0), (1, 0), (0, 1), (1, 1)]:
        let (sx, sy) = (x * 2 + dx, y * 2 + dy)
        if sx >= l.w or sy >= l.h: continue
        let si = sy * l.w + sx
        case l.px[si]
        of pxHole: hole = true
        of pxVoid: allSource = false
        else:
          any = true
          allSource = allSource and l.px[si] == pxSource
          for c in 0 ..< ch: result.img[o + c] += l.img[si * ch + c]
          n += 1
      if n > 0'f32:
        for c in 0 ..< ch: result.img[o + c] /= n
      result.px[y * w + x] = (if hole: pxHole elif not any: pxVoid elif allSource: pxSource else: pxKnown)

proc integral(w, h: int; m: openArray[bool]): seq[uint32] =
  ## Summed-area table of a boolean grid (`(w+1) × (h+1)`).
  result = newSeq[uint32]((w + 1) * (h + 1))
  for y in 0 ..< h:
    var row = 0'u32
    for x in 0 ..< w:
      row += uint32(m[y * w + x])
      result[(y + 1) * (w + 1) + x + 1] = result[y * (w + 1) + x + 1] + row

proc windowCount(s: openArray[uint32]; gw, gh: int; x0i, y0i, x1i, y1i: int): uint32 =
  ## Count of set cells in the clipped window `[x0, x1) × [y0, y1)`.
  let (x0, y0) = (max(x0i, 0), max(y0i, 0))
  let (x1, y1) = (min(max(x1i, 0), gw), min(max(y1i, 0), gh))
  if x1 <= x0 or y1 <= y0: return 0
  let w1 = gw + 1
  s[y1 * w1 + x1] + s[y0 * w1 + x0] - s[y0 * w1 + x1] - s[y1 * w1 + x0]

proc holeBbox(w, h: int; hole: openArray[bool]): Option[(int, int, int, int)] =
  var b = none((int, int, int, int))
  for y in 0 ..< h:
    for x in 0 ..< w:
      if hole[y * w + x]:
        b = (if b.isNone: some((x, y, x + 1, y + 1))
             else: some((min(b.get[0], x), min(b.get[1], y), max(b.get[2], x + 1), max(b.get[3], y + 1))))
  b

proc holeBounds(w, h: int; px: openArray[Px]): Option[Bounds] =
  ## Bounds of the hole pixels of a role grid.
  var b = none(Bounds)
  for y in 0 ..< h:
    var (x0, x1) = (-1, -1)
    for x in 0 ..< w:
      if px[y * w + x] == pxHole:
        if x0 < 0: x0 = x
        x1 = x + 1
    if x0 < 0: continue
    b = (if b.isNone: some(Bounds(x0: x0, y0: y, x1: x1, y1: y + 1))
         else: some(Bounds(x0: min(b.get.x0, x0), y0: b.get.y0, x1: max(b.get.x1, x1), y1: y + 1)))
  b

proc validSources*(w, h, r: int; px: openArray[Px]; ctl: Interrupt): Option[seq[bool]] =
  ## Valid source centres: the whole `2r+1` patch inside the grid and copyable; none when
  ## cancelled.
  const Band = 64
  let side = 2 * r + 1
  var run = newSeq[bool](w * h)
  if w < side or h < side: return some(run)
  for y in 0 ..< h:
    if y mod Band == 0 and ctl.cancelled: return none(seq[bool])
    let o = y * w
    template copyable(x: int): int = int(px[o + x] == pxSource)
    var count = 0
    for x in 0 ..< side: count += copyable(x)
    run[o + r] = count == side
    for x in r + 1 ..< w - r:
      count = count + copyable(x + r) - copyable(x - r - 1)
      run[o + x] = count == side
  var valid = newSeq[bool](w * h)
  var y0 = 0
  while y0 < h:
    if ctl.cancelled: return none(seq[bool])
    let rows = min(Band, h - y0)
    var stack = newSeq[int](w)
    for yy in max(y0 - r, 0) ..< min(y0 + rows + r, h):
      for x in 0 ..< w: stack[x] = (if run[yy * w + x]: stack[x] + 1 else: 0)
      if yy < y0 + r or yy < 2 * r: continue
      let y = yy - r
      for x in r ..< w - r: valid[y * w + x] = stack[x] >= side
    y0 += Band
  some(valid)

proc newSources*(valid: openArray[bool]; w: int): Sources =
  let h = (if w > 0: valid.len div w else: 0)
  let blocks = (w + SourceBlock - 1) div SourceBlock
  result.blocks = blocks
  result.blockStart = newSeq[uint32](h * blocks)
  result.rowStart = newSeqOfCap[int](h + 1)
  result.rowStart.add 0
  var total = 0
  for y in 0 ..< h:
    var n = 0'u32
    for b in 0 ..< blocks:
      result.blockStart[y * blocks + b] = n
      for x in b * SourceBlock ..< min((b + 1) * SourceBlock, w):
        if valid[y * w + x]: n += 1
    total += int(n)
    result.rowStart.add total

proc len*(s: Sources): int = (if s.rowStart.len > 0: s.rowStart[^1] else: 0)

proc partitionPoint[T](v: openArray[T]; k: int): int =
  ## The number of leading entries `≤ k` of a non-decreasing list.
  var (lo, hi) = (0, v.len)
  while lo < hi:
    let mid = (lo + hi) div 2
    if int(v[mid]) <= k: lo = mid + 1 else: hi = mid
  lo

proc nth*(s: Sources; k: int; valid: openArray[bool]; w: int): (int32, int32) =
  ## The `k`-th valid centre in row-major order (`k < len`).
  let y = max(partitionPoint(s.rowStart, k) - 1, 0)
  var left = max(k - (if y < s.rowStart.len: s.rowStart[y] else: 0), 0)
  let rb0 = y * s.blocks
  let rowBlocks = (if rb0 + s.blocks <= s.blockStart.len: s.blockStart[rb0 ..< rb0 + s.blocks] else: @[])
  let b = max(partitionPoint(rowBlocks, left) - 1, 0)
  left = max(left - (if b < rowBlocks.len: int(rowBlocks[b]) else: 0), 0)
  let x0 = b * SourceBlock
  for x in 0 ..< max(w - x0, 0):
    let i = y * w + x0 + x
    if i >= valid.len: break
    if valid[i]:
      if left == 0: return (int32(x0 + x), int32(y))
      dec left
  (0'i32, 0'i32)

proc all*(s: Sources; valid: openArray[bool]; w: int): seq[(int32, int32)] =
  ## All of them, in row-major order.
  result = newSeqOfCap[(int32, int32)](s.len)
  for i, v in valid:
    if v: result.add (int32(i mod w), int32(i div w))

proc targetsAround(w, r: int; px: openArray[Px]; hb, tb: Bounds): seq[bool] =
  ## Per cell of `tb`: is it a target centre (its patch overlaps the hole)?
  let (bw, bh) = (hb.width, hb.height)
  var s = newSeq[uint32]((bw + 1) * (bh + 1))
  for y in 0 ..< bh:
    var row = 0'u32
    for x in 0 ..< bw:
      row += uint32(px[(hb.y0 + y) * w + hb.x0 + x] == pxHole)
      s[(y + 1) * (bw + 1) + x + 1] = s[y * (bw + 1) + x + 1] + row
  result = newSeq[bool](tb.area)
  let (ox, oy) = (hb.x0, hb.y0)
  for y in tb.y0 ..< tb.y1:
    for x in tb.x0 ..< tb.x1:
      let k = (y - tb.y0) * tb.width + (x - tb.x0)
      result[k] = windowCount(s, bw, bh, x - r - ox, y - r - oy, x + r + 1 - ox, y + r + 1 - oy) > 0

proc fieldAt(c: Coarser; x, y: int): (int32, int32) =
  ## The match of cell `(x, y)` (`low(int32)` outside the target box).
  let t = c.tbox
  if x < t.x0 or x >= t.x1 or y < t.y0 or y >= t.y1: return (low(int32), low(int32))
  let i = (y - t.y0) * t.width + (x - t.x0)
  if i < c.field.len: c.field[i] else: (low(int32), low(int32))

# ---------------------------------------------------------------- matching

proc fit*(a: Adapt; n, st, stt, ss, sss: float32): (float32, float32) {.inline.} =
  ## Gain and bias of one channel for target moments `(Σt, Σt²)` and source moments `(Σs, Σs²)`
  ## over `n` samples, within the limits.
  let (mt, ms) = (st / n, ss / n)
  let vt = rmax(stt / n - mt * mt, 0'f32)
  let vs = rmax(sss / n - ms * ms, 0'f32)
  var g = (if vs > 1e-10'f32: sqrt(vt / vs) else: 1'f32)
  g = clamp(g, a.gain[0], a.gain[1])
  let b = clamp(mt - g * ms, -a.bias, a.bias)
  (g, b)

proc patchMoments(img: openArray[float32]; w, ch, c8, side, x0, y0: int; outp: var openArray[array[2, float32]]) =
  ## `Σv, Σv²` per channel of the `side × side` patch with top-left `(x0, y0)`.
  for o in outp.mitems: o = [0'f32, 0'f32]
  for y in y0 ..< y0 + side:
    for x in x0 ..< x0 + side:
      let p = (y * w + x) * ch
      for c in 0 ..< c8:
        let a = img[p + c]
        outp[c][0] += a
        outp[c][1] += a * a

proc adaptedCost(a: Adapt; nf: float32; m: openArray[array[5, float32]]): float32 =
  ## The adapted distance from the moments `[Σt, Σt², Σs, Σs², Σts]` of each channel over `n`
  ## samples: what is left of the target after the source is fitted to it, per sample.
  var sum = 0'f32
  for mc in m:
    let (g, b) = fit(a, nf, mc[0], mc[1], mc[2], mc[3])
    sum += mc[1] + g * g * mc[3] + nf * b * b - 2'f32 * g * mc[4] - 2'f32 * b * mc[0] + 2'f32 * g * b * mc[2]
  rmax(sum, 0'f32) / (nf * float32(m.len))

proc isValid*(s: Solver; x, y: int32): bool {.inline.} =
  x >= 0 and y >= 0 and int(x) < s.w and int(y) < s.h and s.valid[int(y) * s.w + int(x)]

proc prepare*(s: var Solver; keepSourceMoments: bool) =
  ## Mark the full target patches (`full`), and keep the source moments where they pay off.
  let (w, h, r, tb) = (s.w, s.h, int(s.r), s.tbox)
  let side = 2 * r + 1
  s.full = newSeq[bool](tb.area)
  for ty in 0 ..< tb.height:
    let y = tb.y0 + ty
    if y < r or y + r >= h: continue
    for tx in 0 ..< tb.width:
      let x = tb.x0 + tx
      if x < r or x + r >= w: continue
      var ok = true
      for yy in y - r .. y + r:
        for xx in x - r ..< x - r + side:
          if s.px[yy * w + xx] == pxVoid:
            ok = false
            break
        if not ok: break
      s.full[ty * tb.width + tx] = ok
  if s.adapt.isNone: return
  let c8 = min(s.ch, MomCh)
  s.tmom = newSeq[array[2, float32]](tb.area * c8)
  if not keepSourceMoments: return
  s.smom = newSeq[array[2, float32]](w * h * c8)
  for y in 0 ..< h:
    if s.ctl.cancelled: return
    for x in 0 ..< w:
      if s.valid[y * w + x]:
        patchMoments(s.img, w, s.ch, c8, side, x - r, y - r, s.smom.toOpenArray((y * w + x) * c8, (y * w + x + 1) * c8 - 1))

proc distClipped*(s: Solver; t, src: (int32, int32); cutoff: float32): float32 =
  ## Distance without adaptation of a patch that may be clipped, per sample.
  let (w, h, ch, r) = (int32(s.w), int32(s.h), s.ch, s.r)
  let full = float32((2 * r + 1) * (2 * r + 1)) * float32(ch)
  var sum = 0'f32
  var n = 0
  for dy in -r .. r:
    let ty = t[1] + dy
    if ty < 0 or ty >= h: continue
    let sy = src[1] + dy
    for dx in -r .. r:
      let tx = t[0] + dx
      if tx < 0 or tx >= w or s.px[int(ty) * s.w + int(tx)] == pxVoid: continue
      let ti = (int(ty) * s.w + int(tx)) * ch
      let si = (int(sy) * s.w + int(src[0] + dx)) * ch
      for c in 0 ..< ch:
        let d = s.img[ti + c] - s.img[si + c]
        sum += d * d
      n += ch
    # Early out: the mean can only be ≥ sum / (full patch size).
    if sum > cutoff * full: return Inf.float32
  if n == 0: Inf.float32 else: sum / float32(n)

proc distFull*(s: Solver; t, src: (int32, int32); cutoff: float32): float32 =
  ## `distClipped` of a full patch: the same sums, in the same order, giving up (infinity) as
  ## soon as the sum shows the patch is worse than `cutoff`.
  let (w, ch, r) = (s.w, s.ch, int(s.r))
  let side = 2 * r + 1
  let full = float32((2 * r + 1) * (2 * r + 1)) * float32(ch)
  let limit = cutoff * full
  var sum = 0'f32
  for dy in -r .. r:
    let ti = ((int(t[1]) + dy) * w + (int(t[0]) - r)) * ch
    let si = ((int(src[1]) + dy) * w + (int(src[0]) - r)) * ch
    for p in 0 ..< side:
      for c in 0 ..< ch:
        let d = s.img[ti + p * ch + c] - s.img[si + p * ch + c]
        sum += d * d
      if sum > limit: return Inf.float32
  sum / float32(side * side * ch)

proc moments*(s: Solver; t, src: (int32, int32); m: var openArray[array[5, float32]]): int =
  ## Moments of the target patch at `t` and the source patch at `src` per channel:
  ## `[Σt, Σt², Σs, Σs², Σts]`, and the sample count.
  let (w, h, ch, r) = (int32(s.w), int32(s.h), s.ch, s.r)
  for v in m.mitems: v = [0'f32, 0, 0, 0, 0]
  for dy in -r .. r:
    let ty = t[1] + dy
    if ty < 0 or ty >= h: continue
    let sy = src[1] + dy
    for dx in -r .. r:
      let tx = t[0] + dx
      if tx < 0 or tx >= w or s.px[int(ty) * s.w + int(tx)] == pxVoid: continue
      let ti = (int(ty) * s.w + int(tx)) * ch
      let si = (int(sy) * s.w + int(src[0] + dx)) * ch
      for c in 0 ..< min(m.len, ch):
        let (a, b) = (s.img[ti + c], s.img[si + c])
        m[c][0] += a
        m[c][1] += a * a
        m[c][2] += b
        m[c][3] += b * b
        m[c][4] += a * b
      inc result

proc momentsFull*(s: Solver; k: int; t, src: (int32, int32); m: var openArray[array[5, float32]]) =
  ## `moments` of a full target patch (`k` in the `tbox` tables): the target's own sums kept from
  ## `costs`, the source's from `prepare` when kept, each summed in the same order as there.
  let (w, ch, r) = (s.w, s.ch, int(s.r))
  let c8 = m.len
  let side = 2 * r + 1
  let so = (int(src[1]) * w + int(src[0])) * c8
  let haveSm = s.smom.len > 0 and so + c8 <= s.smom.len
  var acc: array[MomCh, array[3, float32]]
  for dy in -r .. r:
    let ti = ((int(t[1]) + dy) * w + (int(t[0]) - r)) * ch
    let si = ((int(src[1]) + dy) * w + (int(src[0]) - r)) * ch
    for p in 0 ..< side:
      for c in 0 ..< c8:
        let (a, b) = (s.img[ti + p * ch + c], s.img[si + p * ch + c])
        if haveSm:
          acc[c][2] += a * b
        else:
          acc[c][0] += b
          acc[c][1] += b * b
          acc[c][2] += a * b
  for c in 0 ..< c8:
    let tm = s.tmom[k * c8 + c]
    let (ss, sss) = (if haveSm: (s.smom[so + c][0], s.smom[so + c][1]) else: (acc[c][0], acc[c][1]))
    m[c] = [tm[0], tm[1], ss, sss, acc[c][2]]

proc distAdapted*(s: Solver; t, src: (int32, int32)): float32 =
  var m: array[MomCh, array[5, float32]]
  let ch = min(s.ch, MomCh)
  let n = s.moments(t, src, m.toOpenArray(0, ch - 1))
  if n == 0: return Inf.float32
  adaptedCost(s.adapt, float32(n), m.toOpenArray(0, ch - 1))

proc distAdaptedFull*(s: Solver; k: int; t, src: (int32, int32)): float32 =
  var m: array[MomCh, array[5, float32]]
  let ch = min(s.ch, MomCh)
  s.momentsFull(k, t, src, m.toOpenArray(0, ch - 1))
  let side = 2 * int(s.r) + 1
  adaptedCost(s.adapt, float32(side * side), m.toOpenArray(0, ch - 1))

proc dist(s: Solver; t: (int32, int32); k: int; src: (int32, int32); cutoff: float32): float32 {.inline.} =
  ## Distance between the target patch at `t` (`k` its index in the `tbox` tables) and the source
  ## patch at `src`, per sample; with colour adaptation the source is fitted first.
  let full = s.full[k]
  if not s.adapt.isNone:
    return (if full: s.distAdaptedFull(k, t, src) else: s.distAdapted(t, src))
  if full: s.distFull(t, src, cutoff) else: s.distClipped(t, src, cutoff)

proc transform(s: Solver; t: (int32, int32); k: int; src: (int32, int32); outp: var openArray[(float32, float32)]) =
  ## Gain and bias per channel for the match `t → src` (identity without adaptation).
  if s.adapt.isNone:
    for v in outp.mitems: v = (1'f32, 0'f32)
    return
  var m: array[MomCh, array[5, float32]]
  let ch = min(s.ch, MomCh)
  var n: int
  if s.full[k]:
    s.momentsFull(k, t, src, m.toOpenArray(0, ch - 1))
    let side = 2 * int(s.r) + 1
    n = side * side
  else:
    n = s.moments(t, src, m.toOpenArray(0, ch - 1))
  for c in 0 ..< outp.len:
    outp[c] = (if n == 0 or c >= ch: (1'f32, 0'f32) else: fit(s.adapt, float32(n), m[c][0], m[c][1], m[c][2], m[c][3]))

proc pmStep(s: Solver; t: (int32, int32); k: int; cur: ((int32, int32), float32); left, up: Option[(int32, int32)];
            step: int32; rng: var IRng; searchRadius: int32): ((int32, int32), float32) =
  ## One PatchMatch step at target `t`: propagation from the neighbours visited just before it,
  ## then random search in exponentially shrinking windows around the best.
  var (best, bc) = cur
  if left.isSome:
    let cand = (left.get[0] + step, left.get[1])
    if s.isValid(cand[0], cand[1]) and cand != best:
      let d = s.dist(t, k, cand, bc)
      if d < bc: (best, bc) = (cand, d)
  if up.isSome:
    let cand = (up.get[0], up.get[1] + step)
    if s.isValid(cand[0], cand[1]) and cand != best:
      let d = s.dist(t, k, cand, bc)
      if d < bc: (best, bc) = (cand, d)
  var rad = searchRadius
  while rad >= 1:
    let ox = rng.range(-rad, rad)
    let oy = rng.range(-rad, rad)
    let cand = (best[0] + ox, best[1] + oy)
    if s.isValid(cand[0], cand[1]) and cand != best:
      let d = s.dist(t, k, cand, bc)
      if d < bc: (best, bc) = (cand, d)
    rad = rad div 2
  (best, bc)

proc patchmatch(s: Solver; nnf: var seq[(int32, int32)]; cost: var seq[float32]; pass: int; seed: uint64; searchRadius: int32) =
  ## One PatchMatch pass over all target rows, in bands of 16 grid rows: each band is visited in
  ## raster order (backwards on odd passes), propagating within itself, its random numbers seeded
  ## by its index (the Rust crate's parallel and wavefront sweeps visit cells with the same
  ## neighbours and random numbers).
  const Band = 16
  let tb = s.tbox
  let tw = tb.width
  if tw == 0 or tb.height == 0 or nnf.len < tb.area or cost.len < tb.area: return
  let reverse = pass mod 2 == 1
  let step: int32 = (if reverse: -1 else: 1)
  var y = tb.y0
  while y < tb.y1:
    let bi = y div Band
    let ys = y
    let rows = min((bi + 1) * Band, tb.y1) - y
    y += rows
    if s.ctl.cancelled: return
    let base = (ys - tb.y0) * tw
    var rng = newIRng(seed xor (uint64(bi) * 0x2545_F491_4F6C_DD1D'u64) xor (uint64(pass) shl 40))
    for ry in 0 ..< rows:
      if s.ctl.cancelled: return
      let ly = (if reverse: rows - 1 - ry else: ry)
      let yy = ys + ly
      for rx in 0 ..< tw:
        let lx = (if reverse: tw - 1 - rx else: rx)
        let li = base + ly * tw + lx
        if not s.targets[li]: continue
        let nx = lx - int(step)
        let left = (if nx >= 0 and nx < tw: some(nnf[base + ly * tw + nx]) else: none((int32, int32)))
        let nly = ly - int(step)
        let up = (if nly >= 0 and nly < rows: some(nnf[base + nly * tw + lx]) else: none((int32, int32)))
        let t = (int32(tb.x0 + lx), int32(yy))
        (nnf[li], cost[li]) = s.pmStep(t, li, (nnf[li], cost[li]), left, up, step, rng, searchRadius)

proc costs(s: var Solver; nnf: seq[(int32, int32)]; cost: var seq[float32]) =
  ## Recompute every target's match cost, and with colour adaptation the full target patches' own
  ## moments. Raises `Cancelled`.
  let (w, ch, r, tb) = (s.w, s.ch, int(s.r), s.tbox)
  s.ctl.check
  if not s.adapt.isNone:
    let c8 = min(ch, MomCh)
    let side = 2 * r + 1
    for ty in 0 ..< tb.height:
      if s.ctl.cancelled: break
      for tx in 0 ..< tb.width:
        let k = ty * tb.width + tx
        if s.full[k]:
          patchMoments(s.img, w, ch, c8, side, tb.x0 + tx - r, tb.y0 + ty - r, s.tmom.toOpenArray(k * c8, (k + 1) * c8 - 1))
  for ty in 0 ..< tb.height:
    if s.ctl.cancelled: break
    let y = tb.y0 + ty
    for tx in 0 ..< tb.width:
      let k = ty * tb.width + tx
      if s.targets[k]:
        cost[k] = s.dist((int32(tb.x0 + tx), int32(y)), k, nnf[k], Inf.float32)
  s.ctl.check

proc vote(s: var Solver; nnf: seq[(int32, int32)]; cost: seq[float32]) =
  ## Re-estimate hole pixels by a similarity-weighted vote of all overlapping patch matches (each
  ## copy adjusted by its gain and bias).
  let (w, h, ch, r) = (s.w, s.h, s.ch, int(s.r))
  let (tb, hb) = (s.tbox, s.holeBox)
  if s.ctl.cancelled: return
  template used(k: int): bool = s.targets[k] and cost[k].isFinite
  # σ² from the 75th percentile of match costs.
  var cs: seq[float32]
  for k in 0 ..< tb.area:
    if used(k): cs.add cost[k]
  var sigma2 = 1'f32
  if cs.len > 0:
    let k = min(cs.len * 3 div 4, cs.len - 1)
    cs.sort(proc(a, b: float32): int = totalCmp(a, b))
    sigma2 = rmax(cs[k], 1e-6'f32)
  let adapted = not s.adapt.isNone
  var wt = newSeq[float32](tb.area)
  var src = newSeq[int](tb.area)
  for k in 0 ..< tb.area:
    if used(k):
      wt[k] = rmax(exp(-cost[k] / (2'f32 * sigma2)), 1e-8'f32)
      src[k] = int(nnf[k][1]) * w + int(nnf[k][0])
  var gb: seq[(float32, float32)]
  if adapted:
    gb = newSeq[(float32, float32)](tb.area * ch)
    for v in gb.mitems: v = (1'f32, 0'f32)
    for ty in 0 ..< tb.height:
      if s.ctl.cancelled: break
      let y = tb.y0 + ty
      for tx in 0 ..< tb.width:
        let k = ty * tb.width + tx
        if used(k): s.transform((int32(tb.x0 + tx), int32(y)), k, nnf[k], gb.toOpenArray(k * ch, (k + 1) * ch - 1))
  # The vote, into a buffer over the hole's bounds (every pixel reads the old pixels).
  var outp = newSeq[float32](hb.area * ch)
  var got = newSeq[bool](hb.area)
  var acc = newSeq[float32](ch)
  for yy in 0 ..< hb.height:
    if s.ctl.cancelled: break
    let y = hb.y0 + yy
    for x in hb.x0 ..< hb.x1:
      if s.px[y * w + x] != pxHole: continue
      for c in 0 ..< ch: acc[c] = 0
      var wsum = 0'f32
      for ty in max(y - r, 0) .. min(y + r, h - 1):
        let dy = ty - y
        let krow = (ty - tb.y0) * tb.width
        for tx in max(x - r, 0) .. min(x + r, w - 1):
          let k = krow + tx - tb.x0
          let wk = wt[k]
          if wk == 0'f32: continue
          # The pixel of the match's patch that lands here.
          let si = (src[k] - dy * w - (tx - x)) * ch
          for c in 0 ..< ch:
            let v = (if adapted: gb[k * ch + c][0] * s.img[si + c] + gb[k * ch + c][1] else: s.img[si + c])
            acc[c] += v * wk
          wsum += wk
      if wsum > 0'f32:
        let o = (yy * hb.width + (x - hb.x0)) * ch
        for c in 0 ..< ch: outp[o + c] = acc[c] / wsum
        got[yy * hb.width + (x - hb.x0)] = true
  for yy in 0 ..< hb.height:
    for xx in 0 ..< hb.width:
      if got[yy * hb.width + xx]:
        let (d, so) = (((hb.y0 + yy) * w + hb.x0 + xx) * ch, (yy * hb.width + xx) * ch)
        for c in 0 ..< ch: s.img[d + c] = outp[so + c]

# ---------------------------------------------------------------- onion peel

proc knownSse(image: openArray[float32]; ch: int; offs: openArray[int]; tv: openArray[float32]; base: int;
              limit: float32): Option[float32] {.inline.} =
  ## `Σ (t − s)²` over a target patch's known pixels against the source patch centred at pixel
  ## `base`; none once the sum reaches `limit` (it only grows).
  var e = 0'f32
  for i, o in offs:
    let si = (base + o) * ch
    for c in 0 ..< ch:
      let d = tv[i * ch + c] - image[si + c]
      e += d * d
    if e >= limit: return none(float32)
  some(e)

proc knownSseLanes(image: openArray[float32]; ch: int; offs: openArray[int]; tv: openArray[float32];
                   bases: array[Lanes, int]; limits: array[Lanes, float32]): array[Lanes, Option[float32]] =
  ## `knownSse` of `Lanes` sources at once, each against its own limit.
  var e: array[Lanes, float32]
  for i, o in offs:
    for l in 0 ..< Lanes:
      let si = (bases[l] + o) * ch
      for c in 0 ..< ch:
        let d = tv[i * ch + c] - image[si + c]
        e[l] += d * d
    var done = true
    for l in 0 ..< Lanes:
      if not (e[l] >= limits[l]): done = false
    if done: break
  for l in 0 ..< Lanes:
    result[l] = (if e[l] < limits[l]: some(e[l]) else: none(float32))

proc leastSum*(v, n: float32; strict: bool): float32 =
  ## The least sum `e ≥ 0` with `e / n > v` (`strict`) or `e / n ≥ v` in float arithmetic;
  ## infinity for an infinite `v`.
  template holds(e: float32): bool = (if strict: e / n > v else: e / n >= v)
  if not v.isFinite or not n.isFinite or n <= 0'f32: return Inf.float32
  var e = rmax(v * n, 0'f32)
  while e > 0'f32 and holds(nextDown(e)): e = nextDown(e)
  while not holds(e) and e < Inf.float32: e = nextUp(e)
  e

proc onionPeel*(w, h, ch: int; r: int32; img: var seq[float32]; px: openArray[Px]; hb: Bounds;
                sources: openArray[(int32, int32)]; ctl: Interrupt) =
  ## Fill the hole from its edge inwards, one ring at a time: each pixel takes the centre of the
  ## source patch that best matches what is already known around it (the first in `sources` of
  ## those that match best). Exhaustive, with sources given up as soon as they can only lose.
  ## Raises `Cancelled`.
  var known = newSeq[bool](w * h)
  for i in 0 ..< w * h: known[i] = px[i] in {pxKnown, pxSource}
  var index = newSeq[uint32](w * h)
  for v in index.mitems: v = NoneU32
  for j, (x, y) in sources: index[int(y) * w + int(x)] = uint32(j)
  var took = newSeq[uint32](w * h)
  for v in took.mitems: v = NoneU32
  const neighbours = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
  let ri = int(r)
  let side = 2 * ri + 1
  while true:
    ctl.check
    var front: seq[int]
    for y in hb.y0 ..< hb.y1:
      for x in hb.x0 ..< hb.x1:
        let i = y * w + x
        if known[i] or px[i] != pxHole: continue
        var edge = false
        for dy in -1 .. 1:
          for dx in -1 .. 1:
            let (nx, ny) = (x + dx, y + dy)
            if nx >= 0 and ny >= 0 and nx < w and ny < h and known[ny * w + nx]: edge = true
        if edge: front.add i
    if front.len == 0: break
    var vals = newSeq[(seq[float32], uint32)](front.len)
    for fi, i in front:
      let (x, y) = (i mod w, i div w)
      var offs = newSeqOfCap[int](side * side)
      var tv = newSeqOfCap[float32](side * side * ch)
      for dy in -ri .. ri:
        for dx in -ri .. ri:
          let (tx, ty) = (x + dx, y + dy)
          if tx >= 0 and ty >= 0 and tx < w and ty < h and known[ty * w + tx]:
            let ti = (ty * w + tx) * ch
            offs.add dy * w + dx
            for c in 0 ..< ch: tv.add img[ti + c]
      var best = Best(v: Inf.float32, j: high(int), worse: Inf.float32, equal: Inf.float32)
      if offs.len > 0:
        let nf = float32(offs.len)
        template baseOf(ix: int): int = (if ix < sources.len: int(sources[ix][1]) * w + int(sources[ix][0]) else: 0)
        template limitOf(ix: int): float32 = (if ix > best.j: best.equal else: best.worse)
        template adopt(ix0: int; e0: float32) =
          let ix = ix0
          let vq = e0 / nf
          if vq < Inf.float32 and (vq < best.v or (vq == best.v and ix < best.j)):
            best = Best(v: vq, j: ix, worse: leastSum(vq, nf, true), equal: leastSum(vq, nf, false))
        template consider(ix0: int) =
          let ix = ix0
          if ix < sources.len:
            let eq = knownSse(img, ch, offs, tv, baseOf(ix), limitOf(ix))
            if eq.isSome: adopt(ix, eq.get)
        # A close bound early: where the filled neighbours' sources point, or at the hole's edge,
        # where none is filled yet, every 16th source.
        for (dx, dy) in neighbours:
          let (qx, qy) = (x + dx, y + dy)
          if qx < 0 or qy < 0 or qx >= w or qy >= h: continue
          let tk = took[qy * w + qx]
          if int(tk) >= sources.len: continue
          let (sx, sy) = sources[int(tk)]
          let (cx, cy) = (int(sx) - dx, int(sy) - dy)
          if cx >= 0 and cy >= 0 and cx < w and cy < h and index[cy * w + cx] != NoneU32:
            consider(int(index[cy * w + cx]))
        if best.j == high(int):
          var j = 0
          while j < sources.len:
            consider(j)
            j += 16
        # The scan in order, `Lanes` sources at a time, each against its limit from the best
        # before them.
        var j = 0
        while j + Lanes <= sources.len:
          if j mod 4096 == 0 and j > 0 and ctl.cancelled: break
          var bases: array[Lanes, int]
          var limits: array[Lanes, float32]
          for l in 0 ..< Lanes:
            bases[l] = baseOf(j + l)
            limits[l] = limitOf(j + l)
          var sums: array[Lanes, Option[float32]]
          if ch in [1, 3, 4]: sums = knownSseLanes(img, ch, offs, tv, bases, limits)
          else:
            for l in 0 ..< Lanes: sums[l] = knownSse(img, ch, offs, tv, bases[l], limits[l])
          for l in 0 ..< Lanes:
            if sums[l].isSome: adopt(j + l, sums[l].get)
          j += Lanes
        while j < sources.len:
          consider(j)
          inc j
      let (sx, sy) = (if best.j < sources.len: sources[best.j] else: (0'i32, 0'i32))
      let si = (int(sy) * w + int(sx)) * ch
      vals[fi] = (img[si ..< si + ch], (if best.j == high(int): NoneU32 else: uint32(best.j)))
    ctl.check
    for fi, i in front:
      for c in 0 ..< ch: img[i * ch + c] = vals[fi][0][c]
      known[i] = true
      took[i] = vals[fi][1]

# ---------------------------------------------------------------- completion

proc completeCore(w, h, ch: int; img: var seq[float32]; px: seq[Px]; p: CompleteParams; ctl: Interrupt): bool =
  ## The completion on `img` in place: false (and `img` as it was) when there is nothing to copy
  ## from. Raises `Cancelled`, or `ValueError` on buffers that don't match the size.
  if w < 0 or h < 0 or ch < 0 or img.len != w * h * ch or px.len != w * h:
    raise newException(ValueError, "buffers do not match the image size")
  let hb0O = holeBounds(w, h, px)
  if hb0O.isNone: return true
  if ch == 0:
    img = @[]
    return true
  let hb0 = hb0O.get
  # Radii past the image's size copy nothing at all; cap them so the arithmetic stays sane.
  let r = clamp(p.patchRadius, 1, max(w, h) + 1)
  let psz = 2 * r + 1
  var levels = @[Level(w: w, h: h, img: move(img), px: px)]
  var boxes = @[hb0]
  var ext = max(hb0.width, hb0.height)
  var exts = @[ext]
  while true:
    ctl.check
    let l = levels.len - 1
    if ext <= 4 * psz or levels[l].w div 2 < 3 * psz or levels[l].h div 2 < 3 * psz: break
    levels.add downsample(levels[l], ch)
    boxes.add boxes[^1].half
    ext = (ext + 1) div 2
    exts.add ext
  let nlev = levels.len
  var prev = none(Coarser)
  var totalPx = 0
  for l in levels: totalPx += l.w * l.h
  var donePx = 0
  let ri = int32(r)
  for li in countdown(nlev - 1, 0):
    ctl.check
    let (lw, lh) = (levels[li].w, levels[li].h)
    var limg = move(levels[li].img)
    let lpx = move(levels[li].px)
    let hb = boxes[li]
    let tb = hb.grow(r, lw, lh)
    let validO = validSources(lw, lh, r, lpx, ctl)
    if validO.isNone:
      if li == 0: img = move(limg)
      raise newException(Cancelled, "cancelled")
    let valid = validO.get
    let sources = newSources(valid, lw)
    # Nothing to copy from at the finest level: the caller falls back.
    if li == 0 and sources.len == 0:
      img = move(limg)
      return false
    let targets = targetsAround(lw, r, lpx, hb, tb)
    ctl.check
    let coarser = prev
    prev = none(Coarser)
    if coarser.isSome:
      let c = coarser.get
      for y in hb.y0 ..< hb.y1:
        for x in hb.x0 ..< hb.x1:
          if lpx[y * lw + x] == pxHole:
            let (sx, sy) = (min(x div 2, c.w - 1), min(y div 2, c.h - 1))
            let (d, s) = ((y * lw + x) * ch, (sy * c.w + sx) * ch)
            for k in 0 ..< ch: limg[d + k] = c.img[s + k]
    elif sources.len > 0:
      onionPeel(lw, lh, ch, ri, limg, lpx, hb, sources.all(valid, lw), ctl)
    else:
      var unknown = newSeq[bool](lw * lh)
      for i, k in lpx: unknown[i] = k in {pxHole, pxVoid}
      let m = membraneFill(lw, lh, ch, limg, unknown)
      for i, k in lpx:
        if k == pxHole:
          for c in 0 ..< ch: limg[i * ch + c] = m[i * ch + c]
    if sources.len == 0:
      prev = some(Coarser(w: lw, h: lh, img: move(limg), tbox: tb))
      continue
    let refineOnly = li + 1 < nlev and exts[li] > p.refineExtent
    var nTargets = 0
    for t in targets:
      if t: inc nTargets
    let keepSourceMoments = lw * lh <= SourceMomentsMax and sources.len <= nTargets * (if refineOnly: 4 else: 64)
    var solver = Solver(w: lw, h: lh, ch: ch, r: ri, img: move(limg), px: lpx, valid: valid, targets: targets,
                        holeBox: hb, tbox: tb, adapt: p.adapt, ctl: ctl)
    solver.prepare(keepSourceMoments)
    ctl.check
    # NNF init: the coarser level's matches scaled up where they land on a valid source, random
    # otherwise.
    var rng = newIRng(p.seed xor (uint64(li) shl 20))
    let tw = tb.width
    var nnf = newSeq[(int32, int32)](tb.area)
    var upsampled = false
    for y in tb.y0 ..< tb.y1:
      if y mod 32 == 0: ctl.check
      for x in tb.x0 ..< tb.x1:
        let k = (y - tb.y0) * tw + (x - tb.x0)
        if not solver.targets[k]: continue
        var cand = none((int32, int32))
        if coarser.isSome and coarser.get.field.len > 0:
          let c = coarser.get
          let (cx, cy) = (min(x div 2, c.w - 1), min(y div 2, c.h - 1))
          let (sx, sy) = c.fieldAt(cx, cy)
          if sx != low(int32):
            # The same displacement, twice as long.
            let cc = (int32(x) + 2'i32 * (sx - int32(cx)), int32(y) + 2'i32 * (sy - int32(cy)))
            if solver.isValid(cc[0], cc[1]): cand = some(cc)
        upsampled = upsampled or cand.isSome
        nnf[k] = (if cand.isSome: cand.get else: sources.nth(int(rng.next mod uint64(sources.len)), solver.valid, lw))
    var cost = newSeq[float32](tb.area)
    for v in cost.mitems: v = Inf.float32
    # A finer level starts from the coarse matches voted with its own (sharp) pixels.
    if upsampled:
      solver.costs(nnf, cost)
      solver.vote(nnf, cost)
    let em = (if li + 1 == nlev: p.emIters
              elif refineOnly: 1
              else: max((if nlev - 1 - li < 63: p.emIters shr (nlev - 1 - li) else: 0), p.minEmIters))
    let pmIters = (if refineOnly: 1 else: p.pmIters)
    let fullSearch = int32(max(lw, lh))
    for it in 0 ..< em:
      solver.costs(nnf, cost)
      # Wide random search on the coarsest level and on each level's first iteration, local
      # refinement afterwards.
      let radius = (if refineOnly: 2'i32
                    elif li + 1 == nlev or it == 0: fullSearch
                    else: min(4'i32 * int32(psz), fullSearch))
      for pass in 0 ..< pmIters:
        ctl.check
        solver.patchmatch(nnf, cost, pass + it * pmIters, p.seed + uint64(li * 1000 + it), radius)
      ctl.check
      solver.vote(nnf, cost)
      ctl.check
    ctl.check
    var field: seq[(int32, int32)]
    if li != 0:
      field = newSeq[(int32, int32)](nnf.len)
      for i in 0 ..< nnf.len: field[i] = (if solver.targets[i]: nnf[i] else: (low(int32), low(int32)))
    donePx += lw * lh
    ctl.progress(float32(donePx) / float32(max(totalPx, 1)))
    prev = some(Coarser(w: lw, h: lh, img: move(solver.img), field: field, tbox: tb))
  img = (if prev.isSome: prev.get.img else: @[])
  true

proc completeMasked*(w, h, ch: int; img: seq[float32]; px: seq[Px]; p: CompleteParams; ctl: Interrupt): Option[seq[float32]] =
  ## Completion with every pixel's role given (`Px`): the hole, the context, what may be copied,
  ## and what isn't image at all. None when there is nothing to copy from. Raises `Cancelled`
  ## when `ctl` was cancelled, `ValueError` on buffers that don't match the size.
  var v = img
  if completeCore(w, h, ch, v, px, p, ctl): some(v) else: none(seq[float32])

proc completeMaskedOwned*(w, h, ch: int; img: var seq[float32]; px: seq[Px]; p: CompleteParams; ctl: Interrupt): bool =
  ## `completeMasked` that fills `img` itself (only the hole changes): false, `img` untouched,
  ## when there is nothing to copy from.
  completeCore(w, h, ch, img, px, p, ctl)

proc completeWith*(w, h, ch: int; img: seq[float32]; hole: seq[bool]; p: CompleteParams; ctl: Interrupt): Option[seq[float32]] =
  ## `complete` that can be cancelled (raises `Cancelled`) and reports progress (by pyramid level).
  if hole.len != w * h: raise newException(ValueError, "hole mask does not match the image size")
  var px = newSeq[Px](hole.len)
  for i, b in hole: px[i] = (if b: pxHole else: pxSource)
  completeMasked(w, h, ch, img, px, p, ctl)

proc complete*(w, h, ch: int; img: seq[float32]; hole: seq[bool]; p: CompleteParams): Option[seq[float32]] =
  ## Content-aware completion of `hole`, copying from any pixel outside it. None when there is no
  ## fully-known patch to copy from (the caller can fall back to `membraneFill`).
  completeWith(w, h, ch, img, hole, p, NoInterrupt)

# ---------------------------------------------------------------- proximity match

proc newHoleIntegral(stride: int; hole: openArray[bool]; b: (int, int, int, int)): Option[HoleIntegral] =
  ## Summed-area table restricted to the hole's bounds.
  let (x0, y0, x1, y1) = b
  let (w, h) = (x1 - x0, y1 - y0)
  if w < 0 or h < 0: return none(HoleIntegral)
  var hi = HoleIntegral(x0: x0, y0: y0, w: w, h: h, data: newSeq[uint32]((w + 1) * (h + 1)))
  for yy in 0 ..< h:
    let base = (y0 + yy) * stride
    if base + x1 > hole.len: return none(HoleIntegral)
    var count = 0'u32
    for x in 0 ..< w:
      count += uint32(hole[base + x0 + x])
      hi.data[(yy + 1) * (w + 1) + x + 1] = hi.data[yy * (w + 1) + x + 1] + count
  some(hi)

proc count(hi: HoleIntegral; x0, y0, x1, y1: int): uint32 =
  windowCount(hi.data, hi.w, hi.h, x0 - hi.x0, y0 - hi.y0, x1 - hi.x0, y1 - hi.y0)

proc bestOffset*(w, h, ch: int; img: seq[float32]; hole: seq[bool]; ring0: int; maxRadius0: int): Option[(int32, int32)] =
  ## Proximity match: the displacement `(dx, dy)` (within `maxRadius`) whose surroundings best
  ## match the ring of `ring` pixels around the hole, with the displaced hole fully inside the grid
  ## and outside the hole. None if no displacement fits.
  if w <= 0 or h <= 0 or ch < 0 or hole.len < w * h or img.len < w * h * ch: return none((int32, int32))
  let bb = holeBbox(w, h, hole)
  if bb.isNone: return none((int32, int32))
  let (bx0, by0, bx1, by1) = bb.get
  let extent = min(max(w, h), int(high(int32) div 4))
  let maxRadius = min(max(maxRadius0, 0), extent)
  if maxRadius == 0: return none((int32, int32))
  let ring = min(max(ring0, 1), extent)
  let hsO = newHoleIntegral(w, hole, bb.get)
  if hsO.isNone: return none((int32, int32))
  let hs = hsO.get
  var pts0: seq[(int, int)]
  let (x0, y0) = (bx0 - ring, by0 - ring)
  let (x1, y1) = (bx1 + ring, by1 + ring)
  for y in max(y0, 0) ..< min(y1, h):
    for x in max(x0, 0) ..< min(x1, w):
      if hole[y * w + x]: continue
      if hs.count(x - ring, y - ring, x + ring + 1, y + ring + 1) > 0: pts0.add (x, y)
  let stride = max(pts0.len div 2000, 1)
  var pts: seq[(int, int)]
  var q = 0
  while q < pts0.len:
    pts.add pts0[q]
    q += stride
  proc eval(dx, dy: int; cutoff: float32): Option[float32] =
    let (hx0, hy0, hx1, hy1) = (x0 + dx, y0 + dy, x1 + dx, y1 + dy)
    let inside = hx0 >= 0 and hy0 >= 0 and hx1 <= w and hy1 <= h
    if (dx == 0 and dy == 0) or not inside or hs.count(hx0, hy0, hx1, hy1) != 0: return none(float32)
    var e = 0'f32
    let n = float32(max(pts.len, 1))
    let penalty = 1'f32 + 1e-3'f32 * (hypot(float32(dx), float32(dy)) / float32(maxRadius))
    for i, (x, y) in pts:
      let (a, b) = ((y * w + x) * ch, ((y + dy) * w + (x + dx)) * ch)
      for c in 0 ..< ch:
        let d = img[a + c] - img[b + c]
        e += d * d
      # The partial score is a lower bound on the final one (SSD terms are nonnegative).
      if i mod 32 == 31 and cutoff.isFinite and e / n * penalty >= cutoff: return none(float32)
    some(e / n * penalty)
  # Coarse grid over the search window, then a full-resolution refinement around the winner.
  let step = max(maxRadius div 24, 1)
  var best = none(((int, int), float32))
  template consider(dx, dy: int) =
    let e = eval(dx, dy, (if best.isSome: best.get[1] else: Inf.float32))
    if e.isSome and (best.isNone or e.get < best.get[1]): best = some(((dx, dy), e.get))
  var dy = -maxRadius
  while dy <= maxRadius:
    var dx = -maxRadius
    while dx <= maxRadius:
      consider(dx, dy)
      dx += step
    dy += step
  if step > 1 and best.isSome:
    let (bx, by) = best.get[0]
    for dy2 in by - step .. by + step:
      for dx2 in bx - step .. bx + step: consider(dx2, dy2)
  if best.isSome: some((int32(best.get[0][0]), int32(best.get[0][1]))) else: none((int32, int32))

# ---------------------------------------------------------------- texture synthesis

proc minCut(across, along: int; err: proc(u, v: int): float32): seq[int] =
  ## The minimum-error cut through a band `across` wide and `along` long (Efros & Freeman's
  ## boundary cut, by dynamic programming): for each step along the band, the offset across it
  ## where the cut runs. Empty when there is no cost anywhere.
  if across <= 0 or along <= 0: return @[]
  var acc = newSeq[float32](across * along)
  var allZero = true
  for v in 0 ..< along:
    for u in 0 ..< across:
      acc[v * across + u] = err(u, v)
      if acc[v * across + u] != 0'f32: allZero = false
  if allZero: return @[]
  for v in 1 ..< along:
    for u in 0 ..< across:
      let (lo, hi) = (max(u - 1, 0), min(u + 1, across - 1))
      var m = Inf.float32
      for k in lo .. hi: m = rmin(m, acc[(v - 1) * across + k])
      acc[v * across + u] += m
  proc argmin(v, lo, hi: int): int =
    result = lo
    for k in lo + 1 .. hi:
      if totalCmp(acc[v * across + k], acc[v * across + result]) < 0: result = k
  result = newSeq[int](along)
  var u = argmin(along - 1, 0, across - 1)
  for v in countdown(along - 1, 0):
    if v + 1 < along: u = argmin(v, max(u - 1, 0), min(u + 1, across - 1))
    result[v] = u

proc synthesize*(w, h, ch: int; img: seq[float32]; hole: seq[bool]; blk: int; seed: uint64): seq[float32] =
  ## Create texture: fill the hole with blocks of surrounding texture.
  result = img
  if w <= 0 or h <= 0 or ch < 0 or hole.len != w * h or img.len != w * h * ch: return
  let bb = holeBbox(w, h, hole)
  if bb.isNone: return
  let (bx0, by0, bx1, by1) = bb.get
  let b = min(max(blk, 4), max(w, h) + 1)
  let ov = max(b div 3, 1)
  let hs = integral(w, h, hole)
  # Candidate block origins: whole block (with overlap) inside the grid and outside the hole.
  var cands: seq[(int, int)]
  for y in ov ..< h - b - ov:
    for x in ov ..< w - b - ov:
      if windowCount(hs, w, h, x - ov, y - ov, x + b + ov, y + b + ov) == 0: cands.add (x, y)
  if cands.len == 0: return membraneFill(w, h, ch, img, hole)
  var filled = newSeq[bool](w * h)
  for i in 0 ..< w * h: filled[i] = not hole[i]
  var rng = newIRng(seed)
  var by = by0
  while by < by1:
    var bx = bx0
    while bx < bx1:
      if windowCount(hs, w, h, bx, by, bx + b, by + b) > 0:
        # Pick the candidate whose block best agrees with already-known pixels around/inside.
        var best = (cands[0], Inf.float32)
        for _ in 0 ..< 48:
          let c = cands[int(rng.next mod uint64(cands.len))]
          var (e, n) = (0'f32, 0'u32)
          for y in by - ov ..< by + b + ov:
            for x in bx - ov ..< bx + b + ov:
              if x < 0 or y < 0 or x >= w or y >= h or not filled[y * w + x]: continue
              let (a, s) = ((y * w + x) * ch, ((y - by + c[1]) * w + (x - bx + c[0])) * ch)
              for k in 0 ..< ch:
                let d = result[a + k] - img[s + k]
                e += d * d
              n += 1
          let ev = (if n == 0: 0'f32 else: e / float32(n))
          if ev < best[1]: best = (c, ev)
        let c = best[0]
        let (x0, y0) = (max(bx - ov, 0), max(by - ov, 0))
        let (x1, y1) = (min(bx + b + ov, w), min(by + b + ov, h))
        let (cbx, cby) = (bx, by)
        proc srcOf(x, y: int): int = ((y - cby + c[1]) * w + (x - cbx + c[0])) * ch
        let outv = result
        let filledv = filled
        let errf = proc(x, y: int): float32 =
          let i = y * w + x
          if not (hole[i] and filledv[i]): return 0'f32
          let (a, s) = (i * ch, srcOf(x, y))
          var t = -0.0'f32 # Rust's Iterator::sum starts at -0.0
          for q in 0 ..< ch:
            let d = outv[a + q] - img[s + q]
            t += d * d
          t
        let left = minCut(max(min(bx + ov, x1) - x0, 0), max(y1 - y0, 0), proc(u, v: int): float32 = errf(x0 + u, y0 + v))
        let top = minCut(max(min(by + ov, y1) - y0, 0), max(x1 - x0, 0), proc(u, v: int): float32 = errf(x0 + v, y0 + u))
        for y in y0 ..< y1:
          for x in x0 ..< x1:
            let i = y * w + x
            if not hole[i]: continue
            let (u, v) = (x - x0, y - y0)
            let keep = filled[i] and ((v < left.len and u < left[v]) or (u < top.len and v < top[u]))
            if not keep:
              let (a, s) = (i * ch, srcOf(x, y))
              for k in 0 ..< ch: result[a + k] = img[s + k]
            filled[i] = true
      bx += b
    by += b
