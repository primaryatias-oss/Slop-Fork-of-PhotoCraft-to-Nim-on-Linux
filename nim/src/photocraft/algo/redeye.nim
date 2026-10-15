## Click-local red-eye correction: neutralize a red pupil around a click (port of `redeye.rs`).
##
## The kernel never scans a full document. Callers pass the search window plus a read halo that
## covers a 1 px morphological shrink and a Gaussian of σ = 0.8.

import std/[math, options]
import ../rmath
import ../geom

const
  Sigma* = 0.8'f32       ## Gaussian σ applied to the pupil mask after a 1 px shrink
  MaxWindow* = 512'u32   ## largest window (search + halo) the engine may allocate, px on a side
  MaxPoints* = 64        ## clicks one command may apply in a single history step

proc isFinite(v: float32): bool {.inline.} = v == v and v - v == 0'f32

proc haloRadius*(): int32 =
  ## How far outside the search window the mask filter reads: `1` (erode) + `ceil(σ × 3)`
  ## (Gaussian) + `1` (margin).
  let blur0 = ceil(Sigma * 3'f32)
  let blur = (if blur0.isFinite: int32(clamp(blur0, 0'f32, 64'f32)) else: 3'i32)
  1'i32 + blur + 1'i32

proc searchRadius*(pupilSize: float32): float32 =
  ## Search radius in document pixels for a Pupil Size in 1..=100.
  let p = (if pupilSize.isFinite: clamp(pupilSize, 1'f32, 100'f32) else: 50'f32)
  clamp(4'f32 + 1.20'f32 * p, 8'f32, 128'f32)

proc unclampedWindowSide*(pupilSize: float32): Option[uint32] =
  ## Side length of the read rect (search + halo) before clamping Pupil Size to 1..=100. None
  ## when `pupilSize` is non-finite or the side would overflow `MaxWindow`.
  if not pupilSize.isFinite: return none(uint32)
  let r = 4'f32 + 1.20'f32 * pupilSize
  if not r.isFinite: return none(uint32)
  let half = abs(r) + float32(haloRadius())
  if not half.isFinite or half > float32(MaxWindow): return none(uint32)
  let side = ceil(2'f32 * half + 1'f32)
  if not side.isFinite or side > float32(MaxWindow): return none(uint32)
  some(asU32(side))

proc luma(p: array[4, float32]): float32 =
  0.2126'f32 * p[0] + 0.7152'f32 * p[1] + 0.0722'f32 * p[2]

proc isRedEye(p: array[4, float32]): bool =
  let (r, g, b) = (p[0], p[1], p[2])
  if not (r.isFinite and g.isFinite and b.isFinite): return false
  if r <= g + 0.15'f32 or r <= b + 0.15'f32: return false
  if luma(p) >= 0.92'f32: return false
  let mx = rmax(rmax(r, g), b)
  let mn = rmin(rmin(r, g), b)
  mx - mn >= 0.08'f32

proc asI32Wrap(x: int): int {.inline.} = int(cast[int32](uint32(x and 0xFFFF_FFFF)))

proc withinSearch(dx, dy: int; radius: float32): bool =
  let d = hypot(float32(dx), float32(dy))
  d.isFinite and d <= radius

proc offset(origin, x, click: int): int {.inline.} = satSub(satAdd(origin, asI32Wrap(x)), click)

proc satAdd64(a, b: int64): int64 {.inline.} =
  if b > 0 and a > high(int64) - b: high(int64)
  elif b < 0 and a < low(int64) - b: low(int64)
  else: a + b

proc nearestSeed(mask: seq[bool]; w, h: int; origin, click: (int, int); radius: float32): Option[int] =
  var best = none((int, int64))
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = y * w + x
      if not (i < mask.len and mask[i]): continue
      let dx = offset(origin[0], x, click[0])
      let dy = offset(origin[1], y, click[1])
      if not withinSearch(dx, dy, radius): continue
      # |dx|, |dy| ≤ 2³¹ so each square fits in i64; the sum saturates.
      let d2 = satAdd64(int64(dx) * int64(dx), int64(dy) * int64(dy))
      if best.isSome and d2 >= best.get[1]: continue
      best = some((i, d2))
  if best.isSome: some(best.get[0]) else: none(int)

proc floodKeep(mask: var seq[bool]; w, h, seed: int) =
  let n = mask.len
  if seed >= n or w == 0: return
  var keep = newSeq[bool](n)
  var stack = @[seed]
  while stack.len > 0:
    let i = stack.pop()
    if i < 0 or i >= n: continue
    if keep[i] or not mask[i]: continue
    keep[i] = true
    let x = i mod w
    let y = i div w
    if x > 0: stack.add i - 1
    if x + 1 < w: stack.add i + 1
    if y > 0 and i >= w: stack.add i - w
    if y + 1 < h: stack.add i + w
    if stack.len > (if n > high(int) div 4: high(int) else: n * 4): break
  mask = keep

proc erode(mask: seq[bool]; w, h: int): seq[bool] =
  result = newSeq[bool](mask.len)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = y * w + x
      var on = true
      for dy in -1 .. 1:
        for dx in -1 .. 1:
          let (xx, yy) = (x + dx, y + dy)
          if xx < 0 or yy < 0 or xx >= w or yy >= h:
            on = false
            continue
          let j = yy * w + xx
          if not (j < mask.len and mask[j]): on = false
      if i < result.len: result[i] = on

proc blur(src: seq[float32]; w, h: int; sigma: float32): seq[float32] =
  let n = (if w != 0 and h > high(int) div w: high(int) else: w * h)
  if w == 0 or h == 0 or src.len < n: return newSeq[float32](n)
  if not sigma.isFinite or sigma < 0.1'f32: return src[0 ..< n]
  let r0 = ceil(sigma * 3'f32)
  let r = (if r0.isFinite: int(clamp(r0, 1'f32, 32'f32)) else: 3)
  var k: seq[float32]
  var sum = 0'f32
  for i in -r .. r:
    let wgt = exp(float32(-(i * i)) / (2'f32 * sigma * sigma))
    if not wgt.isFinite: continue
    k.add wgt
    sum += wgt
  if k.len == 0 or not sum.isFinite or sum <= 0: return src[0 ..< n]
  for wgt in k.mitems: wgt /= sum
  var tmp = newSeq[float32](n)
  result = newSeq[float32](n)
  for y in 0 ..< h:
    for x in 0 ..< w:
      var a = 0'f32
      for ki, wgt in k:
        let xx = clamp(x + ki - r, 0, max(w - 1, 0))
        a += src[y * w + xx] * wgt
      tmp[y * w + x] = a
  for y in 0 ..< h:
    for x in 0 ..< w:
      var a = 0'f32
      for ki, wgt in k:
        let yy = clamp(y + ki - r, 0, max(h - 1, 0))
        a += tmp[yy * w + x] * wgt
      result[y * w + x] = a

proc apply*(px: var seq[array[4, float32]]; w, h: int; click, origin: (int, int); pupilSize, darken: float32): uint32 =
  ## Corrects red-eye in `px` (row-major RGBA, including the `haloRadius` border). `origin` is the
  ## document coordinate of `px[0]`. Returns how many pixels changed. Pixels farther than
  ## `searchRadius` from `click` are left untouched.
  if w <= 0 or h <= 0 or h > px.len div w: return 0
  let n = w * h
  let radius = searchRadius(pupilSize)
  let tAmt = (if darken.isFinite: clamp(clamp(darken, 0'f32, 100'f32) / 100'f32, 0'f32, 1'f32) else: 0.5'f32)
  var hard = newSeq[bool](n)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = y * w + x
      let dx = offset(origin[0], x, click[0])
      let dy = offset(origin[1], y, click[1])
      if withinSearch(dx, dy, radius) and isRedEye(px[i]): hard[i] = true
  let seed = nearestSeed(hard, w, h, origin, click, radius)
  if seed.isNone: return 0
  floodKeep(hard, w, h, seed.get)
  let eroded = erode(hard, w, h)
  var softIn = newSeq[float32](n)
  for i, on in eroded: softIn[i] = (if on: 1'f32 else: 0'f32)
  let soft = blur(softIn, w, h, Sigma)
  var changed = 0'u32
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = y * w + x
      let p = px[i]
      let dx = offset(origin[0], x, click[0])
      let dy = offset(origin[1], y, click[1])
      if not withinSearch(dx, dy, radius): continue
      if luma(p) >= 0.92'f32: continue
      let m = clamp((if i < soft.len: soft[i] else: 0'f32), 0'f32, 1'f32)
      let t = m * tAmt
      if t <= 1.0e-4'f32: continue
      let (r0, g0, b0, a0) = (p[0], p[1], p[2], p[3])
      let neut = (g0 + b0) * 0.5'f32
      let r1 = r0 + (neut - r0) * t
      let scale = 1'f32 - 0.45'f32 * t
      let (r2, g2, b2) = (r1 * scale, g0 * scale, b0 * scale)
      if not (r2.isFinite and g2.isFinite and b2.isFinite): continue
      if abs(r2 - r0) < 1.0e-6'f32 and abs(g2 - g0) < 1.0e-6'f32 and abs(b2 - b0) < 1.0e-6'f32: continue
      px[i] = [r2, g2, b2, a0]
      if changed < high(uint32): inc changed
  changed
