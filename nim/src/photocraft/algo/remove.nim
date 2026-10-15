## The Remove Tool: fill what a brush stroke painted over from its surroundings (port of
## `remove.rs`).
##
## * `closeLoops`: a stroke drawn around an object also removes what it encloses.
## * `dilate`: the hole grows by a few pixels so an object's soft edge and halo go with it.
## * `removeWith`: the hole is completed by `nonlocal.completeWith` (patch-based, with texture
##   features so the fill keeps the grain of its surroundings, thin wires included).
## * `holeRadius`: how thick a hole is, which sets how many scales the completion uses.
##
## Buffers are interleaved `w × h × ch` normalised floats (any model/depth) with `w × h` masks;
## deterministic.

import std/options
import ../raster
import ./holes, ./nonlocal, ./poisson

export closeLoops, dilate, holeRadius

const
  Sparse = 0.25   ## holes covering less than this share of their box are completed tile by tile
  MinTile = 256   ## smallest tile side, in pixels

proc fill(w, h, ch: int; img: seq[float32]; hole: seq[bool]; seed: uint64; ctl: Interrupt): seq[float32] =
  ## Completes one hole, or diffuses it when there is no fully-known patch to copy from.
  var p = defaultParams()
  p.seed = seed
  let r = completeWith(w, h, ch, img, hole, p, ctl)
  if r.isSome: r.get else: membraneFillWith(w, h, ch, img, hole, ctl)

proc removeWith*(w, h, ch: int; img: seq[float32]; hole: seq[bool]; seed: uint64; ctl: Interrupt): seq[float32] =
  ## Fills `hole` in `img` from the pixels around it. Returns the full buffer (unchanged outside
  ## the hole); a buffer or mask of the wrong size comes back unchanged. When nothing around the
  ## hole is fully known, it is diffused from its edges instead. Raises `Cancelled` when `ctl` is
  ## cancelled.
  ##
  ## A sparse hole is filled in tiles eight times its thickness (at least `MinTile` pixels), row by
  ## row, each with a margin of six times its thickness (at least 48 pixels): each tile sees the
  ## ones before it as filled, so the fill runs on across tile edges.
  if w <= 0 or h <= 0 or w > high(int) div h: return img
  let n = w * h
  if ch <= 0 or hole.len != n or img.len != n * ch: return img
  var (bx0, by0, bx1, by1, count) = (w, h, 0, 0, 0)
  for i in 0 ..< n:
    if hole[i]:
      let (x, y) = (i mod w, i div w)
      (bx0, by0, bx1, by1, count) = (min(bx0, x), min(by0, y), max(bx1, x + 1), max(by1, y + 1), count + 1)
  if count == 0: return img
  let thickness = int(min(holeRadius(w, h, hole), 1'u32 shl 20))
  let tile = max(8 * thickness, MinTile)
  let area = (bx1 - bx0) * (by1 - by0)
  if float64(count) >= Sparse * float64(area) or max(bx1 - bx0, by1 - by0) <= 2 * tile:
    return fill(w, h, ch, img, hole, seed, ctl)
  let margin = max(6 * thickness, 48)
  result = img
  var left = hole
  var origins: seq[(int, int)]
  var y = by0
  while y < by1:
    var x = bx0
    while x < bx1:
      origins.add (x, y)
      x += tile
    y += tile
  for k, (tx0, ty0) in origins:
    ctl.check()
    let (tx1, ty1) = (min(tx0 + tile, bx1), min(ty0 + tile, by1))
    var any = false
    for yy in ty0 ..< ty1:
      for xx in tx0 ..< tx1:
        if left[yy * w + xx]:
          any = true
          break
      if any: break
    if not any: continue
    let (x0, y0, x1, y1) = (max(tx0 - margin, 0), max(ty0 - margin, 0), min(tx1 + margin, w), min(ty1 + margin, h))
    let ww = x1 - x0
    var subImg = newSeqOfCap[float32](ww * (y1 - y0) * ch)
    var subHole = newSeqOfCap[bool](ww * (y1 - y0))
    for yy in y0 ..< y1:
      for i in (yy * w + x0) * ch ..< (yy * w + x1) * ch: subImg.add result[i]
      for i in yy * w + x0 ..< yy * w + x1: subHole.add left[i]
    let total = origins.len
    let sub = newInterrupt(proc(): bool = ctl.cancelled,
                           proc(f: float32) = ctl.progress((float32(k) + f) / float32(total)))
    let filled = fill(ww, y1 - y0, ch, subImg, subHole, seed + uint64(k), sub)
    for yy in ty0 ..< ty1:
      for xx in tx0 ..< tx1:
        let i = yy * w + xx
        if left[i]:
          let j = (yy - y0) * ww + (xx - x0)
          for c in 0 ..< ch: result[i * ch + c] = filled[j * ch + c]
          left[i] = false
