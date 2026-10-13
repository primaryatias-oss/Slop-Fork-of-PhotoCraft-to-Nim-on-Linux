## Binary Euclidean distance transform: nearest seeds along columns, then a lower envelope of
## squared-distance parabolas along rows (Felzenszwalb and Huttenlocher, 2012).
##
## Port of `crates/algo/src/selection/distance.rs`.

import std/[math, sequtils]

const
  NoSeed = high(uint32)
  EmptyDistance* = 1e10'f32 ## Distance reported everywhere for a mask with no seed.
  BandRows = 32

proc rowDistance(costs: openArray[uint32]; outp: var openArray[float32]; sites: var seq[int]; starts: var seq[float64]) =
  ## Lower envelope of parabolas with integer vertical distances (O(row length)).
  var count = 0
  for q in 0 ..< costs.len:
    let distance = costs[q]
    if distance == NoSeed: continue
    let fq = float64(distance) * float64(distance)
    var start = NegInf
    while count > 0:
      let p = sites[count - 1]
      let dp = float64(costs[p])
      start = (fq - dp * dp) / (2.0 * float64(q - p)) + (float64(q) + float64(p)) * 0.5
      if start > starts[count - 1]: break
      dec count
    if count == 0: start = NegInf
    sites[count] = q
    starts[count] = start
    inc count
  if count == 0:
    for d in outp.mitems: d = EmptyDistance
    return
  var k = 0
  for x in 0 ..< outp.len:
    while k + 1 < count and starts[k + 1] < float64(x): inc k
    let site = sites[k]
    let vertical = float64(costs[site])
    let dx = float64(x) - float64(site)
    outp[x] = float32(sqrt(dx * dx + vertical * vertical))

proc edt*(inside: openArray[bool]; w, h: int): seq[float32] =
  ## Euclidean distance to the nearest `true` pixel, row-major. Empty or inconsistent
  ## dimensions give an empty buffer; a mask with no seed gives `1e10` everywhere.
  if w <= 0 or h <= 0: return @[]
  if w > high(int) div h: return @[]
  let n = w * h
  if n != inside.len or w > int(high(uint32)) or h > int(high(uint32)): return @[]
  var anySeed, allSeed = false
  allSeed = true
  for s in inside:
    if s: anySeed = true
    else: allSeed = false
  if not anySeed: return newSeqWith(n, EmptyDistance)
  if allSeed: return newSeq[float32](n)
  # Column-major nearest-seed distances.
  var vertical = newSeq[uint32](n)
  for v in vertical.mitems: v = NoSeed
  for x in 0 ..< w:
    let col = x * h
    var nearest = -1
    for y in 0 ..< h:
      if inside[y * w + x]: nearest = y
      if nearest >= 0: vertical[col + y] = uint32(y - nearest)
    nearest = -1
    for y in countdown(h - 1, 0):
      if vertical[col + y] == 0: nearest = y
      if nearest >= 0: vertical[col + y] = min(vertical[col + y], uint32(nearest - y))
  result = newSeq[float32](n)
  var sites = newSeq[int](w)
  var starts = newSeq[float64](w)
  var costs = newSeq[uint32](w * BandRows)
  var y0 = 0
  while y0 < h:
    let rows = min(BandRows, h - y0)
    for x in 0 ..< w:
      let col = x * h + y0
      for y in 0 ..< rows: costs[y * w + x] = vertical[col + y]
    for y in 0 ..< rows:
      rowDistance(costs.toOpenArray(y * w, y * w + w - 1), result.toOpenArray((y0 + y) * w, (y0 + y) * w + w - 1), sites, starts)
    y0 += rows

