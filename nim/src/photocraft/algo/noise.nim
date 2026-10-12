## Add Noise, Median and Dust & Scratches, and the integer coordinate hash many filters use.

import std/[math, algorithm]
import ../geom, ./params, ./util

proc hash01*(x, y: int32; c, seed: uint32): float32 {.inline.} =
  ## A uniform [0, 1) value per (pixel, channel, seed).
  var h = (cast[uint32](x) * 0x8da6_b343'u32) xor (cast[uint32](y) * 0xd816_3841'u32) xor
          (c * 0xcb1a_b31f'u32) xor (seed * 0x9e37_79b9'u32)
  h = h xor (h shr 16)
  h = h * 0x7feb_352d'u32
  h = h xor (h shr 15)
  h = h * 0x846c_a68b'u32
  h = h xor (h shr 16)
  float32(h shr 8) / float32(1'u32 shl 24)

proc hash01*(x, y: int; c, seed: uint32): float32 {.inline.} = hash01(int32(x), int32(y), c, seed)

proc addNoise*(src: Image; o: Rect; ctx: Ctx; amount: float32; dist: Distribution; mono: bool; seed: uint32): seq[float32] =
  let n = src.ch
  let cc = (if ctx.alpha: n - 1 else: n)
  let a = max(amount, 0) / 100 * 0.5'f32
  result = src.crop(o)
  let w = max(o.width, 1)
  for i in 0 ..< (if n > 0: result.len div n else: 0):
    let base = i * n
    if ctx.alpha and result[base + n - 1] <= 0: continue
    let (x, y) = (o.x0 + i mod w, o.y0 + i div w)
    for c in 0 ..< cc:
      let ch = (if mono: 0'u32 else: uint32(c))
      let v = (case dist
        of diUniform: (hash01(x, y, ch, seed) - 0.5'f32) * 2 * a
        of diGaussian:
          let u1 = max(hash01(x, y, ch * 2 + 101, seed), 1e-7'f32)
          let u2 = hash01(x, y, ch * 2 + 102, seed)
          sqrt(-2 * ln(u1)) * cos(2 * float32(PI) * u2) * a * 0.5'f32)
      result[base + c] += v

proc discRows(r: int): seq[int] =
  for dy in -r .. r:
    let lim = r * r + r - dy * dy
    var hw = int(sqrt(float64(lim)))
    while hw * hw > lim: dec hw
    while (hw + 1) * (hw + 1) <= lim: inc hw
    result.add hw

proc medianRanked(src: Image; o: Rect; r: int; t: float32; hasT: bool): seq[float32] =
  let n = src.ch
  let win = o.inflate(r)
  let (ww, wh) = (win.width, win.height)
  let rows = discRows(r)
  var count = 0
  for hw in rows: count += 2 * hw + 1
  let mid = count div 2
  let (ow, oh) = (o.width, o.height)
  let ru = r
  result = newSeq[float32](ow * oh * n)
  var plane = newSeq[uint32](ww * wh)
  var values = newSeqOfCap[float32](ww * wh)
  var hist, blocks: seq[uint32]
  for c in 0 ..< n:
    values.setLen(0)
    for y in win.y0 ..< win.y1:
      for x in win.x0 ..< win.x1: values.add src.get(x, y, c)
    var eightBit = true
    for v in values:
      let k = round(v * 255)
      if not (k >= 0 and k <= 255 and cast[uint32](k / 255) == cast[uint32](v)):
        eightBit = false
        break
    var levels: seq[float32]
    if eightBit:
      for i, v in values: plane[i] = uint32(round(v * 255))
      levels = newSeq[float32](256)
      for k in 0 .. 255: levels[k] = float32(k) / 255
    else:
      var sorted = values
      sorted.sort(totalCmp)
      var dedup = newSeqOfCap[float32](sorted.len)
      for v in sorted:
        if dedup.len == 0 or cast[uint32](dedup[^1]) != cast[uint32](v): dedup.add v
      for i, v in values:
        plane[i] = uint32(dedup.lowerBound(v, totalCmp))
      levels = dedup
    let k = levels.len
    let sq = max(int(sqrt(float64(k))), 1)
    var shift = 0
    while (1 shl shift) <= sq: inc shift
    let bs = 1 shl shift
    hist = newSeq[uint32](k)
    blocks = newSeq[uint32]((k + bs - 1) div bs)
    for j, hw in rows:
      for idx in j * ww + ru - hw .. j * ww + ru + hw:
        let q = int(plane[idx])
        inc hist[q]
        inc blocks[q shr shift]
    var m = 0
    var below = 0
    for oy in 0 ..< oh:
      for step in 0 ..< ow:
        let ox = (if oy mod 2 == 0: step else: ow - 1 - step)
        let (cx, cy) = (ox + ru, oy + ru)
        if oy > 0 or step > 0:
          for j, hw in rows:
            var gone, nw: int
            if step == 0:
              let x = cx + j - ru
              (gone, nw) = ((cy - 1 - hw) * ww + x, (cy + hw) * ww + x)
            elif oy mod 2 == 0:
              let yy = (cy + j - ru) * ww
              (gone, nw) = (yy + cx - 1 - hw, yy + cx + hw)
            else:
              let yy = (cy + j - ru) * ww
              (gone, nw) = (yy + cx + 1 + hw, yy + cx - hw)
            let (g, q) = (int(plane[gone]), int(plane[nw]))
            dec hist[g]
            dec blocks[g shr shift]
            inc hist[q]
            inc blocks[q shr shift]
            if g < m: dec below
            if q < m: inc below
        while below > mid:
          if (m and (bs - 1)) == 0 and below - int(blocks[(m shr shift) - 1]) > mid:
            below -= int(blocks[(m shr shift) - 1])
            m -= bs
          else:
            dec m
            below -= int(hist[m])
        while true:
          if (m and (bs - 1)) == 0 and (m shr shift) < blocks.len and below + int(blocks[m shr shift]) <= mid:
            below += int(blocks[m shr shift])
            m += bs
          elif m < k and below + int(hist[m]) <= mid:
            below += int(hist[m])
            inc m
          else:
            break
        let mv = levels[min(m, k - 1)]
        let ov = src.get(o.x0 + ox, o.y0 + oy, c)
        result[(oy * ow + ox) * n + c] = (if hasT and abs(ov - mv) <= t: ov else: mv)

proc medianDirect(src: Image; o: Rect; r: int; t: float32; hasT: bool): seq[float32] =
  let n = src.ch
  var offs: seq[(int, int)]
  for dy in -r .. r:
    for dx in -r .. r:
      if dx * dx + dy * dy <= r * r + r: offs.add (dx, dy)
  var vals = newSeqOfCap[float32](offs.len)
  result = newSeqOfCap[float32](o.width * o.height * n)
  for y in o.y0 ..< o.y1:
    for x in o.x0 ..< o.x1:
      for c in 0 ..< n:
        vals.setLen(0)
        for (dx, dy) in offs: vals.add src.get(x + dx, y + dy, c)
        vals.sort(totalCmp)
        let m = vals[vals.len div 2]
        let ov = src.get(x, y, c)
        result.add (if hasT and abs(ov - m) <= t: ov else: m)

proc median*(src: Image; o: Rect; radius: float32; threshold = -1'f32): seq[float32] =
  ## Median over a disc of `radius`; with a threshold ≥ 0 (in levels, Dust & Scratches) pixels
  ## within it of the median are kept.
  let r = int(round(max(radius, 0)))
  if r == 0: return src.crop(o)
  let hasT = threshold >= 0
  let t = threshold / 255
  if r >= 2: medianRanked(src, o, r, t, hasT) else: medianDirect(src, o, r, t, hasT)
