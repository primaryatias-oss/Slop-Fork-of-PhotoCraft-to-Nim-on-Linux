## Image › Trap (prepress), port of `trap.rs`. Trapping spreads each ink slightly into neighbouring
## inked areas so that, if the press misregisters the plates, the colours still overlap at an edge
## instead of leaving a white gap. Every ink is spread by `width` px (grayscale dilation) but only
## *under* already-inked pixels, so internal colour edges get a trap band while the
## object-vs-paper edge is left alone. Clean-room approximation of Photoshop's Trap, which works on
## a flattened CMYK image. Operates on an interleaved buffer of `ch` channels per pixel (CMYK =
## inks 0..4, plus alpha).

import ../rmath

proc maxLine(o: var openArray[float32]; radius: int; get: proc(i: int): float32) =
  ## Prefix/suffix block maxima, with clipped edge windows and the original zero floor. One
  ## comparison per sample in each direction, independent of the trap width.
  let len = o.len
  if len == 0: return
  let r = min(radius, len - 1)
  template direct() =
    for x in 0 ..< len:
      var m = 0'f32
      for i in max(x - r, 0) .. min(x + r, len - 1): m = rmax(m, get(i))
      o[x] = m
  if r <= 2:
    direct()
    return
  var suffix = newSeq[float32](len)
  let blk = 2 * r + 1
  var acc = 0'f32
  var negativeZero = false
  for i in 0 ..< len:
    if i mod blk == 0: acc = 0
    let value = get(i)
    negativeZero = negativeZero or cast[uint32](value) == 0x8000_0000'u32
    acc = rmax(acc, value)
    o[i] = acc
  # f32::max may choose either sign of equal zero depending on operand order: keep the original
  # left-to-right fold for these uncommon lines.
  if negativeZero:
    direct()
    return
  acc = 0
  for i in countdown(len - 1, 0):
    if i == len - 1 or (i + 1) mod blk == 0: acc = 0
    acc = rmax(acc, get(i))
    suffix[i] = acc
  var res = newSeq[float32](len)
  for x in 0 ..< len:
    let (lo, hi) = (max(x - r, 0), min(x + r, len - 1))
    res[x] =
      if lo == 0: o[hi]
      elif hi == len - 1 and lo div blk == hi div blk: suffix[lo]
      else: rmax(suffix[lo], o[hi])
  for x in 0 ..< len: o[x] = res[x]

proc dilateChannelDirect(src: openArray[float32]; w, h, ch, c, r: int): seq[float32] =
  var tmp = newSeq[float32](w * h)
  for y in 0 ..< h:
    for x in 0 ..< w:
      var m = 0'f32
      for xx in max(x - r, 0) .. min(x + r, w - 1): m = rmax(m, src[(y * w + xx) * ch + c])
      tmp[y * w + x] = m
  result = newSeq[float32](w * h)
  for y in 0 ..< h:
    for x in 0 ..< w:
      var m = 0'f32
      for yy in max(y - r, 0) .. min(y + r, h - 1): m = rmax(m, tmp[yy * w + x])
      result[y * w + x] = m

proc dilateChannel(src: seq[float32]; w, h, ch, c, r: int): seq[float32] =
  ## A channel's square dilation, transposed (column-major) when `r > 2`.
  if r <= 2: return dilateChannelDirect(src, w, h, ch, c, r)
  var tmp = newSeq[float32](w * h)
  for y in 0 ..< h:
    maxLine(tmp.toOpenArray(y * w, y * w + w - 1), r, proc(x: int): float32 =
      let i = (y * w + x) * ch + c
      if i < src.len: src[i] else: 0'f32)
  result = newSeq[float32](w * h)
  for x in 0 ..< w:
    maxLine(result.toOpenArray(x * h, x * h + h - 1), r, proc(y: int): float32 =
      let i = y * w + x
      if i < tmp.len: tmp[i] else: 0'f32)

proc trap*(px: var seq[float32]; w, h, ch, width: int) =
  ## Traps an interleaved CMYK(+alpha) buffer in place. `width` is the trap in px (>= 1). Inks
  ## 0..4 are spread; any alpha channel (`ch > 4`) is untouched. A pixel keeps its original ink
  ## where it has no ink at all (paper), so only internal colour edges gain a trap band.
  if width <= 0 or w <= 0 or h <= 0 or ch < 4: return
  if w > high(int) div h or w * h > high(int) div ch or w * h * ch != px.len: return
  let inks = min(4, ch)
  let width = min(width, max(w, h))
  var dil: seq[seq[float32]]
  for c in 0 ..< inks: dil.add dilateChannel(px, w, h, ch, c, width)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = (if width <= 2: y * w + x else: x * h + y)
      let b = (y * w + x) * ch
      # Printed where any ink is present.
      var inked = false
      for c in 0 ..< inks:
        if px[b + c] > 1e-4'f32: inked = true
      if not inked: continue
      for c in 0 ..< inks: px[b + c] = rmax(px[b + c], dil[c][i])
