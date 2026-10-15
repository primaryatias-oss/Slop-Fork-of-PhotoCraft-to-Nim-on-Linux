## Minimum, Maximum, Offset (port of `other.rs`).

import std/math
import ../geom
import params, image, util
import ../rmath

proc runningExtreme*(v: openArray[float32]; k: int; isMax: bool; o: var seq[float32]) =
  ## `o[i]` = extreme of `v[i ..< i + k]` for every full window (van Herk / Gil-Werman).
  o.setLen(0)
  let n = v.len
  if k <= 1:
    o.add v
    return
  if n < k: return
  template pick(a, b: float32): float32 = (if isMax: rmax(a, b) else: rmin(a, b))
  var pre = @v
  var suf = @v
  for i in 1 ..< n:
    if i mod k != 0: pre[i] = pick(pre[i - 1], v[i])
  for i in countdown(n - 2, 0):
    if (i + 1) mod k != 0: suf[i] = pick(suf[i + 1], v[i])
  for i in 0 .. n - k: o.add pick(suf[i], pre[i + k - 1])

proc minMax*(src: Image; o: Rect; radius: float32; isMax: bool): seq[float32] =
  ## Minimum or Maximum over a square, separable (rows, then columns).
  let n = src.ch
  let r = asI32(float64(round(rmax(radius, 0))))
  if r == 0: return src.crop(o)
  let k = 2 * r + 1
  let win = rect(o.x0, o.y0 - r, o.x1, o.y1 + r)
  let (ww, wh) = (win.width, win.height)
  var tmp = newSeq[float32](ww * wh * n)
  var line, ext: seq[float32]
  for y in win.y0 ..< win.y1:
    let at = (y - win.y0) * ww * n
    for c in 0 ..< n:
      line.setLen(0)
      for x in win.x0 - r ..< win.x1 + r: line.add src.get(x, y, c)
      runningExtreme(line, k, isMax, ext)
      for x, v in ext: tmp[at + x * n + c] = v
  let (ow, oh) = (o.width, o.height)
  result = newSeq[float32](ow * oh * n)
  for x in 0 ..< ow:
    for c in 0 ..< n:
      line.setLen(0)
      for y in 0 ..< wh: line.add tmp[(y * ww + x) * n + c]
      runningExtreme(line, k, isMax, ext)
      for y, v in ext: result[(y * ow + x) * n + c] = v

proc minMaxRound*(src: Image; o: Rect; radius: float32; isMax: bool): seq[float32] =
  ## Minimum or Maximum over a disc (Preserve: Roundness): each row of the disc is a running
  ## extreme, so the cost is O(radius) per pixel.
  let n = src.ch
  let r = rmax(radius, 0)
  let ri = asI32(float64(floor(r)))
  if ri == 0: return src.crop(o)
  let w = o.width
  result = newSeq[float32](w * o.height * n)
  for v in result.mitems: v = (if isMax: -F32Max else: F32Max)
  var row, ext: seq[float32]
  for dy in -ri .. ri:
    # Half-width of the disc on this row (the epsilon keeps exact squares).
    let hw = asI32(float64(floor(sqrt(rmax(r * r - float32(dy * dy), 0)) + 1e-4'f32)))
    for y in o.y0 ..< o.y1:
      let at = (y - o.y0) * w * n
      for c in 0 ..< n:
        row.setLen(0)
        for x in o.x0 - hw ..< o.x1 + hw: row.add src.get(x, y + dy, c)
        runningExtreme(row, 2 * hw + 1, isMax, ext)
        for x, v in ext:
          let i = at + x * n + c
          result[i] = (if isMax: rmax(result[i], v) else: rmin(result[i], v))

proc minMaxPreserve*(src: Image; o: Rect; radius: float32; isMax: bool; preserve: Preserve): seq[float32] =
  ## Minimum or Maximum with the dialog's Preserve option (square or disc).
  case preserve
  of prSquareness: minMax(src, o, radius, isMax)
  of prRoundness: minMaxRound(src, o, radius, isMax)

proc edgeOf*(u: UndefinedAreas): Edge =
  case u
  of uaWrap: edWrap
  of uaRepeat: edRepeat
  of uaTransparent: edTransparent

proc offset*(src: Image; o: Rect; ctx: Ctx; dx, dy: int; undefined: UndefinedAreas): seq[float32] =
  ## Shifts the bounds' content by whole pixels.
  let n = src.ch
  let edge = edgeOf(undefined)
  result = newSeqOfCap[float32](o.width * o.height * n)
  for y in o.y0 ..< o.y1:
    for x in o.x0 ..< o.x1:
      for c in 0 ..< n: result.add src.getEdge(x - dx, y - dy, c, edge, ctx.bounds)
