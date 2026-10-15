## A float pixel window (interleaved channels over a rect) that filters read from and write to.

import std/math
import ../geom, ../raster

type
  Edge* = enum edTransparent, edRepeat, edWrap
  Image* = object
    rect*: Rect
    ch*: int
    data*: seq[float32]

proc newImage*(r: Rect; ch: int): Image =
  Image(rect: r, ch: ch, data: newSeq[float32](max(r.width * r.height * ch, 0)))

proc readImage*(s: Surface; r: Rect): Image =
  Image(rect: r, ch: s.channels, data: s.readRegion(r))

proc w*(img: Image): int {.inline.} = img.rect.width
proc h*(img: Image): int {.inline.} = img.rect.height

proc index*(img: Image; x, y: int): int {.inline.} =
  ((y - img.rect.y0) * img.rect.width + (x - img.rect.x0)) * img.ch

proc get*(img: Image; x, y, c: int): float32 {.inline.} =
  if img.rect.contains(x, y): img.data[img.index(x, y) + c] else: 0

proc readClamped*(s: Surface; r, extent: Rect): Image =
  ## `r` read from `s`, with pixels outside `extent` replaced by the nearest pixel inside it.
  let inner = r.intersect(extent)
  if inner.isEmpty or inner == r: return readImage(s, r)
  let src = readImage(s, inner)
  result = newImage(r, src.ch)
  let ch = src.ch
  let w = r.width
  for y in r.y0 ..< r.y1:
    let sy = clamp(y, inner.y0, inner.y1 - 1)
    let row = (y - r.y0) * w * ch
    for x in r.x0 ..< r.x1:
      let sx = clamp(x, inner.x0, inner.x1 - 1)
      let d = row + (x - r.x0) * ch
      let s0 = src.index(sx, sy)
      for c in 0 ..< ch: result.data[d + c] = src.data[s0 + c]

proc px*(img: Image; x, y: int): seq[float32] =
  let i = img.index(x, y)
  img.data[i ..< i + img.ch]

proc getEdge*(img: Image; x, y, c: int; edge: Edge; area: Rect): float32 {.inline.} =
  if area.isEmpty: return 0
  if area.contains(x, y): return img.get(x, y, c)
  case edge
  of edTransparent: 0
  of edRepeat: img.get(clamp(x, area.x0, area.x1 - 1), clamp(y, area.y0, area.y1 - 1), c)
  of edWrap:
    let (w, h) = (max(area.width, 1), max(area.height, 1))
    img.get(area.x0 + remEuclid(x - area.x0, w), area.y0 + remEuclid(y - area.y0, h), c)

proc sample*(img: Image; x, y: float32; edge: Edge; area: Rect; alpha: bool; outp: var openArray[float32]) =
  ## Bilinear sample at `(x, y)` (pixel centres at +0.5), alpha-weighted when `alpha`.
  let (fx, fy) = (x - 0.5'f32, y - 0.5'f32)
  let (x0f, y0f) = (floor(fx), floor(fy))
  let (ax, ay) = (fx - x0f, fy - y0f)
  let (x0, y0) = (int(x0f), int(y0f))
  let n = img.ch
  for v in outp.mitems: v = 0
  var accA = 0'f32
  for (dx, dy, wgt) in [(0, 0, (1 - ax) * (1 - ay)), (1, 0, ax * (1 - ay)), (0, 1, (1 - ax) * ay), (1, 1, ax * ay)]:
    if wgt <= 0: continue
    let a = (if alpha: img.getEdge(x0 + dx, y0 + dy, n - 1, edge, area) else: 1'f32)
    for c in 0 ..< (if alpha: n - 1 else: n):
      outp[c] += img.getEdge(x0 + dx, y0 + dy, c, edge, area) * a * wgt
    accA += a * wgt
  if alpha:
    if accA > 0:
      for c in 0 ..< n - 1: outp[c] /= accA
    outp[n - 1] = accA

proc crop*(img: Image; o: Rect): seq[float32] =
  ## The pixels of `o` (zero outside the image), row-major interleaved.
  result = newSeqOfCap[float32](max(o.width * o.height * img.ch, 0))
  for y in o.y0 ..< o.y1:
    for x in o.x0 ..< o.x1:
      for c in 0 ..< img.ch: result.add img.get(x, y, c)

proc premultiply*(data: var openArray[float32]; ch: int; alpha: bool) =
  if not alpha or ch <= 0: return
  var i = 0
  while i + ch <= data.len:
    let a = data[i + ch - 1]
    for k in 0 ..< ch - 1: data[i + k] *= a
    i += ch

proc unpremultiply*(data: var openArray[float32]; ch: int; alpha: bool) =
  if not alpha or ch <= 0: return
  var i = 0
  while i + ch <= data.len:
    let a = data[i + ch - 1]
    for k in 0 ..< ch - 1: data[i + k] = (if a > 1e-7'f32: data[i + k] / a else: 0)
    i += ch
