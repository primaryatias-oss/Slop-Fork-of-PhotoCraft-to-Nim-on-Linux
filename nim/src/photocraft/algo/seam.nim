## Content-aware scaling by seam carving (Edit › Content-Aware Scale; port of `seam.rs`).
##
## S. Avidan, A. Shamir, *Seam Carving for Content-Aware Image Resizing*, SIGGRAPH 2007: a seam
## is an 8-connected path of one pixel per row (vertical seam) found by dynamic programming over a
## gradient-magnitude energy map. Shrinking removes the lowest-energy seams one at a time;
## enlarging finds the `k` lowest seams on a shrinking copy and duplicates them (averaging each
## with its right neighbour) so the same seam is not stretched repeatedly. Heights are handled by
## transposing.
##
## A protect map (`0..=1`, e.g. an alpha channel or detected skin tones) adds a large energy so
## seams avoid protected pixels. Buffers are interleaved `w × h × ch` normalised floats of any
## colour model and depth (alpha is carried like any other channel). Deterministic.

import std/[math, options, algorithm]
import ../raster
import ./util

const
  ProtectEnergy = 1.0e4'f32 ## energy added per unit of protection: far above any gradient
  SeamSearchMaxHeight = 512
    ## Keep full horizontal detail while reducing the height used to find vertical seams.
  ProxyShrinkMinHeight = 2_048
  ProxyShrinkMinSeams = 64

proc energyAt(img: openArray[float32]; stride, cw, h, ch, x, y: int): float32 {.inline.} =
  ## Energy of one pixel of a `cw`-wide image whose rows are `stride` pixels apart. Shared by
  ## the full map and the per-seam updates so both give bit-identical values.
  let (xl, xr) = (max(x - 1, 0), min(x + 1, cw - 1))
  let (yu, yd) = (max(y - 1, 0), min(y + 1, h - 1))
  template at(xx, yy, c: int): float32 = img[(yy * stride + xx) * ch + c]
  var s = 0'f32
  for c in 0 ..< ch:
    let v = at(x, y, c)
    s += abs(at(xr, y, c) - v) + abs(v - at(xl, y, c))
    s += abs(at(x, yd, c) - v) + abs(v - at(x, yu, c))
  s * 0.5'f32

proc validSize(w, h, ch, len: int): bool =
  w >= 0 and h >= 0 and ch >= 0 and (w == 0 or h <= high(int) div w) and
    (w * h == 0 or ch <= high(int) div (w * h)) and len >= w * h * ch

proc energy*(w, h, ch: int; img: openArray[float32]): seq[float32] =
  ## Gradient-magnitude energy (the paper's e1, `|∂x| + |∂y|` summed over channels). Forward and
  ## backward differences are both counted so one-pixel lines carry energy too. Zeros for a
  ## buffer that does not match the size.
  if not validSize(w, h, ch, img.len): return @[]
  result = newSeq[float32](w * h)
  if w == 0 or h == 0: return
  for y in 0 ..< h:
    for x in 0 ..< w: result[y * w + x] = energyAt(img, w, w, h, ch, x, y)

type Dp = object
  ## Scratch buffers for the seam search, reused across seams.
  cost, next: seq[float32]
  `from`: seq[uint8] ## per pixel: 0 = up-left, 1 = up, 2 = up-right

proc seam(dp: var Dp; stride, cw, h: int; energy: openArray[float32]): seq[int] =
  ## The lowest-energy seam of a `cw`-wide, `h`-tall energy map with rows `stride` apart. Ties go
  ## to the pixel straight up, then up-left, then up-right; at the bottom, to the leftmost end.
  dp.cost.setLen(cw)
  for x in 0 ..< cw: dp.cost[x] = energy[x]
  dp.next.setLen(cw)
  if dp.`from`.len < cw * h: dp.`from`.setLen(cw * h)
  for y in 1 ..< h:
    let e = y * stride
    let f = y * cw
    if cw == 1:
      dp.next[0] = dp.cost[0] + energy[e]
      dp.`from`[f] = 1
    else:
      block:
        let (b, d) = (if dp.cost[1] < dp.cost[0]: (dp.cost[1], 2'u8) else: (dp.cost[0], 1'u8))
        dp.next[0] = b + energy[e]
        dp.`from`[f] = d
      for x in 1 ..< cw - 1:
        let (c0, c1, c2) = (dp.cost[x - 1], dp.cost[x], dp.cost[x + 1])
        var (b, d) = (if c0 < c1: (c0, 0'u8) else: (c1, 1'u8))
        if c2 < b: (b, d) = (c2, 2'u8)
        dp.next[x] = b + energy[e + x]
        dp.`from`[f + x] = d
      let (b, d) = (if dp.cost[cw - 2] < dp.cost[cw - 1]: (dp.cost[cw - 2], 0'u8) else: (dp.cost[cw - 1], 1'u8))
      dp.next[cw - 1] = b + energy[e + cw - 1]
      dp.`from`[f + cw - 1] = d
    swap(dp.cost, dp.next)
  var x = 0
  for i in 1 ..< cw:
    if totalCmp(dp.cost[i], dp.cost[x]) < 0: x = i
  result = newSeq[int](h)
  for y in countdown(h - 1, 0):
    result[y] = x
    if y > 0:
      case dp.`from`[y * cw + x]
      of 0: x -= 1
      of 2: x += 1
      else: discard

proc findSeam*(w, h: int; energy: openArray[float32]): seq[int] =
  ## The lowest-energy vertical seam: one x per row.
  if w <= 0 or h <= 0 or energy.len div w < h: return newSeq[int](max(h, 0))
  var dp: Dp
  dp.seam(w, w, h, energy)

type Carver = object
  ## Shrinks an image one seam at a time in place. Rows keep their original stride, so removing
  ## a seam shifts only the rest of each row, and the energy map is updated only next to the
  ## removed seam instead of being recomputed.
  stride, cw, h, ch: int
  img: seq[float32]
  prot: Option[seq[float32]]
  e: seq[float32]            ## energy plus protection, as the seam search sees it
  idx: Option[seq[int]]      ## original column of each pixel, when tracking
  dp: Dp

proc newCarver(w, h, ch: int; img: seq[float32]; prot: Option[seq[float32]]; track: bool): Carver =
  var e = energy(w, h, ch, img)
  if prot.isSome:
    let p = prot.get
    for i in 0 ..< min(e.len, p.len): e[i] += clamp(p[i], 0'f32, 1'f32) * ProtectEnergy
  var idx = none(seq[int])
  if track:
    var v = newSeq[int](w * h)
    for y in 0 ..< h:
      for x in 0 ..< w: v[y * w + x] = x
    idx = some(v)
  Carver(stride: w, cw: w, h: h, ch: ch, img: img, prot: prot, e: e, idx: idx)

proc shiftRows[T](buf: var seq[T]; rowLen: int; seam: openArray[int]; n, cw: int) =
  ## Removes pixel `seam[y]` (of `n` values) from each `rowLen`-long row whose first `cw` pixels
  ## are in use, by shifting the rest of the row left.
  for y, x in seam:
    let r = y * rowLen
    for k in (x + 1) * n ..< cw * n: buf[r + k - n] = buf[r + k]

proc updateEnergy(c: var Carver; seam: openArray[int]) =
  ## Recomputes the energy of the pixels whose neighbours changed when `seam` was removed: in row
  ## y, from one left of the leftmost of the seam's x in rows y-1..=y+1 to the rightmost.
  let (stride, cw, h, ch) = (c.stride, c.cw, c.h, c.ch)
  if cw == 0: return
  for y in 0 ..< h:
    var (lo, hi) = (high(int), low(int))
    for k in max(y - 1, 0) ..< min(y + 2, h):
      lo = min(lo, seam[k])
      hi = max(hi, seam[k])
    lo = max(lo - 1, 0)
    hi = min(hi, cw - 1)
    for x in lo .. hi:
      var v = energyAt(c.img, stride, cw, h, ch, x, y)
      if c.prot.isSome: v += clamp(c.prot.get[y * stride + x], 0'f32, 1'f32) * ProtectEnergy
      c.e[y * stride + x] = v

proc removeSeam(c: var Carver): seq[int] =
  ## Removes the lowest-energy seam. Returns, per row, the removed column (the original column
  ## when tracking).
  let (stride, cw, h, ch) = (c.stride, c.cw, c.h, c.ch)
  let s = c.dp.seam(stride, cw, h, c.e)
  if c.idx.isSome:
    result = newSeq[int](h)
    for y, x in s: result[y] = c.idx.get[y * stride + x]
  else:
    result = s
  shiftRows(c.img, stride * ch, s, ch, cw)
  shiftRows(c.e, stride, s, 1, cw)
  if c.prot.isSome: shiftRows(c.prot.get, stride, s, 1, cw)
  if c.idx.isSome: shiftRows(c.idx.get, stride, s, 1, cw)
  c.cw -= 1
  c.updateEnergy(s)

proc intoPixels(c: Carver): seq[float32] =
  ## The carved pixels, `cw × h × ch`, rows packed.
  let (row, used) = (c.stride * c.ch, c.cw * c.ch)
  if row == used: return c.img
  result = newSeqOfCap[float32](used * c.h)
  for y in 0 ..< c.h:
    for k in 0 ..< used: result.add c.img[y * row + k]

proc transpose(w, h, ch: int; img: openArray[float32]): seq[float32] =
  result = newSeq[float32](img.len)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let (s, d) = ((y * w + x) * ch, (x * h + y) * ch)
      for c in 0 ..< ch: result[d + c] = img[s + c]

proc carveWidthProxyHeight(w, h, ch: int; img: openArray[float32]; protect: Option[seq[float32]]; newW: int;
                           ctl: Interrupt): seq[float32] =
  ## Finds large-image seams on vertically averaged rows, then applies those paths to the
  ## original pixels. The proxy has the same width, so each seam still removes exactly one source
  ## pixel per row.
  let step = (h + SeamSearchMaxHeight - 1) div SeamSearchMaxHeight
  let proxyH = (h + step - 1) div step
  let rowLen = w * ch
  var proxy = newSeq[float32](proxyH * rowLen)
  for py in 0 ..< proxyH:
    let y0 = py * step
    let y1 = min(y0 + step, h)
    let divisor = float32(y1 - y0)
    for y in y0 ..< y1:
      for k in 0 ..< rowLen: proxy[py * rowLen + k] += img[y * rowLen + k] / divisor
  var proxyProtect = none(seq[float32])
  if protect.isSome:
    let mask = protect.get
    var reduced = newSeq[float32](proxyH * w)
    for py in 0 ..< proxyH:
      let y0 = py * step
      let y1 = min(y0 + step, h)
      let divisor = float32(y1 - y0)
      for y in y0 ..< y1:
        for x in 0 ..< w: reduced[py * w + x] += mask[y * w + x] / divisor
    proxyProtect = some(reduced)
  let seamCount = w - newW
  var carver = newCarver(w, proxyH, ch, proxy, proxyProtect, true)
  var removed: seq[seq[int]]
  while carver.cw > newW:
    ctl.check()
    removed.add carver.removeSeam()
    ctl.progress(float32(removed.len) / float32(seamCount))
  var removedByRow = newSeq[seq[int]](proxyH)
  for proxyY in 0 ..< proxyH:
    var columns = newSeq[int](removed.len)
    for i, path in removed: columns[i] = path[proxyY]
    columns.sort()
    removedByRow[proxyY] = columns
  result = newSeqOfCap[float32](newW * h * ch)
  var columns = newSeq[int](seamCount)
  for y in 0 ..< h:
    ctl.check()
    let proxyY = min(y div step, proxyH - 1)
    let nextProxyY = min(proxyY + 1, proxyH - 1)
    let blend = float32(y mod step) / float32(step)
    let (lower, upper) = (removedByRow[proxyY], removedByRow[nextProxyY])
    var minX = 0
    for i in 0 ..< columns.len:
      let interpolated = float32(lower[i]) + (float32(upper[i]) - float32(lower[i])) * blend
      let maxX = w - (seamCount - i)
      columns[i] = clamp(asIdx(round(interpolated)), minX, maxX)
      minX = columns[i] + 1
    var next = 0
    for x in 0 ..< w:
      if next < columns.len and columns[next] == x: inc next
      else:
        for c in 0 ..< ch: result.add img[(y * w + x) * ch + c]
  ctl.progress(1)

proc carveWidthWith*(w, h, ch: int; img: openArray[float32]; protect: Option[seq[float32]]; newW0: int;
                     ctl: Interrupt): seq[float32] =
  ## Changes the width of `img` to `newW` by removing or duplicating vertical seams. Checks `ctl`
  ## before every seam (raises `Cancelled`) and reports progress per seam. A buffer that does not
  ## match the size is returned unchanged; a protect map of the wrong size is ignored.
  let newW = max(newW0, 1)
  if newW == w or w <= 0 or h <= 0 or not validSize(w, h, ch, img.len): return @img
  let protect = (if protect.isSome and protect.get.len >= w * h: protect else: none(seq[float32]))
  let seams = float32(max(abs(newW - w), 1))
  if newW < w:
    if h >= ProxyShrinkMinHeight and w - newW >= ProxyShrinkMinSeams:
      return carveWidthProxyHeight(w, h, ch, img, protect, newW, ctl)
    var carver = newCarver(w, h, ch, @img, protect, false)
    while carver.cw > newW:
      ctl.check()
      ctl.progress(float32(w - carver.cw) / seams)
      discard carver.removeSeam()
    return carver.intoPixels()
  # Enlarge in rounds of at most half the current width, each duplicating the k lowest seams.
  var (cur, cw) = (@img, w)
  var prot = protect
  while cw < newW:
    let k = min(newW - cw, max(cw div 2, 1))
    # Find k seams on a shrinking copy, mapping each back to original columns.
    var carver = newCarver(cw, h, ch, cur, prot, true)
    var dup = newSeq[seq[int]](h)
    for _ in 0 ..< k:
      # A 1 px wide image's only seam is its one column: take it, so it is duplicated.
      if carver.cw == 0: break
      ctl.check()
      ctl.progress(float32(max(cw + (cw - carver.cw) - w, 0)) / seams)
      let r = carver.removeSeam()
      for y in 0 ..< h: dup[y].add r[y]
    let added = dup[0].len
    if added == 0: break
    let nw = cw + added
    var o = newSeqOfCap[float32](nw * h * ch)
    var nprot = (if prot.isSome: some(newSeqOfCap[float32](nw * h)) else: none(seq[float32]))
    for y in 0 ..< h:
      dup[y].sort()
      let d = dup[y]
      let row = y * cw * ch
      var di = 0
      for x in 0 ..< cw:
        for c in 0 ..< ch: o.add cur[row + x * ch + c]
        if prot.isSome: nprot.get.add prot.get[y * cw + x]
        while di < d.len and d[di] == x:
          let xr = min(x + 1, cw - 1)
          for c in 0 ..< ch: o.add (cur[row + x * ch + c] + cur[row + xr * ch + c]) * 0.5'f32
          if prot.isSome: nprot.get.add prot.get[y * cw + x]
          inc di
    cur = o
    prot = nprot
    cw = nw
  cur

proc carveWidth*(w, h, ch: int; img: openArray[float32]; protect: Option[seq[float32]]; newW: int): seq[float32] =
  carveWidthWith(w, h, ch, img, protect, newW, NoInterrupt)

proc resizeBilinear*(w, h, ch: int; img: openArray[float32]; nw, nh: int): seq[float32] =
  ## Plain bilinear resize (the non-content-aware part of the Amount blend).
  if nw <= 0 or nh <= 0 or ch <= 0: return @[]
  result = newSeq[float32](nw * nh * ch)
  if w <= 0 or h <= 0 or not validSize(w, h, ch, img.len): return
  for y in 0 ..< nh:
    let fy = clamp((float32(y) + 0.5'f32) * float32(h) / float32(nh) - 0.5'f32, 0'f32, float32(h - 1))
    let (y0, ty) = (int(floor(fy)), fy - trunc(fy))
    let y1 = min(y0 + 1, h - 1)
    for x in 0 ..< nw:
      let fx = clamp((float32(x) + 0.5'f32) * float32(w) / float32(nw) - 0.5'f32, 0'f32, float32(w - 1))
      let (x0, tx) = (int(floor(fx)), fx - trunc(fx))
      let x1 = min(x0 + 1, w - 1)
      for c in 0 ..< ch:
        template p(xx, yy: int): float32 = img[(yy * w + xx) * ch + c]
        let top = p(x0, y0) + (p(x1, y0) - p(x0, y0)) * tx
        let bot = p(x0, y1) + (p(x1, y1) - p(x0, y1)) * tx
        result[(y * nw + x) * ch + c] = top + (bot - top) * ty

proc carveWith*(w, h, ch: int; img: openArray[float32]; protect: Option[seq[float32]]; newW0, newH0: int;
                ctl: Interrupt): seq[float32] =
  ## Content-aware resize of `img` to `newW × newH` (width first, then height by transposing).
  ## Checks `ctl` before every seam (a cancel takes effect within one seam; raises `Cancelled`)
  ## and reports progress over both passes.
  let (newW, newH) = (max(newW0, 1), max(newH0, 1))
  if w <= 0 or h <= 0 or not validSize(w, h, ch, img.len): return @img
  # Split progress between the passes by their seam counts.
  let (sw, sh) = (float32(abs(newW - w)), float32(abs(newH - h)))
  let split = (if sw + sh > 0: sw / (sw + sh) else: 1'f32)
  let cancel = proc(): bool = ctl.cancelled
  let first = newInterrupt(cancel, proc(f: float32) = ctl.progress(f * split))
  let wide = carveWidthWith(w, h, ch, img, protect, newW, first)
  if newH == h: return wide
  # Protection follows the width change by plain resampling (it only steers seams).
  var protT = none(seq[float32])
  if protect.isSome and protect.get.len >= w * h:
    protT = some(transpose(newW, h, 1, resizeBilinear(w, h, 1, protect.get, newW, h)))
  let t = transpose(newW, h, ch, wide)
  let second = newInterrupt(cancel, proc(f: float32) = ctl.progress(split + f * (1'f32 - split)))
  let tall = carveWidthWith(h, newW, ch, t, protT, newH, second)
  transpose(newH, newW, ch, tall)

proc carve*(w, h, ch: int; img: openArray[float32]; protect: Option[seq[float32]]; newW, newH: int): seq[float32] =
  carveWith(w, h, ch, img, protect, newW, newH, NoInterrupt)

proc skinMask*(w, h, ch: int; img: openArray[float32]): seq[float32] =
  ## Skin-tone protection map (1 = skin) for RGB(A) buffers: the explicit RGB rule of J. Kovač,
  ## P. Peer, F. Solina, *Human Skin Colour Clustering for Face Detection*, EUROCON 2003.
  if w < 0 or h < 0: return @[]
  result = newSeq[float32](w * h)
  if ch < 3 or not validSize(w, h, ch, img.len): return
  for i in 0 ..< w * h:
    let (r, g, b) = (img[i * ch] * 255'f32, img[i * ch + 1] * 255'f32, img[i * ch + 2] * 255'f32)
    let (mx, mn) = (rmax(rmax(r, g), b), rmin(rmin(r, g), b))
    if r > 95 and g > 40 and b > 20 and mx - mn > 15 and abs(r - g) > 15 and r > g and r > b: result[i] = 1
