## File › Automate › Crop and Straighten Photos (port of `scancrop.rs`): find the photos on a
## flatbed scan, measure each one's rotation, and cut them out upright.
##
## 1. **Background**: the median colour of the image border (a scanner lid is uniform).
## 2. **Foreground**: pixels whose colour distance to the background exceeds a threshold chosen
##    by Otsu's method (N. Otsu, *A Threshold Selection Method from Gray-Level Histograms*, IEEE
##    SMC 1979) on a downsampled copy, then closed and opened morphologically (fills specks
##    inside photos, drops dust) and split into 8-connected components; tiny ones are dropped.
## 3. **Rotation**: the minimum-area enclosing rectangle of each component's convex hull
##    (H. Freeman, R. Shapira, *Determining the Minimum-Area Encasing Rectangle for an
##    Arbitrary Closed Curve*, CACM 1975: one side is collinear with a hull edge).
## 4. The rectangle is shrunk by a pixel (to drop the anti-aliased border) and returned in full
##    resolution; callers resample it upright.

import std/[math, algorithm]
import ./util

type
  FoundPhoto* = object
    ## A found photo: centre, size (sides measured along its own axes) and the rotation that
    ## makes it upright.
    center*: array[2, float64]
    width*, height*: float64
    angle*: float64 ## angle of the photo's x axis in the scan (degrees, `-45..=45`)
    area*: float64  ## area as a fraction of the scan

proc corners*(p: FoundPhoto): array[4, array[2, float64]] =
  ## The four corners in the scan, clockwise from the photo's top-left.
  let a = toRadians(p.angle)
  let (s, c) = (sin(a), cos(a))
  let (hw, hh) = (p.width / 2.0, p.height / 2.0)
  for i, (x, y) in [(-hw, -hh), (hw, -hh), (hw, hh), (-hw, hh)]:
    result[i] = [p.center[0] + c * x - s * y, p.center[1] + s * x + c * y]

proc otsu(v: openArray[float32]): float32 =
  const B = 256
  var hist: array[B, float64]
  for x in v:
    hist[min(asIdx(clamp(x, 0'f32, 1'f32) * float32(B - 1)), B - 1)] += 1.0
  var total = -0.0
  for h in hist: total += h
  var sum = -0.0
  for i, h in hist: sum += float64(i) * h
  var (wb, sb, best, thr) = (0.0, 0.0, 0.0, 0)
  for i, h in hist:
    wb += h
    if wb == 0.0: continue
    let wf = total - wb
    if wf == 0.0: break
    sb += float64(i) * h
    let (mb, mf) = (sb / wb, (sum - sb) / wf)
    let between = wb * wf * (mb - mf) * (mb - mf)
    if between > best:
      best = between
      thr = i
  (float32(thr) + 0.5'f32) / float32(B - 1)

proc morph(w, h: int; m: seq[bool]; r: int; dilate: bool): seq[bool] =
  ## Separable square structuring element.
  proc pass(src: seq[bool]; horizontal: bool): seq[bool] =
    result = newSeq[bool](w * h)
    for y in 0 ..< h:
      for x in 0 ..< w:
        var acc = not dilate
        for d in -r .. r:
          let (xx, yy) = (if horizontal: (x + d, y) else: (x, y + d))
          let v = (if xx < 0 or yy < 0 or xx >= w or yy >= h: false else: src[yy * w + xx])
          if dilate: acc = acc or v else: acc = acc and v
        result[y * w + x] = acc
  pass(pass(m, true), false)

proc cross(o, a, b: array[2, float64]): float64 =
  (a[0] - o[0]) * (b[1] - o[1]) - (a[1] - o[1]) * (b[0] - o[0])

proc convexHull(pts0: seq[array[2, float64]]): seq[array[2, float64]] =
  var pts = pts0
  pts.sort(proc(a, b: array[2, float64]): int =
    let c = totalCmp(a[0], b[0])
    if c != 0: c else: totalCmp(a[1], b[1]))
  var dedup: seq[array[2, float64]]
  for p in pts:
    if dedup.len == 0 or dedup[^1] != p: dedup.add p
  pts = dedup
  if pts.len < 3: return pts
  var lower: seq[array[2, float64]]
  for p in pts:
    while lower.len >= 2 and cross(lower[^2], lower[^1], p) <= 0.0: discard lower.pop()
    lower.add p
  var upper: seq[array[2, float64]]
  for i in countdown(pts.high, 0):
    let p = pts[i]
    while upper.len >= 2 and cross(upper[^2], upper[^1], p) <= 0.0: discard upper.pop()
    upper.add p
  discard lower.pop()
  discard upper.pop()
  lower & upper

proc minRect(hull: seq[array[2, float64]]): (array[2, float64], float64, float64, float64) =
  ## Minimum-area rectangle around a convex hull: (centre, width, height, angle in degrees).
  var best = (F64Max, [0.0, 0.0], 0.0, 0.0, 0.0)
  let n = hull.len
  for i in 0 ..< n:
    let (a, b) = (hull[i], hull[(i + 1) mod n])
    let th = arctan2(b[1] - a[1], b[0] - a[0])
    let (s, c) = (sin(th), cos(th))
    var (x0, y0, x1, y1) = (F64Max, F64Max, -F64Max, -F64Max)
    for p in hull:
      let (u, v) = (c * p[0] + s * p[1], -s * p[0] + c * p[1])
      x0 = rmin(x0, u)
      x1 = rmax(x1, u)
      y0 = rmin(y0, v)
      y1 = rmax(y1, v)
    let area = (x1 - x0) * (y1 - y0)
    if area < best[0]:
      let (cu, cv) = ((x0 + x1) / 2.0, (y0 + y1) / 2.0)
      best = (area, [c * cu - s * cv, s * cu + c * cv], x1 - x0, y1 - y0, toDegrees(th))
  var (w, h, ang) = (best[2], best[3], best[4])
  # Normalise to −45..45, swapping the sides when turning by 90°.
  while ang > 45.0:
    ang -= 90.0
    swap(w, h)
  while ang < -45.0:
    ang += 90.0
    swap(w, h)
  (best[1], w, h, ang)

proc findPhotos*(w, h: int; rgba: openArray[array[4, float32]]): seq[FoundPhoto] =
  ## Finds the photos on a scan given as straight RGBA rows (`w × h`). Photos are returned in
  ## reading order (top to bottom, then left to right). Empty for scans that are too small or
  ## whose buffer does not match the size.
  if w < 8 or h < 8 or rgba.len div w < h: return @[]
  # Work on ≤ 800 px.
  let k = max((max(w, h) + 799) div 800, 1)
  let (sw, sh) = ((w + k - 1) div k, (h + k - 1) div k)
  var small = newSeq[array[3, float32]](sw * sh)
  var cnt = newSeq[uint32](sw * sh)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let i = (y div k) * sw + x div k
      let p = rgba[y * w + x]
      for c in 0 .. 2: small[i][c] += p[c]
      cnt[i] += 1
  for i in 0 ..< small.len:
    for c in 0 .. 2: small[i][c] /= float32(max(cnt[i], 1'u32))
  # Background = per-channel median of the border.
  var border: seq[array[3, float32]]
  for x in 0 ..< sw:
    border.add small[x]
    border.add small[(sh - 1) * sw + x]
  for y in 0 ..< sh:
    border.add small[y * sw]
    border.add small[y * sw + sw - 1]
  var bg: array[3, float32]
  for c in 0 .. 2:
    var v = newSeq[float32](border.len)
    for i, p in border: v[i] = p[c]
    v.sort(proc(a, b: float32): int = totalCmp(a, b))
    bg[c] = v[v.len div 2]
  let s3 = sqrt(3'f32)
  var dist = newSeq[float32](small.len)
  for i, p in small:
    let (d0, d1, d2) = (p[0] - bg[0], p[1] - bg[1], p[2] - bg[2])
    dist[i] = sqrt(d0 * d0 + d1 * d1 + d2 * d2) / s3
  let thr = rmax(otsu(dist), 0.04'f32)
  var mask = newSeq[bool](dist.len)
  for i, d in dist: mask[i] = d > thr
  let r = max(max(sw, sh) div 200, 1)
  mask = morph(sw, sh, morph(sw, sh, mask, r, true), r, false)
  mask = morph(sw, sh, morph(sw, sh, mask, 1, false), 1, true)
  # Connected components (8-connected).
  var label = newSeq[uint32](sw * sh)
  for l in label.mitems: l = high(uint32)
  var comps: seq[seq[int]]
  for s in 0 ..< sw * sh:
    if not mask[s] or label[s] != high(uint32): continue
    let id = uint32(comps.len)
    var stack = @[s]
    label[s] = id
    var px: seq[int]
    while stack.len > 0:
      let i = stack.pop()
      px.add i
      let (x, y) = (i mod sw, i div sw)
      for dy in -1 .. 1:
        for dx in -1 .. 1:
          let (xx, yy) = (x + dx, y + dy)
          if xx < 0 or yy < 0 or xx >= sw or yy >= sh: continue
          let j = yy * sw + xx
          if mask[j] and label[j] == high(uint32):
            label[j] = id
            stack.add j
    comps.add px
  let total = float64(sw * sh)
  var found: seq[FoundPhoto]
  for c in comps:
    if not (float64(c.len) > total * 0.01): continue
    # Hull of the boundary pixels' corners (in full-resolution px).
    var pts: seq[array[2, float64]]
    for i in c:
      let (x, y) = (i mod sw, i div sw)
      if x == 0 or y == 0 or x + 1 == sw or y + 1 == sh or not mask[i - 1] or not mask[i + 1] or
          not mask[i - sw] or not mask[i + sw]:
        let (fx, fy) = (float64(x), float64(y))
        for p in [[fx, fy], [fx + 1.0, fy], [fx, fy + 1.0], [fx + 1.0, fy + 1.0]]:
          pts.add [p[0] * float64(k), p[1] * float64(k)]
    let hull = convexHull(pts)
    let (center, wd, ht, ang) = minRect(hull)
    let inset = float64(k)
    found.add FoundPhoto(center: center, width: rmax(wd - 2.0 * inset, 1.0), height: rmax(ht - 2.0 * inset, 1.0),
                         angle: ang, area: float64(c.len) / total)
  # Reading order: rows by centre y (bands of a third of the smallest photo), then x.
  var band = F64Max
  for p in found: band = rmin(band, rmin(p.height, p.width))
  band = rmax(band, 1.0) / 3.0
  found.sort(proc(a, b: FoundPhoto): int =
    let c = totalCmp(floor(a.center[1] / band), floor(b.center[1] / band))
    if c != 0: c else: totalCmp(a.center[0], b.center[0]))
  found
