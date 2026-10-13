## Filter ▸ Video: De-Interlace and NTSC Colors (port of `video.rs`).

import std/math
import ../geom, ../color, ./params, ./fxutil

proc deinterlace*(src: Image; o: Rect; ctx: Ctx; eliminateEven, interpolate: bool): seq[float32] =
  ## Removes one field (odd lines counted from the bounds' top are even offsets) and rebuilds it
  ## from the neighbouring kept lines by duplication or premultiplied interpolation.
  let n = src.ch
  let b = ctx.bounds
  result = src.crop(o)
  var acc: array[MaxC, float32]
  if n == 0 or n > MaxC: return
  for i in 0 ..< result.len div n:
    let (x, y) = xy(o, i)
    if not b.contains(x, y): continue
    let oddLine = (y - b.y0) mod 2 == 0
    if oddLine == eliminateEven: continue
    let hasAbove = y > b.y0
    let hasBelow = y + 1 < b.y1
    var rows: array[2, int]
    var cnt: int
    if interpolate and hasAbove and hasBelow: (rows, cnt) = ([y - 1, y + 1], 2)
    elif hasAbove: (rows, cnt) = ([y - 1, y - 1], 1)
    elif hasBelow: (rows, cnt) = ([y + 1, y + 1], 1)
    else: continue
    for c in 0 ..< n: acc[c] = 0
    let k = 1'f32 / float32(cnt)
    for r in 0 ..< cnt:
      let yy = rows[r]
      let a = (if ctx.alpha: src.get(x, yy, n - 1) else: 1'f32)
      for c in 0 ..< n:
        let s = src.get(x, yy, c)
        acc[c] += (if ctx.alpha and c < n - 1: s * a else: s) * k
    unpremulPx(acc.toOpenArray(0, n - 1), ctx.alpha)
    for c in 0 ..< n: result[i * n + c] = acc[c]

const
  NtscMax = (120'f32 - 7.5'f32) / 92.5'f32
  NtscMin = (-20'f32 - 7.5'f32) / 92.5'f32

proc ntsc*(src: Image; o: Rect; ctx: Ctx): seq[float32] =
  ## Scales chroma (in YIQ) so the composite signal Y ± C stays within broadcast limits.
  let n = src.ch
  let isNative = ctx.mode == cmRgb
  result = src.crop(o)
  if n == 0: return
  for i in 0 ..< result.len div n:
    let b = i * n
    let c = (if isNative: [result[b], result[b + 1], result[b + 2], 1'f32]
             else: rgba(ctx, result.toOpenArray(b, b + n - 1)))
    let yy = 0.299'f32 * c[0] + 0.587'f32 * c[1] + 0.114'f32 * c[2]
    let ii = 0.595_716'f32 * c[0] - 0.274_453'f32 * c[1] - 0.321_263'f32 * c[2]
    let qq = 0.211_456'f32 * c[0] - 0.522_591'f32 * c[1] + 0.311_135'f32 * c[2]
    let chroma = sqrt(ii * ii + qq * qq)
    if chroma <= 1e-7: continue
    var k = 1'f32
    if yy + chroma > NtscMax: k = min(k, max((NtscMax - yy) / chroma, 0'f32))
    if yy - chroma < NtscMin: k = min(k, max((yy - NtscMin) / chroma, 0'f32))
    if k >= 1: continue
    let (i2, q2) = (ii * k, qq * k)
    let rgb = [yy + 0.956_3'f32 * i2 + 0.621_0'f32 * q2, yy - 0.272_1'f32 * i2 - 0.647_4'f32 * q2,
               yy - 1.107_0'f32 * i2 + 1.704_6'f32 * q2]
    if isNative:
      for k2 in 0 .. 2: result[b + k2] = rgb[k2]
    else:
      setRgba(ctx, result.toOpenArray(b, b + n - 1), [rgb[0], rgb[1], rgb[2], c[3]])
