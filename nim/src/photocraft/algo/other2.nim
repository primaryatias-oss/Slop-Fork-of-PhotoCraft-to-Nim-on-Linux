## Other ▸ Custom (5×5 convolution), Other ▸ HSB/HSL, and the HSB/HSL conversions.

import std/math
import ../geom, ../color, ./params, ./fxutil

proc custom*(src: Image; o: Rect; ctx: Ctx; kernel: openArray[float32]; scale, offset: float32): seq[float32] =
  let n = src.ch
  let cc = ncol(ctx, n)
  var k: seq[(int, int, float32)]
  for i in 0 ..< min(25, kernel.len):
    if kernel[i] != 0: k.add (i mod 5 - 2, i div 5 - 2, kernel[i])
  let s = (if scale == 0: 1'f32 else: scale)
  let off = offset / 255
  result = src.crop(o)
  if k.len == 0 or n == 0: return
  for i in 0 ..< result.len div n:
    let (x, y) = xy(o, i)
    for c in 0 ..< cc:
      var sum = 0'f32
      for (dx, dy, w) in k: sum += src.get(x + dx, y + dy, c) * w
      result[i * n + c] = sum / s + off

proc hue(c: array[3, float32]; mx, d: float32): float32 =
  if d <= 0: return 0
  let h = (if mx == c[0]: floorMod((c[1] - c[2]) / d, 6'f32)
           elif mx == c[1]: (c[2] - c[0]) / d + 2
           else: (c[0] - c[1]) / d + 4)
  h / 6

proc rgbToHsb*(c: array[3, float32]): array[3, float32] =
  let (mx, mn) = (max(max(c[0], c[1]), c[2]), min(min(c[0], c[1]), c[2]))
  let d = mx - mn
  [hue(c, mx, d), (if mx > 0: d / mx else: 0'f32), mx]

proc rgbToHsl*(c: array[3, float32]): array[3, float32] =
  let (mx, mn) = (max(max(c[0], c[1]), c[2]), min(min(c[0], c[1]), c[2]))
  let d = mx - mn
  let l = (mx + mn) / 2
  let s = (if d <= 0: 0'f32 else: d / max(1 - abs(2 * l - 1), 1e-7'f32))
  [hue(c, mx, d), clamp(s, 0'f32, 1'f32), l]

proc fromHcm(h, chroma, m: float32): array[3, float32] =
  let hp = floorMod(h, 1'f32) * 6
  let x = chroma * (1 - abs(floorMod(hp, 2'f32) - 1))
  let (r, g, b) = (case int(hp)
    of 0: (chroma, x, 0'f32)
    of 1: (x, chroma, 0'f32)
    of 2: (0'f32, chroma, x)
    of 3: (0'f32, x, chroma)
    of 4: (x, 0'f32, chroma)
    else: (chroma, 0'f32, x))
  [r + m, g + m, b + m]

proc hsbToRgb*(c: array[3, float32]): array[3, float32] =
  let chroma = c[2] * c[1]
  fromHcm(c[0], chroma, c[2] - chroma)

proc hslToRgb*(c: array[3, float32]): array[3, float32] =
  let chroma = (1 - abs(2 * c[2] - 1)) * c[1]
  fromHcm(c[0], chroma, c[2] - chroma / 2)

proc hsbHsl*(src: Image; o: Rect; ctx: Ctx; input, output: HsbModel): seq[float32] =
  let n = src.ch
  result = src.crop(o)
  if input == output or n == 0: return
  let native = ctx.mode == cmRgb
  for i in 0 ..< result.len div n:
    let b = i * n
    let c4 = (if native: [result[b], result[b + 1], result[b + 2], 1'f32]
              else: rgba(ctx, result.toOpenArray(b, b + n - 1)))
    let c = [clamp(c4[0], 0'f32, 1'f32), clamp(c4[1], 0'f32, 1'f32), clamp(c4[2], 0'f32, 1'f32)]
    let rgb = (case input
      of hmRgb: c
      of hmHsb: hsbToRgb(c)
      of hmHsl: hslToRgb(c))
    let ov = (case output
      of hmRgb: rgb
      of hmHsb: rgbToHsb(rgb)
      of hmHsl: rgbToHsl(rgb))
    if native:
      for k in 0 .. 2: result[b + k] = ov[k]
    else:
      setRgba(ctx, result.toOpenArray(b, b + n - 1), [ov[0], ov[1], ov[2], c4[3]])
