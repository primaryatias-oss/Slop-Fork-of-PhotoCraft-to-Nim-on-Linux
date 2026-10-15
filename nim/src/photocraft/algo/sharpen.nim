## Unsharp Mask, Smart Sharpen (basic), High Pass (port of `sharpen.rs`).

import std/math
import ../geom
import params, image, blur

template withBlur(src: Image; o: Rect; ctx: Ctx; radius: float32; f: untyped): seq[float32] =
  ## `f(orig, blurred)` per colour sample of the visible pixels.
  block:
    let k = gaussianKernel(radius)
    let blurred = convSep(src, o, k, k, ctx.alpha)
    var res = src.crop(o)
    let n = src.ch
    let cc = if ctx.alpha: n - 1 else: n
    var i = 0
    while i + n <= res.len:
      if not (ctx.alpha and res[i + n - 1] <= 0):
        for c in 0 ..< cc:
          let orig {.inject.} = res[i + c]
          let bl {.inject.} = blurred[i + c]
          res[i + c] = f
      i += n
    res

proc unsharp*(src: Image; o: Rect; ctx: Ctx; amount, radius, threshold: float32): seq[float32] =
  let k = amount / 100
  let t = threshold / 255
  withBlur(src, o, ctx, radius):
    (let d = orig - bl; if abs(d) + 1e-6'f32 >= t: orig + k * d else: orig)

proc smart*(src: Image; o: Rect; ctx: Ctx; amount, radius, reduceNoise: float32): seq[float32] =
  let k = amount / 100
  let nr = clamp(reduceNoise / 100, 0'f32, 1'f32)
  withBlur(src, o, ctx, radius):
    # Suppress small (noise-level) differences.
    (let d = orig - bl; orig + k * d * (1 - nr * exp(-(abs(d) * 40))))

proc highPass*(src: Image; o: Rect; ctx: Ctx; radius: float32): seq[float32] =
  withBlur(src, o, ctx, radius):
    0.5'f32 + (orig - bl)
