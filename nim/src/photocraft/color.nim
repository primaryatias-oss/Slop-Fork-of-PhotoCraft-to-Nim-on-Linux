## Pixel formats, colour models and blend-mode math.
##
## Bit depth and colour model are *runtime data*: nothing in the engine assumes 8-bit or RGB.
## Port of `crates/color`.

import std/[json, options]
import color/[blend, convert]
export blend, convert

type
  SampleType* = enum
    ## Storage type of one channel sample.
    stU8 = "U8", stU16 = "U16", stF32 = "F32"

  ColorMode* = enum
    ## Document colour model (Photoshop "Image → Mode").
    cmBitmap = "Bitmap", cmGrayscale = "Grayscale", cmIndexed = "Indexed", cmRgb = "Rgb",
    cmCmyk = "Cmyk", cmLab = "Lab", cmMultichannel = "Multichannel", cmDuotone = "Duotone"

  PixelFormat* = object
    ## Layout of a pixel: colour model, sample type, and whether an alpha channel follows.
    mode*: ColorMode
    sample*: SampleType
    alpha*: bool

  Color* = object
    ## A colour value in the document's model with alpha, as normalised floats.
    mode*: ColorMode
    c*: array[4, float32] ## up to 4 colour components (unused ones are 0)
    alpha*: float32

proc ditherNoise*(x, y: int): float32 =
  ## Per-pixel gradient dither noise in `[0, 1)` from a position hash.
  var h = (uint32(cast[uint32](int32(x))) * 0x9E37_79B1'u32) xor (uint32(cast[uint32](int32(y))) * 0x85EB_CA77'u32)
  h = h xor (h shr 15)
  h = h * 0x2C1B_3C6D'u32
  h = h xor (h shr 12)
  float32(h and 0xFFFF) / 65535.0'f32

proc bytes*(s: SampleType): int {.inline.} =
  case s
  of stU8: 1
  of stU16: 2
  of stF32: 4
proc bits*(s: SampleType): int = s.bytes * 8

proc colorChannels*(m: ColorMode): int =
  ## Number of colour (non-alpha) channels used for pixel storage.
  case m
  of cmBitmap, cmGrayscale, cmDuotone: 1
  of cmIndexed, cmRgb, cmLab: 3
  of cmCmyk: 4
  of cmMultichannel: 3

proc pixelFormat*(mode: ColorMode; sample: SampleType; alpha: bool): PixelFormat =
  PixelFormat(mode: mode, sample: sample, alpha: alpha)

const
  Rgba8* = PixelFormat(mode: cmRgb, sample: stU8, alpha: true)
  Rgba16* = PixelFormat(mode: cmRgb, sample: stU16, alpha: true)
  Rgba32f* = PixelFormat(mode: cmRgb, sample: stF32, alpha: true)
  Gray8* = PixelFormat(mode: cmGrayscale, sample: stU8, alpha: false)
  GrayA8* = PixelFormat(mode: cmGrayscale, sample: stU8, alpha: true)
  CmykA8* = PixelFormat(mode: cmCmyk, sample: stU8, alpha: true)

proc channels*(f: PixelFormat): int {.inline.} = f.mode.colorChannels + (if f.alpha: 1 else: 0)
proc bytesPerPixel*(f: PixelFormat): int {.inline.} = f.channels * f.sample.bytes
proc withSample*(f: PixelFormat; s: SampleType): PixelFormat =
  result = f
  result.sample = s

proc readSample*(bytes: openArray[byte]; sample: SampleType; index: int): float32 {.inline.} =
  ## Read one sample at `index` (sample index) as normalised f32 (F32 as stored).
  case sample
  of stU8: float32(bytes[index]) / 255.0'f32
  of stU16:
    let o = index * 2
    float32(cast[ptr uint16](unsafeAddr bytes[o])[]) / 65535.0'f32
  of stF32:
    let o = index * 4
    cast[ptr float32](unsafeAddr bytes[o])[]

proc writeSample*(bytes: var openArray[byte]; sample: SampleType; index: int; v: float32) {.inline.} =
  ## Write a normalised f32 sample; integer types are clamped and rounded.
  case sample
  of stU8: bytes[index] = uint8(clamp(v, 0.0'f32, 1.0'f32) * 255.0'f32 + 0.5'f32)
  of stU16:
    let o = index * 2
    cast[ptr uint16](addr bytes[o])[] = uint16(clamp(v, 0.0'f32, 1.0'f32) * 65535.0'f32 + 0.5'f32)
  of stF32:
    let o = index * 4
    cast[ptr float32](addr bytes[o])[] = v

proc rgb*(r, g, b: float32): Color = Color(mode: cmRgb, c: [r, g, b, 0.0'f32], alpha: 1.0)
proc rgba*(r, g, b, a: float32): Color = Color(mode: cmRgb, c: [r, g, b, 0.0'f32], alpha: a)
proc gray*(v: float32): Color = Color(mode: cmGrayscale, c: [v, 0.0'f32, 0, 0], alpha: 1.0)

const
  Black* = Color(mode: cmRgb, c: [0.0'f32, 0, 0, 0], alpha: 1.0)
  White* = Color(mode: cmRgb, c: [1.0'f32, 1, 1, 0], alpha: 1.0)
  Transparent* = Color(mode: cmRgb, c: [0.0'f32, 0, 0, 0], alpha: 0.0)

proc toRgb*(c: Color): array[3, float32] =
  ## Convert to sRGB (CMYK through the built-in CMYK profile, Lab via D50 formulas).
  case c.mode
  of cmRgb, cmIndexed, cmMultichannel: [c.c[0], c.c[1], c.c[2]]
  of cmGrayscale, cmBitmap, cmDuotone: [c.c[0], c.c[0], c.c[0]]
  of cmCmyk: cmykToRgb([c.c[0], c.c[1], c.c[2], c.c[3]])
  of cmLab: labToSrgb([c.c[0] * 100.0'f32, c.c[1] * 255.0'f32 - 128.0'f32, c.c[2] * 255.0'f32 - 128.0'f32])

proc inMode*(c: Color; mode: ColorMode): Color =
  ## This colour in `mode`'s model (through sRGB); unchanged when already in `mode`.
  proc rgbLike(m: ColorMode): bool = m in {cmRgb, cmIndexed, cmMultichannel}
  if c.mode == mode or (rgbLike(c.mode) and rgbLike(mode)): return c
  let rgb = c.toRgb
  result.alpha = c.alpha
  case mode
  of cmRgb, cmIndexed, cmMultichannel:
    result.mode = cmRgb
    result.c = [rgb[0], rgb[1], rgb[2], 0.0'f32]
  of cmGrayscale, cmBitmap, cmDuotone:
    result.mode = mode
    result.c = [rgbToGray(rgb), 0.0'f32, 0, 0]
  of cmCmyk:
    result.mode = mode
    result.c = rgbToCmyk(rgb)
  of cmLab:
    result.mode = mode
    let l = srgbToLab(rgb)
    result.c = [l[0] / 100.0'f32, (l[1] + 128.0'f32) / 255.0'f32, (l[2] + 128.0'f32) / 255.0'f32, 0.0'f32]

proc q8*(v: float32): uint8 {.inline.} = uint8(clamp(v, 0.0'f32, 1.0'f32) * 255.0'f32 + 0.5'f32)

proc toRgba8*(c: Color): array[4, uint8] =
  let r = c.toRgb
  [q8(r[0]), q8(r[1]), q8(r[2]), q8(c.alpha)]

# ---- JSON (serde-compatible shapes) ----

proc toJson*(c: Color): JsonNode =
  %*{"mode": $c.mode, "c": [c.c[0], c.c[1], c.c[2], c.c[3]], "alpha": c.alpha}

proc parseEnumStr*[T: enum](s: string; default: T): T =
  for v in T:
    if $v == s: return v
  default

proc colorFromJson*(n: JsonNode): Option[Color] =
  if n.isNil or n.kind != JObject: return none(Color)
  var c = Color(mode: parseEnumStr(n{"mode"}.getStr("Rgb"), cmRgb), alpha: float32(n{"alpha"}.getFloat(1.0)))
  let arr = n{"c"}
  if not arr.isNil and arr.kind == JArray:
    for i in 0 ..< min(arr.len, 4): c.c[i] = float32(arr[i].getFloat)
  some(c)

proc toJson*(f: PixelFormat): JsonNode = %*{"mode": $f.mode, "sample": $f.sample, "alpha": f.alpha}
proc pixelFormatFromJson*(n: JsonNode): PixelFormat =
  PixelFormat(mode: parseEnumStr(n{"mode"}.getStr("Rgb"), cmRgb), sample: parseEnumStr(n{"sample"}.getStr("U8"), stU8),
              alpha: n{"alpha"}.getBool(true))
