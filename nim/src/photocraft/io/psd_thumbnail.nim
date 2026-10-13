## The PSD image preview (resource 1036): a JPEG thumbnail of the composite in sRGB, at most
## 160 px on a side (Rust `psd_thumbnail.rs`).

import std/[options, math]
import ../doc, ../color, ../compose, ../cms, ../codecs, ../psd
import common

const
  MaxSide* = 160
  HeaderLen* = 28

proc finite(v: float32): float32 = (if v != v: 0'f32 else: clamp(v, 0'f32, 1'f32))

proc compositeProfile(d: Document): Profile =
  let space = case d.mode
    of cmGrayscale, cmBitmap, cmDuotone: csGray
    of cmCmyk, cmLab: return bSrgb.profile
    else: csRgb
  var own = none(Profile)
  if not d.iccProfile.isNil:
    try:
      let p = parseProfile(d.iccProfile.data)
      if p.colorSpace.kind == space: own = some(p)
    except CatchableError: discard
  if space == csGray:
    if own.isSome:
      let g = own.get.grayAsRgb
      if g.isSome: return g.get
    let g = bSGray.profile.grayAsRgb
    if g.isSome: return g.get
    return bSrgb.profile
  if own.isSome: own.get else: bSrgb.profile

proc putU32(o: var seq[byte]; v: uint32) =
  for s in [24, 16, 8, 0]: o.add byte((v shr s) and 0xff)

proc thumbnailBytes(w, h: int; jfif: seq[byte]): seq[byte] =
  if w <= 0 or h <= 0 or w > 100_000_000 or h > 100_000_000 or jfif.len > int(high(uint32)):
    raise ioError(ieUnsupported, "image preview too large")
  let widthBytes = (w * 24 + 31) div 32 * 4
  let total = widthBytes * h
  if total > int(high(uint32)): raise ioError(ieUnsupported, "image preview too large")
  for v in [1, w, h, widthBytes, total, jfif.len]: result.putU32(uint32(v))
  result.add [0'u8, 24, 0, 1]
  result.add jfif

proc thumbnailResource*(d: Document): ImageResource =
  var buf = thumbnailBuffer(d, MaxSide)
  let (w, h) = (buf.rect.width, buf.rect.height)
  if w == 0 or h == 0 or w > MaxSide or h > MaxSide or buf.px.len != w * h:
    raise ioError(ieUnsupported, "the image preview could not be rendered")
  let src = compositeProfile(d)
  let dst = bSrgb.profile
  if not sameColors(src, dst):
    var t: Transform
    try: t = newTransform(src, dst, RelativeColorimetric, true)
    except CatchableError as e: raise ioError(ieUnsupported, e.msg)
    var flat = newSeq[float32](buf.px.len * 4)
    for i, p in buf.px:
      for c in 0 ..< 4: flat[i * 4 + c] = p[c]
    t.apply(flat, 4)
    for i in 0 ..< buf.px.len:
      for c in 0 ..< 4: buf.px[i][c] = flat[i * 4 + c]
  var rgb = newSeqOfCap[byte](buf.px.len * 3)
  for p in buf.px:
    let a = finite(p[3])
    for c in 0 ..< 3:
      let v = finite(p[c]) * a + (1 - a)
      rgb.add uint8(round(v * 255))
  var opts = defaultEncodeOptions()
  opts.jpegQuality = 80
  opts.embedIcc = false
  opts.embedMetadata = false
  var jfif: seq[byte]
  try: jfif = encode(imageFromRaw(w, h, clRgb, isU8, rgb), fmtJpeg, opts)
  except CodecError as e: raise ioError(ieCodec, e.msg)
  newResource(ridThumbnail, thumbnailBytes(w, h, jfif))
