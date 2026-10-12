## Straight-alpha RGBA float buffers covering a rectangle (the compositor's working type).

import ../geom, ../raster

type
  Px* = array[4, float32]
  Buffer* = object
    rect*: Rect
    px*: seq[Px]

proc transparentBuffer*(r: Rect): Buffer = Buffer(rect: r, px: newSeq[Px](max(r.width * r.height, 0)))

proc filledBuffer*(r: Rect; c: Px): Buffer =
  result = transparentBuffer(r)
  for p in result.px.mitems: p = c

proc get*(b: Buffer; x, y: int): Px {.inline.} = b.px[(y - b.rect.y0) * b.rect.width + (x - b.rect.x0)]

proc toRgba8*(b: Buffer): Rgba8Image =
  result = Rgba8Image(width: b.rect.width, height: b.rect.height, pixels: newSeq[uint8](b.px.len * 4))
  for i, p in b.px:
    for k in 0..3: result.pixels[i * 4 + k] = uint8(clamp(p[k], 0'f32, 1'f32) * 255 + 0.5)

proc overBackground*(b: Buffer; bg: array[3, float32]): Buffer =
  ## Flattens over an opaque background colour.
  result = b
  for p in result.px.mitems:
    let a = p[3]
    for i in 0..2: p[i] = p[i] * a + bg[i] * (1 - a)
    p[3] = 1

proc surfaceToBuffer*(s: Surface; r: Rect): Buffer =
  result = transparentBuffer(r)
  if result.px.len > 0: s.readRgbaInto(r, result.px)
