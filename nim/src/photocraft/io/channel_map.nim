## Alpha/spot channel display info (PSD resources 1077 and 1007): colour, opacity and whether
## the colour marks masked or selected areas, or the channel is a spot ink.

import std/[math, options]
import ../color, ../doc

const
  DisplayInfo* = 1077'u16
  DisplayInfoOld* = 1007'u16
  QuickMaskInfo* = 1022'u16
  SpaceRgb = 0'u16
  SpaceCmyk = 2'u16
  SpaceLab = 7'u16
  SpaceGray = 8'u16

proc putU16(o: var seq[byte]; v: uint16) =
  o.add byte(v shr 8)
  o.add byte(v and 0xff)

proc displayInfo*(channels: openArray[AlphaChannel]): seq[byte] =
  result = @[0'u8, 0, 0, 1]
  for c in channels:
    var (color, opacity, kind) = (c.color, c.opacity, (if c.indicates == ciSelectedAreas: 0'u8 else: 1'u8))
    if c.spot.isSome: (color, opacity, kind) = (c.spot.get[0], c.spot.get[1], 2'u8)
    let rgb = color.toRgb
    result.putU16(SpaceRgb)
    for v in [rgb[0], rgb[1], rgb[2], 0'f32]: result.putU16(uint16(round(clamp(v, 0'f32, 1'f32) * 65535)))
    result.putU16(uint16(round(clamp(opacity, 0'f32, 1'f32) * 100)))
    result.add kind
    result.add 0'u8

proc decodeColor*(space: uint16; c: array[4, uint16]): Color =
  proc n(v: uint16): float32 = float32(v) / 65535
  case space
  of SpaceCmyk:
    let rgb = Color(mode: cmCmyk, c: [1 - n(c[0]), 1 - n(c[1]), 1 - n(c[2]), 1 - n(c[3])], alpha: 1).toRgb
    rgb(rgb[0], rgb[1], rgb[2])
  of SpaceLab:
    let l = float32(min(c[0], 10000'u16)) / 10000
    let a = float32(cast[int16](c[1])) / 100
    let b = float32(cast[int16](c[2])) / 100
    let rgb = Color(mode: cmLab, c: [l, (a + 128) / 255, (b + 128) / 255, 0], alpha: 1).toRgb
    rgb(rgb[0], rgb[1], rgb[2])
  of SpaceGray:
    let v = 1 - float32(min(c[0], 10000'u16)) / 10000
    rgb(v, v, v)
  else: rgb(n(c[0]), n(c[1]), n(c[2]))

proc applyDisplayInfo*(data: openArray[byte]; versioned: bool; channels: var seq[AlphaChannel]) =
  let start = (if versioned: 4 else: 0)
  var i = 0
  while start + (i + 1) * 14 <= data.len and i < channels.len:
    let o = start + i * 14
    proc u(data: openArray[byte]; k: int): uint16 = (uint16(data[k]) shl 8) or uint16(data[k + 1])
    let color = decodeColor(u(data, o), [u(data, o + 2), u(data, o + 4), u(data, o + 6), u(data, o + 8)])
    let opacity = float32(min(u(data, o + 10), 100'u16)) / 100
    case data[o + 12]
    of 2: channels[i].spot = some((color, opacity))
    else:
      channels[i].color = color
      channels[i].opacity = opacity
      channels[i].indicates = (if data[o + 12] == 0: ciSelectedAreas else: ciMaskedAreas)
    inc i
