## Multichannel PSD import: every merged plane becomes a spot channel (ink density is stored
## inverted). The export half lives in `psd_export` (`multichannelToPsd`), which owns the shared
## PSD writing helpers.

import std/options
import ../geom, ../color, ../raster, ../doc, ../psd
import channel_map

proc decodeSample*(plane: openArray[byte]; i: int; s: SampleType): float32 =
  case s
  of stU8: float32(plane[i]) / 255
  of stU16: float32(beU16(plane, 2 * i)) / 65535
  of stF32: cast[float32](beU32(plane, 4 * i))

proc importMultichannel*(file: PsdFile; all: openArray[byte]; d: var Document; names: openArray[string];
                         warnings: var seq[string]) =
  let h = file.header
  let depth = d.depth
  let n = h.width * h.height
  let plane = h.rowBytes * h.height
  let canvas = rect(0, 0, h.width, h.height)
  let fmt = pixelFormat(cmGrayscale, depth, false)
  for k in 0 ..< h.channels:
    if (k + 1) * plane > all.len:
      warnings.add "multichannel: channel " & $(k + 1) & " is missing"
      break
    var vals = newSeq[float32](n)
    for i in 0 ..< n: vals[i] = 1 - decodeSample(all.toOpenArray(k * plane, (k + 1) * plane - 1), i, depth)
    var s = newSurface(fmt)
    s.writeRegion(canvas, vals)
    s.prune()
    let name = if k < names.len and names[k].len > 0: names[k] else: "Channel " & $(k + 1)
    var ch = newAlphaChannel(name, s)
    ch.spot = some((Black, 0'f32))
    d.channels.add ch
  let r = file.resource(DisplayInfo)
  if r.isSome: applyDisplayInfo(r.get.data, true, d.channels)
  else:
    let old = file.resource(DisplayInfoOld)
    if old.isSome: applyDisplayInfo(old.get.data, false, d.channels)
