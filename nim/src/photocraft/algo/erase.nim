## Magic Eraser: erase a wand `Region` to transparency (or, on a layer with locked transparency, to
## a colour), tile by tile (port of `erase.rs`).
##
## Works in the surface's own channels at any depth and in any colour model: only the alpha
## channel changes (or, under a transparency lock, the colour channels move toward `lockColor`).

import std/[options, math]
import ../geom, ../color, ../raster
import ./selection

proc magicErase*(target: var Surface; region: Region; amount0: float32; selection: Option[Surface];
                 lockColor: Option[seq[float32]]): Rect =
  ## Erases `region` from `target` at `amount` (0–1, the tool's opacity), weighted by the
  ## selection's coverage. With `lockColor` (native colour channels) the pixels are painted toward
  ## that colour instead and alpha is kept, as Photoshop does on layers with locked transparency.
  ## Returns the damaged rectangle.
  let amount = (if amount0.classify notin {fcNan, fcInf, fcNegInf}: clamp(amount0, 0'f32, 1'f32) else: 0'f32)
  let bbox = region.bbox
  let bw = bbox.width
  if bbox.isEmpty or amount <= 0 or region.mask.len != bw * bbox.height: return EmptyRect
  let fmt = target.format
  let nc = fmt.mode.colorChannels
  let alpha = (if fmt.alpha: some(nc) else: none(int))
  # Without alpha and without a lock colour there is nothing an eraser can do.
  if alpha.isNone and lockColor.isNone: return EmptyRect
  var lock = none(seq[float32])
  if lockColor.isSome:
    var l = newSeq[float32](nc)
    for i in 0 ..< nc: l[i] = (if i < lockColor.get.len: lockColor.get[i] else: 0'f32)
    lock = some(l)
  let (sample, bpp) = (fmt.sample, fmt.bytesPerPixel)
  proc cov(x, y: int): uint8 =
    let i = (y - bbox.y0) * bw + (x - bbox.x0)
    if i >= 0 and i < region.mask.len: region.mask[i] else: 0'u8
  var done: seq[(Rect, seq[byte])]
  for tc in bbox.tiles:
    let r = tc.rect.intersect(bbox)
    if r.isEmpty: continue
    let w = r.width
    # Skip tiles the region doesn't touch.
    var touched = false
    for y in r.y0 ..< r.y1:
      for x in r.x0 ..< r.x1:
        if cov(x, y) != 0:
          touched = true
          break
      if touched: break
    if not touched: continue
    var bytes = target.toInterleaved(r)
    var sel: seq[float32]
    var sc = 1
    if selection.isSome:
      sc = selection.get.channels
      sel = selection.get.readRegion(r)
    for y in r.y0 ..< r.y1:
      for x in r.x0 ..< r.x1:
        let c = cov(x, y)
        if c == 0: continue
        let i = (y - r.y0) * w + (x - r.x0)
        let s = (if selection.isNone: 1'f32 elif i * sc < sel.len: sel[i * sc] else: 0'f32)
        let k = float32(c) / 255'f32 * amount * s
        if k <= 0: continue
        if (i + 1) * bpp > bytes.len: continue
        template px: untyped = bytes.toOpenArray(i * bpp, (i + 1) * bpp - 1)
        if lock.isSome:
          for ch, t in lock.get:
            let v = readSample(px, sample, ch)
            writeSample(px, sample, ch, v + (t - v) * k)
        elif alpha.isSome:
          let v = readSample(px, sample, alpha.get) * (1'f32 - k)
          if k >= 1 or v <= 0:
            # Fully erased pixels become the empty pixel, so tiles can be pruned.
            for b in i * bpp ..< (i + 1) * bpp: bytes[b] = 0
          else:
            writeSample(px, sample, alpha.get, v)
    done.add (r, bytes)
  result = EmptyRect
  for (r, bytes) in done:
    target.writeInterleaved(r, bytes)
    result = result.union(r)
  if lock.isNone: target.prune()
