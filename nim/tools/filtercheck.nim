## Filter cross-check against the Rust crates (see PORTING.md): mirrors `tools/oracle filter`.
##
##   filtercheck gen <dir>                                  writes the test documents
##   filtercheck run <doc.pcraft> <params.json> <out.pcraft> <layer index>
##
## Compare the outputs of both with `filtercheck same <a.pcraft> <b.pcraft>` (layer pixels, exact
## or with `--tol <levels>`).

import std/[os, json, options, strutils]
import photocraft/[geom, color, raster, doc, format, serde, algo]

proc blobs(fmt: PixelFormat; seed: int; w, h: int): Layer =
  ## Soft discs, hard edges, a gradient and transparent gaps: content most filters react to.
  result = rasterLayer("blobs", fmt)
  var s = uint32(seed)
  proc rnd(): uint32 =
    s = s * 1664525 + 1013904223
    s shr 8
  for y in 0 ..< h:
    for x in 0 ..< w:
      if (x div 16 + y div 16) mod 5 == 0: continue
      let g = float32(x) / float32(w)
      result.content.raster.writePixel(x, y, [g, float32(y) / float32(h), 1 - g, 1'f32])
  for k in 0 ..< 10:
    let (cx, cy, r) = (int(rnd() mod uint32(w)), int(rnd() mod uint32(h)), 6 + int(rnd() mod 24))
    let c = [float32(rnd() and 255) / 255, float32(rnd() and 255) / 255, float32(rnd() and 255) / 255,
             (if k mod 3 == 0: 0.6'f32 else: 1'f32)]
    for y in max(cy - r, 0) ..< min(cy + r, h):
      for x in max(cx - r, 0) ..< min(cx + r, w):
        if (x - cx) * (x - cx) + (y - cy) * (y - cy) < r * r: result.content.raster.writePixel(x, y, c)

proc gen(dir: string) =
  createDir(dir)
  for (name, mode, sample, alpha) in [("rgb8", cmRgb, stU8, true), ("rgb16", cmRgb, stU16, true),
                                      ("rgb32", cmRgb, stF32, true), ("gray8", cmGrayscale, stU8, true),
                                      ("cmyk8", cmCmyk, stU8, true), ("lab16", cmLab, stU16, true),
                                      ("rgb8flat", cmRgb, stU8, false)]:
    var d = newDocument(name, Size(width: 160, height: 120), mode, sample)
    var l = blobs(pixelFormat(mode, sample, alpha), 7, 160, 120)
    d.layers.add l
    writeFile(dir / name & ".pcraft", cast[string](saveToBytes(d)))

proc run(docPath, params, outPath: string; idx: int) =
  var d = loadFromBytes(cast[seq[byte]](readFile(docPath)))
  let p = fromJson(parseJson(params), FilterParams)
  let b = d.bounds
  let s = d.layers[idx].content.raster
  let area = outputArea(p, s.contentBounds, b)
  d.layers[idx].content.raster = apply(s, p, area, b)
  writeFile(outPath, cast[string](saveToBytes(d)))

proc same(a, b: string; tol: float32): bool =
  let x = loadFromBytes(cast[seq[byte]](readFile(a)))
  let y = loadFromBytes(cast[seq[byte]](readFile(b)))
  result = true
  for i in 0 ..< min(x.layers.len, y.layers.len):
    if x.layers[i].content.kind != lkRaster: continue
    let (s, t) = (x.layers[i].content.raster, y.layers[i].content.raster)
    let r = s.contentBounds.union(t.contentBounds)
    let (u, v) = (s.readRegion(r), t.readRegion(r))
    let scale = case s.format.sample
      of stU8: 255'f32
      of stU16: 65535'f32
      else: 1'f32
    var n, worst = 0
    var maxd = 0'f32
    for k in 0 ..< u.len:
      let dd = abs(u[k] - v[k]) * scale
      if dd > tol:
        inc n
        if dd > maxd:
          maxd = dd
          worst = k
    if n > 0:
      result = false
      echo "layer ", i, ": ", n, " of ", u.len, " samples differ, max ", maxd, " at sample ", worst, " (rect ", r, ")"

when isMainModule:
  case paramStr(1)
  of "gen": gen(paramStr(2))
  of "run": run(paramStr(2), paramStr(3), paramStr(4), parseInt(paramStr(5)))
  of "same":
    let tol = if paramCount() >= 5 and paramStr(4) == "--tol": parseFloat(paramStr(5)).float32 else: 0'f32
    if same(paramStr(2), paramStr(3), tol): echo "SAME" else: quit 1
  else: quit "usage: filtercheck gen|run|same …"
