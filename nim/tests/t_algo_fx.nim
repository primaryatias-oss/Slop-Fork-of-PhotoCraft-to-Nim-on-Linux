## Other/Stylize/Distort/Pixelate/Render/Noise filters and the Filter Gallery through the
## dispatcher: registration, tile independence and adversarial parameters. Pixel parity with
## Rust is checked by `tools/filtercheck.py` (see PORTING.md).

import std/[unittest, options, json, math]
import photocraft/[geom, color, raster, serde, algo]

const FxKinds = [fltMinimum, fltMaximum, fltOffset, fltMosaic, fltEmboss, fltFindEdges, fltSolarize, fltInvert,
                 fltDesaturate, fltTwirl, fltPinch, fltSpherize, fltWave, fltRipple, fltPolarCoordinates,
                 fltDisplace, fltShear, fltZigZag, fltColorHalftone, fltCrystallize, fltFacet, fltFragment,
                 fltMezzotint, fltPointillize, fltDiffuse, fltExtrude, fltOilPaint, fltTiles, fltTraceContour,
                 fltWind, fltFibers, fltLensFlare, fltLightingEffects, fltRelight, fltReduceNoise, fltFilterGallery]

proc pattern(st: SampleType): Surface =
  result = newSurface(pixelFormat(cmRgb, st, true))
  for y in 0 ..< 40:
    for x in 0 ..< 56:
      let v = float32((x * 7 + y * 3) mod 23) / 22
      let px = (if (x div 9 + y div 7) mod 2 == 0: [v, 0.3'f32, 1 - v, 1] else: [0.9'f32, v * 0.5'f32, 0.2, 1])
      result.writePixel(x, y, px)

proc gallery(effects: varargs[GalleryEffect]): FilterParams =
  result = newFilterParams(fltFilterGallery)
  result.effects = @effects

proc worst(a, b: seq[float32]): float32 =
  for i in 0 ..< min(a.len, b.len): result = max(result, abs(a[i] - b[i]))

suite "fx filters":
  let b = rect(0, 0, 56, 40)

  test "every fx kind is registered":
    for k in FxKinds: check isAvailable(k)

  test "defaults run and stay finite":
    let s = pattern(stF32)
    for k in FxKinds:
      let p = newFilterParams(k)
      let o = applyTiled(s, p, b, b, none(Surface), 256, some(b)).readRegion(b)
      for v in o: check v.classify notin {fcNan, fcInf, fcNegInf}

  test "adversarial numbers never crash":
    let s = pattern(stU8)
    for k in FxKinds:
      let base = toJ(newFilterParams(k))
      for bad in [-1e6, -3.0, 0.0, 1e6]:
        var j = base.copy
        for key, v in j.mpairs:
          if v.kind in {JInt, JFloat} and key != "seed" and key != "generators": v = %bad
        var p: FilterParams
        try: p = fromJson(j, FilterParams)
        except CatchableError: continue
        if p.kind == fltFilterGallery: continue
        if p.halo != BoundsHalo and p.halo.radius > 4096: continue # Rust would read megapixels too
        discard applyTiled(s, p, b, b, none(Surface), 64, some(b))

suite "filter gallery":
  let b = rect(0, 0, 56, 40)

  test "every effect changes pixels and is tile independent":
    for st in [stU8, stU16, stF32]:
      let s = pattern(st)
      let before = s.readRegion(b)
      for f in GalleryFilter:
        let fp = gallery(newGalleryEffect(f))
        let a = applyTiled(s, fp, b, b, none(Surface), 256, some(b)).readRegion(b)
        check a != before
        for v in a: check v.classify notin {fcNan, fcInf, fcNegInf}
        if st == stF32:
          let t = applyTiled(s, fp, b, b, none(Surface), 16, some(b)).readRegion(b)
          check worst(a, t) < 1e-3

  test "rough pastels and angled strokes respond to every slider":
    let s = pattern(stF32)
    proc run(e: GalleryEffect; tile: int): seq[float32] =
      applyTiled(s, gallery(e), b, b, none(Surface), tile, some(b)).readRegion(b)
    for f in [gfRoughPastels, gfAngledStrokes]:
      let base = run(newGalleryEffect(f), 256)
      for prm in f.params:
        if prm.kind != gpRange: continue
        for v in [prm.min, prm.max]:
          var e = newGalleryEffect(f)
          e.set(prm.key, v)
          let a = run(e, 256)
          check a != base
          check worst(a, run(e, 16)) < 1e-3

  test "params parse and clamp":
    var n = 0
    for f in GalleryFilter:
      inc n
      check f.params.len > 0
      check galleryFromKey(f.key) == some(f)
      check galleryFromKey(f.commandId) == some(f)
    check n == 47
    var e = newGalleryEffect(gfTexturizer)
    check e.choice("texture") == "canvas"
    check e.choice("light") == "top"
    check e.setChoice("texture", "brick")
    check e.choice("texture") == "brick"
    e.set("relief", 500)
    check e.get("relief") == 50

  test "stacks and out-of-range values never crash":
    let s = pattern(stU8)
    var cmyk = newSurface(pixelFormat(cmCmyk, stU8, true))
    cmyk.fillRect(b, [0.2'f32, 0.4, 0.1, 0.3, 1])
    for js in ["""[{"filter":"tornEdges","values":{"contrast":0}},{"filter":"glass","values":{"distortion":40}}]""",
               """[{"filter":"patchwork","values":{"squareSize":-2}},{"filter":"stainedGlass","values":{"cellSize":-0.625}}]""",
               """[{"filter":"craquelure","values":{"crackSpacing":-5}},{"filter":"mosaicTiles","values":{"tileSize":0}}]""",
               """[{"filter":"dryBrush","values":{"brushSize":-1e30}},{"filter":"neonGlow","values":{"glowSize":-1e30}}]""",
               """[{"filter":"graphicPen","values":{"strokeLength":-7}},{"filter":"halftonePattern","values":{"size":-1.5}}]""",
               """[]"""]:
      var p = newFilterParams(fltFilterGallery)
      for e in parseJson(js):
        var g: GalleryEffect
        fromJ(e, g)
        p.effects.add g
      for src in [s, cmyk]:
        try: discard applyTiled(src, p, b, b, none(Surface), 64, some(b))
        except FilterTooLarge: discard
