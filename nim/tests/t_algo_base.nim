## Algo foundation tests: filter params JSON, Image sampling, median, noise, box/gauss blurs.
import std/[unittest, json, math, options]
import photocraft/[geom, raster, color, serde]
import photocraft/algo/[params, image, noise, fxutil, other2, util]

suite "algo base":
  test "filter params round-trip through serde names":
    var p = newFilterParams(fltRadialBlur)
    p.amount = 10
    p.centerX = 0.5
    let j = toJ(p)
    check j["filter"].getStr == "radialBlur"
    check j["quality"].getStr == "good"
    check j["method"].getStr == "spin"
    check not j.hasKey("radius")
    var q: FilterParams
    fromJ(j, q)
    check q == p
    var s: FilterParams
    fromJ(parseJson("""{"filter":"smartBlur","radius":3,"threshold":25,"quality":"high","mode":"edgeOnly"}"""), s)
    check s.blurQuality == bqHigh and s.smartBlurMode == sbEdgeOnly and s.radius == 3
    var g = newFilterParams(fltFilterGallery)
    var e = newGalleryEffect(gfCutout)
    e.set("numberOfLevels", 20)
    g.effects = @[e]
    check e.get("numberOfLevels") == 8
    check g.label == "Cutout"
    var g2: FilterParams
    fromJ(toJ(g), g2)
    check g2 == g
    check gfPaintDaubs.params[2].choices.len == 6
    check galleryFromKey("filter.gallery.sumiE") == some(gfSumiE)

  test "median, dust & scratches and noise":
    var img = newImage(rect(0, 0, 5, 5), 1)
    for i in 0 ..< 25: img.data[i] = float32(i mod 7) / 255
    img.data[12] = 1
    let m = median(img, rect(2, 2, 3, 3), 1)
    check m[0] < 0.1
    let m2 = median(img, rect(1, 1, 4, 4), 2)
    check m2.len == 9
    let ctx = Ctx(bounds: rect(0, 0, 5, 5), mode: cmGrayscale, alpha: false)
    let a = addNoise(img, rect(0, 0, 5, 5), ctx, 50, diUniform, true, 7)
    let b = addNoise(img, rect(0, 0, 5, 5), ctx, 50, diUniform, true, 7)
    check a == b and a != img.data

  test "blurs and HSB round trip":
    var buf = newSeq[float32](16 * 16)
    buf[8 * 16 + 8] = 1
    gaussBlurN(buf, 16, 16, 1, 2)
    var sum = 0'f32
    for v in buf: sum += v
    check abs(sum - 1) < 1e-3
    check buf[8 * 16 + 8] < 0.2
    let c = [0.2'f32, 0.5, 0.7]
    let back = hsbToRgb(rgbToHsb(c))
    for i in 0 .. 2: check abs(back[i] - c[i]) < 1e-5
    let back2 = hslToRgb(rgbToHsl(c))
    for i in 0 .. 2: check abs(back2[i] - c[i]) < 1e-5
    check solveDense(2, @[2.0, 0, 0, 4], @[2.0, 8]) == @[1.0, 2.0]
