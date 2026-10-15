## Filter › Render: Flame, Picture Frame and Tree (ports of the Rust unit tests). Bit parity with
## Rust is checked by `tools/algocheck.py` (`tools/algocases/render2.json`, see PORTING.md).

import std/[unittest, options, json]
import photocraft/[geom, raster, color]
import photocraft/algo/render2

suite "render: flame, picture frame, tree":
  test "composite draws a disc and respects the clip":
    var s = newSurface(pixelFormat(cmRgb, stU8, true))
    let used = composite(s, [disc((20'f32, 20'f32), 8, [1'f32, 0, 0, 1])], rect(0, 0, 20, 40), none(Surface))
    check used.x1 == 20
    let c = s.pixel(18, 20)
    check c[0] > 0.99 and c[3] > 0.99
    check s.pixel(22, 20)[3] == 0 and s.pixel(2, 2)[3] == 0
  test "renderers are deterministic and seeded":
    let canvas = rect(0, 0, 400, 300)
    let a = tree(defaultTreeSpec(), canvas)
    check a == tree(defaultTreeSpec(), canvas)
    var seeded = defaultTreeSpec()
    seeded.seed = 7
    check a != tree(seeded, canvas)
    for k in 1'u32 .. 34'u32:
      var s = defaultTreeSpec()
      s.baseTreeType = k
      check tree(s, canvas).len > 0
    for k in 1'u32 .. uint32(FrameStyles.len):
      var s = defaultFrameSpec()
      s.frame = k
      check pictureFrame(s, canvas).len > 0
    let path = defaultFlamePath(canvas)
    for t in FlameType:
      var s = defaultFlameSpec()
      s.flameType = t
      check flame(s, [path]).len > 0
  test "options parse from JSON with defaults and reject bad values":
    let f = flameSpecFromJson(parseJson("""{"flameType": "candleLight", "quality": 4, "color": [1, 0.5, 0, 1], "extra": 3}"""))
    check f.flameType == ftCandleLight and f.quality == 4 and f.color == some([1'f32, 0.5, 0, 1]) and f.length == 150
    check treeSpecFromJson(parseJson("{}")) == defaultTreeSpec()
    check frameSpecFromJson(parseJson("""{"frame": 3}""")).frame == 3
    expect ValueError: discard flameSpecFromJson(parseJson("""{"flameType": "inferno"}"""))
    expect ValueError: discard treeSpecFromJson(parseJson("""{"seed": 1.5}"""))
    expect ValueError: discard frameSpecFromJson(parseJson("""{"vineColor": [1, 2]}"""))
  test "hostile options and paths draw nothing harmful":
    var s = newSurface(pixelFormat(cmRgb, stU8, true))
    var spec = defaultFlameSpec()
    spec.length = NaN
    spec.width = Inf
    for t in FlameType:
      spec.flameType = t
      discard flame(spec, [@[(float32(NaN), 1'f32), (2'f32, float32(Inf))], @[], @[(1e30'f32, -1e30'f32)]])
    check composite(s, [disc((float32(NaN), 0'f32), 5, [1'f32, 0, 0, 1]), capsule((0'f32, 0'f32), (float32(Inf), 0'f32), 1, 1,
                    [1'f32, 0, 0, 1], [1'f32, 0, 0, 1])], rect(0, 0, 50, 50), none(Surface)).isEmpty
