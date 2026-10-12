## Tests for photocraft/color and photocraft/raster (ported from crates/color and crates/raster).
import std/[unittest, math, tables]
import photocraft/[geom, color, raster]

suite "color":
  test "colors convert into a document mode":
    let blue = rgba(0.0, 0.0, 1.0, 0.5)
    let g = blue.inMode(cmGrayscale)
    check g.mode == cmGrayscale and g.alpha == 0.5
    check abs(g.c[0] - 0.114) < 1e-6
    check blue.inMode(cmIndexed) == blue
    check g.inMode(cmGrayscale) == g
    for mode in [cmCmyk, cmLab]:
      let c = blue.inMode(mode)
      check c.mode == mode
      let back = c.inMode(cmRgb)
      check back.mode == cmRgb
      check back.toRgb == c.toRgb

  test "pixel format sizes":
    check Rgba8.bytesPerPixel == 4
    check Rgba16.bytesPerPixel == 8
    check Rgba32f.bytesPerPixel == 16
    check Gray8.bytesPerPixel == 1
    check CmykA8.channels == 5

  test "sample roundtrip all depths":
    for s in SampleType:
      var buf = newSeq[byte](16)
      for i, v in [0.0'f32, 0.25, 0.5, 1.0]:
        writeSample(buf, s, i, v)
        let r = readSample(buf, s, i)
        let tol = case s
          of stU8: 1.0 / 255.0
          of stU16: 1.0 / 65535.0
          of stF32: 0.0
        check abs(r - v) <= tol

  test "integer samples clamp and floats keep hdr":
    var b = newSeq[byte](2)
    writeSample(b, stU8, 0, 1.7)
    writeSample(b, stU8, 1, -0.3)
    check b == @[255'u8, 0]
    var f = newSeq[byte](4)
    writeSample(f, stF32, 0, 4.5)
    check readSample(f, stF32, 0) == 4.5

  test "color to rgba8":
    check White.toRgba8 == [255'u8, 255, 255, 255]
    check gray(0.5).toRgba8 == [128'u8, 128, 128, 255]
    check Color(mode: cmCmyk, alpha: 1.0).toRgba8 == [255'u8, 255, 255, 255]
    let k = Color(mode: cmCmyk, c: [0.75'f32, 0.68, 0.67, 0.9], alpha: 1.0).toRgba8
    check k[0] < 20 and k[1] < 20 and k[2] < 20

  test "blend modes":
    check blendChannel(bmMultiply, 0.5, 0.5) == 0.25
    check blendChannel(bmScreen, 0.5, 0.5) == 0.75
    check blendFromPsdKey("mul ").get == bmMultiply
    let c = composite(bmNormal, [1.0'f32, 0, 0, 1], [0.0'f32, 0, 1, 1], 0.5)
    check abs(c[0] - 0.5) < 1e-6 and abs(c[2] - 0.5) < 1e-6 and c[3] == 1.0

suite "raster":
  test "fill rect shares covered tiles and edits stay local":
    var s = newSurface(Rgba8)
    s.fillRect(rect(0, 0, 300_000, 300_000), [1.0'f32, 0.5, 0.0, 1.0])
    check s.tile(tileCoord(0, 0)) == s.tile(tileCoord(1000, 900))
    let edge = 300_000 div TileSize
    check s.tile(tileCoord(0, 0)) != s.tile(tileCoord(edge, 0))
    check s.pixel(299_999, 299_999) == @[1.0'f32, 128.0 / 255.0, 0.0, 1.0]
    check s.pixel(300_000, 0) == @[0.0'f32, 0, 0, 0]
    s.writePixel(5, 5, [0.0'f32, 0.0, 1.0, 1.0])
    check s.pixel(5, 5) == @[0.0'f32, 0.0, 1.0, 1.0]
    check s.pixel(300, 5) == @[1.0'f32, 128.0 / 255.0, 0.0, 1.0]
    check s.tile(tileCoord(0, 0)) != s.tile(tileCoord(1, 0))

  test "empty surface reads default":
    let s = newSurface(Rgba8)
    check s.pixel(10, -500) == @[0.0'f32, 0, 0, 0]
    let m = surfaceWithDefault(Gray8, [1.0'f32])
    check m.pixel(123_456, -9) == @[1.0'f32]
    check m.tileCount == 0

  test "write read pixel every depth":
    for sample in SampleType:
      var s = newSurface(Rgba8.withSample(sample))
      s.writePixel(-3, 700, [0.2'f32, 0.4, 0.6, 1.0])
      let p = s.pixel(-3, 700)
      for i, b in [0.2'f32, 0.4, 0.6, 1.0]: check abs(p[i] - b) <= 1.0 / 255.0
      check s.tileCount == 1
      check s.tileBounds == tileContaining(-3, 700).rect

  test "copy on write shares untouched tiles":
    var a = newSurface(Rgba8)
    a.fillRect(rect(0, 0, 512, 512), [1.0'f32, 0.0, 0.0, 1.0])
    # make the four tiles distinct (fillRect shares one tile for full coverage)
    for (x, y) in [(0, 0), (300, 0), (0, 300), (300, 300)]: a.writePixel(x + 1, y + 1, [1.0'f32, 0.0, 0.0, 1.0])
    var b = a
    b.writePixel(10, 10, [0.0'f32, 1.0, 0.0, 1.0])
    check a.pixel(10, 10) == @[1.0'f32, 0.0, 0.0, 1.0]
    check b.pixel(10, 10) == @[0.0'f32, 1.0, 0.0, 1.0]
    var shared = 0
    for c, t in a.tiles:
      if t == b.tile(c): inc shared
    check shared == 3

  test "content bounds and prune":
    var s = newSurface(Rgba8)
    s.fillRect(rect(300, 10, 310, 20), [1.0'f32, 1.0, 1.0, 1.0])
    s.writePixel(-5, -5, [0.0'f32, 0.0, 0.0, 1.0])
    check s.contentBounds == rect(-5, -5, 310, 20)
    s.writePixel(-5, -5, [0.0'f32, 0.0, 0.0, 0.0])
    s.prune()
    check s.tileCount == 1
    check s.contentBounds == rect(300, 10, 310, 20)

  test "interleaved roundtrip crossing tiles":
    let r = rect(-20, 250, 280, 262)
    let bpp = Rgba16.bytesPerPixel
    var bytes = newSeq[byte](r.width * r.height * bpp)
    for i in 0 ..< bytes.len: bytes[i] = byte(i * 31 mod 251)
    let s = surfaceFromInterleaved(Rgba16, r, bytes)
    check s.toInterleaved(r) == bytes
    check s.tileCount == 6

  test "convert depths and models":
    var s = newSurface(Rgba8)
    s.writePixel(1, 1, [1.0'f32, 1.0, 1.0, 1.0])
    check abs(s.convert(GrayA8).pixel(1, 1)[0] - 1.0) < 1e-2
    check s.convert(Rgba32f).pixel(1, 1) == @[1.0'f32, 1.0, 1.0, 1.0]
    let c = s.convert(CmykA8)
    let p = c.pixel(1, 1)
    check p[3] < 0.01 and abs(p[4] - 1.0) < 1e-6
    check c.convert(Rgba8).pixel(1, 1) == @[1.0'f32, 1.0, 1.0, 1.0]

  test "translated matches reading the moved pixels":
    for fmt in [Rgba8, Gray8, Rgba16]:
      var s = newSurface(fmt)
      let r = rect(-30, 200, 400, 300)
      var data = newSeq[float32](r.width * r.height * fmt.channels)
      for i in 0 ..< data.len: data[i] = float32(i mod 97) / 96.0
      s.writeRegion(r, data)
      for (dx, dy) in [(5, -7), (256, 512), (-300, 1)]:
        let t = s.translated(dx, dy, s.contentBounds)
        let moved = r.translate(dx, dy)
        check t.readRegion(moved) == s.readRegion(r)

  test "strided row sampling matches sample channel":
    var s = surfaceWithDefault(Gray8, [0.25'f32])
    s.fillRect(rect(-300, 10, 200, 40), [1.0'f32])
    s.fillRect(rect(250, 10, 700, 20), [0.5'f32])
    for (y, x0, step) in [(15, -520, 7), (15, -1, 1), (30, 3, 64), (500, 0, 9)]:
      var o = newSeq[float32](120)
      s.sampleRowStrided(y, x0, step, 0, o)
      for i, v in o: check v == s.sampleChannel(x0 + i * step, y, 0)

  test "content bounds with tile skipping":
    var s = newSurface(Gray8)
    s.fillRect(rect(0, 0, 1024, 1024), [0.0'f32])
    s.fillRect(rect(300, 310, 700, 720), [1.0'f32])
    check s.contentBounds == rect(300, 310, 700, 720)
    var s2 = newSurface(Gray8)
    s2.fillRect(rect(0, 0, 2000, 1500), [1.0'f32])
    s2.fillRect(rect(-700, 4000, -699, 4001), [0.5'f32])
    check s2.contentBounds == rect(-700, 0, 2000, 4001)

  test "rgba readers agree":
    for fmt in [Rgba8, GrayA8, CmykA8, Rgba16]:
      var s = newSurface(fmt)
      let r = rect(250, 250, 270, 262)
      var data = newSeq[float32](r.width * r.height * fmt.channels)
      for i in 0 ..< data.len: data[i] = float32(i mod 13) / 12.0
      s.writeRegion(r, data)
      var o = newSeq[Rgba](r.width * r.height)
      s.readRgbaInto(r, o)
      for i in 0 ..< o.len:
        let x = r.x0 + i mod r.width
        let y = r.y0 + i div r.width
        let want = s.rgba(x, y)
        for c in 0..3: check abs(o[i][c] - want[c]) < 1e-6

  test "interrupts":
    var n = 0
    let i = newInterrupt(proc(): bool = n > 2)
    check not i.cancelled
    n = 5
    check i.cancelled
    expect Cancelled: i.check()
    check not NoInterrupt.cancelled
