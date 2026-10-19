## Camera raw import (`io/tests/raw.rs`): synthetic DNG / CR2 develop into a 16-bit ProPhoto
## document, uncalibrated raws take their colour from the camera JPEG, the open-time tuning is
## applied after the fit, and unsupported raw variants fall back to the embedded preview.

import std/[unittest, options, strutils, sequtils, math]
import photocraft/[doc, io, codecs, cms]
import photocraft/raw as praw
import photocraft/raw/testgen
import photocraft/algo/camera_raw

proc importOk(name: string; b: seq[byte]): ImportResult = importDoc(name, b)

proc patches(w, h: int): seq[array[3, float32]] =
  ## Camera-space patches, each with a tonal ramp.
  var seed = 99'u32
  proc rnd(): float32 =
    seed = seed * 1_664_525'u32 + 1_013_904_223'u32
    float32(seed shr 8) / float32(1'u32 shl 24)
  var colours: seq[array[3, float32]]
  for _ in 0 ..< 96:
    let a = 0.12'f32 + 0.5'f32 * rnd()
    let b = 0.12'f32 + 0.5'f32 * rnd()
    let c = 0.12'f32 + 0.5'f32 * rnd()
    colours.add [a, b, c]
  for i in 0 ..< w * h:
    let (x, y) = (i mod w, i div w)
    let ramp = 0.15'f32 + 0.85'f32 * float32((x * 12) mod w) / float32(w)
    let c = colours[(y * 8 div h) * 12 + x * 12 div w]
    result.add [c[0] * ramp, c[1] * ramp, c[2] * ramp]

proc jpeg(img: Image): seq[byte] =
  var o = defaultEncodeOptions()
  o.jpegQuality = 95
  encode(img, fmtJpeg, o)

proc rgbImage(w, h: int; px: seq[byte]; layout = clRgb): Image = imageFromRaw(w, h, layout, isU8, px)

proc cameraRendering(cam: openArray[array[3, float32]]; w, h: int): seq[byte] =
  ## A "camera JPEG": more saturated than sRGB primaries, with a brightening curve.
  let k = [[1.3'f32, -0.2, -0.1], [-0.15'f32, 1.25, -0.1], [0'f32, -0.25, 1.25]]
  var px: seq[byte]
  for c in cam:
    for i in 0 .. 2:
      var v = clamp(k[i][0] * c[0] + k[i][1] * c[1] + k[i][2] * c[2], 0'f32, 1'f32)
      v = 1.6'f32 * v / (1'f32 + 0.6'f32 * v)
      let e = if v <= 0.0031308'f32: 12.92'f32 * v else: 1.055'f32 * pow(v, 1'f32 / 2.4'f32) - 0.055'f32
      px.add byte(round(e * 255'f32))
  jpeg(rgbImage(w, h, px))

proc nefUncompressed(cam: openArray[array[3, float32]]; w, h: int; preview: Option[seq[byte]];
                     wb = none(array[2, uint32])): seq[byte] =
  ## An uncompressed NEF-like raw of `cam` with an optional JPEG preview in IFD0.
  let data = mosaic(cam, w, [0'u8, 1, 1, 2], 0, 4095)
  var t = TiffBuilder()
  var stripData: seq[byte]
  for v in data: stripData.add [byte(v and 0xFF), byte(v shr 8)]
  let strip = t.blob(stripData)
  let raw = t.ifd(@[
    (254'u16, vLong([0'u32])), (256'u16, vLong([uint32(w)])), (257'u16, vLong([uint32(h)])), (258'u16, vShort([12'u16])),
    (259'u16, vShort([1'u16])), (262'u16, vShort([32803'u16])), (273'u16, vBlobs([strip])), (277'u16, vShort([1'u16])),
    (278'u16, vLong([uint32(h)])), (279'u16, vLong([uint32(data.len * 2)])), (33421'u16, vShort([2'u16, 2])),
    (33422'u16, vByte([0'u8, 1, 1, 2]))])
  var ifd0 = @[(271'u16, vAscii("NIKON CORPORATION")), (330'u16, vIfds([raw]))]
  if wb.isSome:
    var note = TiffBuilder()
    let noteIfd = note.ifd(@[(0x000C'u16, vRational([(wb.get[0], 1024'u32), (wb.get[1], 1024'u32), (1'u32, 1'u32), (1'u32, 1'u32)]))])
    note.chain = @[noteIfd]
    var maker: seq[byte]
    for ch in "Nikon\0\x02\x10\0\0": maker.add byte(ch)
    maker.add note.build()
    let exif = t.ifd(@[(37500'u16, vUndefined(maker))])
    ifd0.add (34665'u16, vIfds([exif]))
  if preview.isSome:
    let b = t.blob(preview.get)
    ifd0.add (513'u16, vBlobs([b]))
    ifd0.add (514'u16, vLong([uint32(preview.get.len)]))
  let i0 = t.ifd(ifd0)
  t.chain = @[i0]
  t.build()

proc nefWithPreview(): seq[byte] =
  ## A NEF-like file with Nikon's compression and a full-size baseline JPEG preview in IFD0.
  let j = encode(rgbImage(32, 20, newSeqWith(32 * 20 * 3, 180'u8)), fmtJpeg, defaultEncodeOptions())
  var t = TiffBuilder()
  let strip = t.blob(newSeq[byte](64))
  let preview = t.blob(j)
  let raw = t.ifd(@[
    (256'u16, vLong([8'u32])), (257'u16, vLong([8'u32])), (258'u16, vShort([12'u16])), (259'u16, vShort([34713'u16])),
    (262'u16, vShort([32803'u16])), (273'u16, vBlobs([strip])), (279'u16, vLong([64'u32])), (33421'u16, vShort([2'u16, 2])),
    (33422'u16, vByte([0'u8, 1, 1, 2]))])
  let ifd0 = t.ifd(@[(271'u16, vAscii("NIKON CORPORATION")), (330'u16, vIfds([raw])), (513'u16, vBlobs([preview])),
                     (514'u16, vLong([uint32(j.len)]))])
  t.chain = @[ifd0]
  t.build()

proc fitted(w: seq[string]): bool = w.anyIt("fitted to the camera's embedded JPEG" in it)
proc fallback(w: seq[string]): bool = w.anyIt("sRGB primaries" in it)

suite "io: camera raw":
  test "camera names drop a repeated make":
    check cameraNameForTest(some("Canon"), some("Canon EOS 80D")) == "Canon EOS 80D"
    check cameraNameForTest(some("NIKON CORPORATION"), some("NIKON D3200")) == "NIKON D3200"
    check cameraNameForTest(some("SONY"), some("ILCE-7M3")) == "SONY ILCE-7M3"
    check cameraNameForTest(none(string), some("X")) == "X"
    check cameraNameForTest(some("Panasonic"), some("DC-G9")) == "Panasonic DC-G9"

  test "a DNG opens as 16-bit ProPhoto":
    let (w, h) = (40, 24)
    var spec = dngCfa(w, h, mosaic(scene(w, h), w, [0'u8, 1, 1, 2], 0, 65535))
    spec.asShotNeutral = some([0.6, 1.0, 0.8])
    let r = importOk("shot.dng", spec.build())
    let d = r.document
    check (d.size.width, d.size.height) == (40, 24)
    check d.mode == cmRgb and d.depth == stU16
    check d.layers.len == 1
    check not d.iccProfile.isNil and d.iccProfile.data == bProPhotoCompat.profile.toBytes
    check r.warnings.anyIt("DNG" in it and "ProPhoto" in it)

  test "a monochrome DNG opens (#3497)":
    let (w, h) = (40, 24)
    var data: seq[uint16]
    for i in 0 ..< w * h: data.add uint16((i mod w) * 1600)
    let r = importOk("S261009_90_67.dng", dngMonochrome(w, h, data).build())
    check (r.document.size.width, r.document.size.height, r.document.mode, r.document.depth) == (40, 24, cmRgb, stU16)
    check r.warnings.allIt("colour matrix" notin it and "preview" notin it)

  test "a CR2 opens upright":
    let (w, h) = (48, 16)
    let spec = Cr2Spec(width: w, height: h, data: mosaic(scene(w, h), w, [0'u8, 1, 1, 2], 256, 12000), precision: 14,
                       components: 2, slices: @[24, 24], orientation: 6)
    let r = importOk("IMG_0001.CR2", spec.build())
    check (r.document.size.width, r.document.size.height) == (16, 48)
    check r.document.depth == stU16

  test "an unsupported raw falls back to the embedded preview":
    let r = importOk("DSC_0001.NEF", nefWithPreview())
    check (r.document.size.width, r.document.size.height) == (32, 20)
    check r.warnings.len > 0 and "Nikon compressed NEF" in r.warnings[0] and "embedded" in r.warnings[0]

  test "an unsupported raw without a preview is a clear error":
    var cr3 = @[0'u8, 0, 0, 24]
    for ch in "ftypcrx ": cr3.add byte(ch)
    cr3.add newSeq[byte](12)
    try:
      discard importDoc("IMG_0001.CR3", cr3)
      check false
    except IoFailure as e:
      check e.kind == ieRaw and "CR3" in e.msg

  test "truncated raws do not crash":
    let b = nefWithPreview()
    var n = 0
    while n < b.len:
      try: discard importDoc("x.nef", b[0 ..< n])
      except IoFailure: discard
      n += 7

  test "an uncalibrated raw takes its colour from the camera JPEG":
    let (w, h) = (480, 320)
    let cam = patches(w, h)
    let r = importOk("DSC_0002.NEF", nefUncompressed(cam, w, h, some(cameraRendering(cam, w, h))))
    check fitted(r.warnings) and not fallback(r.warnings)
    check (r.document.size.width, r.document.size.height) == (480, 320)
    check r.document.depth == stU16

  test "Camera Raw tuning keeps the default look and applies the user gains after fitting":
    let (w, h) = (480, 320)
    var cam = patches(w, h)
    for c in cam.mitems:
      for v in c.mitems: v *= 0.6'f32
    var balanced: seq[array[3, float32]]
    for c in cam: balanced.add [c[0] * 2'f32, c[1], c[2] * 1.5'f32]
    let bytes = nefUncompressed(cam, w, h, some(cameraRendering(balanced, w, h)), some([2048'u32, 1536]))
    let normal = importOk("camera.nef", bytes)
    check fitted(normal.warnings)
    let sensor = praw.decode(bytes)
    let look = fitPreviewLook(bytes, sensor, defaultDevelopOptions())
    for tuning in [RawTuning(), RawTuning(exposure: 0.5), RawTuning(exposure: -0.5, temperature: 20, tint: -15)]:
      let (got, applied) = importRawTuned("camera.nef", bytes, tuning)
      check applied and fitted(got.warnings) and not fallback(got.warnings)
      check got.document.depth == stU16
      check (got.document.size.width, got.document.size.height) == (w, h)
      let g = whiteBalanceGains(tuning.temperature, tuning.tint)
      var o = defaultDevelopOptions()
      o.exposure = float64(tuning.exposure)
      o.whiteBalance = multipliersWb([2.0 * float64(g[0]), float64(g[1]), 1.5 * float64(g[2])])
      o.cameraLook = some(look)
      let expected = developSensor(sensor, o)
      var expectedBytes: seq[byte]
      for v in expected.rgb: expectedBytes.add [byte(v and 0xFF), byte(v shr 8)]
      var ws: seq[string]
      let actual = documentToImage(got.document, ws)
      check actual.data == expectedBytes
      if tuning == RawTuning():
        var ws2: seq[string]
        check actual.data == documentToImage(normal.document, ws2).data

  test "unusable previews keep the neutral fallback":
    let (w, h) = (480, 320)
    let cam = patches(w, h)
    var seed = 5'u32
    var noise: seq[byte]
    for _ in 0 ..< w * h * 3:
      seed = seed * 1_103_515_245'u32 + 12345'u32
      noise.add byte((seed shr 16) and 0xFF)
    var garbage = jpeg(rgbImage(64, 48, newSeqWith(64 * 48 * 3, 90'u8)))
    var sos = -1
    for i in countdown(garbage.len - 2, 0):
      if garbage[i] == 0xFF and garbage[i + 1] == 0xDA:
        sos = i
        break
    garbage.setLen(sos + 8)
    garbage.add newSeqWith(200, 0xAB'u8)
    let previews = [none(seq[byte]), some(jpeg(rgbImage(w, h, noise))),
                    some(jpeg(rgbImage(240, 240, newSeqWith(240 * 240 * 3, 128'u8)))),
                    some(jpeg(rgbImage(w, h, newSeqWith(w * h, 128'u8), clGray))), some(garbage)]
    for i, p in previews:
      let bytes = nefUncompressed(cam, w, h, p)
      let r = importOk("DSC_0003.NEF", bytes)
      check fallback(r.warnings) and not fitted(r.warnings)
      let (tuned, wb) = importRawTuned("DSC_0003.NEF", bytes, RawTuning(exposure: 0.5))
      check not wb and fallback(tuned.warnings) and not fitted(tuned.warnings)
      check tuned.document.depth == stU16

  test "truncated raws with a fittable preview do not crash":
    let (w, h) = (96, 64)
    let cam = patches(w, h)
    let b = nefUncompressed(cam, w, h, some(cameraRendering(cam, w, h)))
    var n = 0
    while n < b.len:
      try: discard importDoc("x.nef", b[0 ..< n])
      except IoFailure: discard
      try: discard importRawTuned("x.nef", b[0 ..< n], RawTuning())
      except IoFailure: discard
      n += 211

proc uprightColor(x, y: int): array[3, byte] =
  if x < 8 and y < 16: [230'u8, 20, 20] elif y < 16: [20'u8, 230, 20] else: [20'u8, 20, 230]

proc storedOrientation6(): Image =
  ## The upright 16×32 scene as a camera stores it for Orientation 6 (32×16 landscape).
  let (w, h) = (16, 32)
  var px = newSeq[byte](w * h * 3)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let (sx, sy) = (y, w - 1 - x)
      let c = uprightColor(x, y)
      for k in 0 .. 2: px[(sy * h + sx) * 3 + k] = c[k]
  rgbImage(h, w, px)

proc exifBlock(o: uint16): seq[byte] =
  for ch in "II*\0\x08\0\0\0\x01\0\x12\x01\x03\0\x01\0\0\0": result.add byte(ch)
  result.add [byte(o and 0xFF), byte(o shr 8), 0, 0, 0, 0, 0, 0]

proc jpegWithExif(j, exif: seq[byte]): seq[byte] =
  var seg: seq[byte]
  for ch in "Exif\0\0": seg.add byte(ch)
  seg.add exif
  result = j[0 ..< 2] & @[0xFF'u8, 0xE1, byte((seg.len + 2) shr 8), byte((seg.len + 2) and 0xFF)] & seg & j[2 .. ^1]

proc nefOriented(ifd0Orientation: uint16; preview: seq[byte]): seq[byte] =
  ## A NEF-like raw the decoder can't develop, with IFD0 Orientation and a baseline JPEG preview
  ## stored as the sensor reads out.
  var t = TiffBuilder()
  let strip = t.blob(newSeq[byte](64))
  let pv = t.blob(preview)
  let raw = t.ifd(@[
    (256'u16, vLong([8'u32])), (257'u16, vLong([8'u32])), (258'u16, vShort([12'u16])), (259'u16, vShort([34713'u16])),
    (262'u16, vShort([32803'u16])), (273'u16, vBlobs([strip])), (279'u16, vLong([64'u32])), (33421'u16, vShort([2'u16, 2])),
    (33422'u16, vByte([0'u8, 1, 1, 2]))])
  let ifd0 = t.ifd(@[(271'u16, vAscii("NIKON CORPORATION")), (274'u16, vShort([ifd0Orientation])), (330'u16, vIfds([raw])),
                     (513'u16, vBlobs([pv])), (514'u16, vLong([uint32(preview.len)]))])
  t.chain = @[ifd0]
  t.build()

proc assertUpright(img0: Image) =
  check (img0.width, img0.height) == (16, 32)
  let img = img0.convert(clRgb, isU8)
  for (x, y) in [(3, 4), (12, 4), (3, 28), (12, 28)]:
    let want = uprightColor(x, y)
    for k in 0 .. 2: check abs(int(img.data[(y * 16 + x) * 3 + k]) - int(want[k])) <= 12

suite "io: camera raw previews":
  test "a raw preview is turned upright exactly once":
    var o = defaultEncodeOptions()
    o.jpegQuality = 100
    o.jpegChromaSubsampling = false
    let plain = encode(storedOrientation6(), fmtJpeg, o)
    var r = importOk("DSC_0001.NEF", nefOriented(6, plain))
    assertUpright(codecs.decode(exportDoc(r.document, "png").bytes))
    r = importOk("DSC_0002.NEF", nefOriented(6, jpegWithExif(plain, exifBlock(6))))
    assertUpright(codecs.decode(exportDoc(r.document, "png").bytes))
    r = importOk("DSC_0003.NEF", nefOriented(1, plain))
    check (r.document.size.width, r.document.size.height) == (32, 16)
