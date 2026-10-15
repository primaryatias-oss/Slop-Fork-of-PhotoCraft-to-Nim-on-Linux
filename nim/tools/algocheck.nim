## Nim side of the non-filter algo cross-check (see `tools/algocheck.py` and PORTING.md): the
## same commands as the Rust oracle's `ax_*.rs` modules, reading and writing the same raw buffer
## files (`PCIM`, u32 w, h, ch, kind (0 = f32, 1 = u8), then little-endian samples).
##
##   algocheck gen <dir>                 writes the shared input buffers
##   algocheck <command> <args…> <out>   runs one command (as `photocraft-oracle <command> …`)

import std/[os, strutils, options, math, tables, sequtils, json]
import photocraft/[geom, raster]
import photocraft/algo/[selection, util, histogram, tone, hdr, exif, quantize, erase, stack, trap, paint, retouch, poisson, nonlocal, remove, perspective, scancrop, redeye, seam, magnetic, liquify, render2, pyramid, lens, vanishing]
import photocraft/color as pcolor
import photocraft/color/blend

type Buf = object
  w, h, ch: int
  f: seq[float32]
  b: seq[uint8]

proc readBuf(path: string): Buf =
  let d = readFile(path)
  if d.len < 20 or d[0 .. 3] != "PCIM": quit("not a PCIM file: " & path)
  proc u(o: int): int = int(uint32(ord(d[o])) or (uint32(ord(d[o + 1])) shl 8) or (uint32(ord(d[o + 2])) shl 16) or (uint32(ord(d[o + 3])) shl 24))
  result = Buf(w: u(4), h: u(8), ch: u(12))
  let kind = u(16)
  if kind == 0:
    result.f = newSeq[float32]((d.len - 20) div 4)
    for i in 0 ..< result.f.len: result.f[i] = cast[float32](uint32(u(20 + 4 * i)))
  else:
    result.b = newSeq[uint8](d.len - 20)
    for i in 0 ..< result.b.len: result.b[i] = uint8(ord(d[20 + i]))

proc header(w, h, ch, kind: int): string =
  result = "PCIM"
  for v in [w, h, ch, kind]:
    for k in 0 .. 3: result.add char((uint32(v) shr (8 * k)) and 0xFF)

proc writeF32(path: string; w, h, ch: int; v: openArray[float32]) =
  var o = header(w, h, ch, 0)
  for x in v:
    let u = cast[uint32](x)
    for k in 0 .. 3: o.add char((u shr (8 * k)) and 0xFF)
  writeFile(path, o)

proc writeU8(path: string; w, h, ch: int; v: openArray[uint8]) =
  var o = header(w, h, ch, 1)
  for x in v: o.add char(x)
  writeFile(path, o)

proc rgba(b: Buf): seq[array[4, float32]] =
  result = newSeq[array[4, float32]](b.f.len div 4)
  for i in 0 ..< result.len: result[i] = [b.f[4 * i], b.f[4 * i + 1], b.f[4 * i + 2], b.f[4 * i + 3]]

proc rgba8(b: Buf): seq[array[4, uint8]] =
  result = newSeq[array[4, uint8]](b.b.len div 4)
  for i in 0 ..< result.len: result[i] = [b.b[4 * i], b.b[4 * i + 1], b.b[4 * i + 2], b.b[4 * i + 3]]

proc dumpSurface(path: string; s: Surface; area: Rect) =
  let v = s.readRegion(area)
  writeF32(path, area.width, area.height, s.channels, v)
  let b = s.contentBounds
  echo "tiles ", s.tileCount, " bounds ", b.x0, " ", b.y0, " ", b.x1, " ", b.y1

proc f(a: string): float32 = float32(parseFloat(a))
proc i(a: string): int = parseInt(a)
proc bl(a: string): bool = a == "1" or a == "true"
proc floats(a: string): seq[float32] =
  if a.len == 0: return
  for x in a.split(','): result.add float32(parseFloat(x))
proc rectArg(a: string): Rect =
  let v = a.split(',')
  rect(parseInt(v[0]), parseInt(v[1]), parseInt(v[2]), parseInt(v[3]))

proc bitsStr(v: float32): string = $cast[uint32](v)
proc bits3(v: array[3, float32]): string = "[" & bitsStr(v[0]) & ", " & bitsStr(v[1]) & ", " & bitsStr(v[2]) & "]"
# ---------------------------------------------------------------- inputs

proc hashU(x, y, s: uint32): uint32 =
  var h = x * 0x8da6b343'u32 xor y * 0xd8163841'u32 xor s * 0x9e3779b9'u32
  h = h xor (h shr 15)
  h = h * 0x2c1b3c6d'u32
  h = h xor (h shr 12)
  h

proc gen(dir: string) =
  createDir(dir)
  # A "photo": gradients, blocks of flat colour, noise and a transparent corner.
  let (w, h) = (97, 71)
  var img = newSeq[float32](w * h * 4)
  var img8 = newSeq[uint8](w * h * 4)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let n = float32(hashU(uint32(x), uint32(y), 7) and 0xFF) / 255'f32
      var p: array[4, float32]
      if (x div 16 + y div 12) mod 3 == 0: p = [0.8'f32, 0.2, 0.1, 1]
      elif (x div 16 + y div 12) mod 3 == 1: p = [float32(x) / float32(w), float32(y) / float32(h), 0.5'f32, 1]
      else: p = [0.2'f32 + 0.1'f32 * n, 0.6'f32, 0.9'f32 - 0.2'f32 * n, 1]
      if x + y < 12: p[3] = 0
      elif x > 80 and y > 60: p[3] = 0.5'f32
      for c in 0 .. 3:
        img[(y * w + x) * 4 + c] = p[c]
        img8[(y * w + x) * 4 + c] = uint8(round(clamp(p[c], 0'f32, 1'f32) * 255'f32))
  writeF32(dir / "img.pcim", w, h, 4, img)
  var img2 = newSeq[float32](w * h * 4)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let n = float32(hashU(uint32(x), uint32(y), 5) and 0xFF) / 255'f32
      let p = [0.9'f32 - 0.3'f32 * float32(y) / float32(h), 0.5'f32 + 0.2'f32 * n, 0.2'f32 + 0.5'f32 * float32(x) / float32(w), 1'f32]
      for c in 0 .. 3: img2[(y * w + x) * 4 + c] = p[c]
  writeF32(dir / "img2.pcim", w, h, 4, img2)
  var hdr = newSeq[float32](w * h * 4)
  for k in 0 ..< img.len: hdr[k] = img[k]
  hdr[(5 * w + 50) * 4] = 1.5'f32
  hdr[(6 * w + 50) * 4 + 1] = -0.25'f32
  hdr[(7 * w + 50) * 4 + 2] = NaN
  hdr[(8 * w + 50) * 4 + 3] = Inf
  writeF32(dir / "odd.pcim", w, h, 4, hdr)
  # Exposure brackets of a 12-stop scene through a smooth non-sRGB camera response (8-bit codes),
  # and a copy of the brightest with a "ghost" square.
  let (sw, sh) = (192, 64)
  var scene = newSeq[array[3, float32]](sw * sh)
  for k in 0 ..< sw * sh:
    let (x, y) = (float32(k mod sw) / float32(sw), float32(k div sw) / float32(sh))
    let base = 0.002'f32 * pow(2'f32, x * 12'f32)
    let tex = 1'f32 + 0.3'f32 * sin(y * 40'f32)
    scene[k] = [base * tex, base * tex * 0.8'f32, base * tex * 0.6'f32]
  proc camera(e: float32): float32 = min(pow(max(e, 0'f32), 0.45'f32), 1'f32)
  var rad = newSeq[float32](sw * sh * 4)
  for k in 0 ..< sw * sh:
    for c in 0 .. 2: rad[k * 4 + c] = scene[k][c] * 4'f32
    rad[k * 4 + 3] = 1
  writeF32(dir / "rad.pcim", sw, sh, 4, rad)
  for e, dt in [0.25'f32, 1, 4, 16]:
    var shot = newSeq[float32](sw * sh * 4)
    for k in 0 ..< sw * sh:
      for c in 0 .. 2: shot[k * 4 + c] = round(camera(scene[k][c] * dt) * 255'f32) / 255'f32
      shot[k * 4 + 3] = 1
    writeF32(dir / ("ex" & $e & ".pcim"), sw, sh, 4, shot)
    if e == 3:
      for y in 20 ..< 40:
        for x in 60 ..< 90:
          for c in 0 .. 2: shot[(y * sw + x) * 4 + c] = 0.2'f32
      writeF32(dir / "ex3ghost.pcim", sw, sh, 4, shot)
  writeU8(dir / "img8.pcim", w, h, 4, img8)
  # A Lab image (L, a, b, alpha) covering a range around a few colours.
  var lab = newSeq[float32](w * h * 4)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let n = float32(hashU(uint32(x), uint32(y), 9) and 0xFFFF) / 65535'f32
      lab[(y * w + x) * 4 + 0] = 30'f32 + 50'f32 * float32(x) / float32(w)
      lab[(y * w + x) * 4 + 1] = -40'f32 + 80'f32 * float32(y) / float32(h) + n * 3
      lab[(y * w + x) * 4 + 2] = 20'f32 - 30'f32 * n
      lab[(y * w + x) * 4 + 3] = (if (x + 2 * y) mod 17 == 0: 0'f32 else: 1'f32)
  writeF32(dir / "lab.pcim", w, h, 4, lab)
  # Masks: hard shapes, a soft one and specks.
  var m = newSeq[float32](w * h)
  var soft = newSeq[float32](w * h)
  var m2 = newSeq[float32](w * h)
  for y in 0 ..< h:
    for x in 0 ..< w:
      let inCircle = (x - 40) * (x - 40) + (y - 30) * (y - 30) < 400
      let inRect = x >= 60 and x < 90 and y >= 10 and y < 50
      let speck = hashU(uint32(x), uint32(y), 3) mod 97 == 0
      m[y * w + x] = (if inCircle or inRect or speck: 1'f32 else: 0'f32)
      soft[y * w + x] = clamp(1.5'f32 - sqrt(float32((x - 50) * (x - 50) + (y - 35) * (y - 35))) / 20'f32, 0'f32, 1'f32)
      m2[y * w + x] = (if x > 20 and x < 70 and y > 5 and y < 65: 0.75'f32 else: 0'f32)
  writeF32(dir / "mask.pcim", w, h, 1, m)
  writeF32(dir / "soft.pcim", w, h, 1, soft)
  writeF32(dir / "mask2.pcim", w, h, 1, m2)
  # A large sparse mask spanning several tiles (for the tile-wise paths and wide feathers).
  let (bw, bh) = (700, 530)
  var big = newSeq[float32](bw * bh)
  for y in 0 ..< bh:
    for x in 0 ..< bw:
      let inside = (x - 300) * (x - 300) div 4 + (y - 260) * (y - 260) < 40000 or (x > 500 and x < 690 and y > 20 and y < 120)
      big[y * bw + x] = (if inside: 1'f32 else: 0'f32)
  writeF32(dir / "big.pcim", bw, bh, 1, big)
  # A textured 700 × 530 image with a thin diagonal stroke (the Remove Tool's tiled path) and a
  # small blob.
  var tex = newSeq[float32](bw * bh * 4)
  var stroke = newSeq[float32](bw * bh)
  for y in 0 ..< bh:
    for x in 0 ..< bw:
      let n = float32(hashU(uint32(x), uint32(y), 11) and 0xFF) / 255'f32
      let stripe = (if ((x + 2 * y) div 9) mod 2 == 0: 0.15'f32 else: 0'f32)
      let k = (y * bw + x) * 4
      tex[k] = 0.3'f32 + 0.4'f32 * float32(x) / float32(bw) + stripe + 0.1'f32 * n
      tex[k + 1] = 0.5'f32 + 0.2'f32 * sin(float32(y) * 0.05'f32) - stripe
      tex[k + 2] = 0.4'f32 + 0.2'f32 * n
      tex[k + 3] = 1
      let d = abs(float32(y) - 40'f32 - float32(x) * 0.6'f32)
      if (d < 2.5'f32 and x > 20 and x < 680) or (x - 120) * (x - 120) + (y - 400) * (y - 400) < 100:
        stroke[y * bw + x] = 1
  writeF32(dir / "tex.pcim", bw, bh, 4, tex)
  writeF32(dir / "stroke.pcim", bw, bh, 1, stroke)
  # Flatbed scans: rotated textured photos on a near-white lid (Crop and Straighten Photos); the
  # second is large enough to be downsampled and has dust specks.
  proc scan(w, h: int; photos: openArray[(float64, float64, float64, float64, float64)]; dust: bool): seq[float32] =
    result = newSeq[float32](w * h * 4)
    for k in 0 ..< w * h:
      result[k * 4] = 0.97'f32
      result[k * 4 + 1] = 0.97'f32
      result[k * 4 + 2] = 0.96'f32
      result[k * 4 + 3] = 1
      if dust and hashU(uint32(k mod w), uint32(k div w), 13) mod 4001 == 0:
        result[k * 4] = 0.1'f32
        result[k * 4 + 1] = 0.1'f32
        result[k * 4 + 2] = 0.1'f32
    for pk, (cx, cy, pw, ph, ang) in photos:
      let (s, c) = (sin(ang * PI / 180.0), cos(ang * PI / 180.0))
      for y in 0 ..< h:
        for x in 0 ..< w:
          let (dx, dy) = (float64(x) + 0.5 - cx, float64(y) + 0.5 - cy)
          let (u, v) = (c * dx + s * dy, -s * dx + c * dy)
          if abs(u) < pw / 2.0 and abs(v) < ph / 2.0:
            let t = float32(sin(u * 0.05) * cos(v * 0.07))
            let k = (y * w + x) * 4
            result[k] = 0.3'f32 + 0.2'f32 * t
            result[k + 1] = 0.2'f32 + 0.1'f32 * float32(pk)
            result[k + 2] = 0.5'f32 - 0.2'f32 * t
  writeF32(dir / "scan.pcim", 600, 440, 4, scan(600, 440, [(150.0, 120.0, 180.0, 120.0, 7.0), (450.0, 130.0, 160.0, 130.0, -12.0),
                                                            (300.0, 330.0, 220.0, 110.0, 3.0)], false))
  writeF32(dir / "scan2.pcim", 1700, 1000, 4, scan(1700, 1000, [(400.0, 300.0, 500.0, 380.0, 31.0), (1200.0, 320.0, 420.0, 300.0, -44.0),
                                                                (500.0, 780.0, 300.0, 200.0, 89.0), (1300.0, 760.0, 600.0, 280.0, 0.0)], true))
  # Faces with red pupils (Red Eye): two eyes with catchlights in pinkish skin, plus noise; and the
  # red half-plane of the Rust tile-seam test.
  let (ew, eh) = (96, 64)
  var eye = newSeq[float32](ew * eh * 4)
  for y in 0 ..< eh:
    for x in 0 ..< ew:
      let n = float32(hashU(uint32(x), uint32(y), 17) and 0xFF) / 255'f32
      var p = [0.9'f32, 0.66'f32 + 0.05'f32 * n, 0.55'f32, 1'f32]
      for (cx, cy, r) in [(30.0, 32.0, 10.0), (70.0, 30.0, 7.0)]:
        let d = hypot(float64(x) - cx, float64(y) - cy)
        if d <= 1.2: p = [1'f32, 1, 1, 1]
        elif d <= r: p = [0.95'f32 - 0.1'f32 * n, 0.10'f32 + 0.05'f32 * n, 0.12'f32, 1]
        elif d <= r + 4.0: p = [0.38'f32, 0.28'f32, 0.16'f32 + 0.05'f32 * n, 1]
      let k = (y * ew + x) * 4
      for c in 0 .. 3: eye[k + c] = p[c]
  eye[(32 * ew + 36) * 4] = NaN
  writeF32(dir / "eye.pcim", ew, eh, 4, eye)
  var half = newSeq[float32](128 * 128 * 4)
  for k in 0 ..< 128 * 128:
    let p = (if k mod 128 < 64: [0.95'f32, 0.10, 0.10, 1] else: [0.38'f32, 0.28, 0.16, 1])
    for c in 0 .. 3: half[k * 4 + c] = p[c]
  writeF32(dir / "half.pcim", 128, 128, 4, half)
  # A tall RGB image (Content-Aware Scale's proxy-height path) and its protect map.
  let (tw, th) = (300, 2100)
  var tall = newSeq[float32](tw * th * 3)
  var tallProt = newSeq[float32](tw * th)
  for y in 0 ..< th:
    for x in 0 ..< tw:
      let n = float32(hashU(uint32(x), uint32(y), 19) and 0xFF) / 255'f32
      let k = (y * tw + x) * 3
      tall[k] = 0.5'f32 + 0.3'f32 * sin(float32(x) * 0.07'f32 + float32(y) * 0.002'f32)
      tall[k + 1] = (if (x - 150) * (x - 150) + (y mod 400 - 200) * (y mod 400 - 200) < 2500: 0.9'f32 else: 0.2'f32 + 0.05'f32 * n)
      tall[k + 2] = float32(y) / float32(th)
      tallProt[y * tw + x] = (if x in 40 ..< 70: 1'f32 elif x in 200 ..< 210: 0.4'f32 else: 0'f32)
  writeF32(dir / "tall.pcim", tw, th, 3, tall)
  writeF32(dir / "tallprot.pcim", tw, th, 1, tallProt)
  var col = newSeq[float32](40 * 2)
  for y in 0 ..< 40:
    col[y * 2] = float32(y) / 40'f32
    col[y * 2 + 1] = 1
  writeF32(dir / "col.pcim", 1, 40, 2, col)
  # An RGBA8 picture for the Magnetic Lasso: a dark disc, a tilted bar and a faint step over a noisy
  # gradient, with a transparent hole.
  let (mw, mh) = (300, 200)
  var edge8 = newSeq[uint8](mw * mh * 4)
  for y in 0 ..< mh:
    for x in 0 ..< mw:
      let n = int(hashU(uint32(x), uint32(y), 23) and 15)
      var v = [uint8(60 + x div 6 + n), uint8(90 + n), uint8(140 - y div 4), 255'u8]
      if hypot(float64(x) - 90.5, float64(y) - 100.5) < 45: v = [uint8(20 + n), 25'u8, 30'u8, 255'u8]
      let (u, t) = (float64(x - 210) * 0.94 + float64(y - 100) * 0.34, -float64(x - 210) * 0.34 + float64(y - 100) * 0.94)
      if abs(u) < 12 and abs(t) < 70: v = [230'u8, uint8(200 + n), 180'u8, 255'u8]
      if y > 170 and x > 120: v[0] = uint8(min(int(v[0]) + 12, 255))
      if x > 270 and y < 30: v[3] = 0
      for c in 0 .. 3: edge8[(y * mw + x) * 4 + c] = v[c]
  writeU8(dir / "edge8.pcim", mw, mh, 4, edge8)
  # A focus stack: three shots of one scene, each sharp in its own band and blurred elsewhere,
  # the third partly transparent.
  let (fw, fh) = (120, 90)
  var scene2 = newSeq[array[3, float32]](fw * fh)
  for y in 0 ..< fh:
    for x in 0 ..< fw:
      let n = float32(hashU(uint32(x), uint32(y), 29) and 0xFF) / 255'f32
      let ck = (if ((x div 3) + (y div 3)) mod 2 == 0: 0.25'f32 else: 0.75'f32)
      scene2[y * fw + x] = [ck * 0.8'f32 + 0.2'f32 * n, 0.3'f32 + 0.4'f32 * float32(x) / float32(fw), ck * 0.5'f32 + 0.1'f32 * n]
  var blurred = newSeq[array[3, float32]](fw * fh)
  for y in 0 ..< fh:
    for x in 0 ..< fw:
      var s = [0'f32, 0, 0]
      var cnt = 0'f32
      for dy in -3 .. 3:
        for dx in -3 .. 3:
          let (xx, yy) = (clamp(x + dx, 0, fw - 1), clamp(y + dy, 0, fh - 1))
          for c in 0 .. 2: s[c] += scene2[yy * fw + xx][c]
          cnt += 1
      blurred[y * fw + x] = [s[0] / cnt, s[1] / cnt, s[2] / cnt]
  for k in 0 .. 2:
    var shot = newSeq[float32](fw * fh * 4)
    for y in 0 ..< fh:
      for x in 0 ..< fw:
        let sharp = x div 40 == k
        let p = (if sharp: scene2[y * fw + x] else: blurred[y * fw + x])
        let i = (y * fw + x) * 4
        shot[i] = p[0]
        shot[i + 1] = p[1]
        shot[i + 2] = p[2]
        shot[i + 3] = (if k == 2 and y < 20: 0'f32 elif k == 2 and y < 30: 0.4'f32 else: 1'f32)
    writeF32(dir / ("fs" & $k & ".pcim"), fw, fh, 4, shot)

# ---------------------------------------------------------------- selection

proc surfaceOf(path: string; area: Rect): Option[Surface] =
  if path == "-": return none(Surface)
  some(maskToSurface(readBuf(path).f, area))

proc cAdd(a, b: float32): float32 = rmax(a, b)
proc cSub(a, b: float32): float32 = rmax(a - b, 0'f32)
proc cInt(a, b: float32): float32 = rmin(a, b)
proc cDiff(a, b: float32): float32 = abs(a - b)

proc combo(op: string): proc(a, b: float32): float32 =
  case op
  of "add": result = cAdd
  of "sub": result = cSub
  of "int": result = cInt
  else: result = cDiff

proc selectionCmd(a: seq[string]): bool =
  let o = a[^1]
  case a[1]
  of "sel.wand":
    let b = readBuf(a[2])
    let area = rect(0, 0, b.w, b.h)
    writeF32(o, b.w, b.h, 1, magicWand(rgba(b), area, (i(a[3]), i(a[4])), f(a[5]), bl(a[6]), bl(a[7])))
  of "sel.wandregion":
    let b = readBuf(a[2])
    let area = rect(0, 0, b.w, b.h)
    let r = wandRegion(rgba8(b), area, (i(a[3]), i(a[4])), f(a[5]), bl(a[6]), bl(a[7]))
    if r.isSome:
      let rr = r.get
      echo "bbox ", rr.bbox.x0, " ", rr.bbox.y0, " ", rr.bbox.x1, " ", rr.bbox.y1
      writeU8(o, rr.bbox.width, rr.bbox.height, 1, rr.mask)
    else: echo "none"
  of "sel.colorrange":
    let b = readBuf(a[2])
    let c = floats(a[3])
    writeF32(o, b.w, b.h, 1, colorRange(rgba(b), [c[0], c[1], c[2]], f(a[4])))
  of "sel.samples":
    let b = readBuf(a[2])
    var samples: seq[RangeSample]
    for s in a[3].split(';'):
      if s.len == 0: continue
      let v = floats(s)
      samples.add RangeSample(color: [v[0], v[1], v[2]], at: (if v.len >= 5: some((v[3], v[4])) else: none((float32, float32))))
    let loc = (if a[5] == "-": none(float32) else: some(f(a[5])))
    writeF32(o, b.w, b.h, 1, colorRangeSamples(rgba(b), b.w, samples, f(a[4]), loc))
  of "sel.lab":
    let b = readBuf(a[2])
    let (lo, hi) = (floats(a[3]), floats(a[4]))
    let r = LabRange(lo: [lo[0], lo[1], lo[2]], hi: [hi[0], hi[1], hi[2]])
    var loc = none((float32, seq[(float32, float32)]))
    if a[7] != "-":
      let parts = a[7].split(';')
      var pts: seq[(float32, float32)]
      for p in parts[1 .. ^1]:
        let v = floats(p)
        pts.add (v[0], v[1])
      loc = some((f(parts[0]), pts))
    writeF32(o, b.w, b.h, 1, colorRangeLab(rgba(b), b.w, r, f(a[5]), bl(a[6]), loc))
  of "sel.labpoint":
    let (l, lo, hi) = (floats(a[2]), floats(a[3]), floats(a[4]))
    let r = LabRange(lo: [lo[0], lo[1], lo[2]], hi: [hi[0], hi[1], hi[2]])
    let lab = [l[0], l[1], l[2]]
    echo bitsStr(labRangeCoverage(lab, r, f(a[5]))), " ", bitsStr(labRangeCoverage8bit(lab, r, f(a[5]))), " ",
         bits3(quantizeLab8(lab)), " ", bits3(r.grow(lab).lo), " ", bits3(r.grow(lab).hi)
  of "sel.subtract":
    let (lo, hi, m) = (floats(a[2]), floats(a[3]), floats(a[4]))
    let r = LabRange(lo: [lo[0], lo[1], lo[2]], hi: [hi[0], hi[1], hi[2]]).subtract([m[0], m[1], m[2]], f(a[5]))
    echo bits3(r.lo), " ", bits3(r.hi)
  of "sel.hue":
    let b = readBuf(a[2])
    writeF32(o, b.w, b.h, 1, hueRange(rgba(b), f(a[3])))
  of "sel.tone":
    let b = readBuf(a[2])
    writeF32(o, b.w, b.h, 1, toneRange(rgba(b), f(a[3]), f(a[4]), f(a[5])))
  of "sel.modify":
    let b = readBuf(a[3])
    let r = f(a[4])
    let m = (case a[2]
      of "expand": expand(b.f, b.w, b.h, r)
      of "contract": contract(b.f, b.w, b.h, r)
      of "border": border(b.f, b.w, b.h, r)
      of "smooth": smooth(b.f, b.w, b.h, r)
      else: feather(b.f, b.w, b.h, r))
    writeF32(o, b.w, b.h, 1, m)
  of "sel.polygon":
    let p = floats(a[2])
    var pts: seq[(float32, float32)]
    for k in 0 ..< p.len div 2: pts.add (p[2 * k], p[2 * k + 1])
    let area = rectArg(a[3])
    writeF32(o, area.width, area.height, 1, polygon(pts, area, bl(a[4])))
  of "sel.ellipse":
    dumpSurface(o, ellipseSurface(rectArg(a[2]), rectArg(a[3]), bl(a[4])), rectArg(a[5]))
  of "sel.combine":
    let area = rectArg(a[5])
    let old = surfaceOf(a[3], area)
    let nm = readBuf(a[4])
    let s = combine(old, nm.f, area, parseSelectionMode(a[2]))
    if s.isSome: dumpSurface(o, s.get, rectArg(a[6])) else: echo "none"
  of "sel.combineregion":
    let b = readBuf(a[4])
    let area = rect(0, 0, b.w, b.h)
    let old = surfaceOf(a[3], area)
    let r = wandRegion(rgba8(b), area, (i(a[5]), i(a[6])), f(a[7]), bl(a[8]), bl(a[9]))
    let s = combineRegion(old, r, parseSelectionMode(a[2]))
    if s.isSome: dumpSurface(o, s.get, area) else: echo "none"
  of "sel.combinesurf":
    let sa = surfaceOf(a[3], rectArg(a[4])).get
    let sb = surfaceOf(a[5], rectArg(a[6])).get
    dumpSurface(o, combineSurfaces(sa, sb, rectArg(a[7]), combo(a[2])), rectArg(a[8]))
  of "sel.edt":
    let b = readBuf(a[2])
    var inside = newSeq[bool](b.f.len)
    for k, v in b.f: inside[k] = v >= 0.5'f32
    writeF32(o, b.w, b.h, 1, edt(inside, b.w, b.h))
  else: return false
  true

# ---------------------------------------------------------------- tone

proc bitsSeq(v: openArray[float32]): string =
  result = "["
  for k, x in v:
    if k > 0: result.add ", "
    result.add $cast[uint32](x)
  result.add "]"

proc u64s(v: openArray[uint64]): string =
  result = "["
  for k, x in v:
    if k > 0: result.add ", "
    result.add $x
  result.add "]"

proc curveArg(a: string): seq[(float32, float32)] =
  if a == "-": return
  for p in a.split(';'):
    let v = floats(p)
    result.add (v[0], v[1])

proc flat4(px: openArray[array[4, float32]]): seq[float32] =
  for p in px:
    for v in p: result.add v

proc toneCmd(a: seq[string]): bool =
  let o = a[^1]
  case a[1]
  of "tone.hist":
    let b = readBuf(a[2])
    let h = fromRgba(rgba(b))
    echo h.samples, " ", h.transparent, " ", h.invalid, " ", u64s(h.underflow), " ", u64s(h.overflow), " ",
         u64s(h.shadows), " ", u64s(h.highlights)
    var fl: seq[float32]
    for c in h.channels:
      for v in c: fl.add float32(v)
    writeF32(o, 256, 3, 1, fl)
  of "tone.clip":
    let v = floats(a[2])
    let (s, hi) = clipping([v[0], v[1], v[2], v[3]])
    echo "(", s, ", ", hi, ")"
  of "tone.sh":
    let b = readBuf(a[2])
    let v = floats(a[3])
    let p = ShadowsHighlights(shadowAmount: v[0], shadowTone: v[1], shadowRadius: v[2], highlightAmount: v[3],
                              highlightTone: v[4], highlightRadius: v[5], color: v[6], midtone: v[7],
                              blackClip: v[8], whiteClip: v[9])
    var px = rgba(b)
    shadowsHighlights(px, b.w, b.h, p)
    writeF32(o, b.w, b.h, 4, flat4(px))
  of "tone.replace":
    let b = readBuf(a[2])
    let c = floats(a[3])
    var px = rgba(b)
    let m = replaceColor(px, [c[0], c[1], c[2]], f(a[4]), f(a[5]), f(a[6]), f(a[7]))
    var v: seq[float32]
    for k in 0 ..< min(px.len, m.len):
      for x in px[k]: v.add x
      v.add m[k]
    for k in px.len ..< m.len: v.add m[k]
    writeF32(o, b.w, b.h, 5, v)
  of "tone.labstats":
    let b = readBuf(a[2])
    let m = (if a[3] == "-": none(seq[float32]) else: some(readBuf(a[3]).f))
    let s = labStats(rgba(b), m)
    if s.isSome: echo bitsSeq(s.get.mean), " ", bitsSeq(s.get.std) else: echo "none"
  of "tone.match":
    let b = readBuf(a[2])
    var px = rgba(b)
    let t = labStats(px, none(seq[float32])).get
    let s = (if a[3] == "-": none(LabStats) else: labStats(rgba(readBuf(a[3])), none(seq[float32])))
    matchColor(px, t, s, MatchColor(luminance: f(a[4]), intensity: f(a[5]), fade: f(a[6]), neutralize: bl(a[7])))
    writeF32(o, b.w, b.h, 4, flat4(px))
  of "tone.hdr":
    let b = readBuf(a[2])
    let v = floats(a[3])
    let p = HdrToning(radius: v[0], strength: v[1], gamma: v[2], exposure: v[3], detail: v[4], shadow: v[5],
                      highlight: v[6], vibrance: v[7], saturation: v[8], curve: curveArg(a[4]))
    var px = rgba(b)
    hdrToning(px, b.w, b.h, p)
    writeF32(o, b.w, b.h, 4, flat4(px))
  else: return false
  true

# ---------------------------------------------------------------- hdr

proc u64bits(v: openArray[float64]): string =
  result = "["
  for k, x in v:
    if k > 0: result.add ", "
    result.add $cast[uint64](x)
  result.add "]"

proc grey(b: Buf): seq[float32] =
  for k in 0 ..< b.f.len div 4:
    result.add 0.2126'f32 * b.f[4 * k] + 0.7152'f32 * b.f[4 * k + 1] + 0.0722'f32 * b.f[4 * k + 2]

proc hdrCmd(a: seq[string]): bool =
  let o = a[^1]
  case a[1]
  of "hdr.estimate":
    var imgs: seq[seq[array[4, float32]]]
    for p in a[2 ..< a.len - 1]: imgs.add rgba(readBuf(p))
    echo u64bits(estimateExposures(imgs))
  of "hdr.mtb":
    let b = readBuf(a[2])
    let (w, h) = (b.w, b.h)
    let g = grey(b)
    let (dx, dy, k) = (i(a[3]), i(a[4]), f(a[5]))
    var shifted = newSeq[float32](w * h)
    for q in 0 ..< w * h:
      let (x, y) = (q mod w - dx, q div w - dy)
      shifted[q] = g[clamp(y, 0, h - 1) * w + clamp(x, 0, w - 1)] * k
    let (ox, oy) = mtbOffset(w, h, shifted, g, parseInt(a[6]))
    echo "(", ox, ", ", oy, ")"
  of "hdr.samples":
    let b = readBuf(a[2])
    echo "[", samplePixels(b.w, b.h, rgba(b), parseInt(a[3])).join(", "), "]"
  of "hdr.merge":
    var ex: seq[float64]
    for v in floats(a[2]): ex.add float64(v)
    var bufs: seq[Buf]
    for p in a[5 ..< a.len - 1]: bufs.add readBuf(p)
    var imgs: seq[seq[array[4, float32]]]
    for b in bufs: imgs.add rgba(b)
    let base = parseInt(a[4])
    let opts = MergeOptions(exposures: ex, removeGhosts: bl(a[3]), ghostBase: (if base >= 0: some(base) else: none(int)))
    let m = merge(bufs[0].w, bufs[0].h, imgs, opts)
    echo m.ghostBase, " ", cast[uint64](m.ghostFraction), " ", cast[uint64](m.stops)
    for r in m.response: echo u64bits(r)
    writeF32(o, bufs[0].w, bufs[0].h, 4, flat4(m.px))
  of "hdr.tonemap":
    let b = readBuf(a[2])
    let v = floats(a[4])
    let m = (case a[3]
      of "local": ToneMethod(kind: tmLocalAdaptation, toning: HdrToning(radius: v[0], strength: v[1], gamma: v[2],
                  exposure: v[3], detail: v[4], shadow: v[5], highlight: v[6], vibrance: v[7], saturation: v[8]))
      of "expgamma": ToneMethod(kind: tmExposureGamma, exposure: v[0], gamma: v[1])
      of "highlight": ToneMethod(kind: tmHighlightCompression)
      else: ToneMethod(kind: tmEqualizeHistogram))
    var px = rgba(b)
    toneMap(px, b.w, b.h, m)
    writeF32(o, b.w, b.h, 4, flat4(px))
  else: return false
  true

# ---------------------------------------------------------------- exif

proc exifLine(i: CameraInfo): string =
  proc s(v: Option[string]): string = (if v.isSome: v.get else: "-")
  proc n(v: Option[float64]): string = (if v.isSome: $cast[uint64](v.get) else: "-")
  s(i.make) & "|" & s(i.model) & "|" & s(i.lens) & "|" & n(i.exposureTime) & "|" & n(i.fNumber) & "|" & n(i.iso) & "|" &
    n(i.focalLength) & "|" & n(i.focalLength35mm) & "|" & s(i.dateTaken) & "|" & n(i.exposureFactor)

proc exifSample(): CameraInfo =
  CameraInfo(make: some("Generic"), model: some("Camera 1"), lens: some("24-70mm \xEF\xBF\xBD"), exposureTime: some(0.004),
             fNumber: some(8.0), iso: some(200.0), focalLength: some(35.0), focalLength35mm: some(52.0),
             dateTaken: some("2024:02:17 12:40:19"))

proc exifCmd(a: seq[string]): bool =
  let o = a[^1]
  case a[1]
  of "exif.build":
    let b = build(exifSample())
    var s = newString(b.len)
    for k, x in b: s[k] = char(x)
    writeFile(o, s)
    echo exifLine(read(b))
  of "exif.trunc":
    let b = build(exifSample())
    for cut in 0 .. b.len: echo cut, " ", exifLine(read(b[0 ..< cut]))
  of "exif.fuzz":
    var x = uint32(parseInt(a[2]))
    var good = build(exifSample())
    for k in 0 ..< parseInt(a[3]):
      var n = (if k mod 2 == 0: @[byte('I'), byte('I'), 42, 0, 8, 0, 0, 0] else: @[byte('M'), byte('M'), 0, 42, 0, 0, 0, 8])
      for _ in 0 ..< 96:
        x = x xor (x shl 13)
        x = x xor (x shr 17)
        x = x xor (x shl 5)
        n.add byte(x and 0xFF)
      echo k, " ", exifLine(read(n))
      let at = int(x mod uint32(good.len))
      good[at] = good[at] xor byte((x shr 8) and 0xFF)
      echo k, "f ", exifLine(read(good))
  else: return false
  true

# ---------------------------------------------------------------- quantize

proc palLine(p: openArray[Rgb8]): string =
  for k, c in p:
    if k > 0: result.add ' '
    result.add toHex(c[0], 2).toLowerAscii & toHex(c[1], 2).toLowerAscii & toHex(c[2], 2).toLowerAscii

proc forcedArg(s: string): Forced =
  case s
  of "bw": fcBlackWhite
  of "primaries": fcPrimaries
  of "web": fcWeb
  else: fcNone

proc ditherArg(s: string): Dither =
  case s
  of "diffusion": dtDiffusion
  of "pattern": dtPattern
  of "noise": dtNoise
  else: dtNone

proc quantizeCmd(a: seq[string]): bool =
  let o = a[^1]
  case a[1]
  of "q.fixed":
    echo palLine(webPalette())
    echo palLine(macPalette())
    echo palLine(windowsPalette())
    for n in [0, 2, 8, 9, 26, 27, 64, 100, 216, 256, 1000]: echo n, " ", palLine(uniformPalette(n))
  of "q.palette":
    let b = readBuf(a[2])
    try: echo palLine(buildPalette(rgba(b), paletteKindFromId(a[3]).get, parseInt(a[4]), forcedArg(a[5])))
    except QuantizeError as e: echo "ERR ", e.msg
  of "q.quantize":
    let b = readBuf(a[2])
    var px = rgba(b)
    let pal = buildPalette(px, paletteKindFromId(a[3]).get, parseInt(a[4]), forcedArg(a[5]))
    let t = parseInt(a[8])
    let idx = quantize(px, b.w, pal, ditherArg(a[6]), f(a[7]), (if t >= 0: some(t) else: none(int)))
    var line = ""
    for v in idx: line.add toHex(v, 2).toLowerAscii
    echo line
    writeF32(o, b.w, b.h, 4, flat4(px))
  of "q.bitmap":
    let b = readBuf(a[2])
    var g = b.f
    let m = (case a[3]
      of "threshold": BitmapMethod(kind: bmThreshold)
      of "pattern": BitmapMethod(kind: bmPattern)
      of "diffusion": BitmapMethod(kind: bmDiffusion)
      else: BitmapMethod(kind: bmHalftone, cell: f(a[4]), angle: f(a[5]), shape: halftoneShapeFromId(a[6]).get))
    toBitmap(g, b.w, m)
    writeF32(o, b.w, b.h, 1, g)
  else: return false
  true

# ---------------------------------------------------------------- retouch / paint

proc fmtArg(s0: string): PixelFormat =
  var s = s0
  var mode = cmRgb
  for (p, m) in [("cmyk", cmCmyk), ("gray", cmGrayscale), ("lab", cmLab), ("rgb", cmRgb)]:
    if s.startsWith(p):
      mode = m
      s = s[p.len .. ^1]
      break
  let alpha = not s.endsWith("x")
  let st = (case s.strip(chars = {'x'}, leading = false)
    of "16": stU16
    of "32": stF32
    else: stU8)
  pixelFormat(mode, st, alpha)

proc surfaceArg(f: string; b: Buf): Surface =
  var s = newSurface(pixelFormat(cmRgb, stF32, true))
  s.writeRegion(rect(0, 0, b.w, b.h), b.f)
  s.convert(fmtArg(f))

proc maskArg(path: string; area: Rect): Option[Surface] =
  if path == "-": none(Surface) else: some(maskToSurface(readBuf(path).f, area))

proc blendArg(s: string): BlendMode =
  case s
  of "multiply": bmMultiply
  of "screen": bmScreen
  of "overlay": bmOverlay
  of "difference": bmDifference
  of "color": bmColor
  of "luminosity": bmLuminosity
  of "dissolve": bmDissolve
  of "hardmix": bmHardMix
  of "lighten": bmLighten
  else: bmNormal

proc shapeArg(s: string): GradientShape =
  case s
  of "radial": gsRadial
  of "angle": gsAngle
  of "reflected": gsReflected
  of "diamond": gsDiamond
  else: gsLinear

proc statArg(s: string): Stat =
  case s
  of "entropy": stEntropy
  of "kurtosis": stKurtosis
  of "maximum": stMaximum
  of "mean": stMean
  of "median": stMedian
  of "minimum": stMinimum
  of "range": stRange
  of "skewness": stSkewness
  of "sd": stStandardDeviation
  of "sum": stSummation
  else: stVariance

proc toneArg(s: string): ToneRange =
  case s
  of "shadows": trShadows
  of "highlights": trHighlights
  else: trMidtones

proc optAlpha(s: string): Option[int] =
  let v = parseInt(s)
  if v >= 0: some(v) else: none(int)

proc retouchCmd(a: seq[string]): bool =
  let o = a[^1]
  case a[1]
  of "rt.erase":
    let b = readBuf(a[3])
    let area = rect(0, 0, b.w, b.h)
    var s = surfaceArg(a[2], b)
    let img = rgba8Image(s, area)
    let region = wandRegion(img, area, (i(a[4]), i(a[5])), f(a[6]), bl(a[7]), bl(a[8]))
    if region.isNone:
      echo "none"
      return true
    let lock = (if a[11] == "-": none(seq[float32]) else: some(floats(a[11])))
    let r = magicErase(s, region.get, f(a[9]), maskArg(a[10], area), lock)
    echo "dmg ", r.x0, " ", r.y0, " ", r.x1, " ", r.y1
    dumpSurface(o, s, area)
  of "rt.stack":
    var layers: seq[seq[array[4, float32]]]
    for p in a[3 ..< a.len - 1]: layers.add rgba(readBuf(p))
    let r = stack.combine(layers, statArg(a[2]))
    let b = readBuf(a[3])
    writeF32(o, b.w, b.h, 4, flat4(r))
  of "rt.trap":
    let b = readBuf(a[2])
    var px = b.f
    trap(px, b.w, b.h, b.ch, parseInt(a[3]))
    writeF32(o, b.w, b.h, b.ch, px)
  of "rt.gradient":
    let b = readBuf(a[3])
    var s = surfaceArg(a[2], b)
    let p = floats(a[5])
    var stops: seq[Stop4]
    for t in a[6].split(';'):
      let v = floats(t)
      stops.add (v[0], [v[1], v[2], v[3], v[4]])
    let full = rect(0, 0, b.w, b.h)
    paintGradient(s, rectArg(a[13]), (p[0], p[1]), (p[2], p[3]), shapeArg(a[4]), stops, bl(a[7]), f(a[8]),
                  blendArg(a[9]), bl(a[10]), maskArg(a[11], full), bl(a[12]))
    dumpSurface(o, s, full)
  of "rt.bucket":
    let b = readBuf(a[3])
    var s = surfaceArg(a[2], b)
    let full = rect(0, 0, b.w, b.h)
    let c = floats(a[9])
    let ok = bucketFill(s, full, (i(a[4]), i(a[5])), f(a[6]), bl(a[7]), bl(a[8]), [c[0], c[1], c[2], c[3]], f(a[10]),
                        maskArg(a[11], full), bl(a[12]))
    echo ok
    dumpSurface(o, s, full)
  of "rt.kernels":
    var v: seq[float32]
    let vals = [-0.2'f32, 0, 0.01, 0.1, 0.25, 0.5, 0.73, 0.9, 1, 1.3, NaN]
    for r in ["shadows", "midtones", "highlights"]:
      for burn in [false, true]:
        for e in [0'f32, 0.1, 0.5, 1, 2]:
          for x in vals: v.add toneCurve(x, e, toneArg(r), burn)
          for c in [[0.2'f32, 0.5, 0.8], [0.9'f32, 0.95, 1], [0'f32, 0, 0], [0.6'f32, 0.1, 0.1], [1.2'f32, -0.1, 0.4]]:
            for protect in [false, true]:
              for x in dodgeBurn(c, e, toneArg(r), burn, protect): v.add x
    for c in [[0.2'f32, 0.5, 0.8], [0.9'f32, 0.95, 1], [0.5'f32, 0.5, 0.5], [0.6'f32, 0.1, 0.1]]:
      for amt in [0'f32, 0.2, 0.7, 1]:
        for (sat, vb) in [(false, false), (true, false), (false, true), (true, true)]:
          for x in sponge(c, amt, sat, vb): v.add x
      v.add saturation(c)
      v.add retouch.luma(c)
    let stops = @[(0'f32, [1'f32, 0, 0, 1]), (0.4'f32, [0'f32, 1, 0, 0.5]), (0.4'f32, [0'f32, 0, 1, 1]), (1'f32, [1'f32, 1, 1, 0])]
    for t in [-1'f32, 0, 0.2, 0.4, 0.41, 0.7, 1, 2]:
      for x in sampleStops(stops, t): v.add x
      for sh in ["linear", "radial", "angle", "reflected", "diamond"]:
        v.add toolGradientT(shapeArg(sh), (3'f32, 4'f32), (40'f32, -9'f32), t * 30'f32, 20'f32 - t * 7'f32)
    writeF32(o, v.len, 1, 1, v)
  of "rt.blur":
    let b = readBuf(a[2])
    let w = rectArg(a[4])
    let r = localBlur(b.f, b.w, b.h, b.ch, optAlpha(a[5]), (w.x0, w.y0, w.x1, w.y1), f(a[3]))
    writeF32(o, w.x1 - w.x0, w.y1 - w.y0, b.ch, r)
  of "rt.sharpen":
    let b = readBuf(a[2])
    let w = rectArg(a[4])
    let r = localSharpen(b.f, b.w, b.h, b.ch, optAlpha(a[5]), (w.x0, w.y0, w.x1, w.y1), f(a[3]), bl(a[6]))
    writeF32(o, w.x1 - w.x0, w.y1 - w.y0, b.ch, r)
  else: return false
  true

# ---------------------------------------------------------------- poisson / nonlocal / remove

proc holesArg(path: string): (int, int, seq[bool]) =
  let b = readBuf(path)
  var m = newSeq[bool](b.f.len)
  for i, v in b.f: m[i] = v >= 0.5'f32
  (b.w, b.h, m)

proc mb(m: seq[bool]): seq[float32] =
  for v in m: result.add (if v: 1'f32 else: 0'f32)

proc fillCmd(a: seq[string]): bool =
  let o = a[^1]
  case a[1]
  of "pf.holes":
    let (w, h, m) = holesArg(a[2])
    echo holeRadius(w, h, m)
    var v = mb(closeLoops(w, h, m))
    v.add mb(dilate(w, h, m, parseInt(a[3])))
    writeF32(o, w, h * 2, 1, v)
  of "pf.membrane":
    let b = readBuf(a[2])
    let (_, _, m) = holesArg(a[3])
    writeF32(o, b.w, b.h, b.ch, membraneFill(b.w, b.h, b.ch, b.f, m))
  of "pf.clone":
    let (s, d) = (readBuf(a[2]), readBuf(a[3]))
    let (_, _, m) = holesArg(a[4])
    writeF32(o, s.w, s.h, s.ch, seamlessClone(s.w, s.h, s.ch, s.f, d.f, m))
  of "pf.reference":
    let b = readBuf(a[2])
    let (_, _, m) = holesArg(a[3])
    var v = newSeq[float32](b.w * b.h)
    for k in 0 ..< v.len: v[k] = b.f[k * b.ch]
    solveMembraneReference(b.w, b.h, m, v, f(a[4]), parseInt(a[5]))
    writeF32(o, b.w, b.h, 1, v)
  of "pf.complete":
    let b = readBuf(a[2])
    let (_, _, m) = holesArg(a[3])
    var p = defaultParams()
    p.seed = uint64(parseBiggestUInt(a[4]))
    p.patchRadius = parseInt(a[5])
    p.lambda = f(a[6])
    p.maxIters = parseInt(a[7])
    p.pmPasses = parseInt(a[8])
    let r = completeWith(b.w, b.h, b.ch, b.f, m, p, NoInterrupt)
    if r.isSome: writeF32(o, b.w, b.h, b.ch, r.get) else: echo "none"
  of "pf.remove":
    let b = readBuf(a[2])
    let (_, _, m) = holesArg(a[3])
    writeF32(o, b.w, b.h, b.ch, removeWith(b.w, b.h, b.ch, b.f, m, uint64(parseBiggestUInt(a[4])), NoInterrupt))
  else: return false
  true

# ---------------------------------------------------------------- perspective / scan crop / red eye

proc nums64(s: string): seq[float64] =
  for t in s.split(','): result.add parseFloat(t.strip)

proc planesArg(s: string): seq[Plane] =
  let v = nums64(s)
  proc q(o: int): Quad = [[v[o], v[o + 1]], [v[o + 2], v[o + 3]], [v[o + 4], v[o + 5]], [v[o + 6], v[o + 7]]]
  for k in 0 ..< v.len div 16: result.add Plane(src: q(k * 16), dst: q(k * 16 + 8))

proc b64(v: float64): string = $cast[uint64](v)

proc quadLine(q: Quad): string =
  var parts: seq[string]
  for p in q: parts.add b64(p[0]) & " " & b64(p[1])
  parts.join(" ")

proc warpCmd(a: seq[string]): bool =
  let o = a[^1]
  case a[1]
  of "pw.map":
    let ps = planesArg(a[2])
    let (x0, y0, x1, y1) = (parseFloat(a[3]), parseFloat(a[4]), parseFloat(a[5]), parseFloat(a[6]))
    let n = parseInt(a[7])
    let m = newPerspectiveMap(ps)
    if m.isNone: echo "none"
    else:
      var line: seq[string]
      for j in 0 .. n:
        for i in 0 .. n:
          let (x, y) = (x0 + (x1 - x0) * float64(i) / float64(n), y0 + (y1 - y0) * float64(j) / float64(n))
          let (u, v) = m.get.map(x, y)
          line.add b64(u)
          line.add b64(v)
      echo line.join(" ")
    echo "identity ", isIdentity(ps)
    for p in ps:
      for (u, v) in [(0.0, 0.0), (0.3, 0.7), (1.0, 0.5), (1.5, -0.2)]:
        for d in [false, true]:
          let q = planePoint(p, u, v, d)
          echo "pp ", b64(q[0]), " ", b64(q[1])
    echo "unit ", quadLine(unitCorners())
    writeFile(o, "")
  of "pw.straighten":
    var ps = planesArg(a[2])
    for g in linkedCorners(ps):
      var parts: seq[string]
      for (p, c) in g: parts.add "(" & $p & ", " & $c & ")"
      echo "group [", parts.join(", "), "]"
    let m = parseStraighten(a[3])
    if m.isNone: echo "bad mode" else: straighten(ps, m.get)
    for p in ps:
      echo "src ", quadLine(p.src)
      echo "dst ", quadLine(p.dst)
    writeFile(o, "")
  of "sc.find":
    let im = readBuf(a[2])
    let found = findPhotos(im.w, im.h, rgba(im))
    echo found.len
    for p in found:
      echo b64(p.center[0]), " ", b64(p.center[1]), " ", b64(p.width), " ", b64(p.height), " ", b64(p.angle), " ", b64(p.area)
      echo "corners ", quadLine(p.corners)
    writeFile(o, "")
  of "re.apply":
    let im = readBuf(a[2])
    var px = rgba(im)
    let n = redeye.apply(px, im.w, im.h, (parseInt(a[3]), parseInt(a[4])), (parseInt(a[5]), parseInt(a[6])), f(a[7]), f(a[8]))
    echo n
    writeF32(o, im.w, im.h, 4, flat4(px))
  of "re.consts":
    echo "halo ", haloRadius()
    for v in [NaN, Inf, -5'f32, 0, 1, 37.5, 50, 100, 101, 400, 401, 1e30]:
      let s = unclampedWindowSide(v)
      echo cast[uint32](searchRadius(v)), " ", (if s.isSome: "Some(" & $s.get & ")" else: "None")
    writeFile(o, "")
  else: return false
  true

# ---------------------------------------------------------------- reshaping tools

proc optMask(path: string): Option[seq[float32]] =
  if path == "-": none(seq[float32]) else: some(readBuf(path).f)

proc seqStr(v: openArray[int]): string =
  var parts: seq[string]
  for x in v: parts.add $x
  "[" & parts.join(", ") & "]"

proc reshapeCmd(a: seq[string]): bool =
  let o = a[^1]
  case a[1]
  of "cs.energy":
    let b = readBuf(a[2])
    let e = energy(b.w, b.h, b.ch, b.f)
    echo seqStr(findSeam(b.w, b.h, e))
    writeF32(o, b.w, b.h, 1, e)
  of "cs.width":
    let b = readBuf(a[2])
    let nw = max(parseInt(a[4]), 1)
    let r = carveWidth(b.w, b.h, b.ch, b.f, optMask(a[3]), nw)
    echo r.len
    writeF32(o, nw, b.h, b.ch, r)
  of "cs.carve":
    let b = readBuf(a[2])
    let (nw, nh) = (max(parseInt(a[4]), 1), max(parseInt(a[5]), 1))
    let r = carve(b.w, b.h, b.ch, b.f, optMask(a[3]), nw, nh)
    echo r.len
    writeF32(o, nw, nh, b.ch, r)
  of "cs.resize":
    let b = readBuf(a[2])
    let (nw, nh) = (parseInt(a[3]), parseInt(a[4]))
    writeF32(o, nw, nh, b.ch, resizeBilinear(b.w, b.h, b.ch, b.f, nw, nh))
  of "cs.skin":
    let b = readBuf(a[2])
    writeF32(o, b.w, b.h, 1, skinMask(b.w, b.h, b.ch, b.f))
  of "r2.run":
    let canvas = rectArg(a[4])
    var prims: seq[Prim]
    case a[2]
    of "flame":
      let spec = flameSpecFromJson(parseJson(a[3]))
      var paths: seq[seq[(float32, float32)]]
      for p in parseJson(a[8]):
        var q: seq[(float32, float32)]
        for pt in p: q.add (float32(pt[0].getFloat), float32(pt[1].getFloat))
        paths.add q
      if paths.len == 0: paths = @[defaultFlamePath(canvas)]
      prims = flame(spec, paths)
    of "tree": prims = tree(treeSpecFromJson(parseJson(a[3])), canvas)
    else: prims = pictureFrame(frameSpecFromJson(parseJson(a[3])), canvas)
    var v: seq[float32]
    for p in prims:
      if p.shape.kind == skCapsule: v.add [0'f32, p.shape.a[0], p.shape.a[1], p.shape.b[0], p.shape.b[1], p.shape.ra, p.shape.rb]
      else: v.add [1'f32, p.shape.c[0], p.shape.c[1], p.shape.rx, p.shape.ry, p.shape.angle, 0'f32]
      v.add p.c0
      v.add p.c1
      v.add p.soft
      v.add float32(ord(p.blend))
      let r = p.bounds
      v.add [float32(r.x0), float32(r.y0), float32(r.x1), float32(r.y1)]
    let im = readBuf(a[5])
    var surf = surfaceArg(a[6], im)
    let full = rect(0, 0, im.w, im.h)
    let used = composite(surf, prims, canvas.intersect(full), surfaceOf(a[7], full))
    echo "prims ", prims.len, " used ", used.x0, " ", used.y0, " ", used.x1, " ", used.y1
    v.add surf.readRegion(full)
    writeF32(o, v.len, 1, 1, v)
    let b = surf.contentBounds
    echo "tiles ", surf.tileCount, " bounds ", b.x0, " ", b.y0, " ", b.x1, " ", b.y1
  of "lq.field", "lq.apply", "lq.proxy", "lq.full":
    let k = (if a[1] == "lq.field": 2 else: 4)
    let bounds = rectArg(a[k])
    let cell = parseFloat(a[k + 1])
    var strokes: seq[LiquifyStroke]
    for j in parseJson(a[k + 2]): strokes.add strokeFromJson(j)
    var field = newLiquifyField(bounds, cell)
    for st in strokes:
      let r = field.applyStroke(st)
      echo "dirty ", r.x0, " ", r.y0, " ", r.x1, " ", r.y1, " ", st.validate().isNone
    echo "field ", field.w, " ", field.h, " ", b64(field.cell), " ", cast[uint32](field.maxDisplacement), " ", field.isIdentity
    if a[1] == "lq.field":
      var line: seq[string]
      for j in 0 .. 12:
        for i in 0 .. 12:
          let (x, y) = (float64(bounds.x0) - 10.0 + float64(bounds.width + 20) * float64(i) / 12.0,
                        float64(bounds.y0) - 10.0 + float64(bounds.height + 20) * float64(j) / 12.0)
          let d = field.sample(x, y)
          line.add b64(d[0]) & " " & b64(d[1]) & " " & $cast[uint32](field.freezeAt(x, y))
      echo line.join(" ")
      var fo: seq[string]
      for t in [-0.5, 0.0, 0.1, 0.5, 0.9, 1.0, 2.0, NaN]:
        for d in [0.0, 0.3, 1.0, NaN, 7.0]: fo.add b64(falloff(t, d))
      echo "falloff ", fo.join(" ")
      var v: seq[float32]
      for q in 0 ..< field.d.len: v.add [field.d[q][0], field.d[q][1], field.freeze[q]]
      writeF32(o, field.w, field.h, 3, v)
    else:
      let im = readBuf(a[2])
      let src = surfaceArg(a[3], im)
      let full = rect(0, 0, im.w, im.h)
      case a[1]
      of "lq.apply": dumpSurface(o, applyLiquify(src, field), full)
      of "lq.proxy":
        let p = newProxyImage(src, bounds, parseInt(a[k + 3]))
        echo "proxy ", p.w, " ", p.h, " ", b64(p.scale), " ", p.bounds.x0
        var o1 = newSeq[array[4, uint8]](p.w * p.h)
        p.render(field, [0, 0, p.w, p.h], o1)
        var o2 = newSeqWith(p.w * p.h, [7'u8, 7, 7, 7])
        let pr = p.proxyRect(rect(bounds.x0 + 5, bounds.y0 + 3, bounds.x0 + 30, bounds.y0 + 20))
        p.render(field, pr, o2)
        echo "rect [", pr[0], ", ", pr[1], ", ", pr[2], ", ", pr[3], "]"
        var v: seq[uint8]
        for q in o1: v.add q
        for q in o2: v.add q
        for q in p.px:
          for x in q:
            let bits = cast[uint32](x)
            for sh in 0 .. 3: v.add uint8((bits shr (8 * sh)) and 0xFF)
        writeU8(o, v.len, 1, 1, v)
      else:
        let region = rectArg(a[k + 3])
        let ob = bounds.intersect(field.bounds)
        var o1 = newSeqWith(ob.width * ob.height, [3'u8, 3, 3, 3])
        var scratch: seq[array[4, float32]]
        echo renderFullResolution(src, field, bounds, region, o1, scratch)
        var v: seq[uint8]
        for q in o1: v.add q
        writeU8(o, v.len, 1, 1, v)
  of "ml.run":
    let im = readBuf(a[2])
    let px = rgba8(im)
    let (ox, oy) = (parseInt(a[3]), parseInt(a[4]))
    let bad = a[5] == "1"
    let (w, h) = (im.w, im.h)
    let fetch: Fetch = proc(r: Rect): seq[array[4, uint8]] =
      if bad: return newSeqWith(3, [255'u8, 255, 255, 255])
      for y in r.y0 ..< r.y1:
        for x in r.x0 ..< r.x1: result.add px[(y - oy) * w + (x - ox)]
    var t = newTracer(rect(ox, oy, ox + w, oy + h))
    proc line(v: openArray[array[2, float64]]): string =
      var parts: seq[string]
      for p in v: parts.add b64(p[0]) & " " & b64(p[1])
      parts.join(" ")
    for op in a[6 ..< a.len - 1]:
      let tk = op.splitWhitespace()
      proc n(k: int): float64 = parseFloat(tk[k])
      proc pts(start: int): seq[array[2, float64]] =
        var k = start
        while k + 1 < tk.len:
          result.add [parseFloat(tk[k]), parseFloat(tk[k + 1])]
          k += 2
      case tk[0]
      of "s":
        let r = t.snap(fetch, [n(1), n(2)], Settings(width: n(3), contrast: float32(n(4))))
        if r.isSome: echo "snap ", b64(r.get[0]), " ", b64(r.get[1]) else: echo "snap none"
      of "t":
        let r = t.trace(fetch, [n(1), n(2)], [n(3), n(4)], pts(7), Settings(width: n(5), contrast: float32(n(6))))
        echo "trace ", r.len, " ", line(r)
      of "simplify": echo "simplify ", line(simplify(pts(2), n(1)))
      of "len": echo "len ", b64(magnetic.length(pts(1)))
      else: quit("bad op")
    echo "cached ", t.cachedPixels
    writeFile(o, "")
  else: return false
  true

# ---------------------------------------------------------------- photo merge

proc lumaOf(b: Buf): seq[float32] =
  for i in 0 ..< b.f.len div b.ch:
    result.add (if b.ch >= 3: 0.2126'f32 * b.f[i * b.ch] + 0.7152'f32 * b.f[i * b.ch + 1] + 0.0722'f32 * b.f[i * b.ch + 2] else: b.f[i * b.ch])

proc alphaOf(b: Buf): seq[float32] =
  for i in 0 ..< b.f.len div b.ch:
    result.add (if b.ch == 4 or b.ch == 2: b.f[i * b.ch + b.ch - 1] else: 1'f32)

proc photoCmd(a: seq[string]): bool =
  let o = a[^1]
  case a[1]
  of "lc.run":
    let im = readBuf(a[2])
    let src = surfaceArg(a[3], im)
    let frame = rectArg(a[4])
    let lc = lensCorrectionFromJson(parseJson(a[5]))
    let m = newLensMap(lc, frame)
    var line: seq[string]
    for j in 0 .. 8:
      for i in 0 .. 8:
        let (x, y) = (float64(frame.x0) - 5.0 + float64(frame.width + 10) * float64(i) / 8.0,
                      float64(frame.y0) - 5.0 + float64(frame.height + 10) * float64(j) / 8.0)
        let s = m.sample(x, y)
        if s.isSome:
          let q = s.get
          line.add b64(q.p[0]) & " " & b64(q.p[1]) & " " & b64(q.pr[0]) & " " & b64(q.pr[1]) & " " & b64(q.pb[0]) & " " &
                   b64(q.pb[1]) & " " & $cast[uint32](q.gain)
        else: line.add "none"
    echo line.join(" ")
    echo "auto ", b64(autoScale(lc, frame))
    let full = rect(0, 0, im.w, im.h)
    let os = correct(src, frame, lc)
    echo "format PixelFormat { mode: ", os.format.mode, ", sample: ", os.format.sample, ", alpha: ", os.format.alpha, " }"
    dumpSurface(o, os, full)
  of "lc.remapall":
    let bounds = rect(-13, -9, 39, 26)
    var v: seq[float32]
    for mode in [cmRgb, cmGrayscale, cmCmyk, cmLab, cmBitmap, cmDuotone, cmIndexed, cmMultichannel]:
      for sample in [stU8, stU16, stF32]:
        for alpha in [false, true]:
          let fmt = pixelFormat(mode, sample, alpha)
          let n = fmt.channels
          var s = newSurface(fmt)
          var data = newSeq[float32](bounds.width * bounds.height * n)
          for i in 0 ..< data.len: data[i] = (if i mod 17 == 0: 0'f32 else: float32((i * 103 + i div n * 71) mod 65536) / 65535'f32)
          s.writeRegion(bounds, data)
          for edge in [edgeTransparency(), edgeExtension(), edgeColor([0.2'f32, 0.5, 0.7, 0.4])]:
            for ca in [false, true]:
              let map = proc(x, y: float64): Option[Sample] =
                if (asI32(x) + asI32(y)) mod 13 == 0: return none(Sample)
                let p = [x * 0.83 - 3.7, y * 1.14 + 1.3]
                some(Sample(p: p, pr: (if ca: [p[0] - 0.9, p[1] + 0.3] else: p), pb: (if ca: [p[0] + 0.7, p[1] - 0.4] else: p), gain: 1.17'f32))
              let o2 = bounds.inflate(3)
              let r = remap(s, bounds, o2, edge, map)
              echo "PixelFormat { mode: ", r.format.mode, ", sample: ", r.format.sample, ", alpha: ", r.format.alpha, " } ", r.tileCount, " ", r.channels
              v.add r.readRegion(o2)
    writeF32(o, v.len, 1, 1, v)
  of "vp.run":
    let im = readBuf(a[2])
    var surf = surfaceArg(a[3], im)
    let full = rect(0, 0, im.w, im.h)
    let q = nums64(a[4])
    let plane = VpPlane(corners: [[q[0], q[1]], [q[2], q[3]], [q[4], q[5]], [q[6], q[7]]])
    let c = [parseFloat(a[5]), parseFloat(a[6])]
    let focal = (if a[7] == "-": none(float64) else: some(parseFloat(a[7])))
    proc optB(v: Option[float64]): string = (if v.isSome: "Some(\"" & b64(v.get) & "\")" else: "None")
    proc pcName(p: PlaneCheck): string = (case p
      of pcValid: "Valid"
      of pcQuestionable: "Questionable"
      of pcInvalid: "Invalid")
    echo "check ", pcName(plane.check(c)), " focal ", optB(plane.focal(c)), " norm ", quadLine(plane.normalized.corners)
    var scene = newScene(plane, c, focal, parseFloat(a[8]))
    echo "scene focal ", b64(scene.focal)
    var v: seq[float32]
    proc optI(v: Option[int]): string = (if v.isSome: "Some(" & $v.get & ")" else: "None")
    for op in a[9 ..< a.len - 1]:
      let t = op.splitWhitespace()
      proc f(k: int): float64 = parseFloat(t[k])
      case t[0]
      of "plane":
        let i = parseInt(t[1])
        let l = scene.lifted(i)
        var pts: seq[string]
        for p in l.pts: pts.add b64(p[0]) & " " & b64(p[1]) & " " & b64(p[2])
        let (lu, lv) = l.lengths
        echo "plane ", quadLine(scene.planes[i].corners), " ", pts.join(" "), " lengths ", b64(lu), " ", b64(lv), " check ",
             pcName(scene.planes[i].check(c))
      of "tear":
        let edge = (case t[2]
          of "top": edTop
          of "right": edRight
          of "bottom": edBottom
          else: edLeft)
        echo "tear ", optI(scene.tearOff(parseInt(t[1]), edge, f(3), f(4)))
      of "at": echo "at ", optI(scene.planeAt([f(1), f(2)]))
      of "paste":
        let r = rect(asI32(f(2)), asI32(f(3)), asI32(f(4)), asI32(f(5)))
        let o2 = paste(scene, parseInt(t[1]), surf, r, [f(6), f(7)], f(8))
        if o2.isSome:
          let cb = o2.get.contentBounds
          let fm = o2.get.format
          echo "paste ", cb.x0, " ", cb.y0, " ", cb.x1, " ", cb.y1, " PixelFormat { mode: ", fm.mode, ", sample: ", fm.sample,
               ", alpha: ", fm.alpha, " }"
          v.add o2.get.readRegion(cb)
        else: echo "paste none"
      else:
        var pts: seq[array[2, float64]]
        var k = 6
        while k + 1 < t.len:
          pts.add [parseFloat(t[k]), parseFloat(t[k + 1])]
          k += 2
        echo "clone ", cloneStroke(scene, surf, [f(1), f(2)], pts, f(3), f(4), float32(f(5)))
    v.add surf.readRegion(full)
    writeF32(o, v.len, 1, 1, v)
  of "lc.misc":
    for f in [-5.0, 0.0, 12.0, 14.0, 17.5, 18.5, 24.0, 35.0, 49.9, 85.0, 120.0, 200.0, 999.0, NaN, Inf]:
      let p = genericProfile(f)
      echo p.name, " ", b64(p.k1), " ", b64(p.k2), " ", b64(p.p1), " ", b64(p.p2), " ", b64(p.vignette[0]), " ",
           b64(p.vignette[1]), " ", b64(p.vignette[2]), " ", b64(p.caRed), " ", b64(p.caBlue)
    for (ax, ay, bx, by) in [(0.0, 0.0, 10.0, 1.0), (0.0, 0.0, 1.0, 10.0), (5.0, 5.0, -3.0, 4.0), (0.0, 0.0, 0.0, 0.0),
                             (0.0, 0.0, -10.0, -10.0), (1.0, 2.0, 1.0, NaN)]:
      echo "straighten ", b64(straightenAngle([ax, ay], [bx, by]))
    writeFile(o, "")
  of "py.lap":
    let b = readBuf(a[2])
    let pyr = laplacian(b.w, b.h, b.ch, b.f, parseInt(a[3]))
    let g = gaussian(b.w, b.h, b.ch, b.f, parseInt(a[3]))
    var v: seq[float32]
    for l in pyr & g:
      echo "level ", l.w, " ", l.h
      v.add l.px
    v.add collapse(pyr, b.ch)
    echo "auto ", autoLevels(b.w, b.h), " stack ", stackLevels(b.w, b.h)
    writeF32(o, v.len, 1, 1, v)
  of "py.blend", "py.fuse", "py.stackw", "py.panow":
    var imgs: seq[Buf]
    case a[1]
    of "py.blend":
      var k = 3
      while k < a.len - 1:
        imgs.add readBuf(a[k])
        k += 2
    of "py.fuse":
      for p in a[6 ..< a.len - 1]: imgs.add readBuf(p)
    else:
      for p in a[2 ..< a.len - 1]: imgs.add readBuf(p)
    let (w, h, ch) = (imgs[0].w, imgs[0].h, imgs[0].ch)
    var px: seq[seq[float32]]
    for b in imgs: px.add b.f
    var r: seq[float32]
    case a[1]
    of "py.blend":
      var wts: seq[seq[float32]]
      var k = 4
      while k < a.len - 1:
        wts.add readBuf(a[k]).f
        k += 2
      r = blend(w, h, ch, px, wts, parseInt(a[2]))
    of "py.fuse":
      let p = StackFusion(energyRadius: parseInt(a[3]), halo: f(a[4]), guide: parseInt(a[5]),
                          levels: (if a[2] == "-": none(int) else: some(parseInt(a[2]))))
      var l, al: seq[seq[float32]]
      for b in imgs:
        l.add lumaOf(b)
        al.add alphaOf(b)
      r = fuseStack(w, h, ch, px, l, al, p)
    of "py.stackw":
      var l, al: seq[seq[float32]]
      for b in imgs:
        l.add lumaOf(b)
        al.add alphaOf(b)
      for s in stackWeights(w, h, l, al): r.add s
    else:
      var al: seq[seq[float32]]
      for b in imgs: al.add alphaOf(b)
      for s in panoramaWeights(w, h, al): r.add s
    writeF32(o, r.len, 1, 1, r)
  else: return false
  true

when isMainModule:
  let a = @[getAppFilename()] & commandLineParams()
  if a.len < 2: quit("usage: algocheck gen <dir> | <command> <args…> <out>")
  if a[1] == "gen": gen(a[2])
  elif not (selectionCmd(a) or toneCmd(a) or hdrCmd(a) or exifCmd(a) or quantizeCmd(a) or retouchCmd(a) or fillCmd(a) or warpCmd(a) or reshapeCmd(a) or photoCmd(a)): quit("unknown command " & a[1])
