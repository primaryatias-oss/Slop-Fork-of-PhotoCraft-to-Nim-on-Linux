## Nim side of the non-filter algo cross-check (see `tools/algocheck.py` and PORTING.md): the
## same commands as the Rust oracle's `ax_*.rs` modules, reading and writing the same raw buffer
## files (`PCIM`, u32 w, h, ch, kind (0 = f32, 1 = u8), then little-endian samples).
##
##   algocheck gen <dir>                 writes the shared input buffers
##   algocheck <command> <args…> <out>   runs one command (as `photocraft-oracle <command> …`)

import std/[os, strutils, options, math, tables]
import photocraft/[geom, raster]
import photocraft/algo/[selection, util, histogram, tone, hdr, exif]

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

when isMainModule:
  let a = @[getAppFilename()] & commandLineParams()
  if a.len < 2: quit("usage: algocheck gen <dir> | <command> <args…> <out>")
  if a[1] == "gen": gen(a[2])
  elif not (selectionCmd(a) or toneCmd(a) or hdrCmd(a) or exifCmd(a)): quit("unknown command " & a[1])
