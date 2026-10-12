## ICC profiles: the parsed model, the reader (ICC.1:2001-04 v2 and ICC.1:2010 v4) and the
## connection of a profile to the PCS as evaluation stages.

import std/[options, strutils, unicode]
import ./curve, ./clut, ./math, ./pipeline

type
  Intent* = enum
    ## ICC rendering intent.
    Perceptual = 0, RelativeColorimetric = 1, Saturation = 2, AbsoluteColorimetric = 3

  CmsErrorKind* = enum ceTruncated, ceBadSignature, ceInvalid, ceUnsupported
  CmsError* = object of CatchableError
    kind*: CmsErrorKind

  ProfileClassKind* = enum
    pcInput, pcDisplay, pcOutput, pcDeviceLink, pcColorSpace, pcAbstract, pcNamedColor, pcUnknown
  ProfileClass* = object
    kind*: ProfileClassKind
    raw*: uint32

  ColorSpaceKind* = enum
    csXyz, csLab, csLuv, csYCbCr, csYxy, csRgb, csGray, csHsv, csHls, csCmyk, csCmy, csColor, csUnknown
  ColorSpace* = object
    kind*: ColorSpaceKind
    n*: int        ## colorant count for `csColor`
    raw*: uint32   ## signature for `csUnknown`

  Pcs* = enum pcsXyz, pcsLab

  LutKind* = enum lkLut8, lkLut16, lkAb

  Lut* = object
    ## A parsed `AToB`/`BToA` tag: device- and PCS-side values normalised to `[0, 1]`.
    kind*: LutKind
    inputs*, outputs*: int
    stages*: seq[Stage]

  Profile* = ref object
    ## A parsed ICC profile.
    version*: (uint8, uint8)
    class*: ProfileClass
    colorSpace*: ColorSpace
    pcs*: Pcs
    renderingIntent*: Intent
    description*: string
    copyright*: string
    whitePoint*: Vec3
    blackPoint*: Option[Vec3]
    chad*: Option[Mat3]
    matrix*: Option[Mat3]          ## colorant matrix (columns rXYZ, gXYZ, bXYZ), D50-adapted
    trc*: Option[array[3, Curve]]
    grayTrc*: Option[Curve]
    a2b*: array[3, Option[Lut]]    ## A2B0/A2B1/A2B2
    b2a*: array[3, Option[Lut]]
    bytes*: Option[seq[byte]]      ## the bytes this profile was parsed from (or encoded to)
    hashCache*: Option[uint64]

const SameColorsMaxDeltaE* = 0.5

proc newCmsError*(kind: CmsErrorKind; msg = ""): ref CmsError =
  let m = case kind
    of ceTruncated: "ICC profile is truncated"
    of ceBadSignature: "not an ICC profile (missing 'acsp' signature)"
    of ceInvalid: "invalid ICC profile: " & msg
    of ceUnsupported: "unsupported ICC feature: " & msg
  result = newException(CmsError, m)
  result.kind = kind

proc intentFromU32*(v: uint32): Option[Intent] =
  if v <= 3: some(Intent(v)) else: none(Intent)

proc parseIntent*(s: string): Option[Intent] =
  ## Parses `perceptual`, `relative`, `saturation`, `absolute` (case-insensitive, aliases).
  var n = ""
  for c in s.toLowerAscii:
    if c.isAlphaNumeric: n.add c
  case n
  of "perceptual", "0": some(Perceptual)
  of "relative", "relativecolorimetric", "colorimetric", "1": some(RelativeColorimetric)
  of "saturation", "2": some(Saturation)
  of "absolute", "absolutecolorimetric", "3": some(AbsoluteColorimetric)
  else: none(Intent)

proc id*(i: Intent): string =
  case i
  of Perceptual: "perceptual"
  of RelativeColorimetric: "relative"
  of Saturation: "saturation"
  of AbsoluteColorimetric: "absolute"

proc sig4*(s: string): uint32 =
  ## Big-endian u32 of a 4-character signature.
  for i in 0..3: result = (result shl 8) or uint32(ord(s[i]))

proc sigStr*(v: uint32): string =
  for i in countdown(3, 0): result.add char((v shr (8 * i)) and 0xFF)

const ClassSigs: array[pcInput..pcNamedColor, string] = ["scnr", "mntr", "prtr", "link", "spac", "abst", "nmcl"]

proc profileClass*(k: ProfileClassKind): ProfileClass = ProfileClass(kind: k)
proc classFromSig*(s: uint32): ProfileClass =
  let str = sigStr(s)
  for k in pcInput..pcNamedColor:
    if ClassSigs[k] == str: return ProfileClass(kind: k)
  ProfileClass(kind: pcUnknown, raw: s)
proc sig*(c: ProfileClass): uint32 =
  if c.kind == pcUnknown: c.raw else: sig4(ClassSigs[c.kind])
proc `==`*(a, b: ProfileClass): bool = a.kind == b.kind and (a.kind != pcUnknown or a.raw == b.raw)
proc `$`*(c: ProfileClass): string = ($c.kind)[2..^1]

const SpaceSigs: array[csXyz..csCmy, string] = ["XYZ ", "Lab ", "Luv ", "YCbr", "Yxy ", "RGB ", "GRAY", "HSV ", "HLS ", "CMYK", "CMY "]

proc colorSpace*(k: ColorSpaceKind): ColorSpace = ColorSpace(kind: k)
proc spaceFromSig*(s: uint32): ColorSpace =
  let str = sigStr(s)
  for k in csXyz..csCmy:
    if SpaceSigs[k] == str: return ColorSpace(kind: k)
  if str[1..3] == "CLR":
    let d = str[0]
    if d in {'2'..'9'}: return ColorSpace(kind: csColor, n: ord(d) - ord('0'))
    if d in {'A'..'F'}: return ColorSpace(kind: csColor, n: ord(d) - ord('A') + 10)
  ColorSpace(kind: csUnknown, raw: s)
proc sig*(c: ColorSpace): uint32 =
  case c.kind
  of csXyz..csCmy: sig4(SpaceSigs[c.kind])
  of csColor: sig4((if c.n < 10: $char(ord('0') + c.n) else: $char(ord('A') + c.n - 10)) & "CLR")
  of csUnknown: c.raw
proc `==`*(a, b: ColorSpace): bool =
  a.kind == b.kind and (a.kind != csColor or a.n == b.n) and (a.kind != csUnknown or a.raw == b.raw)
proc `$`*(c: ColorSpace): string = ($c.kind)[2..^1]
proc channels*(c: ColorSpace): int =
  ## Number of channels of the space.
  case c.kind
  of csGray: 1
  of csCmyk: 4
  of csColor: c.n
  of csUnknown: 0
  else: 3

proc channels*(p: Profile): int = p.colorSpace.channels

proc mediaWhite*(p: Profile): Vec3 =
  ## Media white for ICC-absolute colorimetry (`wtpt`, or D50 for v2 display profiles).
  if p.version[0] < 4 and p.class.kind == pcDisplay: return D50
  p.whitePoint

proc copyProfile*(p: Profile): Profile =
  ## A field-by-field copy (the ref is new).
  result = Profile()
  result[] = p[]

proc grayAsRgb*(p: Profile): Option[Profile] =
  ## An RGB view of a gray TRC profile: `(v, v, v)` has exactly the colour of gray `v`.
  if p.grayTrc.isNone or p.pcs != pcsXyz: return none(Profile)
  let c = p.grayTrc.get
  let w = D50
  let m: Mat3 = [[w[0] / 3.0, w[0] / 3.0, w[0] / 3.0], [w[1] / 3.0, w[1] / 3.0, w[1] / 3.0], [w[2] / 3.0, w[2] / 3.0, w[2] / 3.0]]
  let q = copyProfile(p)
  q.colorSpace = colorSpace(csRgb)
  q.description = p.description & " (as RGB)"
  q.matrix = some(m)
  q.trc = some([c, c, c])
  q.grayTrc = none(Curve)
  q.a2b = [none(Lut), none(Lut), none(Lut)]
  q.b2a = [none(Lut), none(Lut), none(Lut)]
  q.bytes = none(seq[byte])
  q.hashCache = none(uint64)
  some(q)

proc isMatrixShaper*(p: Profile): bool =
  ## `true` for matrix/TRC RGB or gray TRC profiles without LUTs.
  for l in p.a2b:
    if l.isSome: return false
  (p.matrix.isSome and p.trc.isSome) or p.grayTrc.isSome

proc lutIndex(intent: Intent): int =
  case intent
  of Perceptual: 0
  of RelativeColorimetric, AbsoluteColorimetric: 1
  of Saturation: 2

proc lutFor(luts: array[3, Option[Lut]]; intent: Intent): Option[Lut] =
  let i = lutIndex(intent)
  if luts[i].isSome: luts[i] else: luts[0]

proc supportsIntent*(p: Profile; intent: Intent; input: bool): bool =
  ## Whether the profile has a dedicated table for `intent` (matrix profiles support all).
  let luts = if input: p.a2b else: p.b2a
  if luts[lutIndex(intent)].isSome: return true
  for l in luts:
    if l.isSome: return false
  true

proc decodePcs(pcs: Pcs; kind: LutKind): Stage =
  if pcs == pcsXyz: simpleStage(skDecodeXyz)
  elif kind == lkLut16: simpleStage(skDecodeLabV2)
  else: simpleStage(skDecodeLabV4)

proc encodePcs(pcs: Pcs; kind: LutKind): Stage =
  if pcs == pcsXyz: simpleStage(skEncodeXyz)
  elif kind == lkLut16: simpleStage(skEncodeLabV2)
  else: simpleStage(skEncodeLabV4)

proc deviceToPcs*(p: Profile; intent: Intent): (seq[Stage], Pcs) =
  ## Stages mapping normalised device values to the float PCS. Raises `CmsError`.
  let l = lutFor(p.a2b, intent)
  if l.isSome:
    var st = l.get.stages
    st.add decodePcs(p.pcs, l.get.kind)
    return (st, p.pcs)
  case p.colorSpace.kind
  of csRgb:
    if p.matrix.isNone or p.trc.isNone:
      raise newCmsError(ceUnsupported, "RGB profile without matrix/TRC or AToB tags")
    let t = p.trc.get
    (@[curvesStage(@[t[0], t[1], t[2]]), matrix3(p.matrix.get)], pcsXyz)
  of csGray:
    if p.grayTrc.isNone: raise newCmsError(ceUnsupported, "gray profile without kTRC")
    let c = p.grayTrc.get
    if p.pcs == pcsLab:
      (@[curvesStage(@[c]), matrixStage(3, 1, @[100.0, 0.0, 0.0], @[0.0, 0.0, 0.0])], pcsLab)
    else:
      (@[curvesStage(@[c]), matrixStage(3, 1, @[D50[0], D50[1], D50[2]], @[0.0, 0.0, 0.0])], pcsXyz)
  of csLab: (@[simpleStage(skDecodeLabV4)], pcsLab)
  of csXyz: (@[simpleStage(skDecodeXyz)], pcsXyz)
  else: raise newCmsError(ceUnsupported, $p.colorSpace & " profile without AToB tags")

proc pcsToDevice*(p: Profile; intent: Intent): (seq[Stage], Pcs) =
  ## Stages mapping the float PCS to normalised device values. Raises `CmsError`.
  let l = lutFor(p.b2a, intent)
  if l.isSome:
    var st = @[encodePcs(p.pcs, l.get.kind)]
    st.add l.get.stages
    return (st, p.pcs)
  case p.colorSpace.kind
  of csRgb:
    if p.matrix.isNone or p.trc.isNone:
      raise newCmsError(ceUnsupported, "RGB profile without matrix/TRC or BToA tags")
    let inv = invert(p.matrix.get)
    if inv.isNone: raise newCmsError(ceInvalid, "singular colorant matrix")
    let t = p.trc.get
    (@[matrix3(inv.get), invCurvesStage(@[t[0], t[1], t[2]])], pcsXyz)
  of csGray:
    if p.grayTrc.isNone: raise newCmsError(ceUnsupported, "gray profile without kTRC")
    let c = p.grayTrc.get
    if p.pcs == pcsLab:
      (@[matrixStage(1, 3, @[0.01, 0.0, 0.0], @[0.0]), invCurvesStage(@[c])], pcsLab)
    else:
      (@[matrixStage(1, 3, @[0.0, 1.0, 0.0], @[0.0]), invCurvesStage(@[c])], pcsXyz)
  of csLab: (@[simpleStage(skEncodeLabV4)], pcsLab)
  of csXyz: (@[simpleStage(skEncodeXyz)], pcsXyz)
  else: raise newCmsError(ceUnsupported, $p.colorSpace & " profile without BToA tags")

# ---------------------------------------------------------------- reader

type Reader = object
  b: ptr UncheckedArray[byte]
  len: int

proc reader(b: openArray[byte]): Reader =
  Reader(b: if b.len > 0: cast[ptr UncheckedArray[byte]](unsafeAddr b[0]) else: nil, len: b.len)

proc check(r: Reader; off, len: int) =
  if off < 0 or len < 0 or off > r.len or len > r.len - off: raise newCmsError(ceTruncated)

proc slice(r: Reader; off, len: int): seq[byte] =
  r.check(off, len)
  result = newSeq[byte](len)
  for i in 0 ..< len: result[i] = r.b[off + i]

proc sub(r: Reader; off, len: int): Reader =
  r.check(off, len)
  Reader(b: cast[ptr UncheckedArray[byte]](addr r.b[off]), len: len)

proc u8(r: Reader; off: int): int =
  r.check(off, 1)
  int(r.b[off])

proc u16(r: Reader; off: int): int =
  r.check(off, 2)
  (int(r.b[off]) shl 8) or int(r.b[off + 1])

proc u32(r: Reader; off: int): uint32 =
  r.check(off, 4)
  (uint32(r.b[off]) shl 24) or (uint32(r.b[off + 1]) shl 16) or (uint32(r.b[off + 2]) shl 8) or uint32(r.b[off + 3])

proc s15f16(r: Reader; off: int): float64 = float64(cast[int32](r.u32(off))) / 65536.0
proc xyz(r: Reader; off: int): Vec3 = [r.s15f16(off), r.s15f16(off + 4), r.s15f16(off + 8)]
proc str4(r: Reader; off: int): string =
  r.check(off, 4)
  for i in 0..3: result.add char(r.b[off + i])

const MaxClutValues = 16 * 1024 * 1024

proc parseCurve(r: Reader): (Curve, int) =
  ## Parses a `curv` or `para` element; returns the curve and its byte length (unpadded).
  let ty = r.str4(0)
  case ty
  of "curv":
    let n = int(r.u32(8))
    let len = 12 + n * 2
    case n
    of 0: (identityCurve(), len)
    of 1: (gammaCurve(float64(r.u16(12)) / 256.0), len)
    else:
      if n > (1 shl 20): raise newCmsError(ceInvalid, "curve too long")
      r.check(12, n * 2)
      var t = newSeq[float32](n)
      for i in 0 ..< n: t[i] = float32(r.u16(12 + 2 * i)) / 65535.0'f32
      (tableCurve(t), len)
  of "para":
    let kind = r.u16(8)
    if kind > 4: raise newCmsError(ceUnsupported, "parametric curve type " & $kind)
    let n = paramCount(kind)
    var p: array[7, float64]
    for i in 0 ..< n: p[i] = r.s15f16(12 + i * 4)
    let len = 12 + n * 4
    let c = if kind == 0: gammaCurve(p[0]) else: paraCurve(kind, p)
    (c, len)
  else: raise newCmsError(ceUnsupported, "curve type " & ty.escape)

proc parseCurvesAt(r: Reader; off, n: int): seq[Curve] =
  var o = off
  for _ in 0 ..< n:
    if o > r.len: raise newCmsError(ceTruncated)
    let (c, len) = parseCurve(r.sub(o, r.len - o))
    result.add c
    o += (len + 3) and not 3

proc parseLut(r: Reader; a2b: bool): Lut =
  let ty = r.str4(0)
  case ty
  of "mft1", "mft2":
    let wide = ty == "mft2"
    let i = r.u8(8)
    let o = r.u8(9)
    let g = r.u8(10)
    if i < 1 or i > 15 or o < 1 or o > 15 or g == 0: raise newCmsError(ceInvalid, "bad lut dimensions")
    var m: Mat3
    for a in 0..2:
      for b in 0..2: m[a][b] = r.s15f16(12 + (a * 3 + b) * 4)
    var nIn, nOut, off: int
    if wide: (nIn, nOut, off) = (r.u16(48), r.u16(50), 52)
    else: (nIn, nOut, off) = (256, 256, 48)
    if wide and (nIn < 2 or nIn > 4096 or nOut < 2 or nOut > 4096):
      raise newCmsError(ceInvalid, "bad lut16 table size")
    let bps = if wide: 2 else: 1
    proc readVals(off, count: int): seq[float32] =
      r.check(off, count * bps)
      result = newSeq[float32](count)
      for k in 0 ..< count:
        result[k] = if wide: float32(r.u16(off + 2 * k)) / 65535.0'f32 else: float32(r.u8(off + k)) / 255.0'f32
    var inCurves: seq[Curve]
    for _ in 0 ..< i:
      inCurves.add tableCurve(readVals(off, nIn))
      off += nIn * bps
    var nodes = 1'u64
    for _ in 0 ..< i:
      nodes = nodes * uint64(g)
      if nodes > uint64(MaxClutValues): raise newCmsError(ceInvalid, "CLUT too large")
    if nodes * uint64(o) > uint64(MaxClutValues): raise newCmsError(ceInvalid, "CLUT too large")
    let total = int(nodes) * o
    let data = readVals(off, total)
    off += total * bps
    var outCurves: seq[Curve]
    for _ in 0 ..< o:
      outCurves.add tableCurve(readVals(off, nOut))
      off += nOut * bps
    var stages: seq[Stage]
    var ident = true
    for a in 0..2:
      for b in 0..2:
        if abs(m[a][b] - (if a == b: 1.0 else: 0.0)) >= 1e-6: ident = false
    if not a2b and i == 3 and not ident: stages.add matrix3(m)
    stages.add curvesStage(inCurves)
    var grid = newSeq[int](i)
    for k in 0 ..< i: grid[k] = g
    stages.add clutStage(newClut(grid, o, data))
    stages.add curvesStage(outCurves)
    Lut(kind: (if wide: lkLut16 else: lkLut8), inputs: i, outputs: o, stages: stages)
  of "mAB ", "mBA ":
    let i = r.u8(8)
    let o = r.u8(9)
    if i < 1 or i > 15 or o < 1 or o > 15: raise newCmsError(ceInvalid, "bad lutAB dimensions")
    let ob = int(r.u32(12))
    let om = int(r.u32(16))
    let omc = int(r.u32(20))
    let oc = int(r.u32(24))
    let oa = int(r.u32(28))
    proc matrixAt(off: int): Stage =
      var m = newSeq[float64](9)
      for k in 0 ..< 9: m[k] = r.s15f16(off + k * 4)
      matrixStage(3, 3, m, @[r.s15f16(off + 36), r.s15f16(off + 40), r.s15f16(off + 44)])
    proc clutAt(off, cin, cout: int): Stage =
      var grid: seq[int]
      var nodes = 1'u64
      for k in 0 ..< cin:
        let g = r.u8(off + k)
        if g < 1: raise newCmsError(ceInvalid, "zero grid points")
        grid.add g
        nodes *= uint64(g)
      let prec = r.u8(off + 16)
      if nodes * uint64(cout) > uint64(MaxClutValues): raise newCmsError(ceInvalid, "CLUT too large")
      let total = int(nodes) * cout
      var data = newSeq[float32](total)
      case prec
      of 1:
        r.check(off + 20, total)
        for k in 0 ..< total: data[k] = float32(r.u8(off + 20 + k)) / 255.0'f32
      of 2:
        r.check(off + 20, total * 2)
        for k in 0 ..< total: data[k] = float32(r.u16(off + 20 + 2 * k)) / 65535.0'f32
      else: raise newCmsError(ceInvalid, "CLUT precision " & $prec)
      clutStage(newClut(grid, cout, data))
    var stages: seq[Stage]
    if ty == "mAB ":
      if oc != 0:
        if oa == 0: raise newCmsError(ceInvalid, "lutAtoB with CLUT but no A curves")
        stages.add curvesStage(parseCurvesAt(r, oa, i))
        stages.add clutAt(oc, i, o)
      elif i != o: raise newCmsError(ceInvalid, "lutAtoB without CLUT changes channel count")
      if om != 0:
        stages.add curvesStage(parseCurvesAt(r, omc, o))
        stages.add matrixAt(om)
      if ob == 0: raise newCmsError(ceInvalid, "lutAtoB without B curves")
      stages.add curvesStage(parseCurvesAt(r, ob, o))
    else:
      if ob == 0: raise newCmsError(ceInvalid, "lutBtoA without B curves")
      stages.add curvesStage(parseCurvesAt(r, ob, i))
      if om != 0:
        stages.add matrixAt(om)
        stages.add curvesStage(parseCurvesAt(r, omc, i))
      if oc != 0:
        if oa == 0: raise newCmsError(ceInvalid, "lutBtoA with CLUT but no A curves")
        stages.add clutAt(oc, i, o)
        stages.add curvesStage(parseCurvesAt(r, oa, o))
      elif i != o: raise newCmsError(ceInvalid, "lutBtoA without CLUT changes channel count")
    Lut(kind: lkAb, inputs: i, outputs: o, stages: stages)
  else: raise newCmsError(ceUnsupported, "LUT type " & ty.escape)

proc asciiZ(s: seq[byte]): string =
  for b in s:
    if b == 0: break
    result.add char(b)
  if result.validateUtf8 != -1:
    var clean = ""
    for c in result: clean.add (if ord(c) < 128: c else: '?')
    result = clean
  result = strutils.strip(result)

proc parseText(r: Reader): string =
  ## Text from `desc` (textDescriptionType), `mluc` (English or first record) or `text`.
  try:
    let ty = r.str4(0)
    case ty
    of "desc":
      let n = int(r.u32(8))
      asciiZ(r.slice(12, n))
    of "text": asciiZ(r.slice(8, r.len - 8))
    of "mluc":
      let n = int(r.u32(8))
      let size = int(r.u32(12))
      var best = (-1, 0)
      for k in 0 ..< min(n, 256):
        let o = 16 + k * size
        var lang, off, len: int
        try:
          lang = r.u16(o)
          len = int(r.u32(o + 4))
          off = int(r.u32(o + 8))
        except CmsError: break
        if best[0] < 0 or lang == 0x656E:
          best = (off, len)
          if lang == 0x656E: break
      if best[0] < 0: return ""
      let s = r.slice(best[0], best[1])
      var units: seq[int]
      for k in 0 ..< s.len div 2: units.add (int(s[2 * k]) shl 8) or int(s[2 * k + 1])
      var o = ""
      var k = 0
      while k < units.len:
        var cp = units[k]
        if cp >= 0xD800 and cp < 0xDC00 and k + 1 < units.len and units[k + 1] >= 0xDC00 and units[k + 1] < 0xE000:
          cp = 0x10000 + ((cp - 0xD800) shl 10) + (units[k + 1] - 0xDC00)
          inc k
        elif cp >= 0xD800 and cp < 0xE000: cp = 0xFFFD
        o.add Rune(cp).toUTF8
        inc k
      strutils.strip(o, chars = {'\0'} + Whitespace)
    else: ""
  except CmsError: ""

proc parseProfileModel*(bytes: openArray[byte]): Profile =
  ## Parses ICC profile bytes into the model (without keeping the bytes). Raises `CmsError`.
  if bytes.len < 132: raise newCmsError(ceTruncated)
  let r0 = reader(bytes)
  if r0.str4(36) != "acsp": raise newCmsError(ceBadSignature)
  let declared = int(r0.u32(0))
  let endPos = if declared >= 132 and declared <= bytes.len: declared else: bytes.len
  let r = r0.sub(0, endPos)
  let version = (uint8(r.u8(8)), uint8(r.u8(9)))
  let class = classFromSig(r.u32(12))
  let cs = spaceFromSig(r.u32(16))
  let pcsSig = r.u32(20)
  let pcs = if sigStr(pcsSig) == "Lab ": pcsLab else: pcsXyz
  let intent = intentFromU32(r.u32(64) and 0xFFFF).get(Perceptual)
  let count = int(r.u32(128))
  if count > 1024: raise newCmsError(ceInvalid, $count & " tags")
  var tags: seq[(string, int, int)]
  for i in 0 ..< count:
    let o = 132 + i * 12
    tags.add (r.str4(o), int(r.u32(o + 4)), int(r.u32(o + 8)))
  proc tagData(s: string): Option[Reader] =
    for t in tags:
      if t[0] == s: return some(r.sub(t[1], t[2]))
    none(Reader)
  proc xyzTag(s: string): Option[Vec3] =
    let d = tagData(s)
    if d.isNone: return none(Vec3)
    if d.get.len >= 20 and d.get.str4(0) == "XYZ ": return some(d.get.xyz(8))
    raise newCmsError(ceInvalid, "tag " & s & " is not XYZType")
  proc curveTag(s: string): Option[Curve] =
    let d = tagData(s)
    if d.isNone: none(Curve) else: some(parseCurve(d.get)[0])
  let p = Profile(version: version, class: class, colorSpace: cs, pcs: pcs, renderingIntent: intent)
  p.whitePoint = xyzTag("wtpt").get(D50)
  try: p.blackPoint = xyzTag("bkpt")
  except CmsError: p.blackPoint = none(Vec3)
  let chadD = tagData("chad")
  if chadD.isSome and chadD.get.len >= 44 and chadD.get.str4(0) == "sf32":
    var m: Mat3
    for i in 0..2:
      for j in 0..2: m[i][j] = chadD.get.s15f16(8 + (i * 3 + j) * 4)
    p.chad = some(m)
  let rr = xyzTag("rXYZ")
  let gg = xyzTag("gXYZ")
  let bb = xyzTag("bXYZ")
  if rr.isSome and gg.isSome and bb.isSome:
    let (a, b, c) = (rr.get, gg.get, bb.get)
    p.matrix = some([[a[0], b[0], c[0]], [a[1], b[1], c[1]], [a[2], b[2], c[2]]])
  let rt = curveTag("rTRC")
  let gt = curveTag("gTRC")
  let bt = curveTag("bTRC")
  if rt.isSome and gt.isSome and bt.isSome: p.trc = some([rt.get, gt.get, bt.get])
  p.grayTrc = curveTag("kTRC")
  for k, name in ["A2B0", "A2B1", "A2B2"]:
    let d = tagData(name)
    if d.isSome: p.a2b[k] = some(parseLut(d.get, true))
  for k, name in ["B2A0", "B2A1", "B2A2"]:
    let d = tagData(name)
    if d.isSome: p.b2a[k] = some(parseLut(d.get, false))
  let desc = tagData("desc")
  if desc.isSome: p.description = parseText(desc.get)
  let cprt = tagData("cprt")
  if cprt.isSome: p.copyright = parseText(cprt.get)
  let dev = cs.channels
  let conn = if class.kind == pcDeviceLink: spaceFromSig(pcsSig).channels else: 3
  for l in p.a2b:
    if l.isSome:
      if (dev != 0 and l.get.inputs != dev) or (conn != 0 and l.get.outputs != conn):
        raise newCmsError(ceInvalid, "AToB tag has " & $l.get.inputs & "→" & $l.get.outputs & " channels for a " & $cs & " profile")
  for l in p.b2a:
    if l.isSome:
      if (conn != 0 and l.get.inputs != conn) or (dev != 0 and l.get.outputs != dev):
        raise newCmsError(ceInvalid, "BToA tag has " & $l.get.inputs & "→" & $l.get.outputs & " channels for a " & $cs & " profile")
  p

proc parseProfile*(bytes: openArray[byte]): Profile =
  ## Parses ICC profile bytes, keeping them for byte-exact embedding. Raises `CmsError`.
  result = parseProfileModel(bytes)
  result.bytes = some(@bytes)
