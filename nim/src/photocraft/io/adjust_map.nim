## Adjustment layers ↔ PSD adjustment blocks (`levl`, `curv`, `hue2`, `brit`+`CgEd`, `nvrt`,
## `thrs`, `post`, `expA`, `vibA`, `blnc`, `mixr`, `grdm`, `phfl`, `selc`, `blwh`, `clrL`).
## Unknown or unreadable blocks become `Unsupported` adjustments kept verbatim.

import std/[options, math, algorithm]
import ../color, ../color/convert, ../doc, ../psd, ../cms/lutfile, ../algo/util, ./blocks, ./channel_map

const AdjustmentKeys* = ["levl", "curv", "hue2", "brit", "nvrt", "thrs", "post", "expA", "vibA", "blnc", "mixr",
                         "grdm", "phfl", "selc", "blwh", "clrL"]

type Channels* = enum chRgb, chGray, chCmyk, chLab, chOther

proc be16(d: openArray[byte]; at: int): Option[uint16] =
  if at < 0 or at + 2 > d.len: none(uint16) else: some((uint16(d[at]) shl 8) or uint16(d[at + 1]))
proc bei16(d: openArray[byte]; at: int): Option[int16] =
  let v = be16(d, at)
  if v.isNone: none(int16) else: some(cast[int16](v.get))
proc be32(d: openArray[byte]; at: int): Option[uint32] =
  if at < 0 or at + 4 > d.len: return none(uint32)
  some((uint32(d[at]) shl 24) or (uint32(d[at + 1]) shl 16) or (uint32(d[at + 2]) shl 8) or uint32(d[at + 3]))
proc bef32(d: openArray[byte]; at: int): Option[float32] =
  let v = be32(d, at)
  if v.isNone: none(float32) else: some(cast[float32](v.get))

template tryGet(x: untyped): untyped =
  (let v = x; if v.isNone: return none(Adjustment); v.get)

proc unsupportedAdj(key: string; data: openArray[byte]): Adjustment =
  Adjustment(kind: akUnsupported, psdKey: key, raw: @data)

proc levelsRec(d: openArray[byte]; at: int): Option[LevelsChannel] =
  let (a, b, c, e, g) = (be16(d, at), be16(d, at + 2), be16(d, at + 4), be16(d, at + 6), be16(d, at + 8))
  if a.isNone or b.isNone or c.isNone or e.isNone or g.isNone: return none(LevelsChannel)
  some(LevelsChannel(inBlack: float32(a.get) / 255, inWhite: float32(b.get) / 255, outBlack: float32(c.get) / 255,
                     outWhite: float32(e.get) / 255, gamma: float32(g.get) / 100))

proc line(): seq[CurvePoint] = @[CurvePoint(input: 0, output: 0), CurvePoint(input: 1, output: 1)]

proc parseCurves(d: openArray[byte]): Option[Adjustment] =
  if d.len < 1: return none(Adjustment)
  let pencil = d[0] != 0
  let version = tryGet(be16(d, 1))
  if version != 1 and version != 4: return none(Adjustment)
  let bits = tryGet(be32(d, 3))
  var at = 7
  var curves = @[line(), line(), line(), line(), line()]
  for bit in 0 ..< 32:
    if (bits and (1'u32 shl bit)) == 0: continue
    if pencil:
      if at + CurveMapLen > d.len: return none(Adjustment)
      var outs = newSeq[float32](CurveMapLen)
      for k in 0 ..< CurveMapLen: outs[k] = float32(d[at + k]) / 255
      at += CurveMapLen
      if bit < curves.len: curves[bit] = curveMap(outs)
      continue
    let n = int(tryGet(be16(d, at)))
    at += 2
    var pts: seq[CurvePoint]
    for _ in 0 ..< n:
      let o = tryGet(be16(d, at))
      let i = tryGet(be16(d, at + 2))
      at += 4
      pts.add CurvePoint(input: float32(i) / 255, output: float32(o) / 255)
    if bit < curves.len: curves[bit] = pts
  some(Adjustment(kind: akCurves, curvesMaster: curves[0], curvesPerChannel: [curves[1], curves[2], curves[3]],
                  space: tsRgb, curvesBlack: curves[4], pencil: pencil))

proc descNum(d: Descriptor; key: string): Option[float32] =
  let v = num(d.get(key))
  if v.isNone: none(float32) else: some(float32(v.get))

proc label*(key: string): string =
  case key
  of "levl": "Levels"
  of "curv": "Curves"
  of "hue2": "Hue/Saturation"
  of "brit": "Brightness/Contrast"
  of "nvrt": "Invert"
  of "thrs": "Threshold"
  of "post": "Posterize"
  of "expA": "Exposure"
  of "vibA": "Vibrance"
  of "blnc": "Color Balance"
  of "mixr": "Channel Mixer"
  of "grdm": "Gradient Map"
  of "phfl": "Photo Filter"
  of "selc": "Selective Color"
  of "blwh": "Black & White"
  of "clrL": "Color Lookup"
  else: "adjustment"

proc readLookup(d: openArray[byte]; err: var string): Option[Adjustment] =
  let ver = be16(d, 0)
  if ver.isNone:
    err = "the block is truncated"
    return none(Adjustment)
  if ver.get != 1:
    err = "unknown version " & $ver.get
    return none(Adjustment)
  var desc: Descriptor
  try:
    desc = parseVersionedPrefix(d.toOpenArray(2, d.len - 1))[0].descriptor
  except CatchableError as e:
    err = "unreadable settings (" & e.msg & ")"
    return none(Adjustment)
  let p = desc.getPtr("LUT3DFileData")
  if p == nil:
    err = "it uses a colour profile (abstract or device link), not a LUT file, which isn't supported yet"
    return none(Adjustment)
  if p.kind != vkRawData or p.raw.len == 0:
    err = "its LUT data is empty"
    return none(Adjustment)
  let (ext, format) = (case enumOf(desc, "LUTFormat").get("")
    of "LUTFormat3DL": ("x.3dl", "3DL")
    of "LUTFormatLOOK": ("x.look", "LOOK")
    else: ("x.cube", "CUBE"))
  var lut: LutFile
  try: lut = parseLutFile(ext, p.raw)
  except CatchableError as e:
    err = "its " & format & " LUT can't be read (" & e.msg & ")"
    return none(Adjustment)
  var name = textOf(desc, "LUT3DFileName")
  if desc.getPtr("LUT3DFileName") == nil: name = textOf(desc, "NM  ")
  some(Adjustment(kind: akColorLookup, lutName: name, lutSize: uint32(lut.size), lut: SharedF32(data: lut.data),
                  tetrahedral: false, dither: desc.getBool("Dthr").get(false)))

proc unreadableReason*(key: string; data: openArray[byte]): Option[string] =
  if key == "clrL":
    var err = ""
    if readLookup(data, err).isNone: return some(err)
  none(string)

proc parseGradientMap(d: openArray[byte]): Option[Adjustment] =
  let version = tryGet(be16(d, 0))
  if d.len < 3: return none(Adjustment)
  let reverse = d[2] != 0
  let dither = d.len > 3 and d[3] != 0
  var meth: Option[string]
  var at: int
  case version
  of 1: at = 4
  of 3:
    if d.len < 8: return none(Adjustment)
    meth = some(char(d[4]) & char(d[5]) & char(d[6]) & char(d[7]))
    at = 8
  else: return none(Adjustment)
  at += 4 + int(tryGet(be32(d, at))) * 2
  let n = int(tryGet(be16(d, at)))
  at += 2
  var stops: seq[(float32, Color)]
  var mids: seq[float32]
  for _ in 0 ..< n:
    let loc = clamp(float32(tryGet(be32(d, at))) / 4096, 0'f32, 1'f32)
    let mid = float32(tryGet(be32(d, at + 4))) / 100
    if tryGet(be16(d, at + 8)) != 0: return none(Adjustment)
    let (r, g, b) = (tryGet(be16(d, at + 10)), tryGet(be16(d, at + 12)), tryGet(be16(d, at + 14)))
    stops.add (loc, rgb(float32(r) / 65535, float32(g) / 65535, float32(b) / 65535))
    mids.add mid
    at += 20
  if stops.len < 2: return none(Adjustment)
  let nt = int(tryGet(be16(d, at)))
  at += 2 + nt * 10 + 2
  let smooth = float32(tryGet(be16(d, at))) / 4096
  var order = newSeq[int](stops.len)
  for i in 0 ..< order.len: order[i] = i
  order.sort(proc(a, b: int): int = totalCmp(stops[a][0], stops[b][0]))
  var m2: seq[float32]
  for k in 1 ..< order.len: m2.add mids[order[k]]
  let baked = bake(stops, m2, smooth, methodFromCode(meth))
  var outStops: seq[(float32, array[3, float32])]
  for (t, c) in baked: outStops.add (t, c.toRgb)
  outStops.sort(proc(a, b: (float32, array[3, float32])): int = totalCmp(a[0], b[0]))
  some(Adjustment(kind: akGradientMap, gmStops: outStops, reverse: reverse, dither: dither))

proc put16(v: var seq[byte]; x: uint16) =
  v.add byte(x shr 8)
  v.add byte(x and 0xff)
proc put32(v: var seq[byte]; x: uint32) =
  for k in countdown(3, 0): v.add byte((x shr (8 * k)) and 0xff)
proc putI16(v: var seq[byte]; x: int16) = v.put16(cast[uint16](x))
proc putF32(v: var seq[byte]; x: float32) = v.put32(cast[uint32](x))

proc writeGradientMap(stops: openArray[(float32, array[3, float32])]; reverse, dither: bool): seq[byte] =
  var v: seq[byte]
  v.put16(1)
  v.add uint8(reverse)
  v.add uint8(dither)
  var name = utf16Units("Custom")
  name.add 0
  v.put32(uint32(name.len))
  for u in name: v.put16(u)
  let n = min(stops.len, 65535)
  v.put16(uint16(n))
  for i in 0 ..< n:
    let (t, c) = stops[i]
    v.put32(uint32(round(clamp(t, 0'f32, 1'f32) * 4096)))
    v.put32(50)
    v.put16(0)
    for x in c: v.put16(uint16(round(clamp(x, 0'f32, 1'f32) * 65535)))
    v.put16(0)
    v.put16(0)
  v.put16(2)
  for t in [0'u32, 4096]:
    v.put32(t)
    v.put32(50)
    v.put16(255)
  v.put16(2)
  v.put16(0)
  v.put16(32)
  v.put16(0)
  v.put32(0)
  v.put16(0)
  v.put16(0)
  v.put32(2048)
  v.put16(3)
  for x in [0'u16, 0, 0, 0, 0x8000, 0x8000, 0x8000, 0x8000]: v.put16(x)
  v.put16(0)
  v

proc parseSelective(d: openArray[byte]): Option[Adjustment] =
  if be16(d, 0) != some(1'u16) or d.len < 84: return none(Adjustment)
  let relative = tryGet(be16(d, 2)) == 0
  var adj: array[9, array[4, float32]]
  for r in 0 ..< 9:
    for k in 0 ..< 4: adj[r][k] = float32(tryGet(bei16(d, 4 + (r + 1) * 8 + k * 2)))
  some(Adjustment(kind: akSelectiveColor, relative: relative, selective: adj))

const
  BwDefault = [40'f32, 60, 40, 60, 20, 80]
  BwKeys = ["Rd  ", "Yllw", "Grn ", "Cyn ", "Bl  ", "Mgnt"]
  BwTint = [225'f32 / 255, 211'f32 / 255, 179'f32 / 255]

proc descRgb(d: Descriptor): Option[array[3, float32]] =
  if d.classId.s == "RGBC" and d.get("redFloat").isNone:
    let (r, g, b) = (num(d.get("Rd  ")), num(d.get("Grn ")), num(d.get("Bl  ")))
    if r.isNone or g.isNone or b.isNone: return none(array[3, float32])
    return some([float32(clamp(r.get / 255, 0, 1)), float32(clamp(g.get / 255, 0, 1)), float32(clamp(b.get / 255, 0, 1))])
  let c = colorFromDesc(d)
  if c.isNone: return none(array[3, float32])
  let v = c.get.toRgb
  some([clamp(v[0], 0'f32, 1'f32), clamp(v[1], 0'f32, 1'f32), clamp(v[2], 0'f32, 1'f32)])

proc rgbDesc(c: array[3, float32]): Descriptor =
  template dv(x: float32): Value = vDouble(float64(clamp(x, 0'f32, 1'f32)) * 255)
  newDescriptor("RGBC").with("Rd  ", dv(c[0])).with("Grn ", dv(c[1])).with("Bl  ", dv(c[2]))

proc parseAny(key: string; data: openArray[byte]; cged: Option[seq[byte]]): Adjustment =
  var parsed: Option[Adjustment]
  case key
  of "nvrt": parsed = some(Adjustment(kind: akInvert))
  of "thrs":
    let v = be16(data, 0)
    if v.isSome: parsed = some(Adjustment(kind: akThreshold, level: float32(v.get) / 255))
  of "post":
    let v = be16(data, 0)
    if v.isSome: parsed = some(Adjustment(kind: akPosterize, levels: uint32(v.get)))
  of "brit":
    if cged.isSome:
      let d = parsePrefixVersioned(cged.get)
      if d.isSome:
        let (b, c) = (descNum(d.get, "Brgh"), descNum(d.get, "Cntr"))
        if b.isSome and c.isSome:
          parsed = some(Adjustment(kind: akBrightnessContrast, brightness: b.get, contrast: c.get,
                                   legacy: d.get.getBool("useLegacy").get(false)))
    if parsed.isNone:
      let (b, c) = (bei16(data, 0), bei16(data, 2))
      if b.isSome and c.isSome:
        parsed = some(Adjustment(kind: akBrightnessContrast, brightness: float32(b.get), contrast: float32(c.get), legacy: true))
  of "hue2":
    if data.len > 2:
      let colorize = data[2] != 0
      let base = (if colorize: 4 else: 10)
      var ranges = defaultHueRanges()
      for i in 0 ..< 6:
        let at = 16 + i * 14
        var vs: array[7, float32]
        var ok = true
        for k in 0 ..< 7:
          let v = bei16(data, at + 2 * k)
          if v.isNone: ok = false else: vs[k] = float32(v.get)
        if ok:
          ranges[i] = HueRange(hue: vs[4], saturation: vs[5], lightness: vs[6], bounds: canonicalBounds([vs[0], vs[1], vs[2], vs[3]]))
      let (h, s, l) = (bei16(data, base), bei16(data, base + 2), bei16(data, base + 4))
      if h.isSome and s.isSome and l.isSome:
        parsed = some(Adjustment(kind: akHueSaturation, hue: float32(h.get), saturation: float32(s.get),
                                 lightness: float32(l.get), colorize: colorize, ranges: ranges))
  of "expA":
    let (e, o, g) = (bef32(data, 2), bef32(data, 6), bef32(data, 10))
    if e.isSome and o.isSome and g.isSome:
      parsed = some(Adjustment(kind: akExposure, exposure: e.get, offset: o.get, gamma: g.get))
  of "levl":
    let (m, r, g, b) = (levelsRec(data, 2), levelsRec(data, 12), levelsRec(data, 22), levelsRec(data, 32))
    if m.isSome and r.isSome and g.isSome and b.isSome:
      parsed = some(Adjustment(kind: akLevels, levelsMaster: m.get, levelsPerChannel: [r.get, g.get, b.get], space: tsRgb,
                               levelsBlack: levelsRec(data, 42).get(defaultLevelsChannel())))
  of "curv": parsed = parseCurves(data)
  of "selc": parsed = parseSelective(data)
  of "clrL":
    var err = ""
    parsed = readLookup(data, err)
  of "grdm": parsed = parseGradientMap(data)
  of "vibA":
    let d = parsePrefixVersioned(data)
    if d.isSome:
      proc g(d: Descriptor; k: string): float32 = clamp(descNum(d, k).get(0), -100'f32, 100'f32)
      parsed = some(Adjustment(kind: akVibrance, vibrance: g(d.get, "vibrance"), saturation: g(d.get, "Strt")))
  of "blnc":
    var tones: array[3, array[3, float32]]
    var ok = data.len > 18
    for k in 0 .. 2:
      for i in 0 .. 2:
        let v = bei16(data, (k * 3 + i) * 2)
        if v.isNone: ok = false else: tones[k][i] = clamp(float32(v.get), -100'f32, 100'f32)
    if ok:
      parsed = some(Adjustment(kind: akColorBalance, shadows: tones[0], midtones: tones[1], highlights: tones[2],
                               preserveLuminosity: data[18] != 0))
  of "blwh":
    let d = parsePrefixVersioned(data)
    if d.isSome:
      var weights = BwDefault
      for i, k in BwKeys:
        let v = descNum(d.get, k)
        if v.isSome: weights[i] = clamp(v.get, -200'f32, 300'f32)
      var tint = none(array[3, float32])
      if d.get.getBool("useTint").get(false):
        let td = getD(d.get, "tintColor")
        let rgbv = (if td.isSome: descRgb(td.get) else: none(array[3, float32]))
        tint = some(rgbv.get(BwTint))
      parsed = some(Adjustment(kind: akBlackWhite, weights: weights, tint: tint))
  of "phfl":
    let ver = be16(data, 0)
    var color: array[3, float32]
    var at = -1
    if ver == some(2'u16):
      let space = be16(data, 2)
      if space.isSome and space.get in [0'u16, 2, 7, 8]:
        let (a, b, c, d) = (be16(data, 4), be16(data, 6), be16(data, 8), be16(data, 10))
        if a.isSome and b.isSome and c.isSome and d.isSome:
          color = decodeColor(space.get, [a.get, b.get, c.get, d.get]).toRgb
          at = 12
    elif ver == some(3'u16):
      let (a, b, c) = (be32(data, 2), be32(data, 6), be32(data, 10))
      if a.isSome and b.isSome and c.isSome:
        let lab = [float32(cast[int32](a.get)) / 100, float32(cast[int32](b.get)) / 100, float32(cast[int32](c.get)) / 100]
        if lab[0] >= 0 and lab[0] <= 100 and abs(lab[1]) <= 128 and abs(lab[2]) <= 128:
          color = labToSrgb(lab)
          at = 14
    if at >= 0:
      let dens = be32(data, at)
      if dens.isSome and at + 4 < data.len:
        parsed = some(Adjustment(kind: akPhotoFilter,
                                 filterColor: [clamp(color[0], 0'f32, 1'f32), clamp(color[1], 0'f32, 1'f32), clamp(color[2], 0'f32, 1'f32)],
                                 density: clamp(float32(dens.get) / 100, 0'f32, 1'f32), preserveLuminosity: data[at + 4] != 0))
  of "mixr":
    if be16(data, 0) == some(1'u16):
      let mono = be16(data, 2)
      if mono.isSome:
        proc rec(d: openArray[byte]; r: int): Option[array[4, float32]] =
          let at = 4 + r * 10
          var o: array[4, float32]
          for i, k in [0, 1, 2, 4]:
            let v = bei16(d, at + k * 2)
            if v.isNone: return none(array[4, float32])
            o[i] = clamp(float32(v.get), -200'f32, 200'f32) / 100
          some(o)
        let first = rec(data, 0)
        if first.isSome:
          var g = rec(data, 1)
          var b = rec(data, 2)
          if mono.get != 0:
            if g.isNone: g = some([0'f32, 0, 0, 0])
            if b.isNone: b = some([0'f32, 0, 0, 0])
          if g.isSome and b.isSome:
            parsed = some(Adjustment(kind: akChannelMixer, matrix: [first.get, g.get, b.get], monochrome: mono.get != 0))
  else: discard
  if parsed.isSome: parsed.get else: unsupportedAdj(key, data)

proc parse*(key: string; data: openArray[byte]; cged: Option[seq[byte]]; channels: Channels): Adjustment =
  if key == "mixr" and channels in {chCmyk, chOther}:
    var r = 0
    while true:
      let k = bei16(data, 4 + r * 10 + 6)
      if k.isNone: break
      if k.get != 0: return unsupportedAdj(key, data)
      inc r
  result = parseAny(key, data, cged)
  case result.kind
  of akLevels:
    if channels in {chCmyk, chLab}:
      result.space = (if channels == chCmyk: tsCmyk else: tsLab)
      if channels == chLab: result.levelsBlack = defaultLevelsChannel()
    else: result.levelsBlack = defaultLevelsChannel()
    case channels
    of chGray:
      let g = result.levelsPerChannel[0]
      result.levelsPerChannel = [g, g, g]
    of chOther: result.levelsPerChannel = [defaultLevelsChannel(), defaultLevelsChannel(), defaultLevelsChannel()]
    else: discard
  of akCurves:
    case channels
    of chCmyk:
      result.space = tsCmyk
      if isIdentityCurve(result.curvesBlack): result.curvesBlack.setLen(0)
    of chLab:
      result.space = tsLab
      result.curvesBlack.setLen(0)
    else: result.curvesBlack.setLen(0)
    case channels
    of chGray:
      let g = result.curvesPerChannel[0]
      result.curvesPerChannel = [g, g, g]
    of chOther: result.curvesPerChannel = [line(), line(), line()]
    else: discard
  else: discard

proc q255(v: float32): uint16 = uint16(round(clamp(v, 0'f32, 1'f32) * 255))
proc clampI16(v: float32): int16 = int16(clamp(round(v), -32768'f32, 32767'f32))

proc levelsWrite(v: var seq[byte]; c: LevelsChannel) =
  v.put16(q255(c.inBlack))
  v.put16(q255(c.inWhite))
  v.put16(q255(c.outBlack))
  v.put16(q255(c.outWhite))
  v.put16(uint16(clamp(round(c.gamma * 100), 1'f32, 999'f32)))

proc write*(adj: Adjustment): seq[(string, seq[byte])] =
  ## The PSD block(s) for an adjustment layer.
  var v: seq[byte]
  case adj.kind
  of akInvert: return @[("nvrt", newSeq[byte]())]
  of akThreshold:
    v.put16(max(q255(adj.level), 1'u16))
    v.put16(0)
    return @[("thrs", v)]
  of akPosterize:
    v.put16(uint16(clamp(adj.levels, 2'u32, 255'u32)))
    v.put16(0)
    return @[("post", v)]
  of akBrightnessContrast:
    v.putI16(clampI16(adj.brightness))
    v.putI16(clampI16(adj.contrast))
    v.put16(127)
    v.add 0'u8
    let d = newDescriptor("null").with("Vrsn", vInt(1)).with("Brgh", vInt(int32(round(adj.brightness))))
      .with("Cntr", vInt(int32(round(adj.contrast)))).with("means", vInt(127)).with("Lab ", vBool(false))
      .with("useLegacy", vBool(adj.legacy)).with("Auto", vBool(false))
    return @[("brit", v), ("CgEd", versioned(d).toBytes)]
  of akHueSaturation:
    v.put16(2)
    v.add uint8(adj.colorize)
    v.add 0'u8
    for _ in 0 .. 1:
      for x in [adj.hue, adj.saturation, adj.lightness]: v.putI16(clampI16(x))
    for r in adj.ranges:
      for x in r.bounds: v.put16(uint16(int(round(floorMod(x, 360'f32))) mod 360))
      for x in [r.hue, r.saturation, r.lightness]: v.putI16(clampI16(x))
    return @[("hue2", v)]
  of akExposure:
    v.put16(1)
    for x in [adj.exposure, adj.offset, adj.gamma]: v.putF32(x)
    v.add 1'u8
    return @[("expA", v)]
  of akLevels:
    v.put16(2)
    levelsWrite(v, adj.levelsMaster)
    for c in adj.levelsPerChannel: levelsWrite(v, c)
    levelsWrite(v, (if adj.space == tsCmyk: adj.levelsBlack else: defaultLevelsChannel()))
    for _ in 5 ..< 29: levelsWrite(v, defaultLevelsChannel())
    return @[("levl", v)]
  of akCurves:
    v.add uint8(adj.pencil)
    v.put16(1)
    let ink = adj.space == tsCmyk and adj.curvesBlack.len >= 2
    v.put32(if ink: 0b11111'u32 else: 0b1111'u32)
    var cs = @[adj.curvesMaster, adj.curvesPerChannel[0], adj.curvesPerChannel[1], adj.curvesPerChannel[2]]
    if ink: cs.add adj.curvesBlack
    for c in cs:
      if adj.pencil:
        for o in curveMapOutputs(c): v.add byte(min(q255(o), 255'u16))
        continue
      v.put16(uint16(min(c.len, 19)))
      for i in 0 ..< min(c.len, 19):
        v.put16(q255(c[i].output))
        v.put16(q255(c[i].input))
    return @[("curv", v)]
  of akSelectiveColor:
    v.put16(1)
    v.put16(uint16(not adj.relative))
    for _ in 0 ..< 8: v.add 0'u8
    for rec in adj.selective:
      for x in rec: v.putI16(clampI16(clamp(x, -100'f32, 100'f32)))
    return @[("selc", v)]
  of akColorLookup:
    let n = int(adj.lutSize)
    let len = lut3dLen(adj.lutSize)
    var file: LutFile
    var dither = false
    if not adj.lut.isNil and len.isSome and adj.lut.data.len >= len.get and n <= 256:
      file = LutFile(size: n, data: adj.lut.data[0 ..< len.get], domainMin: [0'f32, 0, 0], domainMax: [1'f32, 1, 1])
      dither = adj.dither
    else:
      file = identityLut(2)
      file.title = ""
    let cube = writeCube(file)
    var cubeBytes = newSeq[byte](cube.len)
    for i, ch in cube: cubeBytes[i] = byte(ch)
    let d = newDescriptor("null").with("lookupType", vEnum("colorLookupType", "3DLUT"))
      .with("NM  ", Value(kind: vkText, text: ustrNul(adj.lutName))).with("Dthr", vBool(dither))
      .with("profile", Value(kind: vkRawData, raw: @[])).with("LUTFormat", vEnum("LUTFormatType", "LUTFormatCUBE"))
      .with("dataOrder", vEnum("colorLookupOrder", "rgbOrder")).with("tableOrder", vEnum("colorLookupOrder", "bgrOrder"))
      .with("LUT3DFileData", Value(kind: vkRawData, raw: cubeBytes))
      .with("LUT3DFileName", Value(kind: vkText, text: ustrNul(adj.lutName)))
    v.put16(1)
    v.add versioned(d).toBytes
    return @[("clrL", v)]
  of akGradientMap:
    var stops = adj.gmStops
    if stops.len == 0: stops = @[(0'f32, [0'f32, 0, 0]), (1'f32, [1'f32, 1, 1])]
    elif stops.len == 1: stops = @[(0'f32, stops[0][1]), (1'f32, stops[0][1])]
    return @[("grdm", writeGradientMap(stops, adj.reverse, adj.dither))]
  of akVibrance:
    let d = newDescriptor("null").with("vibrance", vInt(int32(clampI16(clamp(adj.vibrance, -100'f32, 100'f32)))))
      .with("Strt", vInt(int32(clampI16(clamp(adj.saturation, -100'f32, 100'f32)))))
    return @[("vibA", versioned(d).toBytes)]
  of akColorBalance:
    for t in [adj.shadows, adj.midtones, adj.highlights]:
      for x in t: v.putI16(clampI16(clamp(x, -100'f32, 100'f32)))
    v.add uint8(adj.preserveLuminosity)
    v.add 0'u8
    return @[("blnc", v)]
  of akBlackWhite:
    var d = newDescriptor("null")
    for i, k in BwKeys: d = d.with(k, vInt(int32(clampI16(clamp(adj.weights[i], -200'f32, 300'f32)))))
    d = d.with("useTint", vBool(adj.tint.isSome)).with("tintColor", vDesc(rgbDesc(adj.tint.get(BwTint))))
      .with("bwPresetKind", vInt(1)).with("blackAndWhitePresetFileName", Value(kind: vkText, text: ustrNul("")))
    return @[("blwh", versioned(d).toBytes)]
  of akPhotoFilter:
    v.put16(2)
    v.put16(0)
    for x in adj.filterColor: v.put16(uint16(round(clamp(x, 0'f32, 1'f32) * 65535)))
    v.put16(0)
    v.put32(uint32(round(clamp(adj.density, 0'f32, 1'f32) * 100)))
    v.add uint8(adj.preserveLuminosity)
    v.add [0'u8, 0, 0]
    return @[("phfl", v)]
  of akChannelMixer:
    v.put16(1)
    v.put16(uint16(adj.monochrome))
    for row in [adj.matrix[0], adj.matrix[1], adj.matrix[2], adj.matrix[0]]:
      for i in 0 .. 2: v.putI16(clampI16(clamp(row[i] * 100, -200'f32, 200'f32)))
      v.add [0'u8, 0]
      v.putI16(clampI16(clamp(row[3] * 100, -200'f32, 200'f32)))
    return @[("mixr", v)]
  of akUnsupported:
    if adj.psdKey.len == 4: return @[(adj.psdKey, adj.raw)]
  @[]
