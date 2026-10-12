## Swatch and contour preset files: Photoshop Color Swatches (`.aco`, versions 1 and 2), Adobe
## Swatch Exchange (`.ase`) and contour presets (`.shc`). Layouts from Adobe's public file format
## specification and public descriptions of ASE. Colours are kept as stored; unknown colour
## spaces/models are kept verbatim so a read → write round trip never loses them.

import std/math
import ./io

# ---------------------------------------------------------------- .aco

const
  AcoMaxSwatches* = 65535
  AcoMaxNameUnits* = 1024
  acoRgb* = 0'u16
  acoHsb* = 1'u16
  acoCmyk* = 2'u16
  acoLab* = 7'u16
  acoGray* = 8'u16

type
  AcoColorKind* = enum ackRgb, ackHsb, ackCmyk, ackLab, ackGray, ackOther
  AcoColor* = object
    ## RGB/HSB: v[0..2] full 16-bit; CMYK: v[0..3] ink amounts (65535 = 100 %, the file stores
    ## the complement); Lab: l = v[0] (L*×100), a/b signed ×100 in v[1], v[2]; Gray: v[0]
    ## (0 = black, 10000 = white); Other: `space` and the four stored values.
    kind*: AcoColorKind
    space*: uint16
    v*: array[4, uint16]
  AcoSwatch* = object
    name*: string
    color*: AcoColor
  AcoFile* = object
    swatches*: seq[AcoSwatch]
    hasNames*: bool

proc labA*(c: AcoColor): int16 = cast[int16](c.v[1])
proc labB*(c: AcoColor): int16 = cast[int16](c.v[2])

proc acoFromRecord*(space: uint16; v: array[4, uint16]): AcoColor =
  case space
  of acoRgb: AcoColor(kind: ackRgb, space: space, v: [v[0], v[1], v[2], 0])
  of acoHsb: AcoColor(kind: ackHsb, space: space, v: [v[0], v[1], v[2], 0])
  of acoCmyk: AcoColor(kind: ackCmyk, space: space, v: [65535'u16 - v[0], 65535'u16 - v[1], 65535'u16 - v[2], 65535'u16 - v[3]])
  of acoLab: AcoColor(kind: ackLab, space: space, v: [v[0], v[1], v[2], 0])
  of acoGray: AcoColor(kind: ackGray, space: space, v: [v[0], 0, 0, 0])
  else: AcoColor(kind: ackOther, space: space, v: v)

proc toRecord*(c: AcoColor): (uint16, array[4, uint16]) =
  case c.kind
  of ackRgb: (acoRgb, [c.v[0], c.v[1], c.v[2], 0'u16])
  of ackHsb: (acoHsb, [c.v[0], c.v[1], c.v[2], 0'u16])
  of ackCmyk: (acoCmyk, [65535'u16 - c.v[0], 65535'u16 - c.v[1], 65535'u16 - c.v[2], 65535'u16 - c.v[3]])
  of ackLab: (acoLab, [c.v[0], c.v[1], c.v[2], 0'u16])
  of ackGray: (acoGray, [c.v[0], 0'u16, 0, 0])
  of ackOther: (c.space, c.v)

proc readNameU32(r: var Reader; maxUnits: int): seq[uint16] =
  let n = uint64(r.u32())
  r.checkCount(n, 2)
  for _ in 0'u64 ..< n:
    let u = r.u16()
    if result.len < maxUnits: result.add u

proc trimNul(units: var seq[uint16]) =
  while units.len > 0 and units[^1] == 0: units.setLen(units.len - 1)

proc readAcoSection(r: var Reader; named: bool): seq[AcoSwatch] =
  let count = int(r.u16())
  r.checkCount(uint64(count), if named: 14 else: 10)
  for _ in 0 ..< count:
    let space = r.u16()
    var v: array[4, uint16]
    for k in 0..3: v[k] = r.u16()
    var name = ""
    if named:
      var units = readNameU32(r, AcoMaxNameUnits)
      trimNul(units)
      name = fromUtf16(units)
    result.add AcoSwatch(name: name, color: acoFromRecord(space, v))

proc parseAco*(data: openArray[byte]): AcoFile =
  var r = newReader(data)
  let v = r.u16()
  case v
  of 1:
    let v1 = readAcoSection(r, false)
    if r.remaining >= 4 and r.at(r.pos) == 0 and r.at(r.pos + 1) == 2:
      discard r.u16()
      return AcoFile(swatches: readAcoSection(r, true), hasNames: true)
    AcoFile(swatches: v1, hasNames: false)
  of 2: AcoFile(swatches: readAcoSection(r, true), hasNames: true)
  else: raise psdError(peUnsupportedVersion, $v)

proc writeAco*(swatches: openArray[AcoSwatch]): seq[byte] =
  if swatches.len > AcoMaxSwatches: raise limitExceeded("an .aco file holds at most 65535 swatches")
  for version in [1'u16, 2]:
    result.putU16(version)
    result.putU16(uint16(swatches.len))
    for s in swatches:
      let (space, v) = s.color.toRecord
      result.putU16(space)
      for x in v: result.putU16(x)
      if version == 2:
        var units = utf16Units(s.name)
        if units.len > AcoMaxNameUnits: units.setLen(AcoMaxNameUnits)
        result.putU32(uint32(units.len + 1))
        for u in units: result.putU16(u)
        result.putU16(0)

# ---------------------------------------------------------------- .ase

const
  AseSignature* = "ASEF"
  AseMaxSwatches* = 100_000
  AseMaxNameUnits* = 1024
  aseGroupStart = 0xC001'u16
  aseGroupEnd = 0xC002'u16
  aseColor = 0x0001'u16

type
  AseColorKind* = enum ascRgb, ascCmyk, ascLab, ascGray, ascOther
  AseColor* = object
    kind*: AseColorKind
    c*: seq[float32]     ## components as stored (RGB/CMYK/gray 0..1; Lab L/100, a, b)
    model*: FourCC       ## for `ascOther`
    data*: seq[byte]     ## for `ascOther`: bytes after the model tag
  AseKind* = enum akGlobal, akSpot, akProcess
  AseSwatch* = object
    name*: string
    color*: AseColor
    kind*: AseKind
  AseEntry* = object
    isGroup*: bool
    swatch*: AseSwatch        ## when not a group
    name*: string             ## group name
    swatches*: seq[AseSwatch] ## group colours
  AseFile* = object
    entries*: seq[AseEntry]

proc swatchCount*(f: AseFile): int =
  for e in f.entries: result += (if e.isGroup: e.swatches.len else: 1)

proc f32be(r: var Reader): float32 =
  let v = cast[float32](r.u32())
  if v != v or v == Inf or v == NegInf: 0'f32 else: v

proc readAseName(r: var Reader): string =
  if r.isEmpty: return ""
  let n = int(r.u16())
  r.checkCount(uint64(n), 2)
  var units: seq[uint16]
  for _ in 0 ..< n:
    let u = r.u16()
    if units.len < AseMaxNameUnits: units.add u
  trimNul(units)
  fromUtf16(units)

proc readAseColor(b: var Reader): AseSwatch =
  result.name = readAseName(b)
  let model = b.fourcc()
  template floats(n: int): seq[float32] =
    var s: seq[float32]
    for _ in 0 ..< n: s.add f32be(b)
    s
  case model
  of "RGB ": result.color = AseColor(kind: ascRgb, c: floats(3))
  of "CMYK": result.color = AseColor(kind: ascCmyk, c: floats(4))
  of "LAB ": result.color = AseColor(kind: ascLab, c: floats(3))
  of "Gray": result.color = AseColor(kind: ascGray, c: floats(1))
  else: result.color = AseColor(kind: ascOther, model: model, data: b.bytes(max(b.remaining - 2, 0)))
  result.kind = if b.remaining >= 2:
      (case b.u16()
       of 0: akGlobal
       of 1: akSpot
       else: akProcess)
    else: akProcess

proc parseAse*(data: openArray[byte]): AseFile =
  var r = newReader(data)
  let sig = r.fourcc()
  if sig != AseSignature: raise badSignature("ASEF", sig)
  let major = r.u16()
  discard r.u16()
  if major != 1: raise psdError(peUnsupportedVersion, $major)
  let count = uint64(r.u32())
  r.checkCount(count, 6)
  var open = false
  var group: AseEntry
  var total = 0
  for _ in 0'u64 ..< count:
    let kind = r.u16()
    let ln = uint64(r.u32())
    var body = r.sub(ln)
    case kind
    of aseGroupStart:
      if open: result.entries.add group
      group = AseEntry(isGroup: true, name: readAseName(body))
      open = true
    of aseGroupEnd:
      if open:
        result.entries.add group
        open = false
    of aseColor:
      inc total
      if total > AseMaxSwatches: raise limitExceeded("too many swatches in an .ase file")
      let s = readAseColor(body)
      if open: group.swatches.add s
      else: result.entries.add AseEntry(swatch: s)
    else: discard
  if open: result.entries.add group

proc putAseName(o: var seq[byte]; name: string) =
  var units = utf16Units(name)
  if units.len > AseMaxNameUnits: units.setLen(AseMaxNameUnits)
  o.putU16(uint16(units.len + 1))
  for u in units: o.putU16(u)
  o.putU16(0)

proc putBlock(o: var seq[byte]; kind: uint16; body: openArray[byte]) =
  o.putU16(kind)
  o.putU32(uint32(body.len))
  o.put(body)

proc putAseColor(o: var seq[byte]; s: AseSwatch) =
  var b: seq[byte]
  putAseName(b, s.name)
  case s.color.kind
  of ascRgb: b.put("RGB ")
  of ascCmyk: b.put("CMYK")
  of ascLab: b.put("LAB ")
  of ascGray: b.put("Gray")
  of ascOther: b.put(s.color.model)
  if s.color.kind == ascOther: b.put(s.color.data)
  else:
    for x in s.color.c: b.putU32(cast[uint32](x))
  b.putU16(uint16(ord(s.kind)))
  putBlock(o, aseColor, b)

proc writeAse*(f: AseFile): seq[byte] =
  if f.swatchCount > AseMaxSwatches: raise limitExceeded("too many swatches for an .ase file")
  var body: seq[byte]
  var blocks = 0
  for e in f.entries:
    if e.isGroup:
      var b: seq[byte]
      putAseName(b, e.name)
      putBlock(body, aseGroupStart, b)
      for s in e.swatches: putAseColor(body, s)
      putBlock(body, aseGroupEnd, [])
      blocks += 2 + e.swatches.len
    else:
      putAseColor(body, e.swatch)
      inc blocks
  result.put(AseSignature)
  result.putU16(1)
  result.putU16(0)
  result.putU32(uint32(blocks))
  result.put(body)

# ---------------------------------------------------------------- .shc

const
  ShcMaxContours* = 4096
  ShcMaxPoints* = 256
  ShcMaxNameUnits* = 1024

type
  ShcPoint* = object
    input*, output*: float32  ## 0..255
    continuous*: bool
  ShcContour* = object
    name*: string
    points*: seq[ShcPoint]

proc parseShc*(data: openArray[byte]): seq[ShcContour] =
  var r = newReader(data)
  let sig = r.fourcc()
  if sig != "8BFS": raise badSignature("8BFS", sig)
  let version = r.u16()
  if version != 1: raise psdError(peUnsupportedVersion, $version)
  let count = uint64(r.u32())
  r.checkCount(count, 18)
  if count > ShcMaxContours: raise limitExceeded("too many contours in an .shc file")
  for _ in 0'u64 ..< count:
    let v = r.u32()
    if v != 1 and v != 2: raise invalid("unknown contour version " & $v)
    var units = readNameU32(r, ShcMaxNameUnits)
    if (units.len == 0 or units[^1] != 0) and r.remaining >= 2 and r.at(r.pos) == 0 and r.at(r.pos + 1) == 0:
      discard r.u16()
    trimNul(units)
    var c = ShcContour(name: fromUtf16(units))
    let n = int(r.u16())
    r.checkCount(uint64(n), if v == 2: 5 else: 4)
    if n > ShcMaxPoints: raise limitExceeded("too many points in a contour")
    for _ in 0 ..< n:
      let output = min(float32(r.u16()), 255'f32)
      let input = min(float32(r.u16()), 255'f32)
      c.points.add ShcPoint(input: input, output: output, continuous: true)
    if v == 2:
      for p in c.points.mitems: p.continuous = r.u8() != 0
    r.skip(8)
    result.add c

proc writeShc*(contours: openArray[ShcContour]): seq[byte] =
  if contours.len > ShcMaxContours: raise limitExceeded("too many contours for an .shc file")
  result.put("8BFS")
  result.putU16(1)
  result.putU32(uint32(contours.len))
  for c in contours:
    if c.points.len > ShcMaxPoints: raise limitExceeded("too many points in a contour")
    result.putU32(2)
    var units = utf16Units(c.name)
    if units.len > ShcMaxNameUnits: units.setLen(ShcMaxNameUnits)
    result.putU32(uint32(units.len + 1))
    for u in units: result.putU16(u)
    result.putU16(0)
    result.putU16(uint16(c.points.len))
    template level(v: float32): uint16 =
      (if v == v and v != Inf and v != NegInf: uint16(clamp(round(v), 0'f32, 255'f32)) else: 0'u16)
    for p in c.points:
      result.putU16(level(p.output))
      result.putU16(level(p.input))
    for p in c.points: result.putU8(if p.continuous: 1 else: 0)
    result.putU32(0)
    result.putU32(255)
