## Path records: saved paths (resources 2000–2997), the work path (1025) and vector masks
## (`vmsk`/`vsms`). A path is a list of 26-byte records (2-byte selector + 24 data bytes); knot
## coordinates are signed 8.24 fixed-point fractions of the document size, vertical first.
## Bytes 2–3 of a subpath length record hold the path operation (as documented by psd-tools).

import std/math
import ./io

const
  RecordLen* = 26
  selClosedLength* = 0'u16
  selClosedKnotLinked* = 1'u16
  selClosedKnotUnlinked* = 2'u16
  selOpenLength* = 3'u16
  selOpenKnotLinked* = 4'u16
  selOpenKnotUnlinked* = 5'u16
  selFillRule* = 6'u16
  selClipboard* = 7'u16
  selInitialFill* = 8'u16

type
  PathRecord* = object
    selector*: uint16
    data*: array[24, byte]
  PsdKnot* = object
    linked*: bool
    pre*, anchor*, post*: (float64, float64)  ## (x, y) fractions
  PsdSubpath* = object
    closed*: bool
    operation*: int16  ## -1/1 combine, 2 subtract, 3 intersect, 0 exclude
    knots*: seq[PsdKnot]
  PathData* = object
    records*: seq[PathRecord]
  VectorMaskBlock* = object
    version*, flags*: uint32
    path*: PathData

const
  vmInvert* = 1'u32
  vmNotLinked* = 2'u32
  vmDisabled* = 4'u32

proc fixedToF64*(v: int32): float64 = float64(v) / 16_777_216.0
proc f64ToFixed*(v: float64): int32 =
  int32(clamp(round(v * 16_777_216.0), float64(low(int32)), float64(high(int32))))

proc u16At(r: PathRecord; at: int): uint16 = beU16(r.data, at)
proc i32At(r: PathRecord; at: int): int32 = cast[int32](beU32(r.data, at))
proc isLength*(r: PathRecord): bool = r.selector == selClosedLength or r.selector == selOpenLength
proc isKnot*(r: PathRecord): bool = r.selector in [1'u16, 2, 4, 5]

proc parsePathData*(data: openArray[byte]): PathData =
  if data.len div RecordLen > 4_000_000: raise limitExceeded("path records")
  var i = 0
  while i + RecordLen <= data.len:
    var rec = PathRecord(selector: beU16(data, i))
    for k in 0 ..< 24: rec.data[k] = data[i + 2 + k]
    result.records.add rec
    i += RecordLen

proc toBytes*(p: PathData): seq[byte] =
  for r in p.records:
    result.putU16(r.selector)
    result.put(r.data)

proc initialFill*(p: PathData): bool =
  for r in p.records:
    if r.selector == selInitialFill: return r.u16At(0) != 0
  false

proc subpaths*(p: PathData): seq[PsdSubpath] =
  var remaining = 0
  for r in p.records:
    if r.isLength:
      remaining = int(r.u16At(0))
      result.add PsdSubpath(closed: r.selector == selClosedLength, operation: cast[int16](r.u16At(2)))
    elif r.isKnot:
      let closed = r.selector <= 2
      if remaining == 0 and (result.len == 0 or result[^1].closed != closed):
        result.add PsdSubpath(closed: closed, operation: -1)
      remaining = max(remaining - 1, 0)
      template pt(i: int): (float64, float64) = (fixedToF64(r.i32At(i * 8 + 4)), fixedToF64(r.i32At(i * 8)))
      if result.len > 0:
        result[^1].knots.add PsdKnot(linked: r.selector in [1'u16, 4], pre: pt(0), anchor: pt(1), post: pt(2))

proc fromSubpaths*(subpaths: openArray[PsdSubpath]; initialFill: bool): PathData =
  result.records.add PathRecord(selector: selFillRule)
  var init = PathRecord(selector: selInitialFill)
  init.data[1] = if initialFill: 1 else: 0
  result.records.add init
  for s in subpaths:
    var lr = PathRecord(selector: if s.closed: selClosedLength else: selOpenLength)
    let n = min(s.knots.len, 65535)
    lr.data[0] = byte(n shr 8); lr.data[1] = byte(n and 0xFF)
    let op = cast[uint16](s.operation)
    lr.data[2] = byte(op shr 8); lr.data[3] = byte(op and 0xFF)
    lr.data[5] = 1
    result.records.add lr
    for k in s.knots[0 ..< n]:
      let sel = if s.closed: (if k.linked: selClosedKnotLinked else: selClosedKnotUnlinked)
                else: (if k.linked: selOpenKnotLinked else: selOpenKnotUnlinked)
      var kr = PathRecord(selector: sel)
      for i, p in [k.pre, k.anchor, k.post]:
        let y = cast[uint32](f64ToFixed(p[1]))
        let x = cast[uint32](f64ToFixed(p[0]))
        for b in 0..3:
          kr.data[i * 8 + b] = byte((y shr (8 * (3 - b))) and 0xFF)
          kr.data[i * 8 + 4 + b] = byte((x shr (8 * (3 - b))) and 0xFF)
      result.records.add kr

proc parseVectorMask*(data: openArray[byte]): VectorMaskBlock =
  if data.len < 8: raise eof(0, 8 - data.len)
  VectorMaskBlock(version: beU32(data, 0), flags: beU32(data, 4), path: parsePathData(data.toOpenArray(8, data.len - 1)))

proc toBytes*(v: VectorMaskBlock): seq[byte] =
  result.putU32(v.version)
  result.putU32(v.flags)
  result.add v.path.toBytes
