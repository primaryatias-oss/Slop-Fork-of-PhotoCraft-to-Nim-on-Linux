## Writes synthetic Affinity documents for the reader cross-check (`afgen DIR [MUTATIONS [SEED]]`): every
## parametric shape, curves with handles and live corners, compounds, every colour model and fill
## kind, strokes, artistic and frame text, pixel layers in every tile format and state, placed and
## embedded images, masks, fill layers and the document structure (pages, artboards, blends,
## nesting, cycles). Each document is also written with its stream mutated at random, so the Rust
## oracle and the Nim reader are compared on damaged input too. Not an Affinity exporter.

import std/[os, random, math, strutils]
import photocraft/affinity/[stream, synth]

var rng = initRand(0xAF1)
var nextId = 1000'u32

proc nid(): uint32 =
  inc nextId
  nextId

proc le(v: uint64; n: int): seq[byte] =
  for k in 0 ..< n: result.add byte((v shr (8 * k)) and 0xFF)
proc lef(v: float64): seq[byte] = le(cast[uint64](v), 8)
proc lef32(v: float32): seq[byte] = le(uint64(cast[uint32](v)), 4)
proc tagBytes(t: string): seq[byte] = le(uint64(uint32(tag(t))), 4)

proc fld(t: string; f: F): (Tag, F) = (tag(t), f)
proc def(class: string; fields: seq[(Tag, F)]): F = FDef(nid(), @[tag(class)], fields)
proc defChain(chain: seq[string]; fields: seq[(Tag, F)]): F =
  var c: seq[Tag]
  for x in chain: c.add tag(x)
  FDef(nid(), c, fields)

proc raw(t: string; code: uint8; payload: seq[byte]): (Tag, F) = (tag(t), FRaw(@[code] & tagBytes(t) & payload))
proc f64s(t: string; v: seq[float64]): (Tag, F) =
  var p = le(uint64(v.len), 4)
  for x in v: p.add lef(x)
  raw(t, 0x8a, p)
proc i32s(t: string; v: seq[int32]): (Tag, F) =
  var p = le(uint64(v.len), 4)
  for x in v: p.add le(uint64(cast[uint32](x)), 4)
  raw(t, 0x87, p)
proc f32vecs(t: string; vecs: seq[seq[float32]]): (Tag, F) =
  ## An array of f32 vectors of one arity (2..6).
  let arity = if vecs.len > 0: vecs[0].len else: 2
  var p = le(uint64(vecs.len), 4)
  for v in vecs:
    for x in v: p.add lef32(x)
  raw(t, 0x80 or uint8(0x1d + arity), p)
proc enums(t: string; ids: seq[uint16]; version: uint16): (Tag, F) =
  var p = le(uint64(ids.len), 4) & le(version, 2)
  for x in ids: p.add le(x, 2)
  raw(t, 0xAA, p)
proc ints4(t: string; v: array[4, int32]): (Tag, F) =
  var p: seq[byte]
  for x in v: p.add le(uint64(cast[uint32](x)), 4)
  raw(t, 0x17, p)
proc blob(t: string; data: seq[byte]): (Tag, F) = raw(t, 0x2d, le(uint64(data.len), 4) & data)
proc i64f(t: string; v: int64): (Tag, F) = raw(t, 0x08, le(cast[uint64](v), 8))
proc u64f(t: string; v: uint64): (Tag, F) = raw(t, 0x04, le(v, 8))

proc toStr(b: seq[byte]): string =
  result = newString(b.len)
  for i, x in b: result[i] = char(x)

proc rnd(lo, hi: float64): float64 = lo + rng.rand(1.0) * (hi - lo)
proc pick[T](v: openArray[T]): T = v[rng.rand(v.high)]

proc hostile(): float64 =
  pick([0.0, -0.0, 1.0, -1.0, 0.5, 1e-300, 1e300, -1e300, 1e17, -1e17, 3.0, 7.5, 100.0, -5.0, NaN, Inf, -Inf, 0.999, 1.001])

proc anyNum(t: string; v: float64): (Tag, F) =
  ## A numeric field of a random stream type.
  case rng.rand(4)
  of 0: fld(t, FF64(v))
  of 1: fld(t, FF32(float32(v)))
  of 2: fld(t, FU32(uint32(clamp(abs(if v == v: v else: 0.0), 0.0, 4e9))))
  of 3: fld(t, FI32(int32(clamp(if v == v: v else: 0.0, -2e9, 2e9))))
  else: fld(t, FStr("x"))

proc rgbaCol(r, g, b, a: float32): F = FStruct(lef32(r) & lef32(g) & lef32(b) & lef32(a))

proc colorObj(): F =
  case rng.rand(11)
  of 0, 1, 2: def("RGBA", @[fld("_col", rgbaCol(rng.rand(1.0).float32, rng.rand(1.0).float32, rng.rand(1.0).float32, rnd(-0.2, 1.2).float32))])
  of 3: def("RGBA", @[fld("_col", FF64s([0.2, 0.4, 0.6, 0.8]))])
  of 4: def("HSLA", @[fld("_col", rgbaCol(rnd(-2, 2).float32, rng.rand(1.0).float32, rng.rand(1.0).float32, 1))])
  of 5: def("CMYK", @[fld("_col", FStruct(lef32(0.1) & lef32(0.2) & lef32(0.3) & lef32(0.4) & lef32(rnd(0, 1.5).float32)))])
  of 6: def("GRAY", @[fld("_col", FStruct(lef32(rng.rand(1.0).float32) & lef32(1)))])
  of 7:
    var b: seq[byte]
    for _ in 0 ..< 4: b.add le(uint64(rng.rand(65535)), 2)
    def("LABA", @[fld("_col", FStruct(b))])
  of 8: def("Pant", @[fld("srgb", def("RGBA", @[fld("_col", rgbaCol(1, 0.5, 0, 1))])), fld("base", def("CMYK", @[fld("_col", rgbaCol(0, 0, 0, 1))]))])
  of 9: def("RegC", @[])
  of 10: def("RGBA", @[fld("_col", FStruct(lef32(NaN) & lef32(0) & lef32(0) & lef32(1)))])
  else: def("XYZC", @[])

proc fillObj(): F =
  case rng.rand(9)
  of 0: def("FilN", @[])
  of 1, 2, 3: def("FilS", @[fld("Colr", colorObj())])
  of 4, 5, 6:
    var cols: seq[F]
    var posn: seq[seq[float32]]
    for _ in 0 ..< 1 + rng.rand(3):
      cols.add colorObj()
      posn.add @[rng.rand(1.0).float32, rnd(-0.5, 1.5).float32]
    var fields = @[fld("Grad", def("GrdA", @[fld("Cols", FShared(cols)), f32vecs("Posn", posn)]))]
    if rng.rand(3) > 0: fields.add fld("Type", FEnum(uint16(rng.rand(5)), 0))
    def("FilG", fields)
  of 7: def("FilB", @[])
  of 8: def("FilS", @[])
  else: def("FilQ", @[])

proc descriptor(): F =
  if rng.rand(4) == 0: return def("FilS", @[fld("Colr", colorObj())])
  var fields = @[fld("FDeF", fillObj())]
  if rng.rand(1) == 0: fields.add fld("FDeX", FF64s([rnd(-2, 2), rnd(-1, 1), rnd(-50, 50), rnd(-1, 1), rnd(-2, 2), rnd(-50, 50)]))
  def("FDsc", fields)

proc lineStyle(): F =
  var fields: seq[(Tag, F)]
  if rng.rand(5) > 0: fields.add anyNum("Wght", pick([1.0, 4.0, 0.5, 0.0, -1.0, 2e7, 12.25]))
  if rng.rand(1) == 0:
    var data = lef(pick([4.0, 0.5, 10.0, NaN]))
    data.add byte(pick([0, 2, 1]))
    data.add byte(pick([0, 2, 1]))
    data.add byte(pick([0, 2, 3]))
    fields.add fld("Data", FStruct(data))
  if rng.rand(2) == 0:
    var v: seq[float64]
    for _ in 0 ..< rng.rand(5): v.add pick([0.0, 1.0, 2.0, 3.5, -1.0])
    fields.add f64s("Patn", v)
    fields.add fld("Phse", FF64(rnd(0, 3)))
  if rng.rand(6) == 0: fields.add fld("Brus", def("Brsh", @[]))
  def("LSty", fields)

proc strokeFields(): seq[(Tag, F)] =
  case rng.rand(3)
  of 0:
    var ldsc = @[fld("LDeL", lineStyle())]
    if rng.rand(1) == 0: ldsc.add fld("LDSc", FBool(rng.rand(1) == 0))
    if rng.rand(1) == 0: ldsc.add fld("LDSa", FU32(uint32(rng.rand(3))))
    if rng.rand(6) == 0: ldsc.add fld("LDeP", def("Pres", @[]))
    result.add fld("LIFl", FShared(@[descriptor()]))
    result.add fld("LILn", FShared(@[def("LDsc", ldsc)]))
    if rng.rand(3) == 0: result.add fld("LICr", FU32(uint32(rng.rand(2))))
  of 1:
    result.add fld("PFil", def("FilS", @[fld("Colr", colorObj())]))
    result.add fld("LSty", lineStyle())
  of 2:
    result.add fld("LIFl", FShared(@[descriptor(), descriptor()]))
    result.add fld("LILn", FShared(@[lineStyle()]))
  else: discard
  if rng.rand(4) == 0: result.add enums("DrwO", @[uint16(rng.rand(1))], 0)

proc fillFields(): seq[(Tag, F)] =
  case rng.rand(3)
  of 0: result.add fld("BFFl", FShared(@[descriptor()]))
  of 1:
    result.add fld("BFFl", FShared(@[descriptor(), descriptor()]))
    result.add fld("BFCr", FI32(int32(rng.rand(3) - 1)))
  of 2: result.add fld("BFil", descriptor())
  else: discard
  if rng.rand(4) == 0: result.add fld("FOpc", FF64(rnd(0, 1)))
  if rng.rand(6) == 0: result.add fld("Trns", def("Trns", @[]))

proc xfrm(): (Tag, F) =
  case rng.rand(5)
  of 0: fld("Xfrm", FF64s([1.0, 0.0, 0.0, 0.0, 1.0, 0.0]))
  of 1:
    let a = rnd(-PI, PI)
    let k = rnd(0.2, 3)
    fld("Xfrm", FF64s([k * cos(a), -k * sin(a), rnd(-100, 100), k * sin(a), k * cos(a), rnd(-100, 100)]))
  of 2: fld("Xfrm", FF64s([rnd(-3, 3), rnd(-1, 1), rnd(-50, 50), rnd(-1, 1), rnd(-3, 3), rnd(-50, 50)]))
  of 3: fld("Xfrm", FF64s([2.0, 0.0, 10.0, 0.0, 0.5, 20.0]))
  of 4: fld("Xfrm", FF64s([1.0, 0.0, NaN, 0.0, 1.0, 0.0]))
  else: fld("Xfrm", FF64s([1.0, 0.0]))

proc nodeCommon(): seq[(Tag, F)] =
  if rng.rand(1) == 0: result.add fld("Desc", FStr(pick(["Layer", "Ünïcødé", "", "with space", "tab\tname"])))
  if rng.rand(5) == 0: result.add fld("Visi", FBool(rng.rand(1) == 0))
  if rng.rand(5) == 0: result.add fld("Edtb", FBool(rng.rand(1) == 0))
  if rng.rand(3) == 0: result.add fld("Opac", FF64(rnd(-0.5, 1.5)))
  if rng.rand(4) == 0:
    let pairs = [(0, 0), (1, 0), (2, 1), (2, 0), (3, 0), (4, 0), (6, 1), (5, 0), (6, 0), (7, 0), (8, 0), (9, 0), (10, 0),
                 (11, 0), (12, 0), (15, 1), (13, 0), (14, 0), (15, 0), (16, 0), (17, 0), (18, 0), (19, 0), (20, 0),
                 (21, 0), (22, 0), (23, 0), (24, 0), (25, 0), (1, 1), (26, 0), (3, 2)]
    let (i, v) = pick(pairs)
    result.add fld("Blnd", FEnum(uint16(i), uint16(v)))
  if rng.rand(5) == 0: result.add xfrm()
  if rng.rand(8) == 0: result.add fld("FiEf", FShared(@[def("FxDS", @[fld("Enab", FBool(rng.rand(1) == 0))])]))

const ShapeClasses = ["ShNR", "ShRR", "ShpE", "ShPy", "ShSt", "ShSS", "ShPi", "ShpT", "ShTz", "ShCl", "ShHt", "ShCg",
                      "ShCR", "ShCE", "ShDA", "ShDS", "ShTr", "ShCr", "ShpD", "ShSg", "ShXX"]
const ShapeParams = ["Side", "Pnts", "IRad", "COut", "AngS", "AngE", "Pos ", "PosL", "PosR", "Bubl", "Teth", "TtSz",
                     "NtSz", "Hole", "TlHg", "TlWd", "TlRP", "TlEP", "TlAn", "Thck", "PRad", "Ball", "Bend", "Angl",
                     "Pos0", "Pos1", "Curv", "CrcI", "CrcO", "CrvL", "CrvR"]

proc shapeObj(class: string): F =
  var fields: seq[(Tag, F)]
  for p in ShapeParams:
    if rng.rand(2) == 0:
      let v = if rng.rand(4) == 0: hostile() else: rnd(-1, 13)
      fields.add anyNum(p, v)
      if rng.rand(6) == 0: fields.add anyNum(p, rnd(0, 8)) # a stale block repeats the tag
  if class in ["ShNR", "ShRR"]:
    if rng.rand(3) > 0:
      var ids: seq[uint16]
      for _ in 0 ..< 4: ids.add pick([0'u16, 4, 1, 2])
      fields.add enums("CTyp", ids, 0)
    if rng.rand(3) > 0: fields.add fld("ShCR", FF64s([rnd(-0.1, 0.6), 0.1, 0.1, 0.1]))
    if rng.rand(2) == 0: fields.add fld("AbSz", FBool(rng.rand(1) == 0))
  if class == "ShPy" and rng.rand(1) == 0: fields.add fld("Smth", FBool(true))
  def(class, fields)

proc box(): seq[float64] =
  case rng.rand(5)
  of 0: @[0.0, 0.0, 0.0, 0.0]
  of 1: @[50.0, 50.0, 10.0, 10.0]
  else: @[rnd(-50, 50), rnd(-50, 50), rnd(60, 200), rnd(60, 200)]

proc shapeNode(): F =
  let class = pick(ShapeClasses)
  var fields = nodeCommon() & fillFields() & strokeFields()
  if rng.rand(6) == 0:
    fields.add fld("ShpM", def("ShpN", @[fld("Shpe", shapeObj(class)), fld("ShpB", FF64s(box()))]))
  else:
    fields.add fld("Shpe", shapeObj(class))
    if rng.rand(8) > 0: fields.add fld("ShpB", FF64s(box()))
  if rng.rand(10) == 0: fields.add fld("ABEn", FBool(true))
  def(pick(["ShpN", "ShpN", "ShpN", "SNEN", "SNRR", "ShRN"]), fields)

proc rec(x, y: float64; role: int): seq[byte] = lef(x) & lef(y) & @[1'u8, byte(role)]

proc curveData(): (Tag, F) =
  var items = @[FU8(0)]
  let n = 1 + rng.rand(2)
  items.add FU32(uint32(n))
  for _ in 0 ..< n:
    items.add FBool(rng.rand(1) == 0)
    var recs: seq[seq[byte]]
    let pts = rng.rand(6)
    let (x0, y0) = (rnd(0, 100), rnd(0, 100))
    for i in 0 ..< pts:
      let (x, y) = (rnd(0, 150), rnd(0, 150))
      if i == 0 and rng.rand(3) == 0: recs.add rec(x0 - 5, y0, 1) # a leading out-handle
      if rng.rand(2) == 0: recs.add rec(x - rnd(0, 10), y, 2)
      recs.add rec(x, y, 0)
      if rng.rand(2) == 0: recs.add rec(x + rnd(0, 10), y, 1)
    if pts > 1 and rng.rand(1) == 0: recs.add recs[0] # repeat the first anchor (closed)
    if rng.rand(15) == 0: recs.add rec(NaN, 0, 0)
    if rng.rand(15) == 0: recs.add rec(0, 0, 3)
    items.add FRecords(18, recs)
  if rng.rand(12) == 0: items = items[0 ..< max(1, items.len - 1)]
  fld("Data", FPos(items))

proc cornerData(): (Tag, F) =
  var p = @[0x01'u8, 0]
  let n = rng.rand(4)
  p.add 0x87
  p.add le(uint64(n), 4)
  for _ in 0 ..< n: p.add le(uint64(cast[uint32](int32(rng.rand(10) - 1))), 4)
  p.add 0x8a
  p.add le(uint64(n), 4)
  for _ in 0 ..< n: p.add lef(pick([5.0, 0.0, -1.0, 12.5, 100.0, Inf]))
  p.add 0x81
  p.add le(uint64(n), 4)
  for _ in 0 ..< n: p.add 0
  p.add 0
  raw("CnrD", 0x30, p)

proc curvesObj(): F =
  var fields = @[curveData()]
  if rng.rand(2) == 0: fields.add cornerData()
  FObj(tag("PCvD"), fields)

proc curveNode(): F =
  var fields = nodeCommon() & fillFields() & strokeFields()
  if rng.rand(6) == 0: fields.add fld("MCrM", def("MCrv", @[fld("Crvs", curvesObj())]))
  else: fields.add fld("Crvs", curvesObj())
  if rng.rand(12) == 0: fields.add fld("phrp", def("aprp", @[]))
  if rng.rand(12) == 0: fields.add fld("ShpB", FF64s([0.0, 0.0, 30.0, 40.0]))
  def("PCrv", fields)

proc compoundNode(depth = 0): F =
  var kids: seq[F]
  for _ in 0 ..< 1 + rng.rand(3):
    var k = if rng.rand(1) == 0: curveNode() elif depth < 2 and rng.rand(3) == 0: compoundNode(depth + 1) else: shapeNode()
    kids.add k
  var fields = nodeCommon() & fillFields() & strokeFields()
  fields.add fld("Chld", FShared(kids))
  def("Comp", fields)

proc textNode(): F =
  var blocks: seq[F]
  for _ in 0 ..< 1 + rng.rand(2):
    var glyp: seq[(Tag, F)]
    var length = 0
    if rng.rand(3) > 0:
      let t = pick(["Hello", "Ünï line two", "a\0b", "", "￼field", "abcdefghij"])
      glyp.add fld("Utf8", FStr(t))
      length = t.len
    else:
      var segs: seq[F]
      for _ in 0 ..< 1 + rng.rand(2):
        var sf = @[fld("Utf8", FStr(pick(["seg", "Page ", "x"])))]
        if rng.rand(1) == 0: sf.add fld("Glys", FShared(@[def("InOb", @[])]))
        segs.add def("MxSg", sf)
      glyp.add fld("Mixd", FShared(segs))
      length = 8
    var runs: seq[F]
    var at = 0
    for _ in 0 ..< 1 + rng.rand(2):
      at += rng.rand(length + 2)
      var rf: seq[(Tag, F)]
      if rng.rand(12) > 0: rf.add fld("Indx", (if rng.rand(15) == 0: FI32(-1) else: FU32(uint32(at))))
      if rng.rand(6) > 0:
        var attrs: seq[(Tag, F)]
        var d: seq[float64]
        for i in 0 ..< 14: d.add(if i == 0: pick([24.0, 0.0, 2e6, 9.5]) elif i == 10: pick([1.0, 1.5]) else: rnd(0, 30))
        if rng.rand(3) > 0: attrs.add f64s("Doub", d[0 ..< rng.rand(14)])
        if rng.rand(1) == 0: attrs.add i32s("Ints", @[0'i32, 0, 0, 0, int32(rng.rand(2))])
        var objs: seq[F]
        if rng.rand(3) > 0: objs.add descriptor()
        if rng.rand(2) == 0: objs.add(if rng.rand(1) == 0: descriptor() else: FNull())
        if objs.len > 0: attrs.add fld("Objs", FShared(objs))
        if rng.rand(3) > 0:
          var font = @[fld("Post", FStr("Font-Bold")), fld("Famy", FStr("Font"))]
          if rng.rand(1) == 0: font.add fld("Wegt", FU32(uint32(pick([400, 700, 100]))))
          if rng.rand(1) == 0: font.add fld("Ital", FBool(true))
          attrs.add fld("DFnt", def("Font", font))
        rf.add fld("Item", def("CAtt", attrs))
      runs.add def("Run_", rf)
    var bf = @[fld("Glyp", def("Glyp", glyp)), fld("GAtt", def("GAtt", @[fld("Runs", FShared(runs))]))]
    if rng.rand(1) == 0:
      bf.add fld("PAtt", def("PAtt", @[fld("Runs", FShared(@[def("Run_", @[fld("Item", def("PItm", @[i32s("Ints", @[int32(rng.rand(5))])]))])]))]))
    if rng.rand(12) == 0: bf = bf[1 .. ^1]
    blocks.add def("Blok", bf)
  var frame = @[fld("FrmB", FF64s([rnd(-10, 10), rnd(-10, 10), rnd(20, 200), rnd(20, 200)]))]
  if rng.rand(1) == 0: frame.add fld("ArtV", FF64(rnd(0, 30)))
  if rng.rand(3) == 0: frame.add f64s("ColW", @[10.0, 20.0])
  var fields = nodeCommon()
  fields.add fld("StSt", def("Stry", @[fld("Blok", FShared(blocks))]))
  if rng.rand(10) > 0: fields.add fld("TxtH", def(pick(["ArFr", "FrFr"]), frame))
  def(pick(["TxtA", "TxtF", "TxtC"]), fields)

type Doc = object
  entries: seq[(string, seq[byte])]

proc tileData(kind: int): seq[byte] =
  result = newSeq[byte](256 * 256)
  for i in 0 ..< result.len: result[i] = byte((i * 7 + kind * 31) and 0xFF)

proc bitmapObj(d: var Doc; w, h, format, channels: int; extra: seq[(Tag, F)] = @[]): F =
  var fields = extra & @[fld("BmpW", FU32(uint32(w))), fld("BmpH", FU32(uint32(h))), fld("Frmt", FEnum(uint16(format), 0))]
  let bps = if format in [1, 7]: 2 else: 1
  for c in 1 .. channels:
    let tw = (w * bps + 255) div 256
    let th = (h + 255) div 256
    var states: seq[byte]
    var idx: seq[F]
    for t in 0 ..< tw * th:
      let st = pick([4, 4, 4, 0, 2, 1, 3, 5, 4, 9])
      states.add byte(st)
      if st == 4:
        let name = "t/" & $nid()
        var data = tileData(c + t)
        if rng.rand(5) == 0: data = @[0x00'u8, 0xFF, byte('K'), byte('S')] & newSeq[byte](17) & data & @[0'u8]
        if rng.rand(30) == 0: data = data[0 ..< 100]
        d.entries.add (name, data)
        if rng.rand(30) > 0: idx.add def("Blck", @[fld("Data", FEntry(name))])
    fields.add fld("Sta" & $c, FU8s(states))
    fields.add fld("Idx" & $c, FShared(idx))
    if rng.rand(6) == 0: fields.add fld("TWi" & $c, FU32(uint32(tw)))
  def("DyBm", fields)

proc placedStream(): seq[byte] =
  synth.stream(@[blob("Data", @[0x89'u8, byte('P'), byte('N'), byte('G'), 1, 2, 3])])

proc rasterNode(d: var Doc): F =
  let format = pick([0, 0, 1, 6, 7, 4, 2])
  let channels = case format
    of 0, 1: 4
    of 6, 7: 1
    of 4: 5
    else: 4
  let (w, h) = (1 + rng.rand(300), 1 + rng.rand(300))
  var extra: seq[(Tag, F)]
  if rng.rand(2) == 0:
    let name = "c/" & $nid()
    d.entries.add (name, (if rng.rand(5) == 0: @[1'u8, 2, 3] else: placedStream()))
    extra.add fld("Bckg", FEntry(name))
  var bm = bitmapObj(d, w, h, format, channels, extra)
  var fields = nodeCommon()
  let class = pick(["Rstr", "ImgN"])
  if rng.rand(2) == 0: fields.add ints4("BitR", [int32(rng.rand(50)), int32(rng.rand(50)), int32(rng.rand(400)), int32(rng.rand(400))])
  if rng.rand(15) == 0: fields.add ints4("BitR", [low(int32), 0, 0, 0])
  if rng.rand(12) == 0: bm = def("XXBm", @[])
  if rng.rand(15) > 0: fields.add fld("Bitm", bm)
  def(class, fields)

proc maskNode(d: var Doc): F =
  let format = pick([6, 7, 0])
  var fields = @[fld("Bitm", bitmapObj(d, 1 + rng.rand(100), 1 + rng.rand(100), format, 1))]
  if rng.rand(3) == 0: fields.add xfrm()
  if rng.rand(5) == 0: fields.add fld("Visi", FBool(false))
  def("MRst", fields)

proc embeddedNode(d: var Doc): F =
  var cache = bitmapObj(d, 1 + rng.rand(40), 1 + rng.rand(40), 0, 4)
  var e = @[fld("Cach", cache)]
  if rng.rand(5) > 0: e.add fld("ChBB", FF64s([rnd(-20, 0), rnd(-20, 0), rnd(10, 80), rnd(10, 80)]))
  def("EmbN", nodeCommon() & @[fld("Bitm", def("EmbB", e))])

proc fillLayer(): F =
  var fields = nodeCommon() & fillFields()
  if rng.rand(4) > 0: fields.add fld("Bitm", def("DyBm", @[fld("BmpW", FU32(uint32(rng.rand(200)))), fld("BmpH", FI32(int32(rng.rand(200))))]))
  def("FRst", fields)

proc anyNode(d: var Doc; depth: int): F

proc groupNode(d: var Doc; depth: int): F =
  var kids: seq[F]
  if depth < 4:
    for _ in 0 ..< rng.rand(3): kids.add anyNode(d, depth + 1)
  var fields = nodeCommon()
  if rng.rand(3) == 0: fields.add fld("PasT", FBool(rng.rand(1) == 0))
  if rng.rand(5) == 0: kids.add maskNode(d)
  fields.add fld("Chld", FShared(kids))
  if rng.rand(4) == 0:
    var att: seq[F]
    for _ in 0 ..< 1 + rng.rand(2):
      att.add(case rng.rand(3)
        of 0: curveNode()
        of 1: shapeNode()
        of 2: maskNode(d)
        else: def("HSLA", @[]))
    fields.add fld("AdCh", FShared(att))
  def(pick(["Scop", "Grup"]), fields)

proc artboardNode(d: var Doc; depth: int): F =
  var kids: seq[F]
  for _ in 0 ..< rng.rand(3): kids.add anyNode(d, depth + 1)
  var fields = nodeCommon() & fillFields()
  fields.add fld("ABEn", FBool(true))
  fields.add fld("Shpe", shapeObj("ShNR"))
  fields.add fld("ShpB", FF64s([0.0, 0.0, rnd(10, 300), rnd(10, 300)]))
  fields.add fld("Chld", FShared(kids))
  def(pick(["ShpN", "ShpN", "PCrv"]), fields)

proc anyNode(d: var Doc; depth: int): F =
  case rng.rand(14)
  of 0, 1, 2: shapeNode()
  of 3, 4: curveNode()
  of 5: compoundNode()
  of 6, 7: textNode()
  of 8: rasterNode(d)
  of 9: embeddedNode(d)
  of 10: fillLayer()
  of 11, 12: groupNode(d, depth)
  of 13: artboardNode(d, depth)
  else: def(pick(["Slic", "3DLA", "HSRA", "MPIN", "Zzzz"]), nodeCommon())

proc spreadObj(d: var Doc; content: seq[F]): F =
  var fields: seq[(Tag, F)]
  case rng.rand(4)
  of 0: fields.add fld("SprB", FF64s([0.0, 0.0, rnd(10, 600), rnd(10, 600)]))
  of 1:
    fields.add fld("SprB", FF64s([0.0, 0.0, 400.0, 300.0]))
    fields.add fld("PagC", FU32(uint32(rng.rand(4))))
  of 2:
    var pages: seq[F]
    for _ in 0 ..< rng.rand(3):
      pages.add def("PgRc", @[fld("rctp", FF64s([rnd(0, 100), rnd(0, 100), rnd(0, 300), rnd(0, 300)]))])
    fields.add fld("SpMd", def("SpMd", @[fld("PagR", FShared(pages))]))
  else: discard
  if rng.rand(1) == 0: fields.add fld("SprT", FBool(rng.rand(1) == 0))
  var kids = content
  if rng.rand(6) == 0: kids.add maskNode(d)
  fields.add fld("Chld", FShared(kids))
  def("Sprd", fields)

proc document(d: var Doc; content: seq[F]): seq[byte] =
  var root: seq[(Tag, F)]
  if rng.rand(1) == 0: root.add fld("UVCn", FObj(tag("UVCn"), @[anyNum("UPPI", pick([72.0, 144.0, 300.0, -1.0, 1e6]))]))
  if rng.rand(1) == 0:
    var v = @[fld("Prod", FStr("Affinity Designer"))]
    if rng.rand(2) > 0: v &= @[fld("Majr", FU32(2)), fld("Minr", FU32(4)), fld("Bild", FI32(1))]
    root.add fld("NVer", def("NVer", v))
  var spreads = @[spreadObj(d, content)]
  if rng.rand(5) == 0: spreads.add spreadObj(d, @[shapeNode()])
  if rng.rand(12) == 0: spreads.add def("Mstr", @[])
  var docFields = @[fld("Chld", FShared(spreads))]
  if rng.rand(5) == 0: docFields.add fld("DfSz", FF64s([300.0, 200.0]))
  if rng.rand(8) == 0: docFields.add fld("MpCh", FShared(@[def("MstS", @[])]))
  root.add fld("DocR", def("DocN", docFields))
  synth.stream(root)

proc pack(d: Doc; docdat: seq[byte]; m: Method): seq[byte] =
  var e = @[("doc.dat", docdat, m)]
  for (n, b) in d.entries: e.add (n, b, pick([mStored, mZlib, mZstd]))
  container(e)

proc special(kind: int): seq[F] =
  ## Hand-made structures the random generator rarely reaches.
  case kind
  of 0: # a node that contains itself
    @[FDef(77, @[tag("Grup")], @[fld("Chld", FShared(@[FRef(77)]))])]
  of 1: # nesting deeper than 128 levels
    var n = def("ShpN", @[fld("Shpe", shapeObj("ShpE")), fld("ShpB", FF64s([0.0, 0.0, 9.0, 9.0]))])
    for _ in 0 ..< 140: n = def("Grup", @[fld("Chld", FShared(@[n]))])
    @[n]
  of 2: # a compound listing itself among its operands
    @[FDef(78, @[tag("Comp")], @[fld("Chld", FShared(@[FRef(78), curveNode()]))])]
  else: @[]

when isMainModule:
  let dir = paramStr(1)
  let mutations = if paramCount() >= 2: parseInt(paramStr(2)) else: 8
  if paramCount() >= 3: rng = initRand(parseInt(paramStr(3)))
  createDir(dir)
  var count = 0
  for k in 0 ..< 400 + 3:
    var d = Doc()
    var content: seq[F]
    if k >= 400: content = special(k - 400)
    else:
      for _ in 0 ..< 1 + rng.rand(4): content.add anyNode(d, 0)
    let docdat = document(d, content)
    writeFile(dir / ("doc" & $k & ".af"), toStr(pack(d, docdat, pick([mStored, mZlib, mZstd]))))
    inc count
    for m in 0 ..< mutations:
      var s = docdat
      for _ in 0 ..< 1 + rng.rand(6):
        let i = rng.rand(s.high)
        s[i] = if rng.rand(1) == 0: byte(rng.rand(255)) else: s[i] xor byte(1 shl rng.rand(7))
      if rng.rand(5) == 0: s.setLen(rng.rand(s.len))
      writeFile(dir / ("doc" & $k & "m" & $m & ".af"), toStr(pack(d, s, mStored)))
      inc count
  echo count, " files"
