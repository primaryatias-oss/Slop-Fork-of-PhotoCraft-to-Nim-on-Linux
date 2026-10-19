## Fills, strokes and colours (port of `affinity/src/paint.rs`). Two schemas exist: Affinity 1
## (`BFil` fill, `PFil` stroke paint, `LSty` stroke style) and Affinity 2/3 (`BFFl`, `LIFl` and
## `LILn` lists with a current index).

import std/[options, math]
import ./stream, ./types
import ../rmath

const
  MaxStops = 1024 ## gradient stops per gradient
  MaxColorDepth = 64
    ## Spot colours resolve through their `srgb`/`base` colours; a shared colour that refers back
    ## to itself would otherwise recurse without end (the Rust reader overflows its stack there).

proc floatsOf(v: SValue): Option[seq[float64]] =
  case v.kind
  of svFloats: some(v.floats)
  of svBytes:
    if v.bytes.len mod 4 != 0: return
    var o = newSeq[float64](v.bytes.len div 4)
    for i in 0 ..< o.len:
      let u = uint32(v.bytes[4 * i]) or (uint32(v.bytes[4 * i + 1]) shl 8) or
              (uint32(v.bytes[4 * i + 2]) shl 16) or (uint32(v.bytes[4 * i + 3]) shl 24)
      o[i] = float64(cast[float32](u))
    some(o)
  else: none(seq[float64])

proc finite(v: float64): bool {.inline.} = v == v and abs(v) != Inf

proc hsl(h, s, l: float64): (float64, float64, float64) =
  let c = (1.0 - abs(2.0 * l - 1.0)) * s
  let hp = remEuclidF(h, 1.0) * 6.0
  let x = c * (1.0 - abs(hp mod 2.0 - 1.0))
  let sector = if hp != hp or hp < 0.0: 0'u32 elif hp >= 4294967295.0: high(uint32) else: uint32(hp)
  let (r, g, b) = case sector
    of 0: (c, x, 0.0)
    of 1: (x, c, 0.0)
    of 2: (0.0, c, x)
    of 3: (0.0, x, c)
    of 4: (x, 0.0, c)
    else: (c, 0.0, x)
  let m = l - c / 2.0
  (r + m, g + m, b + m)

proc colorAt(r: var Reader; id: ObjId; depth: int): Option[AfColor] =
  let cls = r.s.class(id)
  if cls.isNone: return
  template col(): seq[float64] =
    let f = r.s.field(id, "_col")
    if f.isNone: return
    let v = floatsOf(f.get)
    if v.isNone: return
    for x in v.get:
      if not finite(x): return
    v.get
  template at(v: seq[float64]; i: int): float64 =
    if i >= v.len: return
    v[i]
  case $cls.get
  of "RGBA":
    let v = col()
    return some(AfColor(kind: acRgb, r: at(v, 0), g: at(v, 1), b: at(v, 2), a: clamp(at(v, 3), 0.0, 1.0)))
  of "HSLA":
    let v = col()
    let (r2, g, b) = hsl(at(v, 0), at(v, 1), at(v, 2))
    return some(AfColor(kind: acRgb, r: r2, g: g, b: b, a: clamp(at(v, 3), 0.0, 1.0)))
  of "CMYK":
    let v = col()
    return some(AfColor(kind: acCmyk, c: at(v, 0), mg: at(v, 1), y: at(v, 2), k: at(v, 3), a: clamp(at(v, 4), 0.0, 1.0)))
  of "GRAY":
    let v = col()
    return some(AfColor(kind: acGray, v: at(v, 0), a: clamp(at(v, 1), 0.0, 1.0)))
  of "LABA":
    let bo = r.s.bytesOf(id, "_col")
    if bo.isNone: return
    let b = bo.get
    template u(i: int): float64 =
      if 2 * i + 1 >= b.len: return
      float64(uint16(b[2 * i]) or (uint16(b[2 * i + 1]) shl 8)) / 65535.0
    let (u0, u1, u2, u3) = (u(0), u(1), u(2), u(3))
    return some(AfColor(kind: acLab, l: 100.0 * u0, la: 255.0 * u1 - 128.0, lb: 255.0 * u2 - 128.0, a: u3))
  of "Pant":
    r.warn("spot colours (imported as their RGB equivalent)")
    if depth >= MaxColorDepth: return
    let s = r.s.child(id, "srgb")
    if s.isSome:
      let c = colorAt(r, s.get, depth + 1)
      if c.isSome: return c
    let b = r.s.child(id, "base")
    if b.isSome: return colorAt(r, b.get, depth + 1)
  of "RegC": return some(AfColor(kind: acCmyk, c: 1.0, mg: 1.0, y: 1.0, k: 1.0, a: 1.0))
  else:
    r.warn("colours in an unknown model")
    return none(AfColor)

proc color*(r: var Reader; id: ObjId): Option[AfColor] =
  ## A colour object (`RGBA`, `HSLA`, `CMYK`, `GRAY`, `LABA`, `Pant`).
  colorAt(r, id, 0)

proc descriptor*(r: var Reader; id: ObjId; local: AfAffine): Option[AfPaint] =
  ## A fill descriptor (`FDsc`) or, in Affinity 1 files, a bare `FilS`; `local` maps the node's
  ## space to document pixels.
  var fill = id
  var unit = Identity
  if r.s.isClass(id, "FDsc"):
    let u = r.s.floatsN(id, "FDeX", 6)
    if u.isSome: unit = fromXfrm(u.get)
    let f = r.s.child(id, "FDeF")
    if f.isNone: return
    fill = f.get
  let cls = r.s.class(fill)
  if cls.isNone: return
  case $cls.get
  of "FilN": some(AfPaint(kind: apNone))
  of "FilS":
    let c = r.s.child(fill, "Colr")
    if c.isNone: return
    let col = color(r, c.get)
    if col.isNone: return
    some(AfPaint(kind: apSolid, color: col.get))
  of "FilG":
    let t = r.s.enumeration(fill, "Type")
    var kind = agLinear
    if t.isSome:
      case t.get[0]
      of 0: kind = agLinear
      of 1, 2: kind = agRadial
      of 3: kind = agConical
      else: r.warn("gradients of an unknown kind (imported as linear)")
    let grad = r.s.child(fill, "Grad")
    if grad.isNone: return
    let cols = r.s.children(grad.get, "Cols")
    var posn: seq[seq[float64]]
    let p = r.s.field(grad.get, "Posn")
    if p.isSome and p.get.kind == svArray:
      for v in p.get.arr:
        let f = floatsOf(v)
        if f.isSome: posn.add f.get
    var stops: seq[AfStop]
    for i in 0 ..< min(min(cols.len, posn.len), MaxStops):
      let col = color(r, cols[i])
      if col.isNone: return
      let pv = posn[i]
      let offset = clamp(if pv.len > 0 and finite(pv[0]): pv[0] else: 0.0, 0.0, 1.0)
      let bias = clamp(if pv.len > 1 and finite(pv[1]): pv[1] else: 0.5, 0.0, 1.0)
      stops.add AfStop(offset: offset, color: col.get, bias: bias)
    if stops.len == 0: return some(AfPaint(kind: apNone))
    some(AfPaint(kind: apGradient, gradient: AfGradient(kind: kind, stops: stops, transform: unit.then(local))))
  of "FilB":
    r.warn("bitmap and pattern fills (left empty)")
    some(AfPaint(kind: apNone))
  else:
    r.warn("fills of an unknown kind (left empty)")
    some(AfPaint(kind: apNone))

proc current(r: Reader; id: ObjId; list, index: string): Option[ObjId] =
  let items = r.s.children(id, list)
  let iv = r.s.intOf(id, index)
  let i = if iv.isSome and iv.get >= 0: iv.get else: 0'i64
  if i < items.len: some(items[i]) elif items.len > 0: some(items[0]) else: none(ObjId)

proc fills*(r: var Reader; id: ObjId; local: AfAffine; list: string): seq[AfPaint] =
  ## The fills of `list` (artboard backgrounds use `BFFl`).
  var d = current(r, id, list, "BFCr")
  if d.isNone: d = r.s.child(id, "BFil")
  if d.isNone: return
  let p = descriptor(r, d.get, local)
  if p.isSome and p.get.kind != apNone: result.add p.get

proc stroke(r: var Reader; pen, ldsc: ObjId; local: AfAffine; behind: bool): Option[AfStroke] =
  let pt = descriptor(r, pen, local)
  if pt.isNone or pt.get.kind == apNone: return
  var lsty = ldsc
  if r.s.isClass(ldsc, "LDsc"):
    let l = r.s.child(ldsc, "LDeL")
    if l.isNone: return
    lsty = l.get
  let wv = r.s.f64(lsty, "Wght")
  if wv.isNone or not (wv.get > 0.0 and wv.get < 1e7): return
  let weight = wv.get
  # "Scale with object": the weight follows the object's transform; otherwise it is absolute.
  let scales = r.s.boolean(ldsc, "LDSc").get(true)
  let width = if scales: weight * local.scale() else: weight
  let data = r.s.bytesOf(lsty, "Data").get(@[])
  var miterLimit = 4.0
  if data.len >= 8:
    var u = 0'u64
    for k in countdown(7, 0): u = (u shl 8) or uint64(data[k])
    let m = cast[float64](u)
    if finite(m) and m >= 1.0: miterLimit = m
  # Cap and join bytes are only partly understood: (0, 0) is butt/miter in every clean sample;
  # (2, 2) ends plain lines square and brush lines round; joins import round, which never
  # looks wrong.
  let d8 = if data.len > 8: int(data[8]) else: -1
  let d9 = if data.len > 9: int(data[9]) else: -1
  let d10 = if data.len > 10: int(data[10]) else: -1
  var cap = capRound
  var join = joinRound
  if d8 == 0 and d9 == 0: (cap, join) = (capButt, joinMiter)
  elif d8 == 0 and d9 >= 0: (cap, join) = (capButt, joinRound)
  elif d8 == 2 and d9 == 2 and d10 == 3: (cap, join) = (capRound, joinRound)
  elif d8 == 2 and d9 == 2: (cap, join) = (capSquare, joinRound)
  let align = case r.s.intOf(ldsc, "LDSa").get(-1)
    of 1: strokeInside
    of 2: strokeOutside
    else: strokeCenter
  var dash = none((seq[float64], float64))
  let patn = r.s.field(lsty, "Patn")
  if patn.isSome and patn.get.kind == svArray and (d10 == 2 or patn.get.arr.len > 0):
    var v: seq[float64]
    for x in patn.get.arr:
      if x.kind == svFloat: v.add x.f
    while v.len >= 2 and v[^1] == 0.0 and v[^2] == 0.0: v.setLen(v.len - 2)
    var valid = v.len > 0
    var anyPositive = false
    for x in v:
      if not (finite(x) and x >= 0.0): valid = false
      if x > 0.0: anyPositive = true
    if valid and anyPositive:
      var lengths: seq[float64]
      for x in v: lengths.add x * width
      dash = some((lengths, r.s.f64(lsty, "Phse").get(0.0) * width))
  if r.s.child(lsty, "Brus").isSome or r.s.child(ldsc, "LDeP").isSome:
    r.warn("brush and pressure strokes (imported as plain strokes)")
  some(AfStroke(paint: pt.get, width: width, cap: cap, join: join, miterLimit: miterLimit, align: align,
                dash: dash, behind: behind))

proc nodePaint*(r: var Reader; id: ObjId; local: AfAffine): (seq[AfPaint], seq[AfStroke]) =
  ## A node's fill and stroke. `local` maps the node's space to document pixels.
  let fl = fills(r, id, local, "BFFl")
  if r.s.children(id, "BFFl").len > 1 or r.s.children(id, "LIFl").len > 1:
    r.warn("several fills or strokes on one object (only the active one is imported)")
  let fo = r.s.f64(id, "FOpc")
  if fo.isSome and fo.get < 0.999:
    r.warn("fill opacity (imported at full fill opacity)")
  if r.s.child(id, "Trns").isSome:
    r.warn("transparency gradients (imported without them)")
  var pen, style: Option[ObjId]
  if r.s.hasField(id, "LIFl"):
    pen = current(r, id, "LIFl", "LICr")
    style = current(r, id, "LILn", "LICr")
  else:
    pen = r.s.child(id, "PFil")
    style = r.s.child(id, "LSty")
  let dr = r.s.field(id, "DrwO")
  let behind = dr.isSome and dr.get.kind == svArray and dr.get.arr.len > 0 and
    dr.get.arr[0] == SValue(kind: svEnum, enumId: 1, enumVersion: 0)
  var strokes: seq[AfStroke]
  if pen.isSome and style.isSome:
    let s = stroke(r, pen.get, style.get, local, behind)
    if s.isSome: strokes.add s.get
  (fl, strokes)
