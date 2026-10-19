## Outlines of curves (`PCvD` node lists), parametric shapes (`Shpe` objects) and compounds, in
## node space (port of `affinity/src/geometry.rs`).

import std/options
import ./stream, ./types, ./shapes
import ../rmath

const
  MaxNodes = 4_000_000 ## nodes per curve object
  MaxCompoundDepth = 384
    ## Compounds nested inside compounds; a shared compound listed among its own operands would
    ## otherwise recurse without end (the Rust reader overflows its stack there).

type Anchor = object
  p: AfPoint
  inh, outh: Option[AfPoint]
  record: int ## index of the anchor's record (live corners refer to records)

proc f64le(b: openArray[byte]; at: int): float64 =
  var v = 0'u64
  for k in countdown(7, 0): v = (v shl 8) or uint64(b[at + k])
  cast[float64](v)

proc record(v: SValue): Option[(AfPoint, uint8)] =
  if v.kind != svBytes or v.bytes.len != 18: return
  let x = f64le(v.bytes, 0)
  let y = f64le(v.bytes, 8)
  if not (x == x and abs(x) != Inf and y == y and abs(y) != Inf): return
  some((pt(x, y), v.bytes[17]))

proc anchors(records: openArray[SValue]): Option[seq[Anchor]] =
  var o = newSeqOfCap[Anchor](records.len div 3 + 1)
  var pendingIn = none(AfPoint)
  for i, v in records:
    let rec = record(v)
    if rec.isNone: return
    let (p, role) = rec.get
    case role
    of 0:
      o.add Anchor(p: p, inh: pendingIn, record: i)
      pendingIn = none(AfPoint)
    of 1:
      # A leading out-handle without an anchor is edit state only.
      if o.len > 0: o[^1].outh = some(p)
    of 2: pendingIn = some(p)
    else: return
  some(o)

proc subpath(anchors: openArray[Anchor]; closed: bool; radii: Option[seq[(int, float64)]]): Option[AfSubPath] =
  if anchors.len == 0: return
  let first = anchors[0]
  if radii.isSome and radii.get.len > 0:
    let rs = radii.get
    var polyline = true
    for a in anchors:
      if (a.inh.isSome and a.inh.get != a.p) or (a.outh.isSome and a.outh.get != a.p): polyline = false
    if polyline:
      var pts: seq[(AfPoint, float64)]
      for a in anchors:
        var c = 0.0
        for (rec, r) in rs:
          if rec == a.record: c = rmax(c, r)
        pts.add (a.p, c)
      # A closed subpath repeats its first anchor; its radius may be on either copy.
      if closed and pts.len > 1 and pts[0][0] == pts[^1][0]:
        let r = pts.pop()[1]
        pts[0][1] = rmax(pts[0][1], r)
      return some(fillet(pts, closed))
  var s = AfSubPath(start: first.p, closed: closed)
  for i in 0 ..< anchors.len - 1:
    let (a, b) = (anchors[i], anchors[i + 1])
    s.segments.add [(if a.outh.isSome: a.outh.get else: a.p), (if b.inh.isSome: b.inh.get else: b.p), b.p]
  some(s)

proc cornerRadii(fields: seq[SValue]): Option[seq[(int, float64)]] =
  ## `CnrD`: positional `[u8, i32[] record indices, f64[] radii, u8[] types]`.
  if fields.len < 3 or fields[1].kind != svArray or fields[2].kind != svArray: return
  var o: seq[(int, float64)]
  for k in 0 ..< min(fields[1].arr.len, fields[2].arr.len):
    let (i, r) = (fields[1].arr[k], fields[2].arr[k])
    if i.kind == svInt and r.kind == svFloat and r.f == r.f and abs(r.f) != Inf and r.f > 0.0 and i.i >= 0:
      o.add (int(i.i), r.f)
  some(o)

proc curves*(r: var Reader; crvs: ObjId): Option[AfPath] =
  ## `PCvD.Data`: positional `[u8, u32 count, then per subpath: bool closed, 18-byte records]`.
  ## A record is (f64 x, f64 y, u8 kind, u8 role): role 0 is an anchor, 1 the out-handle of the
  ## anchor before it, 2 the in-handle of the anchor after it. A closed subpath repeats its first
  ## anchor at the end. `CnrD` live corners are rounded on the stored polyline.
  let data = r.s.child(crvs, "Data")
  if data.isNone: return
  let fields = r.s.positional(data.get)
  var at = 0
  template next(): untyped =
    if at >= fields.len: return
    inc at
    fields[at - 1]
  discard next()
  let c = next()
  if c.kind != svUInt: return
  let count = c.u
  let cnr = r.s.child(crvs, "CnrD")
  let corners = if cnr.isSome: cornerRadii(r.s.positional(cnr.get)) else: none(seq[(int, float64)])
  var path = AfPath()
  var seen = 0
  var index = 0'u64
  while index < count:
    let cl = next()
    if cl.kind != svBool: return
    let closed = cl.b
    let recs = next()
    if recs.kind != svArray: return
    seen += recs.arr.len
    if seen > MaxNodes:
      r.warn("curves with more than four million nodes")
      return
    let anc = anchors(recs.arr)
    if anc.isNone: return
    let radii = if index == 0: corners else: none(seq[(int, float64)])
    let sp = subpath(anc.get, closed, radii)
    if sp.isSome: path.subpaths.add sp.get
    inc index
  if corners.isSome and count > 1:
    r.warn("rounded corners on curves with several subpaths (only the first is rounded)")
  some(path)

proc outlineAt(r: var Reader; id: ObjId; class: Tag; world: AfAffine; depth: int): Option[(AfPath, bool)] =
  case $class
  of "PCrv", "TxtC":
    var crvs = r.s.child(id, "Crvs")
    if crvs.isNone:
      let m = r.s.child(id, "MCrM")
      if m.isSome: crvs = r.s.child(m.get, "Crvs")
    if crvs.isNone: return
    let path = curves(r, crvs.get)
    if path.isNone: return
    let multi = path.get.subpaths.len > 1
    some((path.get, multi))
  of "Comp":
    # One path of all the operands' contours in the compound's space, filled even-odd: this
    # matches add and subtract operands in every public sample; a true union of overlapping
    # added operands would differ.
    var path = AfPath()
    if depth >= MaxCompoundDepth: return some((path, true))
    for c in r.s.children(id, "Chld"):
      let local = r.xfrm(c)
      let cls = r.s.class(c)
      if cls.isNone: return
      let op = r.s.enumeration(c, "ComO")
      if op.isSome and op.get[0] != 0 and op.get[0] != 2:
        r.warn("compound shapes that intersect or exclude (imported as add)")
      let o = outlineAt(r, c, cls.get, local.then(world), depth + 1)
      if o.isSome:
        for s in o.get[0].transformed(local).subpaths: path.subpaths.add s
    some((path, true))
  else:
    var sh = r.s.child(id, "Shpe")
    var boxOwner = id
    if sh.isNone:
      let master = r.s.child(id, "ShpM")
      if master.isNone: return
      sh = r.s.child(master.get, "Shpe")
      if sh.isNone: return
      boxOwner = master.get
    let b = r.s.floatsN(boxOwner, "ShpB", 4)
    if b.isNone: return
    let bv = b.get
    shape(r, sh.get, [bv[0], bv[1], bv[2], bv[3]], world)

proc outline*(r: var Reader; id: ObjId; class: Tag; world: AfAffine): Option[(AfPath, bool)] =
  ## The node-space outline of a curve, shape or compound node, and whether it fills even-odd.
  ## Instances (master-page or symbol copies) take the geometry of their master.
  outlineAt(r, id, class, world, 0)
