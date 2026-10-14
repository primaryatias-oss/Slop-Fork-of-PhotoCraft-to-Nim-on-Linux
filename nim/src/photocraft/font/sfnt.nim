## OpenType / TrueType font files (the role skrifa and read-fonts play for the Rust build):
## table directory and collections, metrics (`head`, `hhea`, `hmtx`, `OS/2`, `post`, `vhea`,
## `vmtx`, `VORG`), names, character maps, variation axes, and glyph outlines from `glyf`
## (quadratic, composites) and `CFF ` (Type 2 charstrings, CID-keyed fonts).
##
## Every read is bounds-checked: malformed fonts raise `FontError` from the parsing entry
## points and never crash. Outlines are unhinted, at the default variation instance.

import std/[tables, options, strutils, unicode, math]

type
  FontError* = object of CatchableError

  FontBlob* = ref object
    ## Shared font file bytes; `id` identifies the file for caches.
    data*: seq[byte]
    id*: uint64

  PathVerb* = enum pvMove, pvLine, pvQuad, pvCubic, pvClose
  PathCmd* = object
    verb*: PathVerb
    pts*: array[3, (float32, float32)]   ## end point last; control points first

  Outline* = seq[PathCmd]

  Axis* = object
    tag*: string
    minValue*, defaultValue*, maxValue*: float32

  NameRecord* = object
    platform*, encoding*, language*, nameId*: int
    value*: string

  CffFont = object
    present: bool
    charStrings: seq[(int, int)]   ## (offset, length) absolute
    globalSubrs: seq[(int, int)]
    localSubrs: seq[seq[(int, int)]]   ## per font dict (one entry for non-CID fonts)
    fdSelect: seq[uint16]            ## per glyph font dict index (CID fonts)
    defaultWidth, nominalWidth: seq[float32]

  Face* = ref object
    blob*: FontBlob
    index*: int
    tables: Table[string, (int, int)]
    unitsPerEm*: int
    numGlyphs*: int
    locaLong: bool
    hheaAscender*, hheaDescender*, hheaLineGap*: int
    typoAscender*, typoDescender*, typoLineGap*: int
    winAscent*, winDescent*: int
    useTypoMetrics*: bool
    xHeight*, capHeight*: int
    underlinePosition*, underlineThickness*: int
    strikeoutPosition*, strikeoutSize*: int
    weightClass*, widthClass*: int
    fsSelection*, macStyle*: int
    italicAngle*: float32
    isFixedPitch*: bool
    numHMetrics: int
    numVMetrics: int
    cmapOffset, cmapFormat: int
    hasVertical*: bool
    names*: seq[NameRecord]
    axes*: seq[Axis]
    cff: CffFont
    outlines: Table[int, Outline]
    layoutCache*: RootRef   ## OpenType layout data (`otl`), built on first use

var nextBlobId {.threadvar.}: uint64

proc fontError(msg: string): ref FontError = newException(FontError, msg)

proc newFontBlob*(data: seq[byte]): FontBlob =
  inc nextBlobId
  FontBlob(data: data, id: nextBlobId)

# ---------------------------------------------------------------- byte access

template need(f: Face; at, n: int) =
  if at < 0 or n < 0 or at > f.blob.data.len - n: raise fontError("font data is truncated")

proc u8(f: Face; at: int): int =
  f.need(at, 1)
  int(f.blob.data[at])
proc u16(f: Face; at: int): int =
  f.need(at, 2)
  (int(f.blob.data[at]) shl 8) or int(f.blob.data[at + 1])
proc i16(f: Face; at: int): int =
  let v = f.u16(at)
  if v >= 0x8000: v - 0x10000 else: v
proc u24(f: Face; at: int): int =
  f.need(at, 3)
  (int(f.blob.data[at]) shl 16) or (int(f.blob.data[at + 1]) shl 8) or int(f.blob.data[at + 2])
proc u32(f: Face; at: int): int =
  f.need(at, 4)
  (int(f.blob.data[at]) shl 24) or (int(f.blob.data[at + 1]) shl 16) or (int(f.blob.data[at + 2]) shl 8) or
    int(f.blob.data[at + 3])
proc i32(f: Face; at: int): int =
  let v = f.u32(at)
  if v >= 0x80000000: v - 0x100000000 else: v
proc fixed(f: Face; at: int): float32 = float32(f.i32(at)) / 65536
proc tag(f: Face; at: int): string =
  f.need(at, 4)
  result = newString(4)
  for i in 0 ..< 4: result[i] = char(f.blob.data[at + i])

proc table*(f: Face; tag: string): Option[(int, int)] =
  ## (offset, length) of a table, when present and inside the file.
  if tag in f.tables: some(f.tables[tag]) else: none((int, int))

proc hasTable*(f: Face; tag: string): bool = tag in f.tables

proc tableData*(f: Face; tag: string): seq[byte] =
  let t = f.table(tag)
  if t.isNone: return
  f.blob.data[t.get[0] ..< t.get[0] + t.get[1]]

# ---------------------------------------------------------------- collections

proc faceCount*(data: openArray[byte]): int =
  ## Faces in a font file: 1 for a single font, n for a collection, 0 when unrecognised.
  if data.len < 12: return 0
  let t = char(data[0]) & char(data[1]) & char(data[2]) & char(data[3])
  if t == "ttcf":
    let n = (int(data[8]) shl 24) or (int(data[9]) shl 16) or (int(data[10]) shl 8) or int(data[11])
    if n > 0 and 12 + 4 * n <= data.len: n else: 0
  elif t in ["\x00\x01\x00\x00", "OTTO", "true", "typ1"]: 1
  else: 0

# ---------------------------------------------------------------- CFF

proc cffIndex(f: Face; at: int): (seq[(int, int)], int) =
  ## Items (absolute offset, length) of a CFF INDEX at `at`, and the offset after it.
  let count = f.u16(at)
  if count == 0: return (@[], at + 2)
  let offSize = f.u8(at + 2)
  if offSize < 1 or offSize > 4: raise fontError("CFF: bad offset size")
  if count > 65535: raise fontError("CFF: index too large")
  let offs = at + 3
  let dataBase = offs + (count + 1) * offSize - 1
  proc off(i: int): int =
    let p = offs + i * offSize
    case offSize
    of 1: f.u8(p)
    of 2: f.u16(p)
    of 3: f.u24(p)
    else: f.u32(p)
  var items = newSeqOfCap[(int, int)](count)
  var prev = off(0)
  for i in 1 .. count:
    let cur = off(i)
    if cur < prev: raise fontError("CFF: index offsets decrease")
    items.add (dataBase + prev, cur - prev)
    prev = cur
  let after = dataBase + prev
  f.need(dataBase, prev)
  (items, after)

type DictEntry = (int, seq[float64])  ## operator (12 x as 1200 + x), operands

proc cffDict(f: Face; at, len: int): seq[DictEntry] =
  var p = at
  let e = at + len
  var ops: seq[float64]
  while p < e:
    let b0 = f.u8(p)
    if b0 <= 21:
      var op = b0
      inc p
      if b0 == 12:
        op = 1200 + f.u8(p)
        inc p
      result.add (op, ops)
      ops.setLen(0)
    elif b0 == 28:
      ops.add float64(f.i16(p + 1)); p += 3
    elif b0 == 29:
      ops.add float64(f.i32(p + 1)); p += 5
    elif b0 == 30:
      # real number: nibbles
      var s = ""
      inc p
      var done = false
      while not done and p < e:
        let b = f.u8(p)
        inc p
        for nib in [b shr 4, b and 15]:
          case nib
          of 0 .. 9: s.add char(ord('0') + nib)
          of 0xa: s.add '.'
          of 0xb: s.add 'E'
          of 0xc: s.add "E-"
          of 0xe: s.add '-'
          of 0xf:
            done = true
            break
          else: discard
      try: ops.add parseFloat(s)
      except ValueError: ops.add 0
    elif b0 >= 32 and b0 <= 246:
      ops.add float64(b0 - 139); inc p
    elif b0 >= 247 and b0 <= 250:
      ops.add float64((b0 - 247) * 256 + f.u8(p + 1) + 108); p += 2
    elif b0 >= 251 and b0 <= 254:
      ops.add float64(-(b0 - 251) * 256 - f.u8(p + 1) - 108); p += 2
    else:
      inc p
    if ops.len > 48: raise fontError("CFF: DICT operand stack overflow")

proc dictGet(d: seq[DictEntry]; op: int): seq[float64] =
  for (o, v) in d:
    if o == op: return v

proc parseCff(f: Face) =
  let t = f.table("CFF ")
  if t.isNone: return
  let base = t.get[0]
  let hdrSize = f.u8(base + 2)
  let (_, afterName) = f.cffIndex(base + hdrSize)
  let (topDicts, afterTop) = f.cffIndex(afterName)
  let (_, afterStrings) = f.cffIndex(afterTop)
  let (gsubrs, _) = f.cffIndex(afterStrings)
  if topDicts.len == 0: raise fontError("CFF: no Top DICT")
  let top = f.cffDict(topDicts[0][0], topDicts[0][1])
  let cs = top.dictGet(17)
  if cs.len < 1: raise fontError("CFF: no CharStrings")
  let csType = top.dictGet(1206)
  if csType.len > 0 and csType[0] != 2: raise fontError("CFF: only Type 2 charstrings are supported")
  var cff = CffFont(present: true, globalSubrs: gsubrs)
  cff.charStrings = f.cffIndex(base + int(cs[0]))[0]
  proc privateOf(entry: seq[float64]): (seq[(int, int)], float32, float32) =
    if entry.len < 2: return (@[], 0'f32, 0'f32)
    let (size, off) = (int(entry[0]), int(entry[1]))
    if size <= 0: return (@[], 0'f32, 0'f32)
    let pd = f.cffDict(base + off, size)
    var subrs: seq[(int, int)]
    let s = pd.dictGet(19)
    if s.len > 0: subrs = f.cffIndex(base + off + int(s[0]))[0]
    let dw = pd.dictGet(20)
    let nw = pd.dictGet(21)
    (subrs, (if dw.len > 0: float32(dw[0]) else: 0'f32), (if nw.len > 0: float32(nw[0]) else: 0'f32))
  let fdArray = top.dictGet(1236)
  if fdArray.len > 0:
    let (fds, _) = f.cffIndex(base + int(fdArray[0]))
    for (o, l) in fds:
      let fd = f.cffDict(o, l)
      let (subrs, dw, nw) = privateOf(fd.dictGet(18))
      cff.localSubrs.add subrs
      cff.defaultWidth.add dw
      cff.nominalWidth.add nw
    let fdSel = top.dictGet(1237)
    cff.fdSelect = newSeq[uint16](cff.charStrings.len)
    if fdSel.len > 0:
      let p = base + int(fdSel[0])
      case f.u8(p)
      of 0:
        for g in 0 ..< cff.charStrings.len: cff.fdSelect[g] = uint16(f.u8(p + 1 + g))
      of 3:
        let n = f.u16(p + 1)
        for i in 0 ..< n:
          let first = f.u16(p + 3 + i * 3)
          let fd = f.u8(p + 3 + i * 3 + 2)
          let next = f.u16(p + 3 + (i + 1) * 3)
          for g in first ..< min(next, cff.charStrings.len): cff.fdSelect[g] = uint16(fd)
      else: discard
  else:
    let (subrs, dw, nw) = privateOf(top.dictGet(18))
    cff.localSubrs = @[subrs]
    cff.defaultWidth = @[dw]
    cff.nominalWidth = @[nw]
  f.cff = cff

proc subrBias(n: int): int = (if n < 1240: 107 elif n < 33900: 1131 else: 32768)

proc cffOutline(f: Face; gid: int): Outline =
  if gid < 0 or gid >= f.cff.charStrings.len: return
  let fd = if f.cff.fdSelect.len > gid: int(f.cff.fdSelect[gid]) else: 0
  let local = if fd < f.cff.localSubrs.len: f.cff.localSubrs[fd] else: @[]
  var stack: seq[float64]
  var x, y = 0.0
  var open = false
  var nStems = 0
  var widthDone = false
  var transient: array[32, float64]
  var depth = 0
  var ops = 0
  var path: Outline
  proc moveTo(nx, ny: float64) =
    if open: path.add PathCmd(verb: pvClose)
    path.add PathCmd(verb: pvMove, pts: [(0'f32, 0'f32), (0'f32, 0'f32), (float32(nx), float32(ny))])
    open = true
  proc lineTo(nx, ny: float64) =
    path.add PathCmd(verb: pvLine, pts: [(0'f32, 0'f32), (0'f32, 0'f32), (float32(nx), float32(ny))])
  proc curveTo(x1, y1, x2, y2, x3, y3: float64) =
    path.add PathCmd(verb: pvCubic, pts: [(float32(x1), float32(y1)), (float32(x2), float32(y2)), (float32(x3), float32(y3))])
  proc run(cs: (int, int)) =
    inc depth
    if depth > 10: raise fontError("CFF: subroutines nested too deeply")
    var p = cs[0]
    let e = cs[0] + cs[1]
    while p < e:
      inc ops
      if ops > 200_000: raise fontError("CFF: charstring too long")
      let b0 = f.u8(p)
      if b0 >= 32 or b0 == 28:
        if b0 == 28:
          stack.add float64(f.i16(p + 1)); p += 3
        elif b0 <= 246:
          stack.add float64(b0 - 139); inc p
        elif b0 <= 250:
          stack.add float64((b0 - 247) * 256 + f.u8(p + 1) + 108); p += 2
        elif b0 <= 254:
          stack.add float64(-(b0 - 251) * 256 - f.u8(p + 1) - 108); p += 2
        else:
          stack.add float64(f.i32(p + 1)) / 65536; p += 5
        if stack.len > 513: raise fontError("CFF: stack overflow")
        continue
      inc p
      template takeWidth(evenArgs: bool) =
        if not widthDone:
          widthDone = true
          if (stack.len mod 2 == 1) == evenArgs: stack.delete(0)
      case b0
      of 1, 3, 18, 23:  # hstem vstem hstemhm vstemhm
        takeWidth(true)
        nStems += stack.len div 2
        stack.setLen(0)
      of 19, 20:  # hintmask cntrmask
        takeWidth(true)
        nStems += stack.len div 2
        stack.setLen(0)
        p += (nStems + 7) div 8
      of 21:  # rmoveto
        takeWidth(true)
        if stack.len >= 2:
          x += stack[^2]; y += stack[^1]
        moveTo(x, y)
        stack.setLen(0)
      of 22:  # hmoveto
        if not widthDone:
          widthDone = true
          if stack.len > 1: stack.delete(0)
        if stack.len >= 1: x += stack[^1]
        moveTo(x, y)
        stack.setLen(0)
      of 4:  # vmoveto
        if not widthDone:
          widthDone = true
          if stack.len > 1: stack.delete(0)
        if stack.len >= 1: y += stack[^1]
        moveTo(x, y)
        stack.setLen(0)
      of 5:  # rlineto
        var i = 0
        while i + 1 < stack.len:
          x += stack[i]; y += stack[i + 1]
          lineTo(x, y)
          i += 2
        stack.setLen(0)
      of 6, 7:  # hlineto vlineto
        var horiz = b0 == 6
        for v in stack:
          if horiz: x += v else: y += v
          lineTo(x, y)
          horiz = not horiz
        stack.setLen(0)
      of 8:  # rrcurveto
        var i = 0
        while i + 5 < stack.len:
          let (x1, y1) = (x + stack[i], y + stack[i + 1])
          let (x2, y2) = (x1 + stack[i + 2], y1 + stack[i + 3])
          x = x2 + stack[i + 4]; y = y2 + stack[i + 5]
          curveTo(x1, y1, x2, y2, x, y)
          i += 6
        stack.setLen(0)
      of 24:  # rcurveline
        var i = 0
        while i + 5 < stack.len - 2:
          let (x1, y1) = (x + stack[i], y + stack[i + 1])
          let (x2, y2) = (x1 + stack[i + 2], y1 + stack[i + 3])
          x = x2 + stack[i + 4]; y = y2 + stack[i + 5]
          curveTo(x1, y1, x2, y2, x, y)
          i += 6
        if i + 1 < stack.len:
          x += stack[i]; y += stack[i + 1]
          lineTo(x, y)
        stack.setLen(0)
      of 25:  # rlinecurve
        var i = 0
        while i + 1 < stack.len - 6:
          x += stack[i]; y += stack[i + 1]
          lineTo(x, y)
          i += 2
        if i + 5 < stack.len:
          let (x1, y1) = (x + stack[i], y + stack[i + 1])
          let (x2, y2) = (x1 + stack[i + 2], y1 + stack[i + 3])
          x = x2 + stack[i + 4]; y = y2 + stack[i + 5]
          curveTo(x1, y1, x2, y2, x, y)
        stack.setLen(0)
      of 26:  # vvcurveto
        var i = 0
        var dx1 = 0.0
        if stack.len mod 4 == 1:
          dx1 = stack[0]
          i = 1
        while i + 3 < stack.len:
          let (x1, y1) = (x + dx1, y + stack[i])
          let (x2, y2) = (x1 + stack[i + 1], y1 + stack[i + 2])
          x = x2; y = y2 + stack[i + 3]
          curveTo(x1, y1, x2, y2, x, y)
          dx1 = 0
          i += 4
        stack.setLen(0)
      of 27:  # hhcurveto
        var i = 0
        var dy1 = 0.0
        if stack.len mod 4 == 1:
          dy1 = stack[0]
          i = 1
        while i + 3 < stack.len:
          let (x1, y1) = (x + stack[i], y + dy1)
          let (x2, y2) = (x1 + stack[i + 1], y1 + stack[i + 2])
          x = x2 + stack[i + 3]; y = y2
          curveTo(x1, y1, x2, y2, x, y)
          dy1 = 0
          i += 4
        stack.setLen(0)
      of 30, 31:  # vhcurveto hvcurveto
        var horiz = b0 == 31
        var i = 0
        while i + 3 < stack.len:
          let last = i + 4 == stack.len - 1
          if horiz:
            let (x1, y1) = (x + stack[i], y)
            let (x2, y2) = (x1 + stack[i + 1], y1 + stack[i + 2])
            y = y2 + stack[i + 3]
            x = x2 + (if last: stack[i + 4] else: 0.0)
            curveTo(x1, y1, x2, y2, x, y)
          else:
            let (x1, y1) = (x, y + stack[i])
            let (x2, y2) = (x1 + stack[i + 1], y1 + stack[i + 2])
            x = x2 + stack[i + 3]
            y = y2 + (if last: stack[i + 4] else: 0.0)
            curveTo(x1, y1, x2, y2, x, y)
          horiz = not horiz
          i += 4
        stack.setLen(0)
      of 10, 29:  # callsubr callgsubr
        if stack.len == 0: raise fontError("CFF: subroutine call without index")
        let subrs = if b0 == 10: local else: f.cff.globalSubrs
        let idx = int(stack.pop()) + subrBias(subrs.len)
        if idx < 0 or idx >= subrs.len: raise fontError("CFF: subroutine index out of range")
        run(subrs[idx])
      of 11: # return
        dec depth
        return
      of 14:  # endchar
        if not widthDone:
          widthDone = true
          if stack.len == 1 or stack.len == 5: stack.delete(0)
        if open:
          path.add PathCmd(verb: pvClose)
          open = false
        stack.setLen(0)
        p = e
        depth = 100
        return
      of 12:
        let b1 = f.u8(p)
        inc p
        case b1
        of 35:  # flex
          if stack.len >= 12:
            let (x1, y1) = (x + stack[0], y + stack[1])
            let (x2, y2) = (x1 + stack[2], y1 + stack[3])
            let (x3, y3) = (x2 + stack[4], y2 + stack[5])
            curveTo(x1, y1, x2, y2, x3, y3)
            let (x4, y4) = (x3 + stack[6], y3 + stack[7])
            let (x5, y5) = (x4 + stack[8], y4 + stack[9])
            x = x5 + stack[10]; y = y5 + stack[11]
            curveTo(x4, y4, x5, y5, x, y)
        of 34:  # hflex
          if stack.len >= 7:
            let (x1, y1) = (x + stack[0], y)
            let (x2, y2) = (x1 + stack[1], y1 + stack[2])
            let (x3, y3) = (x2 + stack[3], y2)
            curveTo(x1, y1, x2, y2, x3, y3)
            let (x4, y4) = (x3 + stack[4], y2)
            let (x5, y5) = (x4 + stack[5], y)
            x = x5 + stack[6]
            curveTo(x4, y4, x5, y5, x, y)
        of 36:  # hflex1
          if stack.len >= 9:
            let (x1, y1) = (x + stack[0], y + stack[1])
            let (x2, y2) = (x1 + stack[2], y1 + stack[3])
            let (x3, y3) = (x2 + stack[4], y2)
            curveTo(x1, y1, x2, y2, x3, y3)
            let (x4, y4) = (x3 + stack[5], y2)
            let (x5, y5) = (x4 + stack[6], y4 + stack[7])
            x = x5 + stack[8]
            curveTo(x4, y4, x5, y5, x, y)
        of 37:  # flex1
          if stack.len >= 11:
            let (sx, sy) = (x, y)
            let (x1, y1) = (x + stack[0], y + stack[1])
            let (x2, y2) = (x1 + stack[2], y1 + stack[3])
            let (x3, y3) = (x2 + stack[4], y2 + stack[5])
            curveTo(x1, y1, x2, y2, x3, y3)
            let (x4, y4) = (x3 + stack[6], y3 + stack[7])
            let (x5, y5) = (x4 + stack[8], y4 + stack[9])
            if abs(x5 - sx) > abs(y5 - sy):
              x = x5 + stack[10]; y = sy
            else:
              x = sx; y = y5 + stack[10]
            curveTo(x4, y4, x5, y5, x, y)
        of 3:  # and
          if stack.len >= 2:
            let b = stack.pop(); let a = stack.pop()
            stack.add (if a != 0 and b != 0: 1.0 else: 0.0)
        of 4:  # or
          if stack.len >= 2:
            let b = stack.pop(); let a = stack.pop()
            stack.add (if a != 0 or b != 0: 1.0 else: 0.0)
        of 5:  # not
          if stack.len >= 1: stack[^1] = (if stack[^1] == 0: 1.0 else: 0.0)
        of 9:  # abs
          if stack.len >= 1: stack[^1] = abs(stack[^1])
        of 10:  # add
          if stack.len >= 2:
            let b = stack.pop(); stack[^1] += b
        of 11:  # sub
          if stack.len >= 2:
            let b = stack.pop(); stack[^1] -= b
        of 12:  # div
          if stack.len >= 2:
            let b = stack.pop()
            stack[^1] = (if b != 0: stack[^1] / b else: 0.0)
        of 14:  # neg
          if stack.len >= 1: stack[^1] = -stack[^1]
        of 15:  # eq
          if stack.len >= 2:
            let b = stack.pop(); let a = stack.pop()
            stack.add (if a == b: 1.0 else: 0.0)
        of 18:  # drop
          if stack.len >= 1: discard stack.pop()
        of 20:  # put
          if stack.len >= 2:
            let i = int(stack.pop()); let v = stack.pop()
            if i >= 0 and i < transient.len: transient[i] = v
        of 21:  # get
          if stack.len >= 1:
            let i = int(stack[^1])
            stack[^1] = (if i >= 0 and i < transient.len: transient[i] else: 0.0)
        of 22:  # ifelse
          if stack.len >= 4:
            let v2 = stack.pop(); let v1 = stack.pop(); let s2 = stack.pop(); let s1 = stack.pop()
            stack.add (if v1 <= v2: s1 else: s2)
        of 23: stack.add 0.5  # random (deterministic)
        of 24:  # mul
          if stack.len >= 2:
            let b = stack.pop(); stack[^1] *= b
        of 26:  # sqrt
          if stack.len >= 1: stack[^1] = (if stack[^1] > 0: sqrt(stack[^1]) else: 0.0)
        of 27:  # dup
          if stack.len >= 1: stack.add stack[^1]
        of 28:  # exch
          if stack.len >= 2: swap(stack[^1], stack[^2])
        of 29:  # index
          if stack.len >= 1:
            let i = int(stack.pop())
            stack.add (if i >= 0 and i < stack.len: stack[^(i + 1)] elif stack.len > 0: stack[^1] else: 0.0)
        of 30:  # roll
          if stack.len >= 2:
            let j = int(stack.pop()); let n = int(stack.pop())
            if n > 0 and n <= stack.len:
              var part = stack[stack.len - n .. ^1]
              let k = ((j mod n) + n) mod n
              part = part[n - k .. ^1] & part[0 ..< n - k]
              for i in 0 ..< n: stack[stack.len - n + i] = part[i]
        else: stack.setLen(0)
      else:
        stack.setLen(0)
    dec depth
  run(f.cff.charStrings[gid])
  if open: path.add PathCmd(verb: pvClose)
  path

# ---------------------------------------------------------------- glyf

proc glyfRange(f: Face; gid: int): (int, int) =
  let loca = f.table("loca")
  let glyf = f.table("glyf")
  if loca.isNone or glyf.isNone or gid < 0 or gid >= f.numGlyphs: return (0, 0)
  var a, b: int
  if f.locaLong:
    if (gid + 2) * 4 > loca.get[1]: return (0, 0)
    a = f.u32(loca.get[0] + gid * 4)
    b = f.u32(loca.get[0] + gid * 4 + 4)
  else:
    if (gid + 2) * 2 > loca.get[1]: return (0, 0)
    a = f.u16(loca.get[0] + gid * 2) * 2
    b = f.u16(loca.get[0] + gid * 2 + 2) * 2
  if b <= a or b > glyf.get[1]: return (0, 0)
  (glyf.get[0] + a, b - a)

proc glyfExtents*(f: Face; gid: int): Option[(int, int, int, int)] =
  ## HarfBuzz glyph extents from the `glyf` header box: (x bearing, y bearing, width, height),
  ## height negative; zero for empty glyphs; none for fonts without `glyf`.
  if not f.hasTable("glyf") or not f.hasTable("loca"): return
  try:
    let (at, len) = f.glyfRange(gid)
    if len < 10: return some((0, 0, 0, 0))
    let (x0, y0, x1, y1) = (f.i16(at + 2), f.i16(at + 4), f.i16(at + 6), f.i16(at + 8))
    some((x0, y1, x1 - x0, y0 - y1))
  except FontError: none((int, int, int, int))

proc glyfOutline(f: Face; gid: int; depth: int; xf: array[6, float32]): Outline =
  if depth > 8: raise fontError("glyf: composite glyphs nested too deeply")
  let (at, len) = f.glyfRange(gid)
  if len < 10: return
  let nContours = f.i16(at)
  proc tp(x, y: float32): (float32, float32) =
    (xf[0] * x + xf[2] * y + xf[4], xf[1] * x + xf[3] * y + xf[5])
  if nContours >= 0:
    var p = at + 10
    var ends = newSeq[int](nContours)
    for i in 0 ..< nContours: ends[i] = f.u16(p + 2 * i)
    p += 2 * nContours
    let nPts = if nContours > 0: ends[^1] + 1 else: 0
    if nPts > 65536: raise fontError("glyf: too many points")
    let insLen = f.u16(p)
    p += 2 + insLen
    var flags = newSeq[int](nPts)
    var i = 0
    while i < nPts:
      let fl = f.u8(p)
      inc p
      flags[i] = fl
      inc i
      if (fl and 8) != 0:
        var r = f.u8(p)
        inc p
        while r > 0 and i < nPts:
          flags[i] = fl
          inc i
          dec r
    var xs = newSeq[float32](nPts)
    var ys = newSeq[float32](nPts)
    var v = 0
    for k in 0 ..< nPts:
      let fl = flags[k]
      if (fl and 2) != 0:
        let d = f.u8(p)
        inc p
        v += (if (fl and 16) != 0: d else: -d)
      elif (fl and 16) == 0:
        v += f.i16(p)
        p += 2
      xs[k] = float32(v)
    v = 0
    for k in 0 ..< nPts:
      let fl = flags[k]
      if (fl and 4) != 0:
        let d = f.u8(p)
        inc p
        v += (if (fl and 32) != 0: d else: -d)
      elif (fl and 32) == 0:
        v += f.i16(p)
        p += 2
      ys[k] = float32(v)
    var start = 0
    for c in 0 ..< nContours:
      let e = ends[c]
      if e < start or e >= nPts:
        start = e + 1
        continue
      let n = e - start + 1
      # Start where skrifa does: the first point when on-curve, else the last when on-curve,
      # else the midpoint of the last and first; then walk the contour back to the start.
      var pts: seq[((float32, float32), bool)]
      template P(k: int): ((float32, float32), bool) =
        ((xs[start + k], ys[start + k]), (flags[start + k] and 1) != 0)
      if P(0)[1]:
        for k in 0 ..< n: pts.add P(k)
      elif P(n - 1)[1]:
        pts.add P(n - 1)
        for k in 0 ..< n - 1: pts.add P(k)
      else:
        let (a, b) = (P(n - 1)[0], P(0)[0])
        pts.add (((a[0] + b[0]) / 2, (a[1] + b[1]) / 2), true)
        for k in 0 ..< n: pts.add P(k)
      let s0 = pts[0][0]
      result.add PathCmd(verb: pvMove, pts: [(0'f32, 0'f32), (0'f32, 0'f32), tp(s0[0], s0[1])])
      var ctrl = none((float32, float32))
      for k in 1 .. pts.len:
        let (q, isOn) = if k < pts.len: pts[k] else: (s0, true)
        if isOn:
          if ctrl.isSome:
            result.add PathCmd(verb: pvQuad, pts: [tp(ctrl.get[0], ctrl.get[1]), (0'f32, 0'f32), tp(q[0], q[1])])
          elif k < pts.len:
            result.add PathCmd(verb: pvLine, pts: [(0'f32, 0'f32), (0'f32, 0'f32), tp(q[0], q[1])])
          ctrl = none((float32, float32))
        else:
          if ctrl.isSome:
            let mid = ((ctrl.get[0] + q[0]) / 2, (ctrl.get[1] + q[1]) / 2)
            result.add PathCmd(verb: pvQuad, pts: [tp(ctrl.get[0], ctrl.get[1]), (0'f32, 0'f32), tp(mid[0], mid[1])])
          ctrl = some(q)
      result.add PathCmd(verb: pvClose)
      start = e + 1
  else:
    var p = at + 10
    var more = true
    var guard = 0
    while more:
      inc guard
      if guard > 1024: raise fontError("glyf: too many components")
      let flags = f.u16(p)
      let glyph = f.u16(p + 2)
      p += 4
      var dx, dy: float32
      if (flags and 1) != 0:
        dx = float32(f.i16(p)); dy = float32(f.i16(p + 2)); p += 4
      else:
        let a = f.u8(p); let b = f.u8(p + 1)
        dx = float32(if a >= 128: a - 256 else: a); dy = float32(if b >= 128: b - 256 else: b)
        p += 2
      if (flags and 2) == 0:
        dx = 0; dy = 0   # point matching: not supported, placed at the origin
      var m = [1'f32, 0, 0, 1]
      if (flags and 8) != 0:
        let s = float32(f.i16(p)) / 16384
        m = [s, 0, 0, s]; p += 2
      elif (flags and 0x40) != 0:
        m = [float32(f.i16(p)) / 16384, 0, 0, float32(f.i16(p + 2)) / 16384]; p += 4
      elif (flags and 0x80) != 0:
        m = [float32(f.i16(p)) / 16384, float32(f.i16(p + 2)) / 16384, float32(f.i16(p + 4)) / 16384,
             float32(f.i16(p + 6)) / 16384]
        p += 8
      # child transform = xf ∘ [m | (dx,dy)]
      let c = [m[0], m[1], m[2], m[3], dx, dy]
      let comb = [xf[0] * c[0] + xf[2] * c[1], xf[1] * c[0] + xf[3] * c[1],
                  xf[0] * c[2] + xf[2] * c[3], xf[1] * c[2] + xf[3] * c[3],
                  xf[0] * c[4] + xf[2] * c[5] + xf[4], xf[1] * c[4] + xf[3] * c[5] + xf[5]]
      result.add f.glyfOutline(glyph, depth + 1, comb)
      more = (flags and 0x20) != 0

# ---------------------------------------------------------------- loading

proc parseNames(f: Face) =
  let t = f.table("name")
  if t.isNone: return
  let base = t.get[0]
  let count = f.u16(base + 2)
  let strOff = base + f.u16(base + 4)
  for i in 0 ..< count:
    let r = base + 6 + i * 12
    if r + 12 > base + t.get[1]: break
    let platform = f.u16(r)
    let encoding = f.u16(r + 2)
    let language = f.u16(r + 4)
    let nameId = f.u16(r + 6)
    let length = f.u16(r + 8)
    let offset = f.u16(r + 10)
    let at = strOff + offset
    if at < 0 or at + length > f.blob.data.len: continue
    var value = ""
    if platform == 0 or platform == 3:
      var units: seq[uint16]
      for k in 0 ..< length div 2: units.add uint16(f.u16(at + 2 * k))
      var k = 0
      while k < units.len:
        var cp = int(units[k])
        if cp >= 0xD800 and cp < 0xDC00 and k + 1 < units.len and int(units[k + 1]) >= 0xDC00 and int(units[k + 1]) < 0xE000:
          cp = 0x10000 + ((cp - 0xD800) shl 10) + (int(units[k + 1]) - 0xDC00)
          inc k
        elif cp >= 0xD800 and cp < 0xE000: cp = 0xFFFD
        value.add $Rune(cp)
        inc k
    elif platform == 1 and encoding == 0:
      for k in 0 ..< length:
        let c = f.u8(at + k)
        value.add (if c < 128: $char(c) else: "?")
    else: continue
    f.names.add NameRecord(platform: platform, encoding: encoding, language: language, nameId: nameId, value: value)

proc selectCmap(f: Face) =
  let t = f.table("cmap")
  if t.isNone: return
  let base = t.get[0]
  let n = f.u16(base + 2)
  var best = -1
  var bestOff = 0
  for i in 0 ..< n:
    let r = base + 4 + i * 8
    let platform = f.u16(r)
    let encoding = f.u16(r + 2)
    let off = f.u32(r + 4)
    let rank =
      if platform == 3 and encoding == 10: 6
      elif platform == 0 and (encoding == 4 or encoding == 6): 5
      elif platform == 3 and encoding == 1: 4
      elif platform == 0: 3
      elif platform == 3 and encoding == 0: 2
      elif platform == 1 and encoding == 0: 1
      else: 0
    if rank > best and off > 0 and off < t.get[1]:
      let fmt = f.u16(base + off)
      if fmt in [0, 4, 6, 10, 12, 13]:
        best = rank
        bestOff = base + off
  if best >= 0:
    f.cmapOffset = bestOff
    f.cmapFormat = f.u16(bestOff)

proc parseFvar(f: Face) =
  let t = f.table("fvar")
  if t.isNone: return
  let base = t.get[0]
  let axesOff = f.u16(base + 4)
  let count = f.u16(base + 8)
  let size = f.u16(base + 10)
  for i in 0 ..< count:
    let a = base + axesOff + i * size
    f.axes.add Axis(tag: f.tag(a), minValue: f.fixed(a + 4), defaultValue: f.fixed(a + 8), maxValue: f.fixed(a + 12))

proc loadFace*(blob: FontBlob; index = 0): Face =
  ## Parses face `index` of a font file. Raises `FontError` when it is not a usable font.
  result = Face(blob: blob, index: index)
  let f = result
  var dir = 0
  if f.tag(0) == "ttcf":
    let n = f.u32(8)
    if index < 0 or index >= n: raise fontError("no face " & $index & " in the collection")
    dir = f.u32(12 + 4 * index)
  elif index != 0: raise fontError("no face " & $index)
  let sfntVersion = f.tag(dir)
  if sfntVersion notin ["\x00\x01\x00\x00", "OTTO", "true"]: raise fontError("not an OpenType font")
  let numTables = f.u16(dir + 4)
  for i in 0 ..< numTables:
    let r = dir + 12 + i * 16
    let tg = f.tag(r)
    let off = f.u32(r + 8)
    let len = f.u32(r + 12)
    if off >= 0 and len >= 0 and off <= blob.data.len - len: f.tables[tg] = (off, len)
  let head = f.table("head")
  let maxp = f.table("maxp")
  if head.isNone or maxp.isNone: raise fontError("missing head or maxp table")
  f.unitsPerEm = f.u16(head.get[0] + 18)
  if f.unitsPerEm < 16 or f.unitsPerEm > 16384: f.unitsPerEm = 1000
  f.macStyle = f.u16(head.get[0] + 44)
  f.locaLong = f.i16(head.get[0] + 50) != 0
  f.numGlyphs = f.u16(maxp.get[0] + 4)
  let hhea = f.table("hhea")
  if hhea.isSome:
    f.hheaAscender = f.i16(hhea.get[0] + 4)
    f.hheaDescender = f.i16(hhea.get[0] + 6)
    f.hheaLineGap = f.i16(hhea.get[0] + 8)
    f.numHMetrics = f.u16(hhea.get[0] + 34)
  let os2 = f.table("OS/2")
  if os2.isSome and os2.get[1] >= 78:
    let b = os2.get[0]
    f.weightClass = f.u16(b + 4)
    f.widthClass = f.u16(b + 6)
    f.strikeoutSize = f.i16(b + 26)
    f.strikeoutPosition = f.i16(b + 28)
    f.fsSelection = f.u16(b + 62)
    f.typoAscender = f.i16(b + 68)
    f.typoDescender = f.i16(b + 70)
    f.typoLineGap = f.i16(b + 72)
    f.winAscent = f.u16(b + 74)
    f.winDescent = f.u16(b + 76)
    f.useTypoMetrics = (f.fsSelection and 128) != 0
    if os2.get[1] >= 90 and f.u16(b) >= 2:
      f.xHeight = f.i16(b + 86)
      f.capHeight = f.i16(b + 88)
  else:
    f.weightClass = (if (f.macStyle and 1) != 0: 700 else: 400)
    f.widthClass = 5
  if f.weightClass == 0: f.weightClass = 400
  let post = f.table("post")
  if post.isSome and post.get[1] >= 16:
    f.italicAngle = f.fixed(post.get[0] + 4)
    f.underlinePosition = f.i16(post.get[0] + 8)
    f.underlineThickness = f.i16(post.get[0] + 10)
    f.isFixedPitch = f.u32(post.get[0] + 12) != 0
  let vhea = f.table("vhea")
  if vhea.isSome and f.hasTable("vmtx"):
    f.hasVertical = true
    f.numVMetrics = f.u16(vhea.get[0] + 34)
  f.parseNames()
  f.selectCmap()
  f.parseFvar()
  if f.hasTable("CFF "): f.parseCff()
  elif not f.hasTable("glyf") and not f.hasTable("CFF2"):
    raise fontError("the font has no glyph outlines")

proc loadFaces*(data: seq[byte]): seq[Face] =
  ## Every usable face of a font file (none when it is not a font).
  let blob = newFontBlob(data)
  let n = faceCount(data)
  for i in 0 ..< n:
    try: result.add loadFace(blob, i)
    except FontError: discard

# ---------------------------------------------------------------- queries

proc italic*(f: Face): bool = (f.fsSelection and 1) != 0 or (f.macStyle and 2) != 0
proc bold*(f: Face): bool = (f.fsSelection and 32) != 0 or (f.macStyle and 1) != 0

proc ascent*(f: Face): float32 =
  ## Typographic ascent in font units (OS/2 typo when USE_TYPO_METRICS, else hhea).
  if f.useTypoMetrics and (f.typoAscender != 0 or f.typoDescender != 0): float32(f.typoAscender)
  elif f.hheaAscender != 0 or f.hheaDescender != 0: float32(f.hheaAscender)
  else: float32(f.typoAscender)

proc descent*(f: Face): float32 =
  ## Descent as a positive distance below the baseline, in font units.
  if f.useTypoMetrics and (f.typoAscender != 0 or f.typoDescender != 0): float32(-f.typoDescender)
  elif f.hheaAscender != 0 or f.hheaDescender != 0: float32(-f.hheaDescender)
  else: float32(-f.typoDescender)

proc lineGap*(f: Face): float32 =
  if f.useTypoMetrics and (f.typoAscender != 0 or f.typoDescender != 0): float32(f.typoLineGap)
  else: float32(f.hheaLineGap)

proc advance*(f: Face; gid: int): float32 =
  ## Horizontal advance in font units.
  let t = f.table("hmtx")
  if t.isNone or f.numHMetrics <= 0 or gid < 0 or gid >= f.numGlyphs: return 0
  let i = min(gid, f.numHMetrics - 1)
  if i < 0 or (i + 1) * 4 > t.get[1]: return 0
  float32(f.u16(t.get[0] + i * 4))

proc leftSideBearing*(f: Face; gid: int): float32 =
  let t = f.table("hmtx")
  if t.isNone or f.numHMetrics <= 0 or gid < 0: return 0
  if gid < f.numHMetrics:
    if (gid + 1) * 4 > t.get[1]: return 0
    return float32(f.i16(t.get[0] + gid * 4 + 2))
  let at = f.numHMetrics * 4 + (gid - f.numHMetrics) * 2
  if at + 2 > t.get[1]: return 0
  float32(f.i16(t.get[0] + at))

proc verticalAdvance*(f: Face; gid: int): Option[float32] =
  let t = f.table("vmtx")
  if t.isNone or f.numVMetrics <= 0: return
  let i = min(gid, f.numVMetrics - 1)
  if i < 0 or (i + 1) * 4 > t.get[1]: return
  some(float32(f.u16(t.get[0] + i * 4)))

proc topSideBearing*(f: Face; gid: int): Option[float32] =
  let t = f.table("vmtx")
  if t.isNone or f.numVMetrics <= 0 or gid < 0: return
  if gid < f.numVMetrics:
    if (gid + 1) * 4 > t.get[1]: return
    return some(float32(f.i16(t.get[0] + gid * 4 + 2)))
  let at = f.numVMetrics * 4 + (gid - f.numVMetrics) * 2
  if at + 2 > t.get[1]: return
  some(float32(f.i16(t.get[0] + at)))

proc verticalOrigin*(f: Face; gid: int): Option[float32] =
  ## The `VORG` vertical origin y of a glyph (CFF fonts), in font units.
  let t = f.table("VORG")
  if t.isNone: return
  let base = t.get[0]
  try:
    let def = f.i16(base + 4)
    let n = f.u16(base + 6)
    var lo = 0
    var hi = n - 1
    while lo <= hi:
      let mid = (lo + hi) div 2
      let g = f.u16(base + 8 + mid * 4)
      if g == gid: return some(float32(f.i16(base + 8 + mid * 4 + 2)))
      if g < gid: lo = mid + 1 else: hi = mid - 1
    some(float32(def))
  except FontError: none(float32)

proc glyphIndex*(f: Face; cp: int): int =
  ## The glyph for a code point (0 when unmapped).
  if f.cmapOffset == 0 or cp < 0: return 0
  let b = f.cmapOffset
  try:
    case f.cmapFormat
    of 0:
      if cp < 256: return f.u8(b + 6 + cp)
    of 4:
      if cp > 0xFFFF: return 0
      let segX2 = f.u16(b + 6)
      let segs = segX2 div 2
      let ends = b + 14
      let starts = ends + segX2 + 2
      let deltas = starts + segX2
      let ranges = deltas + segX2
      var lo = 0
      var hi = segs - 1
      while lo <= hi:
        let mid = (lo + hi) div 2
        let e = f.u16(ends + mid * 2)
        if e < cp: lo = mid + 1
        else: hi = mid - 1
      if lo >= segs: return 0
      let s = f.u16(starts + lo * 2)
      if cp < s: return 0
      let delta = f.u16(deltas + lo * 2)
      let ro = f.u16(ranges + lo * 2)
      if ro == 0: return (cp + delta) and 0xFFFF
      let g = f.u16(ranges + lo * 2 + ro + (cp - s) * 2)
      if g == 0: return 0
      return (g + delta) and 0xFFFF
    of 6:
      let first = f.u16(b + 6)
      let count = f.u16(b + 8)
      if cp >= first and cp < first + count: return f.u16(b + 10 + (cp - first) * 2)
    of 10:
      let first = f.u32(b + 12)
      let count = f.u32(b + 16)
      if cp >= first and cp < first + count: return f.u16(b + 20 + (cp - first) * 2)
    of 12, 13:
      let n = f.u32(b + 12)
      var lo = 0
      var hi = n - 1
      while lo <= hi:
        let mid = (lo + hi) div 2
        let g = b + 16 + mid * 12
        let s = f.u32(g)
        let e = f.u32(g + 4)
        if cp < s: hi = mid - 1
        elif cp > e: lo = mid + 1
        else:
          let start = f.u32(g + 8)
          return (if f.cmapFormat == 12: start + (cp - s) else: start)
    else: discard
  except FontError: discard
  0

proc mappings*(f: Face): seq[(int, int)] =
  ## Every (code point, glyph) pair the character map holds, glyph 0 excluded.
  if f.cmapOffset == 0: return
  let b = f.cmapOffset
  try:
    case f.cmapFormat
    of 0:
      for cp in 0 ..< 256:
        let g = f.u8(b + 6 + cp)
        if g != 0: result.add (cp, g)
    of 4:
      let segs = f.u16(b + 6) div 2
      let ends = b + 14
      let starts = ends + segs * 2 + 2
      for i in 0 ..< segs:
        let s = f.u16(starts + i * 2)
        let e = f.u16(ends + i * 2)
        if e - s > 0x10000: continue
        for cp in s .. e:
          if cp == 0xFFFF: break
          let g = f.glyphIndex(cp)
          if g != 0: result.add (cp, g)
    of 6, 10:
      let (first, count, at) = if f.cmapFormat == 6: (f.u16(b + 6), f.u16(b + 8), b + 10)
                               else: (f.u32(b + 12), f.u32(b + 16), b + 20)
      for i in 0 ..< min(count, 0x110000):
        let g = f.u16(at + i * 2)
        if g != 0: result.add (first + i, g)
    of 12, 13:
      let n = f.u32(b + 12)
      var total = 0
      for i in 0 ..< n:
        let g = b + 16 + i * 12
        let s = f.u32(g)
        let e = min(f.u32(g + 4), 0x10FFFF)
        let start = f.u32(g + 8)
        if e < s: continue
        total += e - s + 1
        if total > 0x110000: break
        for cp in s .. e:
          let gid = if f.cmapFormat == 12: start + (cp - s) else: start
          if gid != 0: result.add (cp, gid)
    else: discard
  except FontError: discard

proc name*(f: Face; id: int): Option[string] =
  ## A name string: US English (Windows), else any English, else the first.
  var english, first = none(string)
  for r in f.names:
    if r.nameId != id or r.value.len == 0: continue
    if r.platform == 3 and r.language == 0x409: return some(r.value)
    if english.isNone and ((r.platform == 3 and (r.language and 0x3FF) == 9) or (r.platform == 1 and r.language == 0)):
      english = some(r.value)
    if first.isNone: first = some(r.value)
  if english.isSome: english else: first

proc familyName*(f: Face): string =
  let t = f.name(16)
  if t.isSome: t.get else: f.name(1).get("")

proc styleName*(f: Face): string =
  let t = f.name(17)
  if t.isSome: t.get else: f.name(2).get("")

proc postscriptName*(f: Face): Option[string] = f.name(6)

const WindowsLanguages = {0x0404: "zh-TW", 0x0804: "zh-CN", 0x0C04: "zh-HK", 0x1004: "zh-SG", 0x0411: "ja-JP",
  0x0412: "ko-KR", 0x0407: "de-DE", 0x040C: "fr-FR", 0x0410: "it-IT", 0x0C0A: "es-ES", 0x040A: "es-ES",
  0x0416: "pt-BR", 0x0816: "pt-PT", 0x0419: "ru-RU", 0x0408: "el-GR", 0x041F: "tr-TR", 0x0415: "pl-PL",
  0x0405: "cs-CZ", 0x0413: "nl-NL", 0x041D: "sv-SE", 0x0406: "da-DK", 0x0414: "nb-NO", 0x040B: "fi-FI",
  0x040E: "hu-HU", 0x041E: "th-TH", 0x042A: "vi-VN", 0x0401: "ar-SA", 0x040D: "he-IL", 0x0409: "en-US",
  0x0809: "en-GB", 0x0C09: "en-AU", 0x1009: "en-CA"}.toTable

proc localizedNames*(f: Face; id: int): seq[(string, Option[string])] =
  ## Every Windows/Unicode name for `id` with its BCP 47 language when known.
  for r in f.names:
    if r.nameId != id: continue
    var lang = none(string)
    if r.platform == 3:
      if r.language in WindowsLanguages: lang = some(WindowsLanguages[r.language])
    elif r.platform == 1 and r.language == 0: lang = some("en")
    result.add (r.value, lang)

proc outline*(f: Face; gid: int): Outline =
  ## The glyph's outline in font units (y up), cached. Empty for blank or unreadable glyphs.
  if gid in f.outlines: return f.outlines[gid]
  try:
    if f.cff.present: result = f.cffOutline(gid)
    elif f.hasTable("glyf"): result = f.glyfOutline(gid, 0, [1'f32, 0, 0, 1, 0, 0])
  except FontError: result = @[]
  if f.outlines.len < 4096: f.outlines[gid] = result

proc glyphBounds*(f: Face; gid: int): Option[array[4, float32]] =
  ## (xMin, yMin, xMax, yMax) of the outline's points, in font units.
  let o = f.outline(gid)
  if o.len == 0: return
  var b = [Inf.float32, Inf.float32, NegInf.float32, NegInf.float32]
  for c in o:
    if c.verb == pvClose: continue
    let k0 = if c.verb == pvCubic: 0 elif c.verb == pvQuad: 0 else: 2
    for k in k0 .. 2:
      if c.verb == pvQuad and k == 1: continue
      let p = c.pts[k]
      b[0] = min(b[0], p[0]); b[1] = min(b[1], p[1]); b[2] = max(b[2], p[0]); b[3] = max(b[3], p[1])
  if b[0] > b[2]: return
  some(b)
