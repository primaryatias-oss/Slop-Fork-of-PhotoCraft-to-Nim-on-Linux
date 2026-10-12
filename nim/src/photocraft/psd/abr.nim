## Photoshop brush files (`.abr`): the legacy v1/v2 layout and the sectioned v6+ layout (`samp`
## sampled tips, `patt` patterns, `desc` brush presets, `phry` preset folders). Implemented from
## public descriptions of the format. Never crashes: lengths are bounds-checked, sizes capped, and
## a damaged brush is skipped with a warning when the rest of the file is readable.

import std/[strutils, strformat, options, unicode]
import ./io, ./compression, ./descriptor, ./patterns

const
  AbrMaxEdge* = 8192
  AbrMaxBrushes* = 20_000
  AbrMaxTotalBytes* = 512 shl 20
  AbrMaxFolderDepth* = 32
  MaxFolderName = 255
  MaxFolderBytes = 16 shl 20

type
  AbrSample* = object
    id*: string
    width*, height*: int
    depth*: int
    data*: seq[byte]  ## row-major, 16-bit big-endian at depth 16
  LegacyTipKind* = enum ltComputed, ltSampled
  LegacyTip* = object
    case kind*: LegacyTipKind
    of ltComputed:
      diameter*, hardness*: uint16
      angle*: int16
      roundness*: uint16
    of ltSampled:
      sample*: AbrSample
  LegacyBrush* = object
    name*: string
    spacing*: uint16
    antiAlias*: bool
    tip*: LegacyTip
  AbrFile* = object
    version*, subversion*: uint16
    legacy*: seq[LegacyBrush]
    samples*: seq[AbrSample]
    patterns*: seq[PsdPattern]
    presets*: seq[Descriptor]
    folders*: seq[seq[string]]
    warnings*: seq[string]

proc toUnit*(s: AbrSample): seq[float32] =
  if s.depth == 16:
    result = newSeq[float32](s.data.len div 2)
    for i in 0 ..< result.len: result[i] = float32(beU16(s.data, 2 * i)) / 65535
  else:
    result = newSeq[float32](s.data.len)
    for i, v in s.data: result[i] = float32(v) / 255

proc readSampleBody(r: var Reader; id: string; budget: var int): AbrSample =
  let (top, left, bottom, right) = (int64(r.i32()), int64(r.i32()), int64(r.i32()), int64(r.i32()))
  let w = right - left
  let h = bottom - top
  if w <= 0 or h <= 0: raise invalid(&"brush sample bounds {w}×{h}")
  if w > AbrMaxEdge or h > AbrMaxEdge: raise limitExceeded("brush sample larger than MAX_EDGE")
  let depth = int(r.u16())
  if depth != 8 and depth != 16: raise invalid("brush sample depth " & $depth)
  let comp = r.u8()
  if comp > 1: raise unsupported("brush sample compression " & $comp)
  let layout = PlaneLayout(planes: 1, width: int(w), height: int(h), depth: depth, version: vPsd)
  let need = layout.decodedLen
  if need > budget: raise limitExceeded("abr samples exceed MAX_TOTAL_BYTES")
  budget -= need
  let rest = r.bytes(r.remaining)
  AbrSample(id: id, width: int(w), height: int(h), depth: depth,
            data: decodePlanes(if comp == 1: cRle else: cRaw, rest, layout))

proc readUcs2(r: var Reader): string =
  let n = int(r.u32())
  if n > 4096: raise limitExceeded("abr name length")
  r.checkCount(uint64(n), 2)
  var units: seq[uint16]
  for _ in 0 ..< n: units.add r.u16()
  while units.len > 0 and units[^1] == 0: units.setLen(units.len - 1)
  fromUtf16(units)

proc isBudget(e: ref PsdError): bool = e.kind == peLimitExceeded and "MAX_TOTAL_BYTES" in e.msg

proc parseV12(r: var Reader; version: uint16; o: var AbrFile) =
  let count = int(r.u16())
  var budget = AbrMaxTotalBytes
  for i in 0 ..< count:
    if r.isEmpty:
      o.warnings.add &"file ends after {i} of {count} brushes"
      break
    let ty = r.u16()
    let ln = int(r.u32())
    if ln > r.remaining:
      o.warnings.add &"brush {i + 1}: truncated"
      break
    var b = r.sub(uint64(ln))
    try:
      case ty
      of 1:
        discard b.u32()
        let spacing = b.u16()
        let diameter = b.u16()
        let roundness = b.u16()
        let angle = cast[int16](b.u16())
        let hardness = b.u16()
        o.legacy.add LegacyBrush(spacing: spacing, antiAlias: true,
          tip: LegacyTip(kind: ltComputed, diameter: diameter, hardness: hardness, angle: angle, roundness: roundness))
      of 2:
        discard b.u32()
        let spacing = b.u16()
        let name = if version == 2: readUcs2(b) else: ""
        let aa = b.u8() != 0
        b.skip(8)
        let s = readSampleBody(b, "", budget)
        o.legacy.add LegacyBrush(name: name, spacing: spacing, antiAlias: aa, tip: LegacyTip(kind: ltSampled, sample: s))
      else: o.warnings.add &"brush {i + 1}: unknown brush type {ty} skipped"
    except PsdError as e:
      if isBudget(e): raise
      o.warnings.add &"brush {i + 1}: {e.msg}; skipped"

proc readSamp(data: openArray[byte]; subversion: uint16; o: var AbrFile; budget: var int) =
  var r = newReader(data)
  while r.remaining >= 4:
    if o.samples.len >= AbrMaxBrushes:
      o.warnings.add "too many sampled tips; the rest were skipped"
      break
    let ln = int(r.u32())
    if ln == 0: break
    let padded = (ln + 3) and not 3
    let take = min(padded, r.remaining)
    if take < ln:
      o.warnings.add &"sampled tip {o.samples.len + 1}: truncated"
      break
    var b = r.sub(uint64(take))
    try:
      let n = int(b.u8())
      let id = decodeLegacyName(b.bytes(n))
      b.skip(if subversion == 1: 10 else: 264)
      o.samples.add readSampleBody(b, id, budget)
    except PsdError as e:
      if isBudget(e): raise
      o.warnings.add &"sampled tip {o.samples.len + 1}: {e.msg}; skipped"

proc readDesc(data: openArray[byte]; o: var AbrFile) =
  try:
    let (vd, _) = parseVersionedPrefix(data)
    let l = vd.descriptor.getList("Brsh")
    if l.isSome:
      for i, v in l.get:
        if i >= AbrMaxBrushes: break
        if v.kind == vkDescriptor: o.presets.add v.desc
    else: o.warnings.add "brush settings (desc) hold no brush list"
  except PsdError as e:
    o.warnings.add "brush settings (desc) unreadable: " & e.msg

proc readPhry(data: openArray[byte]; presets: int; warnings: var seq[string]): (bool, seq[seq[string]]) =
  var vd: VersionedDescriptor
  try: vd = parseVersionedPrefix(data)[0]
  except PsdError as e:
    warnings.add "brush folders (phry) unreadable: " & e.msg & "; brushes imported without folders"
    return (false, @[])
  let items = vd.descriptor.getList("hierarchy")
  if items.isNone:
    warnings.add "brush folders (phry) hold no hierarchy; brushes imported without folders"
    return (false, @[])
  var stack: seq[string]
  var overflow = 0
  var outp: seq[seq[string]]
  var listed, bytes = 0
  var unbalanced, tooDeep, tooBig = false
  for v in items.get:
    if v.kind != vkDescriptor: continue
    let d = v.desc
    if d.classId.s == "Grup":
      if stack.len >= AbrMaxFolderDepth:
        inc overflow
        tooDeep = true
        continue
      var name = strutils.strip(d.getText("Nm  ").get(""))
      if name.runeLen > MaxFolderName: name = name.runeSubStr(0, MaxFolderName)
      stack.add(if name.len == 0: "Group" else: name)
    elif d.classId.s == "groupEnd":
      if overflow > 0: dec overflow
      elif stack.len == 0: unbalanced = true
      else: discard stack.pop()
    elif d.classId.s == "preset":
      inc listed
      if outp.len >= presets: continue
      var size = 0
      for n in stack: size += n.len + 16
      if not tooBig and bytes + size <= MaxFolderBytes:
        bytes += size
        outp.add stack
      else:
        tooBig = true
        outp.add newSeq[string]()
  if unbalanced: warnings.add "brush folders (phry): a folder end without a folder was ignored"
  if tooDeep: warnings.add &"brush folders nested deeper than {AbrMaxFolderDepth} levels were merged into their parent"
  if tooBig: warnings.add "brush folders (phry) too large; the remaining brushes were imported at the top level"
  if listed != presets:
    warnings.add &"brush folders (phry) list {listed} brushes but the file holds {presets}; " &
      (if listed > presets: "the extra entries were ignored" else: "the rest were imported at the top level")
    outp.setLen(presets)
  (true, outp)

proc parseV6(r: var Reader; o: var AbrFile) =
  o.subversion = r.u16()
  if o.subversion notin [1'u16, 2]: raise unsupported("brush file subversion " & $o.subversion)
  var budget = AbrMaxTotalBytes
  var hierarchy: seq[byte]
  var hasHierarchy = false
  while r.remaining >= 12:
    let sig = r.fourcc()
    if sig != "8BIM":
      o.warnings.add &"unexpected data at offset {r.pos - 4} ignored"
      break
    let key = r.fourcc()
    let ln = int(r.u32())
    var data: seq[byte]
    if ln > r.remaining:
      o.warnings.add "section " & key & " truncated"
      data = r.bytes(r.remaining)
    else: data = r.bytes(ln)
    let pad = (4 - ln mod 4) mod 4
    template sigAt(p0: int): bool =
      p0 + 4 <= r.size and char(r.at(p0)) == '8' and char(r.at(p0 + 1)) == 'B' and char(r.at(p0 + 2)) == 'I' and char(r.at(p0 + 3)) == 'M'
    if pad > 0 and not sigAt(r.pos) and sigAt(r.pos + pad): r.pos += pad
    case key
    of "samp": readSamp(data, o.subversion, o, budget)
    of "patt":
      try: o.patterns.add parsePatternBlock(data)
      except PsdError as e: o.warnings.add "embedded patterns unreadable: " & e.msg
    of "desc": readDesc(data, o)
    of "phry":
      hierarchy = data
      hasHierarchy = true
    of "lPdc": discard
    else: o.warnings.add "section " & key & " ignored"
  if hasHierarchy:
    let (ok, folders) = readPhry(hierarchy, o.presets.len, o.warnings)
    if ok:
      var any = false
      for f in folders:
        if f.len > 0: any = true
      if any: o.folders = folders

proc parseAbr*(data: openArray[byte]): AbrFile =
  var r = newReader(data)
  result.version = r.u16()
  case result.version
  of 1, 2: parseV12(r, result.version, result)
  of 6..10: parseV6(r, result)
  else: raise unsupported("brush file version " & $result.version)
  if result.legacy.len == 0 and result.samples.len == 0 and result.presets.len == 0:
    let why = if result.warnings.len > 0: " (" & result.warnings[0] & ")" else: ""
    raise invalid("the brush file holds no readable brushes" & why)

# ---------------------------------------------------------------- writing

proc writeSampleBody(o: var seq[byte]; s: AbrSample; rle: bool) =
  if s.width == 0 or s.height == 0 or s.width > AbrMaxEdge or s.height > AbrMaxEdge or s.depth notin [8, 16]:
    raise limitExceeded("brush sample size or depth")
  for v in [0'i32, 0, int32(s.height), int32(s.width)]: o.putI32(v)
  o.putU16(uint16(s.depth))
  o.putU8(if rle: 1 else: 0)
  let layout = PlaneLayout(planes: 1, width: s.width, height: s.height, depth: s.depth, version: vPsd)
  o.add encodePlanes(if rle: cRle else: cRaw, s.data, layout)

proc writeAbrV12*(version: uint16; brushes: openArray[LegacyBrush]; rle: bool): seq[byte] =
  if version notin [1'u16, 2] or brushes.len > 65535: raise unsupported("writing brush file version " & $version)
  result.putU16(version)
  result.putU16(uint16(brushes.len))
  for b in brushes:
    var body: seq[byte]
    body.putU32(0)
    body.putU16(b.spacing)
    var ty = 1'u16
    case b.tip.kind
    of ltComputed:
      for v in [b.tip.diameter, b.tip.roundness, cast[uint16](b.tip.angle), b.tip.hardness]: body.putU16(v)
    of ltSampled:
      ty = 2
      let s = b.tip.sample
      if version == 2:
        let units = utf16Units(b.name) & @[0'u16]
        body.putU32(uint32(units.len))
        for u in units: body.putU16(u)
      body.putU8(if b.antiAlias: 1 else: 0)
      for v in [0'u16, 0, uint16(min(s.height, 0xFFFF)), uint16(min(s.width, 0xFFFF))]: body.putU16(v)
      writeSampleBody(body, s, rle)
    result.putU16(ty)
    result.putU32(uint32(body.len))
    result.put(body)

proc section(o: var seq[byte]; key: string; data: openArray[byte]) =
  o.put("8BIM")
  o.put(key)
  let pad = (4 - data.len mod 4) mod 4
  o.putU32(uint32(data.len + pad))
  o.put(data)
  for _ in 0 ..< pad: o.add 0

proc fnvPath(path: openArray[string]; sq: int; seed: uint64): uint64 =
    var h = 0xcbf29ce484222325'u64 xor seed
    template mix(b: uint64) =
      h = h xor b
      h = h * 0x100000001b3'u64
    for s in path:
      for c in s: mix(uint64(byte(c)))
      mix(0)
    for i in 0 ..< 8: mix((uint64(sq) shr (8 * i)) and 0xFF)
    h

proc folderUuid(path: openArray[string]; sq: int): string =
  let (a, b) = (fnvPath(path, sq, 1), fnvPath(path, sq, 2))
  &"{a shr 32:08x}-{(a shr 16) and 0xffff:04x}-{a and 0xffff:04x}-{b shr 48:04x}-{b and 0xffffffffffff'u64:012x}"

proc hierarchyDescriptor(folders: openArray[seq[string]]): Descriptor =
  var items: seq[Value]
  var open: seq[string]
  var sq = 0
  for path0 in folders:
    let path = path0[0 ..< min(path0.len, AbrMaxFolderDepth)]
    var common = 0
    while common < open.len and common < path.len and open[common] == path[common]: inc common
    while open.len > common:
      discard open.pop()
      items.add vDesc(newDescriptor("groupEnd"))
    for name in path[common .. ^1]:
      open.add name
      inc sq
      items.add vDesc(newDescriptor("Grup").with("Nm  ", vText(name)).with("zuid", vText(folderUuid(open, sq))))
    items.add vDesc(newDescriptor("preset"))
  for _ in open: items.add vDesc(newDescriptor("groupEnd"))
  newDescriptor("null").with("hierarchy", vList(items))

proc writeAbrV6*(subversion: uint16; samples: openArray[AbrSample]; patterns: openArray[PsdPattern];
                 presets: openArray[Descriptor]; folders: openArray[seq[string]] = []; rle = true): seq[byte] =
  ## A v6 file: sampled tips, embedded patterns, preset descriptors (`Brsh`) and, when any preset
  ## is in a folder, the preset hierarchy (`phry`).
  if folders.len > 0 and folders.len != presets.len:
    raise invalid(&"{folders.len} folder paths for {presets.len} brush presets")
  if subversion notin [1'u16, 2]: raise unsupported("brush file subversion " & $subversion)
  result.putU16(6)
  result.putU16(subversion)
  var samp: seq[byte]
  for s in samples:
    var e: seq[byte]
    let n = min(s.id.len, 255)
    e.putU8(uint8(n))
    for i in 0 ..< n: e.add byte(s.id[i])
    for _ in 0 ..< (if subversion == 1: 10 else: 264): e.add 0
    writeSampleBody(e, s, rle)
    samp.putU32(uint32(e.len))
    samp.put(e)
    for _ in 0 ..< (4 - e.len mod 4) mod 4: samp.add 0
  section(result, "samp", samp)
  if patterns.len > 0: section(result, "patt", writePatternBlock(patterns))
  var list: seq[Value]
  for p in presets: list.add vDesc(p)
  section(result, "desc", versioned(newDescriptor("null").with("Brsh", vList(list))).toBytes)
  var any = false
  for f in folders:
    if f.len > 0: any = true
  if any: section(result, "phry", versioned(hierarchyDescriptor(folders)).toBytes)
