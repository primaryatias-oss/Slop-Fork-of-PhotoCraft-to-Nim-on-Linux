## Document patterns ↔ PSD pattern blocks (`Patt`/`Pat2`/`Pat3`) and `.pat` files.

import std/[options]
import ../geom, ../color, ../raster, ../doc, ../psd, ./pixels

const PatternKeys* = ["Patt", "Pat2", "Pat3"]

proc psdMode(m: ColorMode): uint32 =
  case m
  of cmGrayscale: 1
  of cmCmyk: 4
  of cmLab: 9
  else: 3

proc fromPsd*(p: PsdPattern): Option[Pattern] =
  let (w, h) = (p.width, p.height)
  if w <= 0 or h <= 0: return none(Pattern)
  var channels: seq[seq[byte]]
  var depth = p.depth
  if p.depth == 1:
    let row = (w + 7) div 8
    for plane in p.channels:
      var e = newSeq[byte](w * h)
      for i in 0 ..< w * h:
        let bi = (i div w) * row + (i mod w) div 8
        e[i] = (if bi < plane.len and (plane[bi] and (0x80'u8 shr ((i mod w) mod 8))) != 0: 0'u8 else: 255'u8)
      channels.add e
    depth = 8
  else: channels = p.channels
  var mode: ColorMode
  case p.mode
  of 1, 8: mode = cmGrayscale
  of 2:
    if p.palette.isNone or channels.len == 0: return none(Pattern)
    let pal = p.palette.get
    var ch: seq[seq[byte]]
    for c in 0 .. 2:
      var plane = newSeq[byte](channels[0].len)
      for i, idx in channels[0]:
        let k = int(idx) * 3 + c
        plane[i] = (if k < pal.len: pal[k] else: 0'u8)
      ch.add plane
    channels = ch
    mode = cmRgb
  of 4: mode = cmCmyk
  of 9: mode = cmLab
  of 3: mode = cmRgb
  else:
    mode = cmGrayscale
    if channels.len > 1: channels.setLen(1)
  let s = sampleForDepth(depth)
  let fmt = pixelFormat(mode, s, p.alpha.isSome)
  let cc = modeChannels(psdMode(mode))
  if channels.len < cc: return none(Pattern)
  var refs: seq[seq[byte]]
  var present: seq[bool]
  var fill: seq[seq[byte]]
  var invert: seq[bool]
  for i in 0 ..< cc:
    refs.add channels[i]
    present.add true
    fill.add zeroSample(s)
    invert.add mode == cmCmyk
  if p.alpha.isSome:
    refs.add p.alpha.get
    present.add true
    fill.add maxSample(s)
    invert.add false
  var bytes = interleave(refs, present, fill, w * h, s, invert)
  if mode == cmLab and s == stU16: lab16Chroma(bytes, refs.len, true)
  some(Pattern(id: p.id, name: p.name, width: p.width, height: p.height,
               surface: surfaceFromInterleaved(fmt, rect(0, 0, w, h), bytes)))

proc toPsd*(p: Pattern): PsdPattern =
  let f = p.surface.format
  let mode = (if f.mode in {cmGrayscale, cmRgb, cmCmyk, cmLab}: f.mode else: cmRgb)
  let fmt = pixelFormat(mode, f.sample, f.alpha)
  let surf = (if fmt == f: p.surface else: p.surface.convert(fmt))
  var bytes = surf.toInterleaved(rect(0, 0, p.width, p.height))
  let ch = fmt.channels
  if mode == cmLab and fmt.sample == stU16: lab16Chroma(bytes, ch, false)
  var invert = newSeq[bool](ch)
  for v in invert.mitems: v = mode == cmCmyk
  if fmt.alpha: invert[ch - 1] = false
  var planes = deinterleave(bytes, ch, fmt.sample, invert)
  var alpha = none(seq[byte])
  if fmt.alpha: alpha = some(planes.pop)
  PsdPattern(mode: psdMode(mode), width: p.width, height: p.height, name: p.name, id: p.id,
             depth: psdDepth(fmt.sample), channels: planes, alpha: alpha)

proc fromGlobalBlocks*(d: Document): seq[Pattern] =
  for b in d.metadata.psdGlobalBlocks:
    if b.key notin PatternKeys: continue
    var ps: seq[PsdPattern]
    try: ps = parsePatternBlock(b.data.data)
    except CatchableError: continue
    for p in ps:
      let x = fromPsd(p)
      if x.isSome: result.add x.get

proc exportGlobalBlocks*(d: Document): seq[PsdGlobalBlock] =
  let raw = d.metadata.psdGlobalBlocks
  if fromGlobalBlocks(d) == d.patterns: return raw
  let key = patternBlockKey(psdDepth(d.depth))
  var ps: seq[PsdPattern]
  for p in d.patterns: ps.add toPsd(p)
  var fresh: seq[byte]
  try: fresh = writePatternBlock(ps)
  except CatchableError: fresh = @[]
  var placed = false
  for b in raw:
    if b.key in PatternKeys:
      if not placed:
        result.add PsdGlobalBlock(signature: "8BIM", key: key, data: SharedBytes(data: fresh))
        placed = true
    else: result.add b
  if not placed and d.patterns.len > 0:
    result.add PsdGlobalBlock(signature: "8BIM", key: key, data: SharedBytes(data: fresh))

proc readPat*(bytes: openArray[byte]): seq[Pattern] =
  ## Patterns of a `.pat` file; raises ValueError when it is unreadable.
  var ps: seq[PsdPattern]
  try: ps = parsePatFile(bytes)
  except CatchableError as e: raise newException(ValueError, "not a readable .pat file: " & e.msg)
  for p in ps:
    let x = fromPsd(p)
    if x.isSome: result.add x.get

proc writePat*(patterns: openArray[Pattern]): seq[byte] =
  var ps: seq[PsdPattern]
  for p in patterns: ps.add toPsd(p)
  writePatFile(ps)
