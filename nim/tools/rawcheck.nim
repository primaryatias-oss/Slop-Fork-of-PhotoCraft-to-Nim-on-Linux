## Nim side of the camera raw cross-check (see `tools/algocheck.py`, `tools/algocases/raw.json` and
## PORTING.md): the commands of the Rust oracle's `ax_raw.rs`, printing the same canonical
## summaries (floats as bit patterns) and writing the same bulk data.
##
##   rawcheck <command> <args…> <out>   runs one command (as `photocraft-oracle <command> …`)
##
## The input files are written by the oracle (`photocraft-oracle raw.gen <dir> -`), so both sides
## read the bytes the Rust generator produced.

import std/[os, strutils, options]
import photocraft/raw

proc fnv(b: openArray[byte]): uint64 =
  result = 0xcbf2_9ce4_8422_2325'u64
  for x in b:
    result = result xor uint64(x)
    result = result * 0x100_0000_01b3'u64

proc le16(v: openArray[uint16]): seq[byte] =
  result = newSeqOfCap[byte](v.len * 2)
  for x in v:
    result.add byte(x and 0xFF)
    result.add byte(x shr 8)

proc h16(v: openArray[uint16]): string = $v.len & " " & toHex(fnv(le16(v)), 16).toLowerAscii

proc b32(v: openArray[float32]): string =
  var parts: seq[string]
  for x in v: parts.add $cast[uint32](x)
  parts.join(",")

proc b64(v: openArray[float64]): string =
  var parts: seq[string]
  for x in v: parts.add $cast[uint64](x)
  parts.join(",")

proc mat(m: Mat3): string = b64([m[0][0], m[0][1], m[0][2], m[1][0], m[1][1], m[1][2], m[2][0], m[2][1], m[2][2]])
proc opt(o: Option[string]): string = o.get("-")
proc rect(r: RawRect): string = $r.x & " " & $r.y & " " & $r.width & " " & $r.height

proc sensorSummary(s: Sensor): string =
  var o: seq[string]
  o.add "format " & $s.format
  o.add "make " & opt(s.make)
  o.add "model " & opt(s.model)
  o.add "size " & $s.width & " " & $s.height & " " & $s.samples
  o.add "data " & h16(s.data)
  if s.cfa.isSome:
    let c = s.cfa.get
    var cs: seq[string]
    for v in c.colors: cs.add $v
    o.add "cfa " & $c.width & " " & $c.height & " " & cs.join(",") & " " & $c.originX & " " & $c.originY
  else: o.add "cfa -"
  o.add "lin " & (if s.linearization.isSome: h16(s.linearization.get) else: "-")
  o.add "black " & $s.black.rows & " " & $s.black.cols & " " & b32(s.black.values) & " | " & b32(s.black.deltaH) &
    " | " & b32(s.black.deltaV)
  o.add "white " & b32(s.white)
  o.add "active " & rect(s.active)
  o.add "crop " & rect(s.crop)
  for c in s.color.calibrations:
    o.add "cal " & $cast[uint64](c.temperature) & " " & mat(c.colorMatrix) & " " &
      (if c.forwardMatrix.isSome: mat(c.forwardMatrix.get) else: "-") & " " & mat(c.cameraCalibration)
  o.add "analog " & (if s.color.analogBalance.isSome: b64(s.color.analogBalance.get) else: "-")
  o.add "neutral " & (if s.color.asShotNeutral.isSome: b64(s.color.asShotNeutral.get) else: "-")
  o.add "xy " & (if s.color.asShotWhiteXy.isSome: b64(s.color.asShotWhiteXy.get) else: "-")
  o.add "camwb " & (if s.cameraWb.isSome: b64(s.cameraWb.get) else: "-")
  o.add "orientation " & $s.orientation
  o.add "baseline " & $cast[uint64](s.baselineExposure)
  for g in s.gainMaps:
    o.add "gain " & [$g.top, $g.left, $g.bottom, $g.right, $g.plane, $g.planes, $g.rowPitch, $g.colPitch, $g.pointsV,
                     $g.pointsH, $cast[uint64](g.spacingV), $cast[uint64](g.spacingH), $cast[uint64](g.originV),
                     $cast[uint64](g.originH), $g.mapPlanes, b32(g.gains)].join(" ")
  var tone: seq[float32]
  for p in s.toneCurve:
    tone.add p[0]
    tone.add p[1]
  o.add "tone " & b32(tone)
  for w in s.warnings: o.add "warn " & w
  o.join("\n")

proc mutate(b0: string; spec: string): seq[byte] =
  result = newSeq[byte](b0.len)
  for i, c in b0: result[i] = byte(c)
  let p = spec.split(':')
  case p[0]
  of "trunc":
    let n = parseInt(p[1])
    if n < result.len: result.setLen(n)
  of "flip":
    var s = uint32(parseBiggestUInt(p[1]))
    let k = parseInt(p[2])
    for _ in 0 ..< k:
      s = s * 1_103_515_245'u32 + 12345'u32
      if result.len == 0: break
      let at = int(s shr 8) mod result.len
      result[at] = result[at] xor byte(1 shl (s mod 8))
  else: discard

proc wb(spec: string): WhiteBalance =
  case spec
  of "asshot": asShotWb()
  of "auto": autoWb()
  else:
    let v = spec.replace("m:", "").split(',')
    multipliersWb([parseFloat(v[0]), parseFloat(v[1]), parseFloat(v[2])])

proc lookSummary(l: CameraLook): string =
  var knots: seq[float64]
  if l.tone.isSome:
    for k in l.tone.get.knots:
      knots.add k[0]
      knots.add k[1]
  "look " & mat(l.toXyz) & " | " & (if l.tone.isSome: b64(knots) else: "-") & " | " & $cast[uint64](l.deltaE) & " " &
    $cast[uint64](l.deltaEFallback)

proc toBytes(s: string): seq[byte] =
  result = newSeq[byte](s.len)
  for i, c in s: result[i] = byte(c)

proc writeBytes(path: string; b: openArray[byte]) =
  var s = newString(b.len)
  for i, x in b: s[i] = char(x)
  writeFile(path, s)

proc rawCmd(a: seq[string]): bool =
  let outp = a[^1]
  case a[1]
  of "raw.decode":
    let b = mutate(readFile(a[2]), a[3])
    let f = identify(b)
    echo "identify ", (if f.isSome: $f.get else: "-")
    try:
      let s = decode(b)
      echo sensorSummary(s)
      writeBytes(outp, le16(s.data))
    except RawError as e:
      echo "ERR ", e.msg
  of "raw.develop":
    let b = mutate(readFile(a[2]), a[3])
    var opts = defaultDevelopOptions()
    opts.demosaic = demosaicFromId(a[4]).get
    opts.whiteBalance = wb(a[5])
    opts.exposure = parseFloat(a[6])
    opts.orient = a[7] == "1"
    var s: Sensor
    try: s = decode(b, opts.limits)
    except RawError as e:
      echo "ERR ", e.msg
      return true
    if a[8] != "-":
      let p = a[8].split(':')
      let r = Reference(width: uint32(parseInt(p[1])), height: uint32(parseInt(p[2])), rgb: toBytes(readFile(p[0])))
      try:
        let l = fitLook(s, opts, r)
        echo lookSummary(l)
        opts.cameraLook = some(l)
      except LookRejected as e:
        echo "REJECT ", e.msg
    try:
      let d = developSensor(s, opts)
      echo "dev ", d.width, " ", d.height, " ", h16(d.rgb)
      let i = d.info
      echo "info ", [$i.format, opt(i.make), opt(i.model), $i.sensorWidth, $i.sensorHeight, opt(i.cfa),
                     b64(i.wbMultipliers), $i.orientation, $cast[uint64](i.baselineExposure)].join(" ")
      for w in d.warnings: echo "warn ", w
      writeBytes(outp, le16(d.rgb))
    except RawError as e:
      echo "ERR ", e.msg
  of "raw.look":
    var s: Sensor
    try: s = decode(toBytes(readFile(a[2])))
    except RawError as e:
      echo "ERR ", e.msg
      return true
    if a[7] != "-": s.orientation = uint16(parseInt(a[7]))
    var opts = defaultDevelopOptions()
    opts.whiteBalance = wb(a[6])
    let r = Reference(width: uint32(parseInt(a[4])), height: uint32(parseInt(a[5])), rgb: toBytes(readFile(a[3])))
    try: echo lookSummary(fitLook(s, opts, r))
    except LookRejected as e: echo "REJECT ", e.msg
  of "raw.preview":
    let b = mutate(readFile(a[2]), a[3])
    for p in embeddedPreviews(b): echo "preview ", p.offset, " ", p.len, " ", p.width, " ", p.height
    let p = embeddedPreview(b)
    if p.isSome: echo "best ", p.get.offset, " ", p.get.len, " ", p.get.width, " ", p.get.height
    else: echo "best -"
  of "raw.lj":
    let b = toBytes(readFile(a[2]))
    try:
      let (w, h, c, v) = decodeLosslessJpeg(b, parseInt(a[3]))
      echo "lj ", w, " ", h, " ", c, " ", h16(v)
    except RawError as e:
      echo "ERR ", e.msg
  of "raw.dump":
    stdout.write dumpStructure(mutate(readFile(a[2]), a[3]))
  else: return false
  true

when isMainModule:
  let a = @[getAppFilename()] & commandLineParams()
  if a.len < 2: quit("usage: rawcheck <command> <args…> <out>")
  if not rawCmd(a): quit("unknown command " & a[1])
