## Cross-check tool for the Affinity reader: `afcheck read FILE...` / `afcheck preview FILE...` print
## the same canonical dumps as the Rust oracle's `af.read` / `af.preview`
## (`tools/oracle/src/ax_affinity.rs`): floats as bit patterns, strings escaped, pixels hashed.

import std/[os, options, strutils]
import photocraft/affinity

proc fnv(b: openArray[byte]): uint64 =
  result = 0xcbf29ce484222325'u64
  for x in b:
    result = result xor uint64(x)
    result = result * 0x100000001b3'u64

proc f(v: float64): string = $cast[uint64](v)

proc s(v: string): string =
  for c in v:
    let b = byte(c)
    if b >= 0x21 and b <= 0x7e and c != '\\': result.add c
    else: result.add "\\x" & toHex(int(b), 2).toLowerAscii

proc rect(r: AfRect): string = f(r.x0) & "," & f(r.y0) & "," & f(r.x1) & "," & f(r.y1)
proc pt(p: AfPoint): string = f(p.x) & "," & f(p.y)
proc aff(a: AfAffine): string =
  var parts: seq[string]
  for v in a.m: parts.add f(v)
  parts.join(",")

proc path(p: AfPath): string =
  result = "n=" & $p.subpaths.len
  for sp in p.subpaths:
    result.add " sp c=" & $int(sp.closed) & " s=" & pt(sp.start) & " segs="
    var segs: seq[string]
    for g in sp.segments: segs.add pt(g[0]) & ";" & pt(g[1]) & ";" & pt(g[2])
    result.add segs.join("|")

proc color(c: AfColor): string =
  case c.kind
  of acRgb: "rgb(" & f(c.r) & "," & f(c.g) & "," & f(c.b) & "," & f(c.a) & ")"
  of acCmyk: "cmyk(" & f(c.c) & "," & f(c.mg) & "," & f(c.y) & "," & f(c.k) & "," & f(c.a) & ")"
  of acGray: "gray(" & f(c.v) & "," & f(c.a) & ")"
  of acLab: "lab(" & f(c.l) & "," & f(c.la) & "," & f(c.lb) & "," & f(c.a) & ")"

proc paint(p: AfPaint): string =
  case p.kind
  of apNone: "none"
  of apSolid: "solid " & color(p.color)
  of apGradient:
    var stops: seq[string]
    for st in p.gradient.stops: stops.add f(st.offset) & "/" & color(st.color) & "/" & f(st.bias)
    "grad k=" & $ord(p.gradient.kind) & " xf=" & aff(p.gradient.transform) & " stops=" & stops.join(" ")

proc image(i: AfImage): string =
  let kind = if i.pixels.kind == pxRgba8: "rgba" else: "enc"
  "w=" & $i.width & " h=" & $i.height & " px=" & kind & " len=" & $i.pixels.data.len & " fnv=" &
    toHex(fnv(i.pixels.data), 16).toLowerAscii & " xf=" & aff(i.transform)

proc node(n: AfNode; depth: int; o: var seq[string]) =
  o.add "node d=" & $depth & " class=" & s($n.class) & " name=" & s(n.name) & " vis=" & $int(n.visible) &
    " lock=" & $int(n.locked) & " op=" & f(n.opacity) & " blend=" & $ord(n.blend)
  case n.kind.tag
  of nkLayer: o.add " kind layer"
  of nkGroup: o.add " kind group"
  of nkUnsupported: o.add " kind unsupported"
  of nkArtboard:
    o.add " kind artboard " & rect(n.kind.rect)
    for b in n.kind.background: o.add "  bg " & paint(b)
  of nkShape:
    o.add " kind shape eo=" & $int(n.kind.evenOdd)
    o.add "  path " & path(n.kind.path)
    for fl in n.kind.fills: o.add "  fill " & paint(fl)
    for st in n.kind.strokes:
      var dash = "-"
      if st.dash.isSome:
        var d: seq[string]
        for v in st.dash.get[0]: d.add f(v)
        dash = d.join(",") & " phase=" & f(st.dash.get[1])
      o.add "  stroke w=" & f(st.width) & " cap=" & $ord(st.cap) & " join=" & $ord(st.join) & " miter=" &
        f(st.miterLimit) & " align=" & $ord(st.align) & " behind=" & $int(st.behind) & " dash=" & dash
      o.add "   spaint " & paint(st.paint)
  of nkText:
    let t = n.kind.text
    o.add " kind text align=" & $ord(t.align) & " anchor=" & pt(t.anchor) & " frame=" &
      (if t.frame.isSome: rect(t.frame.get) else: "-") & " xf=" & aff(t.transform)
    for r in t.runs:
      o.add "  run text=" & s(r.text) & " ps=" & s(r.postscript) & " fam=" & s(r.family) & " w=" & $r.weight &
        " it=" & $int(r.italic) & " size=" & f(r.size) & " tr=" & f(r.tracking) & " lead=" &
        (if r.leading.isSome: f(r.leading.get) else: "-") & " fill=" & paint(r.fill)
  of nkImage: o.add " kind image " & image(n.kind.image)
  if n.mask.isSome: o.add " mask " & path(n.mask.get)
  if n.pixelMask.isSome: o.add " pmask " & image(n.pixelMask.get)
  for c in n.children: node(c, depth + 1, o)

proc dump*(d: AfDocument): seq[string] =
  result.add "ok dpi=" & f(d.dpi) & " saved=" & (if d.savedBy.isSome: s(d.savedBy.get) else: "-")
  for sp in d.spreads:
    result.add "spread " & rect(sp.bounds) & " transparent=" & $int(sp.transparent) & " pages=" & $sp.pages.len
    for p in sp.pages: result.add " page " & rect(p)
    for n in sp.nodes: node(n, 0, result)
  for w in d.warnings: result.add "warn " & s(w)

proc bytesOf(path: string): seq[byte] =
  let t = readFile(path)
  result = newSeq[byte](t.len)
  for i, c in t: result[i] = byte(c)

when isMainModule:
  let args = commandLineParams()
  case args[0]
  of "read":
    for p in args[1 .. ^1]:
      echo "== ", p
      let b = bytesOf(p)
      try:
        for l in dump(read(b)): echo l
      except AffinityError as e:
        echo "err ", e.msg
  of "preview":
    for p in args[1 .. ^1]:
      echo "== ", p
      let b = bytesOf(p)
      try:
        let pv = preview(b)
        echo "ok ", pv.width, " ", pv.height, " ", pv.pngStart, " ", pv.pngLen
      except AffinityError as e:
        echo "err ", e.msg
  else: quit "usage: afcheck read|preview FILE..."
