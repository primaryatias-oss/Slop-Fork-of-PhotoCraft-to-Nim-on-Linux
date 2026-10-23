## File-menu commands beyond New/Close (port of `file_cmds.rs` and `guide_layout.rs`): Close All /
## Others, Revert, Save a Copy, Open As, Place Embedded / Linked, File Info, the Automate and
## Scripts items (Fit Image, Conditional Mode Change, Image Processor, Load Files into Stack,
## Flatten All Layer Effects / Masks), Export › Layers to Files and Color Lookup Tables, plus the
## document-level guide layouts of the View menu (New Guide Layout, New Guides From Shape, Clear
## Canvas Guides). Batch plays recorded actions and lands with the Actions panel.
##
## Commands that touch the file system take explicit paths (the UI asks for them with its own
## pickers); their byte-level cores (`placeBytes`, `openBytesAs`) work everywhere.

import std/[json, options, os, osproc, strutils, unicode, math, algorithm, sets, tables, sequtils, posix]
import ./core
import ./actions_cmds
import ../compose, ../io
import ../algo/resample as aresample
import ../algo/exif as aexif
import ../cms/lutfile
import ../rfmt, ../rmath
from ../format import NulPathMsg

# ---------------------------------------------------------------- predicates

proc hasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")

proc nativeE*(s: Session): string =
  ## The file system is reachable (not on the web).
  when defined(js) or defined(wasm32): "not available on the web (no file system)" else: ""

proc nativeDocE*(s: Session): string =
  result = nativeE(s)
  if result.len == 0: result = hasDocE(s)

proc isFile(path: string): bool =
  when defined(js) or defined(wasm32): false
  else:
    if '\0' in path: return false
    var st: Stat
    stat(path.cstring, st) == 0 and S_ISREG(st.st_mode)

proc canRevertE(s: Session): string =
  result = nativeE(s)
  if result.len > 0: return
  let d = s.active
  if d.isNil: return "no document open"
  if d.path.isNone: return "the document has never been saved"
  if not isFile(d.path.get): return d.path.get & " is not a file on disk"

proc hasAdjustmentsE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  for l in d.doc.layers:
    if l.visible and l.content.kind == lkAdjustment: return ""
  "the document has no visible adjustment layers to export"

proc hasShapeE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document open"
  if d.activeLayer.isSome:
    let l = d.doc[].findLayer(d.activeLayer.get)
    if not l.isNil and l[].content.kind == lkShape: return ""
  "the active layer is not a shape layer"

# ---------------------------------------------------------------- file system and names

const Openable = ["pdn", "ora", "psd", "psb", "pcraft", "png", "jpg", "jpeg", "tif", "tiff", "webp", "gif", "bmp", "tga",
                  "exr", "hdr", "qoi", "ico", "pnm", "ppm", "pgm", "fits", "fit", "fts", "heic", "heif", "hif", "dng",
                  "cr2", "nef", "nrw", "arw", "pef", "svg", "svgz", "af", "afdesign", "afphoto", "afpub"]
  ## Extensions the batch commands pick up from a folder.

proc listImages*(dir: string): seq[string] =
  ## Files of a folder that look like images we can open, sorted by name.
  if '\0' in dir: raise other(dir & ": " & NulPathMsg)
  var d = opendir(dir.cstring)
  if d.isNil: raise other(dir & ": " & osErrorText(osLastError()))
  discard closedir(d)
  try:
    for kind, f in walkDir(dir):
      if isFile(f):
        let e = splitFile(f).ext
        if e.len > 1 and e[1 .. ^1].toLowerAscii in Openable: result.add f
  except OSError: raise other(dir & ": " & osErrorText(osLastError()))
  result.sort(cmp[string])

proc fileName*(path: string): string =
  var i = path.len
  while i > 0 and path[i - 1] notin {'/', '\\'}: dec i
  path[i .. ^1]

proc extension*(path: string): Option[string] =
  ## The lower-case extension of the file name in `path` (none for `.hidden` or `name`).
  let n = fileName(path)
  let i = n.rfind('.')
  if i <= 0 or i == n.high: none(string) else: some(n[i + 1 .. ^1].toLowerAscii)

proc layeredExtension*(ext: string): bool = ext in ["psd", "psb", "pcraft", "ora"]
proc savesInPlace*(path: string): bool =
  let e = extension(path)
  e.isSome and layeredExtension(e.get)
proc isTemplate*(path: string): bool = extension(path) == some("psdt")

proc untitledName*(base: string; taken: proc(n: string): bool): string =
  var i = 1
  while true:
    let n = base & "-" & $i
    if not taken(n): return n
    inc i

proc templateName*(s: Session; path: string): Option[string] =
  ## The name a document opened from a template gets: the first "Untitled-N" no open document has.
  if not isTemplate(path): return
  some(untitledName("Untitled", proc(n: string): bool =
    for d in s.documents:
      if d.doc.name == n: return true
    false))

proc stem*(path: string): string =
  let n = fileName(path)
  let i = n.rfind('.')
  if i > 0: n[0 ..< i] else: n

proc joinPath*(dir, name: string): string =
  if dir.len == 0: name
  elif dir.endsWith("/") or dir.endsWith("\\"): dir & name
  else: dir & "/" & name

proc sanitize*(name: string): string =
  ## A file-name-safe version of a layer name.
  var s = ""
  for r in name.runes:
    if r.isAlpha or (r.int32 < 128 and char(r.int32) in Digits) or $r in ["-", "_", " ", "."]: s.add $r
    else: s.add '_'
  s = unicode.strip(s)
  while s.startsWith("."): s = s[1 .. ^1]
  while s.endsWith("."): s.setLen(s.len - 1)
  if s.len == 0: "layer" else: s

# ---------------------------------------------------------------- saving

type SaveOpts* = object
  ## What a headless save writes beyond the format: JPEG quality, TIFF layers, Targa bits, PSD
  ## previews.
  quality*: Option[float64]
  tiffLayers*: bool
  tgaBits*: Option[TgaBits]
  imagePreviews*: bool

proc defaultSaveOpts*(): SaveOpts = SaveOpts(imagePreviews: true)

proc f64Param(p: JsonNode; key: string): Option[float64] =
  let v = p.pget(key).asF64
  if v.isSome and v.get == v.get and abs(v.get) != Inf: v else: none(float64)

proc sizeParam(p: JsonNode; key: string): Option[float64] =
  ## A size parameter; zero or negative means "not set" (dialogs send 0 for empty fields).
  let v = f64Param(p, key)
  if v.isSome and v.get > 0: v else: none(float64)

proc tgaBitsParam*(p: JsonNode; cmdId: string): Option[TgaBits] =
  ## `"tgaBits": 24 | 32` (absent or null: the automatic choice).
  let v = p.pget("tgaBits")
  if v.isNull: return
  let n = v.asU64
  if n.isSome:
    let b = tgaBitsFrom(n.get)
    if b.isSome: return b
  raise bad(cmdId, "`tgaBits` must be 24 or 32, got " & jsonText(v))

proc saveOptsFrom*(s: Session; p: JsonNode; cmdId: string): SaveOpts =
  SaveOpts(quality: f64Param(p, "quality"), tiffLayers: p.pget("tiffLayers").asBool.get(false),
           tgaBits: tgaBitsParam(p, cmdId), imagePreviews: s.prefs.values.fileHandling.imagePreviews != akNever)

proc encodeDoc*(doc: Document; path: string; save: SaveOpts): (seq[byte], seq[string]) =
  ## Encodes `doc` for `path`'s extension.
  allocCheckpoint("saving document")
  var opts = defaultExportOptions()
  opts.tiffLayers = save.tiffLayers
  opts.tgaBits = save.tgaBits
  opts.imagePreviews = save.imagePreviews
  if save.quality.isSome:
    let q = uint8(round(clamp(save.quality.get, 0.0, 12.0) / 12.0 * 99.0 + 1.0))
    opts.encode.jpegQuality = int(q)
    # A quality on a WebP save asks for the lossy encoder; the default WebP stays lossless.
    opts.encode.webpQuality = int(q)
    opts.encode.webpLossless = false
  try:
    let r = exportDoc(doc, path, opts)
    (r.bytes, r.warnings)
  except EngineError: raise
  except CatchableError as e: raise other(path & ": " & e.msg)

proc saveDoc*(doc: Document; path: string; save: SaveOpts): seq[string] =
  let (bytes, warnings) = encodeDoc(doc, path, save)
  writeFileAtomic(path, bytes)
  warnings

proc strParamReq(p: JsonNode; key, cmdId: string): string =
  let v = p.pget(key).asStr
  if v.isNone or v.get.len == 0: raise bad(cmdId, "missing \"" & key & "\"")
  v.get

proc bufferSurface*(buf: Buffer; fmt: PixelFormat): Surface =
  ## A composite buffer as a surface in `fmt` (straight RGBA → the format's model and depth).
  let n = fmt.channels
  var data = newSeq[float32](buf.px.len * n)
  var tmp: array[8, float32]
  for i, p in buf.px:
    discard fromRgbaInto(fmt, p, tmp)
    for k in 0 ..< n: data[i * n + k] = tmp[k]
  result = newSurface(fmt)
  if not buf.rect.isEmpty: result.writeRegion(buf.rect, data)
  result.prune()

# ---------------------------------------------------------------- close / revert / save a copy / open as

proc closeAll(s: var Session): JsonNode =
  let n = s.documents.len
  while s.documents.len > 0: discard s.close(s.documents.len - 1)
  %*{"closed": n}

proc closeOthers(s: var Session; p: JsonNode): JsonNode =
  let v = p.pget("document").asU64
  var keep: int
  if v.isSome: keep = int(min(v.get, uint64(high(int))))
  elif s.activeIndex.isSome: keep = s.activeIndex.get
  else: raise noDocument()
  if keep >= s.documents.len: raise bad("file.closeOthers", "no document " & $keep)
  let n = s.documents.len - 1
  for i in countdown(s.documents.len - 1, 0):
    if i != keep: discard s.close(i)
  discard s.setActive(0)
  %*{"closed": n}

proc revert(s: var Session): JsonNode =
  ## File › Revert: reload the saved file as one history step; the document is clean afterwards.
  let d = s.active
  if d.isNil or d.path.isNone: raise other("the document has never been saved")
  let path = d.path.get
  let bytes = readFileOr(path)
  let fresh = fileImport(fileName(path), bytes)
  s.edit("Revert", proc(doc: var Document; active: var Option[LayerId]) =
    let (id, name) = (doc.id, doc.name)
    doc = fresh
    # Keep the identity (colour state, views and caches are keyed by it) and the tab name.
    doc.id = id
    doc.name = name
    active = doc.topLayer)
  let st = s.active
  if st.isNil: raise noDocument()
  st.savedRevision = st.revision
  %*{"path": path, "layers": st.doc.layers.len}

proc saveACopy(s: var Session; p: JsonNode): JsonNode =
  let path = strParamReq(p, "path", "file.saveACopy")
  let d = s.active
  if d.isNil: raise noDocument()
  var doc = d.doc[]
  if p.pget("layers").asBool == some(false):
    # "Layers" unchecked in Save a Copy: write the flattened image.
    let px = flattenToSurface(doc, doc.pixelFormat)
    doc.layers = @[newLayer("Background", LayerContent(kind: lkRaster, raster: px))]
  let warnings = saveDoc(doc, path, saveOptsFrom(s, p, "file.saveACopy"))
  %*{"path": path, "warnings": warnings}

proc openBytesAs*(s: var Session; name: string; bytes: openArray[byte]; asExt: Option[string]; path: Option[string]): JsonNode =
  ## Open a file's bytes, decoding them as the format `asExt` (Open As) when given.
  allocCheckpoint("importing document")
  var decodeName = name
  if asExt.isSome:
    var e = asExt.get
    while e.startsWith("."): e = e[1 .. ^1]
    decodeName = stem(name) & "." & e
  var r: ImportResult
  try: r = importDoc(decodeName, bytes)
  except EngineError: raise
  except CatchableError as e: raise other(decodeName & ": " & e.msg)
  var doc = r.document
  var docPath = path
  let untitled = templateName(s, name)
  if untitled.isSome:
    doc.name = untitled.get
    docPath = none(string)
  else: doc.name = fileName(name)
  # Color Settings policies (preserve / convert / discard the embedded profile).
  let (i, color) = s.openDocument(doc, if r.sourceReadOnly: none(string) else: docPath)
  let st = s.active
  if not st.isNil: st.sourceReadOnly = r.sourceReadOnly
  %*{"document": i, "color": color, "warnings": r.warnings}

proc openAs(s: var Session; p: JsonNode): JsonNode =
  let path = strParamReq(p, "path", "file.openAs")
  let bytes = readFileOr(path)
  var asV = p.pget("as")
  if asV.isNil: asV = p.pget("format")
  openBytesAs(s, path, bytes, asV.asStr, some(path))

# ---------------------------------------------------------------- place

const PlaceEmbedded* = "Place Embedded" ## history label of an embedded place

proc isSvgBytes(bytes: openArray[byte]): bool =
  ## `io::svg::is_svg`: an SVG root in the first 4 KiB.
  let n = min(bytes.len, 4096)
  var t = newString(n)
  for i in 0 ..< n: t[i] = char(bytes[i])
  while t.startsWith("\xEF\xBB\xBF"): t = t[3 .. ^1]
  t = unicode.strip(t, trailing = false)
  if t.startsWith("<svg"): return true
  (t.startsWith("<?xml") or t.startsWith("<!DOCTYPE") or t.startsWith("<!--")) and "<svg" in t

proc placeBytes*(s: var Session; name: string; bytes: seq[byte]; linked: Option[string]; p: JsonNode): JsonNode =
  ## Place a file's bytes as a smart object layer, centred, and (with Preferences ▸ General ▸
  ## Resize Image During Place, on by default) scaled down to fit when larger than the canvas.
  ## `linked` makes it a linked smart object that refers to that path instead of embedding the
  ## bytes. With Always Create Smart Objects When Placing off, an embedded place lands as a plain
  ## pixel layer instead.
  let d = s.active
  if d.isNil: raise noDocument()
  let (cw, ch) = (float64(d.doc.size.width), float64(d.doc.size.height))
  let fmt = d.doc[].pixelFormat
  discard isSvgBytes(bytes)
  var imported: ImportResult
  try: imported = importDoc(name, bytes)
  except EngineError: raise
  except CatchableError as e: raise other(name & ": " & e.msg)
  if imported.previewOnly:
    raise other(name & ": only this Affinity file's embedded preview could be read; open it with File › Open to see the warning, or export PSD or PNG from Affinity before using it here")
  let src = imported.document
  let (w, h) = (float64(src.size.width), float64(src.size.height))
  # The implicit `fit` is Preferences ▸ General ▸ Resize Image During Place (on by default).
  let fit = p.pget("fit").asBool.get(s.prefs.values.general.resizeImageDuringPlace)
  var scale = 1.0
  let k = f64Param(p, "scale")
  if k.isSome: scale = rmax(k.get / 100.0, 1e-4)
  elif fit and (w > cw or h > ch): scale = rmin(cw / w, ch / h)
  var center = (cw / 2.0, ch / 2.0)
  let c = p.pget("center")
  if c.isArray and c.len == 2: center = (c.elems[0].asF64.get(cw / 2.0), c.elems[1].asF64.get(ch / 2.0))
  let (dx, dy) = (round(center[0] - w * scale / 2.0), round(center[1] - h * scale / 2.0))
  var px = flattenToSurface(src, fmt)
  if abs(scale - 1.0) > 1e-9: px = resizeSurface(px, scale, scale, rsBicubic)
  let placed = translateSurface(px, asI32(dx), asI32(dy))
  # Always Create Smart Objects When Placing (on by default): off, an embedded place lands as a
  # plain pixel layer. A linked place is always a smart object.
  let smart = linked.isSome or p.pget("smartObject").asBool.get(s.prefs.values.general.alwaysCreateSmartObjectsWhenPlacing)
  let source = (if linked.isSome: SmartSource(kind: ssLinked, path: linked.get)
                else: SmartSource(kind: ssEmbedded, fileName: fileName(name), bytes: shared(bytes)))
  let layerName = stem(name)
  let label = if linked.isSome: "Place Linked" else: PlaceEmbedded
  let id = s.editWith(label, proc(doc: var Document; active: var Option[LayerId]): LayerId =
    let layer =
      if smart:
        newLayer(layerName, LayerContent(kind: lkSmart, smart: newSmartObject(source,
          Affine(m: [scale, 0.0, 0.0, scale, dx, dy]), some(placed))))
      else: newLayer(layerName, LayerContent(kind: lkRaster, raster: placed))
    result = doc.insertAbove(active, layer)
    active = some(result))
  %*{"layer": uint64(id), "scale": scale * 100.0, "bounds": [dx, dy, dx + w * scale, dy + h * scale], "smartObject": smart}

proc place(s: var Session; p: JsonNode; linked: bool): JsonNode =
  let cmdId = if linked: "file.placeLinked" else: "file.placeEmbedded"
  let path = strParamReq(p, "path", cmdId)
  let bytes = readFileOr(path)
  placeBytes(s, path, bytes, (if linked: some(path) else: none(string)), p)

# ---------------------------------------------------------------- File Info (XMP)

const InfoFields = [("title", "dc:title", "Alt"), ("author", "dc:creator", "Seq"), ("description", "dc:description", "Alt"),
                    ("keywords", "dc:subject", "Bag"), ("copyright", "dc:rights", "Alt"),
                    ("authorTitle", "photoshop:AuthorsPosition", "")]
  ## The File Info fields we edit, as (JSON key, XMP property, container kind).

proc xmlEscape(s: string): string =
  s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")

proc xmlUnescape(s: string): string =
  s.replace("&lt;", "<").replace("&gt;", ">").replace("&quot;", "\"").replace("&apos;", "'").replace("&amp;", "&")

proc trimR(s: string): string = unicode.strip(s)

proc findElement(xmp: string; tag: string): Option[(int, int, string)] =
  ## The element `<tag …>…</tag>` (first occurrence) as (start, end, inner).
  let open = "<" & tag
  var start = 0
  while true:
    let i = xmp.find(open, start)
    if i < 0: return
    let after = i + open.len
    if after < xmp.len and xmp[after] in {'>', ' ', '/', '\n', '\r', '\t'}:
      let gt = xmp.find('>', i)
      if gt < 0: return
      if gt > 0 and xmp[gt - 1] == '/': return some((i, gt + 1, ""))
      let close = "</" & tag & ">"
      let c = xmp.find(close, gt)
      if c < 0: return
      return some((i, c + close.len, xmp[gt + 1 ..< c]))
    start = i + open.len

proc liValues(inner: string): seq[string] =
  ## `rdf:li` values of a container (or the element text for simple properties).
  var rest = inner
  while true:
    let e = findElement(rest, "rdf:li")
    if e.isNone: break
    result.add xmlUnescape(trimR(e.get[2]))
    rest = rest[e.get[1] .. ^1]
  if result.len == 0 and '<' notin inner and trimR(inner).len > 0: result.add xmlUnescape(trimR(inner))

proc findAttr(xmp: string; prop: string): Option[(int, int, string)] =
  ## Attribute form `prop="value"` on an `rdf:Description`.
  let key = " " & prop & "=\""
  let i = xmp.find(key)
  if i < 0: return
  let v0 = i + key.len
  let v1 = xmp.find('"', v0)
  if v1 < 0: return
  some((i, v1 + 1, xmp[v0 ..< v1]))

proc readFileInfo*(xmp: Option[string]): JsonNode =
  ## File Info fields read from an XMP packet.
  result = newJObject()
  let x = xmp.get("")
  for (key, prop, kind) in InfoFields:
    var vals: seq[string]
    let e = findElement(x, prop)
    if e.isSome: vals = liValues(e.get[2])
    else:
      let a = findAttr(x, prop)
      if a.isSome: vals = @[xmlUnescape(a.get[2])]
    result[key] =
      if kind == "Bag": %vals
      elif kind == "Seq": %vals.join("; ")
      else: %(if vals.len > 0: vals[0] else: "")
  var marked = none(string)
  let me = findElement(x, "xmpRights:Marked")
  if me.isSome: marked = some(trimR(me.get[2]))
  else:
    let ma = findAttr(x, "xmpRights:Marked")
    if ma.isSome: marked = some(ma.get[2])
  result["copyrightStatus"] = %(if marked == some("True"): "copyrighted" elif marked == some("False"): "publicDomain" else: "unknown")
  var url = ""
  let ue = findElement(x, "xmpRights:WebStatement")
  if ue.isSome: url = xmlUnescape(trimR(ue.get[2]))
  else:
    let ua = findAttr(x, "xmpRights:WebStatement")
    if ua.isSome: url = xmlUnescape(ua.get[2])
  result["copyrightUrl"] = %url

const CameraFields* = [("make", "Make"), ("model", "Model"), ("dateTaken", "Date Taken"), ("exposure", "Exposure"),
                       ("fNumber", "F-Stop"), ("iso", "ISO Speed"), ("focalLength", "Focal Length"),
                       ("focalLength35mm", "Focal Length (35 mm)"), ("lens", "Lens")]
  ## File Info's read-only Camera Data, as (result key, English label) in display order.

proc shortNum(v: float64; decimals: int): string =
  ## `v` with at most `decimals` decimals and no trailing zeros (33.10 → "33.1", 8.0 → "8").
  var s = formatFloat(v, ffDecimal, decimals)
  if decimals == 0: s = fixed(v, 0)
  if '.' in s:
    while s.endsWith("0"): s.setLen(s.len - 1)
    if s.endsWith("."): s.setLen(s.len - 1)
  s

proc exposureText(t: float64): Option[string] =
  ## Exposure time as cameras print it: "1/250 s", or "0.4 s" / "2 s".
  if not (t == t and abs(t) != Inf and t > 0): return
  let r = 1.0 / t
  if t < 1.0 and abs(r - round(r)) <= r * 0.01: return some("1/" & displayF64(round(r)) & " s")
  some(shortNum(t, if t < 1.0: 2 else: 1) & " s")

proc dateText(s: string): string =
  ## EXIF (`2024:02:17 12:40:19`) or XMP (`2024-02-17T12:40:19.00+01:00`) dates as
  ## `2024-02-17 12:40:19`; anything else is shown as stored.
  let b = trimR(s)
  proc digits(idx: openArray[int]): bool =
    for i in idx:
      if i >= b.len or b[i] notin Digits: return false
    true
  proc sep(i: int; chars: set[char]): bool = i < b.len and b[i] in chars
  proc part(a, z: int): string =
    for i in a ..< z: result.add(if i < b.len: b[i] else: '0')
  if not (digits([0, 1, 2, 3, 5, 6, 8, 9]) and sep(4, {':', '-'}) and sep(7, {':', '-'})): return b
  let date = part(0, 4) & "-" & part(5, 7) & "-" & part(8, 10)
  if digits([11, 12, 14, 15, 17, 18]) and sep(10, {' ', 'T'}) and sep(13, {':'}) and sep(16, {':'}): date & " " & part(11, 19)
  else: date

proc xmpValue(x, prop: string): Option[string] =
  ## A single XMP value: the first `rdf:li` of a container, element text or attribute.
  var v: Option[string]
  let e = findElement(x, prop)
  if e.isSome:
    let l = liValues(e.get[2])
    if l.len > 0: v = some(l[0])
  else:
    let a = findAttr(x, prop)
    if a.isSome: v = some(xmlUnescape(a.get[2]))
  if v.isNone: return
  let t = trimR(v.get)
  if t.len > 0: some(t) else: none(string)

proc xmpNumber(x, prop: string): Option[float64] =
  ## An XMP number: a rational "331/10" or a decimal.
  let v = xmpValue(x, prop)
  if v.isNone: return
  var n: float64
  let i = v.get.find('/')
  if i >= 0:
    let a = rustParseF64(trimR(v.get[0 ..< i]))
    let b = rustParseF64(trimR(v.get[i + 1 .. ^1]))
    if a.isNone or b.isNone or b.get == 0: return
    n = a.get / b.get
  else:
    let a = rustParseF64(v.get)
    if a.isNone: return
    n = a.get
  if n == n and abs(n) != Inf: some(n) else: none(float64)

proc cameraData*(exif: SharedBytes; xmp: Option[string]): JsonNode =
  ## File Info's read-only Camera Data (keys of `CameraFields`, present ones only), formatted for
  ## display: the EXIF block first, the XMP `tiff:`/`exif:`/`aux:` copies for what it lacks.
  let e = (if not exif.isNil: aexif.read(exif.data) else: CameraInfo())
  let x = xmp.get("")
  proc text(v: Option[string]; props: openArray[string]): Option[string] =
    if v.isSome: return v
    for p in props:
      let r = xmpValue(x, p)
      if r.isSome: return r
  proc num(v: Option[float64]; props: openArray[string]): Option[float64] =
    var r = (if v.isSome and v.get == v.get and abs(v.get) != Inf and v.get > 0: v else: none(float64))
    if r.isNone:
      for p in props:
        r = xmpNumber(x, p)
        if r.isSome: break
    if r.isSome and r.get > 0: r else: none(float64)
  var values: array[9, Option[string]]
  values[0] = text(e.make, ["tiff:Make"])
  values[1] = text(e.model, ["tiff:Model"])
  let dt = text(e.dateTaken, ["exif:DateTimeOriginal"])
  if dt.isSome: values[2] = some(dateText(dt.get))
  let ex = num(e.exposureTime, ["exif:ExposureTime"])
  if ex.isSome: values[3] = exposureText(ex.get)
  let fnum = num(e.fNumber, ["exif:FNumber"])
  if fnum.isSome: values[4] = some("f/" & shortNum(fnum.get, 1))
  let iso = num(e.iso, ["exif:ISOSpeedRatings", "exifEX:PhotographicSensitivity"])
  if iso.isSome: values[5] = some("ISO " & displayF64(round(iso.get)))
  let fl = num(e.focalLength, ["exif:FocalLength"])
  if fl.isSome: values[6] = some(shortNum(fl.get, 1) & " mm")
  let fl35 = num(e.focalLength35mm, ["exif:FocalLengthIn35mmFilm"])
  if fl35.isSome: values[7] = some(shortNum(fl35.get, 1) & " mm")
  values[8] = text(e.lens, ["exifEX:LensModel", "aux:Lens"])
  result = newJObject()
  for i, (key, _) in CameraFields:
    if values[i].isSome:
      # Hostile files can carry megabytes of "lens name"; the dialog shows a line.
      let v = runeSubStr(values[i].get, 0, 200)
      if v.len > 0: result[key] = %v

proc writeFileInfo*(xmp: Option[string]; info: JsonNode): string =
  ## Rewrites only changed File Info fields of an XMP packet (creating one when there is none),
  ## keeping the other properties verbatim. Edited properties move into their own
  ## `rdf:Description`.
  let cur = readFileInfo(xmp)
  proc changed(key: string): bool =
    let v = info.pget(key)
    not v.isNil and (cur.pget(key).isNil or cur.pget(key) != v)
  var x = (if xmp.isSome and "</rdf:RDF>" in xmp.get: xmp.get
           else: "<?xpacket begin=\"\xEF\xBB\xBF\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>\n<x:xmpmeta xmlns:x=\"adobe:ns:meta/\">\n <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">\n </rdf:RDF>\n</x:xmpmeta>\n<?xpacket end=\"w\"?>")
  var all = @InfoFields
  all.add ("copyrightStatus", "xmpRights:Marked", "")
  all.add ("copyrightUrl", "xmpRights:WebStatement", "")
  for (key, prop, _) in all:
    if not changed(key): continue
    while true:
      let e = findElement(x, prop)
      if e.isNone: break
      x = x[0 ..< e.get[0]] & x[e.get[1] .. ^1]
    while true:
      let a = findAttr(x, prop)
      if a.isNone: break
      x = x[0 ..< a.get[0]] & x[a.get[1] .. ^1]
  proc get(k: string): JsonNode =
    if changed(k): info.pget(k) else: newJNull()
  var body = ""
  for (key, prop, kind) in InfoFields:
    let v = get(key)
    var vals: seq[string]
    if v.kind == JArray:
      for it in v.elems:
        if it.kind == JString and it.str.len > 0: vals.add it.str
    elif v.kind == JString and (kind == "Bag" or kind == "Seq"):
      for part in v.str.split({';', ','}):
        let t = trimR(part)
        if t.len > 0: vals.add t
    elif v.kind == JString and v.str.len > 0: vals = @[v.str]
    if vals.len == 0: continue
    var items = ""
    for it in vals:
      if kind == "Alt": items.add "<rdf:li xml:lang=\"x-default\">" & xmlEscape(it) & "</rdf:li>"
      else: items.add "<rdf:li>" & xmlEscape(it) & "</rdf:li>"
    if kind.len == 0: body.add "   <" & prop & ">" & xmlEscape(vals[0]) & "</" & prop & ">\n"
    else: body.add "   <" & prop & "><rdf:" & kind & ">" & items & "</rdf:" & kind & "></" & prop & ">\n"
  let st = get("copyrightStatus").asStr
  if st == some("copyrighted"): body.add "   <xmpRights:Marked>True</xmpRights:Marked>\n"
  elif st == some("publicDomain"): body.add "   <xmpRights:Marked>False</xmpRights:Marked>\n"
  let u = get("copyrightUrl").asStr
  if u.isSome and u.get.len > 0: body.add "   <xmpRights:WebStatement>" & xmlEscape(u.get) & "</xmpRights:WebStatement>\n"
  if body.len > 0:
    let desc = "  <rdf:Description rdf:about=\"\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\" xmlns:photoshop=\"http://ns.adobe.com/photoshop/1.0/\" xmlns:xmpRights=\"http://ns.adobe.com/xap/1.0/rights/\">\n" &
      body & "  </rdf:Description>\n "
    let at = x.rfind("</rdf:RDF>")
    let pos = (if at >= 0: at else: x.len)
    x.insert(desc, pos)
  x

proc fileInfo(s: var Session; p: JsonNode): JsonNode =
  const Keys = ["title", "author", "authorTitle", "description", "keywords", "copyright", "copyrightStatus", "copyrightUrl"]
  var edits = false
  if p.isObject:
    for k in p.fields.keys:
      if k in Keys: edits = true
  if edits:
    let d = s.active
    if d.isNil: raise noDocument()
    let nw = writeFileInfo(d.doc.metadata.xmp, p)
    s.edit("File Info", proc(doc: var Document; active: var Option[LayerId]) = doc.metadata.xmp = some(nw))
  let d = s.active
  if d.isNil: raise noDocument()
  result = readFileInfo(d.doc.metadata.xmp)
  result["camera"] = cameraData(d.doc.metadata.exif, d.doc.metadata.xmp)

# ---------------------------------------------------------------- Automate

proc fitImage(s: var Session; p: JsonNode): JsonNode =
  let d = s.active
  if d.isNil: raise noDocument()
  let (w, h) = (float64(d.doc.size.width), float64(d.doc.size.height))
  let bw = rmax(f64Param(p, "width").get(w), 1.0)
  let bh = rmax(f64Param(p, "height").get(h), 1.0)
  let k = rmin(bw / w, bh / h)
  if (k > 1.0 and p.pget("dontEnlarge").asBool.get(false)) or (round(w * k) == w and round(h * k) == h):
    return %*{"width": w, "height": h, "changed": false}
  let (nw, nh) = (rmax(round(w * k), 1.0), rmax(round(h * k), 1.0))
  let resample = p.pget("resample").asStr.get("bicubic")
  discard s.execute("image.imageSize", %*{"width": nw, "height": nh, "resample": resample})
  %*{"width": nw, "height": nh, "changed": true}

proc modeName(m: ColorMode): string =
  case m
  of cmBitmap: "bitmap"
  of cmGrayscale: "grayscale"
  of cmDuotone: "duotone"
  of cmIndexed: "indexed"
  of cmRgb: "rgb"
  of cmCmyk: "cmyk"
  of cmLab: "lab"
  of cmMultichannel: "multichannel"

proc conditionalModeChange(s: var Session; p: JsonNode): JsonNode =
  const C = "file.automate.conditionalModeChange"
  let to0 = strParamReq(p, "to", C).toLowerAscii
  let to = case to0
    of "gray", "grayscale": "grayscale"
    of "rgb": "rgb"
    of "cmyk": "cmyk"
    of "lab": "lab"
    else: raise bad(C, "unsupported target mode \"" & to0 & "\" (rgb, grayscale, cmyk, lab)")
  let d = s.active
  if d.isNil: raise noDocument()
  let cur = modeName(d.doc.mode)
  var frm: seq[string]
  let fv = p.pget("from")
  if fv.isArray:
    for v in fv.elems:
      if v.kind == JString: frm.add v.str.toLowerAscii.replace("gray", "grayscale").replace("grayscalescale", "grayscale")
  elif not fv.isNil and fv.kind == JString and fv.str != "any": frm = @[fv.str.toLowerAscii]
  else: frm = @["any"]
  if cur == to or not frm.anyIt(it == "any" or it == cur): return %*{"changed": false, "mode": cur}
  discard s.execute("image.mode." & to, newJObject())
  %*{"changed": true, "mode": to}

proc batchInputs*(p: JsonNode; cmdId: string): seq[string] =
  ## Inputs of a batch command: `"input"` is a folder or an array of files.
  var v = p.pget("input")
  if v.isNil: v = p.pget("files")
  if v.isArray:
    for x in v.elems:
      if x.kind == JString: result.add x.str
  elif not v.isNil and v.kind == JString: result = listImages(v.str)
  else: raise bad(cmdId, "missing \"input\" (folder or array of files)")

proc processFiles*(inputs: seq[string]; output, format: string; save: SaveOpts; suffix: string;
                   f: proc(s: var Session)): JsonNode =
  ## Opens each input in a scratch session, runs `f` on it and saves it to `output` as `format`
  ## (`"same"` keeps the input's extension). Errors per file are collected, not fatal.
  var files = newJArray()
  var errors = newJArray()
  var written = initTable[string, string]()
  for path in inputs:
    var ext: string
    if format == "same":
      let i = path.rfind('.')
      ext = (if i >= 0: path[i + 1 .. ^1] else: path).toLowerAscii
    else:
      ext = format
      while ext.startsWith("."): ext = ext[1 .. ^1]
      ext = ext.toLowerAscii
    let outp = joinPath(output, stem(path) & suffix & "." & ext)
    try:
      let key = unicode.toLower(outp)
      if key in written:
        raise other("not written: " & outp & " already holds the result of " & written[key] & " from this run (same output name)")
      let bytes = readFileOr(path)
      var scratch = newSession()
      let doc = fileImport(fileName(path), bytes)
      discard scratch.addDocument(doc, some(path))
      f(scratch)
      let d = scratch.active
      if d.isNil: raise noDocument()
      discard saveDoc(d.doc[], outp, save)
      written[key] = path
      files.add %outp
    except CatchableError as e:
      errors.add %*{"file": path, "error": e.msg}
  %*{"files": files, "errors": errors}

proc parseSteps(v: JsonNode): seq[(string, JsonNode)] =
  ## One `[id, params]` / `{command, params}` / `{id, params}` step of an action, each.
  const C = "file.automate.batch"
  if not v.isArray: raise bad(C, "\"steps\" must be an array")
  for st in v.elems:
    if st.kind == JArray and st.len > 0:
      if st.elems[0].kind != JString: raise bad(C, "step id must be a string")
      result.add (st.elems[0].str, (if st.len > 1: st.elems[1] else: newJObject()))
    elif st.kind == JObject:
      var idv = st.pget("command")
      if idv.isNil: idv = st.pget("id")
      if idv.isNil or idv.kind != JString: raise bad(C, "step needs \"command\"")
      let pp = st.pget("params")
      result.add (idv.str, (if pp.isNil: newJObject() else: pp))
    elif st.kind == JString: result.add (st.str, newJObject())
    else: raise bad(C, "each step is [id, params] or {\"command\", \"params\"}")

proc batch(s: var Session; p: JsonNode): JsonNode =
  const C = "file.automate.batch"
  var sv = p.pget("steps")
  if sv.isNil: sv = p.pget("action")
  if sv.isNil: raise bad(C, "missing \"steps\"")
  var steps = parseSteps(sv)
  # Each result goes to the destination folder, which overrides the action's recorded Save / Save
  # As steps; recorded view steps don't change the output, so they are skipped.
  steps.keepItIf(not shellSaveCommand(it[0]) and not shellViewCommand(it[0]))
  for (id, _) in steps:
    if findSpec(id).isNil: raise bad(C, "unknown command `" & id & "` in the action")
  let inputs = batchInputs(p, C)
  let output = strParamReq(p, "output", C)
  let format = p.pget("format").asStr.get("same")
  # Steps that call another action play it from the caller's actions; the playback stack comes
  # along so an action that batches itself is caught as a recursive call.
  var list: seq[Action]
  let ca = p.pget("calledActions")
  if ca.isNull: list = s.actions.list
  else:
    # A droplet brings the actions it calls and runs with those alone.
    try: list = actionsFromJson(ca)
    except ValueError as e: raise bad(C, "\"calledActions\": " & e.msg)
    if list.len > 4096: raise bad(C, "\"calledActions\" holds more than 4096 actions")
  let actions = ActionState(list: list, playing: s.actions.playing, playbackStack: s.actions.playbackStack)
  let st = steps
  processFiles(inputs, output, format, saveOptsFrom(s, p, C), "", proc(scratch: var Session) =
    scratch.actions = actions
    for (id, params) in st:
      let r = scratch.execute(id, params.copy)
      # A failed step inside a called action makes this file an error, not a saved result.
      let nf = nestedFailure(id, r)
      if nf.isSome: raise other(nf.get))

proc imageProcessor(s: var Session; p: JsonNode): JsonNode =
  const C = "file.scripts.imageProcessor"
  let inputs = batchInputs(p, C)
  let output = strParamReq(p, "output", C)
  let format = p.pget("format").asStr.get("jpg")
  let (w, h) = (sizeParam(p, "width"), sizeParam(p, "height"))
  let fit = (if w.isNone and h.isNone: none(JsonNode)
             else: some(%*{"width": w.get(1e9), "height": h.get(1e9), "dontEnlarge": true}))
  let toSrgb = p.pget("convertToSrgb").asBool.get(false)
  var save = saveOptsFrom(s, p, C)
  if save.quality.isNone: save.quality = some(8.0)
  processFiles(inputs, output, format, save, "", proc(scratch: var Session) =
    if toSrgb and not scratch.active.isNil and scratch.active.doc.mode != cmRgb:
      discard scratch.execute("image.mode.rgb", newJObject())
    if fit.isSome: discard fitImage(scratch, fit.get))

proc loadFilesIntoStack(s: var Session; p: JsonNode): JsonNode =
  const C = "file.scripts.loadFilesIntoStack"
  var v = p.pget("paths")
  if v.isNil: v = p.pget("input")
  var paths: seq[string]
  if v.isArray:
    for x in v.elems:
      if x.kind == JString: paths.add x.str
  elif not v.isNil and v.kind == JString: paths = listImages(v.str)
  else: raise bad(C, "missing \"paths\"")
  if paths.len == 0: raise bad(C, "no files to load")
  var docs: seq[(string, Document)]
  for path in paths:
    let bytes = readFileOr(path)
    docs.add (fileName(path), fileImport(fileName(path), bytes))
  let first = docs[0][1]
  var w, h = 1
  var hasW = false
  for (_, d) in docs:
    if not hasW: (w, h) = (d.size.width, d.size.height)
    else: (w, h) = (max(w, d.size.width), max(h, d.size.height))
    hasW = true
  var stack = newDocument(stem(paths[0]), Size(width: w, height: h), first.mode, first.depth)
  stack.resolutionDpi = first.resolutionDpi
  stack.iccProfile = first.iccProfile
  let fmt = stack.pixelFormat
  # First file at the bottom, like Photoshop's script.
  for (name, d) in docs: stack.layers.add newLayer(name, LayerContent(kind: lkRaster, raster: flattenToSurface(d, fmt)))
  let n = stack.layers.len
  if p.pget("createSmartObject").asBool.get(false):
    # The layers go inside one smart object, ready for Layer › Smart Objects › Stack Mode; they
    # sit at the top level of the contents, with no wrapper group.
    let layers = stack.layers
    stack.layers = @[]
    let smart = layersToSmart(stack, stem(paths[0]), layers)
    stack.layers = @[smart]
  let i = s.addDocument(stack)
  %*{"document": i, "layers": n}

# ---------------------------------------------------------------- Scripts: flatten effects / masks

proc hasLiveEffects(l: Layer): bool =
  if not l.effects.enabled: return false
  for e in l.effects.items:
    if e.enabled: return true

proc bake(l: Layer; canvas: Rect; fmt: PixelFormat; keepEffects: bool): Surface =
  ## The layer rendered alone (keeping its effects or not), as a surface in `fmt`.
  var tmp = l
  tmp.opacity = 1
  tmp.blend = bmNormal
  tmp.visible = true
  tmp.clipped = false
  if not keepEffects:
    tmp.effects.items.setLen(0)
    tmp.fillOpacity = 1
  # Render over the union of the canvas and the content so off-canvas pixels survive.
  var area = canvas
  if l.content.kind in {lkRaster, lkText, lkShape, lkSmart}:
    let has = (case l.content.kind
               of lkText: l.content.text.cache.isSome
               of lkShape: l.content.shape.cache.isSome
               of lkSmart: l.content.smart.cache.isSome
               else: true)
    if has: area = l.surface.contentBounds.union(canvas)
  if keepEffects: area = area.inflate(256)
  bufferSurface(renderLayer(tmp, area), fmt)

proc walkIds(doc: Document): seq[LayerId] =
  for (_, _, l) in doc.walk: result.add l.id

proc flattenAllEffects(s: var Session): JsonNode =
  var n = 0
  s.edit("Flatten All Layer Effects", proc(doc: var Document; active: var Option[LayerId]) =
    let canvas = doc.bounds
    let fmt = doc.pixelFormat
    for id in walkIds(doc):
      let l = doc.findLayer(id)
      if l.isNil: continue
      if not hasLiveEffects(l[]) or l[].content.kind in {lkGroup, lkAdjustment}: continue
      let px = bake(l[], canvas, fmt, true)
      l[].content = LayerContent(kind: lkRaster, raster: px)
      l[].effects = Effects()
      l[].mask = none(LayerMask)
      l[].vectorMask = none(VectorMask)
      l[].fillOpacity = 1
      l[].fillCache = none(FillCache)
      l[].psdBlocks.keepItIf(it.key notin ["TySh", "SoLd", "PlLd", "SoLE", "vmsk", "vsms", "vogk", "vscg", "vstk"])
      inc n)
  %*{"flattened": n}

proc flattenAllMasks(s: var Session): JsonNode =
  var n, skipped = 0
  s.edit("Flatten All Masks", proc(doc: var Document; active: var Option[LayerId]) =
    let canvas = doc.bounds
    let fmt = doc.pixelFormat
    for id in walkIds(doc):
      let l = doc.findLayer(id)
      if l.isNil: continue
      let masked = (l[].mask.isSome and l[].mask.get.enabled) or l[].vectorMask.isSome
      if not masked: continue
      if l[].content.kind != lkRaster:
        # Masks on groups, adjustment, type, shape and smart layers have no pixels to fold into;
        # Photoshop's script leaves them too.
        inc skipped
        continue
      let px = bake(l[], canvas, fmt, false)
      l[].content = LayerContent(kind: lkRaster, raster: px)
      l[].mask = none(LayerMask)
      l[].vectorMask = none(VectorMask)
      l[].psdBlocks.keepItIf(it.key notin ["vmsk", "vsms"])
      inc n)
  %*{"flattened": n, "skipped": skipped}

# ---------------------------------------------------------------- Export

proc exportLayersWith*(s: Session; p: JsonNode; write: proc(name: string; bytes: seq[byte])): seq[string] =
  ## The bodies of `file.export.layersToFiles`, streamed to `write` as a single-component filename
  ## plus its bytes: one file per exported layer (top to bottom, like Photoshop's numbering).
  const C = "file.export.layersToFiles"
  var format = p.pget("format").asStr.get("png")
  while format.startsWith("."): format = format[1 .. ^1]
  format = format.toLowerAscii
  let visibleOnly = p.pget("visibleOnly").asBool.get(true)
  let save = saveOptsFrom(s, p, C)
  let d = s.active
  if d.isNil: raise noDocument()
  let prefix = p.pget("prefix").asStr.get(stem(d.doc.name))
  let doc = d.doc[]
  # Top-level layers (groups export merged), top to bottom like Photoshop's numbering.
  var layers: seq[Layer]
  for k in countdown(doc.layers.high, 0):
    if doc.layers[k].visible or not visibleOnly: layers.add doc.layers[k]
  for i, l in layers:
    if l.content.kind == lkAdjustment: continue
    var one = doc
    var only = l
    only.visible = true
    only.clipped = false
    one.layers = @[only]
    let name = sanitize(prefix) & "_" & align($i, 4, '0') & "_" & sanitize(l.name) & "." & format
    let (bytes, _) = encodeDoc(one, name, save)
    write(name, bytes)
    result.add name

proc layersToFiles(s: var Session; p: JsonNode): JsonNode =
  const C = "file.export.layersToFiles"
  var dir: string
  try: dir = strParamReq(p, "dir", C)
  except EngineError: dir = strParamReq(p, "output", C)
  let d = dir
  let names = exportLayersWith(s, p, proc(name: string; bytes: seq[byte]) = writeFileAtomic(joinPath(d, name), bytes))
  var files = newJArray()
  for n in names: files.add %joinPath(dir, n)
  %*{"files": files}

proc lutLayers(doc: Document; ids: Option[seq[LayerId]]): seq[Layer] =
  ## Adjustment layers that can be represented as an RGB-only LUT, in document stack order.
  const C = "file.export.colorLookupTables"
  var selected: seq[LayerId]
  if ids.isSome:
    if ids.get.len == 0: raise bad(C, "select at least one adjustment layer")
    for id in ids.get:
      if id in selected: raise bad(C, "duplicate layer id " & $uint64(id))
      let layer = doc.findLayer(id)
      if layer.isNil: raise noLayer(id)
      if layer[].content.kind != lkAdjustment: raise bad(C, "layer " & $uint64(id) & " is not an adjustment layer")
      if not layer[].visible: raise bad(C, "layer " & $uint64(id) & " is hidden")
      var top = false
      for root in doc.layers:
        if root.id == id: top = true
      if not top: raise bad(C, "layer " & $uint64(id) & " is nested in a group; export top-level adjustment layers")
      selected.add id
  for l in doc.layers:
    if l.visible and l.content.kind == lkAdjustment and (ids.isNone or l.id in selected): result.add l
  if result.len == 0: raise bad(C, "no visible adjustment layers to export")

proc fixed6(v: float32): string = fixed(float64(v), 6)

proc bakeCubeLayers(size: int; title: string; layers: seq[Layer]): string =
  ## Bake the chosen adjustment stack against an identity RGB lattice.
  let n = size
  let (w, h) = (n * n, n)
  var lattice = newDocument("lut", Size(width: w, height: h), cmRgb, stF32)
  let fmt = lattice.pixelFormat
  let step = 1'f32 / float32(n - 1)
  var data = newSeqOfCap[float32](w * h * fmt.channels)
  var px: array[8, float32]
  for g in 0 ..< n:
    for b in 0 ..< n:
      for r in 0 ..< n:
        let k = fromRgbaInto(fmt, [float32(r) * step, float32(g) * step, float32(b) * step, 1'f32], px)
        for i in 0 ..< k: data.add px[i]
  var surf = newSurface(fmt)
  surf.writeRegion(rect(0, 0, w, h), data)
  lattice.layers.add newLayer("Lattice", LayerContent(kind: lkRaster, raster: surf))
  for l in layers:
    var a = l
    # Masks and clipping are spatial; a LUT is the adjustment stack's colour mapping.
    a.mask = none(LayerMask)
    a.vectorMask = none(VectorMask)
    a.clipped = false
    lattice.layers.add a
  let outp = flatten(lattice)
  result = "TITLE \"" & title.replace("\"", "'") & "\"\n# Created by Photocraft\nLUT_3D_SIZE " & $n &
           "\nDOMAIN_MIN 0.0 0.0 0.0\nDOMAIN_MAX 1.0 1.0 1.0\n"
  # .cube order: red changes fastest, then green, then blue.
  for b in 0 ..< n:
    for g in 0 ..< n:
      for r in 0 ..< n:
        let (x, y) = (b * n + r, g)
        let p = outp.px[y * w + x]
        result.add fixed6(clamp(p[0], 0'f32, 1'f32)) & " " & fixed6(clamp(p[1], 0'f32, 1'f32)) & " " &
                   fixed6(clamp(p[2], 0'f32, 1'f32)) & "\n"

proc bakeCube*(doc: Document; size: int; title: string): string =
  ## Exports all visible top-level adjustments.
  var layers: seq[Layer]
  for l in doc.layers:
    if l.visible and l.content.kind == lkAdjustment: layers.add l
  bakeCubeLayers(clamp(size, 2, MaxLutSize), title, layers)

proc colorLookupTables(s: var Session; p: JsonNode): JsonNode =
  const C = "file.export.colorLookupTables"
  let d = s.active
  if d.isNil: raise noDocument()
  var size = 33'u64
  let sv = p.pget("size")
  if not sv.isNull:
    let v = sv.asU64
    if v.isNone: raise bad(C, "size must be an integer")
    size = v.get
  if size < 2 or size > uint64(MaxLutSize): raise bad(C, "size must be 2..=" & $MaxLutSize)
  var scope = "all"
  let scv = p.pget("scope")
  if not scv.isNull:
    if scv.kind != JString: raise bad(C, "scope must be all or selected")
    scope = scv.str
  if scope notin ["all", "selected"]: raise bad(C, "scope must be all or selected")
  let lv = p.pget("layers")
  if not lv.isNil and scope == "selected": raise bad(C, "use either layers or scope=selected, not both")
  var explicit = none(seq[LayerId])
  if not lv.isNil:
    if not lv.isArray: raise bad(C, "layers must be an array of layer IDs")
    var ids: seq[LayerId]
    for id in lv.elems:
      let v = id.asU64
      if v.isNone: raise bad(C, "layer IDs must be unsigned integers")
      ids.add LayerId(v.get)
    explicit = some(ids)
  elif scope == "selected": explicit = some(d[].selectedOrdered)
  let layers = lutLayers(d.doc[], explicit)
  let count = layers.len
  let title = p.pget("title").asStr.get(stem(d.doc.name))
  let cube = bakeCubeLayers(int(size), title, layers)
  let path = p.pget("path").asStr
  if path.isSome:
    writeFileAtomic(path.get, cube.toOpenArrayByte(0, cube.len - 1))
    %*{"path": path.get, "size": size, "layerCount": count}
  else: %*{"cube": cube, "size": size, "layerCount": count}

# ---------------------------------------------------------------- guides

const
  GuideLayoutCmd = "view.newGuideLayout"
  MaxGuideCount = 1000'u64
  MaxGuideEdges = 16_000'u64

proc glBad(msg: string): ref EngineError = bad(GuideLayoutCmd, msg)

proc glNumber(p: JsonNode; key: string; default: float64): float64 =
  let v = p.pget(key)
  if v.isNull: return default
  let n = v.asF64
  if n.isNone or not (n.get == n.get and abs(n.get) != Inf and n.get >= 0):
    raise glBad(key & " must be a finite non-negative number")
  n.get

proc glCount(p: JsonNode; key: string): uint64 =
  let v = p.pget(key)
  if v.isNil: return 0
  let n = v.asU64
  if n.isNone: raise glBad(key & " must be a whole number")
  if n.get > MaxGuideCount: raise glBad(key & " must be " & $MaxGuideCount & " or fewer (got " & $n.get & ")")
  n.get

proc glFlag(p: JsonNode; key: string): bool =
  let v = p.pget(key)
  if v.isNil: return false
  let b = v.asBool
  if b.isNone: raise glBad(key & " must be a boolean")
  b.get

proc selectedArtboards*(s: Session): seq[Rect] =
  let st = s.active
  if st.isNil: return
  var ids = initHashSet[uint64]()
  for id in st[].selectedOrdered:
    let a = st.doc[].artboardOf(id)
    if a.isSome: ids.incl uint64(a.get)
  for (id, _, board) in st.doc[].artboards:
    if uint64(id) in ids: result.add board.rect

proc glTargets(s: Session; p: JsonNode): seq[Rect] =
  let st = s.active
  if st.isNil: raise noDocument()
  var target = "document"
  let v = p.pget("target")
  if not v.isNil:
    if v.kind != JString: raise glBad("target must be a string")
    target = v.str
  case target
  of "document": @[st.doc[].bounds]
  of "selectedArtboards":
    let boards = selectedArtboards(s)
    if boards.len == 0: raise glBad("select an artboard or a layer inside one")
    boards
  else: raise glBad("target must be document or selectedArtboards")

proc glPushUnique(outp: var seq[float64]; value: float64) =
  if not (value == value and abs(value) != Inf): raise glBad("guide coordinates exceed the supported range")
  for x in outp:
    if abs(x - value) < 0.01: return
  outp.add value

proc glAxis(start, finish: float64; count: uint64; fixed, gap: float64; center: bool; outp: var seq[float64]) =
  if count == 0: return
  let available = finish - start
  let n = float64(count)
  let width = if fixed > 0: fixed else: (available - (n - 1) * gap) / n
  let total = n * width + (n - 1) * gap
  if width <= 0 or not (total == total and abs(total) != Inf) or total > available + 0.001:
    raise glBad("count, size and gutter must fit inside the target margins")
  let origin = if center and fixed > 0: start + (available - total) / 2 else: start
  for i in 0'u64 ..< count:
    let a = origin + float64(i) * (width + gap)
    glPushUnique(outp, a)
    glPushUnique(outp, a + width)

proc newGuideLayout(s: var Session; p: JsonNode): JsonNode =
  let st = s.active
  if st.isNil: raise noDocument()
  let cols = glCount(p, "columns")
  let rows = glCount(p, "rows")
  let width = glNumber(p, "width", 0)
  let height = glNumber(p, "height", 0)
  let gap = glNumber(p, "gutter", 0)
  let rowGap = glNumber(p, "rowGutter", gap)
  let center = glFlag(p, "centerColumns")
  let clear = glFlag(p, "clearExisting")
  var margins: array[4, float64]
  let mv = p.pget("margin")
  if mv.isArray:
    if mv.len != 4: raise glBad("margin must contain top, left, bottom and right")
    for i in 0..3:
      let v = mv.elems[i].asF64
      if v.isNone or not (v.get == v.get and abs(v.get) != Inf and v.get >= 0):
        raise glBad("margins must be finite non-negative numbers")
      margins[i] = v.get
  else:
    let m = glNumber(p, "margin", 0)
    margins = [m, m, m, m]
  let (top, left, bottom, right) = (margins[0], margins[1], margins[2], margins[3])
  let bounds = glTargets(s, p)
  if (cols + rows + 4) * 2 * uint64(bounds.len) > MaxGuideEdges:
    raise glBad("the selected targets would create too many guide lines")
  var added = Guides()
  for r in bounds:
    let (x0, y0, x1, y1) = (float64(r.x0) + left, float64(r.y0) + top, float64(r.x1) - right, float64(r.y1) - bottom)
    if x0 >= x1 or y0 >= y1: raise glBad("margins must leave space inside the target")
    if margins.anyIt(it != 0):
      glPushUnique(added.vertical, x0)
      glPushUnique(added.vertical, x1)
      glPushUnique(added.horizontal, y0)
      glPushUnique(added.horizontal, y1)
    glAxis(x0, x1, cols, width, gap, center, added.vertical)
    glAxis(y0, y1, rows, height, rowGap, center, added.horizontal)
  if added.vertical.len == 0 and added.horizontal.len == 0: raise glBad("give \"columns\", \"rows\" or a \"margin\"")
  var guides = st.doc.guides
  if clear:
    if p.pget("target").asStr == some("selectedArtboards"):
      # Guides are stored as document-wide axes, matching Clear Selected Artboard Guides.
      var keptV, keptH: seq[float64]
      for v in guides.vertical:
        var inside = false
        for r in bounds:
          if v >= float64(r.x0) and v <= float64(r.x1): inside = true
        if not inside: keptV.add v
      for v in guides.horizontal:
        var inside = false
        for r in bounds:
          if v >= float64(r.y0) and v <= float64(r.y1): inside = true
        if not inside: keptH.add v
      guides.vertical = keptV
      guides.horizontal = keptH
    else: guides = Guides()
    # Cleared guides take their colours along, so a new guide at the same place doesn't inherit one.
    guides.pruneColors()
  for x in added.vertical: glPushUnique(guides.vertical, x)
  for y in added.horizontal: glPushUnique(guides.horizontal, y)
  let res = %*{"vertical": added.vertical.len, "horizontal": added.horizontal.len}
  let g = guides
  s.edit("New Guide Layout", proc(doc: var Document; active: var Option[LayerId]) = doc.guides = g)
  res

proc pushGuide(v: var seq[float64]; x: float64) =
  for g in v:
    if abs(g - x) < 0.01: return
  v.add x

proc guidesFromShape(s: var Session; p: JsonNode): JsonNode =
  let id = layerParam(s, p)
  let d = s.active
  if d.isNil: raise noDocument()
  let l = d.doc[].findLayer(id)
  if l.isNil: raise noLayer(id)
  if l[].content.kind != lkShape: raise other("the layer is not a shape layer")
  let cb = l[].content.shape.path.controlBounds
  if cb.isNone: raise other("the shape has no path")
  let (x0, y0, x1, y1) = cb.get
  s.edit("New Guides From Shape", proc(doc: var Document; active: var Option[LayerId]) =
    for x in [x0, (x0 + x1) / 2, x1]: pushGuide(doc.guides.vertical, x)
    for y in [y0, (y0 + y1) / 2, y1]: pushGuide(doc.guides.horizontal, y))
  %*{"bounds": [x0, y0, x1, y1]}

proc clearCanvasGuides(s: var Session): JsonNode =
  let d = s.active
  if d.isNil: raise noDocument()
  # Guides inside an artboard belong to it; the rest are canvas guides.
  var boards: seq[Rect]
  for (_, _, b) in d.doc[].artboards: boards.add b.rect
  proc inX(x: float64): bool = boards.anyIt(x > float64(it.x0) and x < float64(it.x1))
  proc inY(y: float64): bool = boards.anyIt(y > float64(it.y0) and y < float64(it.y1))
  var n = 0
  for x in d.doc.guides.vertical:
    if not inX(x): inc n
  for y in d.doc.guides.horizontal:
    if not inY(y): inc n
  if n == 0: return %*{"cleared": 0}
  s.edit("Clear Canvas Guides", proc(doc: var Document; active: var Option[LayerId]) =
    doc.guides.vertical.keepItIf(inX(it))
    doc.guides.horizontal.keepItIf(inY(it))
    doc.guides.pruneColors())
  %*{"cleared": n}

# ---------------------------------------------------------------- Reveal in Finder

proc rustParent(path: string): Option[string] =
  ## `Path::parent` as text: none for the root or an empty path.
  var p = path
  while p.len > 1 and p.endsWith("/"): p.setLen(p.len - 1)
  if p.len == 0 or p == "/": return
  let i = p.rfind('/')
  if i < 0: some("")
  elif i == 0: some("/")
  else:
    var q = p[0 ..< i]
    while q.len > 1 and q.endsWith("/"): q.setLen(q.len - 1)
    some(q)

proc revealCommand*(path: string): (string, seq[string]) =
  ## The file-manager invocation that selects `path` on this platform.
  when defined(macosx): ("open", @["-R", path])
  elif defined(windows): ("explorer", @["/select," & path])
  else: ("xdg-open", @[rustParent(path).get(".")])

proc reveal*(path: string; dryRun: bool): JsonNode =
  let (program, args) = revealCommand(path)
  if dryRun: return %*{"program": program, "args": args}
  when defined(js) or defined(wasm32): raise other("Reveal in Finder needs the desktop app")
  else:
    try: discard startProcess(program, args = args, options = {poUsePath, poParentStreams})
    except OSError: raise other(program & ": " & osErrorText(osLastError()))
  %*{"path": path}

proc revealInFinder(s: var Session; p: JsonNode): JsonNode =
  const C = "file.revealInFinder"
  var i: int
  let v = p.pget("document")
  if not v.isNil:
    let n = v.asU64
    if n.isNone: raise bad(C, "`document` must be an index")
    i = int(min(n.get, uint64(high(int))))
  elif s.activeIndex.isSome: i = s.activeIndex.get
  else: raise noDocument()
  let d = s.docAt(i)
  if d.isNil or d.path.isNone: raise bad(C, "document " & $i & " has no saved file")
  reveal(d.path.get, p.pget("dryRun").asBool.get(false))

# ---------------------------------------------------------------- registry

proc fileSpecs*(): seq[CommandSpec] =
  @[cmd("file.closeAll", "Close All", ["File"], "Cmd+Alt+W", "{}", enabledProc(hasDocE(s)), runProc(closeAll(s))),
    cmd("file.closeOthers", "Close Others", ["File"], "Cmd+Alt+P",
        """{"document":index? (the one to keep; default active)}""", enabledProc(hasDocE(s)), runProc(closeOthers(s, p))),
    # The document tab's menu: the tab's own file in the platform file manager.
    cmd("file.revealInFinder", "Reveal in Finder", [], "", """{"document":index? (default active),"dryRun":bool=false}""",
        enabledProc(nativeDocE(s)), runProc(revealInFinder(s, p))),
    cmd("file.revert", "Revert", ["File"], "F12", "{} (reloads the saved file as one undoable step)",
        enabledProc(canRevertE(s)), runProc(revert(s))),
    cmd("file.saveACopy", "Save a Copy…", ["File"], "Cmd+Alt+S",
        """{"path":str (format from the extension),"quality":0..12? (JPEG),"layers":bool=true,"tiffLayers":bool=false (TIFF: keep the layers; flat by default),"tgaBits":24|32? (TGA: 32 writes the alpha channel, or else the transparency, as the alpha; unset, 32 with an alpha channel or transparency, else 24)}""",
        enabledProc(nativeDocE(s)), runProc(saveACopy(s, p))),
    cmd("file.openAs", "Open As…", ["File"], "Cmd+Alt+Shift+O",
        """{"path":str,"as":"psd|png|jpg|tiff|…"? (decode as this format)}""", enabledProc(nativeE(s)), runProc(openAs(s, p))),
    cmd("file.placeEmbedded", "Place Embedded…", ["File"], "",
        """{"path":str,"scale":%? (default: fit when larger than the canvas),"fit":bool? (default: Preferences > General > Resize Image During Place),"smartObject":bool? (default: Preferences > General > Always Create Smart Objects When Placing),"center":[x,y]?}""",
        enabledProc(nativeDocE(s)), runProc(place(s, p, false))),
    cmd("file.placeLinked", "Place Linked…", ["File"], "",
        """{"path":str,"scale":%?,"fit":bool? (default: Preferences > General > Resize Image During Place),"center":[x,y]?}""",
        enabledProc(nativeDocE(s)), runProc(place(s, p, true))),
    cmd("file.fileInfo", "File Info…", ["File"], "Cmd+Alt+Shift+I",
        """{"title":str?,"author":str?,"authorTitle":str?,"description":str?,"keywords":[str]|"a; b"?,"copyright":str?,"copyrightStatus":"unknown|copyrighted|publicDomain"?,"copyrightUrl":str?} (no keys: read; the result also has read-only "camera":{"make","model","dateTaken","exposure","fNumber","iso","focalLength","focalLength35mm","lens"}, present keys only)""",
        enabledProc(hasDocE(s)), runProc(fileInfo(s, p))),
    cmd("file.automate.fitImage", "Fit Image…", ["File", "Automate"], "",
        """{"width":px,"height":px,"dontEnlarge":bool=false,"resample":"automatic|preserveDetails|bicubicSmoother|bicubicSharper|bicubicAutomatic|bicubic|nearest|bilinear|lanczos"="bicubic"}""",
        enabledProc(hasDocE(s)), runProc(fitImage(s, p))),
    cmd("file.automate.conditionalModeChange", "Conditional Mode Change…", ["File", "Automate"], "",
        """{"from":["rgb","grayscale","cmyk","lab","indexed","bitmap",…]|"any"="any","to":"rgb|grayscale|cmyk|lab"}""",
        enabledProc(hasDocE(s)), runProc(conditionalModeChange(s, p))),
    cmd("file.automate.batch", "Batch…", ["File", "Automate"], "",
        """{"steps":[[commandId,params]|{"command":id,"params":{}}…] (a recorded action),"input":folder|[paths],"output":folder,"format":"same|png|jpg|psd|tiff|…"="same","quality":0..12?,"tiffLayers":bool=false,"tgaBits":24|32?,"calledActions":[{"name","steps"}…]? (the actions Play Action steps call; default: the session's)} → {files, errors} (an input whose output name was already written in the run goes to errors)""",
        enabledProc(nativeE(s)), runProc(batch(s, p)))]

proc fileSpecsTail*(): seq[CommandSpec] =
  ## The specs after `file.automate.batch` (which lands with the Actions panel).
  @[cmd("file.scripts.imageProcessor", "Image Processor…", ["File", "Scripts"], "",
        """{"input":folder|[paths],"output":folder,"format":"jpg|png|psd|tiff|…"="jpg","quality":0..12=8,"tiffLayers":bool=false,"tgaBits":24|32?,"width":px?,"height":px? (fit, never enlarge),"convertToSrgb":bool=false} → {files, errors} (an input whose output name was already written in the run goes to errors)""",
        enabledProc(nativeE(s)), runProc(imageProcessor(s, p))),
    cmd("file.scripts.loadFilesIntoStack", "Load Files into Stack…", ["File", "Scripts"], "",
        """{"paths":[str]|folder,"createSmartObject":bool=false} → new document with one layer per file (inside one smart object with createSmartObject)""",
        enabledProc(nativeE(s)), runProc(loadFilesIntoStack(s, p))),
    cmd("file.scripts.flattenAllLayerEffects", "Flatten All Layer Effects", ["File", "Scripts"], "", "{}",
        enabledProc(hasDocE(s)), runProc(flattenAllEffects(s))),
    cmd("file.scripts.flattenAllMasks", "Flatten All Masks", ["File", "Scripts"], "",
        "{} (pixel layers; masks on other layer kinds are left)", enabledProc(hasDocE(s)), runProc(flattenAllMasks(s))),
    cmd("file.export.layersToFiles", "Layers to Files…", ["File", "Export"], "",
        """{"dir":folder,"format":"png|jpg|psd|tiff|…"="png","prefix":str=document name,"visibleOnly":bool=true,"quality":0..12?,"tiffLayers":bool=false,"tgaBits":24|32?} → {files}""",
        enabledProc(nativeDocE(s)), runProc(layersToFiles(s, p))),
    cmd("file.export.colorLookupTables", "Color Lookup Tables…", ["File", "Export"], "",
        """{"path":str? (.cube; omit to return the text),"size":2..129=33,"title":str?,"scope":"all|selected"="all","layers":[id,…]?} (selected layer IDs use original document stacking; masks are spatial and omitted)""",
        enabledProc(hasAdjustmentsE(s)), runProc(colorLookupTables(s, p))),
    cmd("view.newGuideLayout", "New Guide Layout…", ["View"], "",
        """{"columns":n=0,"width":px?,"gutter":px=0,"rows":n=0,"height":px?,"rowGutter":px=gutter,"margin":px|[top,left,bottom,right]=0,"centerColumns":bool=false,"clearExisting":bool=false,"target":"document|selectedArtboards"="document"}""",
        enabledProc(hasDocE(s)), runProc(newGuideLayout(s, p))),
    cmd("view.newGuidesFromShape", "New Guides From Shape", ["View"], "", """{"layer":id?}""",
        enabledProc(hasShapeE(s)), runProc(guidesFromShape(s, p))),
    cmd("view.clearCanvasGuides", "Clear Canvas Guides", ["View"], "", "{}", enabledProc(hasDocE(s)), runProc(clearCanvasGuides(s)))]
