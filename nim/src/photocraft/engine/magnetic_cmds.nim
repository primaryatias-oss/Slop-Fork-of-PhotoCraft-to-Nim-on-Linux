## Magnetic Lasso (port of `magnetic_cmds.rs`): a selection border that snaps to edges between
## fastening points, traced by `algo/magnetic`.
##
## The interactive tool traces live through an `EdgeSource` and commits the outline it showed with
## `select.magneticLasso` and `"trace": false`, so an action replays exactly that outline. Given
## fastening points alone, `select.magneticLasso` traces the whole border itself, for scripts and
## agents.

import std/[json, options, math]
import ./core, ./selection_cmds
import ../compose, ../rfmt
import ../algo/magnetic as amagnetic
import ../algo/selection as asel

const
  MagneticCmd = "select.magneticLasso"
  MaxMagneticPoints = 100_000  ## most points one command takes
  MaxBorderLength = 200_000.0  ## longest border one command traces (px, along the fastening points)

type EdgeSource* = object
  ## The pixels the Magnetic Lasso follows: the active layer's when it is a pixel layer (Photoshop's
  ## tool sees only the current layer), else the composite. A snapshot of one document revision,
  ## read lazily and cached while a border is drawn.
  doc: DocRef
  layer: Option[LayerId]
  revision: uint64
  tracer: Tracer

proc hasOwnSurface(l: Layer): bool =
  case l.content.kind
  of lkRaster: true
  of lkText: l.content.text.cache.isSome
  of lkShape: l.content.shape.cache.isSome
  of lkSmart: l.content.smart.cache.isSome
  else: false

proc pixelLayer(d: ptr DocState): Option[LayerId] =
  ## The active layer when it has pixels of its own.
  if d.activeLayer.isNone: return
  let l = d.doc[].findLayer(d.activeLayer.get)
  if not l.isNil and hasOwnSurface(l[]): d.activeLayer else: none(LayerId)

proc newEdgeSource*(s: Session): Option[EdgeSource] =
  ## For the session's active document; none without one.
  let d = s.active
  if d.isNil: return
  some(EdgeSource(doc: d.doc, layer: pixelLayer(d), revision: uint64(d.revision), tracer: newTracer(d.doc[].bounds)))

proc isCurrent*(e: EdgeSource; s: Session): bool =
  ## Whether this still shows the session's active document, layer and revision.
  let d = s.active
  not d.isNil and d.doc.id == e.doc.id and uint64(d.revision) == e.revision and pixelLayer(d) == e.layer

proc readEdges(doc: DocRef; layer: Option[LayerId]): Fetch =
  ## Straight RGBA8 pixels of a rectangle from the layer, or the composite without one.
  result = proc(r: Rect): seq[array[4, uint8]] =
    let l = (if layer.isSome: doc[].findLayer(layer.get) else: nil)
    if not l.isNil and hasOwnSurface(l[]): return asel.rgba8Image(l[].surface, r)
    proc q(v: float32): uint8 = uint8(clamp(v, 0'f32, 1'f32) * 255'f32 + 0.5'f32)
    for p in compose.render(doc[], r).px: result.add [q(p[0]), q(p[1]), q(p[2]), q(p[3])]

proc snap*(e: var EdgeSource; p: array[2, float64]; s: Settings): Option[array[2, float64]] =
  ## Where a fastening point near `p` goes: on the most prominent edge within the detection width,
  ## else at `p`.
  e.tracer.snap(readEdges(e.doc, e.layer), p, s)

proc trace*(e: var EdgeSource; frm, to: array[2, float64]; guide: openArray[array[2, float64]]; s: Settings): seq[array[2, float64]] =
  ## The border from `frm` to `to` along the edges near `guide`, the pointer's path between them.
  e.tracer.trace(readEdges(e.doc, e.layer), frm, to, guide, s)

proc mbad(msg: string): ref EngineError = bad(MagneticCmd, msg)

proc points(p: JsonNode): seq[array[2, float64]] =
  ## `points`: `[[x, y], …]` of finite numbers.
  let a = p.pget("points")
  if not a.isArray: raise mbad("`points` must be [[x, y], …]")
  if a.len > MaxMagneticPoints: raise mbad("at most " & $MaxMagneticPoints & " points")
  for q in a.elems:
    if q.isArray and q.len == 2:
      let (x, y) = (q.elems[0].asF64, q.elems[1].asF64)
      if x.isSome and y.isSome and x.get == x.get and abs(x.get) != Inf and y.get == y.get and abs(y.get) != Inf:
        result.add [x.get, y.get]
        continue
    raise mbad("bad point " & jsonText(q) & " (want [x, y])")

proc number(p: JsonNode; key: string; default, lo, hi: float64): float64 =
  ## A number in `lo..hi`, or `default` when absent.
  let v = p.pget(key)
  if v.isNil: return default
  let x = v.asF64
  if x.isSome and x.get >= lo and x.get <= hi: return x.get
  raise mbad("`" & key & "` must be a number in " & displayF64(lo) & ".." & displayF64(hi) & " (got " & jsonText(v) & ")")

proc traceBorder(src: var EdgeSource; pts: seq[array[2, float64]]; settings: Settings; straight: bool): seq[array[2, float64]] =
  ## The closed border through the fastening points `pts`, each snapped to the nearest prominent
  ## edge first, as the tool places them. The last point joins the first along the edges, or
  ## straight with `straight`.
  let b = src.tracer.bounds
  var ring: seq[array[2, float64]]
  for q in pts: ring.add [clamp(q[0], float64(b.x0), float64(b.x1)), clamp(q[1], float64(b.y0), float64(b.y1))]
  if ring.len > 0: ring.add ring[0]
  if amagnetic.length(ring) > MaxBorderLength: raise mbad("the border is longer than " & displayF64(MaxBorderLength) & " px")
  ring.setLen(ring.len - 1)
  var anchors: seq[array[2, float64]]
  for q in ring:
    let a = src.snap(q, settings)
    if a.isSome: anchors.add a.get
  if anchors.len == 0: raise other("the document has no pixels")
  let first = anchors[0]
  result = @[first]
  for i in 0 ..< anchors.len - 1:
    let seg = src.trace(anchors[i], anchors[i + 1], [], settings)
    for k in 1 ..< seg.len: result.add seg[k]
  if not straight:
    let seg = src.trace(anchors[^1], first, [], settings)
    for k in 1 ..< seg.len: result.add seg[k]
    # The ring ends where it started.
    result.setLen(result.len - 1)

proc magneticLasso(s: var Session; p: JsonNode): JsonNode =
  let pts = points(p)
  if pts.len < 3: raise mbad("needs at least 3 points")
  var trace = true
  let tv = p.pget("trace")
  if not tv.isNil:
    let b = tv.asBool
    if b.isNone: raise mbad("`trace` must be true or false")
    trace = b.get
  var outline: seq[array[2, float64]]
  if trace:
    let width = number(p, "width", 10, WidthRange[0], WidthRange[1])
    let contrast = number(p, "contrast", 10, 1, 100)
    var straight = false
    let cv = p.pget("close")
    if not cv.isNil:
      if cv.kind == JString and cv.str == "magnetic": straight = false
      elif cv.kind == JString and cv.str == "straight": straight = true
      else: raise mbad("`close` must be \"magnetic\" or \"straight\"")
    let settings = newSettings(width, float32(contrast / 100))
    var src = newEdgeSource(s)
    if src.isNone: raise noDocument()
    outline = traceBorder(src.get, pts, settings, straight)
  else: outline = pts
  var polygon: seq[(float32, float32)]
  for q in outline: polygon.add (float32(q[0]), float32(q[1]))
  result = polygonSelection(s, MagneticCmd, "Magnetic Lasso", polygon, p)
  if result.isNil or result.kind != JObject: result = newJObject()
  result["points"] = %outline.len

proc magneticSpecs*(): seq[CommandSpec] =
  @[cmd(MagneticCmd, "Magnetic Lasso", [], "",
        """{"points":[[x,y],…] (≥3 fastening points in order; with trace=false, the finished outline),"width":1..256=10 (px: follows edges this close to the points),"contrast":1..100=10 (%: weaker edges are ignored),"close":"magnetic|straight"="magnetic" (how the last point joins the first),"trace":bool=true,"mode":"replace|add|subtract|intersect"="replace","antiAlias":bool=true,"feather":0..1000=0 (px)}""",
        enabledProc(if s.active.isNil: "no document open" else: ""), runProc(magneticLasso(s, p)))]
