## Per-document painting symmetry (`symmetry_cmds.rs`): straight presets or a sampled Bézier path.
## Path samples reflect across their nearest segment, including curved paths. Part of the
## `engine/core` unit: included, not imported.

const
  MaxSymmetrySegments = 1024 ## keeps nearest-segment searches bounded for long strokes
  SamplesPerCurve = 12

proc symmetryInspect*(p: PaintingSymmetry): JsonNode =
  case p.kind
  of psPath: %*{"mode": "path", "source": p.axis.source, "segments": p.axis.segments.len}
  of psPreset: %*{"mode": p.preset.mode.id, "center": p.preset.center, "rotation": p.preset.rotation}

proc mirrorCount*(p: PaintingSymmetry): int =
  case p.kind
  of psPath: 1
  of psPreset: p.preset.mirrorCount

proc axisFromPath*(source: string; path: Path): SymmetryAxis =
  result.source = source
  for subpath in path.subpaths:
    for seg in subpath.segments:
      let (a, b, c, d) = (seg[0], seg[1], seg[2], seg[3])
      let samples = (if a == b and c == d: 1 else: SamplesPerCurve)
      var previous = [a.x, a.y]
      for step in 1 .. samples:
        if result.segments.len >= MaxSymmetrySegments: raise other("symmetry path is too complex")
        let t = float64(step) / float64(samples)
        let u = 1.0 - t
        let next = [u * u * u * a.x + 3.0 * u * u * t * b.x + 3.0 * u * t * t * c.x + t * t * t * d.x,
                    u * u * u * a.y + 3.0 * u * u * t * b.y + 3.0 * u * t * t * c.y + t * t * t * d.y]
        for v in [previous[0], previous[1], next[0], next[1]]:
          if not (v == v and abs(v) != Inf and abs(v) <= 1_000_000.0):
            raise other("symmetry path coordinates must be finite and within canvas limits")
        if previous != next: result.segments.add [previous, next]
        previous = next
  if result.segments.len == 0: raise other("symmetry path needs at least one nonzero segment")

proc reflect*(axis: SymmetryAxis; point: StrokePoint): StrokePoint =
  var found = false
  var bestDist = 0.0
  var bq, btan: array[2, float64]
  for seg in axis.segments:
    let (a, b) = (seg[0], seg[1])
    let vx = b[0] - a[0]
    let vy = b[1] - a[1]
    let len2 = vx * vx + vy * vy
    if len2 <= 2.220446049250313e-16: continue # f64::EPSILON
    let t = clamp(((point.x - a[0]) * vx + (point.y - a[1]) * vy) / len2, 0.0, 1.0)
    let q = [a[0] + vx * t, a[1] + vy * t]
    let dist2 = (point.x - q[0]) * (point.x - q[0]) + (point.y - q[1]) * (point.y - q[1])
    if not found or dist2 < bestDist:
      found = true
      bestDist = dist2
      bq = q
      btan = [vx, vy]
  if not found: return point
  let len2 = btan[0] * btan[0] + btan[1] * btan[1]
  let dx = point.x - bq[0]
  let dy = point.y - bq[1]
  let along = (dx * btan[0] + dy * btan[1]) / len2
  result = point
  result.x = bq[0] + 2.0 * along * btan[0] - dx
  result.y = bq[1] + 2.0 * along * btan[1] - dy

proc reflectPoints*(axis: SymmetryAxis; points: openArray[StrokePoint]): seq[StrokePoint] =
  for p in points: result.add axis.reflect(p)

proc reflectedPasses*(s: PaintingSymmetry; pts: openArray[StrokePoint]): seq[seq[StrokePoint]] =
  case s.kind
  of psPath: @[s.axis.reflectPoints(pts)]
  of psPreset: s.preset.reflectedPasses(pts)

proc hasDistinctMirror*(pts, mirrored: openArray[StrokePoint]): bool =
  ## A stroke lying exactly on the symmetry path needs only one render pass: its reflected copy
  ## would compound semi-transparent paint along the axis.
  for i in 0 ..< min(pts.len, mirrored.len):
    if abs(pts[i].x - mirrored[i].x) > 1.0e-6 or abs(pts[i].y - mirrored[i].y) > 1.0e-6: return true
  false

proc setSymmetryPreset(s: var Session; p: JsonNode): JsonNode =
  let failure = other("symmetry requires a valid mode, a finite [x,y] centre and a finite rotation")
  let st = s.active
  if st.isNil: raise noDocument()
  let modeName = p.pget("mode").asStr
  if modeName.isNone: raise failure
  let mode = symmetryFromId(modeName.get)
  if mode.isNone: raise failure
  var center = [float64(st.doc.size.width) / 2.0, float64(st.doc.size.height) / 2.0]
  let c = p.pget("center")
  if not c.isNil:
    if not c.isArray or c.len != 2: raise failure
    let (x, y) = (c.elems[0].asF64, c.elems[1].asF64)
    if x.isNone or y.isNone: raise failure
    center = [x.get, y.get]
  var rotation = 0.0
  let r = p.pget("rotation")
  if not r.isNil:
    let v = r.asF64
    if v.isNone: raise failure
    rotation = v.get
  let preset = newPresetSymmetry(mode.get, center, rotation)
  if preset.isNone: raise failure
  let symmetry = PaintingSymmetry(kind: psPreset, preset: preset.get)
  result = symmetryInspect(symmetry)
  st.symmetry = some(symmetry)

proc symmetryPath(s: Session; name: string): Path =
  let st = s.active
  if st.isNil: raise noDocument()
  if name == "work":
    if st.doc.workPath.isNone: raise other("no work path")
    return st.doc.workPath.get
  if name == "layer":
    if st.activeLayer.isNone: raise other("no active layer")
    let id = st.activeLayer.get
    let l = st.doc[].findLayer(id)
    if l.isNil: raise noLayer(id)
    if l[].content.kind == lkShape: return l[].content.shape.path
    if l[].vectorMask.isNone: raise other("active layer has no vector path")
    return l[].vectorMask.get.path
  for np in st.doc.paths:
    if np.name == name: return np.path
  raise other("no path named `" & name & "`")

proc symmetryEnable(s: var Session; p: JsonNode): JsonNode =
  let name = p.pget("name").asStr.get("work")
  let axis = axisFromPath(name, symmetryPath(s, name))
  let count = axis.segments.len
  let st = s.active
  if st.isNil: raise noDocument()
  st.symmetry = some(PaintingSymmetry(kind: psPath, axis: axis))
  %*{"enabled": true, "source": name, "segments": count}

proc symmetryDisable(s: var Session): JsonNode =
  let st = s.active
  if st.isNil: raise noDocument()
  st.symmetry = none(PaintingSymmetry)
  %*{"enabled": false}

proc symmetrySpecs*(): seq[CommandSpec] =
  let hasDoc = enabledProc(if s.active.isNil: "no document open" else: "")
  @[
    cmd("paint.setSymmetry", "Set Painting Symmetry", [], "",
        "{\"mode\":\"vertical|horizontal|dual|diagonal\",\"center\":[x,y]?,\"rotation\":degrees=0}", hasDoc,
        runProc(setSymmetryPreset(s, p))),
    cmd("paint.symmetryFromPath", "Make Symmetry Path", [], "",
        "{\"name\":\"work\"|\"layer\"|savedPathName=\"work\"} — mirror Brush, Pencil and Eraser strokes across the path",
        hasDoc, runProc(symmetryEnable(s, p))),
    cmd("paint.symmetryDisable", "Disable Painting Symmetry", [], "", "{}",
        enabledProc(if s.active.isNil or s.active.symmetry.isNone: "no active painting symmetry" else: ""),
        runProc(symmetryDisable(s))),
  ]
