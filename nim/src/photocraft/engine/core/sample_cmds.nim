## Eyedropper sampling (port of `sample_cmds.rs`): Photoshop's Sample Size (Point Sample, or the
## average of a 3×3 … 101×101 square centred on the clicked pixel) and Sample (which layers the
## colour comes from). The Eyedropper tool, the painting tools' ⌥-click eyedropper and the Info
## panel read colours through `sampleColor`, so they share one setting.
##
## Averages are taken in the document's own colour model and bit depth (CMYK documents average
## their composite RGB, see `averagingModel`), weighted by alpha, rounded to the document's depth,
## then returned as straight-alpha RGBA. Part of the `engine/core` unit: included, not imported.

const SampleSizes* = [1'u32, 3, 5, 11, 31, 51, 101]
  ## The Sample Size choices: 1 = Point Sample, otherwise the side of the averaged square.

type
  SampleLayers* = enum
    ## Which layers the Eyedropper samples (its options bar's Sample menu).
    slCurrent                       ## the active layer alone
    slCurrentAndBelow               ## the active layer and everything below it
    slAll                           ## the whole visible composite (Photoshop's default)
    slCurrentAndBelowNoAdjustments  ## `slCurrentAndBelow` with adjustment layers left out
    slAllNoAdjustments              ## `slAll` with adjustment layers left out

const SampleLayerIds*: array[5, (SampleLayers, string)] = [
  (slCurrent, "current"), (slCurrentAndBelow, "currentAndBelow"), (slAll, "all"),
  (slAllNoAdjustments, "allNoAdjustments"), (slCurrentAndBelowNoAdjustments, "currentAndBelowNoAdjustments")]
  ## Every choice with its param id, in the options bar's order.

proc parseSampleLayers*(id: string): Option[SampleLayers] =
  ## The choice named `id` (the `sampleLayer` param), none for an unknown one.
  for (v, n) in SampleLayerIds:
    if n == id: return some(v)

proc noAdjustments(w: SampleLayers): bool = w in {slAllNoAdjustments, slCurrentAndBelowNoAdjustments}

proc sampleRect*(x, y: float64; size: uint32; canvas: Rect): Option[Rect] =
  ## The pixels a sample of `size` at document point (`x`, `y`) averages: the `size`×`size` square
  ## centred on the pixel under the point, clipped to `canvas`. None off the canvas (or not
  ## finite), or when `size` is not one of `SampleSizes`.
  if not (x == x and abs(x) != Inf and y == y and abs(y) != Inf and size in SampleSizes): return
  let (fx, fy) = (floor(x), floor(y))
  if fx < float64(int32.low) or fx > float64(int32.high) or fy < float64(int32.low) or fy > float64(int32.high): return
  let (px, py) = (int(fx), int(fy))
  if not canvas.contains(px, py): return
  let r = int(size div 2)
  let square = rect(satSub(px, r), satSub(py, r), satAdd(satAdd(px, r), 1), satAdd(satAdd(py, r), 1))
  some(square.intersect(canvas))

proc quantizeSample(v: float32; depth: SampleType): float32 =
  ## Round `v` to what a channel of `depth` can store (32 bit keeps the float).
  var b: array[4, byte]
  writeSample(b, depth, 0, v)
  readSample(b, depth, 0)

proc averageSamples*(px: openArray[array[4, float32]]; mode: ColorMode; depth: SampleType): array[4, float32] =
  ## The alpha-weighted average of straight-alpha RGBA pixels `px`, taken in the channels of
  ## colour model `mode` at bit depth `depth` (`sample_cmds::average`). Alpha is the mean
  ## coverage; fully transparent (or empty) input averages to transparent black.
  let fmt = PixelFormat(mode: mode, sample: depth, alpha: false)
  let n = min(fmt.channels, 8)
  var sum: array[8, float64]
  var alpha = 0'f64
  var native: array[8, float32]
  for p in px:
    let a = if p[3] == p[3] and abs(p[3]) != Inf: clamp(p[3], 0'f32, 1'f32) else: 0'f32
    if a <= 0 or not (p[0] == p[0] and abs(p[0]) != Inf and p[1] == p[1] and abs(p[1]) != Inf and
                      p[2] == p[2] and abs(p[2]) != Inf):
      continue
    let k = min(fromRgbaInto(fmt, p, native), n)
    for i in 0 ..< k: sum[i] += float64(native[i]) * float64(a)
    alpha += float64(a)
  if px.len == 0 or alpha <= 0: return
  var mean = newSeq[float32](n)
  for i in 0 ..< n: mean[i] = quantizeSample(float32(sum[i] / alpha), depth)
  let c = toRgba(fmt, mean)
  [c[0], c[1], c[2], quantizeSample(float32(alpha / float64(px.len)), depth)]

proc sampledDocument(doc: Document; active: Option[LayerId]; which: SampleLayers): Option[Document] =
  ## The document that renders what `which` samples: the whole document, or a copy with the
  ## layers it leaves out hidden. `active` is the active layer (none: Current Layer samples nothing).
  case which
  of slAll: return some(doc)
  of slCurrent:
    # The active layer on its own, as if it were the only layer.
    if active.isNone: return
    let l = doc.findLayer(active.get)
    if l.isNil: return
    var layer = l[]
    layer.visible = true
    layer.clipped = false
    layer.opacity = 1
    layer.fillOpacity = 1
    layer.blend = bmNormal
    var solo = doc
    solo.layers = @[layer]
    return some(solo)
  else: discard
  var copy = doc
  var walk: seq[(LayerPath, LayerId, bool)]
  for (path, _, l) in doc.walk: walk.add (path, l.id, l.content.kind == lkAdjustment)
  if which in {slCurrentAndBelow, slCurrentAndBelowNoAdjustments}:
    # Everything composited after (above) the active layer, except its own contents.
    var pos = -1
    for i, w in walk:
      if active.isSome and w[1] == active.get:
        pos = i
        break
    if pos < 0: return
    let own = walk[pos][0]
    for i in pos + 1 ..< walk.len:
      let path = walk[i][0]
      if path.len >= own.len and path[0 ..< own.len] == own: continue
      let l = copy.layerAtPtr(path)
      if not l.isNil: l.visible = false
  if which.noAdjustments:
    for w in walk:
      if w[2]:
        let l = copy.layerAtPtr(w[0])
        if not l.isNil: l.visible = false
  some(copy)

proc averagingModel(doc: Document): ColorMode =
  ## The colour model the averages are taken in: the document's, except CMYK (its composite RGB:
  ## the profile round trip is not exact, so a 3×3 sample of a flat area would drift).
  let m = doc.pixelFormat.mode
  if m == cmCmyk: cmRgb else: m

proc sampleColor*(doc: Document; active: Option[LayerId]; x, y: float64; size: uint32; which: SampleLayers): array[4, float32] =
  ## The colour the Eyedropper picks at document point (`x`, `y`) with Sample Size `size` from the
  ## layers `which` names, as straight-alpha RGBA. Transparent black off the canvas, over
  ## transparency, or when `which` needs an active layer and there is none.
  let rc = sampleRect(x, y, size, doc.bounds)
  if rc.isNone: return
  let src = sampledDocument(doc, active, which)
  if src.isNone: return
  let buf = render(src.get, rc.get)
  if size == 1:
    return if buf.px.len > 0: buf.px[0] else: [0'f32, 0, 0, 0]
  averageSamples(buf.px, averagingModel(doc), doc.depth)

proc sizeParam*(cmdId: string; p: JsonNode): uint32 =
  ## The `size` param: 1 (Point Sample) or an averaged square's side; "point" is accepted too.
  let v = p.pget("size")
  if v.isNull: return 1
  if v.kind == JString and v.str == "point": return 1
  let n = v.asU64
  if n.isSome and n.get <= uint64(uint32.high) and uint32(n.get) in SampleSizes: return uint32(n.get)
  raise bad(cmdId, "`size` = " & jsonText(v) & " must be \"point\" or one of [1, 3, 5, 11, 31, 51, 101]")

proc layersParam*(cmdId: string; p: JsonNode): SampleLayers =
  ## The `sampleLayer` param (default `all`).
  let v = p.pget("sampleLayer")
  if v.isNull: return slAll
  if v.kind == JString:
    let w = parseSampleLayers(v.str)
    if w.isSome: return w.get
    var ids: seq[string]
    for (_, n) in SampleLayerIds: ids.add n
    raise bad(cmdId, "unknown sampleLayer `" & v.str & "` (" & ids.join("|") & ")")
  raise bad(cmdId, "`sampleLayer` = " & jsonText(v) & " must be a string")

proc coord(cmdId: string; p: JsonNode; key: string): float64 =
  let v = p.pget(key).asF64
  if v.isNone or v.get != v.get or abs(v.get) == Inf:
    raise bad(cmdId, "`" & key & "` must be a finite number (document pixels)")
  v.get

proc sampleSpecs*(): seq[CommandSpec] =
  @[cmd("document.sampleColor", "Sample Color", [], "",
        """{"x":f64,"y":f64,"size":"point"|1|3|5|11|31|51|101=1,"sampleLayer":"current"|"currentAndBelow"|"all"|"allNoAdjustments"|"currentAndBelowNoAdjustments"="all"} → [r,g,b,a] (the Eyedropper's Sample Size and Sample: the composite pixel, or the average of the size×size square centred on it, clipped to the canvas; transparent off the canvas)""",
        hasDocE, runProc do:
      const cmdId = "document.sampleColor"
      let x = coord(cmdId, p, "x")
      let y = coord(cmdId, p, "y")
      let size = sizeParam(cmdId, p)
      let which = layersParam(cmdId, p)
      let d = s.activeOrRaise()
      let c = sampleColor(d.doc[], d.activeLayer, x, y, size, which)
      %*[float64(c[0]), float64(c[1]), float64(c[2]), float64(c[3])],
        journal = false)]
