## Color Sampler points (port of `sampler_cmds.rs`): numbered colour readings pinned to canvas
## pixels and listed in the Info panel.
##
## Points are per-document view state (`DocState.colorSamplers`): they belong to their document,
## are discarded when it closes, and are not saved. Placing, moving and removing them changes no
## pixels and adds no History step. Each point reads one composite pixel; its values match the
## Info panel's pointer readout (RGB 0–255 and the naive device CMYK percentages).

import std/[json, options, math]
import ./core

const MaxSamplers* = 10 ## Photoshop's Color Sampler limit per document

proc hasDocE(s: Session): string = (if s.active.isNil: "no document open" else: "")

proc deviceCmyk*(r, g, b: float32): array[4, int] =
  ## Naive device CMYK (no ICC profile), in percent, as the Info panel shows it.
  let k = 1'f32 - max(max(r, g), b)
  proc f(v: float32): float32 = (if k >= 1: 0'f32 else: (1 - v - k) / (1 - k))
  proc q(v: float32): int = int(round(v * 100'f32))
  [q(f(r)), q(f(g)), q(f(b)), q(k)]

proc readout*(doc: Document; pos: array[2, float64]): (array[3, int], array[4, int]) =
  ## The Info-panel values for a sample point at document pixel `pos`: composite RGB 0–255 and
  ## naive device CMYK percentages. One composite pixel, no averaging.
  let rgba = sampleColor(doc, none(LayerId), pos[0], pos[1], 1, slAll)
  proc q(v: float32): int = int(round(clamp(v, 0'f32, 1'f32) * 255'f32))
  ([q(rgba[0]), q(rgba[1]), q(rgba[2])], deviceCmyk(rgba[0], rgba[1], rgba[2]))

proc pointJson*(doc: Document; index: int; pos: array[2, float64]): JsonNode =
  let (rgb, cmyk) = readout(doc, pos)
  %*{"index": index, "number": index + 1, "position": [pos[0], pos[1]], "rgb": rgb, "cmyk": cmyk}

proc coord(cmdId: string; p: JsonNode; key: string): float64 =
  let v = p.pget(key).asF64
  if v.isNone or v.get != v.get or abs(v.get) == Inf:
    raise bad(cmdId, "`" & key & "` must be a finite number (document pixels)")
  v.get

proc pixelAt(doc: Document; x, y: float64): Option[array[2, float64]] =
  ## The pixel a point at document coordinate `(x, y)` sits on, if it is inside the canvas.
  let (px, py) = (floor(x), floor(y))
  if px < float64(low(int32)) or px > float64(high(int32)) or py < float64(low(int32)) or py > float64(high(int32)): return
  if doc.bounds.contains(int(px), int(py)): some([px, py]) else: none(array[2, float64])

proc indexParam(cmdId: string; p: JsonNode): int =
  let v = p.pget("index").asU64
  if v.isNone or v.get > uint64(high(int)): raise bad(cmdId, "pass `index` (0-based)")
  int(v.get)

template change(s: var Session; body: untyped): untyped =
  ## Apply a Color Sampler change to the active document: view state, so no history step and no
  ## pixels. Bumps the revision (keeping a clean document clean) so UIs refresh.
  let st {.inject.} = s.active
  if st.isNil: raise noDocument()
  let r = body
  let clean = st.savedRevision == st.revision
  inc st.revision
  if clean: st.savedRevision = st.revision
  st.lastDamage = some(EmptyRect)
  r

proc addSampler(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "view.colorSamplers.add"
  let x = coord(cmdId, p, "x")
  let y = coord(cmdId, p, "y")
  change(s):
    let pos = pixelAt(st.doc[], x, y)
    if pos.isNone: raise other("the sample point is outside the canvas")
    if st.colorSamplers.len >= MaxSamplers:
      raise other("Color Sampler limit reached: at most " & $MaxSamplers & " points per document")
    st.colorSamplers.add pos.get
    let i = st.colorSamplers.len - 1
    %*{"index": i, "count": st.colorSamplers.len, "point": pointJson(st.doc[], i, pos.get)}

proc moveSampler(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "view.colorSamplers.move"
  let i = indexParam(cmdId, p)
  let x = coord(cmdId, p, "x")
  let y = coord(cmdId, p, "y")
  change(s):
    let n = st.colorSamplers.len
    if i >= n: raise bad(cmdId, "no sample point " & $i & " (the document has " & $n & ")")
    let pos = pixelAt(st.doc[], x, y)
    if pos.isNone: raise other("the sample point is outside the canvas")
    st.colorSamplers[i] = pos.get
    %*{"index": i, "point": pointJson(st.doc[], i, pos.get)}

proc deleteSampler(s: var Session; p: JsonNode): JsonNode =
  const cmdId = "view.colorSamplers.delete"
  let i = indexParam(cmdId, p)
  change(s):
    let n = st.colorSamplers.len
    if i >= n: raise bad(cmdId, "no sample point " & $i & " (the document has " & $n & ")")
    st.colorSamplers.delete(i)
    %*{"deleted": 1, "remaining": st.colorSamplers.len}

proc clearSamplers(s: var Session): JsonNode =
  change(s):
    let n = st.colorSamplers.len
    st.colorSamplers.setLen(0)
    %*{"deleted": n, "remaining": 0}

proc listSamplers(s: var Session): JsonNode =
  let st = s.active
  if st.isNil: raise noDocument()
  var a = newJArray()
  for i, p in st.colorSamplers: a.add pointJson(st.doc[], i, p)
  %*{"max": MaxSamplers, "samplers": a}

proc samplerSpecs*(): seq[CommandSpec] =
  @[cmd("view.colorSamplers.add", "Add Color Sampler", [], "",
        """{"x":f64,"y":f64} → the new point {index, number, position, rgb, cmyk}; errors at the 10-point limit or off the canvas""",
        hasDocE, runProc(addSampler(s, p)), journal = false),
    cmd("view.colorSamplers.move", "Move Color Sampler", [], "", """{"index":n,"x":f64,"y":f64}""", hasDocE,
        runProc(moveSampler(s, p)), journal = false),
    cmd("view.colorSamplers.delete", "Delete Color Sampler", [], "", """{"index":n}""", hasDocE,
        runProc(deleteSampler(s, p)), journal = false),
    cmd("view.colorSamplers.clear", "Clear Color Samplers", [], "", "{}", hasDocE, runProc(clearSamplers(s)), journal = false),
    cmd("view.colorSamplers.list", "Color Samplers", [], "", """{} → {max, samplers:[{index, number, position, rgb, cmyk}]}""",
        hasDocE, runProc(listSamplers(s)), journal = false)]
