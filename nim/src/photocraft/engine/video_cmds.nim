## Window › Timeline (port of `timeline_cmds.rs`) and Layer › Video Layers (`video_cmds.rs`).
##
## The timeline is a playhead over N frames at a frame rate; navigation adds no history steps. A
## video layer holds a stack of frames; the layer's displayed raster content is the frame at the
## playhead, kept in sync by `syncVideo` after any edit or navigation. Paint, fills and filters edit
## that displayed content, so `storeVideo` writes it back into the playhead's frame before anything
## re-syncs. Render Video writes an image sequence or an animated GIF.

import std/[json, options, os, math, algorithm, strutils]
import ./core
import ../compose, ../io
from ../format import NulPathMsg
import ../io/flat as ioflat
import ../codecs/web as cweb
import ../algo/quantize as aquant

# ---------------------------------------------------------------- video layers: sync and store

proc syncLayers(layers: var seq[Layer]; cur: int) =
  for l in layers.mitems:
    if l.video.isSome:
      let v = l.video.get
      let visible = (if v.showAltered: v.frames elif v.originalFrames.isSome: v.originalFrames.get else: v.frames)
      if visible.len > 0 and l.content.kind == lkRaster: l.content.raster = visible[min(cur, visible.len - 1)]
    let kids = childrenMut(l)
    if not kids.isNil: syncLayers(kids[], cur)

proc syncVideo*(doc: var Document) =
  ## Set every video layer's displayed content to its frame at the playhead.
  let cur = (if doc.timeline.isSome: doc.timeline.get.current else: 0)
  syncLayers(doc.layers, cur)

proc storeLayers(layers: var seq[Layer]; cur: int) =
  for l in layers.mitems:
    if l.video.isSome and l.content.kind == lkRaster:
      let at = min(cur, max(l.video.get.frames.len - 1, 0))
      # When showing the original, the displayed surface is a preview, not an edit to write over
      # the altered frame.
      if (l.video.get.showAltered or l.video.get.originalFrames.isNone) and at < l.video.get.frames.len:
        l.video.get.frames[at] = l.content.raster
    let kids = childrenMut(l)
    if not kids.isNil: storeLayers(kids[], cur)

proc storeVideo*(doc: var Document) =
  ## Keep edits made to every video layer's displayed content by writing it back into its frame at
  ## the playhead. Call before moving the playhead or re-syncing.
  let cur = (if doc.timeline.isSome: doc.timeline.get.current else: 0)
  storeLayers(doc.layers, cur)

# ---------------------------------------------------------------- timeline_cmds.rs

const
  MaxDuration = 1_000_000 ## longest timeline, in frames
  MaxFps = 1000'f32       ## highest frame rate

proc timelineDocE(s: Session): string = (if s.active.isNil: "no document" else: "")

proc hasTimelineE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document"
  if d.doc.timeline.isSome: "" else: "no timeline (create one first)"

proc timelineInfo(s: Session): JsonNode =
  let st = s.active
  if st.isNil: raise noDocument()
  if st.doc.timeline.isNone: return %*{"timeline": nil}
  let t = st.doc.timeline.get
  %*{"timeline": {"fps": t.fps, "duration": t.duration, "current": t.current, "workStart": t.workStart,
                  "workEnd": t.workEnd, "time": t.time}}

proc withTimeline(s: var Session; f: proc(t: var Option[Timeline])): JsonNode =
  ## Mutate the document timeline without a history step (navigation / metadata).
  let st = s.active
  if st.isNil: raise noDocument()
  var doc = st.doc[]
  storeVideo(doc)
  f(doc.timeline)
  if doc.timeline.isSome: doc.timeline.get.clamp()
  syncVideo(doc)
  let r = DocRef()
  r[] = doc
  st.doc = r
  inc st.revision
  timelineInfo(s)

proc durationParam(cmdId: string; p: JsonNode): Option[int] =
  ## The optional `duration` param: a whole number of frames in `1..=MaxDuration`.
  let v = p.pget("duration")
  if v.isNull: return none(int)
  let d = v.asU64
  if d.isSome and d.get >= 1 and d.get <= uint64(MaxDuration): return some(int(d.get))
  raise bad(cmdId, "`duration` must be a whole number of frames from 1 to " & $MaxDuration & ", got " & jsonText(v))

proc fpsParam(cmdId: string; p: JsonNode): Option[float32] =
  ## The optional `fps` param: a finite number in `(0, MaxFps]`.
  let v = p.pget("fps")
  if v.isNull: return none(float32)
  let f = v.asF64
  if f.isSome and f.get == f.get and abs(f.get) != Inf and f.get > 0 and f.get <= float64(MaxFps): return some(float32(f.get))
  raise bad(cmdId, "`fps` must be a number above 0 and at most " & $int(MaxFps) & ", got " & jsonText(v))

proc createTimeline(s: var Session; p: JsonNode): JsonNode =
  let duration = durationParam("timeline.create", p).get(30)
  let fps = fpsParam("timeline.create", p).get(30)
  withTimeline(s, proc(t: var Option[Timeline]) = t = some(newTimeline(duration, fps)))

proc setFrame(s: var Session; p: JsonNode): JsonNode =
  let f = p.pget("frame").asU64
  if f.isNone: raise bad("timeline.setFrame", "need `frame`")
  let frame = int(min(f.get, uint64(high(int))))
  withTimeline(s, proc(t: var Option[Timeline]) =
    if t.isSome: t.get.current = frame)

proc setProps(s: var Session; p: JsonNode): JsonNode =
  let fps = fpsParam("timeline.setProps", p)
  let duration = durationParam("timeline.setProps", p)
  let ws = p.pget("workStart").asU64
  let we = p.pget("workEnd").asU64
  withTimeline(s, proc(t: var Option[Timeline]) =
    if t.isSome:
      if fps.isSome: t.get.fps = fps.get
      if duration.isSome: t.get.duration = duration.get
      if ws.isSome: t.get.workStart = int(min(ws.get, uint64(high(int))))
      if we.isSome: t.get.workEnd = int(min(we.get, uint64(high(int)))))

proc timelineSpecs*(): seq[CommandSpec] =
  @[cmd("timeline.create", "Create Video Timeline", [], "", """{duration?:30, fps?:30} → {timeline}""", timelineDocE,
        runProc(createTimeline(s, p)), journal = false),
    cmd("timeline.delete", "Delete Timeline", [], "", "{} → {timeline:null}", hasTimelineE,
        runProc(withTimeline(s, proc(t: var Option[Timeline]) = t = none(Timeline))), journal = false),
    cmd("timeline.setFrame", "Go to Frame", [], "", """{frame} → {timeline}""", hasTimelineE, runProc(setFrame(s, p)), journal = false),
    cmd("timeline.nextFrame", "Next Frame", [], "", "{} → {timeline}", hasTimelineE,
        runProc(withTimeline(s, proc(t: var Option[Timeline]) =
          if t.isSome: t.get.step(1))), journal = false),
    cmd("timeline.previousFrame", "Previous Frame", [], "", "{} → {timeline}", hasTimelineE,
        runProc(withTimeline(s, proc(t: var Option[Timeline]) =
          if t.isSome: t.get.step(-1))), journal = false),
    cmd("timeline.setProps", "Timeline Settings", [], "", """{fps?, duration?, workStart?, workEnd?} → {timeline}""",
        hasTimelineE, runProc(setProps(s, p)), journal = false),
    cmd("timeline.info", "Timeline Info", [], "", "{} → {timeline}", timelineDocE, runProc(timelineInfo(s)), journal = false)]

# ---------------------------------------------------------------- video_cmds.rs

proc activeVideoE(s: Session): string =
  let st = s.active
  if st.isNil: return "no document"
  let l = (if st.activeLayer.isSome: st.doc[].findLayer(st.activeLayer.get) else: nil)
  if not l.isNil and l[].video.isSome: "" else: "select a video layer"

proc videoDocE(s: Session): string = (if s.active.isNil: "no document" else: "")

type VideoEdit = proc(v: var VideoData; cur: int)

proc editVideo(s: var Session; label: string; f: VideoEdit): JsonNode =
  ## Edit the active video layer's `VideoData` at the playhead, then re-sync the displayed frame.
  let count = s.editWith(label, proc(doc: var Document; active: var Option[LayerId]): int =
    storeVideo(doc)
    let cur = (if doc.timeline.isSome: doc.timeline.get.current else: 0)
    if active.isNone: raise noDocument()
    let id = active.get
    let l = doc.findLayer(id)
    if l.isNil: raise noLayer(id)
    if l[].video.isNone: raise bad(label, "the active layer is not a video layer")
    f(l[].video.get, cur)
    let n = l[].video.get.frames.len
    if doc.timeline.isSome:
      doc.timeline.get.duration = max(doc.timeline.get.duration, max(n, 1))
      doc.timeline.get.workEnd = max(doc.timeline.get.workEnd, doc.timeline.get.duration)
      doc.timeline.get.clamp()
    syncVideo(doc)
    n)
  %*{"frames": count}

proc newBlank(s: var Session): JsonNode =
  let id = s.editWith("New Blank Video Layer", proc(doc: var Document; active: var Option[LayerId]): LayerId =
    storeVideo(doc)
    let fmt = doc.pixelFormat
    if doc.timeline.isNone: doc.timeline = some(newTimeline(1, 30))
    let fps = doc.timeline.get.fps
    let n = max(doc.timeline.get.duration, 1)
    var frames = newSeq[Surface](n)
    for f in frames.mitems: f = newSurface(fmt)
    let name = doc.nextLayerName("Video Layer")
    var l = rasterLayer(name, fmt)
    l.video = some(newVideoData(frames, fps))
    result = doc.insertAbove(active, l)
    active = some(result)
    syncVideo(doc))
  %*{"layer": uint64(id)}

proc docFormat(s: Session): PixelFormat =
  if s.active.isNil: raise noDocument()
  s.active.doc[].pixelFormat

proc insertBlank(s: var Session): JsonNode =
  let fmt = docFormat(s)
  editVideo(s, "Insert Blank Frame", proc(v: var VideoData; cur: int) =
    let at = min(cur + 1, v.frames.len)
    v.frames.insert(newSurface(fmt), at)
    if v.originalFrames.isSome:
      # The stacks stay aligned, but never index past a shorter one.
      v.originalFrames.get.insert(newSurface(fmt), min(at, v.originalFrames.get.len)))

proc duplicateFrame(s: var Session): JsonNode =
  editVideo(s, "Duplicate Frame", proc(v: var VideoData; cur: int) =
    if cur < v.frames.len:
      let f = v.frames[cur]
      if v.originalFrames.isSome:
        let orig = (if cur < v.originalFrames.get.len: v.originalFrames.get[cur] else: newSurface(f.format))
        v.originalFrames.get.insert(orig, min(cur + 1, v.originalFrames.get.len))
      v.frames.insert(f, cur + 1))

proc deleteFrame(s: var Session): JsonNode =
  editVideo(s, "Delete Frame", proc(v: var VideoData; cur: int) =
    if v.frames.len > 1 and cur < v.frames.len:
      v.frames.delete(cur)
      if v.originalFrames.isSome and cur < v.originalFrames.get.len: v.originalFrames.get.delete(cur))

proc restoreFrame(s: var Session): JsonNode =
  let fmt = docFormat(s)
  editVideo(s, "Restore Frame", proc(v: var VideoData; cur: int) =
    let original = (if v.originalFrames.isSome and cur < v.originalFrames.get.len: some(v.originalFrames.get[cur]) else: none(Surface))
    if cur < v.frames.len: v.frames[cur] = (if original.isSome: original.get else: newSurface(fmt)))

proc restoreAll(s: var Session): JsonNode =
  let fmt = docFormat(s)
  editVideo(s, "Restore All Frames", proc(v: var VideoData; cur: int) =
    for i in 0 ..< v.frames.len:
      v.frames[i] = (if v.originalFrames.isSome and i < v.originalFrames.get.len: v.originalFrames.get[i] else: newSurface(fmt)))

proc showAltered(s: var Session): JsonNode =
  editVideo(s, "Show Altered Video", proc(v: var VideoData; cur: int) = v.showAltered = not v.showAltered)

proc rasterizeVideo(s: var Session): JsonNode =
  ## Rasterize: drop the frame stack, keeping the current frame as a normal pixel layer.
  s.edit("Rasterize Video", proc(doc: var Document; active: var Option[LayerId]) =
    if active.isNone: raise noDocument()
    let l = doc.findLayer(active.get)
    if l.isNil: raise noLayer(active.get)
    # The content already shows the current frame (kept in sync); just drop the stack.
    l[].video = none(VideoData))
  %*{"rasterized": true}

const FrameExtensions = ["png", "jpg", "jpeg", "tif", "tiff", "webp", "bmp", "gif", "tga", "exr", "pcraft", "psd"]

proc loadFrames(path: string; fmt: PixelFormat): seq[Surface] =
  ## Load frames from `path`: a single image (1 frame), or a directory of images (sorted = an image
  ## sequence). Each becomes a full raster frame in the document's format.
  proc isImg(f: string): bool =
    let e = splitFile(f).ext
    e.len > 1 and toLowerAscii(e[1 .. ^1]) in FrameExtensions
  var files: seq[string]
  if '\0' notin path and dirExists(path):
    try:
      for _, f in walkDir(path):
        if isImg(f): files.add f
    except OSError: raise other("read dir `" & path & "`: " & osErrorText(osLastError()))
    files.sort(cmp[string])
  else: files = @[path]
  if files.len == 0: raise other("no images in `" & path & "`")
  for f in files:
    var bytes: seq[byte]
    try: bytes = readBytesRust(f)
    except OSError as e: raise other("read `" & f & "`: " & e.msg)
    var doc = fileImport(f, bytes)
    # A Targa's alpha opens as a channel (as in Photoshop); a frame's alpha is transparency.
    if cmpIgnoreCase(splitFile(f).ext, ".tga") == 0: ioflat.tgaAlphaChannelToTransparency(doc)
    result.add flattenToSurface(doc, fmt)

proc pathParam(p: JsonNode; cmdId: string): string =
  let v = p.pget("path").asStr
  if v.isNone: raise bad(cmdId, "need `path`")
  v.get

proc newFromFile(s: var Session; p: JsonNode): JsonNode =
  let path = pathParam(p, "layer.videoLayers.newVideoLayerFromFile")
  let fps = float32(p.pget("fps").asF64.get(30.0))
  let fmt = docFormat(s)
  let frames = loadFrames(path, fmt)
  let n = frames.len
  let id = s.editWith("New Video Layer from File", proc(doc: var Document; active: var Option[LayerId]): LayerId =
    storeVideo(doc)
    if doc.timeline.isNone: doc.timeline = some(newTimeline(n, fps))
    else:
      doc.timeline.get.duration = max(doc.timeline.get.duration, n)
      doc.timeline.get.clamp()
    let stem = splitFile(path).name
    let name = doc.nextLayerName(if stem.len > 0: stem else: "Video")
    var l = rasterLayer(name, fmt)
    var v = newVideoData(frames, fps)
    v.originalFrames = some(v.frames)
    v.source = VideoSource(kind: vsFile, path: path)
    l.video = some(v)
    result = doc.insertAbove(active, l)
    active = some(result)
    syncVideo(doc))
  %*{"layer": uint64(id), "frames": n}

proc replaceFootage(s: var Session; p: JsonNode): JsonNode =
  let path = pathParam(p, "layer.videoLayers.replaceFootage")
  let frames = loadFrames(path, docFormat(s))
  editVideo(s, "Replace Footage", proc(v: var VideoData; cur: int) =
    v.originalFrames = some(frames)
    v.frames = frames
    v.source = VideoSource(kind: vsFile, path: path))

proc interpretFootage(s: var Session; p: JsonNode): JsonNode =
  let fps = p.pget("fps").asF64
  editVideo(s, "Interpret Footage", proc(v: var VideoData; cur: int) =
    if fps.isSome: v.fps = float32(fps.get))

proc reloadFrame(s: var Session): JsonNode =
  ## Reload the whole stack from its file source (what a frame reload needs for a sequence).
  const cmdId = "layer.videoLayers.reloadFrame"
  let st = s.active
  if st.isNil: raise noDocument()
  if st.activeLayer.isNone: raise noDocument()
  let l = st.doc[].findLayer(st.activeLayer.get)
  if l.isNil or l[].video.isNone: raise bad(cmdId, "not a video layer")
  let src = l[].video.get.source
  if src.kind != vsFile: raise bad(cmdId, "this layer has no file source to reload")
  let frames = loadFrames(src.path, st.doc[].pixelFormat)
  editVideo(s, "Reload Frame", proc(v: var VideoData; cur: int) =
    v.originalFrames = some(frames)
    v.frames = frames)

proc framesToLayers(s: var Session; p: JsonNode): JsonNode =
  ## Import › Video Frames to Layers: load frames from a file/sequence and add each as its own layer.
  let path = pathParam(p, "file.import.videoFramesToLayers")
  let fmt = docFormat(s)
  let frames = loadFrames(path, fmt)
  let n = frames.len
  s.edit("Video Frames to Layers", proc(doc: var Document; active: var Option[LayerId]) =
    var above = active
    for i, f in frames:
      var l = rasterLayer("Frame " & $(i + 1), fmt)
      l.content.raster = f
      above = some(doc.insertAbove(above, l))
    active = above)
  %*{"layers": n}

proc gifFrame(px0: seq[array[4, float32]]; w: int; delayCs: uint16): GifFrame =
  ## One animated GIF frame from straight RGBA pixels, as Save for Web's GIF does: pixels under
  ## 50 % opacity get the frame's transparent index, the rest are drawn opaque, and the palette is
  ## built from the visible pixels with one entry kept for transparency.
  var px = px0
  var transparent = false
  var visible: seq[array[4, float32]]
  for p in px:
    if p[3] < 0.5: transparent = true
    else: visible.add [p[0], p[1], p[2], 1'f32]
  # A fully transparent frame still needs a palette.
  var pal: seq[Rgb8]
  try: pal = buildPalette((if visible.len == 0: px else: visible), pkAdaptive, (if transparent: 255 else: 256), fcNone)
  except CatchableError as e: raise other(e.msg)
  var t = none(int)
  if transparent:
    if pal.len > 255: pal.setLen(255)
    pal.add (if pal.len > 0: pal[0] else: [0'u8, 0, 0])
    t = some(pal.len - 1)
  let indices = aquant.quantize(px, w, pal, dtNone, 1, t)
  GifFrame(indices: indices, palette: pal, transparent: (if t.isSome: some(uint8(t.get)) else: none(uint8)), delayCs: delayCs)

proc renderVideoWith*(s: Session; p: JsonNode; write: proc(name: string; bytes: seq[byte]);
                      progress: proc(done, total: int): bool): JsonNode =
  ## Render through a caller-owned writer. The writer receives one filename, never an ambient
  ## directory. Returning false from progress stops at a frame boundary.
  let dir = p.pget("dir").asStr.get("")
  let format = p.pget("format").asStr.get("png")
  let st = s.active
  if st.isNil: raise noDocument()
  var base = st.doc[]
  storeVideo(base)
  let frames = max((if base.timeline.isSome: base.timeline.get.duration else: 1), 1)
  let fps = max((if base.timeline.isSome: base.timeline.get.fps else: 30'f32), 1'f32)
  let dot = base.name.rfind('.')
  let stem = (if dot >= 0: base.name[0 ..< dot] else: base.name)
  let fmt = base.pixelFormat
  let bounds = base.bounds
  let (w, h) = (bounds.width, bounds.height)
  template cancelled(): untyped = other("render cancelled")
  if not progress(0, frames): raise cancelled()
  # Animated GIF: one file, every frame quantised to its own palette.
  if cmpIgnoreCase(format, "gif") == 0:
    let delay = uint16(clamp(round(100'f32 / fps), 1'f32, 65535'f32))
    var gframes: seq[GifFrame]
    for f in 0 ..< frames:
      var doc = base
      if doc.timeline.isSome: doc.timeline.get.current = f
      syncVideo(doc)
      let surf = flattenToSurface(doc, fmt)
      var px = newSeq[array[4, float32]](w * h)
      if px.len > 0: surf.readRgbaInto(bounds, px)
      gframes.add gifFrame(px, w, delay)
      # The final report follows encoding/writing, so total means the output exists.
      if not progress(min(f + 1, max(frames - 1, 0)), frames): raise cancelled()
    var gif: seq[byte]
    try: gif = encodeGifAnimated(uint32(w), uint32(h), gframes, true)
    except CatchableError as e: raise other(e.msg)
    let name = stem & ".gif"
    write(name, gif)
    if not progress(frames, frames): raise cancelled()
    return %*{"frames": frames, "file": dir & "/" & name}
  # Image sequence.
  var files: seq[string]
  for f in 0 ..< frames:
    var doc = base
    if doc.timeline.isSome: doc.timeline.get.current = f
    syncVideo(doc)
    var num = $f
    while num.len < 4: num = "0" & num
    let path = stem & "_" & num & "." & format
    var bytes: seq[byte]
    try: bytes = exportDoc(doc, format).bytes
    except CatchableError as e: raise other("render frame " & $f & ": " & e.msg)
    write(path, bytes)
    files.add path
    if not progress(f + 1, frames): raise cancelled()
  %*{"frames": files.len, "dir": dir}

proc renderVideo(s: var Session; p: JsonNode): JsonNode =
  ## Export › Render Video: composite the whole document at each timeline frame and write an image
  ## sequence (`{dir}/{stem}_NNNN.{format}`) or an animated GIF.
  let dir = p.pget("dir").asStr
  if dir.isNone: raise bad("file.export.renderVideo", "need `dir`")
  if '\0' in dir.get: raise other("mkdir `" & dir.get & "`: " & NulPathMsg)
  try: createDir(dir.get)
  except OSError: raise other("mkdir `" & dir.get & "`: " & osErrorText(osLastError()))
  renderVideoWith(s, p, proc(name: string; bytes: seq[byte]) = writeFileAtomic(dir.get & "/" & name, bytes),
                  proc(done, total: int): bool = true)

proc videoSpecs*(): seq[CommandSpec] =
  const vl = ["Layer", "Video Layers"]
  @[cmd("layer.videoLayers.newBlankVideoLayer", "New Blank Video Layer", vl, "", "{} → {}", videoDocE, runProc(newBlank(s))),
    cmd("layer.videoLayers.insertBlankFrame", "Insert Blank Frame", vl, "", "{} → {}", activeVideoE, runProc(insertBlank(s))),
    cmd("layer.videoLayers.duplicateFrame", "Duplicate Frame", vl, "", "{} → {}", activeVideoE, runProc(duplicateFrame(s))),
    cmd("layer.videoLayers.deleteFrame", "Delete Frame", vl, "", "{} → {}", activeVideoE, runProc(deleteFrame(s))),
    cmd("layer.videoLayers.restoreFrame", "Restore Frame", vl, "", "{} → {}", activeVideoE, runProc(restoreFrame(s))),
    cmd("layer.videoLayers.restoreAllFrames", "Restore All Frames", vl, "", "{} → {}", activeVideoE, runProc(restoreAll(s))),
    cmd("layer.videoLayers.showAlteredVideo", "Show Altered Video", vl, "", "{} → {}", activeVideoE, runProc(showAltered(s))),
    cmd("layer.videoLayers.rasterize", "Rasterize", vl, "", "{} → {}", activeVideoE, runProc(rasterizeVideo(s))),
    cmd("layer.rasterize.video", "Video", ["Layer", "Rasterize"], "", "{} → {}", activeVideoE, runProc(rasterizeVideo(s))),
    cmd("layer.videoLayers.newVideoLayerFromFile", "New Video Layer from File…", vl, "", "{} → {}", videoDocE,
        runProc(newFromFile(s, p))),
    cmd("layer.videoLayers.replaceFootage", "Replace Footage…", vl, "", "{} → {}", activeVideoE, runProc(replaceFootage(s, p))),
    cmd("layer.videoLayers.interpretFootage", "Interpret Footage…", vl, "", "{} → {}", activeVideoE,
        runProc(interpretFootage(s, p))),
    cmd("layer.videoLayers.reloadFrame", "Reload Frame", vl, "", "{} → {}", activeVideoE, runProc(reloadFrame(s))),
    cmd("file.import.videoFramesToLayers", "Video Frames to Layers…", ["File", "Import"], "", "{} → {}", videoDocE,
        runProc(framesToLayers(s, p))),
    cmd("file.export.renderVideo", "Render Video…", ["File", "Export"], "", "{} → {}", videoDocE, runProc(renderVideo(s, p)))]
