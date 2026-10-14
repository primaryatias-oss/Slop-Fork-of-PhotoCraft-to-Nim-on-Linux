## PhotoCraft's type engine (port of `photocraft_text`):
##
## * `text/fonts`: bundled fonts (Inter, JetBrains Mono; always available), optional system fonts
##   from a directory scan (no fontconfig), user font data, collections, PostScript-name lookup.
## * `text/layout`: shaping and line layout (`text/para`, the parley port, over `font/shape`),
##   point and paragraph (box) text, per-run styles, Photoshop leading/indent/spacing rules.
## * `text/render`: anti-aliased rasterization (exact-area accumulation, f32 coverage) into a
##   surface at any depth and colour model.
##
## The usual entry point is `TextEngine.renderLayer`, which refreshes a text layer's cache from
## its style model.

import std/[options, os, locks, unicode, math]
import geom, color, raster, doc
import text/[fonts, layout, render, warp, para, uprops, segment, bidi, optical, cjk, craft_fonts, served, raster as traster]

export fonts, layout, render, warp

type TextEngine* = ref object
  ## Font database + layout context. Create once and reuse.
  fonts*: FontDb
  layouter: Layouter

proc newTextEngine*(): TextEngine =
  ## Bundled fonts only: deterministic output (tests, headless).
  TextEngine(fonts: newFontDb())

proc newTextEngineWithSystemFonts*(): TextEngine =
  ## Bundled plus installed system fonts (desktop).
  TextEngine(fonts: newFontDbWithSystemFonts())

proc layout*(e: TextEngine; layer: TextLayer; dpi: float32): TextLayout =
  ## Lays out a text layer (text space: pixels, before `layer.transform`).
  e.layouter.layout(e.fonts, layer, dpi)

proc pairKerning*(e: TextEngine; layer: TextLayer; dpi: float32; at: int): Option[float32] =
  ## Kerning (1/1000 em) between the character starting at byte `at` and the next one, as laid
  ## out: the manual kern, or the automatic (metrics / optical) kerning of the pair. None when
  ## there is no such pair on one line.
  let runs = layer.charRuns
  if at < 0 or at >= layer.text.len: return
  let n = runeLenAt(layer.text, at)
  let ch = layer.text[at]
  let next = at + n
  if ch in {'\n', '\r'} or next >= layer.text.len or layer.text[next] in {'\n', '\r'}: return
  proc gap(l: TextLayout): Option[float32] =
    var a, b = -1
    for i, c in l.clusters:
      if c.range[0] == at and a < 0: a = i
      if c.range[0] == next and b < 0: b = i
    if a < 0 or b < 0: return
    if l.clusters[a].line == l.clusters[b].line and not l.clusters[a].rtl:
      some(l.clusters[b].x - l.clusters[a].x)
    else: none(float32)
  let with = gap(e.layout(layer, dpi))
  if with.isNone: return
  var acc = 0
  var styleRun = -1
  for i, r in runs:
    acc += r.len
    if at < acc:
      styleRun = i
      break
  if styleRun < 0: return
  let em = runs[styleRun].style.sizePt * (if dpi > 0: dpi / 72 else: 1'f32)
  if em.isNaN or em <= 0: return
  var plain = layer
  plain.runs = @[]
  var start = 0
  for r in runs:
    let stop = start + r.len
    proc push(s, e: int; unkern: bool) =
      if e > s:
        var style = r.style
        if unkern:
          style.kerning = kOff
          style.kern = 0
        plain.runs.add TextRun(len: e - s, style: style)
    push(start, clamp(at, start, stop), false)
    push(clamp(at, start, stop), clamp(next, start, stop), true)
    push(clamp(next, start, stop), stop, false)
    start = stop
  let without = gap(e.layout(plain, dpi))
  if without.isNone: return
  let k = (with.get - without.get) / em * 1000
  if k.classify in {fcNan, fcInf, fcNegInf}: none(float32) else: some(k)

proc render*(e: TextEngine; layer: TextLayer; dpi: float32; format: PixelFormat): (TextLayout, Rendered) =
  ## Lays out and rasterizes a text layer into document space.
  let l = e.layout(layer, dpi)
  let wp = layoutWarp(l, layer.warp)
  (l, rasterizeWarped(l, layer.transform, format, layer.antialias, wp))

proc renderLayer*(e: TextEngine; layer: var TextLayer; dpi: float32; format: PixelFormat): Rect =
  ## Re-renders `layer.cache` from its model. Returns the document-space rectangle drawn.
  let (_, r) = e.render(layer, dpi, format)
  layer.cache = some(r.surface)
  r.rect

var sharedEngine: TextEngine
var sharedLock: Lock
initLock(sharedLock)

proc usesSystemFonts(): bool = not existsEnv("PHOTOCRAFT_NO_SYSTEM_FONTS")

proc shared*(): TextEngine =
  ## A process-wide engine for callers without their own (bundled plus system fonts, unless
  ## `PHOTOCRAFT_NO_SYSTEM_FONTS` is set). The first call scans the installed fonts.
  withLock sharedLock:
    if sharedEngine.isNil:
      sharedEngine = if usesSystemFonts(): newTextEngineWithSystemFonts() else: newTextEngine()
    result = sharedEngine
