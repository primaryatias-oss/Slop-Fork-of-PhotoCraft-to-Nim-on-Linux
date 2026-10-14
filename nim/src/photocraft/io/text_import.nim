## Type layers read from a file, made ready for the canvas and the panels (Rust `text_import.rs`).

import std/options
import ../color, ../raster, ../doc, ../text
import ../text/render as trender

const MaxRenderPixels* = 64'i64 * 1024 * 1024
  ## Largest type raster the import draws on its own (pixels). The model comes from an untrusted
  ## file: an absurd type size, scale or box must not make opening it allocate gigabytes. A layer
  ## over it keeps empty pixels until it is edited.

proc renderBounded(eng: TextEngine; t: var TextLayer; dpi: float32; format: PixelFormat) =
  ## Renders `t` from its model unless its ink would exceed `MaxRenderPixels`.
  let layout = eng.layout(t, dpi)
  let wp = layoutWarp(layout, t.warp)
  let ink = inkRectWarped(layout, t.transform, wp)
  if int64(ink.width) * int64(ink.height) > MaxRenderPixels:
    var f = format
    f.alpha = true
    t.cache = some(newSurface(f))
    return
  let r = rasterizeWarped(layout, t.transform, format, t.antialias, wp)
  t.cache = some(r.surface)

proc visit(layers: var seq[Layer]; eng: TextEngine; dpi: float32; format: PixelFormat) =
  for l in layers.mitems:
    case l.content.kind
    of lkText:
      var changed = false
      for r in l.content.text.runs.mitems:
        if r.style.postscriptName.isSome and not eng.fonts.hasFamily(r.style.fontFamily):
          let f = eng.fonts.resolvePostscript(r.style.postscriptName.get)
          if f.exact or eng.fonts.hasFamily(f.family):
            r.style.fontFamily = f.family
            r.style.weight = f.weight
            r.style.italic = f.italic
            changed = true
      if changed: l.content.text.syncSummary()
      if l.content.text.cache.isNone: renderBounded(eng, l.content.text, dpi, format)
    of lkGroup: visit(l.content.group.children, eng, dpi, format)
    else: discard

proc prepare*(d: var Document) =
  ## Prepares the type layers of a document that was just imported:
  ## - a run whose font came as a PostScript name (PSD files) gets the installed family that name
  ##   resolves to, as the layout does, so Missing Fonts and the Character panel see the real
  ##   family;
  ## - a type layer without pixels (ag-psd, GIMP and other writers leave the image data out and
  ##   let Photoshop re-render on open) is rendered from its model.
  visit(d.layers, text.shared(), d.resolutionDpi, d.pixelFormat)
