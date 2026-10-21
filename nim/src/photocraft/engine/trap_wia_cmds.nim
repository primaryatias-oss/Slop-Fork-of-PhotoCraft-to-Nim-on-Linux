## Image › Trap (`trap_cmds.rs`) and File › Import › WIA Support (`wia_cmds.rs`).

import std/[json, options, math]
import ./core
import ../algo/trap as atrap

proc trapE(s: Session): string =
  let d = s.active
  if d.isNil: return "no document"
  if d.doc.mode == cmCmyk: "" else: "Trap needs a CMYK document (Image › Mode › CMYK Color)"

proc trapCmd(s: var Session; p: JsonNode): JsonNode =
  ## Trapping is a CMYK prepress step: the active pixel layer's inks spread at colour edges over the
  ## canvas, in one history step.
  const cmdId = "image.trap"
  let wf = max(round(p.pget("width").asF64.get(1.0)), 0.0)
  let width = (if wf != wf: 0 elif wf >= float64(high(int)): high(int) else: int(wf))
  let d = s.active
  if d.isNil: raise noDocument()
  if d.doc.mode != cmCmyk: raise bad(cmdId, "Trap needs a CMYK document")
  s.edit("Trap", proc(doc: var Document; active: var Option[LayerId]) =
    if active.isNone: raise noDocument()
    let id = active.get
    let bounds = doc.bounds
    let l = doc.findLayer(id)
    if l.isNil: raise noLayer(id)
    if l[].content.kind != lkRaster:
      raise bad(cmdId, "Trap needs a pixel layer (active layer is " & l[].content.article & " " & l[].content.kindName & ")")
    if bounds.isEmpty: return
    let ch = l[].content.raster.channels
    var data = l[].content.raster.readRegion(bounds)
    atrap.trap(data, bounds.width, bounds.height, ch, width)
    l[].content.raster.writeRegion(bounds, data))
  %*{"trapped": true, "width": width}

proc trapSpecs*(): seq[CommandSpec] =
  @[cmd("image.trap", "Trap…", ["Image"], "", """{width:px>=1=1} → {trapped,width}: spread inks at colour edges (CMYK only)""",
        trapE, runProc(trapCmd(s, p)))]

proc wiaSpecs*(): seq[CommandSpec] =
  ## Windows Image Acquisition is a Windows-only API for scanners and cameras; off Windows the
  ## command reports that no device is available.
  @[cmd("file.import.wiaSupport", "WIA Support…", ["File", "Import"], "",
        """{} → {available, devices, note}: acquire from a scanner/camera (Windows only)""",
        enabledProc(""),
        runProc((when defined(windows): %*{"available": true, "devices": [], "note": "WIA device enumeration not yet wired"}
                 else: %*{"available": false, "devices": [], "note": "WIA scanner/camera import is available on Windows only"})),
        journal = false)]
