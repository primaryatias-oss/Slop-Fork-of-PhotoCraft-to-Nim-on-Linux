## Warp Text: bends a type layer's glyph outlines with one of Photoshop's fifteen warp styles (the
## PSD `warp` descriptor's `warpStyle`). The style math is shared with Edit › Transform › Warp
## (`geom/warp`); the renderer subdivides long segments so straight edges bend.

import std/[options, strutils]
import ../doc
import ../geom/warp as gw

const Styles* = [("warpArc", "arc"), ("warpArcLower", "arcLower"), ("warpArcUpper", "arcUpper"),
  ("warpArch", "arch"), ("warpBulge", "bulge"), ("warpShellLower", "shellLower"),
  ("warpShellUpper", "shellUpper"), ("warpFlag", "flag"), ("warpWave", "wave"), ("warpFish", "fish"),
  ("warpRise", "rise"), ("warpFisheye", "fisheye"), ("warpInflate", "inflate"), ("warpSqueeze", "squeeze"),
  ("warpTwist", "twist")]
  ## Warp styles in Photoshop's menu order: (PSD `warpStyle` value, short id used by commands).

proc psdStyle*(id: string): Option[string] =
  ## PSD style name (`warpArc`) for a short id (`arc`, case-insensitive) or a PSD name.
  let l = id.toLowerAscii
  if l == "none" or l == "warpnone": return some("warpNone")
  for (psd, short) in Styles:
    if psd.toLowerAscii == l or short.toLowerAscii == l: return some(psd)

proc shortStyle*(psd: string): Option[string] =
  ## Short id (`flag`, `none`) for a PSD style name: the reverse of `psdStyle`.
  if psd == "warpNone": return some("none")
  for (p, short) in Styles:
    if p == psd: return some(short)

type TextWarper* = object
  ## A warp prepared for one layout over the text box.
  w: StyleWarp

proc newTextWarper*(w: TextWarp; bounds: array[4, float32]): Option[TextWarper] =
  ## None for `warpNone`, unknown styles, an all-zero warp or empty bounds.
  let style = parseWarpStyle(w.style)
  if style.isNone or not style.get.isPreset or style.get.psdName != w.style: return
  let sw = newStyleWarp(style.get, float64(w.value), float64(w.horizontalDistortion),
                        float64(w.verticalDistortion), not w.horizontal,
                        [float64(bounds[0]), float64(bounds[1]), float64(bounds[2]), float64(bounds[3])])
  if sw.isSome: some(TextWarper(w: sw.get)) else: none(TextWarper)

proc maxSegment*(w: TextWarper): float64 = w.w.maxSegment
  ## Longest segment (text-space px) worth mapping unsplit.

proc apply*(w: TextWarper; x, y: float64): (float64, float64) = w.w.apply(x, y)
