## The engine core (port of `crates/engine`'s strongly connected part): the session, command
## dispatch and every command module that the session or another such module calls back into.
## Rust's modules here call each other freely, so they form one Nim module assembled with
## `include` (one file per Rust module, under `core/`); `core/decls.nim` forward-declares what a
## file uses before its defining file. The command modules nothing in here calls are separate
## modules importing this one (`engine/*.nim`), and `engine.nim` builds the registry.
##
## Rust `Result` errors are `EngineError` exceptions; `serde_json::Value` is `JsonNode`, read with
## the helpers in `core/session.nim` that mirror `as_u64`, `as_f64`, `as_str`, ….

import std/[json, options, tables, sets, strutils, math, algorithm, sequtils, unicode, strformat, hashes, os]
import ../geom, ../color, ../raster, ../doc, ../ops, ../compose, ../vector, ../paint, ../rmath, ../text
import ../algo/[selection, selection_marquee, resample, params, tone]
import ../algo/content_aware as caware
import ../algo/transform as atransform
import ../text/psd_text
import ../format, ../cms, ../io/pattern_map, ../codecs/meta, ../psd/swatches, ../rfmt
import ../doc/types as doctypes
import ../paint/brush as paintbrush
when not defined(js): import std/posix

type
  DocPattern* = doctypes.Pattern     ## a document / library pattern (`photocraft_doc::Pattern`)
  BrushPattern* = paintbrush.Pattern ## a brush texture pattern (`photocraft_paint::Pattern`)

export json, options, geom, color, raster, doc, ops

include core/[types, session, decls]
include core/[fsutil, pixels, canvas_geom, clip_group, commands, mask_view_cmds, channel_cmds, layer_multi_cmds,
              new_layer, background_cmds, inspect, pattern_cmds, fill_cmds, prefs_units, color_cmds, channel_clip,
              layer_copy_cmds, edit_cmds, hidden_target, extra_cmds, layer_style, sample_cmds, adjust_params, adjust_cmds]
include core/pending
