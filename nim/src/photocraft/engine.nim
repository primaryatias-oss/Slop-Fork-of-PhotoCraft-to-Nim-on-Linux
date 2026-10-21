## PhotoCraft's engine (port of `crates/engine`): the `Session` with its open documents and
## history, and the command registry every UI, script and agent drives it through. Commands are
## dispatched by id with JSON params (`Session.execute`).
##
## The modules that call into each other live in `engine/core` (one Nim module assembled from
## `engine/core/*.nim`); `engine/*.nim` holds the command modules nothing else calls. This
## module builds the registry in the order of Rust's `commands::build()`.

import engine/core
import engine/selection_cmds
export core, selection_cmds

proc build(): seq[CommandSpec] =
  ## `commands::build()`: the base table, then every module's `specs()` in Rust's order. Modules
  ## not ported yet are listed as comments in their place.
  result = baseSpecs()
  result.add layerStyleSpecs()
  result.add filterSpecs()
  result.add filtersExtSpecs()
  result.add colorToAlphaSpecs()
  result.add gallerySpecs()
  # gradient_fill_cmds,
  # solid_fill_cmds, pattern_fill_cmds
  result.add typeSpecs()
  # type_mask_cmds
  result.add transformSpecs()
  result.add floatSpecs()
  result.add vectorSpecs()
  # path_edit_cmds, smartselect_cmds, cutout_cmds, symmetry_cmds
  result.add editSpecs()
  result.add colorSpecs()
  # brush_cmds, brush_preset_cmds, eraser_cmds, preset_import_cmds, swatch_cmds,
  # retouch_cmds, redeye_cmds
  result.add imageSpecs()
  # perspective_crop_cmds
  result.add selectionSpecs()
  # magnetic_cmds,
  # select_extra_cmds, paint_cmds
  result.add extraSpecs()
  result.add backgroundSpecs()
  # file_cmds, exr_cmds, type_extra_cmds, type_styles_cmds, type_spell_cmds, type_caret_cmds,
  result.add smartSpecs()
  result.add layerMultiSpecs()
  result.add layerCopySpecs()
  # prefs, edit_menu_cmds, fill_key_cmds
  result.add sampleSpecs()
  # sampler_cmds,
  # brush_key_cmds, stamp_cmds, align_cmds, photo_cmds, lens_cmds, vp_cmds
  result.add channelSpecs()
  result.add adjustSpecs()
  # layer_menu_cmds, layer_label_cmds
  result.add modeSpecs()
  # multichannel_cmds
  result.add patternSpecs()
  result.add warpSpecs()
  # comps_cmds, artboard_cmds, distort_cmds, analysis_cmds, notes_cmds, history_cmds,
  # proof_sim, presets, document_preset_cmds, render_cmds, slice_cmds, web_cmds, automate_cmds,
  # print_cmds, installed_profiles, pick_cmds, layer_nav_cmds, frame_cmds, migrate_cmds,
  # trap_cmds, timeline_cmds, video_cmds, jobs, wia_cmds, rasterize_style_cmds, variables_cmds,
  # plugin_cmds, group_view_cmds, fx_view_cmds, fx_visibility_cmds
  result.add maskViewSpecs()
  # layer_mask_props_cmds, actions_cmds

installRegistry(build())
