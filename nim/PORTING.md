# PhotoCraft → Nim port: guide and status

This directory is a port of the Rust workspace (`../crates`, `../apps`) to **Nim 2.2 using only
the Nim standard library and the official SDL3 binding** (`nim-lang/sdl3`, vendored in
`vendor/sdl3`). The Rust sources stay in the repository as the reference: every Nim module names
the Rust file it ports, keeps its behaviour, and ports its tests.

## Toolchain

```sh
export PATH=/opt/src/nim/bin:$PATH      # Nim 2.2.x
cd nim
nim r --hints:off -d:release tests/t_compose.nim   # one suite
nim r --hints:off -d:release tests/runall.nim      # everything
```

`config.nims` puts `src/` and `vendor/sdl3` on the path, uses ORC, and gives every main module its
own nimcache (`.nimcache/<name>`), so different test files can compile at the same time.

## Layout

| Rust crate | Nim module(s) | Notes |
|---|---|---|
| geom | `photocraft/geom`, `geom/warp` | |
| color | `photocraft/color`, `color/{blend,convert}` | |
| cms | `photocraft/cms`, `cms/*` | ICC parsing, transforms, built-in profiles |
| raster | `photocraft/raster` | COW tiles (`Tile = ref object`), `Surface` |
| (flate2/miniz) | `photocraft/zlib` | inflate/deflate, zlib + gzip framing |
| codecs | `photocraft/codecs`, `codecs/*` | PNG, JPEG, TIFF, WebP (VP8/VP8L), EXR, BMP/TGA/GIF/… |
| psd | `photocraft/psd`, `psd/*` | byte-stable PSD/PSB read/write, descriptors, presets |
| doc | `photocraft/doc`, `doc/types` | the document model (pure data) |
| ops | `photocraft/ops` | history |
| vector | `photocraft/vector`, `vector/*` | paths, exact-area rasterizer, strokes, shapes, editing |
| compose | `photocraft/compose`, `compose/*` | the CPU compositor (reference renderer) |
| algo | `photocraft/algo`, `algo/*` | filters and imaging algorithms |
| paint | `photocraft/paint`, `paint/*` | the brush engine; `retouch::Region` is `PxRegion` |
| raw | `photocraft/raw`, `raw/*` | camera raw decode/develop; `Rect` is `RawRect`, `Limits` is `RawLimits` |
| affinity | `photocraft/affinity`, `affinity/*` | Affinity reader; model types carry an `Af` prefix (`AfDocument`, `AfNode`, `AfPaint`…) |
| (serde/serde_json) | `photocraft/serde` | generic `toJ`/`fromJ` with serde naming |

## Conventions

* **Never crash.** No `assert`/`doAssert` on input, no unchecked indexing with input-derived
  numbers; raise the module's error type (`PsdError`, `CodecError`, `SerdeError`, `ValueError`)
  with an actionable message. Bound recursion and allocations derived from input.
* **Rust enums with data** become *flat* Nim objects: a `kind` enum field plus every variant's
  fields (`Adjustment`, `Effect`, `Fill`, `LayerContent`, `FilterParams`). Fields of different
  variants that share a name but not a type get a prefix in Nim and keep their serde name in JSON
  (see `algo/params.nim` `jsonName`). Structural `==` then works on the whole object.
* **Fieldless enums** carry their serde string: `bmMultiply = "Multiply"`, `rmSpin = "spin"`.
* **JSON** goes through `serde.nim`: `toJ(x)` / `fromJ(node, x)`; field names are snake_case by
  default, `proc serdeCamel*(_: typedesc[T]): bool = true` opts a type into camelCase, missing
  fields keep their defaults, `proc defaultOf*(_: typedesc[T]): T` supplies element defaults.
* **Shared bytes** (`Arc<Vec<u8>>`) are `SharedBytes = ref object data: seq[byte]` with content `==`.
* **Values, not references.** Documents, layers and surfaces are value objects; surfaces share
  their tiles (refs) so copies are cheap and copy-on-write (`isUniqueRef`).
* **Single-threaded.** rayon's parallel loops become plain loops; helpers like `par_rows`
  disappear. Caches become `{.threadvar.}` tables.
* **Naming.** Rust `snake_case` → Nim `camelCase`; types keep their names. Methods become procs
  with the receiver first (`img.get(x, y, c)`). Constructors are `newX`/`xFrom…` procs.
* **Tests** port the Rust `#[cfg(test)]` cases that matter into `tests/t_<area>.nim` (std
  `unittest`), and are listed in `tests/runall.nim`.

## Nim pitfalls met so far

* `f32 * 12.92` with an unsuffixed literal is computed in **float64** in Nim (the literal is a
  float64, so the float32 operand is widened) and narrowed afterwards; Rust does it in f32. Suffix
  literals that meet float32 values (`12.92'f32`), also in `max(x, 1e-6'f32)` and `1'f32 / 3`
  (`1 / 3` is a float64 division). `tools/floatdiag.sh` lists every such site.
* `f32::to_radians` equals `degToRad` (one product by the rounded constant), but `to_degrees` is a
  product by `180/π` where Nim's `radToDeg` divides: use `rmath.toDegrees`.
* Nim folds arithmetic on `float32` constants in float64 (a `const X = float32(PI / 4)` keeps
  the unrounded value, and `1 - X` is folded before rounding), unlike Rust's f32 constant
  folding. Keep float32 constants that take part in arithmetic as module-level `let`s.
* Rust's `f32::cbrt`/`f64::cbrt` are not glibc's (they come from Rust's bundled `libm`); use
  `photocraft/rmath.cbrt`. Every other libm function the port uses matches glibc bit for bit
  (checked on 200k inputs each). Rust's `powi(n)` at run time is repeated multiplication
  (`x * x * x`; `powi(4)` squares twice, `(x²)²`), but a constant-folded `powi` equals `pow`.
* Rust's float `Iterator::sum` starts at `-0.0` (an empty or all-`-0.0` sum is `-0.0`); start
  ported sums at `-0.0` so zero signs match. Explicit `fold(0.0, …)` starts at `+0.0`.
* Rust's `binary_search_by` returns a specific one of several equal elements (its branch-free
  loop); `util.binarySearchBy` reproduces it (checked against rustc on 3000 arrays with ties).
* `high(float32)`/`low(float32)` are ±∞ in Nim, not Rust's `f32::MAX`/`f32::MIN`: use
  `rmath.F32Max` (blend modes at `f32::MAX` stay finite only with it).
* A `float64` argument passed to a `float32` parameter is narrowed silently (no error): give
  helpers both overloads (`util.totalCmp` has them) or make them generic.
* Rust `as` casts from floats saturate and map NaN to 0 (`rmath.asIdx`/`asU8`/`asU16`/`asU32`,
  `geom.asI32`/`asUsize`); a bare `int(NaN)` is undefined. `max_by`/`max_by_key` keep the last
  of equal maxima, `min_by` the first.

* Closures cannot capture `openArray`/`var` parameters. Use a `template`, a top-level `proc` that
  takes the data as a parameter, or copy (`let vals = @values`). `nim check` does not catch this;
  only a real compile (`nim r`/`nim c`) does.
* Template parameters are substituted everywhere in the body, including field names: a template
  parameter called `at`, `x0` or `rect` will rewrite `r.at`/`r.x0`. Pick unique parameter names.
* An array literal assigned to the variable it reads is built in place: `q = [q[0], q[3], q[2],
  q[1]]` gives `[q0, q3, q2, q3]`. Permute through a temporary (`let r = [...]; q = r`); tuples,
  objects and seqs are not affected.
* Identifiers are style-insensitive except the first letter: `im` and `iM` are the same.
* serde parity for settings structs: missing fields default, unknown fields are ignored, a struct
  may also be written as a sequence of its fields (in declaration order). `std/json` keeps the last
  of duplicate keys where serde_json rejects the object; that leniency is the one known difference.
* Nim 2.2.8 bug: at module top level (which includes `unittest` `test` bodies), reassigning the
  rows of a seq-of-seqs through `mitems` (`for row in cap.mitems: row = newSeq[float64](n)`)
  inside a loop corrupts the heap and segfaults; inside a proc it is fine. Use an index loop.
* Tests that need a module's private procs `include` it (`include photocraft/algo/inpaint`)
  rather than exporting test hooks.
* A nested proc that captures an `openArray` (or `var`) parameter is only rejected when its
  enclosing proc is actually used, so importing a module compiles cleanly until a caller appears.
  Copy the parameter (`let feats = @feats`) or pass it to a top-level helper (a template wrapper
  keeps call sites short); compile new modules through a caller (the algocheck twin or a test).
* Statement lists inside parenthesised expressions (`(let a = ..; if ..)`) are fragile; use a
  template or a helper proc.
* `float32` inference: `0.5 * arctan2(a, b)` is `float` (64-bit) when the arguments are; annotate
  (`let angle: float32 = ...`) or convert. `degToRad` on a `float32` literal may yield `float`.
* An object field shadows a proc of the same name in method-call syntax (`l.name` finds the field).
* `let x = s[i]` copies objects that own `seq`s; index in place or pass as a parameter (non-`var`
  object parameters are passed by reference).
* Paint: the Rust paint API trusts its caller to bound stroke coordinates (the engine rejects
  points beyond ±1e6). The Nim `PathWalker` additionally skips distance spacing on a segment
  longer than 1e7 px (a non-finite or absurd point), where the Rust loop never finishes.
* `photocraft/paint` exports `Pose` (Brush Pose) and `algo/direct` exports `Pose` (registration):
  a module importing both qualifies them (`direct.Pose`).
* Result aliasing: `x = f(x.get)` (an argument read *through* the destination, here via `lent`
  `Option.get`) can hand `f` the destination as its result buffer; `f` zeroes `result` first and
  then reads a zeroed argument. Read into a temporary first (`let n = x.get; x = f(n)`). The raw
  white balance lost the as-shot neutral this way until the cross-check caught it.
* A nested proc that captures a non-`var` object parameter compiles, but copies the parameter
  into the closure (a whole file buffer for `Tiff`): use a template or a top-level helper.

## Status

See the bottom of this file (updated as crates land) and `git log`.

| Area | State |
|---|---|
| geom, color, cms, raster, zlib | ported, tested |
| codecs (PNG, JPEG, TIFF, WebP, EXR, BMP, TGA, GIF, …) | ported, tested against Pillow/OpenEXR fixtures |
| psd | ported; byte-stable on the psd-tools corpus (125 files), including the little-endian (Intel-order) layered-TIFF transcoder |
| doc, ops, vector | ported, tested |
| compose | ported, tested (GPU compositor intentionally not ported: SDL3 textures display the CPU result) |
| format (.pcraft, zstd, BLAKE3, autosave/recovery) | ported, tested; files interchange with the Rust build both ways |
| algo | ported, tested: foundation, resample, transform, warp; filter dispatcher (`algo/filters`: halos, tiled apply, selection mixing, registry); blur family (`blur`, `sharpen`, `blur2`, `gallery`, `radial_plan`: Gaussian/Box with the separable fast path, Motion with the FFT fast path, Radial, Surface, Unsharp Mask, Smart Sharpen, High Pass, Smart/Lens/Shape Blur, Tilt-Shift, Iris, Field, Spin, Path); noise, median, custom, HSB/HSL, color to alpha, video, stylize part 2; Other, Stylize, Distort, Pixelate, Oil Paint, Render (clouds, fibers, lens flare, lighting), Relight, Reduce Noise, Filter Gallery (`artistic_fx`, all 47 effects); selection (magic wand/regions, colour and Lab
ranges, modify incl. the cosine-series feather, polygon, marquee tiles, combines), histogram, tone
(Shadows/Highlights, Replace/Match Color, HDR Toning), HDR merge (exposures, MTB, Debevec–Malik
response, ghosts, tone mapping), EXIF, Indexed Color/Bitmap (`quantize`), Magic Eraser, stack modes,
Trap, gradient/bucket tools, retouch kernels (dodge, burn, sponge, local blur/sharpen), Poisson
blending (`poisson`), non-local patch completion (`nonlocal`), the Remove Tool (`remove`, `holes`),
Perspective Warp, Crop and Straighten Photos (`scancrop`), Red Eye, Content-Aware Scale (`seam`),
Magnetic Lasso, Liquify, Render › Flame / Picture Frame / Tree (`render2`), Auto-Blend pyramids and
focus-stack fusion (`pyramid`), Lens Correction (`lens`, including `remap` in every colour mode),
Vanishing Point, Adaptive Wide Angle (`wideangle`), Puppet Warp, Photomerge / Auto-Align (Harris +
steered BRIEF `features`, `panorama` matching, bundle adjustment, photometric, seams, multi-band;
`direct` intensity registration), the Camera Raw Filter (`camera_raw`), hole filling (`inpaint`:
PatchMatch/EM completion with onion-peel init and colour adaptation, proximity match, texture
synthesis), smart selection (`segbase`, `maxflow` (Boykov–Kolmogorov + push/relabel), `gmm`,
`segment` graph cuts, `grabcut`, `slic`, `quick`, `sky`, `subject`, `focus`), Select and Mask
(`matting`: guided filters, refine edge brush, octagonal morphology, decontamination) and
Content-Aware Fill (`content_aware`). Non-filter APIs are checked with `tools/algocheck.py`
(`tools/algocases/*.json` other than `paint.json`, 3625 command runs identical). 2869 of 2870 filter runs identical to Rust (`tools/filtercases.json`, 410 parameter sets × 7 documents); the other differs by ≤1.5e-8 (FFT motion blur, f32) |
| io (PSD/PSB ↔ document, layered TIFF, multi-part EXR, flat formats, facade) | ported, tested; see "io notes" below |
| io: OpenRaster, Paint.NET | ported, tested |
| io: Photoshop brushes (`.abr` v1/v2/v6+ → brush presets, `io/abr_map`) | ported, tested |
| io: camera raw import (`io/raw_import`: develop to 16-bit ProPhoto, colour fitted to the embedded JPEG, open-time tuning, preview fallback) and Camera Raw smart filters in PSD (`io/camera_raw_map`) | ported, tested; imports of the 62 oracle raw files give identical documents and notes, except two whose stub preview JPEG fails with the JPEG decoders' different messages |
| io: Affinity documents (`io/affinity_import`: spreads side by side, pages and artboards, groups, shape layers with even-odd compounds, gradients and strokes, type layers, pixel layers, placed images as embedded smart objects, layer and vector masks; the embedded preview as a warned fallback) | ported, tested; imports identical to Rust (manifest JSON with tile hashes, warnings, errors) on 394 generated documents, 4481 mutated ones and 3000 preview-fallback files (`tools/importcheck.nim`) |
| io: SVG | pending (the facade reports "unsupported" until it lands; it needs a usvg-style normalizer) |
| text: font reading (`font/sfnt`: TrueType/CFF outlines, cmap, metrics, names) | ported; outlines, metrics and charmaps identical to skrifa on the bundled and system fonts |
| text: shaping (`font/otl`, `font/shape`: HarfBuzz default, Hebrew, Arabic, Thai/Lao, Hangul shapers) | ported; glyphs, clusters and positions identical to HarfRust on 324 test strings across 17 fonts. Indic, USE, Khmer and Myanmar shapers pending (those scripts shape with the default shaper) |
| text: Unicode data (`text/ucd_data` from Perl's UCD 15.0 via `tools/ucdgen.nim`), line breaking (UAX #14), grapheme clusters (UAX #29) | ported (no dictionary breaking for Thai/Lao/Khmer/Burmese) |
| text: font database (`text/fonts`: bundled + system fonts, fontique-style matching and synthesis, PostScript names), bidi (UAX #9), paragraph layout (`text/para`, the parley port), type layout (`text/layout`: paragraphs, box text, tabs, small caps, optical kerning, decorations, vertical type) | ported; layouts identical to the Rust text engine on 29 layers (bundled and system fonts: Latin, Hebrew/Arabic RTL, mixed bidi, CJK, Thai, vertical Japanese) |
| text: rendering (`text/raster`, `text/render`, `text/warp`; skrifa-exact 26.6/16.16 outline scaling in `font/sfnt`), `TextEngine` (`text.nim`) | ported; rasterized text bit-identical to the Rust engine on 31 layers (warps, transforms, faux bold/italic, colours, aliasing, vertical, Arabic) |
| text: PSD type (`text/engine_data`: EngineData parse/write; `text/psd_text`: `TySh` ⇄ `TextLayer`, Photoshop's kerning pair fields, `Txt2` auto-kern modes; `text/psd_styles`: named style sheets), io wiring (`io/text_import`, `io/text_styles_map`, `TySh`/`Txt2` regeneration on PSD export) | ported; models, rebuilt `TySh`, EngineData and `Txt2` bytes identical to Rust on every type layer of the corpus at 0/72/96.5/300 dpi, and on generated layers (kerning, Optical, box, vertical, warp, named styles) |
| text: spelling (`text/spell`, SCOWL dictionary), UI fallback scripts (`text/scripts`), Glyphs panel (`text/glyphs`) | ported; checks, suggestions, charmaps, category counts, previews and candidate lists identical to Rust |
| paint (brush model and serde, dynamics, stroke renderer, procedural tips/patterns, presets, Mixer Brush, Color Replacement, Background Eraser, retouch dab machinery and Smudge, cursor outlines, symmetry) | ported, tested; dabs, rendered strokes at every depth/model, coverage, tool effects, outlines, symmetry and the preset JSON identical to Rust (`tools/algocases/paint.json`, 1140 command runs) |
| raw (TIFF/EP reader, DNG incl. deflate / lossless-JPEG / lossy-JPEG tiles, opcodes and default crops, CR2, Nikon Huffman NEF, Sony cRAW, Panasonic RW2, Olympus ORF, Fujifilm RAF incl. X-Trans, embedded previews, camera profiles, demosaicing (bilinear, Malvar–He–Cutler, AHD, X-Trans), develop, the camera-look fit; testgen writers) | ported, tested; decoded sensors, developed images, previews, TIFF dumps and camera-look fits identical to Rust on 1535 runs over 75 oracle-written files, truncated and bit-flipped variants included (`tools/algocases/raw.json`, `tools/rawcheck.nim`); the Nim testgen writes byte-identical files. See "raw notes" |
| affinity (archive with zlib/zstd entries and byte predictors, the tagged object stream, the document model: spreads, pages, artboards, groups, curves with live corners, every parametric shape, compounds, fills, gradients, strokes, artistic and frame text, pixel layers in every tile format, placed and embedded images, masks; the indexed preview; synth builders) | ported, tested; model dumps identical to Rust on about 30,000 generated documents (every node kind, colour model, shape and tile state, plus stream, container and compressed-payload mutations) and on 3000 mutated previews (`tools/afgen.nim`, `tools/afcheck.nim`, `tools/afcompare.py`). See "affinity notes" |
| engine (`engine/core/*`: Session, dispatch and 606 of the 884 commands: layers, selections (Sky, Transform Selection, Isolate Layers included), fill, channels, edit, colour, styles, adjustments, filters (Render › Flame, Picture Frame, Tree included), Image menu, transforms, warps, vector shapes and paths and their point-level edits, floating selections, smart objects and smart filters, type layers, Paint Bucket, Gradient tool, brush, pencil, Mixer Brush, Color Replacement, brush presets and their groups and folders, painting symmetry, Magic and Background Eraser, the retouching tools (Clone and Pattern Stamp, Healing, Spot Healing, Patch, Content-Aware Move, Remove, Dodge, Burn, Sponge, Blur, Sharpen, Smudge, History Brush), Red Eye, Perspective Crop, Trap, layer colour labels, mask density and feather, group and effects disclosure, effects visibility, Rasterize › Layer Style, Multichannel, Magnetic Lasso, Solid Color and Pattern Fill editing, Type Mask, installed profiles, Migrate Presets, layer navigation keys, brush and fill keys, Stamp Visible and Stamp Down, Notes, Color Samplers, frames, Auto-Select, History panel snapshots and actions, the Gradients, Patterns, Styles, Shapes, Tool Presets and Clone Source panels, document presets and File › New › Recent, the video Timeline, Video Layers and Render Video to image sequences and animated GIF) | in progress; step results, errors and `.pcraft` documents identical to Rust on thousands of generated scripts (`tools/enginecases.py`, `tools/enginecheck.nim`). See "engine notes" |
| UI, CLI | not started |
| heif (HEVC decoding), plugins (WebAssembly runtime) | not ported: they need a video decoder / wasm interpreter; the commands report "unsupported" |

## Checking against Rust

`tools/oracle` is a small standalone Cargo project that runs the Rust crates as the reference:
`cargo build --release --manifest-path tools/oracle/Cargo.toml`, then
`oracle write|read|render|import|export|conv|filter …` (see `src/main.rs`). The port is compared
by document JSON + tile hashes (`.pcraft`), raw float renders, or output bytes.

## raw notes

* `tools/rawcheck.nim` is the Nim twin of the oracle's `ax_raw.rs`; `photocraft-oracle raw.gen
  DIR -` writes the shared files (algocheck.py runs it for `%name` arguments), and
  `tools/rawcases.py DIR` regenerates `tools/algocases/raw.json`. Run the cases with
  `ALGOCHECK=<rawcheck binary> python3 tools/algocheck.py tools/algocases/raw.json`.
* JPEG XL-compressed DNG tiles (DNG 1.7, Samsung Expert RAW) are not decoded: the Rust build uses
  jxl-oxide; the port reports them as unsupported (like the Rust browser build).
* Lossy-JPEG DNG tiles go through the port's JPEG decoder where Rust uses zune-jpeg: gray tiles
  decode identically, colour tiles differ by up to 2 levels in about 1.4 % of samples (YCbCr
  conversion), and corrupt tiles fail with different messages.
* Rayon bands become plain loops; every band-structured sum (grey world, the look fit's
  least-squares chunks) keeps Rust's partial-sum order, so results stay bit-identical.
* The colour fit to a camera's embedded preview decodes that JPEG with the port's decoder, so a
  fit from a real colour preview can differ slightly from Rust's (see the lossy-tile note).

## affinity notes

* `tools/afgen.nim DIR [MUTATIONS [SEED]]` writes synthetic documents (and stream-mutated copies);
  `photocraft-oracle af.read FILE…` and `tools/afcheck.nim read FILE…` print the same canonical
  dump (floats as bit patterns); `python3 -I tools/afcompare.py ORACLE AFCHECK DIR [read|preview]`
  compares them. The public Affinity corpus (`cargo xtask corpus --affinity`) could not be fetched
  here, so the real-file tests (`tests/real_files.rs`) are not ported yet.
* Compressed entries are read with the decoders' own streaming behaviour, because the reader's
  "corrupt compressed entry" / "archive entry size mismatch" distinction depends on it:
  `zlib.zlibReadBounded` follows flate2's `ZlibDecoder` over miniz (32 KiB look-ahead, Adler-32
  checked when the stream ends inside it, zero-initialised window for distances before the start,
  zlib's table checks, "incomplete deflate stream" when it ends early) and `zstd.zstdReadFrame`
  follows `ruzstd::StreamingDecoder` (whole blocks until the bytes beyond the window cover the
  request, checksum read but not verified, skippable frames rejected, reserved sequence bits
  ignored, block limits counted on sequences only).
* The Rust reader panics or never returns on some hostile documents; the port returns instead and
  these files are left out of the comparison: `f64::clamp` with min > max in the rectangle callout
  (`ShCR` with an inverted box: Rust panics; the port clamps to the low bound), a compound listing
  itself among its operands and a spot colour whose `srgb`/`base` refers back to it (Rust
  overflows its stack; the port stops at 384 and 64 levels), and a pie whose end angle is so far
  below its start that adding 2π no longer changes it (Rust loops forever; the port keeps the
  value). Pies with large but finite gaps (Rust's `while end < start { end += TAU }` running
  hundreds of millions of times) are fast-forwarded per binade with the same rounding, so the
  results stay bit-identical.
* `tools/importcheck.nim ORACLE FILE…` compares whole imports (`io.importDoc` against the oracle's
  `import`): error or warnings, then the saved `.pcraft` manifests with fresh ids left out, which
  covers the document model and every tile by hash. It found two engine-level differences, both
  fixed: box text measured its first baseline from the unscaled bounds of 'd' where skrifa bounds
  CFF and variable glyphs from the scaled outline (on the 26.6 grid), and codec errors named the
  format by its display name (`PNG:`) where Rust prints the variant (`Png:`). The PNG decoder now
  also checks chunk lengths up front and words inflate failures as the png crate does.

## Filter notes

* `tools/filtercheck.py [--docs …] [--tol L] <params json | cases.json>…` runs `FilterParams`
  through the Rust oracle (`oracle filter`) and the port (`tools/filtercheck.nim`) on seven
  generated documents (RGB 8/16/32-bit, gray, CMYK, Lab 16-bit, no alpha) and compares pixels.
  `tools/filtercases.json` is the regression list of verified parameter sets.
* Filter modules register with `algo/filters` (`registerKernel`, `registerHalo`, `registerPlan`,
  `registerFastPath`); `photocraft/algo` imports them all.
* Halo radii use Rust's release-build i32 arithmetic (wrapping), so absurd radii give the same
  (empty) areas as Rust instead of huge allocations.
* Box radii (`fxutil.gaussBoxRadii`, Filter Gallery line blurs and Kuwahara) are capped at
  `MaxBoxRadius` (65536): Rust loops for hours on such radii (only reachable through hand-written
  params); below the cap results are Rust's. A tile whose source window would exceed 2^28 samples
  raises `FilterTooLarge` where Rust runs out of memory.
* Float → index casts on pixel values use `rmath.asIdx`/`asU8`/`asU16` (Rust `as`: NaN → 0,
  saturating); a bare `int(NaN)` in a CLUT lookup overflowed on NaN colours.
* Motion Blur's FFT path is taken under the same conditions as Rust (including the core count
  from `countProcessors`, rayon's default); its f64 FFT is the port's own, so 32-bit float
  results can differ in the last bits.

* `tools/algocheck.py [--tol X] cases.json…` runs non-filter algo APIs through the oracle's
  `ax_*.rs` commands and `tools/algocheck.nim` on shared raw buffers (`PCIM` files written by
  `algocheck gen`) and compares stdout and output files byte for byte. New groups add a command
  pair (Rust in `tools/oracle/src/ax_<group>.rs`, Nim in `tools/algocheck.nim`) and a cases file.
* `tools/floatdiag.sh [program.nim…]` builds an instrumented Nim compiler that lists every
  implicit float32 ↔ float64 conversion in `src/photocraft`. Comparisons with exactly
  representable literals, `classify` and ±∞ tests are harmless; arithmetic sites are bugs (Rust
  computes them in f32). Run it after porting a module.
* Where the Rust crate panics (or wraps in release builds) on input the port must survive, the
  Nim code saturates instead, and the case stays out of the cross-check: Liquify's `dab` with a
  valid stroke to x = 1e200 (the dab margin overflows `usize`) is one; `t_algo_liquify` keeps it
  as a no-crash test. Lens Correction's `auto_scale` loops forever on a zero or negative scale;
  the port starts that search from 1 %.

## Text notes

* `tools/oracle shape <font> <text> [features] [script] [lang] [rtl]` shapes with HarfRust; the
  port's `font/shape.nim` reproduces it glyph for glyph (ids, clusters, advances, offsets) for the
  shapers listed above, including normalization against the cmap, ligature ids and mark
  attachment, cursive attachment, space fallbacks, default-ignorable hiding and fallback mark
  positioning for fonts without GPOS.
* `tools/oracle layout <layer.json> [dpi]` (set `SYSTEM_FONTS=1` to include installed fonts) dumps
  the Rust engine's layout (faces, glyphs, lines, clusters, decorations) for comparison with the
  port's `text/layout.nim`.
* `tools/oracle tysh <file.psd> <dpi> <outdir>` prints each type layer's model as read from
  `TySh` (+ `Txt2`) and writes the rebuilt `TySh` (fresh and patched), its EngineData, a moved
  and a renumbered copy, and the regenerated `Txt2` blocks, for byte comparison with the port.
* `tools/oracle text-render <layer.json> <out.f32> [dpi]` writes the Rust engine's rendered RGBA
  floats; `font/sfnt.scaledOutline` reproduces skrifa's FreeType-compatible fixed-point scaling, so
  coverage matches bit for bit.
* Variable fonts: shaping, advances and outlines use the default instance (no `gvar`/`HVAR`/`MVAR`);
  normalized coordinates are still computed for face identity.
* Unicode character data comes from the Unicode 15.0 UCD as packaged with Perl
  (`/usr/share/perl/*/unicore`); regenerate `src/photocraft/text/ucd_data.nim` with
  `nim r tools/ucdgen.nim`.

## io notes

Checked against the Rust build on the psd-tools corpus with a Rust oracle (`import`, `conv`):

* PSD → document: identical documents (manifest JSON and tile hashes) on all 309 imports,
  type layers included (typed model, `Txt2` kerning modes, named styles, layers rendered from
  their model when the file has no pixels).
* PSD → PSD: every layer record, tagged block (`TySh`, `Txt2` and the rest), global block and
  resource identical on all 308 files Rust converts, except the 1036 JPEG preview (independent JPEG
  encoder; close but not equal pixels). The merged (composited) image is identical on all 308
  (the last 1–2 level differences were float64 arithmetic leaking into f32 code, found with
  `tools/floatdiag.sh`). 16/32-bit ZIP streams can differ in bytes (the port's own deflate vs
  miniz_oxide) with identical decoded content.
* Flat exports: PNG and TIFF decode to identical pixels; JPEG and WebP (lossy) differ (encoders are
  independent implementations).
  TGA (run-length encoded, as the image crate writes it) and GIF (Render Video) are byte-identical.
* Paths with a NUL byte: Nim passes paths to C as NUL-terminated strings, so every file access on
  a caller-supplied path refuses one first with Rust's message ("file name contained an unexpected
  NUL byte"), instead of reading or creating a truncated path (`tests/t_engine_paths.nim`).
* Layered TIFF: identical layer data (tag 37724) and resources (34377) except text and ZIP cases.
* Rust's serde_json reads floats from `.pcraft` JSON up to 1 ULP off; the port reads them exactly,
  so a vector mask that Rust regenerates after a `.pcraft` round trip is kept verbatim here.
* Flat enum types (`Effect`, `Fill`, `Adjustment`, …) compare only their active variant (as Rust's
  derived `PartialEq` does), through their serde form.

## engine notes

`tools/enginecases.py OUTDIR N SEED [AREAS]` writes random command scripts (valid and hostile
params); `enginecheck ORACLE OUTDIR/*.json` runs each in a fresh Rust process and a fresh Nim
session and compares every step's result or error and every open document's `.pcraft` manifest
(tiles by hash; embedded smart-object sources by their nested manifest, since the two builds'
bundle bytes differ). `enginecheck --specs ORACLE` compares the registry.

* serde_json maps are `BTreeMap`s: iterate params in sorted key order (`sortedPairs`) wherever the
  order shows (first error reported, merged objects).
* `Layer::surface_mut` is raster-only (`surfaceMut`); caches of type, shapes and smart objects are
  never edited in place.
* Ids come from one process-wide counter (layers, documents, smart contents): every id the Rust
  code mints must be minted here too, in the same order. The `.pcraft` loader used to burn one per
  layer through `newLayer`; a decode also reserves one (`reserveIdsThrough`'s probe), so a smart
  object source decoded from the cache mints fewer ids than a cold decode. `enginecheck` clears the
  smart-object caches per script (`clearSourceCache`, `purgeStackCache`) to match a fresh process.
* `geom::Warp` serializes camelCase (`hDistort`), unlike `TextWarp` (snake_case): types opt in with
  `serdeCamel`.
* Process-wide state outlives a script in the checker's one process: besides the smart-object
  caches it resets Color Settings' text gamma (`setTextGamma`), which Rust also keeps per process.
* A face without outlines (a bitmap-only colour emoji font) loads with empty glyphs, as fontique
  lists it: `type.fonts` then names the same families as the Rust build.
* `layer.smartObjects.exportContents` reports the bundle's byte size, which depends on each build's
  zstd encoder; the contents themselves are identical.
* serde_json without `float_roundtrip` reads a decimal as `significand as f64` scaled by a power of
  ten, up to one ULP from the correctly rounded value: a stroke's points (hashed, as JSON text,
  into its jitter seed) then differ. `sjson.parseSerdeJson` reads JSON exactly as serde_json does;
  `enginecheck` reads its scripts with it, as the oracle does.
* Where Rust reports serde's own message (`invalid brush: invalid type: string "x", expected f32`),
  parse under `withSerdeStrict`: serde_json's texts, no key path, map keys in sorted order and no
  `null` floats.
* Job-capable commands (Patch: Content-Aware, Content-Aware Move, Remove) run their work and apply
  inline; an algorithm's `Cancelled` maps to the engine's. The live stroke previews
  (`LiveRetouch`, `LiveDab`) land with the UI; `patchPreview` and `clonePreview` are ported.
* Clock-derived strings (a note's `D:YYYYMMDDhhmmssZ`, ISO 8601 times) compare equal in the
  checker: the two builds run at different moments.
* SVG sources (`io::svg`) are not ported: SVG smart objects report "can't render the SVG smart
  object"; linked-file stamps are taken synchronously (no worker thread).
