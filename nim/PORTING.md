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

* Rust's `f32::cbrt`/`f64::cbrt` are not glibc's (they come from Rust's bundled `libm`); use
  `photocraft/rmath.cbrt`. Every other libm function the port uses matches glibc bit for bit
  (checked on 200k inputs each). Rust's `powi(n)` at run time is repeated multiplication
  (`x * x * x`), but a constant-folded `powi` equals `pow`.

* Closures cannot capture `openArray`/`var` parameters. Use a `template`, a top-level `proc` that
  takes the data as a parameter, or copy (`let vals = @values`). `nim check` does not catch this;
  only a real compile (`nim r`/`nim c`) does.
* Template parameters are substituted everywhere in the body, including field names: a template
  parameter called `at`, `x0` or `rect` will rewrite `r.at`/`r.x0`. Pick unique parameter names.
* Identifiers are style-insensitive except the first letter: `im` and `iM` are the same.
* Statement lists inside parenthesised expressions (`(let a = ..; if ..)`) are fragile; use a
  template or a helper proc.
* `float32` inference: `0.5 * arctan2(a, b)` is `float` (64-bit) when the arguments are; annotate
  (`let angle: float32 = ...`) or convert. `degToRad` on a `float32` literal may yield `float`.
* An object field shadows a proc of the same name in method-call syntax (`l.name` finds the field).
* `let x = s[i]` copies objects that own `seq`s; index in place or pass as a parameter (non-`var`
  object parameters are passed by reference).

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
| algo | in progress: foundation, resample, transform, warp; filter dispatcher (`algo/filters`: halos, tiled apply, selection mixing, registry); blur family (`blur`, `sharpen`, `blur2`, `gallery`, `radial_plan`: Gaussian/Box with the separable fast path, Motion with the FFT fast path, Radial, Surface, Unsharp Mask, Smart Sharpen, High Pass, Smart/Lens/Shape Blur, Tilt-Shift, Iris, Field, Spin, Path); noise, median, custom, HSB/HSL, color to alpha, video, stylize part 2. 397 of 399 filter runs identical to Rust (`tools/filtercases.json` × 7 documents); the two others differ by ≤1.5e-8 (FFT motion blur, f32) and 1 level on 31 samples (HSB/HSL, Lab 16-bit) |
| io (PSD/PSB ↔ document, layered TIFF, multi-part EXR, flat formats, facade) | ported, tested; see "io notes" below |
| io: OpenRaster, Paint.NET | ported, tested |
| io: SVG, camera raw, Affinity | pending (the facade reports "unsupported" until they land; SVG needs a usvg-style normalizer) |
| text: font reading (`font/sfnt`: TrueType/CFF outlines, cmap, metrics, names) | ported; outlines, metrics and charmaps identical to skrifa on the bundled and system fonts |
| text: shaping (`font/otl`, `font/shape`: HarfBuzz default, Hebrew, Arabic, Thai/Lao, Hangul shapers) | ported; glyphs, clusters and positions identical to HarfRust on 324 test strings across 17 fonts. Indic, USE, Khmer and Myanmar shapers pending (those scripts shape with the default shaper) |
| text: Unicode data (`text/ucd_data` from Perl's UCD 15.0 via `tools/ucdgen.nim`), line breaking (UAX #14), grapheme clusters (UAX #29) | ported (no dictionary breaking for Thai/Lao/Khmer/Burmese) |
| text: font database (`text/fonts`: bundled + system fonts, fontique-style matching and synthesis, PostScript names), bidi (UAX #9), paragraph layout (`text/para`, the parley port), type layout (`text/layout`: paragraphs, box text, tabs, small caps, optical kerning, decorations, vertical type) | ported; layouts identical to the Rust text engine on 29 layers (bundled and system fonts: Latin, Hebrew/Arabic RTL, mixed bidi, CJK, Thai, vertical Japanese) |
| text: rendering (`text/raster`, `text/render`, `text/warp`; skrifa-exact 26.6/16.16 outline scaling in `font/sfnt`), `TextEngine` (`text.nim`) | ported; rasterized text bit-identical to the Rust engine on 31 layers (warps, transforms, faux bold/italic, colours, aliasing, vertical, Arabic) |
| text: PSD type (`text/engine_data`: EngineData parse/write; `text/psd_text`: `TySh` ⇄ `TextLayer`, Photoshop's kerning pair fields, `Txt2` auto-kern modes; `text/psd_styles`: named style sheets), io wiring (`io/text_import`, `io/text_styles_map`, `TySh`/`Txt2` regeneration on PSD export) | ported; models, rebuilt `TySh`, EngineData and `Txt2` bytes identical to Rust on every type layer of the corpus at 0/72/96.5/300 dpi, and on generated layers (kerning, Optical, box, vertical, warp, named styles) |
| text: spelling (`text/spell`, SCOWL dictionary), UI fallback scripts (`text/scripts`), Glyphs panel (`text/glyphs`) | ported; checks, suggestions, charmaps, category counts, previews and candidate lists identical to Rust |
| paint, raw, affinity, engine, UI, CLI | in progress |
| heif (HEVC decoding), plugins (WebAssembly runtime) | not ported: they need a video decoder / wasm interpreter; the commands report "unsupported" |

## Checking against Rust

`tools/oracle` is a small standalone Cargo project that runs the Rust crates as the reference:
`cargo build --release --manifest-path tools/oracle/Cargo.toml`, then
`oracle write|read|render|import|export|conv|filter …` (see `src/main.rs`). The port is compared
by document JSON + tile hashes (`.pcraft`), raw float renders, or output bytes.

## Filter notes

* `tools/filtercheck.py [--docs …] [--tol L] <params json | cases.json>…` runs `FilterParams`
  through the Rust oracle (`oracle filter`) and the port (`tools/filtercheck.nim`) on seven
  generated documents (RGB 8/16/32-bit, gray, CMYK, Lab 16-bit, no alpha) and compares pixels.
  `tools/filtercases.json` is the regression list of verified parameter sets.
* Filter modules register with `algo/filters` (`registerKernel`, `registerHalo`, `registerPlan`,
  `registerFastPath`); `photocraft/algo` imports them all.
* Halo radii use Rust's release-build i32 arithmetic (wrapping), so absurd radii give the same
  (empty) areas as Rust instead of huge allocations.
* Motion Blur's FFT path is taken under the same conditions as Rust (including the core count
  from `countProcessors`, rayon's default); its f64 FFT is the port's own, so 32-bit float
  results can differ in the last bits.

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

* PSD → document: identical documents (manifest JSON and tile hashes) on 308 of 309 imports,
  type layers included (typed model, `Txt2` kerning modes, named styles, layers rendered from
  their model when the file has no pixels). The other one differs by 2e-6 in a perceptual
  gradient-map stop.
* PSD → PSD: every layer record, tagged block (`TySh`, `Txt2` and the rest), global block and
  resource identical on all 308 files Rust converts, except the 1036 JPEG preview (independent JPEG
  encoder; close but not equal pixels). The merged image is identical on 301; on 7 (CMYK and
  hue/saturation adjustments, CMYK blend modes, layer effects) 1 to 27 samples differ by 1–2
  levels (compositor float rounding). 16/32-bit ZIP streams can differ in bytes (the port's own
  deflate vs miniz_oxide) with identical decoded content.
* Flat exports: PNG and TIFF decode to identical pixels; JPEG and WebP (lossy) differ (encoders are
  independent implementations).
* Layered TIFF: identical layer data (tag 37724) and resources (34377) except text and ZIP cases.
* Rust's serde_json reads floats from `.pcraft` JSON up to 1 ULP off; the port reads them exactly,
  so a vector mask that Rust regenerates after a `.pcraft` round trip is kept verbatim here.
* Flat enum types (`Effect`, `Fill`, `Adjustment`, …) compare only their active variant (as Rust's
  derived `PartialEq` does), through their serde form.

