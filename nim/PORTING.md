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
| psd | ported; byte-stable on the psd-tools corpus (125 files); little-endian layered TIFF transcoder not ported |
| doc, ops, vector | ported, tested |
| compose | ported, tested (GPU compositor intentionally not ported: SDL3 textures display the CPU result) |
| algo | in progress |
| paint, text, format, io, raw, affinity, engine, UI, CLI | in progress |
| heif (HEVC decoding), plugins (WebAssembly runtime) | not ported: they need a video decoder / wasm interpreter; the commands report "unsupported" |
