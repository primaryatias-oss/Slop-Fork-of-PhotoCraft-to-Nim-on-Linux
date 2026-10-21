## Window › Swatches data (`presets/swatches.rs`): named colours in groups, each kept in the model it
## was made in (RGB, HSB, CMYK, Lab, grayscale, or a colour-book entry kept verbatim). The commands
## live in `swatch_cmds`. The default set is our own. Part of the `engine/core` unit.

proc swRgbColor*(c: array[3, float32]): SwatchColor = SwatchColor(kind: swRgb, v3: c)

proc newSwatch*(name: string; color: SwatchColor): Swatch = Swatch(name: name, color: color, kind: skProcess)

proc swFinite(v: float32): float32 = (if v == v and abs(v) != Inf: v else: 0'f32)
proc swUnit(v: float32): float32 = clamp(swFinite(v), 0'f32, 1'f32)

proc hsbToRgb*(hsb: array[3, float32]): array[3, float32] =
  ## HSB (hue in degrees) to RGB, both in the same RGB space.
  let s = swUnit(hsb[1])
  let v = swUnit(hsb[2])
  let h = remEuclidF(swFinite(hsb[0]), 360'f32) / 60'f32
  let c = v * s
  let x = c * (1'f32 - abs(h mod 2'f32 - 1'f32))
  let (r, g, b) = (case int(h)
                   of 0: (c, x, 0'f32)
                   of 1: (x, c, 0'f32)
                   of 2: (0'f32, c, x)
                   of 3: (0'f32, x, c)
                   of 4: (x, 0'f32, c)
                   else: (c, 0'f32, x))
  let m = v - c
  [swUnit(r + m), swUnit(g + m), swUnit(b + m)]

proc swatchHex*(c: array[3, float32]): string =
  proc q(v: float32): int = int(round(swUnit(v) * 255'f32))
  "#" & toHex(q(c[0]), 2).toLowerAscii & toHex(q(c[1]), 2).toLowerAscii & toHex(q(c[2]), 2).toLowerAscii

proc builtinSwatches*(): seq[PresetGroup[Swatch]] =
  ## The default library: 40 colours in four groups (our own palette).
  proc rgb(n: string; c: array[3, int]): Swatch =
    newSwatch(n, swRgbColor([float32(c[0]) / 255'f32, float32(c[1]) / 255'f32, float32(c[2]) / 255'f32]))
  let grays = [("Black", 0), ("90% Gray", 26), ("80% Gray", 51), ("70% Gray", 77), ("60% Gray", 102), ("50% Gray", 128),
               ("40% Gray", 153), ("30% Gray", 179), ("20% Gray", 204), ("White", 255)]
  let hues = ["Red", "Orange", "Yellow", "Yellow Green", "Green", "Cyan Green", "Cyan", "Blue", "Violet", "Magenta"]
  let pastel = [[236, 128, 128], [244, 176, 132], [250, 224, 128], [214, 240, 128], [150, 232, 150], [128, 232, 200],
                [128, 220, 240], [128, 176, 244], [168, 144, 244], [232, 144, 232]]
  let pure = [[230, 40, 40], [245, 120, 30], [250, 210, 30], [160, 220, 40], [40, 200, 80], [30, 200, 170], [30, 170, 230],
              [40, 100, 230], [120, 70, 220], [210, 50, 180]]
  let dark = [[120, 20, 20], [130, 60, 10], [130, 110, 10], [80, 120, 20], [20, 100, 40], [10, 100, 90], [10, 80, 120],
              [20, 50, 120], [60, 30, 110], [110, 20, 90]]
  proc row(prefix: string; cs: openArray[array[3, int]]): seq[Swatch] =
    for i, h in hues: result.add rgb(if prefix.len == 0: h else: prefix & " " & h, cs[i])
  var g: seq[Swatch]
  for (n, v) in grays: g.add rgb(n, [v, v, v])
  @[newGroup("Grays", g), newGroup("Pastel", row("Pastel", pastel)), newGroup("Pure", row("", pure)),
    newGroup("Dark", row("Dark", dark))]
