# Package

version       = "0.7.0"
author        = "PhotoCraft contributors"
description   = "PhotoCraft: a native, Photoshop-comparable image editor (Nim port: stdlib + SDL3)"
license       = "MIT OR Apache-2.0"
srcDir        = "src"
bin           = @["photocraft", "photocraft_cli"]
binDir        = "bin"

# Dependencies: the Nim standard library and the official SDL3 binding
# (nim-lang/sdl3, vendored under vendor/sdl3 at bb137829ff619b0a27a473628cb28fc1c86f3fe5).

requires "nim >= 2.2.0"

task test, "Run every unit test":
  exec "nim r --hints:off tests/runall.nim"
