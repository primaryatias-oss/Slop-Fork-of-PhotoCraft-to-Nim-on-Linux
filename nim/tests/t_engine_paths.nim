## File paths with a NUL byte: Nim hands paths to C as NUL-terminated strings, so without a guard
## such a path names a different (truncated) file or folder. Every engine file access refuses it
## with Rust's `std::io` message instead, and touches nothing on disk.

import std/[unittest, json, os, strutils]
import photocraft/[engine, format]

proc run(s: var Session; cmd: string; p: JsonNode): string =
  ## The error message of `cmd`, or "" when it succeeds.
  try:
    discard s.execute(cmd, p)
    ""
  except CatchableError as e: e.msg

suite "NUL in file paths":
  let base = getTempDir() / "pc-nul-test"
  setup:
    removeDir(base)

  test "render video refuses a folder with a NUL byte and creates nothing":
    var s = newSession()
    discard s.execute("file.new", %*{"width": 8, "height": 8})
    let msg = s.run("file.export.renderVideo", %*{"dir": base & "/\0x", "format": "png"})
    check msg.contains(NulPathMsg)
    check not dirExists(base)

  test "footage paths with a NUL byte fail cleanly":
    var s = newSession()
    discard s.execute("file.new", %*{"width": 8, "height": 8})
    let msg = s.run("layer.videoLayers.newVideoLayerFromFile", %*{"path": "/tmp\0/x.png"})
    check msg.contains(NulPathMsg)

  test "atomic writes and .pcraft paths refuse a NUL byte":
    expect FormatError: atomicWrite(base & "/a\0b.png", [1'u8, 2, 3])
    expect FormatError: atomicWrite(base & "\0/a.png", [1'u8, 2, 3])
    expect FormatError: discard loadPath(base & "\0.pcraft")
    check not dirExists(base)
    check not fileExists(base & "/a")
