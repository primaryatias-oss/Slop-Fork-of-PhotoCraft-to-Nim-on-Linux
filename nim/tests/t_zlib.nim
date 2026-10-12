## Tests for photocraft/zlib.
import std/[unittest, random, osproc, os, strutils]
import photocraft/zlib

proc pyz(data: seq[byte]; mode: string): seq[byte] =
  let tmp = getTempDir() / "pc_zlib_in.bin"
  let outp = getTempDir() / "pc_zlib_out.bin"
  writeFile(tmp, data.toStr)
  let code = if mode == "compress": "import zlib,sys;d=open(sys.argv[1],'rb').read();open(sys.argv[2],'wb').write(zlib.compress(d,9))"
             else: "import zlib,sys;d=open(sys.argv[1],'rb').read();open(sys.argv[2],'wb').write(zlib.decompress(d))"
  let (o, rc) = execCmdEx("python3 -c \"" & code & "\" " & tmp & " " & outp)
  doAssert rc == 0, o
  readFile(outp).toBytes

suite "zlib":
  test "checksums":
    check crc32("123456789".toBytes) == 0xCBF43926'u32
    check adler32("Wikipedia".toBytes) == 0x11E60398'u32

  test "roundtrip at every level":
    var r = initRand(7)
    for size in [0, 1, 2, 3, 100, 5000, 70000, 300000]:
      var d = newSeq[byte](size)
      for i in 0 ..< size:
        d[i] = if i mod 7 < 3: byte(r.rand(255)) else: byte(i mod 13)
      for level in [0, 1, 6, 9]:
        check zlibDecompress(zlibCompress(d, level)) == d
        check inflateRaw(deflateRaw(d, level)) == d
      check gzipDecompress(gzipCompress(d)) == d

  test "interoperates with the reference zlib":
    if findExe("python3").len > 0:
      var d: seq[byte]
      for i in 0 ..< 200000: d.add byte((i * i) mod 251 xor (i shr 7))
      check zlibDecompress(pyz(d, "compress")) == d
      check pyz(zlibCompress(d, 6), "decompress") == d
      let text = "the quick brown fox jumps over the lazy dog. ".repeat(500).toBytes
      check pyz(zlibCompress(text, 9), "decompress") == text
      check zlibCompress(text, 9).len < 400

  test "hostile streams fail cleanly":
    expect ZlibError: discard zlibDecompress(@[0x78'u8, 0x9C, 0xFF, 0xFF, 0xFF])
    expect ZlibError: discard inflateRaw(@[0x07'u8])
    let bomb = zlibCompress(newSeq[byte](1_000_000))
    expect ZlibError: discard zlibDecompress(bomb, maxOut = 1000)
