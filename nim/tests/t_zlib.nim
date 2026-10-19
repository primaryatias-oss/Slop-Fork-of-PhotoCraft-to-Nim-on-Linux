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

  test "bounded streaming reads behave like flate2's ZlibDecoder (miniz)":
    var d: seq[byte]
    for i in 0 ..< 5000: d.add byte((i * 7) mod 256)
    let z = zlibCompress(d, 6)
    check zlibReadBounded(z, d.len + 1) == d
    check zlibReadBounded(z, 100) == d[0 ..< 100]
    # A wrong checksum is found when the 32 KiB look-ahead reaches the end, even past the cap.
    var bad = z
    bad[^1] = bad[^1] xor 1
    expect ZlibError: discard zlibReadBounded(bad, 100)
    # A stream that ends early fails once the reader still wants bytes, and not before.
    let cut = z[0 ..< z.len - 10]
    expect ZlibEof: discard zlibReadBounded(cut, d.len + 1)
    check zlibReadBounded(cut, 10) == d[0 ..< 10]
    expect ZlibEof: discard zlibReadBounded(@[0x78'u8], 10)
    # A distance before the start of the output reads miniz's zero-initialised window.
    # Fixed block: literal 'A', then a match of length 3 at distance 4 (one byte before the start).
    var o = newSeq[byte]()
    var acc = 0'u64
    var n = 0
    proc put(v: uint64; bits: int) =
      acc = acc or (v shl n)
      n += bits
    proc putRev(code, bits: int) =
      var r = 0
      for k in 0 ..< bits:
        if (code and (1 shl k)) != 0: r = r or (1 shl (bits - 1 - k))
      put(uint64(r), bits)
    put(1, 1); put(1, 2)              # final, fixed Huffman
    putRev(0x30 + 0x41, 8)            # literal 'A'
    putRev(1, 7)                      # length symbol 257: length 3
    putRev(3, 5)                      # distance symbol 3: distance 4
    putRev(0, 7)                      # end of block
    while n > 0:
      o.add byte(acc and 0xFF)
      acc = acc shr 8
      n = max(n - 8, 0)
    let plain = @[0x41'u8, 0, 0, 0]
    let a = adler32(plain)
    let stream = @[0x78'u8, 0x01] & o & @[byte(a shr 24), byte((a shr 16) and 0xFF), byte((a shr 8) and 0xFF), byte(a and 0xFF)]
    check zlibReadBounded(stream, 5) == @[0x41'u8, 0, 0, 0]
    expect ZlibError: discard zlibDecompress(stream)
