## Counter-based deterministic randomness (port of `paint/src/rng.rs`).
##
## Every random value is a pure hash of `(seed, index, stream)`, so a dab's jitter depends only on
## the stroke seed and the dab's position in the stroke — never on how the points were chunked or
## on earlier draws. That is what makes journaled strokes replay exactly.

proc mix64*(z0: uint64): uint64 {.inline.} =
  ## SplitMix64 finaliser.
  var z = z0 + 0x9E37_79B9_7F4A_7C15'u64
  z = (z xor (z shr 30)) * 0xBF58_476D_1CE4_E5B9'u64
  z = (z xor (z shr 27)) * 0x94D0_49BB_1331_11EB'u64
  z xor (z shr 31)

proc rand01*(seed, index, stream: uint64): float32 {.inline.} =
  ## Uniform in [0, 1).
  let h = mix64(seed xor mix64((index * 0xD1B5_4A32_D192_ED03'u64) xor (stream * 0x8CB9_2BA7_2F3D_8DD7'u64)))
  float32(h shr 40) / float32(1'u64 shl 24)

proc randSigned*(seed, index, stream: uint64): float32 {.inline.} =
  ## Uniform in [-1, 1).
  rand01(seed, index, stream) * 2.0'f32 - 1.0'f32

proc hash2*(x, y: int; salt: uint64): float32 {.inline.} =
  ## Hash of integer document coordinates to [0, 1) (noise, dissolve). Coordinates wrap to 32 bits
  ## like the Rust crate's `i32` → `u32` casts.
  let k = (uint64(x and 0xFFFF_FFFF) shl 32) or uint64(y and 0xFFFF_FFFF)
  float32(mix64(k xor (salt * 0xA24B_AED4_963E_E407'u64)) shr 40) / float32(1'u64 shl 24)

const
  ## Streams: one per random quantity so they are independent.
  StSize* = 1'u64
  StAngle* = 2'u64
  StRoundness* = 3'u64
  StFlipX* = 4'u64
  StFlipY* = 5'u64
  StScatterX* = 6'u64
  StScatterY* = 7'u64
  StCount* = 8'u64
  StOpacity* = 9'u64
  StFlow* = 10'u64
  StFgBg* = 11'u64
  StHue* = 12'u64
  StSat* = 13'u64
  StBright* = 14'u64
  StDepth* = 15'u64
  StDualX* = 16'u64
  StDualY* = 17'u64
  StDualFlip* = 18'u64
  StWet* = 19'u64
  StMix* = 20'u64

proc seedFromBytes*(b: openArray[byte]): uint64 =
  ## Seed derived from arbitrary bytes (FNV-1a then mixed), for commands without an explicit seed.
  var h = 0xCBF2_9CE4_8422_2325'u64
  for x in b:
    h = h xor uint64(x)
    h = h * 0x0000_0100_0000_01B3'u64
  mix64(h)

proc seedFromBytes*(s: string): uint64 = seedFromBytes(s.toOpenArrayByte(0, s.high))
