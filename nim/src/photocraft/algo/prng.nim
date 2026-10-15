## The splitmix64 generator of the patch-based fills (`inpaint::Rng` in Rust).

type PatchRng* = object
  s: uint64

proc newPatchRng*(seed: uint64): PatchRng = PatchRng(s: seed xor 0x9E37_79B9_7F4A_7C15'u64)

proc next*(r: var PatchRng): uint64 =
  r.s = r.s + 0x9E37_79B9_7F4A_7C15'u64
  var z = r.s
  z = (z xor (z shr 30)) * 0xBF58_476D_1CE4_E5B9'u64
  z = (z xor (z shr 27)) * 0x94D0_49BB_1331_11EB'u64
  z xor (z shr 31)

proc skip*(r: var PatchRng; n: uint64) =
  ## Skips `n` numbers (the same state as after `n` calls of `next`).
  r.s = r.s + n * 0x9E37_79B9_7F4A_7C15'u64

proc range*(r: var PatchRng; lo, hi: int32): int32 =
  ## Uniform integer in `lo..=hi`.
  if hi <= lo: return lo
  let span = uint64(int64(hi) - int64(lo) + 1)
  int32(int64(lo) + int64(r.next mod span))
