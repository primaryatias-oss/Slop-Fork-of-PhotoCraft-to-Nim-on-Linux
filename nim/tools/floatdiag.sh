#!/bin/bash
# Finds implicit float32 <-> float64 conversions in the port (see PORTING.md, "Nim pitfalls").
#
# Builds a copy of the Nim compiler whose C code generator reports every implicit (hidden)
# float32 -> float64 widening and every non-literal float64 -> float32 narrowing, then compiles
# the given programs (default: the test suite and the cross-check tools) and lists the sites in
# src/photocraft. Rust computes `f32` arithmetic in single precision; a widened operand (usually
# an unsuffixed float literal next to a float32 value) makes Nim compute it in double instead.
#
# Usage: tools/floatdiag.sh [program.nim ...]      (NIM_SRC defaults to /opt/src/nim)
set -e
cd "$(dirname "$0")/.."
NIM_SRC=${NIM_SRC:-/opt/src/nim}
WORK=${WORK:-/tmp/floatdiag}
mkdir -p "$WORK"
if [ ! -x "$WORK/nimdiag" ]; then
  rm -rf "$WORK/src"; mkdir -p "$WORK/src"
  cp -r "$NIM_SRC/compiler" "$NIM_SRC/dist" "$WORK/src/"
  python3 -I - "$WORK/src/compiler/ccgexprs.nim" <<'PY'
import sys
p = sys.argv[1]
s = open(p).read()
old = "proc genConv(p: BProc, e: PNode, d: var TLoc) =\n  if ignoreConv(e):"
new = """proc genConv(p: BProc, e: PNode, d: var TLoc) =
  if e.kind in {nkHiddenStdConv, nkHiddenSubConv} and e.len > 1 and e[1].typ != nil and e.typ != nil:
    let srcT = e[1].typ.skipTypes({tyVar, tyLent, tySink, tyRange, tyDistinct, tyAlias, tyGenericInst, tyStatic})
    let dstT = e.typ.skipTypes({tyVar, tyLent, tySink, tyRange, tyDistinct, tyAlias, tyGenericInst, tyStatic})
    if srcT != nil and dstT != nil:
      if srcT.kind == tyFloat32 and dstT.kind in {tyFloat, tyFloat64}:
        message(p.config, e[1].info, warnUser, "FLOATDIAG f32->f64 widening")
      elif srcT.kind in {tyFloat, tyFloat64} and dstT.kind == tyFloat32 and e[1].kind notin {nkFloatLit..nkFloat128Lit}:
        message(p.config, e[1].info, warnUser, "FLOATDIAG f64->f32 narrowing")
  if ignoreConv(e):"""
assert old in s, "genConv not found: update the patch for this Nim version"
open(p, "w").write(s.replace(old, new))
PY
  (cd "$WORK/src" && "$NIM_SRC/bin/nim" c --hints:off -d:release --nimcache:"$WORK/cache" -o:"$WORK/nimdiag" compiler/nim.nim)
fi
progs=("$@")
[ ${#progs[@]} -eq 0 ] && progs=(tests/runall.nim tools/algocheck.nim tools/filtercheck.nim)
for t in "${progs[@]}"; do
  "$WORK/nimdiag" c --compileOnly --lib:"$NIM_SRC/lib" --path:src --hints:off -d:release \
    --nimcache:"$WORK/n_$(basename "$t" .nim)" "$t" 2>&1 | grep FLOATDIAG || true
done | grep "/src/photocraft/" | sed 's|.*/src/photocraft/||' | sort -u
