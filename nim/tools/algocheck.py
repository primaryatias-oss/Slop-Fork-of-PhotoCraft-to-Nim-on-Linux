#!/usr/bin/env python3
"""Runs non-filter algo commands through the Rust oracle and the Nim port and compares them.

Usage: algocheck.py [--tol X] cases.json...
Each file holds a JSON list of argument lists: `["sel.wand", "@img", "10", "20", "32", "1", "0"]`.
`@name` stands for `$WORK/name.pcim` (`@a:@b` for a pair of them) (written by `algocheck gen`), `%name`
for `$WORK/raw/name` (raw files written by the oracle's `raw.gen`); both programs get the same
arguments plus an output path, and their stdout and output files must match byte for byte (or,
with --tol, f32 outputs to within X).
Environment: ORACLE (Rust oracle binary), ALGOCHECK (Nim binary), WORK (scratch dir).
"""
import json, os, struct, subprocess, sys

ORACLE = os.environ.get("ORACLE", "/tmp/claude-0/scr/xcheck/target/release/photocraft-oracle")
NIM = os.environ.get("ALGOCHECK", "/tmp/claude-0/scr/algocheck")
WORK = os.environ.get("WORK", "/tmp/claude-0/scr/ac")

def floats(path):
    d = open(path, "rb").read()
    w, h, ch, kind = struct.unpack("<4I", d[4:20])
    if kind == 0:
        return list(struct.unpack("<%df" % ((len(d) - 20) // 4), d[20:]))
    return list(d[20:])

def main():
    args = sys.argv[1:]
    tol = 0.0
    cases = []
    while args:
        a = args.pop(0)
        if a == "--tol": tol = float(args.pop(0))
        else: cases.extend(json.load(open(a)))
    os.makedirs(WORK, exist_ok=True)
    if any(x.startswith("@") for c in cases for x in c) and not os.path.exists(os.path.join(WORK, "img.pcim")):
        subprocess.run([NIM, "gen", WORK], check=True)
    if any(x.startswith("%") for c in cases for x in c) and not os.path.exists(os.path.join(WORK, "raw")):
        subprocess.run([ORACLE, "raw.gen", os.path.join(WORK, "raw"), "-"], check=True)
    bad = 0
    for c in cases:
        argv = [":".join(os.path.join(WORK, y[1:] + ".pcim") for y in x.split(":")) if x.startswith("@")
                else os.path.join(WORK, "raw", x[1:]) if x.startswith("%") else x for x in c]
        r, n = os.path.join(WORK, "r.out"), os.path.join(WORK, "n.out")
        for p in (r, n):
            if os.path.exists(p): os.remove(p)
        ro = subprocess.run([ORACLE] + argv + [r], capture_output=True, text=True)
        no = subprocess.run([NIM] + argv + [n], capture_output=True, text=True)
        label = " ".join(c)
        if ro.returncode != 0:
            print(f"RUST-ERR {label}: {ro.stderr.strip()[-300:]}"); bad += 1; continue
        if no.returncode != 0:
            print(f"NIM-ERR {label}: {no.stderr.strip()[-300:]}"); bad += 1; continue
        if ro.stdout != no.stdout:
            print(f"STDOUT {label}:\n  rust: {ro.stdout.strip()[:300]}\n  nim:  {no.stdout.strip()[:300]}"); bad += 1; continue
        if os.path.exists(r) != os.path.exists(n):
            print(f"OUTPUT {label}: only one side wrote a file"); bad += 1; continue
        if os.path.exists(r) and open(r, "rb").read() != open(n, "rb").read():
            rb, nb = open(r, "rb").read(), open(n, "rb").read()
            if not rb.startswith(b"PCIM"):
                first = next((i for i, (x, y) in enumerate(zip(rb, nb)) if x != y), min(len(rb), len(nb)))
                print(f"DIFF {label}: {len(rb)} vs {len(nb)} bytes, first difference at {first}"); bad += 1; continue
            a, b = floats(r), floats(n)
            if len(a) != len(b):
                print(f"DIFF {label}: sizes {len(a)} vs {len(b)}"); bad += 1; continue
            diffs = [(abs(x - y), i) for i, (x, y) in enumerate(zip(a, b)) if not (x == y or (x != x and y != y))]
            worst = max(diffs) if diffs else (0, -1)
            if worst[0] > tol or any(d != d for d, _ in diffs):
                print(f"DIFF {label}: {len(diffs)} of {len(a)} differ, max {worst[0]:.3g} at {worst[1]} ({a[worst[1]]} vs {b[worst[1]]})"); bad += 1
    print(f"{len(cases) - bad}/{len(cases)} identical")
    sys.exit(1 if bad else 0)

main()
