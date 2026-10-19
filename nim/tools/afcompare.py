"""Compares the Affinity reader dumps of the Rust oracle and the Nim port (nim/PORTING.md).

usage: python3 -I afcompare.py ORACLE AFCHECK DIR [read|preview]

Runs `ORACLE af.<mode>` and `AFCHECK <mode>` over every file in DIR in batches; a batch that
aborts (the Rust reader overflows its stack on some cyclic documents) is rerun file by file and
the aborting file is reported as `abort`. Files where the Rust reader panics or aborts are
counted separately: there the Nim port returns an error or a document instead.
"""

import subprocess
import sys
from pathlib import Path


def run(cmd, files):
    """Dumps per file: {path: [lines]}; None for a file whose process aborted."""
    out = {}
    p = subprocess.run(cmd + files, capture_output=True, text=True, errors="replace")
    if p.returncode != 0:
        if len(files) == 1:
            return {files[0]: None}
        for f in files:
            out.update(run(cmd, [f]))
        return out
    cur = None
    for line in p.stdout.splitlines():
        if line.startswith("== "):
            cur = line[3:]
            out[cur] = []
        elif cur is not None:
            out[cur].append(line)
    return out


def main():
    oracle, afcheck, d = sys.argv[1], sys.argv[2], Path(sys.argv[3])
    mode = sys.argv[4] if len(sys.argv) > 4 else "read"
    files = sorted(str(p) for p in d.iterdir() if p.is_file())
    rust, nim = {}, {}
    for i in range(0, len(files), 64):
        batch = files[i : i + 64]
        rust.update(run([oracle, "af." + mode], batch))
        nim.update(run([afcheck, mode], batch))
    same = diff = rust_fail = 0
    for f in files:
        r, n = rust.get(f), nim.get(f)
        if r is None or r == ["panic"]:
            rust_fail += 1
            continue
        if r == n:
            same += 1
        else:
            diff += 1
            if diff <= 5:
                print("DIFF", f)
                for a, b in zip(r, n or []):
                    if a != b:
                        print("  rust:", a[:400])
                        print("  nim: ", b[:400])
                        break
                else:
                    print("  lengths", len(r), len(n or []))
    print(f"{same} identical, {diff} different, {rust_fail} where the Rust reader panics or aborts")
    sys.exit(1 if diff else 0)


if __name__ == "__main__":
    main()
