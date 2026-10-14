#!/usr/bin/env python3
"""Runs filter params through the Rust oracle and the Nim port on the generated test documents.

Usage: filtercheck.py [--tol LEVELS] [--docs rgb8,cmyk8] params.json...
       filtercheck.py [--tol LEVELS] '{"filter": "invert"}' ...
Each argument is a FilterParams JSON string, or a file holding a JSON list of them.
Environment: ORACLE (Rust oracle binary), FILTERCHECK (Nim filtercheck binary), WORK (scratch dir).
"""
import json, os, subprocess, sys

ORACLE = os.environ.get("ORACLE", "/tmp/claude-0/scr/xcheck/target/release/photocraft-oracle")
NIM = os.environ.get("FILTERCHECK", "/tmp/claude-0/scr/filtercheck")
WORK = os.environ.get("WORK", "/tmp/claude-0/scr/fc")
DOCS = ["rgb8", "rgb16", "rgb32", "gray8", "cmyk8", "lab16", "rgb8flat"]

def main():
    args = sys.argv[1:]
    tol = "0"
    docs = DOCS
    params = []
    while args:
        a = args.pop(0)
        if a == "--tol": tol = args.pop(0)
        elif a == "--docs": docs = args.pop(0).split(",")
        elif a.lstrip().startswith("{"): params.append(json.loads(a))
        else: params.extend(json.load(open(a)))
    os.makedirs(WORK, exist_ok=True)
    if not os.path.exists(os.path.join(WORK, "rgb8.pcraft")):
        subprocess.run([NIM, "gen", WORK], check=True)
    bad = 0
    for p in params:
        js = json.dumps(p)
        for d in docs:
            src = os.path.join(WORK, d + ".pcraft")
            r, n = os.path.join(WORK, "r.pcraft"), os.path.join(WORK, "n.pcraft")
            for f in (r, n):
                if os.path.exists(f): os.remove(f)
            ro = subprocess.run([ORACLE, "filter", src, js, r, "0"], capture_output=True, text=True)
            no = subprocess.run([NIM, "run", src, js, n, "0"], capture_output=True, text=True)
            if not os.path.exists(r):
                print(f"RUST-ERR {d} {js}: {ro.stderr.strip()[-200:]}"); bad += 1; continue
            if not os.path.exists(n):
                print(f"NIM-ERR {d} {js}: {no.stderr.strip()[-300:]}"); bad += 1; continue
            s = subprocess.run([NIM, "same", r, n, "--tol", tol], capture_output=True, text=True)
            if s.returncode != 0:
                print(f"DIFF {d} {js}: {s.stdout.strip()[:300]}"); bad += 1
    total = len(params) * len(docs)
    print(f"{total - bad}/{total} identical")
    sys.exit(1 if bad else 0)

main()
