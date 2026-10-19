#!/usr/bin/env python3
"""Writes tools/algocases/raw.json: the camera raw cross-check cases (see algocheck.py, rawcheck.nim).

Usage: rawcases.py RAWDIR   (RAWDIR holds the oracle's `raw.gen` files; their sizes pick the
truncation points). Every file is decoded, listed (previews, TIFF dump) and decoded again after
truncations and seeded bit flips; decodable files are developed with several option sets; the
camera-look fit runs on the look fixtures.

Lossy-JPEG DNG tiles go through each port's own JPEG decoder (zune-jpeg in Rust): gray tiles decode
identically, colour tiles differ by up to 2 levels in a few samples, and corrupt tiles fail with
different messages. So the lossy files are only decoded and developed unmutated, and the colour
one is left out.
"""
import json, os, sys

def main():
    d = sys.argv[1]
    files = sorted(f for f in os.listdir(d) if not f.endswith(".ref"))
    cases = []
    for f in files:
        size = os.path.getsize(os.path.join(d, f))
        p = "%" + f
        if f.endswith(".ljpg"):
            for m in ["1000000", "100", "0"]:
                cases.append(["raw.lj", p, m])
            continue
        if f == "dng_linear_lossy.dng":
            continue
        cases.append(["raw.decode", p, "-"])
        cases.append(["raw.preview", p, "-"])
        cases.append(["raw.dump", p, "-"])
        lossy = "lossy" in f
        cuts = [] if lossy else sorted({0, 4, 8, 12, 16, 100, size // 7, size // 3, size // 2, size - 64, size - 1})
        for n in cuts:
            if 0 <= n < size:
                cases.append(["raw.decode", p, f"trunc:{n}"])
        for seed, k in [] if lossy else [(1, 1), (2, 3), (3, 8), (4, 32), (5, 2), (6, 5)]:
            cases.append(["raw.decode", p, f"flip:{seed}:{k}"])
        if f.startswith("look"):
            continue
        cases.append(["raw.develop", p, "-", "ahd", "asshot", "0", "1", "-"])
        cases.append(["raw.develop", p, "-", "bilinear", "auto", "0.5", "1", "-"])
        cases.append(["raw.develop", p, "-", "mhc", "m:2,1,1.5", "-1", "0", "-"])
        if not lossy:
            cases.append(["raw.develop", p, "flip:7:4", "ahd", "asshot", "0", "1", "-"])
        if f.startswith("dng_rggb") or f.startswith("dng_12bit") or f.startswith("raf_xtrans"):
            for dm in ["bilinear", "mhc", "ahd"]:
                for wb in ["asshot", "auto", "m:1.8,1,1.3", "m:0,1,1", "m:-1,1,1"]:
                    cases.append(["raw.develop", p, "-", dm, wb, "0.25", "1", "-"])
            cases.append(["raw.develop", p, "-", "ahd", "asshot", "20", "1", "-"])
            cases.append(["raw.develop", p, "-", "ahd", "asshot", "nan", "1", "-"])
    look = "%look.nef"
    wb = "m:1.5,1,1.25"
    cases.append(["raw.look", look, "%look.ref", "480", "320", wb, "-"])
    cases.append(["raw.look", look, "%look_up.ref", "320", "480", wb, "6"])
    cases.append(["raw.look", look, "%look.ref", "480", "320", wb, "6"])
    cases.append(["raw.look", look, "%look_noise.ref", "480", "320", wb, "-"])
    cases.append(["raw.look", look, "%look_grey.ref", "480", "320", wb, "-"])
    cases.append(["raw.look", look, "%look.ref", "320", "320", wb, "-"])
    cases.append(["raw.look", look, "%look.ref", "479", "320", wb, "-"])
    cases.append(["raw.look", look, "%look.ref", "0", "0", wb, "-"])
    cases.append(["raw.look", look, "%look.ref", "480", "320", "asshot", "-"])
    cases.append(["raw.develop", look, "-", "ahd", wb, "0", "1", "%look.ref:480:320"])
    cases.append(["raw.develop", look, "-", "bilinear", wb, "0", "1", "%look_noise.ref:480:320"])
    cases.append(["raw.develop", look, "-", "ahd", wb, "0", "1", "-"])
    out = os.path.join(os.path.dirname(os.path.abspath(__file__)), "algocases", "raw.json")
    with open(out, "w") as fh:
        json.dump(cases, fh, separators=(",", ":"))
    print(f"{len(cases)} cases -> {out}")

main()
