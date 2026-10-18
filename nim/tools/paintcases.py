#!/usr/bin/env python3
"""Writes tools/algocases/paint.json, the brush engine cases for algocheck.py.

Usage: python3 tools/paintcases.py tools/algocases/paint.json
"""
import json, math, sys
J = lambda o: json.dumps(o, separators=(",", ":"))
def disc(n, holes=False):
    v = []
    for y in range(n):
        for x in range(n):
            d = math.hypot(x + 0.5 - n / 2, y + 0.5 - n / 2) / (n / 2)
            a = max(0.0, min(1.0, (1 - d) * 3))
            if holes and (x * 7 + y * 3) % 5 == 0: a *= 0.3
            v.append(round(a, 4))
    return v
SAMPLED = {"sampled": {"width": 16, "height": 16, "data": disc(16, True)}}
WIDE = {"sampled": {"width": 21, "height": 9, "data": [round(((x * 13 + y * 7) % 10) / 9, 3) * (1 if 2 < x < 18 else 0) for y in range(9) for x in range(21)]}}
TILE = {"tile": {"width": 3, "height": 2, "data": [0, 0.5, 1, 0.25, 0.75, 0.1]}}
STORED = {"stored": {"key": "abc", "width": 300, "height": 200, "preview": {"width": 8, "height": 8, "data": disc(8)}}}
B = {
 "def": {},
 "hard": {"size": 12, "hardness": 0.5, "spacing": 0.25, "pressureSize": False},
 "soft": {"size": 30, "hardness": 0, "flow": 0.3, "opacity": 0.8, "color": [0.9, 0.2, 0.1, 1]},
 "tiny": {"size": 3, "hardness": 0.8, "spacing": 0.2},
 "pencil1": {"size": 1, "aliased": True},
 "pencil4": {"size": 4, "aliased": True, "spacing": 0.1, "pressureSize": False},
 "block": {"size": 6, "aliased": True, "square": True},
 "shape": {"size": 20, "shapeDynamics": {"enabled": True, "size": {"jitter": 0.5, "minimum": 0.2}, "angle": {"jitter": 0.3, "control": "direction"}, "roundness": {"jitter": 0.4, "minimum": 0.3}, "flipXJitter": True, "flipYJitter": True}, "seed": 7, "roundness": 0.6, "angle": 30},
 "scatter": {"size": 15, "scattering": {"enabled": True, "scatter": {"jitter": 1.5}, "bothAxes": True, "count": 3, "countJitter": {"jitter": 0.5}}, "seed": 3},
 "scatter2": {"size": 15, "scattering": {"enabled": True, "scatter": {"jitter": 0.8, "control": "penPressure"}, "count": 2}, "seed": 12},
 "texture": {"size": 25, "hardness": 0.7, "texture": {"enabled": True, "pattern": {"procedural": {"style": "canvas", "size": 32, "seed": 2}}, "mode": "subtract", "depth": 0.7, "scale": 1.5, "brightness": 0.1, "contrast": 0.2}},
 "texeach": {"size": 25, "texture": {"enabled": True, "eachTip": True, "pattern": {"procedural": {"style": "noise", "size": 16, "seed": 5}}, "mode": "hardMix", "depthJitter": {"jitter": 0.6}, "invert": True}, "seed": 4},
 "textile": {"size": 22, "texture": {"enabled": True, "pattern": TILE, "mode": "linearHeight", "scale": 1}},
 "dual": {"size": 24, "dualBrush": {"enabled": True, "size": 10, "spacing": 0.3, "scatter": 1, "bothAxes": True, "count": 2, "flip": True, "mode": "colorBurn", "tip": SAMPLED}, "seed": 11},
 "dual2": {"size": 18, "dualBrush": {"enabled": True, "size": 6, "spacing": 0.5, "scatter": 2, "mode": "height", "hardness": 0.3, "roundness": 0.5, "angle": 40}, "seed": 2},
 "color": {"size": 14, "colorDynamics": {"enabled": True, "perTip": True, "fgBg": {"jitter": 0.6}, "hueJitter": 0.3, "saturationJitter": 0.2, "brightnessJitter": 0.1, "purity": -0.3}, "color": [0.8, 0.3, 0.1, 1], "background": [0.1, 0.4, 0.9, 0.5], "seed": 5},
 "color2": {"size": 14, "colorDynamics": {"enabled": True, "perTip": False, "fgBg": {"control": "fade", "fadeSteps": 5}, "purity": 0.6}, "color": [0.2, 0.7, 0.3, 1], "seed": 6},
 "transfer": {"size": 18, "transfer": {"enabled": True, "opacity": {"jitter": 0.5, "control": "penPressure"}, "flow": {"control": "fade", "fadeSteps": 10, "minimum": 0.2}, "wetness": {"control": "penPressure"}, "mix": {"jitter": 0.4}}, "pressureOpacity": True},
 "wet": {"size": 26, "hardness": 0.4, "wetEdges": True, "noise": True},
 "air": {"size": 20, "hardness": 0, "flow": 0.2, "buildUp": True, "buildUpRate": 40},
 "smooth": {"size": 10, "smoothing": {"amount": 0.5}},
 "string": {"size": 10, "smoothing": {"amount": 0.3, "pulledString": True}},
 "nocatch": {"size": 10, "smoothing": {"amount": 0.7, "catchUp": False, "catchUpOnEnd": False}},
 "speed": {"size": 9, "spacingEnabled": False},
 "pose": {"size": 16, "pose": {"enabled": True, "tiltX": 30, "tiltY": -20, "rotation": 45, "pressure": 0.5, "overrideTilt": True, "overrideRotation": True, "overridePressure": True}, "shapeDynamics": {"enabled": True, "size": {"control": "penTilt"}, "tiltScale": 1.5, "brushProjection": True, "angle": {"control": "penTilt"}}},
 "proj": {"size": 16, "tip": WIDE, "shapeDynamics": {"enabled": True, "brushProjection": True, "angle": {"control": "rotation"}, "roundness": {"control": "stylusWheel", "minimum": 0.2}}},
 "sampled": {"size": 28, "tip": SAMPLED, "spacing": 0.3, "angle": 25, "flipX": True},
 "sampledal": {"size": 12, "tip": WIDE, "aliased": True, "roundness": 0.5},
 "stored": {"size": 20, "tip": STORED},
 "multiply": {"size": 22, "mode": "Multiply", "color": [0.2, 0.4, 0.9, 1]},
 "screen": {"size": 22, "mode": "Screen", "color": [0.9, 0.4, 0.2, 0.7], "opacity": 0.6},
 "erase": {"size": 20, "erase": True, "hardness": 0.3},
 "dissolve": {"size": 20, "mode": "Dissolve", "seed": 9, "opacity": 0.5, "hardness": 0.2},
 "huge": {"size": 1e9, "scattering": {"enabled": True, "scatter": {"jitter": 1e6}}},
 "neg": {"size": -5, "spacing": -1, "roundness": 0},
}
P = {
 "p0": [{"x": 5, "y": 5}, {"x": 60, "y": 30}, {"x": 80, "y": 60}],
 "p1": [{"x": 10.3, "y": 20.7, "pressure": 0.2}, {"x": 30.1, "y": 22.4, "pressure": 0.7}, {"x": 55.5, "y": 40.2, "pressure": 1}, {"x": 70.9, "y": 15.1, "pressure": 0.4}],
 "p2": [{"x": 40.5, "y": 30.5}],
 "p3": [{"x": 20, "y": 20, "time": 0}, {"x": 20, "y": 20, "time": 50}, {"x": 40, "y": 25, "time": 100}, {"x": 40, "y": 25, "time": 400}, {"x": 70, "y": 50, "time": 420}, {"x": 71, "y": 50, "time": 9e6}],
 "p4": [{"x": 10, "y": 50, "tiltX": 40, "tiltY": 10, "rotation": 90, "wheel": 0.3}, {"x": 50, "y": 10, "tiltX": -20, "tiltY": 60, "rotation": 200, "wheel": 0.8}, {"x": 85, "y": 55, "rotation": 370}],
 "p5": [{"x": 8 + i * 2.7, "y": 35 + 18 * math.sin(i * 0.7), "pressure": 0.3 + 0.7 * abs(math.cos(i * 0.4)), "time": i * 12.5} for i in range(30)],
 "p6": [{"x": 3.2, "y": 4.7}, {"x": 40.6, "y": 22.1}, {"x": 41, "y": 60.5}],
 "p7": [{"x": -30, "y": -30}, {"x": 130, "y": 100}],
}
for k, v in P.items():
    for p in v:
        for kk in list(p): p[kk] = round(p[kk], 4) if isinstance(p[kk], float) else p[kk]
cases = []
for seed in ["0", "1", "12345678901234", "18446744073709551615"]:
    cases.append(["pt.rng", seed, "50"])
for amt in ["0", "0.5", "0.99"]:
    cases.append(["pt.misc", J(P["p1"]), amt])
for st in ["noise", "paper", "canvas", "dots"]:
    for size, seed in [("8", "0"), ("37", "1"), ("64", "4294967295"), ("3", "9"), ("300", "2")]:
        cases.append(["pt.pattern", st, size, seed])
for kind in ["chalk", "spatter", "bristle", "charcoal", "leaf", "grass", "sponge", "star", "rake"]:
    for n, seed, extra, pv in [("16", "1", "5", "96"), ("48", "7", "14", "7"), ("64", "3", "0", "0"), ("1", "2", "1", "1"), ("33", "99", "40", "10")]:
        cases.append(["pt.tip", kind, n, seed, extra, pv])
bj = [B[k] for k in B] + [{"pressure_size": False, "pressure_opacity": True}, {"tip": {"round": None}}, {"tip": "sampled"}, {"tip": {"sampled": {"width": 2, "height": 2, "data": [0, 1]}}},
      {"tip": {"sampled": {"width": 2, "height": 1, "data": "AAD//w=="}}}, {"tip": {"sampled": {"width": 2, "height": 1, "data": "AAD//w="}}}, {"texture": {"pattern": {"procedural": {"style": "dots", "size": 5}}}},
      {"texture": {"pattern": "procedural"}}, {"mode": "Nope"}, {"size": "big"}, {"seed": 123456789012}, {"square": True}, {"unknownField": 3, "scattering": {"count": 0}}, [1, 2], {"tip": {"stored": {"key": "x", "width": 1, "height": 1}}}]
for b in bj:
    cases.append(["pt.brushjson", J(b)])
cases.append(["pt.presets"])
for bk in B:
    for pk in P:
        if bk == "huge" and pk in ("p5", "p7"): continue
        cases.append(["pt.dabs", J(B[bk]), J(P[pk]), "2" if pk in ("p3", "p5") else "1", "1"])
for bk in ["smooth", "string", "nocatch", "air", "speed"]:
    for pk in ["p3", "p5", "p1"]:
        cases.append(["pt.dabs", J(B[bk]), J(P[pk]), "4", "2.5"])
fmts = ["rgb", "rgb16", "rgb32", "gray", "cmyk", "lab", "rgbx", "gray16x"]
for bk in B:
    if bk == "huge": continue
    for pk, img, f in [("p0", "-", "rgb"), ("p1", "@img", "rgb16"), ("p5", "@img", "rgb32"), ("p6", "-", "gray")]:
        cases.append(["pt.render", f, img, J(B[bk]), J(P[pk]), "-", "0", "1"])
for f in fmts:
    for bk in ["hard", "color", "multiply", "erase", "dissolve", "soft"]:
        cases.append(["pt.render", f, "@img", J(B[bk]), J(P["p1"]), "@mask", "1", "1"])
        cases.append(["pt.render", f, "@img", J(B[bk]), J(P["p0"]), "-", "0", "2"])
cases.append(["pt.render", "rgb", "-", J(B["huge"]), J(P["p2"]), "-", "0", "1"])
for bk in ["hard", "dual", "wet", "color", "texture", "smooth", "air", "pencil4"]:
    for ch in ["1", "3"]:
        cases.append(["pt.chunked", "rgb", "@img", J(B[bk]), J(P["p5"]), ch])
for bk in ["hard", "dual", "wet", "texture", "sampled", "texeach", "stored", "pencil1", "tiny"]:
    for pk in ["p0", "p1", "p6"]:
        cases.append(["pt.coverage", J(B[bk]), J(P[pk])])
mixers = [({"wet": 0, "load": 0, "mix": 0, "flow": 1}, {"reservoir": [1, 0, 0, 1], "amount": 1}),
          ({"wet": 1, "load": 0.3, "mix": 0.9, "flow": 1}, {"reservoir": [1, 0, 0, 1], "amount": 1}),
          ({"wet": 0.5}, {}),
          ({"wet": 1, "load": 1, "mix": 1, "flow": 0.7}, {"reservoir": [0.2, 0.3, 0.9, 0.8], "amount": 0.4, "pickup": [0, 1, 0, 1]}),
          ({"wet": 0.8, "mix": 0.3}, {"pickup": [0.5, 0.5, 0.1, 1]})]
for f in ["rgb", "rgb32", "cmyk16", "gray"]:
    for m, s in mixers:
        for bk in ["hard", "transfer"]:
            cases.append(["pt.mixer", f, "@img", J(B[bk]), J(P["p1"]), J(m), J(s), "-", "0"])
    cases.append(["pt.mixer", f, "@img", J(B["hard"]), J(P["p0"]), J(mixers[1][0]), J(mixers[1][1]), "@img2", "1"])
for f in ["rgb", "rgb32", "lab16"]:
    for mode in ["hue", "saturation", "color", "luminosity"]:
        for samp in ["continuous", "once", "backgroundSwatch"]:
            for lim, tol in [("discontiguous", 0.1), ("contiguous", 0.05), ("findEdges", 0.3)]:
                rs = {"mode": mode, "sampling": samp, "limits": lim, "tolerance": tol, "color": [1, 0, 0, 1], "background": [0.1, 0.1, 0.5, 1]}
                cases.append(["pt.replace", f, "@ptsplit", J(B["hard"] | {"size": 30}), J(P["p0"]), J(rs), "0"])
    cases.append(["pt.replace", f, "@img", J(B["shape"]), J(P["p5"]), J({"tolerance": 0.4, "antiAlias": False}), "1"])
for f in ["rgb", "rgb16", "rgb32", "cmyk", "gray", "rgbx"]:
    for samp in ["continuous", "once", "backgroundSwatch"]:
        for lim in ["discontiguous", "contiguous", "findEdges"]:
            bs = {"sampling": samp, "limits": lim, "tolerance": 0.05, "background": [0.1, 0.1, 0.5, 1]}
            cases.append(["pt.bgerase", f, "@ptsplit", J({"size": 30, "spacing": 0.25, "pressureSize": False}), J(P["p0"]), J(bs), "-"])
    cases.append(["pt.bgerase", f, "@ptsplit", J(B["hard"]), J(P["p6"]), J({"sampling": "once", "protectForeground": True, "foreground": [0.2, 0.6, 0.2, 1]}), "-"])
    cases.append(["pt.bgerase", f, "@img", J(B["soft"]), J(P["p1"]), J({"tolerance": 0.3}), "@mask"])
for f in ["rgb", "rgb16", "rgb32", "gray", "cmyk"]:
    for bk in ["hard", "soft", "sampled"]:
        for fin, al, op, halo in [("-", "0", "1", "0"), ("-", "1", "0.5", "2"), ("0,0,1,1", "1", "1", "0"), ("0.3", "0", "0.8", "1")]:
            cases.append(["pt.smudge", f, "@img", J(B[bk]), J(P["p1"]), "0.8", fin, al, op, halo])
for f in ["rgb", "rgb32", "cmyk", "gray16"]:
    for bl in ["normal", "multiply", "color", "difference"]:
        for bk in ["hard", "soft", "dual"]:
            cases.append(["pt.retouch", f, "@img", J(B[bk]), J(P["p1"]), "0.7", bl, "0"])
    cases.append(["pt.retouch", f, "@img", J(B["hard"]), J(P["p0"]), "1", "normal", "1"])
for v, a, k, lk in [("0,0,0,0,1,0.5,0,1", "3", "0.25", "0"), ("0.2,0.2,0.2,1,0,0,0,0", "3", "1", "0"), ("0.5,0.5,0.5,0.5,1,1,1,1", "7", "0.5", "1"), ("0.1,0.9,0.3,0.6,0.7,0.2,0.4,0.3", "3", "0.6", "1")]:
    cases.append(["pt.mixpm", v, a, k, lk])
for b in [{"size": 40, "angle": 33, "roundness": 0.4, "hardness": h} for h in (0, 0.25, 0.5, 0.75, 1)] + [B["sampled"], B["sampledal"], B["stored"], B["proj"],
          {"size": 20, "tip": {"sampled": {"width": 8, "height": 8, "data": [0] * 64}}}, {"size": 0}, {"size": -1}, {"size": 20, "angle": 1e30}, {"size": 3, "roundness": 0.01},
          {"size": 50, "tip": {"sampled": {"width": 60, "height": 50, "data": [round(0.5 + 0.5 * math.sin(x * 0.21) * math.cos(y * 0.13), 2) for y in range(50) for x in range(60)]}}, "flipY": True, "angle": -60}]:
    for full in ["0", "1"]:
        cases.append(["pt.outline", J(b), full])
for mode in ["vertical", "horizontal", "dual", "diagonal", "bogus"]:
    for cx, cy, rot, w, h in [("20", "30", "0", "100", "100"), ("50.5", "26.5", "90", "101", "53"), ("20", "-5", "0", "100", "60"), ("nan", "0", "0", "10", "10"), ("0", "0", "inf", "10", "10"), ("1000001", "0", "0", "10", "10"), ("10", "10", "-725.5", "40", "30")]:
        cases.append(["pt.sym", mode, cx, cy, rot, w, h, J(P["p4"])])
for mode in ["vertical", "horizontal", "dual", "diagonal"]:
    for bk in ["hard", "color", "dual"]:
        cases.append(["pt.union", "rgb", "@img", J(B[bk]), J(P["p1"]), mode, "48.5", "35.5"])
    cases.append(["pt.union", "rgb16", "-", J(B["soft"]), J(P["p0"]), mode, "40", "40"])
for bk in ["hard", "sampled", "dual", "texture", "texeach", "wet", "pencil4", "block", "proj", "tiny"]:
    for dab in ["30.3,20.7,5.5,0.8,0.4,0.6,0,0,1,0.5", "10,10,1.2,1,0,1,1,0.7,0.4,1", "50.5,40.5,8,0.6,-1.2,0.3,1,2.1,0.2,0.3"]:
        cases.append(["pt.ctx", J(B[bk]), dab])
json.dump(cases, open(sys.argv[1], "w"), separators=(",", ":"))
print(len(cases))
